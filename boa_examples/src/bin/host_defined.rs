// This example goes into the details on how to pass closures as functions inside Rust and call them
// from Javascript.

use std::cell::Cell;

use boa_engine::{
    context::{intrinsics::Intrinsics, HostHooks},
    native_function::NativeFunction,
    object::NativeObject,
    Context, JsArgs, JsError, JsNativeError, Source,
};
use boa_gc::{Finalize, Trace};

/// Custom host-defined struct that has some state, and can be shared between JavaScript and rust.
#[derive(Default, Trace, Finalize)]
struct CustomHostDefinedStruct {
    #[unsafe_ignore_trace]
    counter: Cell<usize>,
}

/// Create a custom [`HostHooks`].
#[derive(Default)]
struct CustomHostHooks {}

impl HostHooks for CustomHostHooks {
    // Override default implementation for creating the `[[HostDefined]]` field on realms.
    fn create_host_defined_realm_field(&self, _intrinsics: &Intrinsics) -> Box<dyn NativeObject> {
        Box::<CustomHostDefinedStruct>::default()
    }
}

fn main() -> Result<(), JsError> {
    let host_hooks: &dyn HostHooks = &CustomHostHooks::default();

    // We create a new `Context` to create a new Javascript executor with the custom HostHooks.
    let mut context = Context::builder().host_hooks(host_hooks).build()?;

    // Get the realm from the context.
    let realm = context.realm().clone();

    // Get the host define field from the realm and downcast it to our concrete type.
    let Some(host_defined) = realm.host_defined().downcast_ref::<CustomHostDefinedStruct>() else {
        return Err(JsNativeError::typ().with_message("Realm does not HostDefined field").into());
    };

    // Assert that the initial state.
    assert_eq!(host_defined.counter.get(), 0);

    // Create and register function for setting and setting the realm value.
    //
    // The funtion lives in the context's realm and has access to the host-defined field.
    context.register_global_builtin_callable(
        "setRealmValue", 1, NativeFunction::from_fn_ptr(|_, args, context| {
            let value: usize = args.get_or_undefined(0).try_js_into(context)?;

            let realm = context.realm();
            let Some(host_defined) = realm.host_defined().downcast_ref::<CustomHostDefinedStruct>() else {
                return Err(JsNativeError::typ().with_message("Realm does not HostDefined field").into());
            };

            host_defined.counter.set(value);

            Ok(value.into())
        })
    )?;

    context.register_global_builtin_callable(
        "getRealmValue", 0, NativeFunction::from_fn_ptr(|_, _, context| {
            let realm = context.realm();
            let Some(host_defined) = realm.host_defined().downcast_ref::<CustomHostDefinedStruct>() else {
                return Err(JsNativeError::typ().with_message("Realm does not HostDefined field").into());
            };

            Ok(host_defined.counter.get().into())
        })
    )?;

    // Run code in JavaScript that mutates the host-defined field on the Realm.
    context.eval_script(Source::from_bytes(
        r"
        setRealmValue(50);
        setRealmValue(getRealmValue() * 2);
    ",
    ))?;

    // Assert that the host-defined field changed.
    assert_eq!(host_defined.counter.get(), 100);

    Ok(())
}
