use crate::{
    object::{internal_methods::InternalMethodContext, shape::slot::SlotAttributes},
    property::PropertyKey,
    vm::{opcode::Operation, CompletionType},
    Context, JsResult, JsValue,
};

/// `GetPropertyByName` implements the Opcode Operation for `Opcode::GetPropertyByName`
///
/// Operation:
///  - Get a property by name from an object an push it on the stack.
#[derive(Debug, Clone, Copy)]
pub(crate) struct GetPropertyByName;

impl Operation for GetPropertyByName {
    const NAME: &'static str = "GetPropertyByName";
    const INSTRUCTION: &'static str = "INST - GetPropertyByName";

    fn execute(context: &mut Context<'_>) -> JsResult<CompletionType> {
        let ic_index = context.vm.read::<u32>() as usize;

        let receiver = context.vm.pop();
        let value = context.vm.pop();
        let object = if let Some(object) = value.as_object() {
            object.clone()
        } else {
            value.to_object(context)?
        };

        let ic = &context.vm.frame().code_block().ic[ic_index];
        let mut slot = ic.slot();
        if slot.is_cachable() {
            let object_borrowed = object.borrow();
            if ic.matches(object_borrowed.shape()) {
                // println!(
                //     "GET: T: \"{}\" {}: {:?}",
                //     ic.name.to_std_string_escaped(),
                //     slot.index,
                //     slot.attributes
                // );
                let mut result = if slot.attributes.contains(SlotAttributes::PROTOTYPE) {
                    let prototype = object
                        .borrow()
                        .properties()
                        .shape
                        .prototype()
                        .expect("prototype should have value");
                    let prototype = prototype.borrow();
                    prototype.properties().storage[slot.index as usize].clone()
                } else {
                    object_borrowed.properties().storage[slot.index as usize].clone()
                };

                drop(object_borrowed);
                if slot.attributes.has_get() && result.is_object() {
                    result = result.as_object().expect("should contain getter").call(
                        &receiver,
                        &[],
                        context,
                    )?;
                }
                context.vm.push(result);
                return Ok(CompletionType::Normal);
            }
        }

        let key: PropertyKey = ic.name.clone().into();

        let context = &mut InternalMethodContext::new(context);
        let result = object.__get__(&key, receiver, context)?;

        slot = *context.slot();

        // println!(
        //     "GET: F: \"{}\" {}: {:?} - {}",
        //     key,
        //     slot.index,
        //     slot.attributes,
        //     result.type_of()
        // );

        // Cache the property.
        if slot.attributes.is_cachable() {
            let ic = &context.vm.frame().code_block.ic[ic_index];
            let object_borrowed = object.borrow();
            let shape = object_borrowed.shape().clone();
            ic.set(shape, slot);
        }

        context.vm.push(result);
        Ok(CompletionType::Normal)
    }
}

/// `GetPropertyByValue` implements the Opcode Operation for `Opcode::GetPropertyByValue`
///
/// Operation:
///  - Get a property by value from an object an push it on the stack.
#[derive(Debug, Clone, Copy)]
pub(crate) struct GetPropertyByValue;

impl Operation for GetPropertyByValue {
    const NAME: &'static str = "GetPropertyByValue";
    const INSTRUCTION: &'static str = "INST - GetPropertyByValue";

    fn execute(context: &mut Context<'_>) -> JsResult<CompletionType> {
        let key = context.vm.pop();
        let receiver = context.vm.pop();
        let value = context.vm.pop();
        let object = if let Some(object) = value.as_object() {
            object.clone()
        } else {
            value.to_object(context)?
        };

        let key = key.to_property_key(context)?;

        // Fast Path
        if object.is_array() {
            if let PropertyKey::Index(index) = &key {
                let object_borrowed = object.borrow();
                if let Some(element) = object_borrowed
                    .properties()
                    .dense_indexed_properties()
                    .and_then(|vec| vec.get(*index as usize))
                {
                    context.vm.push(element.clone());
                    return Ok(CompletionType::Normal);
                }
            }
        }

        // Slow path:
        let result = object.__get__(&key, receiver, &mut InternalMethodContext::new(context))?;

        context.vm.push(result);
        Ok(CompletionType::Normal)
    }
}

/// `GetMethod` implements the Opcode Operation for `Opcode::GetMethod`
///
/// Operation:
///  - Get a property method or undefined if the property is null or undefined.
#[derive(Debug, Clone, Copy)]
pub(crate) struct GetMethod;

impl Operation for GetMethod {
    const NAME: &'static str = "GetMethod";
    const INSTRUCTION: &'static str = "INST - GetMethod";

    fn execute(context: &mut Context<'_>) -> JsResult<CompletionType> {
        let index = context.vm.read::<u32>();
        let key = context.vm.frame().code_block.names[index as usize].clone();
        let value = context.vm.pop();

        let method = value.get_method(key, context)?;
        context.vm.push(value);
        context
            .vm
            .push(method.map(JsValue::from).unwrap_or_default());
        Ok(CompletionType::Normal)
    }
}

/// `GetPropertyByValuePush` implements the Opcode Operation for `Opcode::GetPropertyByValuePush`
///
/// Operation:
///  - Get a property by value from an object an push the key and value on the stack.
#[derive(Debug, Clone, Copy)]
pub(crate) struct GetPropertyByValuePush;

impl Operation for GetPropertyByValuePush {
    const NAME: &'static str = "GetPropertyByValuePush";
    const INSTRUCTION: &'static str = "INST - GetPropertyByValuePush";

    fn execute(context: &mut Context<'_>) -> JsResult<CompletionType> {
        let key = context.vm.pop();
        let receiver = context.vm.pop();
        let value = context.vm.pop();
        let object = if let Some(object) = value.as_object() {
            object.clone()
        } else {
            value.to_object(context)?
        };

        let key = key.to_property_key(context)?;

        // Fast path:
        if object.is_array() {
            if let PropertyKey::Index(index) = &key {
                let object_borrowed = object.borrow();
                if let Some(element) = object_borrowed
                    .properties()
                    .dense_indexed_properties()
                    .and_then(|vec| vec.get(*index as usize))
                {
                    context.vm.push(key);
                    context.vm.push(element.clone());
                    return Ok(CompletionType::Normal);
                }
            }
        }

        // Slow path:
        let result = object.__get__(&key, receiver, &mut InternalMethodContext::new(context))?;

        context.vm.push(key);
        context.vm.push(result);
        Ok(CompletionType::Normal)
    }
}
