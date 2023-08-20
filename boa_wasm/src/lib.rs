//! An ECMAScript WASM implementation based on boa_engine.
//!
#![doc = include_str!("../ABOUT.md")]
#![doc(
    html_logo_url = "https://raw.githubusercontent.com/boa-dev/boa/main/assets/logo.svg",
    html_favicon_url = "https://raw.githubusercontent.com/boa-dev/boa/main/assets/logo.svg"
)]
#![cfg_attr(not(test), forbid(clippy::unwrap_used))]
#![warn(missing_docs, clippy::dbg_macro)]
#![deny(
    // rustc lint groups https://doc.rust-lang.org/rustc/lints/groups.html
    warnings,
    future_incompatible,
    let_underscore,
    nonstandard_style,
    rust_2018_compatibility,
    rust_2018_idioms,
    rust_2021_compatibility,
    unused,

    // rustc allowed-by-default lints https://doc.rust-lang.org/rustc/lints/listing/allowed-by-default.html
    macro_use_extern_crate,
    meta_variable_misuse,
    missing_abi,
    missing_copy_implementations,
    missing_debug_implementations,
    non_ascii_idents,
    noop_method_call,
    single_use_lifetimes,
    trivial_casts,
    trivial_numeric_casts,
    unreachable_pub,
    unsafe_op_in_unsafe_fn,
    unused_crate_dependencies,
    unused_import_braces,
    unused_lifetimes,
    unused_qualifications,
    unused_tuple_struct_fields,
    variant_size_differences,

    // rustdoc lints https://doc.rust-lang.org/rustdoc/lints.html
    rustdoc::broken_intra_doc_links,
    rustdoc::private_intra_doc_links,
    rustdoc::missing_crate_level_docs,
    rustdoc::private_doc_tests,
    rustdoc::invalid_codeblock_attributes,
    rustdoc::invalid_rust_codeblocks,
    rustdoc::bare_urls,

    // clippy categories https://doc.rust-lang.org/clippy/
    clippy::all,
    clippy::correctness,
    clippy::suspicious,
    clippy::style,
    clippy::complexity,
    clippy::perf,
    clippy::pedantic,
    clippy::nursery,
)]
#![allow(clippy::new_without_default, clippy::missing_const_for_fn)]

use boa_engine::{vm::trace::Tracer, Context, Source};
use chrono as _;
use getrandom as _;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
fn main() {
    console_error_panic_hook::set_once();
}

/// Evaluate the given ECMAScript code.
#[wasm_bindgen]
pub fn evaluate(src: &str) -> Result<String, JsValue> {
    // Setup the executor
    Context::default()
        .eval(Source::from_bytes(src))
        .map_err(|e| JsValue::from(format!("Uncaught {e}")))
        .map(|v| v.display().to_string())
}

#[wasm_bindgen]
/// Evaluate some JavaScript with trace hooks.
pub fn evaluate_with_debug_hooks(
    src: &str,
    compiled_output_action: &js_sys::Function,
    trace_output_action: &js_sys::Function,
) -> Result<String, JsValue> {
    let compiled_clone = compiled_output_action.clone();
    let compiled_action = move |output: &str| {
        let this = JsValue::null();
        let o = JsValue::from(output);
        let _unused = compiled_clone.call1(&this, &o);
    };

    let trace_clone = trace_output_action.clone();
    let trace_action = move |output: &str| {
        let this = JsValue::null();
        let o = JsValue::from(output);
        let _unused = trace_clone.call1(&this, &o);
    };

    // setup executor
    let mut context = Context::default();
    let mut tracer = WasmTracer::default();
    tracer.set_compiled_handler(Box::new(compiled_action));
    tracer.set_trace_handler(Box::new(trace_action));

    context.set_tracer_implementation(Box::new(tracer));

    context
        .eval(Source::from_bytes(src))
        .map_err(|e| JsValue::from(format!("Uncaught {e}")))
        .map(|v| v.display().to_string())
}

#[derive(Debug)]
#[wasm_bindgen]
/// The WASM exposed `BoaJs` Object.
pub struct BoaJs {
    compiled_action: Option<js_sys::Function>,
    trace_action: Option<js_sys::Function>,
}

#[wasm_bindgen]
impl BoaJs {
    /// Create a new BoaJs Object.
    #[must_use]
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            compiled_action: None,
            trace_action: None,
        }
    }

    /// Set a Js Closure action for handling Boa's ByteCompiler trace output.
    pub fn set_compiled_output_action(&mut self, f: &js_sys::Function) {
        let fun = f.clone();
        self.compiled_action = Some(fun);
    }

    /// Set a Js Closure action for handling Boa's VM Trace output.
    pub fn set_trace_output_action(&mut self, f: &js_sys::Function) {
        let fun = f.clone();
        self.trace_action = Some(fun);
    }

    /// Evaluate some Js Source Code with trace active.
    pub fn evaluate_with_trace(&self, src: &str) -> Result<String, JsValue> {
        // setup executor
        let mut context = Context::default();

        let mut tracer = WasmTracer::default();

        if let Some(func) = &self.compiled_action {
            let func_clone = func.clone();
            let action = move |output: &str| {
                let this = JsValue::null();
                let o = JsValue::from(output);
                let _unused = func_clone.call1(&this, &o);
            };

            tracer.set_compiled_handler(Box::new(action));
        };

        if let Some(func) = &self.trace_action {
            let func_clone = func.clone();
            let action = move |output: &str| {
                let this = JsValue::null();
                let o = JsValue::from(output);
                let _unused = func_clone.call1(&this, &o);
            };

            tracer.set_trace_handler(Box::new(action));
        };

        context.set_tracer_implementation(Box::new(tracer));

        context
            .eval(Source::from_bytes(src))
            .map_err(|e| JsValue::from(format!("Uncaught {e}")))
            .map(|v| v.display().to_string())
    }

    /// Evaluate Js Source code without running trace.
    pub fn evaluate(&self, src: &str) -> Result<String, JsValue> {
        Context::default()
            .eval(Source::from_bytes(src))
            .map_err(|e| JsValue::from(format!("Uncaught {e}")))
            .map(|v| v.display().to_string())
    }
}

type ProvidedFunction = Box<dyn Fn(&str)>;

#[derive(Default)]
pub(crate) struct WasmTracer {
    compiled_handler: Option<ProvidedFunction>,
    trace_handler: Option<ProvidedFunction>,
}

impl WasmTracer {
    fn set_compiled_handler(&mut self, compiled_handler: Box<dyn Fn(&str)>) {
        self.compiled_handler = Some(compiled_handler);
    }

    fn set_trace_handler(&mut self, trace_handler: Box<dyn Fn(&str)>) {
        self.trace_handler = Some(trace_handler);
    }
}

impl Tracer for WasmTracer {
    fn emit_bytecode_trace(&self, msg: &str) {
        if let Some(action) = &self.compiled_handler {
            action(msg);
        }
    }

    fn emit_call_frame_entrance_trace(&self, msg: &str) {
        if let Some(action) = &self.trace_handler {
            action(msg);
        }
    }

    fn emit_instruction_trace(&self, msg: &str) {
        if let Some(action) = &self.trace_handler {
            action(msg);
        }
    }

    fn emit_call_frame_exit_trace(&self, msg: &str) {
        if let Some(action) = &self.trace_handler {
            action(msg);
        }
    }
}
