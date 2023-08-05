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

use boa_engine::{Context, Source};
use chrono as _;
use getrandom as _;
use wasm_bindgen::prelude::*;
use js_sys;

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
pub fn evaluate_with_debug_hooks(src: &str, compiled_output_action: &js_sys::Function, trace_output_action: &js_sys::Function) -> Result<String, JsValue> {
    let ca_clone = compiled_output_action.clone();
    let compiled_action = move |output:&str| {
        let this = JsValue::null();
        let o = JsValue::from(output);
        let _unused = ca_clone.call1(&this, &o);
    };

    let ta_clone = trace_output_action.clone();
    let trace_action = move |output: &str| {
        let this = JsValue::null();
        let o = JsValue::from(output);
        let _unused = ta_clone.call1(&this, &o);
    };

    // setup executor
    let mut context = Context::default();
    context.set_trace();
    context.set_custom_compile_trace(Box::new(compiled_action));
    context.set_custom_runtime_trace(Box::new(trace_action));

    context
        .eval(Source::from_bytes(src))
        .map_err(|e| JsValue::from(format!("Uncaught {e}")))
        .map(|v| v.display().to_string())
}