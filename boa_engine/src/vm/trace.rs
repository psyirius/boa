//! Boa's `Trace` module for the `Vm`.

use bitflags::bitflags;
use std::cell::Cell;
use std::collections::VecDeque;
use std::fmt;

use boa_interner::{Interner, ToInternedString};

use super::Vm;

// TODO: Build out further, maybe provide more element visiblity and events/outputs
/// The `Tracer` trait is a customizable trait that can be provided to `Boa`
/// for customizing output.
pub trait Tracer {
    /// The output from tracing a `CodeBlock`'s bytecode.
    fn emit_bytecode_trace(&self, msg: &str);
    /// The output from entering a `CallFrame`.
    fn emit_call_frame_entrance_trace(&self, msg: &str);
    /// The trace output from an execution.
    fn emit_instruction_trace(&self, msg: &str);
    /// Trace output from exiting a `CallFrame`.
    fn emit_call_frame_exit_trace(&self, msg: &str);
}

#[derive(Debug)]
pub(crate) struct DefaultTracer;

impl Tracer for DefaultTracer {
    fn emit_bytecode_trace(&self, msg: &str) {
        println!("{msg}");
    }

    fn emit_call_frame_entrance_trace(&self, msg: &str) {
        println!("{msg}");
    }

    fn emit_instruction_trace(&self, msg: &str) {
        println!("{msg}");
    }

    fn emit_call_frame_exit_trace(&self, msg: &str) {
        println!("{msg}");
    }
}

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub(crate) struct TraceOptions: u8 {
        const FULL_TRACE =  0b0000_0001;

        const ACTIVE = 0b0000_0010;
    }
}

impl Vm {
    /// Sets the trace for `Vm` to run.
    pub(crate) fn set_trace(&mut self, trace: VmTrace) {
        self.trace = Some(trace);
    }
}

impl Default for VmTrace {
    fn default() -> Self {
        Self {
            options: Cell::new(TraceOptions::FULL_TRACE),
            tracer: Box::new(DefaultTracer),
        }
    }
}

/// `VmTrace` is a boa spcific structure for running Boa's Virtual Machine trace.
///
/// The struct provides options for a user to set customized actions for handling
/// messages output during the trace.
///
/// Currently, the trace supports setting two different actions:
/// - `compiled_action`
/// - `trace_action`
///
/// About the actions
///
/// After the Global callframe is initially provided. It searches
/// for all possible compiled output
pub struct VmTrace {
    options: Cell<TraceOptions>,
    tracer: Box<dyn Tracer>,
}

// ==== Public API ====

impl VmTrace {
    #[must_use]
    /// Create a partial `VmTrace`.
    pub fn partial() -> Self {
        Self {
            options: Cell::new(TraceOptions::empty()),
            tracer: Box::new(DefaultTracer),
        }
    }

    #[must_use]
    /// Method for adding a compiled action on initialization.
    pub fn with_tracer(mut self, tracer: Box<dyn Tracer>) -> Self {
        self.set_tracer(tracer);
        self
    }

    /// Sets the current `Tracer` of `VmTrace`.
    pub fn set_tracer(&mut self, tracer: Box<dyn Tracer>) {
        self.tracer = tracer;
    }
}

// ==== Internal VmTrace methods ====

impl VmTrace {
    /// Returns if Trace type is a complete trace.
    pub(crate) fn is_full_trace(&self) -> bool {
        self.options.get().contains(TraceOptions::FULL_TRACE)
    }

    /// Returns if the trace is only a partial one.
    pub fn is_partial_trace(&self) -> bool {
        !self.is_full_trace()
    }

    /// Returns if the a partial trace has been determined to be active.
    pub fn is_active(&self) -> bool {
        self.options.get().contains(TraceOptions::ACTIVE)
    }

    /// Sets the `ACTIVE` bitflag to true.
    pub(crate) fn activate(&self) {
        let mut flags = self.options.get();
        flags.set(TraceOptions::ACTIVE, true);
        self.options.set(flags);
    }

    /// Sets the `ACTIVE` flag to false.
    pub(crate) fn inactivate(&self) {
        let mut flags = self.options.get();
        flags.set(TraceOptions::ACTIVE, false);
        self.options.set(flags);
    }

    /// Returns whether a trace should run on an instruction.
    pub(crate) fn should_trace(&self) -> bool {
        self.is_full_trace() || self.is_active()
    }
}

// ==== Trace Event/Action Methods ====

impl VmTrace {
    const COLUMN_WIDTH: usize = 26;
    const TIME_COLUMN_WIDTH: usize = Self::COLUMN_WIDTH / 2;
    const OPCODE_COLUMN_WIDTH: usize = Self::COLUMN_WIDTH;
    const OPERAND_COLUMN_WIDTH: usize = Self::COLUMN_WIDTH;
    const NUMBER_OF_COLUMNS: usize = 4;

    /// Trace the current `CallFrame` according to current state
    pub(crate) fn trace_call_frame(&self, vm: &Vm, interner: &Interner) {
        if self.is_full_trace() {
            self.trace_compiled_bytecode(vm, interner);
            self.call_frame_header(vm);
        } else if self.is_partial_trace() && vm.frame().code_block().traceable() {
            if !vm.frame().code_block().frame_traced() {
                self.trace_current_bytecode(vm, interner);
                vm.frame().code_block().set_frame_traced(true);
            }
            self.call_frame_header(vm);
            self.activate();
        } else {
            self.call_frame_header(vm);
        }
    }

    /// Emits the current `CallFrame`'s header.
    pub(crate) fn call_frame_header(&self, vm: &Vm) {
        let msg = format!(
            " Call Frame -- {} ",
            vm.frame().code_block().name().to_std_string_escaped()
        );

        let frame_header = format!(
            "{msg:-^width$}",
            width = Self::COLUMN_WIDTH * Self::NUMBER_OF_COLUMNS - 10
        );
        self.tracer.emit_call_frame_entrance_trace(&frame_header);

        if vm.frames.len() == 1 {
            let column_headers = format!(
                "{:<TIME_COLUMN_WIDTH$} {:<OPCODE_COLUMN_WIDTH$} {:<OPERAND_COLUMN_WIDTH$} Stack\n",
                "Time",
                "Opcode",
                "Operands",
                TIME_COLUMN_WIDTH = Self::TIME_COLUMN_WIDTH,
                OPCODE_COLUMN_WIDTH = Self::OPCODE_COLUMN_WIDTH,
                OPERAND_COLUMN_WIDTH = Self::OPERAND_COLUMN_WIDTH,
            );

            self.tracer.emit_call_frame_entrance_trace(&column_headers);
        }
    }

    /// Searches traces all of the current `CallFrame`'s available `CodeBlock`s.
    pub(crate) fn trace_compiled_bytecode(&self, vm: &Vm, interner: &Interner) {
        // We only continue to the compiled output if we are on the global.
        if vm.frames.len() == 1 {
            let mut queue = VecDeque::new();
            queue.push_back(vm.frame().code_block.clone());

            while !queue.is_empty() {
                let block = queue.pop_front().expect("queue must have a value.");

                queue.extend(block.functions.iter().cloned());

                self.tracer
                    .emit_bytecode_trace(&block.to_interned_string(interner));
            }
        }
    }

    /// Searches and traces for only current frame's `CodeBlock`.
    pub(crate) fn trace_current_bytecode(&self, vm: &Vm, interner: &Interner) {
        self.tracer
            .emit_bytecode_trace(&vm.frame().code_block().to_interned_string(interner));
    }

    /// Emits an exit message for the current `CallFrame`.
    pub(crate) fn trace_frame_end(&self, vm: &Vm, return_msg: &str) {
        if self.should_trace() {
            let msg = format!(
                " Call Frame -- <Exiting {} via {return_msg}> ",
                vm.frame().code_block().name.to_std_string_escaped()
            );
            let frame_footer = format!(
                "{msg:-^width$}",
                width = Self::COLUMN_WIDTH * Self::NUMBER_OF_COLUMNS - 10
            );

            self.tracer.emit_call_frame_exit_trace(&frame_footer);
        }

        self.inactivate();
    }

    // NOTE(nekevss): duration cannot be handled in a wasm32 env. Need to figure a different way to profile
    #[cfg(target_arch = "wasm32")]
    pub(crate) fn trace_instruction(&self, opcode: &str, operands: &str, stack: &str) {
        let instruction_trace = format!(
            "{:<TIME_COLUMN_WIDTH$} {:<OPCODE_COLUMN_WIDTH$} {operands:<OPERAND_COLUMN_WIDTH$} {stack}",
            "--μs",
            opcode,
            TIME_COLUMN_WIDTH = Self::TIME_COLUMN_WIDTH,
            OPCODE_COLUMN_WIDTH = Self::OPCODE_COLUMN_WIDTH,
            OPERAND_COLUMN_WIDTH = Self::OPERAND_COLUMN_WIDTH,
        );

        self.tracer.emit_instruction_trace(&instruction_trace);
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn trace_instruction(
        &self,
        duration: u128,
        opcode: &str,
        operands: &str,
        stack: &str,
    ) {
        let instruction_trace = format!(
            "{:<TIME_COLUMN_WIDTH$} {:<OPCODE_COLUMN_WIDTH$} {operands:<OPERAND_COLUMN_WIDTH$} {stack}",
            format!("{}μs", duration),
            opcode,
            TIME_COLUMN_WIDTH = Self::TIME_COLUMN_WIDTH,
            OPCODE_COLUMN_WIDTH = Self::OPCODE_COLUMN_WIDTH,
            OPERAND_COLUMN_WIDTH = Self::OPERAND_COLUMN_WIDTH,
        );

        self.tracer.emit_instruction_trace(&instruction_trace);
    }
}

impl fmt::Debug for VmTrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Current Active Tracer")
    }
}
