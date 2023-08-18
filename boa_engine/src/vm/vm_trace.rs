//! Boa's `Trace` module for the `Vm`.

use bitflags::bitflags;
use std::cell::Cell;
use std::collections::VecDeque;
use std::fmt;

use boa_interner::{Interner, ToInternedString};

use super::Vm;

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub(crate) struct TraceOptions: u8 {
        const FULL_TRACE =  0b0000_0001;

        const ACTIVE = 0b0000_0010;
    }
}

impl Vm {
    pub(crate) fn initialize_trace(&mut self) {
        self.trace = Some(VmTrace::default());
    }

    pub(crate) fn init_partial_trace(&mut self) {
        self.trace = Some(VmTrace::new_partial());
    }
}

impl Default for VmTrace {
    fn default() -> Self {
        Self {
            compiled_action: None,
            trace_action: None,
            options: Cell::new(TraceOptions::FULL_TRACE),
        }
    }
}

// Action function provided by the user.
type ActionFunction = Box<dyn Fn(&str)>;

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
    compiled_action: Option<ActionFunction>,
    trace_action: Option<ActionFunction>,
    options: Cell<TraceOptions>,
}

impl VmTrace {
    pub(crate) fn new_partial() -> Self {
        Self {
            compiled_action: None,
            trace_action: None,
            options: Cell::new(TraceOptions::empty()),
        }
    }

    // Returns if Trace type is a complete trace.
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

    pub(crate) fn should_trace(&self) -> bool {
        self.is_full_trace() || self.is_active()
    }

    /// Sets the `compiled_action` of `VmTrace` to a custom user-defined action.
    pub fn set_compiled_action(&mut self, f: Box<dyn Fn(&str)>) {
        self.compiled_action = Some(f);
    }

    /// Sets the `trace_action` of `VmTrace` to a custom user-defined action.
    pub fn set_trace_action(&mut self, f: Box<dyn Fn(&str)>) {
        self.trace_action = Some(f);
    }
}

// ---- Trace Event/Action Methods ----
impl VmTrace {
    const COLUMN_WIDTH: usize = 26;
    const TIME_COLUMN_WIDTH: usize = Self::COLUMN_WIDTH / 2;
    const OPCODE_COLUMN_WIDTH: usize = Self::COLUMN_WIDTH;
    const OPERAND_COLUMN_WIDTH: usize = Self::COLUMN_WIDTH;
    const NUMBER_OF_COLUMNS: usize = 4;

    pub(crate) fn trigger_compiled_output_action(&self, msg: &str) {
        if let Some(action) = &self.compiled_action {
            action(&format!("{msg}\n"));
        } else {
            println!("{msg}");
        }
    }

    pub(crate) fn trigger_trace_action(&self, msg: &str) {
        if let Some(action) = &self.trace_action {
            action(&format!("{msg}\n"));
        } else {
            println!("{msg}");
        }
    }
}

impl VmTrace {
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
        self.trigger_trace_action(&frame_header);

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

            self.trigger_trace_action(&column_headers);
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

                self.trigger_compiled_output_action(&block.to_interned_string(interner));
            }
        }
    }

    /// Searches and traces for only current frame's `CodeBlock`.
    pub(crate) fn trace_current_bytecode(&self, vm: &Vm, interner: &Interner) {
        self.trigger_compiled_output_action(&vm.frame().code_block().to_interned_string(interner));
    }

    /// Emits an exit message for the current `CallFrame`.
    pub(crate) fn trace_frame_end(&self, return_msg: &str) {
        let msg = format!(" Call Frame -- <Exiting frame via {return_msg}> ");
        let frame_footer = format!(
            "{msg:-^width$}",
            width = Self::COLUMN_WIDTH * Self::NUMBER_OF_COLUMNS - 10
        );

        self.trigger_trace_action(&frame_footer);
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

        self.trigger_trace_action(&instruction_trace);
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

        self.trigger_trace_action(&instruction_trace);
    }
}

impl fmt::Debug for VmTrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let compiled_action_val = if self.compiled_action.is_none() {
            "None"
        } else {
            "User Defined Fn"
        };

        let trace_action_val = if self.trace_action.is_none() {
            "None"
        } else {
            "User Defined Fn"
        };

        f.debug_struct("VmTrace")
            .field("Compiled Action", &compiled_action_val)
            .field("Runtime Trace Action", &trace_action_val)
            .finish()
    }
}
