use std::collections::VecDeque;
use std::fmt;

use boa_interner::{Interner, ToInternedString};

use super::Vm;

impl Vm {
    pub(crate) fn initialize_trace(&mut self) {
        self.trace = Some(VmTrace::default())
    }
}

impl Default for VmTrace {
    fn default() -> Self {
        Self {
            compiled_action: None,
            trace_action: None,
            bytecode_trace: false,
        }
    }
}

pub(crate) struct VmTrace {
    compiled_action: Option<Box<dyn Fn(&str) -> ()>>,
    trace_action: Option<Box<dyn Fn(&str) -> ()>>,
    bytecode_trace: bool
}

impl VmTrace {
    pub(crate) const fn bytecode_traced(&self) -> bool {
        self.bytecode_trace
    }

    pub(crate) fn set_traced(&mut self, value: bool) {
        self.bytecode_trace = value
    }

    pub(crate) fn set_compiled_action(&mut self, f: Box<dyn Fn(&str)->()>) {
        self.compiled_action = Some(f)
    }

    pub(crate) fn set_trace_action(&mut self, f: Box<dyn Fn(&str)->()>) {
        self.trace_action = Some(f)
    }
}

// ---- Trace Methods ----
impl VmTrace {
    const COLUMN_WIDTH: usize = 26;
    const TIME_COLUMN_WIDTH: usize = Self::COLUMN_WIDTH / 2;
    const OPCODE_COLUMN_WIDTH: usize = Self::COLUMN_WIDTH;
    const OPERAND_COLUMN_WIDTH: usize = Self::COLUMN_WIDTH;
    const NUMBER_OF_COLUMNS: usize = 4;

    pub(crate) fn trigger_compiled_output_action(&self, msg: &str) {
        if let Some(action) = &self.compiled_action {
            action(&format!("{}\n", msg))
        } else {
            println!("{}", msg);
        }
    }

    pub(crate) fn trigger_trace_action(&self, msg: &str) {
        if let Some(action) = &self.trace_action {
            action(&format!("{}\n", msg))
        } else {
            println!("{}", msg)
        }
    }
}

impl VmTrace {
    pub(crate) fn trace_compiled_bytecode(&self, vm: &Vm, interner: &Interner) {
        // We only continue to the compiled output if we are on the global.
        if vm.frames.len() == 1 {
            let mut queue = VecDeque::new();
            queue.push_back(vm.frame().code_block.clone());

            while !queue.is_empty() {
                let block = queue.pop_front().expect("queue must have a value.");

                queue.extend(block.functions.iter().map(|code| code.clone()));

                self.trigger_compiled_output_action(&block.to_interned_string(interner))
            }
        }
    }

    pub(crate) fn trace_call_frame(&self, vm: &Vm) {
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

            self.trigger_trace_action(&column_headers)
        }
    }

    pub(crate) fn trace_frame_end(&self, return_msg: &str) {
        let msg = format!(" Call Frame -- <Exiting frame via {}> ", return_msg);
        let frame_footer = format!(
            "{msg:-^width$}",
            width = Self::COLUMN_WIDTH * Self::NUMBER_OF_COLUMNS - 10
        );

        self.trigger_trace_action(&frame_footer);
    }

    pub(crate) fn trace_instruction(&self, duration: u128, opcode: &str, operands: String, stack: String) {
        #[cfg(not(target_arch = "wasm32"))]
        let instruction_trace = format!(
            "{:<TIME_COLUMN_WIDTH$} {:<OPCODE_COLUMN_WIDTH$} {operands:<OPERAND_COLUMN_WIDTH$} {stack}",
            format!("{}μs", duration),
            opcode,
            TIME_COLUMN_WIDTH = Self::TIME_COLUMN_WIDTH,
            OPCODE_COLUMN_WIDTH = Self::OPCODE_COLUMN_WIDTH,
            OPERAND_COLUMN_WIDTH = Self::OPERAND_COLUMN_WIDTH,
        );

        // NOTE(nekevss): duration cannot be handled in a wasm32 env. Figure a different way to profile
        #[cfg(target_arch = "wasm32")]
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
}


impl fmt::Debug for VmTrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ca_msg = if self.compiled_action.is_none() {
            "None"
        } else {
            "User Defined Fn"
        };

        let ta_msg = if self.trace_action.is_none() {
            "None"
        } else {
            "User Defined Fn"
        };

        f.debug_struct("VmTrace")
         .field("Compiled Action", &ca_msg)
         .field("Runtime Trace Action", &ta_msg)
         .finish()
    }
}