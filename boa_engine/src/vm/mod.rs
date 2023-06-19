//! Boa's ECMAScript Virtual Machine
//!
//! The Virtual Machine (VM) handles generating instructions, then executing them.
//! This module will provide an instruction set for the AST to use, various traits,
//! plus an interpreter to execute those instructions

#[cfg(feature = "fuzz")]
use crate::JsNativeError;
use crate::{
    builtins::async_generator::{AsyncGenerator, AsyncGeneratorState},
    environments::{DeclarativeEnvironment, EnvironmentStack},
    script::Script,
    vm::code_block::Readable,
    Context, JsError, JsObject, JsResult, JsValue, Module,
};

use boa_gc::{custom_trace, Finalize, Gc, Trace};
use boa_profiler::Profiler;
use std::{future::Future, mem::size_of, ops::ControlFlow, pin::Pin, task};

#[cfg(feature = "trace")]
use boa_interner::ToInternedString;
#[cfg(feature = "trace")]
use std::time::Instant;

mod call_frame;
mod code_block;
mod completion_record;
mod opcode;

mod runtime_limits;

#[cfg(feature = "flowgraph")]
pub mod flowgraph;

pub use runtime_limits::RuntimeLimits;
pub use {call_frame::CallFrame, code_block::CodeBlock, opcode::Opcode};

pub(crate) use {
    call_frame::GeneratorResumeKind,
    code_block::{
        create_function_object, create_function_object_fast, create_generator_function_object,
        CodeBlockFlags,
    },
    completion_record::CompletionRecord,
    opcode::BindingOpcode,
};

#[cfg(test)]
mod tests;

/// Virtual Machine.
#[derive(Debug)]
pub struct Vm {
    pub(crate) frames: Vec<CallFrame>,
    pub(crate) stack: Vec<JsValue>,
    pub(crate) err: Option<JsError>,
    pub(crate) environments: EnvironmentStack,
    #[cfg(feature = "trace")]
    pub(crate) trace: bool,
    pub(crate) runtime_limits: RuntimeLimits,
    pub(crate) active_function: Option<JsObject>,
    pub(crate) active_runnable: Option<ActiveRunnable>,
}

/// Active runnable in the current vm context.
#[derive(Debug, Clone, Finalize)]
pub(crate) enum ActiveRunnable {
    Script(Script),
    Module(Module),
}

unsafe impl Trace for ActiveRunnable {
    custom_trace!(this, {
        match this {
            Self::Script(script) => mark(script),
            Self::Module(module) => mark(module),
        }
    });
}

impl Vm {
    /// Creates a new virtual machine.
    pub(crate) fn new(global: Gc<DeclarativeEnvironment>) -> Self {
        Self {
            frames: Vec::with_capacity(16),
            stack: Vec::with_capacity(1024),
            environments: EnvironmentStack::new(global),
            err: None,
            #[cfg(feature = "trace")]
            trace: false,
            runtime_limits: RuntimeLimits::default(),
            active_function: None,
            active_runnable: None,
        }
    }

    /// Push a value on the stack.
    pub(crate) fn push<T>(&mut self, value: T)
    where
        T: Into<JsValue>,
    {
        self.stack.push(value.into());
    }

    /// Pop a value off the stack.
    ///
    /// # Panics
    ///
    /// If there is nothing to pop, then this will panic.
    #[track_caller]
    pub(crate) fn pop(&mut self) -> JsValue {
        self.stack.pop().expect("stack was empty")
    }

    #[track_caller]
    pub(crate) fn read<T: Readable>(&mut self) -> T {
        let value = self.frame().code_block.read::<T>(self.frame().pc as usize);
        self.frame_mut().pc += size_of::<T>() as u32;
        value
    }

    /// Retrieves the VM frame
    ///
    /// # Panics
    ///
    /// If there is no frame, then this will panic.
    #[track_caller]
    pub(crate) fn frame(&self) -> &CallFrame {
        self.frames.last().expect("no frame found")
    }

    /// Retrieves the VM frame mutably
    ///
    /// # Panics
    ///
    /// If there is no frame, then this will panic.
    #[track_caller]
    pub(crate) fn frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().expect("no frame found")
    }

    pub(crate) fn push_frame(&mut self, frame: CallFrame) {
        self.frames.push(frame);
    }

    pub(crate) fn pop_frame(&mut self) -> Option<CallFrame> {
        self.frames.pop()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum CompletionType {
    Normal,
    Return,
    Throw,
}

#[cfg(feature = "trace")]
const COLUMN_WIDTH: usize = 26;
#[cfg(feature = "trace")]
const TIME_COLUMN_WIDTH: usize = COLUMN_WIDTH / 2;
#[cfg(feature = "trace")]
const OPCODE_COLUMN_WIDTH: usize = COLUMN_WIDTH;
#[cfg(feature = "trace")]
const OPERAND_COLUMN_WIDTH: usize = COLUMN_WIDTH;
#[cfg(feature = "trace")]
const NUMBER_OF_COLUMNS: usize = 4;

impl Context<'_> {
    /// Runs the current frame to completion, yielding to the execution each time `budget`
    /// "clock cycles" have passed.
    #[allow(clippy::future_not_send)]
    pub(crate) async fn run_async_with_budget(&mut self, budget: usize) -> CompletionRecord {
        let _timer = Profiler::global().start_event("run_async_with_budget", "vm");

        let mut total_cost: usize = 0;

        self.prepare_for_run();

        let completion = loop {
            let completion = self.run_next_instruction(|opcode, context| {
                total_cost = total_cost.saturating_add(usize::from(opcode.cost()));
                opcode.execute(context)
            });
            if let ControlFlow::Break(completion) = completion {
                break completion;
            }
            if total_cost >= budget {
                total_cost = 0;
                yield_now().await;
            }
        };

        self.finish_run(completion)
    }

    /// Runs the current frame to completion.
    pub(crate) fn run(&mut self) -> CompletionRecord {
        let _timer = Profiler::global().start_event("run", "vm");

        self.prepare_for_run();

        let completion = loop {
            if let ControlFlow::Break(completion) = self.run_next_instruction(Opcode::execute) {
                break completion;
            }
        };

        self.finish_run(completion)
    }

    /// Prepares the current frame for running code.
    fn prepare_for_run(&mut self) {
        #[cfg(feature = "trace")]
        if self.vm.trace {
            let msg = if self.vm.frames.last().is_some() {
                " Call Frame "
            } else {
                " VM Start "
            };

            println!(
                "{}\n",
                self.vm
                    .frame()
                    .code_block
                    .to_interned_string(self.interner())
            );
            println!(
                "{msg:-^width$}",
                width = COLUMN_WIDTH * NUMBER_OF_COLUMNS - 10
            );
            println!(
                "{:<TIME_COLUMN_WIDTH$} {:<OPCODE_COLUMN_WIDTH$} {:<OPERAND_COLUMN_WIDTH$} Top Of Stack\n",
                "Time",
                "Opcode",
                "Operands",
            );
        }

        let current_stack_length = self.vm.stack.len();
        self.vm
            .frame_mut()
            .set_frame_pointer(current_stack_length as u32);
    }

    /// Runs `f` for the next instruction in the current frame.
    fn run_next_instruction<F>(&mut self, f: F) -> ControlFlow<CompletionType>
    where
        F: FnOnce(Opcode, &mut Context<'_>) -> JsResult<CompletionType>,
    {
        fn next_op<F>(context: &mut Context<'_>, f: F) -> JsResult<CompletionType>
        where
            F: FnOnce(Opcode, &mut Context<'_>) -> JsResult<CompletionType>,
        {
            let opcode: Opcode = {
                let _timer = Profiler::global().start_event("Opcode retrieval", "vm");

                let frame = context.vm.frame_mut();

                let pc = frame.pc;
                let opcode = Opcode::from(frame.code_block.bytecode[pc as usize]);
                frame.pc += 1;
                opcode
            };

            let _timer = Profiler::global().start_event(opcode.as_instruction_str(), "vm");

            f(opcode, context)
        }

        // 1. Exit the execution loop if program counter ever is equal to or exceeds the amount of instructions
        if self.vm.frame().code_block.bytecode.len() <= self.vm.frame().pc as usize {
            return ControlFlow::Break(CompletionType::Normal);
        }

        #[cfg(feature = "fuzz")]
        {
            if self.instructions_remaining == 0 {
                let err = JsError::from_native(JsNativeError::no_instructions_remain());
                self.vm.err = Some(err);
                return ControlFlow::Break(CompletionType::Return);
            }
            self.instructions_remaining -= 1;
        }

        // 1. Run the next instruction.
        #[cfg(feature = "trace")]
        let result = if self.vm.trace || self.vm.frame().code_block.traceable() {
            let mut pc = self.vm.frame().pc as usize;
            let opcode: Opcode = self
                .vm
                .frame()
                .code_block
                .read::<u8>(pc)
                .try_into()
                .expect("invalid opcode");
            let operands = self
                .vm
                .frame()
                .code_block
                .instruction_operands(&mut pc, self.interner());

            let instant = Instant::now();
            let result = next_op(self, f);

            let duration = instant.elapsed();
            println!(
                    "{:<TIME_COLUMN_WIDTH$} {:<OPCODE_COLUMN_WIDTH$} {operands:<OPERAND_COLUMN_WIDTH$} {}",
                    format!("{}μs", duration.as_micros()),
                    opcode.as_str(),
                    match self.vm.stack.last() {
                        Some(value) if value.is_callable() => "[function]".to_string(),
                        Some(value) if value.is_object() => "[object]".to_string(),
                        Some(value) => value.display().to_string(),
                        None => "<empty>".to_string(),
                    },
                );

            result
        } else {
            next_op(self, f)
        };

        #[cfg(not(feature = "trace"))]
        let result = next_op(self, f);

        // 2. Evaluate the result of executing the instruction.
        match result {
            Ok(CompletionType::Normal) => ControlFlow::Continue(()),
            Ok(CompletionType::Return) => ControlFlow::Break(CompletionType::Return),
            Ok(CompletionType::Throw) => ControlFlow::Break(CompletionType::Throw),
            Err(err) => {
                #[cfg(feature = "fuzz")]
                {
                    if let Some(native_error) = err.as_native() {
                        // If we hit the execution step limit, bubble up the error to the
                        // (Rust) caller instead of trying to handle as an exception.
                        if native_error.is_no_instructions_remain() {
                            self.vm.err = Some(err);
                            return ControlFlow::Break(CompletionType::Throw);
                        }
                    }
                }

                if let Some(native_error) = err.as_native() {
                    // If we hit the execution step limit, bubble up the error to the
                    // (Rust) caller instead of trying to handle as an exception.
                    if native_error.is_runtime_limit() {
                        self.vm.err = Some(err);
                        return ControlFlow::Break(CompletionType::Throw);
                    }
                }

                self.vm.err = Some(err);

                // If this frame has not evaluated the throw as an AbruptCompletion, then evaluate it
                let evaluation = Opcode::Throw
                    .execute(self)
                    .expect("Opcode::Throw cannot return Err");

                if evaluation == CompletionType::Normal {
                    ControlFlow::Continue(())
                } else {
                    ControlFlow::Break(CompletionType::Throw)
                }
            }
        }
    }

    /// Finishes running the current frame, cleaning up and returning errors if necessary.
    fn finish_run(&mut self, completion: CompletionType) -> CompletionRecord {
        // Early return immediately after loop.
        if self.vm.frame().r#yield {
            self.vm.frame_mut().r#yield = false;
            let result = self.vm.pop();
            return CompletionRecord::Return(result);
        }

        #[cfg(feature = "trace")]
        if self.vm.trace {
            println!("\nStack:");
            if self.vm.stack.is_empty() {
                println!("    <empty>");
            } else {
                for (i, value) in self.vm.stack.iter().enumerate() {
                    println!(
                        "{i:04}{:<width$} {}",
                        "",
                        if value.is_callable() {
                            "[function]".to_string()
                        } else if value.is_object() {
                            "[object]".to_string()
                        } else {
                            value.display().to_string()
                        },
                        width = COLUMN_WIDTH / 2 - 4,
                    );
                }
            }
            println!("\n");
        }

        // Determine the execution result
        let execution_result = if completion == CompletionType::Throw {
            self.vm.frame_mut().abrupt_completion = None;
            self.vm.stack.truncate(self.vm.frame().fp as usize);
            JsValue::undefined()
        } else if completion == CompletionType::Return {
            self.vm.frame_mut().abrupt_completion = None;
            let result = self.vm.pop();
            self.vm.stack.truncate(self.vm.frame().fp as usize);
            result
        } else if self.vm.stack.len() <= self.vm.frame().fp as usize {
            JsValue::undefined()
        } else {
            let result = self.vm.pop();
            self.vm.stack.truncate(self.vm.frame().fp as usize);
            result
        };

        // If the current executing function is an async function we have to resolve/reject it's promise at the end.
        // The relevant spec section is 3. in [AsyncBlockStart](https://tc39.es/ecma262/#sec-asyncblockstart).
        if let Some(promise) = self.vm.frame().promise_capability.clone() {
            match completion {
                CompletionType::Normal => {
                    promise
                        .resolve()
                        .call(&JsValue::undefined(), &[], self)
                        .expect("cannot fail per spec");
                }
                CompletionType::Return => {
                    promise
                        .resolve()
                        .call(&JsValue::undefined(), &[execution_result.clone()], self)
                        .expect("cannot fail per spec");
                }
                CompletionType::Throw => {
                    let err = self.vm.err.take().expect("Take must exist on a Throw");
                    promise
                        .reject()
                        .call(&JsValue::undefined(), &[err.to_opaque(self)], self)
                        .expect("cannot fail per spec");
                    self.vm.err = Some(err);
                }
            }
        } else if let Some(generator_object) = self.vm.frame().async_generator.clone() {
            // Step 3.e-g in [AsyncGeneratorStart](https://tc39.es/ecma262/#sec-asyncgeneratorstart)
            let mut generator_object_mut = generator_object.borrow_mut();
            let generator = generator_object_mut
                .as_async_generator_mut()
                .expect("must be async generator");

            generator.state = AsyncGeneratorState::Completed;
            generator.context = None;

            let next = generator
                .queue
                .pop_front()
                .expect("must have item in queue");
            drop(generator_object_mut);

            if completion == CompletionType::Throw {
                AsyncGenerator::complete_step(
                    &next,
                    Err(self
                        .vm
                        .err
                        .take()
                        .expect("err must exist on a Completion::Throw")),
                    true,
                    None,
                    self,
                );
            } else {
                AsyncGenerator::complete_step(&next, Ok(execution_result), true, None, self);
            }
            AsyncGenerator::drain_queue(&generator_object, self);

            return CompletionRecord::Normal(JsValue::undefined());
        }

        // Any valid return statement is re-evaluated as a normal completion vs. return (yield).
        if completion == CompletionType::Throw {
            return CompletionRecord::Throw(
                self.vm
                    .err
                    .take()
                    .expect("Err must exist for a CompletionType::Throw"),
            );
        }
        CompletionRecord::Normal(execution_result)
    }
}

/// Yields once to the executor.
fn yield_now() -> impl Future<Output = ()> {
    struct YieldNow(bool);

    impl Future for YieldNow {
        type Output = ();

        fn poll(mut self: Pin<&mut Self>, cx: &mut task::Context<'_>) -> task::Poll<Self::Output> {
            if self.0 {
                task::Poll::Ready(())
            } else {
                self.0 = true;
                cx.waker().wake_by_ref();
                task::Poll::Pending
            }
        }
    }

    YieldNow(false)
}
