use crate::{vm::CompletionType, Context, JsResult};

use super::{Opcode, Operation};

/// `Wide` implements the Opcode Operation for `Opcode::Wide`
///
/// Operation:
///  - TODO: doc
#[derive(Debug, Clone, Copy)]
pub(crate) struct Wide;

impl Operation for Wide {
    const NAME: &'static str = "Wide";
    const INSTRUCTION: &'static str = "INST - Wide";

    fn execute(context: &mut Context<'_>) -> JsResult<CompletionType> {
        let opcode = context.vm.read::<u8>() as usize;

        Opcode::EXECUTE_FNS[256 + opcode](context)
    }
}
