use crate::{
    vm::{opcode::Operation, CompletionType},
    Context, JsResult, JsString,
};

/// `ConcatToString` implements the Opcode Operation for `Opcode::ConcatToString`
///
/// Operation:
///  - Concat multiple stack objects into a string.
#[derive(Debug, Clone, Copy)]
pub(crate) struct ConcatToString;

// TODO: This should probably be decomposed into smaller ops.
impl Operation for ConcatToString {
    const NAME: &'static str = "ConcatToString";
    const INSTRUCTION: &'static str = "INST - ConcatToString";
    const COST: usize = 6;

    fn execute(context: &mut Context<'_>) -> JsResult<CompletionType> {
        let value_count = context.vm.read::<u32>();
        let mut strings = Vec::with_capacity(value_count as usize);
        for _ in 0..value_count {
            strings.push(context.vm.pop().to_string(context)?);
        }
        strings.reverse();
        let s = JsString::concat_array(
            &strings
                .iter()
                .map(JsString::as_slice)
                .collect::<Vec<&[u16]>>(),
        );
        context.vm.push(s);
        Ok(CompletionType::Normal)
    }
}
