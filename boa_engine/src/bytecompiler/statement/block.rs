use crate::{bytecompiler::ByteCompiler, vm::Opcode};
use boa_ast::statement::Block;

impl ByteCompiler<'_, '_> {
    /// Compile a [`Block`] `boa_ast` node
    pub(crate) fn compile_block(&mut self, block: &Block, use_expr: bool) {
        self.push_compile_environment(false);

        let push_env = (!self.can_optimize_local_variables)
            .then(|| self.emit_opcode_with_operand(Opcode::PushDeclarativeEnvironment));

        self.block_declaration_instantiation(block);
        self.compile_statement_list(block.statement_list(), use_expr, true);

        let env_index = self.pop_compile_environment();

        if let Some(push_env) = push_env {
            self.patch_jump_with_target(push_env, env_index);
            self.emit_opcode(Opcode::PopEnvironment);
        }
    }
}
