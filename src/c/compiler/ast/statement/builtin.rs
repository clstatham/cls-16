use crate::c::compiler::*;

impl CompilerState {
    pub(crate) fn compile_builtin_statement(
        &mut self,
        stmt: &BuiltinStatement<'_>,
        func_name: &str,
    ) -> Result<()> {
        match stmt.builtin.item {
            Keyword::Printi => {
                let (tmp_dest_reg, tmp_dest) = self
                    .functions
                    .get_mut(func_name)
                    .unwrap()
                    .any_reg(TypeSpecifier::Int)
                    .unwrap();
                self.compile_expr(&stmt.expr.item, Some(&tmp_dest), func_name)?;
                let block = self.functions.get_mut(func_name).unwrap().last_block_mut();

                block.sequence.push(Instruction {
                    op: Opcode::Printi,
                    format: InstrFormat::R(tmp_dest_reg),
                });
                self.functions
                    .get_mut(func_name)
                    .unwrap()
                    .take_back(tmp_dest);
            }
            _ => {
                return Err(Error::from(CompError::InvalidStatement(
                    stmt.builtin.span.location_line() as usize,
                    stmt.builtin.first_line(),
                )))
            }
        }
        Ok(())
    }
}
