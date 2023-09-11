use crate::c::compiler::*;

impl CompilerState {
    pub(crate) fn compile_builtin_statement(
        &mut self,
        stmt: &BuiltinStatement<'_>,
        func_name: &str,
    ) -> Result<()> {
        match stmt.builtin.item {
            Keyword::Printi => {
                let dest = self
                    .functions
                    .get_mut(func_name)
                    .unwrap()
                    .push("dest", TypeSpecifier::Int);
                self.compile_expr(&stmt.expr.item, Some(&dest), func_name)?;
                let tmp_dest = self
                    .functions
                    .get_mut(func_name)
                    .unwrap()
                    .any_reg()
                    .unwrap();
                let block = self.functions.get_mut(func_name).unwrap().last_block_mut();

                load_fp_offset_to_reg(tmp_dest, dest.get_stack_offset().unwrap(), block);
                block.sequence.push(Instruction {
                    op: Opcode::Printi,
                    format: InstrFormat::R(tmp_dest),
                });
                self.functions.get_mut(func_name).unwrap().take_back(dest);
                self.functions
                    .get_mut(func_name)
                    .unwrap()
                    .take_back_reg(tmp_dest);
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
