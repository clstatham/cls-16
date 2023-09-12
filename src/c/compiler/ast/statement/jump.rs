use crate::c::compiler::*;

impl CompilerState {
    pub(crate) fn compile_jump_statement(
        &mut self,
        stmt: &JumpStatement<'_>,
        func_name: &str,
    ) -> Result<()> {
        match stmt {
            JumpStatement::Goto(_label) => todo!(),
            JumpStatement::Return(value) => {
                if let Some(expr) = value.as_ref().map(|v| &v.item) {
                    let rettype = self.functions.get(func_name).unwrap().return_type.clone();
                    let dest = self
                        .functions
                        .get_mut(func_name)
                        .unwrap()
                        .push(Some("returnval"), rettype);
                    self.compile_expr(expr, Some(&dest), func_name)?;
                    let (tmp_reg, tmp) = self
                        .functions
                        .get_mut(func_name)
                        .unwrap()
                        .any_reg(TypeSpecifier::Int)
                        .unwrap();
                    {
                        let block = self.functions.get_mut(func_name).unwrap().last_block_mut();
                        load_fp_offset_to_reg(tmp_reg, dest.get_stack_offset().unwrap(), block);
                        block.sequence.push(Instruction {
                            op: Opcode::Mov,
                            format: InstrFormat::RR(Register::R1, tmp_reg),
                        });
                    }
                    self.functions.get_mut(func_name).unwrap().take_back(tmp);
                }
            }
        }
        Ok(())
    }
}
