use crate::{asm::WithSpan, c::compiler::*};

impl CompilerState {
    pub(crate) fn compile_expr_constant(
        &mut self,
        val: &WithSpan<'_, Constant>,
        dest: Option<&Var>,
        func_name: &str,
    ) -> Result<()> {
        if let Some(dest) = dest {
            let func = self.functions.get_mut(func_name).unwrap();

            match dest.storage() {
                VarStorage::Immediate(_) => {
                    return Err(Error::from(CompError::InvalidExpression(
                        val.span.location_line() as usize,
                        val.first_line(),
                    )))
                }
                VarStorage::Register(reg) => {
                    let Constant::Integer(val) = val.item;
                    func.last_block_mut().sequence.push(Instruction {
                        op: Opcode::Mov,
                        format: InstrFormat::RI(*reg, Immediate::Linked(val)),
                    })
                }
                VarStorage::StackOffset(addr_offset) => {
                    let Constant::Integer(val) = val.item;
                    let tmp_reg = func.any_reg().unwrap();
                    {
                        let block = func.last_block_mut();
                        block.sequence.push(Instruction {
                            op: Opcode::Mov,
                            format: InstrFormat::RI(tmp_reg, Immediate::Linked(val)),
                        });
                        store_reg_to_fp_offset(*addr_offset, tmp_reg, block);
                    }
                    func.take_back_reg(tmp_reg);
                }
            }
        } else {
            return Err(Error::from(CompError::InvalidExpression(
                val.span.location_line() as usize,
                val.first_line(),
            )));
        }
        Ok(())
    }
}
