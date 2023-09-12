use crate::{asm::WithSpan, c::compiler::*};

impl CompilerState {
    pub(crate) fn compile_expr_postfix_index(
        &mut self,
        index: &WithSpan<'_, IndexExpr<'_>>,
        dest: Option<&Var>,
        func_name: &str,
    ) -> Result<()> {
        let IndexExpr { arr, idx } = &index.item;
        if let Some(dest) = dest {
            self.compile_expr(&arr.item, Some(dest), func_name)?;
            let (tmp_index_reg, tmp_index) = self
                .functions
                .get_mut(func_name)
                .unwrap()
                .any_reg(TypeSpecifier::Int)
                .unwrap();
            self.compile_expr(&idx.item, Some(&tmp_index), func_name)?;
            {
                let block = self.functions.get_mut(func_name).unwrap().last_block_mut();
                block.sequence.push(Instruction {
                    op: Opcode::Mul,
                    format: InstrFormat::RRI(tmp_index_reg, tmp_index_reg, Immediate::Linked(2)),
                });
            }
            let (tmp_reg, tmp) = self
                .functions
                .get_mut(func_name)
                .unwrap()
                .any_reg(dest.typ())
                .unwrap();
            match dest.storage() {
                VarStorage::Register(dest_reg) => {
                    let block = self.functions.get_mut(func_name).unwrap().last_block_mut();
                    block.sequence.push(Instruction {
                        op: Opcode::Add,
                        format: InstrFormat::RRR(*dest_reg, *dest_reg, tmp_index_reg),
                    });
                    block.sequence.push(Instruction {
                        op: Opcode::Ldl,
                        format: InstrFormat::RRI(tmp_reg, *dest_reg, Immediate::Linked(0)),
                    });
                    block.sequence.push(Instruction {
                        op: Opcode::Ldh,
                        format: InstrFormat::RRI(tmp_reg, *dest_reg, Immediate::Linked(1)),
                    });
                    block.sequence.push(Instruction {
                        op: Opcode::Mov,
                        format: InstrFormat::RR(*dest_reg, tmp_reg),
                    });
                }
                VarStorage::StackOffset(dest_offset) => {
                    let (tmp_dest_reg, tmp_dest) = self
                        .functions
                        .get_mut(func_name)
                        .unwrap()
                        .any_reg(dest.typ())
                        .unwrap();
                    {
                        let block = self.functions.get_mut(func_name).unwrap().last_block_mut();
                        load_fp_offset_to_reg(tmp_dest_reg, *dest_offset, block);
                        block.sequence.push(Instruction {
                            op: Opcode::Add,
                            format: InstrFormat::RRR(tmp_dest_reg, tmp_dest_reg, tmp_index_reg),
                        });
                        block.sequence.push(Instruction {
                            op: Opcode::Ldl,
                            format: InstrFormat::RRI(tmp_reg, tmp_dest_reg, Immediate::Linked(0)),
                        });
                        block.sequence.push(Instruction {
                            op: Opcode::Ldh,
                            format: InstrFormat::RRI(tmp_reg, tmp_dest_reg, Immediate::Linked(1)),
                        });
                        block.sequence.push(Instruction {
                            op: Opcode::Mov,
                            format: InstrFormat::RR(tmp_dest_reg, tmp_reg),
                        });
                        store_reg_to_fp_offset(*dest_offset, tmp_dest_reg, block);
                    }
                    self.functions
                        .get_mut(func_name)
                        .unwrap()
                        .take_back(tmp_dest);
                }
                VarStorage::Immediate(_) => {
                    return Err(Error::from(CompError::InvalidExpression(
                        index.span.location_line() as usize,
                        index.first_line(),
                    )))
                }
            }
            self.functions
                .get_mut(func_name)
                .unwrap()
                .take_back(tmp_index);
            self.functions.get_mut(func_name).unwrap().take_back(tmp);
        } else {
            todo!("compile_expr_postfix_index: dest is None")
        }

        Ok(())
    }
}
