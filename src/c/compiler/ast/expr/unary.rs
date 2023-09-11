use crate::{asm::WithSpan, c::compiler::*};

impl CompilerState {
    pub(crate) fn compile_expr_unary(
        &mut self,
        unary: &WithSpan<'_, UnaryExpr<'_>>,
        dest: Option<&Var>,
        func_name: &str,
    ) -> Result<()> {
        if let Some(dest) = dest {
            match unary.item.op.item {
                UnaryOp::AddrOf => {
                    if let Expr::Ident(name) = unary.item.expr.item {
                        let src = self
                            .functions
                            .get_mut(func_name)
                            .unwrap()
                            .get(name.item.0)
                            .unwrap();
                        if let VarStorage::StackOffset(src_offset) = src.storage() {
                            match dest.storage() {
                                VarStorage::Register(dest_reg) => {
                                    let block =
                                        self.functions.get_mut(func_name).unwrap().last_block_mut();
                                    block.sequence.push(Instruction {
                                        op: Opcode::Mov,
                                        format: InstrFormat::RR(*dest_reg, Register::FP),
                                    });
                                    block.sequence.push(Instruction {
                                        op: Opcode::Add,
                                        format: InstrFormat::RRI(
                                            *dest_reg,
                                            *dest_reg,
                                            Immediate::Linked(*src_offset),
                                        ),
                                    });
                                }
                                VarStorage::StackOffset(dest_offset) => {
                                    let tmp_dest = self
                                        .functions
                                        .get_mut(func_name)
                                        .unwrap()
                                        .any_reg()
                                        .unwrap();
                                    {
                                        let block = self
                                            .functions
                                            .get_mut(func_name)
                                            .unwrap()
                                            .last_block_mut();
                                        load_fp_offset_to_reg(tmp_dest, *dest_offset, block);
                                        block.sequence.push(Instruction {
                                            op: Opcode::Mov,
                                            format: InstrFormat::RR(tmp_dest, Register::FP),
                                        });
                                        block.sequence.push(Instruction {
                                            op: Opcode::Add,
                                            format: InstrFormat::RRI(
                                                tmp_dest,
                                                tmp_dest,
                                                Immediate::Linked(*src_offset),
                                            ),
                                        });
                                        store_reg_to_fp_offset(*dest_offset, tmp_dest, block);
                                    }
                                }
                                VarStorage::Immediate(_) => {
                                    return Err(Error::from(CompError::InvalidExpression(
                                        unary.span.location_line() as usize,
                                        unary.first_line(),
                                    )))
                                }
                            }
                        } else {
                            return Err(Error::from(CompError::InvalidExpression(
                                unary.span.location_line() as usize,
                                unary.first_line(),
                            )));
                        }
                    } else {
                        return Err(Error::from(CompError::InvalidExpression(
                            unary.span.location_line() as usize,
                            unary.first_line(),
                        )));
                    }
                }
                UnaryOp::Deref => {
                    self.compile_expr(&unary.item.expr.item, Some(dest), func_name)?;
                    let func = self.functions.get_mut(func_name).unwrap();
                    let tmp_reg = func.any_reg().unwrap();

                    match dest.storage() {
                        VarStorage::Register(dest_reg) => {
                            let block = func.last_block_mut();
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
                            let tmp_dest = func.any_reg().unwrap();
                            {
                                let block = func.last_block_mut();
                                load_fp_offset_to_reg(tmp_dest, *dest_offset, block);
                                block.sequence.push(Instruction {
                                    op: Opcode::Ldl,
                                    format: InstrFormat::RRI(
                                        tmp_reg,
                                        tmp_dest,
                                        Immediate::Linked(0),
                                    ),
                                });
                                block.sequence.push(Instruction {
                                    op: Opcode::Ldh,
                                    format: InstrFormat::RRI(
                                        tmp_reg,
                                        tmp_dest,
                                        Immediate::Linked(1),
                                    ),
                                });
                                block.sequence.push(Instruction {
                                    op: Opcode::Mov,
                                    format: InstrFormat::RR(tmp_dest, tmp_reg),
                                });
                                store_reg_to_fp_offset(*dest_offset, tmp_dest, block);
                            }
                            func.take_back_reg(tmp_dest);
                        }
                        VarStorage::Immediate(_) => {
                            return Err(Error::from(CompError::InvalidExpression(
                                unary.span.location_line() as usize,
                                unary.first_line(),
                            )))
                        }
                    }
                    func.take_back_reg(tmp_reg);
                }
                UnaryOp::Plus => todo!(),
                UnaryOp::Minus => todo!(),
                UnaryOp::Tilde => todo!(),
                UnaryOp::Not => todo!(),
            }
        } else if let UnaryOp::Deref = unary.item.op.item {
            match &unary.item.expr.item {
                Expr::Infix(infix) => match infix.item.op.item {
                    InfixOp::Assign
                    | InfixOp::MulAssign
                    | InfixOp::AddAssign
                    | InfixOp::SubAssign
                    | InfixOp::DivAssign
                    | InfixOp::AndAssign
                    | InfixOp::OrAssign
                    | InfixOp::XorAssign => {
                        // this holds the address of the pointer
                        let dest_addr_reg = self
                            .functions
                            .get_mut(func_name)
                            .unwrap()
                            .any_reg()
                            .unwrap();
                        let dest_addr = Var::new(
                            TypeSpecifier::Int,
                            "dest_addr",
                            var::VarStorage::Register(dest_addr_reg),
                        );
                        self.compile_expr(&infix.item.lhs.item, Some(&dest_addr), func_name)?;
                        let rhs_reg = self
                            .functions
                            .get_mut(func_name)
                            .unwrap()
                            .any_reg()
                            .unwrap();
                        let rhs = Var::new(
                            TypeSpecifier::Int,
                            "rhs",
                            var::VarStorage::Register(rhs_reg),
                        );
                        self.compile_expr(&infix.item.rhs.item, Some(&rhs), func_name)?;

                        let func = self.functions.get_mut(func_name).unwrap();
                        {
                            let block = func.last_block_mut();
                            block.sequence.push(Instruction {
                                op: Opcode::Stl,
                                format: InstrFormat::RRI(
                                    dest_addr_reg,
                                    rhs_reg,
                                    Immediate::Linked(0),
                                ),
                            });
                            block.sequence.push(Instruction {
                                op: Opcode::Sth,
                                format: InstrFormat::RRI(
                                    dest_addr_reg,
                                    rhs_reg,
                                    Immediate::Linked(1),
                                ),
                            });
                        }
                        func.take_back_reg(rhs_reg);
                        func.take_back_reg(dest_addr_reg);
                    }
                    _ => todo!(),
                },
                _ => unreachable!(),
            }
        } else {
            return Err(Error::from(CompError::InvalidExpression(
                unary.span.location_line() as usize,
                unary.first_line(),
            )));
        }
        Ok(())
    }
}
