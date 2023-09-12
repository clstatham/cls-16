use crate::{asm::WithSpan, c::compiler::*};

impl CompilerState {
    pub(crate) fn compile_expr_infix(
        &mut self,
        infix: &WithSpan<'_, InfixExpr<'_>>,
        dest: Option<&Var>,
        func_name: &str,
    ) -> Result<()> {
        let (tmp_rhs_reg, rhs) = self
            .functions
            .get_mut(func_name)
            .unwrap()
            .any_reg(TypeSpecifier::Void)
            .unwrap();
        let (tmp_lhs_reg, lhs) = self
            .functions
            .get_mut(func_name)
            .unwrap()
            .any_reg(TypeSpecifier::Void)
            .unwrap();

        if let Some(dest) = dest {
            self.compile_expr(&infix.item.lhs.item, Some(&lhs), func_name)?;
            self.compile_expr(&infix.item.rhs.item, Some(&rhs), func_name)?;
            match &infix.item.op.item {
                InfixOp::Assign => unreachable!(),
                InfixOp::Add
                | InfixOp::Div
                | InfixOp::Mul
                | InfixOp::Sub
                | InfixOp::And
                | InfixOp::Or
                | InfixOp::Xor
                | InfixOp::AndAnd
                | InfixOp::OrOr => {
                    let op = match &infix.item.op.item {
                        InfixOp::Add => Opcode::Add,
                        InfixOp::Sub => Opcode::Sub,
                        InfixOp::Mul => Opcode::Mul,
                        InfixOp::Div => Opcode::Div,
                        InfixOp::And => Opcode::And,
                        InfixOp::Or => Opcode::Or,
                        InfixOp::Xor => Opcode::Xor,
                        InfixOp::AndAnd => Opcode::And,
                        InfixOp::OrOr => Opcode::Or,
                        _ => unreachable!(),
                    };

                    match dest.storage() {
                        VarStorage::Register(dest_reg) => {
                            let block = self.functions.get_mut(func_name).unwrap().last_block_mut();
                            block.sequence.push(Instruction {
                                op,
                                format: InstrFormat::RRR(*dest_reg, tmp_lhs_reg, tmp_rhs_reg),
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
                                let block =
                                    self.functions.get_mut(func_name).unwrap().last_block_mut();
                                load_fp_offset_to_reg(tmp_dest_reg, *dest_offset, block);
                                block.sequence.push(Instruction {
                                    op,
                                    format: InstrFormat::RRR(
                                        tmp_dest_reg,
                                        tmp_lhs_reg,
                                        tmp_rhs_reg,
                                    ),
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
                                infix.span.location_line() as usize,
                                infix.first_line(),
                            )))
                        }
                    }
                }
                InfixOp::EqEq
                | InfixOp::NotEqual
                | InfixOp::Lt
                | InfixOp::LtEq
                | InfixOp::Gt
                | InfixOp::GtEq => {
                    let (tmp_dest_reg, tmp_dest) = self
                        .functions
                        .get_mut(func_name)
                        .unwrap()
                        .any_reg(dest.typ())
                        .unwrap();
                    let nblock = self.functions.get(func_name).unwrap().code.len();
                    let true_block = Block::new(&format!("{func_name}true{nblock}",));
                    let false_block = Block::new(&format!("{func_name}false{nblock}",));
                    let end_block = Block::new(&format!("{func_name}end{nblock}",));
                    {
                        let block = self.functions.get_mut(func_name).unwrap().last_block_mut();
                        block.sequence.push(Instruction {
                            op: Opcode::Sub,
                            format: InstrFormat::RRR(tmp_dest_reg, tmp_lhs_reg, tmp_rhs_reg),
                        });
                        match infix.item.op.item {
                            InfixOp::EqEq => {
                                block.sequence.push(Instruction {
                                    op: Opcode::Jz,
                                    format: InstrFormat::I(Immediate::Unlinked(
                                        true_block.label.clone(),
                                    )),
                                });
                                block.sequence.push(Instruction {
                                    op: Opcode::Jmp,
                                    format: InstrFormat::I(Immediate::Unlinked(
                                        false_block.label.clone(),
                                    )),
                                });
                            }
                            InfixOp::NotEqual => {
                                block.sequence.push(Instruction {
                                    op: Opcode::Jz,
                                    format: InstrFormat::I(Immediate::Unlinked(
                                        false_block.label.clone(),
                                    )),
                                });
                                block.sequence.push(Instruction {
                                    op: Opcode::Jmp,
                                    format: InstrFormat::I(Immediate::Unlinked(
                                        true_block.label.clone(),
                                    )),
                                });
                            }
                            InfixOp::Lt => {
                                block.sequence.push(Instruction {
                                    op: Opcode::Jc,
                                    format: InstrFormat::I(Immediate::Unlinked(
                                        true_block.label.clone(),
                                    )),
                                });
                                block.sequence.push(Instruction {
                                    op: Opcode::Jmp,
                                    format: InstrFormat::I(Immediate::Unlinked(
                                        false_block.label.clone(),
                                    )),
                                });
                            }
                            InfixOp::Gt => {
                                block.sequence.push(Instruction {
                                    op: Opcode::Jc,
                                    format: InstrFormat::I(Immediate::Unlinked(
                                        false_block.label.clone(),
                                    )),
                                });
                                block.sequence.push(Instruction {
                                    op: Opcode::Jz,
                                    format: InstrFormat::I(Immediate::Unlinked(
                                        false_block.label.clone(),
                                    )),
                                });
                                block.sequence.push(Instruction {
                                    op: Opcode::Jmp,
                                    format: InstrFormat::I(Immediate::Unlinked(
                                        true_block.label.clone(),
                                    )),
                                });
                            }
                            InfixOp::LtEq => {
                                block.sequence.push(Instruction {
                                    op: Opcode::Jc,
                                    format: InstrFormat::I(Immediate::Unlinked(
                                        true_block.label.clone(),
                                    )),
                                });
                                block.sequence.push(Instruction {
                                    op: Opcode::Jz,
                                    format: InstrFormat::I(Immediate::Unlinked(
                                        true_block.label.clone(),
                                    )),
                                });
                                block.sequence.push(Instruction {
                                    op: Opcode::Jmp,
                                    format: InstrFormat::I(Immediate::Unlinked(
                                        false_block.label.clone(),
                                    )),
                                });
                            }
                            InfixOp::GtEq => {
                                block.sequence.push(Instruction {
                                    op: Opcode::Jc,
                                    format: InstrFormat::I(Immediate::Unlinked(
                                        false_block.label.clone(),
                                    )),
                                });
                                block.sequence.push(Instruction {
                                    op: Opcode::Jmp,
                                    format: InstrFormat::I(Immediate::Unlinked(
                                        true_block.label.clone(),
                                    )),
                                });
                            }
                            _ => unreachable!(),
                        }
                    }
                    self.functions
                        .get_mut(func_name)
                        .unwrap()
                        .code
                        .push(true_block);
                    {
                        let block = self.functions.get_mut(func_name).unwrap().last_block_mut();
                        block.sequence.push(Instruction {
                            op: Opcode::Mov,
                            format: InstrFormat::RI(tmp_dest_reg, Immediate::Linked(1)),
                        });
                        block.sequence.push(Instruction {
                            op: Opcode::Jmp,
                            format: InstrFormat::I(Immediate::Unlinked(end_block.label.clone())),
                        });
                    }
                    self.functions
                        .get_mut(func_name)
                        .unwrap()
                        .code
                        .push(false_block);
                    {
                        let block = self.functions.get_mut(func_name).unwrap().last_block_mut();
                        block.sequence.push(Instruction {
                            op: Opcode::Mov,
                            format: InstrFormat::RI(tmp_dest_reg, Immediate::Linked(0)),
                        });
                    }
                    self.functions
                        .get_mut(func_name)
                        .unwrap()
                        .code
                        .push(end_block);
                    match dest.storage() {
                        VarStorage::Register(dest_reg) => {
                            let block = self.functions.get_mut(func_name).unwrap().last_block_mut();
                            block.sequence.push(Instruction {
                                op: Opcode::Mov,
                                format: InstrFormat::RR(*dest_reg, tmp_dest_reg),
                            });
                        }
                        VarStorage::StackOffset(dest_offset) => {
                            let block = self.functions.get_mut(func_name).unwrap().last_block_mut();
                            store_reg_to_fp_offset(*dest_offset, tmp_dest_reg, block);
                        }
                        VarStorage::Immediate(_) => {
                            return Err(Error::from(CompError::InvalidExpression(
                                infix.span.location_line() as usize,
                                infix.first_line(),
                            )))
                        }
                    }
                    self.functions
                        .get_mut(func_name)
                        .unwrap()
                        .take_back(tmp_dest);
                }
                InfixOp::Mod => todo!(),

                InfixOp::Shl => todo!(),
                InfixOp::Shr => todo!(),

                _ => unreachable!(),
            }
        } else {
            match infix.item.op.item {
                InfixOp::Assign => {
                    if let Expr::Ident(lhs) = &infix.item.lhs.item {
                        let dest = self
                            .functions
                            .get(func_name)
                            .unwrap()
                            .get_by_name(lhs.item.0)
                            .unwrap();
                        self.compile_expr(&infix.item.rhs.item, Some(&dest), func_name)?;
                    } else if let Expr::Postfix(postfix) = &infix.item.lhs.item {
                        if let PostfixExpr::Index(index) = &postfix.item {
                            if let Expr::Ident(id) = &index.item.arr.item {
                                // arrays in C are just pointers to some some stack memory
                                let arr = self
                                    .functions
                                    .get(func_name)
                                    .unwrap()
                                    .get_by_name(id.item.0)
                                    .unwrap();
                                let (tmp_index_reg, tmp_index) = self
                                    .functions
                                    .get_mut(func_name)
                                    .unwrap()
                                    .any_reg(TypeSpecifier::Int)
                                    .unwrap();
                                self.compile_expr(
                                    &index.item.idx.item,
                                    Some(&tmp_index),
                                    func_name,
                                )?;
                                {
                                    let block =
                                        self.functions.get_mut(func_name).unwrap().last_block_mut();
                                    block.sequence.push(Instruction {
                                        op: Opcode::Mul,
                                        format: InstrFormat::RRI(
                                            tmp_index_reg,
                                            tmp_index_reg,
                                            Immediate::Linked(2),
                                        ),
                                    });
                                }
                                let (tmp_dest_reg, tmp_dest) = self
                                    .functions
                                    .get_mut(func_name)
                                    .unwrap()
                                    .any_reg(arr.typ())
                                    .unwrap();
                                {
                                    let block =
                                        self.functions.get_mut(func_name).unwrap().last_block_mut();
                                    load_fp_offset_to_reg(
                                        tmp_dest_reg,
                                        arr.get_stack_offset().unwrap(),
                                        block,
                                    );
                                    block.sequence.push(Instruction {
                                        op: Opcode::Add,
                                        format: InstrFormat::RRR(
                                            tmp_dest_reg,
                                            tmp_dest_reg,
                                            tmp_index_reg,
                                        ),
                                    });
                                }
                                self.functions
                                    .get_mut(func_name)
                                    .unwrap()
                                    .take_back(tmp_index);
                                self.compile_expr(&infix.item.rhs.item, Some(&rhs), func_name)?;
                                {
                                    let block =
                                        self.functions.get_mut(func_name).unwrap().last_block_mut();
                                    block.sequence.push(Instruction {
                                        op: Opcode::Stl,
                                        format: InstrFormat::RRI(
                                            tmp_dest_reg,
                                            tmp_rhs_reg,
                                            Immediate::Linked(0),
                                        ),
                                    });
                                    block.sequence.push(Instruction {
                                        op: Opcode::Sth,
                                        format: InstrFormat::RRI(
                                            tmp_dest_reg,
                                            tmp_rhs_reg,
                                            Immediate::Linked(1),
                                        ),
                                    });
                                }
                                self.functions
                                    .get_mut(func_name)
                                    .unwrap()
                                    .take_back(tmp_dest);
                            } else {
                                todo!("postfix assign to index of non-ident")
                            }
                        } else {
                            todo!("postfix assign to non-index")
                        }
                    } else {
                        return Err(Error::from(CompError::InvalidExpression(
                            infix.span.location_line() as usize,
                            infix.first_line(),
                        )));
                    }
                }
                InfixOp::ShlAssign => todo!(),
                InfixOp::ShrAssign => todo!(),
                InfixOp::MulAssign
                | InfixOp::AddAssign
                | InfixOp::SubAssign
                | InfixOp::DivAssign
                | InfixOp::AndAssign
                | InfixOp::OrAssign
                | InfixOp::XorAssign => {
                    if let Expr::Ident(lhs) = &infix.item.lhs.item {
                        let dest = self
                            .functions
                            .get(func_name)
                            .unwrap()
                            .get_by_name(lhs.item.0)
                            .unwrap();
                        self.compile_expr(&infix.item.rhs.item, Some(&rhs), func_name)?;
                        let op = match infix.item.op.item {
                            InfixOp::AddAssign => Opcode::Add,
                            InfixOp::SubAssign => Opcode::Sub,
                            InfixOp::MulAssign => Opcode::Mul,
                            InfixOp::DivAssign => Opcode::Div,
                            InfixOp::AndAssign => Opcode::And,
                            InfixOp::OrAssign => Opcode::Or,
                            InfixOp::XorAssign => Opcode::Xor,
                            _ => unreachable!(),
                        };
                        {
                            match dest.storage() {
                                VarStorage::Register(dest_reg) => {
                                    let block =
                                        self.functions.get_mut(func_name).unwrap().last_block_mut();
                                    block.sequence.push(Instruction {
                                        op,
                                        format: InstrFormat::RRR(*dest_reg, *dest_reg, tmp_rhs_reg),
                                    });
                                }
                                VarStorage::StackOffset(dest_offset) => {
                                    let (tmp_dest_reg, tmp_dest) = self
                                        .functions
                                        .get_mut(func_name)
                                        .unwrap()
                                        .any_reg(dest.typ())
                                        .unwrap();
                                    let block =
                                        self.functions.get_mut(func_name).unwrap().last_block_mut();
                                    load_fp_offset_to_reg(tmp_dest_reg, *dest_offset, block);
                                    block.sequence.push(Instruction {
                                        op,
                                        format: InstrFormat::RRR(
                                            tmp_dest_reg,
                                            tmp_dest_reg,
                                            tmp_rhs_reg,
                                        ),
                                    });
                                    store_reg_to_fp_offset(*dest_offset, tmp_dest_reg, block);
                                    self.functions
                                        .get_mut(func_name)
                                        .unwrap()
                                        .take_back(tmp_dest);
                                }
                                VarStorage::Immediate(_) => {
                                    return Err(Error::from(CompError::InvalidExpression(
                                        infix.span.location_line() as usize,
                                        infix.first_line(),
                                    )))
                                }
                            }
                        }
                    } else {
                        return Err(Error::from(CompError::InvalidExpression(
                            infix.span.location_line() as usize,
                            infix.first_line(),
                        )));
                    }
                }

                _ => unreachable!(),
            }
        }
        self.functions.get_mut(func_name).unwrap().take_back(rhs);
        self.functions.get_mut(func_name).unwrap().take_back(lhs);
        Ok(())
    }
}
