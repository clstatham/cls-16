use crate::{asm::WithSpan, c::compiler::*};

impl CompilerState {
    pub(crate) fn compile_expr_postfix_call(
        &mut self,
        call: &WithSpan<'_, CallExpr<'_>>,
        dest: Option<&Var>,
        func_name: &str,
    ) -> Result<()> {
        let CallExpr { func, args } = &call.item;
        let n_args = args.len() as u16;
        let args_stack_size = n_args * 2;

        for (i, arg_expr) in args.iter().enumerate() {
            let arg_reg = self
                .functions
                .get_mut(func_name)
                .unwrap()
                .any_reg()
                .unwrap();
            let arg = Var::new(
                TypeSpecifier::Int,
                &format!("arg{}", i),
                var::VarStorage::Register(arg_reg),
            );
            self.compile_expr(&arg_expr.item, Some(&arg), func_name)?;
            push_reg_to_stack(
                arg_reg,
                self.functions.get_mut(func_name).unwrap().last_block_mut(),
            );
            self.functions
                .get_mut(func_name)
                .unwrap()
                .take_back_reg(arg_reg);
        }
        let ret_addr_reg = self
            .functions
            .get_mut(func_name)
            .unwrap()
            .any_reg()
            .unwrap();
        let nblock = self.functions.get(func_name).unwrap().code.len();
        let ret_addr_label = format!("{func_name}{}retaddr{}", func.item.0, nblock);
        let ret_addr_block = Block::new(&ret_addr_label);
        {
            let block = self.functions.get_mut(func_name).unwrap().last_block_mut();

            block.sequence.push(Instruction {
                op: Opcode::Mov,
                format: InstrFormat::RI(ret_addr_reg, Immediate::Unlinked(ret_addr_label.clone())),
            });
            push_reg_to_stack(ret_addr_reg, block);

            block.sequence.push(Instruction {
                op: Opcode::Jmp,
                format: InstrFormat::I(Immediate::Unlinked(func.item.0.to_owned())),
            });
        }
        self.functions
            .get_mut(func_name)
            .unwrap()
            .code
            .push(ret_addr_block);

        self.functions
            .get_mut(func_name)
            .unwrap()
            .last_block_mut()
            .sequence
            .push(Instruction {
                op: Opcode::Add,
                format: InstrFormat::RRI(
                    Register::FP,
                    Register::FP,
                    Immediate::Linked(args_stack_size + 2),
                ),
            });
        if let Some(dest) = dest {
            match dest.storage() {
                VarStorage::Register(dest_reg) => {
                    let block = self.functions.get_mut(func_name).unwrap().last_block_mut();
                    block.sequence.push(Instruction {
                        op: Opcode::Mov,
                        format: InstrFormat::RR(*dest_reg, Register::R1),
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
                        let block = self.functions.get_mut(func_name).unwrap().last_block_mut();
                        load_fp_offset_to_reg(tmp_dest, *dest_offset, block);
                        block.sequence.push(Instruction {
                            op: Opcode::Mov,
                            format: InstrFormat::RR(tmp_dest, Register::R1),
                        });
                        store_reg_to_fp_offset(*dest_offset, tmp_dest, block);
                    }
                    self.functions
                        .get_mut(func_name)
                        .unwrap()
                        .take_back_reg(tmp_dest);
                }
                VarStorage::Immediate(_) => {
                    return Err(Error::from(CompError::InvalidExpression(
                        call.span.location_line() as usize,
                        call.first_line(),
                    )))
                }
            }
        }
        Ok(())
    }
}
