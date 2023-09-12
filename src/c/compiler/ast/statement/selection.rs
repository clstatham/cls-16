use crate::c::compiler::*;

impl CompilerState {
    pub(crate) fn compile_selection_statement(
        &mut self,
        stmt: &SelectionStatement<'_>,
        func_name: &str,
    ) -> Result<()> {
        let cond = self
            .functions
            .get_mut(func_name)
            .unwrap()
            .push(Some("cond"), TypeSpecifier::Int);
        self.compile_expr(&stmt.cond.item, Some(&cond), func_name)?;
        let nblocks = self.functions.get(func_name).unwrap().code.len();
        let if_block = Block::new(&format!("{func_name}if{nblocks}"));
        let else_block = Block::new(&format!("{func_name}else{nblocks}"));
        let end_block = Block::new(&format!("{func_name}end{nblocks}"));

        let (tmp_cond_reg, tmp_cond) = self
            .functions
            .get_mut(func_name)
            .unwrap()
            .any_reg(TypeSpecifier::Int)
            .unwrap();
        {
            let block = self.functions.get_mut(func_name).unwrap().last_block_mut();
            let cond_offset = cond.get_stack_offset().unwrap();
            load_fp_offset_to_reg(tmp_cond_reg, cond_offset, block);
            block.sequence.push(Instruction {
                op: Opcode::Sub,
                format: InstrFormat::RRR(Register::R0, tmp_cond_reg, Register::R0),
            });
            block.sequence.push(Instruction {
                op: Opcode::Jz,
                format: InstrFormat::I(Immediate::Unlinked(else_block.label.clone())),
            });
            block.sequence.push(Instruction {
                op: Opcode::Jmp,
                format: InstrFormat::I(Immediate::Unlinked(if_block.label.clone())),
            });
        }
        self.functions
            .get_mut(func_name)
            .unwrap()
            .take_back(tmp_cond);

        {
            let func = self.functions.get_mut(func_name).unwrap();
            func.code.push(if_block);
            self.compile_statement(&stmt.if_body.item, func_name)?;
        }
        self.functions
            .get_mut(func_name)
            .unwrap()
            .last_block_mut()
            .sequence
            .push(Instruction {
                op: Opcode::Jmp,
                format: InstrFormat::I(Immediate::Unlinked(end_block.label.clone())),
            });
        if let Some(else_body) = &stmt.else_body {
            {
                let func = self.functions.get_mut(func_name).unwrap();
                func.code.push(else_block);
                self.compile_statement(&else_body.item, func_name)?;
            }
        } else {
            {
                let func = self.functions.get_mut(func_name).unwrap();
                func.code.push(else_block);
            }
        }
        self.functions
            .get_mut(func_name)
            .unwrap()
            .last_block_mut()
            .sequence
            .push(Instruction {
                op: Opcode::Jmp,
                format: InstrFormat::I(Immediate::Unlinked(end_block.label.clone())),
            });
        {
            let func = self.functions.get_mut(func_name).unwrap();
            func.code.push(end_block);
        }

        Ok(())
    }
}
