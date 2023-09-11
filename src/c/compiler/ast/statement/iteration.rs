use crate::c::compiler::*;

impl CompilerState {
    pub(crate) fn compile_iteration_statement(
        &mut self,
        stmt: &IterationStatement<'_>,
        func_name: &str,
    ) -> Result<()> {
        let nblocks = self.functions.get(func_name).unwrap().code.len();
        let cond_block = Block::new(&format!("{func_name}cond{nblocks}"));
        let cond_label = cond_block.label.clone();
        let body_block = Block::new(&format!("{func_name}body{nblocks}"));
        let step_block = Block::new(&format!("{func_name}step{nblocks}"));
        let end_block = Block::new(&format!("{func_name}end{nblocks}"));
        let init_block = Block::new(&format!("{func_name}init{nblocks}"));
        {
            let func = self.functions.get_mut(func_name).unwrap();
            func.code.push(init_block);
        }
        if let Some(init) = stmt.init.as_ref() {
            self.compile_expr(&init.item, None, func_name)?;
        }
        {
            let func = self.functions.get_mut(func_name).unwrap();
            func.code.push(cond_block);
        }
        let cond = self
            .functions
            .get_mut(func_name)
            .unwrap()
            .push("cond", TypeSpecifier::Int);
        self.compile_expr(&stmt.cond.as_ref().unwrap().item, Some(&cond), func_name)?;
        let tmp_cond = self
            .functions
            .get_mut(func_name)
            .unwrap()
            .any_reg()
            .unwrap();
        {
            let block = self.functions.get_mut(func_name).unwrap().last_block_mut();
            let cond_offset = cond.get_stack_offset().unwrap();
            load_fp_offset_to_reg(tmp_cond, cond_offset, block);
            block.sequence.push(Instruction {
                op: Opcode::Sub,
                format: InstrFormat::RRR(Register::R0, tmp_cond, Register::R0),
            });
            block.sequence.push(Instruction {
                op: Opcode::Jz,
                format: InstrFormat::I(Immediate::Unlinked(end_block.label.clone())),
            });
            block.sequence.push(Instruction {
                op: Opcode::Jmp,
                format: InstrFormat::I(Immediate::Unlinked(body_block.label.clone())),
            });
        }
        self.functions
            .get_mut(func_name)
            .unwrap()
            .take_back_reg(tmp_cond);

        {
            let func = self.functions.get_mut(func_name).unwrap();
            func.code.push(body_block);
            self.compile_statement(&stmt.body.item, func_name)?;
        }
        self.functions
            .get_mut(func_name)
            .unwrap()
            .last_block_mut()
            .sequence
            .push(Instruction {
                op: Opcode::Jmp,
                format: InstrFormat::I(Immediate::Unlinked(step_block.label.clone())),
            });
        {
            let func = self.functions.get_mut(func_name).unwrap();
            func.code.push(step_block);
            if let Some(step) = stmt.step.as_ref() {
                self.compile_expr(&step.item, None, func_name)?;
            }
        }
        self.functions
            .get_mut(func_name)
            .unwrap()
            .last_block_mut()
            .sequence
            .push(Instruction {
                op: Opcode::Jmp,
                format: InstrFormat::I(Immediate::Unlinked(cond_label.clone())),
            });
        {
            let func = self.functions.get_mut(func_name).unwrap();
            func.code.push(end_block);
        }

        Ok(())
    }
}
