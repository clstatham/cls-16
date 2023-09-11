use crate::c::compiler::*;

impl CompilerState {
    pub(crate) fn optimize_functions(&mut self) -> Result<()> {
        // function-level optimizations
        for (_, func) in self.functions.iter_mut() {
            // register promotion
            // if the function has spare registers, we can promote stack variables to registers
            for reg in func.avail_regs.drain() {
                for var in func.allocations.values_mut() {
                    if let VarStorage::StackOffset(orig_offset) = var.storage() {
                        let mut changed = true;
                        while changed {
                            changed = false;
                            for block in func.code.iter_mut() {
                                let mut i = 0;
                                while i < block.sequence.len() {
                                    let instr = &block.sequence[i];

                                    if let Some(next) = block.sequence.get(i + 1) {
                                        // replace all loads from the stack with loads from the register
                                        // convert the following pattern:
                                        // ldl r1, fp, X
                                        // ldh r1, fp, Y
                                        // into:
                                        // mov r1, (reg)
                                        if instr.op == Opcode::Ldl && next.op == Opcode::Ldh {
                                            if let InstrFormat::RRI(
                                                ldl_dest,
                                                ldl_src,
                                                ref ldl_offset,
                                            ) = instr.format
                                            {
                                                if let InstrFormat::RRI(
                                                    ldh_dest,
                                                    ldh_src,
                                                    ref ldh_offset,
                                                ) = next.format
                                                {
                                                    if ldl_dest == ldh_dest
                                                        && ldl_src == ldh_src
                                                        && ldl_src == Register::FP
                                                        && ldl_offset.get_linked().unwrap()
                                                            == *orig_offset
                                                        && ldh_offset.get_linked().unwrap()
                                                            == *orig_offset + 1
                                                    {
                                                        block.sequence.remove(i + 1);
                                                        block.sequence.remove(i);
                                                        block.sequence.insert(
                                                            i,
                                                            Instruction {
                                                                op: Opcode::Mov,
                                                                format: InstrFormat::RR(
                                                                    ldl_dest, reg,
                                                                ),
                                                            },
                                                        );
                                                        changed = true;
                                                        continue;
                                                    }
                                                }
                                            }
                                        }
                                        // replace all stores to the stack with stores to the register
                                        // convert the following pattern:
                                        // stl fp, r1, X
                                        // sth fp, r1, Y
                                        // into:
                                        // mov (reg), r1
                                        if instr.op == Opcode::Stl && next.op == Opcode::Sth {
                                            if let InstrFormat::RRI(
                                                stl_dest,
                                                stl_src,
                                                ref stl_offset,
                                            ) = instr.format
                                            {
                                                if let InstrFormat::RRI(
                                                    sth_dest,
                                                    sth_src,
                                                    ref sth_offset,
                                                ) = next.format
                                                {
                                                    if stl_dest == sth_dest
                                                        && stl_src == sth_src
                                                        && stl_dest == Register::FP
                                                        && stl_offset.get_linked().unwrap()
                                                            == *orig_offset
                                                        && sth_offset.get_linked().unwrap()
                                                            == *orig_offset + 1
                                                    {
                                                        block.sequence.remove(i + 1);
                                                        block.sequence.remove(i);
                                                        block.sequence.insert(
                                                            i,
                                                            Instruction {
                                                                op: Opcode::Mov,
                                                                format: InstrFormat::RR(
                                                                    reg, stl_src,
                                                                ),
                                                            },
                                                        );
                                                        changed = true;
                                                        continue;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    i += 1;
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }
}
