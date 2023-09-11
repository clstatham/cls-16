use crate::c::compiler::*;

impl CompilerState {
    pub(crate) fn optimize_blocks(&mut self) -> Result<()> {
        // block-level optimizations
        for (_, func) in self.functions.iter_mut() {
            let mut changed = true;
            while changed {
                changed = false;
                for block in func.code.iter_mut() {
                    let mut i = 0;
                    while i < block.sequence.len() {
                        let instr = &block.sequence[i];
                        // redundant mov
                        // transform the pattern:
                        // mov r1, r2
                        // mov r2, r1
                        // into:
                        // mov r1, r2
                        if let Some(next) = block.sequence.get(i + 1) {
                            if instr.op == Opcode::Mov && next.op == Opcode::Mov {
                                if let InstrFormat::RR(dest, src) = instr.format {
                                    if let InstrFormat::RR(next_dest, next_src) = next.format {
                                        if dest == next_src && src == next_dest {
                                            block.sequence.remove(i + 1);
                                            changed = true;
                                            continue;
                                        }
                                    }
                                }
                            }
                        }
                        // redundant load
                        // transform the pattern:
                        // stl r1, r2, X
                        // sth r1, r2, Y
                        // ldl r2, r1, X
                        // ldh r2, r1, Y
                        // into:
                        // stl r1, r2, X
                        // sth r1, r2, Y
                        if let Some(sth) = block.sequence.get(i + 1) {
                            if let Some(ldl) = block.sequence.get(i + 2) {
                                if let Some(ldh) = block.sequence.get(i + 3) {
                                    if instr.op == Opcode::Stl
                                        && sth.op == Opcode::Sth
                                        && ldl.op == Opcode::Ldl
                                        && ldh.op == Opcode::Ldh
                                    {
                                        if let InstrFormat::RRI(stl_dest, stl_src, ref stl_offset) =
                                            instr.format
                                        {
                                            if let InstrFormat::RRI(
                                                sth_dest,
                                                sth_src,
                                                ref sth_offset,
                                            ) = sth.format
                                            {
                                                if let InstrFormat::RRI(
                                                    ldl_dest,
                                                    ldl_src,
                                                    ref ldl_offset,
                                                ) = ldl.format
                                                {
                                                    if let InstrFormat::RRI(
                                                        ldh_dest,
                                                        ldh_src,
                                                        ref ldh_offset,
                                                    ) = ldh.format
                                                    {
                                                        if stl_dest == sth_dest
                                                            && stl_dest == ldl_src
                                                            && stl_dest == ldh_src
                                                            && stl_src == sth_src
                                                            && stl_src == ldl_dest
                                                            && stl_src == ldh_dest
                                                            && stl_offset == ldl_offset
                                                            && sth_offset == ldh_offset
                                                        {
                                                            block.sequence.remove(i + 3);
                                                            block.sequence.remove(i + 2);
                                                            changed = true;
                                                            continue;
                                                        }
                                                    }
                                                }
                                            }
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
        Ok(())
    }
}
