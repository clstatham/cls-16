use anyhow::Result;
use cls16::{Immediate, InstrFormat, Instruction, Opcode, Register};

use crate::clscc::cg::{Codegen, CodegenError, CodegenErrorKind, Value, ValueStorage};

pub mod arith;
pub mod call;
pub mod mem;

impl Codegen {
    pub(crate) fn cga_start_prelude(&mut self) -> Result<()> {
        self.current_scope.push_instr(Instruction {
            op: Opcode::Mov,
            format: InstrFormat::RI(Register::FP, Immediate::Linked(0xf000)),
        });
        self.current_scope.push_instr(Instruction {
            op: Opcode::Mov,
            format: InstrFormat::RR(Register::SP, Register::FP),
        });
        Ok(())
    }

    pub(crate) fn cga_halt(&mut self) -> Result<()> {
        self.current_scope.push_instr(Instruction {
            op: Opcode::Halt,
            format: InstrFormat::OpOnly,
        });
        Ok(())
    }

    pub(crate) fn cga_printi(&mut self, val: Value) -> Result<Option<Value>> {
        match val.storage() {
            ValueStorage::Immediate(imm) => {
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Printi,
                    format: InstrFormat::I(imm.to_owned()),
                });
            }
            ValueStorage::Register(reg) => {
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Printi,
                    format: InstrFormat::R(*reg),
                });
            }
            ValueStorage::Stack(offset) => {
                let tmp = self.current_scope.any_register()?;
                let tmp_reg = tmp.get_register()?;
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldl,
                    format: InstrFormat::RRI(
                        tmp_reg,
                        Register::FP,
                        Immediate::Linked(*offset as u16),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldh,
                    format: InstrFormat::RRI(
                        tmp_reg,
                        Register::FP,
                        Immediate::Linked(*offset as u16 + 1),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Printi,
                    format: InstrFormat::R(tmp_reg),
                });
                self.current_scope.retake(tmp);
            }
        }
        Ok(Some(val))
    }
}
