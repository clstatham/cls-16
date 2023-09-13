use anyhow::Result;
use cls16::{Immediate, InstrFormat, Instruction, Opcode, Register};

use crate::clscc::cg::{Codegen, Value, ValueStorage};

impl Codegen {
    pub(crate) fn cga_add(&mut self, result: Value, lhs: Value, rhs: Value) -> Result<Value> {
        self.cga_arith(result, lhs, rhs, Opcode::Add)
    }

    pub(crate) fn cga_sub(&mut self, result: Value, lhs: Value, rhs: Value) -> Result<Value> {
        self.cga_arith(result, lhs, rhs, Opcode::Sub)
    }

    pub(crate) fn cga_mul(&mut self, result: Value, lhs: Value, rhs: Value) -> Result<Value> {
        self.cga_arith(result, lhs, rhs, Opcode::Mul)
    }

    pub(crate) fn cga_div(&mut self, result: Value, lhs: Value, rhs: Value) -> Result<Value> {
        self.cga_arith(result, lhs, rhs, Opcode::Div)
    }

    fn cga_arith(&mut self, result: Value, lhs: Value, rhs: Value, op: Opcode) -> Result<Value> {
        match (lhs.storage(), rhs.storage()) {
            (ValueStorage::Register(lhs), ValueStorage::Register(rhs)) => {
                self.current_scope.push_instr(Instruction {
                    op,
                    format: InstrFormat::RRR(result.get_register()?, *lhs, *rhs),
                });
                Ok(result)
            }
            (ValueStorage::Register(lhs), ValueStorage::Stack(rhs_offset)) => {
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldl,
                    format: InstrFormat::RRI(
                        result.get_register()?,
                        Register::FP,
                        Immediate::Linked(*rhs_offset as u16),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldh,
                    format: InstrFormat::RRI(
                        result.get_register()?,
                        Register::FP,
                        Immediate::Linked(*rhs_offset as u16 + 1),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op,
                    format: InstrFormat::RRR(result.get_register()?, *lhs, result.get_register()?),
                });
                Ok(result)
            }
            (ValueStorage::Register(lhs), ValueStorage::Immediate(rhs)) => {
                self.current_scope.push_instr(Instruction {
                    op,
                    format: InstrFormat::RRI(result.get_register()?, *lhs, rhs.to_owned()),
                });
                Ok(result)
            }
            (ValueStorage::Stack(lhs_offset), ValueStorage::Register(rhs)) => {
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldl,
                    format: InstrFormat::RRI(
                        result.get_register()?,
                        Register::FP,
                        Immediate::Linked(*lhs_offset as u16),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldh,
                    format: InstrFormat::RRI(
                        result.get_register()?,
                        Register::FP,
                        Immediate::Linked(*lhs_offset as u16 + 1),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op,
                    format: InstrFormat::RRR(result.get_register()?, result.get_register()?, *rhs),
                });
                Ok(result)
            }
            (ValueStorage::Stack(lhs_offset), ValueStorage::Stack(rhs_offset)) => {
                let tmp_rhs = self.current_scope.any_register(lhs.ty().cloned())?;
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldl,
                    format: InstrFormat::RRI(
                        tmp_rhs.get_register()?,
                        Register::FP,
                        Immediate::Linked(*rhs_offset as u16),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldh,
                    format: InstrFormat::RRI(
                        tmp_rhs.get_register()?,
                        Register::FP,
                        Immediate::Linked(*rhs_offset as u16 + 1),
                    ),
                });

                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldl,
                    format: InstrFormat::RRI(
                        result.get_register()?,
                        Register::FP,
                        Immediate::Linked(*lhs_offset as u16),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldh,
                    format: InstrFormat::RRI(
                        result.get_register()?,
                        Register::FP,
                        Immediate::Linked(*lhs_offset as u16 + 1),
                    ),
                });

                self.current_scope.push_instr(Instruction {
                    op,
                    format: InstrFormat::RRR(
                        result.get_register()?,
                        result.get_register()?,
                        tmp_rhs.get_register()?,
                    ),
                });

                self.current_scope.retake(tmp_rhs);

                Ok(result)
            }
            (ValueStorage::Stack(lhs_offset), ValueStorage::Immediate(rhs)) => {
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldl,
                    format: InstrFormat::RRI(
                        result.get_register()?,
                        Register::FP,
                        Immediate::Linked(*lhs_offset as u16),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldh,
                    format: InstrFormat::RRI(
                        result.get_register()?,
                        Register::FP,
                        Immediate::Linked(*lhs_offset as u16 + 1),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op,
                    format: InstrFormat::RRI(
                        result.get_register()?,
                        result.get_register()?,
                        rhs.to_owned(),
                    ),
                });
                Ok(result)
            }
            (ValueStorage::Immediate(lhs), ValueStorage::Register(rhs)) => {
                self.current_scope.push_instr(Instruction {
                    op,
                    format: InstrFormat::RRI(result.get_register()?, *rhs, lhs.to_owned()),
                });
                Ok(result)
            }
            (ValueStorage::Immediate(lhs), ValueStorage::Stack(rhs_offset)) => {
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldl,
                    format: InstrFormat::RRI(
                        result.get_register()?,
                        Register::FP,
                        Immediate::Linked(*rhs_offset as u16),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldh,
                    format: InstrFormat::RRI(
                        result.get_register()?,
                        Register::FP,
                        Immediate::Linked(*rhs_offset as u16 + 1),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op,
                    format: InstrFormat::RRI(
                        result.get_register()?,
                        result.get_register()?,
                        lhs.to_owned(),
                    ),
                });
                Ok(result)
            }
            (ValueStorage::Immediate(lhs), ValueStorage::Immediate(rhs)) => {
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Mov,
                    format: InstrFormat::RI(result.get_register()?, lhs.to_owned()),
                });
                self.current_scope.push_instr(Instruction {
                    op,
                    format: InstrFormat::RRI(
                        result.get_register()?,
                        result.get_register()?,
                        rhs.to_owned(),
                    ),
                });
                Ok(result)
            }
        }
    }
}
