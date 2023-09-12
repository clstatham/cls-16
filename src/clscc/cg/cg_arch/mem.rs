use anyhow::Result;
use cls16::{Immediate, InstrFormat, Instruction, Opcode, Register};

use crate::clscc::{
    cg::{Codegen, CodegenError, CodegenErrorKind, Value, ValueStorage},
    parser::Type,
};

impl Codegen {
    pub(crate) fn cga_push_stack(&mut self, value: Value) -> Result<Value> {
        let value_reg = value.get_register()?;
        let value_ty = value.ty().unwrap().clone();
        let new_value = self.current_scope.push_stack(None, value_ty.clone())?;
        self.current_scope.push_instr(Instruction {
            op: Opcode::Sub,
            format: InstrFormat::RRI(Register::SP, Register::SP, Immediate::Linked(2)),
        });
        self.current_scope.push_instr(Instruction {
            op: Opcode::Stl,
            format: InstrFormat::RRI(Register::SP, value_reg, Immediate::Linked(0)),
        });
        self.current_scope.push_instr(Instruction {
            op: Opcode::Sth,
            format: InstrFormat::RRI(Register::SP, value_reg, Immediate::Linked(1)),
        });
        self.current_scope.retake(value);
        Ok(new_value)
    }

    pub(crate) fn cga_pop_stack(&mut self) -> Result<Value> {
        let value = self.current_scope.any_register()?;
        self.current_scope.push_instr(Instruction {
            op: Opcode::Ldl,
            format: InstrFormat::RRI(value.get_register()?, Register::SP, Immediate::Linked(0)),
        });
        self.current_scope.push_instr(Instruction {
            op: Opcode::Ldh,
            format: InstrFormat::RRI(value.get_register()?, Register::SP, Immediate::Linked(1)),
        });
        self.current_scope.push_instr(Instruction {
            op: Opcode::Add,
            format: InstrFormat::RRI(Register::SP, Register::SP, Immediate::Linked(2)),
        });
        Ok(value)
    }

    pub(crate) fn cga_store(&mut self, lhs: Value, rhs: Value) -> Result<Value> {
        match (lhs.storage(), rhs.storage()) {
            (ValueStorage::Register(dest), ValueStorage::Register(src)) => {
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Mov,
                    format: InstrFormat::RR(*dest, *src),
                });
                Ok(lhs)
            }
            (ValueStorage::Register(dest), ValueStorage::Stack(src_offset)) => {
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldl,
                    format: InstrFormat::RRI(
                        *dest,
                        Register::FP,
                        Immediate::Linked(*src_offset as u16),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldh,
                    format: InstrFormat::RRI(
                        *dest,
                        Register::FP,
                        Immediate::Linked(*src_offset as u16 + 1),
                    ),
                });
                Ok(lhs)
            }
            (ValueStorage::Register(dest), ValueStorage::Immediate(src)) => {
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Mov,
                    format: InstrFormat::RI(*dest, src.to_owned()),
                });
                Ok(lhs)
            }
            (ValueStorage::Stack(dest_offset), ValueStorage::Register(src)) => {
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Stl,
                    format: InstrFormat::RRI(
                        Register::FP,
                        *src,
                        Immediate::Linked(*dest_offset as u16),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Sth,
                    format: InstrFormat::RRI(
                        Register::FP,
                        *src,
                        Immediate::Linked(*dest_offset as u16 + 1),
                    ),
                });
                Ok(lhs)
            }
            (ValueStorage::Stack(dest_offset), ValueStorage::Stack(src_offset)) => {
                let tmp_src = self.current_scope.any_register()?;
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldl,
                    format: InstrFormat::RRI(
                        tmp_src.get_register()?,
                        Register::FP,
                        Immediate::Linked(*src_offset as u16),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldh,
                    format: InstrFormat::RRI(
                        tmp_src.get_register()?,
                        Register::FP,
                        Immediate::Linked(*src_offset as u16 + 1),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Stl,
                    format: InstrFormat::RRI(
                        Register::FP,
                        tmp_src.get_register()?,
                        Immediate::Linked(*dest_offset as u16),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Sth,
                    format: InstrFormat::RRI(
                        Register::FP,
                        tmp_src.get_register()?,
                        Immediate::Linked(*dest_offset as u16 + 1),
                    ),
                });
                self.current_scope.retake(tmp_src);
                Ok(lhs)
            }
            (ValueStorage::Stack(dest_offset), ValueStorage::Immediate(src)) => {
                let tmp_src = self.current_scope.any_register()?;
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Mov,
                    format: InstrFormat::RI(tmp_src.get_register()?, src.to_owned()),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Stl,
                    format: InstrFormat::RRI(
                        Register::FP,
                        tmp_src.get_register()?,
                        Immediate::Linked(*dest_offset as u16),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Sth,
                    format: InstrFormat::RRI(
                        Register::FP,
                        tmp_src.get_register()?,
                        Immediate::Linked(*dest_offset as u16 + 1),
                    ),
                });
                self.current_scope.retake(tmp_src);
                Ok(lhs)
            }
            (ValueStorage::Immediate(_), _) => {
                Err(CodegenError::new(CodegenErrorKind::CannotStoreToImmediate).into())
            }
        }
    }
}
