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
        let new_value = self
            .current_scope
            .push_stack(None, value_ty.clone(), value.rvalue())?;
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

    pub(crate) fn cga_pop_stack(&mut self, ty: Option<Type>) -> Result<Value> {
        let value = self.current_scope.any_register(ty)?;
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

    pub(crate) fn cga_index(&mut self, arr: Value, index: Value, rvalue: bool) -> Result<Value> {
        let elem_ty = match arr.ty().unwrap() {
            Type::Array(ty, _) => ty.to_owned(),
            Type::Pointer(ty) => ty.to_owned().unwrap(),
            _ => unreachable!(),
        };

        let arr_offset = arr.get_stack_offset()?;
        let index_val = self
            .current_scope
            .any_register(Some(Type::Pointer(Some(elem_ty.clone()))))?;
        self.current_scope.push_instr(Instruction {
            op: Opcode::Mov,
            format: InstrFormat::RI(
                index_val.get_register()?,
                Immediate::Linked(elem_ty.sizeof() as u16),
            ),
        });
        match index.storage() {
            ValueStorage::Register(index) => {
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Mul,
                    format: InstrFormat::RRR(
                        index_val.get_register()?,
                        *index,
                        index_val.get_register()?,
                    ),
                });
            }
            ValueStorage::Stack(_) => todo!(),
            ValueStorage::Immediate(index) => {
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Mul,
                    format: InstrFormat::RRI(
                        index_val.get_register()?,
                        index_val.get_register()?,
                        index.to_owned(),
                    ),
                });
            }
        }

        self.current_scope.push_instr(Instruction {
            op: Opcode::Add,
            format: InstrFormat::RRI(
                index_val.get_register()?,
                index_val.get_register()?,
                Immediate::Linked(arr_offset as u16),
            ),
        });
        self.current_scope.push_instr(Instruction {
            op: Opcode::Add,
            format: InstrFormat::RRR(
                index_val.get_register()?,
                Register::FP,
                index_val.get_register()?,
            ),
        });
        if rvalue {
            let result = self.current_scope.any_register(Some(*elem_ty.to_owned()))?;
            self.current_scope.push_instr(Instruction {
                op: Opcode::Ldl,
                format: InstrFormat::RRI(
                    result.get_register()?,
                    index_val.get_register()?,
                    Immediate::Linked(0),
                ),
            });
            self.current_scope.push_instr(Instruction {
                op: Opcode::Ldh,
                format: InstrFormat::RRI(
                    result.get_register()?,
                    index_val.get_register()?,
                    Immediate::Linked(1),
                ),
            });

            self.current_scope.retake(index_val);

            Ok(result)
        } else {
            Ok(index_val)
        }
    }

    pub(crate) fn cga_addrof(&mut self, result: Value, rhs: Value) -> Result<Value> {
        let rhs_offset = rhs.get_stack_offset()?;
        self.current_scope.push_instr(Instruction {
            op: Opcode::Add,
            format: InstrFormat::RRI(
                result.get_register()?,
                Register::FP,
                Immediate::Linked(rhs_offset as u16),
            ),
        });
        Ok(result)
    }

    pub(crate) fn cga_load(&mut self, result: Value, pointer: Value) -> Result<Value> {
        self.current_scope.push_instr(Instruction {
            op: Opcode::Ldl,
            format: InstrFormat::RRI(
                result.get_register()?,
                pointer.get_register()?,
                Immediate::Linked(0),
            ),
        });
        self.current_scope.push_instr(Instruction {
            op: Opcode::Ldh,
            format: InstrFormat::RRI(
                result.get_register()?,
                pointer.get_register()?,
                Immediate::Linked(1),
            ),
        });
        Ok(result)
    }

    fn cga_store_ptr(&mut self, ptr: Value, rhs: Value) -> Result<Value> {
        let tmp_reg = self.current_scope.any_register(ptr.ty().cloned())?;
        let tmp_reg = self.cga_store(tmp_reg, ptr.clone())?;
        let ptr_reg = tmp_reg.get_register()?;
        let res = match rhs.storage() {
            ValueStorage::Register(rhs_reg) => {
                log::trace!("store ptr: register");
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Stl,
                    format: InstrFormat::RRI(ptr_reg, *rhs_reg, Immediate::Linked(0)),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Sth,
                    format: InstrFormat::RRI(ptr_reg, *rhs_reg, Immediate::Linked(1)),
                });
                Ok(ptr)
            }
            ValueStorage::Stack(rhs_offset) => {
                log::trace!("store ptr: stack");
                let tmp_reg = self.current_scope.any_register(rhs.ty().cloned())?;
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldl,
                    format: InstrFormat::RRI(
                        tmp_reg.get_register()?,
                        Register::FP,
                        Immediate::Linked(*rhs_offset as u16),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Ldh,
                    format: InstrFormat::RRI(
                        tmp_reg.get_register()?,
                        Register::FP,
                        Immediate::Linked(*rhs_offset as u16 + 1),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Stl,
                    format: InstrFormat::RRI(
                        ptr_reg,
                        tmp_reg.get_register()?,
                        Immediate::Linked(0),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Sth,
                    format: InstrFormat::RRI(
                        ptr_reg,
                        tmp_reg.get_register()?,
                        Immediate::Linked(1),
                    ),
                });
                self.current_scope.retake(tmp_reg);
                Ok(ptr)
            }
            ValueStorage::Immediate(imm) => {
                log::trace!("store ptr: immediate");
                let tmp_reg = self.current_scope.any_register(rhs.ty().cloned())?;
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Mov,
                    format: InstrFormat::RI(tmp_reg.get_register()?, imm.to_owned()),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Stl,
                    format: InstrFormat::RRI(
                        ptr_reg,
                        tmp_reg.get_register()?,
                        Immediate::Linked(0),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Sth,
                    format: InstrFormat::RRI(
                        ptr_reg,
                        tmp_reg.get_register()?,
                        Immediate::Linked(1),
                    ),
                });
                self.current_scope.retake(tmp_reg);

                Ok(ptr)
            }
        };
        self.current_scope.retake(tmp_reg);
        res
    }

    pub(crate) fn cga_store(&mut self, lhs: Value, rhs: Value) -> Result<Value> {
        assert!(!lhs.rvalue());
        match (lhs.ty(), rhs.ty()) {
            (Some(Type::Pointer(_)), Some(Type::Pointer(_))) => {
                log::trace!("store: ptr -> ptr");
            }
            (Some(Type::Pointer(_)), t) => {
                log::trace!("store: {t:?} -> ptr");
                return self.cga_store_ptr(lhs, rhs);
            }
            _ => {
                log::trace!("store: _ -> _");
            }
        }
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
                let tmp_src = self.current_scope.any_register(rhs.ty().cloned())?;
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
                let tmp_src = self.current_scope.any_register(rhs.ty().cloned())?;
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
