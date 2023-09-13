use anyhow::Result;
use cls16::{Immediate, InstrFormat, Instruction, Opcode, Register};

use crate::clscc::{
    cg::{Codegen, Value, ValueStorage},
    common::Punctuator,
    parser::Type,
};

impl Codegen {
    pub(crate) fn cga_eq(&mut self, result: Value, lhs: Value, rhs: Value) -> Result<Value> {
        self.cga_compare(result, lhs, rhs, Punctuator::EqEq)
    }

    pub(crate) fn cga_neq(&mut self, result: Value, lhs: Value, rhs: Value) -> Result<Value> {
        self.cga_compare(result, lhs, rhs, Punctuator::BangEq)
    }

    pub(crate) fn cga_lt(&mut self, result: Value, lhs: Value, rhs: Value) -> Result<Value> {
        self.cga_compare(result, lhs, rhs, Punctuator::Lt)
    }

    pub(crate) fn cga_gt(&mut self, result: Value, lhs: Value, rhs: Value) -> Result<Value> {
        self.cga_compare(result, lhs, rhs, Punctuator::Gt)
    }

    pub(crate) fn cga_lte(&mut self, result: Value, lhs: Value, rhs: Value) -> Result<Value> {
        self.cga_compare(result, lhs, rhs, Punctuator::LtEq)
    }

    pub(crate) fn cga_gte(&mut self, result: Value, lhs: Value, rhs: Value) -> Result<Value> {
        self.cga_compare(result, lhs, rhs, Punctuator::GtEq)
    }

    pub(crate) fn cga_compare(
        &mut self,
        result: Value,
        lhs: Value,
        rhs: Value,
        op: Punctuator,
    ) -> Result<Value> {
        let tmp_lhs = self.current_scope.any_register(lhs.ty().cloned())?;
        let tmp_rhs = self.current_scope.any_register(rhs.ty().cloned())?;
        self.cga_store(tmp_lhs.clone(), lhs)?;
        self.cga_store(tmp_rhs.clone(), rhs)?;

        self.current_scope.push_instr(Instruction {
            op: Opcode::Sub,
            format: InstrFormat::RRR(
                Register::R0,
                tmp_lhs.get_register()?,
                tmp_rhs.get_register()?,
            ),
        });

        self.current_scope.retake(tmp_lhs);
        self.current_scope.retake(tmp_rhs);

        let true_label = format!(
            "{}true{}",
            self.current_scope.label,
            self.current_scope.num_children()
        );
        let false_label = format!(
            "{}false{}",
            self.current_scope.label,
            self.current_scope.num_children()
        );
        let end_label = format!(
            "{}end{}",
            self.current_scope.label,
            self.current_scope.num_children()
        );
        // carry flag will be set if and only if lhs < rhs
        // zero flag will be set if and only if lhs == rhs
        // therefore:
        //   if carry flag is set, lhs < rhs
        //   if zero flag is set, lhs == rhs
        //   if neither flag is set, lhs > rhs
        let (jump_zero, jump_carry, jump_neither) = match op {
            Punctuator::EqEq => (true_label.clone(), false_label.clone(), false_label.clone()),
            Punctuator::BangEq => (false_label.clone(), true_label.clone(), true_label.clone()),
            Punctuator::Lt => (false_label.clone(), true_label.clone(), false_label.clone()),
            Punctuator::Gt => (false_label.clone(), false_label.clone(), true_label.clone()),
            Punctuator::LtEq => (true_label.clone(), true_label.clone(), false_label.clone()),
            Punctuator::GtEq => (true_label.clone(), false_label.clone(), true_label.clone()),
            _ => unreachable!(),
        };

        self.current_scope.push_instr(Instruction {
            op: Opcode::Jz,
            format: InstrFormat::I(Immediate::Unlinked(jump_zero.clone())),
        });
        self.current_scope.push_instr(Instruction {
            op: Opcode::Jc,
            format: InstrFormat::I(Immediate::Unlinked(jump_carry.clone())),
        });
        self.current_scope.push_instr(Instruction {
            op: Opcode::Jmp,
            format: InstrFormat::I(Immediate::Unlinked(jump_neither.clone())),
        });

        self.current_scope.push_label(true_label);
        self.current_scope.push_instr(Instruction {
            op: Opcode::Mov,
            format: InstrFormat::RI(result.get_register()?, Immediate::Linked(1)),
        });
        self.current_scope.push_instr(Instruction {
            op: Opcode::Jmp,
            format: InstrFormat::I(Immediate::Unlinked(end_label.clone())),
        });
        self.current_scope.push_label(false_label);
        self.current_scope.push_instr(Instruction {
            op: Opcode::Mov,
            format: InstrFormat::RI(result.get_register()?, Immediate::Linked(0)),
        });
        self.current_scope.push_label(end_label);
        Ok(result)
    }
}
