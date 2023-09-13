use anyhow::Result;
use cls16::{Immediate, InstrFormat, Instruction, Opcode, Register};

use crate::clscc::{
    cg::{Codegen, Value},
    parser::{AstNode, Type},
};

impl Codegen {
    pub(crate) fn cga_if_else(
        &mut self,
        cond: Value,
        then: &AstNode<'_>,
        els: &Option<AstNode<'_>>,
        parent: Option<&AstNode<'_>>,
    ) -> Result<()> {
        let cond_reg = self.current_scope.any_register(Some(Type::Int))?;
        self.cga_store(cond_reg.clone(), cond)?;
        let then_label = "then".to_string();
        let els_label = "els".to_string();
        let end_label = "end".to_string();

        // compare the boolean condition to 0
        self.current_scope.push_instr(Instruction {
            op: Opcode::Sub,
            format: InstrFormat::RRR(Register::R0, cond_reg.get_register()?, Register::R0),
        });

        // if it's 0 (false) jump to the else block
        self.current_scope.push_instr(Instruction {
            op: Opcode::Jz,
            format: InstrFormat::I(Immediate::Unlinked(els_label.clone())),
        });

        // otherwise, jump to the then block
        self.current_scope.push_instr(Instruction {
            op: Opcode::Jmp,
            format: InstrFormat::I(Immediate::Unlinked(then_label.clone())),
        });

        // then block
        self.push_scope(then_label);
        self.dfs_walk(then, parent.to_owned(), then.rvalue)?;
        self.current_scope.push_instr(Instruction {
            op: Opcode::Jmp,
            format: InstrFormat::I(Immediate::Unlinked(end_label.clone())),
        });
        self.pop_scope();

        // else block
        if let Some(els) = els {
            self.push_scope(els_label);
            self.dfs_walk(els, parent, els.rvalue)?;
            self.current_scope.push_instr(Instruction {
                op: Opcode::Jmp,
                format: InstrFormat::I(Immediate::Unlinked(end_label.clone())),
            });
            self.pop_scope();
        } else {
            self.current_scope.push_label(els_label);
            self.current_scope.push_instr(Instruction {
                op: Opcode::Jmp,
                format: InstrFormat::I(Immediate::Unlinked(end_label.clone())),
            });
        }

        self.current_scope.push_label(end_label);
        self.current_scope.retake(cond_reg);

        Ok(())
    }

    pub(crate) fn cga_while(
        &mut self,
        cond: &AstNode<'_>,
        body: &AstNode<'_>,
        parent: Option<&AstNode<'_>>,
    ) -> Result<()> {
        let cond_reg = self.current_scope.any_register(Some(Type::Int))?;
        let cond_label = format!(
            "{}cond{}",
            self.current_scope.label,
            self.current_scope.num_children()
        );
        let body_label = format!(
            "{}body{}",
            self.current_scope.label,
            self.current_scope.num_children()
        );
        let end_label = format!(
            "{}end{}",
            self.current_scope.label,
            self.current_scope.num_children()
        );

        // condition
        self.current_scope.push_label(cond_label.clone());
        let cond = self.dfs_walk(cond, parent, true)?.unwrap();
        self.cga_store(cond_reg.clone(), cond)?;

        // compare the boolean condition to 0
        self.current_scope.push_instr(Instruction {
            op: Opcode::Sub,
            format: InstrFormat::RRR(Register::R0, cond_reg.get_register()?, Register::R0),
        });

        // if it's 0 (false) jump to the end
        self.current_scope.push_instr(Instruction {
            op: Opcode::Jz,
            format: InstrFormat::I(Immediate::Unlinked(end_label.clone())),
        });

        // otherwise, jump to the body
        self.current_scope.push_instr(Instruction {
            op: Opcode::Jmp,
            format: InstrFormat::I(Immediate::Unlinked(body_label.clone())),
        });

        // body
        self.current_scope.push_label(body_label);
        self.dfs_walk(body, parent, false)?;
        self.current_scope.push_instr(Instruction {
            op: Opcode::Jmp,
            format: InstrFormat::I(Immediate::Unlinked(cond_label.clone())),
        });

        self.current_scope.push_label(end_label);
        self.current_scope.retake(cond_reg);

        Ok(())
    }
}
