use anyhow::Result;
use cls16::{Immediate, InstrFormat, Instruction, Opcode, Register};

use crate::clscc::{
    cg::{Codegen, Value},
    parser::Type,
};

impl Codegen {
    pub(crate) fn cga_call(&mut self, name: Value, args: Vec<Value>) -> Result<Option<Value>> {
        let name = name.get_immediate()?;
        let name = if let Immediate::Unlinked(name) = name {
            name
        } else {
            todo!("call linked function")
        };
        let block_name = self.current_scope.label.clone();
        let ret_addr_label = format!(
            "{}{}retaddr{}",
            block_name,
            name,
            self.current_scope.num_children()
        );
        self.current_scope.extern_label(&ret_addr_label);
        let ret_addr_reg = self.current_scope.any_register(Some(Type::Int))?;
        self.current_scope.push_instr(Instruction {
            op: Opcode::Mov,
            format: InstrFormat::RI(
                ret_addr_reg.get_register()?,
                Immediate::Unlinked(ret_addr_label.clone()),
            ),
        });
        self.cga_push_stack(ret_addr_reg)?;
        for arg in args {
            self.cga_push_stack(arg)?;
        }
        self.current_scope.push_instr(Instruction {
            op: Opcode::Jmp,
            format: InstrFormat::RI(Register::PC, Immediate::Unlinked(name)),
        });
        self.current_scope.push_label(ret_addr_label);

        let ret_val = self.current_scope.any_register(None)?; // todo: return types
        self.current_scope.push_instr(Instruction {
            op: Opcode::Mov,
            format: InstrFormat::RR(ret_val.get_register()?, Register::R1),
        });
        Ok(Some(ret_val))
    }
}
