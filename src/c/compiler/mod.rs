use anyhow::{Context, Error, Result};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    asm::Span,
    plat::{Immediate, InstrFormat, Instruction, Opcode, Register},
};

use self::var::{Var, VarStorage};

use super::{
    lexer::{gen::Keyword, lex_tokens},
    parser::ast::*,
    CToken, CompError, Tokens,
};

pub mod var;

pub struct Block {
    pub label: String,
    pub sequence: Vec<Instruction>,
}

impl Block {
    pub fn new(label: &str) -> Self {
        Self {
            label: label.to_owned(),
            sequence: vec![],
        }
    }
}

pub struct FunctionContext {
    pub return_type: TypeSpecifier,
    pub name: String,
    pub args: Vec<Var>,
    allocations: FxHashMap<String, Var>,
    avail_regs: FxHashSet<Register>,
    pub stack_offset: u16,
    pub max_stack_offset: u16,
    pub code: Vec<Block>,
    pub epologue: Block,
}

impl FunctionContext {
    pub fn last_block_mut(&mut self) -> &mut Block {
        self.code.last_mut().unwrap()
    }

    pub fn insert(&mut self, ssa: Var) {
        self.allocations.insert(ssa.name().to_owned(), ssa);
    }

    pub fn get(&self, name: &str) -> Option<Var> {
        self.allocations.get(name).cloned()
    }

    pub fn push(&mut self, name: &str, typ: TypeSpecifier) -> Var {
        self.stack_offset += 2;
        self.max_stack_offset += 2;
        let ssa = Var::new(typ, name, var::VarStorage::StackOffset(self.stack_offset));
        self.allocations.insert(name.to_owned(), ssa.clone());
        ssa
    }

    pub fn get_or_push(&mut self, name: &str, typ: TypeSpecifier) -> Var {
        if let Some(ssa) = self.allocations.get(name) {
            ssa.clone()
        } else {
            self.push(name, typ)
        }
    }

    pub fn take_back_reg(&mut self, reg: Register) {
        self.avail_regs.insert(reg);
    }

    pub fn take_back(&mut self, var: Var) {
        if self.allocations.remove(var.name()).is_some() {
            match var.storage() {
                VarStorage::Register(reg) => {
                    self.avail_regs.insert(*reg);
                }
                VarStorage::StackOffset(offset) => {
                    if *offset == self.stack_offset {
                        // pop the stack
                        self.stack_offset -= 2;
                    }
                }
                VarStorage::Immediate(_) => {}
            }
        }
    }

    pub fn any_reg(&mut self) -> Option<Register> {
        [
            Register::R1,
            Register::R2,
            Register::R3,
            Register::R4,
            Register::R5,
            Register::R6,
        ]
        .into_iter()
        .find(|&reg| self.avail_regs.remove(&reg))
    }
}

#[inline]
#[doc(hidden)]
fn store_reg_to_reg_offset(addr_offset: u16, src: Register, block: &mut Block) {
    block.sequence.push(Instruction {
        op: Opcode::Stl,
        format: InstrFormat::RRI(Register::FP, src, Immediate::Linked(addr_offset)),
    });
    block.sequence.push(Instruction {
        op: Opcode::Sth,
        format: InstrFormat::RRI(Register::FP, src, Immediate::Linked(addr_offset + 1)),
    });
}

#[inline]
#[doc(hidden)]
fn load_reg_offset_to_reg(dest: Register, addr_offset: u16, block: &mut Block) {
    block.sequence.push(Instruction {
        op: Opcode::Ldl,
        format: InstrFormat::RRI(dest, Register::FP, Immediate::Linked(addr_offset)),
    });
    block.sequence.push(Instruction {
        op: Opcode::Ldh,
        format: InstrFormat::RRI(dest, Register::FP, Immediate::Linked(addr_offset + 1)),
    });
}

#[derive(Default)]
pub struct CompilerState {
    pub functions: FxHashMap<String, FunctionContext>,
}

impl CompilerState {
    fn compile_local_decl(&mut self, decl: &Declaration<'_>, func_name: &str) -> Result<()> {
        let dest = self
            .functions
            .get_mut(func_name)
            .unwrap()
            .push(decl.id.item.id.item.0, decl.id.item.typ.item);
        self.compile_expr(&decl.expr.item, Some(&dest), func_name)?;
        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr<'_>, dest: Option<&Var>, func_name: &str) -> Result<()> {
        match expr {
            Expr::Ident(id) => {
                if let Some(dest) = dest {
                    let func = self.functions.get_mut(func_name).unwrap();
                    let src = func.get(id.item.0).unwrap();

                    match (dest.storage(), src.storage()) {
                        (VarStorage::StackOffset(dest_off), VarStorage::StackOffset(src_off)) => {
                            let tmp_reg = func.any_reg().unwrap();
                            {
                                let block = func.last_block_mut();
                                load_reg_offset_to_reg(tmp_reg, *src_off, block);
                                store_reg_to_reg_offset(*dest_off, tmp_reg, block);
                            }
                            func.take_back_reg(tmp_reg);
                        }
                        (VarStorage::Register(dest), VarStorage::StackOffset(src_off)) => {
                            let block = func.last_block_mut();
                            load_reg_offset_to_reg(*dest, *src_off, block);
                        }

                        st => todo!("{:?}", st),
                    }
                }
            }
            Expr::Constant(val) => {
                if let Some(dest) = dest {
                    let func = self.functions.get_mut(func_name).unwrap();

                    match dest.storage() {
                        var::VarStorage::Immediate(_) => {
                            return Err(Error::from(CompError::InvalidExpression(
                                val.span.location_line() as usize,
                                val.first_line(),
                            )))
                        }
                        var::VarStorage::Register(reg) => {
                            let Constant::Integer(val) = val.item;
                            func.last_block_mut().sequence.push(Instruction {
                                op: Opcode::Mov,
                                format: InstrFormat::RI(*reg, Immediate::Linked(val)),
                            })
                        }
                        var::VarStorage::StackOffset(addr_offset) => {
                            let Constant::Integer(val) = val.item;
                            let tmp_reg = func.any_reg().unwrap();
                            {
                                let block = func.last_block_mut();
                                block.sequence.push(Instruction {
                                    op: Opcode::Mov,
                                    format: InstrFormat::RI(tmp_reg, Immediate::Linked(val)),
                                });
                                store_reg_to_reg_offset(*addr_offset, tmp_reg, block);
                            }
                            func.take_back_reg(tmp_reg);
                        }
                    }
                } else {
                    return Err(Error::from(CompError::InvalidExpression(
                        val.span.location_line() as usize,
                        val.first_line(),
                    )));
                }
            }
            Expr::Infix(infix) => {
                let tmp_rhs_reg = self
                    .functions
                    .get_mut(func_name)
                    .unwrap()
                    .any_reg()
                    .unwrap();
                let tmp_lhs_reg = self
                    .functions
                    .get_mut(func_name)
                    .unwrap()
                    .any_reg()
                    .unwrap();

                if let Some(dest) = dest {
                    let lhs = Var::new(dest.typ(), "lhs", var::VarStorage::Register(tmp_lhs_reg));
                    self.compile_expr(&infix.item.lhs.item, Some(&lhs), func_name)?;
                    let rhs = Var::new(dest.typ(), "rhs", var::VarStorage::Register(tmp_rhs_reg));
                    self.compile_expr(&infix.item.rhs.item, Some(&rhs), func_name)?;
                    match &infix.item.op.item {
                        InfixOp::Assign => unreachable!(),
                        InfixOp::Add
                        | InfixOp::Div
                        | InfixOp::Mul
                        | InfixOp::Sub
                        | InfixOp::And
                        | InfixOp::Or
                        | InfixOp::Xor
                        | InfixOp::AndAnd
                        | InfixOp::OrOr => {
                            let op = match &infix.item.op.item {
                                InfixOp::Add => Opcode::Add,
                                InfixOp::Sub => Opcode::Sub,
                                InfixOp::Mul => Opcode::Mul,
                                InfixOp::Div => Opcode::Div,
                                InfixOp::And => Opcode::And,
                                InfixOp::Or => Opcode::Or,
                                InfixOp::Xor => Opcode::Xor,
                                InfixOp::AndAnd => Opcode::And,
                                InfixOp::OrOr => Opcode::Or,
                                _ => unreachable!(),
                            };

                            match dest.storage() {
                                VarStorage::Register(dest_reg) => {
                                    let block =
                                        self.functions.get_mut(func_name).unwrap().last_block_mut();
                                    block.sequence.push(Instruction {
                                        op,
                                        format: InstrFormat::RRR(
                                            *dest_reg,
                                            tmp_lhs_reg,
                                            tmp_rhs_reg,
                                        ),
                                    });
                                }
                                VarStorage::StackOffset(dest_offset) => {
                                    let tmp_dest = self
                                        .functions
                                        .get_mut(func_name)
                                        .unwrap()
                                        .any_reg()
                                        .unwrap();
                                    {
                                        let block = self
                                            .functions
                                            .get_mut(func_name)
                                            .unwrap()
                                            .last_block_mut();
                                        load_reg_offset_to_reg(tmp_dest, *dest_offset, block);
                                        block.sequence.push(Instruction {
                                            op,
                                            format: InstrFormat::RRR(
                                                tmp_dest,
                                                tmp_lhs_reg,
                                                tmp_rhs_reg,
                                            ),
                                        });
                                        store_reg_to_reg_offset(*dest_offset, tmp_dest, block);
                                    }
                                    self.functions
                                        .get_mut(func_name)
                                        .unwrap()
                                        .take_back_reg(tmp_dest);
                                }
                                VarStorage::Immediate(_) => {
                                    return Err(Error::from(CompError::InvalidExpression(
                                        infix.span.location_line() as usize,
                                        infix.first_line(),
                                    )))
                                }
                            }
                        }
                        InfixOp::EqEq
                        | InfixOp::NotEqual
                        | InfixOp::Lt
                        | InfixOp::LtEq
                        | InfixOp::Gt
                        | InfixOp::GtEq => {
                            let tmp_dest = self
                                .functions
                                .get_mut(func_name)
                                .unwrap()
                                .any_reg()
                                .unwrap();
                            let nblock = self.functions.get(func_name).unwrap().code.len();
                            let true_block = Block::new(&format!("{func_name}true{nblock}",));
                            let false_block = Block::new(&format!("{func_name}false{nblock}",));
                            let end_block = Block::new(&format!("{func_name}end{nblock}",));
                            {
                                let block =
                                    self.functions.get_mut(func_name).unwrap().last_block_mut();
                                block.sequence.push(Instruction {
                                    op: Opcode::Sub,
                                    format: InstrFormat::RRR(tmp_dest, tmp_lhs_reg, tmp_rhs_reg),
                                });
                                match infix.item.op.item {
                                    InfixOp::EqEq => {
                                        block.sequence.push(Instruction {
                                            op: Opcode::Jz,
                                            format: InstrFormat::I(Immediate::Unlinked(
                                                true_block.label.clone(),
                                            )),
                                        });
                                        block.sequence.push(Instruction {
                                            op: Opcode::Jmp,
                                            format: InstrFormat::I(Immediate::Unlinked(
                                                false_block.label.clone(),
                                            )),
                                        });
                                    }
                                    InfixOp::NotEqual => {
                                        block.sequence.push(Instruction {
                                            op: Opcode::Jz,
                                            format: InstrFormat::I(Immediate::Unlinked(
                                                false_block.label.clone(),
                                            )),
                                        });
                                        block.sequence.push(Instruction {
                                            op: Opcode::Jmp,
                                            format: InstrFormat::I(Immediate::Unlinked(
                                                true_block.label.clone(),
                                            )),
                                        });
                                    }
                                    InfixOp::Lt => {
                                        block.sequence.push(Instruction {
                                            op: Opcode::Jc,
                                            format: InstrFormat::I(Immediate::Unlinked(
                                                true_block.label.clone(),
                                            )),
                                        });
                                        block.sequence.push(Instruction {
                                            op: Opcode::Jmp,
                                            format: InstrFormat::I(Immediate::Unlinked(
                                                false_block.label.clone(),
                                            )),
                                        });
                                    }
                                    InfixOp::Gt => {
                                        block.sequence.push(Instruction {
                                            op: Opcode::Jc,
                                            format: InstrFormat::I(Immediate::Unlinked(
                                                false_block.label.clone(),
                                            )),
                                        });
                                        block.sequence.push(Instruction {
                                            op: Opcode::Jz,
                                            format: InstrFormat::I(Immediate::Unlinked(
                                                false_block.label.clone(),
                                            )),
                                        });
                                        block.sequence.push(Instruction {
                                            op: Opcode::Jmp,
                                            format: InstrFormat::I(Immediate::Unlinked(
                                                true_block.label.clone(),
                                            )),
                                        });
                                    }
                                    InfixOp::LtEq => {
                                        block.sequence.push(Instruction {
                                            op: Opcode::Jc,
                                            format: InstrFormat::I(Immediate::Unlinked(
                                                true_block.label.clone(),
                                            )),
                                        });
                                        block.sequence.push(Instruction {
                                            op: Opcode::Jz,
                                            format: InstrFormat::I(Immediate::Unlinked(
                                                true_block.label.clone(),
                                            )),
                                        });
                                        block.sequence.push(Instruction {
                                            op: Opcode::Jmp,
                                            format: InstrFormat::I(Immediate::Unlinked(
                                                false_block.label.clone(),
                                            )),
                                        });
                                    }
                                    InfixOp::GtEq => {
                                        block.sequence.push(Instruction {
                                            op: Opcode::Jc,
                                            format: InstrFormat::I(Immediate::Unlinked(
                                                false_block.label.clone(),
                                            )),
                                        });
                                        block.sequence.push(Instruction {
                                            op: Opcode::Jz,
                                            format: InstrFormat::I(Immediate::Unlinked(
                                                true_block.label.clone(),
                                            )),
                                        });
                                        block.sequence.push(Instruction {
                                            op: Opcode::Jmp,
                                            format: InstrFormat::I(Immediate::Unlinked(
                                                true_block.label.clone(),
                                            )),
                                        });
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            self.functions
                                .get_mut(func_name)
                                .unwrap()
                                .code
                                .push(true_block);
                            {
                                let block =
                                    self.functions.get_mut(func_name).unwrap().last_block_mut();
                                block.sequence.push(Instruction {
                                    op: Opcode::Mov,
                                    format: InstrFormat::RI(tmp_dest, Immediate::Linked(1)),
                                });
                                block.sequence.push(Instruction {
                                    op: Opcode::Jmp,
                                    format: InstrFormat::I(Immediate::Unlinked(
                                        end_block.label.clone(),
                                    )),
                                });
                            }
                            self.functions
                                .get_mut(func_name)
                                .unwrap()
                                .code
                                .push(false_block);
                            {
                                let block =
                                    self.functions.get_mut(func_name).unwrap().last_block_mut();
                                block.sequence.push(Instruction {
                                    op: Opcode::Mov,
                                    format: InstrFormat::RI(tmp_dest, Immediate::Linked(0)),
                                });
                            }
                            self.functions
                                .get_mut(func_name)
                                .unwrap()
                                .code
                                .push(end_block);
                            match dest.storage() {
                                VarStorage::Register(dest_reg) => {
                                    let block =
                                        self.functions.get_mut(func_name).unwrap().last_block_mut();
                                    block.sequence.push(Instruction {
                                        op: Opcode::Mov,
                                        format: InstrFormat::RR(*dest_reg, tmp_dest),
                                    });
                                }
                                VarStorage::StackOffset(dest_offset) => {
                                    let block =
                                        self.functions.get_mut(func_name).unwrap().last_block_mut();
                                    store_reg_to_reg_offset(*dest_offset, tmp_dest, block);
                                }
                                VarStorage::Immediate(_) => {
                                    return Err(Error::from(CompError::InvalidExpression(
                                        infix.span.location_line() as usize,
                                        infix.first_line(),
                                    )))
                                }
                            }
                            self.functions
                                .get_mut(func_name)
                                .unwrap()
                                .take_back_reg(tmp_dest);
                        }
                        InfixOp::Mod => todo!(),

                        InfixOp::Shl => todo!(),
                        InfixOp::Shr => todo!(),

                        _ => unreachable!(),
                    }
                } else {
                    match infix.item.op.item {
                        InfixOp::Assign => {
                            if let Expr::Ident(lhs) = &infix.item.lhs.item {
                                let dest = self
                                    .functions
                                    .get(func_name)
                                    .unwrap()
                                    .get(lhs.item.0)
                                    .unwrap();
                                self.compile_expr(&infix.item.rhs.item, Some(&dest), func_name)?;
                            } else {
                                return Err(Error::from(CompError::InvalidExpression(
                                    infix.span.location_line() as usize,
                                    infix.first_line(),
                                )));
                            }
                        }
                        InfixOp::ShlAssign => todo!(),
                        InfixOp::ShrAssign => todo!(),
                        InfixOp::MulAssign
                        | InfixOp::AddAssign
                        | InfixOp::SubAssign
                        | InfixOp::DivAssign
                        | InfixOp::AndAssign
                        | InfixOp::OrAssign
                        | InfixOp::XorAssign => {
                            if let Expr::Ident(lhs) = &infix.item.lhs.item {
                                let dest = self
                                    .functions
                                    .get(func_name)
                                    .unwrap()
                                    .get(lhs.item.0)
                                    .unwrap();
                                let rhs = Var::new(
                                    dest.typ(),
                                    "rhs",
                                    var::VarStorage::Register(tmp_rhs_reg),
                                );
                                self.compile_expr(&infix.item.rhs.item, Some(&rhs), func_name)?;
                                let op = match infix.item.op.item {
                                    InfixOp::AddAssign => Opcode::Add,
                                    InfixOp::SubAssign => Opcode::Sub,
                                    InfixOp::MulAssign => Opcode::Mul,
                                    InfixOp::DivAssign => Opcode::Div,
                                    InfixOp::AndAssign => Opcode::And,
                                    InfixOp::OrAssign => Opcode::Or,
                                    InfixOp::XorAssign => Opcode::Xor,
                                    _ => unreachable!(),
                                };
                                {
                                    match dest.storage() {
                                        VarStorage::Register(dest_reg) => {
                                            let block = self
                                                .functions
                                                .get_mut(func_name)
                                                .unwrap()
                                                .last_block_mut();
                                            block.sequence.push(Instruction {
                                                op,
                                                format: InstrFormat::RRR(
                                                    *dest_reg,
                                                    *dest_reg,
                                                    tmp_rhs_reg,
                                                ),
                                            });
                                        }
                                        VarStorage::StackOffset(dest_offset) => {
                                            let tmp_dest = self
                                                .functions
                                                .get_mut(func_name)
                                                .unwrap()
                                                .any_reg()
                                                .unwrap();
                                            let block = self
                                                .functions
                                                .get_mut(func_name)
                                                .unwrap()
                                                .last_block_mut();
                                            load_reg_offset_to_reg(tmp_dest, *dest_offset, block);
                                            block.sequence.push(Instruction {
                                                op,
                                                format: InstrFormat::RRR(
                                                    tmp_dest,
                                                    tmp_dest,
                                                    tmp_rhs_reg,
                                                ),
                                            });
                                            store_reg_to_reg_offset(*dest_offset, tmp_dest, block);
                                        }
                                        VarStorage::Immediate(_) => {
                                            return Err(Error::from(CompError::InvalidExpression(
                                                infix.span.location_line() as usize,
                                                infix.first_line(),
                                            )))
                                        }
                                    }
                                }
                            } else {
                                return Err(Error::from(CompError::InvalidExpression(
                                    infix.span.location_line() as usize,
                                    infix.first_line(),
                                )));
                            }
                        }

                        _ => unreachable!(),
                    }
                }
                self.functions
                    .get_mut(func_name)
                    .unwrap()
                    .take_back_reg(tmp_rhs_reg);
                self.functions
                    .get_mut(func_name)
                    .unwrap()
                    .take_back_reg(tmp_lhs_reg);
            }
        }
        Ok(())
    }

    fn compile_jump_statement(&mut self, stmt: &JumpStatement<'_>, func_name: &str) -> Result<()> {
        match stmt {
            JumpStatement::Goto(label) => todo!(),
            JumpStatement::Return(value) => {
                if let Some(expr) = value.as_ref().map(|v| &v.item) {
                    let dest = Var::new(
                        self.functions.get(func_name).unwrap().return_type,
                        "returnval",
                        var::VarStorage::Register(Register::R1),
                    );
                    self.compile_expr(expr, Some(&dest), func_name)?;
                }
                if func_name == "start" {
                    // `start` is special, as we need to halt instead of returning
                    let func = self.functions.get_mut(func_name).unwrap();
                    func.epologue.sequence.push(Instruction {
                        op: Opcode::Halt,
                        format: InstrFormat::OpOnly,
                    });
                }
            }
        }
        Ok(())
    }

    fn compile_builtin_statement(
        &mut self,
        stmt: &BuiltinStatement<'_>,
        func_name: &str,
    ) -> Result<()> {
        match stmt.builtin.item {
            Keyword::Printi => {
                let dest = self
                    .functions
                    .get_mut(func_name)
                    .unwrap()
                    .push("dest", TypeSpecifier::Int);
                self.compile_expr(&stmt.expr.item, Some(&dest), func_name)?;
                let tmp_dest = self
                    .functions
                    .get_mut(func_name)
                    .unwrap()
                    .any_reg()
                    .unwrap();
                let block = self.functions.get_mut(func_name).unwrap().last_block_mut();

                load_reg_offset_to_reg(tmp_dest, dest.get_stack_offset().unwrap(), block);
                block.sequence.push(Instruction {
                    op: Opcode::Printi,
                    format: InstrFormat::R(tmp_dest),
                });
                self.functions.get_mut(func_name).unwrap().take_back(dest);
                self.functions
                    .get_mut(func_name)
                    .unwrap()
                    .take_back_reg(tmp_dest);
            }
            _ => {
                return Err(Error::from(CompError::InvalidStatement(
                    stmt.builtin.span.location_line() as usize,
                    stmt.builtin.first_line(),
                )))
            }
        }
        Ok(())
    }

    fn compile_selection_statemtnt(
        &mut self,
        stmt: &SelectionStatement<'_>,
        func_name: &str,
    ) -> Result<()> {
        let cond = self
            .functions
            .get_mut(func_name)
            .unwrap()
            .push("cond", TypeSpecifier::Int);
        self.compile_expr(&stmt.cond.item, Some(&cond), func_name)?;
        let nblocks = self.functions.get(func_name).unwrap().code.len();
        let if_block = Block::new(&format!("{func_name}if{nblocks}"));
        let else_block = Block::new(&format!("{func_name}else{nblocks}"));
        let end_block = Block::new(&format!("{func_name}end{nblocks}"));

        let tmp_cond = self
            .functions
            .get_mut(func_name)
            .unwrap()
            .any_reg()
            .unwrap();
        {
            let block = self.functions.get_mut(func_name).unwrap().last_block_mut();
            let cond_offset = cond.get_stack_offset().unwrap();
            load_reg_offset_to_reg(tmp_cond, cond_offset, block);
            block.sequence.push(Instruction {
                op: Opcode::Sub,
                format: InstrFormat::RRR(Register::R0, tmp_cond, Register::R0),
            });
            block.sequence.push(Instruction {
                op: Opcode::Jz,
                format: InstrFormat::I(Immediate::Unlinked(else_block.label.clone())),
            });
            block.sequence.push(Instruction {
                op: Opcode::Jmp,
                format: InstrFormat::I(Immediate::Unlinked(if_block.label.clone())),
            });
        }
        self.functions
            .get_mut(func_name)
            .unwrap()
            .take_back_reg(tmp_cond);

        {
            let func = self.functions.get_mut(func_name).unwrap();
            func.code.push(if_block);
            self.compile_statement(&stmt.if_body.item, func_name)?;
        }
        self.functions
            .get_mut(func_name)
            .unwrap()
            .last_block_mut()
            .sequence
            .push(Instruction {
                op: Opcode::Jmp,
                format: InstrFormat::I(Immediate::Unlinked(end_block.label.clone())),
            });
        if let Some(else_body) = &stmt.else_body {
            {
                let func = self.functions.get_mut(func_name).unwrap();
                func.code.push(else_block);
                self.compile_statement(&else_body.item, func_name)?;
            }
        } else {
            {
                let func = self.functions.get_mut(func_name).unwrap();
                func.code.push(else_block);
            }
        }
        self.functions
            .get_mut(func_name)
            .unwrap()
            .last_block_mut()
            .sequence
            .push(Instruction {
                op: Opcode::Jmp,
                format: InstrFormat::I(Immediate::Unlinked(end_block.label.clone())),
            });
        {
            let func = self.functions.get_mut(func_name).unwrap();
            func.code.push(end_block);
        }

        Ok(())
    }

    fn compile_iteration_statement(
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
            load_reg_offset_to_reg(tmp_cond, cond_offset, block);
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

    fn compile_statement(&mut self, stmt: &Statement<'_>, func_name: &str) -> Result<()> {
        match stmt {
            Statement::Builtin(stmt) => self
                .compile_builtin_statement(&stmt.item, func_name)
                .context("compiling statement"),
            Statement::Labeled(stmt) => todo!(),
            Statement::Compound(stmt) => {
                for block_item in stmt.item.0.iter() {
                    self.compile_block_item(&block_item.item, func_name)?;
                }
                Ok(())
            }
            Statement::Selection(stmt) => self
                .compile_selection_statemtnt(&stmt.item, func_name)
                .context("compiling statement"),
            Statement::Iteration(stmt) => self
                .compile_iteration_statement(&stmt.item, func_name)
                .context("compiling statement"),
            Statement::Jump(stmt) => self
                .compile_jump_statement(&stmt.item, func_name)
                .context("compiling statement"),
            Statement::Expr(expr) => self
                .compile_expr(&expr.item, None, func_name)
                .context("compiling statement"),
        }
    }

    fn compile_block_item(&mut self, block_item: &BlockItem<'_>, func_name: &str) -> Result<()> {
        // let ctx = self.functions.get_mut(func_name).unwrap();
        match block_item {
            BlockItem::Declaration(decl) => self
                .compile_local_decl(&decl.item, func_name)
                .context("compiling block item"),
            BlockItem::Statement(stmt) => self
                .compile_statement(&stmt.item, func_name)
                .context("compiling block item"),
        }
    }

    fn compile_function_definition(&mut self, func: &FunctionDefinition<'_>) -> Result<()> {
        let func_name = func.id.item.id.item.0.to_owned();
        if !self.functions.contains_key(&func_name) {
            log::debug!("Compiling function: {func_name}");
            let ctx = FunctionContext {
                return_type: func.id.item.typ.item,
                name: func_name.clone(),
                args: vec![],
                allocations: FxHashMap::default(),
                avail_regs: FxHashSet::from_iter([
                    Register::R1,
                    Register::R2,
                    Register::R3,
                    Register::R4,
                    Register::R5,
                    Register::R6,
                ]),
                stack_offset: 0,
                max_stack_offset: 0,
                code: vec![Block::new(&format!("{func_name}outer"))],
                epologue: Block::new(&format!("{func_name}epilogue")),
            };
            for _arg in func.param_list.iter() {
                // args.push(Ssa::new(arg.item.typ.item, arg.item.id.item.0));
                todo!("function args")
            }

            self.functions.insert(func_name.clone(), ctx);
            for block_item in func.body.item.0.iter() {
                self.compile_block_item(&block_item.item, &func_name)?;
            }
        } else {
            return Err(Error::new(CompError::DuplicateSymbol(func_name)));
        }
        Ok(())
    }

    fn compile_external_declaration(&mut self, inp: &ExternalDeclaration<'_>) -> Result<()> {
        match inp {
            ExternalDeclaration::FunctionDefinition(func) => self
                .compile_function_definition(&func.item)
                .context("compiling function definition"),
            ExternalDeclaration::Declaration(decl) => todo!("global declarations"),
        }
    }

    pub fn compile(&mut self, ast: &TranslationUnit<'_>) -> Result<String> {
        for external in ast.0.iter() {
            self.compile_external_declaration(&external.item)
                .context("compiling translation unit")?;
        }
        let mut lines = vec![];
        for (func_name, func) in self.functions.iter() {
            lines.push(format!("%{func_name}"));
            if func_name == "start" {
                // setup a few things before we get into the code
                lines.push("    mov sp $0xf000".to_owned());
                lines.push("    mov fp sp".to_owned());
            }
            // lines.push("    push fp".to_owned());
            lines.push(format!("    sub fp fp ${}", func.max_stack_offset));
            lines.push("    mov sp fp".to_owned());

            for block in func.code.iter() {
                lines.push(format!("%{}", block.label));
                for instr in block.sequence.iter() {
                    lines.push(format!("    {}", instr));
                }
            }
            lines.push(format!("%{func_name}epilogue"));
            lines.push(format!("    add fp fp ${}", func.max_stack_offset));
            lines.push("    mov sp fp".to_owned());
            for instr in func.epologue.sequence.iter() {
                lines.push(format!("    {}", instr));
                // }
            }
        }
        Ok(lines.join("\n"))
    }
}

pub fn compile(program_text: &str) -> Result<String> {
    let span = Span::new_extra(program_text, "");
    let (garbage, tokens) = lex_tokens(span).map_err(|e| {
        let span = match e {
            nom::Err::Error(e) => e.input,
            nom::Err::Failure(e) => e.input,
            nom::Err::Incomplete(_) => span,
        };
        Error::from(CompError::Syntax {
            loc: span.location_line() as usize,
            span: span
                .fragment()
                .split_whitespace()
                .next()
                .unwrap()
                .to_string(),
        })
    })?;
    if !garbage.is_empty() {
        return Err(anyhow::Error::new(CompError::FoundGarbage {
            loc: garbage.location_line() as usize,
            span: garbage
                .fragment()
                .split_whitespace()
                .next()
                .unwrap()
                .to_string(),
        }));
    }
    let tokens = Tokens::new(&tokens);
    let (garbage, tu) = TranslationUnit::parse(tokens).map_err(|e| {
        let span = match e {
            nom::Err::Error(e) => e.input.first_span(),
            nom::Err::Failure(e) => e.input.first_span(),
            nom::Err::Incomplete(_) => tokens.first_span(),
        };
        Error::from(CompError::UnexpectedToken(
            span.location_line() as usize,
            span.fragment()
                .split_whitespace()
                .next()
                .unwrap()
                .to_string(),
        ))
    })?;
    if !garbage.tok.len() == 1 && garbage.tok[0].item != CToken::EOI {
        return Err(anyhow::Error::new(CompError::FoundGarbage {
            loc: garbage.first_span().location_line() as usize,
            span: garbage
                .first_span()
                .fragment()
                .split_whitespace()
                .next()
                .unwrap()
                .to_string(),
        }));
    }

    let mut state = CompilerState::default();
    state.compile(&tu.item)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile() {
        let program = r#"
void start() {
    int a = 0;
    printi a;
    a = 1;
    printi a;
    return;
}"#;
        let asm = compile(program).unwrap();
        println!("{}", asm);
    }
}
