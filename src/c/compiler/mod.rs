pub use anyhow::{Context, Error, Result};
pub use rustc_hash::{FxHashMap, FxHashSet};

pub use crate::{
    asm::Span,
    plat::{Immediate, InstrFormat, Instruction, Opcode, Register},
};

pub use var::{Var, VarStorage};

pub use crate::c::parser::ast::*;
pub use crate::c::{
    lexer::{gen::Keyword, lex_tokens},
    CToken, CompError, Tokens,
};

pub mod ast;
pub mod optimizers;
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
    next_id: u32,
    allocations: FxHashMap<u32, Var>,
    avail_regs: FxHashSet<Register>,
    pub stack_offset: u16,
    pub max_stack_offset: u16,
    pub code: Vec<Block>,
    pub prologue: Block,
    pub epologue: Block,
}

impl FunctionContext {
    pub fn last_block_mut(&mut self) -> &mut Block {
        self.code.last_mut().unwrap()
    }

    pub fn insert(&mut self, ssa: Var) {
        self.allocations.insert(ssa.id().to_owned(), ssa);
    }

    pub fn get(&self, id: u32) -> Option<Var> {
        self.allocations.get(&id).cloned()
    }

    pub fn get_by_name(&self, name: &str) -> Option<Var> {
        self.allocations
            .iter()
            .find(|(_, v)| v.name() == Some(&name.to_owned()))
            .map(|(_, v)| v.clone())
    }

    pub fn push_arr(&mut self, name: Option<&str>, elem_typ: TypeSpecifier, numel: u16) -> Var {
        self.stack_offset += 2 * numel;
        self.max_stack_offset += 2 * numel;
        let ssa = Var::new(
            elem_typ,
            self.next_id,
            name,
            VarStorage::StackOffset(self.stack_offset),
        );
        self.next_id += 1;
        self.allocations.insert(ssa.id(), ssa.clone());
        ssa
    }

    pub fn push(&mut self, name: Option<&str>, typ: TypeSpecifier) -> Var {
        self.stack_offset += 2;
        self.max_stack_offset += 2;
        let ssa = Var::new(
            typ,
            self.next_id,
            name,
            VarStorage::StackOffset(self.stack_offset),
        );
        self.next_id += 1;
        self.allocations.insert(ssa.id(), ssa.clone());
        ssa
    }

    pub fn take_back(&mut self, var: Var) {
        if self.allocations.remove(&var.id()).is_some() {
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

    pub fn any_reg(&mut self, typ: TypeSpecifier) -> Option<(Register, Var)> {
        [
            Register::R2,
            Register::R3,
            Register::R4,
            Register::R5,
            Register::R6,
        ]
        .into_iter()
        .find(|&reg| self.avail_regs.remove(&reg))
        .map(|reg| {
            let var = Var::new(typ, self.next_id, None, VarStorage::Register(reg));
            self.next_id += 1;
            self.insert(var.clone());
            (reg, var)
        })
    }
}

#[inline]
#[doc(hidden)]
pub(crate) fn push_reg_to_stack(reg: Register, block: &mut Block) {
    block.sequence.push(Instruction {
        op: Opcode::Sub,
        format: InstrFormat::RRI(Register::SP, Register::SP, Immediate::Linked(2)),
    });
    block.sequence.push(Instruction {
        op: Opcode::Stl,
        format: InstrFormat::RRI(Register::SP, reg, Immediate::Linked(0)),
    });
    block.sequence.push(Instruction {
        op: Opcode::Sth,
        format: InstrFormat::RRI(Register::SP, reg, Immediate::Linked(1)),
    });
}

#[inline]
#[doc(hidden)]
pub(crate) fn pop_reg_from_stack(reg: Register, block: &mut Block) {
    block.sequence.push(Instruction {
        op: Opcode::Ldl,
        format: InstrFormat::RRI(reg, Register::SP, Immediate::Linked(0)),
    });
    block.sequence.push(Instruction {
        op: Opcode::Ldh,
        format: InstrFormat::RRI(reg, Register::SP, Immediate::Linked(1)),
    });
    block.sequence.push(Instruction {
        op: Opcode::Add,
        format: InstrFormat::RRI(Register::SP, Register::SP, Immediate::Linked(2)),
    });
}

#[inline]
#[doc(hidden)]
pub(crate) fn store_reg_to_fp_offset(addr_offset: u16, src: Register, block: &mut Block) {
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
pub(crate) fn load_fp_offset_to_reg(dest: Register, addr_offset: u16, block: &mut Block) {
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
        let init_expr = &decl.init_expr.item;
        match init_expr {
            Expr::Ident(id) => {
                // default initialization for the variable
                let var = self
                    .functions
                    .get_mut(func_name)
                    .unwrap()
                    .push(Some(id.item.0), decl.typ.item.clone());
                let var_offset = var.get_stack_offset().unwrap();
                {
                    let block = self.functions.get_mut(func_name).unwrap().last_block_mut();
                    block.sequence.push(Instruction {
                        op: Opcode::Stl,
                        format: InstrFormat::RRI(
                            Register::FP,
                            Register::R0,
                            Immediate::Linked(var_offset),
                        ),
                    });
                    block.sequence.push(Instruction {
                        op: Opcode::Sth,
                        format: InstrFormat::RRI(
                            Register::FP,
                            Register::R0,
                            Immediate::Linked(var_offset + 1),
                        ),
                    });
                }
            }
            Expr::Constant(_) => todo!(),
            Expr::Infix(_) => todo!(),
            Expr::Postfix(postfix) => {
                if let PostfixExpr::Index(index) = &postfix.item {
                    if let Expr::Ident(id) = index.item.arr.item {
                        if let Expr::Constant(numel) = index.item.idx.item {
                            let Constant::Integer(numel) = numel.item;
                            let first_elem = self.functions.get_mut(func_name).unwrap().push_arr(
                                None,
                                decl.typ.item.clone(),
                                numel,
                            );
                            let first_elem_offset = first_elem.get_stack_offset().unwrap();
                            let var = self.functions.get_mut(func_name).unwrap().push(
                                Some(id.item.0),
                                TypeSpecifier::Pointer(Box::new(decl.typ.item.clone())),
                            );
                            let var_offset = var.get_stack_offset().unwrap();

                            let (tmp_reg, tmp) = self
                                .functions
                                .get_mut(func_name)
                                .unwrap()
                                .any_reg(TypeSpecifier::Int)
                                .unwrap();
                            {
                                let block =
                                    self.functions.get_mut(func_name).unwrap().last_block_mut();

                                block.sequence.push(Instruction {
                                    op: Opcode::Add,
                                    format: InstrFormat::RRI(
                                        tmp_reg,
                                        Register::FP,
                                        Immediate::Linked(first_elem_offset),
                                    ),
                                });
                                store_reg_to_fp_offset(var_offset, tmp_reg, block);
                            }
                            self.functions.get_mut(func_name).unwrap().take_back(tmp);
                        }
                    }
                }
            }
            Expr::Unary(_) => todo!(),
        }
        Ok(())
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
            let mut ctx = FunctionContext {
                return_type: func.id.item.typ.item.clone(),
                name: func_name.clone(),
                allocations: FxHashMap::default(),
                avail_regs: FxHashSet::from_iter([
                    Register::R2,
                    Register::R3,
                    Register::R4,
                    Register::R5,
                    Register::R6,
                ]),
                next_id: 0,
                stack_offset: 0,
                max_stack_offset: 0,
                prologue: Block::new(&format!("{func_name}prologue")),
                code: vec![Block::new(&format!("{func_name}outer"))],
                epologue: Block::new(&format!("{func_name}epilogue")),
            };

            ctx.prologue.sequence.push(Instruction {
                op: Opcode::Add,
                format: InstrFormat::RRI(Register::SP, Register::SP, Immediate::Linked(2)),
            });
            for arg in func.param_list.iter().rev() {
                let var = ctx.push(Some(arg.item.id.item.0), arg.item.typ.item.clone());
                pop_reg_from_stack(Register::R6, &mut ctx.prologue);
                store_reg_to_fp_offset(
                    var.get_stack_offset().unwrap(),
                    Register::R6,
                    &mut ctx.prologue,
                );
            }

            self.functions.insert(func_name.clone(), ctx);
            for block_item in func.body.item.0.iter() {
                self.compile_block_item(&block_item.item, &func_name)?;
            }

            let ctx = self.functions.get_mut(&func_name).unwrap();
            ctx.prologue.sequence.insert(
                0,
                Instruction {
                    op: Opcode::Sub,
                    format: InstrFormat::RRI(
                        Register::FP,
                        Register::FP,
                        Immediate::Linked(ctx.max_stack_offset),
                    ),
                },
            );
            ctx.prologue.sequence.push(Instruction {
                op: Opcode::Mov,
                format: InstrFormat::RR(Register::SP, Register::FP),
            });

            if func_name == "start" {
                // `start` is special, as we need to halt instead of returning
                ctx.epologue.sequence.push(Instruction {
                    op: Opcode::Halt,
                    format: InstrFormat::OpOnly,
                });
            } else {
                ctx.epologue.sequence.push(Instruction {
                    op: Opcode::Add,
                    format: InstrFormat::RRI(
                        Register::FP,
                        Register::FP,
                        Immediate::Linked(ctx.max_stack_offset),
                    ),
                });

                let (reg, reg_var) = ctx.any_reg(TypeSpecifier::Int).unwrap();
                pop_reg_from_stack(reg, &mut ctx.epologue);
                ctx.epologue.sequence.push(Instruction {
                    op: Opcode::Mov,
                    format: InstrFormat::RR(Register::SP, Register::FP),
                });
                ctx.epologue.sequence.push(Instruction {
                    op: Opcode::Jmp,
                    format: InstrFormat::R(reg),
                });
                ctx.take_back(reg_var);
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
            ExternalDeclaration::Declaration(_decl) => todo!("global declarations"),
        }
    }

    pub fn compile(&mut self, ast: &TranslationUnit<'_>, optimization_level: u8) -> Result<String> {
        for external in ast.0.iter() {
            self.compile_external_declaration(&external.item)
                .context("compiling translation unit")?;
        }
        // broken currently
        // if optimization_level >= 2 {
        //     self.optimize_functions()?;
        // }
        if optimization_level >= 1 {
            self.optimize_blocks()?;
        }

        let mut lines = vec![];
        for (func_name, func) in self.functions.iter() {
            lines.push(format!("%{func_name}"));
            if func_name == "start" {
                // setup a few things before we get into the code
                lines.push("    mov sp $0xf000".to_owned());
                lines.push("    mov fp sp".to_owned());
            }

            lines.push(format!("%{func_name}prologue"));
            for instr in func.prologue.sequence.iter() {
                lines.push(format!("    {}", instr));
            }

            for block in func.code.iter() {
                lines.push(format!("%{}", block.label));
                for instr in block.sequence.iter() {
                    lines.push(format!("    {}", instr));
                }
            }
            lines.push(format!("%{func_name}epilogue"));
            for instr in func.epologue.sequence.iter() {
                lines.push(format!("    {}", instr));
            }
        }
        Ok(lines.join("\n"))
    }
}

pub fn compile(program_text: &str, optimization_level: u8) -> Result<String> {
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
    state.compile(&tu.item, optimization_level)
}
