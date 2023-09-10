use anyhow::{Context, Error, Result};
use rustc_hash::FxHashMap;

use crate::{
    asm::Span,
    plat::{Immediate, InstrFormat, Instruction, Opcode, Register},
};

use self::ssa::{Ssa, Storage};

use super::{
    lexer::{gen::Keyword, lex_tokens},
    parser::ast::*,
    CToken, CompError, Tokens,
};

pub mod ssa;

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
    pub args: Vec<Ssa>,
    pool: FxHashMap<String, Ssa>,
    stack_offset: u16,
    pub code: Vec<Block>,
    pub epologue: Block,
}

impl FunctionContext {
    pub fn last_block_mut(&mut self) -> Option<&mut Block> {
        self.code.last_mut()
    }

    pub fn insert(&mut self, ssa: Ssa) {
        self.pool.insert(ssa.name().to_owned(), ssa);
    }

    pub fn get(&self, name: &str) -> Option<Ssa> {
        self.pool.get(name).cloned()
    }

    pub fn push(&mut self, name: &str, typ: TypeSpecifier) -> Ssa {
        self.stack_offset += 2;
        let ssa = Ssa::new(
            typ,
            name,
            ssa::Storage::AddrOffset(self.stack_offset, Register::FP),
        );
        self.pool.insert(name.to_owned(), ssa.clone());
        ssa
    }

    pub fn get_or_push(&mut self, name: &str, typ: TypeSpecifier) -> Ssa {
        if let Some(ssa) = self.pool.get(name) {
            ssa.clone()
        } else {
            self.push(name, typ)
        }
    }
}

#[inline]
#[doc(hidden)]
fn store_reg_to_reg_offset(addr: Register, addr_offset: u16, src: Register, block: &mut Block) {
    block.sequence.push(Instruction {
        op: Opcode::Stl,
        format: InstrFormat::RRI(addr, src, Immediate::Linked(addr_offset)),
    });
    block.sequence.push(Instruction {
        op: Opcode::Sth,
        format: InstrFormat::RRI(addr, src, Immediate::Linked(addr_offset + 1)),
    });
}

#[inline]
#[doc(hidden)]
fn load_reg_offset_to_reg(dest: Register, addr: Register, addr_offset: u16, block: &mut Block) {
    block.sequence.push(Instruction {
        op: Opcode::Ldl,
        format: InstrFormat::RRI(dest, addr, Immediate::Linked(addr_offset)),
    });
    block.sequence.push(Instruction {
        op: Opcode::Ldh,
        format: InstrFormat::RRI(dest, addr, Immediate::Linked(addr_offset + 1)),
    });
}

#[derive(Default)]
pub struct CompilerState {
    pub functions: FxHashMap<String, FunctionContext>,
    pub out_buf: String,
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

    /// Clobbers R4, R5.
    fn compile_expr(&mut self, expr: &Expr<'_>, dest: Option<&Ssa>, func_name: &str) -> Result<()> {
        match expr {
            Expr::Ident(id) => {
                if let Some(dest) = dest {
                    let func = self.functions.get_mut(func_name).unwrap();
                    let src = func.get(id.item.0).unwrap();
                    let block = func.last_block_mut().unwrap();
                    match (dest.storage(), src.storage()) {
                        (
                            Storage::AddrOffset(dest_off, dest),
                            Storage::AddrOffset(src_off, src),
                        ) => {
                            let tmp_reg = Register::R5;
                            load_reg_offset_to_reg(tmp_reg, *src, *src_off, block);
                            store_reg_to_reg_offset(*dest, *dest_off, tmp_reg, block);
                        }
                        (Storage::Register(dest), Storage::AddrOffset(src_off, src)) => {
                            load_reg_offset_to_reg(*dest, *src, *src_off, block);
                        }

                        st => todo!("{:?}", st),
                    }
                } else {
                    todo!()
                }
            }
            Expr::Constant(val) => {
                if let Some(dest) = dest {
                    let func = self.functions.get_mut(func_name).unwrap();
                    let block = func.last_block_mut().unwrap();
                    let Constant::Integer(val) = val.item;
                    match dest.storage() {
                        ssa::Storage::Immediate(_) => todo!("return an error here"),
                        ssa::Storage::Register(reg) => block.sequence.push(Instruction {
                            op: Opcode::Mov,
                            format: InstrFormat::RI(*reg, Immediate::Linked(val)),
                        }),
                        ssa::Storage::AddrOffset(addr_offset, addr) => {
                            let tmp_reg = Register::R5;
                            block.sequence.push(Instruction {
                                op: Opcode::Mov,
                                format: InstrFormat::RI(tmp_reg, Immediate::Linked(val)),
                            });
                            store_reg_to_reg_offset(*addr, *addr_offset, tmp_reg, block);
                        }
                    }
                } else {
                    todo!("error here");
                }
            }
            Expr::Infix(infix) => {
                // if let Some(dest) = dest {
                let infix = &infix.item;
                match &infix.op.item {
                    InfixOp::Assign => {
                        if let Expr::Ident(lhs) = &infix.lhs.item {
                            let dest = self
                                .functions
                                .get(func_name)
                                .unwrap()
                                .get(lhs.item.0)
                                .unwrap();
                            self.compile_expr(&infix.rhs.item, Some(&dest), func_name)?;
                        } else {
                            todo!("error here")
                        }
                    }
                    InfixOp::Add | InfixOp::Div | InfixOp::Mul | InfixOp::Sub => {
                        if let Some(dest) = dest {
                            self.compile_expr(&infix.lhs.item, Some(dest), func_name)?;
                            let rhs = self
                                .functions
                                .get_mut(func_name)
                                .unwrap()
                                .push("rhs", *dest.typ());
                            self.compile_expr(&infix.rhs.item, Some(&rhs), func_name)?;
                            let block = self
                                .functions
                                .get_mut(func_name)
                                .unwrap()
                                .last_block_mut()
                                .unwrap();

                            let op = match &infix.op.item {
                                InfixOp::Add => Opcode::Add,
                                InfixOp::Sub => Opcode::Sub,
                                InfixOp::Mul => Opcode::Mul,
                                InfixOp::Div => Opcode::Div,
                                _ => unreachable!(),
                            };

                            let (rhs_offset, rhs_reg) = rhs.get_reg_offset().unwrap();
                            // dest might be R5, so use R4
                            let tmp_rhs = Register::R4;
                            load_reg_offset_to_reg(tmp_rhs, rhs_reg, rhs_offset, block);
                            match dest.storage() {
                                Storage::Register(dest_reg) => {
                                    block.sequence.push(Instruction {
                                        op,
                                        format: InstrFormat::RRR(*dest_reg, *dest_reg, tmp_rhs),
                                    });
                                }
                                Storage::AddrOffset(dest_offset, dest_addr) => {
                                    // we know it's not R5, so this should be safe
                                    let tmp_dest = Register::R5;
                                    load_reg_offset_to_reg(
                                        tmp_dest,
                                        *dest_addr,
                                        *dest_offset,
                                        block,
                                    );
                                    block.sequence.push(Instruction {
                                        op,
                                        format: InstrFormat::RRR(tmp_dest, tmp_dest, tmp_rhs),
                                    });
                                    store_reg_to_reg_offset(
                                        *dest_addr,
                                        *dest_offset,
                                        tmp_dest,
                                        block,
                                    );
                                }
                                Storage::Immediate(_) => todo!("error here"),
                            }
                        } else {
                            todo!("error here")
                        }
                    }
                    InfixOp::EqEq => todo!(),
                    InfixOp::NotEqual => todo!(),
                    InfixOp::Lt => todo!(),
                    InfixOp::LtEq => todo!(),
                    InfixOp::Gt => todo!(),
                    InfixOp::GtEq => todo!(),
                }
            }
        }
        Ok(())
    }

    fn compile_jump_statement(&mut self, stmt: &JumpStatement<'_>, func_name: &str) -> Result<()> {
        match stmt {
            JumpStatement::Goto(label) => todo!(),
            JumpStatement::Return(value) => {
                if let Some(expr) = value.as_ref().map(|v| &v.item) {
                    let dest = Ssa::new(
                        self.functions.get(func_name).unwrap().return_type,
                        "returnval",
                        ssa::Storage::Register(Register::R1),
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
                let dest = Ssa::new(
                    TypeSpecifier::Int,
                    "temp",
                    ssa::Storage::Register(Register::R2),
                );
                self.compile_expr(&stmt.expr.item, Some(&dest), func_name)?;
                let block = self
                    .functions
                    .get_mut(func_name)
                    .unwrap()
                    .last_block_mut()
                    .unwrap();
                block.sequence.push(Instruction {
                    op: Opcode::Printi,
                    format: InstrFormat::R(Register::R2),
                });
            }
            _ => todo!("error here"),
        }
        Ok(())
    }

    fn compile_statement(&mut self, stmt: &Statement<'_>, func_name: &str) -> Result<()> {
        match stmt {
            Statement::Builtin(stmt) => self
                .compile_builtin_statement(&stmt.item, func_name)
                .context("compiling statement"),
            Statement::Labeled(stmt) => todo!(),
            Statement::Compound(stmt) => todo!(),
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
                pool: FxHashMap::default(),
                stack_offset: 0,
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

    pub fn compile(&mut self, ast: &TranslationUnit<'_>) -> Result<()> {
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
            lines.push("    push fp".to_owned());
            lines.push("    mov fp sp".to_owned());
            lines.push(format!("    sub sp sp ${}", func.stack_offset));

            for block in func.code.iter() {
                lines.push(format!("%{}", block.label));
                for instr in block.sequence.iter() {
                    lines.push(format!("    {}", instr));
                }
            }
            lines.push(format!("%{func_name}epilogue"));
            lines.push("    mov sp fp".to_owned());
            lines.push("    pop fp".to_owned());
            for instr in func.epologue.sequence.iter() {
                lines.push(format!("    {}", instr));
                // }
            }
        }
        self.out_buf = lines.join("\n");
        Ok(())
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
    state.compile(&tu.item)?;

    Ok(state.out_buf)
}
