use anyhow::{Context, Error, Result};
use rustc_hash::FxHashMap;

use crate::{
    asm::Span,
    plat::{InstrFormat, Instruction, Opcode, Register},
};

use self::ssa::Ssa;

use super::{lexer::lex_tokens, parser::ast::*, CToken, CompError, Tokens};

pub mod ssa;

pub struct Block<'a> {
    pub label: String,
    pub sequence: Vec<Instruction<'a>>,
}

impl<'a> Block<'a> {
    pub fn new(label: &str) -> Self {
        Self {
            label: label.to_owned(),
            sequence: vec![],
        }
    }
}

pub struct FunctionContext<'a> {
    pub name: String,
    pub return_type: TypeSpecifier,
    pub args: Vec<Ssa<'a>>,
    pool: FxHashMap<String, Ssa<'a>>,
    stack_offset: u16,
    pub code: Vec<Block<'a>>,
}

impl<'a> FunctionContext<'a> {
    pub fn insert(&mut self, ssa: Ssa<'a>) {
        self.pool.insert(ssa.name().to_owned(), ssa);
    }

    pub fn get(&self, name: &str) -> Option<Ssa> {
        self.pool.get(name).cloned()
    }

    pub fn push(&mut self, name: &str, typ: TypeSpecifier) -> Ssa<'a> {
        self.stack_offset += 2;
        let ssa = Ssa::new(
            typ,
            name,
            ssa::Storage::RegOffset(self.stack_offset, Register::FP),
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

#[derive(Default)]
pub struct CompilerState<'a> {
    pub functions: FxHashMap<String, FunctionContext<'a>>,
    pub out_buf: String,
}

impl<'a> CompilerState<'a> {
    fn compile_local_decl(&mut self, decl: &Declaration<'_>, func_name: &str) -> Result<()> {
        let ssa = self
            .functions
            .get_mut(func_name)
            .unwrap()
            .push(decl.id.item.id.item.0, decl.id.item.typ.item);
        todo!()
    }

    fn compile_expr(
        &mut self,
        expr: &Expr<'_>,
        dest: Option<&Ssa<'_>>,
        func_name: &str,
    ) -> Result<()> {
        match expr {
            Expr::Ident(id) => todo!(),
            Expr::Constant(val) => todo!(),
            Expr::Infix(infix) => todo!(),
        }
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
                    func.code.push(Block {
                        label: "done".into(),
                        sequence: vec![Instruction {
                            op: Opcode::Halt,
                            format: InstrFormat::OpOnly,
                        }],
                    });
                }
            }
        }
        Ok(())
    }

    fn compile_statement(&mut self, stmt: &Statement<'_>, func_name: &str) -> Result<()> {
        match stmt {
            Statement::Labeled(stmt) => todo!(),
            Statement::Compound(stmt) => todo!(),
            Statement::Jump(stmt) => self
                .compile_jump_statement(&stmt.item, func_name)
                .context("compiling statement"),
            Statement::Expr(expr) => todo!(),
        }
    }

    fn compile_block_item(&mut self, block_item: &BlockItem<'a>, func_name: &str) -> Result<()> {
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

    fn compile_function_definition(&mut self, func: &'a FunctionDefinition<'_>) -> Result<()> {
        let func_name = func.id.item.id.item.0.to_owned();
        if !self.functions.contains_key(&func_name) {
            log::debug!("Compiling function: {func_name}");
            let ctx = FunctionContext {
                name: func_name.clone(),
                return_type: func.id.item.typ.item,
                args: vec![],
                pool: FxHashMap::default(),
                stack_offset: 0,
                code: vec![],
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

    fn compile_external_declaration(&mut self, inp: &'a ExternalDeclaration<'_>) -> Result<()> {
        match inp {
            ExternalDeclaration::FunctionDefinition(func) => self
                .compile_function_definition(&func.item)
                .context("compiling function definition"),
            ExternalDeclaration::Declaration(decl) => todo!("global declarations"),
        }
    }

    pub fn compile(&mut self, ast: &'a TranslationUnit<'_>) -> Result<()> {
        for external in ast.0.iter() {
            self.compile_external_declaration(&external.item)
                .context("compiling translation unit")?;
        }
        let mut lines = vec![];
        for (func_name, func) in self.functions.iter() {
            lines.push(format!("%{func_name}"));
            if func_name == "start" {
                // setup a few things before we get into the code
                lines.push("    mov     sp $0xf000".to_owned());
                lines.push("    mov     fp sp".to_owned());
            }
            for block in func.code.iter() {
                lines.push(format!("%{}", block.label));
                for instr in block.sequence.iter() {
                    lines.push(format!("    {}", instr));
                }
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
