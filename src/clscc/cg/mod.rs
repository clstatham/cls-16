use std::{
    cell::RefCell,
    rc::{Rc, Weak},
    sync::atomic::AtomicUsize,
    vec,
};

use anyhow::{Error, Result};
use cls16::{Immediate, InstrFormat, Instruction, Opcode, Register};
use rustc_hash::{FxHashMap, FxHashSet};
use thiserror::Error;

use crate::clscc::{
    common::{Punctuator, Token, TokenVariant},
    parser::{Ast, AstNode, Type},
};

pub mod cg_arch;
pub mod cg_ast;

#[derive(Debug, Error)]
pub enum CodegenErrorKind {
    #[error("expected {expected:?}, got {got:?}")]
    Expected { expected: Vec<String>, got: String },
    #[error("expected {expected:?}, got {got:?}")]
    ExpectedToken {
        expected: Vec<TokenVariant>,
        got: TokenVariant,
    },
    #[error("expected {expected:?}, got {got:?}")]
    ExpectedType { expected: Vec<Type>, got: Type },
    #[error("expected {expected:?}, got {got:?}")]
    ExpectedValue {
        expected: Vec<ValueStorage>,
        got: ValueStorage,
    },
    #[error("attempted to push unsized type {0:?} to stack")]
    UnsizedValue(Type),
    #[error("identifier {0:?} not found")]
    IdentifierNotFound(String),
    #[error("cannot store to immediate")]
    CannotStoreToImmediate,
    #[error("no registers available")]
    NoRegistersAvailable,
    #[error("not a register: {0:?}")]
    NotARegister(ValueStorage),
    #[error("not an immediate value: {0:?}")]
    NotAnImmediate(ValueStorage),
    #[error("not a stack value: {0:?}")]
    NotOnStack(ValueStorage),
}

#[derive(Debug, Error)]
#[error("CodegenError: {source}")]
pub struct CodegenError {
    pub source: CodegenErrorKind,
}

impl CodegenError {
    pub fn new(source: CodegenErrorKind) -> Self {
        Self { source }
    }
}

#[derive(Debug, Clone)]
pub enum ValueStorage {
    Register(Register),
    Stack(usize),
    Immediate(Immediate),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct ValueId(pub usize);

#[derive(Debug, Clone)]
pub struct ValueInner {
    pub id: ValueId,
    pub ident: Option<String>,
    pub ty: Option<Type>,
    pub storage: ValueStorage,
}

#[derive(Debug, Clone)]
pub struct Value {
    inner: Rc<ValueInner>,
}

impl Value {
    pub fn new(
        id: ValueId,
        ident: Option<String>,
        ty: Option<Type>,
        storage: ValueStorage,
    ) -> Self {
        Self {
            inner: Rc::new(ValueInner {
                id,
                ident,
                ty,
                storage,
            }),
        }
    }

    pub fn ident(&self) -> Option<&String> {
        self.inner.ident.as_ref()
    }

    pub fn ty(&self) -> Option<&Type> {
        self.inner.ty.as_ref()
    }

    pub fn storage(&self) -> &ValueStorage {
        &self.inner.storage
    }

    pub fn get_register(&self) -> Result<Register> {
        if let ValueStorage::Register(reg) = self.inner.storage {
            Ok(reg)
        } else {
            Err(Error::new(CodegenError::new(
                CodegenErrorKind::NotARegister(self.storage().clone()),
            )))
        }
    }

    pub fn get_immediate(&self) -> Result<Immediate> {
        if let ValueStorage::Immediate(ref imm) = self.inner.storage {
            Ok(imm.to_owned())
        } else {
            Err(Error::new(CodegenError::new(
                CodegenErrorKind::NotAnImmediate(self.storage().clone()),
            )))
        }
    }

    pub fn get_stack_offset(&self) -> Result<usize> {
        if let ValueStorage::Stack(offset) = self.inner.storage {
            Ok(offset)
        } else {
            Err(Error::new(CodegenError::new(CodegenErrorKind::NotOnStack(
                self.storage().clone(),
            ))))
        }
    }
}

#[derive(Debug, Clone)]
pub enum BlockElem {
    Instruction(Instruction),
    Comment(String),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub label: String,
    pub sequence: Vec<BlockElem>,
}

pub struct Scope {
    pub parent: Weak<Scope>,
    pub next_id: AtomicUsize,
    pub values: RefCell<FxHashMap<ValueId, Value>>,
    pub available_registers: RefCell<FxHashSet<Register>>,
    pub stack_offset: RefCell<usize>,
    pub blocks: RefCell<Vec<Block>>,
}

impl Scope {
    pub fn new_root() -> Rc<Self> {
        Rc::new(Self {
            parent: Weak::new(),
            next_id: AtomicUsize::new(0),
            values: RefCell::new(FxHashMap::default()),
            available_registers: RefCell::new(FxHashSet::from_iter([
                Register::R2,
                Register::R3,
                Register::R4,
                Register::R5,
                Register::R6,
            ])),
            stack_offset: RefCell::new(0),
            blocks: RefCell::new(Vec::default()),
        })
    }

    fn new_child(parent: &Rc<Self>) -> Rc<Self> {
        Rc::new(Self {
            parent: Rc::downgrade(parent),
            next_id: AtomicUsize::new(parent.next_id.load(std::sync::atomic::Ordering::SeqCst)),
            values: parent.values.clone(),
            available_registers: parent.available_registers.clone(),
            stack_offset: parent.stack_offset.clone(),
            blocks: RefCell::new(Vec::default()),
        })
    }

    pub fn num_blocks(&self) -> usize {
        self.blocks.borrow().len()
    }

    pub fn get_by_ident(&self, ident: &str) -> Option<Value> {
        for value in self.values.borrow().values().cloned() {
            if let Some(value_ident) = value.ident() {
                if value_ident == ident {
                    return Some(value);
                }
            }
        }
        None
    }

    pub fn get_by_id(&self, id: ValueId) -> Option<Value> {
        self.values.borrow().get(&id).cloned()
    }

    pub fn insert(&self, ident: Option<String>, ty: Option<Type>, storage: ValueStorage) -> Value {
        let id = self
            .next_id
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let value = Value::new(ValueId(id), ident, ty, storage);
        self.values.borrow_mut().insert(ValueId(id), value.clone());
        value
    }

    pub fn insert_label(&self, ident: &str) -> Value {
        let id = self
            .next_id
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let value = Value::new(
            ValueId(id),
            Some(ident.to_string()),
            None,
            ValueStorage::Immediate(Immediate::Unlinked(ident.to_string())),
        );
        self.values.borrow_mut().insert(ValueId(id), value.clone());
        value
    }

    pub fn any_register(&self) -> Result<Value> {
        let mut available_registers = self.available_registers.borrow_mut();
        if let Some(reg) = available_registers.iter().next().cloned() {
            available_registers.remove(&reg);
            let id = self
                .next_id
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            let val = Value::new(ValueId(id), None, None, ValueStorage::Register(reg));

            self.values.borrow_mut().insert(ValueId(id), val.clone());
            Ok(val)
        } else {
            Err(Error::new(CodegenError::new(
                CodegenErrorKind::NoRegistersAvailable,
            )))
        }
    }

    pub fn push_stack(&self, ident: Option<String>, ty: Type) -> Result<Value> {
        if ty.sizeof() == 0 {
            return Err(Error::from(CodegenError::new(
                CodegenErrorKind::UnsizedValue(ty),
            )));
        }
        let mut stack_offset = self.stack_offset.borrow_mut();
        *stack_offset += ty.sizeof();
        let id = self
            .next_id
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let val = Value::new(
            ValueId(id),
            ident,
            Some(ty),
            ValueStorage::Stack(*stack_offset),
        );

        self.values.borrow_mut().insert(ValueId(id), val.clone());

        Ok(val)
    }

    pub fn retake(&self, val: Value) -> Option<Value> {
        if let Some(val) = self.values.borrow_mut().remove(&val.inner.id) {
            match val.storage() {
                ValueStorage::Register(reg) => {
                    self.available_registers.borrow_mut().insert(*reg);
                }
                ValueStorage::Stack(_) => {}
                ValueStorage::Immediate(_) => {}
            }
            Some(val)
        } else {
            None
        }
    }

    pub fn push_block(&self, label: String) {
        self.blocks.borrow_mut().push(Block {
            label,
            sequence: vec![],
        });
    }

    pub fn with_last_block<F, R>(&self, f: F) -> Result<R>
    where
        F: FnOnce(&mut Block) -> Result<R>,
    {
        let mut blocks = self.blocks.borrow_mut();
        if let Some(block) = blocks.last_mut() {
            f(block)
        } else {
            unreachable!("no blocks in scope")
        }
    }

    pub fn push_block_elem(&self, elem: BlockElem) {
        let mut blocks = self.blocks.borrow_mut();
        if let Some(block) = blocks.last_mut() {
            block.sequence.push(elem);
        } else {
            unreachable!("no blocks in scope")
        }
    }

    pub fn push_instr(&self, instr: Instruction) {
        self.push_block_elem(BlockElem::Instruction(instr));
    }

    pub fn push_comment(&self, comment: String) {
        self.push_block_elem(BlockElem::Comment(comment));
    }

    pub fn stack_offset(&self) -> usize {
        *self.stack_offset.borrow()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct ScopeId(pub usize);

pub struct Codegen {
    pub scopes: FxHashMap<ScopeId, Rc<Scope>>,
    pub current_scope: Rc<Scope>,
}

impl Codegen {
    pub fn new() -> Self {
        let current_scope = Scope::new_root();
        Self {
            current_scope: current_scope.clone(),
            scopes: FxHashMap::from_iter([(ScopeId(0), current_scope)]),
        }
    }

    fn push_scope(&mut self) -> Rc<Scope> {
        let scope = Scope::new_child(&self.current_scope);
        let id = ScopeId(self.scopes.len());
        self.scopes.insert(id, scope.clone());
        self.current_scope = scope.clone();
        scope
    }

    fn pop_scope(&mut self) {
        let parent = self.current_scope.parent.upgrade().unwrap();
        self.current_scope = parent;
    }

    pub fn gen(&mut self, root: &mut AstNode<'_>) -> Result<String> {
        self.dfs_walk(root)?;
        let mut lines = vec![];
        for scope in self.scopes.values() {
            for block in scope.blocks.borrow().iter() {
                lines.push(format!("%{}", block.label));
                for elem in block.sequence.iter() {
                    match elem {
                        BlockElem::Instruction(instr) => lines.push(format!("    {}", instr)),
                        BlockElem::Comment(comment) => lines.push(format!("    ; {}", comment)),
                    }
                }
            }
        }

        Ok(lines.join("\n"))
    }

    pub fn dfs_walk(&mut self, node: &mut AstNode<'_>) -> Result<Option<Value>> {
        match node.ast.as_mut() {
            Ast::Binary { op, lhs, rhs } => self.cg_binary(op, lhs, rhs),
            Ast::Block(stmts) => {
                for stmt in stmts {
                    self.dfs_walk(stmt)?;
                }
                Ok(None)
            }
            Ast::Call { name, args } => {
                let name = self.dfs_walk(name)?.ok_or(Error::from(CodegenError::new(
                    CodegenErrorKind::Expected {
                        expected: vec!["function name".to_string()],
                        got: String::new(),
                    },
                )))?;
                let mut arg_vals = vec![];
                for arg in args {
                    arg_vals.push(self.dfs_walk(arg)?.ok_or(Error::from(CodegenError::new(
                        CodegenErrorKind::Expected {
                            expected: vec!["argument".to_string()],
                            got: String::new(),
                        },
                    )))?);
                }
                self.cg_call(name, arg_vals)
            }
            Ast::Cast { expr } => {
                let expr = self.dfs_walk(expr)?;
                todo!()
            }
            Ast::Declaration { decl } => {
                let mut decl_vals = vec![];
                for decl in decl {
                    if let Ast::Ident(id) = &*decl.ast {
                        if let TokenVariant::Ident(id) = &id.variant {
                            let val = self.current_scope.push_stack(
                                Some(id.to_owned()),
                                decl.typ.as_ref().unwrap().to_owned(),
                            )?;
                            decl_vals.push(val);
                        } else {
                            return Err(Error::new(CodegenError::new(
                                CodegenErrorKind::ExpectedToken {
                                    expected: vec![TokenVariant::Ident(String::new())],
                                    got: id.variant.clone(),
                                },
                            )));
                        }
                    } else {
                        return Err(Error::new(CodegenError::new(CodegenErrorKind::Expected {
                            expected: vec!["identifier".to_string()],
                            got: format!("{:?}", decl.ast),
                        })));
                    }
                }
                Ok(None)
            }
            Ast::DoWhile { body, cond } => {
                let body = self.dfs_walk(body)?;
                let cond = self.dfs_walk(cond)?;
                todo!()
            }
            Ast::For {
                init,
                cond,
                step,
                body,
            } => {
                let init = self.dfs_walk(init)?;
                let cond = self.dfs_walk(cond)?;
                let step = self.dfs_walk(step)?;
                let body = self.dfs_walk(body)?;
                todo!()
            }
            Ast::FunctionDef { name, params, body } => {
                let (name, name_val) = if let Ast::Ident(name) = &*name.ast {
                    if let TokenVariant::Ident(name) = &name.variant {
                        (name.to_owned(), self.current_scope.insert_label(name))
                    } else {
                        return Err(Error::new(CodegenError::new(
                            CodegenErrorKind::ExpectedToken {
                                expected: vec![TokenVariant::Ident(String::new())],
                                got: name.variant.clone(),
                            },
                        )));
                    }
                } else {
                    return Err(Error::new(CodegenError::new(CodegenErrorKind::Expected {
                        expected: vec!["identifier".to_string()],
                        got: format!("{:?}", name.ast),
                    })));
                };
                let mut param_names = vec![];
                for param in params {
                    let param = self.dfs_walk(param)?.unwrap();
                    if let Some(ident) = param.ident() {
                        param_names.push(ident.to_owned());
                    } else {
                        return Err(Error::new(CodegenError::new(CodegenErrorKind::Expected {
                            expected: vec!["identifier".to_string()],
                            got: String::new(),
                        })));
                    }
                }
                self.push_scope();
                self.current_scope.insert_label("printi");
                self.current_scope
                    .push_block(name_val.ident().unwrap().to_owned());
                if name == "start" {
                    self.cga_start_prelude()?;
                }
                for param in param_names {
                    let val = self.cga_pop_stack()?;
                    self.current_scope.insert(
                        Some(param),
                        val.ty().cloned(),
                        val.storage().clone(),
                    );
                }
                self.current_scope.push_block(format!("{}body", name));
                let body = self.dfs_walk(body)?;
                {
                    let mut blocks = self.current_scope.blocks.borrow_mut();
                    blocks.first_mut().unwrap().sequence.extend_from_slice(&[
                        BlockElem::Instruction(Instruction {
                            op: Opcode::Sub,
                            format: InstrFormat::RRI(
                                Register::FP,
                                Register::FP,
                                Immediate::Linked(self.current_scope.stack_offset() as u16),
                            ),
                        }),
                        BlockElem::Instruction(Instruction {
                            op: Opcode::Mov,
                            format: InstrFormat::RR(Register::SP, Register::FP),
                        }),
                    ]);
                }
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Add,
                    format: InstrFormat::RRI(
                        Register::FP,
                        Register::FP,
                        Immediate::Linked(self.current_scope.stack_offset() as u16),
                    ),
                });
                self.current_scope.push_instr(Instruction {
                    op: Opcode::Mov,
                    format: InstrFormat::RR(Register::SP, Register::FP),
                });
                if name == "start" {
                    self.cga_halt()?;
                }

                self.pop_scope();
                Ok(None)
            }
            Ast::Ident(ident) => {
                if let TokenVariant::Ident(ident) = &ident.variant {
                    if let Some(val) = self.current_scope.get_by_ident(ident) {
                        return Ok(Some(val.clone()));
                    } else {
                        return Err(Error::new(CodegenError::new(
                            CodegenErrorKind::IdentifierNotFound(ident.clone()),
                        )));
                    }
                }
                Err(Error::new(CodegenError::new(
                    CodegenErrorKind::ExpectedToken {
                        expected: vec![TokenVariant::Ident(String::new())],
                        got: ident.variant.clone(),
                    },
                )))
            }
            Ast::If { cond, then, els } => {
                let cond = self.dfs_walk(cond)?;
                let then = self.dfs_walk(then)?;
                let els = if let Some(els) = els {
                    Some(self.dfs_walk(els)?)
                } else {
                    None
                };
                todo!()
            }
            Ast::Index { name, index } => {
                let name = self.dfs_walk(name)?;
                let index = self.dfs_walk(index)?;
                todo!()
            }
            Ast::Integer(val) => Ok(Some(self.current_scope.insert(
                None,
                Some(Type::Int),
                ValueStorage::Immediate(Immediate::Linked((*val).try_into()?)),
            ))),
            Ast::Member { name, member } => {
                let name = self.dfs_walk(name)?;
                let member = self.dfs_walk(member)?;
                todo!()
            }
            Ast::Pointer(expr) => {
                let expr = self.dfs_walk(expr)?;
                todo!()
            }
            Ast::Postfix { op, lhs } => {
                let op = self.dfs_walk(op)?;
                let lhs = self.dfs_walk(lhs)?;
                dbg!(&op, &lhs);
                todo!()
            }
            Ast::Program(stmts) => {
                for stmt in stmts {
                    self.dfs_walk(stmt)?;
                }
                Ok(None)
            }
            Ast::Return(expr) => {
                let expr = if let Some(expr) = expr {
                    Some(self.dfs_walk(expr)?)
                } else {
                    None
                };
                todo!()
            }
            Ast::Sizeof(ty) => {
                let ty = self.dfs_walk(ty)?;
                todo!()
            }
            Ast::StringLiteral(val) => {
                todo!()
            }
            Ast::Token(t) => {
                dbg!(t.string_repr);
                Ok(None)
            }
            Ast::Unary { op, rhs } => {
                let op = self.dfs_walk(op)?;
                let rhs = self.dfs_walk(rhs)?;
                todo!()
            }
            Ast::While { cond, body } => {
                let cond = self.dfs_walk(cond)?;
                let body = self.dfs_walk(body)?;
                todo!()
            }
        }
    }
}

impl Default for Codegen {
    fn default() -> Self {
        Self::new()
    }
}
