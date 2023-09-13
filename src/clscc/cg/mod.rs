use std::{
    backtrace::Backtrace,
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
    common::{Punctuator, TokenVariant},
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
#[error("CodegenError: {source}\nBacktrace:{backtrace}")]
pub struct CodegenError {
    #[source]
    pub source: CodegenErrorKind,
    pub backtrace: Backtrace,
}

impl CodegenError {
    pub fn new(source: CodegenErrorKind) -> Self {
        Self {
            source,
            backtrace: Backtrace::capture(),
        }
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
    pub rvalue: bool,
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
                rvalue: false,
            }),
        }
    }

    pub fn new_rvalue(
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
                rvalue: true,
            }),
        }
    }

    pub fn rvalue(&self) -> bool {
        self.inner.rvalue
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

#[derive(Clone)]
pub enum BlockElem {
    Label(String),
    Instruction(Instruction),
    Comment(String),
    Subscope(Rc<Scope>),
}

pub struct Scope {
    pub sref: Weak<Scope>,
    pub parent: Weak<Scope>,
    pub children: RefCell<Vec<Rc<Scope>>>,
    pub next_id: AtomicUsize,
    pub values: RefCell<FxHashMap<ValueId, Value>>,
    pub available_registers: RefCell<FxHashSet<Register>>,
    pub stack_offset: RefCell<usize>,
    pub label: String,
    pub sequence: RefCell<Vec<BlockElem>>,
}

impl Scope {
    pub fn new_root() -> Rc<Self> {
        Rc::new_cyclic(|sref| Self {
            sref: sref.clone(),
            parent: Weak::new(),
            children: RefCell::new(Vec::default()),
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
            label: String::new(),
            sequence: RefCell::new(Vec::default()),
        })
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

    pub fn insert(
        &self,
        ident: Option<String>,
        ty: Option<Type>,
        storage: ValueStorage,
        rvalue: bool,
    ) -> Value {
        let id = self
            .next_id
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let value = if rvalue {
            Value::new_rvalue(ValueId(id), ident, ty, storage)
        } else {
            Value::new(ValueId(id), ident, ty, storage)
        };
        self.values.borrow_mut().insert(ValueId(id), value.clone());
        value
    }

    pub fn extern_label(&self, ident: &str) -> Value {
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

    pub fn any_register(&self, ty: Option<Type>) -> Result<Value> {
        let mut available_registers = self.available_registers.borrow_mut();
        if let Some(reg) = available_registers.iter().next().cloned() {
            log::debug!("Allocating register {:?} with type {:?}", reg, ty);
            available_registers.remove(&reg);
            let id = self
                .next_id
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            let val = Value::new(ValueId(id), None, ty, ValueStorage::Register(reg));

            self.values.borrow_mut().insert(ValueId(id), val.clone());
            Ok(val)
        } else {
            Err(Error::new(CodegenError::new(
                CodegenErrorKind::NoRegistersAvailable,
            )))
        }
    }

    pub fn push_stack(&self, ident: Option<String>, ty: Type, rvalue: bool) -> Result<Value> {
        if ty.sizeof() == 0 {
            return Err(Error::from(CodegenError::new(
                CodegenErrorKind::UnsizedValue(ty),
            )));
        }
        let mut stack_offset = self.stack_offset.borrow_mut();
        *stack_offset += ty.sizeof();
        let val = self.insert(ident, Some(ty), ValueStorage::Stack(*stack_offset), rvalue);

        Ok(val)
    }

    pub fn retake(&self, val: Value) -> Option<()> {
        if let Some(val) = self.values.borrow_mut().remove(&val.inner.id) {
            match val.storage() {
                ValueStorage::Register(reg) => {
                    log::debug!("Freeing register {:?}", reg);
                    self.available_registers.borrow_mut().insert(*reg);
                }
                ValueStorage::Stack(_) => {}
                ValueStorage::Immediate(_) => {}
            }
            Some(())
        } else {
            None
        }
    }

    pub fn push_block_elem(&self, elem: BlockElem) {
        self.sequence.borrow_mut().push(elem)
    }

    pub fn push_instr(&self, instr: Instruction) {
        self.push_block_elem(BlockElem::Instruction(instr));
    }

    pub fn push_comment(&self, comment: String) {
        self.push_block_elem(BlockElem::Comment(comment));
    }

    pub fn push_label(&self, label: String) {
        self.push_block_elem(BlockElem::Label(label));
    }

    pub fn push_scope(&self, label: String) -> Rc<Scope> {
        let child = Rc::new_cyclic(|sref| Self {
            sref: sref.clone(),
            parent: self.sref.clone(),
            children: RefCell::new(Vec::default()),
            next_id: AtomicUsize::new(self.next_id.load(std::sync::atomic::Ordering::SeqCst)),
            values: self.values.clone(),
            available_registers: self.available_registers.clone(),
            stack_offset: self.stack_offset.clone(),
            label,
            sequence: RefCell::new(Vec::default()),
        });
        self.children.borrow_mut().push(child.clone());
        self.push_block_elem(BlockElem::Subscope(child.clone()));
        child
    }

    pub fn stack_offset(&self) -> usize {
        *self.stack_offset.borrow()
    }

    pub fn num_children(&self) -> usize {
        self.children.borrow().len()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct ScopeId(pub usize);

pub struct Codegen {
    pub root_scope: Rc<Scope>,
    pub current_scope: Rc<Scope>,
}

impl Codegen {
    pub fn new() -> Self {
        let root_scope = Scope::new_root();
        Self {
            current_scope: root_scope.clone(),
            root_scope,
        }
    }

    fn push_scope(&mut self, label: String) -> Rc<Scope> {
        let scope = self.current_scope.push_scope(format!(
            "{}{}{}",
            self.current_scope.label,
            label,
            self.current_scope.num_children()
        ));
        self.current_scope = scope.clone();
        scope
    }

    fn pop_scope(&mut self) {
        let parent = self.current_scope.parent.upgrade().unwrap();
        self.current_scope = parent;
    }

    fn recursive_gen(root: &Rc<Scope>, lines: &mut Vec<String>, depth: usize) {
        for elem in root.sequence.borrow().iter() {
            match elem {
                BlockElem::Instruction(instr) => {
                    lines.push(format!("{}{}", " ".repeat(depth * 2), instr))
                }
                BlockElem::Comment(comment) => {
                    lines.push(format!("{}; {}", " ".repeat(depth * 2), comment))
                }
                BlockElem::Label(label) => {
                    lines.push(format!("{}%{}", " ".repeat((depth - 1) * 2), label))
                }
                BlockElem::Subscope(scope) => {
                    lines.push(format!("{}%{}", " ".repeat(depth * 2), scope.label));
                    Self::recursive_gen(scope, lines, depth + 1)
                }
            }
        }
    }

    pub fn gen(&mut self, root: &AstNode<'_>) -> Result<String> {
        self.dfs_walk(root, None, true)?;
        let mut lines = vec![];
        assert!(self.current_scope.parent.upgrade().is_none());
        Self::recursive_gen(&self.root_scope, &mut lines, 0);
        Ok(lines.join("\n"))
    }

    pub fn dfs_walk(
        &mut self,
        node: &AstNode<'_>,
        parent: Option<&AstNode<'_>>,
        rvalue: bool,
    ) -> Result<Option<Value>> {
        match node.ast.as_ref() {
            Ast::Binary { op, lhs, rhs } => self.cg_binary(op, lhs, rhs, Some(node)),
            Ast::Block(stmts) => {
                self.push_scope("block".to_owned());
                for stmt in stmts {
                    self.dfs_walk(stmt, Some(node), rvalue)?;
                }
                self.pop_scope();
                Ok(None)
            }
            Ast::Call { name, args } => {
                let name = self.dfs_walk(name, Some(node), rvalue)?.ok_or(Error::from(
                    CodegenError::new(CodegenErrorKind::Expected {
                        expected: vec!["function name".to_string()],
                        got: String::new(),
                    }),
                ))?;
                let mut arg_vals = vec![];
                for arg in args {
                    arg_vals.push(self.dfs_walk(arg, Some(node), rvalue)?.ok_or(Error::from(
                        CodegenError::new(CodegenErrorKind::Expected {
                            expected: vec!["argument".to_string()],
                            got: String::new(),
                        }),
                    ))?);
                }
                self.cg_call(name, arg_vals)
            }
            Ast::Cast { expr } => {
                let expr = self.dfs_walk(expr, Some(node), rvalue)?.unwrap();
                Ok(Some(expr))
            }
            Ast::Declaration { decl } => {
                let mut decl_vals = vec![];
                for decl in decl {
                    if let Ast::Ident(id) = &*decl.ast {
                        if let TokenVariant::Ident(id) = &id.variant {
                            if let Some(Type::Array(elem, numel)) = decl.typ.as_ref() {
                                // push the elements
                                for _elem in 0..*numel {
                                    self.current_scope.push_stack(
                                        None,
                                        *elem.to_owned(),
                                        decl.rvalue,
                                    )?;
                                }
                                // push the pointer
                                let ptr = self.current_scope.push_stack(
                                    Some(id.to_owned()),
                                    Type::Array(elem.clone(), *numel),
                                    // can't change the base address of an array
                                    true,
                                )?;
                                decl_vals.push(ptr);
                            } else {
                                let val = self.current_scope.push_stack(
                                    Some(id.to_owned()),
                                    decl.typ.as_ref().unwrap().to_owned(),
                                    decl.rvalue,
                                )?;
                                decl_vals.push(val);
                            }
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
                let body = self.dfs_walk(body, Some(node), rvalue)?;
                let cond = self.dfs_walk(cond, Some(node), rvalue)?;
                todo!()
            }
            Ast::For {
                init,
                cond,
                step,
                body,
            } => {
                let init = self.dfs_walk(init, Some(node), rvalue)?;
                let cond = self.dfs_walk(cond, Some(node), rvalue)?;
                let step = self.dfs_walk(step, Some(node), rvalue)?;
                let body = self.dfs_walk(body, Some(node), rvalue)?;
                todo!()
            }
            Ast::FunctionDef { name, params, body } => {
                let (name, name_val) = if let Ast::Ident(name) = &*name.ast {
                    if let TokenVariant::Ident(name) = &name.variant {
                        (name.to_owned(), self.current_scope.extern_label(name))
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
                let mut param_types = vec![];
                for param in params {
                    let param = self.dfs_walk(param, Some(node), rvalue)?.unwrap();
                    param_types.push(param.ty().cloned().unwrap());
                    if let Some(ident) = param.ident() {
                        param_names.push(ident.to_owned());
                    } else {
                        return Err(Error::new(CodegenError::new(CodegenErrorKind::Expected {
                            expected: vec!["identifier".to_string()],
                            got: String::new(),
                        })));
                    }
                }
                assert!(self.current_scope.parent.upgrade().is_none());
                let scope = self
                    .root_scope
                    .push_scope(name_val.ident().unwrap().to_owned());
                self.current_scope = scope;

                self.current_scope.extern_label("printi");
                if name == "start" {
                    self.cga_start_prelude()?;
                }
                for (param, param_ty) in param_names.into_iter().zip(param_types.into_iter()) {
                    let val = self.cga_pop_stack(Some(param_ty))?;
                    self.current_scope.insert(
                        Some(param),
                        val.ty().cloned(),
                        val.storage().clone(),
                        true,
                    );
                }
                self.current_scope.push_label(format!("{}body", name));
                if let Ast::Block(stmts) = &*body.ast {
                    for stmt in stmts.iter() {
                        self.dfs_walk(stmt, Some(node), rvalue)?;
                    }
                } else {
                    unreachable!()
                }
                let prologue = vec![
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
                ];
                for elem in prologue.into_iter().rev() {
                    self.current_scope.sequence.borrow_mut().insert(0, elem);
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

                self.current_scope.push_label(format!("{}return", name));
                if name == "start" {
                    self.cga_halt()?;
                } else {
                    todo!("return from function")
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
                self.push_scope("if".to_string());
                let cond = self.dfs_walk(cond, Some(node), rvalue)?.unwrap();
                self.pop_scope();
                self.cg_if_else(cond, then, els, Some(node))
            }
            Ast::Index { name, index } => {
                let name = self.dfs_walk(name, Some(node), rvalue)?.unwrap();
                let index = self.dfs_walk(index, Some(node), rvalue)?.unwrap();
                self.cg_index(name, index, rvalue)
            }
            Ast::Integer(val) => Ok(Some(self.current_scope.insert(
                None,
                Some(Type::Int),
                ValueStorage::Immediate(Immediate::Linked((*val).try_into()?)),
                true,
            ))),
            Ast::Member { name, member } => {
                let name = self.dfs_walk(name, Some(node), rvalue)?;
                let member = self.dfs_walk(member, Some(node), rvalue)?;
                todo!()
            }
            Ast::Postfix { op, lhs } => {
                if let Ast::Token(op) = &*op.ast {
                    if let TokenVariant::Punctuator(op) = op.variant {
                        if op == Punctuator::PlusPlus {
                            let lhs = self.dfs_walk(lhs, Some(node), rvalue)?.unwrap();
                            let result = self.current_scope.any_register(lhs.ty().cloned())?;
                            let one = self.current_scope.insert(
                                None,
                                Some(Type::Int),
                                ValueStorage::Immediate(Immediate::Linked(1)),
                                true,
                            );
                            let result = self.cga_add(result, lhs.clone(), one.clone())?;
                            self.current_scope.retake(one);
                            self.cga_store(lhs.clone(), result.clone())?;
                            self.current_scope.retake(result);
                            Ok(Some(lhs))
                        } else if op == Punctuator::MinusMinus {
                            let lhs = self.dfs_walk(lhs, Some(node), rvalue)?.unwrap();
                            let result = self.current_scope.any_register(lhs.ty().cloned())?;
                            let one = self.current_scope.insert(
                                None,
                                Some(Type::Int),
                                ValueStorage::Immediate(Immediate::Linked(1)),
                                true,
                            );
                            let result = self.cga_sub(result, lhs.clone(), one.clone())?;
                            self.current_scope.retake(one);
                            self.cga_store(lhs.clone(), result.clone())?;
                            self.current_scope.retake(result);
                            Ok(Some(lhs))
                        } else {
                            Err(Error::new(CodegenError::new(
                                CodegenErrorKind::ExpectedToken {
                                    expected: vec![
                                        TokenVariant::Punctuator(Punctuator::PlusPlus),
                                        TokenVariant::Punctuator(Punctuator::MinusMinus),
                                    ],
                                    got: TokenVariant::Punctuator(op),
                                },
                            )))
                        }
                    } else {
                        Err(Error::new(CodegenError::new(
                            CodegenErrorKind::ExpectedToken {
                                expected: vec![
                                    TokenVariant::Punctuator(Punctuator::PlusPlus),
                                    TokenVariant::Punctuator(Punctuator::MinusMinus),
                                ],
                                got: TokenVariant::Punctuator(Punctuator::PlusPlus),
                            },
                        )))
                    }
                } else {
                    Err(Error::new(CodegenError::new(CodegenErrorKind::Expected {
                        expected: vec!["token".to_owned()],
                        got: String::new(),
                    })))
                }
            }
            Ast::Program(stmts) => {
                for stmt in stmts {
                    self.dfs_walk(stmt, Some(node), rvalue)?;
                }
                Ok(None)
            }
            Ast::Return(expr) => {
                let expr = if let Some(expr) = expr {
                    Some(self.dfs_walk(expr, Some(node), rvalue)?.unwrap())
                } else {
                    None
                };
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
                let op = self.dfs_walk(op, Some(node), rvalue)?;
                let rhs = self.dfs_walk(rhs, Some(node), rvalue)?;
                todo!()
            }
            Ast::While { cond, body } => self.cg_while(cond, body, Some(node)),
        }
    }
}

impl Default for Codegen {
    fn default() -> Self {
        Self::new()
    }
}
