use std::rc::Rc;

use crate::{
    c::parser::ast::TypeSpecifier,
    plat::{Immediate, Register},
};

/// The "physical" representation of a [`Var`].
/// This is what is actually operated on with instructions.
#[derive(Debug)]
pub enum VarStorage {
    /// The [`Var`] is represented by an immediate value of some sort.
    Immediate(Immediate),
    /// The [`Var`] is represented by a register.
    Register(Register),
    /// The [`Var`] is represented by a location on the stack, relative to [`FP`][Register::FP].
    StackOffset(u16),
}

struct VarInner {
    typ: TypeSpecifier,
    name: String,
    storage: VarStorage,
}

/// A temporary value somewhere in a C program.
/// It can be represented inside as a register, a location on the stack, or as an immediate constant.
///
/// Note that this logical representation is immutable once allocated, in a similar fashion to the Singular Static Assignment (SSA) form used in LLVM.
/// The main difference is that the actual location in memory that a [`Var`] represents IS mutable, and can be reused.
#[derive(Clone)]
pub struct Var {
    inner: Rc<VarInner>,
}

impl Var {
    /// Creates a new [`Var`] with the specified expected type, identifier, and storage method.
    pub fn new(typ: TypeSpecifier, name: &str, storage: VarStorage) -> Self {
        Self {
            inner: Rc::new(VarInner {
                typ,
                name: name.to_owned(),
                storage,
            }),
        }
    }

    /// This [`Var`]'s identifier.
    pub fn name(&self) -> &str {
        &self.inner.name
    }

    /// This [`Var`]'s expected C type.
    pub fn typ(&self) -> TypeSpecifier {
        self.inner.typ
    }

    /// This [`Var`]'s "physical" representation inside the CLS-16 CPU, for example a register or a location on the stack.
    /// This is what is actually operated on with instructions.
    pub fn storage(&self) -> &VarStorage {
        &self.inner.storage
    }

    /// If this [`Var`] is represented as a register, returns that register, otherwise returns `None`.
    pub fn get_register(&self) -> Option<Register> {
        if let VarStorage::Register(reg) = self.inner.storage {
            Some(reg)
        } else {
            None
        }
    }

    /// If this [`Var`] is represented as a stack variable, returns the (positive) offset relative to [`FP`][Register::FP], otherwise returns `None`.
    pub fn get_stack_offset(&self) -> Option<u16> {
        if let VarStorage::StackOffset(off) = self.inner.storage {
            Some(off)
        } else {
            None
        }
    }

    /// If this [`Var`] is represented as an immediate value, returns that value, otherwise returns `None`.
    pub fn get_immediate(&self) -> Option<&Immediate> {
        if let VarStorage::Immediate(imm) = &self.inner.storage {
            Some(imm)
        } else {
            None
        }
    }
}
