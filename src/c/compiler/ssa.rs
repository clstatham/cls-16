use std::rc::Rc;

use crate::{
    c::parser::ast::TypeSpecifier,
    plat::{Immediate, Register},
};

pub enum Storage<'a> {
    Immediate(Immediate<'a>),
    Register(Register),
    RegOffset(u16, Register),
}

pub struct SsaInner<'a> {
    pub typ: TypeSpecifier,
    pub name: String,
    pub storage: Storage<'a>,
}

#[derive(Clone)]
pub struct Ssa<'a> {
    pub inner: Rc<SsaInner<'a>>,
}

impl<'a> Ssa<'a> {
    pub fn new(typ: TypeSpecifier, name: &str, storage: Storage<'a>) -> Self {
        Self {
            inner: Rc::new(SsaInner {
                typ,
                name: name.to_owned(),
                storage,
            }),
        }
    }

    pub fn name(&self) -> &str {
        &self.inner.name
    }

    pub fn typ(&self) -> &TypeSpecifier {
        &self.inner.typ
    }

    pub fn storage(&self) -> &Storage {
        &self.inner.storage
    }

    pub fn get_register(&self) -> Option<Register> {
        if let Storage::Register(reg) = self.inner.storage {
            Some(reg)
        } else {
            None
        }
    }

    pub fn get_reg_offset(&self) -> Option<(u16, Register)> {
        if let Storage::RegOffset(off, reg) = self.inner.storage {
            Some((off, reg))
        } else {
            None
        }
    }

    pub fn get_immediate(&self) -> Option<Immediate<'_>> {
        if let Storage::Immediate(imm) = self.inner.storage {
            Some(imm)
        } else {
            None
        }
    }
}
