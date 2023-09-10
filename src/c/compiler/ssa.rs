use std::rc::Rc;

use crate::{
    c::parser::ast::TypeSpecifier,
    plat::{Immediate, Register},
};

#[derive(Debug)]
pub enum Storage {
    Immediate(Immediate),
    Register(Register),
    RegOffset(u16, Register),
}

pub struct SsaInner {
    pub typ: TypeSpecifier,
    pub name: String,
    pub storage: Storage,
}

#[derive(Clone)]
pub struct Ssa {
    pub inner: Rc<SsaInner>,
}

impl Ssa {
    pub fn new(typ: TypeSpecifier, name: &str, storage: Storage) -> Self {
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

    pub fn get_immediate(&self) -> Option<&Immediate> {
        if let Storage::Immediate(imm) = &self.inner.storage {
            Some(imm)
        } else {
            None
        }
    }
}
