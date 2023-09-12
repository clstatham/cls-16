use anyhow::{Error, Result};

use crate::clscc::{
    cg::{Codegen, CodegenError, CodegenErrorKind, Value, ValueStorage},
    common::{Punctuator, TokenVariant},
    parser::{Ast, AstNode},
};

impl Codegen {
    pub fn cg_call(&mut self, name: Value, args: Vec<Value>) -> Result<Option<Value>> {
        if let ValueStorage::Immediate(cls16::Immediate::Unlinked(str_name)) = name.storage() {
            if str_name == "printi" {
                self.cga_printi(args[0].clone())
            } else {
                self.cga_call(name, args)
            }
        } else {
            self.cga_call(name, args)
        }
    }
}