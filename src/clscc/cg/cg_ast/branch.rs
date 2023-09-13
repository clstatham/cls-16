use std::rc::Rc;

use anyhow::Result;

use crate::clscc::{
    cg::{Codegen, Scope, Value},
    parser::AstNode,
};

impl Codegen {
    pub(crate) fn cg_if_else(
        &mut self,
        cond: Value,
        then: &mut AstNode<'_>,
        els: &mut Option<AstNode<'_>>,
    ) -> Result<Option<Value>> {
        self.cga_if_else(cond, then, els)?;
        Ok(None)
    }

    pub(crate) fn cg_while(
        &mut self,
        cond: &mut AstNode<'_>,
        body: &mut AstNode<'_>,
    ) -> Result<Option<Value>> {
        self.cga_while(cond, body)?;

        Ok(None)
    }
}
