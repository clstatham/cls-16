use anyhow::Result;

use crate::clscc::{
    cg::{Codegen, Value},
    parser::AstNode,
};

impl Codegen {
    pub(crate) fn cg_if_else(
        &mut self,
        cond: Value,
        then: &AstNode<'_>,
        els: &Option<AstNode<'_>>,
        parent: Option<&AstNode<'_>>,
    ) -> Result<Option<Value>> {
        self.cga_if_else(cond, then, els, parent)?;
        Ok(None)
    }

    pub(crate) fn cg_while(
        &mut self,
        cond: &AstNode<'_>,
        body: &AstNode<'_>,
        parent: Option<&AstNode<'_>>,
    ) -> Result<Option<Value>> {
        self.cga_while(cond, body, parent)?;

        Ok(None)
    }

    pub(crate) fn cg_for(
        &mut self,
        init: &AstNode<'_>,
        cond: &AstNode<'_>,
        step: &AstNode<'_>,
        body: &AstNode<'_>,
        parent: Option<&AstNode<'_>>,
    ) -> Result<Option<Value>> {
        self.cga_for(init, cond, step, body, parent)?;

        Ok(None)
    }
}
