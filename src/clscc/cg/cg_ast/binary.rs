use anyhow::{Error, Result};

use crate::clscc::{
    cg::{Codegen, CodegenError, CodegenErrorKind, Value},
    common::{Punctuator, TokenVariant},
    parser::{Ast, AstNode},
};

impl Codegen {
    pub(crate) fn cg_binary(
        &mut self,
        op: &mut AstNode<'_>,
        lhs: &mut AstNode<'_>,
        rhs: &mut AstNode<'_>,
    ) -> Result<Option<Value>> {
        assert!(!lhs.rvalue, "parser incorrectly marked lhs as rvalue");
        assert!(rhs.rvalue, "parser incorrectly marked rhs as lvalue");
        let lhs = self.dfs_walk(lhs)?.unwrap();
        let rhs = self.dfs_walk(rhs)?.unwrap();
        if let Ast::Token(op) = &*op.ast {
            if let TokenVariant::Punctuator(op) = op.variant {
                match op {
                    Punctuator::Equals => {
                        let result = self.cga_store(lhs, rhs.clone())?;
                        self.current_scope.retake(rhs);
                        Ok(Some(result))
                    }
                    Punctuator::Plus => {
                        let result = self.cga_add(lhs, rhs.clone())?;
                        self.current_scope.retake(rhs);
                        Ok(Some(result))
                    }
                    punc => todo!("{:?}", punc),
                }
            } else {
                Err(Error::new(CodegenError::new(CodegenErrorKind::Expected {
                    expected: vec!["punctuator".to_string()],
                    got: format!("{:?}", op.string_repr),
                })))
            }
        } else {
            Err(Error::new(CodegenError::new(CodegenErrorKind::Expected {
                expected: vec!["punctuator".to_string()],
                got: format!("{:?}", op.ast),
            })))
        }
    }
}
