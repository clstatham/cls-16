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
        let rhs = self.dfs_walk(rhs)?.unwrap();
        let lhs = self.dfs_walk(lhs)?.unwrap();

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
                    Punctuator::Minus => {
                        let result = self.cga_sub(lhs, rhs.clone())?;
                        self.current_scope.retake(rhs);
                        Ok(Some(result))
                    }
                    Punctuator::Star => {
                        let result = self.cga_mul(lhs, rhs.clone())?;
                        self.current_scope.retake(rhs);
                        Ok(Some(result))
                    }
                    Punctuator::FSlash => {
                        let result = self.cga_div(lhs, rhs.clone())?;
                        self.current_scope.retake(rhs);
                        Ok(Some(result))
                    }
                    Punctuator::PlusEq => {
                        let result = self.cga_add(lhs.clone(), rhs.clone())?;
                        self.current_scope.retake(rhs);
                        self.cga_store(lhs.clone(), result.clone())?;
                        self.current_scope.retake(result);
                        Ok(Some(lhs))
                    }
                    Punctuator::MinusEq => {
                        let result = self.cga_sub(lhs.clone(), rhs.clone())?;
                        self.current_scope.retake(rhs);
                        self.cga_store(lhs.clone(), result.clone())?;
                        self.current_scope.retake(result);
                        Ok(Some(lhs))
                    }
                    Punctuator::StarEq => {
                        let result = self.cga_mul(lhs.clone(), rhs.clone())?;
                        self.current_scope.retake(rhs);
                        self.cga_store(lhs.clone(), result.clone())?;
                        self.current_scope.retake(result);
                        Ok(Some(lhs))
                    }
                    Punctuator::SlashEq => {
                        let result = self.cga_div(lhs.clone(), rhs.clone())?;
                        self.current_scope.retake(rhs);
                        self.cga_store(lhs.clone(), result.clone())?;
                        self.current_scope.retake(result);
                        Ok(Some(lhs))
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
