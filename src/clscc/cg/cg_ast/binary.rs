use anyhow::{Error, Result};

use crate::clscc::{
    cg::{Codegen, CodegenError, CodegenErrorKind, Value, ValueStorage},
    common::{Punctuator, TokenVariant},
    parser::{Ast, AstNode, Type},
};

impl Codegen {
    pub(crate) fn cg_binary(
        &mut self,
        op: &mut AstNode<'_>,
        lhs: &mut AstNode<'_>,
        rhs: &mut AstNode<'_>,
    ) -> Result<Option<Value>> {
        // assert!(!lhs.rvalue, "parser incorrectly marked lhs as rvalue");
        // lhs.rvalue = false;
        // rhs.rvalue = true;
        // assert!(rhs.rvalue, "parser incorrectly marked rhs as lvalue");
        let lhs = self.dfs_walk(lhs, false)?.unwrap();
        let rhs = self.dfs_walk(rhs, true)?.unwrap();
        assert!(rhs.rvalue());
        assert!(!lhs.rvalue());

        if let Ast::Token(op) = &*op.ast {
            if let TokenVariant::Punctuator(op) = op.variant {
                match op {
                    Punctuator::Equals => {
                        let result = self.cga_store(lhs.clone(), rhs.clone())?;
                        self.current_scope.retake(rhs);
                        if matches!(lhs.storage(), ValueStorage::Register(_))
                            && matches!(lhs.ty(), Some(Type::Pointer(_)))
                        {
                            // free the register used for the temporary pointer
                            self.current_scope.retake(lhs);
                        }
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
