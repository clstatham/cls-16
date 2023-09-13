use anyhow::{Error, Result};

use crate::clscc::{
    cg::{Codegen, CodegenError, CodegenErrorKind, Value, ValueStorage},
    common::{Punctuator, TokenVariant},
    parser::{Ast, AstNode, Type},
};

impl Codegen {
    pub(crate) fn cg_binary(
        &mut self,
        op: &AstNode<'_>,
        lhs: &AstNode<'_>,
        rhs: &AstNode<'_>,
        parent: Option<&AstNode<'_>>,
    ) -> Result<Option<Value>> {
        self.push_scope("binary".to_string());
        // self.push_scope("lhs".to_string());
        let lhs = self.dfs_walk(lhs, parent, false)?.unwrap();
        // self.pop_scope();
        // self.push_scope("rhs".to_string());
        let rhs = self.dfs_walk(rhs, parent, true)?.unwrap();

        // self.pop_scope();

        let res = if let Ast::Token(op) = &*op.ast {
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
                        let result = self.current_scope.any_register(lhs.ty().cloned())?;
                        let result = self.cga_add(result, lhs, rhs.clone())?;
                        self.current_scope.retake(rhs);
                        Ok(Some(result))
                    }
                    Punctuator::Minus => {
                        let result = self.current_scope.any_register(lhs.ty().cloned())?;
                        let result = self.cga_sub(result, lhs, rhs.clone())?;
                        self.current_scope.retake(rhs);
                        Ok(Some(result))
                    }
                    Punctuator::Star => {
                        let result = self.current_scope.any_register(lhs.ty().cloned())?;
                        let result = self.cga_mul(result, lhs, rhs.clone())?;
                        self.current_scope.retake(rhs);
                        Ok(Some(result))
                    }
                    Punctuator::EqEq => {
                        let result = self.current_scope.any_register(Some(Type::Int))?;
                        let result = self.cga_eq(result, lhs, rhs.clone())?;
                        self.current_scope.retake(rhs);
                        Ok(Some(result))
                    }
                    Punctuator::BangEq => {
                        let result = self.current_scope.any_register(Some(Type::Int))?;
                        let result = self.cga_neq(result, lhs, rhs.clone())?;
                        self.current_scope.retake(rhs);
                        Ok(Some(result))
                    }
                    Punctuator::Lt => {
                        let result = self.current_scope.any_register(Some(Type::Int))?;
                        let result = self.cga_lt(result, lhs, rhs.clone())?;
                        self.current_scope.retake(rhs);
                        Ok(Some(result))
                    }
                    Punctuator::LtEq => {
                        let result = self.current_scope.any_register(Some(Type::Int))?;
                        let result = self.cga_lte(result, lhs, rhs.clone())?;
                        self.current_scope.retake(rhs);
                        Ok(Some(result))
                    }
                    Punctuator::Gt => {
                        let result = self.current_scope.any_register(Some(Type::Int))?;
                        let result = self.cga_gt(result, lhs, rhs.clone())?;
                        self.current_scope.retake(rhs);
                        Ok(Some(result))
                    }
                    Punctuator::GtEq => {
                        let result = self.current_scope.any_register(Some(Type::Int))?;
                        let result = self.cga_gte(result, lhs, rhs.clone())?;
                        self.current_scope.retake(rhs);
                        Ok(Some(result))
                    }
                    Punctuator::FSlash => {
                        let result = self.current_scope.any_register(lhs.ty().cloned())?;
                        let result = self.cga_div(result, lhs, rhs.clone())?;
                        self.current_scope.retake(rhs);
                        Ok(Some(result))
                    }
                    Punctuator::PlusEq => {
                        let result = self.current_scope.any_register(lhs.ty().cloned())?;
                        let result = self.cga_add(result.clone(), lhs.clone(), rhs.clone())?;
                        self.current_scope.retake(rhs);
                        self.cga_store(lhs.clone(), result.clone())?;
                        self.current_scope.retake(result);
                        Ok(Some(lhs))
                    }
                    Punctuator::MinusEq => {
                        let result = self.current_scope.any_register(lhs.ty().cloned())?;
                        let result = self.cga_sub(result.clone(), lhs.clone(), rhs.clone())?;
                        self.current_scope.retake(rhs);
                        self.cga_store(lhs.clone(), result.clone())?;
                        self.current_scope.retake(result);
                        Ok(Some(lhs))
                    }
                    Punctuator::StarEq => {
                        let result = self.current_scope.any_register(lhs.ty().cloned())?;
                        let result = self.cga_mul(result.clone(), lhs.clone(), rhs.clone())?;
                        self.current_scope.retake(rhs);
                        self.cga_store(lhs.clone(), result.clone())?;
                        self.current_scope.retake(result);
                        Ok(Some(lhs))
                    }
                    Punctuator::SlashEq => {
                        let result = self.current_scope.any_register(lhs.ty().cloned())?;
                        let result = self.cga_div(result.clone(), lhs.clone(), rhs.clone())?;
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
        };
        self.pop_scope();
        res
    }
}
