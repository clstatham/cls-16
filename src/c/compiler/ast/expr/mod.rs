use crate::c::compiler::*;

pub mod constant;
pub mod ident;
pub mod infix;
pub mod postfix;
pub mod unary;

impl CompilerState {
    pub(crate) fn compile_expr(
        &mut self,
        expr: &Expr<'_>,
        dest: Option<&Var>,
        func_name: &str,
    ) -> Result<()> {
        match expr {
            Expr::Ident(id) => {
                self.compile_expr_ident(id, dest, func_name)
                    .context("compiling ident expr")?;
            }
            Expr::Constant(val) => {
                self.compile_expr_constant(val, dest, func_name)
                    .context("compiling constant expr")?;
            }
            Expr::Unary(unary) => {
                self.compile_expr_unary(unary, dest, func_name)
                    .context("compiling unary expr")?;
            }
            Expr::Postfix(post) => match &post.item {
                PostfixExpr::Call(call) => {
                    self.compile_expr_postfix_call(call, dest, func_name)
                        .context("compiling call expr")?;
                }
                PostfixExpr::Index(index) => {
                    self.compile_expr_postfix_index(index, dest, func_name)
                        .context("compiling index expr")?;
                }
            },
            Expr::Infix(infix) => {
                self.compile_expr_infix(infix, dest, func_name)
                    .context("compiling infix expr")?;
            }
        }
        Ok(())
    }
}
