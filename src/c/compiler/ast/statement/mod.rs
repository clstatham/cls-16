use crate::c::compiler::*;

pub mod builtin;
pub mod iteration;
pub mod jump;
pub mod selection;

impl CompilerState {
    pub(crate) fn compile_statement(
        &mut self,
        stmt: &Statement<'_>,
        func_name: &str,
    ) -> Result<()> {
        match stmt {
            Statement::Builtin(stmt) => self
                .compile_builtin_statement(&stmt.item, func_name)
                .context("compiling statement"),
            Statement::Labeled(_stmt) => todo!(),
            Statement::Compound(stmt) => {
                for block_item in stmt.item.0.iter() {
                    self.compile_block_item(&block_item.item, func_name)?;
                }
                Ok(())
            }
            Statement::Selection(stmt) => self
                .compile_selection_statement(&stmt.item, func_name)
                .context("compiling statement"),
            Statement::Iteration(stmt) => self
                .compile_iteration_statement(&stmt.item, func_name)
                .context("compiling statement"),
            Statement::Jump(stmt) => self
                .compile_jump_statement(&stmt.item, func_name)
                .context("compiling statement"),
            Statement::Expr(expr) => self
                .compile_expr(&expr.item, None, func_name)
                .context("compiling statement"),
        }
    }
}
