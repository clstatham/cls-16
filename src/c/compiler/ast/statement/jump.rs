use crate::c::compiler::*;

impl CompilerState {
    pub(crate) fn compile_jump_statement(
        &mut self,
        stmt: &JumpStatement<'_>,
        func_name: &str,
    ) -> Result<()> {
        match stmt {
            JumpStatement::Goto(_label) => todo!(),
            JumpStatement::Return(value) => {
                if let Some(expr) = value.as_ref().map(|v| &v.item) {
                    let dest = Var::new(
                        self.functions.get(func_name).unwrap().return_type.clone(),
                        "returnval",
                        var::VarStorage::Register(Register::R1),
                    );
                    self.compile_expr(expr, Some(&dest), func_name)?;
                }
            }
        }
        Ok(())
    }
}
