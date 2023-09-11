use crate::{asm::WithSpan, c::compiler::*};

impl CompilerState {
    pub(crate) fn compile_expr_ident(
        &mut self,
        id: &WithSpan<'_, Ident<'_>>,
        dest: Option<&Var>,
        func_name: &str,
    ) -> Result<()> {
        if let Some(dest) = dest {
            let func = self.functions.get_mut(func_name).unwrap();
            let src = func.get(id.item.0).unwrap();

            match (dest.storage(), src.storage()) {
                (VarStorage::StackOffset(dest_off), VarStorage::StackOffset(src_off)) => {
                    let tmp_reg = func.any_reg().unwrap();
                    {
                        let block = func.last_block_mut();
                        load_fp_offset_to_reg(tmp_reg, *src_off, block);
                        store_reg_to_fp_offset(*dest_off, tmp_reg, block);
                    }
                    func.take_back_reg(tmp_reg);
                }
                (VarStorage::Register(dest), VarStorage::StackOffset(src_off)) => {
                    let block = func.last_block_mut();
                    load_fp_offset_to_reg(*dest, *src_off, block);
                }

                st => todo!("{:?}", st),
            }
        }
        Ok(())
    }
}
