use anyhow::Result;

use crate::clscc::cg::{Codegen, Value};

impl Codegen {
    pub(crate) fn cg_index(
        &mut self,
        arr: Value,
        index: Value,
        rvalue: bool,
    ) -> Result<Option<Value>> {
        let res = self.cga_index(arr.clone(), index.clone(), rvalue)?;
        self.current_scope.retake(index);
        Ok(Some(res))
    }
}
