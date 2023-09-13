use anyhow::Result;

use crate::clscc::cg::{Codegen, Value, ValueStorage};

impl Codegen {
    pub fn cg_call(&mut self, name: Value, args: Vec<Value>) -> Result<Option<Value>> {
        let res =
            if let ValueStorage::Immediate(cls16::Immediate::Unlinked(str_name)) = name.storage() {
                if str_name == "printi" {
                    self.cga_printi(args[0].clone())
                } else {
                    self.cga_call(name, args.clone())
                }
            } else {
                self.cga_call(name, args.clone())
            };
        for arg in args {
            self.current_scope.retake(arg);
        }
        res
    }
}
