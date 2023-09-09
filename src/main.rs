#![cfg_attr(doc, warn(missing_docs))]
#![doc = include_str!("../README.md")]

use anyhow::Result;
use asm::Assembler;
use emu::emulator::Emulator;

use crate::c::compiler::compile;

pub mod asm;
pub mod c;
pub mod emu;
pub mod plat;

fn main() -> Result<()> {
    env_logger::init();
    #[cfg(debug_assertions)]
    log::set_max_level(log::LevelFilter::Trace);

    let c_program = include_str!("../ctest.c");
    let program = compile(c_program)?;

    // let program = include_str!("../test.s");
    let mut asm = Assembler::default();
    let bin = asm.assemble(&program)?;
    let mut emu = Emulator::new(&bin, 24000.0)?;
    emu.run_while_continue()?;
    Ok(())
}
