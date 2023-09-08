#![cfg_attr(doc, warn(missing_docs))]
#![doc = include_str!("../README.md")]

use anyhow::Result;
use asm::Assembler;
use emu::emulator::Emulator;

pub mod asm;
pub mod comp;
pub mod emu;
pub mod plat;

fn main() -> Result<()> {
    env_logger::init();
    #[cfg(debug_assertions)]
    log::set_max_level(log::LevelFilter::Trace);

    let program = include_str!("../examples/test1.s");
    let mut asm = Assembler::default();
    let bin = asm.assemble(program)?;
    let mut emu = Emulator::new(&bin, 20.0)?;
    emu.run_until_halt()?;
    Ok(())
}
