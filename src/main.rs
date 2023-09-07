//! The CLS-16 will be a made-up hobby 16-bit CPU architecture with its own assembler, emulator, and C compiler.
//! It was created by Connor Statham, also known as C-STATE on youtube or clstatham on github.
//! CLS-16 exists because Connor's other CPU project, K4S, ended up being too ambitious and messy.
//! The focus for CLS-16 will be on building solid core functionality with cleaner code, more documentation, and generally better semantics.

pub mod asm;
pub mod comp;
pub mod emu;
pub mod spec;

fn main() {
    println!("Hello, CLS-16!");
}
