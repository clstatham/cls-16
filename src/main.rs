#![doc = include_str!("../README.md")]

pub mod asm;
pub mod comp;
pub mod emu;
pub mod plat;

fn main() {
    env_logger::init();
    #[cfg(debug_assertions)]
    log::set_max_level(log::LevelFilter::Trace);
}
