//! The emulator/debugger module for CLS-16.

use thiserror::Error;

pub mod alu;
pub mod emulator;
pub mod microcode;
pub mod ram;
pub mod registers;

#[derive(Debug, Error)]
pub enum EmuError {
    #[error("invalid register")]
    InvalidRegister,
}
