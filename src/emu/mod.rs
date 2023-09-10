//! The emulator/debugger module for CLS-16.

use thiserror::Error;

use crate::plat::Register;

pub mod alu;
pub mod debugger;
pub mod emulator;
pub mod microcode;
pub mod ram;
pub mod registers;

#[derive(Debug, Error)]
pub enum EmuError {
    #[error("invalid register: {0}")]
    InvalidRegister(Register),
}
