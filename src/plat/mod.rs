//! Common platform code between CLS-16's other modules.

use anyhow::{Error, Result};
use thiserror::Error;

/// An error for the core platform of CLS-16.
#[derive(Debug, Error)]
pub enum PlatformError {
    #[error("invalid opcode")]
    InvalidOpcode,
    #[error("invalid instruction")]
    InvalidInstruction,
    #[error("undefined reference to {0}")]
    UndefinedReference(String),
}

/// The twelve registers in CLS-16.
///
/// `R1` - `R8` are general purpose registers. `PC`, `SP`, `FL`, and `R0` are special.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Register {
    /// Zero register
    R0 = 0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    /// Program counter
    PC,
    /// Stack pointer
    SP,
    /// Status flags
    FL,
}

impl TryFrom<u8> for Register {
    type Error = PlatformError;

    fn try_from(value: u8) -> Result<Self, PlatformError> {
        match value {
            v if v == Self::R0 as u8 => Ok(Self::R0),
            v if v == Self::R1 as u8 => Ok(Self::R1),
            v if v == Self::R2 as u8 => Ok(Self::R2),
            v if v == Self::R3 as u8 => Ok(Self::R3),
            v if v == Self::R4 as u8 => Ok(Self::R4),
            v if v == Self::R5 as u8 => Ok(Self::R5),
            v if v == Self::R6 as u8 => Ok(Self::R6),
            v if v == Self::R7 as u8 => Ok(Self::R7),
            v if v == Self::R8 as u8 => Ok(Self::R8),
            v if v == Self::PC as u8 => Ok(Self::PC),
            v if v == Self::SP as u8 => Ok(Self::SP),
            v if v == Self::FL as u8 => Ok(Self::FL),
            _ => Err(PlatformError::InvalidOpcode),
        }
    }
}

/// The various opcodes that can be used in instructions.
///
/// Note that these are the most *basic* operations that the CPU can execute in a single instruction cycle.
/// More complex operations are used in the actual assembly syntax, that are compiled down into combinations of these basic operations.
///
/// ALU opcode notes ([ADD][Opcode::Add], [ADDI][Opcode::Addi], [SUB][Opcode::Sub], [SUBI][Opcode::Subi], [AND][Opcode::And], [OR][Opcode::Or], [NOT][Opcode::Not], [SHL][Opcode::Shl], [SHR][Opcode::Shr]):
///
/// - [FL](Register::FL) Overflow bit is set to 1 if the operation over/underflows, otherwise it is set to 0.
/// - [FL](Register::FL) Parity bit is set to 1 if the result has odd parity, otherwise it is set to 0.
/// - [FL](Register::FL) Zero bit is set to 1 if the result is zero, otherwise it is set to 0.
/// - When [R0](Register::R0) is used as a destination, the result is discarded, but **the [FL](Register::FL) bits are still set.**
///
/// Memory and branching opcode notes ([STL][Opcode::Stl], [STH][Opcode::Sth], [LDL][Opcode::Ldl], [LDH][Opcode::Ldh], [LDI][Opcode::Ldi], [JZ][Opcode::Jz]):
///
/// - The memory addresses are given by a register.
/// - `xxL` will operate on the lower 8 bits of the relevant register.
/// - `xxH` will operate on the higher 8 bits of the relevant register.
/// - `LDI` will load/store 16-bit immediate values into the whole register, setting the other bits to 0.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Opcode {
    /* Halt/Nop */
    /// Stops the clock
    Halt = 0,
    /// Does nothing for the instruction cycle
    Nop,

    /* ALU */
    /// `regA <- regB + regC`
    Add,
    /// `regA <- regA + (immediate value)`
    Addi,
    /// `regA <- regB - regC`
    Sub,
    /// `regA <- regA - (immediate value)`
    Subi,
    /// `regA <- regB & regC`
    And,
    /// `regA <- regB | regC`
    Or,
    /// `regA <- regB ^ regC`
    Xor,
    /// `regA <- ~regB`
    Not,
    /// `regA <- regB << regC`
    Shl,
    /// `regA <- regB >> regC`
    Shr,

    /* Memory */
    /// `mem[regA] <- regB.LO`
    Stl,
    /// `mem[regA] <- regB.HI`
    Sth,
    /// `regA.LO <- mem[regB]`
    Ldl,
    /// `regA.HI <- mem[regB]`
    Ldh,
    /// `regA <- (immediate value)`
    Ldi,

    /* Branching */
    /// "Jump if Zero"
    ///```text
    /// if FL.Zero == 1 {
    ///     PC <- regA
    /// }
    /// ```
    Jz,

    /* Debugging */
    /// Prints the value in `regA` to the debug monitor of the emulator.
    Printi,
    /// Interprets the value in `regA.LO` as an ASCII character and prints it to the debug monitor of the emulator.
    Printc,
}

impl TryFrom<u8> for Opcode {
    type Error = PlatformError;

    fn try_from(value: u8) -> Result<Self, PlatformError> {
        match value {
            v if v == Self::Halt as u8 => Ok(Self::Halt),
            v if v == Self::Nop as u8 => Ok(Self::Nop),
            v if v == Self::Add as u8 => Ok(Self::Add),
            v if v == Self::Addi as u8 => Ok(Self::Addi),
            v if v == Self::Sub as u8 => Ok(Self::Sub),
            v if v == Self::Subi as u8 => Ok(Self::Subi),
            v if v == Self::And as u8 => Ok(Self::And),
            v if v == Self::Or as u8 => Ok(Self::Or),
            v if v == Self::Xor as u8 => Ok(Self::Xor),
            v if v == Self::Not as u8 => Ok(Self::Not),
            v if v == Self::Shl as u8 => Ok(Self::Shl),
            v if v == Self::Shr as u8 => Ok(Self::Shr),
            v if v == Self::Stl as u8 => Ok(Self::Stl),
            v if v == Self::Sth as u8 => Ok(Self::Sth),
            v if v == Self::Ldl as u8 => Ok(Self::Ldl),
            v if v == Self::Ldh as u8 => Ok(Self::Ldh),
            v if v == Self::Ldi as u8 => Ok(Self::Ldi),
            v if v == Self::Jz as u8 => Ok(Self::Jz),
            v if v == Self::Printi as u8 => Ok(Self::Printi),
            v if v == Self::Printc as u8 => Ok(Self::Printc),

            _ => Err(PlatformError::InvalidOpcode),
        }
    }
}

/// An immediate value, either linked (with a raw value) or unlinked (with the symbol name).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Immediate<'a> {
    Linked(u16),
    Unlinked(&'a str),
}

impl<'a> Immediate<'a> {
    pub const fn get_linked(self) -> Option<u16> {
        if let Self::Linked(val) = self {
            Some(val)
        } else {
            None
        }
    }
}

/// Instruction formats indicating how instructions are packed into the 32-bit instruction words in CLS-16.
///
/// Each instruction can be broken down into 4 bytes. The first is always the opcode.
/// The following three are specified by one of this enum's variants.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InstrFormat<'a> {
    /// `<Opcode, Register, Register, Register>`
    RRR(Register, Register, Register),
    /// `<Opcode, Register, ImmediateHi, ImmediateLo>`
    RI(Register, Immediate<'a>),
    /// `<Opcode, ZEROS, Register, Register>`
    RR(Register, Register),
    /// `<Opcode, ZEROS, ZEROS, Register>`
    R(Register),
    /// `<Opcode, ZEROS, ImmediateHi, ImmediateLo>`
    I(Immediate<'a>),
    /// `<Opcode, ZEROS, ZEROS, ZEROS>`
    OpOnly,
}

/// A full 32-bit instruction word in CLS-16.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Instruction<'a> {
    pub op: Opcode,
    pub format: InstrFormat<'a>,
}

impl<'a> Instruction<'a> {
    /// Checks if this instruction has a valid format for its opcode.
    ///
    /// # Errors
    ///
    /// This function will return an error if the instruction's format is invalid for its opcode.
    pub fn validate(self) -> Result<()> {
        #[doc(hidden)]
        macro_rules! assert_format {
            ($fmt:pat) => {
                if matches!(self.format, $fmt) {
                    Ok(())
                } else {
                    Err(PlatformError::InvalidInstruction.into())
                }
            };
        }
        match self.op {
            Opcode::Halt => assert_format!(InstrFormat::OpOnly),
            Opcode::Nop => assert_format!(InstrFormat::OpOnly),
            Opcode::Add => assert_format!(InstrFormat::RRR(_, _, _)),
            Opcode::Addi => assert_format!(InstrFormat::RI(_, _)),
            Opcode::Sub => assert_format!(InstrFormat::RRR(_, _, _)),
            Opcode::Subi => assert_format!(InstrFormat::RI(_, _)),
            Opcode::And => assert_format!(InstrFormat::RRR(_, _, _)),
            Opcode::Or => assert_format!(InstrFormat::RRR(_, _, _)),
            Opcode::Xor => assert_format!(InstrFormat::RRR(_, _, _)),
            Opcode::Not => assert_format!(InstrFormat::RR(_, _)),
            Opcode::Shl => assert_format!(InstrFormat::RRR(_, _, _)),
            Opcode::Shr => assert_format!(InstrFormat::RRR(_, _, _)),
            Opcode::Stl => assert_format!(InstrFormat::RR(_, _)),
            Opcode::Sth => assert_format!(InstrFormat::RR(_, _)),
            Opcode::Ldl => assert_format!(InstrFormat::RR(_, _)),
            Opcode::Ldh => assert_format!(InstrFormat::RR(_, _)),
            Opcode::Ldi => assert_format!(InstrFormat::RI(_, _)),
            Opcode::Jz => assert_format!(InstrFormat::R(_)),
            Opcode::Printi => assert_format!(InstrFormat::R(_)),
            Opcode::Printc => assert_format!(InstrFormat::R(_)),
        }
    }

    /// Generates the corresponding machine-code instruction word for this instruction.
    ///
    /// # Errors
    ///
    /// This function will return an error if the instruction's format is invalid for its opcode.
    pub fn to_bytes(self) -> Result<[u8; 4]> {
        self.validate()?;
        let op = self.op as u8;
        let format = match self.format {
            InstrFormat::RRR(a, b, c) => [a as u8, b as u8, c as u8],
            InstrFormat::RI(a, imm) => {
                let imm = match imm {
                    Immediate::Linked(imm) => imm,
                    Immediate::Unlinked(name) => {
                        return Err(Error::from(PlatformError::UndefinedReference(
                            name.to_string(),
                        )))
                    }
                };
                [a as u8, imm.to_le_bytes()[0], imm.to_le_bytes()[1]]
            }
            InstrFormat::RR(a, b) => [0u8, a as u8, b as u8],
            InstrFormat::R(a) => [0u8, 0u8, a as u8],
            InstrFormat::I(imm) => {
                let imm = match imm {
                    Immediate::Linked(imm) => imm,
                    Immediate::Unlinked(name) => {
                        return Err(Error::from(PlatformError::UndefinedReference(
                            name.to_string(),
                        )))
                    }
                };
                [0u8, imm.to_le_bytes()[0], imm.to_le_bytes()[1]]
            }
            InstrFormat::OpOnly => [0u8, 0u8, 0u8],
        };
        Ok([op, format[0], format[1], format[2]])
    }

    /// Generates an [Instruction] instance from the given instruction word.
    ///
    /// # Errors
    ///
    /// This function will return an error if the given word is not a valid instruction.
    pub fn from_bytes(bytes: [u8; 4]) -> Result<Self> {
        let op: Opcode = bytes[0].try_into()?;
        let format = match op {
            Opcode::Nop => InstrFormat::OpOnly,
            Opcode::Halt => InstrFormat::OpOnly,
            Opcode::Add => InstrFormat::RRR(
                bytes[1].try_into()?,
                bytes[2].try_into()?,
                bytes[3].try_into()?,
            ),
            Opcode::Sub => InstrFormat::RRR(
                bytes[1].try_into()?,
                bytes[2].try_into()?,
                bytes[3].try_into()?,
            ),
            Opcode::And => InstrFormat::RRR(
                bytes[1].try_into()?,
                bytes[2].try_into()?,
                bytes[3].try_into()?,
            ),
            Opcode::Or => InstrFormat::RRR(
                bytes[1].try_into()?,
                bytes[2].try_into()?,
                bytes[3].try_into()?,
            ),
            Opcode::Xor => InstrFormat::RRR(
                bytes[1].try_into()?,
                bytes[2].try_into()?,
                bytes[3].try_into()?,
            ),
            Opcode::Not => InstrFormat::RR(bytes[2].try_into()?, bytes[3].try_into()?),
            Opcode::Shl => InstrFormat::RRR(
                bytes[1].try_into()?,
                bytes[2].try_into()?,
                bytes[3].try_into()?,
            ),
            Opcode::Shr => InstrFormat::RRR(
                bytes[1].try_into()?,
                bytes[2].try_into()?,
                bytes[3].try_into()?,
            ),
            Opcode::Stl => InstrFormat::RR(bytes[2].try_into()?, bytes[3].try_into()?),
            Opcode::Sth => InstrFormat::RR(bytes[2].try_into()?, bytes[3].try_into()?),
            Opcode::Ldl => InstrFormat::RR(bytes[2].try_into()?, bytes[3].try_into()?),
            Opcode::Ldh => InstrFormat::RR(bytes[2].try_into()?, bytes[3].try_into()?),
            Opcode::Ldi => InstrFormat::RI(
                bytes[1].try_into()?,
                Immediate::Linked(u16::from_le_bytes([bytes[2], bytes[3]])),
            ),
            Opcode::Addi => InstrFormat::RI(
                bytes[1].try_into()?,
                Immediate::Linked(u16::from_le_bytes([bytes[2], bytes[3]])),
            ),
            Opcode::Subi => InstrFormat::RI(
                bytes[1].try_into()?,
                Immediate::Linked(u16::from_le_bytes([bytes[2], bytes[3]])),
            ),
            Opcode::Jz => InstrFormat::R(bytes[3].try_into()?),
            Opcode::Printi => InstrFormat::R(bytes[3].try_into()?),
            Opcode::Printc => InstrFormat::R(bytes[3].try_into()?),
        };

        let this = Self { op, format };
        this.validate()?;
        Ok(this)
    }
}
