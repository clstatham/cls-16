//! Common platform code between CLS-16's other modules.

use std::fmt::Display;

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

/// The registers in CLS-16.
///
/// `R1` - `R6`, `SP`, and `FP` are general purpose registers.
/// `R0` is special, as it is always zero.
/// `PC`, `IH`, `IL`, and `FL` are VERY special as they are able to be directly manipulated by the emulator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Register {
    /// Zero register.
    R0 = 0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    /// Stack pointer.
    SP,
    /// Frame pointer.
    FP,
    /// Program counter.
    PC,
    /// Low instruction register.
    IL,
    /// High instruction register.
    IH,
    /// Status flags.
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
            v if v == Self::SP as u8 => Ok(Self::SP),
            v if v == Self::FP as u8 => Ok(Self::FP),
            v if v == Self::PC as u8 => Ok(Self::PC),
            v if v == Self::IL as u8 => Ok(Self::IL),
            v if v == Self::IH as u8 => Ok(Self::IH),
            v if v == Self::FL as u8 => Ok(Self::FL),
            _ => Err(PlatformError::InvalidOpcode),
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // this is lazy, but it works for now
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
}

/// The various opcodes that can be used in instructions.
///
/// Note that these are the most *basic* operations that the CPU can execute in a single instruction cycle.
/// More complex operations are used in the actual assembly syntax, that are compiled down into combinations of these basic operations.
///
/// ALU opcode notes:
///
/// - [`FL`](Register::FL) Carry bit is set to 1 if the operation overflows, otherwise it is set to 0.
/// - [`FL`](Register::FL) Zero bit is set to 1 if the result is zero, otherwise it is set to 0.
/// - When [`R0`](Register::R0) is used as a destination, the result is discarded, but **the [`FL`](Register::FL) bits are still set.**
///
/// Memory and branching opcode notes:
///
/// - The memory addresses are given by a register.
/// - `xxL` will operate on the lower 8 bits of the relevant register.
/// - `xxH` will operate on the higher 8 bits of the relevant register.
/// - `LDI` will load/store 16-bit immediate values into the whole register (starting at the lowest bit), setting the other bits to 0.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Opcode {
    /* Halt/Nop */
    /// Stops all counters.
    Halt = 0,
    /// Does nothing for the instruction cycle.
    Nop,

    /* Registers */
    /// `regA <- (immediate value)`
    Mov,
    /* ALU */
    /// `regA <- regB + regC`
    /// `regA <- regA + (immediate value)`
    Add,
    /// `regA <- regB - regC`
    /// `regA <- regA - (immediate value)`
    Sub,
    /// `regA <- regB * regC`
    /// `regA <- regA * (immediate value)`
    Mul,
    /// `regA <- regB / regC`
    /// `regA <- regA / (immediate value)`
    Div,
    /// `regA <- regB & regC`
    /// `regA <- regA & (immediate value)`
    And,
    /// `regA <- regB | regC`
    /// `regA <- regA | (immediate value)`
    Or,
    /// `regA <- regB ^ regC`
    /// `regA <- regA ^ (immediate value)`
    Xor,
    /// `regA <- ~regB`
    Not,

    /* Shift Unit */
    /// `regA <- regB << 1`
    Shl,
    /// `regA <- regB >> 1`
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

    /* Branching */
    /// "Jump if Zero"
    ///```text
    /// if FL.Zero == 1 {
    ///     PC <- regA
    /// }
    /// ```
    Jz,
    /// `PC <- regA`
    Jmp,

    /* Debugging */
    /// Prints the value in `regA` to the debug monitor of the emulator.
    Printi,
    /// Interprets the value in `regA.LO` as an ASCII character and prints it to the debug monitor of the emulator.
    Printc,
    /// Sets a debugging breakpoint.
    B,
}

impl TryFrom<u8> for Opcode {
    type Error = PlatformError;

    fn try_from(value: u8) -> Result<Self, PlatformError> {
        match value {
            v if v == Self::Halt as u8 => Ok(Self::Halt),
            v if v == Self::Nop as u8 => Ok(Self::Nop),
            v if v == Self::Add as u8 => Ok(Self::Add),
            v if v == Self::Sub as u8 => Ok(Self::Sub),
            v if v == Self::Mul as u8 => Ok(Self::Mul),
            v if v == Self::Div as u8 => Ok(Self::Div),
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
            v if v == Self::Mov as u8 => Ok(Self::Mov),
            v if v == Self::Jmp as u8 => Ok(Self::Jmp),
            v if v == Self::Jz as u8 => Ok(Self::Jz),
            v if v == Self::Printi as u8 => Ok(Self::Printi),
            v if v == Self::Printc as u8 => Ok(Self::Printc),
            v if v == Self::B as u8 => Ok(Self::B),

            _ => Err(PlatformError::InvalidOpcode),
        }
    }
}

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // this is lazy, but it works for now
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
}

/// An immediate value, either linked (with a raw value) or unlinked (with the symbol name).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Immediate {
    Linked(u16),
    Unlinked(String),
}

impl Immediate {
    pub const fn get_linked(&self) -> Option<u16> {
        if let Self::Linked(val) = self {
            Some(*val)
        } else {
            None
        }
    }
}

impl Display for Immediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Linked(val) => write!(f, "$0x{val:x}"),
            Self::Unlinked(name) => write!(f, "%{name}"),
        }
    }
}

/// Instruction formats indicating how instructions are packed into the 32-bit instruction words in CLS-16.
///
/// Each instruction can be broken down into 4 bytes. The first is always the opcode.
/// The following three are specified by one of this enum's variants.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InstrFormat {
    /// 000 Opcode, Register, Register, Immediate
    RRI(Register, Register, Immediate),
    /// 001 Opcode, Register, 0000, Immediate
    RI(Register, Immediate),
    /// 010 Opcode, Register, Register, 0000 0000 0000 0000
    RR(Register, Register),
    /// 011 Opcode, Register, 0000, 0000 0000 0000 0000
    R(Register),
    /// 100 Opcode, 0000, 0000, Immediate
    I(Immediate),
    /// 101 Opcode, 0000, 0000, 0000 0000 0000 0000
    OpOnly,
}

impl InstrFormat {
    /// Encodes the instruction format into the upper 3 bits of the returned byte.
    #[rustfmt::skip]
    pub const fn encode(&self) -> u8 {
        match self {
            Self::RRI(_, _, _) =>   0b00000000,
            Self::RI(_, _) =>       0b00100000,
            Self::RR(_, _) =>       0b01000000,
            Self::R(_) =>           0b01100000,
            Self::I(_) =>           0b10000000,
            Self::OpOnly =>         0b10100000,
        }
    }
}

impl Display for InstrFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RRI(a, b, imm) => write!(f, "{a} {b} {imm}"),
            Self::RI(a, imm) => write!(f, "{a} {imm}"),
            Self::RR(a, b) => write!(f, "{a} {b}"),
            Self::R(a) => write!(f, "{a}"),
            Self::I(imm) => write!(f, "{imm}"),
            Self::OpOnly => Ok(()),
        }
    }
}

/// A full 32-bit instruction word in CLS-16.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Instruction {
    pub op: Opcode,
    pub format: InstrFormat,
}

impl Instruction {
    /// Checks if this instruction has a valid format for its opcode.
    ///
    /// # Errors
    ///
    /// This function will return an error if the instruction's format is invalid for its opcode.
    pub fn validate(&self) -> Result<()> {
        if self.is_valid() {
            Ok(())
        } else {
            Err(Error::from(PlatformError::InvalidInstruction))
        }
    }

    /// Returns `true` if this instruction has a valid format for its opcode.
    pub const fn is_valid(&self) -> bool {
        #[doc(hidden)]
        macro_rules! assert_format {
            ($fmt:pat) => {
                matches!(self.format, $fmt)
            };
        }
        match self.op {
            Opcode::Halt => assert_format!(InstrFormat::OpOnly),
            Opcode::Nop => assert_format!(InstrFormat::OpOnly),
            Opcode::B => assert_format!(InstrFormat::OpOnly),

            Opcode::Mov => assert_format!(InstrFormat::RR(_, _) | InstrFormat::RI(_, _)),
            Opcode::Add => assert_format!(InstrFormat::RR(_, _) | InstrFormat::RI(_, _)),
            Opcode::Sub => assert_format!(InstrFormat::RR(_, _) | InstrFormat::RI(_, _)),
            Opcode::Mul => assert_format!(InstrFormat::RR(_, _) | InstrFormat::RI(_, _)),
            Opcode::Div => assert_format!(InstrFormat::RR(_, _) | InstrFormat::RI(_, _)),
            Opcode::And => assert_format!(InstrFormat::RR(_, _) | InstrFormat::RI(_, _)),
            Opcode::Or => assert_format!(InstrFormat::RR(_, _) | InstrFormat::RI(_, _)),
            Opcode::Xor => assert_format!(InstrFormat::RR(_, _) | InstrFormat::RI(_, _)),

            Opcode::Not => assert_format!(InstrFormat::R(_)),
            Opcode::Shl => assert_format!(InstrFormat::R(_)),
            Opcode::Shr => assert_format!(InstrFormat::R(_)),

            Opcode::Stl => assert_format!(InstrFormat::RR(_, _)),
            Opcode::Sth => assert_format!(InstrFormat::RR(_, _)),
            Opcode::Ldl => assert_format!(InstrFormat::RR(_, _)),
            Opcode::Ldh => assert_format!(InstrFormat::RR(_, _)),

            Opcode::Jz => assert_format!(InstrFormat::R(_) | InstrFormat::I(_)),
            Opcode::Jmp => assert_format!(InstrFormat::R(_) | InstrFormat::I(_)),
            Opcode::Printi => assert_format!(InstrFormat::R(_) | InstrFormat::I(_)),
            Opcode::Printc => assert_format!(InstrFormat::R(_) | InstrFormat::I(_)),
        }
    }

    /// Generates the corresponding machine-code instruction word for this instruction.
    ///
    /// # Errors
    ///
    /// This function will return an error if the instruction's format is invalid for its opcode.
    pub fn to_bytes(&self) -> Result<[u8; 4]> {
        self.validate()?;
        let op = self.op as u8 | self.format.encode();
        let format = match &self.format {
            InstrFormat::RRI(a, b, imm) => {
                let imm = if let Immediate::Linked(imm) = imm {
                    *imm
                } else {
                    0
                }
                .to_le_bytes();
                [(*a as u8) << 4 | (*b as u8), imm[0], imm[1]]
            }
            InstrFormat::RI(a, imm) => {
                let imm = if let Immediate::Linked(imm) = imm {
                    *imm
                } else {
                    0
                }
                .to_le_bytes();
                [(*a as u8) << 4, imm[0], imm[1]]
            }
            InstrFormat::RR(a, b) => [(*a as u8) << 4 | (*b as u8), 0, 0],
            InstrFormat::R(a) => [(*a as u8) << 4, 0, 0],
            InstrFormat::I(imm) => {
                let imm = if let Immediate::Linked(imm) = imm {
                    *imm
                } else {
                    0
                }
                .to_le_bytes();
                [0, imm[0], imm[1]]
            }
            InstrFormat::OpOnly => [0, 0, 0],
        };
        Ok([op, format[0], format[1], format[2]])
    }

    /// Generates an [Instruction] instance from the given instruction word.
    ///
    /// # Errors
    ///
    /// This function will return an error if the given word is not a valid instruction.
    pub fn from_bytes(bytes: [u8; 4]) -> Result<Self> {
        let op: Opcode = (bytes[0] & 0b000_11111).try_into()?;
        let format = match bytes[0] & 0b111_00000 {
            0b000_00000 => {
                // RRI
                let a: Register = ((bytes[1] & 0b1111_0000) >> 4).try_into()?;
                let b: Register = (bytes[1] & 0b0000_1111).try_into()?;
                let imm = Immediate::Linked(u16::from_le_bytes([bytes[2], bytes[3]]));
                InstrFormat::RRI(a, b, imm)
            }
            0b001_00000 => {
                // RI
                let a: Register = ((bytes[1] & 0b1111_0000) >> 4).try_into()?;
                let imm = Immediate::Linked(u16::from_le_bytes([bytes[2], bytes[3]]));
                InstrFormat::RI(a, imm)
            }
            0b010_00000 => {
                // RR
                let a: Register = ((bytes[1] & 0b1111_0000) >> 4).try_into()?;
                let b: Register = (bytes[1] & 0b0000_1111).try_into()?;
                InstrFormat::RR(a, b)
            }
            0b011_00000 => {
                // R
                let a: Register = ((bytes[1] & 0b1111_0000) >> 4).try_into()?;
                InstrFormat::R(a)
            }
            0b100_00000 => {
                // I
                let imm = Immediate::Linked(u16::from_le_bytes([bytes[2], bytes[3]]));
                InstrFormat::I(imm)
            }
            0b101_00000 => InstrFormat::OpOnly,
            _ => return Err(Error::from(PlatformError::InvalidOpcode)),
        };

        let this = Self { op, format };
        this.validate()?;
        Ok(this)
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.op, self.format)
    }
}
