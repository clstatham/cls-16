//! Common platform code between CLS-16's other modules.

use thiserror::Error;

/// An error for the core platform of CLS-16.
#[derive(Error, Debug)]
pub enum PlatformError {
    #[error("invalid opcode")]
    InvalidOpcode,
    #[error("invalid instruction")]
    InvalidInstruction,
}

/// Type alias for Result<T, [PlatformError]>.
pub type PResult<T> = Result<T, PlatformError>;

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
            0 => Ok(Self::R0),
            1 => Ok(Self::R1),
            2 => Ok(Self::R2),
            3 => Ok(Self::R3),
            4 => Ok(Self::R4),
            5 => Ok(Self::R5),
            6 => Ok(Self::R6),
            7 => Ok(Self::R7),
            8 => Ok(Self::R8),
            9 => Ok(Self::PC),
            10 => Ok(Self::SP),
            11 => Ok(Self::FL),
            _ => Err(PlatformError::InvalidOpcode),
        }
    }
}

/// The various opcodes that can be used in instructions.
///
/// Note that these are the most *basic* operations that the CPU can execute in a single instruction cycle.
/// More complex operations are used in the actual assembly syntax, that are compiled down into combinations of these basic operations.
///
/// ALU opcode notes ([ADD][Opcode::Add], [SUB][Opcode::Sub], [AND][Opcode::And], [OR][Opcode::Or], [NOT][Opcode::Not], [SHL][Opcode::Shl], [SHR][Opcode::Shr]):
///
/// - [FL](Register::FL) Overflow bit is set to 1 if the operation over/underflows, otherwise it is set to 0.
/// - [FL](Register::FL) Parity bit is set to 1 if the result has odd parity, otherwise it is set to 0.
/// - [FL](Register::FL) Zero bit is set to 1 if the result is zero, otherwise it is set to 0.
/// - When [R0](Register::R0) is used as a destination, the result is discarded, but **the [FL](Register::FL) bits are still set.**
///
/// Memory and branching opcode notes ([STL][Opcode::Stl], [STH][Opcode::Sth], [LDL][Opcode::Ldl], [LDH][Opcode::Ldh], [LDI][Opcode::Ldi], [JZ][Opcode::Jz]):
///
/// - The memory addresses are given by either a register or a 16-bit immediate (literal) value.
/// - `xxL` will operate on the lower 8 bits of the relevant register.
/// - `xxH` will operate on the higher 8 bits of the relevant register.
/// - `LDI` will load/store immediate values into the whole register, setting the other bits to 0.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Opcode {
    /* Halt */
    /// Stops the clock
    Halt = 0,

    /* ALU */
    /// `regA <- regB + regC`
    Add,
    /// `regA <- regB - regC`
    Sub,
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
    /// "Jump if Zero (Immediate)"
    /// ```text
    /// if FL.Zero == 1 {
    ///     PC <- (immediate value)
    /// }
    /// ```
    Jzi,
}

impl TryFrom<u8> for Opcode {
    type Error = PlatformError;

    fn try_from(value: u8) -> Result<Self, PlatformError> {
        match value {
            0 => Ok(Self::Halt),
            1 => Ok(Self::Add),
            2 => Ok(Self::Sub),
            3 => Ok(Self::And),
            4 => Ok(Self::Or),
            5 => Ok(Self::Xor),
            6 => Ok(Self::Not),
            7 => Ok(Self::Shl),
            8 => Ok(Self::Shr),
            9 => Ok(Self::Stl),
            10 => Ok(Self::Sth),
            11 => Ok(Self::Ldl),
            12 => Ok(Self::Ldh),
            13 => Ok(Self::Ldi),
            14 => Ok(Self::Jz),
            15 => Ok(Self::Jzi),
            _ => Err(PlatformError::InvalidOpcode),
        }
    }
}

/// Instruction formats indicating how instructions are packed into the 32-bit instruction words in CLS-16.
///
/// Each instruction can be broken down into 4 bytes. The first is always the opcode.
/// The following three are specified by one of this enum's variants.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InstrFormat {
    /// `<Opcode, Register, Register, Register>`
    RRR(Register, Register, Register),
    /// `<Opcode, Register, ImmediateHi, ImmediateLo>`
    RI(Register, u16),
    /// `<Opcode, ZEROS, Register, Register>`
    RR(Register, Register),
    /// `<Opcode, ZEROS, ZEROS, Register>`
    R(Register),
    /// `<Opcode, ZEROS, ImmediateHi, ImmediateLo>`
    I(u16),
    /// `<Opcode, ZEROS, ZEROS, ZEROS>`
    OpOnly,
}

/// A full 32-bit instruction word in CLS-16.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    pub fn validate(self) -> PResult<()> {
        #[doc(hidden)]
        macro_rules! assert_format {
            ($fmt:pat) => {
                if matches!(self.format, $fmt) {
                    Ok(())
                } else {
                    Err(PlatformError::InvalidInstruction)
                }
            };
        }
        match self.op {
            Opcode::Halt => assert_format!(InstrFormat::OpOnly),
            Opcode::Add => assert_format!(InstrFormat::RRR(_, _, _)),
            Opcode::Sub => assert_format!(InstrFormat::RRR(_, _, _)),
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
            Opcode::Jzi => assert_format!(InstrFormat::I(_)),
        }
    }

    /// Generates the corresponding machine-code instruction word for this instruction.
    ///
    /// # Errors
    ///
    /// This function will return an error if the instruction's format is invalid for its opcode.
    pub fn to_bytes(self) -> PResult<[u8; 4]> {
        self.validate()?;
        let op = self.op as u8;
        let format = match self.format {
            InstrFormat::RRR(a, b, c) => [a as u8, b as u8, c as u8],
            InstrFormat::RI(a, imm) => [a as u8, imm.to_le_bytes()[0], imm.to_le_bytes()[1]],
            InstrFormat::RR(a, b) => [0u8, a as u8, b as u8],
            InstrFormat::R(a) => [0u8, 0u8, a as u8],
            InstrFormat::I(imm) => [0u8, imm.to_le_bytes()[0], imm.to_le_bytes()[1]],
            InstrFormat::OpOnly => [0u8, 0u8, 0u8],
        };
        Ok([op, format[0], format[1], format[2]])
    }

    /// Generates an [Instruction] instance from the given instruction word.
    ///
    /// # Errors
    ///
    /// This function will return an error if the given word is not a valid instruction.
    pub fn from_bytes(bytes: [u8; 4]) -> PResult<Self> {
        let op: Opcode = bytes[0].try_into()?;
        let format = match op {
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
                u16::from_le_bytes([bytes[2], bytes[3]]),
            ),
            Opcode::Jz => InstrFormat::R(bytes[3].try_into()?),
            Opcode::Jzi => InstrFormat::I(u16::from_le_bytes([bytes[2], bytes[3]])),
        };

        let this = Self { op, format };
        this.validate()?;
        Ok(this)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instruction_encode() {
        let instr = Instruction {
            op: Opcode::Ldi,
            format: InstrFormat::RI(Register::R3, 12345),
        };
        let bytes = instr.to_bytes().unwrap();
        assert_eq!(bytes, [0x0D, 0x03, 0x39, 0x30]);
    }

    #[test]
    fn test_instruction_decode() {
        let bytes = [0x0D, 0x03, 0x39, 0x30];
        let instr = Instruction::from_bytes(bytes).unwrap();
        assert_eq!(
            instr,
            Instruction {
                op: Opcode::Ldi,
                format: InstrFormat::RI(Register::R3, 12345),
            }
        );
    }
}
