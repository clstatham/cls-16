use anyhow::Result;

use crate::plat::{InstrFormat, Instruction, Opcode, Register};

/// Micro-operations used internally, mainly to control various busses.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum MicroOp {
    /* Misc */
    /// Stops all counters.
    Halt,
    /// Does nothing.
    Nop,
    /// End of instruction (advance program counter).
    Eoi,

    /* Registers */
    /// Sets the "write" bit of the register's control bus to 1.
    WriteReg(Register),
    /// Sets the "write" bit of the register's control bus to 0.
    NoWriteReg(Register),
    /// Routes the given register's input bus to its own output bus.
    SetRegLoopback(Register),
    /// Routes the given register's input bus to IL's output bus.
    SetRegImmediate(Register),

    /* ALU */
    /// Routes the ALU's left operand bus to the given register's output bus.
    SetAluLeft(Register),
    /// Routes the ALU's right operand bus to the given register's output bus.
    SetAluRight(Register),
    /// Routes the given register's input bus to the ALU's output bus.
    SetAluResult(Register),
    /// Sets the ALU's mode to "add".
    SetAluModeAdd,
    /// Sets the ALU's mode to "subtract".
    SetAluModeSub,
    /// Sets the ALU's mode to "bitwise and".
    SetAluModeAnd,
    /// Sets the ALU's mode to "bitwise or".
    SetAluModeOr,
    /// Sets the ALU's mode to "bitwise exclusive or".
    SetAluModeXor,
    /// Sets the ALU's mode to "bitwise not". Note that this one only uses the left operand bus.
    SetAluModeNot,
    /// Sets the ALU's mode to "bit shift left". Note that this one only uses the left operand bus.
    SetAluModeShl,
    /// Sets the ALU's mode to "bit shift right". Note that this one only uses the left operand bus.
    SetAluModeShr,

    /* Memory */
    /// Routes the memory unit's address bus to the given register's output bus.
    SetRamAddr(Register),
    /// Routes the memory unit's data bus to the lower 8 bits of the given register's output bus.
    SetRamDataLo(Register),
    /// Routes the memory unit's data bus to the higher 8 bits of the given register's output bus.
    SetRamDataHi(Register),
    /// Routes the lower 8 bits of the given register's input bus to the memory unit's output bus.
    SetRamOutputLo(Register),
    /// Routes the higher 8 bits of the given register's input bus to the memory unit's output bus.
    SetRamOutputHi(Register),
    /// Sets the "write" bit of the memory unit's control bus to 1.
    WriteRam,
    /// Sets the "write" bit of the memory unit's control bus to 0.
    NoWriteRam,
    /// Routes the memory unit's output bus to its own data bus.
    SetRamLoopback,

    /* Branching */
    /// Routes [PC][Register::PC]'s input bus to the given register's output bus if [FL][Register::FL]'s "zero" bit is 1.
    SetPcIfZero(Register),

    /* Debugging */
    /// See [`Opcode::Printi`][crate::plat::Opcode::Printi].
    Printi(Register),
    /// See [`Opcode::Printc`][crate::plat::Opcode::Printc].
    Printc(Register),
}

impl<'a> Instruction<'a> {
    /// Translates the instruction to its corresponding sequence of [MicroOp]s.
    ///
    /// # Errors
    ///
    /// This function will return an error if the instruction's format is invalid for its opcode.
    pub fn to_microcode(self) -> Result<Vec<MicroOp>> {
        self.validate()?;
        let mut mc = match (self.op, self.format) {
            (Opcode::Nop, InstrFormat::OpOnly) => vec![MicroOp::Nop],
            (Opcode::Halt, InstrFormat::OpOnly) => vec![MicroOp::Halt],
            (Opcode::Printi, InstrFormat::R(reg)) => vec![MicroOp::Printi(reg)],
            (Opcode::Printc, InstrFormat::R(reg)) => vec![MicroOp::Printc(reg)],
            (Opcode::Ldi, InstrFormat::RI(reg, _imm)) => vec![
                MicroOp::SetRegImmediate(reg),
                MicroOp::WriteReg(reg),
                MicroOp::NoWriteReg(reg),
                MicroOp::SetRegLoopback(reg),
            ],
            (Opcode::Add, InstrFormat::RRR(dest, left, right)) => vec![
                MicroOp::SetAluLeft(left),
                MicroOp::SetAluRight(right),
                MicroOp::SetAluResult(dest),
                MicroOp::SetAluModeAdd,
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::Sub, InstrFormat::RRR(dest, left, right)) => vec![
                MicroOp::SetAluLeft(left),
                MicroOp::SetAluRight(right),
                MicroOp::SetAluResult(dest),
                MicroOp::SetAluModeSub,
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::And, InstrFormat::RRR(dest, left, right)) => vec![
                MicroOp::SetAluLeft(left),
                MicroOp::SetAluRight(right),
                MicroOp::SetAluResult(dest),
                MicroOp::SetAluModeAnd,
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::Or, InstrFormat::RRR(dest, left, right)) => vec![
                MicroOp::SetAluLeft(left),
                MicroOp::SetAluRight(right),
                MicroOp::SetAluResult(dest),
                MicroOp::SetAluModeOr,
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::Xor, InstrFormat::RRR(dest, left, right)) => vec![
                MicroOp::SetAluLeft(left),
                MicroOp::SetAluRight(right),
                MicroOp::SetAluResult(dest),
                MicroOp::SetAluModeXor,
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::Not, InstrFormat::RR(dest, left)) => vec![
                MicroOp::SetAluLeft(left),
                MicroOp::SetAluResult(dest),
                MicroOp::SetAluModeNot,
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::Shl, InstrFormat::RR(dest, inp)) => vec![
                MicroOp::SetAluLeft(inp),
                MicroOp::SetAluResult(dest),
                MicroOp::SetAluModeShl,
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::Shr, InstrFormat::RR(dest, inp)) => vec![
                MicroOp::SetAluLeft(inp),
                MicroOp::SetAluResult(dest),
                MicroOp::SetAluModeShr,
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::Stl, InstrFormat::RR(addr, inp)) => vec![
                MicroOp::SetRamAddr(addr),
                MicroOp::SetRamDataLo(inp),
                MicroOp::WriteRam,
                MicroOp::NoWriteRam,
                MicroOp::SetRamLoopback,
            ],
            (Opcode::Sth, InstrFormat::RR(addr, inp)) => vec![
                MicroOp::SetRamAddr(addr),
                MicroOp::SetRamDataHi(inp),
                MicroOp::WriteRam,
                MicroOp::NoWriteRam,
                MicroOp::SetRamLoopback,
            ],
            (Opcode::Ldl, InstrFormat::RR(dest, addr)) => vec![
                MicroOp::SetRamAddr(addr),
                MicroOp::SetRamOutputLo(dest),
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::Ldh, InstrFormat::RR(dest, addr)) => vec![
                MicroOp::SetRamAddr(addr),
                MicroOp::SetRamOutputHi(dest),
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::Jz, InstrFormat::R(addr)) => vec![MicroOp::SetPcIfZero(addr)],
            _ => unreachable!("Invalid instruction found which wasn't caught by validate()"),
        };
        mc.push(MicroOp::Eoi);
        Ok(mc)
    }

    /// Calculates the number of CPU cycles required for the instruction to fully execute.
    ///
    /// # Errors
    ///
    /// This function will return an error if the instruction's format is invalid for its opcode.
    pub fn required_cycles(self) -> Result<usize> {
        Ok(self.to_microcode()?.len())
    }
}
