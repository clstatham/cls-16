use anyhow::Result;

use crate::plat::{InstrFormat, Instruction, Opcode, Register};

/// Micro-operations used internally, mainly to control various busses.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    /// Routes the first register's input bus to the second register's output bus.
    SetRegReg(Register, Register),

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
    /// Sets [PC][Register::PC] to the current value at the given register's output bus.
    SetPc(Register),
    /// Sets [PC][Register::PC] to the current value at the given register's output bus if [FL][Register::FL]'s "zero" bit is 1.
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
            (Opcode::Mov, InstrFormat::RR(dest, src)) => vec![
                MicroOp::SetRegReg(dest, src),
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::Mov, InstrFormat::RI(dest, _imm)) => vec![
                MicroOp::SetRegReg(dest, Register::IH),
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::Add, InstrFormat::RR(dest, right)) => vec![
                MicroOp::SetAluLeft(dest),
                MicroOp::SetAluRight(right),
                MicroOp::SetAluResult(dest),
                MicroOp::SetAluModeAdd,
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::Add, InstrFormat::RI(dest, _imm)) => vec![
                MicroOp::SetAluLeft(dest),
                MicroOp::SetAluRight(Register::IH),
                MicroOp::SetAluResult(dest),
                MicroOp::SetAluModeAdd,
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::Sub, InstrFormat::RR(dest, right)) => vec![
                MicroOp::SetAluLeft(dest),
                MicroOp::SetAluRight(right),
                MicroOp::SetAluResult(dest),
                MicroOp::SetAluModeSub,
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::Sub, InstrFormat::RI(dest, _imm)) => vec![
                MicroOp::SetAluLeft(dest),
                MicroOp::SetAluRight(Register::IH),
                MicroOp::SetAluResult(dest),
                MicroOp::SetAluModeSub,
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::And, InstrFormat::RR(dest, right)) => vec![
                MicroOp::SetAluLeft(dest),
                MicroOp::SetAluRight(right),
                MicroOp::SetAluResult(dest),
                MicroOp::SetAluModeAnd,
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::And, InstrFormat::RI(dest, _imm)) => vec![
                MicroOp::SetAluLeft(dest),
                MicroOp::SetAluRight(Register::IH),
                MicroOp::SetAluResult(dest),
                MicroOp::SetAluModeAnd,
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::Or, InstrFormat::RR(dest, right)) => vec![
                MicroOp::SetAluLeft(dest),
                MicroOp::SetAluRight(right),
                MicroOp::SetAluResult(dest),
                MicroOp::SetAluModeOr,
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::Or, InstrFormat::RI(dest, _imm)) => vec![
                MicroOp::SetAluLeft(dest),
                MicroOp::SetAluRight(Register::IH),
                MicroOp::SetAluResult(dest),
                MicroOp::SetAluModeOr,
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::Xor, InstrFormat::RR(dest, right)) => vec![
                MicroOp::SetAluLeft(dest),
                MicroOp::SetAluRight(right),
                MicroOp::SetAluResult(dest),
                MicroOp::SetAluModeXor,
                MicroOp::WriteReg(dest),
                MicroOp::NoWriteReg(dest),
                MicroOp::SetRegLoopback(dest),
            ],
            (Opcode::Xor, InstrFormat::RI(dest, _imm)) => vec![
                MicroOp::SetAluLeft(dest),
                MicroOp::SetAluRight(Register::IH),
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
            (Opcode::Jmp, InstrFormat::R(addr)) => vec![MicroOp::SetPc(addr)],
            (Opcode::Jmp, InstrFormat::I(_imm)) => vec![MicroOp::SetPc(Register::IH)],
            (Opcode::Jz, InstrFormat::R(addr)) => vec![MicroOp::SetPcIfZero(addr)],
            (Opcode::Jz, InstrFormat::I(_imm)) => vec![MicroOp::SetPcIfZero(Register::IH)],
            (Opcode::Printi, InstrFormat::R(reg)) => vec![MicroOp::Printi(reg)],
            (Opcode::Printi, InstrFormat::I(_imm)) => vec![MicroOp::Printi(Register::IH)],
            (Opcode::Printc, InstrFormat::R(reg)) => vec![MicroOp::Printc(reg)],
            (Opcode::Printc, InstrFormat::I(_imm)) => vec![MicroOp::Printc(Register::IH)],
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
