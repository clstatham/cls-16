use std::{collections::VecDeque, time::Duration};

use anyhow::Result;
use tokio::{
    runtime::{Builder, Runtime},
    time::{interval, Interval},
};

use crate::plat::Instruction;

use super::{
    alu::{Alu, AluMode},
    debugger::Debugger,
    microcode::MicroOp,
    ram::Ram,
    registers::{EmuRegisters, Fl},
};

/// The emulator's current state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmuState {
    /// The emulator is allowed to run, and the PC register will be incrememeted by 4 if the end of an instruction occurs on the next clock cycle.
    Continue,
    /// The emulator is allowed to run, and the PC register will NOT be incrememeted on the next end-of-instruction encountered. Used for jumps.
    ///
    /// Note: This automatically switches to [`Continue`][EmuState::Continue] after the current instruction finishes.
    ContinueDontUpdatePc,
    /// Stops execution, but doens't put the emulator in a hard, unrecoverable [`Halt`][EmuState::Halt] state.
    Pause,
    /// Halt execution.
    Halt,
}

/// The main emulation context for CLS-16.
pub struct Emulator<'a> {
    pub registers: EmuRegisters,
    pub alu: Alu,
    pub ram: Ram,
    pub state: EmuState,
    pub instr_history: VecDeque<Instruction>,
    program_memory: &'a [u8],
    micro_op_cache: VecDeque<MicroOp>,
    clock: Interval,
    rt: Runtime,
}

impl<'a> Emulator<'a> {
    /// Loads a binary program into a new [Emulator] instance.
    ///
    /// # Errors
    ///
    /// This function will return an error if the Tokio runtime fails to initialize.
    pub fn new(program: &'a [u8], clock_rate_hz: f64) -> Result<Self> {
        let rt = Builder::new_current_thread()
            .enable_time()
            .global_queue_interval(1)
            .build()?;
        let clock = rt.block_on(async { interval(Duration::from_secs_f64(clock_rate_hz.recip())) });
        let this = Self {
            program_memory: program,
            instr_history: VecDeque::new(),
            registers: EmuRegisters::default(),
            alu: Alu::new(),
            ram: Ram::new(),
            state: EmuState::Continue,
            micro_op_cache: VecDeque::new(),
            clock,
            rt,
        };
        Ok(this)
    }

    /// Runs the emulator, stepping through instructions until it reaches a halt state.
    pub fn run_while_continue(&mut self) -> Result<()> {
        loop {
            match self.state {
                EmuState::Continue | EmuState::ContinueDontUpdatePc => {}
                EmuState::Halt => break,
                EmuState::Pause => {
                    self.debug()?;
                    continue;
                }
            }
            self.microstep()?;
        }
        Ok(())
    }

    pub fn cont(&mut self) -> Result<()> {
        self.state = EmuState::Continue;
        self.run_while_continue()
    }

    pub fn step_instr(&mut self) -> Result<()> {
        let initial_pc = self.registers.pc.value;
        while self.registers.pc.value == initial_pc {
            self.microstep()?;
        }
        Ok(())
    }

    /// Steps a single CPU clock cycle.
    pub fn microstep(&mut self) -> Result<()> {
        if self.state == EmuState::Halt {
            return Ok(());
        }
        self.rt.block_on(async {
            // snap!
            self.clock.tick().await;

            tokio::join!(
                async { self.alu.update() },
                async { self.ram.update() },
                async { self.registers.r0.update() },
                async { self.registers.r1.update() },
                async { self.registers.r2.update() },
                async { self.registers.r3.update() },
                async { self.registers.r4.update() },
                async { self.registers.r5.update() },
                async { self.registers.r6.update() },
                async { self.registers.sp.update() },
                async { self.registers.fp.update() },
                async { self.registers.pc.update() },
                async { self.registers.il.update() },
                async { self.registers.ih.update() },
                async { self.registers.fl.update() },
            );

            if let Some(op) = self.micro_op_cache.pop_front() {
                log::trace!("> {:?}", op);
                match op {
                    MicroOp::Halt => {
                        self.state = EmuState::Halt;
                        return Ok(());
                    }
                    MicroOp::Breakpoint => {
                        println!("Breaking");
                        self.state = EmuState::Pause;
                        return Ok(());
                    }
                    MicroOp::Nop => {}
                    MicroOp::Eoi => {
                        return Ok(());
                    }
                    MicroOp::WriteReg(reg) => self.registers.write(reg)?,
                    MicroOp::NoWriteReg(reg) => self.registers.no_write(reg)?,
                    MicroOp::SetRegLoopback(reg) => self.registers.loopback(reg)?,
                    MicroOp::SetRegReg(dest, src) => {
                        self.registers
                            .route_input_hi(dest, self.registers.subscribe_hi(src))?;
                        self.registers
                            .route_input_lo(dest, self.registers.subscribe_lo(src))?;
                    }
                    MicroOp::SetAluLeft(reg) => {
                        self.alu.route_left_hi(self.registers.subscribe_hi(reg));
                        self.alu.route_left_lo(self.registers.subscribe_lo(reg));
                    }
                    MicroOp::SetAluRight(reg) => {
                        self.alu.route_right_hi(self.registers.subscribe_hi(reg));
                        self.alu.route_right_lo(self.registers.subscribe_lo(reg));
                    }
                    MicroOp::SetAluResult(reg) => {
                        self.registers
                            .route_input_hi(reg, self.alu.output_bus_hi.subscribe())?;
                        self.registers
                            .route_input_lo(reg, self.alu.output_bus_lo.subscribe())?;
                    }
                    MicroOp::SetAluModeAdd => self.alu.mode = AluMode::Add,
                    MicroOp::SetAluModeSub => self.alu.mode = AluMode::Sub,
                    MicroOp::SetAluModeDiv => self.alu.mode = AluMode::Div,
                    MicroOp::SetAluModeMul => self.alu.mode = AluMode::Mul,
                    MicroOp::SetAluModeAnd => self.alu.mode = AluMode::And,
                    MicroOp::SetAluModeOr => self.alu.mode = AluMode::Or,
                    MicroOp::SetAluModeXor => self.alu.mode = AluMode::Xor,
                    MicroOp::SetAluModeNot => self.alu.mode = AluMode::Not,
                    MicroOp::SetAluModeShl => self.alu.mode = AluMode::Shl,
                    MicroOp::SetAluModeShr => self.alu.mode = AluMode::Shr,
                    MicroOp::SetRamAddr(reg) => {
                        self.ram.route_addr_hi(self.registers.subscribe_hi(reg));
                        self.ram.route_addr_lo(self.registers.subscribe_lo(reg));
                    }
                    MicroOp::SetRamDataLo(reg) => {
                        self.ram.route_data(self.registers.subscribe_lo(reg));
                    }
                    MicroOp::SetRamDataHi(reg) => {
                        self.ram.route_data(self.registers.subscribe_hi(reg));
                    }
                    MicroOp::SetRamOutputLo(reg) => {
                        self.registers
                            .route_input_lo(reg, self.ram.output_bus.subscribe())?;
                    }
                    MicroOp::SetRamOutputHi(reg) => {
                        self.registers
                            .route_input_hi(reg, self.ram.output_bus.subscribe())?;
                    }
                    MicroOp::WriteRam => self.ram.write(),
                    MicroOp::NoWriteRam => self.ram.no_write(),
                    MicroOp::SetRamLoopback => self.ram.loopback(),
                    MicroOp::SetPc(reg) => {
                        let lo = *self.registers.subscribe_lo(reg).borrow();
                        let hi = *self.registers.subscribe_hi(reg).borrow();
                        self.registers.pc.value = u16::from_le_bytes([lo, hi]);
                        self.state = EmuState::ContinueDontUpdatePc;
                    }
                    MicroOp::SetPcIfZero(reg) => {
                        let fl = Fl::from_bits_truncate(self.registers.fl.value);
                        if fl.contains(Fl::ZERO) {
                            let lo = *self.registers.subscribe_lo(reg).borrow();
                            let hi = *self.registers.subscribe_hi(reg).borrow();
                            self.registers.pc.value = u16::from_le_bytes([lo, hi]);
                            self.state = EmuState::ContinueDontUpdatePc;
                        }
                    }
                    MicroOp::Printi(reg) => {
                        let lo = *self.registers.subscribe_lo(reg).borrow();
                        let hi = *self.registers.subscribe_hi(reg).borrow();
                        let value = u16::from_le_bytes([lo, hi]);
                        println!("{}", value);
                    }
                    MicroOp::Printc(reg) => {
                        let lo = *self.registers.subscribe_lo(reg).borrow();
                        let lo = &[lo];
                        let c = std::str::from_utf8(lo)?;
                        print!("{}", c);
                    }
                }
            } else {
                if self.state == EmuState::ContinueDontUpdatePc {
                    self.state = EmuState::Continue;
                } else {
                    self.registers.pc.value += 4;
                }
                let current_instr = &self.program_memory
                    [self.registers.pc.value as usize..self.registers.pc.value as usize + 4];
                self.registers.il.value = u16::from_le_bytes([current_instr[0], current_instr[1]]);
                self.registers.ih.value = u16::from_le_bytes([current_instr[2], current_instr[3]]);

                let current_instr = Instruction::from_bytes([
                    current_instr[0],
                    current_instr[1],
                    current_instr[2],
                    current_instr[3],
                ])?;
                self.instr_history.push_back(current_instr.clone());
                log::debug!(">>> {}", current_instr);
                self.micro_op_cache = current_instr.to_microcode()?.into();
            }
            Ok::<(), anyhow::Error>(())
        })
    }

    pub fn debug(&mut self) -> Result<()> {
        Debugger::new(self).repl()
    }
}
