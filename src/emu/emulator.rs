use std::{collections::VecDeque, time::Duration};

use anyhow::Result;
use tokio::{
    runtime::{Builder, Runtime},
    sync::watch::{channel, Sender},
    time::{interval, Interval},
};

use crate::plat::Instruction;

use super::{
    alu::{Alu, AluMode},
    microcode::MicroOp,
    ram::Ram,
    registers::{EmuRegisters, Fl},
};

pub struct Counter {
    count: u16,
    pub tx_hi: Sender<u8>,
    pub tx_lo: Sender<u8>,
}

impl Counter {
    pub fn new(initial_value: u16) -> Self {
        let hi = (initial_value >> 8 & 0xff) as u8;
        let lo = (initial_value & 0xff) as u8;
        let (tx_hi, _) = channel(hi);
        let (tx_lo, _) = channel(lo);
        Self {
            count: initial_value,
            tx_hi,
            tx_lo,
        }
    }

    pub fn tick(&mut self) {
        self.count += 1;
        let hi = (self.count >> 8 & 0xff) as u8;
        let lo = (self.count & 0xff) as u8;
        self.tx_hi.send_replace(hi);
        self.tx_lo.send_replace(lo);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmuState {
    Run,
    Halt,
}

pub struct Emulator<'a> {
    pub registers: EmuRegisters,
    pub alu: Alu,
    pub ram: Ram,
    state: EmuState,
    program_memory: &'a [u8],
    micro_op_cache: VecDeque<MicroOp>,
    clock: Interval,
    rt: Runtime,
}

impl<'a> Emulator<'a> {
    pub fn new(program: &'a [u8], clock_rate_hz: f64) -> Result<Self> {
        let rt = Builder::new_current_thread()
            .enable_time()
            .global_queue_interval(1)
            .build()?;
        let clock = rt.block_on(async { interval(Duration::from_secs_f64(clock_rate_hz.recip())) });
        let this = Self {
            program_memory: program,
            registers: EmuRegisters::default(),
            alu: Alu::new(),
            ram: Ram::new(),
            state: EmuState::Run,
            micro_op_cache: VecDeque::new(),
            clock,
            rt,
        };
        Ok(this)
    }

    pub fn run_until_halt(&mut self) -> Result<()> {
        while self.state != EmuState::Halt {
            self.step()?;
        }
        Ok(())
    }

    pub fn step(&mut self) -> Result<()> {
        if self.state == EmuState::Halt {
            return Ok(());
        }
        self.rt.block_on(async {
            // snap!
            self.clock.tick().await;

            for _ in 0..2 {
                // paranoid multiple updates
                tokio::join!(
                    async { self.alu.update() },
                    async { self.ram.update() },
                    async { self.registers.r1.update() },
                    async { self.registers.r2.update() },
                    async { self.registers.r3.update() },
                    async { self.registers.r4.update() },
                    async { self.registers.r5.update() },
                    async { self.registers.r6.update() },
                    async { self.registers.r7.update() },
                    async { self.registers.r8.update() },
                    async { self.registers.pc.update() },
                    async { self.registers.ih.update() },
                    async { self.registers.il.update() },
                    async { self.registers.fl.update() },
                );
            }

            if let Some(op) = self.micro_op_cache.pop_front() {
                log::debug!("> {:?}", op);
                match op {
                    MicroOp::Halt => {
                        self.state = EmuState::Halt;
                        return Ok(());
                    }
                    MicroOp::Nop => {}
                    MicroOp::Eoi => {
                        return Ok(());
                    }
                    MicroOp::WriteReg(reg) => self.registers.write(reg)?,
                    MicroOp::NoWriteReg(reg) => self.registers.no_write(reg)?,
                    MicroOp::SetRegLoopback(reg) => self.registers.loopback(reg)?,
                    MicroOp::SetRegImmediate(reg) => {
                        self.registers
                            .route_input_hi(reg, self.registers.il.output_bus_hi.subscribe())?;
                        self.registers
                            .route_input_lo(reg, self.registers.il.output_bus_lo.subscribe())?;
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
                    MicroOp::SetPcIfZero(reg) => {
                        let fl = Fl::from_bits_truncate(self.registers.fl.value);
                        if fl.contains(Fl::ZERO) {
                            let lo = *self.registers.subscribe_lo(reg).borrow();
                            let hi = *self.registers.subscribe_hi(reg).borrow();
                            self.registers.pc.value = (lo as u16) << 8 | (hi as u16);
                        }
                    }
                    MicroOp::Printi(reg) => {
                        let lo = *self.registers.subscribe_lo(reg).borrow();
                        let hi = *self.registers.subscribe_hi(reg).borrow();
                        let value = (lo as u16) << 8 | (hi as u16);
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
                self.registers.pc.value += 4;
                let current_instr = &self.program_memory
                    [self.registers.pc.value as usize..self.registers.pc.value as usize + 4];
                self.registers.ih.value =
                    (current_instr[0] as u16) << 8 | (current_instr[1] as u16);
                self.registers.il.value =
                    (current_instr[2] as u16) << 8 | (current_instr[3] as u16);

                let current_instr = Instruction::from_bytes([
                    current_instr[0],
                    current_instr[1],
                    current_instr[2],
                    current_instr[3],
                ])?;
                log::debug!(">>> {:?}", current_instr);
                self.micro_op_cache = current_instr.to_microcode()?.into();
            }
            Ok::<(), anyhow::Error>(())
        })
    }
}
