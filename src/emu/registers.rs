use anyhow::{Error, Result};
use tokio::sync::watch::{channel, Receiver, Sender};

use crate::plat::Register;

use super::EmuError;

bitflags::bitflags! {
    /// The CPU status flags register.
    #[derive(Debug)]
    pub struct Fl: u16 {
        /// Set if the result of the last ALU operation was zero.
        const ZERO = 1 << 0;
        /// Set if the result of the last ALU operation overflowed the register.
        const CARRY = 1 << 1;
    }
}

/// A general purpose register (R1-R8)'s logical representation inside the CPU.
/// These can only be accessed via their I/O busses.
pub struct GpReg {
    write_enabled: bool,
    pub input_bus_lo: Receiver<u8>,
    pub input_bus_hi: Receiver<u8>,
    pub output_bus_lo: Sender<u8>,
    pub output_bus_hi: Sender<u8>,
}

impl GpReg {
    /// Creates a new [`GpReg`].
    pub fn new() -> Self {
        let (output_bus_lo, input_bus_lo) = channel(0u8);
        let (output_bus_hi, input_bus_hi) = channel(0u8);
        Self {
            write_enabled: false,
            input_bus_lo,
            input_bus_hi,
            output_bus_lo,
            output_bus_hi,
        }
    }

    /// Enables writing to the register. The register's internal state will be updated next time `update` is called.
    pub fn write(&mut self) {
        self.write_enabled = true;
    }

    /// Disables writing to the register.
    pub fn no_write(&mut self) {
        self.write_enabled = false;
    }

    /// Routes the register's output bus to its own input bus.
    pub fn loopback(&mut self) {
        self.input_bus_lo = self.output_bus_lo.subscribe();
        self.input_bus_hi = self.output_bus_hi.subscribe();
    }

    /// Routes the given output bus to the higher 8 bits of the register's input bus.
    pub fn route_input_hi(&mut self, output_bus: Receiver<u8>) {
        self.input_bus_hi = output_bus;
    }

    /// Routes the given output bus to the lower 8 bits of the register's input bus.
    pub fn route_input_lo(&mut self, output_bus: Receiver<u8>) {
        self.input_bus_lo = output_bus;
    }

    /// Updates the register's internal state, performing any writes and sending its value along its output bus.
    pub fn update(&mut self) {
        if self.write_enabled {
            let hi = *self.input_bus_hi.borrow();
            let lo = *self.input_bus_lo.borrow();
            self.output_bus_hi.send_replace(hi);
            self.output_bus_lo.send_replace(lo);
        }
    }

    pub fn value(&self) -> u16 {
        let lo = *self.output_bus_lo.subscribe().borrow();
        let hi = *self.output_bus_hi.subscribe().borrow();
        u16::from_le_bytes([lo, hi])
    }
}

impl Default for GpReg {
    fn default() -> Self {
        Self::new()
    }
}

/// One of the "special" registers in the CPU (PC, IH, IL, FL).
/// Contrary to [`GpReg`]s, these CAN be directly modified by the emulator.
/// Special registers are used to control various inner workings of the CPU.
pub struct SpecialReg {
    pub value: u16,
    pub output_bus_lo: Sender<u8>,
    pub output_bus_hi: Sender<u8>,
}

impl SpecialReg {
    pub fn new() -> Self {
        let (output_bus_lo, _) = channel(0);
        let (output_bus_hi, _) = channel(0);
        Self {
            value: 0,
            output_bus_lo,
            output_bus_hi,
        }
    }

    /// Sends the register's value along its output bus.
    pub fn update(&self) {
        self.output_bus_hi
            .send_replace((self.value >> 8 & 0xff) as u8);
        self.output_bus_lo.send_replace((self.value & 0xff) as u8);
    }
}

impl Default for SpecialReg {
    fn default() -> Self {
        Self::new()
    }
}

pub struct R0 {
    tx: Sender<u8>,
}

impl R0 {
    pub fn new() -> Self {
        let (tx, _) = channel(0);
        Self { tx }
    }

    pub fn update(&self) {
        self.tx.send_replace(0);
    }
}

impl Default for R0 {
    fn default() -> Self {
        Self::new()
    }
}

/// The full set of registers in the emulator.
#[derive(Default)]
pub struct EmuRegisters {
    pub r0: R0,
    pub r1: GpReg,
    pub r2: GpReg,
    pub r3: GpReg,
    pub r4: GpReg,
    pub r5: GpReg,
    pub r6: GpReg,
    pub rx: GpReg,
    pub sp: GpReg,
    pub fp: GpReg,
    pub pc: SpecialReg,
    pub il: SpecialReg,
    pub ih: SpecialReg,
    pub fl: SpecialReg,
}

impl EmuRegisters {
    pub fn write(&mut self, reg: Register) -> Result<()> {
        match reg {
            Register::R1 => self.r1.write(),
            Register::R2 => self.r2.write(),
            Register::R3 => self.r3.write(),
            Register::R4 => self.r4.write(),
            Register::R5 => self.r5.write(),
            Register::R6 => self.r6.write(),
            Register::RX => self.rx.write(),
            Register::SP => self.sp.write(),
            Register::FP => self.fp.write(),
            _ => return Err(Error::from(EmuError::InvalidRegister(reg))),
        }
        Ok(())
    }

    pub fn no_write(&mut self, reg: Register) -> Result<()> {
        match reg {
            Register::R1 => self.r1.no_write(),
            Register::R2 => self.r2.no_write(),
            Register::R3 => self.r3.no_write(),
            Register::R4 => self.r4.no_write(),
            Register::R5 => self.r5.no_write(),
            Register::R6 => self.r6.no_write(),
            Register::RX => self.rx.no_write(),
            Register::SP => self.sp.no_write(),
            Register::FP => self.fp.no_write(),
            _ => return Err(Error::from(EmuError::InvalidRegister(reg))),
        }
        Ok(())
    }

    pub fn loopback(&mut self, reg: Register) -> Result<()> {
        match reg {
            Register::R1 => self.r1.loopback(),
            Register::R2 => self.r2.loopback(),
            Register::R3 => self.r3.loopback(),
            Register::R4 => self.r4.loopback(),
            Register::R5 => self.r5.loopback(),
            Register::R6 => self.r6.loopback(),
            Register::RX => self.rx.loopback(),
            Register::SP => self.sp.loopback(),
            Register::FP => self.fp.loopback(),
            _ => return Err(Error::from(EmuError::InvalidRegister(reg))),
        }
        Ok(())
    }

    pub fn route_input_hi(&mut self, reg: Register, output_bus: Receiver<u8>) -> Result<()> {
        match reg {
            Register::R1 => self.r1.route_input_hi(output_bus),
            Register::R2 => self.r2.route_input_hi(output_bus),
            Register::R3 => self.r3.route_input_hi(output_bus),
            Register::R4 => self.r4.route_input_hi(output_bus),
            Register::R5 => self.r5.route_input_hi(output_bus),
            Register::R6 => self.r6.route_input_hi(output_bus),
            Register::RX => self.rx.route_input_hi(output_bus),
            Register::SP => self.sp.route_input_hi(output_bus),
            Register::FP => self.fp.route_input_hi(output_bus),
            _ => return Err(Error::from(EmuError::InvalidRegister(reg))),
        }
        Ok(())
    }

    pub fn route_input_lo(&mut self, reg: Register, output_bus: Receiver<u8>) -> Result<()> {
        match reg {
            Register::R1 => self.r1.route_input_lo(output_bus),
            Register::R2 => self.r2.route_input_lo(output_bus),
            Register::R3 => self.r3.route_input_lo(output_bus),
            Register::R4 => self.r4.route_input_lo(output_bus),
            Register::R5 => self.r5.route_input_lo(output_bus),
            Register::R6 => self.r6.route_input_lo(output_bus),
            Register::RX => self.rx.route_input_lo(output_bus),
            Register::SP => self.sp.route_input_lo(output_bus),
            Register::FP => self.fp.route_input_lo(output_bus),
            _ => return Err(Error::from(EmuError::InvalidRegister(reg))),
        }
        Ok(())
    }

    pub fn subscribe_hi(&self, reg: Register) -> Receiver<u8> {
        match reg {
            Register::R0 => self.r0.tx.subscribe(),
            Register::R1 => self.r1.output_bus_hi.subscribe(),
            Register::R2 => self.r2.output_bus_hi.subscribe(),
            Register::R3 => self.r3.output_bus_hi.subscribe(),
            Register::R4 => self.r4.output_bus_hi.subscribe(),
            Register::R5 => self.r5.output_bus_hi.subscribe(),
            Register::R6 => self.r6.output_bus_hi.subscribe(),
            Register::RX => self.rx.output_bus_hi.subscribe(),
            Register::SP => self.sp.output_bus_hi.subscribe(),
            Register::FP => self.fp.output_bus_hi.subscribe(),
            Register::PC => self.pc.output_bus_hi.subscribe(),
            Register::IL => self.il.output_bus_hi.subscribe(),
            Register::IH => self.ih.output_bus_hi.subscribe(),
            Register::FL => self.fl.output_bus_hi.subscribe(),
        }
    }

    pub fn subscribe_lo(&self, reg: Register) -> Receiver<u8> {
        match reg {
            Register::R0 => self.r0.tx.subscribe(),
            Register::R1 => self.r1.output_bus_lo.subscribe(),
            Register::R2 => self.r2.output_bus_lo.subscribe(),
            Register::R3 => self.r3.output_bus_lo.subscribe(),
            Register::R4 => self.r4.output_bus_lo.subscribe(),
            Register::R5 => self.r5.output_bus_lo.subscribe(),
            Register::R6 => self.r6.output_bus_lo.subscribe(),
            Register::RX => self.rx.output_bus_lo.subscribe(),
            Register::SP => self.sp.output_bus_lo.subscribe(),
            Register::FP => self.fp.output_bus_lo.subscribe(),
            Register::PC => self.pc.output_bus_lo.subscribe(),
            Register::IL => self.il.output_bus_lo.subscribe(),
            Register::IH => self.ih.output_bus_lo.subscribe(),
            Register::FL => self.fl.output_bus_lo.subscribe(),
        }
    }
}
