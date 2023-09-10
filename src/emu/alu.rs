use tokio::sync::watch::{channel, Receiver, Sender};

// The ALU's current mode of operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum AluMode {
    Add = 0,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Xor,
    Not,
    Shl,
    Shr,
}

bitflags::bitflags! {
    /// The ALU status flags.
    #[derive(Debug)]
    pub struct AluStatus: u16 {
        /// Set if the result of the last ALU operation was zero.
        const ZERO = 1 << 0;
        /// Set if the result of the last ALU operation overflowed the register.
        const CARRY = 1 << 1;
    }
}

/// The ALU (Arithmetic and Logic Unit) of CLS-16.
pub struct Alu {
    pub left_bus_lo: Receiver<u8>,
    pub left_bus_hi: Receiver<u8>,
    pub right_bus_lo: Receiver<u8>,
    pub right_bus_hi: Receiver<u8>,
    pub output_bus_lo: Sender<u8>,
    pub output_bus_hi: Sender<u8>,
    pub mode: AluMode,
    pub status: AluStatus,
}

impl Alu {
    /// Creates a new [`Alu`].
    pub fn new() -> Self {
        let (output_bus_lo, left_bus_lo) = channel(0u8);
        let (output_bus_hi, left_bus_hi) = channel(0u8);
        let right_bus_lo = output_bus_lo.subscribe();
        let right_bus_hi = output_bus_hi.subscribe();
        Self {
            left_bus_lo,
            left_bus_hi,
            right_bus_lo,
            right_bus_hi,
            output_bus_lo,
            output_bus_hi,
            mode: AluMode::Add,
            status: AluStatus::empty(),
        }
    }

    /// Routes the given output bus to the lower 8 bits of the ALU's left input.
    pub fn route_left_lo(&mut self, output_bus: Receiver<u8>) {
        self.left_bus_lo = output_bus;
    }

    /// Routes the given output bus to the higher 8 bits of the ALU's left input.
    pub fn route_left_hi(&mut self, output_bus: Receiver<u8>) {
        self.left_bus_hi = output_bus;
    }

    /// Routes the given output bus to the lower 8 bits of the ALU's right input.
    pub fn route_right_lo(&mut self, output_bus: Receiver<u8>) {
        self.right_bus_lo = output_bus;
    }

    /// Routes the given output bus to the higher 8 bits of the ALU's right input.
    pub fn route_right_hi(&mut self, output_bus: Receiver<u8>) {
        self.right_bus_hi = output_bus;
    }

    /// Updates the ALU's internal state, performing its currently-configured calculation and sending the result along its output bus.
    pub fn update(&mut self) {
        let left = u16::from_le_bytes([*self.left_bus_lo.borrow(), *self.left_bus_hi.borrow()]);
        let right = u16::from_le_bytes([*self.right_bus_lo.borrow(), *self.right_bus_hi.borrow()]);
        let result = match self.mode {
            AluMode::Add => {
                let (result, overflow) = left.overflowing_add(right);
                self.status.set(AluStatus::CARRY, overflow);
                result
            }
            AluMode::Sub => {
                let (result, overflow) = left.overflowing_sub(right);
                self.status.set(AluStatus::CARRY, overflow);
                result
            }
            AluMode::Mul => {
                let (result, overflow) = left.overflowing_mul(right);
                self.status.set(AluStatus::CARRY, overflow);
                result
            }
            AluMode::Div => {
                let (result, overflow) = left.overflowing_div(right);
                self.status.set(AluStatus::CARRY, overflow);
                result
            }
            AluMode::And => left & right,
            AluMode::Or => left | right,
            AluMode::Xor => left ^ right,
            AluMode::Not => !left,
            AluMode::Shl => left << 1,
            AluMode::Shr => left >> 1,
        };
        self.status.set(AluStatus::ZERO, result == 0);
        self.output_bus_hi
            .send_replace(((result >> 8) & 0xff) as u8);
        self.output_bus_lo.send_replace((result & 0xff) as u8);
    }
}

impl Default for Alu {
    fn default() -> Self {
        Self::new()
    }
}
