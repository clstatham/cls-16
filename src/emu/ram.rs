use tokio::sync::watch::{channel, Receiver, Sender};

pub struct Ram {
    pub addr_bus_lo: Receiver<u8>,
    pub addr_bus_hi: Receiver<u8>,
    pub data_bus: Receiver<u8>,
    pub output_bus: Sender<u8>,
    write_enabled: bool,
    pub memory: Box<[u8]>,
}

impl Ram {
    /// Creates a new [`Ram`] instance, allocating and initializing its memory to [0u8; 65536].
    pub fn new() -> Self {
        let (_, addr_bus_lo) = channel(0);
        let (_, addr_bus_hi) = channel(0);
        let (output_bus, data_bus) = channel(0);
        Self {
            addr_bus_lo,
            addr_bus_hi,
            data_bus,
            output_bus,
            write_enabled: false,
            memory: vec![0u8; 65536].into_boxed_slice(),
        }
    }

    /// Enables writing to memory. Memory may be modified next time `update` is called.
    pub fn write(&mut self) {
        self.write_enabled = true;
    }

    /// Disables writing to memory.
    pub fn no_write(&mut self) {
        self.write_enabled = false;
    }

    /// Routes the RAM module's output bus to its own data bus.
    pub fn loopback(&mut self) {
        self.data_bus = self.output_bus.subscribe();
    }

    /// Routes the given output bus to the RAM module's data bus.
    pub fn route_data(&mut self, output_bus: Receiver<u8>) {
        self.data_bus = output_bus;
    }

    /// Routes the given output bus to the lower 8 bits of the RAM module's address bus.
    pub fn route_addr_lo(&mut self, output_bus: Receiver<u8>) {
        self.addr_bus_lo = output_bus;
    }

    /// Routes the given output bus to the higher 8 bits of the RAM module's address bus.
    pub fn route_addr_hi(&mut self, output_bus: Receiver<u8>) {
        self.addr_bus_hi = output_bus;
    }

    /// Updates the RAM module's internal state, performing any writes and sending the value pointed to by the address bus along the output bus.
    pub fn update(&mut self) {
        let addr = u16::from_le_bytes([*self.addr_bus_lo.borrow(), *self.addr_bus_hi.borrow()]);
        if self.write_enabled {
            self.memory[addr as usize] = *self.data_bus.borrow();
        }
        self.output_bus.send_replace(self.memory[addr as usize]);
    }
}

impl Default for Ram {
    fn default() -> Self {
        Self::new()
    }
}
