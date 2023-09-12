use std::cell::RefCell;

use anyhow::Result;
use easy_repl::{command, repl::LoopStatus, CommandStatus, Repl};

use super::emulator::{EmuState, Emulator};

pub struct Debugger<'b, 'a> {
    pub emu: RefCell<&'b mut Emulator<'a>>,
}

impl<'b, 'a> Debugger<'b, 'a> {
    pub fn new(emu: &'b mut Emulator<'a>) -> Self {
        Self {
            emu: RefCell::new(emu),
        }
    }

    pub fn repl(&self) -> Result<()> {
        let mut repl = Repl::builder()
            .description("CLS-16 Debug REPL")
            .add(
                "c",
                command! {
                    "Continue execution",
                    () => || {
                        self.emu.borrow_mut().state = EmuState::Continue;
                        Ok(CommandStatus::Quit)
                    }
                },
            )
            .add(
                "s",
                command! {
                    "Step one instruction",
                    () => || {
                        self.emu.borrow_mut().step_instr()?;
                        Ok(CommandStatus::Done)
                    }
                },
            )
            .add(
                "bt",
                command! {
                    "Print last X instructions executed",
                    (x:usize) => |x| {
                        let emu = self.emu.borrow();
                        for instr in emu.instr_history.iter().rev().take(x).rev() {
                            eprintln!("{}", instr);
                        }
                        Ok(CommandStatus::Done)
                    }
                },
            )
            .add(
                "pr",
                command! {
                    "Print the value of all registers",
                    () => || {
                        let emu = self.emu.borrow();
                        macro_rules! print_gpreg {
                            ($reg:ident) => {
                                let val = emu.registers.$reg.value();
                                eprintln!("{}={:08X}", stringify!($reg), val);
                            };
                        }
                        macro_rules! print_specialreg {
                            ($reg:ident) => {
                                let val = emu.registers.$reg.value;
                                eprintln!("{}={:08X}", stringify!($reg), val);
                            };
                        }
                        print_gpreg!(r1);
                        print_gpreg!(r2);
                        print_gpreg!(r3);
                        print_gpreg!(r4);
                        print_gpreg!(r5);
                        print_gpreg!(r6);
                        print_gpreg!(sp);
                        print_gpreg!(fp);
                        print_specialreg!(pc);
                        print_specialreg!(il);
                        print_specialreg!(ih);
                        print_specialreg!(fl);
                        Ok(CommandStatus::Done)
                    }
                },
            )
            .add(
                "peek",
                command! {
                    "Peek a value from memory",
                    (addr:String) => |addr: String| {
                        let emu = self.emu.borrow();
                        let addr = if let Ok(addr) = addr.parse::<u16>() {
                            addr
                        } else {
                            u16::from_str_radix(&addr, 16).unwrap()
                        };
                        let val = emu.ram.memory[addr as usize];
                        eprintln!("{}={:08X}", addr, val);
                        Ok(CommandStatus::Done)
                    }
                },
            )
            .add(
                "halt",
                command! {
                    "Halt execution",
                    () => || {
                        self.emu.borrow_mut().state = EmuState::Halt;
                        Ok(CommandStatus::Quit)
                    }
                },
            )
            .build()?;
        eprintln!("CLS-16 Debug REPL");
        'repl: loop {
            eprintln!();
            {
                let emu = self.emu.borrow();
                if let Some(instr) = emu.instr_history.back() {
                    eprintln!(
                        "Current instruction:\n[pc={:08X}] --> {}",
                        emu.registers.pc.value, instr
                    );
                }
            }

            let status = repl.next()?;
            if let LoopStatus::Break = status {
                break 'repl;
            }
            {
                let emu = self.emu.borrow();
                if let EmuState::Halt = emu.state {
                    break 'repl;
                }
            }
        }
        Ok(())
    }
}
