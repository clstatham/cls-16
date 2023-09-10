#![cfg_attr(doc, warn(missing_docs))]
#![doc = include_str!("../README.md")]

use std::path::PathBuf;

use anyhow::Result;
use asm::Assembler;
use c::compiler::compile;
use clap::{Parser, Subcommand};
use emu::emulator::Emulator;
use simplelog::*;

pub mod asm;
pub mod c;
pub mod emu;
pub mod plat;

#[derive(Parser)]
struct AssembleArgs {
    #[clap(help = "Path to the assembly source file to assemble")]
    input: PathBuf,
}

#[derive(Parser)]
struct CompileArgs {
    #[clap(help = "Path to the C source file to compile")]
    input: PathBuf,
    #[clap(short, long, help = "Emit assembly code instead of a binary")]
    emit_asm: bool,
}

#[derive(Parser)]
struct RunArgs {
    #[clap(help = "Path to the CLS-16 binary to run")]
    input: PathBuf,
    #[clap(short, long, help = "Clockrate to run the emulator at")]
    clockrate: Option<u32>,
}

#[derive(Subcommand)]
enum Command {
    #[clap(about = "Assemble a .s file into a .cls16 executable")]
    Assemble(AssembleArgs),
    #[clap(about = "Compile a .c file into a .cls16 executable")]
    Compile(CompileArgs),
    #[clap(about = "Run a .cls16 executable")]
    Run(RunArgs),
}

#[derive(Parser)]
struct Args {
    #[clap(subcommand)]
    command: Command,
}

fn main() -> Result<()> {
    CombinedLogger::init(vec![TermLogger::new(
        #[cfg(debug_assertions)]
        LevelFilter::Debug,
        #[cfg(not(debug_assertions))]
        LevelFilter::Info,
        Config::default(),
        TerminalMode::Mixed,
        ColorChoice::Auto,
    )])?;

    let args = Args::parse();
    match args.command {
        Command::Compile(args) => {
            let c_program = std::fs::read_to_string(&args.input)?;
            let program = compile(&c_program)?;
            if args.emit_asm {
                let output = args.input.with_extension("s");
                std::fs::write(output, &program)?;
            }
            let program = Assembler.assemble(&program)?;
            let output = args.input.with_extension("cls16");
            std::fs::write(output, program)?;
        }
        Command::Assemble(args) => {
            let asm_program = std::fs::read_to_string(&args.input)?;
            let program = Assembler.assemble(&asm_program)?;
            let output = args.input.with_extension("cls16");
            std::fs::write(output, program)?;
        }
        Command::Run(args) => {
            let program = std::fs::read(&args.input)?;
            let mut emu = Emulator::new(&program, args.clockrate.unwrap_or(2_000_000).into())?;
            emu.cont()?;
        }
    }
    Ok(())
}
