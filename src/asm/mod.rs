//! The assembler module for CLS-16.

use nom_locate::LocatedSpan;
use thiserror::Error;

use crate::plat::{Opcode, Register};

pub mod assembler;
pub mod lexer;

pub use assembler::Assembler;

pub type Span<'a> = LocatedSpan<&'a str, &'a str>;

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct WithSpan<'a, T> {
    pub span: Span<'a>,
    pub item: T,
}

/// An error for the assembler module of CLS-16.
#[derive(Debug, Error)]
pub enum AsmError {
    #[error("on line {}: syntax error", .loc.0)]
    Syntax { loc: (usize, usize), span: String },
    #[error("on line {}: found garbage: {}", .loc.0, .span)]
    FoundGarbage { loc: (usize, usize), span: String },
    #[error("on line {}: invalid format for instruction", .loc)]
    InvalidInstruction { loc: usize },
    #[error("on line {0}: unexpected token: {1}")]
    UnexpectedToken(usize, String),
    #[error("on line {0}: duplicate label found: {1}")]
    DuplicateLabel(usize, String),
    #[error("maximum linker iterations reached")]
    MaxLinkerIters,
    #[error("undefined reference to symbols: {0:?}")]
    UndefinedReferences(Vec<String>),
}

/// An assembly language token. Output for the lexer, input for the parser.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AsmToken<'a> {
    /// A [Mnemonic].
    Mnemonic(Mnemonic),
    /// A label marking a location (address) in the code.
    Label(&'a str),
    /// An immediate value, preceded with a `$`
    Immediate(u16),
    /// A [Register] mnemonic.
    Register(Register),
    /// `\n`
    Newline,
    /// End of file.
    Eof,
}

/// A superset of [Opcode], containing all of the "regular" ones that translate directly to machine code,
/// as well as [Compound] instructions that require more than one machine instruction cycle to execute.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mnemonic {
    /// A regular operation that can be directly translated to machine code.
    Regular(Opcode),
    /// A compound operation that requires multiple instructions to actually implement.
    Compound(Compound),
}

/// Compound instructions that require more than one machine instruction cycle to execute.
/// These are only valid as syntactic sugar in assembly listings, and may not appear if one
/// were to disassemble a CLS-16 binary object.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Compound {
    /// Pushes the value in a register to the top of the stack.
    ///
    /// Equivalent to:
    /// ```text
    /// stl     sp regA
    /// sub     sp $1
    /// sth     sp regA
    /// sub     sp $1
    /// ```
    Push,
    /// Pops a value off the top of the stack and into a register.
    ///
    /// Equivalent to:
    /// ```text
    /// add     sp $1
    /// ldh     regA sp
    /// add     sp $1
    /// ldl     regA sp
    /// ```
    Pop,
}
