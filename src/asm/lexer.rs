use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_until},
    character::complete::{alphanumeric1, line_ending, multispace1},
    combinator::{map, map_res, value},
    multi::many0,
    sequence::{delimited, preceded, terminated},
    IResult,
};

use anyhow::{Error, Result};

use crate::plat::{Opcode, Register};

use super::{AsmError, Compound, Mnemonic, Span, Token};

impl Compound {
    pub fn lex(inp: Span) -> IResult<Span, Self> {
        alt((
            value(Self::Push, tag_no_case("push")),
            value(Self::Pop, tag_no_case("pop")),
        ))(inp)
    }
}

impl Opcode {
    pub fn lex(inp: Span) -> IResult<Span, Self> {
        alt((
            value(Self::Halt, tag_no_case("halt")),
            value(Self::Add, tag_no_case("add")),
            value(Self::Sub, tag_no_case("sub")),
            value(Self::And, tag_no_case("and")),
            value(Self::Or, tag_no_case("or")),
            value(Self::Xor, tag_no_case("xor")),
            value(Self::Not, tag_no_case("not")),
            value(Self::Shl, tag_no_case("shl")),
            value(Self::Shr, tag_no_case("shr")),
            value(Self::Stl, tag_no_case("stl")),
            value(Self::Sth, tag_no_case("sth")),
            value(Self::Ldl, tag_no_case("ldl")),
            value(Self::Ldh, tag_no_case("ldh")),
            value(Self::Ldi, tag_no_case("ldi")),
            value(Self::Jz, tag_no_case("jz")),
            value(Self::Printi, tag_no_case("printi")),
            value(Self::Printc, tag_no_case("printc")),
        ))(inp)
    }
}

impl Mnemonic {
    pub fn lex(inp: Span) -> IResult<Span, Self> {
        alt((
            map(Opcode::lex, Self::Regular),
            map(Compound::lex, Self::Compound),
        ))(inp)
    }
}

impl Register {
    pub fn lex(inp: Span) -> IResult<Span, Self> {
        alt((
            value(Self::R0, tag_no_case("r0")),
            value(Self::R1, tag_no_case("r1")),
            value(Self::R2, tag_no_case("r2")),
            value(Self::R3, tag_no_case("r3")),
            value(Self::R4, tag_no_case("r4")),
            value(Self::R5, tag_no_case("r5")),
            value(Self::R6, tag_no_case("r6")),
            value(Self::R7, tag_no_case("r7")),
            value(Self::R8, tag_no_case("r8")),
            value(Self::PC, tag_no_case("pc")),
            value(Self::SP, tag_no_case("sp")),
            value(Self::FL, tag_no_case("fl")),
        ))(inp)
    }
}

pub fn lex_immediate(inp: Span) -> IResult<Span, u16> {
    alt((
        map_res(
            preceded(tag("$0x"), nom::character::complete::hex_digit1),
            |x: Span| u16::from_str_radix(x.fragment(), 16),
        ),
        map_res(
            preceded(tag("$"), nom::character::complete::digit1),
            |x: Span| x.fragment().parse::<u16>(),
        ),
    ))(inp)
}

pub fn lex_label(inp: Span) -> IResult<Span, &str> {
    map(terminated(alphanumeric1, tag(":")), |s: Span| *s.fragment())(inp)
}

pub fn lex_comment(inp: Span) -> IResult<Span, Span> {
    delimited(tag(";"), take_until("\n"), line_ending)(inp)
}

pub fn lex_comment_or_whitespace(inp: Span) -> IResult<Span, Span> {
    alt((multispace1, lex_comment))(inp)
}

impl<'a> Token<'a> {
    pub fn lex(inp: Span<'a>) -> IResult<Span, Self> {
        alt((
            map(Mnemonic::lex, Self::Mnemonic),
            map(lex_label, Self::Label),
            map(lex_immediate, Self::Immediate),
            map(Register::lex, Self::Register),
        ))(inp)
    }
}

pub fn lex_program(program: &str) -> Result<Vec<Token<'_>>> {
    let span = Span::new_extra(program, program);
    let (garbage, toks) = many0(delimited(
        many0(lex_comment_or_whitespace),
        Token::lex,
        many0(lex_comment_or_whitespace),
    ))(span)
    .map_err(|e| {
        let span = match e {
            nom::Err::Error(e) => e.input,
            nom::Err::Failure(e) => e.input,
            nom::Err::Incomplete(_) => span,
        };
        Error::from(AsmError::Syntax {
            loc: (span.location_line() as usize, span.get_utf8_column()),
            span: span.fragment().to_string(),
        })
    })?;
    if !garbage.is_empty() {
        Err(AsmError::FoundGarbage {
            loc: (garbage.location_line() as usize, span.get_utf8_column()),
            span: garbage.fragment().to_string(),
        }
        .into())
    } else {
        Ok(toks)
    }
}

#[cfg(test)]
mod tests {
    use crate::asm::lexer::lex_program;

    #[test]
    fn test_lex_program() {
        let prog = "
start:
    ; im a comment!
    ldi     r1 $0x1337
    printi  r1
    halt
";
        dbg!(lex_program(prog).unwrap());
    }
}
