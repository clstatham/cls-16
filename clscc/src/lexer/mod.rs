use std::num::ParseIntError;

use nom::{
    branch::*,
    bytes::complete::*,
    character::complete::{multispace0, one_of},
    combinator::*,
    multi::{many0, many1},
    sequence::{delimited, tuple},
    Finish, IResult,
};
use thiserror::Error;

use crate::common::*;

use self::gen::{lex_keyword, lex_punctuator};

pub mod gen;

#[derive(Debug, Error)]
pub enum LexErrorKind {
    #[error("ParseIntError: {0}")]
    ParseIntError(ParseIntError),
    #[error("NomError: {0}")]
    NomError(String),
}

#[derive(Debug, Error)]
#[error("LexError: {source}")]
pub struct LexError {
    pub line_num: usize,
    pub col_num: usize,
    pub line: String,
    #[source]
    pub source: LexErrorKind,
}

impl LexError {
    pub fn new(inp: Span<'_>, source: LexErrorKind) -> Self {
        Self {
            line_num: inp.location_line() as usize,
            col_num: inp.get_utf8_column(),
            line: inp
                .extra
                .lines()
                .nth(inp.location_line() as usize - 1)
                .unwrap()
                .to_string(),
            source,
        }
    }
}

pub const NONDIGIT: &str = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
pub const DIGIT: &str = "0123456789";

pub fn lex_ident(inp: Span<'_>) -> IResult<Span<'_>, Token<'_>> {
    map(
        recognize(tuple((
            one_of(NONDIGIT),
            many0(alt((one_of(NONDIGIT), one_of(DIGIT)))),
        ))),
        |span: Span<'_>| Token::new(span, TokenVariant::Ident(span.fragment().to_string())),
    )(inp)
}

pub fn lex_integer(inp: Span<'_>) -> IResult<Span<'_>, Token<'_>> {
    map_res(recognize(many1(one_of(DIGIT))), |span: Span<'_>| {
        Ok::<_, ParseIntError>(Token::new(span, TokenVariant::Integer(span.parse()?)))
    })(inp)
}

pub fn lex_illegal(inp: Span<'_>) -> IResult<Span<'_>, Token<'_>> {
    map(take(1usize), |span| Token::new(span, TokenVariant::Illegal))(inp)
}

pub fn lex_token(inp: Span<'_>) -> IResult<Span<'_>, Token<'_>> {
    alt((
        lex_punctuator,
        lex_keyword,
        lex_ident,
        lex_integer,
        lex_illegal,
    ))(inp)
}

pub fn lex_tokens(inp: Span<'_>) -> IResult<Span<'_>, Vec<Token<'_>>> {
    many0(delimited(multispace0, lex_token, multispace0))(inp)
}

pub fn lex(inp: &str) -> Result<Vec<Token<'_>>> {
    let inp = Span::new_extra(inp, inp);
    let (_, toks) = lex_tokens(inp)
        .finish()
        .map_err(|e| LexError::new(e.input, LexErrorKind::NomError(e.to_string())))?;
    Ok(toks)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_1() {
        let inp = "int main() {
            return 1 + 2;
        }";
        let inp = Span::new_extra(inp, inp);
        let (_, toks) = lex_tokens(inp).unwrap();
        dbg!(toks);
    }
}
