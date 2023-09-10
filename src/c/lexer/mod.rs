use nom::{
    branch::*,
    bytes::complete::*,
    character::complete::{multispace0, one_of},
    combinator::*,
    multi::{many0, many1},
    sequence::{delimited, tuple},
    IResult,
};

use crate::asm::{Span, WithSpan};

use super::parser::ast::{Constant, Ident};

use super::{
    lexer::gen::{lex_keyword, lex_punctuator},
    CToken,
};

pub mod gen;

pub type SpanToken<'a> = WithSpan<'a, CToken<'a>>;

pub const NONDIGIT: &str = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
pub const DIGIT: &str = "0123456789";

pub fn lex_ident(inp: Span<'_>) -> IResult<Span<'_>, SpanToken<'_>> {
    map(
        recognize(tuple((
            one_of(NONDIGIT),
            many0(alt((one_of(NONDIGIT), one_of(DIGIT)))),
        ))),
        |span| WithSpan {
            span,
            item: CToken::Ident(Ident(span.fragment())),
        },
    )(inp)
}

pub fn lex_integer(inp: Span<'_>) -> IResult<Span<'_>, SpanToken<'_>> {
    map(recognize(many1(one_of(DIGIT))), |span: Span<'_>| WithSpan {
        span,
        item: CToken::Constant(Constant::Integer(span.parse::<u16>().unwrap())),
    })(inp)
}

pub fn lex_illegal(inp: Span<'_>) -> IResult<Span<'_>, SpanToken<'_>> {
    map(take(1usize), |span| WithSpan {
        span,
        item: CToken::Illegal,
    })(inp)
}

pub fn lex_token(inp: Span<'_>) -> IResult<Span<'_>, SpanToken<'_>> {
    alt((
        lex_punctuator,
        lex_keyword,
        lex_ident,
        lex_integer,
        lex_illegal,
    ))(inp)
}

pub fn lex_tokens(inp: Span<'_>) -> IResult<Span<'_>, Vec<SpanToken<'_>>> {
    many0(delimited(multispace0, lex_token, multispace0))(inp).map(|(s, res)| {
        (
            s,
            [
                &res[..],
                &[WithSpan {
                    span: s,
                    item: CToken::EOI,
                }][..],
            ]
            .concat(),
        )
    })
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
