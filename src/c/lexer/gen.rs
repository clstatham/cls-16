use crate::asm::{Span, WithSpan};
use crate::c::CToken;
use nom::branch::*;
use nom::bytes::complete::*;
use nom::combinator::*;
use nom::IResult;

// This is inspired / taken from `monkey-rust`
// https://github.com/Rydgel/monkey-rust/blob/22976ecf97f6b3aa007ba2b511fc9539d7940e13/lib/lexer/mod.rs#L16
macro_rules! syntax {
    ($func:ident, $tag:literal, $typ:ident, $tok:ident) => {
        pub fn $func(s: Span<'_>) -> IResult<Span<'_>, WithSpan<'_, CToken<'_>>> {
            map(tag($tag), |s| WithSpan {
                span: s,
                item: CToken::$typ($typ::$tok),
            })(s)
        }
    };
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    Auto,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    Float,
    For,
    Goto,
    If,
    Inline,
    Int,
    Long,
    Register,
    Restrict,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Punctuator {
    OBrack,
    CBrack,
    OParen,
    CParen,
    OBrace,
    CBrace,
    Period,
    RArrow,
    PlusPlus,
    MinusMinus,
    Ampersand,
    Star,
    Plus,
    Minus,
    Tilde,
    Bang,
    FSlash,
    Percent,
    LtLt,
    GtGt,
    Lt,
    Gt,
    LtEq,
    GtEq,
    EqEq,
    BangEq,
    Caret,
    Bar,
    AndAnd,
    BarBar,
    Question,
    Colon,
    Semicolon,
    Ellipsis,
    Equals,
    StarEq,
    SlashEq,
    PercentEq,
    PlusEq,
    MinusEq,
    LtLtEq,
    GtGtEq,
    AndEq,
    CaretEq,
    BarEq,
    Comma,
    Hash,
    HashHash,
}

syntax! { kw_auto, "auto", Keyword, Auto }
syntax! { kw_break, "break", Keyword, Break }
syntax! { kw_case, "case", Keyword, Case }
syntax! { kw_char, "char", Keyword, Char }
syntax! { kw_const, "const", Keyword, Const }
syntax! { kw_continue, "continue", Keyword, Continue }
syntax! { kw_default, "default", Keyword, Default }
syntax! { kw_do, "do", Keyword, Do }
syntax! { kw_double, "double", Keyword, Double }
syntax! { kw_else, "else", Keyword, Else }
syntax! { kw_enum, "enum", Keyword, Enum }
syntax! { kw_extern, "extern", Keyword, Extern }
syntax! { kw_float, "float", Keyword, Float }
syntax! { kw_for, "for", Keyword, For }
syntax! { kw_goto, "goto", Keyword, Goto }
syntax! { kw_if, "if", Keyword, If }
syntax! { kw_inline, "inline", Keyword, Inline }
syntax! { kw_int, "int", Keyword, Int }
syntax! { kw_long, "long", Keyword, Long }
syntax! { kw_register, "register", Keyword, Register }
syntax! { kw_restrict, "restrict", Keyword, Restrict }
syntax! { kw_return, "return", Keyword, Return }
syntax! { kw_short, "short", Keyword, Short }
syntax! { kw_signed, "signed", Keyword, Signed }
syntax! { kw_sizeof, "sizeof", Keyword, Sizeof }
syntax! { kw_static, "static", Keyword, Static }
syntax! { kw_struct, "struct", Keyword, Struct }
syntax! { kw_switch, "switch", Keyword, Switch }
syntax! { kw_typedef, "typedef", Keyword, Typedef }
syntax! { kw_union, "union", Keyword, Union }
syntax! { kw_unsigned, "unsigned", Keyword, Unsigned }
syntax! { kw_void, "void", Keyword, Void }
syntax! { kw_volatile, "volatile", Keyword, Volatile }
syntax! { kw_while, "while", Keyword, While }

syntax! { punc_obrack, "[", Punctuator, OBrack }
syntax! { punc_cbrack, "]", Punctuator, CBrack }
syntax! { punc_oparen, "(", Punctuator, OParen }
syntax! { punc_cparen, ")", Punctuator, CParen }
syntax! { punc_obrace, "{", Punctuator, OBrace }
syntax! { punc_cbrace, "}", Punctuator, CBrace }
syntax! { punc_period, ".", Punctuator, Period }
syntax! { punc_rarrow, "->", Punctuator, RArrow }
syntax! { punc_plusplus, "++", Punctuator, PlusPlus }
syntax! { punc_minusminus, "--", Punctuator, MinusMinus }
syntax! { punc_ampersand, "&", Punctuator, Ampersand }
syntax! { punc_star, "*", Punctuator, Star }
syntax! { punc_plus, "+", Punctuator, Plus }
syntax! { punc_minus, "-", Punctuator, Minus }
syntax! { punc_tilde, "~", Punctuator, Tilde }
syntax! { punc_bang, "!", Punctuator, Bang }
syntax! { punc_fslash, "/", Punctuator, FSlash }
syntax! { punc_percent, "%", Punctuator, Percent }
syntax! { punc_ltlt, "<<", Punctuator, LtLt }
syntax! { punc_gtgt, ">>", Punctuator, GtGt }
syntax! { punc_lt, "<", Punctuator, Lt }
syntax! { punc_gt, ">", Punctuator, Gt }
syntax! { punc_lteq, "<=", Punctuator, LtEq }
syntax! { punc_gteq, ">=", Punctuator, GtEq }
syntax! { punc_eqeq, "==", Punctuator, EqEq }
syntax! { punc_bangeq, "!=", Punctuator, BangEq }
syntax! { punc_caret, "^", Punctuator, Caret }
syntax! { punc_bar, "|", Punctuator, Bar }
syntax! { punc_andand, "&&", Punctuator, AndAnd }
syntax! { punc_barbar, "||", Punctuator, BarBar }
syntax! { punc_question, "?", Punctuator, Question }
syntax! { punc_colon, ":", Punctuator, Colon }
syntax! { punc_semicolon, ";", Punctuator, Semicolon }
syntax! { punc_ellipsis, "...", Punctuator, Ellipsis }
syntax! { punc_equals, "=", Punctuator, Equals }
syntax! { punc_stareq, "*=", Punctuator, StarEq }
syntax! { punc_slasheq, "/=", Punctuator, SlashEq }
syntax! { punc_percenteq, "%=", Punctuator, PercentEq }
syntax! { punc_pluseq, "+=", Punctuator, PlusEq }
syntax! { punc_minuseq, "-=", Punctuator, MinusEq }
syntax! { punc_ltlteq, "<<=", Punctuator, LtLtEq }
syntax! { punc_gtgteq, ">>=", Punctuator, GtGtEq }
syntax! { punc_andeq, "&=", Punctuator, AndEq }
syntax! { punc_careteq, "^=", Punctuator, CaretEq }
syntax! { punc_bareq, "|=", Punctuator, BarEq }
syntax! { punc_comma, ",", Punctuator, Comma }
syntax! { punc_hash, "#", Punctuator, Hash }
syntax! { punc_hashhash, "##", Punctuator, HashHash }

pub fn lex_keyword(inp: Span<'_>) -> IResult<Span<'_>, WithSpan<'_, CToken<'_>>> {
    alt((
        alt((
            kw_auto,
            kw_break,
            kw_case,
            kw_char,
            kw_const,
            kw_continue,
            kw_default,
            kw_do,
            kw_double,
            kw_else,
            kw_enum,
            kw_extern,
            kw_float,
            kw_for,
            kw_goto,
            kw_if,
            kw_inline,
            kw_int,
            kw_long,
        )),
        alt((
            kw_register,
            kw_restrict,
            kw_return,
            kw_short,
            kw_signed,
            kw_sizeof,
            kw_static,
            kw_struct,
            kw_switch,
            kw_typedef,
            kw_union,
            kw_unsigned,
            kw_void,
            kw_volatile,
            kw_while,
        )),
    ))(inp)
}

pub fn lex_punctuator(inp: Span<'_>) -> IResult<Span<'_>, WithSpan<'_, CToken<'_>>> {
    alt((
        alt((
            punc_obrack,
            punc_cbrack,
            punc_oparen,
            punc_cparen,
            punc_obrace,
            punc_cbrace,
            punc_period,
            punc_rarrow,
            punc_plusplus,
            punc_minusminus,
            punc_ampersand,
            punc_star,
            punc_plus,
            punc_minus,
            punc_tilde,
            punc_bang,
            punc_fslash,
            punc_percent,
            punc_ltlt,
        )),
        alt((
            punc_gtgt,
            punc_lt,
            punc_gt,
            punc_lteq,
            punc_gteq,
            punc_eqeq,
            punc_bangeq,
            punc_caret,
            punc_bar,
            punc_andand,
            punc_barbar,
            punc_question,
            punc_colon,
            punc_semicolon,
            punc_ellipsis,
            punc_equals,
            punc_stareq,
            punc_slasheq,
            punc_percenteq,
            punc_pluseq,
        )),
        alt((
            punc_minuseq,
            punc_ltlteq,
            punc_gtgteq,
            punc_andeq,
            punc_careteq,
            punc_bareq,
            punc_comma,
            punc_hash,
            punc_hashhash,
        )),
    ))(inp)
}
