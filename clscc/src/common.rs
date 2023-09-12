use std::ops::{Range, RangeFrom, RangeFull, RangeTo};

pub use anyhow::{anyhow, Context, Error, Result};
use nom::{InputIter, InputLength, InputTake};

pub type Span<'a> = nom_locate::LocatedSpan<&'a str, &'a str>;

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'a> {
    pub variant: TokenVariant,
    pub string_repr: &'a str,
    pub line: &'a str,
    pub line_num: usize,
    pub col_num: usize,
}

impl<'a> Token<'a> {
    pub fn new(span: Span<'a>, variant: TokenVariant) -> Self {
        Self {
            variant,
            string_repr: span.fragment(),
            line: span
                .extra
                .lines()
                .nth(span.location_line() as usize - 1)
                .unwrap(),
            line_num: span.location_line() as usize,
            col_num: span.get_utf8_column(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Tokens<'a> {
    pub tok: &'a [Token<'a>],
    pub start: usize,
    pub end: usize,
}

impl<'a> Tokens<'a> {
    pub fn new(tok: &'a [Token<'a>]) -> Self {
        Self {
            tok,
            start: 0,
            end: tok.len(),
        }
    }

    pub fn peek(&self) -> Option<&Token<'a>> {
        self.tok.get(0)
    }

    pub fn peek_next(&self) -> Option<&Token<'a>> {
        self.tok.get(1)
    }
}

impl<'a> InputLength for Tokens<'a> {
    fn input_len(&self) -> usize {
        self.tok.len()
    }
}

impl<'a> InputTake for Tokens<'a> {
    fn take(&self, count: usize) -> Self {
        Self {
            tok: &self.tok[..count],
            start: 0,
            end: count,
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (pre, suf) = self.tok.split_at(count);
        let first = Self {
            tok: pre,
            start: 0,
            end: pre.len(),
        };
        let second = Self {
            tok: suf,
            start: 0,
            end: suf.len(),
        };
        (second, first)
    }
}

impl<'a> InputLength for Token<'a> {
    fn input_len(&self) -> usize {
        1
    }
}

impl<'a> nom::Slice<Range<usize>> for Tokens<'a> {
    fn slice(&self, range: Range<usize>) -> Self {
        Self {
            tok: self.tok.slice(range.clone()),
            start: self.start + range.start,
            end: self.start + range.end,
        }
    }
}

impl<'a> nom::Slice<RangeTo<usize>> for Tokens<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a> nom::Slice<RangeFrom<usize>> for Tokens<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.end - self.start)
    }
}

impl<'a> nom::Slice<RangeFull> for Tokens<'a> {
    fn slice(&self, _: RangeFull) -> Self {
        Self {
            tok: self.tok,
            start: self.start,
            end: self.end,
        }
    }
}

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a Token<'a>;
    type Iter = std::iter::Enumerate<std::slice::Iter<'a, Token<'a>>>;
    type IterElem = std::slice::Iter<'a, Token<'a>>;

    fn iter_indices(&self) -> Self::Iter {
        self.tok.iter().enumerate()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.tok.iter()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tok.iter().position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        if self.tok.len() >= count {
            Ok(count)
        } else {
            Err(nom::Needed::Unknown)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenVariant {
    Illegal,
    Keyword(Keyword),
    Punctuator(Punctuator),
    Ident(String),
    Integer(i64),
    Float(f64),
    Char(u8),
    String(String),
    Eof,
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

    // added for CLS-16
    Printi,
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
