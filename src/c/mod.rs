//! The C compiler module for CLS-16.

use std::ops::{Range, RangeFrom, RangeFull, RangeTo};

use nom::{InputIter, InputLength, InputTake};
use thiserror::Error;

use crate::asm::{Span, WithSpan};

use self::{
    lexer::gen::{Keyword, Punctuator},
    parser::ast::{Constant, Ident},
};

pub mod compiler;
pub mod lexer;
pub mod parser;

#[derive(Debug, Error)]
pub enum CompError {
    #[error("on line {}: syntax error: {}", .loc, .span)]
    Syntax { loc: usize, span: String },
    #[error("on line {}: found garbage: {}", .loc, .span)]
    FoundGarbage { loc: usize, span: String },
    #[error("on line {0}: unexpected token: {1}")]
    UnexpectedToken(usize, String),
    #[error("duplicate symbol found: {0}")]
    DuplicateSymbol(String),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CToken<'a> {
    Illegal,
    EOI,
    Ident(Ident<'a>),
    Constant(Constant),
    String,
    Keyword(Keyword),
    Punctuator(Punctuator),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Tokens<'a> {
    pub tok: &'a [WithSpan<'a, CToken<'a>>],
    pub start: usize,
    pub end: usize,
}

impl<'a> Tokens<'a> {
    pub fn new(tok: &'a [WithSpan<'a, CToken<'a>>]) -> Self {
        Self {
            tok,
            start: 0,
            end: tok.len(),
        }
    }

    pub fn first_span(self) -> Span<'a> {
        self.tok[0].span
    }

    pub fn first_into<T: 'a>(self, t: T) -> WithSpan<'a, T> {
        WithSpan {
            span: self.first_span(),
            item: t,
        }
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

impl<'a> InputLength for CToken<'a> {
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
    type Item = &'a WithSpan<'a, CToken<'a>>;
    type Iter = std::iter::Enumerate<std::slice::Iter<'a, WithSpan<'a, CToken<'a>>>>;
    type IterElem = std::slice::Iter<'a, WithSpan<'a, CToken<'a>>>;

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
