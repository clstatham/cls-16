use nom::{
    branch::*,
    bytes::complete::*,
    combinator::*,
    multi::{many0, many1, separated_list1},
    sequence::{delimited, preceded, terminated, tuple},
    Err, Finish, IResult,
};
use thiserror::Error;

use super::common::*;

pub mod gen;
use self::gen::*;

#[derive(Debug, Error)]
pub enum ParseErrorKind {
    #[error("Extra tokens")]
    ExtraTokens,
    #[error("NomError: {0}")]
    NomError(String),
}

#[derive(Debug, Error)]
#[error("\nParseError at {line_num}:{col_num}: '{string_repr}': {source}\n{line}")]
pub struct ParseError {
    pub line_num: usize,
    pub col_num: usize,
    pub line: String,
    pub string_repr: String,
    #[source]
    pub source: ParseErrorKind,
}

impl ParseError {
    pub fn new(inp: &Token<'_>, source: ParseErrorKind) -> Self {
        Self {
            line_num: inp.line_num,
            col_num: inp.col_num,
            string_repr: inp.string_repr.to_owned(),
            line: inp.line.to_string(),
            source,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Struct(String),
    Union(String),
    Enum(String),
    Typedef(String),
    Pointer(Box<Type>),
    Array(Box<Type>, usize),
    Function(Box<Type>, Vec<Type>),
}

impl Type {
    pub fn sizeof(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::Char => 1,
            Type::Short => 2,
            Type::Int => 4,
            Type::Long => 8,
            Type::Float => 4,
            Type::Double => 8,
            Type::Signed => 4,
            Type::Unsigned => 4,
            Type::Struct(_) => 0,
            Type::Union(_) => 0,
            Type::Enum(_) => 4,
            Type::Typedef(_) => 0,
            Type::Pointer(_) => 8,
            Type::Array(ty, len) => ty.sizeof() * len,
            Type::Function(_, _) => 0,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstNode<'a> {
    // The type that this node evaluates to.
    pub typ: Option<Type>,
    // Whether this node is an rvalue or lvalue.
    pub rvalue: bool,
    pub ast: Box<Ast<'a>>,
}

impl<'a> AstNode<'a> {
    /// Creates a new AstNode with the given Ast and Type.
    ///
    /// The node will be assummed to be an lvalue, meaning that its result will be kept after the node is translated to assembly.
    pub fn new(ast: Ast<'a>, typ: Option<Type>) -> Self {
        Self {
            ast: Box::new(ast),
            typ,
            rvalue: false,
        }
    }

    /// Creates a new AstNode with the given Ast and Type.
    ///
    /// The node will be assummed to be an rvalue, meaning that its result will be discarded after the node is translated to assembly.
    pub fn new_rvalue(ast: Ast<'a>, typ: Option<Type>) -> Self {
        Self {
            ast: Box::new(ast),
            typ,
            rvalue: true,
        }
    }
}

// rewrite the above enum but with every variant having fields on AstNode instead of Box<Ast>
#[derive(Debug, PartialEq, Clone)]
pub enum Ast<'a> {
    // Leaf nodes
    Token(Token<'a>),
    Ident(Token<'a>),
    Integer(i64),
    StringLiteral(String),
    // Basic AST nodes
    Pointer(AstNode<'a>),
    // Compound AST nodes
    Program(Vec<AstNode<'a>>),
    FunctionDef {
        name: AstNode<'a>,
        params: Vec<AstNode<'a>>,
        body: AstNode<'a>,
    },
    Block(Vec<AstNode<'a>>),
    If {
        cond: AstNode<'a>,
        then: AstNode<'a>,
        els: Option<AstNode<'a>>,
    },
    While {
        cond: AstNode<'a>,
        body: AstNode<'a>,
    },
    For {
        init: AstNode<'a>,
        cond: AstNode<'a>,
        step: AstNode<'a>,
        body: AstNode<'a>,
    },
    DoWhile {
        body: AstNode<'a>,
        cond: AstNode<'a>,
    },
    Return(Option<AstNode<'a>>),

    // Expressions
    Binary {
        op: AstNode<'a>,
        lhs: AstNode<'a>,
        rhs: AstNode<'a>,
    },
    Unary {
        op: AstNode<'a>,
        rhs: AstNode<'a>,
    },
    Postfix {
        op: AstNode<'a>,
        lhs: AstNode<'a>,
    },
    Call {
        name: AstNode<'a>,
        args: Vec<AstNode<'a>>,
    },
    Index {
        name: AstNode<'a>,
        index: AstNode<'a>,
    },
    Member {
        name: AstNode<'a>,
        member: AstNode<'a>,
    },
    Cast {
        expr: AstNode<'a>,
    },
    Sizeof(AstNode<'a>),
    Declaration {
        decl: Vec<AstNode<'a>>,
    },
}

impl<'a> AstNode<'a> {
    pub fn parse(inp: Tokens<'a>) -> Result<Self> {
        let (inp, res) = Self::parse_program(inp).finish().map_err(|e| {
            Error::from(ParseError::new(
                &e.input.tok[0],
                ParseErrorKind::NomError(e.code.description().to_string()),
            ))
        })?;
        if !inp.tok.is_empty() {
            Err(Error::from(ParseError::new(
                inp.peek().unwrap(),
                ParseErrorKind::ExtraTokens,
            )))
        } else {
            log::trace!("Out Ast::parse");
            Ok(res)
        }
    }

    fn parse_program(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, res) = many0(Self::parse_external_declaration)(inp)?;
        log::trace!("Out Ast::parse_program");
        Ok((inp, AstNode::new(Ast::Program(res), None)))
    }

    fn parse_external_declaration(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, res) = alt((Self::parse_function_def, Self::parse_declaration))(inp)?;
        log::trace!("Out Ast::parse_external_declaration");
        Ok((inp, res))
    }

    fn parse_function_def(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, _) = Self::parse_type_specifier(inp)?;
        let (inp, name) = Self::parse_ident(inp)?;
        let (inp, _) = punc_oparen(inp)?;
        let (inp, params) = many0(Self::parse_declaration)(inp)?;
        let (inp, _) = punc_cparen(inp)?;
        let (inp, body) = Self::parse_compound_statement(inp)?;
        log::trace!("Out Ast::parse_function_def");
        Ok((
            inp,
            AstNode::new(Ast::FunctionDef { name, params, body }, None),
        ))
    }

    fn parse_ident(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, t) = verify(take(1usize), |t: &Tokens<'a>| {
            t.peek()
                .map(|t| matches!(t.variant, TokenVariant::Ident(_)))
                .unwrap_or(false)
        })(inp)?;
        log::trace!(
            "Out Ast::parse_ident with {:?}",
            t.peek().map(|t| &t.string_repr)
        );
        Ok((inp, AstNode::new(Ast::Ident(t.tok[0].clone()), None)))
    }

    fn parse_constant(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, t) = verify(take(1usize), |t: &Tokens<'a>| {
            t.peek()
                .map(|t| matches!(t.variant, TokenVariant::Integer(_)))
                .unwrap_or(false)
        })(inp)?;
        log::trace!("Out Ast::parse_constant");
        if let TokenVariant::Integer(val) = t.tok[0].variant {
            Ok((inp, AstNode::new_rvalue(Ast::Integer(val), Some(Type::Int))))
        } else {
            unreachable!()
        }
    }

    fn parse_string_literal(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, t) = verify(take(1usize), |t: &Tokens<'a>| {
            t.peek()
                .map(|t| matches!(t.variant, TokenVariant::String(_)))
                .unwrap_or(false)
        })(inp)?;
        log::trace!("Out Ast::parse_string_literal");
        if let TokenVariant::String(s) = &t.tok[0].variant {
            // let len = AstNode::new_rvalue(Ast::Integer(s.len() as i64), Some(Type::Int));
            Ok((
                inp,
                AstNode::new_rvalue(
                    Ast::StringLiteral(s.to_owned()),
                    Some(Type::Array(Box::new(Type::Char), s.len())),
                ),
            ))
        } else {
            unreachable!()
        }
    }

    fn parse_primary_expr(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        alt((
            Self::parse_ident,
            Self::parse_constant,
            Self::parse_string_literal,
            delimited(punc_oparen, Self::parse_expr, punc_cparen),
        ))(inp)
    }

    fn parse_postfix_expr(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, lhs) = Self::parse_primary_expr(inp)?;
        if let Ok((inp, _)) = punc_oparen(inp.clone()) {
            // call expr
            let (inp, args) = separated_list1(punc_comma, Self::parse_expr)(inp)?;
            let (inp, _) = punc_cparen(inp)?;
            log::trace!("Out Ast::parse_postfix_expr (call)");
            return Ok((
                inp,
                AstNode::new_rvalue(
                    Ast::Call {
                        name: lhs.to_owned(),
                        args,
                    },
                    lhs.typ,
                ),
            ));
        } else if let Ok((inp, _)) = punc_obrack(inp.clone()) {
            // index expr
            let (inp, index) = Self::parse_expr(inp)?;
            let (inp, _) = punc_cbrack(inp)?;
            log::trace!("Out Ast::parse_postfix_expr (index)");
            return Ok((
                inp,
                AstNode::new_rvalue(
                    Ast::Index {
                        name: lhs.to_owned(),
                        index,
                    },
                    lhs.typ,
                ),
            ));
        }
        let (inp, res) = many0(alt((
            tuple((punc_period, Self::parse_ident)),
            tuple((punc_rarrow, Self::parse_ident)),
            tuple((punc_plusplus, Self::parse_ident)),
            tuple((punc_minusminus, Self::parse_ident)),
        )))(inp)?;
        let res = res.into_iter().fold(lhs, |lhs, (op, rhs)| {
            let typ = match *op.ast {
                Ast::Token(Token {
                    variant: TokenVariant::Punctuator(punc),
                    ..
                }) => match punc {
                    Punctuator::Period => match lhs.typ.as_ref() {
                        Some(Type::Struct(_) | Type::Union(_)) => rhs.typ,
                        None => None,
                        _ => unreachable!(),
                    },
                    Punctuator::RArrow => match lhs.typ.as_ref() {
                        Some(Type::Pointer(ty)) => match **ty {
                            Type::Struct(_) | Type::Union(_) => rhs.typ,
                            _ => unreachable!(),
                        },
                        None => None,
                        _ => unreachable!(),
                    },
                    _ => None,
                },
                _ => None,
            };
            AstNode::new(Ast::Postfix { op, lhs }, typ)
        });
        log::trace!("Out Ast::parse_postfix_expr");
        Ok((inp, res))
    }

    fn parse_cast_expr(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        alt((
            Self::parse_unary_expr,
            map(
                tuple((
                    punc_oparen,
                    Self::parse_type_specifier,
                    punc_cparen,
                    Self::parse_cast_expr,
                )),
                |(_, ty, _, mut expr)| AstNode::new_rvalue(Ast::Cast { expr }, ty.typ),
            ),
        ))(inp)
    }

    fn parse_type_specifier(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, mut ty) = alt((
            kw_void,
            kw_char,
            kw_short,
            kw_int,
            kw_long,
            kw_float,
            kw_double,
            kw_signed,
            kw_unsigned,
            kw_struct,
            kw_union,
            kw_enum,
            // Self::parse_typedef_name,
        ))(inp)?;
        match *ty.ast {
            Ast::Token(Token {
                variant: TokenVariant::Keyword(kw),
                ..
            }) => {
                ty.typ = Some(match kw {
                    Keyword::Void => Type::Void,
                    Keyword::Char => Type::Char,
                    Keyword::Short => Type::Short,
                    Keyword::Int => Type::Int,
                    Keyword::Long => Type::Long,
                    Keyword::Float => Type::Float,
                    Keyword::Double => Type::Double,
                    Keyword::Signed => Type::Signed,
                    Keyword::Unsigned => Type::Unsigned,
                    Keyword::Struct => Type::Struct("".to_owned()),
                    Keyword::Union => Type::Union("".to_owned()),
                    Keyword::Enum => Type::Enum("".to_owned()),
                    _ => unreachable!(),
                });
            }
            _ => unreachable!(),
        };
        log::trace!("Out Ast::parse_type_specifier");
        Ok((inp, ty))
    }

    fn parse_unary_expr(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, res) = many0(alt((
            tuple((punc_ampersand, Self::parse_unary_expr)),
            tuple((punc_star, Self::parse_unary_expr)),
            tuple((punc_plusplus, Self::parse_unary_expr)),
            tuple((punc_minusminus, Self::parse_unary_expr)),
            tuple((punc_plus, Self::parse_unary_expr)),
            tuple((punc_minus, Self::parse_unary_expr)),
            tuple((punc_tilde, Self::parse_unary_expr)),
            tuple((punc_bang, Self::parse_unary_expr)),
            tuple((kw_sizeof, Self::parse_unary_expr)),
        )))(inp)?;
        let out = res.into_iter().fold(None, |lhs, (op, rhs)| {
            let typ = match *op.ast {
                Ast::Token(Token {
                    variant: TokenVariant::Keyword(kw),
                    ..
                }) => match kw {
                    Keyword::Sizeof => Some(Type::Int),
                    _ => unreachable!(),
                },
                Ast::Token(Token {
                    variant: TokenVariant::Punctuator(punc),
                    ..
                }) => match punc {
                    Punctuator::Ampersand => Some(Type::Pointer(Box::new(
                        rhs.typ.as_ref().unwrap().to_owned(),
                    ))),
                    Punctuator::Star => match rhs.typ.as_ref().unwrap() {
                        Type::Pointer(ty) => Some(*ty.to_owned()),
                        _ => unreachable!(),
                    },
                    _ => None,
                },
                _ => None,
            };
            let rhs = match lhs {
                None => rhs,
                Some(lhs) => AstNode::new(Ast::Unary { op, rhs: lhs }, typ),
            };
            Some(rhs)
        });
        if let Some(out) = out {
            log::trace!("Out Ast::parse_unary_expr");
            Ok((inp, out))
        } else {
            Self::parse_postfix_expr(inp)
        }
    }

    fn parse_multiplicative_expr(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, lhs) = Self::parse_cast_expr(inp)?;
        let (inp, res) = many0(alt((
            tuple((punc_star, Self::parse_cast_expr)),
            tuple((punc_fslash, Self::parse_cast_expr)),
            tuple((punc_percent, Self::parse_cast_expr)),
        )))(inp)?;
        let res = res.into_iter().fold(lhs, |lhs, (op, mut rhs)| {
            rhs.rvalue = true;
            AstNode::new_rvalue(
                Ast::Binary {
                    op,
                    lhs: lhs.to_owned(),
                    rhs,
                },
                lhs.typ,
            )
        });
        log::trace!("Out Ast::parse_multiplicative_expr");
        Ok((inp, res))
    }

    fn parse_additive_expr(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, lhs) = Self::parse_multiplicative_expr(inp)?;
        let (inp, res) = many0(alt((
            tuple((punc_plus, Self::parse_multiplicative_expr)),
            tuple((punc_minus, Self::parse_multiplicative_expr)),
        )))(inp)?;
        let res = res.into_iter().fold(lhs, |lhs, (op, mut rhs)| {
            rhs.rvalue = true;
            AstNode::new_rvalue(
                Ast::Binary {
                    op,
                    lhs: lhs.to_owned(),
                    rhs,
                },
                lhs.typ,
            )
        });
        log::trace!("Out Ast::parse_additive_expr");
        Ok((inp, res))
    }

    fn parse_shift_expr(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, lhs) = Self::parse_additive_expr(inp)?;
        let (inp, res) = many0(alt((
            tuple((punc_ltlt, Self::parse_additive_expr)),
            tuple((punc_gtgt, Self::parse_additive_expr)),
        )))(inp)?;
        let res = res.into_iter().fold(lhs, |lhs, (op, mut rhs)| {
            rhs.rvalue = true;
            AstNode::new_rvalue(
                Ast::Binary {
                    op,
                    lhs: lhs.to_owned(),
                    rhs,
                },
                lhs.typ,
            )
        });
        log::trace!("Out Ast::parse_shift_expr");
        Ok((inp, res))
    }

    fn parse_relational_expr(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, lhs) = Self::parse_shift_expr(inp)?;
        let (inp, res) = many0(alt((
            tuple((punc_lt, Self::parse_shift_expr)),
            tuple((punc_gt, Self::parse_shift_expr)),
            tuple((punc_lteq, Self::parse_shift_expr)),
            tuple((punc_gteq, Self::parse_shift_expr)),
        )))(inp)?;
        let res = res.into_iter().fold(lhs, |lhs, (op, mut rhs)| {
            rhs.rvalue = true;
            AstNode::new_rvalue(
                Ast::Binary {
                    op,
                    lhs: lhs.to_owned(),
                    rhs,
                },
                lhs.typ,
            )
        });
        log::trace!("Out Ast::parse_relational_expr");
        Ok((inp, res))
    }

    fn parse_equality_expr(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, lhs) = Self::parse_relational_expr(inp)?;
        let (inp, res) = many0(alt((
            tuple((punc_eqeq, Self::parse_relational_expr)),
            tuple((punc_bangeq, Self::parse_relational_expr)),
        )))(inp)?;
        let res = res.into_iter().fold(lhs, |lhs, (op, mut rhs)| {
            rhs.rvalue = true;
            AstNode::new_rvalue(
                Ast::Binary {
                    op,
                    lhs: lhs.to_owned(),
                    rhs,
                },
                lhs.typ,
            )
        });
        log::trace!("Out Ast::parse_equality_expr");
        Ok((inp, res))
    }

    fn parse_and_expr(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, lhs) = Self::parse_equality_expr(inp)?;
        let (inp, res) = many0(tuple((punc_ampersand, Self::parse_equality_expr)))(inp)?;
        let res = res.into_iter().fold(lhs, |lhs, (op, mut rhs)| {
            rhs.rvalue = true;
            AstNode::new_rvalue(
                Ast::Binary {
                    op,
                    lhs: lhs.to_owned(),
                    rhs,
                },
                lhs.typ,
            )
        });
        log::trace!("Out Ast::parse_and_expr");
        Ok((inp, res))
    }

    fn parse_exclusive_or_expr(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, lhs) = Self::parse_and_expr(inp)?;
        let (inp, res) = many0(tuple((punc_caret, Self::parse_and_expr)))(inp)?;
        let res = res.into_iter().fold(lhs, |lhs, (op, mut rhs)| {
            rhs.rvalue = true;
            AstNode::new_rvalue(
                Ast::Binary {
                    op,
                    lhs: lhs.to_owned(),
                    rhs,
                },
                lhs.typ,
            )
        });
        log::trace!("Out Ast::parse_exclusive_or_expr");
        Ok((inp, res))
    }

    fn parse_inclusive_or_expr(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, lhs) = Self::parse_exclusive_or_expr(inp)?;
        let (inp, res) = many0(tuple((punc_bar, Self::parse_exclusive_or_expr)))(inp)?;
        let res = res.into_iter().fold(lhs, |lhs, (op, mut rhs)| {
            rhs.rvalue = true;
            AstNode::new_rvalue(
                Ast::Binary {
                    op,
                    lhs: lhs.to_owned(),
                    rhs,
                },
                lhs.typ,
            )
        });
        log::trace!("Out Ast::parse_inclusive_or_expr");
        Ok((inp, res))
    }

    fn parse_logical_and_expr(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, lhs) = Self::parse_inclusive_or_expr(inp)?;
        let (inp, res) = many0(tuple((punc_andand, Self::parse_inclusive_or_expr)))(inp)?;
        let res = res.into_iter().fold(lhs, |lhs, (op, mut rhs)| {
            rhs.rvalue = true;
            AstNode::new_rvalue(
                Ast::Binary {
                    op,
                    lhs: lhs.to_owned(),
                    rhs,
                },
                lhs.typ,
            )
        });
        log::trace!("Out Ast::parse_logical_and_expr");
        Ok((inp, res))
    }

    fn parse_logical_or_expr(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, lhs) = Self::parse_logical_and_expr(inp)?;
        let (inp, res) = many0(tuple((punc_barbar, Self::parse_logical_and_expr)))(inp)?;
        let res = res.into_iter().fold(lhs, |lhs, (op, mut rhs)| {
            rhs.rvalue = true;
            AstNode::new_rvalue(
                Ast::Binary {
                    op,
                    lhs: lhs.to_owned(),
                    rhs,
                },
                lhs.typ,
            )
        });
        log::trace!("Out Ast::parse_logical_or_expr");
        Ok((inp, res))
    }

    fn parse_conditional_expr(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, cond) = Self::parse_logical_or_expr(inp)?;
        if let Ok((inp, res)) = tuple((
            punc_question,
            Self::parse_expr,
            punc_colon,
            Self::parse_expr,
        ))(inp.clone())
        {
            let ast = Ast::If {
                cond,
                then: res.1.to_owned(),
                els: Some(res.3),
            };
            log::trace!("Out Ast::parse_conditional_expr (ternary)");
            Ok((inp, AstNode::new_rvalue(ast, res.1.typ)))
        } else {
            log::trace!("Out Ast::parse_conditional_expr");
            Ok((inp, cond))
        }
    }

    fn parse_assignment_expr(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, lhs) = Self::parse_conditional_expr(inp)?;
        let (inp, res) = many0(alt((
            tuple((punc_equals, Self::parse_assignment_expr)),
            tuple((punc_stareq, Self::parse_assignment_expr)),
            tuple((punc_slasheq, Self::parse_assignment_expr)),
            tuple((punc_percenteq, Self::parse_assignment_expr)),
            tuple((punc_pluseq, Self::parse_assignment_expr)),
            tuple((punc_minuseq, Self::parse_assignment_expr)),
            tuple((punc_ltlteq, Self::parse_assignment_expr)),
            tuple((punc_gtgteq, Self::parse_assignment_expr)),
            tuple((punc_andeq, Self::parse_assignment_expr)),
            tuple((punc_careteq, Self::parse_assignment_expr)),
            tuple((punc_bareq, Self::parse_assignment_expr)),
        )))(inp)?;
        let res = res.into_iter().fold(lhs, |lhs, (op, mut rhs)| {
            rhs.rvalue = true;
            // assignment expressions are assumed to be lvalues by default
            AstNode::new(
                Ast::Binary {
                    op,
                    lhs: lhs.to_owned(),
                    rhs,
                },
                lhs.typ,
            )
        });
        log::trace!("Out Ast::parse_assignment_expr");
        Ok((inp, res))
    }

    fn parse_expr(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        Self::parse_assignment_expr(inp)
    }

    fn parse_declaration(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, ty) = Self::parse_declaration_specifiers(inp)?;
        let (inp, decl) = Self::parse_init_declarator_list(inp, &ty)?;
        let (inp, _) = punc_semicolon(inp)?;
        log::trace!("Out Ast::parse_declaration");
        Ok((inp, AstNode::new(Ast::Declaration { decl }, None)))
    }

    fn parse_declaration_specifiers(inp: Tokens<'a>) -> IResult<Tokens<'a>, Vec<Self>> {
        let (inp, mut ty) = many1(alt((
            kw_void,
            kw_char,
            kw_short,
            kw_int,
            kw_long,
            kw_float,
            kw_double,
            kw_signed,
            kw_unsigned,
            kw_struct,
            kw_union,
            kw_enum,
            // Self::parse_typedef_name,
        )))(inp)?;
        for t in ty.iter_mut() {
            match *t.ast {
                Ast::Token(Token {
                    variant: TokenVariant::Keyword(kw),
                    ..
                }) => {
                    t.typ = Some(match kw {
                        Keyword::Void => Type::Void,
                        Keyword::Char => Type::Char,
                        Keyword::Short => Type::Short,
                        Keyword::Int => Type::Int,
                        Keyword::Long => Type::Long,
                        Keyword::Float => Type::Float,
                        Keyword::Double => Type::Double,
                        Keyword::Signed => Type::Signed,
                        Keyword::Unsigned => Type::Unsigned,
                        Keyword::Struct => Type::Struct("".to_owned()),
                        Keyword::Union => Type::Union("".to_owned()),
                        Keyword::Enum => Type::Enum("".to_owned()),
                        _ => unreachable!(),
                    });
                }
                _ => unreachable!(),
            };
        }
        log::trace!("Out Ast::parse_declaration_specifiers");
        Ok((inp, ty))
    }

    fn parse_init_declarator_list(
        inp: Tokens<'a>,
        ty: &[AstNode<'a>],
    ) -> IResult<Tokens<'a>, Vec<Self>> {
        let mut decls = vec![];
        let (mut inp, decl) = Self::parse_init_declarator(inp, &ty[0])?;
        decls.push(decl);
        let mut more = true;
        while more {
            more = false;
            if let Ok((inp2, _)) = punc_comma(inp.to_owned()) {
                more = true;
                inp = inp2;
                let (inp2, decl) = Self::parse_init_declarator(inp, &ty[0])?;
                decls.push(decl);
                inp = inp2;
            }
        }
        log::trace!("Out Ast::parse_init_declarator_list");
        Ok((inp, decls))
    }

    fn parse_init_declarator(inp: Tokens<'a>, ty: &AstNode<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, decl) = Self::parse_declarator(inp, ty)?;
        let (inp, init) = opt(preceded(punc_equals, Self::parse_initializer))(inp)?;
        log::trace!("Out Ast::parse_init_declarator");
        Ok((inp, decl))
    }

    fn parse_declarator(inp: Tokens<'a>, ty: &AstNode<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, ptr) = many0(punc_star)(inp)?;
        let (inp, mut decl) = Self::parse_direct_declarator(inp, ty)?;
        decl.typ = ty.typ.to_owned();
        let (inp, mut decl) = if let Ok((inp, _)) = punc_obrack(inp.to_owned()) {
            let (inp, len) = Self::parse_expr(inp)?;
            if let Ast::Integer(len) = *len.ast.as_ref() {
                let (inp, _) = punc_cbrack(inp)?;
                decl.typ = Some(Type::Array(
                    Box::new(decl.typ.as_ref().unwrap().to_owned()),
                    len as usize,
                ));
                (inp, decl)
            } else {
                unreachable!()
            }
        } else {
            (inp, decl)
        };
        for _ in ptr {
            decl.typ = Some(Type::Pointer(Box::new(
                decl.typ.as_ref().unwrap().to_owned(),
            )));
        }
        log::trace!("Out Ast::parse_declarator");
        Ok((inp, decl))
    }

    fn parse_direct_declarator(inp: Tokens<'a>, ty: &AstNode<'a>) -> IResult<Tokens<'a>, Self> {
        alt((
            map(
                tuple((punc_oparen, |t| Self::parse_declarator(t, ty), punc_cparen)),
                |(_, decl, _)| decl,
            ),
            Self::parse_ident,
        ))(inp)
    }

    fn parse_initializer(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        alt((
            map(
                tuple((punc_obrack, Self::parse_initializer_list, punc_cbrack)),
                |(_, list, _)| list,
            ),
            Self::parse_assignment_expr,
        ))(inp)
    }

    fn parse_initializer_list(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, init) = Self::parse_initializer(inp)?;
        let (inp, inits) = many0(tuple((punc_comma, Self::parse_initializer)))(inp)?;
        let inits = inits.into_iter().fold(vec![init], |mut inits, (_, init)| {
            inits.push(init);
            inits
        });
        log::trace!("Out Ast::parse_initializer_list");
        Ok((inp, AstNode::new_rvalue(Ast::Block(inits), None)))
    }

    fn parse_statement(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        alt((
            // Self::parse_labeled_statement,
            Self::parse_compound_statement,
            Self::parse_expression_statement,
            Self::parse_selection_statement,
            Self::parse_iteration_statement,
            Self::parse_jump_statement,
        ))(inp)
    }

    fn parse_compound_statement(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, _) = punc_obrace(inp)?;
        let (inp, stmts) = many0(alt((Self::parse_declaration, Self::parse_statement)))(inp)?;
        let (inp, _) = punc_cbrace(inp)?;
        log::trace!("Out Ast::parse_compound_statement");
        Ok((inp, AstNode::new_rvalue(Ast::Block(stmts), None)))
    }

    fn parse_expression_statement(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, expr) = opt(Self::parse_expr)(inp)?;
        let (inp, _) = punc_semicolon(inp)?;
        log::trace!("Out Ast::parse_expression_statement");
        Ok((
            inp,
            expr.unwrap_or(AstNode::new_rvalue(Ast::Block(vec![]), None)),
        ))
    }

    fn parse_selection_statement(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, _) = kw_if(inp)?;
        let (inp, _) = punc_oparen(inp)?;
        let (inp, cond) = Self::parse_expr(inp)?;
        let (inp, _) = punc_cparen(inp)?;
        let (inp, then) = Self::parse_statement(inp)?;
        let (inp, els) = opt(preceded(kw_else, Self::parse_statement))(inp)?;
        log::trace!("Out Ast::parse_selection_statement");
        Ok((inp, AstNode::new_rvalue(Ast::If { cond, then, els }, None)))
    }

    fn parse_while_statement(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, _) = kw_while(inp)?;
        let (inp, _) = punc_oparen(inp)?;
        let (inp, cond) = Self::parse_expr(inp)?;
        let (inp, _) = punc_cparen(inp)?;
        let (inp, body) = Self::parse_statement(inp)?;
        log::trace!("Out Ast::parse_while_statement");
        Ok((inp, AstNode::new_rvalue(Ast::While { cond, body }, None)))
    }

    fn parse_for_statement(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, _) = kw_for(inp)?;
        let (inp, _) = punc_oparen(inp)?;
        let (inp, init) = opt(Self::parse_expr)(inp)?;
        let (inp, _) = punc_semicolon(inp)?;
        let (inp, cond) = opt(Self::parse_expr)(inp)?;
        let (inp, _) = punc_semicolon(inp)?;
        let (inp, step) = opt(Self::parse_expr)(inp)?;
        let (inp, _) = punc_cparen(inp)?;
        let (inp, body) = Self::parse_statement(inp)?;
        log::trace!("Out Ast::parse_for_statement");
        Ok((
            inp,
            AstNode::new_rvalue(
                Ast::For {
                    init: init.unwrap_or_else(|| AstNode::new_rvalue(Ast::Block(vec![]), None)),
                    cond: cond.unwrap_or_else(|| AstNode::new_rvalue(Ast::Block(vec![]), None)),
                    step: step.unwrap_or_else(|| AstNode::new_rvalue(Ast::Block(vec![]), None)),
                    body,
                },
                None,
            ),
        ))
    }

    fn parse_do_while_statement(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, _) = kw_do(inp)?;
        let (inp, body) = Self::parse_statement(inp)?;
        let (inp, _) = kw_while(inp)?;
        let (inp, _) = punc_oparen(inp)?;
        let (inp, cond) = Self::parse_expr(inp)?;
        let (inp, _) = punc_cparen(inp)?;
        let (inp, _) = punc_semicolon(inp)?;
        log::trace!("Out Ast::parse_do_while_statement");
        Ok((inp, AstNode::new_rvalue(Ast::DoWhile { body, cond }, None)))
    }

    fn parse_iteration_statement(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        alt((
            Self::parse_while_statement,
            Self::parse_for_statement,
            Self::parse_do_while_statement,
        ))(inp)
    }

    fn parse_jump_statement(inp: Tokens<'a>) -> IResult<Tokens<'a>, Self> {
        let (inp, _) = kw_return(inp)?;
        let (inp, expr) = opt(Self::parse_expr)(inp)?;
        let (inp, _) = punc_semicolon(inp)?;
        log::trace!("Out Ast::parse_jump_statement");
        Ok((inp, AstNode::new_rvalue(Ast::Return(expr), None)))
    }
}
