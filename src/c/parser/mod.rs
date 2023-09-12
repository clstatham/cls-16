use nom::{
    branch::*,
    bytes::complete::*,
    combinator::*,
    error::{Error, ErrorKind},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, terminated, tuple},
    Err, IResult,
};

pub mod ast;
pub mod gen;

use crate::asm::WithSpan;

use self::ast::*;
use self::gen::*;

use super::{
    lexer::gen::{Keyword, Punctuator},
    CToken, Tokens,
};

#[doc(hidden)]
macro_rules! tag_impl_no_lt {
    ($($tok:ident)*) => {
        $(
            impl<'a> $tok {
                pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
                    let (rest, one) = take(1usize)(inp)?;
                    if one.tok.is_empty() {
                        Err(Err::Error(Error::new(inp, ErrorKind::Tag)))
                    } else {
                        match one.tok[0].item {
                            CToken::$tok(tok) => Ok((rest, WithSpan { span: one.tok[0].span, item: tok })),
                            _ => Err(Err::Error(Error::new(inp, ErrorKind::Tag))),
                        }
                    }
                }
            }
        )*
    };
}

#[doc(hidden)]
macro_rules! tag_impl_lt {
    ($a:lifetime, $($tok:ident)*) => {
        $(
            impl<$a> $tok<$a> {
                pub fn parse(inp: Tokens<$a>) -> IResult<Tokens<$a>, WithSpan<Self>> {
                    let (rest, one) = take(1usize)(inp)?;
                    if one.tok.is_empty() {
                        Err(Err::Error(Error::new(inp, ErrorKind::Tag)))
                    } else {
                        match one.tok[0].item {
                            CToken::$tok(tok) => Ok((rest, WithSpan { span: one.tok[0].span, item: tok })),
                            _ => Err(Err::Error(Error::new(inp, ErrorKind::Tag))),
                        }
                    }
                }
            }
        )*
    };
}

tag_impl_lt!('a, Ident);
tag_impl_no_lt!(Constant);

pub fn infix_op<'a>(inp: WithSpan<'a, CToken<'a>>) -> (Precedence, Option<WithSpan<'a, InfixOp>>) {
    if let CToken::Punctuator(t) = inp.item {
        match t {
            Punctuator::PlusEq => (
                Precedence::Equals,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::AddAssign,
                }),
            ),
            Punctuator::StarEq => (
                Precedence::Equals,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::MulAssign,
                }),
            ),
            Punctuator::MinusEq => (
                Precedence::Equals,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::SubAssign,
                }),
            ),
            Punctuator::SlashEq => (
                Precedence::Equals,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::DivAssign,
                }),
            ),
            Punctuator::PercentEq => (
                Precedence::Equals,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::ModAssign,
                }),
            ),
            Punctuator::Equals => (
                Precedence::Equals,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::Assign,
                }),
            ),
            Punctuator::EqEq => (
                Precedence::Equals,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::EqEq,
                }),
            ),
            Punctuator::BangEq => (
                Precedence::Equals,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::NotEqual,
                }),
            ),
            Punctuator::AndAnd => (
                Precedence::Boolean,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::AndAnd,
                }),
            ),
            Punctuator::BarBar => (
                Precedence::Boolean,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::OrOr,
                }),
            ),
            Punctuator::LtEq => (
                Precedence::LessGreater,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::LtEq,
                }),
            ),
            Punctuator::GtEq => (
                Precedence::LessGreater,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::GtEq,
                }),
            ),
            Punctuator::Lt => (
                Precedence::LessGreater,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::Lt,
                }),
            ),
            Punctuator::Gt => (
                Precedence::LessGreater,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::Gt,
                }),
            ),
            Punctuator::Plus => (
                Precedence::Sum,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::Add,
                }),
            ),
            Punctuator::Minus => (
                Precedence::Sum,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::Sub,
                }),
            ),
            Punctuator::Star => (
                Precedence::Product,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::Mul,
                }),
            ),
            Punctuator::FSlash => (
                Precedence::Product,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::Div,
                }),
            ),
            Punctuator::Percent => (
                Precedence::Product,
                Some(WithSpan {
                    span: inp.span,
                    item: InfixOp::Mod,
                }),
            ),
            _ => (Precedence::Lowest, None),
        }
    } else {
        (Precedence::Lowest, None)
    }
}

impl TypeSpecifier {
    pub fn parse(inp: Tokens<'_>) -> IResult<Tokens<'_>, WithSpan<Self>> {
        map(
            tuple((
                alt((
                    map(kw_void, |s| s.first_into(TypeSpecifier::Void)),
                    map(kw_char, |s| s.first_into(TypeSpecifier::Char)),
                    map(kw_short, |s| s.first_into(TypeSpecifier::Short)),
                    map(kw_int, |s| s.first_into(TypeSpecifier::Int)),
                    map(kw_long, |s| s.first_into(TypeSpecifier::Long)),
                    // map(Ident::parse, |s| WithSpan {
                    //     span: s.span,
                    //     item: TypeSpecifier::TypedefName(s.item),
                    // }),
                )),
                opt(punc_star),
            )),
            |(typ, is_pointer)| {
                if is_pointer.is_some() {
                    WithSpan {
                        span: typ.span,
                        item: TypeSpecifier::Pointer(Box::new(typ.item)),
                    }
                } else {
                    typ
                }
            },
        )(inp)
    }
}

impl<'a> TypedIdent<'a> {
    pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
        map(tuple((TypeSpecifier::parse, Ident::parse)), |(typ, id)| {
            WithSpan {
                span: typ.span,
                item: TypedIdent { typ, id },
            }
        })(inp)
    }
}

impl<'a> CallExpr<'a> {
    pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
        map(
            tuple((
                Ident::parse,
                delimited(
                    punc_oparen,
                    separated_list0(punc_comma, Expr::parse),
                    punc_cparen,
                ),
            )),
            |(id, args)| WithSpan {
                span: id.span,
                item: CallExpr { func: id, args },
            },
        )(inp)
    }
}

impl<'a> IndexExpr<'a> {
    pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
        map(
            tuple((
                Ident::parse,
                delimited(punc_obrack, Expr::parse, punc_cbrack),
            )),
            |(arr, idx)| WithSpan {
                span: arr.span,
                item: IndexExpr {
                    arr: WithSpan {
                        span: arr.span,
                        item: Expr::Ident(arr),
                    },
                    idx,
                },
            },
        )(inp)
    }
}

impl<'a> PostfixExpr<'a> {
    pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
        alt((
            map(CallExpr::parse, |call| WithSpan {
                span: call.span,
                item: PostfixExpr::Call(call),
            }),
            map(IndexExpr::parse, |index| WithSpan {
                span: index.span,
                item: PostfixExpr::Index(index),
            }),
        ))(inp)
    }
}

impl UnaryOp {
    pub fn parse(inp: Tokens<'_>) -> IResult<Tokens<'_>, WithSpan<Self>> {
        alt((
            map(punc_plus, |s| s.first_into(UnaryOp::Plus)),
            map(punc_minus, |s| s.first_into(UnaryOp::Minus)),
            map(punc_bang, |s| s.first_into(UnaryOp::Not)),
            map(punc_star, |s| s.first_into(UnaryOp::Deref)),
            map(punc_ampersand, |s| s.first_into(UnaryOp::AddrOf)),
            map(punc_tilde, |s| s.first_into(UnaryOp::Tilde)),
        ))(inp)
    }
}

impl<'a> UnaryExpr<'a> {
    pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
        map(tuple((UnaryOp::parse, Expr::parse)), |(op, expr)| {
            WithSpan {
                span: op.span,
                item: UnaryExpr { op, expr },
            }
        })(inp)
    }
}

pub fn parse_atom_expr(inp: Tokens) -> IResult<Tokens, WithSpan<Expr>> {
    alt((
        map(UnaryExpr::parse, |unary| WithSpan {
            span: unary.span,
            item: Expr::Unary(Box::new(unary)),
        }),
        map(PostfixExpr::parse, |post| WithSpan {
            span: post.span,
            item: Expr::Postfix(Box::new(post)),
        }),
        map(Constant::parse, |con| WithSpan {
            span: con.span,
            item: Expr::Constant(con),
        }),
        map(Ident::parse, |id| WithSpan {
            span: id.span,
            item: Expr::Ident(id),
        }),
        delimited(punc_oparen, Expr::parse, punc_cparen),
    ))(inp)
}

pub fn parse_pratt_expr(inp: Tokens, precedence: Precedence) -> IResult<Tokens, WithSpan<Expr>> {
    let (rest, left) = parse_atom_expr(inp)?;
    parse_pratt_expr_impl(rest, precedence, left)
}

fn parse_pratt_expr_impl<'a>(
    inp: Tokens<'a>,
    precedence: Precedence,
    left: WithSpan<'a, Expr<'a>>,
) -> IResult<Tokens<'a>, WithSpan<'a, Expr<'a>>> {
    let (rest1, one) = take(1usize)(inp)?;
    if one.tok.is_empty() {
        Ok((rest1, left))
    } else {
        let preview = one.tok[0];
        let (p_prec, _op) = infix_op(preview);
        match p_prec {
            p_prec if precedence < p_prec => {
                let (rest2, left2) = parse_infix_expr(inp, left)?;
                parse_pratt_expr_impl(rest2, precedence, left2)
            }
            _ => Ok((inp, left)),
        }
    }
}

pub fn parse_infix_expr<'a>(
    inp: Tokens<'a>,
    left: WithSpan<'a, Expr<'a>>,
) -> IResult<Tokens<'a>, WithSpan<'a, Expr<'a>>> {
    let (rest1, one) = take(1usize)(inp)?;
    if one.tok.is_empty() {
        Err(Err::Error(Error::new(inp, ErrorKind::Tag)))
    } else {
        let next = one.tok[0];
        let (precedence, op) = infix_op(next);
        match op {
            Some(op) => {
                let (rest2, right) = parse_pratt_expr(rest1, precedence)?;
                Ok((
                    rest2,
                    WithSpan {
                        span: left.span,
                        item: Expr::Infix(Box::new(WithSpan {
                            span: left.span,
                            item: InfixExpr {
                                op,
                                lhs: left,
                                rhs: right,
                            },
                        })),
                    },
                ))
            }
            None => Err(Err::Error(Error::new(inp, ErrorKind::Tag))),
        }
    }
}

impl<'a> Expr<'a> {
    pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
        parse_pratt_expr(inp, Precedence::Lowest)
    }
}

impl<'a> Declaration<'a> {
    pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
        map(
            tuple((TypeSpecifier::parse, Expr::parse, punc_semicolon)),
            |(typ, expr, _)| WithSpan {
                span: typ.span,
                item: Declaration {
                    typ,
                    init_expr: expr,
                },
            },
        )(inp)
    }
}

impl<'a> JumpStatement<'a> {
    pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
        alt((
            map(delimited(kw_goto, Ident::parse, punc_semicolon), |id| {
                WithSpan {
                    span: id.span,
                    item: JumpStatement::Goto(id),
                }
            }),
            map(
                terminated(tuple((kw_return, opt(Expr::parse))), punc_semicolon),
                |(ret, expr)| WithSpan {
                    span: ret.first_span(),
                    item: JumpStatement::Return(expr),
                },
            ),
        ))(inp)
    }
}

impl<'a> BlockItem<'a> {
    pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
        alt((
            map(Declaration::parse, |decl| WithSpan {
                span: decl.span,
                item: BlockItem::Declaration(decl),
            }),
            map(Statement::parse, |stmt| WithSpan {
                span: stmt.span,
                item: BlockItem::Statement(stmt),
            }),
        ))(inp)
    }
}

impl<'a> CompoundStatement<'a> {
    pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
        map(
            terminated(tuple((punc_obrace, many0(BlockItem::parse))), punc_cbrace),
            |(ob, items)| WithSpan {
                span: ob.first_span(),
                item: CompoundStatement(items),
            },
        )(inp)
    }
}

impl<'a> LabeledStatement<'a> {
    pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
        map(
            tuple((Ident::parse, punc_colon, Statement::parse)),
            |(label, _, stmt)| WithSpan {
                span: label.span,
                item: LabeledStatement { label, stmt },
            },
        )(inp)
    }
}

impl<'a> BuiltinStatement<'a> {
    pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
        alt((map(
            tuple((kw_printi, Expr::parse, punc_semicolon)),
            |(kw, expr, _)| WithSpan {
                span: kw.first_span(),
                item: BuiltinStatement {
                    builtin: kw.first_into(Keyword::Printi),
                    expr,
                },
            },
        ),))(inp)
    }
}

impl<'a> IterationStatement<'a> {
    pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
        alt((
            map(
                tuple((
                    kw_while,
                    delimited(punc_oparen, Expr::parse, punc_cparen),
                    Statement::parse,
                )),
                |(kw, cond, body)| WithSpan {
                    span: kw.first_span(),
                    item: IterationStatement {
                        init: None,
                        cond: Some(cond),
                        step: None,
                        body,
                    },
                },
            ),
            map(
                tuple((
                    kw_for,
                    punc_oparen,
                    opt(Expr::parse),
                    punc_semicolon,
                    opt(Expr::parse),
                    punc_semicolon,
                    opt(Expr::parse),
                    punc_cparen,
                    Statement::parse,
                )),
                |(kw, _, init, _, cond, _, step, _, body)| WithSpan {
                    span: kw.first_span(),
                    item: IterationStatement {
                        init,
                        cond,
                        step,
                        body,
                    },
                },
            ),
        ))(inp)
    }
}

impl<'a> SelectionStatement<'a> {
    pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
        map(
            tuple((
                kw_if,
                delimited(punc_oparen, Expr::parse, punc_cparen),
                Statement::parse,
                opt(preceded(kw_else, Statement::parse)),
            )),
            |(kw, cond, if_body, else_body)| WithSpan {
                span: kw.first_span(),
                item: SelectionStatement {
                    cond,
                    if_body,
                    else_body,
                },
            },
        )(inp)
    }
}

impl<'a> Statement<'a> {
    pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
        alt((
            map(BuiltinStatement::parse, |stmt| WithSpan {
                span: stmt.span,
                item: Statement::Builtin(stmt),
            }),
            map(LabeledStatement::parse, |stmt| WithSpan {
                span: stmt.span,
                item: Statement::Labeled(Box::new(stmt)),
            }),
            map(CompoundStatement::parse, |stmt| WithSpan {
                span: stmt.span,
                item: Statement::Compound(stmt),
            }),
            map(terminated(Expr::parse, punc_semicolon), |stmt| WithSpan {
                span: stmt.span,
                item: Statement::Expr(stmt),
            }),
            map(SelectionStatement::parse, |stmt| WithSpan {
                span: stmt.span,
                item: Statement::Selection(Box::new(stmt)),
            }),
            map(IterationStatement::parse, |stmt| WithSpan {
                span: stmt.span,
                item: Statement::Iteration(Box::new(stmt)),
            }),
            map(JumpStatement::parse, |stmt| WithSpan {
                span: stmt.span,
                item: Statement::Jump(stmt),
            }),
        ))(inp)
    }
}

impl<'a> FunctionDefinition<'a> {
    pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
        map(
            tuple((
                TypedIdent::parse,
                delimited(
                    punc_oparen,
                    separated_list0(punc_comma, TypedIdent::parse),
                    punc_cparen,
                ),
                CompoundStatement::parse,
            )),
            |(id, param_list, body)| WithSpan {
                span: id.span,
                item: FunctionDefinition {
                    id,
                    param_list,
                    body,
                },
            },
        )(inp)
    }
}

impl<'a> ExternalDeclaration<'a> {
    pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
        alt((
            map(FunctionDefinition::parse, |f| WithSpan {
                span: f.span,
                item: ExternalDeclaration::FunctionDefinition(f),
            }),
            // map(Declaration::parse, |decl| WithSpan {
            //     span: decl.span,
            //     item: ExternalDeclaration::Declaration(decl),
            // }),
        ))(inp)
    }
}

impl<'a> TranslationUnit<'a> {
    pub fn parse(inp: Tokens<'a>) -> IResult<Tokens<'a>, WithSpan<Self>> {
        map(many0(ExternalDeclaration::parse), |decls| WithSpan {
            span: inp.first_span(),
            item: TranslationUnit(decls),
        })(inp)
    }
}
