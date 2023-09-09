use std::fmt::Debug;

use crate::asm::WithSpan;

#[derive(PartialEq, Clone, Copy)]
pub struct Ident<'a>(pub &'a str);

impl<'a> Debug for Ident<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Constant {
    Integer(i64),
    // FloatingPoint(f64),
    // Character(char),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    // Float,
    // Double,
    // TypedefName(Ident<'a>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum InfixOp {
    Star,
    Plus,
    Minus,
    FSlash,
    Equal,
    NotEqual,
    Lt,
    LtEq,
    Gt,
    GtEq,
    // todo...
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition<'a> {
    pub id: WithSpan<'a, TypedIdent<'a>>,
    pub param_list: Vec<WithSpan<'a, TypedIdent<'a>>>,
    pub body: WithSpan<'a, CompoundStatement<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Declaration<'a> {
    pub id: WithSpan<'a, TypedIdent<'a>>,
    pub expr: WithSpan<'a, Expr<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BlockItem<'a> {
    Declaration(WithSpan<'a, Declaration<'a>>),
    Statement(WithSpan<'a, Statement<'a>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum JumpStatement<'a> {
    Goto(WithSpan<'a, Ident<'a>>),
    Return(Option<WithSpan<'a, Expr<'a>>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompoundStatement<'a>(pub Vec<WithSpan<'a, BlockItem<'a>>>);

#[derive(Debug, PartialEq, Clone)]
pub struct LabeledStatement<'a> {
    pub label: WithSpan<'a, Ident<'a>>,
    pub stmt: WithSpan<'a, Statement<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a> {
    Labeled(Box<WithSpan<'a, LabeledStatement<'a>>>),
    Compound(WithSpan<'a, CompoundStatement<'a>>),
    Jump(WithSpan<'a, JumpStatement<'a>>),
    Expr(WithSpan<'a, Expr<'a>>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct TypedIdent<'a> {
    pub typ: WithSpan<'a, TypeSpecifier>,
    pub id: WithSpan<'a, Ident<'a>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct InfixExpr<'a> {
    pub op: WithSpan<'a, InfixOp>,
    pub lhs: WithSpan<'a, Expr<'a>>,
    pub rhs: WithSpan<'a, Expr<'a>>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Call,
    Index,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr<'a> {
    Ident(WithSpan<'a, Ident<'a>>),
    Constant(WithSpan<'a, Constant>),
    Infix(Box<WithSpan<'a, InfixExpr<'a>>>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum ExternalDeclaration<'a> {
    FunctionDefinition(WithSpan<'a, FunctionDefinition<'a>>),
    Declaration(WithSpan<'a, Declaration<'a>>),
}

#[derive(PartialEq, Debug, Clone)]
pub struct TranslationUnit<'a>(pub Vec<WithSpan<'a, ExternalDeclaration<'a>>>);
