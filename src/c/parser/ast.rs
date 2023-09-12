use std::fmt::Debug;

use crate::{asm::WithSpan, c::lexer::gen::Keyword};

#[derive(PartialEq, Clone, Copy)]
pub struct Ident<'a>(pub &'a str);

impl<'a> Debug for Ident<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Constant {
    Integer(u16),
    // FloatingPoint(f64),
    // Character(char),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeSpecifier {
    Pointer(Box<TypeSpecifier>),
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
    Assign,
    Mul,
    Add,
    Sub,
    Div,
    EqEq,
    NotEqual,
    Lt,
    LtEq,
    Gt,
    GtEq,
    MulAssign,
    DivAssign,
    AddAssign,
    SubAssign,
    Mod,
    ModAssign,
    And,
    Or,
    Xor,
    AndAssign,
    OrAssign,
    XorAssign,
    AndAnd,
    OrOr,
    Shl,
    Shr,
    ShlAssign,
    ShrAssign,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    AddrOf,
    Deref,
    Plus,
    Minus,
    Tilde,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition<'a> {
    pub id: WithSpan<'a, TypedIdent<'a>>,
    pub param_list: Vec<WithSpan<'a, TypedIdent<'a>>>,
    pub body: WithSpan<'a, CompoundStatement<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Declaration<'a> {
    pub typ: WithSpan<'a, TypeSpecifier>,
    pub init_expr: WithSpan<'a, Expr<'a>>,
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
pub struct BuiltinStatement<'a> {
    pub builtin: WithSpan<'a, Keyword>,
    pub expr: WithSpan<'a, Expr<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IterationStatement<'a> {
    pub init: Option<WithSpan<'a, Expr<'a>>>,
    pub cond: Option<WithSpan<'a, Expr<'a>>>,
    pub step: Option<WithSpan<'a, Expr<'a>>>,
    pub body: WithSpan<'a, Statement<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SelectionStatement<'a> {
    pub cond: WithSpan<'a, Expr<'a>>,
    pub if_body: WithSpan<'a, Statement<'a>>,
    pub else_body: Option<WithSpan<'a, Statement<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a> {
    Labeled(Box<WithSpan<'a, LabeledStatement<'a>>>),
    Compound(WithSpan<'a, CompoundStatement<'a>>),
    Jump(WithSpan<'a, JumpStatement<'a>>),
    Builtin(WithSpan<'a, BuiltinStatement<'a>>),
    Iteration(Box<WithSpan<'a, IterationStatement<'a>>>),
    Selection(Box<WithSpan<'a, SelectionStatement<'a>>>),
    Expr(WithSpan<'a, Expr<'a>>),
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(PartialEq, Debug, Clone)]
pub struct CallExpr<'a> {
    pub func: WithSpan<'a, Ident<'a>>,
    pub args: Vec<WithSpan<'a, Expr<'a>>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct IndexExpr<'a> {
    pub arr: WithSpan<'a, Expr<'a>>,
    pub idx: WithSpan<'a, Expr<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum PostfixExpr<'a> {
    Call(WithSpan<'a, CallExpr<'a>>),
    Index(WithSpan<'a, IndexExpr<'a>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpr<'a> {
    pub op: WithSpan<'a, UnaryOp>,
    pub expr: WithSpan<'a, Expr<'a>>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Precedence {
    Lowest,
    Equals,
    Boolean,
    LessGreater,
    Sum,
    Product,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr<'a> {
    Ident(WithSpan<'a, Ident<'a>>),
    Constant(WithSpan<'a, Constant>),
    Infix(Box<WithSpan<'a, InfixExpr<'a>>>),
    Postfix(Box<WithSpan<'a, PostfixExpr<'a>>>),
    Unary(Box<WithSpan<'a, UnaryExpr<'a>>>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum ExternalDeclaration<'a> {
    FunctionDefinition(WithSpan<'a, FunctionDefinition<'a>>),
    Declaration(WithSpan<'a, Declaration<'a>>),
}

#[derive(PartialEq, Debug, Clone)]
pub struct TranslationUnit<'a>(pub Vec<WithSpan<'a, ExternalDeclaration<'a>>>);
