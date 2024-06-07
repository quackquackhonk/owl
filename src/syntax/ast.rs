use std::fmt::Display;

use crate::syntax::span::Spanned;

use super::{error::Unrecoverable, lexer::Token};

#[derive(Debug, PartialEq, Eq)]
pub struct Program(pub Vec<Spanned<Declaration>>);

impl Program {
    pub fn new(decls: Vec<Spanned<Declaration>>) -> Self {
        Program(decls)
    }
}

pub type Ident = String;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Arg(pub Spanned<Ident>, pub Option<Spanned<Type>>);

impl Arg {
    pub fn typ(&self) -> &Option<Spanned<Type>> {
        &self.1
    }

    pub fn name(&self) -> Ident {
        self.0.to_string()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Unit,
    Int,
    Bool,
    Var(Ident),
    Arrow(Box<Spanned<Type>>, Box<Spanned<Type>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Unit,
    Int(isize),
    Bool(bool),
    Var(Ident),
    Tuple(Vec<Spanned<Expression>>),
    BinaryOp(BinOp, Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Apply(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Block(Vec<Statement>, Option<Box<Spanned<Expression>>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Declaration {
    Value(Spanned<Arg>, Spanned<Expression>),
    Function(Spanned<Arg>, Vec<Spanned<Arg>>, Spanned<Expression>),
}

impl Declaration {
    pub fn typ(&self) -> &Option<Spanned<Type>> {
        match self {
            Declaration::Value(a, _) => a.typ(),
            Declaration::Function(a, _, _) => a.typ(),
        }
    }
    pub fn body(&self) -> &Spanned<Expression> {
        match self {
            Declaration::Value(_, e) => e,
            Declaration::Function(_, _, e) => e,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Expr(Spanned<Expression>),
    Decl(Spanned<Declaration>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ReplStatement {
    Stmt(Spanned<Statement>),
    Expr(Spanned<Expression>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    And,
    Or,
}

impl TryFrom<Spanned<Token>> for BinOp {
    type Error = Unrecoverable;

    fn try_from(value: Spanned<Token>) -> Result<Self, Self::Error> {
        match value.0 {
            Token::Plus => Ok(BinOp::Add),
            Token::Minus => Ok(BinOp::Sub),
            Token::Mult => Ok(BinOp::Mul),
            Token::Divide => Ok(BinOp::Div),
            Token::Eq => Ok(BinOp::Eq),
            Token::Neq => Ok(BinOp::Neq),
            Token::And => Ok(BinOp::And),
            Token::Or => Ok(BinOp::Or),
            Token::Lt => Ok(BinOp::Lt),
            Token::LtEq => Ok(BinOp::LtEq),
            Token::Gt => Ok(BinOp::Gt),
            Token::GtEq => Ok(BinOp::GtEq),
            _ => Err(Unrecoverable::ExpectedKind(String::from("BinOp"), value)),
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinOp::Add => String::from("+"),
                BinOp::Sub => String::from("-"),
                BinOp::Mul => String::from("*"),
                BinOp::Div => String::from("/"),
                BinOp::Eq => String::from("=="),
                BinOp::Neq => String::from("!="),
                BinOp::Lt => String::from("<"),
                BinOp::LtEq => String::from("<="),
                BinOp::Gt => String::from(">"),
                BinOp::GtEq => String::from(">="),
                BinOp::And => String::from("&&"),
                BinOp::Or => String::from("||"),
            }
        )
    }
}

// TODO: add unary operations
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnOp {
    Neg,
    Not,
}
