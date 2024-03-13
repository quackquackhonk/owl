use crate::syntax::span::Spanned;

#[derive(Debug, PartialEq, Eq)]
pub struct Program(Vec<Spanned<Declaration>>);

impl Program {
    pub fn new(decls: Vec<Spanned<Declaration>>) -> Self {
        Program(decls)
    }
}

pub type Ident = String;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Arg(pub Spanned<Ident>, pub Option<Spanned<Type>>);

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
    BinaryOp(BinOp, Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    UnaryOp(UnOp, Box<Spanned<Expression>>),
    FuncCall(Box<Spanned<Expression>>, Box<Vec<Spanned<Expression>>>),
    Block(Vec<Statement>, Option<Box<Spanned<Expression>>>)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Declaration {
    Value(Spanned<Arg>, Spanned<Expression>),
    Function(Spanned<Ident>, Vec<Spanned<Arg>>, Option<Spanned<Type>>, Spanned<Expression>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Expr(Spanned<Expression>),
    Decl(Spanned<Declaration>)
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

// TODO: add unary operations
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnOp {
    Neg,
    Not,
}

// TODO: Add pretty-printing
