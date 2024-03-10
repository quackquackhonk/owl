use super::Spanned;

#[derive(Debug, PartialEq, Eq)]
pub struct Program(Vec<Spanned<Declaration>>);

impl Program {
    pub fn new(decls: Vec<Spanned<Declaration>>) -> Self {
        Program(decls)
    }
}

pub type Ident = String;

#[derive(Debug, PartialEq, Eq)]
pub struct Arg(Spanned<Ident>, Spanned<Type>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Unit,
    Int,
    Bool,
    Var(Ident),
    Arrow(Box<Spanned<Type>>, Box<Spanned<Type>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Declaration {
    Value(Spanned<Ident>, Spanned<Expression>),
    Function(Spanned<Ident>, Vec<Spanned<Ident>>, Spanned<Expression>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Unit,
    Int(isize),
    Bool(bool),
    Var(Ident),
    BinaryOp(BinOp, Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    UnaryOp(UnOp, Box<Spanned<Expression>>),
    FuncCall(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Expr(Expression),
    Decl(Declaration),
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
