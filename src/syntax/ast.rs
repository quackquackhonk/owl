use super::Spanned;

// TODO: Add pretty-printing

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub decls: Vec<Spanned<Declaration>>,
    pub expr: Spanned<Expression>,
}

impl Program {
    pub fn new(decls: Vec<Spanned<Declaration>>, expr: Spanned<Expression>) -> Self {
        Program { decls, expr }
    }
}

pub type Name = String;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Var(Name),
    Unit,
    Int,
    Bool,
    Arrow(Box<Spanned<Type>>, Box<Spanned<Type>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Declaration {
    Value(Spanned<Name>, Vec<Spanned<Name>>, Spanned<Expression>),
    Type(Spanned<Name>, Spanned<Type>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Unit,
    Int(isize),
    Bool(bool),
    Var(Name),
    BinaryOp(BinOp, Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    UnaryOp(UnOp, Box<Spanned<Expression>>),
    Conditional(
        Box<Spanned<Expression>>,
        Box<Spanned<Expression>>,
        Box<Spanned<Expression>>,
    ),
    Function(Vec<Spanned<Name>>, Box<Spanned<Expression>>),
    FuncCall(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Block(Vec<Spanned<Statement>>),
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnOp {
    Neg,
    Not,
}

