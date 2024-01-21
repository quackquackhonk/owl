use super::Spanned;

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

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Var(Name),
    Unit,
    Int,
    Bool,
    Paren(Box<Type>),
    Arrow(Box<Type>, Box<Type>)
}

#[derive(Debug, PartialEq, Eq)]
pub enum Declaration {
    Value(Name, Vec<Name>, Spanned<Expression>),
    Type(Name, Spanned<Type>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Unit,
    Int(isize),
    Bool(bool),
    Var(Name),
    Paren(Box<Spanned<Expression>>),
    BinaryOp(BinOp, Box<Spanned<Expression>>, Box<Expression>),
    UnaryOp(UnOp, Box<Spanned<Expression>>),
    Conditional(Box<Spanned<Expression>>, Box<Expression>, Box<Expression>),
    Function(Vec<Spanned<Name>>, Box<Spanned<Expression>>),
    FuncCall(Box<Spanned<Expression>>, Box<Expression>),
    Sequence(Vec<Spanned<Statement>>)
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Expr(Expression),
    Decl(Declaration),
}

#[derive(Debug, PartialEq, Eq)]
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
    Or
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not
}
