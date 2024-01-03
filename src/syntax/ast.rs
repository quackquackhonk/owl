#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub decls: Vec<Declaration>,
    pub expr: Expression,
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
    Value(Name, Vec<Name>, Expression),
    Type(Name, Type),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Unit,
    Int(isize),
    Bool(bool),
    Var(Name),
    Paren(Box<Expression>),
    BinaryOp(BinOp, Box<Expression>, Box<Expression>),
    UnaryOp(UnOp, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    Function(Vec<Name>, Box<Expression>),
    FuncCall(Box<Expression>, Box<Expression>),
    Sequence(Vec<Statement>)
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
