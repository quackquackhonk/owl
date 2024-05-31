//! CPS Intermediate Representation datatype
//!
//! This type and the code surrounding it is heavily adapted from
//! - Compiling With Continuations (Andrew W. Appel)

#[derive(Debug, PartialEq, Eq)]
pub struct Variable {}

#[derive(Debug, PartialEq, Eq)]
pub enum Value {
    Var(Variable),
    Label(Variable),
    Int(isize),
    // Real(???),
    // Str(String)
}

#[derive(Debug, PartialEq, Eq)]
pub enum AccessPath {
    OffsetPath(isize),
    SelectPath(isize, Box<AccessPath>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum PrimitiveOp {
    Add,
    Sub,
    Mul,
    Div,
    Negate,
    Lt,
    LtEq,
    Gt,
    GtEq,
    IEq,
    INeq,
    // TODO: CPS: will need more primitive operations as the language expands
}

#[derive(Debug, PartialEq, Eq)]
pub enum CExp {
    // Record(Vec<(Value, AccessPath)>, Variable, Box<CExp>),
    // Select(isize, Value, Variable, Box<CExp>),
    /// Expresses pointer offset
    Offset(isize, Value, Variable, Box<CExp>),
    /// Function Application
    Apply(Value, Vec<Value>),
    /// Used to define mutually recursive functions
    Fix(Vec<(Variable, Vec<Variable>, Box<CExp>)>, Box<CExp>),
    /// Switch operations express branching logic
    Switch(Value, Vec<CExp>),
    /// Represents a primitive operation
    PrimOp(PrimitiveOp, Vec<Value>, Vec<Variable>, Vec<CExp>),
}
