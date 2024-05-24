use crate::syntax::ast;

struct IdFactory {
    next_id: usize,
}

impl IdFactory {
    fn new(next_id: usize) -> Self {
        Self { next_id }
    }

    fn new_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}

/// Enum type to represent the Lambda calculus
pub enum Expression {
    /// Variables have a `usize` as a unique identifier
    Variable(usize),
    /// Call the first `Expression` with the second `Expression`
    Call(Box<Expression>, Box<Expression>),
    /// Anonymouse function expressions
    /// `usize` can be refereneced in the body `Expression`
    Lambda(usize, Box<Expression>),
}

impl From<ast::Program> for Expression {
    fn from(value: ast::Program) -> Self {
        todo!()
    }
}
