use logos::Span;

pub mod ast;
pub mod parser;
pub mod lexer;
pub mod error;

pub type Spanned<T> = (T, Span);
