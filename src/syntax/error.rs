use thiserror::Error;

use super::lexer::Token;
use crate::syntax::span::Spanned;

// use ariadne::{Color, Label, Report, ReportKind, Source};

#[derive(Debug, Error)]
pub enum Recoverable {
    #[error("Invalid tokens found: {0:?}")]
    LexerError(Spanned<String>),
    #[error("Unexpected token! expected {expected:?}, found {found:?}")]
    UnexpectedToken {
        expected: Token,
        found: Spanned<Token>,
    },
}

#[derive(Debug, Error)]
pub enum Unrecoverable {
    #[error("Reached end of input!")]
    EndOfInput,
    #[error("Found a bad token when looking for a type: {0:?}")]
    InvalidType(Spanned<Token>),
    #[error("Expected a(n) {kind}, found: {found:?}")]
    ExpectedKind {
        kind: String,
        found: Spanned<Token>
    },
    #[error("Expected: {0:?}\nFound: {1:?}")]
    ExpectedTokens(Vec<Token>, Spanned<Token>),
    #[error("Finished parsing with errors!")]
    Finished(Vec<Recoverable>),
}

pub fn produce_error_report(errors: Vec<Recoverable>) -> Unrecoverable {
    // TODO: error reporting with ariadne
    Unrecoverable::Finished(errors)
}
