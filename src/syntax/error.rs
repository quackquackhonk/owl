use thiserror::Error;

use super::{lexer::Token, Spanned};

// use ariadne::{Color, Label, Report, ReportKind, Source};

#[derive(Debug, Error)]
pub enum Recoverable {
    #[error("Invalid tokens found: {0:?}")]
    LexerError(Spanned<String>),
    #[error("Unexpected token found!")]
    UnexpectedToken {
        expected: Option<Token>,
        found: Spanned<Token>,
    }
}

#[derive(Debug, Error)]
pub enum Unrecoverable {
    #[error("Reached end of input!")]
    EndOfInput,
    #[error("Found a bad token when looking for a type: {0:?}")]
    InvalidType(Spanned<Token>),
    #[error("Finished parsing with errors!")]
    Finished(Vec<Recoverable>),
}

pub fn produce_error_report(errors: Vec<Recoverable>) -> Unrecoverable {
    // TODO: error reporting with ariadne
    Unrecoverable::Finished(errors)
}
