use thiserror::Error;

use super::{lexer::Token, Spanned};

// use ariadne::{Color, Label, Report, ReportKind, Source};

// FIXME: There should be a distinction between recoverable and unrecoverable errors
#[derive(Debug, Error)]
pub enum OwlParseError {
    #[error("Invalid tokens found: {0:?}")]
    LexerError(Spanned<String>),
    #[error("Unexpected token found!")]
    UnexpectedToken {
        expected: Option<Token>,
        found: Spanned<Token>,
    },
    #[error("Found a bad token when looking for a type: {0:?}")]
    InvalidType(Spanned<Token>),
    #[error("Reached end of input!")]
    EndOfInput,
    #[error("Reached end of input!")]
    Recoverable(Vec<OwlParseError>)
}

pub fn produce_error_report(errors: Vec<OwlParseError>) -> OwlParseError {
    // TODO: error reporting with ariadne
    OwlParseError::Recoverable(errors)
}
