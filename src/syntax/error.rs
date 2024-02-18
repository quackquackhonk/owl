use thiserror::Error;

use super::{Spanned, lexer::Token};

#[derive(Debug, Error)]
pub enum OwlParseError {
    #[error("Invalid tokens found: {0:?}")]
    LexerError(Spanned<String>),
    #[error("Unexpected token found!")]
    UnexpectedToken {
        expected: Token,
        found: Spanned<Token>,
    },
    #[error("Reached end of input!")]
    EndOfInput,
}

// TODO: Manually implement display for pretty printing?
