use thiserror::Error;

use super::Spanned;

#[derive(Debug, Error)]
pub enum OwlParseError {
    #[error("Invalid tokens found: {0:?}")]
    LexerError(Vec<Spanned<String>>),
}

// TODO: Manually implement display for pretty printing?
