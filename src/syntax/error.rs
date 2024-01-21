use thiserror::Error;

use super::Spanned;

#[derive(Debug, Error)]
pub enum OwlParseError {
    #[error("Invalid tokens found: {0:?}")]
    LexerError(Vec<Spanned<String>>),
}

pub type OwlParseResult<T> = Result<T, OwlParseError>;

// TODO: Manually implement display for pretty printing?
