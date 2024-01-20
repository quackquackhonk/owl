use thiserror::Error;

#[derive(Debug, Error)]
pub enum OwlParseError {
    #[error("Nom threw an error: {0}")]
    Nom(String),
}
