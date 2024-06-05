use thiserror::Error;

use super::{lexer::Token, span::Span};
use crate::syntax::span::Spanned;

use ariadne::{Label, Report, ReportKind, Source};

// TODO: I should have some trait that describes `Reportable` types so I can implement the function
// for report by defining label, message

#[derive(Debug, Error)]
pub enum Recoverable {
    #[error("Invalid Token!")]
    LexerError(Spanned<String>),
    #[error("Unexpected Token!")]
    UnexpectedToken {
        expected: Token,
        found: Spanned<Token>,
    },
}

impl Recoverable {
    pub fn message(&self) -> String {
        self.to_string()
    }

    pub fn label(&self, path: String) -> Label<(String, std::ops::Range<usize>)> {
        match self {
            Recoverable::LexerError(found) => Label::new((path, self.span().into()))
                .with_message(format!("Invalid token {} found.", found.val())),
            Recoverable::UnexpectedToken { expected, found } => {
                Label::new((path, self.span().into())).with_message(format!(
                    "Expected token {}, found {}!",
                    expected.to_string(),
                    found.val()
                ))
            }
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Recoverable::LexerError(s) => s.span(),
            Recoverable::UnexpectedToken { found, .. } => found.span(),
        }
    }

    pub fn report<'a>(&self, path: &str, source: &'a str) -> anyhow::Result<()> {
        Report::build::<String>(ReportKind::Error, path.to_string(), self.span().start)
            .with_message(self.message())
            .with_label(self.label(path.to_string()))
            .finish()
            .eprint((path.to_string(), Source::from(source)))?;

        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum Unrecoverable {
    #[error("Reached end of input!")]
    EndOfInput,
    // TODO: Better naming to deliniate what these actually mean
    #[error("Expected to find a {0:}!")]
    ExpectedKind(String, Spanned<Token>),
    #[error("Unexpected tokens found!")]
    ExpectedTokens(Vec<Token>, Spanned<Token>),
    #[error("Finished parsing with errors!")]
    Finished(Vec<Recoverable>),
}

impl Unrecoverable {
    pub fn message(&self) -> String {
        self.to_string()
    }

    fn label(&self, path: String) -> Label<(String, std::ops::Range<usize>)> {
        Label::new((path, self.span().into())).with_message(match self {
            Unrecoverable::EndOfInput => "Reached the end of input early!".to_string(),
            Unrecoverable::ExpectedKind(kind, found) => {
                format!("Found {} when parsing a(n) {}", found.val(), kind)
            }
            Unrecoverable::ExpectedTokens(exp, fo) => {
                format!("Expected to find one of {:?}. Found: {}", exp, fo.val())
            }
            Unrecoverable::Finished(_) => unreachable!(),
        })
    }

    fn span(&self) -> Span {
        match self {
            // FIX: (0..0)is probably not right
            Unrecoverable::EndOfInput => (0..0).into(),
            Unrecoverable::ExpectedKind(_, found) => found.span(),
            Unrecoverable::ExpectedTokens(_, found) => found.span(),
            Unrecoverable::Finished(_) => (0..0).into(),
        }
    }

    pub fn report<'a>(&self, path: &str, source: &'a str) -> anyhow::Result<()> {
        match self {
            Unrecoverable::Finished(ref errors) => errors.iter().for_each(|e| {
                let _ = e.report(path, source);
            }),
            err => Report::build::<String>(ReportKind::Error, path.to_string(), self.span().start)
                .with_message(err.message())
                .with_label(err.label(path.to_string()))
                .finish()
                .eprint((path.to_string(), Source::from(source)))?,
        }
        Ok(())
    }
}
