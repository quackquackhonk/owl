//! This module contains utilities for dealing with the syntax of the Owl language.

use logos::{Span, Logos};

use self::lexer::Token;

pub mod ast;
pub mod parser;
pub mod lexer;
pub mod error;

pub type Spanned<T> = (T, Span);

/// Entry point for the lexer.
/// If any errors were encountered, this function replaces them with [`Token::Error`]s.
///
/// * `source`: the source code of the owl program
pub fn lexer(source: &str) -> Vec<Spanned<Token>> {
    let mut lex = Token::lexer(source).spanned();

    let mut out: Vec<Spanned<Token>> = vec![];

    // NOTE: It would be really nice if I didn't have to collect the values manually
    while let Some(res) = lex.next() {
        match res.0 {
            Ok(tok) => out.push((tok, res.1)),
            Err(_) => {
                let bad = lex.slice().to_string();
                out.push((Token::Error(bad), res.1))
            }
        }
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
    use std::fs::File;
    use std::io::Read;

    #[rstest]
    #[case("examples/anonymous_functions.owl")]
    #[case("examples/fibonacci.owl")]
    #[case("examples/currying.owl")]
    #[case("examples/identifiers.owl")]
    #[case("examples/logical_operations.owl")]
    #[case("examples/precedence.owl")]
    #[case("examples/sequences.owl")]
    fn test_lexing_examples(#[case] path: &str) -> anyhow::Result<()> {
        let mut file = File::open(path)?;
        let mut source = String::new();
        file.read_to_string(&mut source)?;
        assert!(lexer(&source).iter().all(|(tok, _)| {
            match tok {
                Token::Error(_) => false,
                _ => true
            }
        }));

        Ok(())
    }

    #[rstest]
    #[case("examples/anonymous_functions.owl")]
    #[case("examples/fibonacci.owl")]
    #[case("examples/currying.owl")]
    #[case("examples/identifiers.owl")]
    #[case("examples/logical_operations.owl")]
    #[case("examples/precedence.owl")]
    #[case("examples/sequences.owl")]
    fn test_parsing_examples(#[case] path: &str) -> anyhow::Result<()> {
        let mut file = File::open(path)?;
        let mut source = String::new();
        file.read_to_string(&mut source)?;
        let prog = parser::owl_parser(&source);

        assert!(prog.is_ok());

        Ok(())
    }
}
