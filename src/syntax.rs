//! This module contains utilities for dealing with the syntax of the Owl language.

pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;

pub type Span = core::ops::Range<usize>;
pub type Spanned<T> = (T, Span);

pub fn span_add(start: Span, end: Span) -> Span {
    start.start..end.end
}

#[cfg(test)]
mod tests {
    use super::*;
    use logos::Logos;
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
        assert!(lexer::Token::lexer(&source)
            .spanned()
            .all(|(tok, _)| { tok.is_ok() }));

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
        let prog = parser::owl_program_parser(&path);

        assert!(prog.is_ok());

        Ok(())
    }
}
