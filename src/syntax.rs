//! This module contains utilities for dealing with the syntax of the Owl language.

pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod pretty;
pub mod span;

#[cfg(test)]
mod tests {
    use super::*;
    use logos::Logos;
    use rstest::rstest;
    use std::fs::File;
    use std::io::Read;

    #[rstest]
    #[case("examples/comments.owl")]
    #[case("examples/currying.owl")]
    #[case("examples/declarations.owl")]
    #[case("examples/expressions.owl")]
    // TODO: String expressions
    // #[case("examples/hello_world.owl")]
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
    #[case("examples/comments.owl")]
    #[case("examples/currying.owl")]
    #[case("examples/declarations.owl")]
    #[case("examples/expressions.owl")]
    // #[case("examples/hello_world.owl")]
    fn test_parsing_examples(#[case] path: &str) -> anyhow::Result<()> {
        let prog = parser::owl_program_parser(&path);

        assert!(prog.is_ok());

        Ok(())
    }
}
