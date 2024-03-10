//! This module contains utilities for dealing with the syntax of the Owl language.

pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;

pub type Span = core::ops::Range<usize>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Spanned<T> {
    val: T,
    span: Span
}

impl<T> Spanned<T> {
    pub fn new(val: T, span: Span) -> Self {
        Self { val, span }
    }
    pub fn extend(self, sp: Span) -> Self {
        Spanned::new(self.val, self.span.start..sp.end)
    }
}


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
            .all(|(tok, _)| { dbg!(tok).is_ok() }));

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
