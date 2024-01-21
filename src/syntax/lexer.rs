use logos::{Logos, SpannedIter};

#[derive(Logos, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    #[regex(r"[a-zA-Z_][a-zA-Z?'_]*", |lex| lex.slice().to_string())]
    ID(String),
    #[regex(r"[-]?[0-9]+", |lex| lex.slice().to_string())]
    Num(String),
    #[regex("true|false", |lex| if lex.slice() == "true" { true } else { false })]
    Bool(bool),
    #[token("()")]
    Unit,
    // declarations
    #[token("let")]
    Let,
    #[token("be")]
    Be,
    #[token("typ")]
    Typ,
    #[token("is")]
    Is,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    // symbols
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(";")]
    SemiColon,
    #[token("@")]
    At,
    #[token("$")]
    Dollar,
    #[token("->")]
    Arrow,
    #[token("=>")]
    BigArrow,
    // arith
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Mult,
    #[token("/")]
    Divide,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("!")]
    Bang,
    #[token("=")]
    Eq,
    #[token("&")]
    And,
    #[token("|")]
    Or,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,

    #[regex(r"[ \t\f\n]+", logos::skip)]
    Whitespace,
    #[regex(r"#.*\n", logos::skip)]
    Comment,
}

/// Entry point for the lexer.
///
/// * `source`: the source code of the owl program
pub fn lex_owl(source: &str) -> SpannedIter<'_, Token> {
    Token::lexer(source).spanned()
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
        let no_errors = lex_owl(&source).all(|(tok, _sp)| tok.is_ok());
        assert!(no_errors);

        Ok(())
    }
}
