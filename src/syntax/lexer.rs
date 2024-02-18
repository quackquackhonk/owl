use logos::Logos;

use super::Spanned;

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
    // error variant, only procu
    Error(String),
}

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
