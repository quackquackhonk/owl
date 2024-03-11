use logos::Logos;

use crate::syntax::span::Spanned;

#[derive(Logos, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    #[regex(r"[a-zA-Z_][a-zA-Z?'_]*", |lex| lex.slice().to_string())]
    ID(String),
    #[regex(r"[-]?[0-9]+", |lex| lex.slice().to_string().parse::<isize>().unwrap())]
    Num(isize),
    #[regex("true|false", |lex| if lex.slice() == "true" { true } else { false })]
    Bool(bool),
    #[token("()")]
    Unit,
    // declarations
    #[token("let")]
    Let,
    #[token("=")]
    Assign,
    #[token("fun")]
    Fun,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    // symbols
    #[token(";")]
    SemiColon,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("->")]
    Arrow,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    // TODO: Function expressions
    // #[token("@")]
    // At,
    // #[token("=>")]
    // BigArrow,
    // arith
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Mult,
    #[token("/")]
    Divide,
    #[token("!")]
    Bang,
    #[token("==")]
    Eq,
    #[token("!=")]
    Neq,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("<")]
    Lt,
    #[token("<=")]
    LtEq,
    #[token(">")]
    Gt,
    #[token(">=")]
    GtEq,

    #[regex(r"[ \t\f\n]+", logos::skip)]
    Whitespace,
    #[regex(r"#.*\n", logos::skip)]
    Comment,
    #[regex(r"#\*.*\*#")]
    MultilineComment,
    // error variant, only produced by the entry point
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
            Ok(tok) => out.push(Spanned::new(tok, res.1.into())),
            Err(_) => {
                let bad = lex.slice().to_string();
                out.push(Spanned::new(Token::Error(bad), res.1.into()))
            }
        }
    }

    out
}
