use logos::Logos;

#[derive(Logos, Clone, Debug, PartialEq, Eq)]
pub enum Token {
    #[regex(r"[a-zA-Z_][a-zA-Z!?'_]*", |lex| lex.slice().to_string())]
    ID(String),
    #[regex(r"[-]?[0-9]+", |lex| lex.slice().to_string())]
    Num(String),
    #[token("true")]
    True,
    #[token("false")]
    False,
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

    #[regex(r"[ \t\f\n]+", logos::skip)]
    Whitespace,
}
