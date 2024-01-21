use logos::Logos;

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
    /// Error variant, only produced by the entry point
    Error(String),
}

