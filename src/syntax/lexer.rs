use std::fmt::Display;

use logos::Logos;

use crate::syntax::span::Spanned;

#[derive(Logos, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    #[regex(r"[a-zA-Z_][a-zA-Z0-9?'_]*", |lex| lex.slice().to_string())]
    ID(String),
    #[regex(r"[-]?[0-9]+", |lex| lex.slice().to_string().parse::<isize>().unwrap())]
    Num(isize),
    #[regex("true|false", |lex| if lex.slice() == "true" { true } else { false })]
    Bool(bool),
    // declarations
    #[token("let")]
    Let,
    #[token("type")]
    Type,
    #[token("enum")]
    Enum,
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
    #[token("::")]
    DblColon,
    #[token(",")]
    Comma,
    #[token("->")]
    Arrow,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    // Function Expressions
    #[token("\\")]
    Lambda,
    #[token("=>")]
    BigArrow,
    // arith
    #[token("$")]
    App,
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

impl Token {
    pub fn is_tight_op(tok: &Token) -> bool {
        match tok {
            Token::Mult
            | Token::Divide
            | Token::Bang
            | Token::Eq
            | Token::Neq
            | Token::Lt
            | Token::LtEq
            | Token::Gt
            | Token::GtEq => true,
            _ => false,
        }
    }

    /// Checks if the token is a special token.
    ///
    /// * `tok`:
    pub fn is_special(tok: &Token) -> bool {
        match tok {
            Token::Comment | Token::MultilineComment | Token::Error(_) | Token::Whitespace => true,
            _ => false,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Token::ID(st) => st.to_string(),
            Token::Num(n) => n.to_string(),
            Token::Bool(b) => b.to_string(),
            Token::Let => "let".to_string(),
            Token::Assign => "=".to_string(),
            Token::Fun => "fun".to_string(),
            Token::Type => "type".to_string(),
            Token::Enum => "enum".to_string(),
            Token::LBrace => "{".to_string(),
            Token::RBrace => "}".to_string(),
            Token::SemiColon => ";".to_string(),
            Token::DblColon => ":".to_string(),
            Token::Comma => ",".to_string(),
            Token::Arrow => "->".to_string(),
            Token::LParen => "(".to_string(),
            Token::RParen => ")".to_string(),
            Token::Lambda => "\\".to_string(),
            Token::BigArrow => "=>".to_string(),
            Token::App => "$".to_string(),
            Token::Plus => "+".to_string(),
            Token::Minus => "-".to_string(),
            Token::Mult => "*".to_string(),
            Token::Divide => "/".to_string(),
            Token::Bang => "!".to_string(),
            Token::Eq => "==".to_string(),
            Token::Neq => "!=".to_string(),
            Token::And => "&&".to_string(),
            Token::Or => "||".to_string(),
            Token::Lt => "<".to_string(),
            Token::LtEq => "<=".to_string(),
            Token::Gt => ">".to_string(),
            Token::GtEq => ">=".to_string(),
            Token::Whitespace => "whitespace".to_string(),
            Token::Comment => "comment".to_string(),
            Token::MultilineComment => "multiline comment".to_string(),
            Token::Error(s) => s.to_string(),
        };

        write!(f, "{}", s)
    }
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
