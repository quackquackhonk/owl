use std::{fs::File, io::Read, iter::Peekable};

use itertools::peek_nth;
use logos::{Lexer, Logos, SpannedIter};

// use ariadne::{Color, Label, Report, ReportKind, Source};
use super::{
    ast::{Declaration, Expression, Program},
    error::OwlParseError,
    lexer::{lexer, Token},
    Spanned,
};

type InputIter = Peekable<std::vec::IntoIter<Spanned<Token>>>;

/// Entry point for the Owl parser
///
/// * `path`: The path to the source file
pub fn owl_program_parser(path: &str) -> anyhow::Result<Program> {
    let mut file = File::open(path)?;
    let mut source = String::new();
    file.read_to_string(&mut source)?;

    let mut lex = lexer(&source).into_iter().peekable();

    let (prog, errs) = parse_program(&mut lex)?;
    
    // TODO: Produce error report with ariadne

    Ok(prog)
}

fn parse_program(lex: &mut InputIter) -> anyhow::Result<(Program, Vec<OwlParseError>)> {
    let mut errors = vec![];
    let decls = parse_declarations(lex, &mut errors)?;
    let expr = parse_expression(lex, &mut errors)?;

    Ok((Program::new(decls, expr), errors))
}

/// Attempts to parse the given token exactly.
///
/// If another token is found, we keep parsing as if we found the requested token.
fn expect_tok(tok: Token, lex: &mut InputIter, errors: &mut Vec<OwlParseError>) -> anyhow::Result<()> {
    match lex.next() {
        Some((found, _)) if found == tok => Ok(()),
        Some((other, sp)) => {
            errors.push(OwlParseError::UnexpectedToken {
                expected: tok,
                found: (other, sp),
            });

            Ok(())
        },
        None => Err(OwlParseError::EndOfInput.into()),
    }
}

/// Parses semicolon separated declarations, returning the declarations and any recoverable errors
///
/// * `lex`: the token iterator
fn parse_declarations(lex: &mut InputIter, errors: &mut Vec<OwlParseError>) -> anyhow::Result<Vec<Spanned<Declaration>>> {
    let mut errors = vec![];
    let mut decs: Vec<Spanned<Declaration>> = vec![];

    loop {
        match lex.peek() {
            Some((Token::Let, _)) => {
                // parse a value Declaration
                let value_dec = parse_value_declaration(lex, &mut errors)?;
                decs.push(value_dec);
                // parse a semicolon
                let _ = expect_tok(Token::SemiColon, lex, &mut errors)?;
            }
            Some((Token::Typ, _)) => {
                // parse a type Declaration
                let value_dec = parse_type_declaration(lex, &mut errors)?;
                decs.push(value_dec);
                // parse a semicolon
                let _ = expect_tok(Token::SemiColon, lex, &mut errors)?;
            }
            Some((Token::Error(_), _)) => {
                let Some((Token::Error(bad), sp)) = lex.next() else {
                    unreachable!()
                };
                errors.push(OwlParseError::LexerError((bad, (sp.start..sp.end))))
            }
            Some((_, _)) => break,
            // early EOI is unrecoverable
            None => return Err(OwlParseError::EndOfInput.into()),
        };
    }

    Ok(decs)
}

fn parse_value_declaration(lex: &mut InputIter, errors: &mut Vec<OwlParseError>) -> anyhow::Result<Spanned<Declaration>> {
    let _ = expect_tok(Token::Let, lex, errors)?;

    todo!()
}

fn parse_type_declaration(lex: &mut InputIter, errors: &mut Vec<OwlParseError>) -> anyhow::Result<Spanned<Declaration>> {
    todo!()
}

fn parse_expression(lex: &mut InputIter, errors: &mut Vec<OwlParseError>) -> anyhow::Result<Spanned<Expression>> {
    todo!()
}
