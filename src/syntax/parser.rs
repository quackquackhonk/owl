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

type ParseResult<T> = anyhow::Result<(T, Vec<OwlParseError>)>;
type InputIter = Peekable<std::vec::IntoIter<Spanned<Token>>>;

/// Entry point for the Owl parser
///
/// * `path`: The path to the source file
pub fn owl_program_parser(path: &str) -> ParseResult<Program> {
    let mut file = File::open(path)?;
    let mut source = String::new();
    file.read_to_string(&mut source)?;

    let mut lex = lexer(&source).into_iter().peekable();

    parse_program(&mut lex)
}

fn parse_program(lex: &mut InputIter) -> Result<(Program, Vec<OwlParseError>), anyhow::Error> {
    let (decls, errors) = parse_declarations(lex)?;
    let mut errs = errors;
    let (expr, mut errors) = parse_expression(lex)?;
    errs.append(&mut errors);

    Ok((Program::new(decls, expr), errs))
}

fn parse_semicolon(lex: &mut InputIter) -> ParseResult<()> {
    todo!()
}
/// Parses semicolon separated declarations, returning the declarations and any recoverable errors
///
/// * `lex`: the token iterator
fn parse_declarations(lex: &mut InputIter) -> ParseResult<Vec<Spanned<Declaration>>> {
    let mut errors = vec![];
    let mut decs: Vec<Spanned<Declaration>> = vec![];

    let last = loop {
        match lex.peek() {
            Some((Token::Let, sp)) => {
                // parse a value Declaration
                let (value_dec, mut errs) = parse_value_declaration(lex)?;
                errors.append(&mut errs);
                decs.push(value_dec);
                // parse a semicolon
                let (_, mut errs) = parse_semicolon(lex)?;
                errors.append(&mut errs);
            }
            Some((Token::Typ, sp)) => {
                // parse a type Declaration
                let (value_dec, mut errs) = parse_type_declaration(lex)?;
                errors.append(&mut errs);
                decs.push(value_dec);
                // parse a semicolon
                let (_, mut errs) = parse_semicolon(lex)?;
                errors.append(&mut errs);
            }
            Some((Token::Error(_), _)) => {
                let Some((Token::Error(bad), sp)) = lex.next() else {
                    unreachable!()
                };
                errors.push(OwlParseError::LexerError((bad, (sp.start..sp.end))))
            }
            Some((other, sp)) => {
                // no more declarations, we need to parse an expression
                break other;
            }
            // early EOI is unrecoverable
            None => return Err(OwlParseError::EndOfInput.into()),
        };
    };

    Ok((decs, errors))
}

fn parse_value_declaration(lex: &mut InputIter) -> ParseResult<Spanned<Declaration>> {
    todo!()
}

fn parse_type_declaration(lex: &mut InputIter) -> ParseResult<Spanned<Declaration>> {
    todo!()
}

fn parse_expression(lex: &mut InputIter) -> ParseResult<Spanned<Expression>> {
    todo!()
}
