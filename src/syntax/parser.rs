use std::{fs::File, io::Read, iter::Peekable};

use super::{
    ast::{Declaration, Expression, Ident, Program, Type},
    error::{produce_error_report, Recoverable, Unrecoverable},
    lexer::{lexer, Token},
    Span, Spanned,
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

    if errs.is_empty() {
        Ok(prog)
    } else {
        Err(produce_error_report(errs).into())
    }
}

fn parse_program(lex: &mut InputIter) -> anyhow::Result<(Program, Vec<Recoverable>)> {
    todo!()
}

/// Attempts to parse the given token exactly.
///
/// If another token is found, we keep parsing as if we found the requested token.
fn expect_tok(
    tok: Token,
    lex: &mut InputIter,
    errors: &mut Vec<Recoverable>,
) -> anyhow::Result<Span> {
    match lex.next() {
        Some(found) if found.val == tok => Ok(found.span),
        Some(found) => {
            errors.push(Recoverable::UnexpectedToken {
                expected: Some(tok),
                found: found.clone(),
            });

            Ok(found.span)
        }
        None => Err(Unrecoverable::EndOfInput.into()),
    }
}

fn parse_declarations(
    lex: &mut InputIter,
    _errors: &mut Vec<Recoverable>,
) -> anyhow::Result<Vec<Spanned<Declaration>>> {
    todo!()
}

fn parse_name(lex: &mut InputIter, errors: &mut Vec<Recoverable>) -> anyhow::Result<Spanned<Ident>> {
    match lex.next() {
        Some(Spanned { val: Token::ID(name), span }) => Ok(Spanned::new(name, span)),
        Some(found) => {
            errors.push(Recoverable::UnexpectedToken {
                expected: Some(Token::ID("".to_string())),
                found: found.clone(),
            });
            Ok(Spanned::new("".to_string(), found.span))
        }
        None => Err(Unrecoverable::EndOfInput.into()),
    }
}

fn parse_args(
    lex: &mut InputIter,
    _errors: &mut Vec<Recoverable>,
) -> anyhow::Result<Vec<Spanned<Ident>>> {
    todo!()
}

fn parse_type(lex: &mut InputIter, errors: &mut Vec<Recoverable>) -> anyhow::Result<Spanned<Type>> {
    todo!()
}

fn parse_expression(
    lex: &mut InputIter,
    errors: &mut Vec<Recoverable>,
) -> anyhow::Result<Spanned<Expression>> {
    todo!()
}

fn parse_atom(
    lex: &mut InputIter,
    errors: &mut Vec<Recoverable>,
) -> anyhow::Result<Spanned<Expression>> {
    todo!()
}
