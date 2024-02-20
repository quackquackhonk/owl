use std::{fs::File, io::Read, iter::Peekable};

use super::{
    ast::{Declaration, Expression, Name, Program, Type, UnOp},
    error::{produce_error_report, Recoverable, Unrecoverable},
    lexer::{lexer, Token},
    span_add, Span, Spanned,
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
    let mut errors = vec![];
    let decls = parse_declarations(lex, &mut errors)?;
    let expr = parse_expression(lex, &mut errors)?;

    Ok((Program::new(decls, expr), errors))
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
        Some((found, sp)) if found == tok => Ok(sp),
        Some(found) => {
            errors.push(Recoverable::UnexpectedToken {
                expected: Some(tok),
                found: found.clone(),
            });

            Ok(found.1)
        }
        None => Err(Unrecoverable::EndOfInput.into()),
    }
}

/// Parses semicolon separated declarations, returning the declarations and any recoverable errors
///
/// * `lex`: the token iterator
fn parse_declarations(
    lex: &mut InputIter,
    _errors: &mut Vec<Recoverable>,
) -> anyhow::Result<Vec<Spanned<Declaration>>> {
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
                errors.push(Recoverable::LexerError((bad, (sp.start..sp.end))))
            }
            Some((_, _)) => break,
            // early EOI is unrecoverable
            None => return Err(Unrecoverable::EndOfInput.into()),
        };
    }

    Ok(decs)
}

fn parse_name(lex: &mut InputIter, errors: &mut Vec<Recoverable>) -> anyhow::Result<Spanned<Name>> {
    match lex.next() {
        Some((Token::ID(name), sp)) => Ok((name, sp)),
        Some(found) => {
            errors.push(Recoverable::UnexpectedToken {
                expected: Some(Token::ID("".to_string())),
                found: found.clone(),
            });
            Ok(("".to_string(), found.1))
        }
        None => Err(Unrecoverable::EndOfInput.into()),
    }
}

fn parse_args(
    lex: &mut InputIter,
    _errors: &mut Vec<Recoverable>,
) -> anyhow::Result<Vec<Spanned<Name>>> {
    let mut names = vec![];

    loop {
        match lex.peek() {
            Some((Token::ID(_), _)) => {
                // eat and push the token
                if let Some((Token::ID(name), sp)) = lex.next() {
                    names.push((name, sp));
                }
            }
            Some(_) => break,
            None => return Err(Unrecoverable::EndOfInput.into()),
        }
    }

    Ok(names)
}

fn parse_value_declaration(
    lex: &mut InputIter,
    errors: &mut Vec<Recoverable>,
) -> anyhow::Result<Spanned<Declaration>> {
    let start = expect_tok(Token::Let, lex, errors)?;
    let name = parse_name(lex, errors)?;
    let args = parse_args(lex, errors)?;
    let _ = expect_tok(Token::Be, lex, errors)?;
    let (expr, end) = parse_expression(lex, errors)?;

    Ok((
        Declaration::Value(name, args, (expr, end.clone())),
        span_add(start, end),
    ))
}

fn parse_type_declaration(
    lex: &mut InputIter,
    errors: &mut Vec<Recoverable>,
) -> anyhow::Result<Spanned<Declaration>> {
    let start = expect_tok(Token::Typ, lex, errors)?;
    let name = parse_name(lex, errors)?;
    let _ = expect_tok(Token::Is, lex, errors)?;
    let (typ, typ_sp) = parse_type(lex, errors)?;

    Ok((
        Declaration::Type(name, (typ, typ_sp.clone())),
        span_add(start, typ_sp),
    ))
}

fn parse_type(lex: &mut InputIter, errors: &mut Vec<Recoverable>) -> anyhow::Result<Spanned<Type>> {
    // parse a type
    let left = match lex.next() {
        Some((Token::Unit, sp)) => (Type::Unit, sp),
        Some((Token::ID(id), sp)) if id == "Int" => (Type::Int, sp),
        Some((Token::ID(id), sp)) if id == "Bool" => (Type::Int, sp),
        Some((Token::ID(id), sp)) => (Type::Var(id), sp),
        Some((Token::LParen, _)) => {
            let typ = parse_type(lex, errors)?;
            let _ = expect_tok(Token::RParen, lex, errors)?;
            typ
        }
        Some(found) => return Err(Unrecoverable::InvalidType(found).into()),
        None => return Err(Unrecoverable::EndOfInput.into()),
    };

    // check for an arrow
    if let Some((Token::Arrow, _)) = lex.peek() {
        // eat the arrow
        lex.next();

        // do a recursive call to grab the next type
        let right = parse_type(lex, errors)?;

        Ok((
            Type::Arrow(Box::new(left.clone()), Box::new(right.clone())),
            span_add(left.1, right.1),
        ))
    } else {
        Ok(left)
    }
}

fn parse_expression(
    lex: &mut InputIter,
    errors: &mut Vec<Recoverable>,
) -> anyhow::Result<Spanned<Expression>> {
    // TODO: Unary operations
    // TODO: Binary operations
    // TODO: Function calls
    // TODO: blocks
    let left = match lex.next() {
        // Atoms
        Some((Token::Unit, sp)) => (Expression::Unit, sp),
        Some((Token::Num(x), sp)) => {
            let i: isize = x.parse()?;
            (Expression::Int(i), sp)
        }
        Some((Token::Bool(b), sp)) => (Expression::Bool(b), sp),
        Some((Token::ID(name), sp)) => (Expression::Var(name), sp),
        // Function expression
        Some((Token::At, sp)) => {
            let args = parse_args(lex, errors)?;
            let _ = expect_tok(Token::BigArrow, lex, errors)?;
            let (ex, ex_sp) = parse_expression(lex, errors)?;
            (
                Expression::Function(args, Box::new((ex, ex_sp.clone()))),
                span_add(sp, ex_sp),
            )
        }
        Some((Token::LParen, _)) => {
            let ex = parse_expression(lex, errors)?;
            let _ = expect_tok(Token::RParen, lex, errors)?;
            ex
        }
        // Unary operations
        Some((Token::Bang, start)) => {
            // Boolean NOT
            let (ex, ex_sp) = parse_expression(lex, errors)?;
            (
                Expression::UnaryOp(UnOp::Not, Box::new((ex, ex_sp.clone()))),
                span_add(start, ex_sp),
            )
        }
        Some((Token::Minus, start)) => {
            // Negation
            let (ex, ex_sp) = parse_expression(lex, errors)?;
            (
                Expression::UnaryOp(UnOp::Neg, Box::new((ex, ex_sp.clone()))),
                span_add(start, ex_sp),
            )
        }
        // Conditionals
        Some((Token::If, start)) => {
            let cond = parse_expression(lex, errors)?;
            let _ = expect_tok(Token::Then, lex, errors)?;
            let then = parse_expression(lex, errors)?;
            let _ = expect_tok(Token::Else, lex, errors)?;
            let (els, els_sp) = parse_expression(lex, errors)?;

            (
                Expression::Conditional(
                    Box::new(cond),
                    Box::new(then),
                    Box::new((els, els_sp.clone())),
                ),
                span_add(start, els_sp),
            )
        }
        Some(_) => todo!(),
        None => return Err(Unrecoverable::EndOfInput.into()),
    };

    Ok(left)
}

fn parse_atom(
    lex: &mut InputIter,
    errors: &mut Vec<Recoverable>,
) -> anyhow::Result<Spanned<Expression>> {
    match lex.peek() {
        Some((Token::Unit, _)) => {
            if let Some((_, sp)) = lex.next() {
                Ok((Expression::Unit, sp))
            } else {
                unreachable!()
            }
        }
        Some((Token::Num(_), _)) => {
            if let Some((Token::Num(x), sp)) = lex.next() {
                let x: isize = x.parse()?;
                Ok((Expression::Int(x), sp))
            } else {
                unreachable!()
            }
        }
        Some((Token::Bool(_), _)) => {
            if let Some((Token::Bool(b), sp)) = lex.next() {
                Ok((Expression::Bool(b), sp))
            } else {
                unreachable!()
            }
        }
        Some(_) => todo!(),
        None => Err(Unrecoverable::EndOfInput.into()),
    }
}
