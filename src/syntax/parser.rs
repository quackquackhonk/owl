use std::{fs::File, io::Read, iter::Peekable};

use super::{
    ast::{Arg, BinOp, Declaration, Expression, Ident, Program, ReplStatement, Statement, Type},
    error::{Recoverable, Unrecoverable},
    lexer::{lexer, Token},
    span::{Span, Spanned},
};

type InputIter = Peekable<std::vec::IntoIter<Spanned<Token>>>;

type ParseResult<T> = Result<T, Unrecoverable>;

/// Entry point for the Owl parser
///
/// * `path`: The path to the source file
pub fn owl_program_parser(path: &str) -> anyhow::Result<Program> {
    let mut file = File::open(path)?;
    let mut source = String::new();
    file.read_to_string(&mut source)?;

    let mut lex = dbg!(lexer(&source)).into_iter().peekable();

    match parse_program(&mut lex) {
        Ok((prog, errs)) if errs.is_empty() => Ok(prog),
        Ok((_, errs)) => {
            let u = Unrecoverable::Finished(errs);
            let _ = u.report(path, &source)?;
            Err(u.into())
        }
        Err(u) => {
            let _ = u.report(path, &source)?;
            Err(u.into())
        }
    }
}

pub fn owl_repl_parser(lex: &mut InputIter) -> ParseResult<ReplStatement> {
    let mut errors: Vec<Recoverable> = vec![];
    match lex.peek() {
        Some(Spanned(Token::Let, _)) | Some(Spanned(Token::Fun, _)) => {
            if let Some(d) = parse_declaration(lex, &mut errors)? {
                let sp = d.span();
                Ok(ReplStatement::Stmt(Spanned::new(Statement::Decl(d), sp)))
            } else {
                todo!()
            }
        }
        Some(_) => {
            let e = parse_expr(lex, &mut errors)?;
            let sp = e.span();
            if let Some(Spanned(Token::SemiColon, _)) = lex.peek() {
                Ok(ReplStatement::Stmt(Spanned::new(Statement::Expr(e), sp)))
            } else {
                Ok(ReplStatement::Expr(e))
            }
        }
        None => Err(Unrecoverable::EndOfInput),
    }
}

fn parse_program(lex: &mut InputIter) -> ParseResult<(Program, Vec<Recoverable>)> {
    let mut decls: Vec<Spanned<Declaration>> = vec![];
    let mut errors: Vec<Recoverable> = vec![];

    loop {
        match parse_declaration(lex, &mut errors) {
            Ok(Some(decl)) => decls.push(decl),
            Ok(None) => break,
            Err(err) => return Err(err),
        }
    }
    Ok((Program::new(decls), errors))
}

/// Attempts to parse the given token exactly.
///
/// If another token is found, we keep parsing as if we found the requested token.
fn expect_tok(tok: Token, lex: &mut InputIter, errors: &mut Vec<Recoverable>) -> ParseResult<Span> {
    match lex.next() {
        Some(found) if found.0 == tok => Ok(found.span()),
        Some(found) => {
            errors.push(Recoverable::UnexpectedToken {
                expected: tok,
                found: found.clone(),
            });

            Ok(found.span())
        }
        None => Err(Unrecoverable::EndOfInput.into()),
    }
}

fn parse_ident(lex: &mut InputIter, errors: &mut Vec<Recoverable>) -> ParseResult<Spanned<Ident>> {
    match lex.next() {
        Some(Spanned(Token::ID(name), span)) => Ok(Spanned::new(name, span)),
        Some(found) => {
            errors.push(Recoverable::UnexpectedToken {
                expected: Token::ID("".to_string()),
                found: found.clone(),
            });
            Ok(Spanned::new("".to_string(), found.span()))
        }
        None => Err(Unrecoverable::EndOfInput.into()),
    }
}

fn parse_arg(lex: &mut InputIter, errors: &mut Vec<Recoverable>) -> ParseResult<Spanned<Arg>> {
    let id = parse_ident(lex, errors)?;
    // check for a type
    if let Some(Spanned(Token::Colon, _)) = lex.peek() {
        let _ = expect_tok(Token::Colon, lex, errors)?;
        let ty = parse_type(lex, errors)?;
        let sp = id.span() + ty.span();
        Ok(Spanned::new(Arg(id, Some(ty)), sp))
    } else {
        let sp = id.span();
        Ok(Spanned::new(Arg(id, None), sp))
    }
}

fn parse_arg_list(
    lex: &mut InputIter,
    errors: &mut Vec<Recoverable>,
) -> ParseResult<Vec<Spanned<Arg>>> {
    let arg = parse_arg(lex, errors)?;

    if let Some(Spanned(Token::Comma, _)) = lex.peek() {
        let _ = expect_tok(Token::Comma, lex, errors)?;
        let mut args = parse_arg_list(lex, errors)?;
        args.insert(0, arg);
        Ok(args)
    } else {
        Ok(vec![arg])
    }
}

fn parse_declaration(
    lex: &mut InputIter,
    errors: &mut Vec<Recoverable>,
) -> ParseResult<Option<Spanned<Declaration>>> {
    match lex.peek() {
        Some(Spanned(Token::Let, _)) => {
            let start = expect_tok(Token::Let, lex, errors)?;
            let lhs = parse_arg(lex, errors)?;
            let _ = expect_tok(Token::Assign, lex, errors)?;
            let rhs = parse_expr(lex, errors)?;
            let end = expect_tok(Token::SemiColon, lex, errors)?;
            Ok(Some(Spanned(Declaration::Value(lhs, rhs), start + end)))
        }
        Some(Spanned(Token::Fun, _)) => {
            let start = expect_tok(Token::Fun, lex, errors)?;
            let id = parse_ident(lex, errors)?;

            let _ = expect_tok(Token::LParen, lex, errors)?;
            // check for immediate close
            let args = if let Some(Spanned(Token::RParen, _)) = lex.peek() {
                let _ = expect_tok(Token::RParen, lex, errors)?;
                vec![]
            } else {
                let args = parse_arg_list(lex, errors)?;
                let _ = expect_tok(Token::RParen, lex, errors)?;
                args
            };

            let sp = id.span();
            let fun_arg = match lex.peek() {
                Some(Spanned(Token::Arrow, _)) => {
                    let _ = expect_tok(Token::Arrow, lex, errors)?;
                    Spanned::new(Arg(id, Some(parse_type(lex, errors)?)), sp)
                }
                _ => Spanned::new(Arg(id, None), sp),
            };

            let body = match lex.peek() {
                Some(Spanned(Token::Assign, _)) => {
                    let _ = expect_tok(Token::Assign, lex, errors)?;
                    let e = parse_expr(lex, errors)?;
                    let _ = expect_tok(Token::SemiColon, lex, errors)?;
                    e
                }
                Some(Spanned(Token::LBrace, _)) => parse_block(lex, errors)?,
                Some(other) => {
                    // TODO: This should also be Recoverable
                    // we can assume the body is {} and then keep going
                    return Err(Unrecoverable::ExpectedTokens(
                        vec![Token::Assign, Token::LBrace],
                        other.clone(),
                    ));
                }
                None => return Err(Unrecoverable::EndOfInput),
            };

            let sp = start + body.span();
            Ok(Some(Spanned(
                Declaration::Function(fun_arg, args, body),
                sp,
            )))
        }
        Some(_) | None => Ok(None),
    }
}

fn parse_block(
    lex: &mut InputIter,
    errors: &mut Vec<Recoverable>,
) -> ParseResult<Spanned<Expression>> {
    let mut stmts = vec![];
    let start = expect_tok(Token::LBrace, lex, errors)?;

    let expr = loop {
        match lex.peek() {
            // check if its a declaration
            Some(Spanned(Token::Let, _)) | Some(Spanned(Token::Fun, _)) => {
                match parse_declaration(lex, errors)? {
                    Some(decl) => stmts.push(Statement::Decl(decl)),
                    None => continue,
                }
            }
            // check if the block is ending
            Some(Spanned(Token::RBrace, _)) => break None,
            // If anything else, parse an expression
            Some(Spanned(_, _)) => {
                let e = parse_expr(lex, errors)?;
                // Check if this is an expr or a statement
                match lex.peek() {
                    Some(Spanned(Token::SemiColon, _)) => {
                        // statement
                        let _ = expect_tok(Token::SemiColon, lex, errors)?;
                        stmts.push(Statement::Expr(e));
                    }
                    Some(Spanned(Token::RBrace, _)) => {
                        // expressions
                        break Some(Box::new(e));
                    }
                    Some(other) => {
                        return Err(Unrecoverable::ExpectedTokens(
                            vec![Token::SemiColon, Token::RBrace],
                            other.clone(),
                        ))
                    }
                    None => return Err(Unrecoverable::EndOfInput),
                }
            }
            None => return Err(Unrecoverable::EndOfInput),
        };
    };

    let end = expect_tok(Token::RBrace, lex, errors)?;

    Ok(Spanned(Expression::Block(stmts, expr), start + end))
}

fn parse_type(lex: &mut InputIter, errors: &mut Vec<Recoverable>) -> ParseResult<Spanned<Type>> {
    let lhs = match lex.next() {
        Some(Spanned(Token::ID(x), span)) if x == "int" => Spanned::new(Type::Int, span),
        Some(Spanned(Token::ID(x), span)) if x == "bool" => Spanned::new(Type::Bool, span),
        Some(Spanned(Token::ID(x), span)) if x == "unit" => Spanned::new(Type::Unit, span),
        Some(Spanned(Token::LParen, _)) => {
            let ty = parse_type(lex, errors)?;
            let _ = expect_tok(Token::RParen, lex, errors)?;
            ty
        }
        Some(found) => return Err(Unrecoverable::ExpectedKind("type".to_string(), found)),
        None => return Err(Unrecoverable::EndOfInput),
    };

    if let Some(Spanned(Token::Arrow, _)) = lex.peek() {
        let _ = expect_tok(Token::Arrow, lex, errors)?;
        let rhs = parse_type(lex, errors)?;
        let sp = lhs.span() + rhs.span();
        Ok(Spanned(Type::Arrow(Box::new(lhs), Box::new(rhs)), sp))
    } else {
        Ok(lhs)
    }
}

fn parse_atom(
    lex: &mut InputIter,
    errors: &mut Vec<Recoverable>,
) -> ParseResult<Spanned<Expression>> {
    let at = match lex.next() {
        Some(Spanned(Token::Bool(b), span)) => Ok(Spanned::new(Expression::Bool(b), span)),
        Some(Spanned(Token::Num(n), span)) => Ok(Spanned::new(Expression::Int(n), span)),
        Some(Spanned(Token::ID(id), span)) => Ok(Spanned::new(Expression::Var(id), span)),
        Some(Spanned(Token::LParen, st)) => {
            if let Some(Spanned(Token::RParen, _)) = lex.peek() {
                let en = expect_tok(Token::RParen, lex, errors)?;
                Ok(Spanned::new(Expression::Unit, st + en))
            } else {
                let expr = parse_expr(lex, errors)?;
                let _ = expect_tok(Token::RParen, lex, errors)?;
                Ok(expr)
            }
        }
        Some(Spanned(Token::LBrace, _)) => {
            let expr = parse_block(lex, errors)?;
            let _ = expect_tok(Token::RBrace, lex, errors)?;
            Ok(expr)
        }
        Some(found) => {
            // this is probably Recoverable, but its a later problem ngl
            Err(Unrecoverable::ExpectedKind(String::from("atom"), found))
        }
        None => Err(Unrecoverable::EndOfInput),
    }?;

    // peek for parens
    if let Some(Spanned(Token::LParen, _)) = lex.peek() {
        let _ = expect_tok(Token::LParen, lex, errors)?;
        let exprs = parse_expr_list(lex, errors)?;
        let end = expect_tok(Token::RParen, lex, errors)?;
        let sp = at.span() + end;
        Ok(Spanned(
            Expression::FuncCall(Box::new(at), Box::new(exprs)),
            sp,
        ))
    } else {
        Ok(at)
    }
}

fn parse_expr(
    lex: &mut InputIter,
    errors: &mut Vec<Recoverable>,
) -> ParseResult<Spanned<Expression>> {
    let at = parse_atom(lex, errors)?;

    match lex.peek() {
        // Mult and Div have high precedence
        Some(Spanned(Token::Mult, _)) => parse_op_rhs(at, BinOp::Mul, lex, errors),
        Some(Spanned(Token::Divide, _)) => parse_op_rhs(at, BinOp::Div, lex, errors),
        // Comparison ops have the same precedence
        Some(Spanned(Token::Eq, _)) => parse_op_rhs(at, BinOp::Neq, lex, errors),
        Some(Spanned(Token::Neq, _)) => parse_op_rhs(at, BinOp::Eq, lex, errors),
        Some(Spanned(Token::Lt, _)) => parse_op_rhs(at, BinOp::Lt, lex, errors),
        Some(Spanned(Token::LtEq, _)) => parse_op_rhs(at, BinOp::LtEq, lex, errors),
        Some(Spanned(Token::Gt, _)) => parse_op_rhs(at, BinOp::Gt, lex, errors),
        Some(Spanned(Token::GtEq, _)) => parse_op_rhs(at, BinOp::GtEq, lex, errors),
        // + and - have lower precedence
        Some(Spanned(Token::Plus, _)) => parse_op_rhs(at, BinOp::Add, lex, errors),
        Some(Spanned(Token::Minus, _)) => parse_op_rhs(at, BinOp::Sub, lex, errors),
        // Bool ops have the same precedence
        Some(Spanned(Token::And, _)) => parse_op_rhs(at, BinOp::And, lex, errors),
        Some(Spanned(Token::Or, _)) => parse_op_rhs(at, BinOp::Or, lex, errors),
        Some(_) | None => Ok(at),
    }
}

fn parse_op_rhs(
    lhs: Spanned<Expression>,
    op: BinOp,
    lex: &mut InputIter,
    errors: &mut Vec<Recoverable>,
) -> ParseResult<Spanned<Expression>> {
    let _ = lex.next();
    let rhs = parse_expr(lex, errors)?;
    let sp = lhs.span() + rhs.span();
    Ok(Spanned(
        Expression::BinaryOp(op, Box::new(lhs), Box::new(rhs)),
        sp,
    ))
}

fn parse_expr_list(
    lex: &mut InputIter,
    errors: &mut Vec<Recoverable>,
) -> ParseResult<Vec<Spanned<Expression>>> {
    let expr = parse_expr(lex, errors)?;

    if let Some(Spanned(Token::Comma, _)) = lex.peek() {
        let _ = expect_tok(Token::Comma, lex, errors)?;
        let mut exprs = parse_expr_list(lex, errors)?;
        exprs.insert(0, expr);
        Ok(exprs)
    } else {
        Ok(vec![expr])
    }
}
