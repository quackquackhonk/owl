// use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    input::{SpannedInput, Stream},
    prelude::*,
};
use logos::{Logos, Span};

use super::{
    ast::{BinOp, Declaration, Expression, Name, Program, Statement, Type, UnOp},
    error::OwlParseResult,
    lexer,
    lexer::Token,
    Spanned,
};

type ParserInput<'tokens> = SpannedInput<Token, Span, &'tokens [(Token, Span)]>;
type ParserError<'tokens> = extra::Err<Rich<'tokens, Token, Span>>;

pub fn owl_parser(source: &str) -> OwlParseResult<Program> {
    let tokens = dbg!(lexer(source));

    let input_stream = tokens.spanned(source.len()..source.len());

    let prog = parse_program().parse(input_stream).into_result();

    todo!()
}

/// Entry point for the Owl parser
fn parse_program<'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens>, Program, ParserError<'tokens>> {
    let decs = parse_decl()
        .map(|decl| vec![decl])
        .then_ignore(just(Token::SemiColon))
        .foldl(
            parse_decl().then_ignore(just(Token::SemiColon)).repeated(),
            |mut decs, decl| {
                decs.push(decl);
                decs
            },
        )
        .labelled("program declarations");

    decs.then(parse_expr().labelled("program expression"))
        .map(|(decs, expr)| Program::new(decs, expr))
}

fn parse_expr<'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens>, Spanned<Expression>, ParserError<'tokens>> {
    recursive(|expr| {
        // parsing function expressions
        let fun_expr = just(Token::At)
            .ignore_then(parse_name().repeated().collect::<Vec<Spanned<Name>>>())
            .then_ignore(just(Token::BigArrow))
            .then(expr.clone())
            .map(|(args, body)| Expression::Function(args, Box::new(body)));

        let value = select! {
            Token::Bool(b) => Expression::Bool(b),
            Token::Num(n) => Expression::Int(n.parse().expect("This will be a number!")),
            Token::ID(id) => Expression::Var(id),
            Token::Unit => Expression::Unit
        }
        .or(fun_expr)
        .labelled("expr-value")
        .map_with(|v, e| (v, e.span()));

        // atoms are values or parenthesized expressions
        let atom = value
            .or(expr
                .clone()
                .delimited_by(just(Token::LParen), just(Token::RParen)))
            .boxed();

        // Function calls
        let fun_call = atom
            .clone()
            .then(atom.clone())
            .map(|(func, arg)| Expression::FuncCall(Box::new(func), Box::new(arg)))
            .map_with(|fc, e| (fc, e.span()))
            .boxed();

        let element = atom.or(fun_call);

        // Unary operations
        let unop = parse_unary_op()
            .repeated()
            .foldr_with(element, |op, rhs, e| {
                (Expression::UnaryOp(op, Box::new(rhs)), e.span())
            })
            .boxed();

        // Binary operations
        // multiply / divide
        let op = select! {
            Token::Mult => BinOp::Mul,
            Token::Divide => BinOp::Div
        };
        let product = unop
            .clone()
            .foldl_with(op.then(unop).repeated(), |left, (op, right), e| {
                (
                    Expression::BinaryOp(op, Box::new(left), Box::new(right)),
                    e.span(),
                )
            })
            .boxed();

        // add / subtract
        let op = select! {
            Token::Plus => BinOp::Add,
            Token::Minus => BinOp::Sub
        };
        let sum = product
            .clone()
            .foldl_with(op.then(product).repeated(), |left, (op, right), e| {
                (
                    Expression::BinaryOp(op, Box::new(left), Box::new(right)),
                    e.span(),
                )
            })
            .boxed();

        // relative operators
        let op = select! {
            Token::Eq => BinOp::Eq,
            Token::Lt => BinOp::Lt,
            Token::Gt => BinOp::Gt,
        }
        .or(just(Token::Lt).then(just(Token::Eq)).to(BinOp::LtEq))
        .or(just(Token::Gt).then(just(Token::Eq)).to(BinOp::GtEq))
        .or(just(Token::Bang).then(just(Token::Eq)).to(BinOp::Neq));
        let relop = sum
            .clone()
            .foldl_with(op.then(sum).repeated(), |left, (op, right), e| {
                (
                    Expression::BinaryOp(op, Box::new(left), Box::new(right)),
                    e.span(),
                )
            })
            .boxed();

        // logical ops
        let op = select! {
            Token::And => BinOp::And,
            Token::Or => BinOp::Or
        };
        let logicop = relop
            .clone()
            .foldl_with(op.then(relop).repeated(), |left, (op, right), e| {
                (
                    Expression::BinaryOp(op, Box::new(left), Box::new(right)),
                    e.span(),
                )
            })
            .boxed();

        // Dollar function application
        let binop = logicop
            .clone()
            .foldl_with(
                just(Token::Dollar).ignore_then(logicop).repeated(),
                |func, arg, e| {
                    (
                        Expression::FuncCall(Box::new(func), Box::new(arg)),
                        e.span(),
                    )
                },
            )
            .boxed();

        // NOTE: We only return binop since any binop can just be an element by itself
        let simple_expr = binop.labelled("expression").as_context();
        // Conditionals
        let conditional = just(Token::If)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Then))
            .then(expr.clone())
            .then_ignore(just(Token::Else))
            .then(expr.clone())
            .map(|((cond, then), r#else)| {
                Expression::Conditional(Box::new(cond), Box::new(then), Box::new(r#else))
            })
            .map_with(|c, e| (c, e.span()))
            .boxed();

        // Blocks
        let block = parse_stmt()
            .separated_by(just(Token::SemiColon))
            .collect::<Vec<Spanned<Statement>>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map_with(|stmts, e| (Expression::Block(stmts), e.span()))
            .boxed();

        simple_expr.or(conditional).or(block)
    })
}

fn parse_unary_op<'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens>, UnOp, ParserError<'tokens>> {
    select! {
        Token::Bang => UnOp::Not,
        Token::Minus => UnOp::Neg
    }
}

fn parse_name<'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens>, Spanned<Name>, ParserError<'tokens>> {
    select! {
        Token::ID(id) => id
    }
    .labelled("name")
    .map_with(|n, e| (n, e.span()))
}

fn parse_decl<'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens>, Spanned<Declaration>, ParserError<'tokens>> {
    // Value and Function declarations
    let value_dec = just(Token::Let)
        .ignore_then(parse_name())
        .then(parse_name().repeated().collect::<Vec<Spanned<Name>>>())
        .then_ignore(just(Token::Be))
        .then(parse_expr())
        .map_with(|((name, args), body), e| (Declaration::Value(name, args, body), e.span()));

    // Type declarations
    let type_dec = just(Token::Typ)
        .ignore_then(parse_name())
        .then_ignore(just(Token::Is))
        .then(parse_type())
        .map_with(|(name, body), e| (Declaration::Type(name, body), e.span()));

    value_dec.or(type_dec).labelled("declaration")
}

fn parse_type<'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens>, Spanned<Type>, ParserError<'tokens>> {
    recursive(|typ| {
        let simple_type = select! {
            Token::Unit => Type::Unit,
            Token::ID(id) if id == "Int" => Type::Int,
            Token::ID(id) if id == "Bool" => Type::Bool,
            Token::ID(id) => Type::Var(id),
        }
        .labelled("type value")
        .map_with(|v, e| (v, e.span()));

        let atom = simple_type
            .or(typ
                .clone()
                .delimited_by(just(Token::LParen), just(Token::RParen)))
            .boxed();

        let arrow_type = atom.clone().foldl_with(
            just(Token::Arrow).then(atom).repeated(),
            |left, (_, right), e| (Type::Arrow(Box::new(left), Box::new(right)), e.span()),
        );

        arrow_type
    })
}

pub fn parse_stmt<'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens>, Spanned<Statement>, ParserError<'tokens>> {
    let expr_stmt = parse_expr().map(|(expr, expr_sp)| (Statement::Expr(expr), expr_sp));
    let decl_stmt = parse_decl().map(|(decl, decl_sp)| (Statement::Decl(decl), decl_sp));

    expr_stmt.or(decl_stmt)
}
