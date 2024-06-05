use super::{ast, span::Spanned};

pub fn indent(indent: usize) -> String {
    String::from("    ").repeat(indent)
}

pub fn pretty_prog(prog: &ast::Program, ind: usize) -> String {
    format!(
        "{}(program \n{})",
        indent(ind),
        pretty_decs(&prog.0, ind + 1)
    )
}

pub fn pretty_repl_stmt(stmt: &ast::ReplStatement) -> String {
    match stmt {
        ast::ReplStatement::Stmt(stmt) => pretty_stmt(&stmt.val(), 0),
        ast::ReplStatement::Expr(expr) => pretty_expr(&expr.val(), 0),
    }
}

pub fn pretty_stmt(stmt: &ast::Statement, ind: usize) -> String {
    let str = match stmt {
        ast::Statement::Expr(e) => pretty_expr(&e.val(), ind),
        ast::Statement::Decl(d) => pretty_decl(&d.val(), ind),
    };

    format!("{}(stmt {})", indent(ind), str)
}

pub fn pretty_decs(decs: &Vec<Spanned<ast::Declaration>>, ind: usize) -> String {
    let str: String = decs
        .iter()
        .map(|Spanned(dec, _)| format!("{}{}", indent(ind), pretty_decl(dec, ind + 1)))
        .collect::<Vec<String>>()
        .join("\n");
    format!("[\n{}\n]", str)
}

pub fn pretty_decl(decl: &ast::Declaration, ind: usize) -> String {
    match decl {
        ast::Declaration::Value(Spanned(arg, _), Spanned(val, _)) => {
            format!(
                "(value {}\n{})",
                pretty_arg(arg, 0),
                pretty_expr(val, ind + 1)
            )
        }
        ast::Declaration::Function(fun_arg, args, expr) => {
            let id_str = pretty_arg(fun_arg, 0);
            let args_str = args
                .iter()
                .map(|Spanned(a, _)| pretty_arg(a, 0))
                .collect::<Vec<String>>()
                .join(", ");
            let args_str = format!("{}[{}]", indent(ind + 1), args_str);
            let expr_str = pretty_expr(&expr.val(), ind + 1);
            format!("(function {}\n{}\n{})", id_str, args_str, expr_str)
        }
    }
}

pub fn pretty_expr(expr: &ast::Expression, ind: usize) -> String {
    let str = match expr {
        ast::Expression::Unit => String::from("()"),
        ast::Expression::Int(i) => format!("{}", i.to_string()),
        ast::Expression::Bool(b) => format!("{}", b.to_string()),
        ast::Expression::Var(id) => id.to_string(),
        ast::Expression::BinaryOp(op, lhs, rhs) => format!(
            "({} {} {})",
            op.to_string(),
            pretty_expr(&lhs.val(), 0),
            pretty_expr(&rhs.val(), 0)
        ),
        // ast::Expression::UnaryOp(_, _) => unimplemented!(),
        ast::Expression::Apply(func, arg) => format!(
            "(call {} {})",
            pretty_expr(&func.val(), 0),
            pretty_expr(&arg.val(), 0)
        ),
        ast::Expression::Block(stmts, expr) => format!(
            "(block {}{})",
            if stmts.is_empty() {
                String::new()
            } else {
                "\n".to_owned()
                    + &stmts
                        .iter()
                        .map(|stmt| pretty_stmt(stmt, ind + 1))
                        .collect::<Vec<String>>()
                        .join("\n")
            },
            match expr {
                Some(e) => format!(
                    "\n{}(returns {})",
                    indent(ind + 1),
                    pretty_expr(&e.val(), 0)
                ),
                None => String::new(),
            }
        ),
    };

    format!("{}{}", indent(ind), str)
}

pub fn pretty_arg(arg: &ast::Arg, ind: usize) -> String {
    format!(
        "{}{}{}",
        indent(ind),
        arg.0.val(),
        match &arg.1 {
            Some(ty) => format!(": {}", pretty_type(&ty.val(), 0)),
            None => String::from(""),
        }
    )
}

pub fn pretty_type(ty: &ast::Type, ind: usize) -> String {
    let str = match ty {
        ast::Type::Unit => String::from("unit"),
        ast::Type::Int => String::from("int"),
        ast::Type::Bool => String::from("bool"),
        ast::Type::Var(id) => id.to_string(),
        ast::Type::Arrow(lhs, rhs) => format!(
            "{} -> {}",
            pretty_type(&lhs.val(), 0),
            pretty_type(&rhs.val(), 0)
        ),
    };

    format!("{}(type {})", indent(ind), str)
}
