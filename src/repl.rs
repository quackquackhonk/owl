use chumsky::{input::Input, Parser};
use rustyline::{error::ReadlineError, DefaultEditor};

use crate::syntax::{lexer, parser::parse_stmt};

pub fn owl_repl() -> anyhow::Result<()> {
    let mut rl = DefaultEditor::new()?;

    // attempt to load history
    // TODO: this should not be pellets-history
    if rl.load_history("pellets-history.txt").is_err() {
        println!("No previous history.");
    }

    loop {
        // TODO: We should parse accept input until a complete statement
        let readline = rl.readline(">>> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                process_line(line)?
            }
            Err(ReadlineError::Interrupted) => {
                println!("Interrupted!");
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("Got EOF, exiting...");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err)
            }
        };
    }

    rl.save_history("pellets-history.txt")?;
    Ok(())
}

fn process_line(line: String) -> anyhow::Result<()> {
    let len = line.len();

    let toks = lexer(&line);

    let stmt = parse_stmt().parse(toks.spanned(len..len));

    println!("{:?}", stmt);

    if stmt.has_errors() {
        println!(
            "Errors when parsing statement: {:?}",
            stmt.errors().collect::<Vec<_>>()
        );
    } else {
        println!("\t{:?}", stmt.output());
    }

    Ok(())
}
