use rustyline::{error::ReadlineError, DefaultEditor};

use crate::syntax::{lexer::lexer, parser::owl_repl_parser, pretty::pretty_repl_stmt};

pub fn owl_repl() -> anyhow::Result<()> {
    let mut rl = DefaultEditor::new()?;

    // attempt to load history
    // TODO: this should not be pellets-history
    if rl.load_history("pellets-history.txt").is_err() {
        println!("No previous history.");
    }

    loop {
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
    let mut lex = lexer(&line).into_iter().peekable();

    let stmt = owl_repl_parser(&mut lex);

    match stmt {
        Ok(stmt) => println!("{}", pretty_repl_stmt(&stmt)),
        Err(e) => println!("{}", e),
    };

    Ok(())
}
