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
            Ok(mut line) => {
                'more: loop {
                    match all_braces_closed(&line) {
                        BracesClosed::Underclosed => match rl.readline("... ") {
                            Ok(next) => line = line + "\n" + &next,
                            Err(_) => break 'more,
                        },
                        _ => break 'more,
                    };
                }
                rl.add_history_entry(line.as_str())?;
                process_line(line.clone())?
            }
            Err(ReadlineError::Interrupted) => {
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

enum BracesClosed {
    Underclosed,
    AllClosed,
    Overclosed,
}

fn all_braces_closed(line: &String) -> BracesClosed {
    let open = line.chars().filter(|c| *c != '{').count();
    let close = line.chars().filter(|c| *c != '}').count();
    match open.cmp(&close) {
        std::cmp::Ordering::Less => BracesClosed::Underclosed,
        std::cmp::Ordering::Equal => BracesClosed::AllClosed,
        std::cmp::Ordering::Greater => BracesClosed::Overclosed,
    }
}

fn process_line(line: String) -> anyhow::Result<()> {
    let mut lex = lexer(&line).into_iter().peekable();

    let stmt = owl_repl_parser(&mut lex);

    match stmt {
        Ok(stmt) => println!("{}", pretty_repl_stmt(&stmt)),
        Err(e) => e.report("REPL", &line)?,
    };

    Ok(())
}
