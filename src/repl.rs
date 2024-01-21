use rustyline::{error::ReadlineError, DefaultEditor};

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
                println!("\t{}", line);
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
        }
    }

    rl.save_history("pellets-history.txt")?;
    Ok(())
}

fn process_line(line: String) -> anyhow::Result<()> {
    todo!()
}
