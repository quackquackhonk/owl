use std::{fs::File, io::Read};

use clap::Parser;
use owl::{repl, syntax::parser::owl_parser};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    file: Option<String>,
}

/// Entry point for the pellets compiler
fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    if let Some(path) = args.file {
        compile_owl(path)
    } else {
        repl::owl_repl()
    }
}

fn compile_owl(path: String) -> anyhow::Result<()> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let prog = owl_parser(&contents)?;
    println!("{:?}", prog);

    Ok(())
}
