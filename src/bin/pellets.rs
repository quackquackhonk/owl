use std::{fs::File, io::Read};

use clap::Parser;
use owl::syntax::parser::owl_program_parser;
use owl::repl;

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
    let prog = owl_program_parser(&path)?;

    println!("{:?}", prog);

    Ok(())
}
