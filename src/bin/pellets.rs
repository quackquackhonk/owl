use std::{fs::File, io::Read};

use logos::Logos;
use owl::syntax::lexer::{self, Token};

fn main() -> anyhow::Result<()> {
    let mut file = File::open("examples/identifiers.owl")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    let token_iter = Token::lexer(&contents).collect::<Vec<Result<Token, ()>>>();
    println!("{:?}", token_iter);

    Ok(())
}
