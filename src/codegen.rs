use std::{
    fs::{self, File},
    io::Write,
    u8,
};

use crate::syntax::ast::Program;

pub mod wasm;

pub trait OwlBackend {
    // TODO: This might return something, I'm not sure
    fn compile(path: &str, program: Program) -> anyhow::Result<()> {
        let mut outfile = File::create(path)?;
        let contents = Self::codegen(program)?;
        outfile.write_all(&contents)?;
        Ok(())
    }

    fn codegen(program: Program) -> anyhow::Result<Vec<u8>>;
}
