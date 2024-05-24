use std::{
    fs::{self, File},
    io::Write,
    u8,
};

use crate::syntax::ast::Program;

pub mod wasm;

pub trait OwlBackend {
    /// Compiles a given [`Program`], writing the program to the output file.
    ///
    /// * `path`: The of the output file
    /// * `program`: The [`Program`] to compile
    fn compile(path: &str, program: Program) -> anyhow::Result<()> {
        let mut outfile = File::create(path)?;
        let contents = Self::codegen(program)?;
        outfile.write_all(&contents)?;
        Ok(())
    }

    /// Compiles a program into a byte vector.
    ///
    /// * `program`: The [`Program`] to compile
    fn codegen(program: Program) -> anyhow::Result<Vec<u8>>;
}
