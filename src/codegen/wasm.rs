use std::fs::File;

use crate::syntax::ast;

use super::OwlBackend;

#[derive(Debug)]
struct WasmBackend {
    ast: ast::Program,
}

type Bytes = Vec<u8>;

// TODO: Magic Header

impl WasmBackend {
    pub fn new(ast: ast::Program) -> Self {
        Self { ast }
    }

    pub fn emit_declaration(decl: ast::Declaration) -> Bytes {
        todo!()
    }
}

impl OwlBackend for WasmBackend {
    fn codegen(program: ast::Program) -> anyhow::Result<Vec<u8>> {
        // TODO:: compile the functions into a module

        let function_bytes = program.0.iter().map(Self::emit_declaration(decl));

        todo!()
    }
}
