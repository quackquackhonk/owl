use std::fs::File;

use crate::syntax::ast;

use super::OwlBackend;

#[derive(Debug)]
struct WasmBackend {
    ast: ast::Program,
}

// TODO: Magic Header

impl WasmBackend {
    pub fn new(ast: ast::Program) -> Self {
        Self { ast }
    }
}
