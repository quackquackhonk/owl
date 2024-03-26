use crate::syntax::ast;

use super::Emmiter;

#[derive(Debug)]
struct WasmEmmiter {
    ast: ast::Program,
}

impl WasmEmmiter {
    pub fn new(ast: ast::Program) -> Self {
        Self { ast }
    }
}

impl Emmiter for WasmEmmiter {
    fn codegen(path: &str) -> anyhow::Result<()> {
        todo!()
    }
}
