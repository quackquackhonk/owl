pub mod wasm;

pub trait Emmiter {
    fn codegen(path: &str) -> anyhow::Result<()>;
}
