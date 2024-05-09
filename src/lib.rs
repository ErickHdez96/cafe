mod arena;
pub mod asm;
pub mod atty;
pub mod codegen;
pub mod compiler;
pub mod config;
pub mod diagnostics;
pub mod driver;
pub mod env;
pub mod expander;
pub mod file;
mod interner;
pub mod ir;
pub mod lower_ast;
pub mod rnrs;
pub mod span;
mod symbol;
pub mod syntax;
#[cfg(test)]
mod test;
pub mod ty;
pub mod tyc;
mod utils;

pub const fn align(n: usize, a: usize) -> usize {
    (n + (a - 1)) & !(a - 1)
}
