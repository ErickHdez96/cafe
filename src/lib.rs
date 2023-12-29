pub mod atom;
pub mod atty;
pub mod config;
pub mod diagnostics;
pub mod env;
pub mod expander;
pub mod file;
mod interner;
pub mod query;
pub mod span;
pub mod syntax;
pub mod ty;
pub mod tyc;
pub mod utils;

pub use query::BuildSystem;
