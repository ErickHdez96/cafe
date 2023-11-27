use std::{cell::RefCell, collections::HashMap, path::PathBuf, rc::Rc};

use crate::{
    config::CompilerConfig,
    diagnostics::Diagnostic,
    file::{FileId, SourceFile},
    syntax::parser::{parse_source_file, ParseResult},
};

use super::{QCtx, Res};

pub fn set_compiler_config(config: CompilerConfig) {
    COMPILER_CONFIG.with(|c| *c.borrow_mut() = Rc::new(config));
}

pub fn compiler_config_provider(_: &QCtx, _: ()) -> Rc<CompilerConfig> {
    COMPILER_CONFIG.with(|c| Rc::clone(&c.borrow()))
}

pub fn read_file_provider(_: &QCtx, path: PathBuf) -> Res<Rc<SourceFile>> {
    match std::fs::read_to_string(&path) {
        Ok(contents) => {
            let file = Rc::new(SourceFile::new(path, contents));
            FILE_MAPPING.with(|map| map.borrow_mut().insert(file.id, Rc::clone(&file)));
            Ok(file)
        }
        Err(e) => Err(Diagnostic::builder()
            .msg(format!("{}: {e}", path.to_string_lossy()))
            .finish()),
    }
}

pub fn file_provider(_: &QCtx, id: FileId) -> Option<Rc<SourceFile>> {
    FILE_MAPPING.with(|map| map.borrow_mut().get(&id).cloned())
}

pub fn parse_provider(qctx: &QCtx, path: PathBuf) -> Res<ParseResult> {
    let config = qctx.compiler_config(());
    let source_file = qctx.read_file(path)?;
    Ok(parse_source_file(&source_file, config.parser))
}

thread_local! {
    static FILE_MAPPING: RefCell<HashMap<FileId, Rc<SourceFile>>> = RefCell::default();
    static COMPILER_CONFIG: RefCell<Rc<CompilerConfig>> = RefCell::default();
}
