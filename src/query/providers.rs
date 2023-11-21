use std::{cell::RefCell, collections::HashMap, path::PathBuf, rc::Rc};

use crate::{
    diagnostics::Diagnostic,
    file::{FileId, SourceFile},
    syntax::parser::{parse_source_file, ParseResult, ParserConfig},
};

use super::{Query, Res};

pub fn read_file_provider(_: &Query, path: PathBuf) -> Res<Rc<SourceFile>> {
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

pub fn file_provider(_: &Query, id: FileId) -> Option<Rc<SourceFile>> {
    FILE_MAPPING.with(|map| map.borrow_mut().get(&id).cloned())
}

pub fn parse_provider(qctx: &Query, path: PathBuf) -> Res<ParseResult> {
    let source_file = qctx.read_file(path)?;
    Ok(parse_source_file(&source_file, ParserConfig::default()))
}

thread_local! {
    static FILE_MAPPING: RefCell<HashMap<FileId, Rc<SourceFile>>> = RefCell::default();
}
