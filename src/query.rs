use std::{path::PathBuf, rc::Rc};

use crate::{
    diagnostics::Diagnostic,
    file::{FileId, SourceFile},
    syntax::parser::ParseResult,
};

mod providers;

pub type Res<T> = Result<T, Diagnostic>;

pub struct Query {
    read_file: fn(&Query, PathBuf) -> Res<Rc<SourceFile>>,
    file: fn(&Query, FileId) -> Option<Rc<SourceFile>>,
    parse: fn(&Query, PathBuf) -> Res<ParseResult>,
}

macro_rules! query {
    ($name:ident, $key:ty, $res:ty) => {
        pub fn $name(&self, key: $key) -> $res {
            (self.$name)(self, key)
        }
    };
}

impl Default for Query {
    fn default() -> Self {
        Self {
            read_file: providers::read_file_provider,
            file: providers::file_provider,
            parse: providers::parse_provider,
        }
    }
}

impl Query {
    pub fn new() -> Self {
        Self::default()
    }

    query!(read_file, PathBuf, Res<Rc<SourceFile>>);
    query!(file, FileId, Option<Rc<SourceFile>>);
    query!(parse, PathBuf, Res<ParseResult>);
}
