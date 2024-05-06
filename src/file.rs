use std::{cell::Cell, path::PathBuf};

use crate::span::{FilePosition, Span};

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(pub(crate) u16);

impl FileId {
    pub const fn value(self) -> u16 {
        self.0 & !(i16::MIN as u16)
    }

    pub const fn is_dummy(self) -> bool {
        self.value() == 0
    }

    #[must_use]
    pub const fn generated(self) -> Self {
        Self(self.0 | (i16::MIN as u16))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceFile {
    pub id: FileId,
    pub path: PathBuf,
    pub contents: String,
}

fn new_id() -> FileId {
    FILE_ID.with(|file_id| {
        let id = file_id.get();
        let new_id = id + 1;
        assert!(new_id < i16::MIN as u16, "too many files");
        file_id.set(new_id);
        FileId(id)
    })
}

impl SourceFile {
    pub fn new(path: impl Into<PathBuf>, contents: impl Into<String>) -> Self {
        Self {
            id: new_id(),
            path: path.into(),
            contents: contents.into(),
        }
    }

    pub fn position(&self, span: Span) -> FilePosition {
        let lo = span.offset as usize;
        let hi = span.offset as usize + span.len as usize;
        let start_row = self.contents[0..lo].bytes().filter(|&b| b == b'\n').count();
        let end_row = start_row
            + self.contents[lo..hi]
                .bytes()
                .filter(|&b| b == b'\n')
                .count();
        let start_col = lo
            - self.contents[0..lo]
                .rfind('\n')
                .map(|p| p + 1)
                .unwrap_or_default();
        let end_col = hi
            - self.contents[0..hi]
                .rfind('\n')
                .map(|p| p + 1)
                .unwrap_or_default();
        FilePosition {
            start_col,
            start_row,
            end_col,
            end_row,
        }
    }

    pub fn get_text(&self, span: Span) -> &str {
        let lo = span.offset as usize;
        let start = self.contents[0..lo]
            .rfind('\n')
            .map(|p| p + 1)
            .unwrap_or_default();
        let end = self.contents[start..]
            .find('\n')
            .map_or(self.contents.len(), |pos| start + pos);
        &self.contents[start..end]
    }
}

thread_local! {
    // FILE_ID 0 is invalid
    static FILE_ID: Cell<u16> = const { Cell::new(1) };
}
