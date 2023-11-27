use core::fmt;

use crate::file::FileId;

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub file_id: FileId,
    pub offset: u32,
    pub len: u16,
}

impl Span {
    pub const fn new(file_id: FileId, offset: u32, len: u16) -> Self {
        Self {
            file_id,
            offset,
            len,
        }
    }

    pub const fn file_id(self) -> FileId {
        self.file_id
    }

    pub fn dummy() -> Self {
        Self {
            file_id: FileId::default(),
            offset: 0,
            len: 0,
        }
    }

    pub const fn is_dummy(self) -> bool {
        self.file_id.is_dummy() && self.offset == 0 && self.len == 0
    }

    pub fn extend(self, other: Self) -> Self {
        debug_assert!(self.file_id == other.file_id);
        Self {
            file_id: self.file_id,
            offset: self.offset,
            len: ((other.offset as usize + other.len as usize) - (self.offset as usize))
                .try_into()
                .unwrap(),
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}..{}", self.file_id.value(), self.offset, self.len)
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct FilePosition {
    pub start_col: usize,
    pub start_row: usize,
    pub end_col: usize,
    pub end_row: usize,
}
