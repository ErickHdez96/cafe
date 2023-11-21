use crate::file::FileId;

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub file_id: u16,
    pub offset: u32,
    pub len: u16,
}

impl Span {
    pub const fn new(file_id: u16, offset: u32, len: u16) -> Self {
        Self {
            file_id,
            offset,
            len,
        }
    }

    pub const fn file_id(self) -> FileId {
        FileId(self.file_id & !(i16::MIN as u16))
    }

    pub const fn dummy() -> Self {
        Self {
            file_id: 0,
            offset: 0,
            len: 0,
        }
    }

    pub const fn is_dummy(self) -> bool {
        self.file_id == 0 && self.offset == 0 && self.len == 0
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct FilePosition {
    pub start_col: usize,
    pub start_row: usize,
    pub end_col: usize,
    pub end_row: usize,
}
