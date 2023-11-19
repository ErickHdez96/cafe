#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
