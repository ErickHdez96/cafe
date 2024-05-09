#[cfg(target_arch = "aarch64")]
mod aarch64;
#[cfg(target_arch = "aarch64")]
pub use aarch64::*;

use core::fmt;

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Insts(pub Vec<Inst>);

impl fmt::Display for Insts {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (idx, Inst { span, value }) in self.0.iter().enumerate() {
            write!(f, "{value}")?;
            if !span.is_dummy() {
                write!(f, "; ({span})")?;
            }
            if idx < self.0.len() - 1 {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Inst {
    pub span: Span,
    pub value: String,
}

impl Inst {
    pub fn new(span: Span, value: String) -> Self {
        Self { span, value }
    }
}

pub trait TyArch {
    /// Returns the size of the type in bytes.
    fn size(&self) -> usize;
    /// Returns the ABI-required minimum alignment of the type in bytes.
    fn alignment(&self) -> usize;
}

#[allow(clippy::upper_case_acronyms)]
pub struct ISA;

/// A Register in an ISA. The lower 6 bits represent the register. The upper 2 bits are used
/// to represent its width.
///
/// ```text
/// 00 - 64 bits
/// 01 - 32 bits
/// 10 - 16 bits
/// 11 -  8 bits
/// ```
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Register(u8);

impl fmt::Debug for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Register({})", self.0)
    }
}

impl Register {
    const ATTR_MASK: u8 = 0b1100_0000;
    const B8_VALUE: u8 = 0b1100_0000;
    const B16_VALUE: u8 = 0b1000_0000;
    const B32_VALUE: u8 = 0b0100_0000;
    const B64_VALUE: u8 = 0b0000_0000;
    const VALUE_MASK: u8 = 0b0011_1111;

    /// Creates a new 64-bit register.
    pub const fn new(reg: u8) -> Self {
        assert!(
            reg <= Self::VALUE_MASK,
            "There is only support for registers between 0-127"
        );
        Self(reg)
    }

    /// Returns the register's underlying value.
    pub const fn value(self) -> u8 {
        self.0 & Self::VALUE_MASK
    }

    pub const fn is_64bit(self) -> bool {
        (self.0 & Self::ATTR_MASK) == Self::B64_VALUE
    }

    #[must_use]
    pub const fn to_8bit(self) -> Self {
        Self(Self::B8_VALUE | (self.0 & Self::VALUE_MASK))
    }

    #[must_use]
    pub const fn to_16bit(self) -> Self {
        Self(Self::B16_VALUE | (self.0 & Self::VALUE_MASK))
    }

    #[must_use]
    pub const fn to_32bit(self) -> Self {
        Self(Self::B32_VALUE | (self.0 & Self::VALUE_MASK))
    }

    /// Sets the register's width according to the size in bytes of the value.
    pub const fn with_size(self, size: u32) -> Self {
        match size {
            1 => self.to_8bit(),
            2 => self.to_16bit(),
            4 => self.to_32bit(),
            _ => self,
        }
    }

    /// Returns the size of the register in bytes.
    pub const fn size(self) -> u32 {
        match self.0 & Self::ATTR_MASK {
            Self::B8_VALUE => 1,
            Self::B16_VALUE => 2,
            Self::B32_VALUE => 4,
            _ => 8,
        }
    }
}
