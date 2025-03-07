use core::fmt;
use std::fmt::Display;

use crate::{
    align,
    arena::Arena,
    ir,
    span::Span,
    symbol::Symbol,
    ty::{self, TyK},
    utils::mangle_symbol,
};

use super::{Inst, Insts, Register, ISA};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Direction {
    Forwards,
    Backwards,
}

impl fmt::Display for Direction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Direction::Forwards => "f",
                Direction::Backwards => "b",
            }
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Condition {
    Eq,
    Ne,
    Sge,
    Sgt,
    Sle,
    Slt,
    Uge,
    Ugt,
    Ule,
    Ult,
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Condition::Eq => "EQ",
                Condition::Ne => "NE",
                Condition::Sge => "GE",
                Condition::Sgt => "GT",
                Condition::Sle => "LE",
                Condition::Slt => "LT",
                Condition::Uge => "HS",
                Condition::Ugt => "HI",
                Condition::Ule => "LS",
                Condition::Ult => "LO",
            }
        )
    }
}

impl Inst {
    /// ```text
    /// .align {integer}
    /// ```
    pub fn pseudo_align(boundary: u8) -> Self {
        Self {
            span: Span::dummy(),
            value: format!(".align {}", boundary),
        }
    }

    /// ```text
    /// .global {label}
    /// ```
    pub fn pseudo_global(label: Symbol) -> Self {
        Inst::new(Span::dummy(), format!(".global {}", mangle_symbol(label)))
    }

    /// ```text
    /// {label}:
    /// ```
    pub fn pseudo_label(label: Symbol) -> Self {
        Inst::new(Span::dummy(), format!("{}:", mangle_symbol(label)))
    }

    /// ```text
    /// mov {dreg}, {sreg}
    /// ```
    pub fn copy(dst: Register, src: Register, span: Span) -> Self {
        Self {
            span,
            value: format!("\tmov {}, {}", dst, src),
        }
    }

    /// ```text
    /// mov {dreg}, xzr
    /// mov {dreg}, #imm
    /// ```
    pub fn load_immediate(dst: Register, constant: u64, span: Span) -> Vec<Self> {
        let mut out = Vec::with_capacity(4);
        if constant == 0 {
            out.push(Inst::new(
                span,
                format!(
                    "\tmov {}, {}",
                    dst,
                    if dst.is_64bit() { "xzr" } else { "wzr" }
                ),
            ));
        } else {
            out.push(Inst::new(
                span,
                format!("\tmov {}, #{}", dst, constant & 0xFFFF),
            ));
        }

        if constant > 0xFFFF {
            out.push(Inst::new(
                span,
                format!("\tmovk {}, #{}, lsl #16", dst, (constant >> 16) & 0xFFFF),
            ));
        }
        if constant > 0xFFFF_FFFF {
            out.push(Inst::new(
                span,
                format!("\tmovk {}, #{}, lsl #32", dst, (constant >> 32) & 0xFFFF),
            ));
        }
        if constant > 0xFFFF_FFFF_FFFF {
            out.push(Inst::new(
                span,
                format!("\tmovk {}, #{}, lsl #48", dst, (constant >> 48) & 0xFFFF),
            ));
        }

        out
    }

    /// ```text
    /// ret lr
    /// ```
    pub fn ret(span: Span) -> Self {
        Self {
            span,
            value: format!("\tret {}", Register::LR),
        }
    }

    /// ```text
    /// blr {reg}
    /// ```
    pub fn call_register(reg: Register, span: Span) -> Self {
        Self {
            span,
            value: format!("\tblr {}", reg),
        }
    }

    /// ```text
    /// bl {label}
    /// ```
    pub fn call_label(label: Symbol, span: Span) -> Self {
        Self {
            span,
            value: format!("\tbl {}", mangle_symbol(label)),
        }
    }

    /// ```text
    /// b {label}{direction}
    /// ```
    pub fn jump(label: Symbol, direction: Direction, span: Span) -> Self {
        Self {
            span,
            value: format!("\tb {}{}", label, direction),
        }
    }

    /// ```text
    /// b.{cond} {label}{direction}
    /// ```
    pub fn jump_cond(label: Symbol, direction: Direction, cond: Condition, span: Span) -> Self {
        Self {
            span,
            value: format!("\tb.{} {}{}", cond, label, direction),
        }
    }

    /// ```text
    /// cmp {reg}, #imm
    /// ```
    pub fn cmp_immediate(reg: Register, imm: i16, span: Span) -> Self {
        Self {
            span,
            value: format!("\tcmp {}, #{}", reg, imm),
        }
    }
}

impl Register {
    pub const R0: Register = Register::new(0);
    pub const R1: Register = Register::new(1);
    pub const R2: Register = Register::new(2);
    pub const R3: Register = Register::new(3);
    pub const R4: Register = Register::new(4);
    pub const R5: Register = Register::new(5);
    pub const R6: Register = Register::new(6);
    pub const R7: Register = Register::new(7);
    pub const R8: Register = Register::new(8);
    pub const R9: Register = Register::new(9);
    pub const R10: Register = Register::new(10);
    pub const R11: Register = Register::new(11);
    pub const R12: Register = Register::new(12);
    pub const R13: Register = Register::new(13);
    pub const R14: Register = Register::new(14);
    pub const R15: Register = Register::new(15);
    pub const R16: Register = Register::new(16);
    pub const R17: Register = Register::new(17);
    /// Frame pointer.
    /// Pointer to the current's frame base address.
    pub const R29: Register = Register::new(29);
    /// Link register.
    /// Holds the return address of the parent function.
    pub const R30: Register = Register::new(30);
    /// Stack pointer.
    pub const R31: Register = Register::new(31);

    /// Return value register.
    pub const RV: Register = Self::R0;
    /// Frame pointer.
    pub const FP: Register = Self::R29;
    /// Link register.
    pub const LR: Register = Self::R30;
    /// Stack pointer.
    pub const SP: Register = Self::R31;

    /// x0-x7 can be used as parameter registers.
    pub const PARAM_REGISTERS: [Register; 8] = [
        Self::R0,
        Self::R1,
        Self::R2,
        Self::R3,
        Self::R4,
        Self::R5,
        Self::R6,
        Self::R7,
    ];

    /// x8-x17 can be used as temporary registers (caller saved, but not used as parameters).
    pub const TEMPORARY_REGISTERS: [Register; 10] = [
        Self::R8,
        Self::R9,
        Self::R10,
        Self::R11,
        Self::R12,
        Self::R13,
        Self::R14,
        Self::R15,
        Self::R16,
        Self::R17,
    ];

    pub const fn return_value() -> Self {
        Self::RV
    }

    pub const fn stack_pointer() -> Self {
        Self::SP
    }

    pub const fn link_register() -> Self {
        Self::R30
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if *self == Register::SP {
            "sp".fmt(f)
        } else {
            write!(
                f,
                "{}{}",
                if self.is_64bit() { "x" } else { "w" },
                self.value()
            )
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Aarch64;

impl Aarch64 {}

impl ISA for Aarch64 {
    // Size in bytes of a pointer.
    const POINTER_SIZE: usize = 8;

    /// Calculates the body's stack size and the local's stack offsets and sizes.
    fn process_body(&self, body: &mut ir::Body, arena: &Arena<TyK>) {
        let mut stack_size = 0u32;

        body.locals[0].size = self.ty_size(arena.get(body.locals[0].ty.value())) as u32;
        for loc in body.locals.iter_mut().skip(1) {
            let ty = arena.get(loc.ty.value());
            loc.stack_offset = align(stack_size as usize, self.ty_alignment(ty))
                .try_into()
                .unwrap();
            loc.size = self.ty_size(ty).try_into().unwrap();
            stack_size = loc.stack_offset + loc.size;
        }
        // The stack must be aligned to 16 bytes.
        stack_size = align(stack_size as usize, 16) as u32;

        // Store parameters and variables in ascending order, from the bottom of the call frame,
        // towards the stack pointer (i.e. the first parameter is the farthest away from the stack
        // pointer).
        for loc in body.locals.iter_mut().skip(1) {
            loc.stack_offset = stack_size - (loc.stack_offset + loc.size);
        }

        // Stack space to store FP and LR
        let fp_and_lr_size = 16;
        // The stack must be aligned to 16 bytes. It's unnecessary since it was already 16-byte
        // aligned and we added 16 bytes, but... better safe than sorry.
        stack_size = align(stack_size as usize + fp_and_lr_size, 16) as u32;
        assert!(
            stack_size <= 0b1111_1111_1111,
            "Stack size must fit in 12-bits"
        );
        body.stack_size = stack_size;
    }

    /// Generate the prologue to the function.
    fn proc_begin(&self, body: &ir::Body) -> Vec<Inst> {
        let fp_stack_offset = TryInto::<i16>::try_into(body.stack_size - 16).unwrap();
        assert!(
            (-512..=504).contains(&fp_stack_offset),
            "fp out of range for simple stp/ldp"
        );

        let mut prologue = vec![
            Inst::new(
                body.span,
                format!(
                    "\tsub {}, {}, #{}",
                    Register::SP,
                    Register::SP,
                    body.stack_size
                ),
            ),
            Inst::new(
                body.span,
                format!(
                    "\tstp {}, {}, [{}, #{}]",
                    Register::FP,
                    Register::LR,
                    Register::SP,
                    fp_stack_offset
                ),
            ),
            Inst::new(
                body.span,
                format!(
                    "\tadd {}, {}, #{}",
                    Register::FP,
                    Register::SP,
                    fp_stack_offset
                ),
            ),
        ];
        for i in 0..body.param_count {
            let ir::LocalDecl {
                stack_offset, size, ..
            } = body.locals[i + 1];
            let param_reg = Register::PARAM_REGISTERS
                .get(i)
                .expect("too many parameters")
                .with_size(size);
            // body.locals[1..body.param_count+1] contains the param locals
            prologue.push(Inst::new(
                body.span,
                format!(
                    "\tstr{} {}, [{}, #{}]",
                    match param_reg.size() {
                        1 => "b",
                        2 => "h",
                        _ => "",
                    },
                    param_reg,
                    Register::SP,
                    stack_offset
                ),
            ));
        }

        prologue
    }

    fn proc_end(&self, body: &ir::Body) -> Vec<Inst> {
        let fp_stack_offset = TryInto::<i16>::try_into(body.stack_size - 16).unwrap();

        vec![
            Inst::new(
                body.span,
                format!(
                    "\tldp {}, {}, [{}, #{}]",
                    Register::FP,
                    Register::LR,
                    Register::SP,
                    fp_stack_offset
                ),
            ),
            Inst::new(
                body.span,
                format!(
                    "\tadd {}, {}, #{}",
                    Register::SP,
                    Register::SP,
                    body.stack_size
                ),
            ),
        ]
    }

    fn ty_size(&self, ty: &TyK) -> usize {
        match ty {
            TyK::Boolean => 1,
            TyK::Char => 4,
            TyK::String => todo!(),
            TyK::Number(n) => match n {
                ty::NumberTy::I64 => Self::POINTER_SIZE,
            },
            TyK::Lambda { .. } => Self::POINTER_SIZE,
            TyK::None => todo!(),
            TyK::Null => Self::POINTER_SIZE,
            TyK::Array(_) => Self::POINTER_SIZE,
            TyK::Void => 0,
            TyK::Symbol => todo!(),
            TyK::Var(_) => todo!(),
            TyK::Error => todo!(),
        }
    }

    fn ty_alignment(&self, ty: &TyK) -> usize {
        match ty {
            TyK::Boolean => 1,
            TyK::Char => 4,
            TyK::Number(n) => match n {
                ty::NumberTy::I64 => Self::POINTER_SIZE,
            },
            TyK::Lambda { .. } => Self::POINTER_SIZE,
            TyK::Array(_) => Self::POINTER_SIZE,
            TyK::String => todo!(),
            TyK::None => todo!(),
            TyK::Null => todo!(),
            TyK::Void => 1,
            TyK::Symbol => todo!(),
            TyK::Var(_) => todo!(),
            TyK::Error => todo!(),
        }
    }

    /// Required runtime to run a program.
    ///
    /// ### Includes
    ///
    /// * `_start` entry point to set up stack and terminate the program correctly.
    /// * primitive functions
    fn runtime(&self) -> Insts {
        let mut runtime = vec![];
        runtime.append(&mut Self::entry_point());
        Insts(runtime)
    }
}

#[cfg(target_os = "macos")]
impl Aarch64 {
    fn entry_point() -> Vec<Inst> {
        // .align 4
        // .global _main
        // _main:
        //      sub sp, sp, #16
        //      stp x29, x30, [sp, #0]
        //      bl main
        //      bl _exit

        vec![
            Inst::pseudo_align(4),
            Inst::pseudo_global("_main".into()),
            Inst::pseudo_label("_main".into()),
            // Allocate space for the frame pointer.
            Inst::new(Span::dummy(), String::from("\tsub sp, sp, #16")),
            // Store fp and lr, as the stack needs to be 16-byte aligned.
            Inst::new(Span::dummy(), String::from("\tstp x29, x30, [sp, #0]")),
            Inst::new(Span::dummy(), String::from("\tmov x29, sp")),
            Inst::new(Span::dummy(), String::from("\tbl main")),
            // Terminate the program
            // x0 (the exit code) ist set by main.
            // Call exit() from libc
            Inst::new(Span::dummy(), String::from("\tbl _exit")),
        ]
    }
}
