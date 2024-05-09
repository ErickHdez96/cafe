use std::fmt;

use crate::{interner::Interner, span::Span, syntax::ast::Path, ty::Ty};

const INDENTAION_WIDTH: usize = 2;

#[derive(Clone, PartialEq, Eq)]
pub struct Package {
    pub bodies: Vec<Body>,
}

impl Package {
    pub fn display<'a>(&'a self, interner: &'a Interner) -> PackageDisplay<'a> {
        PackageDisplay {
            package: self,
            interner,
        }
    }
}

pub struct PackageDisplay<'a> {
    pub package: &'a Package,
    pub interner: &'a Interner,
}

impl fmt::Debug for PackageDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let padding = " ".repeat(width);
            writeln!(f, "{}(pkg", padding)?;
            for (i, body) in self.package.bodies.iter().enumerate() {
                write!(
                    f,
                    "{:#width$?}{}",
                    body.display(self.interner),
                    if i < self.package.bodies.len() - 1 {
                        "\n"
                    } else {
                        ""
                    },
                    width = width + INDENTAION_WIDTH
                )?;
            }
            write!(f, ")")
        } else {
            f.debug_struct("Package")
                .field(
                    "bodies",
                    &self
                        .package
                        .bodies
                        .iter()
                        .map(|b| b.display(self.interner))
                        .collect::<Vec<_>>(),
                )
                .finish()
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Body {
    pub span: Span,
    pub name: Path,
    pub ty: Ty,
    pub instantiations: Vec<Vec<Ty>>,
    pub param_count: usize,
    pub variable_count: usize,
    pub locals: Vec<LocalDecl>,
    pub stack_size: u32,
    pub basic_blocks: Vec<BasicBlockData>,
}

impl Body {
    pub fn display<'a>(&'a self, interner: &'a Interner) -> BodyDisplay<'a> {
        BodyDisplay {
            body: self,
            interner,
        }
    }
}

pub struct BodyDisplay<'a> {
    pub body: &'a Body,
    pub interner: &'a Interner,
}

impl fmt::Debug for BodyDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let body = self.body;
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let padding = " ".repeat(width);
            writeln!(
                f,
                "{padding}(fn (|{:#?}|{}) {:#?} ({})",
                body.name,
                if body.param_count == 0 {
                    String::new()
                } else {
                    format!(
                        " {}",
                        body.locals
                            .iter()
                            .enumerate()
                            .skip(1)
                            .take(body.param_count)
                            .map(|(i, l)| format!(
                                "[_{i}: {:#?}]",
                                l.ty.with_arena(&self.interner.types)
                            ))
                            .collect::<Vec<_>>()
                            .join(" ")
                    )
                },
                body.locals[0].ty.with_arena(&self.interner.types),
                body.span,
            )?;
            write!(f, "{}(let (", " ".repeat(width + INDENTAION_WIDTH))?;
            for (i, ld) in body.locals.iter().enumerate() {
                write!(
                    f,
                    "{}[{} {:#?} ({})]{}",
                    if i > 0 {
                        format!(
                            "\n{}",
                            " ".repeat(width + INDENTAION_WIDTH + "(let (".len())
                        )
                    } else {
                        String::new()
                    },
                    Local::new(i as u32),
                    ld.ty.with_arena(&self.interner.types),
                    ld.span,
                    if i == body.locals.len() - 1 { ")" } else { "" },
                )?;
            }
            writeln!(f)?;
            for (i, bb) in body.basic_blocks.iter().enumerate() {
                write!(
                    f,
                    "{:#width$?}{}",
                    bb,
                    if i < body.basic_blocks.len() - 1 {
                        "\n"
                    } else {
                        ""
                    },
                    width = width + INDENTAION_WIDTH + INDENTAION_WIDTH
                )?;
            }
            write!(f, "))")
        } else {
            f.debug_struct("Body")
                .field("name", &body.name)
                .field("span", &body.span)
                .field("stack_size", &body.stack_size)
                .field("locals", &body.locals)
                .field("basic_blocks", &body.basic_blocks)
                .field("param_count", &body.param_count)
                .finish()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalDecl {
    pub span: Span,
    pub ty: Ty,
    pub stack_offset: u32,
    pub size: u32,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Local(u32);

impl Local {
    pub fn new(idx: u32) -> Self {
        Self(idx)
    }

    pub fn value(self) -> u32 {
        self.0
    }

    pub fn ret() -> Self {
        Self::new(0)
    }

    /// Tests whether it is the return local.
    pub fn is_return(self) -> bool {
        self.0 == 0
    }
}

impl fmt::Display for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_{}", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Constant {
    pub span: Span,
    pub value: u64,
}

impl Constant {
    pub fn new(value: u64, span: Span) -> Self {
        Self { span, value }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    LShift,
    RShift,

    And,
    Or,
    Xor,

    Lt,
    Lte,
    Gt,
    Gte,
    EqEq,
    NEq,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BinaryOp::*;
        match self {
            Add => "add".fmt(f),
            Sub => "sub".fmt(f),
            Mul => "mul".fmt(f),
            Div => "div".fmt(f),
            LShift => todo!(),
            RShift => todo!(),
            And => todo!(),
            Or => todo!(),
            Xor => todo!(),
            Lt => todo!(),
            Lte => todo!(),
            Gt => todo!(),
            Gte => todo!(),
            EqEq => todo!(),
            NEq => todo!(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct BasicBlock(usize);

impl BasicBlock {
    pub fn new(idx: usize) -> Self {
        Self(idx)
    }

    pub fn value(self) -> usize {
        self.0
    }
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct BasicBlockData {
    pub idx: usize,
    pub statements: Vec<Statement>,
    pub terminator: Terminator,
}

impl fmt::Debug for BasicBlockData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let padding = " ".repeat(width);
            writeln!(f, "{}(bb {}", padding, self.idx)?;
            for stmt in &self.statements {
                writeln!(f, "{:#width$?}", stmt, width = width + INDENTAION_WIDTH)?;
            }
            write!(
                f,
                "{:#width$?})",
                self.terminator,
                width = width + INDENTAION_WIDTH
            )
        } else {
            f.debug_struct("BasicBlockData")
                .field("idx", &self.idx)
                .field("statements", &self.statements)
                .field("terminator", &self.terminator)
                .finish()
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

impl Statement {
    pub fn is_nop(&self) -> bool {
        matches!(self.kind, StatementKind::Nop)
    }
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let padding = " ".repeat(width);
            match &self.kind {
                StatementKind::Nop => {
                    write!(f, "{padding}(nop ({}))", self.span)
                }
                StatementKind::LoadI(dst, src) => {
                    write!(f, "{padding}(loadI {dst} {src} ({}))", self.span,)
                }
                StatementKind::Copy(dst, src) => {
                    write!(f, "{padding}(copy {dst} {src} ({}))", self.span,)
                }
                StatementKind::BinaryOp(op, dst, src1, src2) => {
                    write!(f, "{padding}({op} {dst} {src1} {src2} ({}))", self.span,)
                }
                StatementKind::BinaryOpI(op, dst, src1, src2) => {
                    write!(f, "{padding}({op}I {dst} {src1} {src2} ({}))", self.span,)
                }
                StatementKind::Arg(local) => write!(f, "{padding}(arg {local} ({}))", self.span,),
                StatementKind::Call(dst, fnl) => {
                    write!(f, "{padding}(call {dst} {fnl} ({}))", self.span,)
                }
                StatementKind::CallLabel(dst, fnlbl) => write!(
                    f,
                    "{padding}(call-label {dst} |{fnlbl:#?}| ({}))",
                    self.span,
                ),
                StatementKind::LoadLabel(dst, label) => {
                    write!(f, "{padding}(addressof {dst} |{label:#?}| ({}))", self.span,)
                }
                StatementKind::Load(_, _) => todo!(),
                StatementKind::LoadAI(_, _, _) => todo!(),
                StatementKind::LoadAO(_, _, _) => todo!(),
                StatementKind::Store(_, _) => todo!(),
                StatementKind::StoreAI(_, _, _) => todo!(),
                StatementKind::StoreAO(_, _, _) => todo!(),
                StatementKind::StoreLabel(label, src) => {
                    write!(
                        f,
                        "{padding}(store-label |{label:#?}| {src} ({}))",
                        self.span,
                    )
                }
            }
        } else {
            f.debug_struct("Statement")
                .field("span", &self.span)
                .field("kind", &self.kind)
                .finish()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementKind {
    // nop
    Nop,
    /// _0 = copy _1
    Copy(Local, Local),
    /// _0 = loadI 4
    LoadI(Local, Constant),

    /// _0 = _1 + _2
    BinaryOp(BinaryOp, Local, Local, Local),
    /// _0 = _1 + 4
    BinaryOpI(BinaryOp, Local, Local, Constant),

    /// _0 = load _1 ; memory[_1]
    Load(Local, Local),
    /// _0 = loadAI _1, 4 ; memory[_1 + 4]
    LoadAI(Local, Local, Constant),
    /// _0 = loadAI _1, _2 ; memory[_1 + _2]
    LoadAO(Local, Local, Local),

    /// store _0 = _1 ; memory[_0] = _1
    Store(Local, Local),
    /// storeAI _0, 4 = _1 ; memory[_0 + 4] = _1
    StoreAI(Local, Constant, Local),
    /// storeAO _0, _1 = _2 ; memory[_0 + _1] = _2
    StoreAO(Local, Local, Local),
    /// store @_0, _1 ; memory[@_0] = _1
    StoreLabel(Path, Local),

    /// loadLabel _0 = address of label
    LoadLabel(Local, Path),

    /// arg _0
    /// prepares the arguments to be used in-order in the next call instruction
    Arg(Local),

    /// _0 = call _1
    Call(Local, Local),

    /// _0 = call-label _1
    CallLabel(Local, Path),
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Terminator {
    pub span: Span,
    pub kind: TerminatorKind,
}

impl fmt::Debug for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let padding = " ".repeat(width);
            match &self.kind {
                TerminatorKind::Return => {
                    write!(f, "{padding}(return ({}))", self.span,)
                }
                TerminatorKind::Goto(bb) => {
                    write!(f, "{padding}(goto bb{} ({}))", bb.value(), self.span,)
                }
                TerminatorKind::Cond {
                    cond,
                    true_label,
                    false_label,
                } => write!(
                    f,
                    "{padding}(cond {} bb{} bb{} ({}))",
                    cond,
                    true_label.value(),
                    false_label.value(),
                    self.span,
                ),
            }
        } else {
            f.debug_struct("Terminator")
                .field("span", &self.span)
                .field("kind", &self.kind)
                .finish()
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub enum TerminatorKind {
    /// Unconditional jump to the given basic block.
    Goto(BasicBlock),

    /// Returns from the current body.
    ///
    /// If the function returns a value, the local variable _0 should be assigned the returned
    /// value before reaching the end of the function.
    #[default]
    Return,

    /// If the local is true, jump to trueL, otherwise jump to falseL.
    Cond {
        cond: Local,
        true_label: BasicBlock,
        false_label: BasicBlock,
    },
}
