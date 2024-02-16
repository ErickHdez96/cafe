use std::{fmt, rc::Rc};

use crate::{env::Env, new_expander::Binding, span::Span, ty::Ty, utils::Resolve};

use super::{ast, cst::GreenTree, parser::parse_char, SyntaxKind};

const INDENTATION_WIDTH: usize = 2;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Item {
    List(List),
    Atom(Atom),
}

impl Item {
    pub fn list(&self) -> Option<&List> {
        match self {
            Item::List(l) => Some(l),
            _ => None,
        }
    }

    pub fn atom(&self) -> Option<&Atom> {
        match self {
            Item::Atom(a) => Some(a),
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum List {
    Mod(ModuleList),
    Define(DefineList),
}

impl List {
    pub fn children(&self) -> &[Item] {
        match self {
            List::Mod(m) => &m.children,
            List::Define(d) => &d.children,
        }
    }

    pub fn new_mod(span: Span, source: Rc<GreenTree>, id: ast::ModId, children: Vec<Item>) -> Self {
        Self::Mod(ModuleList {
            id,
            span,
            source,
            children,
        })
    }

    pub fn new_define(span: Span, source: Rc<GreenTree>, children: Vec<Item>) -> Self {
        Self::Define(DefineList {
            span,
            source,
            children,
        })
    }
}

impl From<List> for Item {
    fn from(value: List) -> Self {
        Self::List(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleList {
    pub id: ast::ModId,
    pub span: Span,
    pub source: Rc<GreenTree>,
    pub children: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefineList {
    pub span: Span,
    pub source: Rc<GreenTree>,
    pub children: Vec<Item>,
}

impl DefineList {
    pub fn name(&self) -> Option<&VarAtom> {
        self.children
            .get(0)
            .and_then(Item::atom)
            .and_then(Atom::var)
    }

    pub fn expr(&self) -> Option<&Atom> {
        self.children.get(1).and_then(Item::atom)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Atom {
    Boolean(BooleanAtom),
    Char(CharAtom),
    Var(VarAtom),
}

impl Atom {
    pub fn new_boolean(span: Span, source: Rc<GreenTree>) -> Self {
        debug_assert!(matches!(
            source.kind(),
            SyntaxKind::True | SyntaxKind::False
        ));
        Self::Boolean(BooleanAtom { span, source })
    }

    pub fn new_char(span: Span, source: Rc<GreenTree>) -> Self {
        debug_assert!(matches!(source.kind(), SyntaxKind::Char));
        Self::Char(CharAtom { span, source })
    }

    pub fn new_var(
        span: Span,
        source: Rc<GreenTree>,
        source_module: ast::ModId,
        unique_name: String,
    ) -> Self {
        debug_assert!(matches!(source.kind(), SyntaxKind::Identifier));
        Self::Var(VarAtom {
            span,
            source,
            source_module,
            unique_name,
        })
    }

    pub fn var(&self) -> Option<&VarAtom> {
        match self {
            Atom::Var(v) => Some(v),
            _ => None,
        }
    }
}

impl From<Atom> for Item {
    fn from(value: Atom) -> Self {
        Self::Atom(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BooleanAtom {
    pub span: Span,
    pub source: Rc<GreenTree>,
}

impl BooleanAtom {
    pub fn value(&self) -> bool {
        self.source.kind() == SyntaxKind::True
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CharAtom {
    pub span: Span,
    pub source: Rc<GreenTree>,
}

impl CharAtom {
    pub fn value(&self) -> char {
        // PERF: don't clone the string
        parse_char(&self.source.to_string())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VarAtom {
    pub span: Span,
    pub source: Rc<GreenTree>,
    pub source_module: ast::ModId,
    pub unique_name: String,
}

/// A [`ModuleInterface`] is the outer view of a [`Module`] as seen from another [`Module`]. It
/// contains its location, name and its list of exported bindings.
#[derive(Debug, Clone, Hash)]
pub struct ModuleInterface {
    pub id: ast::ModId,
    pub span: Span,
    // TODO: bring Binding here
    /// Exported bindings.
    pub bindings: Env<'static, String, Binding>,
    pub types: Option<Env<'static, String, Rc<Ty>>>,
    pub dependencies: Vec<ast::ModId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub mid: ast::ModId,
    pub span: Span,
    pub source: List,
    pub dependencies: Vec<ast::ModId>,
    /// Bindings exported from the Module.
    pub exports: Env<'static, String, Binding>,
    /// All the root bindings (e.g. macro, value) of the module.
    pub bindings: Env<'static, String, Binding>,
    //pub types: Option<Env<'static, String, Rc<Ty>>>,
}

impl Module {
    pub fn items(&self) -> &[Item] {
        &self.source.children()
    }

    pub fn to_interface(&self) -> ModuleInterface {
        ModuleInterface {
            id: self.mid,
            span: self.span,
            bindings: self.exports.clone(),
            dependencies: self.dependencies.clone(),
            types: None,
        }
    }
}

//#[derive(Clone, PartialEq, Eq, Hash)]
//pub enum Item {
//    Mod(Module),
//    Def(Define),
//    Expr(Expr),
//}

impl fmt::Debug for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self {
                Item::List(l) => l.fmt(f),
                Item::Atom(a) => a.fmt(f),
                //Item::Mod(m) => m.fmt(f),
                //Item::Def(d) => d.fmt(f),
                //Item::Expr(e) => e.fmt(f),
            }
        } else {
            match self {
                Item::List(l) => f.debug_tuple("Item::List").field(&l).finish(),
                Item::Atom(a) => f.debug_tuple("Item::Atom").field(&a).finish(),
                //Item::Mod(m) => f.debug_tuple("Item::Mod").field(&m).finish(),
                //Item::Def(d) => f.debug_tuple("Item::Def").field(&d).finish(),
                //Item::Expr(e) => f.debug_tuple("Item::Expr").field(&e).finish(),
            }
        }
    }
}

impl fmt::Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let indentation = " ".repeat(width);

            match self {
                Atom::Boolean(b) => write!(
                    f,
                    "{indentation}{{{} {}}}",
                    if b.value() { "#t" } else { "#f" },
                    b.span,
                ),
                Atom::Char(c) => {
                    let v = c.value();
                    write!(
                        f,
                        "{indentation}{{#\\{} {}}}",
                        if v.is_alphanumeric() {
                            format!("{v}")
                        } else {
                            format!("x{:X}", u32::from(v))
                        },
                        c.span
                    )
                }
                Atom::Var(v) => {
                    write!(
                        f,
                        "{indentation}{{var |{}| {} {}}}",
                        v.unique_name,
                        v.source_module.resolve(),
                        v.span
                    )
                } //ExprKind::If(i) => write!(
                  //    f,
                  //    "{indentation}{{if {}{}{}{}}}",
                  //    self.span,
                  //    match i.cond() {
                  //        Some(cond) =>
                  //            format!("\n{cond:#width$?}", width = width + INDENTATION_WIDTH),
                  //        None => String::new(),
                  //    },
                  //    match i.tru() {
                  //        Some(tru) => format!("\n{tru:#width$?}", width = width + INDENTATION_WIDTH),
                  //        None => String::new(),
                  //    },
                  //    match i.fls() {
                  //        Some(fls) => format!("\n{fls:#width$?}", width = width + INDENTATION_WIDTH),
                  //        None => format!(
                  //            "\n{}{{void {}}}",
                  //            " ".repeat(width + INDENTATION_WIDTH),
                  //            self.span
                  //        ),
                  //    },
                  //),
                  //ExprKind::List(l) if l.children.is_empty() => {
                  //    write!(f, "{indentation}{{() {}}}", self.span)
                  //}
                  //ExprKind::List(l) => write!(
                  //    f,
                  //    "{indentation}{{list {}\n{}}}",
                  //    self.span,
                  //    l.children
                  //        .iter()
                  //        .map(|e| format!("{e:#width$?}", width = width + INDENTATION_WIDTH))
                  //        .collect::<Vec<_>>()
                  //        .join("\n"),
                  //),
                  //ExprKind::Error(_) => todo!(),
            }
        } else {
            match self {
                Atom::Boolean(b) => f.debug_tuple("Atom::Boolean").field(&b).finish(),
                Atom::Char(c) => f.debug_tuple("Atom::Char").field(&c).finish(),
                Atom::Var(v) => f.debug_tuple("Atom::Var").field(&v).finish(),
            }
        }
    }
}

impl fmt::Debug for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let width = f.width().unwrap_or_default();
            let padding = " ".repeat(width);

            match self {
                List::Mod(_) => todo!(),
                List::Define(d) => {
                    write!(
                        f,
                        "{padding}{{define@{}\n{}{}}}",
                        d.span,
                        match &d.name() {
                            Some(n) => format!(
                                "{}{{|{}| {}}}",
                                " ".repeat(width + INDENTATION_WIDTH),
                                n.unique_name,
                                n.span
                            ),
                            None => String::from("#<no-value"),
                        },
                        if let Some(e) = &d.expr() {
                            format!("\n{e:#width$?}", width = width + INDENTATION_WIDTH)
                        } else {
                            String::new()
                        },
                    )
                }
            }
        } else {
            match self {
                List::Mod(_) => todo!(),
                List::Define(d) => f.debug_tuple("List::Define").field(&d).finish(),
            }
        }
    }
}

//impl Item {
//    pub fn expr(&self) -> Option<&Expr> {
//        match self {
//            Item::Expr(e) => Some(e),
//            _ => None,
//        }
//    }
//}
//
//#[derive(Clone, PartialEq, Eq, Hash)]
//pub struct Ident {
//    pub span: Span,
//    pub value: String,
//}
//
//impl fmt::Debug for Ident {
//    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//        if f.alternate() {
//            let padding = " ".repeat(f.width().unwrap_or_default());
//            write!(f, "{padding}{{|{}| {}}}", self.value, self.span)
//        } else {
//            f.debug_struct("Ident")
//                .field("span", &self.span)
//                .field("value", &self.value)
//                .finish()
//        }
//    }
//}
//
//#[derive(Clone, PartialEq, Eq, Hash)]
//pub struct Define {
//    pub span: Span,
//    pub green: Rc<GreenTree>,
//    pub name: Option<Ident>,
//    pub expr: Option<Box<Item>>,
//}
//
//impl Define {
//    pub fn into_item(self) -> Item {
//        Item::Def(self)
//    }
//}
//
//impl fmt::Debug for Define {
//    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//        if f.alternate() {
//            let width = f.width().unwrap_or_default();
//            let padding = " ".repeat(width);
//            write!(
//                f,
//                "{padding}{{define@{}\n{}{}{}}}",
//                self.span,
//                " ".repeat(width + INDENTATION_WIDTH),
//                match &self.name {
//                    Some(n) => format!("{:#?}", n),
//                    None => String::from("#<no-value"),
//                },
//                if let Some(e) = &self.expr {
//                    format!("\n{e:#width$?}", width = width + INDENTATION_WIDTH)
//                } else {
//                    String::new()
//                },
//            )
//        } else {
//            f.debug_struct("Define")
//                .field("span", &self.span)
//                .field("name", &self.name)
//                .field("expr", &self.expr)
//                .finish()
//        }
//    }
//}
//
//#[derive(Clone, PartialEq, Eq, Hash)]
//pub struct Expr {
//    pub span: Span,
//    pub kind: ExprKind,
//}
//
//impl Expr {
//    pub fn new_boolean(span: Span, green: &Rc<GreenTree>) -> Self {
//        Self {
//            span,
//            kind: ExprKind::Boolean(BooleanExpr {
//                green: Rc::clone(green),
//            }),
//        }
//    }
//
//    pub fn new_char(span: Span, green: &Rc<GreenTree>) -> Self {
//        Self {
//            span,
//            kind: ExprKind::Char(CharExpr {
//                green: Rc::clone(green),
//            }),
//        }
//    }
//
//    pub fn new_var(
//        span: Span,
//        green: &Rc<GreenTree>,
//        mid: ast::ModId,
//        unique_name: String,
//    ) -> Self {
//        Self {
//            span,
//            kind: ExprKind::Var(VarExpr {
//                mid,
//                unique_name,
//                green: Rc::clone(green),
//            }),
//        }
//    }
//
//    pub fn new_list(span: Span, green: &Rc<GreenTree>, children: Vec<Item>) -> Self {
//        Self {
//            span,
//            kind: ExprKind::List(ListExpr {
//                green: Rc::clone(green),
//                children,
//            }),
//        }
//    }
//
//    pub fn new_if(span: Span, green: &Rc<GreenTree>, children: Vec<Item>) -> Self {
//        Self {
//            span,
//            kind: ExprKind::If(IfExpr {
//                green: Rc::clone(green),
//                children,
//            }),
//        }
//    }
//
//    //pub fn new_quote(span: Span, green: &Rc<GreenTree>, expr: Expr) -> Self {
//    //    Self {
//    //        span,
//    //        kind: ExprKind::Quote(QuoteExpr {
//    //            green: Rc::clone(green),
//    //            expr: Box::new(expr),
//    //        }),
//    //    }
//    //}
//
//    pub fn into_item(self) -> Item {
//        Item::Expr(self)
//    }
//
//    pub fn into_error(self) -> Self {
//        Self {
//            span: self.span,
//            kind: ExprKind::Error(Box::new(self)),
//        }
//    }
//}
//
//impl fmt::Debug for Expr {
//    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//        if f.alternate() {
//            let width = f.width().unwrap_or_default();
//            let indentation = " ".repeat(width);
//
//            match &self.kind {
//                ExprKind::Boolean(b) => write!(
//                    f,
//                    "{indentation}{{{} {}}}",
//                    if b.value() { "#t" } else { "#f" },
//                    self.span,
//                ),
//                ExprKind::Char(c) => {
//                    let c = c.value();
//                    write!(
//                        f,
//                        "{indentation}{{#\\{} {}}}",
//                        if c.is_alphanumeric() {
//                            format!("{c}")
//                        } else {
//                            format!("x{:X}", u32::from(c))
//                        },
//                        self.span
//                    )
//                }
//                ExprKind::Var(v) => {
//                    write!(
//                        f,
//                        "{indentation}{{var |{}| {} {}}}",
//                        v.unique_name,
//                        v.mid.resolve(),
//                        self.span
//                    )
//                }
//                ExprKind::If(i) => write!(
//                    f,
//                    "{indentation}{{if {}{}{}{}}}",
//                    self.span,
//                    match i.cond() {
//                        Some(cond) =>
//                            format!("\n{cond:#width$?}", width = width + INDENTATION_WIDTH),
//                        None => String::new(),
//                    },
//                    match i.tru() {
//                        Some(tru) => format!("\n{tru:#width$?}", width = width + INDENTATION_WIDTH),
//                        None => String::new(),
//                    },
//                    match i.fls() {
//                        Some(fls) => format!("\n{fls:#width$?}", width = width + INDENTATION_WIDTH),
//                        None => format!(
//                            "\n{}{{void {}}}",
//                            " ".repeat(width + INDENTATION_WIDTH),
//                            self.span
//                        ),
//                    },
//                ),
//                ExprKind::List(l) if l.children.is_empty() => {
//                    write!(f, "{indentation}{{() {}}}", self.span)
//                }
//                ExprKind::List(l) => write!(
//                    f,
//                    "{indentation}{{list {}\n{}}}",
//                    self.span,
//                    l.children
//                        .iter()
//                        .map(|e| format!("{e:#width$?}", width = width + INDENTATION_WIDTH))
//                        .collect::<Vec<_>>()
//                        .join("\n"),
//                ),
//                ExprKind::Error(_) => todo!(),
//            }
//        } else {
//            f.debug_struct("Expr")
//                .field("span", &self.span)
//                .field("kind", &self.kind)
//                .finish()
//        }
//    }
//}
//
//#[derive(Debug, Clone, PartialEq, Eq, Hash)]
//pub enum ExprKind {
//    Boolean(BooleanExpr),
//    Char(CharExpr),
//    If(IfExpr),
//    Var(VarExpr),
//    List(ListExpr),
//    Error(Box<Expr>),
//    //Quote(QuoteExpr),
//}
//
//#[derive(Debug, Clone, PartialEq, Eq, Hash)]
//pub struct BooleanExpr {
//    pub green: Rc<GreenTree>,
//}
//
//impl BooleanExpr {
//    pub fn value(&self) -> bool {
//        self.green.kind() == SyntaxKind::True
//    }
//}
//
//#[derive(Debug, Clone, PartialEq, Eq, Hash)]
//pub struct CharExpr {
//    pub green: Rc<GreenTree>,
//}
//
//impl CharExpr {
//    pub fn value(&self) -> char {
//        parse_char(&self.green.to_string())
//    }
//}
//
//#[derive(Debug, Clone, PartialEq, Eq, Hash)]
//pub struct VarExpr {
//    pub mid: ast::ModId,
//    pub unique_name: String,
//    pub green: Rc<GreenTree>,
//}
//
//#[derive(Debug, Clone, PartialEq, Eq, Hash)]
//pub struct IfExpr {
//    pub green: Rc<GreenTree>,
//    pub children: Vec<Item>,
//}
//
//impl IfExpr {
//    pub fn cond(&self) -> Option<&Expr> {
//        self.children.first().and_then(Item::expr)
//    }
//
//    pub fn tru(&self) -> Option<&Expr> {
//        self.children.get(1).and_then(Item::expr)
//    }
//
//    pub fn fls(&self) -> Option<&Expr> {
//        self.children.get(2).and_then(Item::expr)
//    }
//}
//
//#[derive(Debug, Clone, PartialEq, Eq, Hash)]
//pub struct ListExpr {
//    pub green: Rc<GreenTree>,
//    pub children: Vec<Item>,
//}
//
//#[derive(Debug, Clone, PartialEq, Eq, Hash)]
//pub struct QuoteExpr {
//    pub green: Rc<GreenTree>,
//    pub expr: Box<Expr>,
//}
