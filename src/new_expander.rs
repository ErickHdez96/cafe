#![allow(dead_code, unused_variables)]
use std::{cell::Cell, fmt, rc::Rc};

use crate::{
    diagnostics::{Diagnostic, DiagnosticBuilder},
    env::Env,
    file::FileId,
    new_syntax::{
        ast::{self, Expr, Item},
        cst::{Cst, CstKind, ListKind},
        parser::{parse_char, parse_number},
    },
    span::Span,
};

use self::{
    intrinsics::intrinsics_env,
    scopes::Scopes,
    syntax::{SynList, SynSymbol, Syntax},
};

mod core;
mod intrinsics;
mod macros;
mod scopes;
mod syntax;

type CoreMacroDefTransformer = fn(&mut Expander, SynList, &mut Env<String, Binding>);
type CoreDefTransformer = fn(&mut Expander, SynList, &mut Env<String, Binding>) -> Item;
type CoreExprTransformer = fn(&mut Expander, SynList, &Env<String, Binding>) -> Item;
type SyntaxTransformer = fn(SynList) -> Result<Cst, Vec<Diagnostic>>;

pub struct Expander<'i> {
    import: &'i dyn Fn(ast::ModId) -> Result<Rc<ast::ModuleInterface>, Diagnostic>,
    register: &'i dyn Fn(ast::ModId, ast::Module),
    diagnostics: Vec<Diagnostic>,
    module: ast::ModId,
    dependencies: Vec<ast::ModId>,
    module_stack: Vec<(ast::ModId, Vec<ast::ModId>)>,
    file_id: FileId,
}

impl fmt::Debug for Expander<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExpanderResult")
            .field("import", &())
            .field("diagnostics", &self.diagnostics)
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpanderResult {
    pub file_id: FileId,
    pub items: Vec<Item>,
    pub diagnostics: Vec<Diagnostic>,
}

pub fn expand_root(
    root: Vec<Rc<Cst>>,
    file_id: FileId,
    base_env: &Env<'_, String, Binding>,
    import: impl Fn(ast::ModId) -> Result<Rc<ast::ModuleInterface>, Diagnostic>,
    register: impl Fn(ast::ModId, ast::Module),
) -> ExpanderResult {
    let mut expander = Expander {
        import: &import,
        register: &register,
        module: ast::ModuleName::script(),
        module_stack: vec![],
        dependencies: vec![],
        diagnostics: vec![],
        file_id,
    };

    let mid = ast::ModuleName::from_strings(vec!["rnrs", "expander", "intrinsics"]);
    (expander.register)(
        mid,
        ast::Module {
            id: mid,
            span: Span::dummy(),
            dependencies: vec![],
            exports: intrinsics_env(),
            bindings: Env::default(),
            body: ast::Expr::dummy(),
            //types: None,
        },
    );

    expander.expand_root(CstSource::new(root), base_env)
}

#[derive(Debug)]
enum Expanded {
    Syn(Syntax),
    Binding(SynSymbol, Binding),
    Item(Item),
}

impl Expander<'_> {
    fn expand_root(
        &mut self,
        source: CstSource,
        base_env: &Env<'_, String, Binding>,
    ) -> ExpanderResult {
        let mut bindings = base_env.enter();
        let mut deferred = vec![];
        // TODO: Make it optional
        let intrinsics = (self.import)(ast::ModuleName::from_strings(vec![
            "rnrs",
            "expander",
            "intrinsics",
        ]))
        .unwrap();
        for (v, b) in intrinsics.bindings.bindings() {
            bindings.insert(v.clone(), b.clone());
        }

        for cst in source {
            match self.expand_syntax(Syntax::new(cst), &mut bindings, Ctx::Body) {
                Expanded::Binding(syn, _) => deferred.push(Expanded::Syn(syn.into())),
                e => deferred.push(e),
            }
        }

        let mut items = vec![];

        for d in deferred {
            match d {
                Expanded::Syn(s) => match self.expand_syntax(s, &mut bindings, Ctx::Defer) {
                    Expanded::Syn(s) => panic!("expected a fully expanded item - {s:?}"),
                    Expanded::Binding(_, b) => panic!("expected a fully expanded item - {b:?}"),
                    Expanded::Item(i) => items.push(i),
                },
                Expanded::Binding(_, b) => panic!("expected a syntax element or an item - {b:?}"),
                Expanded::Item(i) => items.push(i),
            }
        }

        ExpanderResult {
            file_id: self.file_id,
            items,
            diagnostics: std::mem::take(&mut self.diagnostics),
        }
    }

    fn expand_syntax(&mut self, syn: Syntax, env: &mut Env<String, Binding>, ctx: Ctx) -> Expanded {
        let span = syn.span();
        match &syn.cst.kind {
            CstKind::List(l, ListKind::List) => self.expand_list_syntax(
                SynList {
                    value: l.clone(),
                    kind: ListKind::List,
                    span,
                    scopes: syn.scopes,
                },
                env,
                ctx,
            ),
            CstKind::List(_, _) => todo!(),
            CstKind::Abbreviation(_, _) => todo!(),
            CstKind::Prefix(_) => todo!(),
            CstKind::True | CstKind::False => match ctx {
                Ctx::Body => Expanded::Syn(syn),
                Ctx::Defer | Ctx::Expr => Expanded::Item(Item::Expr(ast::Expr {
                    span,
                    kind: ast::ExprKind::Boolean(syn.cst.kind == CstKind::True),
                })),
            },
            CstKind::HashSemicolon => todo!(),
            CstKind::Ident(ident) => match ctx {
                Ctx::Body => {
                    let symbol = SynSymbol::new(ident.clone(), span, syn.scopes);
                    match resolve(&symbol, env) {
                        Some(b) => Expanded::Binding(symbol, b.clone()),
                        None => Expanded::Syn(symbol.into()),
                    }
                }
                Ctx::Defer | Ctx::Expr => {
                    let symbol = SynSymbol::new(ident.clone(), span, syn.scopes);
                    match resolve(&symbol, env) {
                        Some(Binding::Value {
                            orig_module,
                            orig_name,
                            name,
                            ..
                        }) => Expanded::Item(Item::Expr(ast::Expr {
                            span,
                            kind: ast::ExprKind::Var(ast::Path {
                                span,
                                module: *orig_module,
                                value: name.as_ref().unwrap_or(orig_name).to_string(),
                            }),
                        })),
                        None => {
                            self.emit_error(|b| {
                                b.msg(format!("undefined variabel {}", symbol.value))
                                    .span(span)
                            });
                            Expanded::Item(Item::Expr(ast::Expr {
                                span,
                                kind: ast::ExprKind::Var(ast::Path {
                                    span,
                                    module: self.module,
                                    value: symbol.value,
                                }),
                            }))
                        }
                        _ => todo!(),
                    }
                }
            },
            CstKind::Char(ref c) => match ctx {
                Ctx::Body => Expanded::Syn(syn),
                Ctx::Defer | Ctx::Expr => Expanded::Item(Item::Expr(ast::Expr {
                    span,
                    kind: ast::ExprKind::Char(parse_char(c)),
                })),
            },
            CstKind::Number(ref n) => match ctx {
                Ctx::Body => Expanded::Syn(syn),
                Ctx::Defer | Ctx::Expr => Expanded::Item(Item::Expr(ast::Expr {
                    span,
                    kind: ast::ExprKind::Number(parse_number(n, span).unwrap_or_default()),
                })),
            },
            CstKind::String(_) => todo!(),
            _ => unreachable!(),
        }
    }

    fn expand_list_syntax(
        &mut self,
        syn: SynList,
        env: &mut Env<String, Binding>,
        ctx: Ctx,
    ) -> Expanded {
        let mut source = CstSource::new(syn.value);
        match source.next() {
            Some(head) => match self.expand_syntax(
                Syntax {
                    cst: head,
                    scopes: syn.scopes.clone(),
                },
                env,
                Ctx::Body,
            ) {
                Expanded::Syn(_) => todo!(),
                Expanded::Binding(
                    _,
                    Binding::CoreExprTransformer {
                        scopes,
                        name,
                        transformer,
                    },
                ) => Expanded::Item(transformer(
                    self,
                    SynList {
                        value: source.cst,
                        kind: ListKind::List,
                        span: syn.span,
                        scopes: syn.scopes,
                    },
                    env,
                )),
                Expanded::Binding(
                    symbol,
                    Binding::CoreDefTransformer {
                        scopes,
                        name,
                        transformer,
                    },
                ) => {
                    if let Some(cst) = source.peek() {
                        if let CstKind::Ident(ident) = &cst.kind {
                            let var = Binding::new_var(ident, self.module, syn.scopes.clone());
                            env.insert(ident.into(), var);
                        }
                    }
                    Expanded::Item(transformer(
                        self,
                        SynList {
                            value: source.cst,
                            kind: ListKind::List,
                            span: syn.span,
                            scopes: syn.scopes,
                        },
                        env,
                    ))
                }
                Expanded::Item(_) => todo!(),
                _ => todo!(),
            },
            None => {
                todo!()
            }
        }
    }

    fn expand_expr(&mut self, syn: Syntax, env: &Env<String, Binding>) -> Expr {
        let span = syn.span();
        match &syn.cst.kind {
            CstKind::True | CstKind::False => ast::Expr {
                span,
                kind: ast::ExprKind::Boolean(syn.cst.kind == CstKind::True),
            },
            CstKind::Char(c) => ast::Expr {
                span,
                kind: ast::ExprKind::Char(parse_char(c)),
            },
            CstKind::Number(ref n) => ast::Expr {
                span,
                kind: ast::ExprKind::Number(parse_number(n, span).unwrap_or_default()),
            },
            CstKind::Ident(ident) => {
                let symbol = SynSymbol::new(ident.clone(), span, syn.scopes);
                match resolve(&symbol, env) {
                    Some(Binding::Value {
                        orig_module,
                        orig_name,
                        name,
                        ..
                    }) => ast::Expr {
                        span,
                        kind: ast::ExprKind::Var(ast::Path {
                            span,
                            module: *orig_module,
                            value: name.as_ref().unwrap_or(orig_name).to_string(),
                        }),
                    },
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
    }

    fn emit_error(&mut self, builder: impl Fn(DiagnosticBuilder) -> DiagnosticBuilder) {
        self.diagnostics
            .push(builder(Diagnostic::builder()).finish());
    }
}

fn resolve<'env>(syn: &SynSymbol, env: &'env Env<String, Binding>) -> Option<&'env Binding> {
    dbg!(env)
        .get_all(&syn.value)
        .into_iter()
        .fold((None, 0usize), |acc, cur| {
            match cur.scopes().subset(&syn.scopes) {
                Some(s) if s.get() >= acc.1 => (Some(cur), s.get()),
                _ => acc,
            }
        })
        .0
}

/// The context in which a piece of syntax is expanded.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Ctx {
    /// Used in:
    ///  * Root of a file.
    ///  * Body of a library.
    ///  * Body of a lambda.
    Body,
    /// Evaluate deferred syntax elements.
    Defer,
    /// Syntax must evaluate to an expression. A diagnostic is emitted otherwise.
    Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct CstSource {
    cst: Vec<Rc<Cst>>,
    offset: usize,
}

impl CstSource {
    fn new(cst: Vec<Rc<Cst>>) -> Self {
        Self { cst, offset: 0 }
    }

    fn peek_raw(&self) -> Option<Rc<Cst>> {
        self.cst.get(self.offset).cloned()
    }

    fn next_raw(&mut self) -> Option<Rc<Cst>> {
        match self.cst.get_mut(self.offset) {
            Some(cst) => {
                self.offset += 1;
                Some(Rc::clone(cst))
            }
            None => None,
        }
    }

    fn peek(&mut self) -> Option<Rc<Cst>> {
        while let Some(c) = self.peek_raw() {
            if c.is_datum() {
                break;
            }
            self.next_raw();
        }
        self.peek_raw()
    }
}

impl Iterator for CstSource {
    type Item = Rc<Cst>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(c) = self.peek_raw() {
            if c.is_datum() {
                break;
            }
            self.next_raw();
        }
        self.next_raw()
    }
}

/// The possible values a name may be bound to at compile time.
///
/// Every Binding has a set of scopes in which it is visible (for macro expansion).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Binding {
    /// A normal runtime value. The value isn't stored here, but is generated at runtime.
    Value {
        scopes: Scopes,
        /// The [`Module`] where it comes from. Macros uses may introduce bindings from other
        /// modules without needing to import them.
        orig_module: ast::ModId,
        /// The name as given in the lexical source. For debug and diagnostics.
        orig_name: String,
        /// Name used to identify ambigous identifiers after macro expansion. For debug and
        /// diagnostics.
        name: Option<String>,
    },
    /// Built-in importer binding.
    Import { scopes: Scopes },
    /// Built-in module defintion binding.
    Module { scopes: Scopes },
    /// Built-in `define` binding.
    CoreDefTransformer {
        scopes: Scopes,
        name: String,
        transformer: CoreDefTransformer,
    },
    /// Built-in `define-syntax` binding.
    CoreMacroDefTransformer {
        scopes: Scopes,
        name: String,
        transformer: CoreMacroDefTransformer,
    },
    /// Built-in macro-like expression bindings (e.g. if, lambda, quote, set!).
    CoreExprTransformer {
        scopes: Scopes,
        name: String,
        transformer: CoreExprTransformer,
    },
    /// An expression was compiled at compile-time into a transformer (e.g. lambda).
    SyntaxTransformer {
        source_module: ast::ModId,
        scopes: Scopes,
        name: String,
        transformer: SyntaxTransformer,
    },
    // Temporary transformer returned from syntax-rules. It should be moved to a normal
    // `SyntaxTransformer.
    //NativeSyntaxTransformer {
    //    scopes: Scopes,
    //    name: String,
    //    transformer: Rc<NativeSyntaxTransformer>,
    //},
}

impl Binding {
    /// Returns a new [`Binding::Value`] [`Binding`].
    pub fn new_var(name: &str, orig_module: ast::ModId, scopes: Scopes) -> Self {
        Self::Value {
            scopes,
            orig_name: name.to_string(),
            orig_module,
            name: Some(format!("{} {}", name, Self::new_id())),
        }
    }

    fn new_id() -> u64 {
        IDENTIFIER_ID.with(|id| {
            let old = id.get();
            id.set(old.saturating_add(1));
            old
        })
    }

    /// Returns the original name associated with the [`Binding`]. For debug and diagnostic
    /// purposes.
    pub fn name(&self) -> &str {
        match self {
            Binding::Import { .. } => "import",
            Binding::Module { .. } => "module",
            Binding::Value {
                name, orig_name, ..
            } => name.as_ref().unwrap_or(orig_name),
            Binding::CoreDefTransformer { name, .. }
            | Binding::CoreMacroDefTransformer { name, .. }
            | Binding::CoreExprTransformer { name, .. }
            | Binding::SyntaxTransformer { name, .. } => name,
            //| Binding::NativeSyntaxTransformer { name, .. } => name,
        }
    }

    pub fn scopes(&self) -> &Scopes {
        match self {
            Binding::Value { scopes, .. }
            | Binding::Import { scopes }
            | Binding::Module { scopes }
            | Binding::CoreDefTransformer { scopes, .. }
            | Binding::CoreMacroDefTransformer { scopes, .. }
            | Binding::CoreExprTransformer { scopes, .. }
            | Binding::SyntaxTransformer { scopes, .. } => scopes,
            //| Binding::NativeSyntaxTransformer { scopes, .. } => scopes,
        }
    }

    pub fn scopes_mut(&mut self) -> &mut Scopes {
        match self {
            Binding::Value { ref mut scopes, .. }
            | Binding::Import { ref mut scopes }
            | Binding::Module { ref mut scopes }
            | Binding::CoreDefTransformer { ref mut scopes, .. }
            | Binding::CoreMacroDefTransformer { ref mut scopes, .. }
            | Binding::CoreExprTransformer { ref mut scopes, .. }
            | Binding::SyntaxTransformer { ref mut scopes, .. } => scopes,
            //| Binding::NativeSyntaxTransformer { ref mut scopes, .. } => scopes,
        }
    }
}

thread_local! {
    static IDENTIFIER_ID: Cell<u64> = Cell::new(1);
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, collections::HashMap};

    use expect_test::{expect, Expect};

    use crate::{new_syntax::parser::parse_str, utils::Resolve};

    use super::*;

    fn core_env() -> Env<'static, String, Binding> {
        let mut core = Env::new();
        core.insert(
            String::from("define"),
            Binding::CoreDefTransformer {
                scopes: Scopes::core(),
                name: String::from("define"),
                transformer: core::define_transformer,
            },
        );
        //core.insert(
        //    String::from("define-syntax"),
        //    Binding::CoreMacroDefTransformer {
        //        scopes: Scopes::core(),
        //        name: String::from("define-syntax"),
        //        transformer: core::define_syntax_transformer,
        //    },
        //);
        //core.insert(
        //    String::from("lambda"),
        //    Binding::CoreExprTransformer {
        //        scopes: Scopes::core(),
        //        name: String::from("lambda"),
        //        transformer: core::lambda_transformer,
        //    },
        //);
        core.insert(
            String::from("if"),
            Binding::CoreExprTransformer {
                scopes: Scopes::core(),
                name: String::from("if"),
                transformer: core::if_transformer,
            },
        );
        //core.insert(
        //    String::from("quote"),
        //    Binding::CoreExprTransformer {
        //        scopes: Scopes::core(),
        //        name: String::from("quote"),
        //        transformer: core::quote_transformer,
        //    },
        //);
        core.insert(
            String::from("cons"),
            Binding::Value {
                scopes: Scopes::core(),
                orig_module: ast::ModuleName::from_strings(vec!["rnrs", "expander", "core"]),
                orig_name: String::from("cons"),
                name: None,
            },
        );
        core
    }

    struct Libs {
        libs: RefCell<HashMap<ast::ModId, ast::Module>>,
    }

    impl Default for Libs {
        fn default() -> Self {
            let mid = ast::ModuleName::from_strings(vec!["rnrs", "expander", "core"]);
            Self {
                libs: RefCell::new(HashMap::from([(
                    mid,
                    ast::Module {
                        id: mid,
                        span: Span::dummy(),
                        dependencies: vec![],
                        exports: core_env(),
                        bindings: Env::default(),
                        body: ast::Expr::dummy(),
                    },
                )])),
            }
        }
    }

    impl Libs {
        fn import(&self, mid: ast::ModId) -> Result<Rc<ast::ModuleInterface>, Diagnostic> {
            Ok(Rc::new(
                self.libs
                    .borrow()
                    .get(&mid)
                    .expect(&format!("{}", mid.resolve()))
                    .to_interface(),
            ))
        }

        fn define(&self, mid: ast::ModId, module: ast::Module) {
            self.libs.borrow_mut().insert(mid, module);
        }
    }

    pub fn check(input: &str, expected: Expect) {
        let res = parse_str(input);
        assert_eq!(res.diagnostics, vec![]);
        let env = core_env().enter_consume();
        let libs = Libs::default();
        let res = expand_root(
            res.root,
            res.file_id,
            &env,
            |mid| libs.import(mid),
            |mid, mod_| libs.define(mid, mod_),
        );
        assert_eq!(res.diagnostics, vec![]);
        let mut out = String::new();
        for item in res.items {
            out.push_str(&format!("{item:#?}"));
            out.push('\n');
        }
        expected.assert_eq(&out);
    }

    #[test]
    fn boolean() {
        check(
            "#t",
            expect![[r#"
                {#t 0:0..2}
            "#]],
        );
        check(
            "#f",
            expect![[r#"
                {#f 0:0..2}
            "#]],
        );
    }

    #[test]
    fn number() {
        check(
            "3",
            expect![[r#"
                {3 0:0..1}
            "#]],
        );
        check(
            "10",
            expect![[r#"
                {10 0:0..2}
            "#]],
        );
    }

    #[test]
    fn char() {
        check(
            r"#\a",
            expect![[r#"
                {#\a 0:0..3}
            "#]],
        );
        check(
            r"#\λ",
            expect![[r#"
                {#\λ 0:0..4}
            "#]],
        );
        check(
            r"#\x3bb",
            expect![[r#"
                {#\λ 0:0..6}
            "#]],
        );
        check(
            r"#\nul",
            expect![[r#"
                {#\x0 0:0..5}
            "#]],
        );
        check(
            r"#\alarm",
            expect![[r#"
                {#\x7 0:0..7}
            "#]],
        );
        check(
            r"#\backspace",
            expect![[r#"
                {#\x8 0:0..11}
            "#]],
        );
        check(
            r"#\tab",
            expect![[r#"
                {#\x9 0:0..5}
            "#]],
        );
        check(
            r"#\linefeed",
            expect![[r#"
                {#\xA 0:0..10}
            "#]],
        );
        check(
            r"#\newline",
            expect![[r#"
                {#\xA 0:0..9}
            "#]],
        );
        check(
            r"#\vtab",
            expect![[r#"
                {#\xB 0:0..6}
            "#]],
        );
        check(
            r"#\page",
            expect![[r#"
                {#\xC 0:0..6}
            "#]],
        );
        check(
            r"#\return",
            expect![[r#"
                {#\xD 0:0..8}
            "#]],
        );
        check(
            r"#\esc",
            expect![[r#"
                {#\x1B 0:0..5}
            "#]],
        );
        check(
            r"#\space",
            expect![[r#"
                {#\x20 0:0..7}
            "#]],
        );
        check(
            r"#\delete",
            expect![[r#"
                {#\x7F 0:0..8}
            "#]],
        );
    }

    #[test]
    fn variables() {
        check(
            "cons",
            expect![[r#"
                {var |cons| (rnrs expander core ()) 0:0..4}
            "#]],
        );
    }

    #[test]
    fn if_() {
        check(
            r"(if #\a #\b)",
            expect![[r#"
                {if 0:0..12
                  {#\a 0:4..3}
                  {#\b 0:8..3}
                  {void 0:0..12}}
            "#]],
        );
        check(
            r"(if #t #f #\f)",
            expect![[r#"
                {if 0:0..14
                  {#t 0:4..2}
                  {#f 0:7..2}
                  {#\f 0:10..3}}
            "#]],
        );
    }

    #[test]
    fn define_and_use_variable() {
        check(
            r"(define a #\a)
              a",
            expect![[r#"
                {define@0:0..14
                  {|a| 0:8..1}
                  {#\a 0:10..3}}
                {var |a 1| (#script ()) 0:29..1}
            "#]],
        );
    }
}
