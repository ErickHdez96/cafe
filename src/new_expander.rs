#![allow(dead_code, unused_variables)]
use std::{cell::Cell, fmt, rc::Rc};

use crate::{
    diagnostics::{Diagnostic, DiagnosticBuilder},
    env::Env,
    file::FileId,
    new_syntax::{
        ast::{self, Expr, Item},
        cst::{Cst, CstKind},
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
type CoreExprTransformer = fn(&mut Expander, SynList, &mut Env<String, Binding>) -> Item;
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

    expander.expand_root(
        SynSource::new(root.into_iter().map(|c| c.into()).collect()),
        base_env,
    )
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
        source: SynSource,
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

        for syn in source {
            match self.expand_syntax(syn, &mut bindings, Ctx::Body) {
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
        match syn {
            Syntax::Raw(cst) => match ctx {
                Ctx::Body => Expanded::Syn(cst.into()),
                Ctx::Defer | Ctx::Expr => {
                    Expanded::Item(Item::Expr(self.expand_expr(cst.into(), env)))
                }
            },
            Syntax::List(list) => self.expand_list_syntax(list, env, ctx),
            Syntax::Symbol(symbol) => match ctx {
                Ctx::Body => match resolve(&symbol, env) {
                    Some(b) => Expanded::Binding(symbol, b.clone()),
                    None => Expanded::Syn(symbol.into()),
                },
                Ctx::Defer | Ctx::Expr => {
                    Expanded::Item(Item::Expr(self.expand_expr(symbol.into(), env)))
                }
            },
        }
    }

    fn expand_list_syntax(
        &mut self,
        syn: SynList,
        env: &mut Env<String, Binding>,
        ctx: Ctx,
    ) -> Expanded {
        let mut source = SynSource::new(syn.value);
        match source.next() {
            Some(head) => match self.expand_syntax(head, env, Ctx::Body) {
                Expanded::Binding(
                    _,
                    Binding::CoreExprTransformer {
                        scopes,
                        name,
                        transformer,
                    },
                ) => match ctx {
                    Ctx::Body => Expanded::Syn(
                        SynList {
                            value: source.source,
                            source: syn.source,
                        }
                        .into(),
                    ),
                    Ctx::Defer | Ctx::Expr => Expanded::Item(transformer(
                        self,
                        SynList {
                            value: source.source,
                            source: syn.source,
                        },
                        env,
                    )),
                },
                Expanded::Binding(
                    symbol,
                    Binding::CoreDefTransformer {
                        scopes,
                        name,
                        transformer,
                    },
                ) => match ctx {
                    Ctx::Body => {
                        if let Some(Syntax::Symbol(symbol)) = source.peek() {
                            let var = Binding::new_var(
                                symbol.value(),
                                self.module,
                                symbol.scopes.clone(),
                            );
                            env.insert(symbol.value().into(), var);
                        }
                        Expanded::Syn(
                            SynList {
                                value: source.source,
                                source: syn.source,
                            }
                            .into(),
                        )
                    }
                    Ctx::Defer | Ctx::Expr => {
                        // TODO: Emit diagnostic for ctx = expr
                        Expanded::Item(transformer(
                            self,
                            SynList {
                                value: source.source,
                                source: syn.source,
                            },
                            env,
                        ))
                    }
                },
                _ => todo!(),
            },
            None => {
                todo!()
            }
        }
    }

    fn expand_expr(&mut self, syn: Syntax, env: &mut Env<String, Binding>) -> Expr {
        let span = syn.span();
        match syn {
            Syntax::Raw(ref cst) => match &cst.kind {
                CstKind::True | CstKind::False => ast::Expr {
                    span,
                    kind: ast::ExprKind::Boolean(syn.kind() == &CstKind::True),
                },
                CstKind::Char(c) => ast::Expr {
                    span,
                    kind: ast::ExprKind::Char(parse_char(c)),
                },
                CstKind::Number(ref n) => ast::Expr {
                    span,
                    kind: ast::ExprKind::Number(parse_number(n, span).unwrap_or_default()),
                },
                _ => todo!(),
            },
            Syntax::List(l) => match self.expand_list_syntax(l, env, Ctx::Expr) {
                Expanded::Item(Item::Expr(e)) => e,
                _ => todo!(),
            },
            Syntax::Symbol(symbol) => match resolve(&symbol, env) {
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
                None => {
                    self.emit_error(|b| {
                        b.msg(format!("undefined variable {}", symbol.value()))
                            .span(span)
                    });
                    ast::Expr {
                        span,
                        kind: ast::ExprKind::Var(ast::Path {
                            span,
                            module: self.module,
                            value: symbol.value().into(),
                        }),
                    }
                }
                _ => todo!(),
            },
        }
    }

    const fn current_module(&self) -> ast::ModId {
        self.module
    }

    fn emit_error(&mut self, builder: impl Fn(DiagnosticBuilder) -> DiagnosticBuilder) {
        self.diagnostics
            .push(builder(Diagnostic::builder()).finish());
    }
}

fn resolve<'env>(syn: &SynSymbol, env: &'env Env<String, Binding>) -> Option<&'env Binding> {
    env.get_all(syn.value())
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
struct SynSource {
    source: Vec<Syntax>,
    offset: usize,
}

impl SynSource {
    fn new(source: Vec<Syntax>) -> Self {
        Self { source, offset: 0 }
    }

    fn peek_raw(&self) -> Option<&Syntax> {
        self.source.get(self.offset)
    }

    fn next_raw(&mut self) -> Option<Syntax> {
        match self.source.get_mut(self.offset) {
            Some(syn) => {
                self.offset += 1;
                Some(syn.clone())
            }
            None => None,
        }
    }

    fn peek(&mut self) -> Option<&Syntax> {
        while let Some(syn) = self.peek_raw() {
            if syn.source().is_datum() {
                break;
            }
            if syn.kind() == &CstKind::Dot {
                return None;
            }
            self.next_raw();
        }
        self.peek_raw()
    }

    fn dot(self) -> Self {
        match self.peek_raw().map(Syntax::kind) {
            Some(CstKind::Dot) => Self {
                source: self.source,
                offset: self.offset + 1,
            },
            _ => self,
        }
    }
}

impl Iterator for SynSource {
    type Item = Syntax;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek() {
            Some(_) => self.next_raw(),
            None => None,
        }
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
        core.insert(
            String::from("lambda"),
            Binding::CoreExprTransformer {
                scopes: Scopes::core(),
                name: String::from("lambda"),
                transformer: core::lambda::lambda_transformer,
            },
        );
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
    #[ignore]
    fn function_application() {
        check(
            r"(cons #t #f)",
            expect![[r#"
                {list 0:0..12
                  {var |cons| (rnrs expander core ()) 0:1..4}
                  {#t 0:6..2}
                  {#f 0:9..2}}
            "#]],
        );
        check(
            r"((lambda (x) x) #t)",
            expect![[r#"
                {list 0:0..19
                  {λ 0:1..14
                    ({|x 1| 0:10..1})
                    #f
                    {letrec 0:1..14
                      ()
                      {var |x 1| (#script ()) 0:13..1}}}
                  {#t 0:16..2}}
            "#]],
        );
        check(
            r"((lambda (x) ((lambda (y) y) x)) #t)",
            expect![[r#"
                {list 0:0..36
                  {λ 0:1..31
                    ({|x 2| 0:10..1})
                    #f
                    {letrec 0:1..31
                      ()
                      {list 0:13..18
                        {λ 0:14..14
                          ({|y 3| 0:23..1})
                          #f
                          {letrec 0:14..14
                            ()
                            {var |y 3| (#script ()) 0:26..1}}}
                        {var |x 2| (#script ()) 0:29..1}}}}
                  {#t 0:33..2}}
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

    mod lambda {
        use super::*;

        #[test]
        fn list() {
            check(
                "(lambda x x)",
                expect![[r#"
                    {λ 0:0..12
                      ()
                      {|x 1| 0:8..1}
                      {letrec 0:0..12
                        ()
                        {var |x 1| (#script ()) 0:10..1}}}
                "#]],
            );
        }

        #[test]
        fn id() {
            check(
                "(lambda (x) x)",
                expect![[r#"
                    {λ 0:0..14
                      ({|x 1| 0:9..1})
                      #f
                      {letrec 0:0..14
                        ()
                        {var |x 1| (#script ()) 0:12..1}}}
                "#]],
            );
        }

        #[test]
        fn rest() {
            check(
                "(lambda (x . y) y)",
                expect![[r#"
                    {λ 0:0..18
                      ({|x 1| 0:9..1})
                      {|y 2| 0:13..1}
                      {letrec 0:0..18
                        ()
                        {var |y 2| (#script ()) 0:16..1}}}
                "#]],
            );
        }

        #[test]
        #[ignore]
        fn lambda_with_define() {
            check("(lambda x (define y x) y)", expect![[r#""#]]);
        }
    }

    #[test]
    #[ignore]
    fn quote() {
        check(
            "(quote #t)",
            expect![[r#"
                {quote 0:0..10
                  {#t 0:7..2}}
            "#]],
        );
        check(
            r"(quote #\a)",
            expect![[r#"
                {quote 0:0..11
                  {#\a 0:7..3}}
            "#]],
        );
        check(
            "(quote x)",
            expect![[r#"
                {quote 0:0..9
                  {var |x| (#script ()) 0:7..1}}
            "#]],
        );
        check(
            "(quote x)",
            expect![[r#"
                {quote 0:0..9
                  {var |x| (#script ()) 0:7..1}}
            "#]],
        );
        check(
            "(quote ())",
            expect![[r#"
                {quote 0:0..10
                  {() 0:7..2}}
            "#]],
        );
        check(
            "(quote (x))",
            expect![[r#"
                {quote 0:0..11
                  {list 0:7..3
                    {var |x| (#script ()) 0:8..1}}}
            "#]],
        );
        check(
            r"(quote (x ((#\λ)) . #t))",
            expect![[r#"
                {quote 0:0..25
                  {dotted-list 0:7..17
                    {var |x| (#script ()) 0:8..1}
                    {list 0:10..8
                      {list 0:11..6
                        {#\λ 0:12..4}}}
                    .
                    {#t 0:21..2}}}
            "#]],
        );
    }

    #[test]
    #[ignore]
    fn quote_abbrev() {
        check(
            "'#t",
            expect![[r#"
                {quote 0:0..10
                  {#t 0:7..2}}
            "#]],
        );
    }

    //pub mod modules {
    //    use crate::utils::Resolve;

    //    use super::*;

    //    pub fn check(input: &str, expected: Expect) -> ExpanderResult {
    //        let res = parse_str(input);
    //        assert_eq!(res.diagnostics, vec![]);
    //        let syn = SynRoot::new(&res.tree, res.file_id);
    //        let libs = Libs::default();
    //        let res = expand_root(
    //            syn,
    //            &Env::default(),
    //            |name| libs.import(name),
    //            |name, module| libs.define(name, module),
    //        );
    //        assert_eq!(res.diagnostics, vec![]);
    //        expected.assert_debug_eq(&res.module);
    //        res
    //    }

    //    #[test]
    //    fn define_and_use_variable() {
    //        check(
    //            r"(import (rnrs expander core ()))
    //              (define id (lambda (x) x))
    //              (id #t)",
    //            expect![[r#"
    //                mod (#script ()) @0:0..103
    //                  {letrec 0:0..103
    //                    {define@0:51..26
    //                      {|id| 0:59..2}
    //                      {λ 0:62..14
    //                        ({|x 1| 0:71..1})
    //                        #f
    //                        {letrec 0:62..14
    //                          ()
    //                          {var |x 1| (#script ()) 0:74..1}}}}
    //                    {list 0:96..7
    //                      {var |id| (#script ()) 0:97..2}
    //                      {#t 0:100..2}}}
    //            "#]],
    //        );
    //    }

    //    #[test]
    //    fn define_and_use_macro() {
    //        check(
    //            r"(import (rnrs expander core ()))
    //              (define-syntax id (syntax-rules () [(_ e) e]))
    //              (id #t)",
    //            expect![[r#"
    //                mod (#script ()) @0:0..123
    //                  {letrec 0:0..123
    //                    ()
    //                    {#t 0:120..2}}
    //            "#]],
    //        );
    //    }

    //    #[test]
    //    fn define_let() {
    //        check(
    //            r"(import (rnrs expander core ()))
    //              (define-syntax let
    //                (syntax-rules () [(_ ((v e) ...) body0 body ...) ((lambda (v ...) body0 body ...) e ...)]))
    //              (let ([x #t]
    //                    [y #\a])
    //                (if x (quote y)))",
    //            expect![[r#"
    //                mod (#script ()) @0:0..283
    //                  {letrec 0:0..283
    //                    ()
    //                    {list 0:200..83
    //                      {λ 0:200..83
    //                        ({|x 1| 0:207..1} {|y 2| 0:238..1})
    //                        #f
    //                        {letrec 0:200..83
    //                          ()
    //                          {if 0:266..16
    //                            {var |x 1| (#script ()) 0:270..1}
    //                            {quote 0:272..9
    //                              {var |y| (#script ()) 0:279..1}}
    //                            {void 0:266..16}}}}
    //                      {#t 0:209..2}
    //                      {#\a 0:240..3}}}
    //            "#]],
    //        );
    //    }

    //    #[test]
    //    fn simple_module() {
    //        let res = check(
    //            r"(module (ID ()) (id)
    //                (import (rnrs expander core ()))
    //                (define id (lambda (x) x)))
    //              (import (ID ()))
    //              (id #\a)",
    //            expect![[r#"
    //                mod (#script ()) @0:0..183
    //                  {letrec 0:0..183
    //                    ()
    //                    {list 0:175..8
    //                      {var |id| (ID ()) 0:176..2}
    //                      {#\a 0:179..3}}}
    //            "#]],
    //        );

    //        assert_eq!(
    //            res.module
    //                .dependencies
    //                .iter()
    //                .map(|mid| mid.resolve().clone())
    //                .collect::<Vec<_>>(),
    //            vec![ModuleName {
    //                paths: vec![String::from("ID")],
    //                versions: vec![],
    //            }]
    //        );
    //    }

    //    #[test]
    //    fn reexport() {
    //        let res = check(
    //            r"(module (rnrs base ()) (lambda)
    //                (import (rnrs expander core ())))
    //              (import (rnrs base ()))
    //              (lambda (x) x)",
    //            expect![[r#"
    //                mod (#script ()) @0:0..160
    //                  {letrec 0:0..160
    //                    ()
    //                    {λ 0:146..14
    //                      ({|x 1| 0:155..1})
    //                      #f
    //                      {letrec 0:146..14
    //                        ()
    //                        {var |x 1| (#script ()) 0:158..1}}}}
    //            "#]],
    //        );

    //        assert_eq!(
    //            res.module
    //                .dependencies
    //                .iter()
    //                .map(|mid| mid.resolve().clone())
    //                .collect::<Vec<_>>(),
    //            vec![ModuleName {
    //                paths: vec![String::from("rnrs"), String::from("base")],
    //                versions: vec![],
    //            }]
    //        );
    //    }

    //    #[test]
    //    fn multiple_expressions() {
    //        check(
    //            r"#t
    //              #\a",
    //            expect![[r#"
    //                mod (#script ()) @0:0..24
    //                  {letrec 0:0..24
    //                    ()
    //                    {#t 0:0..2}
    //                    {#\a 0:21..3}}
    //            "#]],
    //        );
    //    }

    //    #[test]
    //    fn import_core() {
    //        check(
    //            "(import (rnrs expander core ()))
    //             (lambda (x) x)",
    //            expect![[r#"
    //                mod (#script ()) @0:0..64
    //                  {letrec 0:0..64
    //                    ()
    //                    {λ 0:50..14
    //                      ({|x 1| 0:59..1})
    //                      #f
    //                      {letrec 0:50..14
    //                        ()
    //                        {var |x 1| (#script ()) 0:62..1}}}}
    //            "#]],
    //        );
    //        let res = check(
    //            "(import (rnrs expander core ()))
    //             (if #t #f)",
    //            expect![[r#"
    //                mod (#script ()) @0:0..60
    //                  {letrec 0:0..60
    //                    ()
    //                    {if 0:50..10
    //                      {#t 0:54..2}
    //                      {#f 0:57..2}
    //                      {void 0:50..10}}}
    //            "#]],
    //        );

    //        assert_eq!(
    //            res.module
    //                .dependencies
    //                .iter()
    //                .map(|mid| mid.resolve().clone())
    //                .collect::<Vec<_>>(),
    //            vec![ModuleName {
    //                paths: vec![
    //                    String::from("rnrs"),
    //                    String::from("expander"),
    //                    String::from("core")
    //                ],
    //                versions: vec![],
    //            }]
    //        );
    //    }
    //}
}
