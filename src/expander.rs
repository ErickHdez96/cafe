use std::{fmt, rc::Rc};

use crate::{
    config::Language,
    diagnostics::{Diagnostic, DiagnosticBuilder},
    env::Env,
    expander::intrinsics::import,
    file::FileId,
    span::Span,
    symbol::Symbol,
    syntax::{
        ast::{self, Item},
        cst::{Cst, CstKind, ListKind, Prefix},
        parser::{parse_char, parse_number, Number},
    },
};

use self::{
    binding::Binding,
    intrinsics::{intrinsics_env, module},
};

pub mod binding;
pub mod core;
pub mod intrinsics;
pub mod scopes;

type SynList = Source;
type CoreDefTransformer = fn(&mut Expander, SynList, Span, &mut Env<Symbol, Binding>) -> Item;
type CoreExprTransformer = fn(&mut Expander, SynList, Span, &mut Env<Symbol, Binding>) -> Item;
type BEnv<'parent> = Env<'parent, Symbol, Binding>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpanderResult {
    Mod(ExpanderResultMod),
    Items {
        file_id: FileId,
        items: Vec<Item>,
        diagnostics: Vec<Diagnostic>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpanderResultMod {
    pub file_id: FileId,
    pub module: ast::Module,
    pub diagnostics: Vec<Diagnostic>,
}

impl ExpanderResult {
    pub fn file_id(&self) -> FileId {
        match self {
            ExpanderResult::Mod(m) => m.file_id,
            ExpanderResult::Items { file_id, .. } => *file_id,
        }
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        match self {
            ExpanderResult::Mod(m) => &m.diagnostics,
            ExpanderResult::Items { diagnostics, .. } => diagnostics,
        }
    }
}

pub fn expand_module_with_config(
    module: Vec<Rc<Cst>>,
    config: ExpanderConfig,
) -> ExpanderResultMod {
    let mut e = Expander {
        language: config.language,
        file_id: config.file_id.expect("missing file_id"),
        diagnostics: vec![],
        module: config.mod_id.unwrap_or_else(ast::ModuleName::script),
        dependencies: vec![],
        module_stack: vec![],
        import: config.import.expect("an import function"),
        register: config.register.expect("a register function"),
    };
    let env = intrinsics_env();
    e.expand_module(module, config.base_env.unwrap_or(&env))
}

pub struct Expander<'i> {
    language: Language,
    file_id: FileId,
    diagnostics: Vec<Diagnostic>,
    module: ast::ModId,
    dependencies: Vec<ast::ModId>,
    module_stack: Vec<(ast::ModId, Vec<ast::ModId>)>,
    import: &'i dyn Fn(ast::ModId) -> Result<Rc<ast::ModuleInterface>, Diagnostic>,
    register: &'i dyn Fn(ast::ModId, ast::Module),
}

impl fmt::Debug for Expander<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Expander")
            .field("file_id", &self.file_id)
            .field("diagnostics", &self.diagnostics)
            .field("module", &self.module)
            .field("dependencies", &self.dependencies)
            .field("module_stack", &self.module_stack)
            .finish()
    }
}

#[derive(Default)]
pub struct ExpanderConfig<'i, 'env> {
    language: Language,
    import: Option<&'i dyn Fn(ast::ModId) -> Result<Rc<ast::ModuleInterface>, Diagnostic>>,
    register: Option<&'i dyn Fn(ast::ModId, ast::Module)>,
    file_id: Option<FileId>,
    base_env: Option<&'env BEnv<'env>>,
    mod_id: Option<ast::ModId>,
}

impl<'i, 'env> ExpanderConfig<'i, 'env> {
    pub fn language(mut self, language: Language) -> Self {
        self.language = language;
        self
    }

    pub fn import(
        mut self,
        importer: &'i dyn Fn(ast::ModId) -> Result<Rc<ast::ModuleInterface>, Diagnostic>,
    ) -> Self {
        self.import = Some(importer);
        self
    }

    pub fn register(mut self, register: &'i dyn Fn(ast::ModId, ast::Module)) -> Self {
        self.register = Some(register);
        self
    }

    pub fn file_id(mut self, file_id: FileId) -> Self {
        self.file_id = Some(file_id);
        self
    }

    pub fn base_env(mut self, base_env: &'env BEnv) -> Self {
        self.base_env = Some(base_env);
        self
    }

    pub fn mod_id(mut self, mod_id: ast::ModId) -> Self {
        self.mod_id = Some(mod_id);
        self
    }
}

impl Expander<'_> {
    pub fn expand_module(&mut self, mod_: Vec<Rc<Cst>>, env: &BEnv) -> ExpanderResultMod {
        let mut bindings = env.enter();
        let items = Source::new(mod_)
            .filter(|c| c.is_datum())
            .map(|c| self.expand_cst(c, Ctx::Toplevel, &mut bindings))
            .collect::<Vec<_>>();
        let span = items
            .first()
            .unwrap()
            .span()
            .extend(items.last().unwrap().span());

        ExpanderResultMod {
            file_id: self.file_id,
            module: ast::Module {
                id: self.module,
                span,
                bindings: Env::with_bindings(bindings.into_bindings()),
                dependencies: std::mem::take(&mut self.dependencies),
                exports: Env::default(),
                body: ast::Expr {
                    span,
                    kind: ast::ExprKind::Body(items),
                    ty: None,
                },
                types: None,
            },
            diagnostics: std::mem::take(&mut self.diagnostics),
        }
    }

    fn expand_cst(&mut self, cst: Rc<Cst>, ctx: Ctx, env: &mut BEnv) -> Item {
        let span = cst.span;

        if !cst.kind.is_list() {
            self.language.expand_expression(self, span, ctx);
        }

        match &cst.kind {
            CstKind::List(l, k) => self.expand_list(Source::new(l.clone()), *k, span, ctx, env),
            CstKind::Abbreviation(prefix, expr) => {
                self.expand_abbreviation(Rc::clone(prefix), Rc::clone(expr), span, ctx, env)
            }
            CstKind::Prefix(_) => todo!(),
            CstKind::Delim(_) => todo!(),
            CstKind::Dot => todo!(),
            CstKind::True | CstKind::False => ast::Expr {
                span,
                kind: ast::ExprKind::Boolean(cst.kind == CstKind::True),
                ty: None,
            }
            .into(),
            CstKind::Ident(ident) => self.expand_ident(*ident, span, env),
            CstKind::Char(c) => ast::Expr {
                span,
                kind: ast::ExprKind::Char(parse_char(c.resolve())),
                ty: None,
            }
            .into(),
            CstKind::Number(n) => ast::Expr {
                span,
                kind: ast::ExprKind::Number(match parse_number(n.resolve(), span) {
                    Ok(n) => n,
                    Err(d) => {
                        self.diagnostics.push(d);
                        Number::Fixnum(0)
                    }
                }),
                ty: None,
            }
            .into(),
            CstKind::String(_) => todo!(),
            _ => panic!(),
        }
    }

    fn expand_list(
        &mut self,
        mut source: Source,
        kind: ListKind,
        span: Span,
        ctx: Ctx,
        env: &mut BEnv,
    ) -> Item {
        match (source.peek(), kind) {
            (Some(cst), ListKind::List) => match &cst.as_ref().kind {
                CstKind::Ident(ident) => match env.get(ident) {
                    Some(Binding::CoreExprTransformer { transformer, .. }) => {
                        self.language.expand_expression(self, span, ctx);
                        transformer(self, source.reset(), span, env)
                    }
                    Some(Binding::CoreDefTransformer { transformer, .. }) => {
                        transformer(self, source.reset(), span, env)
                    }
                    Some(Binding::Import { .. }) => {
                        let mid = import(self, source.reset(), span, env);
                        ast::Item::Import(mid, span)
                    }
                    Some(Binding::Module { .. }) => {
                        let mid = module(self, source.reset(), span);
                        ast::Item::Mod(mid, span)
                    }
                    _ => {
                        self.language.expand_expression(self, span, ctx);
                        ast::Expr {
                            span,
                            kind: ast::ExprKind::List(
                                source
                                    .map(|c| match self.expand_cst(c, Ctx::Expr, env) {
                                        Item::Expr(e) => e,
                                        r => todo!("{r:?}"),
                                    })
                                    .collect(),
                            ),
                            ty: None,
                        }
                        .into()
                    }
                },
                _ => {
                    self.language.expand_expression(self, span, ctx);
                    ast::Expr {
                        span,
                        kind: ast::ExprKind::List(
                            source
                                .map(|c| match self.expand_cst(c, Ctx::Expr, env) {
                                    Item::Expr(e) => e,
                                    _ => todo!(),
                                })
                                .collect(),
                        ),
                        ty: None,
                    }
                    .into()
                }
            },
            (None, ListKind::List) => {
                self.emit_error(|b| b.msg("empty lists must be quoted").span(span));
                ast::Expr {
                    span,
                    kind: ast::ExprKind::List(vec![]),
                    ty: None,
                }
                .into()
            }
            (None, ListKind::Vector) => todo!(),
            (_, _) => todo!(),
        }
    }

    fn expand_ident(&mut self, ident: Symbol, span: Span, env: &BEnv) -> Item {
        match env.get(&ident) {
            Some(Binding::Value { orig_module, .. }) => ast::Expr {
                span,
                kind: ast::ExprKind::Var(ast::Path {
                    span,
                    module: *orig_module,
                    value: ident,
                }),
                ty: None,
            }
            .into(),
            None => {
                self.emit_error(|b| b.msg(format!("undefined variable {ident}")).span(span));
                ast::Expr {
                    span,
                    kind: ast::ExprKind::Var(ast::Path {
                        span,
                        module: self.module,
                        value: ident,
                    }),
                    ty: None,
                }
                .into()
            }
            _ => todo!(),
        }
    }

    fn expand_abbreviation(
        &mut self,
        prefix: Rc<Cst>,
        expr: Rc<Cst>,
        span: Span,
        ctx: Ctx,
        env: &mut BEnv,
    ) -> Item {
        let prefix_span = prefix.span;
        let CstKind::Prefix(prefix) = &prefix.kind else {
            panic!("expected a prefix");
        };

        match prefix {
            Prefix::Quote => self.expand_cst(
                Rc::new(Cst {
                    span,
                    kind: CstKind::List(
                        vec![
                            Rc::new(Cst {
                                span: prefix_span,
                                kind: CstKind::Ident("quote".into()),
                            }),
                            expr,
                        ],
                        ListKind::List,
                    ),
                }),
                ctx,
                env,
            ),
            Prefix::Backtick => todo!(),
            Prefix::Comma => todo!(),
            Prefix::CommaAt => todo!(),
            Prefix::HashQuote => todo!(),
            Prefix::HashBacktick => todo!(),
            Prefix::HashComma => todo!(),
            Prefix::HashCommaAt => todo!(),
        }
    }

    fn import(&mut self, mid: ast::ModId) -> Result<Rc<ast::ModuleInterface>, Diagnostic> {
        self.dependencies.push(mid);
        (self.import)(mid)
    }

    const fn current_module(&self) -> ast::ModId {
        self.module
    }

    fn emit_error(&mut self, builder: impl Fn(DiagnosticBuilder) -> DiagnosticBuilder) {
        self.diagnostics
            .push(builder(Diagnostic::builder()).finish());
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Ctx {
    Toplevel,
    Lambda,
    Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Source {
    source: Vec<Rc<Cst>>,
    offset: usize,
}

impl Source {
    fn new(source: Vec<Rc<Cst>>) -> Self {
        Self { source, offset: 0 }
    }

    fn reset(mut self) -> Self {
        self.offset = 0;
        self
    }

    fn peek_raw(&self) -> Option<Rc<Cst>> {
        self.source.get(self.offset).map(Rc::clone)
    }

    fn next_raw(&mut self) -> Option<Rc<Cst>> {
        match self.source.get_mut(self.offset) {
            Some(syn) => {
                self.offset += 1;
                Some(Rc::clone(syn))
            }
            None => None,
        }
    }

    fn peek(&mut self) -> Option<Rc<Cst>> {
        while let Some(syn) = self.peek_raw() {
            if syn.is_datum() {
                break;
            }
            if syn.kind == CstKind::Dot {
                return None;
            }
            self.next_raw();
        }
        self.peek_raw()
    }

    fn dot(self) -> Self {
        match self.peek_raw() {
            Some(cst) if cst.kind == CstKind::Dot => Self {
                source: self.source,
                offset: self.offset + 1,
            },
            _ => self,
        }
    }
}

impl Iterator for Source {
    type Item = Rc<Cst>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek() {
            Some(_) => self.next_raw(),
            None => None,
        }
    }
}

impl Language {
    fn expand_expression(self, expander: &mut Expander, span: Span, ctx: Ctx) {
        if let (Self::Cafe, Ctx::Toplevel) = (self, ctx) {
            expander.emit_error(|b| {
                b.msg("expressions aren't allowed as top-level items")
                    .span(span)
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use crate::{interner::Interner, test::Tester};

    use super::*;

    pub fn check(input: &str, expected: Expect) -> ExpanderResultMod {
        let res = Tester::with_input(input)
            .interner(&mut Interner::default())
            .test_expand_str();
        expected.assert_debug_eq(&res.module.body);
        res
    }

    #[test]
    fn boolean() {
        check(
            "(define _ #t)",
            expect![[r#"
                {body 0:0..13
                  {define@0:0..13
                    {|_| 0:8..1}
                    {#t 0:10..2}}}
            "#]],
        );
        check(
            "(define _ #f)",
            expect![[r#"
                {body 0:0..13
                  {define@0:0..13
                    {|_| 0:8..1}
                    {#f 0:10..2}}}
            "#]],
        );
    }

    #[test]
    fn char() {
        check(
            r"(define _ #\a)",
            expect![[r#"
                {body 0:0..14
                  {define@0:0..14
                    {|_| 0:8..1}
                    {#\a 0:10..3}}}
            "#]],
        );
        check(
            r"(define _ #\λ)",
            expect![[r#"
                {body 0:0..15
                  {define@0:0..15
                    {|_| 0:8..1}
                    {#\λ 0:10..4}}}
            "#]],
        );
        check(
            r"(define _ #\x3bb)",
            expect![[r#"
                {body 0:0..17
                  {define@0:0..17
                    {|_| 0:8..1}
                    {#\λ 0:10..6}}}
            "#]],
        );
        check(
            r"(define _ #\nul)",
            expect![[r#"
                {body 0:0..16
                  {define@0:0..16
                    {|_| 0:8..1}
                    {#\x0 0:10..5}}}
            "#]],
        );
        check(
            r"(define _ #\alarm)",
            expect![[r#"
                {body 0:0..18
                  {define@0:0..18
                    {|_| 0:8..1}
                    {#\x7 0:10..7}}}
            "#]],
        );
        check(
            r"(define _ #\backspace)",
            expect![[r#"
                {body 0:0..22
                  {define@0:0..22
                    {|_| 0:8..1}
                    {#\x8 0:10..11}}}
            "#]],
        );
        check(
            r"(define _ #\tab)",
            expect![[r#"
                {body 0:0..16
                  {define@0:0..16
                    {|_| 0:8..1}
                    {#\x9 0:10..5}}}
            "#]],
        );
        check(
            r"(define _ #\linefeed)",
            expect![[r#"
                {body 0:0..21
                  {define@0:0..21
                    {|_| 0:8..1}
                    {#\xA 0:10..10}}}
            "#]],
        );
        check(
            r"(define _ #\newline)",
            expect![[r#"
                {body 0:0..20
                  {define@0:0..20
                    {|_| 0:8..1}
                    {#\xA 0:10..9}}}
            "#]],
        );
        check(
            r"(define _ #\vtab)",
            expect![[r#"
                {body 0:0..17
                  {define@0:0..17
                    {|_| 0:8..1}
                    {#\xB 0:10..6}}}
            "#]],
        );
        check(
            r"(define _ #\page)",
            expect![[r#"
                {body 0:0..17
                  {define@0:0..17
                    {|_| 0:8..1}
                    {#\xC 0:10..6}}}
            "#]],
        );
        check(
            r"(define _ #\return)",
            expect![[r#"
                {body 0:0..19
                  {define@0:0..19
                    {|_| 0:8..1}
                    {#\xD 0:10..8}}}
            "#]],
        );
        check(
            r"(define _ #\esc)",
            expect![[r#"
                {body 0:0..16
                  {define@0:0..16
                    {|_| 0:8..1}
                    {#\x1B 0:10..5}}}
            "#]],
        );
        check(
            r"(define _ #\space)",
            expect![[r#"
                {body 0:0..18
                  {define@0:0..18
                    {|_| 0:8..1}
                    {#\x20 0:10..7}}}
            "#]],
        );
        check(
            r"(define _ #\delete)",
            expect![[r#"
                {body 0:0..19
                  {define@0:0..19
                    {|_| 0:8..1}
                    {#\x7F 0:10..8}}}
            "#]],
        );
    }

    #[test]
    fn number() {
        check(
            "(define _ 0)",
            expect![[r#"
                {body 0:0..12
                  {define@0:0..12
                    {|_| 0:8..1}
                    {0 0:10..1}}}
            "#]],
        );
        check(
            "(define _ 3)",
            expect![[r#"
                {body 0:0..12
                  {define@0:0..12
                    {|_| 0:8..1}
                    {3 0:10..1}}}
            "#]],
        );
    }

    #[test]
    fn variables() {
        check(
            "(define _ and)",
            expect![[r#"
                {body 0:0..14
                  {define@0:0..14
                    {|_| 0:8..1}
                    {var |and| (rnrs intrinsics ()) 0:10..3}}}
            "#]],
        );
    }

    #[test]
    fn function_application() {
        check(
            r"(define _ (and #t #f))",
            expect![[r#"
                {body 0:0..22
                  {define@0:0..22
                    {|_| 0:8..1}
                    {list 0:10..11
                      {var |and| (rnrs intrinsics ()) 0:11..3}
                      {#t 0:15..2}
                      {#f 0:18..2}}}}
            "#]],
        );
        check(
            r"(define _ ((lambda (x) x) #t))",
            expect![[r#"
                {body 0:0..30
                  {define@0:0..30
                    {|_| 0:8..1}
                    {list 0:10..19
                      {λ 0:11..14
                        ({|x| 0:20..1})
                        #f
                        {body 0:11..14
                          {var |x| (#script ()) 0:23..1}}}
                      {#t 0:26..2}}}}
            "#]],
        );
        check(
            r"(define _ ((lambda (x) ((lambda (y) y) x)) #t))",
            expect![[r#"
                {body 0:0..47
                  {define@0:0..47
                    {|_| 0:8..1}
                    {list 0:10..36
                      {λ 0:11..31
                        ({|x| 0:20..1})
                        #f
                        {body 0:11..31
                          {list 0:23..18
                            {λ 0:24..14
                              ({|y| 0:33..1})
                              #f
                              {body 0:24..14
                                {var |y| (#script ()) 0:36..1}}}
                            {var |x| (#script ()) 0:39..1}}}}
                      {#t 0:43..2}}}}
            "#]],
        );
    }

    #[test]
    fn if_() {
        check(
            r"(define _ (if #\a #\b))",
            expect![[r#"
                {body 0:0..23
                  {define@0:0..23
                    {|_| 0:8..1}
                    {if 0:10..12
                      {#\a 0:14..3}
                      {#\b 0:18..3}
                      {void 0:10..12}}}}
            "#]],
        );
        check(
            r"(define _ (if #t #f #\f))",
            expect![[r#"
                {body 0:0..25
                  {define@0:0..25
                    {|_| 0:8..1}
                    {if 0:10..14
                      {#t 0:14..2}
                      {#f 0:17..2}
                      {#\f 0:20..3}}}}
            "#]],
        );
    }

    mod lambda {
        use super::*;

        #[test]
        fn list() {
            check(
                "(define _ (lambda x x))",
                expect![[r#"
                    {body 0:0..23
                      {define@0:0..23
                        {|_| 0:8..1}
                        {λ 0:10..12
                          ()
                          {|x| 0:18..1}
                          {body 0:10..12
                            {var |x| (#script ()) 0:20..1}}}}}
                "#]],
            );
        }

        #[test]
        fn id() {
            check(
                "(define _ (lambda (x) x))",
                expect![[r#"
                    {body 0:0..25
                      {define@0:0..25
                        {|_| 0:8..1}
                        {λ 0:10..14
                          ({|x| 0:19..1})
                          #f
                          {body 0:10..14
                            {var |x| (#script ()) 0:22..1}}}}}
                "#]],
            );
        }

        #[test]
        fn rest() {
            check(
                "(define _ (lambda (x . y) y))",
                expect![[r#"
                    {body 0:0..29
                      {define@0:0..29
                        {|_| 0:8..1}
                        {λ 0:10..18
                          ({|x| 0:19..1})
                          {|y| 0:23..1}
                          {body 0:10..18
                            {var |y| (#script ()) 0:26..1}}}}}
                "#]],
            );
        }

        #[test]
        fn lambda_with_define() {
            check(
                "(define _ (lambda x (define y x) y))",
                expect![[r#"
                    {body 0:0..36
                      {define@0:0..36
                        {|_| 0:8..1}
                        {λ 0:10..25
                          ()
                          {|x| 0:18..1}
                          {body 0:10..25
                            {define@0:20..12
                              {|y| 0:28..1}
                              {var |x| (#script ()) 0:30..1}}
                            {var |y| (#script ()) 0:33..1}}}}}
                "#]],
            );
        }
    }

    mod define {
        use super::*;

        #[test]
        fn simple_variable() {
            check(
                "(define x 1)",
                expect![[r#"
                    {body 0:0..12
                      {define@0:0..12
                        {|x| 0:8..1}
                        {1 0:10..1}}}
                "#]],
            );
        }

        #[test]
        fn lambda() {
            check(
                "(define id (lambda (x) x))",
                expect![[r#"
                    {body 0:0..26
                      {define@0:0..26
                        {|id| 0:8..2}
                        {λ 0:11..14
                          ({|x| 0:20..1})
                          #f
                          {body 0:11..14
                            {var |x| (#script ()) 0:23..1}}}}}
                "#]],
            );
        }
    }

    mod quote {
        use super::*;

        #[test]
        fn boolean() {
            check(
                "(define _ (quote #t))",
                expect![[r#"
                    {body 0:0..21
                      {define@0:0..21
                        {|_| 0:8..1}
                        {quote 0:10..10
                          {#t 0:17..2}}}}
                "#]],
            );
            check(
                "(define _ (quote #T))",
                expect![[r#"
                    {body 0:0..21
                      {define@0:0..21
                        {|_| 0:8..1}
                        {quote 0:10..10
                          {#t 0:17..2}}}}
                "#]],
            );
            check(
                "(define _ (quote #f))",
                expect![[r#"
                    {body 0:0..21
                      {define@0:0..21
                        {|_| 0:8..1}
                        {quote 0:10..10
                          {#f 0:17..2}}}}
                "#]],
            );
            check(
                "(define _ (quote #F))",
                expect![[r#"
                    {body 0:0..21
                      {define@0:0..21
                        {|_| 0:8..1}
                        {quote 0:10..10
                          {#f 0:17..2}}}}
                "#]],
            );
        }

        #[test]
        fn char() {
            check(
                r"(define _ (quote #\a))",
                expect![[r#"
                    {body 0:0..22
                      {define@0:0..22
                        {|_| 0:8..1}
                        {quote 0:10..11
                          {#\a 0:17..3}}}}
                "#]],
            );
        }

        #[test]
        fn number() {
            check(
                r"(define _ (quote 3))",
                expect![[r#"
                    {body 0:0..20
                      {define@0:0..20
                        {|_| 0:8..1}
                        {quote 0:10..9
                          {3 0:17..1}}}}
                "#]],
            );
        }

        #[test]
        fn symbol() {
            check(
                "(define _ (quote x))",
                expect![[r#"
                    {body 0:0..20
                      {define@0:0..20
                        {|_| 0:8..1}
                        {quote 0:10..9
                          {var |x| (#script ()) 0:17..1}}}}
                "#]],
            );
            check(
                "(define _ (quote lambda))",
                expect![[r#"
                    {body 0:0..25
                      {define@0:0..25
                        {|_| 0:8..1}
                        {quote 0:10..14
                          {var |lambda| (#script ()) 0:17..6}}}}
                "#]],
            );
        }

        #[test]
        fn null() {
            check(
                "(define _ (quote ()))",
                expect![[r#"
                    {body 0:0..21
                      {define@0:0..21
                        {|_| 0:8..1}
                        {quote 0:10..10
                          {() 0:17..2}}}}
                "#]],
            );
        }

        #[test]
        fn list() {
            check(
                "(define _ (quote (x)))",
                expect![[r#"
                    {body 0:0..22
                      {define@0:0..22
                        {|_| 0:8..1}
                        {quote 0:10..11
                          {list 0:17..3
                            {var |x| (#script ()) 0:18..1}}}}}
                "#]],
            );
            check(
                r"(define _ (quote (x ((#\λ)) . #t)))",
                expect![[r#"
                    {body 0:0..36
                      {define@0:0..36
                        {|_| 0:8..1}
                        {quote 0:10..25
                          {dotted-list 0:17..17
                            {var |x| (#script ()) 0:18..1}
                            {list 0:20..8
                              {list 0:21..6
                                {#\λ 0:22..4}}}
                            .
                            {#t 0:31..2}}}}}
                "#]],
            );
        }
    }

    #[test]
    fn quote_abbrev() {
        check(
            "(define _ '#t)",
            expect![[r#"
                {body 0:0..14
                  {define@0:0..14
                    {|_| 0:8..1}
                    {quote 0:10..3
                      {#t 0:11..2}}}}
            "#]],
        );
    }

    pub mod modules {
        use super::*;

        #[test]
        fn define_and_use_variable() {
            check(
                r"(import (rnrs expander core ()))
                  (define id (lambda (x) x))
                  (define _ (id #t))",
                expect![[r#"
                    {body 0:0..114
                      {import (rnrs expander core ())@0:0..32}
                      {define@0:51..26
                        {|id| 0:59..2}
                        {λ 0:62..14
                          ({|x| 0:71..1})
                          #f
                          {body 0:62..14
                            {var |x| (#script ()) 0:74..1}}}}
                      {define@0:96..18
                        {|_| 0:104..1}
                        {list 0:106..7
                          {var |id| (#script ()) 0:107..2}
                          {#t 0:110..2}}}}
                "#]],
            );
        }

        #[test]
        #[ignore]
        fn define_and_use_macro() {
            check(
                r"(import (rnrs expander core ()))
                  (define-syntax id (syntax-rules () [(_ e) e]))
                  (id #t)",
                expect![[r#"
                    mod (#script ()) @0:0..123
                      {letrec 0:0..123
                        ()
                        {#t 0:120..2}}
                "#]],
            );
        }

        #[test]
        #[ignore]
        fn define_let() {
            check(
                r"(import (rnrs expander core ()))
                  (define-syntax let
                    (syntax-rules () [(_ ((v e) ...) body0 body ...) ((lambda (v ...) body0 body ...) e ...)]))
                  (let ([x #t]
                        [y #\a])
                    (if x (quote y)))",
                expect![[r#"
                    mod (#script ()) @0:0..283
                      {letrec 0:0..283
                        ()
                        {list 0:200..83
                          {λ 0:200..83
                            ({|x 1| 0:207..1} {|y 2| 0:238..1})
                            #f
                            {letrec 0:200..83
                              ()
                              {if 0:266..16
                                {var |x 1| (#script ()) 0:270..1}
                                {quote 0:272..9
                                  {var |y| (#script ()) 0:279..1}}
                                {void 0:266..16}}}}
                          {#t 0:209..2}
                          {#\a 0:240..3}}}
                "#]],
            );
        }

        #[test]
        fn simple_module() {
            let res = check(
                r"(module (ID ()) (id)
                    (import (rnrs expander core ()))
                    (define id (lambda (x) x)))
                  (import (ID ()))
                  (define _ (id #\a))",
                expect![[r#"
                    {body 0:0..194
                      {module (ID ())@0:0..121}
                      {import (ID ())@0:140..16}
                      {define@0:175..19
                        {|_| 0:183..1}
                        {list 0:185..8
                          {var |id| (ID ()) 0:186..2}
                          {#\a 0:189..3}}}}
                "#]],
            );

            assert_eq!(
                res.module
                    .dependencies
                    .iter()
                    .map(|mid| mid.resolve().clone())
                    .collect::<Vec<_>>(),
                vec![ast::ModuleName {
                    paths: vec![Symbol::from("ID")],
                    versions: vec![],
                }]
            );
        }

        #[test]
        fn reexport() {
            let res = check(
                r"(module (rnrs base ()) (lambda)
                    (import (rnrs expander core ())))
                  (import (rnrs base ()))
                  (define _ (lambda (x) x))",
                expect![[r#"
                    {body 0:0..171
                      {module (rnrs base ())@0:0..85}
                      {import (rnrs base ())@0:104..23}
                      {define@0:146..25
                        {|_| 0:154..1}
                        {λ 0:156..14
                          ({|x| 0:165..1})
                          #f
                          {body 0:156..14
                            {var |x| (#script ()) 0:168..1}}}}}
                "#]],
            );

            assert_eq!(
                res.module
                    .dependencies
                    .iter()
                    .map(|mid| mid.resolve().clone())
                    .collect::<Vec<_>>(),
                vec![ast::ModuleName {
                    paths: vec![Symbol::from("rnrs"), Symbol::from("base")],
                    versions: vec![],
                }]
            );
        }

        #[test]
        fn multiple_defines() {
            check(
                r"(define _ #t)
                  (define _ #\a)",
                expect![[r#"
                    {body 0:0..46
                      {define@0:0..13
                        {|_| 0:8..1}
                        {#t 0:10..2}}
                      {define@0:32..14
                        {|_| 0:40..1}
                        {#\a 0:42..3}}}
                "#]],
            );
        }

        #[test]
        fn import_core() {
            check(
                "(import (rnrs expander core ()))
                 (define _ (lambda (x) x))",
                expect![[r#"
                    {body 0:0..75
                      {import (rnrs expander core ())@0:0..32}
                      {define@0:50..25
                        {|_| 0:58..1}
                        {λ 0:60..14
                          ({|x| 0:69..1})
                          #f
                          {body 0:60..14
                            {var |x| (#script ()) 0:72..1}}}}}
                "#]],
            );
            let res = check(
                "(import (rnrs expander core ()))
                 (define _ (if #t #f))",
                expect![[r#"
                    {body 0:0..71
                      {import (rnrs expander core ())@0:0..32}
                      {define@0:50..21
                        {|_| 0:58..1}
                        {if 0:60..10
                          {#t 0:64..2}
                          {#f 0:67..2}
                          {void 0:60..10}}}}
                "#]],
            );

            assert_eq!(
                res.module
                    .dependencies
                    .iter()
                    .map(|mid| mid.resolve().clone())
                    .collect::<Vec<_>>(),
                vec![ast::ModuleName {
                    paths: vec![
                        Symbol::from("rnrs"),
                        Symbol::from("expander"),
                        Symbol::from("core")
                    ],
                    versions: vec![],
                }]
            );
        }
    }
}
