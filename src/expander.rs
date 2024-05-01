use std::{fmt, rc::Rc};

use crate::{
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
    import: Option<&'i dyn Fn(ast::ModId) -> Result<Rc<ast::ModuleInterface>, Diagnostic>>,
    register: Option<&'i dyn Fn(ast::ModId, ast::Module)>,
    file_id: Option<FileId>,
    base_env: Option<&'env BEnv<'env>>,
    mod_id: Option<ast::ModId>,
}

impl<'i, 'env> ExpanderConfig<'i, 'env> {
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
            .map(|c| self.expand_cst(c, &mut bindings))
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
                    ty_hint: None,
                },
                types: None,
            },
            diagnostics: std::mem::take(&mut self.diagnostics),
        }
    }

    fn expand_cst(&mut self, cst: Rc<Cst>, env: &mut BEnv) -> Item {
        let span = cst.span;
        match &cst.kind {
            CstKind::List(l, k) => self.expand_list(Source::new(l.clone()), *k, span, env),
            CstKind::Abbreviation(prefix, expr) => {
                self.expand_abbreviation(Rc::clone(prefix), Rc::clone(expr), span, env)
            }
            CstKind::Prefix(_) => todo!(),
            CstKind::Delim(_) => todo!(),
            CstKind::Dot => todo!(),
            CstKind::True | CstKind::False => ast::Expr {
                span,
                kind: ast::ExprKind::Boolean(cst.kind == CstKind::True),
                ty_hint: None,
            }
            .into(),
            CstKind::Ident(ident) => self.expand_ident(*ident, span, env),
            CstKind::Char(c) => ast::Expr {
                span,
                kind: ast::ExprKind::Char(parse_char(c.resolve())),
                ty_hint: None,
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
                ty_hint: None,
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
        env: &mut BEnv,
    ) -> Item {
        match (source.peek(), kind) {
            (Some(cst), ListKind::List) => match &cst.as_ref().kind {
                CstKind::Ident(ident) => match env.get(ident) {
                    Some(Binding::CoreExprTransformer { transformer, .. }) => {
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
                    _ => ast::Expr {
                        span,
                        kind: ast::ExprKind::List(
                            source
                                .map(|c| match self.expand_cst(c, env) {
                                    Item::Expr(e) => e,
                                    r => todo!("{r:?}"),
                                })
                                .collect(),
                        ),
                        ty_hint: None,
                    }
                    .into(),
                },
                _ => ast::Expr {
                    span,
                    kind: ast::ExprKind::List(
                        source
                            .map(|c| match self.expand_cst(c, env) {
                                Item::Expr(e) => e,
                                _ => todo!(),
                            })
                            .collect(),
                    ),
                    ty_hint: None,
                }
                .into(),
            },
            (None, ListKind::List) => {
                self.emit_error(|b| b.msg("empty lists must be quoted").span(span));
                ast::Expr {
                    span,
                    kind: ast::ExprKind::List(vec![]),
                    ty_hint: None,
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
                ty_hint: None,
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
                    ty_hint: None,
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

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use crate::{interner::Interner, test::test_expand_str};

    use super::*;

    pub fn check(input: &str, expected: Expect) -> ExpanderResultMod {
        let res = test_expand_str(input, &mut Interner::default());
        expected.assert_debug_eq(&res.module.body);
        res
    }

    #[test]
    fn boolean() {
        check(
            "#t",
            expect![[r#"
                {body 0:0..2
                  {#t 0:0..2}}
            "#]],
        );
        check(
            "#f",
            expect![[r#"
                {body 0:0..2
                  {#f 0:0..2}}
            "#]],
        );
    }

    #[test]
    fn char() {
        check(
            r"#\a",
            expect![[r#"
                {body 0:0..3
                  {#\a 0:0..3}}
            "#]],
        );
        check(
            r"#\λ",
            expect![[r#"
                {body 0:0..4
                  {#\λ 0:0..4}}
            "#]],
        );
        check(
            r"#\x3bb",
            expect![[r#"
                {body 0:0..6
                  {#\λ 0:0..6}}
            "#]],
        );
        check(
            r"#\nul",
            expect![[r#"
                {body 0:0..5
                  {#\x0 0:0..5}}
            "#]],
        );
        check(
            r"#\alarm",
            expect![[r#"
                {body 0:0..7
                  {#\x7 0:0..7}}
            "#]],
        );
        check(
            r"#\backspace",
            expect![[r#"
                {body 0:0..11
                  {#\x8 0:0..11}}
            "#]],
        );
        check(
            r"#\tab",
            expect![[r#"
                {body 0:0..5
                  {#\x9 0:0..5}}
            "#]],
        );
        check(
            r"#\linefeed",
            expect![[r#"
                {body 0:0..10
                  {#\xA 0:0..10}}
            "#]],
        );
        check(
            r"#\newline",
            expect![[r#"
                {body 0:0..9
                  {#\xA 0:0..9}}
            "#]],
        );
        check(
            r"#\vtab",
            expect![[r#"
                {body 0:0..6
                  {#\xB 0:0..6}}
            "#]],
        );
        check(
            r"#\page",
            expect![[r#"
                {body 0:0..6
                  {#\xC 0:0..6}}
            "#]],
        );
        check(
            r"#\return",
            expect![[r#"
                {body 0:0..8
                  {#\xD 0:0..8}}
            "#]],
        );
        check(
            r"#\esc",
            expect![[r#"
                {body 0:0..5
                  {#\x1B 0:0..5}}
            "#]],
        );
        check(
            r"#\space",
            expect![[r#"
                {body 0:0..7
                  {#\x20 0:0..7}}
            "#]],
        );
        check(
            r"#\delete",
            expect![[r#"
                {body 0:0..8
                  {#\x7F 0:0..8}}
            "#]],
        );
    }

    #[test]
    fn number() {
        check(
            "0",
            expect![[r#"
                {body 0:0..1
                  {0 0:0..1}}
            "#]],
        );
        check(
            "3",
            expect![[r#"
                {body 0:0..1
                  {3 0:0..1}}
            "#]],
        );
    }

    #[test]
    fn variables() {
        check(
            "cons",
            expect![[r#"
                {body 0:0..4
                  {var |cons| (rnrs intrinsics ()) 0:0..4}}
            "#]],
        );
    }

    #[test]
    fn function_application() {
        check(
            r"(cons #t #f)",
            expect![[r#"
                {body 0:0..12
                  {list 0:0..12
                    {var |cons| (rnrs intrinsics ()) 0:1..4}
                    {#t 0:6..2}
                    {#f 0:9..2}}}
            "#]],
        );
        check(
            r"((lambda (x) x) #t)",
            expect![[r#"
                {body 0:0..19
                  {list 0:0..19
                    {λ 0:1..14
                      ({|x| 0:10..1})
                      #f
                      {body 0:1..14
                        {var |x| (#script ()) 0:13..1}}}
                    {#t 0:16..2}}}
            "#]],
        );
        check(
            r"((lambda (x) ((lambda (y) y) x)) #t)",
            expect![[r#"
                {body 0:0..36
                  {list 0:0..36
                    {λ 0:1..31
                      ({|x| 0:10..1})
                      #f
                      {body 0:1..31
                        {list 0:13..18
                          {λ 0:14..14
                            ({|y| 0:23..1})
                            #f
                            {body 0:14..14
                              {var |y| (#script ()) 0:26..1}}}
                          {var |x| (#script ()) 0:29..1}}}}
                    {#t 0:33..2}}}
            "#]],
        );
    }

    #[test]
    fn if_() {
        check(
            r"(if #\a #\b)",
            expect![[r#"
                {body 0:0..12
                  {if 0:0..12
                    {#\a 0:4..3}
                    {#\b 0:8..3}
                    {void 0:0..12}}}
            "#]],
        );
        check(
            r"(if #t #f #\f)",
            expect![[r#"
                {body 0:0..14
                  {if 0:0..14
                    {#t 0:4..2}
                    {#f 0:7..2}
                    {#\f 0:10..3}}}
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
                    {body 0:0..12
                      {λ 0:0..12
                        ()
                        {|x| 0:8..1}
                        {body 0:0..12
                          {var |x| (#script ()) 0:10..1}}}}
                "#]],
            );
        }

        #[test]
        fn id() {
            check(
                "(lambda (x) x)",
                expect![[r#"
                    {body 0:0..14
                      {λ 0:0..14
                        ({|x| 0:9..1})
                        #f
                        {body 0:0..14
                          {var |x| (#script ()) 0:12..1}}}}
                "#]],
            );
        }

        #[test]
        fn rest() {
            check(
                "(lambda (x . y) y)",
                expect![[r#"
                    {body 0:0..18
                      {λ 0:0..18
                        ({|x| 0:9..1})
                        {|y| 0:13..1}
                        {body 0:0..18
                          {var |y| (#script ()) 0:16..1}}}}
                "#]],
            );
        }

        #[test]
        fn lambda_with_define() {
            check(
                "(lambda x (define y x) y)",
                expect![[r#"
                    {body 0:0..25
                      {λ 0:0..25
                        ()
                        {|x| 0:8..1}
                        {body 0:0..25
                          {define@0:10..12
                            {|y| 0:18..1}
                            {var |x| (#script ()) 0:20..1}}
                          {var |y| (#script ()) 0:23..1}}}}
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
                "(quote #t)",
                expect![[r#"
                    {body 0:0..10
                      {quote 0:0..10
                        {#t 0:7..2}}}
                "#]],
            );
            check(
                "(quote #T)",
                expect![[r#"
                    {body 0:0..10
                      {quote 0:0..10
                        {#t 0:7..2}}}
                "#]],
            );
            check(
                "(quote #f)",
                expect![[r#"
                    {body 0:0..10
                      {quote 0:0..10
                        {#f 0:7..2}}}
                "#]],
            );
            check(
                "(quote #F)",
                expect![[r#"
                    {body 0:0..10
                      {quote 0:0..10
                        {#f 0:7..2}}}
                "#]],
            );
        }

        #[test]
        fn char() {
            check(
                r"(quote #\a)",
                expect![[r#"
                    {body 0:0..11
                      {quote 0:0..11
                        {#\a 0:7..3}}}
                "#]],
            );
        }

        #[test]
        fn number() {
            check(
                r"(quote 3)",
                expect![[r#"
                    {body 0:0..9
                      {quote 0:0..9
                        {3 0:7..1}}}
                "#]],
            );
        }

        #[test]
        fn symbol() {
            check(
                "(quote x)",
                expect![[r#"
                    {body 0:0..9
                      {quote 0:0..9
                        {var |x| (#script ()) 0:7..1}}}
                "#]],
            );
            check(
                "(quote lambda)",
                expect![[r#"
                    {body 0:0..14
                      {quote 0:0..14
                        {var |lambda| (#script ()) 0:7..6}}}
                "#]],
            );
        }

        #[test]
        fn null() {
            check(
                "(quote ())",
                expect![[r#"
                    {body 0:0..10
                      {quote 0:0..10
                        {() 0:7..2}}}
                "#]],
            );
        }

        #[test]
        fn list() {
            check(
                "(quote (x))",
                expect![[r#"
                    {body 0:0..11
                      {quote 0:0..11
                        {list 0:7..3
                          {var |x| (#script ()) 0:8..1}}}}
                "#]],
            );
            check(
                r"(quote (x ((#\λ)) . #t))",
                expect![[r#"
                    {body 0:0..25
                      {quote 0:0..25
                        {dotted-list 0:7..17
                          {var |x| (#script ()) 0:8..1}
                          {list 0:10..8
                            {list 0:11..6
                              {#\λ 0:12..4}}}
                          .
                          {#t 0:21..2}}}}
                "#]],
            );
        }
    }

    #[test]
    fn quote_abbrev() {
        check(
            "'#t",
            expect![[r#"
                {body 0:0..3
                  {quote 0:0..3
                    {#t 0:1..2}}}
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
                  (id #t)",
                expect![[r#"
                    {body 0:0..103
                      {import (rnrs expander core ())@0:0..32}
                      {define@0:51..26
                        {|id| 0:59..2}
                        {λ 0:62..14
                          ({|x| 0:71..1})
                          #f
                          {body 0:62..14
                            {var |x| (#script ()) 0:74..1}}}}
                      {list 0:96..7
                        {var |id| (#script ()) 0:97..2}
                        {#t 0:100..2}}}
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
                  (id #\a)",
                expect![[r#"
                    {body 0:0..183
                      {module (ID ())@0:0..121}
                      {import (ID ())@0:140..16}
                      {list 0:175..8
                        {var |id| (ID ()) 0:176..2}
                        {#\a 0:179..3}}}
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
                  (lambda (x) x)",
                expect![[r#"
                    {body 0:0..160
                      {module (rnrs base ())@0:0..85}
                      {import (rnrs base ())@0:104..23}
                      {λ 0:146..14
                        ({|x| 0:155..1})
                        #f
                        {body 0:146..14
                          {var |x| (#script ()) 0:158..1}}}}
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
        fn multiple_expressions() {
            check(
                r"#t
                  #\a",
                expect![[r#"
                    {body 0:0..24
                      {#t 0:0..2}
                      {#\a 0:21..3}}
                "#]],
            );
        }

        #[test]
        fn import_core() {
            check(
                "(import (rnrs expander core ()))
                 (lambda (x) x)",
                expect![[r#"
                    {body 0:0..64
                      {import (rnrs expander core ())@0:0..32}
                      {λ 0:50..14
                        ({|x| 0:59..1})
                        #f
                        {body 0:50..14
                          {var |x| (#script ()) 0:62..1}}}}
                "#]],
            );
            let res = check(
                "(import (rnrs expander core ()))
                 (if #t #f)",
                expect![[r#"
                    {body 0:0..60
                      {import (rnrs expander core ())@0:0..32}
                      {if 0:50..10
                        {#t 0:54..2}
                        {#f 0:57..2}
                        {void 0:50..10}}}
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
