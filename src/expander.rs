use std::{cell::Cell, fmt, rc::Rc, slice};

use crate::{
    diagnostics::{Diagnostic, DiagnosticBuilder},
    env::Env,
    expander::scopes::{Scope, Scopes},
    file::FileId,
    span::Span,
    syntax::{
        ast::{self, Module, ModuleInterface, ModuleName},
        cst::{GreenTree, RedTree, SynExp, SynList, SynRoot, SynSymbol},
        SyntaxKind,
    },
};

use self::{macros::NativeSyntaxTransformer, transformers::import};

pub mod macros;
pub mod scopes;
pub mod transformers;

type CoreDefTransformer = fn(&mut Expander, SynList, &Env<String, Binding>) -> ast::Expr;
type CoreExprTransformer = fn(&mut Expander, SynList, &Env<String, Binding>) -> ast::Expr;
type SyntaxTransformer = fn(SynList) -> Result<SynExp, Vec<Diagnostic>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExpanderResult {
    pub module: Module,
    pub diagnostics: Vec<Diagnostic>,
}

pub struct Expander<'i> {
    import: &'i dyn Fn(ModuleName) -> Result<Rc<ModuleInterface>, Diagnostic>,
    diagnostics: Vec<Diagnostic>,
}

impl fmt::Debug for Expander<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExpanderResult")
            .field("import", &())
            .field("diagnostics", &self.diagnostics)
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Binding {
    Value {
        scopes: Scopes,
        orig_name: String,
        name: Option<String>,
    },
    Import {
        scopes: Scopes,
    },
    CoreDefTransformer {
        scopes: Scopes,
        name: String,
        transformer: CoreDefTransformer,
    },
    CoreExprTransformer {
        scopes: Scopes,
        name: String,
        transformer: CoreExprTransformer,
    },
    SyntaxTransformer {
        scopes: Scopes,
        name: String,
        transformer: SyntaxTransformer,
    },
    NativeSyntaxTransformer {
        scopes: Scopes,
        name: String,
        transformer: Rc<NativeSyntaxTransformer>,
    },
}

impl Binding {
    pub fn new_var(name: &str, scopes: Scopes) -> Self {
        Self::Value {
            scopes,
            orig_name: name.to_string(),
            name: Some(format!("{} {}", name, Self::new_id())),
        }
    }

    pub fn new_id() -> u64 {
        IDENTIFIER_ID.with(|id| {
            let old = id.get();
            id.set(old.saturating_add(1));
            old
        })
    }

    pub fn name(&self) -> &str {
        match self {
            Binding::Import { .. } => "import",
            Binding::Value {
                name, orig_name, ..
            } => name.as_ref().unwrap_or(orig_name),
            Binding::CoreDefTransformer { name, .. }
            | Binding::CoreExprTransformer { name, .. }
            | Binding::SyntaxTransformer { name, .. }
            | Binding::NativeSyntaxTransformer { name, .. } => name,
        }
    }

    pub fn scopes(&self) -> &Scopes {
        match self {
            Binding::Value { scopes, .. }
            | Binding::Import { scopes }
            | Binding::CoreDefTransformer { scopes, .. }
            | Binding::CoreExprTransformer { scopes, .. }
            | Binding::SyntaxTransformer { scopes, .. }
            | Binding::NativeSyntaxTransformer { scopes, .. } => scopes,
        }
    }

    pub fn scopes_mut(&mut self) -> &mut Scopes {
        match self {
            Binding::Value { ref mut scopes, .. }
            | Binding::Import { ref mut scopes }
            | Binding::CoreDefTransformer { ref mut scopes, .. }
            | Binding::CoreExprTransformer { ref mut scopes, .. }
            | Binding::SyntaxTransformer { ref mut scopes, .. }
            | Binding::NativeSyntaxTransformer { ref mut scopes, .. } => scopes,
        }
    }
}

enum ItemSyn {
    Expr(ast::Expr),
    Syn(SynExp),
}

pub fn expand_root(
    syn: SynRoot,
    base_env: &Env<'_, String, Binding>,
    import: impl Fn(ModuleName) -> Result<Rc<ModuleInterface>, Diagnostic>,
) -> ExpanderResult {
    let mut expander = Expander {
        import: &import,
        diagnostics: vec![],
    };
    expander.expand_root(syn, base_env)
}

impl Expander<'_> {
    fn expand_root(&mut self, syn: SynRoot, base_env: &Env<'_, String, Binding>) -> ExpanderResult {
        let root_scope = Scope::new();
        let mut bindings = base_env.enter();
        let mut deferred = vec![];

        for i in syn.syn_children_with_scope(root_scope) {
            let def = self.expand_macro(i, &mut bindings);
            deferred.push(def);
        }

        let mut items = vec![];

        for d in deferred {
            let item = self.expand_item(d, &mut bindings);
            if let Some(item) = item {
                items.push(item);
            }
        }

        ExpanderResult {
            module: Module {
                span: syn.span(),
                items,
                bindings: Env::with_bindings(bindings.into_bindings()),
            },
            diagnostics: std::mem::take(&mut self.diagnostics),
        }
    }

    fn expand_macro(&mut self, syn: SynExp, env: &mut Env<String, Binding>) -> SynExp {
        if let SynExp::List(l) = syn {
            if let Some(SynExp::Symbol(s)) = l.sexps().first() {
                if let Some(Binding::NativeSyntaxTransformer { transformer, .. }) = resolve(s, env)
                {
                    let s = transformer.expand(self, l, env);
                    return self.expand_macro(s, env);
                }
            }
            SynExp::List(l)
        } else {
            syn
        }
    }

    fn expand_item(&mut self, syn: SynExp, env: &mut Env<String, Binding>) -> Option<ast::Item> {
        match syn {
            SynExp::List(l) => match l.sexps().iter().next() {
                Some(SynExp::Symbol(s)) => match resolve(s, env) {
                    Some(Binding::Import { .. }) => {
                        dbg!(&env);
                        import(self, l, env);
                        dbg!(&env);
                        None
                    }
                    _ => Some(ast::Item::Expr(self.expand_expr(SynExp::List(l), env))),
                },
                _ => Some(ast::Item::Expr(self.expand_expr(SynExp::List(l), env))),
            },
            SynExp::Symbol(_) | SynExp::Boolean(_) | SynExp::Char(_) => {
                Some(ast::Item::Expr(self.expand_expr(syn, env)))
            }
        }
    }

    fn expand_expr(&mut self, syn: SynExp, env: &Env<String, Binding>) -> ast::Expr {
        let span = syn.span();
        match syn {
            SynExp::Boolean(b) => ast::Expr {
                span,
                kind: ast::ExprKind::Boolean(b.value()),
            },
            SynExp::Char(c) => ast::Expr {
                span,
                kind: ast::ExprKind::Char(c.value()),
            },
            SynExp::Symbol(s) => match resolve(&s, env) {
                Some(Binding::Value {
                    orig_name, name, ..
                }) => ast::Expr {
                    span,
                    kind: ast::ExprKind::Var(ast::Ident {
                        span,
                        value: name.as_ref().unwrap_or(orig_name).to_string(),
                    }),
                },
                _ => {
                    self.emit_error(|b| {
                        b.span(span)
                            .msg(format!("undefined variable {}", s.value()))
                    });
                    ast::Expr {
                        span,
                        kind: ast::ExprKind::Var(ast::Ident {
                            span,
                            value: s.value().to_string(),
                        }),
                    }
                }
            },
            SynExp::List(l) => self.expand_list_expr(l, env),
        }
    }

    fn expand_syn(&mut self, syn: SynExp, env: &Env<String, Binding>) -> ItemSyn {
        let span = syn.span();
        if let SynExp::List(l) = syn.clone() {
            if let Some(head) = l.sexps().first() {
                match head {
                    SynExp::List(_) => todo!(),
                    SynExp::Symbol(s) => {
                        if let Some(Binding::SyntaxTransformer { transformer, .. }) =
                            resolve(s, env)
                        {
                            return match transformer(l) {
                                Ok(res) => ItemSyn::Syn(res),
                                Err(d) => {
                                    self.diagnostics.extend(d);
                                    ItemSyn::Expr(
                                        ast::Expr {
                                            span,
                                            kind: ast::ExprKind::List(vec![]),
                                        }
                                        .into_error(),
                                    )
                                }
                            };
                        }
                    }
                    SynExp::Boolean(_) | SynExp::Char(_) => {}
                }
            }
        }
        ItemSyn::Expr(self.expand_expr(syn, env))
    }

    fn expand_list_expr(&mut self, syn: SynList, env: &Env<String, Binding>) -> ast::Expr {
        let mut elems = syn.sexps().iter();
        match elems.next() {
            Some(head) => match head {
                SynExp::List(_) => match self.expand_syn(head.to_owned(), env) {
                    ItemSyn::Expr(e) => {
                        let mut rest_e = self.expand_exprs_iter(elems, env);
                        rest_e.insert(0, e);
                        ast::Expr {
                            span: syn.span(),
                            kind: ast::ExprKind::List(rest_e),
                        }
                    }
                    ItemSyn::Syn(_) => todo!(),
                },
                SynExp::Symbol(s) => match resolve(s, env) {
                    res @ (Some(Binding::Value { .. }) | None) => {
                        let e = self.expand_exprs(syn.sexps(), syn.span(), env);
                        if res.is_some() {
                            e
                        } else {
                            e.into_error()
                        }
                    }
                    Some(Binding::CoreExprTransformer { transformer, .. }) => {
                        transformer(self, syn, env)
                    }
                    Some(Binding::SyntaxTransformer { transformer, .. }) => {
                        let macro_scope = Scope::new();
                        match transformer(syn.with_scope(macro_scope)) {
                            Ok(mut e) => {
                                e.flip_scope(macro_scope);
                                self.expand_expr(e, env)
                            }
                            Err(d) => {
                                self.diagnostics.extend(d);
                                ast::Expr {
                                    span: syn.span(),
                                    kind: ast::ExprKind::List(vec![]),
                                }
                            }
                        }
                    }
                    r => todo!("{r:?}"),
                },
                SynExp::Boolean(_) | SynExp::Char(_) => {
                    self.emit_error(|b| {
                        b.msg(format!(
                            "tried to apply non-procedure {}",
                            head.red().green()
                        ))
                    });
                    let exprs = self.expand_exprs(syn.sexps(), syn.span(), env);
                    exprs.into_error()
                }
            },
            None => {
                self.emit_error(|b| {
                    b.msg("empty lists must be quoted")
                        .sources(vec![Diagnostic::builder().hint().msg("try '()").finish()])
                });
                ast::Expr {
                    span: syn.span(),
                    kind: ast::ExprKind::Quote(Box::new(ast::Expr {
                        span: syn.span(),
                        kind: ast::ExprKind::List(vec![]),
                    })),
                }
            }
        }
    }

    fn expand_exprs(
        &mut self,
        sexps: &[SynExp],
        span: Span,
        env: &Env<String, Binding>,
    ) -> ast::Expr {
        let mut exprs = vec![];
        for e in sexps {
            exprs.push(self.expand_expr(e.to_owned(), env));
        }
        ast::Expr {
            span,
            kind: ast::ExprKind::List(exprs),
        }
    }

    fn expand_exprs_iter(
        &mut self,
        sexps: slice::Iter<SynExp>,
        env: &Env<String, Binding>,
    ) -> Vec<ast::Expr> {
        let mut exprs = vec![];
        for e in sexps {
            exprs.push(self.expand_expr(e.to_owned(), env));
        }
        exprs
    }

    fn emit_error(&mut self, builder: impl Fn(DiagnosticBuilder) -> DiagnosticBuilder) {
        self.diagnostics
            .push(builder(Diagnostic::builder()).finish());
    }
}

fn resolve<'env>(var: &SynSymbol, env: &'env Env<String, Binding>) -> Option<&'env Binding> {
    env.get_all(var.value())
        .into_iter()
        .fold((None, 0usize), |acc, cur| {
            match cur.scopes().subset(var.scopes()) {
                Some(s) if s.get() >= acc.1 => (Some(cur), s.get()),
                _ => acc,
            }
        })
        .0
}

#[allow(dead_code)]
fn green_list(children: Vec<Rc<GreenTree>>) -> Rc<GreenTree> {
    let mut out = vec![green_open()];
    let len = children.len();
    for (i, c) in children.into_iter().enumerate() {
        out.push(c);
        if i + 1 < len {
            out.push(green_space());
        }
    }
    out.push(green_close());
    Rc::new(GreenTree::node(SyntaxKind::List, out))
}

#[allow(dead_code)]
fn syn_list(children: Vec<Rc<GreenTree>>) -> Rc<GreenTree> {
    Rc::new(GreenTree::node(SyntaxKind::List, children))
}

fn green_atom(atom: Rc<GreenTree>) -> Rc<GreenTree> {
    Rc::new(GreenTree::node(SyntaxKind::Atom, vec![atom]))
}

fn green_open() -> Rc<GreenTree> {
    green_atom(Rc::new(GreenTree::token(SyntaxKind::OpenDelim, "(".into())))
}

fn green_close() -> Rc<GreenTree> {
    green_atom(Rc::new(GreenTree::token(SyntaxKind::OpenDelim, ")".into())))
}

#[allow(dead_code)]
fn green_ident(id: impl Into<String>) -> Rc<GreenTree> {
    green_atom(Rc::new(GreenTree::token(SyntaxKind::Identifier, id.into())))
}

#[allow(dead_code)]
fn first_child(green: &Rc<GreenTree>) -> Rc<GreenTree> {
    match &green.as_ref() {
        GreenTree::Node(n) => Rc::clone(n.children().first().expect("expected a child")),
        GreenTree::Token(_) => panic!("expected node"),
    }
}

fn green_space() -> Rc<GreenTree> {
    Rc::new(GreenTree::token(SyntaxKind::Whitespace, " ".into()))
}

#[allow(dead_code)]
fn green_true() -> Rc<GreenTree> {
    green_atom(Rc::new(GreenTree::token(
        SyntaxKind::True,
        "#t".to_string(),
    )))
}

#[allow(dead_code)]
fn let_transformer(syn: SynList) -> Result<SynExp, Vec<Diagnostic>> {
    let sexps_len = syn.sexps().len();
    let (sexps, _) = syn.into_parts();
    let mut l = sexps.into_iter();
    assert!(l.next().is_some());
    let binssyn = l.next().and_then(|s| s.into_list().ok()).ok_or_else(|| {
        vec![Diagnostic::builder()
            .msg("expected a list of bindings")
            .finish()]
    })?;
    let (sexps, _) = binssyn.into_parts();
    let bindings = sexps
        .into_iter()
        .map(|s| {
            s.into_list()
                .map(|l| {
                    let mut ls = l.into_parts().0.into_iter();
                    let b = (
                        ls.next().unwrap().into_symbol().unwrap(),
                        ls.next().unwrap(),
                    );
                    assert!(ls.next().is_none());
                    b
                })
                .map_err(|_| {
                    vec![Diagnostic::builder()
                        .msg("expected a list of variable expression")
                        .finish()]
                })
        })
        .collect::<Result<Vec<_>, Vec<_>>>()?;
    assert!(sexps_len >= 3);

    let λ = green_ident("lambda");
    let bindings_syn = green_list(
        bindings
            .iter()
            .map(|(b, _)| b.red().green().clone())
            .collect(),
    );
    let mut lambda_out_syn = vec![
        SynSymbol::raw(
            &RedTree::new(&first_child(&λ)),
            Scopes::core(),
            FileId::default(),
        )
        .into(),
        SynList::raw(
            &RedTree::new(&bindings_syn),
            bindings.iter().map(|(b, _)| (*b).clone().into()).collect(),
            None,
            FileId::default(),
        )
        .into(),
    ];
    let mut lambda_out = vec![λ.clone(), bindings_syn.clone()];

    for s in l {
        lambda_out.push(s.red().green().clone());
        lambda_out_syn.push(s.clone());
    }

    let lambda_out = green_list(lambda_out);
    let mut app_out_syn = vec![SynList::raw(
        &RedTree::new(&lambda_out),
        lambda_out_syn,
        None,
        FileId::default(),
    )
    .into()];
    let mut app_out = vec![lambda_out.clone()];
    for (_, e) in &bindings {
        app_out.push(e.red().green().clone());
        app_out_syn.push((*e).clone());
    }

    Ok(SynList::raw(
        &RedTree::new(&green_list(app_out)),
        app_out_syn,
        None,
        FileId::default(),
    )
    .into())
}

#[allow(dead_code)]
fn or_transformer(syn: SynList) -> Result<SynExp, Vec<Diagnostic>> {
    let sexps_len = syn.sexps().len();
    let mut sexps = syn.sexps().iter();
    assert!(sexps.next().is_some());
    match sexps.next() {
        None => Ok(SynExp::cast(&RedTree::new(&green_true()), FileId::default()).unwrap()),
        Some(e) if sexps_len == 2 => Ok(e.clone()),
        Some(e) => {
            let let_ = green_ident("let");
            let let_syn = SynSymbol::raw(
                &RedTree::new(&first_child(&let_)),
                Scopes::core(),
                FileId::default(),
            )
            .into();
            let x = green_ident("x");
            let x_syn: SynExp = SynSymbol::raw(
                &RedTree::new(&first_child(&x)),
                Scopes::core(),
                FileId::default(),
            )
            .into();
            let if_ = green_ident("if");
            let if_syn = SynSymbol::raw(
                &RedTree::new(&first_child(&if_)),
                Scopes::core(),
                FileId::default(),
            )
            .into();
            let or = green_ident("or");
            let or_syn = SynSymbol::raw(
                &RedTree::new(&first_child(&or)),
                Scopes::core(),
                FileId::default(),
            )
            .into();
            let binding = green_list(vec![x.clone(), e.red().green().clone()]);
            let bindings = green_list(vec![binding.clone()]);
            let bindings_syn = SynList::raw(
                &RedTree::new(&bindings),
                vec![SynList::raw(
                    &RedTree::new(&binding),
                    vec![x_syn.clone(), e.clone()],
                    None,
                    FileId::default(),
                )
                .into()],
                None,
                FileId::default(),
            )
            .into();

            let mut or_l = vec![or];
            let mut or_syn = vec![or_syn];

            for rest in sexps {
                or_l.push(rest.red().green().clone());
                or_syn.push(rest.clone());
            }

            let or_l = green_list(or_l);
            let if_l = green_list(vec![if_, x.clone(), x, or_l.clone()]);
            let if_syn = SynList::raw(
                &RedTree::new(&if_l),
                vec![
                    if_syn,
                    x_syn.clone(),
                    x_syn,
                    SynList::raw(&RedTree::new(&or_l), or_syn, None, FileId::default()).into(),
                ],
                None,
                FileId::default(),
            )
            .into();
            let out = vec![let_, bindings, if_l];
            let out_syn = vec![let_syn, bindings_syn, if_syn];

            Ok(SynList::raw(
                &RedTree::new(&green_list(out)),
                out_syn,
                None,
                FileId::default(),
            )
            .into())
        }
    }
}

thread_local! {
    static IDENTIFIER_ID: Cell<u64> = Cell::new(1);
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use crate::syntax::{cst::SynRoot, parser::parse_str};

    use super::{
        transformers::{if_core_transformer, lambda_core_transformer},
        *,
    };

    fn intrinsics_env() -> Env<'static, String, Binding> {
        let mut intrinsics = Env::new();
        intrinsics.insert(
            String::from("import"),
            Binding::Import {
                scopes: Scopes::core(),
            },
        );
        intrinsics.enter_consume()
    }

    fn core_env() -> Env<'static, String, Binding> {
        let mut core = Env::new();
        core.insert(
            String::from("lambda"),
            Binding::CoreExprTransformer {
                scopes: Scopes::core(),
                name: String::from("lambda"),
                transformer: lambda_core_transformer,
            },
        );
        core.insert(
            String::from("if"),
            Binding::CoreExprTransformer {
                scopes: Scopes::core(),
                name: String::from("if"),
                transformer: if_core_transformer,
            },
        );
        core.insert(
            String::from("cons"),
            Binding::Value {
                scopes: Scopes::core(),
                orig_name: String::from("cons"),
                name: None,
            },
        );
        core
    }

    fn root_env() -> Env<'static, String, Binding> {
        let mut root = core_env().enter_consume();
        root.insert(
            String::from("let"),
            Binding::SyntaxTransformer {
                scopes: Scopes::core(),
                name: String::from("let"),
                transformer: let_transformer,
            },
        );
        root.insert(
            String::from("or"),
            Binding::SyntaxTransformer {
                scopes: Scopes::core(),
                name: String::from("or"),
                transformer: or_transformer,
            },
        );
        root.enter_consume()
    }

    fn check(input: &str, expected: Expect) {
        let res = parse_str(input);
        assert_eq!(res.diagnostics, vec![]);
        let red = SynRoot::new(&res.tree, res.file_id);
        let mut children = red.syn_children();
        let env = root_env();
        let mut expander = Expander {
            import: &|_| panic!(),
            diagnostics: vec![],
        };
        let ast = expander.expand_expr(children.next().expect("expected an item"), &env);
        assert_eq!(expander.diagnostics, vec![]);
        expected.assert_debug_eq(&ast);
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
                {var |cons| 0:0..4}
            "#]],
        );
    }

    #[test]
    fn function_application() {
        check(
            r"(cons #t #f)",
            expect![[r#"
                {list 0:0..12
                  {var |cons| 0:1..4}
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
                    {var |x 1| 0:13..1}}
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
                    {list 0:13..18
                      {λ 0:14..14
                        ({|y 3| 0:23..1})
                        #f
                        {var |y 3| 0:26..1}}
                      {var |x 2| 0:29..1}}}
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

    #[test]
    fn lambda() {
        check(
            "(lambda x x)",
            expect![[r#"
                {λ 0:0..12
                  ()
                  {|x 1| 0:8..1}
                  {var |x 1| 0:10..1}}
            "#]],
        );
    }

    #[test]
    fn expand_let() {
        check(
            "(let ([x #t]) x)",
            expect![[r#"
                {list 0:0..19
                  {λ 0:0..14
                    ({|x 1| 0:7..1})
                    #f
                    {var |x 1| 0:14..1}}
                  {#t 0:9..2}}
            "#]],
        );
    }

    #[test]
    fn expand_or() {
        check(
            "(or)",
            expect![[r#"
                {#t 0:0..2}
            "#]],
        );
        check(
            "(or #f)",
            expect![[r#"
                {#f 0:4..2}
            "#]],
        );
        check(
            r"(or #\a #\b)",
            expect![[r#"
                {list 0:0..36
                  {λ 0:0..30
                    ({|x 1| 0:0..1})
                    #f
                    {if 0:0..17
                      {var |x 1| 0:0..1}
                      {var |x 1| 0:0..1}
                      {#\b 0:8..3}}}
                  {#\a 0:4..3}}
            "#]],
        );
        check(
            r"(lambda (x y) (or y x))",
            expect![[r#"
                {λ 0:0..23
                  ({|x 2| 0:9..1} {|y 3| 0:11..1})
                  #f
                  {list 0:0..32
                    {λ 0:0..28
                      ({|x 4| 0:0..1})
                      #f
                      {if 0:0..15
                        {var |x 4| 0:0..1}
                        {var |x 4| 0:0..1}
                        {var |x 2| 0:20..1}}}
                    {var |y 3| 0:18..1}}}
            "#]],
        );
    }

    #[test]
    fn let_x() {
        let res = parse_str("(let ([x #t]) x)");
        assert_eq!(res.diagnostics, vec![]);
        let red = RedTree::new(&res.tree);
        let root = SynRoot::new_red(&red, res.file_id);
        let children = root.children();
        let or = SynExp::cast(&children[0], res.file_id).unwrap();
        let out = let_transformer(or.into_list().unwrap()).unwrap();
        assert_eq!(out.to_string(), "((lambda (x) x) #t)");
        assert_eq!(out.into_list().unwrap().syn_string(), "((lambda (x) x) #t)");
    }

    #[test]
    fn or() {
        let res = parse_str("(or)");
        assert_eq!(res.diagnostics, vec![]);
        let red = RedTree::new(&res.tree);
        let root = SynRoot::new_red(&red, res.file_id);
        let children = root.children();
        let or = dbg!(SynExp::cast(&children[0], res.file_id).unwrap());
        let out = or_transformer(or.into_list().unwrap()).unwrap();
        assert_eq!(out.to_string(), "#t");
        assert_eq!(out.syn_string(), "#t");
    }

    #[test]
    fn or_e() {
        let res = parse_str("(or #f)");
        assert_eq!(res.diagnostics, vec![]);
        let red = RedTree::new(&res.tree);
        let root = SynRoot::new_red(&red, res.file_id);
        let children = root.children();
        let or = dbg!(SynExp::cast(&children[0], res.file_id).unwrap());
        let out = or_transformer(or.into_list().unwrap()).unwrap();
        assert_eq!(out.to_string(), "#f");
        assert_eq!(out.syn_string(), "#f");
    }

    #[test]
    fn or_e0_e() {
        let res = parse_str(r"(or #\0 #\1)");
        assert_eq!(res.diagnostics, vec![]);
        let red = RedTree::new(&res.tree);
        let root = SynRoot::new_red(&red, res.file_id);
        let children = root.children();
        let or = dbg!(SynExp::cast(&children[0], res.file_id).unwrap());
        let out = or_transformer(or.into_list().unwrap()).unwrap();
        assert_eq!(out.to_string(), r"(let ((x #\0)) (if x x (or #\1)))");
        assert_eq!(out.syn_string(), r"(let ((x #\0)) (if x x (or #\1)))");
    }

    mod modules {
        use super::*;

        fn importer(module_name: ModuleName) -> Result<Rc<ModuleInterface>, Diagnostic> {
            assert_eq!("(cafe expander intrinsics ())", module_name.to_string());

            Ok(Rc::new(ModuleInterface {
                span: Span::dummy(),
                name: ModuleName {
                    paths: vec!["cafe".into(), "intrinsics".into(), "expander".into()],
                    versions: vec![],
                },
                bindings: core_env(),
            }))
        }

        fn check(input: &str, expected: Expect) {
            let res = parse_str(input);
            assert_eq!(res.diagnostics, vec![]);
            let syn = SynRoot::new(&res.tree, res.file_id);
            let env = intrinsics_env();
            let res = expand_root(syn, &env, importer);
            assert_eq!(res.diagnostics, vec![]);
            expected.assert_debug_eq(&res.module);
        }

        #[test]
        fn define_and_use_variable() {
            check(
                r"(import (cafe expander intrinsics ()))
                  (define id (lambda (x) x))
                  (id #t)",
                expect![[r#"
                    mod@0:0..24
                      {#t 0:0..2}
                      {#\a 0:21..3}
                "#]],
            );
        }

        #[test]
        fn multiple_expressions() {
            check(
                r"#t
                  #\a",
                expect![[r#"
                    mod@0:0..24
                      {#t 0:0..2}
                      {#\a 0:21..3}
                "#]],
            );
        }

        #[test]
        fn import_intrinsics() {
            check(
                "(import (cafe expander intrinsics ()))
                 (lambda (x) x)",
                expect![[r#"
                    mod@0:0..70
                      {λ 0:56..14
                        ({|x 1| 0:65..1})
                        #f
                        {var |x 1| 0:68..1}}
                "#]],
            );
            check(
                "(import (cafe expander intrinsics ()))
                 (if #t #f)",
                expect![[r#"
                    mod@0:0..66
                      {if 0:56..10
                        {#t 0:60..2}
                        {#f 0:63..2}
                        {void 0:56..10}}
                "#]],
            );
        }
    }
}
