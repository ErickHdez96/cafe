use std::{cell::Cell, fmt, rc::Rc, slice};

use crate::{
    diagnostics::{Diagnostic, DiagnosticBuilder},
    env::Env,
    expander::scopes::{Scope, Scopes},
    file::FileId,
    span::Span,
    syntax::{
        ast::{self, ModId, Module, ModuleInterface, ModuleName},
        cst::{GreenTree, RedTree, SynExp, SynList, SynRoot, SynSymbol},
        SyntaxKind,
    },
};

use self::{
    intrinsics::{import, module},
    macros::NativeSyntaxTransformer,
};

pub mod core;
pub mod intrinsics;
pub mod macros;
pub mod scopes;

type CoreDefTransformer = fn(&mut Expander, SynList, &Env<String, Binding>) -> Option<ast::Define>;
type CoreMacroDefTransformer = fn(&mut Expander, SynList, &mut Env<String, Binding>);
type CoreExprTransformer = fn(&mut Expander, SynList, &Env<String, Binding>) -> ast::Expr;
type SyntaxTransformer = fn(SynList) -> Result<SynExp, Vec<Diagnostic>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExpanderResult {
    pub module: Module,
    pub diagnostics: Vec<Diagnostic>,
}

pub fn core_expander_interface() -> ModuleInterface {
    let mut core = Env::new();
    core.insert(
        String::from("define"),
        Binding::CoreDefTransformer {
            scopes: Scopes::core(),
            name: String::from("define"),
            transformer: core::define_transformer,
        },
    );
    core.insert(
        String::from("define-syntax"),
        Binding::CoreMacroDefTransformer {
            scopes: Scopes::core(),
            name: String::from("define-syntax"),
            transformer: core::define_syntax_transformer,
        },
    );
    core.insert(
        String::from("lambda"),
        Binding::CoreExprTransformer {
            scopes: Scopes::core(),
            name: String::from("lambda"),
            transformer: core::lambda_transformer,
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
    core.insert(
        String::from("quote"),
        Binding::CoreExprTransformer {
            scopes: Scopes::core(),
            name: String::from("quote"),
            transformer: core::quote_transformer,
        },
    );
    ModuleInterface {
        id: ModuleName::from_strings(vec!["rnrs", "expander", "core"]),
        span: Span::dummy(),
        bindings: core,
        dependencies: vec![],
        types: None,
    }
}

pub struct Expander<'i> {
    import: &'i dyn Fn(ModId) -> Result<Rc<ModuleInterface>, Diagnostic>,
    register: &'i dyn Fn(ModId, Module),
    diagnostics: Vec<Diagnostic>,
    module: ModId,
    dependencies: Vec<ModId>,
    module_stack: Vec<(ModId, Vec<ModId>)>,
}

impl fmt::Debug for Expander<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExpanderResult")
            .field("import", &())
            .field("diagnostics", &self.diagnostics)
            .finish()
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
        orig_module: ModId,
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
        scopes: Scopes,
        name: String,
        transformer: SyntaxTransformer,
    },
    /// Temporary transformer returned from syntax-rules. It should be moved to a normal
    /// `SyntaxTransformer.
    NativeSyntaxTransformer {
        scopes: Scopes,
        name: String,
        transformer: Rc<NativeSyntaxTransformer>,
    },
}

impl Binding {
    /// Returns a new [`Binding::Value`] [`Binding`].
    pub fn new_var(name: &str, orig_module: ModId, scopes: Scopes) -> Self {
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
            | Binding::SyntaxTransformer { name, .. }
            | Binding::NativeSyntaxTransformer { name, .. } => name,
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
            | Binding::SyntaxTransformer { scopes, .. }
            | Binding::NativeSyntaxTransformer { scopes, .. } => scopes,
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
            | Binding::SyntaxTransformer { ref mut scopes, .. }
            | Binding::NativeSyntaxTransformer { ref mut scopes, .. } => scopes,
        }
    }
}

pub fn expand_root(
    syn: SynRoot,
    base_env: &Env<'_, String, Binding>,
    import: impl Fn(ModId) -> Result<Rc<ModuleInterface>, Diagnostic>,
    register: impl Fn(ModId, Module),
) -> ExpanderResult {
    let mut expander = Expander {
        import: &import,
        register: &register,
        module: ModuleName::script(),
        module_stack: vec![],
        dependencies: vec![],
        diagnostics: vec![],
    };

    let mid = ModuleName::from_strings(vec!["rnrs", "expander", "intrinsics"]);
    (expander.register)(
        mid,
        Module {
            span: Span::dummy(),
            id: mid,
            dependencies: vec![],
            exports: intrinsics_env(),
            bindings: Env::default(),
            body: ast::Expr {
                span: Span::dummy(),
                kind: ast::ExprKind::LetRec {
                    defs: vec![],
                    exprs: vec![ast::Expr {
                        span: Span::dummy(),
                        kind: ast::ExprKind::Void,
                    }],
                },
            },
            types: None,
        },
    );

    expander.expand_root(syn, base_env)
}

fn intrinsics_env() -> Env<'static, String, Binding> {
    let mut env = Env::default();
    env.insert(
        String::from("import"),
        Binding::Import {
            scopes: Scopes::core(),
        },
    );
    env.insert(
        String::from("module"),
        Binding::Module {
            scopes: Scopes::core(),
        },
    );
    env
}

impl Expander<'_> {
    fn expand_root(&mut self, syn: SynRoot, base_env: &Env<'_, String, Binding>) -> ExpanderResult {
        let root_scope = Scope::new();
        let mut bindings = base_env.enter();
        let mut deferred = vec![];
        let intrinsics = (self.import)(ModuleName::from_strings(vec![
            "rnrs",
            "expander",
            "intrinsics",
        ]))
        .unwrap();
        for (v, b) in intrinsics.bindings.bindings() {
            bindings.insert(v.clone(), b.clone());
        }

        for i in syn.syn_children_with_scope(root_scope) {
            if let Some(def) = self.expand_macro(i, &mut bindings) {
                deferred.push(def);
            }
        }

        let mut items = vec![];

        for d in deferred {
            if let Some(item) = self.expand_item(d, &bindings) {
                items.push(item);
            }
        }

        ExpanderResult {
            module: Module {
                id: ModuleName::script(),
                span: syn.span(),
                exports: Env::default(),
                bindings: Env::with_bindings(bindings.into_bindings()),
                dependencies: std::mem::take(&mut self.dependencies),
                body: items_to_letrec(items, syn.span()),
                types: None,
            },
            diagnostics: std::mem::take(&mut self.diagnostics),
        }
    }

    fn expand_macro(&mut self, syn: SynExp, env: &mut Env<String, Binding>) -> Option<SynExp> {
        let SynExp::List(l) = syn else {
            return Some(syn);
        };

        if let Some(SynExp::Symbol(s)) = l.sexps().first() {
            match resolve(s, env) {
                Some(Binding::NativeSyntaxTransformer { transformer, .. }) => {
                    let s = transformer.expand(self, l, env)?;
                    return self.expand_macro(s, env);
                }
                Some(Binding::Import { .. }) => {
                    import(self, l, env);
                    return None;
                }
                Some(Binding::Module { .. }) => {
                    module(self, l);
                    return None;
                }
                Some(Binding::CoreDefTransformer { scopes, .. }) => {
                    let mut sexps = l.sexps().iter();
                    if let Some(s) = sexps.nth(1).and_then(SynExp::symbol) {
                        env.insert(
                            s.value().to_string(),
                            Binding::Value {
                                scopes: scopes.clone(),
                                orig_module: self.current_module(),
                                orig_name: s.value().to_string(),
                                name: Some(s.value().to_string()),
                            },
                        );
                    }
                }
                Some(Binding::CoreMacroDefTransformer { transformer, .. }) => {
                    transformer(self, l, env);
                    return None;
                }
                _ => {}
            }
        }
        Some(SynExp::List(l))
    }

    fn expand_item(&mut self, syn: SynExp, env: &Env<String, Binding>) -> Option<Item> {
        match syn {
            SynExp::List(l) => match l.sexps().iter().next() {
                Some(SynExp::Symbol(s)) => match resolve(s, env) {
                    Some(Binding::Import { .. }) => {
                        panic!("import should have been removed in expand_macro")
                    }
                    Some(Binding::CoreDefTransformer { transformer, .. }) => {
                        transformer(self, l, env).map(Item::Define)
                    }
                    _ => Some(Item::Expr(self.expand_expr(SynExp::List(l), env))),
                },
                _ => Some(Item::Expr(self.expand_expr(SynExp::List(l), env))),
            },
            SynExp::Symbol(_) | SynExp::Boolean(_) | SynExp::Char(_) => {
                Some(Item::Expr(self.expand_expr(syn, env)))
            }
        }
    }

    fn expand_expr(&mut self, syn: SynExp, env: &Env<String, Binding>) -> ast::Expr {
        let span = syn.source_span();
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
                    orig_name,
                    name,
                    orig_module,
                    ..
                }) => ast::Expr {
                    span,
                    kind: ast::ExprKind::Var(ast::Path {
                        span,
                        module: *orig_module,
                        value: name.as_ref().unwrap_or(orig_name).to_string(),
                    }),
                },
                _ => {
                    self.emit_error(|b| {
                        b.span(span)
                            .msg(format!("undefined variable `{}`", s.value()))
                    });
                    ast::Expr {
                        span,
                        kind: ast::ExprKind::Var(ast::Path {
                            span,
                            module: self.current_module(),
                            value: s.value().to_string(),
                        }),
                    }
                }
            },
            SynExp::List(l) => self.expand_list_expr(l, env),
        }
    }

    fn expand_syn(&mut self, syn: SynExp, env: &Env<String, Binding>) -> ItemSyn {
        let span = syn.source_span();
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
                            span: syn.source_span(),
                            kind: ast::ExprKind::List(rest_e),
                        }
                    }
                    ItemSyn::Syn(_) => todo!(),
                },
                SynExp::Symbol(s) => match resolve(s, env) {
                    res @ (Some(Binding::Value { .. }) | None) => {
                        let e = self.expand_exprs(syn.sexps(), syn.source_span(), env);
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
                                    span: syn.source_span(),
                                    kind: ast::ExprKind::List(vec![]),
                                }
                            }
                        }
                    }
                    Some(Binding::NativeSyntaxTransformer { transformer, .. }) => {
                        let macro_scope = Scope::new();
                        match transformer.expand(self, syn.with_scope(macro_scope), env) {
                            Some(mut e) => {
                                e.flip_scope(macro_scope);
                                self.expand_expr(e, env)
                            }
                            None => ast::Expr {
                                span: syn.source_span(),
                                kind: ast::ExprKind::List(vec![]),
                            },
                        }
                    }
                    r => todo!("{r:?}"),
                },
                SynExp::Boolean(_) | SynExp::Char(_) => {
                    self.emit_error(|b| {
                        b.span(head.source_span()).msg(format!(
                            "tried to apply non-procedure `{}`",
                            head.red().green()
                        ))
                    });
                    let exprs = self.expand_exprs(syn.sexps(), syn.source_span(), env);
                    exprs.into_error()
                }
            },
            None => {
                if syn.has_close_delim() {
                    self.emit_error(|b| {
                        b.msg("empty lists must be quoted")
                            .span(syn.source_span())
                            .related(vec![Diagnostic::builder().hint().msg("try '()").finish()])
                    });
                }
                ast::Expr {
                    span: syn.source_span(),
                    kind: ast::ExprKind::List(vec![]),
                }
                .into_quote()
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

    fn import(&mut self, mid: ModId) -> Result<Rc<ModuleInterface>, Diagnostic> {
        self.dependencies.push(mid);
        (self.import)(mid)
    }

    const fn current_module(&self) -> ModId {
        self.module
    }

    fn enter_module(&mut self, module: ModId) {
        self.module_stack.push((
            std::mem::replace(&mut self.module, module),
            std::mem::take(&mut self.dependencies),
        ));
    }

    fn exit_module(&mut self) {
        (self.module, self.dependencies) = self.module_stack.pop().unwrap();
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

enum ItemSyn {
    Expr(ast::Expr),
    Syn(SynExp),
}

#[derive(Debug)]
enum Item {
    Define(ast::Define),
    Expr(ast::Expr),
}

fn items_to_letrec(mut items: Vec<Item>, span: Span) -> ast::Expr {
    let mut defs = vec![];
    let mut exprs = vec![];
    let last_def = items
        .iter()
        .rposition(|i| matches!(i, Item::Define(_)))
        .map(|p| p + 1)
        .unwrap_or_default();
    let expr_items = items.split_off(last_def);

    for item in items {
        match item {
            Item::Define(d) => {
                defs.push(d);
            }
            Item::Expr(e) => {
                let span = e.span;
                defs.push(ast::Define {
                    span,
                    name: ast::Ident {
                        span,
                        value: format!("#generated {}", Binding::new_id()),
                    },
                    expr: Some(ast::Expr {
                        span,
                        kind: ast::ExprKind::Begin(vec![
                            e,
                            ast::Expr {
                                span,
                                kind: ast::ExprKind::Void,
                            },
                        ]),
                    }),
                });
            }
        }
    }

    for item in expr_items {
        match item {
            Item::Define(d) => unreachable!("should have split off the slice here: {d:?}"),
            Item::Expr(e) => {
                exprs.push(e);
            }
        }
    }

    if exprs.is_empty() {
        exprs.push(ast::Expr {
            span,
            kind: ast::ExprKind::Void,
        });
    }

    ast::Expr {
        span,
        kind: ast::ExprKind::LetRec { defs, exprs },
    }
}

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

fn green_atom(atom: Rc<GreenTree>) -> Rc<GreenTree> {
    Rc::new(GreenTree::node(SyntaxKind::Atom, vec![atom]))
}

fn green_open() -> Rc<GreenTree> {
    Rc::new(GreenTree::token(SyntaxKind::OpenDelim, "(".into()))
}

fn green_close() -> Rc<GreenTree> {
    Rc::new(GreenTree::token(SyntaxKind::OpenDelim, ")".into()))
}

fn green_ident(id: impl Into<String>) -> Rc<GreenTree> {
    green_atom(Rc::new(GreenTree::token(SyntaxKind::Identifier, id.into())))
}

fn first_child(green: &Rc<GreenTree>) -> Rc<GreenTree> {
    match &green.as_ref() {
        GreenTree::Node(n) => Rc::clone(n.children().first().expect("expected a child")),
        GreenTree::Token(_) => panic!("expected node"),
    }
}

fn green_space() -> Rc<GreenTree> {
    Rc::new(GreenTree::token(SyntaxKind::Whitespace, " ".into()))
}

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
            None,
        )
        .into(),
        SynList::raw(
            &RedTree::new(&bindings_syn),
            bindings.iter().map(|(b, _)| (*b).clone().into()).collect(),
            None,
            FileId::default(),
            None,
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
        None,
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
        None,
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
                None,
            )
            .into();
            let x = green_ident("x");
            let x_syn: SynExp = SynSymbol::raw(
                &RedTree::new(&first_child(&x)),
                Scopes::core(),
                FileId::default(),
                None,
            )
            .into();
            let if_ = green_ident("if");
            let if_syn = SynSymbol::raw(
                &RedTree::new(&first_child(&if_)),
                Scopes::core(),
                FileId::default(),
                None,
            )
            .into();
            let or = green_ident("or");
            let or_syn = SynSymbol::raw(
                &RedTree::new(&first_child(&or)),
                Scopes::core(),
                FileId::default(),
                None,
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
                    None,
                )
                .into()],
                None,
                FileId::default(),
                None,
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
                    SynList::raw(&RedTree::new(&or_l), or_syn, None, FileId::default(), None)
                        .into(),
                ],
                None,
                FileId::default(),
                None,
            )
            .into();
            let out = vec![let_, bindings, if_l];
            let out_syn = vec![let_syn, bindings_syn, if_syn];

            Ok(SynList::raw(
                &RedTree::new(&green_list(out)),
                out_syn,
                None,
                FileId::default(),
                None,
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
    use std::{cell::RefCell, collections::HashMap};

    use expect_test::{expect, Expect};

    use crate::{
        syntax::{cst::SynRoot, parser::parse_str},
        utils::Resolve,
    };

    use super::*;

    struct Libs {
        libs: RefCell<HashMap<ModId, Module>>,
    }

    impl Default for Libs {
        fn default() -> Self {
            let mid = ModuleName::from_strings(vec!["rnrs", "expander", "core"]);
            Self {
                libs: RefCell::new(HashMap::from([(
                    mid,
                    Module {
                        id: mid,
                        span: Span::dummy(),
                        dependencies: vec![],
                        exports: core_env(),
                        bindings: Env::default(),
                        body: ast::Expr {
                            span: Span::dummy(),
                            kind: ast::ExprKind::LetRec {
                                defs: vec![],
                                exprs: vec![ast::Expr {
                                    span: Span::dummy(),
                                    kind: ast::ExprKind::Void,
                                }],
                            },
                        },
                        types: None,
                    },
                )])),
            }
        }
    }

    impl Libs {
        fn import(&self, mid: ModId) -> Result<Rc<ModuleInterface>, Diagnostic> {
            Ok(Rc::new(
                self.libs
                    .borrow()
                    .get(&mid)
                    .expect(&format!("{}", mid.resolve()))
                    .to_interface(),
            ))
        }

        fn define(&self, mid: ModId, module: Module) {
            self.libs.borrow_mut().insert(mid, module);
        }
    }

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
        core.insert(
            String::from("define-syntax"),
            Binding::CoreMacroDefTransformer {
                scopes: Scopes::core(),
                name: String::from("define-syntax"),
                transformer: core::define_syntax_transformer,
            },
        );
        core.insert(
            String::from("lambda"),
            Binding::CoreExprTransformer {
                scopes: Scopes::core(),
                name: String::from("lambda"),
                transformer: core::lambda_transformer,
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
        core.insert(
            String::from("quote"),
            Binding::CoreExprTransformer {
                scopes: Scopes::core(),
                name: String::from("quote"),
                transformer: core::quote_transformer,
            },
        );
        core.insert(
            String::from("cons"),
            Binding::Value {
                scopes: Scopes::core(),
                orig_module: ModuleName::from_strings(vec!["rnrs", "expander", "core"]),
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
            register: &|_, _| panic!(),
            module: ModuleName::script(),
            module_stack: vec![],
            dependencies: vec![],
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
                {var |cons| (rnrs expander core ()) 0:0..4}
            "#]],
        );
    }

    #[test]
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

    #[test]
    fn lambda() {
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
    #[ignore]
    fn lambda_with_define() {
        check("(lambda x (define y x) y)", expect![[r#""#]]);
    }

    #[test]
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

    #[test]
    fn expand_let() {
        check(
            "(let ([x #t]) x)",
            expect![[r#"
                {list 0:0..19
                  {λ 0:0..14
                    ({|x 1| 0:7..1})
                    #f
                    {letrec 0:0..14
                      ()
                      {var |x 1| (#script ()) 0:14..1}}}
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
                    {letrec 0:0..30
                      ()
                      {if 0:0..17
                        {var |x 1| (#script ()) 0:0..1}
                        {var |x 1| (#script ()) 0:0..1}
                        {#\b 0:8..3}}}}
                  {#\a 0:4..3}}
            "#]],
        );
        check(
            r"(lambda (x y) (or y x))",
            expect![[r#"
                {λ 0:0..23
                  ({|x 2| 0:9..1} {|y 3| 0:11..1})
                  #f
                  {letrec 0:0..23
                    ()
                    {list 0:0..32
                      {λ 0:0..28
                        ({|x 4| 0:0..1})
                        #f
                        {letrec 0:0..28
                          ()
                          {if 0:0..15
                            {var |x 4| (#script ()) 0:0..1}
                            {var |x 4| (#script ()) 0:0..1}
                            {var |x 2| (#script ()) 0:20..1}}}}
                      {var |y 3| (#script ()) 0:18..1}}}}
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
        let or = SynExp::cast(&children[0], res.file_id).unwrap();
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
        let or = SynExp::cast(&children[0], res.file_id).unwrap();
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
        let or = SynExp::cast(&children[0], res.file_id).unwrap();
        let out = or_transformer(or.into_list().unwrap()).unwrap();
        assert_eq!(out.to_string(), r"(let ((x #\0)) (if x x (or #\1)))");
        assert_eq!(out.syn_string(), r"(let ((x #\0)) (if x x (or #\1)))");
    }

    mod modules {
        use crate::utils::Resolve;

        use super::*;

        fn check(input: &str, expected: Expect) -> ExpanderResult {
            let res = parse_str(input);
            assert_eq!(res.diagnostics, vec![]);
            let syn = SynRoot::new(&res.tree, res.file_id);
            let libs = Libs::default();
            let res = expand_root(
                syn,
                &Env::default(),
                |name| libs.import(name),
                |name, module| libs.define(name, module),
            );
            assert_eq!(res.diagnostics, vec![]);
            expected.assert_debug_eq(&res.module);
            res
        }

        #[test]
        fn define_and_use_variable() {
            check(
                r"(import (rnrs expander core ()))
                  (define id (lambda (x) x))
                  (id #t)",
                expect![[r#"
                    mod (#script ()) @0:0..103
                      {letrec 0:0..103
                        {define@0:51..26
                          {|id| 0:59..2}
                          {λ 0:62..14
                            ({|x 1| 0:71..1})
                            #f
                            {letrec 0:62..14
                              ()
                              {var |x 1| (#script ()) 0:74..1}}}}
                        {list 0:96..7
                          {var |id| (#script ()) 0:97..2}
                          {#t 0:100..2}}}
                "#]],
            );
        }

        #[test]
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
                    mod (#script ()) @0:0..183
                      {letrec 0:0..183
                        ()
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
                vec![ModuleName {
                    paths: vec![String::from("ID")],
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
                    mod (#script ()) @0:0..160
                      {letrec 0:0..160
                        ()
                        {λ 0:146..14
                          ({|x 1| 0:155..1})
                          #f
                          {letrec 0:146..14
                            ()
                            {var |x 1| (#script ()) 0:158..1}}}}
                "#]],
            );

            assert_eq!(
                res.module
                    .dependencies
                    .iter()
                    .map(|mid| mid.resolve().clone())
                    .collect::<Vec<_>>(),
                vec![ModuleName {
                    paths: vec![String::from("rnrs"), String::from("base")],
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
                    mod (#script ()) @0:0..24
                      {letrec 0:0..24
                        ()
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
                    mod (#script ()) @0:0..64
                      {letrec 0:0..64
                        ()
                        {λ 0:50..14
                          ({|x 1| 0:59..1})
                          #f
                          {letrec 0:50..14
                            ()
                            {var |x 1| (#script ()) 0:62..1}}}}
                "#]],
            );
            let res = check(
                "(import (rnrs expander core ()))
                 (if #t #f)",
                expect![[r#"
                    mod (#script ()) @0:0..60
                      {letrec 0:0..60
                        ()
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
                vec![ModuleName {
                    paths: vec![
                        String::from("rnrs"),
                        String::from("expander"),
                        String::from("core")
                    ],
                    versions: vec![],
                }]
            );
        }
    }
}
