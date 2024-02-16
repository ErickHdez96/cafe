use std::{cell::Cell, fmt, rc::Rc};

use crate::{
    diagnostics::Diagnostic,
    env::Env,
    file::FileId,
    new_syntax::{
        ast::{self, Item},
        cst::Cst,
    },
};

use self::scopes::Scopes;

mod core;
mod macros;
mod scopes;

type CoreMacroDefTransformer = fn(&mut Expander, Cst, &mut Env<String, Binding>);
type CoreDefTransformer = fn(&mut Expander, Cst, &Env<String, Binding>) -> Item;
type CoreExprTransformer = fn(&mut Expander, Cst, &Env<String, Binding>) -> Item;
type SyntaxTransformer = fn(Cst) -> Result<Cst, Vec<Diagnostic>>;

pub struct Expander<'i> {
    import: &'i dyn Fn(ast::ModId) -> Result<Rc<ast::ModuleInterface>, Diagnostic>,
    register: &'i dyn Fn(ast::ModId, ast::Module),
    diagnostics: Vec<Diagnostic>,
    module: ast::ModId,
    dependencies: Vec<ast::ModId>,
    module_stack: Vec<(ast::ModId, Vec<ast::ModId>)>,
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
    root: Vec<Cst>,
    base_env: &Env<'_, String, Binding>,
    import: impl Fn(ast::ModId) -> Result<Rc<ast::ModuleInterface>, Diagnostic>,
    register: impl Fn(ast::ModId, ast::Module),
) -> ExpanderResult {
    todo!();
    //let mut expander = Expander {
    //    import: &import,
    //    register: &register,
    //    module: ast::ModuleName::script(),
    //    module_stack: vec![],
    //    dependencies: vec![],
    //    diagnostics: vec![],
    //};

    //let mid = ast::ModuleName::from_strings(vec!["rnrs", "expander", "intrinsics"]);
    //(expander.register)(
    //    mid,
    //    ast::Module {
    //        mid,
    //        span: Span::dummy(),
    //        source: List::new_mod(
    //            Span::dummy(),
    //            Rc::new(GreenTree::node(SyntaxKind::Root, vec![])),
    //            mid,
    //            vec![],
    //        )
    //        .into(),
    //        //green: Rc::new(GreenTree::node(SyntaxKind::Root, vec![])),
    //        //children: vec![],
    //        dependencies: vec![],
    //        exports: intrinsics_env(),
    //        bindings: Env::default(),
    //        //types: None,
    //    },
    //);

    //expander.expand_root(syn, base_env)
}

//#[derive(Debug)]
//enum Expanded {
//    Syn(SynExp),
//    Binding(SynSymbol, Binding),
//    Item(Item),
//}
//
//impl Expander<'_> {
//    fn expand_root(&mut self, syn: SynRoot, base_env: &Env<'_, String, Binding>) -> ExpanderResult {
//        let root_scope = Scope::new();
//        let mut bindings = base_env.enter();
//        let mut deferred = vec![];
//        let intrinsics = (self.import)(ast::ModuleName::from_strings(vec![
//            "rnrs",
//            "expander",
//            "intrinsics",
//        ]))
//        .unwrap();
//        for (v, b) in intrinsics.bindings.bindings() {
//            bindings.insert(v.clone(), b.clone());
//        }
//
//        for i in syn.syn_children_with_scope(root_scope) {
//            match self.expand_syntax(i, &mut bindings, Ctx::Body) {
//                Expanded::Binding(syn, _) => deferred.push(Expanded::Syn(SynExp::Symbol(syn))),
//                e => deferred.push(e),
//            }
//            //if let Some(def) = self.expand_macro(i, &mut bindings) {
//            //    deferred.push(def);
//            //}
//        }
//
//        let mut items = vec![];
//
//        for d in deferred {
//            match d {
//                Expanded::Syn(s) => match self.expand_syntax(s, &mut bindings, Ctx::Defer) {
//                    Expanded::Syn(s) => panic!("expected a fully expanded item - {s:?}"),
//                    Expanded::Binding(_, b) => panic!("expected a fully expanded item - {b:?}"),
//                    Expanded::Item(i) => items.push(i),
//                },
//                Expanded::Binding(_, b) => panic!("expected a syntax element or an item - {b:?}"),
//                Expanded::Item(i) => items.push(i),
//            }
//            //if let Some(item) = self.expand_item(d, &bindings) {
//            //    items.push(item);
//            //}
//        }
//
//        ExpanderResult {
//            file_id: syn.file_id(),
//            items,
//            diagnostics: std::mem::take(&mut self.diagnostics),
//        }
//    }
//
//    fn expand_syntax(&mut self, syn: SynExp, env: &mut Env<String, Binding>, ctx: Ctx) -> Expanded {
//        let span = syn.source_span();
//        let green = Rc::clone(syn.red().green());
//        match syn {
//            SynExp::List(l) => self.expand_list_syntax(l, env, ctx),
//            SynExp::Symbol(s) => match ctx {
//                Ctx::Body => match resolve(&s, &env) {
//                    Some(b) => Expanded::Binding(s, b.clone()),
//                    None => Expanded::Syn(SynExp::Symbol(s)),
//                },
//                Ctx::Defer | Ctx::Expr => match resolve(&s, &env) {
//                    Some(Binding::Value {
//                        orig_module,
//                        orig_name,
//                        name,
//                        ..
//                    }) => Expanded::Item(
//                        Atom::new_var(
//                            span,
//                            green,
//                            *orig_module,
//                            name.as_ref().unwrap_or(orig_name).to_string(),
//                        )
//                        .into(),
//                    ),
//                    _ => todo!(),
//                },
//            },
//            SynExp::Boolean(_) => match ctx {
//                Ctx::Body => Expanded::Syn(syn),
//                Ctx::Defer | Ctx::Expr => Expanded::Item(Atom::new_boolean(span, green).into()),
//            },
//            SynExp::Char(_) => match ctx {
//                Ctx::Body => Expanded::Syn(syn),
//                Ctx::Defer | Ctx::Expr => Expanded::Item(Atom::new_char(span, green).into()),
//            },
//        }
//    }
//
//    fn expand_list_syntax(
//        &mut self,
//        syn: SynList,
//        env: &mut Env<String, Binding>,
//        ctx: Ctx,
//    ) -> Expanded {
//        let mut elems = syn.sexps().iter();
//        match elems.next() {
//            Some(head) => match self.expand_syntax(head.to_owned(), env, Ctx::Body) {
//                Expanded::Syn(_) => todo!(),
//                Expanded::Binding(
//                    _,
//                    Binding::CoreDefTransformer {
//                        scopes,
//                        name,
//                        transformer,
//                    },
//                ) => Expanded::Item(transformer(self, syn, env)),
//                Expanded::Item(_) => todo!(),
//                _ => todo!(),
//            },
//            //Some(head) => match head {
//            //    SynExp::List(_) => {
//            //        todo!();
//            //        //match self.expand_syn(head.to_owned(), env) {
//            //        //ItemSyn::Expr(e) => {
//            //        //    let mut rest_e = self.expand_exprs_iter(elems, env);
//            //        //    rest_e.insert(0, e);
//            //        //    ast::Expr {
//            //        //        span: syn.source_span(),
//            //        //        kind: ast::ExprKind::List(rest_e),
//            //        //    }
//            //        //}
//            //        //ItemSyn::Syn(_) => todo!(),
//            //        //}
//            //    }
//            //    SynExp::Symbol(s) => match resolve(s, env) {
//            //        res @ (Some(Binding::Value { .. }) | None) => {
//            //            todo!()
//            //            //let e = self.expand_exprs(
//            //            //    syn.red().green(),
//            //            //    syn.sexps(),
//            //            //    syn.source_span(),
//            //            //    env,
//            //            //);
//            //            //if res.is_some() {
//            //            //    e.into_item()
//            //            //} else {
//            //            //    e.into_error().into_item()
//            //            //}
//            //        }
//            //        Some(Binding::CoreExprTransformer { transformer, .. }) => {
//            //            Expanded::Item(transformer(self, syn, env))
//            //        }
//            //        Some(Binding::SyntaxTransformer { .. }) => {
//            //            todo!()
//            //            //let macro_scope = Scope::new();
//            //            //match transformer(syn.with_scope(macro_scope)) {
//            //            //    Ok(mut e) => {
//            //            //        e.flip_scope(macro_scope);
//            //            //        self.expand_expr(e, env)
//            //            //    }
//            //            //    Err(d) => {
//            //            //        self.diagnostics.extend(d);
//            //            //        ast::Expr {
//            //            //            span: syn.source_span(),
//            //            //            kind: ast::ExprKind::List(vec![]),
//            //            //        }
//            //            //    }
//            //            //}
//            //        }
//            //        //Some(Binding::NativeSyntaxTransformer { .. }) => {
//            //        //    todo!()
//            //        //    //let macro_scope = Scope::new();
//            //        //    //match transformer.expand(self, syn.with_scope(macro_scope), env) {
//            //        //    //    Some(mut e) => {
//            //        //    //        e.flip_scope(macro_scope);
//            //        //    //        self.expand_expr(e, env)
//            //        //    //    }
//            //        //    //    None => ast::Expr {
//            //        //    //        span: syn.source_span(),
//            //        //    //        kind: ast::ExprKind::List(vec![]),
//            //        //    //    },
//            //        //    //}
//            //        //}
//            //        r => todo!("{} - {r:?}", syn.red()),
//            //    },
//            //    SynExp::Boolean(_) | SynExp::Char(_) => {
//            //        todo!()
//            //        //self.emit_error(|b| {
//            //        //    b.span(head.source_span()).msg(format!(
//            //        //        "tried to apply non-procedure `{}`",
//            //        //        head.red().green()
//            //        //    ))
//            //        //});
//            //        //let exprs =
//            //        //    self.expand_exprs(syn.red().green(), syn.sexps(), syn.source_span(), env);
//            //        //exprs.into_error().into_item()
//            //    }
//            //},
//            None => {
//                if syn.has_close_delim() {
//                    self.emit_error(|b| {
//                        b.msg("empty lists must be quoted")
//                            .span(syn.source_span())
//                            .related(vec![Diagnostic::builder().hint().msg("try '()").finish()])
//                    });
//                }
//                todo!()
//                //Expr::new_list(syn.source_span(), syn.red().green(), vec![]).into_item()
//            }
//        }
//    }
//
//    /// Runs the macro expansion step.
//    ///
//    /// Macros are expanded recursively. The returned syntax is **not** a macro call, so it can be
//    /// treated as a normal expression/definition.
//    ///
//    /// * Macro definitions are handled here. The macro is compiled, registered, and [`None`] is
//    /// returned.
//    /// * Imports are handled here. The bindings are imported and [`None`] is returned.
//    /// * Module definitions are handled here. The module is expanded as well and it gets registered.
//    /// * Definitions are registered but not expanded.
//    fn expand_macro(&mut self, syn: SynExp, env: &mut Env<String, Binding>) -> Option<SynExp> {
//        // TODO: Catch recursive macros to prevent a stack overflow.
//        let SynExp::List(l) = syn else {
//            return Some(syn);
//        };
//
//        if let Some(SynExp::Symbol(s)) = l.sexps().first() {
//            match resolve(s, env) {
//                Some(Binding::SyntaxTransformer { transformer, .. }) => match transformer(l) {
//                    Ok(syn) => return self.expand_macro(syn, env),
//                    Err(diags) => {
//                        self.diagnostics.extend(diags);
//                        return None;
//                    }
//                },
//                //Some(Binding::NativeSyntaxTransformer { .. }) => {
//                //    todo!();
//                //    //let s = transformer.expand(self, l, env)?;
//                //    //return self.expand_macro(s, env);
//                //}
//                Some(Binding::Import { .. }) => {
//                    todo!();
//                    //import(self, l, env);
//                    //return None;
//                }
//                Some(Binding::Module { .. }) => {
//                    todo!()
//                    //module(self, l);
//                    //return None;
//                }
//                Some(Binding::CoreDefTransformer { scopes, .. }) => {
//                    let mut sexps = l.sexps().iter();
//                    if let Some(s) = sexps.nth(1).and_then(SynExp::symbol) {
//                        env.insert(
//                            s.value().to_string(),
//                            Binding::Value {
//                                scopes: scopes.clone(),
//                                orig_module: self.current_module(),
//                                orig_name: s.value().to_string(),
//                                name: Some(s.value().to_string()),
//                            },
//                        );
//                    }
//                }
//                Some(Binding::CoreMacroDefTransformer { .. }) => {
//                    todo!()
//                    //transformer(self, l, env);
//                    //return None;
//                }
//                _ => {}
//            }
//        }
//        Some(SynExp::List(l))
//    }
//
//    //fn expand_item(&mut self, syn: SynExp, env: &Env<String, Binding>) -> Option<Item> {
//    //    match syn {
//    //        SynExp::List(l) => match l.sexps().iter().next() {
//    //            Some(SynExp::Symbol(s)) => match resolve(s, env) {
//    //                Some(Binding::Import { .. }) => {
//    //                    panic!("import should have been removed in expand_macro")
//    //                }
//    //                Some(Binding::CoreDefTransformer { transformer, .. }) => {
//    //                    transformer(self, l, env)
//    //                }
//    //                _ => Some(self.expand_expr(SynExp::List(l), env)),
//    //            },
//    //            _ => Some(self.expand_expr(SynExp::List(l), env)),
//    //        },
//    //        SynExp::Symbol(_) | SynExp::Boolean(_) | SynExp::Char(_) => {
//    //            Some(self.expand_expr(syn, env))
//    //        }
//    //    }
//    //}
//
//    //fn expand_expr(&mut self, syn: SynExp, env: &Env<String, Binding>) -> Item {
//    //    let span = syn.source_span();
//    //    match syn {
//    //        SynExp::Boolean(b) => Atom::new_boolean(span, Rc::clone(b.red().green())).into(),
//    //        SynExp::Char(c) => Atom::new_char(span, Rc::clone(c.red().green())).into(),
//    //        SynExp::Symbol(s) => match resolve(&s, env) {
//    //            Some(Binding::Value {
//    //                orig_name,
//    //                name,
//    //                orig_module,
//    //                ..
//    //            }) => Atom::new_var(
//    //                span,
//    //                Rc::clone(s.red().green()),
//    //                *orig_module,
//    //                name.as_ref().unwrap_or(orig_name).to_string(),
//    //            )
//    //            .into(),
//    //            _ => {
//    //                self.emit_error(|b| {
//    //                    b.span(span)
//    //                        .msg(format!("undefined variable `{}`", s.value()))
//    //                });
//    //                Atom::new_var(
//    //                    span,
//    //                    Rc::clone(s.red().green()),
//    //                    self.current_module(),
//    //                    s.value().to_string(),
//    //                )
//    //                .into()
//    //            }
//    //        },
//    //        SynExp::List(l) => self.expand_list_expr(l, env),
//    //    }
//    //}
//
//    //fn expand_list_expr(&mut self, syn: SynList, env: &Env<String, Binding>) -> Item {
//    //    let mut elems = syn.sexps().iter();
//    //    match elems.next() {
//    //        Some(head) => match head {
//    //            SynExp::List(_) => {
//    //                todo!();
//    //                //match self.expand_syn(head.to_owned(), env) {
//    //                //ItemSyn::Expr(e) => {
//    //                //    let mut rest_e = self.expand_exprs_iter(elems, env);
//    //                //    rest_e.insert(0, e);
//    //                //    ast::Expr {
//    //                //        span: syn.source_span(),
//    //                //        kind: ast::ExprKind::List(rest_e),
//    //                //    }
//    //                //}
//    //                //ItemSyn::Syn(_) => todo!(),
//    //                //}
//    //            }
//    //            SynExp::Symbol(s) => match resolve(s, env) {
//    //                res @ (Some(Binding::Value { .. }) | None) => {
//    //                    todo!()
//    //                    //let e = self.expand_exprs(
//    //                    //    syn.red().green(),
//    //                    //    syn.sexps(),
//    //                    //    syn.source_span(),
//    //                    //    env,
//    //                    //);
//    //                    //if res.is_some() {
//    //                    //    e.into_item()
//    //                    //} else {
//    //                    //    e.into_error().into_item()
//    //                    //}
//    //                }
//    //                Some(Binding::CoreExprTransformer { transformer, .. }) => {
//    //                    transformer(self, syn, env)
//    //                }
//    //                Some(Binding::SyntaxTransformer { .. }) => {
//    //                    todo!()
//    //                    //let macro_scope = Scope::new();
//    //                    //match transformer(syn.with_scope(macro_scope)) {
//    //                    //    Ok(mut e) => {
//    //                    //        e.flip_scope(macro_scope);
//    //                    //        self.expand_expr(e, env)
//    //                    //    }
//    //                    //    Err(d) => {
//    //                    //        self.diagnostics.extend(d);
//    //                    //        ast::Expr {
//    //                    //            span: syn.source_span(),
//    //                    //            kind: ast::ExprKind::List(vec![]),
//    //                    //        }
//    //                    //    }
//    //                    //}
//    //                }
//    //                //Some(Binding::NativeSyntaxTransformer { .. }) => {
//    //                //    todo!()
//    //                //    //let macro_scope = Scope::new();
//    //                //    //match transformer.expand(self, syn.with_scope(macro_scope), env) {
//    //                //    //    Some(mut e) => {
//    //                //    //        e.flip_scope(macro_scope);
//    //                //    //        self.expand_expr(e, env)
//    //                //    //    }
//    //                //    //    None => ast::Expr {
//    //                //    //        span: syn.source_span(),
//    //                //    //        kind: ast::ExprKind::List(vec![]),
//    //                //    //    },
//    //                //    //}
//    //                //}
//    //                r => todo!("{} - {r:?}", syn.red()),
//    //            },
//    //            SynExp::Boolean(_) | SynExp::Char(_) => {
//    //                todo!()
//    //                //self.emit_error(|b| {
//    //                //    b.span(head.source_span()).msg(format!(
//    //                //        "tried to apply non-procedure `{}`",
//    //                //        head.red().green()
//    //                //    ))
//    //                //});
//    //                //let exprs =
//    //                //    self.expand_exprs(syn.red().green(), syn.sexps(), syn.source_span(), env);
//    //                //exprs.into_error().into_item()
//    //            }
//    //        },
//    //        None => {
//    //            if syn.has_close_delim() {
//    //                self.emit_error(|b| {
//    //                    b.msg("empty lists must be quoted")
//    //                        .span(syn.source_span())
//    //                        .related(vec![Diagnostic::builder().hint().msg("try '()").finish()])
//    //                });
//    //            }
//    //            todo!()
//    //            //Expr::new_list(syn.source_span(), syn.red().green(), vec![]).into_item()
//    //        }
//    //    }
//    //}
//
//    //fn expand_exprs(
//    //    &mut self,
//    //    green: &Rc<GreenTree>,
//    //    sexps: &[SynExp],
//    //    span: Span,
//    //    env: &Env<String, Binding>,
//    //) -> Expr {
//    //    let mut exprs = vec![];
//    //    for e in sexps {
//    //        exprs.push(self.expand_expr(e.to_owned(), env));
//    //    }
//    //    Expr::new_list(span, green, exprs)
//    //}
//
//    fn binding_to_item(&self, syn: SynSymbol, binding: Binding) -> Item {
//        let span = syn.source_span();
//        let source = Rc::clone(syn.red().green());
//        let (source_module, unique_name) = match binding {
//            Binding::Value {
//                orig_module,
//                orig_name,
//                name,
//                ..
//            } => (Some(orig_module), name.unwrap_or(orig_name)),
//            Binding::CoreDefTransformer { name, .. }
//            | Binding::CoreMacroDefTransformer { name, .. }
//            | Binding::CoreExprTransformer { name, .. } => (None, name),
//            Binding::SyntaxTransformer {
//                source_module,
//                name,
//                ..
//            } => (Some(source_module), name),
//            _ => (None, syn.to_string()),
//        };
//
//        Item::Atom(Atom::new_var(
//            span,
//            source,
//            source_module.unwrap_or_else(|| self.current_module()),
//            unique_name,
//        ))
//    }
//
//    const fn current_module(&self) -> ast::ModId {
//        self.module
//    }
//
//    fn enter_module(&mut self, module: ast::ModId) {
//        self.module_stack.push((
//            std::mem::replace(&mut self.module, module),
//            std::mem::take(&mut self.dependencies),
//        ));
//    }
//
//    fn exit_module(&mut self) {
//        (self.module, self.dependencies) = self.module_stack.pop().unwrap();
//    }
//
//    fn emit_error(&mut self, builder: impl Fn(DiagnosticBuilder) -> DiagnosticBuilder) {
//        self.diagnostics
//            .push(builder(Diagnostic::builder()).finish());
//    }
//}

//fn intrinsics_env() -> Env<'static, String, Binding> {
//    let mut env = Env::default();
//    env.insert(
//        String::from("import"),
//        Binding::Import {
//            scopes: Scopes::core(),
//        },
//    );
//    env.insert(
//        String::from("module"),
//        Binding::Module {
//            scopes: Scopes::core(),
//        },
//    );
//    //env.insert(
//    //    String::from("library"),
//    //    Binding::SyntaxTransformer {
//    //        scopes: Scopes::core(),
//    //        name: String::from("library"),
//    //        transformer: core::library_transformer,
//    //    },
//    //);
//    env
//}

//fn resolve<'env>(var: &SynSymbol, env: &'env Env<String, Binding>) -> Option<&'env Binding> {
//    env.get_all(var.value())
//        .into_iter()
//        .fold((None, 0usize), |acc, cur| {
//            match cur.scopes().subset(var.scopes()) {
//                Some(s) if s.get() >= acc.1 => (Some(cur), s.get()),
//                _ => acc,
//            }
//        })
//        .0
//}

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
    ///// An expression was compiled at compile-time into a transformer (e.g. lambda).
    //SyntaxTransformer {
    //    source_module: ast::ModId,
    //    scopes: Scopes,
    //    name: String,
    //    transformer: SyntaxTransformer,
    //},
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
            | Binding::CoreExprTransformer { name, .. } => name,
            //| Binding::SyntaxTransformer { name, .. } => name,
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
            | Binding::CoreExprTransformer { scopes, .. } => scopes,
            //| Binding::SyntaxTransformer { scopes, .. } => scopes,
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
            | Binding::CoreExprTransformer { ref mut scopes, .. } => scopes,
            //| Binding::SyntaxTransformer { ref mut scopes, .. } => scopes,
            //| Binding::NativeSyntaxTransformer { ref mut scopes, .. } => scopes,
        }
    }
}

thread_local! {
    static IDENTIFIER_ID: Cell<u64> = Cell::new(1);
}

//#[cfg(test)]
//mod tests {
//    use std::{cell::RefCell, collections::HashMap};
//
//    use expect_test::{expect, Expect};
//
//    use crate::{new_syntax::parser::parse_str, utils::Resolve};
//
//    use super::*;
//
//    fn core_env() -> Env<'static, String, Binding> {
//        let mut core = Env::new();
//        core.insert(
//            String::from("define"),
//            Binding::CoreDefTransformer {
//                scopes: Scopes::core(),
//                name: String::from("define"),
//                transformer: core::define_transformer,
//            },
//        );
//        //core.insert(
//        //    String::from("define-syntax"),
//        //    Binding::CoreMacroDefTransformer {
//        //        scopes: Scopes::core(),
//        //        name: String::from("define-syntax"),
//        //        transformer: core::define_syntax_transformer,
//        //    },
//        //);
//        //core.insert(
//        //    String::from("lambda"),
//        //    Binding::CoreExprTransformer {
//        //        scopes: Scopes::core(),
//        //        name: String::from("lambda"),
//        //        transformer: core::lambda_transformer,
//        //    },
//        //);
//        //core.insert(
//        //    String::from("if"),
//        //    Binding::CoreExprTransformer {
//        //        scopes: Scopes::core(),
//        //        name: String::from("if"),
//        //        transformer: core::if_transformer,
//        //    },
//        //);
//        //core.insert(
//        //    String::from("quote"),
//        //    Binding::CoreExprTransformer {
//        //        scopes: Scopes::core(),
//        //        name: String::from("quote"),
//        //        transformer: core::quote_transformer,
//        //    },
//        //);
//        //core.insert(
//        //    String::from("cons"),
//        //    Binding::Value {
//        //        scopes: Scopes::core(),
//        //        orig_module: ModuleName::from_strings(vec!["rnrs", "expander", "core"]),
//        //        orig_name: String::from("cons"),
//        //        name: None,
//        //    },
//        //);
//        core
//    }
//
//    struct Libs {
//        libs: RefCell<HashMap<ast::ModId, ast::Module>>,
//    }
//
//    impl Default for Libs {
//        fn default() -> Self {
//            let mid = ast::ModuleName::from_strings(vec!["rnrs", "expander", "core"]);
//            Self {
//                libs: RefCell::new(HashMap::from([(
//                    mid,
//                    ast::Module {
//                        mid,
//                        span: Span::dummy(),
//                        source: List::new_mod(
//                            Span::dummy(),
//                            Rc::new(GreenTree::node(SyntaxKind::Root, vec![])),
//                            mid,
//                            vec![],
//                        ),
//                        dependencies: vec![],
//                        exports: core_env(),
//                        bindings: Env::default(),
//                    },
//                )])),
//            }
//        }
//    }
//
//    impl Libs {
//        fn import(&self, mid: ast::ModId) -> Result<Rc<ModuleInterface>, Diagnostic> {
//            Ok(Rc::new(
//                self.libs
//                    .borrow()
//                    .get(&mid)
//                    .expect(&format!("{}", mid.resolve()))
//                    .to_interface(),
//            ))
//        }
//
//        fn define(&self, mid: ast::ModId, module: Module) {
//            self.libs.borrow_mut().insert(mid, module);
//        }
//    }
//
//    pub fn check(input: &str, expected: Expect) {
//        let res = parse_str(input);
//        assert_eq!(res.diagnostics, vec![]);
//        let syn = SynRoot::new(&res.tree, res.file_id);
//        let env = core_env().enter_consume();
//        let libs = Libs::default();
//        let res = expand_root(
//            syn,
//            &env,
//            |mid| libs.import(mid),
//            |mid, mod_| libs.define(mid, mod_),
//        );
//        assert_eq!(res.diagnostics, vec![]);
//        let mut out = String::new();
//        for item in res.items {
//            out.push_str(&format!("{item:#?}"));
//            out.push('\n');
//        }
//        expected.assert_eq(&out);
//    }
//
//    #[test]
//    fn boolean() {
//        check(
//            "#t",
//            expect![[r#"
//                {#t 0:0..2}
//            "#]],
//        );
//        check(
//            "#f",
//            expect![[r#"
//                {#f 0:0..2}
//            "#]],
//        );
//    }
//
//    #[test]
//    fn char() {
//        check(
//            r"#\a",
//            expect![[r#"
//                {#\a 0:0..3}
//            "#]],
//        );
//        check(
//            r"#\λ",
//            expect![[r#"
//                {#\λ 0:0..4}
//            "#]],
//        );
//        check(
//            r"#\x3bb",
//            expect![[r#"
//                {#\λ 0:0..6}
//            "#]],
//        );
//        check(
//            r"#\nul",
//            expect![[r#"
//                {#\x0 0:0..5}
//            "#]],
//        );
//        check(
//            r"#\alarm",
//            expect![[r#"
//                {#\x7 0:0..7}
//            "#]],
//        );
//        check(
//            r"#\backspace",
//            expect![[r#"
//                {#\x8 0:0..11}
//            "#]],
//        );
//        check(
//            r"#\tab",
//            expect![[r#"
//                {#\x9 0:0..5}
//            "#]],
//        );
//        check(
//            r"#\linefeed",
//            expect![[r#"
//                {#\xA 0:0..10}
//            "#]],
//        );
//        check(
//            r"#\newline",
//            expect![[r#"
//                {#\xA 0:0..9}
//            "#]],
//        );
//        check(
//            r"#\vtab",
//            expect![[r#"
//                {#\xB 0:0..6}
//            "#]],
//        );
//        check(
//            r"#\page",
//            expect![[r#"
//                {#\xC 0:0..6}
//            "#]],
//        );
//        check(
//            r"#\return",
//            expect![[r#"
//                {#\xD 0:0..8}
//            "#]],
//        );
//        check(
//            r"#\esc",
//            expect![[r#"
//                {#\x1B 0:0..5}
//            "#]],
//        );
//        check(
//            r"#\space",
//            expect![[r#"
//                {#\x20 0:0..7}
//            "#]],
//        );
//        check(
//            r"#\delete",
//            expect![[r#"
//                {#\x7F 0:0..8}
//            "#]],
//        );
//    }
//
//    #[test]
//    #[ignore]
//    fn variables() {
//        check(
//            "cons",
//            expect![[r#"
//                {var |cons| (rnrs expander core ()) 0:0..4}
//            "#]],
//        );
//    }
//
//    #[test]
//    #[ignore]
//    fn if_() {
//        check(
//            r"(if #\a #\b)",
//            expect![[r#"
//                {if 0:0..12
//                  {#\a 0:4..3}
//                  {#\b 0:8..3}
//                  {void 0:0..12}}
//            "#]],
//        );
//        check(
//            r"(if #t #f #\f)",
//            expect![[r#"
//                {if 0:0..14
//                  {#t 0:4..2}
//                  {#f 0:7..2}
//                  {#\f 0:10..3}}
//            "#]],
//        );
//    }
//
//    #[test]
//    fn define_and_use_variable() {
//        check(
//            r"(define a #\a)
//              a",
//            expect![[r#"
//                {define@0:0..14
//                  {|a| 0:8..1}
//                  {#\a 0:10..3}}
//                {var |a| (#script ()) 0:29..1}
//            "#]],
//        );
//    }
//}
