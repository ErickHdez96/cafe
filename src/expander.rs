use std::{cell::Cell, rc::Rc, slice};

use crate::{
    diagnostics::Diagnostic,
    env::Env,
    expander::scopes::{Scope, Scopes},
    file::FileId,
    span::Span,
    syntax::{
        ast,
        cst::{GreenTree, RedTree, SynExp, SynList, SynSymbol},
        SyntaxKind,
    },
};

use self::macros::NativeSyntaxTransformer;

pub mod macros;
pub mod scopes;
pub mod transformers;

type CoreDefTransformer = fn(&SynList, &Env<String, Binding>) -> (ast::Expr, Vec<Diagnostic>);
type CoreExprTransformer = fn(&SynList, &Env<String, Binding>) -> (ast::Expr, Vec<Diagnostic>);
type SyntaxTransformer = fn(&SynList) -> Result<SynExp, Vec<Diagnostic>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Binding {
    Value {
        scopes: Scopes,
        orig_name: String,
        name: Option<String>,
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
            | Binding::CoreDefTransformer { scopes, .. }
            | Binding::CoreExprTransformer { scopes, .. }
            | Binding::SyntaxTransformer { scopes, .. }
            | Binding::NativeSyntaxTransformer { scopes, .. } => scopes,
        }
    }

    pub fn scopes_mut(&mut self) -> &mut Scopes {
        match self {
            Binding::Value { ref mut scopes, .. }
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

pub fn expand_expr(syn: &SynExp, env: &Env<String, Binding>) -> (ast::Expr, Vec<Diagnostic>) {
    match &syn {
        SynExp::Boolean(b) => (
            ast::Expr {
                span: syn.span(),
                kind: ast::ExprKind::Boolean(b.value()),
            },
            vec![],
        ),
        SynExp::Char(c) => (
            ast::Expr {
                span: syn.span(),
                kind: ast::ExprKind::Char(c.value()),
            },
            vec![],
        ),
        SynExp::Symbol(s) => match resolve(s, env) {
            Some(Binding::Value {
                orig_name, name, ..
            }) => (
                ast::Expr {
                    span: syn.span(),
                    kind: ast::ExprKind::Var(ast::Ident {
                        span: syn.span(),
                        value: name.as_ref().unwrap_or(orig_name).to_string(),
                    }),
                },
                vec![],
            ),
            _ => (
                ast::Expr {
                    span: syn.span(),
                    kind: ast::ExprKind::Var(ast::Ident {
                        span: syn.span(),
                        value: s.value().to_string(),
                    }),
                },
                vec![Diagnostic::builder()
                    .span(syn.span())
                    .msg(format!("undefined variable {}", s.value()))
                    .finish()],
            ),
        },
        SynExp::List(l) => expand_list_expr(l, env),
    }
}

fn expand_syn(syn: &SynExp, env: &Env<String, Binding>) -> (ItemSyn, Vec<Diagnostic>) {
    if let SynExp::List(l) = &syn {
        if let Some(head) = l.sexps().first() {
            match head {
                SynExp::List(_) => todo!(),
                SynExp::Symbol(s) => {
                    if let Some(Binding::SyntaxTransformer { transformer, .. }) = resolve(s, env) {
                        return match transformer(l) {
                            Ok(res) => (ItemSyn::Syn(res), vec![]),
                            Err(d) => (
                                ItemSyn::Expr(
                                    ast::Expr {
                                        span: syn.span(),
                                        kind: ast::ExprKind::List(vec![]),
                                    }
                                    .into_error(),
                                ),
                                d,
                            ),
                        };
                    }
                }
                SynExp::Boolean(_) | SynExp::Char(_) => {}
            }
        }
    }
    let (e, d) = expand_expr(syn, env);
    (ItemSyn::Expr(e), d)
}

fn expand_list_expr(syn: &SynList, env: &Env<String, Binding>) -> (ast::Expr, Vec<Diagnostic>) {
    let mut elems = syn.sexps().iter();
    match elems.next() {
        Some(head) => match head {
            SynExp::List(_) => {
                let (res, mut diags) = expand_syn(head, env);
                match res {
                    ItemSyn::Expr(e) => {
                        let (mut rest_e, rest_diags) = expand_exprs_iter(elems, env);
                        rest_e.insert(0, e);
                        diags.extend(rest_diags);
                        (
                            ast::Expr {
                                span: syn.span(),
                                kind: ast::ExprKind::List(rest_e),
                            },
                            diags,
                        )
                    }
                    ItemSyn::Syn(_) => todo!(),
                }
            }
            SynExp::Symbol(s) => match resolve(s, env) {
                res @ (Some(Binding::Value { .. }) | None) => {
                    let (e, d) = expand_exprs(syn.sexps(), syn.span(), env);
                    (if res.is_some() { e } else { e.into_error() }, d)
                }
                Some(Binding::CoreExprTransformer { transformer, .. }) => transformer(syn, env),
                Some(Binding::SyntaxTransformer { transformer, .. }) => {
                    let macro_scope = Scope::new();
                    match transformer(&syn.with_scope(macro_scope)) {
                        Ok(mut e) => {
                            e.flip_scope(macro_scope);
                            expand_expr(&e, env)
                        }
                        Err(d) => (
                            ast::Expr {
                                span: syn.span(),
                                kind: ast::ExprKind::List(vec![]),
                            }
                            .into_error(),
                            d,
                        ),
                    }
                }
                r => todo!("{r:?}"),
            },
            SynExp::Boolean(_) | SynExp::Char(_) => {
                let (exprs, mut diags) = expand_exprs(syn.sexps(), syn.span(), env);
                diags.insert(
                    0,
                    Diagnostic::builder()
                        .msg(format!(
                            "tried to apply non-procedure {}",
                            head.red().green()
                        ))
                        .finish(),
                );
                (exprs.into_error(), diags)
            }
        },
        None => (
            ast::Expr {
                span: syn.span(),
                kind: ast::ExprKind::Quote(Box::new(ast::Expr {
                    span: syn.span(),
                    kind: ast::ExprKind::List(vec![]),
                })),
            },
            vec![Diagnostic::builder()
                .msg("empty lists must be quoted")
                .sources(vec![Diagnostic::builder().hint().msg("try '()").finish()])
                .finish()],
        ),
    }
}

fn expand_exprs(
    sexps: &[SynExp],
    span: Span,
    env: &Env<String, Binding>,
) -> (ast::Expr, Vec<Diagnostic>) {
    let mut exprs = vec![];
    let mut diags = vec![];
    for e in sexps {
        let (expr, d) = expand_expr(e, env);
        exprs.push(expr);
        diags.extend(d);
    }
    (
        ast::Expr {
            span,
            kind: ast::ExprKind::List(exprs),
        },
        diags,
    )
}

fn expand_exprs_iter(
    sexps: slice::Iter<SynExp>,
    env: &Env<String, Binding>,
) -> (Vec<ast::Expr>, Vec<Diagnostic>) {
    let mut exprs = vec![];
    let mut diags = vec![];
    for e in sexps {
        let (expr, d) = expand_expr(e, env);
        exprs.push(expr);
        diags.extend(d);
    }
    (exprs, diags)
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
fn let_transformer(syn: &SynList) -> Result<SynExp, Vec<Diagnostic>> {
    let sexps_len = syn.sexps().len();
    let mut l = syn.sexps().iter();
    assert!(l.next().is_some());
    let binssyn = l.next().and_then(SynExp::list).ok_or_else(|| {
        vec![Diagnostic::builder()
            .msg("expected a list of bindings")
            .finish()]
    })?;
    let bindings = binssyn
        .sexps()
        .iter()
        .map(|s| {
            s.list()
                .map(|l| {
                    let mut ls = l.sexps().iter();
                    let b = (ls.next().unwrap().symbol().unwrap(), ls.next().unwrap());
                    assert!(ls.next().is_none());
                    b
                })
                .ok_or_else(|| {
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
fn or_transformer(syn: &SynList) -> Result<SynExp, Vec<Diagnostic>> {
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

    use crate::{
        file::FileId,
        syntax::{cst::SynRoot, parser::parse_str},
    };

    use super::{
        transformers::{if_core_transformer, lambda_core_transformer},
        *,
    };

    fn root_env() -> Env<'static, String, Binding> {
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
        let mut root = core.enter_consume();
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
        let red = SynRoot::new(&res.tree, FileId::default());
        let mut children = red.syn_children();
        let env = root_env();
        let (ast, errs) = expand_expr(&children.next().expect("expected an item"), &env);
        assert_eq!(errs, vec![]);
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
        let root = SynRoot::new_red(&red, FileId::default());
        let children = root.children();
        let or = dbg!(SynExp::cast(&children[0], FileId::default()).unwrap());
        let out = let_transformer(or.list().unwrap()).unwrap();
        assert_eq!(out.to_string(), "((lambda (x) x) #t)");
        assert_eq!(out.list().unwrap().syn_string(), "((lambda (x) x) #t)");
    }

    #[test]
    fn or() {
        let res = parse_str("(or)");
        assert_eq!(res.diagnostics, vec![]);
        let red = RedTree::new(&res.tree);
        let root = SynRoot::new_red(&red, FileId::default());
        let children = root.children();
        let or = dbg!(SynExp::cast(&children[0], FileId::default()).unwrap());
        let out = or_transformer(or.list().unwrap()).unwrap();
        assert_eq!(out.to_string(), "#t");
        assert_eq!(out.syn_string(), "#t");
    }

    #[test]
    fn or_e() {
        let res = parse_str("(or #f)");
        assert_eq!(res.diagnostics, vec![]);
        let red = RedTree::new(&res.tree);
        let root = SynRoot::new_red(&red, FileId::default());
        let children = root.children();
        let or = dbg!(SynExp::cast(&children[0], FileId::default()).unwrap());
        let out = or_transformer(or.list().unwrap()).unwrap();
        assert_eq!(out.to_string(), "#f");
        assert_eq!(out.syn_string(), "#f");
    }

    #[test]
    fn or_e0_e() {
        let res = parse_str(r"(or #\0 #\1)");
        assert_eq!(res.diagnostics, vec![]);
        let red = RedTree::new(&res.tree);
        let root = SynRoot::new_red(&red, FileId::default());
        let children = root.children();
        let or = dbg!(SynExp::cast(&children[0], FileId::default()).unwrap());
        let out = or_transformer(or.list().unwrap()).unwrap();
        assert_eq!(out.to_string(), r"(let ((x #\0)) (if x x (or #\1)))");
        assert_eq!(out.syn_string(), r"(let ((x #\0)) (if x x (or #\1)))");
    }
}
