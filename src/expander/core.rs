use std::rc::Rc;

use crate::{
    span::Span,
    syntax::{
        ast,
        cst::{Cst, CstKind, ListKind},
        parser::{parse_char, parse_number},
    },
};

use super::{binding::Binding, scopes::Scopes, BEnv, Expander, Source};

pub fn define_transformer(
    expander: &mut Expander<'_>,
    mut source: Source,
    span: Span,
    env: &mut BEnv,
) -> ast::Item {
    let name = match source.nth(1) {
        Some(cst) => match &cst.kind {
            CstKind::List(_, ListKind::List) => todo!(),
            CstKind::Ident(ident) => ast::Ident {
                span: cst.span,
                value: *ident,
            },
            _ => todo!(),
        },
        None => todo!(),
    };
    env.insert(
        name.value,
        Binding::Value {
            scopes: Scopes::core(),
            orig_module: expander.current_module(),
            name: name.value,
        },
    );
    let expr = expander
        .expand_cst(source.next().unwrap(), env)
        .into_expr()
        .unwrap();

    assert!(source.next().is_none());
    assert!(source.dot().next().is_none());

    ast::Define {
        span,
        name,
        expr: Some(Box::new(expr)),
    }
    .into()
}

pub fn if_transformer(
    expander: &mut Expander<'_>,
    mut source: Source,
    span: Span,
    env: &mut BEnv,
) -> ast::Item {
    let cond = Box::new(
        expander
            .expand_cst(source.nth(1).unwrap(), env)
            .into_expr()
            .unwrap(),
    );
    let r#true = Box::new(
        expander
            .expand_cst(source.next().unwrap(), env)
            .into_expr()
            .unwrap(),
    );
    let r#false = Box::new(
        source
            .next()
            .map(|cst| expander.expand_cst(cst, env))
            .map(|i| i.into_expr().unwrap())
            .unwrap_or_else(|| ast::Expr {
                span,
                kind: ast::ExprKind::Void,
                ty: None,
            }),
    );

    assert!(source.next().is_none());
    assert!(source.dot().next().is_none());

    ast::Expr {
        span,
        kind: ast::ExprKind::If(cond, r#true, r#false),
        ty: None,
    }
    .into()
}

pub fn quote_transformer(
    expander: &mut Expander<'_>,
    mut source: Source,
    span: Span,
    _env: &mut BEnv,
) -> ast::Item {
    let expr = Box::new(quote_expander(expander, source.nth(1).unwrap()));

    assert!(source.next().is_none());
    assert!(source.dot().next().is_none());

    ast::Expr {
        span,
        kind: ast::ExprKind::Quote(expr),
        ty: None,
    }
    .into()
}

fn quote_expander(expander: &mut Expander<'_>, cst: Rc<Cst>) -> ast::Expr {
    ast::Expr {
        span: cst.span,
        kind: match &cst.kind {
            CstKind::List(l, ListKind::List) => {
                let mut source = Source::new(l.clone());
                let elements = source
                    .by_ref()
                    .map(|c| quote_expander(expander, c))
                    .collect();

                let mut source = source.dot();
                match source.next() {
                    Some(rest) => {
                        let rest = quote_expander(expander, rest);
                        assert!(source.next().is_none());
                        ast::ExprKind::DottedList(elements, Box::new(rest))
                    }
                    None => ast::ExprKind::List(elements),
                }
            }
            CstKind::List(_, _) => todo!(),
            CstKind::True => ast::ExprKind::Boolean(true),
            CstKind::False => ast::ExprKind::Boolean(false),
            CstKind::Ident(ident) => ast::ExprKind::Var(ast::Path {
                span: cst.span,
                module: expander.current_module(),
                value: *ident,
            }),
            CstKind::Char(c) => ast::ExprKind::Char(parse_char(c.resolve())),
            CstKind::Number(n) => {
                ast::ExprKind::Number(match parse_number(n.resolve(), cst.span) {
                    Ok(n) => n,
                    Err(_) => todo!(),
                })
            }
            CstKind::String(_) => todo!(),
            k => todo!("{k:?}"),
        },
        ty: None,
    }
}

pub fn lambda_transformer(
    expander: &mut Expander<'_>,
    mut source: Source,
    span: Span,
    env: &mut BEnv,
) -> ast::Item {
    let mut env = env.enter();
    let (formals, rest) = formals_transformer(expander, source.nth(1).unwrap());
    for f in &formals {
        env.insert(
            f.value,
            Binding::Value {
                scopes: Scopes::core(),
                orig_module: expander.current_module(),
                name: f.value,
            },
        );
    }
    if let Some(rest) = &rest {
        env.insert(
            rest.value,
            Binding::Value {
                scopes: Scopes::core(),
                orig_module: expander.current_module(),
                name: rest.value,
            },
        );
    }

    let items = source
        .by_ref()
        .map(|c| expander.expand_cst(c, &mut env))
        .collect::<Vec<_>>();
    // TODO: emit diagnostics for expressions before the last define

    assert!(source.next().is_none());
    assert!(source.dot().next().is_none());

    ast::Expr {
        span,
        kind: ast::ExprKind::Lambda {
            formals,
            rest,
            expr: Box::new(ast::Expr {
                span,
                kind: ast::ExprKind::Body(items),
                ty: None,
            }),
        },
        ty: None,
    }
    .into()
}

fn formals_transformer(
    _expander: &mut Expander<'_>,
    cst: Rc<Cst>,
) -> (Vec<ast::Ident>, Option<ast::Ident>) {
    match &cst.as_ref().kind {
        CstKind::List(l, ListKind::List) => {
            let mut source = Source::new(l.clone());
            let mut formals = vec![];
            for cst in source.by_ref() {
                match &cst.as_ref().kind {
                    CstKind::Ident(ident) => {
                        formals.push(ast::Ident {
                            span: cst.span,
                            value: *ident,
                        });
                    }
                    _ => todo!(),
                }
            }
            let rest = match source.dot().next() {
                Some(cst) => match &cst.kind {
                    CstKind::Ident(ident) => Some(ast::Ident {
                        span: cst.span,
                        value: *ident,
                    }),
                    _ => todo!(),
                },
                None => None,
            };
            (formals, rest)
        }
        CstKind::Ident(ident) => (
            vec![],
            Some(ast::Ident {
                span: cst.span,
                value: *ident,
            }),
        ),
        _ => todo!(),
    }
}
