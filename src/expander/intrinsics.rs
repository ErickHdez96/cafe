use crate::{
    env::Env,
    expander::{expand_module_with_config, ExpanderConfig, Source},
    span::Span,
    symbol::Symbol,
    syntax::{
        ast,
        cst::{CstKind, ListKind},
    },
};

use super::{binding::Binding, scopes::Scopes, BEnv, Expander, SynList};

pub fn intrinsics_env() -> BEnv<'static> {
    Env::from_iter(vec![
        (
            Symbol::from("import"),
            Binding::Import {
                scopes: Scopes::core(),
            },
        ),
        (
            Symbol::from("module"),
            Binding::Module {
                scopes: Scopes::core(),
            },
        ),
    ])
}

pub fn import(
    expander: &mut Expander<'_>,
    mut source: SynList,
    _span: Span,
    env: &mut BEnv,
) -> Vec<ast::ModId> {
    source.next();
    let mut mids = vec![];

    for cst in source.by_ref() {
        let CstKind::List(l, ListKind::List) = &cst.kind else {
            todo!()
        };

        let mid = parse_module_name(expander, Source::new(l.clone()));

        match expander.import(mid) {
            Ok(i) => {
                for (v, b) in i.bindings.bindings() {
                    if env.has_immediate(v) {
                        todo!()
                    } else {
                        env.insert(*v, b.clone());
                    }
                }
            }
            Err(d) => {
                expander.diagnostics.push(d);
            }
        }
        mids.push(mid);
    }

    assert!(source.next().is_none());
    assert!(source.dot().next().is_none());

    mids
}

pub fn module(expander: &mut Expander<'_>, mut source: SynList, _span: Span) -> ast::ModId {
    let CstKind::List(l, ListKind::List) = &source.nth(1).unwrap().kind else {
        todo!()
    };
    let mid = parse_module_name(expander, Source::new(l.clone()));
    let exports = parse_export_identifiers(
        expander,
        Source::new(source.next().unwrap().to_list().unwrap().0.into()),
    );
    let mut result = expand_module_with_config(
        source.by_ref().collect(),
        ExpanderConfig::default()
            .file_id(expander.file_id)
            .import(expander.import)
            .register(expander.register)
            .base_env(&intrinsics_env())
            .mod_id(mid),
    );

    assert!(source.next().is_none());
    assert!(source.dot().next().is_none());

    expander.diagnostics.extend(result.diagnostics);
    result.module.exports = Env::from_iter(exports.into_iter().map(|e| {
        (
            e.value,
            result.module.bindings.get(&e.value).unwrap().clone(),
        )
    }));

    (expander.register)(mid, result.module);
    mid
}

fn parse_module_name(_expander: &mut Expander<'_>, mut source: Source) -> ast::ModId {
    let mut symbols = vec![];
    for c in source.by_ref() {
        match &c.kind {
            CstKind::List(l, ListKind::List) => {
                let mut source = Source::new(l.clone());
                assert!(source.next().is_none());
                assert!(source.dot().next().is_none());
            }
            CstKind::Ident(ident) => symbols.push(*ident),
            k => todo!("{k:?}"),
        }
    }
    assert!(source.next().is_none());
    assert!(source.dot().next().is_none());
    ast::ModuleName::from_strings(symbols)
}

fn parse_export_identifiers(_expander: &mut Expander<'_>, mut source: Source) -> Vec<ast::Ident> {
    let exports = source
        .by_ref()
        .map(|c| ast::Ident {
            span: c.span,
            value: c.to_ident().unwrap(),
        })
        .collect();
    assert!(source.next().is_none());
    assert!(source.dot().next().is_none());
    exports
}
