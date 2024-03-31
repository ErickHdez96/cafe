use crate::{
    env::Env,
    expander::{self, binding::Binding, scopes::Scopes},
    interner::BuiltinTys,
    span::Span,
    syntax::ast,
};

pub fn core_expander_interface() -> ast::ModuleInterface {
    let mid = ast::ModuleName::from_strings(vec!["rnrs", "expander", "core"]);
    // TODO: set!, cond, let, let*, letrec, letrec*, let-values, begin

    ast::ModuleInterface {
        id: mid,
        span: Span::dummy(),
        bindings: Env::from_iter(vec![
            (
                "define".into(),
                Binding::CoreDefTransformer {
                    scopes: Scopes::core(),
                    name: "define".into(),
                    transformer: expander::core::define_transformer,
                },
            ),
            (
                "lambda".into(),
                Binding::CoreExprTransformer {
                    scopes: Scopes::core(),
                    name: "lambda".into(),
                    transformer: expander::core::lambda_transformer,
                },
            ),
            (
                "if".into(),
                Binding::CoreExprTransformer {
                    scopes: Scopes::core(),
                    name: "if".into(),
                    transformer: expander::core::if_transformer,
                },
            ),
            (
                "quote".into(),
                Binding::CoreExprTransformer {
                    scopes: Scopes::core(),
                    name: "quote".into(),
                    transformer: expander::core::quote_transformer,
                },
            ),
        ]),
        types: Some(Env::default()),
        dependencies: vec![],
    }
}

pub fn intrinsics_interface(builtin_tys: &BuiltinTys) -> ast::ModuleInterface {
    let mid = ast::ModuleName::from_strings(vec!["rnrs", "intrinsics"]);

    ast::ModuleInterface {
        id: mid,
        span: Span::dummy(),
        bindings: Env::from_iter(vec![
            (
                "+".into(),
                Binding::Value {
                    scopes: Scopes::core(),
                    name: "+".into(),
                    orig_module: mid,
                },
            ),
            (
                "-".into(),
                Binding::Value {
                    scopes: Scopes::core(),
                    name: "-".into(),
                    orig_module: mid,
                },
            ),
            (
                "*".into(),
                Binding::Value {
                    scopes: Scopes::core(),
                    name: "*".into(),
                    orig_module: mid,
                },
            ),
            (
                "/".into(),
                Binding::Value {
                    scopes: Scopes::core(),
                    name: "/".into(),
                    orig_module: mid,
                },
            ),
            (
                "=".into(),
                Binding::Value {
                    scopes: Scopes::core(),
                    name: "=".into(),
                    orig_module: mid,
                },
            ),
            (
                "<".into(),
                Binding::Value {
                    scopes: Scopes::core(),
                    name: "<".into(),
                    orig_module: mid,
                },
            ),
            (
                "<=".into(),
                Binding::Value {
                    scopes: Scopes::core(),
                    name: "<=".into(),
                    orig_module: mid,
                },
            ),
            (
                ">".into(),
                Binding::Value {
                    scopes: Scopes::core(),
                    name: ">".into(),
                    orig_module: mid,
                },
            ),
            (
                ">=".into(),
                Binding::Value {
                    scopes: Scopes::core(),
                    name: ">=".into(),
                    orig_module: mid,
                },
            ),
            (
                "cons".into(),
                Binding::Value {
                    scopes: Scopes::core(),
                    name: "cons".into(),
                    orig_module: mid,
                },
            ),
            (
                "truthy?".into(),
                Binding::Value {
                    scopes: Scopes::core(),
                    name: "cons".into(),
                    orig_module: mid,
                },
            ),
        ]),
        types: Some(Env::from_iter(vec![
            ("+".into(), builtin_tys.object),
            ("-".into(), builtin_tys.object),
            ("*".into(), builtin_tys.object),
            ("/".into(), builtin_tys.object),
            ("=".into(), builtin_tys.object),
            ("<".into(), builtin_tys.object),
            ("<=".into(), builtin_tys.object),
            (">".into(), builtin_tys.object),
            (">=".into(), builtin_tys.object),
            ("cons".into(), builtin_tys.object),
            ("truthy?".into(), builtin_tys.object),
        ])),
        dependencies: vec![],
    }
}
