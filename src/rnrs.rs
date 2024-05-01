use std::rc::Rc;

use crate::{
    env::Env,
    expander::{self, binding::Binding, scopes::Scopes},
    interner::Interner,
    span::Span,
    symbol::Symbol,
    syntax::ast,
    ty::{TyK, TyScheme},
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

pub fn intrinsics_interface(interner: &mut Interner) -> ast::ModuleInterface {
    let mid = ast::ModuleName::from_strings(vec!["rnrs", "intrinsics"]);
    let mut benv = Env::default();
    let mut tyenv = Env::default();
    arithmetic(mid, &mut benv, &mut tyenv, interner);
    logical(mid, &mut benv, &mut tyenv, interner);
    boolean(mid, &mut benv, &mut tyenv, interner);

    ast::ModuleInterface {
        id: mid,
        span: Span::dummy(),
        bindings: benv,
        types: Some(tyenv),
        dependencies: vec![],
    }
}

fn arithmetic(
    mid: ast::ModId,
    benv: &mut Env<Symbol, Binding>,
    tyenv: &mut Env<Symbol, TyScheme>,
    interner: &mut Interner,
) {
    let plus = "+".into();
    benv.insert(
        plus,
        Binding::Value {
            scopes: Scopes::core(),
            name: plus,
            orig_module: mid,
        },
    );
    tyenv.insert(
        plus,
        TyScheme::QTy(
            vec![],
            Rc::new(
                interner
                    .types
                    .alloc(TyK::Lambda {
                        params: vec![interner.builtins.types.i64, interner.builtins.types.i64],
                        rest: None,
                        ret: interner.builtins.types.i64,
                        generics: vec![],
                    })
                    .into(),
            ),
        ),
    );

    let minus = "-".into();
    benv.insert(
        minus,
        Binding::Value {
            scopes: Scopes::core(),
            name: minus,
            orig_module: mid,
        },
    );
    tyenv.insert(
        minus,
        interner
            .types
            .alloc(TyK::Lambda {
                params: vec![],
                rest: Some(interner.builtins.types.array_i64),
                ret: interner.builtins.types.i64,
                generics: vec![],
            })
            .into(),
    );

    let star = "*".into();
    benv.insert(
        star,
        Binding::Value {
            scopes: Scopes::core(),
            name: star,
            orig_module: mid,
        },
    );
    tyenv.insert(
        star,
        interner
            .types
            .alloc(TyK::Lambda {
                params: vec![],
                rest: Some(interner.builtins.types.array_i64),
                ret: interner.builtins.types.i64,
                generics: vec![],
            })
            .into(),
    );

    let slash = "/".into();
    benv.insert(
        slash,
        Binding::Value {
            scopes: Scopes::core(),
            name: slash,
            orig_module: mid,
        },
    );
    tyenv.insert(
        slash,
        interner
            .types
            .alloc(TyK::Lambda {
                params: vec![],
                rest: Some(interner.builtins.types.array_i64),
                ret: interner.builtins.types.i64,
                generics: vec![],
            })
            .into(),
    );
}

fn logical(
    mid: ast::ModId,
    benv: &mut Env<Symbol, Binding>,
    tyenv: &mut Env<Symbol, TyScheme>,
    interner: &mut Interner,
) {
    let eq = "=".into();
    benv.insert(
        eq,
        Binding::Value {
            scopes: Scopes::core(),
            name: eq,
            orig_module: mid,
        },
    );
    tyenv.insert(
        eq,
        interner
            .types
            .alloc(TyK::Lambda {
                params: vec![],
                rest: Some(interner.builtins.types.array_i64),
                ret: interner.builtins.types.i64,
                generics: vec![],
            })
            .into(),
    );

    let lt = "<".into();
    benv.insert(
        lt,
        Binding::Value {
            scopes: Scopes::core(),
            name: lt,
            orig_module: mid,
        },
    );
    tyenv.insert(
        lt,
        interner
            .types
            .alloc(TyK::Lambda {
                params: vec![],
                rest: Some(interner.builtins.types.array_i64),
                ret: interner.builtins.types.i64,
                generics: vec![],
            })
            .into(),
    );

    let lte = "<=".into();
    benv.insert(
        lte,
        Binding::Value {
            scopes: Scopes::core(),
            name: lte,
            orig_module: mid,
        },
    );
    tyenv.insert(
        lte,
        interner
            .types
            .alloc(TyK::Lambda {
                params: vec![],
                rest: Some(interner.builtins.types.array_i64),
                ret: interner.builtins.types.i64,
                generics: vec![],
            })
            .into(),
    );

    let gt = ">".into();
    benv.insert(
        gt,
        Binding::Value {
            scopes: Scopes::core(),
            name: gt,
            orig_module: mid,
        },
    );
    tyenv.insert(
        gt,
        interner
            .types
            .alloc(TyK::Lambda {
                params: vec![],
                rest: Some(interner.builtins.types.array_i64),
                ret: interner.builtins.types.i64,
                generics: vec![],
            })
            .into(),
    );

    let gte = ">=".into();
    benv.insert(
        gte,
        Binding::Value {
            scopes: Scopes::core(),
            name: gte,
            orig_module: mid,
        },
    );
    tyenv.insert(
        gte,
        interner
            .types
            .alloc(TyK::Lambda {
                params: vec![],
                rest: Some(interner.builtins.types.array_i64),
                ret: interner.builtins.types.i64,
                generics: vec![],
            })
            .into(),
    );
}

fn boolean(
    mid: ast::ModId,
    benv: &mut Env<Symbol, Binding>,
    tyenv: &mut Env<Symbol, TyScheme>,
    interner: &mut Interner,
) {
    let truthy = "not".into();
    benv.insert(
        truthy,
        Binding::Value {
            scopes: Scopes::core(),
            name: truthy,
            orig_module: mid,
        },
    );
    tyenv.insert(
        truthy,
        interner
            .types
            .alloc(TyK::Lambda {
                params: vec![interner.builtins.types.boolean],
                rest: None,
                ret: interner.builtins.types.boolean,
                generics: vec![],
            })
            .into(),
    );
    // TODO: Implement as macro
    let and = "and".into();
    benv.insert(
        and,
        Binding::Value {
            scopes: Scopes::core(),
            name: and,
            orig_module: mid,
        },
    );
    tyenv.insert(
        and,
        interner
            .types
            .alloc(TyK::Lambda {
                params: vec![
                    interner.builtins.types.boolean,
                    interner.builtins.types.boolean,
                ],
                rest: None,
                ret: interner.builtins.types.boolean,
                generics: vec![],
            })
            .into(),
    );
    // TODO: Implement as macro
    let or = "or".into();
    benv.insert(
        or,
        Binding::Value {
            scopes: Scopes::core(),
            name: or,
            orig_module: mid,
        },
    );
    tyenv.insert(
        or,
        interner
            .types
            .alloc(TyK::Lambda {
                params: vec![
                    interner.builtins.types.boolean,
                    interner.builtins.types.boolean,
                ],
                rest: None,
                ret: interner.builtins.types.boolean,
                generics: vec![],
            })
            .into(),
    );
}
