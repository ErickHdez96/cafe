use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    diagnostics::Diagnostic,
    env::Env,
    expander::{
        binding::Binding, expand_module_with_config, intrinsics::intrinsics_env, ExpanderConfig,
        ExpanderResultMod,
    },
    ir,
    lower_ast::lower_ast,
    rnrs::{core_expander_interface, intrinsics_interface},
    span::Span,
    syntax::{
        ast,
        parser::{parse_str, ParseResult},
    },
    ty::{self, BuiltinTys},
    tyc::typecheck_module,
    utils::{Resolve, Symbol},
};

pub fn rnrs_env() -> Env<'static, Symbol, Binding> {
    let mut env = intrinsics_env().enter_consume();
    env.set_bindings(core_expander_interface().bindings.into_bindings());
    env = env.enter_consume();
    env.set_bindings(intrinsics_interface().bindings.into_bindings());
    env
}

pub fn test_parse_str(input: &str) -> ParseResult {
    let res = parse_str(input);
    assert_eq!(res.diagnostics, vec![]);
    res
}

pub fn test_expand_str(input: &str) -> ExpanderResultMod {
    let libs = Libs::default();
    test_expand_str_with_libs(input, &libs)
}

fn typecheck(
    libs: &Libs,
    mod_: &mut ast::Module,
    builtin_tys: BuiltinTys,
) -> Env<'static, String, Rc<ty::Ty>> {
    let (res, diags) = typecheck_module(mod_, builtin_tys, |mid| {
        libs.import(mid).expect("unknown mid: {mid:#?}")
    });
    assert_eq!(Vec::<Diagnostic>::new(), diags);
    res
}

pub fn test_expand_str_with_libs(input: &str, libs: &Libs) -> ExpanderResultMod {
    let res = test_parse_str(input);
    let env = rnrs_env();

    let res = expand_module_with_config(
        res.root,
        ExpanderConfig::default()
            .import(&|mid| libs.import(mid))
            .register(&|mid, mod_| libs.define(mid, mod_))
            .file_id(res.file_id)
            .base_env(&env),
    );

    assert_eq!(res.diagnostics, vec![]);
    res
}

pub fn test_lower_str(input: &str) -> ir::Package {
    let libs = Libs::default();
    let builtin_tys = ty::BuiltinTys::default();

    let mut res = test_expand_str_with_libs(input, &libs);
    let env = typecheck(&libs, &mut res.module, builtin_tys.clone());
    res.module.types = Some(env);
    let intrinsics_mid = ast::ModuleName::from_strings(vec!["rnrs", "intrinsics"]);
    match lower_ast(
        &res.module,
        intrinsics_mid,
        &|mid| libs.import_mod(mid),
        builtin_tys,
    ) {
        Ok(p) => p,
        Err(d) => {
            assert_eq!(Vec::<Diagnostic>::new(), d);
            unreachable!();
        }
    }
}

pub struct Libs {
    libs: RefCell<HashMap<ast::ModId, Rc<ast::Module>>>,
}

impl Default for Libs {
    fn default() -> Self {
        let intrinsics = intrinsics_interface();
        let intrinsics_mid = intrinsics.id;

        let core = core_expander_interface();
        let core_mid = core.id;

        Self {
            libs: RefCell::new(HashMap::from([
                (
                    core_mid,
                    Rc::new(ast::Module {
                        id: core_mid,
                        span: Span::dummy(),
                        dependencies: vec![],
                        exports: core.bindings,
                        bindings: Env::default(),
                        body: ast::Expr::dummy(),
                        types: None,
                    }),
                ),
                (
                    intrinsics_mid,
                    Rc::new(ast::Module {
                        id: intrinsics_mid,
                        span: Span::dummy(),
                        dependencies: vec![],
                        exports: intrinsics.bindings,
                        bindings: Env::default(),
                        body: ast::Expr::dummy(),
                        types: None,
                    }),
                ),
            ])),
        }
    }
}

impl Libs {
    pub fn import_mod(&self, mid: ast::ModId) -> Result<Rc<ast::Module>, Diagnostic> {
        Ok(self
            .libs
            .borrow()
            .get(&mid)
            .map(Rc::clone)
            .expect(&format!("{}", mid.resolve())))
    }

    pub fn import(&self, mid: ast::ModId) -> Result<Rc<ast::ModuleInterface>, Diagnostic> {
        Ok(Rc::new(
            self.libs
                .borrow()
                .get(&mid)
                .expect(&format!("{}", mid.resolve()))
                .to_interface(),
        ))
    }

    pub fn define(&self, mid: ast::ModId, module: ast::Module) {
        self.libs.borrow_mut().insert(mid, Rc::new(module));
    }
}
