use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    asm,
    codegen::codegen,
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
    symbol::Symbol,
    syntax::{
        ast,
        parser::{parse_str, ParseResult},
    },
    ty::{self, BuiltinTys},
    tyc::typecheck_module,
    utils::Resolve,
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

fn typecheck_id(libs: &Libs, mid: ast::ModId, builtin_tys: BuiltinTys) {
    let deps = {
        let mod_ = libs.get_mod(mid);
        if mod_.types.is_some() {
            return;
        }
        mod_.dependencies.clone()
    };
    for dep in deps {
        typecheck_id(libs, dep, builtin_tys.clone());
    }

    let mut store = libs.modules.borrow_mut();
    let mut module = Rc::get_mut(
        store
            .get_mut(&mid)
            .unwrap_or_else(|| panic!("expected modid: {:#?}", mid)),
    )
    .unwrap_or_else(|| panic!("module not mutable: {mid:#?}"));
    typecheck(libs, &mut module, builtin_tys);
}

fn typecheck(libs: &Libs, mod_: &mut ast::Module, builtin_tys: BuiltinTys) {
    let diags = typecheck_module(mod_, builtin_tys, |mid| {
        libs.import(mid).expect("unknown mid: {mid:#?}")
    });
    assert_eq!(Vec::<Diagnostic>::new(), diags);
    Rc::get_mut(libs.interfaces.borrow_mut().get_mut(&mod_.id).unwrap())
        .unwrap()
        .types = mod_.types.clone();
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

    let res = test_expand_str_with_libs(input, &libs);
    let mid = res.module.id;
    libs.define(mid, res.module);
    typecheck_id(&libs, mid, builtin_tys.clone());

    let intrinsics_mid = ast::ModuleName::from_strings(vec!["rnrs", "intrinsics"]);
    let store = libs.modules.borrow();
    let module = store.get(&mid).unwrap();

    match lower_ast(
        &module,
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

pub fn test_codegen_str(input: &str) -> Vec<asm::Inst> {
    codegen(test_lower_str(input))
}

pub struct Libs {
    modules: RefCell<HashMap<ast::ModId, Rc<ast::Module>>>,
    interfaces: RefCell<HashMap<ast::ModId, Rc<ast::ModuleInterface>>>,
}

impl Default for Libs {
    fn default() -> Self {
        let intrinsics = intrinsics_interface();
        let intrinsics_mid = intrinsics.id;
        let intrinsics = Rc::new(ast::Module {
            id: intrinsics_mid,
            span: Span::dummy(),
            dependencies: vec![],
            exports: intrinsics.bindings,
            bindings: Env::default(),
            body: ast::Expr::dummy(),
            types: Some(Env::default()),
        });
        let intrinsics_int = Rc::new(intrinsics.to_interface());

        let core = core_expander_interface();
        let core_mid = core.id;
        let core = Rc::new(ast::Module {
            id: core_mid,
            span: Span::dummy(),
            dependencies: vec![],
            exports: core.bindings,
            bindings: Env::default(),
            body: ast::Expr::dummy(),
            types: Some(Env::default()),
        });
        let core_int = Rc::new(core.to_interface());

        Self {
            modules: RefCell::new(HashMap::from([
                (core_mid, core),
                (intrinsics_mid, intrinsics),
            ])),
            interfaces: RefCell::new(HashMap::from([
                (core_mid, core_int),
                (intrinsics_mid, intrinsics_int),
            ])),
        }
    }
}

impl Libs {
    pub fn get_mod(&self, mid: ast::ModId) -> Rc<ast::Module> {
        self.modules
            .borrow()
            .get(&mid)
            .map(Rc::clone)
            .unwrap_or_else(|| panic!("{}", mid.resolve()))
    }

    pub fn import_mod(&self, mid: ast::ModId) -> Result<Rc<ast::Module>, Diagnostic> {
        Ok(self
            .modules
            .borrow()
            .get(&mid)
            .map(Rc::clone)
            .expect(&format!("{}", mid.resolve())))
    }

    pub fn import(&self, mid: ast::ModId) -> Result<Rc<ast::ModuleInterface>, Diagnostic> {
        Ok(self
            .interfaces
            .borrow()
            .get(&mid)
            .map(Rc::clone)
            .expect(&format!("{}", mid.resolve())))
    }

    pub fn define(&self, mid: ast::ModId, module: ast::Module) {
        self.interfaces
            .borrow_mut()
            .insert(mid, Rc::new(module.to_interface()));
        self.modules.borrow_mut().insert(mid, Rc::new(module));
    }
}
