use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    asm::{self, ISA},
    codegen::codegen,
    config::Language,
    diagnostics::Diagnostic,
    env::Env,
    expander::{
        binding::Binding, expand_module_with_config, intrinsics::intrinsics_env, ExpanderConfig,
        ExpanderResultMod,
    },
    interner::Interner,
    ir,
    lower_ast::lower_ast,
    rnrs::{core_expander_interface, intrinsics_interface},
    span::Span,
    symbol::Symbol,
    syntax::{
        ast,
        parser::{parse_str, ParseResult},
    },
    tyc::typecheck_module,
};

#[derive(Debug)]
pub struct Tester<'a> {
    language: Language,
    input: &'a str,
    interner: Option<&'a mut Interner>,
    libs: Option<Libs>,
}

impl<'a> Tester<'a> {
    pub fn with_input(input: &'a str) -> Self {
        Self {
            language: Language::default(),
            input,
            interner: None,
            libs: None,
        }
    }

    pub fn interner(mut self, interner: &'a mut Interner) -> Self {
        self.interner = Some(interner);
        self
    }

    fn assert_interner(&self) {
        assert!(self.interner.is_some(), "Tester needs an interner");
    }

    fn assert_libs(&mut self) {
        if self.libs.is_none() {
            self.libs = Some(Libs::new(
                self.interner.as_mut().expect("Tester needs an interner"),
            ));
        }
    }

    pub fn test_expand_str(&mut self) -> ExpanderResultMod {
        self.assert_interner();
        self.assert_libs();
        let input = self.input;
        let interner = self.interner.as_mut().unwrap();
        let libs = self.libs.as_ref().unwrap();

        let res = test_parse_str(input);
        let env = rnrs_env(interner);

        let res = expand_module_with_config(
            res.root,
            ExpanderConfig::default()
                .language(self.language)
                .import(&|mid| libs.import(mid))
                .register(&|mid, mod_| libs.define(mid, mod_))
                .file_id(res.file_id)
                .base_env(&env),
        );

        assert_eq!(res.diagnostics, vec![]);
        res
    }
}

pub fn rnrs_env(interner: &mut Interner) -> Env<'static, Symbol, Binding> {
    let mut env = intrinsics_env().enter_consume();
    env.set_bindings(core_expander_interface().bindings.into_bindings());
    env = env.enter_consume();
    env.set_bindings(intrinsics_interface(interner).bindings.into_bindings());
    env
}

pub fn test_parse_str(input: &str) -> ParseResult {
    let res = parse_str(input);
    assert_eq!(res.diagnostics, vec![]);
    res
}

pub fn typecheck_id(libs: &Libs, mid: ast::ModId, interner: &mut Interner) {
    let deps = {
        let mod_ = libs.get_mod(mid);
        if mod_.types.is_some() {
            return;
        }
        mod_.dependencies.clone()
    };
    for dep in deps {
        typecheck_id(libs, dep, interner);
    }

    let mut store = libs.modules.borrow_mut();
    let mut module = Rc::get_mut(
        store
            .get_mut(&mid)
            .unwrap_or_else(|| panic!("expected modid: {:#?}", mid)),
    )
    .unwrap_or_else(|| panic!("module not mutable: {mid:#?}"));
    typecheck(libs, &mut module, interner);
}

fn typecheck(libs: &Libs, mod_: &mut ast::Module, interner: &mut Interner) {
    let diags = typecheck_module(mod_, &mut interner.types, &interner.builtins.types, |mid| {
        libs.import(mid).expect("unknown mid: {mid:#?}")
    });
    assert_eq!(Vec::<Diagnostic>::new(), diags);
    Rc::get_mut(
        libs.interfaces
            .borrow_mut()
            .get_mut(&mod_.id)
            .unwrap_or_else(|| {
                panic!(
                    "trying to get mutable reference to module: {:#?}",
                    mod_.id.resolve()
                )
            }),
    )
    .unwrap()
    .types = mod_.types.clone();
}

pub fn test_expand_str_with_libs(
    input: &str,
    libs: &Libs,
    interner: &mut Interner,
) -> ExpanderResultMod {
    let res = test_parse_str(input);
    let env = rnrs_env(interner);

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

pub fn test_lower_str(input: &str, interner: &mut Interner) -> ir::Package {
    let libs = Libs::new(interner);

    let res = test_expand_str_with_libs(input, &libs, interner);
    let mid = res.module.id;
    libs.define(mid, res.module);
    typecheck_id(&libs, mid, interner);

    let store = libs.modules.borrow();
    let module = store.get(&mid).unwrap();

    lower_ast(
        &module,
        &|mid| libs.import_mod(mid).unwrap().into(),
        &interner,
    )
}

pub fn test_codegen_str<I: ISA>(input: &str, isa: &I, interner: &mut Interner) -> asm::Insts {
    codegen(test_lower_str(input, interner), isa, &mut interner.types)
}

#[derive(Debug)]
pub struct Libs {
    pub modules: RefCell<HashMap<ast::ModId, Rc<ast::Module>>>,
    pub interfaces: RefCell<HashMap<ast::ModId, Rc<ast::ModuleInterface>>>,
}

impl Libs {
    pub fn new(interner: &mut Interner) -> Self {
        let intrinsics = intrinsics_interface(interner);
        let intrinsics_mid = intrinsics.id;
        let intrinsics = Rc::new(ast::Module {
            id: intrinsics_mid,
            span: Span::dummy(),
            dependencies: vec![],
            exports: intrinsics.bindings,
            bindings: Env::default(),
            body: ast::Expr::dummy(),
            types: intrinsics.types,
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
