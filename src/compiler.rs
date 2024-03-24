#![allow(dead_code, unused_variables)]
use std::{cell::RefCell, collections::HashMap, path, rc::Rc};

use crate::{
    config::CompilerConfig,
    diagnostics::Diagnostic,
    env::Env,
    expander::{
        binding::Binding, expand_module_with_config, intrinsics::intrinsics_env, ExpanderConfig,
        ExpanderResult, ExpanderResultMod,
    },
    file::{FileId, SourceFile},
    rnrs::{core_expander_interface, intrinsics_interface},
    syntax::{
        ast::{ModId, Module, ModuleInterface, ModuleName},
        cst::Cst,
        parser::{parse_source_file, ParseResult},
    },
    ty::BuiltinTys,
    tyc::typecheck_module,
    utils::Resolve,
};

pub type Res<T> = Result<T, Diagnostic>;

#[derive(Debug)]
pub struct Compiler {
    pub config: CompilerConfig,
    store: RefCell<Store>,
    env: Env<'static, String, Binding>,
    diagnostics: RefCell<Vec<Diagnostic>>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        let s = Self {
            config: CompilerConfig::default(),
            store: RefCell::default(),
            env: intrinsics_env(),
            diagnostics: RefCell::default(),
        };
        s.feed_module(core_expander_interface());
        s.feed_module(intrinsics_interface());
        s
    }

    fn import_module_id(&self, mid: ModId) -> Res<Rc<ModuleInterface>> {
        match self.get_mod_interface(mid) {
            Some(m) => Ok(m),
            None => {
                let mod_ = self.expand_module(mid)?;
                self.feed_module(mod_.to_interface());
                self.get_mod_interface(mid)
                    .ok_or_else(|| panic!("should have stored module interface"))
            }
        }
    }

    fn register_module(&self, mid: ModId, module: Module) {
        self.store
            .borrow_mut()
            .module_interfaces
            .insert(mid, Rc::new(module.to_interface()));
        self.store.borrow_mut().modules.insert(mid, Rc::new(module));
    }

    fn resolve_module_name(&self, mid: ModId) -> Res<FileId> {
        match self.get_module_file(mid) {
            Some(fid) => Ok(fid),
            None => {
                let mut path = std::env::current_dir().unwrap();
                let mod_paths = mid.resolve().paths.join(std::path::MAIN_SEPARATOR_STR);
                path.push(mod_paths);
                path.set_extension("scm");
                if path.exists() {
                    self.feed_file(mid, path)
                } else {
                    todo!(
                        "tried to resolve unknown module {} - {}",
                        mid.resolve(),
                        path.to_string_lossy()
                    )
                }
            }
        }
    }

    fn module_deps(&self, mid: ModId) -> Vec<ModId> {
        self.get_mod_interface(mid)
            .map(|i| i.dependencies.clone())
            .unwrap()
    }

    pub fn file(&self, file_id: FileId) -> Option<Rc<SourceFile>> {
        self.get_file(file_id)
    }

    pub fn feed_file(&self, mid: ModId, path: impl AsRef<path::Path>) -> Res<FileId> {
        let path = path.as_ref();
        let contents = match std::fs::read_to_string(path) {
            Ok(c) => c,
            Err(e) => {
                return Err(Diagnostic::builder()
                    .msg(format!("{}: {e}", path.to_string_lossy()))
                    .finish());
            }
        };
        self.feed_file_contents(mid, path, contents)
    }

    pub fn feed_file_contents(
        &self,
        mid: ModId,
        path: impl AsRef<path::Path>,
        contents: impl Into<String>,
    ) -> Res<FileId> {
        let source_file = SourceFile::new(path.as_ref(), contents.into());
        let fid = source_file.id;
        let mut store = self.store.borrow_mut();
        store.filemap.insert(source_file.id, Rc::new(source_file));
        store.module_file.insert(mid, fid);
        Ok(fid)
    }

    pub fn feed_module(&self, mod_interface: ModuleInterface) {
        self.store
            .borrow_mut()
            .module_interfaces
            .insert(mod_interface.id, Rc::new(mod_interface));
    }

    pub fn take_diagnostics(&self) -> Vec<Diagnostic> {
        std::mem::take(&mut self.diagnostics.borrow_mut())
    }

    pub fn pass_parse(&self, file_id: FileId) -> ParseResult {
        parse_source_file(&self.get_file(file_id).unwrap(), self.config.parser)
    }

    pub fn pass_expand(&self, root: Vec<Rc<Cst>>, file_id: FileId) -> ExpanderResult {
        ExpanderResult::Mod(expand_module_with_config(
            root,
            ExpanderConfig::default()
                .import(&|mid| self.import_module_id(mid))
                .register(&|mid, mod_| self.register_module(mid, mod_))
                .base_env(&self.env)
                .file_id(file_id),
        ))
    }

    pub fn pass_typecheck(&self, mod_: &mut Module, builtin_tys: BuiltinTys) {
        let diags = typecheck_module(mod_, builtin_tys, |mid| {
            self.store
                .borrow()
                .module_interfaces
                .get(&mid)
                .map(Rc::clone)
                .unwrap()
        });
        self.diagnostics.borrow_mut().extend(diags);
    }

    pub fn expand_module(&self, mid: ModId) -> Res<Rc<Module>> {
        if let Some(mod_) = self.get_mod(mid) {
            return Ok(mod_);
        }

        let fid = self.resolve_module_name(mid)?;
        let pres = self.pass_parse(fid);
        self.diagnostics.borrow_mut().extend(pres.diagnostics);
        let res = self.pass_expand(pres.root, fid);
        match res {
            ExpanderResult::Mod(ExpanderResultMod {
                diagnostics,
                module,
                ..
            }) => {
                self.diagnostics.borrow_mut().extend(diagnostics);
                let mid = if self.get_mod(mid).is_some() {
                    let mut paths = mid.resolve().paths.clone();
                    paths.push(String::from("#script"));
                    ModuleName::from_strings(paths)
                } else {
                    mid
                };
                let mut store = self.store.borrow_mut();
                store
                    .module_interfaces
                    .insert(mid, Rc::new(module.to_interface()));
                store.modules.insert(mid, Rc::new(module));
            }
            ExpanderResult::Items { diagnostics, .. } => {
                self.diagnostics.borrow_mut().extend(diagnostics);
            }
        }
        Ok(self.get_mod(mid).unwrap())
    }

    fn run_typecheck_module(&self, mid: ModId) -> Res<()> {
        if let Some(mod_) = self.get_mod(mid) {
            if mod_.types.is_some() {
                return Ok(());
            }
        }

        if let Some(imod) = self.get_mod_interface(mid) {
            if imod.types.is_some() {
                return Ok(());
            }
        }

        {
            let module = self.expand_module(mid)?;
            for dep in &module.dependencies {
                let _ = self.run_typecheck_module(*dep);
            }
        }

        {
            let mut store = self.store.borrow_mut();
            let builtin_tys = store.builtin_tys.clone();
            self.pass_typecheck(
                Rc::get_mut(store.modules.get_mut(&mid).unwrap()).expect("module should be unique"),
                builtin_tys,
            );
        }

        let env = self.store.borrow().get_mod(mid).unwrap().types.clone();
        Rc::get_mut(
            self.store
                .borrow_mut()
                .module_interfaces
                .get_mut(&mid)
                .unwrap(),
        )
        .unwrap()
        .types = env;
        Ok(())
    }

    pub fn typecheck_module(&self, mid: ModId) -> Res<Rc<Module>> {
        self.run_typecheck_module(mid)?;
        self.get_mod(mid).ok_or_else(|| panic!("expected module"))
    }

    pub fn compile_script(&self, path: impl AsRef<path::Path>) -> Res<Rc<Module>> {
        let mid = ModuleName::script();
        self.feed_file(mid, path.as_ref())?;
        self.typecheck_module(mid)
        //let m = self.expand_module(mid);

        //for dep in self.module_deps(mid) {
        //    let env = self.pass_typecheck(
        //        &self.store.borrow().get_mod(dep).unwrap()
        //    );
        //    Rc::get_mut(
        //        self.store
        //            .borrow_mut()
        //            .module_interfaces
        //            .get_mut(&dep)
        //            .unwrap(),
        //    )
        //    .unwrap()
        //    .types = Some(env);
        //}

        //m
    }

    fn get_mod(&self, mid: ModId) -> Option<Rc<Module>> {
        self.store.borrow().get_mod(mid)
    }

    fn get_mod_interface(&self, mid: ModId) -> Option<Rc<ModuleInterface>> {
        self.store.borrow().get_mod_interface(mid)
    }

    fn get_file(&self, fid: FileId) -> Option<Rc<SourceFile>> {
        self.store.borrow().get_file(fid)
    }

    fn get_module_file(&self, mid: ModId) -> Option<FileId> {
        self.store.borrow().get_module_file(mid)
    }
}

#[derive(Debug, Default)]
struct Store {
    filemap: HashMap<FileId, Rc<SourceFile>>,
    module_file: HashMap<ModId, FileId>,
    modules: HashMap<ModId, Rc<Module>>,
    module_interfaces: HashMap<ModId, Rc<ModuleInterface>>,
    builtin_tys: BuiltinTys,
}

impl Store {
    fn get_file(&self, fid: FileId) -> Option<Rc<SourceFile>> {
        self.filemap.get(&fid).map(Rc::clone)
    }

    fn get_module_file(&self, mid: ModId) -> Option<FileId> {
        self.module_file.get(&mid).copied()
    }

    fn get_mod(&self, mid: ModId) -> Option<Rc<Module>> {
        self.modules.get(&mid).map(Rc::clone)
    }

    fn get_mod_interface(&self, mid: ModId) -> Option<Rc<ModuleInterface>> {
        self.module_interfaces.get(&mid).map(Rc::clone)
    }
}
