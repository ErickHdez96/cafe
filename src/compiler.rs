#![allow(dead_code, unused_variables)]
use std::{cell::RefCell, collections::HashMap, path, rc::Rc};

use crate::{
    asm, codegen,
    config::CompilerConfig,
    diagnostics::Diagnostic,
    env::Env,
    expander::{
        binding::Binding, expand_module_with_config, intrinsics::intrinsics_env, ExpanderConfig,
        ExpanderResult, ExpanderResultMod,
    },
    file::{FileId, SourceFile},
    interner::Interner,
    ir, lower_ast,
    rnrs::{core_expander_interface, intrinsics_interface},
    symbol::Symbol,
    syntax::{
        ast,
        cst::Cst,
        parser::{parse_source_file, ParseResult},
    },
    tyc::typecheck_module,
};

pub type Res<T> = Result<T, Diagnostic>;

#[derive(Debug)]
pub struct Compiler {
    pub config: CompilerConfig,
    store: RefCell<CompilerStore>,
    interner: Interner,
    env: Env<'static, Symbol, Binding>,
    diagnostics: RefCell<Vec<Diagnostic>>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        let mut interner = Interner::default();
        let intrinsics = intrinsics_interface(&mut interner);
        let s = Self {
            config: CompilerConfig::default(),
            store: RefCell::default(),
            interner,
            env: intrinsics_env(),
            diagnostics: RefCell::default(),
        };
        s.feed_module(core_expander_interface());
        s.feed_module(intrinsics);
        s
    }

    pub const fn interner(&self) -> &Interner {
        &self.interner
    }

    fn import_module_id(&self, mid: ast::ModId) -> Res<Rc<ast::ModuleInterface>> {
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

    fn register_module(&self, mid: ast::ModId, module: ast::Module) {
        self.store
            .borrow_mut()
            .module_interfaces
            .insert(mid, Rc::new(module.to_interface()));
        self.store.borrow_mut().modules.insert(mid, Rc::new(module));
    }

    fn resolve_module_name(&self, mid: ast::ModId) -> Res<FileId> {
        match self.get_module_file(mid) {
            Some(fid) => Ok(fid),
            None => {
                let mut path = std::env::current_dir().unwrap();
                let mod_paths = mid
                    .resolve()
                    .paths
                    .iter()
                    .copied()
                    .map(Symbol::resolve)
                    .collect::<Vec<_>>()
                    .join(std::path::MAIN_SEPARATOR_STR);
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

    fn module_deps(&self, mid: ast::ModId) -> Vec<ast::ModId> {
        self.get_mod_interface(mid)
            .map(|i| i.dependencies.clone())
            .unwrap()
    }

    pub fn file(&self, file_id: FileId) -> Option<Rc<SourceFile>> {
        self.get_file(file_id)
    }

    pub fn add_source(&self, path: impl AsRef<path::Path>, modname: &[&str]) -> Res<FileId> {
        let mid = ast::ModuleName {
            paths: modname.as_ref().iter().map(Symbol::intern).collect(),
            versions: vec![],
        }
        .intern();
        self.feed_file(mid, path)
    }

    pub fn feed_file(&self, mid: ast::ModId, path: impl AsRef<path::Path>) -> Res<FileId> {
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
        mid: ast::ModId,
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

    pub fn feed_module(&self, mod_interface: ast::ModuleInterface) {
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

    pub fn pass_typecheck(&mut self, mid: ast::ModId) {
        let mut store = self.store.borrow_mut();
        let mod_ =
            Rc::get_mut(store.modules.get_mut(&mid).unwrap()).expect("module should be unique");

        let diags = typecheck_module(
            mod_,
            &mut self.interner.types,
            &self.interner.builtins.types,
            |mid| {
                self.store
                    .borrow()
                    .module_interfaces
                    .get(&mid)
                    .map(Rc::clone)
                    .unwrap()
            },
        );
        self.diagnostics.borrow_mut().extend(diags);
    }

    pub fn expand_module(&self, mid: ast::ModId) -> Res<Rc<ast::Module>> {
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
                    paths.push(Symbol::from("#script"));
                    ast::ModuleName::from_strings(paths)
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

    fn run_typecheck_module(&mut self, mid: ast::ModId) -> Res<()> {
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

        self.pass_typecheck(mid);

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

    pub fn typecheck_module(&mut self, mid: ast::ModId) -> Res<Rc<ast::Module>> {
        self.run_typecheck_module(mid)?;
        self.get_mod(mid).ok_or_else(|| panic!("expected module"))
    }

    fn lower_module(&mut self, mid: ast::ModId) -> ir::Package {
        lower_ast::lower_ast(
            &self.get_mod(mid).expect("module"),
            &|mid| {
                self.get_mod(mid)
                    .map(|m| m.into())
                    .unwrap_or_else(|| self.import_module_id(mid).unwrap().into())
            },
            &self.interner,
        )
    }

    fn codegen(&mut self, ir: ir::Package) -> asm::Insts {
        codegen::codegen(ir, &self.interner.types)
    }

    pub fn compile_file(&mut self, path: impl AsRef<path::Path>) -> Res<asm::Insts> {
        let path = path.as_ref();
        let mid = ast::ModuleName {
            paths: vec![path.file_name().unwrap().to_string_lossy().as_ref().into()],
            versions: vec![],
        }
        .intern();
        self.feed_file(mid, path)?;
        self.run_typecheck_module(mid)?;
        let pkg = self.lower_module(mid);
        Ok(self.codegen(pkg))
    }

    fn get_mod(&self, mid: ast::ModId) -> Option<Rc<ast::Module>> {
        self.store.borrow().get_mod(mid)
    }

    fn get_mod_interface(&self, mid: ast::ModId) -> Option<Rc<ast::ModuleInterface>> {
        self.store.borrow().get_mod_interface(mid)
    }

    fn get_file(&self, fid: FileId) -> Option<Rc<SourceFile>> {
        self.store.borrow().get_file(fid)
    }

    fn get_module_file(&self, mid: ast::ModId) -> Option<FileId> {
        self.store.borrow().get_module_file(mid)
    }
}

#[derive(Debug, Default)]
struct CompilerStore {
    filemap: HashMap<FileId, Rc<SourceFile>>,
    module_file: HashMap<ast::ModId, FileId>,
    modules: HashMap<ast::ModId, Rc<ast::Module>>,
    module_interfaces: HashMap<ast::ModId, Rc<ast::ModuleInterface>>,
}

impl CompilerStore {
    fn get_file(&self, fid: FileId) -> Option<Rc<SourceFile>> {
        self.filemap.get(&fid).map(Rc::clone)
    }

    fn get_module_file(&self, mid: ast::ModId) -> Option<FileId> {
        self.module_file.get(&mid).copied()
    }

    fn get_mod(&self, mid: ast::ModId) -> Option<Rc<ast::Module>> {
        self.modules.get(&mid).map(Rc::clone)
    }

    fn get_mod_interface(&self, mid: ast::ModId) -> Option<Rc<ast::ModuleInterface>> {
        self.module_interfaces.get(&mid).map(Rc::clone)
    }
}
