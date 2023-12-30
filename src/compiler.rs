use std::{cell::RefCell, collections::HashMap, path, rc::Rc};

use crate::{
    config::CompilerConfig,
    diagnostics::Diagnostic,
    env::Env,
    expander::{core_expander_interface, expand_root, Binding, ExpanderResult},
    file::{FileId, SourceFile},
    syntax::{
        ast::{ModId, Module, ModuleInterface, ModuleName},
        cst::SynRoot,
        parser::{parse_source_file, ParseResult},
    },
    ty::{BuiltinTys, Ty},
    tyc::typecheck_module,
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
            env: Env::default(),
            diagnostics: RefCell::default(),
        };
        s.feed_module(core_expander_interface());
        s
    }

    fn import_module_id(&self, mid: ModId) -> Res<Rc<ModuleInterface>> {
        match self.store.borrow().get_mod_interface(mid) {
            Some(m) => Ok(m),
            None => todo!(),
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
        match self.store.borrow().get_module_file(mid) {
            Some(fid) => Ok(fid),
            None => todo!(),
        }
    }

    fn module_deps(&self, mid: ModId) -> Vec<ModId> {
        self.store
            .borrow()
            .get_mod_interface(mid)
            .map(|i| i.dependencies.clone())
            .unwrap()
    }

    pub fn file(&self, file_id: FileId) -> Option<Rc<SourceFile>> {
        self.store.borrow().get_file(file_id)
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
        parse_source_file(
            &self.store.borrow().get_file(file_id).unwrap(),
            self.config.parser,
        )
    }

    pub fn pass_expand(&self, root: SynRoot) -> ExpanderResult {
        expand_root(
            root,
            &self.env,
            |mid| self.import_module_id(mid),
            |mid, m| self.register_module(mid, m),
        )
    }

    pub fn pass_typecheck(&self, mod_: &Module) -> Env<'static, String, Rc<Ty>> {
        let (res, diags) = typecheck_module(mod_, self.store.borrow().builtin_tys.clone(), |mid| {
            self.store
                .borrow()
                .module_interfaces
                .get(&mid)
                .map(Rc::clone)
                .unwrap()
        });
        self.diagnostics.borrow_mut().extend(diags);
        res
    }

    pub fn expand_module(&self, mid: ModId) -> Res<Rc<Module>> {
        let fid = self.resolve_module_name(mid)?;
        let pres = self.pass_parse(fid);
        self.diagnostics.borrow_mut().extend(pres.diagnostics);
        let synroot = SynRoot::new(&pres.tree, pres.file_id);
        let res = self.pass_expand(synroot);
        self.diagnostics.borrow_mut().extend(res.diagnostics);
        {
            let mut store = self.store.borrow_mut();
            store
                .module_interfaces
                .insert(mid, Rc::new(res.module.to_interface()));
            store.modules.insert(mid, Rc::new(res.module));
        }
        Ok(self.store.borrow().get_mod(mid).unwrap())
    }

    pub fn typecheck_module(&self, mid: ModId) -> Res<Rc<Module>> {
        let env = {
            let module = self.expand_module(mid)?;
            self.pass_typecheck(&module)
        };
        Rc::get_mut(
            self.store
                .borrow_mut()
                .module_interfaces
                .get_mut(&mid)
                .unwrap(),
        )
        .unwrap()
        .types = Some(env.clone());
        Rc::get_mut(self.store.borrow_mut().modules.get_mut(&mid).unwrap())
            .unwrap()
            .types = Some(env);
        Ok(self.store.borrow().get_mod(mid).unwrap())
    }

    pub fn compile_script(&self, path: impl AsRef<path::Path>) -> Res<Rc<Module>> {
        let mid = ModuleName::script();
        self.feed_file(mid, path.as_ref())?;
        let m = self.expand_module(mid);

        for dep in self.module_deps(mid) {
            let env = self.pass_typecheck(&self.store.borrow().get_mod(dep).unwrap());
            Rc::get_mut(
                self.store
                    .borrow_mut()
                    .module_interfaces
                    .get_mut(&dep)
                    .unwrap(),
            )
            .unwrap()
            .types = Some(env);
        }

        m
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
