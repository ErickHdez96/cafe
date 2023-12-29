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

    pub fn file(&self, file_id: FileId) -> Option<Rc<SourceFile>> {
        self.store.borrow().get_file(file_id)
    }

    pub fn feed_file(&self, path: &path::Path) -> Res<FileId> {
        let contents = match std::fs::read_to_string(path) {
            Ok(c) => c,
            Err(e) => {
                return Err(Diagnostic::builder()
                    .msg(format!("{}: {e}", path.to_string_lossy()))
                    .finish());
            }
        };

        let source_file = SourceFile::new(path, contents);
        let fid = source_file.id;
        self.store
            .borrow_mut()
            .filemap
            .insert(source_file.id, Rc::new(source_file));
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

    pub fn compile_script(&self, path: impl AsRef<path::Path>) -> Res<Rc<Module>> {
        let mid = ModuleName::script();
        let fid = self.feed_file(path.as_ref())?;
        self.store.borrow_mut().module_file.insert(mid, fid);

        self.expand_module(mid)
    }
}

#[derive(Debug, Default)]
struct Store {
    filemap: HashMap<FileId, Rc<SourceFile>>,
    module_file: HashMap<ModId, FileId>,
    modules: HashMap<ModId, Rc<Module>>,
    module_interfaces: HashMap<ModId, Rc<ModuleInterface>>,
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
