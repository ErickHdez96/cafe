use std::{cell::RefCell, collections::HashMap, path::PathBuf, rc::Rc};

use crate::{
    config::CompilerConfig,
    diagnostics::Diagnostic,
    env::Env,
    expander::{expand_root, Binding, ExpanderResult},
    file::{FileId, SourceFile},
    syntax::{
        ast::{ModId, Module, ModuleInterface},
        cst::SynRoot,
        parser::{parse_source_file, ParseResult},
    },
    tyc::typecheck_module,
    utils::Resolve,
};

use super::{QCtx, Res};

pub fn set_compiler_config(config: CompilerConfig) {
    COMPILER_CONFIG.with(|c| *c.borrow_mut() = Rc::new(config));
}

pub fn compiler_config_provider(_: &QCtx, _: ()) -> Rc<CompilerConfig> {
    COMPILER_CONFIG.with(|c| Rc::clone(&c.borrow()))
}

pub fn set_root_binding_env(env: Env<'static, String, Binding>) {
    ROOT_BINDING_ENV.with(|e| *e.borrow_mut() = Rc::new(env));
}

pub fn root_binding_env_provider(_: &QCtx, _: ()) -> Rc<Env<'static, String, Binding>> {
    ROOT_BINDING_ENV.with(|e| Rc::clone(&e.borrow()))
}

pub fn register_intrinsic_lib(lib: ModuleInterface) {
    INTRINSIC_LIBRARIES.with(|i| i.borrow_mut().insert(lib.id, Rc::new(lib)));
}

pub fn register_lib((name, lib): (ModId, Module)) {
    REGISTERED_LIBRARIES.with(|i| i.borrow_mut().insert(name, Rc::new(lib)));
}

pub fn register_module_name((name, path): (ModId, PathBuf)) {
    REGISTERED_MODULE_NAMES.with(|m| m.borrow_mut().insert(name, path));
}

pub fn register_source_file(source_file: SourceFile) {
    PATH_MAPPING.with(|map| {
        map.borrow_mut()
            .insert(source_file.path.clone(), source_file.id)
    });
    FILE_MAPPING.with(|map| {
        map.borrow_mut()
            .insert(source_file.id, Rc::new(source_file))
    });
}

pub fn read_file_provider(_: &QCtx, path: PathBuf) -> Res<Rc<SourceFile>> {
    let id = PATH_MAPPING.with(|map| map.borrow().get(&path).cloned());

    if let Some(id) = id {
        let file = FILE_MAPPING.with(|map| map.borrow().get(&id).cloned());
        if let Some(file) = file {
            return Ok(file);
        }
    }

    match std::fs::read_to_string(&path) {
        Ok(contents) => {
            let file = Rc::new(SourceFile::new(path, contents));
            FILE_MAPPING.with(|map| map.borrow_mut().insert(file.id, Rc::clone(&file)));
            Ok(file)
        }
        Err(e) => Err(Diagnostic::builder()
            .msg(format!("{}: {e}", path.to_string_lossy()))
            .finish()),
    }
}

pub fn file_provider(_: &QCtx, id: FileId) -> Option<Rc<SourceFile>> {
    FILE_MAPPING.with(|map| map.borrow_mut().get(&id).cloned())
}

pub fn parse_provider(qctx: &QCtx, path: PathBuf) -> Res<ParseResult> {
    let config = qctx.compiler_config(());
    let source_file = qctx.read_file(path)?;
    Ok(parse_source_file(&source_file, config.parser))
}

pub fn lookup_module_name_provider(_qctx: &QCtx, mid: ModId) -> Res<PathBuf> {
    if let Some(path) = REGISTERED_MODULE_NAMES.with(|m| m.borrow().get(&mid).cloned()) {
        Ok(path)
    } else {
        todo!()
    }
}

pub fn module_interface_provider(_qctx: &QCtx, mid: ModId) -> Res<Rc<ModuleInterface>> {
    let lib = INTRINSIC_LIBRARIES.with(|i| i.borrow().get(&mid).map(Rc::clone));
    if let Some(intrinsics_lib) = lib {
        Ok(intrinsics_lib)
    } else {
        let registered_lib = REGISTERED_LIBRARIES.with(|i| i.borrow().get(&mid).map(Rc::clone));
        if let Some(registered_lib) = registered_lib {
            Ok(Rc::new(registered_lib.to_interface()))
        } else {
            Err(Diagnostic::builder()
                .msg(format!("module not found: {}", mid.resolve()))
                .finish())
        }
    }
}

// TODO: use modid?
pub fn expand_provider(qctx: &QCtx, mid: ModId) -> Res<Rc<ExpanderResult>> {
    let pathbuf = qctx.lookup_module_name(mid)?;
    let mut parse_res = qctx.parse(pathbuf)?;
    let syn = SynRoot::new(&parse_res.tree, parse_res.file_id);
    let root_binding_env = qctx.root_binding_env(());
    let mut expander_res = expand_root(
        syn,
        root_binding_env.as_ref(),
        |mid| qctx.module_interface(mid),
        |mname, module| register_lib((mname, module)),
    );
    parse_res
        .diagnostics
        .extend(std::mem::take(&mut expander_res.diagnostics));
    expander_res.diagnostics = parse_res.diagnostics;
    Ok(Rc::new(expander_res))
}

// TODO: move diagnostics to a central place. CompilerSession
pub fn typeck_provider(qctx: &QCtx, mod_id: ModId) -> Vec<Diagnostic> {
    let mut diags = vec![];
    let module = match qctx.expand(mod_id) {
        Ok(m) => m,
        Err(d) => return vec![d],
    };
    diags.extend_from_slice(&module.diagnostics);
    // Move somewhere else?
    match qctx.dependencies(mod_id) {
        Ok(deps) => {
            for dep in deps {
                diags.extend(qctx.typeck(dep));
            }
        }
        Err(d) => {
            diags.push(d);
        }
    }
    diags.extend(typecheck_module(&module.module));
    diags
}

pub fn dependencies_provider(qctx: &QCtx, mod_id: ModId) -> Res<Vec<ModId>> {
    qctx.module_interface(mod_id)
        .map(|m| m.dependencies.clone())
}

// TODO: Simplify the inputs
thread_local! {
    static FILE_MAPPING: RefCell<HashMap<FileId, Rc<SourceFile>>> = RefCell::default();
    static PATH_MAPPING: RefCell<HashMap<PathBuf, FileId>> = RefCell::default();

    static COMPILER_CONFIG: RefCell<Rc<CompilerConfig>> = RefCell::default();
    static ROOT_BINDING_ENV: RefCell<Rc<Env<'static, String, Binding>>> = RefCell::default();
    static INTRINSIC_LIBRARIES: RefCell<HashMap<ModId, Rc<ModuleInterface>>> = RefCell::default();
    static REGISTERED_MODULE_NAMES: RefCell<HashMap<ModId, PathBuf>> = RefCell::default();
    static REGISTERED_LIBRARIES: RefCell<HashMap<ModId, Rc<Module>>> = RefCell::default();
}
