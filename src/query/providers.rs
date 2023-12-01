use std::{cell::RefCell, collections::HashMap, path::PathBuf, rc::Rc};

use crate::{
    config::CompilerConfig,
    diagnostics::Diagnostic,
    env::Env,
    expander::{expand_root, Binding, ExpanderResult},
    file::{FileId, SourceFile},
    syntax::{
        ast::{Module, ModuleInterface, ModuleName},
        cst::SynRoot,
        parser::{parse_source_file, ParseResult},
    },
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
    INTRINSIC_LIBRARIES.with(|i| i.borrow_mut().insert(lib.name.clone(), Rc::new(lib)));
}

pub fn register_lib((name, lib): (ModuleName, Module)) {
    REGISTERED_LIBRARIES.with(|i| i.borrow_mut().insert(name, Rc::new(lib)));
}

pub fn register_module_name((name, path): (ModuleName, PathBuf)) {
    REGISTERED_MODULE_NAMES.with(|m| m.borrow_mut().insert(name, path));
}

pub fn read_file_provider(_: &QCtx, path: PathBuf) -> Res<Rc<SourceFile>> {
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

pub fn lookup_module_name_provider(_qctx: &QCtx, module_name: ModuleName) -> Res<PathBuf> {
    if let Some(path) = REGISTERED_MODULE_NAMES.with(|m| m.borrow().get(&module_name).cloned()) {
        Ok(path)
    } else {
        todo!()
    }
}

pub fn module_interface_provider(
    _qctx: &QCtx,
    module_name: ModuleName,
) -> Res<Rc<ModuleInterface>> {
    let lib = INTRINSIC_LIBRARIES.with(|i| i.borrow().get(&module_name).map(Rc::clone));
    if let Some(intrinsics_lib) = lib {
        Ok(intrinsics_lib)
    } else {
        let registered_lib =
            REGISTERED_LIBRARIES.with(|i| i.borrow().get(&module_name).map(Rc::clone));
        if let Some(registered_lib) = registered_lib {
            Ok(Rc::new(registered_lib.to_interface()))
        } else {
            Err(Diagnostic::builder()
                .msg(format!("module not found: {module_name}"))
                .finish())
        }
    }
}

pub fn expand_provider(qctx: &QCtx, module_name: ModuleName) -> Res<ExpanderResult> {
    let pathbuf = qctx.lookup_module_name(module_name)?;
    let mut parse_res = qctx.parse(pathbuf)?;
    let syn = SynRoot::new(&parse_res.tree, parse_res.file_id);
    let root_binding_env = qctx.root_binding_env(());
    let mut expander_res = expand_root(
        syn,
        root_binding_env.as_ref(),
        |mname| qctx.module_interface(mname),
        |mname, module| register_lib((mname, module)),
    );
    parse_res
        .diagnostics
        .extend(std::mem::take(&mut expander_res.diagnostics));
    expander_res.diagnostics = parse_res.diagnostics;
    Ok(expander_res)
}

thread_local! {
    static FILE_MAPPING: RefCell<HashMap<FileId, Rc<SourceFile>>> = RefCell::default();
    static COMPILER_CONFIG: RefCell<Rc<CompilerConfig>> = RefCell::default();
    static ROOT_BINDING_ENV: RefCell<Rc<Env<'static, String, Binding>>> = RefCell::default();
    static INTRINSIC_LIBRARIES: RefCell<HashMap<ModuleName, Rc<ModuleInterface>>> = RefCell::default();
    static REGISTERED_MODULE_NAMES: RefCell<HashMap<ModuleName, PathBuf>> = RefCell::default();
    static REGISTERED_LIBRARIES: RefCell<HashMap<ModuleName, Rc<Module>>> = RefCell::default();
}
