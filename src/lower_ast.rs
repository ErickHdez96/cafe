use std::rc::Rc;

use crate::{
    env::Env,
    interner::Interner,
    ir,
    span::Span,
    symbol::Symbol,
    syntax::{ast, parser},
    ty::Ty,
};

#[cfg(test)]
mod tests;

type PEnv<'p> = Env<'p, Symbol, Place>;

#[derive(Clone, PartialEq, Eq)]
enum Place {
    Local(ir::Local),
    Global(ast::Path),
}

impl From<ir::Local> for Place {
    fn from(value: ir::Local) -> Self {
        Self::Local(value)
    }
}

impl From<ast::Path> for Place {
    fn from(value: ast::Path) -> Self {
        Self::Global(value)
    }
}

pub enum ModuleOrInterface {
    Mod(Rc<ast::Module>),
    Int(Rc<ast::ModuleInterface>),
}

impl From<Rc<ast::Module>> for ModuleOrInterface {
    fn from(value: Rc<ast::Module>) -> Self {
        Self::Mod(value)
    }
}

impl From<Rc<ast::ModuleInterface>> for ModuleOrInterface {
    fn from(value: Rc<ast::ModuleInterface>) -> Self {
        Self::Int(value)
    }
}

pub fn lower_ast(
    mod_: &ast::Module,
    // TODO: Improve this
    import: &dyn Fn(ast::ModId) -> ModuleOrInterface,
    interner: &Interner,
) -> ir::Package {
    let mut lowerer = Lowerer {
        import,
        interner,
        current_module: mod_.id,
        current_path: vec![],
        body_builder: BodyBuilder::default(),
        bodys_stack: vec![],
        bodies: vec![],
        lowered_mids: vec![],
        exported_bindings: Env::default(),
        lambda_counter: 0,
    };
    lowerer.lower_ast(mod_);

    ir::Package {
        bodies: std::mem::take(&mut lowerer.bodies),
    }
}

struct Lowerer<'i> {
    import: &'i dyn Fn(ast::ModId) -> ModuleOrInterface,
    interner: &'i Interner,
    current_module: ast::ModId,
    current_path: Vec<Symbol>,
    body_builder: BodyBuilder,
    bodys_stack: Vec<BodyBuilder>,
    bodies: Vec<ir::Body>,
    lowered_mids: Vec<ast::ModId>,
    exported_bindings: Env<'static, ast::ModId, PEnv<'static>>,
    lambda_counter: usize,
}

impl Lowerer<'_> {
    fn lower_ast(&mut self, mod_: &ast::Module) {
        self.current_module = mod_.id;
        if self.lowered_mids.contains(&mod_.id) {
            return;
        }

        // Intrinsic modules have void as their bodies
        if mod_.body.is_void() {
            let mut exported = Env::default();
            for key in mod_.exports.keys().copied() {
                exported.insert(
                    key,
                    Place::Global(ast::Path {
                        span: mod_.span,
                        module: mod_.id,
                        value: key,
                    }),
                );
            }
            self.exported_bindings.insert(mod_.id, exported);
            return;
        }

        for dep in &mod_.dependencies {
            match (self.import)(*dep) {
                ModuleOrInterface::Mod(m) => {
                    self.lower_ast(&m);
                }
                ModuleOrInterface::Int(i) => {
                    self.lower_mod_interface(&i);
                }
            }
        }

        self.current_module = mod_.id;
        let mut env = self.lower_mod(mod_);
        let mut exported = Env::default();
        for key in mod_.exports.keys().copied() {
            let Some(place) = env.remove_immediate(&key) else {
                panic!("expected exported binding: {key}");
            };

            exported.insert(key, place);
        }
        self.exported_bindings.insert(mod_.id, exported);
    }

    fn lower_mod_interface(&mut self, int: &ast::ModuleInterface) {
        self.current_module = int.id;
        if self.lowered_mids.contains(&int.id) {
            return;
        }

        // Intrinsic modules have void as their bodies
        let mut exported = Env::default();
        for key in int.bindings.keys().copied() {
            exported.insert(
                key,
                Place::Global(ast::Path {
                    span: int.span,
                    module: int.id,
                    value: key,
                }),
            );
        }
        self.exported_bindings.insert(int.id, exported);
    }

    fn lower_mod(&mut self, mod_: &ast::Module) -> PEnv<'static> {
        let ast::ExprKind::Body(body) = &mod_.body.kind else {
            panic!("expected a body")
        };
        let init = "@init".into();

        self.body_builder
            .start(mod_.span, self.interner.builtins.types.void);

        let env = self.lower_mod_body(body);

        for b in &self.bodies {
            if b.name.module == mod_.id && b.name.value == init {
                todo!("@init not allowed yet: {:?}", b.name);
            }
        }

        if self.body_builder.bbs.is_empty() && self.body_builder.stmts.is_empty() {
            self.body_builder.reset();
        } else {
            self.body_builder
                .terminate_block(ir::TerminatorKind::Return, mod_.span);
            self.finish_body(BodyData {
                span: mod_.span,
                name: ast::Path {
                    span: mod_.span,
                    module: mod_.id,
                    value: "@init".into(),
                },
                ty: self.interner.builtins.types.void_fn,
                param_count: 0,
                variable_count: 0,
            });
        }
        env
    }

    fn lower_mod_body(&mut self, body: &[ast::Item]) -> PEnv<'static> {
        let mut fns = vec![];
        let mut _vars = vec![];
        let mut exprs = vec![];
        let mut env = PEnv::default();

        for item in body {
            match item {
                ast::Item::Define(d) => match &d.expr {
                    Some(e) if e.is_lambda() => {
                        fns.push((e.span, &d.name, e, e.ty.unwrap()));
                    }
                    _ => {
                        _vars.push((&d.name, &d.expr));
                    }
                },
                ast::Item::Expr(e) => {
                    exprs.push(e);
                }
                ast::Item::Import(mids, _) => {
                    for mid in mids {
                        self.import_mod(*mid, &mut env);
                    }
                }
                ast::Item::Mod(_, _) => {}
            }
        }

        for (span, ident, fn_, ty) in fns {
            let ast::ExprKind::Lambda {
                formals,
                rest,
                expr,
                formal_tys,
            } = &fn_.kind
            else {
                panic!("expected a function: {fn_:#?}");
            };

            assert_eq!(
                formals.len(),
                formal_tys.len(),
                "some formals are lacking a type"
            );
            let path = self.lower_lambda(
                Some(ident),
                &formals
                    .iter()
                    .cloned()
                    .zip(formal_tys.iter().copied())
                    .collect::<Vec<_>>(),
                rest.as_ref(),
                expr,
                ty,
                span,
                &env,
            );
            env.insert(ident.value, path.into());
        }

        for (ident, expr) in _vars {
            match expr {
                Some(e) => {
                    let path = ast::Path {
                        span: ident.span,
                        module: self.current_module,
                        value: ident.value,
                    };
                    let local = self.lower_expr(e, Some(ident), None, &env);
                    self.body_builder.store_label(path, local, e.span);
                    env.insert(ident.value, path.into());
                }
                None => todo!(),
            }
        }

        for e in exprs {
            self.lower_expr(e, None, None, &env);
        }
        env
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_lambda(
        &mut self,
        name: Option<&ast::Ident>,
        formals: &[(ast::Ident, Ty)],
        rest: Option<&ast::Ident>,
        body: &ast::Expr,
        ty: Ty,
        span: Span,
        env: &PEnv,
    ) -> ast::Path {
        self.enter_body();
        let mut env = env.enter();
        self.body_builder.start(span, body.ty.unwrap());

        for (f, ty) in formals {
            let local = self.body_builder.alloc(f.span, *ty);
            env.insert(f.value, local.into());
        }

        if rest.is_some() {
            panic!("rest not supported");
        }

        let ast::ExprKind::Body(b) = &body.kind else {
            panic!("expected a body: {:#?}", body.kind)
        };

        let mut _defs = vec![];
        let mut exprs = vec![];
        for i in b {
            match i {
                ast::Item::Import(mids, _) => {
                    for mid in mids {
                        self.import_mod(*mid, &mut env);
                    }
                }
                ast::Item::Mod(_, _) => todo!(),
                ast::Item::Define(d) => _defs.push(d),
                ast::Item::Expr(e) => exprs.push(e),
            }
        }

        let exprs_len = exprs.len();
        for (i, e) in exprs.into_iter().enumerate() {
            self.lower_expr(
                e,
                None,
                if i == exprs_len - 1 {
                    Some(ir::Local::ret())
                } else {
                    None
                },
                &env,
            );
        }

        self.body_builder
            .terminate_block(ir::TerminatorKind::Return, span);
        let lambda_name = match name {
            Some(name) => self.lambda_name(name),
            None => self.lambda_name_anonymous(span),
        };
        self.finish_body(BodyData {
            span,
            name: lambda_name,
            ty,
            param_count: formals.len() + if rest.is_some() { 1 } else { 0 },
            variable_count: 0,
        });
        self.exit_body();

        lambda_name
    }

    fn lower_expr(
        &mut self,
        expr: &ast::Expr,
        name: Option<&ast::Ident>,
        local: Option<ir::Local>,
        env: &PEnv,
    ) -> ir::Local {
        match &expr.kind {
            ast::ExprKind::Body(_) => panic!("should be handled somewhere else"),
            ast::ExprKind::Let { .. } => todo!(),
            ast::ExprKind::Quote(_) => todo!(),
            ast::ExprKind::If(cond, r#true, r#false) => self.lower_if(
                cond,
                r#true,
                r#false,
                expr.ty.unwrap(),
                expr.span,
                local,
                env,
            ),
            ast::ExprKind::Lambda {
                formals,
                rest,
                formal_tys,
                expr: body,
            } => {
                let path = self.lower_lambda(
                    name,
                    &formals
                        .iter()
                        .cloned()
                        .zip(formal_tys.iter().copied())
                        .collect::<Vec<_>>(),
                    rest.as_ref(),
                    body,
                    expr.ty.unwrap(),
                    expr.span,
                    env,
                );
                let l = self.get_or_alloc_local(local, expr.span, expr.ty.unwrap());
                self.body_builder.load_label(l, path, expr.span);
                l
            }
            ast::ExprKind::List(l) => self.lower_list(l, expr.ty.unwrap(), expr.span, local, env),
            ast::ExprKind::DottedList(_, _) => todo!(),
            ast::ExprKind::Boolean(b) => {
                let l = self.get_or_alloc_local(local, expr.span, expr.ty.unwrap());
                self.body_builder.load_i(
                    l,
                    ir::Constant::new(if *b { 1 } else { 0 }, expr.span),
                    expr.span,
                );
                l
            }
            ast::ExprKind::Char(c) => {
                let l = self.get_or_alloc_local(local, expr.span, expr.ty.unwrap());
                self.body_builder
                    .load_i(l, ir::Constant::new(*c as u64, expr.span), expr.span);
                l
            }
            ast::ExprKind::Number(n) => {
                let l = self.get_or_alloc_local(local, expr.span, expr.ty.unwrap());
                match n {
                    parser::Number::Fixnum(fx) => {
                        self.body_builder.load_i(
                            l,
                            ir::Constant::new((*fx).try_into().unwrap(), expr.span),
                            expr.span,
                        );
                    }
                }
                l
            }
            ast::ExprKind::Var(v) => {
                let Some(place) = env.get(&v.value) else {
                    panic!("undefined variable {:}", v.value)
                };

                let l = self.get_or_alloc_local(local, expr.span, expr.ty.unwrap());

                match place {
                    Place::Local(loc) => {
                        self.body_builder.copy(l, *loc, expr.span);
                    }
                    Place::Global(g) => {
                        self.body_builder.load_label(l, *g, expr.span);
                    }
                }

                l
            }
            ast::ExprKind::Error(_) => todo!(),
            ast::ExprKind::Void => todo!(),
            ast::ExprKind::Begin(_) => todo!(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_if(
        &mut self,
        cond: &ast::Expr,
        r#true: &ast::Expr,
        r#false: &ast::Expr,
        ty: Ty,
        span: Span,
        local: Option<ir::Local>,
        env: &PEnv,
    ) -> ir::Local {
        let retl = self.get_or_alloc_local(local, span, ty);
        let cond_local = self.lower_expr(cond, None, None, env);

        let cidx = self
            .body_builder
            .terminate_block(ir::TerminatorKind::default(), cond.span);

        let tl = self.lower_expr(r#true, None, None, env);
        self.body_builder.copy(retl, tl, r#true.span);
        let tidx = self
            .body_builder
            .terminate_block(ir::TerminatorKind::default(), r#true.span);

        let fl = self.lower_expr(r#false, None, None, env);
        self.body_builder.copy(retl, fl, r#false.span);
        let fidx = self
            .body_builder
            .terminate_block(ir::TerminatorKind::default(), r#false.span);

        let endidx = self.body_builder.current_block_idx();
        self.body_builder.get_block_mut(cidx).terminator.kind = ir::TerminatorKind::Cond {
            cond: cond_local,
            true_label: tidx,
            false_label: fidx,
        };
        self.body_builder.get_block_mut(tidx).terminator.kind = ir::TerminatorKind::Goto(endidx);
        self.body_builder.get_block_mut(fidx).terminator.kind = ir::TerminatorKind::Goto(endidx);

        let tlty = self.body_builder.get_local(tl).ty;
        if tlty == self.body_builder.get_local(fl).ty {
            self.body_builder.get_local_mut(retl).ty = tlty;
        }

        retl
    }

    fn lower_list(
        &mut self,
        list: &[ast::Expr],
        ty: Ty,
        span: Span,
        local: Option<ir::Local>,
        env: &PEnv,
    ) -> ir::Local {
        let l = self.get_or_alloc_local(local, span, ty);
        let mut elems = list.iter();

        let func = {
            let e = elems.next().expect("list must not be empty");
            self.lower_expr(e, None, local, env)
        };

        for e in elems {
            let el = self.lower_expr(e, None, None, env);
            self.body_builder.arg(el, e.span);
        }

        self.body_builder.call(l, func, span);

        l
    }

    fn get_or_alloc_local(&mut self, local: Option<ir::Local>, span: Span, ty: Ty) -> ir::Local {
        local.unwrap_or_else(|| self.body_builder.alloc(span, ty))
    }

    fn lambda_name(&self, name: &ast::Ident) -> ast::Path {
        ast::Path {
            span: name.span,
            module: self.current_module,
            value: if self.current_path.is_empty() {
                name.value
            } else {
                format!(
                    "{}#{}",
                    self.current_path
                        .iter()
                        .copied()
                        .map(Symbol::resolve)
                        .collect::<Vec<_>>()
                        .join("#"),
                    name.value
                )
                .into()
            },
        }
    }

    fn lambda_name_anonymous(&mut self, span: Span) -> ast::Path {
        let id = self.lambda_counter;
        self.lambda_counter += 1;
        self.lambda_name(&ast::Ident {
            span,
            value: format!("lambda{id}").into(),
        })
    }

    fn enter_body(&mut self) {
        self.bodys_stack
            .push(std::mem::take(&mut self.body_builder));
    }

    fn exit_body(&mut self) {
        self.body_builder = self.bodys_stack.pop().expect("not inside a body");
    }

    fn finish_body(&mut self, body_data: BodyData) {
        self.bodies.push(self.body_builder.finish(body_data));
    }

    fn import_mod(&self, mid: ast::ModId, env: &mut PEnv) {
        let exported = self
            .exported_bindings
            .get(&mid)
            .unwrap_or_else(|| panic!("module not found {}", mid.resolve()));
        for (k, v) in exported.bindings() {
            env.insert(*k, v.clone());
        }
    }
}

/// ᕙ(⇀‸↼‶)ᕗ
#[derive(Default)]
struct BodyBuilder {
    locals: Vec<ir::LocalDecl>,
    bbs: Vec<ir::BasicBlockData>,
    stmts: Vec<ir::Statement>,
}

struct BodyData {
    span: Span,
    name: ast::Path,
    ty: Ty,
    param_count: usize,
    variable_count: usize,
}

impl BodyBuilder {
    fn reset(&mut self) {
        self.locals = vec![];
        self.bbs = vec![];
        self.stmts = vec![];
    }

    /// Starts a new body with span `span` and return type `ty`.
    fn start(&mut self, span: Span, ty: Ty) {
        assert_eq!(
            Vec::<ir::LocalDecl>::new(),
            self.locals,
            "Body Builder already started"
        );
        self.alloc(span, ty);
    }

    fn current_block_idx(&self) -> ir::BasicBlock {
        ir::BasicBlock::new(self.bbs.len())
    }

    fn get_block_mut(&mut self, bidx: ir::BasicBlock) -> &mut ir::BasicBlockData {
        self.bbs
            .get_mut(bidx.value())
            .expect("invalid basic block index")
    }

    fn terminate_block(&mut self, kind: ir::TerminatorKind, span: Span) -> ir::BasicBlock {
        let bb = self.current_block_idx();
        self.bbs.push(ir::BasicBlockData {
            idx: bb.value(),
            statements: std::mem::take(&mut self.stmts),
            terminator: ir::Terminator { span, kind },
        });
        bb
    }

    fn finish(&mut self, data: BodyData) -> ir::Body {
        assert_eq!(
            Vec::<ir::Statement>::new(),
            self.stmts,
            "should have consumed all statements"
        );
        let body = ir::Body {
            span: data.span,
            name: data.name,
            ty: data.ty,
            param_count: data.param_count,
            variable_count: data.variable_count,
            locals: std::mem::take(&mut self.locals),
            instantiations: vec![],
            stack_size: 0,
            basic_blocks: std::mem::take(&mut self.bbs),
        };
        self.reset();
        body
    }

    fn get_local(&self, local: ir::Local) -> &ir::LocalDecl {
        &self.locals[TryInto::<usize>::try_into(local.value()).unwrap()]
    }

    fn get_local_mut(&mut self, local: ir::Local) -> &mut ir::LocalDecl {
        &mut self.locals[TryInto::<usize>::try_into(local.value()).unwrap()]
    }

    fn alloc(&mut self, span: Span, ty: Ty) -> ir::Local {
        let l = ir::Local::new(self.locals.len().try_into().expect("too many locals"));
        self.locals.push(ir::LocalDecl {
            span,
            ty,
            stack_offset: 0,
            size: 0,
        });
        l
    }

    fn load_i(&mut self, dst: ir::Local, constant: ir::Constant, span: Span) {
        self.stmts.push(ir::Statement {
            span,
            kind: ir::StatementKind::LoadI(dst, constant),
        });
    }

    fn copy(&mut self, dst: ir::Local, src: ir::Local, span: Span) {
        self.stmts.push(ir::Statement {
            span,
            kind: ir::StatementKind::Copy(dst, src),
        });
    }

    fn load_label(&mut self, dst: ir::Local, label: ast::Path, span: Span) {
        self.stmts.push(ir::Statement {
            span,
            kind: ir::StatementKind::LoadLabel(dst, label),
        });
    }

    fn arg(&mut self, arg: ir::Local, span: Span) {
        self.stmts.push(ir::Statement {
            span,
            kind: ir::StatementKind::Arg(arg),
        });
    }

    fn call(&mut self, dst: ir::Local, func: ir::Local, span: Span) {
        self.stmts.push(ir::Statement {
            span,
            kind: ir::StatementKind::Call(dst, func),
        });
    }

    fn store_label(&mut self, label: ast::Path, src: ir::Local, span: Span) {
        self.stmts.push(ir::Statement {
            span,
            kind: ir::StatementKind::StoreLabel(label, src),
        });
    }
}
