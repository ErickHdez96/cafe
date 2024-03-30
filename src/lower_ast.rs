use std::rc::Rc;

use crate::{
    diagnostics::Diagnostic,
    env::Env,
    ir,
    span::Span,
    syntax::{ast, parser},
    ty,
    utils::{Resolve, Symbol},
};

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

pub fn lower_ast(
    mod_: &ast::Module,
    // TODO: Improve this
    intrinsics_mid: ast::ModId,
    import: &dyn Fn(ast::ModId) -> Result<Rc<ast::Module>, Diagnostic>,
    builtin_tys: ty::BuiltinTys,
) -> Result<ir::Package, Vec<Diagnostic>> {
    let mut lowerer = Lowerer {
        import,
        intrinsics_mid,
        builtin_tys,
        diagnostics: vec![],
        current_module: mod_.id,
        current_path: vec![],
        body_builder: BodyBuilder::default(),
        bodys_stack: vec![],
        bodies: vec![],
        lowered_mids: vec![],
        exported_bindings: Env::default(),
    };
    lowerer.lower_ast(mod_);

    if lowerer.diagnostics.iter().any(|d| d.level.is_error()) {
        Err(std::mem::take(&mut lowerer.diagnostics))
    } else {
        Ok(ir::Package {
            bodies: std::mem::take(&mut lowerer.bodies),
        })
    }
}

struct Lowerer<'i> {
    import: &'i dyn Fn(ast::ModId) -> Result<Rc<ast::Module>, Diagnostic>,
    intrinsics_mid: ast::ModId,

    builtin_tys: ty::BuiltinTys,
    diagnostics: Vec<Diagnostic>,
    current_module: ast::ModId,
    current_path: Vec<Symbol>,
    body_builder: BodyBuilder,
    bodys_stack: Vec<BodyBuilder>,
    bodies: Vec<ir::Body>,
    lowered_mids: Vec<ast::ModId>,
    exported_bindings: Env<'static, ast::ModId, PEnv<'static>>,
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
            for key in mod_.exports.keys() {
                exported.insert(
                    key.into(),
                    Place::Global(ast::Path {
                        span: mod_.span,
                        module: mod_.id,
                        value: key.into(),
                    }),
                );
            }
            self.exported_bindings.insert(mod_.id, exported);
            return;
        }

        for dep in &mod_.dependencies {
            match (self.import)(*dep) {
                Ok(m) => {
                    self.lower_ast(&m);
                }
                Err(d) => {
                    self.diagnostics.push(d);
                    return;
                }
            }
        }

        self.current_module = mod_.id;
        let mut env = self.lower_mod(mod_);
        let mut exported = Env::default();
        for key in mod_.exports.keys() {
            let Some(place) = env.remove_immediate(key) else {
                panic!("expected exported binding: {key}");
            };

            exported.insert(key.into(), place);
        }
        self.exported_bindings.insert(mod_.id, exported);
    }

    fn lower_mod(&mut self, mod_: &ast::Module) -> PEnv<'static> {
        let ast::ExprKind::Body(body) = &mod_.body.kind else {
            panic!("expected a body")
        };

        self.body_builder
            .start(mod_.span, Rc::clone(&self.builtin_tys.void));

        let env = self.lower_mod_body(body);

        for b in &self.bodies {
            if b.name.module == mod_.id && &b.name.value == "@init" {
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
                param_count: 0,
                variable_count: 0,
            });
        }
        env
    }

    fn lower_mod_body(&mut self, body: &[ast::Item]) -> PEnv<'static> {
        let mut _fns = vec![];
        let mut _vars = vec![];
        let mut exprs = vec![];
        let mut env = PEnv::default();

        for item in body {
            match item {
                ast::Item::Define(d) => match &d.expr {
                    Some(e) if e.is_lambda() => {
                        _fns.push((d.span, &d.name, e));
                    }
                    _ => {
                        _vars.push((d.span, &d.name, &d.expr));
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

        for fn_ in _fns {
            let ast::ExprKind::Lambda {
                formals,
                rest,
                expr,
            } = &fn_.2.kind
            else {
                panic!("expected a function: {fn_:#?}");
            };

            let global = self.lower_lambda(fn_.1, formals, rest.as_ref(), expr, fn_.0, &env);
            env.insert(fn_.1.value.clone(), global.into());
        }

        for var_ in _vars {
            todo!("{:?}", var_);
        }

        for e in exprs {
            self.lower_expr(e, None, &env);
        }
        env
    }

    fn lower_lambda(
        &mut self,
        name: &ast::Ident,
        formals: &[ast::Ident],
        rest: Option<&ast::Ident>,
        body: &ast::Expr,
        span: Span,
        env: &PEnv,
    ) -> ast::Path {
        self.enter_body();
        let mut env = env.enter();
        self.body_builder
            .start(span, Rc::clone(&self.builtin_tys.object));

        for f in formals {
            let local = self
                .body_builder
                .alloc(f.span, Rc::clone(&self.builtin_tys.object));
            env.insert(f.value.clone(), local.into());
        }

        if let Some(rest) = rest {
            let local = self
                .body_builder
                .alloc(rest.span, Rc::clone(&self.builtin_tys.object));
            env.insert(rest.value.clone(), local.into());
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
        let lambda_name = self.lambda_name(name);
        self.finish_body(BodyData {
            span,
            name: lambda_name.clone(),
            param_count: formals.len() + if rest.is_some() { 1 } else { 0 },
            variable_count: 0,
        });
        self.exit_body();

        lambda_name
    }

    fn lower_expr(&mut self, expr: &ast::Expr, local: Option<ir::Local>, env: &PEnv) -> ir::Local {
        match &expr.kind {
            ast::ExprKind::Body(_) => panic!("should be handled somewhere else"),
            ast::ExprKind::Let { .. } => todo!(),
            ast::ExprKind::Quote(_) => todo!(),
            ast::ExprKind::If(cond, r#true, r#false) => {
                self.lower_if(cond, r#true, r#false, expr.span, local, env)
            }
            ast::ExprKind::Lambda { .. } => todo!(),
            ast::ExprKind::List(l) => self.lower_list(l, expr.span, local, env),
            ast::ExprKind::DottedList(_, _) => todo!(),
            ast::ExprKind::Boolean(b) => {
                let l = self.get_or_alloc_local(local, expr.span, Rc::clone(&expr.ty));
                self.body_builder.load_i(
                    l,
                    ir::Constant::new(if *b { 1 } else { 0 }, expr.span),
                    expr.span,
                );
                l
            }
            ast::ExprKind::Char(c) => {
                let l = self.get_or_alloc_local(local, expr.span, Rc::clone(&expr.ty));
                self.body_builder
                    .load_i(l, ir::Constant::new(*c as u64, expr.span), expr.span);
                l
            }
            ast::ExprKind::Number(n) => {
                let l = self.get_or_alloc_local(local, expr.span, Rc::clone(&expr.ty));
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

                let l = self.get_or_alloc_local(local, expr.span, Rc::clone(&expr.ty));

                match place {
                    Place::Local(loc) => {
                        self.body_builder.copy(l, *loc, expr.span);
                    }
                    Place::Global(g) => {
                        self.body_builder.load_label(l, g.clone(), expr.span);
                    }
                }

                l
            }
            ast::ExprKind::Error(_) => todo!(),
            ast::ExprKind::Void => todo!(),
            ast::ExprKind::Begin(_) => todo!(),
        }
    }

    fn lower_if(
        &mut self,
        cond: &ast::Expr,
        r#true: &ast::Expr,
        r#false: &ast::Expr,
        span: Span,
        local: Option<ir::Local>,
        env: &PEnv,
    ) -> ir::Local {
        let retl = self.get_or_alloc_local(local, span, Rc::clone(&self.builtin_tys.object));
        let cl = self.lower_expr(cond, None, env);
        let cond_local = if self.body_builder.get_local(cl).ty.is_boolean() {
            cl
        } else {
            self.body_builder.arg(cl, cond.span);
            let cond_local =
                self.get_or_alloc_local(None, span, Rc::clone(&self.builtin_tys.boolean));
            self.body_builder.call_label(
                cond_local,
                ast::Path {
                    span,
                    module: self.intrinsics_mid,
                    value: "truthy?".into(),
                },
                cond.span,
            );
            cond_local
        };

        let cidx = self
            .body_builder
            .terminate_block(ir::TerminatorKind::default(), cond.span);

        let tl = self.lower_expr(r#true, None, env);
        self.body_builder.copy(retl, tl, r#true.span);
        let tidx = self
            .body_builder
            .terminate_block(ir::TerminatorKind::default(), r#true.span);

        let fl = self.lower_expr(r#false, None, env);
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

        let tlty = Rc::clone(&self.body_builder.get_local(tl).ty);
        if Rc::ptr_eq(&tlty, &self.body_builder.get_local(fl).ty) {
            self.body_builder.get_local_mut(retl).ty = tlty;
        }

        retl
    }

    fn lower_list(
        &mut self,
        list: &[ast::Expr],
        span: Span,
        local: Option<ir::Local>,
        env: &PEnv,
    ) -> ir::Local {
        let l = self.get_or_alloc_local(local, span, Rc::clone(&self.builtin_tys.object));
        let mut elems = list.iter();

        let func = {
            let e = elems.next().expect("list must not be empty");
            self.lower_expr(e, local, env)
        };

        for e in elems {
            let el = self.lower_expr(e, None, env);
            self.body_builder.arg(el, e.span);
        }

        self.body_builder.call(l, func, span);

        l
    }

    fn get_or_alloc_local(
        &mut self,
        local: Option<ir::Local>,
        span: Span,
        ty: Rc<ty::Ty>,
    ) -> ir::Local {
        local.unwrap_or_else(|| self.body_builder.alloc(span, ty))
    }

    fn lambda_name(&self, name: &ast::Ident) -> ast::Path {
        ast::Path {
            span: name.span,
            module: self.current_module,
            value: if self.current_path.is_empty() {
                name.value.clone()
            } else {
                format!("{}#{}", self.current_path.join("#"), name.value)
            },
        }
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
            env.insert(k.into(), v.clone());
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
    param_count: usize,
    variable_count: usize,
}

impl BodyBuilder {
    fn reset(&mut self) {
        self.locals = vec![];
        self.bbs = vec![];
        self.stmts = vec![];
    }

    fn start(&mut self, span: Span, ty: Rc<ty::Ty>) {
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
            param_count: data.param_count,
            variable_count: data.variable_count,
            locals: std::mem::take(&mut self.locals),
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

    fn alloc(&mut self, span: Span, ty: Rc<ty::Ty>) -> ir::Local {
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

    fn call_label(&mut self, dst: ir::Local, label: ast::Path, span: Span) {
        self.stmts.push(ir::Statement {
            span,
            kind: ir::StatementKind::CallLabel(dst, label),
        });
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use crate::test::test_lower_str;

    pub fn check(input: &str, expected: Expect) {
        let res = test_lower_str(input);
        expected.assert_debug_eq(&res);
    }

    #[test]
    fn boolean() {
        check(
            "#f",
            expect![[r#"
                (pkg
                  (fn (|{|@init| (#script ()) 0:0..2}|) void (0:0..2)
                    (let ([_0 void (0:0..2)]
                          [_1 boolean (0:0..2)])
                      (bb 0
                        (loadI _1 0 (0:0..2))
                        (return (0:0..2))))))
            "#]],
        );
        check(
            "#t",
            expect![[r#"
                (pkg
                  (fn (|{|@init| (#script ()) 0:0..2}|) void (0:0..2)
                    (let ([_0 void (0:0..2)]
                          [_1 boolean (0:0..2)])
                      (bb 0
                        (loadI _1 1 (0:0..2))
                        (return (0:0..2))))))
            "#]],
        );
    }

    #[test]
    fn char() {
        check(
            r"#\a",
            expect![[r#"
                (pkg
                  (fn (|{|@init| (#script ()) 0:0..3}|) void (0:0..3)
                    (let ([_0 void (0:0..3)]
                          [_1 char (0:0..3)])
                      (bb 0
                        (loadI _1 97 (0:0..3))
                        (return (0:0..3))))))
            "#]],
        );
        check(
            r"#\λ",
            expect![[r#"
                (pkg
                  (fn (|{|@init| (#script ()) 0:0..4}|) void (0:0..4)
                    (let ([_0 void (0:0..4)]
                          [_1 char (0:0..4)])
                      (bb 0
                        (loadI _1 955 (0:0..4))
                        (return (0:0..4))))))
            "#]],
        );
    }

    mod numbers {
        use super::*;

        #[test]
        fn integer() {
            check(
                "100",
                expect![[r#"
                    (pkg
                      (fn (|{|@init| (#script ()) 0:0..3}|) void (0:0..3)
                        (let ([_0 void (0:0..3)]
                              [_1 i64 (0:0..3)])
                          (bb 0
                            (loadI _1 100 (0:0..3))
                            (return (0:0..3))))))
                "#]],
            );
        }
    }

    mod conditionals {
        use super::*;

        #[test]
        fn simple_if() {
            check(
                "(if #t 1 2)",
                expect![[r#"
                    (pkg
                      (fn (|{|@init| (#script ()) 0:0..11}|) void (0:0..11)
                        (let ([_0 void (0:0..11)]
                              [_1 i64 (0:0..11)]
                              [_2 boolean (0:4..2)]
                              [_3 i64 (0:7..1)]
                              [_4 i64 (0:9..1)])
                          (bb 0
                            (loadI _2 1 (0:4..2))
                            (cond _2 bb1 bb2 (0:4..2)))
                          (bb 1
                            (loadI _3 1 (0:7..1))
                            (copy _1 _3 (0:7..1))
                            (goto bb3 (0:7..1)))
                          (bb 2
                            (loadI _4 2 (0:9..1))
                            (copy _1 _4 (0:9..1))
                            (goto bb3 (0:9..1)))
                          (bb 3
                            (return (0:0..11))))))
                "#]],
            );
        }
    }

    mod functions {
        use super::*;

        #[test]
        fn identity_call() {
            check(
                "(define id (lambda (x) x))
                 (id #t)",
                expect![[r#"
                    (pkg
                      (fn (|{|id| (#script ()) 0:8..2}| [_1: object]) object (0:0..26)
                        (let ([_0 object (0:0..26)]
                              [_1 object (0:20..1)])
                          (bb 0
                            (copy _0 _1 (0:23..1))
                            (return (0:0..26)))))
                      (fn (|{|@init| (#script ()) 0:0..51}|) void (0:0..51)
                        (let ([_0 void (0:0..51)]
                              [_1 object (0:44..7)]
                              [_2 (-> object object) (0:45..2)]
                              [_3 boolean (0:48..2)])
                          (bb 0
                            (addressof _2 |{|id| (#script ()) 0:8..2}| (0:45..2))
                            (loadI _3 1 (0:48..2))
                            (arg _3 (0:48..2))
                            (call _1 _2 (0:44..7))
                            (return (0:0..51))))))
                "#]],
            );
        }
    }

    mod module {
        use super::*;

        #[test]
        fn identity_call() {
            check(
                "(module (id ()) (id)
                   (import (rnrs expander core ()))
                   (define id (lambda (x) x)))
                 (import (id ()))
                 (id #t)",
                expect![[r#"
                    (pkg
                      (fn (|{|id| (id ()) 0:100..2}| [_1: object]) object (0:92..26)
                        (let ([_0 object (0:92..26)]
                              [_1 object (0:112..1)])
                          (bb 0
                            (copy _0 _1 (0:115..1))
                            (return (0:92..26)))))
                      (fn (|{|@init| (#script ()) 0:0..178}|) void (0:0..178)
                        (let ([_0 void (0:0..178)]
                              [_1 object (0:171..7)]
                              [_2 (-> object object) (0:172..2)]
                              [_3 boolean (0:175..2)])
                          (bb 0
                            (addressof _2 |{|id| (id ()) 0:100..2}| (0:172..2))
                            (loadI _3 1 (0:175..2))
                            (arg _3 (0:175..2))
                            (call _1 _2 (0:171..7))
                            (return (0:0..178))))))
                "#]],
            );
        }
    }
}
