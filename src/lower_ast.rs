#![allow(dead_code)]
use std::rc::Rc;

use crate::{
    diagnostics::Diagnostic,
    env::Env,
    ir,
    span::Span,
    syntax::{ast, parser},
    ty,
    utils::Symbol,
};

type PEnv<'p> = Env<'p, Symbol, Place>;

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
    builtin_tys: &ty::BuiltinTys,
) -> Result<ir::Package, Vec<Diagnostic>> {
    let mut lowerer = Lowerer {
        import,
        intrinsics_mid,
        builtin_tys,
        diagnostics: vec![],
        current_module: mod_.id,
        //modules: vec![],
        current_path: vec![],
        body_builder: BodyBuilder::default(),
        bodys_stack: vec![],
        bodies: vec![],
    };
    lowerer
        .lower_ast(mod_)
        .ok_or_else(|| std::mem::take(&mut lowerer.diagnostics))
}

struct Lowerer<'i, 'ty> {
    import: &'i dyn Fn(ast::ModId) -> Result<Rc<ast::Module>, Diagnostic>,
    intrinsics_mid: ast::ModId,
    builtin_tys: &'ty ty::BuiltinTys,
    diagnostics: Vec<Diagnostic>,
    current_module: ast::ModId,
    //modules: Vec<ast::ModId>,
    current_path: Vec<Symbol>,
    body_builder: BodyBuilder,
    bodys_stack: Vec<BodyBuilder>,
    bodies: Vec<ir::Body>,
}

impl Lowerer<'_, '_> {
    fn lower_ast(&mut self, mod_: &ast::Module) -> Option<ir::Package> {
        self.lower_mod(mod_)
    }

    fn lower_mod(&mut self, mod_: &ast::Module) -> Option<ir::Package> {
        let ast::ExprKind::Body(body) = &mod_.body.kind else {
            panic!("expected a body")
        };

        self.body_builder
            .start(mod_.span, Rc::clone(&self.builtin_tys.void));

        self.lower_mod_body(body);

        for b in &self.bodies {
            if b.name.module == mod_.id && &b.name.value == "@init" {
                todo!("@init not allowed yet: {:?}", b.name);
            }
        }

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

        if self.diagnostics.is_empty() {
            Some(ir::Package {
                bodies: std::mem::take(&mut self.bodies),
            })
        } else {
            None
        }
    }

    fn lower_mod_body(&mut self, body: &[ast::Item]) {
        let mut _fns = vec![];
        let mut _vars = vec![];
        let mut exprs = vec![];
        let mut env = PEnv::default();

        for item in body {
            match item {
                ast::Item::Import(_, _) => todo!(),
                ast::Item::Mod(_, _) => todo!(),
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
                ast::Item::Import(_, _) => todo!(),
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
                let l =
                    self.get_or_alloc_local(local, expr.span, Rc::clone(&self.builtin_tys.boolean));
                self.body_builder.load_i(
                    l,
                    ir::Constant::new(if *b { 1 } else { 0 }, expr.span),
                    expr.span,
                );
                l
            }
            ast::ExprKind::Char(c) => {
                let l =
                    self.get_or_alloc_local(local, expr.span, Rc::clone(&self.builtin_tys.char));
                self.body_builder
                    .load_i(l, ir::Constant::new(*c as u64, expr.span), expr.span);
                l
            }
            ast::ExprKind::Number(n) => {
                let l =
                    self.get_or_alloc_local(local, expr.span, Rc::clone(&self.builtin_tys.fixnum));
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

                let l =
                    self.get_or_alloc_local(local, expr.span, Rc::clone(&self.builtin_tys.object));

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
        let l = self.get_or_alloc_local(local, span, Rc::clone(&self.builtin_tys.object));
        let cl = self.lower_expr(cond, None, env);
        self.body_builder.arg(cl, cond.span);
        let cond_local = self.get_or_alloc_local(None, span, Rc::clone(&self.builtin_tys.boolean));
        self.body_builder.call_label(
            cond_local,
            ast::Path {
                span,
                module: self.intrinsics_mid,
                value: "truthy?".into(),
            },
            cond.span,
        );

        let cidx = self
            .body_builder
            .terminate_block(ir::TerminatorKind::default(), cond.span);

        let tl = self.lower_expr(r#true, None, env);
        self.body_builder.copy(l, tl, r#true.span);
        let tidx = self
            .body_builder
            .terminate_block(ir::TerminatorKind::default(), r#true.span);

        let fl = self.lower_expr(r#false, None, env);
        self.body_builder.copy(l, fl, r#false.span);
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

        l
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
                              [_1 fixnum (0:0..3)])
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
                              [_1 object (0:0..11)]
                              [_2 boolean (0:4..2)]
                              [_3 boolean (0:0..11)]
                              [_4 fixnum (0:7..1)]
                              [_5 fixnum (0:9..1)])
                          (bb 0
                            (loadI _2 1 (0:4..2))
                            (arg _2 (0:4..2))
                            (call-label _3 |{|truthy?| (rnrs intrinsics ()) 0:0..11}| (0:4..2))
                            (cond _3 bb1 bb2 (0:4..2)))
                          (bb 1
                            (loadI _4 1 (0:7..1))
                            (copy _1 _4 (0:7..1))
                            (goto bb3 (0:7..1)))
                          (bb 2
                            (loadI _5 2 (0:9..1))
                            (copy _1 _5 (0:9..1))
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
                              [_2 object (0:45..2)]
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
}
