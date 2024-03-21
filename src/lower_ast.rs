use std::rc::Rc;

use crate::{
    diagnostics::Diagnostic,
    ir,
    span::Span,
    syntax::{ast, parser},
};

pub fn lower_ast(
    mod_: &ast::Module,
    import: &dyn Fn(ast::ModId) -> Result<Rc<ast::Module>, Diagnostic>,
) -> Result<ir::Package, Vec<Diagnostic>> {
    let mut lowerer = Lowerer {
        import,
        diagnostics: vec![],
        current_module: mod_.id,
        modules: vec![],
        body_builder: BodyBuilder::default(),
        bodies: vec![],
    };
    lowerer
        .lower_ast(mod_)
        .ok_or_else(|| std::mem::take(&mut lowerer.diagnostics))
}

struct Lowerer<'i> {
    import: &'i dyn Fn(ast::ModId) -> Result<Rc<ast::Module>, Diagnostic>,
    diagnostics: Vec<Diagnostic>,
    current_module: ast::ModId,
    modules: Vec<ast::ModId>,
    body_builder: BodyBuilder,
    bodies: Vec<ir::Body>,
}

impl Lowerer<'_> {
    fn lower_ast(&mut self, mod_: &ast::Module) -> Option<ir::Package> {
        self.lower_mod(mod_)
    }

    fn lower_mod(&mut self, mod_: &ast::Module) -> Option<ir::Package> {
        let ast::ExprKind::Body(body) = &mod_.body.kind else {
            panic!("expected a body")
        };

        self.body_builder.start(mod_.span, ir::Ty::Unit);

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
            todo!("{:?}", fn_);
        }

        for var_ in _vars {
            todo!("{:?}", var_);
        }

        for e in exprs {
            self.lower_expr(e, None);
        }
    }

    fn lower_expr(&mut self, expr: &ast::Expr, local: Option<ir::Local>) -> ir::Local {
        match &expr.kind {
            ast::ExprKind::Body(_) => todo!(),
            ast::ExprKind::Let { .. } => todo!(),
            ast::ExprKind::Quote(_) => todo!(),
            ast::ExprKind::If(_, _, _) => todo!(),
            ast::ExprKind::Lambda { .. } => todo!(),
            ast::ExprKind::List(_) => todo!(),
            ast::ExprKind::DottedList(_, _) => todo!(),
            ast::ExprKind::Boolean(b) => {
                let l = self.get_or_alloc_local(local, expr.span, ir::Ty::Boolean);
                self.body_builder.load_i(
                    l,
                    ir::Constant::new(if *b { 1 } else { 0 }, expr.span),
                    expr.span,
                );
                l
            }
            ast::ExprKind::Char(c) => {
                let l = self.get_or_alloc_local(local, expr.span, ir::Ty::Boolean);
                self.body_builder
                    .load_i(l, ir::Constant::new(*c as u64, expr.span), expr.span);
                l
            }
            ast::ExprKind::Number(n) => {
                let l = self.get_or_alloc_local(local, expr.span, ir::Ty::Boolean);
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
            ast::ExprKind::Var(_) => todo!(),
            ast::ExprKind::Error(_) => todo!(),
            ast::ExprKind::Void => todo!(),
            ast::ExprKind::Begin(_) => todo!(),
        }
    }

    fn get_or_alloc_local(
        &mut self,
        local: Option<ir::Local>,
        span: Span,
        ty: ir::Ty,
    ) -> ir::Local {
        local.unwrap_or_else(|| self.body_builder.alloc(span, ty))
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

    fn start(&mut self, span: Span, ty: ir::Ty) {
        assert_eq!(
            Vec::<ir::LocalDecl>::new(),
            self.locals,
            "Body Builder already started"
        );
        self.alloc(span, ty);
    }

    fn terminate_block(&mut self, kind: ir::TerminatorKind, span: Span) -> ir::BasicBlock {
        let bb = ir::BasicBlock::new(self.bbs.len());
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

    fn alloc(&mut self, span: Span, ty: ir::Ty) -> ir::Local {
        let l = ir::Local::new(self.locals.len().try_into().expect("too many locals"));
        self.locals.push(ir::LocalDecl {
            span,
            ty,
            stack_offset: 0,
            size: 0,
        });
        l
    }

    fn load_i(&mut self, local: ir::Local, constant: ir::Constant, span: Span) {
        self.stmts.push(ir::Statement {
            span,
            kind: ir::StatementKind::LoadI(local, constant),
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
              (fn (|{|@init| (#script ()) 0:0..2}|) unit (0:0..2)
                (let ([_0 unit (0:0..2)]
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
              (fn (|{|@init| (#script ()) 0:0..2}|) unit (0:0..2)
                (let ([_0 unit (0:0..2)]
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
              (fn (|{|@init| (#script ()) 0:0..3}|) unit (0:0..3)
                (let ([_0 unit (0:0..3)]
                      [_1 boolean (0:0..3)])
                  (bb 0
                    (loadI _1 97 (0:0..3))
                    (return (0:0..3))))))
        "#]],
        );
        check(
            r"#\λ",
            expect![[r#"
            (pkg
              (fn (|{|@init| (#script ()) 0:0..4}|) unit (0:0..4)
                (let ([_0 unit (0:0..4)]
                      [_1 boolean (0:0..4)])
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
                  (fn (|{|@init| (#script ()) 0:0..3}|) unit (0:0..3)
                    (let ([_0 unit (0:0..3)]
                          [_1 boolean (0:0..3)])
                      (bb 0
                        (loadI _1 100 (0:0..3))
                        (return (0:0..3))))))
            "#]],
            );
        }
    }
}
