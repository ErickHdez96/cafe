use std::rc::Rc;

use crate::{
    diagnostics::Diagnostic,
    env::Env,
    symbol::Symbol,
    syntax::{ast, parser},
    ty::{BuiltinTys, Ty, TyCo},
    utils::Resolve,
};

type TyCoEnv<'tyc> = Env<'tyc, Symbol, TyCo>;

struct TypeChecker<'tyc> {
    module: ast::ModId,
    builtins: BuiltinTys,
    diagnostics: Vec<Diagnostic>,
    get_mod_interface: &'tyc dyn Fn(ast::ModId) -> Rc<ast::ModuleInterface>,
}

pub fn typecheck_module(
    module: &mut ast::Module,
    builtins: BuiltinTys,
    get_mod_interface: impl Fn(ast::ModId) -> Rc<ast::ModuleInterface>,
) -> Vec<Diagnostic> {
    let mut tc = TypeChecker {
        module: module.id,
        builtins,
        diagnostics: vec![],
        get_mod_interface: &get_mod_interface,
    };
    let tys = tc.mod_(module);
    module.types = Some(tys);
    tc.diagnostics
}

impl TypeChecker<'_> {
    fn mod_(&mut self, mod_: &mut ast::Module) -> Env<'static, Symbol, Rc<Ty>> {
        let mut tys = Env::default();

        let ast::ExprKind::Body(body) = &mut mod_.body.kind else {
            panic!(
                "expected a letrec as the body of a module: {:#?} - {:#?}",
                mod_.id.resolve(),
                mod_.body
            );
        };

        for item in body {
            match item {
                ast::Item::Import(_, _) => {}
                ast::Item::Mod(_, _) => {}
                ast::Item::Define(d) => {
                    tys.insert(d.name.value, TyCo::Ty(Rc::clone(&self.builtins.object)));
                    if let Some(e) = &mut d.expr {
                        self.expr(e, &mut tys);
                        if let Some(ty) = tys.get_immediate_mut(&d.name.value) {
                            *ty = TyCo::Ty(Rc::clone(&e.ty));
                        }
                    }
                }
                ast::Item::Expr(e) => {
                    self.expr(e, &mut tys);
                }
            }
        }

        tys.into_bindings()
            .into_iter()
            .map(|(k, ty)| (k, ty.into()))
            .collect::<Env<'_, _, _>>()
    }

    fn expr(&mut self, expr: &mut ast::Expr, tyenv: &mut TyCoEnv) -> Rc<Ty> {
        let ty = match &mut expr.kind {
            ast::ExprKind::Body(body) => {
                let mut tyenv = tyenv.enter();
                let mut ret = Rc::clone(&self.builtins.void);
                for item in body {
                    match item {
                        ast::Item::Import(_, _) => {}
                        ast::Item::Mod(_, _) => {}
                        ast::Item::Define(d) => {
                            tyenv.insert(d.name.value, TyCo::Ty(Rc::clone(&self.builtins.object)));
                            if let Some(e) = &mut d.expr {
                                self.expr(e, &mut tyenv);
                                if let Some(ty) = tyenv.get_immediate_mut(&d.name.value) {
                                    *ty = TyCo::Ty(Rc::clone(&e.ty));
                                }
                            }
                            ret = Rc::clone(&self.builtins.void);
                        }
                        ast::Item::Expr(e) => {
                            ret = self.expr(e, &mut tyenv);
                        }
                    }
                }
                ret
            }
            ast::ExprKind::Let { defs, exprs, .. } => {
                for _d in defs {
                    todo!()
                }
                let mut ret = None;
                for e in exprs {
                    ret = Some(self.expr(e, tyenv));
                }
                ret.unwrap()
            }
            ast::ExprKind::Quote(_) => Rc::clone(&self.builtins.object),
            ast::ExprKind::If(cond, r#true, r#false) => {
                self.expr(cond, tyenv);
                self.expr(r#true, tyenv);
                self.expr(r#false, tyenv);
                Rc::clone(&self.builtins.object)
            }
            ast::ExprKind::Lambda {
                formals,
                rest,
                expr,
            } => self.lambda(formals, rest.as_ref(), expr, tyenv),
            ast::ExprKind::List(l) => {
                for e in l {
                    self.expr(e, tyenv);
                }
                Rc::clone(&self.builtins.object)
            }
            ast::ExprKind::DottedList(_, _) => Rc::clone(&self.builtins.object),
            ast::ExprKind::Boolean(_) => Rc::clone(&self.builtins.boolean),
            ast::ExprKind::Char(_) => Rc::clone(&self.builtins.char),
            ast::ExprKind::Var(v) => match self.resolve(v, tyenv) {
                Some(v) => v.clone().into(),
                None => {
                    self.diagnostics.push(
                        Diagnostic::builder()
                            .msg(format!("undefined variable: {}", v.value))
                            .span(v.span)
                            .finish(),
                    );
                    Rc::clone(&self.builtins.object)
                }
            },
            ast::ExprKind::Error(_) => Rc::clone(&self.builtins.object),
            ast::ExprKind::Void => Rc::clone(&self.builtins.void),
            ast::ExprKind::Begin(exprs) => {
                let mut ret = None;
                for e in exprs {
                    ret = Some(self.expr(e, tyenv));
                }
                ret.unwrap()
            }
            ast::ExprKind::Number(n) => match n {
                parser::Number::Fixnum(_) => Rc::clone(&self.builtins.fixnum),
            },
        };
        expr.ty = Rc::clone(&ty);
        ty
    }

    fn lambda(
        &mut self,
        formals: &[ast::Ident],
        rest: Option<&ast::Ident>,
        expr: &mut ast::Expr,
        tyenv: &mut TyCoEnv,
    ) -> Rc<Ty> {
        let mut tyenv = tyenv.enter();
        for f in formals {
            if !tyenv.has_immediate(&f.value) {
                tyenv.insert(f.value, TyCo::Ty(Rc::clone(&self.builtins.object)));
            }
        }
        if let Some(r) = rest {
            tyenv.insert(r.value, TyCo::from_ty(&self.builtins.object));
        }
        let ty = self.expr(expr, &mut tyenv);
        Rc::new(Ty::Lambda {
            params: formals
                .iter()
                .map(|f| tyenv.get(&f.value).cloned().unwrap().into())
                .collect(),
            rest: rest.and_then(|r| tyenv.get(&r.value).cloned().map(|v| v.into())),
            ret: ty,
        })
    }

    fn resolve<'s>(&'s self, var: &ast::Path, tyenv: &'s TyCoEnv) -> Option<TyCo> {
        if self.module == var.module {
            tyenv.get(&var.value).cloned()
        } else {
            let mod_int = (self.get_mod_interface)(var.module);
            match mod_int.types.as_ref().and_then(|t| t.get(&var.value)) {
                Some(ty) => Some(TyCo::from_ty(ty)),
                None => {
                    panic!(
                        "Type for binding {} missing from module {}",
                        var.value,
                        mod_int.id.resolve()
                    );
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use crate::{compiler::Compiler, syntax::ast::ModuleName};

    fn check(input: &str, expected: Expect) {
        let mid = ModuleName::script();
        let compiler = Compiler::default();
        compiler.feed_file_contents(mid, "#script", input).unwrap();
        let module = compiler.typecheck_module(mid).unwrap();
        let mut out = String::new();

        let len = module.types.as_ref().unwrap().iter().len();
        for (i, (name, ty)) in module.types.as_ref().unwrap().iter().enumerate() {
            out.push_str(&format!(
                "{name}: {ty:#?}{}",
                if i + 1 < len { "\n" } else { "" }
            ));
        }

        expected.assert_eq(&out);
    }

    #[test]
    fn boolean() {
        check(
            "(import (rnrs expander core))
             (define a #t)",
            expect!["a: boolean"],
        );
    }

    #[test]
    fn char() {
        check(
            r"(import (rnrs expander core))
              (define a #\λ)",
            expect!["a: char"],
        );
    }

    #[test]
    fn number() {
        check(
            r"(import (rnrs expander core))
              (define a 1)",
            expect!["a: i64"],
        );
    }

    #[test]
    fn simple_lambda() {
        check(
            r"(import (rnrs expander core))
              (define a (lambda (x) x))",
            expect!["a: (-> object object)"],
        );
    }
}
