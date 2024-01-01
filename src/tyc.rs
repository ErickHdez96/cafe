use std::rc::Rc;

use crate::{
    diagnostics::Diagnostic,
    env::Env,
    syntax::ast::{Expr, ExprKind, Ident, ModId, Module, ModuleInterface, Path},
    ty::{BuiltinTys, Ty, TyCo},
    utils::Resolve,
};

type TyCoEnv<'tyc> = Env<'tyc, String, TyCo>;

struct TypeChecker<'tyc> {
    module: ModId,
    builtins: BuiltinTys,
    diagnostics: Vec<Diagnostic>,
    get_mod_interface: &'tyc dyn Fn(ModId) -> Rc<ModuleInterface>,
}

pub fn typecheck_module(
    module: &Module,
    builtins: BuiltinTys,
    get_mod_interface: impl Fn(ModId) -> Rc<ModuleInterface>,
) -> (Env<'static, String, Rc<Ty>>, Vec<Diagnostic>) {
    let mut tc = TypeChecker {
        module: module.id,
        builtins,
        diagnostics: vec![],
        get_mod_interface: &get_mod_interface,
    };
    let tys = tc.mod_(module);
    (tys, tc.diagnostics)
}

impl TypeChecker<'_> {
    fn mod_(&mut self, mod_: &Module) -> Env<'static, String, Rc<Ty>> {
        let mut tys = Env::default();

        let ExprKind::LetRec { defs, exprs } = &mod_.body.kind else {
            panic!(
                "expected a letrec as the body of a module: {:#?}",
                mod_.body
            );
        };

        for def in defs {
            tys.insert(
                def.name.value.clone(),
                TyCo::Union(vec![match &def.expr {
                    Some(e) => self.expr(e, &tys),
                    None => Rc::clone(&self.builtins.uninit),
                }]),
            );
        }

        for exp in exprs {
            self.expr(exp, &tys);
        }

        tys.into_bindings()
            .into_iter()
            .map(|(k, ty)| (k, ty.into()))
            .collect::<Env<'_, _, _>>()
    }

    fn expr(&mut self, expr: &Expr, tyenv: &TyCoEnv) -> Rc<Ty> {
        match &expr.kind {
            ExprKind::LetRec { defs, exprs } => {
                for _d in defs {
                    todo!()
                }
                let mut ret = None;
                for e in exprs {
                    ret = Some(self.expr(e, tyenv));
                }
                ret.unwrap()
            }
            ExprKind::Quote(_) => Rc::clone(&self.builtins.object),
            ExprKind::If(_, _, _) => Rc::clone(&self.builtins.object),
            ExprKind::Lambda {
                formals,
                rest,
                expr,
            } => self.lambda(formals, rest.as_ref(), expr, tyenv),
            ExprKind::List(_) => Rc::clone(&self.builtins.object),
            ExprKind::DottedList(_, _) => Rc::clone(&self.builtins.object),
            ExprKind::Boolean(_) => Rc::clone(&self.builtins.boolean),
            ExprKind::Char(_) => Rc::clone(&self.builtins.char),
            ExprKind::Var(v) => match self.resolve(v, tyenv) {
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
            ExprKind::Error(_) => Rc::clone(&self.builtins.boolean),
            ExprKind::Void => Rc::clone(&self.builtins.void),
            ExprKind::Begin(exprs) => {
                let mut ret = None;
                for e in exprs {
                    ret = Some(self.expr(e, tyenv));
                }
                ret.unwrap()
            }
        }
    }

    fn lambda(
        &mut self,
        formals: &[Ident],
        rest: Option<&Ident>,
        expr: &Expr,
        tyenv: &TyCoEnv,
    ) -> Rc<Ty> {
        let mut tyenv = tyenv.enter();
        for f in formals {
            if !tyenv.has_immediate(&f.value) {
                tyenv.insert(f.value.clone(), TyCo::new_generic());
            }
        }
        if let Some(r) = rest {
            tyenv.insert(r.value.clone(), TyCo::from_ty(&self.builtins.object));
        }
        let expr = self.expr(expr, &tyenv);
        Rc::new(Ty::Lambda {
            params: formals
                .iter()
                .map(|f| tyenv.get(&f.value).cloned().unwrap().into())
                .collect(),
            rest: rest.and_then(|r| tyenv.get(&r.value).cloned().map(|v| v.into())),
            ret: expr,
        })
    }

    fn resolve<'s>(&'s self, var: &Path, tyenv: &'s TyCoEnv) -> Option<TyCo> {
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
    fn simple_lambda() {
        check(
            r"(import (rnrs expander core))
              (define a (lambda (x) x))",
            expect!["a: (-> '0 '0)"],
        );
    }
}
