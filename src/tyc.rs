use std::rc::Rc;

use crate::{
    diagnostics::Diagnostic,
    env::Env,
    syntax::ast::{Expr, ExprKind, ModId, Module, ModuleInterface},
    ty::{BuiltinTys, Ty},
};

#[derive(Debug)]
struct TypeChecker {
    builtins: BuiltinTys,
    diagnostics: Vec<Diagnostic>,
}

pub fn typecheck_module(
    module: &Module,
    builtins: BuiltinTys,
    _get_mod_interface: impl Fn(ModId) -> Rc<ModuleInterface>,
) -> (Env<'static, String, Rc<Ty>>, Vec<Diagnostic>) {
    let mut tc = TypeChecker {
        builtins,
        diagnostics: vec![],
    };
    let tys = tc.mod_(module);
    (tys, tc.diagnostics)
}

impl TypeChecker {
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
                match &def.expr {
                    Some(e) => self.expr(e),
                    None => Rc::clone(&self.builtins.uninit),
                },
            );
        }

        for exp in exprs {
            self.expr(exp);
        }

        tys
    }

    fn expr(&mut self, expr: &Expr) -> Rc<Ty> {
        match &expr.kind {
            ExprKind::LetRec { .. } => todo!(),
            ExprKind::Quote(_) => todo!(),
            ExprKind::If(_, _, _) => todo!(),
            ExprKind::Lambda { .. } => todo!(),
            ExprKind::List(_) => todo!(),
            ExprKind::DottedList(_, _) => todo!(),
            ExprKind::Boolean(_) => Rc::clone(&self.builtins.boolean),
            ExprKind::Char(_) => todo!(),
            ExprKind::Var(_) => todo!(),
            ExprKind::Error(_) => todo!(),
            ExprKind::Void => Rc::clone(&self.builtins.void),
            ExprKind::Begin(_) => todo!(),
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

        for (name, ty) in module.types.as_ref().unwrap().iter() {
            out.push_str(&format!("{name}: {ty:?}\n"));
        }

        expected.assert_eq(&out);
    }

    #[test]
    fn boolean() {
        check(
            "(import (rnrs expander core))
             (define a #t)",
            expect![[""]],
        );
    }
}
