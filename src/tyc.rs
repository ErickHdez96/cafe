use crate::{
    diagnostics::Diagnostic,
    query::QCtx,
    syntax::ast::{Expr, ExprKind, Module},
};

#[derive(Debug)]
struct TypeChecker {
    diagnostics: Vec<Diagnostic>,
}

pub fn typecheck_module(
    module: &Module,
    //type_of: &dyn impl Fn(&Path) -> Ty,
) -> Vec<Diagnostic> {
    let mut tc = TypeChecker {
        diagnostics: vec![],
    };
    tc.expr(&module.body);
    tc.diagnostics
}

impl TypeChecker {
    fn expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::LetRec { defs, exprs } => todo!(),
            ExprKind::Quote(_) => todo!(),
            ExprKind::If(_, _, _) => todo!(),
            ExprKind::Lambda {
                formals,
                rest,
                exprs,
            } => todo!(),
            ExprKind::List(_) => todo!(),
            ExprKind::DottedList(_, _) => todo!(),
            ExprKind::Boolean(_) => todo!(),
            ExprKind::Char(_) => todo!(),
            ExprKind::Var(_) => todo!(),
            ExprKind::Error(_) => todo!(),
            ExprKind::Void => todo!(),
            ExprKind::Begin(_) => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {}
