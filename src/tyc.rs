use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::{
    arena::Arena,
    diagnostics::Diagnostic,
    env::Env,
    interner::BuiltinTys,
    span::Span,
    symbol::Symbol,
    syntax::{ast, parser},
    ty::{Ty, TyK, TyScheme},
};

type TyEnv<'tyc> = Env<'tyc, Symbol, TyScheme>;
type TyArena = Arena<TyK>;

struct TypeChecker<'tyc> {
    module: ast::ModId,
    arena: &'tyc mut TyArena,
    builtins: &'tyc BuiltinTys,
    diagnostics: Vec<Diagnostic>,
    get_mod_interface: &'tyc dyn Fn(ast::ModId) -> Rc<ast::ModuleInterface>,
    engine: InferEngine,
}

pub fn typecheck_module(
    module: &mut ast::Module,
    arena: &mut TyArena,
    builtins: &BuiltinTys,
    get_mod_interface: impl Fn(ast::ModId) -> Rc<ast::ModuleInterface>,
) -> Vec<Diagnostic> {
    let mut tc = TypeChecker {
        module: module.id,
        arena,
        builtins,
        diagnostics: vec![],
        get_mod_interface: &get_mod_interface,
        engine: InferEngine::default(),
    };
    let tys = tc.mod_(module);
    module.types = Some(tys);
    tc.diagnostics
}

impl TypeChecker<'_> {
    fn mod_(&mut self, mod_: &mut ast::Module) -> Env<'static, Symbol, TyScheme> {
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
                    tys.insert(d.name.value, self.new_var().into());
                    if let Some(e) = &mut d.expr {
                        let ty = generalize(self.expr(e, &mut tys), &tys, self.arena);
                        d.ty = Some(ty.clone());
                        if let Some(tys) = tys.get_immediate_mut(&d.name.value) {
                            *tys = ty;
                        }
                        self.engine.substitute_expr(e, self.arena);
                    }
                }
                ast::Item::Expr(e) => {
                    self.expr(e, &mut tys);
                }
            }
        }

        tys.into_bindings().into_iter().collect()
    }

    fn expr(&mut self, expr: &mut ast::Expr, tyenv: &mut TyEnv) -> Ty {
        let ty = match &mut expr.kind {
            // Primitives
            ast::ExprKind::Boolean(_) => self.builtins.boolean,
            ast::ExprKind::Char(_) => self.builtins.char,
            ast::ExprKind::Void => self.builtins.void,
            ast::ExprKind::Number(n) => match n {
                parser::Number::Fixnum(_) => self.builtins.i64,
            },
            // TODO: Check if primitive
            // TODO: Add type for syntax
            ast::ExprKind::Quote(_) => todo!(),

            ast::ExprKind::Body(body) => {
                let mut tyenv = tyenv.enter();
                let mut ret = self.builtins.void;
                for item in body {
                    match item {
                        ast::Item::Import(_, _) => {}
                        ast::Item::Mod(_, _) => {}
                        ast::Item::Define(d) => {
                            tyenv.insert(d.name.value, self.new_var().into());
                            if let Some(e) = &mut d.expr {
                                let ty = generalize(self.expr(e, &mut tyenv), &tyenv, self.arena);
                                d.ty = Some(ty.clone());
                                if let Some(tys) = tyenv.get_immediate_mut(&d.name.value) {
                                    *tys = ty;
                                }
                            }
                            ret = self.builtins.void;
                        }
                        ast::Item::Expr(e) => {
                            ret = self.expr(e, &mut tyenv);
                        }
                    }
                }
                ret
            }
            //ast::ExprKind::Let { defs, exprs, .. } => {
            //    for _d in defs {
            //        todo!()
            //    }
            //    let mut ret = None;
            //    for e in exprs {
            //        ret = Some(self.expr(e, tyenv));
            //    }
            //    ret.unwrap()
            //}
            //ast::ExprKind::If(cond, r#true, r#false) => {
            //    self.expr(cond, tyenv);
            //    self.expr(r#true, tyenv);
            //    self.expr(r#false, tyenv);
            //    self.builtins.object.into()
            //}
            ast::ExprKind::Lambda {
                formals,
                rest,
                expr,
                formal_tys,
            } => self.lambda(formals, rest.as_ref(), formal_tys, expr, tyenv),
            ast::ExprKind::List(elems) => {
                let Some(fnexpr) = elems.get_mut(0) else {
                    return self.builtins.error;
                };

                let fnty = self.expr(fnexpr, tyenv);
                let mut arg_tys = vec![];
                for a in elems.iter_mut().skip(1) {
                    arg_tys.push(self.expr(a, tyenv));
                }

                let retty = self.new_var();
                self.engine.unify(
                    fnty,
                    self.arena
                        .alloc(TyK::Lambda {
                            params: arg_tys,
                            rest: None,
                            ret: retty,
                            generics: vec![],
                        })
                        .into(),
                    expr.span,
                    self.arena,
                );

                self.engine.substitute(retty, self.arena)
            }
            //ast::ExprKind::DottedList(_, _) => self.builtins.object.into(),
            ast::ExprKind::Var(v) => match self.resolve(v, tyenv) {
                Some(tys) => self.engine.instantiate(&tys, self.arena),
                None => {
                    self.diagnostics.push(
                        Diagnostic::builder()
                            .msg(format!("undefined variable: {}", v.value))
                            .span(v.span)
                            .finish(),
                    );
                    self.builtins.error
                }
            },
            //ast::ExprKind::Error(_) => self.builtins.object,
            //ast::ExprKind::Begin(exprs) => {
            //    let mut ret = None;
            //    for e in exprs {
            //        ret = Some(self.expr(e, tyenv));
            //    }
            //    ret.unwrap()
            //}
            e => todo!("{e:#?}"),
        };
        expr.ty_hint = Some(ty);
        ty
    }

    fn lambda(
        &mut self,
        formals: &[ast::Ident],
        rest: Option<&ast::Ident>,
        formal_tys: &mut Vec<TyScheme>,
        expr: &mut ast::Expr,
        ptyenv: &mut TyEnv,
    ) -> Ty {
        let mut tyenv = ptyenv.enter();
        let mut a_params = Vec::with_capacity(formals.len());
        for f in formals {
            if !tyenv.has_immediate(&f.value) {
                let ty = self.new_var();
                tyenv.insert(f.value, ty.into());
                a_params.push(ty);
            }
        }
        assert_eq!(rest, None, "rest parameters not supported yet");

        let retty = self.expr(expr, &mut tyenv);
        let ty = self.engine.substitute(
            self.arena
                .alloc(TyK::Lambda {
                    params: a_params,
                    rest: None,
                    ret: retty,
                    generics: vec![],
                })
                .into(),
            self.arena,
        );
        for f in formals {
            formal_tys.push(
                self.resolve(
                    &ast::Path {
                        module: self.module,
                        span: f.span,
                        value: f.value,
                    },
                    &tyenv,
                )
                .unwrap(),
            );
        }
        ty
    }

    fn resolve<'s>(&'s mut self, var: &ast::Path, tyenv: &'s TyEnv) -> Option<TyScheme> {
        if self.module == var.module {
            tyenv
                .get(&var.value)
                .map(|tys| self.engine.substitute_tys(tys, self.arena))
        } else {
            let mod_int = (self.get_mod_interface)(var.module);
            match mod_int.types.as_ref().and_then(|t| t.get(&var.value)) {
                Some(tys) => Some(self.engine.substitute_tys(tys, self.arena)),
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

    fn new_var(&mut self) -> Ty {
        self.engine.new_var(self.arena)
    }
}

#[derive(Debug, Default)]
struct InferEngine {
    generic_id: usize,
    substitutions: Vec<Substitution>,
}

impl InferEngine {
    fn new_var(&mut self, arena: &mut TyArena) -> Ty {
        let id = self.generic_id;
        self.generic_id += self.generic_id;
        arena.alloc(TyK::Var(id)).into()
    }

    fn unify(&mut self, left: Ty, right: Ty, span: Span, arena: &TyArena) {
        if left == right {
            return;
        }

        match (arena.get(left.value()), arena.get(right.value())) {
            // TODO: Check if right occurs in left
            (_, TyK::Var(_)) => self.substitutions.push(Substitution::Eq {
                left: right,
                right: left,
            }),
            // TODO: Check if left occurs in right
            (TyK::Var(_), _) => self.substitutions.push(Substitution::Eq { left, right }),
            (
                TyK::Lambda {
                    params: lparams,
                    ret: lret,
                    ..
                },
                TyK::Lambda {
                    params: rparams,
                    ret: rret,
                    ..
                },
            ) if lparams.len() == rparams.len() => {
                for (left, right) in lparams.iter().zip(rparams) {
                    self.unify(*left, *right, span, arena);
                }
                self.unify(*lret, *rret, span, arena);
            }
            (_, _) => panic!(
                "unify: {:#?}, {:#?} - {span}",
                left.with_arena(arena),
                right.with_arena(arena)
            ),
        }
    }

    fn substitute(&self, ty: Ty, arena: &mut TyArena) -> Ty {
        self.do_substitute_ty(ty, &HashSet::default(), arena)
    }

    fn substitute_tys(&self, tyscheme: &TyScheme, arena: &mut TyArena) -> TyScheme {
        self.do_substitute_tys(tyscheme, &mut HashSet::default(), arena)
    }

    fn substitute_expr(&self, expr: &mut ast::Expr, arena: &mut TyArena) {
        fn visit_item(item: &mut ast::Item, engine: &InferEngine, arena: &mut TyArena) {
            match item {
                ast::Item::Import(_, _) | ast::Item::Mod(_, _) => {}
                ast::Item::Define(d) => visit_define(d, engine, arena),
                ast::Item::Expr(e) => visit_expr(e, engine, arena),
            }
        }

        fn visit_define(define: &mut ast::Define, engine: &InferEngine, arena: &mut TyArena) {
            if let Some(ty) = &define.ty {
                define.ty = Some(engine.substitute_tys(ty, arena));
            }

            if let Some(e) = &mut define.expr {
                visit_expr(e, engine, arena);
            }
        }

        fn visit_expr(expr: &mut ast::Expr, engine: &InferEngine, arena: &mut TyArena) {
            if let Some(ty) = expr.ty_hint {
                expr.ty_hint = Some(engine.substitute(ty, arena));
            }

            match &mut expr.kind {
                ast::ExprKind::Boolean(_)
                | ast::ExprKind::Char(_)
                | ast::ExprKind::Number(_)
                | ast::ExprKind::Var(_)
                | ast::ExprKind::Error(_)
                | ast::ExprKind::Void => {}

                ast::ExprKind::Body(b) => {
                    for i in b {
                        visit_item(i, engine, arena);
                    }
                }
                ast::ExprKind::Let { .. } => todo!(),
                ast::ExprKind::Quote(_) => todo!(),
                ast::ExprKind::If(_, _, _) => todo!(),
                ast::ExprKind::Lambda { expr, .. } => {
                    visit_expr(expr, engine, arena);
                }
                ast::ExprKind::List(exprs) => {
                    for e in exprs {
                        visit_expr(e, engine, arena);
                    }
                }
                ast::ExprKind::DottedList(_, _) => todo!(),
                ast::ExprKind::Begin(_) => todo!(),
            }
        }

        visit_expr(expr, self, arena);
    }

    fn do_substitute_tys(
        &self,
        tyscheme: &TyScheme,
        bounded: &mut HashSet<Ty>,
        arena: &mut TyArena,
    ) -> TyScheme {
        match tyscheme {
            TyScheme::STy(ty) => self.do_substitute_ty(*ty, bounded, arena).into(),
            TyScheme::QTy(generics, tyscheme) => {
                for g in generics {
                    bounded.insert(*g);
                }
                let ty = self.do_substitute_tys(tyscheme, bounded, arena);
                for g in generics {
                    bounded.remove(g);
                }
                ty
            }
        }
    }

    fn do_substitute_ty(&self, ty: Ty, bounded: &HashSet<Ty>, arena: &mut TyArena) -> Ty {
        match arena.get(ty.value()) {
            TyK::None
            | TyK::SObject
            | TyK::Boolean
            | TyK::Char
            | TyK::Number(_)
            | TyK::String
            | TyK::Null
            | TyK::Void
            | TyK::Symbol
            | TyK::Uninit
            | TyK::Error => ty,

            TyK::Lambda {
                params, rest, ret, ..
            } => {
                let params = params.clone();
                assert_eq!(None, rest.as_ref());
                let ret = *ret;

                let param_tys = params
                    .into_iter()
                    .map(|ty| self.do_substitute_ty(ty, bounded, arena))
                    .collect::<Vec<_>>();
                let retty = self.do_substitute_ty(ret, bounded, arena);

                match arena.get_mut(ty.value()) {
                    TyK::Lambda { params, ret, .. } => {
                        *params = param_tys;
                        *ret = retty;
                    }
                    _ => panic!("expected lambda"),
                }

                ty
            }
            TyK::Pair(_, _) => todo!(),
            TyK::Var(_) if !bounded.contains(&ty) => match self
                .substitutions
                .iter()
                .find(|Substitution::Eq { left, .. }| *left == ty)
            {
                Some(Substitution::Eq { right, .. }) => *right,
                None => ty,
            },
            TyK::Var(_) => ty,
            TyK::Array(_) => todo!(),
        }
    }

    fn instantiate(&mut self, mut tys: &TyScheme, arena: &mut TyArena) -> Ty {
        let mut substs = HashMap::new();

        loop {
            match tys {
                TyScheme::STy(ty) => return subst(*ty, &substs, arena),
                TyScheme::QTy(generics, tys_next) => {
                    for g in generics {
                        substs.insert(*g, self.new_var(arena));
                    }
                    tys = tys_next;
                }
            }
        }

        fn subst(ty: Ty, substs: &HashMap<Ty, Ty>, arena: &mut TyArena) -> Ty {
            match arena.get(ty.value()) {
                TyK::None
                | TyK::SObject
                | TyK::Boolean
                | TyK::Char
                | TyK::Number(_)
                | TyK::String
                | TyK::Null
                | TyK::Void
                | TyK::Symbol
                | TyK::Uninit
                | TyK::Error => ty,
                TyK::Lambda {
                    params,
                    rest,
                    ret,
                    generics,
                } => {
                    let params = params.clone();
                    let rest = *rest;
                    let ret = *ret;
                    let generics = generics.to_vec();
                    let params = params
                        .into_iter()
                        .map(|ty| subst(ty, substs, arena))
                        .collect();
                    rest.map(|rest| subst(rest, substs, arena));
                    let ret = subst(ret, substs, arena);
                    arena
                        .alloc(TyK::Lambda {
                            params,
                            rest,
                            ret,
                            generics,
                        })
                        .into()
                }
                TyK::Pair(_, _) => todo!(),
                TyK::Var(_) => match substs.get(&ty) {
                    Some(ty) => *ty,
                    None => ty,
                },
                TyK::Array(_) => todo!(),
            }
        }
    }
}

#[derive(Debug)]
enum Substitution {
    Eq { left: Ty, right: Ty },
}

fn generalize(ty: Ty, env: &TyEnv, arena: &mut TyArena) -> TyScheme {
    if !ty.with_arena(arena).is_fn() {
        return ty.into();
    }
    let free_vars_in_ty = free_variables_in_type(ty, arena);
    let free_vars_in_env = free_variables_in_env(env, arena);
    let diff = free_vars_in_ty
        .difference(&free_vars_in_env)
        .copied()
        .collect();
    TyScheme::QTy(diff, Rc::new(ty.into()))
}

/// Returns the free variables in `ty`.
fn free_variables_in_type(ty: Ty, arena: &TyArena) -> HashSet<Ty> {
    let mut free = HashSet::default();
    do_free_variables_in_type(ty, &mut free, &HashSet::default(), arena);
    free
}

fn free_variables_in_env(env: &TyEnv, arena: &TyArena) -> HashSet<Ty> {
    let mut free = HashSet::default();
    let mut bound = HashSet::default();

    fn do_free(env: &TyEnv, free: &mut HashSet<Ty>, bound: &mut HashSet<Ty>, arena: &TyArena) {
        for (_, ty) in env.bindings() {
            do_free_variables_in_type_scheme(ty, free, bound, arena);
        }
        if let Some(parent) = env.parent() {
            do_free(parent, free, bound, arena);
        }
    }

    do_free(env, &mut free, &mut bound, arena);
    free
}

fn do_free_variables_in_type(ty: Ty, free: &mut HashSet<Ty>, bound: &HashSet<Ty>, arena: &TyArena) {
    match arena.get(ty.value()) {
        TyK::None
        | TyK::SObject
        | TyK::Boolean
        | TyK::Char
        | TyK::Number(_)
        | TyK::String
        | TyK::Null
        | TyK::Void
        | TyK::Symbol
        | TyK::Uninit
        | TyK::Error => {}
        TyK::Lambda {
            params, rest, ret, ..
        } => {
            for p in params {
                do_free_variables_in_type(*p, free, bound, arena);
            }
            assert_eq!(None, rest.as_ref());
            do_free_variables_in_type(*ret, free, bound, arena);
        }
        TyK::Pair(_, _) => todo!(),
        TyK::Var(_) => {
            if !bound.contains(&ty) {
                free.insert(ty);
            }
        }
        TyK::Array(ty) => do_free_variables_in_type(*ty, free, bound, arena),
    }
}

fn do_free_variables_in_type_scheme(
    tyscheme: &TyScheme,
    free: &mut HashSet<Ty>,
    bound: &mut HashSet<Ty>,
    arena: &TyArena,
) {
    match tyscheme {
        TyScheme::STy(ty) => {
            do_free_variables_in_type(*ty, free, bound, arena);
        }
        TyScheme::QTy(generics, tyscheme) => {
            for g in generics {
                bound.insert(*g);
            }
            do_free_variables_in_type_scheme(tyscheme, free, bound, arena);
            for g in generics {
                bound.remove(g);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};

    use crate::{
        arena::WithOptionalArena,
        interner::Interner,
        test::{test_expand_str_with_libs, typecheck_id, Libs},
    };

    fn check(input: &str, expected_bindings: Expect, expected_mod: Expect) {
        let mut interner = Interner::default();
        let libs = Libs::new(&mut interner);
        let module = test_expand_str_with_libs(input, &libs, &mut interner).module;
        let mid = module.id;
        libs.define(mid, module);
        typecheck_id(&libs, mid, &mut interner);
        let store = libs.modules.borrow();
        let module = store.get(&mid).unwrap();

        let mut out = String::new();

        let len = module.types.as_ref().unwrap().iter().len();
        for (i, (name, ty)) in module.types.as_ref().unwrap().iter().enumerate() {
            out.push_str(&format!(
                "{name}: {:#?}{}",
                ty.with_arena(&interner.types),
                if i + 1 < len { "\n" } else { "" },
            ));
        }

        expected_bindings.assert_eq(&out);
        expected_mod.assert_debug_eq(&WithOptionalArena {
            arena: Some(&interner.types),
            item: &module.body,
        });
    }

    #[test]
    fn boolean() {
        check(
            "(import (rnrs expander core))
             (define a #t)",
            expect!["a: boolean"],
            expect![[r#"
                {body 0:0..56
                  {import (rnrs expander core ())@0:0..29}
                  {define@0:43..13
                    {|a| : boolean 0:51..1}
                    {#t : boolean 0:53..2}}}
            "#]],
        );
    }

    #[test]
    fn char() {
        check(
            r"(import (rnrs expander core))
              (define a #\λ)",
            expect!["a: char"],
            expect![[r#"
                {body 0:0..59
                  {import (rnrs expander core ())@0:0..29}
                  {define@0:44..15
                    {|a| : char 0:52..1}
                    {#\λ : char 0:54..4}}}
            "#]],
        );
    }

    #[test]
    fn number() {
        check(
            r"(import (rnrs expander core))
              (define a 1)",
            expect!["a: i64"],
            expect![[r#"
                {body 0:0..56
                  {import (rnrs expander core ())@0:0..29}
                  {define@0:44..12
                    {|a| : i64 0:52..1}
                    {1 : i64 0:54..1}}}
            "#]],
        );
    }

    mod function {
        use super::*;

        #[test]
        fn id() {
            check(
                r"(import (rnrs expander core))
                  (define a (lambda (x) x))",
                expect!["a: (∀ '(a) (-> a a))"],
                expect![[r#"
                    {body 0:0..73
                      {import (rnrs expander core ())@0:0..29}
                      {define@0:48..25
                        {|a| : (∀ '(a) (-> a a)) 0:56..1}
                        {λ : (-> '0 '0) 0:58..14
                          ({|x| : '0 0:67..1}})
                          #f
                          {body 0:58..14
                            {var |x| : '0 (#script ()) 0:70..1}}}}}
                "#]],
            );
        }

        #[test]
        fn simple_number() {
            check(
                r"(import (rnrs expander core))
                  (define double (lambda (a) (+ a a)))",
                expect!["double: (∀ '() (-> i64 i64))"],
                expect![[r#"
                    {body 0:0..84
                      {import (rnrs expander core ())@0:0..29}
                      {define@0:48..36
                        {|double| : (∀ '() (-> i64 i64)) 0:56..6}
                        {λ : (-> i64 i64) 0:63..20
                          ({|a| : i64 0:72..1}})
                          #f
                          {body 0:63..20
                            {list 0:75..7
                              {var |+| : (-> i64 i64 i64) (rnrs intrinsics ()) 0:76..1}
                              {var |a| : i64 (#script ()) 0:78..1}
                              {var |a| : i64 (#script ()) 0:80..1}}}}}}
                "#]],
            );
        }
    }
}
