use crate::{
    arena::Arena,
    ty::{NumberTy, Ty, TyK},
};

#[derive(Debug)]
pub struct Interner {
    pub types: Arena<TyK>,
    pub builtins: Builtins,
}

impl Default for Interner {
    fn default() -> Self {
        let mut types = Arena::default();
        let builtins = Builtins::new(&mut types);
        Self { types, builtins }
    }
}

impl Interner {}

#[derive(Debug)]
pub struct Builtins {
    pub types: BuiltinTys,
}

impl Builtins {
    pub fn new(arena: &mut Arena<TyK>) -> Self {
        let i64 = arena.alloc(TyK::Number(NumberTy::I64)).into();
        let void = arena.alloc(TyK::Void).into();
        Self {
            types: BuiltinTys {
                none: arena.alloc(TyK::None).into(),
                boolean: arena.alloc(TyK::Boolean).into(),
                char: arena.alloc(TyK::Char).into(),
                i64,
                void,
                array_i64: arena.alloc(TyK::Array(i64)).into(),
                error: arena.alloc(TyK::Error).into(),
                void_fn: arena
                    .alloc(TyK::Lambda {
                        params: vec![],
                        rest: None,
                        ret: void,
                        generics: vec![],
                    })
                    .into(),
            },
        }
    }
}

#[derive(Debug)]
pub struct BuiltinTys {
    pub none: Ty,
    pub boolean: Ty,
    pub char: Ty,
    pub i64: Ty,
    pub void: Ty,
    pub array_i64: Ty,
    pub error: Ty,
    pub void_fn: Ty,
}
