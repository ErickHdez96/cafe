use expect_test::{expect, Expect};

use crate::{
    arena::WithOptionalArena,
    interner::Interner,
    test::{test_expand_str_with_libs, typecheck_id, Libs},
};

mod application;
mod function;
mod primitives;

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

mod if_ {
    use super::*;

    #[test]
    fn select() {
        check(
            r"(import (rnrs expander core))
              (define select (lambda (x y z) (if x y z)))",
            expect!["select: (∀ '(a) (-> boolean a a a))"],
            expect![[r#"
                {body 0:0..87
                  {import (rnrs expander core ())@0:0..29}
                  {define@0:44..43
                    {|select| : (∀ '(a) (-> boolean a a a)) 0:52..6}
                    {λ : (-> boolean '4 '4 '4) 0:59..27
                      ({|x| : boolean 0:68..1} {|y| : '4 0:70..1} {|z| : '4 0:72..1})
                      #f
                      {body 0:59..27
                        {if 0:75..10
                          {var |x| : boolean (#script ()) 0:79..1}
                          {var |y| : '4 (#script ()) 0:81..1}
                          {var |z| : '4 (#script ()) 0:83..1}}}}}}
            "#]],
        );
    }

    #[test]
    fn return_char_or_second_variable() {
        check(
            r"(import (rnrs expander core))
              (define select (lambda (x y) (if x #\a y)))",
            expect!["select: (∀ '() (-> boolean char char))"],
            expect![[r#"
                {body 0:0..87
                  {import (rnrs expander core ())@0:0..29}
                  {define@0:44..43
                    {|select| : (∀ '() (-> boolean char char)) 0:52..6}
                    {λ : (-> boolean char char) 0:59..27
                      ({|x| : boolean 0:68..1} {|y| : char 0:70..1})
                      #f
                      {body 0:59..27
                        {if 0:73..12
                          {var |x| : boolean (#script ()) 0:77..1}
                          {#\a : char 0:79..3}
                          {var |y| : char (#script ()) 0:83..1}}}}}}
            "#]],
        );
    }

    #[test]
    fn nested_if_else() {
        check(
            r"(import (rnrs expander core))
              (define select (lambda (x y z) (if x y (if z x y))))",
            expect!["select: (∀ '() (-> boolean boolean boolean boolean))"],
            expect![[r#"
                {body 0:0..96
                  {import (rnrs expander core ())@0:0..29}
                  {define@0:44..52
                    {|select| : (∀ '() (-> boolean boolean boolean boolean)) 0:52..6}
                    {λ : (-> boolean boolean boolean boolean) 0:59..36
                      ({|x| : boolean 0:68..1} {|y| : boolean 0:70..1} {|z| : boolean 0:72..1})
                      #f
                      {body 0:59..36
                        {if 0:75..19
                          {var |x| : boolean (#script ()) 0:79..1}
                          {var |y| : boolean (#script ()) 0:81..1}
                          {if 0:83..10
                            {var |z| : boolean (#script ()) 0:87..1}
                            {var |x| : boolean (#script ()) 0:89..1}
                            {var |y| : boolean (#script ()) 0:91..1}}}}}}}
            "#]],
        );
    }
}
