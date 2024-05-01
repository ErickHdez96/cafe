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
            {body 0:0..52
              {import (rnrs expander core ())@0:0..29}
              {define@0:39..13
                {|a| : boolean 0:47..1}
                {#t : boolean 0:49..2}}}
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
            {body 0:0..55
              {import (rnrs expander core ())@0:0..29}
              {define@0:40..15
                {|a| : char 0:48..1}
                {#\λ : char 0:50..4}}}
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
            {body 0:0..52
              {import (rnrs expander core ())@0:0..29}
              {define@0:40..12
                {|a| : i64 0:48..1}
                {1 : i64 0:50..1}}}
        "#]],
    );
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
                      ({|x| : boolean 0:68..1}} {|y| : '4 0:70..1}} {|z| : '4 0:72..1}})
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
                      ({|x| : boolean 0:68..1}} {|y| : boolean 0:70..1}} {|z| : boolean 0:72..1}})
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

mod function {
    use super::*;

    #[test]
    fn id() {
        check(
            r"(import (rnrs expander core))
              (define a (lambda (x) x))",
            expect!["a: (∀ '(a) (-> a a))"],
            expect![[r#"
                {body 0:0..69
                  {import (rnrs expander core ())@0:0..29}
                  {define@0:44..25
                    {|a| : (∀ '(a) (-> a a)) 0:52..1}
                    {λ : (-> '1 '1) 0:54..14
                      ({|x| : '1 0:63..1}})
                      #f
                      {body 0:54..14
                        {var |x| : '1 (#script ()) 0:66..1}}}}}
            "#]],
        );
    }

    #[test]
    fn first() {
        check(
            r"(import (rnrs expander core))
              (define fst (lambda (x y) x))",
            expect!["fst: (∀ '(a b) (-> a b a))"],
            expect![[r#"
                {body 0:0..73
                  {import (rnrs expander core ())@0:0..29}
                  {define@0:44..29
                    {|fst| : (∀ '(a b) (-> a b a)) 0:52..3}
                    {λ : (-> '1 '2 '1) 0:56..16
                      ({|x| : '1 0:65..1}} {|y| : '2 0:67..1}})
                      #f
                      {body 0:56..16
                        {var |x| : '1 (#script ()) 0:70..1}}}}}
            "#]],
        );
    }

    #[test]
    fn second() {
        check(
            r"(import (rnrs expander core))
              (define snd (lambda (x y) y))",
            expect!["snd: (∀ '(a b) (-> a b b))"],
            expect![[r#"
                {body 0:0..73
                  {import (rnrs expander core ())@0:0..29}
                  {define@0:44..29
                    {|snd| : (∀ '(a b) (-> a b b)) 0:52..3}
                    {λ : (-> '1 '2 '2) 0:56..16
                      ({|x| : '1 0:65..1}} {|y| : '2 0:67..1}})
                      #f
                      {body 0:56..16
                        {var |y| : '2 (#script ()) 0:70..1}}}}}
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
                {body 0:0..80
                  {import (rnrs expander core ())@0:0..29}
                  {define@0:44..36
                    {|double| : (∀ '() (-> i64 i64)) 0:52..6}
                    {λ : (-> i64 i64) 0:59..20
                      ({|a| : i64 0:68..1}})
                      #f
                      {body 0:59..20
                        {list 0:71..7
                          {var |+| : (-> i64 i64 i64) (rnrs intrinsics ()) 0:72..1}
                          {var |a| : i64 (#script ()) 0:74..1}
                          {var |a| : i64 (#script ()) 0:76..1}}}}}}
            "#]],
        );
    }
}

mod lambda {
    use super::*;

    #[test]
    fn id() {
        check(
            r"(import (rnrs expander core))
              (define a (lambda (x) ((lambda (x) x) x)))",
            expect!["a: (∀ '(a) (-> a a))"],
            expect![[r#"
                {body 0:0..86
                  {import (rnrs expander core ())@0:0..29}
                  {define@0:44..42
                    {|a| : (∀ '(a) (-> a a)) 0:52..1}
                    {λ : (-> '2 '2) 0:54..31
                      ({|x| : '2 0:63..1}})
                      #f
                      {body 0:54..31
                        {list 0:66..18
                          {λ : (-> '2 '2) 0:67..14
                            ({|x| : '2 0:76..1}})
                            #f
                            {body 0:67..14
                              {var |x| : '2 (#script ()) 0:79..1}}}
                          {var |x| : '2 (#script ()) 0:82..1}}}}}}
            "#]],
        );
    }
}
