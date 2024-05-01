use super::*;

#[test]
fn id() {
    check(
        r"(import (rnrs expander core))
          (define a (lambda (x) x))",
        expect!["a: (∀ '(a) (-> a a))"],
        expect![[r#"
            {body 0:0..65
              {import (rnrs expander core ())@0:0..29}
              {define@0:40..25
                {|a| : (∀ '(a) (-> a a)) 0:48..1}
                {λ : (-> '1 '1) 0:50..14
                  ({|x| : '1 0:59..1}})
                  #f
                  {body 0:50..14
                    {var |x| : '1 (#script ()) 0:62..1}}}}}
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
            {body 0:0..69
              {import (rnrs expander core ())@0:0..29}
              {define@0:40..29
                {|fst| : (∀ '(a b) (-> a b a)) 0:48..3}
                {λ : (-> '1 '2 '1) 0:52..16
                  ({|x| : '1 0:61..1}} {|y| : '2 0:63..1}})
                  #f
                  {body 0:52..16
                    {var |x| : '1 (#script ()) 0:66..1}}}}}
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
            {body 0:0..69
              {import (rnrs expander core ())@0:0..29}
              {define@0:40..29
                {|snd| : (∀ '(a b) (-> a b b)) 0:48..3}
                {λ : (-> '1 '2 '2) 0:52..16
                  ({|x| : '1 0:61..1}} {|y| : '2 0:63..1}})
                  #f
                  {body 0:52..16
                    {var |y| : '2 (#script ()) 0:66..1}}}}}
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
            {body 0:0..76
              {import (rnrs expander core ())@0:0..29}
              {define@0:40..36
                {|double| : (∀ '() (-> i64 i64)) 0:48..6}
                {λ : (-> i64 i64) 0:55..20
                  ({|a| : i64 0:64..1}})
                  #f
                  {body 0:55..20
                    {list 0:67..7
                      {var |+| : (-> i64 i64 i64) (rnrs intrinsics ()) 0:68..1}
                      {var |a| : i64 (#script ()) 0:70..1}
                      {var |a| : i64 (#script ()) 0:72..1}}}}}}
        "#]],
    );
}

#[test]
fn apply_first_to_second() {
    check(
        r"(import (rnrs expander core))
          (define apply (lambda (fn x) (fn x)))",
        expect!["apply: (∀ '(a b) (-> (-> a b) a b))"],
        expect![[r#"
            {body 0:0..77
              {import (rnrs expander core ())@0:0..29}
              {define@0:40..37
                {|apply| : (∀ '(a b) (-> (-> a b) a b)) 0:48..5}
                {λ : (-> (-> '2 '3) '2 '3) 0:54..22
                  ({|fn| : (-> '2 '3) 0:63..2}} {|x| : '2 0:66..1}})
                  #f
                  {body 0:54..22
                    {list 0:69..6
                      {var |fn| : (-> '2 '3) (#script ()) 0:70..2}
                      {var |x| : '2 (#script ()) 0:73..1}}}}}}
        "#]],
    );
}
