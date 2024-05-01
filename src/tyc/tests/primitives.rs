use super::*;

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
