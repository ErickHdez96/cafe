use expect_test::{expect, Expect};

use crate::{interner::Interner, test::test_lower_str};

pub fn check(input: &str, expected: Expect) {
    let mut interner = Interner::default();
    let res = test_lower_str(input, &mut interner);
    expected.assert_debug_eq(&res.display(&interner));
}

#[test]
fn boolean() {
    check(
        "#f",
        expect![[r#"
            (pkg
              (fn (|{|@init| (#script ()) 0:0..2}|) void (0:0..2)
                (let ([_0 void (0:0..2)]
                      [_1 boolean (0:0..2)])
                  (bb 0
                    (loadI _1 0 (0:0..2))
                    (return (0:0..2))))))
        "#]],
    );
    check(
        "#t",
        expect![[r#"
            (pkg
              (fn (|{|@init| (#script ()) 0:0..2}|) void (0:0..2)
                (let ([_0 void (0:0..2)]
                      [_1 boolean (0:0..2)])
                  (bb 0
                    (loadI _1 1 (0:0..2))
                    (return (0:0..2))))))
        "#]],
    );
}

#[test]
fn char() {
    check(
        r"#\a",
        expect![[r#"
            (pkg
              (fn (|{|@init| (#script ()) 0:0..3}|) void (0:0..3)
                (let ([_0 void (0:0..3)]
                      [_1 char (0:0..3)])
                  (bb 0
                    (loadI _1 97 (0:0..3))
                    (return (0:0..3))))))
        "#]],
    );
    check(
        r"#\λ",
        expect![[r#"
            (pkg
              (fn (|{|@init| (#script ()) 0:0..4}|) void (0:0..4)
                (let ([_0 void (0:0..4)]
                      [_1 char (0:0..4)])
                  (bb 0
                    (loadI _1 955 (0:0..4))
                    (return (0:0..4))))))
        "#]],
    );
}

mod numbers {
    use super::*;

    #[test]
    fn integer() {
        check(
            "100",
            expect![[r#"
                (pkg
                  (fn (|{|@init| (#script ()) 0:0..3}|) void (0:0..3)
                    (let ([_0 void (0:0..3)]
                          [_1 i64 (0:0..3)])
                      (bb 0
                        (loadI _1 100 (0:0..3))
                        (return (0:0..3))))))
            "#]],
        );
    }
}

mod conditionals {
    use super::*;

    #[test]
    fn simple_if() {
        check(
            "(if #t 1 2)",
            expect![[r#"
                (pkg
                  (fn (|{|@init| (#script ()) 0:0..11}|) void (0:0..11)
                    (let ([_0 void (0:0..11)]
                          [_1 i64 (0:0..11)]
                          [_2 boolean (0:4..2)]
                          [_3 i64 (0:7..1)]
                          [_4 i64 (0:9..1)])
                      (bb 0
                        (loadI _2 1 (0:4..2))
                        (cond _2 bb1 bb2 (0:4..2)))
                      (bb 1
                        (loadI _3 1 (0:7..1))
                        (copy _1 _3 (0:7..1))
                        (goto bb3 (0:7..1)))
                      (bb 2
                        (loadI _4 2 (0:9..1))
                        (copy _1 _4 (0:9..1))
                        (goto bb3 (0:9..1)))
                      (bb 3
                        (return (0:0..11))))))
            "#]],
        );
    }
}

mod functions {
    use super::*;

    #[test]
    fn call_function() {
        check(
            r"(define id (lambda (x) x))
              (id #t)
              (id #\a)",
            expect![[r#"
                (pkg
                  (fn (|{|id| (#script ()) 0:8..2}| [_1: '1]) '1 (0:0..26)
                    (let ([_0 '1 (0:0..26)]
                          [_1 '1 (0:20..1)])
                      (bb 0
                        (copy _0 _1 (0:23..1))
                        (return (0:0..26)))))
                  (fn (|{|@init| (#script ()) 0:0..71}|) void (0:0..71)
                    (let ([_0 void (0:0..71)]
                          [_1 boolean (0:41..7)]
                          [_2 (-> boolean boolean) (0:42..2)]
                          [_3 boolean (0:45..2)]
                          [_4 char (0:63..8)]
                          [_5 (-> char char) (0:64..2)]
                          [_6 char (0:67..3)])
                      (bb 0
                        (addressof _2 |{|id| (#script ()) 0:8..2}| (0:42..2))
                        (loadI _3 1 (0:45..2))
                        (arg _3 (0:45..2))
                        (call _1 _2 (0:41..7))
                        (addressof _5 |{|id| (#script ()) 0:8..2}| (0:64..2))
                        (loadI _6 97 (0:67..3))
                        (arg _6 (0:67..3))
                        (call _4 _5 (0:63..8))
                        (return (0:0..71))))))
            "#]],
        );
    }

    #[test]
    fn call_lambda() {
        check(
            r"((lambda (x) x) #\a)",
            expect![[r#"
                (pkg
                  (fn (|{|lambda0| (#script ()) 0:1..14}| [_1: char]) char (0:1..14)
                    (let ([_0 char (0:1..14)]
                          [_1 char (0:10..1)])
                      (bb 0
                        (copy _0 _1 (0:13..1))
                        (return (0:1..14)))))
                  (fn (|{|@init| (#script ()) 0:0..20}|) void (0:0..20)
                    (let ([_0 void (0:0..20)]
                          [_1 char (0:0..20)]
                          [_2 (-> char char) (0:1..14)]
                          [_3 char (0:16..3)])
                      (bb 0
                        (addressof _2 |{|lambda0| (#script ()) 0:1..14}| (0:1..14))
                        (loadI _3 97 (0:16..3))
                        (arg _3 (0:16..3))
                        (call _1 _2 (0:0..20))
                        (return (0:0..20))))))
            "#]],
        );
    }
}

mod module {
    use super::*;

    #[test]
    fn identity_call() {
        check(
            "(module (id ()) (id)
               (import (rnrs expander core ()))
               (define id (lambda (x) x)))
             (import (id ()))
             (id #t)",
            expect![[r#"
                (pkg
                  (fn (|{|id| (id ()) 0:92..2}| [_1: '1]) '1 (0:84..26)
                    (let ([_0 '1 (0:84..26)]
                          [_1 '1 (0:104..1)])
                      (bb 0
                        (copy _0 _1 (0:107..1))
                        (return (0:84..26)))))
                  (fn (|{|@init| (#script ()) 0:0..162}|) void (0:0..162)
                    (let ([_0 void (0:0..162)]
                          [_1 boolean (0:155..7)]
                          [_2 (-> boolean boolean) (0:156..2)]
                          [_3 boolean (0:159..2)])
                      (bb 0
                        (addressof _2 |{|id| (id ()) 0:92..2}| (0:156..2))
                        (loadI _3 1 (0:159..2))
                        (arg _3 (0:159..2))
                        (call _1 _2 (0:155..7))
                        (return (0:0..162))))))
            "#]],
        );
    }
}
