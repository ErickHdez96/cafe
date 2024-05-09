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
        "(define _ #f)",
        expect![[r#"
            (pkg
              (fn (|{|@init| (#script ()) 0:0..13}|) void (0:0..13)
                (let ([_0 void (0:0..13)]
                      [_1 boolean (0:10..2)])
                  (bb 0
                    (loadI _1 0 (0:10..2))
                    (store-label |{|_| (#script ()) 0:8..1}| _1 (0:10..2))
                    (return (0:0..13))))))
        "#]],
    );
    check(
        "(define _ #t)",
        expect![[r#"
            (pkg
              (fn (|{|@init| (#script ()) 0:0..13}|) void (0:0..13)
                (let ([_0 void (0:0..13)]
                      [_1 boolean (0:10..2)])
                  (bb 0
                    (loadI _1 1 (0:10..2))
                    (store-label |{|_| (#script ()) 0:8..1}| _1 (0:10..2))
                    (return (0:0..13))))))
        "#]],
    );
}

#[test]
fn char() {
    check(
        r"(define _ #\a)",
        expect![[r#"
            (pkg
              (fn (|{|@init| (#script ()) 0:0..14}|) void (0:0..14)
                (let ([_0 void (0:0..14)]
                      [_1 char (0:10..3)])
                  (bb 0
                    (loadI _1 97 (0:10..3))
                    (store-label |{|_| (#script ()) 0:8..1}| _1 (0:10..3))
                    (return (0:0..14))))))
        "#]],
    );
    check(
        r"(define _ #\λ)",
        expect![[r#"
            (pkg
              (fn (|{|@init| (#script ()) 0:0..15}|) void (0:0..15)
                (let ([_0 void (0:0..15)]
                      [_1 char (0:10..4)])
                  (bb 0
                    (loadI _1 955 (0:10..4))
                    (store-label |{|_| (#script ()) 0:8..1}| _1 (0:10..4))
                    (return (0:0..15))))))
        "#]],
    );
}

mod numbers {
    use super::*;

    #[test]
    fn integer() {
        check(
            "(define _ 100)",
            expect![[r#"
                (pkg
                  (fn (|{|@init| (#script ()) 0:0..14}|) void (0:0..14)
                    (let ([_0 void (0:0..14)]
                          [_1 i64 (0:10..3)])
                      (bb 0
                        (loadI _1 100 (0:10..3))
                        (store-label |{|_| (#script ()) 0:8..1}| _1 (0:10..3))
                        (return (0:0..14))))))
            "#]],
        );
    }
}

mod conditionals {
    use super::*;

    #[test]
    fn simple_if() {
        check(
            "(define _ (if #t 1 2))",
            expect![[r#"
                (pkg
                  (fn (|{|@init| (#script ()) 0:0..22}|) void (0:0..22)
                    (let ([_0 void (0:0..22)]
                          [_1 i64 (0:10..11)]
                          [_2 boolean (0:14..2)]
                          [_3 i64 (0:17..1)]
                          [_4 i64 (0:19..1)])
                      (bb 0
                        (loadI _2 1 (0:14..2))
                        (cond _2 bb1 bb2 (0:14..2)))
                      (bb 1
                        (loadI _3 1 (0:17..1))
                        (copy _1 _3 (0:17..1))
                        (goto bb3 (0:17..1)))
                      (bb 2
                        (loadI _4 2 (0:19..1))
                        (copy _1 _4 (0:19..1))
                        (goto bb3 (0:19..1)))
                      (bb 3
                        (store-label |{|_| (#script ()) 0:8..1}| _1 (0:10..11))
                        (return (0:0..22))))))
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
              (define _ (id #t))
              (define _ (id #\a))",
            expect![[r#"
                (pkg
                  (fn (|{|id| (#script ()) 0:8..2}| [_1: '1]) '1 (0:11..14)
                    (let ([_0 '1 (0:11..14)]
                          [_1 '1 (0:20..1)])
                      (bb 0
                        (copy _0 _1 (0:23..1))
                        (return (0:11..14)))))
                  (fn (|{|@init| (#script ()) 0:0..93}|) void (0:0..93)
                    (let ([_0 void (0:0..93)]
                          [_1 boolean (0:51..7)]
                          [_2 (-> boolean boolean) (0:52..2)]
                          [_3 boolean (0:55..2)]
                          [_4 char (0:84..8)]
                          [_5 (-> char char) (0:85..2)]
                          [_6 char (0:88..3)])
                      (bb 0
                        (addressof _2 |{|id| (#script ()) 0:8..2}| (0:52..2))
                        (loadI _3 1 (0:55..2))
                        (arg _3 (0:55..2))
                        (call _1 _2 (0:51..7))
                        (store-label |{|_| (#script ()) 0:49..1}| _1 (0:51..7))
                        (addressof _5 |{|id| (#script ()) 0:8..2}| (0:85..2))
                        (loadI _6 97 (0:88..3))
                        (arg _6 (0:88..3))
                        (call _4 _5 (0:84..8))
                        (store-label |{|_| (#script ()) 0:82..1}| _4 (0:84..8))
                        (return (0:0..93))))))
            "#]],
        );
    }

    #[test]
    fn call_lambda() {
        check(
            r"(define _ ((lambda (x) x) #\a))",
            expect![[r#"
                (pkg
                  (fn (|{|lambda0| (#script ()) 0:11..14}| [_1: char]) char (0:11..14)
                    (let ([_0 char (0:11..14)]
                          [_1 char (0:20..1)])
                      (bb 0
                        (copy _0 _1 (0:23..1))
                        (return (0:11..14)))))
                  (fn (|{|@init| (#script ()) 0:0..31}|) void (0:0..31)
                    (let ([_0 void (0:0..31)]
                          [_1 char (0:10..20)]
                          [_2 (-> char char) (0:11..14)]
                          [_3 char (0:26..3)])
                      (bb 0
                        (addressof _2 |{|lambda0| (#script ()) 0:11..14}| (0:11..14))
                        (loadI _3 97 (0:26..3))
                        (arg _3 (0:26..3))
                        (call _1 _2 (0:10..20))
                        (store-label |{|_| (#script ()) 0:8..1}| _1 (0:10..20))
                        (return (0:0..31))))))
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
             (define _ (id #t))",
            expect![[r#"
                (pkg
                  (fn (|{|id| (id ()) 0:92..2}| [_1: '1]) '1 (0:95..14)
                    (let ([_0 '1 (0:95..14)]
                          [_1 '1 (0:104..1)])
                      (bb 0
                        (copy _0 _1 (0:107..1))
                        (return (0:95..14)))))
                  (fn (|{|@init| (#script ()) 0:0..173}|) void (0:0..173)
                    (let ([_0 void (0:0..173)]
                          [_1 boolean (0:165..7)]
                          [_2 (-> boolean boolean) (0:166..2)]
                          [_3 boolean (0:169..2)])
                      (bb 0
                        (addressof _2 |{|id| (id ()) 0:92..2}| (0:166..2))
                        (loadI _3 1 (0:169..2))
                        (arg _3 (0:169..2))
                        (call _1 _2 (0:165..7))
                        (store-label |{|_| (#script ()) 0:163..1}| _1 (0:165..7))
                        (return (0:0..173))))))
            "#]],
        );
    }
}
