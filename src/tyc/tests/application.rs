use super::*;

#[test]
fn id() {
    check(
        r"(import (rnrs expander core))
          (define id (lambda (x) ((lambda (x) x) x)))",
        expect!["id: (∀ '(a) (-> a a))"],
        expect![[r#"
            {body 0:0..83
              {import (rnrs expander core ())@0:0..29}
              {define@0:40..43
                {|id| : (∀ '(a) (-> a a)) 0:48..2}
                {λ : (-> '2 '2) 0:51..31
                  ({|x| : '2 0:60..1})
                  #f
                  {body 0:51..31
                    {list 0:63..18
                      {λ : (-> '2 '2) 0:64..14
                        ({|x| : '2 0:73..1})
                        #f
                        {body 0:64..14
                          {var |x| : '2 (#script ()) 0:76..1}}}
                      {var |x| : '2 (#script ()) 0:79..1}}}}}}
        "#]],
    );
}

#[test]
fn id_multiple_instantiations() {
    check(
        r"(import (rnrs expander core))
          (define id (lambda (x) x))
          (define _ (id 3))
          (define _ (id #t))
          (define _ (id #\a))",
        expect![[r#"
            id: (∀ '(a) (-> a a))
            _: char"#]],
        expect![[r#"
            {body 0:0..153
              {import (rnrs expander core ())@0:0..29}
              {define@0:40..26
                {|id| : (∀ '(a) (-> a a)) 0:48..2}
                {λ : (-> '1 '1) 0:51..14
                  ({|x| : '1 0:60..1})
                  #f
                  {body 0:51..14
                    {var |x| : '1 (#script ()) 0:63..1}}}}
              {define@0:77..17
                {|_| : i64 0:85..1}
                {list 0:87..6
                  {var |id| : (-> i64 i64) (#script ()) 0:88..2}
                  {3 : i64 0:91..1}}}
              {define@0:105..18
                {|_| : boolean 0:113..1}
                {list 0:115..7
                  {var |id| : (-> boolean boolean) (#script ()) 0:116..2}
                  {#t : boolean 0:119..2}}}
              {define@0:134..19
                {|_| : char 0:142..1}
                {list 0:144..8
                  {var |id| : (-> char char) (#script ()) 0:145..2}
                  {#\a : char 0:148..3}}}}
        "#]],
    );
}

#[test]
fn anonymous_id() {
    check(
        r"(import (rnrs expander core))
          (define _ ((lambda (x) x) #t))",
        expect!["_: boolean"],
        expect![[r#"
            {body 0:0..70
              {import (rnrs expander core ())@0:0..29}
              {define@0:40..30
                {|_| : boolean 0:48..1}
                {list 0:50..19
                  {λ : (-> boolean boolean) 0:51..14
                    ({|x| : boolean 0:60..1})
                    #f
                    {body 0:51..14
                      {var |x| : boolean (#script ()) 0:63..1}}}
                  {#t : boolean 0:66..2}}}}
        "#]],
    );
}
