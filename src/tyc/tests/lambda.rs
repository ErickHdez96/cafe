use super::*;

#[test]
fn id() {
    check(
        r"(import (rnrs expander core))
          (define a (lambda (x) ((lambda (x) x) x)))",
        expect!["a: (∀ '(a) (-> a a))"],
        expect![[r#"
            {body 0:0..82
              {import (rnrs expander core ())@0:0..29}
              {define@0:40..42
                {|a| : (∀ '(a) (-> a a)) 0:48..1}
                {λ : (-> '2 '2) 0:50..31
                  ({|x| : '2 0:59..1}})
                  #f
                  {body 0:50..31
                    {list 0:62..18
                      {λ : (-> '2 '2) 0:63..14
                        ({|x| : '2 0:72..1}})
                        #f
                        {body 0:63..14
                          {var |x| : '2 (#script ()) 0:75..1}}}
                      {var |x| : '2 (#script ()) 0:78..1}}}}}}
        "#]],
    );
}
