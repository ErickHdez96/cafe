# Expansion

After the file has been parsed and turned into a concrete syntax tree, it still needs to be expanded into an abstract syntax tree.

## Scopes

When 

Every time a syntax object gets passed to a transformer, every element gets applied an antimark. The output then gets applied a mark
which cancels the antimark.

Simple let

```scheme
(let ([x 1]) x)
(let ([x₁ 1₁]) x₁)
((lambda (x₁) x₁) 1₁)
((lambda (x) x) 1)
```

Inner let

```scheme
(let ([x 1])
  (let ([x x]) x))
; apply antimark ₁ to everything
(let ([x₁ 1₁])
  (let₁ ([x₁ x₁]) x₁))
; expand outer <let>
((lambda¹ (x₁)
   (let ([x₁ x₁])
     x₁))
 1₁)
; apply mark ¹
((lambda¹ (x)
   (let¹ ([x x])
     x))
 1)
; apply antimark ₂ to everything in <let>
((lambda¹ (x)
   (let¹ ([x₂ x₂])
     x₂))
 1)
; expand <let>
((lambda¹ (x)
   ((lambda (x₂) x₂)
    x₂))
 1)
; apply mark ² to output of inner <let>
((lambda¹ (x)
   ((lambda² (x) x)
    x))
 1)
```

Or

```scheme
(define-syntax or
  (syntax-rules ()
    [(_) #f]
    [(_ e) e]
    [(_ t1 rest ...)
     (let ([x t1])
       (if x x (or rest ...)))]))

(let ([y 1]
      [x 2])
  (or y x))
; anti-mark <let> ₁
(let ([y₁ 1₁]
      [x₁ 2₁])
  (or₁ y₁ x₁))
; expand <let>
((lambda (y₁ x₁)
   (or₁ y₁ x₁))
 1₁ 2₁)
; mark <let> ¹
((lambda¹ (y x)
   (or y x))
 1 2)
; anti-mark <or> ₂
((lambda¹ (y x)
   (or y₂ x₂))
 1 2)
; expand <or>
((lambda¹ (y x)
   (let ([x y₂])
     (if x x (or x₂ ...))))
 1 2)
; mark <or> ²
((lambda¹ (y x)
   (let² ([x² y])
     (if² x² x² (or² x ...))))
 1 2)
; anti-mark <let> ₃
((lambda¹ (y x)
   (let² ([x²₃ y₃])
     (if²₃ x²₃ x²₃ (or²₃ x₃ ...))))
 1 2)
; expand <let>
((lambda¹ (y x)
   ((lambda (x²₃)
      (if²₃ x²₃ x²₃ (or²₃ x₃ ...)))
    y₃))
 1 2)
; mark <let> ³
((lambda¹ (y x)
   ((lambda³ (x²)
      (if² x² x² (or² x ...)))
    y))
 1 2)
; anti-mark <or> ₄
((lambda¹ (y x)
   ((lambda³ (x²)
      (if² x² x² (or² x₄ ...)))
    y))
 1 2)
; expand <or>
((lambda¹ (y x)
   ((lambda³ (x²)
      (if² x² x² x₄))
    y))
 1 2)
; mark <or> ⁴
((lambda¹ (y x)
   ((lambda³ (x²)
      (if² x² x² x))
    y))
 1 2)
```

As can be seen in the 
