; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009,2012
; Placed in the Public Domain
;
; (module <name> <definition> ...)                ==>  unspecific
; (using <name> (<name_i> ...) <expression> ...)  ==>  object
;
; (load-from-library "simple-modules.scm")
;
; Simple modules. Inside of a MODULE expression, DEFINE defines
; a local object and DEFINE* defines a public object. <Name> names
; the module itself.
;
; Expressions inside of USING may use all <name_i>'s that are
; being imported from the module <name>.
;
; Example:   (begin ; Note: BEGIN is only needed for automatic testing
;              (module math
;                (define* (fact x)
;                  (if (= 0 x) 1 (* x (fact (- x 1))))))
;              (using math (fact)
;                (fact 5)))                               ==> 120

(load-from-library "syntax-rules.scm")

(define-syntax module
  (syntax-rules (define define*)
    ((_ (exports env))
       (letrec env
         (list . exports)))

    ((_ (exports env) (define (v . a*) . body) . defs)
       (module (exports env)
               (define v (lambda a* . body)) . defs))

    ((_ (exports env) (define v a) . defs)
       (module (exports ((v a) . env)) . defs))

    ((_ (exports env) (define* (v . a*) . body) . defs)
       (module (exports env)
               (define* v (lambda a* . body)) . defs))

    ((_ (exports env) (define* v a) . defs)
       (module (((cons 'v v) . exports) ((v a) . env)) . defs))

    ((_ name . defs)
       (define name (module (() ()) . defs)))))

(define-syntax using
  (syntax-rules ()
    ((_ (name env ()) . body)
       (let env . body))

    ((_ (name env (import . rest)) . body)
       (using (name ((import (cdr (assq 'import name))) . env)
                   rest) . body))

    ((_ name imports . body)
       (using (name () imports) . body))))
