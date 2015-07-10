; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (fluid-let ((variable expression) ...) expression ...)  ==>  object
;
; Bind variables dynamically, i.e. assign a dynamic (rather than
; a lexical/static) value to each given variable. The variables
; must be defined outside of FLUID-LET. The difference between
; LET and FLUID-LET is as follows:
;
; (let ((a 0))                   (let ((a 0))
;   (let ((f (lambda () a)))       (let ((f (lambda () a)))
;     (let ((a 1))                   (fluid-let ((a 1))
;       (f))))         ==> 0           (f))))         ==> 1
;
; Example:   (let ((a 0))
;              (let ((f (lambda () a)))
;                (fluid-let ((a 1))
;                  (f))))                ==>  1

; This implementation is inefficient, use "fluid-let.scm" instead.

(load-from-library "syntax-rules.scm")

(define-syntax fluid-let
  (syntax-rules ()
    ((_ () expr . exprs)
      (begin expr . exprs))
    ((_ ((v1 a1) (v2 a2) ...) expr . exprs)
      (let ((outer-v v1))
        (set! v1 a1)
        (fluid-let ((v2 a2) ...)
          (let ((r (begin expr . exprs)))
            (set! v1 outer-v)
            r))))))
