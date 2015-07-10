; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (remove obj list)        ==>  list
; (remp procedure^1 list)  ==>  list
; (remq obj list)          ==>  list
; (remv obj list)          ==>  list
;
; (load-from-library "remove.scm")
;
; Remove elements from a list. REMP uses the unary predicate
; PROCEDURE^1 to describe the property of the elements to be
; removed. REMOVE uses EQUAL?, REMV uses EQV?, and REMQ uses
; EQ? to compare each element of LIST to OBJ.
;
; Example:   (remp number? '(a 1 b 2 c 3))   ==>  (a b c)
;            (remove '(b) '(a (b) (c) (b)))  ==>  (a (c))
;            (remq 'b     '(a b c b d))      ==>  (a c d)
;            (remv 3      '(3 1 2 3 1))      ==>  (1 2 1)

(load-from-library "filter.scm")

(define (remp p a)
  (filter (lambda (x) (not (p x))) a))

(define (make-remover pred)
  (lambda (x a)
    (letrec
      ((rem
         (lambda (a r)
           (cond ((null? a)
                   (reverse! r))
                 ((pred x (car a))
                   (rem (cdr a) r))
                 (else
                   (rem (cdr a) (cons (car a) r)))))))
      (rem a '()))))

(define remq   (make-remover eq?))
(define remv   (make-remover eqv?))
(define remove (make-remover equal?))
