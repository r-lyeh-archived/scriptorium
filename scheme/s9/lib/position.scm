; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (position object list)        ==>  integer | #f
; (posv object list)            ==>  integer | #f
; (posq object list)            ==>  integer | #f
; (posp procedure object list)  ==>  integer | #f
;
; Find the position of an object in a list. When LIST contains OBJECT,
; return the position of OBJECT (where the first object is as position
; zero) and otherwise return #F.
;
; POSP uses PROCEDURE as a predicate to compare OBJECT to each member
; of list.
;
; (Position a b)  equals  (posp equal? a b)
; (Posv a b)      equals  (posp eqv? a b)
; (Posq a b)      equals  (posp eq? a b)
;
; Example:   (position '(bar) '((foo) (bar) (baz)))  ==>  1
;            (posv 4 '(0 1 2 3 4 5 6))               ==>  4
;            (posq 'foo '(foo bar baz))              ==>  0
;            (posp (lambda (x y) (= x (car y)))
;                  2
;                  '((0 . a) (1 . b) (2 . c)))       ==>  2

(define (posp p x a)
  (let loop ((a a)
             (i 0))
    (cond ((null? a)
            #f)
          ((p x (car a))
            i)
          (else
            (loop (cdr a) (+ i 1))))))

(define (position x a) (posp equal? x a))
(define (posv x a)     (posp eqv? x a))
(define (posq x a)     (posp eq? x a))
