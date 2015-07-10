; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (adjoin object list)  ==>  list
;
; Add an element to a set.
;
; Example:   (adjoin 'x '(a b c))  ==>  (x a b c)
;            (adjoin 'c '(a b c))  ==>  (a b c)

(define (adjoin x a)
  (if (member x a)
      a
      (cons x a)))
