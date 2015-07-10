; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (union list ...)  ==>  list
;
; Compute the union of a series of sets.
;
; Example:   (union '(v w x) '(w x y) '(x y z))  ==>  (v w x y z)

(load-from-library "list-to-set.scm")

(define (union . a*)
  (list->set (apply append a*)))
