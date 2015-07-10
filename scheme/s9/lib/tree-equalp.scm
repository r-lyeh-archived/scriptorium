; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (tree-equal? procedure pair1 pair2)  ==>  boolean
;
; Compare the leaves of two trees PAIR1 and PAIR2 using the predicate
; PROCEDURE. TREE-EQUAL? returns #T if the trees have the same structure
; and (procedure x1 x2) holds for their pairwise leaves.
;
; Example:   (tree-equal? (lambda (x y) #t)
;                         '(((a . b) (c . d)) (e . f))
;                         '(((1 . 2) (3 . 4)) (5 . 6)))  ==>  #t
;
;            (tree-equal? eqv?
;                         '((1 . 2) (3 . 4))
;                         '((1 . 2) (3 4)))              ==> #f

(define (tree-equal? p t1 t2)
  (if (pair? t1)
      (and (pair? t2)
           (tree-equal? p (car t1) (car t2))
           (tree-equal? p (cdr t1) (cdr t2)))
      (p t1 t2)))
