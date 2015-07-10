; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (equal-ci? object1 object2)  ==>  boolean
;
; EQUAL-CI? is like EQUAL?, but compares strings and characters
; using case-insensitive predicates whereas EQUAL? distinguishes
; case.
;
; Example:   (equal-ci? '(#\A ("b")) '(#\a ("B")))  ==>  #t

(define (equal-ci? a b)
  (cond ((eq? a b)
          #t)
        ((and (pair? a)
              (pair? b))
          (and (equal-ci? (car a) (car b))
               (equal-ci? (cdr a) (cdr b))))
        ((char? a)
          (and (char? b)
               (char-ci=? a b)))
        ((string? a)
          (and (string? b)
               (string-ci=? a b)))
        ((vector? a)
           (and (vector? b)
                (equal-ci? (vector->list a)
                           (vector->list b))))
        (else
          (eqv? a b))))
