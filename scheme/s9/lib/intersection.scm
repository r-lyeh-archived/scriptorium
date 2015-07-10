; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (intersection list ...)  ==>  list
;
; Compute the intersection of a number of sets.
;
; Example:   (intersection '(v w x) '(w x y) '(x y z))  ==>  (x)

(define (intersection . a*)
  (letrec
    ((intersection3 (lambda (a b r)
      (cond ((null? a)
              (reverse! r))
            ((member (car a) b)
              (intersection3 (cdr a) b (cons (car a) r)))
            (else
              (intersection3 (cdr a) b r))))))
    (if (null? a*)
        a*
        (fold-left (lambda (a b)
                     (intersection3 a b '()))
                   (car a*)
                   (cdr a*)))))
