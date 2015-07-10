; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (set-difference list ...)  ==>  list
;
; Compute the difference of a number of sets.
;
; Example:   (set-difference '(a b c d e f) '(b d) '(a f))  ==>  (c e)

(define (set-difference . a*)
  (letrec
    ((set-difference-3 (lambda (a b r)
      (cond ((null? a)
              (reverse! r))
            ((member (car a) b)
              (set-difference-3 (cdr a) b r))
            (else
              (set-difference-3 (cdr a) b (cons (car a) r)))))))
    (if (null? a*)
        a*
        (fold-left (lambda (a b)
                     (set-difference-3 a b '()))
                   (car a*)
                   (cdr a*)))))
