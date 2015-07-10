; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (integer-sqrt integer)  ==>  integer
;
; Compute the largest integer value that is not greater than the
; square root of INTEGER.
;
; Example:   (integer-sqrt 10)  ==>  3

(define (integer-sqrt square)
  (letrec
    ((sqrt2 (lambda (x last)
       (cond ((= last x)
               x)
             ((= last (+ 1 x))
               (if (> (* x x) square) (- x 1) x))
             (else
               (sqrt2 (quotient
                         (+ x (quotient square x))
                         2)
                      x))))))
    (if (negative? square)
        (error "integer-sqrt: negative argument" square)
        (sqrt2 square 0))))
