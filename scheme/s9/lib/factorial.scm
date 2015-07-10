; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (factorial integer)  ==>  integer
;
; Compute the factorial of a number.
;
; Example:   (factorial 30)  ==>  265252859812191058636308480000000

(define (factorial n)
  (letrec
    ((r* (lambda (n m)
           (if (< m 2)
               n
               (let ((k (quotient m 2)))
                 (* (r* n k)
                    (r* (+ n k) (- m k))))))))
    (cond ((negative? n)
            (error "factorial: negative operand" n))
          (else
            (r* 1 n)))))
