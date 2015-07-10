; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (factor integer)  ==>  list
;
; Factor an integer down to its constituent primes.
; The resulting list has the form ((base exponent) ...)
;
; Example:   (factor 24)  ==>  ((3 1) (2 3))

(load-from-library "integer-sqrt.scm")

(define (factor n)
  (letrec
    ((quotient+exponent
       (lambda (n m)
         (letrec
           ((div (lambda (n m r)
                   (if (zero? (remainder n m))
                       (div (quotient n m) m (+ 1 r))
                       (cons n r)))))
           (div n m 0))))
     (add-expt
       (lambda (b e r)
         (if (zero? e)
             r
             (cons (list b e) r))))
     (factorize
       (lambda (n d r)
         (let ((lim (integer-sqrt n)))
           (letrec
             ((factorize3
                (lambda (n d r)
                  (let ((rest/exp (quotient+exponent n d)))
                    (let ((q (car rest/exp))
                          (e (cdr rest/exp)))
                      (cond ((< q 2) (add-expt d e r))
                            ((> d lim) (add-expt n 1 r))
                            (else (factorize3
                                    q
                                    (if (= d 2)
                                        3
                                        (+ 2 d))
                                    (add-expt d e r)))))))))
                (factorize3 n d r))))))
    (cond ((< n 1) (error "factor: operand not positive" n))
          ((= n 1) 1)
          (else (factorize n 2 '())))))
