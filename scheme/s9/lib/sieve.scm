; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (sieve integer)  ==>  list
;
; Given an INTEGER N, generate a list of all prime numbers less
; than N. INTEGER must be positive.
;
; Example:   (sieve 20)  ==>  (2 3 5 7 11 13 17 19)

(define (sieve n)
  (letrec
    ((sieve2
       (lambda (n primes)
         (let loop ((p (reverse primes)))
           (cond ((or (null? p)
                      (> (* (car p) (car p)) n))
                   (cons n primes))
                 ((zero? (remainder n (car p)))
                   primes)
                 (else
                   (loop (cdr p))))))))
    (let loop ((i 3)
               (primes (list 2)))
      (if (> i n)
          (reverse! primes)
          (loop (+ 2 i) (sieve2 i primes))))))
