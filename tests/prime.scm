(define (isprimel n i)
  (cond ((= i n) 1)
        ((= (modulo n i) 0) 0)
        (else (isprimel n (+ i 1)))))

(define (isprime n) (isprimel n 2))


(define (primesl n count i)
  (if (> i n)
    count
    (primesl
      n
      (+ count (isprime i))
      (+ i 1))))

(define (primes n) (primesl n 0 2))


(define n 200000)
(display "primes: ")
(display (primes n))
(display #\lf)
