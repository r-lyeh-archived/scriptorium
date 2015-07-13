(define (fibr n)
  (cond ((< n 2) n)
        (else (+ (fibr (- n 1)) (fibr (- n 2))))))

(define N 34)
(display "fib: ")
(display (fibr N))
(display "          ") ; #\lf)
