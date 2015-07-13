(define (fibr n)
  (cond ((< n 2) n)
        (else (+ (fibr (- n 1)) (fibr (- n 2))))))

(define (fib n cur last)
  (cond ((= n 0) cur)
        (else (fib (- n 1) (+ cur last) cur))))

(define (fibi n) (fib (- n 1) 1 0))



(define N 34)
(display "fib: ")
(display (fibr N))
(display " = ")
(display (fibi N))
(display "          ") ; #\lf)
