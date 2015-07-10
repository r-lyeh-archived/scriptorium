(begin
	(define fib  (lambda (n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)) ))))
	(fib 3)
	(quote (hello world))
	(print 3)
	(print (fib 20))
)
