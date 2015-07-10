(prn "hello world")
(prn 3)
(setfn fib (x) (
	if (<= x 1) 
		1 
		( + (fib (- x 1)) (fib (- x 2)) ) 
	)
)
(prn (fib 34))
