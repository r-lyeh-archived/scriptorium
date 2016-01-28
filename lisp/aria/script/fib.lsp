(do
  
  (= fib (fn (n)
    (if (>= n 2)
        (+ (fib (- n 1)) (fib (- n 2)))
        n)))

  (print (fib 20))) ; prints 6765

