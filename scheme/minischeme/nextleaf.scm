;;;;  Sample of co-routine by call/cc
(define (apply-to-next-leaf proc tree endmark)
  (letrec
    ((return #f)
     (cont (lambda (l)
	     (recurse l)
	     (set! cont (lambda (d) (return endmark)))
	     (cont #f)))
     (recurse (lambda (l)
		(if (pair? l)
		  (for-each recurse l)
		  (call/cc (lambda (c) (set! cont c) (return (proc l))))))))
    (lambda ()
      (call/cc (lambda (c) (set! return c) (cont tree))))))

(define (foo lis)
  (let ((bar (apply-to-next-leaf (lambda (x) (* x x)) lis '())))
    (let loop ((n (bar)))
       (if (not (null? n))
         (begin
           (display n)
           (newline)
           (loop (bar)))))))

;; foo prints each elements (leaves) squared
(foo '(1 2 (3 (4 5) (6 (7)) 8) 9 10))

