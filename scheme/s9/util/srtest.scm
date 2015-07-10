; Scheme 9 from Empty Space
; SYNTAX-RULES Test Suite
; By Nils M Holm, 2007-2010

(load-from-library "syntax-rules.scm")

(define Errors 0)

(define (fail expr result expected)
  (display "test failed: ")
  (write expr)
  (newline)
  (display "got result:  ")
  (write result)
  (newline)
  (display "expected:    ")
  (write expected)
  (newline)
  (set! Errors (+ 1 Errors)))

(define (test3 expr result expected)
;  (write expr) (display " => ") (write result) (newline)
  (if (not (equal? result expected))
      (fail expr result expected)))

(define-syntax (test form result)
  `(test3 ',form ,form ,result))

(define-syntax keyword
  (syntax-rules ()
    ((_) '())))
(test (keyword) '())

(define-syntax iff
  (syntax-rules (then else)
    ((_ p then t)
      (and p t))
    ((_ p then t else f)
      (cond (p t) (else f)))))
(test (iff #t then 'foo) 'foo)
(test (iff #f then 'foo) #f)
(test (iff #t then 'foo else 'bar) 'foo)
(test (iff #f then 'foo else 'bar) 'bar)
(test (iff #f then (error "error")) #f)
(test (iff #t then 'foo else (error "error")) 'foo)
(test (iff #f then (error "error") else 'bar) 'bar)

(define-syntax foo-syntax
  (syntax-rules ()
    ((_ x) x)
    ((_ x y ...) (cons x (foo-syntax y ...)))))
(test (foo-syntax 1 2 3 4 5) '(1 2 3 4 . 5))

(define-syntax bar-syntax
  (syntax-rules ()
    ((_) '())
    ((_ x ...) (list '(x x) ...))))
(test (bar-syntax) '())
(test (bar-syntax x) '((x x)))
(test (bar-syntax x y) '((x x) (y y)))
(test (bar-syntax x y z) '((x x) (y y) (z z)))

(define-syntax rev-syntax
  (syntax-rules ()
    ((_ () r) r)
    ((_ (a . d) r) (rev-syntax d (a . r)))
    ((_ x) (rev-syntax x ()))))
(test (rev-syntax ()) '())
(test (rev-syntax (2 1 cons)) '(1 . 2))
(test (rev-syntax ('bar 'foo #t if)) 'foo)

(define-syntax ell
  (syntax-rules ()
    ((_ ((a b) ...) c ...)
       (list '((b a) ...) c ...))))
(test (ell ()) '(()))
(test (ell () 0) '(() 0))
(test (ell ((1 2)) 3) '(((2 1)) 3))
(test (ell ((1 2) (3 4) (5 6)) 7) '(((2 1) (4 3) (6 5)) 7))
(test (ell ((1 2)) 3 4 5) '(((2 1)) 3 4 5))

(define-syntax false
  (syntax-rules ()
    ((_ x y ...)
       (if x (list y ...) (if #f #f)))))

(test (false #t 1 2 3) '(1 2 3))
(test (false #f 1 2 3) (if #f #f))

(define-syntax fluid-let
  (syntax-rules ()
    ((_ () expr . exprs)
      (begin expr . exprs))
    ((_ ((v1 a1) (v2 a2) ...) expr . exprs)
      (let ((outer-v v1))
        (set! v1 a1)
        (fluid-let ((v2 a2) ...)
          (let ((r (begin expr . exprs)))
            (set! v1 outer-v)
            r))))))

(test (let ((x  0)
            (y  1)
            (z  2)
            (fx #f)
            (fy #f))
        (fluid-let ((x -2)
                    (y -1))
          (set! fx x)
          (set! fy y))
        (list fx fy x y z))
      '(-2 -1 0 1 2))

(define-syntax foo-syntax
  (syntax-rules ()
    ((_ x ...)
      (list #t x ... #f))))

(test (foo-syntax) '(#t #f))
(test (foo-syntax 1) '(#t 1 #f))
(test (foo-syntax 1 2 3) '(#t 1 2 3 #f))

(define-syntax local-foo
  (syntax-rules ()
    ((_ x)
      (let ((local 0))
        x))))

(test (let ((local 1)) (local-foo local)) 1)

(cond ((zero? Errors)
        (display "Everything fine!"))
      (else
        (display Errors)
        (if (> Errors 1)
            (display " errors.")
            (display " error."))))
(display #\newline)
