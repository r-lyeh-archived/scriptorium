; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (complement procedure)               ==>  procedure
; (compose procedure1 procedure2 ...)  ==>  procedure
; (const <expression>)                 ==>  procedure
; (curry procedure object ...)         ==>  procedure
; (curryr procedure object ...)        ==>  procedure
; (fork procedure1 procedure2)         ==>  procedure
;
; (load-from-library "hof.scm")
;
; COMPOSE combines the given procedures to form a new procedure
;
;       (lambda args (p1 ... (apply pN args) ...))
;
; where the procedures P1 through P(N-1) must be unary; the last
; procedure may take any number of arguments.
;
; COMPLEMENT returns a predicate expressing the complement of the given
; procedure (which should also be a predicate).
;
; CONST generates a procedure that discards any arguments passed to it
; and always evaluates to <expression>. <Expression> evaluates each time
; the procedure delivered by CONST is called.
; 
; CURRY partially applies PROCEDURE to the given OBJECTs, resulting
; in a new procedure
;
;       (lambda args (apply p object ... args))
;
; Application of the given PROCEDURE is finished when the procedure
; returned by CURRY is applied to some arguments.
;
; CURRYR curries the right-hand operands of P, yielding a unary procedure
;
;       (lambda (arg) (apply p arg (list object ...)))
;
; FORK arranges two procedures to form a fork:
;
;       ((fork f g) x1 ... xN)  -->  (f (g x1) ... (g xN))
;
; THUNK creates a nullary procedure that, when called, evaluates the
; given expressions in sequence. It returns the value of the last
; expression evaluated.
;
; Example:   ((complement pair?) '(1 2 3))  ==>  #f
;            ((complement eq?) 'foo 'bar)   ==>  #t
;
;            ((compose car cdr) '(1 2 3))         ==>  2
;            ((compose list reverse list) 1 2 3)  ==>  ((3 2 1))
;
;            ((const (+ 1 2)))        ==>  3
;            ((const (+ 1 2)) 3 4 5)  ==>  3
;
;            ((curry + 1) 9)              ==>  10
;            ((curry map list) '(1 2 3))  ==>  ((1) (2) (3))
;
;            ((curry  - 1) 10)  ==>  -9
;            ((curryr - 1) 10)  ==>  9
;
;            ((fork < car) '(1 . a) '(2 . b) '(3 . c))  ==>  #t
;            ((fork append reverse) '(3 2 1) '(6 5 4))  ==>  (1 2 3 4 5 6)

(define-syntax (compose . f*)
  (if (null? f*)
      (error "compose: too few arguments")
      (let ((arg (gensym)))
        (let ((body (let loop ((f* f*))
                      (if (null? (cdr f*))
                          `(apply ,(car f*) ,arg)
                          `(,(car f*) ,(loop (cdr f*)))))))
          `(lambda ,arg ,body)))))

(define (complement p)
  (lambda x
    (not (apply p x))))

(define-syntax (curry f . x)
  (let ((y (gensym)))
    `(lambda ,y (apply ,f ,@x ,y))))

(define-syntax (curryr f . y)
  (let ((x (gensym)))
    `(lambda (,x) (apply ,f ,x (list ,@y)))))

(define (fork f g)
  (lambda x
    (apply f (map g x))))

(define-syntax (const x)
  (let ((y (gensym)))
    `(lambda ,y ,x)))
