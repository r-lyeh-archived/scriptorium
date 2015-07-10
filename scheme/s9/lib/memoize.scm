; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (define-memoizing <name> <procedure>)      ==>  procedure
; (define-memoizing (<name> <args>) <body>)  ==>  procedure
; (memoize procedure)                        ==>  procedure
; (memoize procedure 'reset)                 ==>  procedure
;
; (load-from-library "memoize.scm")
;
; MEMOIZE turns the given procedure into a "memoizing" procedure
; that stores intermediate results in an internal cache in order
; to speed up computation. This works best for massively recursive
; procedures.
;
; When the 'RESET keyword is passed to MEMOIZE as a second argument,
; the resulting memoizing procedure will allow to clear its cache
; by calling it with 'RESET as its only argument.
;
; DEFINE-MEMOIZING is like DEFINE but defines a memoizing procedure.
; Procedures created with DEFINE-MEMOIZING always interpret 'RESET
; as a special argument for clearing their caches.
;
; Example:   (letrec
;              ((fib
;                 (memoize
;                   (lambda (x)
;                     (if (< x 2)
;                         1
;                         (+ (fib (- x 1))
;                            (fib (- x 2))))))))
;              (fib 100))                         ==>  573147844013817084101

(load-from-library "hash-table.scm")

(define (memoize f . reset)
  (let ((h     (make-hash-table))
        (reset (and (not (null? reset))
                    (car reset))))
    (lambda x
      (cond ((and reset
                  (equal? '(reset) x))
              (set! h (make-hash-table)))
            ((hash-table-ref h x)
              => car)
            (else
              (let ((v (apply f x)))
                (hash-table-set! h x v)
                v))))))

(define-syntax (define-memoizing head . body)
  (if (symbol? head)
      `(define head (memoize ,body))
      `(define ,(car head)
         (memoize (lambda ,(cdr head) ,@body) 'reset))))
