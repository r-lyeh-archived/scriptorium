; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (combine integer list)   ==>  list
; (combine* integer list)  ==>  list

; (load-from-library "combine.scm")
;
; Create k-combinations of the elements of the given list. K (the
; size of the combinations) is specified in the integer argument.
; COMBINE creates combinations without repetition, and COMBINE*
; creates combinations with repetition.
;
; Example:   (combine 2 '(a b c))   ==>  ((a b) (a c) (b c))
;            (combine* 2 '(a b c))  ==>  ((a a) (a b) (a c)
;                                         (b b) (b c) (c c))

(define (combine3 n set rest)
  (letrec
    ((tails-of
       (lambda (set)
         (cond ((null? set)
                 '())
               (else
                 (cons set (tails-of (cdr set)))))))
     (combinations
       (lambda (n set)
         (cond
           ((zero? n)
             '())
           ((= 1 n)
             (map list set))
           (else
             (apply append
                    (map (lambda (tail)
                           (map (lambda (sub)
                                  (cons (car tail) sub))
                                (combinations (- n 1) (rest tail))))
                         (tails-of set))))))))
    (combinations n set)))

(define (combine n set)
  (combine3 n set cdr))

(define (combine* n set)
  (combine3 n set (lambda (x) x)))
