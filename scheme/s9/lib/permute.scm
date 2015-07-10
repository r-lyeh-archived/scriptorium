; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (permute integer list)   ==>  list
; (permute* integer list)  ==>  list
;
; (load-from-library "permute.scm")
;
; Create k-permutations of the elements of the given list. K (the
; size of the permutations) is specified in the integer argument.
; PERMUTE creates permutations without repetition and PERMUTE*
; creates permutations with repetition.
;
; Example:   (permute 2 '(a b c))   ==>  ((a b) (b a) (a c)
;                                         (c a) (b c) (c b))
;
;            (permute* 2 '(a b c))  ==>  ((a a) (a b) (a c)
;                                         (b a) (b b) (b c)
;                                         (c a) (c b) (c c))

(load-from-library "combine.scm")

(define (permute n set)
  (letrec
    ((rotate
       (lambda (x)
         (append (cdr x) (list (car x)))))
     (rotations
       (lambda (x)
         (letrec
           ((rot (lambda (x n)
                   (if (null? n)
                       '()
                       (cons x (rot (rotate x)
                                    (cdr n)))))))
           (rot x x))))
     (permutations
       (lambda (set)
         (cond ((null? set)
                 '())
               ((null? (cdr set))
                 (list set))
               ((null? (cddr set))
                 (rotations set))
               (else
                 (apply append
                        (map (lambda (rotn)
                               (map (lambda (x)
                                      (cons (car rotn) x))
                                    (permutations (cdr rotn))))
                             (rotations set))))))))
    (apply append (map permutations (combine n set)))))

(define (permute* n set)
  (cond ((zero? n)
          '())
        ((= n 1)
          (map list set))
        (else
          (apply append
                 (map (lambda (x)
                        (map (lambda (sub)
                               (cons x sub))
                             (permute* (- n 1) set)))
                      set)))))
