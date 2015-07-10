; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (make-partitions integer)  ==>  list
;
; Create all partitions of a positive integer. A (number-theoretical)
; partition of a positive integer N is a set of integers whose sum is
; equal to N. E.g., the partitions of 3 are 3, 2+1, and 1+1+1.
;
; Example:   (make-partitions 4)  ==>  ((4) (3 1) (2 2) (2 1 1) (1 1 1 1))

(load-from-library "iota.scm")
(load-from-library "filter.scm")

(define (make-partitions n)
  (letrec
    ((partition
       (lambda (n)
         (cond ((zero? n) '(()))
               ((= n 1)   '((1)))
               (else      (apply append
                                 (map (lambda (x)
                                        (map (lambda (p)
                                               (cons x p))
                                             (partition (- n x))))
                                      (iota n)))))))
     (filter-descending
       (lambda (x)
         (filter (lambda (p)
                   (or (null? p)
                       (null? (cdr p))
                       (apply >= p)))
                 x))))
    (reverse! (filter-descending (partition n)))))
