; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (quicksort procecure^2 list)  ==>  list
;
; Sort lists using the Quicksort algorithm. PROCEDURE^2 is a
; binary procedure describing the desired order. The original
; list is not changed.
;
; Example:   (quicksort <= '(5 3 7 9 1))  ==>  (1 3 5 7 9)

(load-from-library "partition.scm")

(define (quicksort p a)
  (letrec
    ((sort
       (lambda (a)
         (if (or (null? a)
                 (null? (cdr a)))
             a
             (let ((p* (partition (lambda (x) (p (car a) x))
                                  (cdr a))))
               (append (sort (cadr p*))
                       (list (car a))
                       (sort (car p*))))))))
    (sort a)))
