; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (mergesort procedure^2 list)  ==>  list
;
; Sort lists using the mergesort algorithm. PROCEDURE^2 is a
; binary predicate that describes the desired order. The original
; list is not changed.
;
; Example:   (mergesort <= '(5 3 7 9 1))  ==>  (1 3 5 7 9)

(load-from-library "split.scm")
(load-from-library "merge.scm")

(define (mergesort p a)
  (letrec
    ((sort
       (lambda (a)
         (if (or (null? a)
                 (null? (cdr a)))
             a
             (let ((p* (split a)))
               (merge p (sort (car p*))
                        (sort (cadr p*))))))))
    (sort a)))
