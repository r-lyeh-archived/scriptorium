; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (partition proceduce^1 list)  ==>  (list_1 list_2)
;
; Partition a list into elements with and without a given property.
; The unary predicate P, which is given in the procedure argument,
; specifies the property. The returned value contains two lists,
; where the first one holds the elements satisfying the property
; and the second one the elements not satisfying the property.
;
; Example:   (partition even? '(1 2 3 4 5))  ==>  ((2 4) (1 3 5))

(define (partition p a)
  (letrec
    ((partition3
       (lambda (a r+ r-)
         (cond
           ((null? a)
             (list r+ r-))
           ((p (car a))
             (partition3 (cdr a)
                         (cons (car a) r+)
                         r-))
           (else
             (partition3 (cdr a)
                         r+
                         (cons (car a) r-)))))))
    (partition3 (reverse a) '() '())))
