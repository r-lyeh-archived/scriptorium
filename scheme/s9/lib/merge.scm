; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (merge procedure^2 list1 list2)  ==>  list
;
; Merge the members of two lists into one. The next member to be
; pushed to the output list will always be the one satisfying
;
;       (procedure^2 (car list1) (car list2))
;
; The list moved to the output is removed from the input list.
; When one list is empty, input will be taken from the other
; one.
;
; When both LIST1 and LIST2 are sorted under PROCEDURE^2, so
; will be the output. When a single-element list is merged with
; a sorted list, the single element will be inserted at such a
; position that the order of the other list is preserved.
;
; Example:   (merge < '(1 3 5) '(2 4 6))  ==>  (1 2 3 4 5 6)
;            (merge < '(1 5 3) '(2 4 6))  ==>  (1 2 5 3 4 6)
;            (merge < '(3) '(1 2 4 5))    ==>  (1 2 3 4 5)

(define (merge p a b)
  (letrec
    ((merge3
       (lambda (a b r)
         (cond
           ((null? a)
             (if (null? b)
                 r
                 (merge3 a (cdr b) (cons (car b) r))))
           ((null? b)
             (merge3 (cdr a) b (cons (car a) r)))
           ((p (car a) (car b))
             (merge3 a (cdr b) (cons (car b) r)))
           (else
             (merge3 (cdr a) b (cons (car a) r)))))))
    (merge3 (reverse a) (reverse b) '())))
