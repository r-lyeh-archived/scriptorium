; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (split list)  ==>  list
;
; Split a list into two, where the first one contains the leftmost
; members of the list and the second one its rightmost members. When
; the list has an odd number of members, the first list will hold
; the extra member. Return a list of two lists:
;
;       (leftmost-members rightmost-members)
;
; Example:   (split '(1 2 3 4))    ==>  ((1 2) (3 4))
;            (split '(1 2 3 4 5))  ==>  ((1 2 3) (4 5))
;            (split '())           ==>  (() ())

(define (split a)
  (letrec
    ((split3
       (lambda (a r1 r2)
         (cond ((null? a)
                 (list (reverse! r2) r1))
               ((null? (cdr a))
                 (list (reverse! (cons (car r1) r2))
                       (cdr r1)))
               (else
                 (split3 (cddr a)
                         (cdr r1)
                         (cons (car r1) r2)))))))
    (split3 a a '())))
