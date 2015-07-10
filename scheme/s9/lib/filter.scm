; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (filter procecure^1 list)  ==>  list
;
; Extract elements from a list. PROCEDURE describes the property
; of the elements to be extracted. It must be a procedure of one
; argument.
;
; Example:   (filter number? '(a 1 b 2 c 3))  ==>  (1 2 3)

(define (filter p a)
  (letrec
    ((filter2
       (lambda (a r)
         (cond ((null? a)
                 (reverse! r))
               ((p (car a))
                 (filter2 (cdr a) (cons (car a) r)))
               (else
                 (filter2 (cdr a) r))))))
    (filter2 a '())))
