; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (assp procedure^2 object alist)  ==>  object | #f
;
; Find the first member X of an alist for which
;
;       (procedure^2 object (car X))
;
; holds. When no such member exists, return #F.
;
; Example:   (assp char=? #\b '((#\a . 1) (#\b . 2)))  ==>  (#\b . 2)

(define (assp p x a)
  (cond ((null? a)
          #f)
        ((p x (caar a))
          (car a))
        (else
          (assp p x (cdr a)))))
