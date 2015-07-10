; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (memp procedure^2 object list)  ==>  list | #f
;
; Find the first tail X of LIST for which
;
;       (procedure^2 object (car X))
;
; holds. When no such tail exists, return #F.
;
; Example:   (memp char=? #\b '(#\a #\b #\c))  ==>  (#\b #\c)

(define (memp p x a)
  (cond ((null? a)
          #f)
        ((p x (car a))
          a)
        (else
          (memp p x (cdr a)))))
