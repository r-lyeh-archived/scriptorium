; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (flatten pair)  ==>  list
;
; Convert tree to flat list.
;
; Example:   (flatten '(a (b ((c) d . e))))  ==>  (a b c d e)

(define (flatten x)
  (letrec
    ((f (lambda (x r)
          (cond ((null? x)
                  r)
                ((pair? x)
                  (f (car x)
                     (f (cdr x) r)))
                (else
                  (cons x r))))))
    (f x '())))
