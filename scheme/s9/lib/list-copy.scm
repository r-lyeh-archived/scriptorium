; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2012
; Placed in the Public Domain
;
; (list-copy list)  ==>  list
;
; Copy the spine of a proper list, creating a fresh list with the
; same elements as the original list.
;
; Example:   (list-copy '(foo bar baz))  ==>  (foo bar baz)

(define (list-copy a)
  (let copy ((a a)
             (r '()))
    (if (pair? a)
        (copy (cdr a)
              (cons (car a) r))
        (reverse! r))))
