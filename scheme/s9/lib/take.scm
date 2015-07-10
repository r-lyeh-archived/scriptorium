; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2012
; Placed in the Public Domain
;
; (take list integer)  ==>  list
;
; Copy the first INTEGER conses of a proper list, creating a fresh
; that has the same elements as the original list in the first
; INTEGER positions.
;
; INTEGER may not be larger than the length of LIST.
;
; Example:   (take '(foo bar baz) 0)  ==>  ()
;            (take '(foo bar baz) 1)  ==>  (foo)
;            (take '(foo bar baz) 3)  ==>  (foo bar baz)

(define (take a n)
  (let copy ((a a)
             (n n)
             (r '()))
    (if (or (not (pair? a))
            (zero? n))
        (reverse! r)
        (copy (cdr a)
              (- n 1)
              (cons (car a) r)))))
