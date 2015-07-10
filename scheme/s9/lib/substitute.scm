; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (substitute pair alist)  ==>  pair
;
; Substitute objects in a given PAIR. The association list
; ALIST contains the objects to be substituted as keys and
; the corresponding substitutes as values.
;
; Example:   (substitute '(* (+ 5 7) 9) '(((+ 5 7) . 12)))  ==>  (* 12 9)

(define (substitute x a)
  (cond ((assoc x a) => cdr)
        ((pair? x) (cons (substitute (car x) a)
                         (substitute (cdr x) a)))
        (else x)))
