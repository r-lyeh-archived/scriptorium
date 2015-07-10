; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2013
; Placed in the Public Domain
;
; (collect procedure list)  ==>  list
;
; Collect elements from LIST as long as two subsequent elements
; E1 and E2 satisfy the predicate (PROCEDURE E1 E2). When two
; subsequent elements do not satisfy the predicate, start a new
; output list. Concatenate all output lists in the result.
;
; Example:   (collect eq? '(a a a b c c))  ==>  ((a a a) (b) (c c))
;            (collect < '(1 2 3 3 4 5 4))  ==>  ((1 2 3) (3 4 5) (4))

(define (collect p a)
  (let collect ((in  a)
                (out '())
                (res '()))
    (cond ((null? in)
            (reverse! res))
          ((and (pair? (cdr in))
                (p (car in) (cadr in)))
            (collect (cdr in)
                     (cons (car in) out)
                     res))
          (else
            (let ((out (reverse! (cons (car in) out))))
              (collect (cdr in)
                       '()
                       (cons out res)))))))

