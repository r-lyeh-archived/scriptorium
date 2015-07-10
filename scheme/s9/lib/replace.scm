; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (replace object-old object-new pair)  ==>  pair
;
; Replace elements of a pair. OBJECT-OLD is the object to be
; replaced and OBJECT-NEW is the new object.
;
; Example:   (replace '(x) '(y) '(lambda (x) y))  ==>  (lambda (y) y)

(define (replace old new obj)
  (cond ((equal? obj old)
          new)
        ((pair? obj)
          (cons (replace old new (car obj))
                (replace old new (cdr obj))))
        (else
          obj)))
