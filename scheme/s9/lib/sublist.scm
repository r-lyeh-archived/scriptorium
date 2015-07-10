; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (sublist list integer1 integer2)  ==>  list
;
; Return a fresh list formed from the members of LIST beginning with
; index INTEGER1 (inclusive) and ending with index INTEGER2 (exclusive).
;
; Example:   (sublist '(a b c d e) 2 4)  ==>  (c d)
;            (sublist '(a b c d e) 2 2)  ==>  ()

(define (sublist x p0 pn)
  (let ((k (length x)))
    (cond ((<= 0 p0 pn k)
            (do ((i   p0 (+ 1 i))
                 (in  (list-tail x p0) (cdr in))
                 (out '() (cons (car in) out)))
                ((= i pn)
                  (reverse! out))))
          (else
            (error "sublist: bad range" (list p0 pn))))))
