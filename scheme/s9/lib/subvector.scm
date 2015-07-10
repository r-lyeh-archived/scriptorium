; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (subvector vector integer1 integer2)  ==>  vector
;
; SUBVECTOR returns a fresh vector formed from the members of VECTOR
; beginning with index INTEGER1 (inclusive) and ending with index
; INTEGER2 (exclusive).
;
; Example:   (subvector '#(a b c d e) 2 4)  ==>  #(c d)
;            (subvector '#(a b c d e) 2 2)  ==>  #()

(define (subvector v p0 pn)
  (let ((k (vector-length v)))
    (cond ((<= 0 p0 pn k)
            (vector-copy v p0 pn))
          (else
            (error "subvector: bad range" (list p0 pn))))))
