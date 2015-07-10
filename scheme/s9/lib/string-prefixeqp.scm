; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (string-prefix=? string1 string2)     ==>  boolean
; (string-prefix-ci=? string1 string2)  ==>  boolean
;
; Return #T when STRING2 has a prefix of STRING1 and otherwise
; return #F. STRING-PREFIX-CI=? performs the same operation, but
; ignores the case of the strings.
;
; Example:   (string-prefix=? "foo" "foobar")  ==>  #t
;            (string-prefix=? "foo" "fubar")   ==>  #f

(define (make-s-p=? op)
  (lambda (p s)
    (let ((k (string-length p)))
      (and (>= (string-length s) k)
           (op p (substring s 0 k))))))

(define string-prefix=?    (make-s-p=? string=?))
(define string-prefix-ci=? (make-s-p=? string-ci=?))
