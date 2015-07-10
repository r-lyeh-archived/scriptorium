; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (string-scan char string)     ==>  integer | #f
; (string-ci-scan char string)  ==>  integer | #f
;
; (load-from-library "string-scan.scm")
;
; Scan STRING for CHAR. When STRING contains CHAR, return its position,
; otherwise return #F. STRING-CI-SCAN does the same but ignores case.
;
; Example:   (string-scan #\y "xyz")  ==>  1

(define (make-string-scan c=?)
  (lambda (c s)
    (let ((k (string-length s)))
      (let loop ((i 0))
        (cond ((>= i k)
                #f)
              ((c=? c (string-ref s i))
                i)
              (else
                (loop (+ 1 i))))))))

(define string-scan    (make-string-scan char=?))
(define string-ci-scan (make-string-scan char-ci=?))
