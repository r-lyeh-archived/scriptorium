; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (string-reverse string)   ==>  string
; (string-reverse! string)  ==>  unspecific
;
; Create a fresh string and fill it with the characters of
; STRING, but in reverse order. STRING-REVERSE! reverses the
; characters of STRING in situ, overwriting the original
; string.
;
; Example:   (string-reverse "rats live on no evil star")
;                        ==> "rats live on no evil star"

(define (string-reverse! s)
  (let* ((k (string-length s))
         (m (quotient k 2)))
    (do ((i 0 (+ 1 i))
         (j (- k 1) (- j 1)))
        ((= i m))
      (let ((c (string-ref s i)))
        (string-set! s i (string-ref s j))
        (string-set! s j c)))))

(define (string-reverse s)
  (let ((n (string-copy s)))
    (string-reverse! n)
    n))
