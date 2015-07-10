; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (url-decode string)  ==>  string
;
; Decode an URL-encoded string. In an URL-encoded string, each subsequence
; of the form %NN represents a character with the ASCII code NN, where NN
; is a two-digit hexa-decimal number. URL-DECODE creates a fresh string
; where each %NN sequence has been replaced by the corresponding character.
; When STRING contains invalid %NN sequences, URL-DECODE returns #F.
;
; Example:   (url-decode "%46%4F%4FBAR")  ==>  "FOOBAR"

(load-from-library "and-letstar.scm")
(load-from-library "string-position.scm")

(define (url-decode s)
  (letrec
    ((hex-digit
       (lambda (x)
         (string-position (string (char-downcase x))
                          "0123456789abcdef")))
     (hex->char
       (lambda (h l)
         (and-let* ((h (hex-digit h))
                    (l (hex-digit l))
                    (n (+ (* 16 h) l)))
           (integer->char n)))))
    (let* ((k   (string-length s))
           (new (make-string k)))
      (let loop ((i 0)
                 (j 0))
        (cond ((>= i k)
                (substring new 0 j))
              ((char=? #\% (string-ref s i))
                (if (< i (- k 2))
                    (let ((c (hex->char (string-ref s (+ 1 i))
                                        (string-ref s (+ 2 i)))))
                      (if c
                          (begin (string-set! new j c)
                                 (loop (+ 3 i) (+ 1 j)))
                          #f))
                    #f))
              (else
                (string-set! new j (string-ref s i))
                (loop (+ 1 i) (+ 1 j))))))))
