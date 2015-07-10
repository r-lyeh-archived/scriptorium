; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (string-map procedure^1 string ...)   ==>  string
; (string-map! procedure^1 string ...)  ==>  unspecific
;
; (load-from-library "string-map.scm")
;
; Map a procedure over a string, giving a new string
;
;       (string (f (string-ref s i)) ...)
;
; where F is the given PROCEDURE and S is the string to map.
; STRING-MAP is to strings what MAP is to lists.
;
; STRING-MAP! does not create a fresh string, but changes the
; given one in situ.
;
; Example:   (string-map char-downcase "HELLO")  ==>  "hello"
;            (let ((s (string-copy "HELLO!")))
;              (string-map! char-downcase s)
;              s)                                ==>  "hello!"

(define (string-map! f s)
  (let ((k (string-length s)))
    (do ((i 0 (+ 1 i)))
        ((>= i k))
      (string-set! s i (f (string-ref s i))))))

(define (string-map f s)
  (let ((n (string-copy s)))
    (string-map! f n)
    n))
