; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009,2010
; Placed in the Public Domain
;
; (string-upcase string)    ==>  string
; (string-downcase string)  ==>  string
;
; (load-from-library "string-case.scm")
;
; Return a fresh string containing the characters of STRING, but
; with the case of alphabetic characters converted. STRING-UPCASE
; converts characters to upper case, STRING-DOWNCASE to lower case.
;
; Example:   (string-upcase "Hello, World!")    ==>  "HELLO, WORLD!"
;            (string-downcase "Hello, World!")  ==>  "hello, world!"

(load-from-library "string-map.scm")

(define (string-downcase s)
  (string-map char-downcase s))

(define (string-upcase s)
  (string-map char-upcase s))
