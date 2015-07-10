; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (string-translate string1 string2 string3)  ==>  string
;
; Translate STRING1 by replacing each instance of a character
; that occurs in STRING2 by the character at the corresponding 
; position in STRING3. STRING-TRANSLATE does not alter STRING1
; but returns a fresh string.
;
; Example:   (string-translate "a:b:c" ":" "-")  ==> "a-b-c"
;
;            (string-translate
;              "hello, world!"
;              "abcdefghijklmnopqrstuvwxyz"
;              "nopqrstuvwxyzabcdefghijklm")     ==>  "uryyb, jbeyq!"

(define (string-translate s from to)
  (let* ((k   (string-length s))
         (kf  (string-length from))
         (x   (string->list from))
         (new (string-copy s)))
    (let loop ((i 0))
      (cond ((>= i k)
              new)
            ((memv (string-ref s i) x)
              => (lambda (u)
                   (string-set! new
                                i
                                (string-ref to (- kf (length u))))
                   (loop (+ 1 i))))
            (else
              (loop (+ 1 i)))))))
