; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009,2010
; Placed in the Public Domain
;
; (string-split char string)  ==>  list
;
; Split a string into substrings. CHAR is interpreted as a separator.
; Return a list containing all coherent sequences of non-separating
; characters contained in the given string. When multiple subsequent
; separators are found, empty strings will be generated.
;
; Example:   (string-split #\: "a::b:c:")  ==>  ("a" "" "b" "c" "")

(define (string-split c s)
  (letrec
    ((split
       (lambda (i k tmp res)
         (cond ((= i k)
                 (cons tmp res))
               ((char=? (string-ref s i) c)
                 (split (+ 1 i)
                        k
                        ""
                        (cons tmp res)))
               (else
                 (split (+ 1 i)
                        k
                        (string-append
                          tmp
                          (string (string-ref s i)))
                        res))))))
    (let ((k (string-length s)))
      (reverse! (split 0 k "" '())))))
