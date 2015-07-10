; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009-2010
; Placed in the Public Domain
;
; (string-parse string1 string2)  ==>  list
;
; Split a string into substrings. STRING1 is a set of separators.
; Return a list containing all coherent sequences of non-separating
; characters contained in the given string.
;
; Example:   (string-parse " ?" " to be  or  not to be? ")
;              ==>  ("to" "be" "or" "not" "to" "be")

(define (string-parse c* s)
  (let ((c* (string->list c*)))
    (letrec
      ((skip-separators
         (lambda (i k)
           (cond ((= i k)
                   i)
                 ((memv (string-ref s i) c*)
                   (skip-separators (+ i 1) k))
                 (else
                   i))))
       (parse
         (lambda (i k tmp res)
           (cond ((= i k)
                   (if (string=? "" tmp)
                       res
                       (cons tmp res)))
                 ((memv (string-ref s i) c*)
                   (parse (skip-separators i k)
                           k
                           ""
                           (cons tmp res)))
                 (else
                   (parse (+ 1 i)
                          k
                          (string-append
                            tmp
                            (string (string-ref s i)))
                          res))))))
      (let ((k (string-length s)))
        (reverse! (parse (skip-separators 0 k) k "" '()))))))
