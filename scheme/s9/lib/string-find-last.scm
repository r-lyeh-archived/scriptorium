; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009,2010
; Placed in the Public Domain
;
; (string-find-last string1 string2)          ==>  string | #f
; (string-ci-find-last string1 string2)       ==>  string | #f
; (string-find-last-word string1 string2)     ==>  string | #f
; (string-ci-find-last-word string1 string2)  ==>  string | #f
;
; (load-from-library "string-find-last.scm")
;
; Find the last occurrence of a small string STRING1 in a large
; string STRING2. Return the last substring of STRING2 beginning
; with STRING1. When STRING2 does not contain STRING1, return #F.
; STRING-CI-FIND-LAST performs the same function, but ignores case.
;
; STRING-FIND-LAST-WORD (STRING-CI-FIND-LAST-WORD) differs from
; STRING-FIND-LAST (STRING-CI-FIND-LAST) in that is matches only
; full words, where a full word is a subsequence of characters that
; is delimited on both sides by one of the following:
;
;         - the beginning of the string;
;         - the end of the string;
;         - a non-alphabetic character.
;
; Example:   (string-find-last "a" "aaaaa")        ==>  "a"
;            (string-ci-find-last "A" "ab ac")     ==>  "ac"
;            (string-find-last "ax" "ab ac")       ==>  #f
;            (string-find-last-word "a" "ab a c")  ==>  "a c"
;            (string-find-last-word "a" "ab ac")   ==>  #f

(load-from-library "string-last-position.scm")

(define (make-str-pos pos)
  (lambda (u s)
    (let ((i (pos u s)))
      (and i (substring s i (string-length s))))))

(define string-find-last         (make-str-pos string-last-position))
(define string-ci-find-last      (make-str-pos string-ci-last-position))
(define string-find-last-word    (make-str-pos string-last-word-position))
(define string-ci-find-last-word (make-str-pos string-ci-last-word-position))
