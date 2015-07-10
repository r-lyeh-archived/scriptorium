; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009,2010
; Placed in the Public Domain
;
; (string-find string1 string2)          ==>  string | #f
; (string-ci-find string1 string2)       ==>  string | #f
; (string-find-word string1 string2)     ==>  string | #f
; (string-ci-find-word string1 string2)  ==>  string | #f
;
; (load-from-library "string-find.scm")
;
; Find the first occurrence of a small string STRING1 in a large
; string STRING2. Return the first substring of STRING2 beginning
; with STRING1. When STRING2 does not contain STRING1, return #F.
; STRING-CI-FIND performs the same function, but ignores case.
;
; STRING-FIND-WORD (STRING-CI-FIND-WORD) differs from STRING-FIND
; (STRING-CI-FIND) in that is matches only full words, where a full
; word is a subsequence of characters that is delimited on both
; sides by one of the following:
;
;         - the beginning of the string;
;         - the end of the string;
;         - a non-alphabetic character.
;
; Example:   (string-find "ein" "gemeinsam")     ==>  "einsam"
;            (string-find "people" "democracy")  ==>  #f
;            (string-find-word "me" "test me")   ==>  "me"
;            (string-find-word "me" "testme")    ==>  #f

(load-from-library "string-position.scm")

(define (make-find pos)
  (lambda (u s)
    (let ((i (pos u s)))
      (and i (substring s i (string-length s))))))

(define string-find         (make-find string-position))
(define string-ci-find      (make-find string-ci-position))
(define string-find-word    (make-find string-word-position))
(define string-ci-find-word (make-find string-ci-word-position))
