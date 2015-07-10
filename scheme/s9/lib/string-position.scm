; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (string-position string1 string2)          ==>  integer | #f
; (string-ci-position string1 string2)       ==>  integer | #f
; (string-word-position string1 string2)     ==>  integer | #f
; (string-ci-word-position string1 string2)  ==>  integer | #f
;
; (load-from-library "string-position.scm")
;
; Find the first occurrence of a small string STRING1 in a large
; string STRING2. Return the position of the first occurrence of
; STRING1 in STRING2 (positions start at 0). When STRING2 does not
; contain STRING1, return #F. STRING-CI-POSITION performs the same
; function, but ignores case.
;
; STRING-WORD-POSITION (STRING-CI-WORD-POSITION) differs from
; STRING-POSITION (STRING-CI-POSITION) in that is matches only full
; words, where a full word is a subsequence of characters that is
; delimited on both sides by one of the following:
;
;         - the beginning of the string;
;         - the end of the string;
;         - a non-alphabetic character.
;
; Example:   (string-position "ein" "gemeinsam")     ==>  3
;            (string-position "people" "democracy")  ==>  #f
;            (string-word-position "me" "test me")   ==>  5
;            (string-word-position "me" "testme")    ==>  #f

(define (make-string-position p?)
  (lambda (u s)
    (let ((ks (string-length s))
          (ku (string-length u)))
      (let find ((i 0))
        (cond ((> (+ i ku) ks)
                #f)
              ((p? u (substring s i (+ i ku)))
                i)
              (else
                (find (+ 1 i))))))))

(define (make-string-word-position p?)
  (lambda (w s)
    (let ((ks (string-length s))
          (kw (string-length w)))
      (let find ((i 0))
        (cond ((> (+ i kw) ks) #f)
              ((and (p? w (substring s i (+ i kw)))
                    (or (zero? i)
                        (not (char-alphabetic?
                               (string-ref s (- i 1)))))
                    (or (= ks (+ i kw))
                        (not (char-alphabetic?
                               (string-ref s (+ i kw))))))
                i)
              (else
                (find (+ 1 i))))))))

(define string-position         (make-string-position string=?))
(define string-ci-position      (make-string-position string-ci=?))
(define string-word-position    (make-string-word-position string=?))
(define string-ci-word-position (make-string-word-position string-ci=?))
