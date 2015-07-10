; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (string-last-position string1 string2)          ==>  integer | #f
; (string-ci-last-position string1 string2)       ==>  integer | #f
; (string-last-word-position string1 string2)     ==>  integer | #f
; (string-ci-last-word-position string1 string2)  ==>  integer | #f
;
; (load-from-library "string-last-position.scm")
;
; Find the last occurrence of a small string STRING1 in a large
; string STRING2. Return the position of the rightmost substring of
; STRING2 beginning with STRING1. When STRING2 does not contain
; STRING1, return #F. STRING-CI-LAST-POSITION performs the same
; function, but ignores case.
;
; STRING-LAST-WORD-POSITION (STRING-CI-LAST-WORD-POSITION) differs from
; STRING-LAST-POSITION (STRING-CI-LAST-POSITION) in that is matches only
; full words, where a full word is a subsequence of characters that
; is delimited on both sides by one of the following:
;
;         - the beginning of the string;
;         - the end of the string;
;         - a non-alphabetic character.
;
; Example:   (string-last-position "a" "aaaaa")        ==>  4
;            (string-ci-last-position "A" "ab ac")     ==>  3
;            (string-last-position "ax" "ab ac")       ==>  #f
;            (string-last-word-position "a" "ab a c")  ==>  3
;            (string-last-word-position "a" "ab ac")   ==>  #f

(define (make-string-last-pos p?)
  (lambda (u s)
    (let ((ks (string-length s))
          (ku (string-length u)))
      (let find ((i (- ks ku)))
        (cond ((negative? i)
                #f)
              ((p? u (substring s i (+ i ku)))
                i)
              (else
                (find (- i 1))))))))

(define (make-string-last-word-pos p?)
  (lambda (w s)
    (let ((ks (string-length s))
          (kw (string-length w)))
      (let find ((i (- ks kw)))
        (cond ((negative? i) #f)
              ((and (p? w (substring s i (+ i kw)))
                    (or (zero? i)
                        (not (char-alphabetic?
                               (string-ref s (- i 1)))))
                    (or (= ks (+ i kw))
                        (not (char-alphabetic?
                               (string-ref s (+ i kw))))))
                i)
              (else
                (find (- i 1))))))))

(define string-last-position         (make-string-last-pos string=?))
(define string-ci-last-position      (make-string-last-pos string-ci=?))
(define string-last-word-position    (make-string-last-word-pos string=?))
(define string-ci-last-word-position (make-string-last-word-pos string-ci=?))
