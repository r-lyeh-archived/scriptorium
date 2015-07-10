; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (loutify-char char)      ==>  string
; (loutify-string string)  ==>  string
;
; LOUTIFY-CHAR converts a char to a string that is save for
; inclusion in an Lout document. LOUTIFY-STRING does the
; same for a string.
;
; Example:   (loutify-char #\")        ==>  "\"\\\"\""
;            (loutify-string "\"x\"")  ==>  "\"\\\"x\\\"\""

(define (loutify-char c)
  (let ((quotify
          (lambda (c)
            (string-append "\"" c "\""))))
    (cond ((char=? c #\") (quotify "\\\""))
          ((char=? c #\@) (quotify "@"))
          ((char=? c #\/) (quotify "/"))
          ((char=? c #\|) (quotify "|"))
          ((char=? c #\&) (quotify "&"))
          ((char=? c #\#) (quotify "#"))
          ((char=? c #\\) (quotify "\\\\"))
          ((char=? c #\{) (quotify "{"))
          ((char=? c #\}) (quotify "}"))
          ((char=? c #\`) "{@BQ}")
          (else           (string c)))))

(define (loutify-string s)
  (string-append
    "\""
    (apply string-append
           (map (lambda (c)
                  (cond ((char=? #\" c) "\\\"")
                        ((char=? #\\ c) "\\\\")
                        (else           (string c))))
                (string->list s)))
    "\""))
