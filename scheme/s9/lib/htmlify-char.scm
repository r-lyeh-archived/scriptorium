; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (htmlify-char char)      ==>  string
; (htmlify-string string)  ==>  string
;
; HTMLIFY-CHAR converts a char to a string that is save for
; inclusion in an HTML document. HTMLIFY-STRING does the
; same for a string.
;
; Example:   (htmlify-char #\<)      ==>  "&lt;"
;            (htmlify-string "<&>")  ==>  "&lt;&amp;&gt;"

(define (htmlify-char c)
  (cond ((char=? c #\<) "&lt;")
        ((char=? c #\>) "&gt;")
        ((char=? c #\&) "&amp;")
        (else           (string c))))

(define (htmlify-string s)
  (apply string-append
         (map htmlify-char (string->list s))))
