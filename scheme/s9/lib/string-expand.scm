; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (string-expand string)          ==>  string
; (string-expand string integer)  ==>  string
;
; Expand horizontal tabulation (HT, ASCII 9, aka "TAB") characters
; in STRING to spaces. Return a new string. When INTEGER is specified,
; expand each TAB to INTEGER spaces at maximum (default = 8).
;
; Example:   (let ((tab (integer->char 9)))
;              (string-expand (string #\x tab #\y)))  ==>  "x       y"

(define (string-expand s . n)
  (let ((n   (if (null? n) 8 (car n)))
        (TAB (integer->char 9)))
    (let loop ((in  (string->list s))
               (out '())
               (x   0))
      (cond ((null? in)
              (list->string (reverse! out)))
            ((char=? (car in) TAB)
              (let ((nspaces (- n (remainder x n))))
                (loop (cdr in)
                      (append (string->list (make-string nspaces #\space))
                              out)
                      (+ x nspaces))))
            (else
              (loop (cdr in)
                    (cons (car in) out)
                    (+ 1 x)))))))
