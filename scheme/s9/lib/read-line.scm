; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (read-line)             ==>  string
; (read-line input-port)  ==>  string
;
; Read a line from an input port. When no INPUT-PORT is specified,
; read the current input port.
;
; Example:   (with-input-from-file "lib/read-line.scm" read-line)
;              ==>  "; Scheme 9 from Empty Space, Function Library"

(define (read-line . port)
  (letrec
    ((collect-chars
       (lambda (c s)
         (cond ((eof-object? c)
                 (if (null? s)
                     c
                     (list->string (reverse! s))))
               ((char=? c #\newline)
                 (list->string (reverse! s)))
               (else
                 (collect-chars (apply read-char port)
                                    (cons c s)))))))
    (collect-chars (apply read-char port) '())))
