; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (read-file)             ==>  list
; (read-file input-port)  ==>  list
;
; Read a text file from an input port, return a list containing
; one string for each line read. When no INPUT-PORT is specified,
; read the current input port.
;
; (Example): (with-input-from-file "lib/read-file.scm" read-file)
;              ==>  [lots of lines]

(load-from-library "read-line.scm")

(define (read-file . port)
  (letrec
    ((collect-lines
       (lambda (ln lines)
         (cond ((eof-object? ln)
                 (reverse! lines))
               (else
                 (collect-lines (apply read-line port)
                                (cons ln lines)))))))
    (collect-lines (apply read-line port) '())))
