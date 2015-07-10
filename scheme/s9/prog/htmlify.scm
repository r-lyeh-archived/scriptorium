#! /usr/local/bin/s9 -f

; htmlify -- convert plain text files to HTML
; by Nils M Holm, 2010
; Placed in the Public Domain
;
; Usage: htmlify -t [file ...]

(load-from-library "parse-optionsb.scm")

(define (emit-newlines n)
  (if (not (zero? n))
      (begin (newline)
             (emit-newlines (- n 1)))))

(define (htmlify trim)
  (let loop ((c    (read-char))
             (nnl  0)
             (lead #t))
    (if (eof-object? c)
        (if trim
            (newline)
            (emit-newlines nnl))
        (begin (if (char=? (integer->char 10) c)
                   (loop (read-char) (+ 1 nnl) lead)
                   (begin (if (or (not trim)
                                  (not lead))
                              (emit-newlines nnl))
                          (cond ((char=? #\< c)
                                  (display "&lt;")
                                  (loop (read-char) 0 #f))
                                ((char=? #\> c)
                                  (display "&gt;")
                                  (loop (read-char) 0 #f))
                                ((char=? #\& c)
                                  (display "&amp;")
                                  (loop (read-char) 0 #f))
                                (else
                                  (write-char c)
                                  (loop (read-char) 0 #f)))))))))

(define trim-lead (option #\t #f))
(define show-help (option #\h #f))
(define options   `(,trim-lead
                    ,show-help))

(define (usage)
  (display "Usage: htmlify [-t] [file ...]")
  (newline))

(let ((files (parse-options! (sys:command-line) options usage)))
  (if (opt-val show-help)
      (begin (display-usage
               `(""
                 ,usage
                 ""
                 "Convert plain text files to HTML"
                 ""
                 "-t  trim leading and trailing blank lines"
                 ""))
      (sys:exit 0)))
  (if (null? files)
      (htmlify (opt-val trim-lead))
      (for-each (lambda (file)
                  (with-input-from-file
                    file
                    (lambda ()
                      (htmlify (opt-val trim-lead)))))
                files)))
