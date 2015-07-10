#! /usr/local/bin/s9 -f

; scmpp -- Scheme pretty-printer
; by Nils M Holm, 2010
; Placed in the Public Domain
;
; Usage: scmpp [-cds] [-m margin] [file ...]

(load-from-library "pretty-print.scm")
(load-from-library "parse-optionsb.scm")

(define as-code    (option #\c #f))
(define as-data    (option #\d #f))
(define simple     (option #\s #f))
(define margin     (option #\m 'integer 72))
(define show-help  (option #\h #f))
(define options    `(,as-code
                     ,as-data
                     ,simple
                     ,margin
                     ,show-help))

(define (usage)
  (display "Usage: scmpp [-cds] [-m margin] [file ...]")
  (newline))

(let* ((files  (parse-options! (sys:command-line) options usage)))
  (cond ((opt-val show-help)
          (display-usage
            `(""
             ,usage
             ""
             "Pretty-print Scheme code and objects"
             ""
             "-c    assume that input is Scheme code"
             "-d    assume that input is Scheme data"
             "-s    use (one-line) simple forms where possible"
             "-m N  set right margin to column N"
             ""))
      (sys:exit 0)))
  (let ((options (append (if (opt-val as-code)
                             '(code)
                             '())
                         (if (opt-val as-data)
                             '(data)
                             '())
                         (if (opt-val simple)
                             '(simple)
                             '())
                         (list 'margin: (opt-val margin)))))
    (if (null? files)
        (apply pp-loop options)
        (let loop ((files files))
          (if (not (null? files))
              (begin (apply pp-file (car files) options)
                     (loop (cdr files))))))))
