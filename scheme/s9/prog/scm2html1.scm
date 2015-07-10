#! /usr/local/bin/s9 -f

; scm2html -- print Scheme code to HTML
; By Nils M Holm, 2009,2010
; Placed in the Public Domain
;
; Usage: scm2html [-9dsx] [file ...]
;
; Render Scheme code contained in the given file. When no file
; is given render stdin. Write output to stdout.
;
; Options:
;
; -9  highlight S9fES (non-R4RS) procedures
; -d  write a full HTML document (default: PRE block only)
; -s  enable CSS-based syntax highlighting
; -x  highlight S9fES extension procedures
; -L  emit Lout output instead of HTML
;
; The CSS2 style sheet "scheme.css" contains the default style for
; syntax and expression highlighting.
;
; NOTE: This program handles only a subset of R4RS Scheme correctly.
; Caveat utilitor!

(load-from-library "scm2html.scm")
(load-from-library "parse-optionsb.scm")

(define show-help     (option #\h #f))
(define show-matches  (option #\s #f))
(define full-html     (option #\d #f))
(define mark-s9-procs (option #\9 #f))
(define tilde-quotes  (option #\t #f))
(define mark-s9-extns (option #\x #f))
(define lout-mode     (option #\L #f))
(define options      `(,show-matches
                       ,full-html
                       ,mark-s9-procs
                       ,tilde-quotes
                       ,mark-s9-extns
                       ,show-help
                       ,lout-mode))

(define (usage)
  (display "Usage: scm2html [-9dstxL] [file ...]")
  (newline))

(let ((files (parse-options! (sys:command-line) options usage)))
  (cond ((opt-val show-help)
          (display-usage
            `(""
              ,usage
              ""
              "Render Scheme code in HTML"
              ""
              "-9  highlight S9fES (non-R4RS) procedures"
              "-d  write full HTML document (default: PRE block only)"
              "-s  enable CSS-based syntax highlighting"
              "-t  enable invisible tilde quotation"
              "-x  highlight S9fES extension procedures"
              "-L  emit Lout output instead of HTML"
              ""))
          (sys:exit 0))
        ((null? files)
          (scm2html 'full-html: (opt-val full-html)
                    'show-matches: (opt-val show-matches)
                    'mark-s9-procs: (opt-val mark-s9-procs)
                    'tilde-quotes: (opt-val tilde-quotes)
                    'mark-s9-extns: (opt-val mark-s9-extns)
                    'lout-mode: (opt-val lout-mode)))
        (else
          (for-each (lambda (file)
                      (with-input-from-file
                        file
                        (lambda ()
                          (scm2html 'full-html: (opt-val full-html)
                                    'show-matches: (opt-val show-matches)
                                    'mark-s9-procs: (opt-val mark-s9-procs)
                                    'tilde-quotes: (opt-val tilde-quotes)
                                    'mark-s9-extns: (opt-val mark-s9-extns)
                                    'lout-mode: (opt-val lout-mode)))))
                    files))))
