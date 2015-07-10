#! /usr/local/bin/s9 -f

; c2html -- print C code to HTML
; By Nils M Holm, 2009-2012
; Placed in the Public Domain
;
; Usage: c2html [-dL] [file ...]
;
; Render C code contained in the given file. When no file
; is given, render stdin. Write output to stdout.
;
; Options:
;
; -d  write a full HTML document (default: PRE block only)
; -L  emit Lout output instead of HTML
;
; The CSS2 style sheet "ccode.css" contains the default style for
; syntax highlighting.
;
; NOTE: This program handles only a subset of C89 correctly.
; Caveat utilitor!

(load-from-library "c2html.scm")
(load-from-library "parse-optionsb.scm")

(define show-help     (option #\h #f))
(define full-html     (option #\d #f))
(define lout-mode     (option #\L #f))
(define options      `(,full-html
                       ,lout-mode
                       ,show-help))

(define (usage)
  (display "Usage: c2html [-dL] [file ...]")
  (newline))

(let ((files (parse-options! (sys:command-line) options usage)))
  (cond ((opt-val show-help)
          (display-usage
            `(""
              ,usage
              ""
              "Render C code in HTML"
              ""
              "-d  write full HTML document (default: PRE block only)"
              "-L  emit Lout output instead of HTML"
              ""))
          (sys:exit 0))
        ((null? files)
          (c2html 'full-html: (opt-val full-html)
                  'lout-mode: (opt-val lout-mode)))
        (else
          (for-each (lambda (file)
                      (with-input-from-file
                        file
                        (lambda ()
                          (c2html 'full-html: (opt-val full-html)
                                  'lout-mode: (opt-val lout-mode)))))
                    files))))
