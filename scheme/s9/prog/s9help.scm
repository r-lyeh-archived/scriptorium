#! /usr/local/bin/s9 -f

; s9help -- find and display S9fES help pages
; by Nils M Holm, 2010
; Placed in the Public Domain
;
; Usage: s9help [-als] topic ...
;
; Options:
;
; -a  find any match (default: full words only)
; -l  long results (including context)
; -s  search help pages (default: display)

(load-from-library "find-help.scm")
(load-from-library "name-to-file-name.scm")
(load-from-library "read-line.scm")
(load-from-library "parse-optionsb.scm")

(define search     (option #\s #f))
(define long       (option #\l #f))
(define any-match  (option #\a #f))
(define show-help  (option #\h #f))
(define options    `(,search
                     ,long
                     ,any-match
                     ,show-help))

(define (usage)
  (display "Usage: s9help [-als] topic ...")
  (newline))

(define (display-help topic)
  (let ((path (string-append (find-help-path)
                             "/"
                             (name->file-name topic))))
    (if (not (file-exists? path))
        (begin (display* "s9: " topic ": help file not found" #\newline)
               (sys:exit 1)))
    (with-input-from-file
      path
      (lambda ()
        (newline)
        (let print ((line (read-line)))
          (if (eof-object? line)
              (newline)
              (begin (display line)
                     (newline)
                     (print (read-line)))))))))

(let ((topic* (parse-options! (sys:command-line) options usage)))
  (if (opt-val show-help)
      (begin (display-usage
               `(""
                 ,usage
                 ""
                 "Find and display S9fES help pages"
                 ""
                 "-a  find any match (default: full words only)"
                 "-l  long results (including context)"
                 "-s  search help pages (default: display)"
                 ""))
      (sys:exit 0)))
  (let ((topic* (if (null? topic*)
                    (begin (usage)
                           (sys:exit 1))
                    topic*)))
    (for-each (lambda (topic)
                (if (opt-val search)
                    (find-help topic
                               (string-append
                                 "p"
                                 (if (opt-val long) "l" "")
                                 (if (opt-val any-match) "a" "")))
                    (display-help topic)))
              topic*)))
