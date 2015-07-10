#! /usr/local/bin/s9 -f

; s9symbols -- print S9 symbols
; by Nils M Holm, 2012
; Placed in the Public Domain
;
; This procedure extracts all symbols from the S9 help files and
; dumps them to the output port. The resulting list can be used
; as an auto-completion list for the S9 editor.
;
; Usage: s9symbols

(load-from-library "displaystar.scm")
(load-from-library "find-help-path.scm")
(load-from-library "read-line.scm")
(load-from-library "string-parse.scm")
(load-from-library "list-to-set.scm")
(load-from-library "remove.scm")
(load-from-library "hof.scm")
(load-from-library "sort.scm")

(define LP #\()
(define RP #\))

(define (extract-symbols-from-file path)
  (with-input-from-file
    path
    (lambda ()
      (let loop ((line (read-line))
                 (syms '()))
        (if (or (eof-object? line)
                (string=? "" line))
            (if (null? syms)
                '()
                (reverse! (list->set syms)))
            (let* ((s* (string-split LP line))
                   (s* (if (> (length s*) 1)
                           (cadr s*)
                           ""))
                   (s* (string-parse (string #\space RP) s*))
                   (s* (if (not (null? s*))
                           (car s*)
                           "")))
              (loop (read-line) (cons s* syms))))))))

(let* ((hpath  (find-help-path))
       (files  (if hpath
                   (sys:readdir hpath)
                   '()))
       (files  (apply append
                      (map (lambda (x)
                             (let ((file (string-append hpath "/" x)))
                               (if (sys:lstat-regular? file)
                                   (list x)
                                   '())))
                           files)))
       (topics (apply append
                      (map (lambda (file)
                             (extract-symbols-from-file
                               (string-append hpath "/" file)))
                           files)))
       (topics (remove "" (sort string-ci<=? topics))))
  (for-each (curryr display* #\newline) topics))

