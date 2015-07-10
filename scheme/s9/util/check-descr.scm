; Check if there is a description in util/descriptions
; for each .scm file in lib/, contrib/, and ext/.

(load-from-library "hof.scm")
(load-from-library "filter.scm")
(load-from-library "regex.scm")
(load-from-library "displaystar.scm")
(load-from-library "hash-table.scm")
(load-from-library "read-file.scm")
(load-from-library "string-split.scm")
(load-from-library "basename.scm")

(define *descriptions* (make-hash-table))

(for-each
  (lambda (line)
    (hash-table-set! *descriptions*
                     (car (string-split #\| line))
                     #t))
  (with-input-from-file
    "util/descriptions"
    read-file))

(define (have-description? name)
  (and (hash-table-ref *descriptions* (basename name)) #t))

(for-each
  (lambda (scm)
    (if (not (have-description? scm))
        (display* scm ": no description" #\newline)))
  (let ((re (re-comp ".scm$")))
    (filter (curry re-match re)
            (apply append
                   (map (lambda (x)
                          (map (curry string-append x "/")
                               (sys:readdir x)))
                        '("lib"
                          "contrib"
                          "ext"
                          "prog"
                          "util"))))))
