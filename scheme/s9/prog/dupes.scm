#! /usr/local/bin/s9 -f

; dupes -- find duplicate file names
; by Nils M Holm, 2010
; Placed in the Public Domain
;
; Usage: dupes [directory]

(load-from-library "hof.scm")
(load-from-library "hash-table.scm")
(load-from-library "string-split.scm")
(load-from-library "basename.scm")
(load-from-library "parse-optionsb.scm")
(load-from-library "displaystar.scm")

(define readable-output (option #\r #f))
(define show-help       (option #\h #f))
(define options         `(,readable-output
                          ,show-help))

(define (usage)
  (display "Usage: dupes [-r] [file ...]")
  (newline))

(let* ((files (parse-options! (sys:command-line) options usage))
       (files (if (null? files) '(".") files))
       (found (make-hash-table))
       (first #t)
       (list-dupes
         (lambda (readable)
           (for-each (lambda (set)
                       (if (> (length set) 2)
                           (begin (if first
                                      (set! first #f)
                                      (if readable
                                          (newline)))
                                  (if readable
                                      (display* (car set) ":")
                                      (display (car set)))
                                  (for-each (lambda (loc)
                                              (if readable
                                                  (display* #\newline loc)
                                                  (display* #\space loc)))
                                            (cdr set))
                                  (newline))))
                     (hash-table->alist found)))))
  (if (opt-val show-help)
      (begin (display-usage
               `(""
                 ,usage
                 ""
                 "Find duplicate directory entries"
                 ""
                 "-r  print human-readable output"
                 ""))
      (sys:exit 0)))
  (let loop ((files files))
    (let* ((path (if (null? files)
                     "ignore"
                     (car files)))
           (name (basename path)))
      (cond ((null? files)
              (list-dupes (opt-val readable-output)))
            ((sys:lstat-directory? path)
              (loop (append (map (curry string-append path "/")
                                 (sys:readdir path))
                            (cdr files))))
            ((sys:lstat-symlink? path)
              (loop (cdr files)))
            ((hash-table-ref found name)
              => (lambda (refs)
                   (hash-table-set! found name (cons path (car refs)))
                   (loop (cdr files))))
            (else
              (hash-table-set! found name (list path))
              (loop (cdr files)))))))
