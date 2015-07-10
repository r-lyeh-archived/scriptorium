; Create the CATEGORIES.html file for the webdump.

(load-from-library "read-line.scm")
(load-from-library "string-split.scm")
(load-from-library "displaystar.scm")
(load-from-library "hash-table.scm")
(load-from-library "regex.scm")

(define *descriptions* (make-hash-table))

(define (fetch-descriptions)
  (with-input-from-file
    "util/descriptions"
    (lambda ()
      (let loop ()
        (let ((entry (read-line)))
          (if (not (eof-object? entry))
              (let* ((s*  (string-split #\| entry))
                     (key (car s*))
                     (val (if (string=? "-" (cadddr s*))
                              (caddr s*)
                              (cadddr s*))))
                (hash-table-set! *descriptions* key val)
                (loop))))))))

(define (get-description name)
  (cond ((hash-table-ref *descriptions* name) => car)
        (else "(no description)")))

(define (make-categories)
  (let ((entry (re-comp "<DD>\\(.*\\)</DD>")))
    (let loop ()
      (let ((line (read-line)))
        (cond ((eof-object? line))
              ((re-match entry line)
                => (lambda (match)
                     (let* ((file (apply substring line (cadr match)))
                            (k    (string-length file))
                            (ref  (char=? #\@ (string-ref file (- k 1))))
                            (file (if ref
                                      (substring file 0 (- k 2))
                                      file)))
                       (display* " <DL>"
                                 (if ref "<EM>" "")
                                 "<A href=\""
                                 (string-append file ".html")
                                 "\">"
                                 file
                                 "</A>: "
                                 (get-description file)
                                 (if ref "</EM>" "")
                                 "</DL>"
                                  #\newline)
                       (loop))))
              (else
                (display* line #\newline)
                (loop)))))))

(if (file-exists? "CATEGORIES.html")
    (delete-file "CATEGORIES.html"))

(fetch-descriptions)

(with-input-from-file
  "util/categories.html"
  (lambda ()
    (with-output-to-file
      "CATEGORIES.html"
      make-categories)))
