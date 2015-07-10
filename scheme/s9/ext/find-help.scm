; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (find-help string1)          ==>  list
; (find-help string1 string2)  ==>  list | unspecific
;
; Search the online help database for entries containing the
; word STRING1 and return a list the names of all pages that
; contain the word. When STRING2 is passed to FIND-HELP, it
; is interpreted as a string of options. The following options
; will be evaluated:
;
;         a  Search for substrings instead of full words.
;            This options typically yields more results.
;         l  Return not only the page names but also the
;            context (up to three lines) in which the match
;            was found: (page-name (line1 line2 line3)).
;         p  Print the results instead of returning them.
;            In this case, the result of FIND-HELP is
;            unspecific.
;
; Unknown option characters will be ingored.
;
; (Example): (find-help "help")  ==>  ("help" "locate-file")

(require-extension sys-unix)

(load-from-library "find-help-path.scm")
(load-from-library "basename.scm")
(load-from-library "read-line.scm")
(load-from-library "string-find.scm")
(load-from-library "mergesort.scm")
(load-from-library "displaystar.scm")

(define (search-help-page page what any long)
  (with-input-from-file
    page
    (lambda ()
      (let loop ((line     (read-line))
                 (prev     '()))
        (cond ((eof-object? line)
                '())
              (((if any string-ci-find string-ci-find-word) what line)
                (if long
                    `(,(list (basename page)
                             (append prev
                                     (list line)
                                     (let ((next (read-line)))
                                       (if (eof-object? next)
                                           '()
                                           (list next))))))
                   `(,(basename page))))
              (else
                (loop (read-line) (list line))))))))

(define (print-results pages)
  (for-each (lambda (match)
              (cond ((pair? match)
                      (display* (car match) #\newline)
                      (for-each (lambda (desc)
                                  (if (not (string=? "" desc))
                                      (display* "    " desc #\newline)))
                                (cadr match))
                              (newline))
                    (else
                      (display* match #\newline))))
            pages))

(define find-help
  (let ((find-help-path   find-help-path)
        (search-help-page search-help-page)
        (print-results    print-results))
    (lambda (what . opts)
      (let ((help-path (find-help-path))
            (opts      (if (null? opts)
                           '()
                           (string->list (car opts)))))
        (if (not help-path)
            (error "help pages not found in *library-path*"))
        (let loop ((pages (sys:readdir help-path))
                   (found '()))
          (let ((page (if (null? pages)
                          ""
                          (string-append help-path "/" (car pages)))))
            (cond ((null? pages)
                    (let ((result (apply
                                    append
                                    (map (lambda (page)
                                         (search-help-page page
                                                           what
                                                           (memv #\a opts)
                                                           (memv #\l opts)))
                                  (mergesort string<? found)))))
                      (if (memv #\p opts)
                          (print-results result)
                          result)))
                  ((sys:lstat-regular? page)
                    (loop (cdr pages)
                          (cons page found)))
                  (else
                    (loop (cdr pages)
                          found)))))))))
