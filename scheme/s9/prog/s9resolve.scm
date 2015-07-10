#! /usr/local/bin/s9 -f

; s9resolve -- resolve S9fES library dependencies
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; Usage: s9resolve [-dsv] [file]
;
; This program replaces LOAD-FROM-LIBRARY forms in S9fES with the
; code contained in the referenced libraries, thereby turning
; S9fES code into more portable Scheme code.
;
; Options:
;
; -d  dependency check mode
; -s  strip initial comment blocks from included programs
; -v  verbose mode

(load-from-library "symbols.scm")
(load-from-library "flatten.scm")
(load-from-library "string-split.scm")
(load-from-library "displaystar.scm")
(load-from-library "read-line.scm")
(load-from-library "hash-table.scm")
(load-from-library "programp.scm")
(load-from-library "basename.scm")
(load-from-library "parse-optionsb.scm")

(define *depth* 0)

(define dep-check-mode (option #\d #f))
(define skip-comments  (option #\s #f))
(define verbose        (option #\v #f))
(define show-help      (option #\h #f))
(define options        `(,dep-check-mode
                         ,skip-comments
                         ,verbose
                         ,show-help))

(define *Pass-1*      #t)
(define *Filename*    #t)
(define *Definitions* (make-hash-table 'test eq?))
(define *Undefined*   (make-hash-table 'test eq?))
(define *Libraries*   (make-hash-table))

(define (resolve)
  (let loop ((line (read-line)))
    (cond ((eof-object? line))
          ((let* ((tokens (string-split #\space line))
                  (first  (car tokens)))
             (and (= (string-length first) 18)
                  (string-ci=? "(load-from-library" first)))      ;) balance
            (let* ((tokens (string-split #\space line))
                   (lib    (string-split #\" (cadr tokens)))
                   (lib    (if (or (null? lib)
                                   (null? (cdr lib)))
                               (error "malformed load-from-library" line)
                               (cadr lib))))
              (cond ((or (locate-file lib)
                         (locate-file (string-append lib ".scm")))
                      => (lambda (file)
                           (newline)
                           (display "; ----- included file: ")
                           (display (basename file))
                           (display " -----")
                           (newline)
                           (newline)
                           (resolve-file file)
                           (display "; ----- end of file: ")
                           (display (basename file))
                           (display " -----")
                           (newline)
                           (loop (read-line))))
                    (else
                      (error "failed to resolve" lib)))))
          (else
            (display line)
            (newline)
            (loop (read-line))))))

(define (resolve-file file)
  (set! *depth* (+ 1 *depth*))
  (with-input-from-file
    file
    (lambda ()
      (if (opt-val skip-comments)
          (let loop ((line (read-line)))
            (if (and (not (eof-object? line))
                     (not (string=? "" line))
                     (char=? #\; (string-ref line 0)))
                (loop (read-line)))))
      (resolve)))
  (set! *depth* (- *depth* 1))
  (newline))

(define (add-definition name)
  (hash-table-set! *Definitions* name #t))

(define (defined? name)
  (or *Pass-1*
      (and (hash-table-ref *Definitions* name)
           #t)))

(define (seen-before? name)
  (and (hash-table-ref *Undefined* name)
       #t))

(define (seen-before! name)
  (hash-table-set! *Undefined* name #t))

(define (check-deps form)
  (cond ((and (list? form)
              (not (null? form)))
          (case (car form)
                ((and begin if or)
                  (for-each check-deps (cdr form)))
                ((apply)
                  (check-deps (cdr form)))
                ((case)
                  (check-deps (cadr form))
                  (for-each (lambda (x)
                              (for-each check-deps (cdr x)))
                            (cddr form)))
                ((cond)
                  (for-each (lambda (x)
                              (for-each check-deps (cdr x)))
                            (cdr form)))
                ((define define-syntax)
                  (if (pair? (cadr form))
                      (for-each add-definition (flatten (cadr form)))
                      (add-definition (cadr form)))
                  (for-each check-deps (cddr form)))
                ((delay)
                  (check-deps (cadr form)))
                ((do)
                  (for-each (lambda (x)
                              (add-definition (car x))
                              (for-each check-deps (cdr x)))
                            (cadr form))
                  (for-each check-deps (cddr form)))
                ((lambda)
                  (for-each add-definition (flatten (cadr form)))
                  (check-deps (cddr form)))
                ((let)
                  (if (symbol? (cadr form))
                      (add-definition (cadr form)))
                  (for-each (lambda (x)
                               (add-definition (car x))
                               (check-deps (cadr x)))
                            (if (symbol? (cadr form))
                                (caddr form)
                                (cadr form)))
                  (check-deps (if (symbol? (cadr form))
                                  (cdddr form)
                                  (cddr form))))
                ((let* letrec)
                  (for-each (lambda (x)
                               (add-definition (car x))
                               (check-deps (cadr x)))
                            (cadr form))
                  (check-deps (cddr form)))
                ((quote quasiquote)
                  #t)
                ((set!)
                  (check-deps (cadr form)))
                ((syntax-rules)
                  #t)
                (else
                  (if (pair? (car form))
                      (check-deps (car form))
                      (if (and (not (defined? (car form)))
                               (not (seen-before? (car form))))
                          (begin (seen-before! (car form))
                                 (display* *Filename*
                                           ": "
                                           (car form)
                                           #\newline))))
                  (for-each check-deps (cdr form)))))))

(define (dep-check)
  (let loop ((form (read)))
    (cond ((eof-object? form))
          ((and (pair? form)
                (program? form))
            (if (eq? 'load-from-library (car form))
                  (dep-check-lib (cadr form)))
            (check-deps form)
            (loop (read)))
          (else
            (loop (read))))))

(define (dep-check-file file)
  (let ((old-file *Filename*))
    (set! *Filename* file)
    (if (opt-val verbose)
        (display* "; File: " *Filename* #\newline))
    (set! *Pass-1* #t)
    (with-input-from-file file dep-check)
    (set! *Pass-1* #f)
    (with-input-from-file file dep-check)
    (set! *Pass-1* #t)
    (set! *Filename* old-file)))

(define (dep-check-lib lib)
  (if (not (hash-table-ref *Libraries* lib))
    (let ((path (locate-file lib)))
      (hash-table-set! *Libraries* lib #t)
      (if (not lib)
          (error "library not found" lib)
          (dep-check-file path)))))

(define (usage)
  (display "Usage: s9resolve [-ds] [file ...]")
  (newline))

(for-each (lambda (x)
            (hash-table-set! *Definitions* x #t))
          (append (r4rs-syntax-objects)
                  (r4rs-procedures)
                  (s9fes-syntax-objects)
                  (s9fes-procedures)
                  (s9fes-extension-procedures)
                  (s9fes-extension-symbols)))

(let ((files (parse-options! (sys:command-line) options usage)))
  (cond ((opt-val show-help)
          (display-usage
            `(""
              ,usage
              ""
              "Resolve or check S9 library dependencies"
              ""
              "-d  dependency check mode"
              "-s  strip initial comment blocks from included programs"
              "-v  verbose mode"
              ""))
          (sys:exit 0))
        ((null? files)
          (if (opt-val dep-check-mode)
              (error "-d requires a file")
              (resolve)))
        ((opt-val dep-check-mode)
          (for-each dep-check-file files))
        (else
          (for-each (lambda (file)
                      (newline)
                      (with-input-from-file file resolve))
                    files))))
