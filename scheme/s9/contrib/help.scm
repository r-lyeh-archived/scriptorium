; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009-2014
; Placed in the Public Domain
;
; (help)                     ==>  unspecific
; (help symbol | string)     ==>  unspecific
; (apropos)                  ==>  list
; (apropos symbol | string)  ==>  list
;
; (load-from-library "help.scm")
;
; Display the synopsis of the given procedure or keyword. When
; SYMBOL is described in R4RS, produce its R4RS entry, otherwise
; display a S9FES-specific summary. When no argument is passed
; to HELP, it explains itself.
;
; APROPOS returns a list of all procedure names for which help
; pages exist. When an argument is passed to APROPOS, its output
; is limited to topics whose name contains the argument.
;
; The *LINES-PER-PAGE* variable controls the number of lines
; to be printed by HELP before prompting. Set to #F for continuous
; printing.
;
; (Example): (help 'symbol?)  ==>  unspecific
;
;            Output: R4RS 6.4  (symbol? object)  ==>  boolean
;
;                    Returns #T if OBJECT is a symbol, otherwise returns #F.
;
;                    (symbol? 'foo)          ==>  #t
;                    (symbol? (car '(a b)))  ==>  #t
;                    (symbol? "bar")         ==>  #f
;                    (symbol? 'nil)          ==>  #t
;                    (symbol? '())           ==>  #f
;                    (symbol? #f)            ==>  #f

(load-from-library "name-to-file-name.scm")
(load-from-library "read-line.scm")
(load-from-library "string-find.scm")
(load-from-library "remove.scm")
(load-from-library "mergesort.scm")

(define *lines-per-page* (if (eq? *host-system* 'plan9) #f 23))

(define help
  (let ((name->file-name name->file-name))
    (lambda sym

      (define (more? tty)
        (display "; ----- more (q = quit) -----")
        (let ((s (read-line tty)))
          (if (eof-object? s)
              (begin (newline)
                     #t)
              (not (string-find "q" s)))))

      (define (show-file file)
        (read-line)
        (newline)
        (let ((tty (current-input-port)))
          (with-input-from-file file
            (lambda ()
              (let print ((line (read-line))
                          (lno  1))
                (cond ((eof-object? line)
                        (newline))
                      ((and *lines-per-page*
                            (= lno *lines-per-page*))
                        (if (more? tty)
                            (print line 0)))
                      (else
                        (display line)
                        (newline)
                        (print (read-line) (+ 1 lno)))))))))

      (let* ((name (cond ((null? sym)
                           "help")
                         ((symbol? (car sym))
                           (symbol->string (car sym)))
                         ((string? (car sym))
                           (car sym))
                         (else
                           (error "help: expected string or symbol, got"
                                  (car sym)))))
             (name (name->file-name name)))
        (cond ((locate-file (string-append "help/" name))
                => show-file)
              (else
                (error "help: could not find help page" name)))))))

(define apropos
  (let ((name->file-name name->file-name))
    (lambda sym
      (let* ((name (cond ((null? sym)
                           "")
                         ((symbol? (car sym))
                           (symbol->string (car sym)))
                         ((string? (car sym))
                           (car sym))
                         (else
                           (error "apropos: expected string or symbol, got"
                                  (car sym))))))
        (mergesort
          (lambda (a b)
            (string<=? (symbol->string a)
                       (symbol->string b)))
          (remp null?
                (map (lambda (x)
                       (let ((s (symbol->string x)))
                         (if (and (string-find name s)
                                  (locate-file
                                    (string-append
                                      "help/"
                                      (name->file-name s))))
                             x
                             '())))
                       (let ((index (locate-file "help/INDEX")))
                         (if index
                             (with-input-from-file index read)
                             (error "help index not found"))))))))))
