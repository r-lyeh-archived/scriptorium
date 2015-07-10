; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (display-usage list)                       ==>  unspecific
; (option char boolean object)               ==>  opt-list
; (option char symbol object)                ==>  opt-list
; (opt-char opt-list)                        ==>  char
; (opt-arg? opt-list)                        ==>  boolean
; (opt-type opt-list)                        ==>  symbol
; (opt-val  opt-list)                        ==>  object
; (parse-options! list1 list2 procedure|#f)  ==>  list
;
; (load-from-library "parse-optionsb.scm")
;
; The LIST1 argument of PARSE-OPTIONS! is a list of strings
; containing option groups and their arguments, if any. LIST2
; is a list of lists describing the expected options. LIST2
; has the following format:
;
;         (list (option opt-chr1 arg-type1 [default])
;               ...
;               (option opt-chrN arg-typeN [default]))
;
; Each OPT-CHR is a character associated with that option and
; each ARG-TYPE is the type of the option. When it is a boolean,
; it indicates whether the option expects an argument. When it
; is a symbol, it must be one of the following:
;
; 'STRING   The option may have any type of argument.
;           (This is in fact the same as #T.)
; 'INTEGER  The option expects a numeric (integer) argument.
; 'FILE     The option expects a file name. The file must
;           exist and must be readable by the calling program.
; 'COUNT    The option has no argument, but specifying it
;           multiples times increases its value instead of
;           just setting it to #T.
;
; When a default object is specified in OPTION, it constitutes
; the default value of this option when it is not specified in
; the LIST1 argument of PARSE-OPTIONS!.
;
; The OPT-CHAR and OPT-TYPE procedures access the values of an
; opt-list return by OPTION. OPT-ARG? checks whether an option
; expects an argument. The OPT-VAL procedure returns the current
; value of that option. The initial value of each option is #F.
;
; The PARSE-OPTIONS procedure parses the options passed to it
; in LIST1. Each option group (e.g.: "-abc") and argument must
; be packaged in a separate string, i.e. '("-o" "file") works,
; but '("-o file") probably does not. The SYS:COMMAND-LINE
; procedure is typically used to supply a suitable LIST1.
;
; Each option character in LIST1 must be prefixed with a #\-
; or #\+ character. A #\- will set the value of an option to
; #T, a #\+ will set it to #F. When (opt-arg? x) is true for
; an option X, it will expect an argument in the subsequent
; option group. The value of the option will be that string,
; no matter whether the option character is preceded by #\-
; or #\+ character.
;
; When PARSE-OPTIONS detects a formal error (unknown option
; character, missing argument, etc), it will invoke the given
; PROCEDURE, which should print a short synopsis (a.k.a "usage").
; When #F is privided in the place of the usage procedure, a
; default message will be passed to ERROR. After running a
; user-supplied usage procedure the program will terminate with
; (sys:exit 1).
;
; When PARSE-OPTIONS returns, it delivers a list of trailing
; option groups that do not begin with a #\- or #\+ character.
;
; The special option "--" can be used to abort option processing.
; In this case, any remaining option groups will be returned,
; no matter which characters they contain.
;
; The DISPLAY-USAGE procedure prints the strings in its LIST
; argument. Each member prints in an individual line. When
; a non-string is found in LIST, DISPLAY-USAGE will assume it
; is a null-ary usage procedure, as described above, and call
; it.
;
; Example:   (option #\o 'string)             ==>  (#\o string (#f))
;            (opt-char (option #\o 'string))  ==>  #\o
;            (opt-arg? (option #\o 'string))  ==>  #t
;            (opt-type (option #\o 'string))  ==>  string
;            (opt-val  (option #\o 'string))  ==>  #f
;
;            (parse-options! '("-o" "file1" "file2")
;                            `(,(option #\o #t))
;                            #f)                   ==>  ("file2")

(require-extension sys-unix)

(define opt-char           car)
(define opt-type           cadr)
(define opt-arg?           (lambda (x) (and (cadr x) #t)))
(define opt-box            caddr)
(define (opt-val opt)      (car (opt-box opt)))

(define (option char arg? . default)
  (list char
        arg?
        (list (cond ((null? default)
                      #f)
                    ((not (null? (cdr default)))
                      (error "option: too many arguments" default))
                    (else
                      (car default))))))

(define (parse-options! options table usage)

  (define (set-opt-val! chars box type opts usage)
    (cond ((not (null? (cdr chars)))
            (if usage
                (begin (usage)
                       (sys:exit 1))
                (error
                  (string-append
                    "parse-options!: blank required before argument: "
                    (string (car chars))))))
          ((null? (cdr options))
            (if usage
                (begin (usage)
                       (sys:exit 1))
                (error
                  (string-append "parse-options!: missing argument: -"
                                 (string (car chars))))))
          ((or (eq? type #t)
               (eq? type 'string))
            (set-car! box (if (symbol? (cadr opts))
                              (symbol->string (cadr opts))
                              (cadr opts))))
          ((eq? type 'integer)
            (let ((v (string->number (if (symbol? (cadr opts))
                                         (symbol->string (cadr opts))
                                         (cadr opts)))))
              (if v
                  (set-car! box v)
                  (error
                    (string-append "parse-options!: expected number: -"
                                   (string (car chars)))))))
          ((eq? type 'file)
            (let ((file (if (symbol? (cadr opts))
                            (symbol->string (cadr opts))
                            (cadr opts))))
              (if (sys:access file sys:access-r-ok)
                  (set-car! box file)
                  (error
                    (string-append "parse-options!: expected existing file: -"
                                   (string (car chars)))))))
          (else
            (error (string-append "parse-options!: invalid argument type: `"
                                  (symbol->string type)
                                  "'")))))

  (define (gen->list x)
    (string->list (if (symbol? x) (symbol->string x) x)))

  (define (gen-ref x n)
    (string-ref (if (symbol? x) (symbol->string x) x) n))

  (let opt-loop ((options options))
    (if (null? options)
        '()
        (let* ((opt-group (car options))
               (opt-ind   (gen-ref opt-group 0)))
          (if (not (memv opt-ind  '(#\- #\+)))
              options
              (let char-loop ((opt-chars (cdr (gen->list opt-group))))
                (if (null? opt-chars)
                    (opt-loop (cdr options))
                    (let tbl-loop ((tbl table))
                      (cond ((char=? #\- (car opt-chars))
                              (cdr options))
                            ((null? tbl)
                              (if usage
                                  (begin (usage)
                                         (sys:exit 1))
                                  (error 
                                    (string-append
                                      "parse-options!: unknown option: `"
                                      (string (car opt-chars))
                                      "'"))))
                            ((char=? (car opt-chars) (opt-char (car tbl)))
                              (cond ((eq? 'counter (opt-type (car tbl)))
                                      (set-car! (opt-box (car tbl))
                                                (if (number?
                                                      (opt-val (car tbl)))
                                                    (+ 1 (opt-val (car tbl)))
                                                    1))
                                      (char-loop (cdr opt-chars)))
                                    ((opt-arg? (car tbl))
                                      (set-opt-val! opt-chars
                                                    (opt-box (car tbl))
                                                    (opt-type (car tbl))
                                                    options
                                                    usage)
                                      (opt-loop (cddr options)))
                                    (else
                                      (set-car! (opt-box (car tbl))
                                                (char=? opt-ind #\-))
                                      (char-loop (cdr opt-chars)))))
                            (else
                              (tbl-loop (cdr tbl))))))))))))

(define (display-usage usage)
  (for-each (lambda (line)
              (cond ((string? line)
                      (display line)
                      (newline))
                    (else
                      (line))))
            usage))
