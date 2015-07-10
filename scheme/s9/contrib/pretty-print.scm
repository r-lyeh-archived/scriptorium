; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (pretty-print object <option> ...)  ==>  unspecific
; (pp object <option> ...)            ==>  unspecific
; (pp-file string <option> ...)       ==>  unspecific
; (pp-loop <option> ...)              ==>  unspecific
; (pp-string string <option> ...)     ==>  list
; (pp-string list <option> ...)       ==>  list
;
; (load-from-library "pretty-print.scm")
;
; Pretty-print Scheme objects, programs, or files. PRETTY-PRINT attempts
; to figure out whether OBJECT is a valid Scheme program. If so, it prints
; it as code and otherwise it prints it as data. The automatic detection
; can be disabled by passing an option to these procedures (see below).
;
; PP is just a less verbose name for PRETTY-PRINT.
;
; PP-FILE pretty-prints all objects in the file STRING.
;
; PP-LOOP pretty-prints all objects read from (current-input-stream).
;
; PP-STRING pretty-prints the form in STRING and returns a list
; containing one pretty-printed line per member (as strings).
; When a LIST is passed to PP-STRING instead of a STRING, it
; will assume it is a list of separate lines and append them with
; #\NEWLINEs in between.
;
; Any of these procedures accept the following <option>s that may be
; passed to it after the object to print:
;
; 'CODE       Print OBJECT as if it was a Scheme program.
; 'DATA       Print OBJECT as if it was a Scheme object.
; 'SIMPLE     Attempt to keep some special forms in a single line,
;             if they fit in one (IF, AND, OR, BEGIN, LAMBDA).
; 'MARGIN: n  Set the right margin to N. The printer will attempt
;             not to write any output beyond that margin. The default
;             margin is at column 72.
; 'INDENT: n  Indent all lines except for the first one by the given
;             number of blanks (for embedding in editors).
; 'OUTPUT-PORT: p
;             Send all output to the specified output port instead
;             of (current-output-port).
;
; NOTE: This program handles only a subset of R4RS Scheme correctly
; and removes all comments from its input program. Caveat utilitor.
;
; Example:   (pp-string '("(let ((a 1) (b 2))"
;                         "(cons a b))"))
;                                             ==> ("(let ((a 1)"
;                                                  "      (b 2))"
;                                                  "  (cons a b))")

(load-from-library "programp.scm")
(load-from-library "for-all.scm")
(load-from-library "read-from-string.scm")
(load-from-library "write-to-string.scm")
(load-from-library "string-unsplit.scm")

; If your Scheme does not support FLUID-LET or DEFINE-MACRO,
; there is an alternative implementation using SYNTAX-RULES in
; lib/fluid-let-sr.scm.

(load-from-library "fluid-let.scm")

(define *Input*   #f)
(define *Output*  #f)

(define *Convert-unreadable* #f)

(define (read-form)
  (if *Input*
      (let ((form (if (null? *Input*)
                      '()
                      (read-from-string *Input*))))
        (cond ((pair? form)
                (set! *Input* (cdr form))
                (car form))
              ((null? form)
                #f)
              (else
                (set! *Output* (list #f form *Input*))
                (set! *Input* '())
                #f)))
      (read)))

(define (end-of-input? x)
  (if *Input*
      (not x)
      (eof-object? x)))

(define pretty-print
  (let ((read-form     read-form)
        (end-of-input? end-of-input?))
    (lambda (form . options)

      (define *Margin* 72)
      (define *Offset* 0)
      (define *Column* 0)

      (define *Output-port* (current-output-port))
    
      (define *Print-as-code* #f)
      (define *Print-as-data* #f)
      (define *Print-newline* #f)
      (define *Really-print* #t)
      (define *Max-Column* 0)
      (define *Simple* #f)
    
      (define LP "(")
      (define RP ")")
      (define SP " ")
    
      (define (pr-char c)
        (if *Output*
            (if (char=? #\newline c)
                (begin (set-car! *Output*
                                 (list->string (reverse! (car *Output*))))
                       (set! *Output* (cons '() *Output*)))
                (set-car! *Output* (cons c (car *Output*))))
            (write-char c *Output-port*)))
    
      (define (pr-form x)
        (if *Output*
            (for-each pr-char
                      (string->list (display-to-string x)))
            (display x *Output-port*)))
    
      (define (atom? x)
        (and (not (pair? x))
             (not (null? x))
             (not (vector? x))))
    
      (define (object-length x)
        (string-length (write-to-string x)))
    
      (define (exceeds-margin? x . opt-lead)
        (let ((lead (if (null? opt-lead)
                        0
                        (car opt-lead))))
          (>= (+ *Column* lead (string-length (write-to-string x)))
              *Margin*)))
    
      (define (spaces n)
        (and *Really-print*
             (or (zero? n)
                 (begin (pr-char #\space)
                        (spaces (- n 1))))))
    
      (define (linefeed)
        (if *Really-print*
            (pr-char #\newline))
        (set! *Max-Column* *Margin*)
        (spaces *Offset*)
        (set! *Column* *Offset*))
    
      (define (pr s)
        (if *Really-print*
            (pr-form s))
        (set! *Column* (+ *Column* (string-length s)))
        (if (> *Column* *Max-Column*)
            (set! *Max-Column* *Column*)))
    
      (define (really-simple? x)
        (or (not (list? x))
            (not (pair? x))
            (not (memq (car x) '(lambda cond case do if and or let
                                 let* letrec fluid-let begin)))))

      (define (pp-simple-form x)
        (if (or (not (list? x))
                *Simple*
                (for-all really-simple? x))
            (let* ((s (write-to-string x))
                   (k (string-length s)))
              (if (and (> (+ *Column* k) *Margin*)
                       (> *Column* *Offset*))
                  (linefeed))
              (pr s))
            (pp-inline-app x)))

      (define (pp-datum x)
        (cond ((or (null? x)
                   (symbol? x)
                   (boolean? x)
                   (char? x)
                   (number? x)
                   (string? x))
                (pp-simple-form x))
              ((vector? x)
                (if *Really-print*
                    (pr-char #\#))
                (fluid-let ((*Offset* (+ 1 *Offset*)))
                  (pp-pair(vector->list x))))
              ((pair? x)
                (pp-pair x))
              ((procedure? x)
                (pp-simple-form (string->symbol "#<procedure>")))
              (else
                (error "pretty-print: unknown type" x))))
    
      (define (pp-pair x)
        (pr LP)
        (fluid-let ((*Offset* (+ 1 *Offset*)))
          (let pp-members ((x x)
                           (s #f))
            (cond ((pair? x)
                    (if s
                        (if (or (pair? (car x))
                                (vector? (car x)))
                            (linefeed)
                            (pr SP)))
                    (pp-datum (car x))
                    (pp-members (cdr x) #t))
                  ((not (null? x))
                    (pr " . ")
                    (pp-datum x)))))
          (pr RP))
    
      (define (pp-quote x q)
        (pr q)
        (fluid-let ((*Offset* (+ *Offset* (string-length q))))
          (if (program? (cadr x))
              (pp-form (cadr x))
              (pp-datum (cadr x)))))
    
      (define (pp-body x)
        (cond ((not (null? x))
                (pp-form (car x))
                (if (not (null? (cdr x)))
                    (linefeed))
                (pp-body (cdr x)))))
    
      (define (pp-lambda x)
        (cond ((or (not *Simple*)
                   (> (length x) 3)
                   (exceeds-margin? x))
                (pr LP)
                (pr "lambda ")
                (fluid-let ((*Offset* (+ 2 *Offset*)))
                  (pp-datum (cadr x))
                  (linefeed)
                  (pp-body (cddr x))
                  (pr RP)))
              (else
                (pp-simple-form x))))
    
      (define (fits-in-margin? formatter x)
        (fluid-let ((*Column*       *Column*)
                    (*Offset*       *Offset*)
                    (*Max-Column*   0)
                    (*Really-print* #f))
          (formatter x)
          (< *Max-Column* *Margin*)))

      (define (pp-inline-app x)
        (pr LP)
        (pp-simple-form (car x))
        (if (not (null? (cdr x)))
            (pr SP))
        (fluid-let ((*Offset* (+ 2 (object-length (car x)) *Offset*)))
          (pp-body (cdr x)))
        (pr RP))
    
      (define (pp-indented-app x)
        (pr LP)
        (fluid-let ((*Offset* (+ 1 *Offset*)))
          (pp-form (car x)))
        (let ((indent (if (pair? (car x)) 1 2)))
          (fluid-let ((*Offset* (+ indent *Offset*)))
            (if (not (null? (cdr x)))
                (linefeed))
            (pp-body (cdr x)))
          (pr RP)))
    
      (define (indented-style-preferred? x)
        (and (memq x '(call-with-current-continuation
                       call/cc
                       call-with-input-file
                       call-with-output-file
                       with-input-from-file
                       with-output-to-file))
             #t))
    
      (define (pp-application x)
        (cond ((fits-in-margin? pp-simple-form x)
                (pp-simple-form x))
              ((indented-style-preferred? (car x))
                (pp-indented-app x))
              ((fits-in-margin? pp-inline-app x)
                (pp-inline-app x))
              (else
                (pp-indented-app x))))
    
      (define (pp-cond/case what x)
        (letrec
          ((print-clauses
             (lambda (c*)
               (cond ((not (null? c*))
                       (pr LP)
                       (fluid-let ((*Offset* (+ 1 *Offset*)))
                         (if (eq? what 'cond)
                             (pp-form (caar c*))
                             (pp-datum (caar c*))))
                       (fluid-let ((*Offset* (+ 2 *Offset*)))
                         (linefeed)
                         (if (and (eq? 'cond what)
                                  (pair? (cdar c*))
                                  (eq? '=> (cadar c*)))
                             (fluid-let ((*Offset* (+ 3 *Offset*)))
                               (pr "=> ")
                               (pp-body (cddar c*)))
                             (pp-body (cdar c*))))
                       (pr RP)
                       (if (not (null? (cdr c*)))
                           (linefeed))
                       (print-clauses (cdr c*)))))))
          (pr LP)
          (pr (symbol->string what))
          (pr SP)
          (fluid-let ((*Offset* (+ (if (eq? what 'cond) 6 2)
                                   *Offset*)))
            (if (eq? what 'case)
                (begin (pp-simple-form (cadr x))
                       (linefeed)))
            (let ((c* (if (eq? what 'cond)
                          (cdr x)
                          (cddr x))))
              (print-clauses c*)
              (pr RP)))))
    
      (define (pp-do x)
        (letrec
          ((print-inits
             (lambda (x first)
               (cond ((null? x)
                       #f)
                     (first
                       (pp-simple-form (car x))
                       (print-inits (cdr x) #f))
                     (else
                       (linefeed)
                       (pp-simple-form (car x))
                       (print-inits (cdr x) #f)))))
             (init-part cadr)
             (test-part caddr)
             (do-body   cdddr))
          (pr LP)
          (pr "do ")
          (pr LP)
          (fluid-let ((*Offset* (+ 5 *Offset*)))
            (print-inits (init-part x) #t))
          (fluid-let ((*Offset* (+ 4 *Offset*)))
            (linefeed)
            (pr LP)
            (pp-form (car (test-part x)))
            (if (not (null? (cdr (test-part x))))
                (fluid-let ((*Offset* (+ 2 *Offset*)))
                  (linefeed)
                  (pp-body (cdr (test-part x)))))
            (pr RP))
          (fluid-let ((*Offset* (+ 2 *Offset*)))
            (linefeed)
            (pp-body (do-body x)))
          (pr RP)))
    
      (define (pp-just-indent what x)
        (if (and *Simple*
                 (fits-in-margin? pp-simple-form x))
            (pp-simple-form x)
            (begin (pr LP)
                   (pr what)
                   (if (not (null? (cdr x)))
                       (pr SP))
                   (fluid-let ((*Offset* (+ 2 (string-length what) *Offset*)))
                     (let print ((x (cdr x)))
                       (cond ((not (null? x))
                               (pp-form (car x))
                               (if (not (null? (cdr x)))
                                   (linefeed))
                               (print (cdr x)))))
                     (pr RP)))))
    
      (define (pp-let-bindings b* rec)
        (pr LP)
        (fluid-let ((*Offset* (+ 1 *Offset*)))
          (let pp-bindings ((b* b*))
            (cond ((not (null? b*))
                    (pr LP)
                    (pp-simple-form (caar b*))
                    (cond ((and rec (pair? (cadar b*)))
                            (fluid-let ((*Offset* (+ 2 *Offset*)))
                              (linefeed)
                              (pp-form (cadar b*))))
                          (else
                            (pr SP)
                            (fluid-let ((*Offset* (+ 2
                                                     (object-length (caar b*))
                                                     *Offset*)))
                              (pp-form (cadar b*)))))
                    (pr RP)
                    (if (not (null? (cdr b*)))
                        (linefeed))
                    (pp-bindings (cdr b*))))))
        (pr RP))
    
      (define (pp-let x)
        (pr LP)
        (pr "let ")
        (let* ((named?   (symbol? (cadr x)))
               (bind     (if named? (caddr x) (cadr x)))
               (body     (if named? (cdddr x) (cddr x)))
               (name-len (if named?
                             (+ 1 (object-length (cadr x)))
                             0)))
          (fluid-let ((*Offset* (+ 5 name-len *Offset*)))
            (if named?
                (begin (pp-simple-form (cadr x))
                       (pr SP)))
            (pp-let-bindings bind #f))
          (fluid-let ((*Offset* (+ 2 *Offset*)))
            (linefeed)
            (pp-body body))
          (pr RP)))
    
      (define (pp-let* x)
        (pr LP)
        (pr "let* ")
        (fluid-let ((*Offset* (+ 6 *Offset*)))
          (pp-let-bindings (cadr x) #f))
        (fluid-let ((*Offset* (+ 2 *Offset*)))
          (linefeed)
          (pp-body (cddr x)))
        (pr RP))
    
      (define (pp-letrec x)
        (pr LP)
        (pr "letrec ")
        (fluid-let ((*Offset* (+ 2 *Offset*)))
          (linefeed)
          (pp-let-bindings (cadr x) #t))
        (fluid-let ((*Offset* (+ 2 *Offset*)))
          (linefeed)
          (pp-body (cddr x)))
        (pr RP))
    
      (define (pp-fluid-let x)
        (pr LP)
        (pr "fluid-let ")
        (fluid-let ((*Offset* (+ 11 *Offset*)))
          (pp-let-bindings (cadr x) #f))
        (fluid-let ((*Offset* (+ 2 *Offset*)))
          (linefeed)
          (pp-body (cddr x)))
        (pr RP))
    
      (define (pp-define-etc what x)
        (pr LP)
        (pr what)
        (pr SP)
        (pp-simple-form (cadr x))
        (fluid-let ((*Offset* (+ 2 *Offset*)))
          (if (or (and (pair? (caddr x))
                       (eq? 'lambda  (caaddr x)))
                  (pair? (cadr x))
                  (exceeds-margin? x))
              (linefeed)
              (pr SP))
          (pp-body (cddr x)))
        (pr RP))
    
      (define (pp-syntax-rules x)
        (letrec
          ((pp-rules
             (lambda (r*)
               (cond ((not (null? r*))
                       (pr LP)
                       (pp-datum (caar r*))
                       (fluid-let ((*Offset* (+ 2 *Offset*)))
                         (linefeed)
                         (pp-form (cadar r*))
                         (pr RP))
                       (if (not (null? (cdr r*)))
                         (linefeed))
                       (pp-rules (cdr r*)))))))
          (pr LP)
          (pr "syntax-rules ")
          (fluid-let ((*Offset* (+ 14 *Offset*)))
            (pp-datum (cadr x)))
          (fluid-let ((*Offset* (+ 2 *Offset*)))
            (linefeed)
            (pp-rules (cddr x))
            (pr RP))))
    
      (define (pp-form x)
        (if (not (pair? x))
            (pp-datum x)
            (case (car x)
              ((quote)            (pp-quote x "'"))
              ((quasiquote)       (pp-quote x "`"))
              ((unquote)          (pp-quote x ","))
              ((unquote-splicing) (pp-quote x ",@"))
              ((lambda)           (pp-lambda x))
              ((cond)             (pp-cond/case 'cond x))
              ((case)             (pp-cond/case 'case x))
              ((do)               (pp-do x))
              ((if)               (pp-just-indent "if" x))
              ((and)              (pp-just-indent "and" x))
              ((or)               (pp-just-indent "or" x))
              ((let)              (pp-let x))
              ((let*)             (pp-let* x))
              ((letrec)           (pp-letrec x))
              ((fluid-let)        (pp-fluid-let x))               ; S9fES ext.
              ((begin)            (pp-just-indent "begin" x))
              ((define)           (pp-define-etc "define" x))
              ((define-syntax)    (pp-define-etc "define-syntax" x))
              ((syntax-rules)     (pp-syntax-rules x))
              (else               (pp-application x)))))
    
        (set! *Column* 0)
        (set! *Offset* 0)
        (set! *Margin* 72)
        (set! *Print-as-code* #f)
        (set! *Print-as-data* #f)
        (set! *Print-newline* #f)
        (set! *Simple* #f)
        (set! *Convert-unreadable* #f)
        (let loop ((options options))
          (cond ((null? options)
                  (cond ((and *Print-as-code*
                              *Print-as-data*)
                          (error (string-append
                                   "pretty-print: please specify either the"
                                   " CODE or DATA option, but not both")))
                        (*Print-newline*)
                        (*Print-as-code*
                          (pp-form form))
                        (*Print-as-data*
                          (pp-datum form))
                        ((program? form)
                          (pp-form form))
                        (else
                          (pp-datum form))))
                ((eq? 'code (car options))
                  (set! *Print-as-code* #t)
                  (loop (cdr options)))
                ((eq? 'data (car options))
                  (set! *Print-as-data* #t)
                  (loop (cdr options)))
                ((eq? 'linefeed (car options))
                  (set! *Print-newline* #t)
                  (loop (cdr options)))
                ((eq? 'simple (car options))
                  (set! *Simple* #t)
                  (loop (cdr options)))
                ((eq? 'indent: (car options))
                  (if (null? (cdr options))
                      (error "pretty-print: missing argument to INDENT:"))
                  (if (not (number? (cadr options)))
                      (error "pretty-print: non-numeric argument to INDENT:"))
                  (set! *Offset* (cadr options))
                  (loop (cddr options)))
                ((eq? 'margin: (car options))
                  (if (null? (cdr options))
                      (error "pretty-print: missing argument to MARGIN:"))
                  (if (not (number? (cadr options)))
                      (error "pretty-print: non-numeric argument to MARGIN:"))
                  (set! *Margin* (cadr options))
                  (loop (cddr options)))
                ((eq? 'output-port: (car options))
                  (if (null? (cdr options))
                      (error "pretty-print: missing argument to OUTPUT-PORT:"))
                  (if (not (output-port? (cadr options)))
                      (error "pretty-print: expected port in OUTPUT-PORT:"))
                  (set! *Output-port* (cadr options))
                  (loop (cddr options)))
                (else
                  (error "pretty-print: unknown option"
                         (car options)))))
        (linefeed))))

(define pp pretty-print)

(define (pp-loop . options)
  (let pp* ((x (read-form)))
    (cond ((not (end-of-input? x))
            (apply pp x options)
            (let ((next (read-form)))
              (if (not (end-of-input? next))
                  (pp #f 'linefeed))
              (pp* next))))))

(define (pp-file file . options)
  (with-input-from-file
    file
    (lambda ()
      (apply pp-loop options))))

(define (pp-string str* . options)
      (set! *Input* (if (string? str*)
                      str*
                      (string-unsplit #\newline str*)))
      (set! *Output* (list '()))
      (apply pp-loop options)
      (reverse! (cdr *Output*)))
