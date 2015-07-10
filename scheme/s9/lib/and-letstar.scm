; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (and-let* <binding> ... <body>)  ==>  object
;
; (load-from-library "and-letstar.scm")
;
; Each <binding> has the form (<variable> <expression>) and binds
; the given <variable> to the normal form of <expression>.
;
; Like LET*, AND-LET* evaluates its <binding>s in sequence, so each
; <expression> is evaluated in an environment that includes all previous
; <binding>s of the same AND-LET*. Unlike LET*, though, AND-LET* returns
; #F immediately as soon as one of its <expression>s evaluates to #F.
; Only when all <expression>s evaluate to non-#F values, it evaluates
; <body> and returns its value. AND-LET* expands as follows:
;
; (and-let* ((<var1> <expr1>)   --->  (let ((<var1> <expr1>))
;            ...                        (and <var1>
;            (<varN> <exprN>))               ...
;   <body>)                                  (let ((<varN> <exprN>))
;                                              (and <varN>
;                                                   <body>))))
;
; This is only a subset of SRFI-2 AND-LET*.
;
; Example:   (and-let* ((a '((x . 1)))
;                       (a (assq 'x a)))
;              (cdr a))                   ==>  1
;
;            (and-let* ((a '((x . 1)))
;                       (a (assq 'z a)))
;              (cdr a))                   ==>  #f

(define-syntax (and-let* clauses expr . exprs)
  (letrec
    ((nest-let
       (lambda (c)
         (cond ((null? c)
                 (cons expr exprs))
               ((null? (cdr c))
                 `(let ((,(caar c) ,(cadar c)))
                    (and ,(caar c)
                         ,@(cons expr exprs))))
               (else
                 `(let ((,(caar c) ,(cadar c)))
                    (and ,(caar c)
                         ,(nest-let (cdr c)))))))))
    (let loop ((c clauses))
      (cond ((null? c))
            ((or (not (pair? (car c)))
                 (null? (cdar c))
                 (not (null? (cddar c)))
                 (not (symbol? (caar c))))
              (error "and-let*: syntax error" c))
            (else
              (loop (cdr c)))))
    (if (null? clauses)
        `(let () ,expr ,@exprs)
        (nest-let clauses))))
