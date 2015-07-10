; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (letrec* ((<variable> <expression>) ...) <body> ...)  ==>  object
;
; (load-from-library "letrecstar.scm")
;
; LETREC* is like LETREC, but <expression>s can apply procedures
; bound to <variable> defined earlier in the same LETREC*. LETREC
; does not allow procedures to be defined in terms of each other,
; e.g.:
;
;            (letrec ((a (lambda () (lambda () 1)))
;                     (b (a)))
;              (b))                                  ==>  undefined
;
; LETREC* expands as follows:
;
; (letrec* ((<var1> <expr1>)    --->  (let ((<var1> <undefined>)
;           ...                             ...
;           (<varN> <exprN>))               (<varN> <undefined>))
;   <body>)                             (set! <var1> <expr1>)
;                                       ...
;                                       (set! <varN> <exprN>)
;                                       (let ()
;                                         <body>))
;
; LETREC* may be used in the place of R4RS LETREC. The opposite,
; though, does not generally work (see negative example above).
;
; Example:   (letrec* ((a (lambda () (lambda () 1)))
;                      (b (a)))
;              (b))                                  ==>  1

(define-syntax letrec*
  (letrec
    ((check-bindings
       (lambda (b)
         (cond ((null? b))
               ((or (not (pair? b))
                    (not (pair? (car b)))
                    (not (symbol? (caar b)))
                    (not (pair? (cdar b)))
                    (not (null? (cddar b))))
                 (error "letrec*: invalid syntax") b)
               (else
                 (check-bindings (cdr b)))))))
    (lambda (bindings expr . exprs)
      (check-bindings bindings)
      (let ((tmps (map (lambda (x) (gensym)) bindings))
            (vars (map car bindings))
            (args (map cadr bindings)))
        (let ((undefineds (map (lambda (v) (list v #f))
                               vars))
              (updates    (map (lambda (v t) (list 'set! v t))
                               vars
                               args)))
          `(let ,undefineds
             ,@updates
             (let ()
               ,expr
               ,@exprs)))))))
