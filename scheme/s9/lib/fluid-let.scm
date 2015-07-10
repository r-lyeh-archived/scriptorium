; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009,2012
; Placed in the Public Domain
;
; (fluid-let ((variable expression) ...) expression ...)  ==>  object
;
; Bind variables dynamically, i.e. assign a dynamic (rather than
; a lexical/static) value to each given variable. The variables
; must be defined outside of FLUID-LET. The difference between
; LET and FLUID-LET is as follows:
;
; (let ((a 0))                   (let ((a 0))
;   (let ((f (lambda () a)))       (let ((f (lambda () a)))
;     (let ((a 1))                   (fluid-let ((a 1))
;       (f))))         ==> 0           (f))))         ==> 1
;
; Example:   (let ((a 0))
;              (let ((f (lambda () a)))
;                (fluid-let ((a 1))
;                  (f))))                ==>  1

; In case your Scheme does not support low-level macros,
; try "fluid-let-sr.scm", which uses SYNTAX-RULES.

(define-syntax (fluid-let bind* . body)
  (letrec
    ((split
       (lambda (bind* vars tmps args)
         (cond ((null? bind*)
                 (list vars tmps args))
               ((or (not (pair? bind*))
                    (not (pair? (car bind*)))
                    (not (symbol? (caar bind*)))
                    (not (pair? (cdar bind*)))
                    (not (null? (cddar bind*))))
                 (error "fluid-let: bad syntax" bind*))
               (else
                 (split (cdr bind*)
                        (cons (caar bind*) vars)
                        (cons (gensym) tmps)
                        (cons (cadar bind*) args)))))))
      (let* ((var-tmp-arg* (split bind* '() '() '()))
             (var* (car var-tmp-arg*))
             (tmp* (cadr var-tmp-arg*))
             (arg* (caddr var-tmp-arg*))
             (env* (map (lambda (t v) `(,t ,v))
                        tmp*
                        var*))
             (ini* (map (lambda (v a) `(set! ,v ,a))
                        var*
                        arg*))
             (res* (map (lambda (v t) `(set! ,v ,t))
                        var*
                        tmp*))
             (val  (gensym)))
         `(let ,env*
            ,@(reverse! ini*)
            (let ((,val (begin ,@body)))
              ,@res*
              ,val)))))
