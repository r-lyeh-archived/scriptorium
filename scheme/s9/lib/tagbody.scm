; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (tagbody {<label> | <statement>} ...)  ==>  unspecific
; (go <label>)                           ==>  undefined
;
; (load-from-library "tagbody.scm")
;
; Implement Common LISP-style TAGBODYs. Each symbol in the TAGBODY
; will be interpreted as a <label> and each list as a <statement>. The
; <statement>s are executed in order exactly like in BEGIN. However,
; when a <statement> executes (go <label>), then control will be
; transferred to the specified label immediately. GO never returns.
; TAGBODY does not deliver any meaningful value.
;
; Caveat: nested TAGBODYs are handled through a global variable.
;
; This construct has probably no practical use in Scheme and I am
; not sure why I wrote it in the first place.
;
; Example:   (let ((x   10)
;                  (x0  1)
;                  (x1  1))
;              (tagbody
;                fib
;                  (if (zero? x)
;                      (go end))
;                  (let ((t x1))
;                    (set! x1 (+ x0 x1))
;                    (set! x0 t))
;                  (set! x (- x 1))
;                  (go fib)
;                end)
;              x1)                       ==>  144

(load-from-library "setters.scm")
(load-from-library "iota.scm")

(define *tagbody-stack* '())

(define-syntax (tagbody . body)

  (define (add-block name stmts labels)
    (let ((stmts (if (null? stmts)
                     '(#f)
                     (reverse! stmts))))
        (cons (cons name stmts)
              labels)))

  (define (body->tagged-blocks body labels name stmts)
    (cond ((null? body)
            (reverse! (add-block name stmts labels)))
          ((symbol? (car body))
            (body->tagged-blocks
              (cdr body)
              (if (and (not name)
                       (null? stmts))
                  labels
                  (add-block name stmts labels))
              (car body)
              '()))
          (else
            (body->tagged-blocks
              (cdr body)
              labels
              name
              (cons (car body) stmts)))))

  (let* ((labels (body->tagged-blocks body '() #f '()))
         (symtab (map cons
                      (map car labels)
                      (iota* 0 (length labels))))
         (blocks (map (lambda (label)
                        `(lambda ()
                           ,@(cdr label)))
                      labels))
         (tags   (apply append
                        (map (lambda (x)
                               (if (car x)
                                   `((,(car x) ',(car x)))
                                   '()))
                             labels))))
    (let ((symtab-sym (gensym "symtab-"))
          (blocks-sym (gensym "blocks-"))
          (end-sym    (gensym "end-"))
          (fail-sym   (gensym "fail-"))
          (goto-sym   (gensym "goto-")))
      `(let ,tags
         (let* ((,symtab-sym ',symtab)
                (,goto-sym   (call/cc (lambda (k) (cons k 0))))
                (,fail-sym   (lambda (label)
                               (if (null? *tagbody-stack*)
                                   (error "tagbody: no such label" label)
                                   ((pop! *tagbody-stack*) label))))
                (go          (lambda (label)
                               ((car ,goto-sym)
                                (cons (car ,goto-sym)
                                      (cond ((assq label ,symtab-sym)
                                              => cdr)
                                            (else
                                              (,fail-sym label)))))))
                (,blocks-sym (vector ,@blocks))
                (,end-sym    (vector-length ,blocks-sym)))
           (push! go *tagbody-stack*)
           (let loop ()
             (if (< (cdr ,goto-sym) ,end-sym)
                 (begin ((vector-ref ,blocks-sym (cdr ,goto-sym)))
                        (set-cdr! ,goto-sym (+ 1 (cdr ,goto-sym)))
                        (loop))
                 (set! *tagbody-stack* (cdr *tagbody-stack*)))))))))
