; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (let/cc <name> . <body>)  ==>  object
;
; (load-from-library "letcc.scm")
;
; Bind the current continuation to <name> and evaluate <body> with
; that binding in effect. 
;
; Example:   (let/cc exit
;              (letrec
;                ((f (lambda (x)
;                      (cond ((null? x) 0)
;                            ((pair? x) (+ 1 (f (cdr x))))
;                            (else      (exit 'foo))))))
;                (f '(1 2 3 . 4))))                         ==> foo

(define-syntax (let/cc name . body)
  `(call/cc
     (lambda (,name)
       ,@body)))
