; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (until <test-expression> <body>)   ==>  unspecific
; (while <test-expression> <body>)   ==>  unspecific
;
; (load-from-library "while.scm")
;
; The WHILE form first evaluates <test-expression>. When it evaluates
; to a true value, it also evaluates <body>, which is a sequence of
; expressions. The expressions will be evaluated in order and then the
; WHILE form will be re-entered by evaluating <test-expression> once
; again. Then WHILE form terminates only if the test expression returns
; #F. The value of he form is unspecific.
;
; UNTIL is like WHILE, but evaluates its <body> until <test-expression>
; evaluates to truth.
;
; Example:   (let ((x 0)
;                  (y 1))
;              (while (< x 10)
;                (set! y (* 2 y))
;                (set! x (+ 1 x)))
;              y)                   ==>  1024

(load-from-library "when.scm")

(define-syntax (while p . body)
  (let ((loop (gensym)))
    `(letrec ((,loop (lambda ()
                       (when ,p ,@body (,loop)))))
       (,loop))))

(define-syntax (until p . body)
  (let ((loop (gensym)))
    `(letrec ((,loop (lambda ()
                       (unless ,p ,@body (,loop)))))
       (,loop))))
