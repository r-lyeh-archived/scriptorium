; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (display* object ...)              ==>  unspecific
; (display* output-port object ...)  ==>  unspecific
;
; (load-from-library "displaystar.scm")
;
; Display each OBJECT in the same way as DISPLAY. When the first
; argument of DISPLAY* is an output port, write all output to
; that port.
;
; (Example): (display* "foo" #\= 17 #\newline)  ==>  unspecific
;            ; output: foo = 17

(define (display* . x)
  (if (not (null? x))
      (let* ((args  (if (output-port? (car x))
                        (list (car x) (cdr x))
                        (list (current-output-port) x)))
             (port  (car args))
             (args  (cadr args)))
          (for-each (lambda (x)
                      (display x port))
                    args))))
