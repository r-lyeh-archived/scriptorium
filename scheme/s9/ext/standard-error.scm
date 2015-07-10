; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (standard-error-port)                ==>  #<output-port>
; (call-with-stderr procedure^1)       ==>  object
; (with-output-to-stderr procedure^0)  ==>  unspecific
;
; (load-from-library "standard-error.scm")
;
; STANDARD-ERROR returns an output-file for accessing the Unix
; standard error file descriptor (stderr). CALL-WITH-STDERR passes
; that output-file to a unary procedure, which may use it for writing
; data to stderr. WITH-OUTPUT-TO-STDERR temporarily sets the current
; output port) to the stderr descriptor while running the given nullary
; procedure. CALL-WITH-STDERR returns the value returned by PROCEDURE^1.
;
; (Example): (call-with-stderr
;              (lambda (stderr)
;                (display "Something went wrong!" stderr)
;                (newline stderr)))                       ==> unspecific

(require-extension sys-unix)

(define standard-error-port
  (let ((stderr #f))
    (lambda ()
      (if (not stderr)
          (set! stderr (sys:make-output-port 2)))
      stderr)))

(define call-with-stderr
  (let ((standard-error-port standard-error-port))
    (lambda (proc1)
      (let ((r (proc1 (standard-error-port))))
        (sys:flush (standard-error-port))
        r))))

(define with-output-to-stderr
  (let ((set-output-port!    set-output-port!)
        (standard-error-port standard-error-port))
    (lambda (thunk)
      (let ((outer-port (current-output-port)))
        (set-output-port! (standard-error-port))
        (thunk)
        (sys:flush (standard-error-port))
        (set-output-port! outer-port)))))
