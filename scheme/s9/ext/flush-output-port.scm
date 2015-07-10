; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (flush-output-port output-port)  ==>  unspecific
; (flush-output-port)              ==>  unspecific
;
; Flush the given output port by writing all pending output to
; the associated file descriptor. When no port is specified,
; flush (current-output-port).
;
; (Example): (flush-output-port)  ==>  unspecific

(require-extension sys-unix)

(define (flush-output-port . port)
  (if (null? port)
      (sys:flush (current-output-port))
      (sys:flush (car port))))
