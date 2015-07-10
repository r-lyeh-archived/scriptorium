; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (append-to-output-file string)  ==>  output-port
;
; Take a string naming an output file and return an output port
; capable of writing characters to the that file. Characters
; written to the port will be appended to the file. When the
; file does not exist when this procedure is called, it will
; be created. If the file cannot be opened, an error is signalled.
;
; (Example): (append-to-output-file "logfile")  ==>  #<output-port>

(require-extension sys-unix)

(define (append-to-output-file path)
  (let ((fd (if (file-exists? path)
                (sys:open path sys:write-only)
                (sys:creat path #o644))))
    (sys:lseek fd 0 sys:seek-end)
    (sys:make-output-port fd)))
