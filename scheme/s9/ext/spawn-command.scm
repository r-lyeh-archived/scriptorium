; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2014
; Placed in the Public Domain
;
; (spawn-command string list)     ==>  list
; (spawn-command/fd string list)  ==>  list
;
; (load-from-library "spawn-command.scm")
;
; Spawn a child process running the command STRING with the arguments
; listed in LIST. Return a list of two I/O-ports and the PID of the
; child process:
;
;         (input-port output-port integer)
;
; Note that the full path of the command must be spcified and no
; shell operators like <, >, &, etc may be used. When PATH search
; or shell operators are needed, use SPAWN-SHELL-COMMAND instead.
;
; The input-port can be used to read the output of the command and
; the output-port to send input to the command. Error output of the
; child will be redirected to its standard output.
;
; SPAWN-COMMAND/FD is like SPAWN-COMMAND, but delivers raw Unix file
; descriptors instead of input/output ports.
;
; (Example): (spawn-command "/bin/ls" '(" -l" "/bin"))
;              ==>  (#<input-port> #<output-port> 707)

(require-extension sys-unix)

(define (spawn-command/fd command args)
  (let* ((from-child (sys:pipe))
         (to-child   (sys:pipe))
         (pid        (sys:fork)))
    (cond ((zero? pid)
            (sys:close (car from-child))
            (sys:close (cadr to-child))
            (sys:dup2 (cadr from-child) 2)
            (sys:dup2 (cadr from-child) 1)
            (sys:dup2 (car to-child) 0)
            (sys:execv command args)
            (error "sys:execv should not return")
            (sys:exit 1))
          (else
            (sys:close (cadr from-child))
            (sys:close (car to-child))
            (list (car from-child)
                  (cadr to-child)
                  pid)))))

(define (spawn-command command args)
  (let ((conn (spawn-command/fd command args)))
    (list (sys:make-input-port (car conn))
          (sys:make-output-port (cadr conn))
          (caddr conn))))
