; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (spawn-shell-command string)     ==>  list
; (spawn-shell-command/fd string)  ==>  list
;
; (load-from-library "spawn-shell-command.scm")
;
; Spawn a child process running the shell command STRING. Return
; a list of two I/O-ports and the PID of the child process:
;
;         (input-port output-port integer)
;
; The input-port can be used to read the output of the command and
; the output-port to send input to the command. The command will be
; executed by running the equivalent of:
;
;         execl("/bin/sh", "/bin/sh", "-c", STRING, NULL);
;
; Error output of the child will be redirected to its standard output.
;
; SPAWN-SHELL-COMMAND/FD is like SPAWN-SHELL-COMMAND, but delivers raw
; Unix file descriptors instead of input/output ports.
;
; (Example): (spawn-shell-command "ls -l /bin")
;              ==>  (#<input-port> #<output-port> 707)

(require-extension sys-unix)

(define (spawn-shell-command/fd command)
  (let* ((from-child (sys:pipe))
         (to-child   (sys:pipe))
         (pid        (sys:fork)))
    (cond ((zero? pid)
            (sys:close (car from-child))
            (sys:close (cadr to-child))
            (sys:dup2 (cadr from-child) 2)
            (sys:dup2 (cadr from-child) 1)
            (sys:dup2 (car to-child) 0)
            (sys:execv "/bin/sh" (list "-c" command))
            (error "sys:execv should not return")
            (sys:exit 1))
          (else
            (sys:close (cadr from-child))
            (sys:close (car to-child))
            (list (car from-child)
                  (cadr to-child)
                  pid)))))

(define (spawn-shell-command command)
  (let ((conn (spawn-shell-command/fd command)))
    (list (sys:make-input-port (car conn))
          (sys:make-output-port (cadr conn))
          (caddr conn))))
