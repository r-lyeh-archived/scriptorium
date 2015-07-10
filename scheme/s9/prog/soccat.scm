#! /usr/local/bin/s9 -f

; soccat -- connect to remote hosts
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; Usage: soccat [-i] host port/service
;
; Read a request from default input and send it to the specific
; remote host. Simultaneously pass input from the remote host to
; the default output. In interactive mode reconnect automatically
; when the remote side breaks the connection.
;
; Options:
;
; -i  interactive mode (reconnect automatically)

(load-from-library "read-line.scm")
(load-from-library "displaystar.scm")
(load-from-library "flush-output-port.scm")
(load-from-library "parse-optionsb.scm")

(define (soccat reconnect host port)
  (let* ((s   (sys:inet-connect host port))
         (in  (sys:make-input-port s))
         (out (sys:make-output-port s)))
    (let ((pid (sys:fork)))
      (if (not (zero? pid))
          (let out-loop ()
            (if (sys:waitpid pid)
                (begin (close-input-port in)
                       (close-output-port out)
                       (if reconnect
                           (soccat reconnect host port)
                           (sys:exit))))
            (if (sys:select '(0 100000) '(0) '())
                (let ((line (read-line)))
                  (if (eof-object? line)
                      (begin (sys:wait)
                             (sys:exit))
                      (begin (display* out line #\newline)
                             (flush-output-port out)))))
            (out-loop))
          (let in-loop ((line (read-line in)))
            (if (eof-object? line)
                (sys:exit)
                (begin (display* line #\newline)
                       (flush-output-port)
                       (in-loop (read-line in)))))))))

(define show-help        (option #\h #f))
(define interactive-mode (option #\i #f))
(define options          `(,show-help
                           ,interactive-mode))

(define (usage)
  (display* "Usage: soccat [-i] host port" #\newline))

(let ((args (parse-options! (sys:command-line) options usage)))
  (if (opt-val show-help)
      (begin (display-usage
               `(""
                 ,usage
                 ""
                 "Connect to remote hosts"
                 ""
                 "-i  interactive mode (reconnect automatically)"
                 ""))
             (sys:exit)))
  (if (not (= 2 (length args)))
      (begin (usage)
             (sys:exit 1)))
  (if (opt-val interactive-mode)
      (display* "Interactive mode, send INTR to exit" #\newline))
  (apply soccat (opt-val interactive-mode) args))
