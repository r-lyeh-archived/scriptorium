; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (inet-server string1 string2 procedure^5)          ==>  undefined
; (inet-server string1 string2 procedure^5 integer)  ==>  undefined
;
; Create a server socket, bind it to host STRING1, port STRING2 and
; listen for requests on that socket. When a request comes in, spawn
; a child process to handle the request. PROCEDURE^5 will be called
; by the child process. When the procedure returns, the child will
; close the connection and terminate.
;
; The following arguments are passed to PROCEDURE^5:
;
;       IN     an input-port for reading data from the connection
;       OUT    an output-port for sending data over the connection
;       PEER   a list of the form (IP PORT) where IP is the remote
;              IP address and PORT is the remote-side port of the
;              connection
;       TIME   the time of establishing the connection (Unix timestamp)
;       DELTA  the time span in seconds in which the most recent NREQ
;              requests of *this* client were received; #F if the client
;              did not yet send NREQ resuests; see below for details
;
; When an INTEGER argument (NREQ) is passed to INET-SERVER, it will keep
; track of the NREQ most recent requests received from each IP address.
; When a client sends more than NREQ-1 requests, INET-SERVER will send
; the difference between the time of the most recent request and the
; time of the oldest tracked request to PROCEDURE^5 in the DELTA argument. 
; The request handler can use this value, for instance, to limit the
; number of requests per unit of time that it accepts from each client.
;
; (Example): (load-from-library "read-line.scm")
;
;            (define (handle-echo-request in out . ignore)
;              (display (read-line in) out)
;              (newline out))
;
;            (inet-server "localhost" "12345" handle-echo-request)

(require-extension sys-unix network)

(load-from-library "hash-table.scm")
(load-from-library "remove.scm")

(define (inet-server host port request-handler . nreq)
  (let ((hosts   (make-hash-table))
        (n-track (if (null? nreq)
                     5
                     (car nreq))))
    (letrec
      ((cut-last!
         (lambda (x)
           (cond ((null? x))
                 ((null? (cdr x)))
                 ((null? (cddr x))
                   (set-cdr! x '()))
                 (else
                   (cut-last! (cdr x))))))
       (collect-zombies
         (lambda (procs)
           (remp sys:waitpid procs))))
      (let ((s (sys:inet-listen host port 5)))
        (let connect-loop ((procs '()))
          (let* ((conn (sys:inet-accept s))
                 (peer (sys:inet-getpeername conn))
                 (time (sys:time))
                 (hist (hash-table-ref hosts (and peer (car peer))))
                 (hist (if hist
                           (car hist)
                           '())))
            (let* ((hist  (if (< (length hist) n-track)
                              (cons time hist)
                              (begin (cut-last! hist)
                                     (cons time hist))))
                   (delta (if (>= (length hist) n-track)
                              (- (car hist) (car (reverse hist)))
                              #f)))
              (if peer
                  (hash-table-set! hosts (car peer) hist))
              (if (> (hash-table-length hosts) 1000)
                     (set! hosts (make-hash-table)))
              (set! procs (collect-zombies procs))
              (let ((pid (sys:fork)))
                (if (zero? pid)
                    (let ((in  (sys:make-input-port conn))
                          (out (sys:make-output-port conn)))
                      (request-handler in out peer time delta)
                      (close-output-port out)
                      (close-input-port in)
                      (sys:exit 0))
                    (begin (sys:close conn)
                           (connect-loop (cons pid procs))))))))))))
