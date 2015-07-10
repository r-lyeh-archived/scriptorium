#! /usr/local/bin/s9 -f

; s9hts -- hyper text server
; by Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; Usage: s9hts [-m sec] [host [port]]

(load-from-library "inet-server.scm")
(load-from-library "url-decode.scm")
(load-from-library "split-url.scm")
(load-from-library "hash-table.scm")
(load-from-library "string-position.scm")
(load-from-library "string-parse.scm")
(load-from-library "string-unsplit.scm")
(load-from-library "string-find.scm")
(load-from-library "read-line.scm")
(load-from-library "displaystar.scm")
(load-from-library "parse-optionsb.scm")
(load-from-library "spawn-command.scm")
(load-from-library "format-time.scm")
(load-from-library "unix-time-to-time.scm")

(define show-help (option #\h #f))
(define max-req   (option #\m 'integer 5))
(define options   `(,max-req
                    ,show-help))

(define (usage)
  (display "Usage: s9hts [-m nreq] [host [port]]");
  (newline))

(define CR   (integer->char 13))
(define CRLF (string CR #\newline))

(define (status out n . type)
  (let ((msg (case n ((200) "ok")
                     ((400) "huh?")
                     ((404) "not here")
                     ((500) "oops - I blew it")
                     ((503) "bug me not"))))
    (display* out "HTTP/1.0 " n #\space msg CRLF)
    (if (not (null? type))
        (display* out "Content-type: " (car type) CRLF CRLF))))

(define (last x)
  (car (reverse x)))

(define (trim-empty a)
  (letrec
    ((trim
       (lambda (x)
         (if (and (not (null? x))
                  (equal? "" (car x)))
             (trim (cdr x))
             x))))
    (trim (reverse! (trim (reverse a))))))

(define (cant-parse out request)
  (status out 400 "text/html")
  (display* out "<H1>400 huh?</H1>" #\newline)
  (display* out "<P>Request: " request "</P>" #\newline))

(define (not-found out file)
  (status out 404 "text/html")
  (display* out "<H1>404 not here</H1>" #\newline)
  (display* out "<P>Path: " file "</P>" #\newline))

(define (bug-me-not out)
  (status out 503 "text/html")
  (display* out "<H1>503 bug me not!</H1>" #\newline))

(define (set-url-path! url path)
  (set-car! (cddr url) path))

(define (set-url-suffix! url suffix)
  (set-car! (cdddr url) suffix))

(define (local-path? s)
  (let loop ((in    (string->list s))
             (depth 0))
    (cond ((null? in))
          ((char=? #\/ (car in))
            (if (and (> (length in) 3)
                     (string=? "/.." (string (car in) (cadr in) (caddr in))))
                (if (zero? depth)
                    #f
                    (loop (cdr in) (- depth 1)))
                (loop (cdr in) (+ 1 depth))))
          (else
            (loop (cdr in) depth)))))

(define (mime-type suffix)
  (let ((table '(((#f)           "text/plain")
                 (("html" "htm") "text/html")
                 (("css")        "text/css")
                 (("jpg" "jpeg") "image/jpeg")
                 (("png")        "image/png")
                 (("gif")        "image/gif")
                 (("tar")        "application/x-tar")
                 (("gz" "tgz")   "application/x-gzip")
                 (("zip")        "application/zip"))))
    (let loop ((table table))
      (cond ((null? table)
              "application/octet-stream")
            ((member suffix (caar table))
              (cadar table))
            (else
              (loop (cdr table)))))))

(define (run-script out path args)
  (let* ((conn       (spawn-command path))
         (script-in  (car conn))
         (script-out (cadr conn))
         (args       (map (lambda (x)
                            (cons (car x)
                                  (url-decode (cdr x))))
                          args)))
    (if args
        (begin (display args script-out)
               (newline script-out)
               (close-output-port script-out)))
    (let loop ((c (read-char script-in)))
      (if (not (eof-object? c))
          (begin (write-char c out)
                 (loop (read-char script-in)))))))

(define (serve out url)
  (cond ((and (url-suffix url)
              (string=? "cgi" (url-suffix url)))
          (status out 200)
          (run-script out (url-path url) (url-args url)))
        (else
          (status out 200 (mime-type (url-suffix url)))
          (with-input-from-file
            (url-path url)
            (lambda ()
              (let loop ((c (read-char)))
                (if (not (eof-object? c))
                    (begin (write-char c out)
                           (loop (read-char))))))))))

(define (log-stuff code time peer header)
  (let ((tstp (format-time "~w ~4y-~m-~2d ~2h:~2m:~2s"
                           (unix-time->time time))))
    (display* #\" code #\"
              #\space
              #\" tstp #\"
              #\space
              #\" (if peer (car peer) "unknown") #\"
              #\space
              #\" (cadr (string-split #\space (car header))) #\"
              #\space
              #\" (get-user-agent header) #\"
              #\newline)))

(define (http-request out old time peer header)
  (let ((request (string-parse " " (car header))))
    (cond ((not (<= 2 (length request) 3))
            (log-stuff 400 time peer header)
            (cant-parse out (string-unsplit #\space request)))
          ((not (string=? "GET" (car request)))
            (log-stuff 400 time peer header)
            (cant-parse out (string-unsplit #\space request)))
          (else
            (let* ((url (split-url (cadr request)))
                   (_   (set-url-path! url
                                       (string-append "." (url-path url))))
                   (url (begin 
                          (if (and (sys:stat (url-path url))
                                   (sys:stat-directory? (url-path url)))
                              (begin (set-url-path!
                                       url
                                       (string-append (url-path url)
                                                      "/index.html"))
                                     (set-url-suffix! url "html")))
                          url)))
              (cond ((not (local-path? (url-path url)))
                      (log-stuff 503 time peer header)
                      (bug-me-not out))
                    ((file-exists? (url-path url))
                      (log-stuff 200 time peer header)
                      (serve out url))
                    (else
                      (log-stuff 404 time peer header)
                      (not-found out (cadr request)))))))))

(define (get-user-agent header)
  (let* ((uah "User-Agent: ")
         (k   (string-length uah)))
    (let loop ((h header))
      (cond ((null? h) "unidentified")
            ((and (>= (string-length (car h)) k)
                  (string-ci=? (substring (car h) 0 k) uah))
              (substring (car h) k (string-length (car h))))
            (else
              (loop (cdr h)))))))

(define (old-request? s)
  (let ((s (string-parse " " s)))
    (and (= (length s) 2)
         (>= (string-length (car s)) 3)
         (string=? "GET" (substring (car s) 0 3)))))

(define (handle-request in out peer time delta)
  (if (and delta (< delta 1))
      (bug-me-not out)
      (let loop ((line   (read-line in))
                 (header '()))
        (let* ((k    (if (eof-object? line)
                         -1
                         (- (string-length line) 1)))
               (line (if (and (not (negative? k))
                              (char=? CR (string-ref line k)))
                         (substring line 0 k)
                         line)))
          (cond ((or (eof-object? line)
                     (string=? "" line))
                  (http-request out
                                #f
                                time
                                peer
                                (reverse header)))
                ((old-request? line)
                  (handle-request out
                                  #t
                                  time
                                  peer
                                  (list line)))
                (else
                  (loop (read-line in)
                        (cons line header))))))))

(define (server)
  (let ((args (parse-options! (sys:command-line) options usage)))
    (if (opt-val show-help)
        (begin (display-usage
                 `(""
                   ,usage
                   ""
                   "Hypertext Server"
                   ""
                   "-m nreq   max. requests/second per client (default: 5)"
                   ""))
        (sys:exit 0)))
    (let ((host (if (null? args)
                    #t
                    (car args)))
          (port (if (or (null? args)
                        (null? (cdr args)))
                    "80"
                    (cadr args))))
      (inet-server host port handle-request (opt-val max-req)))))

(server)
