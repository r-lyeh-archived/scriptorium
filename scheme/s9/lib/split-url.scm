; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (split-url string)  ==>  list
; (url-anchor list)   ==>  string
; (url-args list)     ==>  alist
; (url-host list)     ==>  string
; (url-path list)     ==>  string
; (url-proto list)    ==>  string
; (url-suffix list)   ==>  string
;
; (load-from-library "split-url.scm")
;
; Extract the individual parts of an URL string and store them
; in separate elements of the resulting list. The list has the
; general form
;
;       (protocol host path suffix arguments anchor)
;
; Parts that could not be extracted are set to #F.
;
; PROTOCOL is the protocol without the :// part, e.g.: "http"
; HOST is the host name part of the path (if a protocol is given).
; PATH is the local path including the file suffix, e.g.: "foo/bar.html"
; SUFFIX is an extra copy of the file suffix, e.g.: "html"
; ARGUMENTS is a list of key/value pairs as typically received
; in the '?' part of an URL, e.g.: ("page" . "1")
; ANCHOR is an anchor without the '#' character.
;
; The URL-PROTO, URL-HOST, URL-PATH, URL-SUFFIX, URL-ARGS, and
; URL-ANCHOR procedures extract the individual parts of the
; resulting list.
;
; Example:   (split-url "ftp://example.org/foo.bar?a=1&b=2")
;                                    ==>  ("ftp"
;                                          "example.org"
;                                          "/foo.bar"
;                                          "bar"
;                                          (("a" . "1")
;                                           ("b" . "2"))
;                                          #f)

(load-from-library "string-position.scm")
(load-from-library "string-split.scm")
(load-from-library "string-unsplit.scm")
(load-from-library "hof.scm")

(define (split-url s)
  (let* ((next   (cond ((string-position "//" s)
                         => (lambda (i)
                              (list (substring s 0 (- i 1))
                                    (substring s
                                               (+ 2 i)
                                               (string-length s)))))
                       (else
                         (list #f s))))
         (proto  (car next))
         (next   (let ((s* (string-split #\# (cadr next))))
                   (if (null? (cdr s*))
                       (list #f (car s*))
                       (list (cadr s*) (car s*)))))
         (anchor (car next))
         (next   (let ((s* (string-split #\? (cadr next))))
                   (if (null? (cdr s*))
                       (list #f (car s*))
                       (list (cadr s*) (car s*)))))
         (args   (car next))
         (args   (and args
                      (string-split #\& args)))
         (args   (and args
                      (map (lambda (x)
                             (let ((x (string-split #\= x)))
                               (if (null? (cdr x))
                                   x
                                   (cons (car x) (cadr x)))))
                           args)))
         (next   (let ((s* (string-split #\. (cadr next))))
                   (if (null? (cdr s*))
                       (list #f (cadr next))
                       (list (car (reverse! s*)) (cadr next)))))
         (suffix (car next))
         (next   (string-split #\/ (string-unsplit #\. (cdr next))))
         (path   (if proto
                     (if (cdr next)
                         (string-append "/" (string-unsplit #\/ (cdr next)))
                         "/")
                     (string-unsplit #\/ next)))
         (host   (if proto
                     (car next)
                     #f)))
     (list proto host path suffix args anchor)))

(define url-proto  car)
(define url-host   cadr)
(define url-path   caddr)
(define url-suffix cadddr)
(define url-args   (compose car cddddr))
(define url-anchor (compose cadr cddddr))
