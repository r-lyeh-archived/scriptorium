; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (search-path string1 string2)  ==>  string | #f
;
; Search the Unix search path STRING2 for the executable STRING1. Return
; the full path of the first executable found or #F if not executable
; named STRING1 can be found in the given path.
; STRING2 is a colon-separated list of paths, e.g.:
;
;         "/bin:/usr/bin:/usr/local/bin"
;
; SEARCH-PATH uses ACCESS with mode ACCESS-X-OK to check whether a
; file is executable.
;
; (Example): (search-path "vi" "/bin:/usr/bin")  ==>  "/usr/bin/vi"

(require-extension sys-unix)

(load-from-library "string-split.scm")

(define (search-path file path)
  (let loop ((path (string-split #\: path)))
    (cond ((null? path)
            #f)
          ((let ((loc (string-append (car path) "/" file)))
             (and (sys:access loc sys:access-x-ok)
                  loc))
            => (lambda (x) x))
          (else
            (loop (cdr path))))))
