; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (dirname string)  ==>  string
;
; Return the directory part of the path name in STRING. Note that
; DIRNAME performs merely a syntaxical operation without any regard
; to the actual file system structure, i.e.
;
;       (dirname "/foo/bar")  ==>  "/foo"
;
; even if "bar" is also a directory. DIRNAME also takes care of
; trailing slashes and recognizes some special cases (see examples).
;
; Example:   (dirname "/foo/bar/baz")  ==>  "/foo/bar"
;            (dirname "foo/bar")       ==>  "foo"
;            (dirname "foo/bar/")      ==>  "foo"
;            (dirname "/foo")          ==>  "/"
;            (dirname "/")             ==>  "/"
;            (dirname "foo")           ==>  "."

(load-from-library "string-split.scm")
(load-from-library "string-unsplit.scm")

(define (dirname path)
  (letrec
    ((cut
      (lambda (s)
        (let loop ((s (reverse! s)))
          (cond ((null? s)
                  '(""))
                ((string=? "" (car s))
                  (loop (cdr s)))
                (else
                  (reverse! (cdr s))))))))
    (let* ((dirs (string-split #\/ path)))
      (if (null? (cdr dirs))
          "."
          (let ((dir (string-unsplit #\/ (cut dirs))))
            (if (string=? "" dir)
                "/"
                dir))))))
