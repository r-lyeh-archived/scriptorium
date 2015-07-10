; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (mode->string integer)  ==>  string
;
; Return a symbolic representation of the access mode specified in
; INTEGER. The resulting string is in the same format as used by
; the ls(1) Unix command.
;
; Example:   (mode->string #o5751)  ==>  "rwsr-x--t"

(require-extension sys-unix)

(load-from-library "bitops.scm")

(define (mode->string mode)
  (letrec
    ((bit*?
       (lambda (a b)
         (not (zero? (bit* a b)))))
     (flag
       (lambda (f y n)
         (if (bit*? mode f)
             y
             n))))
    (string-append (flag sys:s-irusr "r" "-")
                   (flag sys:s-iwusr "w" "-")
                   (if (bit*? mode sys:s-isuid)
                       (flag sys:s-ixusr "s" "S")
                       (flag sys:s-ixusr "x" "-"))
                   (flag sys:s-irgrp "r" "-")
                   (flag sys:s-iwgrp "w" "-")
                   (if (bit*? mode sys:s-isgid)
                       (flag sys:s-ixgrp "s" "S")
                       (flag sys:s-ixgrp "x" "-"))
                   (flag sys:s-iroth "r" "-")
                   (flag sys:s-iwoth "w" "-")
                   (if (bit*? mode sys:s-isvtx)
                       (flag sys:s-ixoth "t" "T")
                       (flag sys:s-ixoth "x" "-")))))
