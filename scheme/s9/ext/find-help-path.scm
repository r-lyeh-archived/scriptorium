; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (find-help-path)  ==>  string | #f
;
; Returns the directory in which the online help pages are stored
; of #F when the pages cannot be located.
;
; (Example): (find-help-path)  ==>  "/usr/local/share/s9fes/help"

(require-extension sys-unix)

(load-from-library "string-split.scm")

(define (find-help-path)
  (let loop ((dirs (string-split #\: *library-path*)))
    (if (null? dirs)
        #f
        (let ((path (string-append (car dirs) "/help")))
          (if (and (file-exists? path)
                   (sys:stat-directory? path))
              path
              (loop (cdr dirs)))))))
