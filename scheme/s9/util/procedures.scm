; Ad-hoc procedure list generator
; To be run in help/

(display
  (sort
    string<=?
    (list->set
      (apply
        append
        (map (lambda (x)
               (with-input-from-file x
                 (lambda ()
                   (let loop ((ln  (read-line))
                              (fns '()))
                     (if (or (eof-object? ln)
                             (string=? "" ln))
                         (reverse! fns)
                         (let ((ln (and-let* ((ln (string-find "(" ln)) ;)
                                              (p  (string-position " " ln))
                                              (ln (substring ln 1 p))
                                              (k  (- (string-length ln) 1))
                                              (ln (if (char=?
                                                        #\) ;(
                                                        (string-ref ln k))
                                                      (substring ln 0 k)
                                                      ln)))
                                     ln)))
                           (if ln
                               (loop (read-line) (cons ln fns))
                               (loop (read-line) fns))))))))
             (sys:readdir "."))))))
