; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2015
; Placed in the Public Domain
;
; (mode list)  ==>  list
;
; (load-from-library "mode.scm")
;
; Return the mode(s) of a list of values. The mode of a set of samples
; is the value that occurs most frequently. A set may be multi-modal,
; i.e. have multiples modes.
;
; Example:   (mode '(1 2 3 3 4 5 5 6))  ==>  (3 5)

(load-from-library "collect")
(load-from-library "mergesort")

(define (mode set)
  (let* ((gs (collect = (mergesort <= set)))
         (ks (map length gs))
         (m  (apply max ks)))
    (let modes ((gs gs)
                (ks ks)
                (ms '()))
      (cond ((null? gs)
              (reverse! ms))
            ((= m (car ks))
              (modes (cdr gs) (cdr ks) (cons (caar gs) ms)))
            (else
              (modes (cdr gs) (cdr ks) ms))))))
