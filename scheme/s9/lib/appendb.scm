; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (append! list ...)         ==>  unspecific
; (append! list ... object)  ==>  unspecific
;
; APPEND! is like APPEND, but appends the LISTs destructively.
; It destroys all but the last object passed to it and returns
; an unspecific value. The append!ed list will be stored in the
; first LIST, which should be bound to a symbol or it will not
; be accessible after the operation. The first LIST may not be
; '().
;
; Example:   (let ((a (list 1 2 3)))
;              (append! a (list 4 5 6) 'end)
;              a)                             ==>  (1 2 3 4 5 6 . end)

(define (append! . a*)
  (letrec
    ((append2!
       (lambda (a b)
         (cond ((null? a) (error "append!: cannot append to ()"))
               ((null? b) a)
               (else
                 (let find-nil ((a a))
                   (if (null? (cdr a))
                       (begin (set-cdr! a b)
                              a)
                       (find-nil (cdr a)))))))))
    (if (and (not (null? a*))
             (not (null? (cdr a*))))
        (begin (fold-left append2! (car a*) (cdr a*))
               (void)))))
