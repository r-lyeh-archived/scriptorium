; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (list->set list)  ==>  list
;
; (load-from-library "list-to-set.scm")
;
; Convert list to set. A set is a list containing unique members.
;
; Example:   (list->set '(a b c b c))  ==>  (a b c)

(define (list->set a)
  (letrec
    ((list->set2
       (lambda (a r)
         (cond ((null? a)
                 (reverse! r))
               ((member (car a) r)
                 (list->set2 (cdr a) r))
               (else
                 (list->set2 (cdr a)
                             (cons (car a) r)))))))
    (list->set2 a '())))
