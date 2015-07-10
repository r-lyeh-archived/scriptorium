; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (subset? list1 list2 ...) ==>  list
;
; (load-from-library "subsetp.scm")
;
; Check whether each of a sequence of sets is a subset of the subsequent
; set in the list. The test succeeds even if a set is an improper subset
; of (i.e. the same sets) the subsequent one.
;
; Example:   (subset? '(a) '(a b) '(a b) '(a b c d))  ==>  #t
;            (subset? '(a b c))                       ==>  #t

(define (subset? a . a*)
  (letrec
    ((subset2 (lambda (a b)
      (cond ((null? a)
              b)
            ((member (car a) b)
              (subset2 (cdr a) b))
            (else
              #f)))))
    (if (null? a*)
        #t
        (and (fold-left (lambda (a b)
                          (and a b (subset2 a b)))
                        a
                        a*)
             #t))))
