; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (for-all procedure list ...)  ==>  object
;
; Test whether all members of a sequence of N lists have a given
; property. The property is expressed using an N-ary predicate P,
; which is given in the procedure argument. P is applied to a list
; consisting of the first member of each given list. If P returns
; truth, it is applied to a list consisting of the second member of
; each given list, etc. If P returns falsity for any set of members,
; FOR-ALL returns #F. If only one set of members is left to check,
; FOR-ALL returns the value of P applied to this final set.
; When the LISTs are empty, FOR-ALL returns #T.
;
; Example:   (for-all < '(1 7) '(2 8) '(3 9))  ==>  #t
;            ; because (< 1 2 3) and (< 7 8 9)

(define (for-all p . a*)
  (letrec
    ((car-of
       (lambda (a)
         (map car a)))
     (cdr-of
       (lambda (a)
         (map cdr a)))
     (any-null
       (lambda (a)
         (memq '() a)))
     (for-all*
       (lambda (a*)
         (cond ((any-null a*) #t)
               ((any-null (cdr-of a*))
                 (apply p (car-of a*)))
               (else
                 (and (apply p (car-of a*))
                      (for-all* (cdr-of a*))))))))
    (for-all* a*)))
