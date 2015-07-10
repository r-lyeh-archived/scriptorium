; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009,2012
; Placed in the Public Domain
;
; (exists procedure list ...)  ==>  boolean
;
; Test whether a given property exists in a sequence of N lists.
; The property is expressed using the N-ary predicate P, which
; is given in the procedure argument. P is first applied to a
; list consisting of the first member of each given list. If P
; returns truth, EXISTS returns #T immediately. Otherwise it is
; applied to a list consisting of the second members of the given
; lists, etc. If P returns falsity for all sets of members, EXISTS
; returns #F.
;
; Example:   (exists < '(9 1) '(8 2) '(7 3))  ==>  #t
;            ; because (< 1 2 3)

(define (exists p . a*)
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
     (exists*
       (lambda (a*)
         (and (not (any-null a*))
              (or (apply p (car-of a*))
                  (exists* (cdr-of a*)))))))
    (exists* a*)))
