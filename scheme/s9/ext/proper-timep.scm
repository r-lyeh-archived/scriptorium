; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (proper-time? time-list)  ==>  boolean
;
; Return #T if the given TIME-LIST is a proper TIME-LIST as defined
; by the UNIX-TIME->TIME procedure. Otherwise return #F.
;
; Example:   (proper-time? '(3 1970 1 1 0 0 0))  ==>  #t

(load-from-library "leap-yearp.scm")
(load-from-library "for-all.scm")

(define (proper-time? t)
  (let ((days/mon  (vector 31 28 31 30 31 30 31 31 30 31 30 31)))
    (if (and (list? t)
             (= (length t) 7)
             (number? (cadr t))
             (leap-year? (cadr t)))
        (vector-set! days/mon 1 29))
    (and (list? t)
         (= (length t) 7)
         (for-all number? t)
         (<=    0 (list-ref t 0)  6)
         (<=    1 (list-ref t 2) 12)
         (<=    1 (list-ref t 3) (vector-ref days/mon
                                             (- (list-ref t 2) 1)))
         (<=    0 (list-ref t 4) 23)
         (<=    0 (list-ref t 5) 59)
         (<=    0 (list-ref t 6) 59))))
