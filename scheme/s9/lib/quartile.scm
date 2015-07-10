; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2015
; Placed in the Public Domain
;
; (quartile list)  ==>  list
;
; (load-from-library "quartile.scm")
;
; Return a list (L M H), where L is the value that is greater than or
; equal to 25% of the values in LIST, M is >= 50% of the list, and H is
; >= 75% of the list.
;
; Example:   (quartile '(1 2 3 4 5 6 7 ))  ==>  (2 4 6)

(load-from-library "median.scm")

(define (quartile set)
  (define (floor* x) (inexact->exact (floor x)))
  (define (ceil* x)  (inexact->exact (ceiling x)))
  (let* ((vs set)
         (n  (/ (length set) 4))
         (q1 (if (integer? n)
                 (let ((n (inexact->exact n)))
                   (/ (+ (list-ref vs (- n 1)) (list-ref vs n)) 2))
                 (list-ref vs (floor* n))))
         (q2 (median set))
         (n  (* 3 n))
         (q3 (if (integer? n)
                 (let ((n (inexact->exact n)))
                   (/ (+ (list-ref vs (- n 1)) (list-ref vs n)) 2))
                 (list-ref vs (floor* n)))))
    (list q1 q2 q3)))
