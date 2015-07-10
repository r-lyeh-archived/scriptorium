; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009,2010
; Placed in the Public Domain
;
; (time->unix-time time-list)  ==>  integer | #f
;
; TIME->UNIX-TIME converts TIME-LIST (see UNIX-TIME->TIME) to the
; number of seconds since the Unix epoch. It returns #F if TIME-LIST
; is not a proper time list.
;
; Example:   (time->unix-time '(6 2010 4 25 7 1 19))  ==>  1272178879

(load-from-library "leap-yearp.scm")
(load-from-library "proper-timep.scm")

(define (time->unix-time t)
  (let* ((leap-year? leap-year?)
         (proper-time? proper-time?)
         (days/mon  (vector 31 28 31 30 31 30 31 31 30 31 30 31))
         (sec/hour  (* 60 60))
         (sec/day   (* 24 sec/hour))
         (sec/year  (* 365 sec/day))
         (leap-years-until
           (lambda (x)
             (+ (quotient x 4)
                (- (quotient x 100))
                (quotient x 400)))))
    (if (and (list? t)
             (= (length t) 7)
             (number? (cadr t))
             (leap-year? (cadr t)))
        (vector-set! days/mon 1 29))
    (and (proper-time? t)
         (let ((leap-days (- (leap-years-until (- (list-ref t 1) 1))
                             (leap-years-until 1970))))
           (+ (* sec/year (- (list-ref t 1) 1970))
              (* sec/day leap-days)
              (let loop ((d 0)
                         (m 1))
                (if (< m (list-ref t 2))
                    (loop (+ d (vector-ref days/mon (- m 1)))
                          (+ 1 m))
                    (* d sec/day)))
              (* sec/day (- (list-ref t 3) 1))
              (* sec/hour (list-ref t 4))
              (* 60 (list-ref t 5))
              (list-ref t 6))))))
