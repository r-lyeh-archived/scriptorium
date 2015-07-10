; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (unix-time->time integer)  ==>  time-list
;
; UNIX-TIME->TIME converts the number of seconds since the Unix
; epoch specified in INTEGER to a time list, which has the following
; members:
;
;         (weekday    ; 0..6, where 0 = Monday
;          year       ; 1970..2038
;          month      ; 1..12
;          day        ; 1..31, depends on month
;          hour       ; 0..23
;          minute     ; 0..59
;          second)    ; 0..59
;
; Example:   (unix-time->time 1272178879)  ==>  (6 2010 4 25 7 1 19)

(load-from-library "leap-yearp.scm")

(define (unix-time->time n)
  (let* ((leap-year? leap-year?)
         (days/mon  (vector 31 28 31 30 31 30 31 31 30 31 30 31))
         (sec/hour  (* 60 60))
         (sec/day   (* 24 sec/hour))
         (wday      (remainder (+ 3 (quotient n sec/day)) 7))
         (year+rest (let loop ((t n)
                               (y 1970))
                      (let ((s (* sec/day (if (leap-year? y) 366 365))))
                        (if (> s t)
                            (list y t)
                            (loop (- t s) (+ 1 y))))))
         (year      (car year+rest))
         (n         (cadr year+rest))
         (mon+rest  (begin
                      (if (leap-year? year)
                          (vector-set! days/mon 1 29))
                      (let loop ((t n)
                                 (m 0))
                        (let ((nt (- t (* sec/day
                                          (vector-ref days/mon m)))))
                          (if (negative? nt)
                              (list m t)
                              (loop nt (+ 1 m)))))))
         (month     (+ 1 (car mon+rest)))
         (n         (cadr mon+rest))
         (day       (+ 1 (quotient n sec/day)))
         (n         (remainder n sec/day))
         (hour      (quotient n sec/hour))
         (n         (remainder n sec/hour))
         (min       (quotient n 60))
         (sec       (remainder n 60)))
    (list wday year month day hour min sec)))
