; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (time-add list1 list2)         ==>  list
; (time-subtract list1 list2)    ==>  list
; (time-difference list1 list2)  ==>  list
; (time-before? list1 list2)     ==>  boolean
; (time-after? list1 list2)      ==>  boolean
;
; (load-from-library "time-ops.scm")
;
; TIME-ADD adds the <interval> LIST2 to the <time> LIST1 and returns
; a new time that is in the future of LIST1, where
;
;       <time>      :=  (weekday year month day hour minute second)
;       <interval>  :=  (days hours minutes seconds)
;
; The WEEKDAY part is ignored in input, but set correctly in output.
; See the description of the UNIX-TIME->TIME procedure for further
; details.
;
; TIME-SUBTRACT subtracts the interval LIST2 from the time LIST1 and
; returns a new time that is in the past of LIST1. Both TIME-ADD and
; TIME-SUBTRACT return a value of the <time> form.
;
; TIME-DIFFERENCE returns an <interval> specifying the difference
; between the times LIST1 and LIST2. The order of the times does
; not matter, the difference is always positive.
;
; TIME-BEFORE? returns a boolean indicating whether the time LIST1
; is in the past of LIST2. TIME-AFTER? is the inverse operation of
; TIME-BEFORE?.
;
; Example:   (time-add '(0 2010 10 06 12 30 00) '(10 7 30 0))
;              ==>  (5 2010 10 16 20 0 0)
;            (time-difference '(0 2010 10 06 12 30 00)
;                             '(5 2010 10 16 20 00 00))
;              ==>  (10 7 30 0)
;            (time-after? '(5 2010 10 16 20 00 00)
;                         '(0 2010 10 06 12 30 00))
;              ==>  #t

(load-from-library "unix-time-to-time.scm")
(load-from-library "time-to-unix-time.scm")

(define (interval->seconds interval)
  (+ (* 86400 (car interval))
     (*  3600 (cadr interval))
     (*    60 (caddr interval))
              (cadddr interval)))

(define (seconds->interval s)
  (let* ((d (quotient s 86400))
         (s (remainder s 86400))
         (h (quotient s 3600))
         (s (remainder s 3600))
         (m (quotient s 60))
         (s (remainder s 60)))
    (list d h m s)))

(define (time-change op)
  (let ((interval->seconds interval->seconds))
    (lambda (time interval)
      (unix-time->time
        (op (time->unix-time time)
            (interval->seconds interval))))))

(define time-add (time-change +))
(define time-subtract (time-change -))

(define time-difference
  (let ((seconds->interval seconds->interval))
    (lambda (time0 time1)
      (seconds->interval
        (abs (- (time->unix-time time0)
                (time->unix-time time1)))))))

(define (time-before? time0 time1)
  (< (time->unix-time time0)
     (time->unix-time time1)))

(define (time-after? time0 time1)
  (time-before? time1 time0))
