; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (format-time string time-list)  ==>  string | #f
;
; Format the time specification TIME-LIST (as returned by the
; UNIX-TIME->TIME procedure) according to the description in
; STRING. This a poor man's CommonLISP FORMAT-style procedure
; intended for making time lists more readable. It returns #F
; if TIME-LIST is not a proper time list or string is erroneous
; (i.e.: contains a wrong format descriptor). The following
; format descriptors are supported:
;
;         ~w   day of week (Mon, Tue, ...)
;         ~y   year
;         ~:m  number of month
;         ~@m  month name (Jan, Feb, ...)
;         ~h   hour
;         ~m   minute
;         ~s   second
;         ~~   literal ~
;
; When a single digit appears between a ~ and the rest of a
; format descriptor, this digit will be interpreted as a length
; and the resulting string will be padded to this length with
; zeros.
;
; Example:   (format-time "~w ~4y-~@m-~2d ~2h:~2m:~2s"
;                         '(1 2009 3 9 8 53 20))
;                ==> "Tue 2009-Mar-09 08:53:20"

(load-from-library "proper-timep.scm")

(define (format-time format t)
  (let ((proper-time? proper-time?)
        (wdays '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
        (months '#("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
        (zeros (make-string 10 #\0))
        (in-range? (lambda (n0 nn a)
                     (and (not (null? a))
                          (char<=? n0 (car a) nn))))
        (next (lambda (a)
                (if (null? a) a (cdr a)))))
    (and (proper-time? t)
         (let loop ((f (string->list format))
                    (s '()))
           (cond ((null? f)
                   (apply string-append (reverse! s)))
                 ((char=? (car f) #\~)
                   (let* ((k     (if (in-range? #\0 #\9 (cdr f))
                                     (- (char->integer (cadr f))
                                        (char->integer #\0))
                                     0))
                          (f     (if (in-range? #\0 #\9 (cdr f))
                                     (next f)
                                     f))
                          (colon (in-range? #\: #\: (cdr f)))
                          (f     (if (in-range? #\: #\: (cdr f))
                                     (next f)
                                     f))
                          (at    (in-range? #\@ #\@ (cdr f)))
                          (f     (if (in-range? #\@ #\@ (cdr f))
                                     (next f)
                                     f))
                          (type  (cond ((null? (cdr f)) #f)
                                       ((memv (cadr f)
                                              '(#\w #\y #\m #\d #\h #\s #\~))
                                         (cadr f))
                                       (else #f)))
                          (f     (next f)))
                     (and type
                          (let*
                            ((fmt (case type
                                    ((#\w) (vector-ref wdays (list-ref t 0)))
                                    ((#\y) (number->string (list-ref t 1)))
                                    ((#\m) (cond
                                             (colon
                                               (number->string
                                                 (list-ref t 2)))
                                             (at
                                               (vector-ref
                                                 months
                                                 (- (list-ref t 2) 1)))
                                             (else
                                               (number->string
                                                 (list-ref t 5)))))
                                    ((#\d) (number->string (list-ref t 3)))
                                    ((#\h) (number->string (list-ref t 4)))
                                    ((#\s) (number->string (list-ref t 6)))
                                    (else  (string type))))
                             (fmt (let ((n (string-length fmt)))
                                    (if (> k n)
                                        (string-append
                                          (substring zeros 0 (- k n))
                                          fmt)
                                        fmt))))
                            (loop (next f)
                                  (cons fmt s))))))
                 (else
                   (loop (cdr f)
                         (cons (string (car f)) s))))))))
