; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009-2012
; Placed in the Public Domain
;
; (iota integer)             ==>  list
; (iota integer1 integer2)   ==>  list
; (iota* integer1 integer2)  ==>  list
;
; (load-from-library "iota.scm")
;
; IOTA creates a sequence of integers starting at 1 and ending
; at INTEGER (including both). When a second argument is passed
; to IOTA, the sequence returned by it will start at INTEGER1
; and ; end at INTEGER2 (again, including both).
;
; IOTA* differs from IOTA in two ways: it always takes two
; arguments and excludes INTEGER2 from the generated sequence.
;
; Example:   (iota 7)       ==>  (1 2 3 4 5 6 7)
;            (iota 17 21)   ==>  (17 18 19 20 21)
;            (iota* 17 21)  ==>  (17 18 19 20)
;            (iota* 1 1)    ==>  ()

(define (iota* l h)
  (if (> l h)
      (error "iota*: bad range" (list l h)))
  (let j ((x (- h 1))
          (r '()))
    (if (< x l)
        r
        (j (- x 1) (cons x r)))))

(define (iota l . h)
  (let ((l (if (null? h) 1 l))
        (h (if (null? h) l (car h))))
    (iota* l (+ 1 h))))
