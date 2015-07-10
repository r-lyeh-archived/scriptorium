; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2013
; Placed in the Public Domain
;
; (queens n)  ==>  list
;
; Solve the N-Queens puzzle using AMK. All solutions for the
; given board size will be returned, where the offset in each
; list is an X position on the board and the value at that
; offset is the corresponding Y position. E.g., the board
; (2 0 3 1) would look like this:
;
; . . & . 3
; & . . . 2
; . . . & 1
; . & . . 0
; 0 1 2 3
;
; Example:   (queens 4)  ==>  ((2 0 3 1) (1 3 0 2))

(load-from-library "amk.scm")

(define (safeo p1x p1y p2x p2y)
  (all (/=p p1x p2x)
       (/=p p1y p2y)
       (/=p (abs (- p1x p2x))
            (abs (- p1y p2y)))))

(define (board-safeo x y n b)
  (any (nullo b)
       (fresh (a d nn)
         (conso a d b)
         (eql nn (- n 1))
         (safeo x y nn a)
         (board-safeo x y nn d))))

(define (queenso n b z r)
  (any (all (== n z)
            (== b r))
       (fresh (t nn nb)
         (range t 0 (- z 1))
         (board-safeo n t n b)
         (eql nn (+ 1 n))
         (conso t b nb)
         (queenso nn nb z r))))

(define (queens z)
  (run* (q) (queenso 0 '() z q)))
