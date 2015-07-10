; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (random-sort list)          ==>  list
; (random-sort list integer)  ==>  list
;
; Create a random permutation of LIST and return it. When INTEGER
; is specified, use it to initialize a random state (see RANDOM).
; When no seed or the same seed is specified, RANDOM-SORT will
; always deliver the same permutation.
;
; Example:   (random-sort '(1 2 3 4 5))  ==>  (2 3 5 1 4)

(load-from-library "hof.scm")
(load-from-library "random.scm")
(load-from-library "sort.scm")

(define (random-sort a . seed)
  (let ((r (apply random-state seed)))
    (sort (const (= 0 (r 2))) a)))
