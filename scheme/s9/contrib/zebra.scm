; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (zebra)  ==>  list
;
; Solve the zebra puzzle using AMK.
;
; Example:   (zebra)  ==>  (((norwegian kools _.0 fox yellow)
;                            (ukrainian chesterfields tea horse blue)
;                            (englishman oldgolds milk snails red)
;                            (spaniard luckystrikes orangejuice dog ivory)
;                            (japanese parliaments coffee zebra green)))

(load-from-library "amk.scm")

(define (lefto x y l)
  (fresh (t)
    (cdro l t)
    (any (all (caro l x)
              (caro t y))
         (lefto x y t))))

(define (nexto x y l)
  (any (lefto x y l)
       (lefto y x l)))

(define (zebra)
  (run* (h)
    (== h (list (list 'norwegian (_) (_) (_) (_))
                (_)
                (list (_) (_) 'milk (_) (_))
                (_)
                (_)))
    (memo (list 'englishman (_) (_) (_) 'red) h)
    (lefto (list (_) (_) (_) (_) 'ivory)
           (list (_) (_) (_) (_) 'green) h)
    (nexto (list 'norwegian (_) (_) (_) (_))
           (list (_) (_) (_) (_) 'blue) h)
    (memo (list (_) 'kools (_) (_) 'yellow) h)
    (memo (list 'spaniard (_) (_) 'dog (_)) h)
    (memo (list (_) (_) 'coffee (_) 'green) h)
    (memo (list 'ukrainian (_) 'tea (_) (_)) h)
    (memo (list (_) 'luckystrikes 'orangejuice (_) (_)) h)
    (memo (list 'japanese 'parliaments (_) (_) (_)) h)
    (memo (list (_) 'oldgolds (_) 'snails (_)) h)
    (nexto (list (_) (_) (_) 'horse (_))
           (list (_) 'kools (_) (_) (_)) h)
    (nexto (list (_) (_) (_) 'fox (_))
           (list (_) 'chesterfields (_) (_) (_)) h)
  ;  (memo (list (_) (_) 'water (_) (_)) h)
    (memo (list (_) (_) (_) 'zebra (_)) h)))
