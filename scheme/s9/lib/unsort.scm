; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (unsort list seed)  ==>  list
;
; Unsort a list of integers. SEED must be a number in the range
; 0..(- (length LIST) 1). The original list is not changed.
;
; Example:   (unsort '(1 2 3 4 5) 1)  ==>  (1 3 5 4 2)

(define (unsort a seed)
  (letrec
    ((remove-nth
       (lambda (a n r)
         (if (zero? n) 
             (if (null? a)
                 (reverse! r)
                 (append (cdr a) (reverse! r)))
             (remove-nth (cdr a) (- n 1) (cons (car a) r)))))
     (unsort4
       (lambda (a n k r)
         (if (zero? k)
             (cons (car a) r)
             (unsort4 (remove-nth a n '())
                      (remainder (car a) k)
                      (- k 1)
                      (cons (list-ref a n) r))))))
    (unsort4 a seed (- (length a) 1) '())))
