; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (random integer)        ==>  integer
; (random-state integer)  ==>  procedure
;
; (load-from-library "random.scm")
;
; RANDOM returns a random number in the range [0;INTEGER], where
; INTEGER may not be any larger than 2**19 = 524288.
;
; RANDOM-STATE returns a procedure that resembles RANDOM but uses
; a user-supplied seed instead of a default one. RANDOM can be passed
; a different seed by running:
;
;       (set! random (random-state SEED))
;
; RANDOM-STATE uses a 19-bit linear feedback shift register. Hence
; its limited range.
;
; Example:   (list (random 100)
;                  (random 100)
;                  (random 100))  ==>  (5 47 68)

(define (random-state . seed)
  (let ((seed (if (not (null? seed))
                  (remainder (car seed) 524288)
                  #xdead))
        (xor (lambda (x y)
               (if (eqv? x y) #\0 #\1))))
    (lambda (n)
      (let* ((v seed)
             (bits (number->string seed 2))
             (bits (string-append
                     (make-string (- 19 (string-length bits)) #\0)
                     bits))
             (b0   (string-ref bits 18))
             (b1   (string-ref bits 17))
             (b2   (string-ref bits 16))
             (b3   (string-ref bits 13))
             (rot  (fold-right xor b3 (list b0 b1 b2)))
             (next (string->number
                     (string-append (string rot)
                                    (substring bits 0 18))
                     2)))
        (set! seed (remainder (+ 1 next) 524288))
        (remainder v n)))))

(define random (random-state))
