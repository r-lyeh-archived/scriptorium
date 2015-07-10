; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (vector-map procedure vector ...)   ==>  vector
; (vector-map! procedure vector ...)  ==>  unspecific
;
; (load-from-library "vector-map.scm")
;
; Map a procedure over a set of vectors, giving a new vector
;
;       (vector (f (vector-ref v0 i) ... ) ...)
;
; where F is the given PROCEDURE, V0 is the first and VN is the last
; VECTOR specified. The arity of PROCEDURE must match the number of
; vectors passed to these procedures. VECTOR-MAP is to vectors what
; MAP is to lists.
;
; VECTOR-MAP! does not create a fresh vector, but changes the members
; of the *first* vector in situ.
;
; Example:   (vector-map + '#(1 2 3) '#(4 5 6))  ==>  #(5 7 9)
;            (let ((v (vector 1 2 3)))
;              (vector-map! - v)
;              v)                                ==>  #(-1 -2 -3)

(define (vector-map! f . v*)
  (if (null? v*)
      (error "vector-map!: too few arguments")
      (let ((k (apply min (map vector-length v*))))
        (do ((i 0 (+ 1 i)))
            ((>= i k))
          (vector-set! (car v*) i (apply f (map (lambda (x)
                                                  (vector-ref x i))
                                                v*)))))))

(define (vector-map f . v*)
  (if (null? v*)
      (error "vector-map: too few arguments")
      (let* ((k (apply min (map vector-length v*)))
             (n (make-vector k)))
        (do ((i 0 (+ 1 i)))
            ((>= i k)
              n)
          (vector-set! n i (apply f (map (lambda (x)
                                           (vector-ref x i))
                                         v*)))))))
