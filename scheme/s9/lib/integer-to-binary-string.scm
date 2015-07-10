; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (binary-string->integer string)             ==>  integer
; (integer->binary-string integer1 integer2)  ==>  string
; (number-of-bits integer)                    ==>  integer
;
; INTEGER->BINARY-STRING writes a binary notation of INTEGER
; to a fresh string and returns it. The resulting string will
; be INTEGER2 characters wide and will consist of zeros and
; ones exclusively. INTEGER2 must be large enough to hold the
; desired binary representation. NUMBER-OF-BITS may be used
; to find this value.
;
; BINARY-STRING->INTEGER converts the binary representation of
; an integer contained in STRING to an integer. It basically
; does the same as STRING->NUMBER with a radix of 2, but is
; hopefully more efficient.
;
; NUMBER-OF-BITS returns the least number of bits that is
; needed to store the given integer in binary format.
;
; Example:   (integer->binary-string 123 8)       ==>  "01111011"
;            (binary-string->integer "01111011")  ==>  123
;            (number-of-bits 127)                 ==>  7

(define (integer->binary-string v k)
  (let ((s (make-string k #\0)))
    (do ((v v (quotient v 2))
         (i (- k 1) (- i 1)))
        ((zero? v)
          s)
      (if (odd? v)
          (if (negative? i)
              (error "integer->binary-string: string too small")
              (string-set! s i #\1))))))

(define (binary-string->integer s)
  (let ((k (string-length s)))
    (do ((v 0 (* 2 v))
         (i 0 (+ 1 i)))
        ((>= i k)
          (quotient v 2))
      (if (char=? #\1 (string-ref s i))
          (set! v (+ 1 v))))))

(define (number-of-bits x)
  (let loop ((i 1)
             (v 1))
    (if (< v x)
        (loop (+ 1 i) (+ 1 (* 2 v)))
        i)))
