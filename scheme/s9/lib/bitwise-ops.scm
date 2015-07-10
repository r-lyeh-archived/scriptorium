; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009,2012
; Placed in the Public Domain
;
; (bitwise-clear integer1 integer2 ...)        ==>  integer
; (bitwise-and integer1 integer2 ...)          ==>  integer
; (bitwise-and-c2  integer1 integer2 ...)      ==>  integer
; (bitwise-1 integer1 integer2 ...)            ==>  integer
; (bitwise-and-c1 integer1 integer2 ...)       ==>  integer
; (bitwise-2 integer1 integer2 ...)            ==>  integer
; (bitwise-xor integer1 integer2 ...)          ==>  integer
; (bitwise-or integer1 integer2 ...)           ==>  integer
; (bitwise-not-or integer1 integer2 ...)       ==>  integer
; (bitwise-not-xor integer1 integer2 ...)      ==>  integer
; (bitwise-c2 integer1 integer2 ...)           ==>  integer
; (bitwise-or-c2 integer1 integer2 ...)        ==>  integer
; (bitwise-c1 integer1 integer2 ...)           ==>  integer
; (bitwise-or-c1 integer1 integer2 ...)        ==>  integer
; (bitwise-not-and integer1 integer2 ...)      ==>  integer
; (bitwise-set integer1 integer2 ...)          ==>  integer
; (bitwise-shift-left integer1 integer2 ...)   ==>  integer
; (bitwise-shift-right integer1 integer2 ...)  ==>  integer
;
; (load-from-library "bitwise-ops.scm")
;
; Bitwise logic operations. These operations are defined as follows:
;
; INT1             0   0   1   1
; INT2             0   1   0   1  Operation
; ----------------------------------------------------------------
; bitwise-clear    0   0   0   0  set to 0
; bitwise-and      0   0   0   1  and
; bitwise-and-c2   0   0   1   0  and INT1 with complement of INT2
; bitwise-1        0   0   1   1  INT1
; bitwise-and-c1   0   1   0   0  and complement of INT1 with INT2
; bitwise-2        0   1   0   1  INT2
; bitwise-xor      0   1   1   0  exclusive or
; bitwise-or       0   1   1   1  or
; bitwise-not-or   1   0   0   0  not-or (nor)
; bitwise-not-xor  1   0   0   1  not-xor (equivalence)
; bitwise-c2       1   0   1   0  complement of INT2
; bitwise-or-c2    1   0   1   1  or INT1 with complement of INT2
; bitwise-c1       1   1   0   0  complement of INT1
; bitwise-or-c1    1   1   0   1  or complement of INT1 with INT2
; bitwise-not-and  1   1   1   0  not-and (nand)
; bitwise-set      1   1   1   1  set to 1
;
; BITWISE-SHIFT-LEFT shifts its first argument to the left by
; N bits where N is the value of the second argument.
; BITWISE-SHIFT-RIGHT shifts its first argument to the right.
;
; Multiple arguments associate to the left, i.e.: (BITWISE-op a b c)
; equals (BITWISE-op (BITWISE-op a b) c) for all of the above
; operations.
;
; Example:   (bitwise-clear   #b1010 #b1100)  ==>  #b0000
;            (bitwise-not-or  #b1010 #b1100)  ==>  #b0001
;            (bitwise-and-c2  #b1010 #b1100)  ==>  #b0010
;            (bitwise-c2      #b1010 #b1100)  ==>  #b0011
;            (bitwise-and-c1  #b1010 #b1100)  ==>  #b0100
;            (bitwise-c1      #b1010 #b1100)  ==>  #b0101
;            (bitwise-xor     #b1010 #b1100)  ==>  #b0110
;            (bitwise-not-and #b1010 #b1100)  ==>  #b0111
;            (bitwise-and     #b1010 #b1100)  ==>  #b1000
;            (bitwise-not-xor #b1010 #b1100)  ==>  #b1001
;            (bitwise-1       #b1010 #b1100)  ==>  #b1010
;            (bitwise-or-c2   #b1010 #b1100)  ==>  #b1011
;            (bitwise-2       #b1010 #b1100)  ==>  #b1100
;            (bitwise-or-c1   #b1010 #b1100)  ==>  #b1101
;            (bitwise-or      #b1010 #b1100)  ==>  #b1110
;            (bitwise-set     #b1010 #b1100)  ==>  #b1111
;            (bitwise-shift-left 1 10)        ==>  1024
;            (bitwise-shift-right 10 1)       ==>  5

(load-from-library "integer-to-binary-string.scm")

(define (make-variadic f)
  (lambda (a b . c)
    (fold-left f a (cons b c))))

(define (bitwise-op r0 r1 r2 r3)
  (make-variadic
    (lambda (x y)
      (let* ((k (number-of-bits (max x y)))
             (a (integer->binary-string x k))
             (b (integer->binary-string y k))
             (c (make-string k)))
        (let loop ((i 0))
          (if (>= i k)
              (binary-string->integer c)
              (let ((not-a (char=? #\0 (string-ref a i)))
                    (not-b (char=? #\0 (string-ref b i))))
                (let ((r (if not-a (if not-b r0
                                             r1)
                                   (if not-b r2
                                             r3))))
                  (string-set! c i r)
                  (loop (+ 1 i))))))))))

(define bitwise-clear   (bitwise-op #\0 #\0 #\0 #\0))
(define bitwise-and     (bitwise-op #\0 #\0 #\0 #\1))
(define bitwise-and-c2  (bitwise-op #\0 #\0 #\1 #\0))
(define bitwise-1       (bitwise-op #\0 #\0 #\1 #\1))
(define bitwise-and-c1  (bitwise-op #\0 #\1 #\0 #\0))
(define bitwise-2       (bitwise-op #\0 #\1 #\0 #\1))
(define bitwise-xor     (bitwise-op #\0 #\1 #\1 #\0))
(define bitwise-or      (bitwise-op #\0 #\1 #\1 #\1))
(define bitwise-not-or  (bitwise-op #\1 #\0 #\0 #\0))
(define bitwise-not-xor (bitwise-op #\1 #\0 #\0 #\1))
(define bitwise-c2      (bitwise-op #\1 #\0 #\1 #\0))
(define bitwise-or-c2   (bitwise-op #\1 #\0 #\1 #\1))
(define bitwise-c1      (bitwise-op #\1 #\1 #\0 #\0))
(define bitwise-or-c1   (bitwise-op #\1 #\1 #\0 #\1))
(define bitwise-not-and (bitwise-op #\1 #\1 #\1 #\0))
(define bitwise-set     (bitwise-op #\1 #\1 #\1 #\1))

(define bitwise-shift-left
  (make-variadic
    (lambda (a b)
      (* a (expt 2 b)))))

(define bitwise-shift-right
  (make-variadic
    (lambda (a b)
      (quotient a (expt 2 b)))))
