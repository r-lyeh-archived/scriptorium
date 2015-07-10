; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (string-digest string)          ==>  integer
; (string-digest string integer)  ==>  integer
;
; Create a digest of the given string and return it. If specified,
; INTEGER determines the maximum size of the digest (2^INTEGER).
;
; CAVEAT: This procedures uses a simple non-cryptographic BSD
; sum-style algorithm. It should be replaced with a function
; generating more unique digests.
;
; The maximum digest size defaults to 2^32.
;
; Example:   (string-digest "hello")  ==>  2107915172

(define (string-digest s . mod)
  (let* ((ks    (string-length s))
         (mod   (if (null? mod) 32 (car mod)))
         (lim/2 (expt 2 (- mod 1)))
         (lim   (* 2 lim/2)))
    (let loop ((h (* 3 #xf0e1d2c3b4a5968778695a4b3c2d1e0f))
               (i 0))
      (if (>= i ks)
          h
          (loop (remainder
                  (let ((wrap (if (even? h) lim/2 0)))
                     (+ wrap
                        (quotient h 2)
                        (char->integer (string-ref s i))))
                  lim)
                (+ 1 i))))))
