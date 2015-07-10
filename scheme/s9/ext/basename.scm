; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (basename string)          ==>  string
; (basename string string2)  ==>  string
;
; Return the rightmost component of the path name given in STRING.
; When STRING2 is given also remove a STRING2 suffix (if any) from
; STRING.
; 
; Example:   (basename "/foo/bar/baz")     ==>  "baz"
;            (basename "/goo/bar.Z" ".Z")  ==>  "bar"

(load-from-library "string-split.scm")

(define (basename path . suffix)
  (let ((base (car (reverse! (string-split #\/ path)))))
    (if (null? suffix)
        base
        (let ((k  (string-length base))
              (ks (string-length (car suffix))))
          (if (and (> k ks)
                   (string=? (car suffix)
                             (substring base (- k ks) k)))
              (substring base 0 (- k ks))
              base)))))
