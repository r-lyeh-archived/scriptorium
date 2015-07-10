; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (string-unsplit char list)  ==>  string
;
; Unsplit a list of strings, giving a new string. CHAR is placed between
; each two elements of LIST.
;
; Example:   (string-unsplit #\: '("" "a" "b" "" "c"))  ==>  ":a:b::c"

(define (string-unsplit c s*)
  (let loop ((s* s*)
             (r  '())
             (s0 #t))
    (cond ((null? s*)
            (apply string-append (reverse! r)))
          (s0
            (loop (cdr s*)
                  (cons (car s*) r)
                  #f))
          (else
            (loop (cdr s*)
                  (cons (car s*) (cons (string c) r))
                  #f)))))
