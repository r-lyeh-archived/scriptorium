; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (get-prop plist symbol)               ==>  object
; (put-prop plist symbol object)        ==>  plist
; (rem-prop plist symbol)               ==>  plist
; (put-prop! <variable> symbol object)  ==>  plist
; (rem-prop! <variable> symbol)         ==>  plist
;
; A property list (plist) is a list of the form
;
;       (symbol1 object1 symbol2 object2 ...)
;
; GET-PROP returns the object (property) following the given
; SYMBOL or #F if SYMBOL does not exist in an odd position
; in the PLIST.
; PUT-PROP returns a new plist in which the given OBJECT is
; the property associated with SYMBOL. When SYMBOL is already
; in the PLIST, the existing association will be removed.
; REM-PROP returns a new plist with the given SYMBOL and the
; associated property removed.
;
; PUT-PROP! adds a new property to the plist bound to the
; given <variable>. The variable will be bound to the new
; list. REM-PROP! removes a property from a plist that is
; bound to the <variable>.
;
; Example:   (get-prop '() 'foo)        ==>  #f
;            (put-prop '() 'foo 42)     ==>  (foo 42)
;            (get-prop '(foo 42) 'foo)  ==>  42
;            (rem-prop '(foo 42) 'foo)  ==>  ()

(define (get-prop a x)
  (cond ((null? a)
          #f)
        ((and (eq? x (car a))
              (pair? (cdr a)))
          (cadr a))
        ((pair? (cdr a))
          (get-prop (cddr a) x))
        (else
          #f)))

(define (put-prop a k v)
  (let loop ((in  a)
             (out '()))
    (cond ((null? in)
            (cons k (cons v a)))
          ((and (eq? k (car in))
                (pair? (cdr in)))
            (append (reverse! (cons v (cons k out)))
                    (cddr in)))
          ((pair? (cdr in))
            (loop (cddr in)
                  (cons (cadr in)
                        (cons (car in) out))))
          (else
            (cons k (cons v a))))))

(define (rem-prop a x)
  (let loop ((in  a)
             (out '()))
    (cond ((null? in)
            (reverse! out))
          ((and (eq? x (car in))
                (pair? (cdr in)))
            (append (reverse! out) (cddr in)))
          ((pair? (cdr in))
            (loop (cddr in)
                  (cons (cadr in)
                        (cons (car in) out))))
          (else
            a))))

(define-syntax (put-prop! n k v)
  `(set! ,n (put-prop ,n ,k ,v)))

(define-syntax (rem-prop! n k)
  `(set! ,n (rem-prop ,n ,k)))
