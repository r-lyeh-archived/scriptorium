; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (accept-keywords string list1 list2)  ==>  #t | undefined
; (keyword-value list symbol)           ==>  object | undefined
; (keyword-value list symbol object)    ==>  object
;
; KEYWORD-VALUE finds the value associated with a keyword in a property
; list (keywords in odd positions, values in even positions). When the
; keyword is not found in the plist and a default OBJECT is specified,
; that object is returned. When the keyword is not found and no default
; is given, an error is signalled.
;
; ACCEPT-KEYWORDS checks each keyword in LIST1 against symbols listed
; in LIST2. When LIST1 contains a keyword not contained in LIST2, it
; signals an error. It also signals an error when LIST1 contains a
; trailing keyword (without an associated value).
; When reporting an error ACCEPT-KEYWORD will insert the given STRING
; as the source of the error.
;
; Example:   (keyword-value '(foo 1 bar 2) 'bar)  ==>  2
;            (keyword-value '(foo 1) 'bar 0)      ==>  0
;
;            (accept-keywords "test" '(foo 1 bar 2) '(foo bar))  ==>  #t

(define (keyword-value opts key . default)
  (letrec
     ((getprop
        (lambda (x a)
          (cond ((null? a) #f)
                ((null? (cdr a)) #f)
                ((eq? x (car a)) a)
                (else (getprop x (cddr a)))))))
    (cond ((getprop key opts)
            => (lambda (opt)
                 (if (pair? (cdr opt))
                     (cadr opt)
                     (error "keyword-value: missing value" key))))
          ((not (null? default))
            (car default))
          (else
            (error "keyword-value: no such keyword" key)))))

(define (accept-keywords who opts keywords)
  (cond ((null? opts))
        ((null? (cdr opts))
          (error (string-append who ": keyword without value")
                 (car opts)))
        ((memq (car opts) keywords)
          (accept-keywords who (cddr opts) keywords))
        (else
          (error (string-append who ": unknown keyword")
                 (car opts)))))
