; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (unless <test-expression> <body>)  ==>  object
; (when <test-expression> <body>)    ==>  object
;
; (load-from-library "when.scm")
;
; The WHEN syntax first evaluates <test-expression>. When it evaluates
; to a true value, it also evaluates <body>, which is a sequence of
; expressions. The expressions will be evaluated in order and the value
; of the last expression will be returned. When <test-expression>
; evaluates to #f, WHEN does not evaluate its <body> and returns an
; unspecific value.
;
; UNLESS is evaluates its body only if <test-expression> evaluates to #F.
; It evaluates its <body> exactly if WHEN would not evaluate it and vice
; versa.
;
; Example:   (when (= 1 1) 'foo 'bar 'baz)    ==>  baz
;            (unless (= 1 2) 'foo 'bar 'baz)  ==>  baz

(define-syntax (when p . body)
  `(if ,p (begin ,@body)))

(define-syntax (unless p . body)
  `(if (not ,p) (begin ,@body)))
