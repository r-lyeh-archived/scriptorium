; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (catch <tag> <statement> ...)  ==>  object
; (throw <tag> <expression>)     ==>  undefined
;
; (load-from-library "catch.scm")
;
; Implement Common LISP-style CATCH and THROW. <Tag> must evaluate to
; an object that can be checked for identity using EQ? (typically a
; symbol). CATCH establishes a catch named <tag> and then evaluates
; the given <statement>s in an implicit BEGIN. Unless one of the
; statements executes THROW, the value of the last <statement> will
; be returned. When a <statement> executes THROW, control will be
; passed to the innermost catch with the same <tag> as the throw.
; In this case the catch will exit immediately, returning the value
; of <expression> as its result.
;
; Example:   (let ((v #f))
;              (let ((r (catch 'foo
;                         (set! v 0)
;                         (throw 'foo 1)
;                         (set! v 2)
;                         3)))
;                (list v r)))             ==>  (0 1)

(load-from-library "setters.scm")

(define *catch-stack* '())

(define-syntax (catch tag . body)
  `(call/cc
     (lambda (c)
       (push! (cons ,tag c) *catch-stack*)
       (let ((result (begin ,@body)))
         (pop! *catch-stack*)
         result))))

(define (throw tag value)
  (do ((cs *catch-stack* (cdr cs)))
      ((null? cs)
        (set! *catch-stack* '())
        (error "throw: no catcher for" tag))
    (if (eq? tag (caar cs))
        (begin (set! *catch-stack* (cdr cs))
               ((cdar cs) value)))))
