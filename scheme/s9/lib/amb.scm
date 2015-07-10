; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (amb <expression> ...)  ==>  object
; (amb-collector)         ==>  procedure
; (amb-done? object)      ==>  boolean
; (amb-reset)             ==>  unspecific
;
; (load-from-library "amb.scm")
;
; Another variation of McCarthy's AMB operator.
;
; AMB takes a list of <expression>s, immediately evaluates the first one
; and returns its value. When no <expression>s are passed to AMB (or it
; runs out of <expression>s), it checks whether there is another AMB further
; up in the program structure. If there is one, it returns control to that
; instance of AMB, thereby initiating backtracking. When backtracking to an
; AMB, that AMB evaluates the next <expression> from its argument list, and
; so on.
;
; When an AMB runs out of options and there is no AMB to return control
; to, it returns a special object that can be detected with AMB-DONE?.
;
; AMB-COLLECT generates a procedure of the form
;
;       (lambda (p . v*) ...)
;
; where P is a predicate and V* is a list of values. When called, the
; procedure will apply P to V* and if the predicate is satisfied, it will
; store the values internally and initiate backtracking by calling (amb).
; When one of the values of V* satisfies AMB-DONE?, the procedure will
; return the list stored internally. At this point, the list contains
; the union of all combinations of AMB values that satisfy P.
;
; AMB-RESET clears the AMB tree. It serves as a CUT operator and an
; initializing procedure are the same time. After calling AMB-RESET
; (amb) will always fail. Any program that uses AMB should first call
; AMB-RESET.
;
; Example:   (begin (amb-reset)
;                   (let ((collect (amb-collector)))
;                     (let ((x (amb 4 1 7)))
;                       (let ((y (amb 6 8 2)))
;                         (let ((z (amb 5 3 9)))
;                           (collect > x y z))))))  ==>  ((7 6 3) (7 6 5))

(load-from-library "setters.scm")
(load-from-library "exists.scm")

(define *amb-stack* '())

(define *amb-done* (list 'amb-done))

(define (amb-reset)
  (set! *amb-stack* '()))

(define (amb-done? x)
  (eq? *amb-done* x))

(define-syntax (amb . expr*)

  (define (unfold-alternatives e*)
    (if (null? e*)
        '(begin (pop! *amb-stack*)
                (if (null? *amb-stack*)
                    *amb-done*
                    ((car *amb-stack*) *amb-done*)))
        `(let ((x (call/cc
                    (lambda (k)
                      (set-car! *amb-stack* k)
                      ,(car e*)))))
             (if (amb-done? x)
                 ,(unfold-alternatives (cdr e*))
                 x))))

  `(begin (push! #f *amb-stack*)
          ,(unfold-alternatives expr*)))

(define (amb-collector)
  (let ((values '()))
    (lambda (p . v*)
      (if (exists amb-done? v*)
          values
          (begin (if (apply p v*)
                     (push! v* values))
                 (amb))))))
