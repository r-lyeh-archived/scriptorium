; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (dec! <variable>)                ==>  unspecific
; (inc! <variable>)                ==>  unspecific
; (pop! <variable>)                ==>  object
; (push! object <variable>)        ==>  unspecific
; (set-vars! <variable> ... list)  ==>  unspecific
; (swap! <variable1> <variable2>)  ==>  unspecific
;
; PUSH! conses OBJECT to the list bound to <variable>,
; and updates the value of <variable> to the new list.
;
; POP! removes the first element of the list bound to
; <variable> and updates <variable> to the rest of that
; list. It returns the element removed from the list.
;
; INC! and DEC! increment and decrement the value of
; the given variable, respectively.
;
; SET-VARS! sets the given <variable>s to the values in
; LIST. It may be used to set variables to multiple values
; that are returned from a procedure in a list.
;
; SWAP! swap the values bound to <variable1> and <variable2>.
;
; Example:   (let ((stack (list 0 2 3 4)))
;              (let ((x (pop! stack)))
;                (push! 1 stack)
;                (list x stack)))           ==>  (0 (1 2 3 4))
;
;            (let ((x 1))
;              (dec! x)
;              x)          ==>  0
;
;            (let ((a 0) (b 0) (c 0))
;              (set-vars! a b c '(foo bar baz))
;              (list a b c))                     ==>  (foo bar baz)
;
;            (let ((a 0)
;                  (b 1))
;              (swap! a b)
;              (list a b))  ==>  (1 0)

(define-syntax (dec! var)
  `(set! ,var (- ,var 1)))

(define-syntax (inc! var)
  `(set! ,var (+ 1 ,var)))

(define-syntax (push! obj var)
  `(set! ,var (cons ,obj ,var)))

(define-syntax (pop! var)
  (let ((top (gensym)))
    `(let ((,top (car ,var)))
       (set! ,var (cdr ,var))
       ,top)))

(define-syntax (set-vars! . args)
  (if (or (null? args)
          (null? (cdr args)))
      (error "set-vars!: too few arguments" args))
  (let ((expr (gensym)))
    (let loop ((names args)
               (asgmt '())
               (i     0))
      (cond ((null? (cdr names))
              `(let ((,expr ,(car names)))
                 (begin ,@asgmt)))
            ((symbol? (car names))
              (let ((name (car names)))
                (loop (cdr names)
                      (cons (case i
                              ((0)  `(set! ,name (car ,expr)))
                              ((1)  `(set! ,name (cadr ,expr)))
                              ((2)  `(set! ,name (caddr ,expr)))
                              ((3)  `(set! ,name (cadddr ,expr)))
                              (else `(set! ,name (list-ref ,expr ,i))))
                            asgmt)
                      (+ 1 i))))
            (else
              (error "set-vars!: expected symbol, got" (car names)))))))

(define-syntax (swap! a b)
  (let ((t (gensym)))
    `(let ((,t ,a))
       (set! ,a ,b)
       (set! ,b ,t))))
