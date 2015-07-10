;;
;; Scheme 9 from Empty Space, Refactored
;; By Nils M Holm 2007-2015
;; In the public domain
;;

;; Some obvious procedures first

(define (void) (if #f #f))

(define call-with-current-continuation call/cc)

;; Auxiliary definitions, will be redefined later

(define append append2)

; There is no LET or LETREC yet, so

(define-syntax (let bindings . exprs)
  ((lambda (split)
     ((lambda (tmp-split)
        (set! split tmp-split)
        (apply (lambda (vars args)
                 (append
                   (list (append
                           (list 'lambda)
                           (append (list vars)
                                   exprs)))
                   args))
               (split bindings '() '())))
      (lambda (bind* vars args)
        (if (null? bind*)
            (list vars args)
            (split (cdr bind*)
                   (cons (caar bind*) vars)
                   (cons (cadr (car bind*)) args))))))
   #f))

(define (map-car f a)
  (let ((mapcar1 #f))
    (let ((tmp-mapcar1
            (lambda (a)
              (if (null? a)
                  '()
                   (cons (f (car a))
                         (mapcar1 (cdr a)))))))
    (set! mapcar1 tmp-mapcar1)
    (mapcar1 a))))

(define (map f a b)
  (let ((map2 #f))
    (let ((tmp-map2
            (lambda (a b)
              (if (null? a)
                  '()
                   (cons (f (car a) (car b))
                         (map2 (cdr a) (cdr b)))))))
    (set! map2 tmp-map2)
    (map2 a b))))

(define-syntax (letrec bindings . exprs)
  (let ((append3
          (lambda (a b c)
            (append a (append b c))))
        (tmps (map-car (lambda (x) (gensym)) bindings))
        (vars (map-car car bindings))
        (args (map-car cadr bindings)))
    (let ((undefineds   (map-car (lambda (v) (list v #f))
                                 vars))
          (tmp-bindings (map list tmps args))
          (updates      (map (lambda (v t) (list 'set! v t))
                             vars
                             tmps)))
      (list 'let
            undefineds
            (append3 '(let)
                     (list tmp-bindings)
                     (append updates exprs))))))

;; Type predicates

(define number? real?)

(define (port? x)
  (or (input-port? x)
      (output-port? x)))

;; Equivalence predicates

(define (equal? a b)
  (cond ((eq? a b))
        ((and (pair? a)
              (pair? b))
          (and (equal? (car a) (car b))
               (equal? (cdr a) (cdr b))))
        ((string? a)
          (and (string? b)
               (string=? a b)))
        ((vector? a)
           (and (vector? b)
                (equal? (vector->list a)
                        (vector->list b))))
        (else
          (eqv? a b))))

;; List procedures

(define (list? x)
  (letrec
    ((l? (lambda (x y)
           (cond ((eq? x y) #f)
                 ((null? x) #t)
                 ((pair? x) (or (null? (cdr x))
                                (and (pair? (cdr x))
                                     (l? (cddr x) (cdr y)))))
                 (else      #f)))))
    (or (null? x)
        (and (pair? x)
             (l? (cdr x) x)))))

(define (assoc x a)
  (cond ((null? a) #f)
        ((equal? (caar a) x) (car a))
        (else (assoc x (cdr a)))))

(define (member x a)
  (cond ((null? a) #f)
        ((equal? (car a) x) a)
        (else (member x (cdr a)))))

; Auxiliary functions for FOLD-LEFT, FOLD-RIGHT, MAP

(define (map-car f a)
  (letrec
    ((mapcar1
       (lambda (a r)
         (if (null? a)
             (reverse r)
             (mapcar1 (cdr a)
                      (cons (f (car a)) r))))))
    (mapcar1 a '())))

(define car-of
  (let ((map-car map-car))
    (lambda (a*)
      (map-car car a*))))

(define cdr-of
  (let ((map-car map-car))
    (lambda (a*)
      (map-car cdr a*))))

(define (any-null a*)
  (memq '() a*))

(define fold-left
  (let ((car-of   car-of)
        (cdr-of   cdr-of)
        (any-null any-null))
    (lambda (f b . a*)
      (letrec
        ((fold
           (lambda (a* r)
             (if (any-null a*)
                 r
                 (fold (cdr-of a*)
                       (apply f r (car-of a*)))))))
        (if (null? a*)
            (error "fold-left: too few arguments")
            (fold a* b))))))

(define fold-right
  (let ((car-of   car-of)
        (cdr-of   cdr-of)
        (any-null any-null)
        (map-car  map-car))
    (lambda (f b . a*)
      (letrec
        ((foldr
           (lambda (a* r)
             (if (any-null a*)
                 r
                 (foldr (cdr-of a*)
                        (apply f (append2 (car-of a*)
                                          (list r))))))))
        (if (null? a*)
            (error "fold-right: too few arguments")
            (foldr (map-car reverse a*) b))))))

(define append
  (let ((append2 append2))
    (letrec
      ((foldr-app
         (lambda (a)
           (cond ((null? a)
                   '())
                 ((and (pair? a)
                       (not (pair? (car a)))
                       (null? (cdr a)))
                   (car a))
                 (else
                   (append2 (car a) (foldr-app (cdr a))))))))
      (lambda a
        (foldr-app a)))))

(define (list-ref x n)
  (car (list-tail x n)))

(define map
  (let ((car-of   car-of)
        (cdr-of   cdr-of)
        (any-null any-null))
    (lambda (f . a*)
      (letrec
        ((map2
           (lambda (a* r)
             (if (any-null a*)
                 (reverse r)
                 (map2 (cdr-of a*)
                       (cons (apply f (car-of a*))
                             r))))))
        (if (null? a*)
            (error "map: too few arguments")
            (map2 a* '()))))))

(define (for-each f . a*)
  (if (null? a*)
      (error "for-each: too few arguments")
      (apply map f a*))
  (void))

;; Arithmetic procedures

(define gcd
  (let ((fold-left fold-left))
    (lambda a
      (letrec
        ((gcd2
           (lambda (a b)
             (cond ((zero? b) a)
                   ((zero? a) b)
                   ((< a b) (gcd2 a (remainder b a)))
                   (else (gcd2 b (remainder a b)))))))
        (fold-left gcd2 0 (map abs a))))))

(define lcm
  (let ((fold-left fold-left))
    (lambda a
      (letrec
        ((lcm2
           (lambda (a b)
             (let ((cd (gcd a b)))
               (* cd (* (quotient a cd)
                        (quotient b cd)))))))
        (fold-left lcm2 1 (map abs a))))))

(define (modulo a b)
  (let ((rem (remainder a b)))
    (cond ((zero? rem) 0)
          ((eq? (negative? a) (negative? b)) rem)
          (else (+ b rem)))))

;; Input/output procedures

(define (newline . port)
  (apply write-char #\newline port))

(define (call-with-input-file file proc)
  (let ((f (open-input-file file)))
    (let ((r (proc f)))
      (close-input-port f)
      r)))

(define (call-with-output-file file proc)
  (let ((f (open-output-file file)))
    (let ((r (proc f)))
      (close-output-port f)
      r)))

(define with-input-from-file
  (let ((set-input-port! set-input-port!))
    (lambda (file thunk)
      (let ((outer-port (current-input-port))
            (new-port (open-input-file file)))
        (set-input-port! new-port)
        (let ((r (thunk)))
          (close-input-port new-port)
          (set-input-port! outer-port)
          r)))))

(define with-output-to-file
  (let ((set-output-port! set-output-port!))
    (lambda (file thunk)
      (let ((outer-port (current-output-port))
            (new-port (open-output-file file)))
        (set-output-port! new-port)
        (let ((r (thunk)))
          (close-output-port new-port)
          (set-output-port! outer-port)
          r)))))

;; Quasiquote Expander

(define-syntax (quasiquote tmpl)
  (letrec
    ((qq-cons
       (lambda (a b)
         (cond ((and (pair? a)
                     (eq? 'unquote-splicing (car a)))
                 (list 'append (cadr a) b))
               (else 
                 (list 'cons a b)))))
     (qq-expand-1
       (lambda (x)
         (cond ((vector? x)
                 (list 'list->vector (qq-expand-1 (vector->list x))))
               ((not (pair? x))
                 (list 'quote x))
               ((eq? 'unquote (car x))
                 (cadr x))
               ((eq? 'unquote-splicing (car x))
                 x)
               (else
                 (qq-cons (qq-expand-1 (car x))
                          (qq-expand-1 (cdr x)))))))
     (qq-expand
       (lambda (tmpl q)
         (let ((embedded-qq '()))
           (letrec
             ((extract-nested-qq
                (lambda (tmpl q)
                  (cond ((not (pair? tmpl))
                          tmpl)
                        ((or (eq? (car tmpl) 'unquote)
                             (eq? (car tmpl) 'unquote-splicing))
                          (if (not q)
                              (error
                                "quasiquote: extra unquote/unquote-splicing"))
                          (if (and (pair? (cdr tmpl))
                                   (null? (cddr tmpl)))
                              (list (car tmpl)
                                    (extract-nested-qq (cadr tmpl) #f))
                              (error (string-append
                                       (symbol->string (car tmpl))
                                       ": wrong number of arguments")
                                     tmpl)))
                        ((eq? 'quasiquote (car tmpl))
                          (if q (error "quasiquote: may not be nested"))
                          (if (and (pair? (cdr tmpl))
                                   (null? (cddr tmpl)))
                              (let ((g (gensym)))
                                (set! embedded-qq
                                      (cons (list g (qq-expand (cadr tmpl)
                                                               #t))
                                            embedded-qq))
                                g)
                              (error "quasiquote: wrong number of arguments"
                                     tmpl)))
                        (else
                          (cons (extract-nested-qq (car tmpl) q)
                                (extract-nested-qq (cdr tmpl) q)))))))
             (let ((tmpl (extract-nested-qq tmpl q)))
               (if (null? embedded-qq)
                   (qq-expand-1 tmpl)
                   (list 'let embedded-qq (qq-expand-1 tmpl)))))))))
    (qq-expand tmpl #t)))

;; Derived Syntax

; LET/LET*/LETREC helper

(define (check-bindings who b opt-arg)
  (cond ((null? b))
        ((and (pair? b)
              (pair? (car b))
              (symbol? (caar b))
              (pair? (cdar b))
              (or (null? (cddar b))
                  (and opt-arg
                       (pair? (cddar b))
                       (null? (cdddar b)))))
          (check-bindings who (cdr b) opt-arg))
        (else
          (error (string-append who ": invalid syntax") b))))

(define (split-bindings clauses)
  (letrec
    ((split3
       (lambda (clauses vars args opt)
         (cond ((null? clauses)
                 (list (reverse vars)
                       (reverse args)
                       (reverse opt)))
               (else
                 (split3 (cdr clauses)
                         (cons (caar clauses) vars)
                         (cons (cadar clauses) args)
                         (if (null? (cddar clauses))
                             (cons (caar clauses) opt)
                             (cons (caddar clauses) opt))))))))
    (split3 clauses '() '() '())))

; Now that the QQ expander is here, define a
; clean version of LET (including named LET).
; Can't name it LET yet, because it uses LET.

(define-syntax %full-let
  (let ((check-bindings check-bindings)
        (split-bindings split-bindings))
    (lambda (a1 a2 . a3)
      (if (symbol? a1)
          (if (null? a3)
              (error "named let: missing body"
                     `(let ,a1 ,a2 ,@a3))
              (begin (check-bindings "let" a2 #f)
                     (let ((va (split-bindings a2)))
                       (let ((v (car va))
                             (a (cadr va)))
                         `((letrec ((,a1 (lambda ,v ,@a3)))
                             ,a1) ,@a)))))
          (begin (check-bindings "let" a1 #f)
                 (let ((va (split-bindings a1)))
                   (let ((v (car va))
                         (a (cadr va)))
                     `((lambda ,v ,a2 ,@a3) ,@a))))))))

(define-syntax let %full-let)

; Also define a clean version of LETREC.

(define-syntax %clean-letrec
  (let ((check-bindings check-bindings)
        (split-bindings split-bindings))
    (lambda (bindings expr . exprs)
      (check-bindings "letrec" bindings #f)
      (let ((va (split-bindings bindings)))
        (let ((tmps (map (lambda (x) (gensym)) bindings))
              (vars (car va))
              (args (cadr va)))
          (let ((undefineds   (map (lambda (v) (list v #f))
                                   vars))
                (tmp-bindings (map (lambda (t a) (list t a))
                                   tmps
                                   args))
                (updates      (map (lambda (v t) (list 'set! v t))
                                   vars
                                   tmps)))
            `(let ,undefineds
               (let ,tmp-bindings
                 ,@updates
                 ,expr
                 ,@exprs))))))))

(define-syntax letrec %clean-letrec)

(define-syntax let*
  (let ((check-bindings check-bindings))
    (lambda (bindings expr . exprs)
      (letrec
        ((nest-let
           (lambda (b)
             (if (null? (cdr b))
                 `(let (,(car b))
                   ,@(cons expr exprs))
                 `(let (,(car b))
                   ,(nest-let (cdr b)))))))
        (check-bindings "let*" bindings #f)
        (if (null? bindings)
            `(let () ,expr ,@exprs)
            (nest-let bindings))))))

(define-syntax (case key . clauses)
  (letrec
    ((gen-clauses
       (lambda (k c*)
         (cond ((null? c*) '())
               ((or (not (pair? c*))
                    (not (pair? (car c*)))
                    (not (pair? (cdar c*))))
                 (error "case: syntax error" c*))
               ((null? (cdr c*))
                 (if (eq? 'else (caar c*))
                     `((else ,@(cdar c*)))
                     `(((memv ,k ',(caar c*)) ,@(cdar c*)))))
               (else
                 `(((memv ,k ',(caar c*)) ,@(cdar c*))
                     ,@(gen-clauses k (cdr c*))))))))
    (let ((k (gensym)))
      `(let ((,k ,key))
         (cond ,@(gen-clauses k clauses))))))

(define-syntax do
  (let ((check-bindings check-bindings)
        (split-bindings split-bindings))
    (lambda (var-clauses test . body)
      (if (or (not (pair? test))
              (not (list? (cdr test))))
          (error "do: invalid syntax" test))
      (check-bindings "do" var-clauses #t)
      (let ((loop (gensym))
            (var+init+step (split-bindings var-clauses)))
        (let ((v (car   var+init+step))
              (i (cadr  var+init+step))
              (s (caddr var+init+step)))
          `(letrec
             ((,loop
                (lambda ,v
                  (if ,(car test)
                      (begin ,@(cdr test))
                      (begin ,@body (,loop ,@s))))))
             (,loop ,@i)))))))

(define-syntax (delay expr)
  `(let ((value #f))
     (lambda ()
       (if value
           (car value)
           (let ((x ,expr))
             (if value
                 (car value)
                 (begin (set! value (list x))
                        (car value))))))))

(define (force x) (x))

;; Real number arithmetics

(define (expt x y)
  (letrec
    ((square
       (lambda (x) (* x x)))
     (expt2
       (lambda (x y)
         (cond ((zero? y) 1)
               ((even? y) (square (expt2 x (quotient y 2))))
               (else      (* x (square (expt2 x (quotient y 2)))))))))
    (cond ((negative? y)
            (/ (expt (exact->inexact x) (- y))))
          ((zero? x)
            (if (inexact? y)
                (if (positive? y)
                    0
                    (/ 1 0))
                (expt2 x y)))
          ((integer? y)
            (expt2 x y))
          (else
            (exp (* y (log x)))))))

(define (round x)
  (let ((x+ (+ 0.5 x)))
    (let ((rx (floor x+)))
      (if (and (odd? (inexact->exact rx))
               (= x+ rx))
          (- rx 1)
          rx))))

(define (exp x)
  (letrec
    ((e-series
       (lambda (x i x^y y! r last)
         (if (<= (abs (- last r)) *epsilon*)
             r
             (e-series x
                       (+ 1 i)
                       (* x^y x)
                       (* y! (+ 1 i))
                       (+ r (/ x^y y!))
                       r)))))
    (if (>= x 2.0)
        (let ((e^x/2 (exp (/ x 2))))
          (* e^x/2 e^x/2))
        (+ 1 (e-series x 1 x 1 0.0 1.0)))))

(define (log x)
  (letrec
    ((l-series6
       (lambda (x y x^y r last lim)
         (cond ((and lim (zero? lim))
                 r)
               ((<= (abs (- last r)) *epsilon*)
                 (* 2 r))
               (else
                 (l-series6 x
                           (+ 2 y)
                           (* x^y x x)
                           (+ r (/ x^y y))
                           r
                           (if lim (- lim 1) lim))))))
     (l-series
       (lambda (x y r last lim)
         (let ((x (/ (- x 1) (+ x 1))))
           (l-series6 x y x r last lim)))))
    (cond ((negative? x)
            (/ 1.0 0))
          ((< 0.1 x 5)
            (l-series x 1 0.0 1.0 #f))
          (else
            (let ((approx (l-series x 1 0.0 1.0 5)))
              (let ((a (/ x (exp approx))))
                (+ approx (log a))))))))

; auxiliary definitions for SIN, COS, TAN, ATAN

(define pi 3.141592653589793238462643383279502884197169399375105820974944)
(define pi/4  (/ pi 4))
(define pi/2  (/ pi 2))
(define 3pi/4 (+ pi/2 pi/4))
(define 3pi/2 (+ pi pi/2))
(define 5pi/4 (+ pi pi/4))
(define 7pi/4 (+ pi 3pi/4))
(define 2pi   (+ pi pi))

(define ->circle
  (let ((2pi 2pi))
    (lambda (x)
      (let* ((x+ (abs x))
             (d  (* 2pi (floor (/ x+ 2pi))))
             (x+ (- x+ d)))
         (if (negative? x)
             (- 2pi x+)
             x+)))))

; used by SIN, COS, ATAN, and EXP
(define (fact2 n m)
  (if (< n 2)
      m
      (let ((k (quotient n 2)))
        (* (fact2 k m)
           (fact2 (- n k) (+ m k))))))

(define sine-series 
  (let ((fact2 fact2))
    (lambda (x y r add last)
      (if (<= (abs (- last r)) *epsilon*)
          r
          (sine-series x
                       (+ 2 y)
                       ((if add + -) r (/ (expt x y)
                                          (fact2 y 1)))
                       (not add)
                       r)))))

(define cos
  (let ((->circle    ->circle)
        (sine-series sine-series)
        (pi          pi)
        (pi/2        pi/2)
        (3pi/2       3pi/2)
        (2pi         2pi))
    (lambda (x)
      (let ((x (->circle x)))
        (cond ((= 0     x)       (if (inexact? x) 1.0 1))
              ((= pi/2  x)        0.0)
              ((= pi    x)       -1.0)
              ((= 3pi/2 x)        0.0)
              ((<= 0    x pi/2)  (sine-series    x         2 1.0 #f 0))
              ((<= pi/2 x pi)    (- (sine-series (- pi x)  2 1.0 #f 0)))
              ((<= pi   x 3pi/2) (- (sine-series (- x pi)  2 1.0 #f 0)))
              (else              (sine-series    (- 2pi x) 2 1.0 #f 0)))))))

(define sin
  (let ((->circle    ->circle)
        (sine-series sine-series)
        (pi          pi)
        (pi/2        pi/2)
        (3pi/2       3pi/2)
        (2pi         2pi))
    (lambda (x)
      (let ((x (->circle x)))
        (cond ((= 0     x) (if (inexact? x) 0.0 0))
              ((= pi/2  x)  1.0)
              ((= pi    x)  0.0)
              ((= 3pi/2 x) -1.0)
              (else (let ((z (cond ((<= 0    x  pi/2) x)
                                   ((<= pi/2 x  pi)   (- pi x))
                                   ((<= pi   x 3pi/2) (- x pi))
                                   (else              (- 2pi x)))))
                      (if (> x pi)
                          (- (sine-series z 3 z #f 0))
                          (sine-series z 3 z #f 0)))))))))

(define tan
  (let ((->circle ->circle)
        (pi       pi)
        (pi/4     pi/4)
        (3pi/4    3pi/4)
        (5pi/4    5pi/4)
        (7pi/4    7pi/4))
    (lambda (x)
      (let ((x (->circle x)))
        (cond ((or (= x 0)     (= x  pi))   (if (inexact? x) 0.0 0))
              ((or (= x  pi/4) (= x 5pi/4))  1.0)
              ((or (= x 3pi/4) (= x 7pi/4)) -1.0)
              (else                         (/ (sin x) (cos x))))))))

(define atan
  (let ((pi/2 pi/2))
    (letrec
      ((at-series
         (lambda (x y r last)
           (if (<= (abs (- last r)) *epsilon*)
               r
               (at-series x
                          (+ 1 y)
                          (+ r (* (/ (* (expt 2 (+ y y))
                                        (expt (fact2 y 1) 2))
                                     (fact2 (+ y y 1) 1))
                                  (/ (expt x (+ y y 1))
                                     (expt (+ 1 (* x x))
                                           (+ 1 y)))))
                          r)))))
      (lambda (x)
        (cond ((negative? x)
                (- (atan (- x))))
              ((> x 1)
                (- pi/2 (atan (/ x))))
              (else
                (at-series x 0.0 0 1)))))))

(define (asin x)
  (cond ((= 1 x)
          (* 2 (atan x)))
        ((negative? x)
          (- (asin (- x))))
        (else
          (atan (/ x (sqrt (- 1 (* x x))))))))

(define acos
  (let ((pi   pi)
        (pi/2 pi/2))
    (lambda (x)
      (cond ((= -1 x) pi)
            ((=  1 x) 0)
            (else (- pi/2 (asin x)))))))

(define (sqrt square)
  (letrec
    ((sqrt2
       (lambda (x last)
          (if (<= (abs (- last x)) *epsilon*)
              x
              (sqrt2 (/ (+ x (/ square x)) 2)
                     x)))))
    (if (negative? square)
        (error "sqrt: negative argument" square)
        (sqrt2 square 0))))

; Used by NUMBER->STRING and STRING->NUMBER
(define (number-of-digits n r)
    (if (zero? n)
        (if (zero? r) 1 r)
        (number-of-digits (quotient n 10) (+ 1 r))))

(define number->string
  (let ((number-of-digits number-of-digits))
    (lambda (n . radix)
      (letrec
        ((digits
           (list->vector
             (string->list "0123456789abcdefghijklmnopqrstuvwxyz")))
         (conv
           (lambda (n rdx res)
             (if (zero? n)
                 (if (null? res) '(#\0) res)
                 (conv (quotient n rdx)
                       rdx
                       (cons (vector-ref digits (remainder n rdx))
                             res)))))
         (conv-int
           (lambda (n rdx)
             (if (negative? n)
                 (list->string (cons #\- (conv (abs n) rdx '())))
                 (list->string (conv n rdx '())))))
         (conv-sci-real
           (lambda (m e)
             (let ((m-str (conv-int m 10))
                   (e-str (conv-int e 10))
                   (i     (if (negative? m) 2 1)))
               (let ((k (string-length m-str)))
                 (string-append (substring m-str 0 i)
                                "."
                                (if (= k i) "0" (substring m-str i k))
                                "e"
                                (if (>= e 0) "+" "")
                                e-str)))))
         (zeroes
           (lambda (n)
             (let loop ((n n)
                        (z '()))
               (if (positive? n)
                   (loop (- n 1) (cons #\0 z))
                   (list->string z)))))
         (conv-expanded-real
           (lambda (n expn digits)
             (let ((m      (abs n))
                   (offset (+ expn digits)))
               (string-append
                 (if (negative? n) "-" "")
                 (cond ((negative? offset) "0.")
                       ((zero? offset)     "0")
                       (else               ""))
                 (zeroes (- offset))
                 (let ((m-str (conv-int m 10)))
                   (if (<= 0 offset digits)
                       (string-append (substring m-str 0 offset)
                                      "."
                                      (substring m-str offset digits)
                                      (if (= offset digits) "0" ""))
                       m-str))
                 (if (> offset digits)
                     (string-append (zeroes (- offset digits)) ".0")
                     "")))))
         (conv-real
           (lambda (n)
             (let ((m (mantissa n))
                   (e (exponent n)))
               (let ((d (number-of-digits m 0)))
                 (if (< -4 (+ e d) 10)
                     (conv-expanded-real m e d)
                     (conv-sci-real m (+ e d -1)))))))
         (get-radix
           (lambda ()
             (cond ((null? radix) 10)
                   ((<= 2 (car radix) 36) (car radix))
                   (else (error "number->string: invalid radix"
                                (car radix)))))))
        (let ((r (get-radix)))
          (cond ((not (or (exact? n)
                          (= 10 r)))
                  (error "number->string: real number needs a radix of 10" n))
                ((exact? n)
                  (conv-int n r))
                (else
                  (conv-real n))))))))

(define string->number
  (let ((number-of-digits number-of-digits)
        (make-inexact #f)
        (make-exact   #f))
    (lambda (str . radix)
      (letrec
        ((digits
           (string->list "0123456789abcdefghijklmnopqrstuvwxyz"))
         (value-of-digit
           (lambda (x)
             (letrec
               ((v (lambda (x d n)
                     (cond ((null? d) 36)
                           ((char=? (car d) x) n)
                           (else (v x (cdr d) (+ n 1)))))))
               (v (char-downcase x) digits 0))))
         (exponent-mark
           (lambda (c)
             (memv c '(#\d #\D #\e #\E #\f #\F #\l #\L #\s #\S))))
         (make-result cons)
         (value       car)
         (rest        cdr)
         (FAILED      '(#f . #f))
         (failed? (lambda (res)
                    (eq? #f (cdr res))))
         (ok?     (lambda (res)
                    (not (eq? #f (cdr res)))))
         (conv3
           (lambda (lst val rdx)
             (if (null? lst)
                 (make-result val '())
                 (let ((dval (value-of-digit (car lst))))
                   (if (< dval rdx)
                       (conv3 (cdr lst)
                              (+ (value-of-digit (car lst))
                                 (* val rdx))
                              rdx)
                       (make-result val lst))))))
         (conv
           (lambda (lst rdx)
             (if (null? lst)
                 FAILED
                 (conv3 lst 0 rdx))))
         (conv-int
           (lambda (lst rdx)
             (cond ((null? lst)
                     FAILED)
                   ((char=? (car lst) #\+)
                     (conv (cdr lst) rdx))
                   ((char=? (car lst) #\-)
                     (let ((r (conv (cdr lst) rdx)))
                       (if (ok? r)
                           (make-result (- (value r)) (rest r))
                           FAILED)))
                   (else
                     (conv lst rdx)))))
         (make-fract
           (lambda (x)
             (let ((d (number-of-digits x -1)))  ; 123 --> 0.123
               (- (/ x (expt 10.0 d)) 1.0))))
         (make-real
           (lambda (int fract expn)
             (let ((v (* (+ 0.0 (abs int) (make-fract fract))
                         (expt 10.0 expn))))
               (if (negative? int) (- v) v))))
         (conv-exponent
           (lambda (int fract lst)
             (if (null? lst)
                 FAILED
                 (let ((exp-part (conv-int lst 10)))
                   (if (failed? exp-part)
                       FAILED
                       (make-result (make-real int fract (value exp-part))
                                    (rest exp-part)))))))
         (conv-decimals
           (lambda (int lst)
             (cond ((null? lst)
                     (make-result (exact->inexact int) '()))  ; trailing #\.
                   ((exponent-mark (car lst))
                     (conv-exponent int 10 (cdr lst)))
                   (else
                     (let ((fract-part (conv3 lst 1 10)))
                       (if (null? (rest fract-part))
                           (make-result (make-real int (value fract-part) 0)
                                        '())
                           (if (exponent-mark (car (rest fract-part)))
                               (conv-exponent int
                                              (value fract-part)
                                              (cdr (rest fract-part)))
                               FAILED)))))))
         (assert-radix-ten
           (lambda (rdx)
             (cond ((= 10 rdx))
                   ((null? radix) #f)
                   (else
                     (error (string-append "string->number: real number"
                                           " needs a radix of 10"))))))
         (mantissa?
           (lambda (x)
             (cond ((null? x)               #f)
                   ((char-numeric? (car x)) #t)
                   ((exponent-mark (car x)) #f)
                   (else (mantissa? (cdr x))))))
         (conv-real
           (lambda (lst rdx)
             (let ((int-part (conv-int lst rdx)))
               (cond ((failed? int-part)
                       FAILED)
                     ((and (zero? (value int-part))  ; "" or "e"
                           (not (mantissa? lst)))
                       FAILED)
                     ((null? (rest int-part))
                       int-part)
                     ((exponent-mark (car (rest int-part)))
                       (assert-radix-ten rdx)
                       (conv-exponent (value int-part)
                                      10
                                      (cdr (rest int-part))))
                     ((char=? #\. (car (rest int-part)))
                       (assert-radix-ten rdx)
                       (conv-decimals (value int-part)
                                      (cdr (rest int-part))))
                     (else
                       FAILED)))))
         (replace-inexact-digits!
           (lambda (a)
             (cond ((null? a))
                   ((char=? #\# (car a))
                     (set-car! a #\5)
                     (set! make-inexact #t)
                     (replace-inexact-digits! (cdr a)))
                   (else
                     (replace-inexact-digits! (cdr a))))))
         (get-radix
           (lambda ()
             (cond ((null? radix) 10)
                   ((<= 2 (car radix) 36) (car radix))
                   (else (error "string->number: invalid radix"
                                (car radix)))))))
        (set! make-inexact #f)
        (set! make-exact #f)
        (let ((radix   (get-radix))
              (lst     (string->list str)))
          (if (and (> (string-length str) 1)
                   (char=? #\# (car lst)))
              (let ((mod (cadr lst)))
                (set! lst (cddr lst))
                (cond ((char=? mod #\d))
                      ((char=? mod #\e) (set! make-exact #t))
                      ((char=? mod #\i) (set! make-inexact #t))
                      ((char=? mod #\b) (set! radix 2))
                      ((char=? mod #\o) (set! radix 8))
                      ((char=? mod #\x) (set! radix 16))
                      (else             (set! lst '())))))
          (if (or (null? lst)
                  (memv (car lst) '(#\+ #\- #\.))
                  (char-numeric? (car lst)))
              (replace-inexact-digits! lst))
          (let ((r (cond ((null? lst)
                           FAILED)
                         ((char=? #\- (car lst))
                           (conv-real (cdr lst) radix))
                         (else
                           (conv-real lst radix)))))
            (if (null? (rest r))
                (let ((v (if (char=? #\- (car lst))
                             (- (value r))
                             (value r))))
                  (cond (make-inexact
                          (exact->inexact v))
                        (make-exact
                          (if (integer? v)
                              (inexact->exact v)
                              #f))
                        (else
                          v)))
                #f)))))))

;; Utilities

(define (print . x*)
  (letrec
    ((p (lambda (x* first)
          (cond ((not (null? x*))
                  (if (not first) (write-char #\space))
                  (write (car x*))
                  (p (cdr x*) #f))))))
    (p x* #t)
    (newline)))

(define (locate-file file)
  (letrec
    ((split
       (lambda (s)
         (let loop ((in  (string->list s))
                    (tmp '())
                    (out '()))
           (cond ((null? in)
                   (if (null? tmp)
                       out
                       (reverse (cons (list->string (reverse tmp))
                                       out))))
                 ((char=? #\: (car in))
                   (loop (cdr in)
                         '()
                         (cons (list->string (reverse tmp))
                               out)))
                 (else
                   (loop (cdr in)
                         (cons (car in) tmp)
                         out)))))))
    (let loop ((path (split *library-path*)))
      (and (not (null? path))
           (let ((full-path (string-append (car path) "/" file)))
             (if (file-exists? full-path)
                 full-path
                 (loop (cdr path))))))))

(define load-from-library 
  (let ((locate-file locate-file))
    (lambda (file)
      (let ((full-path (locate-file file))
            (do-load (lambda (file)
                       (begin (if (not *loading*)
                                  (begin (display "; loading from ")
                                         (display file)
                                         (newline)))
                              (load file)))))
        (if full-path
            (do-load full-path)
            (let ((full-path (locate-file (string-append file ".scm"))))
              (if full-path
                  (do-load full-path)
                  (error "cannot locate file" file))))))))

(define-syntax (require-extension . x*)
  (do ((x* x* (cdr x*))
       (na '()))
      ((null? x*)
        (if (not (null? na))
            (error "extension(s) required, but not compiled-in"
                   (reverse! na))))
    (if (not (memq (car x*) *extensions*))
        (set! na (cons (car x*) na)))))

