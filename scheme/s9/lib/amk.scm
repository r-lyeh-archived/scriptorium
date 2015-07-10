; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009,2012
; Placed in the Public Domain
;
; (run* (variable) query)  ==>  list
; (run* () query)          ==>  list
;
; (load-from-library "amk.scm")
;
; Run the given AMK (Another Micro Kanren) query and return its
; result, if any. See the book "Logic Programming in Scheme"[1]
; for an introduction to AMK. If a variable is given, return all
; values for that variable that satisfy the query.
; 
; [1] http://www.lulu.com/shop/nils-m-holm/logic-programming-in-scheme/\
;     paperback/product-18693432.html
;
; Example:   (run* (q) (fresh (h t) 
;                        (== q (list h t))
;                        (appendo h t '(1 2 3))))
;              ==>  ((() (1 2 3)) ((1) (2 3)) ((1 2) (3)) ((1 2 3) ()))

(load-from-library "syntax-rules.scm")

; ----- Core -----

(define (fail x)
  '())

(define (succeed x)
  (list x))

(define failed? null?)

(define (var x)
  (cons '? x))

(define (var? x)
  (and (pair? x)
       (eq? (car x) '?)))

(define (_)
  (var '_))

(define empty-s '())

(define (ext-s x v s)
  (cons (cons x v) s))

(define (walk x s)
  (if (not (var? x))
      x
      (let ((v (assq x s)))
        (if v
            (walk (cdr v) s)
            x))))

(define (atom? x)
  (not (pair? x)))

(define (unify x y s)
  (let ((x (walk x s))
        (y (walk y s)))
    (cond ((eqv? x y) s)
          ((var? x) (ext-s x y s))
          ((var? y) (ext-s y x s))
          ((or (atom? x)
               (atom? y)) #f)
          (else
            (let ((s (unify (car x) (car y) s)))
              (and s (unify (cdr x) (cdr y) s)))))))

(define (== x y)
  (lambda (s)
    (let ((s2 (unify x y s)))
      (if s2
          (succeed s2)
          (fail s)))))

(define (any* . g*)
  (lambda (s)
    (letrec
      ((any*
         (lambda g*
           (if (null? g*)
               (fail s)
               (append ((car g*) s)
                       (apply any* (cdr g*)))))))
      (apply any* g*))))

(define-syntax any
  (syntax-rules ()
    ((_) fail)
    ((_ g ...)
       (any* (lambda (s) (g s)) ...))))

(define (all . g*)
  (lambda (s)
    (letrec
      ((all
         (lambda (g* s*)
           (if (null? g*)
               s*
               (all (cdr g*)
                    (apply append
                           (map (car g*) s*)))))))
      (all g* (succeed s)))))

(define (one* . g*)
  (lambda (s)
    (letrec
      ((one*
         (lambda g*
           (if (null? g*)
               (fail s)
               (let ((out ((car g*) s)))
                 (if (failed? out)
                     (apply one* (cdr g*))
                     out))))))
      (apply one* g*))))

(define-syntax one
  (syntax-rules ()
    ((_) fail)
    ((_ g ...)
       (one* (lambda (s) (g s)) ...))))

(define (neg g)
  (lambda (s)
    (let ((out (g s)))
      (if (failed? out)
          (succeed s)
          (fail s)))))

(define-syntax fresh
  (syntax-rules ()
    ((_ () g ...)
       (let () (all g ...)))
    ((_ (v ...) g ...)
       (let ((v (var 'v)) ...)
         (all g ...)))))

(define (occurs? x y s)
  (let ((v (walk y s)))
    (cond ((var? y) (eq? x y))
          ((var? v) (eq? x v))
          ((atom? v) #f)
          (else (or (occurs? x (car v) s)
                    (occurs? x (cdr v) s))))))

(define (circular? x s)
  (let ((v (walk x s)))
    (and (not (eq? x v))
         (occurs? x (walk x s) s))))

(define (walk* x s)
  (let ((x (walk x s)))
    (cond ((var? x) x)
          ((atom? x) x)
          (else (cons (walk* (car x) s)
                      (walk* (cdr x) s))))))

(define *failure* (var 'failure))

(define (s-walk* x s)
  (cond ((circular? x s) *failure*)
        ((eq? x (walk x s)) empty-s)
        (else (walk* x s))))

(define (reify-name n)
  (string->symbol
    (string-append "_." (number->string n))))

(define (reify v)
  (letrec
    ((reify-s
       (lambda (v s)
         (let ((v (walk v s)))
           (cond ((var? v)
                   (ext-s v (reify-name (length s)) s))
                 ((atom? v)
                   s)
                 (else
                   (reify-s (cdr v)
                            (reify-s (car v) s))))))))
    (reify-s v empty-s)))

(define (propagate-failure s)
  (if (occurs? *failure* s s)
      '()
      s))

(define (collapse-null x)
  (letrec
    ((all-null?
       (lambda (x)
         (or (null? x)
             (and (null? (car x))
                  (all-null? (cdr x)))))))
    (cond ((null? x)     x)
          ((all-null? x) (succeed '()))
          (else          x))))

(define (query x . g)
  (propagate-failure
    (map (lambda (s)
           (s-walk* x (append s (reify (s-walk* x s)))))
         ((apply all g) empty-s))))

(define-syntax run*
  (syntax-rules ()
    ((_ () g ...)
       (collapse-null
         (query #f g ...)))
    ((_ (v) g ...)
       (let ((v (var 'v)))
         (query v g ...)))))

; ----- Tools -----

(define (conso a d p) (== (cons a d) p))

(define (caro p a) (conso a (_) p))

(define (cdro p d) (conso (_) d p))

(define (pairo p) (conso (_) (_) p))

(define (eqo x y) (== x y))

(define (nullo a) (eqo a '()))

(define (memo x l)
  (fresh (d)
    (any (caro l x)
         (all (cdro l d)
              (memo x d)))))

(define (membero x l r)
  (fresh (d)
    (any (all (caro l x)
              (== l r))
         (all (cdro l d)
              (membero x d r)))))

(define (reverseo x r)
  (fresh (d)
    (any (all (cdro x d)
              (reverseo d r))
         (all (caro x r)))))

(define (appendo x y r)
  (any (all (== x '())
            (== y r))
       (fresh (h t tr)
         (conso h t x)
         (conso h tr r)
         (appendo t y tr))))

(define (choice x a)
  (if (null? a)
      fail
      (any (== x (car a))
           (choice x (cdr a)))))

; ----- Debugging Helpers -----

(define (printo . x)
  (lambda (s)
    (display (walk (car x) s))
    (for-each (lambda (x)
                (write-char #\space)
                (display (walk x s)))
              (cdr x))
    (newline)
    (succeed s)))

(define (print*o . x)
  (lambda (s)
    (display (walk* (car x) s))
    (for-each (lambda (x)
                (write-char #\space)
                (display (walk* x s)))
              (cdr x))
    (newline)
    (succeed s)))

; ----- Numeric Tools -----

(define-macro (eql vv x)
  (letrec
    ((walk-vars
       (lambda (x)
         (cond ((null? x)   '())
               ((pair? x)   (cons (car x)
                                  (map walk-vars 
                                       (cdr x))))
               ((symbol? x) `(walk ,x s))
               (else        x)))))
    `(lambda (s)
       (let ((v (walk ,vv s)))
         (cond ((var? v)
                 (succeed
                   (ext-s v ,(walk-vars x) s)))
               (else
                 (if (eqv? v ,(walk-vars x))
                     (succeed s)
                     (fail s))))))))

(define-macro (=p a b)  `(eql #t (=  ,a ,b)))
(define-macro (<p a b)  `(eql #t (<  ,a ,b)))
(define-macro (>p a b)  `(eql #t (>  ,a ,b)))
(define-macro (<=p a b) `(eql #t (<= ,a ,b)))
(define-macro (>=p a b) `(eql #t (>= ,a ,b)))
(define-macro (/=p a b) `(eql #F (=  ,a ,b)))

(define (range x l h)
  (if (> l h)
      fail
      (any (== x l)
           (range x (+ 1 l) h))))

; ----- Hard Cutting -----

(define *cut* #f)

(define (try* . g*)
  (lambda (s)
    (letrec
      ((try*
         (lambda g*
           (if (null? g*)
               (fail s)
               (append ((car g*) s)
                       (apply try* (cdr g*)))))))
      (call-with-current-continuation
        (lambda (k)
          (cond (*cut*
                  (apply try* g*))
                (else
                  (set! *cut* k)
                  (let ((r (apply try* g*)))
                    (set! *cut* #f)
                    r))))))))

(define-syntax try
  (syntax-rules ()
    ((_) fail)
    ((_ g ...)
       (try* (lambda (s) (g s)) ...))))

(define (cut)
  (lambda (s)
    (let ((cut *cut*))
      (set! *cut* #f)
      (cut (succeed s)))))
