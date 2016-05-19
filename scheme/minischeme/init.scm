; This is a init file for Mini-Scheme.

;; fake pre R^3 boolean values
(define nil #f)
(define t #t)

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define call/cc call-with-current-continuation)

(define (list . x) x)

(define (map proc list)
    (if (pair? list)
        (cons (proc (car list)) (map proc (cdr list)))))

(define (for-each proc list)
    (if (pair? list)
        (begin (proc (car list)) (for-each proc (cdr list)))
        #t ))

(define (list-tail x k)
    (if (zero? k)
        x
        (list-tail (cdr x) (- k 1))))

(define (list-ref x k)
    (car (list-tail x k)))

(define (last-pair x)
    (if (pair? (cdr x))
        (last-pair (cdr x))
        x))

(define (head stream) (car stream))

(define (tail stream) (force (cdr stream)))

;; The following quasiquote macro is due to Eric S. Tiedemann.
;;   Copyright 1988 by Eric S. Tiedemann; all rights reserved.
;; 
;; --- If you don't use macro or quasiquote, cut below. ---

(macro 
 quasiquote 
 (lambda (l)
   (define (mcons f l r)
     (if (and (pair? r)
              (eq? (car r) 'quote)
              (eq? (car (cdr r)) (cdr f))
              (pair? l)
              (eq? (car l) 'quote)
              (eq? (car (cdr l)) (car f)))
         (list 'quote f)
         (list 'cons l r)))
   (define (mappend f l r)
     (if (or (null? (cdr f))
             (and (pair? r)
                  (eq? (car r) 'quote)
                  (eq? (car (cdr r)) '())))
         l
         (list 'append l r)))
   (define (foo level form)
     (cond ((not (pair? form)) (list 'quote form))
           ((eq? 'quasiquote (car form))
            (mcons form ''quasiquote (foo (+ level 1) (cdr form))))
           (#t (if (zero? level)
                   (cond ((eq? (car form) 'unquote) (car (cdr form)))
                         ((eq? (car form) 'unquote-splicing)
                          (error "Unquote-splicing wasn't in a list:" 
                                 form))
                         ((and (pair? (car form)) 
                               (eq? (car (car form)) 'unquote-splicing))
                          (mappend form (car (cdr (car form))) 
                                   (foo level (cdr form))))
                         (#t (mcons form (foo level (car form))
                                         (foo level (cdr form)))))
                   (cond ((eq? (car form) 'unquote) 
                          (mcons form ''unquote (foo (- level 1) 
                                                     (cdr form))))
                         ((eq? (car form) 'unquote-splicing)
                          (mcons form ''unquote-splicing
                                      (foo (- level 1) (cdr form))))
                         (#t (mcons form (foo level (car form))
                                         (foo level (cdr form)))))))))
   (foo 0 (car (cdr l)))))

;;;;; following part is written by a.k

;;;;	atom?
(define (atom? x)
  (not (pair? x)))

;;;;	memq
(define (memq obj lst)
  (cond
    ((null? lst) #f)
    ((eq? obj (car lst)) lst)
    (else (memq obj (cdr lst)))))

;;;;    equal?
(define (equal? x y)
  (if (pair? x)
    (and (pair? y)
         (equal? (car x) (car y))
         (equal? (cdr x) (cdr y)))
    (and (not (pair? y))
         (eqv? x y))))


;;;;	(do ((var init inc) ...) (endtest result ...) body ...)
;;
(macro do
  (lambda (do-macro)
    (apply (lambda (do vars endtest . body)
             (let ((do-loop (gensym)))
               `(letrec ((,do-loop
                           (lambda ,(map (lambda (x)
                                           (if (pair? x) (car x) x))
                                      `,vars)
                             (if ,(car endtest)
                               (begin ,@(cdr endtest))
                               (begin
                                 ,@body
                                 (,do-loop
                                   ,@(map (lambda (x)
                                            (cond
                                              ((not (pair? x)) x)
                                              ((< (length x) 3) (car x))
                                              (else (car (cdr (cdr x))))))
                                       `,vars)))))))
                  (,do-loop
                    ,@(map (lambda (x)
                             (if (and (pair? x) (cdr x))
                               (car (cdr x))
                               nil))
                        `,vars)))))
      do-macro)))

;;;;; following part is written by c.p

(define (list? x)
  (or (eq? x '())
      (and (pair? x)
           (list? (cdr x)))))
