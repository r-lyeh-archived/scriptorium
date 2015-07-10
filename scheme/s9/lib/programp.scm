; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (program? object)  ==>  boolean
;
; (load-from-library "programp.scm")
;
; Return #T, if OBJECT is a syntactically correct Scheme program.
; This program does not implement all of R4RS. Caveat utilitor.
;
; Example:   (program? '(let ((x 1)) (cons x x)))  ==>  #t

(load-from-library "for-all.scm")

(define (program? x)

  (define INF #f)

  (define (list-of-programs? x)
    (for-all program? x))

  (define (of-length? min max x)
    (let ((k (length x)))
      (or (and (not max)
               (<= min k))
          (<= min k max))))

  (define (argument-list? x)
    (or (symbol? x)
        (null? x)
        (and (pair? x)
             (symbol? (car x))
             (argument-list? (cdr x)))))

  (define (valid-and? x)
    (list-of-programs? (cdr x)))

  (define (valid-begin? x)
    (list-of-programs? (cdr x)))

  (define (valid-case? x)
    (and (of-length? 2 INF x)
         (program? (cadr x))
         (for-all (lambda (x)
                    (and (list? x)
                         (or (list? (car x))
                             (eq? 'else (car x)))
                         (for-all program? (cdr x))))
                  (cddr x))))

  (define (valid-cond? x)
    (and (of-length? 2 INF x)
         (for-all (lambda (x)
                    (and (list? x)
                         (for-all program? x)))
                  (cdr x))))

  (define (valid-define? x)
    (or (and (of-length? 3 INF x)
             (pair? (cadr x))
             (argument-list? (cadr x))
             (for-all program? (cddr x)))
        (and (of-length? 3 3 x)
             (symbol? (cadr x))
             (program? (caddr x)))))

  (define (lambda-expression? x)
    (and (list? x)
         (not (null? x))
         (eq? 'lambda (car x))
         (valid-lambda? x)))

  ; Not in R4RS
  (define (valid-alt-define-syntax? x)
    (and (of-length? 3 3 x)
         (or (and (pair? (cadr x))
                  (argument-list? (cadr x))
                  (program? (caddr x)))
             (and (symbol? (cadr x))
                  (program? (caddr x))))))

  (define (valid-define-syntax? x)
    (or (valid-alt-define-syntax? x)
        (and (of-length? 3 3 x)
             (symbol? (cadr x))
             (list? (caddr x))
             (let ((x (caddr x)))
               (and (not (null? x))
                    (eq? 'syntax-rules (car x))
                    (list? (cadr x))
                    (for-all symbol? (cadr x))
                    (let ((clauses (cddr x)))
                      (for-all (lambda (x)
                                 (and (list? x)
                                      (of-length? 2 2 x)
                                      (pair? (car x))
                                      (symbol? (caar x))
                                      (pair? (cdr x))))
                               clauses)))))))

  (define (valid-delay? x)
    (and (of-length? 1 1 (cdr x))
         (program? (cadr x))))

  (define (valid-do? x)
    (and (of-length? 2 INF (cdr x))
         (list? (cadr x))
         (for-all (lambda (x)
                    (and (list? x)
                         (of-length? 2 3 x)
                         (symbol? (car x))
                         (for-all program? (cdr x))))
                  (cadr x))
         (list? (caddr x))
         (of-length? 1 INF (caddr x))
         (for-all program? (caddr x))
         (for-all program? (cdddr x))))

  (define (valid-if? x)
    (and (of-length? 2 3 (cdr x))
         (list-of-programs? (cdr x))))

  (define (valid-lambda? x)
    (and (of-length? 2 INF (cdr x))
         (argument-list? (cadr x))
         (for-all program? (cddr x))))

  (define (valid-let/*/rec? x named)
    (and (of-length? 2 INF (cdr x))
         (let* ((name (and named
                           (of-length? 3 INF (cdr x))
                           (symbol? (cadr x))))
                (bind (if name
                          (caddr x)
                          (cadr x)))
                (body (if name
                          (cdddr x)
                          (cddr x))))
           (and (for-all (lambda (x)
                           (and (list? x)
                                (of-length? 2 2 x)
                                (symbol? (car x))
                                (program? (cadr x))))
                         bind)
                (for-all program? body)))))

  (define (valid-or? x)
    (list-of-programs? (cdr x)))

  (define (valid-quote? x)
    (of-length? 1 1 (cdr x)))

  (define (valid-set!? x)
    (and (of-length? 2 2 (cdr x))
         (symbol? (cadr x))
         (program? (caddr x))))

  (define (expression? x)
    (and (list? x)
         (not (null? x))
         (case (car x)
               ((and)           (valid-and? x))
               ((begin)         (valid-begin? x))
               ((case)          (valid-case? x))
               ((cond)          (valid-cond? x))
               ((define)        (valid-define? x))
               ((define-syntax) (valid-define-syntax? x))
               ((delay)         (valid-delay? x))
               ((do)            (valid-do? x))
               ((if)            (valid-if? x))
               ((lambda)        (valid-lambda? x))
               ((let)           (valid-let/*/rec? x #t))
               ((let* letrec)   (valid-let/*/rec? x #f))
               ((quote)         (valid-quote? x))
               ((quasiquote)    (valid-quote? x))
               ((or)            (valid-or? x))
               ((set!)          (valid-set!? x))
               (else            (and (or (expression? (car x))
                                         (symbol? (car x)))
                                     (for-all program? (cdr x)))))))

  (or (symbol? x)
      (boolean? x)
      (number? x)
      (char? x)
      (string? x)
      (procedure? x)     ; not formally correct, but useful
      (expression? x)))
