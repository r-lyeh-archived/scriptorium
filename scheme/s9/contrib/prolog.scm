; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 1998-2009
; Placed in the Public Domain
;
; (prolog list1 list2)          ==>  list
; (new-database!)               ==>  unspecific
; (fact! list)                  ==>  unspecific
; (predicate! list1 list2 ...)  ==>  unspecific
; (query list)                  ==>  list
;
; (load-from-library "prolog.scm")
;
; This is a tiny PROLOG interpreter that is based on an even
; tinier PROLOG interpreter written in MACLISP by Ken Kahn.
;
; The PROLOG procedures takes a query LIST1 and a database
; LIST2 as arguments, attempts to prove LIST1 in LIST2, and
; returns the result(s).

; NEW-DATABASE! sets up a fresh PROLOG database (thereby
; deleting any existing one).
;
; FACT! adds a new fact to the database.
;
; PREDICATE! adds a predicate with the head LIST1 and the
; clauses LIST2 ... to the database.
;
; QUERY attempts to prove LIST1. It returns a list of results.
; An empty list indicates that LIST1 could not be proven.
;
; See "prolog-test.scm" for an example program.
;
; The following macros add some syntactic sugar for interactive
; use; they allows you to write, for instance, (! (man socrates))
; instead of (fact! '(man socrates)).
;
; (! fact)              ==>  unspecific
; (:- list1 list2 ...)  ==>  unspecific
; (? query)             ==>  unspecific
;
; The following special predicates are implemented in the
; interpreter: (== A B) returns a new environment if A can be
; unified with B, else NO. (Dif A B) returns NO if A can be
; unified with B, else YES (use only at the end of a clause!)
;
; Example:   (begin (! (man socrates))
;                   (:- (mortal ?x)
;                       (man ?x))
;                   (query '(mortal ?who)))  ==>  (((who . socrates)))

(load-from-library "syntax-rules.scm")
(load-from-library "hash-table.scm")

(define *prolog-database* '())

(define (prolog q db)

  (define empty-env '((())))

  (define top-scope "")

  (define true '(()))

  (define false '())

  (define (unique a)
    (letrec
      ((unique2
         (lambda (a r)
           (cond ((null? a)
                   (reverse! r))
                 ((member (car a) r)
                   (unique2 (cdr a) r))
                 (else
                   (unique2 (cdr a)
                            (cons (car a) r)))))))
      (unique2 a '())))

  (define (variable? x)
    (and (symbol? x)
         (char=? #\? (string-ref (symbol->string x) 0))))

  (define (internal? x)
    (and (symbol? x)
         (char=? #\: (string-ref (symbol->string x) 0))))

  (define (anonymous? x)
    (eq? '_ x))

  (define (extend n v env)
    (cons (cons n v) env))

  (define (new-scope env id)
    (cond ((variable? env)
            (string->symbol
              (string-append (symbol->string env) id)))
          ((pair? env)
            (cons (new-scope (car env) id)
                  (new-scope (cdr env) id)))
          (else
            env)))

  (define (new-env-id x)
    (string-append ";" x))

  (define (value-of x env)
    (if (variable? x)
        (cond ((assq x env)
                => (lambda (v)
                     (value-of (cdr v) env)))
              (else x))
        x))

  (define (unify x y env)
    (let ((x (value-of x env))
          (y (value-of y env)))
      (cond ((variable? x) (extend x y env))
            ((variable? y) (extend y x env))
            ((or (anonymous? x)
                 (anonymous? y))
              env)
            ((and (pair? x)
                  (pair? y))
              (let ((new (unify (car x) (car y) env)))
                (and new (unify (cdr x) (cdr y) new))))
            ((eq? x y) env)
            (else      #f))))

  (define (check-args g n)
    (if (not (= n (length g)))
        (error "wrong number of arguments" g)))

  (define (goal-unify rules goals env id result)
    (check-args (car goals) 3)
    (let* ((this-goal (car goals))
           (new-env (unify (cadr this-goal) (caddr this-goal) env)))
      (if new-env
          (let ((r (prove (cdr goals)
                          new-env
                          (new-env-id id))))
             (try-rules (cdr rules) goals env id (append result r)))
          (try-rules (cdr rules) goals env id result))))

  (define (goal-dif rules goals env id result)
    (check-args (car goals) 3)
    (let* ((this-goal (car goals))
           (new-env (unify (cadr this-goal) (caddr this-goal) env)))
      (if (not new-env)
          (let ((r (prove (cdr goals)
                          env
                          (new-env-id id))))
             (try-rules (cdr rules) goals env id (append result r)))
          false)))

  (define (goal* rules goals env id result)
    (let* ((this-rule (new-scope (car rules) id))
           (new-env (unify (car goals) (car this-rule) env)))
      (if new-env
          (let ((r (prove (append (cdr this-rule) (cdr goals))
                          new-env
                          (new-env-id id))))
             (try-rules (cdr rules) goals env id (append result r)))
          (try-rules (cdr rules) goals env id result))))

  (define (try-rules rules goals env id result)
    (if (null? rules)
        result
        (case (caar goals)
              ((==)  (goal-unify rules goals env id result))
              ((dif) (goal-dif   rules goals env id result))
              (else  (goal*      rules goals env id result)))))

  (define (list-env env)
    (letrec
      ((this-id  caar)
       (scope-id caddr)
       (top-level?
         (lambda (x)
           (not (memv #\; (string->list (symbol->string x))))))
       (var-name
         (lambda (x)
           (let* ((s (symbol->string x))
                  (k (string-length s)))
             (let loop ((i 0))
               (if (or (>= i k)
                       (char=? #\; (string-ref s i)))
                   (string->symbol (substring s 1 i))
                   (loop (+ 1 i)))))))
       (list-env2
         (lambda (e r)
           (cond ((null? (cdr e))
                   (list r))
                 ((top-level? (this-id e))
                   (list-env2 (cdr e)
                              (extend (var-name (this-id e))
                                      (value-of (this-id e) env)
                                      r)))
                 (else
                   (list-env2 (cdr e) r))))))
      (list-env2 env '())))

  ; version without memoization
  (define (prove goals env id)
    (if (null? goals)
        (list-env env)
        (try-rules db goals env id '())))

  (define proven (make-hash-table))

  (define (prove goals env id)
    (if (null? goals)
        (list-env env)
        (let* ((k (append goals env))
               (v (hash-table-ref proven k)))
          (if v
              (car v)
              (let ((v (try-rules db goals env id '())))
                (hash-table-set! proven k v)
                v)))))

  (define (any? p a)
    (cond ((null? a)   #f)
          ((p (car a)) #t)
          (else        (any? p (cdr a)))))

  (define (cleanup env)
    (apply append
           (map (lambda (frame)
                  (if (or (any? (lambda (x) (variable? (cdr x))) frame)
                          (any? (lambda (x) (internal? (cdr x))) frame))
                      '()
                      (list frame)))
                env)))

  (cleanup (unique (prove (new-scope q top-scope)
                          empty-env
                          (new-env-id top-scope)))))

(define (new-database!)
  (set! *prolog-database* '()))

(define (update! x)
  (set! *prolog-database*
        (cons x *prolog-database*)))

(define (fact! x)
  (let ((update! update!))
    (update! (list x))))

(define (predicate! head . clause*)
  (let ((update! update!))
    (update! (cons head clause*))))

(define (query . q)
  (prolog q (reverse *prolog-database*)))

(define (print-frames env)
  (cond ((equal? '(()) env)
          (display "yes")
          (newline))
        ((equal? '() env)
          (display "no")
          (newline))
        (else
          (for-each (lambda (frame)
                      (for-each (lambda (b)
                                  (display (car b))
                                  (display " = ")
                                  (display (cdr b))
                                  (display ";  "))
                                frame)
                      (newline))
                    env))))

(define-syntax !
  (syntax-rules ()
    ((_ token ...)
      (fact! 'token ...))))

(define-syntax :-
  (syntax-rules ()
    ((_ head clause clauses ...)
      (predicate! 'head 'clause 'clauses ...))))

(define-syntax ?
  (syntax-rules ()
    ((_ q ...)
      (print-frames (query 'q ...)))))
