; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (package <name> <option> ... <body>)  ==>  unspecific
;
; PACKAGE packages the definitions in its <body> in such a
; way that they are not visible outside of its body, i.e.
; the scope of each definition is the <body> of the package.
; There must be at least one definition in <body>.
;
; There may be any number of <option>s preceding the definitions
; in the <body> of PACKAGE. All options are lists with a symbol
; beginning with a #\: in their first positions. The following
; options exist:
;
; (:EXPORT symbol ...) lists the symbols to exported from the
; package. When the name X of a definition occurs in :EXPORTS, a
; symbol with the name <name>:X will be made visible outside of
; the package. That symbol will be bound to the same value as X
; inside of the package.
;
; (:IMPORT symbol ...) lists the symbols to imported into the
; package. A symbol that is being imported into a package may be
; redefined later outside of the package without affecting its
; binding inside of the package.
;
; (:MAKE-ALIASES) will create an alias named X for each exported
; symbol named <name>:X, i.e. it will allow to refer to an object
; defined in a package by the same name inside and outside of the
; package.
;
; Example:   (begin
;              (package bar
;                (:export foo2 foo3)
;                (:make-aliases)
;                (define (foo-maker n x)
;                  (if (zero? n)
;                      (lambda ()
;                        x)
;                      (foo-maker
;                        (- n 1)
;                        (cons n x))))
;                (define foo2 (foo-maker 2 '()))
;                (define foo3 (foo-maker 3 '())))
;              (list (bar:foo2) (foo3)))           ==>  ((1 2) (1 2 3))

(load-from-library "filter.scm")
(load-from-library "for-all.scm")
(load-from-library "and-letstar.scm")
(load-from-library "letrecstar.scm")
(load-from-library "setters.scm")

(define-syntax (package %name . %body)
  (if (not (symbol? %name))
      (error "package: expected name, got" %name))
  (letrec
    ((options '())
     (imports '())
     (exports '())
     (for-all-i
       (lambda (p x)
         (cond ((null? x)
                 #t)
               ((pair? x)
                 (and (p (car x))
                      (for-all-i p (cdr x))))
               (else
                 (p x)))))
     (decompose-definition
       (lambda (x)
         (and-let* ((_    (pair? x))
                    (_    (eq? 'define (car x)))
                    (_    (pair? (cdr x)))
                    (body (cddr x))
                    (_    (pair? body))
                    (head (cadr x))
                    (_    (or (symbol? head)
                              (and (pair? head)
                                   (for-all-i symbol? head))))
                    (_    (or (not (symbol? head))
                              (= 1 (length body)))))
           (if (symbol? head)
               (list head (car body))
               (list (car head) `(lambda ,(cdr head) ,@body))))))
     (external
       (lambda (x)
         (string->symbol
           (string-append (symbol->string %name)
                          ":"
                          (symbol->string x)))))
     (make-def
       (lambda (name)
         `(define ,(external name) #f)))
     (make-set
       (lambda (name)
         `(set! ,(external name) ,name)))
     (make-alias
       (lambda (name)
         `(define ,name ,(external name))))
     (make-import
       (lambda (name)
         `(,name ,name)))
     (option-symbol?
       (lambda (x)
         (and (symbol? x)
              (char=? #\: (string-ref (symbol->string x) 0)))))
     (assert-list-of-symbols
       (lambda (who x)
         (if (not (for-all symbol? x))
             (error (string-append "package: "
                                   who
                                   ": expected list of symbols, got")
                    x)))))
    (let parse-opts ((opts %body))
      (cond ((null? opts)
              (error "package: missing body"))
            ((and (pair? opts)
                  (pair? (car opts))
                  (option-symbol? (caar opts)))
              (case (caar opts)
                    ((:make-aliases)
                       (push! ':make-aliases options))
                    ((:import)
                       (assert-list-of-symbols ":import" (cdar opts))
                       (set! imports (cdar opts)))
                    ((:export)
                       (assert-list-of-symbols ":export" (cdar opts))
                       (set! exports (cdar opts))))
              (parse-opts (cdr opts)))
            (else
              (set! %body opts))))
    (let loop ((body %body)
               (defs '()))
      (if (null? body)
          (let ((names (filter (lambda (x)
                                 (or (null? exports)
                                     (memq x exports)))
                               (reverse! (map car defs)))))
            `(begin ,@(map make-def names)
                    (let ,(map make-import imports)
                      (letrec*
                        ,(reverse! defs)
                        ,@(map make-set names)))
                    ,@(if (memq ':make-aliases options)
                          (map make-alias names)
                          '())))
          (let ((name/val (decompose-definition (car body))))
            (cond ((not name/val)
                    (error "package: expected definition, got"
                           (car body)))
                  (else
                    (loop (cdr body) (cons name/val defs)))))))))
