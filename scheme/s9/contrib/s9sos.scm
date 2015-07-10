; Scheme 9 from Empty Space, Simple Object System
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (call-next-method)                              ==>  object
; (class-of object)                               ==>  class
; (class? object)                                 ==>  boolean
; (define-class <name> (<class> ...) <slot> ...)  ==>  unspecific
; (define-generic <name>)                         ==>  unspecific
; (define-method (<name> <argument> ...) <body>)  ==>  unspecific
; (initialize instance)                           ==>  object
; (instance? object)                              ==>  boolean
; (make-instance class <init-arg> ...)            ==>  instance
; (slot-ref instance symbol)                      ==>  object
; (slot-set! instance symbol object)              ==>  unspecific
;
; (load-from-library "sos.scm")
;
; S9SOS is a Scheme Object System that is similar to CLOS, but simpler.
; It has multiple inheritance, generic procedures, and a meta object
; protocol (MOP). If you know CLOS, you will probably find it too
; limited. If you do not know CLOS, you will probably find it too
; complex.
;
; The procedures and macros above form the user-level interface to
; S9SOS. For a more detailed description (including the MOP), see the
; "s9sos.txt" file.
;
; DEFINE-CLASS defines a new class named <name> that is a subclass
; of each specified <class>. Each <slot> specifies an instance
; variable of the new class. It can be a symbol, a list containing
; a symbol, or a list containing a symbol in the car part and any
; expression in the cdr part. In any case the symbol names the slot.
; When an expression is also given, it evaluates to the default value
; of the corresponding slot. Evaluation takes place when the new class
; is being defined.
;
; MAKE-INSTANCE creates a fresh instance of the specified class and
; returns it. Before returning the new instance, MAKE-INSTANCE will
; pass it to the generic procedure INITIALIZE, which can be used to
; perform more elaborate instance initialization.
; When passing some <init-arg>s to MAKE-INSTANCE, these will be used
; to initialize slots with dynamic values. Each <init-arg> consists
; of a slot followed by a value to be stored in that slot. <Init-arg>s
; are evaluated before INITIALIZE, so INITIALIZE can access the values
; stored by them.
;
; DEFINE-GENERIC defines a generic procedure with no methods. When
; the generic exists, all its methods will be removed.
;
; DEFINE-METHOD adds a method to a generic procedure. The generic
; procedure is selected by giving the same name to the generic and
; its methods. All methods must have the same number of arguments.
; Each argument may be a symbol or a list consisting of a symbol
; and a class:
;
;       (define-method (<generic-name> (x <type1>) (n <type2>)) ...)
;
; When a type is specified, the method will only be applied to
; arguments of the specified types (or less specific types from
; which the given type is derived). See "s9sos.txt" for details.
;
; Types may be user-defined classes or "built-in" types. Each
; built-in type corresponds to a Scheme type, e.g.: <pair> would
; represent a pair. When no type is specified, the type of an
; argument defaults to <type>, which matches any type.
;
; Methods, like procedures, may have a "rest" argument. Such an
; argument will not play any role in method dispatch.
;
; CALL-NEXT-METHOD calls the next-unspecific method that is
; applicable to the arguments passed to the method that is
; currently evaluating. Outside of methods, CALL-NEXT-METHOD
; is undefined.
;
; INITIALIZE is a generic procedure that is used to initialize
; instances dynamically. Whenever a new class is defined using
; DEFINE-CLASS, a new method covering that class is added to the
; INITIALIZE generic. Initialization code is added by redefining
; that method. By default INITIALIZE does nothing.
;
; CLASS? is a predicate returning #T if OBJECT is a class.
;
; INSTANCE? is a predicate returning #T if OBJECT is an instance.
;
; CLASS-OF returns the class of a given instance.
;
; SLOT-REF returns the value stored in the slot named SLOT of
; the given INSTANCE.
;
; SLOT-SET! stores the given OBJECT in the slot named SLOT of
; the given INSTANCE, thereby removing its prior value.
;
; Example:   (begin
;              (define-generic mul)
;
;              (define-method (mul (x <integer>) (y <integer>))
;                (* x y))
;
;              (define-method (mul (x <integer>) (a <pair>))
;                (map (lambda (i) (* i x)) a))
;
;              (define-method (mul (a <pair>) (x <integer>))
;                (map (lambda (i) (* i x)) a))
;
;              (define-method (mul (a <pair>) (b <pair>))
;                (map * a b))
;
;              (list (mul 5 7)
;                    (mul 2 '(1 2 3))
;                    (mul '(1 2 3) 2)
;                    (mul '(1 2 3) '(4 5 6))))  ==>  (35
;                                                     (2 4 6)
;                                                     (2 4 6)
;                                                     (4 10 18))
;
;            ; Don't do this! Generic application takes ages.
;            (begin
;              (define-generic len)
;              (define-method (len (x <null>)) 0)
;              (define-method (len (x <pair>))
;                (+ 1 (len (cdr x))))
;              (len '(1 2 3 4 5)))                 ==>  5

(load-from-library "hof.scm")
(load-from-library "and-letstar.scm")
(load-from-library "hash-table.scm")
(load-from-library "iota.scm")
(load-from-library "memp.scm")
(load-from-library "duplicates.scm")
(load-from-library "list-to-set.scm")
(load-from-library "t-sort.scm")
(load-from-library "remove.scm")
(load-from-library "for-all.scm")
(load-from-library "sort.scm")
(load-from-library "memoize.scm")

(define *Instance-tag* (list 'instance))

(define <class> '())

(define %class-magic     0)
(define %class-name      1)
(define %class-d-supers  2)
(define %class-d-slots   3)
(define %class-cpl       4)
(define %class-slots     5)
(define %class-accessors 6)

(define %slot-name   car)
(define %slot-getter cadr)
(define %slot-setter caddr)

(define %method-specializer car)
(define %method-procedure   cadr)

(define %instance-tag   car)
(define %instance-class cdr)

(define (instance? x)
  (and-let* ((_ (procedure? x))
             (x (x))
             (_ (vector? x))
             (_ (> (vector-length x) %class-magic))
             (x (vector-ref x %class-magic)))
    (eq? (%instance-tag x) *Instance-tag*)))

(define (class? x)
  (and (instance? x)
       (eq? <class> (%instance-class (vector-ref (x) %class-magic)))))

(define (class-name class)
  (vector-ref (class) %class-name))

(define (class-cpl class)
  (vector-ref (class) %class-cpl))

(define (class-direct-supers class)
  (vector-ref (class) %class-d-supers))

(define (class-direct-slots class)
  (vector-ref (class) %class-d-slots))

(define (instance-class instance)
  (%instance-class (vector-ref (instance) %class-magic)))

(define class-of instance-class)

(define (compute-class-cpl classes)
  (t-sort eq?
          (car classes)
          (lambda (c)
            (cond ((null? c)
                    '())
                  ((eq? c (car classes))
                    classes)
                  (else
                    (cons c (class-direct-supers c)))))
          'top-down #t
          'reverse #t))

(define (compute-class-slots classes)
  (letrec
    ((proper-set?
       (lambda (a)
         (null? (dupp (fork eq? car) a))))
     (collect-slots
       (lambda (c*)
         (if (null? c*)
             '()
             (apply append 
                    (apply append (map (lambda (x)
                                         (vector-ref (x) %class-d-slots))
                                       c*))
                    (map (lambda (x)
                           (collect-slots (vector-ref (x) %class-d-supers)))
                         c*))))))
    (let ((slots (collect-slots classes)))
      (if (proper-set? slots)
          slots
          (error "compute-class-slots: ambiguous slot references"
                 (map car (dupp (fork eq? car) slots)))))))

(define (make-class name d-supers . d-slots)

  (define (make-slot x)
    (cond ((symbol? x)
            (list x '()))
          ((and (pair? x)
                (null? (cdr x)))
            (list (car x) '()))
          ((and (pair? x)
                (null? (cddr x)))
            x)
          (else
            (error "make-class: invalid slot syntax" x))))

  (define (make-accessors name slot-no)
    (list name
          (lambda (instance)
            (vector-ref (instance) slot-no))
          (lambda (instance value)
            (vector-set! (instance) slot-no value))))

  (let* ((inherited-slots (compute-class-slots d-supers))
         (d-slots         (map make-slot d-slots))
         (slots           (append inherited-slots d-slots))
         (class-vector
           (vector (cons *Instance-tag*            ; magic tag
                         <class>)
                   name                            ; class name
                   d-supers                        ; direct supers
                   d-slots                         ; direct slots
                   '()                             ; CPL
                   slots                           ; all slots
                   (map make-accessors             ; slot accessors
                        (map car slots)
                        (iota (length slots)))))
         (class
           (lambda ()
             class-vector)))
    (vector-set! class-vector
                 %class-cpl
                 (compute-class-cpl
                   (cons class
                         (vector-ref (class) %class-d-supers))))
    class))

(define (find-accessors instance slot-name)
  (let* ((class (class-of instance))
         (acc*  (vector-ref (class) %class-accessors))
         (acc   (assq slot-name acc*)))
    (if (not acc)
        (error (string-append (symbol->string (class-name class))
                              ": no such slot")
               slot-name)
        acc)))

(define slot-ref
  (let ((find-accessors find-accessors))
    (lambda (instance slot-name)
      (let ((acc (find-accessors instance slot-name)))
        ((%slot-getter acc) instance)))))

(define slot-set!
  (let ((find-accessors find-accessors))
    (lambda (instance slot-name value)
      (let ((acc (find-accessors instance slot-name)))
        ((%slot-setter acc) instance value)))))

(define (initialize x) #t)

(define (make-instance class . init-args)
  (let* ((instance-vector
           (apply vector (cons *Instance-tag* class)
                         (map cadr (slot-ref class 'slots))))
         (instance
           (lambda ()
             instance-vector)))
    (let init ((args init-args))
      (cond ((null? args))
            ((null? (cdr args))
              (error "make-instance: missing init value" args))
            (else
              (slot-set! instance (car args) (cadr args))
              (init (cddr args)))))
    (initialize instance)
    instance))

(define <type> (make-class '<type> ()))

(define <instance> (make-class '<instance> (list <type>)))

(define <class> (make-class '<class>
                            (list <instance>)
                            'name
                            'd-supers
                            'd-slots
                            'cpl
                            'slots
                            'accessors))

(vector-set! (<type>)     %class-magic (cons *Instance-tag* <class>))
(vector-set! (<instance>) %class-magic (cons *Instance-tag* <class>))
(vector-set! (<class>)    %class-magic (cons *Instance-tag* <class>))

(define <generic> (make-class '<generic>
                              (list <instance>)
                              (list 'methods ())))

(define (generic-methods generic)
  (slot-ref generic 'methods))

(define (compute-applicable-methods methods args)
  (let ((cpl* (map (compose class-cpl class-of)
                   args)))
    (filter (lambda (m)
              (for-all memq (%method-specializer m) cpl*))
            methods)))

(define (method-more-specific args)
  (let ((cpl* (map (compose class-cpl class-of)
                   args)))
    (lambda (m1 m2)
      (let loop ((s1* (%method-specializer m1))
                 (s2* (%method-specializer m2))
                 (c*  cpl*))
        (cond ((memq (car s2*) (memq (car s1*) (car c*))) #t)
              ((memq (car s1*) (memq (car s2*) (car c*))) #f)
              (else (loop (cdr s1*) (cdr s2*) (cdr c*))))))))

(define (no-applicable-method)
  (error "no applicable method"))

(define (no-next-method)
  (error "no next method"))

(define (compute-effective-method methods args)
  (let ((methods (sort (method-more-specific args) methods)))
    (if (null? methods)
        (lambda args
          (no-applicable-method))
        (let make-proc ((methods methods))
          (if (null? methods)
              (lambda args
                (no-next-method))
              (lambda args
                (apply (%method-procedure (car methods))
                       (lambda ()
                         (apply (make-proc (cdr methods))
                                args))
                       args)))))))

(define (apply-generic generic args)
  (let ((proc (compute-effective-method
                (compute-applicable-methods
                  (generic-methods generic)
                  args)
                args)))
    (apply proc args)))

(define (make-generic)
  (let ((g (make-instance <generic>)))
    (lambda args
      (if (null? args)
          (g)
          (apply-generic g args)))))

(define (add-method generic specializer procedure)
  (let* ((m* (generic-methods generic))
         (k  (if (null? m*)
                 #f
                 (length (%method-specializer (car m*)))))
         (m* (remp (lambda (x)
                     (equal? (%method-specializer x) specializer))
                   m*))
         (m* (cons (list specializer procedure) m*)))
    (if (and k (not (= k (length specializer))))
        (error "dd-method: wrong number of arguments"
               specializer))
    (slot-set! generic 'methods m*)))

(define-syntax (define-class name supers . slots)
  (let make-slots ((slots     slots)
                   (slot-list '()))
    (cond ((null? slots)
            `(begin
               (define ,name (make-class ',name
                                         (list ,@supers)
                                         ,@(reverse! slot-list)))
               (define-method (initialize (x ,name))
                 ,(if (null? supers)
                      #t
                      '(call-next-method)))))
          ((and (pair? (car slots))
                (pair? (cdar slots))
                (null? (cddar slots)))
            (make-slots (cdr slots)
                        (cons `(list ',(caar slots) ,(cadar slots))
                              slot-list)))
          (else
            (make-slots (cdr slots)
                        (cons `(quote ,(car slots))
                              slot-list))))))

(define-syntax (define-generic name)
  `(define ,name (make-generic)))

(define-syntax (define-method head . body)
  (if (not (pair? head))
      (error "define-method: syntax error" head)
      (let ((generic  (car head))
            (rest-arg '()))
        (let decompose ((head (cdr head))
                        (spec '())
                        (args '()))
          (cond ((null? head)
                  (let* ((args (append (reverse! args) rest-arg)))
                    `(add-method ,generic
                                 (list ,@(reverse! spec))
                                 (lambda ,(cons 'call-next-method args)
                                         ,@body))))
                ((symbol? head)
                  (set! rest-arg head)
                  (decompose '() spec args))
                ((and (pair? head)
                      (symbol? (car head)))
                  (decompose (cdr head)
                             (cons <type> spec)
                             (cons (car head) args)))
                ((and-let* ((_    (pair? head))
                            (_    (pair? (car head)))
                            (_    (pair? (cdar head)))
                            (_    (null? (cddar head)))
                            (name (caar head))
                            (_    (symbol? name))
                            (type (cadar head)))
                  (decompose (cdr head)
                             (cons type spec)
                             (cons name args))))
                (else
                  (error "define-method: bad argument" (car head))))))))

(define-generic initialize)

(define-method (initialize (x <type>)) #t)

(define-class <built-in>        (<type>))
(define-class   <boolean>       (<built-in>))
(define-class   <char>          (<built-in>))
(define-class   <eof-object>    (<built-in>))
(define-class   <null>          (<built-in>))
(define-class   <number>        (<built-in>))
(define-class     <integer>     (<number>))
(define-class   <pair>          (<built-in>))
(define-class   <port>          (<built-in>))
(define-class     <input-port>  (<port>))
(define-class     <output-port> (<port>))
(define-class   <procedure>     (<built-in>))
(define-class   <array>         (<built-in>))
(define-class     <string>      (<array>))
(define-class     <vector>      (<array>))
(define-class   <symbol>        (<built-in>))

(define-method (initialize (x <built-in>))
  (error "cannot make instances of <built-in> and its subclasses"))

(define (class-of obj)
  (cond ((boolean? obj)     <boolean>)
        ((char? obj)        <char>)
        ((eof-object? obj)  <eof-object>)
        ((input-port? obj)  <input-port>)
        ((integer? obj)     <integer>)
        ((output-port? obj) <output-port>)
        ((null? obj)        <null>)
        ((pair? obj)        <pair>)
        ((instance? obj)    (instance-class obj))
        ((procedure? obj)   <procedure>)
        ((string? obj)      <string>)
        ((symbol? obj)      <symbol>)
        ((vector? obj)      <vector>)
        (else               <type>)))
