; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (define-structure <name> <slot> ...)  ==>  unspecific
;
; DEFINE-STRUCTURE creates a new type, which is a sub-type of the vector,
; and defines a set of procedures for creating objects of the new type,
; accessing its slots, and checking for its type.
;
; <Name> is the name of the new type. Each <slot> defines a slot of the
; new type. It must be have one of the following forms:
;
;       <slot-name>
;       (<slot-name>)
;       (<slot-name> <initial-value>)
;
; <Slot-name> must be a symbol and <initial-value> may be any value.
; When an <initial-value> is specified, the corresponding slot will
; be filled with that value whenever a new instance of the structure
; is created. When the value is omitted, it defaults to an unspecific
; value. <Slot-name> is equal to (<slot-name>).
;
; (define-structure <type> <slot-1> ... <slot-N>) will expand to
; definitions of the following procedures:
;
; (make-<type> object ...) creates a new object of the type <type> and
; initializes its slots with the values specified in DEFINE-STRUCTURE.
; When some OBJECTs are given, they will replace the default values of
; the first slots of the new <type> object. The number of OBJECTs
; passed to MAKE-<TYPE> must not be larger than the number of slots
; of <type>.
;
; (<type>? x) is a predicate checking whether X has the type <type>.
;
; (<type>-assert caller object) asserts that OBJECT is of the type
; <type>. When the assertion holds, it returns an unspecific value.
; Otherwise, it prints an error message. CALLER is a symbol that
; will be reported as the source of the error (typically the
; procedure calling <type>-assert).
;
; (<type>-copy object) creates an exact (shallow) copy an object of
; the given type and returns it.
;
; (<type>-<slot-1> x) evaluates to the value stored in slot <slot-1>
; of X. When X is not of the type <type>, an error will be signalled.
; (<type>-<slot-N> x) does the same, but accesses <slot-N>.
;
; (<type>-set-<slot-1>! x v) changes the value stored in slot <slot-1>
; of X to V. When X is not of the type <type>, an error will be signalled.
; (<type>-set-<slot-N>! x v) does the same, but changes <slot-N>.
;
; Example:   (begin
;              (define-structure point (x 0) (y 0) (color #f))
;              (let ((p (make-point)))
;                (point-set-color! p 'yellow)
;                (list (point? p)
;                      (point-color p))))               ==>  (#t yellow)

(load-from-library "iota.scm")
(load-from-library "subvector.scm")
(load-from-library "duplicates.scm")

(define-syntax (define-structure name . slots)
  (if (not (symbol? name))
      (error "define-structure: expected name, got" name))
  (let* ((make-slot
           (lambda (x)
             (cond ((symbol? x)
                     (list x #f))
                   ((and (pair? x)
                        (symbol? (car x))
                        (null? (cdr x)))
                     (list (car x) #f))
                   ((and (pair? x)
                         (symbol? (car x))
                         (pair? (cdr x))
                         (null? (cddr x)))
                     x)
                   (else
                     (error "define-structure: expected slot, got" x)))))
         (slots (map make-slot slots))
         (dupes (dupq (map car slots)))
         (symbol-append
           (lambda x
             (string->symbol
               (apply string-append (map symbol->string x)))))
         (slot-defs
           (map cons
                (map car slots)
                (iota (length slots))))
         (def-tag
           `(define ,(symbol-append '* name '-type-tag*) (list ',name)))
         (def-maker
           (let ((args    (gensym))
                 (vec     (gensym))
                 (isym    (gensym))
                 (asym    (gensym))
                 (m-name  (symbol-append 'make- name)))
             `(define (,m-name . ,args)
                      (if (> (length ,args) ,(length slots))
                          (error (string-append
                                   (symbol->string ',m-name)
                                   ": too many arguments")
                                 ,args))
                      (let ((,vec (vector ,(symbol-append '* name '-type-tag*)
                                          ,@(map cadr slots))))
                        (do ((,isym 1 (+ 1 ,isym))
                             (,asym ,args (cdr ,asym)))
                             ((null? ,asym))
                          (vector-set! ,vec ,isym (car ,asym)))
                        ,vec))))
         (def-predicate
           (let ((p-name (symbol-append name '?)))
             `(define ,p-name
                (let ((tag ,(symbol-append '* name '-type-tag*)))
                  (lambda (x)
                    (and (vector? x)
                         (positive? (vector-length x))
                         (eq? tag (vector-ref x 0))))))))
         (def-assert
           `(define (,(symbol-append name '-assert) who x)
              (if (not (,(symbol-append name '?) x))
                  (error (string-append (symbol->string who)
                                        ": expected type <"
                                        (symbol->string ',name)
                                        ">, got")
                         x))))
         (def-copier
           (let ((c-name (symbol-append name '-copy))
                 (a-name (symbol-append name '-assert)))
             `(define ,c-name
                (let ((,a-name ,a-name))
                  (lambda (x)
                    (,a-name ',c-name x)
                    (vector-copy x))))))
         (def-getters
           (map (lambda (s)
                  (let ((g-name (symbol-append name '- (car s)))
                        (a-name (symbol-append name '-assert)))
                    `(define ,g-name
                       (let ((,a-name ,a-name))
                         (lambda (x)
                           (,a-name ',g-name x)
                           (vector-ref x ,(cdr s)))))))
                slot-defs))
         (def-setters
           (map (lambda (s)
                  (let ((s-name (symbol-append name '-set- (car s) '!))
                        (a-name (symbol-append name '-assert)))
                    `(define ,s-name
                       (let ((,a-name ,a-name))
                         (lambda (x v)
                           (,a-name ',s-name x)
                           (vector-set! x ,(cdr s) v))))))
                slot-defs)))
    (if (not (null? dupes))
        (error "define-structure: duplicate slot names" dupes))
    `(begin ,def-tag
            ,def-predicate
            ,def-assert
            ,def-maker
            ,def-copier
            ,@def-getters
            ,@def-setters)))
