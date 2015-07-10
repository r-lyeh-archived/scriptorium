; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009-2012
; Placed in the Public Domain
;
; (make-hash-table <option> ...)                  ==>  hash-table
; (alist->hash-table alist)                       ==>  hash-table
; (alist->hash-table alist integer)               ==>  hash-table
; (hash-table-length hash-table)                  ==>  integer
; (hash-table-ref hash-table object)              ==>  value | #f
; (hash-table-remove! hash-table object)          ==>  value | #f
; (hash-table-set! hash-table object-k object-v)  ==>  unspecific
; (hash-table->alist hash-table)                  ==>  list
;
; (load-from-library "hash-table.scm")
;
; MAKE-HASH-TABLE creates a fresh hash table.
;
; When the 'SIZE option is passed to it, it must be followed by
; an integer specifying its initial number of slots. When no
; initial value is specified, a built-in default will be used.
; No matter what size is specified, the hash table will grow
; automatically when the number of elements stored in it
; exceeds its current size. When specifying an explicit size,
; it should be a prime number.
;
; When the 'TEST option is passed to MAKE-HASH-TABLE, it must
; be followed by a predicate testing for the identity of keys.
; The predicate defaults to EQUAL?, but a more specific and/or
; more efficient predicate can be used when keys are limited to
; the domain of such a predicate.
;
; ALIST->HASH-TABLE creates a fresh hash table and inserts each
; cdr element of the given ALIST into that table using the
; associated car element as a key. It returns the new hash table.
; When an additional INTEGER is passed to ALIST->HASH-TABLE, an
; initial table of the given size will be allocated. The INTEGER
; should be a prime number.
;
; HASH-TABLE-LENGTH returns the number of objects currently
; stored in the given hash table.
;
; HASH-TABLE-REF retrieves a VALUE from a hash table using the
; given OBJECT as key. It returns the value as the only element
; of a fresh list. When no element with the given key exists,
; it returns #F.
;
; HASH-TABLE-REMOVE! removes the key OBJECT (and its associated
; value) from the given hash table.
;
; HASH-TABLE-SET! stores the value OBJECT-V under the key
; OBJECT-K in the given hash-table.
;
; HASH-TABLE->ALIST returns an association list containing all
; pairs of the given hash table in no specific order.
;
; Example:   (let ((h (make-hash-table)))
;              (hash-table-set! h "key" 'value)
;              (hash-table-ref  h "key"))        ==>  (value)

(load-from-library "count.scm")
(load-from-library "assp.scm")
(load-from-library "keyword-value.scm")
(load-from-library "define-structure.scm")

(define-structure ht (len 0) (test equal?) table)

(define make-hash-table
  (let ((make-ht       make-ht)
        (ht-set-test!  ht-set-test!)
        (ht-set-table! ht-set-table!))
    (lambda opts
      (accept-keywords "make-hash-table" opts '(test size))
      (let ((size (keyword-value opts 'size 101))
            (test (keyword-value opts 'test equal?)))
        (let ((ht (make-ht)))
          (ht-set-test! ht test)
          (ht-set-table! ht (make-vector size '()))
          ht)))))

(define (hash x k)
  (letrec
    ((string->hash
       (lambda (s k)
         (let ((ks (string-length s)))
           (let loop ((h 0)
                      (i 0))
             (if (>= i ks)
                 h
                 (loop (remainder
                         (+ (* 8 h) (char->integer (string-ref s i)))
                         k)
                       (+ 1 i))))))))
    (cond ((symbol? x) (string->hash (symbol->string x) k))
          ((string? x) (string->hash x k))
          ((number? x) (remainder (abs x) k))
          ((char? x)   (remainder (char->integer x) k))
          ((pair? x)   (remainder (count x) k))
          ((vector? x) (remainder (count (vector->list x)) k))
          (else        (- k 1)))))

(define hash-table-ref
  (let ((hash    hash)
        (ht-test ht-test))
    (lambda (h k)
      (let ((i (hash k (vector-length (ht-table h)))))
        (cond ((assp (ht-test h) k (vector-ref (ht-table h) i))
                => (lambda (x)
                     (cons (cdr x) '())))
              (else
                #f))))))

(define grow-table!
  (let ((ht-table ht-table)
        (ht-set-table! ht-set-table!))
    (lambda (h)
      (if (< (vector-length (ht-table h)) 49999)
          (let* ((k  (vector-length (ht-table h)))
                 (k  (cond ((< k  499)    499)
                           ((< k 4999)   4999)
                           ((< k 9973)   9973)
                           ((< k 19997) 19997)
                           (else        49999)))
                 (h* (make-hash-table 'size k)))
            (let loop ((i 0)
                       (k (vector-length (ht-table h))))
              (cond ((>= i k)
                      (ht-set-table! h (ht-table h*)))
                    (else
                      (for-each (lambda (x)
                                  (hash-table-set! h* (car x) (cdr x)))
                                (vector-ref (ht-table h) i))
                      (loop (+ 1 i) k)))))))))

(define hash-table-set!
  (let ((hash        hash)
        (ht-len      ht-len)
        (ht-test     ht-test)
        (ht-table    ht-table)
        (grow-table! grow-table!))
    (lambda (h k v)
      (if (> (ht-len h) (vector-length (ht-table h)))
          (grow-table! h))
      (let ((i (hash k (vector-length (ht-table h)))))
        (cond ((assp (ht-test h) k (vector-ref (ht-table h) i))
                => (lambda (x)
                     (set-cdr! x v)))
              (else
                (ht-set-len! h (+ 1 (ht-len h)))
                (vector-set! (ht-table h)
                             i
                             (cons (cons k v)
                                   (vector-ref (ht-table h) i)))))))))

(define hash-table-remove!
  (let ((hash     hash)
        (ht-test  ht-test)
        (ht-table ht-table))
    (lambda (h k)
      (let* ((i   (hash k (vector-length (ht-table h))))
             (new (let rem ((v* (vector-ref (ht-table h) i)))
                    (cond ((null? v*)
                            '())
                          (((ht-test h) k (caar v*))
                            (ht-set-len! h (- (ht-len h) 1))
                            (cdr v*))
                          (else
                            (cons (car v*)
                                  (rem (cdr v*))))))))
        (vector-set! (ht-table h) i new)))))

(define hash-table-length ht-len)

(define (alist->hash-table alist . opts)
  (let ((h (apply make-hash-table opts)))
    (for-each (lambda (x)
                (hash-table-set! h (car x) (cdr x)))
              alist)
    h))

(define hash-table->alist
  (let ((ht-table ht-table))
    (lambda (h)
      (apply append (vector->list (ht-table h))))))
