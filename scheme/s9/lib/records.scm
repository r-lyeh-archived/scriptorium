; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (record pair ...)                   ==>  record
; (record? object)                    ==>  boolean
; (record-ref record symbol)          ==>  object
; (record-set! record symbol object)  ==>  unspecific
; (list->record alist)                ==>  record
; (record->list record)               ==>  alist
; (record-equal? record1 record2)     ==>  boolean
; (record-copy record)                ==>  record
; (record-signature record)           ==>  list
; (record-type-matches? list record)  ==>  boolean
; (assert-record-type list record)    ==>  record
;
; (load-from-library "records.scm")
;
; These procedures implement ML-style records.
;
; RECORD creates a new record from the given (TAG . OBJECT) PAIRs,
; where TAG is a symbol naming the record field and OBJECT is the
; value stored in that field. Each TAG must be unique.
;
; RECORD? returns #T, if the given OBJECT is a record.
;
; RECORD-REF extracts the value of the field tagged SYMBOL
; from RECORD.
;
; RECORD-SET! sets the SYMBOL field of RECORD to OBJECT. If OBJECT
; and the value of the SYMBOL have different types, an error is
; reported.
;
; LIST->RECORD creates a RECORD from the association list ALIST.
; (LIST->RECORD (LIST P ...)) is equal to (RECORD P ...).
;
; RECORD->LIST returns an association list containing the same
; fields as the given RECORD. Tags of the fields become keys of
; the alist.
;
; RECORD-EQUAL? returns true, if RECORD1 and RECORD2 contain
; the same fields and corresponding fields contain equal values
; in the sense of EQUAL.
;
; RECORD-COPY creates a fresh copy of RECORD.
;
; RECORD-SIGNATURE creates a "type signature" of RECORD.
;
; RECORD-TYPE-MATCHES? returns #T, if LIST is the type signature
; or RECORD.
;
; ASSERT-RECORD-TYPE returns RECORD, if LIST is the type signature
; of RECORD. Otherwise, it reports an error.
;
; The RECORD type also extends the built-in EQUAL? procedure to
; handle records properly by dispatching their comparison to
; RECORD-EQUAL?.
;
; Example:   (record-ref (record (list 'name "Foo") (list 'value 31415))
;                        'name)
;              ==> "Foo"
;
;            (equal? (record (list 'name "Foo") (list 'value 31415))
;                    (record (list 'value 31415) (list 'name "Foo")))
;              ==> #t

(define record-tag (list '%record))

; The idea of using vectors to introduce a new disjoint type
; is taken from SRFI-9 by Richard Kelsey.

(define real-vector? vector?)

(define vector?
  (let ((record-tag record-tag)
        (real-vector? real-vector?))
    (lambda (x)
      (and (real-vector? x)
           (or (zero? (vector-length x))
               (not (eq? record-tag (vector-ref x 0))))))))

(define record?
  (let ((record-tag record-tag)
        (real-vector? real-vector?))
    (lambda (x)
      (and (real-vector? x)
           (> (vector-length x) 0)
           (eq? record-tag (vector-ref x 0))))))

(define list->record
  (let ((record-tag record-tag))
    (lambda (a)
      (letrec
        ((valid-fields?
           (lambda (a)
             (or (null? a)
                 (and (pair? (car a))
                      (symbol? (caar a))
                      (pair? (cdar a))
                      (null? (cddar a))
                      (valid-fields? (cdr a)))))))
        (if (valid-fields? a)
            (list->vector (cons record-tag a))
            (error "list->record: bad record structure" a))))))

(define (record . x) (list->record x))

(define (record->list r)
  (if (record? r)
      (cdr (vector->list r))
      (error "record->list: expected record, got" r)))

(define (record-box x t)
  (cond ((assq t (record->list x))
          => (lambda (x) (cdr x)))
        (else
          (error "record-box: no such tag"
                 (list 'record: x 'tag: t)))))

(define record-ref
  (let ((record-box record-box))
    (lambda (r t)
      (car (record-box r t)))))

(define (type-of x)
  (cond ((boolean? x)   'boolean)
        ((char? x)      'char)
        ((null? x)      'nil)
        ((number? x)    'number)
        ((record? x)    'record)
        ((pair? x)      'pair)
        ((port? x)      'port)
        ((procedure? x) 'procedure)
        ((string? x)    'string)
        ((symbol? x)    'symbol)
        ((vector? x)    'vector)
        (else           (error "type-of: unknown type" x))))

(define (record-equal? r1 r2)
  (letrec
    ((equal-fields?
       (lambda (r1 r2)
         (cond ((null? r1)
                 #t)
               ((assq (caar r1) r2)
                 => (lambda (x)
                      (and (equal? (cadar r1) (cadr x))
                           (equal-fields? (cdr r1) r2))))
               (else
                 #f)))))
    (let ((lr1 (record->list r1))
          (lr2 (record->list r2)))
      (and (= (length lr1) (length lr2))
           (equal-fields? lr1 lr2)))))

(define real-equal? equal?)

(define equal?
  (let ((real-equal? real-equal?))
    (lambda (a b)
      (if (record? a)
          (and (record? b) (record-equal? a b))
          (real-equal? a b)))))

(define (record-copy r)
  (letrec
    ((copy
       (lambda (x)
         (if (pair? x)
             (cons (copy (car x))
                   (copy (cdr x)))
             x))))
    (list->record (copy (record->list r)))))

(define record-signature
  (let ((type-of type-of))
    (lambda (r)
      (letrec
        ((make-sig
           (lambda (x)
             (map (lambda (x)
                    (if (record? (cadr x))
                        (list (car x)
                              (list (type-of (cadr x))
                                    (record-signature (cadr x))))
                        (list (car x) (type-of (cadr x)))))
                  x))))
        (list->record (make-sig (record->list r)))))))

(define types-match
  (let ((type-of type-of))
    (lambda (a b)
      (let ((ta (type-of a))
            (tb (type-of b)))
        (or (eq? ta tb)
            (and (eq? ta 'pair) (eq? tb 'nil))
            (and (eq? ta 'nil) (eq? tb 'pair)))))))

(define record-set!
  (let ((types-match types-match))
    (lambda (r t v)
      (let ((b (record-box r t)))
        (if (types-match (car b) v)
            (if (or (not (record? v))
                    (record-equal? (record-signature (car b))
                                   (record-signature v)))
                (set-car! b v)
                (error "record-set!: type mismatch"
                       (list 'record: r 'tag: t 'value: v)))
            (error "record-set!: type mismatch"
                   (list 'record: r 'tag: t 'value: v)))))))

(define (record-type-matches? sig r)
  (record-equal? sig (record-signature r)))

(define (assert-record-type sig r)
  (if (not (record-type-matches? sig r))
      (error "record type assertion failed"
             (list 'signature: sig 'record: r))
      r))
