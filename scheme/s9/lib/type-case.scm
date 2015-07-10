; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (type-case object <clause> ...)  ==>  object
; (type-of object)                 ==>  symbol
;
; (load-from-library "type-case.scm")
;
; TYPE-OF returns a symbol describing the type of the given OBJECT.
; The following symbols may be returned by the procedure:
;
;       boolean char eof-object input-port integer output-port
;       pair procedure real string symbol syntax unknown-object
;       vector
;
; (Type-case obj ...)  is shorthand for  (case (type-of obj) ...)
;
; Example:   (type-of type-of)  ==>  procedure
;
;            (let ((x '#(1 2 3))
;                  (i 0))
;              (type-case x
;                ((string) (string-ref x i))
;                ((vector) (vector-ref x i))
;                (else     x)))               ==>  1

(define (type-of obj)
  (cond ((symbol? obj)      'symbol)
        ((pair? obj)        'pair)
        ((boolean? obj)     'boolean)
        ((integer? obj)     'integer)
        ((real? obj)        'real)
        ((char? obj)        'char)
        ((string? obj)      'string)
        ((vector? obj)      'vector)
        ((procedure? obj)   'procedure)
        ((input-port? obj)  'input-port)
        ((output-port? obj) 'output-port)
        ((eof-object? obj)  'eof-object)
        (else               'unknown-object)))

(define-syntax (type-case obj . clauses)
  `(case (type-of ,obj) ,@clauses))
