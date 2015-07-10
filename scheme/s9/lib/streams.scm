; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2006,2010
; Placed in the Public Domain
;
; (make-stream obj1 proc1 proc2 proc3 proc4 obj2)  ==>  stream
; (stream-any object)                              ==>  #t
; (stream-none object)                             ==>  #f
; (stream-id object)                               ==>  object
; (stream-value stream)                            ==>  object
; (stream-next stream)                             ==>  stream
; (stream-eos? object)                             ==>  boolean
; (list->stream list)                              ==>  stream
; (stream->list stream)                            ==>  list
; (map-stream procedure stream)                    ==>  stream
; (filter-stream procedure stream)                 ==>  stream
; (append-streams stream ...)                      ==>  stream
; (stream-member procedure stream)                 ==>  stream
; (stream-extract integer stream)                  ==>  list
; (stream-iota integer1 integer2)                  ==>  stream
;
; (load-from-library "streams.scm")
;
; These procedures implement purely functional streams. The most
; general procedure for creating a new stream is MAKE-STREAM,
; whose arguments are as follows:
;
; OBJ1 is the initial object to be stored in the stream, e.g.
; the first number of a sequence or a list object when the
; stream is to produce the members of a list.
;
; PROC1 fetches the current value of OBJ1 and returns it.
; This would be CAR for list objects or STREAM-ID for numeric
; streams.
;
; PROC2 is a filter that selects specific members from OBJ1.
; For instance, EVEN? would make sure that a given numeric
; stream would produce only even numbers. STREAM-ANY would
; pass through all values.
;
; PROC3 generates a new value, i.e. it turns OBJ1 into OBJ1',
; where OBJ1' is the next member of the sequence produced by
; the stream. CDR would typically be used in a list stream
; and (lambda (x) (+ 1 x)) in a stream enumerating integers.
;
; PROC4 checks whether the end of the stream has been reached.
; In list streams, this would be NULL?. Infinite streams can
; be created by specifying STREAM-ANY or STREAM-INDEFINITE.
;
; OBJ2 is the final object to be delivered. This is typically
; *STREAM-EOS*, but this argument can also be used to
; concatenate streams. In infinite streams, this value does
; not matter.
;
; STREAM-VALUE returns the current value of a stream.
; STREAM-NEXT advances to the next value of a stream.
; STREAM-EOS? checks if a stream has been exhausted (end of stream).
;
; LIST->STREAM and STREAM->LIST convert lists to streams and
; vice versa. STREAM-EXTRACT is like STREAM->LIST, but extracts
; only a given number of arguments. The stream passed to it must
; be able to produce the given number of arguments.
;
; MAP-STREAM, STREAM-MEMBER, and APPEND-STREAMS resemble the
; corresponding Scheme functions.
;
; FILTER-STREAM adds an additional filter to an existing stream.
;
; STREAM-IOTA generates a finite numeric stream that counts from
; INTEGER1 to INTEGER2.
;
; Example:   (stream->list
;              (append-streams (list->stream '(a b c))
;                              (stream-iota 1 3)))   ==>  (a b c 1 2 3)
;            (stream->list
;              (filter-stream even?
;                             (stream-iota 1 10)))   ==>  (2 4 6 8 10)
;            (stream->list
;              (map-stream (lambda (x) (* 7 x))
;                          (stream-iota 1 5)))       ==>  (7 14 21 28 35)
;            (stream->list
;              (stream-member (lambda (x) (= 27 x))
;                             (stream-iota 1 30)))   ==>  (27 28 29 30)
;            (stream-extract
;              5
;              (stream-iota 1 10000000))             ==>  (1 2 3 4 5)

(define (make-stream init first filter rest lim final)

  (define (find x)
    (if (or (lim x)
            (filter (first x)))
        x
        (find (rest x))))

  (define (new-stream v)
    (lambda ()
      (let ((next-value (find v)))
        (if (lim next-value)
            final
            (cons (first next-value)
                  (new-stream (rest next-value)))))))

  ((new-stream init)))

(define (stream-any x) #t)

(define stream-indefinite stream-any)

(define (stream-none x) #f)

(define (stream-id x) x)

(define stream-value car)

(define (stream-next s) ((cdr s)))

(define *stream-eos* #f)

(define stream-eos? not)

(define (list->stream v)
  (make-stream v car stream-any cdr null? *stream-eos*))

(define (stream->list s)
  (letrec
    ((stream->list2
       (lambda (s lst)
         (if s
             (stream->list2 (stream-next s)
                            (cons (stream-value s) lst))
             (reverse! lst)))))
    (stream->list2 s '())))

(define (map-stream f s)
  (make-stream s
               (lambda (s) (f (stream-value s)))
               stream-any
               stream-next
               stream-eos?
               *stream-eos*))

(define (append-streams . s*)
  (if (null? s*)
      *stream-eos*
      (make-stream (car s*)
                   stream-value
                   stream-any
                   stream-next
                   stream-eos?
                   (apply append-streams (cdr s*)))))

(define (filter-stream p s)
  (make-stream s
               stream-value
               p
               stream-next
               stream-eos?
               *stream-eos*))

(define (stream-member p s)
  (cond ((stream-eos? s)      *stream-eos*)
        ((p (stream-value s)) s)
        (else                 (stream-member p (stream-next s)))))

(define (stream-extract n s)
  (if (zero? n)
      '()
      (cons (stream-value s)
            (stream-extract (- n 1) (stream-next s)))))

(define (stream-iota l h)
  (make-stream l
               stream-id 
               stream-any
               (lambda (x) (+ 1 x))
               (lambda (x) (> x h))
               *stream-eos*))
