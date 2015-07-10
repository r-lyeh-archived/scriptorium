; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (make-array integer ...)                           ==>  array
; (array object ...)                                 ==>  array
; (array? object)                                    ==>  boolean
; (array-dimensions array)                           ==>  list
; (array-map procedure array ...)                    ==>  array
; (array-rank array)                                 ==>  integer
; (array-ref array integer ...)                      ==>  object
; (array-set! array integer ... object)              ==>  unspecific
; (subarray array (list integer1,1 integer1,2) ...)  ==>  array
;
; (load-from-library "array.scm")
;
; MAKE-ARRAY creates a new array whose rank (number of dimensions)
; equals the number of integers specified. Each integer specifies
; the size of one dimension, e.g.: (make-array 2 3 4) creates an
; array with two elements in the first dimensions, three in the
; second and four in the third.
;
; ARRAY creates a fresh array of rank one and stores the given
; elements in it. Arrays of higher rank can be created by nesting
; applications of ARRAY.
;
; (Array? x) returns #T if X is an array and otherwise #F. The array
; is a subtype of the vector, so (vector? x) follows from (array? x).
;
; ARRAY-DIMENIONS returns a list of the dimensions of the given
; ARRAY. For a zero-dimensional array it returns ().
;
; ARRAY-MAP maps a procedure over the elements of the given arrays.
; The arity of PROCEDURE must match the number of ARRAYs. ARRAY-MAP
; returns a new array in which the elements of the input arrays have
; been tuple-wise combined with PROCEDURE, e.g.:
;
; (new1,1 ... new1,N  =  ((P a1,1,1 ... aK,1,1) ... (P a1,1,N aK,1,N)
;  ...                    ...
;  newM,1 ... newM,N)     (P a1,M,1 ... aK,M,1) ... (P a1,M,N aK,M,N))
;
; where K is the number of arrays, P is the procedure, M is the number
; of rows and N is the number of columns of a matrix. When more than
; two dimensions are involved the number of parameters grows accordingly.
;
; ARRAY-RANK returns the number of dimensions of the given ARRAY.
; Note that a one-dimensional array with one element has a rank
; of 0.
;
; (Array-ref a i1 ... iN) returns element <i1,...,iN> of the array A.
; N must be equal to the rank of A.
;
; (Array-set! a i1 ... iN v) sets element <i1,...,iN> of the array A
; to the value V. N must be equal to the rank of A.
;
; SUBARRAY creates a fresh array with the same rank as the given ARRAY
; and copies the specified elements to the new array. Each INTEGER1
; specifies the first element to copy from the corresponding dimension
; and INTEGER2 specifies the first element not to copy. The dimensions
; of the new array are
; (INTEGER1,2)-(INTEGER1,1) ... (INTEGERn,2)-(INTEGERn,1)
; where N is the rank of the array.
;
; Example:   (let ((a (make-array 3 3 3)))
;              (array-set! a 1 1 1 'foo)
;              (array-ref a 1 1 1))            ==>  foo
;
;            (let ((a (array (array 1 2 3 4)
;                            (array 3 4 5 6)
;                            (array 5 6 7 8))))
;              (list (array-rank a)
;                    (array-dimensions a)))    ==>  (2 (3 4))

(load-from-library "hof.scm")
(load-from-library "for-all.scm")
(load-from-library "transpose.scm")
(load-from-library "vector-map.scm")
(load-from-library "subvector.scm")

(define *array-type-tag* (list 'array))

(define (make-array . dim*)
  (if (null? dim*)
      (vector *array-type-tag* #f)
      (let make ((dim* dim*))
        (let ((subvec (make-vector (+ 1 (car dim*)))))
          (vector-set! subvec 0 *array-type-tag*)
          (if (not (null? (cdr dim*)))
              (vector-map! (lambda (x)
                             (if (not (eq? x *array-type-tag*))
                                 (make (cdr dim*))
                                 x))
                           subvec))
          subvec))))

(define (array? x)
  (and (vector? x)
       (positive? (vector-length x))
       (eq? *array-type-tag* (vector-ref x 0))))

(define (array-ref a . indexes)
  (if (not (array? a))
      (error "array-ref: expected array, got" a)
      (let aref ((a  a)
                 (i* indexes))
        (if (null? i*)
            (if (array? a)
                (error "array-ref: too few indexes" indexes)
                a)
            (if (array? a)
                (if (< (+ 1 (car i*)) (vector-length a))
                    (aref (vector-ref a (+ 1 (car i*)))
                          (cdr i*))
                    (error "array-ref: index out of range" (car i*)))
                (error "array-ref: too many indexes" indexes))))))

(define (array-set! a . indexes+val)
  (if (not (array? a))
      (error "array-set!: expected array, got" a))
  (if (< (length indexes+val) 2)
      (error "array-set!: missing indexes"))
  (let* ((x    (reverse indexes+val))
         (val  (car x))
         (ind* (reverse (cdr x))))
    (let aset ((a  a)
               (i* ind*))
      (if (null? (cdr i*))
          (if (array? a)
              (if (< (+ 1 (car i*)) (vector-length a))
                  (if (array? (vector-ref a (+ 1 (car i*))))
                      (error "array-set!: too few indexes" ind*)
                      (vector-set! a (+ 1 (car i*)) val))
                  (error "array-set!: index out of range" (car i*)))
              (error "array-set!: too many indexes" ind*))
          (if (array? a)
              (if (< (+ 1 (car i*)) (vector-length a))
                  (aset (vector-ref a (+ 1 (car i*))) (cdr i*))
                  (error "array-set!: index out of range" (car i*)))
              (error "array-set!: too many indexes" ind*))))))

(define (array . v)
  (list->vector (cons *array-type-tag* v)))

(define (subarray ar . co*)
  (if (not (array? ar))
      (error "subarray: expected array, got" ar)
      (let asub ((a   ar)
                 (co* co*))
        (if (null? co*)
            (if (array? a)
                (error "subarray: too few indexes" ar)
                a)
            (if (array? a)
                (if (or (not (list? (car co*)))
                        (not (= 2 (length (car co*))))
                        (not (integer? (caar co*)))
                        (not (integer? (cadar co*))))
                    (error "subarray: invalid coordinate" (car co*))
                    (let ((x0 (+ 1 (caar co*)))
                          (xn (+ 1 (cadar co*))))
                      (if (<= 1 x0 xn (vector-length a))
                          (let ((v (subvector a x0 xn)))
                            (vector-map! (lambda (sa)
                                           (asub sa (cdr co*)))
                                         v)
                            (apply array (vector->list v)))
                          (error "subarray: invalid range" (car co*)))))
                (error "subarray: too many indexes" ar))))))

(define (array-rank a)
  (cond ((not (array? a))
          (error "array-rank: expected array, got" a))
        ((and (= 2 (vector-length a))
              (not (array? (vector-ref a 1))))
          0)
        (else
          (let loop ((a a)
                     (n 0))
            (if (and (array? a)
                     (> (vector-length a) 1))
                (loop (vector-ref a 1)
                      (+ 1 n))
                n)))))

(define (array-dimensions a)
  (cond ((not (array? a))
          (error "array-dimensions: expected array, got" a))
        ((and (= 2 (vector-length a))
              (not (array? (vector-ref a 1))))
          '())
        (else
          (let loop ((a  a)
                     (d* '()))
            (let ((k (and (array? a)
                          (- (vector-length a) 1))))
              (if (and (array? a)
                       (positive? k))
                  (loop (vector-ref a 1) (cons k d*))
                  (reverse! d*)))))))

(define (array-map f . a*)
  (cond ((for-all array? a*)
          (apply array (map (lambda (x)
                              (apply array-map f x))
                            (transpose
                              (map (compose cdr vector->list)
                                   a*)))))
        (else
          (apply f a*))))
