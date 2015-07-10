; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (make-queue)           ==>  queue
; (queue! queue object)  ==>  unspecific
; (unqueue! queue)       ==>  object
; (queue-empty? queue)   ==>  queue
; (unqueue* queue)       ==>  list
;
; MAKE-QUEUE returns an empty queue. A queue is a data structure
; that delivers unqueued elements in the same order in which they
; were queued (a first-in first-out structure). Both enqueue and
; dequeue operations are O(1).
;
; QUEUE! (destructively) inserts an OBJECT at the input end
; of QUEUE.
;
; QUEUE-EMPTY? returns #T if QUEUE is empty.
;
; UNQUEUE! (destructively) removes an element from the output end
; of QUEUE and returns it.
;
; UNQUEUE* removes an element from a queue and returns both the
; element and the queue in a list of the form
;
;       (element queue)
;
; NOTE: although a queue may look like a list, it is actually
; a (directed acylic) graph. Altering a queue with list operations
; is therefore not recommended! Copying a queue with procedures
; like TREE-COPY will turn it into a non-queue. Caveat utilitor!
;
; Example:   (let ((q (make-queue)))
;              (for-each (lambda (x) (queue! q x))
;                        '(a b c d e))
;              (unqueue* q))            ==>  (a ((e) b c d e))

(define (make-queue)
  (cons '() '()))

(define (queue! q x)
  (let ((b (list x)))
    (if (null? (car q))
        (set-cdr! q b)
        (set-cdr! (car q) b))
    (set-car! q b)))

(define (queue-empty? q)
  (null? (car q)))

(define (unqueue! q)
  (let ((x (cadr q)))
    (if (null? (cddr q))
        (set-car! q '()))
    (set-cdr! q (cddr q))
    x))

(define (unqueue* q)
  (let ((x (unqueue! q)))
    (list x q)))
