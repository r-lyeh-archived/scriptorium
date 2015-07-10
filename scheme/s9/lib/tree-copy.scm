; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (tree-copy pair)              ==>  pair
; (tree-copy pair 'with-atoms)  ==>  pair
;
; Create an exact copy of an arbitrary non-cyclic cons structure.
; When a second argument is passed to TREE-COPY and that argument
; is not #F, then TREE-COPY will copy modifiable leaves of the tree,
; too.
;
; Example:   (tree-copy '(((a . b) (c . d)) (e . f)))
;                ==>  (((a . b) (c . d)) (e . f))
;
;            (let* ((tree  (list (string #\A)))
;                   (tree2 (tree-copy tree))
;                   (tree3 (tree-copy tree 'with-atoms)))
;              (string-set! (car tree) 0 #\X)
;              (list tree2 tree3))              ==>  (("X") ("A"))

(load-from-library "subvector.scm")
(load-from-library "type-case.scm")

(define (tree-copy tree . with-atoms)
  (let ((with-atoms (and (not (null? with-atoms))
                         (car with-atoms))))
    (let copy ((tree tree))
      (cond ((pair? tree)
              (cons (copy (car tree))
                    (copy (cdr tree))))
            (with-atoms
              (type-case tree
                ((vector) (vector-copy tree))
                ((string) (string-copy tree))
                (else     tree)))
            (else
              tree)))))
