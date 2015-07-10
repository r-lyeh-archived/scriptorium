; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (make-rbt procedure)                   ==>  rb-tree
; (rbt-find rb-tree object)              ==>  object | #f
; (rbt-insert rb-tree object1 object2)   ==>  rb-tree
; (rbt-rebuild rb-tree object)           ==>  rb-tree
; (rbt-remove rb-tree object)            ==>  rb-tree
;
; These procedures implement Red-Black Trees.
;
; MAKE-RBT returns an empty tree that uses PROCEDURE as an ordering
; predicate. If you plan, for instance, to use strings as keys, use
; STRING<? as a predicate.
;
; RBT-FIND locates the value associated with the key OBJECT in the
; given RB-TREE and returns it. When the key is not contain in the
; tree, it returns #F.
;
; RBT-INSERT returns a new rb-tree with OBJECT2 inserted into Rb-TREE
; under the key OBJECT1.
;
; RBT-REBUILD rebuilds the given tree and returns it. The original
; tree remains unchanged.
;
; RBT-REMOVE creates a new tree from RB-TREE with the key OBJECT
; removed. In fact, this procedure only marks the key as "inactive",
; i.e. it is left in the tree, but cannot be found any longer. To
; remove inactive nodes, use RBT-REBUILD.
;
; Example:   (let ((tree (fold-left
;                          (lambda (t k)
;                             (rbt-insert t k (make-string k #\x)))
;                          (make-rbt <)
;                          '(1 2 3 4 5 6 7))))
;              (rbt-find tree 5))               ==>  "xxxxx"

(load-from-library "hof.scm")
(load-from-library "package.scm")
(load-from-library "matcher.scm")
(load-from-library "define-structure.scm")

(define-structure rbt-type pred data)

(define-matcher rbt-balance
  (('black v1                        ;      Bv1
           ('red v2                  ;      / \
                 ('red v3 l3 r3)     ;    Rv2 r1
                 r2)                 ;    / \
           r1)                       ;  Rv3 r2
    => `(red ,v2                     ;  / \
             (black ,v3 ,l3 ,r3)     ; l3 r3
             (black ,v1 ,r2 ,r1))) 
  (('black v1                        ;  Bv1
           l1                        ;  / \
           ('red v2                  ; l1 Rv2
                 l2                  ;    / \
                 ('red v3 l3 r3)))   ;   l2 Rv3
    => `(red ,v2                     ;      / \
             (black ,v1 ,l1 ,l2)     ;     l3 r3
             (black ,v3 ,l3 ,r3)))
  (('black v1                        ;    Bv1
           ('red v2                  ;    / \
                 l2                  ;  Rv2  r1
                 ('red v3 l3 r3))    ;  / \
           r1)                       ; l2 Rv3
    => `(red ,v3                     ;    / \
             (black ,v2 ,l2 ,l3)     ;   l3 r3
             (black ,v1 ,r3 ,r1)))
  (('black v1                        ;  Bv1
           l1                        ;  / \
           ('red v2                  ; l1 Rv2
                 ('red v3 l3 r3)     ;    / \
                 r2))                ;  Rv3 r2
    => `(red ,v3                     ;  / \
             (black ,v1 ,l1 ,l3)     ; l3 r3
             (black ,v2 ,r3 ,r2)))
  (tree
    => tree))

(package red-black-tree

  (:import make-rbt-type
           rbt-type-pred
           rbt-type-data
           rbt-balance)

  (:export rbt-insert
           rbt-remove
           rbt-rebuild
           rbt-find
           make-rbt)

  (:make-aliases)

  (define (make-rbt pred)
      (make-rbt-type pred '()))

  (define (make-rb-tree color key value left right active)
    (list color (list key value active) left right))

  (define rbt-color  car)
  (define rbt-key    caadr)
  (define rbt-value  cadadr)
  (define rbt-active (compose car cddadr))
  (define rbt-left   caddr)
  (define rbt-right  cadddr)

  (define (find tree p x)
    (cond ((null? tree) #f)
          ((p x (rbt-key tree))
            (find (rbt-left tree) p x))
          ((p (rbt-key tree) x)
            (find (rbt-right tree) p x))
          ((rbt-active tree)
            (rbt-value tree))
          (else
            #f)))

  (define (rbt-find rbt x)
    (find (rbt-type-data rbt)
          (rbt-type-pred rbt)
          x))

  (define (insert tree p k v)
    (letrec
      ((ins
         (lambda (tree)
           (cond ((null? tree)
                   (make-rb-tree 'red k v '() '() #t))
                 ((p k (rbt-key tree))
                   (rbt-balance
                     (make-rb-tree
                       (rbt-color tree)
                       (rbt-key   tree)
                       (rbt-value tree)
                       (ins (rbt-left tree))
                       (rbt-right tree)
                       #t)))
                 ((p (rbt-key tree) k)
                   (rbt-balance
                     (make-rb-tree
                       (rbt-color tree)
                       (rbt-key   tree)
                       (rbt-value tree)
                       (rbt-left  tree)
                       (ins (rbt-right tree))
                       #t)))
                 (else
                   (make-rb-tree
                     (rbt-color tree)
                     k
                     v
                     (rbt-left tree)
                     (rbt-right tree)
                     #t))))))
      (let ((new (ins tree)))
        (make-rb-tree 'black
                      (rbt-key   new)
                      (rbt-value new)
                      (rbt-left  new)
                      (rbt-right new)
                      #t))))

  (define (rbt-insert rbt k v)
    (make-rbt-type (rbt-type-pred rbt)
                   (insert (rbt-type-data rbt)
                           (rbt-type-pred rbt)
                           k
                           v)))

  (define (rebuild tree p)
    (letrec
      ((reb
         (lambda (in out)
           (cond ((null? in) out)
                 ((rbt-active in)
                   (let* ((out (reb (rbt-left in) out))
                          (out (insert out p (rbt-key in) (rbt-value in)))
                          (out (reb (rbt-right in) out)))
                     out))
                 (else
                   (let* ((out (reb (rbt-left in) out))
                          (out (reb (rbt-right in) out)))
                     out))))))
      (reb tree '())))

  (define (rbt-rebuild rbt)
    (make-rbt-type (rbt-type-pred rbt)
                   (rebuild (rbt-type-data rbt)
                            (rbt-type-pred rbt))))

  (define (remove tree p x)
    (letrec
      ((rem
         (lambda (tree)
           (cond ((null? tree)
                   tree)
                 ((p x (rbt-key tree))
                   (make-rb-tree
                     (rbt-color tree)
                     (rbt-key   tree)
                     (rbt-value tree)
                     (rem (rbt-left tree))
                     (rbt-right tree)
                     #t))
                 ((p (rbt-key tree) x)
                   (make-rb-tree
                     (rbt-color tree)
                     (rbt-key   tree)
                     (rbt-value tree)
                     (rbt-left  tree)
                     (rem (rbt-right tree))
                     #t))
                 (else
                   (make-rb-tree
                     (rbt-color tree)
                     (rbt-key   tree)
                     (rbt-value tree)
                     (rbt-left  tree)
                     (rbt-right tree)
                     #f))))))
      (rem tree)))

  (define (rbt-remove rbt x)
    (make-rbt-type (rbt-type-pred rbt)
                   (remove (rbt-type-data rbt)
                           (rbt-type-pred rbt)
                           x))))
