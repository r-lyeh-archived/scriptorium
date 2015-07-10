; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; (t-sort procedure1 object procedure2 <option> ...)  ==>  list
; (t-sort-net procedure^2 list <option> ...)          ==>  list
;
; Sort the directed acyclic graph (DAG) LIST topologically in
; such a way that all dependencies of the first node in the DAG
; (called the "goal") are resolved. PROCEDURE^2 is used to identify
; nodes in the DAG.
;
; A DAG is represented by a list of lists of the form
;
;       ((<name-1> <ref> ...)
;        ...
;        (<name-N> <ref> ...))
;
; where each <name-I> names a node of the DAG and each <ref>
; names a child of the node. Node <name-I> is said to depend on
; each <ref> in the same sublist. A node with zero <ref>s is a
; leaf node.
;
; When the 'STRICT keyword with a #T value is passed as an option
; argument to T-SORT-NET, it will operate in "strict mode" where each
; <ref> in the DAG must have a corresponding node. In non-strict
; operation undefined <ref>s are assumed to be leaves.
;
; T-SORT-NET returns #F when it cannot sort a given DAG, either because
; it contains undefined <refs> in strict mode or because it cycles (and
; hence is not a DAG at all).
;
; When 'CHECK #T is passed as an option to T-SORT-NET, it will return
; more useful information in case of an error, namely
;
;       (cyclic . name)     when the graph cycles through NAME
;       (undefined . name)  when node NAME is undefined.
;
; The result can be distinguished from success by the fact that
; the cdr of a negative result is not a pair.
;
; When the 'REVERSE #T option is passed to T-SORT-NET, it will list
; each dependent object before its dependencies.
;
; When the 'TOP-DOWN #T option is passed to T-SORT-NET, it will
; preserve the order of dependencies and the hierarchy of the
; net to sort, i.e. objects closer to the goal will appear last
; in the resulting list (or first, if 'REVERSE #T is also given).
;
; T-SORT is a more general version of T-SORT-NET that allows to sort
; structures without knowing their exact internal representation.
; PROCEDURE1 is the predicate used to compare objects, like in
; T-SORT-NET. OBJECT is the goal. PROCEDURE2 is a procedure that maps
; objects to dependencies their associated dependencies. The procedure
; should return #F when a dependency cannot be resolved. In case of
; success, it delivers a list of the form
;
;       (goal object ...)
;
; GOAL is the goal that has been looked up and each OBJECT is an
; object on which the goal depends.
;
; Example:   (t-sort-net eq?
;                        '((dressed shoes hat)
;                          (shoes socks pants)
;                          (pants underpants)
;                          (hat pullover)
;                          (pullover shirt undershirt)
;                          (shirt undershirt)
;                          (underpants)))      ==>  (socks underpants pants
;                                                    shoes undershirt shirt
;                                                    pullover hat dressed)
;
;            (let ((db '((a b c)
;                        (b u)
;                        (c v)
;                        (u x)
;                        (v y)
;                        (w z))))
;              (t-sort eq? 'a (lambda (x)
;                               (assq x db))
;                             'top-down #t
;                             'reverse #t))      ==>  (a b c u v x y)
;
;            (t-sort-net eq? '((a b c d)))             ==>  (b c d a)
;            (t-sort-net eq? '((a b c d)) 'strict #t)  ==>  #f
;            (t-sort-net eq? '((a b) (b a)))           ==>  #f
;            (t-sort-net eq? '((foo foo)) 'check #t)   ==>  (cyclic . foo)

(load-from-library "letcc.scm")
(load-from-library "assp.scm")
(load-from-library "memp.scm")
(load-from-library "hash-table.scm")
(load-from-library "keyword-value.scm")

(define (t-sort p goal lookup . opts)
  (let/cc exit
    (let ((visited   (make-hash-table 'test p))
          (_         (accept-keywords "t-sort"
                                      opts
                                      '(strict check reverse top-down)))
          (strict    (keyword-value opts 'strict #f))
          (check     (keyword-value opts 'check #f))
          (rev-order (keyword-value opts 'reverse #f))
          (top-down  (keyword-value opts 'top-down #f)))
      (letrec
        ((find-dep
           (lambda (x)
             (cond ((lookup x)
                     => (lambda (x) x))
                   (strict
                     (exit (if check
                               `(undefined . ,dep)
                               #f)))
                   (else
                     '()))))
         (sort-bu
           (lambda (dep)
             (cond ((pair? dep)
                     (let ((res (apply append (map sort-bu (cdr dep)))))
                       (if (memp p (car dep) res)
                           (exit (if check
                                     `(cyclic . ,(car dep))
                                     #f)))
                       (if rev-order
                           (append (list (car dep)) res)
                           (append res (list (car dep))))))
                   ((hash-table-ref visited dep)
                     '())
                   (else
                     (hash-table-set! visited dep #t)
                     (let ((new-dep (find-dep dep)))
                       (cond ((null? new-dep)
                               (list dep))
                             ((null? (cdr new-dep))
                               (list (car new-dep)))
                             (else
                               (sort-bu new-dep))))))))
         (sort-td
           (lambda (dep)
             (cond ((pair? dep)
                     (if (hash-table-ref visited dep)
                         (exit (if check
                                   `(cyclic . ,dep)
                                   #f)))
                     (hash-table-set! visited dep #t)
                     (let* ((res (map sort-td dep))
                            (res (map (lambda (x)
                                        (if (null? x)
                                            '()
                                            (cdr x)))
                                      res))
                            (res (apply append res)))
                       (append dep (sort-td res))))
                   (else
                     (find-dep dep))))))
      (if top-down
          (let* ((dep (find-dep goal))
                 (res (cons (car dep) (sort-td (cdr dep))))
                 (res (list->set res)))
            (if rev-order
                res
                (reverse res)))
          (sort-bu (find-dep goal)))))))

(define (t-sort-net p net . opts)
  (apply t-sort p
                (caar net) 
                (lambda (x)
                  (assp p x net))
                opts))
