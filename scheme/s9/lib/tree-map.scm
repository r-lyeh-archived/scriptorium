; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (tree-map procedure1 procedure2 pair)  ==>  pair
;
; Apply PROCEDURE2 to each node of the tree rooted at PAIR
; to which the predicate PROCEDURE1 applies. Return a fresh
; tree.
;
; Example:   (tree-map number? list '((a . 1) (b . 2)))
;              ==>  ((a . (1)) (b . (2)))
;            (tree-map (lambda (x) (and (pair? x)
;                                       (string? (car x))
;                                       (string? (cdr x))))
;                      (lambda (x) (string-append (car x) (cdr x)))
;                      '(("foo" . "bar") ("bar" . "baz")))
;              ==>  ("foobar" "barbaz")

(define (tree-map p f x)
  (letrec
    ((tree-map1
       (lambda (x)
         (cond ((p x)
                 (f x))
               ((pair? x)
                 (cons (tree-map1 (car x))
                       (tree-map1 (cdr x))))
               (else
                 x)))))
    (tree-map1 x)))
