; Automatically generated test suite,
; See libtest.sh for details.

(load-from-library "syntax-rules.scm")

(define Errors 0)

(define (check src expr result)
;  (write src) (display " ==> ") (write expr) (newline)
  (if (not (equal? expr result))
      (begin (write src)
             (display " FAILED!")
             (newline)
             (display "Expected: ")
             (write result)
             (newline)
             (display "But got:  ")
             (write expr)
             (newline)
             (set! Errors (+ 1 Errors)))))

(define-syntax %test
  (syntax-rules (==>)
    ((_) #t)
    ((_ expr ==> result)
       (check 'expr expr 'result))
    ((_ expr ==> result . more)
       (begin (check 'expr expr 'result)
              (%test . more)))))

(load-from-library "adjoin.scm")
(%test
  (adjoin 'x '(a b c))  ==>  (x a b c)
  (adjoin 'c '(a b c))  ==>  (a b c)
)

(load-from-library "amb.scm")
(%test
  (begin (amb-reset)
         (let ((collect (amb-collector)))
           (let ((x (amb 4 1 7)))
             (let ((y (amb 6 8 2)))
               (let ((z (amb 5 3 9)))
                 (collect > x y z))))))  ==>  ((7 6 3) (7 6 5))
)

(load-from-library "amk.scm")
(%test
  (run* (q) (fresh (h t) 
              (== q (list h t))
              (appendo h t '(1 2 3))))
    ==>  ((() (1 2 3)) ((1) (2 3)) ((1 2) (3)) ((1 2 3) ()))
)

(load-from-library "and-letstar.scm")
(%test
  (and-let* ((a '((x . 1)))
             (a (assq 'x a)))
    (cdr a))                   ==>  1
;
  (and-let* ((a '((x . 1)))
             (a (assq 'z a)))
    (cdr a))                   ==>  #f
)

(load-from-library "appendb.scm")
(%test
  (let ((a (list 1 2 3)))
    (append! a (list 4 5 6) 'end)
    a)                             ==>  (1 2 3 4 5 6 . end)
)

(load-from-library "array.scm")
(%test
  (let ((a (make-array 3 3 3)))
    (array-set! a 1 1 1 'foo)
    (array-ref a 1 1 1))            ==>  foo
;
  (let ((a (array (array 1 2 3 4)
                  (array 3 4 5 6)
                  (array 5 6 7 8))))
    (list (array-rank a)
          (array-dimensions a)))    ==>  (2 (3 4))
)

(load-from-library "assp.scm")
(%test
  (assp char=? #\b '((#\a . 1) (#\b . 2)))  ==>  (#\b . 2)
)

(load-from-library "bitops.scm")
(%test
  (bit0  123 456)  ==>  0
  (bit*  127  99)  ==>  99
  (bit+   63  64)  ==>  127
  (bita  123 456)  ==>  123
  (bitb  123 456)  ==>  456
  (bitsl 123   1)  ==>  246
)

(load-from-library "bitwise-ops.scm")
(%test
  (bitwise-clear   #b1010 #b1100)  ==>  #b0000
  (bitwise-not-or  #b1010 #b1100)  ==>  #b0001
  (bitwise-and-c2  #b1010 #b1100)  ==>  #b0010
  (bitwise-c2      #b1010 #b1100)  ==>  #b0011
  (bitwise-and-c1  #b1010 #b1100)  ==>  #b0100
  (bitwise-c1      #b1010 #b1100)  ==>  #b0101
  (bitwise-xor     #b1010 #b1100)  ==>  #b0110
  (bitwise-not-and #b1010 #b1100)  ==>  #b0111
  (bitwise-and     #b1010 #b1100)  ==>  #b1000
  (bitwise-not-xor #b1010 #b1100)  ==>  #b1001
  (bitwise-1       #b1010 #b1100)  ==>  #b1010
  (bitwise-or-c2   #b1010 #b1100)  ==>  #b1011
  (bitwise-2       #b1010 #b1100)  ==>  #b1100
  (bitwise-or-c1   #b1010 #b1100)  ==>  #b1101
  (bitwise-or      #b1010 #b1100)  ==>  #b1110
  (bitwise-set     #b1010 #b1100)  ==>  #b1111
  (bitwise-shift-left 1 10)        ==>  1024
  (bitwise-shift-right 10 1)       ==>  5
)

(load-from-library "catch.scm")
(%test
  (let ((v #f))
    (let ((r (catch 'foo
               (set! v 0)
               (throw 'foo 1)
               (set! v 2)
               3)))
      (list v r)))             ==>  (0 1)
)

(load-from-library "cdf.scm")
(%test
  (cdf 0)  ==>  0.5
)

(load-from-library "char-canvas.scm")
(%test
  (let ((c (make-canvas 10 5 10 10)))
    (canvas-plot-line c 0 9 9 0 #\#)
    (canvas-plot-line c 0 0 9 9 #\*)
    (canvas-dump c))                   ==>  #("##      **"
                                              "  ##  **  "
                                              "    **    "
                                              "  **  ##  "
                                              "**      ##")
)

(load-from-library "choose.scm")
(%test
  (choose 23 1)  ==>  23
  (choose 23 2)  ==>  253
  (choose 23 3)  ==>  1771
)

(load-from-library "collect.scm")
(%test
  (collect eq? '(a a a b c c))  ==>  ((a a a) (b) (c c))
  (collect < '(1 2 3 3 4 5 4))  ==>  ((1 2 3) (3 4 5) (4))
)

(load-from-library "combine.scm")
(%test
  (combine 2 '(a b c))   ==>  ((a b) (a c) (b c))
  (combine* 2 '(a b c))  ==>  ((a a) (a b) (a c)
                               (b b) (b c) (c c))
)

(load-from-library "cond-expand.scm")
(%test
  (cond-expand (s9fes (cons 1 2)))               ==>  (1 . 2)
  (cond-expand (foo (cons 1 2)) (else (+ 1 2)))  ==>  3
)

(load-from-library "count.scm")
(%test
  (count '(a (b (c)) d . e))  ==>  5
)

(load-from-library "define-structure.scm")
(%test
  (begin
    (define-structure point (x 0) (y 0) (color #f))
    (let ((p (make-point)))
      (point-set-color! p 'yellow)
      (list (point? p)
            (point-color p))))               ==>  (#t yellow)
)

(load-from-library "depth.scm")
(%test
  (depth '(a (b (c d (e)))))  ==>  4
)

(load-from-library "duplicates.scm")
(%test
  (dupp = '(1 2 3 1 2))        ==>  (1 2)
  (duplicates '((1) (2) (1)))  ==>  ((1))
  (dupv '(#\a #\b #\a #\c))    ==>  (#\a)
  (dupq '(a b c d a c e f c))  ==>  (a c)
)

(load-from-library "equal-cip.scm")
(%test
  (equal-ci? '(#\A ("b")) '(#\a ("B")))  ==>  #t
)

(load-from-library "erf.scm")
(%test
  (erf 0)  ==>  0.0
)

(load-from-library "exists.scm")
(%test
  (exists < '(9 1) '(8 2) '(7 3))  ==>  #t
  ; because (< 1 2 3)
)

(load-from-library "explode.scm")
(%test
  (explode 'supernova)  ==>  (s u p e r n o v a)
)

(load-from-library "factor.scm")
(%test
  (factor 24)  ==>  ((3 1) (2 3))
)

(load-from-library "factorial.scm")
(%test
  (factorial 30)  ==>  265252859812191058636308480000000
)

(load-from-library "filter.scm")
(%test
  (filter number? '(a 1 b 2 c 3))  ==>  (1 2 3)
)

(load-from-library "flatten.scm")
(%test
  (flatten '(a (b ((c) d . e))))  ==>  (a b c d e)
)

(load-from-library "fluid-let-sr.scm")
(%test
  (let ((a 0))
    (let ((f (lambda () a)))
      (fluid-let ((a 1))
        (f))))                ==>  1
)

(load-from-library "fluid-let.scm")
(%test
  (let ((a 0))
    (let ((f (lambda () a)))
      (fluid-let ((a 1))
        (f))))                ==>  1
)

(load-from-library "for-all.scm")
(%test
  (for-all < '(1 7) '(2 8) '(3 9))  ==>  #t
  ; because (< 1 2 3) and (< 7 8 9)
)

(load-from-library "get-prop.scm")
(%test
  (get-prop '() 'foo)        ==>  #f
  (put-prop '() 'foo 42)     ==>  (foo 42)
  (get-prop '(foo 42) 'foo)  ==>  42
  (rem-prop '(foo 42) 'foo)  ==>  ()
)

(load-from-library "group.scm")
(%test
  (group '(1 2 3 4 5) 2)  ==>  ((1 2) (3 4) (5))
  (group '(1 2 3 4 5) 5)  ==>  ((1 2 3 4 5))
)

(load-from-library "hash-table.scm")
(%test
  (let ((h (make-hash-table)))
    (hash-table-set! h "key" 'value)
    (hash-table-ref  h "key"))        ==>  (value)
)

(load-from-library "hof.scm")
(%test
  ((complement pair?) '(1 2 3))  ==>  #f
  ((complement eq?) 'foo 'bar)   ==>  #t
;
  ((compose car cdr) '(1 2 3))         ==>  2
  ((compose list reverse list) 1 2 3)  ==>  ((3 2 1))
;
  ((const (+ 1 2)))        ==>  3
  ((const (+ 1 2)) 3 4 5)  ==>  3
;
  ((curry + 1) 9)              ==>  10
  ((curry map list) '(1 2 3))  ==>  ((1) (2) (3))
;
  ((curry  - 1) 10)  ==>  -9
  ((curryr - 1) 10)  ==>  9
;
  ((fork < car) '(1 . a) '(2 . b) '(3 . c))  ==>  #t
  ((fork append reverse) '(3 2 1) '(6 5 4))  ==>  (1 2 3 4 5 6)
)

(load-from-library "htmlify-char.scm")
(%test
  (htmlify-char #\<)      ==>  "&lt;"
  (htmlify-string "<&>")  ==>  "&lt;&amp;&gt;"
)

(load-from-library "hyper.scm")
(%test
  (hyper 4 3 3)  ==>  7625597484987
)

(load-from-library "id.scm")
(%test
  (true)          ==>  #t
  (false 1 2 3)   ==>  #f
  (id 'whatever)  ==>  whatever
)

(load-from-library "implode.scm")
(%test
  (implode '(b l a c k h o l e))  ==>  blackhole
)

(load-from-library "integer-sqrt.scm")
(%test
  (integer-sqrt 10)  ==>  3
)

(load-from-library "integer-to-binary-string.scm")
(%test
  (integer->binary-string 123 8)       ==>  "01111011"
  (binary-string->integer "01111011")  ==>  123
  (number-of-bits 127)                 ==>  7
)

(load-from-library "intersection.scm")
(%test
  (intersection '(v w x) '(w x y) '(x y z))  ==>  (x)
)

(load-from-library "iota.scm")
(%test
  (iota 7)       ==>  (1 2 3 4 5 6 7)
  (iota 17 21)   ==>  (17 18 19 20 21)
  (iota* 17 21)  ==>  (17 18 19 20)
  (iota* 1 1)    ==>  ()
)

(load-from-library "keyword-value.scm")
(%test
  (keyword-value '(foo 1 bar 2) 'bar)  ==>  2
  (keyword-value '(foo 1) 'bar 0)      ==>  0
;
  (accept-keywords "test" '(foo 1 bar 2) '(foo bar))  ==>  #t
)

(load-from-library "letcc.scm")
(%test
  (let/cc exit
    (letrec
      ((f (lambda (x)
            (cond ((null? x) 0)
                  ((pair? x) (+ 1 (f (cdr x))))
                  (else      (exit 'foo))))))
      (f '(1 2 3 . 4))))                         ==> foo
)

(load-from-library "letrecstar.scm")
(%test
  (letrec* ((a (lambda () (lambda () 1)))
            (b (a)))
    (b))                                  ==>  1
)

(load-from-library "list-copy.scm")
(%test
  (list-copy '(foo bar baz))  ==>  (foo bar baz)
)

(load-from-library "list-to-set.scm")
(%test
  (list->set '(a b c b c))  ==>  (a b c)
)

(load-from-library "listq.scm")
(%test
  (listq a (b c) d)  ==>  (a (b c) d)
)

(load-from-library "loutify-char.scm")
(%test
  (loutify-char #\")        ==>  "\"\\\"\""
  (loutify-string "\"x\"")  ==>  "\"\\\"x\\\"\""
)

(load-from-library "make-partitions.scm")
(%test
  (make-partitions 4)  ==>  ((4) (3 1) (2 2) (2 1 1) (1 1 1 1))
)

(load-from-library "matcher.scm")
(%test
  (begin
    (define-matcher len
      (()      => 0)
      ((_ . x) => (+ 1 (len x))))
    (len '(a b c d e f)))                  ==>  6
;
  (let-matcher how-many
    ((nil
       => 0)
     (_ @ more
       => (+ 1 (apply how-many more))))
    (how-many 1 2 3 4 5))                  ==> 5
;
  (let-matcher appnd
    ((() x      => x)
     ((h . t) x => (cons h (appnd t x))))
    (appnd '(a b c) '(d e f)))             ==>  (a b c d e f)
)

(load-from-library "mean.scm")
(%test
  (mean '(1 2 3 4 5 6))  ==>  3.5
)

(load-from-library "median.scm")
(%test
  (mean '(1 2 3 4 5 6))  ==>  3.5
)

(load-from-library "memoize.scm")
(%test
  (letrec
    ((fib
       (memoize
         (lambda (x)
           (if (< x 2)
               1
               (+ (fib (- x 1))
                  (fib (- x 2))))))))
    (fib 100))                         ==>  573147844013817084101
)

(load-from-library "memp.scm")
(%test
  (memp char=? #\b '(#\a #\b #\c))  ==>  (#\b #\c)
)

(load-from-library "merge.scm")
(%test
  (merge < '(1 3 5) '(2 4 6))  ==>  (1 2 3 4 5 6)
  (merge < '(1 5 3) '(2 4 6))  ==>  (1 2 5 3 4 6)
  (merge < '(3) '(1 2 4 5))    ==>  (1 2 3 4 5)
)

(load-from-library "mergesort.scm")
(%test
  (mergesort <= '(5 3 7 9 1))  ==>  (1 3 5 7 9)
)

(load-from-library "mode.scm")
(%test
  (mode '(1 2 3 3 4 5 5 6))  ==>  (3 5)
)

(load-from-library "name-to-file-name.scm")
(%test
   (name->file-name "sys:stat-pipe?")   ==>  "sys_stat-pipep"
   (name->file-name "a->b")             ==>  "a-to-b"
   (name->file-name "*foo*")            ==>  "starfoostar"
)

(load-from-library "package.scm")
(%test
  (begin
    (package bar
      (:export foo2 foo3)
      (:make-aliases)
      (define (foo-maker n x)
        (if (zero? n)
            (lambda ()
              x)
            (foo-maker
              (- n 1)
              (cons n x))))
      (define foo2 (foo-maker 2 '()))
      (define foo3 (foo-maker 3 '())))
    (list (bar:foo2) (foo3)))           ==>  ((1 2) (1 2 3))
)

(load-from-library "partition.scm")
(%test
  (partition even? '(1 2 3 4 5))  ==>  ((2 4) (1 3 5))
)

(load-from-library "permute.scm")
(%test
  (permute 2 '(a b c))   ==>  ((a b) (b a) (a c)
                               (c a) (b c) (c b))
;
  (permute* 2 '(a b c))  ==>  ((a a) (a b) (a c)
                               (b a) (b b) (b c)
                               (c a) (c b) (c c))
)

(load-from-library "position.scm")
(%test
  (position '(bar) '((foo) (bar) (baz)))  ==>  1
  (posv 4 '(0 1 2 3 4 5 6))               ==>  4
  (posq 'foo '(foo bar baz))              ==>  0
  (posp (lambda (x y) (= x (car y)))
        2
        '((0 . a) (1 . b) (2 . c)))       ==>  2
)

(load-from-library "programp.scm")
(%test
  (program? '(let ((x 1)) (cons x x)))  ==>  #t
)

(load-from-library "quartile.scm")
(%test
  (quartile '(1 2 3 4 5 6 7 ))  ==>  (2 4 6)
)

(load-from-library "queue.scm")
(%test
  (let ((q (make-queue)))
    (for-each (lambda (x) (queue! q x))
              '(a b c d e))
    (unqueue* q))            ==>  (a ((e) b c d e))
)

(load-from-library "quicksort.scm")
(%test
  (quicksort <= '(5 3 7 9 1))  ==>  (1 3 5 7 9)
)

(load-from-library "random-sort.scm")
(%test
  (random-sort '(1 2 3 4 5))  ==>  (2 3 5 1 4)
)

(load-from-library "random.scm")
(%test
  (list (random 100)
        (random 100)
        (random 100))  ==>  (5 47 68)
)

(load-from-library "range.scm")
(%test
  (range '(1 2 3 4 5))  ==>  (1 5)
)

(load-from-library "rb-tree.scm")
(%test
  (let ((tree (fold-left
                (lambda (t k)
                   (rbt-insert t k (make-string k #\x)))
                (make-rbt <)
                '(1 2 3 4 5 6 7))))
    (rbt-find tree 5))               ==>  "xxxxx"
)

(load-from-library "read-from-string.scm")
(%test
  (read-from-string "  (this \"is\" #(a) (list)) ; comment")
    ==>  ((this "is" #(a) (list)))
;
  (read-from-string "  (this \"is\" #(a) (list))  more text")
    ==>  ((this "is" #(a) (list)) . "  more text")
;
  (read-from-string ")")
    ==>  "read-from-string: unexpected closing parenthesis"
;
  (read-from-string "#<foo>")
    ==>  "unreadable expression #<foo>"
;
  (read-from-string "#<foo>" 'convert-unreadable #t)
    ==>  ("#<foo>")
)

(load-from-library "read-line.scm")
(%test
  (with-input-from-file "lib/read-line.scm" read-line)
    ==>  "; Scheme 9 from Empty Space, Function Library"
)

(load-from-library "records.scm")
(%test
  (record-ref (record (list 'name "Foo") (list 'value 31415))
              'name)
    ==> "Foo"
;
  (equal? (record (list 'name "Foo") (list 'value 31415))
          (record (list 'value 31415) (list 'name "Foo")))
    ==> #t
)

(load-from-library "regex.scm")
(%test
  (re-match (re-comp "^a[1-9]*z$") "a1289z")     ==>  ((0 6))
  (re-match (re-comp "a[1-9]+z") "___a123z___")  ==>  ((3 8))
  (re-match (re-comp "a[^1-9]+z") "a123z")       ==>  #f
  (re-match (re-comp "[1-9]*") "__1__")          ==>  ((2 3))
  (re-match (re-comp "[1-9]*") "_____")          ==>  ()
;
  (re-match (re-comp "f\\(.\\)\\(.\\)bar")
            "foobar")                      ==>  ((0 6) (1 2) (2 3))
;
  (re-match (re-comp "a\\(.\\)a") "aba_aca_ada" 'all)
                                           ==> (((0 3) (1 2))
                                                ((4 7) (5 6))
                                                ((8 11) (9 10)))
;
  (re-subst (re-comp "\\([0-9]+\\)\\.\\([0-9]+\\)\\.")
            "_01.1._31.12._"
            "\\2/\\1"
            'all)                          ==>  "_1/01_12/31_"
)

(load-from-library "remove.scm")
(%test
  (remp number? '(a 1 b 2 c 3))   ==>  (a b c)
  (remove '(b) '(a (b) (c) (b)))  ==>  (a (c))
  (remq 'b     '(a b c b d))      ==>  (a c d)
  (remv 3      '(3 1 2 3 1))      ==>  (1 2 1)
)

(load-from-library "replace.scm")
(%test
  (replace '(x) '(y) '(lambda (x) y))  ==>  (lambda (y) y)
)

(load-from-library "set-difference.scm")
(%test
  (set-difference '(a b c d e f) '(b d) '(a f))  ==>  (c e)
)

(load-from-library "setters.scm")
(%test
  (let ((stack (list 0 2 3 4)))
    (let ((x (pop! stack)))
      (push! 1 stack)
      (list x stack)))           ==>  (0 (1 2 3 4))
;
  (let ((x 1))
    (dec! x)
    x)          ==>  0
;
  (let ((a 0) (b 0) (c 0))
    (set-vars! a b c '(foo bar baz))
    (list a b c))                     ==>  (foo bar baz)
;
  (let ((a 0)
        (b 1))
    (swap! a b)
    (list a b))  ==>  (1 0)
)

(load-from-library "sieve.scm")
(%test
  (sieve 20)  ==>  (2 3 5 7 11 13 17 19)
)

(load-from-library "simple-modules.scm")
(%test
  (begin ; Note: BEGIN is only needed for automatic testing
    (module math
      (define* (fact x)
        (if (= 0 x) 1 (* x (fact (- x 1))))))
    (using math (fact)
      (fact 5)))                               ==> 120
)

(load-from-library "sort.scm")
(%test
  (sort <= '(5 3 7 9 1))  ==>  (1 3 5 7 9)
)

(load-from-library "split-url.scm")
(%test
  (split-url "ftp://example.org/foo.bar?a=1&b=2")
                          ==>  ("ftp"
                                "example.org"
                                "/foo.bar"
                                "bar"
                                (("a" . "1")
                                 ("b" . "2"))
                                #f)
)

(load-from-library "split.scm")
(%test
  (split '(1 2 3 4))    ==>  ((1 2) (3 4))
  (split '(1 2 3 4 5))  ==>  ((1 2 3) (4 5))
  (split '())           ==>  (() ())
)

(load-from-library "stddev.scm")
(%test
  (stddev '(1 1 2 1 1))  ==>  0.4
)

(load-from-library "streams.scm")
(%test
  (stream->list
    (append-streams (list->stream '(a b c))
                    (stream-iota 1 3)))   ==>  (a b c 1 2 3)
  (stream->list
    (filter-stream even?
                   (stream-iota 1 10)))   ==>  (2 4 6 8 10)
  (stream->list
    (map-stream (lambda (x) (* 7 x))
                (stream-iota 1 5)))       ==>  (7 14 21 28 35)
  (stream->list
    (stream-member (lambda (x) (= 27 x))
                   (stream-iota 1 30)))   ==>  (27 28 29 30)
  (stream-extract
    5
    (stream-iota 1 10000000))             ==>  (1 2 3 4 5)
)

(load-from-library "string-case.scm")
(%test
  (string-upcase "Hello, World!")    ==>  "HELLO, WORLD!"
  (string-downcase "Hello, World!")  ==>  "hello, world!"
)

(load-from-library "string-digest.scm")
(%test
  (string-digest "hello")  ==>  2107915172
)

(load-from-library "string-expand.scm")
(%test
  (let ((tab (integer->char 9)))
    (string-expand (string #\x tab #\y)))  ==>  "x       y"
)

(load-from-library "string-find-last.scm")
(%test
  (string-find-last "a" "aaaaa")        ==>  "a"
  (string-ci-find-last "A" "ab ac")     ==>  "ac"
  (string-find-last "ax" "ab ac")       ==>  #f
  (string-find-last-word "a" "ab a c")  ==>  "a c"
  (string-find-last-word "a" "ab ac")   ==>  #f
)

(load-from-library "string-find.scm")
(%test
  (string-find "ein" "gemeinsam")     ==>  "einsam"
  (string-find "people" "democracy")  ==>  #f
  (string-find-word "me" "test me")   ==>  "me"
  (string-find-word "me" "testme")    ==>  #f
)

(load-from-library "string-last-position.scm")
(%test
  (string-last-position "a" "aaaaa")        ==>  4
  (string-ci-last-position "A" "ab ac")     ==>  3
  (string-last-position "ax" "ab ac")       ==>  #f
  (string-last-word-position "a" "ab a c")  ==>  3
  (string-last-word-position "a" "ab ac")   ==>  #f
)

(load-from-library "string-map.scm")
(%test
  (string-map char-downcase "HELLO")  ==>  "hello"
  (let ((s (string-copy "HELLO!")))
    (string-map! char-downcase s)
    s)                                ==>  "hello!"
)

(load-from-library "string-parse.scm")
(%test
  (string-parse " ?" " to be  or  not to be? ")
    ==>  ("to" "be" "or" "not" "to" "be")
)

(load-from-library "string-position.scm")
(%test
  (string-position "ein" "gemeinsam")     ==>  3
  (string-position "people" "democracy")  ==>  #f
  (string-word-position "me" "test me")   ==>  5
  (string-word-position "me" "testme")    ==>  #f
)

(load-from-library "string-prefixeqp.scm")
(%test
  (string-prefix=? "foo" "foobar")  ==>  #t
  (string-prefix=? "foo" "fubar")   ==>  #f
)

(load-from-library "string-reverse.scm")
(%test
  (string-reverse "rats live on no evil star")
              ==> "rats live on no evil star"
)

(load-from-library "string-scan.scm")
(%test
  (string-scan #\y "xyz")  ==>  1
)

(load-from-library "string-split.scm")
(%test
  (string-split #\: "a::b:c:")  ==>  ("a" "" "b" "c" "")
)

(load-from-library "string-translate.scm")
(%test
  (string-translate "a:b:c" ":" "-")  ==> "a-b-c"
;
  (string-translate
    "hello, world!"
    "abcdefghijklmnopqrstuvwxyz"
    "nopqrstuvwxyzabcdefghijklm")     ==>  "uryyb, jbeyq!"
)

(load-from-library "string-unsplit.scm")
(%test
  (string-unsplit #\: '("" "a" "b" "" "c"))  ==>  ":a:b::c"
)

(load-from-library "sublist.scm")
(%test
  (sublist '(a b c d e) 2 4)  ==>  (c d)
  (sublist '(a b c d e) 2 2)  ==>  ()
)

(load-from-library "subsetp.scm")
(%test
  (subset? '(a) '(a b) '(a b) '(a b c d))  ==>  #t
  (subset? '(a b c))                       ==>  #t
)

(load-from-library "substitute.scm")
(%test
  (substitute '(* (+ 5 7) 9) '(((+ 5 7) . 12)))  ==>  (* 12 9)
)

(load-from-library "subvector.scm")
(%test
  (subvector '#(a b c d e) 2 4)  ==>  #(c d)
  (subvector '#(a b c d e) 2 2)  ==>  #()
)

(load-from-library "sum.scm")
(%test
  (sum 2 5)  ==>  14
)

(load-from-library "symbols.scm")
(%test
  (s9fes-syntax-objects)  ==>  ()
)

(load-from-library "t-sort.scm")
(%test
  (t-sort-net eq?
              '((dressed shoes hat)
                (shoes socks pants)
                (pants underpants)
                (hat pullover)
                (pullover shirt undershirt)
                (shirt undershirt)
                (underpants)))      ==>  (socks underpants pants
                                          shoes undershirt shirt
                                          pullover hat dressed)
;
  (let ((db '((a b c)
              (b u)
              (c v)
              (u x)
              (v y)
              (w z))))
    (t-sort eq? 'a (lambda (x)
                     (assq x db))
                   'top-down #t
                   'reverse #t))      ==>  (a b c u v x y)
;
  (t-sort-net eq? '((a b c d)))             ==>  (b c d a)
  (t-sort-net eq? '((a b c d)) 'strict #t)  ==>  #f
  (t-sort-net eq? '((a b) (b a)))           ==>  #f
  (t-sort-net eq? '((foo foo)) 'check #t)   ==>  (cyclic . foo)
)

(load-from-library "tagbody.scm")
(%test
  (let ((x   10)
        (x0  1)
        (x1  1))
    (tagbody
      fib
        (if (zero? x)
            (go end))
        (let ((t x1))
          (set! x1 (+ x0 x1))
          (set! x0 t))
        (set! x (- x 1))
        (go fib)
      end)
    x1)                       ==>  144
)

(load-from-library "take.scm")
(%test
  (take '(foo bar baz) 0)  ==>  ()
  (take '(foo bar baz) 1)  ==>  (foo)
  (take '(foo bar baz) 3)  ==>  (foo bar baz)
)

(load-from-library "transpose.scm")
(%test
  (transpose '((1 2 3) (4 5 6)))  ==>  ((1 4) (2 5) (3 6))
)

(load-from-library "tree-copy.scm")
(%test
  (tree-copy '(((a . b) (c . d)) (e . f)))
      ==>  (((a . b) (c . d)) (e . f))
;
  (let* ((tree  (list (string #\A)))
         (tree2 (tree-copy tree))
         (tree3 (tree-copy tree 'with-atoms)))
    (string-set! (car tree) 0 #\X)
    (list tree2 tree3))              ==>  (("X") ("A"))
)

(load-from-library "tree-equalp.scm")
(%test
  (tree-equal? (lambda (x y) #t)
               '(((a . b) (c . d)) (e . f))
               '(((1 . 2) (3 . 4)) (5 . 6)))  ==>  #t
;
  (tree-equal? eqv?
               '((1 . 2) (3 . 4))
               '((1 . 2) (3 4)))              ==> #f
)

(load-from-library "tree-map.scm")
(%test
  (tree-map number? list '((a . 1) (b . 2)))
    ==>  ((a . (1)) (b . (2)))
  (tree-map (lambda (x) (and (pair? x)
                             (string? (car x))
                             (string? (cdr x))))
            (lambda (x) (string-append (car x) (cdr x)))
            '(("foo" . "bar") ("bar" . "baz")))
    ==>  ("foobar" "barbaz")
)

(load-from-library "type-case.scm")
(%test
  (type-of type-of)  ==>  procedure
;
  (let ((x '#(1 2 3))
        (i 0))
    (type-case x
      ((string) (string-ref x i))
      ((vector) (vector-ref x i))
      (else     x)))               ==>  1
)

(load-from-library "union.scm")
(%test
  (union '(v w x) '(w x y) '(x y z))  ==>  (v w x y z)
)

(load-from-library "unsort.scm")
(%test
  (unsort '(1 2 3 4 5) 1)  ==>  (1 3 5 4 2)
)

(load-from-library "url-decode.scm")
(%test
  (url-decode "%46%4F%4FBAR")  ==>  "FOOBAR"
)

(load-from-library "variance.scm")
(%test
  (variance '(1 1 2 1 1))  ==>  0.16
)

(load-from-library "vector-map.scm")
(%test
  (vector-map + '#(1 2 3) '#(4 5 6))  ==>  #(5 7 9)
  (let ((v (vector 1 2 3)))
    (vector-map! - v)
    v)                                ==>  #(-1 -2 -3)
)

(load-from-library "when.scm")
(%test
  (when (= 1 1) 'foo 'bar 'baz)    ==>  baz
  (unless (= 1 2) 'foo 'bar 'baz)  ==>  baz
)

(load-from-library "while.scm")
(%test
  (let ((x 0)
        (y 1))
    (while (< x 10)
      (set! y (* 2 y))
      (set! x (+ 1 x)))
    y)                   ==>  1024
)

(load-from-library "write-to-string.scm")
(%test
  (write-to-string '(a 1 #\c #(v) #t "str" "\"s\"" (a . d)))
    ==>  "(a 1 #\\c #(v) #t \"str\" \"\\\"s\\\"\" (a . d))"
;
  (display-to-string '(a 1 #\c #(v) #t "str" "\"s\"" (a . d)))
    ==>  "(a 1 c #(v) #t str \"s\" (a . d))"
)

(load-from-library "format.scm")
(%test
  (format #f "~A ~:* ~S" '(#\c "s"))
    ==>  "(c s)  (#\\c \"s\")"
;
  (format #f "~20,'_,',,3:D" 123456789)
    ==>  "_________123,456,789"
;
  (format #f "~@{ ~A,~A ~}" 'a 1 'b 2 'c 3)
    ==>  " a,1  b,2  c,3 "
)

(load-from-library "pretty-print.scm")
(%test
  (pp-string '("(let ((a 1) (b 2))"
               "(cons a b))"))
                                   ==> ("(let ((a 1)"
                                        "      (b 2))"
                                        "  (cons a b))")
)

(load-from-library "prolog.scm")
(%test
  (begin (! (man socrates))
         (:- (mortal ?x)
             (man ?x))
         (query '(mortal ?who)))  ==>  (((who . socrates)))
)

(load-from-library "queens.scm")
(%test
  (queens 4)  ==>  ((2 0 3 1) (1 3 0 2))
)

(load-from-library "s9sos.scm")
(%test
  (begin
    (define-generic mul)
;
    (define-method (mul (x <integer>) (y <integer>))
      (* x y))
;
    (define-method (mul (x <integer>) (a <pair>))
      (map (lambda (i) (* i x)) a))
;
    (define-method (mul (a <pair>) (x <integer>))
      (map (lambda (i) (* i x)) a))
;
    (define-method (mul (a <pair>) (b <pair>))
      (map * a b))
;
    (list (mul 5 7)
          (mul 2 '(1 2 3))
          (mul '(1 2 3) 2)
          (mul '(1 2 3) '(4 5 6))))  ==>  (35
                                           (2 4 6)
                                           (2 4 6)
                                           (4 10 18))
;
  ; Don't do this! Generic application takes ages.
  (begin
    (define-generic len)
    (define-method (len (x <null>)) 0)
    (define-method (len (x <pair>))
      (+ 1 (len (cdr x))))
    (len '(1 2 3 4 5)))                 ==>  5
)

(load-from-library "scm2html.scm")
(%test
  (scm2html 'input-string: "'()")
     ==> (("c" #f quote 0 ())
          "<SPAN class=y><B>'</B></SPAN><SPAN class=c>()")
)

(load-from-library "string-locate.scm")
(%test
  (string-locate "test" "This is a test string")     ==>  10
  (string-locate "TEST" "This is a test string")     ==>  #f
)

(load-from-library "zebra.scm")
(%test
  (zebra)  ==>  (((norwegian kools _.0 fox yellow)
                  (ukrainian chesterfields tea horse blue)
                  (englishman oldgolds milk snails red)
                  (spaniard luckystrikes orangejuice dog ivory)
                  (japanese parliaments coffee zebra green)))
)

(load-from-library "basename.scm")
(%test
  (basename "/foo/bar/baz")     ==>  "baz"
  (basename "/goo/bar.Z" ".Z")  ==>  "bar"
)

(load-from-library "dirname.scm")
(%test
  (dirname "/foo/bar/baz")  ==>  "/foo/bar"
  (dirname "foo/bar")       ==>  "foo"
  (dirname "foo/bar/")      ==>  "foo"
  (dirname "/foo")          ==>  "/"
  (dirname "/")             ==>  "/"
  (dirname "foo")           ==>  "."
)

(load-from-library "format-time.scm")
(%test
  (format-time "~w ~4y-~@m-~2d ~2h:~2m:~2s"
               '(1 2009 3 9 8 53 20))
      ==> "Tue 2009-Mar-09 08:53:20"
)

(load-from-library "leap-yearp.scm")
(%test
  (leap-year? 2000)  ==>  #t
  (leap-year? 2003)  ==>  #f
)

(load-from-library "mode-to-string.scm")
(%test
  (mode->string #o5751)  ==>  "rwsr-x--t"
)

(load-from-library "parse-optionsb.scm")
(%test
  (option #\o 'string)             ==>  (#\o string (#f))
  (opt-char (option #\o 'string))  ==>  #\o
  (opt-arg? (option #\o 'string))  ==>  #t
  (opt-type (option #\o 'string))  ==>  string
  (opt-val  (option #\o 'string))  ==>  #f
;
  (parse-options! '("-o" "file1" "file2")
                  `(,(option #\o #t))
                  #f)                   ==>  ("file2")
)

(load-from-library "proper-timep.scm")
(%test
  (proper-time? '(3 1970 1 1 0 0 0))  ==>  #t
)

(load-from-library "time-ops.scm")
(%test
  (time-add '(0 2010 10 06 12 30 00) '(10 7 30 0))
    ==>  (5 2010 10 16 20 0 0)
  (time-difference '(0 2010 10 06 12 30 00)
                   '(5 2010 10 16 20 00 00))
    ==>  (10 7 30 0)
  (time-after? '(5 2010 10 16 20 00 00)
               '(0 2010 10 06 12 30 00))
    ==>  #t
)

(load-from-library "time-to-unix-time.scm")
(%test
  (time->unix-time '(6 2010 4 25 7 1 19))  ==>  1272178879
)

(load-from-library "unix-time-to-time.scm")
(%test
  (unix-time->time 1272178879)  ==>  (6 2010 4 25 7 1 19)
)

(if (= 0 Errors)
    (begin (display "Everything fine!")
           (newline)))
