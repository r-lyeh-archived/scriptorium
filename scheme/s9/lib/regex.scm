; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009,2010
; Placed in the Public Domain
;
; (re-comp string)                         ==>  list
; (re-match list string)                   ==>  list | #f
; (re-match list string 'symbol ...)       ==>  list | #f
; (re-subst list string1 string2)          ==>  list | #f
; (re-subst list string1 string2 'symbol)  ==>  list | #f
;
; (load-from-library "regex.scm")
;
; Compile, match, and substitute regular expressions.
;
; RE-COMP compiles a regular expression (RE) and returns it.
; Compiled REs (CREs) are represented by lists.
;
; RE-MATCH matches a compiled RE against a string. When (part of) the
; string matches the CRE, it returns a list of the form
;
;       ((first limit) ...)
;
; where FIRST is the index of the first character matching the RE and
; LIMIT is the first character *after* the match (so the match can be
; extracted with SUBSTRING). When no sub-matches are used, just a list
; of the form ((first limit)) will be returned. When sub-matches using
; \( and \) are contained in the regular expression, the subsequent pairs
; will contain the ranges matched by the sub-expressions (in order of
; occurrence of sub-expressions in the RE). See examples.
;
; When the CRE does not match the string, RE-MATCH returns #F.
;
; A special case occurs when a given pattern may match a string of zero
; length. In this case, RE-MATCH returns () when no better match could
; be found.
;
; When 'LAZY is passed as an additional argument to RE-MATCH, it matches
; repetitions "lazily," e.g.: "A+" will match "A" (rather than "AAA")
; in "AAAB".
;
; When 'ALL is passed as an additional argument to RE-MATCH, it will
; generate a list of *all* matches found in the given string, i.e. a
; list of the form:
;
;       (((first limit) ...) ...)
;
; The following RE patterns are evaluated:
; .          match any character
; [char...]  match character class (may contain ranges of the form c1-c2)
; ^          match beginning of line
; $          match end of line
; *          match zero or more instances of the preceding pattern
; +          match one or more instances of the preceding pattern
; ?          match the preceding pattern optionally
; \          match the following character literally (exception below!)
; \(         mark the beginning of a sub-match
; \)         mark the end of a sub-match
;
; RE-SUBST attempts to match the CRE LIST against STRING1. When it
; succeeds, it returns a fresh copy of STRING1 with the match replaced
; by STRING2. When the 'ALL option is used (see RE-MATCH), all occurrences
; of the match will be replaced. When the CRE contains sub-matches, they can
; be referred to in STRING2 by using a backslash and the position of the
; sub-match, i.e.: \1, \2, ... The notation \0 denotes the entire match. To
; include a literal backslash in the replacement string, use \\. (Of course,
; Scheme strings use backslashes for escaping, so you will have to use \\1
; in the place of \1 and \\\\ in the place of \\. Yes, this is awkward.)
; When SUBST fails to match the CRE, it returns #F.
;
; Example:   (re-match (re-comp "^a[1-9]*z$") "a1289z")     ==>  ((0 6))
;            (re-match (re-comp "a[1-9]+z") "___a123z___")  ==>  ((3 8))
;            (re-match (re-comp "a[^1-9]+z") "a123z")       ==>  #f
;            (re-match (re-comp "[1-9]*") "__1__")          ==>  ((2 3))
;            (re-match (re-comp "[1-9]*") "_____")          ==>  ()
;
;            (re-match (re-comp "f\\(.\\)\\(.\\)bar")
;                      "foobar")                      ==>  ((0 6) (1 2) (2 3))
;
;            (re-match (re-comp "a\\(.\\)a") "aba_aca_ada" 'all)
;                                                     ==> (((0 3) (1 2))
;                                                          ((4 7) (5 6))
;                                                          ((8 11) (9 10)))
;
;            (re-subst (re-comp "\\([0-9]+\\)\\.\\([0-9]+\\)\\.")
;                      "_01.1._31.12._"
;                      "\\2/\\1"
;                      'all)                          ==>  "_1/01_12/31_"

(load-from-library "and-letstar.scm")

(define (re-comp re)

  (define LP   #\()
  (define RP   #\))

  (define (make-range c0 cn cls)
      (if (> c0 cn)
          cls
          (make-range (+ 1 c0)
                      cn
                      (cons (integer->char c0) cls))))

  (define (compile-class in out cls first)
    (cond ((null? in)
            #f)
          ((char=? #\] (car in))
            (list (cdr in) (cons (reverse! cls) out)))
          ((and first (char=? #\^ (car in)))
            (compile-class (cdr in) out (list #\]) #f))
          ((and (not first)
                (not (null? (cdr cls)))
                (char=? #\- (car in))
                (pair? (cdr in))
                (not (char=? #\] (cadr in))))
            (let ((c0 (char->integer (car cls)))
                  (cn (char->integer (cadr in))))
              (if (< c0 cn)
                  (compile-class (cddr in)
                                 out
                                 (make-range c0 cn (cdr cls)) #f)
                  (compile-class (cdr in)
                                 out
                                 (cons #\- cls) #f))))
          (else
            (compile-class (cdr in)
                           out
                           (cons (car in) cls) #f))))

  (let compile ((in-sub #f)
                (in     (string->list re))
                (out    '()))
    (cond
      ((not in)
        #f)
      ((null? in)
        (if in-sub
            #f
            (reverse! out)))
      (else
        (case (car in)
              ((#\\)
                (if (pair? (cdr in))
                    (if (memv (cadr in) '(#\( #\)))
                        (if (or (and      in-sub  (char=? LP (cadr in)))
                                (and (not in-sub) (char=? RP (cadr in))))
                            #f
                            (compile (char=? (cadr in) LP)
                                     (cddr in)
                                     (cons (list (cadr in)) out)))
                        (compile in-sub
                                 (cddr in)
                                 (cons (cadr in) out)))
                    #f))
              ((#\^ #\$ #\.)
                (compile in-sub
                         (cdr in)
                         (cons (list (car in)) out)))
              ((#\* #\?)
                (compile in-sub
                         (cdr in)
                         (if (null? out)
                             (cons (car in) out)
                             (cons (list (car in) (car out))
                                   (cdr out)))))
              ((#\+)
                (compile in-sub
                         (cdr in)
                         (if (null? out)
                             (cons (car in) out)
                             (cons (list #\* (car out)) out))))
              ((#\[)
                (let ((class (compile-class (cdr in) out (list #\[) #t)))
                  (if class
                      (apply compile in-sub class)
                      #f)))
              (else
                (compile in-sub
                         (cdr in)
                         (cons (car in) out))))))))

(define (re-match cre s . opts)

  (define lazy        #f)
  (define all-matches #f)
  (define LP          #\()
  (define RP          #\))

  (define (match-char p c)
    (cond ((char? p)
            (char=? p c))
          ((char=? #\. (car p)))
          ((char=? #\[ (car p))
            (memv c (cdr p)))
          ((char=? #\] (car p))
            (not (memv c (cdr p))))
          (else
            #f)))

  (define (extend-match m n)
    (if (null? m)
        (list (list n 0))
        (cons (cons (+ 1 (caar m)) (cdar m)) (cdr m))))

  (define (make-choices cre s m)
    (if (or (null? s)
            (not (match-char (cadar cre) (car s))))
        (list (list s m))
        (cons (list s m)
              (make-choices cre (cdr s) (extend-match m 1)))))

  (define (begin-submatch m)
    (if (null? m)
        `((0 0) (0 0))
        `((,(caar m) ,(caar m)) ,(car m) ,@(cdr m))))

  (define (end-submatch m)
    (let ((sub  (car m))
          (main (cadr m)))
      `((,(car sub) ,(cadr main)) ,sub ,@(cddr m))))

  (define (match-cre cre s pos m)
    (cond
      ((null? cre)
        (map reverse! m))
      ((null? s)
        (cond ((equal? cre '((#\$)))
                (match-cre '() '() pos m))
              ((equal? cre `((,RP)))
                (match-cre '() '() pos (end-submatch m)))
              ((and (pair? (car cre))
                    (char=? #\* (caar cre))
                    (null? (cdr cre)))
                '())
              (else
                #f)))
      ((pair? (car cre))
        (cond ((char=? LP (caar cre))
                (match-cre (cdr cre)
                           s
                           pos
                           (begin-submatch m)))
              ((char=? RP (caar cre))
                (match-cre (cdr cre)
                           s
                           pos
                           (end-submatch m)))
              ((char=? #\* (caar cre))
                (match-star cre s pos m))
              ((char=? #\? (caar cre))
                (if (match-char (cadar cre) (car s))
                    (match-cre (cdr cre)
                               (cdr s)
                               (+ 1 pos)
                               (extend-match m 1))
                    (match-cre (cdr cre) s pos m)))
              ((match-char (car cre) (car s))
                (match-cre (cdr cre)
                           (cdr s)
                           (+ 1 pos)
                           (extend-match m 1)))
              (else
                #f)))
      ((char=? (car cre) (car s))
        (match-cre (cdr cre)
                   (cdr s)
                   (+ 1 pos)
                   (extend-match m 1)))
      (else
        #f)))

  (define (match-star cre s pos m)
    (let try-choices ((c* (if lazy
                              (make-choices cre s m)
                              (reverse (make-choices cre s m)))))
      (and (not (null? c*))
           (let ((r (match-cre (cdr cre) (caar c*) pos (cadar c*))))
             (or r
                 (try-choices (cdr c*)))))))

  (define (adjust res pos)
    (and res
         (let ((res (map (lambda (range)
                           (map (lambda (x)
                                  (+ x pos))
                                range))
                         res)))
           `(,(car res) ,@(reverse! (cdr res))))))

  (define (try-matches cre s pos)
    (cond ((null? s)
            (match-cre cre s pos '()))
          (else
            (let ((r (match-cre cre s pos '())))
              (if (or (not r)
                      (null? r)
                      (and (pair? r)
                           (null? (car r))))
                  (try-matches cre (cdr s) (+ 1 pos))
                  (adjust r pos))))))

  (define (collect-matches cre s pos)
    (let loop ((r  (try-matches cre s pos))
               (m* '()))
      (if (or (not r)
              (null? r))
          (reverse! m*)
          (let ((k (cadar r)))
            (loop (try-matches cre (list-tail s k) (+ k pos))
                  (cons r m*))))))

  (if (memq 'lazy opts)
      (set! lazy #t))

  (if (memq 'all opts)
      (set! all-matches #t))

  (cond ((equal? cre '((#\^)))
          (if all-matches
              `(((0 0)))
              `((0 0))))
        ((equal? cre '((#\$)))
          (let ((k (string-length s)))
            (if all-matches
                `(((,k ,k)))
                `((,k ,k)))))
        ((and (pair? cre)
              (equal? '(#\^) (car cre)))
          (let ((m (adjust (match-cre (cdr cre) (string->list s) 0 '()) 0)))
            (if all-matches
                (list m)
                m)))
        (all-matches
          (collect-matches cre (string->list s) 0))
        (else
          (try-matches cre (string->list s) 0))))

(define (replacement-string new m*)
  (letrec
    ((append-match
       (lambda (m* i out)
         (let loop ((in  (string->list (list-ref m* i)))
                    (out out))
           (if (null? in)
               out
               (loop (cdr in) (cons (car in) out)))))))
    (let ((k (string-length new))
          (n (length m*)))
      (let loop ((i   0)
                 (out '()))
        (cond ((>= i k)
                (list->string (reverse! out)))
              ((char=? #\\ (string-ref new i))
                (cond ((and-let* ((_ (< i (- k 1)))
                                  (c (string-ref new (+ 1 i)))
                                  (_ (char<=? #\0 c #\9))
                                  (j (- (char->integer c)
                                        (char->integer #\0)))
                                  (_ (<= 0 j n)))
                         j)
                         => (lambda (j)
                              (loop (+ 2 i) (append-match m* j out))))
                       ((< i (- k 1))
                         (loop (+ 2 i) (cons (string-ref new (+ 1 i)) out)))
                       (else
                         (loop (+ 1 i) out))))
                (else
                  (loop (+ 1 i) (cons (string-ref new i) out))))))))

(define re-subst
  (let ((replacement-string replacement-string))
    (lambda (cre old new . opts)
      (and-let* ((pos** (apply re-match cre old opts))
                 (_     (not (null? pos**)))
                 (pos** (if (memq 'all opts)
                            pos**
                            (list pos**)))
                 (m**   (map (lambda (pos*)
                               (map (lambda (pos)
                                      (substring old (car pos) (cadr pos)))
                                    pos*))
                             pos**)))
        (let ((repl* (map (lambda (m*)
                            (replacement-string new m*))
                          m**)))
          (let loop ((pos*  (append '((0 0))
                                    (map car pos**)
                                    (list (list (string-length old)
                                                (string-length old)))))
                     (repl* repl*)
                     (out   '()))
            (cond ((null? (cdr pos*))
                    (apply string-append (reverse! out)))
                  (else
                    (let ((s (substring old (cadar pos*) (caadr pos*)))
                          (r (if (null? repl*) "" (car repl*))))
                      (loop (cdr pos*)
                            (if (null? repl*) '() (cdr repl*))
                            (cons r (cons s out))))))))))))
