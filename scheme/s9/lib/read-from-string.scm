; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009,2010
; Placed in the Public Domain
;
; (read-from-string string <option> ...)  ==>  object
;
; Read a datum from a string. READ-FROM-STRING is like READ but it
; takes its input from a string instead of a port. It returns a pair
; containing the datum read in case of success. If there are any
; trailing characters after the extracted datum, the trailing string
; is placed in the cdr part of the returned pair. If the trailing
; string is empty, the cdr part is set to (). When an empty string
; or a string consisting of a comment exclusively is passed to
; READ-FROM-STRING, it returns (). In case of an error, a string
; explaining the cause of the error is returned.
;
; When the 'CONVERT-UNREADABLE option with a non-#F value is passed
; to READ-FROM-STRING, it will write the informal representation of
; #<...> expressions to a string and include that string in the
; resulting form in the place of the unreadable expression. By default
; it will signal an error when an unreadable expression is found.
;
; Example:   (read-from-string "  (this \"is\" #(a) (list)) ; comment")
;              ==>  ((this "is" #(a) (list)))
;
;            (read-from-string "  (this \"is\" #(a) (list))  more text")
;              ==>  ((this "is" #(a) (list)) . "  more text")
;
;            (read-from-string ")")
;              ==>  "read-from-string: unexpected closing parenthesis"
;
;            (read-from-string "#<foo>")
;              ==>  "unreadable expression #<foo>"
;
;            (read-from-string "#<foo>" 'convert-unreadable #t)
;              ==>  ("#<foo>")

(load-from-library "keyword-value.scm")

(define (read-from-string s . opts)

  (define LPAREN #\()
  (define RPAREN #\))

  (define convert-unreadable #f)

  (define separator?
    (let ((separators
            (append (string->list "#'(),;`\"")
                    (list #\space
                          (integer->char 9)
                          (integer->char 10)
                          (integer->char 12)
                          (integer->char 13)))))
    (lambda (c)
      (and (memv c separators) #t))))

  (define (skip-blanks s)
    (cond ((null? s)
            '())
          ((char-whitespace? (car s))
            (skip-blanks (cdr s)))
          (else
            s)))

  (define (skip-to-next-line s)
    (cond ((null? s)
            '())
          ((char=? #\newline (car s))
            (skip-blanks (cdr s)))
          (else
            (skip-to-next-line (cdr s)))))

  (define (digit-value c)
    (- (char->integer c)
       (char->integer #\0)))

  (define char-symbolic?
    (let ((symbol-chars (string->list "+-.*/<=>!?@:$%_&~^")))
      (lambda (c)
        (or (char-alphabetic? c)
            (and (memv c symbol-chars) #t)))))

  (define (extract-symbol s)
    (letrec
      ((extract-symbol2
         (lambda (s sym)
           (cond ((or (null? s)
                      (separator? (car s)))
                   (cons (list->string (reverse! sym)) s))
                 (else
                   (extract-symbol2 (cdr s) (cons (car s) sym)))))))
      (extract-symbol2 s '())))

  (define (read-symbol s)
    (let ((x (extract-symbol s)))
      (cons (string->symbol (car x)) (cdr x))))

  (define (read-symbol-or-number s)
    (let ((x (extract-symbol s)))
      (cond ((string->number (car x))
              => (lambda (n)
                   (cons n (cdr x))))
            (else
              (cons (string->symbol (car x)) (cdr x))))))

  (define (read-string s)
    (letrec
      ((rev-lst->str!
         (lambda (s)
           (list->string (reverse! s))))
       (read-string3
         (lambda (s t q)
           (cond ((null? s)
                   "read-from-string: unterminated string literal")
                 ((and (not q)
                       (char=? #\" (car s)))
                   (cons (rev-lst->str! t) (cdr s)))
                 ((char=? #\\ (car s))
                   (read-string3 (cdr s)
                                 (cons (car s) t) #t))
                 (else
                   (read-string3 (cdr s)
                                 (cons (car s) t) #f))))))
      (read-string3 (cdr s) '() #f)))

  (define (read-character s)
    (cond ((null? (cddr s))
            "read-from-string: bad char literal")
          ((null? (cdddr s))
            (cons (caddr s) (cdddr s)))
          ((separator? (cadddr s))
            (cons (caddr s) (cdddr s)))
          (else
            (let ((r (read-symbol (cddr s))))
              (case (car r)
                    ((space)   (cons #\space (cdr r)))
                    ((newline) (cons #\newline (cdr r)))
                    (else      "read-from-string: bad char name"))))))

  (define (read-dotted-cdr s lst)
    (let ((s (skip-blanks (cdr s))))
      (if (or (null? s)
              (char=? RPAREN (car s)))
          "read-from-string: missing cdr part in dotted pair"
          (let ((x (char-list->datum s)))
            (if (pair? x)
                (let ((s (skip-blanks (cdr x))))
                  (if (or (null? s)
                          (not (char=? RPAREN (car s))))
                      (string-append "read-from-string: missing closing"
                                     " parenthesis in dotted list")
                      (cons (append (reverse! lst) (car x)) (cdr s))))
                x)))))

  (define (read-pair s)
    (letrec
      ((read-list
         (lambda (s lst)
           (let ((s (if (and (pair? s)
                             (char-whitespace? (car s)))
                        (skip-blanks (cdr s))
                        s)))
             (cond ((null? s)
                     "read-from-string: missing closing parenthesis")
                   ((char=? RPAREN (car s))
                     (cons (reverse! lst) (cdr s)))
                   ((and (char=? #\. (car s))
                         (pair? (cdr s))
                         (separator? (cadr s)))
                     (read-dotted-cdr s lst))
                   (else
                     (let ((x (char-list->datum s)))
                       (if (pair? x)
                           (read-list (cdr x)
                                      (cons (car x) lst))
                           "read-from-string: unexpected end of list"))))))))
      (read-list (cdr s) '())))

  (define (read-based-number base s)
    (let ((x (extract-symbol s)))
      (cond ((string->number (car x) base)
              => (lambda (n)
                   (cons n (cdr x))))
            (else
              (string-append
                "read-from-string: invalid "
                (case base
                      ((2)  "binary")
                      ((8)  "octal")
                      ((10) "decimal")
                      ((16) "hexa-decimal"))
                " numeric literal: "
                (car x))))))

  (define (read-unreadable s r)
    (cond ((char=? #\> (car s))
            (cons (list->string (reverse! (cons #\> r)))
                  (cdr s)))
          ((char=? #\newline (car s))
            "unreadable expression")
          (else
            (read-unreadable (cdr s) (cons (car s) r)))))

  (define (read-hash s)
    (let ((s1 (if (pair? (cdr s)) (cadr s) #\<)))
      (case s1
            ((#\t) (cons #t (cddr s)))
            ((#\f) (cons #f (cddr s)))
            ((#\b) (read-based-number 2 (cddr s)))
            ((#\d) (read-based-number 10 (cddr s)))
            ((#\o) (read-based-number 8 (cddr s)))
            ((#\x) (read-based-number 16 (cddr s)))
            ((#\\) (read-character s))
            ((#\() (let ((x (read-pair (cdr s))))
                     (if (pair? x)
                         (if (list? (car x))
                             (cons (list->vector (car x)) (cdr x))
                             "read-from-string: bad vector syntax")
                         x)))                      ; #\) balance parens
            ((#\<) (let ((x (read-unreadable (cdr s) (list #\#))))
                     (if convert-unreadable
                         x
                         (string-append "unreadable expression "
                                        (car x)))))
            (else  "read-from-string: bad # syntax"))))

  (define (read-quote s q)
    (let ((x (char-list->datum (cdr s))))
      (cond ((pair? x)
              (cons (list q (car x)) (cdr x)))
            ((null? x)
              (string-append "read-from-string: object expected after "
                             (symbol->string q)))
            (else
              x))))

  (define (char-list->datum s)
    (let ((s (skip-blanks s)))
      (cond ((null? s)
              '())
            ((char=? #\; (car s))
              (char-list->datum (skip-to-next-line s)))
            ((or (char-symbolic? (car s))
                 (char-numeric? (car s)))
              (read-symbol-or-number s))
            ((or (char=? #\+ (car s))
                 (char=? #\- (car s)))
              (read-symbol-or-number s))
            ((char=? #\" (car s))
              (read-string s))
            ((char=? #\# (car s))
              (read-hash s))
            ((char=? #\' (car s))
              (read-quote s 'quote))
            ((char=? #\` (car s))
              (read-quote s 'quasiquote))
            ((char=? #\, (car s))
              (if (and (not (null? (cdr s)))
                       (char=? #\@ (cadr s)))
                  (read-quote (cdr s) 'unquote-splicing)
                  (read-quote s 'unquote)))
            ((char=? LPAREN (car s))
              (read-pair s))
            ((char=? RPAREN (car s))
              "read-from-string: unexpected closing parenthesis")
            (else
              (string-append
                "read-from-string: can't parse this: "
                (list->string s))))))

  (define (string->datum s)
    (char-list->datum (string->list s)))

  (accept-keywords "read-from-string" opts '(convert-unreadable))
  (set! convert-unreadable (keyword-value opts 'convert-unreadable #f))

  (let ((r (string->datum s)))
    (if (pair? r)
        (if (null? (cdr r))
            (list (car r))
            (let ((r2 (char-list->datum (cdr r))))
              (if (null? r2)
                  (list (car r))
                  (cons (car r) (list->string (cdr r))))))
        r)))
