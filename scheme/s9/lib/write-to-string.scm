; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; Placed in the Public Domain
;
; (write-to-string object)    ==>  string
; (display-to-string object)  ==>  string
;
; (load-from-library "write-to-string.scm")
;
; Write the external representation of the given OBJECT to a fresh
; string. WRITE-TO-STRING is like WRITE but writes its output to a
; string instead of an output port. DISPLAY-TO-STRING is like
; DISPLAY but writes its output to a string.
;
; Example:   (write-to-string '(a 1 #\c #(v) #t "str" "\"s\"" (a . d)))
;              ==>  "(a 1 #\\c #(v) #t \"str\" \"\\\"s\\\"\" (a . d))"
;
;            (display-to-string '(a 1 #\c #(v) #t "str" "\"s\"" (a . d)))
;              ==>  "(a 1 c #(v) #t str \"s\" (a . d))"

(define (make-string-writer readable)
  (lambda (x)

    (define (stringify-improper-list a first)
      (cond
        ((pair? a)
          (string-append (if first "" " ")
                         (to-string (car a))
                         (stringify-improper-list (cdr a) #f)))
        ((null? a)
          "")
        (else
          (string-append " . " (to-string a)))))

    (define (char->string c)
      (if readable
          (let ((v (char->integer c)))
            (cond ((= v 10)
                    "#\\newline")
                  ((= v 32)
                    "#\\space")
                  ((or (<= 0 v 31)
                       (> v 126))
                    (string-append "#<unrepresentable character, code="
                                   (number->string v)
                                   ">"))
                  (else
                    (string-append "#\\" (string c)))))
          (string c)))

    (define (quote-string s)
      (list->string
        (let q ((si (string->list s))
                (so '()))
          (cond ((null? si)
                  (reverse! so))
                ((char=? #\\ (car si))
                  (q (cdr si) (append (list #\\ #\\) so)))
                ((char=? #\" (car si))
                  (q (cdr si) (append (list #\" #\\) so)))
                (else
                  (q (cdr si) (cons (car si) so)))))))

    (define (to-string x)
      (cond ((eq? #t x)
              "#t")
            ((eq? #f x)
              "#f")
            ((symbol? x)
              (symbol->string x))
            ((number? x)
              (number->string x))
            ((char? x)
              (char->string x))
            ((string? x)
              (if readable
                  (string-append "\"" (quote-string x) "\"")
                  x))
            ((null? x)
              "()")
            ((pair? x)
              (if (and (pair? (cdr x))
                       (null? (cddr x)))
                  (case (car x)
                    ((quote)
                      (string-append "'" (to-string (cadr x))))
                    ((quasiquote)
                      (string-append "`" (to-string (cadr x))))
                    ((unquote)
                      (string-append "," (to-string (cadr x))))
                    ((unquote-splicing)
                      (string-append ",@" (to-string (cadr x))))
                    (else
                      (string-append "("
                                     (stringify-improper-list x #t)
                                     ")")))
                  (string-append "("
                                 (stringify-improper-list x #t)
                                 ")")))
            ((vector? x)
              (string-append "#" (to-string (vector->list x))))
            ((procedure? x)
              "#<procedure>")
            ((eof-object? x)
              "#<eof>")
            (else
              "#<unspecific>")))

    (to-string x)))

(define write-to-string (make-string-writer #t))

(define display-to-string (make-string-writer #f))
