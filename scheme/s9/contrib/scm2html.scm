; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010-2015
; Placed in the Public Domain
;
; (scm2html <option> ...)  ==>  string | unspecific
;
; Render Scheme code in HTML with syntax highlighting and optional
; CSS-based paren-matching. Input is read from (current-input-stream)
; and output is written to (current-output-stream) unless the
; 'INPUT-STRING: option is specified (see below).
;
; The rendition of the Scheme code will be placed in a PRE container
; of the class "scheme" (<PRE class=scheme>). When paren-matching is
; enabled, the class will change to "scheme-hl".
;
; The following container classes are used to specify the colors
; and other styles of the individual elements:
;
;       o  comment
;       p  parenthesis
;       s  symbol
;       c  constant
;       r  R4RS procedure
;       y  R4RS syntax
;       x  S9fES procedure
;       z  S9fES syntax
;       m  normal form
;       n  nested expression (for paren matching)
;
; See the "scheme.css" style sheet for examples.
;
; The following <option>s exist;
;
; 'FULL-HTML: boolean
;       When set to #T, SCM2HTML will output a full HTML document
;       and not just a PRE container. Will not work in string mode.
;
; 'LOUT-MODE: boolean
;       Generate Lout output rather than HTML output.
;
; 'INPUT-STRING: string
;       Input is read from a string and output is written to a string.
;       In string mode, the 'FULL-HTML: option does not work. When this
;       option is set, the result of the procedure will be of the form:
;
;               (attributes string)
;
;       where STRING is the output of the rendering process. See the
;       description of 'INITIAL-STYLE: for more information on the
;       ATTRIBUTES part. The output string of SCM2HTML is always
;       lacking a trailing </SPAN> element.
;
; 'INITIAL-STYLE: list
;       Initialize the color class and boldface flag with the values taken
;       from LIST. LIST should be the car part of an object returned by
;       SCM2HTML previously. It allows to render multiple lines that are
;       logically connected by preserving the style across line boundaries.
;
; 'MARK-S9-PROCS: boolean
;       When set to #T, S9fES procedures will be highlighted with an
;       extra color. Otherwise, they will be rendered in the same color
;       as user-defined symbols.
;
; 'MARK-S9-EXTNS: boolean
;       When set to #T, S9fES syntax extensions will be highlighted with
;       an extra color. Otherwise, they will be rendered in the same color
;       as user-defined symbols.
;
; 'SHOW-MATCHES: boolean
;       When set to #T, SCM2HTML will insert CSS code that allow to match
;       parentheses interactively in the resulting code by moving the
;       cursor over expressions. Does not work in string mode.
;
; 'TILDE-QUOTES: boolean
;       When set to #T, #\~ characters in programs will serve is
;       invisible quotation. Used to facilitate the rendering of
;       evaluation sequences.
;
; 'TERMINATE: list
;       Return termination tags for the color and boldface settings
;       specified in LIST (see INPUT-STRING:).
;
; Example:   (scm2html 'input-string: "'()")
;               ==> (("c" #f quote 0 ())
;                    "<SPAN class=y><B>'</B></SPAN><SPAN class=c>()")

(load-from-library "keyword-value.scm")
(load-from-library "symbols.scm")
(load-from-library "read-line.scm")
(load-from-library "setters.scm")
(load-from-library "hof.scm")
(load-from-library "htmlify-char.scm")
(load-from-library "loutify-char.scm")

(define (scm2html . options)

  (define *load-from-library* 0)
  (define *input-string* #f)
  (define *output-string* #f)

  (define END-OF-INPUT (list 'EOI))

  (define LP #\()
  (define RP #\))
  (define LB #\[)
  (define RB #\])

  (define (Prolog)
    (let ((p (cond (show-matches
                     '("<PRE class=scheme-hl>"))
                   (lout-mode
                     '("@Pre{"))
                   (else
                     '("<PRE class=scheme>")))))
      (if full-html
          (append
           '("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\""
              "  \"http://www.w3.org/TR/html4/loose.dtd\">"
              "<HTML>"
              "<HEAD>"
              "<TITLE></TITLE>"
              "<LINK rel=\"stylesheet\" type=\"text/css\" href=\"scheme.css\">"
              "</HEAD>"
              "<BODY>")
            p)
          p)))

  (define (Epilog)
    (change-color #f #f #f)
    (let ((p (if lout-mode
                 '("}")
                 '("</PRE>"))))
      (if full-html
          (append p '("</BODY>" "</HTML>"))
          p)))

  (define (end-of-input? x)
    (if *input-string*
        (eq? x END-OF-INPUT)
        (eof-object? x)))

  (define (next-char)
    (if (not *input-string*)
        (read-char)
        (if (null? *input-string*)
            END-OF-INPUT
            (pop! *input-string*))))

  (define (peek-next-char)
    (if (not *input-string*)
        (peek-char)
        (if (null? *input-string*)
            END-OF-INPUT
            (car *input-string*))))

  (define (output x)
    (if *output-string*
        (push! x *output-string*)
        (display x)))

  (define (output-string)
    (apply string-append
           (map (lambda (x)
                  (if (string? x)
                      x
                      (string x)))
                (reverse! *output-string*))))

  (define (output* lines)
    (let* ((l*    (reverse lines))
           (last  (car l*))
           (lines (reverse! (cdr l*))))
      (for-each (lambda (s)
                  (output s)
                  (output #\newline))
                lines)
      (output last)))

  (define Color-comment     "o")
  (define Color-paren       "p")
  (define Color-symbol      "s")
  (define Color-constant    "c")
  (define Color-std-proc    "r")
  (define Color-std-syntax  "y")
  (define Color-ext-proc    "x")
  (define Color-ext-syntax  "z")
  (define Color-normal-form "m")

  (define *Color*        #f)
  (define *Bold*         #f)
  (define *Qtype*        #f)
  (define *Parens*        0)
  (define *Paren-stack* '())

  (define (escaped-output s)
    (if lout-mode
        (output (apply string-append
                       (map loutify-char
                            (string->list s))))
        (output (htmlify-string s))))

  (define (change-color quoted co bo)
    (cond (quoted)
          ((and (equal? co *Color*) (eq? bo *Bold*)))
          (else
            (if *Bold*
                (if lout-mode
                    (output "}")
                    (output "</B>")))
            (if *Color*
                (if lout-mode
                    (output "}")
                    (output "</SPAN>")))
            (if co
                (if lout-mode
                    (begin (output "@S_")
                           (output co)
                           (output "{"))
                    (begin (output "<SPAN class=")
                           (output co)
                           (output ">"))))
            (if bo
                (if lout-mode
                    (output "@B{")
                    (output "<B>")))
            (set! *Color* co)
            (set! *Bold* bo))))

  (define (with-color quoted co thunk)
    (change-color quoted co #f)
    (thunk))

  (define (with-bold-color quoted co thunk)
    (change-color quoted co #t)
    (thunk))

  (define symbolic?
    (let ((specials "!#$%&*+-./:<=>?@^_~"))
      (lambda (c)
        (or (char-alphabetic? c)
            (char-numeric? c)
            (and (memv c (string->list specials)) #t)))))

  (define (print-paren c q)
    (cond (show-matches
            (if *Bold*
                (begin (output "</B>")
                       (set! *Bold* #f)))
            (output "</SPAN>")
            (if (or (char=? c LP) (char=? c LB))
                (output "<SPAN class=n>"))
            (output "<SPAN class=")
            (output (if q Color-constant Color-paren))
            (output ">")
            (set! *Color* #f)
            (output c)
            (if (or (char=? c RP) (char=? c RB))
                (output "</SPAN></SPAN>")))
          (else
            (with-color q
                        Color-paren
                        (lambda ()
                          (output c)))))
    (next-char))

  (define (r4rs-syntax? s)
    (and (memq (string->symbol s)
               (r4rs-syntax-objects))
         #t))

  (define (s9fes-syntax? s)
    (and mark-s9-procs
         (memq (string->symbol s)
               (s9fes-syntax-objects))
         #t))

  (define (r4rs-procedure? s)
    (and (memq (string->symbol s)
               (r4rs-procedures))
         #t))

  (define (s9fes-procedure? s)
    (and mark-s9-procs
         (memq (string->symbol s)
               (s9fes-procedures))
         #t))

  (define (s9fes-extension? s)
    (and mark-s9-extns
         (or (memq (string->symbol s)
                   (s9fes-extension-procedures))
             (memq (string->symbol s)
                   (s9fes-extension-symbols)))
         #t))

  (define (collect pred c s)
    (if (pred c)
        (collect pred (next-char) (cons c s))
        (cons c (list->string (reverse! s)))))

  (define (print-symbol-or-number c q)
    (let ((c/s (collect symbolic? c '())))
      (cond ((string->number (cdr c/s))
              (with-color q
                          Color-constant
                          (lambda () (escaped-output (cdr c/s)))))
            ((r4rs-syntax? (cdr c/s))
              (with-bold-color q
                               Color-std-syntax
                               (lambda () (escaped-output (cdr c/s)))))
            ((r4rs-procedure? (cdr c/s))
              (with-color q
                          Color-std-proc
                          (lambda () (escaped-output (cdr c/s)))))
            ((s9fes-syntax? (cdr c/s))
              (with-bold-color q
                               Color-ext-syntax
                               (lambda () (escaped-output (cdr c/s)))))
            ((s9fes-procedure? (cdr c/s))
              (if (string=? "load-from-library" (cdr c/s))
                  (set! *load-from-library* 2))
              (with-color q
                          Color-ext-proc
                          (lambda () (escaped-output (cdr c/s)))))
            ((s9fes-extension? (cdr c/s))
              (with-color q
                          Color-ext-proc
                          (lambda () (escaped-output (cdr c/s)))))
            (else
              (with-color q
                          Color-symbol
                          (lambda () (escaped-output (cdr c/s))))))
      (car c/s)))

  (define (print-const s q)
    (with-color q
                Color-constant
                (lambda () (escaped-output s)))
    (next-char))

  (define (print-string c q)
    (letrec
      ((collect-string
         (lambda (c s esc)
           (cond ((end-of-input? c)
                   (error "scm2html: unexpected EOF in string literal"))
                 ((and (char=? c #\")
                       (not esc))
                   (list->string (reverse! (cons #\" s))))
                 (else
                   (collect-string (next-char)
                                   (cons c s)
                                   (and (not esc) (char=? #\\ c))))))))
      (let* ((s  (collect-string c '() #t))
             (s2 (substring s 1 (- (string-length s) 1))))
        (if (and (not lout-mode)
                 (= *load-from-library* 1))
            (with-color q
                        Color-constant
                        (lambda ()
                          (output "\"<A href=\"")
                          (output s2)
                          (let ((k (string-length s2)))
                            (if (not (and (> k 3)
                                          (string=? (substring s2 (- k 4) k)
                                                    ".scm")))
                                (output ".scm")))
                          (output ".html\">")
                          (escaped-output s2)
                          (output "</A>\"")))
            (with-color q
                        Color-constant
                        (lambda () (escaped-output s)))))
      (next-char)))

  (define (print-comment c)
    (let ((col (if *Color* *Color* Color-symbol)))
      (with-color #f
                  Color-comment
                  (lambda ()
                    (escaped-output
                      (cdr (collect (curry (compose not char=?)
                                           #\newline)
                                    c
                                    '())))))
      (with-color #f col (lambda () #t)))
    #\newline)

  (define (print-name pre c q)
    (with-color q
                Color-constant
                (lambda ()
                  (escaped-output "#")
                  (escaped-output pre)
                  (let ((c/s (collect (lambda (c)
                                        (or (char-alphabetic? c)
                                            (char-numeric? c)))
                                      (next-char)
                                      (list c))))
                    (escaped-output (cdr c/s))
                    (car c/s)))))

  (define (print-unreadable c q)
    (with-color q
                Color-ext-syntax
                (lambda ()
                  (escaped-output "#")
                  (let ((c/s (collect (lambda (c)
                                        (not (char=? #\> c)))
                                      (next-char)
                                      (list c))))
                    (escaped-output (cdr c/s))
                    (escaped-output ">")
                    (next-char)))))

  (define (print-shbang)
    (with-bold-color #f
                     Color-ext-syntax
                     (lambda ()
                       (escaped-output "#!")))
    (with-color #f
                Color-comment
                (lambda ()
                  (output (read-line))))
    #\newline)

  (define (print-block-comment)
    (let ((comment-color
            (lambda ()
              (change-color #f Color-comment #f)))
          (syntax-color
            (lambda ()
              (change-color #f Color-ext-syntax #t)))
          (flush
            (lambda (s)
              (cond ((negative? s) (escaped-output "|"))
                    ((positive? s) (escaped-output "#"))))))
      (with-color #f
                  Color-ext-syntax
                  (lambda ()
                    (escaped-output "#|")))
      (with-color #f
                  Color-comment
                  (lambda ()
                    (let loop ((c (next-char))
                               (s 0)
                               (n 0))
                      (cond ((end-of-input? c)
                              (error "scm2html: unexpected EOF"))
                            ((char=? #\# c)
                              (if (negative? s)
                                  (if (positive? n)
                                      (begin (syntax-color)
                                             (escaped-output "|#")
                                             (comment-color)
                                             (loop (next-char) 0 (- n 1))))
                                  (begin (flush s)
                                         (loop (next-char) 1 n))))
                            ((char=? #\| c)
                              (if (positive? s)
                                  (begin (syntax-color)
                                         (escaped-output "#|")
                                         (comment-color)
                                         (loop (next-char) 0 (+ 1 n)))
                                  (begin (flush s)
                                         (loop (next-char) -1 n))))
                            (else
                              (flush s)
                              (escaped-output (string c))
                              (loop (next-char) 0 n))))))
      (with-color #f
                  Color-ext-syntax
                  (lambda ()
                    (escaped-output "|#")))
      (next-char)))

  (define (print-hash-syntax c q)
    (let ((c (next-char)))
      (case c
            ((#\f) (print-const "#f" q))
            ((#\F) (print-const "#F" q))
            ((#\t) (print-const "#t" q))
            ((#\T) (print-const "#T" q))
            ((#\e) (print-name "e" (next-char) q))
            ((#\E) (print-name "E" (next-char) q))
            ((#\i) (print-name "i" (next-char) q))
            ((#\I) (print-name "I" (next-char) q))
            ((#\b) (print-name "b" (next-char) q))
            ((#\B) (print-name "B" (next-char) q))
            ((#\d) (print-name "d" (next-char) q))
            ((#\D) (print-name "D" (next-char) q))
            ((#\o) (print-name "o" (next-char) q))
            ((#\O) (print-name "O" (next-char) q))
            ((#\x) (print-name "x" (next-char) q))
            ((#\X) (print-name "X" (next-char) q))
            ((#\\) (print-name "\\" (next-char) q))
            ((#\|) (print-block-comment))
            ((#\() (print-vector q))
            ((#\<) (print-unreadable c q))
            ((#\!) (print-shbang))
            (else  (error "scm2html: unknown # syntax" c)))))

  (define (print-quoted-datum q type color)
    (with-color q
                color
                (lambda ()
                  (set! *Qtype* (if (eq? q 'quote)
                                    'quote
                                    type))
                  (print-quoted-form (next-char) *Qtype*))))

  (define (print-quoted c q type)
    (with-bold-color
      q
      Color-std-syntax
      (lambda ()
        (output (if (eq? type 'quote) #\' #\`))))
    (print-quoted-datum q type Color-constant))

  (define (print-unquoted q)
    (with-bold-color
      (eq? q 'quote)
      Color-std-syntax
      (lambda ()
        (output #\,)
        (if (char=? (peek-next-char) #\@)
            (output (next-char)))))
    (if (not (eq? q 'quote))
        (let ((c (print-quoted-form (next-char) #f)))
          (with-color #f
                      Color-constant
                      (lambda () c)))
        (next-char)))

  (define (print-object c q)
    (cond ((or (char=? c LP) (char=? c LB))
                          (set! *Parens* (+ 1 *Parens*))
                          (print-paren c q))
          ((or (char=? c RP) (char=? c RB))
                          (set! *Parens* (- *Parens* 1))
                          (print-paren c q))
          ((char=? c #\#) (print-hash-syntax c q))
          ((char=? c #\") (print-string c q))
          ((char=? c #\;) (print-comment c))
          ((char=? c #\') (print-quoted c q 'quote))
          ((char=? c #\`) (print-quoted c q 'quasiquote))
          ((char=? c #\,) (print-unquoted q))
          ((and tilde-quotes
                (char=? c #\~))
                          (print-quoted-datum q 'quote Color-normal-form))
          ((symbolic? c)  (print-symbol-or-number c q))
          (else           (error "scm2html: unknown character class" c))))

  (define (skip-spaces c)
    (let loop ((c c)
               (n 0))
      (cond ((and (char? c)
                  (char=? #\space c))
              (loop (next-char) (+ 1 n)))
            (lout-mode
              (if (positive? n)
                  (begin (output "{&")
                         (output (number->string n))
                         (output "s}")))
              c)
            (else
              (let loop ((n n))
                (cond ((positive? n)
                        (output #\space)
                        (loop (- n 1)))))
              c))))

  (define (skip-whitespace c)
    (let loop ((c (skip-spaces c)))
      (if (and (char? c)
               (char-whitespace? c))
          (begin (if (or (not *input-string*)
                         (not (char=? c #\newline)))
                     (output c))
                 (loop (skip-spaces (next-char))))
          c)))

  (define (print-vector q)
    (with-color q
                Color-constant
                (lambda () (escaped-output "#")))
    (print-object LP q))

  (define (print-quoted-list c type)
    (let ((c (skip-whitespace c)))
      (cond ((end-of-input? c)
              c)
            (else
              (let ((c (print-object c type)))
                (if (<= *Parens* (car *Paren-stack*))
                    (begin (pop! *Paren-stack*)
                           c)
                    (print-quoted-list c type)))))))

  (define (print-quoted-form c type)
    (let ((c (skip-whitespace c))
          (p0 *Parens*))
      (let ((c (print-object c type)))
        (if (= p0 *Parens*)
            c
            (begin (push! p0 *Paren-stack*)
                   (print-quoted-list c type))))))

  (define (print-program c q)
    (let ((c (skip-whitespace c)))
      (if (end-of-input? c)
          c
          (let ((c (print-object c q)))
            (set! *load-from-library*
                  (if (zero? *load-from-library*)
                      0
                      (- *load-from-library* 1)))
            (print-program c q)))))

  (define full-html     #f)
  (define lout-mode     #f)
  (define mark-s9-procs #f)
  (define mark-s9-extns #f)
  (define show-matches  #f)
  (define tilde-quotes  #f)
  (define input-string  #f)

  (accept-keywords "scm2html"
                   options
                   '(full-html: input-string: initial-style: lout-mode:
                     mark-s9-procs: mark-s9-extns: show-matches: tilde-quotes:
                     terminate:))
  (let ((fh  (keyword-value options 'full-html: #f))
        (lm  (keyword-value options 'lout-mode: #f))
        (is  (keyword-value options 'input-string: #f))
        (st  (keyword-value options 'initial-style: '(#f #f #f 0 ())))
        (msp (keyword-value options 'mark-s9-procs: #f))
        (msx (keyword-value options 'mark-s9-extns: #f))
        (sm  (keyword-value options 'show-matches: #f))
        (tq  (keyword-value options 'tilde-quotes: #f))
        (te  (keyword-value options 'terminate: #f)))
    (set! full-html fh)
    (set! lout-mode lm)
    (set! input-string is)
    (set! mark-s9-procs msp)
    (set! mark-s9-extns msx)
    (set! show-matches sm)
    (set! tilde-quotes tq)
    (set! *Color*       (car st))
    (set! *Bold*        (cadr st))
    (set! *Parens*      (cadddr st))
    (set! *Paren-stack* (car (cddddr st)))
    (set! *Qtype*       (if (null? *Paren-stack*) #f (caddr st)))
    (if (and lout-mode full-html)
        (error "Lout mode cannot be combined with HTML mode"))
    (if (and lout-mode show-matches)
        (error "Lout mode cannot be combined with paren matching"))
    (cond (te
            (if lout-mode
                ""
                (string-append (if (cadr te) "</B>" "")
                               (if (car te) "</SPAN>" ""))))
          (input-string
            (set! *input-string* (append (string->list input-string)
                                         (list #\newline)))
            (set! *output-string* '())
            (if lout-mode
                (let ((c *Color*)
                      (b *Bold*))
                  (set! *Color* #f)
                  (set! *Bold* #f)
                  (change-color #f c b)))
            (let ((c (if (not (null? *Paren-stack*))
                         (print-quoted-list (next-char) *Qtype*)
                         (next-char))))
              (print-program c #f))
            (let* ((out (output-string))
                   (out (if lout-mode
                            (string-append
                              out
                              (if *Bold* "}" "")
                              (if *Color* "}" ""))
                            out)))
              (list (list *Color* *Bold* *Qtype* *Parens* *Paren-stack*)
                    out)))
          (else
            (output* (Prolog))
            (print-program (next-char) #f)
            (output* (Epilog))
            (output #\newline)))))
