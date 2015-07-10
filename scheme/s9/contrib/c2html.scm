; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010-2015
; Placed in the Public Domain
;
; (c2html <option> ...)  ==>  string | unspecific
;
; Render C code in HTML with syntax highlighting. Input is read from
; (current-input-stream) and output is written to (current-output-stream)
; unless the 'INPUT-STRING: option is specified (see below).
;
; The rendition of the C code will be placed in a PRE container of
; the class "ccode" (<PRE class=ccode>). The following container
; classes are used to specify the colors and other styles of the
; individual elements:
;
;       co  comment
;       cp  punctuation
;       cs  symbol
;       cc  constant
;       cr  reserved word/operator
;       cl  standard library symbol
;       cx  extension
;
; See the "ccode.css" style sheet for examples.
;
; The following <option>s exist;
;
; 'FULL-HTML: boolean
;       When set to #T, C2HTML will output a full HTML document
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
;       C2HTML previously. It allows to render multiple lines that are
;       logically connected by preserving the style across line boundaries.
;
; 'TERMINATE: list
;       Return termination tags for the color and boldface settings
;       specified in LIST (see INPUT-STRING:).
;
; (Example):   (c2html 'input-string: "x++")
;                ==>  (("cr" #f)
;                      "</SPAN><SPAN class=cs>x</SPAN><SPAN class=cr>++")

(load-from-library "keyword-value.scm")
(load-from-library "symbols.scm")
(load-from-library "read-line.scm")
(load-from-library "setters.scm")
(load-from-library "hof.scm")
(load-from-library "htmlify-char.scm")
(load-from-library "loutify-char.scm")
(load-from-library "string-expand.scm")

(define (c2html . options)

  (define *local-include* 0)
  (define *input-string* #f)
  (define *output-string* #f)

  (define END-OF-INPUT (list 'EOI))

  (define LP #\()
  (define RP #\))

  (define (Prolog)
    (let ((p  (if lout-mode
                  '("@Pre{")
                  '("<PRE class=ccode>"))))
      (if full-html
          (append
           '("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\""
              "  \"http://www.w3.org/TR/html4/loose.dtd\">"
              "<HTML>"
              "<HEAD>"
              "<TITLE></TITLE>"
              "<LINK rel=\"stylesheet\" type=\"text/css\" href=\"ccode.css\">"
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

  (define Color-comment     "co")
  (define Color-punctuation "cp")
  (define Color-symbol      "cs")
  (define Color-constant    "cc")
  (define Color-reserved    "cr")
  (define Color-std-symbol  "cl")
  (define Color-extension   "cx")

  (define Color #f)
  (define Bold #f)

  (define (escaped-output s)
    (if lout-mode
        (output (apply string-append
                       (map loutify-char
                            (string->list s))))
        (output (htmlify-string s))))

  (define (change-color quoted co bo)
    (cond (quoted)
          ((and (equal? co Color) (eq? bo Bold)))
          (else
            (if Bold
                (if lout-mode
                    (output "}")
                    (output "</B>")))
            (if Color
                (if lout-mode
                    (output "}")
                    (output "</SPAN>")))
            (if co
                (if lout-mode
                    (begin (output "@C_")
                           (output co)
                           (output "{"))
                    (begin (output "<SPAN class=")
                           (output co)
                           (output ">"))))
            (if bo
                (if lout-mode
                    (output "@B{")
                    (output "<B>")))
            (set! Color co)
            (set! Bold bo))))

  (define (with-color quoted co thunk)
    (change-color quoted co #f)
    (thunk))

  (define (with-bold-color quoted co thunk)
    (change-color quoted co #t)
    (thunk))

  (define symbolic?
    (lambda (c)
      (or (char-alphabetic? c)
          (char-numeric? c)
          (and (memv c '(#\_ #\#)) #t))))

  (define (print-punct c)
    (with-color #f
                Color-punctuation
                (lambda () (escaped-output (string c))))
    (next-char))

  (define reserved?
    (let ((reserved
            (map string->symbol
                 '("#define" "#else" "#error" "#endif" "#if" "#ifdef"
                    "#ifndef" "#include" "#undef" "auto" "break" "case"
                    "char" "const" "continue" "do" "default" "else" "enum"
                    "extern" "for" "goto" "if" "int" "long" "register"
                    "return" "short" "signed" "sizeof" "static" "struct"
                    "switch" "typedef" "union" "unsigned" "void" "volatile"
                    "while"))))
      (lambda (s)
        (memq (string->symbol s) reserved))))

  (define stdsym?
    (let ((stdsym
            (map string->symbol
                 '("BUFSIZ" "EOF" "FILE" "NULL" "SEEK_CUR" "SEEK_END"
                   "SEEK_SET" "SIGHUP" "SIGINT" "SIGQUIT" "SIGTERM" "abort"
                   "abs" "atoi" "atol" "clearerr" "close" "exit" "fclose"
                   "fdopen" "feof" "ferror" "fflush" "fgetc" "fgets"
                   "fileno" "fopen" "fprintf" "fputc" "fputs" "fread"
                   "free" "freopen" "fscanf" "fseek" "ftell" "fwrite"
                   "getc" "getchar" "getenv" "getw" "isalpha" "isalnum"
                   "iscntrl" "isdigit" "isxdigit" "islower" "isprint"
                   "isspecial" "isupper" "labs" "malloc" "memcmp" "memcpy"
                   "memmove" "memset" "open" "printf" "putc" "putchar"
                   "putw" "qsort" "read" "realloc" "remove" "rewind"
                   "scanf" "setbuf" "setvbuf" "sprintf" "sscanf" "stderr"
                   "stdin" "stdout" "strcat" "strchr" "strcmp" "strcpy"
                   "strlen" "strncmp" "strncpy" "strrchr" "strstr" "strtok"
                   "tolower" "toupper" "ungetc" "write"))))
      (lambda (s)
        (memq (string->symbol s) stdsym))))

  (define punctuation?
    (let ((punct-chars (string->list "(){};,\\")))
       (lambda (c)
         (and (memv c punct-chars)
              #t))))

  (define (collect p c s)
    (if (p c)
        (collect p (next-char) (cons c s))
        (cons c (list->string (reverse! s)))))

  (define (collect-string c s delim esc)
    (if (and (char=? c delim)
             (not esc))
        (list->string (reverse! (cons delim s)))
                (collect-string (next-char)
                                (cons c s)
                                delim
                                (and (not esc) (char=? #\\ c)))))

  (define (print-string c)
    (let* ((s  (collect-string c '() #\" #t))
           (s2 (substring s 1 (- (string-length s) 1))))
      (if (and (not lout-mode)
               (= *local-include* 1))
          (with-color #f
                      Color-constant
                      (lambda ()
                        (output "\"<A href=\"")
                        (output s2)
                        (output ".html\">")
                        (escaped-output s2)
                        (output "</A>\"")))
          (with-color #f
              Color-constant
              (lambda () (escaped-output s)))))
    (next-char))

  (define (print-char c)
    (let* ((s  (collect-string c '() #\' #t)))
      (with-color #f
          Color-constant
          (lambda () (escaped-output s))))
    (next-char))

  (define (print-number c)
    (let ((c/s (collect (lambda (x)
                          (or (char-numeric? x)
                              (memv x '(#\x #\a #\b #\c #\d
                                        #\e #\f #\L #\U))))
                        (next-char)
                        (list c))))
      (with-color #f
                  Color-constant
                  (lambda () (escaped-output (cdr c/s))))
      (car c/s)))

  (define (extension? s)
    (and (> (string-length s) 2)
         (string=? "__" (substring s 0 2))
         (not (char=? #\_ (string-ref s 2)))))

  (define (print-symbol c)
    (let ((c/s (collect symbolic? (next-char) (list c))))
      (cond ((reserved? (cdr c/s))
              (if (string=? "#include" (cdr c/s))
                  (set! *local-include* 2))
              (with-bold-color #f
                               Color-reserved
                               (lambda () (escaped-output (cdr c/s)))))
            ((stdsym? (cdr c/s))
              (with-color #f
                          Color-std-symbol
                          (lambda () (escaped-output (cdr c/s)))))
            ((extension? (cdr c/s))
              (with-color #f
                          Color-extension
                          (lambda () (escaped-output (cdr c/s)))))
            (else
              (with-color #f
                          Color-symbol
                          (lambda () (escaped-output (cdr c/s))))))
      (car c/s)))

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

  (define (collect-comment)
    (let loop ((c    (next-char))
               (star #f))
      (if (and (not (end-of-input? c))
               (not (and *input-string*
                         (char=? #\newline c)))
               (not (char=? #\space c)))
          (escaped-output (string c)))
      (cond ((end-of-input? c))
            ((char=? #\* c)
              (loop (next-char) #t))
            ((char=? #\/ c)
              (if (not star)
                  (loop (next-char) #f)
                  (change-color #f #f #f)))
            ((char=? c #\space)
              (loop (skip-spaces #\space)
                    #f))
            (else
              (loop (next-char) #f)))))

  (define (comment)
    (with-color #f Color-comment
                   (lambda ()
                     (escaped-output "/*")
                     (collect-comment)))
    (next-char))

; [ ] ~ ? : ,
; ! !=
; % %=
; & && &=
; * *=
; + ++ +=
; + +=
; - -- -> -=
; . ...
; / /=
; < << <= <<=
; = ==
; > >> >= >>=
; ^ ^=
; | || |=

  (define (operator c c2)
    (with-color #f Color-reserved
                   (lambda ()
                     (escaped-output (string c))))
    c2)

  (define (figure-it-out c)
    (let ((c2 (next-char)))
      (cond ((and (char=? #\/ c) (char=? #\* c2))
              (comment))
            (else
              (operator c c2)))))

  (define (print-object c)
    (cond ((char-numeric? c) (print-number c))
          ((punctuation? c)  (print-punct c))
          ((symbolic? c)     (print-symbol c))
          ((char=? c #\")    (print-string c))
          ((char=? c #\')    (print-char c))
          (else              (figure-it-out c))))

  (define (skip-whitespace c)
    (let loop ((c (skip-spaces c)))
      (if (and (char? c)
               (char-whitespace? c))
          (begin (if (or (not *input-string*)
                         (not (char=? c #\newline)))
                     (output c))
                 (loop (skip-spaces (next-char))))
          c)))

  (define (print-program c)
    (let ((c (skip-whitespace c)))
      (if (not (end-of-input? c))
          (let ((c (print-object c)))
            (set! *local-include*
                  (if (zero? *local-include*)
                      0
                      (- *local-include* 1)))
            (print-program c)))))

  (define full-html    #f)
  (define lout-mode    #f)
  (define input-string #f)

  (accept-keywords "c2html"
                   options
                   '(full-html: input-string: initial-style: lout-mode:
                     terminate:))
  (let ((fh (keyword-value options 'full-html: #f))
        (lm (keyword-value options 'lout-mode: #f))
        (is (keyword-value options 'input-string: #f))
        (st (keyword-value options 'initial-style: '(#f #f)))
        (te (keyword-value options 'terminate: #f)))
    (set! full-html fh)
    (set! lout-mode lm)
    (set! input-string is)
    (set! Color (car st))
    (set! Bold  (cadr st))
    (if (and lout-mode full-html)
        (error "Lout mode cannot be combined with HTML mode"))
    (cond (te
            (if lout-mode
                ""
                (string-append (if (cadr te) "</B>" "")
                               (if (car te) "</SPAN>" ""))))
          (input-string
            (set! *input-string* (append (string->list
                                           (string-expand input-string))
                                         (list #\newline)))
            (set! *output-string* '())
            (if lout-mode
                (let ((c Color)
                      (b Bold))
                  (set! Color #f)
                  (set! Bold #f)
                  (change-color #f c b)))
            (if (equal? Color Color-comment)
                (collect-comment))
            (print-program (next-char))
            (let* ((out (output-string))
                   (out (if lout-mode
                            (string-append
                              out
                              (if Bold "}" "")
                              (if Color "}" ""))
                            out)))
              (list (list Color Bold) out)))
          (else
            (output* (Prolog))
            (print-program (next-char))
            (output* (Epilog))
            (output #\newline)))))
