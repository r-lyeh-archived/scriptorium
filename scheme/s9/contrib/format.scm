; Common LISP-style text output formatter for R4RS Scheme
; By Dirk Lutzebaeck, 1992, 1993
; With some additional hacking by Nils M Holm in 2009.
; Authors of the original version (< 1.4) were Ken Dickey and Aubrey Jaffer.
; Placed in the Public Domain by the authors.
;
; (format #t string-f object ...)           ==>  string | #t | #f
; (format #f string-f object ...)           ==>  string | #t | #f
; (format string string-f object ...)       ==>  string | #t | #f
; (format output-port string-f object ...)  ==>  string | #t | #f
;
; FORMAT Returns #T, #F or a string. It has the side effect of printing
; according to the formatting instruction in STRING. If the first
; argument is #T, the output is to the current output port and #T is
; returned. If the argument is #F, a formatted string is returned as
; the result of the call. If the first argument is is a string, the
; output is appended to that string by string-append (note: this
; returns a fresh string). Otherwise the first argument must be an
; output port and #T is returned.
;
; Characters are output as if they were output by the DISPLAY function
; with the exception of those prefixed by a tilde (~). In case of a
; formatting error FORMAT prints a message on the current output port
; and aborts.  For a detailed description of the STRING-F syntax
; please consult a Common LISP FORMAT reference manual. For a quick
; overview of implemented, not supported and extended control properties
; of STRING-F see "format.txt". For a test suite to verify this FORMAT
; implementation load "format-test.scm".
;
; Example:   (format #f "~A ~:* ~S" '(#\c "s"))
;              ==>  "(c s)  (#\\c \"s\")"
;
;            (format #f "~20,'_,',,3:D" 123456789)
;              ==>  "_________123,456,789"
;
;            (format #f "~@{ ~A,~A ~}" 'a 1 'b 2 'c 3)
;              ==>  " a,1  b,2  c,3 "

(define (format . args)

  (define (abort)
    (error "FORMAT: aborting"))   ; S9fES-specific

  (define destination #f)
  (define output-buffer "")
  (define case-conversion #f)

  (define form-feed (integer->char 12))

  (define tab (integer->char 9))

  ; char->string converts a character into a slashified string as
  ; done by WRITE. The following procedure is dependent on the integer
  ; representation of characters and assumes a character number according
  ; to the ASCII character set.

  (define (char->string ch)
    (let ((int-rep (char->integer ch))
          (ascii-non-printable-charnames
            '#("nul" "soh" "stx" "etx" "eot" "enq" "ack" "bel"
               "bs"  "ht"  "lf"  "vt"  "np"  "cr"  "so"  "si"
               "dle" "dc1" "dc2" "dc3" "dc4" "nak" "syn" "etb"
               "can" "em"  "sub" "esc" "fs"  "gs"  "rs"  "us"
               "space")))
      (string-append "#\\"
        (cond ((char=? ch #\newline) "newline")
                ((<= 0 int-rep 32)
                  (vector-ref ascii-non-printable-charnames int-rep))
                ((= int-rep 127)
                  "del")
                ((>= int-rep 128)
                  (number->string int-rep 8))
                (else
                  (string ch))))))

  (define (obj->str-padded pad-left obj options params)
    (let ((mincol 0)
          (colinc 1)         ; sorry, I don't understand this CL parm
          (minpad 0)
          (padchar #\space)
          (objstr (apply obj->string (append (list obj) options))))
      (if (null? params)
          objstr
          (begin
            (set! params (append params '(#f #f #f #f)))
            (if (list-ref params 0) (set! mincol (list-ref params 0)))
            (if (list-ref params 1) (set! colinc (list-ref params 1)))
            (if (list-ref params 2) (set! minpad (list-ref params 2)))
            (if (list-ref params 3)
                (set! padchar (integer->char (list-ref params 3))))
            (pad-str objstr (negative? mincol) pad-left
                            (abs mincol) minpad padchar)))))

  (define (num->str-padded modifier number params radix-num radix-prefix)
    (if (not (number? number)) (format-error "argument not a number"))
    (let ((mincol 0)
          (padchar #\space)
          (commachar #\,)
          (commawidth 3)                  ; an extension to CL
          (numstr-len 0)
          (numstr (number->string number radix-num)))

      (if (and (null? params) (not modifier))
          numstr
          (begin
            (set! params (append params '(#f #f #f #f)))
            (if (list-ref params 0) (set! mincol (list-ref params 0)))
            (if (list-ref params 1)
                (set! padchar (integer->char (list-ref params 1))))
            (if (list-ref params 2)
                (set! commachar (integer->char (list-ref params 2))))
            (if (list-ref params 3) (set! commawidth (list-ref params 3)))
            (set! numstr-len (string-length numstr))

            (if (and (memq modifier '(colon colon-at)) ; insert comma char
                     (integer? number))   ; only integers are ,-separated
                (set! numstr
                      (do ((s "")
                           (i (- numstr-len commawidth) (- i commawidth)))
                          ((or (zero? i) (negative? i))
                            (string-append
                              (substring numstr 0 (+ i commawidth )) s))
                        (set! s (string-append
                                  (string commachar)
                                  (substring numstr i (+ i commawidth)) s)))))

            (if (memq modifier '(at colon-at))    ; append numerical prefix
                (set! numstr (string-append radix-prefix numstr)))

            (pad-str numstr (negative? mincol) #t
                            (abs mincol) 0 padchar)))))

  (define (pad-str objstr fixed-field pad-left mincol minpad padchar)
    (let ((objstr-len (string-length objstr)))
      (if fixed-field
          (if (> objstr-len mincol)
              (if pad-left
                  (string-append "<"
                    (substring objstr (- objstr-len (- mincol 1)) objstr-len))
                  (string-append (substring objstr 0 (- mincol 1)) ">"))
              (if pad-left
                  (string-append (make-string (- mincol objstr-len) padchar)
                                 objstr)
                  (string-append objstr
                                 (make-string (- mincol objstr-len) padchar))))
          (if (> objstr-len mincol)
              (if pad-left
                  (string-append (make-string minpad padchar) objstr)
                  (string-append objstr (make-string minpad padchar)))
              (if pad-left
                  (string-append (make-string (- mincol objstr-len) padchar)
                                 objstr)
                  (string-append objstr
                                 (make-string (- mincol objstr-len)
                                              padchar)))))))

  ; obj->string converts an arbitrary scheme object to a string.
  ; `options' is a list of options which may contain the following symbols:
  ;   slashify:      slashifies output string as (write) does
  ;   readproof:     prints out #<...> objects as quoted strings "#<...>" so
  ;                  that the output can always be processed by (read)

  (define (obj->string obj . options)
    (let to-str ((obj obj)
                 (slashify (if (memq 'slashify options) #t #f))
                 (readproof (if (memq 'readproof options) #t #f)))
      (cond
        ((string? obj)
         (if slashify
             (let ((obj-len (string-length obj)))
               (string-append
                 "\""
                 (let loop ((i 0) (j 0))    ; taken from Marc Feeley's pp.scm
                   (if (= j obj-len)
                       (string-append (substring obj i j) "\"")
                       (let ((c (string-ref obj j)))
                         (if (or (char=? c #\\)
                                 (char=? c #\"))
                             (string-append (substring obj i j) "\\"
                                            (loop j (+ j 1)))
                             (loop i (+ j 1))))))))
             obj))
       ((boolean? obj)
         (if obj "#t" "#f"))
       ((number? obj)
         (number->string obj))
       ((symbol? obj)
         (symbol->string obj))
       ((char? obj)
        (if slashify
            (char->string obj)
            (string obj)))
       ((null? obj)
         "()")
       ((procedure? obj)
         (if readproof "\"#<procedure>\"" "#<procedure>"))
       ((output-port? obj)
         (if readproof "\"#<output-port>\"" "#<output-port>"))
       ((input-port? obj)
         (if readproof "\"#<input-port>\"" "#<input-port>"))
       ((pair? obj)            ; from my WRITE-TO-STRING --nmh
         (string-append
           "("
           (let loop ((a obj)
                      (first #t))
             (cond ((pair? a)
                     (string-append (if first "" " ")
                                    (to-str (car a)
                                            slashify
                                            readproof)
                                    (loop (cdr a) #f)))
                   ((null? a)
                     "")
                   (else
                     (string-append
                       " . "
                       (to-str a slashify readproof)))))
           ")"))
       ((eof-object? obj)
         (if readproof "\"#<eof-object>\"" "#<eof-object>"))
       ((vector? obj)
         (string-append "#" (to-str (vector->list obj) 'slashify readproof)))
       (else (if readproof "\"#<unspecified>\"" "#<unspecified>")))))

  (define (string-upcase str)
    (let ((up-str (string-copy str)))
      (do ((i (- (string-length str) 1) (- i 1)))
          ((< i 0) up-str)
        (string-set! up-str i (char-upcase (string-ref str i))))))

  (define (string-downcase str)
    (let ((down-str (string-copy str)))
      (do ((i (- (string-length str) 1) (- i 1)))
          ((< i 0) down-str)
        (string-set! down-str i (char-downcase (string-ref str i))))))

  (define (string-capitalize str)         ; "hello" -> "Hello"
    (let ((cap-str (string-copy str))     ; "hELLO" -> "Hello"
          (non-first-alpha #f)            ; "*hello" -> "*Hello"
          (str-len (string-length str)))  ; "hello you" -> "Hello You"
      (do ((i 0 (+ i 1)))
          ((= i str-len) cap-str)
        (let ((c (string-ref str i)))
          (if (char-alphabetic? c)
              (if non-first-alpha
                  (string-set! cap-str i (char-downcase c))
                  (begin
                    (set! non-first-alpha #t)
                    (string-set! cap-str i (char-upcase c))))
              (set! non-first-alpha #f))))))

  (define (string-capitalize-first str)   ; "hello" -> "Hello"
    (let ((cap-str (string-copy str))     ; "hELLO" -> "Hello"
          (non-first-alpha #f)            ; "*hello" -> "*Hello"
          (str-len (string-length str)))  ; "hello you" -> "Hello you"
      (do ((i 0 (+ i 1)))
          ((= i str-len) cap-str)
        (let ((c (string-ref str i)))
          (if (char-alphabetic? c)
              (if non-first-alpha
                  (string-set! cap-str i (char-downcase c))
                  (begin
                    (set! non-first-alpha #t)
                    (string-set! cap-str i (char-upcase c)))))))))

  (define (out-str str)           ; append to output-buffer
    (set! output-buffer
          (string-append output-buffer
                         (if (procedure? case-conversion)
                             (case-conversion str)
                             str))))

  (define (out-char ch)
    (out-str (string ch)))

  (define (format-error . args*)
    (let ((format-args args))
      (format #t "FORMAT: ")
      (apply format `(#t ,@args*))
      (format #t ", ARGS: ~a~%" format-args)
      (abort)))

  (define parameter-characters
    '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+ #\v #\# #\'))

  (define (format-work format-string arg-list) ; does the formatting work
    (letrec
      ((format-string-len (string-length format-string))
       (pos 0)                     ; input format-string position
       (arg-pos 0)                 ; argument position in arg-list
       (arg-len (length arg-list))  ; number of arguments
       (modifier #f)               ; 'colon | 'at | 'colon-at | #f
       (params '())                ; directive parameter list
       (param-value-found #f)      ; a directive parameter value found
       (conditional-nest 0)        ; conditional nesting level
       (clause-pos 0)              ; last cond. clause beginning char pos
       (clause-default #f)         ; conditional default clause string
       (clauses '())               ; conditional clause string list
       (conditional-type #f)       ; reflects the contional modifiers
       (conditional-arg #f)        ; argument to apply the conditional
       (iteration-nest 0)          ; iteration nesting level
       (iteration-pos 0)           ; iteration string beginning char pos
       (iteration-type #f)         ; reflects the iteration modifiers
       (max-iterations #f)         ; maximum number of iterations
       (cond-indicators (append '(#\[ #\] #\; #\: #\@ #\^)
                                parameter-characters))
       (iter-indicators (append '(#\{ #\} #\: #\@ #\^)
                                parameter-characters))

       (next-char
         (lambda ()
           (let ((ch (peek-next-char)))
             (set! pos (+ 1 pos))
             ch)))

       (peek-next-char
         (lambda ()
           (if (>= pos format-string-len)
               (format-error "illegal format string")
               (string-ref format-string pos))))

       (one-positive-integer?
         (lambda (params)
           (cond ((null? params)
                   #f)
                 ((and (integer? (car params))
                       (>= (car params) 0)
                       (null? (cdr params)))
                   #t)
                 (else (fail "one positive integer parameter expected")))))

       (add-arg-pos
         (lambda (n)
           (set! arg-pos (+ n arg-pos))))

       (next-arg
         (lambda ()
           (if (>= arg-pos arg-len)
               (format-error "missing argument(s)"))
           (add-arg-pos 1)
           (list-ref arg-list (- arg-pos 1))))

       (prev-arg
         (lambda ()
           (add-arg-pos -1)
           (if (negative? arg-pos)
               (format-error "missing backward argument(s)"))
           (list-ref arg-list arg-pos)))

       (rest-args
         (lambda ()
           (list-tail arg-list arg-pos)))

       (fail
         (lambda (fmt . args)
           (apply format-error
                  `(,(string-append fmt ", POS: \"~a\"")
                    ,@args
                    ,(string-append
                      (substring format-string 0 pos)
                      "<--"
                      (substring format-string pos format-string-len))))))

       (char-dispatch
         (lambda ()
           (if (>= pos format-string-len)
               arg-pos                       ; used for ~? continuance
               (let ((char (next-char)))
                 (cond ((char=? char #\~)
                         (set! modifier #f)
                         (set! params '())
                         (set! param-value-found #f)
                         (tilde-dispatch))
                       (else
                         (if (and (zero? conditional-nest)
                                  (zero? iteration-nest))
                             (out-char char))
                         (char-dispatch)))))))

       (tilde-dispatch
         (lambda ()
           (cond
             ((>= pos format-string-len)
               (out-str "~")        ; tilde at end of string is just output
               arg-pos)             ; used for ~? continuance
             ((and (or (zero? conditional-nest)
                       (memv (peek-next-char) cond-indicators))
                   (or (zero? iteration-nest)
                       (memv (peek-next-char) iter-indicators)))
               (case (char-upcase (next-char))

                 ; format directives

                 ((#\A)                    ; Any -- for humans
                   (out-str
                     (obj->str-padded (memq modifier '(at colon-at))
                                      (next-arg)
                                      (if (eq? modifier 'colon)
                                          '(readproof)
                                          '())
                                      params))
                    (char-dispatch))
                 ((#\S)                    ; Slashified -- for parsers
                   (out-str
                     (obj->str-padded (memq modifier '(at colon-at))
                                      (next-arg)
                                      (if (eq? modifier 'colon)
                                          '(readproof slashify)
                                          '(slashify))
                                      params))
                   (char-dispatch))
                 ((#\D)                    ; Decimal
                   (out-str
                     (num->str-padded modifier (next-arg) params 10 "#d"))
                   (char-dispatch))
                 ((#\X)                    ; Hexadecimal
                   (out-str
                    (num->str-padded modifier (next-arg) params 16 "#x"))
                   (char-dispatch))
                 ((#\O)                    ; Octal
                   (out-str
                    (num->str-padded modifier (next-arg) params 8 "#o"))
                   (char-dispatch))
                 ((#\B)                    ; Binary
                   (out-str
                    (num->str-padded modifier (next-arg) params 2 "#b"))
                   (char-dispatch))
                 ((#\R)                    ; any Radix
                   (if (null? params)
                       (fail "~~r requires an explicit radix")
                       (out-str (num->str-padded modifier
                                                 (next-arg)
                                                 (cdr params)
                                                 (car params)
                                                 "#r")))
                   (char-dispatch))
                 ((#\E #\F #\G #\$)        ; Floating point
                   (fail "~~e, ~~f, ~~g, ~~$ are not implemented"))
                 ((#\C)                    ; Character
                   (let ((ch (if (one-positive-integer? params)
                                 (integer->char (car params))
                                 (next-arg))))
                     (if (not (char? ch)) (fail "~~c expects a character"))
                     (if (eq? modifier 'at)
                         (out-str (obj->string ch 'slashify))
                         (out-char ch)))
                   (char-dispatch))
                 ((#\P)                    ; Plural
                   (if (memq modifier '(colon colon-at))
                       (prev-arg))
                   (let ((arg (next-arg)))
                     (if (not (number? arg))
                         (fail "~~p expects a number argument"))
                     (if (= arg 1)
                         (if (memq modifier '(at colon-at))
                             (out-str "y"))
                         (if (memq modifier '(at colon-at))
                             (out-str "ies")
                             (out-str "s"))))
                   (char-dispatch))
                 ((#\~)            ; Tilde
                   (if (one-positive-integer? params)
                       (out-str (make-string (car params) #\~))
                       (out-str "~"))
                   (char-dispatch))
                 ((#\% #\&)                ; Newline (Freshline is the same)
                   (if (one-positive-integer? params)
                       (out-str (make-string (car params) #\newline))
                       (out-char #\newline))
                   (char-dispatch))
                 ((#\_)                    ; Space
                   (if (one-positive-integer? params)
                       (out-str (make-string (car params) #\space))
                       (out-str " "))
                   (char-dispatch))
                 ((#\T)                    ; Tab
                   (if (one-positive-integer? params)
                       (out-str (make-string (car params) tab))
                       (out-char tab))
                   (char-dispatch))
                 ((#\|)                    ; Page Seperator
                   (if (one-positive-integer? params)
                       (out-str (make-string (car params) form-feed))
                       (out-char form-feed))
                   (char-dispatch))
                 ((#\? #\K)                ; Indirection (is "~K" in T)
                   (cond
                     ((memq modifier '(colon colon-at))
                       (fail "illegal modifier in ~~?"))
                     ((eq? modifier 'at)
                       (let* ((frmt (next-arg))
                              (args (rest-args)))
                         (add-arg-pos (format-work frmt args))))
                     (else
                       (let* ((frmt (next-arg))
                              (args (next-arg)))
                         (format-work frmt args))))
                   (char-dispatch))
                 ((#\newline)              ; Continuation lines
                   (if (eq? modifier 'at)
                       (out-char #\newline))
                   (if (< pos format-string-len)
                       (do ((ch (peek-next-char) (peek-next-char)))
                           ((or (not (char-whitespace? ch))
                                (= pos (- format-string-len 1))))
                         (if (eq? modifier 'colon)
                             (out-char (next-char))
                             (next-char))))
                   (char-dispatch))
                 ((#\*)                    ; Argument jumping
                   (case modifier
                     ((colon)              ; jump backwards
                       (if (one-positive-integer? params)
                           (do ((i 0 (+ i 1)))
                               ((= i (car params)))
                             (prev-arg))
                           (prev-arg)))
                     ((at)                 ; jump absolute
                       (set! arg-pos (if (one-positive-integer? params)
                                         (car params) 0)))
                     ((colon-at)
                       (fail "illegal modifier `:@' in ~~* directive"))
                     (else                 ; jump forward
                       (if (one-positive-integer? params)
                           (do ((i 0 (+ i 1)))
                               ((= i (car params)))
                             (next-arg))
                           (next-arg))))
                   (char-dispatch))
                 ((#\()                    ; Case conversion begin
                   (set! case-conversion
                         (case modifier
                           ((at) string-capitalize-first)
                           ((colon) string-capitalize)
                           ((colon-at) string-upcase)
                           (else string-downcase)))
                   (char-dispatch))
                 ((#\))                    ; Case conversion end
                   (if (not case-conversion)
                       (fail "missing ~~(")) ; )balance
                   (set! case-conversion #f)
                   (char-dispatch))
                 ((#\[)                    ; Conditional begin
                   (set! conditional-nest (+ conditional-nest 1))
                   (cond
                     ((= conditional-nest 1)
                       (set! clause-pos pos)
                       (set! clause-default #f)
                       (set! clauses '())
                       (set! conditional-type
                             (case modifier
                               ((at) 'if-then)
                               ((colon) 'if-else-then)
                               ((colon-at) (fail "illegal modifier in ~~["))
                               (else 'num-case)))
                       (set! conditional-arg
                             (if (one-positive-integer? params)
                                 (car params)
                                 (next-arg)))))
                   (char-dispatch))
                 ((#\;)                    ; Conditional separator
                   (if (zero? conditional-nest)
                       (fail "~~; not in ~~[~~] conditional"))
                   (if (not (null? params))
                       (fail "no parameter allowed in ~~;"))
                   (if (= conditional-nest 1)
                       (let ((clause-str
                              (cond
                                ((eq? modifier 'colon)
                                  (set! clause-default #t)
                                  (substring format-string
                                             clause-pos
                                             (- pos 3)))
                                ((memq modifier '(at colon-at))
                                  (fail "illegal modifier in ~~;"))
                                (else
                                  (substring format-string
                                             clause-pos
                                             (- pos 2))))))
                         (set! clauses (append clauses (list clause-str)))
                         (set! clause-pos pos)))
                   (char-dispatch))
                 ((#\])                    ; Conditional end
                   (if (zero? conditional-nest) (fail "missing ~~["))
                   (set! conditional-nest (- conditional-nest 1))
                   (if modifier
                       (fail "no modifier allowed in ~~]"))
                   (if (not (null? params))
                       (fail "no parameter allowed in ~~]"))
                   (cond
                     ((zero? conditional-nest)
                       (let ((clause-str (substring format-string
                                                    clause-pos
                                                    (- pos 2))))
                         (if clause-default
                             (set! clause-default clause-str)
                             (set! clauses
                                   (append clauses (list clause-str)))))
                       (case conditional-type
                         ((if-then)
                           (if conditional-arg
                               (format-work (car clauses)
                                            (list conditional-arg))))
                         ((if-else-then)
                           (add-arg-pos
                             (format-work (if conditional-arg
                                              (cadr clauses)
                                              (car clauses))
                                          (rest-args))))
                         ((num-case)
                           (if (or (not (integer? conditional-arg))
                                   (< conditional-arg 0))
                               (fail "argument not a positive integer"))
                           (if (not (and (>= conditional-arg (length clauses))
                                         (not clause-default)))
                               (add-arg-pos
                                 (format-work
                                   (if (>= conditional-arg (length clauses))
                                       clause-default
                                       (list-ref clauses conditional-arg))
                                   (rest-args))))))))
                   (char-dispatch))
                 ((#\{)                    ; Iteration begin
                   (set! iteration-nest (+ iteration-nest 1))
                   (cond
                     ((= iteration-nest 1)
                       (set! iteration-pos pos)
                       (set! iteration-type
                             (case modifier
                               ((at) 'rest-args)
                               ((colon) 'sublists)
                               ((colon-at) 'rest-sublists)
                               (else 'list)))
                       (set! max-iterations (if (one-positive-integer? params)
                                                (car params)
                                                #f))))
                   (char-dispatch))
                 ((#\})                    ; Iteration end
                   (if (zero? iteration-nest) (fail "missing ~~{"))
                   (set! iteration-nest (- iteration-nest 1))
                   (case modifier
                     ((colon)
                       (if (not max-iterations) (set! max-iterations 1)))
                     ((colon-at at)
                       (fail "illegal modifier"))
                     (else
                       (if (not max-iterations)
                           (set! max-iterations 100))))
                   (if (not (null? params))
                       (fail "no parameters allowed in ~~}"))
                   (if (zero? iteration-nest)
                     (let ((iteration-str
                            (substring format-string
                                       iteration-pos
                                       (- pos (if modifier 3 2)))))
                       (if (string=? iteration-str "")
                           (set! iteration-str (next-arg)))
                       (case iteration-type
                         ((list)
                           (let ((args (next-arg))
                                 (args-len 0))
                             (if (not (list? args))
                                 (fail "expected a list argument"))
                             (set! args-len (length args))
                             (do ((arg-pos 0 (+ arg-pos
                                                (format-work
                                                  iteration-str
                                                  (list-tail args arg-pos))))
                                  (i 0 (+ i 1)))
                                 ((or (>= arg-pos args-len)
                                      (>= i max-iterations))))))
                         ((sublists)
                           (let ((args (next-arg))
                                 (args-len 0))
                             (if (not (list? args))
                                 (fail "expected a list argument"))
                             (set! args-len (length args))
                             (do ((arg-pos 0 (+ arg-pos 1)))
                                 ((or (>= arg-pos args-len)
                                      (>= arg-pos max-iterations)))
                               (let ((sublist (list-ref args arg-pos)))
                                 (if (not (list? sublist))
                                     (fail
                                       "expected a list of lists argument"))
                                 (format-work iteration-str sublist)))))
                         ((rest-args)
                           (let* ((args (rest-args))
                                  (args-len (length args))
                                  (usedup-args
                                    (do ((arg-pos 0 (+ arg-pos
                                                       (format-work
                                                         iteration-str
                                                         (list-tail args
                                                                    arg-pos))))
                                         (i 0 (+ i 1)))
                                        ((or (>= arg-pos args-len)
                                             (>= i max-iterations))
                                         arg-pos))))
                             (add-arg-pos usedup-args)))
                         ((rest-sublists)
                           (let* ((args (rest-args))
                                  (args-len (length args))
                                  (usedup-args
                                    (do ((arg-pos 0 (+ arg-pos 1)))
                                        ((or (>= arg-pos args-len)
                                             (>= arg-pos max-iterations))
                                         arg-pos)
                                      (let ((sublist (list-ref args arg-pos)))
                                        (if (not (list? sublist))
                                            (fail "expected list arguments"))
                                        (format-work iteration-str sublist)))))
                             (add-arg-pos usedup-args)))
                         (else (fail "internal error in ~~}")))))
                   (char-dispatch))
                 ((#\^)                    ; Up and out
                   (let*
                     ((continue
                       (cond
                         ((not (null? params))
                           (not
                             (case (length params)
                               ((1) (zero? (car params)))
                               ((2) (= (list-ref params 0)
                                       (list-ref params 1)))
                               ((3) (<= (list-ref params 0)
                                        (list-ref params 1)
                                        (list-ref params 2)))
                               (else (fail "too many parameters")))))
                         (case-conversion ; if in conversion, stop conversion
                           (set! case-conversion string-copy) #t)
                         ((= iteration-nest 1) #t)
                         ((= conditional-nest 1) #t)
                         ((>= arg-pos arg-len)
                           (set! pos format-string-len) #f)
                         (else #t))))
                     (if continue
                         (char-dispatch))))

                 ; format directive modifiers and parameters

                 ((#\@)                    ; `@' modifier
                   (if (eq? modifier 'colon-at)
                       (fail "double `@' modifier"))
                   (set! modifier (if (eq? modifier 'colon) 'colon-at 'at))
                   (tilde-dispatch))
                 ((#\:)                    ; `:' modifier
                   (if modifier (fail "illegal `:' modifier position"))
                   (set! modifier 'colon)
                   (tilde-dispatch))
                 ((#\')                    ; Character parameter
                   (if modifier (fail "misplaced modifier"))
                   (set! params (append params
                                        (list (char->integer (next-char)))))
                   (set! param-value-found #t)
                   (tilde-dispatch))
                 ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+)
                   (if modifier (fail "misplaced modifier"))
                   (let ((num-str-beg (- pos 1))
                         (num-str-end pos))
                     (do ((ch (peek-next-char) (peek-next-char)))
                         ((not (char-numeric? ch)))
                       (next-char)
                       (set! num-str-end (+ 1 num-str-end)))
                     (set! params
                           (append params
                                   (list (string->number
                                          (substring format-string
                                                     num-str-beg
                                                     num-str-end))))))
                   (set! param-value-found #t)
                   (tilde-dispatch))
                 ((#\V)                    ; Variable parameter from next argum.
                   (if modifier (fail "misplaced modifier"))
                   (set! params (append params (list (next-arg))))
                   (set! param-value-found #t)
                   (tilde-dispatch))
                 ((#\#)                    ; Number of remaining args
                   (if modifier (fail "misplaced modifier"))
                   (set! params (append params (list (length (rest-args)))))
                   (set! param-value-found #t)
                   (tilde-dispatch))
                 ((#\,)                    ; Parameter separators
                   (if modifier (fail "misplaced modifier"))
                   (if (not param-value-found)
                       (set! params (append params '(#f)))) ; append empty pmtr
                   (set! param-value-found #f)
                   (tilde-dispatch))
                 (else                     ; Unknown tilde directive
                   (fail "unknown control character `~c'"
                         (string-ref format-string (- pos 1))))))
             (else (char-dispatch)))))) ; in case of conditional

    (char-dispatch)                  ; start formatting
    arg-pos))                        ; return position in arg. list

  (if (< (length args) 2)
      (format-error "not enough arguments"))
  (let ((destination (car args))
        (format-string (cadr args))
        (arg-list (cddr args)))
    (set! destination
          (cond
            ((boolean? destination)
              (if destination (current-output-port) #f))
            ((output-port? destination) destination)
            ((string? destination) destination)
            (else (format-error "illegal destination `~a'" output-port))))
    (if (not (string? format-string))
        (format-error "illegal format string `~a'" format-string))
    (set! output-buffer "")
    (set! case-conversion #f) ; modifier case conversion procedure
    (let ((arg-pos (format-work format-string arg-list))
          (arg-len (length arg-list)))
      (cond
        ((< arg-pos arg-len)
          (format-error "~a superfluous argument~:p" (- arg-len arg-pos)))
        ((> arg-pos arg-len)
          (format-error "~a missing argument~:p" (- arg-pos arg-len)))
        ((output-port? destination)
          (display output-buffer destination)
          #t)
        ((string? destination)
          (string-append destination output-buffer))
        (else output-buffer)))))
