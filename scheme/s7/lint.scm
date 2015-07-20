;;; lint for s7 scheme
;;;
;;; (lint "file.scm") checks file.scm for infelicities
;;; to control the kinds of checks, set the variables below.

(provide 'lint.scm)
(require stuff.scm)

(if (provided? 'make-complex)
    (begin
      (define magnitude abs)
      (define make-rectangular make-complex)
      (define (make-polar mag ang) (make-complex (* mag (cos ang)) (* mag (sin ang))))))

(if (provided? 'pure-s7)
    (begin
      (define (memq a b) (member a b eq?))
      (define (memv a b) (member a b eqv?))
      (define (assq a b) (assoc a b eq?))
      (define (assv a b) (assoc a b eqv?))

      (define (char-ci=? . chars) (apply char=? (map char-upcase chars)))
      (define (char-ci<=? . chars) (apply char<=? (map char-upcase chars)))
      (define (char-ci>=? . chars) (apply char>=? (map char-upcase chars)))
      (define (char-ci<? . chars) (apply char<? (map char-upcase chars)))
      (define (char-ci>? . chars) (apply char>? (map char-upcase chars)))

      (define (string-ci=? . strs) (apply string=? (map string-upcase strs)))
      (define (string-ci<=? . strs) (apply string<=? (map string-upcase strs)))
      (define (string-ci>=? . strs) (apply string>=? (map string-upcase strs)))
      (define (string-ci<? . strs) (apply string<? (map string-upcase strs)))
      (define (string-ci>? . strs) (apply string>? (map string-upcase strs)))

      (define (let->list e)
	(if (let? e)
	    (reverse! (map values e))
	    (error 'wrong-type-arg "let->list argument should be an environment: ~A" str)))
      ))

(define *report-unused-parameters* #f)
(define *report-unused-top-level-functions* #f)
(define *report-multiply-defined-top-level-functions* #f) ; same name defined at top level in more than one file
(define *report-shadowed-variables* #f)
(define *report-minor-stuff* #t)                          ; let*, (= 1.5 x), numerical and boolean simplification
(define *report-doc-strings* #f)                          ; report old-style (CL) doc strings

(define *load-file-first* #f)                             ; this will actually load the file, so errors will stop lint
(define start-up-let (rootlet))
(define *current-file* "")
(define *top-level-objects* (make-hash-table))
(define *lint-output-port* *stderr*)

(format *stderr* "loading lint.scm~%")
(set! reader-cond #f)
(define-macro (reader-cond . clauses) `(values))          ; clobber reader-cond to avoid dumb unbound-variable errors


;;; --------------------------------------------------------------------------------
;;; for snd-test.scm

(if (provided? 'snd)
    (set! *#readers* 
	  (cons (cons #\_ (lambda (str)
			    (if (string=? str "__line__")
				(port-line-number)
				#f)))
		*#readers*)))

;;; --------------------------------------------------------------------------------


(define lint
  (let ()
    (define +boolean+ #t)
    (define +any+ #f)
    (define +not-integer+ 1/2)
    (define +number+ 2)
    (define +integer+ 3)
    (define +string+ " ")
    (define +list+ (list 1))
    (define +vector+ (vector 1))
    (define +unspecified+ #<unspecified>)
    (define +symbol+ 'symbol)
    (define +character+ #\a)
    (define +let+ (inlet))
    (define +hash-table+ (hash-table))
    (define +output-port+ *stdout*)
    (define +input-port+ *stdin*)
    (define +c-pointer+ (c-pointer 0))
    (define +iterator+ (make-iterator "123"))
    (define +rng+ (make-random-state 123))
    (define +function+ (lambda args args))
    (define +macro+ (define-macro (_m_ . args) args))

    (define (integer-between-2-and-16? radix) 
      (and (integer? radix) 
	   (<= 2 radix 16)))
    
    (define (non-negative-integer? index)
      (and (integer? index) 
	   (not (negative? index))))
    
    (define (non-zero-number? x)
      (and (number? x)
	   (not (zero? x))))
    
    (define (real-but-not-rational? x)
      (and (real? x)
	   (not (rational? x))))
    
    (define (any-real? lst) ; ignore 0.0 and 1.0 in this since they normally work
      (and (pair? lst)
	   (or (and (number? (car lst))
		    (not (rational? (car lst)))
		    (not (= (car lst) 0.0))
		    (not (= (car lst) 1.0)))
	       (any-real? (cdr lst)))))
    
    (define (non-null-string? x)
      (and (string? x)
	   (> (length x) 0)))
    
    (define (non-null-vector? x)
      (and (vector? x)
	   (> (length x) 0)))
    
    (define (port? p)
      (or (input-port? p)
	  (output-port? p)))
    
    (define (thunk? p)
      (and (procedure? p)
	   (aritable? p 0)))

    (define (int-or-pair? p)
      (or (integer? p)
	  (pair? p)))
    
    (define (one-argable? p)
      (and (procedure? p)
	   (aritable? p 1)))
    
    (define (byte? i) 
      (and (integer? i) (<= 0 i 255)))
    
    (define (sequence? obj)
      ;; scheme and C types here are ok, so...
      (not (or (number? obj)
	       (char? obj)
	       (boolean? obj)
	       (symbol? obj))))
    
    (define (pair-or-null? obj) ; list? is proper-list?
      (or (pair? obj)
	  (null? obj)))

    (define (code-constant? x)
      (not (or (pair? x)
	       (symbol? x))))

    (define (t? obj) #t)

    (let ((no-side-effect-functions 
	   (let ((ht (make-hash-table)))
	     (for-each
	      (lambda (op) 
		(hash-table-set! ht op #t))
	      '(* + - / < <= = > >= 
		  abs acos acosh and angle append aritable? arity ash asin asinh assoc assq assv atan atanh 
		  begin boolean? boolean=?
		  caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr caddr cadr 
		  call-with-exit car case catch cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr 
		  cddar cdddar cddddr cdddr cddr cdr ceiling char->integer char-alphabetic? char-ci<=? char-ci<? 
		  char-ci=? char-ci>=? char-ci>? char-downcase char-lower-case? char-numeric? char-position char-ready? char-upcase 
		  char-upper-case? char-whitespace? char<=? char<? char=? char>=? char>? char? complex? cond 
		  cons constant? continuation? cos cosh curlet current-error-port current-input-port current-output-port 
		  defined? denominator do dynamic-wind 
		  inlet let-ref let?
		  eof-object? eq? equal? eqv? owlet even? exact->inexact exact? exp expt 
		  floor ;for-each 
		  gcd gensym gensym? rootlet
		  hash-table hash-table* hash-table-ref hash-table-size hash-table-entries hash-table? iterator? hook-functions 
		  if imag-part inexact->exact inexact? infinite? unlet input-port? integer->char integer-decode-float 
		  integer-length integer? 
		  keyword->symbol keyword? 
		  lcm length let let* letrec letrec* list list->string list->vector list-ref list-tail 
		  list? log logand logbit? logior lognot logxor 
		  macro? magnitude make-hash-table make-iterator make-hook make-keyword make-list make-polar
		  make-random-state make-rectangular make-string make-vector map max member memq memv min modulo morally-equal?
		  nan? negative? not null? number->string number? numerator 
		  object->string odd? openlet? or outlet output-port? 
		  pair? pair-line-number port-closed? port-filename port-line-number positive? 
		  procedure-documentation funclet
		  procedure-setter procedure-source dilambda? procedure? provided? 
		  quasiquote quote quotient 
		  ;random 
		  random-state? rational? rationalize real-part real? remainder reverse round 
		  s7-version sin sinh sqrt string string->list string->number string->symbol string-append string-ci<=? string-ci<? 
		  string-ci=? string-ci>=? string-ci>? string-length string-position string-ref string<=? string<? string=? string>=? 
		  string>? string? string-downcase string-upcase substring symbol symbol->dynamic-value symbol->keyword symbol->string symbol->value symbol? symbol=?
		  tan tanh truncate 
		  vector vector->list vector-dimensions vector-length vector-ref vector? 
		  zero?))
	     ht))
	  
	  (function-types (hash-table* 
			   '* +number+
			   '+ +number+
			   '- +number+
			   '/ +number+
			   '< +boolean+
			   '<= +boolean+
			   '= +boolean+
			   '> +boolean+
			   '>= +boolean+
			   'abs +number+
			   'acos +not-integer+
			   'acosh +not-integer+
			   'angle +number+
			   'aritable? +boolean+
			   'arity +list+
			   'ash +integer+
			   'asin +not-integer+
			   'asinh +not-integer+
			   'assoc 'list-or-f
			   'assq 'list-or-f
			   'assv 'list-or-f
			   'atan +not-integer+
			   'atanh +not-integer+
			   'boolean=? +boolean+
			   'boolean? +boolean+
			   'byte-vector? +boolean+
			   'c-pointer +c-pointer+
			   'ceiling +integer+
			   'char->integer +integer+
			   'char-alphabetic? +boolean+
			   'char-ci<=? +boolean+
			   'char-ci<? +boolean+
			   'char-ci=? +boolean+
			   'char-ci>=? +boolean+
			   'char-ci>? +boolean+
			   'char-downcase +character+
			   'char-lower-case? +boolean+
			   'char-numeric? +boolean+
			   'char-position 'integer-or-f
			   'char-ready? +boolean+
			   'char-upcase +character+
			   'char-upper-case? +boolean+
			   'char-whitespace? +boolean+
			   'char<=? +boolean+
			   'char<? +boolean+
			   'char=? +boolean+
			   'char>=? +boolean+
			   'char>? +boolean+
			   'char? +boolean+
			   'close-input-port +unspecified+
			   'close-output-port +unspecified+
			   'complex? +boolean+
			   'cons +list+
			   'constant? +boolean+
			   'continuation? +boolean+
			   'cos +number+
			   'cosh +not-integer+
			   'curlet +let+
			   'current-error-port +output-port+
			   'current-input-port +input-port+
			   'current-output-port +output-port+
			   'cyclic-sequences +list+
			   'define-bacro +macro+
			   'define-bacro* +macro+
			   'define-macro +macro+
			   'define-macro* +macro+
			   'defined? +boolean+
			   'denominator +integer+
			   'dilambda? +boolean+
			   'display +unspecified+
			   'eof-object? +boolean+
			   'eq? +boolean+
			   'equal? +boolean+
			   'eqv? +boolean+
			   'even? +boolean+
			   'exact->inexact +not-integer+
			   'exact? +boolean+
			   'exp +number+
			   'expt +number+
			   'float-vector +vector+
			   'float-vector-ref +number+
			   'float-vector-set! +number+
			   'float-vector? +boolean+
			   'floor +integer+
			   'flush-output-port +unspecified+
			   'for-each +unspecified+
			   'funclet +let+
			   'gcd +number+
			   'gensym +symbol+
			   'gensym? +boolean+
			   'get-output-string +string+
			   'hash-table +hash-table+
			   'hash-table* +hash-table+
			   'hash-table-entries +integer+
			   'hash-table-size +integer+
			   'hash-table? +boolean+
			   'imag-part +number+
			   'inexact->exact +number+
			   'inexact? +boolean+
			   'infinite? +boolean+
			   'inlet +let+
			   'inlet +let+
			   'input-port? +boolean+
			   'int-vector +vector+
			   'int-vector? +boolean+
			   'integer->char +character+
			   'integer-decode-float +list+
			   'integer-length +integer+
			   'integer? +boolean+
			   'iterator? +boolean+
			   'keyword->symbol +symbol+
			   'keyword? +boolean+
			   'lambda +function+
			   'lambda* +function+
			   'lcm +number+
			   'length +integer+   ; actually integer or #f, so we get false complaints about (if (length...) ..) never false
			   'let->list +list+
			   'let? +boolean+
			   'list +list+
			   'list->string +string+
			   'list->vector +vector+
			   'list-tail 'pair-or-null
			   'list? +boolean+
			   'log +number+
			   'logand +integer+
			   'logbit? +boolean+
			   'logior +integer+
			   'lognot +integer+
			   'logxor +integer+
			   'macro? +boolean+
			   'magnitude +number+
			   'make-float-vector +vector+
			   'make-hash-table +hash-table+
			   'make-int-vector +vector+
			   'make-iterator +iterator+
			   'make-keyword +symbol+
			   'make-list +list+
			   'make-polar +number+
			   'make-random-state +rng+
			   'make-rectangular +number+
			   'make-shared-vector +vector+
			   'make-string +string+
			   'make-vector +vector+
			   'map +list+
			   'max +number+
			   'member 'list-or-f
			   'memq 'list-or-f
			   'memv 'list-or-f
			   'min +number+
			   'modulo +number+
			   'morally-equal? +boolean+
			   'nan? +boolean+
			   'negative? +boolean+
			   'newline +unspecified+
			   'not +boolean+
			   'null? +boolean+
			   'number->string +string+
			   'number? +boolean+
			   'numerator +integer+
			   'object->string +string+
			   'odd? +boolean+
			   'open-input-file +input-port+
			   'open-output-file +output-port+
			   'open-input-string +input-port+
			   'open-output-string +output-port+
			   'openlet +let+
			   'openlet? +boolean+
			   'outlet +let+
			   'output-port? +boolean+
			   'owlet +let+
			   'pair-line-number +integer+
			   'pair? +boolean+
			   'peek-char 'char-or-eof
			   'port-closed? +boolean+
			   'port-file-name +string+
			   'port-line-number +integer+
			   'positive? +boolean+
			   'procedure? +boolean+
			   'provided? +boolean+
			   'quotient +number+
			   'random +number+
			   'random-state->list +list+
			   'random-state? +boolean+
			   'rational? +boolean+
			   'rationalize +number+
			   'read-byte 'number-or-eof
			   'read-char 'char-or-eof
			   'read-line 'string-or-eof
			   'read-string 'string-or-eof
			   'real-part +number+
			   'real? +boolean+
			   'remainder +number+
			   'rootlet +let+
			   'round +integer+
			   's7-version +string+
			   'sin +number+
			   'sinh +not-integer+
			   'sqrt +number+
			   'string +string+
			   'string->list +list+
			   'string->number 'number-or-f
			   'string->symbol +symbol+
			   'string-append +string+
			   'string-ci<=? +boolean+
			   'string-ci<? +boolean+
			   'string-ci=? +boolean+
			   'string-ci>=? +boolean+
			   'string-ci>? +boolean+
			   'string-copy +string+
			   'string-downcase +string+
			   'string-fill! +character+
			   'string-length +integer+
			   'string-position 'integer-or-f
			   'string-ref +character+
			   'string-upcase +string+
			   'string<=? +boolean+
			   'string<? +boolean+
			   'string=? +boolean+
			   'string>=? +boolean+
			   'string>? +boolean+
			   'string? +boolean+
			   'sublet +let+
			   'substring +string+
			   'symbol +symbol+
			   'symbol->string +string+
			   'symbol=? +boolean+
			   'symbol? +boolean+
			   'tan +number+
			   'tanh +not-integer+
			   'truncate +integer+
			   'varlet +let+
			   'vector +vector+
			   'vector->list +list+
			   'vector-append +vector+
			   'vector-dimensions +list+
			   'vector-length +integer+
			   'vector-rank +integer+
			   'vector? +boolean+
			   'write +unspecified+
			   'write-char +unspecified+
			   'write-string +unspecified+
			   'zero? +boolean+))
	  (argument-data (hash-table*
			  '* number?
			  '+ number?
			  '- number?
			  '->byte-vector string?
			  '/ number?
			  '< real?
			  '<= real?
			  '= number?
			  '> real?
			  '>= real?
			  'abs number?
			  'acos number?
			  'acosh number?
			  'angle number?
			  'ash (list integer? integer?)
			  'asin number?
			  'asinh number?
			  'assoc (list t? pair? procedure?)
			  'assq (list t? pair?)
			  'assv (list t? pair?)
			  'atan (list number? number?)
			  'atanh number?
			  'byte-vector byte?
			  'c-pointer integer?
			  'caaaar pair?
			  'caaadr pair?
			  'caaar pair?
			  'caadar pair?
			  'caaddr pair?
			  'caadr pair?
			  'caar pair?
			  'cadaar pair?
			  'cadadr pair?
			  'cadar pair?
			  'caddar pair?
			  'cadddr pair?
			  'caddr pair?
			  'cadr pair?
			  'call-with-current-continuation one-argable?
			  'call-with-exit one-argable?
			  'call-with-input-file (list string? procedure?) ; maybe these should also be one-argable?
			  'call-with-input-string (list string? procedure?)
			  'call-with-output-file (list string? procedure?)
			  'call-with-output-string procedure?
			  'call/cc one-argable?
			  'car pair?
			  'cdaaar pair?
			  'cdaadr pair?
			  'cdaar pair?
			  'cdadar pair?
			  'cdaddr pair?
			  'cdadr pair?
			  'cdar pair?
			  'cddaar pair?
			  'cddadr pair?
			  'cddar pair?
			  'cdddar pair?
			  'cddddr pair?
			  'cdddr pair?
			  'cddr pair?
			  'cdr pair?
			  'ceiling real?
			  'char->integer char?
			  'char-alphabetic? char?
			  'char-ci<=? char?
			  'char-ci<? char?
			  'char-ci=? char?
			  'char-ci>=? char?
			  'char-ci>? char?
			  'char-downcase char?
			  'char-lower-case? char?
			  'char-numeric? char?
			  'char-ready? port?
			  'char-upcase char?
			  'char-upper-case? char?
			  'char-whitespace? char?
			  'char<=? char?
			  'char<? char?
			  'char=? char?
			  'char>=? char?
			  'char>? char?
			  'close-input-port input-port?
			  'close-output-port output-port?
			  'cos number?
			  'cosh number?
			  'coverlet let?
			  'defined? (list symbol? let? boolean?)
			  'denominator rational?
			  'dilambda procedure?
			  'dynamic-wind (list thunk? thunk? thunk?)
			  'eval-string (list string? let?)
			  'even? integer?
			  'exact->inexact real?
			  'exact? number?
			  'exp number?
			  'expt (list number? number?)
			  'fill! (list sequence?)
			  'float-vector real?
			  'floor real?
			  'funclet procedure?
			  'gc boolean?
			  'gcd (list real? real?)
			  'gensym string?
			  'get-output-string (list output-port? boolean?)
			  'hash-table-entries hash-table?
			  'hash-table-ref (list hash-table?)
			  'hash-table-set! (list hash-table?)
			  'hash-table-size hash-table?
			  'imag-part number?
			  'inexact->exact real?
			  'inexact? number?
			  'infinite? number?
			  'input-port? port?
			  'int-vector integer?
			  'integer->char byte?
			  'integer-decode-float real-but-not-rational?
			  'integer-length integer?
			  'iterate iterator?
			  'keyword->symbol keyword?
			  'lcm (list real? real?)
			  'length sequence?
			  'let->list (list let?)
			  'let-ref (list let? symbol?)
			  'let-set! (list let? symbol?)
			  'list->string list?
			  'list->vector list?
			  'list-ref (list pair-or-null? non-negative-integer?)
			  'list-set! (list pair? non-negative-integer?)
			  'list-tail (list pair-or-null? non-negative-integer?)
			  'load (list non-null-string?)
			  'log (list number? non-zero-number?)
			  'logand (list integer? integer?)
			  'logbit? (list integer? integer?)
			  'logior (list integer? integer?)
			  'lognot integer?
			  'logxor (list integer? integer?)
			  'magnitude number?
			  'make-int-vector (list int-or-pair? integer?)
			  'make-float-vector (list int-or-pair? real?)
			  'make-hash-table (list non-negative-integer? procedure?)
			  'make-keyword string?
			  'make-list (list non-negative-integer?)
			  'make-polar real?
			  'make-random-state integer?
			  'make-rectangular real?
			  'make-shared-vector (list vector? int-or-pair? integer?)
			  'make-string (list non-negative-integer? char?)
			  'make-vector (list int-or-pair? t? boolean?)
			  'max real?
			  'member (list t? pair? procedure?) ; or list? 
			  'memq (list t? pair?)
			  'memv (list t? pair?)
			  'min real?
			  'modulo (list real? real?)
			  'nan? number?
			  'negative? real?
			  'number->string (list number? integer-between-2-and-16?)
			  'numerator rational?
			  'odd? integer?
			  'open-input-file (list string? string?)
			  'open-input-string string?
			  'openlet let?
			  'open-output-file (list string? string?)
			  'outlet let?
			  'output-port? port?
			  'pair-line-number pair?
			  'peek-char input-port?
			  'port-closed? port?
			  'port-filename input-port?
			  'port-line-number input-port?
			  'positive? real?
			  'procedure-documentation procedure?
			  'procedure-setter procedure?
			  'procedure-source procedure?
			  'provide symbol?
			  'provided? symbol?
			  'quotient (list real? real?)
			  'random (list number? random-state?)
			  'rationalize (list real? real?)
			  'read-char input-port?
			  'read-byte input-port?
			  'read-line (list input-port? boolean?)
			  'read-string (list integer? input-port?)
			  'real-part number?
			  'remainder (list real? real?)
			  'reverse sequence?
			  'reverse! sequence?
			  'round real?
			  'set-car! (list pair?)
			  'set-cdr! (list pair?)
			  'sin number?
			  'sinh number?
			  'sort! (list sequence? procedure?)
			  'sqrt number?
			  'string char?
			  'string->list string?
			  'string->number (list string? integer-between-2-and-16?)
			  'string->symbol string?
			  'string-append string?
			  'string-ci<=? string?
			  'string-ci<? string?
			  'string-ci=? string?
			  'string-ci>=? string?
			  'string-ci>? string?
			  'string-copy string?
			  'string-downcase string?
			  'string-fill! (list string? char? non-negative-integer? non-negative-integer?)
			  'string-length string?
			  'string-position (list string? string?)
			  'string-ref (list non-null-string? non-negative-integer?)
			  'string-set! (list non-null-string? non-negative-integer? char?)
			  'string-upcase string?
			  'string<=? string?
			  'string<? string?
			  'string=? string?
			  'string>=? string?
			  'string>? string?
			  'substring (list string? non-negative-integer? non-negative-integer?)
			  'symbol->dynamic-value symbol?
			  'symbol->keyword symbol?
			  'symbol->string symbol?
			  'symbol->value (list symbol?) ; opt arg is env
			  'symbol=? symbol?
			  'symbol string?
			  'symbol-access (list symbol? let?)
			  'system (list string? boolean?)
			  'tan number?
			  'tanh number?
			  'truncate real?
			  'vector->list vector?
			  'vector-append vector?
			  'vector-dimensions vector?
			  'vector-fill! (list vector?)
			  'vector-length vector?
			  'vector-ref (list non-null-vector? non-negative-integer?)
			  'vector-set! (list non-null-vector? non-negative-integer?)
			  'with-input-from-file (list string? thunk?)
			  'with-input-from-string (list string? thunk?)
			  'with-output-to-file (list string? thunk?)
			  'with-output-to-string thunk?
			  'write-byte (list byte?)
			  'write-char (list char?)
			  'write-string (list string?)
			  'zero? number?))

	  (deprecated-ops '((global-environment . rootlet)
			    (current-environment . curlet)
			    (make-procedure-with-setter . dilambda)
			    (procedure-with-setter? . dilambda?)

			    (data-format . sample-type)
			    (mus-sound-data-format . mus-sound-sample-type)
			    (mus-data-format-name . mus-sample-type-name)
			    (mus-data-format->string . mus-sample-type->string)))

	  (numeric-ops (let ((h (make-hash-table)))
			 (for-each
			  (lambda (op)
			    (set! (h op) #t))
			  '(+ * - / 
			      sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh 
			      log exp expt sqrt make-polar make-rectangular
			      imag-part real-part abs magnitude angle max min exact->inexact
			      modulo remainder quotient lcm gcd
			      rationalize inexact->exact random
			      logior lognot logxor logand numerator denominator 
			      floor round truncate ceiling ash))
			 h))

	  (repeated-args-table (let ((h (make-hash-table)))
				 (for-each
				  (lambda (op)
				    (set! (h op) #t))
				  '(= / max min < > <= >= - quotient remainder modulo and or
				      string=? string<=? string>=? string<? string>?
				      char=? char<=? char>=? char<? char>?
				      boolean=? symbol=?))
				 h))
	  
	  (repeated-args-table-2 (let ((h (make-hash-table)))
				   (for-each
				    (lambda (op)
				      (set! (h op) #t))
				    '(= max min < > <= >= and or
					string=? string<=? string>=? string<? string>?
					char=? char<=? char>=? char<? char>?
					boolean=? symbol=?))
				   h))

	  (syntaces (let ((h (make-hash-table)))
		      (for-each
		       (lambda (op)
			 (set! (h op) #t))
		       '(quote if begin let let* letrec cond case or and do set! unless when
			       with-let with-baffle
			       lambda lambda* define define* define-envelope
			       define-macro define-macro* define-bacro define-bacro* 
			       define-constant))
		      h))
	  
	  (format-control-char (let ((chars (make-vector 256 #f)))
				 (for-each
				  (lambda (c)
				    (vector-set! chars (char->integer c) #t))
				  '(#\A #\S #\C #\F #\E #\G #\O #\D #\B #\X #\, #\{ #\} #\@ #\P #\*
				    #\a #\s #\c #\f #\e #\g #\o #\d #\b #\x #\p #\N #\n #\W #\w
				    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
				 chars))
	  (f-types (list +any+ +boolean+ +symbol+ 'integer-or-f 'list-or-f 'number-or-f))
	  (sel-types (list +any+ +symbol+ +boolean+ +character+ +integer+ +number+ +unspecified+ 'char-or-eof))
	  (num-types (list integer? real? rational? complex?))
	  (v-types (list +any+ +symbol+))
	  (outport #t)
	  (loaded-files #f)
	  (globals #f)
	  (other-identifiers #f)
	  (last-simplify-boolean-line-number -1)
	  (last-simplify-numeric-line-number -1)
	  (line-number -1))


      ;; list version
      (define-constant var-name car)
      (define-constant var-ref cadr)
      (define-constant var-set caddr)
      (define-constant var-func-info cadddr)
      (define (set-cadr! v val) (list-set! v 1 val))
      (define (set-caddr! v val) (list-set! v 2 val))
      (define (set-cadddr! v val) (list-set! v 3 val))
      (set! (procedure-setter cadr) set-cadr!)
      (set! (procedure-setter caddr) set-caddr!)
      (set! (procedure-setter cadddr) set-cadddr!)
      (define-constant var-type (dilambda (lambda (v) (list-ref v 4)) (lambda (v x) (list-set! v 4 x))))
      (define-constant var-value (dilambda (lambda (v) (list-ref v 5)) (lambda (v x) (list-set! v 5 x))))
      (define make-var (lambda* (name ref set fnc typ val :allow-other-keys)
	;(reflective-probe)
	(list name ref set fnc typ val)))
      (define-constant var? pair?)
      (define-constant var-member assq)

#|      
      ;; vector version
      (define var-name (dilambda (lambda (v) (v 0)) (lambda (v x) (set! (v 0) x))))
      (define var-ref (dilambda (lambda (v) (v 1)) (lambda (v x) (set! (v 1) x))))
      (define var-set (dilambda (lambda (v) (v 2)) (lambda (v x) (set! (v 2) x))))
      (define var-func-info (dilambda (lambda (v) (v 3)) (lambda (v x) (set! (v 3) x))))
      (define var-type (dilambda (lambda (v) (v 4)) (lambda (v x) (set! (v 4) x))))
      (define var-value (dilambda (lambda (v) (v 5)) (lambda (v x) (set! (v 5) x))))
      (define* (make-var name ref set fnc typ val :allow-other-keys) 
        (vector name ref set fnc typ val))
      (define var? vector?)
      (define (mf a b) (eq? a (vector-ref b 0)))
      (define (var-member v q) (let ((lst (member v q mf))) (and lst (car lst))))

      ;; environment version
      (define var-name (dilambda (lambda (v) (v 'name)) (lambda (v x) (set! (v 'name) x))))
      (define var-ref (dilambda (lambda (v) (v 'ref)) (lambda (v x) (set! (v 'ref) x))))
      (define var-set (dilambda (lambda (v) (v 'set)) (lambda (v x) (set! (v 'set) x))))
      (define var-func-info (dilambda (lambda (v) (v 'fnc)) (lambda (v x) (set! (v 'fnc) x))))
      (define var-type (dilambda (lambda (v) (v 'type)) (lambda (v x) (set! (v 'type) x))))
      (define var-value (dilambda (lambda (v) (v 'value)) (lambda (v x) (set! (v 'value) x))))
      (define* (make-var name ref set fnc typ val :allow-other-keys)
        (inlet 'name name 'ref ref 'set set 'fnc fnc 'type typ 'value val))
      (define var? let?)
      (define (mf a b) (eq? a (b 'name)))
      (define (var-member v q) (let ((lst (member v q mf))) (and lst (car lst))))
|#


      (define (->type c)
	(cond ((pair? c)
	       (if (symbol? (car c))
		   (hash-table-ref function-types (car c))
		   (if (pair? (car c))
		       +any+ ; might be expr as func
		       +list+)))
	      ((integer? c) +integer+)
	      ((symbol? c) +symbol+)
	      ((number? c) +not-integer+)
	      ((null? c) +list+)
	      ((string? c) +string+)
	      ((char? c) +character+)
	      ((boolean? c) +boolean+)
	      ((vector? c) +vector+)
	      ((let? c) +let+)
	      ((hash-table? c) +hash-table+)
	      ((input-port? c) +input-port)
	      ((output-port? c) +output-port+)
	      (#t (if (eq? c #<unspecified>) +unspecified+ +any+))))
      
      (define (type-compatible type obj)
	(or (eq? type +any+)
	    (let ((obj-type (->type obj)))
	      (or (equal? type obj-type)
		  (and (equal? type +number+) (equal? obj-type +integer+))
		  (and (equal? type +integer+) (equal? obj-type +number+))
		  (and (equal? type 'char-or-eof) (equal? obj-type +character+))))))

      (define (never-false expr)
	(or (eq? expr #t)
	    (let ((type (if (pair? expr)
			    (hash-table-ref function-types (car expr))
			    (->type expr))))
	      (not (member type f-types)))))
	
      (define (never-true expr)
	(or (not expr)
	    (and (pair? expr)
		 (eq? (car expr) 'not)
		 (never-false (cadr expr)))))

      ;; --------------------------------------------------------------------------------
      
      (define (truncated-list->string form)
	;; return form -> string with limits on its length
	(let* ((str (object->string form))
	       (len (length str)))
	  (if (< len 8)
	      (format #f " ~A" str)
	      (if (<= len 80)
		  (format #f "~%        ~A" str)
		  (do ((i 77 (- i 1)))
		      ((or (= i 40)
			   (char-whitespace? (str i)))
		       (format #f "~%        ~A..." (substring str 0 (if (<= i 40) 77 i)))))))))
      
      (define (lists->string f1 f2)
	;; same but 2 strings that may need to be lined up vertically
	(let* ((str1 (object->string f1))
	       (len1 (length str1))
	       (str2 (object->string f2))
	       (len2 (length str2)))
	  (if (< (+ len1 len2) 20)
	      (format #f " ~A -> ~A" str1 str2)
	      (if (< (+ len1 len2) 70)
		  (format #f "~%        ~A -> ~A" str1 str2)
		  (format #f "~%        ~A ->~%        ~A" str1 str2)))))
      
      (define (lint-format str name . args)
	(if (and (positive? line-number)
		 (< line-number 100000))
	    (apply format outport (string-append "  ~A (line ~D): " str "~%") name line-number args)
	    (apply format outport (string-append "  ~A: " str "~%") name args)))
      
      (define (side-effect? form env)
	;; could evaluation of form have any side effects (like IO etc)
	(if (and (list? form)                   ; we don't want dotted lists or () here
		 (not (null? form)))
	    ;; can't optimize ((...)...) because the car might eval to a function
	    (or (and (not (hash-table-ref no-side-effect-functions (car form))) ; if func is not in that list, make no assumptions about it
		     (or (not (eq? (car form) 'format))                 ; (format #f ...)
			 (cadr form)))
		(call-with-exit
		 (lambda (return)
		   (for-each
		    (lambda (f)
		      (if (side-effect? f env)
			  (return #t)))
		    (cdr form))
		   #f)))
	    (and (symbol? form)
		 (not (hash-table-ref no-side-effect-functions form))
		 (let ((e (or (var-member form env) (hash-table-ref globals form))))
		   (and (var? e)
			(pair? (var-func-info e))))))) ; it is a local function

      (define (just-constants? form env)
	;; can we probably evaluate form given just built-in stuff?
	(or (and (constant? form)
		 (not (pair? form))
		 (not (vector? form)))
	    (and (pair? form)
		 (or (and (symbol? (car form))
			  (hash-table-ref no-side-effect-functions (car form))
			  (not (hash-table-ref globals (car form)))
			  (not (var-member (car form) env))) ; e.g. exp declared locally as a list
		     (and (constant? (car form))
			  (not (pair? (car form)))
			  (not (vector? (car form)))))
		 (just-constants? (cdr form) env))))

      
      (define (equal-ignoring-constants? a b)
	(or (equal? a b)
	    (and (symbol? a)
		 (constant? a) 
		 (morally-equal? (symbol->value a) b))
	    (and (symbol? b)
		 (constant? b)
		 (morally-equal? (symbol->value b) a))
	    (and (pair? a)
		 (pair? b)
		 (equal-ignoring-constants? (car a) (car b))
		 (equal-ignoring-constants? (cdr a) (cdr b)))))

      
      (define (just-symbols? form)
	(or (null? form)
	    (symbol? form)
	    (and (pair? form)
		 (symbol? (car form))
		 (just-symbols? (cdr form)))))
      
      
      (define (repeated-member? lst env)
	(and (pair? lst)
	     (or (and (or (not (pair? (car lst)))
			  (and (not (side-effect? (car lst) env))
			       (not (eq? (caar lst) 'random))))
		      (pair? (cdr lst))
		      (member (car lst) (cdr lst)))
		 (repeated-member? (cdr lst) env))))
      
      
      (define (check-for-repeated-args name head form env)
	(if (and (or (memq head '(eq? eqv? equal?))
		     (and (= (length form) 3)
			  (hash-table-ref repeated-args-table head)))
		 (repeated-member? (cdr form) env))
	    (lint-format "this looks odd:~A"
			 name
			 ;; sigh (= a a) could be used to check for non-finite numbers, I suppose,
			 ;;   and (/ 0 0) might be deliberate (as in gmp)
			 ;;   also (min (random x) (random x)) is not pointless
			 (truncated-list->string form))
	    (if (and (hash-table-ref repeated-args-table-2 head)
		     (repeated-member? (cdr form) env))
		(lint-format "it looks odd to have repeated arguments in~A" name (truncated-list->string form)))))
      
      
      (define (check-for-repeated-args-with-not name form env)
	
	(define (repeated-member-with-not? lst env)
	  (and (pair? lst)
	       (or (and (or (not (pair? (car lst)))
			    (not (side-effect? (car lst) env)))
			(or (member (list 'not (car lst)) (cdr lst))
			    (and (pair? (car lst))
				 (eq? (caar lst) 'not)
				 (= (length (car lst)) 2)
				 (member (cadar lst) (cdr lst)))))
		   (repeated-member-with-not? (cdr lst) env))))
	
	(if (repeated-member-with-not? (cdr form) env)
	    (lint-format "this looks odd:~A" name (truncated-list->string form))))
      
      
      (define (check-args name head form checkers env max-arity)
	;; check for obvious argument type problems
	;; name = overall caller, head = current caller, checkers = proc or list of procs for checking args

	(define (prettify-arg-number argn)
	  (if (or (not (= argn 1))
		  (pair? (cddr form)))
	      (format #f "~D " argn)
	      ""))

	(let ((arg-number 1))
	  (call-with-exit
	   (lambda (done)
	     (for-each 
	      (lambda (arg)
		(let ((checker (if (list? checkers) 
				   (car checkers) 
				   checkers)))
		  (if (pair? arg)
		      (if (eq? (car arg) 'quote)
			  (if (and checker
				   (not (checker (if (pair? (cadr arg)) +list+ (->type (cadr arg))))))
			      (lint-format "~A's argument ~Ashould be a~A ~A: ~S:~A" 
					   name head 
					   (prettify-arg-number arg-number)
					   (if (char=? (string-ref (format #f "~A" checker) 0) #\i) "n" "")
					   checker arg 
					   (truncated-list->string form)))
			  (let ((op (hash-table-ref function-types (car arg))))
			    (when op
			      (if (and (symbol? op)
				       (not (eq? symbol +symbol+)))
				  (if (and checker
					   *report-minor-stuff*)
				      (if (memq op '(number-or-f list-or-f))
					  (lint-format "~A's argument ~Amight be #f:~A"
						       name head 
						       (prettify-arg-number arg-number)
						       (truncated-list->string form))
					  (if (memq op '(number-or-eof char-or-eof string-or-eof))
					      (lint-format "~A's argument ~Amight be #<eof>:~A"
							   name head 
							   (prettify-arg-number arg-number)
							   (truncated-list->string form)))))
				  (if (or (not (checker op))
					  (and (just-constants? arg env)
					       (catch #t 
						 (lambda ()
						   (not (checker (eval arg))))
						 (lambda ignore-catch-error-args
						   #f))))
				      (lint-format "~A's argument ~Ashould be a~A ~A: ~S:~A" 
						   name head 
						   (prettify-arg-number arg-number)
						   (if (char=? (string-ref (format #f "~A" checker) 0) #\i) "n" "")
						   checker arg 
						   (truncated-list->string form))
				      
				      (if (and (eq? (car arg) 'if)
					       (= (length arg) 3)
					       (not (checker #<unspecified>)))
					  (lint-format "~A argument might be ~A:~A"
						       name head
						       #<unspecified>
						       (truncated-list->string form))))))))
		      
		      (if (and (symbol? arg)
			       (not (keyword? arg)))
			  ;; if we're in a loop of some sort and the set! follows the ref,
			  ;;   this can be fooled, especially by with-let
			  (let ((var-data (or (var-member arg env) (hash-table-ref globals arg))))
			    (if (and (var? var-data)
				     (not (var-ref var-data)) ; a stop-gap -- refd?
				     (not (var-set var-data)) ;               set?
				     (not (memq (var-type var-data) v-types))
				     (not (checker (var-type var-data))))
				(lint-format "~A's argument ~Amight not be a~A ~A: ~S:~A" 
					     name head 
					     (prettify-arg-number arg-number)
					     (if (char=? (string-ref (format #f "~A" checker) 0) #\i) "n" "")
					     checker arg
					     (truncated-list->string form)))
			    (if (and (not (memq arg '(angle)))
				     (not (var-member arg env))
				     (hash-table-ref globals arg)
				     (not (checker (symbol->value arg))))
				(lint-format "~A's argument ~Ashould be a~A ~A: ~S:~A" 
					     name head 
					     (prettify-arg-number arg-number)
					     (if (char=? (string-ref (format #f "~A" checker) 0) #\i) "n" "")
					     checker arg 
					     (truncated-list->string form))))
			  (if (not (checker arg))
			      (lint-format "~A's argument ~Ashould be a~A ~A: ~S:~A" 
					   name head
					   (prettify-arg-number arg-number)
					   (if (char=? (string-ref (format #f "~A" checker) 0) #\i) "n" "")
					   checker arg 
					   (truncated-list->string form)))))
		  (if (list? checkers)
		      (if (null? (cdr checkers))
			  (done)
			  (set! checkers (cdr checkers))))
		  (set! arg-number (+ arg-number 1))
		  (if (> arg-number max-arity) (done))))
	      (cdr form))))))
      
      
      (define (set-ref? name env)
	;; if name is in env, set its "I've been referenced" flag
	(let ((data (or (var-member name env) (hash-table-ref globals name))))
	  (if (var? data)
	      (set! (var-ref data) #t)))
	env)
      
      (define (set-set? name new-val env)
	(let ((data (or (var-member name env) (hash-table-ref globals name))))
	  (if (var? data)
	      (begin
		(set! (var-value data) new-val)
		(set! (var-set data) #t)))))
      
      
      (define (proper-list lst)
	;; return lst as a proper list
	(if (pair? lst)
	    (cons (car lst) 
		  (if (pair? (cdr lst)) 
		      (proper-list (cdr lst)) 
		      (if (null? (cdr lst)) 
			  () 
			  (list (cdr lst)))))
	    lst))

      
      (define (keywords lst)
	(let ((count 0))
	  (do ((p lst (cdr p)))
	      ((null? p) count)
	    (if (keyword? (car p))
		(set! count (+ count 1))))))
      ;(count-if keyword? lst))
      
      (define (tree-member sym tree)
	(and (pair? tree)
	     (or (eq? (car tree) sym)
		 (and (pair? (car tree))
		      (tree-member sym (car tree)))
		 (tree-member sym (cdr tree)))))
      
      (define (tree-car-member sym tree)
	(and (pair? tree)
	     (or (eq? (car tree) sym)
		 (and (pair? (car tree))
		      (tree-car-member sym (car tree)))
		 (and (pair? (cdr tree))
		      (call-with-exit
		       (lambda (return)
			 (for-each
			  (lambda (subtree)
			    (if (tree-car-member sym subtree)
				(return #t)))
			  (cdr tree))
			 #f))))))
      
      (define (remove item sequence)
	(let ((got-it #f))
	  (map (lambda (x)
		 (if (and (not got-it)
			  (eqv? x item))
		     (begin
		       (set! got-it #t)
		       (values))
		     x))
	       sequence)))
      
      (define (remove-all item sequence)
	(map (lambda (x)
	       (if (equal? x item)
		   (values)
		   x))
	     sequence))
      
      (define (remove-if p l)
	(cond ((null? l) ())
	      ((p (car l)) (remove-if p (cdr l)))
	      (else (cons (car l) 
			  (remove-if p (cdr l))))))

      (define (checked-eval form)
	(catch #t
	  (lambda ()
	    (eval form))
	  (lambda args
	    #t)))   ; just ignore errors in this context


      (define (simplify-boolean in-form true false env)
	;; (or)->#f, (or x) -> x, (or x ... from here on we know x is #f), (or x #t...) -> (or x #t), any constant expr can be collapsed
	;;   (or ... (or ...) ...) -> or of all, (or ... #f ...) toss the #f
	;; similarly for and
	;; (or ... (not (and ...))) -> (or ... (not x) [from here we know x is true] (not y)...)
	;; (or ... (not (and x1 x2 ...))) -> (or ... (not x1) (not x2)...), but is that simpler?
	
	;; I wonder how far this could be pushed
	;;   (or x1 x2 x1) -> (or x1 x2) 
	;;   (and x1 x2 x1) -> (and x2 x1)
	
	(define (bsimp uform)
	  ;; find and remove any expressions that have no effect on the outcome
	  (if (or (not (pair? uform))
		  (not (memq (car uform) '(and or not)))
		  (side-effect? uform env))
	      uform
	      
	      (let ((vars ())
		    (associated-exprs ())
		    (ctr 0))
		
		(define (tree-remove-all x lst) 
		  (cond ((null? lst) ()) 
			((equal? (car lst) x) (tree-remove-all x (cdr lst)))
			((pair? (car lst)) (cons (tree-remove-all x (car lst)) (tree-remove-all x (cdr lst))))
			(else (cons (car lst) (tree-remove-all x (cdr lst))))))
		
		(define (canonical-tree lst)
		  (let ((data (assoc lst associated-exprs)))
		    (if data
			(cdr data)
			(if (pair? lst) 
			    (cons (canonical-tree (car lst)) 
				  (canonical-tree (cdr lst))) 
			    lst))))
		
		(define (expand expr)
		  (define (car-rassoc val lst)
		    (if (not (pair? lst))
			#f
			(if (equal? (cdar lst) val)
			    (car lst)
			    (car-rassoc val (cdr lst)))))
		  (let ((data (car-rassoc expr associated-exprs)))
		    (if data
			(copy (car data))
			(if (pair? expr)
			    (cons (expand (car expr))
				  (expand (cdr expr)))
			    expr))))
		
		(define (bool-walk form func) 
		  (if (and (pair? form)
			   (memq (car form) '(and or not)))
		      (for-each
		       (lambda (e)
			 (bool-walk e func))
		       (cdr form))
		      (func form)))
		
		(bool-walk uform (lambda (val) 
				   (if (and (or (pair? val)
						(symbol? val))
					    (not (assoc val associated-exprs))
					    (not (memq val '(and or not)))) ; (not not)
				       (let ((new-var (string->symbol (format #f "bool-~D" ctr))))
					 (set! vars (cons new-var vars))
					 (set! associated-exprs (cons (cons val new-var) associated-exprs))
					 (set! ctr (+ ctr 1))))))
		
		(if (or (null? vars)
			(> (length vars) 8))
		    uform
		    (let ((len (length vars)))
		      (let ((vsize (expt 2 len))) ; 2^n possible cases
			(let ((v (make-vector vsize))
			      (vals ())
			      (nonf (make-vector len))
			      (cur 0)
			      (ctr 0)
			      (form (canonical-tree uform)))
			  
			  (for-each
			   (lambda (var)
			     (do ((i cur (+ i 1)))
				 ((not (tree-member i form))
				  (set! cur (+ i 1))
				  (vector-set! nonf ctr i)
				  (set! ctr (+ ctr 1)))))
			   vars)
			  
			  (let ((new-func (apply lambda vars form ())))
			    (do ((ctr 0 (+ ctr 1)))
				((= ctr vsize))
			      (vector-set! v ctr (apply new-func (let ((args ()))
								   (do ((i 0 (+ i 1)))
								       ((= i len) (reverse args))
								     (set! args (cons (and (logbit? ctr i) 
											   (vector-ref nonf i))
										      args))))))
			      (if (not (member (vector-ref v ctr) vals))
				  (set! vals (cons (vector-ref v ctr) vals)))))
			  
			  (if (= (length vals) 1)
			      (car vals)
			      (let ((none-vars ())
				    (pos -1))
				(for-each
				 (lambda (var)
				   (set! pos (+ pos 1))
				   (call-with-exit
				    (lambda (return)
				      (do ((ctr 0 (+ ctr 1)))
					  ((= ctr vsize)
					   (set! none-vars (cons var none-vars)))
					(if (and (not (logbit? ctr pos))
						 (not (equal? (vector-ref v ctr) (vector-ref v (logior ctr (ash 1 pos))))))
					    (return #f))))))
				 vars)
				
				(if (pair? none-vars)
				    (begin
				      (for-each
				       (lambda (nv)
					 (set! form (tree-remove-all nv form)))
				       none-vars)
				      (expand form))
				    uform))))))))))
	
	(define (true? e)
	  (or (member e true)
	      (and (pair? e)
		   (= (length e) 2)
		   (or (member e true 
			       (lambda (a b)
				 ;; if a follows b, and b is true, do we know already know that a?
				 ;; (and (< x1 12) (real? x1) (= x1 1)) -> (and (< x1 12) (= x1 1))
				 (and (pair? b)
				      (or (and (= (length b) 2)
					       (equal? (cadr a) (cadr b))
					       (case (car a)
						 ((complex?)  (memq (car b) '(number? real? rational? integer? even? odd? 
										      positive? negative? zero? exact? inexact?)))
						 ((number?)   (memq (car b) '(complex? real? rational? integer? even? odd? 
										       positive? negative? zero? exact? inexact?)))
						 ((real?)     (memq (car b) '(rational? integer? even? odd? positive? negative? exact? inexact?)))
						 ((rational?) (memq (car b) '(integer? even? odd?)))
						 ((integer?)  (memq (car b) '(even? 'odd?)))
						 (else #f)))
					  (and (> (length b) 2)
					       (member (cadr a) (cdr b))
					       (case (car a)
						 ((complex? number?) (eq? (car b) '=))
						 ((real?)            (memq (car b) '(< > <= >=)))
						 (else #f)))))))
		       (and (pair? (cadr e))
			    (case (car e)
			      ((complex? number?) (number? (hash-table-ref function-types (caadr e))))
			      ((exact? rational?) (eq? (caadr e) 'inexact->exact))
			      ((inexact? real?)   (eq? (caadr e) 'exact->inexact))
			      ((char?)            (char? (hash-table-ref function-types (caadr e))))
			      ((string?)          (string? (hash-table-ref function-types (caadr e))))
			      ((vector?)          (vector? (hash-table-ref function-types (caadr e))))
			      (else #f)))))))
	
	(define (false? e)
	  
	  (define (bad-arg-match a b)
	    
	    ;; these accept only the given type and can return a boolean (so their value in a boolean expression is not known in advance)
	    (define (number-op? x) (memq x '(= < > <= >= even? odd? positive? negative? zero?)))
	    (define (char-op? x)   (memq x '(char=? char<? char<=? char>? char>=? char-ci=? char-ci<? char-ci<=? char-ci>? char-ci>=? 
						    char-alphabetic? char-numeric? char-whitespace? char-lower-case? char-upper-case?)))
	    (define (list-op? x)   (memq x '(caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr caddr 
						    cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar 
						    cddddr cdddr cddr cdr list-ref)))
	    (define (string-op? x) (memq x '(string=? string<? string<=? string>? string>=? string-ci=? string-ci<? string-ci<=? string-ci>? string-ci>=?)))
	    
	    (case a
	      ((complex? number? real? rational? integer?) 
	       (or (char-op? b)     ; that is, if these are false, then a non-number was accepted
		   (list-op? b)     ;    earlier as a valid argument, so it can't be a number
		   (string-op? b))) ;    (or (char=? x1 #\a) (complex? x1) x1) -> (or (char=? x1 #\a) x1)
	      ((char?) 
	       (or (number-op? b)
		   (list-op? b)
		   (string-op? b)))
	      ((string?) 
	       (or (char-op? b)
		   (list-op? b)
		   (number-op? b)))
	      ((list?) 
	       (or (char-op? b)
		   (number-op? b)
		   (string-op? b)))
	      ((boolean? procedure? symbol? continuation? let? let?)
	       (or (char-op? b)
		   (number-op? b)
		   (list-op? b)
		   (string-op? b)))
	      (else #f)))
	  
	  (or (member e false)
	      (and (pair? e)
		   (= (length e) 2)
		   (or (member e false (lambda (a b)
					 (and (pair? b)
					      (>= (length b) 2)
					      (member (cadr a) (cdr b))
					      (bad-arg-match (car a) (car b)))))
		       (member e true (lambda (a b)
					(and (pair? b)
					     (>= (length b) 2)
					     (member (cadr a) (cdr b))
					     (bad-arg-match (car a) (car b)))))
		       (and (eq? (car e) 'null?)
			    (pair? (cadr e))
			    (eq? (hash-table-ref function-types (caadr e)) 'list-or-f))))))
	
	
	(define (contradictory? ands)
	  (let ((vars ()))
	    (call-with-exit
	     (lambda (return)
	       (do ((b ands (cdr b)))
		   ((null? b) #f)
		 (if (and (pair? b)
			  (pair? (car b))
			  (pair? (cdar b)))
		     (let* ((func (caar b))
			    (arg-type (or (hash-table-ref argument-data func)
					  (and (memq func '(string? pair? symbol? number? hash-table? boolean? char? vector? procedure?))
					       (symbol->value func))
					  (and (memq func '(complex? integer? rational? real?))
					       number?)
					  (and (memq func '(null? list?))
					       pair?)))
			    (args (cdar b)))
		       (if (memq arg-type num-types)
			   (set! arg-type number?)
			   (if (eq? arg-type list?)
			       (set! arg-type pair?)))
		       
		       (if (and (procedure? arg-type)
				(not (eq? arg-type sequence?)))
			   (for-each
			    (lambda (arg)
			      (if (symbol? arg)
				  (let ((type (assq arg vars)))
				    (if (not type)
					(set! vars (cons (cons arg arg-type) vars))
					(if (not (eq? (cdr type) arg-type))
					    (return #t))))))
			    args)))))))))
	
	
	(define (classify e)
	  ;; do we already know that e is true or false?
	  ;;   some cases subsume others: if we know (integer? x) is true, (complex? x) is also true
	  (if (true? e)
	      #t ; the simple boolean is passed back which will either be dropped or will stop the outer expr build
	      (if (false? e)
		  #f
		  ;; eval of a constant expression here is tricky -- for example, (sqrt 2) should not be turned into 1.414...
		  (if (just-constants? e env)
		      (catch #t
			(lambda ()
			  (let ((val (eval e)))
			    (if (boolean? val)
				val
				e)))
			(lambda ignore e))
		      e))))
	
	(define (store e value and/or)
	  ;; we can't make any assumptions about the expression if it might have side effects
	  ;;   for example (or (= (oscil o) 0.0) (= (oscil o) 0.0)) can't be reduced
	  
	  (if (or (not (pair? e))
		  (not (side-effect? e env)))
	      (let ((its-true (if (eq? and/or 'or)
				  (eq? value #t)             ; or, so it's false if unknown
				  value)))
		(if its-true
		    (set! true (cons e true))
		    (set! false (cons e false))))))
	
	(let ((form (bsimp in-form)))
	  (if (or (not (pair? form))
		  (not (memq (car form) '(or and not))))
	      (classify form)
	      (let ((len (length form)))
		
		(case (car form)
		  
		  ((not)
		   (if (= len 2)
		       (let* ((arg (cadr form))
			      (val (if (and (pair? arg)
					    (memq (car arg) '(and or not)))
				       (classify (simplify-boolean arg true false env))
				       (classify arg))))
			 (if (boolean? val)
			     (not val)
			     (if (or (code-constant? arg)
				     (and (pair? arg)
					  (symbol? (car arg))
					  (not (hash-table-ref globals (car arg)))
					  (not (member (hash-table-ref function-types (car arg)) '(#f #t list-or-f number-or-f integer-or-f)))
					  (not (var-member (car arg) env))))
				 #f
				 (if (and (pair? arg)               ; (not (not ...)) -> ...
					  (pair? (cdr arg))
					  (eq? (car arg) 'not))
				     (cadr arg)
				     (if (not (equal? val arg))
					 `(not ,val)
					 (if (and (pair? arg)
						  (<= (length arg) 3)) ; avoid (<= 0 i 12) and such
					     (case (car arg)
					       ((<)            `(>= ,@(cdr arg)))   ; (not (< ...)) -> (>= ...) 
					       ((>)            `(<= ,@(cdr arg)))
					       ((<=)           (if (morally-equal? (caddr arg) 0) `(positive? ,(cadr arg)) `(> ,@(cdr arg))))
					       ((>=)           (if (morally-equal? (caddr arg) 0) `(negative? ,(cadr arg)) `(< ,@(cdr arg))))
					       ((char<?)       `(char>=? ,@(cdr arg)))
					       ((char>?)       `(char<=? ,@(cdr arg)))
					       ((char<=?)      `(char>? ,@(cdr arg)))
					       ((char>=?)      `(char<? ,@(cdr arg)))
					       ((char-ci<?)    `(char-ci>=? ,@(cdr arg)))
					       ((char-ci>?)    `(char-ci<=? ,@(cdr arg)))
					       ((char-ci<=?)   `(char-ci>? ,@(cdr arg)))
					       ((char-ci>=?)   `(char-ci<? ,@(cdr arg)))
					       ((string<?)     `(string>=? ,@(cdr arg)))
					       ((string>?)     `(string<=? ,@(cdr arg)))
					       ((string<=?)    `(string>? ,@(cdr arg)))
					       ((string>=?)    `(string<? ,@(cdr arg)))
					       ((string-ci<?)  `(string-ci>=? ,@(cdr arg)))
					       ((string-ci>?)  `(string-ci<=? ,@(cdr arg)))
					       ((string-ci<=?) `(string-ci>? ,@(cdr arg)))
					       ((string-ci>=?) `(string-ci<? ,@(cdr arg)))
					       ((odd?)         `(even? ,@(cdr arg)))
					       ((even?)        `(odd? ,@(cdr arg)))
					       ((exact?)       `(inexact? ,@(cdr arg)))
					       ((inexact?)     `(exact? ,@(cdr arg)))
					       ;; ((null?)        `(pair? ,@(cdr arg)))
					       ;;      this is not quite right
					       ;; char-upper-case? and lower are not switchable here

					       ;; if stuff loaded, (not (every? ...)) => any? and so on

					       ((zero?)       ; (not (zero? (logand p (ash 1 i)))) -> (logbit? p i)
						(let ((zarg (cadr arg)))  ; (logand...)
						  (if (and (pair? zarg)
							   (eq? (car zarg) 'logand)
							   (pair? (cddr zarg))
							   (pair? (caddr zarg))
							   (eq? (caaddr zarg) 'ash)
							   (equal? (cadr (caddr zarg)) 1))
						      `(logbit? ,(cadr zarg) ,(caddr (caddr zarg)))
						      form)))
					       (else form))
					     form))))))
		       form))
		  
		  ((or)
		   (if (= len 1)
		       #f
		       (if (= len 2)
			   (if (code-constant? (cadr form))
			       (cadr form)
			       (classify (cadr form)))
			   (if (true? (cadr form)) ; no need to check anything else
			       #t                  ; side-effect? here is a nightmare
			       (let ((new-form ()))
				 (do ((exprs (cdr form) (cdr exprs)))
				     ((null? exprs) 
				      (if (null? new-form)
					  #f
					  (if (null? (cdr new-form))
					      (car new-form)
					      `(or ,@(reverse new-form)))))
				   (let* ((e (car exprs))
					  (val (classify e)))
				     
				     (if (and (pair? val)
					      (memq (car val) '(and or not)))
					 (set! val (classify (set! e (simplify-boolean e true false env)))))
				     
				     (if val                                ; #f in or is ignored
					 (if (or (eq? val #t)               ; #t or any non-#f constant in or ends the expression
						 (code-constant? val))
					     (begin
					       (if (null? new-form)         ; (or x1 123) -> value of x1 first
						   (set! new-form (list val))           ;was `(,val))
						   (set! new-form (cons val new-form))) ;was (append `(,val) new-form))) ; reversed when returned
					       (set! exprs '(#t)))
					     
					     ;; (or x1 x2 x1) -> (or x1 x2) is ok because if we get to x2, x1 is #f, so trailing x1 would still be #f
					     
					     (if (and (pair? e)             ; (or ...) -> splice into current
						      (eq? (car e) 'or))
						 (set! exprs (append e (cdr exprs))) ; we'll skip the 'or in do step
						 (begin                     ; else add it to our new expression with value #f
						   (store e val 'or)
						   (if (not (memq val new-form))
						       (set! new-form (cons val new-form))))))))))))))
		  
		  ((and)
		   (if (= len 1)
		       #t
		       (if (= len 2)
			   (classify (cadr form))
			   (if (contradictory? (cdr form))
			       #f
			       (let ((new-form ()))
				 (if (and (= len 3)
					  (symbol? (cadr form))
					  (eq? (cadr form) (caddr form)))
				     (cadr form)
				     (do ((exprs (cdr form) (cdr exprs)))
					 ((null? exprs) 
					  (if (null? new-form)
					      #t
					      (let* ((nform (reverse new-form))
						     (newer-form (map (lambda (x cdr-x)
									(if (and x (code-constant? x))
									    (values)
									    x))
								     nform (cdr nform))))
						(if (null? newer-form)
						    (car new-form)
						    `(and ,@newer-form ,(car new-form))))))
				       
				       (let* ((e (car exprs))
					      (val (classify e)))
					 
					 (if (and (pair? val)
						  (memq (car val) '(and or not)))
					     (set! val (classify (set! e (simplify-boolean e true false env)))))
					 
					 ;; (and x1 x2 x1) is not reducible, unless to (and x2 x1)
					 ;;   the final thing has to remain at the end, but can be deleted earlier if it can't short-circuit the evaluation,
					 ;;   but if there are expressions following the first x1, we can't be sure that it is not
					 ;;   protecting them:
					 ;;       (and false-or-0 (display (list-ref lst false-or-0)) false-or-0)
					 ;;   so I'll not try to optimize that case.  But (and x x) is optimizable.
					 
					 (if (eq? val #t)
					     (if (and (not (eq? e #t))
						      (or (not (pair? e))
							  (not (eq? (hash-table-ref function-types (car e)) #t)))
						      (or (null? new-form)
							  (not (equal? e (car new-form)))))
						 (set! new-form (cons e new-form)))
					     (if (not val)             ; #f in and ends the expression
						 (begin
						   (if (or (null? new-form)   
							   (just-symbols? new-form))
						       (set! new-form '(#f))
						       (set! new-form (cons #f new-form))) ;was (append '(#f) new-form)))
						   (set! exprs '(#f)))
						 (if (and (pair? e)       ; if (and ...) splice into current
							  (eq? (car e) 'and))
						     (set! exprs (append e (cdr exprs)))
						     (if (not (and (pair? e)                   ; (and ... (or ... 123) ...) -> splice out or
								   (pair? (cdr exprs))
								   (eq? (car e) 'or)
								   (> (length e) 2)
								   (let ((last (list-ref e (- (length e) 1))))
								     (and last ; (or ... #f)
									  (code-constant? last)))))
							 (begin                 ; else add it to our new expression with value #t
							   (store e val 'and)
							   (if (or (not (pair? new-form))
								   (not (eq? val (car new-form))))
							       (set! new-form (cons val new-form))))))))))))))
		       )))))))
      
      
      (define (splice-if f lst)
	(cond ((null? lst) ())
	      ((pair? lst)
	       (if (and (pair? (car lst))
			(f (caar lst)))
		   (append (splice-if f (cdar lst)) (splice-if f (cdr lst)))
		   (cons (car lst) (splice-if f (cdr lst)))))
	      (#t lst)))
      
      
      (define (simplify-numerics form env)
	;; this returns a form, possibly the original simplified
	(let ((real-result? (lambda (op) (memq op '(imag-part real-part abs magnitude angle max min exact->inexact
							      modulo remainder quotient lcm gcd))))
	      (rational-result? (lambda (op) (memq op '(rationalize inexact->exact))))
	      (integer-result? (lambda (op) (memq op '(logior lognot logxor logand numerator denominator 
							      floor round truncate ceiling ash)))))
	  
	  (define (inverse-op op)
	    (case op 
	      ((sin) 'asin) ((cos) 'acos) ((tan) 'atan) ((asin) 'sin) ((acos) 'cos) ((atan) 'tan)
	      ((sinh) 'asinh) ((cosh) 'acosh) ((tanh) 'atanh) ((asinh) 'sinh) ((acosh) 'cosh) ((atanh) 'tanh)
	      ((log) exp) ((exp) log)))
	  
	  
	  (define (remove-duplicates lst)
	    (letrec ((rem-dup
		      (lambda (lst nlst)
			(cond ((null? lst) nlst)
			      ((and (member (car lst) nlst)
				    (or (not (pair? (car lst)))
					(not (eq? (caar lst) 'random)))) ; this problem applies to anything that calls random, mus-random etc
			       (rem-dup (cdr lst) nlst))
			      (else (rem-dup (cdr lst) (cons (car lst) nlst)))))))
	      (reverse (rem-dup lst ()))))
	  
	  (define (just-rationals? form)
	    (or (null? form)
		(rational? form)
		(and (pair? form)
		     (rational? (car form))
		     (just-rationals? (cdr form)))))
	  
	  (define (just-reals? form)
	    (or (null? form)
		(real? form)
		(and (pair? form)
		     (real? (car form))
		     (just-reals? (cdr form)))))
	  
	  (define (just-integers? form)
	    (or (null? form)
		(integer? form)
		(and (pair? form)
		     (integer? (car form))
		     (just-integers? (cdr form)))))
	  
	  (define (simplify-arg x)
	    (if (or (not (pair? x))                      ; constants and the like look dumb if simplified
		    (hash-table-ref globals (car x))
		    (not (hash-table-ref no-side-effect-functions (car x)))
		    (var-member (car x) env))
		x
		(let ((f (simplify-numerics x env)))
		  (if (and (pair? f)
			   (just-rationals? f))
		      (catch #t
			(lambda ()
			  (eval f))
			(lambda ignore f))
		      f))))
	  
	  (let* ((args (map simplify-arg (cdr form)))
		 (len (length args)))

	    (case (car form)
	      ((+)
	       (case len
		 ((0) 0)
		 ((1) (car args))
		 (else 
		  (let ((val (remove-all 0 (splice-if (lambda (x) (eq? x '+)) args))))
		    (if (every? (lambda (x) (or (not (number? x)) (rational? x))) val)
			(let ((rats (collect-if list rational? val)))
			  (if (> (length rats) 1)
			      (let ((y (apply + rats)))
				(if (zero? y)
				  (set! val (collect-if list (lambda (x) (not (number? x))) val))
				  (set! val (cons y (collect-if list (lambda (x) (not (number? x))) val))))))))
		    (case (length val)
		      ((0) 0)
		      ((1) (car val))                     ; (+ x) -> x
		      (else 
		       (if (every? (lambda (arg)      ; (+ (log x) (log y)) -> (log (* x y))
				     (and (pair? arg)
					  (pair? (cdr arg))
					  (null? (cddr arg))
					  (eq? (car arg) 'log)))
				   val)
			   `(log (* ,@(map cadr val)))
			   `(+ ,@val))))))))
	      
	      ((*)
	       (case len
		 ((0) 1)
		 ((1) (car args))
		 (else 
		  (let ((val (remove-all 1 (splice-if (lambda (x) (eq? x '*)) args))))
		    (if (every? (lambda (x) (or (not (number? x)) (rational? x))) val)
			(let ((rats (collect-if list rational? val)))
			  (if (> (length rats) 1)
			      (let ((y (apply * rats)))
				(if (= y 1)
				  (set! val (collect-if list (lambda (x) (not (number? x))) val))
				  (set! val (cons y (collect-if list (lambda (x) (not (number? x))) val))))))))
		    (case (length val)
		      ((0) 1)
		      ((1) (car val))                     ; (* x) -> x
		      (else 
		       (if (just-rationals? val)
			   (apply * val)
			   (if (member 0 val)             ; (* x 0 2) -> 0
			       0 
			       (if (and (= (length val) 2)
					(member -1 val))
				   `(- ,@(remove -1 val)) ; (* -1 x) -> (- x)
				   `(* ,@val))))))))))

	      ((-)
	       (case len
		 ((0) form)
		 ((1) ; negate
		  (if (number? (car args))
		      (- (car args))
		      (if (not (list? (car args)))
			  `(- ,@args)
			  (case (length (car args))
			    ((2) (if (eq? (caar args) '-)
				     (cadar args)          ; (- (- x)) -> x
				     `(- ,@args)))
			    ((3) (if (eq? (caar args) '-)
				     `(- ,(caddr (car args)) ,(cadr (car args))) ; (- (- x y)) -> (- y x)
				     `(- ,@args)))
			    (else `(- ,@args))))))
		 ((2) 
		  (if (just-rationals? args)
		      (apply - args)
		      (if (equal? (car args) 0)
			  `(- ,(cadr args))                ; (- 0 x) -> (- x)
			  (if (equal? (cadr args) 0)
			      (car args)                   ; (- x 0) -> x
			      (if (equal? (car args) (cadr args))
				  0                        ; (- x x)
				  (if (and (pair? (car args))
					   (eq? (caar args) '-)
					   (> (length (car args)) 2))
				      `(- ,@(cdar args) ,(cadr args)) ; (- (- x y) z) -> (- x y z) but leave (- (- x) ...)
				      `(- ,@args)))))))
		 (else 
		  (let ((val (remove-all 0 (splice-if (lambda (x) (eq? x '+)) (cdr args)))))
		    (if (every? (lambda (x) (or (not (number? x)) (rational? x))) val)
			(let ((rats (collect-if list rational? val)))
			  (if (> (length rats) 1)
			      (let ((y (apply + rats)))
				(if (zero? y)
				    (set! val (collect-if list (lambda (x) (not (number? x))) val))
				    (set! val (cons y (collect-if list (lambda (x) (not (number? x))) val))))))))
		    (set! val (cons (car args) val))
		    (let ((first-arg (car args))
			  (nargs (cdr val)))
			(if (member first-arg nargs)
			    (begin
			      (set! nargs (remove first-arg nargs)) ; remove once
			      (set! first-arg 0)))
			(if (null? nargs)
			    first-arg                     ; (- x 0 0 0)?
			    (if (and (equal? first-arg 0)
				     (= (length nargs) 1))
				(if (number? (car nargs))
				    (- (car nargs))
				    `(- ,(car nargs)))    ; (- 0 0 0 x)?
				`(- ,@(cons first-arg nargs)))))))))
	      
	      ((/)
	       (case len
		 ((0) form)
		 ((1) ; invert
		  (if (number? (car args))
		      (if (zero? (car args))
			  `(/ ,(car args))
			  (/ (car args)))
		      (if (pair? (car args))
			  (if (and (= (length (car args)) 2)
				   (eq? (caar args) '/))
			      (cadar args)
			      `(/ ,@args))
			  `(/ ,@args))))
		 ((2)
		  (if (and (just-rationals? args)
			   (not (zero? (cadr args))))
		      (apply / args)                      ; including (/ 0 12) -> 0
		      (if (equal? (car args) 1)           ; (/ 1 x) -> (/ x)
			  `(/ ,(cadr args))
			  (if (and (equal? (car args) 0)
				   (not (equal? (cadr args) 0)))
			      0
			      (if (and (pair? (car args))     ; (/ (log x) (log y)) -> (log x y)
				       (= (length (car args)) 2)
				       (pair? (cadr args))
				       (= (length (cadr args)) 2)
				       (eq? (caar args) 'log)
				       (eq? (caadr args) 'log))
				  `(log ,(cadar args) ,(cadadr args))
				  (if (equal? (cadr args) 1)
				      (car args)             ; (/ x 1) -> x
				      (if (and (pair? (cadr args))
					       (= (length (cadr args)) 2)
					       (eq? '/ (caadr args)))
					  `(* ,(car args) ,(cadadr args))  ; (/ x (/ y)) -> (* x y)
					  `(/ ,@args))))))))
		 (else 
		  (if (and (just-rationals? args)
			   (not (member 0 (cdr args)))
			   (not (member 0.0 (cdr args))))
		      (apply / args)
		      (let ((nargs                      ; (/ x a (* b 1 c) d) -> (/ x a b c d) but not short cases
			     (remove-all 1 (splice-if (lambda (x) (eq? x '*)) (cdr args)))))
			(if (null? nargs) ; (/ x 1 1) -> x
			    (car args)
			    (if (equal? (car args) 0)
				0
				`(/ ,@(cons (car args) nargs)))))))))
	      
	      ((sin cos asin acos sinh cosh tanh asinh acosh atanh exp)
	       (if (= len 1)
		   (if (and (pair? (car args))
			    (= (length (car args)) 2)
			    (eq? (caar args) (inverse-op (car form))))
		       (cadar args)
		       (if (equal? (car args) 0)
			   (case (car form)
			     ((sin asin sinh asinh tanh atanh) 0)
			     ((exp cos cosh) 1)
			     (else `(,(car form) ,@args)))
			   (if (and (eq? (car form) 'cos)
				    (pair? (car args))
				    (eq? (caar args) '-)
				    (null? (cddar args)))
			       `(cos ,(cadar args))
			       (if (eq? (car args) 'pi)
				   (case (car form)
				     ((sin) 0.0)
				     ((cos) 1.0)
				     (else `(,(car form) ,@args)))
				   (if (equal? (car args) 0.0)
				       (apply (symbol->value (car form)) '(0.0))
				       (if (and (eq? (car form) 'exp) ; (exp (* a (log b))) -> (expt b a)
						(pair? (car args))
						(eq? (caar args) '*))
					   (let ((targ (cdar args)))
					     (if (= (length targ) 2)
						 (if (and (pair? (car targ))
							  (eq? (caar targ) 'log)
							  (pair? (cdar targ))
							  (null? (cddar targ)))
						     `(expt ,(cadar targ) ,(cadr targ))
						     (if (and (pair? (cadr targ))
							      (eq? (caadr targ) 'log) 
							      (pair? (cdr (cadr targ)))
							      (null? (cddr (cadr targ))))
							 `(expt ,(cadadr targ) ,(car targ))
							 `(,(car form) ,@args)))
						 `(,(car form) ,@args)))
				       `(,(car form) ,@args)))))))
		   `(,(car form) ,@args)))
	      
	      ((log)
	       (if (pair? args)
		   (if (equal? (car args) 1)
		       0
		       (if (and (= len 1)
				(pair? (car args))
				(= (length (car args)) 2)
				(eq? (caar args) 'exp))
			   (cadar args)
			   (if (and (= len 2)
				    (equal? (car args) (cadr args)))
			       (if (integer? (car args))
				   1
				   1.0)
			       `(log ,@args))))
		   form))
	      
	      ((sqrt)
	       (if (pair? args)
		   (if (and (rational? (car args))
			    (= (car args) (* (sqrt (car args)) (sqrt (car args)))))
		       (sqrt (car args))
		       (if (and (pair? (car args)) ; (sqrt (* x x)) -> x
				(eq? (caar args) '*)
				(pair? (cdar args))
				(pair? (cddar args))
				(null? (cdddar args))
				(equal? (cadar args) (caddar args)))
			   (cadar args)
			   `(sqrt ,@args)))
		   form))
	      
	      ((floor round ceiling truncate)
	       (if (= len 1)
		   (if (number? (car args))
		       (catch #t (lambda () 
				   (apply (symbol->value (car form)) args)) 
			      (lambda any 
				`(,(car form) ,@args)))
		       (if (and (pair? (car args))
				(integer-result? (caar args)))
			   (car args)
			   `(,(car form) ,@args)))
		   form))
	      
	      ((abs magnitude)
	       (if (= len 1)
		   (if (and (pair? (car args))
			    (memq (caar args) '(abs magnitude)))
		       (car args)
		       (if (rational? (car args))
			   (abs (car args))
			   (if (and (pair? (car args))   ; (abs (- x)) -> (abs x)
				    (eq? (caar args) '-)
				    (pair? (cdar args))
				    (null? (cddar args)))
			       `(,(car form) ,(cadar args))
			       `(,(car form) ,@args))))
		   form))
	      
	      ((imag-part)
	       (if (= len 1)
		   (if (or (and (number? (car args))
				(real? (car args)))
			   (and (pair? (car args))
				(real-result? (caar args))))
		       0.0
		       `(imag-part ,@args))
		   form))
	      
	      ((real-part)
	       (if (= len 1)
		   (if (or (real? (car args))
			   (and (pair? (car args))
				(real-result? (caar args))))
		       (car args)
		       `(real-part ,@args))
		   form))
	      
	      ((denominator)
	       (if (= len 1)
		   (if (or (integer? (car args))
			   (and (pair? (car args))
				(integer-result? (caar args))))
		       1
		       `(denominator ,(car args)))
		   form))

	      ((numerator)
	       (if (= len 1)
		   (if (or (integer? (car args))
			   (and (pair? (car args))
				(integer-result? (caar args))))
		       (car args)
		       (if (rational? (car args))
			   (numerator (car args))
			   `(numerator ,(car args))))
		   form))
	      
	      ((random)
	       (if (and (= len 1)
			(number? (car args)))
		   (if (and (integer? (car args))
			    (= (car args) 0))
		       0
		       (if (morally-equal? (car args) 0.0)
			   0.0
			   `(random ,@args)))
		   `(random ,@args)))
	      ;; what about (* 2.0 (random 1.0)) and the like?
	      ;;   this is trickier than it appears: (* 2.0 (random 3)) etc

	      ((rationalize make-polar make-rectangular lognot ash modulo remainder quotient tan exact->inexact)
	       (if (just-rationals? args)
		   (catch #t ; catch needed here for things like (ash 2 64)
		     (lambda ()
		       (apply (symbol->value (car form)) args))
		     (lambda ignore
		       `(,(car form) ,@args)))
		   (if (and (eq? (car form) 'make-rectangular)
			    (= len 2)
			    (morally-equal? (cadr args) 0.0))
		       (car args)
		       `(,(car form) ,@args))))
	      
	      ((expt) 
	       (if (= len 2)
		   (if (and (equal? (car args) 0)
			    (not (equal? (cadr args) 0)))
		       0
		       (if (and (equal? (cadr args) 0)
				(not (equal? (car args) 0)))
			   1
			   (if (equal? (car args) 1)
			       1
			       (if (equal? (cadr args) 1)
				   (car args)
				   (if (equal? (cadr args) -1)
				       `(/ ,(car args))
				       (if (just-rationals? args)
					   (catch #t
					     (lambda ()
					       (let ((val (apply expt args)))
						 (if (integer? val)
						     val
						     `(expt ,@args))))
					     (lambda args
					       `(expt ,@args)))
					   `(expt ,@args)))))))
		   form))

	      ((angle)
	       (if (pair? args)
		   (if (equal? (car args) -1)
		       'pi
		       (if (or (morally-equal? (car args) 0.0)
			       (eq? (car args) 'pi))
			   0.0
			   `(angle ,@args)))
		   form))

	      ((atan)
	       (if (and (= len 1)
			(pair? (car args))
			(= (length (car args)) 3)
			(eq? (caar args) '/))
		   `(atan ,@(cdar args))
		   `(atan ,@args)))
	      
	      ((inexact->exact)
	       (if (= len 1)
		   (if (or (rational? (car args))
			   (and (pair? (car args))
				(or (rational-result? (caar args))
				    (integer-result? (caar args)))))
		       (car args)
		       (if (number? (car args))
			   (catch #t (lambda () (inexact->exact (car args))) (lambda any `(inexact->exact ,@args)))
			   `(inexact->exact ,@args)))
		   form))
	      
	      ((logior)
	       (set! args (remove-duplicates (remove-all 0 (splice-if (lambda (x) (eq? x 'logior)) args))))
	       (if (every? (lambda (x) (or (not (number? x)) (integer? x))) args)
		   (let ((rats (collect-if list integer? args)))
		     (if (> (length rats) 1)
			 (let ((y (apply logior rats)))
			   (if (zero? y)
			       (set! args (collect-if list (lambda (x) (not (number? x))) args))
			       (set! args (cons y (collect-if list (lambda (x) (not (number? x))) args))))))))
	       (if (null? args)           ; (logior) -> 0
		   0
		   (if (null? (cdr args)) ; (logior x) -> x
		       (car args)
		       (if (member -1 args)
			   -1
			   (if (just-integers? args)
			       (apply logior args)
			       `(logior ,@args))))))
	      
	      ((logand)
	       (set! args (remove-duplicates (remove-all -1 (splice-if (lambda (x) (eq? x 'logand)) args))))
	       (if (every? (lambda (x) (or (not (number? x)) (integer? x))) args)
		   (let ((rats (collect-if list integer? args)))
		     (if (> (length rats) 1)
			 (let ((y (apply logand rats)))
			   (if (= y -1)
			       (set! args (collect-if list (lambda (x) (not (number? x))) args))
			       (set! args (cons y (collect-if list (lambda (x) (not (number? x))) args))))))))
	       (if (null? args)
		   -1
		   (if (null? (cdr args)) ; (logand x) -> x
		       (car args)
		       (if (member 0 args)
			   0
			   (if (just-integers? args)
			       (apply logand args)
			       `(logand ,@args))))))
	      ;; (logand 1 (logior 2 x)) -> (logand 1 x)? 
	      ;; (logand 1 (logior 1 x)) -> 1
	      ;; (logand 3 (logior 1 x))?
	      ;; similarly for (logior...(logand...))

	      ((logxor)
	       (set! args (splice-if (lambda (x) (eq? x 'logxor)) args)) ; is this correct??
	       (if (null? args)
		   0
		   (if (null? (cdr args)) ; (logxor x) -> x??
		       (car args)
		       (if (just-integers? args)
			   (apply logxor args)
			   (if (and (= len 2)
				    (equal? (car args) (cadr args)))
			       0
			       `(logxor ,@args))))))
		   
	      ((gcd)
	       (set! args (remove-duplicates (splice-if (lambda (x) (eq? x 'gcd)) args)))
	       (if (null? args)
		   0
		   ;; here and in lcm, if just 1 arg -> (abs arg) 
		   (if (member 1 args)
		       1
		       (if (just-integers? args)
			   (catch #t  ; maybe (gcd -9223372036854775808 -9223372036854775808)
			     (lambda ()
			       (apply gcd args))
			     (lambda ignore
			       `(gcd ,@args)))
			   (if (null? (cdr args))
			       `(abs ,(car args))
			       `(gcd ,@args))))))

	      ;; (gcd x 0) -> (abs x)
	      ;; (* (gcd a b) (lcm a b)) -> (abs (* a b))
	      
	      ((lcm)
	       (set! args (remove-duplicates (splice-if (lambda (x) (eq? x 'lcm)) args)))
	       (if (null? args)
		   1
		   (if (member 0 args)
		       0
		       (if (just-integers? args)
			   (catch #t
			     (lambda ()
			       (apply lcm args))
			     (lambda ignore
			       `(lcm ,@args)))
			   (if (null? (cdr args))
			       `(abs ,(car args))
			       `(lcm ,@args))))))
	      
	      ((max min)
	       (if (pair? args)
		   (begin
		     (set! args (remove-duplicates (splice-if (lambda (x) (eq? x (car form))) args)))
		     (if (= len 1)
			 (car args)
			 (if (just-reals? args)
			     (apply (symbol->value (car form)) args)
			     (let ((nums (collect-if list number? args))
				   (other (if (eq? (car form) 'min) 'max 'min)))
			       (if (and (pair? nums)
					(just-reals? nums)) ; non-real case checked elsewhere (later)
				   (let ((relop (if (eq? (car form) 'min) >= <=)))
				     (if (> (length nums) 1)
					 (set! nums (list (apply (symbol->value (car form)) nums))))
				     (let ((new-args (append nums (collect-if list (lambda (x) (not (number? x))) args))))
				       (let ((c1 (car nums)))
					 (set! new-args (collect-if list (lambda (x)
									   (or (not (pair? x))
									       (<= (length x) 2)
									       (not (eq? (car x) other))
									       (let ((c2 (find-if number? (cdr x))))
										 (or (not c2)
										     (relop c1 c2)))))
								    new-args)))
				       (if (< (length new-args) (length args))
					   (set! args new-args)))))
				     ;; if (max c1 (min c2 . args1) . args2) where (>= c1 c2) -> (max c1 . args2)
				     ;; if (min c1 (max c2 . args1) . args2) where (<= c1 c2) -> (min c1 . args2)
			       
			       ;; there are more such cases: (max x (min x 3)) -> x and (min x (max x c)) -> x
			       ;; (max a b) is (- (min (- a) (- b))), but that doesn't help here -- the "-" gets in our way
			       (if (null? (cdr args)) ; (max (min x 3) (min x 3)) -> (max (min x 3)) -> (min x 3)
				   (car args)
				   (if (and (null? (cddr args))   ; (max|min x (min|max x ...) -> x
					    (or (and (pair? (car args))
						     (eq? (caar args) other)
						     (symbol? (cadr args))   ; actually this is probably not needed, but I want to avoid (random ...)
						     ;; perhaps instead use not min/max 
						     (member (cadr args) (car args)))
						(and (pair? (cadr args))
						     (eq? (caadr args) other)
						     (symbol? (car args))
						     (member (car args) (cadr args)))))
				       ((if (pair? (car args)) cadr car) args)
				       `(,(car form) ,@args)))))))
		   form))
	      
	      (else `(,(car form) ,@args))))))
      
      
      (define (check-special-cases name head form env)
	(case head
	  ((load) ; pick up the top level declarations
	   (if (>= (length form) 2)
	       (scan form))
	   env)

	  ((=)
	   (if (and *report-minor-stuff*
		    (> (length form) 2)
		    (any-real? (cdr form)))
	       (lint-format "= can be troublesome with floats:~A" name (truncated-list->string form)))
	   (let ((cleared-form (cons = (remove-if (lambda (x) (not (number? x))) (cdr form)))))
	     (if (and (> (length cleared-form) 2)
		      (not (checked-eval cleared-form)))
		 (lint-format "this comparison can't be true:~A" name (truncated-list->string form)))))
	  
	  ((memq assq)
	   (if (and (= (length form) 3)
		    (or (and (number? (cadr form))
			     (not (integer? (cadr form))))
			(string? (cadr form))
			(vector? (cadr form))
			(and (pair? (cadr form))
			     (memq (caadr form) '(list vector string hash-table cons lambda lambda* string-append append reverse)))))
	       (lint-format "this is always #f:~A" name (truncated-list->string form))))
	  
	  ((memv assv)
	   (if (and (= (length form) 3)
		    (or (string? (cadr form))
			(vector? (cadr form))))
	       (lint-format "this is problematic -- perhaps use ~A instead:~A"
			    name 
			    (if (eq? head 'memv) 'member 'assoc)
			    (truncated-list->string form))))
	  
	  ((member)
	   (if (= (length form) 4)
	       (let ((func (list-ref form 3)))
		 (if (and (pair? func)
			  (= (length func) 3)
			  (eq? (car func) 'lambda)
			  (pair? (cadr func))
			  (pair? (caddr func)))
		     (if (not (member (length (cadr func)) '(2 -1)))
			 (lint-format "member equality function (optional 3rd arg) should take two arguments" name)
			 (let ((eq (caddr func))
			       (args (cadr func)))
			   (if (and (memq (car eq) '(eq? eqv? equal?))
				    (eq? (car args) (cadr eq))
				    (pair? (caddr eq))
				    (eq? (car (caddr eq)) 'car)
				    (pair? (cdr (caddr eq)))
				    (pair? (cdr args))
				    (eq? (cadr args) (cadr (caddr eq))))
			       (lint-format "member might perhaps be ~A"
					    name
					    (if (eq? func 'eq?) 'assq
						(if (eq? (car (caddr func)) 'eq?) 'assq
						    (if (eq? (car (caddr func)) 'eqv?) 'assv 'assoc)))))))))))
	  
	  ((if)
	   (let ((len (length form)))
	     (if (> len 4)
		 (lint-format "if has too many clauses: ~S" name form)
		 (if (< len 3)
		     (lint-format "if has too few clauses: ~S" name form)
		     (let ((test (cadr form))
			   (true (caddr form))
			   (false (if (= len 4) (cadddr form) 'no-false)))

		       (if (never-false test)
			   (lint-format "if test is never false: ~A" name form)
			   (if (never-true test)
			       (if (equal? form '(if #f #f))
				   (lint-format "~A can be replaced with #<unspecified>" name form)
				   (lint-format "if test is never true: ~A" name form))))

		       (if *report-minor-stuff*
			   (let ((expr (simplify-boolean test () () env)))

			     (if (and (eq? false 'no-false) ; (if (pair? lst) (for-each f lst)) -> (for-each f lst)
				      (pair? test)
				      (eq? (car test) 'pair?)
				      (pair? true)
				      (memq (car true) '(map for-each))
				      (eq? (cadr test) (caddr true)))
				 (lint-format "possible simplification:~A" name (lists->string form true)))

			     (if (and (eq? false 'no-false) ; (if test0 (if test1 expr)) -> (if (and test0 test1) expr) (else #<unspecified>)
				      (pair? true)
				      (eq? (car true) 'if)
				      (null? (cdddr true)))
				 (let ((test1 (simplify-boolean `(and ,test ,(cadr true)) () () env)))
				   (lint-format "possible simplification:~A" name (lists->string form `(if ,test1 ,(caddr true))))))
			     
			     (if (and (pair? false) ; (if test0 expr (if test1 expr)) -> if (or test0 test1) expr) 
				      (eq? (car false) 'if)
				      (equal? true (caddr false))
				      (null? (cdddr false)))
				 (let ((test1 (simplify-boolean `(or ,test ,(cadr false)) () () env)))
				   (lint-format "possible simplification:~A" name (lists->string form `(if ,test1 ,true)))))

			     (if (and (not (eq? false 'no-false))        ; (if expr (set! var #t|#f) (set! var #f|#t)) -> (set! var expr|(not expr))??
				      (pair? true)
				      (pair? false)
				      (eq? (car true) 'set!)
				      (eq? (car false) 'set!)
				      (eq? (cadr true) (cadr false))
				      (boolean? (caddr true))
				      (boolean? (caddr false))
				      (not (eq? (caddr true) (caddr false))))
				 (lint-format "possible simplification:~A"
					      name
					      (lists->string form 
							     (if (caddr true)
								 `(set! ,(cadr true) ,test)
								 `(set! ,(cadr true) (not ,test))))))
			     (if (eq? expr #t)
				 (lint-format "possible simplification:~A" name (lists->string form true))
				 (if (not expr)
				     (if (eq? false 'no-false)
					 (if true                             ; (if #f #f) as a kludgey #<unspecified>
					     (lint-format "~S is never #t:~A" name test (truncated-list->string form)))
					 (lint-format "possible simplification:~A" name (lists->string form false)))
				     (if (and (boolean? true)
					      (boolean? false)
					      (not (eq? true false))) ; !  (if expr #t #f) turned into something less verbose
					 (lint-format "possible simplification:~A" name (lists->string form (if true expr `(not ,expr))))
					 (if (and (= len 4)
						  (equal? true false))
					     (lint-format "if is not needed here:~A" name (truncated-list->string form)))))))))))))

	  ((car cdr)
	   (if (and *report-minor-stuff*
		    (pair? (cadr form))
		    (eq? (car (cadr form)) 'cons))
	       (lint-format "(~A~A) is the same as~A"
			    name head
			    (truncated-list->string (cadr form))
			    (if (eq? head 'car)
				(truncated-list->string (cadr (cadr form)))
				(truncated-list->string (caddr (cadr form)))))))
	  
	  ((and or not)
	   (if (and *report-minor-stuff*
		    (not (= line-number last-simplify-boolean-line-number)))
	       (let ((val (simplify-boolean form () () env)))
		 (set! last-simplify-boolean-line-number line-number)
		 (if (not (equal? form val))
		     (lint-format "possible simplification:~A" name (lists->string form val))))))

	  ((< > <= >=) ; '= handled above
	   (let ((cleared-form (cons (car form) ; keep operator
				     (remove-if (lambda (x) 
						  (not (number? x))) 
						(cdr form)))))
	     (if (and (> (length cleared-form) 2)
		      (not (checked-eval cleared-form)))
		 (lint-format "this comparison can't be true:~A" name (truncated-list->string form)))))
	     
	  ((char<? char>? char<=? char>=? char=? 
	    char-ci<? char-ci>? char-ci<=? char-ci>=? char-ci=?)
	   (let ((cleared-form (cons (car form) ; keep operator
				     (remove-if (lambda (x) 
						  (not (char? x))) 
						(cdr form)))))
	     (if (and (> (length cleared-form) 2)
		      (not (checked-eval cleared-form)))
		 (lint-format "this comparison can't be true:~A" name (truncated-list->string form)))))
	     
	  ((string<? string>? string<=? string>=? string=? 
	    string-ci<? string-ci>? string-ci<=? string-ci>=? string-ci=?)
	   (let ((cleared-form (cons (car form) ; keep operator
				     (remove-if (lambda (x) 
						  (not (string? x))) 
						(cdr form)))))
	     (if (and (> (length cleared-form) 2)
		      (not (checked-eval cleared-form)))
		 (lint-format "this comparison can't be true:~A" name (truncated-list->string form)))))
	     
#|
	  ;; this check isn't correct unfortunately
	  ((call/cc call-with-current-continuation)
	   (let ((continuation (and (pair? (cdr form))
				    (pair? (cadr form))
				    (eq? (caadr form) 'lambda)
				    (pair? (cdadr form))
				    (pair? (cadadr form))
				    (car (cadadr form)))))
	     (if (symbol? continuation)
		 (let ((body (cddadr form)))
		   (if (not (eq? continuation (car body)))
		       (if (not (tree-member continuation body))
			   (lint-format "~A is not needed:~A" name head (truncated-list->string form))))))))
|#	  
	  ((/)
	   (if (pair? (cdr form))
	       (if (and (null? (cddr form))
			(number? (cadr form))
			(zero? (cadr form)))
		   (lint-format "attempt to invert zero:~A" name (truncated-list->string form))
		   (if (and (pair? (cddr form))
			    (member 0 (cddr form)))
		       (lint-format "attempt to divide by 0:~A" name (truncated-list->string form))))))
	  
	  ((copy)
	   (if (and (pair? (cdr form))
		    (or (number? (cadr form))
			(boolean? (cadr form))
			(char? (cadr form))
			(and (pair? (cadr form))
			     (or (eq? (caadr form) 'copy)
				 (eq? (caadr form) 'string-copy)))))
	       (lint-format "~A could be ~A" name form (cadr form))
	       (if (and (pair? (cdr form)) (equal? (cadr form) '(owlet)))
		   (lint-format "~A could be (owlet): owlet is copied internally" name form))))
	  
	  ((string-copy)
	   (if (and (pair? (cdr form))
		    (pair? (cadr form))
		    (or (eq? (caadr form) 'copy)
			(eq? (caadr form) 'string-copy)))
	       (lint-format "~A could be ~A" name form (cadr form))))

	  ((string)
	   (if (every? (lambda (x) (and (char? x) (not (member x '(#\null #\newline #\escape #\linefeed))))) (cdr form))
	       (lint-format "~A could be ~S" name form (apply string (cdr form)))))

	  ((string-append)
	   ;; also (string-append . constants)? and (string-append)->""
	   (if (not (= line-number last-simplify-boolean-line-number))
	       (let ((args (remove-all "" (splice-if (lambda (x) (eq? x 'string-append)) (cdr form)))))
		 (if (null? args)
		     (lint-format "this is pointless:~A" name (truncated-list->string form))
		     (if (null? (cdr args))
			 (lint-format "possible simplification:~A" name (lists->string form (car args)))
			 (if (< (length args) (length (cdr form)))
			     (lint-format "possible simplification:~A" name (lists->string form `(string-append ,@args))))))
		 (set! last-simplify-boolean-line-number line-number))))

	  ((vector-set! list-set! hash-table-set!)
	   (if (= (length form) 4)
	       (let ((target (cadr form))
		     (index (caddr form))
		     (val (cadddr form)))
		 (if (and (pair? val)
			  (= (length val) 3)
			  (eq? target (cadr val))
			  (memq (car val) '(vector-ref list-ref hash-table-ref)))
		     (let ((ref (car val))
			   (ind (caddr val)))
		       (if (equal? ind index)
			   (lint-format "redundant?:~A" name (truncated-list->string form)))
		       (case head
			 ((vector-set!) (if (not (eq? ref 'vector-ref)) (lint-format "type mismatch?:~A" name (truncated-list->string form))))
			 ((list-set!) (if (not (eq? ref 'list-ref)) (lint-format "type mismatch?:~A" name (truncated-list->string form))))
			 ((hash-table-set!) (if (not (eq? ref 'hash-table-ref)) (lint-format "type mismatch?:~A" name (truncated-list->string form))))))))))
	  
	  ((object->string)
	   (if (pair? (cdr form))
	       (if (and (pair? (cadr form))
			(eq? (caadr form) 'object->string))
		   (lint-format "~A could be ~A" name form (cadr form))
		   (if (and (pair? (cddr form))
			    (not (pair? (caddr form)))
			    (or (not (symbol? (caddr form))) (keyword? (caddr form)))
			    (not (memq (caddr form) '(#f #t :readable))))
		       (lint-format "bad second argument: ~A" name (caddr form))))))

	  ((display)
	   (if (and (= (length form) 2)
		    (pair? (cadr form))
		    (eq? (car (cadr form)) 'format)
		    (not (cadr (cadr form))))
	       (lint-format "~A could be ~A" name form `(format #t ,@(cddr (cadr form))))))

	  ((make-vector)
	   (if (and (= (length form) 4)
		    (code-constant? (caddr form))
		    (not (real? (caddr form)))
		    (eq? (cadddr form) #t))
	       (lint-format "~A won't create an homogenous vector" name form)))

	  ((reverse list->vector vector->list list->string string->list symbol->string string->symbol number->string)
	   (let ((inverses '((reverse . reverse) 
			     (list->vector . vector->list)
			     (vector->list . list->vector)
			     (symbol->string . string->symbol)
			     (string->symbol . symbol->string)
			     (list->string . string->list)
			     (string->list . list->string)
			     (number->string . string->number))))
	     (if (and (pair? (cdr form))
		      (pair? (cadr form))
		      (pair? (cdadr form))
		      (eq? (caadr form) (let ((p (assq head inverses))) (and (pair? p) (cdr p)))))
		 (lint-format "~A could be (copy ~A)" name form (cadadr form)))))
	  
	  ((char->integer integer->char symbol->keyword keyword->symbol string->number)
	   (let ((inverses '((char->integer . integer->char)
			     (integer->char . char->integer)
			     (symbol->keyword . keyword->symbol)
			     (keyword->symbol . symbol->keyword)
			     (string->number . number->string))))
	     (if (and (pair? (cdr form))
		      (pair? (cadr form))
		      (pair? (cdadr form))
		      (eq? (caadr form) (let ((p (assq head inverses))) (and (pair? p) (cdr p)))))
		 (lint-format "~A could be ~A" name form (cadadr form)))))
	  
	  ((append)
	   ;; also (append . constants)? and (append)->()
	   (if (= (length form) 2)
	       (lint-format "~A could be ~A" name form (cadr form))))

	  ((apply) 
	   (if (and (pair? (cdr form))
		    (not (symbol? (cadr form)))
		    (not (applicable? (cadr form))))
	       (lint-format "~S is not applicable: ~A" name (cadr form) (truncated-list->string form))
	       (let ((len (length form)))
		 (if (and (> len 2)
			  (not (list? (form (- len 1))))
			  (code-constant? (form (- len 1))))
		     (lint-format "last argument should be a list: ~A" name (truncated-list->string form))))))

	  ((format snd-display)
	   (if (< (length form) 3)
	       (begin
		 (if (< (length form) 2)
		     (lint-format "~A has too few arguments:~A" name head (truncated-list->string form))
		     (if (and (pair? (cadr form))
			      (eq? (caadr form) 'format))
			 (lint-format "redundant format: ~A" name (truncated-list->string form))
			 (if (and (code-constant? (cadr form))
				  (not (string? (cadr form))))
			     (lint-format "format with one argument takes a string: ~A" name (truncated-list->string form)))))
		 env)
	       (let ((control-string (if (string? (cadr form)) (cadr form) (caddr form)))
		     (args (if (string? (cadr form)) (cddr form) (cdddr form))))
		 
		 (define (count-directives str name form)
		   (let ((curlys 0)
			 (dirs 0)
			 (pos (char-position #\~ str)))
		     (if pos
			 (let ((len (length str))
			       (tilde-time #t)) 
			   (do ((i (+ pos 1) (+ i 1)))
			       ((>= i len))
			     (let ((c (string-ref str i)))
			       (if tilde-time
				   (begin
				     (if (and (= curlys 0)
					      (not (memq c '(#\~ #\T #\t #\& #\% #\^ #\| #\newline #\}))) ; ~* consumes an arg
					      (not (call-with-exit
						    (lambda (return)
						      (do ((k i (+ k 1)))
							  ((= k len) #f)
							;; this can be confused by pad chars in ~T
							(if (and (not (char-numeric? (string-ref str k)))
								 (not (char=? (string-ref str k) #\,)))
							    (return (char-ci=? (string-ref str k) #\t))))))))
					 (begin
					   ;; the possibilities are endless, so I'll stick to the simplest
					   (if (not (vector-ref format-control-char (char->integer c)))
					       (lint-format "unrecognized format directive: ~C in ~S, ~S" name c str form))
					   (set! dirs (+ dirs 1))
					   
					   ;; ~n so try to figure out how many args are needed (this is not complete)
					   (when (char-ci=? c #\n)
					     (let ((j (+ i 1)))
					       (if (>= j len)
						   (lint-format "missing format directive: ~S" name str)
						   (begin
						     ;; if ,n -- add another, if then not T, add another
						     (if (char=? (string-ref str j) #\,)
							 (if (>= (+ j 1) len)
							     (lint-format "missing format directive: ~S" name str)
							     (begin
							       (if (char-ci=? (string-ref str (+ j 1)) #\n)
								   (begin
								     (set! dirs (+ dirs 1))
								     (set! j (+ j 2)))
								   (if (char-numeric? (string-ref str (+ j 1)))
								       (set! j (+ j 2))
								       (set! j ( + j 1)))))))
						     (if (>= j len)
							 (lint-format "missing format directive: ~S" name str)
							 (if (not (char-ci=? (string-ref str j) #\t))
							     (set! dirs (+ dirs 1))))))))))
				     
				     (set! tilde-time #f)
				     (case c 
				       ((#\{) (set! curlys (+ curlys 1)))
				       ((#\}) (set! curlys (- curlys 1)))
				       ((#\^ #\|)
					(if (zero? curlys)
					    (lint-format "~A has ~C outside ~~{~~}?" name str c)))))
				   (begin
				     (set! pos (char-position #\~ str i))
				     (if pos 
					 (begin
					   (set! tilde-time #t)
					   (set! i pos))
					 (set! i len))))))
			   
			   (if tilde-time
			       (lint-format "~A control string ends in tilde:~A" name head (truncated-list->string form)))))
		     
		     (if (not (= curlys 0))
			 (lint-format "~A has ~D unmatched ~A~A:~A"
				      name head 
				      (abs curlys) 
				      (if (positive? curlys) "{" "}") 
				      (if (> curlys 1) "s" "") 
				      (truncated-list->string form)))
		     dirs))
		 
		 (if (not (string? control-string))
		     (if (not (list? args))
			 (lint-format "~S looks suspicious" name form))
		     (let ((ndirs (count-directives control-string name form))
			   (nargs (if (or (null? args) (pair? args)) (length args) 0)))
		       (if (not (= ndirs nargs))
			   (lint-format "~A has ~A arguments:~A" 
					name head 
					(if (> ndirs nargs) "too few" "too many")
					(truncated-list->string form))
			   (if (and (not (cadr form))
				    (zero? ndirs)
				    (not (char-position #\~ control-string)))
			       (lint-format "~A could be ~S, (format is a no-op here)" name form (caddr form))))))
		 
					;(lint-walk name (cdr form) env)
		 )))

	  ((sort!)
	   (if (and (= (length form) 3)
		    (memq (caddr form) '(= <= >= eq? eqv? equal?
					   string=? string<=? string>=? char=? char<=? char>=?
					   string-ci=? string-ci<=? string-ci>=? char-ci=? char-ci<=? char-ci>=?)))
	       (lint-format "sort! with ~A may hang:~A" name (caddr form) (truncated-list->string form))))

	  ((substring) ; and others? -- but this sort of thing never happens in real code
	   (if (every? code-constant? (cdr form))
	       (catch #t
		 (lambda ()
		   (let ((val (eval form)))
		     (lint-format "possible simplification:~S" name val)))
		 (lambda (type info)
		   (lint-format "~A -> ~A~%" name form (apply format #f info))))))
	  ))

      
      (define (check-call name head form env)
	(let ((fdata (or (var-member head env) (hash-table-ref globals head))))
	  ;(format *stderr* "call: ~A~%" fdata)
	  (if (var? fdata)
	      ;; a local var
	      (begin
		(if (pair? (var-func-info fdata))
		    (let ((type (car (var-func-info fdata)))
			  (args (cadr (var-func-info fdata))))
		      
		      (let ((rst (or (not (pair? args))
				     (not (list? args))
				     (memq :rest args)))
			    (pargs (if (pair? args) (proper-list args) ())))
			
			(let ((call-args (length (cdr form)))
			      (decl-args (max 0 (- (length pargs) (keywords pargs) (if rst 1 0)))))
			  
			  (let ((req (if (memq type '(define lambda)) decl-args 0))
				(opt (if (memq type '(define lambda)) 0 decl-args)))
			    (if (< call-args req)
				(lint-format "~A needs ~D argument~A:~A" 
					     name head 
					     req (if (> req 1) "s" "") 
					     (truncated-list->string form))
				(if (and (not rst)
					 (> (- call-args (keywords (cdr form))) (+ req opt))
					 (symbol? head))
				    (lint-format "~A has too many arguments:~A" name head (truncated-list->string form))))
			    (if (and (memq type '(define* lambda*))
				     (not (memq :allow-other-keys pargs))
				     (not (memq :rest pargs)))
				(for-each
				 (lambda (arg)
				   (if (and (keyword? arg)
					    (not (memq arg '(:rest :key :optional)))
					    (not (member (keyword->symbol arg) pargs 
							 (lambda (a b)
							   (if (pair? b) 
							       (eq? a (car b))
							       (eq? a b))))))
				       (lint-format "~A keyword argument ~A (in ~S) does not match any argument in ~S" name head arg form pargs)))
				 (cdr form)))))))))
	      ;; not local var
	      (if (symbol? head)
		  (let ((head-value (symbol->value head))) ; head might be "arity"!
		    (if (or (procedure? head-value)
			    (macro? head-value))
			;; check arg number
			(let* ((args (length (cdr form)))
			       (ary (arity head-value))
			       (min-arity (car ary))
			       (max-arity (cdr ary)))
			  (if (< args min-arity)
			      (lint-format "~A needs ~A~D argument~A:~A" 
					   name head 
					   (if (= min-arity max-arity) "" "at least ")
					   min-arity
					   (if (> min-arity 1) "s" "") 
					   (truncated-list->string form))
			      (if (and (not (procedure-setter head-value))
				       (> (- args (keywords (cdr form))) max-arity))
				  (lint-format "~A has too many arguments:~A" name head (truncated-list->string form))))
			  (if (and (procedure? head-value)
				   (pair? (cdr form))) ; there are args (the not-enough-args case is checked above)
			      (if (zero? max-arity)
				  (lint-format "too many arguments:~A" name (truncated-list->string form))    
				  (begin
				    ;; if keywords, check that they are acceptable
				    ;;    this only applies to lambda*'s that have been previously loaded (lint doesn't create them)
				    (let ((source (procedure-source head-value)))
				      (if (and (pair? source)
					       (eq? (car source) 'lambda*))
					  (let ((decls (cadr source)))
					    (if (not (memq :allow-other-keys decls))
						(for-each
						 (lambda (arg)
						   (if (and (keyword? arg)
							    (not (memq arg '(:rest :key :optional)))
							    (not (member arg decls 
									 (lambda (a b) 
									   (if (pair? b) 
									       (eq? (keyword->symbol a) (car b))
									       (eq? (keyword->symbol a) b))))))
						       (lint-format "~A keyword argument ~A (in ~S) does not match any argument in ~S" name head arg form decls)))
						 (cdr form))))))
				    
				    (case head
				      ((eq?) 
				       (if (< (length form) 3)
					   (lint-format "eq? needs 2 arguments:~A" name (truncated-list->string form))
					   (begin
					     (if (or (and (number? (cadr form))
							  (rational? (cadr form)))
						     (char? (cadr form))
						     (and (number? (caddr form))
							  (rational? (caddr form)))
						     (char? (caddr form)))
						 (lint-format "eq? doesn't work reliably with args like ~S" name form)
						 (if (or (and (number? (cadr form))
							      (not (rational? (cadr form))))
							 (string? (cadr form))
							 (vector? (cadr form))
							 (and (number? (caddr form))
							      (not (rational? (caddr form))))
							 (string? (caddr form))
							 (vector? (caddr form)))
						     (lint-format "~A is always #f" name form)))
					     
					     (if *report-minor-stuff*  ; (eq? e #f) or (eq? #f e) -> (not e); can't simplify #t as well here
						 (let ((expr 'unset))
						   (if (not (cadr form))
						       (set! expr (simplify-boolean `(not ,(caddr form)) () () env))
						       (if (not (caddr form))
							   (set! expr (simplify-boolean `(not ,(cadr form)) () () env))))
						   (if (not (eq? expr 'unset))
						       (lint-format "possible simplification:~A" name (lists->string form expr)))))))
				       
				       (check-for-repeated-args name head form env)
				       (check-for-repeated-args-with-not name form env))
				      
				      ((eqv?) 
				       (if (< (length form) 3)
					   (lint-format "eqv? needs 2 arguments:~A" name (truncated-list->string form))
					   (if (or (vector? (cadr form))
						   (string? (cadr form))
						   (vector? (caddr form))
						   (string? (caddr form)))
					       (lint-format "eqv? doesn't work reliably with args like ~S" name form)))
				       
				       (check-for-repeated-args name head form env)
				       (check-for-repeated-args-with-not name form env))
				      
				      ((map for-each)
				       (let* ((len (length form))
					      (args (- len 2)))
					 (if (< len 3)
					     (lint-format "~A missing argument~A in:~A"
							  name head 
							  (if (= len 2) "" "s") 
							  (truncated-list->string form)))
					 (let ((func (cadr form))
					       (ary #f))
					   (if (and (symbol? func)
						    (defined? func)
						    (procedure? (symbol->value func)))
					       (set! ary (arity (symbol->value func)))
					       
					       (if (and (pair? func)
							(memq (car func) '(lambda lambda*))
							(pair? (cadr func)))
						   (let ((arglen (length (cadr func))))
						     (if (eq? (car func) 'lambda)
							 (if (negative? arglen)
							     (set! ary (cons (abs arglen) 512000))
							     (set! ary (cons arglen arglen)))
							 (if (or (negative? arglen)
								 (memq :rest (cadr func)))
							     (set! ary (cons 0 512000))
							     (set! ary (cons 0 arglen)))))))
					   
					   (if (pair? ary)
					       (if (< args (car ary))
						   (lint-format "~A has too few arguments in: ~A"
								name head 
								(truncated-list->string form))
						   (if (> args (cdr ary))
						       (lint-format "~A has too many arguments in: ~A"
								    name head 
								    (truncated-list->string form)))))
					   
					   (if (and *report-minor-stuff*
						    (= len 3)
						    (pair? func)
						    (eq? (car func) 'lambda))
					       (let ((farg (cadr func))
						     (body (cddr func)))
						 (if (and (pair? farg)
							  (null? (cdr farg))
							  (pair? body)
							  (null? (cdr body))
							  (pair? (car body))
							  (pair? (cdar body))
							  (eq? (cadar body) (car farg))
							  (null? (cddar body)))
						     (lint-format "possible simplification:~A"
								  name 
								  (lists->string form `(,(car form) ,(caar body) ,(caddr form)))))))
					   
					   (for-each 
					    (lambda (obj)
					      (if (and (pair? obj)
						       (memq (car obj) '(vector->list string->list let->list)))
						  (lint-format "~A could be simplified to: ~A ; (~A accepts non-list sequences)" 
							       name
							       (truncated-list->string obj) 
							       (truncated-list->string (cadr obj))
							       head)))
					    (cddr form)))))
				      
				      ((catch)
				       (if (and (not (symbol? (cadr form)))
						(not (boolean? (cadr form)))
						(or (not (pair? (cadr form)))
						    (not (eq? (caadr form) 'quote))))
					   (lint-format "catch tag ~S is unreliable" name (cadr form))))
				      
				      (else
				       ;; we've already checked for head in the current env above
				       (check-for-repeated-args name head form env)
				       
				       ;; now try to check arg types for egregious errors
				       (let ((arg-data (hash-table-ref argument-data head)))
					 (if arg-data
					     (check-args name head form arg-data env max-arity)))))))))))))))

      (define (get-generator form name head) ; defgenerator funcs
	(let ((name (if (pair? (cadr form))
			(car (cadr form))
			(cadr form))))
	  ;; auto-define make-name, name?
	  (let ((make-name (string->symbol (string-append "make-" (symbol->string name))))
		(name? (string->symbol (string-append (symbol->string name) "?"))))
	    
	    (hash-table-set! globals make-name (make-var make-name))
	    (hash-table-set! globals name? (make-var name?)))))
      
      
      (define (load-walk form)
	;; check form for top-level declarations, if load seen, and we haven't seen that file, load it
	(let ((head (car form)))
	  (case head
	    ((begin)
	     (load-walk (cdr form)))
	    
	    ((define-constant define-envelope)
	     (hash-table-set! globals (cadr form) (make-var (cadr form) :val (and (pair? (cddr form)) (caddr form)))))
	    
	    ((defmacro defmacro*)
	     (hash-table-set! globals (cadr form) (make-var (cadr form) :fnc (list head (caddr form)))))
	    
	    ((define)
	     (if (pair? (cadr form))
		 (if (symbol? (caadr form))
		     (hash-table-set! globals (caadr form) (make-var (caadr form) :fnc (list head (cdadr form)))))
		 (hash-table-set! globals (cadr form) (make-var (cadr form) :val (and (pair? (cddr form)) (caddr form))))))

	    ((define* definstrument defanimal define-expansion define-macro define-macro* define-bacro define-bacro*)
	     (hash-table-set! globals (car (cadr form)) (make-var (car (cadr form)) :fnc (list head (cdr (cadr form))))))

	    ((defgenerator)
	     (get-generator form 'defgenerator head))
	    
	    ((if)
	     (if (pair? (cddr form))
		 (if (pair? (cdddr form))
		     (begin
		       (load-walk (cadddr form))
		       (load-walk (caddr form)))
		     (load-walk (caddr form)))))
	    
	    ((load)
	     (if (>= (length form) 2)
		 (scan form))))))
      
      
      (define (scan form)
	(let ((file (cadr form)))
	  (if (and (string? file)
		   (not (member file loaded-files)))
	      (let ((fp (catch #t
			  (lambda ()
			    (open-input-file file))
			  (lambda args
			    (format outport "  can't load ~S: ~A~%" file (apply format #f (cadr args)))
			    #f))))
		(if (input-port? fp)
		    (begin
		      (set! loaded-files (cons file loaded-files))
					;(format outport "  (scanning ~S)~%" file)
		      (do ((form (read fp) (read fp)))
			  ((eof-object? form))
			(if (and (pair? form)
				 (pair? (cdr form)))
			    (load-walk form)))
		      (close-input-port fp)))))))
      
      
      (define (binding-ok? name head binding env second-pass)
	;; check let-style variable binding for various syntactic problems
	(if (not (pair? binding))
	    (begin 
	      (if (not second-pass)
		  (lint-format "~A binding is not a list? ~S" name head binding))
	      #f)
	    (if (not (symbol? (car binding)))
		(begin 
		  (if (not second-pass)
		      (lint-format "~A variable is not a symbol? ~S" name head binding))
		  #f)
		(begin
		  (if (constant? (car binding))
		      (lint-format "can't bind a constant: ~S" name binding))
		  (if (keyword? (car binding))
		      (begin 
			(if (not second-pass)
			    (lint-format "~A variable is a keyword? ~S" name head binding))
			#f)
		      (if (not (pair? (cdr binding)))
			  (begin 
			    (if (not second-pass)
				(if (null? (cdr binding))
				    (lint-format "~A variable value is missing? ~S" name head binding)
				    (lint-format "~A binding is an improper list? ~S" name head binding)))
			    #f)
			  (if (and (not (= (length binding) 2))
				   (not (eq? head 'do)))
			      (begin
				(if (not second-pass)
				    (lint-format "~A binding is messed up: ~S" name head binding))
				#f)
			      (begin
				(if (and *report-shadowed-variables*
					 (not second-pass)
					 (or (hash-table-ref globals (car binding))
					     (var-member (car binding) env)))
				    (lint-format "~A variable ~A in ~S shadows an earlier declaration" name head (car binding) binding))
				#t))))))))
      
      
      (define (env-difference name e1 e2 lst)
	(if (or (null? e1)
		(null? e2)
		(eq? (car e1) (car e2)))
	    lst
	    (env-difference name (cdr e1) e2 
			    (if (eq? name (var-name (car e1)))
				lst
				(cons (car e1) lst)))))
      
      
      (define (report-usage name type head vars)
	;; report unused or set-but-unreferenced variables
	(if (and (not (eq? head 'begin)) ; begin can redefine = set a variable
		 (list? vars)
		 (pair? vars))
	    (do ((cur vars (cdr cur))
		 (rst (cdr vars) (cdr rst)))
		((null? rst))
	      (let ((repeat (var-member (var-name (car cur)) rst)))
		;; not globals here because the same name might be used as a global
		(if repeat
		    (lint-format "~A ~A ~A is declared twice" name head type (var-name (car cur)))))))
	
	(let ((set ())
	      (unused ()))
	  (for-each 
	   (lambda (arg)
	     (if (hash-table-ref syntaces (var-name arg))
		 (lint-format "~A ~A named ~A is asking for trouble" name head type (var-name arg))
		 (if (not (symbol? (var-name arg)))
		     (lint-format "bad ~A ~A name: ~S" name head type (var-name arg))))
	     (if (and (not (var-ref arg))
		      (not (hash-table-ref other-identifiers (var-name arg))))
		 (if (var-set arg)
		     (set! set (cons (var-name arg) set))
		     (if (not (memq (var-name arg) '(documentation iterator?)))
			 (set! unused (cons (var-name arg) unused))))))
	   vars)
	  
	  (if (pair? set)
	      (lint-format "~A ~A~A ~{~A~^, ~} set, but not used" 
			   name head type (if (> (length set) 1) "s" "") (reverse set)))
	  (if (pair? unused)
	      (lint-format "~A ~A~A ~{~A~^, ~} not used" 
			   name head type (if (> (length unused) 1) "s" "") (reverse unused)))))
      
      
      (define (lint-walk-body name head body env)
	;; walk a body (a list of forms, the value of the last of which might be returned)

	(if (not (list? body))
	    (lint-format "stray dot? ~A" name (truncated-list->string body))
	    
	    (let ((ctr 0)
		  (prev-f #f)
		  (len (length body)))
	      (if (eq? head 'do) (set! len (+ len 1))) ; last form in do body is not returned

	      (for-each
	       (lambda (f)
		 (if (and (pair? f)
			  (eq? (car f) 'begin))
		     (lint-format "redundant begin:~A" name (truncated-list->string f)))
		 (if (< ctr (- len 1)) ; not the last form, so its value is ignored
		     (begin
		       (if (and (pair? f)
				(eq? (car f) 'map))
			   (lint-format "map could be for-each:~A" name (truncated-list->string f)))
		       
		       (if (not (side-effect? f env))
			   (lint-format "this could be omitted:~A" name (truncated-list->string f))))
		     (begin
		       (when (and (pair? prev-f)
				  (pair? (cdr prev-f))
				  (pair? (cddr prev-f)))                ; (set! ((L 1) 2)) an error, but lint should keep going
			 (if (eq? (car prev-f) 'set!)
			     (if (or (and (equal? (caddr prev-f) f)     ; (begin ... (set! x (...)) (...))
					  (not (side-effect? f env)))
				     (and (symbol? f)                   ; (begin ... (set! x ...) x)
					  (eq? f (cadr prev-f))))
				 (lint-format "this could be omitted:~A" name (truncated-list->string f))))
			 (if (and (pair? f)
				  (pair? (cdr f))
				  (pair? (cddr f))
				  (eq? (cadr prev-f) (cadr f))
				  (or (and (eq? (car prev-f) 'vector-set!)
					   (eq? (car f) 'vector-ref))
				      (and (eq? (car prev-f) 'list-set!)
					   (eq? (car f) 'list-ref)))
				  (equal? (caddr f) (caddr prev-f))
				  (pair? (cdddr prev-f))
				  (not (pair? (cddddr prev-f)))
				  (not (pair? (cdddr f)))
				  (not (side-effect? (caddr f) env)))
			     (lint-format "this could be omitted:~A" name (truncated-list->string f)))
			 )))
		 (set! prev-f f)
		 (if (and (pair? f)
			  (memq head '(defmacro defmacro* define-macro define-macro* define-bacro define-bacro*))
			  (tree-member 'unquote f))
		     (lint-format "~A possibly has too many unquotes:~A" name head (truncated-list->string f)))
		 
		 (set! env (lint-walk name f env))
		 (set! ctr (+ ctr 1)))
	       body)))
	env)
      
      
      (define (lint-walk-function-body name head args arg-data body env)
	;; walk function body, with possible doc string at the start
	(if (and (pair? body)
		 (pair? (cdr body))
		 (string? (car body)))
	    (begin
	      (if *report-doc-strings*
		  (lint-format "old-style doc string: ~S~%" name (car body)))
	      (set! body (cdr body)))) ; ignore old-style doc-string
	(lint-walk-body name head body env)
	env)
      
      
      (define (lint-walk-function head name args val env)
	;; check out function arguments (adding them to the current env), then walk its body, (name == function name, val == body)
	;; first check for (define (hi...) (ho...)) where ho has no opt args (and try to ignore possible string constant doc string)

	(if (and *report-minor-stuff*
		 (eq? head 'define))
	    (let ((bval (if (and (pair? val)
				 (string? (car val)))
			    (cdr val)         ; strip away the (old-style) documentation string
			    val)))
	      (if (and (pair? bval)           ; not (define (hi a) . 1)!
		       (pair? (car bval))
		       (null? (cdr bval))
		       (symbol? (caar bval))) ; not (define (hi) ((if #f + abs) 0))
		  (if (equal? args (cdar bval))
		      (let* ((cval (caar bval))
			     (p (symbol->value cval))
			     (ary (arity p)))
			(if (or (and (procedure? p)
				     (= (car ary) (cdr ary)))
				(let ((e (or (var-member cval env) 
					     (hash-table-ref globals cval))))
				  (and e
				       (var? e)
				       (let ((def (var-func-info e)))
					 (and 
					  (pair? def)
					  (eq? (car def) 'define)
					  (or (and (null? args)
						   (null? (cadr def)))
					      (and (symbol? args)
						   (symbol? (cadr def)))
					      (and (pair? args)
						   (pair? (cadr def))
						   (= (length args) (length (cadr def))))))))))
			    (lint-format "~A could be (define ~A ~A)" name name name cval)))
		      (if (and (eq? (caar bval) 'list-ref)
			       (pair? (cdar bval))
			       (pair? (cddar bval))
			       (eq? (car args) (cadar bval))
			       (null? (cdr args)))
			  (case (caddar bval)
			    ((0) (lint-format "~A could be (define ~A car)" name name name))
			    ((1) (lint-format "~A could be (define ~A cadr)" name name name))
			    ((2) (lint-format "~A could be (define ~A caddr)" name name name))
			    ((3) (lint-format "~A could be (define ~A cadddr)" name name name))))))))
	    
	(let ((ldata (and (not (memq head '(lambda lambda*)))
			  (not (var-member name env))
			  (make-var name :fnc (list head args)))))
	  (if (null? args)
	      (begin
		(if (memq head '(define* lambda* defmacro* define-macro* define-bacro*))
		    (lint-format "~A could be ~A" 
				 name head
				 (symbol (substring (symbol->string head) 0 (- (length (symbol->string head)) 1)))))
		(lint-walk-function-body name head args () val env)
		(if ldata
		    (append (list ldata) env)
		    env))
	    
	      (if (or (symbol? args) 
		      (pair? args))
		  (let ((arg-data (if (symbol? args)                            ; this is getting arg names to add to the environment
				      (list (make-var args))
				      (map
				       (lambda (arg)
					 (if (symbol? arg)
					     (if (memq arg '(:optional :key :rest :allow-other-keys))
						 (values)                  ; map omits this entry 
						 (make-var arg))
					     (if (or (not (pair? arg))
						     (not (= (length arg) 2))
						     (not (memq head '(define* lambda* defmacro* define-macro* define-bacro* definstrument))))
						 (begin
						   (lint-format "strange parameter for ~A: ~S" name head arg)
						   (values))
						 (begin
						   (if (eq? (cadr arg) #f)
						       (lint-format "s7 default is #f ~A ~A" name head arg))
						   (make-var (car arg))))))
				       (proper-list args)))))
		  
		    (lint-walk-function-body name head args arg-data val (append arg-data (if ldata (append (list ldata) env) env)))
		    (if *report-unused-parameters* 
			(report-usage name 'parameter head arg-data))
		    (if ldata 
			(append (list ldata) env)
			env))
		
		  (begin
		    (lint-format "strange ~A parameter list ~A" name head args)
		    env)))))
      
      
      (define (lint-walk name form env)
	;; walk a form 
	
	(if (symbol? form)
	    (set-ref? form env) ; returns env
	    
	    (if (pair? form)
		(let ((head (car form)))
		  (set! line-number (pair-line-number form))
		  (case head
		    
		    ;; ---------------- defmacro ----------------
		    ((defmacro defmacro*)
		     (if (or (< (length form) 4)
			     (not (symbol? (cadr form))))
			 (lint-format "~A declaration is messed up: ~S" name head form)
			 (let ((sym (cadr form))
			       (args (caddr form))
			       (body (cdddr form)))
			   (if (and (pair? args)
				    (repeated-member? args env))
			       (lint-format "~A parameter is repeated:~A" name head (truncated-list->string args)))
			   (lint-walk-function head sym args body env))))
		    
		    ;; ---------------- define ----------------		  
		    ((define define* 
		       define-constant define-envelope
		       define-expansion define-macro define-macro* define-bacro define-bacro*
		       definstrument defanimal)

		     (if (< (length form) 2)
			 (begin
			   (lint-format "~S makes no sense" name form)
			   env)
			 (let ((sym (cadr form))
			       (val (cddr form)))
			   
			   (if (symbol? sym)
			       (begin
				 (if (memq head '(define define-constant define-envelope))
				     (let ((len (length form)))
				       (if (not (= len 3))
					   (lint-format "~S has ~A value~A?"
							name form 
							(if (< len 3) "no" "too many") 
							(if (< len 3) "" "s"))))
				     (lint-format "~S is messed up" name form))
				 
				 (if (and (pair? val)
					  (null? (cdr val))
					  (equal? sym (car val)))
				     (lint-format "this ~A is either not needed, or is an error:~A" name head (truncated-list->string form)))
				 
				 (if (pair? (cddr form))
				     (let ((e (lint-walk sym (caddr form) env)))
				       (if (and (pair? e)
						(eq? (var-name (car e)) sym)) ; (define x (lambda ...)) but it misses closures
					   e
					   (append (list (make-var sym)) env)))
				     (append (list (make-var sym)) env)))
			       
			       (if (and (pair? sym)
					(not (pair? (car sym))))
				   (begin
				     (if (and (pair? (cdr sym))
					      (repeated-member? (proper-list (cdr sym)) env))
					 (lint-format "~A parameter is repeated:~A" name head (truncated-list->string sym)))

				     (if (and (eq? head 'definstrument)
					      (string? (car val)))
					 (set! val (cdr val)))
				     (lint-walk-function head (car sym) (cdr sym) val env))
				   
				   (begin
				     (lint-format "strange form: ~S" head form)
				     env))))))
		    
		    ;; ---------------- defgenerator ----------------
		    ((defgenerator)
		     (get-generator form name head)
		     env)
		    
		    ;; ---------------- lambda ----------------		  
		    ((lambda lambda*)
		     (if (< (length form) 3)
			 (begin
			   (lint-format "~A is messed up in ~A"	name head (truncated-list->string form))
			   env)
			 (begin
			   (if (and (pair? (cadr form))
				    (repeated-member? (proper-list (cadr form)) env))
			       (lint-format "~A parameter is repeated:~A" name head (truncated-list->string (cadr form))))
			   (lint-walk-function head name (cadr form) (cddr form) env))))
		    ;; the lambda case includes stuff like call/cc
		    
		    ;; ---------------- set! ----------------		  
		    ((set!)
		     (if (not (= (length form) 3))
			 (begin
			   (lint-format "set! has too ~A arguments: ~S" 
					name 
					(if (> (length form) 3) "many" "few") 
					form)
			   env)
			 
			 (let ((settee (cadr form))
			       (setval (caddr form)))

			   (let ((result (lint-walk name setval env)))

			     (if (symbol? settee)
				 (begin
				   (if (and (pair? setval)
					    (> (length setval) 2) ; not (set! i (+))
					    (eq? (car setval) '+)
					    (not (eq? (cadr setval) settee))
					    (or (not (equal? (cadr setval) 1)) (pair? (cdddr setval))) ; ignore (+ 1 i) -- it's handled ok
					    (memq settee setval))
				       (lint-format "possible optimization: ~A -> ~A"
						    name form
						    `(set! ,settee (+ ,settee ,(cadr setval) ,@(remove settee (cddr setval))))))
				   (if (constant? settee)
				       (lint-format "can't set! a constant:~A" name (truncated-list->string form))))
			     
				 (if (pair? settee)
				     (begin
				       (if (and *report-minor-stuff*
						(memq (car settee) '(vector-ref list-ref string-ref hash-table-ref)))
					   (lint-format "~A as target of set!~A" name (car settee) (truncated-list->string form)))
				       (lint-walk name settee env) ; this counts as a reference since it's by reference so to speak
				       (set! settee (do ((sym (car settee) (car sym)))
							((not (pair? sym)) sym))))
				     (lint-format "odd target for set! ~A" name (truncated-list->string form))))

			     (if (symbol? settee) ; might be changed in loop above
				 (begin
				   (set-set? settee setval env)
				   (if (equal? (cadr form) (caddr form)) ; not settee and setval here!
				       (lint-format "pointless set!~A" name (truncated-list->string form)))))
			     
			     result))))
		    
		    ;; ---------------- quote ----------------		  
		    ((quote) 
		     (let ((len (length form)))
		       (if (negative? len)
			   (lint-format "stray dot in quote's arguments? ~S" name form)
			   (if (not (= len 2))
			       (lint-format "quote has too ~A arguments: ~S" 
					    name 
					    (if (> (length form) 2) "many" "few") 
					    form)
			       (if (and *report-minor-stuff*
					(or (number? (cadr form))
					    (boolean? (cadr form))
					    (string? (cadr form))
					    (vector? (cadr form))
					    (null? (cadr form))
					    (memq (cadr form) '(#<unspecified> #<undefined> #<eof>))))
				   (lint-format "quote is not needed here:~A" name (truncated-list->string form))))))
		     env)
		    
		    ;; ---------------- cond ----------------
		    ((cond)
		     (let ((ctr 0)
			   (len (- (length form) 1)))
		       (if (negative? len)
			   (lint-format "cond is messed up:~A" name (truncated-list->string form))
			   (let ((exprs ())
				 (result :unset)
				 (has-else #f)
				 (falses ()))
			     (for-each
			      (lambda (clause)
				(set! ctr (+ ctr 1))
				(if (not (pair? clause))
				    (lint-format "cond clause is messed up: ~A" name (truncated-list->string clause))
				    (let ((expr (simplify-boolean (car clause) () falses env)))
				      (if (or (eq? (car clause) 'else)
					      (eq? (car clause) #t))
					  (set! has-else #t))
				      (if (never-false expr)
					  (if (not (= ctr len))
					      (lint-format "cond test is never false: ~A" name form)
					      (if (and (not (memq expr '(#t else)))
						       (not (side-effect? (car clause) env)))
						  (lint-format "cond last test could be #t: ~A" name form)))
					  (if (never-true expr)
					      (lint-format "cond test is never true: ~A" name form)))
				      (if (not (side-effect? (car clause) env))
					  (if (member (car clause) exprs)
					      (lint-format "cond test repeated:~A" name (truncated-list->string clause))
					      (set! exprs (cons (car clause) exprs))))
				      (if (boolean? expr)
					  (if (not expr)
					      (lint-format "cond test is always false:~A" name (truncated-list->string clause))
					      (if (not (= ctr len))
						  (lint-format "cond #t clause is not the last: ~A" name (truncated-list->string form))))
					  (if (eq? (car clause) 'else)
					      (if (not (= ctr len))
						  (lint-format "cond else clause is not the last: ~A" name (truncated-list->string form)))
					      (lint-walk name (car clause) env)))
				      (if (eq? result :unset)
					  (set! result (cdr clause))
					  (if (not (equal? result (cdr clause)))
					      (set! result :unequal)))
				      (if (pair? (cdr clause))
					  (if (eq? (cadr clause) '=>)
					      (if (not (pair? (cddr clause)))
						  (lint-format "cond => target is messed up: ~A" name (truncated-list->string clause))
						  (lint-walk name (caddr clause) env))
					      (lint-walk-body name head (cdr clause) env))
					  (if (not (null? (cdr clause)))  ; (not (null?...)) here is correct -- we're looking for stray dots (lint is confused)
					      (lint-format "cond clause is messed up: ~A" name (truncated-list->string clause))))
				      (if (not (side-effect? expr env))
					  (set! falses (cons expr falses))
					  (set! result :unequal)))))
			      (cdr form))
			     (if (and has-else (pair? result))
				 (if (null? (cdr result))
				     (lint-format "possible simplification: ~A" name (lists->string form (car result)))
				     (lint-format "possible simplification: ~A" name (lists->string form `(begin ,@result)))))

			     (if (and (= len 2)
				      *report-minor-stuff*)
				 (let ((c1 (cadr form))
				       (c2 (caddr form)))
				   (if (and (pair? c1) (= (length c1) 2)
					    (pair? c2) (= (length c2) 2)
					    (boolean? (cadr c1))
					    (boolean? (cadr c2))
					    (memq (car c2) '(#t else)))
				       (if (equal? (cadr c1) (cadr c2))
					   (if (not (side-effect? (car c1) env))
					       (lint-format "possible simplification: ~A" name (lists->string form (cadr c1))))
					   (if (eq? (cadr c1) #t)
					       (lint-format "possible simplification: ~A" name (lists->string form (car c1)))
					       (lint-format "possible simplification: ~A" name (lists->string form `(not ,(car c1)))))))))))
		       env))
		    
		    
		    ;; ---------------- case ----------------		  
		    ((case)
		     ;; here the keys are not evaluated, so we might have a list like (letrec define ...)
		     (if (< (length form) 3)
			 (lint-format "case is messed up: ~A" name (truncated-list->string form))
			 (let ((sel-type +any+)
			       (selector (cadr form)))
			   (if (and (not (pair? selector))
				    (constant? selector))
			       (lint-format "case selector is a constant: ~A" name (truncated-list->string form)))
			   (lint-walk name selector env)
			   (if (and (pair? selector)
				    (symbol? (car selector)))
			       (begin
				 (set! sel-type (hash-table-ref function-types (car selector)))
				 (if (not (member sel-type sel-types))
				     (lint-format "case selector may not work with eqv: ~A" name (truncated-list->string selector)))))
			   (let ((all-keys ())
				 (ctr 0)
				 (result :unset)
				 (has-else #f)
				 (len (length (cddr form))))
			     (for-each
			      (lambda (clause)
				(set! ctr (+ ctr 1))
				(if (not (pair? clause))
				    (lint-format "case clause should be a list: ~A" name (truncated-list->string clause))
				    (let ((keys (car clause))
					  (exprs (cdr clause)))
				      (if (eq? result :unset)
					  (set! result (cdr clause))
					  (if (not (equal? result (cdr clause)))
					      (set! result :unequal)))
				      (if (pair? keys)
					  (if (not (list? keys))
					      (lint-format "stray dot in case case key list: ~A" name (truncated-list->string clause))
					      (for-each
					       (lambda (key)
						 (if (or (vector? key)
							 (string? key)
							 (pair? key)
							 (hash-table? key))
						     (lint-format "case key ~S in ~S is unlikely to work (case uses eqv?)" name key clause))
						 (if (member key all-keys)
						     (lint-format "repeated case key ~S in ~S" name key clause))
						 (if (not (type-compatible sel-type key))
						     (lint-format "case key ~S in ~S is pointless" name key clause)))
					       keys))
					  (if (not (eq? keys 'else))
					      (lint-format "bad case key ~S in ~S" name keys clause)
					      (begin
						(set! has-else #t)
						(if (not (= ctr len))
						    (lint-format "case else clause is not the last:~A"
								 name 
								 (truncated-list->string (cddr form)))))))
				      (set! all-keys (append (if (and (list? keys)
								      (pair? keys))
								 keys 
								 (list keys))
							     all-keys))
				      (lint-walk-body name head exprs env))))
			      (cddr form))
			     (if (and has-else (pair? result))
				 (if (null? (cdr result))
				     (lint-format "possible simplification: ~A" name (lists->string form (car result)))
				     (lint-format "possible simplification: ~A" name (lists->string form `(begin ,@result))))))))
		     env)
		    
		    ;; ---------------- do ----------------		  
		    ((do)
		     (let ((vars ()))
		       (if (or (< (length form) 3)
			       (not (list? (cadr form)))
			       (not (list? (caddr form))))
			   (lint-format "do is messed up: ~A" name (truncated-list->string form))
			   
			   (let ((step-vars (cadr form)))
			     
			     ;; walk the init forms before adding the step vars to env
			     (do ((bindings step-vars (cdr bindings)))
				 ((not (pair? bindings))
				  (if (pair? bindings)
				      (lint-format "do variable list is not a proper list? ~S" name step-vars)))
			       (if (binding-ok? name head (car bindings) env #f)
				   (begin
				     (lint-walk name (cadar bindings) env)
				     (set! vars (append (list (make-var (caar bindings) :typ (->type (cadar bindings)))) vars)))))
			     
			     ;; walk the step exprs
			     (do ((bindings step-vars (cdr bindings)))
				 ((not (pair? bindings)))
			       (if (and (binding-ok? name head (car bindings) env #t)
					(pair? (cddar bindings)))
				   (lint-walk name (caddar bindings) (append vars env))))
			     
			     ;; walk the body and end stuff (it's too tricky to find infinite do loops)
			     (if (pair? (caddr form))
				 (let ((end+result (caddr form)))
				   (lint-walk-body name head (cddr form) (append vars env))
				   (if (pair? end+result)
				       (if (never-false (car end+result))
					   (lint-format "end test is never false: ~A" name (car end+result))
					   (if (car end+result) ; not #f
					       (if (never-true (car end+result))
						   (lint-format "end test is never true: ~A" name (car end+result)))
					       (if (pair? (cdr end+result))
						   (lint-format "result is unreachable: ~A" name end+result))))))
				 (lint-walk-body name head (cdddr form) (append vars env)))
			     (report-usage name 'variable head vars)

			     ;; check for do-loop as copy/fill! stand-in
			     (let ((end-test (and (pair? (caddr form)) (caaddr form)))
				   (body (cdddr form))
				   (setv #f))
			       (when (and (pair? end-test)
					  (= (length vars) 1)
					  (= (length body) 1)
					  (pair? (car body)) 
					  (memq (caar body) '(vector-set! float-vector-set! list-set! string-set!))
					  (equal? (var-type (car vars)) +integer+)
					  (eq? (car end-test) '=)
					  (eq? (cadr end-test) (var-name (car vars)))
					  (eq? (caddar body) (var-name (car vars)))
					  (let ((val (car (cdddar body))))
					    (set! setv val)
					    (or (code-constant? val)
						(and (pair? val)
						     (memq (car val) '(vector-ref float-vector-ref list-ref string-ref))
						     (eq? (caddr val) (var-name (car vars)))))))
				 (if (code-constant? setv)
				     (lint-format "possible simplification: ~A" name 
						  (lists->string form `(fill! ,(cadar body) ,(car (cdddar body)) 0 ,(caddr end-test))))
				     (lint-format "possible simplification: ~A" name 
						  (lists->string form `(copy ,(cadr setv) ,(cadar body) 0 ,(caddr end-test)))))))
			     ))

		       ;; if end=#f, result can't be reached?

		       ;; if while walking the do loop body we see an expression involving
		       ;;    no-side-effect-function[but not random] + args-not-local-or-step-vars
		       ;; can that be lifted out of the body?

		       env))
		    
		    ;; ---------------- let ----------------		  
		    ((let)
		     (if (or (< (length form) 3)
			     (and (not (symbol? (cadr form)))
				  (not (null? (cadr form)))
				  (not (pair? (cadr form)))))
			 (lint-format "let is messed up: ~A" name (truncated-list->string form))
			 (let ((named-let (and (symbol? (cadr form)) (cadr form))))

			   (unless named-let
			     ;; this could be extended to other such cases
			     (call-with-exit
			      (lambda (return)
				;; check var values
				(for-each
				 (lambda (var)
				   (if (or (not (pair? var))
					   (not (pair? (cdr var)))
					   (not (code-constant? (cadr var))))
				       (return)))
				 (cadr form))
				;; check the body
				(for-each
				 (lambda (expr)
				   (if (side-effect? expr env)
				       (return)))
				 (cddr form))
				;; try to eval it
				(catch #t
				  (lambda ()
				    (let ((val (eval form (rootlet))))
				      (lint-format "possible simplification:~A" name (lists->string form val))))
				  (lambda args
				    'error)))))

			   (let ((vars (if named-let 
					   (list (make-var named-let 
							   :fnc (and (pair? (cddr form)) ; trying to protect against badly formed let's here
								     (pair? (caddr form))
								     (every? (lambda (b)
									       (and (pair? b)
										    (pair? (cdr b))))
									     (caddr form))
								     (list 'define (map car (caddr form))))))
					   ()))
				 (varlist (if named-let (caddr form) (cadr form))))
			     (if (and (not (null? varlist))
				      (not (pair? varlist)))
				 (lint-format "let is messed up: ~A" name (truncated-list->string form)))
			     (do ((bindings varlist (cdr bindings)))
				 ((not (pair? bindings))
				  (if (pair? bindings)
				      (lint-format "let variable list is not a proper list? ~S" 
						   name (if named-let (caddr form) (cadr form)))))
			       (if (binding-ok? name head (car bindings) env #f)
				   (begin
				     (if (and (pair? (cadar bindings))
					      (eq? 'lambda (car (cadar bindings)))
					      (not (hash-table-ref globals (caar bindings)))
					      (tree-car-member (caar bindings) (cadar bindings))
					      (not (var-member (caar bindings) env)))
					 (lint-format "let variable ~A is called in its binding?  Perhaps let should be letrec:~A"
						      name (caar bindings) 
						      (truncated-list->string bindings)))
				     (lint-walk name (cadar bindings) env)
				     
				     ;; can we tell its type and (as long as not set) check for type errors?
				     ;; need a function that turns a constant into a type indication,
				     ;;   then append that as the 4th entry below (unused currently I think)
				     ;;   then use that in arg checks if arg is a known var
				     
				     (set! vars (append (list (make-var (caar bindings) :val (cadar bindings) :typ (->type (cadar bindings)))) vars)))))
			     ;; each var is (sym ref set opt-func-data opt-type-data)
			     
			     (let* ((cur-env (append vars env))
				    (e (lint-walk-body name head (if named-let (cdddr form) (cddr form)) cur-env))
				    (nvars (and (not (eq? e cur-env))
						(env-difference name e cur-env ()))))
			       (if (pair? nvars)
				   (set! vars (append nvars vars))))
			     (report-usage name 'variable head vars))))
		     env)
		    
		    ;; ---------------- let* ----------------		  
		    ((let*)
		     (if (< (length form) 3)
			 (lint-format "let* is messed up: ~A" name (truncated-list->string form))
			 (let ((named-let (and (symbol? (cadr form)) (cadr form))))
			   (let ((vars (if named-let (list (make-var named-let)) ()))
				 (varlist (if named-let (caddr form) (cadr form))))
			     (if (and (not (null? varlist))
				      (not (pair? varlist)))
				 (lint-format "let* is messed up: ~A" name (truncated-list->string form)))
			     (do ((bindings varlist (cdr bindings)))
				 ((not (pair? bindings))
				  (if (pair? bindings)
				      (lint-format "let* variable list is not a proper list? ~S" 
						   name (if named-let (caddr form) (cadr form)))))
			       (if (binding-ok? name head (car bindings) env #f)
				   (begin
				     (lint-walk name (cadar bindings) (append vars env))
				     (set! vars (append (list (make-var (caar bindings) :val (cadar bindings) :typ (->type (cadar bindings)))) vars)))))
			     
			     (if (and *report-minor-stuff* ; maybe we need *report-very-minor-stuff* !
				      (call-with-exit
				       (lambda (return)
					 (for-each
					  (lambda (v)
					    (if (var-ref v) 
						(return #f)))
					  vars)
					 #t)))
				 (lint-format "let* could be let:~A" name (truncated-list->string form)))

			     ;; in s7, let evaluates var values top down, so this message is correct
			     ;;   even in cases like (let ((ind (open-sound...)) (mx (maxamp))) ...)
			     ;; in r7rs, the order is not specified (section 4.2.2 of the spec), so
			     ;;   here we would restrict this message to cases where there is only
			     ;;   one variable, or where subsequent values are known to be independent.
			     ;; if each function could tell us what globals it depends on or affects,
			     ;;   we could make this work in all cases.
			     
			     (let* ((cur-env (append vars env))
				    (e (lint-walk-body name head (if named-let (cdddr form) (cddr form)) cur-env))
				    (nvars (and (not (eq? e cur-env))
						(env-difference name e cur-env ()))))
			       (if (pair? nvars)
				   (set! vars (append nvars vars))))
			     
			     (report-usage name 'variable head vars))))
		     env)
		    
		    ;; ---------------- letrec ----------------		  
		    ((letrec letrec*)
		     (if (< (length form) 3)
			 (lint-format "~A is messed up: ~A" name head (truncated-list->string form))
			 (let ((vars ()))
			   (if (null? (cadr form))
			       (lint-format "~A could be let:~A" name head (truncated-list->string form))
			       (if (not (pair? (cadr form)))
				   (lint-format "~A is messed up: ~A" name head (truncated-list->string form))))
			   (do ((bindings (cadr form) (cdr bindings)))
			       ((not (pair? bindings))
				(if (pair? bindings)
				    (lint-format "letrec variable list is not a proper list? ~S" name (cadr form))))
			     (if (binding-ok? name head (car bindings) env #f)
				 (set! vars (append (list (make-var (caar bindings) :val (cadar bindings) :typ (->type (cadar bindings)))) vars))))
			   (let ((new-env (append vars env)))
			     (do ((bindings (cadr form) (cdr bindings)))
				 ((not (pair? bindings)))
			       (if (binding-ok? name head (car bindings) env #t)
				   (lint-walk name (cadar bindings) new-env)))
			     
			     (let* ((cur-env (append vars env))
				    (e (lint-walk-body name head (cddr form) cur-env))
				    (nvars (and (not (eq? e cur-env))
						(env-difference name e cur-env ()))))
			       (if (pair? nvars)
				   (set! vars (append nvars vars)))))
			   
			   (report-usage name 'variable head vars)))
		     env)
		    
		    ;; ---------------- begin ----------------
		    ((begin)
		     (if (not (list? form))
			 (begin
			   (lint-format "stray dot in begin? ~A" name (truncated-list->string form))
			   env)
			 (begin
			   (if (and (pair? (cdr form))
				    (null? (cddr form)))
			       (lint-format "begin could be omitted:~A" name (truncated-list->string form)))
			   (lint-walk-body name head (cdr form) env)))
		     env)
		    
		    ;; ---------------- other schemes ----------------		  
		    ((define-syntax let-syntax letrec-syntax define-module re-export case-lambda) ; for other's code
		     env) 
		    
		    ;; -------- when, unless --------
		    ((when unless)
		     (if (< (length form) 3)
			 (lint-format "~A is messed up: ~A" name head (truncated-list->string form))
			 (let ((test (cadr form)))
			   (if (and (pair? test)
				    (eq? (car test) 'not))
			       (lint-format "possible optimization: ~A -> ~A"
					    name 
					    (truncated-list->string form)
					    (truncated-list->string `(,(if (eq? head 'when) 'unless 'when)
								      ,(cadr test)
								      ,@(cddr form)))))
			   (if (never-false test)
			       (lint-format "~A test is never false: ~A" name head form)
			       (if (never-true test)
				   (lint-format "~A test is never true: ~A" name head form)))
			   (if (symbol? test)
			       (set-ref? test env)
			       (if (pair? test)
				   (lint-walk name test env)))
			   (let* ((e (lint-walk-body name head (cddr form) env))
				  (vars (if (not (eq? e env))
					    (env-difference name e env ())
					    ())))
			     (report-usage name 'variable head vars))))
		     env)

		    ;; -------- with-let --------
		    ((with-let)
		     (if (< (length form) 3)
			 (lint-format "~A is messed up: ~A" head name (truncated-list->string form))
			 (let ((e (cadr form)))
			   (if (or (and (code-constant? e)
					(not (let? e)))
				   (and (symbol? e)
					(defined? e)
					(not (let? (symbol->value e))))
				   (and (pair? e)
					(let ((op (hash-table-ref function-types (car e))))
					  (and op
					       (not (let? op))))))
			       (lint-format "~A: first argument should be an environment: ~A" head name (truncated-list->string form)))
			   (if (symbol? e)
			       (set-ref? e env)
			       (if (pair? e)
				   (begin
				     (if (and *report-minor-stuff*
					      (null? (cdr e))
					      (eq? (car e) 'curlet))
					 (lint-format "~A is not needed here: ~A" head name (truncated-list->string form)))
				     (lint-walk name e env))))
			   (let* ((e (lint-walk-body name head (cddr form) env))
				  (vars (if (not (eq? e env))
					    (env-difference name e env ())
					    ())))
			     (report-usage name 'variable head vars))))
		     env)

		    ;; with-baffle is not a special case, I guess

		    
		    ;; ---------------- everything else ----------------		  
		    (else
		     (if (not (list? form))
			 (begin
			   (lint-format "stray dot? ~A" name (truncated-list->string form))
			   env)
			 (begin
			   (if (symbol? head)
			       (begin
				 (check-call name head form env)
				 (if (not (or (hash-table-ref globals head)
					      (var-member head env) ))
				     (check-special-cases name head form env))
				 (if (assq head deprecated-ops)
				     (lint-format "~A is deprecated; use ~A" name head (cdr (assq head deprecated-ops))))

				 (if (and *report-minor-stuff*
					  (not (= line-number last-simplify-numeric-line-number))
					  ;(pair? (cdr form))
					  (not (hash-table-ref globals head))
					  (hash-table-ref numeric-ops head)
					  (not (var-member head env)))
				     (let ((val (simplify-numerics form env)))
				       (if (not (equal-ignoring-constants? form val))
					   (begin
					     (set! last-simplify-numeric-line-number line-number)
					     (lint-format "possible simplification:~A" name (lists->string form val))))))
				 
				 ;; if we loaded this file first, and f (head) is defined (e.g. scan above),
				 ;; and it is used before it is defined, but not thereafter, the usage stuff 
				 ;; can get confused, so other-identifiers is trying to track those.
				 
				 (if (and (not (hash-table-ref other-identifiers head))
					  (not (defined? head start-up-let)))
				     (hash-table-set! other-identifiers head #t))))
			   
			   (let ((vars env))
			     (for-each
			      (lambda (f)
				(set! vars (lint-walk name f vars)))
			      form))
			   ))
		     env)))
		
		;; else form is not a symbol and not a pair
		env)))
      
    ;;; --------------------------------------------------------------------------------
      (let ((documentation "(lint file port) looks for infelicities in file's scheme code"))
	(lambda* (file (outp *lint-output-port*) (report-input #t))
	  (set! outport outp)
	  (set! globals (make-hash-table))
	  (set! other-identifiers (make-hash-table))
	  (set! loaded-files ())
	  (set! last-simplify-boolean-line-number -1)
	  (set! last-simplify-numeric-line-number -1)
	  (set! line-number -1)
	  
	  ;(format *stderr* "lint ~S~%" file)
	  
	  (let ((fp (if (input-port? file)
			file
			(begin
			  (set! *current-file* file)
			  (if *load-file-first* ; this can improve the error checks
			      (load file))
			  (catch #t
			    (lambda ()
			      (let ((p (open-input-file file)))
				(if report-input (format outport ";~A~%" file))
				(set! loaded-files (cons file loaded-files))
				p))
			    (lambda args
			      (format outport "  can't open ~S: ~A~%" file (apply format #f (cadr args)))
			      #f))))))
	    
	    (if (input-port? fp)
		(let ((vars ())
		      (line 0)
		      (last-form #f)
		      (last-line-number -1))
		  (do ((form (read fp) (read fp)))
		      ((eof-object? form))
		    (if (pair? form)
			(set! line (max line (pair-line-number form))))
		    
		    (if (and (not (= last-line-number -1))
			     (not (side-effect? last-form vars)))
			(format outport "  top-level (line ~D): this has no effect:~A~%" 
				last-line-number
				(truncated-list->string last-form)))
		    (set! last-form form)
		    (set! last-line-number line)
		    (set! vars (lint-walk (if (symbol? form) 
					      form 
					      (if (pair? form) 
						  (car form)
						  #f))
					  form 
					  vars)))
		  
		  (if *report-multiply-defined-top-level-functions*
		      (for-each
		       (lambda (var)
			 (let ((var-file (hash-table-ref *top-level-objects* (car var))))
			   (if (not var-file)
			       (hash-table-set! *top-level-objects* (car var) *current-file*)
			       (if (and (string? *current-file*)
					(not (string=? var-file *current-file*)))
				   (format outport ";~S is defined at the top level in ~S and ~S~%" (car var) var-file *current-file*)))))
		       vars))
		  
		  (if (and (string? file)
			   *report-unused-top-level-functions*)
		      (report-usage file 'top-level-var "" vars))
	      
		  (if (not (input-port? file))
		      (close-input-port fp))))))))))



;;; macros that cause definitions are ignored (this also affects variable usage stats)
;;; and cload'ed identifiers are missed
;;;
;;; what about cond expr that can't be true given previous exprs? -- like or (1508)  and test (cond (#f ...)...)?
;;;  also (set! x 32) (set! x 123) etc [list-set!...]
;;;  also (set! x 32) (list-ref x 1)...
;;;
;;; if with-let, lint should try to be smarter about local names (read if *libc*)


;;; --------------------------------------------------------------------------------
;;; this reads an HTML file, finds likely-looking scheme code, and runs lint over it.
;;;    called on all snd files in hg.scm

(define (html-lint file)
  
  (define (remove-markups str)
    (let ((tpos (string-position "<b>" str)))
      (if tpos
	  (let ((epos (string-position "</b>" str)))
	    (remove-markups (string-append (substring str 0 tpos)
					   (substring str (+ tpos 3) epos)
					   (substring str (+ epos 4)))))
	  (let ((apos (string-position "<a " str))
		(epos (string-position "<em " str)))
	    (if (and (not apos)
		     (not epos))
		str
		(let* ((pos (if (and apos epos) (min apos epos) (or apos epos)))
		       (bpos (char-position #\> str (+ pos 1)))
		       (epos (if (and apos (= pos apos))
				 (string-position "</a>" str (+ bpos 1))
				 (string-position "</em>" str (+ bpos 1)))))
		  (string-append (substring str 0 pos)
				 (substring str (+ bpos 1) epos)
				 (remove-markups (substring str (+ epos (if (and apos (= apos pos)) 4 5)))))))))))
  
  (define (fixup-html str)
    (let ((pos (char-position #\& str)))
      (if (not pos)
	  str
	  (string-append (substring str 0 pos)
			 (let ((epos (char-position #\; str pos)))
			   (let ((substr (substring str (+ pos 1) epos)))
			     (let ((replacement (cond ((string=? substr "gt") ">")
						      ((string=? substr "lt") "<")
						      ((string=? substr "mdash") "-")
						      ((string=? substr "amp") "&")
						      (else (format #t "unknown: ~A~%" substr)))))
			       (string-append replacement
					      (fixup-html (substring str (+ epos 1)))))))))))
  
  (call-with-input-file file
    (lambda (f)
      (do ((line-num 0 (+ line-num 1))
	   (line (read-line f #t) (read-line f #t)))
	  ((eof-object? line))
	
	;; look for <pre , gather everything until </pre>
	;;   decide if it is scheme code (first char is #\()
	;;   if so, clean out html markup stuff, call lint on that
	
	(let ((pos (string-position "<pre" line)))
	  (if pos
	      (let ((code (substring line (+ (char-position #\> line) 1))))
		(do ((cline (read-line f #t) (read-line f #t))
		     (rline 1 (+ rline 1)))
		    ((string-position "</pre>" cline)
		     (set! line-num (+ line-num rline)))
		  (set! code (string-append code cline)))
		
		;; is first non-whitespace char #\(? ignoring comments
		(let ((len (length code)))
		  (do ((i 0 (+ i 1)))
		      ((>= i len))
		    (let ((c (string-ref code i)))
		      (if (not (char-whitespace? c))
			  (if (char=? c #\;)
			      (set! i (char-position #\newline code i))
			      (begin
				(set! i (+ len 1))
				(if (char=? c #\()
				    (catch #t
				      (lambda ()
					(let ((ncode (with-input-from-string 
							 (fixup-html (remove-markups code)) 
						       (lambda () 
							 (read)))))
					  (call-with-output-file "t631-temp.scm"
					    (lambda (fout)
					      (format fout "~S~%" ncode)))
					  (let ((outstr (call-with-output-string
							 (lambda (p)
							   (let ((old-shadow *report-shadowed-variables*))
							     (set! *report-shadowed-variables* #t)
							     (lint "t631-temp.scm" p #f)
							     (set! *report-shadowed-variables* old-shadow))))))
					    (if (> (length outstr) 0)
						(format #t ";~A ~D: ~A~%" file line-num outstr)))))
				      (lambda args
					(format #t ";~A ~D, error in read: ~A ~A~%" file line-num args
						(fixup-html (remove-markups code)))))))))))))))))))
