(provide 'mockery.scm)

(define (outlet-member obj e)
  (or (eq? obj e)
      (and (not (eq? obj (rootlet)))
	   (outlet-member (outlet obj) e))))

(define (make-method f accessor)
  (lambda args
    (if (let? (car args)) 
	(apply f (accessor (car args)) (cdr args))
	(apply f (car args) (accessor (cadr args)) (cddr args)))))

(define (make-object . args)
  (openlet
   (apply inlet args)))


(define (mock->string obj . args)
  (dynamic-wind
      (lambda () (coverlet obj))
      (lambda () (apply #_object->string (obj 'value) args))
      (lambda () (openlet obj))))



;;; --------------------------------------------------------------------------------

(define *mock-vector*
  (let ((mock-vector? #f))
    (let ((mock-vector-class
	   (openlet  
	    (inlet 'morally-equal?     (make-method #_morally-equal? (lambda (obj) (obj 'value))) ; see comment below

		   'local-set!         (lambda (obj i val)          ; reactive-vector uses this as a hook into vector-set!
					 (if (vector? (obj 'value))
					     (#_vector-set! (obj 'value) i val)
					     (error 'wrong-type-arg "vector-set! ~S ~S ~S" obj i val))) ; the wrong arg here is 'i
		   'vector-set!        (lambda (obj i val) ((obj 'local-set!) obj i val) val)
		   'let-set!           (lambda (obj i val) ((obj 'local-set!) obj i val) val)

		   'vector-ref         (lambda (obj i) 
					 (if (mock-vector? obj)
					     (#_vector-ref (obj 'value) i)
					     (error 'wrong-type-arg "vector-ref ~S ~S" obj i)))

		   'let-ref            (lambda (obj i) (#_vector-ref (obj 'value) i))   ; the implicit case, so 'i can't be the culprit
		   'vector-length      (lambda (obj) (#_length (obj 'value)))
		   'vector-append      (make-method #_vector-append (lambda (obj) (obj 'value)))
		   'reverse            (lambda (obj) (#_reverse (obj 'value)))
		   'sort!              (lambda (obj f) (#_sort! (obj 'value) f))
		   'make-iterator      (lambda (obj) (#_make-iterator (obj 'value)))
		   'arity              (lambda (obj) (#_arity (obj 'value)))
		   'object->string     (lambda* (obj (w #t)) "#<mock-vector-class>")
		   'vector-dimensions  (lambda (obj) (#_vector-dimensions (obj 'value)))
		   'fill!              (lambda (obj val) (#_fill! (obj 'value) val))

		   'vector->list       (lambda (obj . args)
					 (if (mock-vector? obj)
					     (map values (obj 'value))
					     (error 'wrong-type-arg "vector->list ~S ~S" obj args)))

		   'make-shared-vector (lambda* (obj dim (off 0))
					 (if (mock-vector? obj)
					     (#_make-shared-vector (obj 'value) dim off)
					     (error 'wrong-type-arg "make-shared-vector ~S ~S ~S" obj dim off)))

		   'vector-fill!       (lambda (obj . args)
					 (if (mock-vector? obj)
					     (apply #_fill! (obj 'value) args)
					     (error 'wrong-type-arg "vector-fill! ~S ~S ~S ~S" obj val start end)))

		   'copy               (lambda* (source dest . args)
					 ;; copy by itself does not make a new vector, but if no dest we
					 ;;   need a copy of source, so use coverlet/openlet to make sure
					 ;;   we aren't caught in an infinite recursion.
					 (if (mock-vector? source)
					     (if (and dest (not (let? dest)))
						 (apply copy (source 'value) dest args)
						 (let ((nobj (or dest 
								 (dynamic-wind
								     (lambda () (coverlet source))
								     (lambda () (openlet (copy source)))
								     (lambda () (openlet source))))))
						   (if dest
						       (apply copy (source 'value) (nobj 'value) args)
						       (set! (nobj 'value) (copy (source 'value))))
						   nobj))
					     (error 'wrong-type-arg "copy ~S ~S ~S" source dest args)))
		   'vector?            (lambda (obj) #t)
		   'values             (lambda (obj . args) (obj 'value))
		   'length             (lambda (obj) (#_length (obj 'value)))
		   'class-name         'mock-vector))))
      
      (define* (make-mock-vector len (init #<unspecified>))
	(openlet (sublet mock-vector-class 
		   'value (#_make-vector len init)
		   'object->string mock->string)))
      
      (define (mock-vector . args)
	(let ((v (make-mock-vector 0)))
	  (set! (v 'value) (apply #_vector args))
	  v))
      
      (set! mock-vector? (lambda (obj)
			   (and (openlet? obj)
				(outlet-member obj mock-vector-class))))
      
      (curlet))))


#|
;;; the morally-equal? method should track circles (via cyclic-sequences?)
;;;   as it is now, this code cores up indefinitely:
(let ((v1 ((*mock-vector* 'mock-vector) 1 2 3 4))
      (v2 ((*mock-vector* 'mock-vector) 2 3 4))
      (v3 #(1 2 3 4))
      (v4 #(2 3 4)))
  (vector-set! v2 0 v1)
  (vector-set! v1 0 v2)
  (vector-set! v4 0 v3)
  (vector-set! v3 0 v4)
  (morally-equal? v1 v3))
;;; but how to make this cooperate with the built-in method -- we should call cyclic-sequences just once
|#

#|
;; vector that grows to accommodate vector-set!

(define stretchable-vector-class
  (let ((local-ref (lambda (obj index)
		     (if (>= index (length (obj 'value)))
			 (obj 'initial-element)
			 (#_vector-ref (obj 'value) index))))
	(local-set! (lambda (obj index val)
		      (if (>= index (length (obj 'value)))
			  (set! (obj 'value) (copy (obj 'value) (make-vector (+ index 8) (obj 'initial-element)))))
		      (#_vector-set! (obj 'value) index val))))
    (openlet
     (sublet (*mock-vector* 'mock-vector-class)
       'value (vector)
       'object->string mock->string
       'initial-element #f
       'vector-ref local-ref
       'let-ref local-ref
       'vector-set! local-set!
       'let-set! local-set!))))
|#


;;; --------------------------------------------------------------------------------

(define *mock-hash-table*
  (let ((mock-hash-table? #f))
    (let ((mock-hash-table-class
	   (openlet
	    (inlet 'morally-equal?     (lambda (x y)          (#_morally-equal? (x 'mock-hash-table-table) y))
		   'hash-table-ref     (lambda (obj key)      (#_hash-table-ref (obj 'mock-hash-table-table) key))
		   'hash-table-set!    (lambda (obj key val)  (#_hash-table-set! (obj 'mock-hash-table-table) key val))
		   'hash-table-size    (lambda (obj)          (#_length (obj 'mock-hash-table-table)))
		   'hash-table-entries (lambda (obj)          (#_hash-table-entries (obj 'mock-hash-table-table)))
		   'make-iterator      (lambda (obj)          (#_make-iterator (obj 'mock-hash-table-table)))

		   'let-ref-fallback   (lambda (obj key)
					 (if (defined? 'mock-hash-table-table obj)
					     (#_hash-table-ref (obj 'mock-hash-table-table) key)))
		   'let-set!-fallback  (lambda (obj key val)  
					 (if (defined? 'mock-hash-table-table obj)
					     (#_hash-table-set! (obj 'mock-hash-table-table) key val)))
		   
		   ;; the fallbacks are needed because hash-tables and lets use exactly the same syntax in implicit indexing:
		   ;;   (x 'y) but s7 can't tell that in this one case, we actually want the 'y to be a key not a field.
		   ;;   So, to avoid infinite recursion in let-ref (implicit index), if let-ref can't find the let field,
		   ;;   and the let has 'let-ref|set!-fallback, let-ref|set! passes the argument to that function rather than
		   ;;   return #<undefined>.
		   ;;
		   ;; (round (openlet (inlet 'round (lambda (obj) (#_round (obj 'value))) 'let-ref-fallback (lambda args 3)))) -> 3
		   
		   'fill!              (lambda (obj val)      (#_fill! (obj 'mock-hash-table-table) val))
		   'reverse            (lambda (obj)          (#_reverse (obj 'mock-hash-table-table)))
		   'object->string     (lambda* (obj (w #t))  "#<mock-hash-table-class>")
		   'arity              (lambda (obj)          (#_arity (obj 'mock-hash-table-table)))
		   'copy               (lambda* (source dest . args)
					 (if (mock-hash-table? source)
					     (if (and dest (not (let? dest)))
						 (apply copy (obj 'mock-hash-table-table) dest args)
						 (let ((nobj (or dest (openlet (copy (coverlet source))))))
						   (openlet source)
						   (set! (nobj 'mock-hash-table-table) (copy (source 'mock-hash-table-table)))
						   nobj))
					     (error 'wrong-type-arg "copy ~S ~S ~S" source dest args)))
		   'hash-table?        (lambda (obj) #t)
		   'values             (lambda (obj . args) (obj 'mock-hash-table-table))
		   'length             (lambda (obj)          (#_length (obj 'mock-hash-table-table)))
		   'class-name         'mock-hash-table))))

      (define* (make-mock-hash-table (len 511))
	(openlet 
	 (sublet mock-hash-table-class 
	   'mock-hash-table-table (#_make-hash-table len)

	   ;; object->string here is a problem -- don't set any value to the object itself!
	   'object->string (lambda (obj . args) ; can't use mock->string because the value is not in the 'value field
			     (dynamic-wind
				 (lambda () (coverlet obj))
				 (lambda () (apply #_object->string (obj 'mock-hash-table-table) args))
				 (lambda () (openlet obj)))))))
      
      (define (mock-hash-table . args)
	(let ((v (make-mock-hash-table)))
	  (set! (v 'mock-hash-table-table) (apply #_hash-table args))
	  v))
      
      (define (mock-hash-table* . args)
	(let ((v (make-mock-hash-table)))
	  (set! (v 'mock-hash-table-table) (apply #_hash-table* args))
	  v))
      
      (set! mock-hash-table? (lambda (obj)
			       (and (openlet? obj)
				    (outlet-member obj mock-hash-table-class))))
      
      (curlet))))


#|
;; hash-table that returns a special identifier when key is not in the table

(define gloomy-hash-table
  (openlet
   (sublet (*mock-hash-table* 'mock-hash-table-class) ; ideally this would be a separate (not copied) gloomy-hash-table-class 
     'mock-hash-table-table #f
     'false (gensym)
     'not-a-key #f
     'hash-table-ref (lambda (obj key)
		       (let ((val (#_hash-table-ref (obj 'mock-hash-table-table) key)))
			 (if (eq? val (obj 'false))
			     #f
			     (or val (obj 'not-a-key)))))
     'hash-table-key? (lambda (obj key)
			(#_hash-table-ref (obj 'mock-hash-table-table) key)))))

(define (hash-table-key? obj key) 
  ((obj 'hash-table-key?) obj key))

(define* (make-gloomy-hash-table (len 511) not-a-key)
  (let ((ht (copy gloomy-hash-table)))
    (set! (ht 'mock-hash-table-table) (make-hash-table len))
    (set! (ht 'not-a-key) not-a-key)
    ht))
|#



;;; --------------------------------------------------------------------------------

(define *mock-string*
  (let ((mock-string? #f))
    (let ((mock-string-class
	   (openlet
	    (inlet 'morally-equal?         (lambda (x y) (#_morally-equal? (x 'value) y))
		   'reverse                (lambda (obj) (#_reverse (obj 'value)))
		   'object->string         (lambda* (obj (w #t)) "#<mock-string-class>")
		   'arity                  (lambda (obj) (#_arity (obj 'value)))
		   'make-iterator          (lambda (obj) (#_make-iterator (obj 'value)))
		   'let-ref                (lambda (obj i) (#_string-ref (obj 'value) i))           ; these are the implicit cases
		   'let-set!               (lambda (obj i val) (string-set! (obj 'value) i val))
		   'string-length          (lambda (obj) (#_length (obj 'value)))
		   'string-append          (make-method #_string-append (lambda (obj) (obj 'value)))
		   'string-copy            (lambda (obj) (#_copy (obj 'value)))
		   'string=?               (make-method #_string=? (lambda (obj) (obj 'value)))
		   'string<?               (make-method #_string<? (lambda (obj) (obj 'value)))
		   'string>?               (make-method #_string>? (lambda (obj) (obj 'value)))
		   'string<=?              (make-method #_string<=? (lambda (obj) (obj 'value)))
		   'string>=?              (make-method #_string>=? (lambda (obj) (obj 'value)))
		   'string-downcase        (lambda (obj) (#_string-downcase (obj 'value)))
		   'string-upcase          (lambda (obj) (#_string-upcase (obj 'value)))
		   'string->symbol         (lambda (obj) (#_string->symbol (obj 'value)))
		   'symbol                 (lambda (obj) (#_symbol (obj 'value)))
		   'gensym                 (lambda (obj) (#_gensym (obj 'value)))
		   'make-keyword           (lambda (obj) (#_make-keyword (obj 'value)))
		   'open-input-string      (lambda (obj) (#_open-input-string (obj 'value)))
		   'call-with-input-string (lambda (obj f) (#_call-with-input-string (obj 'value) f))
		   'with-input-from-string (lambda (obj f) (#_with-input-from-string (obj 'value) f))
		   'directory?             (lambda (obj) (#_directory? (obj 'value)))
		   'file-exists?           (lambda (obj) (#_file-exists? (obj 'value)))
		   'getenv                 (lambda (obj) (#_getenv (obj 'value)))
		   'delete-file            (lambda (obj) (#_delete-file (obj 'value)))
		   'system                 (lambda* (obj cap) (#_system (obj 'value) cap))
		   '->byte-vector           (lambda (obj) (#_->byte-vector (obj 'value))) ; this is in-place! 
		   'load                   (lambda* (obj (e (curlet))) (#_load (obj 'value) e))
		   'eval-string            (lambda* (obj (e (curlet))) (#_eval-string (obj 'value) e))
		   'char-position          (make-method #_char-position (lambda (obj) (obj 'value)))

		   'format                 (make-method #_format (lambda (obj) (obj 'value)))
		   'string-fill!           (lambda* (obj val (start 0) end) 
					     (if (mock-string? obj)
						 (#_string-fill! (obj 'value) val start (or end (#_string-length (obj 'value))))
						 (error 'wrong-type-arg "string-fill! ~S ~S ~S ~S" obj val start end)))
		   
		   'fill!                  (lambda (obj val) 
					     (if (mock-string? obj)
						 (#_fill! (obj 'value) val)
						 (error 'wrong-type-arg "fill! ~S ~S" obj val)))
		   
		   'copy                   (lambda* (obj . args) 
					     (if (mock-string? obj)
						 (apply #_copy (obj 'value) args)
						 (error 'wrong-type-arg "copy ~S ~S" obj args)))
		   
		   'substring              (lambda (obj . args) 
					     (if (mock-string? obj)
						 (apply #_substring (obj 'value) args)
						 (error 'wrong-type-arg "substring ~S ~S" obj args)))
		   
		   'string->number         (lambda* (obj (r 10))
					     (if (mock-string? obj)
						 (#_string->number (obj 'value) r)
						 (error 'wrong-type-arg "string->number ~S ~S" obj r)))
		   
		   'write-string           (lambda (obj . args) 
					     (if (mock-string? obj)
						 (apply #_write-string (obj 'value) args)
						 (error 'wrong-type-arg "write-string ~S ~S" obj args)))
		   
		   'string-position        (lambda* (s1 s2 (start 0))
					     (if (mock-string? s1)
						 (#_string-position (s1 'value) s2 start)
						 (if (mock-string? s2)
						     (#_string-position s1 (s2 'value) start)
						     (error 'wrong-type-arg "write-string ~S ~S ~S" s1 s2 start))))
		   'string-ref             (lambda (obj i) 
					     (if (mock-string? obj)
						 (#_string-ref (obj 'value) i)
						 (error 'wrong-type-arg "string-ref ~S ~S" obj i)))
		   
		   'string-set!            (lambda (obj i val) 
					     (if (mock-string? obj)
						 (#_string-set! (obj 'value) i val)
						 (error 'wrong-type-arg "string-set! ~S ~S ~S" obj i val)))
		   (reader-cond ((not (provided? 'pure-s7))
		   'string->list           (make-method #_string->list (lambda (obj) (obj 'value)))
		   'string-ci=?            (make-method #_string-ci=? (lambda (obj) (obj 'value)))
		   'string-ci<?            (make-method #_string-ci<? (lambda (obj) (obj 'value)))
		   'string-ci>?            (make-method #_string-ci>? (lambda (obj) (obj 'value)))
		   'string-ci<=?           (make-method #_string-ci<=? (lambda (obj) (obj 'value)))
		   'string-ci>=?           (make-method #_string-ci>=? (lambda (obj) (obj 'value)))))
		   
		   'string?                (lambda (obj) #t)
		   'values                 (lambda (obj . args) (obj 'value))
		   'length                 (lambda (obj) (#_string-length (obj 'value)))
		   'class-name             'mock-string))))
      
      (define* (make-mock-string len (init #\null))
	(openlet (sublet mock-string-class 
		   'value (#_make-string len init)
		   'object->string mock->string)))
      
      (define (mock-string . args)
	(let ((v (make-mock-string 0)))
	  (set! (v 'value) 
		(if (string? (car args))
		    (car args)
		    (apply #_string args)))
	  v))
      
      (set! mock-string? (lambda (obj)
			   (and (openlet? obj)
				(outlet-member obj mock-string-class))))
      
      (curlet))))

#|
;; string that is always the current time of day
(require libc.scm)

(define time-string
  (let ((daytime (lambda args
		   (with-let (sublet *libc*)
		     (let ((timestr (make-string 64))) 
		       (let ((len (strftime timestr 64 "%a %d-%b-%Y %H:%M %Z"
					    (localtime 
					     (time.make (time (c-pointer 0)))))))
			 (substring timestr 0 len)))))))
    (openlet
     (sublet (*mock-string* 'mock-string-class) ; the mock-string isn't really needed here
       'let-ref-fallback daytime
       'object->string daytime))))

;; similarly ("JIT data"):
(define ? (openlet 
	   (inlet 'object->string (lambda (obj . args) 
				    (apply #_object->string (owlet) args)))))

|#


;;; --------------------------------------------------------------------------------

(define *mock-char*
  (let ((mock-char? #f))
    (let ((mock-char-class
	   (openlet
	    (inlet 'morally-equal?     (lambda (x y) (#_morally-equal? (x 'value) y))
		   'char-upcase        (lambda (obj) (#_char-upcase (obj 'value)))
		   'char-downcase      (lambda (obj) (#_char-downcase (obj 'value)))
		   'char->integer      (lambda (obj) (#_char->integer (obj 'value)))
		   'char-upper-case?   (lambda (obj) (#_char-upper-case? (obj 'value)))
		   'char-lower-case?   (lambda (obj) (#_char-lower-case? (obj 'value)))
		   'char-alphabetic?   (lambda (obj) (#_char-alphabetic? (obj 'value)))
		   'char-numeric?      (lambda (obj) (#_char-numeric? (obj 'value)))
		   'char-whitespace?   (lambda (obj) (#_char-whitespace? (obj 'value)))
		   'char=?             (make-method #_char=? (lambda (obj) (obj 'value)))
		   'char<?             (make-method #_char<? (lambda (obj) (obj 'value)))
		   'char>?             (make-method #_char>? (lambda (obj) (obj 'value)))
		   'char<=?            (make-method #_char<=? (lambda (obj) (obj 'value)))
		   'char>=?            (make-method #_char>=? (lambda (obj) (obj 'value)))
		   'char-ci=?          (make-method #_char-ci=? (lambda (obj) (obj 'value)))
		   'char-ci<?          (make-method #_char-ci<? (lambda (obj) (obj 'value)))
		   'char-ci>?          (make-method #_char-ci>? (lambda (obj) (obj 'value)))
		   'char-ci<=?         (make-method #_char-ci<=? (lambda (obj) (obj 'value)))
		   'char-ci>=?         (make-method #_char-ci>=? (lambda (obj) (obj 'value)))
		   'string             (make-method #_string (lambda (obj) (obj 'value)))
		   'object->string     (lambda (obj . args) "#<mock-char-class>")
		   'arity              (lambda (obj) (#_arity (obj 'value)))
		   'format             (make-method #_format (lambda (obj) (obj 'value)))
		   'make-string        (make-method #_make-string (lambda (obj) (obj 'value)))
		   'char-position      (make-method #_char-position (lambda (obj) (obj 'value)))
		   
		   'write-char         (lambda (obj . args) 
					 (if (mock-char? obj)
					     (apply #_write-char (obj 'value) args)
					     (error 'wrong-type-arg "write-char: ~S ~S" obj args)))
		   
		   'string-set!        (lambda (obj ind val)
					 (if (and (string? obj)
						  (integer? ind))
					     (#_string-set! obj ind (val 'value))
					     (error 'wrong-type-arg "string-set! ~S ~S ~S" obj ind val)))
		   
		   'copy               (lambda (obj . args) 
					 (if (mock-char? obj)
					     (obj 'value)
					     (error 'wrong-type-arg "copy: ~S ~S" obj args)))
		   'char?              (lambda (obj) #t)
		   'class-name         'mock-char
		   'values             (lambda (obj . args) (obj 'value))))))
      
      (define (mock-char c) 
	(if (and (char? c)
		 (not (let? c)))
	    (openlet
	     (sublet (*mock-char* 'mock-char-class)
	       'value c
	       'object->string mock->string))
	    (error 'wrong-type-arg "mock-char ~S is not a char" c)))
      
      (set! mock-char? (lambda (obj)
			 (and (openlet? obj)
			      (outlet-member obj mock-char-class))))
      
      (curlet))))

#|
;;; eventually I'll conjure up unichars like (define lambda (byte-vector #xce #xbb)) via mock-char,
;;;   then combine those into unistring via mock-string
;;;
;;; (string-length obj)->g_utf8_strlen etc 
;;;   (g_unichar_isalpha (g_utf8_get_char (byte-vector #xce #xbb))) -> #t
;;;   (g_utf8_strlen (byte-vector #xce #xbb #xce #xba) 10) -> 2
;;;   (g_utf8_normalize (byte-vector #xce #xbb #xce #xba) 4 G_NORMALIZE_DEFAULT)
;;;   but the ones that return gunichar (toupper) currently don't return a byte-vector or a string
;;;   maybe gunichar->byte-vector?
;;;   need glib.scm, or unicode.scm to load the stuff
|#



;;; --------------------------------------------------------------------------------

(define *mock-number*
  (let ((mock-number? #f))

    (define* (range-method-1 func arg start end)
      (if (not end)
	  (func arg (start 'value))
	  (if (not (let? start))
	      (func arg start (end 'value))
	      (apply func arg (start 'value) end ()))))

    (define* (range-method-2 func arg1 arg2 start end)
      (if (not end)
	  (func arg1 arg2 (start 'value))
	  (if (not (let? start))
	      (func arg1 arg2 start (end 'value))
	      (apply func arg1 arg2 (start 'value) end ()))))

    (let ((mock-number-class
	   (openlet
	    (inlet 
		   'morally-equal?   (lambda (x y) (#_morally-equal? (x 'value) y))
		   'object->string   (lambda (obj . args) "#<mock-number-class>")
		   'arity            (lambda (obj) (#_arity (obj 'value)))
		   'real-part        (lambda (obj) (#_real-part (obj 'value)))
		   'imag-part        (lambda (obj) (#_imag-part (obj 'value)))
		   'numerator        (lambda (obj) (#_numerator (obj 'value)))
		   'denominator      (lambda (obj) (#_denominator (obj 'value)))
		   'even?            (lambda (obj) (#_even? (obj 'value)))
		   'odd?             (lambda (obj) (#_odd? (obj 'value)))
		   'zero?            (lambda (obj) (#_zero? (obj 'value)))
		   'positive?        (lambda (obj) (#_positive? (obj 'value)))
		   'negative?        (lambda (obj) (#_negative? (obj 'value)))
		   'infinite?        (lambda (obj) (#_infinite? (obj 'value)))
		   'nan?             (lambda (obj) (#_nan? (obj 'value)))
		   'make-polar       (make-method #_make-polar (lambda (obj) (obj 'value)))
		   'make-rectangular (make-method #_make-rectangular (lambda (obj) (obj 'value)))
		   'make-complex     (make-method #_make-rectangular (lambda (obj) (obj 'value)))
		   'make-random-state (make-method #_make-random-state (lambda (obj) (obj 'value)))
		   'magnitude        (lambda (obj) (#_magnitude (obj 'value)))
		   'angle            (lambda (obj) (#_angle (obj 'value)))
		   'rationalize      (make-method #_rationalize (lambda (obj) (obj 'value)))
		   'abs              (lambda (obj) (#_abs (obj 'value)))
		   'exp              (lambda (obj) (#_exp (obj 'value)))
		   'log              (make-method #_log (lambda (obj) (obj 'value)))
		   'sin              (lambda (obj) (#_sin (obj 'value)))
		   'cos              (lambda (obj) (#_cos (obj 'value)))
		   'tan              (lambda (obj) (#_tan (obj 'value)))
		   'asin             (lambda (obj) (#_asin (obj 'value)))
		   'acos             (lambda (obj) (#_acos (obj 'value)))
		   'atan             (make-method #_atan (lambda (obj) (obj 'value)))
		   'sinh             (lambda (obj) (#_sinh (obj 'value)))
		   'cosh             (lambda (obj) (#_cosh (obj 'value)))
		   'tanh             (lambda (obj) (#_tanh (obj 'value)))
		   'asinh            (lambda (obj) (#_asinh (obj 'value)))
		   'acosh            (lambda (obj) (#_acosh (obj 'value)))
		   'atanh            (lambda (obj) (#_atanh (obj 'value)))
		   'sqrt             (lambda (obj) (#_sqrt (obj 'value)))
		   'expt             (make-method #_expt (lambda (obj) (obj 'value)))
		   'floor            (lambda (obj) (#_floor (obj 'value)))
		   'ceiling          (lambda (obj) (#_ceiling (obj 'value)))
		   'truncate         (lambda (obj) (#_truncate (obj 'value)))
		   'round            (lambda (obj) (#_round (obj 'value)))
		   'integer->char    (lambda (obj) (#_integer->char (obj 'value)))
		   'inexact->exact   (lambda (obj) (#_inexact->exact (obj 'value)))
		   'exact->inexact   (lambda (obj) (#_exact->inexact (obj 'value)))
		   'integer-length   (lambda (obj) (#_integer-length (obj 'value)))
		   'integer-decode-float (lambda (obj) (#_integer-decode-float (obj 'value)))
		   'number?          (lambda (obj) (#_number? (obj 'value))) ; why not #t? currently (mock-number (mock-number ...)) is an error
		   'integer?         (lambda (obj) (#_integer? (obj 'value)))
		   'real?            (lambda (obj) (#_real? (obj 'value)))
		   'complex?         (lambda (obj) (#_complex? (obj 'value)))
		   'rational?        (lambda (obj) (#_rational? (obj 'value)))
		   'exact?           (lambda (obj) (#_exact? (obj 'value)))
		   'inexact?         (lambda (obj) (#_inexact? (obj 'value)))
		   'ash              (make-method #_ash (lambda (obj) (obj 'value)))
		   'logbit?          (make-method #_logbit? (lambda (obj) (obj 'value)))
		   'number->string   (make-method #_number->string (lambda (obj) (obj 'value)))
		   'random           (make-method #_random (lambda (obj) (obj 'value)))
		   'quotient         (make-method #_quotient (lambda (obj) (obj 'value)))
		   'remainder        (make-method #_remainder (lambda (obj) (obj 'value)))
		   'modulo           (make-method #_modulo (lambda (obj) (obj 'value)))
		   'lognot           (lambda (obj) (#_lognot (obj 'value)))
		   'logior           (make-method #_logior (lambda (obj) (obj 'value)))
		   'logxor           (make-method #_logxor (lambda (obj) (obj 'value)))
		   'logand           (make-method #_logand (lambda (obj) (obj 'value)))
		   ;; any object that has lcm or gcd also needs rational?
		   'lcm              (make-method #_lcm (lambda (obj) (obj 'value)))
		   'gcd              (make-method #_gcd (lambda (obj) (obj 'value)))
		   '+                (make-method #_+ (lambda (obj) (obj 'value)))
		   '-                (make-method #_- (lambda (obj) (obj 'value)))
		   '*                (make-method #_* (lambda (obj) (obj 'value)))
		   '/                (make-method #_/ (lambda (obj) (obj 'value)))
		   ;; any object that has min or max also needs real?
		   'max              (make-method #_max (lambda (obj) (obj 'value)))
		   'min              (make-method #_min (lambda (obj) (obj 'value)))
		   '=                (make-method #_= (lambda (obj) (obj 'value)))
		   '<                (make-method #_< (lambda (obj) (obj 'value)))
		   '>                (make-method #_> (lambda (obj) (obj 'value)))
		   '<=               (make-method #_<= (lambda (obj) (obj 'value)))
		   '>=               (make-method #_>= (lambda (obj) (obj 'value)))
		   'write-byte       (lambda (byte . port) (apply #_write-byte (byte 'value) port))

		   'make-list        (lambda (ind . args) (apply #_make-list (ind 'value) args))
		   'make-vector      (make-method #_make-vector (lambda (obj) (obj 'value)))
		   'make-float-vector(make-method #_make-float-vector (lambda (obj) (obj 'value)))
		   'make-hash-table  (lambda (ind . args) (apply #_make-hash-table (ind 'value) args))
		   'make-byte-vector  (make-method #_make-byte-vector (lambda (obj) (obj 'value)))

		   'byte-vector       (make-method #_byte-vector (lambda (obj) (obj 'value)))
		   'format           (make-method #_format (lambda (obj) (obj 'value)))

		   'make-string      (lambda (ind . args) 
				       (if (mock-number? ind)
					   (apply #_make-string (ind 'value) args)
					   (error 'wrong-type-arg "make-string ~S ~S" ind args)))
		   
		   'string-ref       (lambda (str ind) 
				       (if (string? str)
					   (#_string-ref str (ind 'value))
					   (error 'wrong-type-arg "make-string ~S ~S" str ind)))

		   'string-set!      (lambda (str ind val) 
				       (if (string? str)
					   (#_string-set! str (ind 'value) val)
					   (error 'wrong-type-arg "string-set! ~S ~S ~S" str ind val)))

		   'string->number   (lambda (str radix) 
				       (if (string? str)
					   (#_string->number str (radix 'value))
					   (error 'wrong-type-arg "string->number ~S ~S" str radix)))

		   'list-ref         (lambda (lst ind) 
				       (if (pair? lst)
					   (#_list-ref lst (ind 'value))
					   (error 'wrong-type-arg "list-ref ~S ~S" lst ind)))

		   'list-set!        (lambda (lst ind val)
				       (if (pair? lst)
					   (#_list-set! lst (ind 'value) val)
					   (error 'wrong-type-arg "list-set! ~S ~S ~S" lst ind val)))

		   'list-tail        (lambda (lst ind) 
				       (if (pair? lst)
					   (#_list-tail lst (ind 'value))
					   (error 'wrong-type-arg "list-tail ~S ~S" ind args)))

		   'vector-ref       (lambda (vec ind) 
				       (if (vector? vec)
					   (#_vector-ref vec (ind 'value))
					   (error 'wrong-type-arg "vector-ref ~S ~S" vec ind)))

		   'vector-set!      (lambda (vec ind val) 
				       (if (vector? vec)
					   (#_vector-set! vec (ind 'value) val)
					   (error 'wrong-type-arg "vector-set! ~S ~S ~S" vec ind val)))

		   'make-shared-vector (lambda (obj dims offset) 
					 (if (and (vector? obj)
						  (pair? dims))
					     (#_make-shared-vector obj dims (offset 'value))
					     (error 'wrong-type-arg "make-shared-vector ~S ~S ~S" obj dims offset)))
		   
		   'vector->list     (lambda* (vec start end) 
				       (if (vector? vec)
					   ;; when there are two args, either might have triggered this method call,
					   ;;   if (not (let? end)) or (let? start), start must be a mock-number
					   ;;   and if (not (let? start)), end must be a mock-number, but
					   ;;   if (let? start) and (let? end), end might be anything
					   (range-method-1 #_vector->list vec start end)
					   (error 'wrong-type-arg "vector->list ~S ~S ~S" vec start end)))

		   'vector-fill!     (lambda* (vec c start end)
				       (if (vector? vec)
					   (range-method-2 #_vector-fill! vec c start end)
					   (error 'wrong-type-arg "vector-fill! ~S ~S ~S ~S" vec c start end)))

		   'substring        (lambda* (str start end) 
				       (if (string? str)
					   (range-method-1 #_substring start end)
					   (error 'wrong-type-arg "substring ~S ~S ~S" str start end)))

		   'string-fill!     (lambda* (str c start end)
				       (if (and (string? str)
						(char? c))
					   (range-method-2 #_string-fill! str c start end)
					   (error 'wrong-type-arg "string-fill! ~S ~S ~S ~S" str c start end)))

		   'string->list     (lambda* (str start end) 
				       (if (string? str)
					   (range-method-1 #_string->list str start end)
					   (error 'wrong-type-arg "string->list ~S ~S ~S" str start end)))

		   'write-string     (lambda* (str port start end)
				       (if (and (string? str)
						(output-port? port))
					   (range-method-2 #_write-string str port start end)
					   (error 'wrong-type-arg "write-string ~S ~S ~S ~S" str port start end)))

		   'read-string     (lambda* (k (port (current-input-port)))
				      (#_read-string (k 'value) port))

		   'copy             (lambda* (obj dest start end)
				       (if (mock-number? obj)
					   (obj 'value)
					   (if (or (mock-number? start)
						   (mock-number? end))
					       (range-method-2 #_copy obj dest start end)
					       (error 'wrong-type-arg "copy ~S ~S ~S ~S" obj dest start end))))

		   'length           (lambda (obj) #f)
		   'number?          (lambda (obj) #t)
		   'values           (lambda (obj . args) (obj 'value))
		   'class-name       'mock-number))))
      
      (define (mock-number x)
	(if (and (number? x)
		 (not (let? x)))
	    (openlet
	     (sublet (*mock-number* 'mock-number-class)
	       'value x
	       'object->string mock->string))
	    (error 'wrong-type-arg "mock-number ~S is not a number" x)))
      
      (set! mock-number? (lambda (obj)
			   (and (openlet? obj)
				(outlet-member obj mock-number-class))))
      
      (curlet))))


#|
;;; fuzzy number

(define fuzzy-number
  (let ((fuzz (lambda (fx)
		(#_* fx (#_- 1.05 (#_random .1))))))
    (lambda (fx)
      (openlet 
       (sublet 
	   (*mock-number* 'mock-number-class)
	 'let-ref-fallback (lambda (obj sym) (fuzz fx))
	 'object->string (lambda (obj . args) (#_number->string (fuzz fx))))))))


;;; interval arithmetic 
;;; 
;;; from Wikipedia:
;;; x + y =	[a+c, b+d]
;;; x - y =	[a-d, b-c]
;;; x × y =	[min(ac, ad, bc, bd), max(ac, ad, bc, bd)]
;;; x / y =	[min(a/c, a/d, b/c, b/d), max(a/c, a/d, b/c, b/d)]

(define *interval*
  (let* ((make-interval #f)
	 (low (lambda (z) (z 'low)))
	 (high (lambda (z) (z 'high)))
	 (interval-class 
	  (openlet (sublet (*mock-number* 'mock-number-class)

		     '+ (lambda args
			  (let ((lo 0)
				(hi 0))
			    (for-each
			     (lambda (z)
			       (if (let? z)
				   (begin
				     (set! lo (+ lo (low z)))
				     (set! hi (+ hi (high z))))
				   (begin
				     (set! lo (+ lo z))
				     (set! hi (+ hi z)))))
			     args)
			    (make-interval lo hi)))
		     
		     '* (lambda args
			  (let ((lo 1)
				(hi 1))
			    (for-each
			     (lambda (z)
			       (let ((zlo (if (let? z) (low z) z))
				     (zhi (if (let? z) (high z) z)))
				 (let ((ac (* lo zlo))
				       (ad (* lo zhi))
				       (bc (* hi zlo))
				       (bd (* hi zhi)))
				   (set! lo (min ac ad bc bd))
				   (set! hi (max ac ad bc bd)))))
			     args)
			    (make-interval lo hi)))
		     
		     '- (lambda args
			  (let ((z (car args)))
			    (if (null? (cdr args)) ; negate (must be let? else how did we get here?)
				(make-interval (- (high z)) (- (low z)))
				(let ((lo (low z))
				      (hi (high z)))
				  (for-each
				   (lambda (z)
				     (if (let? z)
					 (begin
					   (set! lo (- lo (high z)))
					   (set! hi (- hi (low z))))
					 (begin
					   (set! lo (- lo z))
					   (set! hi (- hi z)))))
				   (cdr args))
				  (make-interval lo hi)))))
			  
		     '/ (lambda args
			  (let ((z (car args)))
			    (if (null? (cdr args)) ; invert
				(make-interval (/ (high z)) (/ (low z)))
				(let ((lo (low z))
				      (hi (high z)))
				  (for-each
				   (lambda (z)
				     (let ((zlo (if (let? z) (low z) z))
					   (zhi (if (let? z) (high z) z)))
				       (let ((ac (/ lo zlo))
					     (ad (/ lo zhi))
					     (bc (/ hi zlo))
					     (bd (/ hi zhi)))
					 (set! lo (min ac ad bc bd))
					 (set! hi (max ac ad bc bd)))))
				   (cdr args))
				  (make-interval lo hi)))))

		     'abs (lambda (z)
			    (if (positive? (low z))
				(make-interval (low z) (high z))
				(if (negative? (high z))
				    (make-interval (abs (high z)) (abs (low z)))
				    (make-interval 0 (max (abs (low z)) (abs (high z)))))))
			  
		     'object->string (lambda (obj . args) 
				       (format #f "#<interval: ~S ~S>" (low obj) (high obj)))
		     ))))
    
    (set! make-interval (lambda (low high)
			  (if (> low high) (format *stderr* "~A ~A~%" low high))
			  (openlet (sublet interval-class 'low low 'high high))))

    (curlet)))

(define x ((*interval* 'make-interval) 3.0 4.0))
|#



;;; --------------------------------------------------------------------------------

(define *mock-pair*
  (let ((mock-pair? #f))
    (let ((mock-pair-class
	   (openlet
	    (inlet 'morally-equal?   (lambda (x y) (#_morally-equal? (x 'value) y))
		   'pair-line-number (lambda (obj) (#_pair-line-number (obj 'value)))
		   'list->string     (lambda (obj) (#_list->string (obj 'value)))
		   'object->string   (lambda (obj . arg) "#<mock-pair-class>")
		   'list?            (lambda (obj) (#_list? (obj 'value)))
		   'car              (lambda (obj) (#_car (obj 'value)))
		   'cdr              (lambda (obj) (#_cdr (obj 'value)))
		   'set-car!         (lambda (obj val) (#_set-car! (obj 'value) val))
		   'set-cdr!         (lambda (obj val) (#_set-cdr! (obj 'value) val))
		   'caar             (lambda (obj) (#_caar (obj 'value)))
		   'cadr             (lambda (obj) (#_cadr (obj 'value)))
		   'cdar             (lambda (obj) (#_cdar (obj 'value)))
		   'cddr             (lambda (obj) (#_cddr (obj 'value)))
		   'caaar            (lambda (obj) (#_caaar (obj 'value)))
		   'caadr            (lambda (obj) (#_caadr (obj 'value)))
		   'cadar            (lambda (obj) (#_cadar (obj 'value)))
		   'cdaar            (lambda (obj) (#_cdaar (obj 'value)))
		   'caddr            (lambda (obj) (#_caddr (obj 'value)))
		   'cdddr            (lambda (obj) (#_cdddr (obj 'value)))
		   'cdadr            (lambda (obj) (#_cdadr (obj 'value)))
		   'cddar            (lambda (obj) (#_cddar (obj 'value)))
		   'caaaar           (lambda (obj) (#_caaaar (obj 'value)))
		   'caaadr           (lambda (obj) (#_caaadr (obj 'value)))
		   'caadar           (lambda (obj) (#_caadar (obj 'value)))
		   'cadaar           (lambda (obj) (#_cadaar (obj 'value)))
		   'caaddr           (lambda (obj) (#_caaddr (obj 'value)))
		   'cadddr           (lambda (obj) (#_cadddr (obj 'value)))
		   'cadadr           (lambda (obj) (#_cadadr (obj 'value)))
		   'caddar           (lambda (obj) (#_caddar (obj 'value)))
		   'cdaaar           (lambda (obj) (#_cdaaar (obj 'value)))
		   'cdaadr           (lambda (obj) (#_cdaadr (obj 'value)))
		   'cdadar           (lambda (obj) (#_cdadar (obj 'value)))
		   'cddaar           (lambda (obj) (#_cddaar (obj 'value)))
		   'cdaddr           (lambda (obj) (#_cdaddr (obj 'value)))
		   'cddddr           (lambda (obj) (#_cddddr (obj 'value)))
		   'cddadr           (lambda (obj) (#_cddadr (obj 'value)))
		   'cdddar           (lambda (obj) (#_cdddar (obj 'value)))
		   'assoc            (lambda (val obj . args) (apply #_assoc val (obj 'value) args))
		   (reader-cond ((not (provided? 'pure-s7))
		   'assq             (lambda (val obj) (#_assq val (obj 'value)))
		   'assv             (lambda (val obj) (#_assv val (obj 'value)))
		   'memq             (lambda (val obj) (#_memq val (obj 'value)))
		   'memv             (lambda (val obj) (#_memv val (obj 'value)))))
		   'member           (lambda (val obj . args) (apply #_member val (obj 'value) args))
		   'let-ref          (lambda (obj ind) (coverlet obj) (let ((val (#_list-ref (obj 'value) ind))) (openlet obj) val))
		   'let-set!         (lambda (obj ind val) (coverlet obj) (#_list-set! (obj 'value) ind val) (openlet obj) val)
		   'arity            (lambda (obj) (#_arity (obj 'value)))
		   'fill!            (lambda (obj val) (#_fill! (obj 'value) val))
		   'reverse          (lambda (obj) (#_reverse (obj 'value)))
		   'reverse!         (lambda (obj) (set! (obj 'value) (#_reverse (obj 'value))))
		   'sort!            (lambda (obj f) (#_sort! (obj 'value) f))
		   'make-iterator    (lambda (obj) (#_make-iterator (obj 'value)))
		   'eval             (lambda (f obj) (#_eval (obj 'value)))
		   'list->vector     (lambda (obj) (#_list->vector (obj 'value)))

		   'list-tail        (lambda (obj . args) 
				       (if (mock-pair? obj)
					   (apply #_list-tail (obj 'value) args)
					   (error 'wrong-type-arg "list-tail ~S ~S" obj args)))

		   'copy             (lambda (obj . args) 
				       (if (mock-pair? obj)
					   (apply #_copy (obj 'value) args)
					   (error 'wrong-type-arg "copy ~S ~S" obj args)))
		   
		   'make-shared-vector (lambda (obj dims . args) 
					 (if (mock-pair? dims)
					     (apply #_make-shared-vector obj (dims 'value) args)
					     (error 'wrong-type-arg "make-shared-vector ~S ~S ~S" obj dims args)))

		   'make-vector      (lambda (obj dims . args) 
				       (if (mock-pair? dims)
					   (apply #_make-vector obj (dims 'value) args)
					   (error 'wrong-type-arg "make-vector ~S ~S ~S" obj dims args)))

		   'append           (make-method #_append (lambda (obj) (obj 'value)))

		   'list-ref         (lambda (obj ind) 
				       (if (mock-pair? obj)
					   (#_list-ref (obj 'value) ind)
					   (error 'wrong-type-arg "list-ref ~S ~S" obj ind)))

		   'list-set!        (lambda (obj ind val) 
				       (if (mock-pair? obj)
					   (#_list-set! (obj 'value) ind val)
					   (error 'wrong-type-arg "list-set! ~S ~S ~S" obj ind val)))

		   'pair?            (lambda (obj) #t)
		   'length           (lambda (obj) (#_length (obj 'value)))
		   'values           (lambda (obj . args) (obj 'value))
		   'class-name       'mock-pair))))
      
      (define (mock-pair . args)
	(openlet
	 (sublet (*mock-pair* 'mock-pair-class)
	   'value (copy args)
	   'object->string mock->string)))
      
      (set! mock-pair? (lambda (obj)
			 (and (openlet? obj)
			      (outlet-member obj mock-pair-class))))
      
      (curlet))))

#|
(let ((immutable-list-class 
       (sublet (*mock-pair* 'mock-pair-class)
	 'object->string   mock->string

	 'let-set!         (lambda (obj i val) 
			     (set! (obj 'value) (append (copy (obj 'value) (make-list (+ i 1))) (list-tail (obj 'value) (+ i 1))))
			     (list-set! (obj 'value) i val))
	 'list-set!        (lambda (obj i val) 
			     (set! (obj 'value) (append (copy (obj 'value) (make-list (+ i 1))) (list-tail (obj 'value) (+ i 1))))
			     (list-set! (obj 'value) i val))
	 'set-car!         (lambda (obj val) 
			     (set! (obj 'value) (cons val (cdr (obj 'value)))))
	 'set-cdr!         (lambda (obj val) 
			     (set! (obj 'value) (cons (car (obj 'value)) val)))
	 'fill!            (lambda (obj val) 
			     (set! (obj 'value) (fill! (copy (obj 'value)) val)))
	 'reverse!         (lambda (obj) 
			     (set! (obj 'value) (reverse (obj 'value))))
	 'sort!            (lambda (obj func) 
			     (set! (obj 'value) (sort! (copy (obj 'value)) func))))))

    (define (immutable-list lst)
      (openlet 
       (sublet immutable-list-class
	 'value lst))))
|#

;;; since a mock-pair prints itself as if a list, you can get some strange printout results:
;;;    (cons 'a ((*mock-pair* 'mock-pair) 'b 'c)) -> '(a . (b c))



;;; --------------------------------------------------------------------------------

(define *mock-symbol*
  (let ((mock-symbol-class
	 (openlet
	  (inlet 'object->string        (lambda (obj . arg) "#<mock-symbol-class>")
		 'morally-equal?        (lambda (x y) (#_morally-equal? (x 'value) y))
		 'gensym?               (lambda (obj) (#_gensym? (obj 'value)))
		 'symbol->string        (lambda (obj) (#_symbol->string (obj 'value)))
		 'symbol->value         (lambda (obj . args) (apply #_symbol->value (obj 'value) args))
		 'symbol->dynamic-value (lambda (obj) (#_symbol->dynamic-value (obj 'value)))
		 'symbol-access         (lambda (obj . args) (apply #_symbol-access (obj 'value) args))
		 'provided?             (lambda (obj) (#_provided? (obj 'value)))
		 'provide               (lambda (obj) (#_provide (obj 'value)))
		 'defined?              (lambda (obj) (#_defined? (obj 'value)))
		 'symbol->keyword       (lambda (obj) (#_symbol->keyword (obj 'value)))
		 'keyword?              (lambda (obj) (#_keyword? (obj 'value)))
		 'keyword->symbol       (lambda (obj) (#_keyword->symbol (obj 'value)))
		 'format                (lambda (str s . args) (#_symbol->string s))
		 'symbol?               (lambda (obj) #t)
		 'class-name            'mock-symbol
		 'values                (lambda (obj . args) (obj 'value))
		 ))))

    (define (mock-symbol s)
      (if (and (symbol? s)
	       (not (let? s)))
	  (openlet
	   (sublet (*mock-symbol* 'mock-symbol-class)
	     'value s
	     'object->string mock->string))
	  (error 'wrong-type-arg "mock-symbol ~S is not a symbol" s)))
    
    (define (mock-symbol? obj)
      (and (openlet? obj)
	   (outlet-member obj mock-symbol-class)))

    (curlet)))


;;; --------------------------------------------------------------------------------

(define *mock-port*
  (let ((mock-port? #f))
    (let ((mock-port-class
	   (openlet
	    (inlet 'port?               (lambda (obj) #t)
		   'morally-equal?      (lambda (x y) (#_morally-equal? (x 'value) y))
		   'close-input-port    (lambda (obj) (#_close-input-port (obj 'value)))
		   'close-output-port   (lambda (obj) (#_close-output-port (obj 'value)))
		   'flush-output-port   (lambda (obj) (#_flush-output-port (obj 'value)))
		   'get-output-string   (lambda (obj) (#_get-output-string (obj 'value)))
		   'newline             (lambda (obj) (#_newline (obj 'value)))
		   'write               (lambda (x obj) (#_write x (obj 'value)))
		   'display             (lambda (x obj) (#_display x (obj 'value)))
		   'read-char           (lambda (obj) (#_read-char (obj 'value)))
		   'peek-char           (lambda (obj) (#_peek-char (obj 'value)))
		   'read-byte           (lambda (obj) (#_read-byte (obj 'value)))
		   'read-line           (lambda (obj . args) (apply #_read-line (obj 'value) args))
		   'read                (lambda (obj) (#_read (obj 'value)))
		   'input-port?         (lambda (obj) (#_input-port? (obj 'value)))
		   'output-port?        (lambda (obj) (#_output-port? (obj 'value)))
		   'port-closed?        (lambda (obj) (#_port-closed? (obj 'value)))
		   'char-ready?         (lambda (obj) (#_char-ready? (obj 'value)))
		   'port-line-number    (lambda (obj) (#_port-line-number (obj 'value)))
		   'port-filename       (lambda (obj) (#_port-filename (obj 'value)))
		   'object->string      (lambda (obj . args) "#<mock-port-class>")
		   'format              (make-method #_format (lambda (obj) (obj 'value)))

		   'set-current-output-port (lambda (obj) (#_set-current-output-port (obj 'value)))
		   'set-current-input-port  (lambda (obj) (#_set-current-input-port (obj 'value)))
		   'set-current-error-port  (lambda (obj) (#_set-current-error-port (obj 'value)))
		   
		   'write-char          (lambda (c obj) 
					  (if (mock-port? obj)
					      (#_write-char c (obj 'value))
					      (error 'wrong-type-arg "write-char ~S ~S" c obj)))

		   'write-string        (lambda (s obj . args) 
					  (if (mock-port? obj)
					      (apply #_write-string s (obj 'value) args)
					      (error 'wrong-type-arg "write-string ~S ~S ~S" s obj args)))

		   'write-byte          (lambda (b obj) 
					  (if (mock-port? obj)
					      (#_write-byte b (obj 'value))
					      (error 'wrong-type-arg "write-byte ~S ~S" b obj)))
		   
		   'read-string         (lambda (k obj) 
					  (if (mock-port? obj)
					      (#_read-string k (obj 'value))
					      (error 'wrong-type-arg "read-string ~S ~S" k obj)))
		   'values              (lambda (obj . args) (obj 'value))
		   'class-name          'mock-port
		   ))))
      
      (define (mock-port port)
	(if (and (or (input-port? port)
		     (output-port? port))
		 (not (let? port)))
	    (openlet
	     (sublet (*mock-port* 'mock-port-class)
	       'value port
	       'object->string mock->string))
	    (error 'wrong-type-arg "mock-port ~S is not a port" port)))
      
      (set! mock-port? (lambda (obj)
			 (and (openlet? obj)
			      (outlet-member obj mock-port-class))))
      
      (curlet))))

;;; sublet of any of these needs to include the value field or a let-ref-fallback
#|
(require libc.scm)

(define *input-file*
  (let ((file-write-date (lambda (file)
			   (with-let (sublet *libc* :file file)
			     (let ((buf (stat.make)))
			       (stat file buf)
			       (let ((date (stat.st_mtime buf)))
				 (free buf)
				 date)))))
	(file-size (lambda (file)
		      (with-let (sublet *libc* :file file)
			(let ((buf (stat.make)))
			  (stat file buf)
			  (let ((size (stat.st_size buf)))
			    (free buf)
			    size)))))
	(file-owner (lambda (file)
		      (with-let (sublet *libc* :file file)
			(let ((buf (stat.make)))
			  (stat file buf)
			  (let ((uid (stat.st_uid buf)))
			    (free buf)
			    (let ((pwd (getpwuid uid)))
			      (passwd.pw_name pwd))))))))
    (openlet
     (sublet (*mock-port* 'mock-port-class)
       'value      #f
       'length     (lambda (obj) (file-size (obj 'file-name)))
       'owner      (lambda (obj) (file-owner (obj 'file-name)))
       'write-date (lambda (obj) (file-write-date (obj 'file-name)))))))

(define (open-a-file file)
  (let ((p (openlet
	    (sublet *input-file* 
	      'file-name file))))
    (set! (p 'value) (open-input-file "oboe.snd"))
    p))

(define p (open-a-file "oboe.snd"))
(length p) -> 101684
((p 'owner) p) -> "bil"
|#

