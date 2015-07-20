(provide 'cload.scm)

;;; --------------------------------------------------------------------------------
;;; automatically link a C function into s7 (there are a bunch of examples below)
;;;     (c-define '(double j0 (double)) "m" "math.h")
;;; means link the name m:j0 to the math library function j0 passing a double arg and getting a double result (reals in s7)
;;;
;;; (c-define c-info prefix headers cflags ldflags)
;;;    prefix is some arbitrary prefix (it can be "") that you want prepended to various names.
;;;    headers is a list of headers (as strings) that the c-info relies on, (("math.h") for example).
;;;    cflags are any special C compiler flags that are needed ("-I." in particular).
;;;    ldflags is the similar case for the loader.
;;;    c-info is a list that describes the C entities that you want to tie into s7.
;;;       it can be either one list describing one entity, or a list of such lists.
;;;       Each description has the form: (return-type entity-name-in-C (argument-type...))
;;;       where each entry is a symbol, and C names are used throughout.  So, in the j0
;;;       example above, (double j0 (double)) says we want access to j0, it returns
;;;       a C double, and takes one argument, also a C double.  s7 tries to figure out 
;;;       what the corresponding s7 type is, but in tricky cases, you should tell it
;;;       by replacing the bare type name with a list: (C-type underlying-C-type).  For example,
;;;       the Snd function set_graph_style takes an (enum) argument of type graph_style_t.
;;;       This is actually an int, so we use (graph_style_t int) as the type:
;;;         (void set_graph_style ((graph_style_t int)))
;;;       If the C entity is a constant, then the descriptor list has just two entries,
;;;       the C-type and the entity name: (int F_OK) for example. The entity name can also be a list 
;;;       (an enum listing for example).
;;;       If the C type has a space ("struct tm*" for example), use (symbol "struct tm*") 
;;;       to construct the corresponding symbol.
;;;    The entity is placed in the current s7 environment under the name (string-append prefix ":" name)
;;;    where the ":" is omitted if the prefix is null.  So in the j0 example, we get in s7 the function m:j0.
;;;
;;; some examples:
;;;
;;;  (c-define '((double j0 (double)) 
;;;              (double j1 (double)) 
;;;              (double erf (double)) 
;;;              (double erfc (double))
;;;              (double lgamma (double)))
;;;             "m" "math.h")
;;; 
;;;
;;; (c-define '(char* getenv (char*)))
;;; (c-define '(int setenv (char* char* int)))
;;; (define get-environment-variable (let () (c-define '(char* getenv (char*))) getenv))
;;;
;;; (define file-exists? (let () (c-define '((int F_OK) (int access (char* int))) "" "unistd.h") (lambda (arg) (= (access arg F_OK) 0))))
;;; (define delete-file (let () (c-define '(int unlink (char*)) "" "unistd.h") (lambda (file) (= (unlink file) 0)))) ; 0=success, -1=failure
;;;
;;;
;;; these pick up Snd stuff:
;;;   (c-define '(char* version_info ()) "" "snd.h" "-I.")
;;;   (c-define '(mus_float_t mus_degrees_to_radians (mus_float_t)) "" "snd.h" "-I.")
;;;
;;;   (c-define '(snd_info* any_selected_sound ()) "" "snd.h" "-I.")
;;;   (c-define '(void select_channel (snd_info* int)) "" "snd.h" "-I.")
;;;   (c-define '(((graph_style_t int) (GRAPH_LINES GRAPH_DOTS GRAPH_FILLED GRAPH_DOTS_AND_LINES GRAPH_LOLLIPOPS)) 
;;;               (void set_graph_style ((graph_style_t int)))) 
;;;             "" "snd.h" "-I.")
;;;   
;;;
;;;  (c-define '(char* getcwd (char* size_t)) "" "unistd.h")
;;;    :(let ((str (make-string 32))) (getcwd str 32) str)
;;;    "/home/bil/cl\x00                   "
;;;    so it works in a sense -- there is a memory leak here
;;; 
;;;
;;; (c-define (list '(void* calloc (size_t size_t))
;;;		    '(void* malloc (size_t))
;;;		    '(void free (void*))
;;;		    '(void* realloc(void* size_t))
;;;		    '(void time (time_t*)) ; ignore returned value
;;;		    (list (symbol "struct tm*") 'localtime '(time_t*))
;;;                 (list 'size_t 'strftime (list 'char* 'size_t 'char* (symbol "struct tm*"))))
;;;          "" "time.h")
;;;   > (let ((p (calloc 1 8)) (str (make-string 32))) (time p) (strftime str 32 "%a %d-%b-%Y %H:%M %Z" (localtime p)) (free p) str)
;;;   "Sat 11-Aug-2012 08:55 PDT\x00      "
;;;
;;;
;;; (c-define '((int closedir (DIR*))
;;; 	        (DIR* opendir (char*))
;;; 		(in-C "static char *read_dir(DIR *p) \
;;;                    {                             \
;;;                      struct dirent *dirp;         \
;;;                      dirp = readdir(p);           \
;;;                      if (!dirp) return(NULL);     \
;;;                      else return(dirp->d_name);   \
;;;                     }")
;;; 	        (char* read_dir (DIR*)))
;;;   "" '("sys/types.h" "dirent.h"))
;;; 
;;; (let ((dir (opendir "/home/bil/gtk-snd")))
;;;   (do ((p (read_dir dir) (read_dir dir)))
;;;       ((= (length p) 0))
;;;     (format *stderr* "~A " p))
;;;   (closedir dir))
;;;
;;; (define (memory-usage)
;;;   (let ((v (rusage.make))) 
;;;     (getrusage (*libc* 'RUSAGE_SELF) v)
;;;     (let ((mem (rusage.ru_maxrss v))) 
;;;       (free v) 
;;;       (* 1024 mem))))
;;; --------------------------------------------------------------------------------

(define *cload-cflags* "") 
(define *cload-ldflags* "") 


(define-macro (defvar name value) 
  `(if (not (defined? ',name)) 
       (define ,name ,value)))

(defvar c-define-output-file-counter 0)   ; ugly, but I can't find a way around this (dlopen/dlsym stupidity)


;;; to place the new function in the caller's current environment, we need to pass the environment in explicitly:
(define-macro (c-define . args) 
  `(c-define-1 (curlet) ,@args))


(define* (c-define-1 cur-env function-info (prefix "") (headers ()) (cflags "") (ldflags "") output-name)
  ;; write a C shared library module that links in the functions in function-info
  ;;    function info is either a list: (return-type c-name arg-type) or a list thereof
  ;;    the new functions are placed in cur-env

  (define collides?
    (let ((all-names ()))
      (lambda (name)
	(if (member name all-names eq?) 
	    (format *stderr* "~A twice?~%" name)
	    (set! all-names (cons name all-names)))
	name)))

  (define handlers (list '(integer s7_is_integer s7_integer s7_make_integer s7_int)
			 '(boolean s7_is_boolean s7_boolean s7_make_boolean bool)
			 '(real s7_is_real s7_number_to_real s7_make_real s7_double)

			 ;; '(complex s7_is_complex #f s7_make_complex s7_Complex)
			 ;; the typedef is around line 6116 in s7.c, but we also need s7_complex which requires the s7_Complex type
			 ;; xen.h uses (s7_real_part(a) + s7_imag_part(a) * _Complex_I) instead since c++ won't let use define s7_Complex in s7.h

			 '(string s7_is_string s7_string s7_make_string char*)
			 (list 'character 's7_is_character 's7_character 's7_make_character (symbol "unsigned char"))
			 '(c_pointer s7_is_c_pointer s7_c_pointer s7_make_c_pointer void*)
			 '(s7_pointer #f #f #f s7_pointer)
			 ))

  (define (find-handler handle choice)
    (let ((found (assoc handle handlers eq?)))
      (if (pair? found)
	  (choice found)
	  #t)))
  

  (define (C-type->s7-type type)

    (if (pair? type)                             ; in case the type name does not make its C type obvious: (graph_style_t int)
	(symbol->string (cadr type))
	(let ((type-name (symbol->string type)))
	  (cond ((string-position "**" type-name)     ; any complicated C pointer is uninterpreted
		 'c_pointer)

		((string=? "s7_pointer" type-name)
		 's7_pointer)
		
		((string-position "char*" type-name)  ; but not char** (caught above)
		 'string)

		((or (string-position "*" type-name)  ; float* etc
		     (string-position "pointer" type-name))
		 'c_pointer)

		((string-position "char" type-name)
		 'character)

		((string-position "bool" type-name) 
		 'boolean)
		
		;; ((string-position "complex" type-name)
		;;  'complex)                              ; double complex or complex double (mus_edot_product in clm.c uses the latter)

		((or (string-position "float" type-name) 
		     (string-position "double" type-name)) 
		 'real)

		((or (string-position "int" type-name) 
		     (string-position "long" type-name) ; assuming not "long double" here so we need to look for it first (above)
		     (string-position "short" type-name) 
		     (string-position "size" type-name)
		     (string-position "byte" type-name)) 
		 'integer)

		(#t #t)))))

  (define (C->s7-cast type)
    (find-handler (C-type->s7-type type) (lambda (p) (car (cddddr p)))))
    
  (define (C->s7 type)
    (find-handler (C-type->s7-type type) cadddr))
    
  (define (s7->C type)
    (find-handler (C-type->s7-type type) caddr))

  (define (checker type)
    (find-handler (C-type->s7-type type) cadr))

  (set! c-define-output-file-counter (+ c-define-output-file-counter 1))

  (let* ((file-name (or output-name (format "temp-s7-output-~D" c-define-output-file-counter)))
	 (c-file-name (string-append file-name ".c"))
	 (o-file-name (string-append file-name ".o"))
	 (so-file-name (string-append file-name ".so"))
	 (init-name (if output-name
			(string-append output-name "_init")
			(string-append "init_" (number->string c-define-output-file-counter))))
	 (functions ())
	 (constants ())
	 (macros ())     ; these are protected by #ifdef ... #endif
	 (inits ())      ; C code (a string in s7) inserted in the library initialization function
	 (p #f))

    (define (initialize-c-file)
      ;; C header stuff
      (set! p (open-output-file c-file-name))
      (format p "#include <stdlib.h>~%")
      (format p "#include <stdio.h>~%")
      (format p "#include <string.h>~%")
      (if (string? headers)
	  (format p "#include <~A>~%" headers)
	  (for-each
	   (lambda (header)
	     (format p "#include <~A>~%" header))
	   headers))
      (format p "#include \"s7.h\"~%~%"))
  

    (define* (add-one-function return-type name arg-types doc)
      ;; (format *stderr* "~A ~A ~A~%" return-type name arg-types)
      ;; C function -> scheme
      (let* ((func-name (symbol->string (collides? name)))
	     (num-args (length arg-types))
	     (base-name (string-append (if (> (length prefix) 0) prefix "s7_dl") "_" func-name)) ; not "g" -- collides with glib
	     (scheme-name (string-append prefix (if (> (length prefix) 0) ":" "") func-name)))

	(if (and (= num-args 1) 
		 (eq? (car arg-types) 'void))
	    (set! num-args 0))
	(format p "static s7_pointer ~A(s7_scheme *sc, s7_pointer args)~%" base-name)
	(format p "{~%")
	
	;; get the Scheme args, check their types, assign to local C variables
	(if (positive? num-args)
	    (begin
	      (format p "  s7_pointer arg;~%")
	      (do ((i 0 (+ i 1))
		   (type arg-types (cdr type)))
		  ((= i num-args))
		(format p "  ~A ~A_~D;~%" (if (pair? (car type)) (caar type) (car type)) base-name i))
	      (format p "  arg = args;~%")
	      (do ((i 0 (+ i 1))
		   (type arg-types (cdr type)))
		  ((= i num-args))

		(let* ((nominal-type (if (pair? (car type)) (caar type) (car type)))
		       (true-type (if (pair? (car type)) (cadar type) (car type)))
		       (s7-type (C-type->s7-type true-type)))
		  (if (eq? true-type 's7_pointer)
		      (format p "    ~A_~D = s7_car(arg);~%" base-name i)
		      (begin
			(format p "  if (~A(s7_car(arg)))~%" (checker true-type))
			(format p "    ~A_~D = (~A)~A(~As7_car(arg));~%"
				base-name i
				nominal-type
				(s7->C true-type)
				(if (member s7-type '(boolean real) eq?)
				    "sc, " ""))
			(format p "  else return(s7_wrong_type_arg_error(sc, ~S, ~D, s7_car(arg), ~S));~%"
				func-name 
				(if (= num-args 1) 0 (+ i 1))
				(if (symbol? s7-type) 
				    (symbol->string s7-type) 
				    (error 'bad-arg (format #f "in ~S, ~S is not a symbol~%" name s7-type))))))
		  (if (< i (- num-args 1))
		      (format p "  arg = s7_cdr(arg);~%"))))))
	
	;; return C value to Scheme
	(if (pair? return-type) 
	    (set! return-type (cadr return-type)))
	(let ((return-translator (C->s7 return-type)))
	  (format p "  ")
	  (if (not (eq? return-translator #t))
	      (format p "return("))
	  (if (symbol? return-translator)
	      (format p "~A(sc, (~A)" return-translator (C->s7-cast return-type)))
	  (format p "~A(" func-name)
	  (do ((i 0 (+ i 1)))
	      ((>= i (- num-args 1)))
	    (format p "~A_~D, " base-name i))
	  (if (positive? num-args)
	      (format p "~A_~D" base-name (- num-args 1)))
	  (format p ")")
	  (if (symbol? return-translator)
	      (format p ")"))
	  (if (not (eq? return-translator #t))
	      (format p ");~%")
	      (format p ";~%  return(s7_unspecified(sc));~%"))
	  (format p "}~%~%"))

	(set! functions (cons (list scheme-name base-name (or doc func-name) num-args) functions))))

    
    (define (add-one-constant type name)
      ;; C constant -> scheme
      (let ((c-type (if (pair? type) (cadr type) type)))
	(if (symbol? name)
	    (set! constants (cons (list c-type (symbol->string (collides? name))) constants))
	    (for-each 
	     (lambda (c)
	       (set! constants (cons (list c-type (symbol->string (collides? c))) constants)))
	     name))))


    (define (add-one-macro type name)
      ;; C macro (with definition check) -> scheme
      (let ((c-type (if (pair? type) (cadr type) type)))
	(if (symbol? name)
	    (set! macros (cons (list c-type (symbol->string (collides? name))) macros))
	    (for-each 
	     (lambda (c)
	       (set! macros (cons (list c-type (symbol->string (collides? c))) macros)))
	     name))))

  
    (define (end-c-file)
      ;; now the init function
      ;;   the new scheme variables and functions are placed in the current environment

      (format p "void ~A(s7_scheme *sc);~%" init-name)
      (format p "void ~A(s7_scheme *sc)~%" init-name)
      (format p "{~%")
      (format p "  s7_pointer cur_env;~%")
      (format p "  cur_env = s7_outlet(s7_curlet(sc));~%") ; this must exist because we pass load the env ourselves
      
      ;; send out any special initialization code
      (for-each
       (lambda (init-str)
	 (format p "  ~A~%" init-str))
       (reverse inits))

      ;; "constants" -- actually variables in s7 because we want them to be local to the current environment
      (if (pair? constants)
	  (begin
	    (format p "~%")
	    (for-each
	     (lambda (c)
	       (let* ((type (c 0))
		      (c-name (c 1))
		      (scheme-name (string-append prefix (if (> (length prefix) 0) ":" "") c-name)))
		 (format p "  s7_define(sc, cur_env, s7_make_symbol(sc, ~S), ~A(sc, (~A)~A));~%" 
			 scheme-name
			 (C->s7 type)
			 (C->s7-cast type)
			 c-name)))
	     constants)))
      
      ;; C macros -- need #ifdef name #endif wrapper
      (if (pair? macros)
	  (begin
	    (format p "~%")
	    (for-each
	     (lambda (c)
	       (let* ((type (c 0))
		      (c-name (c 1))
		      (scheme-name (string-append prefix (if (> (length prefix) 0) ":" "") c-name)))
		 (format p "#ifdef ~A~%" c-name)
		 (format p "  s7_define(sc, cur_env, s7_make_symbol(sc, ~S), ~A(sc, (~A)~A));~%" 
			 scheme-name
			 (C->s7 type)
			 (C->s7-cast type)
			 c-name)
		 (format p "#endif~%")))
	     macros)))
      
      ;; functions
      (for-each
       (lambda (f)
	 (let ((scheme-name (f 0))
	       (base-name   (f 1))
	       (help        (f 2))
	       (num-args    (f 3))
	       (opt-args    (if (= (length f) 5) (f 4) 0)))
	   (format p "~%  s7_define(sc, cur_env,~%            s7_make_symbol(sc, ~S),~%" scheme-name)
	   (format p "            s7_make_safe_function(sc, ~S, ~A, ~D, ~D, false, ~S));~%"
		   scheme-name
		   base-name
		   num-args
		   opt-args
		   help)))
       functions)
      
      (format p "}~%")
      (close-output-port p)
      
      ;; now we have the module .c file -- make it into a shared object, load it, delete the temp files
      
      (if (provided? 'osx)
	  (begin
	    ;; I assume the caller is also compiled with these flags?
	    (system (format #f "gcc -c ~A ~A" 
			    c-file-name (string-append *cload-cflags* " " cflags)))
	    (system (format #f "gcc ~A -o ~A -dynamic -bundle -undefined suppress -flat_namespace ~A" 
			    o-file-name so-file-name (string-append *cload-ldflags* " " ldflags))))

	  (if (provided? 'freebsd)
	      (begin
		(system (format #f "cc -fPIC -c ~A ~A" 
				c-file-name (string-append *cload-cflags* " " cflags)))
		(system (format #f "cc ~A -shared -o ~A ~A" 
				o-file-name so-file-name (string-append *cload-ldflags* " " ldflags))))

	      (if (provided? 'openbsd)
		  (begin
		    (system (format #f "cc -fPIC -ftrampolines -c ~A ~A" 
				    c-file-name (string-append *cload-cflags* " " cflags)))
		    (system (format #f "cc ~A -shared -o ~A ~A" 
				    o-file-name so-file-name (string-append *cload-ldflags* " " ldflags))))

		  (begin
		    (system (format #f "gcc -fPIC -c ~A ~A" 
				    c-file-name (string-append *cload-cflags* " " cflags)))
		    (system (format #f "gcc ~A -shared -o ~A ~A" 
				    o-file-name so-file-name (string-append *cload-ldflags* " " ldflags))))))))


    (define (handle-declaration func)
      ;; functions
      (if (>= (length func) 3)
	  (apply add-one-function func)
	  (case (car func)
	    ((in-C)       (format p "~A~%" (cadr func)))
	    ((C-init)     (set! inits (cons (cadr func) inits)))
	    ((C-macro)    (apply add-one-macro (cadr func)))
	    ((C-function) (collides? (caadr func)) (set! functions (cons (cadr func) functions)))
	    (else         (apply add-one-constant func)))))


    ;; this is the body of c-define
    (if (or (not output-name)
	    (not (file-exists? c-file-name))
	    (not (file-exists? so-file-name))
	    (not (provided? 'system-extras))
	    (< (file-mtime so-file-name) (file-mtime c-file-name))   ; they are equal on my linux system
	    (and (file-exists? (port-filename (current-input-port))) ; we're actually loading a file
		 (< (file-mtime so-file-name) (file-mtime (port-filename (current-input-port))))))
	(begin
	  (format *stderr* "writing ~A~%" c-file-name)
	  ;; write a new C file and compile it
	  (initialize-c-file)

	  (if (and (pair? (cdr function-info))
		   (symbol? (cadr function-info)))
	      (handle-declaration function-info)
	      (for-each handle-declaration function-info))
	  
	  (end-c-file)
	  (delete-file o-file-name)))

    ;; load the obejct file, clean up
    (let ((new-env (sublet cur-env 'init_func (string->symbol init-name))))
      (format *stderr* "loading ~A~%" so-file-name)
      (load so-file-name new-env))))



;;; backwards compatibility
(define define-c-function c-define)


#|
(let ((cd (symbol "complex double"))
      (cd* (symbol "complex double *")))
  (c-define (list cd 'mus_edot_product (list cd cd* 'int))))

;complex double mus_edot_product(complex double freq, complex double *data, mus_long_t size)
|#
