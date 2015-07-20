#ifndef S7_H
#define S7_H

#define S7_VERSION "3.31"
#define S7_DATE "16-Jul-15"


typedef long long int s7_int;
/* This sets the size of integers in Scheme; it needs to be big enough to accomodate a C pointer.
 */

typedef double s7_double;
/* similarly for Scheme reals; only "double" works in C++
 */

/* s7 itself is based on the types and functions in this file, so the first place to look for examples
 *   is s7.c.  There are also a few variations on a REPL at the end of s7.html.  s7test.scm
 *   is a regression test for s7 -- it still turns up a few problems.  More tests are certainly welcome!  
 *   Extended examples of s7 usage are:
 *
 *   Snd: ftp://ccrma-ftp.stanford.edu/pub/Lisp/snd-15.tar.gz (a sound editor)
 *     which includes:
 *       libxm: libxm.tar.gz (X, Motif, Gtk, Cairo, and openGL bindings)
 *       sndlib: sndlib.tar.gz (sound file, audio port, and CLM bindings plus an optimizer (run))
 *
 *   Common Music/Grace by Rick Taube: http://camil.music.uiuc.edu/Software/grace/downloads/cm3.tar.gz (composition)
 *     which can use sndlib -- see Snd's grfsnd.html or the cmdist archives for details
 */

/* old forms... */
typedef s7_int s7_Int;
typedef s7_double s7_Double;


#include <stdio.h>

#ifndef __cplusplus
#ifndef _MSC_VER
  #include <stdbool.h>
#else
#ifndef true
  #define bool	int
  #define true	1
  #define false	0
  /* this fallback code does not actually work -- we need stdbool.h */
#endif
#endif
#endif


#ifdef __cplusplus
extern "C" {
#endif

typedef struct s7_scheme s7_scheme;
typedef struct s7_cell *s7_pointer;

s7_scheme *s7_init(void);

  /* s7_scheme is our interpreter
   * s7_pointer is a Scheme object of any (Scheme) type
   * s7_init creates the interpreter.
   */

typedef s7_pointer (*s7_function)(s7_scheme *sc, s7_pointer args);   /* that is, obj = func(s7, args) -- args is a list of arguments */

s7_pointer s7_f(s7_scheme *sc);                                      /* #f */
s7_pointer s7_t(s7_scheme *sc);                                      /* #t */
s7_pointer s7_nil(s7_scheme *sc);                                    /* () */
s7_pointer s7_undefined(s7_scheme *sc);                              /* #<undefined> */
s7_pointer s7_unspecified(s7_scheme *sc);                            /* #<unspecified> */
bool s7_is_unspecified(s7_scheme *sc, s7_pointer val);               /*     returns true if val is #<unspecified> */
s7_pointer s7_eof_object(s7_scheme *sc);                             /* #<eof> */
bool s7_is_null(s7_scheme *sc, s7_pointer p);                        /* null? */

  /* these are the Scheme constants; they do not change in value during a run,
   *   so they can be safely assigned to C global variables if desired. 
   */

bool s7_is_valid(s7_scheme *sc, s7_pointer arg);                     /* does 'arg' look like an s7 object? */
bool s7_is_c_pointer(s7_pointer arg);
void *s7_c_pointer(s7_pointer p);
s7_pointer s7_make_c_pointer(s7_scheme *sc, void *ptr);
  /* these are for passing uninterpreted C pointers through Scheme */

s7_pointer s7_eval_c_string(s7_scheme *sc, const char *str);         /* (eval-string str) */
s7_pointer s7_eval_c_string_with_environment(s7_scheme *sc, const char *str, s7_pointer e);
s7_pointer s7_object_to_string(s7_scheme *sc, s7_pointer arg, bool use_write);       
                                                                     /* (object->string obj) */
char *s7_object_to_c_string(s7_scheme *sc, s7_pointer obj);          /* same as object->string but returns a C char* directly */
                                                                     /*   the returned value should be freed by the caller */

s7_pointer s7_load(s7_scheme *sc, const char *file);                 /* (load file) */
s7_pointer s7_load_path(s7_scheme *sc);                              /* *load-path* */
s7_pointer s7_add_to_load_path(s7_scheme *sc, const char *dir);      /* (set! *load-path* (cons dir *load-path*)) */
s7_pointer s7_autoload(s7_scheme *sc, s7_pointer symbol, s7_pointer file_or_function);  /* (autoload symbol file-or-function) */

  /* the load path is a list of directories to search if load can't find the file passed as its argument.
   */
void s7_quit(s7_scheme *sc);
  /* this tries to break out of the current evaluation, leaving everything else intact */

void (*s7_begin_hook(s7_scheme *sc))(s7_scheme *sc, bool *val);
void s7_set_begin_hook(s7_scheme *sc, void (*hook)(s7_scheme *sc, bool *val));
  /* call "hook" at the start of any block; use NULL to cancel.
   *   s7_begin_hook returns the current begin_hook function or NULL.
   */

s7_pointer s7_eval(s7_scheme *sc, s7_pointer code, s7_pointer e);
s7_pointer s7_eval_form(s7_scheme *sc, s7_pointer form, s7_pointer e);

void s7_provide(s7_scheme *sc, const char *feature);                 /* add feature (as a symbol) to the *features* list */
bool s7_is_provided(s7_scheme *sc, const char *feature);             /* (provided? feature) */


s7_pointer s7_error(s7_scheme *sc, s7_pointer type, s7_pointer info);
s7_pointer s7_wrong_type_arg_error(s7_scheme *sc, const char *caller, int arg_n, s7_pointer arg, const char *descr);
  /* set arg_n to 0 to indicate that caller takes only one argument (so the argument number need not be reported */
s7_pointer s7_out_of_range_error(s7_scheme *sc, const char *caller, int arg_n, s7_pointer arg, const char *descr);
s7_pointer s7_wrong_number_of_args_error(s7_scheme *sc, const char *caller, s7_pointer args);
s7_pointer s7_stacktrace(s7_scheme *sc);

  /* these are equivalent to (error ...) in Scheme
   *   the first argument to s7_error is a symbol that can be caught (via (catch tag ...))
   *   the rest of the arguments are passed to the error handler (if in catch) 
   *   or printed out (in the default case).  If the first element of the list
   *   of args ("info") is a string, the default error handler treats it as
   *   a format control string, and passes it to format with the rest of the
   *   info list as the format function arguments.
   *
   *   s7_wrong_type_arg_error is equivalent to s7_error with a type of 'wrong-type-arg
   *   and similarly s7_out_of_range_error with type 'out-of-range.
   *
   * catch in Scheme is taken from Guile:
   *
   *  (catch tag thunk handler)
   *
   *  evaluates 'thunk'.  If an error occurs, and the type matches 'tag' (or if 'tag' is #t),
   *  the handler is called, passing it the arguments (including the type) passed to the
   *  error function.  If no handler is found, the default error handler is called,
   *  normally printing the error arguments to current-error-port.
   */

unsigned int s7_gc_protect(s7_scheme *sc, s7_pointer x);
void s7_gc_unprotect(s7_scheme *sc, s7_pointer x);
void s7_gc_unprotect_at(s7_scheme *sc, unsigned int loc);
s7_pointer s7_gc_protected_at(s7_scheme *sc, unsigned int loc);
s7_pointer s7_gc_on(s7_scheme *sc, bool on);
void s7_gc_stats(s7_scheme *sc, bool on);
unsigned int s7_heap_size(s7_scheme *sc);
int s7_gc_freed(s7_scheme *sc);

  /* any s7_pointer object held in C (as a local variable for example) needs to be
   *   protected from garbage collection if there is any chance the GC may run without
   *   an existing Scheme-level reference to that object.  s7_gc_protect places the
   *   object in a vector that the GC always checks, returning the object's location
   *   in that table.  s7_gc_unprotect and s7_gc_unprotect_at unprotect the object
   *   (remove it from the vector).  s7_gc_unprotect_at uses the location passed
   *   to it, whereas s7_gc_unprotect scans the vector to find the object.  
   *   s7_gc_protected_at returns the object at the given location.
   * 
   * You can turn the GC on and off via s7_gc_on.
   *
   * There is a built-in lag between the creation of a new object and its first possible GC
   *    (the lag time is set indirectly by GC_TEMPS_SIZE in s7.c), so you don't need to worry about
   *    very short term temps such as the arguments to s7_cons in:
   *
   *    s7_cons(s7, s7_make_real(s7, 3.14), 
   *                s7_cons(s7, s7_make_integer(s7, 123), 
   *                            s7_nil(s7)));
   */


bool s7_is_eq(s7_pointer a, s7_pointer b);                                   /* (eq? a b) */
bool s7_is_eqv(s7_pointer a, s7_pointer b);                                  /* (eqv? a b) */
bool s7_is_equal(s7_scheme *sc, s7_pointer a, s7_pointer b);                 /* (equal? a b) */


bool s7_is_boolean(s7_pointer x);                                            /* (boolean? x) */
bool s7_boolean(s7_scheme *sc, s7_pointer x);                                /* Scheme boolean -> C bool */
s7_pointer s7_make_boolean(s7_scheme *sc, bool x);                           /* C bool -> Scheme boolean */

  /* for each Scheme type (boolean, integer, string, etc), there are three
   *   functions: s7_<type>(...), s7_make_<type>(...), and s7_is_<type>(...):
   *
   *   s7_boolean(s7, obj) returns the C bool corresponding to the value of 'obj' (#f -> false)
   *   s7_make_boolean(s7, false|true) returns the s7 boolean corresponding to the C bool argument (false -> #f)
   *   s7_is_boolean(s7, obj) returns true if 'obj' has a boolean value (#f or #t).
   */


bool s7_is_pair(s7_pointer p);                                               /* (pair? p) */
s7_pointer s7_cons(s7_scheme *sc, s7_pointer a, s7_pointer b);               /* (cons a b) */

s7_pointer s7_car(s7_pointer p);                                             /* (car p) */
s7_pointer s7_cdr(s7_pointer p);                                             /* (cdr p) */

s7_pointer s7_set_car(s7_pointer p, s7_pointer q);                           /* (set-car! p q) */
s7_pointer s7_set_cdr(s7_pointer p, s7_pointer q);                           /* (set-cdr! p q) */

s7_pointer s7_cadr(s7_pointer p);                                            /* (cadr p) */
s7_pointer s7_cddr(s7_pointer p);                                            /* (cddr p) */
s7_pointer s7_cdar(s7_pointer p);                                            /* (cdar p) */
s7_pointer s7_caar(s7_pointer p);                                            /* (caar p) */

s7_pointer s7_caadr(s7_pointer p);                                           /* etc */
s7_pointer s7_caddr(s7_pointer p);
s7_pointer s7_cadar(s7_pointer p);
s7_pointer s7_caaar(s7_pointer p);
s7_pointer s7_cdadr(s7_pointer p);
s7_pointer s7_cdddr(s7_pointer p);
s7_pointer s7_cddar(s7_pointer p);
s7_pointer s7_cdaar(s7_pointer p);

s7_pointer s7_caaadr(s7_pointer p);
s7_pointer s7_caaddr(s7_pointer p);
s7_pointer s7_caadar(s7_pointer p);
s7_pointer s7_caaaar(s7_pointer p);
s7_pointer s7_cadadr(s7_pointer p);
s7_pointer s7_cadddr(s7_pointer p);
s7_pointer s7_caddar(s7_pointer p);
s7_pointer s7_cadaar(s7_pointer p);
s7_pointer s7_cdaadr(s7_pointer p);
s7_pointer s7_cdaddr(s7_pointer p);
s7_pointer s7_cdadar(s7_pointer p);
s7_pointer s7_cdaaar(s7_pointer p);
s7_pointer s7_cddadr(s7_pointer p);
s7_pointer s7_cddddr(s7_pointer p);
s7_pointer s7_cdddar(s7_pointer p);
s7_pointer s7_cddaar(s7_pointer p);


bool s7_is_list(s7_scheme *sc, s7_pointer p);                                /* (list? p) -> (or (pair? p) (null? p)) */
int s7_list_length(s7_scheme *sc, s7_pointer a);                             /* (length a) */
s7_pointer s7_list(s7_scheme *sc, int num_values, ...);                      /* (list ...) */
s7_pointer s7_reverse(s7_scheme *sc, s7_pointer a);                          /* (reverse a) */
s7_pointer s7_append(s7_scheme *sc, s7_pointer a, s7_pointer b);             /* (append a b) */
s7_pointer s7_list_ref(s7_scheme *sc, s7_pointer lst, int num);              /* (list-ref lst num) */
s7_pointer s7_list_set(s7_scheme *sc, s7_pointer lst, int num, s7_pointer val); /* (list-set! lst num val) */
s7_pointer s7_assoc(s7_scheme *sc, s7_pointer obj, s7_pointer lst);          /* (assoc obj lst) */
s7_pointer s7_assq(s7_scheme *sc, s7_pointer obj, s7_pointer x);             /* (assq obj lst) */
s7_pointer s7_member(s7_scheme *sc, s7_pointer obj, s7_pointer lst);         /* (member obj lst) */
s7_pointer s7_memq(s7_scheme *sc, s7_pointer obj, s7_pointer x);             /* (memq obj lst) */

bool s7_is_string(s7_pointer p);                                             /* (string? p) */
const char *s7_string(s7_pointer p);                                         /* Scheme string -> C string (do not free the string) */
s7_pointer s7_make_string(s7_scheme *sc, const char *str);                   /* C string -> Scheme string (str is copied) */
s7_pointer s7_make_string_with_length(s7_scheme *sc, const char *str, int len);  /* same as s7_make_string, but provides strlen */
s7_pointer s7_make_permanent_string(const char *str);                        /* make a string that will never be GC'd */
unsigned int s7_string_length(s7_pointer str);                               /* (string-length str) */

bool s7_is_character(s7_pointer p);                                          /* (character? p) */
char s7_character(s7_pointer p);                                             /* Scheme character -> C char */
s7_pointer s7_make_character(s7_scheme *sc, unsigned int c);                 /* C char (as unsigned int) -> Scheme character */


bool s7_is_number(s7_pointer p);                                             /* (number? p) */
bool s7_is_integer(s7_pointer p);                                            /* (integer? p) */
s7_int s7_integer(s7_pointer p);                                             /* Scheme integer -> C int (long long int probably) */
s7_pointer s7_make_integer(s7_scheme *sc, s7_int num);                       /* C long long int -> Scheme integer */

bool s7_is_real(s7_pointer p);                                               /* (real? p) */
s7_double s7_real(s7_pointer p);                                             /* Scheme real -> C double */
s7_pointer s7_make_real(s7_scheme *sc, s7_double num);                       /* C double -> Scheme real */
s7_pointer s7_make_mutable_real(s7_scheme *sc, s7_double n);
s7_double s7_number_to_real(s7_scheme *sc, s7_pointer x);                    /* x can be any kind of number */
s7_double s7_number_to_real_with_caller(s7_scheme *sc, s7_pointer x, const char *caller);
s7_int s7_number_to_integer(s7_scheme *sc, s7_pointer x);
s7_int s7_number_to_integer_with_caller(s7_scheme *sc, s7_pointer x, const char *caller);
bool s7_is_mutable(s7_pointer p);

bool s7_is_ulong(s7_pointer arg);                                            /* returns true if arg is an unsigned long */
unsigned long s7_ulong(s7_pointer p);                                        /* Scheme unsigned long -> C */
s7_pointer s7_make_ulong(s7_scheme *sc, unsigned long n);                    /* C unsigned lonog -> Scheme */
bool s7_is_ulong_long(s7_pointer arg);                                       /* returns true if arg is an unsigned long long */
unsigned long long s7_ulong_long(s7_pointer p);                              /* Scheme unsigned long long -> C */
s7_pointer s7_make_ulong_long(s7_scheme *sc, unsigned long long n);          /* C unsigned long long -> Scheme */
  /* the ulong stuff is intended for passing uninterpreted C pointers through Scheme and back to C */

bool s7_is_rational(s7_pointer arg);                                        /* (rational? arg) -- integer or ratio */
bool s7_is_ratio(s7_pointer arg);                                           /* true if arg is a ratio, not an integer */
s7_pointer s7_make_ratio(s7_scheme *sc, s7_int a, s7_int b);                /* returns the Scheme object a/b */
s7_pointer s7_rationalize(s7_scheme *sc, s7_double x, s7_double error);     /* (rationalize x error) */
s7_int s7_numerator(s7_pointer x);                                          /* (numerator x) */
s7_int s7_denominator(s7_pointer x);                                        /* (denominator x) */
s7_double s7_random(s7_scheme *sc, s7_pointer state);                       /* (random x) */
s7_pointer s7_make_random_state(s7_scheme *sc, s7_pointer seed);            /* (make-random-state seed) */
s7_pointer s7_random_state_to_list(s7_scheme *sc, s7_pointer args);         /* (random-state->list r) */
void s7_set_default_random_state(s7_scheme *sc, s7_int seed, s7_int carry);

bool s7_is_complex(s7_pointer arg);                                         /* (complex? arg) */
s7_pointer s7_make_complex(s7_scheme *sc, s7_double a, s7_double b);        /* returns the Scheme object a+bi */
s7_double s7_real_part(s7_pointer z);                                       /* (real-part z) */
s7_double s7_imag_part(s7_pointer z);                                       /* (imag-part z) */
char *s7_number_to_string(s7_scheme *sc, s7_pointer obj, int radix);        /* (number->string obj radix) */


bool s7_is_vector(s7_pointer p);                                            /* (vector? p) */
s7_int s7_vector_length(s7_pointer vec);                                    /* (vector-length vec) */
int s7_vector_rank(s7_pointer vect);                                        /* number of dimensions in vect */
s7_int *s7_vector_dimensions(s7_pointer vec);                               /* dimensions */
s7_int *s7_vector_offsets(s7_pointer vec);                                  /* precalculated offsets to speed-up addressing */
bool s7_is_sundry_vector(s7_pointer p);
s7_pointer *s7_vector_elements(s7_pointer vec);                             /* a pointer to the array of s7_pointers */
s7_int *s7_int_vector_elements(s7_pointer vec);
s7_double *s7_float_vector_elements(s7_pointer vec);
bool s7_is_float_vector(s7_pointer p);                                    
bool s7_is_int_vector(s7_pointer p);                                      

s7_pointer s7_vector_ref(s7_scheme *sc, s7_pointer vec, s7_int index);                            /* (vector-ref vec index) */
s7_pointer s7_vector_set(s7_scheme *sc, s7_pointer vec, s7_int index, s7_pointer a);              /* (vector-set! vec index a) */
s7_pointer s7_vector_ref_n(s7_scheme *sc, s7_pointer vector, int indices, ...);                   /* multidimensional vector-ref */
s7_pointer s7_vector_set_n(s7_scheme *sc, s7_pointer vector, s7_pointer value, int indices, ...); /* multidimensional vector-set! */

s7_pointer s7_make_vector(s7_scheme *sc, s7_int len);                                 /* (make-vector len) */
s7_pointer s7_make_int_vector(s7_scheme *sc, s7_int len, int dims, s7_int *dim_info);
s7_pointer s7_make_float_vector(s7_scheme *sc, s7_int len, int dims, s7_int *dim_info);
s7_pointer s7_make_float_vector_wrapper(s7_scheme *sc, s7_int len, s7_double *data, int dims, s7_int *dim_info, bool free_data);
s7_pointer s7_make_and_fill_vector(s7_scheme *sc, s7_int len, s7_pointer fill);       /* (make-vector len fill) */

void s7_vector_fill(s7_scheme *sc, s7_pointer vec, s7_pointer obj);                   /* (vector-fill! vec obj) */
s7_pointer s7_vector_copy(s7_scheme *sc, s7_pointer old_vect);
s7_pointer s7_vector_to_list(s7_scheme *sc, s7_pointer vect);                         /* (vector->list vec) */

s7_int s7_print_length(s7_scheme *sc);                                                /* value of (*s7* 'print-length) */
s7_int s7_set_print_length(s7_scheme *sc, s7_int new_len);

  /* 
   *  (vect i) is the same as (vector-ref vect i)
   *  (set! (vect i) x) is the same as (vector-set! vect i x)
   *  (vect i j k) accesses the 3-dimensional vect
   *  (set! (vect i j k) x) sets that element (vector-ref and vector-set! can also be used)
   *  (make-vector (list 2 3 4)) returns a 3-dimensional vector with the given dimension sizes
   *  (make-vector '(2 3) 1.0) returns a 2-dim vector with all elements set to 1.0
   */
  

bool s7_is_hash_table(s7_pointer p);                                        /* (hash-table? p) */
s7_pointer s7_make_hash_table(s7_scheme *sc, s7_int size);                  /* (make-hash-table size) */
s7_pointer s7_hash_table_ref(s7_scheme *sc, s7_pointer table, s7_pointer key);   
                                                                            /* (hash-table-ref table key) */
s7_pointer s7_hash_table_set(s7_scheme *sc, s7_pointer table, s7_pointer key, s7_pointer value);  
                                                                            /* (hash-table-set! table key value) */

s7_pointer s7_hook_functions(s7_scheme *sc, s7_pointer hook);                              /* (hook-functions hook) */
s7_pointer s7_hook_set_functions(s7_scheme *sc, s7_pointer hook, s7_pointer functions);    /* (set! (hook-functions hook) ...) */


bool s7_is_input_port(s7_scheme *sc, s7_pointer p);                         /* (input-port? p) */
bool s7_is_output_port(s7_scheme *sc, s7_pointer p);                        /* (output-port? p) */
const char *s7_port_filename(s7_pointer x);                                 /* (port-filename p) */
int s7_port_line_number(s7_pointer p);                                      /* (port-line-number p) */

s7_pointer s7_current_input_port(s7_scheme *sc);                            /* (current-input-port) */
s7_pointer s7_set_current_input_port(s7_scheme *sc, s7_pointer p);          /* (set-current-input-port) */
s7_pointer s7_current_output_port(s7_scheme *sc);                           /* (current-output-port) */
s7_pointer s7_set_current_output_port(s7_scheme *sc, s7_pointer p);         /* (set-current-output-port) */
s7_pointer s7_current_error_port(s7_scheme *sc);                            /* (current-error-port) */
s7_pointer s7_set_current_error_port(s7_scheme *sc, s7_pointer port);       /* (set-current-error-port port) */
void s7_close_input_port(s7_scheme *sc, s7_pointer p);                      /* (close-input-port p) */
void s7_close_output_port(s7_scheme *sc, s7_pointer p);                     /* (close-output-port p) */
s7_pointer s7_open_input_file(s7_scheme *sc, const char *name, const char *mode);  
                                                                            /* (open-input-file name mode) */
s7_pointer s7_open_output_file(s7_scheme *sc, const char *name, const char *mode); 
                                                                            /* (open-output-file name mode) */
  /* mode here is an optional C style flag, "a" for "alter", etc ("r" is the input default, "w" is the output default) */
s7_pointer s7_open_input_string(s7_scheme *sc, const char *input_string);  
                                                                            /* (open-input-string str) */
s7_pointer s7_open_output_string(s7_scheme *sc);                            /* (open-output-string) */
const char *s7_get_output_string(s7_scheme *sc, s7_pointer out_port);       /* (get-output-string port) -- current contents of output string */
  /*    don't free the string */
void s7_flush_output_port(s7_scheme *sc, s7_pointer p);                     /* (flush-output-port port) */

typedef enum {S7_READ, S7_READ_CHAR, S7_READ_LINE, S7_READ_BYTE, S7_PEEK_CHAR, S7_IS_CHAR_READY} s7_read_t;
s7_pointer s7_open_output_function(s7_scheme *sc, void (*function)(s7_scheme *sc, unsigned char c, s7_pointer port));  
s7_pointer s7_open_input_function(s7_scheme *sc, s7_pointer (*function)(s7_scheme *sc, s7_read_t read_choice, s7_pointer port));

int s7_read_char(s7_scheme *sc, s7_pointer port);                           /* (read-char port) */
int s7_peek_char(s7_scheme *sc, s7_pointer port);                           /* (peek-char port) */
s7_pointer s7_read(s7_scheme *sc, s7_pointer port);                         /* (read port) */
void s7_newline(s7_scheme *sc, s7_pointer port);                            /* (newline port) */
void s7_write_char(s7_scheme *sc, int c, s7_pointer port);                  /* (write-char c port) */
void s7_write(s7_scheme *sc, s7_pointer obj, s7_pointer port);              /* (write obj port) */
void s7_display(s7_scheme *sc, s7_pointer obj, s7_pointer port);            /* (display obj port) */
const char *s7_format(s7_scheme *sc, s7_pointer args);                      /* (format ... */


bool s7_is_procedure(s7_pointer x);                                         /* (procedure? x) */
bool s7_is_macro(s7_scheme *sc, s7_pointer x);                              /* (macro? x) */
s7_pointer s7_closure_body(s7_scheme *sc, s7_pointer p);
s7_pointer s7_closure_let(s7_scheme *sc, s7_pointer p);
s7_pointer s7_closure_args(s7_scheme *sc, s7_pointer p);
s7_pointer s7_funclet(s7_scheme *sc, s7_pointer p);                         /* (funclet x) */
const char *s7_procedure_documentation(s7_scheme *sc, s7_pointer p);        /* (procedure-documentation x) if any (don't free the string) */
bool s7_is_aritable(s7_scheme *sc, s7_pointer x, int args);                 /* (aritable? x args) */
s7_pointer s7_arity(s7_scheme *sc, s7_pointer x);                           /* (arity x) */

const char *s7_help(s7_scheme *sc, s7_pointer obj);                         /* (help obj) */

s7_pointer s7_make_continuation(s7_scheme *sc);                             /* call/cc... (see example below) */
bool s7_is_syntax(s7_pointer p);
bool s7_is_symbol(s7_pointer p);                                            /* (symbol? p) */
const char *s7_symbol_name(s7_pointer p);                                   /* (symbol->string p) -- don't free the string */
s7_pointer s7_make_symbol(s7_scheme *sc, const char *name);                 /* (string->symbol name) */
s7_pointer s7_gensym(s7_scheme *sc, const char *prefix);                    /* (gensym prefix) */

bool s7_is_keyword(s7_pointer obj);                                         /* (keyword? obj) */
s7_pointer s7_make_keyword(s7_scheme *sc, const char *key);                 /* (make-keyword key) */

s7_pointer s7_symbol_access(s7_scheme *sc, s7_pointer sym);
s7_pointer s7_symbol_set_access(s7_scheme *sc, s7_pointer symbol, s7_pointer func);

s7_pointer s7_slot(s7_scheme *sc, s7_pointer symbol);
s7_pointer s7_slot_value(s7_pointer slot);
s7_pointer s7_slot_set_value(s7_scheme *sc, s7_pointer slot, s7_pointer value);
s7_pointer s7_make_slot(s7_scheme *sc, s7_pointer env, s7_pointer symbol, s7_pointer value);

s7_pointer s7_rootlet(s7_scheme *sc);                                       /* (rootlet) */
s7_pointer s7_shadow_rootlet(s7_scheme *sc);
s7_pointer s7_set_shadow_rootlet(s7_scheme *sc, s7_pointer let);
s7_pointer s7_curlet(s7_scheme *sc);                                        /* (curlet) */
s7_pointer s7_set_curlet(s7_scheme *sc, s7_pointer e);                      /* returns previous curlet */
s7_pointer s7_outlet(s7_pointer e);                                         /* (outlet e) */
s7_pointer s7_sublet(s7_scheme *sc, s7_pointer env, s7_pointer bindings);   /* (sublet e ...) */
s7_pointer s7_inlet(s7_scheme *sc, s7_pointer bindings);                    /* (inlet ...) */
s7_pointer s7_let_to_list(s7_scheme *sc, s7_pointer env);                   /* (let->list env) */
bool s7_is_let(s7_pointer e);                                               /* )let? e) */
s7_pointer s7_let_ref(s7_scheme *sc, s7_pointer env, s7_pointer sym);       /* (let-ref e sym) */
s7_pointer s7_let_set(s7_scheme *sc, s7_pointer env, s7_pointer sym, s7_pointer val); /* (let-set! e sym val) */
s7_pointer s7_openlet(s7_pointer e);                                        /* (openlet e) */
bool s7_is_openlet(s7_pointer e);                                           /* (openlet? e) */
s7_pointer s7_method(s7_scheme *sc, s7_pointer obj, s7_pointer method);

s7_pointer s7_name_to_value(s7_scheme *sc, const char *name);
s7_pointer s7_symbol_table_find_name(s7_scheme *sc, const char *name);
s7_pointer s7_symbol_value(s7_scheme *sc, s7_pointer sym);
s7_pointer s7_symbol_set_value(s7_scheme *sc, s7_pointer sym, s7_pointer val);
s7_pointer s7_symbol_local_value(s7_scheme *sc, s7_pointer sym, s7_pointer local_env);
char *s7_symbol_documentation(s7_scheme *sc, s7_pointer sym);
char *s7_symbol_set_documentation(s7_scheme *sc, s7_pointer sym, const char *new_doc);
bool s7_for_each_symbol_name(s7_scheme *sc, bool (*symbol_func)(const char *symbol_name, void *data), void *data);
bool s7_for_each_symbol(s7_scheme *sc, bool (*symbol_func)(const char *symbol_name, s7_pointer value, void *data), void *data);
  
  /* these access the current environment and symbol table, providing
   *   a symbol's current binding (s7_name_to_value takes the symbol name as a char*,
   *   s7_symbol_value takes the symbol itself, s7_symbol_set_value changes the
   *   current binding, and s7_symbol_local_value uses the environment passed
   *   as its third argument.
   *
   * To iterate over the complete symbol table, use s7_for_each_symbol_name,
   *   and s7_for_each_symbol.  The latter calls the 'symbol_func' on each
   *   symbol, passing the symbol name, its current binding, and the uninterpreted
   *   'data' pointer.  s7_for_each_symbol_name is similar, but does not include
   *   the current binding.
   *
   * The for-each loop stops if the symbol_func returns true, or at the end of the table.
   */


void s7_define(s7_scheme *sc, s7_pointer env, s7_pointer symbol, s7_pointer value);
bool s7_is_defined(s7_scheme *sc, const char *name);
s7_pointer s7_define_variable(s7_scheme *sc, const char *name, s7_pointer value);
s7_pointer s7_define_variable_with_documentation(s7_scheme *sc, const char *name, s7_pointer value, const char *help);
s7_pointer s7_define_constant(s7_scheme *sc, const char *name, s7_pointer value);
s7_pointer s7_define_constant_with_documentation(s7_scheme *sc, const char *name, s7_pointer value, const char *help);
bool s7_is_constant(s7_pointer p);

  /* These three functions add a symbol and its binding to either the top-level environment
   *    or the 'env' passed as the second argument to s7_define.
   *
   *    s7_define_variable(sc, "*features*", sc->NIL);
   *
   * in s7.c is equivalent to the top level form
   *
   *    (define *features* '())
   *
   * s7_define_variable is simply s7_define with string->symbol and the global environment.
   * s7_define_constant is s7_define but makes its "definee" immutable.
   * s7_define is equivalent to define in Scheme.
   */

bool s7_is_function(s7_pointer p); 
s7_pointer s7_make_function(s7_scheme *sc, const char *name, s7_function fnc, int required_args, int optional_args, bool rest_arg, const char *doc);
s7_pointer s7_make_safe_function(s7_scheme *sc, const char *name, s7_function fnc, int required_args, int optional_args, bool rest_arg, const char *doc);

s7_pointer s7_define_function(s7_scheme *sc, const char *name, s7_function fnc, int required_args, int optional_args, bool rest_arg, const char *doc);
s7_pointer s7_define_safe_function(s7_scheme *sc, const char *name, s7_function fnc, int required_args, int optional_args, bool rest_arg, const char *doc);
void s7_define_function_star(s7_scheme *sc, const char *name, s7_function fnc, const char *arglist, const char *doc);
s7_pointer s7_define_integer_function(s7_scheme *sc, const char *name, s7_function fnc, int required_args, int optional_args, bool rest_arg, const char *doc);
void s7_define_safe_function_star(s7_scheme *sc, const char *name, s7_function fnc, const char *arglist, const char *doc);
void s7_define_function_with_setter(s7_scheme *sc, const char *name, s7_function get_fnc, s7_function set_fnc, int req_args, int opt_args, const char *doc);
  /* this is now the same as s7_dilambda (different args) */

s7_pointer s7_apply_function(s7_scheme *sc, s7_pointer fnc, s7_pointer args);
s7_pointer s7_define_macro(s7_scheme *sc, const char *name, s7_function fnc, int required_args, int optional_args, bool rest_arg, const char *doc);

  /* s7_make_function creates a Scheme function object from the s7_function 'fnc'.
   *   Its name (for s7_describe_object) is 'name', it requires 'required_args' arguments,
   *   can accept 'optional_args' other arguments, and if 'rest_arg' is true, it accepts
   *   a "rest" argument (a list of all the trailing arguments).  The function's documentation
   *   is 'doc'.
   *
   * s7_define_function is the same as s7_make_function, but it also adds 'name' (as a symbol) to the
   *   global (top-level) environment, with the function as its value.  For example, the Scheme
   *   function 'car' is essentially:
   *
   *     s7_pointer g_car(s7_scheme *sc, s7_pointer args) 
   *       {return(s7_car(sc, s7_car(sc, args)));}
   *
   *   then bound to the name "car":
   *
   *     s7_define_function(sc, "car", g_car, 1, 0, false, "(car obj)");
   *                                          one required arg, no optional arg, no "rest" arg
   *
   * s7_is_function returns true if its argument is a function defined in this manner.
   * s7_apply_function applies the function (the result of s7_make_function) to the arguments.
   *
   * s7_define_macro defines a Scheme macro; its arguments are not evaluated (unlike a function),
   *   but its returned value (assumed to be some sort of Scheme expression) is evaluated.
   */

  /* In s7, (define* (name . args) body) or (define name (lambda* args body))
   *   define a function that takes optional (keyword) named arguments.
   *   The keywords :key and :optional are ok, but they are ignored --
   *   they exist to be compatible with other define* implementations.  
   *   The "args" is a list that can contain either names (normal arguments),
   *   or lists of the form (name default-value), in any order.  When called,
   *   the names are bound to their default values (or #f), then the function's
   *   current arglist is scanned.  Any name that occurs as a keyword (":name")
   *   precedes that argument's new value.  Otherwise, as values occur, they
   *   are plugged into the environment based on their position in the arglist
   *   (as normal for a function).  So,
   *   
   *   (define* (hi a (b 32) (c "hi")) (list a b c))
   *
   *   is equivalent to other implementations (define* (hi a :key (b 32) ...))
   *   or (define* (hi a :optional (b 32) ...)) -- these args are all
   *   "optional-key" args in CLM jargon.
   *
   *   (hi 1) -> '(1 32 "hi")
   *   (hi :b 2 :a 3) -> '(3 2 "hi")
   *   (hi 3 2 1) -> '(3 2 1)
   *
   *   and so on.  :rest causes its argument to be bound to the rest
   *   of the arguments at that point.
   *
   * The C connection to this takes the function name, the C function to call, the argument 
   *   list as written in Scheme, and the documentation string.  s7 makes sure the arguments
   *   are ordered correctly and have the specified defaults before calling the C function.
   *     s7_define_function_star(sc, "a-func", a_func, "arg1 (arg2 32)", "an example of C define*");
   *   Now (a-func :arg1 2) calls the C function a_func(2, 32). See the example program in s7.html.
   *
   * In s7 Scheme, define* can be used just for its optional arguments feature, but that is
   *   included in s7_define_function.  s7_define_function_star implements keyword arguments
   *   for C-level functions (as well as optional/rest arguments).
   */

s7_pointer s7_call(s7_scheme *sc, s7_pointer func, s7_pointer args);
s7_pointer s7_call_with_location(s7_scheme *sc, s7_pointer func, s7_pointer args, const char *caller, const char *file, int line);
  
  /* s7_call takes a Scheme function (e.g. g_car above), and applies it to 'args' (a list of arguments)
   *   returning the result.
   *   
   *   s7_integer(s7_call(s7, g_car, s7_cons(s7, s7_make_integer(s7, 123), s7_nil(s7))));
   *  
   *   returns 123.
   *
   * s7_call_with_location passes some information to the error handler.  
   */
s7_pointer s7_dynamic_wind(s7_scheme *sc, s7_pointer init, s7_pointer body, s7_pointer finish);


bool s7_is_dilambda(s7_pointer obj);
s7_pointer s7_dilambda(s7_scheme *sc, 
		       const char *name,
		       s7_pointer (*getter)(s7_scheme *sc, s7_pointer args), 
		       int get_req_args, int get_opt_args,
		       s7_pointer (*setter)(s7_scheme *sc, s7_pointer args),
		       int set_req_args, int set_opt_args,
		       const char *documentation);
s7_pointer s7_procedure_setter(s7_scheme *sc, s7_pointer obj);
s7_pointer s7_values(s7_scheme *sc, s7_pointer args);
s7_pointer s7_make_iterator(s7_scheme *sc, s7_pointer e);
bool s7_is_iterator(s7_pointer obj);
bool s7_iterator_is_at_end(s7_pointer obj);
s7_pointer s7_iterate(s7_scheme *sc, s7_pointer iter);


int s7_new_type(const char *name, 
		char *(*print)(s7_scheme *sc, void *value), 
		void (*free)(void *value), 
		bool (*equal)(void *val1, void *val2),
		void (*gc_mark)(void *val),
		s7_pointer (*apply)(s7_scheme *sc, s7_pointer obj, s7_pointer args),
		s7_pointer (*set)(s7_scheme *sc, s7_pointer obj, s7_pointer args));

int s7_new_type_x(s7_scheme *sc,
		  const char *name, 
		  char *(*print)(s7_scheme *sc, void *value), 
		  void (*free)(void *value), 
		  bool (*equal)(void *val1, void *val2),
		  void (*gc_mark)(void *val),
		  s7_pointer (*apply)(s7_scheme *sc, s7_pointer obj, s7_pointer args),
		  s7_pointer (*set)(s7_scheme *sc, s7_pointer obj, s7_pointer args),
		  s7_pointer (*length)(s7_scheme *sc, s7_pointer obj),
		  s7_pointer (*copy)(s7_scheme *sc, s7_pointer args),
		  s7_pointer (*reverse)(s7_scheme *sc, s7_pointer obj),
		  s7_pointer (*fill)(s7_scheme *sc, s7_pointer args));

bool s7_is_object(s7_pointer p);
int s7_object_type(s7_pointer obj);
void *s7_object_value(s7_pointer obj);
void *s7_object_value_checked(s7_pointer obj, int type);
void *s7_vector_ref_object_value_checked(s7_scheme *sc, s7_pointer args, int type);
s7_pointer s7_make_object(s7_scheme *sc, int type, void *value);
void s7_mark_object(s7_pointer p);
s7_pointer s7_object_let(s7_pointer obj);
s7_pointer s7_object_set_let(s7_pointer obj, s7_pointer e);
void s7_set_object_print_readably(int type, char *(*printer)(s7_scheme *sc, void *val));

  /* These functions create a new Scheme object type.  There is a simple example in s7.html.
   *
   * s7_new_type describes the type for Scheme:
   *   name:    the name used by describe-object
   *   print:   the function called whenever s7 is asked to display a value with this type
   *   free:    the function called when an object of this type is about to be garbage collected
   *   equal:   compare two objects of this type; (equal? obj1 obj2)
   *   gc_mark: called during the GC mark pass -- you should call s7_mark_object
   *            on any embedded s7_pointer associated with the object.
   *   apply:   a function that is called whenever an object of this type
   *            occurs in the function position (at the car of a list; the rest of the list
   *            is passed to the apply function as the arguments).
   *   set:     a function that is called whenever an object of this type occurs as
   *            the target of a generalized set!
   *
   * in the extended version (s7_new_type_x), you can also set the following:
   *   length:  the function called when the object is asked what its length is.
   *   copy:    the function called when a copy of the object is needed.
   *   fill:    the function called to fill the object with some value.
   *
   *   s7_new_type and s7_new_typ_x return an integer that identifies the new type for the other functions.
   *
   * s7_is_object returns true if 'p' holds a value of a type created by s7_new_type.
   * s7_object_type returns the object's type
   * s7_object_value returns the value bound to that object (the void *value of s7_make_object)
   * s7_make_object creates a new Scheme entity of the given type with the given (uninterpreted) value
   * s7_mark_object marks any Scheme object as in-use (use this in the gc_mark function to mark
   *    any embedded s7_pointer variables).
   */


void s7_autoload_set_names(s7_scheme *sc, const char **names, int size);

s7_pointer s7_copy(s7_scheme *sc, s7_pointer args);
s7_pointer s7_fill(s7_scheme *sc, s7_pointer args);


  /* these are aimed at the CLM optimizer */
void s7_function_set_returns_temp(s7_pointer f);

typedef s7_double (*s7_rf_t)(s7_scheme *sc, s7_pointer **p);
typedef s7_rf_t (*s7_rp_t)(s7_scheme *sc, s7_pointer expr);
void s7_rf_set_function(s7_pointer f, s7_rp_t rp);
s7_rp_t s7_rf_function(s7_scheme *sc, s7_pointer func);
s7_rf_t s7_is_rf_1(s7_scheme *sc, s7_pointer expr, s7_rf_t r, s7_rf_t s, s7_rf_t x);
s7_rf_t s7_is_rf_2(s7_scheme *sc, s7_pointer expr, s7_rf_t rr, s7_rf_t sr, s7_rf_t xr, s7_rf_t rs, s7_rf_t ss, s7_rf_t xs, s7_rf_t rx, s7_rf_t sx, s7_rf_t xx);

typedef s7_int (*s7_if_t)(s7_scheme *sc, s7_pointer **p);
typedef s7_if_t (*s7_ip_t)(s7_scheme *sc, s7_pointer expr);
void s7_if_set_function(s7_pointer f, s7_ip_t rp);
s7_ip_t s7_if_function(s7_scheme *sc, s7_pointer func);

typedef s7_pointer (*s7_pf_t)(s7_scheme *sc, s7_pointer **p);
typedef s7_pf_t (*s7_pp_t)(s7_scheme *sc, s7_pointer expr);
void s7_pf_set_function(s7_pointer f, s7_pp_t rp);
s7_pp_t s7_pf_function(s7_scheme *sc, s7_pointer func);

void s7_xf_store_at(s7_scheme *sc, s7_int index, s7_pointer val);
s7_int s7_xf_store(s7_scheme *sc, s7_pointer val);
void s7_xf_free(s7_scheme *sc);
s7_pointer *s7_xf_start(s7_scheme *sc);
void *s7_xf_new(s7_scheme *sc, s7_pointer e);
bool s7_xf_is_stepper(s7_scheme *sc, s7_pointer sym);

s7_int s7_slot_integer_value(s7_pointer slot);
bool s7_is_stepper(s7_pointer p);
s7_int s7_slot_integer_value_checked(s7_scheme *sc, s7_pointer slot, const char *caller);
s7_double s7_slot_real_value(s7_scheme *sc, s7_pointer slot, const char *caller);
void s7_slot_set_real_value(s7_scheme *sc, s7_pointer slot, s7_double value);
/* end CLM stuff */


  /* this is experimental */
s7_pointer s7_apply_1(s7_scheme *sc, s7_pointer args, s7_pointer (*f1)(s7_pointer a1));
s7_pointer s7_apply_2(s7_scheme *sc, s7_pointer args, s7_pointer (*f2)(s7_pointer a1, s7_pointer a2));
s7_pointer s7_apply_3(s7_scheme *sc, s7_pointer args, s7_pointer (*f3)(s7_pointer a1, s7_pointer a2, s7_pointer a3));
s7_pointer s7_apply_4(s7_scheme *sc, s7_pointer args, s7_pointer (*f4)(s7_pointer a1, s7_pointer a2, s7_pointer a3, s7_pointer a4));
s7_pointer s7_apply_5(s7_scheme *sc, s7_pointer args, s7_pointer (*f5)(s7_pointer a1, s7_pointer a2, s7_pointer a3, s7_pointer a4, s7_pointer a5));
s7_pointer s7_apply_6(s7_scheme *sc, s7_pointer args, 
		      s7_pointer (*f6)(s7_pointer a1, s7_pointer a2, s7_pointer a3, s7_pointer a4,
				       s7_pointer a5, s7_pointer a6));
s7_pointer s7_apply_7(s7_scheme *sc, s7_pointer args, 
		      s7_pointer (*f7)(s7_pointer a1, s7_pointer a2, s7_pointer a3, s7_pointer a4, 
				       s7_pointer a5, s7_pointer a6, s7_pointer a7));
s7_pointer s7_apply_8(s7_scheme *sc, s7_pointer args, 
		      s7_pointer (*f8)(s7_pointer a1, s7_pointer a2, s7_pointer a3, s7_pointer a4, 
				       s7_pointer a5, s7_pointer a6, s7_pointer a7, s7_pointer a8));
s7_pointer s7_apply_9(s7_scheme *sc, s7_pointer args, 
		      s7_pointer (*f9)(s7_pointer a1, s7_pointer a2, s7_pointer a3, s7_pointer a4, 
				       s7_pointer a5, s7_pointer a6, s7_pointer a7, s7_pointer a8, s7_pointer a9));

s7_pointer s7_apply_n_1(s7_scheme *sc, s7_pointer args, s7_pointer (*f1)(s7_pointer a1));
s7_pointer s7_apply_n_2(s7_scheme *sc, s7_pointer args, s7_pointer (*f2)(s7_pointer a1, s7_pointer a2));
s7_pointer s7_apply_n_3(s7_scheme *sc, s7_pointer args, s7_pointer (*f3)(s7_pointer a1, s7_pointer a2, s7_pointer a3));
s7_pointer s7_apply_n_4(s7_scheme *sc, s7_pointer args, s7_pointer (*f4)(s7_pointer a1, s7_pointer a2, s7_pointer a3, s7_pointer a4));
s7_pointer s7_apply_n_5(s7_scheme *sc, s7_pointer args, s7_pointer (*f5)(s7_pointer a1, s7_pointer a2, s7_pointer a3, s7_pointer a4, s7_pointer a5));
s7_pointer s7_apply_n_6(s7_scheme *sc, s7_pointer args, 
		      s7_pointer (*f6)(s7_pointer a1, s7_pointer a2, s7_pointer a3, s7_pointer a4,
				       s7_pointer a5, s7_pointer a6));
s7_pointer s7_apply_n_7(s7_scheme *sc, s7_pointer args, 
		      s7_pointer (*f7)(s7_pointer a1, s7_pointer a2, s7_pointer a3, s7_pointer a4, 
				       s7_pointer a5, s7_pointer a6, s7_pointer a7));
s7_pointer s7_apply_n_8(s7_scheme *sc, s7_pointer args, 
		      s7_pointer (*f8)(s7_pointer a1, s7_pointer a2, s7_pointer a3, s7_pointer a4, 
				       s7_pointer a5, s7_pointer a6, s7_pointer a7, s7_pointer a8));
s7_pointer s7_apply_n_9(s7_scheme *sc, s7_pointer args, 
		      s7_pointer (*f9)(s7_pointer a1, s7_pointer a2, s7_pointer a3, s7_pointer a4, 
				       s7_pointer a5, s7_pointer a6, s7_pointer a7, s7_pointer a8, s7_pointer a9));

#if WITH_GMP
  #include <gmp.h>
  #include <mpfr.h>
  #include <mpc.h>

  bool s7_is_bignum(s7_pointer obj);
  mpfr_t *s7_big_real(s7_pointer x);
  mpz_t *s7_big_integer(s7_pointer x);
  mpq_t *s7_big_ratio(s7_pointer x);
  mpc_t *s7_big_complex(s7_pointer x);
  s7_pointer s7_make_big_integer(s7_scheme *sc, mpz_t *val);
  s7_pointer s7_make_big_ratio(s7_scheme *sc, mpq_t *val);
  s7_pointer s7_make_big_real(s7_scheme *sc, mpfr_t *val);
  s7_pointer s7_make_big_complex(s7_scheme *sc, mpc_t *val);
#endif


#ifdef __cplusplus
}
#endif

#endif


#if (!DISABLE_DEPRECATED)
/* cm uses this: */
#define s7_UNSPECIFIED(Sc) s7_unspecified(Sc)
#define s7_NIL(Sc) s7_nil(Sc)
#define s7_is_procedure_with_setter s7_is_dilambda
#define s7_make_procedure_with_setter s7_dilambda
#endif



/* --------------------------------------------------------------------------------
 * 
 *        s7 changes
 *
 * 3-Jul:     s7_Double -> s7_double, s7_Int -> s7_int. Removed function_chooser_data.
 * 27-Jun:    s7_rf_t, s7_rp_t etc.
 * 19-Jun:    removed the ex_parser stuff, set_step_safe, s7_ex_fallback.
 * 5-May:     s7_make_iterator and friends.
 * 16-Apr:    added s7_fill, changed arg interpretation of s7_copy, s7_dynamic_wind.
 * 30-Mar:    s7_eval_c_string_with_environment (repl experiment).
 * 19-Mar:    repl.scm.
 * 28-Feb:    s7_vector_print_length -> s7_print_length, set case also.
 * 25-Feb:    s7_closure_* funcs to replace clumsy (deprecated) s7_procedure_source.
 * 29-Jan:    changed args to s7_new_type_x (added s7_scheme arg, fill! takes s7_function).
 * 14-Jan-15: make-iterator, iterator?
 * --------
 * 26-Dec:    s7_arity replaces s7_procedure_arity.  s7_define_integer_function. deprecate s7_procedure_name.
 * 5-Nov:     s7_shadow_rootlet and s7_set_shadow_rootlet.
 * 30-Aug:    s7_make_safe_function (for cload.scm).
 * 25-July:   define and friends now return the value, not the symbol.
 *            procedure_with_setter -> dilambda.
 *            environment -> let.  All the replaced names are deprecated.
 * 30-June:   s7_method.
 * 16-June:   remove unoptimize and s7_unoptimize.
 * 14-May:    s7_define_safe_function_star.  Removed s7_catch_all.
 * 22-Apr:    remove s7_apply_n_10, s7_is_valid_pointer, s7_keyword_eq_p.
 * 5-Mar:     s7_heap_size, s7_gc_freed.
 * 22-Jan-14: s7_vector_ref_object_value_checked.
 * --------
 * 8-Nov:     s7_symbol_documentation, s7_define_constant_with_documentation.
 * 17-Oct:    bignum-precision (procedure-with-setter) is now an integer variable named *bignum-precision*.
 * 28-Aug:    s7_int|float_vector_elements (homogenous vectors), libc.scm.
 * 16-Aug:    ~W directive in format, make-shared-vector.
 * 23-Jul:    s7_autoload_set_names, libm.scm, libdl.scm, libgdbm.scm, r7rs.scm, s7libtest.scm.
 * 21-Jul:    s7_is_valid (replaces deprecated s7_is_valid_pointer).
 * 24-Jun:    some bool-related changes for Windows Visual C++, including change to s7_begin_hook.
 * 3-June:    s7_autoload.
 * 28-May:    export s7_is_provided.  Added s7_scheme* arg to s7_procedure_environment.
 * 21-May:    equality predicate optional arg in make-hash-table.
 * 14-May:    glistener.c, glistener.h, s7_symbol_table_find_name (for glistener).
 * 2-May:     r7rs changes: flush-output-port, vector-append, read|write-string, boolean=?, symbol=?.
 *              start/end args for string-fill!, vector-fill!, string->list, vector->list, and copy.
 *              exit, emergency-exit.
 * 7-Apr:     removed s7_scheme* arg from s7_slot_value, added s7_is_local_variable.
 * 25-Mar:    char-position, string-position, environment-ref, environment-set! added to the scheme side.
 * 9-Jan-13:  s7_cos, s7_sin, other optimization changes.
 * --------
 * 24-Dec:    s7_set_object_array_info and other such changes.
 * 20-Nov:    removed s7_set_error_exiter and s7_error_and_exit which I think have never been used.
 * 22-Oct:    changed args to s7_function_class and s7_function_set_class.
 * 22-Aug:    symbol->dynamic-value.
 * 10-Aug:    exported s7_outer_environment.
 * 6-Aug:     removed WITH_OPTIMIZATION.
 * 25-July:   environment (in scheme). s7_vector_ref_n and s7_vector_set_n. s7_copy.
 *              added s7_scheme arg to s7_number_to_real|integer.
 * 16-July:   s7_function_returns_temp (an experiment).
 * 2-July:    s7_object_set_* functions.
 * 11-June:   throw.
 * 4-June.    s7_object_environment.
 * 31-May:    added s7_scheme argument to all the optimizer chooser functions.
 * 24-May:    open-environment?
 * 17-May:    arity, aritable?
 *            removed trace and untrace.
 * 14-May:    s7_list. s7_procedure_set_setter.  Removed s7_procedure_getter.
 *              procedure-setter is settable: removed most of procedure-with-setter.
 *            make-type replaced by open-environment.
 * 11-May:    s7 2.0: hook implementation changed completely.
 *            s7_environment_ref|set.
 * 4-May:     *error-info* replaced by error-environment, and stacktrace has changed.
 * 22-Apr:    #_<name> = startup (built-in) value of name
 * 17-Apr:    with-baffle.
 * 14-Apr:    WITH_SYSTEM_EXTRAS (default 0) has additional OS and IO functions:
 *              directory? file-exists? delete-file getenv directory->list system
 * 26-Mar:    "@" as exponent, WITH_AT_SIGN_AS_EXPONENT switch (default is 1).
 * 18-Mar:    removed *trace-hook*.
 * 6-Feb:     random-state?, hash-table-iterator?, and morally-equal?
 * 18-Jan:    s7_environment_to_list and environment->list return just the local environment's bindings.
 *            outer-environment returns the environment enclosing its argument (an environment).
 *            environments are now applicable objects. 
 *            added the object system example to s7.html.
 * 12-Jan:    added reverse argument to s7_new_type_x.  This is needed because an object might implement
 *              the apply and set methods, but they might refer to different things.
 * 6-Jan-12:  added (scheme side) logbit?.
 * --------
 * 21-Dec:    s7_eval, s7_make_slot, s7_slot_set_value.
 *            changed s7_symbol_slot to s7_slot, and s7_symbol_slot_value to s7_slot_value.
 * 26-Oct:    s7_procedure_name.
 * 6-Oct:     changed s7_make_closure args: split the code argument in two (args and body).
 *               s7_make_closure(... code ...) is now s7_make_closure(... car(code), cdr(code) ...)
 *            s7_is_environment.
 * 19-Aug:    s7_function_chooser_data.
 * 11-Aug:    s7_symbol_accessor functions. s7_cxxxxr.
 * 9-Aug:     s7_function_chooser, s7_function_choice, s7_function_choice_set_direct.
 * 20-Jul:    s7_function_class, s7_function_set_class, and s7_function_set_chooser.
 * 14-Jul:    removed thread and profiling support.
 * 5-June:    s7_define_safe_function and s7_unoptimize exported; added unoptimize function in scheme.
 * 30-May:    environment->list and s7_environment_to_list since environments are no longer alists internally.
 * 26-May:    added s7_scheme argument to s7_procedure_setter and getter (old names had "with_setter_").
 * 28-Apr:    s7_help.
 * 5-Apr:     pair-line-number.
 * 14-Mar:    s7_make_random_state, optional state argument to s7_random, random-state->list, s7_random_state_to_list.
 * 10-Feb:    s7_vector_print_length, s7_set_vector_print_length.
 * 7-Feb:     s7_begin_hook, s7_set_begin_hook.
 * 25-Jan:    s7_is_thread, s7_thread, s7_make_thread, s7_thread_s7, s7_thread_data. 
 *               s7_is_lock, s7_make_lock, s7_lock.
 *               changed s7_thread_variable_value to s7_thread_variable.
 * 23-Jan:    removed (scheme-level) quit.
 * 17-Jan-11: make-hash-table-iterator.
 *            map and for-each accept any applicable object as the first argument.
 *            format's ~{...~} directive can handle any applicable object.
 * --------
 * 17-Dec:    removed unquote-splicing; replaced by (unquote (apply values ...)).
 * 12-Dec:    environment? 
 * 7-Dec:     member and assoc have an optional 3rd arg, the comparison function.
 * 1-Dec:     *gc-stats* in Scheme, s7_gc_stats in C.
 *            gmp and gtk-repl examples in s7.html.
 * 21-Nov:    Load C module example in s7.html.
 * 12-Nov:    *trace-hook*, *load-hook*, *error-hook*, and *unbound-variable-hook* are now s7 hooks.
 * 9-Nov:     hooks: C side: s7_is_hook, s7_make_hook, s7_hook_apply, s7_hook_functions, s7_hook_arity, s7_hook_documentation.
 *                   s7 side: hook?, make-hook, hook, hook-apply, hook-functions, hook-arity, hook-documentation.
 * 8-Nov:     Closure defined in C example in s7.html.
 * 23-Oct:    s7_call_with_location for better error reporting.
 * 19-Oct:    *stdin*, *stdout*, *stderr* for default IO ports (rather than nil which is ambiguous).
 * 14-Oct:    removed special variable support.
 * 30-Sep:    setters for current-input-port, current-output-port, and current-error-port.
 * 30-Aug:    :allow-other-keys in define*.
 * 10-Aug:    added boolean argument use_write to s7_object_to_string (true=write, false=display).
 * 30-July:   special macro for access to dynamic binding.
 *            s7_symbol_special_value for C-side access to dynamic bindings.
 *            s7_is_macro.
 *            port-closed? returns #t if its argument (a port) is closed.
 * 22-July:   s7_make_character takes unsigned int, rather than int.
 *            added symbol function for funny symbol names.
 * 12-July:   initial-environment.
 * 7-July:    removed force and delay: use slib.
 * 3-July:    new backquote implementation.
 * 28-June:   syntactic keywords (e.g. lambda) are applicable.
 * 7-June:    changed key arg in s7_hash_table_ref|set to be s7_pointer, not const char*.
 *            hash-tables can now handle any s7 object as the key.
 *            map and for-each now pass a hash-table entry to the function, rather than an internal alist.
 *            reverse of a hash-table reverses the keys and values (i.e. old value becomes new key, etc).
 * 2-June:    removed procedure-with-setter-setter-arity and folded that info into procedure-arity (use cdddr).
 * 22-May:    multidimensional vectors are no longer optional.
 * 9-May:     s7_read_char and s7_peek_char have to return an int, not a char (<eof>=-1, but 255 is a legit char).
 *            s7_write_char and s7_open_output_function have similar changes.
 * 3-May:     *#readers* to customize #... reading.  Also nan? and infinite?.
 *            multidimensional vector constants using #nD(...): (#2D((1 2 3) (4 5 6)) 0 0) -> 1.
 * 13-Apr:    removed hash-table|vector|string-for-each -- these are handled by for-each.
 *            also removed vector-map -- map is generic, but always returns a list.
 * 12-Apr:    removed immutable constant checks -- see s7.html.
 * 7-Apr:     *unbound-variable-hook*.
 *            augment-environment and s7_augment_environment.
 * 29-Mar:    symbol-access, s7_symbol_access, s7_symbol_set_access.
 *            C example of notification in s7.html.
 * 25-Mar:    make-type.  s7_is_equal now includes an s7_scheme pointer as its first argument.
 * 24-Mar:    s7_is_defined.
 * 19-Mar:    removed encapsulation mechanism and s7_define_set_function.
 * 18-Mar:    added macro?.
 * 27-Feb:    removed r4rs-style macro syntax.
 * 17-Feb:    s7_number_to_integer.
 * 20-Jan-10: removed the stack function.
 * --------
 * 16-Dec:    hash-table-for-each.
 * 1-Dec:     mpc versions before 0.8.0 are no longer supported.
 * 24-Nov:    define-macro* and defmacro*.
 *            force and delay included only if WITH_FORCE set, promise? removed.
 * 17-Nov:    s7_is_boolean no longer takes the s7_scheme argument.
 * 7-Nov:     s7_vector_dimensions, s7_vector_offsets, example of use.
 * 3-Nov:     s7_vector_rank.
 * 30-Oct:    *trace-hook*.
 * 12-Oct:    s7_port_filename.
 * 5-Oct:     s7_c_pointer and friends.
 * 14-Sep:    s7_values, s7_make_continuation, and a better interrupt example.
 *            vector-for-each, vector-map, string-for-each.
 * 7-Sep:     s7_open_input_function. with-environment. receive.
 * 3-Sep:     s7.html, s7-slib-init.scm. 
 *            s7_stacktrace in s7.h.
 * 27-Aug:    vector and hash-table sizes are now s7_ints, rather than ints.
 * 20-Aug:    s7_remove_from_heap.
 * 17-Aug:    *error-info*.
 * 14-Aug:    define-expansion.
 * 7-Aug:     s7_define_function_with_setter. 
 *            s7_quit and example of signal handling.
 * 6-Aug:     encapsulation.  s7_define_set_function.  s7_new_type_x.  
 *            generic function: copy, and length is generic.
 * 1-Aug:     lower-case versions of s7_T and friends.
 *            s7_define_macro. macroexpand.
 *            strings are set-applicable (like vectors).
 * 31-Jul:    *error-hook*.
 * 30-Jul:    changed backtrace handling: removed backtrace stuff, added stacktrace.
 *            removed gc-verbose and load-verbose replaced by *load-hook*.
 * 23-Jul:    __func__.
 * 20-Jul:    trace and untrace.
 * 14-Jul:    replaced s7_make_closure_star with s7_define_function_star.
 * 29-Jun:    s7_format declaration.
 * 12-May:    s7_is_constant.
 * 20-Apr:    changed rationalize to be both r5rs-acceptable and fast.
 * 6-Apr:     added s7_make_permanent_string.
 * 14-Mar:    removed s7_local_gc_protect and s7_local_gc_unprotect.
 * 4-Mar:     multidimensional and applicable vectors.
 * 1-Mar:     s7_random added to s7.h.
 * 29-Jan:    s7_is_bignum and friends.
 * 26-Jan:    added s7_scheme arg to s7_vector_fill.
 * 16-Jan:    s7_is_ulong_long and friends for C pointers in 64-bit situations.
 * 9-Jan-09   multiprecision arithmetic (gmp, mpfr, mpc) on the WITH_GMP switch
 * --------
 * 29-Dec:    "+" specialization example, s7_apply_function.
 * 3-Dec:     s7_open_output_function.
 * 30-Nov:    s7_wrong_number_of_args_error.
 * 24-Nov:    changed s7_make_counted_string to s7_make_string_with_length.
 *              also added built-in format and define*
 * 10-Nov:    s7_define_constant,
 *              built-in (scheme-side) pi, most-positive-fixnum, most-negative-fixnum
 * 7-Nov:     removed s7_is_immutable and friends, s7_reverse_in_place.
 *              removed the s7_pointer arg to s7_gc_on.
 *              added s7_UNSPECIFIED
 * 25-Oct:    added name arg to s7_make_procedure_with_setter, 
 *              and s7_scheme arg to new_type print func.
 * 1-Oct-08   version 1.0
 */
