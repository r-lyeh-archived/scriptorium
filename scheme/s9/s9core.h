/*
 * S9 Core Toolkit
 * By Nils M Holm, 2007-2015
 * In the public domain
 */

/*
 * Ugly prelude to figure out if
 * we are compiling on a Un*x system.
 */

#ifdef __NetBSD__
 #ifndef unix
  #define unix
 #endif
#endif

#ifdef __unix
 #ifndef unix
  #define unix
 #endif
#endif

#ifdef __linux
 #ifndef unix
  #define unix
 #endif
#endif

#ifdef __GNUC__
 #ifndef unix
  #define unix
 #endif
#endif

#ifdef __clang__
 #ifndef unix
  #define unix
 #endif
#endif

#ifndef unix
 #ifndef plan9
  #error "Either 'unix' or 'plan9' must be #defined."
 #endif
#endif

/*
 * Tell later MSC compilers to let us use the standard CLIB API.
 * Blake McBride < b l a k e @ m c b r i d e . n a m e >
 */

#ifdef _MSC_VER
 #if _MSC_VER > 1200
  #ifndef _CRT_SECURE_NO_DEPRECATE
   #define _CRT_SECURE_NO_DEPRECATE
  #endif
 #endif
 #ifndef _POSIX_
  #define _POSIX_
 #endif
#endif

#ifdef plan9
 #include <u.h>
 #include <libc.h>
 #include <stdio.h>
 #include <ctype.h>
 #define bye(x)	exits((x)? "error": NULL)
 #define ptrdiff_t int
#endif

#ifdef unix
 #include <stdlib.h>
 #include <stddef.h>
 #include <stdio.h>
 #include <string.h>
 #include <ctype.h>
 #define bye(x)	exit((x)? EXIT_FAILURE: EXIT_SUCCESS);
#endif

/* A "cell" must be large enough to hold a pointer */
#define cell	ptrdiff_t

/* Default memory limit in K-nodes, 0 = none */
#define NODE_LIMIT	14013
#define VECTOR_LIMIT	14013

/* Pick one ... */
/* #define BITS_PER_WORD_64 */
/* #define BITS_PER_WORD_32 */
/* #define BITS_PER_WORD_16 */

/* ... or try some magic constants (unreliable, though) ... */

#ifdef __amd64__
 #define BITS_PER_WORD_64
#endif
#ifdef __amd64
 #define BITS_PER_WORD_64
#endif
#ifdef __x86_64__
 #define BITS_PER_WORD_64
#endif
#ifdef __x86_64
 #define BITS_PER_WORD_64
#endif

/* ... or assume a reasonable default */
#ifndef BITS_PER_WORD_16
 #ifndef BITS_PER_WORD_32
  #ifndef BITS_PER_WORD_64
   #define BITS_PER_WORD_32
  #endif
 #endif
#endif

/*
 * Node tags
 */

#define ATOM_TAG	0x01	/* Atom, Car = type, CDR = next */
#define MARK_TAG	0x02	/* Mark */
#define STATE_TAG	0x04	/* State */
#define VECTOR_TAG	0x08	/* Vector, Car = type, CDR = content */
#define PORT_TAG	0x10	/* Atom is an I/O port (with ATOM_TAG) */
#define USED_TAG	0x20	/* Port: used flag */
#define LOCK_TAG	0x40	/* Port: locked (do not close) */
#define CONST_TAG	0x80	/* Node is immutable */

/*
 * Integer segment specs
 */

#ifdef BITS_PER_WORD_64
 #define DIGITS_PER_CELL	18
 #define INT_SEG_LIMIT		1000000000000000000L
 #define MANTISSA_SEGMENTS	1
#else
 #ifdef BITS_PER_WORD_32
  #define DIGITS_PER_CELL	9
  #define INT_SEG_LIMIT		1000000000L
  #define MANTISSA_SEGMENTS	2
 #else
  #ifdef BITS_PER_WORD_16
   #define DIGITS_PER_CELL	4
   #define INT_SEG_LIMIT	10000
   #define MANTISSA_SEGMENTS	2
  #else
   #error "BITS_PER_WORD_* undefined (this should not happen)"
  #endif
 #endif
#endif

/*
 * Real number mantissa size
 */

#define MANTISSA_SIZE		(MANTISSA_SEGMENTS * DIGITS_PER_CELL)

/*
 * Special objects
 */

#define special_value_p(x)	((x) < 0)
#define NIL			(-1)
#define TRUE			(-2)
#define FALSE			(-3)
#define END_OF_FILE		(-4)
#define UNDEFINED		(-5)
#define UNSPECIFIC		(-6)
#define NOEXPR			(-7)

/*
 * Types
 */

#define T_ANY		(-10)
#define T_BOOLEAN	(-11)
#define T_CHAR		(-12)
#define T_INPUT_PORT	(-13)
#define T_INTEGER	(-14)
#define T_LIST		(-17)
#define T_OUTPUT_PORT	(-15)
#define T_PAIR		(-16)
#define T_PRIMITIVE	(-18)
#define T_FUNCTION	(-19)
#define T_REAL		(-20)
#define T_STRING	(-21)
#define T_SYMBOL	(-22)
#define T_SYNTAX	(-23)
#define T_VECTOR	(-24)
#define T_CONTINUATION	(-25)

#define USER_SPECIALS		(-100)

/*
 * Short cuts for primitive procedure definitions
 * Yes, ___ violates the C standard, but it's too tempting
 */

#ifdef S9FES
 #define BOL T_BOOLEAN
 #define CHR T_CHAR
 #define INP T_INPUT_PORT
 #define INT T_INTEGER
 #define LST T_LIST
 #define OUP T_OUTPUT_PORT
 #define PAI T_PAIR
 #define FUN T_FUNCTION
 #define REA T_REAL
 #define STR T_STRING
 #define SYM T_SYMBOL
 #define VEC T_VECTOR
 #define ___ T_ANY
#endif

/*
 * Globals
 */

struct Counter {
	int	n, n1k, n1m, n1g, n1t;
};

#define counter	struct Counter

struct Primitive_function {
	char	*name;
	cell	(*handler)(cell expr);
	int	min_args;
	int	max_args;	/* -1 = variadic */
	int	arg_types[3];
};

#define PRIM    struct Primitive_function

/*
 * I/O
 */

#define nl()		prints("\n")
#define reject(c)	ungetc(c, Ports[Input_port])
#define readc()		getc(Ports[Input_port])

/*
 * Access to fields of atoms
 */

#define string(n)	((char *) &Vectors[Cdr[n]])
#define string_len(n)	(Vectors[Cdr[n] - 1])
#define symbol_name(n)	(string(n))
#define symbol_len(n)	(string_len(n))
#define vector(n)	(&Vectors[Cdr[n]])
#define vector_link(n)	(Vectors[Cdr[n] - 3])
#define vector_index(n)	(Vectors[Cdr[n] - 2])
#define vector_size(k)	(((k) + sizeof(cell)-1) / sizeof(cell) + 3)
#define vector_len(n)	(vector_size(string_len(n)) - 3)
#define port_no(n)	(cadr(n))
#define char_value(n)	(cadr(n))
#define prim_slot(n)	(cadr(n))
#define prim_info(n)	(&Primitives[prim_slot(n)])

/*
 * Nested lists
 */

#define car(x)          (Car[x])
#define cdr(x)          (Cdr[x])
#define caar(x)         (Car[Car[x]])
#define cadr(x)         (Car[Cdr[x]])
#define cdar(x)         (Cdr[Car[x]])
#define cddr(x)         (Cdr[Cdr[x]])
#define caaar(x)        (Car[Car[Car[x]]])
#define caadr(x)        (Car[Car[Cdr[x]]])
#define cadar(x)        (Car[Cdr[Car[x]]])
#define caddr(x)        (Car[Cdr[Cdr[x]]])
#define cdaar(x)        (Cdr[Car[Car[x]]])
#define cdadr(x)        (Cdr[Car[Cdr[x]]])
#define cddar(x)        (Cdr[Cdr[Car[x]]])
#define cdddr(x)        (Cdr[Cdr[Cdr[x]]])
#define caaaar(x)       (Car[Car[Car[Car[x]]]])
#define caaadr(x)       (Car[Car[Car[Cdr[x]]]])
#define caadar(x)       (Car[Car[Cdr[Car[x]]]])
#define caaddr(x)       (Car[Car[Cdr[Cdr[x]]]])
#define cadaar(x)       (Car[Cdr[Car[Car[x]]]])
#define cadadr(x)       (Car[Cdr[Car[Cdr[x]]]])
#define caddar(x)       (Car[Cdr[Cdr[Car[x]]]])
#define cadddr(x)       (Car[Cdr[Cdr[Cdr[x]]]])
#define cdaaar(x)       (Cdr[Car[Car[Car[x]]]])
#define cdaadr(x)       (Cdr[Car[Car[Cdr[x]]]])
#define cdadar(x)       (Cdr[Car[Cdr[Car[x]]]])
#define cdaddr(x)       (Cdr[Car[Cdr[Cdr[x]]]])
#define cddaar(x)       (Cdr[Cdr[Car[Car[x]]]])
#define cddadr(x)       (Cdr[Cdr[Car[Cdr[x]]]])
#define cdddar(x)       (Cdr[Cdr[Cdr[Car[x]]]])
#define cddddr(x)       (Cdr[Cdr[Cdr[Cdr[x]]]])

/*
 * Type predicates
 */

#define eof_p(n)	((n) == END_OF_FILE)
#define undefined_p(n)	((n) == UNDEFINED)
#define unspecific_p(n)	((n) == UNSPECIFIC)

#define boolean_p(n)	((n) == TRUE || (n) == FALSE)

#define constant_p(n)	(!special_value_p(n) && (Tag[n] & CONST_TAG))

#define integer_p(n) \
	(!special_value_p(n) && (Tag[n] & ATOM_TAG) && Car[n] == T_INTEGER)
#define number_p(n) \
	(!special_value_p(n) && (Tag[n] & ATOM_TAG) && \
		(Car[n] == T_REAL || Car[n] == T_INTEGER))
#define primitive_p(n) \
	(!special_value_p(n) && (Tag[n] & ATOM_TAG) && Car[n] == T_PRIMITIVE)
#define function_p(n) \
	(!special_value_p(n) && (Tag[n] & ATOM_TAG) && Car[n] == T_FUNCTION)
#define continuation_p(n) \
	(!special_value_p(n) && (Tag[n] & ATOM_TAG) && \
		Car[n] == T_CONTINUATION)
#define real_p(n) \
	(!special_value_p(n) && (Tag[n] & ATOM_TAG) && Car[n] == T_REAL)
#define char_p(n) \
	(!special_value_p(n) && (Tag[n] & ATOM_TAG) && Car[n] == T_CHAR)
#define syntax_p(n) \
	(!special_value_p(n) && (Tag[n] & ATOM_TAG) && Car[n] == T_SYNTAX)
#define input_port_p(n) \
	(!special_value_p(n) && (Tag[n] & ATOM_TAG) && (Tag[n] & PORT_TAG) \
	 && Car[n] == T_INPUT_PORT)
#define output_port_p(n) \
	(!special_value_p(n) && (Tag[n] & ATOM_TAG) && (Tag[n] & PORT_TAG) \
	 && Car[n] == T_OUTPUT_PORT)

#define symbol_p(n) \
	(!special_value_p(n) && (Tag[n] & VECTOR_TAG) && Car[n] == T_SYMBOL)
#define vector_p(n) \
	(!special_value_p(n) && (Tag[n] & VECTOR_TAG) && Car[n] == T_VECTOR)
#define string_p(n) \
	(!special_value_p(n) && (Tag[n] & VECTOR_TAG) && Car[n] == T_STRING)

#define atom_p(n) \
	(special_value_p(n) || (Tag[n] & ATOM_TAG) || (Tag[n] & VECTOR_TAG))

#define pair_p(x) (!atom_p(x))

/*
 * Allocators
 */

#define cons(pa, pd)		cons3((pa), (pd), 0)
#define new_atom(pa, pd)	cons3((pa), (pd), ATOM_TAG)
#define save(n)			(Stack = cons((n), Stack))

/*
 * Bignum arithmetics
 */

#define bignum_negative_p(a)	((cadr(a)) < 0)
#define bignum_zero_p(a)	((cadr(a)) == 0)
#define bignum_positive_p(a)	((cadr(a)) > 0)

/*
 * Real number structure
 */

#define Real_flags(x)		(cadr(x))
#define Real_exponent(x)	(caddr(x))
#define Real_mantissa(x)	(cdddr(x))

#define REAL_NEGATIVE   0x01

#define Real_negative_flag(x)	(Real_flags(x) & REAL_NEGATIVE)

/*
 * Real-number arithmetics
 */

#define Real_zero_p(x) \
	(car(Real_mantissa(x)) == 0 && cdr(Real_mantissa(x)) == NIL)

#define Real_negative_p(x) \
	(Real_negative_flag(x) && !Real_zero_p(x))

#define Real_positive_p(x) \
	(!Real_negative_flag(x) && !Real_zero_p(x))

#define Real_negate(a) \
	Make_quick_real(Real_flags(a) & REAL_NEGATIVE?	\
			Real_flags(a) & ~REAL_NEGATIVE: \
			Real_flags(a) |  REAL_NEGATIVE, \
			Real_exponent(a), Real_mantissa(a))

/*
 * Globals
 */

extern cell	*Car,
		*Cdr;
extern char	*Tag;

extern cell	*Vectors;

extern cell	Stack;

extern PRIM	*Primitives;

extern cell	Zero,
		One,
		Two;

extern cell	Epsilon;

extern FILE	*Ports[];

extern int	Input_port,
		Output_port,
		Error_port;

/*
 * Prototypes
 */

cell	apply_prim(cell f, cell a);
long	asctol(char *s);
cell	bignum_abs(cell a);
cell	bignum_add(cell a, cell b);
cell	bignum_divide(cell a, cell b);
int	bignum_equal_p(cell a, cell b);
int	bignum_even_p(cell a);
int	bignum_less_p(cell a, cell b);
cell	bignum_multiply(cell a, cell b);
cell	bignum_negate(cell a);
cell	bignum_shift_left(cell a, int fill);
cell	bignum_shift_right(cell a);
cell	bignum_subtract(cell a, cell b);
cell	bignum_to_int(cell x);
cell	bignum_to_real(cell a);
cell	bignum_to_string(cell x);
int	blockread(char *s, int k);
void	blockwrite(char *s, int k);
void	close_port(int port);
cell	cons3(cell pcar, cell pcdr, int ptag);
void	cons_stats(int x);
cell	copy_string(cell x);
void	count(counter *c);
char	*dump_image(char *path, char *magic);
void	exponent_chars(char *s);
void	fatal(char *msg);
cell	find_symbol(char *s);
cell	flat_copy(cell n, cell *lastp);
void	flush(void);
int	gc(void);
int	gcv(void);
void	gc_verbosity(int n);
void	get_counters(counter **nc, counter **cc, counter **gc);
void	mem_error_handler(void (*h)(int src));
void	image_vars(cell **v);
cell	input_port(void);
int	integer_string_p(char *s);
cell	intern_symbol(cell y);
int	io_status(void);
void	io_reset(void);
int	length(cell n);
char	*load_image(char *path, char *magic);
int	lock_port(cell port);
cell	make_char(int c);
cell	make_integer(cell i);
cell	make_norm_real(int flags, cell exp, cell mant);
cell	make_port(int portno, cell type);
cell	make_primitive(PRIM *p);
cell	Make_real(int flags, cell exp, cell mant);
cell	make_real(int sign, cell exp, cell mant);
cell	make_string(char *s, int k);
cell	make_symbol(char *s, int k);
cell	make_vector(int k);
int	new_port(void);
cell	new_vec(cell type, int size);
int	open_input_port(char *path);
int	open_output_port(char *path, int append);
cell	output_port(void);
void	prints(char *s);
int	printer_limit(void);
void	print_bignum(cell n);
void	print_expanded_real(cell n);
void	print_real(cell n);
void	print_sci_real(cell n);
cell	read_counter(counter *c);
cell	real_abs(cell a);
cell	real_add(cell a, cell b);
cell	real_ceil(cell x);
cell	real_divide(cell a, cell b);
int	real_equal_p(cell a, cell b);
cell	real_exponent(cell x);
cell	real_floor(cell x);
cell	real_integer_p(cell x);
int	real_less_p(cell a, cell b);
cell	real_mantissa(cell x);
cell	real_multiply(cell a, cell b);
cell	real_negate(cell a);
cell	real_negative_p(cell a);
cell	real_positive_p(cell a);
cell	real_subtract(cell a, cell b);
cell	real_to_bignum(cell r);
cell	real_to_string(cell r, int mode);
cell	real_trunc(cell x);
cell	real_zero_p(cell a);
void	reset_counter(counter *c);
void	reset_std_ports(void);
void	run_stats(int x);
void	s9fini(void);
void	s9init(cell **extroots);
cell	set_input_port(cell port);
void	set_node_limit(int k);
cell	set_output_port(cell port);
void	set_printer_limit(int k);
void	set_vector_limit(int k);
int	string_numeric_p(char *s);
cell	string_to_bignum(char *s);
cell	string_to_number(char *s);
cell	string_to_real(char *s);
cell	string_to_symbol(cell x);
cell	symbol_ref(char *s);
cell	symbol_table(void);
cell	symbol_to_string(cell x);
char	*typecheck(cell f, cell a);
int	unlock_port(cell port);
cell	unsave(int k);

#ifdef S9FES
void	add_primitives(char *name, PRIM *p);
cell	error(char *msg, cell expr);
cell	integer_value(char *src, cell x);
#endif
