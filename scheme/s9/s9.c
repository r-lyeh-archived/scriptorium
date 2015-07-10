/*
 * Scheme 9 from Empty Space, Refactored
 * By Nils M Holm, 2007-2015
 * In the public domain
 */

#define VERSION "2015-07-01"

#define S9FES
#include "s9core.h"

#ifdef unix
 #include <signal.h>
 #define handle_sigquit()	signal(SIGQUIT, keyboard_quit)
 #define handle_sigterm()	signal(SIGTERM, terminated)
 #define handle_sigint()	signal(SIGINT, keyboard_interrupt)
#endif
#ifdef plan9
 #define handle_sigquit()
 #define handle_sigterm()
 #define handle_sigint()	notify(keyboard_interrupt)
#endif

#ifndef LIBRARY_PATH
 #define LIBRARY_PATH \
		"."                             \
		":lib"                          \
		":ext"                          \
		":contrib"                      \
		":~/.s9fes"                     \
		":/usr/local/share/s9fes"
#endif

#define TOKEN_LENGTH		1024
#define MAX_PORTS		32
#define MAX_IO_DEPTH		65536	/* Reduce on 16-bit systems! */
#define HASH_THRESHOLD		5
#define MAX_CALL_TRACE		5

/*
 * Evaluator states
 */

enum EVAL_STATES {
	EV_ATOM,	/* Evaluating atom */
	EV_ARGS,	/* Evaluating argument list */
	EV_BETA,	/* Evaluating procedure body */
	EV_IF_PRED,	/* Evaluating predicate of IF */
	EV_SET_VAL,	/* Evaluating value of SET! and DEFINE */
	EV_MACRO,	/* Evaluating value of DEFINE-SYNTAX */
	EV_BEGIN,	/* Evaluating expressions of BEGIN */
	EV_AND,		/* Evaluating arguments of AND */
	EV_OR,		/* Evaluating arguments of OR */
	EV_COND		/* Evaluating clauses of COND */
};

/*
 * Binding structure
 */

#define make_binding(v, a)	(cons((v), (a)))
#define binding_box(x)		(x)
#define binding_value(x)	(cdr(x))
#define box_value(x)		(cdr(x))

/*
 * Internal specials
 */

#define RPAREN	(USER_SPECIALS-1)
#define RBRACK	(USER_SPECIALS-2)
#define DOT	(USER_SPECIALS-3)

/*
 * Globals
 */

static char	S9magic[17];

static cell	Stack_bottom;
static cell	State_stack;
static cell	Tmp_car, Tmp_cdr;
static cell	Tmp;
static cell	Program;
static cell	Environment;
static cell	Acc;
static cell	Apply_magic, Callcc_magic;

static int	Level;
static int	Load_level;
static int	Displaying;

static cell     Called_procedures[MAX_CALL_TRACE];
static int      Proc_ptr;
static cell     File_list;
static int      Line_no;
static int	Opening_line;

static cell     Trace_list;

static int      Quiet_mode;

static int	Eval_stats;
static counter	Reductions;

static volatile int     Error_flag, Intr_flag;

cell	Argv;

/* Short cuts for accessing predefined symbols */
static cell	S_and, S_arguments, S_arrow, S_begin, S_cond,
		S_define, S_define_syntax, S_else, S_extensions,
		S_host_system, S_if, S_lambda, S_latest, S_library_path,
		S_loading, S_or, S_quasiquote, S_quote, S_set_b,
		S_unquote, S_unquote_splicing;

/*
 * I/O
 */

#define readc_ci()     tolower(readc())

/*
 * Type predicates
 */

#define special_p(n)	((n) == S_quote	  || \
			 (n) == S_begin	  || \
			 (n) == S_if	  || \
			 (n) == S_cond	  || \
			 (n) == S_and	  || \
			 (n) == S_or	  || \
			 (n) == S_lambda  || \
			 (n) == S_set_b	  || \
			 (n) == S_define  || \
			 (n) == S_define_syntax)

#define auto_quoting_p(n) atom_p(n)

/*
 * Rib structure
 */

#define rib_args(x)     (car(x))
#define rib_append(x)   (cadr(x))
#define rib_result(x)   (caddr(x))
#define rib_source(x)   (cdddr(x))

/*
 * Allocators
 */

#define save_state(v)   (State_stack = cons3((v), State_stack, ATOM_TAG))

/*
 * Error Handling
 */

void reset_tty(void) {
#ifdef CURSES_RESET
	cell pp_curs_endwin(cell);
	pp_curs_endwin(NIL);
#endif
}

void quit(int n) {
	reset_tty();
	bye(n);
}

void print_form(cell n);

void print_error_form(cell n) {
	set_printer_limit(50);
	print_form(n);
	set_printer_limit(0);
}

void print_calltrace(void) {
	int	i, j;

	for (i=0; i<MAX_CALL_TRACE; i++)
		if (Called_procedures[i] != NIL)
			break;
	if (i == MAX_CALL_TRACE)
		return;
	prints("call trace:");
	i = Proc_ptr;
	for (j=0; j<MAX_CALL_TRACE; j++) {
		if (i >= MAX_CALL_TRACE)
			i = 0;
		if (Called_procedures[i] != NIL) {
			prints(" ");
			print_form(Called_procedures[i]);
		}
		i++;
	}
	nl();
}

cell error(char *msg, cell expr) {
	int	oport;
	char	buf[100];

	if (Error_flag)
		return UNSPECIFIC;
	oport = output_port();
	set_output_port(Quiet_mode? 2: 1);
	Error_flag = 1;
	prints("error: ");
	if (Load_level) {
		if (File_list != NIL) {
			print_form(car(File_list));
			prints(": ");
		}
		sprintf(buf, "%d: ", Line_no);
		prints(buf);
	}
	prints(msg);
	if (expr != NOEXPR) {
		prints(": ");
		Error_flag = 0;
		print_error_form(expr);
		Error_flag = 1;
	}
	nl();
	print_calltrace();
	set_output_port(oport);
	if (Quiet_mode)
		quit(1);
	return UNSPECIFIC;
}

/*
 * Reader
 */

cell read_form(int flags);

cell read_list(int flags, int delim) {
	cell	n,	/* Node read */
		m,	/* List */
		a;	/* Used to append nodes to m */
	int	c;	/* Member counter */
	cell	new;
	char	badpair[] = "malformed pair";
	char	msg[80];

	if (!Level)
		Opening_line = Line_no;
	if (++Level > MAX_IO_DEPTH) {
		error("reader: too many nested lists or vectors", NOEXPR);
		return NIL;
	}
	m = cons3(NIL, NIL, flags);	/* root */
	save(m);
	a = NIL;
	c = 0;
	while (1) {
		if (Error_flag) {
			unsave(1);
			return NIL;
		}
		n = read_form(flags);
		if (n == END_OF_FILE)  {
			if (Load_level) {
				unsave(1);
				return END_OF_FILE;
			}
			sprintf(msg, "missing ')', started in line %d",
					Opening_line);
			error(msg, NOEXPR);
		}
		if (n == DOT) {
			if (c < 1) {
				error(badpair, NOEXPR);
				continue;
			}
			n = read_form(flags);
			cdr(a) = n;
			if (n == delim || read_form(flags) != delim) {
				error(badpair, NOEXPR);
				continue;
			}
			unsave(1);
			Level--;
			return m;
		}
		if (n == RPAREN || n == RBRACK) {
			if (n != delim)
				error(n == RPAREN?
				  "list starting with `[' ended with `)'":
				  "list starting with `(' ended with `]'",
				  NOEXPR);
			break;
		}
		if (a == NIL)
			a = m;		/* First member: insert at root */
		else
			a = cdr(a);	/* Subsequent members: append */
		car(a) = n;
		new = cons3(NIL, NIL, flags); /* Space for next member */
		cdr(a) = new;
		c++;
	}
	Level--;
	if (a != NIL)
		cdr(a) = NIL;	/* Remove trailing empty node */
	unsave(1);
	return c? m: NIL;
}

cell quote(cell n, cell quotation) {
	cell	q;

	q = cons(n, NIL);
	return cons(quotation, q);
}

int strcmp_ci(char *s1, char *s2) {
	int	c1, c2;

	while (1) {
		c1 = tolower((int) *s1++);
		c2 = tolower((int) *s2++);
		if (!c1 || !c2 || c1 != c2)
			break;
	}
	return c1<c2? -1: c1>c2? 1: 0;
}

int memcmp_ci(char *s1, char *s2, int k) {
	int	c1 = 0, c2 = 0;

	while (k--) {
		c1 = tolower((int) *s1++);
		c2 = tolower((int) *s2++);
		if (c1 != c2)
			break;
	}
	return c1<c2? -1: c1>c2? 1: 0;
}

/* Read a character literal. */
cell read_character(void) {
	char	buf[10], msg[50];
	int	i, c = 0; /*LINT*/

	for (i=0; i<sizeof(buf)-1; i++) {
		c = readc();
		if (i > 0 && !isalpha(c))
			break;
		buf[i] = c;
	}
	reject(c);
	buf[i] = 0;
	if (i == 0)
		c = ' ';
	else if (i == 1)
		c = buf[0];
	else if (!strcmp_ci(buf, "space"))
		c = ' ';
	else if (!strcmp_ci(buf, "newline"))
		c = '\n';
	else {
		sprintf(msg, "unknown character: #\\%s", buf);
		error(msg, NOEXPR);
		c = 0;
	}
	return make_char(c);
}

/* Read a string literal. */
cell read_string(void) {
	char	s[TOKEN_LENGTH+1];
	cell	n;
	int	c, i, q;
	int	inv;

	i = 0;
	q = 0;
	c = readc();
	inv = 0;
	while (q || c != '"') {
		if (c == '\n')
			Line_no++;
		if (c == EOF)
			error("missing '\"' in string literal", NOEXPR);
		if (Error_flag)
			break;
		if (i >= TOKEN_LENGTH-2) {
			error("string literal too long", NOEXPR);
			i--;
		}
		if (q && c != '"' && c != '\\') {
			s[i++] = '\\';
			inv = 1;
		}
		s[i] = c;
		q = !q && c == '\\';
		if (!q)
			i++;
		c = readc();
	}
	s[i] = 0;
	n = make_string(s, i);
	Tag[n] |= CONST_TAG;
	if (inv)
		error("invalid escape sequence in string", n);
	return n;
}

#define separator(c) \
	((c) == ' '  || (c) == '\t' || (c) == '\n' || \
	 (c) == '\r' || (c) == '('  || (c) == ')'  || \
	 (c) == ';'  || (c) == '\'' || (c) == '`'  || \
	 (c) == ','  || (c) == '"'  || (c) == '['  || \
	 (c) == ']'  || (c) == EOF)

#define SYM_CHARS	"!@$%^&*-/_+=~.?<>:"

#define is_symbolic(c) \
	(isalpha(c) || \
	 isdigit(c) || \
	 strchr(SYM_CHARS, (c)))

cell funny_char(char *msg, int c) {
	char	buf[128];

	if (isprint(c))
		return error(msg, make_char(c));
	sprintf(buf, "%s, code", msg);
	return error(buf, make_integer(c));
}

cell read_symbol_or_number(int c) {
	char	s[TOKEN_LENGTH];
	int	i, funny = 0;

	i = 0;
	while (!separator(c)) {
		if (!is_symbolic(c))
			funny = c;
		if (i >= TOKEN_LENGTH-2) {
			error("symbol too long", NOEXPR);
			i--;
		}
		s[i] = c;
		i++;
		c = readc_ci();
	}
	s[i] = 0;
	reject(c);
	if (funny)
		return funny_char("funny character in symbol", funny);
	if (string_numeric_p(s))
		return string_to_number(s);
	if (!strcmp(s, "define-macro"))
		return S_define_syntax;
	return symbol_ref(s);
}

cell list_to_vector(cell m, char *msg, int flags) {
	cell	n, vec;
	int	k;
	cell	*p;

	k = 0;
	for (n = m; n != NIL; n = cdr(n)) {
		if (atom_p(n))
			return error(msg, m);
		k++;
	}
	vec = new_vec(T_VECTOR, k*sizeof(cell));
	Tag[vec] |= flags;
	p = vector(vec);
	for (n = m; n != NIL; n = cdr(n)) {
		*p = car(n);
		p++;
	}
	return vec;
}

cell read_vector(void) {
	cell	n;

	n = read_list(0, RPAREN);
	save(n);
	n = list_to_vector(n, "invalid vector syntax", CONST_TAG);
	unsave(1);
	return n;
}

cell meta_command(void) {
	int	c, cmd, i;
	cell	n, cmdsym;
	char	s[128];

	cmd = readc_ci();
	c = readc();
	while (c == ' ')
		c = readc();
	i = 0;
	while (c != '\n' && c != EOF) {
		if (i < sizeof(s) - 2)
			s[i++] = c;
		c = readc();
	}
	reject(c);
	s[i] = 0;
	n = make_string(s, strlen(s));
	n = i == 0? NIL: cons(n, NIL);
	switch (cmd) {
	case 'a':	cmdsym = symbol_ref("apropos"); break;
	case 'h':	cmdsym = symbol_ref("help"); break;
	case 'l':	cmdsym = symbol_ref("load-from-library"); break;
	case 'q':	cmdsym = symbol_ref("sys:exit"); break;
	default: 	prints(",a = apropos"); nl();
			prints(",h = help"); nl();
			prints(",l = load-from-library"); nl();
			prints(",q = sys:exit"); nl();
			return UNSPECIFIC;
	}
	return cons(cmdsym, n);
}

int block_comment(void) {
	int	n, c, state = 0;

	for (n=1; n; ) {
		c = readc_ci();
		switch (c) {
		case EOF:
			error("missing |#", NOEXPR);
			return 0;
		case '|':
			switch (state) {
			case 1:		n++; state = 0; break;
			default:	state = -1; break;
			}
			break;
		case '#':
			switch (state) {
			case -1:	n--; state = 0; break;
			default:	state = 1; break;
			}
			break;
		case '\n':
			Line_no++;
			state = 0;
			break;
		default:
			state = 0;
			break;
		}
	}
	return readc_ci();
}

int closing_paren(void) {
	int c = readc_ci();

	reject(c);
	return c == ')';
}

cell bignum_read(char *pre, int radix) {
	char	digits[] = "0123456789abcdef";
	char	buf[100];
	cell	base, num;
	int	c, s, p, nd;

	base = make_integer(radix);
	save(base);
	num = Zero;
	save(num);
	c = readc_ci();
	s = 0;
	if (c == '-') {
		s = 1;
		c = readc_ci();
	}
	else if (c == '+') {
		c = readc_ci();
	}
	nd = 0;
	while (!separator(c)) {
		p = 0;
		while (digits[p] && digits[p] != c)
			p++;
		if (p >= radix) {
			sprintf(buf, "invalid digit in %s number", pre);
			unsave(2);
			return funny_char(buf, c);
		}
		num = bignum_multiply(num, base);
		car(Stack) = num;
		num = bignum_add(num, make_integer(p));
		car(Stack) = num;
		nd++;
		c = readc_ci();
	}
	unsave(2);
	if (!nd) {
		sprintf(buf, "digits expected after %s", pre);
		return error(buf, NOEXPR);
	}
	reject(c);
	return s? bignum_negate(num): num;
}


cell read_real_number(int inexact) {
	cell	n, m;
	int	flags;
	char	buf[50];

	n = read_form(0);
	if (integer_p(n)) {
		if (!inexact)
			return n;
		flags = bignum_negative_p(n)? REAL_NEGATIVE: 0;
		m = bignum_abs(n);
		return Make_real(flags, 0, cdr(m));
	}
	else if (real_p(n)) {
		if (inexact)
			return n;
		m = real_to_bignum(n);
		if (m == UNDEFINED)
			return error("#e: no exact representation for", n);
		return m;
	}
	sprintf(buf, "number expected after #%c, got",
		inexact? 'i': 'e');
	return error(buf, n);
}

cell integer_argument(char *who, cell x) {
	cell	n;
	char	msg[100];

	if (real_p(x)) {
		n = real_to_bignum(x);
		if (n == UNDEFINED) {
			sprintf(msg, "%s: expected integer, got", who);
			error(msg, x);
			return UNDEFINED;
		}
		return n;
	}
	return x;
}

/* Report unreadable object */
cell unreadable(void) {
	int	c, i;
	char	buf[TOKEN_LENGTH];
	int	d;

	strcpy(buf, "#<");
	i = 2;
	while (1) {
		c = readc_ci();
		if (c == '>' || c == '\n') {
			if (c == '\n')
				Line_no++;
			break;
		}
		if (i < TOKEN_LENGTH-2)
			buf[i++] = c;
	}
	buf[i++] = '>';
	buf[i] = 0;
	d = Displaying;
	Displaying = 1;
	error("unreadable object", make_string(buf, i));
	Displaying = d;
	return UNDEFINED;
}

cell read_form(int flags) {
	char	buf[50];
	int	c, c2;

	c = readc_ci();
	while (1) {	/* Skip over spaces and comments */
		while (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
			if (c == '\n')
				Line_no++;
			if (Error_flag)
				return NIL;
			c = readc_ci();
		}
		if (c == '#') {
			c = readc_ci();
			if (c == '!') {
				/* ok */
			}
			else if (c == '|') {
				c = block_comment();
				continue;
			}
			else {
				reject(c);
				c = '#';
				break;
			}
		}
		else if (c != ';')
			break;
		while (!Error_flag && c != '\n' && c != EOF)
			c = readc_ci();
		if (Error_flag)
			return UNSPECIFIC;
	}
	if (c == EOF)
		return END_OF_FILE;
	if (Error_flag)
		return UNSPECIFIC;
	if (c == '(') {
		return read_list(flags, RPAREN);
	}
	else if (c == '[') {
		return read_list(flags, RBRACK);
	}
	else if (c == '\'' || c == '`') {
		cell	n;

		if (closing_paren())
			return error("missing form after \"'\" or \"`\"",
					NOEXPR);
		Level++;
		n = quote(read_form(CONST_TAG), c=='`'? S_quasiquote:
							S_quote);
		Level--;
		return n;
	}
	else if (c == ',') {
		if (closing_paren())
			return error("missing form after \",\"",
					NOEXPR);
		c = readc_ci();
		if (c == '@') {
			return quote(read_form(0), S_unquote_splicing);
		}
		else {
			reject(c);
			if (!Level)
				return meta_command();
			return quote(read_form(0), S_unquote);
		}
	}
	else if (c == '#') {
		c = readc_ci();
		switch (c) {
		case 'f':	return FALSE;
		case 't':	return TRUE;
		case '\\':	return read_character();
		case '(':	return read_vector();
		case 'b':	return bignum_read("#b", 2);
		case 'd':	return bignum_read("#d", 10);
		case 'o':	return bignum_read("#o", 8);
		case 'x':	return bignum_read("#x", 16);
		case 'e':	return read_real_number(0);
		case 'i':	return read_real_number(1);
		case '<':	return unreadable();
		default:	sprintf(buf, "unknown # syntax: #%c", c);
				return error(buf, NOEXPR);
		}
	}
	else if (c == '"') {
		return read_string();
	}
	else if (c == ')') {
		if (!Level) return error("unexpected ')'", NOEXPR);
		return RPAREN;
	}
	else if (c == ']') {
		if (!Level) return error("unexpected ']'", NOEXPR);
		return RBRACK;
	}
	else if (c == '.') {
		c2 = readc_ci();
		reject(c2);
		if (separator(c2)) {
			if (!Level)
				return error("unexpected '.'", NOEXPR);
			return DOT;
		}
		return read_symbol_or_number(c);
	}
	else if (is_symbolic(c)) {
		return read_symbol_or_number(c);
	}
	else {
		return funny_char("funny input character", c);
	}
}

cell xread(void) {
	Level = 0;
	return read_form(0);
}

/*
 * Printer
 */

char *ntoa(char *b, cell x, int w) {
	char	buf[40];
	int	i = 0, neg = 0;
	char	*p = &buf[sizeof(buf)-1];

	if (x < 0) {
		x = -x;
		neg = 1;
	}
	*p = 0;
	while (x || i == 0) {
		i++;
		if (i >= sizeof(buf)-1)
			fatal("ntoa: number too big");
		p--;
		*p = x % 10 + '0';
		x = x / 10;
	}
	while (i < (w-neg) && i < sizeof(buf)-1) {
		i++;
		p--;
		*p = '0';
	}
	if (neg) {
		if (i >= sizeof(buf)-1)
			fatal("ntoa: number too big");
		p--;
		*p = '-';
	}
	strcpy(b, p);
	return b;
}

/* Print bignum integer. */
int print_integer(cell n) {
	if (!integer_p(n))
		return 0;
	print_bignum(n);
	return 1;
}

/* Print real number. */
int print_realnum(cell n) {
	if (!real_p(n))
		return 0;
	print_real(n);
	return 1;
}

/* Print expressions of the form (QUOTE X) as 'X. */
int print_quoted(cell n) {
	if (	car(n) == S_quote &&
		cdr(n) != NIL &&
		cddr(n) == NIL
	) {
		prints("'");
		print_form(cadr(n));
		return 1;
	}
	return 0;
}

int print_procedure(cell n) {
	if (function_p(n)) {
		prints("#<procedure ");
		print_form(cadr(n));
		prints(">");
		return 1;
	}
	return 0;
}

int print_continuation(cell n) {
	if (continuation_p(n)) {
		prints("#<continuation>");
		return 1;
	}
	return 0;
}

int print_char(cell n) {
	char	b[2];
	int	c;

	if (!char_p(n))
		return 0;
	if (!Displaying)
		prints("#\\");
	c = cadr(n);
	b[1] = 0;
	if (!Displaying && c == ' ')
		prints("space");
	else if (!Displaying && c == '\n')
		prints("newline");
	else {
		b[0] = c;
		prints(b);
	}
	return 1;
}

int print_string(cell n) {
	char	b[2];
	int	k;
	char	*s;

	if (!string_p(n))
		return 0;
	if (!Displaying)
		prints("\"");
	s = string(n);
	k = string_len(n)-1;
	b[1] = 0;
	while (k) {
		b[0] = *s++;
		if (!Displaying && (b[0] == '"' || b[0] == '\\'))
			prints("\\");
		prints(b);
		k--;
	}
	if (!Displaying)
		prints("\"");
	return 1;
}

int print_symbol(cell n) {
	char	*s;

	if (!symbol_p(n))
		return 0;
	s = symbol_name(n);
	prints(s);
	return 1;
}

int print_primitive(cell n) {
	PRIM	*p;

	if (!primitive_p(n))
		return 0;
	prints("#<primitive ");
	p = &Primitives[cadr(n)];
	prints(p->name);
	prints(">");
	return 1;
}

int print_syntax(cell n) {
	if (!syntax_p(n))
		return 0;
	prints("#<syntax>");
	return 1;
}

int print_vector(cell n) {
	cell	*p;
	int	k;

	if (!vector_p(n))
		return 0;
	prints("#(");
	p = vector(n);
	k = vector_len(n);
	while (k--) {
		print_form(*p++);
		if (k)
			prints(" ");
	}
	prints(")");
	return 1;
}

int print_port(cell n) {
	char	buf[100];

	if (!input_port_p(n) && !output_port_p(n))
		return 0;
	sprintf(buf, "#<%s-port %d>",
		input_port_p(n)? "input": "output",
		(int) port_no(n));
	prints(buf);
	return 1;
}

void x_print_form(cell n, int depth) {
	if (depth > MAX_IO_DEPTH) {
		error("printer: too many nested lists or vectors", NOEXPR);
		return;
	}
	if (n == NIL) {
		prints("()");
	}
	else if (eof_p(n)) {
		prints("#<eof>");
	}
	else if (n == FALSE) {
		prints("#f");
	}
	else if (n == TRUE) {
		prints("#t");
	}
	else if (undefined_p(n)) {
		prints("#<undefined>");
	}
	else if (unspecific_p(n)) {
		prints("#<unspecific>");
	}
	else {
		if (print_char(n)) return;
		if (print_procedure(n)) return;
		if (print_continuation(n)) return;
		if (print_realnum(n)) return;
		if (print_integer(n)) return;
		if (print_primitive(n)) return;
		if (print_quoted(n)) return;
		if (print_string(n)) return;
		if (print_symbol(n)) return;
		if (print_syntax(n)) return;
		if (print_vector(n)) return;
		if (print_port(n)) return;
		prints("(");
		while (n != NIL) {
			if (Error_flag) return;
			if (printer_limit())
				return;
			x_print_form(car(n), depth+1);
			if (Error_flag) return;
			n = cdr(n);
			if (n != NIL && atom_p(n)) {
				prints(" . ");
				x_print_form(n, depth+1);
				n = NIL;
			}
			if (n != NIL) prints(" ");
		}
		prints(")");
	}
}

void print_form(cell n) {
	x_print_form(n, 0);
}

/*
 * Special Form Handlers
 */

int proper_list_p(cell n) {
	while (pair_p(n))
		n = cdr(n);
	return n == NIL;
}

cell append_b(cell a, cell b) {
	cell	p, last = NIL;

	if (a == NIL)
		return b;
	p = a;
	while (p != NIL) {
		if (atom_p(p))
			fatal("append!: improper list");
		last = p;
		p = cdr(p);
	}
	cdr(last) = b;
	return a;
}

int argument_list_p(cell n) {
	if (n == NIL || symbol_p(n))
		return 1;
	if (atom_p(n))
		return 0;
	while (pair_p(n)) {
		if (!symbol_p(car(n)))
			return 0;
		n = cdr(n);
	}
	return n == NIL || symbol_p(n);
}

#define hash(s, h) \
	do {					\
		h = 0;				\
		while (*s)			\
			h = ((h<<5)+h) ^ *s++;	\
	} while (0)

int hash_size(int n) {
	if (n < 5) return 5;
	if (n < 11) return 11;
	if (n < 23) return 23;
	if (n < 47) return 47;
	if (n < 97) return 97;
	if (n < 199) return 199;
	if (n < 499) return 499;
	if (n < 997) return 997;
	if (n < 9973) return 9973;
	return 19997;
}

void rehash(cell e) {
	unsigned int	i;
	cell		p, *v, new;
	unsigned int	h, k = hash_size(length(e));
	char		*s;

	if (Program == NIL || k < HASH_THRESHOLD)
		return;
	new = new_vec(T_VECTOR, k * sizeof(cell));
	car(e) = new;
	v = vector(car(e));
	for (i=0; i<k; i++)
		v[i] = NIL;
	p = cdr(e);
	while (p != NIL) {
		s = symbol_name(caar(p));
		h = 0;
		hash(s, h);
		new = cons(car(p), v[h%k]);
		v = vector(car(e));
		v[h%k] = new;
		p = cdr(p);
	}
}

cell extend(cell v, cell a, cell e) {
	cell	n, new;

	n = make_binding(v, a);
	new = cons(n, cdr(e));
	cdr(e) = new;
	if (Load_level == 0)
		rehash(e);
	return e;
}

cell make_env(cell rib, cell env) {
	cell	e;

	Tmp = env;
	rib = cons(NIL, rib);
	e = cons(rib, env);
	Tmp = NIL;
	if (length(rib) >= HASH_THRESHOLD) {
		save(e);
		rehash(rib);
		unsave(1);
	}
	return e;
}

cell try_hash(cell v, cell e) {
	cell		*hv, p;
	unsigned int	h, k;
	char		*s;

	if (e == NIL || car(e) == NIL)
		return NIL;
	hv = vector(car(e));
	k = vector_len(car(e));
	s = symbol_name(v);
	hash(s, h);
	p = hv[h%k];
	while (p != NIL) {
		if (caar(p) == v)
			return car(p);
		p = cdr(p);
	}
	return NIL;
}

cell lookup(cell v, cell env, int req) {
	cell	e, n;

	while (env != NIL) {
		e = car(env);
		n = try_hash(v, e);
		if (n != NIL)
			return n;
		if (e != NIL)
			e = cdr(e);	/* skip over hash table */
		while (e != NIL) {
			if (v == caar(e))
				return car(e);
			e = cdr(e);
		}
		env = cdr(env);
	}
	if (!req)
		return NIL;
	if (special_p(v))
		error("invalid syntax", v);
	else
		error("symbol not bound", v);
	return NIL;
}

cell too_few_args(cell n) {
	return error("too few arguments", n);
}

cell too_many_args(cell n) {
	return error("too many arguments", n);
}

/* Set up sequence for AND, BEGIN, OR. */
cell make_sequence(int state, cell neutral, cell x, int *pc, int *ps) {
	if (cdr(x) == NIL) {
		return neutral;
	}
	else if (cddr(x) == NIL) {
		*pc = 1;
		return cadr(x);
	}
	else {
		*pc = 2;
		*ps = state;
		save(cdr(x));
		return cadr(x);
	}
}

#define sf_and(x, pc, ps) \
	make_sequence(EV_AND, TRUE, x, pc, ps)

#define sf_begin(x, pc, ps) \
	make_sequence(EV_BEGIN, UNSPECIFIC, x, pc, ps)

cell sf_cond(cell x, int *pc, int *ps) {
	cell	clauses, p;

	clauses = cdr(x);
	p = clauses;
	while (p != NIL) {
		if (atom_p(car(p)))
			return error("cond: invalid syntax", car(p));
		p = cdr(p);
	}
	if (clauses == NIL)
		return UNSPECIFIC;
	if (caar(clauses) == S_else && cdr(clauses) == NIL) {
		p = cons(TRUE, cdar(clauses));
		clauses = cons(p, cdr(clauses));
	}
	save(clauses);
	*pc = 2;
	*ps = EV_COND;
	return caar(clauses);
}

cell sf_if(cell x, int *pc, int *ps) {
	cell	m, new;

	m = cdr(x);
	if (m == NIL || cdr(m) == NIL)
		return too_few_args(x);
	if (cddr(m) != NIL && cdddr(m) != NIL)
		return too_many_args(x);
	if (cddr(m) == NIL) {
		new = cons(UNSPECIFIC, NIL);
		cddr(m) = new;
	}
	save(m);
	*pc = 2;
	*ps = EV_IF_PRED;
	return car(m);
}

cell gensym(char *prefix);

cell make_temporaries(cell x) {
	cell	n, v;

	n = NIL;
	save(n);
	while (x != NIL) {
		v = gensym("g");
		n = cons(v, n);
		car(Stack) = n;
		x = cdr(x);
	}
	unsave(1);
	return n;
}

/*
 * Return (begin (set! x1 t1)
 *               ...
 *               (set! xN tN))
 */
cell make_assignments(cell x, cell t) {
	cell	n, asg;

	n = NIL;
	save(n);
	while (x != NIL) {
		asg = cons(car(t), NIL);
		asg = cons(car(x), asg);
		asg = cons(S_set_b, asg);
		n = cons(asg, n);
		car(Stack) = n;
		x = cdr(x);
		t = cdr(t);
	}
	unsave(1);
	return cons(S_begin, n);
}

cell make_undefineds(cell x) {
	cell	n;

	n = NIL;
	while (x != NIL) {
		n = cons(UNDEFINED, n);
		x = cdr(x);
	}
	return n;
}

/* Return ((lambda (v1 ...)
 *           ((lambda (t1 ...)
 *              (begin (set! v1 t1)
 *                     ...
 *                     body))
 *            a1 ...))
 *         #<undefined>
 *         ...)
 */
cell make_recursive_lambda(cell v, cell a, cell body) {
	cell	t, n;

	t = make_temporaries(v);
	save(t);
	body = append_b(make_assignments(v, t), body);
	body = cons(body, NIL);
	n = cons(t, body);
	n = cons(S_lambda, n);
	n = cons(n, a);
	n = cons(n, NIL);
	n = cons(v, n);
	n = cons(S_lambda, n);
	save(n);
	n = cons(n, make_undefineds(v));
	unsave(2);
	return n;
}

enum { VARS, VALS };

/* Extract variables or arguments from a set of DEFINEs. */
cell extract_from_defines(cell x, int part, cell *restp) {
	cell	a, n, new;
	int	k;

	a = NIL;
	while (x != NIL) {
		if (atom_p(x) || atom_p(car(x)) || caar(x) != S_define)
			break;
		n = car(x);
		if (	!proper_list_p(n) ||
			(k = length(n)) < 3 ||
			!argument_list_p(cadr(n)) ||
			(symbol_p(cadr(n)) && k > 3)
		)
			return error("define: invalid syntax", n);
		if (pair_p(cadr(n))) {
			/* (define (proc vars) ...) */
			if (part == VARS) {
				a = cons(caadr(n), a);
			}
			else {
				a = cons(NIL, a);
				save(a);
				new = cons(cdadr(n), cddr(n));
				new = cons(S_lambda, new);
				car(a) = new;
				unsave(1);
			}
		}
		else {
			a = cons(part==VARS? cadr(n): caddr(n), a);
		}
		x = cdr(x);
	}
	*restp = x;
	return a;
}

/*
 * Rewrite local DEFINEs using LAMBDA and SET!.
 * This is semantically equivalent to:
 *
 * (lambda ()        --->  (lambda ()
 *   (define v1 a1)          (letrec ((v1 a1)
 *   ...                              ...)
 *   body)                     body))
 */
cell resolve_local_defines(int x) {
	cell	v, a, n, rest;

	a = extract_from_defines(x, VALS, &rest);
	if (Error_flag)
		return NIL;
	save(a);
	v = extract_from_defines(x, VARS, &rest);
	save(v);
	if (rest == NIL)
		rest = cons(UNSPECIFIC, NIL);
	save(rest);
	n = make_recursive_lambda(v, a, rest);
	unsave(3);
	return n;
}

cell sf_lambda(cell x) {
	cell	n;
	int	k;

	k = length(x);
	if (k < 3)
		return too_few_args(x);
	if (!argument_list_p(cadr(x)))
		return error("malformed argument list", cadr(x));
	if (pair_p(caddr(x)) && caaddr(x) == S_define)
		n = resolve_local_defines(cddr(x));
	else if (k > 3)
		n = cons(S_begin, cddr(x));
	else
		n = caddr(x);
	n = cons(n, Environment);
	n = cons(cadr(x), n);
	return new_atom(T_FUNCTION, n);
}

cell sf_quote(cell x) {
	if (cdr(x) == NIL) return too_few_args(x);
	if (cddr(x) != NIL) return too_many_args(x);
	return cadr(x);
}

#define sf_or(x, pc, ps) \
	make_sequence(EV_OR, FALSE, x, pc, ps)

cell sf_set_b(cell x, int *pc, int *ps) {
	cell	n;
	int	k;

	k = length(x);
	if (k < 3) return too_few_args(x);
	if (k > 3) return too_many_args(x);
	if (!symbol_p(cadr(x)))
		return error("set!: expected symbol, got", cadr(x));
	n = lookup(cadr(x), Environment, 1);
	if (Error_flag)
		return NIL;
	save(n);
	*pc = 2;
	*ps = EV_SET_VAL;
	return caddr(x);
}

cell find_local_variable(cell v, cell e) {
	if (e == NIL)
		return NIL;
	e = cdr(e);
	while (e != NIL) {
		if (v == caar(e))
			return car(e);
		e = cdr(e);
	}
	return NIL;
}

cell sf_define(int syntax, cell x, int *pc, int *ps) {
	cell	v, a, n, new;
	int	k;

	if (car(State_stack) == EV_ARGS)
		return error(syntax?
				"define-syntax: invalid context":
				"define: invalid context",
				x);
	k = length(x);
	if (k < 3)
		return too_few_args(x);
	if (symbol_p(cadr(x)) && k > 3)
		return too_many_args(x);
	if (!argument_list_p(cadr(x)))
		return error(syntax?
				"define-syntax: expected argument list, got":
				"define: expected argument list, got",
				cadr(x));
	if (!symbol_p(cadr(x))) {
		a = cddr(x);
		a = cons(cdadr(x), a);
		a = cons(S_lambda, a);
		save(a);
		n = caadr(x);
	}
	else {
		save(NIL);
		a = caddr(x);
		n = cadr(x);
	}
	v = find_local_variable(n, car(Environment));
	if (v == NIL) {
		new = extend(n, UNDEFINED, car(Environment));
		car(Environment) = new;
		v = cadar(Environment);
	}
	car(Stack) = binding_box(v);
	*pc = 2;
	if (syntax)
		*ps = EV_MACRO;
	else
		*ps = EV_SET_VAL;
	return a;
}

cell apply_special(cell x, int *pc, int *ps) {
	cell	sf;

	sf = car(x);
	if (sf == S_quote) return sf_quote(x);
	else if (sf == S_if) return sf_if(x, pc, ps);
	else if (sf == S_and) return sf_and(x, pc, ps);
	else if (sf == S_or) return sf_or(x, pc, ps);
	else if (sf == S_cond) return sf_cond(x, pc, ps);
	else if (sf == S_begin) return sf_begin(x, pc, ps);
	else if (sf == S_lambda) return sf_lambda(x);
	else if (sf == S_set_b) return sf_set_b(x, pc, ps);
	else if (sf == S_define) return sf_define(0, x, pc, ps);
	else if (sf == S_define_syntax) return sf_define(1, x, pc, ps);
	else fatal("internal: unknown special form");
	return UNSPECIFIC;
}

/*
 * Primitives
 */

cell pp_apply(cell x) {
	cell	m, p, q, last;

	m = x;
	p = cdr(m);
	last = p;
	while (p != NIL) {
		last = p;
		p = cdr(p);
	}
	p = car(last);
	while (p != NIL) {
		if (atom_p(p))
			return error("apply: improper argument list",
					car(last));
		p = cdr(p);
	}
	if (cddr(m) == NIL) {
		p = cadr(m);
	}
	else {
		p = flat_copy(cdr(m), &q);
		q = p;
		while (cddr(q) != NIL)
			q = cdr(q);
		cdr(q) = car(last);
	}
	return cons(car(m), p);
}

cell pp_call_cc(cell x) {
	cell	cc, n;

	cc = cons(Stack, NIL);
	cc = cons(Stack_bottom, cc);
	cc = cons(State_stack, cc);
	cc = cons(Environment, cc);
	cc = new_atom(T_CONTINUATION, cc);
	n = cons(cc, NIL);
	n = cons(car(x), n);
	return n;
}

cell resume(cell x) {
	cell	cc;

	if (cdr(x) == NIL) return too_few_args(x);
	if (cddr(x) != NIL) return too_many_args(x);
	cc = cdar(x);
	Environment = car(cc);
	State_stack = cadr(cc);
	Stack_bottom = caddr(cc);
	Stack = cadddr(cc);
	return cadr(x);
}

cell pp_unquote(cell x) {
	return error("unquote: not in quasiquote context", NOEXPR);
}

cell pp_unquote_splicing(cell x) {
	return error("unquote-splicing: not in quasiquote context", NOEXPR);
}

/*
 * Predicates and Booleans
 */

cell pp_eq_p(cell x) {
	return car(x) == cadr(x)? TRUE: FALSE;
}

int eqv_p(cell a, cell b) {
	if (a == b)
		return 1;
	if (char_p(a) && char_p(b) && char_value(a) == char_value(b))
		return 1;
	if (number_p(a) && number_p(b)) {
		if (real_p(a) != real_p(b))
			return 0;
		return real_equal_p(a, b);
	}
	return a == b;
}

cell pp_eqv_p(cell x) {
	return eqv_p(car(x), cadr(x))? TRUE: FALSE;
}

cell pp_not(cell x) {
	return car(x) == FALSE? TRUE: FALSE;
}

cell pp_null_p(cell x) {
	return car(x) == NIL? TRUE: FALSE;
}

/*
 * Pairs and Lists
 */

cell pp_append2(cell x) {
	cell	new, n, p, a, *pa;

	if (car(x) == NIL)
		return cadr(x);
	if (cadr(x) == NIL) {
		if (pair_p(car(x)))
			return car(x);
		else
			return error("append2: expected list, got",
					car(x));
	}
	a = n = cons(NIL, NIL);
	pa = &a;
	save(n);
	for (p = car(x); p != NIL; p = cdr(p)) {
		if (!pair_p(p))
			return error("append2: improper list", car(x));
		car(a) = car(p);
		new = cons(NIL, NIL);
		cdr(a) = new;
		pa = &cdr(a);
		a = cdr(a);
	}
	unsave(1);
	*pa = cadr(x);
	return n;
}

int assqv(char *who, int v, cell x, cell a) {
	cell	p;
	char	buf[64];

	for (p = a; p != NIL; p = cdr(p)) {
		if (!pair_p(p) || !pair_p(car(p))) {
			sprintf(buf, "%s: bad element in alist", who);
			return error(buf, p);
		}
		if (!v && x == caar(p))
			return car(p);
		if (v && eqv_p(x, caar(p)))
			return car(p);
	}
	return FALSE;
}

cell pp_assq(cell x) {
	return assqv("assq", 0, car(x), cadr(x));
}

cell pp_assv(cell x) {
	return assqv("assv", 1, car(x), cadr(x));
}

char *rev_cxr_name(char *s) {
	int		i, k = strlen(s);
	static char	buf[8];

	for (i=0; i<k; i++) {
		buf[i] = s[k-i-1];
	}
	buf[i] = 0;
	return buf;
}

cell cxr(char *op, cell x) {
	char	*p;
	cell	n = car(x);
	char	buf[64];

	for (p = op; *p; p++) {
		if (!pair_p(n)) {
			sprintf(buf, "c%sr: unsuitable type for operation",
				rev_cxr_name(op));
			error(buf, car(x));
		}
		if (*p == 'a')
			n = car(n);
		else 
			n = cdr(n);
	}
	return n;
}

cell pp_caar(cell x)   { return cxr("aa", x); }
cell pp_cadr(cell x)   { return cxr("da", x); }
cell pp_cdar(cell x)   { return cxr("ad", x); }
cell pp_cddr(cell x)   { return cxr("dd", x); }
cell pp_caaar(cell x)  { return cxr("aaa", x); }
cell pp_caadr(cell x)  { return cxr("daa", x); }
cell pp_cadar(cell x)  { return cxr("ada", x); }
cell pp_caddr(cell x)  { return cxr("dda", x); }
cell pp_cdaar(cell x)  { return cxr("aad", x); }
cell pp_cdadr(cell x)  { return cxr("dad", x); }
cell pp_cddar(cell x)  { return cxr("add", x); }
cell pp_cdddr(cell x)  { return cxr("ddd", x); }
cell pp_caaaar(cell x) { return cxr("aaaa", x); }
cell pp_caaadr(cell x) { return cxr("daaa", x); }
cell pp_caadar(cell x) { return cxr("adaa", x); }
cell pp_caaddr(cell x) { return cxr("ddaa", x); }
cell pp_cadaar(cell x) { return cxr("aada", x); }
cell pp_cadadr(cell x) { return cxr("dada", x); }
cell pp_caddar(cell x) { return cxr("adda", x); }
cell pp_cadddr(cell x) { return cxr("ddda", x); }
cell pp_cdaaar(cell x) { return cxr("aaad", x); }
cell pp_cdaadr(cell x) { return cxr("daad", x); }
cell pp_cdadar(cell x) { return cxr("adad", x); }
cell pp_cdaddr(cell x) { return cxr("ddad", x); }
cell pp_cddaar(cell x) { return cxr("aadd", x); }
cell pp_cddadr(cell x) { return cxr("dadd", x); }
cell pp_cdddar(cell x) { return cxr("addd", x); }
cell pp_cddddr(cell x) { return cxr("dddd", x); }

cell pp_car(cell x) {
	return caar(x);
}

cell pp_cdr(cell x) {
	return cdar(x);
}

cell pp_cons(cell x) {
	return cons(car(x), cadr(x));
}

cell pp_length(cell x) {
	int	k = 0;
	cell	p;

	for (p = car(x); p != NIL; p = cdr(p)) {
		if (!pair_p(p))
			return error("length: improper list", cadr(x));
		k++;
	}
	return make_integer(k);
}

cell pp_list(cell x) {
	return x;
}

cell integer_value(char *src, cell x) {
	char	msg[100];

	x = integer_argument(src, x);
	if (x == UNDEFINED)
		return 0;
	if (cddr(x) != NIL) {
		sprintf(msg, "%s: integer argument too big", src);
		error(msg, x);
		return 0;
	}
	return cadr(x);
}

cell pp_list_tail(cell x) {
	cell	p, k;

	k = integer_value("list-tail", cadr(x));
	for (p = car(x); p != NIL; p = cdr(p), k--) {
		if (!pair_p(p))
			return error("list-tail: improper list", car(x));
		if (k <= 0)
			break;
	}
	if (k != 0)
		return error("list-tail: index out of range", cadr(x));
	return p;
}

int memqv(char *who, int v, cell x, cell a) {
	cell	p;
	char	buf[64];

	for (p = a; p != NIL; p = cdr(p)) {
		if (!pair_p(p)) {
			sprintf(buf, "%s: improper list", who);
			return error(buf, a);
		}
		if (!v && x == car(p))
			return p;
		if (v && eqv_p(x, car(p)))
			return p;
	}
	return FALSE;
}

cell pp_memq(cell x) {
	return memqv("memq", 0, car(x), cadr(x));
}

cell pp_memv(cell x) {
	return memqv("memv", 1, car(x), cadr(x));
}

cell pp_reverse(cell x) {
	cell	n, m;

	m = NIL;
	for (n = car(x); n != NIL; n = cdr(n)) {
		if (!pair_p(n))
			return error("reverse: improper list", car(x));
		m = cons(car(n), m);
	}
	return m;
}

cell pp_set_car_b(cell x) {
	if (constant_p(car(x)))
		return error("set-car!: immutable object", car(x));
	caar(x) = cadr(x);
	return UNSPECIFIC;
}

cell pp_set_cdr_b(cell x) {
	if (constant_p(car(x)))
		return error("set-cdr!: immutable object", car(x));
	cdar(x) = cadr(x);
	return UNSPECIFIC;
}

/*
 * Arithmetics
 */

cell pp_abs(cell x) {
	return real_abs(car(x));
}

cell pp_equal(cell x) {
	while (cdr(x) != NIL) {
		if (!number_p(cadr(x)))
			return error("=: expected number, got", cadr(x));
		if (!real_equal_p(car(x), cadr(x)))
			return FALSE;
		x = cdr(x);
	}
	return TRUE;
}

int even_p(char *who, cell x) {
	x = integer_argument(who, x);
	if (x == UNDEFINED)
		return UNDEFINED;
	return bignum_even_p(x);
}

cell pp_even_p(cell x) {
	return even_p("even?", car(x))? TRUE: FALSE;
}

cell pp_greater(cell x) {
	while (cdr(x) != NIL) {
		if (!number_p(cadr(x)))
			return error(">: expected number, got", cadr(x));
		if (!real_less_p(cadr(x), car(x)))
			return FALSE;
		x = cdr(x);
	}
	return TRUE;
}

cell pp_greater_equal(cell x) {
	while (cdr(x) != NIL) {
		if (!number_p(cadr(x)))
			return error(">=: expected number, got", cadr(x));
		if (real_less_p(car(x), cadr(x)))
			return FALSE;
		x = cdr(x);
	}
	return TRUE;
}

cell pp_less(cell x) {
	while (cdr(x) != NIL) {
		if (!number_p(cadr(x)))
			return error("<: expected number, got", cadr(x));
		if (!real_less_p(car(x), cadr(x)))
			return FALSE;
		x = cdr(x);
	}
	return TRUE;
}

cell pp_less_equal(cell x) {
	while (cdr(x) != NIL) {
		if (!number_p(cadr(x)))
			return error("<=: expected number, got", cadr(x));
		if (real_less_p(cadr(x), car(x)))
			return FALSE;
		x = cdr(x);
	}
	return TRUE;
}

cell limit(char *msg, int(*pred)(cell,cell), cell x) {
	cell	k, p;
	int	exact = 1;

	k = car(x);
	if (real_p(k))
		exact = 0;
	for (p = cdr(x); p != NIL; p = cdr(p)) {
		if (!number_p(car(p)))
			return error(msg, (car(p)));
		if (real_p(car(p)))
			exact = 0;
		if (pred(car(p), k))
			k = car(p);
	}
	if (exact)
		return k;
	if (integer_p(k))
		return bignum_to_real(k);
	return k;
}

int real_greater_p(cell x, cell y) {
	return real_less_p(y, x);
}

cell pp_max(cell x) {
	return limit("max: expected number, got", real_greater_p, x);
}

cell pp_min(cell x) {
	return limit("min: expected number, got", real_less_p, x);
}

cell pp_minus(cell x) {
	cell	a;

	if (cdr(x) == NIL)
		return real_negate(car(x));
	a = car(x);
	x = cdr(x);
	save(a);
	while (x != NIL) {
		if (!number_p(car(x))) {
			unsave(1);
			return error("-: expected number, got", car(x));
		}
		a = real_subtract(a, car(x));
		car(Stack) = a;
		x = cdr(x);
	}
	unsave(1);
	return a;
}

cell pp_negative_p(cell x) {
	return real_negative_p(car(x))? TRUE: FALSE;
}

cell pp_odd_p(cell x) {
	return even_p("odd?", car(x))? FALSE: TRUE;
}

cell pp_plus(cell x) {
	cell	a;

	if (x == NIL)
		return Zero;
	if (cdr(x) == NIL)
		return car(x);
	a = Zero;
	save(a);
	while (x != NIL) {
		if (!number_p(car(x))) {
			unsave(1);
			return error("+: expected number, got", car(x));
		}
		a = real_add(a, car(x));
		car(Stack) = a;
		x = cdr(x);
	}
	unsave(1);
	return a;
}

cell pp_quotient(cell x) {
        char    *name;
        cell    a, b;

	name = "quotient";
        a = integer_argument(name, car(x));
        save(a);
        b = integer_argument(name, cadr(x));
        unsave(1);
	if (a == UNDEFINED || b == UNDEFINED)
		return UNDEFINED;
	a = bignum_divide(a, b);
	if (a == UNDEFINED)
		return error("divide by zero", x);
        return car(a);
}

cell pp_remainder(cell x) {
        char    *name;
        cell    a, b;

	name = "remainder"; /*LINT*/
        a = integer_argument(name, car(x));
        save(a);
        b = integer_argument(name, cadr(x));
        unsave(1);
	if (a == UNDEFINED || b == UNDEFINED)
		return UNDEFINED;
	a = bignum_divide(a, b);
	if (a == UNDEFINED)
		return error("divide by zero", x);
        return cdr(a);
}

cell pp_positive_p(cell x) {
	return real_positive_p(car(x))? TRUE: FALSE;
}

cell pp_times(cell x) {
	cell	a;

	if (x == NIL)
		return One;
	if (cdr(x) == NIL)
		return car(x);
	a = One;
	save(a);
	while (x != NIL) {
		if (!number_p(car(x))) {
			unsave(1);
			return error("*: expected number, got", car(x));
		}
		a = real_multiply(a, car(x));
		car(Stack) = a;
		x = cdr(x);
	}
	unsave(1);
	return a;
}

cell pp_zero_p(cell x) {
	return real_zero_p(car(x))? TRUE: FALSE;
}

cell pp_ceiling(cell x) {
	return real_ceil(car(x));
}

cell pp_divide(cell x) {
	cell	a;

	if (cdr(x) == NIL)
		return real_divide(One, car(x));
	a = car(x);
	x = cdr(x);
	save(a);
	while (x != NIL) {
		if (!number_p(car(x))) {
			unsave(1);
			return error("/: expected number, got", car(x));
		}
		a = real_divide(a, car(x));
		if (a == UNDEFINED) {
			unsave(1);
			return a;
		}
		car(Stack) = a;
		x = cdr(x);
	}
	unsave(1);
	return a;
}

cell pp_exact_to_inexact(cell x) {
	cell	n;
	int	flags;

	x = car(x);
	if (integer_p(x)) {
		flags = bignum_negative_p(x)? REAL_NEGATIVE: 0;
		n = bignum_abs(x);
		n = Make_real(flags, 0, cdr(n));
		if (n == UNDEFINED)
			return error("exact->inexact: overflow", x);
		return n;
	}
	return x;
}

cell pp_exact_p(cell x) {
	return integer_p(car(x))? TRUE: FALSE;
}

cell pp_exponent(cell x) {
	return make_integer(real_exponent(car(x)));
}

cell pp_floor(cell x) {
	return real_floor(car(x));
}

cell pp_inexact_p(cell x) {
	return real_p(car(x))? TRUE: FALSE;
}

cell pp_inexact_to_exact(cell x) {
	cell	n;

	x = car(x);
	if (integer_p(x))
		return x;
	n = real_to_bignum(x);
	if (n != NIL)
		return n;
	return error("inexact->exact: no exact representation for", x);
}

cell pp_mantissa(cell x) {
	return real_mantissa(car(x));
}

cell pp_real_p(cell x) {
	return number_p(car(x))? TRUE: FALSE;
}

cell pp_truncate(cell x) {
	return real_trunc(car(x));
}

/*
 * Type Predicates and Conversion
 */

cell pp_boolean_p(cell x) {
	return boolean_p(car(x))? TRUE: FALSE;
}

cell pp_char_p(cell x) {
	return char_p(car(x))? TRUE: FALSE;
}

cell pp_char_to_integer(cell x) {
	return make_integer(char_value(car(x)));
}

cell pp_input_port_p(cell x) {
	return input_port_p(car(x))? TRUE: FALSE;
}

cell pp_integer_to_char(cell x) {
	cell	n;

	n = integer_value("integer->char", car(x));
	if (n < 0 || n > 255)
		return error("integer->char: argument value out of range",
				car(x));
	return make_char(n);
}

cell pp_integer_p(cell x) {
	return real_integer_p(car(x))? TRUE: FALSE;
}

cell list_to_string(char *who, cell x) {
	cell	n;
	int	k = length(x);
	char	*s;
	char	buf[100];

	n = make_string("", k);
	s = string(n);
	while (x != NIL) {
		if (atom_p(x))
			return error("list->string: improper list", x);
		if (!char_p(car(x))) {
			sprintf(buf, "%s: expected list of char,"
					" got list containing",
				who);
			return error(buf, car(x));
		}
		*s++ = cadar(x);
		x = cdr(x);
	}
	*s = 0;
	return n;
}

cell pp_list_to_string(cell x) {
	return list_to_string("list->string", car(x));
}

cell pp_list_to_vector(cell x) {
	return list_to_vector(car(x), "list->vector: improper list", 0);
}

cell pp_output_port_p(cell x) {
	return output_port_p(car(x))? TRUE: FALSE;
}

cell pp_pair_p(cell x) {
	return pair_p(car(x))? TRUE: FALSE;
}

cell pp_procedure_p(cell x) {
	return (function_p(car(x)) ||
		primitive_p(car(x)) ||
		continuation_p(car(x)))?
			TRUE: FALSE;
}

cell pp_string_to_list(cell x) {
	char	*s;
	cell	n, a, new;
	int	k, i;

	k = string_len(car(x));
	n = NIL;
	a = NIL;
	for (i=0; i<k-1; i++) {
		s = string(car(x));
		if (n == NIL) {
			n = a = cons(make_char(s[i]), NIL);
			save(n);
		}
		else {
			new = cons(make_char(s[i]), NIL);
			cdr(a) = new;
			a = cdr(a);
		}
	}
	if (n != NIL)
		unsave(1);
	return n;
}

cell pp_string_to_symbol(cell x) {
	return string_to_symbol(car(x));
}

cell pp_string_p(cell x) {
	return string_p(car(x))? TRUE: FALSE;
}

cell pp_symbol_to_string(cell x) {
	return symbol_to_string(car(x));
}

cell pp_symbol_p(cell x) {
	return symbol_p(car(x))? TRUE: FALSE;
}

cell pp_vector_to_list(cell x) {
	cell	*v;
	cell	n, a, new;
	int	k, i;

	k = vector_len(car(x));
	n = NIL;
	a = NIL;
	for (i=0; i<k; i++) {
		v = vector(car(x));
		if (n == NIL) {
			n = a = cons(v[i], NIL);
			save(n);
		}
		else {
			new = cons(v[i], NIL);
			cdr(a) = new;
			a = cdr(a);
		}
	}
	if (n != NIL)
		unsave(1);
	return n;
}

cell pp_vector_p(cell x) {
	return vector_p(car(x))? TRUE: FALSE;
}

/*
 * Characters
 */

cell pp_char_alphabetic_p(cell x) {
	return isalpha(char_value(car(x)))? TRUE: FALSE;
}

#define L(c) tolower(c)
int char_ci_le(int c1, int c2) { return L(c1) <= L(c2); }
int char_ci_lt(int c1, int c2) { return L(c1) <  L(c2); }
int char_ci_eq(int c1, int c2) { return L(c1) == L(c2); }
int char_ci_ge(int c1, int c2) { return L(c1) >= L(c2); }
int char_ci_gt(int c1, int c2) { return L(c1) >  L(c2); }

int char_le(int c1, int c2) { return c1 <= c2; }
int char_lt(int c1, int c2) { return c1 <  c2; }
int char_eq(int c1, int c2) { return c1 == c2; }
int char_ge(int c1, int c2) { return c1 >= c2; }
int char_gt(int c1, int c2) { return c1 >  c2; }

cell char_predicate(char *name, int (*p)(int c1, int c2), cell x) {
	char	msg[100];

	while (cdr(x) != NIL) {
		if (!char_p(cadr(x))) {
			sprintf(msg, "%s: expected char, got", name);
			return error(msg, cadr(x));
		}
		if (!p(char_value(car(x)), char_value(cadr(x))))
			return FALSE;
		x = cdr(x);
	}
	return TRUE;
}

#define CP return char_predicate
cell pp_char_ci_le_p(cell x) { CP("char-ci<=?", char_ci_le, x); }
cell pp_char_ci_lt_p(cell x) { CP("char-ci<?",  char_ci_lt, x); }
cell pp_char_ci_eq_p(cell x) { CP("char-ci=?",  char_ci_eq, x); }
cell pp_char_ci_ge_p(cell x) { CP("char-ci>=?", char_ci_ge, x); }
cell pp_char_ci_gt_p(cell x) { CP("char-ci>?",  char_ci_gt, x); }

cell pp_char_le_p(cell x) { CP("char<=?", char_le, x); }
cell pp_char_lt_p(cell x) { CP("char<?",  char_lt, x); }
cell pp_char_eq_p(cell x) { CP("char=?",  char_eq, x); }
cell pp_char_ge_p(cell x) { CP("char>=?", char_ge, x); }
cell pp_char_gt_p(cell x) { CP("char>?",  char_gt, x); }

cell pp_char_downcase(cell x) {
	return make_char(tolower(char_value(car(x))));
}

cell pp_char_lower_case_p(cell x) {
	return islower(char_value(car(x)))? TRUE: FALSE;
}

cell pp_char_numeric_p(cell x) {
	return isdigit(char_value(car(x)))? TRUE: FALSE;
}

cell pp_char_upcase(cell x) {
	return make_char(toupper(char_value(car(x))));
}

cell pp_char_upper_case_p(cell x) {
	return isupper(char_value(car(x)))? TRUE: FALSE;
}

cell pp_char_whitespace_p(cell x) {
	int	c = char_value(car(x));

	return (c == ' '  || c == '\t' || c == '\n' ||
		c == '\r' || c == '\f')? TRUE: FALSE;
}

/*
 * Strings
 */

cell pp_make_string(cell x) {
	cell	n;
	int	c, k;
	char	*s;

	k = integer_value("make-string", car(x));
	if (k < 0)
		return error("make-string: got negative length", x);
	n = make_string("", k);
	s = string(n);
	c = cdr(x) == NIL? ' ': char_value(cadr(x));
	memset(s, c, k);
	s[k] = 0;
	return n;
}

cell pp_string(cell x) {
	return list_to_string("string", x);
}

cell pp_string_append(cell x) {
	cell	p, n;
	int	k, m;
	char	*s;

	k = 0;
	for (p = x; p != NIL; p = cdr(p)) {
		if (!string_p(car(p)))
			return error("string-append: expected string, got",
					car(p));
		k += string_len(car(p))-1;
	}
	n = make_string("", k);
	s = string(n);
	k = 0;
	for (p = x; p != NIL; p = cdr(p)) {
		m = string_len(car(p));
		memcpy(&s[k], string(car(p)), m);
		k += string_len(car(p))-1;
	}
	return n;
}

cell pp_string_copy(cell x) {
	cell	n, k;

	/*
	 * Cannot pass name to make_string(), because
	 * string(car(x)) may move during GC.
	 */
	k = string_len(car(x));
	n = make_string("", k-1);
	memcpy(string(n), string(car(x)), k);
	return n;
}

cell pp_string_fill_b(cell x) {
	int	c = char_value(cadr(x)),
		i, k = string_len(car(x))-1;
	char	*s = string(car(x));

	if (constant_p(car(x)))
		return error("string-fill!: immutable object", car(x));
	for (i=0; i<k; i++)
		s[i] = c;
	return UNSPECIFIC;
}

cell pp_string_length(cell x) {
	return make_integer(string_len(car(x))-1);
}

cell pp_string_ref(cell x) {
	int	p, k = string_len(car(x))-1;

	p = integer_value("string-ref", cadr(x));
	if (p < 0 || p >= k)
		return error("string-ref: index out of range",
				cadr(x));
	return make_char(string(car(x))[p]);
}

cell pp_string_set_b(cell x) {
	int	p, k = string_len(car(x))-1;

	if (constant_p(car(x)))
		return error("string-set!: immutable object", car(x));
	p = integer_value("string-set!", cadr(x));
	if (p < 0 || p >= k)
		return error("string-set!: index out of range",
				cadr(x));
	string(car(x))[p] = char_value(caddr(x));
	return UNSPECIFIC;
}

#define RT	k=0; return

int string_ci_le(char *s1, char *s2, int k) { RT strcmp_ci(s1, s2) <= 0; }
int string_ci_lt(char *s1, char *s2, int k) { RT strcmp_ci(s1, s2) <  0; }
int string_ci_ge(char *s1, char *s2, int k) { RT strcmp_ci(s1, s2) >= 0; }
int string_ci_gt(char *s1, char *s2, int k) { RT strcmp_ci(s1, s2) >  0; }
int string_ci_eq(char *s1, char *s2, int k) {
	return memcmp_ci(s1, s2, k) == 0;
}

int string_le(char *s1, char *s2, int k) { RT strcmp(s1, s2) <= 0; }
int string_lt(char *s1, char *s2, int k) { RT strcmp(s1, s2) <  0; }
int string_ge(char *s1, char *s2, int k) { RT strcmp(s1, s2) >= 0; }
int string_gt(char *s1, char *s2, int k) { RT strcmp(s1, s2) >  0; }
int string_eq(char *s1, char *s2, int k) {
	return memcmp(s1, s2, k) == 0;
}

cell string_predicate(char *name, int (*p)(char *s1, char *s2, int k), cell x)
{
	char	msg[100];
	int	k = 0;

	while (cdr(x) != NIL) {
		if (!string_p(cadr(x))) {
			sprintf(msg, "%s: expected string, got", name);
			return error(msg, cadr(x));
		}
		if (p == string_eq || p == string_ci_eq) {
			k = string_len(car(x));
			if (k != string_len(cadr(x)))
				return FALSE;
		}
		if (!p(string(car(x)), string(cadr(x)), k))
			return FALSE;
		x = cdr(x);
	}
	return TRUE;
}

#define SP return string_predicate
cell pp_string_ci_le_p(cell x) { SP("string-ci<=?", string_ci_le, x); }
cell pp_string_ci_lt_p(cell x) { SP("string-ci<?",  string_ci_lt, x); }
cell pp_string_ci_eq_p(cell x) { SP("string-ci=?",  string_ci_eq, x); }
cell pp_string_ci_ge_p(cell x) { SP("string-ci>=?", string_ci_ge, x); }
cell pp_string_ci_gt_p(cell x) { SP("string-ci>?",  string_ci_gt, x); }

cell pp_string_le_p(cell x) { SP("string<=?", string_le, x); }
cell pp_string_lt_p(cell x) { SP("string<?",  string_lt, x); }
cell pp_string_eq_p(cell x) { SP("string=?",  string_eq, x); }
cell pp_string_ge_p(cell x) { SP("string>=?", string_ge, x); }
cell pp_string_gt_p(cell x) { SP("string>?",  string_gt, x); }

cell pp_substring(cell x) {
	int	k = string_len(car(x))-1;
	int	p0 = integer_value("substring", cadr(x));
	int	pn = integer_value("substring", caddr(x));
	char	*src, *dst;
	cell	n;

	if (p0 < 0 || p0 > k || pn < 0 || pn > k || pn < p0) {
		n = cons(caddr(x), NIL);
		return error("substring: invalid range",
				cons(cadr(x), n));
	}
	n = make_string("", pn-p0);
	dst = string(n);
	src = string(car(x));
	if (pn-p0 != 0)
		memcpy(dst, &src[p0], pn-p0);
	dst[pn-p0] = 0;
	return n;
}

/*
 * Vectors
 */

cell pp_make_vector(cell x) {
	int	i, k;
	cell	n, *v, m;

	k = integer_value("make-vector", car(x));
	if (k < 0)
		return error("make-vector: got negative length", car(x));
	n = new_vec(T_VECTOR, k * sizeof(cell));
	v = vector(n);
	m = cdr(x) == NIL? FALSE: cadr(x);
	for (i=0; i<k; i++)
		v[i] = m;
	return n;
}

cell pp_vector(cell x) {
	return list_to_vector(x, "vector: improper list", 0);
}

cell pp_vector_append(cell x) {
	cell	n, p, *ov, *nv;
	int	i, j, k, total;

	total = 0;
	for (p = x; p != NIL; p = cdr(p))
		if (vector_p(car(p)))
			total += vector_len(car(p));
		else
			return error("vector-append: expected vector, got",
					car(p));
	n = new_vec(T_VECTOR, total * sizeof(cell));;
	nv = vector(n);
	j = 0;
	for (p = x; p != NIL; p = cdr(p)) {
		ov = vector(car(p));
		k = vector_len(car(p));
		for (i = 0; i < k; i++)
			nv[j++] = ov[i];
	}
	return n;
}

cell pp_vector_copy(cell x) {
	cell	n, vec, *ov, *nv;
	int	k0 = 0, kn, k = vector_len(car(x));
	int	i, j;
	cell	fill = UNSPECIFIC;
	char	err[] = "vector-copy: expected integer, got";
	char	name[] = "vector-copy";

	kn = k;
	vec = car(x);
	x = cdr(x);
	if (x != NIL) {
		if (!number_p(car(x)))
			return error(err, car(x));
		k0 = integer_value(name, car(x));
		x = cdr(x);
	}
	if (x != NIL) {
		if (!number_p(car(x)))
			return error(err, car(x));
		kn = integer_value(name, car(x));
		x = cdr(x);
	}
	if (k0 > kn)
		return error("vector-copy: bad range", NOEXPR);
	if (x != NIL) {
		fill = car(x);
		x = cdr(x);
	}
	if (x != NIL)
		return error("vector-copy: too many arguments", NOEXPR);
	n = new_vec(T_VECTOR, (kn-k0) * sizeof(cell));
	nv = vector(n);
	ov = vector(vec);
	for (j = 0, i = k0; i < kn; i++, j++)
		if (i >= k)
			nv[j] = fill;
		else
			nv[j] = ov[i];
	return n;
}

cell pp_vector_fill_b(cell x) {
	cell	fill = cadr(x);
	int	i, k = vector_len(car(x));
	cell	*v = vector(car(x));

	if (constant_p(car(x)))
		return error("vector-fill!: immutable object", car(x));
	for (i=0; i<k; i++)
		v[i] = fill;
	return UNSPECIFIC;
}

cell pp_vector_length(cell x) {
	return make_integer(vector_len(car(x)));
}

cell pp_vector_ref(cell x) {
	int	p, k = vector_len(car(x));

	p = integer_value("vector-ref", cadr(x));
	if (p < 0 || p >= k)
		return error("vector-ref: index out of range",
				cadr(x));
	return vector(car(x))[p];
}

cell pp_vector_set_b(cell x) {
	int	p, k = vector_len(car(x));

	if (constant_p(car(x)))
		return error("vector-set!: immutable object", car(x));
	p = integer_value("vector-set!", cadr(x));
	if (p < 0 || p >= k)
		return error("vector-set!: index out of range",
				cadr(x));
	vector(car(x))[p] = caddr(x);
	return UNSPECIFIC;
}

/*
 * I/O
 */

cell pp_close_input_port(cell x) {
	if (port_no(car(x)) < 2)
		return error("please do not close the standard input port",
				NOEXPR);
	close_port(port_no(car(x)));
	return UNSPECIFIC;
}

cell pp_close_output_port(cell x) {
	if (port_no(car(x)) < 2)
		return error("please do not close the standard output port",
				NOEXPR);
	close_port(port_no(car(x)));
	return UNSPECIFIC;
}

cell pp_current_input_port(cell x) {
	return make_port(input_port(), T_INPUT_PORT);
}

cell pp_current_output_port(cell x) {
	return make_port(output_port(), T_OUTPUT_PORT);
}

cell pp_write(cell x);

cell pp_display(cell x) {
	Displaying = 1;
	pp_write(x);
	Displaying = 0;
	return UNSPECIFIC;
}

cell pp_eof_object_p(cell x) {
	return car(x) == END_OF_FILE? TRUE: FALSE;
}

cell eval(cell x);

int load(char *file) {
	int	n;
	int	outer_lno;
	int	outer_loading;
	int	new_port, old_port;

	new_port = open_input_port(file);
	if (new_port == -1)
		return -1;
	lock_port(new_port);
	File_list = cons(make_string(file, (int) strlen(file)), File_list);
	save(Environment);
	while (cdr(Environment) != NIL)
		Environment = cdr(Environment);
	Load_level++;
	outer_loading = box_value(S_loading);
	box_value(S_loading) = TRUE;
	old_port = input_port();
	outer_lno = Line_no;
	Line_no = 1;
	while (!Error_flag) {
		set_input_port(new_port);
		n = xread();
		set_input_port(old_port);
		if (n == END_OF_FILE)
			break;
		if (!Error_flag)
			eval(n);
	}
	unlock_port(new_port);
	close_port(new_port);
	Line_no = outer_lno;
	box_value(S_loading) = outer_loading;
	Load_level--;
	File_list = cdr(File_list);
	rehash(car(Environment));
	Environment = unsave(1);
	return 0;
}

cell pp_load(cell x) {
	char	file[TOKEN_LENGTH+1];

	if (string_len(car(x)) > TOKEN_LENGTH)
		return error("load: path too long", car(x));
	strcpy(file, string(car(x)));
	if (load(file) < 0)
		return error("load: cannot open file", car(x));
	return UNSPECIFIC;
}

cell pp_open_input_file(cell x) {
	int	p;

	p = open_input_port(string(car(x)));
	if (p < 0)
		return error("open-input-file: could not open file",
				car(x));
	return make_port(p, T_INPUT_PORT);
}

cell pp_open_output_file(cell x) {
	int	p;

	p = open_output_port(string(car(x)), 0);
	if (p < 0)
		return error("open-output-file: could not open file",
				car(x));
	return make_port(p, T_OUTPUT_PORT);
}

cell pp_read(cell x) {
	cell	n;
	int	new_port, old_port;

	new_port = x == NIL? input_port(): port_no(car(x));
	if (new_port < 0 || new_port >= MAX_PORTS)
		return error("read: invalid input port (oops)", car(x));
	old_port = input_port();
	set_input_port(new_port);
	n = xread();
	set_input_port(old_port);
	return n;
}

cell read_char(cell x, int unget) {
	int	c, new_port, old_port;

	new_port = x == NIL? input_port(): port_no(car(x));
	if (new_port < 0 || new_port >= MAX_PORTS)
		return error("read-char: invalid input port (oops)", car(x));
	old_port = input_port();
	set_input_port(new_port);
	c = readc();
	if (unget)
		reject(c);
	set_input_port(old_port);
	return c == EOF? END_OF_FILE: make_char(c);
}

cell pp_peek_char(cell x) {
	return read_char(x, 1);
}

cell pp_read_char(cell x) {
	return read_char(x, 0);
}

cell pp_write(cell x) {
	int	new_port, old_port;

	new_port = cdr(x) == NIL? output_port(): port_no(cadr(x));
	if (new_port < 0 || new_port >= MAX_PORTS)
		return error("write: invalid output port (oops)", cadr(x));
	old_port = output_port();
	set_output_port(new_port);
	print_form(car(x));
	set_output_port(old_port);
	return UNSPECIFIC;
}

cell pp_write_char(cell x) {
	return pp_display(x);
}

/*
 * Extensions
 */

cell pp_bit_op(cell x) {
	char		name[] = "bit-op";
	cell		op, a, b;
	static cell	mask = 0;

	if (mask == 0) {
		mask = 1;
		while (mask <= INT_SEG_LIMIT)
			mask <<= 1;
		if (mask > INT_SEG_LIMIT)
			mask >>= 1;
		mask--;
	}
	op = integer_value(name, car(x));
	x = cdr(x);
	a = integer_value(name, car(x));
	for (x = cdr(x); x != NIL; x = cdr(x)) {
		b = integer_value(name, car(x));
		if (a & ~mask || b & ~mask || a < 0 || b < 0)
			return FALSE;
		switch (op) {
		case  0: a =  0;        break;
		case  1: a =   a &  b;  break;
		case  2: a =   a & ~b;  break;
		case  3: /* a =   a; */ break;
		case  4: a =  ~a &  b;  break;
		case  5: a =        b;  break;
		case  6: a =   a ^  b;  break;
		case  7: a =   a |  b;  break;
		case  8: a = ~(a |  b); break;
		case  9: a = ~(a ^  b); break;
		case 10: a =       ~b;  break;
		case 11: a =   a | ~b;  break;
		case 12: a =  ~a;       break;
		case 13: a =  ~a |  b;  break;
		case 14: a = ~(a &  b); break;
		case 15: a = ~0;        break;
		case 16: a = a  <<  b;  break;
		case 17: a = a  >>  b;  break;
		default: return FALSE;  break;
		}
		a &= mask;
	}
	return make_integer(a);
}

cell pp_delete_file(cell x) {
	if (remove(string(car(x))) < 0)
		error("delete-file: file does not exist", car(x));
	return UNSPECIFIC;
}

cell pp_error(cell x) {
	return error(string(car(x)), cdr(x) != NIL? cadr(x): NOEXPR);
}

cell pp_eval(cell x) {
	return eval(car(x));
}

cell pp_file_exists_p(cell x) {
	FILE	*f;

	f = fopen(string(car(x)), "r");
	if (f == NULL)
		return FALSE;
	fclose(f);
	return TRUE;
}

cell gensym(char *prefix) {
	static long	g = 0;
	char		s[200];

	do {
		sprintf(s, "%s%ld", prefix, g);
		g++;
	} while (find_symbol(s) != NIL);
	return symbol_ref(s);
}

cell pp_gensym(cell x) {
	char	pre[101];
	int	k;

	if (x == NIL) {
		strcpy(pre, "g");
		k = 1;
	}
	else if (string_p(car(x))) {
		memcpy(pre, string(car(x)), 100);
		k = string_len(car(x));
	}
	else if (symbol_p(car(x))) {
		memcpy(pre, symbol_name(car(x)), 100);
		k = symbol_len(car(x));
	}
	else
		return error("gensym: expected string or symbol, got",
				car(x));
	if (k > 100)
		return error("gensym: prefix too long", car(x));
	pre[100] = 0;
	return gensym(pre);
}

cell expand_syntax(cell x);

cell pp_macro_expand(cell x) {
	x = car(x);
	save(x);
	x = expand_syntax(x);
	unsave(1);
	return x;
}

cell expand_syntax_1(cell x);

cell pp_macro_expand_1(cell x) {
	x = car(x);
	save(x);
	x = expand_syntax_1(x);
	unsave(1);
	return x;
}

cell pp_reverse_b(cell x) {
	cell	n, m, h;

	m = NIL;
	n = car(x);
	while (n != NIL) {
		if (constant_p(n))
			return error("reverse!: immutable object", n);
		if (!pair_p(n))
			return error("reverse!: expected list, got", car(x));
		h = cdr(n);
		cdr(n) = m;
		m = n;
		n = h;
	}
	return m;
}

cell pp_set_input_port_b(cell x) {
	set_input_port(port_no(car(x)));
	return UNSPECIFIC;
}

cell pp_set_output_port_b(cell x) {
	set_output_port(port_no(car(x)));
	return UNSPECIFIC;
}

cell pp_stats(cell x) {
	cell	n, m;
	counter	*nodes, *conses, *collections;

	gcv(); /* start from a known state */
	reset_counter(&Reductions);
	run_stats(1);
	cons_stats(0);
	Eval_stats = 1;
	n = eval(car(x));
	Eval_stats = 0;
	run_stats(0);
	get_counters(&nodes, &conses, &collections);
	save(n);
	n = read_counter(collections);
	n = cons(n, NIL);
	save(n);
	car(Stack) = n;
	m = read_counter(nodes);
	n = cons(m, n);
	car(Stack) = n;
	m = read_counter(conses);
	n = cons(m, n);
	car(Stack) = n;
	m = read_counter(&Reductions);
	n = cons(m, n);
	n = cons(unsave(2), n);
	return n;
}

cell pp_symbols(cell x) {
	cell	n, a, y, new;

	n = NIL;
	a = NIL;
	for (y = symbol_table(); y != NIL; y = cdr(y)) {
		if (n == NIL) {
			n = a = cons(car(y), NIL);
			save(n);
		}
		else {
			new = cons(car(y), NIL);
			cdr(a) = new;
			a = cdr(a);
		}
	}
	if (n != NIL)
		unsave(1);
	return n;
}

#ifdef unix

cell pp_argv(cell x) {
	cell	n, a;

	a = binding_value(S_arguments);
	if (a == NIL)
		return FALSE;
	n = integer_value("argv", car(x));
	for (; a != NIL && n > 0; a = cdr(a), n--)
		;
	return a == NIL? FALSE: car(a);
}

cell pp_environ(cell x) {
	char	*s;

	s = getenv(string(car(x)));
	if (s == NULL)
		return FALSE;
	return make_string(s, strlen(s));
}

cell pp_system(cell x) {
	int	r;

	r = system(string(car(x)));
	return make_integer(r >> 8);
}

#endif /* unix */

/*
 * Evaluator
 */

PRIM Core_primitives[] = {
 { "*",                   pp_times,               0, -1, { REA,___,___ } },
 { "+",                   pp_plus,                0, -1, { REA,___,___ } },
 { "-",                   pp_minus,               1, -1, { REA,___,___ } },
 { "/",                   pp_divide,              1, -1, { REA,___,___ } },
 { "<",                   pp_less,                2, -1, { REA,___,___ } },
 { "<=",                  pp_less_equal,          2, -1, { REA,___,___ } },
 { "=",                   pp_equal,               2, -1, { REA,___,___ } },
 { ">",                   pp_greater,             2, -1, { REA,___,___ } },
 { ">=",                  pp_greater_equal,       2, -1, { REA,___,___ } },
 { "abs",                 pp_abs,                 1,  1, { REA,___,___ } },
 { "append2",             pp_append2,             2,  2, { LST,___,___ } },
 { "apply",               pp_apply,               2, -1, { FUN,___,___ } },
 { "assq",                pp_assq,                2,  2, { ___,LST,___ } },
 { "assv",                pp_assv,                2,  2, { ___,LST,___ } },
 { "bit-op",              pp_bit_op,              3, -1, { INT,INT,INT } },
 { "boolean?",            pp_boolean_p,           1,  1, { ___,___,___ } },
 { "caar",                pp_caar,                1,  1, { PAI,___,___ } },
 { "cadr",                pp_cadr,                1,  1, { PAI,___,___ } },
 { "cdar",                pp_cdar,                1,  1, { PAI,___,___ } },
 { "cddr",                pp_cddr,                1,  1, { PAI,___,___ } },
 { "caaar",               pp_caaar,               1,  1, { PAI,___,___ } },
 { "caadr",               pp_caadr,               1,  1, { PAI,___,___ } },
 { "cadar",               pp_cadar,               1,  1, { PAI,___,___ } },
 { "caddr",               pp_caddr,               1,  1, { PAI,___,___ } },
 { "call/cc",             pp_call_cc,             1,  1, { FUN,___,___ } },
 { "cdaar",               pp_cdaar,               1,  1, { PAI,___,___ } },
 { "cdadr",               pp_cdadr,               1,  1, { PAI,___,___ } },
 { "cddar",               pp_cddar,               1,  1, { PAI,___,___ } },
 { "cdddr",               pp_cdddr,               1,  1, { PAI,___,___ } },
 { "caaaar",              pp_caaaar,              1,  1, { PAI,___,___ } },
 { "caaadr",              pp_caaadr,              1,  1, { PAI,___,___ } },
 { "caadar",              pp_caadar,              1,  1, { PAI,___,___ } },
 { "caaddr",              pp_caaddr,              1,  1, { PAI,___,___ } },
 { "cadaar",              pp_cadaar,              1,  1, { PAI,___,___ } },
 { "cadadr",              pp_cadadr,              1,  1, { PAI,___,___ } },
 { "caddar",              pp_caddar,              1,  1, { PAI,___,___ } },
 { "cadddr",              pp_cadddr,              1,  1, { PAI,___,___ } },
 { "cdaaar",              pp_cdaaar,              1,  1, { PAI,___,___ } },
 { "cdaadr",              pp_cdaadr,              1,  1, { PAI,___,___ } },
 { "cdadar",              pp_cdadar,              1,  1, { PAI,___,___ } },
 { "cdaddr",              pp_cdaddr,              1,  1, { PAI,___,___ } },
 { "cddaar",              pp_cddaar,              1,  1, { PAI,___,___ } },
 { "cddadr",              pp_cddadr,              1,  1, { PAI,___,___ } },
 { "cdddar",              pp_cdddar,              1,  1, { PAI,___,___ } },
 { "cddddr",              pp_cddddr,              1,  1, { PAI,___,___ } },
 { "car",                 pp_car,                 1,  1, { PAI,___,___ } },
 { "cdr",                 pp_cdr,                 1,  1, { PAI,___,___ } },
 { "ceiling",             pp_ceiling,             1,  1, { REA,___,___ } },
 { "char->integer",       pp_char_to_integer,     1,  1, { CHR,___,___ } },
 { "char-alphabetic?",    pp_char_alphabetic_p,   1,  1, { CHR,___,___ } },
 { "char-ci<=?",          pp_char_ci_le_p,        2, -1, { CHR,___,___ } },
 { "char-ci<?",           pp_char_ci_lt_p,        2, -1, { CHR,___,___ } },
 { "char-ci=?",           pp_char_ci_eq_p,        2, -1, { CHR,___,___ } },
 { "char-ci>=?",          pp_char_ci_ge_p,        2, -1, { CHR,___,___ } },
 { "char-ci>?",           pp_char_ci_gt_p,        2, -1, { CHR,___,___ } },
 { "char-downcase",       pp_char_downcase,       1,  1, { CHR,___,___ } },
 { "char-lower-case?",    pp_char_lower_case_p,   1,  1, { CHR,___,___ } },
 { "char-numeric?",       pp_char_numeric_p,      1,  1, { CHR,___,___ } },
 { "char-upcase",         pp_char_upcase,         1,  1, { CHR,___,___ } },
 { "char-upper-case?",    pp_char_upper_case_p,   1,  1, { CHR,___,___ } },
 { "char-whitespace?",    pp_char_whitespace_p,   1,  1, { CHR,___,___ } },
 { "char<=?",             pp_char_le_p,           2, -1, { CHR,___,___ } },
 { "char<?",              pp_char_lt_p,           2, -1, { CHR,___,___ } },
 { "char=?",              pp_char_eq_p,           2, -1, { CHR,___,___ } },
 { "char>=?",             pp_char_ge_p,           2, -1, { CHR,___,___ } },
 { "char>?",              pp_char_gt_p,           2, -1, { CHR,___,___ } },
 { "char?",               pp_char_p,              1,  1, { ___,___,___ } },
 { "close-input-port",    pp_close_input_port,    1,  1, { INP,___,___ } },
 { "close-output-port",   pp_close_output_port,   1,  1, { OUP,___,___ } },
 { "cons",                pp_cons,                2,  2, { ___,___,___ } },
 { "current-input-port",  pp_current_input_port,  0,  0, { ___,___,___ } },
 { "current-output-port", pp_current_output_port, 0,  0, { ___,___,___ } },
 { "delete-file",         pp_delete_file,         1,  1, { STR,___,___ } },
 { "display",             pp_display,             1,  2, { ___,OUP,___ } },
 { "eof-object?",         pp_eof_object_p,        1,  1, { ___,___,___ } },
 { "eq?",                 pp_eq_p,                2,  2, { ___,___,___ } },
 { "eqv?",                pp_eqv_p,               2,  2, { ___,___,___ } },
 { "error",               pp_error,               1,  2, { STR,___,___ } },
 { "eval",                pp_eval,                1,  2, { ___,___,___ } },
 { "even?",               pp_even_p,              1,  1, { REA,___,___ } },
 { "exact->inexact",      pp_exact_to_inexact,    1,  1, { REA,___,___ } },
 { "exact?",              pp_exact_p,             1,  1, { REA,___,___ } },
 { "exponent",            pp_exponent,            1,  1, { REA,___,___ } },
 { "file-exists?",        pp_file_exists_p,       1,  1, { STR,___,___ } },
 { "floor",               pp_floor,               1,  1, { REA,___,___ } },
 { "gensym",              pp_gensym,              0,  1, { ___,___,___ } },
 { "inexact->exact",      pp_inexact_to_exact,    1,  1, { REA,___,___ } },
 { "inexact?",            pp_inexact_p,           1,  1, { REA,___,___ } },
 { "input-port?",         pp_input_port_p,        1,  1, { ___,___,___ } },
 { "integer->char",       pp_integer_to_char,     1,  1, { INT,___,___ } },
 { "integer?",            pp_integer_p,           1,  1, { ___,___,___ } },
 { "length",              pp_length,              1,  1, { LST,___,___ } },
 { "list",                pp_list,                0, -1, { ___,___,___ } },
 { "list->string",        pp_list_to_string,      1,  1, { LST,___,___ } },
 { "list->vector",        pp_list_to_vector,      1,  1, { LST,___,___ } },
 { "list-tail",           pp_list_tail,           2,  2, { LST,INT,___ } },
 { "load",                pp_load,                1,  1, { STR,___,___ } },
 { "macro-expand",        pp_macro_expand,        1,  1, { ___,___,___ } },
 { "macro-expand-1",      pp_macro_expand_1,      1,  1, { ___,___,___ } },
 { "make-string",         pp_make_string,         1,  2, { INT,CHR,___ } },
 { "make-vector",         pp_make_vector,         1,  2, { INT,___,___ } },
 { "mantissa",            pp_mantissa,            1,  1, { REA,___,___ } },
 { "max",                 pp_max,                 1, -1, { REA,___,___ } },
 { "memq",                pp_memq,                2,  2, { ___,LST,___ } },
 { "memv",                pp_memv,                2,  2, { ___,LST,___ } },
 { "min",                 pp_min,                 1, -1, { REA,___,___ } },
 { "negative?",           pp_negative_p,          1,  1, { REA,___,___ } },
 { "not",                 pp_not,                 1,  1, { ___,___,___ } },
 { "null?",               pp_null_p,              1,  1, { ___,___,___ } },
 { "odd?",                pp_odd_p,               1,  1, { REA,___,___ } },
 { "open-input-file",     pp_open_input_file,     1,  1, { STR,___,___ } },
 { "open-output-file",    pp_open_output_file,    1,  1, { STR,___,___ } },
 { "output-port?",        pp_output_port_p,       1,  1, { ___,___,___ } },
 { "pair?",               pp_pair_p,              1,  1, { ___,___,___ } },
 { "peek-char",           pp_peek_char,           0,  1, { INP,___,___ } },
 { "positive?",           pp_positive_p,          1,  1, { REA,___,___ } },
 { "procedure?",          pp_procedure_p,         1,  1, { ___,___,___ } },
 { "quotient",            pp_quotient,            2,  2, { REA,REA,___ } },
 { "read",                pp_read,                0,  1, { INP,___,___ } },
 { "read-char",           pp_read_char,           0,  1, { INP,___,___ } },
 { "real?",               pp_real_p,              1,  1, { ___,___,___ } },
 { "remainder",           pp_remainder,           2,  2, { REA,REA,___ } },
 { "reverse",             pp_reverse,             1,  1, { LST,___,___ } },
 { "reverse!",            pp_reverse_b,           1,  1, { LST,___,___ } },
 { "set-car!",            pp_set_car_b,           2,  2, { PAI,___,___ } },
 { "set-cdr!",            pp_set_cdr_b,           2,  2, { PAI,___,___ } },
 { "set-input-port!",     pp_set_input_port_b,    1,  1, { INP,___,___ } },
 { "set-output-port!",    pp_set_output_port_b,   1,  1, { OUP,___,___ } },
 { "stats",               pp_stats,               1,  1, { ___,___,___ } },
 { "string",              pp_string,              0, -1, { CHR,___,___ } },
 { "string->list",        pp_string_to_list,      1,  1, { STR,___,___ } },
 { "string->symbol",      pp_string_to_symbol,    1,  1, { STR,___,___ } },
 { "string-append",       pp_string_append,       0, -1, { STR,___,___ } },
 { "string-ci<=?",        pp_string_ci_le_p,      2, -1, { STR,___,___ } },
 { "string-ci<?",         pp_string_ci_lt_p,      2, -1, { STR,___,___ } },
 { "string-ci=?",         pp_string_ci_eq_p,      2, -1, { STR,___,___ } },
 { "string-ci>=?",        pp_string_ci_ge_p,      2, -1, { STR,___,___ } },
 { "string-ci>?",         pp_string_ci_gt_p,      2, -1, { STR,___,___ } },
 { "string-copy",         pp_string_copy,         1,  1, { STR,___,___ } },
 { "string-fill!",        pp_string_fill_b,       2,  2, { STR,CHR,___ } },
 { "string-length",       pp_string_length,       1,  1, { STR,___,___ } },
 { "string-ref",          pp_string_ref,          2,  2, { STR,INT,___ } },
 { "string-set!",         pp_string_set_b,        3,  3, { STR,INT,CHR } },
 { "string<=?",           pp_string_le_p,         2, -1, { STR,___,___ } },
 { "string<?",            pp_string_lt_p,         2, -1, { STR,___,___ } },
 { "string=?",            pp_string_eq_p,         2, -1, { STR,___,___ } },
 { "string>=?",           pp_string_ge_p,         2, -1, { STR,___,___ } },
 { "string>?",            pp_string_gt_p,         2, -1, { STR,___,___ } },
 { "string?",             pp_string_p,            1,  1, { ___,___,___ } },
 { "substring",           pp_substring,           3,  3, { STR,INT,INT } },
 { "symbol->string",      pp_symbol_to_string,    1,  1, { SYM,___,___ } },
 { "symbols",             pp_symbols,             0,  0, { ___,___,___ } },
 { "symbol?",             pp_symbol_p,            1,  1, { ___,___,___ } },
 { "truncate",            pp_truncate,            1,  1, { REA,___,___ } },
 { "unquote",             pp_unquote,             1,  1, { ___,___,___ } },
 { "unquote-splicing",    pp_unquote_splicing,    1,  1, { ___,___,___ } },
 { "vector",              pp_vector,              0, -1, { ___,___,___ } },
 { "vector->list",        pp_vector_to_list,      1,  1, { VEC,___,___ } },
 { "vector-append",       pp_vector_append,       0, -1, { VEC,___,___ } },
 { "vector-copy",         pp_vector_copy,         1, -1, { VEC,INT,INT } },
 { "vector-fill!",        pp_vector_fill_b,       2,  2, { VEC,___,___ } },
 { "vector-length",       pp_vector_length,       1,  1, { VEC,___,___ } },
 { "vector-ref",          pp_vector_ref,          2,  2, { VEC,INT,___ } },
 { "vector-set!",         pp_vector_set_b,        3,  3, { VEC,INT,___ } },
 { "vector?",             pp_vector_p,            1,  1, { ___,___,___ } },
 { "write",               pp_write,               1,  2, { ___,OUP,___ } },
 { "write-char",          pp_write_char,          1,  2, { CHR,OUP,___ } },
 { "zero?",               pp_zero_p,              1,  1, { REA,___,___ } },
#ifdef unix
 { "argv",                pp_argv,                1,  1, { INT,___,___ } },
 { "environ",             pp_environ,             1,  1, { STR,___,___ } },
 { "system",              pp_system,              1,  1, { STR,___,___ } },
#endif
 { NULL }
};

cell expected(cell who, char *what, cell got) {
	char	msg[100];
	PRIM	*p;

	p = &Primitives[cadr(who)];
	sprintf(msg, "%s: expected %s, got", p->name, what);
	return error(msg, got);
}

cell apply_primitive(cell x) {
	cell	op, args;
	char	*s;

	op = car(x);
	args = cdr(x);
	if ((s = typecheck(op, args)) != NULL)
		return error(s, args);
	return apply_prim(op, args);
}

int uses_transformer_p(cell x) {
	cell	y;
	int	special = 0;

	if (atom_p(x) || car(x) == S_quote)
		return 0;
	/* Skip argument lists of LAMBDA and DEFINE */
	if (	pair_p(x) &&
		(car(x) == S_lambda || car(x) == S_define) &&
		pair_p(cdr(x)) &&
		pair_p(cadr(x))
	) {
		x = cddr(x);
		special = 1;
	}
	if (!special && pair_p(x) && symbol_p(car(x))) {
		y = lookup(car(x), Environment, 0);
		if (y != NIL && syntax_p(binding_value(y)))
			return 1;
	}
	while (pair_p(x)) {
		if (uses_transformer_p(car(x)))
			return 1;
		x = cdr(x);
	}
	return 0;
}

cell xeval(cell x, int cbn);

cell expand_syntax_1(cell x) {
	cell	y, m, n, a, app;

	if (Error_flag || atom_p(x) || car(x) == S_quote)
		return x;
	if (symbol_p(car(x))) {
		y = lookup(car(x), Environment, 0);
		if (y != NIL && syntax_p(binding_value(y))) {
			save(x);
			app = cons(cdr(binding_value(y)), cdr(x));
			unsave(1);
			return xeval(app, 1);
		}
	}
	/*
	 * If DEFINE-SYNTAX is followed by (MACRO-NAME ...)
	 * unbind the MACRO-NAME first to avoid erroneous
	 * expansion.
	 */
	if (	car(x) == S_define_syntax &&
		pair_p(cdr(x)) &&
		pair_p(cadr(x))
	) {
		m = lookup(caadr(x), Environment, 0);
		if (m != NIL)
			binding_value(m) = UNDEFINED;
	}
	n = a = NIL;
	save(n);
	/*
	 * If LAMBDA or DEFINE is followed by an argument list,
	 * skip over it
	 */
	if (	(car(x) == S_lambda || car(x) == S_define) &&
		pair_p(x) &&
		pair_p(cadr(x))
	) {
		n = cons(car(x), cons(cadr(x), NIL));
		car(Stack) = n;
		a = cdr(n);
		x = cddr(x);
	}
	while (pair_p(x)) {
		m = cons(expand_syntax_1(car(x)), NIL);
		if (n == NIL) {
			n = m;
			car(Stack) = n;
			a = n;
		}
		else {
			cdr(a) = m;
			a = cdr(a);
		}
		x = cdr(x);
	}
	cdr(a) = x;
	unsave(1);
	return n;
}

cell expand_syntax(cell x) {
	if (Error_flag || atom_p(x) || car(x) == S_quote)
		return x;
	save(x);
	while (!Error_flag) {
		if (!uses_transformer_p(x))
			break;
		x = expand_syntax_1(x);
		car(Stack) = x;
	}
	unsave(1);
	return x;
}

cell restore_state(void) {
	cell	v;

	if (State_stack == NIL)
		fatal("restore_state: stack underflow");
	v = car(State_stack);
	State_stack = cdr(State_stack);
	return v;
}

cell bind_arguments(cell n) {
	cell	p, v, a;
	cell	rib;

	save(Environment);
	p = car(n);
	a = cdr(n);
	v = cadr(p);
	Environment = cdddr(p);
	rib = NIL;
	save(rib);
	while (pair_p(v)) {
		if (atom_p(a)) {
			unsave(1);
			return too_few_args(n);
		}
		Tmp = make_binding(car(v), car(a));
		rib = cons(Tmp, rib);
		car(Stack) = rib;
		v = cdr(v);
		a = cdr(a);
	}
	if (symbol_p(v)) {
		Tmp = make_binding(v, a);
		rib = cons(Tmp, rib);
		car(Stack) = rib;
	}
	else if (a != NIL) {
		unsave(1);
		return too_many_args(n);
	}
	Tmp = NIL;
	unsave(1);
	Environment = make_env(rib, Environment);
	return UNSPECIFIC;
}

int tail_call(void) {
	if (State_stack == NIL || car(State_stack) != EV_BETA)
		return 0;
	Tmp = unsave(1);
	Environment = car(Stack);
	unsave(2);
	restore_state();
	save(Tmp);
	Tmp = NIL;
	return 1;
}

void trace(cell name, cell expr) {
	if (Error_flag)
		return;
	if (	Trace_list == TRUE ||
		memqv("trace", 0, name, Trace_list) != FALSE
	) {
		prints("+ ");
		print_form(cons(name, cdr(expr)));
		nl();
	}
}

cell xeval(cell x, int cbn) {
	cell	m2,	/* Root of result list */
		a,	/* Used to append to result */
		rib;	/* Temp storage for args */
	int	s,	/* Current state */
		c;	/* Continue */
	cell	name;	/* Name of procedure to apply */

	save(x);
	save(State_stack);
	save(Stack_bottom);
	Stack_bottom = Stack;
	s = EV_ATOM;
	c = 0;
	while (!Error_flag) {
		if (Eval_stats) {
			count(&Reductions);
		}
		if (symbol_p(x)) {		/* Symbol -> Value */
			if (cbn) {
				Acc = x;
				cbn = 0;
			}
			else {
				Acc = lookup(x, Environment, 1);
				if (Error_flag)
					break;
				Acc = box_value(Acc);
			}
		}
		else if (auto_quoting_p(x) || cbn == 2) {
			Acc = x;		/* Object -> Object */
			cbn = 0;
		}
		else {				/* (...) -> Value */
			/*
			 * This block is used to DESCEND into lists.
			 * The following structure is saved on the
			 * Stack: RIB = (args append result source)
			 * The current s is saved on the State_stack.
			 */
			Acc = x;
			x = car(x);
			save_state(s);
			/* Check call-by-name built-ins and flag */
			if (special_p(x) || cbn) {
				cbn = 0;
				rib = cons(Acc, Acc);	/* result/source */
				rib = cons(NIL, rib);	/* append */
				rib = cons(NIL, rib);	/* args */
				if (!proper_list_p(Acc))
					error("syntax error", Acc);
				x = NIL;
			}
			else {
				Tmp = cons(NIL, NIL);
				rib = cons(Tmp, Acc);	/* result/source */
				rib = cons(Tmp, rib);	/* append */
				rib = cons(cdr(Acc), rib); /* args */
				Tmp = NIL;
				x = car(Acc);
			}
			save(rib);
			s = EV_ARGS;
			continue;
		}
		/*
		 * The following loop is used to ASCEND back to the
		 * root of a list, thereby performing BETA REDUCTION.
		 */
		while (!Error_flag)
		if (s == EV_BETA) {
			/* Finish BETA reduction */
			Environment = unsave(1);
			unsave(1);		/* source expression */
			s = restore_state();
		}
		else if (s == EV_ARGS) {	/* append to list, reduce */
			rib = car(Stack);
			x = rib_args(rib);
			a = rib_append(rib);
			m2 = rib_result(rib);
			if (a != NIL) 	/* Append new member */
				car(a) = Acc;
			if (x == NIL) {	/* End of list */
				Acc = m2;
				/* Remember name of caller */
				name = car(rib_source(rib));
				if (Trace_list != NIL)
					trace(name, Acc);
				if (primitive_p(car(Acc))) {
					if (cadar(Acc) == Apply_magic)
						c = cbn = 1;
					if (cadar(Acc) == Callcc_magic)
						c = cbn = 1;
					cons_stats(1);
					Acc = x = apply_primitive(Acc);
					cons_stats(0);
				}
				else if (special_p(car(Acc))) {
					Acc = x = apply_special(Acc, &c, &s);
				}
				else if (function_p(car(Acc))) {
					name = symbol_p(name)? name: NIL;
					Called_procedures[Proc_ptr] = name;
					Proc_ptr++;
					if (Proc_ptr >= MAX_CALL_TRACE)
						Proc_ptr = 0;
					tail_call();
					bind_arguments(Acc);
					x = caddar(Acc);
					c = 2;
					s = EV_BETA;
				}
				else if (continuation_p(car(Acc))) {
					Acc = resume(Acc);
				}
				else {
					error("application of non-procedure",
						name);
					x = NIL;
				}
				if (c != 2) {
					unsave(1); /* drop source expr */
					s = restore_state();
				}
				/* Leave the ASCENDING loop and descend */
				/* once more into X. */
				if (c)
					break;
			}
			else if (atom_p(x)) {
				error("syntax error", rib_source(rib));
				x = NIL;
				break;
			}
			else {		/* X =/= NIL: append to list */
				/* Create space for next argument */
				Acc = cons(NIL, NIL);
				cdr(a) = Acc;
				rib_append(rib) = cdr(a);
				rib_args(rib) = cdr(x);
				x = car(x);	/* evaluate next member */
				break;
			}
		}
		else if (s == EV_IF_PRED) {
			x = unsave(1);
			unsave(1);	/* source expression */
			s = restore_state();
			if (Acc != FALSE)
				x = cadr(x);
			else
				x = caddr(x);
			c = 1;
			break;
		}
		else if (s == EV_AND || s == EV_OR) {
			Stack = cons(cdar(Stack), cdr(Stack));
			if (	(Acc == FALSE && s == EV_AND) ||
				(Acc != FALSE && s == EV_OR) ||
				car(Stack) == NIL
			) {
				unsave(2);	/* state, source expr */
				s = restore_state();
				x = Acc;
				cbn = 2;
			}
			else if (cdar(Stack) == NIL) {
				x = caar(Stack);
				unsave(2);	/* state, source expr */
				s = restore_state();
			}
			else {
				x = caar(Stack);
			}
			c = 1;
			break;
		}
		else if (s == EV_COND) {
			char	cond_err[] = "cond: invalid syntax";

			if (Acc != FALSE) {
				x = cdaar(Stack);
				if (x == NIL) {
					x = quote(Acc, S_quote);
				}
				else if (pair_p(cdr(x))) {
					if (car(x) == S_arrow) {
						if (cddr(x) != NIL)
							error(cond_err, x);
						Acc = quote(Acc, S_quote);
						Acc = cons(Acc, NIL);
						Acc = x = cons(cadr(x), Acc);
					}
					else {
						Acc = x = cons(S_begin, x);
					}
				}
				else {
					x = car(x);
				}
				unsave(2);	/* state, source expr */
				s = restore_state();
			}
			else if (cdar(Stack) == NIL)  {
				unsave(2);	/* state, source expr */
				s = restore_state();
				x = UNSPECIFIC;
			}
			else {
				Stack = cons(cdar(Stack), cdr(Stack));
				x = caaar(Stack);
				if (x == S_else && cdar(Stack) == NIL)
					x = TRUE;
			}
			c = 1;
			break;
		}
		else if (s == EV_BEGIN) {
			Stack = cons(cdar(Stack), cdr(Stack));
			if (cdar(Stack) == NIL) {
				x = caar(Stack);
				unsave(2);	/* state, source expr */
				s = restore_state();
			}
			else {
				x = caar(Stack);
			}
			c = 1;
			break;
		}
		else if (s == EV_SET_VAL || s == EV_MACRO) {
			char err[] = "define-syntax: expected procedure, got";

			if (s == EV_MACRO) {
				if (function_p(Acc)) {
					Acc = new_atom(T_SYNTAX, Acc);
				}
				if (syntax_p(Acc)) {
					/* Acc = Acc; */
				}
				else {
					error(err, Acc);
					break;
				}
			}
			x = unsave(1);
			unsave(1);	/* source expression */
			s = restore_state();
			box_value(x) = Acc;
			Acc = x = UNSPECIFIC;
			c = 0;
			break;
		}
		else { /* s == EV_ATOM */
			break;
		}
		if (c) {	/* Continue evaluation if requested */
			c = 0;
			continue;
		}
		if (Stack == Stack_bottom)
			break;
	}
	Stack = Stack_bottom;
	Stack_bottom = unsave(1);
	State_stack = unsave(1);
	unsave(1);
	return Acc;		/* Return the evaluated expr */
}

void reset_calltrace(void) {
	int	i;

	for (i=0; i<MAX_CALL_TRACE; i++)
		Called_procedures[i] = NIL;
}

cell eval(cell x) {
	reset_calltrace();
	save(x);
	x = expand_syntax(x);
	unsave(1);
	x = xeval(x, 0);
	return x;
}

/*
 * REPL
 */

void clear_leftover_envs(void) {
	while (cdr(Environment) != NIL)
		Environment = cdr(Environment);
}

#ifdef unix
 void keyboard_interrupt(int sig) {
	reset_std_ports();
	error("interrupted", NOEXPR);
	Intr_flag = 1;
	signal(SIGINT, keyboard_interrupt);
 }

 void keyboard_quit(int sig) {
	reset_tty();
	fatal("received quit signal, exiting");
 }

 void terminated(int sig) {
	quit(1);
 }
#endif /* unix */

#ifdef plan9
 void keyboard_interrupt(void *dummy, char *note) {
	if (strstr(note, "interrupt") == NULL)
		noted(NDFLT);
	reset_std_ports();
	error("interrupted", NOEXPR);
	Intr_flag = 1;
	noted(NCONT);
 }
#endif /* plan9 */

void mem_error(int src) {
	if (src == 1)
		error("hit node limit", NOEXPR);
	else
		error("hit vector limit", NOEXPR);
}

void repl(void) {
	cell	n = NIL; /*LINT*/
	cell	sane_env;

	mem_error_handler(mem_error);
	sane_env = cons(NIL, NIL);
	save(sane_env);
	if (!Quiet_mode) {
		handle_sigint();
	}
	while (1) {
		reset_tty();
		Error_flag = 0;
		reset_std_ports();
		clear_leftover_envs();
		reset_calltrace();
		car(sane_env) = Environment;
		if (!Quiet_mode) {
			prints("> "); flush();
		}
		Intr_flag = 0;
		Program = xread();
		if (Program == END_OF_FILE && !Intr_flag)
			break;
		if (!Error_flag)
			n = eval(Program);
		if (!Error_flag && !unspecific_p(n)) {
			print_form(n);
			prints("\n");
			box_value(S_latest) = n;
		}
		if (Error_flag)
			Environment = car(sane_env);
	}
	unsave(1);
	prints("\n");
}

/*
 * Startup and Initialization
 */

int exists(char *p) {
	FILE	*f;

	if ((f = fopen(p, "r")) == NULL)
		return 0;
	fclose(f);
	return 1;
}

cell get_library_path(void) {
	char	*s;

	s = getenv("S9FES_LIBRARY_PATH");
	if (s == NULL)
		s = LIBRARY_PATH;
	return make_string(s, (int) strlen(s));
}

int try_image(char *p, char *i) {
	char	path[TOKEN_LENGTH];
	char	*msg;
	cell	new;

	if (strlen(p) + strlen(i) + 8 >= TOKEN_LENGTH) {
		error("image path too long", make_string(p, strlen(p)));
		return 0;
	}
	sprintf(path, "%s/%s.image", p, i);
	if (!exists(path))
		return 0;
	if ((msg = load_image(path, S9magic)) != NULL) {
		error(msg, make_string(path, strlen(path)));
		fatal("bad image file");
	}
	/* *library-path* is overwritten by image */
	new = get_library_path();
	box_value(S_library_path) = new;
	return 1;
}

int try_src(char *p, char *i) {
	char	path[TOKEN_LENGTH];

	if (strlen(p) + strlen(i) + 6 >= TOKEN_LENGTH) {
		error("source path too long", make_string(p, strlen(p)));
		return 0;
	}
	sprintf(path, "%s/%s.scm", p, i);
	return load(path) == 0;
}

void load_library(char *argv0) {
	char	pathbuf[TOKEN_LENGTH];
	char	*libpath;
	char	*s;
	char	s9[] = "s9";

	if ((s = strrchr(argv0, '/')) != NULL)
		argv0 = s+1;
	libpath = string(binding_value(S_library_path));
	if (strlen(libpath) >= TOKEN_LENGTH)
		fatal("library path too long");
	if (!strcmp(argv0, "-")) {
		if (try_src(".", s9)) return;
		if (libpath && try_src(libpath, s9)) return;
	}
	else {
		libpath = strcpy(pathbuf, libpath);
		s = strtok(libpath, ":");
		while (s != NULL) {
			if (try_image(s, argv0)) return;
			s = strtok(NULL, ":");
		}
		libpath = strcpy(pathbuf, libpath);
		s = strtok(libpath, ":");
		while (s != NULL) {
			if (try_src(s, argv0)) return;
			s = strtok(NULL, ":");
		}
	}
	fatal("no suitable image file or library source found");
}

void add_primitives(char *name, PRIM *p) {
	cell	v, n, new;
	int	i;

	if (name) {
		n = symbol_ref(name);
		new = cons(n, box_value(S_extensions));
		box_value(S_extensions) = new;
	}
	for (i=0; p && p[i].name; i++) {
		v = symbol_ref(p[i].name);
		n = make_primitive(&p[i]);
		if (Apply_magic < 0 && !strcmp(p[i].name, "apply"))
			Apply_magic = prim_slot(n);
		if (Callcc_magic < 0 && !strcmp(p[i].name, "call/cc"))
			Callcc_magic = prim_slot(n);
		Environment = extend(v, n, Environment);
	}
}

cell get_args(char **argv) {
	int	i;
	cell	a, n;

	if (argv[0] == NULL)
		return Argv = NIL;
	a = cons(NIL, NIL);
	save(a);
	for (i = 0; argv[i] != NULL; i++) {
		n = make_string(argv[i], strlen(argv[i]));
		car(a) = n;
		if (argv[i+1] != NULL) {
			n = cons(NIL, NIL);
			cdr(a) = n;
			a = cdr(a);
		}
	}
	return Argv = unsave(1);
}

#ifndef EXTENSIONS
 #define EXTENSIONS
#endif

/* Extension prototypes; add your own here. */
void curs_init(void);
void sys_init(void);

void make_initial_env(void) {
	cell	new;

	Environment = cons(NIL, NIL);
	Environment = extend(symbol_ref("**"), UNDEFINED, Environment);
	S_latest = cadr(Environment);
	Environment = extend(symbol_ref("*arguments*"), NIL, Environment);
	S_arguments = cadr(Environment);
	Environment = extend(symbol_ref("*epsilon*"), Epsilon, Environment);
        Environment = extend(symbol_ref("*extensions*"), NIL, Environment);
	S_extensions = cadr(Environment);
        Environment = extend(symbol_ref("*host-system*"), NIL, Environment);
	S_host_system = cadr(Environment);
#ifdef unix
	box_value(S_host_system) = symbol_ref("unix");
#else
 #ifdef plan9
	box_value(S_host_system) = symbol_ref("plan9");
 #else
	box_value(S_host_system) = FALSE;
 #endif
#endif
	Environment = extend(symbol_ref("*library-path*"), NIL, Environment);
	S_library_path = cadr(Environment);
	new = get_library_path();
	box_value(S_library_path) = new;
        Environment = extend(symbol_ref("*loading*"), FALSE, Environment);
	S_loading = cadr(Environment);
	Apply_magic = -1;
	Callcc_magic = -1;
	add_primitives(NULL, Core_primitives);
	EXTENSIONS;
	Environment = cons(Environment, NIL);
	Program = TRUE; /* or rehash() will not work */
	rehash(car(Environment));
}

cell *GC_root[] = {
	&Program, &Environment, &Tmp, &Tmp_car, &Tmp_cdr,
	&Stack_bottom, &State_stack, &Acc, &Trace_list,
	&File_list,
NULL };

cell *Image_vars[] = {
	&Environment, &S_and, &S_arguments, &S_arrow, &S_begin,
	&S_cond, &S_define, &S_define_syntax, &S_else, &S_extensions,
	&S_host_system, &S_if, &S_lambda, &S_latest, &S_library_path,
	&S_loading, &S_or, &S_quasiquote, &S_quote, &S_quote, &S_set_b,
	&S_unquote, &S_unquote_splicing,
NULL };

void init(void) {
	strcpy(S9magic, "S9:");
	strcat(S9magic, VERSION);
	s9init(GC_root);
	image_vars(Image_vars);
	exponent_chars("eEdDfFlLsS");
	Stack_bottom = NIL;
	State_stack = NIL;
	Tmp_car = NIL;
	Tmp_cdr = NIL;
	Tmp = NIL;
	Program = NIL;
	Proc_ptr = 0;
	Environment = NIL;
	Acc = NIL;
	Trace_list = NIL;
	Level = 0;
	Line_no = 1;
	Error_flag = 0;
	Intr_flag = 0;
	Load_level = 0;
	File_list = NIL;
	Displaying = 0;
	Quiet_mode = 0;
	Eval_stats = 0;
	S_arrow = symbol_ref("=>");
	S_and = symbol_ref("and");
	S_begin = symbol_ref("begin");
	S_cond = symbol_ref("cond");
	S_define = symbol_ref("define");
	S_define_syntax = symbol_ref("define-syntax");
	S_else = symbol_ref("else");
	S_if = symbol_ref("if");
	S_lambda = symbol_ref("lambda");
	S_or = symbol_ref("or");
	S_quasiquote = symbol_ref("quasiquote");
	S_quote = symbol_ref("quote");
	S_set_b = symbol_ref("set!");
	S_unquote = symbol_ref("unquote");
	S_unquote_splicing = symbol_ref("unquote-splicing");
	make_initial_env();
	reset_calltrace();
}

void init_extensions(void) {
	cell	e, n;
	char	initproc[TOKEN_LENGTH+2];
	char	*s;
	char	*s9 = "s9";

	e = box_value(S_extensions);
	while (s9 || e != NIL) {
		if (e == NIL) {
			s = s9; 
			s9 = NULL;
		}
		else {
			s = string(car(e));
		}
		if (strlen(s)*2+1 >= TOKEN_LENGTH)
			fatal("init_extension(): procedure name too long");
		sprintf(initproc, "%s:%s", s, s);
		n = find_symbol(initproc);
		if (n != NIL) {
			n = cons(n, NIL);
			eval(n);
		}
		if (e != NIL)
			e = cdr(e);
	}
}

void usage(int q) {
	prints("Usage: s9 [-hv?] [-i name|-] [-gqu] [-d image]");
	prints(" [-k size[m]] [-l prog]");
	nl();
	prints("          [-n size[m]] [[-f] prog [arg ...]]");
	prints(" [-- [arg ...]]");
	nl();
	if (q) quit(1);
}

void long_usage(void) {
	cell	x;

	nl();
	prints("Scheme 9 from Empty Space by Nils M Holm, ");
	prints(VERSION);
	nl();
	prints("");
#ifdef unix
	prints("unix");
#else
 #ifdef plan9
	prints("plan 9");
 #else
	prints("unknown platform");
 #endif
#endif
#ifdef BITS_PER_WORD_64
	prints(", 64 bits");
#else
 #ifdef BITS_PER_WORD_32
	prints(", 32 bits");
 #endif
#endif
	x = binding_value(S_extensions);
	if (x == NIL) {
		prints(", no extensions");
	}
	else {
		prints(", extensions: ");
		while (x != NIL) {
			print_form(car(x));
			x = cdr(x);
			if (x != NIL)
				prints(", ");
		}
	}
	nl();
	prints("library path: ");
	prints(string(binding_value(S_library_path)));
	nl();
	prints("This program is in the public domain");
	nl();
	nl();
	usage(0);
	nl();
	prints("-h        display this summary (also -v, -?)"); nl();
	prints("-i name   base name of image file (must be first option!)");
	nl();
	prints("-i -      ignore image, load source file instead"); nl();
	prints("-d file   dump heap image to file and exit"); nl();
	prints("-g        print GC summaries (-gg = more)"); nl();
	prints("-f file   run program with args, then exit (-f is optional)");
	nl();
	prints("-k n[m]   set vector limit to nK (or nM) cells"); nl();
	prints("-l file   load program (may be repeated)"); nl();
	prints("-n n[m]   set node limit to nK (or nM) nodes"); nl();
	prints("-q        be quiet (no banner, no prompt, exit on errors)");
	nl();
	prints("-u        use unlimited node and vector memory"); nl();
	prints("-- args   bind remaining arguments to *arguments*");
	nl();
	nl();
}

long get_size_k(char *s) {
	int	c;
	long	n;

	c = s[strlen(s)-1];
	n = asctol(s);
	if (c == 'M' || c == 'm')
		n *= 1024L;
	else if (!isdigit(c))
		usage(1);
	return n * 1024;
}

int main(int argc, char **argv) {
	int	vgc = 0;
	int	f_opt = 0;
	int	arg_opt = 0;
	char	*argv0;
	char	*s;

	if (argc > 2 && !strcmp(argv[1], "-i"))
		argv += 2;
	init();
	handle_sigquit();
	handle_sigterm();
	argv0 = *argv++;
	load_library(argv0);
	init_extensions();
	while (*argv != NULL && !f_opt && !arg_opt) {
		if (**argv != '-')
			break;
		(*argv)++;
		while (**argv) {
			switch (**argv)  {
			case '-':
				arg_opt = 1;
				(*argv)++;
				break;
			case 'd':
				if (argv[1] == NULL)
					usage(1);
				s = dump_image(argv[1], S9magic);
				if (s != NULL)
					error(s, NOEXPR);
				quit(Error_flag? 1: 0);
				break;
			case 'f':
				if (argv[1] == NULL)
					usage(1);
				(*argv)++;
				f_opt = 1;
				break;
			case 'g':
				vgc++;
				(*argv)++;
				break;
			case 'k':
				if (argv[1] == NULL)
					usage(1);
				set_vector_limit(get_size_k(argv[1]));
				argv++;
				*argv += strlen(*argv);
				break;
			case 'l':
				if (argv[1] == NULL)
					usage(1);
				if (load(argv[1]))
					error("program file not found",
						make_string(argv[1],
							(int)strlen(argv[1])));
				if (Error_flag)
					quit(1);
				argv++;
				*argv = &(*argv)[strlen(*argv)];
				break;
			case 'n':
				if (argv[1] == NULL)
					usage(1);
				set_node_limit(get_size_k(argv[1]));
				argv++;
				*argv += strlen(*argv);
				break;
			case 'q':
				Quiet_mode = 1;
				(*argv)++;
				break;
			case 'u':
				set_node_limit(0);
				set_vector_limit(0);
				break;
			case 'h':
			case 'v':
			case '?':
				long_usage();
				quit(0);
				break;
			default:
				usage(1);
				break;
			}
		}
		argv++;
	}
	gc_verbosity(vgc % 3);
	if (argv[0] != NULL && !arg_opt) {
		box_value(S_arguments) = get_args(argv+1);
		Quiet_mode = 1;
		if (load(argv[0]))
			error("program file not found",
				make_string(argv[0], (int)strlen(argv[0])));
		quit(Error_flag);
	}
	box_value(S_arguments) = get_args(argv);
	if (!Quiet_mode)
		prints("Scheme 9 from Empty Space\n");
	repl();
	return 0;
}
