/*
 *      ---------- Mini-Scheme Interpreter Version 0.85 ----------
 *
 *                coded by Atsushi Moriwaki (11/5/1989)
 *
 *            E-MAIL :  moriwaki@kurims.kurims.kyoto-u.ac.jp
 *
 *               THIS SOFTWARE IS IN THE PUBLIC DOMAIN
 *               ------------------------------------
 * This software is completely free to copy, modify and/or re-distribute.
 * But I would appreciate it if you left my name on the code as the author.
 *
 */
/*--
 *
 *  This version has been modified by Chris Pressey.
 *	current version is 0.85p1 (as yet unreleased)
 *
 *  This version has been modified by R.C. Secrist.
 *
 *  Mini-Scheme is now maintained by Akira KIDA.
 *
 *  This is a revised and modified version by Akira KIDA.
 *	current version is 0.85k4 (15 May 1994)
 *
 *  Please send suggestions, bug reports and/or requests to:
 *		<SDI00379@niftyserve.or.jp>
 *--
 */ 

/*
 * Here is System declaration.
 * Please define exactly one symbol in the following section.
 */
/* #define LSC		*/	/* LightSpeed C for Macintosh */
/* #define LSC4		*/	/* THINK C version 4.0 for Macintosh */
/* #define MPW2		*/	/* Macintosh Programmer's Workshop v2.0x */
/* #define BSD		*/	/* 4.x BSD */
/* #define MSC		*/	/* Microsoft C Compiler v.4.00 - 7.00 */
/* #define TURBOC	*/	/* Turbo C compiler v.2.0, or TC++ 1.0  */
/* #define SYSV		*/	/* System-V, or POSIX */
/* #define VAXC		*/	/* VAX/VMS VAXC 2.x or later */ /* (automatic) */

#ifdef __BORLANDC__	/* Borland C++ - MS-DOS */
#define TURBOC
#endif

#ifdef __TURBOC__	/* Turbo C V1.5 - MS-DOS */
#define TURBOC
#endif

#ifdef mips		/* DECstation running OSF/1 */
#define BSD
#endif

#ifdef __osf__		/* Alpha AXP running OSF/1 */
#define BSD
#endif

#ifdef __DECC		/* Alpha AXP running VMS */
#define VAXC
#endif

#ifdef _AIX		/* RS/6000 running AIX */
#define BSD
#endif

/*
 * Define or undefine following symbols as you need.
 */
/* #define VERBOSE */	/* define this if you want verbose GC */
#define	AVOID_HACK_LOOP	/* define this if your compiler is poor
			 * enougth to complain "do { } while (0)"
			 * construction.
			 */
#define USE_SETJMP	/* undef this if you do not want to use setjmp() */
#define USE_QQUOTE	/* undef this if you do not need quasiquote */
#define USE_MACRO	/* undef this if you do not need macro */

	
#ifdef USE_QQUOTE
/*--
 *  If your machine can't support "forward single quotation character"
 *  i.e., '`',  you may have trouble to use backquote.
 *  So use '^' in place of '`'.
 */
# define BACKQUOTE '`'
#endif

/*
 *  Basic memory allocation units
 */

#ifdef TURBOC             	/* rcs */
#define CELL_SEGSIZE  2048
#define CELL_NSEGMENT  100
#define STR_SEGSIZE   2048
#define STR_NSEGMENT   100
#else
#define CELL_SEGSIZE    5000	/* # of cells in one segment */
#define CELL_NSEGMENT   100	/* # of segments for cells */
#define STR_SEGSIZE     2500	/* bytes of one string segment */
#define STR_NSEGMENT    100	/* # of segments for strings */
#endif



#define banner "Hello, This is Mini-Scheme Interpreter Version 0.85p1.\n"


#include <stdio.h>
#include <ctype.h>
#ifdef USE_SETJMP
#include <setjmp.h>
#endif


/* System dependency */
#ifdef LSC
#include <strings.h>
#include <unix.h>
#define malloc(x)	NewPtr((long)(x))
#define prompt "> "
#define InitFile "init.scm"
#define FIRST_CELLSEGS 5
#endif

#ifdef LSC4
#include <string.h>
#include <stdlib.h>
#define malloc(x)	NewPtr((long)(x))
#define prompt "> "
#define InitFile "init.scm"
#define FIRST_CELLSEGS 5
#endif

#ifdef MPW2
#include <strings.h>
#include <memory.h>
#define malloc(x)	NewPtr((long)(x))
#define prompt "> [enter at next line]\n"
#define InitFile "init.scm"
#define FIRST_CELLSEGS 5
#endif

#ifdef BSD
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#define prompt "> "
#define InitFile "init.scm"
#define FIRST_CELLSEGS 10
#endif

#ifdef MSC
#include <string.h>
#include <stdlib.h>
#include <malloc.h>
#include <process.h>
#define prompt "> "
#define InitFile "init.scm"
#define FIRST_CELLSEGS 3
#endif

#ifdef TURBOC
#include <string.h>
#include <stdlib.h>
#define prompt "> "
#define InitFile "init.scm"
#define FIRST_CELLSEGS 3
#endif

#ifdef SYSV
#include <string.h>
#include <malloc.h>
#if __STDC__
# include <stdlib.h>
#endif
#define prompt "> "
#define InitFile "init.scm"
#define FIRST_CELLSEGS 10
#endif

#ifdef	VAXC
#include <string.h>
#include <stdlib.h>
#define prompt "> "
#define InitFile "init.scm"
#define FIRST_CELLSEGS 10
#endif

#ifdef __GNUC__
/*
 * If we use gcc, AVOID_HACK_LOOP is unnecessary
 */
#undef AVOID_HACK_LOOP
#endif

#ifndef	FIRST_CELLSEGS
#error Please define your system type.
/*
 * We refrain this to raise an error anyway even if on pre-ANSI system.
 */
error Please define your system type.
#endif

/* cell structure */
struct cell {
	unsigned short _flag;
	union {
		struct {
			char   *_svalue;
			short   _keynum;
		} _string;
		struct {
			long    _ivalue;
		} _number;
		struct {
			struct cell *_car;
			struct cell *_cdr;
		} _cons;
	} _object;
};

typedef struct cell *pointer;

#define T_STRING         1	/* 0000000000000001 */
#define T_NUMBER         2	/* 0000000000000010 */
#define T_SYMBOL         4	/* 0000000000000100 */
#define T_SYNTAX         8	/* 0000000000001000 */
#define T_PROC          16	/* 0000000000010000 */
#define T_PAIR          32	/* 0000000000100000 */
#define T_CLOSURE       64	/* 0000000001000000 */
#define T_CONTINUATION 128	/* 0000000010000000 */
#ifdef USE_MACRO
# define T_MACRO        256	/* 0000000100000000 */
#endif
#define T_PROMISE      512	/* 0000001000000000 */
#define T_ATOM       16384	/* 0100000000000000 */	/* only for gc */
#define CLRATOM      49151	/* 1011111111111111 */	/* only for gc */
#define MARK         32768	/* 1000000000000000 */
#define UNMARK       32767	/* 0111111111111111 */

/* macros for cell operations */
#define type(p)         ((p)->_flag)

#define isstring(p)     (type(p)&T_STRING)
#define strvalue(p)     ((p)->_object._string._svalue)
#define keynum(p)       ((p)->_object._string._keynum)

#define isnumber(p)     (type(p)&T_NUMBER)
#define ivalue(p)       ((p)->_object._number._ivalue)

#define ispair(p)       (type(p)&T_PAIR)
#define car(p)          ((p)->_object._cons._car)
#define cdr(p)          ((p)->_object._cons._cdr)

#define issymbol(p)     (type(p)&T_SYMBOL)
#define symname(p)      strvalue(car(p))
#define hasprop(p)      (type(p)&T_SYMBOL)
#define symprop(p)      cdr(p)

#define issyntax(p)     (type(p)&T_SYNTAX)
#define isproc(p)       (type(p)&T_PROC)
#define syntaxname(p)   strvalue(car(p))
#define syntaxnum(p)    keynum(car(p))
#define procnum(p)      ivalue(p)

#define isclosure(p)    (type(p)&T_CLOSURE)
#ifdef USE_MACRO
# define ismacro(p)      (type(p)&T_MACRO)
#endif
#define closure_code(p) car(p)
#define closure_env(p)  cdr(p)

#define iscontinuation(p) (type(p)&T_CONTINUATION)
#define cont_dump(p)    cdr(p)

#define ispromise(p)    (type(p)&T_PROMISE)
#define setpromise(p)   type(p) |= T_PROMISE

#define isatom(p)       (type(p)&T_ATOM)
#define setatom(p)      type(p) |= T_ATOM
#define clratom(p)      type(p) &= CLRATOM

#define ismark(p)       (type(p)&MARK)
#define setmark(p)      type(p) |= MARK
#define clrmark(p)      type(p) &= UNMARK

#define caar(p)         car(car(p))
#define cadr(p)         car(cdr(p))
#define cdar(p)         cdr(car(p))
#define cddr(p)         cdr(cdr(p))
#define cadar(p)        car(cdr(car(p)))
#define caddr(p)        car(cdr(cdr(p)))
#define cadaar(p)       car(cdr(car(car(p))))
#define cadddr(p)       car(cdr(cdr(cdr(p))))
#define cddddr(p)       cdr(cdr(cdr(cdr(p))))

/* arrays for segments */
pointer cell_seg[CELL_NSEGMENT];
int     last_cell_seg = -1;
char   *str_seg[STR_NSEGMENT];
int     str_seglast = -1;

/* We use 4 registers. */
pointer args;			/* register for arguments of function */
pointer envir;			/* stack register for current environment */
pointer code;			/* register for current code */
pointer dump;			/* stack register for next evaluation */

struct cell _NIL;
pointer NIL = &_NIL;		/* special cell representing empty cell */
struct cell _T;
pointer T = &_T;		/* special cell representing #t */
struct cell _F;
pointer F = &_F;		/* special cell representing #f */
pointer oblist = &_NIL;		/* pointer to symbol table */
pointer global_env;		/* pointer to global environment */

/* global pointers to special symbols */
pointer LAMBDA;			/* pointer to syntax lambda */
pointer QUOTE;			/* pointer to syntax quote */

#ifdef USE_QQUOTE
pointer QQUOTE;			/* pointer to symbol quasiquote */
pointer UNQUOTE;		/* pointer to symbol unquote */
pointer UNQUOTESP;		/* pointer to symbol unquote-splicing */

#endif

pointer free_cell = &_NIL;	/* pointer to top of free cells */
long    fcells = 0;		/* # of free cells */

FILE   *infp;			/* input file */
FILE   *outfp;			/* output file */

#ifdef USE_SETJMP
jmp_buf error_jmp;

#endif
char    gc_verbose;		/* if gc_verbose is not zero, print gc status */
int     quiet = 0;		/* if not zero, print banner, prompt, results */
int     all_errors_fatal = 0;   /* if not zero, every error is a FatalError */

/* allocate new cell segment */
alloc_cellseg(n)
int     n;
{
	register pointer p;
	register long i;
	register int k;

	for (k = 0; k < n; k++) {
		if (last_cell_seg >= CELL_NSEGMENT - 1)
			return k;
		p = (pointer) malloc(CELL_SEGSIZE * sizeof(struct cell));
		if (p == (pointer) 0)
			return k;
		cell_seg[++last_cell_seg] = p;
		fcells += CELL_SEGSIZE;
		for (i = 0; i < CELL_SEGSIZE - 1; i++, p++) {
			type(p) = 0;
			car(p) = NIL;
			cdr(p) = p + 1;
		}
		type(p) = 0;
		car(p) = NIL;
		cdr(p) = free_cell;
		free_cell = cell_seg[last_cell_seg];
	}
	return n;
}

/* allocate new string segment */
alloc_strseg(n)
int     n;
{
	register char *p;
	register long i;
	register int k;

	for (k = 0; k < n; k++) {
		if (str_seglast >= STR_NSEGMENT)
			return k;
		p = (char *) malloc(STR_SEGSIZE * sizeof(char));
		if (p == (char *) 0)
			return k;
		str_seg[++str_seglast] = p;
		for (i = 0; i < STR_SEGSIZE; i++)
			*p++ = (char) (-1);
	}
	return n;
}

/* initialization of Mini-Scheme */
init_scheme()
{
	register pointer i;

	if (alloc_cellseg(FIRST_CELLSEGS) != FIRST_CELLSEGS)
		FatalError("Unable to allocate initial cell segments");
	if (!alloc_strseg(1))
		FatalError("Unable to allocate initial string segments");
#ifdef VERBOSE
	gc_verbose = 1;
#else
	gc_verbose = 0;
#endif
	init_globals();
}

/* get new cell.  parameter a, b is marked by gc. */
pointer get_cell(a, b)
register pointer a, b;
{
	register pointer x;

	if (free_cell == NIL) {
		gc(a, b);
		if (free_cell == NIL)
#ifdef USE_SETJMP
			if (!alloc_cellseg(1)) {
				args = envir = code = dump = NIL;
				gc(NIL, NIL);
				if (free_cell != NIL)
					Error("run out of cells --- rerurn to top level");
				else
					FatalError("run out of cells --- unable to recover cells");
			}
#else
			if (!alloc_cellseg(1))
				FatalError("run out of cells  --- unable to recover cells");
#endif
	}
	x = free_cell;
	free_cell = cdr(x);
	--fcells;
	return (x);
}

/* get new cons cell */
pointer cons(a, b)
register pointer a, b;
{
	register pointer x = get_cell(a, b);

	type(x) = T_PAIR;
	car(x) = a;
	cdr(x) = b;
	return (x);
}

/* get number atom */
pointer mk_number(num)
register long num;
{
	register pointer x = get_cell(NIL, NIL);

	type(x) = (T_NUMBER | T_ATOM);
	ivalue(x) = num;
	return (x);
}

/* allocate name to string area */
char   *store_string(name)
char   *name;
{
	register char *q;
	register short i;
	long    len, remain;

	/* first check name has already listed */
	for (i = 0; i <= str_seglast; i++)
		for (q = str_seg[i]; *q != (char) (-1); ) {
			if (!strcmp(q, name))
				goto FOUND;
			while (*q++)
				;	/* get next string */
		}
	len = strlen(name) + 2;
	remain = (long) STR_SEGSIZE - ((long) q - (long) str_seg[str_seglast]);
	if (remain < len) {
		if (!alloc_strseg(1))
			FatalError("run out of string area");
		q = str_seg[str_seglast];
	}
	strcpy(q, name);
FOUND:
	return (q);
}

/* get new string */
pointer mk_string(str)
char   *str;
{
	register pointer x = get_cell(NIL, NIL);

	strvalue(x) = store_string(str);
	type(x) = (T_STRING | T_ATOM);
	keynum(x) = (short) (-1);
	return (x);
}

/* get new symbol */
pointer mk_symbol(name)
char   *name;
{
	register pointer x;

	/* fisrt check oblist */
	for (x = oblist; x != NIL; x = cdr(x))
		if (!strcmp(name, symname(car(x))))
			break;

	if (x != NIL)
		return (car(x));
	else {
		x = cons(mk_string(name), NIL);
		type(x) = T_SYMBOL;
		oblist = cons(x, oblist);
		return (x);
	}
}

/* make symbol or number atom from string */
pointer mk_atom(q)
char   *q;
{
	char    c, *p;

	p = q;
	if (!isdigit(c = *p++)) {
		if ((c != '+' && c != '-') || !isdigit(*p))
			return (mk_symbol(q));
	}
	for ( ; (c = *p) != 0; ++p)
		if (!isdigit(c))
			return (mk_symbol(q));
	return (mk_number(atol(q)));
}

/* make constant */
pointer mk_const(name)
char   *name;
{
	long    x;
	char    tmp[256];

	if (!strcmp(name, "t"))
		return (T);
	else if (!strcmp(name, "f"))
		return (F);
	else if (*name == 'o') {/* #o (octal) */
		sprintf(tmp, "0%s", &name[1]);
		sscanf(tmp, "%lo", (unsigned long int *)&x);
		return (mk_number(x));
	} else if (*name == 'd') {	/* #d (decimal) */
		sscanf(&name[1], "%ld", &x);
		return (mk_number(x));
	} else if (*name == 'x') {	/* #x (hex) */
		sprintf(tmp, "0x%s", &name[1]);
		sscanf(tmp, "%lx", (unsigned long int *)&x);
		return (mk_number(x));
	} else
		return (NIL);
}


/* ========== garbage collector ========== */

/*--
 *  We use algorithm E (Kunuth, The Art of Computer Programming Vol.1,
 *  sec.3.5) for marking.
 */
mark(a)
pointer a;
{
	register pointer t, q, p;

E1:	t = (pointer) 0;
	p = a;
E2:	setmark(p);
E3:	if (isatom(p))
		goto E6;
E4:	q = car(p);
	if (q && !ismark(q)) {
		setatom(p);
		car(p) = t;
		t = p;
		p = q;
		goto E2;
	}
E5:	q = cdr(p);
	if (q && !ismark(q)) {
		cdr(p) = t;
		t = p;
		p = q;
		goto E2;
	}
E6:	if (!t)
		return;
	q = t;
	if (isatom(q)) {
		clratom(q);
		t = car(q);
		car(q) = p;
		p = q;
		goto E5;
	} else {
		t = cdr(q);
		cdr(q) = p;
		p = q;
		goto E6;
	}
}


/* garbage collection. parameter a, b is marked. */
gc(a, b)
register pointer a, b;
{
	register pointer p;
	register short i;
	register long j;

	if (gc_verbose)
		printf("gc...");

	/* mark system globals */
	mark(oblist);
	mark(global_env);

	/* mark current registers */
	mark(args);
	mark(envir);
	mark(code);
	mark(dump);

	/* mark variables a, b */
	mark(a);
	mark(b);

	/* garbage collect */
	clrmark(NIL);
	fcells = 0;
	free_cell = NIL;
	for (i = 0; i <= last_cell_seg; i++) {
		for (j = 0, p = cell_seg[i]; j < CELL_SEGSIZE; j++, p++) {
			if (ismark(p))
				clrmark(p);
			else {
				type(p) = 0;
				cdr(p) = free_cell;
				car(p) = NIL;
				free_cell = p;
				++fcells;
			}
		}
	}

	if (gc_verbose)
		printf(" done %ld cells are recovered.\n", fcells);
}


/* ========== Rootines for Reading ========== */

#define TOK_LPAREN  0
#define TOK_RPAREN  1
#define TOK_DOT     2
#define TOK_ATOM    3
#define TOK_QUOTE   4
#define TOK_COMMENT 5
#define TOK_DQUOTE  6
#ifdef USE_QQUOTE
# define TOK_BQUOTE  7
# define TOK_COMMA   8
# define TOK_ATMARK  9
#endif
#define TOK_SHARP   10

#define LINESIZE 1024
char    linebuff[LINESIZE];
char    strbuff[256];
char   *currentline = linebuff;
char   *endline = linebuff;

/* get new character from input file */
int     inchar()
{
	if (currentline >= endline) {	/* input buffer is empty */
		if (feof(infp)) {
			fclose(infp);
			infp = stdin;
			if (!quiet)
				printf(prompt);
		}
		strcpy(linebuff, "\n");
		if (fgets(currentline = linebuff, LINESIZE, infp) == NULL)
			if (infp == stdin) {
				if (!quiet)
					fprintf(stderr, "Good-bye\n");
				exit(0);
			}
		endline = linebuff + strlen(linebuff);
	}
	return (*currentline++);
}

/* clear input buffer */
clearinput()
{
	currentline = endline = linebuff;
}

/* back to standard input */
flushinput()
{
	if (infp != stdin) {
		fclose(infp);
		infp = stdin;
	}
	clearinput();
}

/* back character to input buffer */
backchar()
{
	currentline--;
}

/* read chacters to delimiter */
char   *readstr(delim)
char   *delim;
{
	char   *p = strbuff;

	while (isdelim(delim, (*p++ = inchar())))
		;
	backchar();
	*--p = '\0';
	return (strbuff);
}

/* read string expression "xxx...xxx" */
char   *readstrexp()
{
	char    c, *p = strbuff;

	for (;;) {
		if ((c = inchar()) != '"')
			*p++ = c;
		else if (p > strbuff && *(p - 1) == '\\')
			*(p - 1) = '"';
		else {
			*p = '\0';
			return (strbuff);
		}
	}
}

/* check c is delimiter */
isdelim(s, c)
char   *s;
char    c;
{
	while (*s)
		if (*s++ == c)
			return (0);
	return (1);
}

/* skip white characters */
skipspace()
{
	while (isspace(inchar()))
		;
	backchar();
}

/* get token */
token()
{
	skipspace();
	switch (inchar()) {
	case '(':
		return (TOK_LPAREN);
	case ')':
		return (TOK_RPAREN);
	case '.':
		return (TOK_DOT);
	case '\'':
		return (TOK_QUOTE);
	case ';':
		return (TOK_COMMENT);
	case '"':
		return (TOK_DQUOTE);
#ifdef USE_QQUOTE
	case BACKQUOTE:
		return (TOK_BQUOTE);
	case ',':
		if (inchar() == '@')
			return (TOK_ATMARK);
		else {
			backchar();
			return (TOK_COMMA);
		}
#endif
	case '#':
		return (TOK_SHARP);
	default:
		backchar();
		return (TOK_ATOM);
	}
}

/* ========== Rootines for Printing ========== */
#define	ok_abbrev(x)	(ispair(x) && cdr(x) == NIL)

strunquote(p, s)
char *p;
char *s;
{
	*p++ = '"';
	for ( ; *s; ++s) {
		if (*s == '"') {
			*p++ = '\\';
			*p++ = '"';
		} else if (*s == '\n') {
			*p++ = '\\';
			*p++ = 'n';
		} else
			*p++ = *s;
	}
	*p++ = '"';
	*p = '\0';
}


/* print atoms */
int printatom(l, f)
pointer l;
int     f;
{
	char	*p;
	
	if (l == NIL)
		p = "()";
	else if (l == T)
		p = "#t";
	else if (l == F)
		p = "#f";
	else if (isnumber(l)) {
		p = strbuff;
		sprintf(p, "%ld", ivalue(l));
	} else if (isstring(l)) {
		if (!f)
			p = strvalue(l);
		else {
			p = strbuff;
			strunquote(p, strvalue(l));
		}
	} else if (issymbol(l))
		p = symname(l);
	else if (isproc(l)) {
		p = strbuff;
		sprintf(p, "#<PROCEDURE %ld>", procnum(l));
#ifdef USE_MACRO
	} else if (ismacro(l)) {
		p = "#<MACRO>";
#endif
	} else if (isclosure(l))
		p = "#<CLOSURE>";
	else if (iscontinuation(l))
		p = "#<CONTINUATION>";
	if (f < 0)
		return strlen(p);
	fputs(p, outfp);
	return 0;
}


/* ========== Rootines for Evaluation Cycle ========== */

/* make closure. c is code. e is environment */
pointer mk_closure(c, e)
register pointer c, e;
{
	register pointer x = get_cell(c, e);

	type(x) = T_CLOSURE;
	car(x) = c;
	cdr(x) = e;
	return (x);
}

/* make continuation. */
pointer mk_continuation(d)
register pointer d;
{
	register pointer x = get_cell(NIL, d);

	type(x) = T_CONTINUATION;
	cont_dump(x) = d;
	return (x);
}

/* reverse list -- make new cells */
pointer reverse(a)
register pointer a;		/* a must be checked by gc */
{
	register pointer p = NIL;

	for ( ; ispair(a); a = cdr(a))
		p = cons(car(a), p);
	return (p);
}

/* reverse list --- no make new cells */
pointer non_alloc_rev(term, list)
pointer term, list;
{
	register pointer p = list, result = term, q;

	while (p != NIL) {
		q = cdr(p);
		cdr(p) = result;
		result = p;
		p = q;
	}
	return (result);
}

/* append list -- make new cells */
pointer append(a, b)
register pointer a, b;
{
	register pointer p = b, q;

	if (a != NIL) {
		a = reverse(a);
		while (a != NIL) {
			q = cdr(a);
			cdr(a) = p;
			p = a;
			a = q;
		}
	}
	return (p);
}

/* equivalence of atoms */
eqv(a, b)
register pointer a, b;
{
	if (isstring(a)) {
		if (isstring(b))
			return (strvalue(a) == strvalue(b));
		else
			return (0);
	} else if (isnumber(a)) {
		if (isnumber(b))
			return (ivalue(a) == ivalue(b));
		else
			return (0);
	} else
		return (a == b);
}

/* true or false value macro */
#define istrue(p)       ((p) != NIL && (p) != F)
#define isfalse(p)      ((p) == NIL || (p) == F)

/* Error macro */
#ifdef	AVOID_HACK_LOOP
# define	BEGIN	{
# define	END	}
#else
/*
 * I believe this is better, but some compiler complains....
 */
# define	BEGIN	do {
# define	END	} while (0)
#endif

#define Error_0(s) BEGIN                       \
    args = cons(mk_string((s)), NIL);          \
    operator = (short)OP_ERR0;                 \
    return T; END

#define Error_1(s, a) BEGIN                    \
    args = cons((a), NIL);                     \
    args = cons(mk_string((s)), args);         \
    operator = (short)OP_ERR0;                 \
    return T; END

/* control macros for Eval_Cycle */
#define s_goto(a) BEGIN                        \
    operator = (short)(a);                     \
    return T; END

#define s_save(a, b, c)  (                     \
    dump = cons(envir, cons((c), dump)),       \
    dump = cons((b), dump),                    \
    dump = cons(mk_number((long)(a)), dump))   \


#define s_return(a) BEGIN                      \
    value = (a);                               \
    operator = ivalue(car(dump));              \
    args = cadr(dump);                         \
    envir = caddr(dump);                       \
    code = cadddr(dump);                       \
    dump = cddddr(dump);                       \
    return T; END

#define s_retbool(tf)	s_return((tf) ? T : F)



/* ========== Evaluation Cycle ========== */

/* operator code */
#define	OP_LOAD			0
#define	OP_T0LVL		1
#define	OP_T1LVL		2
#define	OP_READ			3
#define	OP_VALUEPRINT		4
#define	OP_EVAL			5
#define	OP_E0ARGS		6
#define	OP_E1ARGS		7
#define	OP_APPLY		8
#define	OP_DOMACRO		9

#define	OP_LAMBDA		10
#define	OP_QUOTE		11
#define	OP_DEF0			12
#define	OP_DEF1			13
#define	OP_BEGIN		14
#define	OP_IF0			15
#define	OP_IF1			16
#define	OP_SET0			17
#define	OP_SET1			18
#define	OP_LET0			19
#define	OP_LET1			20
#define	OP_LET2			21
#define	OP_LET0AST		22
#define	OP_LET1AST		23
#define	OP_LET2AST		24
#define	OP_LET0REC		25
#define	OP_LET1REC		26
#define	OP_LET2REC		27
#define	OP_COND0		28
#define	OP_COND1		29
#define	OP_DELAY		30
#define	OP_AND0			31
#define	OP_AND1			32
#define	OP_OR0			33
#define	OP_OR1			34
#define	OP_C0STREAM		35
#define	OP_C1STREAM		36
#define	OP_0MACRO		37
#define	OP_1MACRO		38
#define	OP_CASE0		39
#define	OP_CASE1		40
#define	OP_CASE2		41

#define	OP_PEVAL		42
#define	OP_PAPPLY		43
#define	OP_CONTINUATION		44
#define	OP_ADD			45
#define	OP_SUB			46
#define	OP_MUL			47
#define	OP_DIV			48
#define	OP_REM			49
#define	OP_CAR			50
#define	OP_CDR			51
#define	OP_CONS			52
#define	OP_SETCAR		53
#define	OP_SETCDR		54
#define	OP_NOT			55
#define	OP_BOOL			56
#define	OP_NULL			57
#define	OP_ZEROP		58
#define	OP_POSP			59
#define	OP_NEGP			60
#define	OP_NEQ			61
#define	OP_LESS			62
#define	OP_GRE			63
#define	OP_LEQ			64
#define	OP_GEQ			65
#define	OP_SYMBOL		66
#define	OP_NUMBER		67
#define	OP_STRING		68
#define	OP_PROC			69
#define	OP_PAIR			70
#define	OP_EQ			71
#define	OP_EQV			72
#define	OP_FORCE		73
#define	OP_WRITE		74
#define	OP_DISPLAY		75
#define	OP_NEWLINE		76
#define	OP_ERR0			77
#define	OP_ERR1			78
#define	OP_REVERSE		79
#define	OP_APPEND		80
#define	OP_PUT			81
#define	OP_GET			82
#define	OP_QUIT			83
#define	OP_GC			84
#define	OP_GCVERB		85
#define	OP_NEWSEGMENT		86

#define	OP_RDSEXPR		87
#define	OP_RDLIST		88
#define	OP_RDDOT		89
#define	OP_RDQUOTE		90
#define	OP_RDQQUOTE		91
#define	OP_RDUNQUOTE		92
#define	OP_RDUQTSP		93

#define	OP_P0LIST		94
#define	OP_P1LIST		95

#define	OP_LIST_LENGTH		96
#define	OP_ASSQ			97
#define	OP_PRINT_WIDTH		98
#define	OP_P0_WIDTH		99
#define	OP_P1_WIDTH		100
#define	OP_GET_CLOSURE		101
#define	OP_CLOSUREP		102
#define	OP_MACROP		103


static FILE *tmpfp;
static int tok;
static int print_flag;
static pointer value;
static short operator;

pointer opexe_0(op)
register short op;
{
	register pointer x, y;

	switch (op) {
	case OP_LOAD:		/* load */
		if (!isstring(car(args))) {
			Error_0("load -- argument is not string");
		}
		if ((infp = fopen(strvalue(car(args)), "r")) == NULL) {
			infp = stdin;
			Error_1("Unable to open", car(args));
		}
		if (!quiet)
			fprintf(outfp, "loading %s", strvalue(car(args)));
		s_goto(OP_T0LVL);

	case OP_T0LVL:	/* top level */
		if (!quiet)
			fprintf(outfp, "\n");
		dump = NIL;
		envir = global_env;
		s_save(OP_VALUEPRINT, NIL, NIL);
		s_save(OP_T1LVL, NIL, NIL);
		if (infp == stdin && !quiet)
			printf(prompt);
		s_goto(OP_READ);

	case OP_T1LVL:	/* top level */
		code = value;
		s_goto(OP_EVAL);
		
	case OP_READ:		/* read */
		tok = token();
		s_goto(OP_RDSEXPR);

	case OP_VALUEPRINT:	/* print evalution result */
		print_flag = 1;
		args = value;
		if (quiet) {
			s_goto(OP_T0LVL);
		} else {
			s_save(OP_T0LVL, NIL, NIL);
			s_goto(OP_P0LIST);
		}

	case OP_EVAL:		/* main part of evalution */
		if (issymbol(code)) {	/* symbol */
			for (x = envir; x != NIL; x = cdr(x)) {
				for (y = car(x); y != NIL; y = cdr(y))
					if (caar(y) == code)
						break;
				if (y != NIL)
					break;
			}
			if (x != NIL) {
				s_return(cdar(y));
			} else {
				Error_1("Unbounded variable", code);
			}
		} else if (ispair(code)) {
			if (issyntax(x = car(code))) {	/* SYNTAX */
				code = cdr(code);
				s_goto(syntaxnum(x));
			} else {/* first, eval top element and eval arguments */
#ifdef USE_MACRO
				s_save(OP_E0ARGS, NIL, code);
#else
				s_save(OP_E1ARGS, NIL, cdr(code));
#endif
				code = car(code);
				s_goto(OP_EVAL);
			}
		} else {
			s_return(code);
		}

#ifdef USE_MACRO
	case OP_E0ARGS:	/* eval arguments */
		if (ismacro(value)) {	/* macro expansion */
			s_save(OP_DOMACRO, NIL, NIL);
			args = cons(code, NIL);
			code = value;
			s_goto(OP_APPLY);
		} else {
			code = cdr(code);
			s_goto(OP_E1ARGS);
		}
#endif

	case OP_E1ARGS:	/* eval arguments */
		args = cons(value, args);
		if (ispair(code)) {	/* continue */
			s_save(OP_E1ARGS, args, cdr(code));
			code = car(code);
			args = NIL;
			s_goto(OP_EVAL);
		} else {	/* end */
			args = reverse(args);
			code = car(args);
			args = cdr(args);
			s_goto(OP_APPLY);
		}

	case OP_APPLY:		/* apply 'code' to 'args' */
		if (isproc(code)) {
			s_goto(procnum(code));	/* PROCEDURE */
		} else if (isclosure(code)) {	/* CLOSURE */
			/* make environment */
			envir = cons(NIL, closure_env(code));
			for (x = car(closure_code(code)), y = args;
			     ispair(x); x = cdr(x), y = cdr(y)) {
				if (y == NIL) {
					Error_0("Few arguments");
				} else {
					car(envir) = cons(cons(car(x), car(y)), car(envir));
				}
			}
			if (x == NIL) {
				/*--
				 * if (y != NIL) {
				 * 	Error_0("Many arguments");
				 * }
				 */
			} else if (issymbol(x))
				car(envir) = cons(cons(x, y), car(envir));
			else {
				Error_0("Syntax error in closure");
			}
			code = cdr(closure_code(code));
			args = NIL;
			s_goto(OP_BEGIN);
		} else if (iscontinuation(code)) {	/* CONTINUATION */
			dump = cont_dump(code);
			s_return(args != NIL ? car(args) : NIL);
		} else {
			Error_0("Illegal function");
		}

#ifdef USE_MACRO
	case OP_DOMACRO:	/* do macro */
		code = value;
		s_goto(OP_EVAL);
#endif

	case OP_LAMBDA:	/* lambda */
		s_return(mk_closure(code, envir));

	case OP_QUOTE:		/* quote */
		s_return(car(code));

	case OP_DEF0:	/* define */
		if (ispair(car(code))) {
			x = caar(code);
			code = cons(LAMBDA, cons(cdar(code), cdr(code)));
		} else {
			x = car(code);
			code = cadr(code);
		}
		if (!issymbol(x)) {
			Error_0("Variable is not symbol");
		}
		s_save(OP_DEF1, NIL, x);
		s_goto(OP_EVAL);

	case OP_DEF1:	/* define */
		for (x = car(envir); x != NIL; x = cdr(x))
			if (caar(x) == code)
				break;
		if (x != NIL)
			cdar(x) = value;
		else
			car(envir) = cons(cons(code, value), car(envir));
		s_return(code);

	case OP_SET0:		/* set! */
		s_save(OP_SET1, NIL, car(code));
		code = cadr(code);
		s_goto(OP_EVAL);

	case OP_SET1:		/* set! */
		for (x = envir; x != NIL; x = cdr(x)) {
			for (y = car(x); y != NIL; y = cdr(y))
				if (caar(y) == code)
					break;
			if (y != NIL)
				break;
		}
		if (x != NIL) {
			cdar(y) = value;
			s_return(value);
		} else {
			Error_1("Unbounded variable", code);
		}

	case OP_BEGIN:		/* begin */
		if (!ispair(code)) {
			s_return(code);
		}
		if (cdr(code) != NIL) {
			s_save(OP_BEGIN, NIL, cdr(code));
		}
		code = car(code);
		s_goto(OP_EVAL);

	case OP_IF0:		/* if */
		s_save(OP_IF1, NIL, cdr(code));
		code = car(code);
		s_goto(OP_EVAL);

	case OP_IF1:		/* if */
		if (istrue(value))
			code = car(code);
		else
			code = cadr(code);	/* (if #f 1) ==> () because
						 * car(NIL) = NIL */
		s_goto(OP_EVAL);

	case OP_LET0:		/* let */
		args = NIL;
		value = code;
		code = issymbol(car(code)) ? cadr(code) : car(code);
		s_goto(OP_LET1);

	case OP_LET1:		/* let (caluculate parameters) */
		args = cons(value, args);
		if (ispair(code)) {	/* continue */
			s_save(OP_LET1, args, cdr(code));
			code = cadar(code);
			args = NIL;
			s_goto(OP_EVAL);
		} else {	/* end */
			args = reverse(args);
			code = car(args);
			args = cdr(args);
			s_goto(OP_LET2);
		}

	case OP_LET2:		/* let */
		envir = cons(NIL, envir);
		for (x = issymbol(car(code)) ? cadr(code) : car(code), y = args;
		     y != NIL; x = cdr(x), y = cdr(y))
			car(envir) = cons(cons(caar(x), car(y)), car(envir));
		if (issymbol(car(code))) {	/* named let */
			for (x = cadr(code), args = NIL; x != NIL; x = cdr(x))
				args = cons(caar(x), args);
			x = mk_closure(cons(reverse(args), cddr(code)), envir);
			car(envir) = cons(cons(car(code), x), car(envir));
			code = cddr(code);
			args = NIL;
		} else {
			code = cdr(code);
			args = NIL;
		}
		s_goto(OP_BEGIN);

	case OP_LET0AST:	/* let* */
		if (car(code) == NIL) {
			envir = cons(NIL, envir);
			code = cdr(code);
			s_goto(OP_BEGIN);
		}
		s_save(OP_LET1AST, cdr(code), car(code));
		code = cadaar(code);
		s_goto(OP_EVAL);

	case OP_LET1AST:	/* let* (make new frame) */
		envir = cons(NIL, envir);
		s_goto(OP_LET2AST);

	case OP_LET2AST:	/* let* (caluculate parameters) */
		car(envir) = cons(cons(caar(code), value), car(envir));
		code = cdr(code);
		if (ispair(code)) {	/* continue */
			s_save(OP_LET2AST, args, code);
			code = cadar(code);
			args = NIL;
			s_goto(OP_EVAL);
		} else {	/* end */
			code = args;
			args = NIL;
			s_goto(OP_BEGIN);
		}
	default:
		sprintf(strbuff, "%d is illegal operator", operator);
		Error_0(strbuff);
	}
	return T;
}


pointer opexe_1(op)
register short op;
{
	register pointer x, y;

	switch (op) {
	case OP_LET0REC:	/* letrec */
		envir = cons(NIL, envir);
		args = NIL;
		value = code;
		code = car(code);
		s_goto(OP_LET1REC);

	case OP_LET1REC:	/* letrec (caluculate parameters) */
		args = cons(value, args);
		if (ispair(code)) {	/* continue */
			s_save(OP_LET1REC, args, cdr(code));
			code = cadar(code);
			args = NIL;
			s_goto(OP_EVAL);
		} else {	/* end */
			args = reverse(args);
			code = car(args);
			args = cdr(args);
			s_goto(OP_LET2REC);
		}

	case OP_LET2REC:	/* letrec */
		for (x = car(code), y = args; y != NIL; x = cdr(x), y = cdr(y))
			car(envir) = cons(cons(caar(x), car(y)), car(envir));
		code = cdr(code);
		args = NIL;
		s_goto(OP_BEGIN);

	case OP_COND0:		/* cond */
		if (!ispair(code)) {
			Error_0("Syntax error in cond");
		}
		s_save(OP_COND1, NIL, code);
		code = caar(code);
		s_goto(OP_EVAL);

	case OP_COND1:		/* cond */
		if (istrue(value)) {
			if ((code = cdar(code)) == NIL) {
				s_return(value);
			}
			s_goto(OP_BEGIN);
		} else {
			if ((code = cdr(code)) == NIL) {
				s_return(NIL);
			} else {
				s_save(OP_COND1, NIL, code);
				code = caar(code);
				s_goto(OP_EVAL);
			}
		}

	case OP_DELAY:		/* delay */
		x = mk_closure(cons(NIL, code), envir);
		setpromise(x);
		s_return(x);

	case OP_AND0:		/* and */
		if (code == NIL) {
			s_return(T);
		}
		s_save(OP_AND1, NIL, cdr(code));
		code = car(code);
		s_goto(OP_EVAL);

	case OP_AND1:		/* and */
		if (isfalse(value)) {
			s_return(value);
		} else if (code == NIL) {
			s_return(value);
		} else {
			s_save(OP_AND1, NIL, cdr(code));
			code = car(code);
			s_goto(OP_EVAL);
		}

	case OP_OR0:		/* or */
		if (code == NIL) {
			s_return(F);
		}
		s_save(OP_OR1, NIL, cdr(code));
		code = car(code);
		s_goto(OP_EVAL);

	case OP_OR1:		/* or */
		if (istrue(value)) {
			s_return(value);
		} else if (code == NIL) {
			s_return(value);
		} else {
			s_save(OP_OR1, NIL, cdr(code));
			code = car(code);
			s_goto(OP_EVAL);
		}

	case OP_C0STREAM:	/* cons-stream */
		s_save(OP_C1STREAM, NIL, cdr(code));
		code = car(code);
		s_goto(OP_EVAL);

	case OP_C1STREAM:	/* cons-stream */
		args = value;	/* save value to register args for gc */
		x = mk_closure(cons(NIL, code), envir);
		setpromise(x);
		s_return(cons(args, x));

#ifdef USE_MACRO
	case OP_0MACRO:	/* macro */
		x = car(code);
		code = cadr(code);
		if (!issymbol(x)) {
			Error_0("Variable is not symbol");
		}
		s_save(OP_1MACRO, NIL, x);
		s_goto(OP_EVAL);

	case OP_1MACRO:	/* macro */
		type(value) |= T_MACRO;
		for (x = car(envir); x != NIL; x = cdr(x))
			if (caar(x) == code)
				break;
		if (x != NIL)
			cdar(x) = value;
		else
			car(envir) = cons(cons(code, value), car(envir));
		s_return(code);
#endif

	case OP_CASE0:		/* case */
		s_save(OP_CASE1, NIL, cdr(code));
		code = car(code);
		s_goto(OP_EVAL);

	case OP_CASE1:		/* case */
		for (x = code; x != NIL; x = cdr(x)) {
			if (!ispair(y = caar(x)))
				break;
			for ( ; y != NIL; y = cdr(y))
				if (eqv(car(y), value))
					break;
			if (y != NIL)
				break;
		}
		if (x != NIL) {
			if (ispair(caar(x))) {
				code = cdar(x);
				s_goto(OP_BEGIN);
			} else {/* else */
				s_save(OP_CASE2, NIL, cdar(x));
				code = caar(x);
				s_goto(OP_EVAL);
			}
		} else {
			s_return(NIL);
		}

	case OP_CASE2:		/* case */
		if (istrue(value)) {
			s_goto(OP_BEGIN);
		} else {
			s_return(NIL);
		}
	case OP_PAPPLY:	/* apply */
		code = car(args);
		args = cadr(args);
		s_goto(OP_APPLY);

	case OP_PEVAL:	/* eval */
		code = car(args);
		args = NIL;
		s_goto(OP_EVAL);

	case OP_CONTINUATION:	/* call-with-current-continuation */
		code = car(args);
		args = cons(mk_continuation(dump), NIL);
		s_goto(OP_APPLY);

	default:
		sprintf(strbuff, "%d is illegal operator", operator);
		Error_0(strbuff);
	}
	return T;
}


pointer opexe_2(op)
register short op;
{
	register pointer x, y;
	register long v;

	switch (op) {
	case OP_ADD:		/* + */
		for (x = args, v = 0; x != NIL; x = cdr(x))
			v += ivalue(car(x));
		s_return(mk_number(v));

	case OP_SUB:		/* - */
		for (x = cdr(args), v = ivalue(car(args)); x != NIL; x = cdr(x))
			v -= ivalue(car(x));
		s_return(mk_number(v));

	case OP_MUL:		/* * */
		for (x = args, v = 1; x != NIL; x = cdr(x))
			v *= ivalue(car(x));
		s_return(mk_number(v));

	case OP_DIV:		/* / */
		for (x = cdr(args), v = ivalue(car(args)); x != NIL; x = cdr(x)) {
			if (ivalue(car(x)) != 0)
				v /= ivalue(car(x));
			else {
				Error_0("Divided by zero");
			}
		}
		s_return(mk_number(v));

	case OP_REM:		/* remainder */
		for (x = cdr(args), v = ivalue(car(args)); x != NIL; x = cdr(x)) {
			if (ivalue(car(x)) != 0)
				v %= ivalue(car(x));
			else {
				Error_0("Divided by zero");
			}
		}
		s_return(mk_number(v));

	case OP_CAR:		/* car */
		if (ispair(car(args))) {
			s_return(caar(args));
		} else {
			Error_0("Unable to car for non-cons cell");
		}

	case OP_CDR:		/* cdr */
		if (ispair(car(args))) {
			s_return(cdar(args));
		} else {
			Error_0("Unable to cdr for non-cons cell");
		}

	case OP_CONS:		/* cons */
		cdr(args) = cadr(args);
		s_return(args);

	case OP_SETCAR:	/* set-car! */
		if (ispair(car(args))) {
			caar(args) = cadr(args);
			s_return(car(args));
		} else {
			Error_0("Unable to set-car! for non-cons cell");
		}

	case OP_SETCDR:	/* set-cdr! */
		if (ispair(car(args))) {
			cdar(args) = cadr(args);
			s_return(car(args));
		} else {
			Error_0("Unable to set-cdr! for non-cons cell");
		}

	default:
		sprintf(strbuff, "%d is illegal operator", operator);
		Error_0(strbuff);
	}
	return T;
}


pointer opexe_3(op)
register short op;
{
	register pointer x, y;

	switch (op) {
	case OP_NOT:		/* not */
		s_retbool(isfalse(car(args)));
	case OP_BOOL:		/* boolean? */
		s_retbool(car(args) == F || car(args) == T);
	case OP_NULL:		/* null? */
		s_retbool(car(args) == NIL);
	case OP_ZEROP:		/* zero? */
		s_retbool(ivalue(car(args)) == 0);
	case OP_POSP:		/* positive? */
		s_retbool(ivalue(car(args)) > 0);
	case OP_NEGP:		/* negative? */
		s_retbool(ivalue(car(args)) < 0);
	case OP_NEQ:		/* = */
		s_retbool(ivalue(car(args)) == ivalue(cadr(args)));
	case OP_LESS:		/* < */
		s_retbool(ivalue(car(args)) < ivalue(cadr(args)));
	case OP_GRE:		/* > */
		s_retbool(ivalue(car(args)) > ivalue(cadr(args)));
	case OP_LEQ:		/* <= */
		s_retbool(ivalue(car(args)) <= ivalue(cadr(args)));
	case OP_GEQ:		/* >= */
		s_retbool(ivalue(car(args)) >= ivalue(cadr(args)));
	case OP_SYMBOL:	/* symbol? */
		s_retbool(issymbol(car(args)));
	case OP_NUMBER:	/* number? */
		s_retbool(isnumber(car(args)));
	case OP_STRING:	/* string? */
		s_retbool(isstring(car(args)));
	case OP_PROC:		/* procedure? */
		/*--
	         * continuation should be procedure by the example
	         * (call-with-current-continuation procedure?) ==> #t
                 * in R^3 report sec. 6.9
	         */
		s_retbool(isproc(car(args)) || isclosure(car(args))
			  || iscontinuation(car(args)));
	case OP_PAIR:		/* pair? */
		s_retbool(ispair(car(args)));
	case OP_EQ:		/* eq? */
		s_retbool(car(args) == cadr(args));
	case OP_EQV:		/* eqv? */
		s_retbool(eqv(car(args), cadr(args)));
	default:
		sprintf(strbuff, "%d is illegal operator", operator);
		Error_0(strbuff);
	}
	return T;
}


pointer opexe_4(op)
register short op;
{
	register pointer x, y;

	switch (op) {
	case OP_FORCE:		/* force */
		code = car(args);
		if (ispromise(code)) {
			args = NIL;
			s_goto(OP_APPLY);
		} else {
			s_return(code);
		}

	case OP_WRITE:		/* write */
		print_flag = 1;
		args = car(args);
		s_goto(OP_P0LIST);

	case OP_DISPLAY:	/* display */
		print_flag = 0;
		args = car(args);
		s_goto(OP_P0LIST);

	case OP_NEWLINE:	/* newline */
		fprintf(outfp, "\n");
		s_return(T);

	case OP_ERR0:	/* error */
		if (!isstring(car(args))) {
			Error_0("error -- first argument must be string");
		}
		tmpfp = outfp;
		outfp = stderr;
                if (all_errors_fatal) {
                        FatalError(strvalue(car(args)));
                }
		fprintf(outfp, "Error: ");
		fprintf(outfp, "%s", strvalue(car(args)));
		args = cdr(args);
		s_goto(OP_ERR1);

	case OP_ERR1:	/* error */
		fprintf(outfp, " ");
		if (args != NIL) {
			s_save(OP_ERR1, cdr(args), NIL);
			args = car(args);
			print_flag = 1;
			s_goto(OP_P0LIST);
		} else {
			fprintf(outfp, "\n");
			flushinput();
			outfp = tmpfp;
			s_goto(OP_T0LVL);
		}

	case OP_REVERSE:	/* reverse */
		s_return(reverse(car(args)));

	case OP_APPEND:	/* append */
		s_return(append(car(args), cadr(args)));

	case OP_PUT:		/* put */
		if (!hasprop(car(args)) || !hasprop(cadr(args))) {
			Error_0("Illegal use of put");
		}
		for (x = symprop(car(args)), y = cadr(args); x != NIL; x = cdr(x))
			if (caar(x) == y)
				break;
		if (x != NIL)
			cdar(x) = caddr(args);
		else
			symprop(car(args)) = cons(cons(y, caddr(args)),
						  symprop(car(args)));
		s_return(T);

	case OP_GET:		/* get */
		if (!hasprop(car(args)) || !hasprop(cadr(args))) {
			Error_0("Illegal use of get");
		}
		for (x = symprop(car(args)), y = cadr(args); x != NIL; x = cdr(x))
			if (caar(x) == y)
				break;
		if (x != NIL) {
			s_return(cdar(x));
		} else {
			s_return(NIL);
		}

	case OP_QUIT:		/* quit */
		return (NIL);

	case OP_GC:		/* gc */
		gc(NIL, NIL);
		s_return(T);

	case OP_GCVERB:		/* gc-verbose */
	{	int	was = gc_verbose;
		
		gc_verbose = (car(args) != F);
		s_retbool(was);
	}

	case OP_NEWSEGMENT:	/* new-segment */
		if (!isnumber(car(args))) {
			Error_0("new-segment -- argument must be number");
		}
		fprintf(outfp, "allocate %d new segments\n",
			alloc_cellseg((int) ivalue(car(args))));
		s_return(T);
	}
}


pointer opexe_5(op)
register short op;
{
	register pointer x, y;

	switch (op) {
	/* ========== reading part ========== */
	case OP_RDSEXPR:
		switch (tok) {
		case TOK_COMMENT:
			while (inchar() != '\n')
				;
			tok = token();
			s_goto(OP_RDSEXPR);
		case TOK_LPAREN:
			tok = token();
			if (tok == TOK_RPAREN) {
				s_return(NIL);
			} else if (tok == TOK_DOT) {
				Error_0("syntax error -- illegal dot expression");
			} else {
				s_save(OP_RDLIST, NIL, NIL);
				s_goto(OP_RDSEXPR);
			}
		case TOK_QUOTE:
			s_save(OP_RDQUOTE, NIL, NIL);
			tok = token();
			s_goto(OP_RDSEXPR);
#ifdef USE_QQUOTE
		case TOK_BQUOTE:
			s_save(OP_RDQQUOTE, NIL, NIL);
			tok = token();
			s_goto(OP_RDSEXPR);
		case TOK_COMMA:
			s_save(OP_RDUNQUOTE, NIL, NIL);
			tok = token();
			s_goto(OP_RDSEXPR);
		case TOK_ATMARK:
			s_save(OP_RDUQTSP, NIL, NIL);
			tok = token();
			s_goto(OP_RDSEXPR);
#endif
		case TOK_ATOM:
			s_return(mk_atom(readstr("();\t\n ")));
		case TOK_DQUOTE:
			s_return(mk_string(readstrexp()));
		case TOK_SHARP:
			if ((x = mk_const(readstr("();\t\n "))) == NIL) {
				Error_0("Undefined sharp expression");
			} else {
				s_return(x);
			}
		default:
			Error_0("syntax error -- illegal token");
		}
		break;

	case OP_RDLIST:
		args = cons(value, args);
		tok = token();
		if (tok == TOK_COMMENT) {
			while (inchar() != '\n')
				;
			tok = token();
		}
		if (tok == TOK_RPAREN) {
			s_return(non_alloc_rev(NIL, args));
		} else if (tok == TOK_DOT) {
			s_save(OP_RDDOT, args, NIL);
			tok = token();
			s_goto(OP_RDSEXPR);
		} else {
			s_save(OP_RDLIST, args, NIL);;
			s_goto(OP_RDSEXPR);
		}

	case OP_RDDOT:
		if (token() != TOK_RPAREN) {
			Error_0("syntax error -- illegal dot expression");
		} else {
			s_return(non_alloc_rev(value, args));
		}

	case OP_RDQUOTE:
		s_return(cons(QUOTE, cons(value, NIL)));

#ifdef USE_QQUOTE
	case OP_RDQQUOTE:
		s_return(cons(QQUOTE, cons(value, NIL)));

	case OP_RDUNQUOTE:
		s_return(cons(UNQUOTE, cons(value, NIL)));

	case OP_RDUQTSP:
		s_return(cons(UNQUOTESP, cons(value, NIL)));
#endif

	/* ========== printing part ========== */
	case OP_P0LIST:
		if (!ispair(args)) {
			printatom(args, print_flag);
			s_return(T);
		} else if (car(args) == QUOTE && ok_abbrev(cdr(args))) {
			fprintf(outfp, "'");
			args = cadr(args);
			s_goto(OP_P0LIST);
		} else if (car(args) == QQUOTE && ok_abbrev(cdr(args))) {
			fprintf(outfp, "`");
			args = cadr(args);
			s_goto(OP_P0LIST);
		} else if (car(args) == UNQUOTE && ok_abbrev(cdr(args))) {
			fprintf(outfp, ",");
			args = cadr(args);
			s_goto(OP_P0LIST);
		} else if (car(args) == UNQUOTESP && ok_abbrev(cdr(args))) {
			fprintf(outfp, ",@");
			args = cadr(args);
			s_goto(OP_P0LIST);
		} else {
			fprintf(outfp, "(");
			s_save(OP_P1LIST, cdr(args), NIL);
			args = car(args);
			s_goto(OP_P0LIST);
		}

	case OP_P1LIST:
		if (ispair(args)) {
			s_save(OP_P1LIST, cdr(args), NIL);
			fprintf(outfp, " ");
			args = car(args);
			s_goto(OP_P0LIST);
		} else {
			if (args != NIL) {
				fprintf(outfp, " . ");
				printatom(args, print_flag);
			}
			fprintf(outfp, ")");
			s_return(T);
		}

	default:
		sprintf(strbuff, "%d is illegal operator", operator);
		Error_0(strbuff);

	}
	return T;
}


pointer opexe_6(op)
register short op;
{
	register pointer x, y;
	register long v;
	static long	w;
	char	buffer[32];

	switch (op) {
	case OP_LIST_LENGTH:	/* list-length */	/* a.k */
		for (x = car(args), v = 0; ispair(x); x = cdr(x))
			++v;
		s_return(mk_number(v));
		
	case OP_ASSQ:		/* assq */	/* a.k */
		x = car(args);
		for (y = cadr(args); ispair(y); y = cdr(y)) {
			if (!ispair(car(y))) {
				Error_0("Unable to handle non pair element");
			}
			if (x == caar(y))
				break;
		}
		if (ispair(y)) {
			s_return(car(y));
		} else {
			s_return(F);
		}
		
	case OP_PRINT_WIDTH:	/* print-width */	/* a.k */
		w = 0;
		args = car(args);
		print_flag = -1;
		s_goto(OP_P0_WIDTH);
		
	case OP_P0_WIDTH:
		if (!ispair(args)) {
			w += printatom(args, print_flag);
			s_return(mk_number(w));
		} else if (car(args) == QUOTE
			   && ok_abbrev(cdr(args))) {
			++w;
			args = cadr(args);
			s_goto(OP_P0_WIDTH);
		} else if (car(args) == QQUOTE
			   && ok_abbrev(cdr(args))) {
			++w;
			args = cadr(args);
			s_goto(OP_P0_WIDTH);
		} else if (car(args) == UNQUOTE
			   && ok_abbrev(cdr(args))) {
			++w;
			args = cadr(args);
			s_goto(OP_P0_WIDTH);
		} else if (car(args) == UNQUOTESP
			   && ok_abbrev(cdr(args))) {
			w += 2;
			args = cadr(args);
			s_goto(OP_P0_WIDTH);
		} else {
			++w;
			s_save(OP_P1_WIDTH, cdr(args), NIL);
			args = car(args);
			s_goto(OP_P0_WIDTH);
		}
		
	case OP_P1_WIDTH:
		if (ispair(args)) {
			s_save(OP_P1_WIDTH, cdr(args), NIL);
			++w;
			args = car(args);
			s_goto(OP_P0_WIDTH);
		} else {
			if (args != NIL)
				w += 3 + printatom(args, print_flag);
			++w;
			s_return(mk_number(w));
		}
		
	case OP_GET_CLOSURE:	/* get-closure-code */	/* a.k */
		args = car(args);
		if (args == NIL) {
			s_return(F);
		} else if (isclosure(args)) {
			s_return(cons(LAMBDA, closure_code(value)));
#ifdef USE_MACRO
		} else if (ismacro(args)) {
			s_return(cons(LAMBDA, closure_code(value)));
#endif
		} else {
			s_return(F);
		}
	case OP_CLOSUREP:		/* closure? */
		/*
		 * Note, macro object is also a closure.
		 * Therefore, (closure? <#MACRO>) ==> #t
		 */
		if (car(args) == NIL) {
		    s_return(F);
		}
		s_retbool(isclosure(car(args)));
#ifdef USE_MACRO
	case OP_MACROP:		/* macro? */
		if (car(args) == NIL) {
		    s_return(F);
		}
		s_retbool(ismacro(car(args)));
#endif
	default:
		sprintf(strbuff, "%d is illegal operator", operator);
		Error_0(strbuff);
	}
	return T;	/* NOTREACHED */
}




pointer	(*dispatch_table[])() = {
	opexe_0,	/* OP_LOAD = 0, */
	opexe_0,	/* OP_T0LVL, */
	opexe_0,	/* OP_T1LVL, */
	opexe_0,	/* OP_READ, */
	opexe_0,	/* OP_VALUEPRINT, */
	opexe_0,	/* OP_EVAL, */
	opexe_0,	/* OP_E0ARGS, */
	opexe_0,	/* OP_E1ARGS, */
	opexe_0,	/* OP_APPLY, */
	opexe_0,	/* OP_DOMACRO, */
	
	opexe_0,	/* OP_LAMBDA, */
	opexe_0,	/* OP_QUOTE, */
	opexe_0,	/* OP_DEF0, */
	opexe_0,	/* OP_DEF1, */
	opexe_0,	/* OP_BEGIN, */
	opexe_0,	/* OP_IF0, */
	opexe_0,	/* OP_IF1, */
	opexe_0,	/* OP_SET0, */
	opexe_0,	/* OP_SET1, */
	opexe_0,	/* OP_LET0, */
	opexe_0,	/* OP_LET1, */
	opexe_0,	/* OP_LET2, */
	opexe_0,	/* OP_LET0AST, */
	opexe_0,	/* OP_LET1AST, */
	opexe_0,	/* OP_LET2AST, */
	
	opexe_1,	/* OP_LET0REC, */
	opexe_1,	/* OP_LET1REC, */
	opexe_1,	/* OP_LETREC2, */
	opexe_1,	/* OP_COND0, */
	opexe_1,	/* OP_COND1, */
	opexe_1,	/* OP_DELAY, */
	opexe_1,	/* OP_AND0, */
	opexe_1,	/* OP_AND1, */
	opexe_1,	/* OP_OR0, */
	opexe_1,	/* OP_OR1, */
	opexe_1,	/* OP_C0STREAM, */
	opexe_1,	/* OP_C1STREAM, */
	opexe_1,	/* OP_0MACRO, */
	opexe_1,	/* OP_1MACRO, */
	opexe_1,	/* OP_CASE0, */
	opexe_1,	/* OP_CASE1, */
	opexe_1,	/* OP_CASE2, */
	
	opexe_1,	/* OP_PEVAL, */
	opexe_1,	/* OP_PAPPLY, */
	opexe_1,	/* OP_CONTINUATION, */
	
	opexe_2,	/* OP_ADD, */
	opexe_2,	/* OP_SUB, */
	opexe_2,	/* OP_MUL, */
	opexe_2,	/* OP_DIV, */
	opexe_2,	/* OP_REM, */
	opexe_2,	/* OP_CAR, */
	opexe_2,	/* OP_CDR, */
	opexe_2,	/* OP_CONS, */
	opexe_2,	/* OP_SETCAR, */
	opexe_2,	/* OP_SETCDR, */
	
	opexe_3,	/* OP_NOT, */
	opexe_3,	/* OP_BOOL, */
	opexe_3,	/* OP_NULL, */
	opexe_3,	/* OP_ZEROP, */
	opexe_3,	/* OP_POSP, */
	opexe_3,	/* OP_NEGP, */
	opexe_3,	/* OP_NEQ, */
	opexe_3,	/* OP_LESS, */
	opexe_3,	/* OP_GRE, */
	opexe_3,	/* OP_LEQ, */
	opexe_3,	/* OP_GEQ, */
	opexe_3,	/* OP_SYMBOL, */
	opexe_3,	/* OP_NUMBER, */
	opexe_3,	/* OP_STRING, */
	opexe_3,	/* OP_PROC, */
	opexe_3,	/* OP_PAIR, */
	opexe_3,	/* OP_EQ, */
	opexe_3,	/* OP_EQV, */
	
	opexe_4,	/* OP_FORCE, */
	opexe_4,	/* OP_WRITE, */
	opexe_4,	/* OP_DISPLAY, */
	opexe_4,	/* OP_NEWLINE, */
	opexe_4,	/* OP_ERR0, */
	opexe_4,	/* OP_ERR1, */
	opexe_4,	/* OP_REVERSE, */
	opexe_4,	/* OP_APPEND, */
	opexe_4,	/* OP_PUT, */
	opexe_4,	/* OP_GET, */
	opexe_4,	/* OP_QUIT, */
	opexe_4,	/* OP_GC, */
	opexe_4,	/* OP_GCVERB, */
	opexe_4,	/* OP_NEWSEGMENT, */
	
	opexe_5,	/* OP_RDSEXPR, */
	opexe_5,	/* OP_RDLIST, */
	opexe_5,	/* OP_RDDOT, */
	opexe_5,	/* OP_RDQUOTE, */
	opexe_5,	/* OP_RDQQUOTE, */
	opexe_5,	/* OP_RDUNQUOTE, */
	opexe_5,	/* OP_RDUQTSP, */
	opexe_5,	/* OP_P0LIST, */
	opexe_5,	/* OP_P1LIST, */
	
	opexe_6,	/* OP_LIST_LENGTH, */
	opexe_6,	/* OP_ASSQ, */
	opexe_6,	/* OP_PRINT_WIDTH, */
	opexe_6,	/* OP_P0_WIDTH, */
	opexe_6,	/* OP_P1_WIDTH, */
	opexe_6,	/* OP_GET_CLOSURE, */
	opexe_6,	/* OP_CLOSUREP, */
#ifdef USE_MACRO
	opexe_6,	/* OP_MACROP, */
#endif
	
};


/* kernel of this intepreter */
pointer Eval_Cycle(op)
register short op;
{

	operator = op;
	for (;;)
		if ((*dispatch_table[operator])(operator) == NIL)
			return NIL;
}

/* ========== Initialization of internal keywords ========== */

mk_syntax(op, name)
unsigned short op;
char   *name;
{
	pointer x;

	x = cons(mk_string(name), NIL);
	type(x) = (T_SYNTAX | T_SYMBOL);
	syntaxnum(x) = op;
	oblist = cons(x, oblist);
}

mk_proc(op, name)
unsigned short op;
char   *name;
{
	pointer x, y;

	x = mk_symbol(name);
	y = get_cell(NIL, NIL);
	type(y) = (T_PROC | T_ATOM);
	ivalue(y) = (long) op;
	car(global_env) = cons(cons(x, y), car(global_env));
}


init_vars_global()
{
	pointer x;

	/* init input/output file */
	infp = stdin;
	outfp = stdout;
	/* init NIL */
	type(NIL) = (T_ATOM | MARK);
	car(NIL) = cdr(NIL) = NIL;
	/* init T */
	type(T) = (T_ATOM | MARK);
	car(T) = cdr(T) = T;
	/* init F */
	type(F) = (T_ATOM | MARK);
	car(F) = cdr(F) = F;
	/* init global_env */
	global_env = cons(NIL, NIL);
	/* init else */
	x = mk_symbol("else");
	car(global_env) = cons(cons(x, T), car(global_env));
}


init_syntax()
{
	/* init syntax */
	mk_syntax(OP_LAMBDA, "lambda");
	mk_syntax(OP_QUOTE, "quote");
	mk_syntax(OP_DEF0, "define");
	mk_syntax(OP_IF0, "if");
	mk_syntax(OP_BEGIN, "begin");
	mk_syntax(OP_SET0, "set!");
	mk_syntax(OP_LET0, "let");
	mk_syntax(OP_LET0AST, "let*");
	mk_syntax(OP_LET0REC, "letrec");
	mk_syntax(OP_COND0, "cond");
	mk_syntax(OP_DELAY, "delay");
	mk_syntax(OP_AND0, "and");
	mk_syntax(OP_OR0, "or");
	mk_syntax(OP_C0STREAM, "cons-stream");
#ifdef USE_MACRO
	mk_syntax(OP_0MACRO, "macro");
#endif
	mk_syntax(OP_CASE0, "case");
}


init_procs()
{
	/* init procedure */
	mk_proc(OP_PEVAL, "eval");
	mk_proc(OP_PAPPLY, "apply");
	mk_proc(OP_CONTINUATION, "call-with-current-continuation");
	mk_proc(OP_FORCE, "force");
	mk_proc(OP_CAR, "car");
	mk_proc(OP_CDR, "cdr");
	mk_proc(OP_CONS, "cons");
	mk_proc(OP_SETCAR, "set-car!");
	mk_proc(OP_SETCDR, "set-cdr!");
	mk_proc(OP_ADD, "+");
	mk_proc(OP_SUB, "-");
	mk_proc(OP_MUL, "*");
	mk_proc(OP_DIV, "/");
	mk_proc(OP_REM, "remainder");
	mk_proc(OP_NOT, "not");
	mk_proc(OP_BOOL, "boolean?");
	mk_proc(OP_SYMBOL, "symbol?");
	mk_proc(OP_NUMBER, "number?");
	mk_proc(OP_STRING, "string?");
	mk_proc(OP_PROC, "procedure?");
	mk_proc(OP_PAIR, "pair?");
	mk_proc(OP_EQV, "eqv?");
	mk_proc(OP_EQ, "eq?");
	mk_proc(OP_NULL, "null?");
	mk_proc(OP_ZEROP, "zero?");
	mk_proc(OP_POSP, "positive?");
	mk_proc(OP_NEGP, "negative?");
	mk_proc(OP_NEQ, "=");
	mk_proc(OP_LESS, "<");
	mk_proc(OP_GRE, ">");
	mk_proc(OP_LEQ, "<=");
	mk_proc(OP_GEQ, ">=");
	mk_proc(OP_READ, "read");
	mk_proc(OP_WRITE, "write");
	mk_proc(OP_DISPLAY, "display");
	mk_proc(OP_NEWLINE, "newline");
	mk_proc(OP_LOAD, "load");
	mk_proc(OP_ERR0, "error");
	mk_proc(OP_REVERSE, "reverse");
	mk_proc(OP_APPEND, "append");
	mk_proc(OP_PUT, "put");
	mk_proc(OP_GET, "get");
	mk_proc(OP_GC, "gc");
	mk_proc(OP_GCVERB, "gc-verbose");
	mk_proc(OP_NEWSEGMENT, "new-segment");
	mk_proc(OP_LIST_LENGTH, "list-length");	/* a.k */
	mk_proc(OP_ASSQ, "assq");	/* a.k */
	mk_proc(OP_PRINT_WIDTH, "print-width");	/* a.k */	
	mk_proc(OP_GET_CLOSURE, "get-closure-code");	/* a.k */
	mk_proc(OP_CLOSUREP, "closure?");	/* a.k */
#ifdef USE_MACRO
	mk_proc(OP_MACROP, "macro?");	/* a.k */
#endif
	mk_proc(OP_QUIT, "quit");
}


/* initialize several globals */
init_globals()
{
	init_vars_global();
	init_syntax();
	init_procs();
	/* intialization of global pointers to special symbols */
	LAMBDA = mk_symbol("lambda");
	QUOTE = mk_symbol("quote");
#ifdef USE_QQUOTE
	QQUOTE = mk_symbol("quasiquote");
	UNQUOTE = mk_symbol("unquote");
	UNQUOTESP = mk_symbol("unquote-splicing");
#endif

}

/* ========== Error ==========  */

FatalError(fmt, a, b, c)
char   *fmt, *a, *b, *c;
{
	fprintf(stderr, "Fatal error: ");
	fprintf(stderr, fmt, a, b, c);
	fprintf(stderr, "\n");
	exit(1);
}

#ifdef USE_SETJMP
Error(fmt, a, b, c)
char   *fmt, *a, *b, *c;
{
	fprintf(stderr, "Error: ");
	fprintf(stderr, fmt, a, b, c);
	fprintf(stderr, "\n");
	flushinput();
	longjmp(error_jmp, OP_T0LVL);
}

#endif

/* ========== Main ========== */

#ifdef CMDLINE
main(argc, argv)
int argc;
char **argv;
#else
main()
#endif
{
	short   i;
	short   op = (short) OP_LOAD;

#ifdef CMDLINE
	for (i = 1; i < argc; i++) {
		if (strcmp(argv[i], "-e") == 0) {
			all_errors_fatal = 1;
		} else if (strcmp(argv[i], "-q") == 0) {
			quiet = 1;
		} else {
			fclose(stdin);
			init_scheme();
			args = cons(mk_string(argv[i]), NIL);
			Eval_Cycle(op);
			exit(0);
		}
	}
#endif

	if (!quiet)
		printf(banner);
	init_scheme();
	args = cons(mk_string(InitFile), NIL);
#ifdef USE_SETJMP
	op = setjmp(error_jmp);
#endif
	Eval_Cycle(op);
	exit(0);
}

