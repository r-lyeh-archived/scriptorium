/*
 * S9 Core Toolkit
 * By Nils M Holm, 2007-2015
 * In the public domain
 */

#define VERSION "20150615"

#include "s9core.h"

#define INITIAL_SEGMENT_SIZE	32768
#define PRIM_SEG_SIZE		256
#define MAX_PORTS		32

/*
 * Global state
 */

static int	Cons_segment_size,
		Vec_segment_size;
static int	Cons_pool_size,
		Vec_pool_size;

static int	Verbose_GC = 0;

cell		*Car,
		*Cdr;
char		*Tag;

cell		*Vectors;

cell		Stack;

static cell	Free_list;
static cell	Free_vecs;

PRIM		*Primitives;
static int	Last_prim,
		Max_prims;

static cell	Tmp_car,
		Tmp_cdr,
		Tmp;

static cell	Symbols;

static int	Printer_count,
		Printer_limit;

static int	IO_error;

FILE		*Ports[MAX_PORTS];
static char	Port_flags[MAX_PORTS];

int		Input_port,
		Output_port,
		Error_port;

char		*Str_port;
int		Str_port_len;

static long     Node_limit,
		Vector_limit;

static char	*Exponent_chars;
static cell	**Image_vars;

static void	(*Mem_error_handler)(int src);

/* Predefined bignum literals */
cell	Zero,
	One,
	Two;

/* Smallest representable real number */
cell	Epsilon;

/* Internal GC roots */
static cell	*GC_int_roots[] = { &Stack, &Symbols, &Tmp, &Tmp_car,
					&Tmp_cdr, &Zero, &One, &Two,
					&Epsilon, NULL };

/* External GC roots */
static cell	**GC_ext_roots = NULL;

/*
 * Internal vector representation
 */

#define RAW_VECTOR_LINK         0
#define RAW_VECTOR_INDEX        1
#define RAW_VECTOR_SIZE         2
#define RAW_VECTOR_DATA         3

/*
 * Counting
 */

int	Run_stats, Cons_stats;

counter	Conses,
	Nodes,
	Collections;

void run_stats(int x) {
	Run_stats = x;
	if (Run_stats) {
		reset_counter(&Nodes);
		reset_counter(&Conses);
		reset_counter(&Collections);
	}
}

void cons_stats(int x) {
	Cons_stats = x;
}

void reset_counter(counter *c) {
	c->n = 0;
	c->n1k = 0;
	c->n1m = 0;
	c->n1g = 0;
	c->n1t = 0;
}

void count(counter *c) {
	c->n++;
	if (c->n >= 1000) {
		c->n -= 1000;
		c->n1k++;
		if (c->n1k >= 1000) {
			c->n1k -= 1000;
			c->n1m++;
			if (c->n1m >= 1000) {
				c->n1m -= 1000;
				c->n1g++;
				if (c->n1g >= 1000) {
					c->n1t -= 1000;
					c->n1t++;
				}
			}
		}
	}
}

cell read_counter(counter *c) {
	cell	n, m;

	n = make_integer(c->n);
	n = cons(n, NIL);
	save(n);
	m = make_integer(c->n1k);
	n = cons(m, n);
	car(Stack) = n;
	m = make_integer(c->n1m);
	n = cons(m, n);
	car(Stack) = n;
	m = make_integer(c->n1g);
	n = cons(m, n);
	car(Stack) = n;
	m = make_integer(c->n1t);
	n = cons(m, n);
	unsave(1);
	return n;
}

void get_counters(counter **nc, counter **cc, counter **gc) {
	*nc = &Nodes;
	*cc = &Conses;
	*gc = &Collections;
}

/*
 * Raw I/O
 */

void flush(void) {
	if (fflush(Ports[Output_port]))
		IO_error = 1;
}

void set_printer_limit(int k) {
	Printer_limit = k;
	Printer_count = 0;
}

int printer_limit(void) {
	return Printer_limit && Printer_count >= Printer_limit;
}

void blockwrite(char *s, int k) {
	if (Str_port) {
		if (k >= Str_port_len) {
			k = Str_port_len;
			IO_error = 1;
		}
		memcpy(Str_port, s, k);
		Str_port += k;
		Str_port_len -= k;
		*Str_port = 0;
		return;
	}
	if (Printer_limit && Printer_count > Printer_limit) {
		if (Printer_limit > 0)
			fwrite("...", 1, 3, Ports[Output_port]);
		Printer_limit = -1;
		return;
	}
	if (fwrite(s, 1, k, Ports[Output_port]) != k)
		IO_error = 1;
	if (Output_port == 1 && s[k-1] == '\n')
		flush();
	Printer_count += k;
}

int blockread(char *s, int k) {
	int	n;

	n = fread(s, 1, k, Ports[Input_port]);
	if (n < 0) IO_error = 1;
	return n;
}

void prints(char *s) {
	if (Ports[Output_port] == NULL)
		fatal("pr: output port is not open");
	blockwrite(s, strlen(s));
}

int io_status(void) {
	return IO_error? -1: 0;
}

void io_reset(void) {
	IO_error = 0;
}

/*
 * Error Handling
 */

void fatal(char *msg) {
	fprintf(stderr, "S9core: fatal error: ");
	fprintf(stderr, "%s\n", msg);
	bye(1);
}

/*
 * Memory Management
 */

void set_node_limit(int n) {
	Node_limit = n * 1024L;
}

void set_vector_limit(int n) {
	Vector_limit = n * 1024L;
}

void gc_verbosity(int n) {
	Verbose_GC = n;
}

void mem_error_handler(void (*h)(int src)) {
	Mem_error_handler = h;
}

static void new_cons_segment(void) {
	Car = realloc(Car, sizeof(cell) * (Cons_pool_size+Cons_segment_size));
	Cdr = realloc(Cdr, sizeof(cell) * (Cons_pool_size+Cons_segment_size));
	Tag = realloc(Tag, Cons_pool_size + Cons_segment_size);
	if (Car == NULL || Cdr == NULL || Tag == NULL)
		fatal("new_cons_segment: out of physical memory");
	memset(&car(Cons_pool_size), 0, Cons_segment_size * sizeof(cell));
	memset(&cdr(Cons_pool_size), 0, Cons_segment_size * sizeof(cell));
	memset(&Tag[Cons_pool_size], 0, Cons_segment_size);
	Cons_pool_size += Cons_segment_size;
	Cons_segment_size = Cons_pool_size / 2;
}

static void new_vec_segment(void) {
	Vectors = realloc(Vectors, sizeof(cell) *
			(Vec_pool_size + Vec_segment_size));
	if (Vectors == NULL)
		fatal("new_vec_segment: out of physical memory");
	memset(&Vectors[Vec_pool_size], 0, Vec_segment_size * sizeof(cell));
	Vec_pool_size += Vec_segment_size;
	Vec_segment_size = Vec_pool_size / 2;
}

/*
 * Mark nodes which can be accessed through N.
 * Using the Deutsch/Schorr/Waite pointer reversal algorithm.
 * S0: M==0, S==0, unvisited, process CAR (vectors: process 1st slot);
 * S1: M==1, S==1, CAR visited, process CDR (vectors: process next slot);
 * S2: M==1, S==0, completely visited, return to parent.
 */

static void mark(cell n) {
	cell	p, parent, *v;
	int	i;

	parent = NIL;	/* Initially, there is no parent node */
	while (1) {
		if (special_value_p(n) || Tag[n] & MARK_TAG) {
			if (parent == NIL)
				break;
			if (Tag[parent] & VECTOR_TAG) {	/* S1 --> S1|done */
				i = vector_index(parent);
				v = vector(parent);
				if (Tag[parent] & STATE_TAG &&
				    i+1 < vector_len(parent)
				) {			/* S1 --> S1 */
					p = v[i+1];
					v[i+1] = v[i];
					v[i] = n;
					n = p;
					vector_index(parent) = i+1;
				}
				else {			/* S1 --> done */
					p = parent;
					parent = v[i];
					v[i] = n;
					n = p;
				}
			}
			else if (Tag[parent] & STATE_TAG) {	/* S1 --> S2 */
				p = cdr(parent);
				cdr(parent) = car(parent);
				car(parent) = n;
				Tag[parent] &= ~STATE_TAG;
				Tag[parent] |=  MARK_TAG;
				n = p;
			}
			else {				/* S2 --> done */
				p = parent;
				parent = cdr(p);
				cdr(p) = n;
				n = p;
			}
		}
		else {
			if (Tag[n] & VECTOR_TAG) {	/* S0 --> S1|S2 */
				Tag[n] |= MARK_TAG;
				/* Tag[n] &= ~STATE_TAG; */
				vector_link(n) = n;
				if (car(n) == T_VECTOR && vector_len(n) != 0) {
					Tag[n] |= STATE_TAG;
					vector_index(n) = 0;
					v = vector(n);
					p = v[0];
					v[0] = parent;
					parent = n;
					n = p;
				}
			}
			else if (Tag[n] & ATOM_TAG) {	/* S0 --> S2 */
				if (input_port_p(n) || output_port_p(n))
					Port_flags[port_no(n)] |= USED_TAG;
				p = cdr(n);
				cdr(n) = parent;
				/*Tag[n] &= ~STATE_TAG;*/
				parent = n;
				n = p;
				Tag[parent] |= MARK_TAG;
			}
			else {				/* S0 --> S1 */
				p = car(n);
				car(n) = parent;
				Tag[n] |= MARK_TAG;
				parent = n;
				n = p;
				Tag[parent] |= STATE_TAG;
			}
		}
	}
}

/* Mark and sweep GC. */
int gc(void) {
	int	i, k;
	char	buf[100];

	if (Run_stats)
		count(&Collections);
	for (i=0; i<MAX_PORTS; i++)
		if (Port_flags[i] & LOCK_TAG)
			Port_flags[i] |= USED_TAG;
		else
			Port_flags[i] &= ~USED_TAG;
	for (i=0; GC_int_roots[i] != NULL; i++)
		mark(GC_int_roots[i][0]);
	if (GC_ext_roots)
		for (i=0; GC_ext_roots[i] != NULL; i++)
			mark(GC_ext_roots[i][0]);
	k = 0;
	Free_list = NIL;
	for (i=0; i<Cons_pool_size; i++) {
		if (!(Tag[i] & MARK_TAG)) {
			cdr(i) = Free_list;
			Free_list = i;
			k++;
		}
		else {
			Tag[i] &= ~MARK_TAG;
		}
	}
	for (i=0; i<MAX_PORTS; i++) {
		if (!(Port_flags[i] & USED_TAG) && Ports[i] != NULL) {
			fclose(Ports[i]);
			Ports[i] = NULL;
		}
	}
	if (Verbose_GC > 1) {
		sprintf(buf, "GC: %d nodes reclaimed", k);
		prints(buf); nl();
	}
	return k;
}

/* Allocate a fresh node and initialize with PCAR,PCDR,PTAG. */
cell cons3(cell pcar, cell pcdr, int ptag) {
	cell	n;
	int	k;
	char	buf[100];

	if (Run_stats) {
		count(&Nodes);
		if (Cons_stats)
			count(&Conses);
	}
	if (Free_list == NIL) {
		if (ptag == 0)
			Tmp_car = pcar;
		if (!(ptag & VECTOR_TAG))
			Tmp_cdr = pcdr;
		k = gc();
		/*
		 * Performance increases dramatically if we
		 * do not wait for the pool to run dry.
		 * In fact, don't even let it come close to that.
		 */
		if (k < Cons_pool_size / 2) {
			if (	Node_limit &&
				Cons_pool_size + Cons_segment_size
					> Node_limit
			) {
				if (Mem_error_handler)
					(*Mem_error_handler)(1);
				else
					fatal("cons3: hit memory limit");
			}
			else {
				new_cons_segment();
				if (Verbose_GC) {
					sprintf(buf,
						"GC: new segment,"
						 " nodes = %d,"
						 " next segment = %d",
						Cons_pool_size,
						Cons_segment_size);
					prints(buf); nl();
				}
				gc();
			}
		}
		Tmp_car = Tmp_cdr = NIL;
	}
	if (Free_list == NIL)
		fatal("cons3: failed to recover from low memory condition");
	n = Free_list;
	Free_list = cdr(Free_list);
	car(n) = pcar;
	cdr(n) = pcdr;
	Tag[n] = ptag;
	return n;
}

/* Mark all vectors unused */
static void unmark_vectors(void) {
	int	p, k, link;

	p = 0;
	while (p < Free_vecs) {
		link = p;
		k = Vectors[p + RAW_VECTOR_SIZE];
		p += vector_size(k);
		Vectors[link] = NIL;
	}
}

/* In situ vector pool garbage collection and compaction */
int gcv(void) {
	int	v, k, to, from;
	char	buf[100];

	unmark_vectors();
	gc();		/* re-mark live vectors */
	to = from = 0;
	while (from < Free_vecs) {
		v = Vectors[from + RAW_VECTOR_SIZE];
		k = vector_size(v);
		if (Vectors[from + RAW_VECTOR_LINK] != NIL) {
			if (to != from) {
				memmove(&Vectors[to], &Vectors[from],
					k * sizeof(cell));
				cdr(Vectors[to + RAW_VECTOR_LINK]) =
					to + RAW_VECTOR_DATA;
			}
			to += k;
		}
		from += k;
	}
	k = Free_vecs - to;
	if (Verbose_GC > 1) {
		sprintf(buf, "GC: gcv: %d cells reclaimed", k);
		prints(buf); nl();
	}
	Free_vecs = to;
	return k;
}

/* Allocate vector from pool */
cell new_vec(cell type, int size) {
	cell	n;
	int	v, wsize;
	char	buf[100];

	wsize = vector_size(size);
	if (Free_vecs + wsize >= Vec_pool_size) {
		gcv();
		while (	Free_vecs + wsize >=
			Vec_pool_size - Vec_pool_size / 2
		) {
			if (	Vector_limit &&
				Vec_pool_size + Vec_segment_size
					> Vector_limit
			) {
				if (Mem_error_handler)
					(*Mem_error_handler)(2);
				else
					fatal("new_vec: hit memory limit");
				break;
			}
			else {
				new_vec_segment();
				gcv();
				if (Verbose_GC) {
					sprintf(buf,
						"GC: new_vec: new segment,"
						 " cells = %d",
						Vec_pool_size);
					prints(buf); nl();
				}
			}
		}
	}
	if (Free_vecs + wsize >= Vec_pool_size)
		fatal("new_vec: failed to recover from low memory condition");
	v = Free_vecs;
	Free_vecs += wsize;
	n = cons3(type, v + RAW_VECTOR_DATA, VECTOR_TAG);
	Vectors[v + RAW_VECTOR_LINK] = n;
	Vectors[v + RAW_VECTOR_INDEX] = 0;
	Vectors[v + RAW_VECTOR_SIZE] = size;
	return n;
}

/* Pop K nodes off the Stack, return last one. */
cell unsave(int k) {
	cell	n = NIL; /*LINT*/

	while (k) {
		if (Stack == NIL)
			fatal("unsave: stack underflow");
		n = car(Stack);
		Stack = cdr(Stack);
		k--;
	}
	return n;
}

cell find_symbol(char *s) {
	cell	y;

	y = Symbols;
	while (y != NIL) {
		if (!strcmp(symbol_name(car(y)), s))
			return car(y);
		y = cdr(y);
	}
	return NIL;
}

cell make_symbol(char *s, int k) {
	cell	n;

	n = new_vec(T_SYMBOL, k+1);
	strcpy(symbol_name(n), s);
	return n;
}

cell intern_symbol(cell y) {
	Symbols = cons(y, Symbols);
	return y;
}

cell symbol_table(void) {
	return Symbols;
}

cell symbol_ref(char *s) {
	cell	y, new;

	y = find_symbol(s);
	if (y != NIL)
		return y;
	new = make_symbol(s, strlen(s));
	return intern_symbol(new);
}

cell make_string(char *s, int k) {
	cell	n;

	n = new_vec(T_STRING, k+1);
	strncpy(string(n), s, k+1);
	return n;
}

cell make_vector(int k) {
	cell	n, *v;
	int	i;

	k *= sizeof(cell);
	n = new_vec(T_VECTOR, k);
	v = vector(n);
	for (i=0; i<k; i++)
		v[i] = UNDEFINED;
	return n;
}

cell make_integer(cell i) {
	cell	n;

	n = new_atom(i, NIL);
	return new_atom(T_INTEGER, n);
}

cell make_char(int x) {
	cell n;

	n = new_atom(x & 0xff, NIL);
	return new_atom(T_CHAR, n);
}

static cell real_normalize(cell x);

cell Make_quick_real(int flags, cell exp, cell mant) {
	cell	n;

	n = new_atom(exp, mant);
	n = new_atom(flags, n);
	n = new_atom(T_REAL, n);
	return n;
}

cell Make_real(int flags, cell exp, cell mant) {
	cell	r;

	save(mant);
	r = Make_quick_real(flags, exp, mant);
	r = real_normalize(r);
	unsave(1);
	return r;
}

cell make_real(int sign, cell exp, cell mant) {
	cell	m;
	int	i;

	i = 0;
	for (m = cdr(mant); m != NIL; m = cdr(m))
		i++;
	if (i > MANTISSA_SIZE)
		return UNDEFINED;
	return Make_real(sign < 0? REAL_NEGATIVE: 0, exp, cdr(mant));
}

static void grow_primitives(void) {
	Max_prims += PRIM_SEG_SIZE;
	Primitives = (PRIM *) realloc(Primitives, sizeof(PRIM) * Max_prims);
	if (Primitives == NULL)
		fatal("grow_primitives: out of physical memory");
}

cell make_primitive(PRIM *p) {
	cell	n;

	n = new_atom(Last_prim, NIL);
	n = new_atom(T_PRIMITIVE, n);
	if (Last_prim >= Max_prims)
		grow_primitives();
	memcpy(&Primitives[Last_prim], p, sizeof(PRIM));
	Last_prim++;
	return n;
}

cell make_port(int portno, cell type) {
	cell	n;
	int	pf;

	pf = Port_flags[portno];
	Port_flags[portno] |= LOCK_TAG;
	n = new_atom(portno, NIL);
	n = cons3(type, n, ATOM_TAG|PORT_TAG);
	Port_flags[portno] = pf;
	return n;
}

cell string_to_symbol(cell x) {
	cell	y, n, k;

	y = find_symbol(string(x));
	if (y != NIL)
		return y;
	/*
	 * Cannot pass content to make_symbol(), because
	 * string(x) may move during GC.
	 */
	k = string_len(x);
	n = make_symbol("", k-1);
	memcpy(symbol_name(n), string(x), k);
	return intern_symbol(n);
}

cell symbol_to_string(cell x) {
	cell	n, k;

	/*
	 * Cannot pass name to make_string(), because
	 * symbol_name(x) may move during GC.
	 */
 	k = symbol_len(x);
	n = make_string("", k-1);
	memcpy(string(n), symbol_name(x), k);
	return n;
}

cell copy_string(cell x) {
	cell	n, k;

	/*
	 * See string_to_symbol(), above.
	 */
 	k = string_len(x);
	n = make_string("", k-1);
	memcpy(string(n), string(x), k);
	return n;
}

/*
 * Miscellanea
 */

int length(cell n) {
	int	k;

	for (k = 0; n != NIL; n = cdr(n))
		k++;
	return k;
}

cell flat_copy(cell n, cell *lastp) {
	cell	a, m, last, new;

	if (n == NIL) {
		if (lastp != NULL)
			lastp[0] = NIL;
		return NIL;
	}
	m = cons3(NIL, NIL, Tag[n]);
	save(m);
	a = m;
	last = m;
	while (n != NIL) {
		car(a) = car(n);
		last = a;
		n = cdr(n);
		if (n != NIL) {
			new = cons3(NIL, NIL, Tag[n]);
			cdr(a) = new;
			a = cdr(a);
		}
	}
	unsave(1);
	if (lastp != NULL)
		lastp[0] = last;
	return m;
}

long asctol(char *s) {
	while (*s == '0' && s[1])
		s++;
	return atol(s);
}

static char *ntoa(char *b, cell x, int w) {
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
/*
 * Bignums
 */

cell bignum_abs(cell a) {
	cell	n;

	save(a);
	n = new_atom(labs(cadr(a)), cddr(a));
	n = new_atom(T_INTEGER, n);
	unsave(1);
	return n;
}

cell bignum_negate(cell a) {
	cell	n;

	save(a);
	n = new_atom(-cadr(a), cddr(a));
	n = new_atom(T_INTEGER, n);
	unsave(1);
	return n;
}

static cell reverse_segments(cell n) {
	cell	m;

	m = NIL;
	while (n != NIL) {
		m = new_atom(car(n), m);
		n = cdr(n);
	}
	return m;
}

int bignum_even_p(cell a) {
	while (cdr(a) != NIL)
		a = cdr(a);
	return car(a) % 2 == 0;
}

cell bignum_add(cell a, cell b);
cell bignum_subtract(cell a, cell b);

static cell Bignum_add(cell a, cell b) {
	cell	fa, fb, result, r;
	int	carry;

	if (bignum_negative_p(a)) {
		if (bignum_negative_p(b)) {
			/* -A+-B --> -(|A|+|B|) */
			a = bignum_abs(a);
			save(a);
			a = bignum_add(a, bignum_abs(b));
			unsave(1);
			return bignum_negate(a);
		}
		else {
			/* -A+B --> B-|A| */
			return bignum_subtract(b, bignum_abs(a));
		}
	}
	else if (bignum_negative_p(b)) {
		/* A+-B --> A-|B| */
		return bignum_subtract(a, bignum_abs(b));
	}
	/* A+B */
	a = reverse_segments(cdr(a));
	save(a);
	b = reverse_segments(cdr(b));
	save(b);
	carry = 0;
	result = NIL;
	save(result);
	while (a != NIL || b != NIL || carry) {
		fa = a == NIL? 0: car(a);
		fb = b == NIL? 0: car(b);
		r = fa + fb + carry;
		carry = 0;
		if (r >= INT_SEG_LIMIT) {
			r -= INT_SEG_LIMIT;
			carry = 1;
		}
		result = new_atom(r, result);
		car(Stack) = result;
		if (a != NIL) a = cdr(a);
		if (b != NIL) b = cdr(b);
	}
	unsave(3);
	return new_atom(T_INTEGER, result);
}

cell bignum_add(cell a, cell b) {
	Tmp = b;
	save(a);
	save(b);
	Tmp = NIL;
	a = Bignum_add(a, b);
	unsave(2);
	return a;
}

int bignum_less_p(cell a, cell b) {
	int	ka, kb, neg_a, neg_b;

	neg_a = bignum_negative_p(a);
	neg_b = bignum_negative_p(b);
	if (neg_a && !neg_b) return 1;
	if (!neg_a && neg_b) return 0;
	ka = length(a);
	kb = length(b);
	if (ka < kb) return neg_a? 0: 1;
	if (ka > kb) return neg_a? 1: 0;
	Tmp = b;
	a = bignum_abs(a);
	save(a);
	b = bignum_abs(b);
	unsave(1);
	Tmp = NIL;
	a = cdr(a);
	b = cdr(b);
	while (a != NIL) {
		if (car(a) < car(b)) return neg_a? 0: 1;
		if (car(a) > car(b)) return neg_a? 1: 0;
		a = cdr(a);
		b = cdr(b);
	}
	return 0;
}

int bignum_equal_p(cell a, cell b) {
	a = cdr(a);
	b = cdr(b);
	while (a != NIL && b != NIL) {
		if (car(a) != car(b))
			return 0;
		a = cdr(a);
		b = cdr(b);
	}
	return a == NIL && b == NIL;
}

static cell Bignum_subtract(cell a, cell b) {
	cell	fa, fb, result, r;
	int	borrow;

	if (bignum_negative_p(a)) {
		if (bignum_negative_p(b)) {
			/* -A--B --> -A+|B| --> |B|-|A| */
			a = bignum_abs(a);
			save(a);
			a = bignum_subtract(bignum_abs(b), a);
			unsave(1);
			return a;
		}
		else {
			/* -A-B --> -(|A|+B) */
			return bignum_negate(bignum_add(bignum_abs(a), b));
		}
	}
	else if (bignum_negative_p(b)) {
		/* A--B --> A+|B| */
		return bignum_add(a, bignum_abs(b));
	}
	/* A-B, A<B --> -(B-A) */
	if (bignum_less_p(a, b))
		return bignum_negate(bignum_subtract(b, a));
	/* A-B, A>=B */
	a = reverse_segments(cdr(a));
	save(a);
	b = reverse_segments(cdr(b));
	save(b);
	borrow = 0;
	result = NIL;
	save(result);
	while (a != NIL || b != NIL || borrow) {
		fa = a == NIL? 0: car(a);
		fb = b == NIL? 0: car(b);
		r = fa - fb - borrow;
		borrow = 0;
		if (r < 0) {
			r += INT_SEG_LIMIT;
			borrow = 1;
		}
		result = new_atom(r, result);
		car(Stack) = result;
		if (a != NIL) a = cdr(a);
		if (b != NIL) b = cdr(b);
	}
	unsave(3);
	while (car(result) == 0 && cdr(result) != NIL)
		result = cdr(result);
	return new_atom(T_INTEGER, result);
}

cell bignum_subtract(cell a, cell b) {
	Tmp = b;
	save(a);
	save(b);
	Tmp = NIL;
	a = Bignum_subtract(a, b);
	unsave(2);
	return a;
}

cell bignum_shift_left(cell a, int fill) {
	cell	r, c, result;
	int	carry;

	save(a);
	a = reverse_segments(cdr(a));
	save(a);
	carry = fill;
	result = NIL;
	save(result);
	while (a != NIL) {
		if (car(a) >= INT_SEG_LIMIT/10) {
			c = car(a) / (INT_SEG_LIMIT/10);
			r = car(a) % (INT_SEG_LIMIT/10) * 10;
			r += carry;
			carry = c;
		}
		else {
			r = car(a) * 10 + carry;
			carry = 0;
		}
		result = new_atom(r, result);
		car(Stack) = result;
		a = cdr(a);
	}
	if (carry)
		result = new_atom(carry, result);
	result = new_atom(T_INTEGER, result);
	unsave(3);
	return result;
}

/* Result: (a/10 . a%10) */
cell bignum_shift_right(cell a) {
	cell	r, c, result;
	int	carry;

	save(a);
	a = cdr(a);
	save(a);
	carry = 0;
	result = NIL;
	save(result);
	while (a != NIL) {
		c = car(a) % 10;
		r = car(a) / 10;
		r += carry * (INT_SEG_LIMIT/10);
		carry = c;
		result = new_atom(r, result);
		car(Stack) = result;
		a = cdr(a);
	}
	result = reverse_segments(result);
	if (car(result) == 0 && cdr(result) != NIL)
		result = cdr(result);
	result = new_atom(T_INTEGER, result);
	car(Stack) = result;
	carry = make_integer(carry);
	result = cons(result, carry);
	unsave(3);
	return result;
}

cell bignum_multiply(cell a, cell b) {
	int	neg;
	cell	r, i, result;

	Tmp = b;
	save(a);
	save(b);
	Tmp = NIL;
	neg = bignum_negative_p(a) != bignum_negative_p(b);
	a = bignum_abs(a);
	save(a);
	b = bignum_abs(b);
	save(b);
	result = Zero;
	save(result);
	while (!bignum_zero_p(a)) {
		r = bignum_shift_right(a);
		i = caddr(r);
		a = car(r);
		caddr(Stack) = a;
		while (i) {
			result = bignum_add(result, b);
			car(Stack) = result;
			i--;
		}
		b = bignum_shift_left(b, 0);
		cadr(Stack) = b;
	}
	if (neg)
		result = bignum_negate(result);
	unsave(5);
	return result;
}

/*
 * Equalize A and B, e.g.:
 * A=123, B=12345 ---> 12300, 100
 * Return (scaled-a . scaling-factor)
 */
static cell bignum_equalize(cell a, cell b) {
	cell	r, f, r0, f0;

	r0 = a;
	save(r0);
	f0 = One;
	save(f0);
	r = r0;
	save(r);
	f = f0;
	save(f);
	while (bignum_less_p(r, b)) {
		cadddr(Stack) = r0 = r;
		caddr(Stack) = f0 = f;
		r = bignum_shift_left(r, 0);
		cadr(Stack) = r;
		f = bignum_shift_left(f, 0);
		car(Stack) = f;
	}
	unsave(4);
	return cons(r0, f0);
}

/* Result: (a/b . a%b) */
static cell Bignum_divide(cell a, cell b) {
	int	neg, neg_a;
	cell	result, f;
	int	i;
	cell	c, c0;

	neg_a = bignum_negative_p(a);
	neg = neg_a != bignum_negative_p(b);
	a = bignum_abs(a);
	save(a);
	b = bignum_abs(b);
	save(b);
	if (bignum_less_p(a, b)) {
		if (neg_a)
			a = bignum_negate(a);
		unsave(2);
		return cons(Zero, a);
	}
	b = bignum_equalize(b, a);
	cadr(Stack) = b; /* cadr+cddddr */
	car(Stack) = a;	/* car+cddddr */
	c = NIL;
	save(c);	/* cadddr */
	c0 = NIL;
	save(c0);	/* caddr */
	f = cdr(b);
	b = car(b);
	cadddr(Stack) = b;
	save(f);	/* cadr */
	result = Zero;
	save(result);	/* car */
	while (!bignum_zero_p(f)) {
		c = Zero;
		cadddr(Stack) = c;
		caddr(Stack) = c0 = c;
		i = 0;
		while (!bignum_less_p(a, c)) {
			caddr(Stack) = c0 = c;
			c = bignum_add(c, b);
			cadddr(Stack) = c;
			i++;
		}
		result = bignum_shift_left(result, i-1);
		car(Stack) = result;
		a = bignum_subtract(a, c0);
		car(cddddr(Stack)) = a;
		f = bignum_shift_right(f);
		f = car(f);
		cadr(Stack) = f;
		b = bignum_shift_right(b);
		b = car(b);
		cadr(cddddr(Stack)) = b;
	}
	if (neg)
		result = bignum_negate(result);
	car(Stack) = result;
	if (neg_a)
		a = bignum_negate(a);
	unsave(6);
	return cons(result, a);
}

cell bignum_divide(cell a, cell b) {
	if (bignum_zero_p(b))
		return UNDEFINED;
	Tmp = b;
	save(a);
	save(b);
	Tmp = NIL;
	a = Bignum_divide(a, b);
	unsave(2);
	return a;
}

/*
 * Real Number Arithmetics
 */

static cell count_digits(cell m) {
	int	k = 0;
	cell	x;

	x = car(m);
	k = 0;
	while (x != 0) {
		x /= 10;
		k++;
	}
	k = k==0? 1: k;
	m = cdr(m);
	while (m != NIL) {
		k += DIGITS_PER_CELL;
		m = cdr(m);
	}
	return k;
}

cell real_exponent(cell x) {
	if (integer_p(x))
		return Zero;
	return Real_exponent(x);
}

cell real_mantissa(cell x) {
	cell	m;

	if (integer_p(x))
		return x;
	m = new_atom(T_INTEGER, Real_mantissa(x));
	if (Real_negative_p(x))
		m = bignum_negate(m);
	return m;
}

/*
 * Remove trailing zeros and move the decimal
 * point to the END of the mantissa, e.g.:
 * real_normalize(1.234e0) --> 1234e-3
 *
 * Limit the mantissa to MANTISSA_SEGMENTS
 * machine words. This may cause a loss of
 * precision.
 *
 * Also handle numeric overflow/underflow.
 */

static cell real_normalize(cell x) {
	cell	m, e, r;
	int	dgs;

	save(x);
	e = Real_exponent(x);
	m = new_atom(T_INTEGER, Real_mantissa(x));
	save(m);
	dgs = count_digits(cdr(m));
	while (dgs > MANTISSA_SIZE) {
		r = bignum_shift_right(m);
		m = car(r);
		car(Stack) = m;
		dgs--;
		e++;
	}
	while (!bignum_zero_p(m)) {
		r = bignum_shift_right(m);
		if (!bignum_zero_p(cdr(r)))
			break;
		m = car(r);
		car(Stack) = m;
		e++;
	}
	if (bignum_zero_p(m))
		e = 0;
	r = new_atom(e, NIL);
	if (count_digits(r) > DIGITS_PER_CELL) {
		unsave(2);
		return UNDEFINED;
	}
	r = Make_quick_real(Real_flags(x), e, cdr(m));
	unsave(2);
	return r;
}

cell bignum_to_real(cell a) {
	int	e, flags, d;
	cell	m, n;

	save(a);
	m = flat_copy(a, NULL);
	cadr(m) = labs(cadr(m));
	e = 0;
	if (length(cdr(m)) > MANTISSA_SEGMENTS) {
		d = count_digits(cdr(m));
		while (d > MANTISSA_SIZE) {
			m = bignum_shift_right(m);
			m = car(m);
			e++;
			d--;
		}
	}
	flags = bignum_negative_p(a)? REAL_NEGATIVE: 0;
	n = Make_quick_real(flags, e, cdr(m));
	n = real_normalize(n);
	unsave(1);
	return n;
}

cell real_negate(cell a) {
	if (integer_p(a))
		return bignum_negate(a);
	Tmp = a;
	a = Real_negate(a);
	Tmp = NIL;
	return a;
}

cell real_negative_p(cell a) {
	if (integer_p(a))
		return bignum_negative_p(a);
	return Real_negative_p(a);
}

cell real_positive_p(cell a) {
	if (integer_p(a))
		return bignum_positive_p(a);
	return Real_positive_p(a);
}

cell real_zero_p(cell a) {
	if (integer_p(a))
		return bignum_zero_p(a);
	return Real_zero_p(a);
}

cell real_abs(cell a) {
	if (integer_p(a))
		return bignum_abs(a);
	if (Real_negative_p(a)) {
		Tmp = a;
		a = Real_negate(a);
		Tmp = NIL;
		return a;
	}
	return a;
}

int real_equal_p(cell a, cell b) {
	cell	ma, mb;

	if (integer_p(a) && integer_p(b))
		return bignum_equal_p(a, b);
	Tmp = b;
	save(a);
	save(b);
	Tmp = NIL;
	if (integer_p(a))
		a = bignum_to_real(a);
	if (integer_p(b)) {
		save(a);
		b = bignum_to_real(b);
		unsave(1);
	}
	unsave(2);
	if (Real_exponent(a) != Real_exponent(b))
		return 0;
	if (Real_zero_p(a) && Real_zero_p(b))
		return 1;
	if (Real_negative_p(a) != Real_negative_p(b))
		return 0;
	ma = Real_mantissa(a);
	mb = Real_mantissa(b);
	while (ma != NIL && mb != NIL) {
		if (car(ma) != car(mb))
			return 0;
		ma = cdr(ma);
		mb = cdr(mb);
	}
	if (ma != mb)
		return 0;
	return 1;
}

/*
 * Scale the number R so that it gets exponent DESIRED_E
 * without changing its value. When there is not enough
 * room for scaling the mantissa of R, return UNDEFINED.
 * E.g.: scale_mantissa(1.0e0, -2, 0) --> 100.0e-2
 *
 * Allow the mantissa to grow to MAX_SIZE segments.
 */

static cell scale_mantissa(cell r, cell desired_e, int max_size) {
	int	dgs;
	cell	n, e;

	dgs = count_digits(Real_mantissa(r));
	if (max_size && (max_size - dgs < Real_exponent(r) - desired_e))
		return UNDEFINED;
	n = new_atom(T_INTEGER, flat_copy(Real_mantissa(r), NULL));
	save(n);
	e = Real_exponent(r);
	while (e > desired_e) {
		n = bignum_shift_left(n, 0);
		car(Stack) = n;
		e--;
	}
	unsave(1);
	return Make_quick_real(Real_flags(r), e, cdr(n));
}

static void autoscale(cell *pa, cell *pb) {
	if (Real_exponent(*pa) < Real_exponent(*pb)) {
		*pb = scale_mantissa(*pb, Real_exponent(*pa),
					MANTISSA_SIZE*2);
		return;
	}
	if (Real_exponent(*pa) > Real_exponent(*pb)) {
		*pa = scale_mantissa(*pa, Real_exponent(*pb),
					MANTISSA_SIZE*2);
	}
}

int real_less_p(cell a, cell b) {
	cell	ma, mb;
	int	ka, kb, neg;
	int	dpa, dpb;

	if (integer_p(a) && integer_p(b))
		return bignum_less_p(a, b);
	Tmp = b;
	save(a);
	save(b);
	Tmp = NIL;
	if (integer_p(a))
		a = bignum_to_real(a);
	if (integer_p(b)) {
		save(a);
		b = bignum_to_real(b);
		unsave(1);
	}
	unsave(2);
	if (Real_negative_p(a) && !Real_negative_p(b)) return 1;
	if (Real_negative_p(b) && !Real_negative_p(a)) return 0;
	if (Real_zero_p(a) && Real_positive_p(b)) return 1;
	if (Real_zero_p(b) && Real_positive_p(a)) return 0;
	neg = Real_negative_p(a);
	dpa = count_digits(Real_mantissa(a)) + Real_exponent(a);
	dpb = count_digits(Real_mantissa(b)) + Real_exponent(b);
	if (dpa < dpb) return neg? 0: 1;
	if (dpa > dpb) return neg? 1: 0;
	Tmp = b;
	save(a);
	save(b);
	Tmp = NIL;
	autoscale(&a, &b);
	unsave(2);
	if (a == UNDEFINED) return neg? 1: 0;
	if (b == UNDEFINED) return neg? 0: 1;
	ma = Real_mantissa(a);
	mb = Real_mantissa(b);
	ka = length(ma);
	kb = length(mb);
	if (ka < kb) return 1;
	if (ka > kb) return 0;
	while (ma != NIL) {
		if (car(ma) < car(mb)) return neg? 0: 1;
		if (car(ma) > car(mb)) return neg? 1: 0;
		ma = cdr(ma);
		mb = cdr(mb);
	}
	return 0;
}

cell real_add(cell a, cell b) {
	cell	r, m, e, aa, ab;
	int	flags, nega, negb;

	if (integer_p(a) && integer_p(b))
		return bignum_add(a, b);
	Tmp = b;
	save(a);
	save(b);
	Tmp = NIL;
	if (integer_p(a))
		a = bignum_to_real(a);
	save(a);
	if (integer_p(b))
		b = bignum_to_real(b);
	save(b);
	if (Real_zero_p(a)) {
		unsave(4);
		return b;
	}
	if (Real_zero_p(b)) {
		unsave(4);
		return a;
	}
	autoscale(&a, &b);
	if (a == UNDEFINED || b == UNDEFINED) {
		ab = real_abs(car(Stack));
		save(ab);
		aa = real_abs(caddr(Stack));
		unsave(1);
		b = unsave(1);
		a = unsave(1);
		unsave(2);
		return real_less_p(aa, ab)? b: a;
	}
	cadr(Stack) = a;
	car(Stack) = b;
	e = Real_exponent(a);
	nega = Real_negative_p(a);
	negb = Real_negative_p(b);
	a = new_atom(T_INTEGER, Real_mantissa(a));
	if (nega)
		a = bignum_negate(a);
	cadr(Stack) = a;
	b = new_atom(T_INTEGER, Real_mantissa(b));
	if (negb)
		b = bignum_negate(b);
	car(Stack) = b;
	m = bignum_add(a, b);
	flags = bignum_negative_p(m)? REAL_NEGATIVE: 0;
	r = bignum_abs(m);
	r = Make_quick_real(flags, e, cdr(r));
	r = real_normalize(r);
	unsave(4);
	return r;
}

cell real_subtract(cell a, cell b) {
	cell	r;

	Tmp = b;
	save(a);
	save(b);
	Tmp = NIL;
	if (integer_p(b))
		b = bignum_negate(b);
	else
		b = Real_negate(b);
	save(b);
	r = real_add(a, b);
	unsave(3);
	return r;
}

cell real_multiply(cell a, cell b) {
	cell	r, m, e, ma, mb, ea, eb, neg;

	if (integer_p(a) && integer_p(b))
		return bignum_multiply(a, b);
	Tmp = b;
	save(a);
	save(b);
	Tmp = NIL;
	if (integer_p(a))
		a = bignum_to_real(a);
	save(a);
	if (integer_p(b))
		b = bignum_to_real(b);
	save(b);
	neg = Real_negative_flag(a) != Real_negative_flag(b);
	ea = Real_exponent(a);
	eb = Real_exponent(b);
	ma = new_atom(T_INTEGER, Real_mantissa(a));
	cadr(Stack) = ma;
	mb = new_atom(T_INTEGER, Real_mantissa(b));
	car(Stack) = mb;
	e = ea + eb;
	m = bignum_multiply(ma, mb);
	r = Make_quick_real(neg? REAL_NEGATIVE: 0, e, cdr(m));
	r = real_normalize(r);
	unsave(4);
	return r;
}

cell real_divide(cell a, cell b) {
	cell	r, m, e, ma, mb, ea, eb, neg, div2;
	int	nd, dd;

	Tmp = b;
	save(a);
	save(b);
	Tmp = NIL;
	if (integer_p(a))
		a = bignum_to_real(a);
	save(a);
	if (integer_p(b))
		b = bignum_to_real(b);
	save(b);
	if (Real_zero_p(b)) {
		unsave(4);
		return UNDEFINED;
	}
	if (Real_zero_p(a)) {
		r = Make_quick_real(0, 0, cdr(Zero));
		unsave(4);
		return r;
	}
	neg = Real_negative_flag(a) != Real_negative_flag(b);
	ea = Real_exponent(a);
	eb = Real_exponent(b);
	ma = new_atom(T_INTEGER, Real_mantissa(a));
	cadr(Stack) = ma;
	mb = new_atom(T_INTEGER, Real_mantissa(b));
	car(Stack) = mb;
	if (bignum_zero_p(mb)) {
		unsave(4);
		return UNDEFINED;
	}
	nd = count_digits(cdr(ma));
	dd = MANTISSA_SIZE + count_digits(cdr(mb));
	while (nd < dd) {
		ma = bignum_shift_left(ma, 0);
		cadr(Stack) = ma;
		nd++;
		ea--;
	}
	e = ea - eb;
	m = bignum_divide(ma, mb);
	save(m);
	div2 = bignum_abs(mb);
	div2 = bignum_divide(div2, Two);
	div2 = car(div2);
	if (bignum_less_p(div2, cdr(m))) {
		m = bignum_add(car(m), One);
	}
	else {
		m = car(m);
	}
	r = Make_quick_real(neg? REAL_NEGATIVE: 0, e, cdr(m));
	r = real_normalize(r);
	unsave(5);
	return r;
}

/* type: 0=trunc, 1=floor, 2=ceil */
cell rround(cell x, int type) {
	cell	n, m, e;

	e = Real_exponent(x);
	if (e >= 0)
		return x;
	save(x);
	m = new_atom(T_INTEGER, Real_mantissa(x));
	save(m);
	while (e < 0) {
		m = bignum_shift_right(m);
		m = car(m);
		car(Stack) = m;
		e++;
	}
	if (	(type == 1 && Real_negative_p(x)) ||
		(type == 2 && Real_positive_p(x))
	) {
		m = bignum_add(m, One);
	}
	n = Make_real(Real_flags(x), e, cdr(m));
	unsave(2);
	return n;
}

cell real_trunc(cell x) { return rround(x, 0); }
cell real_floor(cell x) { return rround(x, 1); }
cell real_ceil (cell x) { return rround(x, 2); }

cell real_to_bignum(cell r) {
	cell	n;
	int	neg;

	if (Real_exponent(r) >= 0) {
		save(r);
		neg = Real_negative_p(r);
		n = scale_mantissa(r, 0, 0);
		if (n == UNDEFINED) {
			unsave(1);
			return UNDEFINED;
		}
		n = new_atom(T_INTEGER, Real_mantissa(n));
		if (neg)
			n = bignum_negate(n);
		unsave(1);
		return n;
	}
	return UNDEFINED;
}

cell real_integer_p(cell x) {
	if (integer_p(x))
		return 1;
	if (real_p(x) && real_to_bignum(x) != UNDEFINED)
		return 1;
	return 0;
}

/*
 * String/number conversion
 */

static int exponent_char_p(int c) {
	return c && strchr(Exponent_chars, c) != NULL;
}

int integer_string_p(char *s) {
	if (*s == '-' || *s == '+')
		s++;
	if (!*s)
		return 0;
	while (isdigit(*s))
		s++;
	return *s == 0;
}

int string_numeric_p(char *s) {
	int	i;
	int	got_point = 0,
		got_digit = 0;

	i = 0;
	if (s[0] == '+' || s[0] == '-')
		i = 1;
	if (!s[i])
		return 0;
	while (s[i]) {
		if (isdigit(s[i])) {
			got_digit = 1;
			i++;
		}
		else if (s[i] == '.' && !got_point) {
			got_point = 1;
			i++;
		}
		else {
			break;
		}
	}
	if (!got_digit)
		return 0;
	if (s[i] && strchr(Exponent_chars, s[i]))
		return integer_string_p(&s[i+1]);
	return s[i] == 0;
}

cell string_to_bignum(char *s) {
	cell	n, v, str;
	int	k, j, sign;

	str = make_string(s, strlen(s));
	s = string(str);
	save(str);
	sign = 1;
	if (s[0] == '-') {
		s++;
		sign = -1;
	}
	else if (s[0] == '+') {
		s++;
	}
	k = (int) strlen(s);
	n = NIL;
	while (k) {
		j = k <= DIGITS_PER_CELL? k: DIGITS_PER_CELL;
		v = asctol(&s[k-j]);
		s[k-j] = 0;
		k -= j;
		if (k == 0)
			v *= sign;
		n = new_atom(v, n);
	}
	unsave(1);
	return new_atom(T_INTEGER, n);
}

cell string_to_real(char *s) {
	cell	mantissa, n;
	cell	exponent;
	int	found_dp;
	int	neg = 0;
	int	i, j, v;

	mantissa = Zero;
	save(mantissa);
	exponent = 0;
	i = 0;
	if (s[i] == '+') {
		i++;
	}
	else if (s[i] == '-') {
		neg = 1;
		i++;
	}
	found_dp = 0;
	while (isdigit((int) s[i]) || s[i] == '#' || s[i] == '.') {
		if (s[i] == '.') {
			i++;
			found_dp = 1;
			continue;
		}
		if (found_dp)
			exponent--;
		mantissa = bignum_shift_left(mantissa, 0);
		car(Stack) = mantissa;
		if (s[i] == '#')
			v = 5;
		else
			v = s[i]-'0';
		mantissa = bignum_add(mantissa, make_integer(v));
		car(Stack) = mantissa;
		i++;
	}
	j = 0;
	for (n = cdr(mantissa); n != NIL; n = cdr(n))
		j++;
	if (exponent_char_p(s[i])) {
		i++;
		if (!isdigit(s[i]) && s[i] != '-' && s[i] != '+') {
			unsave(1);
			return UNDEFINED;
		}
		n = string_to_bignum(&s[i]);
		if (cddr(n) != NIL) {
			unsave(1);
			return UNDEFINED;
		}
		exponent += cadr(n);
	}
	unsave(1);
	n = Make_quick_real((neg? REAL_NEGATIVE: 0),
			exponent, cdr(mantissa));
	return real_normalize(n);
}

cell string_to_number(char *s) {
	if (integer_string_p(s))
		return string_to_bignum(s);
	else
		return string_to_real(s);
}

void print_bignum(cell n) {
	int	first;
	char	buf[DIGITS_PER_CELL+2];

	n = cdr(n);
	first = 1;
	while (n != NIL) {
		prints(ntoa(buf, car(n), first? 0: DIGITS_PER_CELL));
		n = cdr(n);
		first = 0;
	}
}

void print_expanded_real(cell n) {
	char	buf[DIGITS_PER_CELL+3];
	int	k, first;
	int	dp_offset, old_offset;
	cell	m, e;
	int	n_digits, neg;

	m = Real_mantissa(n);
	e = Real_exponent(n);
	neg = Real_negative_p(n);
	n_digits = count_digits(m);
	dp_offset = e+n_digits;
	if (neg)
		prints("-");
	if (dp_offset <= 0)
		prints("0");
	if (dp_offset < 0)
		prints(".");
	while (dp_offset < 0) {
		prints("0");
		dp_offset++;
	}
	dp_offset = e+n_digits;
	first = 1;
	while (m != NIL) {
		ntoa(buf, labs(car(m)), first? 0: DIGITS_PER_CELL);
		k = strlen(buf);
		old_offset = dp_offset;
		dp_offset -= k;
		if (dp_offset < 0 && old_offset >= 0) {
			memmove(&buf[k+dp_offset+1], &buf[k+dp_offset],
				-dp_offset+1);
			buf[k+dp_offset] = '.';
		}
		prints(buf);
		m = cdr(m);
		first = 0;
	}
	if (dp_offset >= 0) {
		while (dp_offset > 0) {
			prints("0");
			dp_offset--;
		}
		prints(".0");
	}
}

void print_sci_real(cell n) {
	int	n_digits;
	cell	m, e;
	char	buf[DIGITS_PER_CELL+2];
	char	es[2];

	m = Real_mantissa(n);
	e = Real_exponent(n);
	n_digits = count_digits(m);
	if (Real_negative_flag(n))
		prints("-");
	ntoa(buf, car(m), 0);
	blockwrite(buf, 1);
	prints(".");
	prints(buf[1] || cdr(m) != NIL? &buf[1]: "0");
	m = cdr(m);
	while (m != NIL) {
		prints(ntoa(buf, car(m), DIGITS_PER_CELL));
		m = cdr(m);
	}
	es[0] = Exponent_chars[0];
	es[1] = 0;
	prints(es);
	if (e+n_digits-1 >= 0)
		prints("+");
	prints(ntoa(buf, e+n_digits-1, 0));
}

void print_real(cell n) {
	int	n_digits;
	cell	m, e;

	m = Real_mantissa(n);
	e = Real_exponent(n);
	n_digits = count_digits(m);
	if (e+n_digits > -4 && e+n_digits <= 6) {
		print_expanded_real(n);
		return;
	}
	print_sci_real(n);
}

cell bignum_to_int(cell x) {
	if (cddr(x) != NIL)
		return UNDEFINED;
	return cadr(x);
}

cell bignum_to_string(cell x) {
	int	n;
	cell	s;

	n = count_digits(cdr(x));
	s = make_string("", n);
	Str_port = string(s);
	Str_port_len = n+1;
	print_bignum(x);
	Str_port = NULL;
	Str_port_len = 0;
	if (IO_error)
		return UNDEFINED;
	return s;
}

cell real_to_string(cell x, int mode) {
	#define Z MANTISSA_SIZE+DIGITS_PER_CELL+10
	char	buf[Z];

	Str_port = buf;
	Str_port_len = Z;
	switch (mode) {
	case 0:	print_real(x); break;
	case 1:	print_sci_real(x); break;
	case 2:	print_expanded_real(x); break;
	default:
		Str_port = NULL;
		Str_port_len = 0;
		return UNDEFINED;
		break;
	}
	Str_port = NULL;
	Str_port_len = 0;
	if (IO_error)
		return UNDEFINED;
	return make_string(buf, strlen(buf));
}

/*
 * I/O
 */

void close_port(int port) {
	if (port < 0 || port >= MAX_PORTS)
		return;
	if (Ports[port] == NULL) {
		Port_flags[port] = 0;
		return;
	}
	fclose(Ports[port]); /* already closed? don't care */
	Ports[port] = NULL;
	Port_flags[port] = 0;
}

int new_port(void) {
	int	i, tries;

	for (tries=0; tries<2; tries++) {
		for (i=0; i<MAX_PORTS; i++) {
			if (Ports[i] == NULL)
				return i;
		}
		if (tries == 0)
			gc();
	}
	return -1;
}

int open_input_port(char *path) {
	int	i = new_port();

	if (i < 0)
		return -1;
	Ports[i] = fopen(path, "r");
	if (Ports[i] == NULL)
		return -1;
	return i;
}

int open_output_port(char *path, int append) {
	int	i = new_port();

	if (i < 0)
		return -1;
	Ports[i] = fopen(path, append? "a": "w");
	if (Ports[i] == NULL)
		return -1;
	return i;
}

cell input_port(void) {
	return Input_port;
}

cell output_port(void) {
	return Output_port;
}

cell set_input_port(cell port) {
	cell	p = Input_port;

	Input_port = port;
	return p;
}

cell set_output_port(cell port) {
	cell	p = Output_port;

	Output_port = port;
	return p;
}

void reset_std_ports(void) {
	clearerr(stdin);
	clearerr(stdout);
	clearerr(stderr);
	Input_port = 0;
	Output_port = 1;
	Error_port = 2;
}

int lock_port(cell port) {
	if (port < 0 || port >= MAX_PORTS)
		return -1;
	Port_flags[port] |= LOCK_TAG;
	return 0;
}

int unlock_port(cell port) {
	if (port < 0 || port >= MAX_PORTS)
		return -1;
	Port_flags[port] &= ~LOCK_TAG;
	return 0;
}

/*
 * Primitives
 */

static char *expected(int n, cell who, char *what, cell got) {
	static char	msg[100];
	PRIM		*p;

	p = &Primitives[cadr(who)];
	sprintf(msg, "%s: expected %s in argument #%d", p->name, what, n);
	return msg;
}

static char *wrongargs(char *name, char *what) {
	static char	buf[100];

	sprintf(buf, "%s: too %s arguments", name, what);
	return buf;
}

char *typecheck(cell f, cell a) {
	PRIM	*p;
	int	k, na, i;

	p = prim_info(f);
	k = length(a);
	if (k < p->min_args)
		return wrongargs(p->name, "few");
	if (k > p->max_args && p->max_args >= 0)
		return wrongargs(p->name, "many");
	na = p->max_args < 0? p->min_args: p->max_args;
	if (na > k)
		na = k;
	for (i=1; i<=na; i++) {
		switch (p->arg_types[i-1]) {
		case T_ANY:
			break;
		case T_BOOLEAN:
			if (!boolean_p(car(a)))
				return expected(i, f, "boolean", car(a));
			break;
		case T_CHAR:
			if (!char_p(car(a)))
				return expected(i, f, "char", car(a));
			break;
		case T_INPUT_PORT:
			if (!input_port_p(car(a)))
				return expected(i, f, "input-port", car(a));
			break;
		case T_INTEGER:
			if (!integer_p(car(a)))
				return expected(i, f, "integer", car(a));
			break;
		case T_OUTPUT_PORT:
			if (!output_port_p(car(a)))
				return expected(i, f, "output-port", car(a));
			break;
		case T_PAIR:
			if (atom_p(car(a)))
				return expected(i, f, "pair", car(a));
			break;
		case T_LIST:
			if (car(a) != NIL && atom_p(car(a)))
				return expected(i, f, "list", car(a));
			break;
		case T_FUNCTION:
			if (	!function_p(car(a)) &&
				!primitive_p(car(a)) &&
				!continuation_p(car(a))
			)
				return expected(i, f, "function", car(a));
			break;
		case T_REAL:
			if (!integer_p(car(a)) && !real_p(car(a)))
				return expected(i, f, "number", car(a));
			break;
		case T_STRING:
			if (!string_p(car(a)))
				return expected(i, f, "string", car(a));
			break;
		case T_SYMBOL:
			if (!symbol_p(car(a)))
				return expected(i, f, "symbol", car(a));
			break;
		case T_VECTOR:
			if (!vector_p(car(a)))
				return expected(i, f, "vector", car(a));
			break;
		}
		a = cdr(a);
	}
	return NULL;
}

cell apply_prim(cell f, cell a) {
	PRIM	*p;

	p = prim_info(f);
	return (*p->handler)(a);
}

/*
 * Image I/O
 */

struct magic {
	char	id[16];			/* "magic#"	*/
	char	version[8];		/* "yyyymmdd"	*/
	char	cell_size[1];		/* size + '0'	*/
	char    mantissa_size[1];	/* size + '0'	*/
	char	byte_order[8];		/* e.g. "4321"	*/
	char	prim_slots[8];		/* see code	*/
	char	pad[6];
};

static char *xfwrite(void *buf, int siz, int n, FILE *f) {
	if (fwrite(buf, siz, n, f) != n) {
		return "image file write error";
	}
	return NULL;
}

char *dump_image(char *path, char *magic) {
	FILE		*f;
	cell		n, **v;
	int		i;
	struct magic	m;
	char		*s;

	f = fopen(path, "wb");
	if (f == NULL) {
		return "cannot create image file";
	}
	memset(&m, '_', sizeof(m));
	strncpy(m.id, magic, sizeof(m.id));
	strncpy(m.version, VERSION, sizeof(m.version));
	m.cell_size[0] = sizeof(cell)+'0';
	m.mantissa_size[0] = MANTISSA_SEGMENTS+'0';
#ifdef BITS_PER_WORD_64
	n = 0x3132333435363738L;
#else
	n = 0x31323334L;
#endif
	memcpy(m.byte_order, &n, sizeof(n)>8? 8: sizeof(n));
	n = Last_prim;
	memcpy(m.prim_slots, &n, sizeof(n)>8? 8: sizeof(n));
	if ((s = xfwrite(&m, sizeof(m), 1, f)) != NULL) {
		fclose(f);
		return s;
	}
	i = Cons_pool_size;
	if ((s = xfwrite(&i, sizeof(int), 1, f)) != NULL) {
		fclose(f);
		return s;
	}
	i = Vec_pool_size;
	if ((s = xfwrite(&i, sizeof(int), 1, f)) != NULL) {
		fclose(f);
		return s;
	}
	if (	(s = xfwrite(&Free_list, sizeof(cell), 1, f)) != NULL ||
		(s = xfwrite(&Free_vecs, sizeof(cell), 1, f)) != NULL ||
		(s = xfwrite(&Symbols, sizeof(cell), 1, f)) != NULL
	) {
		fclose(f);
		return s;
	}
	i = 0;
	v = Image_vars;
	while (v[i]) {
		if ((s = xfwrite(v[i], sizeof(cell), 1, f)) != NULL) {
			fclose(f);
			return s;
		}
		i++;
	}
	if (	fwrite(Car, 1, Cons_pool_size*sizeof(cell), f)
		 != Cons_pool_size*sizeof(cell) ||
		fwrite(Cdr, 1, Cons_pool_size*sizeof(cell), f)
		 != Cons_pool_size*sizeof(cell) ||
		fwrite(Tag, 1, Cons_pool_size, f) != Cons_pool_size ||
		fwrite(Vectors, 1, Vec_pool_size*sizeof(cell), f)
		 != Vec_pool_size*sizeof(cell)
	) {
		fclose(f);
		return "image dump failed";
	}
	fclose(f);
	return NULL;
}

static char *xfread(void *buf, int siz, int n, FILE *f) {
	if (fread(buf, siz, n, f) != n) {
		return "image file read error";
	}
	return NULL;
}

char *load_image(char *path, char *magic) {
	FILE		*f;
	cell		n, **v;
	int		i;
	struct magic	m;
	int		image_nodes, image_vcells;
	char		*s;

	f = fopen(path, "rb");
	if (f == NULL)
		return "could not open file";
	if ((s = xfread(&m, sizeof(m), 1, f)) != NULL)
		return s;
	if (memcmp(m.id, magic, 2)) {
		fclose(f);
		return "magic match failed";
	}
	if (memcmp(m.version, VERSION, sizeof(m.version))) {
		fclose(f);
		return "wrong image version";
	}
	if (m.cell_size[0]-'0' != sizeof(cell)) {
		fclose(f);
		return "wrong cell size";
	}
	if (m.mantissa_size[0]-'0' != MANTISSA_SEGMENTS) {
		fclose(f);
		return "wrong mantissa size";
	}
	memcpy(&n, m.byte_order, sizeof(cell));
#ifdef BITS_PER_WORD_64
	if (n != 0x3132333435363738L) {
#else
	if (n != 0x31323334L) {
#endif
		fclose(f);
		return "wrong byte order";
	}
	memcpy(&n, m.prim_slots, sizeof(cell));
	if (n != Last_prim) {
		fclose(f);
		return "wrong number of primitives";
	}
	memset(Tag, 0, Cons_pool_size);
	if ((s = xfread(&image_nodes, sizeof(int), 1, f)) != NULL)
		return s;
	if ((s = xfread(&image_vcells, sizeof(int), 1, f)) != NULL)
		return s;
	while (image_nodes > Cons_pool_size) {
		if (	Node_limit &&
			Cons_pool_size + Cons_segment_size > Node_limit
		) {
			fclose(f);
			return "image cons pool too large";
		}
		new_cons_segment();
	}
	while (image_vcells > Vec_pool_size) {
		if (	Vector_limit &&
			Vec_pool_size + Vec_segment_size > Vector_limit
		) {
			fclose(f);
			return "image vector pool too large";
		}
		new_vec_segment();
	}
	if (	(s = xfread(&Free_list, sizeof(cell), 1, f)) != NULL ||
		(s = xfread(&Free_vecs, sizeof(cell), 1, f)) != NULL ||
		(s = xfread(&Symbols, sizeof(cell), 1, f)) != NULL
	) {
		fclose(f);
		return s;
	}
	v = Image_vars;
	i = 0;
	while (v[i]) {
		if ((s = xfread(v[i], sizeof(cell), 1, f)) != NULL)
			return s;
		i++;
	}
	if (	(fread(Car, 1, image_nodes*sizeof(cell), f)
		  != image_nodes*sizeof(cell) ||
		 fread(Cdr, 1, image_nodes*sizeof(cell), f)
		  != image_nodes*sizeof(cell) ||
		 fread(Tag, 1, image_nodes, f) != image_nodes ||
		 fread(Vectors, 1, image_vcells*sizeof(cell), f)
		  != image_vcells*sizeof(cell) ||
		 fgetc(f) != EOF)
	) {
		fclose(f);
		return "wrong file size";
	}
	fclose(f);
	return NULL;
}

/*
 * Initialization
 */

void exponent_chars(char *s) {
	Exponent_chars = s;
}

void image_vars(cell **v) {
	Image_vars = v;
}

static void resetpools(void) {
	Cons_segment_size = INITIAL_SEGMENT_SIZE;
	Vec_segment_size = INITIAL_SEGMENT_SIZE;
	Cons_pool_size = 0,
	Vec_pool_size = 0;
	Car = NULL,
	Cdr = NULL;
	Tag = NULL;
	Free_list = NIL;
	Vectors = NULL;
	Free_vecs = 0;
	Primitives = NULL;
	Max_prims = 0;
}

void s9init(cell **extroots) {
	int	i;

	GC_ext_roots = extroots;
	for (i=2; i<MAX_PORTS; i++)
		Ports[i] = NULL;
	Ports[0] = stdin;
	Ports[1] = stdout;
	Ports[2] = stderr;
	Port_flags[0] = LOCK_TAG;
	Port_flags[1] = LOCK_TAG;
	Port_flags[2] = LOCK_TAG;
	Input_port = 0;
	Output_port = 1;
	Error_port = 2;
	Str_port = NULL;
	Str_port_len = 0;
	resetpools();
	Node_limit = NODE_LIMIT * 1024L;
	Vector_limit = VECTOR_LIMIT * 1024L;
	Stack = NIL,
	Tmp_car = NIL;
	Tmp_cdr = NIL;
	Tmp = NIL;
	Symbols = NIL;
	Printer_limit = 0;
	IO_error = 0;
	Exponent_chars = "eE";
	Run_stats = 0;
	Cons_stats = 0;
	Mem_error_handler = NULL;
	new_cons_segment();
	new_vec_segment();
	gc();
	Zero = make_integer(0);
	One = make_integer(1);
	Two = make_integer(2);
	Epsilon = Make_quick_real(0, -MANTISSA_SIZE+1, cdr(One));
}

void s9fini() {
	int	i;

	for (i=2; i<MAX_PORTS; i++) {
		if (Ports[i] != NULL)
			fclose(Ports[i]);
		Ports[i] = NULL;
	}
	if (Car) free(Car);
	if (Cdr) free(Cdr);
	if (Tag) free(Tag);
	if (Vectors) free(Vectors);
	if (Primitives) free(Primitives);
	resetpools();
}

/***********************************************************************
			Test cases below
***********************************************************************/

#ifdef TEST
volatile int	Mem_err = 0;
int		Errors = 0;

#define TESTFILE	"__testfile__"

cell pcons(cell x) {
	return cons(car(x), cadr(x));
}

cell	A, B, N;

cell	*Roots[] = { &A, &B, &N, NULL };

PRIM P = { "cons", pcons, 2, 2, { T_ANY, T_LIST, T_ANY } };

void mem_error(int src) {
	Mem_err = src;
}

void error(char *s) {
	printf("Failed: %s\n", s);
	Errors++;
}

void test_types(void) {
	cell	n, m;

	if (!eof_p(END_OF_FILE)) error("eof_p()");
	if (!undefined_p(UNDEFINED)) error("undefined_p()");
	if (!unspecific_p(UNSPECIFIC)) error("unspecific_p()");
	if (!special_value_p(USER_SPECIALS)) error("special_value_p()");
	if (!boolean_p(TRUE)) error("boolean_p(TRUE)");
	if (!boolean_p(FALSE)) error("boolean_p(FALSE)");
	n = make_char('x');
	if (!char_p(n)) error("char_p()");
	if (char_value(n) != 'x') error("char_value()");
	n = make_port(0, T_INPUT_PORT);
	if (!input_port_p(n)) error("input_port_p()");
	if (port_no(n) != 0) error("port_no(0)");
	n = make_integer(12345);
	if (!integer_p(n)) error("integer_p()");
	if (bignum_to_int(n) != 12345) error("bignum_to_int()");
	n = cons(One, NIL);
	n = cons(Zero, n);
	if (!pair_p(n)) error("pair_p(1)");
	if (!pair_p(cdr(n))) error("pair_p(1)");
	if (car(n) != Zero) error("list/1");
	if (cadr(n) != One) error("list/2");
	if (cddr(n) != NIL) error("list/NIL");
	n = make_port(1, T_OUTPUT_PORT);
	if (!output_port_p(n)) error("output_port_p()");
	if (port_no(n) != 1) error("port_no(1)");
	n = make_primitive(&P);
	if (!primitive_p(n)) error("primitive_p()");
	if (prim_slot(n) != 0) error("prim_slot()");
	if (strcmp(prim_info(n)->name, "cons")) error("prim_info()");
	if (!function_p(new_atom(T_FUNCTION, NIL))) error("function_p()");
	n = make_real(1, -5, make_integer(12345));
	if (!real_p(n)) error("real_p()");
	if (real_exponent(n) != -5) error("real_exponent()");
	if (bignum_to_int(real_mantissa(n)) != 12345)
		error("real_mantissa()");
	n = make_string("hello, world!", 13);
	if (!string_p(n)) error("string_p()");
	if (strcmp(string(n), "hello, world!")) error("string()");
	if (string_len(n) != 14) error("string_len()");
	n = symbol_ref("foobarbaz");
	if (!symbol_p(n)) error("symbol_p()");
	if (strcmp(symbol_name(n), "foobarbaz")) error("symbol_name()");
	if (symbol_len(n) != 10) error("symbol_len()");
	if (symbol_ref("foobarbaz") != n) error("symbol_ref()");
	if (!syntax_p(new_atom(T_SYNTAX, NIL))) error("syntax_p()");
	n = make_vector(100);
	vector(n)[0] = Zero;
	vector(n)[99] = One;
	if (!vector_p(n)) error("vector_p()");
	if (vector(n)[0] != Zero) error("vector(0)");
	if (vector(n)[99] != One) error("vector(99)");
	if (vector_len(n) != 100) error("vector_len()");
	if (!continuation_p(new_atom(T_CONTINUATION, NIL)))
		error("continuation_p()");
	n = make_string("foo", 3);
	m = copy_string(n);
	if (strcmp(string(n), string(m))) error("copy_string()");
	if (!atom_p(new_atom(0, NIL))) error("atom_p()");
	n = new_vec(T_STRING, 100);
	if (!string_p(n)) error("new_vec(1)");
	if (string_len(n) != 100) error("new_vec(2)");
	save(One);
	save(Two);
	if (unsave(1) != Two) error("save(2)");
	if (unsave(1) != One) error("save(1)");
	if (!constant_p(cons3(NIL, NIL, CONST_TAG))) error("constant_p()");
	n = make_primitive(&P);
	save(n);
	m = cons(NIL, NIL);
	if (typecheck(n, m) == NULL) error("typecheck(1)");
	m = cons(One, NIL);
	m = cons(NIL, m);
	m = cons(Zero, m);
	if (typecheck(n, m) == NULL) error("typecheck(2)");
	m = cons(One, NIL);
	m = cons(Zero, m);
	if (typecheck(n, m) == NULL) error("typecheck(3)");
	m = cons(NIL, NIL);
	m = cons(Zero, m);
	if (typecheck(n, m) != NULL) error("typecheck(4)");
	n = apply_prim(n, m);
	unsave(1);
	if (car(n) != Zero) error("apply_prim(1)");
	if (cdr(n) != NIL) error("apply_prim(2)");
	if (find_symbol("new-symbol") != NIL) error("find_symbol(1)");
	n = make_symbol("new-symbol", 10);
	if (find_symbol("new-symbol") != NIL) error("find_symbol(2)");
	intern_symbol(n);
	if (find_symbol("new-symbol") == NIL) error("find_symbol(3)");
	m = symbol_to_string(n);
	if (!string_p(m) || strcmp(string(m), "new-symbol"))
		error("symbol_to_string()");
	if (string_to_symbol(m) != n) error("string_to_symbol(1)");
	string_to_symbol(make_string("xxyyzz", 6));
	if (find_symbol("xxyyzz") == NIL) error("string_to_symbol(2)");
}

void test_bignum(void) {
	cell	n;

	if (bignum_to_int(Zero) != 0) error("Zero");
	if (bignum_to_int(One) != 1) error("One");
	if (bignum_to_int(Two) != 2) error("Two");
	n = make_integer(-123);
	if (bignum_to_int(bignum_abs(n)) != 123) error("bignum_abs()");
	A = make_integer(1235);
	B = make_integer(5678);
	if (bignum_to_int(bignum_add(A, B)) != 6913) error("bignum_add()");
	N = bignum_divide(B, A);
	if (bignum_to_int(car(N)) != 4) error("bignum_divide(1)");
	if (bignum_to_int(cdr(N)) != 738) error("bignum_divide(2)");
	if (bignum_equal_p(A, B)) error("bignum_equal_p(1)");
	if (!bignum_equal_p(A, A)) error("bignum_equal_p(2)");
	if (bignum_even_p(A)) error("bignum_even_p(1)");
	if (!bignum_even_p(B)) error("bignum_even_p(2)");
	if (!bignum_less_p(A, B)) error("bignum_less_p(1)");
	if (bignum_less_p(B, A)) error("bignum_less_p(2)");
	if (bignum_less_p(B, B)) error("bignum_less_p(3)");
	N = make_integer(123);
	if (bignum_to_int(bignum_multiply(N, N)) != 15129)
		error("bignum_multiply()");
	if (bignum_to_int(bignum_negate(A)) != -1235) error("bignum_negate()");
	N = bignum_shift_left(N, 7);
	if (bignum_to_int(N) != 1237) error("bignum_shift_left()");
	N = bignum_shift_right(A);
	if (bignum_to_int(car(N)) != 123) error("bignum_shift_right(1)");
	if (bignum_to_int(cdr(N)) != 5) error("bignum_shift_right(2)");
	if (bignum_to_int(bignum_subtract(A, B)) != -4443)
		error("bignum_subtract()");
	N = bignum_to_string(A);
	if (!string_p(N) || strcmp(string(N), "1235"))
		error("bignum_to_string()");
}

cell mant(cell x) {
	return bignum_to_int(real_mantissa(x));
}

cell result(cell r, cell xe, cell xm) {
	int	m = mant(r);

	return xe == real_exponent(r) && m == xm;
}

void test_real(void) {
	if (!real_p(Epsilon)) error("Epsilon");
	N = make_real(1, 2, make_integer(314));
	if (Real_exponent(N) != 2) error("Real_exponent()");
	A = bignum_to_int(new_atom(T_INTEGER, Real_mantissa(N)));
	if (A != 314) error("Real_mantissa()");
	if (Real_negative_flag(N)) error("Real_negative_flag()");
	if (Real_zero_p(N)) error("Real_zero_p()");
	if (!Real_positive_p(N)) error("Real_positive_p()");
	if (Real_negative_p(N)) error("Real_negative_p(1)");
	N = Real_negate(N);
	if (!Real_negative_p(N)) error("Real_negative_p(2)");
	A = make_real(1, 1, make_integer(123));
	B = make_real(-1, -1, make_integer(456));
	if (real_negative_p(real_abs(B))) error("real_abs()");
	N = real_add(A, B);
	if (!result(N, -1, 11844)) error("real_add()");
	N = real_divide(A, Two);
	if (!result(N, 0, 615)) error("real_divide()");
	if (real_equal_p(A, B)) error("real_equal_p(1)");
	if (!real_equal_p(A, A)) error("real_equal_p(2)");
	N = real_floor(B);
	if (!result(N, 0, -46)) error("real_floor()");
	N = real_trunc(B);
	if (!result(N, 0, -45)) error("real_floor()");
	N = real_ceil(B);
	if (!result(N, 0, -45)) error("real_ceil()");
	if (!real_integer_p(A)) error("real_integer_p(1)");
	if (real_integer_p(B)) error("real_integer_p(2)");
	if (real_less_p(A, B)) error("real_less_p(1)");
	if (!real_less_p(B, A)) error("real_less_p(1)");
	if (real_less_p(B, B)) error("real_less_p(2)");
	N = real_multiply(B, Two);
	if (!result(N, -1, -912)) error("real_multiply()");
	N = real_negate(B);
	if (!result(N, -1, 456)) error("real_negate()");
	if (real_zero_p(N)) error("real_zero_p()");
	if (!real_positive_p(N)) error("real_positive_p()");
	if (real_negative_p(N)) error("real_negative_p(1)");
	N = real_negate(N);
	if (!real_negative_p(N)) error("real_negative_p(2)");
	N = real_subtract(A, B);
	if (!result(N, -1, 12756)) error("real_subtract()");
	N = real_to_bignum(A);
	if (bignum_to_int(N) != 1230) error("real_to_bignum()");
}

void print_test(char *name, void (*printer)(cell), cell n, char *s) {
	int	p, op, i;
	char	b[100];

	p = open_output_port(TESTFILE, 0);
	op = output_port();
	set_output_port(p);
	(*printer)(n);
	close_port(p);
	set_output_port(op);
	p = open_input_port(TESTFILE);
	op = input_port();
	set_input_port(p);
	i = blockread(b, 100);
	if (i > 0)
		b[i] = 0;
	close_port(p);
	set_input_port(op);
	if (strcmp(s, b))
		error(name);
}

void test_io(void) {
	int	c, i, p;
	char	b[100];

	if (input_port() != 0) error("input_port(1)");
	if (output_port() != 1) error("output_port(1)");
	p = open_output_port(TESTFILE, 0);
	set_output_port(p);
	prints("0123456789");
	close_port(p);
	reset_std_ports();
	p = open_input_port(TESTFILE);
	set_input_port(p);
	for (i=0; i<5; i++) {
		if ((c = readc()) != "0123456789"[i])
			error("readc(1)");
	}
	reject(c);
	for (i=4; i<10; i++) {
		if ((c = readc()) != "0123456789"[i])
			error("readc(2)");
	}
	close_port(p);
	reset_std_ports();
	p = open_output_port(TESTFILE, 1);
	set_output_port(p);
	prints("0123456789");
	close_port(p);
	reset_std_ports();
	p = open_input_port(TESTFILE);
	set_input_port(p);
	if (blockread(b, 20) < 20) error("blockread()");
	close_port(p);
	reset_std_ports();
	if (input_port() != 0) error("input_port(2)");
	if (output_port() != 1) error("output_port(2)");
	print_test("print_bignum()", print_bignum,
			string_to_bignum("-12345678901234567890"),
			"-12345678901234567890");
	print_test("print_expanded_real()" ,print_expanded_real,
			make_real(1, -6, make_integer(12345)),
			"0.012345");
	print_test("print_sci_real()" ,print_sci_real,
			make_real(1, -6, make_integer(12345)),
			"1.2345e-2");
	for (i=0; i<MAX_PORTS*3; i++)
		if (open_input_port(TESTFILE) < 0)
			error("port finalization");
}

void test_conv(void) {
	if (integer_string_p("")) error("integer_string_p(1)");
	if (integer_string_p("-")) error("integer_string_p(2)");
	if (!integer_string_p("0")) error("integer_string_p(3)");
	if (!integer_string_p("-1")) error("integer_string_p(4)");
	if (!integer_string_p("+01234567890")) error("integer_string_p(5)");
	if (string_numeric_p("")) error("string_numeric_p(1)");
	if (string_numeric_p("-")) error("string_numeric_p(2)");
	if (!string_numeric_p("0")) error("string_numeric_p(3)");
	if (!string_numeric_p("-1")) error("string_numeric_p(4)");
	if (!string_numeric_p("+01234567890")) error("string_numeric_p(5)");
	if (string_numeric_p(".")) error("string_numeric_p(6)");
	if (string_numeric_p("0e")) error("string_numeric_p(7)");
	if (!string_numeric_p(".0")) error("string_numeric_p(8)");
	if (!string_numeric_p("0.")) error("string_numeric_p(9)");
	if (!string_numeric_p("1e0")) error("string_numeric_p(10)");
	if (!string_numeric_p("+1e-23")) error("string_numeric_p(11)");
	if (!string_numeric_p("-1e+23")) error("string_numeric_p(12)");
	if (bignum_to_int(string_to_bignum("12345")) != 12345)
		error("string_to_bignum()");
	N = string_to_real("+12345.6e-7");
	if (!result(N, -8, 123456)) error("string_to_real()");
}

void test_util(void) {
	int	i;
	cell	a;

	if (asctol("12345") != 12345) error("asctol()");
	N = cons(NIL, NIL);
	for (i=0; i<100; i++)
		N = cons(NIL, N);
	if (length(N) != 101) error("length()");
	N = flat_copy(N, &a);
	if (length(N) != 101) error("flat_copy(1)");
	cdr(a) = cons(NIL, NIL);
	if (length(N) != 102) error("flat_copy(2)");
}

int main(void) {
	s9init(Roots);
	mem_error_handler(mem_error);
	test_types();
	test_bignum();
	test_real();
	test_io();
	test_conv();
	test_util();
	s9fini();
	remove(TESTFILE);
	bye(Errors);
}
#endif /* TEST */
