/* A Bison parser, made by GNU Bison 2.7.  */

/* Bison implementation for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2012 Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.7"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
/* Line 371 of yacc.c  */
#line 7 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"

#undef PARSER_DEBUG
#ifdef PARSER_DEBUG
# define YYDEBUG 1
#endif
#define YYERROR_VERBOSE 1
/*
 * Force yacc to use our memory management.  This is a little evil because
 * the macros assume that "parser_state *p" is in scope
 */
#define YYMALLOC(n)    mrb_malloc(p->mrb, (n))
#define YYFREE(o)      mrb_free(p->mrb, (o))
#define YYSTACK_USE_ALLOCA 0

#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include "mruby.h"
#include "mruby/compile.h"
#include "mruby/proc.h"
#include "mruby/error.h"
#include "node.h"
#include "mruby/throw.h"

#define YYLEX_PARAM p

typedef mrb_ast_node node;
typedef struct mrb_parser_state parser_state;
typedef struct mrb_parser_heredoc_info parser_heredoc_info;

static int yyparse(parser_state *p);
static int yylex(void *lval, parser_state *p);
static void yyerror(parser_state *p, const char *s);
static void yywarn(parser_state *p, const char *s);
static void yywarning(parser_state *p, const char *s);
static void backref_error(parser_state *p, node *n);
static void tokadd(parser_state *p, int32_t c);

#define identchar(c) (ISALNUM(c) || (c) == '_' || !ISASCII(c))

typedef unsigned int stack_type;

#define BITSTACK_PUSH(stack, n) ((stack) = ((stack)<<1)|((n)&1))
#define BITSTACK_POP(stack)     ((stack) = (stack) >> 1)
#define BITSTACK_LEXPOP(stack)  ((stack) = ((stack) >> 1) | ((stack) & 1))
#define BITSTACK_SET_P(stack)   ((stack)&1)

#define COND_PUSH(n)    BITSTACK_PUSH(p->cond_stack, (n))
#define COND_POP()      BITSTACK_POP(p->cond_stack)
#define COND_LEXPOP()   BITSTACK_LEXPOP(p->cond_stack)
#define COND_P()        BITSTACK_SET_P(p->cond_stack)

#define CMDARG_PUSH(n)  BITSTACK_PUSH(p->cmdarg_stack, (n))
#define CMDARG_POP()    BITSTACK_POP(p->cmdarg_stack)
#define CMDARG_LEXPOP() BITSTACK_LEXPOP(p->cmdarg_stack)
#define CMDARG_P()      BITSTACK_SET_P(p->cmdarg_stack)

#define SET_LINENO(c,n) ((c)->lineno = (n))
#define NODE_LINENO(c,n) do {\
  if (n) {\
     (c)->filename_index = (n)->filename_index;\
     (c)->lineno = (n)->lineno;\
  }\
} while (0)

#define sym(x) ((mrb_sym)(intptr_t)(x))
#define nsym(x) ((node*)(intptr_t)(x))

static inline mrb_sym
intern_cstr_gen(parser_state *p, const char *s)
{
  return mrb_intern_cstr(p->mrb, s);
}
#define intern_cstr(s) intern_cstr_gen(p,(s))

static inline mrb_sym
intern_gen(parser_state *p, const char *s, size_t len)
{
  return mrb_intern(p->mrb, s, len);
}
#define intern(s,len) intern_gen(p,(s),(len))

static inline mrb_sym
intern_gen_c(parser_state *p, const char c)
{
  return mrb_intern(p->mrb, &c, 1);
}
#define intern_c(c) intern_gen_c(p,(c))

static void
cons_free_gen(parser_state *p, node *cons)
{
  cons->cdr = p->cells;
  p->cells = cons;
}
#define cons_free(c) cons_free_gen(p, (c))

static void*
parser_palloc(parser_state *p, size_t size)
{
  void *m = mrb_pool_alloc(p->pool, size);

  if (!m) {
    MRB_THROW(p->jmp);
  }
  return m;
}

static node*
cons_gen(parser_state *p, node *car, node *cdr)
{
  node *c;

  if (p->cells) {
    c = p->cells;
    p->cells = p->cells->cdr;
  }
  else {
    c = (node *)parser_palloc(p, sizeof(mrb_ast_node));
  }

  c->car = car;
  c->cdr = cdr;
  c->lineno = p->lineno;
  c->filename_index = p->current_filename_index;
  return c;
}
#define cons(a,b) cons_gen(p,(a),(b))

static node*
list1_gen(parser_state *p, node *a)
{
  return cons(a, 0);
}
#define list1(a) list1_gen(p, (a))

static node*
list2_gen(parser_state *p, node *a, node *b)
{
  return cons(a, cons(b,0));
}
#define list2(a,b) list2_gen(p, (a),(b))

static node*
list3_gen(parser_state *p, node *a, node *b, node *c)
{
  return cons(a, cons(b, cons(c,0)));
}
#define list3(a,b,c) list3_gen(p, (a),(b),(c))

static node*
list4_gen(parser_state *p, node *a, node *b, node *c, node *d)
{
  return cons(a, cons(b, cons(c, cons(d, 0))));
}
#define list4(a,b,c,d) list4_gen(p, (a),(b),(c),(d))

static node*
list5_gen(parser_state *p, node *a, node *b, node *c, node *d, node *e)
{
  return cons(a, cons(b, cons(c, cons(d, cons(e, 0)))));
}
#define list5(a,b,c,d,e) list5_gen(p, (a),(b),(c),(d),(e))

static node*
list6_gen(parser_state *p, node *a, node *b, node *c, node *d, node *e, node *f)
{
  return cons(a, cons(b, cons(c, cons(d, cons(e, cons(f, 0))))));
}
#define list6(a,b,c,d,e,f) list6_gen(p, (a),(b),(c),(d),(e),(f))

static node*
append_gen(parser_state *p, node *a, node *b)
{
  node *c = a;

  if (!a) return b;
  while (c->cdr) {
    c = c->cdr;
  }
  if (b) {
    c->cdr = b;
  }
  return a;
}
#define append(a,b) append_gen(p,(a),(b))
#define push(a,b) append_gen(p,(a),list1(b))

static char*
parser_strndup(parser_state *p, const char *s, size_t len)
{
  char *b = (char *)parser_palloc(p, len+1);

  memcpy(b, s, len);
  b[len] = '\0';
  return b;
}
#undef strndup
#define strndup(s,len) parser_strndup(p, s, len)

static char*
parser_strdup(parser_state *p, const char *s)
{
  return parser_strndup(p, s, strlen(s));
}
#undef strdup
#define strdup(s) parser_strdup(p, s)

/* xxx ----------------------------- */

static node*
local_switch(parser_state *p)
{
  node *prev = p->locals;

  p->locals = cons(0, 0);
  return prev;
}

static void
local_resume(parser_state *p, node *prev)
{
  p->locals = prev;
}

static void
local_nest(parser_state *p)
{
  p->locals = cons(0, p->locals);
}

static void
local_unnest(parser_state *p)
{
  if (p->locals) {
    p->locals = p->locals->cdr;
  }
}

static mrb_bool
local_var_p(parser_state *p, mrb_sym sym)
{
  node *l = p->locals;

  while (l) {
    node *n = l->car;
    while (n) {
      if (sym(n->car) == sym) return TRUE;
      n = n->cdr;
    }
    l = l->cdr;
  }
  return FALSE;
}

static void
local_add_f(parser_state *p, mrb_sym sym)
{
  if (p->locals) {
    p->locals->car = push(p->locals->car, nsym(sym));
  }
}

static void
local_add(parser_state *p, mrb_sym sym)
{
  if (!local_var_p(p, sym)) {
    local_add_f(p, sym);
  }
}

static node*
locals_node(parser_state *p)
{
  return p->locals ? p->locals->car : NULL;
}

/* (:scope (vars..) (prog...)) */
static node*
new_scope(parser_state *p, node *body)
{
  return cons((node*)NODE_SCOPE, cons(locals_node(p), body));
}

/* (:begin prog...) */
static node*
new_begin(parser_state *p, node *body)
{
  if (body) {
    return list2((node*)NODE_BEGIN, body);
  }
  return cons((node*)NODE_BEGIN, 0);
}

#define newline_node(n) (n)

/* (:rescue body rescue else) */
static node*
new_rescue(parser_state *p, node *body, node *resq, node *els)
{
  return list4((node*)NODE_RESCUE, body, resq, els);
}

/* (:ensure body ensure) */
static node*
new_ensure(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_ENSURE, cons(a, cons(0, b)));
}

/* (:nil) */
static node*
new_nil(parser_state *p)
{
  return list1((node*)NODE_NIL);
}

/* (:true) */
static node*
new_true(parser_state *p)
{
  return list1((node*)NODE_TRUE);
}

/* (:false) */
static node*
new_false(parser_state *p)
{
  return list1((node*)NODE_FALSE);
}

/* (:alias new old) */
static node*
new_alias(parser_state *p, mrb_sym a, mrb_sym b)
{
  return cons((node*)NODE_ALIAS, cons(nsym(a), nsym(b)));
}

/* (:if cond then else) */
static node*
new_if(parser_state *p, node *a, node *b, node *c)
{
  return list4((node*)NODE_IF, a, b, c);
}

/* (:unless cond then else) */
static node*
new_unless(parser_state *p, node *a, node *b, node *c)
{
  return list4((node*)NODE_IF, a, c, b);
}

/* (:while cond body) */
static node*
new_while(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_WHILE, cons(a, b));
}

/* (:until cond body) */
static node*
new_until(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_UNTIL, cons(a, b));
}

/* (:for var obj body) */
static node*
new_for(parser_state *p, node *v, node *o, node *b)
{
  return list4((node*)NODE_FOR, v, o, b);
}

/* (:case a ((when ...) body) ((when...) body)) */
static node*
new_case(parser_state *p, node *a, node *b)
{
  node *n = list2((node*)NODE_CASE, a);
  node *n2 = n;

  while (n2->cdr) {
    n2 = n2->cdr;
  }
  n2->cdr = b;
  return n;
}

/* (:postexe a) */
static node*
new_postexe(parser_state *p, node *a)
{
  return cons((node*)NODE_POSTEXE, a);
}

/* (:self) */
static node*
new_self(parser_state *p)
{
  return list1((node*)NODE_SELF);
}

/* (:call a b c) */
static node*
new_call(parser_state *p, node *a, mrb_sym b, node *c)
{
  node *n = list4((node*)NODE_CALL, a, nsym(b), c);
  NODE_LINENO(n, a);
  return n;
}

/* (:fcall self mid args) */
static node*
new_fcall(parser_state *p, mrb_sym b, node *c)
{
  node *n = new_self(p);
  NODE_LINENO(n, c);
  n = list4((node*)NODE_FCALL, n, nsym(b), c);
  NODE_LINENO(n, c);
  return n;
}

/* (:super . c) */
static node*
new_super(parser_state *p, node *c)
{
  return cons((node*)NODE_SUPER, c);
}

/* (:zsuper) */
static node*
new_zsuper(parser_state *p)
{
  return list1((node*)NODE_ZSUPER);
}

/* (:yield . c) */
static node*
new_yield(parser_state *p, node *c)
{
  if (c) {
    if (c->cdr) {
      yyerror(p, "both block arg and actual block given");
    }
    return cons((node*)NODE_YIELD, c->car);
  }
  return cons((node*)NODE_YIELD, 0);
}

/* (:return . c) */
static node*
new_return(parser_state *p, node *c)
{
  return cons((node*)NODE_RETURN, c);
}

/* (:break . c) */
static node*
new_break(parser_state *p, node *c)
{
  return cons((node*)NODE_BREAK, c);
}

/* (:next . c) */
static node*
new_next(parser_state *p, node *c)
{
  return cons((node*)NODE_NEXT, c);
}

/* (:redo) */
static node*
new_redo(parser_state *p)
{
  return list1((node*)NODE_REDO);
}

/* (:retry) */
static node*
new_retry(parser_state *p)
{
  return list1((node*)NODE_RETRY);
}

/* (:dot2 a b) */
static node*
new_dot2(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_DOT2, cons(a, b));
}

/* (:dot3 a b) */
static node*
new_dot3(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_DOT3, cons(a, b));
}

/* (:colon2 b c) */
static node*
new_colon2(parser_state *p, node *b, mrb_sym c)
{
  return cons((node*)NODE_COLON2, cons(b, nsym(c)));
}

/* (:colon3 . c) */
static node*
new_colon3(parser_state *p, mrb_sym c)
{
  return cons((node*)NODE_COLON3, nsym(c));
}

/* (:and a b) */
static node*
new_and(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_AND, cons(a, b));
}

/* (:or a b) */
static node*
new_or(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_OR, cons(a, b));
}

/* (:array a...) */
static node*
new_array(parser_state *p, node *a)
{
  return cons((node*)NODE_ARRAY, a);
}

/* (:splat . a) */
static node*
new_splat(parser_state *p, node *a)
{
  return cons((node*)NODE_SPLAT, a);
}

/* (:hash (k . v) (k . v)...) */
static node*
new_hash(parser_state *p, node *a)
{
  return cons((node*)NODE_HASH, a);
}

/* (:sym . a) */
static node*
new_sym(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_SYM, nsym(sym));
}

static mrb_sym
new_strsym(parser_state *p, node* str)
{
  const char *s = (const char*)str->cdr->car;
  size_t len = (size_t)str->cdr->cdr;

  return mrb_intern(p->mrb, s, len);
}

/* (:lvar . a) */
static node*
new_lvar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_LVAR, nsym(sym));
}

/* (:gvar . a) */
static node*
new_gvar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_GVAR, nsym(sym));
}

/* (:ivar . a) */
static node*
new_ivar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_IVAR, nsym(sym));
}

/* (:cvar . a) */
static node*
new_cvar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_CVAR, nsym(sym));
}

/* (:const . a) */
static node*
new_const(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_CONST, nsym(sym));
}

/* (:undef a...) */
static node*
new_undef(parser_state *p, mrb_sym sym)
{
  return list2((node*)NODE_UNDEF, nsym(sym));
}

/* (:class class super body) */
static node*
new_class(parser_state *p, node *c, node *s, node *b)
{
  return list4((node*)NODE_CLASS, c, s, cons(locals_node(p), b));
}

/* (:sclass obj body) */
static node*
new_sclass(parser_state *p, node *o, node *b)
{
  return list3((node*)NODE_SCLASS, o, cons(locals_node(p), b));
}

/* (:module module body) */
static node*
new_module(parser_state *p, node *m, node *b)
{
  return list3((node*)NODE_MODULE, m, cons(locals_node(p), b));
}

/* (:def m lv (arg . body)) */
static node*
new_def(parser_state *p, mrb_sym m, node *a, node *b)
{
  return list5((node*)NODE_DEF, nsym(m), locals_node(p), a, b);
}

/* (:sdef obj m lv (arg . body)) */
static node*
new_sdef(parser_state *p, node *o, mrb_sym m, node *a, node *b)
{
  return list6((node*)NODE_SDEF, o, nsym(m), locals_node(p), a, b);
}

/* (:arg . sym) */
static node*
new_arg(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_ARG, nsym(sym));
}

/* (m o r m2 b) */
/* m: (a b c) */
/* o: ((a . e1) (b . e2)) */
/* r: a */
/* m2: (a b c) */
/* b: a */
static node*
new_args(parser_state *p, node *m, node *opt, mrb_sym rest, node *m2, mrb_sym blk)
{
  node *n;

  n = cons(m2, nsym(blk));
  n = cons(nsym(rest), n);
  n = cons(opt, n);
  return cons(m, n);
}

/* (:block_arg . a) */
static node*
new_block_arg(parser_state *p, node *a)
{
  return cons((node*)NODE_BLOCK_ARG, a);
}

/* (:block arg body) */
static node*
new_block(parser_state *p, node *a, node *b)
{
  return list4((node*)NODE_BLOCK, locals_node(p), a, b);
}

/* (:lambda arg body) */
static node*
new_lambda(parser_state *p, node *a, node *b)
{
  return list4((node*)NODE_LAMBDA, locals_node(p), a, b);
}

/* (:asgn lhs rhs) */
static node*
new_asgn(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_ASGN, cons(a, b));
}

/* (:masgn mlhs=(pre rest post)  mrhs) */
static node*
new_masgn(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_MASGN, cons(a, b));
}

/* (:asgn lhs rhs) */
static node*
new_op_asgn(parser_state *p, node *a, mrb_sym op, node *b)
{
  return list4((node*)NODE_OP_ASGN, a, nsym(op), b);
}

/* (:int . i) */
static node*
new_int(parser_state *p, const char *s, int base)
{
  return list3((node*)NODE_INT, (node*)strdup(s), (node*)(intptr_t)base);
}

/* (:float . i) */
static node*
new_float(parser_state *p, const char *s)
{
  return cons((node*)NODE_FLOAT, (node*)strdup(s));
}

/* (:str . (s . len)) */
static node*
new_str(parser_state *p, const char *s, int len)
{
  return cons((node*)NODE_STR, cons((node*)strndup(s, len), (node*)(intptr_t)len));
}

/* (:dstr . a) */
static node*
new_dstr(parser_state *p, node *a)
{
  return cons((node*)NODE_DSTR, a);
}

/* (:str . (s . len)) */
static node*
new_xstr(parser_state *p, const char *s, int len)
{
  return cons((node*)NODE_XSTR, cons((node*)strndup(s, len), (node*)(intptr_t)len));
}

/* (:xstr . a) */
static node*
new_dxstr(parser_state *p, node *a)
{
  return cons((node*)NODE_DXSTR, a);
}

/* (:dsym . a) */
static node*
new_dsym(parser_state *p, node *a)
{
  return cons((node*)NODE_DSYM, new_dstr(p, a));
}

/* (:str . (a . a)) */
static node*
new_regx(parser_state *p, const char *p1, const char* p2)
{
  return cons((node*)NODE_REGX, cons((node*)p1, (node*)p2));
}

/* (:dregx . a) */
static node*
new_dregx(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_DREGX, cons(a, b));
}

/* (:backref . n) */
static node*
new_back_ref(parser_state *p, int n)
{
  return cons((node*)NODE_BACK_REF, (node*)(intptr_t)n);
}

/* (:nthref . n) */
static node*
new_nth_ref(parser_state *p, int n)
{
  return cons((node*)NODE_NTH_REF, (node*)(intptr_t)n);
}

/* (:heredoc . a) */
static node*
new_heredoc(parser_state *p)
{
  parser_heredoc_info *inf = (parser_heredoc_info *)parser_palloc(p, sizeof(parser_heredoc_info));
  return cons((node*)NODE_HEREDOC, (node*)inf);
}

static void
new_bv(parser_state *p, mrb_sym id)
{
}

static node*
new_literal_delim(parser_state *p)
{
  return cons((node*)NODE_LITERAL_DELIM, 0);
}

/* (:words . a) */
static node*
new_words(parser_state *p, node *a)
{
  return cons((node*)NODE_WORDS, a);
}

/* (:symbols . a) */
static node*
new_symbols(parser_state *p, node *a)
{
  return cons((node*)NODE_SYMBOLS, a);
}

/* xxx ----------------------------- */

/* (:call a op) */
static node*
call_uni_op(parser_state *p, node *recv, const char *m)
{
  return new_call(p, recv, intern_cstr(m), 0);
}

/* (:call a op b) */
static node*
call_bin_op(parser_state *p, node *recv, const char *m, node *arg1)
{
  return new_call(p, recv, intern_cstr(m), list1(list1(arg1)));
}

static void
args_with_block(parser_state *p, node *a, node *b)
{
  if (b) {
    if (a->cdr) {
      yyerror(p, "both block arg and actual block given");
    }
    a->cdr = b;
  }
}

static void
call_with_block(parser_state *p, node *a, node *b)
{
  node *n;

  if (a->car == (node*)NODE_SUPER ||
      a->car == (node*)NODE_ZSUPER) {
    if (!a->cdr) a->cdr = cons(0, b);
    else {
      args_with_block(p, a->cdr, b);
    }
  }
  else {
    n = a->cdr->cdr->cdr;
    if (!n->car) n->car = cons(0, b);
    else {
      args_with_block(p, n->car, b);
    }
  }
}

static node*
negate_lit(parser_state *p, node *n)
{
  return cons((node*)NODE_NEGATE, n);
}

static node*
cond(node *n)
{
  return n;
}

static node*
ret_args(parser_state *p, node *n)
{
  if (n->cdr) {
    yyerror(p, "block argument should not be given");
    return NULL;
  }
  if (!n->car->cdr) return n->car->car;
  return new_array(p, n->car);
}

static void
assignable(parser_state *p, node *lhs)
{
  if ((int)(intptr_t)lhs->car == NODE_LVAR) {
    local_add(p, sym(lhs->cdr));
  }
}

static node*
var_reference(parser_state *p, node *lhs)
{
  node *n;

  if ((int)(intptr_t)lhs->car == NODE_LVAR) {
    if (!local_var_p(p, sym(lhs->cdr))) {
      n = new_fcall(p, sym(lhs->cdr), 0);
      cons_free(lhs);
      return n;
    }
  }

  return lhs;
}

typedef enum mrb_string_type  string_type;

static node*
new_strterm(parser_state *p, string_type type, int term, int paren)
{
  return cons((node*)(intptr_t)type, cons((node*)0, cons((node*)(intptr_t)paren, (node*)(intptr_t)term)));
}

static void
end_strterm(parser_state *p)
{
  cons_free(p->lex_strterm->cdr->cdr);
  cons_free(p->lex_strterm->cdr);
  cons_free(p->lex_strterm);
  p->lex_strterm = NULL;
}

static parser_heredoc_info *
parsing_heredoc_inf(parser_state *p)
{
  node *nd = p->parsing_heredoc;
  if (nd == NULL)
    return NULL;
  /* mrb_assert(nd->car->car == NODE_HEREDOC); */
  return (parser_heredoc_info*)nd->car->cdr;
}

static void
heredoc_treat_nextline(parser_state *p)
{
  if (p->heredocs_from_nextline == NULL)
    return;
  if (p->parsing_heredoc == NULL) {
    node *n;
    p->parsing_heredoc = p->heredocs_from_nextline;
    p->lex_strterm_before_heredoc = p->lex_strterm;
    p->lex_strterm = new_strterm(p, parsing_heredoc_inf(p)->type, 0, 0);
    n = p->all_heredocs;
    if (n) {
      while (n->cdr)
        n = n->cdr;
      n->cdr = p->parsing_heredoc;
    }
    else {
      p->all_heredocs = p->parsing_heredoc;
    }
  }
  else {
    node *n, *m;
    m = p->heredocs_from_nextline;
    while (m->cdr)
      m = m->cdr;
    n = p->all_heredocs;
    mrb_assert(n != NULL);
    if (n == p->parsing_heredoc) {
      m->cdr = n;
      p->all_heredocs = p->heredocs_from_nextline;
      p->parsing_heredoc = p->heredocs_from_nextline;
    }
    else {
      while (n->cdr != p->parsing_heredoc) {
        n = n->cdr;
        mrb_assert(n != NULL);
      }
      m->cdr = n->cdr;
      n->cdr = p->heredocs_from_nextline;
      p->parsing_heredoc = p->heredocs_from_nextline;
    }
  }
  p->heredocs_from_nextline = NULL;
}

static void
heredoc_end(parser_state *p)
{
  p->parsing_heredoc = p->parsing_heredoc->cdr;
  if (p->parsing_heredoc == NULL) {
    p->lstate = EXPR_BEG;
    p->cmd_start = TRUE;
    end_strterm(p);
    p->lex_strterm = p->lex_strterm_before_heredoc;
    p->lex_strterm_before_heredoc = NULL;
    p->heredoc_end_now = TRUE;
  }
  else {
    /* next heredoc */
    p->lex_strterm->car = (node*)(intptr_t)parsing_heredoc_inf(p)->type;
  }
}
#define is_strterm_type(p,str_func) ((int)(intptr_t)((p)->lex_strterm->car) & (str_func))

/* xxx ----------------------------- */


/* Line 371 of yacc.c  */
#line 1074 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\build\\host-debug\\mrbgems\\mruby-compiler\\core\\y.tab.c"

# ifndef YY_NULL
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULL nullptr
#  else
#   define YY_NULL 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     keyword_class = 258,
     keyword_module = 259,
     keyword_def = 260,
     keyword_begin = 261,
     keyword_if = 262,
     keyword_unless = 263,
     keyword_while = 264,
     keyword_until = 265,
     keyword_for = 266,
     keyword_undef = 267,
     keyword_rescue = 268,
     keyword_ensure = 269,
     keyword_end = 270,
     keyword_then = 271,
     keyword_elsif = 272,
     keyword_else = 273,
     keyword_case = 274,
     keyword_when = 275,
     keyword_break = 276,
     keyword_next = 277,
     keyword_redo = 278,
     keyword_retry = 279,
     keyword_in = 280,
     keyword_do = 281,
     keyword_do_cond = 282,
     keyword_do_block = 283,
     keyword_do_LAMBDA = 284,
     keyword_return = 285,
     keyword_yield = 286,
     keyword_super = 287,
     keyword_self = 288,
     keyword_nil = 289,
     keyword_true = 290,
     keyword_false = 291,
     keyword_and = 292,
     keyword_or = 293,
     keyword_not = 294,
     modifier_if = 295,
     modifier_unless = 296,
     modifier_while = 297,
     modifier_until = 298,
     modifier_rescue = 299,
     keyword_alias = 300,
     keyword_BEGIN = 301,
     keyword_END = 302,
     keyword__LINE__ = 303,
     keyword__FILE__ = 304,
     keyword__ENCODING__ = 305,
     tIDENTIFIER = 306,
     tFID = 307,
     tGVAR = 308,
     tIVAR = 309,
     tCONSTANT = 310,
     tCVAR = 311,
     tLABEL = 312,
     tINTEGER = 313,
     tFLOAT = 314,
     tCHAR = 315,
     tXSTRING = 316,
     tREGEXP = 317,
     tSTRING = 318,
     tSTRING_PART = 319,
     tSTRING_MID = 320,
     tNTH_REF = 321,
     tBACK_REF = 322,
     tREGEXP_END = 323,
     tUPLUS = 324,
     tUMINUS = 325,
     tPOW = 326,
     tCMP = 327,
     tEQ = 328,
     tEQQ = 329,
     tNEQ = 330,
     tGEQ = 331,
     tLEQ = 332,
     tANDOP = 333,
     tOROP = 334,
     tMATCH = 335,
     tNMATCH = 336,
     tDOT2 = 337,
     tDOT3 = 338,
     tAREF = 339,
     tASET = 340,
     tLSHFT = 341,
     tRSHFT = 342,
     tCOLON2 = 343,
     tCOLON3 = 344,
     tOP_ASGN = 345,
     tASSOC = 346,
     tLPAREN = 347,
     tLPAREN_ARG = 348,
     tRPAREN = 349,
     tLBRACK = 350,
     tLBRACE = 351,
     tLBRACE_ARG = 352,
     tSTAR = 353,
     tAMPER = 354,
     tLAMBDA = 355,
     tSYMBEG = 356,
     tREGEXP_BEG = 357,
     tWORDS_BEG = 358,
     tSYMBOLS_BEG = 359,
     tSTRING_BEG = 360,
     tXSTRING_BEG = 361,
     tSTRING_DVAR = 362,
     tLAMBEG = 363,
     tHEREDOC_BEG = 364,
     tHEREDOC_END = 365,
     tLITERAL_DELIM = 366,
     tHD_LITERAL_DELIM = 367,
     tHD_STRING_PART = 368,
     tHD_STRING_MID = 369,
     tLOWEST = 370,
     tUMINUS_NUM = 371,
     tLAST_TOKEN = 372
   };
#endif


#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{
/* Line 387 of yacc.c  */
#line 1017 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"

    node *nd;
    mrb_sym id;
    int num;
    stack_type stack;
    const struct vtable *vars;


/* Line 387 of yacc.c  */
#line 1240 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\build\\host-debug\\mrbgems\\mruby-compiler\\core\\y.tab.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (parser_state *p);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* Copy the second part of user declarations.  */

/* Line 390 of yacc.c  */
#line 1267 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\build\\host-debug\\mrbgems\\mruby-compiler\\core\\y.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(N) (N)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (YYID (0))
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   10703

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  144
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  161
/* YYNRULES -- Number of rules.  */
#define YYNRULES  556
/* YYNRULES -- Number of states.  */
#define YYNSTATES  971

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   372

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     143,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   130,     2,     2,     2,   128,   123,     2,
     139,   140,   126,   124,   137,   125,   136,   127,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   118,   142,
     120,   116,   119,   117,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   135,     2,   141,   122,     2,   138,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   133,   121,   134,   131,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   129,   132
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     7,    10,    12,    14,    18,    21,
      23,    24,    30,    35,    38,    40,    42,    46,    49,    50,
      55,    58,    62,    66,    70,    74,    78,    83,    85,    89,
      93,   100,   106,   112,   118,   124,   128,   132,   136,   140,
     142,   146,   150,   152,   156,   160,   164,   167,   169,   171,
     173,   175,   177,   182,   183,   189,   192,   196,   201,   207,
     212,   218,   221,   224,   227,   230,   233,   235,   239,   241,
     245,   247,   250,   254,   260,   263,   268,   271,   276,   278,
     282,   284,   288,   291,   295,   297,   300,   302,   307,   311,
     315,   319,   323,   326,   328,   330,   335,   339,   343,   347,
     351,   354,   356,   358,   360,   363,   365,   369,   371,   373,
     375,   377,   379,   381,   383,   385,   386,   391,   393,   395,
     397,   399,   401,   403,   405,   407,   409,   411,   413,   415,
     417,   419,   421,   423,   425,   427,   429,   431,   433,   435,
     437,   439,   441,   443,   445,   447,   449,   451,   453,   455,
     457,   459,   461,   463,   465,   467,   469,   471,   473,   475,
     477,   479,   481,   483,   485,   487,   489,   491,   493,   495,
     497,   499,   501,   503,   505,   507,   509,   511,   513,   515,
     517,   519,   521,   523,   525,   527,   529,   533,   539,   543,
     549,   556,   562,   568,   574,   580,   585,   589,   593,   597,
     601,   605,   609,   613,   617,   621,   626,   631,   634,   637,
     641,   645,   649,   653,   657,   661,   665,   669,   673,   677,
     681,   685,   689,   692,   695,   699,   703,   707,   711,   718,
     720,   722,   724,   727,   732,   735,   739,   741,   743,   745,
     747,   750,   755,   758,   760,   763,   766,   771,   773,   774,
     777,   780,   783,   785,   787,   790,   794,   799,   804,   810,
     814,   819,   822,   824,   826,   828,   830,   832,   834,   836,
     838,   839,   844,   845,   846,   852,   853,   857,   861,   865,
     868,   872,   876,   878,   883,   887,   889,   894,   898,   901,
     903,   906,   907,   908,   914,   921,   928,   929,   930,   938,
     939,   940,   948,   954,   959,   960,   961,   971,   972,   979,
     980,   981,   990,   991,   997,   998,   999,  1007,  1008,  1009,
    1019,  1021,  1023,  1025,  1027,  1029,  1031,  1033,  1036,  1038,
    1040,  1042,  1048,  1050,  1053,  1055,  1057,  1059,  1063,  1065,
    1069,  1071,  1076,  1083,  1087,  1093,  1096,  1101,  1103,  1107,
    1114,  1123,  1128,  1135,  1140,  1143,  1150,  1153,  1158,  1165,
    1168,  1173,  1176,  1181,  1183,  1185,  1187,  1191,  1193,  1198,
    1200,  1205,  1207,  1211,  1213,  1215,  1220,  1222,  1226,  1230,
    1231,  1237,  1240,  1245,  1251,  1257,  1260,  1265,  1270,  1274,
    1278,  1282,  1285,  1287,  1292,  1293,  1299,  1300,  1306,  1312,
    1314,  1316,  1323,  1325,  1327,  1329,  1331,  1334,  1336,  1339,
    1341,  1343,  1345,  1347,  1349,  1351,  1353,  1356,  1360,  1362,
    1365,  1367,  1368,  1373,  1375,  1378,  1381,  1385,  1388,  1392,
    1394,  1395,  1397,  1399,  1402,  1404,  1407,  1409,  1412,  1414,
    1415,  1420,  1423,  1427,  1429,  1434,  1437,  1439,  1441,  1443,
    1445,  1447,  1450,  1453,  1457,  1459,  1461,  1464,  1467,  1469,
    1471,  1473,  1475,  1477,  1479,  1481,  1483,  1485,  1487,  1489,
    1491,  1493,  1495,  1497,  1499,  1500,  1505,  1508,  1512,  1515,
    1522,  1531,  1536,  1543,  1548,  1555,  1558,  1563,  1570,  1573,
    1578,  1581,  1586,  1588,  1589,  1591,  1593,  1595,  1597,  1599,
    1601,  1603,  1607,  1609,  1613,  1616,  1619,  1622,  1624,  1628,
    1630,  1634,  1636,  1638,  1641,  1643,  1645,  1647,  1650,  1653,
    1655,  1657,  1658,  1663,  1665,  1668,  1670,  1674,  1678,  1681,
    1683,  1685,  1687,  1689,  1691,  1693,  1695,  1697,  1699,  1701,
    1703,  1705,  1706,  1708,  1709,  1711,  1714,  1717,  1718,  1720,
    1722,  1724,  1726,  1727,  1731,  1733,  1736
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     145,     0,    -1,    -1,   146,   147,    -1,   148,   295,    -1,
     304,    -1,   149,    -1,   148,   303,   149,    -1,     1,   149,
      -1,   154,    -1,    -1,    46,   150,   133,   147,   134,    -1,
     152,   240,   218,   243,    -1,   153,   295,    -1,   304,    -1,
     154,    -1,   153,   303,   154,    -1,     1,   154,    -1,    -1,
      45,   175,   155,   175,    -1,    12,   176,    -1,   154,    40,
     158,    -1,   154,    41,   158,    -1,   154,    42,   158,    -1,
     154,    43,   158,    -1,   154,    44,   154,    -1,    47,   133,
     152,   134,    -1,   156,    -1,   164,   116,   159,    -1,   265,
      90,   159,    -1,   214,   135,   185,   298,    90,   159,    -1,
     214,   136,    51,    90,   159,    -1,   214,   136,    55,    90,
     159,    -1,   214,    88,    55,    90,   159,    -1,   214,    88,
      51,    90,   159,    -1,   267,    90,   159,    -1,   171,   116,
     192,    -1,   164,   116,   181,    -1,   164,   116,   192,    -1,
     157,    -1,   171,   116,   159,    -1,   171,   116,   156,    -1,
     159,    -1,   157,    37,   157,    -1,   157,    38,   157,    -1,
      39,   296,   157,    -1,   130,   159,    -1,   180,    -1,   157,
      -1,   163,    -1,   160,    -1,   233,    -1,   233,   294,   292,
     187,    -1,    -1,    97,   162,   224,   152,   134,    -1,   291,
     187,    -1,   291,   187,   161,    -1,   214,   136,   292,   187,
      -1,   214,   136,   292,   187,   161,    -1,   214,    88,   292,
     187,    -1,   214,    88,   292,   187,   161,    -1,    32,   187,
      -1,    31,   187,    -1,    30,   186,    -1,    21,   186,    -1,
      22,   186,    -1,   166,    -1,    92,   165,   297,    -1,   166,
      -1,    92,   165,   297,    -1,   168,    -1,   168,   167,    -1,
     168,    98,   170,    -1,   168,    98,   170,   137,   169,    -1,
     168,    98,    -1,   168,    98,   137,   169,    -1,    98,   170,
      -1,    98,   170,   137,   169,    -1,    98,    -1,    98,   137,
     169,    -1,   170,    -1,    92,   165,   297,    -1,   167,   137,
      -1,   168,   167,   137,    -1,   167,    -1,   168,   167,    -1,
     264,    -1,   214,   135,   185,   298,    -1,   214,   136,    51,
      -1,   214,    88,    51,    -1,   214,   136,    55,    -1,   214,
      88,    55,    -1,    89,    55,    -1,   267,    -1,   264,    -1,
     214,   135,   185,   298,    -1,   214,   136,    51,    -1,   214,
      88,    51,    -1,   214,   136,    55,    -1,   214,    88,    55,
      -1,    89,    55,    -1,   267,    -1,    51,    -1,    55,    -1,
      89,   172,    -1,   172,    -1,   214,    88,   172,    -1,    51,
      -1,    55,    -1,    52,    -1,   178,    -1,   179,    -1,   174,
      -1,   260,    -1,   175,    -1,    -1,   176,   137,   177,   175,
      -1,   121,    -1,   122,    -1,   123,    -1,    72,    -1,    73,
      -1,    74,    -1,    80,    -1,    81,    -1,   119,    -1,    76,
      -1,   120,    -1,    77,    -1,    75,    -1,    86,    -1,    87,
      -1,   124,    -1,   125,    -1,   126,    -1,    98,    -1,   127,
      -1,   128,    -1,    71,    -1,   130,    -1,   131,    -1,    69,
      -1,    70,    -1,    84,    -1,    85,    -1,   138,    -1,    48,
      -1,    49,    -1,    50,    -1,    46,    -1,    47,    -1,    45,
      -1,    37,    -1,     6,    -1,    21,    -1,    19,    -1,     3,
      -1,     5,    -1,    26,    -1,    18,    -1,    17,    -1,    15,
      -1,    14,    -1,    36,    -1,    11,    -1,    25,    -1,     4,
      -1,    22,    -1,    34,    -1,    39,    -1,    38,    -1,    23,
      -1,    13,    -1,    24,    -1,    30,    -1,    33,    -1,    32,
      -1,    16,    -1,    35,    -1,    12,    -1,    20,    -1,    31,
      -1,     7,    -1,     8,    -1,     9,    -1,    10,    -1,   171,
     116,   180,    -1,   171,   116,   180,    44,   180,    -1,   265,
      90,   180,    -1,   265,    90,   180,    44,   180,    -1,   214,
     135,   185,   298,    90,   180,    -1,   214,   136,    51,    90,
     180,    -1,   214,   136,    55,    90,   180,    -1,   214,    88,
      51,    90,   180,    -1,   214,    88,    55,    90,   180,    -1,
      89,    55,    90,   180,    -1,   267,    90,   180,    -1,   180,
      82,   180,    -1,   180,    83,   180,    -1,   180,   124,   180,
      -1,   180,   125,   180,    -1,   180,   126,   180,    -1,   180,
     127,   180,    -1,   180,   128,   180,    -1,   180,    71,   180,
      -1,   129,    58,    71,   180,    -1,   129,    59,    71,   180,
      -1,    69,   180,    -1,    70,   180,    -1,   180,   121,   180,
      -1,   180,   122,   180,    -1,   180,   123,   180,    -1,   180,
      72,   180,    -1,   180,   119,   180,    -1,   180,    76,   180,
      -1,   180,   120,   180,    -1,   180,    77,   180,    -1,   180,
      73,   180,    -1,   180,    74,   180,    -1,   180,    75,   180,
      -1,   180,    80,   180,    -1,   180,    81,   180,    -1,   130,
     180,    -1,   131,   180,    -1,   180,    86,   180,    -1,   180,
      87,   180,    -1,   180,    78,   180,    -1,   180,    79,   180,
      -1,   180,   117,   180,   296,   118,   180,    -1,   193,    -1,
     180,    -1,   304,    -1,   191,   299,    -1,   191,   137,   289,
     299,    -1,   289,   299,    -1,   139,   185,   297,    -1,   304,
      -1,   183,    -1,   304,    -1,   186,    -1,   191,   137,    -1,
     191,   137,   289,   137,    -1,   289,   137,    -1,   163,    -1,
     191,   190,    -1,   289,   190,    -1,   191,   137,   289,   190,
      -1,   189,    -1,    -1,   188,   186,    -1,    99,   181,    -1,
     137,   189,    -1,   304,    -1,   181,    -1,    98,   181,    -1,
     191,   137,   181,    -1,   191,   137,    98,   181,    -1,   191,
     137,   253,   181,    -1,   191,   137,   253,    98,   181,    -1,
     191,   137,   181,    -1,   191,   137,    98,   181,    -1,    98,
     181,    -1,   244,    -1,   245,    -1,   249,    -1,   250,    -1,
     251,    -1,   266,    -1,   267,    -1,    52,    -1,    -1,     6,
     194,   151,    15,    -1,    -1,    -1,    93,   195,   157,   196,
     297,    -1,    -1,    93,   197,   297,    -1,    92,   152,   140,
      -1,   214,    88,    55,    -1,    89,    55,    -1,    95,   182,
     141,    -1,    96,   288,   134,    -1,    30,    -1,    31,   139,
     186,   297,    -1,    31,   139,   297,    -1,    31,    -1,    39,
     139,   157,   297,    -1,    39,   139,   297,    -1,   291,   235,
      -1,   234,    -1,   234,   235,    -1,    -1,    -1,   100,   198,
     229,   199,   230,    -1,     7,   158,   215,   152,   217,    15,
      -1,     8,   158,   215,   152,   218,    15,    -1,    -1,    -1,
       9,   200,   158,   216,   201,   152,    15,    -1,    -1,    -1,
      10,   202,   158,   216,   203,   152,    15,    -1,    19,   158,
     295,   238,    15,    -1,    19,   295,   238,    15,    -1,    -1,
      -1,    11,   219,    25,   204,   158,   216,   205,   152,    15,
      -1,    -1,     3,   173,   268,   206,   151,    15,    -1,    -1,
      -1,     3,    86,   157,   207,   300,   208,   151,    15,    -1,
      -1,     4,   173,   209,   151,    15,    -1,    -1,    -1,     5,
     174,   210,   211,   270,   151,    15,    -1,    -1,    -1,     5,
     286,   294,   212,   174,   213,   270,   151,    15,    -1,    21,
      -1,    22,    -1,    23,    -1,    24,    -1,   193,    -1,   300,
      -1,    16,    -1,   300,    16,    -1,   300,    -1,    27,    -1,
     218,    -1,    17,   158,   215,   152,   217,    -1,   304,    -1,
      18,   152,    -1,   171,    -1,   164,    -1,   273,    -1,    92,
     222,   297,    -1,   220,    -1,   221,   137,   220,    -1,   221,
      -1,   221,   137,    98,   273,    -1,   221,   137,    98,   273,
     137,   221,    -1,   221,   137,    98,    -1,   221,   137,    98,
     137,   221,    -1,    98,   273,    -1,    98,   273,   137,   221,
      -1,    98,    -1,    98,   137,   221,    -1,   275,   137,   279,
     137,   282,   285,    -1,   275,   137,   279,   137,   282,   137,
     275,   285,    -1,   275,   137,   279,   285,    -1,   275,   137,
     279,   137,   275,   285,    -1,   275,   137,   282,   285,    -1,
     275,   137,    -1,   275,   137,   282,   137,   275,   285,    -1,
     275,   285,    -1,   279,   137,   282,   285,    -1,   279,   137,
     282,   137,   275,   285,    -1,   279,   285,    -1,   279,   137,
     275,   285,    -1,   282,   285,    -1,   282,   137,   275,   285,
      -1,   284,    -1,   304,    -1,   225,    -1,   121,   226,   121,
      -1,    79,    -1,   121,   223,   226,   121,    -1,   296,    -1,
     296,   142,   227,   296,    -1,   228,    -1,   227,   137,   228,
      -1,    51,    -1,   272,    -1,   139,   271,   226,   140,    -1,
     271,    -1,   108,   152,   134,    -1,    29,   152,    15,    -1,
      -1,    28,   232,   224,   152,    15,    -1,   163,   231,    -1,
     233,   294,   292,   184,    -1,   233,   294,   292,   184,   235,
      -1,   233,   294,   292,   187,   231,    -1,   291,   183,    -1,
     214,   136,   292,   184,    -1,   214,    88,   292,   183,    -1,
     214,    88,   293,    -1,   214,   136,   183,    -1,   214,    88,
     183,    -1,    32,   183,    -1,    32,    -1,   214,   135,   185,
     298,    -1,    -1,   133,   236,   224,   152,   134,    -1,    -1,
      26,   237,   224,   152,    15,    -1,    20,   191,   215,   152,
     239,    -1,   218,    -1,   238,    -1,    13,   241,   242,   215,
     152,   240,    -1,   304,    -1,   181,    -1,   192,    -1,   304,
      -1,    91,   171,    -1,   304,    -1,    14,   152,    -1,   304,
      -1,   263,    -1,   259,    -1,   258,    -1,   262,    -1,    60,
      -1,    63,    -1,   105,    63,    -1,   105,   246,    63,    -1,
     247,    -1,   246,   247,    -1,    65,    -1,    -1,    64,   248,
     152,   134,    -1,   111,    -1,   112,   253,    -1,   106,    61,
      -1,   106,   246,    61,    -1,   102,    62,    -1,   102,   246,
      62,    -1,   109,    -1,    -1,   253,    -1,   254,    -1,   253,
     254,    -1,   110,    -1,   255,   110,    -1,   256,    -1,   255,
     256,    -1,   114,    -1,    -1,   113,   257,   152,   134,    -1,
     103,    63,    -1,   103,   246,    63,    -1,   260,    -1,   101,
     105,   247,    63,    -1,   101,   261,    -1,   174,    -1,    54,
      -1,    53,    -1,    56,    -1,    63,    -1,   105,    63,    -1,
     104,    63,    -1,   104,   246,    63,    -1,    58,    -1,    59,
      -1,   129,    58,    -1,   129,    59,    -1,    51,    -1,    54,
      -1,    53,    -1,    56,    -1,    55,    -1,   264,    -1,   264,
      -1,    34,    -1,    33,    -1,    35,    -1,    36,    -1,    49,
      -1,    48,    -1,    66,    -1,    67,    -1,   300,    -1,    -1,
     120,   269,   158,   300,    -1,     1,   300,    -1,   139,   271,
     297,    -1,   271,   300,    -1,   275,   137,   280,   137,   282,
     285,    -1,   275,   137,   280,   137,   282,   137,   275,   285,
      -1,   275,   137,   280,   285,    -1,   275,   137,   280,   137,
     275,   285,    -1,   275,   137,   282,   285,    -1,   275,   137,
     282,   137,   275,   285,    -1,   275,   285,    -1,   280,   137,
     282,   285,    -1,   280,   137,   282,   137,   275,   285,    -1,
     280,   285,    -1,   280,   137,   275,   285,    -1,   282,   285,
      -1,   282,   137,   275,   285,    -1,   284,    -1,    -1,    55,
      -1,    54,    -1,    53,    -1,    56,    -1,   272,    -1,    51,
      -1,   273,    -1,    92,   222,   297,    -1,   274,    -1,   275,
     137,   274,    -1,    51,   116,    -1,   276,   181,    -1,   276,
     214,    -1,   278,    -1,   279,   137,   278,    -1,   277,    -1,
     280,   137,   277,    -1,   126,    -1,    98,    -1,   281,    51,
      -1,   281,    -1,   123,    -1,    99,    -1,   283,    51,    -1,
     137,   284,    -1,   304,    -1,   266,    -1,    -1,   139,   287,
     157,   297,    -1,   304,    -1,   289,   299,    -1,   290,    -1,
     289,   137,   290,    -1,   181,    91,   181,    -1,    57,   181,
      -1,    51,    -1,    55,    -1,    52,    -1,    51,    -1,    55,
      -1,    52,    -1,   178,    -1,    51,    -1,    52,    -1,   178,
      -1,   136,    -1,    88,    -1,    -1,   303,    -1,    -1,   301,
      -1,   296,   140,    -1,   296,   141,    -1,    -1,   301,    -1,
     137,    -1,   142,    -1,   301,    -1,    -1,   143,   302,   252,
      -1,   300,    -1,   303,   142,    -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  1167,  1167,  1167,  1178,  1184,  1188,  1193,  1197,  1203,
    1205,  1204,  1216,  1243,  1249,  1253,  1258,  1262,  1268,  1268,
    1272,  1276,  1280,  1284,  1288,  1292,  1296,  1301,  1302,  1306,
    1310,  1314,  1318,  1322,  1327,  1331,  1336,  1340,  1344,  1348,
    1351,  1355,  1362,  1363,  1367,  1371,  1375,  1379,  1382,  1389,
    1390,  1393,  1394,  1398,  1397,  1410,  1414,  1419,  1423,  1428,
    1432,  1437,  1441,  1445,  1449,  1453,  1459,  1463,  1469,  1470,
    1476,  1480,  1484,  1488,  1492,  1496,  1500,  1504,  1508,  1512,
    1518,  1519,  1525,  1529,  1535,  1539,  1545,  1549,  1553,  1557,
    1561,  1565,  1571,  1577,  1584,  1588,  1592,  1596,  1600,  1604,
    1610,  1616,  1623,  1627,  1630,  1634,  1638,  1644,  1645,  1646,
    1647,  1652,  1659,  1660,  1663,  1667,  1667,  1673,  1674,  1675,
    1676,  1677,  1678,  1679,  1680,  1681,  1682,  1683,  1684,  1685,
    1686,  1687,  1688,  1689,  1690,  1691,  1692,  1693,  1694,  1695,
    1696,  1697,  1698,  1699,  1700,  1701,  1704,  1704,  1704,  1705,
    1705,  1706,  1706,  1706,  1707,  1707,  1707,  1707,  1708,  1708,
    1708,  1709,  1709,  1709,  1710,  1710,  1710,  1710,  1711,  1711,
    1711,  1711,  1712,  1712,  1712,  1712,  1713,  1713,  1713,  1713,
    1714,  1714,  1714,  1714,  1715,  1715,  1718,  1722,  1726,  1730,
    1734,  1738,  1742,  1746,  1750,  1755,  1760,  1765,  1769,  1773,
    1777,  1781,  1785,  1789,  1793,  1797,  1801,  1805,  1809,  1813,
    1817,  1821,  1825,  1829,  1833,  1837,  1841,  1845,  1849,  1853,
    1857,  1861,  1865,  1869,  1873,  1877,  1881,  1885,  1889,  1893,
    1899,  1906,  1907,  1912,  1916,  1923,  1929,  1930,  1933,  1934,
    1935,  1940,  1945,  1952,  1957,  1962,  1967,  1972,  1979,  1979,
    1990,  1996,  2000,  2006,  2011,  2016,  2020,  2024,  2028,  2034,
    2038,  2042,  2048,  2049,  2050,  2051,  2052,  2053,  2054,  2055,
    2060,  2059,  2071,  2075,  2070,  2080,  2080,  2084,  2088,  2092,
    2096,  2101,  2106,  2110,  2114,  2118,  2122,  2126,  2130,  2134,
    2135,  2141,  2147,  2140,  2158,  2166,  2174,  2174,  2174,  2181,
    2181,  2181,  2188,  2194,  2199,  2201,  2198,  2210,  2208,  2224,
    2229,  2222,  2244,  2242,  2257,  2261,  2256,  2276,  2282,  2275,
    2297,  2301,  2305,  2309,  2315,  2322,  2323,  2324,  2327,  2328,
    2331,  2332,  2340,  2341,  2347,  2351,  2354,  2358,  2364,  2368,
    2374,  2378,  2382,  2386,  2390,  2394,  2398,  2402,  2406,  2412,
    2416,  2420,  2424,  2428,  2432,  2436,  2440,  2444,  2448,  2452,
    2456,  2460,  2464,  2468,  2474,  2475,  2482,  2486,  2490,  2497,
    2501,  2507,  2508,  2511,  2516,  2519,  2523,  2529,  2533,  2540,
    2539,  2552,  2562,  2566,  2571,  2578,  2582,  2586,  2590,  2594,
    2598,  2602,  2606,  2610,  2617,  2616,  2629,  2628,  2642,  2650,
    2659,  2662,  2669,  2672,  2676,  2677,  2680,  2684,  2687,  2691,
    2694,  2695,  2696,  2697,  2700,  2701,  2702,  2706,  2712,  2713,
    2719,  2724,  2723,  2734,  2738,  2744,  2748,  2754,  2758,  2764,
    2767,  2768,  2771,  2772,  2775,  2781,  2787,  2788,  2791,  2798,
    2797,  2811,  2815,  2822,  2826,  2833,  2840,  2841,  2842,  2843,
    2844,  2848,  2854,  2858,  2864,  2865,  2866,  2870,  2876,  2880,
    2884,  2888,  2892,  2898,  2904,  2908,  2912,  2916,  2920,  2924,
    2931,  2940,  2941,  2944,  2949,  2948,  2957,  2964,  2970,  2976,
    2980,  2984,  2988,  2992,  2996,  3000,  3004,  3008,  3012,  3016,
    3020,  3024,  3028,  3033,  3039,  3044,  3049,  3054,  3061,  3065,
    3072,  3076,  3082,  3086,  3092,  3099,  3105,  3111,  3115,  3121,
    3125,  3131,  3132,  3135,  3140,  3147,  3148,  3151,  3158,  3162,
    3169,  3174,  3174,  3199,  3200,  3206,  3211,  3217,  3221,  3227,
    3228,  3229,  3232,  3233,  3234,  3235,  3238,  3239,  3240,  3243,
    3244,  3247,  3248,  3251,  3252,  3255,  3258,  3261,  3262,  3263,
    3266,  3267,  3271,  3270,  3277,  3278,  3282
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "keyword_class", "keyword_module",
  "keyword_def", "keyword_begin", "keyword_if", "keyword_unless",
  "keyword_while", "keyword_until", "keyword_for", "keyword_undef",
  "keyword_rescue", "keyword_ensure", "keyword_end", "keyword_then",
  "keyword_elsif", "keyword_else", "keyword_case", "keyword_when",
  "keyword_break", "keyword_next", "keyword_redo", "keyword_retry",
  "keyword_in", "keyword_do", "keyword_do_cond", "keyword_do_block",
  "keyword_do_LAMBDA", "keyword_return", "keyword_yield", "keyword_super",
  "keyword_self", "keyword_nil", "keyword_true", "keyword_false",
  "keyword_and", "keyword_or", "keyword_not", "modifier_if",
  "modifier_unless", "modifier_while", "modifier_until", "modifier_rescue",
  "keyword_alias", "keyword_BEGIN", "keyword_END", "keyword__LINE__",
  "keyword__FILE__", "keyword__ENCODING__", "tIDENTIFIER", "tFID", "tGVAR",
  "tIVAR", "tCONSTANT", "tCVAR", "tLABEL", "tINTEGER", "tFLOAT", "tCHAR",
  "tXSTRING", "tREGEXP", "tSTRING", "tSTRING_PART", "tSTRING_MID",
  "tNTH_REF", "tBACK_REF", "tREGEXP_END", "tUPLUS", "tUMINUS", "tPOW",
  "tCMP", "tEQ", "tEQQ", "tNEQ", "tGEQ", "tLEQ", "tANDOP", "tOROP",
  "tMATCH", "tNMATCH", "tDOT2", "tDOT3", "tAREF", "tASET", "tLSHFT",
  "tRSHFT", "tCOLON2", "tCOLON3", "tOP_ASGN", "tASSOC", "tLPAREN",
  "tLPAREN_ARG", "tRPAREN", "tLBRACK", "tLBRACE", "tLBRACE_ARG", "tSTAR",
  "tAMPER", "tLAMBDA", "tSYMBEG", "tREGEXP_BEG", "tWORDS_BEG",
  "tSYMBOLS_BEG", "tSTRING_BEG", "tXSTRING_BEG", "tSTRING_DVAR", "tLAMBEG",
  "tHEREDOC_BEG", "tHEREDOC_END", "tLITERAL_DELIM", "tHD_LITERAL_DELIM",
  "tHD_STRING_PART", "tHD_STRING_MID", "tLOWEST", "'='", "'?'", "':'",
  "'>'", "'<'", "'|'", "'^'", "'&'", "'+'", "'-'", "'*'", "'/'", "'%'",
  "tUMINUS_NUM", "'!'", "'~'", "tLAST_TOKEN", "'{'", "'}'", "'['", "'.'",
  "','", "'`'", "'('", "')'", "']'", "';'", "'\\n'", "$accept", "program",
  "$@1", "top_compstmt", "top_stmts", "top_stmt", "@2", "bodystmt",
  "compstmt", "stmts", "stmt", "$@3", "command_asgn", "expr", "expr_value",
  "command_call", "block_command", "cmd_brace_block", "$@4", "command",
  "mlhs", "mlhs_inner", "mlhs_basic", "mlhs_item", "mlhs_list",
  "mlhs_post", "mlhs_node", "lhs", "cname", "cpath", "fname", "fsym",
  "undef_list", "$@5", "op", "reswords", "arg", "arg_value", "aref_args",
  "paren_args", "opt_paren_args", "opt_call_args", "call_args",
  "command_args", "@6", "block_arg", "opt_block_arg", "args", "mrhs",
  "primary", "@7", "@8", "$@9", "$@10", "@11", "@12", "$@13", "$@14",
  "$@15", "$@16", "$@17", "$@18", "@19", "@20", "@21", "@22", "@23", "@24",
  "@25", "@26", "primary_value", "then", "do", "if_tail", "opt_else",
  "for_var", "f_marg", "f_marg_list", "f_margs", "block_param",
  "opt_block_param", "block_param_def", "opt_bv_decl", "bv_decls", "bvar",
  "f_larglist", "lambda_body", "do_block", "$@27", "block_call",
  "method_call", "brace_block", "@28", "@29", "case_body", "cases",
  "opt_rescue", "exc_list", "exc_var", "opt_ensure", "literal", "string",
  "string_rep", "string_interp", "@30", "xstring", "regexp", "heredoc",
  "opt_heredoc_bodies", "heredoc_bodies", "heredoc_body",
  "heredoc_string_rep", "heredoc_string_interp", "@31", "words", "symbol",
  "basic_symbol", "sym", "symbols", "numeric", "variable", "var_lhs",
  "var_ref", "backref", "superclass", "$@32", "f_arglist", "f_args",
  "f_bad_arg", "f_norm_arg", "f_arg_item", "f_arg", "f_opt_asgn", "f_opt",
  "f_block_opt", "f_block_optarg", "f_optarg", "restarg_mark",
  "f_rest_arg", "blkarg_mark", "f_block_arg", "opt_f_block_arg",
  "singleton", "$@33", "assoc_list", "assocs", "assoc", "operation",
  "operation2", "operation3", "dot_or_colon", "opt_terms", "opt_nl",
  "rparen", "rbracket", "trailer", "term", "nl", "$@34", "terms", "none", YY_NULL
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,    61,    63,    58,    62,
      60,   124,    94,    38,    43,    45,    42,    47,    37,   371,
      33,   126,   372,   123,   125,    91,    46,    44,    96,    40,
      41,    93,    59,    10
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   144,   146,   145,   147,   148,   148,   148,   148,   149,
     150,   149,   151,   152,   153,   153,   153,   153,   155,   154,
     154,   154,   154,   154,   154,   154,   154,   154,   154,   154,
     154,   154,   154,   154,   154,   154,   154,   154,   154,   154,
     156,   156,   157,   157,   157,   157,   157,   157,   158,   159,
     159,   160,   160,   162,   161,   163,   163,   163,   163,   163,
     163,   163,   163,   163,   163,   163,   164,   164,   165,   165,
     166,   166,   166,   166,   166,   166,   166,   166,   166,   166,
     167,   167,   168,   168,   169,   169,   170,   170,   170,   170,
     170,   170,   170,   170,   171,   171,   171,   171,   171,   171,
     171,   171,   172,   172,   173,   173,   173,   174,   174,   174,
     174,   174,   175,   175,   176,   177,   176,   178,   178,   178,
     178,   178,   178,   178,   178,   178,   178,   178,   178,   178,
     178,   178,   178,   178,   178,   178,   178,   178,   178,   178,
     178,   178,   178,   178,   178,   178,   179,   179,   179,   179,
     179,   179,   179,   179,   179,   179,   179,   179,   179,   179,
     179,   179,   179,   179,   179,   179,   179,   179,   179,   179,
     179,   179,   179,   179,   179,   179,   179,   179,   179,   179,
     179,   179,   179,   179,   179,   179,   180,   180,   180,   180,
     180,   180,   180,   180,   180,   180,   180,   180,   180,   180,
     180,   180,   180,   180,   180,   180,   180,   180,   180,   180,
     180,   180,   180,   180,   180,   180,   180,   180,   180,   180,
     180,   180,   180,   180,   180,   180,   180,   180,   180,   180,
     181,   182,   182,   182,   182,   183,   184,   184,   185,   185,
     185,   185,   185,   186,   186,   186,   186,   186,   188,   187,
     189,   190,   190,   191,   191,   191,   191,   191,   191,   192,
     192,   192,   193,   193,   193,   193,   193,   193,   193,   193,
     194,   193,   195,   196,   193,   197,   193,   193,   193,   193,
     193,   193,   193,   193,   193,   193,   193,   193,   193,   193,
     193,   198,   199,   193,   193,   193,   200,   201,   193,   202,
     203,   193,   193,   193,   204,   205,   193,   206,   193,   207,
     208,   193,   209,   193,   210,   211,   193,   212,   213,   193,
     193,   193,   193,   193,   214,   215,   215,   215,   216,   216,
     217,   217,   218,   218,   219,   219,   220,   220,   221,   221,
     222,   222,   222,   222,   222,   222,   222,   222,   222,   223,
     223,   223,   223,   223,   223,   223,   223,   223,   223,   223,
     223,   223,   223,   223,   224,   224,   225,   225,   225,   226,
     226,   227,   227,   228,   228,   229,   229,   230,   230,   232,
     231,   233,   233,   233,   233,   234,   234,   234,   234,   234,
     234,   234,   234,   234,   236,   235,   237,   235,   238,   239,
     239,   240,   240,   241,   241,   241,   242,   242,   243,   243,
     244,   244,   244,   244,   245,   245,   245,   245,   246,   246,
     247,   248,   247,   247,   247,   249,   249,   250,   250,   251,
     252,   252,   253,   253,   254,   254,   255,   255,   256,   257,
     256,   258,   258,   259,   259,   260,   261,   261,   261,   261,
     261,   261,   262,   262,   263,   263,   263,   263,   264,   264,
     264,   264,   264,   265,   266,   266,   266,   266,   266,   266,
     266,   267,   267,   268,   269,   268,   268,   270,   270,   271,
     271,   271,   271,   271,   271,   271,   271,   271,   271,   271,
     271,   271,   271,   271,   272,   272,   272,   272,   273,   273,
     274,   274,   275,   275,   276,   277,   278,   279,   279,   280,
     280,   281,   281,   282,   282,   283,   283,   284,   285,   285,
     286,   287,   286,   288,   288,   289,   289,   290,   290,   291,
     291,   291,   292,   292,   292,   292,   293,   293,   293,   294,
     294,   295,   295,   296,   296,   297,   298,   299,   299,   299,
     300,   300,   302,   301,   303,   303,   304
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     1,     3,     2,     1,
       0,     5,     4,     2,     1,     1,     3,     2,     0,     4,
       2,     3,     3,     3,     3,     3,     4,     1,     3,     3,
       6,     5,     5,     5,     5,     3,     3,     3,     3,     1,
       3,     3,     1,     3,     3,     3,     2,     1,     1,     1,
       1,     1,     4,     0,     5,     2,     3,     4,     5,     4,
       5,     2,     2,     2,     2,     2,     1,     3,     1,     3,
       1,     2,     3,     5,     2,     4,     2,     4,     1,     3,
       1,     3,     2,     3,     1,     2,     1,     4,     3,     3,
       3,     3,     2,     1,     1,     4,     3,     3,     3,     3,
       2,     1,     1,     1,     2,     1,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     4,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     5,     3,     5,
       6,     5,     5,     5,     5,     4,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     4,     4,     2,     2,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     2,     2,     3,     3,     3,     3,     6,     1,
       1,     1,     2,     4,     2,     3,     1,     1,     1,     1,
       2,     4,     2,     1,     2,     2,     4,     1,     0,     2,
       2,     2,     1,     1,     2,     3,     4,     4,     5,     3,
       4,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     4,     0,     0,     5,     0,     3,     3,     3,     2,
       3,     3,     1,     4,     3,     1,     4,     3,     2,     1,
       2,     0,     0,     5,     6,     6,     0,     0,     7,     0,
       0,     7,     5,     4,     0,     0,     9,     0,     6,     0,
       0,     8,     0,     5,     0,     0,     7,     0,     0,     9,
       1,     1,     1,     1,     1,     1,     1,     2,     1,     1,
       1,     5,     1,     2,     1,     1,     1,     3,     1,     3,
       1,     4,     6,     3,     5,     2,     4,     1,     3,     6,
       8,     4,     6,     4,     2,     6,     2,     4,     6,     2,
       4,     2,     4,     1,     1,     1,     3,     1,     4,     1,
       4,     1,     3,     1,     1,     4,     1,     3,     3,     0,
       5,     2,     4,     5,     5,     2,     4,     4,     3,     3,
       3,     2,     1,     4,     0,     5,     0,     5,     5,     1,
       1,     6,     1,     1,     1,     1,     2,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     2,     3,     1,     2,
       1,     0,     4,     1,     2,     2,     3,     2,     3,     1,
       0,     1,     1,     2,     1,     2,     1,     2,     1,     0,
       4,     2,     3,     1,     4,     2,     1,     1,     1,     1,
       1,     2,     2,     3,     1,     1,     2,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     4,     2,     3,     2,     6,
       8,     4,     6,     4,     6,     2,     4,     6,     2,     4,
       2,     4,     1,     0,     1,     1,     1,     1,     1,     1,
       1,     3,     1,     3,     2,     2,     2,     1,     3,     1,
       3,     1,     1,     2,     1,     1,     1,     2,     2,     1,
       1,     0,     4,     1,     2,     1,     3,     3,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     0,     1,     2,     2,     0,     1,     1,
       1,     1,     0,     3,     1,     2,     0
};

/* YYDEFACT[STATE-NAME] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     0,     1,     0,     0,     0,     0,   270,     0,
       0,   296,   299,     0,     0,   541,   320,   321,   322,   323,
     282,   248,   248,   466,   465,   467,   468,   543,     0,    10,
       0,   470,   469,   458,   531,   460,   459,   462,   461,   454,
     455,   414,   415,   471,   472,     0,     0,     0,     0,   272,
     556,   556,    78,   291,     0,     0,     0,     0,     0,     0,
     429,     0,     0,     0,     3,   541,     6,     9,    27,    39,
      42,    50,    49,     0,    66,     0,    70,    80,     0,    47,
     229,     0,    51,   289,   262,   263,   264,   265,   266,   412,
     411,   443,   413,   410,   464,     0,   267,   268,   248,     5,
       8,   320,   321,   282,   285,   392,     0,   102,   103,     0,
       0,     0,     0,   105,     0,   324,     0,   464,   268,     0,
     312,   156,   166,   157,   153,   182,   183,   184,   185,   164,
     179,   172,   162,   161,   177,   160,   159,   155,   180,   154,
     167,   171,   173,   165,   158,   174,   181,   176,   175,   168,
     178,   163,   152,   170,   169,   151,   149,   150,   146,   147,
     148,   107,   109,   108,   141,   142,   138,   120,   121,   122,
     129,   126,   128,   123,   124,   143,   144,   130,   131,   135,
     125,   127,   117,   118,   119,   132,   133,   134,   136,   137,
     139,   140,   145,   521,   314,   110,   111,   520,     0,     0,
       0,    48,     0,     0,     0,   464,     0,   268,     0,     0,
       0,     0,   335,   334,     0,     0,   464,   268,   175,   168,
     178,   163,   146,   147,   107,   108,     0,   112,   114,    20,
     113,   550,   552,   541,     0,   554,   551,   542,     0,     0,
       0,     0,   243,   230,   253,    64,   247,   556,   556,   525,
      65,    63,   543,    62,     0,   556,   391,    61,   543,     0,
     544,    18,     0,     0,   207,     0,   208,   279,     0,     0,
       0,   541,    15,   543,    68,    14,     0,   543,     0,   547,
     547,   231,     0,     0,   547,   523,     0,     0,    76,     0,
      86,    93,   493,   448,   447,   449,   450,     0,   446,   445,
     427,   421,   420,   423,     0,     0,   418,   441,     0,   452,
       0,   416,     0,   425,     0,   456,   457,    46,   222,   223,
       4,   542,     0,     0,     0,     0,     0,     0,     0,   379,
     381,     0,    82,     0,    74,    71,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   556,     0,   540,   539,     0,   396,
     394,   290,     0,     0,   385,    55,   288,   309,   102,   103,
     104,   456,   457,     0,   474,   307,   473,     0,   556,     0,
       0,     0,   315,   317,     0,   556,   279,   326,     0,   325,
       0,     0,   556,     0,     0,     0,     0,     0,     0,   279,
       0,   556,     0,   304,     0,   115,   430,     0,     0,     0,
     555,   528,   254,   250,     0,     0,   244,   252,     0,   245,
     543,     0,   284,   249,   543,   239,   556,   556,   238,   543,
     287,    45,     0,     0,     0,     0,     0,     0,    17,   543,
     277,    13,   542,    67,   273,   276,   280,   549,   232,   548,
     549,   234,   281,   524,    92,    84,     0,    79,     0,     0,
     556,     0,   499,   496,   495,   494,   497,     0,   512,   516,
     515,   511,   493,   292,   376,   498,   500,   502,   556,     0,
     509,   556,   514,   556,     0,   492,   451,     0,     0,   434,
     439,   438,   424,   432,     0,   436,   428,   419,   442,   453,
     417,   426,     0,     0,     7,    21,    22,    23,    24,    25,
      43,    44,   556,     0,    28,    37,     0,    38,   543,     0,
      72,    83,    41,    40,     0,   186,   253,    36,   204,   212,
     217,   218,   219,   214,   216,   226,   227,   220,   221,   197,
     198,   224,   225,   543,   213,   215,   209,   210,   211,   199,
     200,   201,   202,   203,   532,   537,   533,   538,   390,   248,
     388,   543,   532,   534,   533,   535,   389,   248,   532,   533,
     248,   556,   556,    29,   188,    35,   196,    53,    56,     0,
     476,     0,     0,   102,   103,   106,     0,   543,   556,     0,
     543,   493,     0,   271,   556,   556,   402,   556,   327,   186,
     536,   533,   543,   532,   533,   556,   329,   297,   328,   300,
     536,   278,   543,   532,   533,     0,     0,   553,   431,     0,
       0,   303,   527,     0,   255,   251,     0,   556,   526,   283,
     545,   235,   240,   242,   286,    19,     0,    26,   195,    69,
      16,   543,   547,    85,    77,    89,    91,   543,   532,   533,
     504,   499,     0,   347,   338,   340,   543,   336,   543,     0,
       0,   485,   519,   505,     0,   488,   513,     0,   490,   517,
     444,     0,     0,   433,   435,   437,   205,   206,   367,   543,
       0,   365,   364,   261,     0,    81,    75,     0,     0,     0,
       0,     0,     0,   387,    59,     0,   393,     0,     0,   237,
     386,    57,   236,   382,    52,     0,     0,     0,   556,   310,
       0,     0,   393,   313,   522,   493,     0,     0,   318,   403,
     404,   556,   405,     0,   556,   332,     0,     0,   330,     0,
       0,   393,     0,     0,     0,     0,     0,   393,     0,   116,
     302,     0,     0,   256,     0,   257,   246,   556,    11,   274,
     233,    87,   543,     0,   345,     0,   501,     0,   369,     0,
       0,   293,   503,   556,   556,   518,   556,   510,   556,   556,
     422,     0,   543,     0,   556,     0,   507,   556,   556,   363,
       0,     0,   259,    73,   187,     0,    34,   193,    33,   194,
      60,   546,     0,    31,   191,    32,   192,    58,   383,   384,
       0,     0,   189,     0,     0,   475,   308,   543,     0,   478,
     493,     0,     0,   407,   333,     0,    12,   409,     0,   294,
       0,   295,     0,     0,   305,   255,   556,   258,   241,   337,
     348,     0,   343,   339,   375,     0,     0,     0,     0,   481,
       0,   483,     0,   489,     0,   486,   491,   440,     0,   366,
     354,   356,     0,   506,     0,   359,     0,   361,   380,   260,
     228,    30,   190,   397,   395,     0,     0,   477,   316,     0,
       0,   406,     0,    94,   101,     0,   408,     0,   298,   301,
       0,   399,   400,   398,     0,   346,     0,   341,   373,   543,
     371,   374,   378,   377,   556,   556,   556,   556,   368,   556,
     556,   279,     0,   556,   508,   556,   556,    54,   311,     0,
     100,     0,   556,     0,   556,   556,     0,   344,     0,     0,
     370,   482,     0,   479,   484,   487,     0,   351,     0,   353,
     536,   278,   360,     0,   357,   362,   319,   536,    99,   543,
     532,   533,   401,   331,   306,   342,   372,   556,   556,   556,
     556,   556,   393,   480,   352,     0,   349,   355,   358,   556,
     350
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    64,    65,    66,   262,   394,   395,   271,
     272,   442,    68,    69,   202,    70,    71,   588,   718,    72,
      73,   273,    74,    75,    76,   467,    77,   203,   113,   114,
     227,   228,   229,   626,   195,   196,    79,   244,   278,   568,
     710,   434,   435,   253,   254,   246,   426,   436,   527,    80,
     199,   276,   651,   277,   292,   669,   209,   745,   210,   746,
     625,   890,   592,   589,   814,   390,   392,   601,   602,   820,
     265,   398,   617,   737,   738,   215,   664,   665,   666,   782,
     690,   691,   767,   899,   900,   483,   771,   330,   522,    82,
      83,   376,   582,   581,   419,   893,   605,   731,   822,   826,
      84,    85,   305,   306,   498,    86,    87,    88,   627,   636,
     503,   504,   505,   682,    89,    90,    91,   299,    92,    93,
     205,   206,    96,   207,   385,   591,   726,   727,   485,   486,
     487,   488,   489,   490,   786,   787,   491,   492,   493,   494,
     775,   671,   198,   391,   283,   437,   249,   119,   596,   570,
     368,   234,   431,   432,   706,   458,   399,   260,   416,   237,
     275
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -765
static const yytype_int16 yypact[] =
{
    -765,   136,  2528,  -765,  7060,  8868,  9195,  5434,  -765,  8529,
    8529,  -765,  -765,  8977,  6457,  4953,  7738,  7738,  -765,  -765,
    7738,  3149,  2756,  -765,  -765,  -765,  -765,   -20,  6457,  -765,
      28,  -765,  -765,  5571,  2887,  -765,  -765,  5708,  -765,  -765,
    -765,  -765,  -765,  -765,  -765,  8642,  8642,   118,  4354,    -9,
    7851,  8077,  6729,  -765,  6185,   526,   626,   734,   740,   246,
    -765,    86,  8755,  8642,  -765,   350,  -765,   840,  -765,   475,
    -765,  -765,   164,    70,  -765,    60,  9086,  -765,   112,  2997,
       7,   286,    20,    79,  -765,  -765,  -765,  -765,  -765,  -765,
    -765,  -765,  -765,  -765,   390,   171,  -765,   415,    49,  -765,
    -765,  -765,  -765,  -765,   149,   162,   166,   291,   340,  8529,
     215,  4468,   457,  -765,    56,  -765,   382,  -765,  -765,    49,
    -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,
    -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,
    -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,    32,    36,
      39,    53,  -765,  -765,  -765,  -765,  -765,  -765,    65,    75,
    -765,    78,  -765,   163,  -765,  -765,  -765,  -765,  -765,  -765,
    -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,
    -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,
    -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,    20,  3554,
     253,   475,    93,   196,   407,   204,   225,   294,    93,  8529,
    8529,   270,  -765,  -765,   414,   179,    85,   134,  -765,  -765,
    -765,  -765,  -765,  -765,  -765,  -765,  6321,  -765,  -765,   191,
    -765,  -765,  -765,   350,   310,  -765,  -765,   190,  8642,  8642,
    8642,  8642,  -765,  2997,   254,  -765,  -765,   223,   226,  -765,
    -765,  -765,  4839,  -765,  7738,  7738,  -765,  -765,  5067,  8529,
    -765,  -765,   219,  4582,  -765,   434,   309,   429,  7286,  4354,
     248,   350,   840,   251,   282,  -765,  8529,   251,   275,   -15,
     186,  -765,   254,   284,   186,  -765,   349,  9304,   292,   486,
     567,   719,  1015,  -765,  -765,  -765,  -765,   748,  -765,  -765,
    -765,  -765,  -765,  -765,   234,   611,  -765,  -765,   759,  -765,
     764,  -765,   778,  -765,   545,   362,   379,  -765,  -765,  -765,
    -765,  5181,  8529,  8529,  8529,  8529,  7286,  8529,  8529,  -765,
    -765,  8190,  -765,  4354,  6838,   317,  8190,  8642,  8642,  8642,
    8642,  8642,  8642,  8642,  8642,  8642,  8642,  8642,  8642,  8642,
    8642,  8642,  8642,  8642,  8642,  8642,  8642,  8642,  8642,  8642,
    8642,  8642,  8642,  2264,  7738,  9583,  -765,  -765, 10555,  -765,
    -765,  -765,  8755,  8755,  -765,   397,  -765,   475,  -765,   818,
    -765,  -765,  -765,   350,  -765,  -765,  -765,  9664,  7738,  9745,
    3554,  8529,  -765,  -765,   446,   459,   323,  -765,  3697,   482,
    8642,  9826,  7738,  9907,  8642,  8642,  3983,   120,   120,   142,
    9988,  7738, 10069,  -765,   467,  -765,   234,   310,  8303,   494,
    -765,  -765,  -765,  -765,  8642,  6947,  -765,  -765,  7964,  -765,
     251,   392,  -765,  -765,   251,  -765,   410,   420,  -765,   143,
    -765,  -765,  6457,  4097,   400,  9826,  9907,  8642,   840,   251,
    -765,  -765,  5294,   422,   475,  -765,  -765,  7173,  -765,  -765,
    8077,  -765,  -765,  -765,   818,    60,  9304,  -765,  9304, 10150,
    7738, 10231,   452,  -765,  -765,  -765,  -765,   934,  -765,  -765,
    -765,  -765,   628,  -765,  -765,  -765,  -765,  -765,   425,  8642,
    -765,   474,   520,   480,   538,  -765,  -765,   532,  4582,  -765,
    -765,  -765,   234,  -765,   364,  -765,  -765,  -765,  -765,  -765,
    -765,  -765,  8642,  8642,  -765,  -765,  -765,  -765,  -765,  -765,
    -765,  -765,    19,  8642,  -765,   491,   496,  -765,   251,  9304,
     509,  -765,  -765,  -765,   533,  2735,  -765,  -765,   309,  9520,
    9520,  9520,  9520,  1147,  1147, 10575,  3385,  9520,  9520,  3128,
    3128,   458,   458,  3255,  1147,  1147,   413,   413,   663,   263,
     263,   309,   309,   309,  3280,  5935,  3411,  6048,  -765,   162,
    -765,   251,   576,  -765,   588,  -765,  -765,  3018,  -765,  -765,
    2147,    19,    19,  -765,  2604,  -765,  2997,  -765,  -765,   350,
    -765,  8529,  3554,   375,   419,  -765,   162,   251,   162,   650,
     143,  1030,  6593,  -765,  8416,   649,  -765,   583,  -765,  2866,
    5821,  2625,   251,   333,   366,   649,  -765,  -765,  -765,  -765,
     139,   146,   251,   154,   158,  8529,  6457,  -765,   234,   653,
      88,  -765,  -765,  8642,   254,  -765,  7399,   226,  -765,  -765,
    -765,  -765,  6947,  7964,  -765,  -765,   540,  -765,  2997,   -30,
     840,   251,   186,   317,  -765,   375,   419,   251,   144,   153,
    -765,  -765,   934,   300,  -765,   548,   251,  -765,   251,    54,
     628,  -765,  -765,  -765,   628,  -765,  -765,   874,  -765,  -765,
    -765,   543,  4582,  -765,  -765,  -765,   309,   309,  -765,   894,
    4725,  -765,  -765,   556,  7512,  -765,  -765,  9304,  8755,  8642,
     577,  8755,  8755,  -765,   397,   557,   619,  8755,  8755,  -765,
    -765,   397,  -765,    79,   164,  4725,  4582,  8642,    19,  -765,
     350,   684,  -765,  -765,  -765,   628,  3554,   350,  -765,   491,
    -765,   615,  -765,  4240,   693,  -765,  8529,   702,  -765,  8642,
    8642,   534,  8642,  8642,   713,  4725,  4725,   160,   120,  -765,
    -765,  7625,  3840,  -765,  8642,  -765,  -765,   593,  -765,  -765,
    -765,   352,   251,   691,   596,  1108,  -765,   601,   617,  4725,
    4582,  -765,  -765,   624,   625,  -765,   642,  -765,   643,   642,
    -765,   659,   251,   674,   647,  9413,  -765,   664,   665,  -765,
     781,  8642,   671,  -765,  2997,  8642,  -765,  2997,  -765,  2997,
    -765,  -765,  8755,  -765,  2997,  -765,  2997,  -765,  -765,  -765,
     802,   687,  2997,  4582,  3554,  -765,  -765,   251,   803,  -765,
    1030,  9522,    93,  -765,  -765,  4725,  -765,  -765,    93,  -765,
    8642,  -765,   810,   816,  -765,  -765,   130,  -765,  7964,  -765,
     700,   691,   470,  -765,  -765,   951,   824,   715,   628,  -765,
     874,  -765,   874,  -765,   874,  -765,  -765,  -765,   726,  -765,
     628,  -765,   801,   827,   628,  -765,   874,  -765,  -765,   720,
    2997,  -765,  2997,  -765,  -765,   724,   846,  -765,  -765,  3554,
     808,  -765,   833,   567,   719,  3554,  -765,  3697,  -765,  -765,
    4725,  -765,  -765,  -765,   691,   700,   691,   729,  -765,   239,
    -765,  -765,  -765,  -765,   642,   730,   642,   642,  -765,   735,
     750,  -765, 10312,   642,  -765,   751,   642,  -765,  -765,   862,
     818, 10393,  7738, 10474,   459,   583,   876,   700,   691,   951,
    -765,  -765,   874,  -765,  -765,  -765,   628,  -765,   874,  -765,
     753,   754,  -765,   874,  -765,  -765,  -765,    90,   419,   251,
      98,   101,  -765,  -765,  -765,   700,  -765,   642,   642,   761,
     642,   642,   114,  -765,  -765,   874,  -765,  -765,  -765,   642,
    -765
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -765,  -765,  -765,   453,  -765,    29,  -765,  -383,   632,  -765,
      12,  -765,  -306,    45,    17,   -54,  -765,  -572,  -765,    -5,
     886,  -176,   -11,   -63,  -265,  -391,   -18,  1627,   -74,   903,
      -3,   -22,  -765,  -765,  -252,  -765,  1200,  1342,  -765,    -4,
     320,  -341,    72,   -13,  -765,  -369,  -222,    83,  -295,    15,
    -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,
    -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,  -765,
     610,  -183,  -384,    -7,  -547,  -765,  -692,  -689,   264,  -765,
    -510,  -765,  -605,  -765,    10,  -765,  -765,   202,  -765,  -765,
    -765,   -82,  -765,  -765,  -388,  -765,   -14,  -765,  -765,  -765,
    -765,  -765,   991,   667,  -765,  -765,  -765,  -765,  -765,  -178,
    -423,  -765,   438,  -765,  -765,  -765,     3,  -765,  -765,  -765,
    1385,  1729,   944,  1423,  -765,  -765,   132,  -282,  -764,  -102,
    -618,   161,  -644,  -630,  -752,   100,   301,  -765,  -483,  -765,
    -287,   267,  -765,  -765,  -765,    46,  -378,   442,  -325,  -765,
     763,   -23,   -25,  -103,  -532,  -245,     4,   -12,  -765,   -19,
      -2
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -557
static const yytype_int16 yytable[] =
{
      99,   371,   259,   236,   194,   495,   261,   599,   317,   257,
     484,   242,   242,   335,    67,   242,    67,   230,   256,   235,
     115,   115,   466,   571,   619,   406,   429,   208,   115,   629,
     532,   230,   233,   100,   288,   461,   380,   274,   569,   463,
     577,   537,   320,   580,   777,   785,   321,   597,   281,   285,
     638,   298,   772,   236,   201,   201,   635,   383,   734,   635,
     201,   612,   248,   248,   598,   722,   248,   115,   744,   235,
     622,   715,   716,   843,   840,   369,   569,   654,   577,   683,
     741,   901,   638,   769,   783,   375,   -67,   598,   245,   250,
     747,   115,   251,   449,   374,  -324,   280,   284,   688,   247,
     247,   256,   236,   247,   397,   369,   -97,   -81,   366,   397,
     -94,   567,   914,   575,   -96,   374,   575,   -98,   386,   258,
    -466,   598,   457,   232,  -465,   761,   502,  -467,   232,   657,
     -95,  -275,   800,   279,  -275,   567,     3,   575,   696,   807,
     689,  -468,  -324,  -324,   315,   316,   598,   616,   733,   567,
     418,   575,   895,  -470,   377,   440,   367,   528,   567,  -101,
     575,   263,   770,  -469,   -97,   901,  -458,  -100,  -466,   -88,
     453,   -99,  -465,   267,   455,  -467,   384,   858,   -90,   -96,
     327,   328,   370,   -98,   914,   -95,   331,   774,   255,  -468,
     236,   778,   329,   567,   575,   495,   236,   332,   231,   232,
     668,  -470,   843,   466,   413,   683,   788,   927,   813,   721,
     417,  -469,   370,   683,  -458,   429,   785,   567,   777,   575,
     785,   236,   -86,   298,   465,   751,   407,   408,   336,  -532,
     231,   232,   -97,   -97,   772,   231,   232,   235,   628,   955,
     -96,   -96,   772,   -98,   -98,   427,   427,   242,   451,   242,
     242,  -462,   452,   438,   201,   201,   -95,   -95,   274,   236,
     -88,   372,   231,   232,   466,   638,   378,   459,   459,   -90,
     379,   -93,   459,   635,   635,   235,   -89,   524,  -532,   -92,
     448,   -88,   533,   -91,   -88,  -533,   232,   -88,   252,   891,
     -90,   -88,   785,   -90,  -463,   -90,   -90,   -87,   248,  -462,
     248,   255,   115,   439,   441,   258,   793,   313,   396,   730,
     301,   302,   400,   595,   495,   404,   530,  -529,   583,   585,
     -94,   454,   274,   460,   430,   409,   433,   639,   415,   232,
     418,   641,   420,    67,   337,   247,   644,   247,   519,   515,
     516,   517,   518,   818,   499,   424,   649,   500,   501,   115,
     514,   661,   443,   473,   474,   475,   476,   303,   304,   242,
     425,   576,   438,   428,   834,   905,  -530,   201,   201,   201,
     201,   236,   520,   521,   363,   667,   929,   910,  -393,  -458,
     337,   915,   232,   242,   405,   576,   438,   590,   450,   360,
     361,   362,   532,   606,   232,   236,   236,   242,   -66,   576,
     438,  -536,   789,   653,   464,   465,   242,   760,   576,   438,
    -101,   618,   618,   447,   526,   756,   456,   962,   462,   526,
     645,   364,   365,   742,  -529,   695,  -458,  -458,  -462,   468,
    -529,   876,   466,   512,   427,   427,   600,   763,   495,  -100,
    -393,    99,   576,   817,    98,   230,    98,   752,   892,   -96,
     513,    98,    98,   959,   531,    67,   743,    98,    98,    98,
     638,   603,    98,  -536,   650,   242,   465,   576,   438,   635,
     387,   637,   604,  -530,   684,  -462,  -462,   500,   501,  -530,
    -463,   115,   -98,   115,   337,  -393,   672,  -393,  -393,   672,
      98,   672,   231,   232,   587,   401,   919,   724,   608,   350,
     351,   630,   410,   652,    98,   373,   -94,  -278,  -536,   631,
    -536,  -536,   327,   328,  -532,   381,   382,   388,   389,   447,
     692,   661,   445,   473,   474,   475,   476,   -86,   700,   337,
     496,  -101,   640,   495,   647,   756,   357,   358,   359,   360,
     361,   362,   402,   403,   115,  -100,   705,   642,   759,   411,
     412,    98,   -93,    98,  -278,  -278,   704,   643,  -533,   -81,
     667,   764,   670,   766,   711,   703,   -92,   714,   660,   402,
     446,   676,   705,   709,   469,   712,   709,   236,   712,   692,
     692,   949,   358,   359,   360,   361,   362,   705,   300,   679,
     301,   302,   703,   719,   709,   680,   712,   705,   598,   728,
     736,   733,   732,   735,   749,   735,   511,   896,   720,   301,
     302,   674,    81,   735,    81,   116,   116,   677,   236,   204,
     204,   470,   471,   214,   830,   204,   204,   204,  -253,   230,
     204,   808,   705,   694,   465,   427,   201,   303,   304,   885,
     459,    98,   748,   768,   533,   887,   697,   796,   798,   698,
     -95,    98,    98,   803,   805,  -464,   303,   304,    81,   839,
     567,   667,   289,   667,   768,   723,   707,   733,   750,   567,
     201,   575,   204,   506,   758,   301,   302,   780,   708,   472,
     270,   473,   474,   475,   476,   765,   289,   526,   757,   307,
     301,   302,   -96,  -254,    98,   795,    98,    98,   801,   816,
      98,    98,  -464,  -464,   -98,    98,   821,   825,   236,   802,
      98,    98,   115,   -88,   877,   236,   692,   829,    98,   204,
     477,    81,   303,   304,   815,   -90,   478,   479,   831,   823,
     838,   819,   827,   841,   337,   -95,   236,   303,   304,   667,
     897,   844,   661,   270,   473,   474,   475,   476,   871,   350,
     351,   480,   618,   828,   481,   427,   -87,   768,   675,   845,
     678,   848,   850,    98,    98,    98,    98,    98,    98,    98,
      98,   672,   672,    98,   672,    98,   672,   672,    98,   852,
     854,   201,   672,   662,   860,   672,   672,   358,   359,   360,
     361,   362,   667,   857,   667,   859,   868,   309,   301,   302,
     115,   864,   866,   311,   301,   302,    98,  -268,  -255,    81,
     236,   496,   301,   302,    98,    98,   236,   873,   878,   204,
     204,   874,   508,   301,   302,   888,   667,   509,   301,   302,
      98,   889,    98,    98,   735,   776,   115,   894,   779,   902,
      98,   510,   301,   302,    98,   303,   304,   908,    98,   903,
     784,   303,   304,    98,  -268,  -268,   911,  -256,   917,   303,
     304,   918,   204,   920,   204,   204,   928,   932,   204,   204,
     303,   304,   936,    81,   930,   303,   304,   946,    81,    81,
     322,   323,   324,   325,   326,    98,   204,   938,   943,   303,
     304,   954,  -532,  -533,    98,   444,   646,   289,   965,   212,
     713,   270,   672,   672,   672,   672,  -279,   672,   672,   120,
     952,   672,    98,   672,   672,   912,   809,   242,   953,   576,
     438,   921,   606,   735,   705,   661,   762,   473,   474,   475,
     476,    81,   204,   204,   204,   204,    81,   204,   204,   956,
      98,   204,   685,    81,   289,   472,   204,   473,   474,   475,
     476,   197,   879,  -279,  -279,   672,   672,   672,   672,   672,
     909,   393,   388,   389,   497,   270,   477,   672,   922,   923,
       0,   773,   507,   479,   204,   507,     0,   507,     0,   507,
       0,   507,   204,   204,     0,   661,   477,   473,   474,   475,
     476,     0,   478,   479,     0,     0,     0,   480,   204,     0,
      81,   204,   898,     0,   473,   474,   475,   476,    81,   904,
       0,   906,   204,     0,     0,   907,    81,   480,     0,     0,
     481,   204,     0,     0,     0,   913,   662,   916,     0,     0,
     607,     0,   663,    98,    98,     0,     0,   232,   615,     0,
     849,   851,     0,   853,     0,   855,   856,   308,   310,   312,
     314,   861,     0,    81,   865,   867,     0,     0,     0,     0,
       0,     0,    81,     0,     0,     0,   472,    98,   473,   474,
     475,   476,     0,     0,     0,     0,   289,     0,   289,     0,
     204,   472,     0,   473,   474,   475,   476,     0,     0,     0,
       0,     0,     0,   957,     0,     0,     0,   958,     0,   960,
       0,     0,     0,     0,   961,     0,     0,   477,    81,     0,
       0,     0,     0,   478,   479,     0,     0,     0,     0,     0,
       0,     0,   477,     0,    98,     0,   969,     0,   478,   479,
     681,     0,    98,     0,     0,     0,     0,     0,   480,   289,
      98,   481,     0,    98,    98,     0,     0,     0,     0,    98,
      98,     0,     0,   480,   482,     0,   481,    98,    98,   661,
       0,   473,   474,   475,   476,     0,     0,     0,    98,   725,
       0,   931,   933,   934,   935,    98,   937,   939,    98,     0,
     942,     0,   944,   945,     0,     0,     0,    98,    98,     0,
       0,     0,     0,     0,    98,     0,     0,     0,     0,     0,
     662,   204,    81,     0,     0,     0,   842,     0,     0,     0,
       0,    98,    98,     0,     0,     0,   243,   243,   337,     0,
     243,     0,     0,     0,   963,   964,   966,   967,   968,     0,
       0,     0,     0,   350,   351,   204,   970,     0,     0,     0,
       0,     0,     0,     0,    98,   264,   266,     0,     0,     0,
     243,   243,     0,     0,     0,    98,    98,     0,     0,     0,
       0,     0,   318,   319,     0,     0,     0,    98,   355,   356,
     357,   358,   359,   360,   361,   362,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    81,     0,     0,     0,     0,     0,     0,     0,
      81,     0,     0,     0,     0,     0,     0,   289,   204,     0,
       0,   204,   204,     0,   781,     0,     0,   204,   204,     0,
       0,    98,   790,     0,     0,    81,    81,    98,     0,    98,
       0,     0,    98,     0,     0,     0,    81,     0,     0,     0,
       0,     0,     0,    81,     0,     0,   204,   810,   811,     0,
       0,     0,     0,     0,     0,    81,    81,     0,     0,     0,
       0,     0,    81,     0,    98,   824,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   832,   833,    81,
      81,     0,     0,     0,   836,     0,     0,    94,     0,    94,
     117,   117,   117,   282,     0,   863,     0,     0,   216,     0,
       0,   846,   847,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   204,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    81,    81,    97,     0,    97,   118,   118,
       0,   882,     0,    94,     0,    81,   217,   290,   243,   243,
     243,   318,     0,     0,     0,   875,     0,     0,     0,     0,
       0,     0,   243,     0,   243,   243,     0,   886,     0,     0,
       0,   290,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    97,     0,     0,     0,   291,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    81,
       0,     0,     0,     0,     0,    81,    94,    81,     0,   291,
      81,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   924,     0,   925,
       0,     0,   926,     0,     0,     0,     0,     0,     0,     0,
       0,   243,   204,     0,    97,     0,   535,   538,   539,   540,
     541,   542,   543,   544,   545,   546,   547,   548,   549,   550,
     551,   552,   553,   554,   555,   556,   557,   558,   559,   560,
     561,   562,   563,     0,   243,     0,     0,     0,     0,     0,
       0,     0,   584,   586,     0,     0,     0,     0,     0,     0,
     421,   422,   423,     0,    94,     0,     0,     0,   243,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     609,     0,   243,     0,   584,   586,     0,     0,     0,     0,
       0,   243,     0,     0,     0,     0,     0,     0,   243,     0,
       0,     0,    97,     0,   243,   243,     0,     0,   243,    78,
       0,    78,     0,     0,     0,     0,     0,     0,     0,     0,
     213,     0,     0,     0,     0,     0,     0,   648,    94,     0,
       0,     0,     0,    94,    94,     0,     0,   243,     0,     0,
     243,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     243,     0,   290,   525,     0,    78,     0,     0,   536,     0,
       0,     0,     0,     0,     0,     0,    97,     0,     0,   243,
       0,    97,    97,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    94,     0,     0,     0,
     291,    94,   686,   687,     0,     0,     0,     0,    94,   290,
       0,     0,     0,   243,     0,     0,     0,     0,     0,     0,
       0,    95,     0,    95,     0,     0,     0,     0,    78,     0,
       0,     0,     0,     0,    97,     0,     0,     0,     0,    97,
       0,     0,     0,     0,     0,     0,    97,   291,     0,     0,
     536,     0,     0,     0,     0,     0,   632,   634,     0,     0,
     282,     0,     0,     0,     0,    94,     0,    95,     0,     0,
       0,     0,     0,    94,     0,     0,     0,     0,     0,     0,
       0,    94,     0,     0,     0,     0,     0,     0,     0,   634,
       0,     0,   282,     0,   243,     0,     0,     0,     0,     0,
       0,     0,     0,    97,     0,     0,     0,     0,     0,     0,
       0,    97,     0,     0,     0,     0,    78,     0,    94,    97,
       0,   673,     0,   243,     0,     0,   243,    94,     0,     0,
      95,     0,   243,   243,     0,     0,     0,     0,     0,     0,
       0,   290,     0,   290,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   693,    97,     0,     0,     0,
       0,     0,     0,     0,     0,    97,     0,     0,     0,     0,
       0,     0,     0,    94,     0,     0,     0,     0,     0,   291,
      78,   291,     0,     0,   243,    78,    78,     0,   609,   794,
       0,   797,   799,     0,     0,     0,     0,   804,   806,     0,
       0,     0,     0,     0,   290,     0,     0,   812,     0,     0,
       0,    97,     0,     0,     0,     0,     0,     0,    95,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   797,
     799,     0,   804,   806,     0,     0,   729,     0,    78,     0,
       0,   243,   291,    78,   243,     0,     0,     0,     0,     0,
      78,     0,     0,   534,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   753,     0,    94,   755,     0,
       0,     0,     0,     0,   634,   282,     0,     0,     0,     0,
       0,   243,    95,     0,     0,   870,     0,    95,    95,     0,
       0,     0,   872,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    97,     0,    78,     0,     0,
       0,     0,     0,     0,     0,    78,     0,     0,     0,     0,
     872,     0,     0,    78,     0,     0,   792,     0,   243,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      95,     0,     0,     0,     0,    95,     0,     0,     0,     0,
       0,     0,    95,     0,     0,     0,     0,    94,     0,     0,
      78,     0,     0,     0,     0,    94,     0,     0,     0,    78,
       0,     0,   290,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   835,     0,     0,   837,     0,     0,     0,
      94,    94,     0,     0,     0,    97,     0,     0,     0,     0,
       0,    94,     0,    97,     0,     0,     0,     0,    94,    95,
     291,     0,   243,     0,     0,    78,     0,    95,     0,     0,
      94,    94,     0,   869,     0,    95,     0,    94,    97,    97,
       0,     0,     0,     0,     0,     0,     0,  -556,     0,    97,
       0,     0,     0,     0,    94,    94,    97,     0,     0,     0,
    -556,  -556,  -556,  -556,  -556,  -556,     0,  -556,    97,    97,
     117,     0,    95,  -556,  -556,    97,     0,     0,     0,     0,
     282,    95,     0,     0,  -556,  -556,     0,  -556,  -556,  -556,
    -556,  -556,    97,    97,     0,     0,     0,     0,    94,    94,
       0,     0,     0,     0,     0,     0,   883,     0,   118,     0,
      94,     0,     0,     0,     0,     0,     0,     0,     0,    78,
       0,     0,     0,     0,     0,     0,     0,    95,     0,     0,
       0,     0,     0,     0,     0,  -556,    97,    97,     0,     0,
       0,     0,     0,     0,   884,     0,     0,     0,    97,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    94,     0,     0,     0,     0,     0,
      94,     0,    94,     0,     0,    94,     0,     0,     0,     0,
    -556,  -556,     0,  -556,     0,     0,   255,  -556,     0,  -556,
    -556,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    97,     0,     0,     0,     0,     0,    97,    78,
      97,     0,     0,    97,     0,   564,   565,    78,     0,   566,
       0,    95,     0,     0,     0,   534,     0,     0,     0,     0,
       0,     0,     0,   164,   165,   166,   167,   168,   169,   170,
     171,   172,    78,    78,   173,   174,     0,     0,   175,   176,
     177,   178,     0,    78,     0,     0,     0,     0,     0,     0,
      78,     0,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    78,    78,     0,     0,     0,     0,     0,    78,
       0,     0,     0,   180,   181,   182,   183,   184,   185,   186,
     187,   188,   189,     0,   190,   191,    78,    78,     0,     0,
       0,     0,   192,   255,     0,     0,     0,     0,     0,     0,
       0,    95,     0,     0,     0,     0,     0,     0,     0,    95,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      78,    78,     0,     0,    95,    95,     0,     0,   881,     0,
       0,     0,    78,     0,     0,    95,     0,     0,     0,     0,
       0,     0,    95,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    95,    95,     0,     0,     0,     0,
       0,    95,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    95,    95,
       0,     0,     0,     0,     0,     0,    78,     0,     0,     0,
       0,     0,    78,     0,    78,     0,     0,    78,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -556,     4,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,    95,    95,     0,     0,     0,    15,     0,    16,
      17,    18,    19,     0,    95,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,    29,    30,    31,    32,     0,    33,
      34,    35,    36,    37,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,    45,    46,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    95,     0,
       0,     0,     0,     0,    95,     0,    95,    47,     0,    95,
      48,    49,     0,    50,    51,  -278,    52,     0,    53,    54,
      55,    56,    57,    58,    59,     0,     0,    60,  -278,  -278,
    -278,  -278,  -278,  -278,     0,  -278,     0,     0,   717,     0,
       0,     0,  -278,  -278,  -278,     0,     0,    61,    62,    63,
       0,     0,  -278,  -278,     0,  -278,  -278,  -278,  -278,  -278,
    -556,  -556,     0,     0,     0,   337,   338,   339,   340,   341,
     342,   343,   344,   345,   346,   347,   348,   349,     0,     0,
     350,   351,     0,     0,     0,     0,  -278,  -278,  -278,  -278,
    -278,  -278,  -278,  -278,  -278,  -278,  -278,  -278,  -278,     0,
       0,  -278,  -278,  -278,     0,   740,  -278,     0,     0,     0,
       0,   352,  -278,   353,   354,   355,   356,   357,   358,   359,
     360,   361,   362,  -278,     0,     0,     0,     0,     0,     0,
       0,   -99,  -278,  -278,  -278,  -278,  -278,  -278,  -278,  -278,
    -278,  -278,  -278,  -278,     0,     0,  -392,     0,     0,  -278,
    -278,  -278,  -278,     0,     0,  -278,  -278,  -278,  -278,  -392,
    -392,  -392,  -392,  -392,  -392,     0,  -392,     0,     0,   699,
       0,     0,  -392,  -392,  -392,     0,     0,     0,     0,     0,
       0,     0,     0,  -392,  -392,     0,  -392,  -392,  -392,  -392,
    -392,     0,     0,     0,     0,     0,   337,   338,   339,   340,
     341,   342,   343,   344,   345,   346,   347,   348,   349,     0,
       0,   350,   351,     0,     0,     0,     0,  -392,  -392,  -392,
    -392,  -392,  -392,  -392,  -392,  -392,  -392,  -392,  -392,  -392,
       0,     0,  -392,  -392,  -392,     0,     0,  -392,     0,     0,
       0,     0,   352,  -392,   353,   354,   355,   356,   357,   358,
     359,   360,   361,   362,     0,     0,     0,     0,     0,     0,
       0,     0,  -230,  -392,     0,  -392,  -392,  -392,  -392,  -392,
    -392,  -392,  -392,  -392,  -392,     0,     0,  -269,     0,  -392,
    -392,  -392,  -392,  -392,     0,   255,  -392,  -392,  -392,  -392,
    -269,  -269,  -269,  -269,  -269,  -269,     0,  -269,     0,     0,
     699,     0,     0,     0,  -269,  -269,  -269,     0,     0,     0,
       0,     0,     0,     0,  -269,  -269,     0,  -269,  -269,  -269,
    -269,  -269,     0,     0,     0,     0,     0,   337,   338,   339,
     340,   341,   342,   343,   344,   345,   346,   347,   348,   349,
       0,     0,   350,   351,     0,     0,     0,     0,  -269,  -269,
    -269,  -269,  -269,  -269,  -269,  -269,  -269,  -269,  -269,  -269,
    -269,     0,     0,  -269,  -269,  -269,     0,     0,  -269,     0,
       0,     0,     0,   352,  -269,   353,   354,   355,   356,   357,
     358,   359,   360,   361,   362,  -269,     0,     0,     0,     0,
       0,     0,     0,     0,  -269,  -269,  -269,  -269,  -269,  -269,
    -269,  -269,  -269,  -269,  -269,  -269,     0,     0,  -556,     0,
       0,  -269,  -269,  -269,  -269,     0,     0,  -269,  -269,  -269,
    -269,  -556,  -556,  -556,  -556,  -556,  -556,     0,  -556,     0,
       0,     0,     0,     0,  -556,  -556,  -556,     0,     0,     0,
       0,     0,     0,     0,     0,  -556,  -556,     0,  -556,  -556,
    -556,  -556,  -556,     0,     0,     0,     0,     0,   337,   338,
     339,   340,   341,   342,   343,   344,   345,   346,   347,   348,
     349,     0,     0,   350,   351,     0,     0,     0,     0,  -556,
    -556,  -556,  -556,  -556,  -556,  -556,  -556,  -556,  -556,  -556,
    -556,  -556,     0,     0,  -556,  -556,  -556,     0,     0,  -556,
       0,     0,     0,     0,   352,  -556,   353,   354,   355,   356,
     357,   358,   359,   360,   361,   362,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -556,     0,  -556,  -556,  -556,
    -556,  -556,  -556,  -556,  -556,  -556,  -556,     0,     0,  -285,
       0,  -556,  -556,  -556,  -556,  -556,     0,   255,  -556,  -556,
    -556,  -556,  -285,  -285,  -285,  -285,  -285,  -285,     0,  -285,
       0,     0,     0,     0,     0,     0,  -285,  -285,     0,     0,
       0,     0,     0,     0,     0,     0,  -285,  -285,     0,  -285,
    -285,  -285,  -285,  -285,     0,     0,     0,     0,     0,   337,
     338,   339,   340,   341,   342,   343,   344,   345,   346,   347,
    -557,  -557,     0,     0,   350,   351,     0,     0,     0,     0,
    -285,  -285,  -285,  -285,  -285,  -285,  -285,  -285,  -285,  -285,
    -285,  -285,  -285,     0,     0,  -285,  -285,  -285,     0,     0,
    -285,     0,     0,     0,     0,     0,  -285,   353,   354,   355,
     356,   357,   358,   359,   360,   361,   362,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -285,     0,  -285,  -285,
    -285,  -285,  -285,  -285,  -285,  -285,  -285,  -285,     0,     0,
    -536,     0,     0,  -285,  -285,  -285,  -285,     0,   252,  -285,
    -285,  -285,  -285,  -536,  -536,  -536,     0,  -536,  -536,     0,
    -536,     0,     0,     0,     0,     0,  -536,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -536,  -536,     0,
    -536,  -536,  -536,  -536,  -536,     0,   337,   338,   339,   340,
     341,   342,   343,   344,   345,   346,   347,   348,   349,     0,
       0,   350,   351,     0,     0,     0,     0,     0,     0,     0,
       0,  -536,  -536,  -536,  -536,  -536,  -536,  -536,  -536,  -536,
    -536,  -536,  -536,  -536,     0,     0,  -536,  -536,  -536,     0,
     701,     0,   352,     0,   353,   354,   355,   356,   357,   358,
     359,   360,   361,   362,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   -97,  -536,   232,  -536,
    -536,  -536,  -536,  -536,  -536,  -536,  -536,  -536,  -536,     0,
       0,  -278,     0,  -536,  -536,  -536,  -536,   -89,     0,     0,
    -536,     0,  -536,  -536,  -278,  -278,  -278,     0,  -278,  -278,
       0,  -278,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -278,  -278,
       0,  -278,  -278,  -278,  -278,  -278,   337,   338,   339,   340,
     341,   342,   343,   344,     0,   346,   347,     0,     0,     0,
       0,   350,   351,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -278,  -278,  -278,  -278,  -278,  -278,  -278,  -278,
    -278,  -278,  -278,  -278,  -278,     0,     0,  -278,  -278,  -278,
       0,   702,     0,     0,   353,   354,   355,   356,   357,   358,
     359,   360,   361,   362,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   -99,  -278,     0,
    -278,  -278,  -278,  -278,  -278,  -278,  -278,  -278,  -278,  -278,
       0,     0,     0,     0,     0,  -278,  -278,  -278,   -91,     0,
       0,  -278,     0,  -278,  -278,   268,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,  -556,  -556,  -556,
       0,     0,  -556,    15,     0,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
       0,    30,    31,    32,     0,    33,    34,    35,    36,    37,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,    45,    46,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    47,     0,     0,    48,    49,     0,    50,
      51,     0,    52,     0,    53,    54,    55,    56,    57,    58,
      59,     0,     0,    60,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -556,  -556,   268,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       0,     0,  -556,     0,  -556,  -556,    15,     0,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,     0,    30,    31,    32,     0,    33,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,    45,    46,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    47,     0,     0,    48,
      49,     0,    50,    51,     0,    52,     0,    53,    54,    55,
      56,    57,    58,    59,     0,     0,    60,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -556,
    -556,   268,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,  -556,     0,     0,  -556,    15,
    -556,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,     0,    30,    31,    32,
       0,    33,    34,    35,    36,    37,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,    45,
      46,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    47,
       0,     0,    48,    49,     0,    50,    51,     0,    52,     0,
      53,    54,    55,    56,    57,    58,    59,     0,     0,    60,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -556,  -556,   268,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,     0,     0,  -556,     0,
       0,  -556,    15,     0,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,     0,
      30,    31,    32,     0,    33,    34,    35,    36,    37,    38,
       0,    39,    40,    41,     0,     0,    42,     0,     0,    43,
      44,     0,    45,    46,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    47,     0,     0,    48,    49,     0,    50,    51,
       0,    52,     0,    53,    54,    55,    56,    57,    58,    59,
       0,     0,    60,     0,     0,     0,     0,     0,     4,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       0,     0,    61,    62,    63,     0,    15,     0,    16,    17,
      18,    19,     0,     0,     0,  -556,  -556,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,    29,    30,    31,    32,     0,    33,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,    45,    46,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    47,     0,     0,    48,
      49,     0,    50,    51,     0,    52,     0,    53,    54,    55,
      56,    57,    58,    59,     0,     0,    60,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,    62,    63,     0,
       0,  -556,     0,     0,     0,     0,     0,     0,     0,  -556,
    -556,   268,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,  -556,  -556,     0,     0,     0,    15,
       0,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,     0,    30,    31,    32,
       0,    33,    34,    35,    36,    37,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,    45,
      46,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    47,
       0,     0,    48,    49,     0,    50,    51,     0,    52,     0,
      53,    54,    55,    56,    57,    58,    59,     0,     0,    60,
       0,     0,     0,     0,     0,   268,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,    61,
      62,    63,     0,    15,     0,    16,    17,    18,    19,     0,
       0,     0,  -556,  -556,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
       0,    30,    31,    32,     0,    33,    34,    35,    36,    37,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,    45,    46,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    47,     0,     0,   269,    49,     0,    50,
      51,     0,    52,     0,    53,    54,    55,    56,    57,    58,
      59,     0,     0,    60,     0,     0,     0,     0,     0,   268,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,     0,    61,    62,    63,     0,    15,     0,    16,
      17,    18,    19,     0,  -556,     0,  -556,  -556,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,     0,    30,    31,    32,     0,    33,
      34,    35,    36,    37,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,    45,    46,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    47,     0,     0,
      48,    49,     0,    50,    51,     0,    52,     0,    53,    54,
      55,    56,    57,    58,    59,     0,     0,    60,     0,     0,
       0,     0,     0,   268,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,     0,    61,    62,    63,
       0,    15,     0,    16,    17,    18,    19,     0,  -556,     0,
    -556,  -556,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,     0,    30,
      31,    32,     0,    33,    34,    35,    36,    37,    38,     0,
      39,    40,    41,     0,     0,    42,     0,     0,    43,    44,
       0,    45,    46,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    47,     0,     0,    48,    49,     0,    50,    51,     0,
      52,     0,    53,    54,    55,    56,    57,    58,    59,     0,
       0,    60,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,    62,    63,     0,     0,  -556,     0,     0,     0,
       0,     0,     0,     0,  -556,  -556,   268,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,     0,     0,
    -556,     0,     0,     0,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,     0,    30,    31,    32,     0,    33,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,    45,    46,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    47,     0,     0,    48,    49,     0,
      50,    51,     0,    52,     0,    53,    54,    55,    56,    57,
      58,    59,     0,     0,    60,     0,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,    61,    62,    63,     0,    15,     0,
      16,    17,    18,    19,     0,     0,     0,  -556,  -556,    20,
      21,    22,    23,    24,    25,    26,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,     0,
      33,    34,    35,    36,    37,    38,   238,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,    45,    46,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,   111,    49,     0,    50,    51,     0,   239,   240,    53,
      54,    55,    56,    57,    58,    59,     0,     0,    60,     0,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,    61,   241,
      63,     0,    15,     0,    16,    17,    18,    19,     0,     0,
       0,     0,   232,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,     0,    33,    34,    35,    36,    37,    38,
       0,    39,    40,    41,     0,     0,    42,     0,     0,    43,
      44,     0,    45,    46,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   200,     0,     0,   111,    49,     0,    50,    51,
       0,     0,     0,    53,    54,    55,    56,    57,    58,    59,
       0,     0,    60,     0,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,    61,    62,    63,     0,    15,     0,    16,    17,
      18,    19,     0,     0,     0,   231,   232,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,     0,    33,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,    45,    46,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,     0,     0,   111,
      49,     0,    50,    51,     0,     0,     0,    53,    54,    55,
      56,    57,    58,    59,     0,     0,    60,     0,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,    61,    62,    63,     0,
      15,     0,    16,    17,    18,    19,     0,     0,     0,     0,
     232,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,    29,    30,    31,
      32,     0,    33,    34,    35,    36,    37,    38,     0,    39,
      40,    41,     0,     0,    42,     0,     0,    43,    44,     0,
      45,    46,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      47,     0,     0,    48,    49,     0,    50,    51,     0,    52,
       0,    53,    54,    55,    56,    57,    58,    59,     0,     0,
      60,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,     0,
      61,    62,    63,    15,     0,    16,    17,    18,    19,     0,
       0,     0,     0,   420,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
       0,    30,    31,    32,     0,    33,    34,    35,    36,    37,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,    45,    46,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    47,     0,     0,    48,    49,     0,    50,
      51,     0,    52,     0,    53,    54,    55,    56,    57,    58,
      59,     0,     0,    60,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   420,   121,   122,   123,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,   141,   142,   143,
     144,     0,     0,     0,   145,   146,   147,   148,   149,   150,
     151,   152,   153,   154,     0,     0,     0,     0,     0,   155,
     156,   157,   158,   159,   160,   161,   162,    35,    36,   163,
      38,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   164,   165,   166,   167,   168,   169,   170,
     171,   172,     0,     0,   173,   174,     0,     0,   175,   176,
     177,   178,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   180,   181,   182,   183,   184,   185,   186,
     187,   188,   189,     0,   190,   191,     0,     0,     0,     0,
       0,     0,   192,   193,  -529,  -529,  -529,  -529,  -529,  -529,
    -529,  -529,  -529,     0,     0,     0,     0,     0,     0,     0,
    -529,     0,  -529,  -529,  -529,  -529,     0,  -529,     0,     0,
       0,  -529,  -529,  -529,  -529,  -529,  -529,  -529,     0,     0,
    -529,     0,     0,     0,     0,     0,     0,     0,     0,  -529,
    -529,     0,  -529,  -529,  -529,  -529,  -529,  -529,  -529,  -529,
    -529,  -529,     0,     0,  -529,     0,     0,  -529,  -529,     0,
    -529,  -529,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -529,     0,     0,  -529,  -529,     0,  -529,  -529,     0,  -529,
    -529,  -529,  -529,  -529,  -529,  -529,  -529,  -529,     0,     0,
    -529,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -529,  -529,  -529,     0,  -529,     0,     0,     0,     0,     0,
    -529,  -530,  -530,  -530,  -530,  -530,  -530,  -530,  -530,  -530,
       0,     0,     0,     0,     0,     0,     0,  -530,     0,  -530,
    -530,  -530,  -530,     0,  -530,     0,     0,     0,  -530,  -530,
    -530,  -530,  -530,  -530,  -530,     0,     0,  -530,     0,     0,
       0,     0,     0,     0,     0,     0,  -530,  -530,     0,  -530,
    -530,  -530,  -530,  -530,  -530,  -530,  -530,  -530,  -530,     0,
       0,  -530,     0,     0,  -530,  -530,     0,  -530,  -530,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -530,     0,     0,
    -530,  -530,     0,  -530,  -530,     0,  -530,  -530,  -530,  -530,
    -530,  -530,  -530,  -530,  -530,     0,     0,  -530,     0,     0,
       0,     0,     0,     0,  -532,  -532,  -532,  -532,  -532,  -532,
    -532,  -532,  -532,     0,     0,     0,     0,  -530,  -530,  -530,
    -532,  -530,  -532,  -532,  -532,  -532,     0,  -530,     0,     0,
       0,  -532,  -532,  -532,  -532,  -532,  -532,  -532,     0,     0,
    -532,     0,     0,     0,     0,     0,     0,     0,     0,  -532,
    -532,     0,  -532,  -532,  -532,  -532,  -532,  -532,  -532,  -532,
    -532,  -532,     0,     0,  -532,     0,     0,  -532,  -532,     0,
    -532,  -532,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -532,   739,     0,  -532,  -532,     0,  -532,  -532,     0,  -532,
    -532,  -532,  -532,  -532,  -532,  -532,  -532,  -532,     0,     0,
    -532,     0,     0,     0,     0,     0,     0,   -97,  -534,  -534,
    -534,  -534,  -534,  -534,  -534,  -534,  -534,     0,     0,     0,
    -532,  -532,  -532,     0,  -534,     0,  -534,  -534,  -534,  -534,
    -532,     0,     0,     0,     0,  -534,  -534,  -534,  -534,  -534,
    -534,  -534,     0,     0,  -534,     0,     0,     0,     0,     0,
       0,     0,     0,  -534,  -534,     0,  -534,  -534,  -534,  -534,
    -534,  -534,  -534,  -534,  -534,  -534,     0,     0,  -534,     0,
       0,  -534,  -534,     0,  -534,  -534,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -534,     0,     0,  -534,  -534,     0,
    -534,  -534,     0,  -534,  -534,  -534,  -534,  -534,  -534,  -534,
    -534,  -534,     0,     0,  -534,     0,     0,     0,     0,     0,
       0,  -535,  -535,  -535,  -535,  -535,  -535,  -535,  -535,  -535,
       0,     0,     0,     0,  -534,  -534,  -534,  -535,     0,  -535,
    -535,  -535,  -535,     0,  -534,     0,     0,     0,  -535,  -535,
    -535,  -535,  -535,  -535,  -535,     0,     0,  -535,     0,     0,
       0,     0,     0,     0,     0,     0,  -535,  -535,     0,  -535,
    -535,  -535,  -535,  -535,  -535,  -535,  -535,  -535,  -535,     0,
       0,  -535,     0,     0,  -535,  -535,     0,  -535,  -535,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -535,     0,     0,
    -535,  -535,     0,  -535,  -535,     0,  -535,  -535,  -535,  -535,
    -535,  -535,  -535,  -535,  -535,     0,     0,  -535,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -535,  -535,  -535,
       0,     0,     0,     0,     0,     0,     0,  -535,   121,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,   134,   135,   136,   137,   138,   139,   140,   141,   142,
     143,   144,     0,     0,     0,   145,   146,   147,   218,   219,
     220,   221,   152,   153,   154,     0,     0,     0,     0,     0,
     155,   156,   157,   222,   223,   160,   224,   162,   293,   294,
     225,   295,     0,     0,     0,     0,     0,     0,   296,     0,
       0,     0,     0,     0,   164,   165,   166,   167,   168,   169,
     170,   171,   172,     0,     0,   173,   174,     0,     0,   175,
     176,   177,   178,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   179,     0,     0,     0,     0,     0,     0,
     297,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   180,   181,   182,   183,   184,   185,
     186,   187,   188,   189,     0,   190,   191,     0,     0,     0,
       0,     0,     0,   192,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,     0,     0,
       0,   145,   146,   147,   218,   219,   220,   221,   152,   153,
     154,     0,     0,     0,     0,     0,   155,   156,   157,   222,
     223,   160,   224,   162,   293,   294,   225,   295,     0,     0,
       0,     0,     0,     0,   296,     0,     0,     0,     0,     0,
     164,   165,   166,   167,   168,   169,   170,   171,   172,     0,
       0,   173,   174,     0,     0,   175,   176,   177,   178,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   179,
       0,     0,     0,     0,     0,     0,   414,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     180,   181,   182,   183,   184,   185,   186,   187,   188,   189,
       0,   190,   191,     0,     0,     0,     0,     0,     0,   192,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,     0,     0,     0,   145,   146,   147,
     218,   219,   220,   221,   152,   153,   154,     0,     0,     0,
       0,     0,   155,   156,   157,   222,   223,   160,   224,   162,
       0,     0,   225,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   164,   165,   166,   167,
     168,   169,   170,   171,   172,     0,     0,   173,   174,     0,
       0,   175,   176,   177,   178,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   179,     0,     0,   226,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   180,   181,   182,   183,
     184,   185,   186,   187,   188,   189,     0,   190,   191,     0,
       0,     0,     0,     0,     0,   192,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
       0,     0,     0,   145,   146,   147,   218,   219,   220,   221,
     152,   153,   154,     0,     0,     0,     0,     0,   155,   156,
     157,   222,   223,   160,   224,   162,     0,     0,   225,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   164,   165,   166,   167,   168,   169,   170,   171,
     172,     0,     0,   173,   174,     0,     0,   175,   176,   177,
     178,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   179,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   180,   181,   182,   183,   184,   185,   186,   187,
     188,   189,     0,   190,   191,     0,     0,     0,     0,     0,
       0,   192,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,     0,     0,     0,    15,     0,
     101,   102,    18,    19,     0,     0,     0,     0,     0,   103,
     104,   105,    23,    24,    25,    26,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,     0,
      33,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   286,     0,
       0,   111,    49,     0,    50,    51,     0,     0,     0,    53,
      54,    55,    56,    57,    58,    59,     0,     0,    60,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,     0,     0,     0,    15,   112,   101,
     102,    18,    19,     0,     0,     0,   287,     0,   103,   104,
     105,    23,    24,    25,    26,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,     0,    33,
      34,    35,    36,    37,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   286,     0,     0,
     111,    49,     0,    50,    51,     0,     0,     0,    53,    54,
      55,    56,    57,    58,    59,     0,     0,    60,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,     0,     0,     0,    15,   112,   101,   102,
      18,    19,     0,     0,     0,   529,     0,   103,   104,   105,
      23,    24,    25,    26,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,     0,    33,    34,
      35,    36,    37,    38,   238,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,    45,    46,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,     0,     0,   111,
      49,     0,    50,    51,     0,   633,   240,    53,    54,    55,
      56,    57,    58,    59,     0,     0,    60,   499,     0,     0,
     500,   501,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,     0,    61,   241,    63,    15,
       0,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,    30,    31,    32,
       0,    33,    34,    35,    36,    37,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,    45,
      46,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    47,
       0,     0,    48,    49,     0,    50,    51,     0,    52,     0,
      53,    54,    55,    56,    57,    58,    59,     0,     0,    60,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    61,
      62,    63,    15,     0,   101,   102,    18,    19,     0,     0,
       0,     0,     0,   103,   104,   105,    23,    24,    25,    26,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,     0,    33,    34,    35,    36,    37,    38,
     238,    39,    40,    41,     0,     0,    42,     0,     0,    43,
      44,     0,    45,    46,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   200,     0,     0,   111,    49,     0,    50,    51,
       0,   633,     0,    53,    54,    55,    56,    57,    58,    59,
       0,     0,    60,   499,     0,     0,   500,   501,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,     0,
       0,     0,    61,   241,    63,    15,     0,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,     0,    30,    31,    32,     0,    33,    34,    35,
      36,    37,    38,     0,    39,    40,    41,     0,     0,    42,
       0,     0,    43,    44,     0,    45,    46,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    47,     0,     0,    48,    49,
       0,    50,    51,     0,    52,     0,    53,    54,    55,    56,
      57,    58,    59,     0,     0,    60,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,    61,    62,    63,    15,     0,
     101,   102,    18,    19,     0,     0,     0,     0,     0,   103,
     104,   105,    23,    24,    25,    26,     0,     0,   106,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,     0,
      33,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,    45,    46,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,   111,    49,     0,    50,    51,     0,   754,     0,    53,
      54,    55,    56,    57,    58,    59,     0,     0,    60,   499,
       0,     0,   500,   501,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,    61,   241,
      63,    15,     0,   101,   102,    18,    19,     0,     0,     0,
       0,     0,   103,   104,   105,    23,    24,    25,    26,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,     0,    33,    34,    35,    36,    37,    38,     0,
      39,    40,    41,     0,     0,    42,     0,     0,    43,    44,
       0,    45,    46,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   200,     0,     0,   111,    49,     0,    50,    51,     0,
     791,     0,    53,    54,    55,    56,    57,    58,    59,     0,
       0,    60,   499,     0,     0,   500,   501,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    61,   241,    63,    15,     0,   101,   102,    18,    19,
       0,     0,     0,     0,     0,   103,   104,   105,    23,    24,
      25,    26,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,     0,    33,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,    45,    46,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,     0,     0,   111,    49,     0,
      50,    51,     0,   633,     0,    53,    54,    55,    56,    57,
      58,    59,     0,     0,    60,   499,     0,     0,   500,   501,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    61,   241,    63,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,     0,    33,
      34,    35,    36,    37,    38,   238,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,    45,    46,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   200,     0,     0,
     111,    49,     0,    50,    51,     0,   239,   240,    53,    54,
      55,    56,    57,    58,    59,     0,     0,    60,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,    61,   241,    63,
      15,     0,   101,   102,    18,    19,     0,     0,     0,     0,
       0,   103,   104,   105,    23,    24,    25,    26,     0,     0,
     106,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,     0,    33,    34,    35,    36,    37,    38,   238,    39,
      40,    41,     0,     0,    42,     0,     0,    43,    44,     0,
      45,    46,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     200,     0,     0,   111,    49,     0,    50,    51,     0,   239,
       0,    53,    54,    55,    56,    57,    58,    59,     0,     0,
      60,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
      61,   241,    63,    15,     0,   101,   102,    18,    19,     0,
       0,     0,     0,     0,   103,   104,   105,    23,    24,    25,
      26,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,     0,    33,    34,    35,    36,    37,
      38,   238,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,    45,    46,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   200,     0,     0,   111,    49,     0,    50,
      51,     0,     0,   240,    53,    54,    55,    56,    57,    58,
      59,     0,     0,    60,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,    61,   241,    63,    15,     0,   101,   102,
      18,    19,     0,     0,     0,     0,     0,   103,   104,   105,
      23,    24,    25,    26,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,     0,    33,    34,
      35,    36,    37,    38,   238,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,    45,    46,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,     0,     0,   111,
      49,     0,    50,    51,     0,     0,     0,    53,    54,    55,
      56,    57,    58,    59,     0,     0,    60,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    61,   241,    63,    15,
       0,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,   106,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
       0,    33,    34,    35,    36,    37,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,    45,
      46,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   200,
       0,     0,   111,    49,     0,    50,    51,     0,   523,     0,
      53,    54,    55,    56,    57,    58,    59,     0,     0,    60,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    61,
     241,    63,    15,     0,   101,   102,    18,    19,     0,     0,
       0,     0,     0,   103,   104,   105,    23,    24,    25,    26,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,     0,    33,    34,    35,    36,    37,    38,
       0,    39,    40,    41,     0,     0,    42,     0,     0,    43,
      44,     0,    45,    46,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   200,     0,     0,   111,    49,     0,    50,    51,
       0,   239,     0,    53,    54,    55,    56,    57,    58,    59,
       0,     0,    60,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,    61,   241,    63,    15,     0,   101,   102,    18,
      19,     0,     0,     0,     0,     0,   103,   104,   105,    23,
      24,    25,    26,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,     0,    33,    34,    35,
      36,    37,    38,     0,    39,    40,    41,     0,     0,    42,
       0,     0,    43,    44,     0,    45,    46,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   200,     0,     0,   111,    49,
       0,    50,    51,     0,   523,     0,    53,    54,    55,    56,
      57,    58,    59,     0,     0,    60,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,    61,   241,    63,    15,     0,
      16,    17,    18,    19,     0,     0,     0,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,     0,
      33,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,    45,    46,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,   111,    49,     0,    50,    51,     0,     0,     0,    53,
      54,    55,    56,    57,    58,    59,     0,     0,    60,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,    61,    62,
      63,    15,     0,   101,   102,    18,    19,     0,     0,     0,
       0,     0,   103,   104,   105,    23,    24,    25,    26,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,     0,    33,    34,    35,    36,    37,    38,     0,
      39,    40,    41,     0,     0,    42,     0,     0,    43,    44,
       0,    45,    46,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   200,     0,     0,   111,    49,     0,    50,    51,     0,
       0,     0,    53,    54,    55,    56,    57,    58,    59,     0,
       0,    60,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    61,   241,    63,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,     0,    33,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,    45,    46,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,     0,     0,   111,    49,     0,
      50,    51,     0,     0,     0,    53,    54,    55,    56,    57,
      58,    59,     0,     0,    60,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    61,   241,    63,    15,     0,   101,
     102,    18,    19,     0,     0,     0,     0,     0,   103,   104,
     105,    23,    24,    25,    26,     0,     0,   106,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,     0,   107,
      34,    35,    36,   108,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,     0,     0,   110,     0,     0,
     111,    49,     0,    50,    51,     0,     0,     0,    53,    54,
      55,    56,    57,    58,    59,     0,     0,    60,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,     0,     0,     0,    15,   112,   101,   102,
      18,    19,     0,     0,     0,     0,     0,   103,   104,   105,
      23,    24,    25,    26,     0,     0,   106,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,     0,    33,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   211,     0,     0,    48,
      49,     0,    50,    51,     0,    52,     0,    53,    54,    55,
      56,    57,    58,    59,     0,     0,    60,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,     0,     0,     0,    15,   112,   101,   102,    18,
      19,     0,     0,     0,     0,     0,   103,   104,   105,    23,
      24,    25,    26,     0,     0,   106,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,     0,    33,    34,    35,
      36,    37,    38,     0,    39,    40,    41,     0,     0,    42,
       0,     0,    43,    44,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   286,     0,     0,   333,    49,
       0,    50,    51,     0,   334,     0,    53,    54,    55,    56,
      57,    58,    59,     0,     0,    60,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,     0,     0,     0,    15,   112,   101,   102,    18,    19,
       0,     0,     0,     0,     0,   103,   104,   105,    23,    24,
      25,    26,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,     0,   107,    34,    35,    36,
     108,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   110,     0,     0,   111,    49,     0,
      50,    51,     0,     0,     0,    53,    54,    55,    56,    57,
      58,    59,     0,     0,    60,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
       0,     0,     0,    15,   112,   101,   102,    18,    19,     0,
       0,     0,     0,     0,   103,   104,   105,    23,    24,    25,
      26,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,     0,    33,    34,    35,    36,    37,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   286,     0,     0,   333,    49,     0,    50,
      51,     0,     0,     0,    53,    54,    55,    56,    57,    58,
      59,     0,     0,    60,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,     0,
       0,     0,    15,   112,   101,   102,    18,    19,     0,     0,
       0,     0,     0,   103,   104,   105,    23,    24,    25,    26,
       0,     0,   106,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,     0,    33,    34,    35,    36,    37,    38,
       0,    39,    40,    41,     0,     0,    42,     0,     0,    43,
      44,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   862,     0,     0,   111,    49,     0,    50,    51,
       0,     0,     0,    53,    54,    55,    56,    57,    58,    59,
       0,     0,    60,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,     0,     0,
       0,    15,   112,   101,   102,    18,    19,     0,     0,     0,
       0,     0,   103,   104,   105,    23,    24,    25,    26,     0,
       0,   106,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,     0,    33,    34,    35,    36,    37,    38,     0,
      39,    40,    41,     0,     0,    42,     0,     0,    43,    44,
       0,   337,  -557,  -557,  -557,  -557,   342,   343,     0,     0,
    -557,  -557,     0,     0,     0,     0,   350,   351,     0,     0,
       0,   880,     0,     0,   111,    49,     0,    50,    51,     0,
       0,     0,    53,    54,    55,    56,    57,    58,    59,     0,
       0,    60,     0,     0,   572,   573,     0,     0,   574,   353,
     354,   355,   356,   357,   358,   359,   360,   361,   362,     0,
       0,   112,   164,   165,   166,   167,   168,   169,   170,   171,
     172,     0,     0,   173,   174,     0,     0,   175,   176,   177,
     178,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   179,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   180,   181,   182,   183,   184,   185,   186,   187,
     188,   189,     0,   190,   191,   593,   565,     0,     0,   594,
       0,   192,   255,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   164,   165,   166,   167,   168,   169,   170,
     171,   172,     0,     0,   173,   174,     0,     0,   175,   176,
     177,   178,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   180,   181,   182,   183,   184,   185,   186,
     187,   188,   189,     0,   190,   191,   578,   573,     0,     0,
     579,     0,   192,   255,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   164,   165,   166,   167,   168,   169,
     170,   171,   172,     0,     0,   173,   174,     0,     0,   175,
     176,   177,   178,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   179,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   180,   181,   182,   183,   184,   185,
     186,   187,   188,   189,     0,   190,   191,   610,   565,     0,
       0,   611,     0,   192,   255,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   164,   165,   166,   167,   168,
     169,   170,   171,   172,     0,     0,   173,   174,     0,     0,
     175,   176,   177,   178,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   179,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   180,   181,   182,   183,   184,
     185,   186,   187,   188,   189,     0,   190,   191,   613,   573,
       0,     0,   614,     0,   192,   255,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   164,   165,   166,   167,
     168,   169,   170,   171,   172,     0,     0,   173,   174,     0,
       0,   175,   176,   177,   178,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   179,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   180,   181,   182,   183,
     184,   185,   186,   187,   188,   189,     0,   190,   191,   620,
     565,     0,     0,   621,     0,   192,   255,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   164,   165,   166,
     167,   168,   169,   170,   171,   172,     0,     0,   173,   174,
       0,     0,   175,   176,   177,   178,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   179,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   180,   181,   182,
     183,   184,   185,   186,   187,   188,   189,     0,   190,   191,
     623,   573,     0,     0,   624,     0,   192,   255,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   164,   165,
     166,   167,   168,   169,   170,   171,   172,     0,     0,   173,
     174,     0,     0,   175,   176,   177,   178,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   179,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   180,   181,
     182,   183,   184,   185,   186,   187,   188,   189,     0,   190,
     191,   655,   565,     0,     0,   656,     0,   192,   255,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   164,
     165,   166,   167,   168,   169,   170,   171,   172,     0,     0,
     173,   174,     0,     0,   175,   176,   177,   178,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   180,
     181,   182,   183,   184,   185,   186,   187,   188,   189,     0,
     190,   191,   658,   573,     0,     0,   659,     0,   192,   255,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     164,   165,   166,   167,   168,   169,   170,   171,   172,     0,
       0,   173,   174,     0,     0,   175,   176,   177,   178,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   179,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     180,   181,   182,   183,   184,   185,   186,   187,   188,   189,
       0,   190,   191,   940,   565,     0,     0,   941,     0,   192,
     255,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   164,   165,   166,   167,   168,   169,   170,   171,   172,
       0,     0,   173,   174,     0,     0,   175,   176,   177,   178,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   180,   181,   182,   183,   184,   185,   186,   187,   188,
     189,     0,   190,   191,   947,   565,     0,     0,   948,     0,
     192,   255,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   164,   165,   166,   167,   168,   169,   170,   171,
     172,     0,     0,   173,   174,     0,     0,   175,   176,   177,
     178,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   179,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   180,   181,   182,   183,   184,   185,   186,   187,
     188,   189,     0,   190,   191,   950,   573,     0,     0,   951,
       0,   192,   255,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   164,   165,   166,   167,   168,   169,   170,
     171,   172,     0,     0,   173,   174,     0,     0,   175,   176,
     177,   178,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   180,   181,   182,   183,   184,   185,   186,
     187,   188,   189,     0,   190,   191,   578,   573,     0,     0,
     579,     0,   192,   255,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   164,   165,   166,   167,   168,   169,
     170,   171,   172,     0,     0,   173,   174,     0,     0,   175,
     176,   177,   178,     0,     0,     0,   337,   338,   339,   340,
     341,   342,   343,   179,     0,   346,   347,     0,     0,     0,
       0,   350,   351,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   180,   181,   182,   183,   184,   185,
     186,   187,   188,   189,     0,   190,   191,     0,     0,     0,
       0,     0,     0,   192,   353,   354,   355,   356,   357,   358,
     359,   360,   361,   362
};

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-765)))

#define yytable_value_is_error(Yytable_value) \
  (!!((Yytable_value) == (-557)))

static const yytype_int16 yycheck[] =
{
       2,    83,    27,    15,     7,   292,    28,   390,    62,    22,
     292,    16,    17,    76,     2,    20,     4,    14,    22,    15,
       5,     6,   287,   364,   408,   208,   248,    10,    13,   417,
     336,    28,    15,     4,    52,   280,   110,    48,   363,   284,
     365,   336,    65,   368,   674,   689,    65,   388,    50,    51,
     428,    54,   670,    65,     9,    10,   425,     1,   605,   428,
      15,   402,    16,    17,   389,   597,    20,    52,   615,    65,
     411,   581,   582,   765,   763,    26,   401,   468,   403,   502,
     612,   845,   460,    29,   689,    98,   116,   412,    16,    17,
     622,    76,    20,   269,    98,    88,    50,    51,    79,    16,
      17,   105,   114,    20,    16,    26,    16,   137,    88,    16,
      25,   363,   864,   365,    16,   119,   368,    16,   114,   139,
      88,   446,   137,   143,    88,   657,   304,    88,   143,   470,
      16,   140,   704,    50,   143,   387,     0,   389,   529,   711,
     121,    88,   135,   136,    58,    59,   471,    27,    18,   401,
      20,   403,   841,    88,   109,   258,   136,   333,   410,    25,
     412,   133,   108,    88,    25,   929,    88,    25,   136,    25,
     273,    25,   136,    55,   277,   136,   120,   782,    25,    25,
      37,    38,   133,    25,   936,    25,   116,   670,   139,   136,
     202,   674,    28,   445,   446,   482,   208,   137,   142,   143,
     482,   136,   894,   468,    25,   628,   689,   896,   718,   592,
     233,   136,   133,   636,   136,   437,   860,   469,   848,   471,
     864,   233,   137,   226,   287,   137,   209,   210,   116,   139,
     142,   143,   142,   143,   852,   142,   143,   233,   416,   928,
     142,   143,   860,   142,   143,   247,   248,   252,   271,   254,
     255,    88,   271,   255,   209,   210,   142,   143,   269,   271,
     116,    90,   142,   143,   529,   643,    51,   279,   280,   116,
      55,   137,   284,   642,   643,   271,   137,   331,   139,   137,
     268,   137,   336,   137,   140,   139,   143,   143,   139,   836,
     137,   137,   936,   140,    90,   137,   143,   137,   252,   136,
     254,   139,   287,   258,   259,   139,   697,    61,    55,   604,
      64,    65,   116,   387,   601,    90,   334,    26,   372,   373,
     116,   276,   333,   137,   252,    55,   254,   430,   137,   143,
      20,   434,   142,   321,    71,   252,   439,   254,   326,   322,
     323,   324,   325,   726,   110,    91,   449,   113,   114,   334,
     321,    51,   133,    53,    54,    55,    56,   111,   112,   364,
     137,   365,   364,   137,   748,   848,    26,   322,   323,   324,
     325,   383,   327,   328,    88,   477,   137,   860,    26,    88,
      71,   864,   143,   388,    90,   389,   388,   383,   140,   126,
     127,   128,   698,   395,   143,   407,   408,   402,   116,   403,
     402,    26,   689,   466,    55,   468,   411,   652,   412,   411,
     116,   407,   408,    90,   331,   637,   141,   949,   134,   336,
     442,   135,   136,    90,   133,   528,   135,   136,    88,   137,
     139,   814,   697,    71,   436,   437,   391,   137,   725,   116,
      88,   443,   446,   725,     2,   442,     4,   630,   836,   116,
      71,     9,    10,   936,   137,   443,    90,    15,    16,    17,
     838,    15,    20,    88,   452,   470,   529,   471,   470,   838,
      88,   425,    13,   133,   110,   135,   136,   113,   114,   139,
      90,   466,   116,   468,    71,   133,   488,   135,   136,   491,
      48,   493,   142,   143,    97,    88,   879,   600,    16,    86,
      87,   418,    88,   457,    62,    90,   116,    88,   133,    15,
     135,   136,    37,    38,   139,    58,    59,   135,   136,    90,
     522,    51,    88,    53,    54,    55,    56,   137,   553,    71,
      63,   116,   140,   820,   134,   757,   123,   124,   125,   126,
     127,   128,   135,   136,   529,   116,   571,   137,   651,   135,
     136,   109,   137,   111,   135,   136,   569,   137,   139,   137,
     662,   663,   137,   666,   577,   569,   137,   580,   116,   135,
     136,    51,   597,   577,    88,   577,   580,   589,   580,   581,
     582,   922,   124,   125,   126,   127,   128,   612,    62,    51,
      64,    65,   596,   589,   598,    63,   598,   622,   923,   602,
      17,    18,   604,   605,   626,   607,    61,   137,   591,    64,
      65,   137,     2,   615,     4,     5,     6,   137,   630,     9,
      10,   135,   136,    13,    90,    15,    16,    17,   137,   626,
      20,   713,   657,   137,   697,   637,   591,   111,   112,   822,
     652,   199,   625,   668,   698,   828,   137,   701,   702,   116,
     116,   209,   210,   707,   708,    88,   111,   112,    48,   762,
     912,   763,    52,   765,   689,    15,    90,    18,    15,   921,
     625,   923,    62,    62,   134,    64,    65,   134,    90,    51,
      48,    53,    54,    55,    56,   137,    76,   604,   642,    63,
      64,    65,   116,   137,   252,   118,   254,   255,   141,    15,
     258,   259,   135,   136,   116,   263,    91,    14,   720,    90,
     268,   269,   697,   137,   817,   727,   718,    15,   276,   109,
      92,   111,   111,   112,   720,   137,    98,    99,    15,   731,
     137,   727,   734,   137,    71,   116,   748,   111,   112,   841,
     842,   140,    51,   111,    53,    54,    55,    56,   802,    86,
      87,   123,   748,   736,   126,   757,   137,   782,   491,   142,
     493,   137,   137,   321,   322,   323,   324,   325,   326,   327,
     328,   773,   774,   331,   776,   333,   778,   779,   336,   137,
     137,   736,   784,    92,   137,   787,   788,   124,   125,   126,
     127,   128,   894,   134,   896,   121,    15,    63,    64,    65,
     785,   137,   137,    63,    64,    65,   364,    88,   137,   199,
     822,    63,    64,    65,   372,   373,   828,    15,    15,   209,
     210,   134,    63,    64,    65,    15,   928,    63,    64,    65,
     388,    15,   390,   391,   836,   674,   821,   137,   677,    15,
     398,    63,    64,    65,   402,   111,   112,   121,   406,   134,
     689,   111,   112,   411,   135,   136,    55,   137,   134,   111,
     112,    15,   252,    55,   254,   255,   137,   137,   258,   259,
     111,   112,   137,   263,   899,   111,   112,    15,   268,   269,
      40,    41,    42,    43,    44,   443,   276,   137,   137,   111,
     112,    15,   139,   139,   452,   263,   443,   287,   137,    13,
     580,   269,   904,   905,   906,   907,    88,   909,   910,     6,
     924,   913,   470,   915,   916,    88,   714,   922,   925,   923,
     922,    88,   924,   925,   949,    51,   662,    53,    54,    55,
      56,   321,   322,   323,   324,   325,   326,   327,   328,   929,
     498,   331,   504,   333,   334,    51,   336,    53,    54,    55,
      56,     7,   820,   135,   136,   957,   958,   959,   960,   961,
     860,   198,   135,   136,   297,   333,    92,   969,   135,   136,
      -1,   670,   305,    99,   364,   308,    -1,   310,    -1,   312,
      -1,   314,   372,   373,    -1,    51,    92,    53,    54,    55,
      56,    -1,    98,    99,    -1,    -1,    -1,   123,   388,    -1,
     390,   391,    51,    -1,    53,    54,    55,    56,   398,   848,
      -1,   850,   402,    -1,    -1,   854,   406,   123,    -1,    -1,
     126,   411,    -1,    -1,    -1,   864,    92,   866,    -1,    -1,
     398,    -1,    98,   591,   592,    -1,    -1,   143,   406,    -1,
     773,   774,    -1,   776,    -1,   778,   779,    56,    57,    58,
      59,   784,    -1,   443,   787,   788,    -1,    -1,    -1,    -1,
      -1,    -1,   452,    -1,    -1,    -1,    51,   625,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,   466,    -1,   468,    -1,
     470,    51,    -1,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,   932,    -1,    -1,    -1,   936,    -1,   938,
      -1,    -1,    -1,    -1,   943,    -1,    -1,    92,   498,    -1,
      -1,    -1,    -1,    98,    99,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    92,    -1,   682,    -1,   965,    -1,    98,    99,
     498,    -1,   690,    -1,    -1,    -1,    -1,    -1,   123,   529,
     698,   126,    -1,   701,   702,    -1,    -1,    -1,    -1,   707,
     708,    -1,    -1,   123,   139,    -1,   126,   715,   716,    51,
      -1,    53,    54,    55,    56,    -1,    -1,    -1,   726,   139,
      -1,   904,   905,   906,   907,   733,   909,   910,   736,    -1,
     913,    -1,   915,   916,    -1,    -1,    -1,   745,   746,    -1,
      -1,    -1,    -1,    -1,   752,    -1,    -1,    -1,    -1,    -1,
      92,   591,   592,    -1,    -1,    -1,    98,    -1,    -1,    -1,
      -1,   769,   770,    -1,    -1,    -1,    16,    17,    71,    -1,
      20,    -1,    -1,    -1,   957,   958,   959,   960,   961,    -1,
      -1,    -1,    -1,    86,    87,   625,   969,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   802,    45,    46,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,   813,   814,    -1,    -1,    -1,
      -1,    -1,    62,    63,    -1,    -1,    -1,   825,   121,   122,
     123,   124,   125,   126,   127,   128,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   682,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     690,    -1,    -1,    -1,    -1,    -1,    -1,   697,   698,    -1,
      -1,   701,   702,    -1,   682,    -1,    -1,   707,   708,    -1,
      -1,   879,   690,    -1,    -1,   715,   716,   885,    -1,   887,
      -1,    -1,   890,    -1,    -1,    -1,   726,    -1,    -1,    -1,
      -1,    -1,    -1,   733,    -1,    -1,   736,   715,   716,    -1,
      -1,    -1,    -1,    -1,    -1,   745,   746,    -1,    -1,    -1,
      -1,    -1,   752,    -1,   922,   733,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   745,   746,   769,
     770,    -1,    -1,    -1,   752,    -1,    -1,     2,    -1,     4,
       5,     6,     7,    51,    -1,   785,    -1,    -1,    13,    -1,
      -1,   769,   770,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   802,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   813,   814,     2,    -1,     4,     5,     6,
      -1,   821,    -1,    48,    -1,   825,    13,    52,   238,   239,
     240,   241,    -1,    -1,    -1,   813,    -1,    -1,    -1,    -1,
      -1,    -1,   252,    -1,   254,   255,    -1,   825,    -1,    -1,
      -1,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    -1,    -1,    -1,    52,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   879,
      -1,    -1,    -1,    -1,    -1,   885,   111,   887,    -1,    76,
     890,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   885,    -1,   887,
      -1,    -1,   890,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   331,   922,    -1,   111,    -1,   336,   337,   338,   339,
     340,   341,   342,   343,   344,   345,   346,   347,   348,   349,
     350,   351,   352,   353,   354,   355,   356,   357,   358,   359,
     360,   361,   362,    -1,   364,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   372,   373,    -1,    -1,    -1,    -1,    -1,    -1,
     238,   239,   240,    -1,   199,    -1,    -1,    -1,   388,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     400,    -1,   402,    -1,   404,   405,    -1,    -1,    -1,    -1,
      -1,   411,    -1,    -1,    -1,    -1,    -1,    -1,   418,    -1,
      -1,    -1,   199,    -1,   424,   425,    -1,    -1,   428,     2,
      -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    -1,    -1,    -1,    -1,    -1,    -1,   447,   263,    -1,
      -1,    -1,    -1,   268,   269,    -1,    -1,   457,    -1,    -1,
     460,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     470,    -1,   287,   331,    -1,    48,    -1,    -1,   336,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   263,    -1,    -1,   489,
      -1,   268,   269,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   321,    -1,    -1,    -1,
     287,   326,   512,   513,    -1,    -1,    -1,    -1,   333,   334,
      -1,    -1,    -1,   523,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     2,    -1,     4,    -1,    -1,    -1,    -1,   111,    -1,
      -1,    -1,    -1,    -1,   321,    -1,    -1,    -1,    -1,   326,
      -1,    -1,    -1,    -1,    -1,    -1,   333,   334,    -1,    -1,
     418,    -1,    -1,    -1,    -1,    -1,   424,   425,    -1,    -1,
     428,    -1,    -1,    -1,    -1,   390,    -1,    48,    -1,    -1,
      -1,    -1,    -1,   398,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   406,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   457,
      -1,    -1,   460,    -1,   604,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   390,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   398,    -1,    -1,    -1,    -1,   199,    -1,   443,   406,
      -1,   489,    -1,   633,    -1,    -1,   636,   452,    -1,    -1,
     111,    -1,   642,   643,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   466,    -1,   468,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   523,   443,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   452,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   498,    -1,    -1,    -1,    -1,    -1,   466,
     263,   468,    -1,    -1,   694,   268,   269,    -1,   698,   699,
      -1,   701,   702,    -1,    -1,    -1,    -1,   707,   708,    -1,
      -1,    -1,    -1,    -1,   529,    -1,    -1,   717,    -1,    -1,
      -1,   498,    -1,    -1,    -1,    -1,    -1,    -1,   199,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   739,
     740,    -1,   742,   743,    -1,    -1,   604,    -1,   321,    -1,
      -1,   751,   529,   326,   754,    -1,    -1,    -1,    -1,    -1,
     333,    -1,    -1,   336,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   633,    -1,   592,   636,    -1,
      -1,    -1,    -1,    -1,   642,   643,    -1,    -1,    -1,    -1,
      -1,   791,   263,    -1,    -1,   795,    -1,   268,   269,    -1,
      -1,    -1,   802,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   592,    -1,   390,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   398,    -1,    -1,    -1,    -1,
     830,    -1,    -1,   406,    -1,    -1,   694,    -1,   838,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     321,    -1,    -1,    -1,    -1,   326,    -1,    -1,    -1,    -1,
      -1,    -1,   333,    -1,    -1,    -1,    -1,   682,    -1,    -1,
     443,    -1,    -1,    -1,    -1,   690,    -1,    -1,    -1,   452,
      -1,    -1,   697,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   751,    -1,    -1,   754,    -1,    -1,    -1,
     715,   716,    -1,    -1,    -1,   682,    -1,    -1,    -1,    -1,
      -1,   726,    -1,   690,    -1,    -1,    -1,    -1,   733,   390,
     697,    -1,   922,    -1,    -1,   498,    -1,   398,    -1,    -1,
     745,   746,    -1,   791,    -1,   406,    -1,   752,   715,   716,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     0,    -1,   726,
      -1,    -1,    -1,    -1,   769,   770,   733,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,   745,   746,
     785,    -1,   443,    26,    27,   752,    -1,    -1,    -1,    -1,
     838,   452,    -1,    -1,    37,    38,    -1,    40,    41,    42,
      43,    44,   769,   770,    -1,    -1,    -1,    -1,   813,   814,
      -1,    -1,    -1,    -1,    -1,    -1,   821,    -1,   785,    -1,
     825,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   592,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   498,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    88,   813,   814,    -1,    -1,
      -1,    -1,    -1,    -1,   821,    -1,    -1,    -1,   825,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   879,    -1,    -1,    -1,    -1,    -1,
     885,    -1,   887,    -1,    -1,   890,    -1,    -1,    -1,    -1,
     133,   134,    -1,   136,    -1,    -1,   139,   140,    -1,   142,
     143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   879,    -1,    -1,    -1,    -1,    -1,   885,   682,
     887,    -1,    -1,   890,    -1,    51,    52,   690,    -1,    55,
      -1,   592,    -1,    -1,    -1,   698,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    70,    71,    72,    73,    74,    75,
      76,    77,   715,   716,    80,    81,    -1,    -1,    84,    85,
      86,    87,    -1,   726,    -1,    -1,    -1,    -1,    -1,    -1,
     733,    -1,    98,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   745,   746,    -1,    -1,    -1,    -1,    -1,   752,
      -1,    -1,    -1,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,    -1,   130,   131,   769,   770,    -1,    -1,
      -1,    -1,   138,   139,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   682,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   690,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     813,   814,    -1,    -1,   715,   716,    -1,    -1,   821,    -1,
      -1,    -1,   825,    -1,    -1,   726,    -1,    -1,    -1,    -1,
      -1,    -1,   733,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   745,   746,    -1,    -1,    -1,    -1,
      -1,   752,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   769,   770,
      -1,    -1,    -1,    -1,    -1,    -1,   879,    -1,    -1,    -1,
      -1,    -1,   885,    -1,   887,    -1,    -1,   890,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     0,     1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,   813,   814,    -1,    -1,    -1,    19,    -1,    21,
      22,    23,    24,    -1,   825,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    -1,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   879,    -1,
      -1,    -1,    -1,    -1,   885,    -1,   887,    89,    -1,   890,
      92,    93,    -1,    95,    96,     0,    98,    -1,   100,   101,
     102,   103,   104,   105,   106,    -1,    -1,   109,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    -1,    44,    -1,
      -1,    -1,    27,    28,    29,    -1,    -1,   129,   130,   131,
      -1,    -1,    37,    38,    -1,    40,    41,    42,    43,    44,
     142,   143,    -1,    -1,    -1,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    -1,    -1,
      86,    87,    -1,    -1,    -1,    -1,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    86,    87,    88,    -1,    90,    91,    -1,    -1,    -1,
      -1,   117,    97,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   108,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,    -1,    -1,     0,    -1,    -1,   134,
     135,   136,   137,    -1,    -1,   140,   141,   142,   143,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    -1,    44,
      -1,    -1,    26,    27,    28,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    86,    87,    -1,    -1,    -1,    -1,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    86,    87,    88,    -1,    -1,    91,    -1,    -1,
      -1,    -1,   117,    97,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   137,   117,    -1,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,    -1,    -1,     0,    -1,   133,
     134,   135,   136,   137,    -1,   139,   140,   141,   142,   143,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    -1,
      44,    -1,    -1,    -1,    27,    28,    29,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    86,    87,    -1,    -1,    -1,    -1,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    -1,    -1,    86,    87,    88,    -1,    -1,    91,    -1,
      -1,    -1,    -1,   117,    97,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   108,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,   128,    -1,    -1,     0,    -1,
      -1,   134,   135,   136,   137,    -1,    -1,   140,   141,   142,
     143,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      -1,    -1,    -1,    -1,    26,    27,    28,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    88,    -1,    -1,    91,
      -1,    -1,    -1,    -1,   117,    97,   119,   120,   121,   122,
     123,   124,   125,   126,   127,   128,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   117,    -1,   119,   120,   121,
     122,   123,   124,   125,   126,   127,   128,    -1,    -1,     0,
      -1,   133,   134,   135,   136,   137,    -1,   139,   140,   141,
     142,   143,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    -1,    -1,    -1,    -1,    -1,    27,    28,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    86,    87,    -1,    -1,    -1,    -1,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    86,    87,    88,    -1,    -1,
      91,    -1,    -1,    -1,    -1,    -1,    97,   119,   120,   121,
     122,   123,   124,   125,   126,   127,   128,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   117,    -1,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,    -1,    -1,
       0,    -1,    -1,   134,   135,   136,   137,    -1,   139,   140,
     141,   142,   143,    13,    14,    15,    -1,    17,    18,    -1,
      20,    -1,    -1,    -1,    -1,    -1,    26,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    86,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    86,    87,    88,    -1,
      90,    -1,   117,    -1,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   116,   117,   143,   119,
     120,   121,   122,   123,   124,   125,   126,   127,   128,    -1,
      -1,     0,    -1,   133,   134,   135,   136,   137,    -1,    -1,
     140,    -1,   142,   143,    13,    14,    15,    -1,    17,    18,
      -1,    20,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,
      -1,    40,    41,    42,    43,    44,    71,    72,    73,    74,
      75,    76,    77,    78,    -1,    80,    81,    -1,    -1,    -1,
      -1,    86,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    86,    87,    88,
      -1,    90,    -1,    -1,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   116,   117,    -1,
     119,   120,   121,   122,   123,   124,   125,   126,   127,   128,
      -1,    -1,    -1,    -1,    -1,   134,   135,   136,   137,    -1,
      -1,   140,    -1,   142,   143,     1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      -1,    -1,    18,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      -1,    47,    48,    49,    -1,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,
      96,    -1,    98,    -1,   100,   101,   102,   103,   104,   105,
     106,    -1,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,   130,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,   143,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,    15,    -1,    17,    18,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    -1,    47,    48,    49,    -1,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    98,    -1,   100,   101,   102,
     103,   104,   105,   106,    -1,    -1,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
     143,     1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    15,    -1,    -1,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,
      -1,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,
     100,   101,   102,   103,   104,   105,   106,    -1,    -1,   109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
     130,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,   143,     1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,    15,    -1,
      -1,    18,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,
      47,    48,    49,    -1,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    98,    -1,   100,   101,   102,   103,   104,   105,   106,
      -1,    -1,   109,    -1,    -1,    -1,    -1,    -1,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,   129,   130,   131,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,   142,   143,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    -1,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    98,    -1,   100,   101,   102,
     103,   104,   105,   106,    -1,    -1,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,    -1,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
     143,     1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    14,    15,    -1,    -1,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,
      -1,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,
     100,   101,   102,   103,   104,   105,   106,    -1,    -1,   109,
      -1,    -1,    -1,    -1,    -1,     1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,   129,
     130,   131,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,   142,   143,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      -1,    47,    48,    49,    -1,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,
      96,    -1,    98,    -1,   100,   101,   102,   103,   104,   105,
     106,    -1,    -1,   109,    -1,    -1,    -1,    -1,    -1,     1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,   129,   130,   131,    -1,    19,    -1,    21,
      22,    23,    24,    -1,   140,    -1,   142,   143,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    -1,    47,    48,    49,    -1,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    95,    96,    -1,    98,    -1,   100,   101,
     102,   103,   104,   105,   106,    -1,    -1,   109,    -1,    -1,
      -1,    -1,    -1,     1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,   129,   130,   131,
      -1,    19,    -1,    21,    22,    23,    24,    -1,   140,    -1,
     142,   143,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,
      48,    49,    -1,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      98,    -1,   100,   101,   102,   103,   104,   105,   106,    -1,
      -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,   130,   131,    -1,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,   143,     1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    -1,
      15,    -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    -1,    47,    48,    49,    -1,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,    -1,    -1,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,   129,   130,   131,    -1,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,   142,   143,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    -1,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    98,    99,   100,
     101,   102,   103,   104,   105,   106,    -1,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,   129,   130,
     131,    -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,   143,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    -1,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    -1,    -1,   100,   101,   102,   103,   104,   105,   106,
      -1,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,   129,   130,   131,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,   142,   143,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    -1,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    -1,    -1,   100,   101,   102,
     103,   104,   105,   106,    -1,    -1,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,   129,   130,   131,    -1,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
     143,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    -1,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,
      -1,   100,   101,   102,   103,   104,   105,   106,    -1,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,    -1,
     129,   130,   131,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,   142,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      -1,    47,    48,    49,    -1,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,
      96,    -1,    98,    -1,   100,   101,   102,   103,   104,   105,
     106,    -1,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,   130,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    -1,    -1,    80,    81,    -1,    -1,    84,    85,
      86,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,    -1,   130,   131,    -1,    -1,    -1,    -1,
      -1,    -1,   138,   139,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      19,    -1,    21,    22,    23,    24,    -1,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    -1,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,
      99,   100,   101,   102,   103,   104,   105,   106,    -1,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,   130,   131,    -1,   133,    -1,    -1,    -1,    -1,    -1,
     139,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,
      22,    23,    24,    -1,    26,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    -1,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    95,    96,    -1,    98,    99,   100,   101,
     102,   103,   104,   105,   106,    -1,    -1,   109,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,   129,   130,   131,
      19,   133,    21,    22,    23,    24,    -1,   139,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    -1,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    90,    -1,    92,    93,    -1,    95,    96,    -1,    98,
      99,   100,   101,   102,   103,   104,   105,   106,    -1,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,   116,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
     129,   130,   131,    -1,    19,    -1,    21,    22,    23,    24,
     139,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    -1,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    99,   100,   101,   102,   103,   104,
     105,   106,    -1,    -1,   109,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   129,   130,   131,    19,    -1,    21,
      22,    23,    24,    -1,   139,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    -1,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    95,    96,    -1,    98,    99,   100,   101,
     102,   103,   104,   105,   106,    -1,    -1,   109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   139,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    63,    -1,
      -1,    -1,    -1,    -1,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,    84,
      85,    86,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,    -1,    -1,
     105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,    -1,   130,   131,    -1,    -1,    -1,
      -1,    -1,    -1,   138,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
      -1,    80,    81,    -1,    -1,    84,    85,    86,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     119,   120,   121,   122,   123,   124,   125,   126,   127,   128,
      -1,   130,   131,    -1,    -1,    -1,    -1,    -1,    -1,   138,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    -1,    -1,    80,    81,    -1,
      -1,    84,    85,    86,    87,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,   101,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   119,   120,   121,   122,
     123,   124,   125,   126,   127,   128,    -1,   130,   131,    -1,
      -1,    -1,    -1,    -1,    -1,   138,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    -1,    -1,    80,    81,    -1,    -1,    84,    85,    86,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   119,   120,   121,   122,   123,   124,   125,   126,
     127,   128,    -1,   130,   131,    -1,    -1,    -1,    -1,    -1,
      -1,   138,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    -1,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    -1,    -1,   100,
     101,   102,   103,   104,   105,   106,    -1,    -1,   109,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,   129,    21,
      22,    23,    24,    -1,    -1,    -1,   137,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    -1,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    95,    96,    -1,    -1,    -1,   100,   101,
     102,   103,   104,   105,   106,    -1,    -1,   109,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,   129,    21,    22,
      23,    24,    -1,    -1,    -1,   137,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    -1,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    98,    99,   100,   101,   102,
     103,   104,   105,   106,    -1,    -1,   109,   110,    -1,    -1,
     113,   114,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    -1,   129,   130,   131,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      -1,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,
     100,   101,   102,   103,   104,   105,   106,    -1,    -1,   109,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   129,
     130,   131,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    -1,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    98,    -1,   100,   101,   102,   103,   104,   105,   106,
      -1,    -1,   109,   110,    -1,    -1,   113,   114,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,    -1,   129,   130,   131,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    -1,    47,    48,    49,    -1,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      -1,    95,    96,    -1,    98,    -1,   100,   101,   102,   103,
     104,   105,   106,    -1,    -1,   109,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   129,   130,   131,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    -1,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,   100,
     101,   102,   103,   104,   105,   106,    -1,    -1,   109,   110,
      -1,    -1,   113,   114,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,   129,   130,
     131,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    -1,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      98,    -1,   100,   101,   102,   103,   104,   105,   106,    -1,
      -1,   109,   110,    -1,    -1,   113,   114,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   129,   130,   131,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    -1,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,    -1,    -1,   109,   110,    -1,    -1,   113,   114,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   129,   130,   131,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    -1,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    95,    96,    -1,    98,    99,   100,   101,
     102,   103,   104,   105,   106,    -1,    -1,   109,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,   129,   130,   131,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    -1,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,
      -1,   100,   101,   102,   103,   104,   105,   106,    -1,    -1,
     109,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
     129,   130,   131,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    -1,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,
      96,    -1,    -1,    99,   100,   101,   102,   103,   104,   105,
     106,    -1,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,   129,   130,   131,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    -1,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    -1,    -1,   100,   101,   102,
     103,   104,   105,   106,    -1,    -1,   109,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   129,   130,   131,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      -1,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    -1,
     100,   101,   102,   103,   104,   105,   106,    -1,    -1,   109,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   129,
     130,   131,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    -1,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    98,    -1,   100,   101,   102,   103,   104,   105,   106,
      -1,    -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   129,   130,   131,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    -1,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      -1,    95,    96,    -1,    98,    -1,   100,   101,   102,   103,
     104,   105,   106,    -1,    -1,   109,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   129,   130,   131,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    -1,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    -1,    -1,   100,
     101,   102,   103,   104,   105,   106,    -1,    -1,   109,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,   129,   130,
     131,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    -1,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      -1,    -1,   100,   101,   102,   103,   104,   105,   106,    -1,
      -1,   109,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   129,   130,   131,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    -1,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    -1,    -1,   100,   101,   102,   103,   104,
     105,   106,    -1,    -1,   109,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   129,   130,   131,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    -1,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    86,    -1,    -1,    89,    -1,    -1,
      92,    93,    -1,    95,    96,    -1,    -1,    -1,   100,   101,
     102,   103,   104,   105,   106,    -1,    -1,   109,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,   129,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    -1,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,
      93,    -1,    95,    96,    -1,    98,    -1,   100,   101,   102,
     103,   104,   105,   106,    -1,    -1,   109,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    19,   129,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    -1,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,
      -1,    95,    96,    -1,    98,    -1,   100,   101,   102,   103,
     104,   105,   106,    -1,    -1,   109,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    19,   129,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    -1,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,
      95,    96,    -1,    -1,    -1,   100,   101,   102,   103,   104,
     105,   106,    -1,    -1,   109,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    19,   129,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    -1,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,
      96,    -1,    -1,    -1,   100,   101,   102,   103,   104,   105,
     106,    -1,    -1,   109,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    19,   129,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    -1,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    -1,    -1,   100,   101,   102,   103,   104,   105,   106,
      -1,    -1,   109,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    19,   129,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    -1,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    71,    72,    73,    74,    75,    76,    77,    -1,    -1,
      80,    81,    -1,    -1,    -1,    -1,    86,    87,    -1,    -1,
      -1,    89,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      -1,    -1,   100,   101,   102,   103,   104,   105,   106,    -1,
      -1,   109,    -1,    -1,    51,    52,    -1,    -1,    55,   119,
     120,   121,   122,   123,   124,   125,   126,   127,   128,    -1,
      -1,   129,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    -1,    -1,    80,    81,    -1,    -1,    84,    85,    86,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   119,   120,   121,   122,   123,   124,   125,   126,
     127,   128,    -1,   130,   131,    51,    52,    -1,    -1,    55,
      -1,   138,   139,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    -1,    -1,    80,    81,    -1,    -1,    84,    85,
      86,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,    -1,   130,   131,    51,    52,    -1,    -1,
      55,    -1,   138,   139,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,    84,
      85,    86,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,    -1,   130,   131,    51,    52,    -1,
      -1,    55,    -1,   138,   139,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,
      84,    85,    86,    87,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,    -1,   130,   131,    51,    52,
      -1,    -1,    55,    -1,   138,   139,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    -1,    -1,    80,    81,    -1,
      -1,    84,    85,    86,    87,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   119,   120,   121,   122,
     123,   124,   125,   126,   127,   128,    -1,   130,   131,    51,
      52,    -1,    -1,    55,    -1,   138,   139,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    -1,    -1,    80,    81,
      -1,    -1,    84,    85,    86,    87,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,   120,   121,
     122,   123,   124,   125,   126,   127,   128,    -1,   130,   131,
      51,    52,    -1,    -1,    55,    -1,   138,   139,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    -1,    -1,    80,
      81,    -1,    -1,    84,    85,    86,    87,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,    -1,   130,
     131,    51,    52,    -1,    -1,    55,    -1,   138,   139,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    -1,    -1,
      80,    81,    -1,    -1,    84,    85,    86,    87,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,
     120,   121,   122,   123,   124,   125,   126,   127,   128,    -1,
     130,   131,    51,    52,    -1,    -1,    55,    -1,   138,   139,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
      -1,    80,    81,    -1,    -1,    84,    85,    86,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     119,   120,   121,   122,   123,   124,   125,   126,   127,   128,
      -1,   130,   131,    51,    52,    -1,    -1,    55,    -1,   138,
     139,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    69,    70,    71,    72,    73,    74,    75,    76,    77,
      -1,    -1,    80,    81,    -1,    -1,    84,    85,    86,    87,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      98,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   119,   120,   121,   122,   123,   124,   125,   126,   127,
     128,    -1,   130,   131,    51,    52,    -1,    -1,    55,    -1,
     138,   139,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    -1,    -1,    80,    81,    -1,    -1,    84,    85,    86,
      87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   119,   120,   121,   122,   123,   124,   125,   126,
     127,   128,    -1,   130,   131,    51,    52,    -1,    -1,    55,
      -1,   138,   139,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    -1,    -1,    80,    81,    -1,    -1,    84,    85,
      86,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,    -1,   130,   131,    51,    52,    -1,    -1,
      55,    -1,   138,   139,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,    84,
      85,    86,    87,    -1,    -1,    -1,    71,    72,    73,    74,
      75,    76,    77,    98,    -1,    80,    81,    -1,    -1,    -1,
      -1,    86,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,    -1,   130,   131,    -1,    -1,    -1,
      -1,    -1,    -1,   138,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   145,   146,     0,     1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    19,    21,    22,    23,    24,
      30,    31,    32,    33,    34,    35,    36,    39,    45,    46,
      47,    48,    49,    51,    52,    53,    54,    55,    56,    58,
      59,    60,    63,    66,    67,    69,    70,    89,    92,    93,
      95,    96,    98,   100,   101,   102,   103,   104,   105,   106,
     109,   129,   130,   131,   147,   148,   149,   154,   156,   157,
     159,   160,   163,   164,   166,   167,   168,   170,   171,   180,
     193,   214,   233,   234,   244,   245,   249,   250,   251,   258,
     259,   260,   262,   263,   264,   265,   266,   267,   291,   304,
     149,    21,    22,    30,    31,    32,    39,    51,    55,    86,
      89,    92,   129,   172,   173,   193,   214,   264,   267,   291,
     173,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    45,    46,    47,    48,    49,
      50,    51,    52,    55,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    80,    81,    84,    85,    86,    87,    98,
     119,   120,   121,   122,   123,   124,   125,   126,   127,   128,
     130,   131,   138,   139,   174,   178,   179,   266,   286,   194,
      89,   157,   158,   171,   214,   264,   265,   267,   158,   200,
     202,    89,   164,   171,   214,   219,   264,   267,    33,    34,
      35,    36,    48,    49,    51,    55,   101,   174,   175,   176,
     260,   142,   143,   158,   295,   300,   301,   303,    57,    98,
      99,   130,   163,   180,   181,   186,   189,   191,   289,   290,
     186,   186,   139,   187,   188,   139,   183,   187,   139,   296,
     301,   175,   150,   133,   180,   214,   180,    55,     1,    92,
     152,   153,   154,   165,   166,   304,   195,   197,   182,   191,
     289,   304,   181,   288,   289,   304,    89,   137,   170,   214,
     264,   267,   198,    53,    54,    56,    63,   105,   174,   261,
      62,    64,    65,   111,   112,   246,   247,    63,   246,    63,
     246,    63,   246,    61,   246,    58,    59,   159,   180,   180,
     295,   303,    40,    41,    42,    43,    44,    37,    38,    28,
     231,   116,   137,    92,    98,   167,   116,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      86,    87,   117,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,    88,   135,   136,    88,   136,   294,    26,
     133,   235,    90,    90,   183,   187,   235,   157,    51,    55,
     172,    58,    59,     1,   120,   268,   300,    88,   135,   136,
     209,   287,   210,   294,   151,   152,    55,    16,   215,   300,
     116,    88,   135,   136,    90,    90,   215,   158,   158,    55,
      88,   135,   136,    25,   105,   137,   302,   295,    20,   238,
     142,   181,   181,   181,    91,   137,   190,   304,   137,   190,
     186,   296,   297,   186,   185,   186,   191,   289,   304,   157,
     297,   157,   155,   133,   152,    88,   136,    90,   154,   165,
     140,   295,   303,   297,   157,   297,   141,   137,   299,   301,
     137,   299,   134,   299,    55,   167,   168,   169,   137,    88,
     135,   136,    51,    53,    54,    55,    56,    92,    98,    99,
     123,   126,   139,   229,   271,   272,   273,   274,   275,   276,
     277,   280,   281,   282,   283,   284,    63,   247,   248,   110,
     113,   114,   253,   254,   255,   256,    62,   247,    63,    63,
      63,    61,    71,    71,   149,   158,   158,   158,   158,   154,
     157,   157,   232,    98,   159,   181,   191,   192,   165,   137,
     170,   137,   156,   159,   171,   180,   181,   192,   180,   180,
     180,   180,   180,   180,   180,   180,   180,   180,   180,   180,
     180,   180,   180,   180,   180,   180,   180,   180,   180,   180,
     180,   180,   180,   180,    51,    52,    55,   178,   183,   292,
     293,   185,    51,    52,    55,   178,   183,   292,    51,    55,
     292,   237,   236,   159,   180,   159,   180,    97,   161,   207,
     300,   269,   206,    51,    55,   172,   292,   185,   292,   151,
     157,   211,   212,    15,    13,   240,   304,   152,    16,   180,
      51,    55,   185,    51,    55,   152,    27,   216,   300,   216,
      51,    55,   185,    51,    55,   204,   177,   252,   253,   238,
     191,    15,   181,    98,   181,   189,   253,   289,   290,   297,
     140,   297,   137,   137,   297,   175,   147,   134,   180,   297,
     154,   196,   289,   167,   169,    51,    55,   185,    51,    55,
     116,    51,    92,    98,   220,   221,   222,   273,   271,   199,
     137,   285,   304,   181,   137,   285,    51,   137,   285,    51,
      63,   152,   257,   254,   110,   256,   180,   180,    79,   121,
     224,   225,   304,   181,   137,   297,   169,   137,   116,    44,
     296,    90,    90,   183,   187,   296,   298,    90,    90,   183,
     184,   187,   304,   184,   187,   224,   224,    44,   162,   300,
     158,   151,   298,    15,   297,   139,   270,   271,   174,   181,
     192,   241,   304,    18,   218,   304,    17,   217,   218,    90,
      90,   298,    90,    90,   218,   201,   203,   298,   158,   175,
      15,   137,   215,   181,    98,   181,   190,   289,   134,   297,
     299,   298,   222,   137,   273,   137,   297,   226,   296,    29,
     108,   230,   274,   280,   282,   284,   275,   277,   282,   275,
     134,   152,   223,   226,   275,   276,   278,   279,   282,   284,
     152,    98,   181,   169,   180,   118,   159,   180,   159,   180,
     161,   141,    90,   159,   180,   159,   180,   161,   235,   231,
     152,   152,   180,   224,   208,   300,    15,   271,   151,   300,
     213,    91,   242,   304,   152,    14,   243,   304,   158,    15,
      90,    15,   152,   152,   216,   181,   152,   181,   137,   297,
     221,   137,    98,   220,   140,   142,   152,   152,   137,   285,
     137,   285,   137,   285,   137,   285,   285,   134,   226,   121,
     137,   285,    89,   214,   137,   285,   137,   285,    15,   181,
     180,   159,   180,    15,   134,   152,   151,   297,    15,   270,
      89,   171,   214,   264,   267,   215,   152,   215,    15,    15,
     205,   218,   238,   239,   137,   221,   137,   273,    51,   227,
     228,   272,    15,   134,   275,   282,   275,   275,   121,   279,
     282,    55,    88,   275,   278,   282,   275,   134,    15,   151,
      55,    88,   135,   136,   152,   152,   152,   221,   137,   137,
     296,   285,   137,   285,   285,   285,   137,   285,   137,   285,
      51,    55,   285,   137,   285,   285,    15,    51,    55,   185,
      51,    55,   240,   217,    15,   221,   228,   275,   275,   282,
     275,   275,   298,   285,   285,   137,   285,   285,   285,   275,
     285
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  However,
   YYFAIL appears to be in use.  Nevertheless, it is formally deprecated
   in Bison 2.4.2's NEWS entry, where a plan to phase it out is
   discussed.  */

#define YYFAIL		goto yyerrlab
#if defined YYFAIL
  /* This is here to suppress warnings from the GCC cpp's
     -Wunused-macros.  Normally we don't worry about that warning, but
     some users do, and we want to make it easy for users to remove
     YYFAIL uses, which will produce warnings from Bison 2.5.  */
#endif

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (p, YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))

/* Error token number */
#define YYTERROR	1
#define YYERRCODE	256


/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */
#ifdef YYLEX_PARAM
# define YYLEX yylex (&yylval, YYLEX_PARAM)
#else
# define YYLEX yylex (&yylval, p)
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value, p); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, parser_state *p)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep, p)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    parser_state *p;
#endif
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
  YYUSE (p);
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
        break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, parser_state *p)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep, p)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    parser_state *p;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep, p);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule, parser_state *p)
#else
static void
yy_reduce_print (yyvsp, yyrule, p)
    YYSTYPE *yyvsp;
    int yyrule;
    parser_state *p;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       , p);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule, p); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULL, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULL;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - Assume YYFAIL is not used.  It's too flawed to consider.  See
       <http://lists.gnu.org/archive/html/bison-patches/2009-12/msg00024.html>
       for details.  YYERROR is fine as it does not invoke this
       function.
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULL, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, parser_state *p)
#else
static void
yydestruct (yymsg, yytype, yyvaluep, p)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
    parser_state *p;
#endif
{
  YYUSE (yyvaluep);
  YYUSE (p);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
        break;
    }
}




/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (parser_state *p)
#else
int
yyparse (p)
    parser_state *p;
#endif
#endif
{
/* The lookahead symbol.  */
int yychar;


#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
static YYSTYPE yyval_default;
# define YY_INITIAL_VALUE(Value) = Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval YY_INITIAL_VALUE(yyval_default);

    /* Number of syntax errors so far.  */
    int yynerrs;

    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
/* Line 1792 of yacc.c  */
#line 1167 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      p->lstate = EXPR_BEG;
                      if (!p->locals) p->locals = cons(0,0);
                    }
    break;

  case 3:
/* Line 1792 of yacc.c  */
#line 1172 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      p->tree = new_scope(p, (yyvsp[(2) - (2)].nd));
                      NODE_LINENO(p->tree, (yyvsp[(2) - (2)].nd));
                    }
    break;

  case 4:
/* Line 1792 of yacc.c  */
#line 1179 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (2)].nd);
                    }
    break;

  case 5:
/* Line 1792 of yacc.c  */
#line 1185 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_begin(p, 0);
                    }
    break;

  case 6:
/* Line 1792 of yacc.c  */
#line 1189 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_begin(p, (yyvsp[(1) - (1)].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (1)].nd));
                    }
    break;

  case 7:
/* Line 1792 of yacc.c  */
#line 1194 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), newline_node((yyvsp[(3) - (3)].nd)));
                    }
    break;

  case 8:
/* Line 1792 of yacc.c  */
#line 1198 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_begin(p, 0);
                    }
    break;

  case 10:
/* Line 1792 of yacc.c  */
#line 1205 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = local_switch(p);
                    }
    break;

  case 11:
/* Line 1792 of yacc.c  */
#line 1209 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      yyerror(p, "BEGIN not supported");
                      local_resume(p, (yyvsp[(2) - (5)].nd));
                      (yyval.nd) = 0;
                    }
    break;

  case 12:
/* Line 1792 of yacc.c  */
#line 1220 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      if ((yyvsp[(2) - (4)].nd)) {
                        (yyval.nd) = new_rescue(p, (yyvsp[(1) - (4)].nd), (yyvsp[(2) - (4)].nd), (yyvsp[(3) - (4)].nd));
                        NODE_LINENO((yyval.nd), (yyvsp[(1) - (4)].nd));
                      }
                      else if ((yyvsp[(3) - (4)].nd)) {
                        yywarn(p, "else without rescue is useless");
                        (yyval.nd) = push((yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].nd));
                      }
                      else {
                        (yyval.nd) = (yyvsp[(1) - (4)].nd);
                      }
                      if ((yyvsp[(4) - (4)].nd)) {
                        if ((yyval.nd)) {
                          (yyval.nd) = new_ensure(p, (yyval.nd), (yyvsp[(4) - (4)].nd));
                        }
                        else {
                          (yyval.nd) = push((yyvsp[(4) - (4)].nd), new_nil(p));
                        }
                      }
                    }
    break;

  case 13:
/* Line 1792 of yacc.c  */
#line 1244 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (2)].nd);
                    }
    break;

  case 14:
/* Line 1792 of yacc.c  */
#line 1250 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_begin(p, 0);
                    }
    break;

  case 15:
/* Line 1792 of yacc.c  */
#line 1254 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_begin(p, (yyvsp[(1) - (1)].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (1)].nd));
                    }
    break;

  case 16:
/* Line 1792 of yacc.c  */
#line 1259 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), newline_node((yyvsp[(3) - (3)].nd)));
                    }
    break;

  case 17:
/* Line 1792 of yacc.c  */
#line 1263 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_begin(p, (yyvsp[(2) - (2)].nd));
                    }
    break;

  case 18:
/* Line 1792 of yacc.c  */
#line 1268 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {p->lstate = EXPR_FNAME;}
    break;

  case 19:
/* Line 1792 of yacc.c  */
#line 1269 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_alias(p, (yyvsp[(2) - (4)].id), (yyvsp[(4) - (4)].id));
                    }
    break;

  case 20:
/* Line 1792 of yacc.c  */
#line 1273 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    }
    break;

  case 21:
/* Line 1792 of yacc.c  */
#line 1277 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_if(p, cond((yyvsp[(3) - (3)].nd)), (yyvsp[(1) - (3)].nd), 0);
                    }
    break;

  case 22:
/* Line 1792 of yacc.c  */
#line 1281 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[(3) - (3)].nd)), (yyvsp[(1) - (3)].nd), 0);
                    }
    break;

  case 23:
/* Line 1792 of yacc.c  */
#line 1285 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_while(p, cond((yyvsp[(3) - (3)].nd)), (yyvsp[(1) - (3)].nd));
                    }
    break;

  case 24:
/* Line 1792 of yacc.c  */
#line 1289 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_until(p, cond((yyvsp[(3) - (3)].nd)), (yyvsp[(1) - (3)].nd));
                    }
    break;

  case 25:
/* Line 1792 of yacc.c  */
#line 1293 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_rescue(p, (yyvsp[(1) - (3)].nd), list1(list3(0, 0, (yyvsp[(3) - (3)].nd))), 0);
                    }
    break;

  case 26:
/* Line 1792 of yacc.c  */
#line 1297 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      yyerror(p, "END not suported");
                      (yyval.nd) = new_postexe(p, (yyvsp[(3) - (4)].nd));
                    }
    break;

  case 28:
/* Line 1792 of yacc.c  */
#line 1303 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 29:
/* Line 1792 of yacc.c  */
#line 1307 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[(1) - (3)].nd), (yyvsp[(2) - (3)].id), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 30:
/* Line 1792 of yacc.c  */
#line 1311 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[(1) - (6)].nd), intern("[]",2), (yyvsp[(3) - (6)].nd)), (yyvsp[(5) - (6)].id), (yyvsp[(6) - (6)].nd));
                    }
    break;

  case 31:
/* Line 1792 of yacc.c  */
#line 1315 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), 0), (yyvsp[(4) - (5)].id), (yyvsp[(5) - (5)].nd));
                    }
    break;

  case 32:
/* Line 1792 of yacc.c  */
#line 1319 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), 0), (yyvsp[(4) - (5)].id), (yyvsp[(5) - (5)].nd));
                    }
    break;

  case 33:
/* Line 1792 of yacc.c  */
#line 1323 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = 0;
                    }
    break;

  case 34:
/* Line 1792 of yacc.c  */
#line 1328 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), 0), (yyvsp[(4) - (5)].id), (yyvsp[(5) - (5)].nd));
                    }
    break;

  case 35:
/* Line 1792 of yacc.c  */
#line 1332 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      backref_error(p, (yyvsp[(1) - (3)].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
    break;

  case 36:
/* Line 1792 of yacc.c  */
#line 1337 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_asgn(p, (yyvsp[(1) - (3)].nd), new_array(p, (yyvsp[(3) - (3)].nd)));
                    }
    break;

  case 37:
/* Line 1792 of yacc.c  */
#line 1341 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 38:
/* Line 1792 of yacc.c  */
#line 1345 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[(1) - (3)].nd), new_array(p, (yyvsp[(3) - (3)].nd)));
                    }
    break;

  case 40:
/* Line 1792 of yacc.c  */
#line 1352 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_asgn(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 41:
/* Line 1792 of yacc.c  */
#line 1356 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_asgn(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 43:
/* Line 1792 of yacc.c  */
#line 1364 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_and(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 44:
/* Line 1792 of yacc.c  */
#line 1368 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_or(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 45:
/* Line 1792 of yacc.c  */
#line 1372 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[(3) - (3)].nd)), "!");
                    }
    break;

  case 46:
/* Line 1792 of yacc.c  */
#line 1376 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[(2) - (2)].nd)), "!");
                    }
    break;

  case 48:
/* Line 1792 of yacc.c  */
#line 1383 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      if (!(yyvsp[(1) - (1)].nd)) (yyval.nd) = new_nil(p);
                      else (yyval.nd) = (yyvsp[(1) - (1)].nd);
                    }
    break;

  case 53:
/* Line 1792 of yacc.c  */
#line 1398 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      local_nest(p);
                    }
    break;

  case 54:
/* Line 1792 of yacc.c  */
#line 1404 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_block(p, (yyvsp[(3) - (5)].nd), (yyvsp[(4) - (5)].nd));
                      local_unnest(p);
                    }
    break;

  case 55:
/* Line 1792 of yacc.c  */
#line 1411 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_fcall(p, (yyvsp[(1) - (2)].id), (yyvsp[(2) - (2)].nd));
                    }
    break;

  case 56:
/* Line 1792 of yacc.c  */
#line 1415 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      args_with_block(p, (yyvsp[(2) - (3)].nd), (yyvsp[(3) - (3)].nd));
                      (yyval.nd) = new_fcall(p, (yyvsp[(1) - (3)].id), (yyvsp[(2) - (3)].nd));
                    }
    break;

  case 57:
/* Line 1792 of yacc.c  */
#line 1420 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].id), (yyvsp[(4) - (4)].nd));
                    }
    break;

  case 58:
/* Line 1792 of yacc.c  */
#line 1424 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      args_with_block(p, (yyvsp[(4) - (5)].nd), (yyvsp[(5) - (5)].nd));
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), (yyvsp[(4) - (5)].nd));
                   }
    break;

  case 59:
/* Line 1792 of yacc.c  */
#line 1429 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].id), (yyvsp[(4) - (4)].nd));
                    }
    break;

  case 60:
/* Line 1792 of yacc.c  */
#line 1433 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      args_with_block(p, (yyvsp[(4) - (5)].nd), (yyvsp[(5) - (5)].nd));
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), (yyvsp[(4) - (5)].nd));
                    }
    break;

  case 61:
/* Line 1792 of yacc.c  */
#line 1438 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_super(p, (yyvsp[(2) - (2)].nd));
                    }
    break;

  case 62:
/* Line 1792 of yacc.c  */
#line 1442 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_yield(p, (yyvsp[(2) - (2)].nd));
                    }
    break;

  case 63:
/* Line 1792 of yacc.c  */
#line 1446 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_return(p, ret_args(p, (yyvsp[(2) - (2)].nd)));
                    }
    break;

  case 64:
/* Line 1792 of yacc.c  */
#line 1450 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_break(p, ret_args(p, (yyvsp[(2) - (2)].nd)));
                    }
    break;

  case 65:
/* Line 1792 of yacc.c  */
#line 1454 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_next(p, ret_args(p, (yyvsp[(2) - (2)].nd)));
                    }
    break;

  case 66:
/* Line 1792 of yacc.c  */
#line 1460 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (1)].nd);
                    }
    break;

  case 67:
/* Line 1792 of yacc.c  */
#line 1464 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (3)].nd);
                    }
    break;

  case 69:
/* Line 1792 of yacc.c  */
#line 1471 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (3)].nd);
                    }
    break;

  case 70:
/* Line 1792 of yacc.c  */
#line 1477 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                    }
    break;

  case 71:
/* Line 1792 of yacc.c  */
#line 1481 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list1(push((yyvsp[(1) - (2)].nd),(yyvsp[(2) - (2)].nd)));
                    }
    break;

  case 72:
/* Line 1792 of yacc.c  */
#line 1485 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list2((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 73:
/* Line 1792 of yacc.c  */
#line 1489 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list3((yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].nd), (yyvsp[(5) - (5)].nd));
                    }
    break;

  case 74:
/* Line 1792 of yacc.c  */
#line 1493 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list2((yyvsp[(1) - (2)].nd), new_nil(p));
                    }
    break;

  case 75:
/* Line 1792 of yacc.c  */
#line 1497 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list3((yyvsp[(1) - (4)].nd), new_nil(p), (yyvsp[(4) - (4)].nd));
                    }
    break;

  case 76:
/* Line 1792 of yacc.c  */
#line 1501 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list2(0, (yyvsp[(2) - (2)].nd));
                    }
    break;

  case 77:
/* Line 1792 of yacc.c  */
#line 1505 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list3(0, (yyvsp[(2) - (4)].nd), (yyvsp[(4) - (4)].nd));
                    }
    break;

  case 78:
/* Line 1792 of yacc.c  */
#line 1509 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list2(0, new_nil(p));
                    }
    break;

  case 79:
/* Line 1792 of yacc.c  */
#line 1513 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list3(0, new_nil(p), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 81:
/* Line 1792 of yacc.c  */
#line 1520 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[(2) - (3)].nd), NULL);
                    }
    break;

  case 82:
/* Line 1792 of yacc.c  */
#line 1526 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (2)].nd));
                    }
    break;

  case 83:
/* Line 1792 of yacc.c  */
#line 1530 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), (yyvsp[(2) - (3)].nd));
                    }
    break;

  case 84:
/* Line 1792 of yacc.c  */
#line 1536 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                    }
    break;

  case 85:
/* Line 1792 of yacc.c  */
#line 1540 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (2)].nd), (yyvsp[(2) - (2)].nd));
                    }
    break;

  case 86:
/* Line 1792 of yacc.c  */
#line 1546 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      assignable(p, (yyvsp[(1) - (1)].nd));
                    }
    break;

  case 87:
/* Line 1792 of yacc.c  */
#line 1550 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (4)].nd), intern("[]",2), (yyvsp[(3) - (4)].nd));
                    }
    break;

  case 88:
/* Line 1792 of yacc.c  */
#line 1554 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id), 0);
                    }
    break;

  case 89:
/* Line 1792 of yacc.c  */
#line 1558 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id), 0);
                    }
    break;

  case 90:
/* Line 1792 of yacc.c  */
#line 1562 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id), 0);
                    }
    break;

  case 91:
/* Line 1792 of yacc.c  */
#line 1566 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id));
                    }
    break;

  case 92:
/* Line 1792 of yacc.c  */
#line 1572 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[(2) - (2)].id));
                    }
    break;

  case 93:
/* Line 1792 of yacc.c  */
#line 1578 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      backref_error(p, (yyvsp[(1) - (1)].nd));
                      (yyval.nd) = 0;
                    }
    break;

  case 94:
/* Line 1792 of yacc.c  */
#line 1585 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      assignable(p, (yyvsp[(1) - (1)].nd));
                    }
    break;

  case 95:
/* Line 1792 of yacc.c  */
#line 1589 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (4)].nd), intern("[]",2), (yyvsp[(3) - (4)].nd));
                    }
    break;

  case 96:
/* Line 1792 of yacc.c  */
#line 1593 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id), 0);
                    }
    break;

  case 97:
/* Line 1792 of yacc.c  */
#line 1597 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id), 0);
                    }
    break;

  case 98:
/* Line 1792 of yacc.c  */
#line 1601 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id), 0);
                    }
    break;

  case 99:
/* Line 1792 of yacc.c  */
#line 1605 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id));
                    }
    break;

  case 100:
/* Line 1792 of yacc.c  */
#line 1611 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[(2) - (2)].id));
                    }
    break;

  case 101:
/* Line 1792 of yacc.c  */
#line 1617 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      backref_error(p, (yyvsp[(1) - (1)].nd));
                      (yyval.nd) = 0;
                    }
    break;

  case 102:
/* Line 1792 of yacc.c  */
#line 1624 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      yyerror(p, "class/module name must be CONSTANT");
                    }
    break;

  case 104:
/* Line 1792 of yacc.c  */
#line 1631 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons((node*)1, nsym((yyvsp[(2) - (2)].id)));
                    }
    break;

  case 105:
/* Line 1792 of yacc.c  */
#line 1635 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons((node*)0, nsym((yyvsp[(1) - (1)].id)));
                    }
    break;

  case 106:
/* Line 1792 of yacc.c  */
#line 1639 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons((yyvsp[(1) - (3)].nd), nsym((yyvsp[(3) - (3)].id)));
                    }
    break;

  case 110:
/* Line 1792 of yacc.c  */
#line 1648 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[(1) - (1)].id);
                    }
    break;

  case 111:
/* Line 1792 of yacc.c  */
#line 1653 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[(1) - (1)].id);
                    }
    break;

  case 114:
/* Line 1792 of yacc.c  */
#line 1664 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_undef(p, (yyvsp[(1) - (1)].id));
                    }
    break;

  case 115:
/* Line 1792 of yacc.c  */
#line 1667 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {p->lstate = EXPR_FNAME;}
    break;

  case 116:
/* Line 1792 of yacc.c  */
#line 1668 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (4)].nd), nsym((yyvsp[(4) - (4)].id)));
                    }
    break;

  case 117:
/* Line 1792 of yacc.c  */
#line 1673 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern_c('|');   }
    break;

  case 118:
/* Line 1792 of yacc.c  */
#line 1674 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern_c('^');   }
    break;

  case 119:
/* Line 1792 of yacc.c  */
#line 1675 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern_c('&');   }
    break;

  case 120:
/* Line 1792 of yacc.c  */
#line 1676 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern("<=>",3); }
    break;

  case 121:
/* Line 1792 of yacc.c  */
#line 1677 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern("==",2);  }
    break;

  case 122:
/* Line 1792 of yacc.c  */
#line 1678 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern("===",3); }
    break;

  case 123:
/* Line 1792 of yacc.c  */
#line 1679 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern("=~",2);  }
    break;

  case 124:
/* Line 1792 of yacc.c  */
#line 1680 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern("!~",2);  }
    break;

  case 125:
/* Line 1792 of yacc.c  */
#line 1681 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern_c('>');   }
    break;

  case 126:
/* Line 1792 of yacc.c  */
#line 1682 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern(">=",2);  }
    break;

  case 127:
/* Line 1792 of yacc.c  */
#line 1683 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern_c('<');   }
    break;

  case 128:
/* Line 1792 of yacc.c  */
#line 1684 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern("<=",2);  }
    break;

  case 129:
/* Line 1792 of yacc.c  */
#line 1685 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern("!=",2);  }
    break;

  case 130:
/* Line 1792 of yacc.c  */
#line 1686 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern("<<",2);  }
    break;

  case 131:
/* Line 1792 of yacc.c  */
#line 1687 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern(">>",2);  }
    break;

  case 132:
/* Line 1792 of yacc.c  */
#line 1688 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern_c('+');   }
    break;

  case 133:
/* Line 1792 of yacc.c  */
#line 1689 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern_c('-');   }
    break;

  case 134:
/* Line 1792 of yacc.c  */
#line 1690 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern_c('*');   }
    break;

  case 135:
/* Line 1792 of yacc.c  */
#line 1691 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern_c('*');   }
    break;

  case 136:
/* Line 1792 of yacc.c  */
#line 1692 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern_c('/');   }
    break;

  case 137:
/* Line 1792 of yacc.c  */
#line 1693 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern_c('%');   }
    break;

  case 138:
/* Line 1792 of yacc.c  */
#line 1694 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern("**",2);  }
    break;

  case 139:
/* Line 1792 of yacc.c  */
#line 1695 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern_c('!');   }
    break;

  case 140:
/* Line 1792 of yacc.c  */
#line 1696 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern_c('~');   }
    break;

  case 141:
/* Line 1792 of yacc.c  */
#line 1697 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern("+@",2);  }
    break;

  case 142:
/* Line 1792 of yacc.c  */
#line 1698 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern("-@",2);  }
    break;

  case 143:
/* Line 1792 of yacc.c  */
#line 1699 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern("[]",2);  }
    break;

  case 144:
/* Line 1792 of yacc.c  */
#line 1700 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern("[]=",3); }
    break;

  case 145:
/* Line 1792 of yacc.c  */
#line 1701 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    { (yyval.id) = intern_c('`');   }
    break;

  case 186:
/* Line 1792 of yacc.c  */
#line 1719 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_asgn(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 187:
/* Line 1792 of yacc.c  */
#line 1723 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_asgn(p, (yyvsp[(1) - (5)].nd), new_rescue(p, (yyvsp[(3) - (5)].nd), list1(list3(0, 0, (yyvsp[(5) - (5)].nd))), 0));
                    }
    break;

  case 188:
/* Line 1792 of yacc.c  */
#line 1727 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[(1) - (3)].nd), (yyvsp[(2) - (3)].id), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 189:
/* Line 1792 of yacc.c  */
#line 1731 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[(1) - (5)].nd), (yyvsp[(2) - (5)].id), new_rescue(p, (yyvsp[(3) - (5)].nd), list1(list3(0, 0, (yyvsp[(5) - (5)].nd))), 0));
                    }
    break;

  case 190:
/* Line 1792 of yacc.c  */
#line 1735 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[(1) - (6)].nd), intern("[]",2), (yyvsp[(3) - (6)].nd)), (yyvsp[(5) - (6)].id), (yyvsp[(6) - (6)].nd));
                    }
    break;

  case 191:
/* Line 1792 of yacc.c  */
#line 1739 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), 0), (yyvsp[(4) - (5)].id), (yyvsp[(5) - (5)].nd));
                    }
    break;

  case 192:
/* Line 1792 of yacc.c  */
#line 1743 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), 0), (yyvsp[(4) - (5)].id), (yyvsp[(5) - (5)].nd));
                    }
    break;

  case 193:
/* Line 1792 of yacc.c  */
#line 1747 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), 0), (yyvsp[(4) - (5)].id), (yyvsp[(5) - (5)].nd));
                    }
    break;

  case 194:
/* Line 1792 of yacc.c  */
#line 1751 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
    break;

  case 195:
/* Line 1792 of yacc.c  */
#line 1756 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
    break;

  case 196:
/* Line 1792 of yacc.c  */
#line 1761 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      backref_error(p, (yyvsp[(1) - (3)].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
    break;

  case 197:
/* Line 1792 of yacc.c  */
#line 1766 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_dot2(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 198:
/* Line 1792 of yacc.c  */
#line 1770 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_dot3(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 199:
/* Line 1792 of yacc.c  */
#line 1774 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "+", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 200:
/* Line 1792 of yacc.c  */
#line 1778 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "-", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 201:
/* Line 1792 of yacc.c  */
#line 1782 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "*", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 202:
/* Line 1792 of yacc.c  */
#line 1786 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "/", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 203:
/* Line 1792 of yacc.c  */
#line 1790 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "%", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 204:
/* Line 1792 of yacc.c  */
#line 1794 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "**", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 205:
/* Line 1792 of yacc.c  */
#line 1798 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_uni_op(p, call_bin_op(p, (yyvsp[(2) - (4)].nd), "**", (yyvsp[(4) - (4)].nd)), "-@");
                    }
    break;

  case 206:
/* Line 1792 of yacc.c  */
#line 1802 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_uni_op(p, call_bin_op(p, (yyvsp[(2) - (4)].nd), "**", (yyvsp[(4) - (4)].nd)), "-@");
                    }
    break;

  case 207:
/* Line 1792 of yacc.c  */
#line 1806 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[(2) - (2)].nd), "+@");
                    }
    break;

  case 208:
/* Line 1792 of yacc.c  */
#line 1810 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[(2) - (2)].nd), "-@");
                    }
    break;

  case 209:
/* Line 1792 of yacc.c  */
#line 1814 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "|", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 210:
/* Line 1792 of yacc.c  */
#line 1818 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "^", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 211:
/* Line 1792 of yacc.c  */
#line 1822 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "&", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 212:
/* Line 1792 of yacc.c  */
#line 1826 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "<=>", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 213:
/* Line 1792 of yacc.c  */
#line 1830 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), ">", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 214:
/* Line 1792 of yacc.c  */
#line 1834 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), ">=", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 215:
/* Line 1792 of yacc.c  */
#line 1838 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "<", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 216:
/* Line 1792 of yacc.c  */
#line 1842 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "<=", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 217:
/* Line 1792 of yacc.c  */
#line 1846 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "==", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 218:
/* Line 1792 of yacc.c  */
#line 1850 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "===", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 219:
/* Line 1792 of yacc.c  */
#line 1854 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "!=", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 220:
/* Line 1792 of yacc.c  */
#line 1858 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "=~", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 221:
/* Line 1792 of yacc.c  */
#line 1862 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "!~", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 222:
/* Line 1792 of yacc.c  */
#line 1866 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[(2) - (2)].nd)), "!");
                    }
    break;

  case 223:
/* Line 1792 of yacc.c  */
#line 1870 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[(2) - (2)].nd)), "~");
                    }
    break;

  case 224:
/* Line 1792 of yacc.c  */
#line 1874 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), "<<", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 225:
/* Line 1792 of yacc.c  */
#line 1878 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[(1) - (3)].nd), ">>", (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 226:
/* Line 1792 of yacc.c  */
#line 1882 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_and(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 227:
/* Line 1792 of yacc.c  */
#line 1886 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_or(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 228:
/* Line 1792 of yacc.c  */
#line 1890 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_if(p, cond((yyvsp[(1) - (6)].nd)), (yyvsp[(3) - (6)].nd), (yyvsp[(6) - (6)].nd));
                    }
    break;

  case 229:
/* Line 1792 of yacc.c  */
#line 1894 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (1)].nd);
                    }
    break;

  case 230:
/* Line 1792 of yacc.c  */
#line 1900 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (1)].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
    break;

  case 232:
/* Line 1792 of yacc.c  */
#line 1908 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (2)].nd);
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (2)].nd));
                    }
    break;

  case 233:
/* Line 1792 of yacc.c  */
#line 1913 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (4)].nd), new_hash(p, (yyvsp[(3) - (4)].nd)));
                    }
    break;

  case 234:
/* Line 1792 of yacc.c  */
#line 1917 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons(new_hash(p, (yyvsp[(1) - (2)].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (2)].nd));
                    }
    break;

  case 235:
/* Line 1792 of yacc.c  */
#line 1924 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (3)].nd);
                    }
    break;

  case 240:
/* Line 1792 of yacc.c  */
#line 1936 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons((yyvsp[(1) - (2)].nd),0);
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (2)].nd));
                    }
    break;

  case 241:
/* Line 1792 of yacc.c  */
#line 1941 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons(push((yyvsp[(1) - (4)].nd), new_hash(p, (yyvsp[(3) - (4)].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (4)].nd));
                    }
    break;

  case 242:
/* Line 1792 of yacc.c  */
#line 1946 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons(list1(new_hash(p, (yyvsp[(1) - (2)].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (2)].nd));
                    }
    break;

  case 243:
/* Line 1792 of yacc.c  */
#line 1953 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons(list1((yyvsp[(1) - (1)].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (1)].nd));
                    }
    break;

  case 244:
/* Line 1792 of yacc.c  */
#line 1958 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons((yyvsp[(1) - (2)].nd), (yyvsp[(2) - (2)].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (2)].nd));
                    }
    break;

  case 245:
/* Line 1792 of yacc.c  */
#line 1963 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons(list1(new_hash(p, (yyvsp[(1) - (2)].nd))), (yyvsp[(2) - (2)].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (2)].nd));
                    }
    break;

  case 246:
/* Line 1792 of yacc.c  */
#line 1968 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons(push((yyvsp[(1) - (4)].nd), new_hash(p, (yyvsp[(3) - (4)].nd))), (yyvsp[(4) - (4)].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (4)].nd));
                    }
    break;

  case 247:
/* Line 1792 of yacc.c  */
#line 1973 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons(0, (yyvsp[(1) - (1)].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (1)].nd));
                    }
    break;

  case 248:
/* Line 1792 of yacc.c  */
#line 1979 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.stack) = p->cmdarg_stack;
                      CMDARG_PUSH(1);
                    }
    break;

  case 249:
/* Line 1792 of yacc.c  */
#line 1984 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      p->cmdarg_stack = (yyvsp[(1) - (2)].stack);
                      (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    }
    break;

  case 250:
/* Line 1792 of yacc.c  */
#line 1991 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_block_arg(p, (yyvsp[(2) - (2)].nd));
                    }
    break;

  case 251:
/* Line 1792 of yacc.c  */
#line 1997 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    }
    break;

  case 252:
/* Line 1792 of yacc.c  */
#line 2001 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = 0;
                    }
    break;

  case 253:
/* Line 1792 of yacc.c  */
#line 2007 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons((yyvsp[(1) - (1)].nd), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (1)].nd));
                    }
    break;

  case 254:
/* Line 1792 of yacc.c  */
#line 2012 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons(new_splat(p, (yyvsp[(2) - (2)].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[(2) - (2)].nd));
                    }
    break;

  case 255:
/* Line 1792 of yacc.c  */
#line 2017 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 256:
/* Line 1792 of yacc.c  */
#line 2021 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (4)].nd), new_splat(p, (yyvsp[(4) - (4)].nd)));
                    }
    break;

  case 257:
/* Line 1792 of yacc.c  */
#line 2025 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (4)].nd), (yyvsp[(4) - (4)].nd));
                    }
    break;

  case 258:
/* Line 1792 of yacc.c  */
#line 2029 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (5)].nd), new_splat(p, (yyvsp[(5) - (5)].nd)));
                    }
    break;

  case 259:
/* Line 1792 of yacc.c  */
#line 2035 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 260:
/* Line 1792 of yacc.c  */
#line 2039 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (4)].nd), new_splat(p, (yyvsp[(4) - (4)].nd)));
                    }
    break;

  case 261:
/* Line 1792 of yacc.c  */
#line 2043 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list1(new_splat(p, (yyvsp[(2) - (2)].nd)));
                    }
    break;

  case 269:
/* Line 1792 of yacc.c  */
#line 2056 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_fcall(p, (yyvsp[(1) - (1)].id), 0);
                    }
    break;

  case 270:
/* Line 1792 of yacc.c  */
#line 2060 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
    break;

  case 271:
/* Line 1792 of yacc.c  */
#line 2066 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      p->cmdarg_stack = (yyvsp[(2) - (4)].stack);
                      (yyval.nd) = (yyvsp[(3) - (4)].nd);
                    }
    break;

  case 272:
/* Line 1792 of yacc.c  */
#line 2071 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
    break;

  case 273:
/* Line 1792 of yacc.c  */
#line 2075 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {p->lstate = EXPR_ENDARG;}
    break;

  case 274:
/* Line 1792 of yacc.c  */
#line 2076 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      p->cmdarg_stack = (yyvsp[(2) - (5)].stack);
                      (yyval.nd) = (yyvsp[(3) - (5)].nd);
                    }
    break;

  case 275:
/* Line 1792 of yacc.c  */
#line 2080 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {p->lstate = EXPR_ENDARG;}
    break;

  case 276:
/* Line 1792 of yacc.c  */
#line 2081 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = 0;
                    }
    break;

  case 277:
/* Line 1792 of yacc.c  */
#line 2085 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (3)].nd);
                    }
    break;

  case 278:
/* Line 1792 of yacc.c  */
#line 2089 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_colon2(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id));
                    }
    break;

  case 279:
/* Line 1792 of yacc.c  */
#line 2093 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_colon3(p, (yyvsp[(2) - (2)].id));
                    }
    break;

  case 280:
/* Line 1792 of yacc.c  */
#line 2097 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_array(p, (yyvsp[(2) - (3)].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[(2) - (3)].nd));
                    }
    break;

  case 281:
/* Line 1792 of yacc.c  */
#line 2102 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_hash(p, (yyvsp[(2) - (3)].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[(2) - (3)].nd));
                    }
    break;

  case 282:
/* Line 1792 of yacc.c  */
#line 2107 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_return(p, 0);
                    }
    break;

  case 283:
/* Line 1792 of yacc.c  */
#line 2111 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_yield(p, (yyvsp[(3) - (4)].nd));
                    }
    break;

  case 284:
/* Line 1792 of yacc.c  */
#line 2115 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_yield(p, 0);
                    }
    break;

  case 285:
/* Line 1792 of yacc.c  */
#line 2119 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_yield(p, 0);
                    }
    break;

  case 286:
/* Line 1792 of yacc.c  */
#line 2123 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[(3) - (4)].nd)), "!");
                    }
    break;

  case 287:
/* Line 1792 of yacc.c  */
#line 2127 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = call_uni_op(p, new_nil(p), "!");
                    }
    break;

  case 288:
/* Line 1792 of yacc.c  */
#line 2131 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_fcall(p, (yyvsp[(1) - (2)].id), cons(0, (yyvsp[(2) - (2)].nd)));
                    }
    break;

  case 290:
/* Line 1792 of yacc.c  */
#line 2136 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      call_with_block(p, (yyvsp[(1) - (2)].nd), (yyvsp[(2) - (2)].nd));
                      (yyval.nd) = (yyvsp[(1) - (2)].nd);
                    }
    break;

  case 291:
/* Line 1792 of yacc.c  */
#line 2141 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      local_nest(p);
                      (yyval.num) = p->lpar_beg;
                      p->lpar_beg = ++p->paren_nest;
                    }
    break;

  case 292:
/* Line 1792 of yacc.c  */
#line 2147 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
    break;

  case 293:
/* Line 1792 of yacc.c  */
#line 2152 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      p->lpar_beg = (yyvsp[(2) - (5)].num);
                      (yyval.nd) = new_lambda(p, (yyvsp[(3) - (5)].nd), (yyvsp[(5) - (5)].nd));
                      local_unnest(p);
                      p->cmdarg_stack = (yyvsp[(4) - (5)].stack);
                    }
    break;

  case 294:
/* Line 1792 of yacc.c  */
#line 2162 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_if(p, cond((yyvsp[(2) - (6)].nd)), (yyvsp[(4) - (6)].nd), (yyvsp[(5) - (6)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (6)].num));
                    }
    break;

  case 295:
/* Line 1792 of yacc.c  */
#line 2170 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[(2) - (6)].nd)), (yyvsp[(4) - (6)].nd), (yyvsp[(5) - (6)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (6)].num));
                    }
    break;

  case 296:
/* Line 1792 of yacc.c  */
#line 2174 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {COND_PUSH(1);}
    break;

  case 297:
/* Line 1792 of yacc.c  */
#line 2174 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {COND_POP();}
    break;

  case 298:
/* Line 1792 of yacc.c  */
#line 2177 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_while(p, cond((yyvsp[(3) - (7)].nd)), (yyvsp[(6) - (7)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (7)].num));
                    }
    break;

  case 299:
/* Line 1792 of yacc.c  */
#line 2181 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {COND_PUSH(1);}
    break;

  case 300:
/* Line 1792 of yacc.c  */
#line 2181 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {COND_POP();}
    break;

  case 301:
/* Line 1792 of yacc.c  */
#line 2184 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_until(p, cond((yyvsp[(3) - (7)].nd)), (yyvsp[(6) - (7)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (7)].num));
                    }
    break;

  case 302:
/* Line 1792 of yacc.c  */
#line 2191 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_case(p, (yyvsp[(2) - (5)].nd), (yyvsp[(4) - (5)].nd));
                    }
    break;

  case 303:
/* Line 1792 of yacc.c  */
#line 2195 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_case(p, 0, (yyvsp[(3) - (4)].nd));
                    }
    break;

  case 304:
/* Line 1792 of yacc.c  */
#line 2199 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {COND_PUSH(1);}
    break;

  case 305:
/* Line 1792 of yacc.c  */
#line 2201 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {COND_POP();}
    break;

  case 306:
/* Line 1792 of yacc.c  */
#line 2204 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_for(p, (yyvsp[(2) - (9)].nd), (yyvsp[(5) - (9)].nd), (yyvsp[(8) - (9)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (9)].num));
                    }
    break;

  case 307:
/* Line 1792 of yacc.c  */
#line 2210 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "class definition in method body");
                      (yyval.nd) = local_switch(p);
                    }
    break;

  case 308:
/* Line 1792 of yacc.c  */
#line 2217 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_class(p, (yyvsp[(2) - (6)].nd), (yyvsp[(3) - (6)].nd), (yyvsp[(5) - (6)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (6)].num));
                      local_resume(p, (yyvsp[(4) - (6)].nd));
                    }
    break;

  case 309:
/* Line 1792 of yacc.c  */
#line 2224 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.num) = p->in_def;
                      p->in_def = 0;
                    }
    break;

  case 310:
/* Line 1792 of yacc.c  */
#line 2229 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons(local_switch(p), (node*)(intptr_t)p->in_single);
                      p->in_single = 0;
                    }
    break;

  case 311:
/* Line 1792 of yacc.c  */
#line 2235 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_sclass(p, (yyvsp[(3) - (8)].nd), (yyvsp[(7) - (8)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (8)].num));
                      local_resume(p, (yyvsp[(6) - (8)].nd)->car);
                      p->in_def = (yyvsp[(4) - (8)].num);
                      p->in_single = (int)(intptr_t)(yyvsp[(6) - (8)].nd)->cdr;
                    }
    break;

  case 312:
/* Line 1792 of yacc.c  */
#line 2244 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "module definition in method body");
                      (yyval.nd) = local_switch(p);
                    }
    break;

  case 313:
/* Line 1792 of yacc.c  */
#line 2251 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_module(p, (yyvsp[(2) - (5)].nd), (yyvsp[(4) - (5)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (5)].num));
                      local_resume(p, (yyvsp[(3) - (5)].nd));
                    }
    break;

  case 314:
/* Line 1792 of yacc.c  */
#line 2257 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
    break;

  case 315:
/* Line 1792 of yacc.c  */
#line 2261 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      p->in_def++;
                      (yyval.nd) = local_switch(p);
                    }
    break;

  case 316:
/* Line 1792 of yacc.c  */
#line 2268 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_def(p, (yyvsp[(2) - (7)].id), (yyvsp[(5) - (7)].nd), (yyvsp[(6) - (7)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (7)].num));
                      local_resume(p, (yyvsp[(4) - (7)].nd));
                      p->in_def--;
                      p->cmdarg_stack = (yyvsp[(3) - (7)].stack);
                    }
    break;

  case 317:
/* Line 1792 of yacc.c  */
#line 2276 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      p->lstate = EXPR_FNAME;
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
    break;

  case 318:
/* Line 1792 of yacc.c  */
#line 2282 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      p->in_single++;
                      p->lstate = EXPR_ENDFN; /* force for args */
                      (yyval.nd) = local_switch(p);
                    }
    break;

  case 319:
/* Line 1792 of yacc.c  */
#line 2290 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_sdef(p, (yyvsp[(2) - (9)].nd), (yyvsp[(5) - (9)].id), (yyvsp[(7) - (9)].nd), (yyvsp[(8) - (9)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(1) - (9)].num));
                      local_resume(p, (yyvsp[(6) - (9)].nd));
                      p->in_single--;
                      p->cmdarg_stack = (yyvsp[(4) - (9)].stack);
                    }
    break;

  case 320:
/* Line 1792 of yacc.c  */
#line 2298 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_break(p, 0);
                    }
    break;

  case 321:
/* Line 1792 of yacc.c  */
#line 2302 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_next(p, 0);
                    }
    break;

  case 322:
/* Line 1792 of yacc.c  */
#line 2306 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_redo(p);
                    }
    break;

  case 323:
/* Line 1792 of yacc.c  */
#line 2310 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_retry(p);
                    }
    break;

  case 324:
/* Line 1792 of yacc.c  */
#line 2316 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (1)].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
    break;

  case 331:
/* Line 1792 of yacc.c  */
#line 2335 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_if(p, cond((yyvsp[(2) - (5)].nd)), (yyvsp[(4) - (5)].nd), (yyvsp[(5) - (5)].nd));
                    }
    break;

  case 333:
/* Line 1792 of yacc.c  */
#line 2342 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    }
    break;

  case 334:
/* Line 1792 of yacc.c  */
#line 2348 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list1(list1((yyvsp[(1) - (1)].nd)));
                    }
    break;

  case 336:
/* Line 1792 of yacc.c  */
#line 2355 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_arg(p, (yyvsp[(1) - (1)].id));
                    }
    break;

  case 337:
/* Line 1792 of yacc.c  */
#line 2359 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[(2) - (3)].nd), 0);
                    }
    break;

  case 338:
/* Line 1792 of yacc.c  */
#line 2365 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                    }
    break;

  case 339:
/* Line 1792 of yacc.c  */
#line 2369 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 340:
/* Line 1792 of yacc.c  */
#line 2375 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list3((yyvsp[(1) - (1)].nd),0,0);
                    }
    break;

  case 341:
/* Line 1792 of yacc.c  */
#line 2379 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list3((yyvsp[(1) - (4)].nd), new_arg(p, (yyvsp[(4) - (4)].id)), 0);
                    }
    break;

  case 342:
/* Line 1792 of yacc.c  */
#line 2383 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list3((yyvsp[(1) - (6)].nd), new_arg(p, (yyvsp[(4) - (6)].id)), (yyvsp[(6) - (6)].nd));
                    }
    break;

  case 343:
/* Line 1792 of yacc.c  */
#line 2387 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list3((yyvsp[(1) - (3)].nd), (node*)-1, 0);
                    }
    break;

  case 344:
/* Line 1792 of yacc.c  */
#line 2391 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list3((yyvsp[(1) - (5)].nd), (node*)-1, (yyvsp[(5) - (5)].nd));
                    }
    break;

  case 345:
/* Line 1792 of yacc.c  */
#line 2395 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[(2) - (2)].id)), 0);
                    }
    break;

  case 346:
/* Line 1792 of yacc.c  */
#line 2399 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[(2) - (4)].id)), (yyvsp[(4) - (4)].nd));
                    }
    break;

  case 347:
/* Line 1792 of yacc.c  */
#line 2403 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list3(0, (node*)-1, 0);
                    }
    break;

  case 348:
/* Line 1792 of yacc.c  */
#line 2407 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list3(0, (node*)-1, (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 349:
/* Line 1792 of yacc.c  */
#line 2413 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (6)].nd), (yyvsp[(3) - (6)].nd), (yyvsp[(5) - (6)].id), 0, (yyvsp[(6) - (6)].id));
                    }
    break;

  case 350:
/* Line 1792 of yacc.c  */
#line 2417 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (8)].nd), (yyvsp[(3) - (8)].nd), (yyvsp[(5) - (8)].id), (yyvsp[(7) - (8)].nd), (yyvsp[(8) - (8)].id));
                    }
    break;

  case 351:
/* Line 1792 of yacc.c  */
#line 2421 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].nd), 0, 0, (yyvsp[(4) - (4)].id));
                    }
    break;

  case 352:
/* Line 1792 of yacc.c  */
#line 2425 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (6)].nd), (yyvsp[(3) - (6)].nd), 0, (yyvsp[(5) - (6)].nd), (yyvsp[(6) - (6)].id));
                    }
    break;

  case 353:
/* Line 1792 of yacc.c  */
#line 2429 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (4)].nd), 0, (yyvsp[(3) - (4)].id), 0, (yyvsp[(4) - (4)].id));
                    }
    break;

  case 354:
/* Line 1792 of yacc.c  */
#line 2433 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (2)].nd), 0, 1, 0, 0);
                    }
    break;

  case 355:
/* Line 1792 of yacc.c  */
#line 2437 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (6)].nd), 0, (yyvsp[(3) - (6)].id), (yyvsp[(5) - (6)].nd), (yyvsp[(6) - (6)].id));
                    }
    break;

  case 356:
/* Line 1792 of yacc.c  */
#line 2441 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (2)].nd), 0, 0, 0, (yyvsp[(2) - (2)].id));
                    }
    break;

  case 357:
/* Line 1792 of yacc.c  */
#line 2445 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].id), 0, (yyvsp[(4) - (4)].id));
                    }
    break;

  case 358:
/* Line 1792 of yacc.c  */
#line 2449 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[(1) - (6)].nd), (yyvsp[(3) - (6)].id), (yyvsp[(5) - (6)].nd), (yyvsp[(6) - (6)].id));
                    }
    break;

  case 359:
/* Line 1792 of yacc.c  */
#line 2453 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[(1) - (2)].nd), 0, 0, (yyvsp[(2) - (2)].id));
                    }
    break;

  case 360:
/* Line 1792 of yacc.c  */
#line 2457 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[(1) - (4)].nd), 0, (yyvsp[(3) - (4)].nd), (yyvsp[(4) - (4)].id));
                    }
    break;

  case 361:
/* Line 1792 of yacc.c  */
#line 2461 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[(1) - (2)].id), 0, (yyvsp[(2) - (2)].id));
                    }
    break;

  case 362:
/* Line 1792 of yacc.c  */
#line 2465 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[(1) - (4)].id), (yyvsp[(3) - (4)].nd), (yyvsp[(4) - (4)].id));
                    }
    break;

  case 363:
/* Line 1792 of yacc.c  */
#line 2469 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[(1) - (1)].id));
                    }
    break;

  case 365:
/* Line 1792 of yacc.c  */
#line 2476 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      p->cmd_start = TRUE;
                      (yyval.nd) = (yyvsp[(1) - (1)].nd);
                    }
    break;

  case 366:
/* Line 1792 of yacc.c  */
#line 2483 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = 0;
                    }
    break;

  case 367:
/* Line 1792 of yacc.c  */
#line 2487 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = 0;
                    }
    break;

  case 368:
/* Line 1792 of yacc.c  */
#line 2491 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (4)].nd);
                    }
    break;

  case 369:
/* Line 1792 of yacc.c  */
#line 2498 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = 0;
                    }
    break;

  case 370:
/* Line 1792 of yacc.c  */
#line 2502 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = 0;
                    }
    break;

  case 373:
/* Line 1792 of yacc.c  */
#line 2512 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      local_add_f(p, (yyvsp[(1) - (1)].id));
                      new_bv(p, (yyvsp[(1) - (1)].id));
                    }
    break;

  case 375:
/* Line 1792 of yacc.c  */
#line 2520 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (4)].nd);
                    }
    break;

  case 376:
/* Line 1792 of yacc.c  */
#line 2524 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (1)].nd);
                    }
    break;

  case 377:
/* Line 1792 of yacc.c  */
#line 2530 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (3)].nd);
                    }
    break;

  case 378:
/* Line 1792 of yacc.c  */
#line 2534 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (3)].nd);
                    }
    break;

  case 379:
/* Line 1792 of yacc.c  */
#line 2540 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      local_nest(p);
                    }
    break;

  case 380:
/* Line 1792 of yacc.c  */
#line 2546 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_block(p,(yyvsp[(3) - (5)].nd),(yyvsp[(4) - (5)].nd));
                      local_unnest(p);
                    }
    break;

  case 381:
/* Line 1792 of yacc.c  */
#line 2553 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      if ((yyvsp[(1) - (2)].nd)->car == (node*)NODE_YIELD) {
                        yyerror(p, "block given to yield");
                      }
                      else {
                        call_with_block(p, (yyvsp[(1) - (2)].nd), (yyvsp[(2) - (2)].nd));
                      }
                      (yyval.nd) = (yyvsp[(1) - (2)].nd);
                    }
    break;

  case 382:
/* Line 1792 of yacc.c  */
#line 2563 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].id), (yyvsp[(4) - (4)].nd));
                    }
    break;

  case 383:
/* Line 1792 of yacc.c  */
#line 2567 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), (yyvsp[(4) - (5)].nd));
                      call_with_block(p, (yyval.nd), (yyvsp[(5) - (5)].nd));
                    }
    break;

  case 384:
/* Line 1792 of yacc.c  */
#line 2572 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (5)].nd), (yyvsp[(3) - (5)].id), (yyvsp[(4) - (5)].nd));
                      call_with_block(p, (yyval.nd), (yyvsp[(5) - (5)].nd));
                    }
    break;

  case 385:
/* Line 1792 of yacc.c  */
#line 2579 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_fcall(p, (yyvsp[(1) - (2)].id), (yyvsp[(2) - (2)].nd));
                    }
    break;

  case 386:
/* Line 1792 of yacc.c  */
#line 2583 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].id), (yyvsp[(4) - (4)].nd));
                    }
    break;

  case 387:
/* Line 1792 of yacc.c  */
#line 2587 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].id), (yyvsp[(4) - (4)].nd));
                    }
    break;

  case 388:
/* Line 1792 of yacc.c  */
#line 2591 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].id), 0);
                    }
    break;

  case 389:
/* Line 1792 of yacc.c  */
#line 2595 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (3)].nd), intern("call",4), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 390:
/* Line 1792 of yacc.c  */
#line 2599 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (3)].nd), intern("call",4), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 391:
/* Line 1792 of yacc.c  */
#line 2603 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_super(p, (yyvsp[(2) - (2)].nd));
                    }
    break;

  case 392:
/* Line 1792 of yacc.c  */
#line 2607 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_zsuper(p);
                    }
    break;

  case 393:
/* Line 1792 of yacc.c  */
#line 2611 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_call(p, (yyvsp[(1) - (4)].nd), intern("[]",2), (yyvsp[(3) - (4)].nd));
                    }
    break;

  case 394:
/* Line 1792 of yacc.c  */
#line 2617 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      local_nest(p);
                      (yyval.num) = p->lineno;
                    }
    break;

  case 395:
/* Line 1792 of yacc.c  */
#line 2623 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_block(p,(yyvsp[(3) - (5)].nd),(yyvsp[(4) - (5)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(2) - (5)].num));
                      local_unnest(p);
                    }
    break;

  case 396:
/* Line 1792 of yacc.c  */
#line 2629 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      local_nest(p);
                      (yyval.num) = p->lineno;
                    }
    break;

  case 397:
/* Line 1792 of yacc.c  */
#line 2635 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_block(p,(yyvsp[(3) - (5)].nd),(yyvsp[(4) - (5)].nd));
                      SET_LINENO((yyval.nd), (yyvsp[(2) - (5)].num));
                      local_unnest(p);
                    }
    break;

  case 398:
/* Line 1792 of yacc.c  */
#line 2645 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons(cons((yyvsp[(2) - (5)].nd), (yyvsp[(4) - (5)].nd)), (yyvsp[(5) - (5)].nd));
                    }
    break;

  case 399:
/* Line 1792 of yacc.c  */
#line 2651 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      if ((yyvsp[(1) - (1)].nd)) {
                        (yyval.nd) = cons(cons(0, (yyvsp[(1) - (1)].nd)), 0);
                      }
                      else {
                        (yyval.nd) = 0;
                      }
                    }
    break;

  case 401:
/* Line 1792 of yacc.c  */
#line 2665 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list1(list3((yyvsp[(2) - (6)].nd), (yyvsp[(3) - (6)].nd), (yyvsp[(5) - (6)].nd)));
                      if ((yyvsp[(6) - (6)].nd)) (yyval.nd) = append((yyval.nd), (yyvsp[(6) - (6)].nd));
                    }
    break;

  case 403:
/* Line 1792 of yacc.c  */
#line 2673 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                        (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                    }
    break;

  case 406:
/* Line 1792 of yacc.c  */
#line 2681 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    }
    break;

  case 408:
/* Line 1792 of yacc.c  */
#line 2688 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    }
    break;

  case 416:
/* Line 1792 of yacc.c  */
#line 2703 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    }
    break;

  case 417:
/* Line 1792 of yacc.c  */
#line 2707 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_dstr(p, push((yyvsp[(2) - (3)].nd), (yyvsp[(3) - (3)].nd)));
                    }
    break;

  case 419:
/* Line 1792 of yacc.c  */
#line 2714 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = append((yyvsp[(1) - (2)].nd), (yyvsp[(2) - (2)].nd));
                    }
    break;

  case 420:
/* Line 1792 of yacc.c  */
#line 2720 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                    }
    break;

  case 421:
/* Line 1792 of yacc.c  */
#line 2724 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    }
    break;

  case 422:
/* Line 1792 of yacc.c  */
#line 2730 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      p->lex_strterm = (yyvsp[(2) - (4)].nd);
                      (yyval.nd) = list2((yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].nd));
                    }
    break;

  case 423:
/* Line 1792 of yacc.c  */
#line 2735 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
    break;

  case 424:
/* Line 1792 of yacc.c  */
#line 2739 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
    break;

  case 425:
/* Line 1792 of yacc.c  */
#line 2745 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                        (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    }
    break;

  case 426:
/* Line 1792 of yacc.c  */
#line 2749 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_dxstr(p, push((yyvsp[(2) - (3)].nd), (yyvsp[(3) - (3)].nd)));
                    }
    break;

  case 427:
/* Line 1792 of yacc.c  */
#line 2755 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                        (yyval.nd) = (yyvsp[(2) - (2)].nd);
                    }
    break;

  case 428:
/* Line 1792 of yacc.c  */
#line 2759 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_dregx(p, (yyvsp[(2) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 434:
/* Line 1792 of yacc.c  */
#line 2776 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, new_str(p, "", 0));
                      heredoc_end(p);
                    }
    break;

  case 435:
/* Line 1792 of yacc.c  */
#line 2782 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      heredoc_end(p);
                    }
    break;

  case 438:
/* Line 1792 of yacc.c  */
#line 2792 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, (yyvsp[(1) - (1)].nd));
                      heredoc_treat_nextline(p);
                    }
    break;

  case 439:
/* Line 1792 of yacc.c  */
#line 2798 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    }
    break;

  case 440:
/* Line 1792 of yacc.c  */
#line 2804 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      p->lex_strterm = (yyvsp[(2) - (4)].nd);
                      inf->doc = push(push(inf->doc, (yyvsp[(1) - (4)].nd)), (yyvsp[(3) - (4)].nd));
                    }
    break;

  case 441:
/* Line 1792 of yacc.c  */
#line 2812 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_words(p, list1((yyvsp[(2) - (2)].nd)));
                    }
    break;

  case 442:
/* Line 1792 of yacc.c  */
#line 2816 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_words(p, push((yyvsp[(2) - (3)].nd), (yyvsp[(3) - (3)].nd)));
                    }
    break;

  case 443:
/* Line 1792 of yacc.c  */
#line 2823 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_sym(p, (yyvsp[(1) - (1)].id));
                    }
    break;

  case 444:
/* Line 1792 of yacc.c  */
#line 2827 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      p->lstate = EXPR_END;
                      (yyval.nd) = new_dsym(p, push((yyvsp[(3) - (4)].nd), (yyvsp[(4) - (4)].nd)));
                    }
    break;

  case 445:
/* Line 1792 of yacc.c  */
#line 2834 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      p->lstate = EXPR_END;
                      (yyval.id) = (yyvsp[(2) - (2)].id);
                    }
    break;

  case 450:
/* Line 1792 of yacc.c  */
#line 2845 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.id) = new_strsym(p, (yyvsp[(1) - (1)].nd));
                    }
    break;

  case 451:
/* Line 1792 of yacc.c  */
#line 2849 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.id) = new_strsym(p, (yyvsp[(2) - (2)].nd));
                    }
    break;

  case 452:
/* Line 1792 of yacc.c  */
#line 2855 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_symbols(p, list1((yyvsp[(2) - (2)].nd)));
                    }
    break;

  case 453:
/* Line 1792 of yacc.c  */
#line 2859 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_symbols(p, push((yyvsp[(2) - (3)].nd), (yyvsp[(3) - (3)].nd)));
                    }
    break;

  case 456:
/* Line 1792 of yacc.c  */
#line 2867 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = negate_lit(p, (yyvsp[(2) - (2)].nd));
                    }
    break;

  case 457:
/* Line 1792 of yacc.c  */
#line 2871 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = negate_lit(p, (yyvsp[(2) - (2)].nd));
                    }
    break;

  case 458:
/* Line 1792 of yacc.c  */
#line 2877 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_lvar(p, (yyvsp[(1) - (1)].id));
                    }
    break;

  case 459:
/* Line 1792 of yacc.c  */
#line 2881 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_ivar(p, (yyvsp[(1) - (1)].id));
                    }
    break;

  case 460:
/* Line 1792 of yacc.c  */
#line 2885 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_gvar(p, (yyvsp[(1) - (1)].id));
                    }
    break;

  case 461:
/* Line 1792 of yacc.c  */
#line 2889 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_cvar(p, (yyvsp[(1) - (1)].id));
                    }
    break;

  case 462:
/* Line 1792 of yacc.c  */
#line 2893 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_const(p, (yyvsp[(1) - (1)].id));
                    }
    break;

  case 463:
/* Line 1792 of yacc.c  */
#line 2899 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      assignable(p, (yyvsp[(1) - (1)].nd));
                    }
    break;

  case 464:
/* Line 1792 of yacc.c  */
#line 2905 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = var_reference(p, (yyvsp[(1) - (1)].nd));
                    }
    break;

  case 465:
/* Line 1792 of yacc.c  */
#line 2909 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_nil(p);
                    }
    break;

  case 466:
/* Line 1792 of yacc.c  */
#line 2913 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_self(p);
                    }
    break;

  case 467:
/* Line 1792 of yacc.c  */
#line 2917 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_true(p);
                    }
    break;

  case 468:
/* Line 1792 of yacc.c  */
#line 2921 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_false(p);
                    }
    break;

  case 469:
/* Line 1792 of yacc.c  */
#line 2925 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      if (!p->filename) {
                        p->filename = "(null)";
                      }
                      (yyval.nd) = new_str(p, p->filename, strlen(p->filename));
                    }
    break;

  case 470:
/* Line 1792 of yacc.c  */
#line 2932 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      char buf[16];

                      snprintf(buf, sizeof(buf), "%d", p->lineno);
                      (yyval.nd) = new_int(p, buf, 10);
                    }
    break;

  case 473:
/* Line 1792 of yacc.c  */
#line 2945 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = 0;
                    }
    break;

  case 474:
/* Line 1792 of yacc.c  */
#line 2949 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
    break;

  case 475:
/* Line 1792 of yacc.c  */
#line 2954 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(3) - (4)].nd);
                    }
    break;

  case 476:
/* Line 1792 of yacc.c  */
#line 2958 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      yyerrok;
                      (yyval.nd) = 0;
                    }
    break;

  case 477:
/* Line 1792 of yacc.c  */
#line 2965 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(2) - (3)].nd);
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
    break;

  case 478:
/* Line 1792 of yacc.c  */
#line 2971 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (2)].nd);
                    }
    break;

  case 479:
/* Line 1792 of yacc.c  */
#line 2977 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (6)].nd), (yyvsp[(3) - (6)].nd), (yyvsp[(5) - (6)].id), 0, (yyvsp[(6) - (6)].id));
                    }
    break;

  case 480:
/* Line 1792 of yacc.c  */
#line 2981 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (8)].nd), (yyvsp[(3) - (8)].nd), (yyvsp[(5) - (8)].id), (yyvsp[(7) - (8)].nd), (yyvsp[(8) - (8)].id));
                    }
    break;

  case 481:
/* Line 1792 of yacc.c  */
#line 2985 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].nd), 0, 0, (yyvsp[(4) - (4)].id));
                    }
    break;

  case 482:
/* Line 1792 of yacc.c  */
#line 2989 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (6)].nd), (yyvsp[(3) - (6)].nd), 0, (yyvsp[(5) - (6)].nd), (yyvsp[(6) - (6)].id));
                    }
    break;

  case 483:
/* Line 1792 of yacc.c  */
#line 2993 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (4)].nd), 0, (yyvsp[(3) - (4)].id), 0, (yyvsp[(4) - (4)].id));
                    }
    break;

  case 484:
/* Line 1792 of yacc.c  */
#line 2997 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (6)].nd), 0, (yyvsp[(3) - (6)].id), (yyvsp[(5) - (6)].nd), (yyvsp[(6) - (6)].id));
                    }
    break;

  case 485:
/* Line 1792 of yacc.c  */
#line 3001 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, (yyvsp[(1) - (2)].nd), 0, 0, 0, (yyvsp[(2) - (2)].id));
                    }
    break;

  case 486:
/* Line 1792 of yacc.c  */
#line 3005 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[(1) - (4)].nd), (yyvsp[(3) - (4)].id), 0, (yyvsp[(4) - (4)].id));
                    }
    break;

  case 487:
/* Line 1792 of yacc.c  */
#line 3009 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[(1) - (6)].nd), (yyvsp[(3) - (6)].id), (yyvsp[(5) - (6)].nd), (yyvsp[(6) - (6)].id));
                    }
    break;

  case 488:
/* Line 1792 of yacc.c  */
#line 3013 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[(1) - (2)].nd), 0, 0, (yyvsp[(2) - (2)].id));
                    }
    break;

  case 489:
/* Line 1792 of yacc.c  */
#line 3017 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[(1) - (4)].nd), 0, (yyvsp[(3) - (4)].nd), (yyvsp[(4) - (4)].id));
                    }
    break;

  case 490:
/* Line 1792 of yacc.c  */
#line 3021 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[(1) - (2)].id), 0, (yyvsp[(2) - (2)].id));
                    }
    break;

  case 491:
/* Line 1792 of yacc.c  */
#line 3025 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[(1) - (4)].id), (yyvsp[(3) - (4)].nd), (yyvsp[(4) - (4)].id));
                    }
    break;

  case 492:
/* Line 1792 of yacc.c  */
#line 3029 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[(1) - (1)].id));
                    }
    break;

  case 493:
/* Line 1792 of yacc.c  */
#line 3033 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      local_add_f(p, 0);
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, 0);
                    }
    break;

  case 494:
/* Line 1792 of yacc.c  */
#line 3040 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      yyerror(p, "formal argument cannot be a constant");
                      (yyval.nd) = 0;
                    }
    break;

  case 495:
/* Line 1792 of yacc.c  */
#line 3045 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      yyerror(p, "formal argument cannot be an instance variable");
                      (yyval.nd) = 0;
                    }
    break;

  case 496:
/* Line 1792 of yacc.c  */
#line 3050 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      yyerror(p, "formal argument cannot be a global variable");
                      (yyval.nd) = 0;
                    }
    break;

  case 497:
/* Line 1792 of yacc.c  */
#line 3055 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      yyerror(p, "formal argument cannot be a class variable");
                      (yyval.nd) = 0;
                    }
    break;

  case 498:
/* Line 1792 of yacc.c  */
#line 3062 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.id) = 0;
                    }
    break;

  case 499:
/* Line 1792 of yacc.c  */
#line 3066 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      local_add_f(p, (yyvsp[(1) - (1)].id));
                      (yyval.id) = (yyvsp[(1) - (1)].id);
                    }
    break;

  case 500:
/* Line 1792 of yacc.c  */
#line 3073 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_arg(p, (yyvsp[(1) - (1)].id));
                    }
    break;

  case 501:
/* Line 1792 of yacc.c  */
#line 3077 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = new_masgn(p, (yyvsp[(2) - (3)].nd), 0);
                    }
    break;

  case 502:
/* Line 1792 of yacc.c  */
#line 3083 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                    }
    break;

  case 503:
/* Line 1792 of yacc.c  */
#line 3087 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 504:
/* Line 1792 of yacc.c  */
#line 3093 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      local_add_f(p, (yyvsp[(1) - (2)].id));
                      (yyval.id) = (yyvsp[(1) - (2)].id);
                    }
    break;

  case 505:
/* Line 1792 of yacc.c  */
#line 3100 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons(nsym((yyvsp[(1) - (2)].id)), (yyvsp[(2) - (2)].nd));
                    }
    break;

  case 506:
/* Line 1792 of yacc.c  */
#line 3106 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons(nsym((yyvsp[(1) - (2)].id)), (yyvsp[(2) - (2)].nd));
                    }
    break;

  case 507:
/* Line 1792 of yacc.c  */
#line 3112 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                    }
    break;

  case 508:
/* Line 1792 of yacc.c  */
#line 3116 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 509:
/* Line 1792 of yacc.c  */
#line 3122 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                    }
    break;

  case 510:
/* Line 1792 of yacc.c  */
#line 3126 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 513:
/* Line 1792 of yacc.c  */
#line 3136 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      local_add_f(p, (yyvsp[(2) - (2)].id));
                      (yyval.id) = (yyvsp[(2) - (2)].id);
                    }
    break;

  case 514:
/* Line 1792 of yacc.c  */
#line 3141 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      local_add_f(p, 0);
                      (yyval.id) = -1;
                    }
    break;

  case 517:
/* Line 1792 of yacc.c  */
#line 3152 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      local_add_f(p, (yyvsp[(2) - (2)].id));
                      (yyval.id) = (yyvsp[(2) - (2)].id);
                    }
    break;

  case 518:
/* Line 1792 of yacc.c  */
#line 3159 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.id) = (yyvsp[(2) - (2)].id);
                    }
    break;

  case 519:
/* Line 1792 of yacc.c  */
#line 3163 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      local_add_f(p, 0);
                      (yyval.id) = 0;
                    }
    break;

  case 520:
/* Line 1792 of yacc.c  */
#line 3170 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (1)].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
    break;

  case 521:
/* Line 1792 of yacc.c  */
#line 3174 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {p->lstate = EXPR_BEG;}
    break;

  case 522:
/* Line 1792 of yacc.c  */
#line 3175 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      if ((yyvsp[(3) - (4)].nd) == 0) {
                        yyerror(p, "can't define singleton method for ().");
                      }
                      else {
                        switch ((enum node_type)(int)(intptr_t)(yyvsp[(3) - (4)].nd)->car) {
                        case NODE_STR:
                        case NODE_DSTR:
                        case NODE_XSTR:
                        case NODE_DXSTR:
                        case NODE_DREGX:
                        case NODE_MATCH:
                        case NODE_FLOAT:
                        case NODE_ARRAY:
                        case NODE_HEREDOC:
                          yyerror(p, "can't define singleton method for literals");
                        default:
                          break;
                        }
                      }
                      (yyval.nd) = (yyvsp[(3) - (4)].nd);
                    }
    break;

  case 524:
/* Line 1792 of yacc.c  */
#line 3201 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = (yyvsp[(1) - (2)].nd);
                    }
    break;

  case 525:
/* Line 1792 of yacc.c  */
#line 3207 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = list1((yyvsp[(1) - (1)].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[(1) - (1)].nd));
                    }
    break;

  case 526:
/* Line 1792 of yacc.c  */
#line 3212 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = push((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 527:
/* Line 1792 of yacc.c  */
#line 3218 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons((yyvsp[(1) - (3)].nd), (yyvsp[(3) - (3)].nd));
                    }
    break;

  case 528:
/* Line 1792 of yacc.c  */
#line 3222 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = cons(new_sym(p, (yyvsp[(1) - (2)].id)), (yyvsp[(2) - (2)].nd));
                    }
    break;

  case 550:
/* Line 1792 of yacc.c  */
#line 3266 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {yyerrok;}
    break;

  case 552:
/* Line 1792 of yacc.c  */
#line 3271 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      p->lineno++;
                      p->column = 0;
                    }
    break;

  case 555:
/* Line 1792 of yacc.c  */
#line 3278 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {yyerrok;}
    break;

  case 556:
/* Line 1792 of yacc.c  */
#line 3282 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"
    {
                      (yyval.nd) = 0;
                    }
    break;


/* Line 1792 of yacc.c  */
#line 8825 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\build\\host-debug\\mrbgems\\mruby-compiler\\core\\y.tab.c"
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (p, YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (p, yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval, p);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp, p);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (p, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, p);
    }
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp, p);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


/* Line 2055 of yacc.c  */
#line 3286 "D:\\prj\\painkiller\\101-hello\\ruby\\mruby\\mrbgems\\mruby-compiler\\core\\parse.y"

#define yylval  (*((YYSTYPE*)(p->ylval)))

static void
yyerror(parser_state *p, const char *s)
{
  char* c;
  int n;

  if (! p->capture_errors) {
#ifdef ENABLE_STDIO
    if (p->filename) {
      fprintf(stderr, "%s:%d:%d: %s\n", p->filename, p->lineno, p->column, s);
    }
    else {
      fprintf(stderr, "line %d:%d: %s\n", p->lineno, p->column, s);
    }
#endif
  }
  else if (p->nerr < sizeof(p->error_buffer) / sizeof(p->error_buffer[0])) {
    n = strlen(s);
    c = (char *)parser_palloc(p, n + 1);
    memcpy(c, s, n + 1);
    p->error_buffer[p->nerr].message = c;
    p->error_buffer[p->nerr].lineno = p->lineno;
    p->error_buffer[p->nerr].column = p->column;
  }
  p->nerr++;
}

static void
yyerror_i(parser_state *p, const char *fmt, int i)
{
  char buf[256];

  snprintf(buf, sizeof(buf), fmt, i);
  yyerror(p, buf);
}

static void
yywarn(parser_state *p, const char *s)
{
  char* c;
  int n;

  if (! p->capture_errors) {
#ifdef ENABLE_STDIO
    if (p->filename) {
      fprintf(stderr, "%s:%d:%d: %s\n", p->filename, p->lineno, p->column, s);
    }
    else {
      fprintf(stderr, "line %d:%d: %s\n", p->lineno, p->column, s);
    }
#endif
  }
  else if (p->nwarn < sizeof(p->warn_buffer) / sizeof(p->warn_buffer[0])) {
    n = strlen(s);
    c = (char *)parser_palloc(p, n + 1);
    memcpy(c, s, n + 1);
    p->warn_buffer[p->nwarn].message = c;
    p->warn_buffer[p->nwarn].lineno = p->lineno;
    p->warn_buffer[p->nwarn].column = p->column;
  }
  p->nwarn++;
}

static void
yywarning(parser_state *p, const char *s)
{
  yywarn(p, s);
}

static void
yywarning_s(parser_state *p, const char *fmt, const char *s)
{
  char buf[256];

  snprintf(buf, sizeof(buf), fmt, s);
  yywarning(p, buf);
}

static void
backref_error(parser_state *p, node *n)
{
  int c;

  c = (int)(intptr_t)n->car;

  if (c == NODE_NTH_REF) {
    yyerror_i(p, "can't set variable $%d", (int)(intptr_t)n->cdr);
  }
  else if (c == NODE_BACK_REF) {
    yyerror_i(p, "can't set variable $%c", (int)(intptr_t)n->cdr);
  }
  else {
    mrb_bug(p->mrb, "Internal error in backref_error() : n=>car == %S", mrb_fixnum_value(c));
  }
}

static void pushback(parser_state *p, int c);
static mrb_bool peeks(parser_state *p, const char *s);
static mrb_bool skips(parser_state *p, const char *s);

static inline int
nextc(parser_state *p)
{
  int c;

  if (p->pb) {
    node *tmp;

    c = (int)(intptr_t)p->pb->car;
    tmp = p->pb;
    p->pb = p->pb->cdr;
    cons_free(tmp);
  }
  else {
#ifdef ENABLE_STDIO
    if (p->f) {
      if (feof(p->f)) goto eof;
      c = fgetc(p->f);
      if (c == EOF) goto eof;
    }
    else
#endif
      if (!p->s || p->s >= p->send) {
        goto eof;
      }
      else {
        c = (unsigned char)*p->s++;
      }
  }
  if (c >= 0) {
    p->column++;
  }
  if (c == '\r') {
    c = nextc(p);
    if (c != '\n') {
      pushback(p, c);
      return '\r';
    }
    return c;
  }
  return c;

  eof:
  if (!p->cxt) return -1;
  else {
    if (p->cxt->partial_hook(p) < 0)
      return -1;                /* end of program(s) */
    return -2;                  /* end of a file in the program files */
  }
}

static void
pushback(parser_state *p, int c)
{
  if (c >= 0) {
    p->column--;
  }
  p->pb = cons((node*)(intptr_t)c, p->pb);
}

static void
skip(parser_state *p, char term)
{
  int c;

  for (;;) {
    c = nextc(p);
    if (c < 0) break;
    if (c == term) break;
  }
}

static int
peekc_n(parser_state *p, int n)
{
  node *list = 0;
  int c0;

  do {
    c0 = nextc(p);
    if (c0 == -1) return c0;    /* do not skip partial EOF */
    list = push(list, (node*)(intptr_t)c0);
  } while(n--);
  if (p->pb) {
    p->pb = append((node*)list, p->pb);
  }
  else {
    p->pb = list;
  }
  return c0;
}

static mrb_bool
peek_n(parser_state *p, int c, int n)
{
  return peekc_n(p, n) == c && c >= 0;
}
#define peek(p,c) peek_n((p), (c), 0)

static mrb_bool
peeks(parser_state *p, const char *s)
{
  int len = strlen(s);

#ifdef ENABLE_STDIO
  if (p->f) {
    int n = 0;
    while (*s) {
      if (!peek_n(p, *s++, n++)) return FALSE;
    }
    return TRUE;
  }
  else
#endif
    if (p->s && p->s + len <= p->send) {
      if (memcmp(p->s, s, len) == 0) return TRUE;
    }
  return FALSE;
}

static mrb_bool
skips(parser_state *p, const char *s)
{
  int c;

  for (;;) {
    /* skip until first char */
    for (;;) {
      c = nextc(p);
      if (c < 0) return c;
      if (c == '\n') {
        p->lineno++;
        p->column = 0;
      }
      if (c == *s) break;
    }
    s++;
    if (peeks(p, s)) {
      int len = strlen(s);

      while (len--) {
        if (nextc(p) == '\n') {
          p->lineno++;
          p->column = 0;
        }
      }
      return TRUE;
    }
    else{
      s--;
    }
  }
  return FALSE;
}


static int
newtok(parser_state *p)
{
  p->bidx = 0;
  return p->column - 1;
}

static void
tokadd(parser_state *p, int32_t c)
{
  char utf8[4];
  unsigned len;

  /* mrb_assert(-0x10FFFF <= c && c <= 0xFF); */
  if (c >= 0) {
    /* Single byte from source or non-Unicode escape */
    utf8[0] = (char)c;
    len = 1;
  }
  else {
    /* Unicode character */
    c = -c;
    if (c < 0x80) {
      utf8[0] = (char)c;
      len = 1;
    }
    else if (c < 0x800) {
      utf8[0] = (char)(0xC0 | (c >> 6));
      utf8[1] = (char)(0x80 | (c & 0x3F));
      len = 2;
    }
    else if (c < 0x10000) {
      utf8[0] = (char)(0xE0 |  (c >> 12)        );
      utf8[1] = (char)(0x80 | ((c >>  6) & 0x3F));
      utf8[2] = (char)(0x80 | ( c        & 0x3F));
      len = 3;
    }
    else {
      utf8[0] = (char)(0xF0 |  (c >> 18)        );
      utf8[1] = (char)(0x80 | ((c >> 12) & 0x3F));
      utf8[2] = (char)(0x80 | ((c >>  6) & 0x3F));
      utf8[3] = (char)(0x80 | ( c        & 0x3F));
      len = 4;
    }
  }
  if (p->bidx+len <= MRB_PARSER_BUF_SIZE) {
    unsigned i;
    for (i = 0; i < len; i++) {
      p->buf[p->bidx++] = utf8[i];
    }
  }
}

static int
toklast(parser_state *p)
{
  return p->buf[p->bidx-1];
}

static void
tokfix(parser_state *p)
{
  int i = p->bidx, imax = MRB_PARSER_BUF_SIZE - 1;

  if (i > imax) {
    i = imax;
    yyerror(p, "string too long (truncated)");
  }
  p->buf[i] = '\0';
}

static const char*
tok(parser_state *p)
{
  return p->buf;
}

static int
toklen(parser_state *p)
{
  return p->bidx;
}

#define IS_ARG() (p->lstate == EXPR_ARG || p->lstate == EXPR_CMDARG)
#define IS_END() (p->lstate == EXPR_END || p->lstate == EXPR_ENDARG || p->lstate == EXPR_ENDFN)
#define IS_BEG() (p->lstate == EXPR_BEG || p->lstate == EXPR_MID || p->lstate == EXPR_VALUE || p->lstate == EXPR_CLASS)
#define IS_SPCARG(c) (IS_ARG() && space_seen && !ISSPACE(c))
#define IS_LABEL_POSSIBLE() ((p->lstate == EXPR_BEG && !cmd_state) || IS_ARG())
#define IS_LABEL_SUFFIX(n) (peek_n(p, ':',(n)) && !peek_n(p, ':', (n)+1))

static int
scan_oct(const int *start, int len, int *retlen)
{
  const int *s = start;
  int retval = 0;

  /* mrb_assert(len <= 3) */
  while (len-- && *s >= '0' && *s <= '7') {
    retval <<= 3;
    retval |= *s++ - '0';
  }
  *retlen = s - start;

  return retval;
}

static int32_t
scan_hex(const int *start, int len, int *retlen)
{
  static const char hexdigit[] = "0123456789abcdef0123456789ABCDEF";
  const int *s = start;
  int32_t retval = 0;
  char *tmp;

  /* mrb_assert(len <= 8) */
  while (len-- && *s && (tmp = (char*)strchr(hexdigit, *s))) {
    retval <<= 4;
    retval |= (tmp - hexdigit) & 15;
    s++;
  }
  *retlen = s - start;

  return retval;
}

/* Return negative to indicate Unicode code point */
static int32_t
read_escape(parser_state *p)
{
  int32_t c;

  switch (c = nextc(p)) {
  case '\\':/* Backslash */
    return c;

  case 'n':/* newline */
    return '\n';

  case 't':/* horizontal tab */
    return '\t';

  case 'r':/* carriage-return */
    return '\r';

  case 'f':/* form-feed */
    return '\f';

  case 'v':/* vertical tab */
    return '\13';

  case 'a':/* alarm(bell) */
    return '\007';

  case 'e':/* escape */
    return 033;

  case '0': case '1': case '2': case '3': /* octal constant */
  case '4': case '5': case '6': case '7':
  {
    int buf[3];
    int i;

    buf[0] = c;
    for (i=1; i<3; i++) {
      buf[i] = nextc(p);
      if (buf[i] < 0) goto eof;
      if (buf[i] < '0' || '7' < buf[i]) {
        pushback(p, buf[i]);
        break;
      }
    }
    c = scan_oct(buf, i, &i);
  }
  return c;

  case 'x':     /* hex constant */
  {
    int buf[2];
    int i;

    for (i=0; i<2; i++) {
      buf[i] = nextc(p);
      if (buf[i] < 0) goto eof;
      if (!ISXDIGIT(buf[i])) {
        pushback(p, buf[i]);
        break;
      }
    }
    c = scan_hex(buf, i, &i);
    if (i == 0) {
      yyerror(p, "Invalid escape character syntax");
      return 0;
    }
  }
  return c;

  case 'u':     /* Unicode */
  {
    int buf[9];
    int i;

    /* Look for opening brace */
    i = 0;
    buf[0] = nextc(p);
    if (buf[0] < 0) goto eof;
    if (buf[0] == '{') {
      /* \u{xxxxxxxx} form */
      for (i=0; i<9; i++) {
        buf[i] = nextc(p);
        if (buf[i] < 0) goto eof;
        if (buf[i] == '}') {
          break;
        }
        else if (!ISXDIGIT(buf[i])) {
          yyerror(p, "Invalid escape character syntax");
          pushback(p, buf[i]);
          return 0;
        }
      }
    }
    else if (ISXDIGIT(buf[0])) {
      /* \uxxxx form */
      for (i=1; i<4; i++) {
        buf[i] = nextc(p);
        if (buf[i] < 0) goto eof;
        if (!ISXDIGIT(buf[i])) {
          pushback(p, buf[i]);
          break;
        }
      }
    }
    else {
      pushback(p, buf[0]);
    }
    c = scan_hex(buf, i, &i);
    if (i == 0) {
      yyerror(p, "Invalid escape character syntax");
      return 0;
    }
    if (c < 0 || c > 0x10FFFF || (c & 0xFFFFF800) == 0xD800) {
      yyerror(p, "Invalid Unicode code point");
      return 0;
    }
  }
  return -c;

  case 'b':/* backspace */
    return '\010';

  case 's':/* space */
    return ' ';

  case 'M':
    if ((c = nextc(p)) != '-') {
      yyerror(p, "Invalid escape character syntax");
      pushback(p, c);
      return '\0';
    }
    if ((c = nextc(p)) == '\\') {
      return read_escape(p) | 0x80;
    }
    else if (c < 0) goto eof;
    else {
      return ((c & 0xff) | 0x80);
    }

  case 'C':
    if ((c = nextc(p)) != '-') {
      yyerror(p, "Invalid escape character syntax");
      pushback(p, c);
      return '\0';
    }
  case 'c':
    if ((c = nextc(p))== '\\') {
      c = read_escape(p);
    }
    else if (c == '?')
      return 0177;
    else if (c < 0) goto eof;
    return c & 0x9f;

    eof:
  case -1:
  case -2:                      /* end of a file */
    yyerror(p, "Invalid escape character syntax");
    return '\0';

  default:
    return c;
  }
}

static int
parse_string(parser_state *p)
{
  int c;
  string_type type = (string_type)(intptr_t)p->lex_strterm->car;
  int nest_level = (intptr_t)p->lex_strterm->cdr->car;
  int beg = (intptr_t)p->lex_strterm->cdr->cdr->car;
  int end = (intptr_t)p->lex_strterm->cdr->cdr->cdr;
  parser_heredoc_info *hinf = (type & STR_FUNC_HEREDOC) ? parsing_heredoc_inf(p) : NULL;

  newtok(p);
  while ((c = nextc(p)) != end || nest_level != 0) {
    if (hinf && (c == '\n' || c < 0)) {
      mrb_bool line_head;
      tokadd(p, '\n');
      tokfix(p);
      p->lineno++;
      p->column = 0;
      line_head = hinf->line_head;
      hinf->line_head = TRUE;
      if (line_head) {
        /* check whether end of heredoc */
        const char *s = tok(p);
        int len = toklen(p);
        if (hinf->allow_indent) {
          while (ISSPACE(*s) && len > 0) {
            ++s;
            --len;
          }
        }
        if ((len-1 == hinf->term_len) && (strncmp(s, hinf->term, len-1) == 0)) {
          return tHEREDOC_END;
        }
      }
      if (c < 0) {
        char buf[256];
        snprintf(buf, sizeof(buf), "can't find heredoc delimiter \"%s\" anywhere before EOF", hinf->term);
        yyerror(p, buf);
        return 0;
      }
      yylval.nd = new_str(p, tok(p), toklen(p));
      return tHD_STRING_MID;
    }
    if (c < 0) {
      yyerror(p, "unterminated string meets end of file");
      return 0;
    }
    else if (c == beg) {
      nest_level++;
      p->lex_strterm->cdr->car = (node*)(intptr_t)nest_level;
    }
    else if (c == end) {
      nest_level--;
      p->lex_strterm->cdr->car = (node*)(intptr_t)nest_level;
    }
    else if (c == '\\') {
      c = nextc(p);
      if (type & STR_FUNC_EXPAND) {
        if (c == end || c == beg) {
          tokadd(p, c);
        }
        else if (c == '\n') {
          p->lineno++;
          p->column = 0;
          if (type & STR_FUNC_ARRAY) {
            tokadd(p, '\n');
          }
        }
        else if (type & STR_FUNC_REGEXP) {
          tokadd(p, '\\');
          tokadd(p, c);
        }
        else {
          pushback(p, c);
          tokadd(p, read_escape(p));
          if (hinf)
            hinf->line_head = FALSE;
        }
      }
      else {
        if (c != beg && c != end) {
          if (c == '\n') {
            p->lineno++;
            p->column = 0;
          }
          if (!(c == '\\' || ((type & STR_FUNC_ARRAY) && ISSPACE(c)))) {
            tokadd(p, '\\');
          }
        }
        tokadd(p, c);
      }
      continue;
    }
    else if ((c == '#') && (type & STR_FUNC_EXPAND)) {
      c = nextc(p);
      if (c == '{') {
        tokfix(p);
        p->lstate = EXPR_BEG;
        p->cmd_start = TRUE;
        yylval.nd = new_str(p, tok(p), toklen(p));
        if (hinf) {
          hinf->line_head = FALSE;
          return tHD_STRING_PART;
        }
        return tSTRING_PART;
      }
      tokadd(p, '#');
      pushback(p, c);
      continue;
    }
    if ((type & STR_FUNC_ARRAY) && ISSPACE(c)) {
      if (toklen(p) == 0) {
        do {
          if (c == '\n') {
            p->lineno++;
            p->column = 0;
            heredoc_treat_nextline(p);
            if (p->parsing_heredoc != NULL) {
              return tHD_LITERAL_DELIM;
            }
          }
          c = nextc(p);
        } while (ISSPACE(c));
        pushback(p, c);
        return tLITERAL_DELIM;
      }
      else {
        pushback(p, c);
        tokfix(p);
        yylval.nd = new_str(p, tok(p), toklen(p));
        return tSTRING_MID;
      }
    }
    tokadd(p, c);
  }

  tokfix(p);
  p->lstate = EXPR_END;
  end_strterm(p);

  if (type & STR_FUNC_XQUOTE) {
    yylval.nd = new_xstr(p, tok(p), toklen(p));
    return tXSTRING;
  }

  if (type & STR_FUNC_REGEXP) {
    int f = 0;
    int re_opt;
    char *s = strndup(tok(p), toklen(p));
    char flags[3];
    char *flag = flags;
    char *dup;

    newtok(p);
    while (re_opt = nextc(p), re_opt >= 0 && ISALPHA(re_opt)) {
      switch (re_opt) {
      case 'i': f |= 1; break;
      case 'x': f |= 2; break;
      case 'm': f |= 4; break;
      default: tokadd(p, re_opt); break;
      }
    }
    pushback(p, re_opt);
    if (toklen(p)) {
      char msg[128];
      tokfix(p);
      snprintf(msg, sizeof(msg), "unknown regexp option%s - %s",
          toklen(p) > 1 ? "s" : "", tok(p));
      yyerror(p, msg);
    }
    if (f != 0) {
      if (f & 1) *flag++ = 'i';
      if (f & 2) *flag++ = 'x';
      if (f & 4) *flag++ = 'm';
      dup = strndup(flags, (size_t)(flag - flags));
    }
    else {
      dup = NULL;
    }
    yylval.nd = new_regx(p, s, dup);

    return tREGEXP;
  }

  yylval.nd = new_str(p, tok(p), toklen(p));
  return tSTRING;
}


static int
heredoc_identifier(parser_state *p)
{
  int c;
  int type = str_heredoc;
  mrb_bool indent = FALSE;
  mrb_bool quote = FALSE;
  node *newnode;
  parser_heredoc_info *info;

  c = nextc(p);
  if (ISSPACE(c) || c == '=') {
    pushback(p, c);
    return 0;
  }
  if (c == '-') {
    indent = TRUE;
    c = nextc(p);
  }
  if (c == '\'' || c == '"') {
    int term = c;
    if (c == '\'')
      quote = TRUE;
    newtok(p);
    while ((c = nextc(p)) >= 0 && c != term) {
      if (c == '\n') {
        c = -1;
        break;
      }
      tokadd(p, c);
    }
    if (c < 0) {
      yyerror(p, "unterminated here document identifier");
      return 0;
    }
  }
  else {
    if (c < 0) {
      return 0;                 /* missing here document identifier */
    }
    if (! identchar(c)) {
      pushback(p, c);
      if (indent) pushback(p, '-');
      return 0;
    }
    newtok(p);
    do {
      tokadd(p, c);
    } while ((c = nextc(p)) >= 0 && identchar(c));
    pushback(p, c);
  }
  tokfix(p);
  newnode = new_heredoc(p);
  info = (parser_heredoc_info*)newnode->cdr;
  info->term = strndup(tok(p), toklen(p));
  info->term_len = toklen(p);
  if (! quote)
    type |= STR_FUNC_EXPAND;
  info->type = (string_type)type;
  info->allow_indent = indent;
  info->line_head = TRUE;
  info->doc = NULL;
  p->heredocs_from_nextline = push(p->heredocs_from_nextline, newnode);
  p->lstate = EXPR_END;

  yylval.nd = newnode;
  return tHEREDOC_BEG;
}

static int
arg_ambiguous(parser_state *p)
{
  yywarning(p, "ambiguous first argument; put parentheses or even spaces");
  return 1;
}

#include "lex.def"

static int
parser_yylex(parser_state *p)
{
  int32_t c;
  int space_seen = 0;
  int cmd_state;
  enum mrb_lex_state_enum last_state;
  int token_column;

  if (p->lex_strterm) {
    if (is_strterm_type(p, STR_FUNC_HEREDOC)) {
      if (p->parsing_heredoc != NULL)
        return parse_string(p);
    }
    else
      return parse_string(p);
  }
  cmd_state = p->cmd_start;
  p->cmd_start = FALSE;
  retry:
  last_state = p->lstate;
  switch (c = nextc(p)) {
  case '\004':  /* ^D */
  case '\032':  /* ^Z */
  case '\0':    /* NUL */
  case -1:      /* end of script. */
    if (p->heredocs_from_nextline)
      goto maybe_heredoc;
    return 0;

  /* white spaces */
  case ' ': case '\t': case '\f': case '\r':
  case '\13':   /* '\v' */
    space_seen = 1;
    goto retry;

  case '#':     /* it's a comment */
    skip(p, '\n');
    /* fall through */
  case -2:      /* end of a file */
  case '\n':
    maybe_heredoc:
    heredoc_treat_nextline(p);
  switch (p->lstate) {
  case EXPR_BEG:
  case EXPR_FNAME:
  case EXPR_DOT:
  case EXPR_CLASS:
  case EXPR_VALUE:
    p->lineno++;
    p->column = 0;
    if (p->parsing_heredoc != NULL) {
      if (p->lex_strterm) {
        return parse_string(p);
      }
    }
    goto retry;
  default:
    break;
  }
  if (p->parsing_heredoc != NULL) {
    return '\n';
  }
  while ((c = nextc(p))) {
    switch (c) {
    case ' ': case '\t': case '\f': case '\r':
    case '\13': /* '\v' */
      space_seen = 1;
      break;
    case '.':
      if ((c = nextc(p)) != '.') {
        pushback(p, c);
        pushback(p, '.');
        goto retry;
      }
    case -1:                  /* EOF */
    case -2:                  /* end of a file */
      goto normal_newline;
    default:
      pushback(p, c);
      goto normal_newline;
    }
  }
  normal_newline:
  p->cmd_start = TRUE;
  p->lstate = EXPR_BEG;
  return '\n';

  case '*':
    if ((c = nextc(p)) == '*') {
      if ((c = nextc(p)) == '=') {
        yylval.id = intern("**",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      c = tPOW;
    }
    else {
      if (c == '=') {
        yylval.id = intern_c('*');
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      if (IS_SPCARG(c)) {
        yywarning(p, "'*' interpreted as argument prefix");
        c = tSTAR;
      }
      else if (IS_BEG()) {
        c = tSTAR;
      }
      else {
        c = '*';
      }
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return c;

  case '!':
    c = nextc(p);
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if (c == '@') {
        return '!';
      }
    }
    else {
      p->lstate = EXPR_BEG;
    }
    if (c == '=') {
      return tNEQ;
    }
    if (c == '~') {
      return tNMATCH;
    }
    pushback(p, c);
    return '!';

  case '=':
    if (p->column == 1) {
      static const char begin[] = "begin";
      static const char end[] = "\n=end";
      if (peeks(p, begin)) {
        c = peekc_n(p, sizeof(begin)-1);
        if (c < 0 || ISSPACE(c)) {
          do {
            if (!skips(p, end)) {
              yyerror(p, "embedded document meets end of file");
              return 0;
            }
            c = nextc(p);
          } while (!(c < 0 || ISSPACE(c)));
          if (c != '\n') skip(p, '\n');
          p->lineno++;
          p->column = 0;
          goto retry;
        }
      }
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    if ((c = nextc(p)) == '=') {
      if ((c = nextc(p)) == '=') {
        return tEQQ;
      }
      pushback(p, c);
      return tEQ;
    }
    if (c == '~') {
      return tMATCH;
    }
    else if (c == '>') {
      return tASSOC;
    }
    pushback(p, c);
    return '=';

  case '<':
    c = nextc(p);
    if (c == '<' &&
        p->lstate != EXPR_DOT &&
        p->lstate != EXPR_CLASS &&
        !IS_END() &&
        (!IS_ARG() || space_seen)) {
      int token = heredoc_identifier(p);
      if (token)
        return token;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
      if (p->lstate == EXPR_CLASS) {
        p->cmd_start = TRUE;
      }
    }
    if (c == '=') {
      if ((c = nextc(p)) == '>') {
        return tCMP;
      }
      pushback(p, c);
      return tLEQ;
    }
    if (c == '<') {
      if ((c = nextc(p)) == '=') {
        yylval.id = intern("<<",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tLSHFT;
    }
    pushback(p, c);
    return '<';

  case '>':
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    if ((c = nextc(p)) == '=') {
      return tGEQ;
    }
    if (c == '>') {
      if ((c = nextc(p)) == '=') {
        yylval.id = intern(">>",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tRSHFT;
    }
    pushback(p, c);
    return '>';

  case '"':
    p->lex_strterm = new_strterm(p, str_dquote, '"', 0);
    return tSTRING_BEG;

  case '\'':
    p->lex_strterm = new_strterm(p, str_squote, '\'', 0);
    return parse_string(p);

  case '`':
    if (p->lstate == EXPR_FNAME) {
      p->lstate = EXPR_ENDFN;
      return '`';
    }
    if (p->lstate == EXPR_DOT) {
      if (cmd_state)
        p->lstate = EXPR_CMDARG;
      else
        p->lstate = EXPR_ARG;
      return '`';
    }
    p->lex_strterm = new_strterm(p, str_xquote, '`', 0);
    return tXSTRING_BEG;

  case '?':
    if (IS_END()) {
      p->lstate = EXPR_VALUE;
      return '?';
    }
    c = nextc(p);
    if (c < 0) {
      yyerror(p, "incomplete character syntax");
      return 0;
    }
    if (ISSPACE(c)) {
      if (!IS_ARG()) {
        int c2;
        switch (c) {
        case ' ':
          c2 = 's';
          break;
        case '\n':
          c2 = 'n';
          break;
        case '\t':
          c2 = 't';
          break;
        case '\v':
          c2 = 'v';
          break;
        case '\r':
          c2 = 'r';
          break;
        case '\f':
          c2 = 'f';
          break;
        default:
          c2 = 0;
          break;
        }
        if (c2) {
          char buf[256];
          snprintf(buf, sizeof(buf), "invalid character syntax; use ?\\%c", c2);
          yyerror(p, buf);
        }
      }
      ternary:
      pushback(p, c);
      p->lstate = EXPR_VALUE;
      return '?';
    }
    newtok(p);
    /* need support UTF-8 if configured */
    if ((isalnum(c) || c == '_')) {
      int c2 = nextc(p);
      pushback(p, c2);
      if ((isalnum(c2) || c2 == '_')) {
        goto ternary;
      }
    }
    if (c == '\\') {
      c = read_escape(p);
      tokadd(p, c);
    }
    else {
      tokadd(p, c);
    }
    tokfix(p);
    yylval.nd = new_str(p, tok(p), toklen(p));
    p->lstate = EXPR_END;
    return tCHAR;

  case '&':
    if ((c = nextc(p)) == '&') {
      p->lstate = EXPR_BEG;
      if ((c = nextc(p)) == '=') {
        yylval.id = intern("&&",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tANDOP;
    }
    else if (c == '=') {
      yylval.id = intern_c('&');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    pushback(p, c);
    if (IS_SPCARG(c)) {
      yywarning(p, "'&' interpreted as argument prefix");
      c = tAMPER;
    }
    else if (IS_BEG()) {
      c = tAMPER;
    }
    else {
      c = '&';
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return c;

  case '|':
    if ((c = nextc(p)) == '|') {
      p->lstate = EXPR_BEG;
      if ((c = nextc(p)) == '=') {
        yylval.id = intern("||",2);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tOROP;
    }
    if (c == '=') {
      yylval.id = intern_c('|');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    pushback(p, c);
    return '|';

  case '+':
    c = nextc(p);
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if (c == '@') {
        return tUPLUS;
      }
      pushback(p, c);
      return '+';
    }
    if (c == '=') {
      yylval.id = intern_c('+');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (IS_BEG() || (IS_SPCARG(c) && arg_ambiguous(p))) {
      p->lstate = EXPR_BEG;
      pushback(p, c);
      if (c >= 0 && ISDIGIT(c)) {
        c = '+';
        goto start_num;
      }
      return tUPLUS;
    }
    p->lstate = EXPR_BEG;
    pushback(p, c);
    return '+';

  case '-':
    c = nextc(p);
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if (c == '@') {
        return tUMINUS;
      }
      pushback(p, c);
      return '-';
    }
    if (c == '=') {
      yylval.id = intern_c('-');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (c == '>') {
      p->lstate = EXPR_ENDFN;
      return tLAMBDA;
    }
    if (IS_BEG() || (IS_SPCARG(c) && arg_ambiguous(p))) {
      p->lstate = EXPR_BEG;
      pushback(p, c);
      if (c >= 0 && ISDIGIT(c)) {
        return tUMINUS_NUM;
      }
      return tUMINUS;
    }
    p->lstate = EXPR_BEG;
    pushback(p, c);
    return '-';

  case '.':
    p->lstate = EXPR_BEG;
    if ((c = nextc(p)) == '.') {
      if ((c = nextc(p)) == '.') {
        return tDOT3;
      }
      pushback(p, c);
      return tDOT2;
    }
    pushback(p, c);
    if (c >= 0 && ISDIGIT(c)) {
      yyerror(p, "no .<digit> floating literal anymore; put 0 before dot");
    }
    p->lstate = EXPR_DOT;
    return '.';

    start_num:
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
  {
    int is_float, seen_point, seen_e, nondigit;

    is_float = seen_point = seen_e = nondigit = 0;
    p->lstate = EXPR_END;
    newtok(p);
    if (c == '-' || c == '+') {
      tokadd(p, c);
      c = nextc(p);
    }
    if (c == '0') {
#define no_digits() do {yyerror(p,"numeric literal without digits"); return 0;} while (0)
      int start = toklen(p);
      c = nextc(p);
      if (c == 'x' || c == 'X') {
        /* hexadecimal */
        c = nextc(p);
        if (c >= 0 && ISXDIGIT(c)) {
          do {
            if (c == '_') {
              if (nondigit) break;
              nondigit = c;
              continue;
            }
            if (!ISXDIGIT(c)) break;
            nondigit = 0;
            tokadd(p, tolower(c));
          } while ((c = nextc(p)) >= 0);
        }
        pushback(p, c);
        tokfix(p);
        if (toklen(p) == start) {
          no_digits();
        }
        else if (nondigit) goto trailing_uc;
        yylval.nd = new_int(p, tok(p), 16);
        return tINTEGER;
      }
      if (c == 'b' || c == 'B') {
        /* binary */
        c = nextc(p);
        if (c == '0' || c == '1') {
          do {
            if (c == '_') {
              if (nondigit) break;
              nondigit = c;
              continue;
            }
            if (c != '0' && c != '1') break;
            nondigit = 0;
            tokadd(p, c);
          } while ((c = nextc(p)) >= 0);
        }
        pushback(p, c);
        tokfix(p);
        if (toklen(p) == start) {
          no_digits();
        }
        else if (nondigit) goto trailing_uc;
        yylval.nd = new_int(p, tok(p), 2);
        return tINTEGER;
      }
      if (c == 'd' || c == 'D') {
        /* decimal */
        c = nextc(p);
        if (c >= 0 && ISDIGIT(c)) {
          do {
            if (c == '_') {
              if (nondigit) break;
              nondigit = c;
              continue;
            }
            if (!ISDIGIT(c)) break;
            nondigit = 0;
            tokadd(p, c);
          } while ((c = nextc(p)) >= 0);
        }
        pushback(p, c);
        tokfix(p);
        if (toklen(p) == start) {
          no_digits();
        }
        else if (nondigit) goto trailing_uc;
        yylval.nd = new_int(p, tok(p), 10);
        return tINTEGER;
      }
      if (c == '_') {
        /* 0_0 */
        goto octal_number;
      }
      if (c == 'o' || c == 'O') {
        /* prefixed octal */
        c = nextc(p);
        if (c < 0 || c == '_' || !ISDIGIT(c)) {
          no_digits();
        }
      }
      if (c >= '0' && c <= '7') {
        /* octal */
        octal_number:
        do {
          if (c == '_') {
            if (nondigit) break;
            nondigit = c;
            continue;
          }
          if (c < '0' || c > '9') break;
          if (c > '7') goto invalid_octal;
          nondigit = 0;
          tokadd(p, c);
        } while ((c = nextc(p)) >= 0);

        if (toklen(p) > start) {
          pushback(p, c);
          tokfix(p);
          if (nondigit) goto trailing_uc;
          yylval.nd = new_int(p, tok(p), 8);
          return tINTEGER;
        }
        if (nondigit) {
          pushback(p, c);
          goto trailing_uc;
        }
      }
      if (c > '7' && c <= '9') {
        invalid_octal:
        yyerror(p, "Invalid octal digit");
      }
      else if (c == '.' || c == 'e' || c == 'E') {
        tokadd(p, '0');
      }
      else {
        pushback(p, c);
        yylval.nd = new_int(p, "0", 10);
        return tINTEGER;
      }
    }

    for (;;) {
      switch (c) {
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        nondigit = 0;
        tokadd(p, c);
        break;

      case '.':
        if (nondigit) goto trailing_uc;
        if (seen_point || seen_e) {
          goto decode_num;
        }
        else {
          int c0 = nextc(p);
          if (c0 < 0 || !ISDIGIT(c0)) {
            pushback(p, c0);
            goto decode_num;
          }
          c = c0;
        }
        tokadd(p, '.');
        tokadd(p, c);
        is_float++;
        seen_point++;
        nondigit = 0;
        break;

      case 'e':
      case 'E':
        if (nondigit) {
          pushback(p, c);
          c = nondigit;
          goto decode_num;
        }
        if (seen_e) {
          goto decode_num;
        }
        tokadd(p, c);
        seen_e++;
        is_float++;
        nondigit = c;
        c = nextc(p);
        if (c != '-' && c != '+') continue;
        tokadd(p, c);
        nondigit = c;
        break;

      case '_':       /* '_' in number just ignored */
        if (nondigit) goto decode_num;
        nondigit = c;
        break;

      default:
        goto decode_num;
      }
      c = nextc(p);
    }

    decode_num:
    pushback(p, c);
    if (nondigit) {
      trailing_uc:
      yyerror_i(p, "trailing '%c' in number", nondigit);
    }
    tokfix(p);
    if (is_float) {
      double d;
      char *endp;

      errno = 0;
      d = strtod(tok(p), &endp);
      if (d == 0 && endp == tok(p)) {
        yywarning_s(p, "corrupted float value %s", tok(p));
      }
      else if (errno == ERANGE) {
        yywarning_s(p, "float %s out of range", tok(p));
        errno = 0;
      }
      yylval.nd = new_float(p, tok(p));
      return tFLOAT;
    }
    yylval.nd = new_int(p, tok(p), 10);
    return tINTEGER;
  }

  case ')':
  case ']':
    p->paren_nest--;
    /* fall through */
  case '}':
    COND_LEXPOP();
    CMDARG_LEXPOP();
    if (c == ')')
      p->lstate = EXPR_ENDFN;
    else
      p->lstate = EXPR_ENDARG;
    return c;

  case ':':
    c = nextc(p);
    if (c == ':') {
      if (IS_BEG() || p->lstate == EXPR_CLASS || IS_SPCARG(-1)) {
        p->lstate = EXPR_BEG;
        return tCOLON3;
      }
      p->lstate = EXPR_DOT;
      return tCOLON2;
    }
    if (IS_END() || ISSPACE(c)) {
      pushback(p, c);
      p->lstate = EXPR_BEG;
      return ':';
    }
    pushback(p, c);
    p->lstate = EXPR_FNAME;
    return tSYMBEG;

  case '/':
    if (IS_BEG()) {
      p->lex_strterm = new_strterm(p, str_regexp, '/', 0);
      return tREGEXP_BEG;
    }
    if ((c = nextc(p)) == '=') {
      yylval.id = intern_c('/');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    pushback(p, c);
    if (IS_SPCARG(c)) {
      p->lex_strterm = new_strterm(p, str_regexp, '/', 0);
      return tREGEXP_BEG;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return '/';

  case '^':
    if ((c = nextc(p)) == '=') {
      yylval.id = intern_c('^');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    pushback(p, c);
    return '^';

  case ';':
    p->lstate = EXPR_BEG;
    return ';';

  case ',':
    p->lstate = EXPR_BEG;
    return ',';

  case '~':
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      if ((c = nextc(p)) != '@') {
        pushback(p, c);
      }
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return '~';

  case '(':
    if (IS_BEG()) {
      c = tLPAREN;
    }
    else if (IS_SPCARG(-1)) {
      c = tLPAREN_ARG;
    }
    p->paren_nest++;
    COND_PUSH(0);
    CMDARG_PUSH(0);
    p->lstate = EXPR_BEG;
    return c;

  case '[':
    p->paren_nest++;
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if ((c = nextc(p)) == ']') {
        if ((c = nextc(p)) == '=') {
          return tASET;
        }
        pushback(p, c);
        return tAREF;
      }
      pushback(p, c);
      return '[';
    }
    else if (IS_BEG()) {
      c = tLBRACK;
    }
    else if (IS_ARG() && space_seen) {
      c = tLBRACK;
    }
    p->lstate = EXPR_BEG;
    COND_PUSH(0);
    CMDARG_PUSH(0);
    return c;

  case '{':
    if (p->lpar_beg && p->lpar_beg == p->paren_nest) {
      p->lstate = EXPR_BEG;
      p->lpar_beg = 0;
      p->paren_nest--;
      COND_PUSH(0);
      CMDARG_PUSH(0);
      return tLAMBEG;
    }
    if (IS_ARG() || p->lstate == EXPR_END || p->lstate == EXPR_ENDFN)
      c = '{';          /* block (primary) */
    else if (p->lstate == EXPR_ENDARG)
      c = tLBRACE_ARG;  /* block (expr) */
    else
      c = tLBRACE;      /* hash */
    COND_PUSH(0);
    CMDARG_PUSH(0);
    p->lstate = EXPR_BEG;
    return c;

  case '\\':
    c = nextc(p);
    if (c == '\n') {
      p->lineno++;
      p->column = 0;
      space_seen = 1;
      goto retry; /* skip \\n */
    }
    pushback(p, c);
    return '\\';

  case '%':
    if (IS_BEG()) {
      int term;
      int paren;

      c = nextc(p);
      quotation:
      if (c < 0 || !ISALNUM(c)) {
        term = c;
        c = 'Q';
      }
      else {
        term = nextc(p);
        if (isalnum(term)) {
          yyerror(p, "unknown type of %string");
          return 0;
        }
      }
      if (c < 0 || term < 0) {
        yyerror(p, "unterminated quoted string meets end of file");
        return 0;
      }
      paren = term;
      if (term == '(') term = ')';
      else if (term == '[') term = ']';
      else if (term == '{') term = '}';
      else if (term == '<') term = '>';
      else paren = 0;

      switch (c) {
      case 'Q':
        p->lex_strterm = new_strterm(p, str_dquote, term, paren);
        return tSTRING_BEG;

      case 'q':
        p->lex_strterm = new_strterm(p, str_squote, term, paren);
        return parse_string(p);

      case 'W':
        p->lex_strterm = new_strterm(p, str_dword, term, paren);
        return tWORDS_BEG;

      case 'w':
        p->lex_strterm = new_strterm(p, str_sword, term, paren);
        return tWORDS_BEG;

      case 'x':
        p->lex_strterm = new_strterm(p, str_xquote, term, paren);
        return tXSTRING_BEG;

      case 'r':
        p->lex_strterm = new_strterm(p, str_regexp, term, paren);
        return tREGEXP_BEG;

      case 's':
        p->lex_strterm = new_strterm(p, str_ssym, term, paren);
        return tSYMBEG;

      case 'I':
        p->lex_strterm = new_strterm(p, str_dsymbols, term, paren);
        return tSYMBOLS_BEG;

      case 'i':
        p->lex_strterm = new_strterm(p, str_ssymbols, term, paren);
        return tSYMBOLS_BEG;

      default:
        yyerror(p, "unknown type of %string");
        return 0;
      }
    }
    if ((c = nextc(p)) == '=') {
      yylval.id = intern_c('%');
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (IS_SPCARG(c)) {
      goto quotation;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    pushback(p, c);
    return '%';

  case '$':
    p->lstate = EXPR_END;
    token_column = newtok(p);
    c = nextc(p);
    if (c < 0) {
      yyerror(p, "incomplete global variable syntax");
      return 0;
    }
    switch (c) {
    case '_':     /* $_: last read line string */
      c = nextc(p);
      if (c >= 0 && identchar(c)) { /* if there is more after _ it is a variable */
        tokadd(p, '$');
        tokadd(p, c);
        break;
      }
      pushback(p, c);
      c = '_';
      /* fall through */
    case '~':     /* $~: match-data */
    case '*':     /* $*: argv */
    case '$':     /* $$: pid */
    case '?':     /* $?: last status */
    case '!':     /* $!: error string */
    case '@':     /* $@: error position */
    case '/':     /* $/: input record separator */
    case '\\':    /* $\: output record separator */
    case ';':     /* $;: field separator */
    case ',':     /* $,: output field separator */
    case '.':     /* $.: last read line number */
    case '=':     /* $=: ignorecase */
    case ':':     /* $:: load path */
    case '<':     /* $<: reading filename */
    case '>':     /* $>: default output handle */
    case '\"':    /* $": already loaded files */
      tokadd(p, '$');
      tokadd(p, c);
      tokfix(p);
      yylval.id = intern_cstr(tok(p));
      return tGVAR;

    case '-':
      tokadd(p, '$');
      tokadd(p, c);
      c = nextc(p);
      pushback(p, c);
      gvar:
      tokfix(p);
      yylval.id = intern_cstr(tok(p));
      return tGVAR;

    case '&':     /* $&: last match */
    case '`':     /* $`: string before last match */
    case '\'':    /* $': string after last match */
    case '+':     /* $+: string matches last pattern */
      if (last_state == EXPR_FNAME) {
        tokadd(p, '$');
        tokadd(p, c);
        goto gvar;
      }
      yylval.nd = new_back_ref(p, c);
      return tBACK_REF;

    case '1': case '2': case '3':
    case '4': case '5': case '6':
    case '7': case '8': case '9':
      do {
        tokadd(p, c);
        c = nextc(p);
      } while (c >= 0 && isdigit(c));
      pushback(p, c);
      if (last_state == EXPR_FNAME) goto gvar;
      tokfix(p);
      {
        unsigned long n = strtoul(tok(p), NULL, 10);
        if (n > INT_MAX) {
          yyerror_i(p, "capture group index must be <= %d", INT_MAX);
          return 0;
        }
        yylval.nd = new_nth_ref(p, (int)n);
      }
      return tNTH_REF;

    default:
      if (!identchar(c)) {
        pushback(p,  c);
        return '$';
      }
      /* fall through */
    case '0':
      tokadd(p, '$');
    }
    break;

    case '@':
      c = nextc(p);
      token_column = newtok(p);
      tokadd(p, '@');
      if (c == '@') {
        tokadd(p, '@');
        c = nextc(p);
      }
      if (c < 0) {
        if (p->bidx == 1) {
          yyerror(p, "incomplete instance variable syntax");
        }
        else {
          yyerror(p, "incomplete class variable syntax");
        }
        return 0;
      }
      else if (isdigit(c)) {
        if (p->bidx == 1) {
          yyerror_i(p, "'@%c' is not allowed as an instance variable name", c);
        }
        else {
          yyerror_i(p, "'@@%c' is not allowed as a class variable name", c);
        }
        return 0;
      }
      if (!identchar(c)) {
        pushback(p, c);
        return '@';
      }
      break;

    case '_':
      token_column = newtok(p);
      break;

    default:
      if (!identchar(c)) {
        yyerror_i(p,  "Invalid char '\\x%02X' in expression", c);
        goto retry;
      }

      token_column = newtok(p);
      break;
  }

  do {
    tokadd(p, c);
    c = nextc(p);
    if (c < 0) break;
  } while (identchar(c));
  if (token_column == 0 && toklen(p) == 7 && (c < 0 || c == '\n') &&
      strncmp(tok(p), "__END__", toklen(p)) == 0)
    return -1;

  switch (tok(p)[0]) {
  case '@': case '$':
    pushback(p, c);
    break;
  default:
    if ((c == '!' || c == '?') && !peek(p, '=')) {
      tokadd(p, c);
    }
    else {
      pushback(p, c);
    }
  }
  tokfix(p);
  {
    int result = 0;

    switch (tok(p)[0]) {
    case '$':
      p->lstate = EXPR_END;
      result = tGVAR;
      break;
    case '@':
      p->lstate = EXPR_END;
      if (tok(p)[1] == '@')
        result = tCVAR;
      else
        result = tIVAR;
      break;

    default:
      if (toklast(p) == '!' || toklast(p) == '?') {
        result = tFID;
      }
      else {
        if (p->lstate == EXPR_FNAME) {
          if ((c = nextc(p)) == '=' && !peek(p, '~') && !peek(p, '>') &&
              (!peek(p, '=') || (peek_n(p, '>', 1)))) {
            result = tIDENTIFIER;
            tokadd(p, c);
            tokfix(p);
          }
          else {
            pushback(p, c);
          }
        }
        if (result == 0 && ISUPPER(tok(p)[0])) {
          result = tCONSTANT;
        }
        else {
          result = tIDENTIFIER;
        }
      }

      if (IS_LABEL_POSSIBLE()) {
        if (IS_LABEL_SUFFIX(0)) {
          p->lstate = EXPR_BEG;
          nextc(p);
          tokfix(p);
          yylval.id = intern_cstr(tok(p));
          return tLABEL;
        }
      }
      if (p->lstate != EXPR_DOT) {
        const struct kwtable *kw;

        /* See if it is a reserved word.  */
        kw = mrb_reserved_word(tok(p), toklen(p));
        if (kw) {
          enum mrb_lex_state_enum state = p->lstate;
          yylval.num = p->lineno;
          p->lstate = kw->state;
          if (state == EXPR_FNAME) {
            yylval.id = intern_cstr(kw->name);
            return kw->id[0];
          }
          if (p->lstate == EXPR_BEG) {
            p->cmd_start = TRUE;
          }
          if (kw->id[0] == keyword_do) {
            if (p->lpar_beg && p->lpar_beg == p->paren_nest) {
              p->lpar_beg = 0;
              p->paren_nest--;
              return keyword_do_LAMBDA;
            }
            if (COND_P()) return keyword_do_cond;
            if (CMDARG_P() && state != EXPR_CMDARG)
              return keyword_do_block;
            if (state == EXPR_ENDARG || state == EXPR_BEG)
              return keyword_do_block;
            return keyword_do;
          }
          if (state == EXPR_BEG || state == EXPR_VALUE)
            return kw->id[0];
          else {
            if (kw->id[0] != kw->id[1])
              p->lstate = EXPR_BEG;
            return kw->id[1];
          }
        }
      }

      if (IS_BEG() || p->lstate == EXPR_DOT || IS_ARG()) {
        if (cmd_state) {
          p->lstate = EXPR_CMDARG;
        }
        else {
          p->lstate = EXPR_ARG;
        }
      }
      else if (p->lstate == EXPR_FNAME) {
        p->lstate = EXPR_ENDFN;
      }
      else {
        p->lstate = EXPR_END;
      }
    }
    {
      mrb_sym ident = intern_cstr(tok(p));

      yylval.id = ident;
#if 0
      if (last_state != EXPR_DOT && islower(tok(p)[0]) && lvar_defined(ident)) {
        p->lstate = EXPR_END;
      }
#endif
    }
    return result;
  }
}

static int
yylex(void *lval, parser_state *p)
{
  p->ylval = lval;
  return parser_yylex(p);
}

static void
parser_init_cxt(parser_state *p, mrbc_context *cxt)
{
  if (!cxt) return;
  if (cxt->filename) mrb_parser_set_filename(p, cxt->filename);
  if (cxt->lineno) p->lineno = cxt->lineno;
  if (cxt->syms) {
    int i;

    p->locals = cons(0,0);
    for (i=0; i<cxt->slen; i++) {
      local_add_f(p, cxt->syms[i]);
    }
  }
  p->capture_errors = cxt->capture_errors;
  p->no_optimize = cxt->no_optimize;
  if (cxt->partial_hook) {
    p->cxt = cxt;
  }
}

static void
parser_update_cxt(parser_state *p, mrbc_context *cxt)
{
  node *n, *n0;
  int i = 0;

  if (!cxt) return;
  if ((int)(intptr_t)p->tree->car != NODE_SCOPE) return;
  n0 = n = p->tree->cdr->car;
  while (n) {
    i++;
    n = n->cdr;
  }
  cxt->syms = (mrb_sym *)mrb_realloc(p->mrb, cxt->syms, i*sizeof(mrb_sym));
  cxt->slen = i;
  for (i=0, n=n0; n; i++,n=n->cdr) {
    cxt->syms[i] = sym(n->car);
  }
}

void mrb_codedump_all(mrb_state*, struct RProc*);
void mrb_parser_dump(mrb_state *mrb, node *tree, int offset);

MRB_API void
mrb_parser_parse(parser_state *p, mrbc_context *c)
{
  struct mrb_jmpbuf buf;
  p->jmp = &buf;

  MRB_TRY(p->jmp) {

    p->cmd_start = TRUE;
    p->in_def = p->in_single = 0;
    p->nerr = p->nwarn = 0;
    p->lex_strterm = NULL;

    parser_init_cxt(p, c);
    yyparse(p);
    if (!p->tree) {
      p->tree = new_nil(p);
    }
    parser_update_cxt(p, c);
    if (c && c->dump_result) {
      mrb_parser_dump(p->mrb, p->tree, 0);
    }

  }
  MRB_CATCH(p->jmp) {
    yyerror(p, "memory allocation error");
    p->nerr++;
    p->tree = 0;
    return;
  }
  MRB_END_EXC(p->jmp);
}

MRB_API parser_state*
mrb_parser_new(mrb_state *mrb)
{
  mrb_pool *pool;
  parser_state *p;
  static const parser_state parser_state_zero = { 0 };

  pool = mrb_pool_open(mrb);
  if (!pool) return NULL;
  p = (parser_state *)mrb_pool_alloc(pool, sizeof(parser_state));
  if (!p) return NULL;

  *p = parser_state_zero;
  p->mrb = mrb;
  p->pool = pool;

  p->s = p->send = NULL;
#ifdef ENABLE_STDIO
  p->f = NULL;
#endif

  p->cmd_start = TRUE;
  p->in_def = p->in_single = 0;

  p->capture_errors = FALSE;
  p->lineno = 1;
  p->column = 0;
#if defined(PARSER_TEST) || defined(PARSER_DEBUG)
  yydebug = 1;
#endif

  p->lex_strterm = NULL;
  p->all_heredocs = p->parsing_heredoc = NULL;
  p->lex_strterm_before_heredoc = NULL;

  p->current_filename_index = -1;
  p->filename_table = NULL;
  p->filename_table_length = 0;

  return p;
}

MRB_API void
mrb_parser_free(parser_state *p) {
  mrb_pool_close(p->pool);
}

MRB_API mrbc_context*
mrbc_context_new(mrb_state *mrb)
{
  return (mrbc_context *)mrb_calloc(mrb, 1, sizeof(mrbc_context));
}

MRB_API void
mrbc_context_free(mrb_state *mrb, mrbc_context *cxt)
{
  mrb_free(mrb, cxt->syms);
  mrb_free(mrb, cxt);
}

MRB_API const char*
mrbc_filename(mrb_state *mrb, mrbc_context *c, const char *s)
{
  if (s) {
    int len = strlen(s);
    char *p = (char *)mrb_alloca(mrb, len + 1);

    memcpy(p, s, len + 1);
    c->filename = p;
  }
  return c->filename;
}

MRB_API void
mrbc_partial_hook(mrb_state *mrb, mrbc_context *c, int (*func)(struct mrb_parser_state*), void *data)
{
  c->partial_hook = func;
  c->partial_data = data;
}

MRB_API void
mrb_parser_set_filename(struct mrb_parser_state *p, const char *f)
{
  mrb_sym sym;
  size_t i;
  mrb_sym* new_table;

  sym = mrb_intern_cstr(p->mrb, f);
  p->filename = mrb_sym2name_len(p->mrb, sym, NULL);
  p->lineno = (p->filename_table_length > 0)? 0 : 1;

  for (i = 0; i < p->filename_table_length; ++i) {
    if (p->filename_table[i] == sym) {
      p->current_filename_index = i;
      return;
    }
  }

  p->current_filename_index = p->filename_table_length++;

  new_table = (mrb_sym*)parser_palloc(p, sizeof(mrb_sym) * p->filename_table_length);
  if (p->filename_table) {
    memmove(new_table, p->filename_table, sizeof(mrb_sym) * p->filename_table_length);
  }
  p->filename_table = new_table;
  p->filename_table[p->filename_table_length - 1] = sym;
}

MRB_API char const*
mrb_parser_get_filename(struct mrb_parser_state* p, uint16_t idx) {
  if (idx >= p->filename_table_length) { return NULL; }
  else {
    return mrb_sym2name_len(p->mrb, p->filename_table[idx], NULL);
  }
}

#ifdef ENABLE_STDIO
MRB_API parser_state*
mrb_parse_file(mrb_state *mrb, FILE *f, mrbc_context *c)
{
  parser_state *p;

  p = mrb_parser_new(mrb);
  if (!p) return NULL;
  p->s = p->send = NULL;
  p->f = f;

  mrb_parser_parse(p, c);
  return p;
}
#endif

MRB_API parser_state*
mrb_parse_nstring(mrb_state *mrb, const char *s, int len, mrbc_context *c)
{
  parser_state *p;

  p = mrb_parser_new(mrb);
  if (!p) return NULL;
  p->s = s;
  p->send = s + len;

  mrb_parser_parse(p, c);
  return p;
}

MRB_API parser_state*
mrb_parse_string(mrb_state *mrb, const char *s, mrbc_context *c)
{
  return mrb_parse_nstring(mrb, s, strlen(s), c);
}

static mrb_value
load_exec(mrb_state *mrb, parser_state *p, mrbc_context *c)
{
  struct RClass *target = mrb->object_class;
  struct RProc *proc;
  mrb_value v;
  unsigned int keep = 0;

  if (!p) {
    return mrb_undef_value();
  }
  if (!p->tree || p->nerr) {
    if (p->capture_errors) {
      char buf[256];
      int n;

      n = snprintf(buf, sizeof(buf), "line %d: %s\n",
          p->error_buffer[0].lineno, p->error_buffer[0].message);
      mrb->exc = mrb_obj_ptr(mrb_exc_new(mrb, E_SYNTAX_ERROR, buf, n));
      mrb_parser_free(p);
      return mrb_undef_value();
    }
    else {
      mrb->exc = mrb_obj_ptr(mrb_exc_new_str_lit(mrb, E_SYNTAX_ERROR, "syntax error"));
      mrb_parser_free(p);
      return mrb_undef_value();
    }
  }
  proc = mrb_generate_code(mrb, p);
  mrb_parser_free(p);
  if (proc == NULL) {
    mrb->exc = mrb_obj_ptr(mrb_exc_new_str_lit(mrb, E_SCRIPT_ERROR, "codegen error"));
    return mrb_undef_value();
  }
  if (c) {
    if (c->dump_result) mrb_codedump_all(mrb, proc);
    if (c->no_exec) return mrb_obj_value(proc);
    if (c->target_class) {
      target = c->target_class;
    }
    if (c->keep_lv) {
      keep = c->slen + 1;
    }
    else {
      c->keep_lv = TRUE;
    }
  }
  proc->target_class = target;
  if (mrb->c->ci) {
    mrb->c->ci->target_class = target;
  }
  v = mrb_toplevel_run_keep(mrb, proc, keep);
  if (mrb->exc) return mrb_nil_value();
  return v;
}

#ifdef ENABLE_STDIO
MRB_API mrb_value
mrb_load_file_cxt(mrb_state *mrb, FILE *f, mrbc_context *c)
{
  return load_exec(mrb, mrb_parse_file(mrb, f, c), c);
}

MRB_API mrb_value
mrb_load_file(mrb_state *mrb, FILE *f)
{
  return mrb_load_file_cxt(mrb, f, NULL);
}
#endif

MRB_API mrb_value
mrb_load_nstring_cxt(mrb_state *mrb, const char *s, int len, mrbc_context *c)
{
  return load_exec(mrb, mrb_parse_nstring(mrb, s, len, c), c);
}

MRB_API mrb_value
mrb_load_nstring(mrb_state *mrb, const char *s, int len)
{
  return mrb_load_nstring_cxt(mrb, s, len, NULL);
}

MRB_API mrb_value
mrb_load_string_cxt(mrb_state *mrb, const char *s, mrbc_context *c)
{
  return mrb_load_nstring_cxt(mrb, s, strlen(s), c);
}

MRB_API mrb_value
mrb_load_string(mrb_state *mrb, const char *s)
{
  return mrb_load_string_cxt(mrb, s, NULL);
}

#ifdef ENABLE_STDIO

static void
dump_prefix(node *tree, int offset)
{
  printf("%05d ", tree->lineno);
  while (offset--) {
    putc(' ', stdout);
    putc(' ', stdout);
  }
}

static void
dump_recur(mrb_state *mrb, node *tree, int offset)
{
  while (tree) {
    mrb_parser_dump(mrb, tree->car, offset);
    tree = tree->cdr;
  }
}

#endif

void
mrb_parser_dump(mrb_state *mrb, node *tree, int offset)
{
#ifdef ENABLE_STDIO
  int nodetype;

  if (!tree) return;
  again:
  dump_prefix(tree, offset);
  nodetype = (int)(intptr_t)tree->car;
  tree = tree->cdr;
  switch (nodetype) {
  case NODE_BEGIN:
    printf("NODE_BEGIN:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_RESCUE:
    printf("NODE_RESCUE:\n");
    if (tree->car) {
      dump_prefix(tree, offset+1);
      printf("body:\n");
      mrb_parser_dump(mrb, tree->car, offset+2);
    }
    tree = tree->cdr;
    if (tree->car) {
      node *n2 = tree->car;

      dump_prefix(n2, offset+1);
      printf("rescue:\n");
      while (n2) {
        node *n3 = n2->car;
        if (n3->car) {
          dump_prefix(n2, offset+2);
          printf("handle classes:\n");
          dump_recur(mrb, n3->car, offset+3);
        }
        if (n3->cdr->car) {
          dump_prefix(n3, offset+2);
          printf("exc_var:\n");
          mrb_parser_dump(mrb, n3->cdr->car, offset+3);
        }
        if (n3->cdr->cdr->car) {
          dump_prefix(n3, offset+2);
          printf("rescue body:\n");
          mrb_parser_dump(mrb, n3->cdr->cdr->car, offset+3);
        }
        n2 = n2->cdr;
      }
    }
    tree = tree->cdr;
    if (tree->car) {
      dump_prefix(tree, offset+1);
      printf("else:\n");
      mrb_parser_dump(mrb, tree->car, offset+2);
    }
    break;

  case NODE_ENSURE:
    printf("NODE_ENSURE:\n");
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("ensure:\n");
    mrb_parser_dump(mrb, tree->cdr->cdr, offset+2);
    break;

  case NODE_LAMBDA:
    printf("NODE_BLOCK:\n");
    goto block;

  case NODE_BLOCK:
    block:
    printf("NODE_BLOCK:\n");
    tree = tree->cdr;
    if (tree->car) {
      node *n = tree->car;

      if (n->car) {
        dump_prefix(n, offset+1);
        printf("mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("optional args:\n");
        {
          node *n2 = n->car;

          while (n2) {
            dump_prefix(n2, offset+2);
            printf("%s=", mrb_sym2name(mrb, sym(n2->car->car)));
            mrb_parser_dump(mrb, n2->car->cdr, 0);
            n2 = n2->cdr;
          }
        }
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("rest=*%s\n", mrb_sym2name(mrb, sym(n->car)));
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("post mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n) {
        dump_prefix(n, offset+1);
        printf("blk=&%s\n", mrb_sym2name(mrb, sym(n)));
      }
    }
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->car, offset+2);
    break;

  case NODE_IF:
    printf("NODE_IF:\n");
    dump_prefix(tree, offset+1);
    printf("cond:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("then:\n");
    mrb_parser_dump(mrb, tree->cdr->car, offset+2);
    if (tree->cdr->cdr->car) {
      dump_prefix(tree, offset+1);
      printf("else:\n");
      mrb_parser_dump(mrb, tree->cdr->cdr->car, offset+2);
    }
    break;

  case NODE_AND:
    printf("NODE_AND:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_OR:
    printf("NODE_OR:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_CASE:
    printf("NODE_CASE:\n");
    if (tree->car) {
      mrb_parser_dump(mrb, tree->car, offset+1);
    }
    tree = tree->cdr;
    while (tree) {
      dump_prefix(tree, offset+1);
      printf("case:\n");
      dump_recur(mrb, tree->car->car, offset+2);
      dump_prefix(tree, offset+1);
      printf("body:\n");
      mrb_parser_dump(mrb, tree->car->cdr, offset+2);
      tree = tree->cdr;
    }
    break;

  case NODE_WHILE:
    printf("NODE_WHILE:\n");
    dump_prefix(tree, offset+1);
    printf("cond:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_UNTIL:
    printf("NODE_UNTIL:\n");
    dump_prefix(tree, offset+1);
    printf("cond:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_FOR:
    printf("NODE_FOR:\n");
    dump_prefix(tree, offset+1);
    printf("var:\n");
    {
      node *n2 = tree->car;

      if (n2->car) {
        dump_prefix(n2, offset+2);
        printf("pre:\n");
        dump_recur(mrb, n2->car, offset+3);
      }
      n2 = n2->cdr;
      if (n2) {
        if (n2->car) {
          dump_prefix(n2, offset+2);
          printf("rest:\n");
          mrb_parser_dump(mrb, n2->car, offset+3);
        }
        n2 = n2->cdr;
        if (n2) {
          if (n2->car) {
            dump_prefix(n2, offset+2);
            printf("post:\n");
            dump_recur(mrb, n2->car, offset+3);
          }
        }
      }
    }
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf("in:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf("do:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    break;

  case NODE_SCOPE:
    printf("NODE_SCOPE:\n");
    {
      node *n2 = tree->car;
      mrb_bool first_lval = TRUE;

      if (n2 && (n2->car || n2->cdr)) {
        dump_prefix(n2, offset+1);
        printf("local variables:\n");
        dump_prefix(n2, offset+2);
        while (n2) {
          if (n2->car) {
            if (!first_lval) printf(", ");
            printf("%s", mrb_sym2name(mrb, sym(n2->car)));
            first_lval = FALSE;
          }
          n2 = n2->cdr;
        }
        printf("\n");
      }
    }
    tree = tree->cdr;
    offset++;
    goto again;

  case NODE_FCALL:
  case NODE_CALL:
    printf("NODE_CALL:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    dump_prefix(tree, offset+1);
    printf("method='%s' (%d)\n",
        mrb_sym2name(mrb, sym(tree->cdr->car)),
        (int)(intptr_t)tree->cdr->car);
    tree = tree->cdr->cdr->car;
    if (tree) {
      dump_prefix(tree, offset+1);
      printf("args:\n");
      dump_recur(mrb, tree->car, offset+2);
      if (tree->cdr) {
        dump_prefix(tree, offset+1);
        printf("block:\n");
        mrb_parser_dump(mrb, tree->cdr, offset+2);
      }
    }
    break;

  case NODE_DOT2:
    printf("NODE_DOT2:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_DOT3:
    printf("NODE_DOT3:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_COLON2:
    printf("NODE_COLON2:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    dump_prefix(tree, offset+1);
    printf("::%s\n", mrb_sym2name(mrb, sym(tree->cdr)));
    break;

  case NODE_COLON3:
    printf("NODE_COLON3:\n");
    dump_prefix(tree, offset+1);
    printf("::%s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_ARRAY:
    printf("NODE_ARRAY:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_HASH:
    printf("NODE_HASH:\n");
    while (tree) {
      dump_prefix(tree, offset+1);
      printf("key:\n");
      mrb_parser_dump(mrb, tree->car->car, offset+2);
      dump_prefix(tree, offset+1);
      printf("value:\n");
      mrb_parser_dump(mrb, tree->car->cdr, offset+2);
      tree = tree->cdr;
    }
    break;

  case NODE_SPLAT:
    printf("NODE_SPLAT:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_ASGN:
    printf("NODE_ASGN:\n");
    dump_prefix(tree, offset+1);
    printf("lhs:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("rhs:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_MASGN:
    printf("NODE_MASGN:\n");
    dump_prefix(tree, offset+1);
    printf("mlhs:\n");
    {
      node *n2 = tree->car;

      if (n2->car) {
        dump_prefix(tree, offset+2);
        printf("pre:\n");
        dump_recur(mrb, n2->car, offset+3);
      }
      n2 = n2->cdr;
      if (n2) {
        if (n2->car) {
          dump_prefix(n2, offset+2);
          printf("rest:\n");
          if (n2->car == (node*)-1) {
            dump_prefix(n2, offset+2);
            printf("(empty)\n");
          }
          else {
            mrb_parser_dump(mrb, n2->car, offset+3);
          }
        }
        n2 = n2->cdr;
        if (n2) {
          if (n2->car) {
            dump_prefix(n2, offset+2);
            printf("post:\n");
            dump_recur(mrb, n2->car, offset+3);
          }
        }
      }
    }
    dump_prefix(tree, offset+1);
    printf("rhs:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_OP_ASGN:
    printf("NODE_OP_ASGN:\n");
    dump_prefix(tree, offset+1);
    printf("lhs:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf("op='%s' (%d)\n", mrb_sym2name(mrb, sym(tree->car)), (int)(intptr_t)tree->car);
    tree = tree->cdr;
    mrb_parser_dump(mrb, tree->car, offset+1);
    break;

  case NODE_SUPER:
    printf("NODE_SUPER:\n");
    if (tree) {
      dump_prefix(tree, offset+1);
      printf("args:\n");
      dump_recur(mrb, tree->car, offset+2);
      if (tree->cdr) {
        dump_prefix(tree, offset+1);
        printf("block:\n");
        mrb_parser_dump(mrb, tree->cdr, offset+2);
      }
    }
    break;

  case NODE_ZSUPER:
    printf("NODE_ZSUPER\n");
    break;

  case NODE_RETURN:
    printf("NODE_RETURN:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_YIELD:
    printf("NODE_YIELD:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_BREAK:
    printf("NODE_BREAK:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_NEXT:
    printf("NODE_NEXT:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_REDO:
    printf("NODE_REDO\n");
    break;

  case NODE_RETRY:
    printf("NODE_RETRY\n");
    break;

  case NODE_LVAR:
    printf("NODE_LVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_GVAR:
    printf("NODE_GVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_IVAR:
    printf("NODE_IVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_CVAR:
    printf("NODE_CVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_CONST:
    printf("NODE_CONST %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_MATCH:
    printf("NODE_MATCH:\n");
    dump_prefix(tree, offset + 1);
    printf("lhs:\n");
    mrb_parser_dump(mrb, tree->car, offset + 2);
    dump_prefix(tree, offset + 1);
    printf("rhs:\n");
    mrb_parser_dump(mrb, tree->cdr, offset + 2);
    break;

  case NODE_BACK_REF:
    printf("NODE_BACK_REF: $%c\n", (int)(intptr_t)tree);
    break;

  case NODE_NTH_REF:
    printf("NODE_NTH_REF: $%d\n", (int)(intptr_t)tree);
    break;

  case NODE_ARG:
    printf("NODE_ARG %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_BLOCK_ARG:
    printf("NODE_BLOCK_ARG:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_INT:
    printf("NODE_INT %s base %d\n", (char*)tree->car, (int)(intptr_t)tree->cdr->car);
    break;

  case NODE_FLOAT:
    printf("NODE_FLOAT %s\n", (char*)tree);
    break;

  case NODE_NEGATE:
    printf("NODE_NEGATE\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_STR:
    printf("NODE_STR \"%s\" len %d\n", (char*)tree->car, (int)(intptr_t)tree->cdr);
    break;

  case NODE_DSTR:
    printf("NODE_DSTR\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_XSTR:
    printf("NODE_XSTR \"%s\" len %d\n", (char*)tree->car, (int)(intptr_t)tree->cdr);
    break;

  case NODE_DXSTR:
    printf("NODE_DXSTR\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_REGX:
    printf("NODE_REGX /%s/%s\n", (char*)tree->car, (char*)tree->cdr);
    break;

  case NODE_DREGX:
    printf("NODE_DREGX\n");
    dump_recur(mrb, tree->car, offset+1);
    dump_prefix(tree, offset);
    printf("tail: %s\n", (char*)tree->cdr->cdr->car);
    dump_prefix(tree, offset);
    printf("opt: %s\n", (char*)tree->cdr->cdr->cdr);
    break;

  case NODE_SYM:
    printf("NODE_SYM :%s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_SELF:
    printf("NODE_SELF\n");
    break;

  case NODE_NIL:
    printf("NODE_NIL\n");
    break;

  case NODE_TRUE:
    printf("NODE_TRUE\n");
    break;

  case NODE_FALSE:
    printf("NODE_FALSE\n");
    break;

  case NODE_ALIAS:
    printf("NODE_ALIAS %s %s:\n",
        mrb_sym2name(mrb, sym(tree->car)),
        mrb_sym2name(mrb, sym(tree->cdr)));
    break;

  case NODE_UNDEF:
    printf("NODE_UNDEF");
    {
      node *t = tree;
      while (t) {
        printf(" %s", mrb_sym2name(mrb, sym(t->car)));
        t = t->cdr;
      }
    }
    printf(":\n");
    break;

  case NODE_CLASS:
    printf("NODE_CLASS:\n");
    if (tree->car->car == (node*)0) {
      dump_prefix(tree, offset+1);
      printf(":%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else if (tree->car->car == (node*)1) {
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else {
      mrb_parser_dump(mrb, tree->car->car, offset+1);
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    if (tree->cdr->car) {
      dump_prefix(tree, offset+1);
      printf("super:\n");
      mrb_parser_dump(mrb, tree->cdr->car, offset+2);
    }
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->cdr->car->cdr, offset+2);
    break;

  case NODE_MODULE:
    printf("NODE_MODULE:\n");
    if (tree->car->car == (node*)0) {
      dump_prefix(tree, offset+1);
      printf(":%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else if (tree->car->car == (node*)1) {
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else {
      mrb_parser_dump(mrb, tree->car->car, offset+1);
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->car->cdr, offset+2);
    break;

  case NODE_SCLASS:
    printf("NODE_SCLASS:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->car->cdr, offset+2);
    break;

  case NODE_DEF:
    printf("NODE_DEF:\n");
    dump_prefix(tree, offset+1);
    printf("%s\n", mrb_sym2name(mrb, sym(tree->car)));
    tree = tree->cdr;
    {
      node *n2 = tree->car;
      mrb_bool first_lval = TRUE;

      if (n2 && (n2->car || n2->cdr)) {
        dump_prefix(n2, offset+1);
        printf("local variables:\n");
        dump_prefix(n2, offset+2);
        while (n2) {
          if (n2->car) {
            if (!first_lval) printf(", ");
            printf("%s", mrb_sym2name(mrb, sym(n2->car)));
            first_lval = FALSE;
          }
          n2 = n2->cdr;
        }
        printf("\n");
      }
    }
    tree = tree->cdr;
    if (tree->car) {
      node *n = tree->car;

      if (n->car) {
        dump_prefix(n, offset+1);
        printf("mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("optional args:\n");
        {
          node *n2 = n->car;

          while (n2) {
            dump_prefix(n2, offset+2);
            printf("%s=", mrb_sym2name(mrb, sym(n2->car->car)));
            mrb_parser_dump(mrb, n2->car->cdr, 0);
            n2 = n2->cdr;
          }
        }
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("rest=*%s\n", mrb_sym2name(mrb, sym(n->car)));
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("post mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      if (n->cdr) {
        dump_prefix(n, offset+1);
        printf("blk=&%s\n", mrb_sym2name(mrb, sym(n->cdr)));
      }
    }
    mrb_parser_dump(mrb, tree->cdr->car, offset+1);
    break;

  case NODE_SDEF:
    printf("NODE_SDEF:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf(":%s\n", mrb_sym2name(mrb, sym(tree->car)));
    tree = tree->cdr->cdr;
    if (tree->car) {
      node *n = tree->car;

      if (n->car) {
        dump_prefix(n, offset+1);
        printf("mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("optional args:\n");
        {
          node *n2 = n->car;

          while (n2) {
            dump_prefix(n2, offset+2);
            printf("%s=", mrb_sym2name(mrb, sym(n2->car->car)));
            mrb_parser_dump(mrb, n2->car->cdr, 0);
            n2 = n2->cdr;
          }
        }
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("rest=*%s\n", mrb_sym2name(mrb, sym(n->car)));
      }
      n = n->cdr;
      if (n->car) {
        dump_prefix(n, offset+1);
        printf("post mandatory args:\n");
        dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n) {
        dump_prefix(n, offset+1);
        printf("blk=&%s\n", mrb_sym2name(mrb, sym(n)));
      }
    }
    tree = tree->cdr;
    mrb_parser_dump(mrb, tree->car, offset+1);
    break;

  case NODE_POSTEXE:
    printf("NODE_POSTEXE:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_HEREDOC:
    printf("NODE_HEREDOC:\n");
    mrb_parser_dump(mrb, ((parser_heredoc_info*)tree)->doc, offset+1);
    break;

  default:
    printf("node type: %d (0x%x)\n", nodetype, (unsigned)nodetype);
    break;
  }
#endif
}
