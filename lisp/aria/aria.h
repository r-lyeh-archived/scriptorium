/** 
 * Copyright (c) 2016 rxi
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the MIT license. See LICENSE for details.
 */

#ifndef ARIA_H
#define ARIA_H

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <setjmp.h>
#include <ctype.h>

#define AR_VERSION "0.1.0"

typedef struct ar_Value ar_Value;
typedef struct ar_State ar_State;
typedef struct ar_Chunk ar_Chunk;
typedef struct ar_Frame ar_Frame;

typedef void *(*ar_Alloc)(void *udata, void *ptr, size_t size);
typedef ar_Value* (*ar_CFunc)(ar_State *S, ar_Value* args);
typedef ar_Value* (*ar_Prim)(ar_State *S, ar_Value* args, ar_Value *env);


struct ar_Value {
  unsigned char type, mark;
  union {
    struct { ar_Value *name; int line;            } dbg;
    struct { ar_Value *pair, *left, *right;       } map;
    struct { ar_Value *car, *cdr, *dbg;           } pair;
    struct { double n;                            } num;
    struct { ar_Value *params, *body, *env;       } func;
    struct { void *ptr; ar_CFunc gc, mark;        } udata;
    struct { ar_Value *parent, *map;              } env;
    struct { ar_CFunc fn;                         } cfunc;
    struct { ar_Prim fn;                          } prim;
    struct { char *s; size_t len; unsigned hash;  } str;
  } u;
};


struct ar_Frame {
  struct ar_Frame *parent;  /* Parent stack frame */
  ar_Value *caller;         /* Calling function pair */
  jmp_buf *err_env;         /* Jumped to on error, if it exists */
  int stack_idx;            /* Index on stack where frame's values start */
};


struct ar_State {
  ar_Alloc alloc;           /* Allocator function */
  void *udata;              /* Pointer passed as allocator's udata */
  ar_Value *global;         /* Global environment */
  ar_Frame base_frame;      /* Base stack frame */
  ar_Frame *frame;          /* Current stack frame */
  int frame_idx;            /* Current stack frame index */
  ar_Value *t;              /* Symbol `t` */
  ar_CFunc panic;           /* Called if an unprotected error occurs */
  ar_Value *err_args;       /* Error args passed to error handler */
  ar_Value *oom_error;      /* Value thrown on an out of memory error */
  ar_Value *oom_args;       /* Args passed to err handler on out of mem */
  ar_Value *parse_name;     /* Parser's current chunk name */
  int parse_line;           /* Parser's current line */
  ar_Value **gc_stack;      /* Stack of values (protected from GC) */
  int gc_stack_idx;         /* Current index for the top of the gc_stack */
  int gc_stack_cap;         /* Max capacity of protected values stack */
  ar_Chunk *gc_chunks;      /* List of all chunks */
  ar_Value *gc_pool;        /* Dead (usable) Values */
  int gc_count;             /* Counts down number of new values until GC */
};


enum {
  AR_TNIL,
  AR_TDBGINFO,
  AR_TMAPNODE,
  AR_TPAIR,
  AR_TNUMBER,
  AR_TSTRING,
  AR_TSYMBOL,
  AR_TFUNC,
  AR_TMACRO,
  AR_TPRIM,
  AR_TCFUNC,
  AR_TENV,
  AR_TUDATA
};

#define ar_get_global(S,x)    ar_eval(S, ar_new_symbol(S, x), (S)->global)
#define ar_bind_global(S,x,v) ar_bind(S, ar_new_symbol(S, x), v, (S)->global)
#define ar_call_global(S,f,a) ar_call(S, ar_get_global(S, f), a)
                              
#define ar_check_string(S,v)  ar_to_string(S, ar_check(S, v, AR_TSTRING))
#define ar_check_udata(S,v)   ar_to_udata(S, ar_check(S, v, AR_TUDATA))
#define ar_check_number(S,v)  ar_to_number(S, ar_check(S, v, AR_TNUMBER))

#define ar_try(S, err_val, blk, err_blk)\
  do {\
    jmp_buf err_env__, *old_env__ = (S)->frame->err_env;\
    S->frame->err_env = &err_env__;\
    if (setjmp(err_env__)) {\
      ar_Value *err_val = (S)->err_args;\
      (S)->frame->err_env = old_env__;\
      err_blk;\
    } else {\
      blk;\
      (S)->frame->err_env = old_env__;\
    }\
  } while (0)

ar_State *ar_new_state(ar_Alloc alloc, void *udata);
void ar_close_state(ar_State *S);
ar_CFunc ar_at_panic(ar_State *S, ar_CFunc fn);
void ar_error(ar_State *S, ar_Value *err);
void ar_error_str(ar_State *S, const char *fmt, ...);

ar_Value *ar_new_env(ar_State *S, ar_Value *parent);
ar_Value *ar_new_pair(ar_State *S, ar_Value *car, ar_Value *cdr);
ar_Value *ar_new_list(ar_State *S, size_t n, ...);
ar_Value *ar_new_number(ar_State *S, double n);
ar_Value *ar_new_udata(ar_State *S, void *ptr, ar_CFunc gc, ar_CFunc mark);
ar_Value *ar_new_stringl(ar_State *S, const char *str, size_t len);
ar_Value *ar_new_string(ar_State *S, const char *str);
ar_Value *ar_new_symbol(ar_State *S, const char *name);
ar_Value *ar_new_cfunc(ar_State *S, ar_CFunc fn);
ar_Value *ar_new_prim(ar_State *S, ar_Prim fn);

int ar_type(ar_Value *v);
const char *ar_type_str(int type);
ar_Value *ar_check(ar_State *S, ar_Value *v, int type);
ar_Value *ar_car(ar_Value *v);
ar_Value *ar_cdr(ar_Value *v);
ar_Value *ar_nth(ar_Value *v, int idx);
ar_Value **ar_append_tail(ar_State *S, ar_Value **last, ar_Value *v);
ar_Value *ar_to_string_value(ar_State *S, ar_Value *v, int quotestr);

const char *ar_to_stringl(ar_State *S, ar_Value *v, size_t *len);
const char *ar_to_string(ar_State *S, ar_Value *v);
void *ar_to_udata(ar_State *S, ar_Value *v);
double ar_to_number(ar_State *S, ar_Value *v);
const char *ar_opt_string(ar_State *S, ar_Value *v, const char *def);
void *ar_opt_udata(ar_State *S, ar_Value *v, void *def);
double ar_opt_number(ar_State *S, ar_Value *v, double def);

ar_Value *ar_bind(ar_State *S, ar_Value *sym, ar_Value *v, ar_Value *env);
ar_Value *ar_set(ar_State *S, ar_Value *sym, ar_Value *v, ar_Value *env);

void ar_mark(ar_State *S, ar_Value *v);
void ar_gc(ar_State *S);

ar_Value *ar_parse(ar_State *S, const char *str, const char *name);
ar_Value *ar_eval(ar_State *S, ar_Value *v, ar_Value *env);
ar_Value *ar_call(ar_State *S, ar_Value *fn, ar_Value *args);
ar_Value *ar_do_list(ar_State *S, ar_Value *body, ar_Value *env);
ar_Value *ar_do_string(ar_State *S, const char *str);
ar_Value *ar_do_file(ar_State *S, const char *filename);

#endif
