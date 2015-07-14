#ifndef _TINYRB_H_
#define _TINYRB_H_

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <assert.h>
#include <errno.h>

#include <gc.h>

#include <pcre.h>

#include "config.h"
#include "vendor/kvec.h"
#include "vendor/khash.h"

#define UNUSED(expr)         do { (void)(expr); } while (0)

/* allocation macros */
#define TR_MALLOC            GC_malloc
#define TR_CALLOC(m,n)       TR_MALLOC((m)*(n))
#define TR_REALLOC           GC_realloc
#define TR_FREE(S)           UNUSED(S)

/* type convertion macros */
#define TR_TYPE(X)           TrObject_type(vm, (X))
#define TR_CLASS(X)          (TR_IMMEDIATE(X) ? vm->Classes[TR_TYPE(X)] : TR_COBJECT(X)->Class)
#define TR_IS_A(X,T)         (TR_TYPE(X) == TR_T_##T)
#define TR_COBJECT(X)        ((TrObject*)(X))
#define TR_TYPE_ERROR(T)     TR_THROW(EXCEPTION, TrException_new(vm, vm->cTypeError, TrString_new2(vm, "Expected " #T)))
#define TR_CTYPE(X,T)        ((TR_IS_A(X,T) ? 0 : TR_TYPE_ERROR(T)),(Tr##T*)(X))
#define TR_CCLASS(X)         ((TR_IS_A(X,Class) || TR_IS_A(X,Module) ? 0 : TR_TYPE_ERROR(T)),(TrClass*)(X))
#define TR_CMODULE(X)        TR_CCLASS(X)
#define TR_CARRAY(X)         TR_CTYPE(X,Array)
#define TR_CHASH(X)          TR_CTYPE(X,Hash)
#define TR_CRANGE(X)         TR_CTYPE(X,Range)
#define TR_CREGEXP(X)        TR_CTYPE(X,Regexp)
#define TR_CSTRING(X)        ((TR_IS_A(X,String) || TR_IS_A(X,Symbol) ? 0 : TR_TYPE_ERROR(T)),(TrString*)(X))
#define TR_CMETHOD(X)        ((TrMethod*)X)
#define TR_CBINDING(X)       TR_CTYPE(X,Binding)

/* string macros */
#define TR_STR_PTR(S)        (TR_CSTRING(S)->ptr)
#define TR_STR_LEN(S)        (TR_CSTRING(S)->len)

/* array macros */
#define TR_ARRAY_PUSH(X,I)   kv_push(OBJ, ((TrArray*)(X))->kv, (I))
#define TR_ARRAY_AT(X,I)     kv_A((TR_CARRAY(X))->kv, (I))
#define TR_ARRAY_SIZE(X)     kv_size(TR_CARRAY(X)->kv)
#define TR_ARRAY_EACH(T,I,V,B) do { \
    TrArray *__a##V = TR_CARRAY(T); \
    if (kv_size(__a##V->kv) != 0) { \
      size_t I; \
      for (I = 0; I < kv_size(__a##V->kv); I++) { \
        OBJ V = kv_A(__a##V->kv, I); \
        B \
      } \
    } \
  } while(0)

/* raw hash macros */
#define TR_KH_GET(KH,K) ( [&] { \
  OBJ key = (K); \
  khash_t(OBJ) *kh = (KH); \
  khiter_t k = kh_get(OBJ, kh, key); \
  return k == kh_end(kh) ? TR_NIL : kh_value(kh, k); \
}())
#define TR_KH_SET(KH,K,V) ( [&] { \
  OBJ key = (K); \
  khash_t(OBJ) *kh = (KH); \
  int ret; \
  khiter_t k = kh_put(OBJ, kh, key, &ret); \
  return kh_value(kh, k) = (V); \
}())
#define TR_KH_EACH(H,I,V,B) do { \
    khiter_t __k##V; \
    for (__k##V = kh_begin(H); __k##V != kh_end(H); ++__k##V) \
      if (kh_exist((H), __k##V)) { \
        OBJ V = kh_value((H), __k##V); \
        B \
      } \
  } while(0)

/* vm macros */
#define VM                   struct TrVM *vm

/* throw macros */
#define TR_THROW(R,V)        /*assert(0)*/ ([&] { \
                               vm->throw_reason = TR_THROW_##R; \
                               vm->throw_value = (V); \
                               return TR_UNDEF; \
                             }() )
#define TR_HAS_EXCEPTION(R)  ((R) == TR_UNDEF && vm->throw_reason == TR_THROW_EXCEPTION)
#define TR_FAILSAFE(R)       if (TR_HAS_EXCEPTION(R)) { \
                               TrException_default_handler(vm, TR_EXCEPTION); \
                               abort(); \
                             }
#define TR_EXCEPTION         (assert(vm->throw_reason == TR_THROW_EXCEPTION), vm->throw_value)

/* immediate values macros */
#define TR_IMMEDIATE(X)      (X==TR_NIL || X==TR_TRUE || X==TR_FALSE || X==TR_UNDEF || TR_IS_FIX(X))
#define TR_IS_FIX(F)         ((F) & 1)
#define TR_FIX2INT(F)        (((int)(F) >> 1))
#define TR_INT2FIX(I)        ((I) << 1 |  1)
#define TR_NIL               ((OBJ)0)
#define TR_FALSE             ((OBJ)2)
#define TR_TRUE              ((OBJ)4)
#define TR_UNDEF             ((OBJ)6)
#define TR_TEST(X)           ((X) == TR_NIL || (X) == TR_FALSE ? 0 : 1)
#define TR_BOOL(X)           ((X) ? TR_TRUE : TR_FALSE)

/* common header share by all object */
#define TR_OBJECT_HEADER \
  TR_T type; \
  OBJ Class; \
  khash_t(OBJ) *ivars

/* core Classes macros */
#define TR_INIT_CORE_OBJECT(T) ([&] { \
  Tr##T *o = TR_ALLOC(Tr##T); \
  o->type  = TR_T_##T; \
  o->Class = vm->Classes[TR_T_##T]; \
  o->ivars = kh_init(OBJ); \
  return o; \
}())
#define TR_CORE_CLASS(T)     vm->Classes[TR_T_##T]
#define TR_INIT_CORE_CLASS(T,S) ( [&] { \
    return TR_CORE_CLASS(T) = TrObject_const_set(vm, vm->self, tr_intern(#T), \
    TrClass_new(vm, tr_intern(#T), TR_CORE_CLASS(S))); \
}())

/* API macros */
#define tr_getivar(O,N)      TR_KH_GET(TR_COBJECT(O)->ivars, tr_intern(N))
#define tr_setivar(O,N,V)    TR_KH_SET(TR_COBJECT(O)->ivars, tr_intern(N), V)
#define tr_getglobal(N)      TR_KH_GET(vm->globals, tr_intern(N))
#define tr_setglobal(N,V)    TR_KH_SET(vm->globals, tr_intern(N), V)
#define tr_intern(S)         TrSymbol_new(vm, (S))
#define tr_raise(T,M,...)    TR_THROW(EXCEPTION, TrException_new(vm, vm->c##T, tr_sprintf(vm, (M), ##__VA_ARGS__)))
#define tr_raise_errno(M)    tr_raise(SystemCallError, "%s: %s", strerror(errno), (M))
#define tr_def(C,N,F,A)      TrModule_add_method(vm, (C), tr_intern(N), TrMethod_new(vm, (TrFunc *)(F), TR_NIL, (A)))
#define tr_metadef(O,N,F,A)  TrObject_add_singleton_method(vm, (O), tr_intern(N), TrMethod_new(vm, (TrFunc *)(F), TR_NIL, (A)))
#define tr_defClass(N,S)     TrObject_const_set(vm, vm->self, tr_intern(N), TrClass_new(vm, tr_intern(N), S))
#define tr_defmodule(N)      TrObject_const_set(vm, vm->self, tr_intern(N), TrModule_new(vm, tr_intern(N)))

#define tr_send(R,MSG,...)   ( [&] { \
  OBJ argv__[] = { (MSG), ##__VA_ARGS__ }; \
  return TrObject_send(vm, R, sizeof(argv__)/sizeof(OBJ), argv__); \
} () )
#define tr_send2(R,STR,...)  tr_send((R), tr_intern(STR), ##__VA_ARGS__)

typedef unsigned long OBJ;
typedef unsigned char u8;
typedef unsigned int TrInst;

KHASH_MAP_INIT_STR(str, OBJ)
KHASH_MAP_INIT_INT(OBJ, OBJ)

typedef enum {
  /*  0 */ TR_T_Object, TR_T_Module, TR_T_Class, TR_T_Method, TR_T_Binding,
  /*  5 */ TR_T_Symbol, TR_T_String, TR_T_Fixnum, TR_T_Range, TR_T_Regexp,
  /* 10 */ TR_T_NilClass, TR_T_TrueClass, TR_T_FalseClass,
  /* 12 */ TR_T_Array, TR_T_Hash,
  /* 14 */ TR_T_Node,
  TR_T_MAX /* keep last */
} TR_T;

typedef enum {
  TR_THROW_EXCEPTION,
  TR_THROW_RETURN,
  TR_THROW_BREAK
} TR_THROW_REASON;

struct TrVM;
struct TrFrame;

typedef struct {
  OBJ Class;
  OBJ method;
  OBJ message;
  int method_missing:1;
  size_t miss;
} TrCallSite;

typedef struct TrBlock {
  /* static */
  kvec_t(OBJ) k;
  kvec_t(char *) strings;
  kvec_t(OBJ) locals;
  kvec_t(OBJ) upvals;
  kvec_t(TrInst) code;
  kvec_t(int) defaults;
  kvec_t(struct TrBlock *) blocks; /* TODO should not be pointers */
  size_t regc;
  size_t argc;
  size_t arg_splat;
  OBJ filename;
  size_t line;
  struct TrBlock *parent;
  /* dynamic */
  kvec_t(TrCallSite) sites;
} TrBlock;

typedef struct TrUpval {
  OBJ *value;
  OBJ closed; /* value when closed */
} TrUpval;

typedef struct TrClosure {
  TrBlock *block;
  TrUpval *upvals;
  OBJ self;
  OBJ Class;
  struct TrClosure *parent;
} TrClosure;

typedef OBJ (TrFunc)(VM, OBJ receiver, ...);
typedef struct {
  TR_OBJECT_HEADER;
  TrFunc *func;
  OBJ data;
  OBJ name;
  int arity;
} TrMethod;

typedef struct TrFrame {
  TrClosure *closure;
  TrMethod *method;  /* current called method */
  OBJ *stack;
  OBJ *upvals;
  OBJ self;
  OBJ Class;
  OBJ filename;
  size_t line;
  struct TrFrame *previous;
} TrFrame;

typedef struct {
  TR_OBJECT_HEADER;
  TrFrame *frame;
} TrBinding;

typedef struct TrVM {
  khash_t(str) *symbols;
  khash_t(OBJ) *globals;
  khash_t(OBJ) *consts;           /* TODO this goes in modules */
  OBJ Classes[TR_T_MAX];          /* core Classes */
  TrFrame *top_frame;             /* top level frame */
  TrFrame *frame;                 /* current frame */
  int cf;                         /* current frame number */
  OBJ self;                       /* root object */
  int debug;
  int throw_reason;
  OBJ throw_value;
  
  /* exceptions */
  OBJ cException;
  OBJ cScriptError;
  OBJ cSyntaxError;
  OBJ cStandardError;
  OBJ cArgumentError;
  OBJ cRuntimeError;
  OBJ cRegexpError;
  OBJ cTypeError;
  OBJ cSystemCallError;
  OBJ cIndexError;
  OBJ cLocalJumpError;
  OBJ cSystemStackError;
  OBJ cNameError;
  OBJ cNoMethodError;
  
  /* cached objects */
  OBJ sADD;
  OBJ sSUB;
  OBJ sLT;
  OBJ sNEG;
  OBJ sNOT;
} TrVM;

typedef struct {
  TR_OBJECT_HEADER;
} TrObject;

typedef struct {
  TR_OBJECT_HEADER;
  OBJ name;
  OBJ super;
  khash_t(OBJ) *methods;
  int meta:1;
} TrClass;
typedef TrClass TrModule;

typedef struct {
  TR_OBJECT_HEADER;
  char *ptr;
  size_t len;
  int interned:1;
} TrString;
typedef TrString TrSymbol;

typedef struct {
  TR_OBJECT_HEADER;
  OBJ first;
  OBJ last;
  int exclusive;
} TrRange;

typedef struct {
  TR_OBJECT_HEADER;
  kvec_t(OBJ) kv;
} TrArray;

typedef struct {
  TR_OBJECT_HEADER;
  khash_t(OBJ) *kh;
} TrHash;

typedef struct TrRegexp {
  TR_OBJECT_HEADER;
  pcre *re;
} TrRegexp;

/* vm */
TrVM *TrVM_new();
OBJ TrVM_backtrace(VM);
OBJ TrVM_eval(VM, char *code, char *filename);
OBJ TrVM_load(VM, char *filename);
OBJ TrVM_run(VM, TrBlock *b, OBJ self, OBJ Class, int argc, OBJ argv[]);
void TrVM_destroy(TrVM *vm);

/* string */
OBJ TrSymbol_new(VM, const char *str);
OBJ TrString_new(VM, const char *str, size_t len);
OBJ TrString_new2(VM, const char *str);
OBJ TrString_new3(VM, size_t len);
OBJ TrString_push(VM, OBJ self, OBJ other);
OBJ tr_sprintf(VM, const char *fmt, ...);
void TrSymbol_init(VM);
void TrString_init(VM);

/* number */
void TrFixnum_init(VM);

/* array */
OBJ TrArray_new(VM);
OBJ TrArray_new2(VM, int argc, ...);
OBJ TrArray_new3(VM, int argc, OBJ items[]);
void TrArray_init(VM);

/* hash */
OBJ TrHash_new(VM);
OBJ TrHash_new2(VM, size_t n, OBJ items[]);
void TrHash_init(VM);

/* range */
OBJ TrRange_new(VM, OBJ start, OBJ end, int exclusive);
void TrRange_init(VM);

/* proc */
TrClosure *TrClosure_new(VM, TrBlock *b, OBJ self, OBJ Class, TrClosure *parent);

/* object */
OBJ TrObject_alloc(VM, OBJ Class);
int TrObject_type(VM, OBJ obj);
OBJ TrObject_method(VM, OBJ self, OBJ name);
OBJ TrObject_send(VM, OBJ self, int argc, OBJ argv[]);
OBJ TrObject_const_set(VM, OBJ self, OBJ name, OBJ value);
OBJ TrObject_const_get(VM, OBJ self, OBJ name);
OBJ TrObject_add_singleton_method(VM, OBJ self, OBJ name, OBJ method);
void TrObject_preinit(VM);
void TrObject_init(VM);

/* module */
OBJ TrModule_new(VM, OBJ name);
OBJ TrModule_instance_method(VM, OBJ self, OBJ name);
OBJ TrModule_add_method(VM, OBJ self, OBJ name, OBJ method);
OBJ TrModule_include(VM, OBJ self, OBJ mod);
void TrModule_init(VM);

/* kernel */
void TrKernel_init(VM);

/* Class */
OBJ TrClass_new(VM, OBJ name, OBJ super);
OBJ TrMetaClass_new(VM, OBJ super);
OBJ TrClass_allocate(VM, OBJ self);
void TrClass_init(VM);

/* method */
OBJ TrMethod_new(VM, TrFunc *func, OBJ data, int arity);
void TrMethod_init(VM);
void TrBinding_init(VM);

/* primitive */
void TrPrimitive_init(VM);

/* error */
OBJ TrException_new(VM, OBJ Class, OBJ message);
OBJ TrException_backtrace(VM, OBJ self);
OBJ TrException_set_backtrace(VM, OBJ self, OBJ backtrace);
OBJ TrException_default_handler(VM, OBJ exception);
void TrError_init(VM);

/* regexp */
OBJ TrRegexp_new(VM, char *pattern, int options);
void TrRegex_free(VM, OBJ self);
void TrRegexp_init(VM);

/* compiler */
TrBlock *TrBlock_compile(VM, char *code, char *fn, size_t lineno);
void TrBlock_dump(VM, TrBlock *b);

#endif /* _TINYRB_H_ */
