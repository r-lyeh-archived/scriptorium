#include <stdio.h>
#include <sys/stat.h>
#include <assert.h>

#include "tr.h"
#include "opcode.h"
#include "internal.h"
#include "call.h"

#define RETHROW(R) if (unlikely((R) == TR_UNDEF)) return TR_UNDEF

static OBJ TrVM_interpret(VM, TrFrame *f, TrBlock *b, int start, int argc, OBJ argv[], TrClosure *closure);

static OBJ TrVM_lookup(VM, TrBlock *b, OBJ receiver, OBJ msg, TrInst *ip) {
  OBJ method = TrObject_method(vm, receiver, msg);
  RETHROW(method);

  TrInst *boing = (ip-1);
  /* TODO do not prealloc TrCallSite here, every one is a memory leak and a new
          one is created on polymorphic calls. */
  TrCallSite *s = (kv_pushp(TrCallSite, b->sites));
  s->Class = TR_CLASS(receiver);
  s->miss = 0;
  s->method = method;
  s->message = msg;
  if (unlikely(method == TR_NIL)) {
    s->method = TrObject_method(vm, receiver, tr_intern("method_missing"));
    s->method_missing = 1;
  }
  
  /* Implement Monomorphic method cache by replacing the previous instruction (BOING)
     w/ CACHE that uses the CallSite to find the method instead of doing a full lookup. */
  if (GET_OPCODE(*boing) == TR_OP_CACHE) {
    /* Existing call site */
    /* TODO maybe take existing call site hit miss into consideration to replace it with this one.
       For now, we just don't replace it, the first one is always the cached one. */
  } else {
    /* New call site, we cache it fo shizzly! */
    SET_OPCODE(*boing, TR_OP_CACHE);
    SETARG_A(*boing, GETARG_A(*ip)); /* receiver register */
    SETARG_B(*boing, 1); /* jmp */
    SETARG_C(*boing, kv_size(b->sites)-1); /* CallSite index */
  }
  
  return (OBJ)s;
}

static OBJ TrVM_defClass(VM, OBJ name, TrBlock *b, int module, OBJ super) {
  OBJ mod = TrObject_const_get(vm, vm->frame->Class, name);
  RETHROW(mod);
  
  if (!mod) { /* new module/Class */
    if (module)
      mod = TrModule_new(vm, name);
    else
      mod = TrClass_new(vm, name, super ? super : TR_CORE_CLASS(Object));
    RETHROW(mod);
    TrObject_const_set(vm, vm->frame->Class, name, mod);
  }
  OBJ ret = TR_NIL;
  TR_WITH_FRAME(mod, mod, 0, {
    ret = TrVM_interpret(vm, vm->frame, b, 0, 0, 0, 0);
  });
  RETHROW(ret);
  return mod;
}

static OBJ TrVM_interpret_method(VM, OBJ self, int argc, OBJ argv[]) {
  UNUSED(self);
  assert(vm->frame->method);
  register TrBlock *b = (TrBlock *)TR_CMETHOD(vm->frame->method)->data;
  if (unlikely(argc != (int)b->argc)) tr_raise(ArgumentError, "wrong number of arguments (%d for %lu)", argc, b->argc);
  return TrVM_interpret(vm, vm->frame, b, 0, argc, argv, 0);
}

static OBJ TrVM_interpret_method_with_defaults(VM, OBJ self, int argc, OBJ argv[]) {
  UNUSED(self);
  assert(vm->frame->method);
  register TrBlock *b = (TrBlock *)TR_CMETHOD(vm->frame->method)->data;
  int req_argc = b->argc - kv_size(b->defaults);
  if (argc < req_argc) tr_raise(ArgumentError, "wrong number of arguments (%d for %d)", argc, req_argc);
  if (argc > (int)b->argc) tr_raise(ArgumentError, "wrong number of arguments (%d for %lu)", argc, b->argc);
  int defi = argc - req_argc - 1; /* index in defaults table or -1 for none */
  return TrVM_interpret(vm, vm->frame, b, defi < 0 ? 0 : kv_A(b->defaults, defi), argc, argv, 0);
}

static OBJ TrVM_interpret_method_with_splat(VM, OBJ self, int argc, OBJ argv[]) {
  UNUSED(self);
  assert(vm->frame->method);
  register TrBlock *b = (TrBlock *)TR_CMETHOD(vm->frame->method)->data;
  /* TODO support defaults */
  assert(kv_size(b->defaults) == 0 && "defaults with splat not supported for now");
  if (argc < (int)b->argc-1) tr_raise(ArgumentError, "wrong number of arguments (%d for %lu)", argc, b->argc-1);
  argv[b->argc-1] = TrArray_new3(vm, argc - b->argc + 1, &argv[b->argc-1]);
  return TrVM_interpret(vm, vm->frame, b, 0, b->argc, argv, 0);
}

static OBJ TrVM_defmethod(VM, TrFrame *f, OBJ name, TrBlock *b, int meta, OBJ receiver) {
  TrFunc *func;
  if (b->arg_splat)
    func = (TrFunc *) TrVM_interpret_method_with_splat;
  else if (kv_size(b->defaults) > 0)
    func = (TrFunc *) TrVM_interpret_method_with_defaults;
  else
    func = (TrFunc *) TrVM_interpret_method;
  OBJ method = TrMethod_new(vm, func, (OBJ)b, -1);
  RETHROW(method);
  if (meta)
    TrObject_add_singleton_method(vm, receiver, name, method);
  else
    TrModule_add_method(vm, f->Class, name, method);
  return TR_NIL;
}

static inline OBJ TrVM_yield(VM, TrFrame *f, int argc, OBJ argv[]) {
  TrClosure *cl = f->closure;
  if (!cl) tr_raise(LocalJumpError, "no block given");
  OBJ ret = TR_NIL;
  TR_WITH_FRAME(cl->self, cl->Class, cl->parent, {
    ret = TrVM_interpret(vm, vm->frame, cl->block, 0, argc, argv, cl);
  });
  return ret;
}

/* dispatch macros */
#define NEXT_INST      (i = *++ip)
#if TR_THREADED_DISPATCH
#define OPCODES        static void *labels[] = { TR_OP_LABELS }; goto *labels[OPCODE];
#define END_OPCODES    
#define OP(name)       op_##name
#define DISPATCH       NEXT_INST; goto *labels[OPCODE]
#else
#define OPCODES        for(;;) { switch(OPCODE) {
#define END_OPCODES    default: printf("unknown opcode: %d\n", (int)OPCODE); }}
#define OP(name)       case TR_OP_##name
#define DISPATCH       NEXT_INST; break
#endif

/* register access macros */
#define OPCODE GET_OPCODE(i)
#define A      GETARG_A(i)
#define B      GETARG_B(i)
#define C      GETARG_C(i)
#define nA     GETARG_A(*(ip+1))
#define nB     GETARG_B(*(ip+1))
#define R      stack
#define RK(X)  (X & (1 << (SIZE_B - 1)) ? k[X & ~0x100] : R[X])
#define Bx     GETARG_Bx(i)
#define sBx    GETARG_sBx(i)
#define SITE   (b->sites.a)

#define RETURN(V) \
  /* TODO GC release everything on the stack before returning */ \
  return (V)
#define VM_RETHROW(R) if (unlikely((OBJ)(R) == TR_UNDEF)) RETURN(TR_UNDEF)

/* Interprets the code in b->code.
   Returns TR_UNDEF on error. */
static OBJ TrVM_interpret(VM, register TrFrame *f, TrBlock *b, int start, int argc, OBJ argv[], TrClosure *closure) {
  f->stack = (OBJ *)alloca(sizeof(OBJ) * b->regc);
#if TR_USE_MACHINE_REGS && __i386__
  register TrInst *ip __asm__ ("esi") = b->code.a + start;
  register OBJ *stack __asm__ ("edi") = f->stack;
#elif TR_USE_MACHINE_REGS && __x86_64__
  register TrInst *ip __asm__ ("r15") = b->code.a + start;
  register OBJ *stack __asm__ ("r14") = f->stack;
#else
  register TrInst *ip = b->code.a + start;
  register OBJ *stack = f->stack;
#endif
  TrInst i = *ip;
  OBJ *k = b->k.a;
  char **strings = b->strings.a;
  TrBlock **blocks = b->blocks.a;
  f->line = b->line;
  f->filename = b->filename;
  TrUpval *upvals = closure ? closure->upvals : 0;
  TrCallSite *call = 0;

  /* transfer locals */
  if (argc > 0) { 
    assert(argc <= (int)kv_size(b->locals) && "can't fit args in locals");
    TR_MEMCPY_N(stack, argv, OBJ, argc);
  }
  
  OPCODES;
    
    OP(BOING):      DISPATCH;
    
    /* register loading */
    OP(MOVE):       R[A] = R[B]; DISPATCH;
    OP(LOADK):      R[A] = k[Bx]; DISPATCH;
    OP(STRING):     R[A] = TrString_new2(vm, strings[Bx]); DISPATCH;
    OP(SELF):       R[A] = f->self; DISPATCH;
    OP(NIL):        R[A] = TR_NIL; DISPATCH;
    OP(BOOL):       R[A] = B; DISPATCH;
    OP(NEWARRAY):   R[A] = TrArray_new3(vm, B, &R[A+1]); DISPATCH;
    OP(NEWHASH):    R[A] = TrHash_new2(vm, B, &R[A+1]); DISPATCH;
    OP(NEWRANGE):   R[A] = TrRange_new(vm, R[A], R[B], C); DISPATCH;
    
    /* return */
    OP(RETURN):     RETURN(R[A]);
    OP(THROW):
      vm->throw_reason = A;
      vm->throw_value = R[B];
      RETURN(TR_UNDEF);
    OP(YIELD):      VM_RETHROW(R[A] = TrVM_yield(vm, f, B, &R[A+1])); DISPATCH;
    
    /* variable and consts */
    OP(SETUPVAL):   assert(upvals && upvals[B].value); *(upvals[B].value) = R[A]; DISPATCH;
    OP(GETUPVAL):   assert(upvals); R[A] = *(upvals[B].value); DISPATCH;
    OP(SETIVAR):    TR_KH_SET(TR_COBJECT(f->self)->ivars, k[Bx], R[A]); DISPATCH;
    OP(GETIVAR):    R[A] = TR_KH_GET(TR_COBJECT(f->self)->ivars, k[Bx]); DISPATCH;
    OP(SETCVAR):    TR_KH_SET(TR_COBJECT(f->Class)->ivars, k[Bx], R[A]); DISPATCH;
    OP(GETCVAR):    R[A] = TR_KH_GET(TR_COBJECT(f->Class)->ivars, k[Bx]); DISPATCH;
    OP(SETCONST):   TrObject_const_set(vm, f->self, k[Bx], R[A]); DISPATCH;
    OP(GETCONST):   R[A] = TrObject_const_get(vm, f->self, k[Bx]); DISPATCH;
    OP(SETGLOBAL):  TR_KH_SET(vm->globals, k[Bx], R[A]); DISPATCH;
    OP(GETGLOBAL):  R[A] = TR_KH_GET(vm->globals, k[Bx]); DISPATCH;
    
    /* method calling */
    OP(LOOKUP):     VM_RETHROW(call = (TrCallSite*)TrVM_lookup(vm, b, R[A], k[Bx], ip)); DISPATCH;
    OP(CACHE):
      /* TODO how to expire cache? */
      assert(&SITE[C] && "Method cached but no CallSite found");
      if (likely(SITE[C].Class == TR_CLASS((R[A])))) {
        call = &SITE[C];
        ip += B;
      } else {
        /* TODO invalidate CallSite if too much miss. */
        SITE[C].miss++;
      }
      DISPATCH;
    OP(CALL): {
      TrClosure *cl = 0;
      TrInst ci = i;
      if (unlikely(C > 0)) {
        /* Get upvalues using the pseudo-instructions following the CALL instruction.
           Eg.: there's one upval to a local (x) to be passed:
             call    0  0  0
             move    0  0  0 ; this is not executed
             return  0
         */
        cl = TrClosure_new(vm, blocks[C-1], f->self, f->Class, f->closure);
        size_t n, nupval = kv_size(cl->block->upvals);
        for (n = 0; n < nupval; ++n) {
          NEXT_INST;
          if (OPCODE == TR_OP_MOVE) {
            cl->upvals[n].value = &R[B];
          } else {
            assert(OPCODE == TR_OP_GETUPVAL);
            cl->upvals[n].value = upvals[B].value;
          }
        }
      }
      int argc = GETARG_B(ci) >> 1;
      OBJ *argv = &R[GETARG_A(ci)+2];
      if (unlikely(call->method_missing)) {
        argc++;
        *(--argv) = call->message;
      }
      OBJ ret = TrMethod_call(vm,
                              call->method,
                              R[GETARG_A(ci)], /* receiver */
                              argc, argv,
                              GETARG_B(ci) & 1, /* splat */
                              cl /* closure */
                             );
      /* Handle throw if some.
         A "throw" is done by returning TR_UNDEF to exit a current call frame (TrFrame)
         until one handle it by returning are real value or continuing execution.
         Non-local returns and exception propagation are implemented this way.
         Rubinius and Python do it this way. */
      if (unlikely(ret == TR_UNDEF)) {
        switch (vm->throw_reason) {
          case TR_THROW_EXCEPTION:
            /* TODO run rescue and stop propagation if rescued */
            /* TODO run ensure block */
            RETURN(TR_UNDEF);

          case TR_THROW_RETURN:
            /* TODO run ensure block */
            if (f->closure) RETURN(TR_UNDEF);
            RETURN(vm->throw_value);

          case TR_THROW_BREAK:
            break;

          default:
            assert(0 && "BUG: invalid throw_reason");
        }
      }

      R[GETARG_A(ci)] = ret;
      DISPATCH;
    }
    
    /* definition */
    OP(DEF):        VM_RETHROW(TrVM_defmethod(vm, f, k[Bx], blocks[A], 0, 0)); DISPATCH;
    OP(METADEF):    VM_RETHROW(TrVM_defmethod(vm, f, k[Bx], blocks[A], 1, R[nA])); ip++; DISPATCH;
    OP(CLASS):      VM_RETHROW(TrVM_defClass(vm, k[Bx], blocks[A], 0, R[nA])); ip++; DISPATCH;
    OP(MODULE):     VM_RETHROW(TrVM_defClass(vm, k[Bx], blocks[A], 1, 0)); DISPATCH;
    
    /* jumps */
    OP(JMP):        ip += sBx; DISPATCH;
    OP(JMPIF):      if ( TR_TEST(R[A])) ip += sBx; DISPATCH;
    OP(JMPUNLESS):  if (!TR_TEST(R[A])) ip += sBx; DISPATCH;
    
    /* arithmetic optimizations */
    /* TODO cache lookup in tr_send and force send if method was redefined */
    #define ARITH_OPT(MSG, FUNC) {\
      OBJ rb = RK(B); \
      if (likely(TR_IS_FIX(rb))) \
        R[A] = FUNC; \
      else \
        R[A] = tr_send(rb, MSG, RK(C)); \
    }
    OP(ADD):        ARITH_OPT(vm->sADD, TR_INT2FIX(TR_FIX2INT(rb) + TR_FIX2INT(RK(C))) ); DISPATCH;
    OP(SUB):        ARITH_OPT(vm->sSUB, TR_INT2FIX(TR_FIX2INT(rb) - TR_FIX2INT(RK(C))) ); DISPATCH;
    OP(LT):         ARITH_OPT(vm->sLT, TR_BOOL(TR_FIX2INT(rb) < TR_FIX2INT(RK(C))) ); DISPATCH;
    OP(NEG):        ARITH_OPT(vm->sNEG, TR_INT2FIX(-TR_FIX2INT(rb)) ); DISPATCH;
    OP(NOT): {
      OBJ rb = RK(B);
      R[A] = TR_BOOL(!TR_TEST(rb));
      DISPATCH;
    }
  END_OPCODES;
}

/* returns the backtrace of the current call frames */
OBJ TrVM_backtrace(VM) {
  OBJ backtrace = TrArray_new(vm);
  
  if (!vm->frame) return backtrace;
  
  /* skip a frame since it's the one doing the raising */
  TrFrame *f = vm->frame->previous;
  while (f) {
    OBJ str;
    char *filename = f->filename ? TR_STR_PTR(f->filename) : "?";
    if (f->method)
      str = tr_sprintf(vm, "\tfrom %s:%lu:in `%s'",
                       filename, f->line, TR_STR_PTR(TR_CMETHOD(f->method)->name));
    else
      str = tr_sprintf(vm, "\tfrom %s:%lu",
                       filename, f->line);
    TR_ARRAY_PUSH(backtrace, str);
    
    f = f->previous;
  }
  
  return backtrace;
}

OBJ TrVM_eval(VM, char *code, char *filename) {
  TrBlock *b = TrBlock_compile(vm, code, filename, 0);
  if (!b) return TR_UNDEF;
  if (vm->debug) TrBlock_dump(vm, b);
  return TrVM_run(vm, b, vm->self, TR_CLASS(vm->self), 0, 0);
}

OBJ TrVM_load(VM, char *filename) {
  FILE *fp;
  struct stat stats;
  
  if (stat(filename, &stats) == -1) tr_raise_errno(filename);
  fp = fopen(filename, "rb");
  if (!fp) tr_raise_errno(filename);
  
  char *string = TR_ALLOC_N(char, stats.st_size + 1);
  if (fread(string, 1, stats.st_size, fp) == (unsigned)stats.st_size)
    return TrVM_eval(vm, string, filename);
  
  tr_raise_errno(filename);
  return TR_NIL;
}

OBJ TrVM_run(VM, TrBlock *b, OBJ self, OBJ Class, int argc, OBJ argv[]) {
  OBJ ret = TR_NIL;
  TR_WITH_FRAME(self, Class, 0, {
    ret = TrVM_interpret(vm, vm->frame, b, 0, argc, argv, 0);
  });
  return ret;
}

TrVM *TrVM_new() {
  GC_INIT();

  TrVM *vm = TR_ALLOC(TrVM);
  vm->symbols = kh_init(str);
  vm->globals = kh_init(OBJ);
  vm->consts = kh_init(OBJ);
  vm->debug = 0;
  
  /* bootstrap core Classes,
     order is important here, so careful, mkay? */
  TrMethod_init(vm);
  TrSymbol_init(vm);
  TrModule_init(vm);
  TrClass_init(vm);
  TrObject_preinit(vm);
  TrClass *symbolc = (TrClass*)TR_CORE_CLASS(Symbol);
  TrClass *modulec = (TrClass*)TR_CORE_CLASS(Module);
  TrClass *Classc = (TrClass*)TR_CORE_CLASS(Class);
  TrClass *methodc = (TrClass*)TR_CORE_CLASS(Method);
  TrClass *objectc = (TrClass*)TR_CORE_CLASS(Object);
  /* set proper superClass has Object is defined last */
  symbolc->super = modulec->super = methodc->super = (OBJ)objectc;
  Classc->super = (OBJ)modulec;
  /* inject core Classes metaClass */
  symbolc->Class = TrMetaClass_new(vm, objectc->Class);
  modulec->Class = TrMetaClass_new(vm, objectc->Class);
  Classc->Class = TrMetaClass_new(vm, objectc->Class);
  methodc->Class = TrMetaClass_new(vm, objectc->Class);
  objectc->Class = TrMetaClass_new(vm, objectc->Class);
  
  /* Some symbols are created before Object, so make sure all have proper Class. */
  TR_KH_EACH(vm->symbols, i, sym, {
    TR_COBJECT(sym)->Class = (OBJ)symbolc;
  });
  
  /* bootstrap rest of core Classes, order is no longer important here */
  TrObject_init(vm);
  TrError_init(vm);
  TrBinding_init(vm);
  TrPrimitive_init(vm);
  TrKernel_init(vm);
  TrString_init(vm);
  TrFixnum_init(vm);
  TrArray_init(vm);
  TrHash_init(vm);
  TrRange_init(vm);
  TrRegexp_init(vm);
  
  vm->self = TrObject_alloc(vm, 0);
  vm->cf = -1;
  
  /* cache some commonly used values */
  vm->sADD = tr_intern("+");
  vm->sSUB = tr_intern("-");
  vm->sLT = tr_intern("<");
  vm->sNEG = tr_intern("@-");
  vm->sNOT = tr_intern("!");
  
  TR_FAILSAFE(TrVM_load(vm, "lib/boot.rb"));
  
  return vm;
}

void TrVM_destroy(TrVM *vm) {
  kh_destroy(str, vm->symbols);
  GC_gcollect();
}
