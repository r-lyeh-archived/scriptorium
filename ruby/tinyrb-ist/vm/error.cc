#include "tr.h"

/* Error management stuff */

/* Exception
 NoMemoryError
 ScriptError
   LoadError
   NotImplementedError
   SyntaxError
 SignalException
   Interrupt
 StandardError
   ArgumentError
   IOError
     EOFError
   IndexError
   LocalJumpError
   NameError
     NoMethodError
   RangeError
     FloatDomainError
   RegexpError
   RuntimeError
   SecurityError
   SystemCallError
   SystemStackError
   ThreadError
   TypeError
   ZeroDivisionError
 SystemExit
 fatal */

OBJ TrException_new(VM, OBJ Class, OBJ message) {
  OBJ e = TrObject_alloc(vm, Class);
  tr_setivar(e, "@message", message);
  tr_setivar(e, "@backtrace", TR_NIL);
  return (OBJ)e;
}

static OBJ TrException_cexception(VM, OBJ self, int argc, OBJ argv[]) {
  if (argc == 0) return TrException_new(vm, self, TR_CCLASS(self)->name);
  return TrException_new(vm, self, argv[0]);
}

static OBJ TrException_iexception(VM, OBJ self, int argc, OBJ argv[]) {
  if (argc == 0) return self;
  return TrException_new(vm, TR_CLASS(self), argv[0]);
}

static OBJ TrException_message(VM, OBJ self) {
  return tr_getivar(self, "@message");
}

OBJ TrException_backtrace(VM, OBJ self) {
  return tr_getivar(self, "@backtrace");
}

OBJ TrException_set_backtrace(VM, OBJ self, OBJ backtrace) {
  return tr_setivar(self, "@backtrace", backtrace);
}

OBJ TrException_default_handler(VM, OBJ exception) {
  TrClass *c = TR_CCLASS(TR_CLASS(exception));
  OBJ msg = tr_getivar(exception, "@message");
  OBJ backtrace = tr_getivar(exception, "@backtrace");
  
  printf("%s: %s\n", TR_STR_PTR(c->name), TR_STR_PTR(msg));
  if (backtrace) {
    TR_ARRAY_EACH(backtrace, i, v, {
      printf("%s\n", TR_STR_PTR(v));
    });
  }
  
  TrVM_destroy(vm);
  exit(1);
}

void TrError_init(VM) {
  OBJ c = vm->cException = tr_defClass("Exception", 0);
  tr_metadef(c, "exception", TrException_cexception, -1);
  tr_def(c, "exception", TrException_iexception, -1);
  tr_def(c, "backtrace", TrException_backtrace, 0);
  tr_def(c, "message", TrException_message, 0);
  tr_def(c, "to_s", TrException_message, 0);
  
  vm->cScriptError = tr_defClass("ScriptError", vm->cException);
  vm->cSyntaxError = tr_defClass("SyntaxError", vm->cScriptError);
  vm->cStandardError = tr_defClass("StandardError", vm->cException);
  vm->cArgumentError = tr_defClass("ArgumentError", vm->cStandardError);
  vm->cRegexpError = tr_defClass("RegexpError", vm->cStandardError);
  vm->cRuntimeError = tr_defClass("RuntimeError", vm->cStandardError);
  vm->cTypeError = tr_defClass("TypeError", vm->cStandardError);
  vm->cSystemCallError = tr_defClass("SystemCallError", vm->cStandardError);
  vm->cIndexError = tr_defClass("IndexError", vm->cStandardError);
  vm->cLocalJumpError = tr_defClass("LocalJumpError", vm->cStandardError);
  vm->cSystemStackError = tr_defClass("SystemStackError", vm->cStandardError);
  vm->cNameError = tr_defClass("NameError", vm->cStandardError);
  vm->cNoMethodError = tr_defClass("NoMethodError", vm->cNameError);
}
