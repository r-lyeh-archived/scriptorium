#include "tr.h"
#include "internal.h"
#include "call.h"

OBJ TrObject_alloc(VM, OBJ Class) {
  TrObject *o = TR_INIT_CORE_OBJECT(Object);
  if (Class) o->Class = Class;
  return (OBJ) o;
}

int TrObject_type(VM, OBJ obj) {
  UNUSED(vm);
  switch (obj) {
    case TR_NIL: return TR_T_NilClass;
    case TR_TRUE: return TR_T_TrueClass;
    case TR_FALSE: return TR_T_FalseClass;
  }
  if (TR_IS_FIX(obj)) return TR_T_Fixnum;
  return TR_COBJECT(obj)->type;
}

OBJ TrObject_method(VM, OBJ self, OBJ name) {
  return TrModule_instance_method(vm, TR_CLASS(self), name);
}

OBJ TrObject_method_missing(VM, OBJ self, int argc, OBJ argv[]) {
  UNUSED(self);
  assert(argc > 0);
  tr_raise(NoMethodError, "Method not found: `%s'", TR_STR_PTR(argv[0]));
  return 0;
}

OBJ TrObject_send(VM, OBJ self, int argc, OBJ argv[]) {
  if (unlikely(argc == 0))
    tr_raise(ArgumentError, "wrong number of arguments (%d for 1)", argc);
  OBJ method = TrObject_method(vm, self, argv[0]);
  if (unlikely(method == TR_NIL)) {
    method = TrObject_method(vm, self, tr_intern("method_missing"));
    return TrMethod_call(vm, method, self, argc, argv, 0, 0);
  } else {
    return TrMethod_call(vm, method, self, argc-1, argv+1, 0, 0);
  }
}

/* TODO respect namespace */
OBJ TrObject_const_get(VM, OBJ self, OBJ name) {
  UNUSED(self);
  khiter_t k = kh_get(OBJ, vm->consts, name);
  if (k != kh_end(vm->consts)) return kh_value(vm->consts, k);
  return TR_NIL;
}

OBJ TrObject_const_set(VM, OBJ self, OBJ name, OBJ value) {
  UNUSED(self);
  int ret;
  khiter_t k = kh_put(OBJ, vm->consts, name, &ret);
  if (!ret) kh_del(OBJ, vm->consts, k);
  kh_value(vm->consts, k) = value;
  return value;
}

OBJ TrObject_add_singleton_method(VM, OBJ self, OBJ name, OBJ method) {
  TrObject *o = TR_COBJECT(self);
  if (!TR_CCLASS(o->Class)->meta)
    o->Class = TrMetaClass_new(vm, o->Class);
  assert(TR_CCLASS(o->Class)->meta && "first Class must be the metaClass");
  TrModule_add_method(vm, o->Class, name, method);
  return method;
}

static OBJ TrObject_Class(VM, OBJ self) {
  OBJ Class = TR_CLASS(self);
  /* find the first non-metaClass */
  while (Class && (!TR_IS_A(Class, Class) || TR_CCLASS(Class)->meta))
    Class = TR_CCLASS(Class)->super;
  assert(Class && "Classless object");
  return Class;
}

static OBJ TrObject_object_id(VM, OBJ self) {
  UNUSED(vm);
  return TR_INT2FIX((int)&self);
}

static OBJ TrObject_instance_eval(VM, OBJ self, OBJ code) {
  TrBlock *b = TrBlock_compile(vm, TR_STR_PTR(code), "<eval>", 0);
  if (!b) return TR_UNDEF;
  return TrVM_run(vm, b, self, TR_COBJECT(self)->Class, 0, 0);
}

static OBJ TrObject_inspect(VM, OBJ self) {
  const char *name;
  name = TR_STR_PTR(tr_send2(tr_send2(self, "Class"), "name"));
  return tr_sprintf(vm, "#<%s:%p>", name, (void*)self);
}

void TrObject_preinit(VM) {
  TR_INIT_CORE_CLASS(Object, /* ignored */ Object);
}

void TrObject_init(VM) {
  OBJ c = TR_CORE_CLASS(Object);
  tr_def(c, "Class", TrObject_Class, 0);
  tr_def(c, "method", TrObject_method, 1);
  tr_def(c, "method_missing", TrObject_method_missing, -1);
  tr_def(c, "send", TrObject_send, -1);
  tr_def(c, "object_id", TrObject_object_id, 0);
  tr_def(c, "instance_eval", TrObject_instance_eval, 1);
  tr_def(c, "to_s", TrObject_inspect, 0);
  tr_def(c, "inspect", TrObject_inspect, 0);
}
