#include "tr.h"
#include "internal.h"

#define TR_INIT_MODULE(M) \
  (M)->name = name; \
  (M)->methods = kh_init(OBJ); \
  (M)->meta = 0

/* included module proxy */

OBJ TrIModule_new(VM, OBJ module, OBJ super) {
  TrModule *m = TR_CMODULE(module);
  TrModule *im = TR_INIT_CORE_OBJECT(Module);
  im->name = m->name;
  im->methods = m->methods;
  im->super = super;
  return (OBJ)im;
}

/* module */

OBJ TrModule_new(VM, OBJ name) {
  TrModule *m = TR_INIT_CORE_OBJECT(Module);
  TR_INIT_MODULE(m);
  return (OBJ)m;
}

OBJ TrModule_instance_method(VM, OBJ self, OBJ name) {
  TrClass *Class = TR_CCLASS(self);
  while (Class) {
    OBJ method = TR_KH_GET(Class->methods, name);
    if (method) return method;
    Class = (TrClass *)Class->super;
  }
  return TR_NIL;
}

OBJ TrModule_add_method(VM, OBJ self, OBJ name, OBJ method) {
  TrClass *m = TR_CMODULE(self);
  TR_KH_SET(m->methods, name, method);
  TR_CMETHOD(method)->name = name;
  return method;
}

OBJ TrModule_alias_method(VM, OBJ self, OBJ new_name, OBJ old_name) {
  return TrModule_add_method(vm, self, new_name, TrModule_instance_method(vm, self, old_name));
}

OBJ TrModule_include(VM, OBJ self, OBJ mod) {
  TrClass *Class = TR_CCLASS(self);
  Class->super = TrIModule_new(vm, mod, Class->super);
  return mod;
}

static OBJ TrModule_name(VM, OBJ self) {
  return TR_CMODULE(self)->name;
}

void TrModule_init(VM) {
  OBJ c = TR_INIT_CORE_CLASS(Module, Object);
  tr_def(c, "name", TrModule_name, 0);
  tr_def(c, "include", TrModule_include, 1);
  tr_def(c, "instance_method", TrModule_instance_method, 1);
  tr_def(c, "alias_method", TrModule_alias_method, 2);
  tr_def(c, "to_s", TrModule_name, 0);
}

/* Class */

OBJ TrClass_new(VM, OBJ name, OBJ super) {
  TrClass *c = TR_INIT_CORE_OBJECT(Class);
  TR_INIT_MODULE(c);
  /* if VM is booting, those might not be set */
  if (super && TR_CCLASS(super)->Class) c->Class = TrMetaClass_new(vm, TR_CCLASS(super)->Class);
  c->super = super;
  return (OBJ)c;
}

OBJ TrClass_allocate(VM, OBJ self) {
  TrObject *o = TR_INIT_CORE_OBJECT(Object);
  o->Class = self;
  return (OBJ)o;
}

OBJ TrClass_superClass(VM, OBJ self) {
  OBJ super = TR_CCLASS(self)->super;
  while (super && !TR_IS_A(super, Class))
    super = TR_CCLASS(super)->super;
  return super;
}

void TrClass_init(VM) {
  OBJ c = TR_INIT_CORE_CLASS(Class, Module);
  tr_def(c, "superClass", TrClass_superClass, 0);
  tr_def(c, "allocate", TrClass_allocate, 0);
}

/* metaClass */

OBJ TrMetaClass_new(VM, OBJ super) {
  TrClass *c = TR_CCLASS(super);
  OBJ name = tr_sprintf(vm, "Class:%s", TR_STR_PTR(c->name));
  name = tr_intern(TR_STR_PTR(name)); /* symbolize */
  TrClass *mc = (TrClass *)TrClass_new(vm, name, 0);
  mc->super = super;
  mc->meta = 1;
  return (OBJ)mc;
}

/* method */

OBJ TrMethod_new(VM, TrFunc *func, OBJ data, int arity) {
  TrMethod *m = TR_INIT_CORE_OBJECT(Method);
  m->func = func;
  m->data = data;
  m->arity = arity;
  return (OBJ)m;
}

OBJ TrMethod_name(VM, OBJ self) { UNUSED(vm); return TR_CMETHOD(self)->name; }
OBJ TrMethod_arity(VM, OBJ self) { UNUSED(vm); return TR_INT2FIX(TR_CMETHOD(self)->arity); }

OBJ TrMethod_dump(VM, OBJ self) {
  TrMethod *m = TR_CMETHOD(self);
  if (m->name) printf("<Method '%s':%p>\n", TR_STR_PTR(m->name), m);
  if (m->data)
    TrBlock_dump(vm, (TrBlock*)m->data);
  else
    printf("<CFunction:%p>\n", m->func);
  return TR_NIL;
}

void TrMethod_init(VM) {
  OBJ c = TR_INIT_CORE_CLASS(Method, Object);
  tr_def(c, "name", TrMethod_name, 0);
  tr_def(c, "arity", TrMethod_arity, 0);
  tr_def(c, "dump", TrMethod_dump, 0);
}
