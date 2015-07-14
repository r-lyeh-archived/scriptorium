#include <stdarg.h>
#include "tr.h"
#include "internal.h"

OBJ TrArray_new(VM) {
  TrArray *a = TR_INIT_CORE_OBJECT(Array);
  kv_init(a->kv);
  return (OBJ)a;
}

OBJ TrArray_new2(VM, int argc, ...) {
  OBJ a = TrArray_new(vm);
  va_list argp;
  int i;
  va_start(argp, argc);
  for (i = 0; i < argc; ++i) TR_ARRAY_PUSH(a, va_arg(argp, OBJ));
  va_end(argp);
  return a;
}

OBJ TrArray_new3(VM, int argc, OBJ items[]) {
  OBJ a = TrArray_new(vm);
  int i;
  for (i = 0; i < argc; ++i) TR_ARRAY_PUSH(a, items[i]);
  return a;
}

static OBJ TrArray_push(VM, OBJ self, OBJ x) {
  UNUSED(vm);
  TR_ARRAY_PUSH(self, x);
  return x;
}

static inline int TrArray_at2index(VM, OBJ self, OBJ at) {
  int i = TR_FIX2INT(at);
  if (i < 0) i = TR_ARRAY_SIZE(self) + i;
  return i;
}

static OBJ TrArray_at(VM, OBJ self, OBJ at) {
  int i = TrArray_at2index(vm, self, at);
  if (i < 0 || i >= (int)TR_ARRAY_SIZE(self)) return TR_NIL;
  return TR_ARRAY_AT(self, i);
}

static OBJ TrArray_set(VM, OBJ self, OBJ at, OBJ x) {
  int i = TrArray_at2index(vm, self, at);
  if (i < 0) tr_raise(IndexError, "index %d out of array", i);
  kv_a(OBJ, (TR_CARRAY(self))->kv, i) = x;
  return x;
}

static OBJ TrArray_length(VM, OBJ self) {
  return TR_INT2FIX(TR_ARRAY_SIZE(self));
}

void TrArray_init(VM) {
  OBJ c = TR_INIT_CORE_CLASS(Array, Object);
  tr_def(c, "length", TrArray_length, 0);
  tr_def(c, "size", TrArray_length, 0);
  tr_def(c, "<<", TrArray_push, 1);
  tr_def(c, "[]", TrArray_at, 1);
  tr_def(c, "[]=", TrArray_set, 2);
}
