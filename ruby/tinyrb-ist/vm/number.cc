#include "tr.h"
#include "internal.h"

#define MATH(A,OP,B)  TR_INT2FIX(TR_FIX2INT(A) OP TR_FIX2INT(B))
#define CMP(A,OP,B)   TR_BOOL(TR_FIX2INT(A) OP TR_FIX2INT(B))

OBJ TrFixnum_add(VM, OBJ self, OBJ other) { UNUSED(vm); return MATH(self, +, other); }
OBJ TrFixnum_sub(VM, OBJ self, OBJ other) { UNUSED(vm); return MATH(self, -, other); }
OBJ TrFixnum_mul(VM, OBJ self, OBJ other) { UNUSED(vm); return MATH(self, *, other); }
OBJ TrFixnum_div(VM, OBJ self, OBJ other) { UNUSED(vm); return MATH(self, /, other); }

OBJ TrFixnum_eq(VM, OBJ self, OBJ other) { UNUSED(vm); return CMP(self, ==, other); }
OBJ TrFixnum_ne(VM, OBJ self, OBJ other) { UNUSED(vm); return CMP(self, !=, other); }
OBJ TrFixnum_lt(VM, OBJ self, OBJ other) { UNUSED(vm); return CMP(self, <, other); }
OBJ TrFixnum_gt(VM, OBJ self, OBJ other) { UNUSED(vm); return CMP(self, >, other); }
OBJ TrFixnum_le(VM, OBJ self, OBJ other) { UNUSED(vm); return CMP(self, <=, other); }
OBJ TrFixnum_ge(VM, OBJ self, OBJ other) { UNUSED(vm); return CMP(self, >=, other); }

OBJ TrFixnum_to_s(VM, OBJ self) {
  return tr_sprintf(vm, "%d", TR_FIX2INT(self));
}

void TrFixnum_init(VM) {
  OBJ c = TR_INIT_CORE_CLASS(Fixnum, Object);
  tr_def(c, "+", TrFixnum_add, 1);
  tr_def(c, "-", TrFixnum_sub, 1);
  tr_def(c, "*", TrFixnum_mul, 1);
  tr_def(c, "/", TrFixnum_div, 1);
  tr_def(c, "==", TrFixnum_eq, 1);
  tr_def(c, "!=", TrFixnum_eq, 1);
  tr_def(c, "<", TrFixnum_lt, 1);
  tr_def(c, "<=", TrFixnum_le, 1);
  tr_def(c, ">", TrFixnum_gt, 1);
  tr_def(c, ">=", TrFixnum_ge, 1);
  tr_def(c, "to_s", TrFixnum_to_s, 0);
}
