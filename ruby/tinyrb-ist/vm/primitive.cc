#include "tr.h"
#include "internal.h"

OBJ TrNil_to_s(VM, OBJ self) {
  UNUSED(self);
  return TrString_new2(vm, "");
}

OBJ TrTrue_to_s(VM, OBJ self) {
  UNUSED(self);
  return TrString_new2(vm, "true");
}

OBJ TrFalse_to_s(VM, OBJ self) {
  UNUSED(self);
  return TrString_new2(vm, "false");
}

void TrPrimitive_init(VM) {
  OBJ nilc = TR_INIT_CORE_CLASS(NilClass, Object);
  OBJ truec = TR_INIT_CORE_CLASS(TrueClass, Object);
  OBJ falsec = TR_INIT_CORE_CLASS(FalseClass, Object);
  
  tr_def(nilc, "to_s", TrNil_to_s, 0);
  tr_def(truec, "to_s", TrTrue_to_s, 0);
  tr_def(falsec, "to_s", TrFalse_to_s, 0);
}