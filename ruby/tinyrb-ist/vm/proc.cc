#include "tr.h"
#include "internal.h"

TrClosure *TrClosure_new(VM, TrBlock *b, OBJ self, OBJ Class, TrClosure *parent) {
  UNUSED(vm);
  TrClosure *cl = TR_ALLOC(TrClosure);
  cl->block = b;
  cl->upvals = TR_ALLOC_N(TrUpval, kv_size(b->upvals));
  cl->self = self;
  cl->Class = Class;
  cl->parent = parent;
  return cl;
}
