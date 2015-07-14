#include "tr.h"
#include "internal.h"

OBJ TrHash_new(VM) {
  TrHash *h = TR_INIT_CORE_OBJECT(Hash);
  h->kh = kh_init(OBJ);
  return (OBJ)h;
}

OBJ TrHash_new2(VM, size_t n, OBJ items[]) {
  TrHash *h = (TrHash *)TrHash_new(vm);
  size_t i;
  int ret;
  for (i = 0; i < n; i+=2) {
    khiter_t k = kh_put(OBJ, h->kh, items[i], &ret);
    kh_value(h->kh, k) = items[i+1];
  }
  return (OBJ)h;
}

static OBJ TrHash_size(VM, OBJ self) {
  TrHash *h = TR_CHASH(self);
  return TR_INT2FIX(kh_size(h->kh));
}

/* TODO use Object#hash as the key */
static OBJ TrHash_get(VM, OBJ self, OBJ key) {
  TrHash *h = TR_CHASH(self);
  khiter_t k = kh_get(OBJ, h->kh, key);
  if (k != kh_end(h->kh)) return kh_value(h->kh, k);
  return TR_NIL;
}

static OBJ TrHash_set(VM, OBJ self, OBJ key, OBJ value) {
  TrHash *h = TR_CHASH(self);
  int ret;
  khiter_t k = kh_put(OBJ, h->kh, key, &ret);
  if (!ret) kh_del(OBJ, h->kh, k);
  kh_value(h->kh, k) = value;
  return value;
}

static OBJ TrHash_delete(VM, OBJ self, OBJ key) {
  TrHash *h = TR_CHASH(self);
  khiter_t k = kh_get(OBJ, h->kh, key);
  if (k != kh_end(h->kh)) {
    OBJ value = kh_value(h->kh, k);
    kh_del(OBJ, h->kh, k);
    return value;
  }
  return TR_NIL;
}

void TrHash_init(VM) {
  OBJ c = TR_INIT_CORE_CLASS(Hash, Object);
  tr_def(c, "length", TrHash_size, 0);
  tr_def(c, "size", TrHash_size, 0);
  tr_def(c, "[]", TrHash_get, 1);
  tr_def(c, "[]=", TrHash_set, 2);
  tr_def(c, "delete", TrHash_delete, 1);
}
