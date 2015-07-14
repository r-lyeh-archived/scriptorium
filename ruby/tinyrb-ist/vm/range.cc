#include "tr.h"
#include "internal.h"

OBJ TrRange_new(VM, OBJ first, OBJ last, int exclusive) {
  TrRange *r = TR_INIT_CORE_OBJECT(Range);
  r->first = first;
  r->last = last;
  r->exclusive = exclusive;
  return (OBJ)r;
}

static OBJ TrRange_first(VM, OBJ self) { return TR_CRANGE(self)->first; }
static OBJ TrRange_last(VM, OBJ self) { return TR_CRANGE(self)->last; }
static OBJ TrRange_exclude_end(VM, OBJ self) { return TR_BOOL(TR_CRANGE(self)->exclusive); }

//static OBJ TrRange_each(VM, OBJ self) { }

void TrRange_init(VM) {
  OBJ c = TR_INIT_CORE_CLASS(Range, Object);
  tr_def(c, "first", TrRange_first, 0);
  tr_def(c, "last", TrRange_last, 0);
  tr_def(c, "exclude_end?", TrRange_exclude_end, 0);
  //tr_def(c, "each", TrRange_each, 0);
}
