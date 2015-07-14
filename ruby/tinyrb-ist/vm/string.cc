#include <stdarg.h>
#include <stdio.h>
#include "tr.h"
#include "internal.h"

/* symbol */

static OBJ TrSymbol_lookup(VM, const char *str) {
  khash_t(str) *kh = vm->symbols;
  khiter_t k = kh_get(str, kh, str);
  if (k != kh_end(kh)) return kh_value(kh, k);
  return TR_NIL;
}

static void TrSymbol_add(VM, const char *str, OBJ id) {
  int ret;
  khash_t(str) *kh = vm->symbols;
  khiter_t k = kh_put(str, kh, str, &ret);
  if (!ret) kh_del(str, kh, k);
  kh_value(kh, k) = id;
}

OBJ TrSymbol_new(VM, const char *str) {
  OBJ id = TrSymbol_lookup(vm, str);
  
  if (!id) {
    TrSymbol *s = TR_INIT_CORE_OBJECT(Symbol);
    s->len = strlen(str);
    s->ptr = TR_ALLOC_N(char, s->len+1);
    s->interned = 1;
    TR_MEMCPY_N(s->ptr, str, char, s->len);
    s->ptr[s->len] = '\0';
    
    id = (OBJ)s;
    TrSymbol_add(vm, s->ptr, id);
  }
  return id;
}

static OBJ TrSymbol_to_s(VM, OBJ self) {
  return TrString_new(vm, TR_STR_PTR(self), TR_STR_LEN(self));
}

void TrSymbol_init(VM) {
  OBJ c = TR_INIT_CORE_CLASS(Symbol, Object);
  tr_def(c, "to_s", TrSymbol_to_s, 0);
}

/* string */

static OBJ TrString_to_s(VM, OBJ self) {
  UNUSED(vm);
  return self;
}

static OBJ TrString_size(VM, OBJ self) {
  return TR_INT2FIX(TR_CSTRING(self)->len);
}

OBJ TrString_new(VM, const char *str, size_t len) {
  TrString *s = TR_INIT_CORE_OBJECT(String);
  s->len = len;
  s->ptr = TR_ALLOC_N(char, s->len+1);
  s->interned = 0;
  TR_MEMCPY_N(s->ptr, str, char, s->len);
  s->ptr[s->len] = '\0';
  return (OBJ)s;
}

OBJ TrString_new2(VM, const char *str) {
  return TrString_new(vm, str, strlen(str));
}

OBJ TrString_new3(VM, size_t len) {
  TrString *s = TR_INIT_CORE_OBJECT(String);
  s->len = len;
  s->ptr = TR_ALLOC_N(char, s->len+1);
  s->interned = 0;
  s->ptr[s->len] = '\0';
  return (OBJ)s;
}

OBJ TrString_add(VM, OBJ self, OBJ other) {
  return tr_sprintf(vm, "%s%s", TR_STR_PTR(self), TR_STR_PTR(other));
}

OBJ TrString_push(VM, OBJ self, OBJ other) {
  TrString *s = TR_CSTRING(self);
  TrString *o = TR_CSTRING(other);
  
  size_t orginal_len = s->len;
  s->len += o->len;
  s->ptr = (char *)TR_REALLOC(s->ptr, s->len+1);
  TR_MEMCPY_N(s->ptr + orginal_len, o->ptr, char, o->len);
  s->ptr[s->len] = '\0';

  return self;
}

OBJ TrString_replace(VM, OBJ self, OBJ other) {
  TR_FREE(TR_STR_PTR(self));
  TR_STR_PTR(self) = TR_STR_PTR(other);
  TR_STR_LEN(self) = TR_STR_LEN(other);
  return self;
}

OBJ TrString_cmp(VM, OBJ self, OBJ other) {
  if (!TR_IS_A(other, String)) return TR_INT2FIX(-1);
  return TR_INT2FIX(strcmp(TR_STR_PTR(self), TR_STR_PTR(other)));
}

OBJ TrString_substring(VM, OBJ self, OBJ start, OBJ len) {
  int s = TR_FIX2INT(start);
  int l = TR_FIX2INT(len);
  if (s < 0 || (s+l) > (int)TR_STR_LEN(self)) return TR_NIL;
  return TrString_new(vm, TR_STR_PTR(self)+s, l);
}

OBJ TrString_to_sym(VM, OBJ self) {
  return tr_intern(TR_STR_PTR(self));
}

OBJ tr_sprintf(VM, const char *fmt, ...) {
  va_list arg;
  va_start(arg, fmt);
  int len = vsnprintf(NULL, 0, fmt, arg);
  char *ptr = (char *)alloca(sizeof(char) * len);
  va_end(arg);
  va_start(arg, fmt);
  vsprintf(ptr, fmt, arg);
  va_end(arg);
  OBJ str = TrString_new(vm, ptr, len);
  TR_FREE(ptr);
  return str;
}

void TrString_init(VM) {
  OBJ c = TR_INIT_CORE_CLASS(String, Object);
  tr_def(c, "to_s", TrString_to_s, 0);
  tr_def(c, "to_sym", TrString_to_sym, 0);
  tr_def(c, "size", TrString_size, 0);
  tr_def(c, "replace", TrString_replace, 1);
  tr_def(c, "substring", TrString_substring, 2);
  tr_def(c, "+", TrString_add, 1);
  tr_def(c, "<<", TrString_push, 1);
  tr_def(c, "<=>", TrString_cmp, 1);
}
