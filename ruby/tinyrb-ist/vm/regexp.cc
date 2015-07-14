#include <pcre.h>
#include "tr.h"
#include "internal.h"

/* Loosely based on http://vcs.pcre.org/viewvc/code/trunk/pcredemo.c */

OBJ TrRegexp_new(VM, char *pattern, int options) {
  TrRegexp *r = TR_INIT_CORE_OBJECT(Regexp);
  const char *error;
  int erroffset;
  
  r->re = pcre_compile(
    pattern,              /* the pattern */
    options,              /* default options */
    &error,               /* for error message */
    &erroffset,           /* for error offset */
    NULL);                /* use default character tables */
  
  if (r->re == NULL) {
    TrRegex_free(vm, (OBJ)r);
    tr_raise(RegexpError, "compilation failed at offset %d: %s", erroffset, error);
  }
    
  return (OBJ)r;
}

OBJ TrRegexp_compile(VM, OBJ self, OBJ pattern) {
  UNUSED(self);
  return TrRegexp_new(vm, TR_STR_PTR(pattern), 0);
}

#define OVECCOUNT 30    /* should be a multiple of 3 */

OBJ TrRegexp_match(VM, OBJ self, OBJ str) {
  TrRegexp *r = TR_CREGEXP(self);
  char *subject = TR_STR_PTR(str);
  int rc;
  int ovector[OVECCOUNT];
  
  rc = pcre_exec(
    r->re,                /* the compiled pattern */
    NULL,                 /* no extra data - we didn't study the pattern */
    subject,              /* the subject string */
    TR_STR_LEN(str),      /* the length of the subject */
    0,                    /* start at offset 0 in the subject */
    0,                    /* default options */
    ovector,              /* output vector for substring information */
    OVECCOUNT);           /* number of elements in the output vector */
  
  if (rc < 0) return TR_NIL;
  
  if (rc == 0) {
    rc = OVECCOUNT/3;
    tr_raise(RegexpError, "Too much matches, only %d supported for now", rc - 1);
  }
  
  /* TODO should create a MatchData object */
  OBJ data = TrArray_new(vm);
  int i;
  for (i = 0; i < rc; i++) {
    char *substring_start = subject + ovector[2*i];
    int substring_length = ovector[2*i+1] - ovector[2*i];
    TR_ARRAY_PUSH(data, TrString_new(vm, substring_start, substring_length));
  }
  
  return data;
}

void TrRegex_free(VM, OBJ self) {
  UNUSED(vm);
  TrRegexp *r = (TrRegexp*)self;
  pcre_free(r->re);
  TR_FREE(r);
}

void TrRegexp_init(VM) {
  OBJ c = TR_INIT_CORE_CLASS(Regexp, Object);
  tr_metadef(c, "new", TrRegexp_compile, 1);
  tr_def(c, "match", TrRegexp_match, 1);
}
