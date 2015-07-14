#include "tr.h"
#include "opcode.h"
#include "internal.h"

TrBlock *TrBlock_new(TrCompiler *compiler, TrBlock *parent) {
  TrBlock *b = TR_ALLOC(TrBlock);
  kv_init(b->k);
  kv_init(b->code);
  kv_init(b->locals);
  kv_init(b->strings);
  kv_init(b->sites);
  b->regc = 0;
  b->argc = 0;
  b->filename = compiler->filename;
  b->line = 1;
  b->parent = parent;
  return b;
}

#define INSPECT_K(K)  (TR_IS_A(K, Symbol) ? TR_STR_PTR(K) : (sprintf(buf, "%d", TR_FIX2INT(K)), buf))

static OBJ TrBlock_dump2(VM, TrBlock *b, int level) {
  static char *opcode_names[] = { TR_OP_NAMES };
  char buf[10];
  
  size_t i;
  printf("; block definition: %p (level %lu)\n", b, (long unsigned)level);
  printf("; %lu registers ; %lu nested blocks\n", (long unsigned int)b->regc, (long unsigned int)kv_size(b->blocks));
  printf("; %lu args ", (long unsigned int)b->argc);
  if (b->arg_splat) printf(", splat");
  printf("\n");
  if (kv_size(b->defaults) > 0) {
    printf("; defaults table: ");
    for (i = 0; i < kv_size(b->defaults); ++i) printf("%d ", kv_A(b->defaults, i));
    printf("\n");
  }
  for (i = 0; i < kv_size(b->locals); ++i)
	  printf(".local  %-8s ; %lu\n", INSPECT_K(kv_A(b->locals, i)), (long unsigned int)i);
  for (i = 0; i < kv_size(b->upvals); ++i)
	  printf(".upval  %-8s ; %lu\n", INSPECT_K(kv_A(b->upvals, i)), (long unsigned int)i);
  for (i = 0; i < kv_size(b->k); ++i)
	  printf(".value  %-8s ; %lu\n", INSPECT_K(kv_A(b->k, i)), (long unsigned int)i);
  for (i = 0; i < kv_size(b->strings); ++i)
	  printf(".string %-8s ; %lu\n", kv_A(b->strings, i), (long unsigned int)i);
  for (i = 0; i < kv_size(b->code); ++i) {
    TrInst op = kv_A(b->code, i);
	printf("[%03lu] %-10s %3d %3d %3d", (long unsigned int)i, opcode_names[GET_OPCODE(op)], GETARG_A(op), GETARG_B(op), GETARG_C(op));
    switch (GET_OPCODE(op)) {
      case TR_OP_LOADK:    printf(" ; R[%d] = %s", GETARG_A(op), INSPECT_K(kv_A(b->k, GETARG_Bx(op)))); break;
      case TR_OP_STRING:   printf(" ; R[%d] = \"%s\"", GETARG_A(op), kv_A(b->strings, GETARG_Bx(op))); break;
      case TR_OP_LOOKUP:   printf(" ; R[%d] = R[%d].method(:%s)", GETARG_A(op)+1, GETARG_A(op), INSPECT_K(kv_A(b->k, GETARG_Bx(op)))); break;
      case TR_OP_CALL:     printf(" ; R[%d] = R[%d].R[%d](%d)", GETARG_A(op), GETARG_A(op), GETARG_A(op)+1, GETARG_B(op)>>1); break;
      case TR_OP_SETUPVAL: printf(" ; %s = R[%d]", INSPECT_K(kv_A(b->upvals, GETARG_B(op))), GETARG_A(op)); break;
      case TR_OP_GETUPVAL: printf(" ; R[%d] = %s", GETARG_A(op), INSPECT_K(kv_A(b->upvals, GETARG_B(op)))); break;
      case TR_OP_JMP:      printf(" ; %d", GETARG_sBx(op)); break;
      case TR_OP_DEF:      printf(" ; %s => %p", INSPECT_K(kv_A(b->k, GETARG_Bx(op))), kv_A(b->blocks, GETARG_A(op))); break;
    }
    printf("\n");
  }
  printf("; block end\n\n");

  for (i = 0; i < kv_size(b->blocks); ++i)
    TrBlock_dump2(vm, kv_A(b->blocks, i), level+1);
  return TR_NIL;
}

void TrBlock_dump(VM, TrBlock *b) {
  TrBlock_dump2(vm, b, 0);
}

int TrBlock_push_value(TrBlock *blk, OBJ k) {
  size_t i;
  for (i = 0; i < kv_size(blk->k); ++i)
    if (kv_A(blk->k, i) == k) return i;
  kv_push(OBJ, blk->k, k);
  return kv_size(blk->k)-1;
}

int TrBlock_push_string(TrBlock *blk, char *str) {
  size_t i;
  for (i = 0; i < kv_size(blk->strings); ++i)
    if (strcmp(kv_A(blk->strings, i), str) == 0) return i;
  int len = strlen(str);
  char *ptr = TR_ALLOC_N(char, len+1);
  TR_MEMCPY_N(ptr, str, char, len+1);
  kv_push(char *, blk->strings, ptr);
  return kv_size(blk->strings)-1;
}

int TrBlock_find_local(TrBlock *blk, OBJ name) {
  size_t i;
  for (i = 0; i < kv_size(blk->locals); ++i)
    if (kv_A(blk->locals, i) == name) return i;
  return -1;
}

int TrBlock_push_local(TrBlock *blk, OBJ name) {
  int i = TrBlock_find_local(blk, name);
  if (i != -1) return i;
  kv_push(OBJ, blk->locals, name);
  return kv_size(blk->locals)-1;
}

int TrBlock_find_upval(TrBlock *blk, OBJ name) {
  size_t i;
  for (i = 0; i < kv_size(blk->upvals); ++i)
    if (kv_A(blk->upvals, i) == name) return i;
  return -1;
}

int TrBlock_find_upval_in_scope(TrBlock *blk, OBJ name) {
  if (!blk->parent) return -1;
  int i = -1;
  while (blk && (i = TrBlock_find_local(blk, name)) == -1)
    blk = blk->parent;
  return i;
}

int TrBlock_push_upval(TrBlock *blk, OBJ name) {
  int i = TrBlock_find_upval(blk, name);
  if (i != -1) return i;
  
  TrBlock *b = blk;
  while (b->parent) {
    if (TrBlock_find_upval(b, name) == -1) kv_push(OBJ, b->upvals, name);
    b = b->parent;
  }
  
  return kv_size(blk->upvals)-1;
}
