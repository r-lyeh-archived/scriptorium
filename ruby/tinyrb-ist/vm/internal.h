#ifndef _INTERNAL_H_
#define _INTERNAL_H_

#define TR_ALLOC(T)          (T *)TR_MALLOC(sizeof(T))
#define TR_ALLOC_N(T,N)      (T *)TR_MALLOC(sizeof(T)*(N))

#define TR_MEMZERO(X,T)      memset((X), 0, sizeof(T))
#define TR_MEMZERO_N(X,T,N)  memset((X), 0, sizeof(T)*(N))
#define TR_MEMCPY(X,Y,T)     memcpy((X), (Y), sizeof(T))
#define TR_MEMCPY_N(X,Y,T,N) memcpy((X), (Y), sizeof(T)*(N))

/* ast building macros */
#define NODE(T,A)            TrNode_new(compiler->vm, NODE_##T, (A), 0, 0, 0, compiler->line)
#define NODE2(T,A,B)         TrNode_new(compiler->vm, NODE_##T, (A), (B), 0, 0, compiler->line)
#define NODE3(T,A,B,C)       TrNode_new(compiler->vm, NODE_##T, (A), (B), (C), 0, compiler->line)
#define NODE4(T,A,B,C,D)     TrNode_new(compiler->vm, NODE_##T, (A), (B), (C), (D), compiler->line)
#define NODES(I)             TrArray_new2(compiler->vm, 1, (I))
#define NODES_N(N,...)       TrArray_new2(compiler->vm, (N), ##__VA_ARGS__)
#define PUSH_NODE(A,N)       TR_ARRAY_PUSH((A),(N))
#define SYMCAT(A,B)          tr_intern(strcat(((TrString*)(A))->ptr, ((TrString*)(B))->ptr))

/* This provides the compiler about branch hints, so it
   keeps the normal case fast. Stolen from Rubinius. */
#ifdef __GNUC__
#define likely(x)       __builtin_expect((long int)(x),1)
#define unlikely(x)     __builtin_expect((long int)(x),0)
#else
#define likely(x)       (x)
#define unlikely(x)     (x)
#endif

/* types of nodes in the AST built by the parser */
typedef enum {
  NODE_ROOT,
  NODE_BLOCK,
  NODE_VALUE,
  NODE_STRING,
  NODE_ASSIGN,
  NODE_ARG,
  NODE_SEND,
  NODE_MSG,
  NODE_IF,
  NODE_UNLESS,
  NODE_LOGICAND,
  NODE_LOGICOR,
  NODE_AND,
  NODE_OR,
  NODE_WHILE,
  NODE_UNTIL,
  NODE_BOOL,
  NODE_NIL,
  NODE_SELF,
  NODE_LEAVE,
  NODE_RETURN,
  NODE_BREAK,
  NODE_YIELD,
  NODE_DEF,
  NODE_METHOD,
  NODE_PARAM,
  NODE_CLASS,
  NODE_MODULE,
  NODE_CONST,
  NODE_SETCONST,
  NODE_ARRAY,
  NODE_HASH,
  NODE_RANGE,
  NODE_GETIVAR,
  NODE_SETIVAR,
  NODE_GETCVAR,
  NODE_SETCVAR,
  NODE_GETGLOBAL,
  NODE_SETGLOBAL,
  NODE_ADD,
  NODE_INC,
  NODE_DEC,
  NODE_SUB,
  NODE_LT,
  NODE_NEG,
  NODE_NOT,
  NODE_FOR
} TrNodeType;

typedef struct {
  TR_OBJECT_HEADER;
  TrNodeType ntype;
  OBJ args[4];
  size_t line;
} TrNode;

typedef struct {
  int line;
  OBJ filename;
  TrVM *vm;
  TrBlock *block;
  size_t reg;
  OBJ node;
} TrCompiler;

/* node */
OBJ TrNode_new(VM, TrNodeType type, OBJ a, OBJ b, OBJ c, OBJ d, size_t line);

/* block */
TrBlock *TrBlock_new(TrCompiler *compiler, TrBlock *parent);
void TrBlock_dump(VM, TrBlock *b);
int TrBlock_push_value(TrBlock *blk, OBJ k);
int TrBlock_push_string(TrBlock *blk, char *str);
int TrBlock_find_local(TrBlock *blk, OBJ name);
int TrBlock_push_local(TrBlock *blk, OBJ name);
int TrBlock_find_upval(TrBlock *blk, OBJ name);
int TrBlock_find_upval_in_scope(TrBlock *blk, OBJ name);
int TrBlock_push_upval(TrBlock *blk, OBJ name);

/* compiler */
TrCompiler *TrCompiler_new(VM, const char *fn);
void TrCompiler_compile(TrCompiler *c);

#endif /* _INTERNAL_H_ */
