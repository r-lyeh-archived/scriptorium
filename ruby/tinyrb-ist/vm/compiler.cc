#include "tr.h"
#include "opcode.h"
#include "internal.h"

/* ast node */
OBJ TrNode_new(VM, TrNodeType type, OBJ a, OBJ b, OBJ c, OBJ d, size_t line) {
  UNUSED(vm);
  TrNode *n = TR_ALLOC(TrNode);
  n->ntype = type;
  n->type = TR_T_Node;
  n->args[0] = a;
  n->args[1] = b;
  n->args[2] = c;
  n->args[3] = d;
  n->line = line;
  return (OBJ)n;
}

/* compiler */

TrCompiler *TrCompiler_new(VM, const char *fn) {
  TrCompiler *c = TR_ALLOC(TrCompiler);
  c->line = 1;
  c->vm = vm;
  c->block = TrBlock_new(c, 0);
  c->reg = 0;
  c->node = TR_NIL;
  c->filename = TrString_new2(vm, fn);
  return c;
}

/* code generation macros */
#define REG(R)                        if ((size_t)R >= b->regc) b->regc = (size_t)R+1;
#define PUSH_OP(BLK,I) ([&] { \
  kv_push(TrInst, (BLK)->code, (I)); \
  return kv_size(BLK->code)-1; \
}())
#define PUSH_OP_A(BLK, OP, A)         PUSH_OP(BLK, CREATE_ABC(TR_OP_##OP, A, 0, 0))
#define PUSH_OP_AB(BLK, OP, A, B)     PUSH_OP(BLK, CREATE_ABC(TR_OP_##OP, A, B, 0))
#define PUSH_OP_ABC(BLK, OP, A, B, C) PUSH_OP(BLK, CREATE_ABC(TR_OP_##OP, A, B, C))
#define PUSH_OP_ABx(BLK, OP, A, Bx)   PUSH_OP(BLK, CREATE_ABx(TR_OP_##OP, A, Bx))
#define PUSH_OP_AsBx(BLK, OP, A, sBx) ([&] { \
  TrInst __i = CREATE_ABx(TR_OP_##OP, A, 0); SETARG_sBx(__i, sBx); \
  return PUSH_OP(BLK, __i); \
}())

#define COMPILE_NODE(BLK,NODE,R) ([&] {\
  size_t nlocal = kv_size(BLK->locals); \
  REG(R); \
  TrCompiler_compile_node(vm, c, BLK, (TrNode *)NODE, R); \
  return kv_size(BLK->locals) - nlocal; \
}())

#define ASSERT_NO_LOCAL_IN(MSG) \
  if (start_reg != reg) tr_raise(SyntaxError, "Can't create local variable inside " #MSG)

#define COMPILE_NODES(BLK,NODES,I,R,ROFF) \
  TR_ARRAY_EACH(NODES, I, v, { \
    R += COMPILE_NODE(BLK, v, R+ROFF); \
    REG(R); \
  })
  
#define CNODE(N)              ((TrNode *)N)
#define NODE_TYPE(N)          (CNODE(N)->ntype)
#define NODE_ARG(N,I)         (CNODE(N)->args[I])

OBJ TrCompiler_compile_node(VM, TrCompiler *c, TrBlock *b, TrNode *n, int reg);

static int TrCompiler_compile_node_to_RK(VM, TrCompiler *c, TrBlock *b, TrNode *n, int reg) {
  int i;
  
  /* k value */
  if (NODE_TYPE(n) == NODE_VALUE) {
    return TrBlock_push_value(b, NODE_ARG(n, 0)) | 0x100;
    
  /* local */
  } else if (NODE_TYPE(n) == NODE_SEND && (i = TrBlock_find_local(b, NODE_ARG(NODE_ARG(n, 1), 0))) != -1) {
    return i;
  
  /* not a local, need to compile */
  } else {
    COMPILE_NODE(b, n, reg);
    return reg;
  }
  
}

OBJ TrCompiler_compile_node(VM, TrCompiler *c, TrBlock *b, TrNode *n, int reg) {
  if (!n) return TR_NIL;
  int start_reg = reg;
  REG(reg);
  b->line = n->line;
  /* TODO this shit is very repetitive, need to refactor */
  switch (n->ntype) {
    case NODE_ROOT:
    case NODE_BLOCK:
      COMPILE_NODES(b, n->args[0], i, reg, 0);
      break;
    case NODE_VALUE: {
      int i = TrBlock_push_value(b, n->args[0]);
      PUSH_OP_ABx(b, LOADK, reg, i);
    } break;
    case NODE_STRING: {
      int i = TrBlock_push_string(b, TR_STR_PTR(n->args[0]));
      PUSH_OP_ABx(b, STRING, reg, i);
    } break;
    case NODE_ARRAY: {
      size_t size = 0;
      if (n->args[0]) {
        size = TR_ARRAY_SIZE(n->args[0]);
        /* compile args */
        COMPILE_NODES(b, n->args[0], i, reg, i+1);
        ASSERT_NO_LOCAL_IN(Array);
      }
      PUSH_OP_AB(b, NEWARRAY, reg, size);
    } break;
    case NODE_HASH: {
      size_t size = 0;
      if (n->args[0]) {
        size = TR_ARRAY_SIZE(n->args[0]);
        /* compile args */
        COMPILE_NODES(b, n->args[0], i, reg, i+1);
        ASSERT_NO_LOCAL_IN(Hash);
      }
      PUSH_OP_AB(b, NEWHASH, reg, size/2);
    } break;
    case NODE_RANGE: {
      COMPILE_NODE(b, n->args[0], reg);
      COMPILE_NODE(b, n->args[1], reg+1);
      REG(reg+1);
      ASSERT_NO_LOCAL_IN(Range);
      PUSH_OP_ABC(b, NEWRANGE, reg, reg+1, n->args[2]);
    } break;
    case NODE_ASSIGN: {
      OBJ name = n->args[0];
      COMPILE_NODE(b, n->args[1], reg);
	  
      if (TrBlock_find_upval_in_scope(b, name) != -1) {
        /* upval */
		int i = TrBlock_push_upval(b, name);
        PUSH_OP_AB(b, SETUPVAL, reg, i);
      } else {
        /* local */
        int i = TrBlock_push_local(b, name);
        TrInst *last_inst = &kv_A(b->code, kv_size(b->code) - 1);
        switch (GET_OPCODE(*last_inst)) {
          case TR_OP_ADD: /* Those instructions can load direcly into a local */
          case TR_OP_SUB:
          case TR_OP_LT:
          case TR_OP_NEG:
          case TR_OP_NOT:
            SETARG_A(*last_inst, i); break;
          default:
            if (i != reg) PUSH_OP_AB(b, MOVE, i, reg);
        }
      }
    } break;
    case NODE_SETIVAR:
      COMPILE_NODE(b, n->args[1], reg);
      PUSH_OP_ABx(b, SETIVAR, reg, TrBlock_push_value(b, n->args[0]));
      break;
    case NODE_GETIVAR:
      PUSH_OP_ABx(b, GETIVAR, reg, TrBlock_push_value(b, n->args[0]));
      break;
    case NODE_SETCVAR:
      COMPILE_NODE(b, n->args[1], reg);
      PUSH_OP_ABx(b, SETCVAR, reg, TrBlock_push_value(b, n->args[0]));
      break;
    case NODE_GETCVAR:
      PUSH_OP_ABx(b, GETCVAR, reg, TrBlock_push_value(b, n->args[0]));
      break;
    case NODE_SETGLOBAL:
      COMPILE_NODE(b, n->args[1], reg);
      PUSH_OP_ABx(b, SETGLOBAL, reg, TrBlock_push_value(b, n->args[0]));
      break;
    case NODE_GETGLOBAL:
      PUSH_OP_ABx(b, GETGLOBAL, reg, TrBlock_push_value(b, n->args[0]));
      break;
    case NODE_SEND:
    /* can also be a variable access */
    {
      TrNode *msg = (TrNode *)n->args[1];
      OBJ name = msg->args[0];
      assert(msg->ntype == NODE_MSG);
      int i;
      /* local */
      if ((i = TrBlock_find_local(b, name)) != -1) {
        if (reg != i) PUSH_OP_AB(b, MOVE, reg, i);
        
      /* upval */
      } else if (TrBlock_find_upval_in_scope(b, name) != -1) {
        i = TrBlock_push_upval(b, name);
        PUSH_OP_AB(b, GETUPVAL, reg, i);
        
      /* method call */
      } else {
        /* receiver */
        if (n->args[0])
          COMPILE_NODE(b, n->args[0], reg);
        else
          PUSH_OP_A(b, SELF, reg);
        i = TrBlock_push_value(b, name);
        /* args */
        size_t argc = 0;
        if (msg->args[1]) {
          argc = TR_ARRAY_SIZE(msg->args[1]) << 1;
          TR_ARRAY_EACH(msg->args[1], i, v, {
            TrNode *arg = (TrNode *)v;
            assert(arg->ntype == NODE_ARG);
            reg += COMPILE_NODE(b, arg->args[0], reg+i+2);
            if (arg->args[1]) argc |= 1; /* splat */
          });
          ASSERT_NO_LOCAL_IN(arguments);
        }
        /* block */
        size_t blki = 0;
        TrBlock *blk = 0;
        if (n->args[2]) {
          blk = TrBlock_new(c, b);
          TrNode *blkn = (TrNode *)n->args[2];
          blki = kv_size(b->blocks) + 1;
          blk->argc = 0;
          if (blkn->args[1]) {
            blk->argc = TR_ARRAY_SIZE(blkn->args[1]);
            /* add parameters as locals in block context */
            TR_ARRAY_EACH(blkn->args[1], i, v, {
              TrNode *param = (TrNode *)v;
              TrBlock_push_local(blk, param->args[0]);
            });
          }
          kv_push(TrBlock *, b->blocks, blk);
          int blk_reg = kv_size(blk->locals);
          COMPILE_NODE(blk, blkn, blk_reg);
          PUSH_OP_A(blk, RETURN, blk_reg);
        }
        PUSH_OP_A(b, BOING, 0);
        PUSH_OP_ABx(b, LOOKUP, reg, i);
        PUSH_OP_ABC(b, CALL, reg, argc, blki);
        
        /* if passed block has upvalues generate one pseudo-instructions for each (A reg is ignored). */
        if (blk && kv_size(blk->upvals)) {
          size_t j;
          for(j = 0; j < kv_size(blk->upvals); ++j) {
            OBJ upval_name = kv_A(blk->upvals, j);
            int vali = TrBlock_find_local(b, upval_name);
            if (vali != -1)
              PUSH_OP_AB(b, MOVE, 0, vali);
            else
              PUSH_OP_AB(b, GETUPVAL, 0, TrBlock_find_upval(b, upval_name));
          }
        }
      }
    } break;
    case NODE_IF:
    case NODE_UNLESS: {
      /* condition */
      COMPILE_NODE(b, n->args[0], reg);
      int jmp;
      if (n->ntype == NODE_IF)
        jmp = PUSH_OP_A(b, JMPUNLESS, reg);
      else
        jmp = PUSH_OP_A(b, JMPIF, reg);
      /* body */
      COMPILE_NODES(b, n->args[1], i, reg, 0);
      SETARG_sBx(kv_A(b->code, jmp), kv_size(b->code) - jmp);
      /* else body */
      jmp = PUSH_OP_A(b, JMP, reg);
      if (n->args[2]) {
        COMPILE_NODES(b, n->args[2], i, reg, 0);
      } else {
        /* if condition fail and not else block
           nil is returned */
        PUSH_OP_A(b, NIL, reg);
      }
      SETARG_sBx(kv_A(b->code, jmp), kv_size(b->code) - jmp - 1);
    } break;
    case NODE_WHILE:
    case NODE_UNTIL: {
      size_t jmp_beg = kv_size(b->code);
      /* condition */
      COMPILE_NODE(b, n->args[0], reg);
      if (n->ntype == NODE_WHILE)
        PUSH_OP_ABx(b, JMPUNLESS, reg, 0);
      else
        PUSH_OP_ABx(b, JMPIF, reg, 0);
      size_t jmp_end = kv_size(b->code);
      /* body */
      COMPILE_NODES(b, n->args[1], i, reg, 0);
      SETARG_sBx(kv_A(b->code, jmp_end - 1), kv_size(b->code) - jmp_end + 1);
      PUSH_OP_AsBx(b, JMP, 0, 0-(kv_size(b->code) - jmp_beg) - 1);
    } break;
    case NODE_FOR: {
      /* BYTECODE DO FOR */
      COMPILE_NODE(b, n->args[0], reg); // range de valores
	  OBJ name = TrSymbol_new(c->vm, "each");
      int i = TrBlock_push_value(b, name); // each
      size_t blki = 0;
      TrBlock *blk = 0;
      if(n->args[2]) { // body
        blk = TrBlock_new(c,b);
        TrNode *blk_node = (TrNode *)TrNode_new(c->vm, NODE_BLOCK, n->args[2], n->args[1], 0, 0, b->line);
        blki = kv_size(b->blocks) + 1;
        blk->argc = 1;
        TrNode *param = (TrNode *)TrNode_new(c->vm, NODE_PARAM, n->args[1], 0, 0, 0, b->line);
        TrBlock_push_local(blk, param->args[0]); // var
        kv_push(TrBlock *, b->blocks, blk);
        int blk_reg = kv_size(blk->locals);
        COMPILE_NODE(blk, blk_node, blk_reg);
        PUSH_OP_A(blk, RETURN, blk_reg); 
      }
      PUSH_OP_A(b, BOING, 0);
      PUSH_OP_ABx(b, LOOKUP, reg, i); // procura "each" na lib
      PUSH_OP_ABC(b, CALL, reg, 0, blki);
    } break;
	case NODE_LOGICAND:{
		int jmp=0 ;
		TrCompiler_compile_node(vm, c, b, (TrNode *)n->args[0], reg);
		jmp = PUSH_OP_A(b, JMPUNLESS, reg);
		TrCompiler_compile_node(vm, c, b, (TrNode *)n->args[1], reg);
		if(jmp == 0)
			jmp = PUSH_OP_A(b, JMPUNLESS, reg+1);
		SETARG_sBx(kv_A(b->code, jmp), kv_size(b->code) - jmp - 1);
	} break;
	case NODE_LOGICOR:{
		int jmp=0 ;
		TrCompiler_compile_node(vm, c, b, (TrNode *)n->args[0], reg);
		jmp = PUSH_OP_A(b, JMPIF, reg);
		TrCompiler_compile_node(vm, c, b, (TrNode *)n->args[1], reg);
		SETARG_sBx(kv_A(b->code, jmp), kv_size(b->code) - jmp - 1);
	} break;
    case NODE_AND:
    case NODE_OR: {
      /* receiver */
      COMPILE_NODE(b, n->args[0], reg);
      TrCompiler_compile_node(vm, c, b, (TrNode *)n->args[0], reg);
      int jmp;
      if (n->ntype == NODE_AND)
        jmp = PUSH_OP_A(b, JMPUNLESS, reg);
      else
        jmp = PUSH_OP_A(b, JMPIF, reg);
      /* arg */
      COMPILE_NODE(b, n->args[1], reg);
      SETARG_sBx(kv_A(b->code, jmp), kv_size(b->code) - jmp - 1);
    } break;
    case NODE_BOOL:
      PUSH_OP_AB(b, BOOL, reg, n->args[0]);
      break;
    case NODE_NIL:
      PUSH_OP_A(b, NIL, reg);
      break;
    case NODE_SELF:
      PUSH_OP_A(b, SELF, reg);
      break;
    case NODE_RETURN:
      if (n->args[0]) COMPILE_NODE(b, n->args[0], reg);
      if (b->parent)
        PUSH_OP_AB(b, THROW, TR_THROW_RETURN, reg);
      else
        PUSH_OP_A(b, RETURN, reg);
      break;
    case NODE_BREAK:
      PUSH_OP_A(b, THROW, TR_THROW_BREAK);
      break;
    case NODE_YIELD: {
      size_t argc = 0;
      if (n->args[0]) {
        argc = TR_ARRAY_SIZE(n->args[0]);
        COMPILE_NODES(b, n->args[0], i, reg, i+1);
        ASSERT_NO_LOCAL_IN(yield);
      }
      PUSH_OP_AB(b, YIELD, reg, argc);
    } break;
    case NODE_DEF: {
      TrNode *method = (TrNode *)n->args[0];
      assert(method->ntype == NODE_METHOD);
      TrBlock *blk = TrBlock_new(c, 0);
      size_t blki = kv_size(b->blocks);
      int blk_reg = 0;
      kv_push(TrBlock *, b->blocks, blk);
      if (n->args[1]) {
        /* add parameters as locals in method context */
        blk->argc = TR_ARRAY_SIZE(n->args[1]);
        TR_ARRAY_EACH(n->args[1], i, v, {
          TrNode *param = (TrNode *)v;
          TrBlock_push_local(blk, param->args[0]);
          if (param->args[1]) blk->arg_splat = 1;
          /* compile default expression and store location
             in defaults table for later jump when executing. */
          if (param->args[2]) {
            COMPILE_NODE(blk, param->args[2], blk_reg);
            kv_push(int, blk->defaults, kv_size(blk->code));
          }
          blk_reg++;
        });
      }
      /* compile body of method */
      COMPILE_NODES(blk, n->args[2], i, blk_reg, 0);
      PUSH_OP_A(blk, RETURN, blk_reg);
      if (method->args[0]) {
        /* metaClass def */
        COMPILE_NODE(b, method->args[0], reg);
        PUSH_OP_ABx(b, METADEF, blki, TrBlock_push_value(b, method->args[1]));
        PUSH_OP_A(b, BOING, reg);
      } else {
        PUSH_OP_ABx(b, DEF, blki, TrBlock_push_value(b, method->args[1]));
      }
    } break;
    case NODE_CLASS:
    case NODE_MODULE: {
      TrBlock *blk = TrBlock_new(c, 0);
      size_t blki = kv_size(b->blocks);
      kv_push(TrBlock *, b->blocks, blk);
      reg = 0;
      /* compile body of Class */
      COMPILE_NODES(blk, n->args[2], i, reg, 0);
      PUSH_OP_A(blk, RETURN, reg);
      if (n->ntype == NODE_CLASS) {
        /* superClass */
        if (n->args[1])
          PUSH_OP_ABx(b, GETCONST, reg, TrBlock_push_value(b, n->args[1]));
        else
          PUSH_OP_A(b, NIL, reg);
        PUSH_OP_ABx(b, CLASS, blki, TrBlock_push_value(b, n->args[0]));
        PUSH_OP_A(b, BOING, reg);
      } else {
        PUSH_OP_ABx(b, MODULE, blki, TrBlock_push_value(b, n->args[0]));
      }
    } break;
    case NODE_CONST:
      PUSH_OP_ABx(b, GETCONST, reg, TrBlock_push_value(b, n->args[0]));
      break;
    case NODE_SETCONST:
      COMPILE_NODE(b, n->args[1], reg);
      PUSH_OP_ABx(b, SETCONST, reg, TrBlock_push_value(b, n->args[0]));
      break;
    case NODE_ADD:
    case NODE_SUB:
    case NODE_LT: {
      int rcv = TrCompiler_compile_node_to_RK(vm, c, b, CNODE(NODE_ARG(n, 0)), reg);
      int arg = TrCompiler_compile_node_to_RK(vm, c, b, CNODE(NODE_ARG(n, 1)), reg+1);
      REG(reg+1);
      switch (n->ntype) {
        case NODE_ADD: PUSH_OP_ABC(b, ADD, reg, rcv, arg); break;
        case NODE_SUB: PUSH_OP_ABC(b, SUB, reg, rcv, arg); break;
        case NODE_LT:  PUSH_OP_ABC(b, LT, reg, rcv, arg); break;
        default: assert(0);
      }
    } break;
	case NODE_INC: 
	case NODE_DEC:{
	  /*int id=0, val=0;
	  COMPILE_NODE(b, n->args[0], id);	 // n->args[0]
	  val = TrCompiler_compile_node_to_RK(vm, c, b, CNODE(NODE_ARG(n, 1)), reg+1); // n->args[1]; */
	  OBJ name = n->args[0];
	  COMPILE_NODE(b, n->args[1], reg);
	  int i = TrBlock_push_local(b, name);
	  int val = TrCompiler_compile_node_to_RK(vm, c, b, CNODE(NODE_ARG(n, 1)), reg);
	  switch(n->ntype) {
	    case NODE_INC: PUSH_OP_ABC(b, ADD, reg, i, val); break;
	    case NODE_DEC: PUSH_OP_ABC(b, SUB, reg, i, val); break;
	    default: assert(0);
	  }
	  PUSH_OP_AB(b, MOVE, i, reg);
	} break;
    case NODE_NEG:
    case NODE_NOT: {
      int rcv = TrCompiler_compile_node_to_RK(vm, c, b, CNODE(NODE_ARG(n, 0)), reg);
      switch (n->ntype) {
        case NODE_NEG: PUSH_OP_AB(b, NEG, reg, rcv); break;
        case NODE_NOT: PUSH_OP_AB(b, NOT, reg, rcv); break;
        default: assert(0);
      }
    } break;
    default:
      printf("Compiler: unknown node type: %d in %s:%lu\n", n->ntype, TR_STR_PTR(b->filename),(long unsigned int)b->line);
      if (vm->debug) assert(0);
  }
  return TR_NIL;
}

void TrCompiler_compile(TrCompiler *c) {
  TrBlock *b = c->block;
  b->filename = c->filename;
  TrCompiler_compile_node(c->vm, c, b, (TrNode *)c->node, 0);
  PUSH_OP_A(b, RETURN, 0);
}
