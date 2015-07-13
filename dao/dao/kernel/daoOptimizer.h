/*
// Dao Virtual Machine
// http://www.daovm.net
//
// Copyright (c) 2006-2015, Limin Fu
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without modification,
// are permitted provided that the following conditions are met:
//
// * Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimer.
// * Redistributions in binary form must reproduce the above copyright notice,
//   this list of conditions and the following disclaimer in the documentation
//   and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED  BY THE COPYRIGHT HOLDERS AND  CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED  WARRANTIES,  INCLUDING,  BUT NOT LIMITED TO,  THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
// IN NO EVENT SHALL  THE COPYRIGHT HOLDER OR CONTRIBUTORS  BE LIABLE FOR ANY DIRECT,
// INDIRECT,  INCIDENTAL, SPECIAL,  EXEMPLARY,  OR CONSEQUENTIAL  DAMAGES (INCLUDING,
// BUT NOT LIMITED TO,  PROCUREMENT OF  SUBSTITUTE  GOODS OR  SERVICES;  LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION)  HOWEVER CAUSED  AND ON ANY THEORY OF
// LIABILITY,  WHETHER IN CONTRACT,  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
// OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
// OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef __DAO_OPTIMIZER_H__
#define __DAO_OPTIMIZER_H__

#include"daoBase.h"


enum DaoCnodeOperandType
{
	DAO_OP_NONE,
	DAO_OP_SINGLE,
	DAO_OP_PAIR,
	DAO_OP_TRIPLE,
	DAO_OP_RANGE,
	DAO_OP_RANGE2
};


/* Code Node */
struct DaoCnode
{
	uchar_t   type;       /* use type of operands; */
	uchar_t   reachable;  /* reachable status; */
	ushort_t  index;      /* index of the node; */
	ushort_t  first;      /* the only (SINGLE) or the first (PAIR/RANGE) used variable; */
	ushort_t  second;     /* the second (PAIR) or the one-past-last (RANGE) used variable; */
	ushort_t  third;      /* the third (TRIPLE) used variable; */
	ushort_t  lvalue;     /* variable defined by the instruction; 0xffff for none; */
	ushort_t  lvalue2;    /* C operand for SETF, SETI, SETDI, SETMI instructions; */
	ushort_t  exprid;     /* expression id; 0xffff for none; */
	int       initvar;    /* variable id; -1 for none; */

	DList   *ins;   /* in nodes in the flow graph; */
	DList   *outs;  /* out nodes in the flow graph; */
	DList   *kills; /* expressions that are killed by this one; */

	DList   *defs; /* definitions for this use node; */
	DList   *uses; /* uses for this definition node; */

	DList   *list; /* sorted list for the analysis results; */
};

DAO_DLL void DaoCnode_InitOperands( DaoCnode *self, DaoVmCode *code );
DAO_DLL int  DaoCnode_FindResult( DaoCnode *self, void *key );

typedef void (*AnalysisInit)( DaoOptimizer*, DaoCnode* );
typedef int (*AnalysisUpdate)( DaoOptimizer*, DaoCnode*, DaoCnode* );

struct DaoOptimizer
{
	DaoRoutine *routine;

	int reverseFlow;

	AnalysisInit    init;
	AnalysisUpdate  update;

	DList  *nodes;  /* all nodes (labels); */
	DList  *enodes; /* expression nodes (labels); */
	DList  *uses;   /* nodes that use a variable; */
	DList  *refers; /* variables: 0, non-reference; 1, reference; */

	DMap    *exprs;   /* all expressions; */
	DMap    *inits;   /* init nodes; */
	DMap    *finals;  /* final nodes; */

	DMap    *tmp;
	DList  *array;
	DList  *array2;
	DList  *array3;
	DList  *nodeCache;
	DList  *arrayCache;
};

DAO_DLL DaoOptimizer* DaoOptimizer_New();
DAO_DLL void DaoOptimizer_Clear( DaoOptimizer *self );
DAO_DLL void DaoOptimizer_Delete( DaoOptimizer *self );

DAO_DLL void DaoOptimizer_DoLVA( DaoOptimizer *self, DaoRoutine *routine );
DAO_DLL void DaoOptimizer_DoRDA( DaoOptimizer *self, DaoRoutine *routine );
DAO_DLL void DaoOptimizer_DoVIA( DaoOptimizer *self, DaoRoutine *routine );

/*
// Link Definition-Use and Use-Definition:
// The results are stored in each node:
// node->ins:  node is the use, node->ins are the defintions;
// node->outs: node is the defintion, node->outs are the uses;
*/
DAO_DLL void DaoOptimizer_LinkDU( DaoOptimizer *self, DaoRoutine *routine );

void DaoOptimizer_RemoveUnreachableCodes( DaoOptimizer *self, DaoRoutine *routine );
void DaoOptimizer_Optimize( DaoOptimizer *self, DaoRoutine *routine );
void DaoRoutine_UpdateRegister( DaoRoutine *self, DList *mapping );

#endif
