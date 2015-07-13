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

#include<string.h>
#include<ctype.h>
#include<stdlib.h>
#include<stdio.h>
#include<assert.h>

#include"daoGC.h"
#include"daoTasklet.h"
#include"daoLexer.h"
#include"daoValue.h"
#include"daoRoutine.h"
#include"daoNamespace.h"
#include"daoVmspace.h"
#include"daoOptimizer.h"



static DaoCnode* DaoCnode_New()
{
	DaoCnode *self = (DaoCnode*) dao_calloc( 1, sizeof(DaoCnode) );
	self->ins = DList_New(0);
	self->outs = DList_New(0);
	self->kills = DList_New(0);
	self->defs = DList_New(0);
	self->uses = DList_New(0);
	self->list = DList_New(0);
	return self;
}
static void DaoCnode_Delete( DaoCnode *self )
{
	DList_Delete( self->ins );
	DList_Delete( self->outs );
	DList_Delete( self->kills );
	DList_Delete( self->defs );
	DList_Delete( self->uses );
	DList_Delete( self->list );
	dao_free( self );
}
static void DaoCnode_Clear( DaoCnode *self )
{
	self->ins->size = self->outs->size = 0;
	self->list->size = 0;
	self->kills->size = 0;
}
void DaoCnode_InitOperands( DaoCnode *self, DaoVmCode *vmc )
{
	uchar_t type = DaoVmCode_GetOpcodeType( vmc );
	int k, m;

	self->type = DAO_OP_NONE;
	self->first = self->second = self->third = 0xffff;
	self->lvalue = self->lvalue2 = 0xffff;
	self->exprid = 0xffff;
	switch( type ){
	case DAO_CODE_NOP :
		break;
	case DAO_CODE_GETC :
	case DAO_CODE_GETG :
		self->lvalue = vmc->c;
		break;
	case DAO_CODE_SETG :
		self->type = DAO_OP_SINGLE;
		self->first = vmc->a;
		break;
	case DAO_CODE_GETF :
	case DAO_CODE_MOVE :
	case DAO_CODE_UNARY :
		self->type = DAO_OP_SINGLE;
		self->first = vmc->a;
		self->lvalue = vmc->c;
		break;
	case DAO_CODE_SETU :
		if( vmc->c == 0 ){
			self->type = DAO_OP_SINGLE;
			self->first = vmc->a;
		}else{
			self->type = DAO_OP_PAIR;
			self->first = vmc->a;
			self->second = vmc->b;
			self->lvalue2 = vmc->b;
		}
		break;
	case DAO_CODE_SETF :
		self->type = DAO_OP_PAIR;
		self->first = vmc->a;
		self->second = vmc->c;
		self->lvalue2 = vmc->c;
		break;
	case DAO_CODE_SETI :
		self->type = DAO_OP_TRIPLE;
		self->first = vmc->a;
		self->second = vmc->b;
		self->third = vmc->c;
		self->lvalue2 = vmc->c;
		break;
	case DAO_CODE_GETI :
	case DAO_CODE_BINARY :
		self->type = DAO_OP_PAIR;
		self->first = vmc->a;
		self->second = vmc->b;
		self->lvalue = vmc->c;
		break;
	case DAO_CODE_GETU :
		if( vmc->a != 0 ){
			self->type = DAO_OP_SINGLE;
			self->first = vmc->b;
		}
		self->lvalue = vmc->c;
		break;
	case DAO_CODE_UNARY2 :
		self->type = DAO_OP_SINGLE;
		self->first = vmc->b;
		self->lvalue = vmc->c;
		break;
	case DAO_CODE_ENUM :
		self->type = DAO_OP_RANGE;
		self->first = vmc->a;
		self->second = vmc->a + (vmc->b & (0xffff>>2));
		self->lvalue = vmc->c;
		break;
	case DAO_CODE_GETM :
	case DAO_CODE_ENUM2 :
	case DAO_CODE_ROUTINE :
		self->type = DAO_OP_RANGE;
		self->first = vmc->a;
		self->second = vmc->a + vmc->b + 1;
		self->lvalue = vmc->c;
		break;
	case DAO_CODE_SETM:
		self->type = DAO_OP_RANGE2;
		self->first = vmc->c;
		self->second = vmc->c + vmc->b + 1;
		self->third = vmc->a;
		self->lvalue2 = vmc->c;
		break;
	case DAO_CODE_CALL :
		k = vmc->b & 0xff;
		self->type = DAO_OP_RANGE;
		self->first = vmc->a;
		self->second = vmc->a + k + 1;
		self->lvalue = vmc->c;
		break;
	case DAO_CODE_MATRIX :
		k = vmc->b & 0xff;
		m = vmc->b >> 8;
		self->type = DAO_OP_RANGE;
		self->first = vmc->a;
		self->second = vmc->a + k*m;
		self->lvalue = vmc->c;
		break;
	case DAO_CODE_BRANCH :
		self->type = DAO_OP_SINGLE;
		self->first = vmc->a;
		break;
	case DAO_CODE_EXPLIST :
		self->type = DAO_OP_RANGE;
		self->first = vmc->a;
		self->second = vmc->a + vmc->b;
		break;
	case DAO_CODE_YIELD :
		self->type = DAO_OP_RANGE;
		self->first = vmc->a;
		self->second = vmc->a + (vmc->b & 0xff);
		self->lvalue = vmc->c;
		break;
	default: break;
	}
	/* Handle zero number of variables in the range: */
	if( self->type == DAO_OP_RANGE && self->second == 0xffff ) self->type = DAO_OP_NONE;
}
int DaoCnode_FindResult( DaoCnode *self, void *key )
{
	int first = 0, last = self->list->size - 1;
	while( first <= last ){
		int mid = (first + last) / 2;
		void *val = self->list->items.pVoid[mid];
		if( val == key ){
			return mid;
		}else if( val < key ){
			first = mid + 1;
		}else{
			last = mid - 1;
		}
	}
	return -1;
}


DaoOptimizer* DaoOptimizer_New()
{
	DaoOptimizer *self = (DaoOptimizer*) dao_malloc( sizeof(DaoOptimizer) );
	self->routine = NULL;
	self->array = DList_New(0); /* DList<daoint> */
	self->array2 = DList_New(0); /* DList<daoint|DaoCnode*> */
	self->array3 = DList_New(0); /* DList<daoint|DaoCnode*> */
	self->nodeCache  = DList_New(0); /* DList<DaoCnode*> */
	self->arrayCache = DList_New( DAO_DATA_LIST ); /* DList<DList<DaoCnode*>> */
	self->nodes = DList_New(0);  /* DList<DaoCnode*> */
	self->enodes = DList_New(0);  /* DList<DaoCnode*> */
	self->uses  = DList_New(0);  /* DList<DList<DaoCnode*>> */
	self->refers  = DList_New(0);  /* DList<daoint> */
	self->exprs = DHash_New( DAO_DATA_VMCODE, 0 ); /* DMap<DaoVmCode*,int> */
	self->inits = DHash_New(0,0);   /* DMap<DaoCnode*,int> */
	self->finals = DHash_New(0,0);  /* DMap<DaoCnode*,int> */
	self->tmp = DHash_New(0,0);
	self->reverseFlow = 0;
	self->update = NULL;
	return self;
}
void DaoOptimizer_Clear( DaoOptimizer *self )
{
	self->nodes->size = 0;
	self->enodes->size = 0;
	self->uses->size = 0;
	DMap_Reset( self->inits );
	DMap_Reset( self->finals );
	DMap_Reset( self->exprs );
}
void DaoOptimizer_Delete( DaoOptimizer *self )
{
	daoint i;
	for(i=0; i<self->nodeCache->size; i++) DaoCnode_Delete( self->nodeCache->items.pCnode[i] );
	DList_Delete( self->array );
	DList_Delete( self->array2 );
	DList_Delete( self->array3 );
	DList_Delete( self->nodeCache );
	DList_Delete( self->arrayCache );
	DList_Delete( self->enodes );
	DList_Delete( self->nodes );
	DList_Delete( self->uses );
	DList_Delete( self->refers );
	DMap_Delete( self->inits );
	DMap_Delete( self->finals );
	DMap_Delete( self->exprs );
	DMap_Delete( self->tmp );
	dao_free( self );
}

void DaoRoutine_FormatCode( DaoRoutine *self, int i, DaoVmCodeX vmc, DString *output );
static int DaoOptimizer_UpdateRDA( DaoOptimizer *self, DaoCnode *first, DaoCnode *second );
static int DaoOptimizer_UpdateAEA( DaoOptimizer *self, DaoCnode *first, DaoCnode *second );

static void DaoOptimizer_Print( DaoOptimizer *self )
{
	DaoRoutine *routine = self->routine;
	DaoStream *stream = routine->nameSpace->vmSpace->stdioStream;
	DaoVmCodeX **vmCodes = routine->body->annotCodes->items.pVmc;
	DString *annot = DString_New();
	daoint i, j, k, n;

	DaoStream_WriteChars( stream, "============================================================\n" );
	DaoStream_WriteChars( stream, daoRoutineCodeHeader );
	for( j=0,n=routine->body->annotCodes->size; j<n; j++){
		DaoCnode *node = self->nodes->items.pCnode[j];
		DaoRoutine_FormatCode( routine, j, *vmCodes[j], annot );
		DString_Chop( annot, 0 );
		while( annot->size < 80 ) DString_AppendChar( annot, ' ' );
		DString_AppendChars( annot, "| " );
		DaoStream_WriteString( stream, annot );
		if( self->update == DaoOptimizer_UpdateAEA ){
			k = -1;
			if( node->exprid != 0xffff ) k = self->enodes->items.pCnode[node->exprid]->index;
			DaoStream_WriteInt( stream, k );
			DaoStream_WriteChars( stream, " | " );
			for(i=0; i<node->list->size; ++i){
				DaoStream_WriteInt( stream, node->list->items.pCnode[i]->index );
				DaoStream_WriteChars( stream, ", " );
			}
		}else{
			DaoStream_WriteInt( stream, j );
			DaoStream_WriteChars( stream, " | " );
			for(i=0; i<node->list->size; ++i){
				DaoStream_WriteInt( stream, node->list->items.pInt[i] );
				DaoStream_WriteChars( stream, ", " );
			}
		}
		DaoStream_WriteChars( stream, "\n" );
	}
	DString_Delete( annot );
}



static int DaoAEA_Compare( DaoCnode *node1, DaoCnode *node2, DaoVmCode *codes )
{
	DaoVmCode c1 = codes[node1->index];
	DaoVmCode c2 = codes[node2->index];
	if( c1.code != c2.code ) return c1.code - c2.code;
	if( c1.a != c2.a ) return c1.a - c2.a;
	if( c1.b != c2.b ) return c1.b - c2.b;
	return node1->index - node2->index;
}
static int DaoAEA_Compare2( DaoCnode *node1, DaoCnode *node2, DaoVmCode *codes )
{
	DaoVmCode c1 = codes[node1->index];
	DaoVmCode c2 = codes[node2->index];
	if( c1.code != c2.code ) return c1.code - c2.code;
	if( c1.a != c2.a ) return c1.a - c2.a;
	return c1.b - c2.b;
}
static void DaoAEA_Sort( DaoCnode **nodes, int first, int last, DaoVmCode *codes )
{
	int lower=first+1, upper=last;
	DaoCnode *val;
	DaoCnode *pivot;
	if( first >= last ) return;
	val = nodes[first];
	nodes[first] = nodes[ (first+last)/2 ];
	nodes[ (first+last)/2 ] = val;
	pivot = nodes[ first ];

	while( lower <= upper ){
		while( lower < last && DaoAEA_Compare( nodes[lower], pivot, codes ) < 0 ) lower ++;
		while( upper > first && DaoAEA_Compare( pivot, nodes[upper], codes ) < 0 ) upper --;
		if( lower < upper ){
			val = nodes[lower];
			nodes[lower] = nodes[upper];
			nodes[upper] = val;
			upper --;
		}
		lower ++;
	}
	val = nodes[first];
	nodes[first] = nodes[upper];
	nodes[upper] = val;
	if( first+1 < upper ) DaoAEA_Sort( nodes, first, upper-1, codes );
	if( upper+1 < last  ) DaoAEA_Sort( nodes, upper+1, last, codes );
}
static void DaoOptimizer_InitNodeAEA( DaoOptimizer *self, DaoCnode *node )
{
	node->list->size = 0;
	if( DMap_Find( self->inits, node ) == 0 ){
		DaoVmCode *codes = self->routine->body->vmCodes->data.codes;
		daoint i;
		for(i=0; i<node->index; ++i){
			DaoCnode *node2 = self->nodes->items.pCnode[i];
			if( node2->exprid != 0xffff ) DList_Append( node->list, node2 );
		}
		DaoAEA_Sort( node->list->items.pCnode, 0, node->list->size - 1, codes );
	}
}
/* Transfer function for Available Expression Analysis: */
static void DaoOptimizer_AEA( DaoOptimizer *self, DaoCnode *node, DList *out )
{
	DaoVmCode *codes = self->routine->body->vmCodes->data.codes;
	DaoCnode *genAE = node->exprid == 0xffff ? NULL : node;
	daoint i, j, pushed = 0;

	switch( node->type ){
	case DAO_OP_SINGLE :
		if( node->lvalue == node->first ) genAE = NULL;
		break;
	case DAO_OP_PAIR :
		if( node->lvalue == node->first ) genAE = NULL;
		if( node->lvalue == node->second ) genAE = NULL;
		break;
	case DAO_OP_RANGE :
		if( node->first <= node->lvalue && node->lvalue < node->second ) genAE = NULL;
		break;
	case DAO_OP_TRIPLE :
	case DAO_OP_RANGE2 :
		genAE = NULL;
		break;
	}

#if 0
	DMap_Assign( out, node->set );
	for(i=0; i<node->kills->size; i++) DMap_Erase( out, node->kills->items.pVoid[i] );
	if( genAE != NULL ) DMap_Insert( out, genAE, NULL );
#endif

	out->size = 0;
	for(i=0,j=0; i<node->list->size; ){
		DaoCnode *node1 = node->list->items.pCnode[i];
		/* Skip the node if it is in the kills: */
		if( j < node->kills->size ){
			DaoCnode *node2 = node->kills->items.pCnode[j];
			if( node1->index == node2->index ){
				i += 1;
				j += 1;
				continue;
			}else if( DaoAEA_Compare( node1, node2, codes ) > 0 ){
				j += 1;
				continue;
			}
		}
		i += 1;
		if( genAE != NULL ){
			//if( DaoAEA_Compare2( node1, genAE, codes ) == 0 ) continue;
			if( node1 == genAE ) pushed = 1;
			if( pushed == 0 && DaoAEA_Compare( genAE, node1, codes ) < 0 ){
				DList_Append( out, genAE );
				pushed = 1;
			}
		}
		DList_Append( out, node1 );
	}
	if( genAE != NULL && pushed == 0 ) DList_Append( out, genAE );
}
static int DaoOptimizer_UpdateAEA( DaoOptimizer *self, DaoCnode *first, DaoCnode *second )
{
	DaoCnode **cnodes1, **cnodes2;
	DaoVmCode *codes = self->routine->body->vmCodes->data.codes;
	daoint i, j, k, m, size1, size2, changes = 0;

	DaoOptimizer_AEA( self, first, self->array3 );

	/* Intersection: */
	DList_Assign( self->array2, second->list );
	cnodes1 = self->array2->items.pCnode;
	cnodes2 = self->array3->items.pCnode;
	size1 = self->array2->size;
	size2 = self->array3->size;
	second->list->size = 0;
	for(i=0, j=0; i<size1 && j<size2; ){
		DaoCnode *node1 = cnodes1[i];
		DaoCnode *node2 = cnodes2[j];
		int cmp = DaoAEA_Compare2( node1, node2, codes );
		if( cmp == 0 ){
			/*
			// Find all the equivalent expressions;
			// And push them only once to second->list;
			*/
			for(k=i+1;  k<size1 && DaoAEA_Compare2( node1, cnodes1[k], codes ) ==0; ) k += 1;
			for(m=j+1;  m<size2 && DaoAEA_Compare2( node2, cnodes2[m], codes ) ==0; ) m += 1;
			while( i < k || j < m ){
				if( i < k && j < m ){
					DaoCnode *node1 = cnodes1[i];
					DaoCnode *node2 = cnodes2[j];
					if( node1->index == node2->index ){
						DList_Append( second->list, node1 );
						i += 1;
						j += 1;
					}else if( node1->index < node2->index ){
						DList_Append( second->list, node1 );
						i += 1;
					}else{
						DList_Append( second->list, node2 );
						changes += 1;
						j += 1;
					}
				}else if( i < k ){
					DList_Append( second->list, cnodes1[i] );
					i += 1;
				}else{
					DList_Append( second->list, cnodes2[j] );
					changes += 1;
					j += 1;
				}
			}
		}else if( cmp < 0 ){
			changes += 1;
			i += 1;
		}else{
			j += 1;
		}
	}
	return changes;
}


static void Dao_SortInts( daoint *values, int first, int last )
{
	int lower=first+1, upper=last;
	daoint val;
	daoint pivot;
	if( first >= last ) return;
	val = values[first];
	values[first] = values[ (first+last)/2 ];
	values[ (first+last)/2 ] = val;
	pivot = values[ first ];

	while( lower <= upper ){
		while( lower < last  && values[lower] < pivot ) lower ++;
		while( upper > first && pivot < values[upper] ) upper --;
		if( lower < upper ){
			val = values[lower];
			values[lower] = values[upper];
			values[upper] = val;
			upper --;
		}
		lower ++;
	}
	val = values[first];
	values[first] = values[upper];
	values[upper] = val;
	if( first+1 < upper ) Dao_SortInts( values, first, upper-1 );
	if( upper+1 < last  ) Dao_SortInts( values, upper+1, last );
}
static int Dao_IntsUnion( DList *first, DList *second, DList *output, daoint excluding )
{
	daoint size1 = first->size;
	daoint size2 = second->size;
	daoint i, j, changes = 0;
	output->size = 0;
	for(i=0, j=0; i<size1 || j<size2; ){
		if( i < size1 && first->items.pInt[i] == excluding ){
			i += 1;
			continue;
		}
		if( i < size1 && j < size2 ){
			daoint id1 = first->items.pInt[i];
			daoint id2 = second->items.pInt[j];
			if( id1 == id2 ){
				DList_Append( output, IntToPointer(id1) );
				i += 1;
				j += 1;
			}else if( id1 < id2 ){
				DList_Append( output, IntToPointer(id1) );
				i += 1;
			}else{
				DList_Append( output, IntToPointer(id2) );
				changes += 1;
				j += 1;
			}
		}else if( i < size1 ){
			DList_Append( output, first->items.pVoid[i] );
			i += 1;
		}else{
			DList_Append( output, second->items.pVoid[j] );
			changes += 1;
			j += 1;
		}
	}
	return changes;
}
static int Dao_IntsIntersection( DList *first, DList *second, DList *output, daoint including )
{
	daoint size1 = first->size;
	daoint size2 = second->size;
	daoint i, j, changes = 0;
	int included = 0;

	output->size = 0;
	for(i=0, j=0; i<size1 && j<size2; ){
		daoint id1 = first->items.pInt[i];
		daoint id2 = second->items.pInt[j];
		if( id1 == id2 ){
			if( including >= 0 && id1 > including ){
				DList_Append( output, IntToPointer(including) );
				including = -1; /* To avoid duplication; */
			}
			if( id1 == including ) including = -1; /* To avoid duplication; */
			DList_Append( output, IntToPointer(id1) );
			i += 1;
			j += 1;
		}else if( id1 < id2 ){
			changes += 1;
			i += 1;
		}else{
			j += 1;
		}
	}
	if( including >= 0 ) DList_Append( output, IntToPointer(including) );
	return changes;
}

#define RDA_OFFSET  0xffff

/*
// For Reaching Definition Analysis:
// (x,?): is represented by the variable index of "x";
// (x,lab): is represented by the index of "lab" + 0xffff;
*/
static void DaoOptimizer_InitNodeRDA( DaoOptimizer *self, DaoCnode *node )
{
	int i;
	node->list->size = 0;
	if( DMap_Find( self->inits, node ) ){
		for(i=0; i<self->routine->body->regCount; i++)
			DList_Append( node->list, IntToPointer(i) );
	}
}
/* Transfer function for Reaching Definition Analysis: */
static void DaoOptimizer_RDA( DaoOptimizer *self, DaoCnode *node, DList *out )
{
	DList *kills;
	daoint i, j, pushed = 0;

	if( node->lvalue == 0xffff ){
		DList_Assign( out, node->list );
		return;
	}
	out->size = 0;
	kills = self->uses->items.pList[node->lvalue];
	for(i=0,j=0; i<node->list->size; ){
		daoint id = node->list->items.pInt[i];
		if( id == node->lvalue ){
			i += 1;
			continue;
		}else if( j < kills->size ){
			/* Skip if it is in the kills: */
			daoint id2 = kills->items.pInt[j];
			if( id == id2 ){
				i += 1;
				j += 1;
				continue;
			}else if( id > id2 ){
				j += 1;
				continue;
			}
		}
		i += 1;
		if( (node->index + RDA_OFFSET) == id ) pushed = 1;
		if( pushed == 0 && (node->index + RDA_OFFSET) < id ){
			DList_Append( out, IntToPointer( node->index + RDA_OFFSET ) );
			pushed = 1;
		}
		DList_Append( out, IntToPointer( id ) );
	}
	if( pushed == 0 ) DList_Append( out, IntToPointer( node->index + RDA_OFFSET ) );
}
static int DaoOptimizer_UpdateRDA( DaoOptimizer *self, DaoCnode *first, DaoCnode *second )
{
	DaoOptimizer_RDA( self, first, self->array3 );
	/* Union: */
	DList_Assign( self->array2, second->list );
	return Dao_IntsUnion( self->array2, self->array3, second->list, -1 );
}


static void DaoCnode_GetOperands( DaoCnode *self, DList *operands )
{
	int i;
	operands->size = 0;
	switch( self->type ){
	case DAO_OP_SINGLE :
		DList_Append( operands, IntToPointer(self->first) );
		break;
	case DAO_OP_PAIR   :
		DList_Append( operands, IntToPointer(self->first) );
		DList_Append( operands, IntToPointer(self->second) );
		break;
	case DAO_OP_TRIPLE :
		DList_Append( operands, IntToPointer(self->first) );
		DList_Append( operands, IntToPointer(self->second) );
		DList_Append( operands, IntToPointer(self->third) );
		break;
	case DAO_OP_RANGE :
	case DAO_OP_RANGE2 :
		for(i=self->first; i<self->second; i++) DList_Append( operands, IntToPointer(i) );
		if( self->type == DAO_OP_RANGE2 ) DList_Append( operands, IntToPointer(self->third) );
		break;
	}
	Dao_SortInts( operands->items.pInt, 0, operands->size-1 );
}
static void DaoOptimizer_InitNodeLVA( DaoOptimizer *self, DaoCnode *node )
{
	node->list->size = 0;
	if( DMap_Find( self->finals, node ) ) DaoCnode_GetOperands( node, node->list );
}
/* Transfer function for Live Variable Analysis: */
static void DaoOptimizer_LVA( DaoOptimizer *self, DaoCnode *node, DList *out )
{
	DaoCnode_GetOperands( node, self->array2 );
	Dao_IntsUnion( node->list, self->array2, out, node->lvalue );
}
static int DaoOptimizer_UpdateLVA( DaoOptimizer *self, DaoCnode *first, DaoCnode *second )
{
	DaoOptimizer_LVA( self, first, self->array3 );

	/* Union: */
	DList_Assign( self->array2, second->list );
	return Dao_IntsUnion( self->array2, self->array3, second->list, -1 );
}
static void DaoOptimizer_GetInitVariables( DaoOptimizer *self, DaoCnode *node, DList *out )
{
	out->size = 0;
	if( node->initvar >= 0 ){
		DList_Append( out, IntToPointer(node->initvar) );
	}else{
		DaoVmCodeX *vmc = self->routine->body->annotCodes->items.pVmc[node->index];
		if( vmc->code == DVM_SECT ){
			int i;
			for(i=0; i<vmc->b; ++i){
				DList_Append( out, IntToPointer(vmc->a + i) );
			}
		}
	}
}
/* Variable initialization analysis: */
static void DaoOptimizer_InitNodeVIA( DaoOptimizer *self, DaoCnode *node )
{
	int i;
	DaoOptimizer_GetInitVariables( self, node, self->array2 );
	if( node->index ){
		DaoCnode *prev = self->nodes->items.pCnode[node->index-1];
		Dao_IntsUnion( prev->list, self->array2, node->list, -1 );
	}else{
		DList *partypes = self->routine->routType->nested;
		self->array3->size = 0;
		for(i=0; i<self->routine->parCount; ++i){
			DList_Append( self->array3, IntToPointer(i) );
		}
		if( (self->routine->attribs & DAO_ROUT_DECORATOR) && partypes->size ){
			DList_Append( self->array3, IntToPointer(self->routine->parCount) );
		}
		Dao_IntsUnion( self->array3, self->array2, node->list, -1 );
	}
}
static int DaoOptimizer_UpdateVIA( DaoOptimizer *self, DaoCnode *first, DaoCnode *second )
{
	int changes;

	DList_Assign( self->array2, second->list );
	changes = Dao_IntsIntersection( first->list, self->array2, self->array3, -1 );

	DaoOptimizer_GetInitVariables( self, second, self->array2 );
	Dao_IntsUnion( self->array2, self->array3, second->list, -1 );
	return changes;
}
static void DaoOptimizer_ProcessWorklist( DaoOptimizer *self, DList *worklist )
{
	daoint i;
	/* printf( "DaoOptimizer_ProcessWorklist: %i\n", (int)worklist->size ); */
	while( worklist->size ){
		DaoCnode *second = (DaoCnode*) DList_PopBack( worklist );
		DaoCnode *first = (DaoCnode*) DList_PopBack( worklist );
		/* if( (++m) % 10000 ==0 ) printf( "%9i  %9i\n", (int)m, (int)worklist->size ); */
		if( self->update( self, first, second ) == 0 ) continue;
		if( self->reverseFlow ){
			for(i=0; i<second->ins->size; i++){
				DList_PushBack( worklist, second );
				DList_PushBack( worklist, second->ins->items.pVoid[i] );
			}
		}else{
			for(i=0; i<second->outs->size; i++){
				DList_PushBack( worklist, second );
				DList_PushBack( worklist, second->outs->items.pVoid[i] );
			}
		}
	}
}
static void DaoOptimizer_SolveFlowEquation( DaoOptimizer *self )
{
	DList *worklist = DList_New(0);
	DaoCnode *node, **nodes = self->nodes->items.pCnode;
	daoint i, j, N = self->nodes->size;
	for(i=0; i<N; ++i){
		node = nodes[i];
		/*
		// Always initialize in forward order:
		// Variable Initialization Analysis (VIA) relies on it;
		// Other analyses do not;
		*/
		self->init( self, node );
		if( self->reverseFlow ){
			for(j=0; j<node->outs->size; j++){
				DList_PushBack( worklist, node->outs->items.pVoid[j] );
				DList_PushBack( worklist, node );
			}
		}else{
			for(j=0; j<node->outs->size; j++){
				DList_PushFront( worklist, node->outs->items.pVoid[j] );
				DList_PushFront( worklist, node );
			}
		}
	}
	DaoOptimizer_ProcessWorklist( self, worklist );
	DList_Delete( worklist );
}

static void DaoOptimizer_InitNode( DaoOptimizer *self, DaoCnode *node, DaoVmCode *code );


/* Optimization should be done on routine->body->annotCodes,
// which should not contain opcodes such as DEBUG and JITC. */
static void DaoOptimizer_Init( DaoOptimizer *self, DaoRoutine *routine )
{
	DaoCnode *node, **nodes;
	DaoVmCode *vmc, **codes = (DaoVmCode**)routine->body->annotCodes->items.pVmc;
	daoint N = routine->body->annotCodes->size;
	daoint M = routine->body->regCount;
	daoint i, j, k;

	self->routine = routine;
	DaoOptimizer_Clear( self );
	DList_Resize( self->refers, M, 0 );
	if( self->nodeCache->size < N ) DList_Resize( self->nodeCache, N, NULL );
	if( self->arrayCache->size < M ){
		DList *array = DList_New(0);
		DList_Resize( self->arrayCache, M, array );
		DList_Delete( array );
	}
	for(i=0; i<M; i++){
		DList *array = self->arrayCache->items.pList[i];
		DList_Append( self->uses, array );
		self->refers->items.pInt[i] = 0;
		array->size = 0;
	}
	for(i=0; i<N; i++){
		node = self->nodeCache->items.pCnode[i];
		if( node == NULL ){
			node = DaoCnode_New();
			self->nodeCache->items.pCnode[i] = node;
		}
		node->index = i;
		DaoCnode_Clear( node );
		DaoOptimizer_InitNode( self, node, codes[i] );
		if( node->lvalue != 0xffff ){
			self->refers->items.pInt[node->lvalue] = DaoVmCode_MayCreateReference( codes[i]->code );
		}
		DList_Append( self->nodes, node );
	}
#if 0
	printf( "number of nodes: %i, number of registers: %i\n", self->nodes->size, M );
	printf( "number of interesting expression: %i\n", self->exprs->size );
#endif
	nodes = self->nodes->items.pCnode;
	DMap_Insert( self->inits, nodes[0], NULL );
	for(i=0; i<N; i++){
		vmc = codes[i];
		node = nodes[i];
		if( i && vmc->code != DVM_CASE ){
			k = codes[i-1]->code;
			if( vmc->code == DVM_SECT || (vmc->code == DVM_GOTO && vmc->c == DVM_SECT) ){
				/* Code section is isolated from the main codes: */
				DList_Append( nodes[i-1]->outs, node );
				DList_Append( node->ins, nodes[i-1] );
			}else if( k != DVM_GOTO && k != DVM_CASE && k != DVM_RETURN ){
				DList_Append( nodes[i-1]->outs, node );
				DList_Append( node->ins, nodes[i-1] );
			}
		}
		switch( vmc->code ){
		case DVM_GOTO : case DVM_CASE :
		case DVM_TEST : case DVM_TEST_B : case DVM_TEST_I : case DVM_TEST_F :
			DList_Append( node->outs, nodes[vmc->b] );
			DList_Append( nodes[vmc->b]->ins, node );
			break;
		case DVM_SWITCH :
			DList_Append( node->outs, nodes[vmc->b] );
			DList_Append( nodes[vmc->b]->ins, node );
			for(j=1; j<=vmc->c; j++){
				DList_Append( node->outs, nodes[i+j] );
				DList_Append( nodes[i+j]->ins, node );
			}
			break;
		case DVM_SECT :
			/*
			// Expressions outside should NOT be available inside code sections.
			// Otherwise, due to Common Sub-expression Elimination, values from
			// outside maybe access by instructions other than GETVH and SETVH.
			// This will break the basic assumption that code sections always
			// accesss outside values through GETVH and SETVH, as a result some
			// code section methods may not work, for example, mt.start::{} in
			// parallel quicksort.
			*/
			DMap_Insert( self->inits, node, NULL );
			break;
		case DVM_RETURN : DMap_Insert( self->finals, node, NULL ); break;
		default : break;
		}
	}
}
static void DaoOptimizer_InitKills( DaoOptimizer *self );
static void DaoOptimizer_InitAEA( DaoOptimizer *self, DaoRoutine *routine )
{
	self->reverseFlow = 0;
	self->init = DaoOptimizer_InitNodeAEA;
	self->update = DaoOptimizer_UpdateAEA;
	DaoOptimizer_Init( self, routine );
	DaoOptimizer_InitKills( self );
}
static void DaoOptimizer_InitNodesRDA( DaoOptimizer *self );
static void DaoOptimizer_InitRDA( DaoOptimizer *self, DaoRoutine *routine )
{
	self->reverseFlow = 0;
	self->init = DaoOptimizer_InitNodeRDA;
	self->update = DaoOptimizer_UpdateRDA;
	DaoOptimizer_Init( self, routine );
	DaoOptimizer_InitNodesRDA( self );
}
static void DaoOptimizer_InitLVA( DaoOptimizer *self, DaoRoutine *routine )
{
	self->reverseFlow = 1;
	self->init = DaoOptimizer_InitNodeLVA;
	self->update = DaoOptimizer_UpdateLVA;
	DaoOptimizer_Init( self, routine );
}
void DaoOptimizer_DoLVA( DaoOptimizer *self, DaoRoutine *routine )
{
	DaoOptimizer_InitLVA( self, routine );
	DaoOptimizer_SolveFlowEquation( self );
}
void DaoOptimizer_DoRDA( DaoOptimizer *self, DaoRoutine *routine )
{
	DaoOptimizer_InitRDA( self, routine );
	DaoOptimizer_SolveFlowEquation( self );
}
void DaoOptimizer_DoVIA( DaoOptimizer *self, DaoRoutine *routine )
{
	self->reverseFlow = 0;
	self->init = DaoOptimizer_InitNodeVIA;
	self->update = DaoOptimizer_UpdateVIA;
	DaoOptimizer_Init( self, routine );
	DaoOptimizer_SolveFlowEquation( self );
}
void DaoOptimizer_LinkDU( DaoOptimizer *self, DaoRoutine *routine )
{
	DaoCnode *node, *node2, **nodes;
	daoint i, j, N;

	DaoOptimizer_InitRDA( self, routine );
	DaoOptimizer_SolveFlowEquation( self );

	nodes = self->nodes->items.pCnode;
	N = self->nodes->size;
	for(i=0; i<N; i++){
		node = nodes[i];
		node->defs->size = 0;
		node->uses->size = 0;
	}
	for(i=0; i<N; i++){
		node = nodes[i];
		for(j=0; j<node->list->size; ++j){
			int id = node->list->items.pInt[j];
			int uses = 0;
			if( id < RDA_OFFSET ) continue;
			node2 = self->nodes->items.pCnode[id-RDA_OFFSET];
			switch( node->type ){
			case DAO_OP_SINGLE :
				uses = node->first == node2->lvalue;
				break;
			case DAO_OP_PAIR   :
				uses = node->first == node2->lvalue;
				uses |= node->second == node2->lvalue;
				break;
			case DAO_OP_TRIPLE :
				uses = node->first == node2->lvalue;
				uses |= node->second == node2->lvalue;
				uses |= node->third == node2->lvalue;
				break;
			case DAO_OP_RANGE :
			case DAO_OP_RANGE2 :
				uses = node->first <= node2->lvalue && node2->lvalue < node->second;
				if( node->type == DAO_OP_RANGE2 ) uses |= node->third == node2->lvalue;
				break;
			}
			if( uses ){
				DList_Append( node->defs, node2 );
				DList_Append( node2->uses, node );
			}
		}
	}
#if 0
	DaoOptimizer_Print( self );
	for(i=0; i<N; i++){
		node = nodes[i];
		printf( "%03i: ", i );
		DaoVmCode_Print( routine->body->vmCodes->data.codes[i], NULL );
		for(j=0; j<node->defs->size; j++) printf( "%3i ", node->defs->items.pCnode[j]->index );
		printf("\n");
		for(j=0; j<node->uses->size; j++) printf( "%3i ", node->uses->items.pCnode[j]->index );
		printf("\n\n" );
	}
#endif
}

static int DaoRoutine_IsVolatileParameter( DaoRoutine *self, int id )
{
	DaoType *T;
	if( id >= self->parCount ) return 0;
	if( id >= self->routType->nested->size ) return 1;
	T = self->routType->nested->items.pType[id];
	if( T && (T->attrib & DAO_TYPE_PARNAMED) ) T = (DaoType*) T->aux;
	if( T == NULL || T->tid == DAO_UDT || T->tid >= DAO_ARRAY  ) return 1;
	return 0;
}
static void DaoOptimizer_AddKill( DaoOptimizer *self, DMap *out, int kill )
{
	daoint i;
	DList *kills = self->uses->items.pList[kill];
	for(i=0; i<kills->size; i++){
		DaoCnode *node = kills->items.pCnode[i];
		if( node->exprid != 0xffff ) MAP_Insert( out, node, 0 );
	}
}
static void DaoOptimizer_InitKills( DaoOptimizer *self )
{
	DNode *it;
	DMap *kills = self->tmp;
	DaoType **types = self->routine->body->regType->items.pType;
	DaoCnode *node, **nodes = self->nodes->items.pCnode;
	DaoVmCodeX *vmc, **codes = self->routine->body->annotCodes->items.pVmc;
	DaoVmCode *codes2 = self->routine->body->vmCodes->data.codes;
	daoint i, j, N = self->nodes->size;
	daoint at, bt, ct, code, lvalue, overload;
	for(i=0; i<N; i++){
		vmc = codes[i];
		node = nodes[i];
		DMap_Reset( kills );
		lvalue = node->lvalue != 0xffff ? node->lvalue : node->lvalue2;
		if( lvalue != 0xffff ) DaoOptimizer_AddKill( self, kills, lvalue );

		/* Check if the operation may modify its arguments or operands: */
		code = DaoVmCode_GetOpcodeType( (DaoVmCode*) vmc );
		switch( code ){
		case DAO_CODE_CALL :
		case DAO_CODE_YIELD :
			for(j=node->first; j<node->second; j++) DaoOptimizer_AddKill( self, kills, j );
			break;
		case DAO_CODE_GETF :
		case DAO_CODE_GETI :
		case DAO_CODE_GETM :
			at = types[node->first] ? types[node->first]->tid : DAO_UDT;
			overload = (at & DAO_ANY) || at == DAO_VARIANT;
			overload |= at >= DAO_OBJECT && at <= DAO_INTERFACE;
			if( overload == 0 ) break;
			DaoOptimizer_AddKill( self, kills, node->first );
			if( code == DAO_CODE_GETI ) DaoOptimizer_AddKill( self, kills, node->second );
			/* node->lvalue must already be in kills; */
			if( code != DAO_CODE_GETM ) break;
			for(j=node->first; j<node->second; j++) DaoOptimizer_AddKill( self, kills, j );
			break;
		case DAO_CODE_SETF :
		case DAO_CODE_SETI :
		case DAO_CODE_SETM :
			ct = types[node->lvalue2] ? types[node->lvalue2]->tid : DAO_UDT;
			overload = (ct & DAO_ANY) || ct == DAO_VARIANT;
			overload |= ct >= DAO_OBJECT && ct <= DAO_INTERFACE;
			if( overload == 0 ) break;
			DaoOptimizer_AddKill( self, kills, node->first );
			if( code == DAO_CODE_SETI ) DaoOptimizer_AddKill( self, kills, node->second );
			/* node->lvalue2 must already be in kills; */
			if( code != DAO_CODE_SETM ) break;
			for(j=node->first; j<node->second; j++) DaoOptimizer_AddKill( self, kills, j );
			DaoOptimizer_AddKill( self, kills, node->third );
			break;
		case DAO_CODE_MOVE :
		case DAO_CODE_UNARY :
			at = types[node->first] ? types[node->first]->tid : DAO_UDT;
			ct = types[node->lvalue] ? types[node->lvalue]->tid : DAO_UDT;
			overload  = (at & DAO_ANY) || at == DAO_VARIANT;
			overload |= (ct & DAO_ANY) || ct == DAO_VARIANT;
			overload |= at >= DAO_OBJECT && at <= DAO_INTERFACE;
			overload |= ct >= DAO_OBJECT && ct <= DAO_INTERFACE;
			if( overload == 0 ) break;
			DaoOptimizer_AddKill( self, kills, node->first );
			/* node->lvalue must already be in kills; */
			break;
		case DAO_CODE_BINARY :
			at = types[node->first] ? types[node->first]->tid : DAO_UDT;
			bt = types[node->second] ? types[node->second]->tid : DAO_UDT;
			ct = types[node->lvalue] ? types[node->lvalue]->tid : DAO_UDT;
			overload  = (at & DAO_ANY) || at == DAO_VARIANT;
			overload |= (bt & DAO_ANY) || bt == DAO_VARIANT;
			overload |= (ct & DAO_ANY) || ct == DAO_VARIANT;
			overload |= at >= DAO_OBJECT && at <= DAO_INTERFACE;
			overload |= bt >= DAO_OBJECT && bt <= DAO_INTERFACE;
			overload |= ct >= DAO_OBJECT && ct <= DAO_INTERFACE;
			if( overload == 0 ) break;
			DaoOptimizer_AddKill( self, kills, node->first );
			DaoOptimizer_AddKill( self, kills, node->second );
			/* node->lvalue must already be in kills; */
			break;
		}
		node->kills->size = 0;
		for(it=DMap_First(kills); it; it=DMap_Next(kills,it)){
			DList_Append( node->kills, it->key.pVoid );
		}
		DaoAEA_Sort( node->kills->items.pCnode, 0, node->kills->size - 1, codes2 );
	}
}
static void DaoOptimizer_InitNodesRDA( DaoOptimizer *self )
{
	DaoCnode *node, **nodes = self->nodes->items.pCnode;
	daoint i, N = self->nodes->size;
	self->enodes->size = 0;
	for(i=0; i<N; i++){
		node = nodes[i];
		if( node->lvalue == 0xffff ) continue;
		DList_Append( self->uses->items.pList[node->lvalue], (node->index + RDA_OFFSET) );
	}
}

static void DaoRoutine_UpdateCodes( DaoRoutine *self )
{
	DList *annotCodes = self->body->annotCodes;
	DArray *vmCodes = self->body->vmCodes;
	DaoVmCodeX *vmc, **vmcs = annotCodes->items.pVmc;
	int i, C, K, N = annotCodes->size;
	int *ids;

	if( vmCodes->size < N ) DArray_Resize( vmCodes, N );
	ids = (int*)vmCodes->data.codes; /* as temporary buffer; */
	for(i=0,K=0; i<N; i++){
		ids[i] = K;
		K += vmcs[i]->code < DVM_UNUSED;
	}
	for(i=0,K=0; i<N; i++){
		vmc = vmcs[i];
		if( vmc->code >= DVM_UNUSED ) continue;
		switch( vmc->code ){
		case DVM_GOTO : case DVM_CASE : case DVM_SWITCH :
		case DVM_TEST : case DVM_TEST_B : case DVM_TEST_I : case DVM_TEST_F :
			vmc->b = ids[ vmc->b ];
			break;
		default : break;
		}
		*vmcs[K++] = *vmc;
	}
	DList_Erase( annotCodes, K, -1 );
	vmcs = annotCodes->items.pVmc;
	vmCodes->size = K;
	N = 0;
	for(i=0; i<K; i++){
		vmc = vmcs[i];
		vmCodes->data.codes[i] = *(DaoVmCode*)vmc;
		C = vmc->code;
		if( C == DVM_GOTO || C == DVM_TEST || (C >= DVM_TEST_B && C <= DVM_TEST_F) ){
			if( vmc->b == (i+1) ){
				vmc->code = DVM_UNUSED;
				N = 1;
			}
		}
	}
	if( N ) DaoRoutine_UpdateCodes( self );
	if( annotCodes->size < 0.8 * annotCodes->bufsize ){
		DList_Resize( annotCodes, annotCodes->size, NULL );
		DArray_Resize( vmCodes, vmCodes->size );
	}
}
void DaoRoutine_UpdateRegister( DaoRoutine *self, DList *mapping )
{
	DNode *it;
	DList *array = DList_New( DAO_DATA_VALUE );
	DMap *localVarType2 = DMap_New(0,0);
	DMap *localVarType = self->body->localVarType;
	DaoType **types = self->body->regType->items.pType;
	DaoVmCode check, *vmc, *codes = self->body->vmCodes->data.codes;
	DaoVmCode **codes2 = (DaoVmCode**) self->body->annotCodes->items.pVmc;
	daoint i, N = self->body->annotCodes->size;
	daoint k, m = 0, M = self->body->regCount;
	daoint *regmap = mapping->items.pInt;

	for(i=0; i<M; i++){
		k = regmap[i];
		if( (k + 1) >= m && k < M ) m = k + 1;
	}
	DList_Resize( array, m, 0 );
	for(i=0; i<M; i++){
		k = regmap[i];
		if( k >= m ) continue;
		GC_Assign( & array->items.pType[k], types[i] );
		if( (it = MAP_Find( localVarType, i )) ) MAP_Insert( localVarType2, k, it->value.pVoid );
	}
	self->body->regCount = array->size;
	self->body->localVarType = localVarType2;
	DList_Assign( self->body->regType, array );
	DList_Delete( array );
	DMap_Delete( localVarType );

	DaoRoutine_SetupSimpleVars( self );

	for(i=0; i<N; i++){
		vmc = codes2[i];
		check = DaoVmCode_CheckOperands( vmc );
		if( check.a ) vmc->a = regmap[ vmc->a ];
		if( check.b ) vmc->b = regmap[ vmc->b ];
		if( check.c ) vmc->c = regmap[ vmc->c ];
		codes[i] = *vmc;
	}
}
static void DaoOptimizer_UpdateRegister( DaoOptimizer *self, DaoRoutine *routine, DList *mapping )
{
	DNode *it;
	DaoVmCode *vmc, **codes = (DaoVmCode**) routine->body->annotCodes->items.pVmc;
	DaoCnode *node, **nodes = self->nodes->items.pCnode;
	daoint i, N = routine->body->annotCodes->size;
	daoint k, m = 0, M = routine->body->regCount;
	daoint *regmap = (daoint*)dao_calloc( M, sizeof(daoint) );

	/* Assuming nonmonotonic mapping: */
	for(i=0; i<M; i++){
		k = mapping->items.pInt[i];
		if( k >= 0 && k < M && regmap[k] == 0 ) regmap[k] = ++ m;
	}
	for(i=0; i<M; i++){
		k = mapping->items.pInt[i];
		if( k < 0 || k >= M ) continue;
		k = regmap[k] - 1;
		mapping->items.pInt[i] = k;
	}
	dao_free( regmap );
	regmap = mapping->items.pInt;

	DaoRoutine_UpdateRegister( routine, mapping );

	for(i=0; i<N; i++){
		vmc = codes[i];
		node = nodes[i];
		switch( (k = DaoVmCode_GetOpcodeType( vmc )) ){
		case DAO_CODE_NOP :
			break;
		case DAO_CODE_GETC :
		case DAO_CODE_GETG :
			node->lvalue = vmc->c;
			break;
		case DAO_CODE_SETU :
			node->first = vmc->a;
			if( vmc->c != 0 ){
				node->second = vmc->b;
				node->lvalue2 = vmc->b;
			}
			break;
		case DAO_CODE_SETG :
		case DAO_CODE_BRANCH :
			node->first = vmc->a;
			break;
		case DAO_CODE_EXPLIST :
			if( vmc->b == 0 ) break;
			node->second += vmc->a - node->first;
			node->first = vmc->a;
			break;
		case DAO_CODE_GETF : case DAO_CODE_SETF :
		case DAO_CODE_MOVE : case DAO_CODE_UNARY :
			node->first = vmc->a;
			if( k == DAO_CODE_SETF ){
				node->second = vmc->c;
				node->lvalue2 = vmc->c;
			}else{
				node->lvalue = vmc->c;
			}
			break;
		case DAO_CODE_GETM :
		case DAO_CODE_ENUM2 : case DAO_CODE_MATRIX :
		case DAO_CODE_ROUTINE : case DAO_CODE_CALL :
			node->second += vmc->a - node->first;
			node->first = vmc->a;
			node->lvalue = vmc->c;
			break;
		case DAO_CODE_SETM:
			node->second += vmc->c - node->first;
			node->first = vmc->c;
			node->third = vmc->a;
			node->lvalue2 = vmc->c;
			break;
		case DAO_CODE_ENUM :
			if( (vmc->b&(0xffff>>2)) || vmc->a < M ){
				node->second += vmc->a - node->first;
				node->first = vmc->a;
			}
			node->lvalue = vmc->c;
			break;
		case DAO_CODE_YIELD :
			if( vmc->b || vmc->a < M ){
				node->second += vmc->a - node->first;
				node->first = vmc->a;
			}
			node->lvalue = vmc->c;
			break;
		case DAO_CODE_SETI :
		case DAO_CODE_GETI :
		case DAO_CODE_BINARY :
			node->first = vmc->a;
			node->second = vmc->b;
			if( k == DAO_CODE_SETI ){
				node->third = vmc->c;
				node->lvalue2 = vmc->c;
			}else{
				node->lvalue = vmc->c;
			}
			break;
		case DAO_CODE_GETU :
			node->lvalue = vmc->c;
			if( vmc->a != 0 ) node->second = vmc->b;
			break;
		case DAO_CODE_UNARY2 :
			node->second = vmc->b;
			node->lvalue = vmc->c;
			break;
		default: break;
		}
	}
	self->enodes->size = 0;
	DMap_Reset( self->exprs );
	for(i=0; i<N; i++){
		vmc = codes[i];
		node = nodes[i];
		if( node->exprid == 0xffff ) continue;
		it = MAP_Find( self->exprs, vmc );
		if( it == NULL ){
			it = MAP_Insert( self->exprs, vmc, self->enodes->size );
			DList_Append( self->enodes, node );
		}
		node->exprid = it->value.pInt;
	}
}
static int DaoCnode_CountDefinitions( DaoCnode *self, int reg )
{
	int i, K = 0, N = self->defs->size;
	for(i=0; i<N; ++i){
		DaoCnode *node = self->defs->items.pCnode[i];
		K += node->lvalue == reg;
	}
	return K;
}
/* Common Subexpression Elimination: */
static void DaoOptimizer_CSE( DaoOptimizer *self, DaoRoutine *routine )
{
	DList *fixed = DList_New(0);
	DList *inodes = DList_New(0);
	DList *avexprs = DList_New(0);
	DList *types = routine->body->regType;
	DList *annotCodes = routine->body->annotCodes;
	DaoCnode *node, *node2, **nodes;
	daoint i, j, k, N = annotCodes->size;
	daoint M = routine->body->regCount;
	int reg, tid, fixed1, fixed2, sametype;

	DaoOptimizer_LinkDU( self, routine );
	DaoOptimizer_InitAEA( self, routine );
	DaoOptimizer_SolveFlowEquation( self );
	DaoRoutine_CodesToInodes( routine, inodes, 0 );
	nodes = self->nodes->items.pCnode;

	/* DaoOptimizer_Print( self ); */
	DList_Resize( fixed, M, 0 );
	for(i=0; i<M; ++i) fixed->items.pInt[i] = 0;
	for(i=0; i<N; ++i){
		node = nodes[i];
		if( node->type != DAO_OP_RANGE && node->type != DAO_OP_RANGE2 ) continue;
		for(j=node->first; j<node->second; ++j) fixed->items.pInt[j] = 1;
	}

	for(i=0; i<N; ++i){
		DaoInode *vmc2, *vmc = inodes->items.pInode[i];
		DaoVmCode check;
		node = nodes[i];
		if( node->lvalue == 0xffff || node->exprid == 0xffff ) continue;
		if( DaoVmCode_GetOpcodeType( (DaoVmCode*) vmc ) == DAO_CODE_MOVE ) continue;
		if( vmc->code <= DVM_JITC ) continue;

		sametype = 1;
		avexprs->size = 0;
		for(j=0; j<node->list->size; ++j){
			node2 = node->list->items.pCnode[j];
			vmc2 = inodes->items.pInode[node2->index];
			if( vmc->code != vmc2->code || vmc->a != vmc2->a || vmc->b != vmc2->b ) continue;
			if( types->items.pType[vmc->c] != types->items.pType[node2->lvalue] ) sametype = 0;
			DList_Append( avexprs, node2 );
		}
		if( avexprs->size == 0 ) continue;

		node2 = avexprs->items.pCnode[0];
		fixed1 = fixed->items.pInt[vmc->c];
		fixed2 = fixed->items.pInt[node2->lvalue];
		if( avexprs->size == 1 && sametype && fixed1 == 0 && fixed2 == 0 ){
			/*
			// Check for each use of node->lvalue, if they have single definition
			// which is node->lvalue, one can simply update the operands of the
			// use instructions:
			 */
			for(j=0; j<node->uses->size; ++j){
				DaoCnode *use = node->uses->items.pCnode[j];
				if( DaoCnode_CountDefinitions( use, node->lvalue ) > 1 ) break;
			}
			if( j == node->uses->size ){
				for(j=0; j<node->uses->size; ++j){
					DaoCnode *use = node->uses->items.pCnode[j];
					DaoInode *inode = inodes->items.pInode[use->index];
					check = DaoVmCode_CheckOperands( (DaoVmCode*) inode );
					if( check.a && inode->a == vmc->c ) inode->a = node2->lvalue;
					if( check.b && inode->b == vmc->c ) inode->b = node2->lvalue;
					if( check.c && inode->c == vmc->c ) inode->c = node2->lvalue;
				}
				vmc->code = DVM_UNUSED;
				node->type = DAO_OP_NONE;
				node->lvalue = 0xffff;
				node->lvalue2 = 0xffff;
				node->exprid = 0xffff;
				continue;
			}
		}
		k = DaoVmCode_GetOpcodeType( (DaoVmCode*) vmc );
		/* Adding moves for these will just increase the codes, so skip it: */
		if( k == DAO_CODE_MOVE || k == DAO_CODE_GETC ) continue;

		reg = types->size;
		tid = types->items.pType[vmc->c]->tid;
		DList_Append( fixed, IntToPointer(0) );
		DList_Append( types, types->items.pVoid[vmc->c] );
		for(j=0; j<avexprs->size; ++j){
			DaoCnode *cnode = avexprs->items.pCnode[j];
			DaoInode *prev = inodes->items.pInode[cnode->index];
			DaoInode *next = prev->next;
			DaoInode *inode;

			inode = DaoInode_New();
			inode->level = prev->level;
			inode->line = prev->line;
			inode->first = prev->first;
			inode->middle = prev->middle;
			inode->last = prev->last;
			inode->code = DVM_MOVE;

			switch( types->items.pType[prev->c]->tid == tid ? tid : 0 ){
			case DAO_BOOLEAN : inode->code = DVM_MOVE_BB; break;
			case DAO_INTEGER : inode->code = DVM_MOVE_II; break;
			case DAO_FLOAT   : inode->code = DVM_MOVE_FF; break;
			case DAO_COMPLEX : inode->code = DVM_MOVE_CC; break;
			case DAO_STRING  : inode->code = DVM_MOVE_SS; break;
			}

			inode->c = prev->c;
			prev->c = inode->a = reg;

			prev->next = inode;
			inode->prev = next;
			inode->next = next;
			next->prev = inode;
		}
		vmc->code = DVM_MOVE_XX;
		vmc->a = reg;
		vmc->b = 0;
		node->type = DAO_OP_SINGLE;
		node->lvalue2 = 0xffff;
		switch( types->items.pType[vmc->c]->tid ){
		case DAO_BOOLEAN : vmc->code = DVM_MOVE_BB; break;
		case DAO_INTEGER : vmc->code = DVM_MOVE_II; break;
		case DAO_FLOAT   : vmc->code = DVM_MOVE_FF; break;
		case DAO_COMPLEX : vmc->code = DVM_MOVE_CC; break;
		case DAO_STRING  : vmc->code = DVM_MOVE_SS; break;
		}
	}
	routine->body->regCount = routine->body->regType->size;
	DaoRoutine_SetupSimpleVars( routine );
	DaoRoutine_CodesFromInodes( routine, inodes );
	DaoInodes_Clear( inodes );

	DList_Delete( inodes );
	DList_Delete( fixed );
	DList_Delete( avexprs );
}
/* Dead Code Elimination: */
static void DaoOptimizer_DCE( DaoOptimizer *self, DaoRoutine *routine )
{
	DList *inodes = DList_New(0);
	DaoCnode *node, *node2, **nodes;
	daoint N = routine->body->annotCodes->size;
	daoint i, j;

	DaoOptimizer_LinkDU( self, routine );
	DaoOptimizer_DoLVA( self, routine );
	DaoRoutine_CodesToInodes( routine, inodes, 0 );

	nodes = self->nodes->items.pCnode;
	for(i=N-1; i>=0; --i){
		int used = 0;
		node = nodes[i];
		if( node->lvalue == 0xffff ) continue;
		if( node->exprid == 0xffff ) continue; /* this instruction may have side effect; */
		if( DaoRoutine_IsVolatileParameter( self->routine, node->lvalue ) ) continue;
		if( DaoCnode_FindResult( node, IntToPointer(node->lvalue) ) >= 0 ){
			/* Check if the instructions that use node->lvalue are marked as dead: */
			for(j=0; j<node->uses->size; ++j){
				node2 = node->uses->items.pCnode[j];
				if( inodes->items.pInode[node2->index]->code != DVM_UNUSED ){
					used = 1;
					break;
				}
			}
		}
		if( used ) continue;
		inodes->items.pInode[node->index]->code = DVM_UNUSED;
	}
	DaoRoutine_CodesFromInodes( routine, inodes );

	DaoInodes_Clear( inodes );
	DList_Delete( inodes );
}
/* Simple remapping the used registers to remove the unused ones: */
static void DaoOptimizer_RemapRegister( DaoOptimizer *self, DaoRoutine *routine )
{
	DaoCnode *node, **nodes;
	DList *array = self->array;
	daoint i, N = routine->body->annotCodes->size;
	daoint j, M = routine->body->regCount;
	daoint *regmap;

	DList_Resize( array, M, 0 );
	regmap = array->items.pInt;
	for(i=0; i<M; i++) array->items.pInt[i] = M; /* mark all register unused; */

	/* Dead Code Elimination may have produce some dead registers. Remove them first: */
	self->update = NULL;
	DaoOptimizer_Init( self, routine );
	nodes = self->nodes->items.pCnode;
	for(i=0; i<N; i++){
		node = nodes[i];
		switch( node->type ){
		case DAO_OP_SINGLE :
			regmap[ node->first ] = node->first;
			break;
		case DAO_OP_PAIR :
			regmap[ node->first ] = node->first;
			regmap[ node->second ] = node->second;
			break;
		case DAO_OP_TRIPLE :
			regmap[ node->first ] = node->first;
			regmap[ node->second ] = node->second;
			regmap[ node->third ] = node->third;
			break;
		case DAO_OP_RANGE :
		case DAO_OP_RANGE2 :
			for(j=node->first; j<node->second; j++) regmap[j] = j;
			if( node->type == DAO_OP_RANGE2 ) regmap[ node->third ] = node->third;
			break;
		}
		if( node->lvalue != 0xffff ) regmap[ node->lvalue ] = node->lvalue;
	}
	for(i=0; i<routine->parCount; i++) regmap[i] = i;
	DaoOptimizer_UpdateRegister( self, routine, array );
}
void DaoOptimizer_RemoveUnreachableCodes( DaoOptimizer *self, DaoRoutine *routine )
{
	DList *array = self->array;
	DList *annotCodes = routine->body->annotCodes;
	DaoVmCodeX **codes = annotCodes->items.pVmc;
	DaoCnode *node, *node2, **nodes;
	daoint i, j, m, N = annotCodes->size;

	if( N == 0 ) return;
	self->update = NULL;
	DaoOptimizer_Init( self, routine );
	nodes = self->nodes->items.pCnode;
	for(i=0; i<N; i++) nodes[i]->reachable = 0;
	array->size = 0;
	DList_Append( array, nodes[0] );
	for(i=0; i<array->size; i++){
		node = array->items.pCnode[i];
		node->reachable = 1;
		for(j=0,m=node->outs->size; j<m; j++){
			node2 = node->outs->items.pCnode[j];
			if( node2->reachable == 0 ) DList_Append( array, node2 );
		}
	}
	for(i=0; i<N; i++){
		if( nodes[i]->reachable == 0 ) codes[i]->code = DVM_UNUSED;
	}
	DaoRoutine_UpdateCodes( routine );
	DaoOptimizer_RemapRegister( self, routine );
}
/* Remove redundant registers for the same data types: */
static void DaoOptimizer_ReduceRegister( DaoOptimizer *self, DaoRoutine *routine )
{
	DNode *it, *it2, *it3, *it4;
	DMap *one = DHash_New(0,0);
	DMap *sets = DHash_New(0,DAO_DATA_MAP);
	DMap *livemap = DMap_New(0,0);
	DMap *actives = DMap_New(0,0);
	DMap *offsets = DMap_New(0,0);
	DList *array = DList_New(0);
	DList *annotCodes = routine->body->annotCodes;
	DaoVmCodeX *vmc, **codes = annotCodes->items.pVmc;
	DaoCnode *node, *node2, **nodes;
	DaoType *type, **types;
	daoint i, j, k, N = annotCodes->size;
	daoint regCount, M = routine->body->regCount;
	daoint *intervals, *regmap;

	/* Dead Code Elimination may have produce some dead registers. Remove them first: */
	DaoOptimizer_RemapRegister( self, routine );
	regmap = self->array->items.pInt;

	/* DaoRoutine_PrintCode( routine, routine->nameSpace->vmSpace->errorStream ); */

	/* Live Variable Analysis for register reallocation: */
	DaoOptimizer_DoLVA( self, routine );

	/* DaoOptimizer_Print( self ); */

	/* Now use the linear scan algorithm (Poletto and Sarkar) to reallocate registers: */
	DList_Resize( array, 2*M, 0 );
	intervals = array->items.pInt;
	regCount = routine->parCount;
	M = routine->body->regCount;
	types = routine->body->regType->items.pType;
	for(i=0; i<routine->parCount; i++){
		regmap[i] = i;
		intervals[2*i] = 0; /* initialize the live interval of the register; */
		intervals[2*i+1] = N-1;
	}
	for(i=routine->parCount; i<M; i++){
		type = types[i];
		regmap[i] = -1; /* mark the register not yet mapped; */
		intervals[2*i] = -1; /* initialize the live interval of the register; */
		intervals[2*i+1] = -1;
		/* map untyped register to distinctive new register ids: */
		if( type == NULL || type->tid == DAO_ANY || type->tid == DAO_VARIANT
				|| (type->attrib & (DAO_TYPE_SPEC|DAO_TYPE_UNDEF)) )
			regmap[i] = regCount ++;
	}
	nodes = self->nodes->items.pCnode;
	for(i=0; i<N; i++){
		node = nodes[i];
		vmc = codes[i];
		for(j=0,k=0; j<node->list->size; ++j){
			int id = node->list->items.pInt[j];
			if( intervals[2*id] < 0 ) intervals[2*id] = i;
			intervals[2*id+1] = i + 1; /* plus one because it is alive at the exit; */
			k += 1;
		}
		if( vmc->code == DVM_LOAD || vmc->code == DVM_CAST || vmc->code == DVM_UNTAG ){
			/* These opcodes may create a reference (alias) of ::a at the result register (::c),
			// the liveness interval of ::a must be expanded to the point where ::c is used: */
			for(j=i+1; j<N; ++j){
				node2 = nodes[j];
				k = 0;
				switch( node2->type ){
				case DAO_OP_TRIPLE : k = node2->third == vmc->c;   /* fall through; */
				case DAO_OP_PAIR   : k |= node2->second == vmc->c; /* fall through; */
				case DAO_OP_SINGLE : k |= node2->first == vmc->c; break;
				case DAO_OP_RANGE :
				case DAO_OP_RANGE2 : k = node2->first <= vmc->c && vmc->c < node2->second; break;
				}
				if( k ) break;
			}
			if( j < N && j > intervals[2*vmc->a+1] ) intervals[2*vmc->a+1] = j;
		}
		if( node->lvalue != 0xffff && intervals[2*node->lvalue] < 0 ){
			/* For some DVM_CALL, the lvalue is not alive, but it is needed: */
			intervals[2*node->lvalue] = i;
			intervals[2*node->lvalue+1] = i;
		}
		if( node->lvalue != 0xffff && regmap[ node->lvalue ] < 0 ){
			type = types[ node->lvalue ];
			/* No reduction for register that may hold reference to simple types: */
			if( type && type->tid && (type->tid < DAO_ARRAY || type->tid == DAO_VARIANT) ){
				if( DaoVmCode_MayCreateReference( vmc->code ) ) regmap[node->lvalue] = regCount ++;
			}
		}
		if( node->type != DAO_OP_RANGE && node->type != DAO_OP_RANGE2 ) continue;
		/* map this range of registers to a new range of registers: */
		for(j=node->first; j<node->second; j++){
			if( regmap[j] >= 0 ) continue;
			regmap[j] = regCount ++;
		}
	}
	for(i=0; i<M; i++){
		type = types[i];
		/* printf( "%3i: %3i %3i\n", i, intervals[2*i], intervals[2*i+1] ); */
		if( regmap[i] >= 0 ) continue;
		/* count the number of registers with the same types: */
		if( (it = MAP_Find( sets, type )) == NULL ) it = MAP_Insert( sets, type, one );
		MAP_Insert( it->value.pMap, it->value.pMap->size, 0 );
	}
	/* The registers of the same type will be mapped to the same group of registers.
	// Now calculate the register index offset for each group: */
	for(it=DMap_First(sets); it; it=DMap_Next(sets,it)){
		MAP_Insert( offsets, it->key.pVoid, regCount );
		regCount += it->value.pMap->size;
	}
	assert( regCount == M );

	/* Sort all the live intervals by the starting point and the register index: */
	for(i=0; i<M; i++){
		size_t key = (intervals[2*i] << 16) | i;
		MAP_Insert( livemap, key, i );
	}
	/* Iterate through the liveness starting points: */
	for(it=DMap_First(livemap); it; it=DMap_Next(livemap,it)){
		daoint start = it->key.pInt >> 16;
		i = it->value.pInt;
		if( regmap[i] >= 0 ) continue;
		/* Remove intervals ended before this point: */
		while( (it2 = DMap_First(actives)) && (it2->key.pInt >> 16) < start ){
			type = types[ it2->key.pInt & 0xffff ];
			it3 = MAP_Find( sets, type );
			MAP_Insert( it3->value.pMap, it2->value.pVoid, 0 ); /* Release the register; */
			DMap_EraseNode( actives, it2 );
		}
		type = types[i];
		it2 = MAP_Find( offsets, type );
		it3 = MAP_Find( sets, type );
		it4 = DMap_First( it3->value.pMap );
		regmap[i] = it2->value.pInt + it4->key.pInt;
		/* add new active interval; */
		MAP_Insert( actives, (intervals[2*i+1]<<16) | i, it4->key.pVoid );
		DMap_EraseNode( it3->value.pMap, it4 );
	}
	DaoOptimizer_UpdateRegister( self, routine, self->array );
	DList_Delete( array );
	DMap_Delete( livemap );
	DMap_Delete( offsets );
	DMap_Delete( actives );
	DMap_Delete( sets );
	DMap_Delete( one );
	for(i=0; i<N; i++){
		vmc = codes[i];
		if( vmc->code >= DVM_MOVE_BB && vmc->code <= DVM_MOVE_XX && vmc->a == vmc->c )
			vmc->code = DVM_UNUSED;
	}
	DaoRoutine_UpdateCodes( routine );
}


void DaoOptimizer_Optimize( DaoOptimizer *self, DaoRoutine *routine )
{
	DaoType *type, **types = routine->body->regType->items.pType;
	DaoVmSpace *vms = routine->nameSpace->vmSpace;
	daoint i, k;

	if( daoConfig.optimize == 0 ) return;

	/* Do not perform optimization if it may take too much memory: */
	if( (routine->body->vmCodes->size * routine->body->regCount) > 1000000 ) return;

	if( routine->body->simpleVariables->size < routine->body->regCount / 2 ) return;
	for(i=0,k=0; i<routine->body->simpleVariables->size; i++){
		type = types[ routine->body->simpleVariables->items.pInt[i] ];
		k += type ? type->tid >= DAO_BOOLEAN && type->tid <= DAO_COMPLEX : 0;
	}
	/* Optimize only if there are sufficient amount of numeric calculations: */
	if( k < routine->body->regCount / 2 ) return;

	DaoOptimizer_CSE( self, routine );
	DaoOptimizer_DCE( self, routine );
	DaoOptimizer_ReduceRegister( self, routine );

	/* DaoOptimizer_LinkDU( self, routine ); */
}


void DaoOptimizer_InitNode( DaoOptimizer *self, DaoCnode *node, DaoVmCode *vmc )
{
	DNode *it;
	DList **uses = self->uses->items.pList;
	DaoRoutine *routine = self->routine;
	DaoType **types = routine->body->regType->items.pType;
	DMap *localVarType = routine->body->localVarType;
	uchar_t type = DaoVmCode_GetOpcodeType( vmc );
	int i, at, bt, ct, code = vmc->code;

	DaoCnode_InitOperands( node, vmc );

	node->initvar = -1;
	if( self->update == DaoOptimizer_UpdateVIA ){
		switch( type ){
		case DAO_CODE_GETC :
		case DAO_CODE_GETG :
		case DAO_CODE_GETU :
		case DAO_CODE_GETF :
		case DAO_CODE_GETI :
		case DAO_CODE_GETM :
		case DAO_CODE_MOVE :
		case DAO_CODE_UNARY :
		case DAO_CODE_UNARY2 :
		case DAO_CODE_BINARY :
		case DAO_CODE_MATRIX :
		case DAO_CODE_ENUM :
		case DAO_CODE_ENUM2 :
		case DAO_CODE_CALL :
		case DAO_CODE_ROUTINE :
		case DAO_CODE_YIELD :
			node->initvar = vmc->c;
			break;
		case DAO_CODE_EXPLIST :
			if( vmc->code != DVM_SECT ) break;
			/* Is handled during the analysis */
			break;
		case DAO_CODE_SETG :
			switch( vmc->code ){
			case DVM_SETVO :
			case DVM_SETVO_BB :
			case DVM_SETVO_II :
			case DVM_SETVO_FF :
			case DVM_SETVO_CC :
				node->initvar = (DAO_OBJECT_VARIABLE<<16)|vmc->b;
				break;
			}
			break;
		case DAO_CODE_SETU :
			if( vmc->a != 0 ) node->initvar = vmc->b;
			break;
		}
	}

	/* Exclude expression that does not yield new value: */
	if( node->lvalue == 0xffff ) return;
	if( self->update == DaoOptimizer_UpdateRDA ) return;
	switch( type ){
	case DAO_CODE_GETG :
	case DAO_CODE_GETU :
	case DAO_CODE_ROUTINE :
	case DAO_CODE_CALL :
	case DAO_CODE_YIELD :
		/* Exclude expressions that access global data or must be evaluated: */
		return;
	case DAO_CODE_MOVE :
		if( vmc->code == DVM_LOAD ) return;
		if( DaoRoutine_IsVolatileParameter( routine, vmc->c ) ) return;
		/*
		// Exclude MOVE if the destination is a potential reference (list.back()=1).
		// Note:
		// routine->body->localVarType still contains registers of explicitly declared
		// variables even after optimizations, because registers from instructions that
		// may create references are not merged or reused for register reduction.
		// See also DaoVmCode_MayCreateReference().
		*/
		if( self->refers->items.pInt[vmc->c] ) return;
		break;
	case DAO_CODE_UNARY2 :
		/* Exclude expressions that may have side effects: */
		if( DaoRoutine_IsVolatileParameter( routine, vmc->b ) ) return;
		break;
	case DAO_CODE_GETF :
	case DAO_CODE_UNARY :
		/* Exclude expressions that may have side effects by operator overloading: */
		at = types[vmc->a] ? types[vmc->a]->tid : DAO_UDT;
		ct = types[vmc->c] ? types[vmc->c]->tid : DAO_UDT;
		if( (at & DAO_ANY) || at == DAO_VARIANT ) return;
		if( (ct & DAO_ANY) || ct == DAO_VARIANT ) return;
		if( at >= DAO_OBJECT && at <= DAO_INTERFACE ) return;
		if( ct >= DAO_OBJECT && ct <= DAO_INTERFACE ) return;
		if( DaoRoutine_IsVolatileParameter( routine, vmc->a ) ) return;
		break;
	case DAO_CODE_BINARY :
		/* Exclude binary operations if the destination is a potential reference (list.back()+=1): */
		if( vmc->a == vmc->c || vmc->b == vmc->c )
			if( MAP_Find( localVarType, vmc->c ) == NULL ) return;
		/* Fall through for further checking; */
	case DAO_CODE_GETI :
		/* Exclude expressions that may have side effects by operator overloading: */
		at = types[vmc->a] ? types[vmc->a]->tid : DAO_UDT;
		bt = types[vmc->b] ? types[vmc->b]->tid : DAO_UDT;
		ct = types[vmc->c] ? types[vmc->c]->tid : DAO_UDT;
		/* Need to be executed in any case to finish a loop: */
		if( types[vmc->b] == dao_type_for_iterator ) return;
		if( (at & DAO_ANY) || at == DAO_VARIANT ) return;
		if( (bt & DAO_ANY) || bt == DAO_VARIANT ) return;
		if( (ct & DAO_ANY) || ct == DAO_VARIANT ) return;
		if( at >= DAO_OBJECT && at <= DAO_INTERFACE ) return;
		if( bt >= DAO_OBJECT && bt <= DAO_INTERFACE ) return;
		if( ct >= DAO_OBJECT && ct <= DAO_INTERFACE ) return;
		if( DaoRoutine_IsVolatileParameter( routine, vmc->a ) ) return;
		if( DaoRoutine_IsVolatileParameter( routine, vmc->b ) ) return;
		break;
	}

	it = MAP_Find( self->exprs, vmc );
	if( it == NULL ){
		it = MAP_Insert( self->exprs, vmc, self->enodes->size );
		DList_Append( self->enodes, node );
	}
	node->exprid = it->value.pInt;

	if( node->lvalue != 0xffff ) DList_Append( uses[node->lvalue], node );
	if( node->lvalue2 != 0xffff ) DList_Append( uses[node->lvalue2], node );
	switch( node->type ){
	case DAO_OP_SINGLE :
		DList_Append( uses[node->first], node );
		break;
	case DAO_OP_PAIR   :
		DList_Append( uses[node->first], node );
		DList_Append( uses[node->second], node );
		break;
	case DAO_OP_RANGE :
		for(i=node->first; i<node->second; i++) DList_Append( uses[i], node );
		break;
	}
}


