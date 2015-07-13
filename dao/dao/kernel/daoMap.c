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

#include"stdlib.h"
#include"string.h"
#include"math.h"
#include"assert.h"

#include"daoConst.h"
#include"daoMap.h"
#include"daoList.h"
#include"daoString.h"
#include"daoNumtype.h"
#include"daoValue.h"
#include"daoGC.h"

#define RB_RED    0
#define RB_BLACK  1


static DNode* DNode_New( DMap *map, int keytype, int valtype )
{
	if( map->list && map->keytype == keytype && map->valtype == valtype ){
		DNode *node = map->list;
		map->list = map->list->parent;
		node->parent = NULL;
		return node;
	}
	return (DNode*) dao_calloc( 1, sizeof(DNode) );
}
DNode* DNode_First( DNode *self )
{
	if( self ) while( self->left ) self = self->left;
	return self;
}

DNode* DNode_Next( DNode *self )
{
	DNode* p = self->right;
	if( self->right ){
		while( p->left ) p = p->left;
	}else if( self->parent ){
		if( self == self->parent->left )
			p = self->parent;
		else{
			p = self->parent;
			while( p->parent && p==p->parent->right ) p = p->parent;
			p = p->parent;
		}
	}
	return p;
}
void* DNode_GetKey( DNode *self )
{
	return self->key.pVoid;
}
void* DNode_GetValue( DNode *self )
{
	return self->value.pVoid;
}
DaoValue* DNode_Key( DNode *self )
{
	return self->key.pValue;
}
DaoValue* DNode_Value( DNode *self )
{
	return self->value.pValue;
}

DMap* DMap_New( short kt, short vt )
{
	DMap *self = (DMap*) dao_malloc( sizeof( DMap) );
	self->size = 0;
	self->tsize = 0;
	self->list = NULL;
	self->root = NULL;
	self->table = NULL;
	self->keytype = kt;
	self->valtype = vt;
	self->hashing = 0;
	return self;
}
DMap* DHash_New( short kt, short vt )
{
	DMap *self = DMap_New( kt, vt );
	self->hashing = DAO_HASH_SEED;
	self->tsize = 4;
	self->table = (DNode**) dao_calloc( self->tsize, sizeof(DNode*) );
	return self;
}


static int DaoValue_Hash( DaoValue *self, DaoProcess *process, unsigned int hash )
{
	DaoBoolean bl = {DAO_BOOLEAN,0,0,0,0,1};
	DaoRoutine *routine;
	DaoValue *params[2];
	void *data = NULL;
	int i, len = 0;

	params[0] = (DaoValue*) dao_type_int;
	params[1] = (DaoValue*) & bl;

	switch( self->type ){
	case DAO_INTEGER :
		data = & self->xInteger.value;  len = sizeof(dao_integer);  break;
	case DAO_FLOAT  :
		data = & self->xFloat.value;  len = sizeof(dao_float);  break;
	case DAO_COMPLEX :
		data = & self->xComplex.value;  len = sizeof(dao_complex);  break;
	case DAO_ENUM  :
		data = & self->xEnum.value;
		len = sizeof(int);
		break;
	case DAO_STRING  :
		data = self->xString.value->chars;
		len = self->xString.value->size;
		break;
	case DAO_ARRAY :
		data = self->xArray.data.p;
		len = self->xArray.size;
		switch( self->xArray.etype ){
		case DAO_INTEGER : len *= sizeof(dao_integer); break;
		case DAO_FLOAT   : len *= sizeof(dao_float); break;
		case DAO_COMPLEX : len *= sizeof(dao_complex); break;
		default : break;
		}
		break;
	case DAO_TUPLE :
		for(i=0; i<self->xTuple.size; i++){
			hash = DaoValue_Hash( self->xTuple.values[i], process, hash );
		}
		break;
	case DAO_OBJECT :
		routine = self->xObject.defClass->intOperators;
		if( process == NULL || routine == NULL ) goto Default;
		if( DaoProcess_Call( process, routine, self, params, 2 ) ) goto Default;
		hash = DaoValue_GetInteger( process->stackValues[0] );
		hash = Dao_Hash( & hash, 4, 0 );
		break;
	case DAO_CSTRUCT :
	case DAO_CDATA :
	case DAO_CTYPE :
		routine = self->xCstruct.ctype->kernel->intOperators;
		if( process == NULL || routine == NULL ) goto Default;
		if( DaoProcess_Call( process, routine, self, params, 2 ) ) goto Default;
		hash = DaoValue_GetInteger( process->stackValues[0] );
		hash = Dao_Hash( & hash, 4, 0 );
		break;
	default :
Default:
		data = & self;
		len = sizeof(DaoValue*);
		break;
	}
	if( data ) hash = Dao_Hash( data, len, hash );
	return hash;
}

static int DHash_HashIndex( DMap *self, void *key, DaoProcess *process )
{
#define HASH_MAX  32
	DString *s;
	DList *array;
	unsigned int hash = 0;
	void *data;

	switch( self->keytype ){
	case DAO_DATA_COMPLEX :
		hash = Dao_Hash( key, 2*sizeof(double), self->hashing );
		break;
	case DAO_DATA_STRING :
		s = (DString*)key;
		hash = Dao_Hash( s->chars, s->size, self->hashing );
		break;
	case DAO_DATA_VALUE2 :
	case DAO_DATA_VALUE3 :
		process = NULL;
	case DAO_DATA_VALUE :
		hash = DaoValue_Hash( (DaoValue*) key, process, self->hashing );
		break;
	case DAO_DATA_LIST :
		array = (DList*)key;
		hash = Dao_Hash( array->items.pVoid, array->size * sizeof(void*), self->hashing );
		break;
	case DAO_DATA_VOID2 :
		hash = Dao_Hash( key, 2*sizeof(void*), self->hashing );
		break;
	case DAO_DATA_VMCODE :
		hash = Dao_Hash( key, 4*sizeof(unsigned short), self->hashing );
		break;
	case DAO_DATA_VMCODE2 :
		hash = Dao_Hash( key, 3*sizeof(unsigned short), self->hashing );
		break;
	default :
		hash = Dao_Hash( & key, sizeof(void*), self->hashing );
		break;
	}
	return hash & 0x7fffffff;
}
static DNode* DMap_SimpleInsert( DMap *self, DNode *node, DaoProcess *process );
static void DMap_InsertNode( DMap *self, DNode *node );
static void DMap_InsertTree( DMap *self, DNode *node, DaoProcess *process )
{
	DNode *left = node->left;
	DNode *right = node->right;
	node->parent = node->left = node->right = NULL;
	self->root = self->table[ node->hash % self->tsize ];
	if( self->root == NULL ){
		node->color = RB_BLACK;
		self->table[ node->hash % self->tsize ] = node;
		self->size += 1;
	}else{
		DMap_SimpleInsert( self, node, process );
		DMap_InsertNode( self, node );
		self->table[ node->hash % self->tsize ] = self->root;
	}
	if( left ) DMap_InsertTree( self, left, process );
	if( right ) DMap_InsertTree( self, right, process );
}
static int DMap_Lockable( DMap *self )
{
	int lockable = self->keytype >= DAO_DATA_VALUE && self->keytype <= DAO_DATA_VALUE3;
	lockable |= self->valtype >= DAO_DATA_VALUE && self->valtype <= DAO_DATA_VALUE3;
	return lockable;
}
static void DHash_ResetTable( DMap *self, DaoProcess *process )
{
	DNode **nodes = self->table;
	int i, tsize = self->tsize;

	if( self->hashing ==0 ) return;
	if( DMap_Lockable( self ) ) DaoGC_LockData();
	self->tsize = 2 * self->size + 1;
	self->table = (DNode**)dao_calloc( self->tsize, sizeof(DNode*) );
	self->size = 0;
	for(i=0; i<tsize; i++) if( nodes[i] ) DMap_InsertTree( self, nodes[i], process );
	if( DMap_Lockable( self ) ) DaoGC_UnlockData();
	if( nodes ) dao_free( nodes );
}
DMap* DMap_Copy( DMap *other )
{
	DMap *self = NULL;
	if( other->hashing ){
		self = DHash_New( other->keytype, other->valtype );
		self->tsize = other->tsize;
		self->table = (DNode**)dao_realloc( self->table, other->tsize*sizeof(DNode*) );
		memset( self->table, 0, other->tsize*sizeof(DNode*) );
	}else{
		self = DMap_New( other->keytype, other->valtype );
	}
	DMap_Assign( self, other );
	return self;
}
void DMap_Assign( DMap *self, DMap *other )
{
	DNode *node = DMap_First( other );
	DMap_Reset( self );
	while( node ){
		DMap_Insert( self, node->key.pVoid, node->value.pVoid );
		node = DMap_Next( other, node );
	}
}
static void DMap_DeleteNode( DMap *self, DNode *node );
void DMap_Delete( DMap *self )
{
	DNode *p, *node;
	DMap_Clear( self );
	if( self->table ) dao_free( self->table );
	node = self->list;
	while( node ){
		p = node;
		node = node->parent;
		DMap_DeleteNode( self, p );
	}
	dao_free( self );
}
static void DMap_SwapNode( DMap *self, DNode *node, DNode *extreme )
{
	void *key, *value;
	int hash = extreme->hash;
	key = extreme->key.pVoid;
	value = extreme->value.pVoid;
	extreme->hash = node->hash;
	extreme->key.pVoid = node->key.pVoid;
	extreme->value.pVoid = node->value.pVoid;
	node->hash = hash;
	node->key.pVoid = key;
	node->value.pVoid = value;
}
static dao_complex* dao_complex_new( dao_complex *other )
{
	dao_complex *res = (dao_complex*) dao_malloc( sizeof(dao_complex) );
	res->real = other->real;
	res->imag = other->imag;
	return res;
}
static void DMap_CopyItem( void **dest, void *item, short type )
{
	int n = 2*sizeof(void*);
	if( *dest == NULL ){
		switch( type ){
		case DAO_DATA_COMPLEX : *dest =  dao_complex_new( (dao_complex*) item ); break;
		case DAO_DATA_STRING : *dest = DString_Copy( (DString*) item ); break;
		case DAO_DATA_LIST   : *dest = DList_Copy( (DList*) item ); break;
		case DAO_DATA_MAP    : *dest = DMap_Copy( (DMap*) item ); break;
		case DAO_DATA_VALUE  :
		case DAO_DATA_VALUE2 :
		case DAO_DATA_VALUE3 : DaoValue_Copy( (DaoValue*)item, (DaoValue**) dest ); break;
		case DAO_DATA_VOID2  : *dest = dao_malloc(n); memcpy(*dest, item, n); break;
		default : *dest = item; break;
		}
	}else{
		switch( type ){
		case DAO_DATA_COMPLEX : *(dao_complex*) (*dest) = *(dao_complex*) item; break;
		case DAO_DATA_STRING : DString_Assign( (DString*)(*dest), (DString*) item ); break;
		case DAO_DATA_LIST   : DList_Assign( (DList*)(*dest), (DList*) item ); break;
		case DAO_DATA_MAP    : DMap_Assign( (DMap*)(*dest), (DMap*) item ); break;
		case DAO_DATA_VALUE  :
		case DAO_DATA_VALUE2 :
		case DAO_DATA_VALUE3 : DaoValue_Copy( (DaoValue*) item, (DaoValue**) dest ); break;
		case DAO_DATA_VOID2  : memcpy(*dest, item, n); break;
		default : *dest = item; break;
		}
	}
}
static void DMap_DeleteItem( void *item, short type )
{
	switch( type ){
	case DAO_DATA_COMPLEX : dao_free( item ); break;
	case DAO_DATA_STRING : DString_Delete( (DString*) item ); break;
	case DAO_DATA_LIST   : DList_Delete( (DList*) item ); break;
	case DAO_DATA_MAP    : DMap_Delete( (DMap*) item ); break;
	case DAO_DATA_VALUE  :
	case DAO_DATA_VALUE2 :
	case DAO_DATA_VALUE3 : GC_DecRC( (DaoValue*) item ); break;
	case DAO_DATA_VOID2  : dao_free( item ); break;
	default : break;
	}
}
static void DMap_BufferNode( DMap *self, DNode *node )
{
	node->parent = node->left = node->right = NULL;
	if( self->keytype >= DAO_DATA_VALUE && self->keytype <= DAO_DATA_VALUE3 ){
		DaoValue_Clear( & node->key.pValue );
	}
	if( self->valtype >= DAO_DATA_VALUE && self->valtype <= DAO_DATA_VALUE3 ){
		DaoValue_Clear( & node->value.pValue );
	}
	if( self->list == NULL ){
		self->list = node;
		return;
	}
	node->parent = self->list;
	self->list = node;
}
static void DMap_BufferTree( DMap *self, DNode *node )
{
	if( node == NULL ) return;
	DMap_BufferTree( self, node->left );
	DMap_BufferTree( self, node->right );
	DMap_BufferNode( self, node );
}
static void DMap_DeleteNode( DMap *self, DNode *node )
{
	if( node->key.pVoid ) DMap_DeleteItem( node->key.pVoid, self->keytype );
	if( node->value.pVoid ) DMap_DeleteItem( node->value.pVoid, self->valtype );
	dao_free( node );
}
static void DMap_DeleteTree( DMap *self, DNode *node )
{
	if( node == NULL ) return;
	DMap_DeleteTree( self, node->left );
	DMap_DeleteTree( self, node->right );
	DMap_DeleteNode( self, node );
}
void DMap_Clear( DMap *self )
{
	daoint i;
	if( self->hashing ){
		for(i=0; i<self->tsize; i++) DMap_DeleteTree( self, self->table[i] );
		if( DMap_Lockable( self ) ) DaoGC_LockData();
		if( self->table ) dao_free( self->table );
		self->tsize = 4;
		self->table = (DNode**) dao_calloc( self->tsize, sizeof(DNode*) );
		if( DMap_Lockable( self ) ) DaoGC_UnlockData();
	}else{
		DMap_DeleteTree( self, self->root );
	}
	self->root = NULL;
	self->size = 0;
}
void DMap_Reset( DMap *self )
{
	daoint i;
	if( DMap_Lockable( self ) ) DaoGC_LockData();
	if( self->hashing ){
		for(i=0; i<self->tsize; i++) DMap_BufferTree( self, self->table[i] );
		memset( self->table, 0, self->tsize*sizeof(DNode*) );
	}else{
		DMap_BufferTree( self, self->root );
	}
	self->root = NULL;
	self->size = 0;
	if( DMap_Lockable( self ) ) DaoGC_UnlockData();
}

static void DMap_RotateLeft( DMap *self, DNode *child )
{
	DNode *grandpa = child->parent;
	DNode *parent = child->right;

	if( grandpa ){
		if( child == grandpa->right )
			grandpa->right = parent;
		else
			grandpa->left = parent;
	}else{
		self->root = parent;
	}
	parent->parent = grandpa;

	child->right = parent->left;
	if( child->right ) child->right->parent = child;

	parent->left = child;
	child->parent = parent;
}
static void DMap_RotateRight( DMap *self, DNode *parent )
{
	DNode *grandpa = parent->parent;
	DNode *child = parent->left;

	if( grandpa ){
		if( parent == grandpa->right )
			grandpa->right = child;
		else
			grandpa->left = child;
	}else{
		self->root = child;
	}
	child->parent = grandpa;

	parent->left = child->right;
	if( parent->left ) parent->left->parent = parent;

	child->right = parent;
	parent->parent = child;
}
void DMap_InsertNode( DMap *self, DNode *node )
{
	DNode *grandpa, *parent, *uncle;

	node->color = RB_RED;
	self->size ++;

	while( node->parent != NULL ){
		parent = node->parent;
		grandpa = parent->parent;
		if( parent->color == RB_RED ){ /* insert_case2() */
			/* grandpa can't be NULL, since parent is RED and can't be root. */
			uncle = ( parent == grandpa->left ? grandpa->right : grandpa->left );
			if( uncle != NULL && uncle->color == RB_RED ){ /* insert_case3(): */
				parent->color = RB_BLACK;
				uncle->color  = RB_BLACK;
				grandpa->color = RB_RED;
				node = grandpa;
				continue; /* insert_case1() */
			}else{
				if( node == parent->right && parent == grandpa->left ){
					DMap_RotateLeft( self, parent );
					node = node->left;
				}else if( node == parent->left && parent == grandpa->right ){
					/* rotate right around parent: */
					DMap_RotateRight( self, parent );
					node = node->right;
				}
				/* node changed, update parent and grandpa. */
				parent = node->parent;
				grandpa = parent->parent;
				/* insert_case5() */

				parent->color = RB_BLACK;
				grandpa->color = RB_RED;
				if( node == parent->left && parent == grandpa->left )
					DMap_RotateRight( self, grandpa );
				else
					DMap_RotateLeft( self, grandpa );
			}
		}
		break;
	}
	/* insert_case1() as in Wikipedia term: Red-black tree. */
	if( node->parent == NULL){
		self->root = node;
		node->color = RB_BLACK;
	}
}
static void DMap_EraseChild( DMap *self, DNode *node )
{
	DNode *extreme = node;
	DNode *child = 0;

	if( node == NULL ) return;
	self->size --;

	/* deletion by coping */
	if( node->left ){
		extreme = node->left;
		while( extreme->right ) extreme = extreme->right;
		child = extreme->left;
	}else if( node->right ){
		extreme = node->right;
		while( extreme->left ) extreme = extreme->left;
		child = extreme->right;
	}
	DMap_SwapNode( self, node, extreme );

	if( child ){
		/* replace node */
		child->parent = extreme->parent;
		if( extreme->parent ){
			if( extreme == extreme->parent->left )
				extreme->parent->left = child;
			else
				extreme->parent->right = child;
		}
		if( extreme->color == RB_BLACK ){
			if( child->color == RB_RED )
				child->color = RB_BLACK;
			else{
				node = child;
				while( node->parent ){ /* delete_case1() */

					DNode *parent = node->parent;
					DNode *sibling = ( node == parent->left ? parent->right : parent->left );
					if( sibling && sibling->color == RB_RED ){ /* delete_case2() */
						parent->color = RB_RED;
						sibling->color = RB_BLACK;
						if( node == parent->left )
							DMap_RotateLeft( self, parent );
						else
							DMap_RotateRight( self, parent );
					}
					/* node relationship changed, update parent and sibling: */
					parent = node->parent;
					sibling = ( node == parent->left ? parent->right : parent->left );
					if( ! sibling ) break;
					/* delete_case3() */
					if( parent->color == RB_BLACK && sibling->color == RB_BLACK
							&& ( ! sibling->left || sibling->left->color == RB_BLACK )
							&& ( ! sibling->right|| sibling->right->color == RB_BLACK)){
						sibling->color = RB_RED;
						node = node->parent;
						continue; /* delete_case1() */
					}else{
						/* delete_case4() */
						if( parent->color == RB_RED && sibling->color == RB_BLACK
								&& ( ! sibling->left || sibling->left->color == RB_BLACK )
								&& ( ! sibling->right|| sibling->right->color == RB_BLACK)){
							sibling->color = RB_RED;
							parent->color = RB_BLACK;
						}else{
							/* delete_case5() */
							if( node == parent->left && sibling->color == RB_BLACK
									&&( sibling->left && sibling->left->color == RB_RED )
									&&( !sibling->right|| sibling->right->color == RB_BLACK)){
								sibling->color = RB_RED;
								sibling->left->color = RB_BLACK;
								DMap_RotateRight( self, sibling );
							}else if( node == parent->right && sibling->color == RB_BLACK
									&&( sibling->right && sibling->right->color == RB_RED )
									&&( !sibling->left || sibling->left->color == RB_BLACK)){
								sibling->color = RB_RED;
								sibling->right->color = RB_BLACK;
								DMap_RotateLeft( self, sibling );
							}
							/* node relationship changed, update parent and sibling: */
							parent = node->parent;
							sibling = ( node==parent->left ? parent->right:parent->left );
							/* delete_case6() */
							sibling->color = parent->color;
							parent->color = RB_BLACK;
							if( node == parent->left ){
								sibling->right->color = RB_BLACK;
								DMap_RotateLeft( self, parent );
							}else{
								sibling->left->color = RB_BLACK;
								DMap_RotateRight( self, parent );
							}
						} /* end if */
					} /* end if */
				} /* end while */
			}
		}
	}else if( extreme->parent ){
		if( extreme == extreme->parent->left )
			extreme->parent->left = NULL;
		else
			extreme->parent->right = NULL;
	}else{
		self->root = NULL;
	}
	DMap_BufferNode( self, extreme );
}
void DMap_EraseNodePro( DMap *self, DNode *node, DaoProcess *process )
{
	if( node == NULL ) return;
	if( self->hashing ){
		int hash = node->hash;
		self->root = self->table[ hash % self->tsize ];
		if( self->root == NULL ) return;
		if( DMap_Lockable( self ) ) DaoGC_LockData();
		DMap_EraseChild( self, node );
		self->table[ hash % self->tsize ] = self->root;
		if( DMap_Lockable( self ) ) DaoGC_UnlockData();
		if( self->size < 0.25*self->tsize ) DHash_ResetTable( self, process );
	}else{
		if( DMap_Lockable( self ) ) DaoGC_LockData();
		DMap_EraseChild( self, node );
		if( DMap_Lockable( self ) ) DaoGC_UnlockData();
	}
}
static daoint DList_Compare( DList *k1, DList *k2 )
{
	daoint i = 0, n = k1->size;
	daoint *v1 = k1->items.pInt;
	daoint *v2 = k2->items.pInt;
	if( n != k2->size ) return (int)(n - k2->size);
	while( i < n && v1[i] == v2[i] ) i += 1;
	if( i < n ) return v1[i] - v2[i];
	return 0;
}
static daoint DVoid2_Compare( void **k1, void **k2 )
{
	if( k1[0] != k2[0] ) return (daoint)k1[0] - (daoint)k2[0];
	return (daoint)k1[1] - (daoint)k2[1];
}
static daoint DaoVmCode_Compare( DaoVmCode *k1, DaoVmCode *k2 )
{
	if( k1->code != k2->code ) return k1->code - k2->code;
	if( k1->a != k2->a ) return k1->a - k2->a;
	if( k1->b != k2->b ) return k1->b - k2->b;
	return k1->c - k2->c;
}
static daoint DaoVmCode_Compare2( DaoVmCode *k1, DaoVmCode *k2 )
{
	if( k1->code != k2->code ) return k1->code - k2->code;
	if( k1->a != k2->a ) return k1->a - k2->a;
	return k1->b - k2->b;
}

extern int DaoArray_Compare( DaoArray *x, DaoArray *y );

static int DaoValue_Compare2( DaoValue *left, DaoValue *right )
{
	if( left == right ) return 0;
	if( left == NULL || right == NULL ) return left < right ? -100 : 100;
	if( left->type != right->type ) return left->type < right->type ? -100 : 100;
	if( left->type == DAO_TUPLE && left->xTuple.ctype == right->xTuple.ctype ){
		return DaoValue_Compare( left, right );
#ifdef DAO_WITH_NUMARRAY
	}else if( left->type == DAO_ARRAY && left->xArray.etype == right->xArray.etype ){
		return DaoArray_Compare( (DaoArray*) left, (DaoArray*) right );
#endif
	}else if( left->type == DAO_LIST && left->xList.ctype == right->xList.ctype ){
		return DaoValue_Compare( left, right );
	}
	if( left->type <= DAO_STRING ) return DaoValue_Compare( left, right );
	return left < right ? -100 : 100;
}
static int DaoValue_Compare3( DaoValue *left, DaoValue *right )
{
	if( left == right ) return 0;
	if( left == NULL || right == NULL ) return left < right ? -100 : 100;
	if( left->type != right->type ) return left->type < right->type ? -100 : 100;
	if( left->type <= DAO_STRING ) return DaoValue_Compare( left, right );
	return left < right ? -100 : 100;
}
static int dao_complex_compare( dao_complex *left, dao_complex *right )
{
	if( left->real != right->real ) return left->real < right->real ? -1 : 1;
	if( left->imag != right->imag ) return left->imag < right->imag ? -1 : 1;
	return 0;
}
static daoint DMap_CompareKeys( DMap *self, DNode *n1, DNode *n2, DaoProcess *proc )
{
	void *k1 = n1->key.pVoid;
	void *k2 = n2->key.pVoid;
	daoint cmp = 0;
	if( self->hashing && n1->hash != n2->hash ) return n1->hash < n2->hash ? -1 : 1;
	switch( self->keytype ){
	case DAO_DATA_COMPLEX : cmp = dao_complex_compare( (dao_complex*) k1, (dao_complex*) k2 ); break;
	case DAO_DATA_STRING : cmp = DString_Compare( (DString*) k1, (DString*) k2 ); break;
	case DAO_DATA_VALUE  : cmp = DaoValue_ComparePro( (DaoValue*)k1, (DaoValue*)k2, proc ); break;
	case DAO_DATA_VALUE2 : cmp = DaoValue_Compare2( (DaoValue*) k1, (DaoValue*) k2 ); break;
	case DAO_DATA_VALUE3 : cmp = DaoValue_Compare3( (DaoValue*) k1, (DaoValue*) k2 ); break;
	case DAO_DATA_LIST  : cmp = DList_Compare( (DList*) k1, (DList*) k2 );        break;
	case DAO_DATA_VOID2  : cmp = DVoid2_Compare( (void**) k1, (void**) k2 );          break;
	case DAO_DATA_VMCODE : cmp = DaoVmCode_Compare( (DaoVmCode*) k1, (DaoVmCode*) k2 );  break;
	case DAO_DATA_VMCODE2: cmp = DaoVmCode_Compare2( (DaoVmCode*) k1, (DaoVmCode*) k2 ); break;
	default : cmp = k1 == k2 ? 0 : (k1 < k2 ? -1 : 1 ); break;
	}
	return cmp;
}

static DNode* DMap_FindChild( DMap *self, DNode *root, DNode *query, int type, DaoProcess *process )
{
	DNode *p = root;
	DNode *m = 0;
	daoint compare;

	if( root == NULL ) return NULL;

	for(;;){
		compare = DMap_CompareKeys( self, query, p, process );
		if( compare == 0 ) return p;

		if( compare < 0 ){
			if( type == DAO_KEY_GE ) m = p;
			if( p->left ) p = p->left; else break;
		}else{
			if( type == DAO_KEY_LE ) m = p;
			if( p->right ) p = p->right; else break;
		}
	}
	return m;
}
static DNode* DMap_FindNode( DMap *self, void *key, int type, DaoProcess *process )
{
	DNode query = {0, 0, NULL, NULL};
	DNode *root = self->root;
	int hash = 0;
	query.key.pVoid = key;
	if( self->hashing ){
		hash = DHash_HashIndex( self, key, process );
		query.hash = hash;
		root = self->table[hash % self->tsize];
		if( root == NULL ) return NULL;
	}
	return DMap_FindChild( self, root, & query, type, process );
}
static DNode* DMap_SimpleInsert( DMap *self, DNode *node, DaoProcess *process )
{
	DNode *p = self->root;
	int compare;
	node->color = RB_RED;
	if( self->root == NULL ) return node;
	for(;;){
		compare = DMap_CompareKeys( self, node, p, process );
		if( compare == 0 ){
			return p;
		}else if( compare < 0 ){
			if( p->left ){
				p = p->left;
			}else{
				p->left = node;
				node->parent = p;
				break;
			}
		}else{
			if( p->right ){
				p = p->right;
			}else{
				p->right = node;
				node->parent = p;
				break;
			}
		}
	}
	return node;
}
DNode* DMap_InsertPro( DMap *self, void *key, void *value, DaoProcess *process )
{
	DNode *p, *node = DNode_New( self, self->keytype, self->valtype );
	void *okey = node->key.pVoid;
	void *ovalue = node->value.pVoid;
	int locked, id = 0;
	if( self->hashing ){
		id = DHash_HashIndex( self, key, process );
		node->hash = id;
		self->root = self->table[id % self->tsize];
		if( self->root ==NULL ){
			self->size += 1;
			self->table[id % self->tsize] = node;
			node->color = RB_BLACK;
			DMap_CopyItem( & node->key.pVoid, key, self->keytype );
			DMap_CopyItem( & node->value.pVoid, value, self->valtype );
			return node;
		}
	}
	node->key.pVoid = key;
	node->value.pVoid = value;
	p = DMap_SimpleInsert( self, node, process );
	node->key.pVoid = okey;
	node->value.pVoid = ovalue;
	if( p == node ){ /* key not exist: */
		DMap_CopyItem( & node->key.pVoid, key, self->keytype );
		DMap_CopyItem( & node->value.pVoid, value, self->valtype );
		if( DMap_Lockable( self ) ) DaoGC_LockData();
		DMap_InsertNode( self, node );
		if( DMap_Lockable( self ) ) DaoGC_UnlockData();
		if( self->hashing ){
			self->table[id % self->tsize] = self->root;
			if( self->size >= self->tsize ) DHash_ResetTable( self, process );
		}
	}else{
		if( self->valtype < DAO_DATA_VALUE || self->valtype > DAO_DATA_VALUE3 ){
			DMap_DeleteItem( p->value.pVoid, self->valtype );
			p->value.pVoid = NULL;
		}
		DMap_CopyItem( & p->value.pVoid, value, self->valtype );
		DMap_BufferNode( self, node );
	}
	return p;
}
void DMap_ErasePro( DMap *self, void *key, DaoProcess *process )
{
	DMap_EraseNodePro( self, DMap_FindNode( self, key, DAO_KEY_EQ, process ), process );
}
DNode* DMap_FindPro( DMap *self, void *key, int type, DaoProcess *process )
{
	return DMap_FindNode( self, key, type, process );
}
void DMap_EraseNode( DMap *self, DNode *node )
{
	return DMap_EraseNodePro( self, node, NULL );
}
DNode* DMap_Insert( DMap *self, void *key, void *value )
{
	return DMap_InsertPro( self, key, value, NULL );
}
void DMap_Erase( DMap *self, void *key )
{
	DMap_ErasePro( self, key, NULL );
}
DNode* DMap_Find( DMap *self, void *key )
{
	return DMap_FindNode( self, key, DAO_KEY_EQ, NULL );
}
DNode* DMap_First( DMap *self )
{
	DNode *node = NULL;
	daoint i = 0;
	if( self == NULL ) return NULL;
	if( self->hashing ){
		while( i < self->tsize && self->table[i] == NULL ) i += 1;
		if( i < self->tsize ) node = DNode_First( self->table[i] );
	}
	if( node == NULL && self->root ) node = DNode_First( self->root );
	return node;
}
DNode* DMap_Next( DMap *self, DNode *node )
{
	DNode *next;
	if( node == NULL ) return NULL;
	next = DNode_Next( node );
	if( next == NULL && self->hashing ){
		daoint i = node->hash % self->tsize + 1;
		while( i < self->tsize && self->table[i] == NULL ) i += 1;
		if( i < self->tsize ) next = DNode_First( self->table[i] );
	}
	return next;
}


/*
//  MurmurHash3, by Austin Appleby
//  PUBLIC DOMAIN CODES
//  http://sites.google.com/site/murmurhash/
//  http://code.google.com/p/smhasher/
//  http://www.burtleburtle.net/bob/hash/doobs.html
*/

#define ROTL32(x,r) ((x << r) | (x >> (32 - r)))

unsigned int Dao_Hash( const void *key, int len, unsigned int seed )
{
	const int nblocks = len / 4;
	const uchar_t *data = (const uchar_t*)key;
	const uchar_t *tail = (const uchar_t*)(data + nblocks*4);
	const uint_t *blocks = (const uint_t*)(data + nblocks*4);
	const uint_t c1 = 0xcc9e2d51;
	const uint_t c2 = 0x1b873593;
	uint_t h1 = seed;
	uint_t k1 = 0;
	int i;

	/*
	// Notes:
	// Emscripten compiler appears to assume that the address "blocks" is aligned at
	// four-byte (size of uint_t) boundary, and generates codes like HEAP32[$X>>2]|0.
	// But in Dao, it is possible to pass arbitrary address to this hash function.
	// For example, a string may hold "Exception::Error", and a temporary substring
	// "Error" may be passed in for hashing. If "Exception::Error" is aligned at
	// four-byte boundary, "Error" will not be!
	*/

	/* body: */
	for(i = -nblocks; i; i++){
#ifdef DAO_BUILD_JS_TARGET
		const uchar_t *b = tail + 4 * i;
		k1 = b[0] | (b[1]<<8) | (b[2]<<16) | (b[3]<<24);
#else
		k1 = blocks[i];
#endif

		k1 *= c1;
		k1 = ROTL32(k1,15);
		k1 *= c2;

		h1 ^= k1;
		h1 = ROTL32(h1,13); 
		h1 = h1*5+0xe6546b64;
	}

	/* tail: */
	switch(len & 3){
	case 3: k1 ^= tail[2] << 16;
	case 2: k1 ^= tail[1] << 8;
	case 1: k1 ^= tail[0]; k1 *= c1; k1 = ROTL32(k1,15); k1 *= c2; h1 ^= k1;
	}

	/* finalization: */
	h1 ^= len;
	h1 ^= h1 >> 16;
	h1 *= 0x85ebca6b;
	h1 ^= h1 >> 13;
	h1 *= 0xc2b2ae35;
	h1 ^= h1 >> 16;
	return h1;
}
