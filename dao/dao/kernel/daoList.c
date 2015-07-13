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
#include"assert.h"

#include"daoList.h"
#include"daoType.h"
#include"daoValue.h"
#include"daoNumtype.h"
#include"daoParser.h"
#include"daoGC.h"


int daoCountArray = 0;

DList* DList_New( short type )
{
#ifdef DAO_USE_GC_LOGGER
	daoCountArray ++;
#endif
	DList *self = (DList*)dao_malloc( sizeof(DList) );
	self->items.pVoid = NULL;
	self->size = self->bufsize = 0;
	self->offset = 0;
	self->type = type;
	return self;
}
void DList_Delete( DList *self )
{
#ifdef DAO_USE_GC_LOGGER
	daoCountArray --;
#endif
	DList_Clear( self );
	dao_free( self );
}


static DaoVmCodeX* DaoVmCodeX_Copy( DaoVmCodeX *self )
{
	DaoVmCodeX* copy = (DaoVmCodeX*) dao_malloc( sizeof(DaoVmCodeX) );
	memcpy( copy, self, sizeof(DaoVmCodeX) );
	return copy;
}
static void DaoVmCodeX_Delete( DaoVmCodeX *self )
{
	dao_free( self );
}
static void* DList_CopyItem( DList *self, void *item )
{
	DaoValue *v;
	if( item == NULL ) return NULL;
	switch( self->type ){
	case DAO_DATA_VALUE  : v = DaoValue_SimpleCopy( (DaoValue*) item ); GC_IncRC( v ); return v;
	case DAO_DATA_VMCODE : return DaoVmCodeX_Copy( (DaoVmCodeX*) item );
	case DAO_DATA_TOKEN  : return DaoToken_Copy( (DaoToken*) item );
	case DAO_DATA_STRING : return DString_Copy( (DString*) item );
	case DAO_DATA_ARRAY  : return DArray_Copy( (DArray*) item );
	case DAO_DATA_LIST   : return DList_Copy( (DList*) item );
	case DAO_DATA_MAP    : return DMap_Copy( (DMap*) item );
	default : break;
	}
	return item;
}
static void DList_DeleteItem( DList *self, void *item )
{
	switch( self->type ){
	case DAO_DATA_VALUE  : GC_DecRC( item ); break;
	case DAO_DATA_VMCODE : DaoVmCodeX_Delete( (DaoVmCodeX*) item ); break;
	case DAO_DATA_TOKEN  : DaoToken_Delete( (DaoToken*) item ); break;
	case DAO_DATA_STRING : DString_Delete( (DString*) item ); break;
	case DAO_DATA_ARRAY  : DArray_Delete( (DArray*) item ); break;
	case DAO_DATA_LIST   : DList_Delete( (DList*) item ); break;
	case DAO_DATA_MAP    : DMap_Delete( (DMap*) item ); break;
	default : break;
	}
}
static void DList_DeleteItems( DList *self, daoint M, daoint N )
{
	daoint i;
	switch( self->type ){
	case DAO_DATA_VALUE  : for(i=M; i<N; i++) GC_DecRC( self->items.pValue[i] ); break;
	case DAO_DATA_VMCODE : for(i=M; i<N; i++) DaoVmCodeX_Delete( self->items.pVmc[i] ); break;
	case DAO_DATA_TOKEN  : for(i=M; i<N; i++) DaoToken_Delete( self->items.pToken[i] ); break;
	case DAO_DATA_STRING : for(i=M; i<N; i++) DString_Delete( self->items.pString[i] ); break;
	case DAO_DATA_ARRAY  : for(i=M; i<N; i++) DArray_Delete( self->items.pArray[i] ); break;
	case DAO_DATA_LIST   : for(i=M; i<N; i++) DList_Delete( self->items.pList[i] ); break;
	case DAO_DATA_MAP    : for(i=M; i<N; i++) DMap_Delete( self->items.pMap[i] ); break;
	default : break;
	}
}
void DList_Resize( DList *self, daoint size, void *val )
{
	void **buf = self->items.pVoid - self->offset;
	daoint i;

	if( size == self->size && self->bufsize>0 ) return;
	DList_DeleteItems( self, size, self->size );

	if( self->type == DAO_DATA_VALUE ) DaoGC_LockData();
	if( self->offset ){
		daoint min = size > self->size ? self->size : size;
		memmove( buf, self->items.pVoid, min*sizeof(void*) );
		self->items.pVoid = buf;
		self->offset = 0;
	}
	/* When resize() is called, probably this is the intended size,
	 * not to be changed frequently. */
	if( size >= self->bufsize || size < self->bufsize /2 ){
		self->bufsize = size;
		self->items.pVoid = (void**) dao_realloc( buf, self->bufsize*sizeof(void*) );
	}

	if( size < self->size ) self->size = size;
	if( self->type == DAO_DATA_VALUE ) DaoGC_UnlockData();

	if( self->type && val != NULL ){
		for(i=self->size; i<size; i++ ) self->items.pVoid[i] = DList_CopyItem( self, val );
	}else{
		for(i=self->size; i<size; i++ ) self->items.pVoid[i] = val;
	}
	self->size = size;
}
void DList_Clear( DList *self )
{
	void **buf = self->items.pVoid - self->offset;
	DList_DeleteItems( self, 0, self->size );
	if( self->type == DAO_DATA_VALUE ) DaoGC_LockData();
	if( buf ) dao_free( buf );
	self->items.pVoid = NULL;
	self->size = self->bufsize = 0;
	self->offset = 0;
	if( self->type == DAO_DATA_VALUE ) DaoGC_UnlockData();
}

DList* DList_Copy( DList *self )
{
	DList *copy = DList_New( self->type );
	DList_Assign( copy, self );
	return copy;
}
void DList_Assign( DList *left, DList *right )
{
	daoint i;
	assert( left->type == right->type || (left->type == DAO_DATA_VALUE && right->type == 0) );

	if( left == right ) return;
	if( right->size == 0 ){
		DList_Clear( left );
		return;
	}
	if( left->type ){
		DList_Clear( left);
		for( i=0; i<right->size; i++ ) DList_Append( left, right->items.pVoid[i] );
	}else{
		DList_Resize( left, right->size, NULL );
		if( left->type == DAO_DATA_VALUE ) DaoGC_LockData();
		for( i=0; i<right->size; i++ ) left->items.pVoid[i] = right->items.pVoid[i];
		if( left->type == DAO_DATA_VALUE ) DaoGC_UnlockData();
	}
}
void DList_Swap( DList *left, DList *right )
{
	daoint tmpSize = left->size;
	daoint tmpBufSize = left->bufsize;
	size_t tmpOffset = left->offset;
	void **tmpItem = left->items.pVoid;
	assert( left->type == right->type );
	assert( left->type != DAO_DATA_VALUE );
	if( left->type == DAO_DATA_VALUE ) DaoGC_LockData();
	left->size = right->size;
	left->offset = right->offset;
	left->bufsize = right->bufsize;
	left->items.pVoid = right->items.pVoid;
	right->size = tmpSize;
	right->offset = tmpOffset;
	right->bufsize = tmpBufSize;
	right->items.pVoid = tmpItem;
	if( left->type == DAO_DATA_VALUE ) DaoGC_UnlockData();
}
void DList_Insert( DList *self, void *val, daoint id )
{
	void **buf = self->items.pVoid - self->offset;
	daoint i;
	if( id == 0 ){
		DList_PushFront( self, val );
		return;
	}else if( id >= self->size ){
		DList_PushBack( self, val );
		return;
	}
	if( self->type == DAO_DATA_VALUE ) DaoGC_LockData();
	if( (daoint)(self->offset + self->size + 1) >= self->bufsize ){
		if( self->offset > 0 ) memmove( buf, self->items.pVoid, self->size*sizeof(void*) );
		self->bufsize += self->bufsize/5 + 5;
		self->items.pVoid = (void**) dao_realloc( buf, (self->bufsize+1)*sizeof(void*) );
		self->offset = 0;
	}
	if( self->type && val != NULL ){
		for( i=self->size; i>id; i-- ) self->items.pVoid[i] = self->items.pVoid[i-1];
		self->items.pVoid[ id ] = NULL;
	}else{
		for( i=self->size; i>id; i-- ) self->items.pVoid[i] = self->items.pVoid[i-1];
		self->items.pVoid[id] = val;
	}
	if( self->type == DAO_DATA_VALUE ) DaoGC_UnlockData();
	if( self->type && val != NULL ){
		self->items.pVoid[ id ] = DList_CopyItem( self, val );
	}
	self->size++;
}
void DList_InsertList( DList *self, daoint at, DList *list, daoint id, daoint n )
{
	void **buf = self->items.pVoid - self->offset;
	void **objs = list->items.pVoid;
	daoint i;
	assert( self->type == list->type );
	if( n < 0 ) n = list->size;
	n += id;
	if( n > list->size ) n = list->size;
	if( n ==0 || id >= list->size ) return;
	if( self->type == DAO_DATA_VALUE ) DaoGC_LockData();
	if( (daoint)(self->offset + self->size + n-id) >= self->bufsize ){
		if( self->offset > 0 ) memmove( buf, self->items.pVoid, self->size*sizeof(void*) );
		self->bufsize += self->bufsize/5 + 1 + ( n - id );
		self->items.pVoid = (void**) dao_realloc( buf, (self->bufsize+1)*sizeof(void*) );
		self->offset = 0;
	}
	if( at < self->size ){
		memmove( self->items.pVoid+at+(n-id), self->items.pVoid+at, (self->size-at)*sizeof(void*) );
		memset( self->items.pVoid + at, 0, (n-id)*sizeof(void*) );
	}
	if( self->type == DAO_DATA_VALUE ) DaoGC_UnlockData();
	if( self->type ){
		if( at >= self->size ){
			for(i=id; i<n; i++) self->items.pVoid[ self->size+i-id ] = DList_CopyItem( self, objs[i] );
		}else{
			for(i=id; i<n; i++) self->items.pVoid[ at+i-id ] = DList_CopyItem( self, objs[i] );
		}
	}else{
		if( at >= self->size ){
			for(i=id; i<n; i++) self->items.pVoid[ self->size+i-id ] = objs[i];
		}else{
			for(i=id; i<n; i++) self->items.pVoid[ at+i-id ] = objs[i];
		}
	}
	self->size += (n-id);
}
void DList_AppendList( DList *self, DList *list )
{
	DList_InsertList( self, self->size, list, 0, list->size );
}
void DList_Erase( DList *self, daoint start, daoint n )
{
	void **buf = self->items.pVoid - self->offset;
	daoint rest;
	if( start >= self->size ) return;
	if( n < 0 ) n = self->size;
	if( n > self->size - start ) n = self->size - start;
	if( n == 1 ){
		if( start == 0 ){
			DList_PopFront( self );
			return;
		}else if( start == self->size -1 ){
			DList_PopBack( self );
			return;
		}
	}

	DList_DeleteItems( self, start, start+n );
	if( self->type == DAO_DATA_VALUE ) DaoGC_LockData();
	rest = self->size - start - n;
	memmove( self->items.pVoid + start, self->items.pVoid + start + n, rest * sizeof(void*) );
	self->size -= n;
	if( self->size < 0.5*self->bufsize && self->size + 10 < self->bufsize ){
		if( self->offset ) memmove( buf, self->items.pVoid, self->size * sizeof(void*));
		self->bufsize = 0.6 * self->bufsize + 1;
		self->items.pVoid = (void**) dao_realloc( buf, (self->bufsize+1)*sizeof(void*) );
		self->offset = 0;
	}
	if( self->type == DAO_DATA_VALUE ) DaoGC_UnlockData();
}
void* DList_PushFront( DList *self, void *val )
{
	void **buf = self->items.pVoid - self->offset;
	if( self->type == DAO_DATA_VALUE ) DaoGC_LockData();
	if( self->offset > 0 ){
		/* make sure the concurrent gc won't access an invalid pointer: */
		self->items.pVoid[-1] = NULL;
		self->items.pVoid --;
	}else{
		size_t moffset = 0xffff;
		size_t offset = self->bufsize/5 + 5;
		self->offset = offset < moffset ? offset : moffset;
		self->bufsize += self->offset;
		buf = (void**) dao_realloc( buf, (self->bufsize+1)*sizeof(void*) );
		memmove( buf + self->offset, buf, self->size*sizeof(void*) );
		self->items.pVoid = buf + self->offset - 1;
	}
	if( self->type && val != NULL ){
		self->items.pVoid[0] = NULL;
	}else{
		self->items.pVoid[0] = val;
	}
	if( self->type == DAO_DATA_VALUE ) DaoGC_UnlockData();
	if( self->type && val != NULL ){
		self->items.pVoid[0] = DList_CopyItem( self, val );
	}
	self->size ++;
	self->offset --;
	return self->items.pVoid[0];
}
void* DList_PopFront( DList *self )
{
	void *ret, **buf = self->items.pVoid - self->offset;
	size_t moffset = 0xffff;
	if( self->size == 0 ) return NULL;

	ret = self->items.pVoid[0];
	if( self->type ) DList_DeleteItem( self, ret );

	if( self->type == DAO_DATA_VALUE ) DaoGC_LockData();
	self->size --;
	self->offset ++;
	self->items.pVoid ++;
	if( self->offset >= moffset ){
		self->offset /= 2;
		memmove( buf + self->offset, self->items.pVoid, self->size*sizeof(void*) );
		self->items.pVoid = buf + self->offset;
	}else if( self->size < 0.5 * self->bufsize && self->size + 10 < self->bufsize ){
		if( self->offset < 0.1 * self->bufsize ){ /* shrink from back */
			self->bufsize = 0.6 * self->bufsize + 1;
		}else{ /* shrink from front */
			self->offset = (size_t)(0.05 * self->bufsize);
			memmove( buf + self->offset, self->items.pVoid, self->size*sizeof(void*) );
		}
		buf = (void**) dao_realloc( buf, (self->bufsize+1)*sizeof(void*) );
		self->items.pVoid = buf + self->offset;
	}
	if( self->type == DAO_DATA_VALUE ) DaoGC_UnlockData();
	if( self->type ) return NULL;
	return ret;
}
void* DList_PushBack( DList *self, void *val )
{
	void **buf = self->items.pVoid - self->offset;
	if( self->type == DAO_DATA_VALUE ) DaoGC_LockData();
	if( (daoint)(self->offset + self->size + 1) >= self->bufsize ){
		self->bufsize += self->bufsize/5 + 5;
		buf = (void**) dao_realloc( buf, (self->bufsize+1)*sizeof(void*) );
		self->items.pVoid = buf + self->offset;
	}
	if( self->type && val != NULL ){
		self->items.pVoid[ self->size ] = NULL;
	}else{
		self->items.pVoid[ self->size ] = val;
	}
	if( self->type == DAO_DATA_VALUE ) DaoGC_UnlockData();
	if( self->type && val != NULL ){
		self->items.pVoid[ self->size ] = DList_CopyItem( self, val );
	}
	self->size++;
	return self->items.pVoid[ self->size - 1 ];
}
void* DList_PopBack( DList *self )
{
	void *ret, **buf = self->items.pVoid - self->offset;
	if( self->size == 0 ) return NULL;
	self->size --;
	ret = self->items.pVoid[ self->size ];
	if( self->type ) DList_DeleteItem( self, self->items.pVoid[ self->size ] );
	if( self->type == DAO_DATA_VALUE ) DaoGC_LockData();
	if( self->size < 0.5 * self->bufsize && self->size + 10 < self->bufsize ){
		if( self->offset < 0.1 * self->bufsize ){ /* shrink from back */
			self->bufsize = 0.6 * self->bufsize + 1;
		}else{ /* shrink from front */
			self->offset = (size_t)(0.05 * self->bufsize);
			memmove( buf + self->offset, self->items.pVoid, self->size*sizeof(void*) );
		}
		buf = (void**) dao_realloc( buf, (self->bufsize+1)*sizeof(void*) );
		self->items.pVoid = buf + self->offset;
	}
	if( self->type == DAO_DATA_VALUE ) DaoGC_UnlockData();
	if( self->type ) return NULL;
	return ret;
}
void  DList_SetItem( DList *self, daoint index, void *value )
{
	if( index >= self->size ) return;
	if( self->type == DAO_DATA_VALUE ) DaoGC_LockData();
	if( self->type && value ){
		self->items.pVoid[ index ] = DList_CopyItem( self, value );
	}else{
		self->items.pVoid[index] = value;
	}
	if( self->type == DAO_DATA_VALUE ) DaoGC_UnlockData();
}
void* DList_GetItem( DList *self, daoint index )
{
	if( index >= self->size ) return NULL;
	return self->items.pVoid[ index ];
}
void* DList_Front( DList *self )
{
	return DList_GetItem( self, 0 );
}
void* DList_Back( DList *self )
{
	if( self->size ==0 ) return NULL;
	return DList_GetItem( self, self->size-1 );
}
void** DList_GetBuffer( DList *self )
{
	return self->items.pVoid;
}





DArray* DArray_New( int stride )
{
	DArray *self = (DArray*) dao_calloc( 1, sizeof(DArray) );
	self->stride = stride;
	return self;
}
DArray* DArray_Copy( DArray *self )
{
	DArray *copy = DArray_New( self->stride );
	copy->type = self->type;
	DArray_Resize( copy, self->size );
	memcpy( copy->data.base, self->data.base, self->size * self->stride );
	return copy;
}

void DArray_Delete( DArray *self )
{
	if( self->data.base ) dao_free( self->data.base );
	dao_free( self );
}

void DArray_Clear( DArray *self )
{
	if( self->data.base ) dao_free( self->data.base );
	self->data.base = NULL;
	self->size = self->capacity = 0;
}
void DArray_Resize( DArray *self, daoint size )
{
	if( self->capacity != size ){
		self->capacity = size;
		self->data.base = dao_realloc( self->data.base, self->capacity*self->stride );
	}
	self->size = size;
}

void DArray_Reserve( DArray *self, daoint size )
{
	if( size <= self->capacity ) return;
	self->capacity = 1.2 * size + 4;
	self->data.base = dao_realloc( self->data.base, self->capacity*self->stride );
}

void DArray_Reset( DArray *self, daoint size )
{
	if( size <= self->capacity ){
		self->size = size;
		return;
	}
	DArray_Resize( self, size );
}

void* DArray_Get( DArray *self, daoint i )
{
	return self->data.chars + i * self->stride;
}

void DArray_Assign( DArray *left, DArray *right )
{
	assert( left->stride == right->stride );
	DArray_Resize( left, right->size );
	memcpy( left->data.base, right->data.base, right->size * right->stride );
}

void* DArray_Insert( DArray *self, daoint i, daoint n )
{
	char *data;

	if( i < 0 ) i += self->size;
	if( i < 0 || i > self->size ) return NULL;

	DArray_Reserve( self, self->size + n );

	data = self->data.chars + i * self->stride;
	memmove( data + n*self->stride, data, (self->size - i) *self->stride );

	self->size += n;
	return data;
}
void* DArray_Push( DArray *self )
{
	DArray_Reserve( self, self->size + 1 );
	self->size += 1;
	return self->data.chars + (self->size - 1) * self->stride;
}
void* DArray_Pop( DArray *self )
{
	if( self->capacity > (2*self->size + 1) ) DArray_Reserve( self, self->size + 1 );
	if( self->size == 0 ) return NULL;
	self->size -= 1;
	return self->data.chars + self->size * self->stride;
}
void* DArray_Back( DArray *self )
{
	if( self->size == 0 ) return NULL;
	return self->data.chars + (self->size - 1) * self->stride;
}
void DArray_Erase( DArray *self, daoint i, daoint n )
{
	char *src, *dest;

	if( n == 0 ) return;
	if( n < 0 ) n = self->size;
	if( i < 0 || i >= self->size ) return;

	if( (i + n) >= self->size ) n = self->size - i;

	dest = self->data.chars + i*self->stride;
	src = dest + n*self->stride;
	memmove( dest, src, (self->size - i - n)*self->stride );
	self->size -= n;
}

int* DArray_PushInt( DArray *self, int value )
{
	int *item = (int*) DArray_Push( self );
	*item = value;
	return item;
}
daoint* DArray_PushDaoInt( DArray *self, daoint value )
{
	daoint *item = (daoint*) DArray_Push( self );
	*item = value;
	return item;
}
float* DArray_PushFloat( DArray *self, float value )
{
	float *item = (float*) DArray_Push( self );
	*item = value;
	return item;
}
ushort_t* DArray_PushUshort( DArray *self, ushort_t value )
{
	ushort_t *item = (ushort_t*) DArray_Push( self );
	*item = value;
	return item;
}

DaoVmCode* DArray_PushCode( DArray *self, DaoVmCode code )
{
	DaoVmCode *code2 = (DaoVmCode*) DArray_Push( self );
	*code2 = code;
	return code2;
}
DaoToken* DArray_PushToken( DArray *self, DaoToken token )
{
	DaoToken *token2 = (DaoToken*) DArray_Push( self );
	*token2 = token;
	return token2;
}
