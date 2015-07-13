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

#ifndef DAO_LIST_H
#define DAO_LIST_H

#include<limits.h>
#include"daoBase.h"

#ifndef DAO_LIST_ITEM_TYPES
#define DAO_LIST_ITEM_TYPES
#endif

#ifndef DAO_ARRAY_ITEM_TYPES
#define DAO_ARRAY_ITEM_TYPES
#endif

/* List of pointers or integers: */
struct DList
{
	union{
		daoint        *pInt;
		void         **pVoid;

		DaoValue     **pValue;
		DaoTuple     **pTuple;
		DaoClass     **pClass;
		DaoObject    **pObject;
		DaoInterface **pInter;
		DaoRoutine   **pRoutine;
		DaoCdata     **pCdata;
		DaoCtype     **pCtype;
		DaoType      **pType;
		DaoConstant  **pConst;
		DaoVariable  **pVar;
		DaoNamespace **pNS;

		DString      **pString;
		DArray       **pArray;
		DList        **pList;
		DMap         **pMap;
		DaoInode     **pInode;
		DaoCnode     **pCnode;
		DaoVmCodeX   **pVmc;
		DaoToken     **pToken;

		DAO_LIST_ITEM_TYPES

	} items;

	daoint    size;
	daoint    bufsize;
	ushort_t  offset;
	ushort_t  type; /* can be 0 (for integers or pointers), or, D_STRING, D_ARRAY, etc. */
};

DAO_DLL DList* DList_New( short type );
DAO_DLL DList* DList_Copy( DList *self );
DAO_DLL void DList_Delete( DList *self );
DAO_DLL void DList_Assign( DList *left, DList *right );
DAO_DLL void DList_Swap( DList *left, DList *right );
DAO_DLL void DList_Resize( DList *self, daoint size, void *val );
DAO_DLL void DList_Clear( DList *self );
DAO_DLL void DList_Insert( DList *self, void *val, daoint id );
DAO_DLL void DList_InsertList( DList *self, daoint at, DList *list, daoint id, daoint n );
DAO_DLL void DList_AppendList( DList *self, DList *list );
DAO_DLL void DList_Erase( DList *self, daoint start, daoint n );
DAO_DLL void* DList_PushFront( DList *self, void *val );
DAO_DLL void* DList_PushBack( DList *self, void *val );

DAO_DLL void* DList_PopFront( DList *self );
DAO_DLL void* DList_PopBack( DList *self );
DAO_DLL void* DList_Front( DList *self );
DAO_DLL void* DList_Back( DList *self );

#define DList_Append( self, val )   DList_PushBack( self, (void*)(daoint)(val) )
#define DList_Pop( self )           DList_PopBack( self )
#define DList_Top( self )           DList_Back( self )
#define DList_TopInt( self )        (self)->items.pInt[ (self)->size -1 ]
#define DList_Item( self, i )       (self)->items.pVoid[i]
#define DList_String( self, i )     (self)->items.pString[i]



/*
// Vector container for different data types/sizes:
// A typical use:
//   DArray *vector = DArray_New( sizeof(SomeType) );
//   SomeType *item = (SomeType*) DArray_Push( vector );
//   do-something-with-item;
*/
struct DArray
{
	union {
		void       *base;
		char       *chars;
		int        *ints;
		daoint     *daoints;
		float      *floats;
		double     *doubles;
		uint_t     *uints;
		uchar_t    *uchars;
		wchar_t    *wchars;
		ushort_t   *ushorts;
		DString    *strings;
		DaoToken   *tokens;
		DaoVmCode  *codes;
		DaoValue  **values;

		dao_complex  *complexes;

		DAO_ARRAY_ITEM_TYPES
	} data;

	daoint  size;      /* Number of data items in the vector; */
	daoint  capacity;  /* Total number of data items that can be stored in the vector; */
	short   stride;    /* Data item size in bytes; */
	short   type;      /* Reserved; */
};

DAO_DLL DArray* DArray_New( int stride );
DAO_DLL DArray* DArray_Copy( DArray *self );
DAO_DLL void DArray_Delete( DArray *self );
DAO_DLL void DArray_Clear( DArray *self );

DAO_DLL void DArray_Resize( DArray *self, daoint size );
DAO_DLL void DArray_Reserve( DArray *self, daoint size );
DAO_DLL void DArray_Reset( DArray *self, daoint size );

DAO_DLL void DArray_Assign( DArray *left, DArray *right );

DAO_DLL void* DArray_Insert( DArray *self, daoint i, daoint n );
DAO_DLL void* DArray_Push( DArray *self );
DAO_DLL void* DArray_Pop( DArray *self );
DAO_DLL void* DArray_Back( DArray *self );
DAO_DLL void* DArray_Get( DArray *self, daoint i );

DAO_DLL void DArray_Erase( DArray *self, daoint i, daoint n );

DAO_DLL int* DArray_PushInt( DArray *self, int value );
DAO_DLL daoint* DArray_PushDaoInt( DArray *self, daoint value );
DAO_DLL float* DArray_PushFloat( DArray *self, float value );
DAO_DLL ushort_t* DArray_PushUshort( DArray *self, ushort_t value );



#endif
