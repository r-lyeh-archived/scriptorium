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

#ifndef DAO_STDTYPE_H
#define DAO_STDTYPE_H

#include"daoConst.h"
#include"daoBase.h"
#include"daoString.h"
#include"daoList.h"
#include"daoMap.h"

#define DAO_VALUE_CORE      uchar_t type, subtype, trait, marks; int refCount
#define DAO_VALUE_COMMON    DAO_VALUE_CORE; int cycRefCount
#define DAO_GENERIC_COMMON  DAO_VALUE_COMMON; DaoType *ctype
#define DAO_CSTRUCT_COMMON  DAO_GENERIC_COMMON; DaoObject *object

void DaoValue_Init( void *dbase, char type );

struct DaoNone
{
	DAO_VALUE_CORE;
};
DAO_DLL DaoValue *dao_none_value;
DAO_DLL DaoValue *dao_false_value;
DAO_DLL DaoValue *dao_true_value;
DAO_DLL DaoNone* DaoNone_New();

struct DaoInteger
{
	DAO_VALUE_CORE;

	dao_integer value;
};

struct DaoFloat
{
	DAO_VALUE_CORE;

	dao_float value;
};

struct DaoComplex
{
	DAO_VALUE_CORE;

	dao_complex value;
};


struct DaoString
{
	DAO_VALUE_CORE;

	DString  *value;
};
DAO_DLL DaoString* DaoString_Copy( DaoString *self );
DAO_DLL void DaoString_Delete( DaoString *self );


/*
// Structure for symbol, enum, flag and bool:
// Storage modes:
// Symbol: $AA => { type<enum<AA>>, 0 }
// Symbols: $AA + $BB => { type<enum<AA;BB>>, 1|2 }
// Enum: enum MyEnum{ AA=1, BB=2 }, MyEnum.AA => { 1, type<MyEnum> }
// Flag: enum MyFlag{ AA=1; BB=2 }, MyFlag.AA + MyFlag.BB => { 1|2, type<MyFlag> }
*/
struct DaoEnum
{
	DAO_VALUE_COMMON;

	int       value; /* value associated with the symbol(s) or flag(s) */
	DaoType  *etype; /* type information structure */
};

DAO_DLL DaoEnum* DaoEnum_New( DaoType *type, int value );
DAO_DLL DaoEnum* DaoEnum_Copy( DaoEnum *self, DaoType *type );
DAO_DLL void DaoEnum_Delete( DaoEnum *self );
DAO_DLL void DaoEnum_MakeName( DaoEnum *self, DString *name );
DAO_DLL void DaoEnum_SetType( DaoEnum *self, DaoType *type );
DAO_DLL int DaoEnum_SetSymbols( DaoEnum *self, const char *symbols );
DAO_DLL int DaoEnum_SetValue( DaoEnum *self, DaoEnum *other );
DAO_DLL int DaoEnum_AddValue( DaoEnum *self, DaoEnum *other );
DAO_DLL int DaoEnum_RemoveValue( DaoEnum *self, DaoEnum *other );


struct DaoList
{
	DAO_GENERIC_COMMON;

	DList  *value;
};

DAO_DLL DaoList* DaoList_New();
DAO_DLL void DaoList_Delete( DaoList *self );
DAO_DLL void DaoList_Clear( DaoList *self );

DAO_DLL void DaoList_Erase( DaoList *self, daoint id );
DAO_DLL int DaoList_SetItem( DaoList *self, DaoValue *it, daoint id );
DAO_DLL int DaoList_Append( DaoList *self, DaoValue *it );

DAO_DLL DaoList* DaoList_Copy( DaoList *self, DaoType *type );


struct DaoMap
{
	DAO_GENERIC_COMMON;

	DMap  *value;
};

/*
// hashing = 0: no hashing;
// hashing = 1: hashing with the default seed;
// hashing > 1: hashing with "hashing" as the seed;
*/
DAO_DLL DaoMap* DaoMap_New( unsigned int hashing );
DAO_DLL void DaoMap_Delete( DaoMap *self );
DAO_DLL void DaoMap_Clear( DaoMap *self );
/*
// hashing = 0: no hashing;
// hashing = 1: hashing with the same seed;
// hashing > 1: hashing with "hashing" as the seed;
*/
DAO_DLL void DaoMap_Reset( DaoMap *self, unsigned int hashing );
DAO_DLL DaoMap* DaoMap_Copy( DaoMap *self, DaoType *type );

DAO_DLL DNode* DaoMap_Find2( DaoMap *self, DaoValue *key, DaoProcess *proc );
DAO_DLL int DaoMap_Insert( DaoMap *self, DaoValue *key, DaoValue *value );
DAO_DLL int DaoMap_Insert2( DaoMap *self, DaoValue *key, DaoValue *value, DaoProcess *proc );
DAO_DLL void DaoMap_Erase( DaoMap *self, DaoValue *key );


#define DAO_TUPLE_MINSIZE 2
/*
// 2 is used instead of 1, for two reasons:
// A. most often used tuples have at least two items;
// B. some builtin tuples have at least two items, and are accessed by
//    constant sub index, compilers such Clang may complain if 1 is used.
*/

struct DaoTuple
{
	DAO_VALUE_COMMON;

	/* packed with the previous field in 64-bits system; */
	int        size;
	DaoType   *ctype;
	DaoValue  *values[DAO_TUPLE_MINSIZE]; /* the actual number of items is in ::size; */
};

DAO_DLL DaoTuple* DaoTuple_Create( DaoType *type, int size, int init );
DAO_DLL DaoTuple* DaoTuple_Copy( DaoTuple *self, DaoType *type );
DAO_DLL void DaoTuple_Delete( DaoTuple *self );
DAO_DLL void DaoTuple_SetItem( DaoTuple *self, DaoValue *it, int pos );
DAO_DLL int DaoTuple_GetIndex( DaoTuple *self, DString *name );


/*
// Mainly used for passing named parameters and fields:
*/
struct DaoNameValue
{
	DAO_VALUE_COMMON;

	DString   *name;
	DaoValue  *value;
	DaoType   *ctype;
};
DaoNameValue* DaoNameValue_New( DString *name, DaoValue *value );




/* Customized/extended Dao data: */
struct DaoCstruct
{
	DAO_CSTRUCT_COMMON;
};

DAO_DLL void DaoCstruct_Init( DaoCstruct *self, DaoType *type );
DAO_DLL void DaoCstruct_Free( DaoCstruct *self );



/* Opaque C/C++ data: */
struct DaoCdata
{
	DAO_CSTRUCT_COMMON;

	void  *data;
};


DAO_DLL void DaoCdata_Delete( DaoCdata *self );


/*
// DaoCtype is the class struct for C/C++ types:
//
// Each C/C++ type is principly represented by three objects:
// -- A class object:  DaoCtype;
// -- A class type:    DaoType (DaoCtype::ctype);
// -- A instance type: DaoType (DaoCtype::cdtype);
*/
struct DaoCtype
{
	DAO_CSTRUCT_COMMON;

	DaoType  *cdtype;
	DString  *name;
	DString  *info;
	int       attribs;
};
DAO_DLL DaoCtype* DaoCtype_New( DaoType *cttype, DaoType *cdtype );
DAO_DLL void DaoCtype_Delete( DaoCtype *self );



struct DaoException
{
	DAO_CSTRUCT_COMMON;

	DList     *callers;
	DList     *lines;
	DString   *info;
	DaoValue  *data;
};

DaoException* DaoException_New( DaoType *type );
void DaoException_Delete( DaoException *self );
void DaoException_SetData( DaoException *self, DaoValue *data );

void DaoException_Setup( DaoNamespace *ns );
void DaoException_Init( DaoException *self, DaoProcess *proc, const char *info, DaoValue *dat );
void DaoException_Print( DaoException *self, DaoStream *stream );




struct DaoConstant
{
	DAO_VALUE_COMMON;

	DaoValue *value;
};

struct DaoVariable
{
	DAO_VALUE_COMMON;

	DaoValue *value;
	DaoType  *dtype;
};

DAO_DLL DaoConstant* DaoConstant_New( DaoValue *value, int subtype );
DAO_DLL DaoVariable* DaoVariable_New( DaoValue *value, DaoType *type, int subtype );

DAO_DLL void DaoConstant_Delete( DaoConstant *self );
DAO_DLL void DaoVariable_Delete( DaoVariable *self );

DAO_DLL void DaoConstant_Set( DaoConstant *self, DaoValue *value );
DAO_DLL int  DaoVariable_Set( DaoVariable *self, DaoValue *value, DaoType *type );
DAO_DLL void DaoVariable_SetType( DaoVariable *self, DaoType *type );


#endif
