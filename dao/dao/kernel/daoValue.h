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

#ifndef DAO_VALUE_H
#define DAO_VALUE_H

#include"daoType.h"
#include"daoStdtype.h"
#include"daoNumtype.h"
#include"daoStream.h"
#include"daoClass.h"
#include"daoObject.h"
#include"daoInterface.h"
#include"daoRoutine.h"
#include"daoNamespace.h"
#include"daoProcess.h"




union DaoValue
{
	uchar_t        type;
	DaoNone        xBase;
	DaoNone        xNone;
	DaoBoolean     xBoolean;
	DaoInteger     xInteger;
	DaoFloat       xFloat;
	DaoComplex     xComplex;
	DaoString      xString;
	DaoEnum        xEnum;
	DaoArray       xArray;
	DaoList        xList;
	DaoMap         xMap;
	DaoTuple       xTuple;
	DaoStream      xStream;
	DaoObject      xObject;
	DaoCdata       xCstruct;
	DaoCdata       xCdata;
	DaoCtype       xCtype;
	DaoClass       xClass;
	DaoInterface   xInterface;
	DaoCinType     xCinType;
	DaoCinValue    xCinValue;
	DaoRoutine     xRoutine;
	DaoProcess     xProcess;
	DaoNamespace   xNamespace;
	DaoNameValue   xNameValue;
	DaoConstant    xConst;
	DaoVariable    xVar;
	DaoType        xType;

	struct {
		uchar_t  type;
		uchar_t  subtype;
		uchar_t  trait;
		uchar_t  work  : 1; /* mark objects in the work buffer; */
		uchar_t  alive : 1; /* mark alive objects (scanned for reachable objects); */
		uchar_t  delay : 1; /* mark objects in the delayed list; */
		uchar_t  dummy : 5;
		int  refCount;
		int  cycRefCount;
	} xGC;
};

DAO_DLL DaoType* DaoValue_GetType( DaoValue *self );

/*
// Copy when self is a simple data type (with type <= DAO_ENUM),
// or it is a constant array, list, map or tuple.
*/
DAO_DLL DaoValue* DaoValue_SimpleCopy( DaoValue *self );
DAO_DLL DaoValue* DaoValue_SimpleCopyWithType( DaoValue *self, DaoType *type );
DAO_DLL DaoValue* DaoValue_CopyContainer( DaoValue *self, DaoType *tp );

DAO_DLL void DaoValue_Clear( DaoValue **self );

DAO_DLL int DaoValue_ComparePro( DaoValue *left, DaoValue *right, DaoProcess *process );
DAO_DLL int DaoValue_Compare( DaoValue *left, DaoValue *right );

DAO_DLL void DaoValue_Assign( DaoValue *src, DaoValue **dest );
DAO_DLL void DaoValue_Copy( DaoValue *src, DaoValue **dest );
DAO_DLL void DaoValue_CopyX( DaoValue *src, DaoValue **dest, DaoType *cst );
DAO_DLL int DaoValue_Move( DaoValue *src, DaoValue **dest, DaoType *destype );
DAO_DLL int DaoValue_Move2( DaoValue *src, DaoValue **dest, DaoType *destype, DMap *defs );

DAO_DLL void DaoValue_SetType( DaoValue *to, DaoType *tp );
DAO_DLL void DaoValue_MarkConst( DaoValue *self );

DAO_DLL int DaoValue_IsZero( DaoValue *self );
DAO_DLL dao_integer DaoValue_GetInteger( DaoValue *self );
DAO_DLL dao_float   DaoValue_GetFloat( DaoValue *self );
DAO_DLL dao_complex DaoValue_GetComplex( DaoValue *self );
DAO_DLL DString* DaoValue_GetString( DaoValue *self, DString *str );

DAO_DLL int DaoValue_IsNumber( DaoValue *self );

DAO_DLL void DaoValue_GetField( DaoValue *self, DaoProcess *proc, DString *name );
DAO_DLL void DaoValue_SetField( DaoValue *self, DaoProcess *proc, DString *name, DaoValue *value );
DAO_DLL void DaoValue_GetItem( DaoValue *self, DaoProcess *proc, DaoValue *pid[], int N );
DAO_DLL void DaoValue_SetItem( DaoValue *self, DaoProcess *proc, DaoValue *pid[], int N, DaoValue *value );
DAO_DLL void DaoValue_Print( DaoValue *self, DaoProcess *proc, DaoStream *stream, DMap *cycData );

DaoTypeBase* DaoValue_GetTyper( DaoValue *p );

#endif
