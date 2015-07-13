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

#ifndef DAO_INTERFACE_H
#define DAO_INTERFACE_H

#include"daoType.h"


/*
// Abstract Interface:
*/
struct DaoInterface
{
	DAO_VALUE_COMMON;

	int       derived;
	DaoType  *abtype;  /* type object for this interface; */
	DList    *supers;  /* parent interfaces; */
	DMap     *methods; /* DHash<DString*,DaoRoutine*>; */
	DMap     *concretes; /* the concrete interfaces for abstract interfaces; */
};

DaoInterface* DaoInterface_New( const char *name );
void DaoInterface_Delete( DaoInterface *self );

void DaoInterface_DeriveMethods( DaoInterface *self );

int DaoInterface_Bind( DList *pairs, DList *fails );
int DaoInterface_BindTo( DaoInterface *self, DaoType *type, DMap *binds );
DaoCinType* DaoInterface_GetConcrete( DaoInterface *self, DaoType *type );

int DaoType_MatchInterface( DaoType *self, DaoInterface *inter, DMap *binds );



/*
// Concrete Interface Type:
//
// interface SomeInterface for SomeTargetType {
//     routine SomeMethod(){
//     }
// }
//
// -- SomeInterface:
//    Abstract interface type whose methods must all be implemented here;
//
// -- SomeTargetType:
//    Target type which can be any type;
//
// -- SomeMethod:
//    Inside non-static methods, the implicit self parameter is automatically casted to
//    the target type.
//
// -- Type Names:
//    interface<SomeInterface<SomeTargetType>>: class  type for the concrete interface;
//    SomeInterface<SomeTargetType>           : object type for the concrete interface;
//
// -- Type Matching:
//    SomeTargetType can match to both SomeInterface and SomeInterface<SomeTargetType>;
//    SomeInterface<SomeTargetType> can match to SomeInterface;
//
// -- Type Casting:
//    Assignment and moving between SomeTargetType and SomeInterface will convert
//    one to the other automatically with implicit type casting.
//
// -- Inheritance:
//    interface SubInterface for SomeTargetType : SomeInterface<SomeTargetType> {
//        routine SomeMethod2(){
//        }
//    }
//    The abstract interface SubInterface must be derived from SomeInterface.
//    The target types must be the same.
*/
struct DaoCinType
{
	DAO_VALUE_COMMON;

	int       derived;
	DaoType  *citype;  /* concrete interface type type; */
	DaoType  *vatype;  /* concrete interface value type; */
	DList    *supers;  /* parent interface classes; */
	DMap     *methods; /* DHash<DString*,DaoRoutine*>; */

	DaoType       *target;    /* target type; */
	DaoInterface  *abstract;  /* abstract interface; */
};

DaoCinType* DaoCinType_New( DaoInterface *inter, DaoType *target );
void DaoCinType_Delete( DaoCinType *self );

void DaoCinType_DeriveMethods( DaoCinType *self );



/*
// Concrete Interface Value:
*/
struct DaoCinValue
{
	DAO_VALUE_COMMON;

	DaoCinType  *cintype;
	DaoValue    *value;
};
DaoCinValue* DaoCinValue_New( DaoCinType *cintype, DaoValue *value );
void DaoCinValue_Delete( DaoCinValue *self );


#endif
