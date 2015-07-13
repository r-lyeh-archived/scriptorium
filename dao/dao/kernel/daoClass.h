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

#ifndef DAO_CLASS_H
#define DAO_CLASS_H

#include"daoType.h"

#define DAO_CLASS_CONST_CSTOR  1

/*
// The DaoClass structure contains all the information for a Dao class.
//
// In the Dao type system, Dao class is represented by two DaoType objects:
// -- DaoClass::clsType: for the class object itself;
// -- DaoClass::objType: for the instance objects of the class;
//
// The class members can be looked up by DaoClass::lookupTable, which maps
// the member names to lookup indices.
//
// Bit structure for the lookup indices: EEPPSSSSUUUUUUUUIIIIIIIIIIIIIIII.
// Where:
// -- EE: 2 bits reserved for encoding error;
// -- PP: 2 bits to encode permission;
// -- SSSS: 4 bits to encode storage type;
// -- UUUUUUUU: 8 bits to encode up index;
// -- IIIIIIIIIIIIIIII: 16 bits to encode the actual index in field arrays;
//
// The permission bits will encode the following values:
// -- DAO_PERM_PRIVATE;
// -- DAO_PERM_PROTECTED;
// -- DAO_PERM_PUBLIC;
//
// The storage bits will encode the following values:
// -- DAO_CLASS_CONSTANT: for constants;
// -- DAO_CLASS_VARIABLE: for static variables;
// -- DAO_OBJECT_VARIABLE: for instance variables;
//
// The up-index bits will encode the following values:
// -- Zero: for members from the current class;
// -- One:  for members from the direct base class;
// -- Two and above: for members from the indirect base classes;
//
// Class constant layout in DaoClass:constants:
// -- Single Index (0): the class object itself, with its name ("X") as the lookup name;
// -- Single Index (1): the class constructor (implicit/explicit), with lookup name "X::X";
// -- Multiple Indices: the constants from the mixin component classes;
// -- Mulitple Indices: the constants from the base class;
// -- Mulitple Indices: the constants defined in the current class;
//
// Class instance variable information layout in DaoClass::instvars;
// -- Single Index (0): information for the self object with type DaoClass::objType;
// -- Multiple Indices: the "instvars" from the mixin component classes;
// -- Multiple Indices: the "instvars" from the base class;
// -- Mulitple Indices: the instance variables defined in the current class;
//
// Class static variable information layout in DaoClass::instvars;
// -- Multiple Indices: the static variables from the mixin component classes;
// -- Multiple Indices: the static variables from the base class;
// -- Mulitple Indices: the static variables defined in the current class;
//
// Index ranges for the constants and variables from mixin component and base classes:
// -- [ DaoClass::cstMixinStart, DaoClass::cstMixinEnd ) :
//    Constants from mixin components;
// -- [ DaoClass::cstMixinStart, DaoClass::cstMixinEnd2 ) :
//    Constants from mixin components, plus extra constants created for method overloading;
// -- [ DaoClass::glbMixinStart, DaoClass::glbMixinEnd ) :
//    Static variables from mixin components;
// -- [ DaoClass::objMixinStart, DaoClass::objMixinEnd ) :
//    Instance variables from mixin components;
// -- [ DaoClass::cstParentStart, DaoClass::cstParentEnd ) :
//    Constants from base/parent classes;
// -- [ DaoClass::glbParentStart, DaoClass::glbParentEnd ) :
//    Static variables from base/parent classes;
// Index ranges for individual mixin components are stored in DaoClass::ranges;
*/
struct DaoClass
{
	DAO_VALUE_COMMON;

	DHash_(DString*,size_t)  *lookupTable;  /* member lookup table; */

	DList_(DaoConstant*)  *constants; /* constants; */
	DList_(DaoVariable*)  *variables; /* static variables (types and init values); */
	DList_(DaoVariable*)  *instvars;  /* instance variable (types and default values); */

	DList_(DString*)  *cstDataName;  /* keep track field declaration order; */
	DList_(DString*)  *glbDataName;  /* keep track field declaration order; */
	DList_(DString*)  *objDataName;  /* keep tracking field declaration order; */

	DaoValue  *parent;  /* DaoClass or DaoCData; */

	DList_(DaoClass*)            *mixinBases;  /* direct mixin classes; */
	DList_(DaoClass*|DaoCData*)  *allBases;    /* mixin or parent classes; */

	DList_(DaoClass*)  *mixins;  /* mixin classes; */
	DArray_(ushort_t)  *ranges;  /* ranges of the fields of the mixin classes; */

	ushort_t  cstMixinStart, cstMixinEnd, cstMixinEnd2;
	ushort_t  glbMixinStart, glbMixinEnd;
	ushort_t  objMixinStart, objMixinEnd;
	ushort_t  cstParentStart, cstParentEnd;
	ushort_t  glbParentStart, glbParentEnd;
	ushort_t  objParentStart, objParentEnd;

	/* Routines with overloading signatures: */
	/* They are inserted into constants, no refCount updating for this. */
	DMap_(DString*,DaoRoutine*)  *methSignatures;

	DMap_(DaoRoutine*,DaoRoutine*)  *interMethods;

	DaoRoutine  *initRoutine;   /* Default class constructor. */
	DaoRoutine  *initRoutines;  /* All explicit constructors; GC handled in constants; */
	DaoRoutine  *castOperators; /* All user defined cast methods; */
	DaoRoutine  *intOperators;  /* Int casting routine(s); */
	DaoRoutine  *eqOperators;   /* Overloaded operator ==; */
	DaoRoutine  *ltOperators;   /* Overloaded operator <; */

	DString  *className;

	DaoType  *clsType;
	DaoType  *objType; /* GC handled in constants; */

	DList_(DString*) *decoTargets;

	DList_(DaoValue*) *references; /* for GC */

	uint_t    attribs;
	ushort_t  objDefCount;
	ushort_t  derived;
};

DAO_DLL DaoClass* DaoClass_New();
DAO_DLL void DaoClass_Delete( DaoClass *self );

DAO_DLL void DaoClass_PrintCode( DaoClass *self, DaoStream *stream );
DAO_DLL void DaoClass_AddReference( DaoClass *self, void *reference );

DAO_DLL void DaoClass_SetName( DaoClass *self, DString *name, DaoNamespace *ns );

DAO_DLL int DaoClass_CopyField( DaoClass *self, DaoClass *other, DMap *deftypes );
DAO_DLL int DaoClass_DeriveClassData( DaoClass *self );
DAO_DLL void DaoClass_DeriveObjectData( DaoClass *self );
DAO_DLL void DaoClass_UpdateMixinConstructors( DaoClass *self );
DAO_DLL void DaoClass_UpdateAttributes( DaoClass *self );
DAO_DLL void DaoClass_MakeInterface( DaoClass *self );
DAO_DLL int DaoClass_UseMixinDecorators( DaoClass *self );
DAO_DLL void DaoClass_UpdateVirtualMethods( DaoClass *self );
DAO_DLL void DaoClass_CastingMethod( DaoClass *self, DaoRoutine *routine );

DAO_DLL int  DaoClass_ChildOf( DaoClass *self, DaoValue *super );
DAO_DLL void DaoClass_AddMixinClass( DaoClass *self, DaoClass *mixin );
DAO_DLL void DaoClass_AddSuperClass( DaoClass *self, DaoValue *super );
DAO_DLL DaoValue* DaoClass_CastToBase( DaoClass *self, DaoType *parent );

DAO_DLL int  DaoClass_FindConst( DaoClass *self, DString *name );
DAO_DLL DaoValue* DaoClass_GetConst( DaoClass *self, int id );
DAO_DLL void DaoClass_SetConst( DaoClass *self, int id, DaoValue *value );

DAO_DLL DaoValue* DaoClass_GetData( DaoClass *self, DString *name, DaoClass *thisClass );
DAO_DLL int DaoClass_GetDataIndex( DaoClass *self, DString *name );

DAO_DLL int DaoClass_AddConst( DaoClass *self, DString *name, DaoValue *value, int pm );
DAO_DLL int DaoClass_AddGlobalVar( DaoClass *self, DString *name, DaoValue *val, DaoType *tp, int pm );
DAO_DLL int DaoClass_AddObjectVar( DaoClass *self, DString *name, DaoValue *val, DaoType *tp, int pm );

DAO_DLL void DaoClass_AddOverloadedRoutine( DaoClass *self, DString *signature, DaoRoutine *rout );
DAO_DLL DaoRoutine* DaoClass_GetOverloadedRoutine( DaoClass *self, DString *signature );

DAO_DLL DaoRoutine* DaoClass_FindMethod( DaoClass *self, const char *name, DaoClass *scoped );

#endif
