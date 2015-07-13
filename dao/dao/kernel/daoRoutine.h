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

#ifndef DAO_ROUTINE_H
#define DAO_ROUTINE_H

#include"daoType.h"


#define ROUT_HOST_TID( t ) ((t)->routHost ? (t)->routHost->tid : 0)

typedef struct DaoRoutineBody DaoRoutineBody;


/*
// Two types of specializatins may happen to a routine:
// 1. Method Specialization (MS) for specialized template-like types;
// 2. Parametric Specialization (PS) according to parameter types;
//
// For C methods specialization of cdata types, only the routine type
// needs specialization.
//
// For Dao routines, only the original routines have type inference done
// at compiling time. Routine specialization on parameters at compiling time
// is only done for the routine type (DaoRoutine::routType), such shallowly
// specialized routine will share the same routine body (DaoRoutine::body)
// as the original one. Deep specialization with type inference can be performed
// at runtime.
//
// For Parametric Specialization, routine constants can be reused.
//
// For a Dao routine, "body" is not NULL;
// For a C function, "pFunc" is not NULL;
// For abstract routine in interface, "body", "pFunc" and "original" are NULL;
//
// For a partially applied function, "body" and "pFunc" are NULL, but "original" is not;
// and "routConsts" holds partially applied parameters.
*/
struct DaoRoutine
{
	DAO_VALUE_COMMON;

	ushort_t         attribs;
	ushort_t         parCount; /* number of parameters that can be accepted; */
	ushort_t         defLine;  /* definition line number in the source file; */
	DString         *routName; /* routine name; */
	DaoType         *routType; /* routine type; */
	DaoType         *routHost; /* host type, for routine that is a method; */
	DaoList         *routConsts; /* default parameters and routine constants; */
	DaoNamespace    *nameSpace; /* definition namespace; */

	DaoRoutineBody  *body; /* data for Dao routines; */
	DaoCFunction     pFunc;

	DaoRoutine      *original; /* the original routine of a PS specialized one; */
	DRoutines       *specialized; /* specialization based on parameters; */
	DRoutines       *overloads; /* overloaded routines; */
};

DAO_DLL DaoRoutine* DaoRoutine_New( DaoNamespace *nspace, DaoType *host, int body );
DAO_DLL DaoRoutine* DaoRoutines_New( DaoNamespace *nspace, DaoType *host, DaoRoutine *init );
DAO_DLL DaoRoutine* DaoRoutine_Copy( DaoRoutine *self, int copy_const, int copy_body, int copy_stat );
DAO_DLL void DaoRoutine_CopyFields( DaoRoutine *self, DaoRoutine *from, int copy_body, int copy_const, int copy_stat );
DAO_DLL void DaoRoutine_Delete( DaoRoutine *self );
DAO_DLL int  DaoRoutine_AddConstant( DaoRoutine *self, DaoValue *value );

DAO_DLL int DaoRoutine_SetVmCodes( DaoRoutine *self, DList *vmCodes );
DAO_DLL void DaoRoutine_SetSource( DaoRoutine *self, DList *tokens, DaoNamespace *ns );

DAO_DLL void DaoRoutine_FormatCode( DaoRoutine *self, int i, DaoVmCodeX vmc, DString *output );
DAO_DLL void DaoRoutine_PrintCode( DaoRoutine *self, DaoStream *stream );
DAO_DLL void DaoRoutine_PrintCodeSnippet( DaoRoutine *self, DaoStream *stream, int k );

DAO_DLL void DaoRoutine_MapTypes( DaoRoutine *self, DaoRoutine *origcopy, DMap *deftypes );
DAO_DLL int DaoRoutine_Finalize( DaoRoutine *self, DaoRoutine *origcopy, DaoType *host, DMap *deftypes );
DAO_DLL int DaoRoutine_DoTypeInference( DaoRoutine *self, int silent );

struct DaoRoutineBody
{
	DAO_VALUE_COMMON;

	/* virtual machine codes: */
	DArray *vmCodes;

	/* data type for local registers: */
	DList *regType;   /* DList<DaoType*> */
	DList *upValues;  /* DList<DaoVariable*> */

	/* VM codes with annotations */
	DList *annotCodes; /* DList<DaoVmCodeX*> */

	/* definition of local constants and variables: */
	DList *defLocals; /* DList<DaoToken*> */
	DList *source; /* DList<DaoToken*> */
	DList *decoTargets;

	DList *simpleVariables;
	DMap  *localVarType;  /* DMap<int,DaoType*> local variable types */

	ushort_t  regCount;
	uchar_t   exeMode;
	uchar_t   hasStatic;
	ushort_t  codeStart;
	ushort_t  codeEnd;

	DMap   *aux;

	DaoRoutine  *revised; /* to support edit & continue */

	void *jitData;
};

DaoRoutineBody* DaoRoutineBody_New();
DaoRoutineBody* DaoRoutineBody_Copy( DaoRoutineBody *self, int copy_stat );
void DaoRoutineBody_Delete( DaoRoutineBody *self );



typedef struct DParamNode DParamNode;

struct DParamNode
{
	DaoType     *type;    /* type of the parameter node; */
	DaoType     *type2;   /* name + type; */
	DaoRoutine  *routine; /* routine of a leaf node; */
	DParamNode  *first;   /* first child node; */
	DParamNode  *last;    /* last child node; */
	DParamNode  *next;    /* next sibling node; */
};


/*
// DRoutines is a structure to organize overloaded/specialized functions into trees (tries),
// for fast function resolving based on parameter types.
//
//
// In data structures for namespace and class,
// each individual function should have its own entry in these structures,
// and an additional entry of DRoutines should be added for overloaded
// functions. This will simplify some operations such as deriving methods from
// parent type or instantiating template classes!
*/

struct DRoutines
{
	unsigned int   attribs;
	DParamNode    *tree;
	DParamNode    *mtree;    /* for routines with self parameter */
	DList        *routines; /* list of overloaded routines on both trees */
	DList        *array;    /* list of all added routines (may not be on the trees) */
	DList        *array2;
};

DRoutines* DRoutines_New();
void DRoutines_Delete( DRoutines *self );

DaoRoutine* DRoutines_Add( DRoutines *self, DaoRoutine *routine );

void DaoRoutines_Add( DaoRoutine *self, DaoRoutine *other );


/*
// Resolve overloaded routines based on parameter values and/or types:
// -- For overloaded routines, it checks and returns the best routine that is
//    callable with the given parameter values or types. The best routines is
//    the routine that produces the highest score based type matching of the
//    parameters;
// -- At compiling time, routines are resolved exclusively by parameter types.
//    So only the parameter types need to be passed to this function;
// -- At running time, routines can be resolved by both parameter values and types,
//    Parameter types are needed to handle "invar" parameters, because the invar
//    type information may not be stored in the values (since it is not thread safe
//    to tag normal values for invar parameters);
// -- When both parameter values and types are passed to this function, type
//    matching is performed on the values, with additional checks on the types
//    for information that is not available in the values;
// -- Non-overloaded routine is returned immediately without checking whether
//    it is callable with the given parameter values and types.
//    In order to check the routine against the paramter values and types,
//    use DaoRoutine_ResolveX() instead;
// -- The "callmode" parameter is a bit combination of the virtual instruction
//    values for calls (DVM_CALL or DVM_MCALL) and the second argument for the
//    instruction: DaoVmCode::code|(DaoVmCode::b<<16).
*/
DAO_DLL DaoRoutine* DaoRoutine_Resolve( DaoRoutine *self, DaoValue *svalue, DaoType *stype, DaoValue *values[], DaoType *types[], int count, int callmode );

/*
// Resolve overloaded routines and check if the routine is callable with the given
// parameter values.
*/
DAO_DLL DaoRoutine* DaoRoutine_ResolveX( DaoRoutine *self, DaoValue *svalue, DaoType *stype, DaoValue *values[], DaoType *types[], int count, int callmode );

#endif
