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
#include<assert.h>

#include"daoConst.h"
#include"daoRoutine.h"
#include"daoGC.h"
#include"daoClass.h"
#include"daoObject.h"
#include"daoStream.h"
#include"daoParser.h"
#include"daoProcess.h"
#include"daoVmspace.h"
#include"daoRegex.h"
#include"daoNumtype.h"
#include"daoNamespace.h"
#include"daoValue.h"

DMutex mutex_routines_update;
DMutex mutex_routine_specialize;
DMutex mutex_routine_specialize2;

DaoRoutine* DaoRoutine_New( DaoNamespace *nspace, DaoType *host, int body )
{
	DaoRoutine *self = (DaoRoutine*) dao_calloc( 1, sizeof(DaoRoutine) );
	DaoValue_Init( self, DAO_ROUTINE );
	self->trait |= DAO_VALUE_DELAYGC;
	self->subtype = body ? DAO_ROUTINE : DAO_CFUNCTION;
	self->routName = DString_New();
	self->routConsts = DaoList_New();
	self->nameSpace = nspace;
	self->routHost = host;
	GC_IncRC( self->nameSpace );
	GC_IncRC( self->routHost );
	GC_IncRC( self->routConsts );
	if( body ){
		self->body = DaoRoutineBody_New();
		GC_IncRC( self->body );
	}
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
DaoRoutine* DaoRoutines_New( DaoNamespace *nspace, DaoType *host, DaoRoutine *init )
{
	DaoRoutine *self = DaoRoutine_New( nspace, host, 0 );
	self->subtype = DAO_ROUTINES;
	self->overloads = DRoutines_New();
	self->routType = DaoType_New( "routine", DAO_ROUTINE, (DaoValue*)self, NULL );
	self->routType->subtid = DAO_ROUTINES;
	GC_IncRC( self->routType );
	if( init == NULL ) return self;

	DString_Assign( self->routName, init->routName );
	if( self->nameSpace == NULL ){
		self->nameSpace = init->nameSpace;
		GC_IncRC( self->nameSpace );
	}
	DaoRoutines_Add( self, init );
	return self;
}
void DaoRoutine_CopyFields( DaoRoutine *self, DaoRoutine *from, int cst, int cbody, int stat )
{
	int i;
	self->subtype = from->subtype;
	self->attribs = from->attribs;
	self->parCount = from->parCount;
	self->defLine = from->defLine;
	self->pFunc = from->pFunc;
	GC_Assign( & self->routHost, from->routHost );
	GC_Assign( & self->routType, from->routType );
	GC_Assign( & self->nameSpace, from->nameSpace );
	DString_Assign( self->routName, from->routName );
	if( cst ){
		DaoList *list = DaoList_New();
		GC_Assign( & self->routConsts, list );
		DList_Assign( self->routConsts->value, from->routConsts->value );
	}else{
		GC_Assign( & self->routConsts, from->routConsts );
	}
	if( from->body ){
		DaoRoutineBody *body = from->body;
		if( cbody ) body = DaoRoutineBody_Copy( body, stat );
		GC_Assign( & self->body, body );
	}
}
DaoRoutine* DaoRoutine_Copy( DaoRoutine *self, int cst, int body, int stat )
{
	DaoRoutine *copy = DaoRoutine_New( self->nameSpace, self->routHost, 0 );
	DaoRoutine_CopyFields( copy, self, cst, body, stat );
	return copy;
}
void DaoRoutine_Delete( DaoRoutine *self )
{
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	GC_DecRC( self->routHost );
	GC_DecRC( self->routType );
	GC_DecRC( self->routConsts );
	GC_DecRC( self->nameSpace );
	DString_Delete( self->routName );
	if( self->overloads ) DRoutines_Delete( self->overloads );
	if( self->specialized ) DRoutines_Delete( self->specialized );
	if( self->original ) GC_DecRC( self->original );
	if( self->body ) GC_DecRC( self->body );
	dao_free( self );
}
int DaoRoutine_IsWrapper( DaoRoutine *self )
{
	return self->pFunc != NULL;
}
int DaoRoutine_AddConstant( DaoRoutine *self, DaoValue *value )
{
	DList *consts = self->routConsts->value;
	DList_Append( consts, value );
	DaoValue_MarkConst( consts->items.pValue[consts->size-1] );
	return consts->size-1;
}

DaoTypeBase routTyper=
{
	"routine", & baseCore, NULL, NULL, {0}, {0},
	(FuncPtrDel) DaoRoutine_Delete, NULL
};

DaoRoutineBody* DaoRoutineBody_New()
{
	DaoRoutineBody *self = (DaoRoutineBody*) dao_calloc( 1, sizeof( DaoRoutineBody ) );
	DaoValue_Init( self, DAO_ROUTBODY );
	self->trait |= DAO_VALUE_DELAYGC;
	self->source = NULL;
	self->upValues = NULL;
	self->vmCodes = DArray_New( sizeof(DaoVmCode) );
	self->regType = DList_New( DAO_DATA_VALUE );
	self->defLocals = DList_New( DAO_DATA_TOKEN );
	self->annotCodes = DList_New( DAO_DATA_VMCODE );
	self->localVarType = DMap_New(0,0);
	self->simpleVariables = DList_New(0);
	self->codeStart = self->codeEnd = 0;
	self->aux = DMap_New(0,0);
	self->jitData = NULL;
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
void DaoRoutineBody_Delete( DaoRoutineBody *self )
{
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	DArray_Delete( self->vmCodes );
	DList_Delete( self->simpleVariables );
	DList_Delete( self->regType );
	DList_Delete( self->defLocals );
	DList_Delete( self->annotCodes );
	DMap_Delete( self->localVarType );
	if( self->upValues ) DList_Delete( self->upValues );
	if( self->decoTargets ) DList_Delete( self->decoTargets );
	if( self->revised ) GC_DecRC( self->revised );
	if( self->aux ) DaoAux_Delete( self->aux );
	if( dao_jit.Free && self->jitData ) dao_jit.Free( self->jitData );
	dao_free( self );
}
void DaoRoutineBody_CopyFields( DaoRoutineBody *self, DaoRoutineBody *other, int copy_stat )
{
	int i;
	DMap_Delete( self->localVarType );
	DList_Delete( self->annotCodes );
	self->source = other->source;
	self->annotCodes = DList_Copy( other->annotCodes );
	self->localVarType = DMap_Copy( other->localVarType );
	if( self->decoTargets ){
		DList_Delete( self->decoTargets );
		self->decoTargets = NULL;
	}
	if( other->decoTargets ) self->decoTargets = DList_Copy( other->decoTargets );
	DArray_Assign( self->vmCodes, other->vmCodes );
	DList_Assign( self->regType, other->regType );
	DList_Assign( self->simpleVariables, other->simpleVariables );
	self->regCount = other->regCount;
	self->codeStart = other->codeStart;
	self->codeEnd = other->codeEnd;
	if( other->upValues == NULL ) return;
	if( self->upValues == NULL ) self->upValues = DList_New( DAO_DATA_VALUE );
	DList_Clear( self->upValues );
	for(i=0; i<other->upValues->size; ++i){
		DaoVariable *var = other->upValues->items.pVar[i];
		if( copy_stat ) var = DaoVariable_New( var->value, var->dtype, DAO_LOCAL_CONSTANT );
		DList_Append( self->upValues, var );
	}
}
DaoRoutineBody* DaoRoutineBody_Copy( DaoRoutineBody *self, int copy_stat )
{
	DaoRoutineBody *copy = DaoRoutineBody_New();
	DaoRoutineBody_CopyFields( copy, self, copy_stat );
	return copy;
}

extern void DaoRoutine_JitCompile( DaoRoutine *self );

int DaoRoutine_SetVmCodes( DaoRoutine *self, DList *vmCodes )
{
	int i, n;
	DaoRoutineBody *body = self->body;
	if( body == NULL ) return 0;
	if( vmCodes == NULL || vmCodes->type != DAO_DATA_VMCODE ) return 0;
	DList_Swap( body->annotCodes, vmCodes );
	vmCodes = body->annotCodes;
	DArray_Resize( body->vmCodes, vmCodes->size );
	for(i=0,n=vmCodes->size; i<n; i++){
		body->vmCodes->data.codes[i] = *(DaoVmCode*) vmCodes->items.pVmc[i];
	}
	return DaoRoutine_DoTypeInference( self, 0 );
}
int DaoRoutine_SetVmCodes2( DaoRoutine *self, DArray *vmCodes )
{
	DArray_Assign( self->body->vmCodes, vmCodes );
	return DaoRoutine_DoTypeInference( self, 0 );
}

void DaoRoutine_SetSource( DaoRoutine *self, DList *tokens, DaoNamespace *ns )
{
	DList_Append( ns->sources, tokens );
	self->body->source = (DList*) DList_Back( ns->sources );
}


void DaoRoutine_MapTypes( DaoRoutine *self, DaoRoutine *original, DMap *deftypes )
{
	DaoType *tp;
	DNode *it;
	int i, n;
#if 0
	printf( "DaoRoutine_MapTypes() %s\n", self->routName->chars );
	for(it=DMap_First(deftypes); it; it=DMap_Next(deftypes,it) ){
		printf( "%16p -> %p\n", it->key.pType, it->value.pType );
		printf( "%16s -> %s\n", it->key.pType->name->chars, it->value.pType->name->chars );
	}
#endif
	if( self->body != original->body ){
		for(it=DMap_First(self->body->localVarType); it; it=DMap_Next(self->body->localVarType,it)){
			tp = DaoType_DefineTypes( it->value.pType, self->nameSpace, deftypes );
			it->value.pType = tp;
		}
	}
	for(i=0,n=self->routConsts->value->size; i<n; ++i){
		DaoValue **value2 = self->routConsts->value->items.pValue + i;
		DaoType *type = DaoValue_CastType( *value2 );
		if( type ){
			DaoType *type2 = DaoType_DefineTypes( type, self->nameSpace, deftypes );
			if( type2 == type ) continue;
			if( self->routConsts == original->routConsts ){
				DaoList *list = DaoList_New();
				DList_Assign( list->value, self->routConsts->value );
				GC_Assign( & self->routConsts, list );
				value2 = self->routConsts->value->items.pValue + i;
			}
			GC_Assign( value2, type2 );
		}else if( *value2 != NULL && i < self->routType->nested->size ){
			DaoType *partype = self->routType->nested->items.pType[i];
			DaoValue *value = NULL;
			if( partype->tid != DAO_PAR_DEFAULT ) continue;
			partype = (DaoType*) partype->aux;
			DaoValue_Move( *value2, & value, partype );
			GC_Assign( value2, value );
			GC_DecRC( value );
		}
	}
	if( self->body == original->body || self->body->upValues == NULL ) return;
	for(i=0,n=self->body->upValues->size; i<n; ++i){
		DaoVariable *var = self->body->upValues->items.pVar[i];
		DaoType *type = DaoType_DefineTypes( var->dtype, self->nameSpace, deftypes );
		GC_Assign( & var->dtype, type );
	}
}
int DaoRoutine_Finalize( DaoRoutine *self, DaoRoutine *original, DaoType *host, DMap *deftypes )
{
	DaoType *tp = DaoType_DefineTypes( self->routType, self->nameSpace, deftypes );
	if( tp == NULL ) return 0;
	GC_Assign( & self->routType, tp );
	if( host ) GC_Assign( & self->routHost, host );
	if( self->body == NULL ) return 1;
	DaoRoutine_MapTypes( self, original, deftypes );
	return 1;
	/*
	 DaoRoutine_PrintCode( self, self->nameSpace->vmSpace->stdioStream );
	 */
}


static const char *const sep1 = "==========================================\n";
static const char *const sep2 =
"-------------------------------------------------------------------------\n";

DAO_DLL void DaoRoutine_FormatCode( DaoRoutine *self, int i, DaoVmCodeX vmc, DString *output )
{
	char buffer1[10];
	char buffer2[200];
	const char *fmt = daoRoutineCodeFormat;
	const char *name;

	DString_Clear( output );
	name = DaoVmCode_GetOpcodeName( vmc.code );
	sprintf( buffer1, "%5i :  ", i);
	if( self->body->source ) DaoLexer_AnnotateCode( self->body->source, vmc, output, 24 );
	sprintf( buffer2, fmt, name, vmc.a, vmc.b, vmc.c, vmc.line, output->chars );
	DString_SetChars( output, buffer1 );
	DString_AppendChars( output, buffer2 );
}
void DaoRoutine_PrintCode( DaoRoutine *self, DaoStream *stream )
{
	DaoVmCodeX **vmCodes;
	DString *annot;
	int j, n;

	DaoStream_WriteChars( stream, sep1 );
	DaoStream_WriteChars( stream, "routine " );
	DaoStream_WriteString( stream, self->routName );
	DaoStream_WriteChars( stream, "():\n" );
	DaoStream_WriteChars( stream, "type: " );
	DaoStream_WriteString( stream, self->routType->name );
	if( self->body ){
		DaoStream_WriteChars( stream, "\nNumber of register:\n" );
		DaoStream_WriteInt( stream, self->body->regCount );
	}
	DaoStream_WriteChars( stream, "\n" );
	if( self->body == NULL ) return;

	DaoStream_WriteChars( stream, sep1 );
	DaoStream_WriteChars( stream, "Virtual Machine Code:\n\n" );
	DaoStream_WriteChars( stream, daoRoutineCodeHeader );

	DaoStream_WriteChars( stream, sep2 );
	annot = DString_New();
	vmCodes = self->body->annotCodes->items.pVmc;
	for(j=0,n=self->body->annotCodes->size; j<n; j++){
		DaoVmCode vmc = self->body->vmCodes->data.codes[j];
		if( vmc.code == DVM_JITC ){
			DaoVmCodeX vmcx = *vmCodes[j];
			memcpy( &vmcx, &vmc, sizeof(DaoVmCode) );
			DaoRoutine_FormatCode( self, j, vmcx, annot );
			DaoStream_WriteString( stream, annot );
		}
		DaoRoutine_FormatCode( self, j, *vmCodes[j], annot );
		DaoStream_WriteString( stream, annot );
	}
	DaoStream_WriteChars( stream, sep2 );
	DString_Delete( annot );
}
void DaoRoutine_PrintCodeSnippet( DaoRoutine *self, DaoStream *stream, int k )
{
	DString* mbs = DString_New();
	DaoVmCodeX **codes = self->body->annotCodes->items.pVmc;
	int debug = self->nameSpace->vmSpace->options & DAO_OPTION_DEBUG;
	int prev = debug ? 16 : 1;
	int next = debug ? 8 : 1;
	int j, m = self->body->annotCodes->size;
	int j1 = k >= prev ? k-prev : 0;
	int j2 = (k+next) < m ? k+next : m-1;

	DaoStream_WriteChars( stream, "In code snippet:\n" );
	for(j=j1; j<=j2; ++j){
		DaoRoutine_FormatCode( self, j, *codes[j], mbs );
		DaoStream_WriteChars( stream, j==k ? ">>" : "  " );
		DaoStream_WriteString( stream, mbs );
	}
	DString_Delete( mbs );
}



static DParamNode* DParamNode_New()
{
	DParamNode *self = (DParamNode*) dao_calloc( 1, sizeof(DParamNode) );
	return self;
}
static void DParamNode_Delete( DParamNode *self )
{
	while( self->first ){
		DParamNode *node = self->first;
		self->first = node->next;
		DParamNode_Delete( node );
	}
	dao_free( self );
}


DRoutines* DRoutines_New()
{
	DRoutines *self = (DRoutines*) dao_calloc( 1, sizeof(DRoutines) );
	self->tree = NULL;
	self->mtree = NULL;
	self->routines = DList_New(0);
	self->array = DList_New( DAO_DATA_VALUE );
	self->array2 = DList_New(0);
	return self;
}
void DRoutines_Delete( DRoutines *self )
{
	if( self->tree ) DParamNode_Delete( self->tree );
	if( self->mtree ) DParamNode_Delete( self->mtree );
	DList_Delete( self->routines );
	DList_Delete( self->array );
	DList_Delete( self->array2 );
	dao_free( self );
}

static DParamNode* DParamNode_Add( DParamNode *self, DaoRoutine *routine, int pid )
{
	DaoType *partype;
	DParamNode *param, *it;
	if( pid >= (int)routine->routType->nested->size ){
		/* If a routine with the same parameter signature is found, return it: */
		for(it=self->first; it; it=it->next){
			/*
			// Code section methods may be overloaded with normal methods with
			// exactly the same parameter signatures (map::keys()).
			// But they differ in attributes.
			*/
			if( it->routine && it->routine->attribs == routine->attribs ) return it;
		}
		param = DParamNode_New();
		param->routine = routine;
		/* Add as a leaf. */
		if( self->last ){
			self->last->next = param;
			self->last = param;
		}else{
			self->first = self->last = param;
		}
		return param;
	}
	partype = routine->routType->nested->items.pType[pid];
	for(it=self->first; it; it=it->next){
		if( DaoType_MatchTo( partype, it->type2, NULL ) >= DAO_MT_EQ ){
			return DParamNode_Add( it, routine, pid + 1 );
		}
	}
	/* Add a new internal node: */
	param = DParamNode_New();
	param->type = routine->routType->nested->items.pType[pid];
	if( param->type->tid >= DAO_PAR_NAMED && param->type->tid <= DAO_PAR_VALIST ){
		param->type2 = param->type;
		param->type = (DaoType*) param->type->aux;
	}
	it = DParamNode_Add( param, routine, pid+1 );
	/*
	// Add the node to the tree after all its child nodes have been created, to ensure
	// a reader will always lookup in a valid tree in multi-threaded applications:
	*/
	if( self->last ){
		self->last->next = param;
		self->last = param;
	}else{
		self->first = self->last = param;
	}
	return it;
}
static void DParamNode_ExportRoutine( DParamNode *self, DList *routines )
{
	DParamNode *it;
	if( self->routine ) DList_PushFront( routines, self->routine );
	for(it=self->first; it; it=it->next) DParamNode_ExportRoutine( it, routines );
}
DaoRoutine* DRoutines_Add( DRoutines *self, DaoRoutine *routine )
{
	int i, n, bl = 0;
	DParamNode *param = NULL;
	DList *routs;

	if( routine->routType == NULL ) return NULL;
	/* If the name is not set yet, set it: */
	self->attribs |= DString_FindChar( routine->routType->name, '@', 0 ) != DAO_NULLPOS;
	DMutex_Lock( & mutex_routines_update );
	if( routine->routType->attrib & DAO_TYPE_SELF ){
		if( self->mtree == NULL ) self->mtree = DParamNode_New();
		param = DParamNode_Add( self->mtree, routine, 0 );
	}else{
		if( self->tree == NULL ) self->tree = DParamNode_New();
		param = DParamNode_Add( self->tree, routine, 0 );
	}
	/*
	// Always replace the previous routine with the current one.
	*/
	param->routine = routine;

	/*
	// Runtime routine specialization based on parameter types may create
	// two specializations with identical parameter signature, so one of
	// the specialized routine will not be successully added to the tree.
	// To avoid memory leaking, the one not added to the tree should also
	// be appended to "array", so that it can be properly garbage collected.
	*/
	DList_Append( self->array, routine );

	self->array2->size = 0;
	if( self->mtree ) DParamNode_ExportRoutine( self->mtree, self->array2 );
	if( self->tree ) DParamNode_ExportRoutine( self->tree, self->array2 );
	/* to ensure safety for readers: */
	routs = self->routines;
	self->routines = self->array2;
	self->array2 = routs;
	DMutex_Unlock( & mutex_routines_update );
	return param->routine;
}
void DaoRoutines_Add( DaoRoutine *self, DaoRoutine *other )
{
	DaoType *host = self->routHost;
	DaoNamespace *nspace = self->nameSpace;
	int i;
	if( self->overloads == NULL ) return;
	if( other->overloads == NULL ){
		DRoutines_Add( self->overloads, other );
		return;
	}
	for(i=0; i<other->overloads->routines->size; i++){
		DaoRoutine *routine = other->overloads->routines->items.pRoutine[i];
		if( routine->attribs & DAO_ROUT_PRIVATE ){
			if( routine->routHost && routine->routHost != host ) continue;
			if( routine->routHost == NULL && routine->nameSpace != nspace ) continue;
		}
		DRoutines_Add( self->overloads, routine );
	}
}
static DaoRoutine* DParamNode_GetLeaf( DParamNode *self, int *ms, int mode )
{
	int b1 = (mode & DAO_CALL_BLOCK) != 0;
	DParamNode *param;
	DaoRoutine *rout;
	DNode *it;

	*ms = 0;
	if( self->routine ){
		int b2 = (self->routine->attribs & DAO_ROUT_CODESECT) != 0;
		if( b1 == b2 ) return self->routine; /* a leaf */
		return NULL;
	}
	for(param=self->first; param; param=param->next){
		if( param->type == NULL ){
			int b2 = (param->routine->attribs & DAO_ROUT_CODESECT) != 0;
			if( b1 == b2 ) return param->routine; /* a leaf */
			continue;
			return NULL;
		}
		if( param->type->tid == DAO_PAR_VALIST ){
			rout = DParamNode_GetLeaf( param, ms, mode );
			if( rout == NULL ) continue;
			*ms += 1;
			return rout;
		}
	}
	/* check for routines with default parameters: */
	for(param=self->first; param; param=param->next){
		if( param->type2 == NULL ) continue;
		if( param->type2->tid != DAO_PAR_DEFAULT && param->type2->tid != DAO_PAR_VALIST ) continue;
		rout = DParamNode_GetLeaf( param, ms, mode );
		if( rout == NULL ) continue;
		*ms += 1;
		return rout;
	}
	return NULL;
}


static int Dao_CheckParameter( DaoType *partype, DaoValue *argvalue, DaoType *argtype, DMap *defs )
{
	int m = 0;
	if( argvalue && argtype && argtype->tid > DAO_ENUM ){
		if( DaoType_CheckInvarMatch( argtype, partype, 0 ) == 0 ) return 0;
		m = DaoType_MatchValue( partype, argvalue, defs );
	}else if( argtype && argtype->tid > DAO_ENUM ){
		if( DaoType_CheckInvarMatch( argtype, partype, 0 ) == 0 ) return 0;
		m = DaoType_MatchTo( argtype, partype, defs );
	}else if( argvalue ){
		m = DaoType_MatchValue( partype, argvalue, defs );
	}else if( argtype ){
		m = DaoType_MatchTo( argtype, partype, defs );
	}
	return m;
}

static DaoRoutine* DParamNode_Lookup( DParamNode *self, DaoValue *values[], DaoType *types[], int count, int mode, int strict, int *ms, DMap *defs, int clear )
{
	int i, m, mt, mt2, k = 0, max = 0;
	DaoValue **values2 = values ? values + 1 : NULL;
	DaoType  **types2  = types  ? types  + 1 : NULL;
	DaoRoutine *rout = NULL;
	DaoRoutine *best = NULL;
	DaoValue *argvalue = NULL;
	DaoType *argtype = NULL;
	DParamNode *parnode;

	*ms = 1;
	if( count == 0 ) return DParamNode_GetLeaf( self, ms, mode );

	if( self->type2 && self->type2->tid == DAO_PAR_VALIST ){
		*ms = DAO_MT_EQ;
		for(i=0; i<count; ++i){
			argvalue = values ? values[i] : NULL;
			argtype = types ? types[i] : NULL;
			m = Dao_CheckParameter( self->type, argvalue, argtype, defs );
			if( m == 0 ) return NULL;
			if( m < *ms ) *ms = m;
		}
		return DParamNode_GetLeaf( self, & k, mode );
	}

	argvalue = values ? values[0] : NULL;
	argtype = types ? types[0] : NULL;
	for(parnode=self->first; parnode; parnode=parnode->next){
		int tid = parnode->type2 ? parnode->type2->tid : 0;
		DaoType *type = parnode->type;
		if( type == NULL ) continue;
		if( parnode->type2 && (parnode->type2->attrib & DAO_TYPE_SELFNAMED) ){
			/*
			// self parameter will be passed by reference, const self argument
			// should not be passed to non-const self parameter;
			*/
			if( DaoType_CheckInvarMatch( argtype, parnode->type, 1 ) == 0 ) continue;
		}
		if( defs && clear ) DMap_Reset( defs );

		m = Dao_CheckParameter( parnode->type, argvalue, argtype, defs );
		if( m == 0 ) continue;
		if( strict && m < DAO_MT_ANY ) continue;
		rout = DParamNode_Lookup( parnode, values2, types2, count-1, mode, strict, & k, defs, 0 );
		if( rout == NULL ) continue;
		m += k;
		if( m > max ){
			best = rout;
			max = m;
		}
	}
	*ms = max;
	return best;
}

static DaoRoutine* DRoutines_Lookup2( DRoutines *self, DaoValue *svalue, DaoType *stype, DaoValue *values[], DaoType *types[], int count, int callmode, int strict )
{
	int i, k, m, mt, mt2, score = 0;
	int code = callmode & 0xffff;
	int mode = callmode >> 16;
	int mcall = code == DVM_MCALL;
	DParamNode *parnode = NULL;
	DaoRoutine *rout = NULL;
	DMap *defs = NULL;
	if( self->attribs ) defs = DHash_New(0,0);
	if( self->mtree != NULL && (svalue != NULL || stype != NULL) && mcall == 0 ){
		DaoRoutine *rout2 = NULL;
		/*
		// class Klass {
		//     routine Meth1() { }
		//     routine Meth2() { Meth1() }
		// }
		 */
		for(parnode=self->mtree->first; parnode; parnode=parnode->next){
			if( parnode->type == NULL ) continue;
			if( stype != NULL && DaoType_CheckInvarMatch( stype, parnode->type, 1 ) == 0 ) continue;

			m = Dao_CheckParameter( parnode->type, svalue, stype, defs );
			if( strict && m < DAO_MT_ANY ) continue;
			if( m == 0 ) continue;

			rout2 = DParamNode_Lookup( parnode, values, types, count, mode, strict, & k, defs, 0 );
			if( rout2 == NULL ) continue;

			m += k;
			if( m > score ){
				rout = rout2;
				score = m;
			}
		}
		if( rout ) goto Finalize;
	}
	if( mcall && self->mtree ){
		/*
		// object = Klass()
		// object.Meth1()
		*/
		rout = DParamNode_Lookup( self->mtree, values, types, count, mode, strict, & score, defs, 1 );
		if( rout ) goto Finalize;
	}
	if( mcall == 0 && svalue == NULL && stype == NULL && self->mtree ){
		/*
		// routine test(self: array<int>){}
		// routine test(self: array<int>, x: int){}
		// test([1, 2, 3]) 
		*/
		rout = DParamNode_Lookup( self->mtree, values, types, count, mode, strict, & score, defs, 1 );
		if( rout ) goto Finalize;
	}
	if( self->tree ){
		if( mcall ){
			/* obj.function(), where function() is not method of obj: */
			if( values ) values += 1;
			if( types  ) types += 1;
			count -= 1;
		}
		rout = DParamNode_Lookup( self->tree, values, types, count, mode, strict, & score, defs, 1 );
	}
Finalize:
	if( defs ) DMap_Delete( defs );
	return rout;
}
static DaoRoutine* DRoutines_Lookup( DRoutines *self, DaoValue *svalue, DaoType *stype, DaoValue *values[], DaoType *types[], int count, int callmode )
{
	return DRoutines_Lookup2( self, svalue, stype, values, types, count, callmode, 0 );
}

DaoRoutine* DaoRoutine_Resolve( DaoRoutine *self, DaoValue *svalue, DaoType *stype, DaoValue *values[], DaoType *types[], int count, int callmode )
{
	DaoRoutine *rout, *rout2;
	int b1, b2;

	if( self == NULL ) return NULL;
	if( self->overloads ){
		self = DRoutines_Lookup( self->overloads, svalue, stype, values, types, count, callmode );

		if( self == NULL ) return NULL;
	}
	rout = self;
	if( rout->specialized ){
		/* strict checking for specialized routines: */
		rout2 = DRoutines_Lookup2( rout->specialized, svalue, stype, values, types, count, callmode, 1 );

		if( rout2 ) rout = rout2;
	}
	b1 = ((callmode>>16) & DAO_CALL_BLOCK) != 0;
	b2 = (rout->attribs & DAO_ROUT_CODESECT) != 0;
	if( b1 != b2 ) return NULL;
	return (DaoRoutine*) rout;
}
DaoRoutine* DaoRoutine_ResolveByValue( DaoRoutine *self, DaoValue *svalue, DaoValue *values[], int count )
{
	return DaoRoutine_Resolve( self, svalue, NULL, values, NULL, count, 0 );
}
DaoRoutine* DaoRoutine_ResolveByType( DaoRoutine *self, DaoType *stype, DaoType *types[], int count )
{
	return DaoRoutine_Resolve( self, NULL, stype, NULL, types, count, 0 );
}

static int DaoRoutine_Check( DaoRoutine *self, DaoValue *svalue, DaoType *stype, DaoValue *values[], DaoType *types[], int count, int callmode )
{
	DNode *node;
	DMap *defs = DMap_New(0,0);
	DMap *mapNames = self->routType->mapNames;
	DaoValue *argvalue;
	DaoType *partype, *argtype;
	DaoType **partypes = self->routType->nested->items.pType;
	DaoType **argtypes = types;
	DaoValue **argvalues = values;
	int need_self = self->routType->attrib & DAO_TYPE_SELF;
	int code = callmode & 0xffff;
	int selfChecked = 0, selfMatch = 0;
	int parcount = self->parCount;
	int argcount = count;
	int j, ok, argindex, parindex;
	int parpass[DAO_MAX_PARAM];

	/* Call types: func(..), obj.func(..), obj::func(..); */
	if( code == DVM_MCALL && ! (self->routType->attrib & DAO_TYPE_SELF) ){
		if( argvalues ) argvalues ++;
		if( argtypes )  argtypes ++;
		argcount --;
	}else if( (svalue != NULL || stype != NULL) && need_self && code != DVM_MCALL ){
		/*
		// class DaoClass : CppClass { cppmethod(); }
		// use io; writeln(..);
		*/
		partype = partypes[0];
		if( partype->attrib & DAO_TYPE_PARNAMED ) partype = (DaoType*) partype->aux;
		selfMatch = Dao_CheckParameter( partype, svalue, stype, defs );
		if( selfMatch ){
			parpass[0] = selfMatch;
			selfChecked = 1;
		}
	}
#if 0
	printf( "%p, parlist = %s; argcount = %i; parcount = %i, %i\n", self, self->routType->name->chars, argcount, parcount, selfChecked );
#endif

	if( (argcount | parcount) == 0 ) goto Matched;
	if( argcount > parcount ) goto NotMatched;

	for(j=selfChecked; j<parcount; ++j) parpass[j] = 0;

	for(argindex=0; argindex<argcount; argindex++){
		argvalue = argvalues ? argvalues[argindex] : NULL;
		argtype  = argtypes  ? argtypes[ argindex] : NULL;
		parindex = argindex + selfChecked;
		if( parindex >= parcount ) goto NotMatched;
		partype = partypes[parindex];
		if( partype->tid == DAO_PAR_VALIST ){
			partype = (DaoType*) partype->aux;  /* ... or ...:type; */
			for(; argindex<argcount; argindex++){
				argvalue = argvalues ? argvalues[argindex] : NULL;
				argtype  = argtypes  ? argtypes[ argindex] : NULL;
				ok = Dao_CheckParameter( partype, argvalue, argtype, defs );
				if( partype && ! ok ) goto NotMatched;
				parpass[argindex+selfChecked] = 1;
			}
			break;
		}else if( (partype->attrib & DAO_TYPE_SELFNAMED) && partype->aux->xType.invar == 0 ){
			DaoType *ptype = (DaoType*) partype->aux;
			if( argtype != NULL && DaoType_CheckInvarMatch( argtype, ptype, 1 ) == 0 ) continue;
		}
		if( partype->attrib & DAO_TYPE_PARNAMED ) partype = (DaoType*) partype->aux;
		parpass[parindex] = Dao_CheckParameter( partype, argvalue, argtype, defs );
		/*
		   printf( "%i:  %i  %s  %s\n", parpass[parindex], partype->tid, partype->name->chars,
		   argtype ? argtype->name->chars : "" );
		 */
		if( parpass[parindex] == 0 ) goto NotMatched;
	}
	for(parindex=0; parindex<parcount; parindex++){
		int tid = partypes[parindex]->tid;
		if( tid == DAO_PAR_VALIST ) break;
		if( parpass[parindex] ) continue;
		if( tid != DAO_PAR_DEFAULT ) goto NotMatched;
		parpass[parindex] = 1;
	}
Matched:
	DMap_Delete( defs );
	return 1;
NotMatched:
	DMap_Delete( defs );
	return 0;
}

DaoRoutine* DaoRoutine_ResolveX( DaoRoutine *self, DaoValue *svalue, DaoType *stype, DaoValue *values[], DaoType *types[], int count, int callmode )
{
	DaoRoutine *rout = DaoRoutine_Resolve( self, svalue, stype, values, types, count, callmode );
	if( rout == self ){ /* parameters not yet checked: */
		if( DaoRoutine_Check( self, svalue, stype, values, types, count, callmode ) == 0 ){
			return NULL;
		}
	}
	return rout;
}
