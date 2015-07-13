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
#include"daoInferencer.h"


extern DMutex mutex_routine_specialize;


DaoInode* DaoInode_New()
{
	DaoInode *self = (DaoInode*) dao_calloc( 1, sizeof(DaoInode) );
	return self;
}
void DaoInode_Delete( DaoInode *self )
{
	dao_free( self );
}
void DaoInode_Print( DaoInode *self, int index )
{
	const char *name = DaoVmCode_GetOpcodeName( self->code );
	static const char *fmt = "%3i: %-8s : %5i, %5i, %5i;  [%3i] [%2i] %9p %9p %9p, %s\n";
	if( index < 0 ) index = self->index;
	printf( fmt, index, name, self->a, self->b, self->c, self->line, self->level,
			self, self->jumpTrue, self->jumpFalse, "" );
}



void DaoInodes_Clear( DList *inodes )
{
	DaoInode *tmp, *inode = (DaoInode*) DList_Front( inodes );
	while( inode && inode->prev ) inode = inode->prev;
	while( inode ){
		tmp = inode;
		inode = inode->next;
		DaoInode_Delete( tmp );
	}
	DList_Clear( inodes );
}

void DaoRoutine_CodesToInodes( DaoRoutine *self, DList *inodes, int duplicate )
{
	DaoInode *inode, *inode2;
	DaoVmCodeX *vmc, **vmcs = self->body->annotCodes->items.pVmc;
	daoint i, N = self->body->annotCodes->size;

	for(i=0; i<N; i++){
		inode2 = (DaoInode*) DList_Back( inodes );

		if( duplicate ){
			/*
			// Add unused duplicated nodes to allow insertion of new nodes,
			// without messing up the jumps.
			*/
			inode = DaoInode_New();
			inode->code = DVM_UNUSED;
			if( inode2 ){
				inode2->next = inode;
				inode->prev = inode2;
			}
			inode2 = inode;
		}

		inode = DaoInode_New();
		vmc = vmcs[i];
		if( vmc->code == DVM_GETMI && vmc->b == 1 ){
			vmc->code = DVM_GETI;
			vmc->b = vmc->a + 1;
		}else if( vmc->code == DVM_SETMI && vmc->b == 1 ){
			vmc->code = DVM_SETI;
			vmc->b = vmc->c + 1;
		}
		*(DaoVmCodeX*)inode = *vmc;
		inode->index = i;
		if( inode2 ){
			inode2->next = inode;
			inode->prev = inode2;
		}
		DList_PushBack( inodes, inode );
	}
	for(i=0; i<N; i++){
		vmc = vmcs[i];
		inode = inodes->items.pInode[i];
		switch( vmc->code ){
		case DVM_GOTO : case DVM_CASE : case DVM_SWITCH :
		case DVM_TEST : case DVM_TEST_B : case DVM_TEST_I : case DVM_TEST_F :
			inode->jumpFalse = inodes->items.pInode[vmc->b];
			if( duplicate ) inode->jumpFalse = inode->jumpFalse->prev;
			break;
		default : break;
		}
	}
}
void DaoRoutine_CodesFromInodes( DaoRoutine *self, DList *inodes )
{
	int count = 0;
	DaoRoutineBody *body = self->body;
	DaoInode *it, *first = (DaoInode*) DList_Front( inodes );
	while( first->prev ) first = first->prev;
	for(it=first; it; it=it->next){
		it->index = count;
		count += it->code != DVM_UNUSED;
		//while( it->jumpFalse && it->jumpFalse->extra ) it->jumpFalse = it->jumpFalse->extra;
	}
	DArray_Clear( body->vmCodes );
	DList_Clear( body->annotCodes );
	for(it=first,count=0; it; it=it->next){
		/* DaoInode_Print( it ); */
		switch( it->code ){
		case DVM_GOTO : case DVM_CASE : case DVM_SWITCH :
		case DVM_TEST : case DVM_TEST_B : case DVM_TEST_I : case DVM_TEST_F :
			it->b = it->jumpFalse->index;
			break;
		default : break;
		}
		if( it->code >= DVM_UNUSED ) continue;
		DArray_PushCode( body->vmCodes, *(DaoVmCode*) it );
		DList_PushBack( body->annotCodes, (DaoVmCodeX*) it );
	}
}
void DaoRoutine_SetupSimpleVars( DaoRoutine *self )
{
	DMap *refers = DMap_New(0,0);
	DaoRoutineBody *body = self->body;
	DaoVmCodeX **vmcs = body->annotCodes->items.pVmc;
	int i, n;

	self->attribs &= ~DAO_ROUT_REUSABLE;
	for(i=0,n=body->annotCodes->size; i<n; ++i){
		if( DaoVmCode_MayCreateReference( vmcs[i]->code ) ){
			DMap_Insert( refers, IntToPointer( vmcs[i]->code ), 0 );
		}
	}

	DList_Clear( body->simpleVariables );
	for(i=self->parCount,n=body->regType->size; i<n; ++i){
		DaoType *tp = body->regType->items.pType[i];
		if( tp && tp->tid <= DAO_ENUM ){
			DList_Append( body->simpleVariables, (daoint)i );
			if( DMap_Find( refers, IntToPointer(i) ) != NULL ){
				self->attribs |= DAO_ROUT_REUSABLE;
			}
		}
	}
	DMap_Delete( refers );
}



DaoInferencer* DaoInferencer_New()
{
	DaoInferencer *self = (DaoInferencer*) dao_calloc( 1, sizeof(DaoInferencer) );
	self->inodes = DList_New(0);
	self->consts = DList_New( DAO_DATA_VALUE );
	self->types = DList_New( DAO_DATA_VALUE );
	self->types2 = DList_New( DAO_DATA_VALUE );
	self->rettypes = DList_New(0);
	self->typeMaps = DList_New( DAO_DATA_MAP );
	self->errors = DList_New(0);
	self->array = DList_New(0);
	self->array2 = DList_New(0);
	self->defers = DList_New(0);
	self->defs = DHash_New(0,0);
	self->defs2 = DHash_New(0,0);
	self->defs3 = DHash_New(0,0);
	self->mbstring = DString_New();
	return self;
}
void DaoInferencer_Reset( DaoInferencer *self )
{
	DaoInodes_Clear( self->inodes );
	DList_Clear( self->consts );
	DList_Clear( self->types );
	DList_Clear( self->types2 );
	DList_Clear( self->typeMaps );
	DMap_Reset( self->defs );
	DMap_Reset( self->defs2 );
	DMap_Reset( self->defs3 );
	self->rettypes->size = 0;
	self->errors->size = 0;
	self->array->size = 0;
	self->array2->size = 0;
	self->defers->size = 0;
	self->error = 0;
	self->annot_first = 0;
	self->annot_last = 0;
	self->tid_target = 0;
	self->type_source = NULL;
	self->type_target = NULL;
}
void DaoInferencer_Delete( DaoInferencer *self )
{
	DaoInferencer_Reset( self );
	DList_Delete( self->inodes );
	DList_Delete( self->consts );
	DList_Delete( self->types );
	DList_Delete( self->types2 );
	DList_Delete( self->rettypes );
	DList_Delete( self->typeMaps );
	DList_Delete( self->errors );
	DList_Delete( self->array );
	DList_Delete( self->array2 );
	DList_Delete( self->defers );
	DString_Delete( self->mbstring );
	DMap_Delete( self->defs );
	DMap_Delete( self->defs2 );
	DMap_Delete( self->defs3 );
	dao_free( self );
}
void DaoInferencer_Init( DaoInferencer *self, DaoRoutine *routine, int silent )
{
	DNode *node;
	DMap *defs = self->defs;
	DaoType *type, **types;
	DaoNamespace *NS = routine->nameSpace;
	DList *partypes = routine->routType->nested;
	daoint i, n, M = routine->body->regCount;

	DaoInferencer_Reset( self );
	self->silent = silent;
	self->routine = routine;
	self->tidHost = routine->routHost ? routine->routHost->tid : 0;
	self->hostClass = self->tidHost == DAO_OBJECT ? & routine->routHost->aux->xClass:NULL;

	DaoRoutine_CodesToInodes( routine, self->inodes, 1 );

	DList_Resize( self->consts, M, NULL );
	/*
	// Allocate more memory so that the "types" and "typeVH" variables in
	// DaoInferencer_DoInference() will not be invalidated by inserting instructions.
	*/
	DList_Resize( self->types, 3*M, NULL );
	self->types->size = M;
	types = self->types->items.pType;

	for(i=0,n=partypes->size; i<n; i++){
		types[i] = partypes->items.pType[i];
		if( types[i] && types[i]->tid == DAO_PAR_VALIST ){
			DaoType *vltype = (DaoType*) types[i]->aux;
			while( i < DAO_MAX_PARAM ) types[i++] = vltype;
			break;
		}
		type = types[i];
		if( type && (type->attrib & DAO_TYPE_PARNAMED) ) types[i] = & type->aux->xType;
		node = MAP_Find( routine->body->localVarType, i );
		if( node == NULL ) continue;
		if( node->value.pType == NULL || types[i] == NULL ) continue;
		DaoType_MatchTo( types[i], node->value.pType, defs );
	}
	node = DMap_First( routine->body->localVarType );
	for( ; node !=NULL; node = DMap_Next(routine->body->localVarType,node) ){
		if( node->key.pInt < (int)partypes->size ) continue;
		types[ node->key.pInt ] = DaoType_DefineTypes( node->value.pType, NS, defs );
	}
	for(i=0; i<self->types->size; i++) GC_IncRC( types[i] );
	DList_PushBack( self->typeMaps, defs );

	self->typeEnum = DaoNamespace_MakeType( NS, "enum", DAO_ENUM, NULL, NULL, 0 );
	self->typeString = DaoNamespace_MakeType( NS, "string", DAO_STRING, NULL, NULL, 0 );
	self->basicTypes[DAO_NONE] = dao_type_none;
	self->basicTypes[DAO_BOOLEAN] = dao_type_bool;
	self->basicTypes[DAO_INTEGER] = dao_type_int;
	self->basicTypes[DAO_FLOAT] = dao_type_float;
	self->basicTypes[DAO_COMPLEX] = dao_type_complex;
	self->basicTypes[DAO_ENUM] = self->typeEnum;
	self->basicTypes[DAO_STRING] = self->typeString;
}


static int DaoRoutine_CheckTypeX( DaoType *routType, DaoNamespace *ns, DaoType *selftype,
		DaoType *ts[], int np, int code, int def, int *parpass, int passdefault )
{
	int ndef = 0;
	int i, j, match = 1;
	int ifrom, ito;
	int npar = np, size = routType->nested->size;
	int selfChecked = 0, selfMatch = 0;
	DaoType  *partype, **partypes = routType->nested->items.pType;
	DaoType **tps = ts;
	DMap *defs;

	/* Check for explicit self parameter: */
	if( np && (ts[0]->attrib & DAO_TYPE_SELFNAMED) ){
		selftype = NULL;
		code = DVM_MCALL;
	}

	defs = DMap_New(0,0);
	if( routType->nested ){
		ndef = routType->nested->size;
		if( ndef ){
			partype = partypes[ ndef-1 ];
			if( partype->tid == DAO_PAR_VALIST ) ndef = DAO_MAX_PARAM;
		}
	}

#if 0
	printf( "=====================================\n" );
	for( j=0; j<npar; j++){
		DaoType *tp = tps[j];
		if( tp != NULL ) printf( "tp[ %i ]: %s\n", j, tp->name->chars );
	}
	printf( "%s %i %i\n", routType->name->chars, ndef, npar );
	if( selftype ) printf( "%i\n", routType->name->chars, ndef, npar, selftype );
#endif

	if( code == DVM_MCALL && ! (routType->attrib & DAO_TYPE_SELF) ){
		npar --;
		tps ++;
	}else if( selftype && (routType->attrib & DAO_TYPE_SELF) && code != DVM_MCALL ){
		/* class DaoClass : CppClass{ cppmethod(); } */
		partype = (DaoType*) partypes[0]->aux;
		/*
		// self object will be passed by reference,
		// so invariable/constant cannot be passed to variable type
		*/
		selfMatch = DaoType_MatchTo( selftype, partype, defs );
		if( selfMatch ){
			if( DaoType_CheckInvarMatch( selftype, partype, 1 ) == 0 ) goto FinishError;
			selfChecked = 1;
			parpass[0] = selfMatch;
		}
	}
	if( npar == ndef && ndef == 0 ) goto FinishOK;
	if( (npar+selfChecked) > ndef && (size == 0 || partypes[size-1]->tid != DAO_PAR_VALIST ) ){
		goto FinishError;
	}

	for(j=selfChecked; j<ndef; j++) parpass[j] = 0;
	for(ifrom=0; ifrom<npar; ifrom++){
		DaoType *tp = tps[ifrom];
		ito = ifrom + selfChecked;
		partype = partypes[ito];
		if( partype->tid == DAO_PAR_VALIST ){
			DaoType *vlt = (DaoType*) partype->aux;
			for(; ifrom<npar; ifrom++, ito++){
				parpass[ito] = 1;
				if( vlt && DaoType_MatchTo( tp, vlt, defs ) == 0 ) goto FinishError;
			}
			break;
		}else if( (partype->attrib & DAO_TYPE_SELFNAMED) && partype->aux->xType.invar == 0 ){
			if( DaoType_CheckInvarMatch( tp, (DaoType*) partype->aux, 1 ) == 0 ) goto FinishError;
		}
		if( tp == NULL )  goto FinishError;
		if( tp->attrib & DAO_TYPE_PARNAMED ) tp = (DaoType*) tp->aux;
		if( partype->attrib & DAO_TYPE_PARNAMED ) partype = (DaoType*) partype->aux;
		parpass[ito] = DaoType_MatchTo( tp, partype, defs );

#if 0
		printf( "%s %s\n", tp->name->chars, partype->name->chars );
		printf( "%i:  %i\n", ito, parpass[ito] );
#endif

		if( parpass[ito] == 0 ) goto FinishError;
		if( def ){
			DaoType *tp = DaoType_DefineTypes( tps[ifrom], ns, defs );
			GC_Assign( & tps[ifrom], tp );
		}
	}
	if( passdefault ){
		for(ito=0; ito<ndef; ito++){
			i = partypes[ito]->tid;
			if( i == DAO_PAR_VALIST ) break;
			if( parpass[ito] ) continue;
			if( i != DAO_PAR_DEFAULT ) goto FinishError;
			parpass[ito] = 1;
		}
	}
	match = DAO_MT_EQ;
	for(j=0; j<(npar+selfChecked); j++) if( match > parpass[j] ) match = parpass[j];

#if 0
	printf( "%s %i %i %i\n", routType->name->chars, match, ndef, npar );
#endif

FinishOK:
	DMap_Delete( defs );
	return match;
FinishError:
	DMap_Delete( defs );
	return 0;
}
static int DaoRoutine_CheckType( DaoType *routType, DaoNamespace *ns, DaoType *selftype,
		DaoType *ts[], int np, int codemode, int def )
{
	int parpass[DAO_MAX_PARAM];
	int code = codemode & 0xffff;
	int b1 = ((codemode>>16) & DAO_CALL_BLOCK) != 0;
	int b2 = (routType->attrib & DAO_TYPE_CODESECT) != 0;
	if( b1 != b2 ) return 0;
	return DaoRoutine_CheckTypeX( routType, ns, selftype, ts, np, code, def, parpass, 1 );
}

DaoType* DaoRoutine_PartialCheck( DaoNamespace *NS, DaoType *routype, DList *routines, DList *partypes, int call, int *which, int *matched )
{
	DString *name;
	DaoType *type, *type2, **types;
	DaoType *retype = (DaoType*) routype->aux;
	DList *routypes = DList_New(0);
	int parpass[DAO_MAX_PARAM];
	int npar = partypes->size;
	int j, k, max = 0;

	if( routines ){
		for(j=0; j<routines->size; j++){
			type = routines->items.pRoutine[j]->routType;
			DList_Append( routypes, type );
		}
	}else{
		DList_Append( routypes, routype );
	}
	*matched = 0;
	routype = NULL;
	for(j=0; j<routypes->size; j++){
		type = routypes->items.pType[j];
		k = type->nested->size;
		partypes->size = npar;
		while( partypes->size < k ) DList_Append( partypes, dao_type_any );
		k = DaoRoutine_CheckTypeX( type, NS, NULL, partypes->items.pType, k, call, 0, parpass, 0 );
		*matched += k != 0 && k == max;
		if( k > max ){
			if( routines ) *which = j;
			*matched = 0;
			routype = type;
			max = k;
		}
	}
	DList_Delete( routypes );
	if( routype == NULL ) return NULL;
	DaoRoutine_CheckTypeX( routype, NS, NULL, partypes->items.pType, npar, call, 0, parpass, 0 );
	partypes->size = 0;
	k = routype->nested->size - (routype->variadic != 0);
	for(j=0; j<k; j++){
		if( parpass[j] ) continue;
		DList_Append( partypes, routype->nested->items.pType[j] );
	}
	if( routype->variadic ) DList_Append( partypes, DList_Back( routype->nested ) );
	if( call == DVM_MCALL && partypes->items.pType[0]->tid == DAO_OBJECT ){
		DaoClass *klass = (DaoClass*) partypes->items.pType[0]->aux;
		if( klass->attribs & DAO_CLS_ASYNCHRONOUS ){
			retype = DaoType_Specialize( dao_type_future, & retype, retype != NULL );
		}
	}
	k = partypes->size;
	types = partypes->items.pType;
	type = DaoNamespace_MakeType( NS, "routine", DAO_ROUTINE, (DaoValue*) retype, types, k );
	if( routype->cbtype == NULL ) return type;
	name = DString_Copy( type->name );
	DString_Append( name, routype->cbtype->name );
	type2 = DaoNamespace_FindType( NS, name );
	DString_Delete( name );
	if( type2 ) return type2;
	type = DaoType_Copy( type );
	DString_Append( type->name, routype->cbtype->name );
	GC_Assign( & type->cbtype, routype->cbtype );
	DaoNamespace_AddType( NS, type->name, type );
	return type;
}

void DaoRoutine_PassParamTypes( DaoRoutine *self, DaoType *selftype,
		DaoType *ts[], int np, int code, DMap *defs )
{
	int npar = np;
	int ndef = self->parCount;
	int ifrom, ito;
	int selfChecked = 0;
	DaoType **parType = self->routType->nested->items.pType;
	DaoType **tps = ts;
	DaoType  *partype, *tp;
	/*
	   printf( "%s %s\n", self->routName->chars, self->routType->name->chars );
	 */
	/* Check for explicit self parameter: */
	if( np && (ts[0]->attrib & DAO_TYPE_SELFNAMED) ) selftype = NULL;
	if( npar == ndef && ndef == 0 ) return;

	/* Remove type holder bindings for the self parameter: */
	if( self->routType->attrib & DAO_TYPE_SELF ){
		partype = (DaoType*) self->routType->nested->items.pType[0]->aux;
		DaoType_ResetTypeHolders( partype, defs );
	}

	if( code == DVM_MCALL && ! (self->routType->attrib & DAO_TYPE_SELF) ){
		npar --;
		tps ++;
	}else if( selftype && (self->routType->attrib & DAO_TYPE_SELF) && code != DVM_MCALL ){
		/* class DaoClass : CppClass{ cppmethod(); } */
		partype = (DaoType*) self->routType->nested->items.pType[0]->aux;
		if( DaoType_MatchTo( selftype, partype, defs ) ) selfChecked = 1;
	}
	for(ifrom=0; ifrom<npar; ifrom++){
		ito = ifrom + selfChecked;
		if( ito >= (int)self->routType->nested->size ) break;
		if( ito < ndef && parType[ito]->tid == DAO_PAR_VALIST ){
			DaoType *vlt = (DaoType*) parType[ito]->aux;
			while( ifrom < npar ) DaoType_MatchTo( tps[ifrom++], vlt, defs );
			break;
		}
		tp = tps[ifrom];
		if( tp == NULL || ito >= ndef ) break;
		partype = parType[ito];
		if( partype->attrib & DAO_TYPE_PARNAMED ) partype = (DaoType*) partype->aux;
		if( tp == NULL || partype == NULL )  break;
		DaoType_MatchTo( tp, partype, defs );
	}
	/*
	   for(node=DMap_First(defs);node;node=DMap_Next(defs,node))
	   printf( "binding:  %s  %s\n", node->key.pType->name->chars, node->value.pType->name->chars );
	 */
	return;
}


enum DaoTypingErrorCode
{
	DTE_TYPE_AMBIGIOUS_PFA = 1,
	DTE_TYPE_NOT_CONSISTENT ,
	DTE_TYPE_NOT_MATCHING ,
	DTE_TYPE_NOT_INITIALIZED,
	DTE_TYPE_WRONG_CONTAINER ,
	DTE_DATA_CANNOT_CREATE ,
	DTE_CALL_INVALID ,
	DTE_CALL_NON_INVAR ,
	DTE_CALL_NOT_PERMIT ,
	DTE_CALL_WITHOUT_INSTANCE ,
	DTE_CALL_INVALID_SECTPARAM ,
	DTE_CALL_INVALID_SECTION ,
	DTE_ROUT_INVALID_YIELD ,
	DTE_ROUT_INVALID_RETURN ,
	DTE_ROUT_INVALID_RETURN2 ,
	DTE_FIELD_NOT_PERMIT ,
	DTE_FIELD_NOT_EXIST ,
	DTE_FIELD_OF_INSTANCE ,
	DTE_INVALID_ENUMERATION ,
	DTE_ITEM_WRONG_ACCESS ,
	DTE_INDEX_NOT_VALID ,
	DTE_KEY_NOT_VALID ,
	DTE_OPERATION_NOT_VALID ,
	DTE_INVALID_INVAR_CAST ,
	DTE_INVAR_VAL_TO_VAR_VAR,
	DTE_PARAM_ERROR ,
	DTE_PARAM_WRONG_NUMBER ,
	DTE_PARAM_WRONG_TYPE ,
	DTE_PARAM_WRONG_NAME ,
	DTE_INVALID_TYPE_CASE ,
	DTE_CONST_WRONG_MODIFYING ,
	DTE_INVALID_INVAR_INITOR ,
	DTE_INVAR_INITOR_MUTABLE ,
	DTE_ROUT_NOT_IMPLEMENTED
};
static const char*const DaoTypingErrorString[] =
{
	"",
	"Ambigious partial function application on overloaded functions",
	"Inconsistent typing",
	"Types not matching",
	"Variable not initialized",
	"Wrong container type",
	"Data cannot be created",
	"Invalid call",
	"Calling non-invar method inside invar method",
	"Calling not permitted",
	"Calling non-static method without instance",
	"Calling with invalid code section parameter",
	"Calling normal method with code section",
	"Invalid yield in ordinary routine",
	"Invalid return for the constructor or defer block",
	"Invalid return type",
	"Member not permitted",
	"Member not exist",
	"Need class instance",
	"Invalid enumeration" ,
	"Invalid index/key access",
	"Invalid index access",
	"Invalid key acess",
	"Invalid operation on the type",
	"Invalid casting from invar type",
	"Invalid assignment from invar value to var varaible",
	"Invalid parameters for the call",
	"Invalid number of parameter",
	"Invalid parameter type",
	"Invalid parameter name",
	"Invalid type case",
	"Constant or invariable cannot be modified",
	"Invalid constructor definition for invar class",
	"Invalid operation that might return external nonprimitive and mutable types",
	"Call to un-implemented function"
};

static DaoType* DaoCheckBinArith0( DaoRoutine *self, DaoVmCodeX *vmc,
		DaoType *at, DaoType *bt, DaoType *ct, DaoClass *hostClass,
		DString *mbs, int setname )
{
	DaoNamespace *ns = self->nameSpace;
	DaoType *dao_type_int = DaoNamespace_MakeType( ns, "int", DAO_INTEGER, NULL, NULL, 0 );
	DaoType *ts[3];
	DaoRoutine *rout = NULL;
	DaoRoutine *rout2 = NULL;
	DNode *node;
	int opa = vmc->a;
	int opc = vmc->c;
	int code = vmc->code;
	int boolop = code >= DVM_AND && code <= DVM_NE;
	ts[0] = ct;
	ts[1] = at;
	ts[2] = bt;
	if( setname && opa == opc && daoBitBoolArithOpers2[code-DVM_NOT] ){
		/* Check composite assignment operator first: */
		DString_SetChars( mbs, daoBitBoolArithOpers2[code-DVM_NOT] );
		if( at->tid == DAO_INTERFACE || at->tid == DAO_CINVALUE ){
			rout = DaoType_FindFunction( at, mbs );
		}else if( at->tid == DAO_OBJECT ){
			rout = DaoClass_FindMethod( & at->aux->xClass, mbs->chars, hostClass );
		}else if( at->tid == DAO_CDATA || at->tid == DAO_CSTRUCT ){
			rout = DaoType_FindFunction( at, mbs );
		}
		if( rout ){
			rout2 = rout;
			/* Check the method with self parameter first, then other methods: */
			rout = DaoRoutine_ResolveX( rout, NULL, ts[1], NULL, ts+2, bt!=NULL, DVM_CALL );
			if( rout == NULL ) rout = DaoRoutine_ResolveX( rout2, NULL, NULL, NULL, ts+1, 1+(bt!=NULL), DVM_CALL );
			/* if the operation is used in the overloaded operator, do operation by address */
			if( boolop && rout == self ) return dao_type_int;
			if( rout ) return ct;
		}
	}
	if( setname ) DString_SetChars( mbs, daoBitBoolArithOpers[code-DVM_NOT] );
	if( at->tid == DAO_INTERFACE || at->tid == DAO_CINVALUE ){
		rout = DaoType_FindFunction( at, mbs );
	}else if( at->tid == DAO_OBJECT ){
		rout = DaoClass_FindMethod( & at->aux->xClass, mbs->chars, hostClass );
	}else if( at->tid == DAO_CDATA || at->tid == DAO_CSTRUCT ){
		rout = DaoType_FindFunction( at, mbs );
	}
	if( rout == NULL ) return NULL;
	rout2 = rout;
	rout = NULL;
	if( ct ){ /* Check methods that can take all three parameters: */
		/* Check the method with self parameter first, then other methods: */
		rout = DaoRoutine_ResolveX( rout2, NULL, ts[0], NULL, ts+1, 1+(bt!=NULL), DVM_CALL );
		if( rout == NULL ) rout = DaoRoutine_ResolveX( rout2, NULL, NULL, NULL, ts, 2+(bt!=NULL), DVM_CALL );
	}
	/* Check the method with self parameter first, then other methods: */
	if( rout == NULL ) rout = DaoRoutine_ResolveX( rout2, NULL, ts[1], NULL, ts+2, bt!=NULL, DVM_CALL );
	if( rout == NULL ) rout = DaoRoutine_ResolveX( rout2, NULL, NULL, NULL, ts+1, 1+(bt!=NULL), DVM_CALL );
	/* if the operation is used in the overloaded operator, do operation by address */
	if( boolop && rout == self ) return dao_type_int;
	return rout ? (DaoType*) rout->routType->aux : NULL;
}
static DaoType* DaoCheckBinArith( DaoRoutine *self, DaoVmCodeX *vmc,
		DaoType *at, DaoType *bt, DaoType *ct, DaoClass *hostClass, DString *mbs )
{
	DaoType *rt = DaoCheckBinArith0( self, vmc, at, bt, ct, hostClass, mbs, 1 );
	if( rt == NULL && (vmc->code == DVM_LT || vmc->code == DVM_LE) ){
		DString_SetChars( mbs, vmc->code == DVM_LT ? ">" : ">=" );
		return DaoCheckBinArith0( self, vmc, bt, at, ct, hostClass, mbs, 0 );
	}
	return rt;
}
static DString* AppendError( DList *errors, DaoValue *rout, size_t type )
{
	DString *s = DString_New();
	DList_Append( errors, rout );
	DList_Append( errors, s );
	DString_AppendChars( s, DaoTypingErrorString[ type ] );
	DString_AppendChars( s, " --- \" " );
	return s;
}
static void DString_AppendTypeError( DString *self, DaoType *from, DaoType *to )
{
	DString_AppendChar( self, '\'' );
	DString_Append( self, from->name );
	DString_AppendChars( self, "\' for \'" );
	DString_Append( self, to->name );
	DString_AppendChars( self, "\' \";\n" );
}
void DaoRoutine_CheckError( DaoNamespace *ns, DaoRoutine *rout, DaoType *routType, DaoType *selftype, DaoType *ts[], int np, int codemode, DList *errors )
{
	DNode *node;
	DString *s;
	DMap *defs = DHash_New(0,0);
	DaoType *abtp, **partypes = routType->nested->items.pType;
	DaoValue *routobj = rout ? (DaoValue*)rout : (DaoValue*)routType;
	int npar = np, size = routType->nested->size;
	int i, j, ndef = 0;
	int ifrom, ito;
	int parpass[DAO_MAX_PARAM];
	int selfChecked = 0, selfMatch = 0;
	int code = codemode & 0xffff;
	int b1 = ((codemode>>16) & DAO_CALL_BLOCK) != 0;
	int b2 = (routType->attrib & DAO_TYPE_CODESECT) != 0;

	if( b1 == 0 && b2 != 0 ){
		DString *s = AppendError( errors, routobj, DTE_CALL_INVALID );
		DString_AppendChars( s, "calling code section method without code section \";\n" );
		goto FinishError;
	}else if( b1 != 0 && b2 == 0 ){
		DString *s = AppendError( errors, routobj, DTE_CALL_INVALID );
		DString_AppendChars( s, "calling normal method with code section \";\n" );
		goto FinishError;
	}

	if( routType->nested ){
		ndef = routType->nested->size;
		if( ndef ){
			abtp = partypes[ ndef-1 ];
			if( abtp->tid == DAO_PAR_VALIST ) ndef = DAO_MAX_PARAM;
		}
	}

#if 0
	printf( "=====================================\n" );
	printf( "%s\n", rout->routName->chars );
	for( j=0; j<npar; j++){
		DaoType *tp = ts[j];
		if( tp != NULL ) printf( "tp[ %i ]: %s\n", j, tp->name->chars );
	}
	printf( "%s %i %i\n", routType->name->chars, ndef, npar );
#endif

	if( code == DVM_MCALL && ! ( routType->attrib & DAO_TYPE_SELF ) ){
		npar --;
		ts ++;
	}else if( selftype && ( routType->attrib & DAO_TYPE_SELF) && code != DVM_MCALL ){
		/* class DaoClass : CppClass{ cppmethod(); } */
		abtp = & partypes[0]->aux->xType;
		selfMatch = DaoType_MatchTo( selftype, abtp, defs );
		if( selfMatch ){
			selfChecked = 1;
			parpass[0] = selfMatch;
			if( DaoType_CheckInvarMatch( selftype, abtp, 1 ) == 0 ){
				DString *s = AppendError( errors, routobj, DTE_PARAM_WRONG_TYPE );
				abtp = DaoType_DefineTypes( abtp, ns, defs );
				DString_AppendTypeError( s, selftype, abtp );
				goto FinishError;
			}
		}
	}
	if( npar == ndef && ndef == 0 ) goto FinishOK;
	if( (npar+selfChecked) > ndef && (size == 0 || partypes[size-1]->tid != DAO_PAR_VALIST ) ){
		DString *s = AppendError( errors, routobj, DTE_PARAM_WRONG_NUMBER );
		DString_AppendChars( s, "too many parameters \";\n" );
		goto FinishError;
	}

	for( j=selfChecked; j<ndef; j++) parpass[j] = 0;
	for(ifrom=0; ifrom<npar; ifrom++){
		DaoType *abtp, *tp = ts[ifrom];
		ito = ifrom + selfChecked;
		abtp = partypes[ito];
		if( abtp->tid == DAO_PAR_VALIST ){
			DaoType *vlt = (DaoType*) abtp->aux;
			for(; ifrom<npar; ifrom++){
				tp = ts[ifrom];
				parpass[ifrom+selfChecked] = vlt ? DaoType_MatchTo( tp, vlt, defs ) : 1;
				if( parpass[ifrom+selfChecked] == 0 ){
					DString *s = AppendError( errors, routobj, DTE_PARAM_WRONG_TYPE );
					abtp = DaoType_DefineTypes( vlt, ns, defs );
					DString_AppendTypeError( s, tp, abtp );
					goto FinishError;
				}
			}
			break;
		}else if( (abtp->attrib & DAO_TYPE_SELFNAMED) && abtp->aux->xType.invar == 0 ){
			if( DaoType_CheckInvarMatch( tp, (DaoType*) abtp->aux, 1 ) == 0 ) goto WrongParamType;
		}
		if( tp == NULL ){
			DString *s = AppendError( errors, routobj, DTE_PARAM_WRONG_TYPE );
			DString_AppendChars( s, "unknown parameter type \";\n" );
			goto FinishError;
		}
		if( tp == NULL ){
			DString *s = AppendError( errors, routobj, DTE_PARAM_WRONG_TYPE );
			DString_AppendChars( s, "unknown parameter type \";\n" );
			goto FinishError;
		}else if( ito >= ndef ){
			DString *s = AppendError( errors, routobj, DTE_PARAM_WRONG_NUMBER );
			DString_AppendChars( s, "too many parameters \";\n" );
			goto FinishError;
		}
		abtp = (DaoType*) routType->nested->items.pType[ito]->aux;
		parpass[ito] = DaoType_MatchTo( tp, abtp, defs );

#if 0
		printf( "%p %s %p %s\n", tp->aux, tp->name->chars, abtp->aux, abtp->name->chars );
		printf( "%i:  %i\n", ito, parpass[ito] );
#endif
		if( parpass[ito] ) continue;

WrongParamType:
		s = AppendError( errors, routobj, DTE_PARAM_WRONG_TYPE );
		abtp = DaoType_DefineTypes( abtp, ns, defs );
		DString_AppendTypeError( s, tp, abtp );
		goto FinishError;
	}
	for(ito=0; ito<ndef; ito++){
		i = partypes[ito]->tid;
		if( i == DAO_PAR_VALIST ) break;
		if( parpass[ito] ) continue;
		if( i != DAO_PAR_DEFAULT ){
			DString *s = AppendError( errors, routobj, DTE_PARAM_WRONG_NUMBER );
			DString_AppendChars( s, "too few parameters \";\n" );
			goto FinishError;
		}
		parpass[ito] = 1;
	}

	/*
	   printf( "%s %i\n", routType->name->chars, *min );
	 */
FinishOK:
FinishError:
	DMap_Delete( defs );
}
DaoRoutine* DaoRoutine_Check( DaoRoutine *self, DaoType *selftype, DaoType *ts[], int np, int codemode, DList *errors )
{
	int i, n;
	DaoRoutine *rout = DaoRoutine_ResolveX( self, NULL, selftype, NULL, ts, np, codemode );
	if( rout ) return rout;
	if( self->overloads == NULL ){
		DaoRoutine_CheckError( self->nameSpace, self, self->routType, selftype, ts, np, codemode, errors );
		return NULL;
	}
	for(i=0,n=self->overloads->routines->size; i<n; i++){
		DaoRoutine *rout = self->overloads->routines->items.pRoutine[i];
		/*
		   printf( "=====================================\n" );
		   printf("ovld %i, %p %s : %s\n", i, rout, self->routName->chars, rout->routType->name->chars);
		 */
		DaoRoutine_CheckError( rout->nameSpace, rout, rout->routType, selftype, ts, np, codemode, errors );
	}
	return NULL;
}

void DaoPrintCallError( DList *errors, DaoStream *stream )
{
	DString *mbs = DString_New();
	int i, j, k, n;
	for(i=0,n=errors->size; i<n; i+=2){
		DaoType *routType = errors->items.pType[i];
		DaoRoutine *rout = NULL;
		if( routType->type == DAO_ROUTINE ){
			rout = errors->items.pRoutine[i];
			routType = rout->routType;
		}
		DaoStream_WriteChars( stream, "  ** " );
		DaoStream_WriteString( stream, errors->items.pString[i+1] );
		DaoStream_WriteChars( stream, "     Assuming  : " );
		if( rout ){
			if( isalpha( rout->routName->chars[0] ) ){
				DaoStream_WriteChars( stream, "routine " );
			}else{
				DaoStream_WriteChars( stream, "operator " );
			}
			k = DString_RFindChars( routType->name, "=>", routType->name->size );
			DString_Assign( mbs, rout->routName );
			DString_AppendChar( mbs, '(' );
			for(j=0; j<routType->nested->size; ++j){
				if( j ) DString_AppendChar( mbs, ',' );
				DString_Append( mbs, routType->nested->items.pType[j]->name );
			}
			DString_AppendChar( mbs, ')' );
			if( routType->cbtype ) DString_Append( mbs, routType->cbtype->name );
			if( routType->aux && routType->aux->type == DAO_TYPE ){
				DString_AppendChars( mbs, "=>" );
				DString_Append( mbs, routType->aux->xType.name );
			}
		}else{
			DaoStream_WriteString( stream, routType->name );
		}
		DString_AppendChars( mbs, ";\n" );
		DaoStream_WriteString( stream, mbs );
		if( rout ){
			DaoStream_WriteChars( stream, "     Reference : " );
			if( rout->body ){
				DaoStream_WriteChars( stream, "line " );
				DaoStream_WriteInt( stream, rout->defLine );
				DaoStream_WriteChars( stream, ", " );
			}
			DaoStream_WriteChars( stream, "file \"" );
			DaoStream_WriteString( stream, rout->nameSpace->name );
			DaoStream_WriteChars( stream, "\";\n" );
		}
		DString_Delete( errors->items.pString[i+1] );
	}
	DString_Delete( mbs );
}

static DaoInode* DaoInferencer_InsertNode( DaoInferencer *self, DaoInode *inode, int code, int addreg, DaoType *type )
{
	DaoInode *next = inode;
	DaoInode *prev = inode->prev;
	int i;

	inode = DaoInode_New();
	*(DaoVmCodeX*)inode = *(DaoVmCodeX*)next;
	inode->code = code;
	if( addreg ){
		inode->c = self->types->size;
		DList_Append( self->types, type );
		DList_Append( self->consts, NULL );
	}
	if( prev ){
		prev->next = inode;
		inode->prev = next;
	}
	inode->next = next;
	next->prev = inode;
	for(i=0; i<self->inodes->size; ++i){
		if( next == self->inodes->items.pInode[i] ){
			DList_Insert( self->inodes, inode, i );
			break;
		}
	}
	/* For proper setting up the jumps: */
	if( next->extra == NULL ) next->extra = inode;
	return inode;
}
static DaoInode* DaoInferencer_InsertMove( DaoInferencer *self, DaoInode *inode, unsigned short *op, DaoType *at, DaoType *ct )
{
	int K = DAO_FLOAT - DAO_BOOLEAN + 1;
	int code = DVM_MOVE_BB + K*(ct->tid - DAO_BOOLEAN) + (at->tid - DAO_BOOLEAN);
	DaoInode *move = DaoInferencer_InsertNode( self, inode, code, 1, ct );
	move->a = *op;
	move->b = 0;
	*op = move->c;
	return move;
}
static DaoInode* DaoInferencer_InsertUntag( DaoInferencer *self, DaoInode *inode, unsigned short *op, DaoType *ct )
{
	DaoInode *cast = DaoInferencer_InsertNode( self, inode, DVM_UNTAG, 1, ct );
	cast->a = *op;
	*op = cast->c;
	return cast;
}
static void DaoInferencer_InsertMove2( DaoInferencer *self, DaoInode *inode, DaoType *at, DaoType *ct )
{
	unsigned short opc = inode->c;
	int K = DAO_FLOAT - DAO_BOOLEAN + 1;
	int code = DVM_MOVE_BB + K*(ct->tid - DAO_BOOLEAN) + (at->tid - DAO_BOOLEAN);
	DaoInode *move = DaoInferencer_InsertNode( self, inode->next, code, 1, at );
	inode->c = move->c;
	move->a = move->c;
	move->c = opc;
}
static void DaoInferencer_Finalize( DaoInferencer *self )
{
	DaoRoutineBody *body = self->routine->body;

	DaoRoutine_CodesFromInodes( self->routine, self->inodes );
	DList_Assign( body->regType, self->types );

	body->regCount = body->regType->size;
	DaoRoutine_SetupSimpleVars( self->routine );
}
static DaoType* DaoInferencer_UpdateTypeX( DaoInferencer *self, int id, DaoType *type, int c )
{
	DaoNamespace *NS = self->routine->nameSpace;
	DaoType **types = self->types->items.pType;
	DMap *defs = (DMap*)DList_Back( self->typeMaps );
	/*
	// Do NOT update types that have been inferred (even as undefined types):
	// Because if it has been inferred, some instructions may have been
	// specialized according to this inferred type. If it is allowed to
	// be updated here, other instructions may be specialized differently.
	// So the previously specialized instruction and the currently specialized
	// instruction will assume different types of the same register!
	//
	// This happens for short curcuit evaluation of boolean operations:
	// expression_produces_string_but_inferred_as_undefined && expression_produces_int
	*/
	if( types[id] != NULL ) return types[id];

	/* If c == 0, the de-const type should be used: */
	if( type->invar && c == 0 ) type = DaoType_GetBaseType( type );

	if( type->attrib & DAO_TYPE_SPEC ) type = DaoType_DefineTypes( type, NS, defs );
	GC_Assign( & types[id], type );
	return types[id];
}
static DaoType* DaoInferencer_UpdateType( DaoInferencer *self, int id, DaoType *type )
{
	return DaoInferencer_UpdateTypeX( self, id, type, 1 );
}
static DaoType* DaoInferencer_UpdateVarType( DaoInferencer *self, int id, DaoType *type )
{
	return DaoInferencer_UpdateTypeX( self, id, type, 0 );
}
void DaoInferencer_PrintCodeSnippet( DaoInferencer *self, DaoStream *stream, int k )
{
	DString* mbs = DString_New();
	DaoVmCodeX **codes = self->inodes->items.pVmc;
	int debug = self->routine->nameSpace->vmSpace->options & DAO_OPTION_DEBUG;
	int prev = debug ? 16 : 1;
	int next = debug ? 8 : 1;
	int j, m = self->routine->body->annotCodes->size;
	int j1 = k >= prev ? k-prev : 0;
	int j2 = (k+next) < m ? k+next : m-1;

	DaoStream_WriteChars( stream, "In code snippet:\n" );
	for(j=j1; j<=j2; ++j){
		DaoRoutine_FormatCode( self->routine, j, *codes[j], mbs );
		DaoStream_WriteChars( stream, j==k ? ">>" : "  " );
		DaoStream_WriteString( stream, mbs );
	}
	DString_Delete( mbs );
}
static void DaoInferencer_WriteErrorHeader2( DaoInferencer *self )
{
	DaoRoutine *routine = self->routine;
	DaoStream  *stream = routine->nameSpace->vmSpace->errorStream;
	int invarinit = !!(routine->attribs & DAO_ROUT_INITOR);
	char char50[50], char200[200];

	if( invarinit ){
		invarinit &= routine->routHost->tid == DAO_OBJECT;
		invarinit &= !!(routine->routHost->aux->xClass.attribs & DAO_CLS_INVAR);
	}

	DaoStream_WriteChars( stream, "[[ERROR]] in file \"" );
	DaoStream_WriteString( stream, routine->nameSpace->name );
	DaoStream_WriteChars( stream, "\":\n" );
	sprintf( char50, "  At line %i : ", routine->defLine );
	DaoStream_WriteChars( stream, char50 );
	if( invarinit ){
		DaoStream_WriteChars( stream, DaoTypingErrorString[DTE_INVALID_INVAR_INITOR] );
	}else{
		DaoStream_WriteChars( stream, "Invalid function definition" );
	}
	DaoStream_WriteChars( stream, " --- \" " );
	DaoStream_WriteString( stream, routine->routName );
	DaoStream_WriteChars( stream, "() \";\n" );
}
static void DaoInferencer_WriteErrorHeader( DaoInferencer *self )
{
	DaoVmCodeX *vmc;
	DaoRoutine *routine = self->routine;
	DaoStream  *stream = routine->nameSpace->vmSpace->errorStream;
	DaoVmCodeX **codes = self->inodes->items.pVmc;
	int invarinit = !!(routine->attribs & DAO_ROUT_INITOR);
	char char50[50], char200[200];

	if( invarinit ){
		invarinit &= routine->routHost->tid == DAO_OBJECT;
		invarinit &= !!(routine->routHost->aux->xClass.attribs & DAO_CLS_INVAR);
	}

	self->error = 1;
	if( self->silent ) return;

	vmc = self->inodes->items.pVmc[self->currentIndex];
	sprintf( char200, "%s:%i,%i,%i", DaoVmCode_GetOpcodeName( vmc->code ), vmc->a, vmc->b, vmc->c );

	DaoInferencer_WriteErrorHeader2( self );

	sprintf( char50, "  At line %i : ", vmc->line );
	DaoStream_WriteChars( stream, char50 );
	DaoStream_WriteChars( stream, "Invalid virtual machine instruction --- \" " );
	DaoStream_WriteChars( stream, char200 );
	DaoStream_WriteChars( stream, " \";\n" );

	DaoInferencer_PrintCodeSnippet( self, stream, self->currentIndex );
}
static void DaoInferencer_WriteErrorGeneral( DaoInferencer *self, int error )
{
	char char50[50];
	DaoRoutine *routine = self->routine;
	DaoStream  *stream = routine->nameSpace->vmSpace->errorStream;
	DaoVmCodeX *vmc = self->inodes->items.pVmc[self->currentIndex];
	DString *mbs;

	if( error == 0 ) return;

	self->error = 1;
	if( self->silent ) return;
	sprintf( char50, "  At line %i : ", vmc->line );

	mbs = DString_New();
	DaoStream_WriteChars( stream, char50 );
	DaoStream_WriteChars( stream, DaoTypingErrorString[error] );
	DaoStream_WriteChars( stream, " --- \" " );
	DaoLexer_AnnotateCode( routine->body->source, *vmc, mbs, 32 );
	DaoStream_WriteString( stream, mbs );
	if( error == DTE_FIELD_NOT_EXIST ){
		DaoStream_WriteChars( stream, " for " );
		DaoStream_WriteChars( stream, self->type_source->name->chars );
	}
	DaoStream_WriteChars( stream, " \";\n" );
	DString_Delete( mbs );
}
static void DaoInferencer_WriteErrorSpecific( DaoInferencer *self, int error )
{
	char char50[50];
	int annot_first = self->annot_first;
	int annot_last = self->annot_last;
	DaoRoutine *routine = self->routine;
	DaoStream  *stream = routine->nameSpace->vmSpace->errorStream;
	DaoVmCodeX *vmc = self->inodes->items.pVmc[self->currentIndex];
	DaoVmCodeX vmc2 = *vmc;
	DString *mbs;

	if( error == 0 ) return;

	self->error = 1;
	if( self->silent ) return;
	sprintf( char50, "  At line %i : ", vmc->line );

	mbs = DString_New();
	DaoStream_WriteChars( stream, char50 );
	DaoStream_WriteChars( stream, DaoTypingErrorString[error] );
	DaoStream_WriteChars( stream, " --- \" " );
	if( error == DTE_TYPE_NOT_INITIALIZED ){
		vmc2.middle = 0;
		vmc2.first = annot_first;
		vmc2.last = annot_last > annot_first ? annot_last - annot_first : 0;
		DaoLexer_AnnotateCode( routine->body->source, vmc2, mbs, 32 );
	}else if( error == DTE_TYPE_NOT_MATCHING || error == DTE_TYPE_NOT_CONSISTENT ){
		DString_SetChars( mbs, "'" );
		DString_AppendChars( mbs, self->type_source ? self->type_source->name->chars : "none" );
		DString_AppendChars( mbs, error == DTE_TYPE_NOT_MATCHING ? "' for '" : "' with '" );
		if( self->type_target ){
			DString_AppendChars( mbs, self->type_target->name->chars );
		}else if( self->tid_target <= DAO_TUPLE ){
			DString_AppendChars( mbs, coreTypeNames[self->tid_target] );
		}
		DString_AppendChar( mbs, '\'' );
	}else{
		DaoLexer_AnnotateCode( routine->body->source, *vmc, mbs, 32 );
	}
	DaoStream_WriteString( stream, mbs );
	DaoStream_WriteChars( stream, " \";\n" );
	DString_Delete( mbs );
}
static int DaoInferencer_Error( DaoInferencer *self, int error )
{
	DaoInferencer_WriteErrorHeader( self );
	if( self->errors->size ){
		DaoPrintCallError( self->errors, self->routine->nameSpace->vmSpace->errorStream );
		return 0;
	}
	DaoInferencer_WriteErrorGeneral( self, error );
	return 0;
}
static int DaoInferencer_ErrorModifyConst( DaoInferencer *self )
{
	return DaoInferencer_Error( self, DTE_CONST_WRONG_MODIFYING );
}
static int DaoInferencer_ErrorTypeNotMatching( DaoInferencer *self, DaoType *S, DaoType *T )
{
	if( S ) self->type_source = S;
	if( T ) self->type_target = T;
	DaoInferencer_WriteErrorHeader( self );
	DaoInferencer_WriteErrorGeneral( self, DTE_OPERATION_NOT_VALID );
	DaoInferencer_WriteErrorSpecific( self, DTE_TYPE_NOT_MATCHING );
	return 0;
}
static int DaoInferencer_ErrorTypeNotConsistent( DaoInferencer *self, DaoType *S, DaoType *T )
{
	if( S ) self->type_source = S;
	if( T ) self->type_target = T;
	DaoInferencer_WriteErrorHeader( self );
	DaoInferencer_WriteErrorGeneral( self, DTE_OPERATION_NOT_VALID );
	DaoInferencer_WriteErrorSpecific( self, DTE_TYPE_NOT_CONSISTENT );
	return 0;
}
static int DaoInferencer_ErrorTypeID( DaoInferencer *self, DaoType *S, int tid )
{
	self->type_source = S;
	self->tid_target = tid;
	DaoInferencer_WriteErrorHeader( self );
	DaoInferencer_WriteErrorGeneral( self, DTE_TYPE_NOT_MATCHING );
	DaoInferencer_WriteErrorSpecific( self, DTE_TYPE_NOT_MATCHING );
	return 0;
}
static int DaoInferencer_ErrorNotInitialized( DaoInferencer *self, int error, int first, int last )
{
	self->annot_first = first;
	self->annot_last = last;
	DaoInferencer_WriteErrorHeader( self );
	DaoInferencer_WriteErrorGeneral( self, error );
	DaoInferencer_WriteErrorSpecific( self, DTE_TYPE_NOT_INITIALIZED );
	return 0;
}
static int DaoInferencer_ErrorInvalidIndex( DaoInferencer *self )
{
	return DaoInferencer_Error( self, DTE_INDEX_NOT_VALID );
}

#define NoCheckingType(t) (t->tid & DAO_ANY)

static int DaoInferencer_AssertPairNumberType( DaoInferencer *self, DaoType *type )
{
	DaoType *itp = type->nested->items.pType[0];
	if( itp->tid == DAO_PAR_NAMED ) itp = & itp->aux->xType;
	if( itp->tid > DAO_FLOAT && ! NoCheckingType(itp) ) return 0;
	itp = type->nested->items.pType[1];
	if( itp->tid == DAO_PAR_NAMED ) itp = & itp->aux->xType;
	if( itp->tid > DAO_FLOAT && ! NoCheckingType(itp) ) return 0;
	return 1;
}
static DaoType* DaoType_GetAutoCastType2( DaoType *self )
{
	if( self->tid != DAO_VARIANT ) return NULL;
	if( self->nested->size == 1 ){
		return self->nested->items.pType[0];
	}else if( self->nested->size == 2 ){
		DaoType *T1 = self->nested->items.pType[0];
		DaoType *T2 = self->nested->items.pType[1];
		if( T1->tid == DAO_NONE ) return T2;
		if( T2->tid == DAO_NONE ) return T1;
	}
	return NULL;
}
static DaoType* DaoType_GetAutoCastType( DaoType *self )
{
	int invar = self->invar;
	int konst = self->konst;
	DaoType *type = DaoType_GetAutoCastType2( self );
	if( type == NULL ) return NULL;
	if( konst ) return DaoType_GetConstType( type );
	if( invar ) return DaoType_GetInvarType( type );
	return type;
}


#define AssertTypeMatching( source, target, defs ) \
	if( !(source->tid & DAO_ANY ) && DaoType_MatchTo( source, target, defs ) ==0 ) \
		return DaoInferencer_ErrorTypeNotMatching( self, source, target );

#define AssertTypeIdMatching( source, id ) \
	if( source == NULL || source->tid != id ) \
		return DaoInferencer_ErrorTypeID( self, source, id );

#define AssertPairNumberType( tp ) \
	if( DaoInferencer_AssertPairNumberType( self, tp ) == 0 ) \
		return DaoInferencer_ErrorTypeNotMatching( self, NULL, NULL );

static DaoType* DaoInferencer_HandleSlicedType( DaoInferencer *self, DaoType *type )
{
	int i, tid = type->tid;
	const char *name = coreTypeNames[tid];
	DaoNamespace *NS = self->routine->nameSpace;
	DList *types = self->array;

	DList_Clear( types );
	for(i=0; i<type->nested->size; ++i){
		DaoType *it = type->nested->items.pType[i];
		if( it->tid && it->tid <= DAO_ENUM ){
			it = DaoType_GetBaseType( it );
		}else if( it->tid == DAO_PAR_NAMED || it->tid == DAO_PAR_VALIST ){
			const char *fn = it->fname->chars;
			DaoType *tp = (DaoType*) it->aux;
			if( tp->tid && tp->tid <= DAO_ENUM ){
				tp = DaoType_GetBaseType( tp );
			}else{
				tp = DaoType_GetInvarType( tp );
			}
			it = DaoNamespace_MakeType( NS, fn, it->tid, (DaoValue*) tp, NULL, 0 );
		}else{
			it = DaoType_GetInvarType( it );
		}
		DList_Append( types, it );
	}
	return DaoNamespace_MakeType( NS, name, tid, NULL, types->items.pType, types->size );
}

int DaoInferencer_HandleGetItem( DaoInferencer *self, DaoInode *inode, DMap *defs )
{
	int code = inode->code;
	int opa = inode->a;
	int opb = inode->b;
	int opc = inode->c;
	DList *errors = self->errors;
	DString *mbs = self->mbstring;
	DaoInteger integer = {DAO_INTEGER,0,0,0,1,0};
	DaoType *type, **tp, **types = self->types->items.pType;
	DaoNamespace *NS = self->routine->nameSpace;
	DaoClass *hostClass = self->hostClass;
	DaoVmCodeX *vmc = (DaoVmCodeX*) inode;
	DaoRoutine *rout, *meth = NULL;
	DaoType *at = types[opa];
	DaoType *bt = types[opb];
	DaoType *ct = types[opc];
	DaoValue *value;
	DNode *node;
	int k;

	integer.value = opb;
	value = (DaoValue*)(DaoInteger*)&integer;
	bt = dao_type_int;
	if( code == DVM_GETI ){
		bt = types[opb];
		value = self->consts->items.pValue[opb];
	}
	ct = NULL;
	k = at->tid != DAO_CLASS && at->tid != DAO_OBJECT;
	k = k && at->tid != DAO_CDATA && at->tid != DAO_CSTRUCT;
	if( bt->tid == DAO_NONE && bt->valtype ){ /* a[] or a[:] */
		ct = at;
	}else if( NoCheckingType( at ) || NoCheckingType( bt ) ){
		/* allow less strict typing: */
		ct = dao_type_udf;
	}else if( at->tid == DAO_PAR_NAMED && code == DVM_GETDI ){
		ct = opb ? (DaoType*) at->aux : dao_type_string;
	}else if( at->tid == DAO_BOOLEAN || at->tid == DAO_INTEGER ){
		goto InvIndex;
	}else if( at->tid == DAO_STRING ){
		ct = at;
		if( bt->realnum ){
			ct = dao_type_int;
			if( code == DVM_GETI ){
				if( bt->tid != DAO_INTEGER ){
					DaoInferencer_InsertMove( self, inode, & inode->b, bt, dao_type_int );
					bt = dao_type_int;
				}
				if( bt->tid == DAO_INTEGER ) vmc->code = DVM_GETI_SI;
			}
		}else if( bt == dao_type_for_iterator ){
			ct = dao_type_int;
		}else if( bt->tid ==DAO_TUPLE && bt->subtid == DAO_PAIR ){
			ct = at;
			AssertPairNumberType( bt );
		}else if( bt->tid ==DAO_LIST || bt->tid ==DAO_ARRAY ){
			/* passed */
			k = bt->nested->items.pType[0]->tid;
			if( k > DAO_FLOAT && k !=DAO_ANY ) goto NotMatch;
		}
	}else if( at->tid == DAO_TYPE ){
		at = at->nested->items.pType[0];
		if( at->tid == DAO_ENUM && at->mapNames ){
			ct = at; /* TODO const index */
		}else{
			self->type_source = at;
			goto NotExist;
		}
	}else if( at->tid == DAO_LIST ){
		if( bt->realnum ){
			ct = at->nested->items.pType[0];
			if( code == DVM_GETI ){
				if( ct->tid >= DAO_BOOLEAN && ct->tid <= DAO_COMPLEX ){
					vmc->code = DVM_GETI_LBI + ct->tid - DAO_BOOLEAN;
				}else if( ct->tid == DAO_STRING ){
					vmc->code = DVM_GETI_LSI;
				}else if( ct->tid >= DAO_ARRAY && ct->tid < DAO_ANY ){
					/* for skipping type checking */
					vmc->code = DVM_GETI_LI;
				}
				if( bt->tid != DAO_INTEGER )
					DaoInferencer_InsertMove( self, inode, & inode->b, bt, dao_type_int );
			}
		}else if( bt == dao_type_for_iterator ){
			ct = at->nested->items.pType[0];
		}else if( bt->tid == DAO_TUPLE && bt->subtid == DAO_PAIR ){
			ct = at;
			AssertPairNumberType( bt );
		}else if( bt->tid == DAO_LIST || bt->tid == DAO_ARRAY ){
			ct = at;
			k = bt->nested->items.pType[0]->tid;
			if( k != DAO_INTEGER && k != DAO_ANY && k != DAO_UDT ) goto NotMatch;
		}else{
			goto InvIndex;
		}
	}else if( at->tid == DAO_MAP ){
		DaoType *t0 = at->nested->items.pType[0];
		/*
		   printf( "at %s %s\n", at->name->chars, bt->name->chars );
		 */
		if( bt == dao_type_for_iterator ){
			ct = DaoNamespace_MakeType( NS, "tuple", DAO_TUPLE,
					NULL, at->nested->items.pType, 2 );
		}else if( bt->tid == DAO_TUPLE && bt->subtid == DAO_PAIR ){  /* Check slicing: */
			DaoType *start, *end, **kts = bt->nested->items.pType;
			int openStart = 0, openEnd = 0;
			start = kts[0]->tid == DAO_PAR_NAMED ? (DaoType*) kts[0]->aux : kts[0];
			end   = kts[1]->tid == DAO_PAR_NAMED ? (DaoType*) kts[1]->aux : kts[1];
			openStart = start->tid == DAO_NONE && start->valtype;
			openEnd   = end->tid == DAO_NONE   && end->valtype;
			if( !openStart && DaoType_MatchTo( start, t0, defs ) == 0 ) goto InvKey;
			if( !openEnd && DaoType_MatchTo( end, t0, defs ) == 0 ) goto InvKey;
			ct = at;
		}else{
			if( DaoType_MatchTo( bt, t0, defs ) == 0 ) goto InvKey;
			ct = at->nested->items.pType[1];
		}
	}else if( at->tid == DAO_ARRAY ){
		if( bt->realnum ){
			/* array[i] */
			ct = at->nested->items.pType[0];
			if( code == DVM_GETI ){
				if( ct->realnum || ct->tid == DAO_COMPLEX )
					vmc->code = DVM_GETI_ABI + ct->tid - DAO_BOOLEAN;
				if( bt->tid != DAO_INTEGER )
					DaoInferencer_InsertMove( self, inode, & inode->b, bt, dao_type_int );
			}
		}else if( bt == dao_type_for_iterator ){
			ct = at->nested->items.pType[0];
		}else if( bt->tid == DAO_TUPLE && bt->subtid == DAO_PAIR ){
			ct = at;
			AssertPairNumberType( bt );
		}else if( bt->tid == DAO_LIST || bt->tid == DAO_ARRAY ){
			/* array[ {1,2,3} ] or array[ [1,2,3] ] */
			ct = at;
			k = bt->nested->items.pType[0]->tid;
			if( k != DAO_INTEGER && k != DAO_ANY && k != DAO_UDT ) goto NotMatch;
		}
	}else if( at->tid == DAO_TUPLE ){
		DaoTuple *tupidx = DaoValue_CastTuple( value );
		ct = dao_type_udf;
		/* tuple slicing with constant index range, will produce a tuple with type
		// determined from the index range. For variable range, it produces tuple<...>. */
		if( tupidx && tupidx->subtype == DAO_PAIR ){
			DaoValue *first = value->xTuple.values[0];
			DaoValue *second = value->xTuple.values[1];
			daoint start = DaoValue_GetInteger( first );
			daoint end = DaoValue_GetInteger( second );
			/* Note: a tuple may contain more items than what are explicitly typed. */
			if( start < 0 || end < 0 ) goto InvIndex; /* No support for negative index; */
			if( at->variadic == 0 && start >= at->nested->size ) goto InvIndex;
			if( at->variadic == 0 && end >= at->nested->size ) goto InvIndex;
			if( first->type > DAO_FLOAT || second->type > DAO_FLOAT ) goto InvIndex;
			if( first->type == DAO_NONE && second->type == DAO_NONE ){
				ct = at;
			}else{
				end = second->type == DAO_NONE ? at->nested->size : end + 1;
				if( end >= at->nested->size ) end = at->nested->size;
				if( start >= at->nested->size ) end = start;
				tp = at->nested->items.pType + start;
				ct = DaoNamespace_MakeType( NS, "tuple", DAO_TUPLE, NULL, tp, end-start );
			}
		}else if( value && value->type == DAO_NONE ){
			ct = at;
		}else if( value && value->type ){
			if( value->type > DAO_FLOAT ) goto InvIndex;
			k = DaoValue_GetInteger( value );
			if( k < 0 ) goto InvIndex; /* No support for negative index; */
			if( at->variadic && k >= (at->nested->size - 1) ){
				type = (DaoType*) at->nested->items.pType[at->nested->size-1]->aux;
				ct = type ? type : dao_type_any;
			}else{
				if( k >= at->nested->size ) goto InvIndex;
				ct = at->nested->items.pType[ k ];
				if( ct->tid == DAO_PAR_NAMED ) ct = & ct->aux->xType;
				if( k <= 0xffff ){
					vmc->b = k;
					if( ct->tid >= DAO_BOOLEAN && ct->tid <= DAO_COMPLEX ){
						vmc->code = DVM_GETF_TB + ( ct->tid - DAO_BOOLEAN );
					}else{
						/* for skipping type checking */
						vmc->code = DVM_GETF_TX;
					}
				}
			}
		}else if( bt == dao_type_for_iterator ){
			DaoType *itypes[2];
			int j;
			if( at->nested->size == 0 ) goto InvIndex;
			ct = at->nested->items.pType[0];
			if( ct->tid >= DAO_PAR_NAMED && ct->tid <= DAO_PAR_VALIST ){
				ct = (DaoType*) ct->aux;
			}
			for(j=1; j<at->nested->size; ++j){
				DaoType *it = at->nested->items.pType[j];
				if( it->tid >= DAO_PAR_NAMED && it->tid <= DAO_PAR_VALIST ){
					it = (DaoType*) it->aux;
				}
				if( DaoType_MatchTo( it, ct, defs ) < DAO_MT_EQ ){
					ct = dao_type_any;
					break;
				}
			}
			itypes[0] = dao_type_string;
			itypes[1] = ct;
			ct = DaoNamespace_MakeType( NS, "tuple", DAO_TUPLE, NULL, itypes, 2 );
		}else if( bt->realnum ){
			ct = DaoNamespace_MakeType( NS, "", DAO_VARIANT, NULL, at->nested->items.pType, at->nested->size );
			if( code == DVM_GETI ){
				vmc->code = DVM_GETI_TI;
				if( bt->tid != DAO_INTEGER )
					DaoInferencer_InsertMove( self, inode, & inode->b, bt, dao_type_int );
			}
		}else if( bt->tid == DAO_TUPLE && bt->subtid == DAO_PAIR ){
			ct = dao_type_tuple;
		}else if( bt->tid != DAO_UDT && bt->tid != DAO_ANY ){
			goto InvIndex;
		}
	}else if( at->tid == DAO_CLASS || at->tid == DAO_OBJECT ){
		meth = DaoClass_FindMethod( & at->aux->xClass, "[]", hostClass );
		if( meth == NULL ) goto InvIndex;
	}else if( at->tid == DAO_CDATA || at->tid == DAO_CSTRUCT || at->tid == DAO_CTYPE ){
		DString_SetChars( mbs, "[]" );
		meth = DaoType_FindFunction( at, mbs );
		if( meth == NULL ) goto WrongContainer;
	}else if( at->tid == DAO_INTERFACE || at->tid == DAO_CINTYPE || at->tid == DAO_CINVALUE ){
		DString_SetChars( mbs, "[]" );
		meth = DaoType_FindFunction( at, mbs );
		if( meth == NULL ) goto WrongContainer;
	}else if( at->tid & DAO_ANY ){
		ct = dao_type_udf;
	}else if( at->typer ){
		/* Use at->typer instead of at->kernel, because at->kernel may still be NULL,
		 * if the type is created before the setup of the typer structure. */
		DString_SetChars( mbs, "[]" );
		meth = DaoType_FindFunction( at, mbs );
		if( meth == NULL ) goto WrongContainer;
	}else{
		goto WrongContainer;
	}
	if( meth ){
		rout = DaoRoutine_Check( meth, at, & bt, 1, DVM_CALL, errors );
		if( rout == NULL ) goto InvIndex;
		ct = & rout->routType->aux->xType;
	}
	if( ct == NULL ) goto InvKey;
	if( at->tid >= DAO_ARRAY ){
		if( at->konst && ct->konst == 0 ) ct = DaoType_GetConstType( ct );
		if( at->invar && ct->invar == 0 ) ct = DaoType_GetInvarType( ct );
		if( bt->tid == DAO_NONE || (bt->tid == DAO_TUPLE && bt->subtid == DAO_PAIR) ){
			if( ct->invar && ct->konst == 0 ){
				if( at->tid == DAO_ARRAY ){
					ct = DaoType_GetBaseType( ct );
				}else if( at->tid == DAO_LIST || at->tid == DAO_MAP || at->tid == DAO_TUPLE ){
					ct = DaoInferencer_HandleSlicedType( self, ct );
				}
			}
		}
	}else{
		if( at->invar && ct->invar ) ct = DaoType_GetBaseType( ct );
	}
	DaoInferencer_UpdateType( self, opc, ct );
	AssertTypeMatching( ct, types[opc], defs );
	return 1;
NotMatch : return DaoInferencer_ErrorTypeNotMatching( self, NULL, NULL );
NotExist : return DaoInferencer_Error( self, DTE_FIELD_NOT_EXIST );
WrongContainer : return DaoInferencer_Error( self, DTE_TYPE_WRONG_CONTAINER );
InvIndex : return DaoInferencer_Error( self, DTE_INDEX_NOT_VALID );
InvKey : return DaoInferencer_Error( self, DTE_KEY_NOT_VALID );
}
int DaoInferencer_HandleGetMItem( DaoInferencer *self, DaoInode *inode, DMap *defs )
{
	int opa = inode->a;
	int opb = inode->b;
	int opc = inode->c;
	DList *errors = self->errors;
	DString *mbs = self->mbstring;
	DaoType *type, **types = self->types->items.pType;
	DaoNamespace *NS = self->routine->nameSpace;
	DaoClass *hostClass = self->hostClass;
	DaoRoutine *meth, *rout;
	DaoType *at = types[opa];
	DaoType *ct = types[opc];
	DaoInode *inode2;
	DNode *node;
	int j;

	ct = at;
	meth = NULL;
	DString_SetChars( mbs, "[]" );
	if( opb == 0 ){
		ct = at;
	}else if( NoCheckingType( at ) ){
		/* allow less strict typing: */
		ct = dao_type_udf;
	}else if( at->tid == DAO_STRING ){
		if( opb > 2 ) goto InvIndex;
		if( types[opa+1]->tid == DAO_NONE ) goto InvIndex;
		if( types[opa+1]->tid > DAO_FLOAT ) goto InvIndex;
		if( types[opa+2]->tid != DAO_NONE ) goto InvIndex;
		ct = at;
		if( types[opa+2]->tid != DAO_NONE ) ct = dao_type_int;
	}else if( at->tid == DAO_ARRAY ){
		int max = DAO_NONE, min = DAO_COMPLEX;
		ct = type = at->nested->items.pType[0];
		for(j=1; j<=opb; j++){
			int tid = types[j+opa]->tid;
			if( tid > max ) max = tid;
			if( tid < min ) min = tid;
		}
		if( min == DAO_NONE || max > DAO_FLOAT ) ct = at;
		if( ct->tid && ct->tid <= DAO_COMPLEX ){
			if( min >= DAO_INTEGER && max <= DAO_FLOAT ){
				inode->code = DVM_GETMI_ABI + (ct->tid - DAO_BOOLEAN);
				if( max > DAO_INTEGER ){
					inode2 = DaoInferencer_InsertNode( self, inode, DVM_MOVE_PP, 1, at );
					inode2->a = inode->a;
					inode->a = self->types->size - 1;
					for(j=1; j<=opb; j++){
						unsigned short op = j+opa;
						DaoInferencer_InsertMove( self, inode, & op, types[j+opa], dao_type_int );
					}
				}
			}
		}
	}else if( at->tid == DAO_MAP ){
		goto InvIndex;
	}else if( at->tid == DAO_CLASS || at->tid == DAO_OBJECT ){
		meth = DaoClass_FindMethod( & at->aux->xClass, "[]", hostClass );
		if( meth == NULL ) goto WrongContainer;
	}else if( at->tid == DAO_CDATA || at->tid == DAO_CSTRUCT || at->tid == DAO_CTYPE ){
		meth = DaoType_FindFunction( at, mbs );
		if( meth == NULL ) goto WrongContainer;
	}else if( at->tid == DAO_INTERFACE || at->tid == DAO_CINTYPE || at->tid == DAO_CINVALUE ){
		meth = DaoType_FindFunction( at, mbs );
		if( meth == NULL ) goto WrongContainer;
	}else if( at->typer ){
		meth = DaoType_FindFunction( at, mbs );
		if( meth == NULL ) goto WrongContainer;
	}
	if( meth ){
		/* TODO, self type for class? */
		rout = DaoRoutine_Check( meth, at, types+opa+1, opb, DVM_CALL, errors );
		if( rout == NULL ) goto InvIndex;
		ct = & rout->routType->aux->xType;
	}
	if( at->tid >= DAO_ARRAY ){
		if( at->konst && ct->konst == 0 ) ct = DaoType_GetConstType( ct );
		if( at->invar && ct->invar == 0 ) ct = DaoType_GetInvarType( ct );
	}else{
		if( at->invar && ct->invar ) ct = DaoType_GetBaseType( ct );
	}
	DaoInferencer_UpdateType( self, opc, ct );
	AssertTypeMatching( ct, types[opc], defs );
	return 1;
WrongContainer : return DaoInferencer_Error( self, DTE_TYPE_WRONG_CONTAINER );
InvIndex : return DaoInferencer_Error( self, DTE_INDEX_NOT_VALID );
}
int DaoInferencer_HandleGetField( DaoInferencer *self, DaoInode *inode, DMap *defs )
{
	int code = inode->code;
	int opa = inode->a;
	int opb = inode->b;
	int opc = inode->c;
	DList *errors = self->errors;
	DString *str, *mbs = self->mbstring;
	DList  *routConsts = self->routine->routConsts->value;
	DaoType **type2, **types = self->types->items.pType;
	DaoValue *value, **consts = self->consts->items.pValue;
	DaoNamespace *NS = self->routine->nameSpace;
	DaoClass *klass, *hostClass = self->hostClass;
	DaoVmCodeX *vmc = (DaoVmCodeX*) inode;
	DaoRoutine *meth, *rout, *rout2;
	DaoType *at = types[opa];
	DaoType *ct = types[opc];
	DNode *node;
	int k;
	DaoType **pars = NULL;
	int npar = 0;
	int ak = 0;

	ct = NULL;
	value = routConsts->items.pValue[opb];
	if( value == NULL || value->type != DAO_STRING ) goto NotMatch;
	str = value->xString.value;
	ak = at->tid == DAO_CLASS;
	self->type_source = at;
	if( NoCheckingType( at ) ){
		/* allow less strict typing: */
		ct = dao_type_udf;
	}else if( at->tid == DAO_COMPLEX ){
		if( strcmp( str->chars, "real" ) && strcmp( str->chars, "imag" ) ) goto NotExist;
		ct = dao_type_float;
		inode->code = DVM_GETF_CX;
		inode->b = strcmp( str->chars, "imag" ) == 0;
	}else if( at->tid == DAO_TYPE ){
		self->type_source = at;
		at = at->nested->items.pType[0];
		if( at->tid == DAO_ENUM && at->mapNames ){
			if( DMap_Find( at->mapNames, str ) == NULL ) goto NotExist;
			ct = at;
		}else{
			goto NotExist;
		}
	}else if( at->tid == DAO_INTERFACE || at->tid == DAO_CINTYPE || at->tid == DAO_CINVALUE ){
		meth = DaoType_FindFunction( at, str );
		if( meth ){
			ct = meth->routType;
		}else{
			DString_SetChars( mbs, "." );
			DString_Append( mbs, str );
			meth = DaoType_FindFunction( at, mbs );
			if( meth == NULL ){
				pars = & dao_type_string;
				npar = 1;
				DString_SetChars( mbs, "." );
				meth = DaoType_FindFunction( at, mbs );
			}
			if( meth == NULL ) goto NotExist;
			rout = DaoRoutine_Check( meth, at, pars, npar, DVM_CALL, errors );
			if( rout == NULL ) goto NotExist;
			ct = & rout->routType->aux->xType;
		}
	}else if( at->tid == DAO_CLASS || at->tid == DAO_OBJECT ){
		DaoValue *data;
		int j, getter = 0;
		klass = (DaoClass*) at->aux;
		data = DaoClass_GetData( klass, str, hostClass );
		if( data == NULL || data->type == DAO_NONE ){
			DString_SetChars( mbs, "." );
			DString_Append( mbs, str );
			data = DaoClass_GetData( klass, mbs, hostClass );
			if( data == NULL ){
				pars = & dao_type_string;
				npar = 1;
				DString_SetChars( mbs, "." );
				data = DaoClass_GetData( klass, mbs, hostClass );
			}
			if( data && data->type == DAO_CONSTANT && data->xConst.value->type == DAO_ROUTINE ){
				rout2 = rout = (DaoRoutine*) data->xConst.value;
				rout = DaoRoutine_Check( rout, at, pars, npar, DVM_CALL, errors );
				if( rout == NULL ) goto NotMatch;
				ct = & rout->routType->aux->xType;
				getter = 1;
				if( at->konst && ct->konst == 0 ) ct = DaoType_GetConstType( ct );
				if( at->invar && ct->invar == 0 ) ct = DaoType_GetInvarType( ct );
				DaoInferencer_UpdateType( self, opc, ct );
				AssertTypeMatching( ct, types[opc], defs );
			}else{
				goto InvOper;
			}
		}
		if( data == NULL ) goto NotExist;
		if( data->type == DAO_NONE ) goto NotPermit;
		if( data->xBase.subtype == DAO_OBJECT_VARIABLE && at->tid ==DAO_CLASS ) goto NeedInstVar;
		if( data->xBase.subtype == DAO_CLASS_VARIABLE ) consts[opc] = NULL;
		if( getter ) return 1;
		if( data->xBase.subtype == DAO_CLASS_CONSTANT ){
			ct = DaoNamespace_GetType( NS, data->xConst.value );
			GC_Assign( & consts[opc], data->xConst.value );
		}else{
			ct = data->xVar.dtype;
		}
		if( ct == NULL ) ct = dao_type_any; /* static declared without type and init; */
		j = DaoClass_GetDataIndex( klass, str );

		/* specialize instructions for finalized class/instance: */
		k = LOOKUP_ST( j );
		vmc->b = LOOKUP_ID( j );
		if( ct && ct->tid >= DAO_BOOLEAN && ct->tid <= DAO_COMPLEX ){
			switch( k ){
			case DAO_CLASS_CONSTANT : code = ak ? DVM_GETF_KCB : DVM_GETF_OCB; break;
			case DAO_CLASS_VARIABLE : code = ak ? DVM_GETF_KGB : DVM_GETF_OGB; break;
			case DAO_OBJECT_VARIABLE : code = DVM_GETF_OVB; break;
			}
			code += ct->tid - DAO_BOOLEAN;
		}else{
			switch( k ){
			case DAO_CLASS_CONSTANT : code = ak ? DVM_GETF_KC : DVM_GETF_OC; break;
			case DAO_CLASS_VARIABLE : code = ak ? DVM_GETF_KG : DVM_GETF_OG; break;
			case DAO_OBJECT_VARIABLE : code = DVM_GETF_OV; break;
			}
		}
		vmc->code = code;
	}else if( at->tid == DAO_TUPLE ){
		if( at->mapNames == NULL ) goto NotExist;
		node = MAP_Find( at->mapNames, str );
		if( node == NULL ) goto NotExist;
		k = node->value.pInt;
		if( k <0 || k >= (int)at->nested->size ) goto NotExist;
		ct = at->nested->items.pType[ k ];
		if( ct->tid == DAO_PAR_NAMED ) ct = & ct->aux->xType;
		if( k < 0xffff ){
			if( ct->tid >= DAO_BOOLEAN && ct->tid <= DAO_COMPLEX ){
				vmc->code = DVM_GETF_TB + ( ct->tid - DAO_BOOLEAN );
				vmc->b = k;
			}else{
				/* for skipping type checking */
				vmc->code = DVM_GETF_TX;
				vmc->b = k;
			}
		}
	}else if( at->tid == DAO_NAMESPACE ){
		ct = dao_type_udf;
		if( consts[opa] && consts[opa]->type == DAO_NAMESPACE ){
			DaoNamespace *ans = & consts[opa]->xNamespace;
			k = DaoNamespace_FindVariable( ans, str );
			if( k >=0 ){
				ct = DaoNamespace_GetVariableType( ans, k );
			}else{
				k = DaoNamespace_FindConst( ans, str );
				value = DaoNamespace_GetConst( ans, k );
				if( value ) ct = DaoNamespace_GetType( ans, value );
			}
			if( k <0 ) goto NotExist;
		}
	}else if( at->typer ){
		value = DaoType_FindValue( at, str );
		if( value && value->type == DAO_ROUTINE ){
			DaoRoutine *func = (DaoRoutine*) value;
			ct = func->routType;
			GC_Assign( & consts[opc], value );
		}else if( value ){
			ct = DaoNamespace_GetType( NS, value );
			GC_Assign( & consts[opc], value );
		}else{
			DString_SetChars( mbs, "." );
			DString_Append( mbs, str );
			meth = DaoType_FindFunction( at, mbs );
			if( meth == NULL ){
				pars = & dao_type_string;
				npar = 1;
				DString_SetChars( mbs, "." );
				meth = DaoType_FindFunction( at, mbs );
			}
			if( meth == NULL ) goto NotExist;
			rout = DaoRoutine_Check( meth, at, pars, npar, DVM_CALL, errors );
			if( rout == NULL ) goto NotMatch;
			ct = & rout->routType->aux->xType;
		}
		if( ct == NULL ) ct = dao_type_udf;
	}
	if( ct && ct->tid == DAO_ROUTINE && (ct->attrib & DAO_TYPE_SELF) ){
		DaoType *selftype = (DaoType*) ct->nested->items.pType[0]->aux;
		/* Remove type holder bindings for the self parameter: */
		DaoType_ResetTypeHolders( selftype, defs );
		DaoType_MatchTo( at, selftype, defs );
		ct = DaoType_DefineTypes( ct, NS, defs );
	}
	if( at->tid != DAO_CLASS && at->tid != DAO_NAMESPACE ){
		if( at->konst && ct->konst == 0 ) ct = DaoType_GetConstType( ct );
		if( at->invar && ct->invar == 0 ) ct = DaoType_GetInvarType( ct );
	}
	DaoInferencer_UpdateType( self, opc, ct );
	AssertTypeMatching( ct, types[opc], defs );
	return 1;
NotMatch : return DaoInferencer_ErrorTypeNotMatching( self, NULL, NULL );
NotPermit : return DaoInferencer_Error( self, DTE_FIELD_NOT_PERMIT );
NotExist : return DaoInferencer_Error( self, DTE_FIELD_NOT_EXIST );
NeedInstVar : return DaoInferencer_Error( self, DTE_FIELD_OF_INSTANCE );
InvOper : return DaoInferencer_Error( self, DTE_OPERATION_NOT_VALID );
}
int DaoInferencer_HandleSetItem( DaoInferencer *self, DaoInode *inode, DMap *defs )
{
	int code = inode->code;
	int opa = inode->a;
	int opb = inode->b;
	int opc = inode->c;
	DList *errors = self->errors;
	DString *mbs = self->mbstring;
	DaoInteger integer = {DAO_INTEGER,0,0,0,1,0};
	DaoType *ts[DAO_ARRAY+DAO_MAX_PARAM];
	DaoType *tt, *type, **types = self->types->items.pType;
	DaoValue *value, **consts = self->consts->items.pValue;
	DaoClass *hostClass = self->hostClass;
	DaoVmCodeX *vmc = (DaoVmCodeX*) inode;
	DaoRoutine *meth, *rout;
	DaoType *at = types[opa];
	DaoType *bt = types[opb];
	DaoType *ct = types[opc];
	int k, K;

	if( ct == NULL ) goto ErrorTyping;
	integer.value = opb;
	value = (DaoValue*)(DaoInteger*)&integer;
	bt = dao_type_int;
	if( code == DVM_SETI ){
		bt = types[opb];
		value = consts[opb];
	}
	if( NoCheckingType(at) || NoCheckingType(bt) || NoCheckingType(ct) ) return 1;
	switch( ct->tid ){
	case DAO_STRING :
		if( code == DVM_SETI ){
			if( at->realnum && bt->realnum ){
				vmc->code = DVM_SETI_SII;
				if( at->tid != DAO_INTEGER )
					DaoInferencer_InsertMove( self, inode, & inode->a, at, dao_type_int );
				if( bt->tid != DAO_INTEGER )
					DaoInferencer_InsertMove( self, inode, & inode->b, bt, dao_type_int );
			}
		}
		/* less strict checking */
		if( at->tid >= DAO_ARRAY && at->tid != DAO_ANY ) goto NotMatch;

		if( bt->tid == DAO_TUPLE && bt->subtid == DAO_PAIR
				&& (at->tid == DAO_STRING || at->tid <= DAO_FLOAT) ){
			/* passed */
			AssertPairNumberType( bt );
		}else if( bt->tid == DAO_LIST && at->tid <= DAO_FLOAT ){
			/* passed */
		}else if( bt->tid > DAO_FLOAT && bt->tid != DAO_ANY ){
			/* less strict checking */
			goto NotMatch;
		}
		break;
	case DAO_LIST :
		type = ct->nested->items.pType[0];
		if( bt->tid >= DAO_BOOLEAN && bt->tid <= DAO_FLOAT ){
			ct = ct->nested->items.pType[0];
			AssertTypeMatching( at, ct, defs );
			if( code == DVM_SETI ){
				if( ct->tid && ct->tid <= DAO_COMPLEX && at->tid && at->tid <= DAO_COMPLEX ){
					if( at->tid != ct->tid )
						DaoInferencer_InsertMove( self, inode, & inode->a, at, ct );
					vmc->code = DVM_SETI_LBIB + ct->tid - DAO_BOOLEAN;
				}else if( at->tid == DAO_STRING && ct->tid == DAO_STRING ){
					vmc->code = DVM_SETI_LSIS;
				}else{
					if( at == ct || ct->tid == DAO_ANY ) vmc->code = DVM_SETI_LI;
				}
				if( bt->tid != DAO_INTEGER )
					DaoInferencer_InsertMove( self, inode, & inode->b, bt, dao_type_int );
			}
		}else if( bt->tid == DAO_TUPLE && bt->subtid == DAO_PAIR ){
			AssertPairNumberType( bt );
			AssertTypeMatching( at, type, defs );
		}else if( bt->tid == DAO_LIST || bt->tid == DAO_ARRAY ){
			tt = bt->nested->items.pType[0];
			if( tt->tid != DAO_INTEGER && tt->tid != DAO_ANY && tt->tid != DAO_UDT )
				return DaoInferencer_ErrorTypeNotMatching( self, tt, dao_type_int );
			AssertTypeMatching( at, type, defs );
		}else{
			return DaoInferencer_ErrorTypeNotMatching( self, bt, dao_type_int );
		}
		break;
	case DAO_MAP :
		{
			DaoType *t0 = ct->nested->items.pType[0];
			DaoType *t1 = ct->nested->items.pType[1];
			AssertTypeMatching( at, t1, defs );
			if( bt->tid == DAO_TUPLE && bt->subtid == DAO_PAIR ){  /* Check slicing: */
				DaoType *start, *end, **kts = bt->nested->items.pType;
				int openStart = 0, openEnd = 0;
				start = kts[0]->tid == DAO_PAR_NAMED ? (DaoType*) kts[0]->aux : kts[0];
				end   = kts[1]->tid == DAO_PAR_NAMED ? (DaoType*) kts[1]->aux : kts[1];
				openStart = start->tid == DAO_NONE && start->valtype;
				openEnd   = end->tid == DAO_NONE   && end->valtype;
				if( !openStart && DaoType_MatchTo( start, t0, defs ) == 0 ) goto InvIndex;
				if( !openEnd && DaoType_MatchTo( end, t0, defs ) == 0 ) goto InvIndex;
			}else{
				AssertTypeMatching( bt, t0, defs );
			}
			break;
		}
	case DAO_ARRAY :
		if( bt->tid >= DAO_INTEGER && bt->tid <= DAO_FLOAT ){
			if( DaoType_MatchTo( at, ct, defs ) ) break;
			ct = ct->nested->items.pType[0];
			/* array[i] */
			if( code == DVM_SETI ){
				if( ct->realnum && at->realnum ){
					if( at->tid != ct->tid )
						DaoInferencer_InsertMove( self, inode, & inode->a, at, ct );
					vmc->code = DVM_SETI_ABIB + ct->tid - DAO_BOOLEAN;
				}else if( ct->tid == DAO_COMPLEX && at->tid && at->tid <= DAO_COMPLEX ){
					if( at->tid != DAO_COMPLEX )
						DaoInferencer_InsertMove( self, inode, & inode->a, at, ct );
					vmc->code = DVM_SETI_ACIC;
				}else if( at->tid != DAO_UDT && at->tid != DAO_ANY ){
					AssertTypeMatching( at, ct, defs );
				}
				if( bt->tid != DAO_INTEGER )
					DaoInferencer_InsertMove( self, inode, & inode->b, bt, dao_type_int );
			}
			AssertTypeMatching( at, ct, defs );
		}else if( bt->tid == DAO_LIST || bt->tid == DAO_ARRAY ){
			k = bt->nested->items.pType[0]->tid;
			if( k >= DAO_FLOAT && k != DAO_ANY ) goto NotMatch;
			/* imprecise checking */
			if( DaoType_MatchTo( at, ct->nested->items.pType[0], defs )==0
					&& DaoType_MatchTo( at, ct, defs )==0 )
				goto NotMatch;
		}
		break;
	case DAO_TUPLE :
		if( value && value->type ){
			if( value->type > DAO_FLOAT ) goto InvIndex;
			k = DaoValue_GetInteger( value );
			if( k < 0 ) goto InvIndex;
			if( ct->variadic == 0 && k >= (int)ct->nested->size ) goto InvIndex;
			if( ct->variadic && k >= (ct->nested->size - 1) ){
				ct = ct->nested->items.pType[ ct->nested->size - 1 ];
				ct = ct->aux ? (DaoType*) ct->aux : dao_type_any;
			}else{
				ct = ct->nested->items.pType[ k ];
			}
			if( ct->tid == DAO_PAR_NAMED ) ct = & ct->aux->xType;
			AssertTypeMatching( at, ct, defs );
			if( k <= 0xffff ){
				if( ct->tid && ct->tid <= DAO_COMPLEX && at->tid && at->tid <= DAO_COMPLEX ){
					vmc->b = k;
					if( at->tid != ct->tid )
						DaoInferencer_InsertMove( self, inode, & inode->a, at, ct );
					vmc->code = DVM_SETF_TBB + ct->tid - DAO_BOOLEAN;
				}else if( at == ct || ct->tid == DAO_ANY ){
					vmc->b = k;
					if( at->tid ==DAO_STRING && ct->tid ==DAO_STRING ){
						vmc->code = DVM_SETF_TSS;
					}else if( at->tid >= DAO_ARRAY && at->tid <= DAO_TYPE && consts[opa] == NULL ){
						vmc->code = DVM_SETF_TPP;
					}else{
						vmc->code = DVM_SETF_TXX;
					}
				}
			}
		}else if( bt->realnum ){
			vmc->code = DVM_SETI_TI;
			if( bt->tid != DAO_INTEGER )
				DaoInferencer_InsertMove( self, inode, & inode->b, bt, dao_type_int );
		}else if( bt->tid != DAO_UDT && bt->tid != DAO_ANY ){
			goto InvIndex;
		}
		break;
	case DAO_CLASS :
	case DAO_OBJECT :
		if( (meth=DaoClass_FindMethod( & ct->aux->xClass, "[]=", hostClass )) == NULL)
			goto InvIndex;
		ts[0] = at;
		ts[1] = bt;
		k = 2;
		if( bt->tid == DAO_TUPLE ){
			if( bt->nested->size + 2 > DAO_MAX_PARAM ) goto InvIndex;
			ts[0] = at;
			for(k=0,K=bt->nested->size; k<K; k++) ts[k+1] = bt->nested->items.pType[k];
			k = bt->nested->size + 1;
		}
		rout = DaoRoutine_Check( meth, ct, ts, k, DVM_CALL, errors );
		if( rout == NULL ) goto InvIndex;
		break;
	case DAO_CTYPE :
	case DAO_CDATA :
	case DAO_CSTRUCT :
	case DAO_INTERFACE :
	case DAO_CINTYPE :
	case DAO_CINVALUE :
		DString_SetChars( mbs, "[]=" );
		meth = DaoType_FindFunction( ct, mbs );
		if( meth == NULL ) goto InvIndex;
		ts[0] = at;
		ts[1] = bt;
		k = 2;
		if( bt->tid == DAO_TUPLE ){
			if( bt->nested->size + 2 > DAO_MAX_PARAM ) goto InvIndex;
			ts[0] = at;
			for(k=0,K=bt->nested->size; k<K; k++) ts[k+1] = bt->nested->items.pType[k];
			k = bt->nested->size + 1;
		}
		rout = DaoRoutine_Check( meth, ct, ts, k, DVM_CALL, errors );
		if( rout == NULL ) goto InvIndex;
		break;
	default : goto InvIndex;
	}
	return 1;
NotMatch : return DaoInferencer_ErrorTypeNotMatching( self, NULL, NULL );
InvIndex : return DaoInferencer_Error( self, DTE_INDEX_NOT_VALID );
ErrorTyping: return DaoInferencer_Error( self, DTE_TYPE_NOT_MATCHING );
}
int DaoInferencer_HandleSetMItem( DaoInferencer *self, DaoInode *inode, DMap *defs )
{
	int opa = inode->a;
	int opb = inode->b;
	int opc = inode->c;
	DList *errors = self->errors;
	DString *mbs = self->mbstring;
	DaoType *ts[DAO_ARRAY+DAO_MAX_PARAM];
	DaoType *type, **types = self->types->items.pType;
	DaoClass *hostClass = self->hostClass;
	DaoRoutine *meth, *rout;
	DaoType *at = types[opa];
	DaoType *ct = types[opc];
	DaoInode *inode2;
	DNode *node;
	int j, min, max;

	if( ct == NULL ) goto ErrorTyping;
	if( NoCheckingType( at ) || NoCheckingType( ct ) ) return 1;
	meth = NULL;
	DString_SetChars( mbs, "[]=" );
	switch( ct->tid ){
	case DAO_ARRAY :
		max = DAO_NONE;
		min = DAO_COMPLEX;
		type = ct->nested->items.pType[0];
		for(j=1; j<=opb; j++){
			int tid = types[j+opc]->tid;
			if( tid > max ) max = tid;
			if( tid < min ) min = tid;
		}
		if( at->tid == DAO_NONE || (at->tid & DAO_ANY) || (type->tid & DAO_ANY) ) return 1;
		if( type->tid <= DAO_COMPLEX && at->tid <= DAO_COMPLEX ){
			if( at->tid == DAO_COMPLEX &&  type->tid != DAO_COMPLEX ) goto ErrorTyping;
			if( min >= DAO_INTEGER && max <= DAO_FLOAT ){
				inode->code = DVM_SETMI_ABIB + (type->tid - DAO_BOOLEAN);
				if( at->tid != type->tid )
					DaoInferencer_InsertMove( self, inode, & inode->a, at, type );
				if( max > DAO_INTEGER ){
					inode2 = DaoInferencer_InsertNode( self, inode, DVM_MOVE_PP, 1, ct );
					inode2->c = inode->c;
					inode->c = self->types->size - 1;
					for(j=1; j<=opb; j++){
						unsigned short op = j+opc;
						DaoInferencer_InsertMove( self, inode, & op, types[j+opc], dao_type_int );
					}
				}
			}
		}
		break;
	case DAO_MAP :
		goto InvIndex;
	case DAO_CLASS :
	case DAO_OBJECT :
		meth = DaoClass_FindMethod( & ct->aux->xClass, "[]=", hostClass );
		if( meth == NULL ) goto WrongContainer;
		break;
	case DAO_CDATA :
	case DAO_CSTRUCT :
	case DAO_CTYPE :
		meth = DaoType_FindFunction( ct, mbs );
		if( meth == NULL ) goto WrongContainer;
		break;
	case DAO_INTERFACE :
	case DAO_CINTYPE :
	case DAO_CINVALUE :
		meth = DaoType_FindFunction( ct, mbs );
		if( meth == NULL ) goto WrongContainer;
		break;
	default :
		if( ct->typer ){
			meth = DaoType_FindFunction( ct, mbs );
			if( meth == NULL ) goto WrongContainer;
			break;
		}
		goto WrongContainer;
	}
	if( meth ){
		ts[0] = at;
		memcpy( ts + 1, types+opc+1, opb*sizeof(DaoType*) );
		rout = DaoRoutine_Check( meth, ct, ts, opb+1, DVM_CALL, errors );
		if( rout == NULL ) goto InvIndex;
	}
	return 1;
WrongContainer : return DaoInferencer_Error( self, DTE_TYPE_WRONG_CONTAINER );
InvIndex : return DaoInferencer_Error( self, DTE_INDEX_NOT_VALID );
ErrorTyping: return DaoInferencer_Error( self, DTE_TYPE_NOT_MATCHING );
}
int DaoInferencer_HandleSetField( DaoInferencer *self, DaoInode *inode, DMap *defs )
{
	int opa = inode->a;
	int opb = inode->b;
	int opc = inode->c;
	DList *errors = self->errors;
	DString *str, *mbs = self->mbstring;
	DList  *routConsts = self->routine->routConsts->value;
	DaoRoutine *routine = self->routine;
	DaoType *type, **types = self->types->items.pType;
	DaoValue *data, *value, **consts = self->consts->items.pValue;
	DaoClass *klass, *hostClass = self->hostClass;
	DaoVmCodeX *vmc = (DaoVmCodeX*) inode;
	DaoRoutine *meth, *rout;
	DaoType *at = types[opa];
	DaoType *ct = types[opc];
	DNode *node;
	DaoType *pars[2] = { NULL, NULL };
	int j = 0, setter = 0;
	int npar = 1;
	int ck = 0;
	int k;

#if 0
	printf( "a: %s\n", types[opa]->name->chars );
	printf( "c: %s\n", types[opc]->name->chars );
#endif
	pars[0] = pars[1] = types[opa];
	value = routConsts->items.pValue[opb];
	if( value == NULL || value->type != DAO_STRING ) goto NotMatch;
	self->type_source = ct;
	str = value->xString.value;
	switch( ct->tid ){
	case DAO_UDT :
	case DAO_ANY :
	case DAO_THT :
		/* allow less strict typing: */
		break;
	case DAO_COMPLEX :
		if( strcmp( str->chars, "real" ) && strcmp( str->chars, "imag" ) ) goto NotExist;
		if( at->realnum == 0 && !(at->tid & DAO_ANY) ) goto NotMatch;
		if( at->tid & DAO_ANY ) break;
		if( at->tid != DAO_FLOAT )
			DaoInferencer_InsertMove( self, inode, & inode->a, at, dao_type_float );
		inode->code = DVM_SETF_CX;
		inode->b = strcmp( str->chars, "imag" ) == 0;
		break;
	case DAO_CLASS :
	case DAO_OBJECT :
		ck = ct->tid ==DAO_CLASS;
		klass = (DaoClass*) ct->aux;
		data = DaoClass_GetData( klass, str, hostClass );
		if( strcmp( str->chars, "self" ) ==0 ) goto NotPermit;
		if( data == NULL || data->type == DAO_NONE ){
			DString_SetChars( mbs, "." );
			DString_Append( mbs, str );
			DString_AppendChars( mbs, "=" );
			data = DaoClass_GetData( klass, mbs, hostClass );
			if( data == NULL ){
				pars[0] = dao_type_string;
				npar = 2;
				DString_SetChars( mbs, ".=" );
				data = DaoClass_GetData( klass, mbs, hostClass );
			}
			if( data && data->type == DAO_CONSTANT && data->xConst.value->type == DAO_ROUTINE ){
				meth = (DaoRoutine*) data->xConst.value;
				setter = 1;
				rout = DaoRoutine_Check( meth, ct, pars, npar, DVM_CALL, errors );
				if( rout == NULL ) goto NotMatch;
			}else{
				goto InvOper;
			}
		}
		if( data == NULL ) goto NotExist;
		if( data->type == DAO_NONE ) goto NotPermit;
		if( data->xBase.subtype == DAO_OBJECT_VARIABLE && at->tid == DAO_CLASS ) goto NeedInstVar;

		if( setter ) break;
		if( data->xBase.subtype == DAO_CLASS_CONSTANT ) goto InvOper;

		if( data->xVar.dtype && data->xVar.dtype->invar ){
			if( !(routine->attribs & DAO_ROUT_INITOR) ) goto ModifyConstant;
		}
		if( data->xVar.dtype == NULL && data->xVar.dtype->tid == DAO_UDT ){
			GC_Assign( & data->xVar.dtype, types[opa] );
		}
		AssertTypeMatching( types[opa], data->xVar.dtype, defs );
		j = DaoClass_GetDataIndex( klass, str );

		k = LOOKUP_ST( j );
		type = data->xVar.dtype;
		if( data->xVar.dtype && data->xVar.dtype->realnum && at->realnum ){
			vmc->code = ck ? DVM_SETF_KGBB : DVM_SETF_OGBB;
			if( k == DAO_OBJECT_VARIABLE ) vmc->code = DVM_SETF_OVBB;
			if( at->tid != type->tid )
				DaoInferencer_InsertMove( self, inode, & inode->a, at, type );
			vmc->code += type->tid - DAO_BOOLEAN;
			vmc->b = LOOKUP_ID( j );
		}else if( type && type->tid == DAO_COMPLEX && at->tid && at->tid <= DAO_COMPLEX ){
			vmc->b = LOOKUP_ID( j );
			vmc->code = ck ? DVM_SETF_KGCC : DVM_SETF_OGCC;
			if( k == DAO_OBJECT_VARIABLE ) vmc->code = DVM_SETF_OVCC;
			if( at->tid != type->tid )
				DaoInferencer_InsertMove( self, inode, & inode->a, at, type );
		}else if( at == type || type->tid == DAO_ANY ){
			vmc->b = LOOKUP_ID( j );
			vmc->code = ck ? DVM_SETF_KG : DVM_SETF_OG;
			if( k == DAO_OBJECT_VARIABLE ) vmc->code = DVM_SETF_OV;
		}
		break;
	case DAO_TUPLE :
		{
			if( ct->mapNames == NULL ) goto NotExist;
			node = MAP_Find( ct->mapNames, str );
			if( node == NULL ) goto NotExist;
			k = node->value.pInt;
			if( k <0 || k >= (int)ct->nested->size ) goto InvIndex;
			ct = ct->nested->items.pType[ k ];
			if( ct->tid == DAO_PAR_NAMED ) ct = & ct->aux->xType;
			if( ct && ct->invar ) goto ModifyConstant;
			AssertTypeMatching( at, ct, defs );
			if( k < 0xffff ){
				if( ct->tid && ct->tid <= DAO_COMPLEX && at->tid && at->tid <= DAO_COMPLEX ){
					if( at->tid != ct->tid )
						DaoInferencer_InsertMove( self, inode, & inode->a, at, ct );
					vmc->code = DVM_SETF_TBB + ct->tid - DAO_BOOLEAN;
					vmc->b = k;
				}else if( at->tid == DAO_STRING && ct->tid == DAO_STRING ){
					vmc->code = DVM_SETF_TSS;
					vmc->b = k;
				}else if( at == ct || ct->tid == DAO_ANY ){
					vmc->b = k;
					if( at->tid >= DAO_ARRAY && at->tid <= DAO_TYPE && consts[opa] == NULL ){
						vmc->code = DVM_SETF_TPP;
					}else{
						vmc->code = DVM_SETF_TXX;
					}
				}
			}
			break;
		}
	case DAO_NAMESPACE :
		{
			if( consts[opc] && consts[opc]->type == DAO_NAMESPACE ){
				DaoNamespace *ans = & consts[opc]->xNamespace;
				k = DaoNamespace_FindVariable( ans, str );
				if( k >=0 ){
					ct = DaoNamespace_GetVariableType( ans, k );
				}else{
					k = DaoNamespace_FindConst( ans, str );
					value = DaoNamespace_GetConst( ans, k );
					if( value ) ct = DaoNamespace_GetType( ans, value );
				}
				if( k <0 ) goto NotExist;
				if( ct && ct->invar ) goto ModifyConstant;
				AssertTypeMatching( at, ct, defs );
			}
			break;
		}
	case DAO_CDATA :
	case DAO_CSTRUCT :
	case DAO_INTERFACE :
	case DAO_CINTYPE :
	case DAO_CINVALUE :
		{
			DString_SetChars( mbs, "." );
			DString_Append( mbs, str );
			DString_AppendChars( mbs, "=" );
			meth = DaoType_FindFunction( ct, mbs );
			if( meth == NULL ){
				pars[0] = dao_type_string;
				npar = 2;
				DString_SetChars( mbs, ".=" );
				meth = DaoType_FindFunction( ct, mbs );
			}
			if( meth == NULL ) goto NotMatch;
			rout = DaoRoutine_Check( meth, ct, pars, npar, DVM_CALL, errors );
			if( rout == NULL ) goto NotMatch;
			break;
		}
	default: goto InvOper;
	}
	return 1;
NotMatch : return DaoInferencer_ErrorTypeNotMatching( self, NULL, NULL );
NotPermit : return DaoInferencer_Error( self, DTE_FIELD_NOT_PERMIT );
NotExist : return DaoInferencer_Error( self, DTE_FIELD_NOT_EXIST );
NeedInstVar : return DaoInferencer_Error( self, DTE_FIELD_OF_INSTANCE );
InvOper : return DaoInferencer_Error( self, DTE_OPERATION_NOT_VALID );
InvIndex : return DaoInferencer_Error( self, DTE_INDEX_NOT_VALID );
ModifyConstant: return DaoInferencer_Error( self, DTE_CONST_WRONG_MODIFYING );
}
DaoType* DaoInferencer_CheckBinaryOper( DaoInferencer *self, DaoInode *inode, DaoType *at, DaoType *bt )
{
	DString *mbs = self->mbstring;
	DaoType **types = self->types->items.pType;
	DaoNamespace *NS = self->routine->nameSpace;
	DaoClass *hostClass = self->hostClass;
	DaoVmCodeX *vmc = (DaoVmCodeX*) inode;
	DaoType *ct = NULL;
	int code = inode->code;
	int opc = inode->c;

	if( NoCheckingType( at ) || NoCheckingType( bt ) ){
		ct = dao_type_udf;
#if 0
	}else if( at->tid == DAO_VARIANT || bt->tid == DAO_VARIANT ){
		DList *types = DList_New(0);
		int i;
		if( at->tid == DAO_VARIANT ){
			for(i=0; i<at->nested->size; ++i){
				DaoType *at2 = at->nested->items.pType[i];
				DaoType *ct2 = DaoInferencer_CheckBinaryOper( self, inode, at2, bt );
				if( ct2 == NULL ){
					DList_Delete( types );
					return NULL;
				}
				DList_Append( types, ct2 );
			}
		}else{
			for(i=0; i<bt->nested->size; ++i){
				DaoType *bt2 = bt->nested->items.pType[i];
				DaoType *ct2 = DaoInferencer_CheckBinaryOper( self, inode, at, bt2 );
				if( ct2 == NULL ){
					DList_Delete( types );
					return NULL;
				}
				DList_Append( types, ct2 );
			}
		}
		ct = DaoNamespace_MakeType( NS, "", DAO_VARIANT, NULL, types->items.pType, types->size );
		DList_Delete( types );
#endif
	}else if( at->tid == DAO_OBJECT || bt->tid == DAO_OBJECT
			|| at->tid == DAO_CDATA || bt->tid == DAO_CDATA
			|| at->tid == DAO_CSTRUCT || bt->tid == DAO_CSTRUCT
			|| at->tid == DAO_CINVALUE || bt->tid == DAO_CINVALUE 
			|| at->tid == DAO_INTERFACE || bt->tid == DAO_INTERFACE ){
		ct = DaoCheckBinArith( self->routine, vmc, at, bt, types[opc], hostClass, mbs );
		if( ct == NULL ) return NULL;
	}else if( code == DVM_AND || code == DVM_OR ){
		if( at->realnum && bt->realnum ){
			ct = dao_type_bool;
		}else{
			return NULL;
		}
	}else if( at->tid == bt->tid ){
		ct = at;
		switch( at->tid ){
		case DAO_INTEGER : case DAO_FLOAT :
			break;
		case DAO_COMPLEX :
			if( code == DVM_MOD ) return NULL;
			break;
		case DAO_STRING :
			if( code != DVM_ADD && code != DVM_DIV ) return NULL;
			break;
		case DAO_ENUM :
			if( code != DVM_ADD && code != DVM_SUB ) return NULL;
			if( at->subtid == DAO_ENUM_SYM && bt->subtid == DAO_ENUM_SYM ){
				DString_Assign( self->mbstring, at->name );
				DString_Change( self->mbstring, "enum%< (.*) %>", "%1", 0 );
				DString_Append( self->mbstring, bt->name );
				ct = DaoNamespace_MakeEnumType( NS, self->mbstring->chars );
			}else if( at->subtid != DAO_ENUM_FLAG ){
				return NULL;
			}
			break;
		case DAO_ARRAY :
			break;
		default : return NULL;
		}
	}else if( at->realnum && bt->realnum ){
		ct = at->tid > bt->tid ? at : bt;
	}else if( at->realnum && (bt->tid ==DAO_COMPLEX || bt->tid ==DAO_ARRAY) ){
		ct = bt;
	}else if( (at->tid ==DAO_COMPLEX || at->tid ==DAO_ARRAY) && bt->realnum ){
		ct = at;
	}else if( ( at->tid ==DAO_COMPLEX && bt->tid ==DAO_ARRAY )
			|| ( at->tid ==DAO_ARRAY && bt->tid ==DAO_COMPLEX ) ){
		ct = dao_array_types[DAO_COMPLEX];
	}else{
		return NULL;
	}
	return ct;
}
int DaoInferencer_HandleBinaryArith( DaoInferencer *self, DaoInode *inode, DMap *defs )
{
	int code = inode->code;
	int opa = inode->a;
	int opb = inode->b;
	int opc = inode->c;
	DString *mbs = self->mbstring;
	DaoType **types = self->types->items.pType;
	DaoNamespace *NS = self->routine->nameSpace;
	DaoClass *hostClass = self->hostClass;
	DaoVmCodeX *vmc = (DaoVmCodeX*) inode;
	DaoType *at = types[opa];
	DaoType *bt = types[opb];
	DaoType *ct = types[opc];

#if 0
	if( types[opa] ) printf( "a: %s\n", types[opa]->name->chars );
	if( types[opb] ) printf( "b: %s\n", types[opb]->name->chars );
	if( types[opc] ) printf( "c: %s\n", types[opc]->name->chars );
#endif

	ct = DaoInferencer_CheckBinaryOper( self, inode, at, bt );
	if( ct == NULL ) goto InvOper;

	DaoInferencer_UpdateVarType( self, opc, ct );
	/* allow less strict typing: */
	if( ct->tid == DAO_UDT || ct->tid == DAO_ANY ) return 1;
	AssertTypeMatching( ct, types[opc], defs );
	ct = types[opc];
	if( at->realnum && bt->realnum && ct->realnum ){
		DaoType *max = ct;
		if( at->tid > max->tid ) max = at;
		if( bt->tid > max->tid ) max = bt;
		if( max->tid == DAO_BOOLEAN ) max = dao_type_int; /* such oper on bools produce int; */
		if( at->tid != max->tid ){
			DaoInferencer_InsertMove( self, inode, & inode->a, at, max );
			if( opa == opb ) inode->b = inode->a;
		}
		if( opa != opb && bt->tid != max->tid )
			DaoInferencer_InsertMove( self, inode, & inode->b, bt, max );

		switch( max->tid ){
		case DAO_INTEGER : vmc->code += DVM_ADD_III - DVM_ADD; break;
		case DAO_FLOAT  : vmc->code += DVM_ADD_FFF - DVM_ADD; break;
		}
		if( max->tid != ct->tid ) DaoInferencer_InsertMove2( self, inode, max, ct );
	}else if( ct->tid == DAO_COMPLEX && code <= DVM_DIV ){
		if( at->tid && at->tid <= DAO_COMPLEX && bt->tid && bt->tid <= DAO_COMPLEX ){
			if( at->tid != DAO_COMPLEX ){
				DaoInferencer_InsertMove( self, inode, & inode->a, at, dao_type_complex );
				if( opa == opb ) inode->b = inode->a;
			}
			if( opa != opb && bt->tid != DAO_COMPLEX )
				DaoInferencer_InsertMove( self, inode, & inode->b, bt, dao_type_complex );
			vmc->code += DVM_ADD_CCC - DVM_ADD;
		}
	}else if( at->tid == bt->tid && ct->tid == at->tid ){
		if( ct->tid == DAO_STRING && code == DVM_ADD ) vmc->code = DVM_ADD_SSS;
	}
	return 1;
InvOper :
	return DaoInferencer_ErrorTypeNotConsistent( self, at, bt );
}
int DaoInferencer_HandleBinaryBool( DaoInferencer *self, DaoInode *inode, DMap *defs )
{
	int code = inode->code;
	int opa = inode->a;
	int opb = inode->b;
	int opc = inode->c;
	DString *mbs = self->mbstring;
	DaoType **types = self->types->items.pType;
	DaoNamespace *NS = self->routine->nameSpace;
	DaoClass *hostClass = self->hostClass;
	DaoVmCodeX *vmc = (DaoVmCodeX*) inode;
	DaoType *at = types[opa];
	DaoType *bt = types[opb];
	DaoType *ct = types[opc];

#if 0
	if( types[opa] ) printf( "a: %s\n", types[opa]->name->chars );
	if( types[opb] ) printf( "b: %s\n", types[opb]->name->chars );
	if( types[opc] ) printf( "c: %s\n", types[opc]->name->chars );
#endif

	ct = DaoInferencer_CheckBinaryOper( self, inode, at, bt );
	if( ct == NULL ) goto InvOper;

	DaoInferencer_UpdateVarType( self, opc, ct );
	/* allow less strict typing: */
	if( ct->tid == DAO_UDT || ct->tid == DAO_ANY ) return 1;
	AssertTypeMatching( ct, types[opc], defs );
	ct = types[opc];
	if( at->realnum && bt->realnum && ct->realnum ){
		DaoType *max = at->tid == bt->tid ? at : dao_type_bool;
		if( at->tid != max->tid ){
			DaoInferencer_InsertMove( self, inode, & inode->a, at, max );
			if( opa == opb ) inode->b = inode->a;
		}
		if( opa != opb && bt->tid != max->tid )
			DaoInferencer_InsertMove( self, inode, & inode->b, bt, max );

		switch( max->tid ){
		case DAO_BOOLEAN : vmc->code += DVM_AND_BBB - DVM_AND; break;
		case DAO_INTEGER : vmc->code += DVM_AND_BII - DVM_AND; break;
		case DAO_FLOAT  : vmc->code += DVM_AND_BFF - DVM_AND; break;
		}
		if( ct->tid != DAO_BOOLEAN ) DaoInferencer_InsertMove2( self, inode, dao_type_bool, ct );
	}
	return 1;
InvOper : return DaoInferencer_Error( self, DTE_OPERATION_NOT_VALID );
}
int DaoInferencer_HandleListArrayEnum( DaoInferencer *self, DaoInode *inode, DMap *defs )
{
	int code = inode->code;
	int opa = inode->a;
	int opb = inode->b & (0xffff>>2);
	int opc = inode->c;
	int mode = inode->b >> 14;
	DaoType **types = self->types->items.pType;
	DaoNamespace *NS = self->routine->nameSpace;
	DaoVmCodeX *vmc = (DaoVmCodeX*) inode;
	DaoType *at = types[opa];
	DaoType *ct = types[opc];
	int j;

	int tid = code == DVM_LIST ? DAO_LIST : DAO_ARRAY;
	if( types[opc] && types[opc]->tid == tid ){
		if( types[opc]->nested && types[opc]->nested->size == 1 ){
			DaoType *it = types[opc]->nested->items.pType[0];
			if( code == DVM_VECTOR && mode == DVM_ENUM_MODE1 ){
				int m1 = DaoType_MatchTo( types[opa], types[opc], defs );
				int m2 = DaoType_MatchTo( types[opa], it, defs );
				if( m1 == 0 && m2 == 0 ){
					return DaoInferencer_ErrorTypeNotMatching( self, types[opa], it );
				}
				if( opb == 3 ){
					int m1 = DaoType_MatchTo( types[opa+1], types[opc], defs );
					int m2 = DaoType_MatchTo( types[opa+1], it, defs );
					if( m1 == 0 && m2 == 0 ){
						return DaoInferencer_ErrorTypeNotMatching( self, types[opa+1], it );
					}
				}
				AssertTypeMatching( types[opa+opb-1], dao_type_int, defs );
			}else if( code == DVM_LIST && mode == DVM_ENUM_MODE1 ){
				AssertTypeMatching( types[opa], it, defs );
				if( opb == 3 ) AssertTypeMatching( types[opa+1], it, defs );
				AssertTypeMatching( types[opa+opb-1], dao_type_int, defs );
			}else if( code == DVM_VECTOR ){
				int m1 = DaoType_MatchTo( types[opa], types[opc], defs );
				int m2 = DaoType_MatchTo( types[opa], it, defs );
				if( m1 == 0 && m2 == 0 ){
					return DaoInferencer_ErrorTypeNotMatching( self, types[opa], it );
				}
				if( m1 ) it = types[opc];
				for(j=0; j<opb; ++j) AssertTypeMatching( types[opa+j], it, defs );
			}else{
				for(j=0; j<opb; ++j) AssertTypeMatching( types[opa+j], it, defs );
			}
			return 1;
		}
	}
	at = dao_type_udf;
	if( code == DVM_VECTOR && mode == DVM_ENUM_MODE0 && opb ){
		at = types[opa];
		for(j=1; j<opb; j++) AssertTypeMatching( types[opa+j], at, defs );
		if( at->tid == DAO_ARRAY ) at = at->nested->items.pType[0];
		if( at->tid == DAO_NONE || at->tid > DAO_COMPLEX ) at = dao_type_float;
	}else if( code == DVM_LIST && mode == DVM_ENUM_MODE0 && opb ){
		at = types[opa];
		for(j=1; j<opb; j++){
			if( DaoType_MatchTo( types[opa+j], at, defs )==0 ){
				at = dao_type_any;
				break;
			}
			if( at->tid < types[opa+j]->tid ) at = types[opa+j];
		}
	}else if( mode == DVM_ENUM_MODE1 ){
		int num = types[opa+1+(opb==3)]->tid;
		int init = types[opa]->tid;
		at = types[opa];
		if( num == 0 || (num > DAO_FLOAT && (num & DAO_ANY) == 0) ) goto ErrorTyping;
		if( opb == 3 && (init & DAO_ANY) == 0 && (types[opa+1]->tid & DAO_ANY) == 0 ){
			int step = types[opa+1]->tid;
			if( step == 0 ) goto ErrorTyping;
			if( types[opa]->realnum ){
				if( types[opa+1]->realnum == 0 ) goto ErrorTyping;
			}else if( init == DAO_COMPLEX ){
				if( step > DAO_COMPLEX ) goto ErrorTyping;
			}else if( init == DAO_STRING && code == DVM_LIST ){
				if( step != DAO_STRING ) goto ErrorTyping;
			}else{
				goto ErrorTyping;
			}
		}
	}else if( opb == 0 && types[opc] != NULL ){
		if( types[opc]->tid == DAO_LIST ){
			if( code == DVM_LIST ) return 1;
		}else if( types[opc]->tid == DAO_ARRAY ){
			if( code == DVM_VECTOR ) return 1;
		}
	}
	if( opb == 0 ){
		if( code == DVM_LIST ){
			ct = dao_type_list_empty;
		}else{
			ct = dao_type_array_empty;
		}
	}else if( code == DVM_LIST ){
		at = DaoType_GetBaseType( at );
		ct = DaoType_Specialize( dao_type_list, & at, at != NULL );
	}else if( at && at->tid >= DAO_BOOLEAN && at->tid <= DAO_COMPLEX ){
		at = DaoType_GetBaseType( at );
		ct = DaoType_Specialize( dao_type_array, & at, 1 );
	}else if( NoCheckingType( at ) ){
		ct = dao_type_array_empty; /* specially handled for copying; */
	}else{
		goto ErrorTyping;
	}
	DaoInferencer_UpdateType( self, opc, ct );
	AssertTypeMatching( ct, types[opc], defs );
	return 1;
ErrorTyping: return DaoInferencer_Error( self, DTE_TYPE_NOT_MATCHING );
}
int DaoInferencer_HandleSwitch( DaoInferencer *self, DaoInode *inode, int i, DMap *defs )
{
	int opa = inode->a;
	int opc = inode->c;
	DList  *routConsts = self->routine->routConsts->value;
	DaoInode *inode2, **inodes = self->inodes->items.pInode;
	DaoType *bt, *type, **types = self->types->items.pType;
	DaoValue **consts = self->consts->items.pValue;
	DaoNamespace *NS = self->routine->nameSpace;
	DaoVmCodeX *vmc = (DaoVmCodeX*) inode;
	DaoType *at = types[opa];
	int j, k;

	if( inodes[i+1]->c == DAO_CASE_TYPES ){
		for(k=1; k<=opc; k++){
			DaoType *tt = (DaoType*) routConsts->items.pValue[ inodes[i+k]->a ];
			if( tt->type != DAO_TYPE ) return DaoInferencer_Error( self, DTE_INVALID_TYPE_CASE );
		}
		return 1;
	}

	j = 0;
	for(k=1; k<=opc; k++){
		DaoValue *cc = routConsts->items.pValue[ inodes[i+k]->a ];
		j += (cc && cc->type == DAO_ENUM && cc->xEnum.subtype == DAO_ENUM_SYM);
		bt = DaoNamespace_GetType( NS, cc );
		if( at->tid == DAO_ENUM && bt->tid == DAO_ENUM ){
			if( at->subtid == DAO_ENUM_SYM && bt->subtid == DAO_ENUM_SYM ) continue;
		}
		if( DaoType_MatchValue( at, cc, defs ) ==0 ){
			self->currentIndex = i + k;
			type = DaoNamespace_GetType( NS, cc );
			return DaoInferencer_ErrorTypeNotMatching( self, type, at );
		}
	}
	if( consts[opa] && consts[opa]->type ){
		DaoValue *sv = consts[opa];
		for(k=1; k<=opc; k++){
			DaoValue *cc = routConsts->items.pValue[ inodes[i+k]->a ];
			if( DaoValue_Compare( sv, cc ) ==0 ){
				inode->code = DVM_GOTO;
				inode->jumpFalse = inodes[i+k];
				break;
			}
		}
	}else if( at->tid == DAO_ENUM && at->subtid != DAO_ENUM_SYM && j == opc ){
		DaoInode *front = inodes[i];
		DaoInode *back = inodes[i+opc+1];
		DaoEnum denum = {DAO_ENUM,DAO_ENUM_SYM,0,0,0,0,0,NULL};
		DMap *jumps = DMap_New( DAO_DATA_VALUE, 0 );
		DNode *it, *find;
		int max=0, min=0;
		denum.etype = at;
		denum.subtype = at->subtid;
		for(k=1; k<=opc; k++){
			DaoValue *cc = routConsts->items.pValue[ inodes[i+k]->a ];
			if( DaoEnum_SetValue( & denum, & cc->xEnum ) == 0 ){
				self->currentIndex = i + k;
				DMap_Delete( jumps );
				return DaoInferencer_ErrorTypeNotMatching( self, cc->xEnum.etype, at );
			}
			if( k ==1 ){
				max = min = denum.value;
			}else{
				if( denum.value > max ) max = denum.value;
				if( denum.value < min ) min = denum.value;
			}
			MAP_Insert( jumps, (DaoValue*) & denum, inodes[i+k] );
		}
		if( at->subtid != DAO_ENUM_FLAG && opc > 0.75*(max-min+1) ){
			for(it=DMap_First(at->mapNames); it; it=DMap_Next(at->mapNames,it)){
				if( it->value.pInt < min || it->value.pInt > max ) continue;
				denum.value = it->value.pInt;
				find = DMap_Find( jumps, (DaoValue*) & denum );
				if( find == NULL ){
					inode2 = DaoInferencer_InsertNode( self, inodes[i+1], DVM_CASE, 0, 0 );
					inode2->jumpFalse = inode->jumpFalse;
					inode2->a = routConsts->size;
					inode2->c = DAO_CASE_TABLE;
					inodes[i+1]->extra = NULL;
					DMap_Insert( jumps, (DaoValue*) & denum, inode2 );
				}else{
					find->value.pInode->a = routConsts->size;
					find->value.pInode->c = DAO_CASE_TABLE;
				}
				DaoRoutine_AddConstant( self->routine, (DaoValue*) & denum );
			}
			vmc->c = jumps->size;
		}
		front->next = back;
		back->prev = front;
		for(it=DMap_First(jumps); it; it=DMap_Next(jumps,it)){
			inode2 = it->value.pInode;
			front->next = inode2;
			back->prev = inode2;
			inode2->prev = front;
			inode2->next = back;
			front = inode2;
		}
		DMap_Delete( jumps );
	}else if( j ){
		inodes[i + 1]->c = DAO_CASE_UNORDERED;
	}
	return 1;
}
static DaoRoutine* DaoInferencer_Specialize( DaoInferencer *self, DaoRoutine *rout, DMap *defs2, DaoInode *inode )
{
	DaoNamespace *NS = self->routine->nameSpace;
	DaoType *routype = DaoType_DefineTypes( rout->routType, NS, defs2 );
	DaoRoutine *orig = rout, *rout2 = rout;
	DMap *defs3 = self->defs3;

	DMap_Reset( defs3 );
	if( DaoType_MatchTo( routype, rout->routType, defs3 ) >= DAO_MT_EQ ) return rout;
	if( rout->original ) rout = orig = rout->original;

	/* Do not share function body. It may be thread unsafe to share: */
	rout = DaoRoutine_Copy( rout, 0, 1, 0 );
	DaoRoutine_Finalize( rout, orig, NULL, defs2 );

	if( rout->routType == orig->routType || rout->routType == rout2->routType ){
		DaoGC_TryDelete( (DaoValue*) rout );
		rout = rout2;
	}else{
		DMutex_Lock( & mutex_routine_specialize );
		if( orig->specialized == NULL ) orig->specialized = DRoutines_New();
		DMutex_Unlock( & mutex_routine_specialize );

		GC_Assign( & rout->original, orig );
		/*
		// Need to add before specializing the body,
		// to avoid possible infinite recursion:
		 */
		DRoutines_Add( orig->specialized, rout );
		inode->b &= ~DAO_CALL_FAST;

		/* rout may has only been declared */
		/* forward declared routine may have an empty routine body: */
		if( rout->body && rout->body->vmCodes->size ){
			DaoRoutine *rout3 = rout;
			/* Create a new copy of the routine for specialization: */
			rout = DaoRoutine_Copy( rout, 0, 1, 0 );
			GC_Assign( & rout->original, orig );
			DMap_Reset( defs3 );
			DaoType_MatchTo( rout->routType, orig->routType, defs3 );
			DaoRoutine_MapTypes( rout, rout3, defs3 );

			/* to infer returned type */
			if( DaoRoutine_DoTypeInference( rout, self->silent ) ==0 ){
				DaoGC_TryDelete( (DaoValue*) rout );
				return NULL;
			}
			/* Replace the previous unspecialized copy with this specialized copy: */
			DRoutines_Add( orig->specialized, rout );
		}
	}
	return rout;
}
int DaoInferencer_HandleCall( DaoInferencer *self, DaoInode *inode, int i, DMap *defs )
{
	int code = inode->code;
	int opa = inode->a;
	int opc = inode->c;
	DMap *defs2 = self->defs2;
	DMap *defs3 = self->defs3;
	DList *errors = self->errors;
	DList *rettypes = self->rettypes;
	DaoInode **inodes = self->inodes->items.pInode;
	DaoType *bt, *tt, **tp, **types = self->types->items.pType;
	DaoValue **pp, **consts = self->consts->items.pValue;
	DaoNamespace *NS = self->routine->nameSpace;
	DaoClass *hostClass = self->hostClass;
	DaoVmCodeX *vmc = (DaoVmCodeX*) inode;
	DaoRoutine *routine = self->routine;
	DaoRoutine *rout, *rout2;
	DaoType *at = types[opa];
	DaoType *ct = types[opc];
	int N = self->inodes->size;
	int j, k, m, K;
	int checkfast = 0;
	int ctchecked = 0;
	int argc = vmc->b & 0xff;
	int codemode = code | ((int)vmc->b<<16);
	DaoType *cbtype = NULL;
	DaoInode *sect = NULL;

	if( (vmc->b & DAO_CALL_BLOCK) && inodes[i+2]->code == DVM_SECT ){
		sect = inodes[ i + 2 ];
		for(j=0, k=sect->a; j<sect->b; j++, k++){
			DaoInferencer_UpdateType( self, k, dao_type_udf );
		}
	}
	bt = ct = NULL;
	if( code == DVM_CALL && self->tidHost == DAO_OBJECT ) bt = hostClass->objType;

#if 0
	DaoVmCodeX_Print( *vmc, NULL, NULL );
	printf( "call: %s %i\n", types[opa]->name->chars, types[opa]->tid );
	if(bt) printf( "self: %s\n", bt->name->chars );
#endif

	pp = consts+opa+1;
	tp = types+opa+1;
	for(k=0; k<argc; k++){
		tt = DaoType_DefineTypes( tp[k], NS, defs );
		GC_Assign( & tp[k], tt );
	}
	if( ! (routine->attribs & DAO_ROUT_MAIN) ){
		m = 1; /* tail call; */
		for(k=i+1; k<N; k++){
			DaoInode *ret = inodes[k];
			if( ret->code == DVM_RETURN ){
				m &= ret->c ==0 && (ret->b ==0 || (ret->b ==1 && ret->a == vmc->c));
				break;
			}
			m = 0;
			break;
		}
		if( m ) vmc->b |= DAO_CALL_TAIL;
	}
	if( (vmc->b & DAO_CALL_EXPAR) && argc && tp[argc-1]->tid == DAO_TUPLE ){
		DList *its = tp[argc-1]->nested;
		DList_Clear( self->types2 );
		for(k=0; k<(argc-1); k++) DList_Append( self->types2, tp[k] );
		for(k=0; k<its->size; k++){
			DaoType *it = its->items.pType[k];
			if( it->tid == DAO_PAR_NAMED || it->tid == DAO_PAR_DEFAULT ) it = (DaoType*) it->aux;
			DList_Append( self->types2, it );
		}
		tp = self->types2->items.pType;
		argc = self->types2->size;
	}

	ct = types[opa];
	rout = NULL;
	if( at->tid == DAO_CLASS ){
		if( at->aux->xClass.initRoutines->overloads->routines->size ){
			rout = (DaoRoutine*) at->aux->xClass.initRoutines; /* XXX */
		}else{
			rout = at->aux->xClass.initRoutine;
		}
		ct = at->aux->xClass.objType;
	}else if( at->tid == DAO_CTYPE ){
		rout = DaoType_GetInitor( at );
		if( rout == NULL ) goto ErrorTyping;
	}else if( consts[opa] && consts[opa]->type == DAO_ROUTINE ){
		rout = (DaoRoutine*) consts[opa];
	}else if( at->tid == DAO_THT ){
		DaoInferencer_UpdateType( self, opc, dao_type_any );
		AssertTypeMatching( dao_type_any, types[opc], defs );
		goto TryPushBlockReturnType;
	}else if( at->tid == DAO_UDT || at->tid == DAO_ANY ){
		DaoInferencer_UpdateType( self, opc, dao_type_any );
		goto TryPushBlockReturnType;
	}else if( at->tid == DAO_OBJECT ){
		rout = DaoClass_FindMethod( & at->aux->xClass, "()", hostClass );
		if( rout == NULL ) goto ErrorTyping;
		bt = at;
	}else if( at->tid == DAO_CDATA || at->tid == DAO_CSTRUCT ){
		rout = DaoType_FindFunctionChars( at, "()" );
		if( rout == NULL ) goto ErrorTyping;
		bt = at;
	}else if( at->tid == DAO_INTERFACE ){
		DaoInterface *inter = (DaoInterface*) at->aux;
		DNode *it = DMap_Find( inter->methods, inter->abtype->name );
		if( it == NULL ) goto ErrorTyping;
		rout = it->value.pRoutine;
	}else if( at->tid == DAO_TYPE ){
		at = at->nested->items.pType[0];
		rout = DaoType_FindFunction( at, at->name );
		if( rout == NULL ) goto ErrorTyping;
	}else if( at->tid != DAO_ROUTINE ){
		goto ErrorTyping;
	}
	if( at->tid == DAO_ROUTINE && at->subtid == DAO_ROUTINES ) rout = (DaoRoutine*)at->aux;
	if( rout == NULL && at->aux == NULL ){ /* "routine" type: */
		/* DAO_CALL_INIT: mandatory passing the implicit self parameter. */
		if( !(vmc->b & DAO_CALL_INIT) ) vmc->b |= DAO_CALL_NOSELF;
		cbtype = at->cbtype;
		ct = dao_type_any;
		ctchecked = 1;
	}else if( rout == NULL ){
		cbtype = at->cbtype;
		if( !(vmc->b & DAO_CALL_INIT) ) vmc->b |= DAO_CALL_NOSELF;
		if( DaoRoutine_CheckType( at, NS, NULL, tp, argc, codemode, 0 ) ==0 ){
			DaoRoutine_CheckError( NS, NULL, at, NULL, tp, argc, codemode, errors );
			goto ErrorTyping;
		}
		if( at->name->chars[0] == '@' ){
			ct = tp[0];
			if( pp[0] && pp[0]->type == DAO_ROUTINE ) ct = pp[0]->xRoutine.routType;
			DaoInferencer_UpdateType( self, opc, ct );
			AssertTypeMatching( ct, types[opc], defs );
			goto TryPushBlockReturnType;
		}
		DaoRoutine_CheckType( at, NS, NULL, tp, argc, codemode, 1 );
		ct = types[opa];
	}else{
		if( rout->type != DAO_ROUTINE ) goto ErrorTyping;
		rout2 = rout;
		/* rout can be DRoutines: */
		rout = DaoRoutine_Check( rout, bt, tp, argc, codemode, errors );
		if( rout == NULL ) goto ErrorTyping;
		if( rout->attribs & DAO_ROUT_PRIVATE ){
			if( rout->routHost && rout->routHost != routine->routHost ) goto CallNotPermit;
			if( rout->routHost == NULL && rout->nameSpace != NS ) goto CallNotPermit;
		}else if( rout->attribs & DAO_ROUT_PROTECTED ){
			if( rout->routHost && routine->routHost == NULL ) goto CallNotPermit;
		}
		if( vmc->code == DVM_CALL && routine->routHost ){
			if( DaoType_ChildOf( routine->routHost, rout->routHost ) ){
				int invarCaller = routine->attribs & DAO_ROUT_INVAR;
				int staticCaller = routine->attribs & DAO_ROUT_STATIC;
				int staticCallee = rout->attribs & DAO_ROUT_STATIC;
				int invarCallee = rout->attribs & DAO_ROUT_INVAR;
				int initorCallee = rout->attribs & DAO_ROUT_INITOR;
				if( staticCaller && ! staticCallee && ! initorCallee ) goto CallWithoutInst;
				if( invarCaller && ! invarCallee && ! initorCallee ) goto CallNonInvar;
			}
		}
		checkfast = DVM_CALL && ((vmc->b & 0xff00) & ~DAO_CALL_TAIL) == 0;
		checkfast &= at->tid == DAO_ROUTINE && argc >= rout2->parCount;
		checkfast &= rout2->routHost == NULL;
		checkfast &= rout2 == rout;
		checkfast &= rout2->body != NULL || rout2->pFunc != NULL; /* not curry; */
		checkfast &= (vmc->code == DVM_CALL) == !(rout2->routType->attrib & DAO_TYPE_SELF);
		vmc->b &= ~DAO_CALL_FAST;
		if( checkfast ){
			int fast = 1;
			for(k=0; fast && k<rout2->routType->nested->size; ++k){
				DaoType *part = rout2->routType->nested->items.pType[k];
				DaoType *argt = tp[k];
				if( part->tid >= DAO_PAR_NAMED && part->tid <= DAO_PAR_VALIST ){
					part = (DaoType*) part->aux;
					fast &= part->tid != DAO_PAR_NAMED;
				}
				if( part->tid == DAO_ANY ) continue;
				fast &= DaoType_MatchTo( argt, part, NULL ) >= DAO_MT_EQ;
			}
			if( fast ) vmc->b |= DAO_CALL_FAST;
		}
		if( rout->attribs & DAO_ROUT_DECORATOR ){
			ct = tp[0];
			if( pp[0] && pp[0]->type == DAO_ROUTINE ){
				ct = pp[0]->xRoutine.routType;
				if( pp[0]->xRoutine.overloads ){
					DaoType *ft = & rout->routType->nested->items.pType[0]->aux->xType;
					DaoType **pts = ft->nested->items.pType;
					int nn = ft->nested->size;
					int cc = DVM_CALL + (ft->attrib & DAO_TYPE_SELF);
					rout = DaoRoutine_Check( (DaoRoutine*)pp[0], NULL, pts, nn, cc|((int)vmc->b<<16), errors );
					if( rout == NULL ) goto ErrorTyping;
				}
			}
			DaoInferencer_UpdateType( self, opc, ct );
			AssertTypeMatching( ct, types[opc], defs );
			goto TryPushBlockReturnType;
		}

		if( rout2->overloads && rout2->overloads->routines->size > 1 ){
			DList *routines = rout2->overloads->routines;
			m = DaoRoutine_CheckType( rout->routType, NS, bt, tp, argc, codemode, 1 );
			if( m <= DAO_MT_ANY ){
				/* For situations like:
				//
				// routine test( x :int ){ io.writeln(1); return 1; }
				// routine test( x ){ io.writeln(2); return 'abc'; }
				// a :dao_type_any = 1;
				// b = test( a );
				//
				// The function call may be resolved at compiling time as test(x),
				// which returns a string. But at runnning time, the function call
				// will be resolved as test(x:int), which return an integer.
				// Such discrepancy need to be solved here:
				 */
				DList_Clear( self->array );
				for(k=0,K=routines->size; k<K; k++){
					DaoType *type = routines->items.pRoutine[k]->routType;
					m = DaoRoutine_CheckType( type, NS, bt, tp, argc, codemode, 1 );
					if( m == 0 ) continue;
					type = (DaoType*) type->aux;
					if( type == NULL ) type = dao_type_none;
					if( type && type->tid == DAO_ANY ){
						ctchecked = 1;
						ct = dao_type_any;
						break;
					}
					for(m=0,K=self->array->size; m<K; m++){
						if( self->array->items.pType[m] == type ) break;
					}
					if( m >= self->array->size ) DList_Append( self->array, type );
				}
				if( self->array->size > 1 ){
					ctchecked = 1;
					ct = dao_type_any; /* XXX return variant type? */
				}
			}
		}

		tt = rout->routType;
		cbtype = tt->cbtype;

		DMap_Reset( defs2 );
		if( at->tid == DAO_CTYPE && at->kernel->sptree ){
			/* For type holder specialization: */
			k = DaoType_MatchTo( at, at->kernel->abtype->aux->xCdata.ctype, defs2 );
		}

		k = defs2->size;
		DaoRoutine_PassParamTypes( rout, bt, tp, argc, code, defs2 );
		/* rout->body->vmCodes->size is zero for declared but unimplemented routines: */
		if( rout != routine && defs2->size && (defs2->size > k || rout->routType->aux->xType.tid == DAO_UDT) && (rout->body == NULL || rout->body->vmCodes->size) ){
			rout = DaoInferencer_Specialize( self, rout, defs2, inode );
			if( rout == NULL ) goto InvParam;
		}
		if( at->tid != DAO_CLASS && ! ctchecked ) ct = rout->routType;
		/*
		   printf( "ct2 = %s\n", ct ? ct->name->chars : "" );
		 */
	}
	k = routine->routType->attrib & ct->attrib;
	if( at->tid != DAO_CLASS && ! ctchecked ) ct = & ct->aux->xType;
	if( ct ) ct = DaoType_DefineTypes( ct, NS, defs2 );

	if( code == DVM_MCALL && tp[0]->tid == DAO_OBJECT
			&& (tp[0]->aux->xClass.attribs & DAO_CLS_ASYNCHRONOUS) ){
		ct = DaoType_Specialize( dao_type_future, & ct, ct != NULL );
	}else if( vmc->b & DAO_CALL_ASYNC ){
		ct = DaoType_Specialize( dao_type_future, & ct, ct != NULL );
	}
	if( types[opc] && types[opc]->tid == DAO_ANY ) goto TryPushBlockReturnType;
	if( ct == NULL ) ct = DaoNamespace_GetType( NS, dao_none_value );
	DaoInferencer_UpdateType( self, opc, ct );
	AssertTypeMatching( ct, types[opc], defs );

TryPushBlockReturnType:
	if( sect && cbtype && cbtype->nested ){
		for(j=0, k=sect->a; j<sect->b; j++, k++){
			if( j < (int)cbtype->nested->size ){
				tt = cbtype->nested->items.pType[j];
				if( tt->tid == DAO_PAR_VALIST ){
					tt = (DaoType*) tt->aux;
					for(; j<sect->b; j++, k++){
						GC_DecRC( types[k] );
						types[k] = NULL;
						tt = DaoType_DefineTypes( tt, NS, defs2 );
						DaoInferencer_UpdateType( self, k, tt );
					}
					break;
				}
			}else{
				if( j < sect->c ) goto CallInvalidSectParam;
				break;
			}
			if( tt->attrib & DAO_TYPE_PARNAMED ) tt = (DaoType*)tt->aux;
			GC_DecRC( types[k] );
			types[k] = NULL;
			tt = DaoType_DefineTypes( tt, NS, defs2 );
			DaoInferencer_UpdateType( self, k, tt );
		}
		tt = DaoType_DefineTypes( (DaoType*)cbtype->aux, NS, defs2 );
		DList_Append( rettypes, inodes[i+1]->jumpFalse );
		DList_Append( rettypes, inode ); /* type at "opc" to be redefined; */
		DList_Append( rettypes, tt );
		DList_Append( rettypes, tt );
		DList_PushBack( self->typeMaps, defs2 );
	}else if( sect && cbtype == NULL ){
		if( NoCheckingType( types[opc] ) == 0 ) goto CallInvalidSection;
		DList_Append( rettypes, inodes[i+1]->jumpFalse );
		DList_Append( rettypes, inode );
		DList_Append( rettypes, NULL );
		DList_Append( rettypes, NULL );
		DList_PushBack( self->typeMaps, defs2 );
	}
	return 1;
InvParam : return DaoInferencer_Error( self, DTE_PARAM_ERROR );
CallNonInvar : return DaoInferencer_Error( self, DTE_CALL_NON_INVAR );
CallNotPermit : return DaoInferencer_Error( self, DTE_CALL_NOT_PERMIT );
CallWithoutInst : return DaoInferencer_Error( self, DTE_CALL_WITHOUT_INSTANCE );
CallInvalidSectParam : return DaoInferencer_Error( self, DTE_CALL_INVALID_SECTPARAM );
CallInvalidSection: return DaoInferencer_Error( self, DTE_CALL_INVALID_SECTION );
ErrorTyping: return DaoInferencer_Error( self, DTE_TYPE_NOT_MATCHING );
}
int DaoInferencer_HandleClosure( DaoInferencer *self, DaoInode *inode, int i, DMap *defs )
{
	int j;
	int code = inode->code;
	int opa = inode->a;
	int opc = inode->c;
	DMap *defs2 = self->defs2;
	DList *rettypes = self->rettypes;
	DaoType *tt, **tp, **types = self->types->items.pType;
	DaoValue **pp, **consts = self->consts->items.pValue;
	DaoInode *inode2, **inodes = self->inodes->items.pInode;
	DaoNamespace *NS = self->routine->nameSpace;
	DaoVmCodeX *vmc = (DaoVmCodeX*) inode;
	DaoRoutine *routine = self->routine;
	DaoRoutine *closure = (DaoRoutine*) consts[opa];
	DaoType *at = types[opa];
	DaoType *ct;

	if( types[opa]->tid != DAO_ROUTINE ) goto ErrorTyping;
	if( closure->attribs & DAO_ROUT_DEFER_RET ) DList_Append( self->defers, closure );

	DaoRoutine_DoTypeInference( closure, self->silent );

	self->array->size = 0;
	DList_Resize( self->array, closure->parCount, 0 );
	for(j=0; j<closure->parCount; j+=1){
		DaoType *partype = closure->routType->nested->items.pType[j];
		self->array->items.pType[j] = partype;
	}
	for(j=0; j<vmc->b; j+=2){
		DaoInode *idata = inodes[i - vmc->b + j + 1];
		if( idata->b < DAO_MAX_PARAM ){
			self->array->items.pType[idata->b] = types[opa+1+j];
		}else{
			DaoType *uptype = types[opa+1+j];
			DaoVariable *var = closure->body->upValues->items.pVar[ idata->b - DAO_MAX_PARAM ];
			if( uptype->invar ) uptype = DaoType_GetBaseType( uptype );
			GC_Assign( & var->dtype, uptype );
		}
	}
	at = DaoNamespace_MakeRoutType( NS, at, NULL, self->array->items.pType, NULL );

	DaoInferencer_UpdateType( self, opc, at );
	AssertTypeMatching( at, types[opc], defs );
	return 1;
ErrorTyping: return DaoInferencer_Error( self, DTE_TYPE_NOT_MATCHING );
}
int DaoInferencer_HandleYieldReturn( DaoInferencer *self, DaoInode *inode, DMap *defs )
{
	int code = inode->code;
	int opa = inode->a;
	int opc = inode->c;
	DMap *defs2 = self->defs2;
	DList *rettypes = self->rettypes;
	DaoType *tt, **tp, **types = self->types->items.pType;
	DaoNamespace *NS = self->routine->nameSpace;
	DaoVmCodeX *vmc = (DaoVmCodeX*) inode;
	DaoRoutine *routine = self->routine;
	DaoType *at, *ct;
	DaoInode *redef;
	DaoType *ct2;

	ct = rettypes->items.pType[ rettypes->size - 1 ];
	ct2 = rettypes->items.pType[ rettypes->size - 2 ];
	redef = rettypes->items.pInode[ rettypes->size - 3 ];
	DMap_Reset( defs2 );
	DMap_Assign( defs2, defs );

	/*
	// DO NOT CHANGE
	// FROM: return (e1, e2, e3, ... )
	// TO:   return e1, e2, e3, ...
	//
	// Because they bear different semantic meaning.
	// For example, if "e1" is in the form of "name=>expression",
	// the name is not stored in the tuple value but in the tuple type for
	// the first. For the second, it should be part of the returned value.
	//
	// The following code should NOT be used!
	 */
#if 0
	if( i && inodes[i-1]->code == DVM_TUPLE && inodes[i-1]->c == vmc->a && vmc->b == 1 ){
		vmc->a = inodes[i-1]->a;
		vmc->b = inodes[i-1]->b;
		inodes[i-1]->code = DVM_UNUSED;
		opa = vmc->a;
		opb = vmc->b;
		opc = vmc->c;
	}
#endif


#if 0
	printf( "%p %i %s %s\n", self, routine->routType->nested->size, routine->routType->name->chars, ct?ct->name->chars:"" );
#endif
	if( code == DVM_YIELD ){ /* yield in functional method: */
		DaoType **partypes, **argtypes;
		int parcount = 0, argcount = 0;
		int k, opb = vmc->b & 0xff;
		tt = NULL;
		if( routine->routType->cbtype ){
			tt = routine->routType->cbtype;
		}else if( routine->attribs & DAO_ROUT_DECORATOR ){
			if( routine->routType->nested->size == 0 ) goto InvalidYield;
			tt = routine->routType->nested->items.pType[0];
			if( tt->tid == DAO_PAR_NAMED ) tt = (DaoType*) tt->aux;
			if( tt == NULL || tt->tid != DAO_ROUTINE || tt->cbtype == NULL ) goto InvalidYield;
			tt = tt->cbtype;
		}else{
			goto InvalidYield;
		}
		partypes = tt->nested->items.pType;
		parcount = tt->nested->size;
		if( vmc->b == 0 ){
			if( tt->nested->size && partypes[0]->tid != DAO_PAR_VALIST ) goto ErrorTyping;
			ct = (DaoType*) tt->aux;
			if( ct == NULL ) ct = dao_type_none;
			DaoInferencer_UpdateType( self, opc, ct );
			return 1;
		}
		argtypes = types + opa;
		argcount = opb;
		if( (vmc->b & DAO_CALL_EXPAR) && opb && argtypes[opb-1]->tid == DAO_TUPLE ){
			DList *its = argtypes[opb-1]->nested;
			DList_Clear( self->types2 );
			for(k=0; k<(opb-1); k++) DList_Append( self->types2, argtypes[k] );
			for(k=0; k<its->size; k++) DList_Append( self->types2, its->items.pType[k] );
			argtypes = self->types2->items.pType;
			argcount = self->types2->size;
		}
		at = ct = DaoNamespace_MakeType( NS, "tuple<>", DAO_TUPLE, NULL, NULL, 0 );
		if( argcount ){
			at = DaoNamespace_MakeType2( NS, "tuple", DAO_TUPLE, NULL, argtypes, argcount );
		}
		if( parcount ){
			ct = DaoNamespace_MakeType2( NS, "tuple", DAO_TUPLE, NULL, partypes, parcount );
		}
#if 0
		printf( "%s %s\n", at->name->chars, ct->name->chars );
#endif
		AssertTypeMatching( at, ct, defs2 );
		ct = (DaoType*) tt->aux;
		if( ct == NULL ) ct = dao_type_none;
		if( ct ){
			DaoInferencer_UpdateType( self, opc, ct );
			AssertTypeMatching( ct, types[opc], defs2 );
		}
		return 1;
	}
	if( vmc->b ==0 ){
		/* less strict checking for type holder as well (case mt.start()): */
		if( ct && (ct->tid == DAO_UDT || ct->tid == DAO_THT) ){
			ct = DaoNamespace_MakeValueType( NS, dao_none_value );
			rettypes->items.pType[ rettypes->size - 1 ] = ct;
			ct = DaoNamespace_MakeRoutType( NS, routine->routType, NULL, NULL, ct );
			GC_Assign( & routine->routType, ct );
			return 1;
		}
		if( ct && DaoType_MatchValue( ct, dao_none_value, NULL ) ) return 1;
		if( ct && ! (routine->attribs & DAO_ROUT_INITOR) ) goto ErrorTyping;
	}else{
		if( code == DVM_RETURN && (routine->attribs & DAO_ROUT_INITOR) ){
			/* goto InvalidReturn; */  /* TODO: not for decorated initor; */
		}else if( code == DVM_RETURN && (routine->attribs & DAO_ROUT_DEFER) ){
			if( !(routine->attribs & DAO_ROUT_DEFER_RET) ) goto InvalidReturn;
		}
		at = types[opa];
		if( at == NULL ) goto ErrorTyping;
		if( vmc->b >1 )
			at = DaoNamespace_MakeType2( NS, "tuple", DAO_TUPLE, NULL, types+opa, vmc->b);

		if( ct && DaoType_MatchTo( at, ct, defs2 ) == 0 ) goto ErrorTyping;
		/* XXX */
		if( ct == NULL || ( ct->attrib & (DAO_TYPE_SPEC|DAO_TYPE_UNDEF)) ){
			if( rettypes->size == 4 ){
				DaoType *ct3 = DaoType_DefineTypes( ct, NS, defs2 );
				if( ct3 != NULL && ct3 != ct ){
					tt = DaoNamespace_MakeRoutType( NS, routine->routType, NULL, NULL, ct3 );
					GC_Assign( & routine->routType, tt );

					ct = (DaoType*)routine->routType->aux;
					rettypes->items.pType[ rettypes->size - 1 ] = ct;
				}
			}else{
				ct = DaoType_DefineTypes( ct, NS, defs2 );
				if( ct != NULL && redef != NULL ){
					tt = DaoType_DefineTypes( types[redef->c], NS, defs2 );
					GC_DecRC( types[redef->c] );
					types[redef->c] = NULL;
					DaoInferencer_UpdateType( self, redef->c, tt );
				}
				rettypes->items.pType[ rettypes->size - 1 ] = ct;
			}
		}
		if( ct != ct2 ){
			int m1 = DaoType_MatchTo( at, ct2, defs2 );
			int m2 = DaoType_MatchTo( ct, ct2, defs2 );
			if( m1 == 0 || m2 == 0 ) goto ErrorTyping;
		}
	}
	return 1;
ErrorTyping: return DaoInferencer_Error( self, DTE_TYPE_NOT_MATCHING );
InvalidYield: return DaoInferencer_Error( self, DTE_ROUT_INVALID_YIELD );
InvalidReturn: return DaoInferencer_Error( self, DTE_ROUT_INVALID_RETURN );
}

static DaoType* DaoInferencer_HandleVarInvarDecl( DaoInferencer *self, DaoType *at, int opb )
{
	if( at->konst ) at = DaoType_GetBaseType( at ); /* Constant types do not propagate; */

	if( ! (opb & 0x2) ) return at; /* Not an explicit declaration; */

	/* Invar declaration: */
	if( opb & 0x4 ) return DaoType_GetInvarType( at );

	/* Var declaration: */

	/* Move from constant: get base type without const; */
	if( at->konst == 1 ) return DaoType_GetBaseType( at );
	
	/* Move from invariable: */
	if( at->invar == 1 ){
		/* Move from primitive type: get base type without const or invar; */	
		if( at->tid <= DAO_ENUM ){
			return DaoType_GetBaseType( at );
		}else if( DaoType_IsImmutable( at ) ){
			/*
			// Variables of immutable types are immutable regardless of the "var" keyword.
			// Such declaration is permited for conveniece, otherwise, every declaration
			// of variables or parameters of such immutable types (such as invar class or
			// ctype, e.g., DateTime) would require the use of the "invar" keyword, and
			// too verbose to write.
			//
			// Also the point of supporting invar class and invar ctype is exactly that
			// we will not need to write invar everywhere to guarantee its immutability!
			*/
			return at;
		}else{
			DaoInferencer_Error( self, DTE_INVAR_VAL_TO_VAR_VAR );
			return NULL;
		}
	}
	return at;
}

int DaoInferencer_DoInference( DaoInferencer *self )
{
	DNode *node;
	DMap *defs = self->defs;
	DList *errors = self->errors;
	DString *str, *mbs = self->mbstring;
	DaoRoutine *closure;
	DaoVariable *var;
	DaoVmCodeX *vmc;
	DaoType *at, *bt, *ct, *tt, *catype, *ts[DAO_ARRAY+DAO_MAX_PARAM];
	DaoType *type, **tp, **type2, **types = self->types->items.pType;
	DaoValue *value, **consts = self->consts->items.pValue;
	DaoInode *inode, **inodes = self->inodes->items.pInode;
	DaoRoutine *rout, *meth, *routine = self->routine;
	DaoClass *klass, *hostClass = self->hostClass;
	DaoNamespace *NS = routine->nameSpace;
	DaoRoutineBody *body = routine->body;
	DaoType **typeVH[DAO_MAX_SECTDEPTH+1] = { NULL };
	DList  *rettypes = self->rettypes;
	DList  *routConsts = routine->routConsts->value;
	daoint i, N = routine->body->annotCodes->size;
	daoint j, k, J, K, M = routine->body->regCount;
	int invarinit = !!(routine->attribs & DAO_ROUT_INITOR);
	int invarmeth = routine->attribs & DAO_ROUT_INVAR;
	int code, opa, opb, opc, first, middle, last;
	int TT1, TT2, TT3, TT6;

	if( self->inodes->size == 0 ) return 1;
	/*
	DaoRoutine_PrintCode( routine, routine->nameSpace->vmSpace->errorStream );
	*/

	catype = dao_array_types[DAO_COMPLEX];

	for(i=1; i<=DAO_MAX_SECTDEPTH; i++) typeVH[i] = types;
	for(i=0; i<N; i++) inodes[i]->index = N | (1<<16); /* for return ranges (rettypes); */

	if( invarinit ){
		invarinit &= routine->routHost->tid == DAO_OBJECT;
		invarinit &= !!(routine->routHost->aux->xClass.attribs & DAO_CLS_INVAR);
	}
	if( invarinit ){
		DaoStream *stream = routine->nameSpace->vmSpace->errorStream;
		DaoType *routype = routine->routType;
		DaoType *retype = (DaoType*) routype->aux;
		DaoType *partype = NULL;
		int error = 0;
		for(i=0; i<routype->nested->size; ++i){
			partype = routype->nested->items.pType[i];
			if( partype->tid >= DAO_PAR_NAMED && partype->tid <= DAO_PAR_VALIST ){
				partype = (DaoType*) partype->aux;
			}
			if( DaoType_IsPrimitiveOrImmutable( partype ) == 0 ){
				error = DTE_PARAM_WRONG_TYPE;
				break;
			}
		}
		if( error == 0 && DaoType_IsPrimitiveOrImmutable( retype ) == 0 ){
			error = DTE_ROUT_INVALID_RETURN2;
			partype = retype;
		}
		if( error ){
			char char50[50];
			sprintf( char50, "  At line %i : ", routine->defLine );
			DaoInferencer_WriteErrorHeader2( self );
			DaoStream_WriteChars( stream, char50 );
			DaoStream_WriteChars( stream, DaoTypingErrorString[error] );
			DaoStream_WriteChars( stream, " --- \" " );
			DaoStream_WriteChars( stream, partype->name->chars );
			DaoStream_WriteChars( stream, " \";\n" );
			DaoStream_WriteChars( stream, char50 );
			DaoStream_WriteChars( stream, "Expecting primitive or immutable types;\n" );
			return 0;
		}
	}

	DList_Append( rettypes, inodes[N-1] );
	DList_Append( rettypes, NULL );
	DList_Append( rettypes, routine->routType->aux );
	DList_Append( rettypes, routine->routType->aux );
	for(i=0; i<N; i++){
		inodes = self->inodes->items.pInode;
		consts = self->consts->items.pValue;
		types = self->types->items.pType;
		N = self->inodes->size;
		M = self->types->size;
		self->currentIndex = i;
		inode = inodes[i];
		inode->index = i;
		vmc = (DaoVmCodeX*) inode;
		code = vmc->code;
		opa = vmc->a;  opb = vmc->b;  opc = vmc->c;
		at = opa < M ? types[opa] : NULL;
		bt = opb < M ? types[opb] : NULL;
		ct = opc < M ? types[opc] : NULL;
		first = vmc->first;
		middle = first + vmc->middle;
		last = middle + vmc->last;

		if( rettypes->size > 4 ){
			DaoInode *range = rettypes->items.pInode[rettypes->size - 4];
			if( inode->prev == range ){
				DList_Erase( rettypes, rettypes->size - 4, -1 );
				DList_PopBack( self->typeMaps );
			}
		}
		DMap_Reset( defs );
		DMap_Assign( defs, (DMap*)DList_Back( self->typeMaps ) );

#if 0
		DaoLexer_AnnotateCode( routine->body->source, *(DaoVmCodeX*)inode, mbs, 24 );
		printf( "%4i: ", i );DaoVmCodeX_Print( *(DaoVmCodeX*)inode, mbs->chars, NULL );
#endif

		K = DaoVmCode_GetOpcodeType( (DaoVmCode*) inode );
		if( K && K < DAO_CODE_EXPLIST && K != DAO_CODE_SETG && K != DAO_CODE_SETU ){
			if( K != DAO_CODE_MOVE ){
				if( ct != NULL && (ct->tid == DAO_CLASS || ct->tid == DAO_CTYPE) ){
					/* TODO: SETF; see daoParser.c: line 6893; */
					if( K == DAO_CODE_SETI || K == DAO_CODE_SETM ) goto SkipChecking;
				}
				if( ct != NULL && ct->invar != 0 && K == DAO_CODE_SETF ){
					if( ct->tid != DAO_CLASS && ct->tid != DAO_NAMESPACE ) goto ModifyConstant;
				}else if( ct != NULL && ct->invar != 0 && K > DAO_CODE_GETG ){
					if( (code < DVM_PAIR || code > DVM_MPACK) && code != DVM_TUPLE_SIM ){
						goto ModifyConstant;
					}
				}
			}
		}
		if( ct != NULL && ct->invar != 0 ){
			if( code == DVM_MOVE || (code >= DVM_MOVE_BB && code <= DVM_MOVE_XX) ){
				if( (opb & 0x1) && !(opb & 0x4) ) goto ModifyConstant;
			}
		}
		if( invarmeth ){
			if( code == DVM_SETVO || (code >= DVM_SETVO_II && code <= DVM_SETVO_CC ) ){
				goto ModifyConstant;
			}
		}

SkipChecking:
		switch( K ){
		case DAO_CODE_MOVE :
			if( code == DVM_LOAD ){
				tt = DaoType_GetAutoCastType( at );
				if( tt != NULL ) DaoInferencer_InsertUntag( self, inode, & inode->a, tt );
			}
			break;
		case DAO_CODE_GETF :
		case DAO_CODE_GETI :
		case DAO_CODE_UNARY :
		case DAO_CODE_GETM :
		case DAO_CODE_ENUM2 :
		case DAO_CODE_CALL :
			tt = DaoType_GetAutoCastType( at );
			if( tt != NULL ) DaoInferencer_InsertUntag( self, inode, & inode->a, tt );
			break;
		case DAO_CODE_UNARY2 :
			tt = DaoType_GetAutoCastType( bt );
			if( tt != NULL ) DaoInferencer_InsertUntag( self, inode, & inode->b, tt );
			break;
		case DAO_CODE_SETF :
		case DAO_CODE_SETI :
		case DAO_CODE_SETM :
			tt = DaoType_GetAutoCastType( ct );
			if( tt != NULL ) DaoInferencer_InsertUntag( self, inode, & inode->c, tt );
			break;
		case DAO_CODE_BINARY :
			if( code == DVM_EQ || code == DVM_NE ) break;
			tt = DaoType_GetAutoCastType( at );
			if( tt != NULL ) DaoInferencer_InsertUntag( self, inode, & inode->a, tt );
			tt = DaoType_GetAutoCastType( bt );
			if( tt != NULL ) DaoInferencer_InsertUntag( self, inode, & inode->b, tt );
			break;
		}
		if( self->inodes->size != N ){
			i--;
			continue;
		}

		switch( code ){
		case DVM_DATA :
			if( opa > DAO_STRING ) return DaoInferencer_Error( self, DTE_DATA_CANNOT_CREATE );
			at = self->basicTypes[ opa ];
			if( types[opc]== NULL || types[opc]->tid == DAO_UDT ){
				DaoInferencer_UpdateType( self, opc, at );
			}else{
				AssertTypeMatching( at, types[opc], defs );
			}
			value = NULL;
			switch( opa ){
			case DAO_NONE : value = dao_none_value; break;
			case DAO_BOOLEAN : value = (DaoValue*) DaoBoolean_New( opb ); break;
			case DAO_INTEGER : value = (DaoValue*) DaoInteger_New( opb ); break;
			case DAO_FLOAT : value = (DaoValue*) DaoFloat_New( opb ); break;
			}
			GC_Assign( & consts[opc], value );
			if( at->tid >= DAO_BOOLEAN && at->tid <= DAO_COMPLEX ){
				vmc->code = DVM_DATA_B + (at->tid - DAO_BOOLEAN);
			}
			break;
		case DVM_GETCL :
		case DVM_GETCK :
		case DVM_GETCG :
			switch( code ){
			case DVM_GETCL : value = routConsts->items.pValue[opb]; break;
			case DVM_GETCK : value = hostClass->constants->items.pConst[opb]->value; break;
			case DVM_GETCG : value = NS->constants->items.pConst[opb]->value; break;
			}
			at = DaoNamespace_GetType( NS, value );
			if( at->konst == 0 && (code != DVM_GETCL || opa != 0) ){
				/*
				// Do not produce constant type for implicit constant.
				// Consider: a = "something"; a += "else";
				// Also code such as: DVM_GETCL: 0, 2, 3;
				// could have been added by routine decoration.
				// They shouldn't be const type.
				*/
				at = DaoType_GetConstType( at );
			}
			DaoInferencer_UpdateType( self, opc, at );
			/*
			   printf( "at %i %i %p, %p\n", at->tid, types[opc]->tid, at, types[opc] );
			 */
			AssertTypeMatching( at, types[opc], defs );
			GC_Assign( & consts[opc], value );
			if( at->tid >= DAO_BOOLEAN && at->tid <= DAO_COMPLEX ){
				int K = DAO_COMPLEX - DAO_BOOLEAN + 1;
				vmc->code = DVM_GETCL_B + K*(code - DVM_GETCL) + (at->tid - DAO_BOOLEAN);
			}
			break;
		case DVM_GETVH :
		case DVM_GETVS :
		case DVM_GETVO :
		case DVM_GETVK :
		case DVM_GETVG :
			at = 0;
			switch( code ){
			case DVM_GETVH : at = typeVH[opa][opb]; break;
			case DVM_GETVS : at = body->upValues->items.pVar[opb]->dtype; break;
			case DVM_GETVO : at = hostClass->instvars->items.pVar[opb]->dtype; break;
			case DVM_GETVK : at = hostClass->variables->items.pVar[opb]->dtype; break;
			case DVM_GETVG : at = NS->variables->items.pVar[opb]->dtype; break;
			}
			if( at == NULL ) at = dao_type_udf;
			if( invarmeth && code == DVM_GETVO && at->konst == 0 ){
				at = DaoType_GetInvarType( at );
			}
			DaoInferencer_UpdateType( self, opc, at );

#if 0
			printf( "%s\n", at->name->chars );
			printf( "%p %p\n", at, types[opc] );
			printf( "%s %s\n", at->name->chars, types[opc]->name->chars );
#endif
			AssertTypeMatching( at, types[opc], defs );
			if( at->tid >= DAO_BOOLEAN && at->tid <= DAO_COMPLEX ){
				int K = DAO_COMPLEX - DAO_BOOLEAN + 1;
				vmc->code = DVM_GETVH_B + K*(code - DVM_GETVH) + (at->tid - DAO_BOOLEAN);
			}
			break;
		case DVM_SETVH :
		case DVM_SETVS :
		case DVM_SETVO :
		case DVM_SETVK :
		case DVM_SETVG :
			if( code == DVM_SETVO && opb == 0 ) goto InvOper; /* Invalid move to "self"; */
			var = NULL;
			type2 = NULL;
			switch( code ){
			case DVM_SETVH : type2 = typeVH[opc] + opb; break;
			case DVM_SETVS : var = body->upValues->items.pVar[opb]; break;
			case DVM_SETVO : var = hostClass->instvars->items.pVar[opb]; break;
			case DVM_SETVK : var = hostClass->variables->items.pVar[opb]; break;
			case DVM_SETVG : var = NS->variables->items.pVar[opb]; break;
			}
			if( var ) type2 = & var->dtype;
			at = types[opa];
			if( code == DVM_SETVG ){
				if( !(opc & 0x4) && var->dtype && var->dtype->invar ) goto ModifyConstant;
				at = DaoInferencer_HandleVarInvarDecl( self, at, opc );
				if( at == NULL ) return 0;
			}else if( code == DVM_SETVO && var->subtype == DAO_INVAR ){
				if( !(routine->attribs & DAO_ROUT_INITOR) ) goto ModifyConstant;
				at = DaoType_GetInvarType( at );
			}
			if( at->tid <= DAO_ENUM ) at = DaoType_GetBaseType( at );
			if( type2 && *type2 != NULL && at->tid > DAO_ENUM ){
				int im = DaoType_IsImmutable( at );
				if( im == 0 && type2[0]->var == 1 && (at->invar == 1 && at->konst == 0) ){
					return DaoInferencer_ErrorTypeNotMatching( self, at, *type2 );
				}
			}
			if( type2 && (*type2 == NULL || (*type2)->tid == DAO_UDT || (*type2)->tid == DAO_THT) ){
				GC_Assign( type2, at );
			}
			/* less strict checking */
			if( types[opa]->tid & DAO_ANY ) break;
			if( type2 == NULL ) break;

			k = DaoType_MatchTo( types[opa], *type2, defs );
			if( k ==0 ) return DaoInferencer_ErrorTypeNotMatching( self, types[opa], *type2 );
			at = types[opa];
			if( type2[0]->tid && type2[0]->tid <= DAO_COMPLEX && at->tid && at->tid <= DAO_COMPLEX ){
				int K = DAO_COMPLEX - DAO_BOOLEAN + 1;
				/* Check and make a proper value object with default value: */
				if( var && (var->value == NULL || var->value->type != type2[0]->value->type) ){
					DaoValue_Copy( type2[0]->value, & var->value );
				}
				if( at->tid != type2[0]->tid ){
					DaoInferencer_InsertMove( self, inode, & inode->a, at, *type2 );
				}
				vmc->code = DVM_SETVH_BB + type2[0]->tid - DAO_BOOLEAN;
				vmc->code += K*(code - DVM_SETVH);
			}
			break;
		case DVM_GETI :
		case DVM_GETDI :
			if( DaoInferencer_HandleGetItem( self, inode, defs ) == 0 ) return 0;
			break;
		case DVM_GETMI :
			if( DaoInferencer_HandleGetMItem( self, inode, defs ) == 0 ) return 0;
			break;
		case DVM_GETF :
			if( DaoInferencer_HandleGetField( self, inode, defs ) == 0 ) return 0;
			break;
		case DVM_SETI :
		case DVM_SETDI :
			if( DaoInferencer_HandleSetItem( self, inode, defs ) == 0 ) return 0;
			break;
		case DVM_SETMI :
			if( DaoInferencer_HandleSetMItem( self, inode, defs ) == 0 ) return 0;
			break;
		case DVM_SETF :
			if( DaoInferencer_HandleSetField( self, inode, defs ) == 0 ) return 0;
			break;
		case DVM_CAST :
			if( routConsts->items.pValue[opb]->type != DAO_TYPE ) goto InvalidCasting;
			bt = (DaoType*) routConsts->items.pValue[opb];
			if( bt->tid == DAO_INTERFACE ){
				DaoCinType *cintype = DaoInterface_GetConcrete( (DaoInterface*) bt->aux, at );
				if( cintype ) bt = cintype->vatype;
			}
			DaoInferencer_UpdateType( self, opc, bt );
			AssertTypeMatching( bt, types[opc], defs );
			at = types[opa];
			ct = types[opc];
			if( at->realnum && ct->realnum ){
				int K = DAO_FLOAT - DAO_BOOLEAN + 1;
				vmc->code = DVM_MOVE_BB + K*(ct->tid - DAO_BOOLEAN) + at->tid - DAO_BOOLEAN;
			}else if( at->tid == DAO_COMPLEX && ct->tid == DAO_COMPLEX ){
				vmc->code = DVM_MOVE_CC;
			}else if( ct->tid >= DAO_BOOLEAN && ct->tid <= DAO_STRING ){
				switch( ct->tid ){
				case DAO_BOOLEAN : vmc->code = DVM_CAST_B; break;
				case DAO_INTEGER : vmc->code = DVM_CAST_I; break;
				case DAO_FLOAT   : vmc->code = DVM_CAST_F; break;
				case DAO_COMPLEX : vmc->code = DVM_CAST_C; break;
				case DAO_STRING  : vmc->code = DVM_CAST_S; break;
				}
			}else if( at->tid == DAO_VARIANT ){
				for(j=0,k=0; j<at->nested->size; ++j){
					int mt = DaoType_MatchTo( at->nested->items.pType[j], ct, defs );
					k += at->nested->items.pType[j]->tid == ct->tid;
					if( mt >= DAO_MT_EQ ){
						if( ct->tid == DAO_ENUM ){
							vmc->code = DVM_CAST_VE;
						}else if( ct->tid == DAO_NONE || ct->tid > DAO_ENUM ){
							vmc->code = DVM_CAST_VX;
						}
						break;
					}
				}
				if( vmc->code == DVM_CAST_VE || vmc->code == DVM_CAST_VX ){
					if( k > 1 ) vmc->code = DVM_CAST; /* not distinctive; */
				}
			}
			break;
		case DVM_CAST_B :
		case DVM_CAST_I :
		case DVM_CAST_F :
		case DVM_CAST_C :
		case DVM_CAST_S :
			if( routConsts->items.pValue[opb]->type != DAO_TYPE ) goto ErrorTyping;
			bt = (DaoType*) routConsts->items.pValue[opb];
			DaoInferencer_UpdateType( self, opc, bt );
			AssertTypeMatching( bt, types[opc], defs );
			switch( types[opc]->tid ){
			case DAO_BOOLEAN : if( vmc->code == DVM_CAST_B ) break; goto ErrorTyping;
			case DAO_INTEGER : if( vmc->code == DVM_CAST_I ) break; goto ErrorTyping;
			case DAO_FLOAT   : if( vmc->code == DVM_CAST_F ) break; goto ErrorTyping;
			case DAO_COMPLEX : if( vmc->code == DVM_CAST_C ) break; goto ErrorTyping;
			case DAO_STRING  : if( vmc->code == DVM_CAST_S ) break; goto ErrorTyping;
			default : goto ErrorTyping;
			}
			break;
		case DVM_CAST_VE :
		case DVM_CAST_VX :
			if( at->tid != DAO_VARIANT ) goto ErrorTyping;
			if( routConsts->items.pValue[opb]->type != DAO_TYPE ) goto ErrorTyping;
			bt = (DaoType*) routConsts->items.pValue[opb];
			DaoInferencer_UpdateType( self, opc, bt );
			AssertTypeMatching( bt, types[opc], defs );
			ct = types[opc];
			for(j=0,k=0; j<at->nested->size; ++j){
				int mt = DaoType_MatchTo( at->nested->items.pType[j], ct, defs );
				k += at->nested->items.pType[j]->tid == ct->tid;
				if( mt >= DAO_MT_EQ ) break;
			}
			if( k != 1 ) goto ErrorTyping;
			switch( ct->tid ){
			case DAO_ENUM : if( vmc->code == DVM_CAST_VE ) break; goto ErrorTyping;
			default : if( vmc->code == DVM_CAST_VX ) break; goto ErrorTyping;
			}
			break;
		case DVM_LOAD :
			DaoInferencer_UpdateType( self, opc, at );
			AssertTypeMatching( at, types[opc], defs );
			if( at == types[opc] && at->tid >= DAO_ARRAY && at->tid <= DAO_TYPE && consts[opa] == NULL ){
				vmc->code = DVM_MOVE_PP;
			}
			break;
		case DVM_MOVE :
			{
				if( opc == 0 && !(routine->attribs & (DAO_ROUT_INITOR|DAO_ROUT_STATIC)) ){
					if( routine->routHost ) goto InvOper; /* Invalid move to "self"; */
				}
				at = DaoInferencer_HandleVarInvarDecl( self, at, opb );
				if( at == NULL ) return 0;
				if( opb & 0x2 ){
					at = DaoType_DefineTypes( at, NS, defs );

					if( ct == NULL || ct->tid == DAO_UDT || ct->tid == DAO_THT ){
						GC_Assign( & types[opc], at );
					}
				}else{
					if( at->tid <= DAO_ENUM ) at = DaoType_GetBaseType( at );
					DaoInferencer_UpdateType( self, opc, at );
				}
				k = DaoType_MatchTo( at, types[opc], defs );
				ct = types[opc];

#if 0
				DaoVmCodeX_Print( *vmc, NULL, NULL );
				if( at ) printf( "a: %s %i\n", at->name->chars, at->invar );
				if( ct ) printf( "c: %s %i\n", ct->name->chars, ct->invar );
				printf( "%i  %i\n", DAO_MT_SUB, k );
#endif

				if( at->tid == DAO_UDT || at->tid == DAO_ANY ){
					/* less strict checking */
				}else if( at != ct && (ct->tid == DAO_OBJECT || ct->tid == DAO_CDATA || ct->tid == DAO_CSTRUCT) ){
					if( ct->tid == DAO_OBJECT ){
						meth = DaoClass_FindMethod( & ct->aux->xClass, "=", hostClass );
					}else{
						meth = DaoType_FindFunctionChars( ct, "=" );
					}
					if( meth ){
						rout = DaoRoutine_Check( meth, ct, & at, 1, DVM_CALL, errors );
						if( rout == NULL ) goto NotMatch;
					}else if( k ==0 ){
						return DaoInferencer_ErrorTypeNotMatching( self, at, types[opc] );
					}
				}else if( at->tid ==DAO_TUPLE && DaoType_MatchTo(types[opc], at, defs)){
					/* less strict checking */
				}else if( k ==0 ){
					return DaoInferencer_ErrorTypeNotMatching( self, at, types[opc] );
				}

				if( k == DAO_MT_SUB && at != ct ){
					/* L = { 1.5, 2.5 }; L = { 1, 2 }; L[0] = 3.5 */
					if( at->tid && at->tid <= DAO_COMPLEX && types[opc]->tid == DAO_COMPLEX ){
						if( at->tid < DAO_FLOAT ){
							DaoInferencer_InsertMove( self, inode, & inode->a, at, dao_type_float );
							at = dao_type_float;
						}
						vmc->code = DVM_MOVE_CF + (at->tid - DAO_FLOAT);
					}
					break;
				}

				if( at->realnum && ct->realnum ){
					int K = DAO_FLOAT - DAO_BOOLEAN + 1;
					vmc->code = DVM_MOVE_BB + K*(ct->tid - DAO_BOOLEAN) + at->tid - DAO_BOOLEAN;
				}else if( at->tid && at->tid <= DAO_COMPLEX && ct->tid == DAO_COMPLEX ){
					if( at->tid < DAO_FLOAT ){
						DaoInferencer_InsertMove( self, inode, & inode->a, at, dao_type_float );
						at = dao_type_float;
					}
					vmc->code = DVM_MOVE_CF + (at->tid - DAO_FLOAT);
				}else if( at->tid == DAO_STRING && ct->tid == DAO_STRING ){
					vmc->code = DVM_MOVE_SS;
				}else if( at == ct || ct->tid == DAO_ANY ){
					if( types[opa]->konst ){
						vmc->code = DVM_MOVE_XX;
					}else if( at->tid >= DAO_ARRAY && at->tid <= DAO_TYPE && consts[opa] == NULL ){
						vmc->code = DVM_MOVE_PP;
					}else{
						vmc->code = DVM_MOVE_XX;
					}
				}
				break;
			}
		case DVM_UNTAG :
			tt = DaoType_GetAutoCastType( at );
			if( tt == NULL ) goto ErrorTyping;
			DaoInferencer_UpdateType( self, opc, tt );
			AssertTypeMatching( tt, types[opc], defs );
			break;
		case DVM_ADD : case DVM_SUB : case DVM_MUL :
		case DVM_DIV : case DVM_MOD : case DVM_POW :
			if( DaoInferencer_HandleBinaryArith( self, inode, defs ) == 0 ) return 0;
			break;
		case DVM_AND : case DVM_OR :
			if( DaoInferencer_HandleBinaryBool( self, inode, defs ) == 0 ) return 0;
			break;
		case DVM_LT : case DVM_LE :
		case DVM_EQ : case DVM_NE :
			{
				ct = dao_type_bool;
				if( NoCheckingType( at ) || NoCheckingType( bt ) ){
					ct = dao_type_udf;
				}else if( at->tid == DAO_OBJECT || bt->tid == DAO_OBJECT
						|| at->tid == DAO_CDATA || bt->tid == DAO_CDATA
						|| at->tid == DAO_CSTRUCT || bt->tid == DAO_CSTRUCT
						|| at->tid == DAO_CINVALUE || bt->tid == DAO_CINVALUE 
						|| at->tid == DAO_INTERFACE || bt->tid == DAO_INTERFACE ){
					ct = DaoCheckBinArith( routine, vmc, at, bt, types[opc], hostClass, mbs );
					if( ct == NULL && code != DVM_EQ && code != DVM_NE ) goto InvOper;
					if( ct == NULL && at->tid != DAO_NONE && bt->tid != DAO_NONE ){
						j = DaoType_MatchTo( at, bt, NULL );
						k = DaoType_MatchTo( bt, at, NULL );
						if( j == 0 && k == 0 ) goto InvOper;
					}
					if( ct == NULL ) ct = dao_type_bool;
				}else if( at->tid == bt->tid ){
					if( at->tid == DAO_COMPLEX && code < DVM_EQ ) goto InvOper;
					if( at->tid > DAO_TUPLE && code != DVM_EQ && code != DVM_NE ) goto InvOper;
				}else if( at->tid >= DAO_BOOLEAN && at->tid <= DAO_FLOAT
						&& bt->tid >= DAO_BOOLEAN && bt->tid <= DAO_FLOAT ){
					/* pass */
				}else if( code != DVM_EQ && code != DVM_NE ){
					goto InvOper;
				}else if( at->tid != DAO_NONE && bt->tid != DAO_NONE ){
					j = DaoType_MatchTo( at, bt, NULL );
					k = DaoType_MatchTo( bt, at, NULL );
					if( j == 0 && k == 0 ) goto InvOper;
				}
				DaoInferencer_UpdateVarType( self, opc, ct );
				/* allow less strict typing: */
				if( ct->tid == DAO_UDT || ct->tid == DAO_ANY ) continue;
				AssertTypeMatching( ct, types[opc], defs );
				ct = types[opc];
				if( at->realnum && bt->realnum && ct->realnum ){
					DaoType *max = at->tid > bt->tid ? at : bt;
					if( at->tid != max->tid ){
						DaoInferencer_InsertMove( self, inode, & inode->a, at, max );
						if( opa == opb ) inode->b = inode->a;
					}
					if( opa != opb && bt->tid != max->tid )
						DaoInferencer_InsertMove( self, inode, & inode->b, bt, max );

					switch( max->tid ){
					case DAO_BOOLEAN : vmc->code += DVM_LT_BBB - DVM_LT; break;
					case DAO_INTEGER : vmc->code += DVM_LT_BII - DVM_LT; break;
					case DAO_FLOAT  : vmc->code += DVM_LT_BFF - DVM_LT; break;
					}
					if( ct->tid != DAO_BOOLEAN )
						DaoInferencer_InsertMove2( self, inode, dao_type_bool, ct );
				}else if( ct->realnum && at->tid == bt->tid && bt->tid == DAO_COMPLEX ){
					vmc->code += DVM_EQ_BCC - DVM_EQ;
					if( ct->tid != DAO_BOOLEAN )
						DaoInferencer_InsertMove2( self, inode, dao_type_bool, ct );
				}else if( ct->realnum && at->tid == bt->tid && bt->tid == DAO_STRING ){
					vmc->code += DVM_LT_BSS - DVM_LT;
					if( ct->tid != DAO_BOOLEAN )
						DaoInferencer_InsertMove2( self, inode, dao_type_bool, ct );
				}
				break;
			}
		case DVM_IN :
			ct = dao_type_bool;
			if( at->tid != DAO_ENUM && bt->tid == DAO_ENUM ) goto InvOper;
			if( NoCheckingType( bt ) ==0 ){
				if( bt->tid < DAO_STRING ) goto InvOper;
			}
			DaoInferencer_UpdateType( self, opc, ct );
			AssertTypeMatching( ct, types[opc], defs );
			break;
		case DVM_NOT :
			ct = NoCheckingType( at ) ? dao_type_udf : dao_type_bool;
			if( at->tid == DAO_OBJECT || at->tid == DAO_CDATA || at->tid == DAO_CINVALUE
					|| at->tid == DAO_CSTRUCT || at->tid == DAO_INTERFACE ){
				ct = DaoCheckBinArith( routine, vmc, at, NULL, types[opc], hostClass, mbs );
				if( ct == NULL ) ct = dao_type_bool;
			}
			if( at->subtid == DAO_ENUM_SYM ) goto InvOper;
			DaoInferencer_UpdateVarType( self, opc, ct );
			AssertTypeMatching( ct, types[opc], defs );
			if( NoCheckingType( at ) ) continue;
			ct = types[opc];
			if( at->realnum ){
				if( ct->realnum ) inode->code = DVM_NOT_B + (at->tid - DAO_BOOLEAN);
				if( ct->tid != DAO_BOOLEAN ) DaoInferencer_InsertMove2( self, inode, dao_type_bool, ct );
				continue;
			}
			if( at->tid == DAO_ENUM || at->tid == DAO_ARRAY ) continue;
			if( at->tid >= DAO_OBJECT && at->tid <= DAO_INTERFACE ) continue;
			goto InvOper;
			break;
		case DVM_MINUS :
			ct = DaoInferencer_UpdateVarType( self, opc, at );
			if( at->tid == DAO_OBJECT || at->tid == DAO_CDATA || at->tid == DAO_CINVALUE
					|| at->tid == DAO_CSTRUCT || at->tid == DAO_INTERFACE ){
				ct = DaoCheckBinArith( routine, vmc, at, NULL, types[opc], hostClass, mbs );
				if( ct == NULL ) goto InvOper;
			}
			AssertTypeMatching( at, ct, defs );
			if( NoCheckingType( at ) ) continue;
			if( at->tid >= DAO_INTEGER && at->tid <= DAO_COMPLEX ){
				if( at != ct ) DaoInferencer_InsertMove( self, inode, & inode->a, at, ct );
				inode->code = DVM_MINUS_I + (ct->tid - DAO_INTEGER);
				continue;
			}
			if( at->tid == DAO_ARRAY ) continue;
			if( at->tid >= DAO_OBJECT && at->tid <= DAO_INTERFACE ) continue;
			goto InvOper;
			break;
		case DVM_TILDE :
			{
				ct = DaoInferencer_UpdateVarType( self, opc, at );
				if( NoCheckingType( at ) ) continue;
				if( at->tid == DAO_OBJECT || at->tid == DAO_CDATA || at->tid == DAO_CINVALUE
						|| at->tid == DAO_CSTRUCT || at->tid == DAO_INTERFACE ){
					ct = DaoCheckBinArith( routine, vmc, at, NULL, types[opc], hostClass, mbs );
					if( ct == NULL ) goto InvOper;
				}
				AssertTypeMatching( at, ct, defs );
				if( at->realnum && ct->realnum ){
					if( at->tid != DAO_INTEGER )
						DaoInferencer_InsertMove( self, inode, & inode->a, at, dao_type_int );
					vmc->code = DVM_TILDE_I;
					if( ct->tid != DAO_INTEGER )
						DaoInferencer_InsertMove2( self, inode, dao_type_int, ct );
				}else if( at->tid == DAO_COMPLEX && ct->tid == DAO_COMPLEX ){
					vmc->code = DVM_TILDE_C;
				}
				break;
			}
		case DVM_SIZE :
			{
				ct = DaoInferencer_UpdateType( self, opc, dao_type_int );
				if( NoCheckingType( at ) ) continue;
				if( at->tid == DAO_OBJECT || at->tid == DAO_CDATA || at->tid == DAO_CINVALUE
						|| at->tid == DAO_CSTRUCT || at->tid == DAO_INTERFACE ){
					ct = DaoCheckBinArith( routine, vmc, at, NULL, types[opc], hostClass, mbs );
					if( ct == NULL ) goto InvOper;
				}
				AssertTypeMatching( dao_type_int, ct, defs );
				if( at->tid >= DAO_INTEGER && at->tid <= DAO_COMPLEX ){
					vmc->code = DVM_DATA_I;
					vmc->a = DAO_INTEGER;
					switch( at->tid ){
					case DAO_BOOLEAN : vmc->b = sizeof(dao_integer); break;
					case DAO_INTEGER : vmc->b = sizeof(dao_integer); break;
					case DAO_FLOAT   : vmc->b = sizeof(dao_float); break;
					case DAO_COMPLEX : vmc->b = sizeof(dao_complex); break;
					}
				}
				break;
			}
		case DVM_BITAND : case DVM_BITOR : case DVM_BITXOR :
		case DVM_BITLFT : case DVM_BITRIT :
			{
				ct = NULL;
				if( NoCheckingType( at ) || NoCheckingType( bt ) ){
					ct = dao_type_udf;
				}else if( at->tid == DAO_OBJECT || bt->tid == DAO_OBJECT
						|| at->tid == DAO_CDATA || bt->tid == DAO_CDATA
						|| at->tid == DAO_CSTRUCT || bt->tid == DAO_CSTRUCT
						|| at->tid == DAO_CINVALUE || bt->tid == DAO_CINVALUE 
						|| at->tid == DAO_INTERFACE || bt->tid == DAO_INTERFACE ){
					ct = DaoCheckBinArith( routine, vmc, at, bt, ct, hostClass, mbs );
					if( ct == NULL ) goto InvOper;
				}else if( at->tid == bt->tid && at->tid == DAO_ENUM ){
					if( code != DVM_BITAND && code != DVM_BITOR ) goto InvOper;
					if( at != bt ) goto InvOper;
					ct = at;
				}else if( at->tid == bt->tid ){
					ct = at;
					if( at->tid > DAO_FLOAT ) goto InvOper;
				}else if( at->realnum && bt->realnum ){
					ct = at->tid > bt->tid ? at : bt;
				}else{
					goto InvOper;
				}
				if( at->realnum && bt->realnum ) ct = dao_type_int;
				DaoInferencer_UpdateVarType( self, opc, ct );
				/* allow less strict typing: */
				if( ct->tid == DAO_UDT || ct->tid == DAO_ANY ) continue;
				AssertTypeMatching( ct, types[opc], defs );
				ct = types[opc];
				if( at->realnum && bt->realnum && ct->realnum ){
					vmc->code += DVM_BITAND_III - DVM_BITAND;
					if( at->tid != DAO_INTEGER ){
						DaoInferencer_InsertMove( self, inode, & inode->a, at, dao_type_int );
						if( opa == opb ) inode->b = inode->a;
					}
					if( opa != opb && bt->tid != DAO_INTEGER )
						DaoInferencer_InsertMove( self, inode, & inode->b, bt, dao_type_int );
					if( ct->tid != DAO_INTEGER )
						DaoInferencer_InsertMove2( self, inode, dao_type_int, ct );
				}
				break;
			}
		case DVM_SAME :
			{
				DaoInferencer_UpdateVarType( self, opc, dao_type_bool );
				if( NoCheckingType( at ) || NoCheckingType( bt ) ) continue;
				AssertTypeMatching( dao_type_int, types[opc], defs );
				if( at->tid != bt->tid && types[opc]->tid == DAO_BOOLEAN ){
					vmc->code = DVM_DATA_B;
					vmc->b = 0;
				}
				break;
			}
		case DVM_ISA :
			{
				DaoInferencer_UpdateVarType( self, opc, dao_type_bool );
				if( NoCheckingType( bt ) ) continue;
				if( bt->tid == DAO_CTYPE || bt->tid == DAO_CLASS ){
					AssertTypeMatching( dao_type_bool, types[opc], defs );
					continue;
				}
				if( bt->tid != DAO_TYPE ) goto ErrorTyping;
				AssertTypeMatching( dao_type_bool, types[opc], defs );
				ct = types[opc];
				k = bt->tid == DAO_TYPE ? bt->nested->items.pType[0]->tid : DAO_UDT;
				if( k <= DAO_STRING && ct->tid == DAO_BOOLEAN ){
					if( at->tid == k ){
						vmc->code = DVM_DATA_B;
						vmc->b = 1;
					}else{
						vmc->code = DVM_ISA_ST;
					}
				}
				break;
			}
		case DVM_ISA_ST :
			{
				DaoInferencer_UpdateType( self, opc, dao_type_bool );
				if( bt->tid != DAO_TYPE ) goto ErrorTyping;
				k = bt->tid == DAO_TYPE ? bt->nested->items.pType[0]->tid : DAO_UDT;
				if( k > DAO_STRING ) goto ErrorTyping;
				break;
			}
		case DVM_NAMEVA :
			{
				ct = DaoNamespace_MakeType2( NS, routConsts->items.pValue[opa]->xString.value->chars,
						DAO_PAR_NAMED, (DaoValue*) types[opb], 0, 0 );
				DaoInferencer_UpdateType( self, opc, ct );
				AssertTypeMatching( ct, types[opc], defs );
				break;
			}
		case DVM_PAIR :
			{
				if( types[opc] && types[opc]->tid == DAO_ANY ) continue;
				ct = DaoNamespace_MakePairType( NS, types[opa], types[opb] );
				DaoInferencer_UpdateType( self, opc, ct );
				AssertTypeMatching( ct, types[opc], defs );
				break;
			}
		case DVM_TUPLE :
			{
				int mode = opb >> 14;
				opb = opb & (0xffff>>2);
				ct = NULL;
				if( mode == DVM_ENUM_MODE1 ){
					k = routine->routType->nested->size;
					tp = routine->routType->nested->items.pType;
					ct = DaoNamespace_MakeType( NS, "tuple", DAO_TUPLE, NULL, tp+opa, k-opa );
					DaoInferencer_UpdateType( self, opc, ct );
				}else if( mode == DVM_ENUM_MODE2 ){
					DaoType *t, *its[DAO_MAX_PARAM];
					DaoInode *sect = inodes[i-1];
					int count = sect->c;

					memcpy( its, types + opa, sect->c * sizeof(DaoType*) );
					if( opb > sect->c ){
						t = DaoNamespace_MakeType( NS, "...", DAO_PAR_VALIST, 0, 0 ,0 );
						its[count++] = t;
					}
					ct = DaoNamespace_MakeType2( NS, "tuple", DAO_TUPLE, 0, its, count );
					DaoInferencer_UpdateType( self, opc, ct );
				}
				if( ct == NULL ){
					ct = DaoNamespace_MakeType2( NS, "tuple", DAO_TUPLE, 0, types + opa, opb );
					DaoInferencer_UpdateType( self, opc, ct );
					if( types[opc]->variadic == 0 && opb > types[opc]->nested->size ){
						goto InvEnum;
					}
					if( mode == DVM_ENUM_MODE0 ){
						for(j=0; j<opb; ++j){
							DaoType *t = types[opc]->nested->items.pType[j];
							tt = types[opa+j];
							if( t->tid >= DAO_PAR_NAMED && t->tid <= DAO_PAR_VALIST ) break;
							if( tt->tid >= DAO_PAR_NAMED && tt->tid <= DAO_PAR_VALIST ) break;
						}
						if( j >= opb ) vmc->code = DVM_TUPLE_SIM;
					}
				}
				AssertTypeMatching( ct, types[opc], defs );
				break;
			}
		case DVM_TUPLE_SIM :
			ct = DaoNamespace_MakeType2( NS, "tuple", DAO_TUPLE, 0, types + opa, opb );
			DaoInferencer_UpdateType( self, opc, ct );
			AssertTypeMatching( ct, types[opc], defs );
			break;
		case DVM_LIST : case DVM_VECTOR :
			if( DaoInferencer_HandleListArrayEnum( self, inode, defs ) == 0 ) return 0;
			break;
		case DVM_MAP :
			{
				opb = opb & (0xffff>>2);
				if( types[opc] && types[opc]->tid == DAO_ANY ) continue;
				if( types[opc] && types[opc]->tid == DAO_MAP ){
					if( types[opc]->nested && types[opc]->nested->size == 2 ){
						DaoType *kt = types[opc]->nested->items.pType[0];
						DaoType *vt = types[opc]->nested->items.pType[1];
						for(j=0; j<opb; j+=2){
							AssertTypeMatching( types[opa+j], kt, defs );
							AssertTypeMatching( types[opa+j+1], vt, defs );
						}
						continue;
					}
				}
				ts[0] = ts[1] = dao_type_udf;
				if( opb > 0 ){
					ts[0] = types[opa];
					ts[1] = types[opa+1];
					for(j=2; j<opb; j+=2){
						if( DaoType_MatchTo( types[opa+j], ts[0], defs ) ==0 ) ts[0] = NULL;
						if( DaoType_MatchTo( types[opa+j+1], ts[1], defs ) ==0 ) ts[1] = NULL;
						if( ts[0] ==NULL && ts[1] ==NULL ) break;
					}
				}else if( opb == 0 && types[opc] != NULL && types[opc]->tid == DAO_MAP ){
					continue;
				}
				if( ts[0] ==NULL ) ts[0] = opb ? dao_type_any : dao_type_udf;
				if( ts[1] ==NULL ) ts[1] = opb ? dao_type_any : dao_type_udf;
				ct = DaoNamespace_MakeType2( NS, "map", DAO_MAP, NULL, ts, 2 );
				if( opb == 0 ) ct = dao_type_map_empty;
				DaoInferencer_UpdateType( self, opc, ct );
				AssertTypeMatching( ct, types[opc], defs );
				break;
			}
		case DVM_MATRIX :
			{
				k = (vmc->b >> 8) * (0xff & vmc->b);
				if( types[opc] && types[opc]->tid == DAO_ANY ) continue;
				if( k == 0 && types[opc] != NULL ) continue;
				at = k > 0 ? types[opa] : dao_type_udf;
				for( j=0; j<k; j++){
					if( DaoType_MatchTo( types[opa+j], at, defs )==0 ) goto ErrorTyping;
				}
				at = DaoType_GetBaseType( at );
				ct = DaoType_Specialize( dao_type_array, & at, at != NULL );
				DaoInferencer_UpdateType( self, opc, ct );
				AssertTypeMatching( ct, types[opc], defs );
				break;
			}
		case DVM_PACK :
		case DVM_MPACK :
			{
				ct = NULL;
				if( at->tid == DAO_TYPE ) at = at->nested->items.pType[0];
				if( at->tid == DAO_ROUTINE ){
					int wh = 0, mc = 0, call = DVM_CALL + (code - DVM_PACK);
					DList *routines;
					rout = (DaoRoutine*)consts[opa];
					if( rout == NULL && at->subtid == DAO_ROUTINES ) rout = (DaoRoutine*) at->aux;
					routines = (rout && rout->overloads) ? rout->overloads->routines : NULL;
					self->array->size = 0;
					for(j=1; j<=opb; j++) DList_Append( self->array, types[opa+j] );
					ct = DaoRoutine_PartialCheck( NS, at, routines, self->array, call, & wh, & mc );
					if( mc > 1 ) return DaoInferencer_Error( self, DTE_TYPE_AMBIGIOUS_PFA );
					if( ct == NULL ) goto InvOper;
				}else if( at->tid == DAO_CLASS ){
					if( consts[opa] == NULL ) goto NotInit;
					klass = & at->aux->xClass;
					if( !(klass->attribs & DAO_CLS_AUTO_INITOR) ) goto InvOper;
					if( klass->attribs & (DAO_CLS_PRIVATE_VAR|DAO_CLS_PROTECTED_VAR) ) goto InvOper;
					if( opb >= klass->instvars->size ) goto InvEnum;
					str = klass->className;
					ct = klass->objType;
					if( code == DVM_MPACK ){
						opa += 1;
						opb -= 1;
					}
					for(j=1; j<=opb; j++){
						int id = j;
						bt = types[opa+j];
						if( bt == NULL ) goto InvEnum;
						if( bt->tid == DAO_PAR_NAMED ){
							id = DaoClass_GetDataIndex( klass, bt->fname );
							if( LOOKUP_ST( id ) != DAO_OBJECT_VARIABLE ) goto InvEnum;
							bt = & bt->aux->xType;
							id = LOOKUP_ID( id );
						}
						tt = klass->instvars->items.pVar[id]->dtype;
						AssertTypeMatching( bt, tt, defs );
					}
				}else if( at->tid == DAO_TUPLE ){
					ct = at;
					if( code == DVM_MPACK ){
						opa += 1;
						opb -= 1;
					}
					if( opb < (at->nested->size - at->variadic) ) goto InvEnum;
					if( at->variadic == 0 && opb > at->nested->size ) goto InvEnum;
					for(j=0; j<opb; j++){
						bt = types[opa+1+j];
						if( bt == NULL ) goto ErrorTyping;
						if( bt->tid == DAO_PAR_NAMED ){
							if( j >= (at->nested->size - at->variadic) ) goto InvEnum;
							if( at->mapNames == NULL ) goto InvEnum;
							node = MAP_Find( at->mapNames, bt->fname );
							if( node == NULL || node->value.pInt != j ) goto InvEnum;
							bt = & bt->aux->xType;
						}
						if( j >= at->nested->size ){
							tt = at->nested->items.pType[at->nested->size-1];
						}else{
							tt = at->nested->items.pType[j];
						}
						if( tt->tid >= DAO_PAR_NAMED && tt->tid <= DAO_PAR_VALIST ) tt = & tt->aux->xType;
						AssertTypeMatching( bt, tt, defs );
					}
				}else{
					ct = dao_type_udf;
				}
				DaoInferencer_UpdateType( self, opc, ct );
				if( at->tid == DAO_ANY || at->tid == DAO_UDT ) break;
				AssertTypeMatching( ct, types[opc], defs );
				break;
			}
		case DVM_SWITCH :
			if( DaoInferencer_HandleSwitch( self, inode, i, defs ) == 0 ) return 0;
			break;
		case DVM_CASE :
			break;
		case DVM_ITER :
			{
				DaoValue *data;
				int j, skip = 0;
				if( vmc->b ){
					for(j=0; j<vmc->b; ++j){
						AssertTypeMatching( types[opa+j], dao_type_for_iterator, defs );
					}
					DaoInferencer_UpdateType( self, opc, dao_type_bool );
					break;
				}
				ts[0] = dao_type_for_iterator;
				meth = NULL;
				self->type_source = at;
				ct = dao_type_for_iterator;
				DaoInferencer_UpdateType( self, opc, ct );
				AssertTypeMatching( ct, types[opc], defs );
				if( NoCheckingType( at ) ) break;
				DString_SetChars( mbs, "for" );
				switch( at->tid ){
				case DAO_STRING :
				case DAO_ARRAY :
				case DAO_LIST :
				case DAO_MAP :
				case DAO_TUPLE :
					skip = 1;
					break;
				case DAO_CLASS :
				case DAO_OBJECT :
					klass = & at->aux->xClass;
					data = DaoClass_GetData( klass, mbs, hostClass );
					if( data == NULL ) goto NotExist;
					if( data->type == DAO_NONE ) goto NotPermit;
					if( data->xBase.subtype == DAO_OBJECT_VARIABLE && at->tid ==DAO_CLASS ) goto NeedInstVar;
					if( data->type != DAO_CONSTANT || data->xConst.value->type != DAO_ROUTINE ) goto NotMatch;
					meth = (DaoRoutine*) data->xConst.value;
					break;
				case DAO_INTERFACE :
					node = DMap_Find( at->aux->xInterface.methods, mbs );
					if( node == NULL ) goto NotExist;
					meth = node->value.pRoutine;
					break;
				default :
					if( at->typer ) meth = DaoType_FindFunction( at, mbs );
					break;
				}
				if( skip ) break;
				if( meth == NULL ) goto NotMatch;
				rout = DaoRoutine_Check( meth, at, ts, 1, DVM_CALL, errors );
				if( rout == NULL ) goto NotMatch;
				ct = dao_type_for_iterator;
				DaoInferencer_UpdateType( self, opc, ct );
				AssertTypeMatching( ct, types[opc], defs );
				break;
			}
		case DVM_GOTO :
			break;
		case DVM_TEST :
			{
				if( types[opa] == NULL ) goto NotMatch;
				if( at->tid == DAO_STRING ) goto NotMatch;
				if( at->subtid == DAO_ENUM_SYM ) goto NotMatch;
				if( at->tid >= DAO_ARRAY && at->tid <= DAO_TUPLE ) goto NotMatch;
				if( consts[opa] && consts[opa]->type <= DAO_COMPLEX ){
					vmc->code =  DaoValue_IsZero( consts[opa] ) ? (int)DVM_GOTO : (int)DVM_UNUSED;
					continue;
				}
				if( at->tid >= DAO_BOOLEAN && at->tid <= DAO_FLOAT )
					vmc->code = DVM_TEST_B + at->tid - DAO_BOOLEAN;
				break;
			}
		case DVM_MATH :
			if( bt->tid == DAO_NONE ) goto InvParam;
			if( bt->tid > DAO_COMPLEX && !(bt->tid & DAO_ANY) ) goto InvParam;
			ct = bt; /* return the same type as the argument by default; */
			K = bt->realnum ? DVM_MATH_B + (bt->tid - DAO_BOOLEAN) : DVM_MATH;
			if( opa <= DVM_MATH_FLOOR ){
				DaoInferencer_UpdateVarType( self, opc, ct );
				if( bt->tid <= DAO_COMPLEX && types[opc]->tid == bt->tid ) code = K;
			}else if( opa == DVM_MATH_ABS ){
				if( bt->tid == DAO_COMPLEX ) ct = dao_type_float; /* return double; */
				DaoInferencer_UpdateVarType( self, opc, ct );
				if( bt->tid == DAO_COMPLEX && types[opc]->tid == DAO_FLOAT ) code = K;
				if( bt->realnum && types[opc]->tid == bt->tid ) code = K;
			}else if( opa <= DVM_MATH_REAL ){
				if( bt->tid != DAO_COMPLEX && !(bt->tid & DAO_ANY) ) goto InvParam;
				ct = dao_type_float; /* return double; */
				DaoInferencer_UpdateVarType( self, opc, ct );
				if( bt->tid == DAO_COMPLEX && types[opc]->tid == DAO_FLOAT ) code = K;
			}else if( bt->tid >= DAO_BOOLEAN && bt->tid <= DAO_FLOAT ){
				ct = dao_type_float; /* return float; */
				DaoInferencer_UpdateVarType( self, opc, ct );
				if( types[opc]->tid == DAO_FLOAT ) code = K;
			}else{
				DaoInferencer_UpdateVarType( self, opc, ct );
				if( bt->tid <= DAO_COMPLEX && types[opc]->tid == bt->tid ) code = K;
			}
			AssertTypeMatching( ct, types[opc], defs );
			inode->code = K;
			break;
		case DVM_CALL : case DVM_MCALL :
			if( DaoInferencer_HandleCall( self, inode, i, defs ) == 0 ) return 0;
			break;
		case DVM_ROUTINE :
			if( DaoInferencer_HandleClosure( self, inode, i, defs ) == 0 ) return 0;
			break;

		case DVM_RETURN :
		case DVM_YIELD :
			if( DaoInferencer_HandleYieldReturn( self, inode, defs ) == 0 ) return 0;
			break;

		case DVM_DATA_B : case DVM_DATA_I : case DVM_DATA_F : case DVM_DATA_C :
			TT1 = DAO_BOOLEAN + (code - DVM_DATA_B);
			ct = DaoInferencer_UpdateType( self, opc, self->basicTypes[TT1] );
			AssertTypeIdMatching( ct, TT1 );
			break;
		case DVM_GETCL_B : case DVM_GETCL_I : case DVM_GETCL_F : case DVM_GETCL_C :
			value = routConsts->items.pValue[opb];
			TT1 = DAO_BOOLEAN + (code - DVM_GETCL_B);
			at = DaoNamespace_GetType( NS, value );
			ct = DaoInferencer_UpdateType( self, opc, self->basicTypes[TT1] );
			AssertTypeIdMatching( at, TT1 );
			AssertTypeIdMatching( ct, TT1 );
			break;
		case DVM_GETCK_B : case DVM_GETCK_I : case DVM_GETCK_F : case DVM_GETCK_C :
			value = hostClass->constants->items.pConst[opb]->value;
			TT1 = DAO_BOOLEAN + (code - DVM_GETCK_B);
			at = DaoNamespace_GetType( NS, value );
			ct = DaoInferencer_UpdateType( self, opc, self->basicTypes[TT1] );
			AssertTypeIdMatching( at, TT1 );
			AssertTypeIdMatching( ct, TT1 );
			break;
		case DVM_GETCG_B : case DVM_GETCG_I : case DVM_GETCG_F : case DVM_GETCG_C :
			value = NS->constants->items.pConst[opb]->value;
			TT1 = DAO_BOOLEAN + (code - DVM_GETCG_B);
			at = DaoNamespace_GetType( NS, value );
			ct = DaoInferencer_UpdateType( self, opc, self->basicTypes[TT1] );
			AssertTypeIdMatching( at, TT1 );
			AssertTypeIdMatching( ct, TT1 );
			break;
		case DVM_GETVH_B : case DVM_GETVH_I : case DVM_GETVH_F : case DVM_GETVH_C :
			TT1 = DAO_BOOLEAN + (code - DVM_GETVH_B);
			at = typeVH[opa][opb];
			ct = DaoInferencer_UpdateType( self, opc, self->basicTypes[TT1] );
			AssertTypeIdMatching( at, TT1 );
			AssertTypeIdMatching( ct, TT1 );
			break;
		case DVM_GETVS_B : case DVM_GETVS_I : case DVM_GETVS_F : case DVM_GETVS_C :
			TT1 = DAO_BOOLEAN + (code - DVM_GETVS_B);
			at = body->upValues->items.pVar[opb]->dtype;
			ct = DaoInferencer_UpdateType( self, opc, self->basicTypes[TT1] );
			AssertTypeIdMatching( at, TT1 );
			AssertTypeIdMatching( ct, TT1 );
			break;
		case DVM_GETVO_B : case DVM_GETVO_I : case DVM_GETVO_F : case DVM_GETVO_C :
			TT1 = DAO_BOOLEAN + (code - DVM_GETVO_B);
			at = hostClass->instvars->items.pVar[opb]->dtype;
			ct = DaoInferencer_UpdateType( self, opc, self->basicTypes[TT1] );
			AssertTypeIdMatching( at, TT1 );
			AssertTypeIdMatching( ct, TT1 );
			break;
		case DVM_GETVK_B : case DVM_GETVK_I : case DVM_GETVK_F : case DVM_GETVK_C :
			TT1 = DAO_BOOLEAN + (code - DVM_GETVK_B);
			at = hostClass->variables->items.pVar[opb]->dtype;
			ct = DaoInferencer_UpdateType( self, opc, self->basicTypes[TT1] );
			AssertTypeIdMatching( at, TT1 );
			AssertTypeIdMatching( ct, TT1 );
			break;
		case DVM_GETVG_B : case DVM_GETVG_I : case DVM_GETVG_F : case DVM_GETVG_C :
			TT1 = DAO_BOOLEAN + (code - DVM_GETVG_B);
			at = NS->variables->items.pVar[opb]->dtype;
			ct = DaoInferencer_UpdateType( self, opc, self->basicTypes[TT1] );
			AssertTypeIdMatching( at, TT1 );
			AssertTypeIdMatching( ct, TT1 );
			break;
		case DVM_SETVH_BB : case DVM_SETVH_II : case DVM_SETVH_FF : case DVM_SETVH_CC :
			tp = typeVH[opc] + opb;
			at = DaoInferencer_HandleVarInvarDecl( self, at, opc );
			if( at == NULL ) return 0;
			if( at->tid <= DAO_ENUM ) at = DaoType_GetBaseType( at );
			if( *tp == NULL || (*tp)->tid == DAO_UDT || (*tp)->tid == DAO_THT ){
				GC_Assign( tp, at );
			}
			TT1 = DAO_BOOLEAN + (code - DVM_SETVH_BB);
			AssertTypeMatching( at, *tp, defs );
			AssertTypeIdMatching( at, TT1 );
			AssertTypeIdMatching( tp[0], TT1 );
			break;
		case DVM_SETVS_BB : case DVM_SETVS_II : case DVM_SETVS_FF : case DVM_SETVS_CC :
			var = body->upValues->items.pVar[opb];
			at = DaoInferencer_HandleVarInvarDecl( self, at, opc );
			if( at == NULL ) return 0;
			if( at->tid <= DAO_ENUM ) at = DaoType_GetBaseType( at );
			if( var->dtype == NULL || var->dtype->tid == DAO_UDT || var->dtype->tid == DAO_THT ){
				DaoVariable_SetType( var, at );
			}
			TT1 = DAO_BOOLEAN + (code - DVM_SETVS_BB);
			AssertTypeMatching( at, var->dtype, defs );
			AssertTypeIdMatching( at, TT1 );
			AssertTypeIdMatching( var->dtype, TT1 );
			break;
		case DVM_SETVO_BB : case DVM_SETVO_II : case DVM_SETVO_FF : case DVM_SETVO_CC :
			if( self->tidHost != DAO_OBJECT ) goto ErrorTyping;
			var = hostClass->instvars->items.pVar[opb];
			at = DaoInferencer_HandleVarInvarDecl( self, at, opc );
			if( at == NULL ) return 0;
			if( var->subtype == DAO_INVAR ){
				if( !(routine->attribs & DAO_ROUT_INITOR) ) goto ModifyConstant;
				at = DaoType_GetInvarType( at );
			}
			if( var->dtype == NULL || var->dtype->tid == DAO_UDT || var->dtype->tid == DAO_THT ){
				DaoVariable_SetType( var, at );
			}
			TT1 = DAO_BOOLEAN + (code - DVM_SETVO_BB);
			AssertTypeMatching( at, var->dtype, defs );
			AssertTypeIdMatching( at, TT1 );
			AssertTypeIdMatching( var->dtype, TT1 );
			break;
		case DVM_SETVK_BB : case DVM_SETVK_II : case DVM_SETVK_FF : case DVM_SETVK_CC :
			var = hostClass->variables->items.pVar[opb];
			at = DaoInferencer_HandleVarInvarDecl( self, at, opc );
			if( at == NULL ) return 0;
			if( at->tid <= DAO_ENUM ) at = DaoType_GetBaseType( at );
			if( var->dtype == NULL || var->dtype->tid == DAO_UDT || var->dtype->tid == DAO_THT ){
				DaoVariable_SetType( var, at );
			}
			TT1 = DAO_BOOLEAN + (code - DVM_SETVK_BB);
			AssertTypeMatching( at, var->dtype, defs );
			AssertTypeIdMatching( at, TT1 );
			AssertTypeIdMatching( var->dtype, TT1 );
			break;
		case DVM_SETVG_BB : case DVM_SETVG_II : case DVM_SETVG_FF : case DVM_SETVG_CC :
			var = NS->variables->items.pVar[opb];
			if( !(opc & 0x4) && var->dtype && var->dtype->invar ) goto ModifyConstant;
			at = DaoInferencer_HandleVarInvarDecl( self, at, opc );
			if( at == NULL ) return 0;
			if( at->tid <= DAO_ENUM ) at = DaoType_GetBaseType( at );
			if( var->dtype == NULL || var->dtype->tid == DAO_UDT || var->dtype->tid == DAO_THT ){
				DaoVariable_SetType( var, at );
			}
			TT1 = DAO_BOOLEAN + (code - DVM_SETVG_BB);
			AssertTypeMatching( at, var->dtype, defs );
			AssertTypeIdMatching( at, TT1 );
			AssertTypeIdMatching( var->dtype, TT1 );
			break;
		case DVM_MOVE_BB : case DVM_MOVE_BI : case DVM_MOVE_BF :
		case DVM_MOVE_IB : case DVM_MOVE_II : case DVM_MOVE_IF :
		case DVM_MOVE_FB : case DVM_MOVE_FI : case DVM_MOVE_FF :
		case DVM_MOVE_CF :
			k = DAO_FLOAT - DAO_BOOLEAN + 1;
			TT1 = DAO_BOOLEAN + (code - DVM_MOVE_BB) % k;
			TT3 = DAO_BOOLEAN + ((code - DVM_MOVE_BB)/k) % k;
			if( code == DVM_MOVE_CF ){
				TT1 = DAO_FLOAT;
				TT2 = DAO_COMPLEX;
			}
			at = DaoInferencer_HandleVarInvarDecl( self, at, opb );
			if( at == NULL ) return 0;
			if( opb & 0x2 ){
				if( ct == NULL || ct->tid == DAO_UDT || ct->tid == DAO_THT ){
					ct = self->basicTypes[TT3];
					if( opb & 0x4 ) ct = DaoType_GetInvarType( ct );
					GC_Assign( & types[opc], ct );
				}
			}else{
				DaoInferencer_UpdateType( self, opc, self->basicTypes[TT3] );
			}
			AssertTypeIdMatching( at, TT1 );
			AssertTypeIdMatching( types[opc], TT3 );
			break;
		case DVM_NOT_B : case DVM_NOT_I : case DVM_NOT_F :
			DaoInferencer_UpdateVarType( self, opc, dao_type_bool );
			TT1 = DAO_BOOLEAN + code - DVM_NOT_B;
			TT3 = DAO_BOOLEAN;
			AssertTypeIdMatching( at, TT1 );
			AssertTypeIdMatching( types[opc], TT3 );
			break;
		case DVM_MINUS_I : case DVM_MINUS_F :
			DaoInferencer_UpdateVarType( self, opc, at );
			TT1 = TT3 = DAO_INTEGER + code - DVM_MINUS_I;
			AssertTypeIdMatching( at, TT1 );
			AssertTypeIdMatching( types[opc], TT3 );
			break;
		case DVM_TILDE_I :
			DaoInferencer_UpdateVarType( self, opc, at );
			AssertTypeIdMatching( at, DAO_INTEGER );
			AssertTypeIdMatching( types[opc], DAO_INTEGER );
			break;
		case DVM_TILDE_C :
			DaoInferencer_UpdateVarType( self, opc, at );
			AssertTypeIdMatching( at, DAO_COMPLEX );
			AssertTypeIdMatching( types[opc], DAO_COMPLEX );
			break;
		case DVM_MINUS_C :
			DaoInferencer_UpdateVarType( self, opc, at );
			TT1 = TT3 = code == DVM_MOVE_SS ? DAO_STRING : DAO_COMPLEX;
			AssertTypeIdMatching( at, TT1 );
			AssertTypeIdMatching( types[opc], TT3 );
			break;
		case DVM_MOVE_CC :
		case DVM_MOVE_SS :
			at = DaoInferencer_HandleVarInvarDecl( self, at, opb );
			if( at == NULL ) return 0;
			if( opb & 0x2 ){
				if( ct == NULL || ct->tid == DAO_UDT || ct->tid == DAO_THT ){
					GC_Assign( & types[opc], at );
				}
			}else{
				DaoInferencer_UpdateType( self, opc, at );
			}
			TT1 = TT3 = code == DVM_MOVE_SS ? DAO_STRING : DAO_COMPLEX;
			AssertTypeIdMatching( at, TT1 );
			AssertTypeIdMatching( types[opc], TT3 );
			break;
		case DVM_MOVE_PP :
		case DVM_MOVE_XX :
			if( code == DVM_MOVE_PP ){
				if( at->tid && (at->tid < DAO_ARRAY || at->tid > DAO_TYPE) ) goto NotMatch;
			}
			at = DaoInferencer_HandleVarInvarDecl( self, at, opb );
			if( at == NULL ) return 0;
			if( opb & 0x2 ){
				if( ct == NULL || ct->tid == DAO_UDT || ct->tid == DAO_THT ){
					GC_Assign( & types[opc], at );
				}
			}else{
				DaoInferencer_UpdateType( self, opc, at );
			}
			if( types[opc]->tid != DAO_ANY ){
				if( DaoType_MatchTo( types[opc], at, NULL ) != DAO_MT_EQ ) goto NotMatch;
			}
			break;
		case DVM_AND_BBB : case DVM_OR_BBB : case DVM_LT_BBB :
		case DVM_LE_BBB  : case DVM_EQ_BBB : case DVM_NE_BBB :
			DaoInferencer_UpdateVarType( self, opc, dao_type_bool );
			AssertTypeIdMatching( at, DAO_BOOLEAN );
			AssertTypeIdMatching( bt, DAO_BOOLEAN );
			AssertTypeIdMatching( types[opc], DAO_BOOLEAN );
			break;
		case DVM_AND_BII : case DVM_OR_BII : case DVM_LT_BII :
		case DVM_LE_BII  : case DVM_EQ_BII : case DVM_NE_BII :
			DaoInferencer_UpdateVarType( self, opc, dao_type_bool );
			AssertTypeIdMatching( at, DAO_INTEGER );
			AssertTypeIdMatching( bt, DAO_INTEGER );
			AssertTypeIdMatching( types[opc], DAO_BOOLEAN );
			break;
		case DVM_ADD_III : case DVM_SUB_III : case DVM_MUL_III :
		case DVM_DIV_III : case DVM_MOD_III : case DVM_POW_III :
		case DVM_BITAND_III  : case DVM_BITOR_III  : case DVM_BITXOR_III :
		case DVM_BITLFT_III  : case DVM_BITRIT_III  :
			DaoInferencer_UpdateVarType( self, opc, dao_type_int );
			AssertTypeIdMatching( at, DAO_INTEGER );
			AssertTypeIdMatching( bt, DAO_INTEGER );
			AssertTypeIdMatching( types[opc], DAO_INTEGER );
			break;
		case DVM_ADD_FFF : case DVM_SUB_FFF : case DVM_MUL_FFF :
		case DVM_DIV_FFF : case DVM_MOD_FFF : case DVM_POW_FFF :
		case DVM_AND_BFF : case DVM_OR_BFF : case DVM_LT_BFF  :
		case DVM_LE_BFF  : case DVM_EQ_BFF : case DVM_NE_BFF :
			ct = (code < DVM_AND_BFF) ? dao_type_float : dao_type_bool;
			DaoInferencer_UpdateVarType( self, opc, ct );
			AssertTypeIdMatching( at, DAO_FLOAT );
			AssertTypeIdMatching( bt, DAO_FLOAT );
			AssertTypeIdMatching( types[opc], ct->tid );
			break;
		case DVM_ADD_CCC : case DVM_SUB_CCC : case DVM_MUL_CCC : case DVM_DIV_CCC :
		case DVM_EQ_BCC : case DVM_NE_BCC :
			ct = code < DVM_EQ_BCC ? dao_type_complex : dao_type_bool;
			DaoInferencer_UpdateVarType( self, opc, ct );
			AssertTypeIdMatching( at, DAO_COMPLEX );
			AssertTypeIdMatching( bt, DAO_COMPLEX );
			AssertTypeIdMatching( types[opc], ct->tid );
			break;
		case DVM_ADD_SSS : case DVM_LT_BSS : case DVM_LE_BSS :
		case DVM_EQ_BSS : case DVM_NE_BSS :
			ct = code == DVM_ADD_SSS ? self->typeString : dao_type_bool;
			DaoInferencer_UpdateVarType( self, opc, ct );
			AssertTypeIdMatching( at, DAO_STRING );
			AssertTypeIdMatching( bt, DAO_STRING );
			AssertTypeIdMatching( types[opc], ct->tid );
			break;
		case DVM_GETI_SI :
			AssertTypeIdMatching( at, DAO_STRING );
			if( code == DVM_GETI_SI && bt->tid != DAO_INTEGER ) goto NotMatch;
			DaoInferencer_UpdateType( self, opc, dao_type_int );
			AssertTypeIdMatching( types[opc], DAO_INTEGER );
			break;
		case DVM_SETI_SII :
			AssertTypeIdMatching( at, DAO_INTEGER );
			AssertTypeIdMatching( bt, DAO_INTEGER );
			AssertTypeIdMatching( ct, DAO_STRING );
			break;
		case DVM_GETI_LI :
			AssertTypeIdMatching( at, DAO_LIST );
			AssertTypeIdMatching( bt, DAO_INTEGER );
			at = types[opa]->nested->items.pType[0];
			if( at->tid < DAO_ARRAY || at->tid >= DAO_ANY ) goto NotMatch;
			DaoInferencer_UpdateType( self, opc, at );
			AssertTypeMatching( at, types[opc], defs );
			break;
		case DVM_GETI_LBI : case DVM_GETI_LII : case DVM_GETI_LFI : case DVM_GETI_LCI :
		case DVM_GETI_ABI : case DVM_GETI_AII : case DVM_GETI_AFI : case DVM_GETI_ACI :
		case DVM_GETI_LSI :
			TT1 = TT3 = 0;
			if( code >= DVM_GETI_ABI ){
				TT3 = DAO_ARRAY;
				TT1 = DAO_BOOLEAN + (code - DVM_GETI_ABI);
			}else if( code != DVM_GETI_LSI ){
				TT3 = DAO_LIST;
				TT1 = DAO_BOOLEAN + (code - DVM_GETI_LBI);
			}else{
				TT3 = DAO_LIST;
				TT1 = DAO_STRING;
			}
			if( at->tid != TT3 || at->nested->size ==0 ) goto NotMatch;
			at = at->nested->items.pType[0];
			if( at == NULL || at->tid != TT1 ) goto NotMatch;
			if( bt == NULL || bt->tid != DAO_INTEGER ) goto NotMatch;
			DaoInferencer_UpdateType( self, opc, at );
			AssertTypeIdMatching( types[opc], TT1 );
			break;
		case DVM_GETMI_ABI : case DVM_GETMI_AII :
		case DVM_GETMI_AFI : case DVM_GETMI_ACI :
			for(j=0; j<opb; j++){
				bt = types[opa + j + 1];
				if( bt->tid == DAO_NONE || bt->tid > DAO_FLOAT ) goto InvIndex;
			}
			at = at->nested->items.pType[0];
			DaoInferencer_UpdateType( self, opc, at );
			AssertTypeMatching( at, types[opc], defs );
			break;
		case DVM_SETI_LI :
			AssertTypeIdMatching( bt, DAO_INTEGER );
			AssertTypeIdMatching( ct, DAO_LIST );
			ct = types[opc]->nested->items.pType[0];
			if( at != ct && ct->tid != DAO_ANY ) goto NotMatch;
			break;
		case DVM_SETI_LBIB : case DVM_SETI_LIII : case DVM_SETI_LFIF : case DVM_SETI_LCIC :
		case DVM_SETI_ABIB : case DVM_SETI_AIII : case DVM_SETI_AFIF : case DVM_SETI_ACIC :
		case DVM_SETI_LSIS :
			TT2 = DAO_INTEGER;
			TT1 = TT6 = 0;
			if( code >= DVM_SETI_ABIB ){
				TT6 = DAO_ARRAY;
				TT1 = DAO_BOOLEAN + code - DVM_SETI_ABIB;
			}else if( code != DVM_SETI_LSIS ){
				TT6 = DAO_LIST;
				TT1 = DAO_BOOLEAN + code - DVM_SETI_LBIB;
			}else{
				TT6 = DAO_LIST;
				TT1 = DAO_STRING;
			}
			if( ct->tid != TT6 || bt->tid != TT2 || at->tid != TT1 ) goto NotMatch;
			if( ct->nested->size !=1 || ct->nested->items.pType[0]->tid != TT1 ) goto NotMatch;
			break;
		case DVM_SETMI_ABIB : case DVM_SETMI_AIII :
		case DVM_SETMI_AFIF : case DVM_SETMI_ACIC :
			for(j=0; j<opb; j++){
				bt = types[opc + j + 1];
				if( bt->tid == DAO_NONE || bt->tid > DAO_FLOAT ) goto InvIndex;
			}
			if( at->tid != DAO_BOOLEAN + (code - DVM_SETMI_ABIB) ) goto NotMatch;
			if( at->tid == DAO_NONE || at->tid > DAO_COMPLEX ) goto NotMatch;
			if( ct->tid != DAO_ARRAY || ct->nested->items.pType[0]->tid != at->tid ) goto NotMatch;
			break;
		case DVM_GETI_TI :
			if( at->tid != DAO_TUPLE || bt->tid != DAO_INTEGER ) goto NotMatch;
			ct = DaoNamespace_MakeType( NS, "", DAO_VARIANT, NULL, at->nested->items.pType, at->nested->size );
			DaoInferencer_UpdateType( self, opc, ct );
			break;
		case DVM_SETI_TI :
			if( ct->tid != DAO_TUPLE || bt->tid != DAO_INTEGER ) goto NotMatch;
			break;
		case DVM_SETF_TPP :
		case DVM_SETF_TXX :
			if( at ==NULL || ct ==NULL || ct->tid != DAO_TUPLE ) goto NotMatch;
			if( opb >= ct->nested->size ) goto InvIndex;
			tt = ct->nested->items.pType[opb];
			if( tt->tid == DAO_PAR_NAMED ) tt = & tt->aux->xType;
			if( tt && tt->invar ) goto ModifyConstant;
			if( at != tt && tt->tid != DAO_ANY ) goto NotMatch;
			if( code == DVM_SETF_TPP && consts[opa] ) goto InvOper;
			break;
		case DVM_GETF_TB :
		case DVM_GETF_TI : case DVM_GETF_TF :
		case DVM_GETF_TC : case DVM_GETF_TX :
			if( at ==NULL || at->tid != DAO_TUPLE ) goto NotMatch;
			if( opb >= at->nested->size ) goto InvIndex;
			ct = at->nested->items.pType[opb];
			if( ct->tid == DAO_PAR_NAMED ) ct = & ct->aux->xType;
			DaoInferencer_UpdateType( self, opc, ct );
			if( code != DVM_GETF_TX ){
				TT3 = DAO_BOOLEAN + (code - DVM_GETF_TB);
				if( ct == NULL || ct->tid != TT3 ) goto NotMatch;
				if( types[opc]->tid != TT3 ) goto NotMatch;
			}else{
				AssertTypeMatching( ct, types[opc], defs );
			}
			break;
		case DVM_SETF_TBB : case DVM_SETF_TII : case DVM_SETF_TFF :
		case DVM_SETF_TCC : case DVM_SETF_TSS :
			if( at ==NULL || ct ==NULL ) goto NotMatch;
			TT1 = 0;
			if( code == DVM_SETF_TSS ){
				TT1 = DAO_STRING;
			}else{
				TT1 = DAO_BOOLEAN + (code - DVM_SETF_TBB);
			}
			if( ct->tid != DAO_TUPLE || at->tid != TT1 ) goto NotMatch;
			if( opb >= ct->nested->size ) goto InvIndex;
			tt = ct->nested->items.pType[opb];
			if( tt->tid == DAO_PAR_NAMED ) tt = & tt->aux->xType;
			if( tt && tt->invar ) goto ModifyConstant;
			if( tt->tid != TT1 ) goto NotMatch;
			break;
		case DVM_GETF_CX :
			if( at->tid != DAO_COMPLEX ) goto NotMatch;
			ct = DaoInferencer_UpdateType( self, opc, dao_type_float );
			if( ct->tid != DAO_FLOAT ) goto NotMatch;
			break;
		case DVM_SETF_CX :
			if( at->tid != DAO_FLOAT ) goto NotMatch;
			if( ct->tid != DAO_COMPLEX ) goto NotMatch;
			break;
		case DVM_GETF_KCB : case DVM_GETF_KCI : case DVM_GETF_KCF :
		case DVM_GETF_KCC :
		case DVM_GETF_KC :
			if( types[opa]->tid != DAO_CLASS ) goto NotMatch;
			klass = & types[opa]->aux->xClass;
			ct = DaoNamespace_GetType( NS, klass->constants->items.pConst[ opb ]->value );
			DaoInferencer_UpdateType( self, opc, ct );
			AssertTypeMatching( ct, types[opc], defs );
			if( code == DVM_GETF_KC ) break;
			if( ct->tid != (DAO_BOOLEAN + code - DVM_GETF_KCB) ) goto NotMatch;
			break;
		case DVM_GETF_KGB : case DVM_GETF_KGI : case DVM_GETF_KGF :
		case DVM_GETF_KGC :
		case DVM_GETF_KG :
			if( types[opa]->tid != DAO_CLASS ) goto NotMatch;
			klass = & types[opa]->aux->xClass;
			ct = klass->variables->items.pVar[ opb ]->dtype;
			DaoInferencer_UpdateType( self, opc, ct );
			AssertTypeMatching( ct, types[opc], defs );
			if( code == DVM_GETF_KG ) break;
			if( ct->tid != (DAO_BOOLEAN + code - DVM_GETF_KGB) ) goto NotMatch;
			break;
		case DVM_GETF_OCB : case DVM_GETF_OCI : case DVM_GETF_OCF :
		case DVM_GETF_OCC :
		case DVM_GETF_OC :
			if( types[opa]->tid != DAO_OBJECT ) goto NotMatch;
			klass = & types[opa]->aux->xClass;
			ct = DaoNamespace_GetType( NS, klass->constants->items.pConst[ opb ]->value );
			DaoInferencer_UpdateType( self, opc, ct );
			AssertTypeMatching( ct, types[opc], defs );
			if( code == DVM_GETF_OC ){
				value = klass->constants->items.pConst[opb]->value;
				GC_Assign( & consts[opc], value );
				break;
			}
			if( ct->tid != (DAO_BOOLEAN + code - DVM_GETF_OCB) ) goto NotMatch;
			break;
		case DVM_GETF_OGB : case DVM_GETF_OGI : case DVM_GETF_OGF :
		case DVM_GETF_OGC :
		case DVM_GETF_OG :
			if( types[opa]->tid != DAO_OBJECT ) goto NotMatch;
			klass = & types[opa]->aux->xClass;
			ct = klass->variables->items.pVar[ opb ]->dtype;
			DaoInferencer_UpdateType( self, opc, ct );
			AssertTypeMatching( ct, types[opc], defs );
			if( code == DVM_GETF_OG ) break;
			if( ct->tid != (DAO_BOOLEAN + code - DVM_GETF_OGB) ) goto NotMatch;
			break;
		case DVM_GETF_OVB : case DVM_GETF_OVI : case DVM_GETF_OVF :
		case DVM_GETF_OVC :
		case DVM_GETF_OV :
			if( types[opa]->tid != DAO_OBJECT ) goto NotMatch;
			klass = & types[opa]->aux->xClass;
			ct = klass->instvars->items.pVar[ opb ]->dtype;
			DaoInferencer_UpdateType( self, opc, ct );
			AssertTypeMatching( ct, types[opc], defs );
			if( code == DVM_GETF_OV ) break;
			if( ct->tid != (DAO_BOOLEAN + code - DVM_GETF_OVB) ) goto NotMatch;
			break;
		case DVM_SETF_KGBB : case DVM_SETF_KGII : case DVM_SETF_KGFF :
		case DVM_SETF_KGCC :
		case DVM_SETF_KG :
			if( ct == NULL ) goto ErrorTyping;
			if( types[opa] ==NULL || types[opc] ==NULL ) goto NotMatch;
			if( ct->tid != DAO_CLASS ) goto NotMatch;
			ct = ct->aux->xClass.variables->items.pVar[ opb ]->dtype;
			if( ct && ct->invar && !(routine->attribs & DAO_ROUT_INITOR) ) goto ModifyConstant;
			if( code == DVM_SETF_KG ){
				if( at != ct && ct->tid != DAO_ANY ) goto NotMatch;
				break;
			}
			k = DAO_FLOAT - DAO_BOOLEAN + 1;
			AssertTypeMatching( at, ct, defs );
			if( at->tid != (DAO_BOOLEAN + (code - DVM_SETF_KGBB)%k) ) goto NotMatch;
			if( ct->tid != (DAO_BOOLEAN + (code - DVM_SETF_KGBB)/k) ) goto NotMatch;
			break;
		case DVM_SETF_OGBB : case DVM_SETF_OGII : case DVM_SETF_OGFF :
		case DVM_SETF_OGCC :
		case DVM_SETF_OG :
			if( ct == NULL ) goto ErrorTyping;
			if( types[opa] ==NULL || types[opc] ==NULL ) goto NotMatch;
			if( ct->tid != DAO_OBJECT ) goto NotMatch;
			ct = ct->aux->xClass.variables->items.pVar[ opb ]->dtype;
			if( ct && ct->invar && !(routine->attribs & DAO_ROUT_INITOR) ) goto ModifyConstant;
			if( code == DVM_SETF_OG ){
				if( at != ct && ct->tid != DAO_ANY ) goto NotMatch;
				break;
			}
			if( at->tid != ct->tid ) goto NotMatch;
			if( at->tid != (DAO_BOOLEAN + (code - DVM_SETF_OGBB)) ) goto NotMatch;
			break;
		case DVM_SETF_OVBB : case DVM_SETF_OVII : case DVM_SETF_OVFF :
		case DVM_SETF_OVCC :
		case DVM_SETF_OV :
			if( ct == NULL ) goto ErrorTyping;
			if( types[opa] ==NULL || types[opc] ==NULL ) goto NotMatch;
			if( ct->tid != DAO_OBJECT ) goto NotMatch;
			ct = ct->aux->xClass.instvars->items.pVar[ opb ]->dtype;
			if( ct && ct->invar && !(routine->attribs & DAO_ROUT_INITOR) ) goto ModifyConstant;
			if( code == DVM_SETF_OV ){
				if( ct->tid == DAO_ANY ) break;
				if( DaoType_MatchTo( at, ct, NULL ) != DAO_MT_EQ ) goto NotMatch;
				/* Same type may be represented by different type objects by different namespaces; */
				/* if( at != ct && ct->tid != DAO_ANY ) goto NotMatch; */
				break;
			}
			if( at->tid != ct->tid ) goto NotMatch;
			if( at->tid != (DAO_BOOLEAN + (code - DVM_SETF_OVBB)) ) goto NotMatch;
			break;
		case DVM_MATH_B :
		case DVM_MATH_I :
		case DVM_MATH_F :
			TT1 = DAO_BOOLEAN + (code - DVM_MATH_B);
			type = self->basicTypes[TT1];
			if( opa <= DVM_MATH_ABS ){
				ct = DaoInferencer_UpdateType( self, opc, type );
				if( bt->tid != TT1 || ct->tid != TT1 ) goto NotMatch;
			}else if( bt->tid >= DAO_BOOLEAN && bt->tid <= DAO_FLOAT ){
				ct = DaoInferencer_UpdateType( self, opc, dao_type_float );
				if( ct->tid != DAO_FLOAT ) goto NotMatch;
			}else{
				if( bt->tid == DAO_NONE || bt->tid > DAO_FLOAT ) goto NotMatch;
				DaoInferencer_UpdateType( self, opc, dao_type_float );
				if( ct->tid != DAO_FLOAT ) goto NotMatch;
			}
			break;
		default : break;
		}
		if( self->inodes->size != N ){
			i--;
			continue;
		}
	}

	for(i=0; i<self->defers->size; ++i){
		DaoRoutine *closure = self->defers->items.pRoutine[i];
		DaoType *retype = (DaoType*) routine->routType->aux;
		DaoType *type = closure->routType;
		type = DaoNamespace_MakeRoutType( NS, type, NULL, type->nested->items.pType, retype );
		GC_Assign( & closure->routType, type );
		if( DaoRoutine_DoTypeInference( closure, self->silent ) == 0 ) return 0;
	}
#if 0
	inodes = self->inodes->items.pInode;
	types = self->types->items.pType;
	N = self->inodes->size;
	for(i=0; i<N; i++){
		inode = inodes[i];
		inode->index = i;
		self->currentIndex = i;

		switch( DaoVmCode_GetOpcodeType( (DaoVmCode*) inode ) ){
		case DAO_CODE_GETG :
		case DAO_CODE_GETF :
		case DAO_CODE_GETI :
		case DAO_CODE_UNARY :
		case DAO_CODE_BINARY :
		case DAO_CODE_CALL :
			if( inode->code == DVM_GETVO ) continue;
			if( inode->code == DVM_CALL && (inode->b & DAO_CALL_INIT) ) continue;
			if( invarinit && DaoType_IsPrimitiveOrImmutable( types[inode->c] ) == 0 ){
				return DaoInferencer_Error( self, DTE_INVAR_INITOR_MUTABLE );
			}
			break;
		}
	}
#endif

	DaoInferencer_Finalize( self );
	return 1;
NotMatch: return DaoInferencer_ErrorTypeNotMatching( self, NULL, NULL );
NotInit: return DaoInferencer_ErrorNotInitialized( self, 0, 0, 0 );
NotPermit: return DaoInferencer_Error( self, DTE_FIELD_NOT_PERMIT );
NotExist: return DaoInferencer_Error( self, DTE_FIELD_NOT_EXIST );
NeedInstVar: return DaoInferencer_Error( self, DTE_FIELD_OF_INSTANCE );
ModifyConstant: return DaoInferencer_Error( self, DTE_CONST_WRONG_MODIFYING );
InvEnum: return DaoInferencer_Error( self, DTE_INVALID_ENUMERATION );
InvIndex: return DaoInferencer_Error( self, DTE_INDEX_NOT_VALID );
InvOper: return DaoInferencer_Error( self, DTE_OPERATION_NOT_VALID );
InvalidCasting: return DaoInferencer_Error( self, DTE_INVALID_INVAR_CAST );
InvParam: return DaoInferencer_Error( self, DTE_PARAM_ERROR );
ErrorTyping: return DaoInferencer_Error( self, DTE_TYPE_NOT_MATCHING );
}
static void DaoRoutine_ReduceLocalConsts( DaoRoutine *self )
{
	DaoList *list = DaoList_New();
	DMap *used = DMap_New(0,0);
	DNode *it;
	daoint i;
	for(i=0; i<self->routType->nested->size; ++i){
		DMap_Insert( used, IntToPointer(i), IntToPointer(i) );
	}
	for(i=0; i<self->routConsts->value->size; ++i){
		/* For reserved space in the constant list (for example, in decorators): */
		if( self->routConsts->value->items.pValue[i] == NULL ){
			DMap_Insert( used, IntToPointer(i), IntToPointer(i) );
		}
	}
	for(i=0; i<self->body->annotCodes->size; ++i){
		DaoVmCodeX *vmc = self->body->annotCodes->items.pVmc[i];
		DaoVmCode *vmc2 = self->body->vmCodes->data.codes + i;
		int id = used->size;
		switch( vmc->code ){
		case DVM_GETCL :
		case DVM_GETCL_I : case DVM_GETCL_F :
		case DVM_GETCL_C :
		case DVM_GETF : case DVM_SETF :
		case DVM_CAST :
		case DVM_CAST_I : case DVM_CAST_F :
		case DVM_CAST_C : case DVM_CAST_S : case DVM_CAST_VE :
		case DVM_CAST_VX :
			it = DMap_Find( used, IntToPointer(vmc->b) );
			if( it == NULL ) it = DMap_Insert( used, IntToPointer(vmc->b), IntToPointer(id) );
			vmc->b = vmc2->b = it->value.pInt;
			break;
		case DVM_NAMEVA :
		case DVM_CASE :
			it = DMap_Find( used, IntToPointer(vmc->a) );
			if( it == NULL ) it = DMap_Insert( used, IntToPointer(vmc->a), IntToPointer(id) );
			vmc->a = vmc2->a = it->value.pInt;
			break;
		}
	}
	DList_Resize( list->value, used->size, NULL );
	for(it=DMap_First(used); it; it=DMap_Next(used,it)){
		DaoValue **src = self->routConsts->value->items.pValue + it->key.pInt;
		DaoValue **dest = list->value->items.pValue + it->value.pInt;
		DaoValue_Copy( src[0], dest );
		DaoValue_MarkConst( dest[0] );
	}
	GC_Assign( & self->routConsts, list );
	DMap_Delete( used );
}

#define AssertInitialized( reg, ec, first, last ) { \
	if( DaoCnode_FindResult( node, IntToPointer(reg) ) < 0 ) \
		return DaoInferencer_ErrorNotInitialized( self, ec, first, last ); }

static int DaoInferencer_CheckInitialization( DaoInferencer *self, DaoOptimizer *optimizer )
{
	DaoClass *klass;
	DaoRoutine *routine = self->routine;
	DaoStream  *stream = routine->nameSpace->vmSpace->errorStream;
	DList *annotCodes = routine->body->annotCodes;
	DaoVmCodeX **codes = annotCodes->items.pVmc;
	DaoCnode **nodes;
	char char50[50];
	int i, j, J, ret = 1;

	DaoOptimizer_DoVIA( optimizer, routine );

	nodes = optimizer->nodes->items.pCnode;
	for(i=0; i<annotCodes->size; i++){
		DaoCnode *node = nodes[i];
		DaoVmCodeX *vmc = codes[i];
		int first = vmc->first;
		int middle = first + vmc->middle;
		int last = middle + vmc->last;

		self->currentIndex = i;

#if 0
		DString *mbs = self->mbstring;
		DaoLexer_AnnotateCode( routine->body->source, *vmc, mbs, 24 );
		printf( "%4i: ", i );DaoVmCodeX_Print( *vmc, mbs->chars, NULL );
#endif

		switch( DaoVmCode_GetOpcodeType( (DaoVmCode*) vmc ) ){
		case DAO_CODE_GETU :
			if( vmc->a != 0 ) AssertInitialized( vmc->b, 0, middle, middle );
			break;
		case DAO_CODE_UNARY2 :
			AssertInitialized( vmc->b, 0, middle, middle );
			break;
		case DAO_CODE_GETF :
		case DAO_CODE_BRANCH :
			AssertInitialized( vmc->a, 0, first, first );
			break;
		case DAO_CODE_SETG :
		case DAO_CODE_SETU :
			AssertInitialized( vmc->a, 0, first, first );
			break;
		case DAO_CODE_MOVE :
		case DAO_CODE_UNARY :
			AssertInitialized( vmc->a, 0, first, last );
			break;
		case DAO_CODE_SETF :
			AssertInitialized( vmc->a, 0, last+1, last+1 );
			AssertInitialized( vmc->c, 0, first, first );
			break;
		case DAO_CODE_GETI :
			AssertInitialized( vmc->a, DTE_ITEM_WRONG_ACCESS, first, first );
			AssertInitialized( vmc->b, DTE_ITEM_WRONG_ACCESS, middle, middle );
			break;
		case DAO_CODE_BINARY :
			AssertInitialized( vmc->a, 0, first, first );
			AssertInitialized( vmc->b, 0, middle, middle );
			break;
		case DAO_CODE_SETI :
			AssertInitialized( vmc->c, DTE_ITEM_WRONG_ACCESS, first, first );
			AssertInitialized( vmc->b, DTE_ITEM_WRONG_ACCESS, middle, middle );
			AssertInitialized( vmc->a, DTE_ITEM_WRONG_ACCESS, last+1, last+1 );
			break;
		case DAO_CODE_GETM :
		case DAO_CODE_ENUM2 :
		case DAO_CODE_ROUTINE :
			for(j=0; j<=vmc->b; ++j){
				AssertInitialized( vmc->a+j, 0, first, first );
			}
			break;
		case DAO_CODE_SETM :
			AssertInitialized( vmc->c, DTE_ITEM_WRONG_ACCESS, first, first );
			for(j=0; j<=vmc->b; ++j) AssertInitialized( vmc->c+j, 0, first, first );
			break;
		case DAO_CODE_MATRIX :
			J=(vmc->b>>8)*(vmc->b&0xff);
			for(j=0; j<J; ++j) AssertInitialized( vmc->a+j, 0, first, first );
			break;
		case DAO_CODE_CALL :
			for(j=0, J=vmc->b&0xff; j<=J; ++j){
				AssertInitialized( vmc->a+j, 0, middle, last );
			}
			break;
		case DAO_CODE_ENUM :
			for(j=0, J=vmc->b&(0xffff>>2); j<J; ++j){
				AssertInitialized( vmc->a+j, 0, first, first );
			}
			break;
		case DAO_CODE_EXPLIST :
			for(j=0; j<vmc->b; ++j) AssertInitialized( vmc->a+j, 0, first, first );
			break;
		case DAO_CODE_YIELD :
			for(j=0; j<(vmc->b&0xff); ++j) AssertInitialized( vmc->a+j, 0, first, first );
			break;
		}
	}

	if( !(routine->attribs & DAO_ROUT_INITOR) ) return 1;
	if( routine->attribs & DAO_ROUT_MIXIN ) return 1;  /* Alread checked; */

	klass = (DaoClass*) routine->routHost->aux;
	for(i=0; i<optimizer->nodes->size; i++){
		DaoCnode *node = nodes[i];
		DaoVmCodeX *vmc = codes[i];
		int first = vmc->first;
		int middle = first + vmc->middle;
		int last = middle + vmc->last;

		if( vmc->code != DVM_RETURN ) continue;
		for(j=klass->objParentEnd; j<klass->instvars->size; ++j){
			DaoVariable *var = klass->instvars->items.pVar[j];
			int key = (DAO_OBJECT_VARIABLE<<16) | j;
			if( var->dtype && var->dtype->tid <= DAO_STRING ) continue;
			if( var->value && DaoType_MatchValue( var->dtype, var->value, NULL ) ) continue;
			if( DaoCnode_FindResult( node, IntToPointer(key) ) < 0 ){
				sprintf( char50, "  At line %i : ", routine->defLine );
				DaoInferencer_WriteErrorHeader2( self );
				DaoStream_WriteChars( stream, char50 );
				DaoStream_WriteChars( stream, "Class instance field \"" );
				DaoStream_WriteString( stream, klass->objDataName->items.pString[j] );
				DaoStream_WriteChars( stream, "\" not initialized!\n" );
				ret = 0;
			}
		}
	}
	return ret;
}

int DaoRoutine_DoTypeInference( DaoRoutine *self, int silent )
{
	DaoInferencer *inferencer;
	DaoOptimizer *optimizer;
	DaoVmSpace *vmspace = self->nameSpace->vmSpace;
	int retc, decorator = self->attribs & DAO_ROUT_DECORATOR;
	int notide = ! (vmspace->options & DAO_OPTION_IDE);

	DaoRoutine_ReduceLocalConsts( self );

	if( self->body->vmCodes->size == 0 ) return 1;

	optimizer = DaoVmSpace_AcquireOptimizer( vmspace );
	DList_Resize( self->body->regType, self->body->regCount, NULL );
	if( ! decorator ) DaoOptimizer_RemoveUnreachableCodes( optimizer, self );

	inferencer = DaoVmSpace_AcquireInferencer( vmspace );
	DaoInferencer_Init( inferencer, self, silent );
	retc  = DaoInferencer_CheckInitialization( inferencer, optimizer );
	retc &= DaoInferencer_DoInference( inferencer );
	DaoVmSpace_ReleaseInferencer( vmspace, inferencer );

	/*
	// Do not optimize decorators now, because there are reverved
	// registers for decoration, but not used in the codes.
	// Optimization may lose those registers, and lead to error
	// during decorator application.
	*/
	if( retc && ! decorator ) DaoOptimizer_Optimize( optimizer, self );
	/* Maybe more unreachable code after inference and optimization: */
	if( ! decorator ) DaoOptimizer_RemoveUnreachableCodes( optimizer, self );

	if( retc && notide && daoConfig.jit && dao_jit.Compile ){
		/* LLVMContext provides no locking guarantees: */
		DMutex_Lock( & mutex_routine_specialize );
		dao_jit.Compile( self, optimizer );
		DMutex_Unlock( & mutex_routine_specialize );
	}

	/* DaoRoutine_PrintCode( self, self->nameSpace->vmSpace->errorStream ); */
	DaoVmSpace_ReleaseOptimizer( vmspace, optimizer );
	return retc;
}

#ifdef DAO_WITH_DECORATOR
/*
// Function decoration is done in the following way:
// 1. Use the decoration parameters to find the right decorator, if overloaded;
// 2. Use the decorator's parameter to determine the right (OLD) function, if overloaded;
// 3. The decorator function is copied to form the basis of the result (NEW) function;
// 4. Then the NEW function is adjusted to take the same parameters as the OLD function;
// 5. Arguments to the decorator are stored and accessed in the same way as local constants;
// 6. Code is added at the beginning of the NEW function to access decorator arguments;
// 7. Code is added to create a tuple (named args) from the parameters of the NEW function;
// 8. The registers from the decorate function will be mapped to higher indexes to reserve
//    indexes for the parameters of the NEW function.
*/
DaoRoutine* DaoRoutine_Decorate( DaoRoutine *self, DaoRoutine *decorator, DaoValue *p[], int n, int ip )
{
	int i, k, m;
	int code, decolen, hasself = 0;
	int parpass[DAO_MAX_PARAM];
	DList *annotCodes, *added = NULL, *regmap = NULL;
	DList *nested, *ptypes;
	DaoValue *selfpar = NULL;
	DaoObject object = {0}, *obj = & object;
	DaoType *ftype, **decotypes;
	DaoRoutine *newfn, *oldfn = self;
	DaoVmCodeX *vmc;

	/* No in place decoration of overloaded function: */
	if( self->overloads && ip ) return NULL;
	if( self->attribs & DAO_ROUT_DECORATOR ) return NULL;
	if( self->overloads ){
		DList *routs = DList_New(0);
		for(i=0; i<self->overloads->routines->size; i++){
			DaoRoutine *rout = self->overloads->routines->items.pRoutine[i];
			rout = DaoRoutine_Decorate( rout, decorator, p, n, 0 );
			if( rout ) DList_Append( routs, rout );
		}
		if( routs->size == 0 ){
			DList_Delete( routs );
			return NULL;
		}else if( routs->size == 1 ){
			newfn = routs->items.pRoutine[0];
			DList_Delete( routs );
			return newfn;
		}
		newfn = DaoRoutine_Copy( self, 0, 0, 1 );
		newfn->overloads = DRoutines_New();
		for(i=0; i<routs->size; i++) DRoutines_Add( newfn->overloads, routs->items.pRoutine[i] );
		DList_Delete( routs );
		return newfn;
	}

	if( self->routHost ){
		object.type = DAO_OBJECT;
		object.defClass = (DaoClass*) self->routHost->aux;
		selfpar = (DaoValue*) obj;
	}

	decorator = DaoRoutine_ResolveX( decorator, selfpar, NULL, p, NULL, n, 0 );
	if( decorator == NULL || decorator->type != DAO_ROUTINE ) return NULL;

	if( oldfn->attribs & DAO_ROUT_INVAR ){
		if( !(decorator->attribs & (DAO_ROUT_INVAR|DAO_ROUT_STATIC)) ) return NULL;
	}

	nested = decorator->routType->nested;
	decotypes = nested->items.pType;
	decolen = nested->size;
	if( decotypes[0]->attrib & DAO_TYPE_SELFNAMED ){
		/* Non-static decorator can only be applied to methods of the same class: */
		if( decorator->routHost != self->routHost ) return NULL;
		if( decolen == 1 ) return NULL;
		decotypes += 1;
		decolen -= 1;
		hasself = 1;
	}
	if( decolen == 0 ) return NULL;

	ftype = (DaoType*) decotypes[0]->aux;
	ptypes = ftype->nested;
	code = DVM_CALL + (ftype->attrib & DAO_TYPE_SELF);
	/* ftype->aux is NULL for type "routine": */
	if( ftype->aux ){
		DList *TS = DList_New(0);
		for(i=0; i<ptypes->size; ++i){
			DaoType *T = ptypes->items.pType[i];
			if( T->tid == DAO_PAR_NAMED || T->tid == DAO_PAR_DEFAULT ) T = (DaoType*) T->aux;
			DList_Append( TS, T );
		}
		oldfn = DaoRoutine_ResolveX( self, NULL, NULL, NULL, TS->items.pType, TS->size, code );
		DList_Delete( TS );
	}
	if( oldfn == NULL ) return NULL;

	newfn = DaoRoutine_Copy( decorator, 1, 1, 1 );
	added = DList_New( DAO_DATA_VMCODE );
	regmap = DList_New(0);

	DList_Resize( regmap, decorator->body->regCount + oldfn->parCount, 0 );
	for(i=0,m=decorator->body->regCount; i<m; i++) regmap->items.pInt[i] = i + oldfn->parCount;
	for(i=0,m=oldfn->parCount; i<m; i++) regmap->items.pInt[i + decorator->body->regCount] = i;

	DList_Resize( newfn->body->regType, newfn->body->regCount + oldfn->parCount, NULL );
	for(i=0,m=oldfn->routType->nested->size; i<m; i++){
		DaoType *T = oldfn->routType->nested->items.pType[i];
		if( T->tid == DAO_PAR_NAMED || T->tid == DAO_PAR_DEFAULT ) T = (DaoType*) T->aux;
		GC_Assign( & newfn->body->regType->items.pType[i + newfn->body->regCount], T );
		/* DList_Append( newfn->body->defLocals, oldfn->body->defLocals->items.pToken[i] ); */
	}
	newfn->body->regCount += oldfn->parCount;
	annotCodes = newfn->body->annotCodes;
	k = hasself;
	for(i=0; i<decolen; i++) parpass[i] = 0;
	for(i=0; i<n; i++){
		DaoValue *pv = p[i];
		if( i == 0 ){
			/*
			// This should be the function to be called from inside of the new function.
			// If the new function does not override the old one, the old function will
			// be called; Otherwise, the function bodies of the old and the new will be
			// swapped, and the new function will be called.
			*/
			pv = (DaoValue*)(ip ? newfn : oldfn);
		}
		if( pv->type == DAO_PAR_NAMED ) pv = pv->xNameValue.value;
		parpass[k] = 1;
		DList_PushBack( added, annotCodes->items.pVoid[0] );
		vmc = added->items.pVmc[added->size-1];
		vmc->code = DVM_GETCL;
		vmc->a = 0;
		vmc->b = DaoRoutine_AddConstant( newfn, pv );
		vmc->c = k++;
	}
	for(i=1; i<decolen; i++){
		k = decotypes[i]->tid;
		if( k == DAO_PAR_VALIST ) break;
		if( parpass[i] ) continue;
		if( k != DAO_PAR_DEFAULT ) continue;
		DList_PushBack( added, annotCodes->items.pVoid[0] );
		vmc = added->items.pVmc[added->size-1];
		vmc->code = DVM_GETCL;
		vmc->a = 0;
		vmc->b = DaoRoutine_AddConstant( newfn, decorator->routConsts->value->items.pValue[i] );
		vmc->c = i;
	}
	DList_PushBack( added, annotCodes->items.pVoid[0] ); /* XXX */
	vmc = added->items.pVmc[added->size-1];
	vmc->code = DVM_TUPLE;
	vmc->a = decorator->body->regCount;
	vmc->b = oldfn->parCount;
	vmc->c = decorator->parCount;
	if( vmc->b == 0 ) vmc->a = 0;
	for(i=0,m=annotCodes->size; i<m; i++){
		vmc = annotCodes->items.pVmc[i];
		k = DaoVmCode_GetOpcodeType( (DaoVmCode*) vmc );
		if( k == DAO_CODE_BRANCH || k == DAO_CODE_JUMP ) vmc->b += added->size;
	}
	DList_InsertList( annotCodes, 0, added, 0, added->size );
	DArray_Resize( newfn->body->vmCodes, annotCodes->size );
	for(i=0,m=annotCodes->size; i<m; i++){
		vmc = annotCodes->items.pVmc[i];
		newfn->body->vmCodes->data.codes[i] = *(DaoVmCode*) vmc;
	}

	GC_Assign( & newfn->routType, oldfn->routType );
	newfn->parCount = oldfn->parCount;
	newfn->attribs = oldfn->attribs;
	DString_Assign( newfn->routName, oldfn->routName );
	/* Decorator should have reserved spaces for up to DAO_MAX_PARAM default parameters: */
	assert( newfn->routConsts->value->size >= DAO_MAX_PARAM );
	i = oldfn->routConsts->value->size;
	m = oldfn->parCount < i ? oldfn->parCount : i;
	for(i=0; i<m; i++){
		DaoValue *value = oldfn->routConsts->value->items.pValue[i];
		if( value ) DaoValue_Copy( value, newfn->routConsts->value->items.pValue + i );
	}

	DaoRoutine_UpdateRegister( newfn, regmap );
	for(i=0,m=annotCodes->size; i<m; i++){
		vmc = annotCodes->items.pVmc[i];
		if( vmc->code == DVM_CALL && (vmc->b & DAO_CALL_DECSUB) ){
			if( vmc->b & DAO_CALL_BLOCK ){
				DaoVmCodeX *sect = annotCodes->items.pVmc[i+2];
				DaoVmCodeX *first = annotCodes->items.pVmc[i+3];
				DaoType *cbtype = oldfn->routType->cbtype;
				DaoType **cbtypes = cbtype->nested->items.pType;
				int count = cbtype->nested->size;
				if( sect->b != 0 && sect->c == 0 ){ /* [...] or [... as name]; */
					if( first->code != DVM_TUPLE || (first->b >> 14) != DVM_ENUM_MODE2 ){
						first = NULL;
					}
					if( count && cbtypes[count-1]->tid == DAO_PAR_VALIST ){
						sect->b = DAO_MAX_PARAM;
						sect->c = count - 1;
					}else{
						sect->b = sect->c = count;
					}
					if( first != NULL ){
						first->b = sect->b;
						newfn->body->vmCodes->data.codes[i+3] = *(DaoVmCode*) first;
					}
					newfn->body->vmCodes->data.codes[i+2] = *(DaoVmCode*) sect;
				}
			}
		}
	}

	for(i=0,m=annotCodes->size; i<m; i++){
		vmc = annotCodes->items.pVmc[i];
		if( vmc->code == DVM_CALL && (vmc->b & DAO_CALL_DECSUB) ){
			/*
			// Call to the decorated function was marked with DAO_CALL_NOSELF
			// by the type inferencer, to not pass an implicit self parameter.
			// But for decorating constructors, it is necessary to pass the
			// implicit self parameter, because the self parameter is not in
			// the parameter list.
			*/
			if( oldfn->attribs & DAO_ROUT_INITOR ){
				vmc->b &= ~ DAO_CALL_NOSELF; /* Allow passing implicit self; */
				vmc->b |= DAO_CALL_INIT; /* Avoid resetting DAO_CALL_NOSELF; */
			}
			vmc->b &= ~ DAO_CALL_DECSUB;
			newfn->body->vmCodes->data.codes[i] = *(DaoVmCode*) vmc;
		}
	}
	if( DaoRoutine_DoTypeInference( newfn, 0 ) ==  0 ) goto ErrorDecorator;

#if 0
	printf( "###################################\n" );
	printf( "################################### %s\n", oldfn->routName->chars );
	printf( "###################################\n" );
	DaoRoutine_PrintCode( decorator, decorator->nameSpace->vmSpace->errorStream );
	DaoRoutine_PrintCode( newfn, newfn->nameSpace->vmSpace->errorStream );
#endif

	if( ip ){
		/* For in place decoration, override the old function by swapping the function
		// bodies and other associated data: */
		DaoRoutineBody *body = oldfn->body;
		DaoNamespace *ns = oldfn->nameSpace;
		DaoList *clist = oldfn->routConsts;
		oldfn->routConsts = newfn->routConsts;
		oldfn->nameSpace = newfn->nameSpace;
		oldfn->body = newfn->body;
		newfn->routConsts = clist;
		newfn->nameSpace = ns;
		newfn->body = body;
	}
	DList_Delete( added );
	DList_Delete( regmap );

	return newfn;
ErrorDecorator:
	if( added ) DList_Delete( added );
	if( regmap ) DList_Delete( regmap );
	DaoGC_TryDelete( (DaoValue*) newfn );
	return NULL;
}
#endif
