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

#include"assert.h"
#include"string.h"
#include"daoConst.h"
#include"daoClass.h"
#include"daoObject.h"
#include"daoRoutine.h"
#include"daoProcess.h"
#include"daoOptimizer.h"
#include"daoGC.h"
#include"daoStream.h"
#include"daoNumtype.h"
#include"daoValue.h"
#include"daoNamespace.h"
#include"daoVmspace.h"

static void DaoClass_GetField( DaoValue *self0, DaoProcess *proc, DString *name )
{
	int tid = proc->activeRoutine->routHost ? proc->activeRoutine->routHost->tid : 0;
	DaoType *type = proc->activeRoutine->routHost;
	DaoClass *host = tid == DAO_OBJECT ? & type->aux->xClass : NULL;
	DaoClass *self = & self0->xClass;
	DString *mbs = DString_New();
	DaoValue *data = DaoClass_GetData( self, name, host );;
	if( data == NULL || data->type == DAO_NONE || data->xBase.subtype == DAO_OBJECT_VARIABLE ){
		int rc = data == NULL ? DAO_ERROR_FIELD_NOTEXIST : DAO_ERROR_FIELD_NOTPERMIT;
		DString_SetChars( mbs, DString_GetData( self->className ) );
		DString_AppendChars( mbs, "." );
		DString_Append( mbs, name );
		DaoProcess_RaiseException( proc, daoExceptionNames[rc], mbs->chars, NULL );
	}else{
		DaoProcess_PutValue( proc, data->xConst.value );
	}
	DString_Delete( mbs );
}
static void DaoClass_SetField( DaoValue *self0, DaoProcess *proc, DString *name, DaoValue *value )
{
	DaoClass *self = & self0->xClass;
	DNode *node = DMap_Find( self->lookupTable, name );
	if( node && LOOKUP_ST( node->value.pInt ) == DAO_CLASS_VARIABLE ){
		int up = LOOKUP_UP( node->value.pInt );
		int id = LOOKUP_ID( node->value.pInt );
		DaoVariable *dt = self->variables->items.pVar[id];
		if( DaoValue_Move( value, & dt->value, dt->dtype ) ==0 )
			DaoProcess_RaiseError( proc, "Param", "not matched" );
	}else{
		/* XXX permission */
		DaoProcess_RaiseError( proc, "Field", "not exist" );
	}
}
static void DaoClass_GetItem( DaoValue *self0, DaoProcess *proc, DaoValue *ids[], int N )
{
}
static void DaoClass_SetItem( DaoValue *self0, DaoProcess *proc, DaoValue *ids[], int N, DaoValue *value )
{
}

static DaoTypeCore classCore=
{
	NULL,
	DaoClass_GetField,
	DaoClass_SetField,
	DaoClass_GetItem,
	DaoClass_SetItem,
	DaoValue_Print
};

DaoTypeBase classTyper =
{
	"class", & classCore, NULL, NULL, {0}, {0},
	(FuncPtrDel) DaoClass_Delete, NULL
};

DaoClass* DaoClass_New()
{
	DaoClass *self = (DaoClass*) dao_calloc( 1, sizeof(DaoClass) );
	DaoValue_Init( self, DAO_CLASS );
	self->trait |= DAO_VALUE_DELAYGC;
	self->className = DString_New();

	self->lookupTable = DHash_New( DAO_DATA_STRING, 0 );
	self->methSignatures = DHash_New( DAO_DATA_STRING, 0 );
	self->constants   = DList_New( DAO_DATA_VALUE );
	self->variables   = DList_New( DAO_DATA_VALUE );
	self->instvars    = DList_New( DAO_DATA_VALUE );
	self->objDataName = DList_New( DAO_DATA_STRING );
	self->cstDataName = DList_New( DAO_DATA_STRING );
	self->glbDataName = DList_New( DAO_DATA_STRING );
	self->parent = NULL;
	self->mixinBases = DList_New(0);  /* refcount handled in ::allBases; */
	self->allBases   = DList_New( DAO_DATA_VALUE );
	self->mixins  = DList_New(0);
	self->ranges  = DArray_New(sizeof(ushort_t));
	self->references  = DList_New( DAO_DATA_VALUE );

	self->cstMixinStart = self->cstMixinEnd = self->cstMixinEnd2 = 0;
	self->glbMixinStart = self->glbMixinEnd = 0;
	self->objMixinStart = self->objMixinEnd = 0;
	self->cstParentStart = self->cstParentEnd = 0;
	self->glbParentStart = self->glbParentEnd = 0;
	self->objParentStart = self->objParentEnd = 0;
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
void DaoClass_Delete( DaoClass *self )
{
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	GC_DecRC( self->clsType );
	GC_DecRC( self->castOperators );
	DMap_Delete( self->lookupTable );
	DMap_Delete( self->methSignatures );
	DList_Delete( self->constants );
	DList_Delete( self->variables );
	DList_Delete( self->instvars );
	DList_Delete( self->objDataName );
	DList_Delete( self->cstDataName );
	DList_Delete( self->glbDataName );
	DList_Delete( self->allBases );
	DList_Delete( self->mixinBases );
	DList_Delete( self->mixins );
	DArray_Delete( self->ranges );
	DList_Delete( self->references );
	if( self->interMethods ) DMap_Delete( self->interMethods );
	if( self->decoTargets ) DList_Delete( self->decoTargets );

	DString_Delete( self->className );
	dao_free( self );
}
void DaoClass_AddReference( DaoClass *self, void *reference )
{
	if( reference == NULL ) return;
	DList_Append( self->references, reference );
}
void DaoClass_Parents( DaoClass *self, DList *parents, DList *offsets );


void DaoClass_SetName( DaoClass *self, DString *name, DaoNamespace *ns )
{
	DaoRoutine *rout;
	DString *str;

	if( self->initRoutine ) return;

	self->objType = DaoType_New( name->chars, DAO_OBJECT, (DaoValue*)self, NULL );
	self->clsType = DaoType_New( name->chars, DAO_CLASS, (DaoValue*) self, NULL );
	GC_IncRC( self->clsType );
	DString_InsertChars( self->clsType->name, "class<", 0, 0, 0 );
	DString_AppendChar( self->clsType->name, '>' );

	str = DString_New();
	DString_SetChars( str, "self" );
	DaoClass_AddObjectVar( self, str, NULL, self->objType, DAO_PERM_PRIVATE );
	DString_Assign( self->className, name );

	rout = DaoRoutine_New( ns, self->objType, 1 );
	DString_Assign( rout->routName, name );
	DString_AppendChars( rout->routName, "::" );
	DString_Append( rout->routName, name );
	self->initRoutine = rout; /* XXX class<name> */
	GC_IncRC( rout );

	rout->routType = DaoType_New( "routine<=>", DAO_ROUTINE, (DaoValue*)self->objType, NULL );
	DString_Append( rout->routType->name, name );
	DString_AppendChars( rout->routType->name, ">" );
	GC_IncRC( rout->routType );
	rout->attribs |= DAO_ROUT_INITOR;

	DaoClass_AddConst( self, name, (DaoValue*) self, DAO_PERM_PUBLIC );

	self->initRoutines = DaoRoutines_New( ns, self->objType, NULL );
	DString_Assign( self->initRoutines->routName, name );

	DaoClass_AddConst( self, rout->routName, (DaoValue*)self->initRoutines, DAO_PERM_PUBLIC );

	DString_Delete( str );
}
/* breadth-first search */
void DaoClass_Parents( DaoClass *self, DList *parents, DList *offsets )
{
	DaoValue *dbase;
	DaoClass *klass;
	DaoCdata *cdata;
	DaoTypeBase *typer;
	daoint i, j, offset;
	DList_Clear( parents );
	DList_Clear( offsets );
	DList_Append( parents, self );
	DList_Append( offsets, self->objDataName->size );
	for(i=0; i<parents->size; i++){
		dbase = parents->items.pValue[i];
		offset = offsets->items.pInt[i];
		if( dbase->type == DAO_CLASS ){
			klass = (DaoClass*) dbase;
			if( klass->parent ){
				DaoClass *cls = (DaoClass*) klass->parent;
				DList_Append( parents, klass->parent );
				DList_Append( offsets, (daoint) offset );
				offset += (cls->type == DAO_CLASS) ? cls->objDataName->size : 0;
			}
		}else if( dbase->type == DAO_CTYPE ){
			cdata = (DaoCdata*) dbase;
			typer = cdata->ctype->kernel->typer;
			for(j=0; j<DAO_MAX_CDATA_SUPER; j++){
				if( typer->supers[j] == NULL ) break;
				DList_Append( parents, typer->supers[j]->core->kernel->abtype->aux );
				DList_Append( offsets, (daoint) offset );
			}
		}
	}
}


typedef struct DaoMethodFields DaoMethodFields;

struct DaoMethodFields
{
	DList  *names;
	DList  *perms;
	DList  *routines;
};
static DaoMethodFields* DaoMethodFields_New()
{
	DaoMethodFields *self = (DaoMethodFields*) dao_malloc( sizeof(DaoMethodFields) );
	self->names = DList_New( DAO_DATA_STRING );
	self->perms = DList_New(0);
	self->routines = DList_New(0);
	return self;
}
static void DaoMethodFields_Delete( DaoMethodFields *self )
{
	DList_Delete( self->names );
	DList_Delete( self->perms );
	DList_Delete( self->routines );
	dao_free( self );
}


static DaoClass* DaoClass_FindMixin( DaoClass *mixin, int st, int id, int *offset )
{
	DaoClass *last;
	ushort_t *offsets = mixin->ranges->data.ushorts;
	int size1 = mixin->mixins->size - 1;
	int i = 0, j = size1;
	int st2 = 0;

	if( offset ) *offset = 0;

	/* return this mixin if there is no component mixins: */
	if( size1 == -1 ) return mixin;

	last = mixin->mixins->items.pClass[size1];
	switch( st ){
	case DAO_CLASS_CONSTANT  : st2 = 0; break;
	case DAO_CLASS_VARIABLE  : st2 = 1; break;
	case DAO_OBJECT_VARIABLE : st2 = 2; break;
	}

	/* return this mixin if the index is for this mixin: */
	if( id < offsets[st2] ) return mixin;
	if( id >= offsets[6*size1+3+st2] ) return mixin;

	if( offset ) *offset = offsets[st2]; /* set the offset of the component mixin; */

	/* return the component mixin if there is only one: */
	if( size1 == 0 ) return last;

	while( i <= j ){ /* binary searching: */
		int k = (i + j) / 2;
		DaoClass *mid = mixin->mixins->items.pClass[k];
		if( offset ) *offset = offsets[6*k+st2];
		if( i == j ) return mid;
		if( id < offsets[6*k+st2] ){
			j = k - 1;
		}else if( id >= offsets[6*k+3+st2] ){
			i = k + 1;
		}else{
			return mid;
		}
	}
	return mixin;
}
static int DaoClass_MapIndex( DaoClass *mixin, int st, int id, DMap *mixed )
{
	int offset = 0;
	DaoClass *mx = DaoClass_FindMixin( mixin, st, id, & offset );
	DNode *it = DMap_Find( mixed, mx );
	if( it != NULL ) it = MAP_Find( it->value.pMap, LOOKUP_BIND( st, 0, 0, id-offset ) );
	if( it != NULL ) return LOOKUP_ID( it->value.pInt );
	return id;
}
static DString* DaoClass_GetDataName( DaoClass *self, int st, int id )
{
	DNode *it;
	for(it=DMap_First(self->lookupTable); it; it=DMap_Next(self->lookupTable,it)){
		if( st == LOOKUP_ST(it->value.pInt) && id == LOOKUP_ID(it->value.pInt) ){
			return it->value.pString;
		}
	}
	return NULL;
}
static int DaoRoutine_GetFieldIndex( DaoRoutine *self, DString *name )
{
	DString none = DString_WrapChars( "" );
	DaoString str = {DAO_STRING,0,0,0,0,NULL};
	DaoString *s = & str;
	daoint i;
	if( name == NULL ) name = & none;
	for(i=0; i<self->routConsts->value->size; ++i){
		DaoValue *item = DaoList_GetItem( self->routConsts, i );
		DString *field = DaoValue_TryGetString( item );
		if( field == NULL ) continue;
		if( DString_EQ( field, name ) ) return i;
	}
	str.value = name;
	return DaoRoutine_AddConstant( self, (DaoValue*) s );
}
static void DaoRoutine_OriginalHost( void *p ){}
/*
// The layout of mixins in a host class:
// 1. Each mixin occupies a continuous segment in the data arrays of the host.
//    The ranges of the segments are stored in DaoClass::ranges;
// 2. If the mixin contains other mixins, those mixins occupy segments preceding
//    the segment for this mixin;
// 3. The segments for the direct mixins of the host are arranged in the same
//    order as the mixins;
//
// For example, there are the following mixins:
//    class AA { var x = 1 }
//    class BB { var x = 1 }
//    class CC ( AA, BB ) { var x = 1 }
//    class DD ( CC, AA ) { var x = 1 }
// The mixin layout for CC:
//    CC_Header, AA_Header, AA_Data, BB_Header, BB_Data, CC_Data
// The mixin layout for DD:
//    DD_Header, AA_Header, AA_Data, BB_Header, BB_Data, CC_Header, CC_Data, DD_Data
//
// Where XX_Header are the data fields that are always placed at the header
// of the data array. For example, XX_Header for class constants contains
// two fields: one for the class itself, the other for the class constructor(s);
// XX_Header for class static variables is empty; and XX_Header for class
// instance variables contains only the field for the "self" variable.
// And XX_Data constains the mixin's own data which are not from its
// component mixins or from its paraent classes (actually only classes
// without parent classes can be used as mixins).
//
//
// To mix a mixin in the host class, the mixin (and its component mixins if any)
// are properly arranged in the host class with layouts described above.
// The non-trivial part is the update of variable types and the methods
// that are added to the host class from the mixin. To update the types,
// the type for the mixin are all replaced with the type for the host class.
//
// The update of methods involves three major steps:
// 1. Update the routine signature type, local variable types and the static
//    variable types;
// 2. Update the lookup table of the host class for the data from the mixins,
//    which is done by mapping the indices for the mixin data arrays to the
//    indices for the host data arrays;
// 3. Update the method code (VM instructions) such that operands involving
//    class or class instance data are properly mapped from the indices for
//    the mixin data arrays to the indices for the host data arrays.
*/
static int DaoClass_MixIn( DaoClass *self, DaoClass *mixin, DMap *mixed, DaoMethodFields *mf )
{
	daoint i, j, k, id, bl = 1;
	DaoNamespace *ns = self->initRoutine->nameSpace;
	DList *routines;
	DMap *deftypes;
	DMap *routmap;
	DMap *idmap;
	DNode *it;

	if( mixin->parent != NULL ) return 0;
	if( DMap_Find( mixed, mixin ) != NULL ) return 1;

	/* Mix the component mixins first: */
	for(i=0; i<mixin->mixinBases->size; ++i){
		DaoClass *mx = mixin->mixinBases->items.pClass[i];
		bl = bl && DaoClass_MixIn( self, mx, mixed, mf );
	}
	if( bl == 0 ) return 0;

	idmap = DMap_New(0,0);
	routmap = DMap_New(0,0);
	deftypes = DMap_New(0,0);
	routines = DList_New(0);
	DMap_Insert( mixed, mixin, idmap );
	DMap_Delete( idmap );
	idmap = DMap_Find( mixed, mixin )->value.pMap;

	/* Add this mixin to the mixin list for both direct and indirect mixins: */
	DList_Append( self->mixins, mixin );
	/* Save the starts of the ranges for this mixin in the host class: */
	DArray_PushUshort( self->ranges, self->constants->size );
	DArray_PushUshort( self->ranges, self->variables->size );
	DArray_PushUshort( self->ranges, self->instvars->size );

	/* For updating the types for the mixin to the types for the host class: */
	DMap_Insert( deftypes, mixin->clsType, self->clsType );
	DMap_Insert( deftypes, mixin->objType, self->objType );

#if 0
	printf( "MixIn: %s %p %i\n", mixin->className->chars, mixin, mixin->cstDataName->size );
#endif

	/* Add the own constants of the mixin to the host class: */
	for(i=0; i<mixin->cstDataName->size; ++i){
		daoint src = LOOKUP_BIND( DAO_CLASS_CONSTANT, 0, 0, i );
		daoint des = LOOKUP_BIND( DAO_CLASS_CONSTANT, 0, 0, self->constants->size );
		DString *name = mixin->cstDataName->items.pString[i];
		DaoValue *value = mixin->constants->items.pConst[i]->value;
		DaoRoutine *rout = (DaoRoutine*) value;

		if( i >= mixin->cstMixinStart && i < mixin->cstMixinEnd2 ) continue;

		MAP_Insert( idmap, src, des );  /* Setup index mapping; */
		DList_Append( self->cstDataName, (void*) name );
		if( value->type != DAO_ROUTINE || rout->routHost != mixin->objType ){
			DaoConstant *cst = DaoConstant_New( value, DAO_CLASS_CONSTANT );
			DList_Append( self->constants, cst );
			continue;
		}
		if( rout->overloads == NULL ){
			DaoRoutine *old = rout;
			DNode *it = DMap_Find( old->body->aux, DaoRoutine_OriginalHost );
			void *original2 = it ? it->value.pVoid : old->routHost;
			rout = DaoRoutine_Copy( rout, 1, 1, 1 );
			rout->attribs |= DAO_ROUT_MIXIN;
			DMap_Insert( rout->body->aux, DaoRoutine_OriginalHost, original2 );
			bl = bl && DaoRoutine_Finalize( rout, old, self->objType, deftypes );
#if 0
			printf( "%2i:  %s  %s\n", i, rout->routName->chars, rout->routType->name->chars );
#endif

			/*
			// Do not use DaoClass_AddConst() here, so that the original
			// method overloading structures will be mantained, without
			// interference from methods of other mixin component classes
			// or of the host class.
			*/
			it = DMap_Find( routmap, old );
			if( it ) DRoutines_Add( it->value.pRoutine->overloads, rout );
			DList_Append( self->constants, DaoConstant_New( (DaoValue*) rout, DAO_CLASS_CONSTANT ) );
			DList_Append( routines, rout );
			if( bl == 0 ) goto Finalize;
		}else{
			/* No need to added the overloaded routines now; */
			/* Each of them has an entry in constants, and will be handled later: */
			DaoRoutine *routs = DaoRoutines_New( ns, self->objType, NULL );
			routs->trait |= DAO_VALUE_CONST;
			DList_Append( self->constants, DaoConstant_New( (DaoValue*) routs, DAO_CLASS_CONSTANT ) );
			for(j=0; j<rout->overloads->routines->size; ++j){
				DaoRoutine *R = rout->overloads->routines->items.pRoutine[j];
				DMap_Insert( routmap, R, routs );
			}
		}
	}
	for(i=mixin->glbMixinEnd; i<mixin->glbDataName->size; ++i){
		daoint src = LOOKUP_BIND( DAO_CLASS_VARIABLE, 0, 0, i );
		daoint des = LOOKUP_BIND( DAO_CLASS_VARIABLE, 0, 0, self->variables->size );
		DString *name = mixin->glbDataName->items.pString[i];
		DaoValue *var = mixin->variables->items.pVar[i]->value;
		DaoType *type = mixin->variables->items.pVar[i]->dtype;

		type = DaoType_DefineTypes( type, ns, deftypes );

		MAP_Insert( idmap, src, des );
		DList_Append( self->glbDataName, (void*) name );
		DList_Append( self->variables, DaoVariable_New( var, type, DAO_CLASS_VARIABLE ) );
	}
	for(i=mixin->objMixinEnd; i<mixin->objDataName->size; ++i){
		daoint src = LOOKUP_BIND( DAO_OBJECT_VARIABLE, 0, 0, i );
		daoint des = LOOKUP_BIND( DAO_OBJECT_VARIABLE, 0, 0, self->instvars->size );
		DString *name = mixin->objDataName->items.pString[i];
		DaoValue *var = mixin->instvars->items.pVar[i]->value;
		DaoType *type = mixin->instvars->items.pVar[i]->dtype;

		type = DaoType_DefineTypes( type, ns, deftypes );

		MAP_Insert( idmap, src, des );
		DList_Append( self->objDataName, (void*) name );
		DList_Append( self->instvars, DaoVariable_New( var, type, DAO_OBJECT_VARIABLE ) );
	}

	/* Find the ends of own data of this mixin: */
	DArray_PushUshort( self->ranges, self->constants->size );
	DArray_PushUshort( self->ranges, self->variables->size );
	DArray_PushUshort( self->ranges, self->instvars->size );

	/* Update the lookup table: */
	for(it=DMap_First(mixin->lookupTable); it; it=DMap_Next(mixin->lookupTable,it)){
		int pm = LOOKUP_PM( it->value.pInt );
		int st = LOOKUP_ST( it->value.pInt );
		int up = LOOKUP_UP( it->value.pInt );
		int id = LOOKUP_ID( it->value.pInt );
		DaoValue *cst;
		/* Skip names from component mixins (because they have been handled): */
		switch( st ){
		case DAO_CLASS_CONSTANT :
			if( id >= mixin->cstMixinStart && id < mixin->cstMixinEnd2 ) continue;
			break;
		case DAO_CLASS_VARIABLE :
			if( id >= mixin->glbMixinStart && id < mixin->glbMixinEnd ) continue;
			break;
		case DAO_OBJECT_VARIABLE :
			if( id >= mixin->objMixinStart && id < mixin->objMixinEnd ) continue;
			break;
		}
		if( st != DAO_OBJECT_VARIABLE || id != 0 ){ /* not a "self": */
			DNode *it2 = MAP_Find( idmap, LOOKUP_BIND( st, 0, 0, id ) );
			if( it2 ) id = LOOKUP_ID( it2->value.pInt ); /* map index; */
		}
		MAP_Insert( self->lookupTable, it->key.pString, LOOKUP_BIND( st, pm, up+1, id ) );
		if( st != DAO_CLASS_CONSTANT ) continue;
		cst = self->constants->items.pConst[id]->value;
		if( cst->type != DAO_ROUTINE ) continue;
		DList_Append( mf->names, it->key.pString );
		DList_Append( mf->perms, IntToPointer( pm ) );
		DList_Append( mf->routines, cst );
	}

	for(i=0; i<routines->size; i++){
		DaoRoutine *rout = routines->items.pRoutine[i];
		DaoType **types;
		if( rout->body == NULL ) continue;
		//DaoRoutine_PrintCode( rout, rout->nameSpace->vmSpace->stdioStream );
		types = rout->body->regType->items.pType;
		for(j=0; j<rout->body->annotCodes->size; ++j){
			DaoVmCodeX *vmc = rout->body->annotCodes->items.pVmc[j];
			DaoClass *klass;
			DString *name;
			switch( vmc->code ){
			case DVM_GETCK:
			case DVM_GETCK_I: case DVM_GETCK_F:
			case DVM_GETCK_C:
				vmc->b = DaoClass_MapIndex( mixin, DAO_CLASS_CONSTANT, vmc->b, mixed );
				break;
			case DVM_GETVK:
			case DVM_GETVK_I: case DVM_GETVK_F:
			case DVM_GETVK_C:
			case DVM_SETVK:
			case DVM_SETVK_II: case DVM_SETVK_FF:
			case DVM_SETVK_CC:
				vmc->b = DaoClass_MapIndex( mixin, DAO_CLASS_VARIABLE, vmc->b, mixed );
				break;
			case DVM_GETVO:
			case DVM_GETVO_I: case DVM_GETVO_F:
			case DVM_GETVO_C:
			case DVM_SETVO:
			case DVM_SETVO_II: case DVM_SETVO_FF:
			case DVM_SETVO_CC:
				vmc->b = DaoClass_MapIndex( mixin, DAO_OBJECT_VARIABLE, vmc->b, mixed );
				break;
			case DVM_GETF_KC:
			case DVM_GETF_KCI: case DVM_GETF_KCF:
			case DVM_GETF_KCC:
			case DVM_GETF_OC:
			case DVM_GETF_OCI: case DVM_GETF_OCF:
			case DVM_GETF_OCC:
				klass = (DaoClass*) types[ vmc->a ]->aux;
				name  = DaoClass_GetDataName( klass, DAO_CLASS_CONSTANT, vmc->b );
				vmc->b = DaoRoutine_GetFieldIndex( rout, name );
				vmc->code = DVM_GETF;
				break;
			case DVM_GETF_KG:
			case DVM_GETF_KGI: case DVM_GETF_KGF:
			case DVM_GETF_KGC:
			case DVM_GETF_OG:
			case DVM_GETF_OGI: case DVM_GETF_OGF:
			case DVM_GETF_OGC:
				klass = (DaoClass*) types[ vmc->a ]->aux;
				name  = DaoClass_GetDataName( klass, DAO_CLASS_VARIABLE, vmc->b );
				vmc->b = DaoRoutine_GetFieldIndex( rout, name );
				vmc->code = DVM_GETF;
				break;
			case DVM_GETF_OV:
			case DVM_GETF_OVI: case DVM_GETF_OVF:
			case DVM_GETF_OVC:
				klass = (DaoClass*) types[ vmc->a ]->aux;
				name  = DaoClass_GetDataName( klass, DAO_OBJECT_VARIABLE, vmc->b );
				vmc->b = DaoRoutine_GetFieldIndex( rout, name );
				vmc->code = DVM_GETF;
				break;
			case DVM_SETF_KG:
			case DVM_SETF_KGII: case DVM_SETF_KGFF:
			case DVM_SETF_KGCC:
			case DVM_SETF_OG:
			case DVM_SETF_OGII: case DVM_SETF_OGFF:
			case DVM_SETF_OGCC:
				klass = (DaoClass*) types[ vmc->c ]->aux;
				name  = DaoClass_GetDataName( klass, DAO_CLASS_VARIABLE, vmc->b );
				vmc->b = DaoRoutine_GetFieldIndex( rout, name );
				vmc->code = DVM_SETF;
				break;
			case DVM_SETF_OV:
			case DVM_SETF_OVII: case DVM_SETF_OVFF:
			case DVM_SETF_OVCC:
				klass = (DaoClass*) types[ vmc->c ]->aux;
				name  = DaoClass_GetDataName( klass, DAO_OBJECT_VARIABLE, vmc->b );
				vmc->b = DaoRoutine_GetFieldIndex( rout, name );
				vmc->code = DVM_SETF;
				break;
			}
		}
		//DaoRoutine_PrintCode( rout, rout->nameSpace->vmSpace->stdioStream );
		bl = bl && DaoRoutine_DoTypeInference( rout, 0 );
		if( bl == 0 ) goto Finalize;
	}
Finalize:
	DList_Delete( routines );
	DMap_Delete( routmap );
	DMap_Delete( deftypes );
	return bl;
}
static void* DaoRoutine_GetOriginal2( DaoRoutine *self )
{
	DNode *it;
	if( self->body == NULL ) return self->routHost;
	it = DMap_Find( self->body->aux, DaoRoutine_OriginalHost );
	if( it ) return it->value.pVoid;
	return self->routHost;
}
static void DaoClass_UpdateConstructor( DaoClass *self, DaoRoutine *routine, DMap *updated )
{
	DList *values;
	int i, modified = 0;
	if( routine->overloads ){
		for(i=0; i<routine->overloads->routines->size; ++i){
			DaoRoutine *rout = routine->overloads->routines->items.pRoutine[i];
			DaoClass_UpdateConstructor( self, rout, updated );
		}
		return;
	}
	if( !(routine->attribs & DAO_ROUT_INITOR) ) return;
	if( routine->routHost != self->objType ) return;
	if( routine->body == NULL ) return;
	if( DMap_Find( updated, DaoRoutine_GetOriginal2( routine ) ) != NULL ) return;

	DMap_Insert( updated, DaoRoutine_GetOriginal2( routine ), NULL );

	values = DList_New(0);
	DList_Resize( values, routine->body->regCount, NULL );
	for(i=0; i<routine->body->annotCodes->size; ++i){
		DaoVmCodeX *vmc = routine->body->annotCodes->items.pVmc[i];
		if( vmc->code == DVM_GETCK ){
			values->items.pValue[vmc->c] = self->constants->items.pConst[ vmc->b ]->value;;
		}else if( vmc->code == DVM_CALL && (vmc->b & DAO_CALL_INIT) ){
			DaoRoutine *callee = values->items.pRoutine[ vmc->a ];
			if( callee->overloads ){
				if( callee->overloads->routines->size == 0 ) continue;
				callee = callee->overloads->routines->items.pRoutine[0];
			}
			if( DMap_Find( updated, DaoRoutine_GetOriginal2( callee ) ) != NULL ){
				vmc->code = DVM_UNUSED;
				modified = 1;
				continue;
			}
			DaoClass_UpdateConstructor( self, callee, updated );
		}
	}
	DList_Delete( values );
	if( modified ) DaoRoutine_DoTypeInference( routine, 1 );
}
void DaoClass_UpdateMixinConstructors( DaoClass *self )
{
	DMap *updated;
	daoint i;
	if( self->mixins->size == 0 ) return;
	updated = DMap_New(0,0);
	for(i=0; i<self->constants->size; ++i){
		DaoValue *cst = self->constants->items.pConst[i]->value;
		if( cst == NULL || cst->type != DAO_ROUTINE ) continue;
		DaoClass_UpdateConstructor( self, (DaoRoutine*) cst, updated );
	}
	DMap_Delete( updated );
}
static void DaoClass_SetupMethodFields( DaoClass *self, DaoMethodFields *mf )
{
	DaoValue *cst;
	DMap *overloads = DMap_New( DAO_DATA_STRING, 0 );
	DNode *it, *search;
	daoint i, id, pm, pm2;

	for(i=0; i<mf->names->size; ++i){
		DString *name = mf->names->items.pString[i];
		it = DMap_Find( self->lookupTable, name );
		if( it == NULL ) continue;
		if( LOOKUP_ST( it->value.pInt ) != DAO_CLASS_CONSTANT ) continue;

		id = LOOKUP_ID( it->value.pInt );
		DMap_Insert( overloads, name, self->constants->items.pConst[id]->value );
	}

	for(i=0; i<mf->names->size; ++i){
		DString *name = mf->names->items.pString[i];
		it = DMap_Find( self->lookupTable, name );
		if( it == NULL ) continue;
		if( LOOKUP_ST( it->value.pInt ) != DAO_CLASS_CONSTANT ) continue;

		cst = mf->routines->items.pValue[i];
		search = DMap_Find( overloads, name );
		if( cst == search->value.pValue ) continue;

		pm = LOOKUP_PM( it->value.pInt );
		pm2 = mf->perms->items.pInt[i];
		if( pm2 > pm ) pm = pm2;
		/*
		// Add again the overloaded methods, so that a new overloading structure
		// will be created for the host class. This is necessary to avoid messing
		// the function calls in the methods of the mixins.
		*/
		DaoClass_AddConst( self, name, cst, pm );
	}
	DMap_Delete( overloads );
}
int DaoCass_DeriveMixinData( DaoClass *self )
{
	DMap *mixed = DMap_New(0,DAO_DATA_MAP);
	DaoMethodFields *mf = DaoMethodFields_New();
	DNode *it, *search;
	daoint i, bl = 1;

	self->cstMixinStart = self->constants->size;
	self->glbMixinStart = self->variables->size;
	self->objMixinStart = self->instvars->size;

	for(i=0; i<self->mixinBases->size; ++i){
		DaoClass *mixin = self->mixinBases->items.pClass[i];
		bl &= DaoClass_MixIn( self, mixin, mixed, mf );
	}
	self->cstMixinEnd = self->constants->size;
	self->glbMixinEnd = self->variables->size;
	self->objMixinEnd = self->instvars->size;

	DaoClass_SetupMethodFields( self, mf );

	self->cstMixinEnd2 = self->constants->size;

	DMap_Delete( mixed );
	DaoMethodFields_Delete( mf );
	return bl;
}

void DaoClass_CastingMethod( DaoClass *self, DaoRoutine *routine )
{
	DaoNamespace *NS = self->initRoutine->nameSpace;
	if( self->castOperators == NULL ){
		self->castOperators = DaoRoutines_New( NS, self->objType, NULL );
		GC_IncRC( self->castOperators );
	}
	DaoRoutines_Add( self->castOperators, routine );
}

/* assumed to be called before parsing class body */
int DaoClass_DeriveClassData( DaoClass *self )
{
	DaoType *type;
	DaoValue *value;
	DString *mbs;
	DNode *it, *search;
	DaoMethodFields *mf;
	daoint i, j, k, id;

	if( DaoCass_DeriveMixinData( self ) == 0 ) return 0;

	mbs = DString_New();
	mf = DaoMethodFields_New();

	if( self->clsType->bases == NULL ) self->clsType->bases = DList_New( DAO_DATA_VALUE );
	if( self->objType->bases == NULL ) self->objType->bases = DList_New( DAO_DATA_VALUE );
	DList_Clear( self->clsType->bases );
	DList_Clear( self->objType->bases );

	self->cstParentStart = self->constants->size;
	self->glbParentStart = self->variables->size;

	if( self->parent && self->parent->type == DAO_CLASS ){
		DaoClass *klass = (DaoClass*) self->parent;
		DList_Append( self->clsType->bases, klass->clsType );
		DList_Append( self->objType->bases, klass->objType );
		DList_AppendList( self->cstDataName, klass->cstDataName );
		DList_AppendList( self->glbDataName, klass->glbDataName );
		for(j=0; j<klass->constants->size; ++j){
			DaoValue *cst = klass->constants->items.pConst[j]->value;
			DList_Append( self->constants, klass->constants->items.pVoid[j] );
		}
		for(j=0; j<klass->variables->size; ++j){
			DList_Append( self->variables, klass->variables->items.pVoid[j] );
		}
		for(it=DMap_First(klass->lookupTable); it; it=DMap_Next(klass->lookupTable,it)){
			daoint pm = LOOKUP_PM( it->value.pInt );
			daoint st = LOOKUP_ST( it->value.pInt );
			daoint up = LOOKUP_UP( it->value.pInt );
			daoint id = LOOKUP_ID( it->value.pInt );
			DaoValue *cst;
			if( st == DAO_CLASS_CONSTANT ){
				id = LOOKUP_ID( it->value.pInt );
				cst = klass->constants->items.pConst[id]->value;
				if( cst->type == DAO_ROUTINE ){
					DList_Append( mf->names, it->key.pString );
					DList_Append( mf->perms, IntToPointer( pm ) );
					DList_Append( mf->routines, cst );
				}
			}
			if( st == DAO_OBJECT_VARIABLE ) continue;
			if( pm == DAO_PERM_PRIVATE ) continue;
			if( DMap_Find( self->lookupTable, it->key.pString ) ) continue;
			switch( st ){
			case DAO_CLASS_CONSTANT : id += self->cstParentStart; break;
			case DAO_CLASS_VARIABLE : id += self->glbParentStart; break;
			case DAO_OBJECT_VARIABLE : continue;
			}
			id = LOOKUP_BIND( st, pm, up+1, id );
			DMap_Insert( self->lookupTable, it->key.pString, (void*)id );
		}
		if( klass->castOperators ) DaoClass_CastingMethod( self, klass->castOperators );
	}else if( self->parent && self->parent->type == DAO_CTYPE ){
		DaoCtype *cdata = (DaoCtype*) self->parent;
		DaoTypeKernel *kernel = cdata->ctype->kernel;
		DaoTypeBase *typer = kernel->typer;
		DMap *methods = kernel->methods;
		DMap *values = kernel->values;

		DList_Append( self->clsType->bases, cdata->ctype );
		DList_Append( self->objType->bases, cdata->cdtype );
		DaoClass_AddConst( self, cdata->ctype->name, (DaoValue*)cdata, DAO_PERM_PUBLIC );

		if( kernel->SetupValues ) kernel->SetupValues( kernel->nspace, kernel->typer );
		if( kernel->SetupMethods ) kernel->SetupMethods( kernel->nspace, kernel->typer );

		DaoType_SpecializeMethods( cdata->ctype );
		kernel = cdata->ctype->kernel;
		values = kernel->values;
		methods = kernel->methods;

		for(it=DMap_First(values); it; it=DMap_Next(values, it)){
			if( DMap_Find( self->lookupTable, it->key.pString ) ) continue;
			id = self->constants->size;
			id = LOOKUP_BIND( DAO_CLASS_CONSTANT, DAO_PERM_PUBLIC, 1, id );
			DMap_Insert( self->lookupTable, it->key.pString, IntToPointer( id ) );
			DList_Append( self->cstDataName, it->key.pString );
			DList_Append( self->constants, DaoConstant_New( it->value.pValue, DAO_CLASS_CONSTANT ) );
		}
		for(it=DMap_First( methods ); it; it=DMap_Next( methods, it )){
			if( DMap_Find( self->lookupTable, it->key.pString ) ) continue;
			id = self->constants->size;
			id = LOOKUP_BIND( DAO_CLASS_CONSTANT, DAO_PERM_PUBLIC, 1, id );
			DMap_Insert( self->lookupTable, it->key.pString, IntToPointer( id ) );
			DList_Append( self->cstDataName, it->key.pString );
			DList_Append( self->constants, DaoConstant_New( it->value.pValue, DAO_CLASS_CONSTANT ) );

			DList_Append( mf->names, it->key.pString );
			DList_Append( mf->perms, IntToPointer( DAO_PERM_PUBLIC ) );
			DList_Append( mf->routines, it->value.pValue );
		}
		if( kernel->castOperators ) DaoClass_CastingMethod( self, kernel->castOperators );
	}
	DaoClass_SetupMethodFields( self, mf );
	DaoMethodFields_Delete( mf );

	self->cstParentEnd = self->constants->size;
	self->glbParentEnd = self->variables->size;

#if 0
	for(j=0; j<self->constants->size; ++j){
		DaoValue *value = self->constants->items.pConst[j]->value;
		DaoRoutine *routine = (DaoRoutine*) value;
		printf( "%3i: %3i %s\n", j, value->type, self->cstDataName->items.pString[j]->chars );
		if( value->type != DAO_ROUTINE ) continue;
		printf( "%3i: %3i %s\n", j, value->type, routine->routName->chars );
		if( routine->overloads ){
			DList *routs = routine->overloads->routines;
			for(k=0; k<routs->size; ++k){
				DaoRoutine *rout = routs->items.pRoutine[k];
			}
		}else{
			if( routine->attribs & DAO_ROUT_PRIVATE ) continue;
		}
	}
	for(it=DMap_First(self->lookupTable); it; it=DMap_Next(self->lookupTable,it)){
		printf( "%s %i\n", it->key.pString->chars, it->value.pInt );
		if( LOOKUP_ST( it->value.pInt ) != DAO_CLASS_CONSTANT ) continue;
		DaoValue *value = DaoClass_GetConst( self, it->value.pInt );
		printf( "%i\n", value->type );
	}
#endif

	DString_Delete( mbs );
	return 1;
}
void DaoClass_DeriveObjectData( DaoClass *self )
{
	DaoType *type;
	DaoValue *value;
	DList *parents, *offsets;
	DString *mbs;
	DNode *search;
	daoint i, id, perm, index, offset = 0;

	self->objParentStart = self->instvars->size;
	self->objDefCount = self->objDataName->size;
	offset = self->objDataName->size;

	mbs = DString_New();
	parents = DList_New(0);
	offsets = DList_New(0);
	DaoClass_Parents( self, parents, offsets );
	if( self->parent && self->parent->type == DAO_CLASS ){
		DaoClass *klass = (DaoClass*) self->parent;
		/* for properly arrangement object data: */
		for( id=0; id<klass->objDataName->size; id ++ ){
			DString *name = klass->objDataName->items.pString[id];
			DaoVariable *var = klass->instvars->items.pVar[id];
			var = DaoVariable_New( var->value, var->dtype, DAO_OBJECT_VARIABLE );
			DList_Append( self->objDataName, name );
			DList_Append( self->instvars, var );
			DaoValue_MarkConst( (DaoValue*) var->value );
		}
	}
	for(i=1; i<parents->size; i++){
		DaoClass *klass = parents->items.pClass[i];
		offset = offsets->items.pInt[i]; /* plus self */
		if( klass->type == DAO_CLASS ){
			/* For object data: */
			for( id=0; id<klass->objDataName->size; id ++ ){
				DString *name = klass->objDataName->items.pString[id];
				DNode *search2, *search = MAP_Find( klass->lookupTable, name );
				int perm = LOOKUP_PM( search->value.pInt );
				int idx = LOOKUP_ID( search->value.pInt );
				/* NO deriving private member: */
				if( perm <= DAO_PERM_PRIVATE ) continue;
				search2 = MAP_Find( self->lookupTable, name );
				if( search2 == NULL ){ /* To not overide data and routine: */
					index = LOOKUP_BIND( DAO_OBJECT_VARIABLE, perm, i, (offset+idx) );
					MAP_Insert( self->lookupTable, name, index );
				}
			}
		}
	}
	self->objParentEnd = self->instvars->size;

	self->derived = 1;
	DString_Delete( mbs );
	DList_Delete( parents );
	DList_Delete( offsets );
}
int DList_MatchAffix( DList *self, DString *name )
{
	daoint i, pos;
	if( self == NULL ) return 0;
	for(i=0; i<self->size; ++i){
		DString tmp, *pat = self->items.pString[i];
		daoint pos = DString_FindChar( pat, '~', 0 );
		if( pos < 0 ){
			if( DString_EQ( pat, name ) ) return 1;
			continue;
		}
		if( pos ){
			tmp = *pat;
			tmp.size = pos;
			if( DString_Find( name, & tmp, 0 ) != 0 ) continue;
		}
		if( pos < pat->size-1 ){
			tmp = DString_WrapChars( pat->chars + pos + 1 );
			if( DString_RFind( name, & tmp, -1 ) != (name->size - 1) ) continue;
		}
		return 1;
	}
	return 0;
}
int DaoClass_UseMixinDecorators( DaoClass *self )
{
	int bl = 1;
#ifdef DAO_WITH_DECORATOR
	daoint i, j, k;
	DaoObject object = {0};
	DaoObject *obj = & object;

	object.type = DAO_OBJECT;
	object.defClass = self;

	/*
	// Apply the decorators from mixins only to the methods defined in this class.
	// Two reasons for doing this:
	// 1. Mixins are only presented once in the current class, so when the mixins
	//    are composed of mixins, they are flatten in the current class.
	//    The order in which they are arranged in the current class is not obvious,
	//    if the decorators are allowed to decorate the methods from mixins, the
	//    result may be quite confusing;
	// 2. If the methods from mixins are allowed to be decorated, such decoration
	//    will not be written to bytecode file. Because when a class is written
	//    to a bytecode file, only its own data are encoded and saved (this is
	//    necessary to properly handle module loading). As a result, when a class
	//    is loaded from a bytecode file, it will obtain an un-decorated version
	//    of the methods from the mixins.
	*/
	for(j=self->cstMixinEnd-1; j>=self->cstMixinStart; --j){
		DaoRoutine *deco = (DaoRoutine*) self->constants->items.pConst[j]->value;
		DString *decoName = deco->routName;

		if( deco->type != DAO_ROUTINE || deco->body == NULL ) continue;
		if( !(deco->attribs & DAO_ROUT_DECORATOR) ) continue; /* Not a decorator; */
		if( deco->body->decoTargets == NULL || deco->body->decoTargets->size == 0 ) continue;

		for(k=self->cstParentEnd; k<self->constants->size; ++k){
			DaoValue *cst = self->constants->items.pConst[k]->value;
			DaoRoutine *rout = (DaoRoutine*) cst;
			DaoRoutine *deco2;

			if( rout->type != DAO_ROUTINE || rout->body == NULL ) continue;
			if( rout->attribs & (DAO_ROUT_CODESECT|DAO_ROUT_DECORATOR) ) continue;
			if( rout->routHost != self->objType ) continue;

			deco2 = DaoRoutine_ResolveX( deco, (DaoValue*) obj, NULL, & cst, NULL, 1, 0 );
			if( deco2 == NULL ) continue;
			if( DList_MatchAffix( deco2->body->decoTargets, rout->routName ) == 0 ) continue;
			bl = bl && DaoRoutine_Decorate( rout, deco2, & cst, 1, 1 ) != NULL;
			if( bl == 0 ) break;
		}
		if( bl == 0 ) break;
	}
#endif
	return bl;
}
void DaoClass_UpdateAttributes( DaoClass *self )
{
	DNode *node;
	int i, k, id, autoinitor = self->parent == NULL;

	for(i=0; autoinitor && (i<self->initRoutines->overloads->routines->size); i++){
		DaoRoutine *rout = self->initRoutines->overloads->routines->items.pRoutine[i];
		if( rout == self->initRoutine ) continue;
		if( !(rout->attribs & DAO_ROUT_INITOR) ) continue;
		if( rout->routHost != self->objType ) continue;
		autoinitor = 0;
	}
	if( autoinitor ) self->attribs |= DAO_CLS_AUTO_INITOR;
	self->intOperators = DaoClass_FindMethod( self, "(int)", NULL );
	self->eqOperators = DaoClass_FindMethod( self, "==", NULL );
	self->ltOperators = DaoClass_FindMethod( self, "<", NULL );
#if 0
	printf( "%s %i\n", self->className->chars, autoinitor );
#endif
}

int DaoClass_ChildOf( DaoClass *self, DaoValue *other )
{
	if( other->type == DAO_CLASS ){
		return DaoType_ChildOf( self->clsType, other->xClass.clsType );
	}else if( other->type == DAO_CTYPE ){
		return DaoType_ChildOf( self->clsType, other->xCtype.ctype );
	}
	return 0;
}
DaoValue* DaoClass_CastToBase( DaoClass *self, DaoType *parent )
{
	DaoValue *sup;
	if( parent == NULL ) return NULL;
	if( self->clsType == parent ) return (DaoValue*) self;
	if( self->parent == NULL ) return NULL;
	if( self->parent->type == DAO_CLASS ){
		if( (sup = DaoClass_CastToBase( (DaoClass*) self->parent, parent ) ) ) return sup;
	}else if( self->parent->type == DAO_CTYPE && parent->tid == DAO_CTYPE ){
		if( (sup = DaoType_CastToParent( self->parent, parent ) ) ) return sup;
	}
	return NULL;
}
void DaoClass_AddMixinClass( DaoClass *self, DaoClass *mixin )
{
	DList_Append( self->allBases, mixin );
	DList_Append( self->mixinBases, mixin );
}
void DaoClass_AddSuperClass( DaoClass *self, DaoValue *super )
{
	self->parent = super;
	DList_Append( self->allBases, super );
}
int  DaoClass_FindConst( DaoClass *self, DString *name )
{
	DNode *node = MAP_Find( self->lookupTable, name );
	if( node == NULL || LOOKUP_ST( node->value.pInt ) != DAO_CLASS_CONSTANT ) return -1;
	return node->value.pInt;
}
DaoValue* DaoClass_GetConst( DaoClass *self, int id )
{
	id = LOOKUP_ID( id );
	if( id >= self->constants->size ) return NULL;
	return self->constants->items.pConst[id]->value;
}
void DaoClass_SetConst( DaoClass *self, int id, DaoValue *data )
{
	id = LOOKUP_ID( id );
	if( id >= self->constants->size ) return;
	DaoValue_Copy( data, & self->constants->items.pConst[id]->value );
	DaoValue_MarkConst( self->constants->items.pConst[id]->value );
}
DaoValue* DaoClass_GetData( DaoClass *self, DString *name, DaoClass *thisClass )
{
	DaoValue *data = NULL;
	DNode *node = MAP_Find( self->lookupTable, name );
	int st, pm, up, id, child;

	if( ! node ) return NULL;

	st = LOOKUP_ST( node->value.pInt );
	id = LOOKUP_ID( node->value.pInt );
	up = LOOKUP_UP( node->value.pInt );
	pm = LOOKUP_PM( node->value.pInt );
	child = thisClass && DaoClass_ChildOf( thisClass, (DaoValue*)self );
	if( self == thisClass || pm == DAO_PERM_PUBLIC || (child && pm >= DAO_PERM_PROTECTED) ){
		switch( st ){
		case DAO_CLASS_CONSTANT  : return self->constants->items.pValue[id];
		case DAO_CLASS_VARIABLE  : return self->variables->items.pValue[id];
		case DAO_OBJECT_VARIABLE : return self->instvars->items.pValue[id];
		default : break;
		}
	}
	return dao_none_value;
}
int DaoClass_GetDataIndex( DaoClass *self, DString *name )
{
	DNode *node = MAP_Find( self->lookupTable, name );
	if( ! node ) return -1;
	return node->value.pInt;
}
int DaoClass_AddObjectVar( DaoClass *self, DString *name, DaoValue *deft, DaoType *t, int s )
{
	int id;
	DNode *node = MAP_Find( self->lookupTable, name );
	if( node && LOOKUP_UP( node->value.pInt ) == 0 ) return -DAO_CTW_WAS_DEFINED;

	id = self->objDataName->size;
	if( id != 0 ){ /* not self; */
		if( s == DAO_PERM_PRIVATE   ) self->attribs |= DAO_CLS_PRIVATE_VAR;
		if( s == DAO_PERM_PROTECTED ) self->attribs |= DAO_CLS_PROTECTED_VAR;
	}
	MAP_Insert( self->lookupTable, name, LOOKUP_BIND( DAO_OBJECT_VARIABLE, s, 0, id ) );
	DList_Append( self->objDataName, (void*)name );
	DList_Append( self->instvars, DaoVariable_New( deft, t, DAO_OBJECT_VARIABLE ) );
	DaoValue_MarkConst( self->instvars->items.pVar[ id ]->value );
	return LOOKUP_BIND( DAO_OBJECT_VARIABLE, s, 0, id );
}
static void DaoClass_AddConst3( DaoClass *self, DString *name, DaoValue *data )
{
	DaoConstant *cst = DaoConstant_New( data, DAO_CLASS_CONSTANT );
	DList_Append( self->cstDataName, (void*)name );
	DList_Append( self->constants, cst );
	DaoValue_MarkConst( cst->value );
}
static int DaoClass_AddConst2( DaoClass *self, DString *name, DaoValue *data, int s )
{
	int id = LOOKUP_BIND( DAO_CLASS_CONSTANT, s, 0, self->constants->size );
	DaoNamespace *ns = self->initRoutine->nameSpace;
	if( data->type == DAO_ROUTINE && data->xRoutine.routHost != self->objType ){
		if( data->xRoutine.overloads ){
			DaoRoutine *routs = DaoRoutines_New( ns, self->objType, (DaoRoutine*) data );
			data = (DaoValue*) routs;
		}
	}
	MAP_Insert( self->lookupTable, name, id );
	DaoClass_AddConst3( self, name, data );
	return id;
}
int DaoClass_AddConst( DaoClass *self, DString *name, DaoValue *data, int s )
{
	int fromMixin = 0;
	int fromParent = 0;
	int sto, pm, up, id;
	DNode *node = MAP_Find( self->lookupTable, name );
	DaoNamespace *ns = self->initRoutine->nameSpace;
	DaoConstant *dest;
	DaoValue *value;

	if( node ){
		id = LOOKUP_ID( node->value.pInt );
		fromParent = LOOKUP_UP( node->value.pInt ); /* From parent classes; */
		switch( LOOKUP_ST( node->value.pInt ) ){ /* Check if it is from mixins; */
		case DAO_CLASS_CONSTANT :
			fromMixin = id >= self->cstMixinStart && id < self->cstMixinEnd;
			break;
		case DAO_CLASS_VARIABLE :
			fromMixin = id >= self->glbMixinStart && id < self->glbMixinEnd;
			break;
		case DAO_OBJECT_VARIABLE :
			fromMixin = id >= self->objMixinStart && id < self->objMixinEnd;
			break;
		}
	}

	assert( data != NULL );
	if( fromParent || fromMixin ){ /* inherited field: */
		sto = LOOKUP_ST( node->value.pInt );
		pm = LOOKUP_PM( node->value.pInt );
		id = LOOKUP_ID( node->value.pInt );
		if( sto != DAO_CLASS_CONSTANT ){ /* override inherited variable: */
			DMap_EraseNode( self->lookupTable, node );
			return DaoClass_AddConst( self, name, data, s );
		}
		node->value.pInt = LOOKUP_BIND( sto, pm, 0, id );
		dest = self->constants->items.pConst[id];
		if( dest->value->type == DAO_ROUTINE && data->type == DAO_ROUTINE ){
			/* Add the inherited routine(s) for overloading: */
			DaoRoutine *routs = DaoRoutines_New( ns, self->objType, (DaoRoutine*)dest->value );
			DaoConstant *cst = DaoConstant_New( (DaoValue*) routs, DAO_CLASS_CONSTANT );
			routs->trait |= DAO_VALUE_CONST;
			node->value.pInt = LOOKUP_BIND( sto, pm, 0, self->constants->size );
			DList_Append( self->cstDataName, (void*) name );
			DList_Append( self->constants, cst );
			return DaoClass_AddConst( self, name, data, s );
		}else{
			/* Add the new constant: */
			DaoConstant *cst = DaoConstant_New( data, DAO_CLASS_CONSTANT );
			node->value.pInt = LOOKUP_BIND( sto, pm, 0, self->constants->size );
			DList_Append( self->cstDataName, (void*) name );
			DList_Append( self->constants, cst );
			return node->value.pInt;
		}
	}else if( node ){
		sto = LOOKUP_ST( node->value.pInt );
		pm = LOOKUP_PM( node->value.pInt );
		id = LOOKUP_ID( node->value.pInt );
		if( sto != DAO_CLASS_CONSTANT ) return -DAO_CTW_WAS_DEFINED;
		dest = self->constants->items.pConst[id];
		value = dest->value;
		if( value->type != DAO_ROUTINE || data->type != DAO_ROUTINE ) return -DAO_CTW_WAS_DEFINED;
		if( s > pm ) node->value.pInt = LOOKUP_BIND( sto, s, 0, id );
		if( value->xRoutine.overloads == NULL || value->xRoutine.routHost != self->objType ){
			DaoRoutine *routs = DaoRoutines_New( ns, self->objType, (DaoRoutine*) value );
			routs->trait |= DAO_VALUE_CONST;
			/* Add individual entry for the existing function: */
			if( value->xRoutine.routHost == self->objType ) DaoClass_AddConst3( self, name, value );
			GC_Assign( & dest->value, routs );
		}
		if( data->xRoutine.overloads ){
			DaoRoutines_Add( (DaoRoutine*) dest->value, (DaoRoutine*) data );
		}else{
			DaoRoutine *rout = (DaoRoutine*) data;
			DRoutines_Add( dest->value->xRoutine.overloads, rout );
			/* Add individual entry for the new function: */
			if( data->xRoutine.routHost == self->objType ) DaoClass_AddConst3( self, name, data );
		}
		return node->value.pInt;
	}

	node = MAP_Find( self->lookupTable, name );
	if( node && LOOKUP_UP( node->value.pInt ) ) return -DAO_CTW_WAS_DEFINED;
	return DaoClass_AddConst2( self, name, data, s );
}
int DaoClass_AddGlobalVar( DaoClass *self, DString *name, DaoValue *data, DaoType *t, int s )
{
	int size = self->variables->size;
	int id = LOOKUP_BIND( DAO_CLASS_VARIABLE, s, 0, size );
	DNode *node = MAP_Find( self->lookupTable, name );
	if( node && LOOKUP_UP( node->value.pInt ) ) return -DAO_CTW_WAS_DEFINED;
	if( data == NULL && t ) data = t->value;
	MAP_Insert( self->lookupTable, name, id );
	DList_Append( self->variables, DaoVariable_New( NULL, t, DAO_CLASS_VARIABLE ) );
	DList_Append( self->glbDataName, (void*)name );
	if( data && DaoValue_Move( data, & self->variables->items.pVar[size]->value, t ) ==0 )
		return -DAO_TYPE_NOT_MATCHING;
	return id;
}
void DaoClass_AddOverloadedRoutine( DaoClass *self, DString *signature, DaoRoutine *rout )
{
	MAP_Insert( self->methSignatures, signature, rout );
}
DaoRoutine* DaoClass_GetOverloadedRoutine( DaoClass *self, DString *signature )
{
	DNode *node = MAP_Find( self->methSignatures, signature );
	if( node ) return (DaoRoutine*) node->value.pValue;
	return NULL;
}

const char *interface_meth_warning =
"[[WARNING]] Invalid overriding of interface (virtual) method with unmatched return types!\n";

static void DaoStream_WriteMessageHeader( DaoStream *self )
{
	DaoStream_WriteChars( self, interface_meth_warning );
}
static void DaoStream_WriteRoutineInfo( DaoStream *self, DaoRoutine *routine, int virt )
{
	DaoStream_WriteChars( self, virt ? ">> Interface  method: " : ">> Overriding method: " );
	DaoStream_WriteChars( self, routine->routHost ? routine->routHost->name->chars : "" );
	DaoStream_WriteChars( self, "::" );
	DaoStream_WriteString( self, routine->routName );
	DaoStream_WriteChars( self, "(), " );
	DaoStream_WriteString( self, routine->routType->name );
	DaoStream_WriteChars( self, "\n   At line " );
	DaoStream_WriteInt( self, routine->defLine );
	DaoStream_WriteChars( self, ", in file: " );
	DaoStream_WriteString( self, routine->nameSpace->name );
	DaoStream_WriteChars( self, "\n" );
}

void DaoClass_UpdateVirtualMethods( DaoClass *self )
{
	DaoStream *stream = self->initRoutine->nameSpace->vmSpace->errorStream;
	DNode *it;
	int i, j;

	if( self->interMethods ) return;

	for(i=0; i<self->allBases->size; ++i){
		DaoClass *base = self->allBases->items.pClass[i];
		if( base->type != DAO_CLASS ) continue;
		if( base->interMethods == NULL ) continue;

		if( self->interMethods == NULL ) self->interMethods = DHash_New(0,0);
		for(it=DMap_First(base->interMethods); it; it=DMap_Next(base->interMethods,it)){
			DMap_Insert( self->interMethods, it->key.pVoid, it->value.pVoid );
		}
	}
	for(i=0; i<self->constants->size; ++i){
		DaoValue *value = self->constants->items.pConst[i]->value;
		DaoRoutine *ometh = (DaoRoutine*) value;
		if( value->type != DAO_ROUTINE ) continue;
		if( ometh->routHost != self->objType ) continue;
		if( ometh->attribs & (DAO_ROUT_INITOR | DAO_ROUT_STATIC) ) continue;
		if( ometh->attribs & DAO_ROUT_INTERFACE ){
			if( self->interMethods == NULL ) self->interMethods = DHash_New(0,0);
			DMap_Insert( self->interMethods, ometh, ometh );
		}
		for(it=DMap_First(self->interMethods); it; it=DMap_Next(self->interMethods,it)){
			DaoRoutine *imeth = it->key.pRoutine;
			DaoType *otype = ometh->routType;
			DaoType *itype = imeth->routType;

			/* Interface method of this class: */
			if( imeth->routHost == self->objType ) continue;

			/*
			// It was overridden by a non-interface method which means further
			// overriding is no longer allowed:
			*/
			if( !(it->value.pRoutine->attribs & DAO_ROUT_INTERFACE) ) continue;
			
			if( otype->nested->size != itype->nested->size ) continue;
			if( ! DString_EQ( imeth->routName, ometh->routName ) ) continue;

			for(j=1; j<otype->nested->size; ++j){
				DaoType *optype = otype->nested->items.pType[j];
				DaoType *iptype = itype->nested->items.pType[j];
				if( ! DString_EQ( iptype->name, optype->name ) ) break;
				/*
				// The parameter types of the interface methods must match to that of
				// the overriding methods, so that the parameters intended for the
				// interface methods can be passed properly to the overriding methods:
				*/
				if( DaoType_MatchTo( iptype, optype, NULL ) == 0 ) break;
			}
			if( j < otype->nested->size ) continue;

			/* Type names must match (type matching may be insufficiently precise): */
			if( ! DString_EQ( itype->aux->xType.name, otype->aux->xType.name ) ){
				goto UnmatchingReturn;
			}

			/*
			// The return types of the overriding methods must match to that of the
			// interface methods, so that the return values of the overriding methods
			// can be passed properly:
			*/
			if( DaoType_MatchTo( (DaoType*)otype->aux, (DaoType*)itype->aux, NULL ) == 0 ){
				goto UnmatchingReturn;
			}
			it->value.pRoutine = ometh;
			continue;
UnmatchingReturn:
			DaoStream_WriteMessageHeader( stream );
			DaoStream_WriteRoutineInfo( stream, imeth, 1 );
			DaoStream_WriteRoutineInfo( stream, ometh, 0 );
		}
	}
}
void DaoClass_PrintCode( DaoClass *self, DaoStream *stream )
{
	daoint i;
	DaoStream_WriteChars( stream, "class " );
	DaoStream_WriteString( stream, self->className );
	DaoStream_WriteChars( stream, ":\n" );
	for(i=0; i<self->constants->size; ++i){
		DaoValue *cst = self->constants->items.pConst[i]->value;
		if( cst->type != DAO_ROUTINE || cst->xRoutine.body == NULL ) continue;
		if( cst->xRoutine.routHost != self->objType ) continue;
		DaoRoutine_PrintCode( & cst->xRoutine, stream );
	}
}
DaoRoutine* DaoClass_FindMethod( DaoClass *self, const char *name, DaoClass *scoped )
{
	DString name2 = DString_WrapChars( name );
	DaoValue *V = DaoClass_GetData( self, & name2, scoped );
	if( V == NULL || V->type != DAO_CONSTANT ) return NULL;
	if( V->xConst.value == NULL || V->xConst.value->type != DAO_ROUTINE ) return NULL;
	return (DaoRoutine*) V->xConst.value;
}
