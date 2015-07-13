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
#include"stdio.h"
#include"string.h"
#include"ctype.h"
#include"assert.h"

#include"daoType.h"
#include"daoVmspace.h"
#include"daoNamespace.h"
#include"daoNumtype.h"
#include"daoStream.h"
#include"daoRoutine.h"
#include"daoObject.h"
#include"daoProcess.h"
#include"daoGC.h"
#include"daoStdlib.h"
#include"daoClass.h"
#include"daoParser.h"
#include"daoRegex.h"
#include"daoValue.h"


/* Need separated mutexes for values and methods setup.
 * Otherwise, a mutex deadlock may occur if values setup
 * is triggered by methods setup. */
DMutex mutex_values_setup;
DMutex mutex_methods_setup;
DMutex mutex_type_map;


static void DNS_GetField( DaoValue *self0, DaoProcess *proc, DString *name )
{
	DaoNamespace *self = & self0->xNamespace;
	DNode *node = NULL;
	int st, pm, id;
	node = MAP_Find( self->lookupTable, name );
	if( node == NULL ) goto FieldNotExist;
	st = LOOKUP_ST( node->value.pInt );
	pm = LOOKUP_PM( node->value.pInt );
	id = LOOKUP_ID( node->value.pInt );
	if( pm == DAO_PERM_PRIVATE && self != proc->activeNamespace ) goto FieldNoPermit;
	if( st == DAO_GLOBAL_CONSTANT ){
		DaoProcess_PutValue( proc, self->constants->items.pConst[id]->value );
	}else{
		DaoProcess_PutValue( proc, self->variables->items.pVar[id]->value );
	}
	return;
FieldNotExist:
	DaoProcess_RaiseError( proc, "Field::NotExist", name->chars );
	return;
FieldNoPermit:
	DaoProcess_RaiseError( proc, "Field::NotPermit", name->chars );
	return;
InvalidField:
	DaoProcess_RaiseError( proc, "Field", name->chars );
}
static void DNS_SetField( DaoValue *self0, DaoProcess *proc, DString *name, DaoValue *value )
{
	DaoNamespace *self = & self0->xNamespace;
	DaoVariable *dest;
	DNode *node = NULL;
	int st, pm, id;
	node = MAP_Find( self->lookupTable, name );
	if( node == NULL ) goto FieldNotExist;
	st = LOOKUP_ST( node->value.pInt );
	pm = LOOKUP_PM( node->value.pInt );
	id = LOOKUP_ID( node->value.pInt );
	if( pm == DAO_PERM_PRIVATE && self != proc->activeNamespace ) goto FieldNoPermit;
	if( st == DAO_GLOBAL_CONSTANT ) goto FieldNoPermit;
	dest = self->variables->items.pVar[id];
	if( DaoValue_Move( value, & dest->value, dest->dtype ) ==0 ) goto TypeNotMatching;
	return;
FieldNotExist:
	DaoProcess_RaiseError( proc, "Field::NotExist", name->chars );
	return;
FieldNoPermit:
	DaoProcess_RaiseError( proc, "Field::NotPermit", name->chars );
	return;
TypeNotMatching:
	DaoProcess_RaiseError( proc, "Type", "not matching" );
	return;
InvalidField:
	DaoProcess_RaiseError( proc, "Field", name->chars );
}
static void DNS_GetItem( DaoValue *self0, DaoProcess *proc, DaoValue *ids[], int N )
{
}
static void DNS_SetItem( DaoValue *self0, DaoProcess *proc, DaoValue *ids[], int N, DaoValue *value )
{
}
static DaoTypeCore nsCore =
{
	NULL,
	DNS_GetField,
	DNS_SetField,
	DNS_GetItem,
	DNS_SetItem,
	DaoValue_Print
};

DaoNamespace* DaoNamespace_GetNamespace( DaoNamespace *self, const char *name )
{
	DaoNamespace *ns;
	DString mbs = DString_WrapChars( name );
	ns = DaoNamespace_FindNamespace( self, & mbs );
	if( ns == NULL ){
		ns = DaoNamespace_New( self->vmSpace, name );
		DString_InsertChars( ns->name, "::", 0, 0, -1 );
		DString_Insert( ns->name, self->name, 0, 0, -1 );
		DList_Append( ns->namespaces, self );
		DaoNamespace_AddConst( self, & mbs, (DaoValue*)ns, DAO_PERM_PUBLIC );
		DList_Append( ns->auxData, self ); /* for GC */
	}
	return ns;
}
void DaoNamespace_AddValue( DaoNamespace *self, const char *s, DaoValue *v, const char *t )
{
	DaoType *abtp = NULL;
	DString name = DString_WrapChars( s );
	if( t && strlen( t ) >0 ){
		DaoParser *parser = DaoVmSpace_AcquireParser( self->vmSpace );
		parser->nameSpace = self;
		parser->vmSpace = self->vmSpace;
		abtp = DaoParser_ParseTypeName( t, self, NULL ); /* XXX warn */
		DaoVmSpace_ReleaseParser( self->vmSpace, parser );
	}
	DaoNamespace_AddVariable( self, & name, v, abtp, DAO_PERM_PUBLIC );
}
DaoValue* DaoNamespace_FindData( DaoNamespace *self, const char *name )
{
	DString s = DString_WrapChars( name );
	return DaoNamespace_GetData( self, & s );
}
void DaoNamespace_AddConstNumbers( DaoNamespace *self, DaoNumItem *items )
{
	DaoValue buf = {0};
	DaoValue *value = (DaoValue*) & buf;
	int i = 0;
	memset( value, 0, sizeof(DaoValue) );
	while( items[i].name != NULL ){
		DString name = DString_WrapChars( items[i].name );
		switch( items[i].type ){
		case DAO_INTEGER : value->xInteger.value = (dao_integer) items[i].value; break;
		case DAO_FLOAT   : value->xFloat.value = items[i].value; break;
		default: continue;
		}
		value->type = items[i].type;
		DaoNamespace_AddConst( self, & name, value, DAO_PERM_PUBLIC );
		i ++;
	}
}
void DaoNamespace_AddConstValue( DaoNamespace *self, const char *name, DaoValue *value )
{
	DString s = DString_WrapChars( name );
	DaoNamespace_AddConst( self, & s, value, DAO_PERM_PUBLIC );
}
static void DaoTypeBase_Parents( DaoTypeBase *typer, DList *parents )
{
	daoint i, k, n;
	DList_Clear( parents );
	DList_Append( parents, typer );
	for(k=0; k<parents->size; k++){
		typer = (DaoTypeBase*) parents->items.pVoid[k];
		for(i=0; i<DAO_MAX_CDATA_SUPER; i++){
			if( typer->supers[i] == NULL ) break;
			DList_Append( parents, typer->supers[i] );
		}
	}
}
int DaoNamespace_SetupValues( DaoNamespace *self, DaoTypeBase *typer )
{
	daoint i, j, valCount;
	DList *parents;
	DMap *values;
	DNode *it;

	if( typer->core == NULL ) return 0;
	if( typer->core->kernel->SetupValues == NULL ) return 1;
	for(i=0; i<DAO_MAX_CDATA_SUPER; i++){
		if( typer->supers[i] == NULL ) break;
		DaoNamespace_SetupValues( self, typer->supers[i] );
	}
	valCount = 0;
	if( typer->numItems != NULL ){
		while( typer->numItems[ valCount ].name != NULL ) valCount ++;
	}

	DMutex_Lock( & mutex_values_setup );
	if( typer->core->kernel == NULL ){
		typer->core->kernel = DaoTypeKernel_New( typer );
		DList_Append( self->auxData, typer->core->kernel );
	}
	if( typer->core->kernel->values == NULL ){
		DaoValue buf = {0};
		DaoValue *value = (DaoValue*) & buf;
		DaoType *abtype = typer->core->kernel->abtype;

		memset( value, 0, sizeof(DaoValue) );
		values = DHash_New( DAO_DATA_STRING, DAO_DATA_VALUE );
		typer->core->kernel->values = values;
		for(i=0; i<valCount; i++){
			DString name = DString_WrapChars( typer->numItems[i].name );
			double dv = typer->numItems[i].value;
			value->type = typer->numItems[i].type;
			switch( value->type ){
			case DAO_INTEGER : value->xInteger.value = (dao_integer) dv; break;
			case DAO_FLOAT   : value->xFloat.value = dv; break;
			default : continue;
			}
			DMap_Insert( values, & name, value );
		}
		parents = DList_New(0);
		DaoTypeBase_Parents( typer, parents );
		for(i=1; i<parents->size; i++){
			DaoTypeBase *sup = (DaoTypeBase*) parents->items.pVoid[i];
			DaoTypeKernel *skn = sup->core->kernel;
			if( sup->numItems == NULL ) continue;
			for(j=0; sup->numItems[j].name!=NULL; j++){
				DString name = DString_WrapChars( sup->numItems[j].name );
				it = DMap_Find( sup->core->kernel->values, & name );
				if( it && DMap_Find( values, & name ) == NULL ){
					DMap_Insert( values, it->key.pVoid, it->value.pVoid );
				}
			}
		}
		DList_Delete( parents );
	}
	typer->core->kernel->SetupValues = NULL;
	DMutex_Unlock( & mutex_values_setup );
	return 1;
}
void DaoMethods_Insert( DMap *methods, DaoRoutine *rout, DaoNamespace *ns, DaoType *host )
{
	DNode *node = MAP_Find( methods, rout->routName );
	if( node == NULL ){
		DMap_Insert( methods, rout->routName, rout );
	}else if( node->value.pRoutine->overloads ){
		DRoutines_Add( node->value.pRoutine->overloads, rout );
	}else{
		DaoRoutine *mroutine = DaoRoutines_New( ns, host, node->value.pRoutine );
		DRoutines_Add( mroutine->overloads, rout );
		GC_Assign( & node->value.pValue, mroutine );
	}
}
int DaoNamespace_SetupMethods( DaoNamespace *self, DaoTypeBase *typer )
{
	DaoParser *parser, *defparser;
	DaoRoutine *cur;
	DList *parents;
	DMap *methods;
	DMap *supMethods;
	DNode *it;
	daoint i, k, size;

#if DEBUG
	assert( typer->core != NULL );
#endif

	if( typer->core->kernel->SetupMethods == NULL ) return 1;
	if( typer->funcItems == NULL && typer->supers[0] == NULL ) return 0;
	for(i=0; i<DAO_MAX_CDATA_SUPER; i++){
		if( typer->supers[i] == NULL ) break;
		DaoNamespace_SetupMethods( self, typer->supers[i] );
	}
	DMutex_Lock( & mutex_methods_setup );
	if( typer->core->kernel == NULL ){
		typer->core->kernel = DaoTypeKernel_New( typer );
		DList_Append( self->auxData, typer->core->kernel );
	}
	if( typer->core->kernel->methods == NULL ){
		DaoType *hostype = typer->core->kernel->abtype;
		DaoInterface *inter = DaoValue_CastInterface( hostype->aux );
		DString name;

		methods = DHash_New( DAO_DATA_STRING, DAO_DATA_VALUE );
		DaoNamespace_InitConstEvalData( self );

		typer->core->kernel->methods = methods;

		parser = DaoVmSpace_AcquireParser( self->vmSpace );
		parser->vmSpace = self->vmSpace;
		parser->nameSpace = self;
		parser->hostType = hostype;
		parser->hostCtype = (DaoCtype*) hostype->aux;
		parser->defParser = defparser = DaoVmSpace_AcquireParser( self->vmSpace );
		defparser->vmSpace = self->vmSpace;
		defparser->nameSpace = self;
		defparser->hostType = hostype;
		defparser->hostCtype = (DaoCtype*) hostype->aux;
		defparser->routine = self->constEvalRoutine;

		size = 0;
		if( typer->funcItems != NULL ){
			while( typer->funcItems[ size ].proto != NULL ) size ++;
		}

		for( i=0; i<size; i++ ){
			const char *proto = typer->funcItems[i].proto;
			if( strcmp( proto, "__SLICED__" ) == 0 ){
				typer->core->kernel->Sliced = (FuncPtrSliced) typer->funcItems[i].fpter;
				continue;
			}
			cur = DaoNamespace_ParseSignature( self, proto, parser );
			if( cur == NULL ){
				printf( "  In function: %s::%s\n", typer->name, proto );
				continue;
			}
			cur->pFunc = typer->funcItems[i].fpter;
			if( hostype && DString_EQ( cur->routName, hostype->name ) ){
				cur->attribs |= DAO_ROUT_INITOR;
				DaoTypeKernel_InsertInitor( hostype->kernel, self, hostype, cur );
			}
			DaoMethods_Insert( methods, cur, self, hostype );
		}
		if( hostype->tid == DAO_INTERFACE ){
			for(i=0; i<DAO_MAX_CDATA_SUPER; ++i){
				if( typer->supers[i] == NULL ) break;
				DList_Append( inter->supers, typer->supers[i]->core->kernel->abtype->aux );
			}
		}
		parents = DList_New(0);
		DaoTypeBase_Parents( typer, parents );
		for(i=1; i<parents->size; i++){
			DaoTypeBase *sup = (DaoTypeBase*) parents->items.pVoid[i];
			supMethods = sup->core->kernel->methods;
			for(it=DMap_First(supMethods); it; it=DMap_Next(supMethods, it)){
				if( it->value.pRoutine->overloads ){
					DRoutines *meta = (DRoutines*) it->value.pRoutine->overloads;
					for(k=0; k<meta->routines->size; k++){
						DaoRoutine *rout = meta->routines->items.pRoutine[k];
						/* skip constructor */
						if( rout->attribs & DAO_ROUT_INITOR ) continue;
						/* skip methods not defined in this parent type */
						if( rout->routHost != sup->core->kernel->abtype ) continue;
						DaoMethods_Insert( hostype->kernel->methods, rout, self, hostype );
						if( !( rout->attribs & DAO_ROUT_CASTOR ) ) continue;
						DaoTypeKernel_InsertCastor( hostype->kernel, self, hostype, rout );
					}
				}else{
					DaoRoutine *rout = it->value.pRoutine;
					/* skip constructor */
					if( rout->attribs & DAO_ROUT_INITOR ) continue;
					/* skip methods not defined in this parent type */
					if( rout->routHost != sup->core->kernel->abtype ) continue;
					DaoMethods_Insert( hostype->kernel->methods, rout, self, hostype );
					if( !( rout->attribs & DAO_ROUT_CASTOR ) ) continue;
					DaoTypeKernel_InsertCastor( hostype->kernel, self, hostype, rout );
				}
			}
		}
		if( hostype->tid == DAO_INTERFACE ){
			DMap_Assign( hostype->aux->xInterface.methods, methods );
			hostype->aux->xInterface.derived = 1;
		}
		DList_Delete( parents );
		DaoVmSpace_ReleaseParser( self->vmSpace, parser );
		DaoVmSpace_ReleaseParser( self->vmSpace, defparser );
		name = DString_WrapChars( "(int)" );
		it = DMap_Find( methods, & name );
		if( it ) hostype->kernel->intOperators = it->value.pRoutine;
		name = DString_WrapChars( "==" );
		it = DMap_Find( methods, & name );
		if( it ) hostype->kernel->eqOperators = it->value.pRoutine;
		name = DString_WrapChars( "<" );
		it = DMap_Find( methods, & name );
		if( it ) hostype->kernel->ltOperators = it->value.pRoutine;
	}
	typer->core->kernel->SetupMethods = NULL;
	DMutex_Unlock( & mutex_methods_setup );
	return 1;
}
enum { DAO_DT_FAILED, DAO_DT_SCOPED, DAO_DT_UNSCOPED };

void DaoParser_Error( DaoParser *self, int code, DString *ext );
void DaoParser_Error2( DaoParser *self, int code, int m, int n, int single_line );
void DaoParser_PrintError( DaoParser *self, int line, int code, DString *ext );
int DaoParser_FindPairToken( DaoParser *self,  uchar_t lw, uchar_t rw, int start, int stop );
int DaoParser_ParseTemplateParams( DaoParser *self, int start, int end, DList *holders, DList *defaults, DString *name );
DaoType* DaoParser_ParseTypeItems( DaoParser *self, int start, int end, DList *types, int *co );
DaoType* DaoCdata_NewType( DaoTypeBase *typer );

int DaoParser_ParseMaybeScopeConst( DaoParser *self, DaoValue **scope, DaoValue **value, int start, int stop, int type );

static void DaoValue_AddType( DaoValue *self, DString *name, DaoType *type )
{
	DaoType *type2 = type;
	DaoValue *cst = (DaoValue*) type;
	DaoTypeCore *core;
	if( type->tid == DAO_CTYPE ) type2 = type->aux->xCtype.cdtype;
	if( type->tid >= DAO_OBJECT && type->tid <= DAO_INTERFACE ) cst = type->aux;
	switch( self->type ){
	case DAO_CTYPE :
		core = self->xCdata.ctype->kernel->core;
		DaoNamespace_SetupValues( core->kernel->nspace, self->xCdata.ctype->kernel->typer );
		if( core->kernel->values == NULL ){
			core->kernel->values = DHash_New( DAO_DATA_STRING, DAO_DATA_VALUE );
		}
		DMap_Insert( core->kernel->values, name, cst );
		break;
	case DAO_CLASS :
		DaoClass_AddConst( & self->xClass, name, cst, DAO_PERM_PUBLIC );
		break;
	case DAO_NAMESPACE :
		if( type->typer->core && type->typer->core->kernel ){
			/* For properly parsing methods (self of types and default values): */
			GC_Assign( & type->typer->core->kernel->nspace, self );
		}
		DaoNamespace_AddType( & self->xNamespace, name, type2 );
		DaoNamespace_AddTypeConstant( & self->xNamespace, name, type );
		break;
	}
}

static int DaoNS_ParseType( DaoNamespace *self, const char *name, DaoType *type, DaoType *type2, int isnew )
{
	DList *types = NULL;
	DList *defts = NULL;
	DString *string = NULL;
	DaoToken **tokens;
	DaoParser *parser = DaoVmSpace_AcquireParser( self->vmSpace );
	DaoTypeKernel *kernel = type->typer->core->kernel;
	DaoValue *scope = NULL, *value = NULL;
	DTypeSpecTree *sptree = NULL;
	daoint i, k, n, tid, ret = DAO_DT_UNSCOPED;

	DaoNamespace_InitConstEvalData( self );
	parser->vmSpace = self->vmSpace;
	parser->nameSpace = self;
	parser->routine = self->constEvalRoutine;
	parser->evalMode |= DAO_CONST_EVAL_GETVALUE;

	if( ! DaoLexer_Tokenize( parser->lexer, name, 0 ) ) goto Error;
	tokens = parser->tokens->items.pToken;
	n = parser->tokens->size - 1;

	if( parser->tokens->size == 0 ) goto Error;
	DList_Clear( parser->errors );
	k = DaoParser_ParseMaybeScopeConst( parser, &scope, &value, 0, 0, DAO_EXPRLIST_SCOPE );
	if( k < 0 ) goto Error;
	if( k == 0 && n ==0 ) goto Finalize; /* single identifier name; */
	if( scope && (tid=scope->type) != DAO_CTYPE && tid != DAO_CLASS && tid != DAO_NAMESPACE ){
		DaoParser_Error2( parser, DAO_UNDEFINED_SCOPE_NAME, k-2, k-2, 0 );
		goto Error;
	}
	if( k == n ){
		DaoTypeCore *core;
		DString *name = & tokens[k]->string;
		if( value != NULL ){
			DaoParser_Error2( parser, DAO_SYMBOL_WAS_DEFINED, k, k, 0 );
			goto Error;
		}else if( scope == NULL ){
			DaoParser_Error2( parser, DAO_UNDEFINED_SCOPE_NAME, k-2, k-2, 0 );
			goto Error;
		}
		if( isnew ){
			DString_Assign( type->name, name );
			DString_Assign( type2->name, name );
		}
		DaoValue_AddType( scope, name, type );
		DaoVmSpace_ReleaseParser( self->vmSpace, parser );
		return DAO_DT_SCOPED;
	}
	ret = k ? DAO_DT_SCOPED : DAO_DT_UNSCOPED;
	if( type->tid != DAO_CTYPE && type->tid != DAO_ARRAY && type->tid != DAO_LIST && type->tid != DAO_MAP ) goto Error;
	if( (value && value->type != DAO_CTYPE) || tokens[k+1]->type != DTOK_LT ) goto Error;
	if( DaoParser_FindPairToken( parser, DTOK_LT, DTOK_GT, k+1, -1 ) != (int)n ) goto Error;

	types = DList_New(0);
	defts = DList_New(0);
	string = DString_New();
	DList_Clear( parser->errors );
	DaoParser_ParseTemplateParams( parser, k+2, n, types, defts, NULL );
	if( parser->errors->size ) goto Error;

	type->nested = DList_New( DAO_DATA_VALUE );
	if( type2 != type ) type2->nested = DList_New( DAO_DATA_VALUE );
	for(i=0; i<types->size; i++){
		DList_Append( type->nested, types->items.pType[i] );
		if( type2 != type ) DList_Append( type2->nested, types->items.pType[i] );
	}
	if( isnew ){
		DString_Assign( type->name, & tokens[k]->string );
		DString_AppendChar( type->name, '<' );
		for(i=0; i<types->size; i++){
			if( i ) DString_AppendChar( type->name, ',' );
			DString_Append( type->name, types->items.pType[i]->name );
		}
		DString_AppendChar( type->name, '>' );
		DString_Assign( type2->name, type->name );
	}

	/*
	// Declaration: type name declared in the typer structure;
	// Current: the current types (namely the paremeter "type" and "type2");
	// Template: the type that hosts type specialziation data structure;
	// Alias: the type that can be accessed with the template name;
	//
	// CASE 1:
	// Declaration:                TypeName<>
	// Current:                    TypeName<>
	// Template:                   TypeName<>
	// Alias:        TypeName  =>  TypeName<>
	//
	// CASE 2:
	// Declaration:                TypeName<@TypeHolder=DefaultType,...>
	// Current:                    TypeName<@TypeHolder,...>
	// Template:                   TypeName<@TypeHolder,...>
	// Alias:        TypeName  =>  TypeName<DefaultType,...>
	//
	// CASE 3:
	// Declaration:                TypeName<@TypeHolder,...>
	// Current:                    TypeName<@TypeHolder,...>
	// Template:                   TypeName<@TypeHolder,...>
	// Alias:        TypeName  =>  TypeName<@TypeHolder,...>
	//
	// CASE 4:
	// Declaration:                TypeName<ConcreteType,...>
	// Current:                    TypeName<ConcreteType,...>
	// Template:                   TypeName<>, must have been declared!
	// Alias:        TypeName  =>  TypeName<>
	//
	// Note: @TypeHolder can be a variant type holder: @TypeHolder<Type1|Type2...>;
	*/
	if( types->size ){
		DString *name = & tokens[k]->string;
		DaoType *it = types->items.pType[0];
		DaoType *aux = (DaoType*) it->aux;
		if( it->tid == DAO_VARIANT && it->aux && it->aux->type == DAO_TYPE ) it = aux;
		if( it->tid != DAO_THT ){ /* CASE 4: ConcreteType; */
			int id = DaoNamespace_FindConst( self, name );
			value = DaoNamespace_GetConst( self, id );
			if( value == NULL ) goto Error;
		}else{
			kernel->attribs |= DAO_TYPEKERNEL_TEMPLATE;
		}
	}
	if( value == NULL ){
		DaoType *alias = type2;
		DString *name = & tokens[k]->string;

		if( scope == NULL ) scope = (DaoValue*) self;

		kernel->sptree = DTypeSpecTree_New();
		for(i=0; i<types->size; i++){
			DList_Append( kernel->sptree->holders, types->items.pType[i] );
			DList_Append( kernel->sptree->defaults, defts->items.pType[i] );
		}

		/* CASE 2: */
		if( defts->size && defts->items.pType[0] )
			alias = DaoType_Specialize( type, defts->items.pType, defts->size );
		DaoValue_AddType( scope, name, alias );

	}else{
		sptree = value->xCdata.ctype->kernel->sptree;
		if( sptree == NULL ) goto Error;
		if( sptree->holders->size && types->size )
			if( DTypeSpecTree_Test( sptree, types->items.pType, types->size ) == 0 ) goto Error;
		DTypeSpecTree_Add( sptree, types->items.pType, types->size, type2 );
	}


Finalize:
	if( sptree == NULL && ret == DAO_DT_UNSCOPED ){
		if( string == NULL ){
			string = DString_New();
			DString_SetChars( string, name );
		}
		DaoNamespace_AddType( self, string, type2 );
		DaoNamespace_AddTypeConstant( self, string, type );
	}
	DaoVmSpace_ReleaseParser( self->vmSpace, parser );
	if( string ) DString_Delete( string );
	if( types ) DList_Delete( types );
	if( defts ) DList_Delete( defts );
	return ret;
Error:
	DaoParser_Error2( parser, DAO_INVALID_TYPE_FORM, 0, parser->tokens->size-1, 0 );
	DaoParser_PrintError( parser, 0, 0, NULL );
	DaoVmSpace_ReleaseParser( self->vmSpace, parser );
	if( string ) DString_Delete( string );
	if( types ) DList_Delete( types );
	if( defts ) DList_Delete( defts );
	return DAO_DT_FAILED;
}
void DaoParser_PushLevel( DaoParser *self );
void DaoParser_PopLevel( DaoParser *self );
DaoType* DaoNamespace_DefineType( DaoNamespace *self, const char *type, const char *alias )
{
	DaoNamespace *ns;
	DaoType *tp, *tp2, *tht = NULL;
	DaoStream *stream = self->vmSpace->errorStream;
	DString name = DString_WrapChars( type );
	DString alias2 = DString_WrapChars( alias );
	DNode *node;
	int i = 0, id, recursive = 0;
	/* printf( "DaoNamespace_TypeDefine: %s %s\n", type, alias ); */
	tp = DaoNamespace_FindType( self, & name );
	if( tp == NULL ){
		DaoParser *parser = DaoVmSpace_AcquireParser( self->vmSpace );
		if( ! DaoLexer_Tokenize( parser->lexer, type, DAO_LEX_ESCAPE ) ) goto DoneSourceType;
		DaoNamespace_InitConstEvalData( self );
		parser->nameSpace = self;
		parser->routine = self->constEvalRoutine;

		if( alias != NULL ){
			DaoParser_PushLevel( parser );
			tht = DaoType_New( alias, DAO_THT, NULL, NULL );
			id = LOOKUP_BIND_LC( parser->routine->routConsts->value->size );
			MAP_Insert( parser->lookupTables->items.pMap[ parser->lexLevel ], & alias2, id );
			DaoRoutine_AddConstant( parser->routine, (DaoValue*) tht ); 
			id = tht->refCount;
		}

		tp = DaoParser_ParseType( parser, 0, parser->tokens->size-1, &i, NULL );
		if( i < parser->tokens->size && tp ) tp = NULL;
		if( alias != NULL ){
			recursive = tht->refCount > id;
			DaoParser_PopLevel( parser );
		}

DoneSourceType:
		DaoVmSpace_ReleaseParser( self->vmSpace, parser );
		if( tp == NULL ){
			DaoStream_WriteChars( stream, "ERROR: in DaoNamespace_DefineType(), type \"" );
			DaoStream_WriteChars( stream, type );
			DaoStream_WriteChars( stream, "\" is not valid!\n" );
			return NULL;
		}
	}
	if( alias == NULL ) return tp;

	tp2 = DaoNamespace_FindType( self, & alias2 );
	if( tp2 == NULL ) tp2 = DaoParser_ParseTypeName( alias, self, NULL );
	if( tp == tp2 ) return tp;
	/* printf( "ns = %p  tp = %p  name = %s\n", self, tp, alias ); */

	/* Only allow overiding types defined in parent namespaces: */
	node = MAP_Find( self->abstypes, & alias2 );
	if( node != NULL ){
		DaoStream_WriteChars( stream, "ERROR: in DaoNamespace_DefineType(), type \"" );
		DaoStream_WriteChars( stream, alias );
		DaoStream_WriteChars( stream, "\" was defined!\n" );
		return NULL;
	}

	/*
	// Copy primitive types, so that it can be treated differently when necessary.
	// For example, a SQL module may alias "int" to "INT", "TINYINT", "SMALLINT"
	// and "INT_PRIMARY_KEY" etc., so that they can be handled differently in handling
	// table fields.
	//
	// Do not copy other non-primitive types, in particular, do not copy cdata types.
	// Otherwise, inheritance relationship will not be handled properly for the copied
	// types. If it is a template-like type, copying the type and using new template
	// parameter types may cause problems in type matching, or function specialization
	// if the function tries to specialize based on the types of template parameters.
	//
	// To create a template-like alias to a template-like cdata type, it is only
	// necessary to add a specialization entry in the template cdata type.
	*/
	if( tp->tid == DAO_CDATA || tp->tid == DAO_CSTRUCT ) tp = tp->aux->xCtype.ctype;
	tp2 = tp;
	if( (tp->tid && tp->tid <= DAO_TUPLE) || tp->tid == DAO_VARIANT ){
		tp = DaoType_Copy( tp );
		DString_SetChars( tp->name, alias );
	}
	if( recursive ){
		tp->recursive = 1; 
		DaoType_SetupRecursive( tp, tht, tp );
	}    
	if( DaoNS_ParseType( self, alias, tp, tp, tp != tp2 ) == DAO_DT_FAILED ){
		DaoStream_WriteChars( stream, "ERROR: in DaoNamespace_DefineType(), type aliasing from \"" );
		DaoStream_WriteChars( stream, type );
		DaoStream_WriteChars( stream, "\" to \"" );
		DaoStream_WriteChars( stream, alias );
		DaoStream_WriteChars( stream, "\" failed!\n" );
		GC_IncRC( tp );
		GC_DecRC( tp );
		return NULL;
	}
	return tp;
}

static DaoType* DaoNamespace_MakeCdataType( DaoNamespace *self, DaoTypeBase *typer, int opaque )
{
	DaoTypeKernel *kernel = DaoTypeKernel_New( typer );
	DaoType *cdata_type = DaoCdata_NewType( typer );
	DaoType *ctype_type = cdata_type->aux->xCdata.ctype;

	GC_IncRC( self );
	GC_IncRC( cdata_type );
	kernel->nspace = self;
	kernel->abtype = cdata_type;
	cdata_type->tid = opaque ? DAO_CDATA : DAO_CSTRUCT;
	GC_Assign( & ctype_type->kernel, kernel );
	GC_Assign( & cdata_type->kernel, kernel );
	typer->core = kernel->core;
	return ctype_type;
}

static DaoType* DaoNamespace_WrapType2( DaoNamespace *self, DaoTypeBase *typer, int options )
{
	DaoType *ctype_type, *cdata_type;

	if( typer->core ) return typer->core->kernel->abtype;

	ctype_type = DaoNamespace_MakeCdataType( self, typer, options & DAO_CTYPE_OPAQUE );
	cdata_type = typer->core->kernel->abtype;
	if( options & DAO_CTYPE_INVAR ) cdata_type->aux->xCtype.attribs |= DAO_CLS_INVAR;
	typer->core->kernel->SetupValues = DaoNamespace_SetupValues;
	typer->core->kernel->SetupMethods = DaoNamespace_SetupMethods;
	if( DaoNS_ParseType( self, typer->name, ctype_type, cdata_type, 1 ) == DAO_DT_FAILED ){
		GC_IncRC( ctype_type );
		GC_DecRC( ctype_type );
		printf( "type wrapping failed: %s from %s\n", typer->name, self->name->chars );
		return NULL;
	}
	DString_SetChars( cdata_type->aux->xCtype.name, cdata_type->name->chars );
	return cdata_type;
}
DaoType* DaoNamespace_WrapType( DaoNamespace *self, DaoTypeBase *typer, int options )
{
	return DaoNamespace_WrapType2( self, typer, options );
}
DaoType* DaoNamespace_WrapGenericType( DaoNamespace *self, DaoTypeBase *typer, int tid )
{
	DaoTypeKernel *kernel;
	DaoType *cdata_type;

	if( typer->core->kernel ) return typer->core->kernel->abtype;

	kernel = DaoTypeKernel_New( typer );
	cdata_type = DaoType_New( typer->name, tid, NULL, NULL );

	GC_IncRC( self );
	GC_IncRC( cdata_type );
	kernel->nspace = self;
	kernel->abtype = cdata_type;
	GC_Assign( & cdata_type->kernel, kernel );
	typer->core = kernel->core;
	typer->core->kernel->SetupValues = DaoNamespace_SetupValues;
	typer->core->kernel->SetupMethods = DaoNamespace_SetupMethods;
	if( DaoNS_ParseType( self, typer->name, cdata_type, cdata_type, 0 ) == DAO_DT_FAILED ){
		GC_DecRC( cdata_type );
		printf( "type wrapping failed: %s\n", typer->name );
		return NULL;
	}
	//printf( "type wrapping: %s\n", typer->name );
	return cdata_type;
}
DaoType* DaoNamespace_WrapInterface( DaoNamespace *self, DaoTypeBase *typer )
{
	DaoInterface *inter;
	DaoTypeKernel *kernel;
	DaoType *abtype;

	if( typer->core ) return typer->core->kernel->abtype;

	inter = DaoInterface_New( typer->name );
	kernel = DaoTypeKernel_New( typer );
	abtype = inter->abtype;

	GC_Assign( & abtype->kernel, kernel );
	GC_Assign( & kernel->abtype, abtype );
	GC_Assign( & kernel->nspace, self );

	typer->core = kernel->core;

	kernel->SetupValues = DaoNamespace_SetupValues;
	kernel->SetupMethods = DaoNamespace_SetupMethods;
	if( DaoNS_ParseType( self, typer->name, abtype, abtype, 1 ) == DAO_DT_FAILED ){
		GC_IncRC( inter );
		GC_DecRC( inter );
		printf( "type wrapping failed: %s from %s\n", typer->name, self->name->chars );
		return NULL;
	}
	return abtype;
}
void DaoNamespace_SetupType( DaoNamespace *self, DaoTypeBase *typer, DaoType *type )
{
	if( typer->core == NULL ) return;
	DMutex_Lock( & mutex_values_setup ); // XXX
	if( typer->core->kernel == NULL ){
		typer->core->kernel = DaoTypeKernel_New( typer );
		typer->core->kernel->abtype = type;
		typer->core->kernel->nspace = self;
		typer->core->kernel->SetupValues = DaoNamespace_SetupValues;
		typer->core->kernel->SetupMethods = DaoNamespace_SetupMethods;
		DList_Append( self->auxData, typer->core->kernel );
		GC_IncRC( self );
		if( type ){
			GC_IncRC( type );
			GC_Assign( & type->kernel, typer->core->kernel );
		}
	}
	DMutex_Unlock( & mutex_values_setup );
}
int DaoNamespace_WrapTypes( DaoNamespace *self, DaoTypeBase *typers[] )
{
	DaoParser *parser = DaoVmSpace_AcquireParser( self->vmSpace );
	int i, ec = 0;
	for(i=0; typers[i]; i++ ){
		ec += DaoNamespace_WrapType2( self, typers[i], 1 ) == NULL;
		/* e |= ( DaoNamespace_SetupValues( self, typers[i] ) == 0 ); */
	}
	/* if( setup ) return DaoNamespace_SetupTypes( self, typers ); */
	DaoVmSpace_ReleaseParser( self->vmSpace, parser );
	return ec;
}
int DaoNamespace_AliasTypes( DaoNamespace *self, const char *alias[] )
{
	int i = 0, ec = 0;
	if( alias == NULL ) return 0;
	while( alias[i] && alias[i+1] ){
		ec += DaoNamespace_DefineType( self, alias[i], alias[i+1] ) == NULL;
		i += 2;
	}
	return ec;
}
int DaoNamespace_SetupTypes( DaoNamespace *self, DaoTypeBase *typers[] )
{
	int i, ec = 0;
	for(i=0; typers[i]; i++ ){
		ec += ( DaoNamespace_SetupMethods( self, typers[i] ) == 0 );
	}
	return ec;
}
DaoRoutine* DaoNamespace_MakeFunction( DaoNamespace *self, const char *proto, DaoParser *parser )
{
	DaoParser *old = parser;
	DaoParser *defparser = NULL;
	DaoRoutine *func;
	DaoValue *value;

	if( parser == NULL ){
		DaoNamespace_InitConstEvalData( self );
		parser = DaoVmSpace_AcquireParser( self->vmSpace );
		defparser = DaoVmSpace_AcquireParser( self->vmSpace );
		parser->vmSpace = self->vmSpace;
		parser->nameSpace = self;
		parser->defParser = defparser;
		defparser->vmSpace = self->vmSpace;
		defparser->nameSpace = self;
		defparser->routine = self->constEvalRoutine;
	}
	func = DaoNamespace_ParseSignature( self, proto, parser );
	if( old == NULL ){
		DaoVmSpace_ReleaseParser( self->vmSpace, parser );
		DaoVmSpace_ReleaseParser( self->vmSpace, defparser );
	}
	if( func == NULL ) return NULL;
	value = DaoNamespace_GetData( self, func->routName );
	if( value && value->type == DAO_ROUTINE && value->xRoutine.overloads ){
		DRoutines_Add( value->xRoutine.overloads, func );
	}else{
		DaoNamespace_AddConst( self, func->routName, (DaoValue*) func, DAO_PERM_PUBLIC );
	}
	return func;
}
DaoRoutine* DaoNamespace_WrapFunction( DaoNamespace *self, DaoCFunction fptr, const char *proto )
{
	DaoRoutine *func = DaoNamespace_MakeFunction( self, proto, NULL );
	if( func == NULL ) return NULL;
	func->pFunc = fptr;
	return func;
}

int DaoNamespace_WrapFunctions( DaoNamespace *self, DaoFuncItem *items )
{
	DaoParser *defparser = DaoVmSpace_AcquireParser( self->vmSpace );
	DaoParser *parser = DaoVmSpace_AcquireParser( self->vmSpace );
	DaoRoutine *func;
	int i = 0;
	int ec = 0;
	DaoNamespace_InitConstEvalData( self );
	parser->vmSpace = self->vmSpace;
	parser->nameSpace = self;
	parser->defParser = defparser;
	defparser->vmSpace = self->vmSpace;
	defparser->nameSpace = self;
	defparser->routine = self->constEvalRoutine;
	while( items[i].fpter != NULL ){
		func = DaoNamespace_MakeFunction( self, items[i].proto, parser );
		if( func ) func->pFunc = (DaoCFunction)items[i].fpter;
		ec += func == NULL;
		i ++;
	}
	DaoVmSpace_ReleaseParser( self->vmSpace, parser );
	DaoVmSpace_ReleaseParser( self->vmSpace, defparser );
	return ec;
}
int DaoNamespace_Load( DaoNamespace *self, const char *fname )
{
	DString *src;
	DaoVmSpace *vms = self->vmSpace;
	FILE *fin = Dao_OpenFile( fname, "r" );
	int ret;
	if( ! fin ){
		DaoStream_WriteChars( vms->errorStream, "ERROR: can not open file \"" );
		DaoStream_WriteChars( vms->errorStream, fname );
		DaoStream_WriteChars( vms->errorStream, "\".\n" );
		return 0;
	}
	src = DString_New();
	DaoFile_ReadAll( fin, src, 1 );
	ret = DaoProcess_Eval( self->vmSpace->mainProcess, self, src->chars );
	DString_Delete( src );
	return ret;
}
void DaoNamespace_SetOptions( DaoNamespace *self, int options )
{
	self->options = options;
}
int DaoNamespace_GetOptions( DaoNamespace *self )
{
	return self->options;
}


DaoTypeBase nsTyper =
{
	"namespace", & nsCore, NULL, NULL, {0}, {0},
	(FuncPtrDel) DaoNamespace_Delete, NULL
};

DaoNamespace* DaoNamespace_New( DaoVmSpace *vms, const char *nsname )
{
	DaoValue *value;
	DString *name = DString_New();
	DaoNamespace *self = (DaoNamespace*) dao_calloc( 1, sizeof(DaoNamespace) );
	DaoValue_Init( self, DAO_NAMESPACE );
	self->trait |= DAO_VALUE_DELAYGC;
	self->vmSpace = vms;
	self->constants = DList_New( DAO_DATA_VALUE );
	self->variables = DList_New( DAO_DATA_VALUE );
	self->auxData = DList_New( DAO_DATA_VALUE );
	self->namespaces = DList_New(0);
	self->lookupTable = DHash_New( DAO_DATA_STRING, 0 );
	self->mainRoutines  = DList_New( DAO_DATA_VALUE );
	self->definedRoutines = DList_New(0);
	self->localMacros = DHash_New( DAO_DATA_STRING, DAO_DATA_VALUE );
	self->globalMacros = DHash_New( DAO_DATA_STRING, DAO_DATA_VALUE );
	self->abstypes = DHash_New( DAO_DATA_STRING, DAO_DATA_VALUE );
	self->codeInliners = DHash_New( DAO_DATA_STRING, 0 );
	self->tokenFilters = DList_New(0);
	self->argParams = DaoList_New();
	self->file = DString_New();
	self->path = DString_New();
	self->name = DString_New();
	self->lang = DString_New();
	self->inputs = DString_New();
	self->sources = DList_New( DAO_DATA_LIST );

	DList_Append( self->auxData, self->argParams );

	DString_SetChars( self->lang, "dao" );
	DList_Append( self->namespaces, self );

	DaoNamespace_SetName( self, nsname );
	DaoNamespace_AddConst( self, self->name, (DaoValue*) self, DAO_PERM_PUBLIC );

	DString_SetChars( name, "none" );
	DaoNamespace_AddConst( self, name, dao_none_value, DAO_PERM_PUBLIC );
	DString_SetChars( name, "false" );
	DaoNamespace_AddConst( self, name, dao_false_value, DAO_PERM_PUBLIC );
	DString_SetChars( name, "true" );
	DaoNamespace_AddConst( self, name, dao_true_value, DAO_PERM_PUBLIC );

	/* reserved for main */
	DList_Append( self->constants, DaoConstant_New( dao_none_value, DAO_GLOBAL_CONSTANT ) );

	if( vms && vms->daoNamespace ){
		DaoNamespace *ns = vms->daoNamespace;
		DaoNamespace_AddConst( self, ns->name, (DaoValue*)ns, DAO_PERM_PUBLIC );
		DList_Append( self->namespaces, ns );
		DaoNamespace_UpdateLookupTable( self );
	}else if( vms && vms->preloadModules ){
		daoint i;
		for(i=0; i<vms->preloadModules->size; i++){
			DList_Append( self->namespaces, vms->preloadModules->items.pNS[i] );
		}
		DaoNamespace_UpdateLookupTable( self );
	}
	DString_Delete( name );
	self->cstUser = self->constants->size;
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
void DaoNamespace_Delete( DaoNamespace *self )
{
	/* printf( "DaoNamespace_Delete  %s\n", self->name->chars ); */

#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	DMap_Delete( self->lookupTable );
	DList_Delete( self->constants );
	DList_Delete( self->variables );
	DList_Delete( self->auxData );

	/* no need for GC, because these namespaces are indirectly
	 * referenced through functions. */
	DList_Delete( self->namespaces );

	DList_Delete( self->mainRoutines );
	DList_Delete( self->definedRoutines );
	DMap_Delete( self->localMacros );
	DMap_Delete( self->globalMacros );
	DMap_Delete( self->abstypes );
	DMap_Delete( self->codeInliners );
	DList_Delete( self->tokenFilters );
	DString_Delete( self->file );
	DString_Delete( self->path );
	DString_Delete( self->name );
	DString_Delete( self->lang );
	DString_Delete( self->inputs );
	DList_Delete( self->sources );
	dao_free( self );
}
void DaoNamespace_InitConstEvalData( DaoNamespace *self )
{
	if( self->constEvalProcess ) return;
	self->constEvalProcess = DaoProcess_New( self->vmSpace );
	self->constEvalRoutine = DaoRoutine_New( self, NULL, 1 );
	self->constEvalRoutine->routType = dao_type_routine;
	self->constEvalProcess->activeNamespace = self;
	GC_IncRC( dao_type_routine );
	DaoProcess_InitTopFrame( self->constEvalProcess, self->constEvalRoutine, NULL );
	DaoProcess_SetActiveFrame( self->constEvalProcess, self->constEvalProcess->topFrame );
	self->constEvalRoutine->trait |= DAO_VALUE_CONST;
	self->constEvalProcess->trait |= DAO_VALUE_CONST;
	DList_Append( self->auxData, (DaoValue*) self->constEvalRoutine );
	DList_Append( self->auxData, (DaoValue*) self->constEvalProcess );
}
void DaoNamespace_SetName( DaoNamespace *self, const char *name )
{
	daoint i;
	DString_SetChars( self->name, name );
	i = DString_RFindChar( self->name, '/', -1 );
	if( i != DAO_NULLPOS ){
		DString_SetChars( self->file, name + i + 1 );
		DString_SetBytes( self->path, name, i + 1 );
		i = DString_RFindChar( self->name, '.', -1 );
		if( i != DAO_NULLPOS ) DString_SetChars( self->lang, self->name->chars + i + 1 );
	}else{
		DString_Clear( self->file );
		DString_Clear( self->path );
	}
}
int DaoNamespace_FindConst( DaoNamespace *self, DString *name )
{
	DNode *node = DMap_Find( self->lookupTable, name );
	if( node == NULL ) return -1;
	if( LOOKUP_ST( node->value.pInt ) != DAO_GLOBAL_CONSTANT ) return -1;
	return node->value.pInt;
}
/* Generate a new lookup name for the existing constant/variable (for bytecode): */
static void DaoNamespace_RenameLookup( DaoNamespace *self, DNode *node )
{
	DString *name = DString_Copy( node->key.pString );
	char chs[32];
	sprintf( chs, "[%i]", LOOKUP_ID( node->value.pInt ) );
	DString_AppendChars( name, chs );
	MAP_Insert( self->lookupTable, name, node->value.pVoid ) ;
	DString_Delete( name );
}
int DaoNamespace_AddConst( DaoNamespace *self, DString *name, DaoValue *value, int pm )
{
	DaoValue *vdest;
	DaoConstant *cst, *dest;
	DaoRoutine *mroutine;
	DNode *node = MAP_Find( self->lookupTable, name );
	int isrout2, isrout = value->type == DAO_ROUTINE;
	daoint st, pm2, up, id = 0;

	if( node && LOOKUP_ST( node->value.pInt ) == DAO_GLOBAL_CONSTANT ){
		st = LOOKUP_ST( node->value.pInt );
		id = LOOKUP_ID( node->value.pInt );
		cst = self->constants->items.pConst[id];
		if( cst->value->type == DAO_ROUTINE && value->type == DAO_ROUTINE ){
			DaoNamespace_RenameLookup( self, node );
			/* For different overloadings at different definition points: */
			mroutine = DaoRoutines_New( self, NULL, (DaoRoutine*) cst->value );
			mroutine->trait |= DAO_VALUE_CONST;
			node->value.pInt = LOOKUP_BIND( st, pm, 0, self->constants->size );
			DaoRoutines_Add( mroutine, (DaoRoutine*) value );
			DList_Append( self->constants, DaoConstant_New( (DaoValue*) mroutine, DAO_GLOBAL_CONSTANT ) );
			return node->value.pInt;
		}
	}
	if( node && LOOKUP_UP( node->value.pInt ) ){
		DaoNamespace_RenameLookup( self, node );
		node = NULL;
	}
	if( node ) return -DAO_CTW_WAS_DEFINED;

	id = LOOKUP_BIND( DAO_GLOBAL_CONSTANT, pm, 0, self->constants->size );
	MAP_Insert( self->lookupTable, name, id );
	DList_Append( self->constants, (dest = DaoConstant_New( value, DAO_GLOBAL_CONSTANT )) );
	DaoValue_MarkConst( dest->value );
	return id;
}
void DaoNamespace_SetConst( DaoNamespace *self, int index, DaoValue *value )
{
	DaoConstant *dest;
	daoint id = LOOKUP_ID( index );
	if( LOOKUP_ST( index ) != DAO_GLOBAL_CONSTANT ) return;
	if( id >= self->constants->size ) return;
	dest = self->constants->items.pConst[id];
	DaoValue_Copy( value, & dest->value );
	DaoValue_MarkConst( dest->value );
}
DaoValue* DaoNamespace_GetConst( DaoNamespace *self, int index )
{
	daoint st = LOOKUP_ST( index );
	daoint id = LOOKUP_ID( index );
	if( index < 0 ) return NULL;
	if( st != DAO_GLOBAL_CONSTANT ) return NULL;
	if( id >= self->constants->size ) return NULL;
	return self->constants->items.pConst[id]->value;
}
int DaoNamespace_FindVariable( DaoNamespace *self, DString *name )
{
	DNode *node = DMap_Find( self->lookupTable, name );
	if( node == NULL ) return -1;
	if( LOOKUP_ST( node->value.pInt ) != DAO_GLOBAL_VARIABLE ) return -1;
	return node->value.pInt;
}
int DaoNamespace_AddVariable( DaoNamespace *self, DString *name, DaoValue *value, DaoType *tp, int pm )
{
	DNode *node = MAP_Find( self->lookupTable, name );
	DaoType *abtp = DaoNamespace_GetType( self, value );
	DaoVariable *dest;
	daoint id = 0;

	if( abtp == NULL ) abtp = dao_type_udf;
	if( tp && value && DaoType_MatchValue( tp, value, NULL ) ==0 ) return -1;
	if( tp == NULL ) tp = abtp;
	if( value == NULL && tp ) value = tp->value;

	if( node ){
		if( LOOKUP_UP( node->value.pInt ) ){ /* overriding */
			DMap_EraseNode( self->lookupTable, node );
			DaoNamespace_AddVariable( self, name, value, tp, pm );
			node = MAP_Find( self->lookupTable, name );
			return node->value.pInt;
		}
		return -1;
	}else{
		id = LOOKUP_BIND( DAO_GLOBAL_VARIABLE, pm, 0, self->variables->size );
		MAP_Insert( self->lookupTable, name, id ) ;
		DList_Append( self->variables, DaoVariable_New( value, tp, DAO_GLOBAL_VARIABLE ) );
	}
	return id;
}
int DaoNamespace_AddStaticConst( DaoNamespace *self, DString *name, DaoValue *value, int level )
{
	int ret;
	char suffix[32];
	sprintf( suffix, "{%i}[%i]", level, (int)self->constants->size );
	name = DString_Copy( name );
	DString_AppendChars( name, suffix );
	/* should always succeed: */
	ret = DaoNamespace_AddConst( self, name, value, DAO_PERM_NONE );
	DString_Delete( name );
	return ret;
}
int DaoNamespace_AddStaticVar( DaoNamespace *self, DString *name, DaoValue *var, DaoType *tp, int level )
{
	int ret;
	char suffix[32];
	sprintf( suffix, "{%i}[%i]", level, (int)self->variables->size );
	name = DString_Copy( name );
	DString_AppendChars( name, suffix );
	ret = DaoNamespace_AddVariable( self, name, var, tp, DAO_PERM_NONE );
	DString_Delete( name );
	return ret;
}
int DaoNamespace_SetVariable( DaoNamespace *self, int index, DaoValue *value )
{
	DaoVariable *dest;
	daoint id = LOOKUP_ID( index );
	if( LOOKUP_ST( index ) != DAO_GLOBAL_VARIABLE ) return 0;
	if( id >= self->variables->size ) return 0;
	dest = self->variables->items.pVar[id];
	return DaoValue_Move( value, & dest->value, dest->dtype );
}
DaoValue* DaoNamespace_GetVariable( DaoNamespace *self, int index )
{
	daoint st = LOOKUP_ST( index );
	daoint id = LOOKUP_ID( index );
	if( st != DAO_GLOBAL_VARIABLE ) return NULL;
	if( id >= self->variables->size ) return NULL;
	return self->variables->items.pVar[id]->value;
}
DaoType* DaoNamespace_GetVariableType( DaoNamespace *self, int index )
{
	daoint st = LOOKUP_ST( index );
	daoint id = LOOKUP_ID( index );
	if( st != DAO_GLOBAL_VARIABLE ) return NULL;
	if( id >= self->variables->size ) return NULL;
	return self->variables->items.pVar[id]->dtype;
}
void DaoNamespace_SetData( DaoNamespace *self, DString *name, DaoValue *value )
{
	DNode *node = MAP_Find( self->lookupTable, name );
	if( node ){
		daoint id = node->value.pInt;
		daoint st = LOOKUP_ST( id );
		if( st == DAO_GLOBAL_CONSTANT ) DaoNamespace_SetConst( self, id, value );
		if( st == DAO_GLOBAL_VARIABLE ) DaoNamespace_SetVariable( self, id, value );
		return;
	}
	DaoNamespace_AddVariable( self, name, value, NULL, DAO_PERM_PROTECTED );
}
DaoValue* DaoNamespace_GetData( DaoNamespace *self, DString *name )
{
	DNode *node = MAP_Find( self->lookupTable, name );
	if( node == NULL ) return NULL;
	return DaoNamespace_GetValue( self, node->value.pInt );
}
DaoValue* DaoNamespace_GetValue( DaoNamespace *self, daoint index )
{
	daoint st = LOOKUP_ST( index );
	if( st == DAO_GLOBAL_CONSTANT ) return DaoNamespace_GetConst( self, index );
	if( st == DAO_GLOBAL_VARIABLE ) return DaoNamespace_GetVariable( self, index );
	return NULL;
}
DaoClass* DaoNamespace_FindClass( DaoNamespace *self, DString *name )
{
	int id = DaoNamespace_FindConst( self, name );
	DaoValue *value = DaoNamespace_GetConst( self, id );
	if( value && value->type == DAO_CLASS ) return & value->xClass;
	return NULL;
}
DaoNamespace* DaoNamespace_FindNamespace( DaoNamespace *self, DString *name )
{
	int id = DaoNamespace_FindConst( self, name );
	DaoValue *value = DaoNamespace_GetConst( self, id );
	if( value && value->type == DAO_NAMESPACE ) return & value->xNamespace;
	return NULL;
}
int DaoNamespace_CyclicParent( DaoNamespace *self, DaoNamespace *parent )
{
	daoint i;
	if( parent == self ) return 1;
	for(i=0; i<self->namespaces->size; i++)
		if( self->namespaces->items.pNS[i] == parent ) return 0;
	for(i=1; i<parent->namespaces->size; i++){
		if( DaoNamespace_CyclicParent( self, parent->namespaces->items.pNS[i] ) ) return 1;
	}
	return 0;
}
static void DaoNS_ImportRoutine( DaoNamespace *self, DString *name, DaoRoutine *routine, int pm )
{
	DNode *search = MAP_Find( self->lookupTable, name );
	if( search == NULL ){
		DaoNamespace_AddConst( self, name, (DaoValue*)routine, pm );
	}else if( LOOKUP_ST( search->value.pInt ) == DAO_GLOBAL_CONSTANT ){
		DaoRoutine *routine2 = (DaoRoutine*) DaoNamespace_GetConst( self, search->value.pInt );
		DaoRoutine **routines = & routine;
		int i, num = 1;
		if( routine2->type != DAO_ROUTINE ) return;
		if( routine == routine2 ) return;
		if( routine->overloads ){
			routines = routine->overloads->routines->items.pRoutine;
			num = routine->overloads->routines->size;
		}
		if( routine2->overloads ){
			for(i=0; i<num; ++i){
				DaoRoutine *rout = routines[i];
				DRoutines_Add( routine2->overloads, rout );
			}
		}else{
			DaoRoutine *routs = DaoRoutines_New( self, NULL, routine );
			for(i=0; i<num; ++i){
				DaoRoutine *rout = routines[i];
				DRoutines_Add( routs->overloads, routine2 );
			}
			DaoValue_MarkConst( (DaoValue*) routine2 );
			/* Add individual entry for the existing function: */
			DList_Append( self->constants, DaoConstant_New( (DaoValue*) routine2, DAO_GLOBAL_CONSTANT ) );
			DaoNamespace_SetConst( self, search->value.pInt, (DaoValue*) routs );
		}
	}
}
void DaoNamespace_UpdateLookupTable( DaoNamespace *self )
{
	DNode *it, *search;
	daoint i, j, k, pm, st, up, id;

	for(i=1; i<self->namespaces->size; i++){
		DaoNamespace *ns = self->namespaces->items.pNS[i];
		DaoNamespace_UpdateLookupTable( ns );
		for(it=DMap_First( ns->lookupTable ); it; it=DMap_Next(ns->lookupTable,it) ){
			DaoValue *value = DaoNamespace_GetValue( ns, it->value.pInt );
			DString *name = it->key.pString;
			up = LOOKUP_UP( it->value.pInt );
			pm = LOOKUP_PM( it->value.pInt );
			st = LOOKUP_ST( it->value.pInt );
			id = LOOKUP_ID( it->value.pInt );
			if( pm != DAO_PERM_PUBLIC || value == NULL ) continue;

			search = MAP_Find( self->lookupTable, name );
			if( search && value->type == DAO_ROUTINE && up == 0 ){
				DaoNS_ImportRoutine( self, name, (DaoRoutine*)value, pm );
				continue;
			}
			if( search ) continue;
			if( st == DAO_GLOBAL_CONSTANT ){
				DaoConstant *cst = ns->constants->items.pConst[id];
				if( cst->value->type == DAO_ROUTINE ){
					DaoNamespace_AddConst( self, name, cst->value, pm );
					continue;
				}
				k = LOOKUP_BIND( st, pm, up+1, self->constants->size );
				MAP_Insert( self->lookupTable, name, k );
				DList_Append( self->constants, ns->constants->items.pConst[id] );
			}else{
				k = LOOKUP_BIND( st, pm, up+1, self->variables->size );
				MAP_Insert( self->lookupTable, name, k );
				DList_Append( self->variables, ns->variables->items.pVar[id] );
			}
		}
	}
}
int DaoNamespace_AddParent( DaoNamespace *self, DaoNamespace *parent )
{
	daoint i;
	if( parent == self ) return 0;
	if( DaoNamespace_CyclicParent( self, parent ) ) return 0;
	for(i=0; i<self->namespaces->size; i++){
		if( self->namespaces->items.pNS[i] == parent ){
			DaoNamespace_UpdateLookupTable( self );
			return 1;
		}
	}
	parent->trait |= DAO_VALUE_CONST;
	DList_Append( self->auxData, parent );
	DList_Append( self->namespaces, parent );
	DaoNamespace_UpdateLookupTable( self );
	return 1;
}

void DaoNamespace_AddCodeInliner( DaoNamespace *self, const char *name, DaoCodeInliner fp )
{
	DString mbs = DString_WrapChars( name );
	DMap_Insert( self->codeInliners, & mbs, (void*)fp );
}
DaoCodeInliner DaoNamespace_FindCodeInliner( DaoNamespace *self, DString *name )
{
	int i, n = self->namespaces->size;
	DNode *node = MAP_Find( self->codeInliners, name );
	if( node ) return (DaoCodeInliner) node->value.pVoid;
	for(i=1; i<n; i++){
		DaoNamespace *ns = self->namespaces->items.pNS[i];
		DaoCodeInliner inliner = DaoNamespace_FindCodeInliner( ns, name );
		if( inliner ) return inliner;
	}
	return NULL;
}

DaoType* DaoNamespace_FindType( DaoNamespace *self, DString *name )
{
	DNode *node;
	DaoType *type = NULL;
	int i, n = self->namespaces->size;

	DMutex_Lock( & mutex_type_map );
	node = MAP_Find( self->abstypes, name );
	if( node ) type = node->value.pType;
	DMutex_Unlock( & mutex_type_map );
	if( type ) return type;

	for(i=1; i<n; i++){
		DaoNamespace *ns = self->namespaces->items.pNS[i];
		DaoType *type = DaoNamespace_FindType( ns, name );
		if( type == NULL ) continue;
		return type;
	}
	return NULL;
}
DaoType* DaoNamespace_FindTypeChars( DaoNamespace *self, const char *name )
{
	DString name2 = DString_WrapChars( name );
	return DaoNamespace_FindType( self, & name2 );
}
DaoType* DaoNamespace_ParseType( DaoNamespace *self, const char *name )
{
	return DaoParser_ParseTypeName( name, self, NULL );
}
DaoType* DaoNamespace_AddType( DaoNamespace *self, DString *name, DaoType *type )
{
	DNode *node;
	if( DString_FindChar( type->name, '@', 0 ) == DAO_NULLPOS ){
		DMutex_Lock( & mutex_type_map );
		node = MAP_Find( self->abstypes, name );
		if( node == NULL ){
			MAP_Insert( self->abstypes, name, type );
		}else{
			DList_Append( self->auxData, type );
			type = node->value.pType;
		}
		DMutex_Unlock( & mutex_type_map );
	}else{
		DList_Append( self->auxData, type );
	}
	return type;
}
void DaoNamespace_AddTypeConstant( DaoNamespace *self, DString *name, DaoType *tp )
{
	int id = DaoNamespace_FindConst( self, name );
	if( id >=0 && LOOKUP_UP(id) ) return;
	if( tp->aux && (tp->tid >= DAO_OBJECT && tp->tid <= DAO_INTERFACE) ){
		DaoNamespace_AddConst( self, name, tp->aux, DAO_PERM_PUBLIC );
	}else{
		DaoNamespace_AddConst( self, name, (DaoValue*) tp, DAO_PERM_PUBLIC );
	}
}


DaoType* DaoNamespace_GetType( DaoNamespace *self, DaoValue *value )
{
	DNode *node;
	DList *nested;
	DString *mbs;
	DaoType *abtp;
	DaoType *itp;
	DaoTypeBase *typer;
	int i, tid;

	abtp = DaoValue_GetType( value );
	if( abtp ) return abtp;

	if( value == NULL ) return NULL;

	if( value->type == DAO_LIST ){
		if( value->xList.value->size == 0 ) return dao_type_list_empty;
		return DaoNamespace_MakeType( self, "list", DAO_LIST, NULL, NULL, 0 );
	}else if( value->type == DAO_MAP ){
		if( value->xMap.value->size == 0 ) return dao_type_map_empty;
		return DaoNamespace_MakeType( self, "map", DAO_MAP, NULL, NULL, 0 );
	}

	tid = value->type;
	nested = NULL;
	mbs = DString_New();
	if( value->type <= DAO_TUPLE ){
		DString_SetChars( mbs, coreTypeNames[value->type] );
		if( value->type == DAO_TUPLE ){
			DaoTuple *tuple = (DaoTuple*) value;
			DString_SetChars( mbs, "tuple<" );
			nested = DList_New(0);
			for(i=0; i<tuple->size; i++){
				itp = DaoNamespace_GetType( self, tuple->values[i] );
				DList_Append( nested, itp );
				DString_Append( mbs, itp->name );
				if( i+1 < tuple->size ) DString_AppendChars( mbs, "," );
			}
			DString_AppendChars( mbs, ">" );
#ifdef DAO_WITH_NUMARRAY
		}else if( value->type == DAO_ARRAY ){
			DaoArray *array = (DaoArray*) value;
			nested = DList_New(0);
			if( array->size ==0 ){
				DString_AppendChars( mbs, "<?>" );
				DList_Append( nested, dao_type_udf );
			}else if( array->etype == DAO_INTEGER ){
				itp = DaoNamespace_MakeType( self, "int", DAO_INTEGER, 0,0,0 );
				DString_AppendChars( mbs, "<int>" );
				DList_Append( nested, itp );
			}else if( array->etype == DAO_FLOAT ){
				itp = DaoNamespace_MakeType( self, "float", DAO_FLOAT, 0,0,0 );
				DString_AppendChars( mbs, "<float>" );
				DList_Append( nested, itp );
			}else{
				itp = DaoNamespace_MakeType( self, "complex", DAO_COMPLEX, 0,0,0 );
				DString_AppendChars( mbs, "<complex>" );
				DList_Append( nested, itp );
			}
#endif
		}
		abtp = DaoNamespace_FindType( self, mbs );
		if( abtp == NULL ){
			abtp = DaoType_New( mbs->chars, tid, NULL, nested );
			abtp = DaoNamespace_AddType( self, abtp->name, abtp );
		}
	}else if( value->type == DAO_TYPE ){
		itp = (DaoType*) value;
		DString_SetChars( mbs, "type<" );
		nested = DList_New(0);
		DList_Append( nested, itp );
		DString_Append( mbs, itp->name );
		DString_AppendChars( mbs, ">" );
		abtp = DaoNamespace_FindType( self, mbs );
		if( abtp == NULL ){
			abtp = DaoType_New( mbs->chars, value->type, NULL, nested );
			abtp = DaoNamespace_AddType( self, abtp->name, abtp );
		}
	}else{
		typer = DaoValue_GetTyper( value );
		DString_SetChars( mbs, typer->name );
		abtp = DaoNamespace_FindType( self, mbs );
		if( abtp == NULL ){
			abtp = DaoType_New( typer->name, value->type, NULL, NULL );
			abtp = DaoNamespace_AddType( self, abtp->name, abtp );
		}
	}
	/* abtp might be rout->routType, which might be NULL,
	 * in case rout is DaoNamespace.constEvalRoutine */
	//XXX if( abtp && abtp->typer ==NULL ) abtp->typer = DaoValue_GetTyper( value );
	DString_Delete( mbs );
	if( nested ) DList_Delete( nested );
	return abtp;
}
DaoType* DaoNamespace_MakeType( DaoNamespace *self, const char *name,
		uint_t tid, DaoValue *pb, DaoType *nest[], int N )
{
	DaoClass *klass;
	DaoType *any = NULL;
	DaoType *tp;
	DNode   *node;
	DString *mbs;
	DList  *nstd = NULL;
	int i, n = strlen( name );
	int attrib = tid >> 16;

	if( (tid & DAO_ANY) && self != self->vmSpace->daoNamespace ){
		return DaoNamespace_MakeType( self->vmSpace->daoNamespace, name, tid, pb, nest, N );
	}

	tid = tid & 0xffff;
	if( tid != DAO_ANY ) any = dao_type_any;

	switch( tid ){
	case DAO_ARRAY :
		if( dao_type_array == NULL ) return NULL; /* Numeric array not enable; */
		return DaoType_Specialize( dao_type_array, nest, N );
	case DAO_LIST :
		return DaoType_Specialize( dao_type_list, nest, N );
	case DAO_MAP :
		return DaoType_Specialize( dao_type_map, nest, N );
	case DAO_INTERFACE :
		if( pb == NULL ) break; /* may be the general "interface" type; */
		return pb->xInterface.abtype;
	case DAO_CLASS :
		if( pb == NULL ) break; /* may be the general "class" type; */
		return pb->xClass.clsType;
	case DAO_OBJECT :
		if( pb == NULL ) return NULL;
		return pb->xClass.objType;
	case DAO_CTYPE :
		if( pb == NULL ) return NULL;
		return pb->xCtype.ctype;
	case DAO_CDATA :
	case DAO_CSTRUCT :
		if( pb == NULL ) return NULL;
		return pb->xCtype.cdtype;
	}
	
	if( tid == DAO_VARIANT ){
		DList *types;
		int newlist = 0;
		int j, k = 0;
		if( N == 0 ){
			return dao_type_none;
		}else if( N == 1 ){
			return nest[0];
		}
		types = DList_New(0);
		/* Coalesce variants: */
		for(i=0; i<N; ++i){
			if( nest[i]->tid == DAO_VARIANT ){
				for(j=0; j<nest[i]->nested->size; ++j){
					DList_Append( types, nest[i]->nested->items.pType[j] );
				}
			}else{
				DList_Append( types, nest[i] );
			}
		}
		newlist = types->size > N;
		/* Remove redundant variants: (and do not modify "nest"!) */
		for(i=0; i<types->size; ++i){
			DaoType *it = types->items.pType[i];
			int unique = 1;
			for(j=0; j<k; ++j){
				DaoType *jt = types->items.pType[j];
				int e1 = it->tid == jt->tid;
				int e2 = it->aux == jt->aux;
				int e3 = DString_EQ( it->name, jt->name );
				/* Note:
				// 1. Consider aliased types as unique types;
				// 2. Different type objects may exist for the same type (such as "int");
				*/
				if( e1 && e2 && e3 ){
					newlist = 1;
					unique = 0;
					break;
				}
			}
			if( unique ) types->items.pType[k++] = it;
		}
		if( newlist ){
			tp = DaoNamespace_MakeType( self, "", DAO_VARIANT, NULL, types->items.pType, k );
			DList_Delete( types );
			return tp;
		}
		DList_Delete( types );
		/* Then use the original "nest" and "N" arguments: */
	}

	mbs = DString_New();
	DString_Reserve( mbs, 128 );
	DString_SetChars( mbs, name );
	if( tid == DAO_CODEBLOCK ) DString_Clear( mbs );
	if( N > 0 || tid == DAO_CODEBLOCK ){
		nstd = DList_New(0);
		if( n || tid != DAO_VARIANT ) DString_AppendChar( mbs, '<' );
		for(i=0; i<N; i++){
			DaoType *it = nest[i];
			if( tid == DAO_TUPLE && it->tid == DAO_PAR_DEFAULT ){
				it = DaoNamespace_MakeType( self, it->fname->chars, DAO_PAR_NAMED, it->aux, NULL, 0 );
			}

			if( i ) DString_AppendChar( mbs, tid == DAO_VARIANT ? '|' : ',' );
			DString_Append( mbs, it->name );
			DList_Append( nstd, it );
		}
		if( (tid == DAO_ROUTINE || tid == DAO_CODEBLOCK) && pb && pb->type == DAO_TYPE ){
			DString_AppendChars( mbs, "=>" );
			DString_Append( mbs, ((DaoType*)pb)->name );
		}
		if( n || tid != DAO_VARIANT ) DString_AppendChar( mbs, '>' );
	}else if( tid == DAO_LIST || tid == DAO_ARRAY ){
		nstd = DList_New(0);
		DString_AppendChars( mbs, "<any>" );
		DList_Append( nstd, any );
	}else if( tid == DAO_MAP ){
		nstd = DList_New(0);
		DString_AppendChars( mbs, "<any,any>" );
		DList_Append( nstd, any );
		DList_Append( nstd, any );
	}else if( tid == DAO_TUPLE ){
		if( DString_FindChar( mbs, '<', 0 ) == DAO_NULLPOS ){
			DString_AppendChars( mbs, "<...>" );
			attrib |= DAO_TYPE_VARIADIC;
		}
	}else if( tid == DAO_CLASS && pb ){
		/*
		// do not save the abstract type for class and object in namespace,
		// because the class may be nested in another class, and different
		// class may nest different class with the same name, eg:
		// Error::Field::NotExist and Error::Key::NotExist
		*/
		klass = (DaoClass*) pb;
		tp = klass->clsType;
		goto Finalizing;
	}else if( tid == DAO_OBJECT ){
		klass = (DaoClass*) pb;
		tp = klass->objType;
		goto Finalizing;
	}else if( (tid == DAO_ROUTINE || tid == DAO_CODEBLOCK) && pb && pb->type == DAO_TYPE ){
		DString_AppendChar( mbs, '<' );
		DString_AppendChars( mbs, "=>" );
		DString_Append( mbs, ((DaoType*)pb)->name );
		DString_AppendChar( mbs, '>' );
	}else if( tid == DAO_PAR_NAMED ){
		DString_AppendChars( mbs, ":" );
		if( pb->type == DAO_TYPE ) DString_Append( mbs, ((DaoType*)pb)->name );
	}else if( tid == DAO_PAR_DEFAULT ){
		DString_AppendChars( mbs, "=" );
		if( pb->type == DAO_TYPE ) DString_Append( mbs, ((DaoType*)pb)->name );
	}else if( tid == DAO_PAR_VALIST ){
		if( pb && pb->type == DAO_TYPE ){
			DString_AppendChars( mbs, ":" );
			DString_Append( mbs, ((DaoType*)pb)->name );
		}
	}
	if( tid == DAO_CODEBLOCK ){
		mbs->chars[0] = '[';
		mbs->chars[mbs->size-1] = ']';
	}
	tp = DaoNamespace_FindType( self, mbs );
	if( tp == NULL ){
		tp = DaoType_New( mbs->chars, tid, pb, nstd );
		tp->attrib |= attrib;
		if( attrib & DAO_TYPE_VARIADIC ) tp->variadic = 1;
		if( tid == DAO_PAR_NAMED && N > 0 ){
			DaoType *it = nest[0];
			DaoValue *aux = (DaoValue*) it;
			DString *fname = NULL;
			if( it->tid == DAO_PAR_NAMED && DString_FindChars( it->name, "var<", 0 ) != 0 ){
				aux = it->aux;
				fname = it->fname;
			}
			if( fname ) DString_Assign( tp->fname, fname );
			GC_Assign( & tp->aux, aux );
		}else if( tid == DAO_PAR_NAMED || tid == DAO_PAR_DEFAULT ){
			DString_SetChars( tp->fname, name );
		}
		DaoType_CheckAttributes( tp );
		tp = DaoNamespace_AddType( self, tp->name, tp );
	}
Finalizing:
	DString_Delete( mbs );
	if( nstd ) DList_Delete( nstd );
	return tp;
}
DaoType* DaoNamespace_MakeType2( DaoNamespace *self, const char *name,
		uint_t tid, DaoValue *pb, DaoType *nest[], int N )
{
	DaoType **nest2 = (DaoType**) dao_calloc( N, sizeof(DaoType*) );
	DaoType *type, *aux = DaoValue_CastType( pb );
	int i;
	if( aux && aux->invar ) aux = DaoType_GetBaseType( aux );
	for(i=0; i<N; ++i){
		DaoType *type = nest[i];
		if( type && type->invar ) type = DaoType_GetBaseType( type );
		nest2[i] = type;
	}
	type = DaoNamespace_MakeType( self, name, tid, (DaoValue*) aux, nest2, N );
	dao_free( nest2 );
	return type;
}
DaoType* DaoNamespace_MakeRoutType( DaoNamespace *self, DaoType *routype,
		DaoValue *vals[], DaoType *types[], DaoType *retp )
{
	DaoType *tp, *tp2, *abtp;
	DString *fname = NULL;
	DNode *node;
	daoint i, ch = 0;

	abtp = DaoType_New( "", DAO_ROUTINE, NULL, NULL );
	abtp->attrib = routype->attrib;
	if( routype->mapNames ){
		if( abtp->mapNames ) DMap_Delete( abtp->mapNames );
		abtp->mapNames = DMap_Copy( routype->mapNames );
	}

	if( routype->name->chars[0] == '@' ) DString_AppendChar( abtp->name, '@' );
	DString_AppendChars( abtp->name, "routine<" );
	for(i=0; i<routype->nested->size; i++){
		if( i >0 ) DString_AppendChars( abtp->name, "," );
		tp = tp2 = routype->nested->items.pType[i];
		if( tp && (tp->tid == DAO_PAR_NAMED || tp->tid == DAO_PAR_DEFAULT) ){
			tp2 = & tp->aux->xType;
		}
		if( tp2 && (tp2->tid == DAO_UDT || tp2->tid == DAO_THT) ){
			if( vals && vals[i] ){
				tp2 = DaoNamespace_GetType( self, vals[i] );
			}else if( types && types[i] ){
				tp2 = types[i];
			}
			if( tp2 && (tp2->tid == DAO_PAR_NAMED || tp2->tid == DAO_PAR_DEFAULT) ){
				tp2 = & tp2->aux->xType;
			}
		}
		/* XXX typing DString_AppendChars( abtp->name, tp ? tp->name->chars : "..." ); */
		if( tp2 != tp && tp2 != & tp->aux->xType ){
			tp = DaoType_New( tp->fname->chars, tp->tid, (DaoValue*) tp2, NULL );
		}
		DString_Append( abtp->name, tp->name );
		DList_Append( abtp->nested, tp );
	}
	tp = retp ? retp : & routype->aux->xType;
	if( tp ){
		DString_AppendChars( abtp->name, "=>" );
		DString_Append( abtp->name, tp->name );
	}
	DString_AppendChars( abtp->name, ">" );
	GC_Assign( & abtp->aux, tp );
	if( routype->cbtype ){
		DMap *defs = DHash_New(0,0);
		DaoType_MatchTo( abtp, routype, defs );
		tp = DaoType_DefineTypes( routype->cbtype, self, defs );
		GC_Assign( & abtp->cbtype, tp );
		DMap_Delete( defs );
		DString_Append( abtp->name, abtp->cbtype->name );
	}
	tp = DaoNamespace_FindType( self, abtp->name );
	if( tp ){
		DaoType_Delete( abtp );
		return tp;
	}
	DaoType_CheckAttributes( abtp );
	DaoType_InitDefault( abtp );
	DaoNamespace_AddType( self, abtp->name, abtp );
	return abtp;
}

DaoRoutine* DaoNamespace_ParseSignature( DaoNamespace *self, const char *proto, DaoParser *parser )
{
	DaoRoutine *func = DaoRoutine_New( self, NULL, 0 );
	DaoParser *defparser, *oldparser = NULL;
	int optok = 0;

	assert( parser != NULL );
	defparser = oldparser = parser->defParser;
	if( parser->defParser == NULL ){
		parser->defParser = defparser = DaoVmSpace_AcquireParser( self->vmSpace );
		defparser->vmSpace = self->vmSpace;
		defparser->nameSpace = self;
		defparser->hostType = parser->hostType;
		defparser->hostCtype = parser->hostCtype;
		defparser->routine = self->constEvalRoutine;
	}

	GC_IncRC( parser->hostType );
	func->routHost = parser->hostType;
	if( ! DaoLexer_Tokenize( defparser->lexer, proto, 0 ) ) goto Error;
	if( defparser->tokens->size < 3 ) goto Error;

	parser->routine = (DaoRoutine*) func; /* safe to parse params only */
	if( DaoParser_ParseSignature( defparser, parser, optok ) < 0 ){
		DaoParser_PrintError( defparser, 0, 0, NULL );
		goto Error;
	}
	if( oldparser == NULL ) DaoVmSpace_ReleaseParser( self->vmSpace, parser->defParser );
	parser->defParser = oldparser;
	return func;
Error:
	printf( "Function wrapping failed for %s\n", proto );
	if( oldparser == NULL ) DaoVmSpace_ReleaseParser( self->vmSpace, parser->defParser );
	parser->defParser = oldparser;
	DaoRoutine_Delete( func );
	return NULL;
}
DaoEnum* DaoNamespace_MakeSymbol( DaoNamespace *self, const char *symbol )
{
	DString *name = DString_NewChars( symbol );
	DaoType *type;

	if( symbol[0] != '$' ) DString_InsertChar( name, '$', 0 );

	type = DaoNamespace_MakeSymbolType( self, name->chars );
	DString_Delete( name );

	return (DaoEnum*) type->value;
}
DaoType* DaoNamespace_MakeSymbolType( DaoNamespace *self, const char *symbol )
{
	DString sym = DString_WrapChars( symbol + 1 );
	DString *name = DString_NewChars( "enum<" );
	DaoType *type;

	DString_Append( name, & sym );
	DString_AppendChar( name, '>' );
	type = DaoNamespace_FindType( self, name );
	if( type || symbol[0] != '$' ){
		DString_Delete( name );
		return type;
	}

	type = DaoType_New( name->chars, DAO_ENUM, NULL, NULL );
	type->subtid = DAO_ENUM_SYM;
	DString_Assign( type->fname, type->name );
	DMap_Insert( type->mapNames, & sym, (void*)0 );
	DaoNamespace_AddType( self, type->name, type );
	DString_Delete( name );
	return type;
}
/* symbols should be comma or semicolon delimited string */
DaoType* DaoNamespace_MakeEnumType( DaoNamespace *self, const char *symbols )
{
	DaoType *type;
	DString *key, *name = DString_New();
	int n = strlen( symbols );
	int i, k = 0, t1 = 0, t2 = 0;

	DString_SetChars( name, "enum<" );
	DString_AppendChars( name, symbols );
	DString_AppendChar( name, '>' );
	type = DaoNamespace_FindType( self, name );
	if( type ){
		DString_Delete( name );
		return type;
	}
	key = DString_New();
	type = DaoType_New( name->chars, DAO_ENUM, NULL, NULL );
	type->subtid = DAO_ENUM_SYM;
	for(i=0; i<n; i++){
		char sym = symbols[i];
		if( sym == ',' ){
			MAP_Insert( type->mapNames, key, k );
			DString_Clear( key );
			k += 1;
			t1 = 1;
		}else if( sym == ';' ){
			MAP_Insert( type->mapNames, key, 1<<k );
			DString_Clear( key );
			k += 1;
			t2 = sym;
		}else{
			DString_AppendChar( key, sym );
		}
	}
	if( t2 ){
		if( t2 == ';' ) type->subtid = DAO_ENUM_FLAG;
		MAP_Insert( type->mapNames, key, 1<<k );
	}else{
		if( t1 == ',' ) type->subtid = DAO_ENUM_STATE;
		MAP_Insert( type->mapNames, key, k );
	}
	DaoNamespace_AddType( self, name, type );
	DString_Delete( name );
	DString_Delete( key );
	return t1 != 0 && t2 != 0 ? NULL : type;
}
DaoType* DaoNamespace_MakeValueType( DaoNamespace *self, DaoValue *value )
{
	DaoType *type;
	DString *name;
	if( value == NULL || value->type >= DAO_ARRAY ) return NULL;
	name = DString_New();
	DaoValue_GetString( value, name );
	if( value->type == DAO_ENUM && value->xEnum.subtype == DAO_ENUM_SYM ){
		type = DaoNamespace_MakeSymbolType( self, name->chars );
		DString_Delete( name );
		return type;
	}
	if( value->type == DAO_STRING ){
		DString_InsertChar( name, '\'', 0 );
		DString_AppendChar( name, '\'' );
	}
	if( name->size ==0 && value->type ==0 ) DString_SetChars( name, "none" );
	type = DaoNamespace_FindType( self, name );
	if( type == NULL ){
		if( value->type == DAO_NONE ){
			type = DaoType_New( "none", DAO_NONE, NULL, NULL );
		}else{
			type = DaoNamespace_GetType( self, value );
			type = DaoType_Copy( type );
			DString_Assign( type->name, name );
		}
		GC_Assign( & type->value, value );
		type->valtype = 1;
		DaoNamespace_AddType( self, name, type );
	}
	DString_Delete( name );
	return type;
}
DaoType* DaoNamespace_MakePairType( DaoNamespace *self, DaoType *first, DaoType *second )
{
	DaoType *noneType = DaoNamespace_MakeValueType( self, dao_none_value );
	DaoType *types[2] = {NULL, NULL};
	DaoType *type, *type2;
	DString *name;

	if( first == NULL ) first = noneType;
	if( second == NULL ) second = noneType;
	if( first->invar )  first = DaoType_GetBaseType( first );
	if( second->invar ) second = DaoType_GetBaseType( second );
	types[0] = DaoNamespace_MakeType( self, "first", DAO_PAR_NAMED, (DaoValue*)first, 0, 0 );
	types[1] = DaoNamespace_MakeType( self, "second", DAO_PAR_NAMED, (DaoValue*)second, 0, 0 );
	type = DaoNamespace_MakeType( self, "tuple", DAO_TUPLE, NULL, types, 2 );
	name = DString_Copy( type->name );
	DString_AppendChars( name, "::subtype::pair" ); /* Distinguish with normal tuple types; */
	type2 = DaoNamespace_FindType( self, name );
	if( type2 == NULL ){
		type = type2 = DaoType_Copy( type );
		type->subtid = DAO_PAIR;
		DaoNamespace_AddType( self, name, type );
	}
	DString_Delete( name );
	return type2;
}
DaoType* DaoNamespace_MakePairValueType( DaoNamespace *self, DaoValue *first, DaoValue *second )
{
	DaoType *tp1, *tp2;
	tp1 = DaoNamespace_MakeValueType( self, first );
	tp2 = DaoNamespace_MakeValueType( self, second );
	return DaoNamespace_MakePairType( self, tp1, tp2 );
}

DaoNamespace* DaoNamespace_LoadModule( DaoNamespace *self, DString *name )
{
	DaoNamespace *mod = DaoNamespace_FindNamespace( self, name );
	if( mod ) return mod;

	name = DString_Copy( name );
	mod = DaoVmSpace_LoadModule( self->vmSpace, name );
	DString_Delete( name );
	return mod;
}
