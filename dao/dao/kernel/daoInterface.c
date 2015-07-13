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

#include<stdlib.h>
#include<stdio.h>
#include<string.h>
#include<ctype.h>

#include"daoVmspace.h"
#include"daoNamespace.h"
#include"daoValue.h"
#include"daoInterface.h"
#include"daoGC.h"

extern int DaoType_Match( DaoType *self, DaoType *type, DMap *defs, DMap *binds, int dep );
void DaoMethods_Insert( DMap *methods, DaoRoutine *rout, DaoNamespace *ns, DaoType *host );


DaoInterface* DaoInterface_New( const char *name )
{
	DaoInterface *self = (DaoInterface*) dao_calloc( 1, sizeof(DaoInterface) );
	DaoValue_Init( self, DAO_INTERFACE );
	self->trait |= DAO_VALUE_DELAYGC;
	self->derived = 0;
	self->supers = DList_New( DAO_DATA_VALUE );
	self->methods = DHash_New( DAO_DATA_STRING, DAO_DATA_VALUE );
	self->abtype = DaoType_New( name, DAO_INTERFACE, (DaoValue*)self, NULL );
	GC_IncRC( self->abtype );
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
void DaoInterface_Delete( DaoInterface *self )
{
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	if( self->concretes ) DMap_Delete( self->concretes );
	GC_DecRC( self->abtype );
	DList_Delete( self->supers );
	DMap_Delete( self->methods );
	dao_free( self );
}

void DaoInterface_DeriveMethods( DaoInterface *self )
{
	daoint i, k, m, N = self->supers->size;
	DaoInterface *super;
	DNode *it;
	for(i=0; i<N; i++){
		super = (DaoInterface*) self->supers->items.pValue[i];
		if( self->abtype->bases == NULL ) self->abtype->bases = DList_New( DAO_DATA_VALUE );
		DList_Append( self->abtype->bases, super->abtype );
		for(it=DMap_First(super->methods); it; it=DMap_Next( super->methods, it )){
			if( it->value.pRoutine->overloads ){
				DRoutines *routs = it->value.pRoutine->overloads;
				for(k=0,m=routs->routines->size; k<m; k++){
					DaoRoutine *rout = routs->routines->items.pRoutine[i];
					DaoMethods_Insert( self->methods, rout, NULL, self->abtype );
				}
			}else{
				DaoMethods_Insert( self->methods, it->value.pRoutine, NULL, self->abtype );
			}
		}
	}
	self->derived = 1;
}

static int DaoRoutine_IsCompatible( DaoRoutine *self, DaoType *type, DMap *binds )
{
	DaoRoutine *rout;
	daoint i, j, n, k=-1, max = 0;
	if( self->overloads == NULL ) return DaoType_Match( self->routType, type, NULL, binds, 0 );
	for(i=0,n=self->overloads->routines->size; i<n; i++){
		rout = self->overloads->routines->items.pRoutine[i];
		if( rout->routType == type ) return 1;
	}
	for(i=0,n=self->overloads->routines->size; i<n; i++){
		rout = self->overloads->routines->items.pRoutine[i];
		j = DaoType_Match( rout->routType, type, NULL, binds, 0 );
		/*
		   printf( "%3i %3i: %3i  %s  %s\n",n,i,j,rout->routType->name->chars,type->name->chars );
		 */
		if( j && j >= max ){
			max = j;
			k = i;
		}
	}
	return (k >= 0);
}
int DaoInterface_CheckBind( DList *methods, DaoType *type, DMap *binds )
{
	DNode *it;
	DaoRoutine *rout2;
	daoint i, n, id;
	if( type->tid == DAO_OBJECT || type->tid == DAO_CLASS ){
		DaoClass *klass = & type->aux->xClass;
		for(i=0,n=methods->size; i<n; i++){
			DaoRoutine *rout = methods->items.pRoutine[i];
			rout2 = klass->initRoutines;
			if( !(rout->attribs & DAO_ROUT_INITOR) ){
				id = DaoClass_FindConst( klass, rout->routName );
				if( id <0 ) return 0;
				rout2 = (DaoRoutine*) DaoClass_GetConst( klass, id );
				if( rout2->type != DAO_ROUTINE ) return 0;
			}
			/*printf( "AAA: %s %s\n", rout->routType->name->chars,rout2->routType->name->chars);*/
			if( DaoRoutine_IsCompatible( rout2, rout->routType, binds ) ==0 ) return 0;
		}
	}else if( type->tid == DAO_INTERFACE ){
		DaoInterface *inter = (DaoInterface*) type->aux;
		for(i=0,n=methods->size; i<n; i++){
			DaoRoutine *rout = methods->items.pRoutine[i];
			DString *name = rout->routName;
			if( rout->attribs & DAO_ROUT_INITOR ) name = inter->abtype->name;
			it = DMap_Find( inter->methods, name );
			if( it == NULL ) return 0;
			if( DaoRoutine_IsCompatible( it->value.pRoutine, rout->routType, binds ) ==0 ) return 0;
		}
	}else if( type->tid == DAO_CINVALUE ){
		DaoCinType *cintype = (DaoCinType*) type->aux;
		for(i=0,n=methods->size; i<n; i++){
			DaoRoutine *rout = methods->items.pRoutine[i];
			DString *name = rout->routName;
			if( rout->attribs & DAO_ROUT_INITOR ) name = cintype->vatype->name;
			it = DMap_Find( cintype->methods, name );
			if( it == NULL ) return 0;
			if( DaoRoutine_IsCompatible( it->value.pRoutine, rout->routType, binds ) ==0 ) return 0;
		}
	}else{
		for(i=0,n=methods->size; i<n; i++){
			DaoRoutine *rout = methods->items.pRoutine[i];
			DString *name = rout->routName;
			DaoRoutine *func;
			if( rout->attribs & DAO_ROUT_INITOR ) name = type->name;
			func = DaoType_FindFunction( type, name );
			if( func == NULL ) return 0;
			if( DaoRoutine_IsCompatible( func, rout->routType, binds ) ==0 ) return 0;
		}
	}
	return 1;
}
static void DaoInterface_TempBind( DaoInterface *self, DaoType *type, DMap *binds )
{
	daoint i, N = self->supers->size;
	void *pvoid[2];
	pvoid[0] = type;
	pvoid[1] = self->abtype;
	if( DMap_Find( binds, pvoid ) ) return;
	DMap_Insert( binds, pvoid, NULL );
	for(i=0; i<N; i++){
		DaoInterface *super = (DaoInterface*) self->supers->items.pValue[i];
		DaoInterface_TempBind( super, type, binds );
	}
}
static void DMap_SortMethods( DMap *hash, DList *methods )
{
	DMap *map = DMap_New( DAO_DATA_STRING, 0 );
	DString *name = DString_New();
	DNode *it;
	daoint i, n;
	for(it=DMap_First(hash); it; it=DMap_Next(hash,it)){
		if( it->value.pRoutine->overloads ){
			DRoutines *one = it->value.pRoutine->overloads;
			for(i=0,n=one->routines->size; i<n; i++){
				DaoRoutine *rout = one->routines->items.pRoutine[i];
				DString_Assign( name, rout->routName );
				DString_AppendChars( name, " " );
				DString_Append( name, rout->routType->name );
				DMap_Insert( map, name, (void*)rout );
			}
		}else{
			DaoRoutine *rout = it->value.pRoutine;
			DString_Assign( name, rout->routName );
			DString_AppendChars( name, " " );
			DString_Append( name, rout->routType->name );
			DMap_Insert( map, name, (void*)rout );
		}
	}
	DList_Clear( methods );
	for(it=DMap_First(map); it; it=DMap_Next(map,it))
		DList_Append( methods, it->value.pVoid );
	DMap_Delete( map );
	DString_Delete( name );
}
int DaoInterface_BindTo( DaoInterface *self, DaoType *type, DMap *binds )
{
	DNode *it;
	DMap *newbinds = NULL;
	DList *methods;
	void *pvoid[2];
	daoint i, n, bl;

	/* XXX locking */
	if( type->interfaces == NULL ) type->interfaces = DHash_New( DAO_DATA_VALUE, 0 );

	pvoid[0] = type;
	pvoid[1] = self->abtype;
	if( (it = DMap_Find( type->interfaces, self )) ) return it->value.pVoid != NULL;
	if( binds && DMap_Find( binds, pvoid ) ) return 1;
	if( binds ==NULL ) newbinds = binds = DHash_New( DAO_DATA_VOID2, 0 );
	DaoInterface_TempBind( self, type, binds );
	methods = DList_New(0);
	DMap_SortMethods( self->methods, methods );
	bl = DaoInterface_CheckBind( methods, type, binds );
	DList_Delete( methods );
	if( newbinds ) DMap_Delete( newbinds );
	DMap_Insert( type->interfaces, self, bl ? self : NULL );
	if( bl == 0 ) return 0;
	for(i=0,n=self->supers->size; i<n; i++){
		DaoInterface *super = (DaoInterface*) self->supers->items.pValue[i];
		if( DMap_Find( type->interfaces, super ) ) continue;
		DMap_Insert( type->interfaces, super, super );
	}
	return 1;
}
DaoCinType* DaoInterface_GetConcrete( DaoInterface *self, DaoType *type )
{
	DNode *it;
	
	if( self->concretes == NULL ) return NULL;

	it = DMap_Find( self->concretes, type );
	if( it == NULL ){
		for(it=DMap_First(self->concretes); it; it=DMap_Next(self->concretes,it)){
			if( DaoType_MatchTo( it->key.pType, type, NULL ) >= DAO_MT_EQ ){
				return (DaoCinType*) it->value.pVoid;
			}
		}
		return NULL;
	}
	return (DaoCinType*) it->value.pVoid;
}

int DaoType_MatchInterface( DaoType *self, DaoInterface *inter, DMap *binds )
{
	DMap *inters = self->interfaces;
	DNode *it;
	daoint i;
	if( inter == NULL ) return DAO_MT_NOT;
	if( inter->abtype->kernel->SetupMethods ){
		DaoTypeKernel *kernel = inter->abtype->kernel;
		kernel->SetupMethods( kernel->nspace, kernel->typer );
	}
	if( inters == NULL ) return DAO_MT_SUB * DaoInterface_BindTo( inter, self, binds );
	if( (it = DMap_Find( inters, inter )) ) return it->value.pVoid ? DAO_MT_SUB : DAO_MT_NOT;
	return DAO_MT_SUB * DaoInterface_BindTo( inter, self, binds );
}

DaoTypeBase interTyper =
{
	"interface", & baseCore, NULL, NULL, {0}, {0},
	(FuncPtrDel) DaoInterface_Delete, NULL
};




DaoCinType* DaoCinType_New( DaoInterface *inter, DaoType *target )
{
	DaoCinType *self = (DaoCinType*) dao_calloc( 1, sizeof(DaoCinType) );
	DaoValue_Init( self, DAO_CINTYPE );
	self->trait |= DAO_VALUE_DELAYGC;
	self->derived = 0;
	self->supers = DList_New( DAO_DATA_VALUE );
	self->methods = DHash_New( DAO_DATA_STRING, DAO_DATA_VALUE );
	self->citype = DaoType_New( "interface<", DAO_CINTYPE, (DaoValue*)self, NULL );
	self->vatype = DaoType_New( inter->abtype->name->chars, DAO_CINVALUE, (DaoValue*)self, NULL );
	self->abstract = inter;
	self->target = target;
	GC_IncRC( self->citype );
	GC_IncRC( self->vatype );
	GC_IncRC( self->abstract );
	GC_IncRC( self->target );

	DString_AppendChar( self->vatype->name, '<' );
	DString_Append( self->vatype->name, target->name );
	DString_AppendChar( self->vatype->name, '>' );

	DString_Append( self->citype->name, self->vatype->name );
	DString_AppendChar( self->citype->name, '>' );

#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
void DaoCinType_Delete( DaoCinType *self )
{
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	GC_DecRC( self->abstract );
	GC_DecRC( self->target );
	GC_DecRC( self->citype );
	GC_DecRC( self->vatype );
	DList_Delete( self->supers );
	DMap_Delete( self->methods );
	dao_free( self );
}

void DaoCinType_DeriveMethods( DaoCinType *self )
{
	daoint i, k, m, N = self->supers->size;
	DaoCinType *super;
	DNode *it;
	for(i=0; i<N; i++){
		super = (DaoCinType*) self->supers->items.pValue[i];
		self->citype->bases = DList_New( DAO_DATA_VALUE );
		self->vatype->bases = DList_New( DAO_DATA_VALUE );
		DList_Append( self->citype->bases, super->citype );
		DList_Append( self->vatype->bases, super->vatype );
		for(it=DMap_First(super->methods); it; it=DMap_Next( super->methods, it )){
			if( it->value.pRoutine->overloads ){
				DRoutines *routs = it->value.pRoutine->overloads;
				for(k=0,m=routs->routines->size; k<m; k++){
					DaoRoutine *rout = routs->routines->items.pRoutine[i];
					DaoMethods_Insert( self->methods, rout, NULL, self->vatype );
				}
			}else{
				DaoMethods_Insert( self->methods, it->value.pRoutine, NULL, self->vatype );
			}
		}
	}
	self->derived = 1;
}



DaoTypeBase cinTypeTyper =
{
	"CinType", & baseCore, NULL, NULL, {0}, {0},
	(FuncPtrDel) DaoCinType_Delete, NULL
};




DaoCinValue* DaoCinValue_New( DaoCinType *cintype, DaoValue *value )
{
	DaoCinValue *self = (DaoCinValue*)dao_calloc( 1, sizeof(DaoCinValue) );
	DaoValue_Init( self, DAO_CINVALUE );
	GC_Assign( & self->cintype, cintype );
	GC_Assign( & self->value, value );
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}

void DaoCinValue_Delete( DaoCinValue *self )
{
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	GC_DecRC( self->cintype );
	GC_DecRC( self->value );
	dao_free( self );
}


static DaoTypeCore cinValueCore=
{
	NULL,
	DaoValue_GetField,
	DaoValue_SetField,
	DaoValue_GetItem,
	DaoValue_SetItem,
	DaoValue_Print
};

DaoTypeBase cinValueTyper =
{
	"CinValue", & cinValueCore, NULL, NULL, {0}, {0}, (FuncPtrDel) DaoCinValue_Delete, NULL
};

