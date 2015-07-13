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

#include"stdio.h"
#include"string.h"

#include"daoObject.h"
#include"daoClass.h"
#include"daoRoutine.h"
#include"daoProcess.h"
#include"daoVmspace.h"
#include"daoGC.h"
#include"daoStream.h"
#include"daoNumtype.h"
#include"daoValue.h"

void DaoProcess_ShowCallError( DaoProcess *self, DaoRoutine *rout, DaoValue *selfobj, DaoValue *ps[], int np, int code );

int DaoObject_InvokeMethod( DaoObject *self, DaoObject *othis, DaoProcess *proc,
		DString *name, DaoValue *P[], int N, int ignore_return, int execute )
{
	DaoValue *V = NULL;
	DaoValue *O = (DaoValue*)self;
	int errcode = DaoObject_GetData( self, name, &V, othis );
	if( errcode ) return errcode;
	if( V == NULL || V->type != DAO_ROUTINE ) return DAO_ERROR_TYPE;
	if( DaoProcess_PushCallable( proc, (DaoRoutine*) V, O, P, N ) ) goto InvalidParam;
	if( ignore_return ) DaoProcess_InterceptReturnValue( proc );
	if( execute ) DaoProcess_Execute( proc );
	return 0;
InvalidParam:
	DaoProcess_ShowCallError( proc, (DaoRoutine*)V, O, P, N, DVM_CALL );
	return DAO_ERROR_PARAM;
}
static void DaoObject_Print( DaoValue *self0, DaoProcess *proc, DaoStream *stream, DMap *cycData )
{
	int ec = 0;
	char buf[50];
	DaoObject *self = & self0->xObject;
	DaoValue *params[2];
	DaoRoutine *meth;

	sprintf( buf, "[%p]", self );
	if( self0 == self->defClass->objType->value ){
		DaoStream_WriteString( stream, self->defClass->className );
		DaoStream_WriteChars( stream, "[null]" );
		return;
	}
	if( cycData != NULL && DMap_Find( cycData, self ) != NULL ){
		DaoStream_WriteString( stream, self->defClass->className );
		DaoStream_WriteChars( stream, buf );
		return;
	}
	if( cycData ) MAP_Insert( cycData, self, self );

	DaoValue_Clear( & proc->stackValues[0] );

	params[0] = (DaoValue*) dao_type_string;
	params[1] = (DaoValue*) stream;
	meth = DaoClass_FindMethod( self->defClass, "(string)", NULL );
	if( meth ){
		ec = DaoProcess_Call( proc, meth, self0, params, 2 );
		if( ec ) ec = DaoProcess_Call( proc, meth, self0, params, 1 );
	}else{
		meth = DaoClass_FindMethod( self->defClass, "serialize", NULL );
		if( meth ) ec = DaoProcess_Call( proc, meth, self0, NULL, 0 );
	}
	if( ec ){
		DaoProcess_RaiseException( proc, daoExceptionNames[ec], proc->string->chars, NULL );
	}else if( meth && proc->stackValues[0] ){
		DaoValue_Print( proc->stackValues[0], proc, stream, cycData );
	}else{
		DaoStream_WriteString( stream, self->defClass->className );
		DaoStream_WriteChars( stream, buf );
	}
}
static void DaoObject_Core_GetField( DaoValue *self0, DaoProcess *proc, DString *name )
{
	DaoObject *self = & self0->xObject;
	DaoValue *value = NULL;
	int rc = DaoObject_GetData( self, name, & value, proc->activeObject );
	if( rc ){
		DString *field = proc->string;
		DString_SetChars( field, "." );
		DString_Append( field, name );
		rc = DaoObject_InvokeMethod( self, proc->activeObject, proc, field, NULL,0,0,0 );
		if( rc == DAO_ERROR_FIELD_NOTEXIST ){
			DaoString str = {DAO_STRING,0,0,0,1,NULL};
			DaoValue *pars = (DaoValue*) & str;
			str.value = name;
			DString_SetChars( field, "." );
			rc = DaoObject_InvokeMethod( self, proc->activeObject, proc, field, &pars,1,0,0 );
		}
	}else{
		DaoProcess_PutValue( proc, value );
	}
	if( rc ) DaoProcess_RaiseException( proc, daoExceptionNames[rc], name->chars, NULL );
}
static void DaoObject_Core_SetField( DaoValue *self0, DaoProcess *proc, DString *name, DaoValue *value )
{
	DaoObject *self = & self0->xObject;
	int ec = DaoObject_SetData( self, name, value, proc->activeObject );
	int ec2 = ec;
	if( ec != DAO_ERROR ){
		DString *mbs = proc->string;
		DString_SetChars( mbs, "." );
		DString_Append( mbs, name );
		DString_AppendChars( mbs, "=" );
		ec = DaoObject_InvokeMethod( self, proc->activeObject, proc, mbs, & value, 1,1,0 );
		if( ec == DAO_ERROR_FIELD_NOTEXIST ){
			DaoString str = {DAO_STRING,0,0,0,1,NULL};
			DaoValue *pars[2];
			pars[0] = (DaoValue*) & str;
			pars[1] = value;
			str.value = name;
			DString_SetChars( mbs, ".=" );
			ec = DaoObject_InvokeMethod( self, proc->activeObject, proc, mbs, pars,2,1,0 );
		}
		if( ec == DAO_ERROR_FIELD_NOTEXIST ) ec = ec2;
	}
	if( ec ) DaoProcess_RaiseException( proc, daoExceptionNames[ec], name->chars, NULL );
}
static void DaoObject_GetItem( DaoValue *self0, DaoProcess *proc, DaoValue *ids[], int N )
{
	DaoObject *self = & self0->xObject;
	int rc = 0;
	DString_SetChars( proc->string, "[]" );
	rc = DaoObject_InvokeMethod( self, proc->activeObject, proc, proc->string, ids, N,0,0 );
	if( rc ) DaoProcess_RaiseException( proc, daoExceptionNames[rc], proc->string->chars, NULL );
}
static void DaoObject_SetItem( DaoValue *self0, DaoProcess *proc, DaoValue *ids[], int N, DaoValue *value )
{
	DaoObject *self = & self0->xObject;
	DaoValue *ps[ DAO_MAX_PARAM ];
	int rc;
	memcpy( ps+1, ids, N*sizeof(DaoValue*) );
	ps[0] = value;
	DString_SetChars( proc->string, "[]=" );
	rc = DaoObject_InvokeMethod( self, proc->activeObject, proc, proc->string, ps, N+1,1,0 );
	if( rc ) DaoProcess_RaiseException( proc, daoExceptionNames[rc], proc->string->chars, NULL );
}

static DaoTypeCore objCore =
{
	NULL,
	DaoObject_Core_GetField,
	DaoObject_Core_SetField,
	DaoObject_GetItem,
	DaoObject_SetItem,
	DaoObject_Print
};

DaoTypeBase objTyper=
{
	"object", & objCore, NULL, NULL, {0}, {0},
	(FuncPtrDel) DaoObject_Delete, NULL
};

DaoObject* DaoObject_Allocate( DaoClass *klass, int value_count )
{
	int extra = value_count * sizeof(DaoValue*);
	DaoObject *self = (DaoObject*) dao_calloc( 1, sizeof(DaoObject) + extra );

	DaoValue_Init( self, DAO_OBJECT );
	GC_IncRC( klass );
	self->defClass = klass;
	self->isRoot = 1;
	self->valueCount = value_count;
	self->objValues = (DaoValue**) (self + 1);
	memset( self->objValues, 0, value_count*sizeof(DaoValue*) );
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
DaoObject* DaoObject_New( DaoClass *klass )
{
	DaoObject *self = DaoObject_Allocate( klass, klass->objDataName->size );
	GC_IncRC( self );
	self->rootObject = self;
	DaoObject_Init( self, NULL, 0 );
	return self;
}
void DaoObject_Init( DaoObject *self, DaoObject *that, int offset )
{
	DaoClass *klass = self->defClass;
	daoint i;

	self->isAsync = (klass->attribs & DAO_CLS_ASYNCHRONOUS) != 0;
	if( that ){
		GC_Assign( & self->rootObject, that );
		self->objValues = that->objValues + offset;
	}else if( self->rootObject == NULL ){
		GC_Assign( & self->rootObject, self );
		if( self->isNull ){ /* no value space is allocated for null object yet! */
			self->valueCount = klass->objDataName->size;
			self->objValues = (DaoValue**) dao_calloc( self->valueCount, sizeof(DaoValue*) );
		}
	}
	offset += self->defClass->objDefCount;
	if( klass->parent != NULL && klass->parent->type == DAO_CLASS ){
		DaoObject *sup = NULL;
		if( self->isNull ){
			sup = & klass->parent->xClass.objType->value->xObject;
		}else{
			sup = DaoObject_Allocate( (DaoClass*) klass->parent, 0 );
			sup->isRoot = 0;
			DaoObject_Init( sup, self->rootObject, offset );
		}
		GC_Assign( & self->parent, sup );
	}
	GC_Assign( & self->objValues[0], self );
	if( self->isRoot == 0 ) return;
	for(i=1; i<klass->instvars->size; i++){
		DaoVariable *var = klass->instvars->items.pVar[i];
		DaoValue **value = self->objValues + i;
		/* for data type such as list/map/array,
		 * its .ctype may need to be set properaly */
		if( var->value ){
			DaoValue_Move( var->value, value, var->dtype );
		}else if( *value == NULL && var->dtype && var->dtype->value ){
			DaoValue_Copy( var->dtype->value, value );
		}
	}
}
void DaoObject_Delete( DaoObject *self )
{
	int i;
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	GC_DecRC( self->defClass );
	GC_DecRC( self->parent );
	if( self->isRoot ){
		for(i=0; i<self->valueCount; i++) GC_DecRC( self->objValues[i] );
		if( self->objValues != (DaoValue**) (self + 1) ) dao_free( self->objValues );
	}
	dao_free( self );
}

int DaoObject_ChildOf( DaoValue *self, DaoValue *obj )
{
	int i;
	if( obj == self ) return 1;
	if( self->type == DAO_CDATA || self->type == DAO_CSTRUCT ){
		if( obj->type == DAO_CDATA || obj->type == DAO_CSTRUCT ){
			DaoCstruct *cdata1 = (DaoCstruct*) self;
			DaoCstruct *cdata2 = (DaoCstruct*) obj;
			if( DaoType_ChildOf( cdata1->ctype, cdata2->ctype ) ) return 1;
		}
		return 0;
	}
	if( self->type != DAO_OBJECT || self->xObject.parent == NULL ) return 0;
	if( obj == self->xObject.parent ) return 1;
	if( DaoObject_ChildOf( self->xObject.parent, obj ) ) return 1;
	return 0;
}

DaoValue* DaoObject_CastToBase( DaoObject *self, DaoType *host )
{
	DaoValue *sup = self->parent;
	if( host == NULL ) return NULL;
	host = DaoType_GetBaseType( host );
	if( self->defClass->objType == host ) return (DaoValue*) self;
	if( self->parent == NULL ) return NULL;
	if( sup->type == DAO_OBJECT ){
		if( (sup = DaoObject_CastToBase( & sup->xObject, host ) ) ) return sup;
	}else if( sup->type == DAO_CSTRUCT && host->tid == DAO_CSTRUCT ){
		if( DaoType_ChildOf( sup->xCdata.ctype, host ) ) return sup;
	}else if( sup->type == DAO_CDATA && host->tid == DAO_CDATA ){
		if( DaoType_ChildOf( sup->xCdata.ctype, host ) ) return sup;
	}
	return NULL;
}
void DaoObject_SetParentCdata( DaoObject *self, DaoCdata *parent )
{
	DaoObject *child = NULL;
	DaoObject *obj = (DaoObject*) self->parent;
	DaoValue *sup = self->defClass->parent;
	if( parent == NULL ) return;
	if( sup == NULL ) return;
	if( obj && obj->type == DAO_OBJECT ){
		DaoObject_SetParentCdata( obj, parent );
	}else if( sup->type == DAO_CTYPE ){
		DaoCdata *cdata = (DaoCdata*)sup;
		if( DaoType_ChildOf( cdata->ctype, parent->ctype ) ){
			GC_Assign( & self->parent, parent );
		}
	}
}
DaoCstruct* DaoObject_CastCstruct( DaoObject *self, DaoType *type )
{
	DaoValue *p = NULL;
	if( type ) p = DaoObject_CastToBase( self, type );
	if( p && (p->type == DAO_CDATA || p->type == DAO_CSTRUCT) ) return (DaoCstruct*) p;
	return NULL;
}
DaoCdata* DaoObject_CastCdata( DaoObject *self, DaoType *type )
{
	DaoCstruct *p = DaoObject_CastCstruct( self, type );
	if( p && p->type == DAO_CDATA ) return (DaoCdata*) p;
	return NULL;
}

void DaoObject_AddData( DaoObject *self, DString *name, DaoValue *data )
{
}
int DaoObject_SetData( DaoObject *self, DString *name, DaoValue *data, DaoObject *othis )
{
	DNode *node;
	DaoType *type;
	DaoValue **value ;
	DaoClass *klass = self->defClass;
	DaoObject *null = & klass->objType->value->xObject;
	int child = othis && DaoObject_ChildOf( (DaoValue*)othis, (DaoValue*)self );
	int id, st, up, pm, access;

	if( self == (DaoObject*)self->defClass->objType->value ) return DAO_ERROR;

	node = DMap_Find( self->defClass->lookupTable, name );
	if( node == NULL ) return DAO_ERROR_FIELD_NOTEXIST;

	pm = LOOKUP_PM( node->value.pInt );
	st = LOOKUP_ST( node->value.pInt );
	up = LOOKUP_UP( node->value.pInt );
	id = LOOKUP_ID( node->value.pInt );
	if( self == null && st == DAO_OBJECT_VARIABLE ) return DAO_ERROR_FIELD_NOTPERMIT;
	access = othis == self || pm == DAO_PERM_PUBLIC || (child && pm >= DAO_PERM_PROTECTED);
	if( access == 0 ) return DAO_ERROR_FIELD_NOTPERMIT;
	if( st == DAO_OBJECT_VARIABLE ){
		if( id <0 ) return DAO_ERROR_FIELD_NOTPERMIT;
		type = klass->instvars->items.pVar[ id ]->dtype;
		value = self->objValues + id;
		if( DaoValue_Move( data, value, type ) ==0 ) return DAO_ERROR_VALUE;
	}else if( st == DAO_CLASS_VARIABLE ){
		DaoVariable *var = klass->variables->items.pVar[id];
		if( DaoValue_Move( data, & var->value, var->dtype ) ==0 ) return DAO_ERROR_VALUE;
	}else if( st == DAO_CLASS_CONSTANT ){
		return DAO_ERROR_FIELD;
	}else{
		return DAO_ERROR_FIELD;
	}
	return 0;
}
int DaoObject_GetData( DaoObject *self, DString *name, DaoValue **data, DaoObject *othis )
{
	DNode *node;
	DaoValue *p = NULL;
	DaoClass *klass = self->defClass;
	DaoObject *null = & klass->objType->value->xObject;
	int child = othis && DaoObject_ChildOf( (DaoValue*)othis, (DaoValue*)self );
	int id, st, up, pm, access;

	*data = NULL;
	node = DMap_Find( self->defClass->lookupTable, name );
	if( node == NULL ) return DAO_ERROR_FIELD_NOTEXIST;

	pm = LOOKUP_PM( node->value.pInt );
	st = LOOKUP_ST( node->value.pInt );
	up = LOOKUP_UP( node->value.pInt );
	id = LOOKUP_ID( node->value.pInt );
	if( self == null && st == DAO_OBJECT_VARIABLE ) return DAO_ERROR_FIELD_NOTPERMIT;
	access = othis == self || pm == DAO_PERM_PUBLIC || (child && pm >= DAO_PERM_PROTECTED);
	if( access == 0 ) return DAO_ERROR_FIELD_NOTPERMIT;
	switch( st ){
	case DAO_OBJECT_VARIABLE : p = self->objValues[id]; break;
	case DAO_CLASS_VARIABLE  : p = klass->variables->items.pVar[id]->value; break;
	case DAO_CLASS_CONSTANT  : p = klass->constants->items.pConst[id]->value; break;
	default : break;
	}
	*data = p;
	return 0;
}

DaoValue* DaoObject_GetField( DaoObject *self, const char *name )
{
	DaoValue *res = NULL;
	DString str = DString_WrapChars( name );
	DaoObject_GetData( self, & str, & res, self );
	return res;
}
DaoRoutine* DaoObject_GetMethod( DaoObject *self, const char *name )
{
	DaoValue *V;
	DString str = DString_WrapChars( name );
	int id = DaoClass_FindConst( self->defClass, & str );
	if( id < 0 ) return NULL;
	V = DaoClass_GetConst( self->defClass, id );
	if( V == NULL || V->type != DAO_ROUTINE ) return NULL;
	return (DaoRoutine*) V;
}
