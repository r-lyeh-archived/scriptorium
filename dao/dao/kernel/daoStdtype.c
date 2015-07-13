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
#include"math.h"
#include"locale.h"

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
#include"daoTasklet.h"
#include"daoValue.h"


void DaoValue_Init( void *value, char type )
{
	DaoNone *self = (DaoNone*) value;
	self->type = type;
	self->subtype = self->trait = self->marks = 0;
	self->refCount = 0;
	if( type >= DAO_ENUM ) ((DaoValue*)self)->xGC.cycRefCount = 0;
}

DaoNone* DaoNone_New()
{
	DaoNone *self = (DaoNone*) dao_malloc( sizeof(DaoNone) );
	memset( self, 0, sizeof(DaoNone) );
	self->type = DAO_NONE;
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}

DaoBoolean* DaoBoolean_New( dao_boolean value )
{
	DaoBoolean *self = (DaoBoolean*) dao_malloc( sizeof(DaoBoolean) );
	DaoValue_Init( self, DAO_BOOLEAN );
	self->value = value != 0;
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
dao_boolean DaoBoolean_Get( DaoBoolean *self )
{
	return self->value;
}
void DaoBoolean_Set( DaoBoolean *self, dao_boolean value )
{
	self->value = value != 0;
}

DaoInteger* DaoInteger_New( dao_integer value )
{
	DaoInteger *self = (DaoInteger*) dao_malloc( sizeof(DaoInteger) );
	DaoValue_Init( self, DAO_INTEGER );
	self->value = value;
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
dao_integer DaoInteger_Get( DaoInteger *self )
{
	return self->value;
}
void DaoInteger_Set( DaoInteger *self, dao_integer value )
{
	self->value = value;
}

DaoFloat* DaoFloat_New( dao_float value )
{
	DaoFloat *self = (DaoFloat*) dao_malloc( sizeof(DaoFloat) );
	DaoValue_Init( self, DAO_FLOAT );
	self->value = value;
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
dao_float DaoFloat_Get( DaoFloat *self )
{
	return self->value;
}
void DaoFloat_Set( DaoFloat *self, dao_float value )
{
	self->value = value;
}


DaoComplex* DaoComplex_New( dao_complex value )
{
	DaoComplex *self = (DaoComplex*) dao_malloc( sizeof(DaoComplex) );
	DaoValue_Init( self, DAO_COMPLEX );
	self->value = value;
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
DaoComplex* DaoComplex_New2( dao_float real, dao_float imag )
{
	DaoComplex *self = (DaoComplex*) dao_malloc( sizeof(DaoComplex) );
	DaoValue_Init( self, DAO_COMPLEX );
	self->value.real = real;
	self->value.imag = imag;
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
dao_complex  DaoComplex_Get( DaoComplex *self )
{
	return self->value;
}
void DaoComplex_Set( DaoComplex *self, dao_complex value )
{
	self->value = value;
}


DaoString* DaoString_New()
{
	DaoString *self = (DaoString*) dao_malloc( sizeof(DaoString) );
	DaoValue_Init( self, DAO_STRING );
	self->value = DString_New();
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
DaoString* DaoString_NewChars( const char *mbs )
{
	DaoString *self = DaoString_New();
	DString_SetChars( self->value, mbs );
	return self;
}
DaoString* DaoString_NewBytes( const char *bytes, daoint n )
{
	DaoString *self = DaoString_New();
	DString_SetBytes( self->value, bytes, n );
	return self;
}
DaoString* DaoString_Copy( DaoString *self )
{
	DaoString *copy = (DaoString*) dao_malloc( sizeof(DaoString) );
	DaoValue_Init( copy, DAO_STRING );
	copy->value = DString_Copy( self->value );
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) copy );
#endif
	return copy;
}
void DaoString_Delete( DaoString *self )
{
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	DString_Delete( self->value );
	dao_free( self );
}
daoint  DaoString_Size( DaoString *self )
{
	return self->value->size;
}
DString* DaoString_Get( DaoString *self )
{
	return self->value;
}
const char* DaoString_GetChars( DaoString *self )
{
	return DString_GetData( self->value );
}

void DaoString_Set( DaoString *self, DString *str )
{
	DString_Assign( self->value, str );
}
void DaoString_SetChars( DaoString *self, const char *mbs )
{
	DString_SetChars( self->value, mbs );
}
void DaoString_SetBytes( DaoString *self, const char *bytes, daoint n )
{
	DString_SetBytes( self->value, bytes, n );
}

enum{
	IDX_NULL,
	IDX_EMPTY,
	IDX_SINGLE,
	IDX_FROM,
	IDX_TO,
	IDX_PAIR,
	IDX_ALL,
	IDX_NONUMINDEX,
	IDX_OUTOFRANGE
};


static void MakeIndex( DaoProcess *proc, DaoValue *index, daoint N, daoint *start, daoint *end, int *idtype )
{
	daoint n1, n2;
	DaoValue *first, *second;

	*idtype = IDX_NULL;
	*start = 0;
	*end = N - 1;
	if( index == NULL ) return;

	switch( index->type ){
	case DAO_INTEGER :
		*idtype = IDX_SINGLE;
		n1 = index->xInteger.value;
		if( n1 <0 ) n1 += N;
		if( n1 <0 || n1 >= N ) *idtype = IDX_OUTOFRANGE;
		*start = n1;
		*end = n1;
		break;
	case DAO_FLOAT :
		*idtype = IDX_SINGLE;
		n1 = (daoint)(index->xFloat.value);
		if( n1 <0 ) n1 += N;
		if( n1 <0 || n1 >= N ) *idtype = IDX_OUTOFRANGE;
		*start = n1;
		*end = n1;
		break;
	case DAO_TUPLE:
		*idtype = IDX_PAIR;
		if( index->xTuple.ctype == dao_type_for_iterator ){
			DaoValue **data = index->xTuple.values;
			if( data[0]->type == DAO_BOOLEAN && data[1]->type == DAO_INTEGER ){
				*start = data[1]->xInteger.value;
				*idtype = *start < N ? IDX_SINGLE : IDX_OUTOFRANGE;
				data[1]->xInteger.value += 1;
				data[0]->xInteger.value = data[1]->xInteger.value < N;
				break;
			}
		}
		if( index->xTuple.subtype != DAO_PAIR ){
			*idtype = IDX_OUTOFRANGE;
			break;
		}
		first = index->xTuple.values[0];
		second = index->xTuple.values[1];
		/* a[ : 1 ] ==> pair(nil,int) */
		if( first->type > DAO_FLOAT || second->type > DAO_FLOAT ){
			*idtype = IDX_NONUMINDEX;
			break;
		}
		n1 = DaoValue_GetInteger( first );
		n2 = DaoValue_GetInteger( second );
		if( n1 <0 ) n1 += N;
		if( n2 <0 ) n2 += N;
		*start = n1;
		*end = n2;
		if( first->type ==DAO_NONE && second->type ==DAO_NONE ){
			*idtype = IDX_ALL;
		}else if( first->type ==DAO_NONE ){
			*idtype = IDX_TO;
		}else if( second->type ==DAO_NONE ){
			*idtype = IDX_FROM;
		}
		/* when specify an index range, allow out of range: (eg, str[:5]=='abcde') */
		if( n1 >= N || n2 < 0 ){
			*idtype = IDX_EMPTY;
		}else if( n1 < 0 ){
			*idtype = IDX_TO;
		}else if( n2 >= N ){
			*idtype = IDX_FROM;
		}
		break;
	default : break;
	}
	switch( *idtype ){
	case IDX_NONUMINDEX : DaoProcess_RaiseError( proc, "Index", "need number" ); break;
	case IDX_OUTOFRANGE : DaoProcess_RaiseError( proc, "Index::Range", NULL ); break;
	}
}



DaoTypeCore baseCore =
{
	NULL,
	DaoValue_GetField,
	DaoValue_SetField,
	DaoValue_GetItem,
	DaoValue_SetItem,
	DaoValue_Print
};
DaoTypeBase baseTyper =
{
	"none", & baseCore, NULL, NULL, {0}, {0}, dao_free, NULL
};
static DaoNone none = {DAO_NONE,0,DAO_VALUE_CONST,0,1};
static DaoBoolean dao_false = {DAO_BOOLEAN,0,DAO_VALUE_CONST,0,1,0};
static DaoBoolean dao_true = {DAO_BOOLEAN,0,DAO_VALUE_CONST,0,1,1};
DaoValue *dao_none_value = (DaoValue*) (void*) & none;
DaoValue *dao_false_value = (DaoValue*) (void*) & dao_false;
DaoValue *dao_true_value = (DaoValue*) (void*) & dao_true;



DaoEnum* DaoEnum_New( DaoType *type, int value )
{
	DaoEnum *self = (DaoEnum*) dao_malloc( sizeof(DaoEnum) );
	DaoValue_Init( self, DAO_ENUM );
	self->subtype = type ? type->subtid : DAO_ENUM_SYM;
	self->value = value;
	self->etype = type;
	if( type ) GC_IncRC( type );
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
DaoEnum* DaoEnum_Copy( DaoEnum *self, DaoType *type )
{
	DaoEnum *copy = DaoEnum_New( self->etype, self->value );
	copy->subtype = self->subtype;
	if( self->etype != type && type ){
		DaoEnum_SetType( copy, type );
		DaoEnum_SetValue( copy, self );
	}
	return copy;
}
void DaoEnum_Delete( DaoEnum *self )
{
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	if( self->etype ) GC_DecRC( self->etype );
	dao_free( self );
}
void DaoEnum_MakeName( DaoEnum *self, DString *name )
{
	DMap *mapNames;
	DNode *node;
	DString_Clear( name );
	mapNames = self->etype->mapNames;
	for(node=DMap_First(mapNames); node; node=DMap_Next(mapNames,node)){
		if( self->subtype == DAO_ENUM_FLAG ){
			if( (node->value.pInt & self->value) != node->value.pInt ) continue;
		}else if( node->value.pInt != self->value ){
			continue;
		}
		DString_AppendChar( name, '$' );
		DString_Append( name, node->key.pString );
	}
}
void DaoEnum_SetType( DaoEnum *self, DaoType *type )
{
	type = DaoType_GetBaseType( type );
	if( self->etype == type ) return;
	GC_Assign( & self->etype, type );
	self->subtype = type->subtid;
	self->value = type->mapNames->root->value.pInt;
}
int DaoEnum_SetSymbols( DaoEnum *self, const char *symbols )
{
	DString *names;
	daoint first = 0;
	daoint value = 0;
	int notfound = 0;
	int i, n, k = 0;

	names = DString_New();
	DString_SetChars( names, symbols );
	for(i=0,n=names->size; i<n; i++){
		char ch = names->chars[i];
		if( ch == '$' || ch == ',' || ch == ';' ) names->chars[i] = 0;
	}
	i = 0;
	if( names->chars[0] == '\0' ) i += 1;
	do{ /* for multiple symbols */
		DString name = DString_WrapChars( names->chars + i );
		DNode *node = DMap_Find( self->etype->mapNames, &name );
		if( node ){
			if( ! k ) first = node->value.pInt;
			value |= node->value.pInt;
			k += 1;
		}else{
			notfound = 1;
		}
		i += name.size + 1;
	} while( i < names->size );
	DString_Delete( names );
	if( k == 0 ) return 0;
	if( self->subtype == DAO_ENUM_STATE && k > 1 ){
		self->value = first;
		return 0;
	}
	self->value = value;
	return notfound == 0;
}
int DaoEnum_SetValue( DaoEnum *self, DaoEnum *other )
{
	DMap *selfNames = self->etype->mapNames;
	DMap *otherNames = other->etype->mapNames;
	DNode *node, *search;
	int ret = 0;

	if( self->etype == other->etype ){
		self->value = other->value;
		return 1;
	}
	if( self->subtype == DAO_ENUM_SYM ) return 0;

	self->value = 0;
	if( other->subtype == DAO_ENUM_STATE || other->subtype == DAO_ENUM_SYM ){
		for(node=DMap_First(otherNames); node; node=DMap_Next(otherNames,node)){
			if( node->value.pInt != other->value ) continue;
			search = DMap_Find( selfNames, node->key.pVoid );
			if( search == NULL ) return 0;
			self->value |= search->value.pInt;
			ret += 1;
		}
		/* State or bool enums are supposed to have only one symbol; */
		ret = ret == 1;
	}else{
		for(node=DMap_First(otherNames); node; node=DMap_Next(otherNames,node)){
			if( (node->value.pInt & other->value) != node->value.pInt ) continue;
			search = DMap_Find( selfNames, node->key.pVoid );
			if( search == NULL ) return 0;
			self->value |= search->value.pInt;
			ret += 1;
		}
		if( self->subtype == DAO_ENUM_STATE ) ret = ret==1;
	}
	return ret;
}
int DaoEnum_AddValue( DaoEnum *self, DaoEnum *other )
{
	DMap *selfNames = self->etype->mapNames;
	DMap *otherNames = other->etype->mapNames;
	DNode *node, *search;

	if( self->subtype != DAO_ENUM_FLAG ) return 0;

	if( self->etype == other->etype ){
		self->value |= other->value;
		return 1;
	}

	for(node=DMap_First(otherNames); node; node=DMap_Next(otherNames,node)){
		if( other->subtype == DAO_ENUM_FLAG ){
			if( (node->value.pInt & other->value) != node->value.pInt ) continue;
		}else{
			if( node->value.pInt != other->value ) continue;
		}
		search = DMap_Find( selfNames, node->key.pVoid );
		if( search == NULL ) return 0;
		self->value |= search->value.pInt;
	}
	return other->subtype == DAO_ENUM_SYM;
}
int DaoEnum_RemoveValue( DaoEnum *self, DaoEnum *other )
{
	DMap *selfNames = self->etype->mapNames;
	DMap *otherNames = other->etype->mapNames;
	DNode *node, *search;

	if( self->subtype != DAO_ENUM_FLAG ) return 0;

	if( self->etype == other->etype ){
		self->value &= ~ other->value;
		return 1;
	}

	for(node=DMap_First(otherNames); node; node=DMap_Next(otherNames,node)){
		if( other->subtype == DAO_ENUM_FLAG ){
			if( (node->value.pInt & other->value) != node->value.pInt ) continue;
		}else{
			if( node->value.pInt != other->value ) continue;
		}
		search = DMap_Find( selfNames, node->key.pVoid );
		if( search == NULL ) return 0;
		self->value &= ~search->value.pInt;
	}
	return other->subtype == DAO_ENUM_SYM;
}

DaoTypeBase enumTyper =
{
	"enum", & baseCore, NULL, NULL, {0}, {0},
	(FuncPtrDel) DaoEnum_Delete, NULL
};



/**/
static void DaoNumber_Print( DaoValue *self, DaoProcess *proc, DaoStream *stream, DMap *cycData )
{
	switch( self->type ){
	case DAO_INTEGER : DaoStream_WriteInt( stream, self->xInteger.value ); break;
	case DAO_FLOAT   : DaoStream_WriteFloat( stream, self->xFloat.value ); break;
	}
}

static DaoTypeCore numberCore=
{
	NULL,
	DaoValue_GetField,
	DaoValue_SetField,
	DaoValue_GetItem,
	DaoValue_SetItem,
	DaoNumber_Print
};

static void DaoNumer_Delete( DaoValue *self )
{
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	dao_free( self );
}

DaoTypeBase numberTyper=
{
	"float", & numberCore, NULL, NULL, {0}, {0}, (FuncPtrDel) DaoNumer_Delete, NULL
};

/**/
static void DaoString_Print( DaoValue *self, DaoProcess *proc, DaoStream *stream, DMap *cycData )
{
	DaoStream_WriteString( stream, self->xString.value );
}
static void DaoString_GetItem1( DaoValue *self0, DaoProcess *proc, DaoValue *pid )
{
	DString *self = self0->xString.value;
	daoint size = DString_Size( self );
	daoint start, end;
	DString *res = NULL;
	dao_integer *num = NULL;
	int idtype;

	MakeIndex( proc, pid, size, & start, & end, & idtype );
	if( idtype == IDX_SINGLE ){
		num = DaoProcess_PutInteger( proc, 0 );
	}else{
		res = DaoProcess_PutChars( proc, "" );
	}
	switch( idtype ){
	case IDX_EMPTY  : break;
	case IDX_NULL   : DString_Assign( res, self ); break;
	case IDX_SINGLE : *num = (uchar_t) self->chars[start]; break;
	case IDX_FROM   : DString_SubString( self, res, start, -1 ); break;
	case IDX_TO     : DString_SubString( self, res, 0, end+1 ); break;
	case IDX_PAIR   : DString_SubString( self, res, start, end-start+1 ); break;
	case IDX_ALL    : DString_SubString( self, res, 0, -1 ); break;
	default : break;
	}
}
static void DaoString_GetItem2( DaoValue *self0, DaoProcess *proc, DaoValue *pid1, DaoValue *pid2 )
{
	DString *self = self0->xString.value;
	DString *res = DaoProcess_PutChars( proc, "" );
	daoint i, j, valid;

	valid = pid1->type > DAO_NONE && pid1->type <= DAO_FLOAT && pid2->type == DAO_NONE;
	if( valid == 0 ){
		DaoProcess_RaiseError( proc, "Index", NULL );
		return;
	}
	i = DaoValue_GetInteger( pid1 );
	if( i < 0 ) i += self->size;
	if( i < 0 || i >= self->size ){
		DaoProcess_RaiseError( proc, "Index::Range", NULL );
		return;
	}
	j = DString_LocateChar( self, i, 0 );
	if( i != j ){
		DaoProcess_RaiseError( proc, "Index", NULL );
		return;
	}
	DString_SetBytes( res, self->chars + i, DString_UTF8CharSize( self->chars[i] ) );
}
static void DaoString_SetItem1( DaoValue *self0, DaoProcess *proc, DaoValue *pid, DaoValue *value )
{
	DString *self = self0->xString.value;
	daoint size = DString_Size( self );
	daoint start, end;
	int idtype;
	MakeIndex( proc, pid, size, & start, & end, & idtype );
	DString_Detach( self, self->size );
	if( value->type >= DAO_INTEGER && value->type <= DAO_FLOAT ){
		daoint i, id = value->xInteger.value;
		for(i=start; i<=end; i++) self->chars[i] = id;
	}else if( value->type == DAO_STRING ){
		DString *str = value->xString.value;
		switch( idtype ){
		case IDX_EMPTY  : break;
		case IDX_NULL   : DString_Assign( self, str ); break;
		case IDX_SINGLE : self->chars[start] = str->chars[0]; break;
		case IDX_FROM   : DString_Replace( self, str, start, -1 ); break;
		case IDX_TO     : DString_Replace( self, str, 0, end+1 ); break;
		case IDX_PAIR   : DString_Replace( self, str, start, end-start+1 ); break;
		case IDX_ALL    : DString_Assign( self, str ); break;
		default : break;
		}
	}
}
static void DaoString_GetItem( DaoValue *self, DaoProcess *proc, DaoValue *ids[], int N )
{
	switch( N ){
	case 0 : DaoString_GetItem1( self, proc, NULL ); break;
	case 1 : DaoString_GetItem1( self, proc, ids[0] ); break;
	case 2 : DaoString_GetItem2( self, proc, ids[0], ids[1] ); break;
	default : DaoProcess_RaiseError( proc, "Index", "not supported" );
	}
}
static void DaoString_SetItem( DaoValue *self, DaoProcess *proc, DaoValue *ids[], int N, DaoValue *value )
{
	switch( N ){
	case 0 : DaoString_SetItem1( self, proc, NULL, value ); break;
	case 1 : DaoString_SetItem1( self, proc, ids[0], value ); break;
	default : DaoProcess_RaiseError( proc, "Index", "not supported" );
	}
}
static DaoTypeCore stringCore=
{
	NULL,
	DaoValue_GetField,
	DaoValue_SetField,
	DaoString_GetItem,
	DaoString_SetItem,
	DaoString_Print
};

static void DaoSTR_New( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *res = DaoProcess_PutChars( proc, "" );
	daoint i, count = p[0]->xInteger.value;
	size_t width, ch = p[1]->xInteger.value;
	char buffer[8];

	DString_AppendWChar( res, ch );
	width = res->size;
	memcpy( buffer, res->chars, width );
	DString_Resize( res, count * width );
	if( width == 1 ){
		memset( res->chars, buffer[0], res->size );
	}else{
		for(i=0; i<count; ++i) memcpy( res->chars + i * width, buffer, width );
	}
}
static void DaoSTR_New2( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoInteger idint = {DAO_INTEGER,0,0,0,0,0};
	DaoValue *index = (DaoValue*)(void*)&idint;
	DString *string = DaoProcess_PutChars( proc, "" );
	daoint i, entry, size = p[0]->xInteger.value;
	DaoVmCode *sect;

	if( size < 0 ){
		DaoProcess_RaiseError( proc, "Param", "Invalid parameter value" );
		return;
	}
	if( size == 0 ) return;
	sect = DaoProcess_InitCodeSection( proc, 1 );
	if( sect == NULL ) return;
	entry = proc->topFrame->entry;
	for(i=0; i<size; i++){
		idint.value = i;
		if( sect->b >0 ) DaoProcess_SetValue( proc, sect->a, index );
		proc->topFrame->entry = entry;
		DaoProcess_Execute( proc );
		if( proc->status == DAO_PROCESS_ABORTED ) break;
		DString_Append( string, proc->stackValues[0]->xString.value );
	}
	DaoProcess_PopFrame( proc );
}
static void DaoSTR_Size( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *self = p[0]->xString.value;
	int chars = p[1]->xBoolean.value;
	DaoProcess_PutInteger( proc, chars ? DString_GetCharCount( self ) : self->size );
}

static void DaoSTR_Insert( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *self = p[0]->xString.value;
	DString *str = p[1]->xString.value;
	DString *res = DaoProcess_PutString( proc, self );
	daoint pos = p[2]->xInteger.value;
	if( pos < 0 ) pos += self->size;
	if( pos < 0 || pos > self->size ){
		DaoProcess_RaiseError( proc, "Index::Range", NULL );
		return;
	}
	DString_Insert( res, str, pos, 0, -1 );
}
static void DaoSTR_Erase( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *self = p[0]->xString.value;
	DString *res = DaoProcess_PutString( proc, self );
	daoint pos = p[1]->xInteger.value;
	daoint count = p[2]->xInteger.value;
	if( pos < 0 ) pos += self->size;
	if( pos < 0 || pos > self->size ){
		DaoProcess_RaiseError( proc, "Index::Range", NULL );
		return;
	}
	DString_Erase( res, pos, count );
}
static void DaoSTR_Chop( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *res = DaoProcess_PutString( proc, p[0]->xString.value );
	daoint utf8 = p[1]->xBoolean.value;
	DString_Chop( res, utf8 );
}
static void DaoSTR_Trim( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *res = DaoProcess_PutString( proc, p[0]->xString.value );
	daoint head = p[1]->xEnum.value & 0x1;
	daoint tail = p[1]->xEnum.value & 0x2;
	daoint utf8 = p[2]->xBoolean.value;
	DString_Trim( res, head, tail, utf8 );
}
static void DaoSTR_Find( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *self = p[0]->xString.value;
	DString *str = p[1]->xString.value;
	daoint from = p[2]->xInteger.value;
	daoint pos = DAO_NULLPOS;
	if( p[3]->xBoolean.value ){
		pos = DString_RFind( self, str, from );
	}else{
		pos = DString_Find( self, str, from );
	}
	DaoProcess_PutInteger( proc, pos );
}
static void DaoSTR_Contains( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *self = p[0]->xString.value;
	int i, res = 1;
	for(i=1; i<N; ++i){
		DaoTuple *tuple = (DaoTuple*) p[i];
		DString *keyword = tuple->values[1]->xString.value;
		daoint pos = 0, len = keyword->size;
		if( tuple->values[0]->type == DAO_INTEGER ){
			pos = tuple->values[0]->xInteger.value;
			if( pos < 0 ) pos += self->size - (len - 1);
		}else{ /* DAO_ENUM: */
			pos = tuple->values[0]->xEnum.value == 0 ? 0 : self->size - len;
		}
		if( pos < 0 || (self->size - pos) < len ){
			res = 0;
			break;
		}
		res = res && memcmp( self->chars + pos, keyword->chars, len ) == 0;
		if( res == 0 ) break;
	}
	DaoProcess_PutBoolean( proc, res );
}
static void DaoSTR_Replace( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *res = DaoProcess_PutChars( proc, "" );
	DString *self = p[0]->xString.value;
	DString *str1 = p[1]->xString.value;
	DString *str2 = p[2]->xString.value;
	daoint index = p[3]->xInteger.value;
	daoint pos, offset = 0, count = 0;

	DString_Reserve( res, self->size + (str2->size - str1->size) );
	if( index == 0 ){
		pos = DString_Find( self, str1, offset );
		if( pos == DAO_NULLPOS ) pos = self->size;
		while( offset < self->size ){
			count += pos < self->size;
			DString_AppendBytes( res, self->chars + offset, pos - offset );
			if( pos < self->size ) DString_Append( res, str2 );
			offset = pos + str1->size;
			pos = DString_Find( self, str1, offset );
			if( pos == DAO_NULLPOS ) pos = self->size;
		}
	}else if( index > 0){
		pos = DString_Find( self, str1, offset );
		while( pos != DAO_NULLPOS ){
			count ++;
			offset = pos + str1->size;
			if( count == index ){
				DString_AppendBytes( res, self->chars, pos );
				DString_Append( res, str2 );
				DString_AppendBytes( res, self->chars + offset, self->size - offset  );
				break;
			}
			pos = DString_Find( self, str1, offset );
		}
		if( count != index ) DString_Assign( res, self );
	}else{
		offset = DAO_NULLPOS;
		pos = DString_RFind( self, str1, offset );
		while( pos != DAO_NULLPOS ){
			count --;
			offset = pos - str1->size;
			if( count == index ){
				DString_AppendBytes( res, self->chars, offset + 1 );
				DString_Append( res, str2 );
				DString_AppendBytes( res, self->chars + pos + 1, self->size - pos - 1 );
				break;
			}
			pos = DString_RFind( self, str1, offset );
		}
		if( count != index ) DString_Assign( res, self );
	}
}
static void DaoSTR_Expand( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoString *key = NULL;
	DString *self = p[0]->xString.value;
	DString *spec = p[2]->xString.value;
	DString *res = NULL, *sub = NULL;
	DaoTuple *tup = DaoValue_CastTuple( p[1] );
	DaoValue *val = NULL;
	DMap  *keys = NULL;
	DNode *node = NULL;
	daoint keep = p[3]->xBoolean.value;
	daoint i, pos1, pos2, prev = 0;
	char spec2;
	int replace;
	int ch;

	if( DString_Size( spec ) ==0 ){
		DaoProcess_PutString( proc, self );
		return;
	}
	if( tup ){
		keys = tup->ctype->mapNames;
	}else{
		keys = p[1]->xMap.value;
	}

	res = DaoProcess_PutChars( proc, "" );
	key = DaoString_New();
	sub = DString_New();
	spec2 = spec->chars[0];
	pos1 = DString_FindChar( self, spec2, prev );
	while( pos1 != DAO_NULLPOS ){
		pos2 = DString_FindChar( self, ')', pos1 );
		replace = 0;
		if( pos2 != DAO_NULLPOS && self->chars[pos1+1] == '(' ){
			replace = 1;
			for(i=pos1+2; i<pos2; i++){
				ch = self->chars[i];
				if( ch != '-' && ch != '_' && ch != ' ' && ! isalnum( ch ) ){
					replace = 0;
					break;
				}
			}
			if( replace ){
				DString_SubString( self, key->value, pos1+2, pos2-pos1-2 );
				node = DMap_Find( keys, tup ? (void*) key->value : (void*) key );
				if( node ){
					if( tup ){
						val = tup->values[node->value.pInt];
					}else{
						val = node->value.pValue;
					}
				}else if( keep ){
					replace = 0;
				}else{
					DString_Clear( key->value );
					val = (DaoValue*) key;
				}
			}
		}
		DString_SubString( self, sub, prev, pos1 - prev );
		DString_Append( res, sub );
		prev = pos1 + 1;
		if( replace ){
			DString *s = DaoValue_GetString( val, sub );
			DString_Append( res, s );
			prev = pos2 + 1;
		}else{
			DString_AppendChar( res, spec2 );
		}
		pos1 = DString_FindChar( self, spec2, prev );
	}
	DString_SubString( self, sub, prev, DString_Size( self ) - prev );
	DString_Append( res, sub );
	DString_Delete( sub );
	DaoString_Delete( key );
}
static void DaoSTR_Split( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoList *list = DaoProcess_PutList( proc );
	DaoValue *value = (DaoValue*) DaoString_New();
	DString *self = p[0]->xString.value;
	DString *delm = p[1]->xString.value;
	DString *str = value->xString.value;
	daoint dlen = DString_Size( delm );
	daoint size = DString_Size( self );
	daoint last = 0;
	daoint posDelm = DString_Find( self, delm, last );

	if( N ==1 || DString_Size( delm ) ==0 ){
		uchar_t *bytes = (unsigned char*) self->chars;
		daoint i = 0;
		while( i < size ){
			daoint pos = DString_LocateChar( self, i, 0 );
			int w = pos == DAO_NULLPOS ? 1 : DString_UTF8CharSize( bytes[i] );
			DString_SetBytes( str, (char*) bytes + i, w );
			DList_Append( list->value, value );
			i += w;
		}
		DaoString_Delete( (DaoString*) value );
		return;
	}
	while( posDelm != DAO_NULLPOS ){
		DString_SubString( self, str, last, posDelm-last );
		DList_Append( list->value, value );

		last = posDelm + dlen;
		posDelm = DString_Find( self, delm, last );
	}
	DString_SubString( self, str, last, size-last );
	DList_Append( list->value, value );
	DaoString_Delete( (DaoString*) value );
}
static void DaoSTR_Fetch( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *self = p[0]->xString.value;
	DString *pt = p[1]->xString.value;
	daoint group = p[2]->xInteger.value;
	daoint start = p[3]->xInteger.value;
	daoint end = p[4]->xInteger.value;
	DaoRegex *patt = DaoProcess_MakeRegex( proc, pt );
	int matched = 0;

	DString_Clear( pt ); /* passed in by value; */
	if( start < 0 ) start += self->size;
	if( end < 0 ) end += self->size;
	if( end == 0 ) end = DString_Size( self ) - 1;
	if( (patt == NULL) | (start < 0) | (end < 0) ) goto Done;
	if( DaoRegex_Match( patt, self, & start, & end ) ){
		matched = 1;
		if( group > 0 && DaoRegex_SubMatch( patt, group, & start, & end ) ==0 ) matched = 0;
	}
Done:
	if( matched ) DString_SubString( self, pt, start, end-start+1 );
	DaoProcess_PutString( proc, pt );
}
static void DaoSTR_Match( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *self = p[0]->xString.value;
	DString *pt = p[1]->xString.value;
	daoint group = p[2]->xInteger.value;
	daoint start = p[3]->xInteger.value;
	daoint end = p[4]->xInteger.value;
	DaoRegex *patt = DaoProcess_MakeRegex( proc, pt );
	int matched = 0;

	if( start < 0 ) start += self->size;
	if( end < 0 ) end += self->size;
	if( end == 0 ) end = DString_Size( self ) - 1;
	if( (patt == NULL) | (start < 0) | (end < 0) ) goto Done;
	if( DaoRegex_Match( patt, self, & start, & end ) ){
		matched = 1;
		if( group > 0 && DaoRegex_SubMatch( patt, group, & start, & end ) ==0 ) matched = 0;
	}
Done:
	if( matched ){
		DaoTuple *tuple = DaoProcess_PutTuple( proc, 2 );
		tuple->values[0]->xInteger.value = start;
		tuple->values[1]->xInteger.value = end;
	}else{
		DaoProcess_PutNone( proc );
	}
}
static void DaoSTR_Change( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *res = DaoProcess_PutChars( proc, "" );
	DString *self = p[0]->xString.value;
	DString *pt = p[1]->xString.value;
	DString *str = p[2]->xString.value;
	DaoRegex *patt = DaoProcess_MakeRegex( proc, pt );
	daoint start = p[4]->xInteger.value;
	daoint end = p[5]->xInteger.value;
	daoint index = p[3]->xInteger.value;
	if( start < 0 ) start += self->size;
	if( end < 0 ) end += self->size;
	if( end == 0 ) end = DString_Size( self ) - 1;
	if( (patt == NULL) | (start < 0) | (end < 0) ) return;
	DaoRegex_ChangeExt( patt, self, res, str, index, & start, & end );
}
static void DaoSTR_Capture( DaoProcess *proc, DaoValue *p[], int N )
{
	int gid;
	DString *self = p[0]->xString.value;
	DString *pt = p[1]->xString.value;
	daoint start = p[2]->xInteger.value;
	daoint end = p[3]->xInteger.value;
	DaoList *list = DaoProcess_PutList( proc );
	DaoRegex *patt = DaoProcess_MakeRegex( proc, pt );
	DaoString *subs;

	if( start < 0 ) start += self->size;
	if( end < 0 ) end += self->size;
	if( end == 0 ) end = DString_Size( self ) - 1;
	if( (patt == NULL) | (start < 0) | (end < 0) ) return;
	if( DaoRegex_Match( patt, self, & start, & end ) ==0 ) return;
	subs = DaoString_New();
	for(gid=0; gid<=patt->group; ++gid){
		DString_Clear( subs->value );
		if( DaoRegex_SubMatch( patt, gid, & start, & end ) ){
			DString_SubString( self, subs->value, start, end-start+1 );
		}
		DList_Append( list->value, (DaoValue*) subs );
	}
	DaoString_Delete( subs );
}
static void DaoSTR_Extract( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *self = p[0]->xString.value;
	DString *pt = p[1]->xString.value;
	int type = p[2]->xEnum.value;
	daoint offset, start, end, size, matched, done = 0;
	DaoList *list = DaoProcess_PutList( proc );
	DaoRegex *patt = DaoProcess_MakeRegex( proc, pt );
	DaoString *subs;
	
	size = DString_Size( self );
	start = offset = 0;
	end = size - 1;
	if( size == 0 || patt == NULL ) return;
	subs = DaoString_New();
	while( (matched = DaoRegex_Match( patt, self, & start, & end )) || done == 0 ){
		if( matched == 0 ) start = size;
		if( type == 0 || type == 2 ){
			if( start > offset ){
				DString_SubString( self, subs->value, offset, start-offset );
				DaoList_Append( list, (DaoValue*) subs );
			}
		}
		if( matched == 0 && done != 0 ) break;
		if( type == 0 || type == 1 ){
			if( matched ){
				DString_SubString( self, subs->value, start, end-start+1 );
				DaoList_Append( list, (DaoValue*) subs );
			}
		}
		done = matched == 0;
		start = offset = end + 1;
		end = size - 1;
	}
	DaoString_Delete( subs );
}
static void DaoSTR_Scan( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *self = p[0]->xString.value;
	DString *pt = p[1]->xString.value;
	daoint from = p[2]->xInteger.value;
	daoint to = p[3]->xInteger.value;
	daoint entry, offset, start, end, matched, done = 0;
	DaoList *list = DaoProcess_PutList( proc );
	DaoRegex *patt = DaoProcess_MakeRegex( proc, pt );
	DaoInteger startpos = {DAO_INTEGER,0,0,0,0,0};
	DaoInteger endpos = {DAO_INTEGER,0,0,0,0,0};
	DaoEnum denum = {DAO_ENUM,DAO_ENUM_SYM,0,0,0,0,0,NULL};
	DaoValue *res;
	DaoVmCode *sect;

	if( from < 0 ) from += self->size;
	if( to < 0 ) to += self->size;
	if( to == 0 ) to = DString_Size( self ) - 1;
	if( (patt == NULL) | (from < 0) | (to < 0) ){
		DaoProcess_RaiseError( proc, "Param", NULL );
		return;
	}

	sect = DaoProcess_InitCodeSection( proc, 3 );
	if( sect == NULL ) return;

	denum.etype = DaoNamespace_MakeEnumType( proc->activeNamespace, "unmatched,matched" );
	denum.subtype = DAO_ENUM_STATE;
	entry = proc->topFrame->entry;

	start = offset = from;
	end = to;
	while( (matched = DaoRegex_Match( patt, self, & start, & end )) || done == 0 ){
		if( matched == 0 ) start = to + 1;
		if( start > offset ){
			startpos.value = offset;
			endpos.value = start-1;
			denum.value = 0;
			if( sect->b > 0 ) DaoProcess_SetValue( proc, sect->a, (DaoValue*) & startpos );
			if( sect->b > 1 ) DaoProcess_SetValue( proc, sect->a+1, (DaoValue*) & endpos );
			if( sect->b > 2 ) DaoProcess_SetValue( proc, sect->a+2, (DaoValue*) & denum );
			proc->topFrame->entry = entry;
			DaoProcess_Execute( proc );
			if( proc->status == DAO_PROCESS_ABORTED ) break;
			res = proc->stackValues[0];
			if( res && res->type != DAO_NONE ) DaoList_Append( list, res );
		}
		if( matched == 0 && done != 0 ) break;
		if( matched ){
			startpos.value = start;
			endpos.value = end;
			denum.value = 1;
			if( sect->b > 0 ) DaoProcess_SetValue( proc, sect->a, (DaoValue*) & startpos );
			if( sect->b > 1 ) DaoProcess_SetValue( proc, sect->a+1, (DaoValue*) & endpos );
			if( sect->b > 2 ) DaoProcess_SetValue( proc, sect->a+2, (DaoValue*) & denum );
			proc->topFrame->entry = entry;
			DaoProcess_Execute( proc );
			if( proc->status == DAO_PROCESS_ABORTED ) break;
			res = proc->stackValues[0];
			if( res && res->type != DAO_NONE ) DaoList_Append( list, res );
		}
		done = matched == 0;
		start = offset = end + 1;
		end = to;
	}
	DaoProcess_PopFrame( proc );
}


static void DaoSTR_Convert( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *res = DaoProcess_PutString( proc, p[0]->xString.value );
	int bl = 1;
	switch( p[1]->xEnum.value ){
	case 0 : bl = DString_ToLocal( res ); break; /* local */
	case 1 : bl = DString_ToUTF8( res ); break; /* utf8 */
	case 2 : DString_ToLower( res ); break; /* lower */
	case 3 : DString_ToUpper( res ); break; /* upper */
	}
	if( bl == 0 ) DaoProcess_RaiseError( proc, "Value", "Conversion failed" );
}
static void DaoSTR_Functional( DaoProcess *proc, DaoValue *p[], int np, int funct )
{
	DString *string = NULL;
	DaoString *self = & p[0]->xString;
	DaoInteger chint = {DAO_INTEGER,0,0,0,0,0};
	DaoInteger idint = {DAO_INTEGER,0,0,0,0,0};
	DaoValue *res, *index = (DaoValue*)(void*)&idint;
	DaoValue *chr = (DaoValue*)(void*)&chint;
	DaoVmCode *sect = DaoProcess_InitCodeSection( proc, 2 );
	DString *data = self->value;
	daoint unit = p[1]->xEnum.value;
	daoint entry, i, N = data->size;
	char *chars = data->chars, *end = chars + N;
	DCharState state = { 1, 1, 0 };

	switch( funct ){
	case DVM_FUNCT_COLLECT :
		string = DaoProcess_PutChars( proc, "" );
		DString_Reserve( string, self->value->size );
		break;
	}
	if( sect == NULL ) return;
	entry = proc->topFrame->entry;
	for(i=0; i<N; ){
		if( unit ){
			state = DString_DecodeChar( chars, end );
		}else{
			state.value = data->chars[i];
		}
		chars += state.width;
		i += state.width;
		idint.value = i;
		chint.value = state.value;
		if( sect->b >0 ) DaoProcess_SetValue( proc, sect->a, chr );
		if( sect->b >1 ) DaoProcess_SetValue( proc, sect->a+1, index );
		proc->topFrame->entry = entry;
		DaoProcess_Execute( proc );
		if( proc->status == DAO_PROCESS_ABORTED ) break;
		res = proc->stackValues[0];
		switch( funct ){
		case DVM_FUNCT_COLLECT :
			if( res->type == DAO_NONE ) break;
			if( unit ){
				DString_AppendWChar( string, res->xInteger.value );
			}else{
				DString_AppendChar( string, res->xInteger.value );
			}
			break;
		}
	}
	DaoProcess_PopFrame( proc );
}
static void DaoSTR_Iterate( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoSTR_Functional( proc, p, N, DVM_FUNCT_ITERATE );
}
static void DaoSTR_Index( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *self = p[0]->xString.value;
	daoint index = p[1]->xInteger.value;
	daoint pos = DString_GetByteIndex( self, index );
	DaoProcess_PutInteger( proc, pos );
}
static void DaoSTR_Char( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *self = p[0]->xString.value;
	daoint index = p[1]->xInteger.value;
	daoint pos = DString_GetByteIndex( self, index );
	DCharState state = DString_DecodeChar( self->chars + pos, self->chars + self->size );
	DaoProcess_PutBytes( proc, self->chars + pos, state.width );
}

static DaoFuncItem stringMeths[] =
{
	{ DaoSTR_New,
		"string( count: int, char = 0 ) => string"
		/*
		// Create and return a string that is composed of "count" of "char".
		*/
	},
	{ DaoSTR_New2,
		"string( count: int )[index: int => string] => string"
		/*
		// Create and return a string that is concatenation of the resulting
		// strings from the exection of the code section.
		*/
	},
	{ DaoSTR_Size,
		"size( invar self: string, utf8 = false ) => int"
		/*
		// Return the number of bytes or characters in the string.
		*/
	},
	{ DaoSTR_Insert,
		"insert( invar self: string, str: string, pos = 0 ) => string"
		/*
		// Insert "str" at "pos";
		// Return a new string;
		*/
	},
	{ DaoSTR_Erase,
		"erase( invar self: string, pos = 0, count = -1 ) => string"
		/*
		// Erase "count" bytes starting from "pos";
		// Return a new string;
		*/
	},
	{ DaoSTR_Chop,
		"chop( invar self: string, utf8 = false ) => string"
		/*
		// Chop EOF, '\n' and/or '\r' off the end of the string;
		// -- EOF  is first checked and removed if found;
		// -- '\n' is then checked and removed if found;
		// -- '\r' is last checked and removed if found;
		// If "utf8" is not zero, all bytes that do not constitute a
		// valid UTF-8 encoding sequence are removed from the end.
		*/
	},
	{ DaoSTR_Trim,
		"trim( invar self: string, where: enum<head;tail> = $head+$tail, utf8 = false )"
			"=> string"
		/*
		// Trim whitespaces from the head and/or the tail of the string;
		// If "utf8" is not zero, all bytes that do not constitute a
		// valid UTF-8 encoding sequence are trimmed as well.
		*/
	},
	{ DaoSTR_Find,
		"find( invar self: string, str: string, from = 0, reverse = false ) => int"
		/*
		// Find the first occurrence of "str" in this string, searching from "from";
		// If "reverse" is zero, search forward, otherwise backward;
		// Return -1, if "str" is not found; Otherwise,
		// Return the index of the first byte of the found substring for forward searching;
		// Return the index of the last byte of the found substring for backward searching;
		*/
	},
	{ DaoSTR_Contains,
		"contains( invar self: string, "
			"...: tuple<pos:int|enum<prefix,suffix>,keyword:string> ) => bool"
		/*
		// Check if the string contain specific keywords at specific locations.
		// Locations can be specified by byte indices or symbol $prefix or $suffix.
		*/
	},
	{ DaoSTR_Convert,
		"convert( invar self: string, to: enum<local,utf8,lower,upper> ) => string"
		/*
		// Convert the string:
		// -- To local encoding if the string is encoded in UTF-8;
		// -- To UTF-8 encoding if the string is not encoded in UTF-8;
		// -- To lower cases;
		// -- To upper cases;
		*/
	},
	{ DaoSTR_Replace,
		"replace( invar self: string, str1: string, str2: string, index = 0 ) => string"
		/*
		// Replace the substring "str1" in "self" to "str2";
		// Replace all occurrences of "str1" to "str2" if "index" is zero;
		// Otherwise, replace only the "index"-th occurrence;
		// Positive "index" is counted forwardly;
		// Negative "index" is counted backwardly;
		*/
	},
	{ DaoSTR_Expand,
		"expand( invar self: string, invar subs: map<string,string>"
			"|tuple<...:int|float|string>, spec = \"$\", keep = true ) => string"
		/*
		// Expand this string into a new string with substrings from the keys
		// of "subs" substituted with the corresponding values of "subs".
		// If "spec" is not an empty string, each key has to be occurred inside
		// a pair of parenthesis preceded with "spec", and the "spec", the
		// parenthesis and the key are together substituted by the corresponding
		// value from "subs"; If "spec" is not empty and "keep" is zero, "spec(key)"
		// that contain substrings not found in the keys of "subs" are removed;
		// Otherwise kept.
		*/
	},
	{ DaoSTR_Split,
		"split( invar self: string, sep = \"\" ) => list<string>"
		/*
		// Split the string by seperator "sep", and return the tokens as a list.
		// If "sep" is empty, split at character boundaries assuming UTF-8 encoding.
		*/
	},
	{ DaoSTR_Fetch,
		"fetch( invar self: string, pattern: string, group = 0, start = 0, end = -1 )"
			"=> string"
		/*
		// Fetch the substring that matches the "group"-th group of pattern "pattern".
		// Only the region between "start" and "end" is searched.
		// Negative index starts from the last byte of the string.
		*/
	},
	{ DaoSTR_Match,
		"match( invar self: string, pattern: string, group = 0, start = 0, end = -1 )"
			"=> tuple<start:int,end:int>|none"
		/*
		// Match part of this string to pattern "pattern".
		// If matched, the indexes of the first and the last byte of the matched
		// substring will be returned as a tuple. If not matched, "none" is returned.
		// Parameter "start" and "end" have the same meaning as in string::fetch().
		*/
	},
	{ DaoSTR_Change,
		"change( invar self: string, pattern: string, target: string, index = 0, "
			"start = 0, end = -1 ) => string"
		/*
		// Change the part(s) of the string that match pattern "pattern" to "target".
		// The target string "target" can contain back references from pattern "pattern".
		// If "index" is zero, all matched parts are changed; otherwise, only
		// the "index" match is changed.
		// Parameter "start" and "end" have the same meaning as in string::fetch().
		// Returns a shallow copy of the self string.
		*/
	},
	{ DaoSTR_Capture,
		"capture( invar self: string, pattern: string, start = 0, end = -1 ) => list<string>"
		/*
		// Match pattern "pattern" to the string, and capture all the substrings that
		// match to each of the groups of "pattern". Note that the pattern groups are
		// indexed starting from one, and zero index is reserved for the whole pattern.
		// The strings in the returned list correspond to the groups that have the
		// same index as that of the strings in the list.
		// Parameter "start" and "end" have the same meaning as in string::fetch().
		*/
	},
	{ DaoSTR_Extract,
		"extract( invar self: string, pattern: string, "
			"mtype: enum<both,matched,unmatched> = $matched ) => list<string>"
		/*
		// Extract the substrings that match to, or are between the matched ones,
		// or both, and return them as a list.
		*/
	},
	{ DaoSTR_Scan,
		"scan( invar self: string, pattern: string, start = 0, end = -1 )"
			"[start: int, end: int, state: enum<unmatched,matched> => none|@V]"
			"=> list<@V>"
		/*
		// Scan the string with pattern "pattern", and invoke the attached code
		// section for each matched substring and substrings between matches.
		// The start and end index as well as the state of matching or not matching
		// can be passed to the code section.
		// Parameter "start" and "end" have the same meaning as in string::fetch().
		// 
		// Use "none|@V" for the code section return, so that if "return none" is used first,
		// it will not be specialized to "none|none", which is the case for "@V|none".
		*/
	},

	{ DaoSTR_Iterate,
		"iterate( invar self: string, unit: enum<byte,char> = $byte )[char: int, index: int]"
		/*
		// Iterate over each unit of the string.
		// If "unit" is "$byte", iterate per byte;
		// If "unit" is "$char", iterate per character; Assuming UTF-8 encoding;
		// Each byte that is not part of a valid UTF-8 encoding unit is iterated once.
		// For the code section parameters, the first will hold the byte value or
		// character codepoint for each iteration, and the second will be the byte
		// location in the string.
		*/
	},

	{ DaoSTR_Index,
		"offset( invar self: string, charIndex: int ) => int"
		/*
		// Get byte offset for the character with index "charIndex";
		*/
	},
	{ DaoSTR_Char,
		"char( invar self: string, charIndex: int ) => string"
		/*
		// Get the character with index "charIndex";
		*/
	},
	{ NULL, NULL }
};

DaoTypeBase stringTyper=
{
	"string", & stringCore, NULL, (DaoFuncItem*) stringMeths, {0}, {0},
	(FuncPtrDel) DaoString_Delete, NULL
};

static void Dao_Print( DaoValue *self, DaoValue **items, daoint size, char lb, char rb, DaoProcess *proc, DaoStream *stream, DMap *cycData )
{
	DNode *node = NULL;
	daoint i;

	if( cycData ) node = MAP_Find( cycData, self );
	if( node ){
		DaoStream_WriteChar( stream, lb );
		DaoStream_WriteChars( stream, "..." );
		DaoStream_WriteChar( stream, rb );
		return;
	}
	if( cycData ) MAP_Insert( cycData, self, self );

	DaoStream_WriteChar( stream, lb );
	DaoStream_WriteChar( stream, ' ' );
	for( i=0; i<size; i++ ){
		if( items[i] && items[i]->type == DAO_STRING ) DaoStream_WriteChar( stream, '"' );
		DaoValue_Print( items[i], proc, stream, cycData );
		if( items[i] && items[i]->type == DAO_STRING ) DaoStream_WriteChar( stream, '"' );
		if( i != size-1 ) DaoStream_WriteChars( stream, ", " );
	}
	DaoStream_WriteChar( stream, ' ' );
	DaoStream_WriteChar( stream, rb );
	if( cycData ) MAP_Erase( cycData, self );
}
static void DaoListCore_Print( DaoValue *self0, DaoProcess *proc, DaoStream *stream, DMap *cycData )
{
	DaoList *self = (DaoList*) self0;
	DaoValue **data = self->value->items.pValue;
	daoint size = self->value->size;
	Dao_Print( self0, data, size, '{', '}', proc, stream, cycData );
}
static void DaoListCore_GetItem1( DaoValue *self0, DaoProcess *proc, DaoValue *pid )
{
	DaoList *res, *self = & self0->xList;
	daoint size = self->value->size;
	daoint e = proc->exceptions->size;
	daoint i, n, start, end;
	int idtype;
	MakeIndex( proc, pid, size, & start, & end, & idtype );
	if( proc->exceptions->size > e ) return;

	switch( idtype ){
	case IDX_EMPTY :
		break;
	case IDX_NULL :
		res = DaoList_Copy( self, NULL );
		DaoProcess_PutValue( proc, (DaoValue*) res );
		break;
	case IDX_SINGLE :
		DaoProcess_PutValue( proc, self->value->items.pValue[start] );
		break;
	case IDX_FROM :
		res = DaoProcess_PutList( proc );
		if( start >= self->value->size ) break;
		DList_Resize( res->value, self->value->size - start, NULL );
		for(i=start,n=self->value->size; i<n; i++)
			DaoList_SetItem( res, self->value->items.pValue[i], i-start );
		break;
	case IDX_TO :
		res = DaoProcess_PutList( proc );
		DList_Resize( res->value, end +1, NULL );
		for(i=0; i<=end; i++) DaoList_SetItem( res, self->value->items.pValue[i], i );
		break;
	case IDX_PAIR :
		res = DaoProcess_PutList( proc );
		DList_Resize( res->value, end - start + 1, NULL );
		for(i=start; i<=end; i++) DaoList_SetItem( res, self->value->items.pValue[i], i-start );
		break;
	case IDX_ALL :
		res = DaoList_Copy( self, NULL );
		DaoProcess_PutValue( proc, (DaoValue*) res );
		break;
	default : break;
	}
}
static void DaoListCore_SetItem1( DaoValue *self0, DaoProcess *proc, DaoValue *pid, DaoValue *value )
{
	DaoList *self = & self0->xList;
	daoint size = self->value->size;
	daoint i, n, start, end;
	int idtype, rc = 0;
	MakeIndex( proc, pid, size, & start, & end, & idtype );
	if( self->ctype == NULL ){
		/* a : tuple<string,list<int>> = ('',{});
		   duplicating the constant to assign to a may not set the ctype properly */
		self->ctype = proc->activeTypes[ proc->activeCode->c ];
		GC_IncRC( self->ctype );
	}
	switch( idtype ){
	case IDX_EMPTY :
		break;
	case IDX_NULL :
		for( i=0; i<size; i++ ) rc |= DaoList_SetItem( self, value, i );
		break;
	case IDX_SINGLE :
		DaoList_SetItem( self, value, start );
		break;
	case IDX_FROM :
		for( i=start,n=self->value->size; i<n; i++ ) rc |= DaoList_SetItem( self, value, i );
		break;
	case IDX_TO :
		for( i=0; i<=end; i++ ) rc |= DaoList_SetItem( self, value, i );
		break;
	case IDX_PAIR :
		for( i=start; i<=end; i++ ) rc |= DaoList_SetItem( self, value, i );
		break;
	case IDX_ALL :
		for( i=0,n=self->value->size; i<n; i++ ) rc |= DaoList_SetItem( self, value, i );
		break;
	default : break;
	}
	if( rc ) DaoProcess_RaiseError( proc, "Value", "Invalid value type" );
}
static void DaoListCore_GetItem( DaoValue *self, DaoProcess *proc, DaoValue *ids[], int N )
{
	switch( N ){
	case 0 : DaoListCore_GetItem1( self, proc, NULL ); break;
	case 1 : DaoListCore_GetItem1( self, proc, ids[0] ); break;
	default : DaoProcess_RaiseError( proc, "Index", "not supported" );
	}
}
static void DaoListCore_SetItem( DaoValue *self, DaoProcess *proc, DaoValue *ids[], int N, DaoValue *value )
{
	switch( N ){
	case 0 : DaoListCore_SetItem1( self, proc, NULL, value ); break;
	case 1 : DaoListCore_SetItem1( self, proc, ids[0], value ); break;
	default : DaoProcess_RaiseError( proc, "Index", "not supported" );
	}
}
static DaoTypeCore listCore=
{
	NULL,
	DaoValue_GetField,
	DaoValue_SetField,
	DaoListCore_GetItem,
	DaoListCore_SetItem,
	DaoListCore_Print
};

static void DaoLIST_New( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoInteger idint = {DAO_INTEGER,0,0,0,0,0};
	DaoValue *res = p[N==2], *index = (DaoValue*)(void*)&idint;
	DaoList *list = DaoProcess_PutList( proc );
	daoint i, entry, size = p[0]->xInteger.value;
	daoint fold = N == 2;
	DaoVmCode *sect;

	if( fold ) DaoList_Append( list, res );
	if( size < 0 ){
		DaoProcess_RaiseError( proc, "Param", "Invalid parameter value" );
		return;
	}
	if( size == 0 ) return;
	sect = DaoProcess_InitCodeSection( proc, 1 + fold );
	if( sect == NULL ) return;
	entry = proc->topFrame->entry;
	for(i=fold; i<size; i++){
		idint.value = i;
		if( sect->b >0 ) DaoProcess_SetValue( proc, sect->a, index );
		if( sect->b >1 && N ==2 ) DaoProcess_SetValue( proc, sect->a+1, res );
		proc->topFrame->entry = entry;
		DaoProcess_Execute( proc );
		if( proc->status == DAO_PROCESS_ABORTED ) break;
		res = proc->stackValues[0];
		DaoList_Append( list, res );
	}
	DaoProcess_PopFrame( proc );
}

static daoint DaoList_MakeIndex( DaoList *self, daoint index, int one_past_last )
{
	if( index < 0 ) index += self->value->size;
	if( (index < 0) | (index > (self->value->size - 1 + one_past_last)) ) return -1;
	return index;
}
static void DaoLIST_Insert( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoList *self = & p[0]->xList;
	daoint size = self->value->size;
	daoint pos = DaoList_MakeIndex( self, p[2]->xInteger.value, 1 );
	DaoProcess_PutValue( proc, p[0] );
	if( pos == -1 ){
		DaoProcess_RaiseError( proc, "Index::Range", NULL );
		return;
	}
	DaoList_Insert( self, p[1], pos );
	if( size == self->value->size ) DaoProcess_RaiseError( proc, "Value", "value type" );
}
static void DaoLIST_Erase( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoList *self = & p[0]->xList;
	daoint start = p[1]->xInteger.value;
	daoint n = p[2]->xInteger.value;
	DaoProcess_PutValue( proc, p[0] );
	DList_Erase( self->value, start, n );
}
static void DaoLIST_Clear( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoList *self = & p[0]->xList;
	DaoList_Clear( self );
}
static void DaoLIST_Size( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoList *self = & p[0]->xList;
	DaoProcess_PutInteger( proc, self->value->size );
}
static void DaoLIST_Resize( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoList *self = & p[0]->xList;
	DaoValue *fill = dao_none_value;
	daoint size = p[1]->xInteger.value;
	if( self->ctype && self->ctype->nested->size )
		fill = self->ctype->nested->items.pType[0]->value;
	DList_Resize( self->value, size, fill );
}
static void DaoLIST_Resize2( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoList *self = & p[0]->xList;
	DaoType *tp = NULL;
	DaoValue *fill = p[1];
	daoint size = p[2]->xInteger.value;
	if( self->ctype && self->ctype->nested->size )
		tp = self->ctype->nested->items.pType[0];

	fill = DaoValue_CopyContainer( fill, tp );
	if( fill != p[1] ){
		fill->xBase.trait |= DAO_VALUE_CONST; /* force copying; */
		DaoGC_IncRC( fill );
	}
	DList_Resize( self->value, size, fill );
	if( fill != p[1] ) DaoGC_DecRC( fill );
}
static int DaoList_CheckType( DaoList *self, DaoProcess *proc )
{
	daoint i, type;
	DaoValue **data = self->value->items.pValue;
	if( self->value->size == 0 ) return 0;
	type = data[0]->type;
	for(i=1; i<self->value->size; i++){
		if( type != data[i]->type ){
			DaoProcess_RaiseWarning( proc, NULL, "need list of same type of elements" );
			return 0;
		}
	}
	if( type < DAO_INTEGER || type >= DAO_ARRAY ){
		DaoProcess_RaiseWarning( proc, NULL, "need list of primitive data" );
		return 0;
	}
	return type;
}
static void DaoLIST_Max( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoTuple *tuple;
	DaoList *self = (DaoList*) p[0];
	DaoValue *res, **data = self->value->items.pValue;
	daoint i, imax, size = self->value->size;

	if( DaoList_CheckType( self, proc ) == 0 ){
		DaoProcess_PutNone( proc );
		return;
	}
	imax = 0;
	res = data[0];
	for(i=1; i<size; i++){
		if( DaoValue_ComparePro( res, data[i], proc ) <0 ){
			imax = i;
			res = data[i];
		}
	}
	tuple = DaoProcess_PutTuple( proc, 2 );
	tuple->values[1]->xInteger.value = imax;
	DaoTuple_SetItem( tuple, res, 0 );
}
static void DaoLIST_Min( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoTuple *tuple;
	DaoList *self = (DaoList*) p[0];
	DaoValue *res, **data = self->value->items.pValue;
	daoint i, imin, size = self->value->size;

	if( DaoList_CheckType( self, proc ) == 0 ){
		DaoProcess_PutNone( proc );
		return;
	}
	imin = 0;
	res = data[0];
	for(i=1; i<size; i++){
		if( DaoValue_ComparePro( res, data[i], proc ) >0 ){
			imin = i;
			res = data[i];
		}
	}
	tuple = DaoProcess_PutTuple( proc, 2 );
	tuple->values[1]->xInteger.value = imin;
	DaoTuple_SetItem( tuple, res, 0 );
}
extern DaoEnum* DaoProcess_GetEnum( DaoProcess *self, DaoVmCode *vmc );
static void DaoLIST_Sum( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoList *self = & p[0]->xList;
	daoint i, len, type, size = self->value->size;
	DaoValue **data = self->value->items.pValue;
	type = DaoList_CheckType( self, proc );
	if( type == 0 ){
		DaoProcess_PutValue( proc, self->ctype->nested->items.pType[0]->value );
		return;
	}
	switch( type ){
	case DAO_BOOLEAN :
		{
			daoint res = 0;
			for(i=0; i<size; i++) res |= data[i]->xInteger.value;
			DaoProcess_PutBoolean( proc, res );
			break;
		}
	case DAO_INTEGER :
		{
			dao_integer res = 0;
			for(i=0; i<size; i++) res += data[i]->xInteger.value;
			DaoProcess_PutInteger( proc, res );
			break;
		}
	case DAO_FLOAT :
		{
			dao_float res = 0.0;
			for(i=0; i<size; i++) res += data[i]->xFloat.value;
			DaoProcess_PutFloat( proc, res );
			break;
		}
	case DAO_COMPLEX :
		{
			dao_complex res = { 0.0, 0.0 };
			for(i=0; i<self->value->size; i++) COM_IP_ADD( res, data[i]->xComplex.value );
			DaoProcess_PutComplex( proc, res );
			break;
		}
	case DAO_STRING :
		{
			DString *m = DaoProcess_PutChars( proc, "" );
			for(i=0,len=0; i<size; i++) len += data[i]->xString.value->size;
			DString_Reserve( m, len );
			for(i=0; i<size; i++) DString_Append( m, data[i]->xString.value );
			break;
		}
	default : break;
	}
}
static void DaoLIST_Push( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoList *self = & p[0]->xList;
	daoint size = self->value->size;
	DaoProcess_PutValue( proc, p[0] );
	if ( p[2]->xEnum.value == 0 )
		DaoList_PushFront( self, p[1] );
	else
		DaoList_Append( self, p[1] );
	if( size == self->value->size ) DaoProcess_RaiseError( proc, "Value", "value type" );
}
static void DaoLIST_Pop( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoList *self = & p[0]->xList;
	if( self->value->size == 0 ){
		DaoProcess_RaiseError( proc, "Value", "list is empty" );
		return;
	}
	if ( p[1]->xEnum.value == 0 ){
		DaoProcess_PutValue( proc, self->value->items.pValue[0] );
		DaoList_Erase( self, 0 );
	}else{
		DaoProcess_PutValue( proc, self->value->items.pValue[self->value->size -1] );
		DaoList_Erase( self, self->value->size -1 );
	}
}
static void DaoLIST_Join( DaoProcess *proc, DaoValue *p[], int N )
{
	int i;
	DaoList *self = & p[0]->xList;
	DaoProcess_PutValue( proc, p[0] );
	for(i=1; i<N; ++i) DList_AppendList( self->value, p[i]->xList.value );
}
static void DaoLIST_PushBack( DaoProcess *proc, DaoValue *p[], int N )
{
	int i;
	DaoList *self = & p[0]->xList;
	DaoProcess_PutValue( proc, p[0] );
	for(i=1; i<N; ++i) DaoList_Append( self, p[i] );
}
static void DaoLIST_Front( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoList *self = & p[0]->xList;
	if( self->value->size == 0 ){
		DaoProcess_PutValue( proc, dao_none_value );
		DaoProcess_RaiseError( proc, "Value", "list is empty" );
		return;
	}
	DaoProcess_PutValue( proc, self->value->items.pValue[0] );
}
static void DaoLIST_Back( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoList *self = & p[0]->xList;
	if( self->value->size == 0 ){
		DaoProcess_PutValue( proc, dao_none_value );
		DaoProcess_RaiseError( proc, "Value", "list is empty" );
		return;
	}
	DaoProcess_PutValue( proc, self->value->items.pValue[ self->value->size -1 ] );
}
/* Quick Sort.
 * Adam Drozdek: Data Structures and Algorithms in C++, 2nd Edition.
 */
static int Compare( DaoProcess *proc, int entry, int reg0, int reg1, DaoValue *v0, DaoValue *v1 )
{
	DaoValue **locs = proc->activeValues;
	DaoValue_Copy( v0, locs + reg0 );
	DaoValue_Copy( v1, locs + reg1 );
	proc->topFrame->entry = entry;
	DaoProcess_Execute( proc );
	return DaoValue_GetInteger( proc->stackValues[0] );
}
static void PartialQuickSort( DaoProcess *proc, int entry, int r0, int r1,
		DaoValue **data, daoint first, daoint last, daoint part )
{
	daoint lower=first+1, upper=last;
	DaoValue *val, *pivot;

	if( first >= last ) return;
	val = data[first];
	data[first] = data[ (first+last)/2 ];
	data[ (first+last)/2 ] = val;
	pivot = data[ first ];

	while( lower <= upper ){
		while( lower < last && Compare( proc, entry, r0, r1, data[lower], pivot ) ) lower ++;
		while( upper > first && Compare( proc, entry, r0, r1, pivot, data[upper] ) ) upper --;
		if( lower < upper ){
			val = data[lower];
			data[lower] = data[upper];
			data[upper] = val;
			upper --;
		}
		lower ++;
	}
	val = data[first];
	data[first] = data[upper];
	data[upper] = val;
	if( first+1 < upper ) PartialQuickSort( proc, entry, r0, r1, data, first, upper-1, part );
	if( upper >= part ) return;
	if( upper+1 < last ) PartialQuickSort( proc, entry, r0, r1, data, upper+1, last, part );
}
static void QuickSort( DaoProcess *self, DaoValue *data[], daoint first, daoint last, daoint part, int asc )
{
	daoint lower=first+1, upper=last;
	DaoValue *val, *pivot;

	if( first >= last ) return;
	val = data[first];
	data[first] = data[ (first+last)/2 ];
	data[ (first+last)/2 ] = val;
	pivot = data[ first ];

	while( lower <= upper ){
		if( asc ){
			while( lower < last && DaoValue_ComparePro( data[lower], pivot, self ) <0 ) lower ++;
			while( upper > first && DaoValue_ComparePro( pivot, data[upper], self ) <0 ) upper --;
		}else{
			while( lower < last && DaoValue_ComparePro( data[lower], pivot, self ) >0 ) lower ++;
			while( upper > first && DaoValue_ComparePro( pivot, data[upper], self ) >0 ) upper --;
		}
		if( lower < upper ){
			val = data[lower];
			data[lower] = data[upper];
			data[upper] = val;
			upper --;
		}
		lower ++;
	}
	val = data[first];
	data[first] = data[upper];
	data[upper] = val;
	if( first+1 < upper ) QuickSort( self, data, first, upper-1, part, asc );
	if( upper >= part ) return;
	if( upper+1 < last ) QuickSort( self, data, upper+1, last, part, asc );
}
static void DaoLIST_Sort( DaoProcess *proc, DaoValue *p[], int npar )
{
	DaoList *list = & p[0]->xList;
	DaoValue **items = list->value->items.pValue;
	daoint part = p[1 + (p[1]->type== DAO_ENUM)]->xInteger.value;
	daoint N;

	DaoProcess_PutValue( proc, p[0] );
	N = list->value->size;
	if( N < 2 ) return;
	if( part ==0 ) part = N;

	if( p[1]->type != DAO_ENUM ){
		DaoVmCode *sect = DaoProcess_InitCodeSection( proc, 2 );
		int entry = proc->topFrame->entry;
		if( sect == NULL ) return;
		if( sect->b < 2 ){
			DaoProcess_RaiseError( proc, NULL, "Two few code section parameters" );
			return;
		}
		PartialQuickSort( proc, entry, sect->a, sect->a + 1, items, 0, N-1, part );
		DaoProcess_PopFrame( proc );
		return;
	}
	QuickSort( proc, items, 0, N-1, part, p[1]->xEnum.value == 0 );
}
static void DaoLIST_BasicFunctional( DaoProcess *proc, DaoValue *p[], int npar, int funct )
{
	int direction = p[1]->xEnum.value;
	DaoList *list = & p[0]->xList;
	DaoList *list2 = NULL;
	DaoTuple *tuple = NULL;
	DaoInteger idint = {DAO_INTEGER,0,0,0,0,0};
	DaoValue **items = list->value->items.pValue;
	DaoValue *res, *index = (DaoValue*)(void*)&idint;
	DaoVmCode *sect = DaoProcess_InitCodeSection( proc, 2 );
	daoint entry, i, j, N = list->value->size;
	int popped = 0;
	switch( funct ){
	case DVM_FUNCT_SELECT :
	case DVM_FUNCT_COLLECT : list2 = DaoProcess_PutList( proc ); break;
	case DVM_FUNCT_APPLY : DaoProcess_PutValue( proc, p[0] ); break;
	case DVM_FUNCT_FIND : DaoProcess_PutValue( proc, dao_none_value ); break;
	}
	if( sect == NULL ) return;
	entry = proc->topFrame->entry;
	for(j=0; j<N; j++){
		i = direction ? N-1-j : j;
		idint.value = i;
		if( sect->b >0 ) DaoProcess_SetValue( proc, sect->a, items[i] );
		if( sect->b >1 ) DaoProcess_SetValue( proc, sect->a+1, index );
		proc->topFrame->entry = entry;
		DaoProcess_Execute( proc );
		if( proc->status == DAO_PROCESS_ABORTED ) break;
		res = proc->stackValues[0];
		switch( funct ){
		case DVM_FUNCT_COLLECT :
			if( res->type != DAO_NONE ) DaoList_Append( list2, res );
			break;
		case DVM_FUNCT_SELECT :
			if( res->xBoolean.value ) DaoList_Append( list2, items[i] );
			break;
		case DVM_FUNCT_APPLY : DaoList_SetItem( list, res, i ); break;
		}
		if( funct == DVM_FUNCT_FIND && res->xBoolean.value ){
			popped = 1;
			DaoProcess_PopFrame( proc );
			tuple = DaoProcess_PutTuple( proc, 2 );
			GC_Assign( & tuple->values[1], items[i] );
			tuple->values[0]->xInteger.value = j;
			break;
		}
	}
	if( popped == 0 ) DaoProcess_PopFrame( proc );
}
static void DaoLIST_Collect( DaoProcess *proc, DaoValue *p[], int npar )
{
	DaoLIST_BasicFunctional( proc, p, npar, DVM_FUNCT_COLLECT );
}
static void DaoLIST_Find( DaoProcess *proc, DaoValue *p[], int npar )
{
	DaoLIST_BasicFunctional( proc, p, npar, DVM_FUNCT_FIND );
}
static void DaoLIST_Select( DaoProcess *proc, DaoValue *p[], int npar )
{
	DaoLIST_BasicFunctional( proc, p, npar, DVM_FUNCT_SELECT );
}
static void DaoLIST_Iterate( DaoProcess *proc, DaoValue *p[], int npar )
{
	DaoLIST_BasicFunctional( proc, p, npar, DVM_FUNCT_ITERATE );
}
static void DaoLIST_Apply( DaoProcess *proc, DaoValue *p[], int npar )
{
	DaoLIST_BasicFunctional( proc, p, npar, DVM_FUNCT_APPLY );
}
static void DaoLIST_Reduce( DaoProcess *proc, DaoValue *p[], int npar, int which )
{
	DaoList *list = & p[0]->xList;
	DaoInteger idint = {DAO_INTEGER,0,0,0,0,0};
	DaoValue **items = list->value->items.pValue;
	DaoValue *res = NULL, *index = (DaoValue*)(void*)&idint;
	daoint entry, i, j, first = 0, D = 0, N = list->value->size;
	DaoVmCode *sect;

	if( which == 1 ){
		res = list->value->size ? items[0] : dao_none_value;
		D = p[1]->xEnum.value;
		first = 1;
	}else{
		res= p[1];
		D = p[2]->xEnum.value;
	}
	if( list->value->size == 0 ){
		DaoProcess_PutValue( proc, res );
		return;
	}
	sect = DaoProcess_InitCodeSection( proc, 3 );
	if( sect == NULL ) return;
	entry = proc->topFrame->entry;
	for(j=first; j<N; j++){
		i = D ? N-1-j : j;
		idint.value = i;
		if( sect->b >0 ) DaoProcess_SetValue( proc, sect->a, items[i] );
		if( sect->b >1 ) DaoProcess_SetValue( proc, sect->a+1, res );
		if( sect->b >2 ) DaoProcess_SetValue( proc, sect->a+2, index );
		proc->topFrame->entry = entry;
		DaoProcess_Execute( proc );
		if( proc->status == DAO_PROCESS_ABORTED ) break;
		res = proc->stackValues[0];
	}
	DaoProcess_PopFrame( proc );
	DaoProcess_PutValue( proc, res );
}
static void DaoLIST_Reduce1( DaoProcess *proc, DaoValue *p[], int npar )
{
	DaoLIST_Reduce( proc, p, npar, 1 );
}
static void DaoLIST_Reduce2( DaoProcess *proc, DaoValue *p[], int npar )
{
	DaoLIST_Reduce( proc, p, npar, 2 );
}
static unsigned int DaoProcess_GetHashSeed( DaoProcess *self, DaoValue *seed )
{
	if( seed->type == DAO_INTEGER ) return seed->xInteger.value;
	if( seed->type == DAO_ENUM ){
		unsigned int hashing = seed->xEnum.value;
		if( hashing == 2 ) hashing = rand();
		return hashing;
	}
	return 0;
}
static void DaoLIST_Functional2( DaoProcess *proc, DaoValue *p[], int npar, int meth )
{
	DaoValue *res = NULL;
	DaoMap *map = NULL;
	DaoList *list3 = NULL;
	DaoList *list = & p[0]->xList;
	DaoList *list2 = & p[1]->xList;
	DaoInteger idint = {DAO_INTEGER,0,0,0,0,0};
	DaoValue **items = list->value->items.pValue;
	DaoValue **items2 = list2->value->items.pValue;
	DaoValue *index = (DaoValue*)(void*)&idint;
	DaoVmCode *sect = DaoProcess_InitCodeSection( proc, 3 );
	daoint entry, i, j, N = list->value->size;
	unsigned int hashing = DaoProcess_GetHashSeed( proc, p[2] );
	int direction = DaoValue_TryGetEnum( p[2] );

	switch( meth ){
	case DVM_FUNCT_COLLECT :
		list3 = DaoProcess_PutList( proc );
		break;
	case DVM_FUNCT_ASSOCIATE :
		map = DaoProcess_PutMap( proc, hashing );
		direction = 0;
		break;
	}

	if( sect == NULL ) return;
	if( N > list2->value->size ) N = list2->value->size;
	entry = proc->topFrame->entry;
	for(j=0; j<N; j++){
		i = direction ? N-1-j : j;
		idint.value = i;
		if( sect->b > 0 ) DaoProcess_SetValue( proc, sect->a, items[i] );
		if( sect->b > 1 ) DaoProcess_SetValue( proc, sect->a+1, items2[i] );
		if( sect->b > 2 ) DaoProcess_SetValue( proc, sect->a+2, index );
		proc->topFrame->entry = entry;
		DaoProcess_Execute( proc );
		if( proc->status == DAO_PROCESS_ABORTED ) break;
		res = proc->stackValues[0];
		if( res->type == DAO_NONE ) continue;
		switch( meth ){
		case DVM_FUNCT_COLLECT :
			DaoList_Append( list3, res );
			break;
		case DVM_FUNCT_ASSOCIATE :
			DaoMap_Insert2( map, res->xTuple.values[0], res->xTuple.values[1], proc );
			break;
		}
	}
	DaoProcess_PopFrame( proc );
}
static void DaoLIST_Collect2( DaoProcess *proc, DaoValue *p[], int npar )
{
	DaoLIST_Functional2( proc, p, npar, DVM_FUNCT_COLLECT );
}
static void DaoLIST_Associate2( DaoProcess *proc, DaoValue *p[], int npar )
{
	DaoLIST_Functional2( proc, p, npar, DVM_FUNCT_ASSOCIATE );
}
static void DaoLIST_Associate( DaoProcess *proc, DaoValue *p[], int npar )
{
	DaoValue *res = NULL;
	DaoList *list = & p[0]->xList;
	DaoInteger idint = {DAO_INTEGER,0,0,0,0,0};
	DaoValue **items = list->value->items.pValue;
	DaoValue *index = (DaoValue*)(void*)&idint;
	DaoMap *map = DaoProcess_PutMap( proc, DaoProcess_GetHashSeed( proc, p[1] ) );
	DaoVmCode *sect = DaoProcess_InitCodeSection( proc, 2 );
	daoint entry = proc->topFrame->entry;
	daoint i, N = list->value->size;

	if( sect == NULL ) return;
	for(i=0; i<N; i++){
		idint.value = i;
		if( sect->b > 0 ) DaoProcess_SetValue( proc, sect->a, items[i] );
		if( sect->b > 1 ) DaoProcess_SetValue( proc, sect->a+1, index );
		proc->topFrame->entry = entry;
		DaoProcess_Execute( proc );
		if( proc->status == DAO_PROCESS_ABORTED ) break;
		res = proc->stackValues[0];
		if( res->type == DAO_NONE ) continue;
		DaoMap_Insert2( map, res->xTuple.values[0], res->xTuple.values[1], proc );
	}
	DaoProcess_PopFrame( proc );
}
static DaoFuncItem listMeths[] =
{
	{ DaoLIST_New,
		"list<@T=any>( count: int )[index: int => @T] => list<@T>"
	},
	{ DaoLIST_New,
		"list<@T=any>( count: int, init: @T )[index: int, prev: @T => @T] => list<@T>"
	},
	{ DaoLIST_Clear,
		"clear( self: list<@T> )"
		/*
		// Clear the list.
		*/
	},
	{ DaoLIST_Size,
		"size( invar self: list<@T> )=>int"
		/*
		// Return the size of the list.
		*/
	},
	{ DaoLIST_Resize,
		"resize( self: list<@T<bool|int|float|complex|string|enum>>, size: int )"
		/*
		// Resize the list of primitive data to size "size".
		*/
	},
	{ DaoLIST_Resize2,
		"resize( self: list<@T>, value: @T, size: int )"
		/*
		// Resize the list to size "size", and fill the new items with value "value".
		*/
	},
	{ DaoLIST_Max,
		"max( invar self: list<@T<bool|int|float|complex|string|enum>> ) => tuple<@T,int>|none"
		/*
		// Return the maximum value of the list and its index.
		// The list has to contain primitive data.
		// In case of complex values, complex numbers are compared by the real part
		// first, and then by the imaginary part.
		*/
	},
	{ DaoLIST_Min,
		"min( invar self: list<@T<bool|int|float|complex|string|enum>> ) => tuple<@T,int>|none"
		/*
		// Return the minimum value of the list and its index.
		*/
	},
	{ DaoLIST_Sum,
		"sum( invar self: list<@T<bool|int|float|complex|string>> ) => @T"
		/*
		// Return the sum of the list.
		*/
	},
	{ DaoLIST_Insert,
		"insert( self: list<@T>, item: @T, pos = 0 ) => list<@T>"
		/*
		// Insert iten "item" as position "pos".
		// Return the self list;
		*/
	},
	{ DaoLIST_Erase,
		"erase( self: list<@T>, start = 0, count = 1 ) => list<@T>"
		/*
		// Erase from the list "count" items starting from "start".
		// Return the self list;
		*/
	},
	{ DaoLIST_Join,
		"join( self: list<@T>, other: list<@T>, ... : list<@T> ) => list<@T>"
		/*
		// Join none or more lists at the end of the list.
		// Return the self list;
		*/
	},
	{ DaoLIST_PushBack,
		"append( self: list<@T>, item: @T, ... : @T ) => list<@T>"
		/*
		// Append one or more items at the end of the list.
		// Return the self list;
		*/
	},
	{ DaoLIST_Push,
		"push( self: list<@T>, item: @T, to: enum<front, back> = $back ) => list<@T>"
		/*
		// Push an item to the list, either at the front or at the back.
		// Return the self list;
		*/
	},
	{ DaoLIST_Pop,
		"pop( self: list<@T>, from: enum<front,back> = $back ) => @T"
		/*
		// Pop off an item from the list, either from the front or from the end.
		// Return the self list;
		*/
	},
	{ DaoLIST_Front,
		"front( invar self: list<@T> ) => @T"
		/*
		// Get the front item of the list.
		*/
	},
	{ DaoLIST_Back,
		"back( invar self: list<@T> ) => @T"
		/*
		// Get the back item of the list.
		*/
	},
	{ DaoLIST_Collect,
		"collect( invar self: list<@T>, direction: enum<forward,backward> = $forward )"
			"[item: @T, index: int => none|@V] => list<@V>"
		/*
		// Collect the non-"none" values produced by evaluating the code section
		// on the items of the list.
		// The iteration direction can be controlled by the "direction" parameter.
		//
		// Note: invar<@T> will not match to none|@V;
		*/
	},
	{ DaoLIST_Collect2,
		"collect( invar self: list<@T>, invar other: list<@S>, "
			"direction: enum<forward,backward> = $forward )"
			"[item: @T, item2: @S, index: int => none|@V] => list<@V>"
		/*
		// Collect the non-"none" values produced by evaluating the code section
		// on the items of the two lists.
		// The iteration direction can be controlled by the "direction" parameter.
		*/
	},
	{ DaoLIST_Associate,
		"associate( invar self: list<@T>, hashing: enum<none,auto,random>|int = $none )"
			"[item: invar<@T>, index: int => none|tuple<@K,@V>] => map<@K,@V>"
		/*
		// Iterate over this list and evaluate the code section on the item
		// value(s) and index. The code section may return none value, or a
		// pair of key and value as a tuple. These keys and values from the
		// code section will produce a map/hash (associative array) which will
		// be returned by the method.
		//
		// The last optional parameter "hashing" may take the following values:
		// -- "0" or "$none": indicating the resulting map will be ordered by keys;
		// -- "1" or "$auto": indicating the resulting map will be a hash map with
		//                    the default hashing seed;
		// -- "2" or "$random": indicating the resulting map will be a hash map with
		//                      a random hashing seed;
		// -- Other integers: indicating the resulting map will be a hash map with
		//                    this "hashing" value as the hashing seed;
		*/
	},
	{ DaoLIST_Associate2,
		"associate( invar self: list<@T>, invar other: list<@S>,"
			"hashing: enum<none,auto,random>|int = $none )"
			"[item: invar<@T>, item2: invar<@S>, index: int => none|tuple<@K,@V>]"
			"=> map<@K,@V>"
		/*
		// The same as above method except this method iterate over two lists.
		*/
	},
	{ DaoLIST_Reduce1,
		"reduce( invar self: list<@T>, direction: enum<forward,backward> = $forward )"
			"[item: invar<@T>, value: @T, index: int => @T] => @T|none"
		/*
		// Reduce (fold) the items of the list.
		// The process is the following:
		// 1. The first item is taken as the initial and current value;
		// 2. Starting from the second item, each item and the current value are
		//    passed to the code section to evaluate for a new current value;
		// 3. Each new current value will be passed along with the next item
		//    to do the same code section evaluation to update the value.
		// 4. When all items are processed, the current value will be returned.
		//
		// The direction of iteration can be controlled by the "direction" paramter.
		// If the list is empty, "none" will be returned.
		*/
	},
	{ DaoLIST_Reduce2,
		"reduce( invar self: list<@T>, init: @V,"
			"direction: enum<forward,backward> = $forward )"
			"[item: invar<@T>, value: @V, index: int => @V] => @V"
		/*
		// Reduce (fold) the items of the list.
		// The process is essentially the same as the above "reduce()" method,
		// except that:
		// 1. The initial value is passed in as parameter, so the iteration will
		//    start from the first item;
		// 2. The value produced by the code section does not have to be the same
		//    as the items of the list;
		// 3. When the list is empty, the "init" value will be returned.
		*/
	},
	{ DaoLIST_Find,
		"find( invar self: list<@T>, direction: enum<forward,backward> = $forward )"
			"[item: invar<@T>, index: int => bool] => tuple<index:int,value:@T> | none"
		/*
		// Find the first item in the list that meets the condition as expressed
		// by the code section. A true value of the code section indicates
		// the condition is met.
		// The direction of iteration can be controlled by the "direction" paramter.
		*/
	},
	{ DaoLIST_Select,
		"select( invar self: list<@T>, direction: enum<forward,backward> = $forward )"
			"[item: invar<@T>, index: int => bool] => list<@T>"
		/*
		// Select items in the list that meets the condition as expressed
		// by the code section. A true value of the code section indicates
		// the condition is met.
		// The direction of iteration can be controlled by the "direction" paramter.
		*/
	},
	{ DaoLIST_Iterate,
		"iterate( invar self: list<@T>, direction: enum<forward,backward> = $forward )"
			"[item: invar<@T>, index: int]"
		/*
		// Iterate on the list. The direction of iteration can be controlled by
		// the "direction" paramter.
		*/
	},
	{ DaoLIST_Iterate,
		"iterate( self: list<@T>, direction: enum<forward,backward> = $forward )"
			"[item: @T, index: int]"
		/*
		// Iterate on the list. The direction of iteration can be controlled by
		// the "direction" paramter.
		// The only difference from the above "iterate()" method is that this
		// method cannot take constant list as the self parameter.
		*/
	},
	{ DaoLIST_Sort,
		"sort( self: list<@T>, order: enum<ascend,descend> = $ascend, part = 0 )"
			"=> list<@T>"
		/*
		// Sort the list by asceding or descending order. And stops when the
		// largest "part" items (for descending sorting) or the smallest "part"
		// items (for asceding sorting) have been correctly sorted, which also
		// means the first "part" items in the (partially) sorted list are in
		// the right positions. Zero "part" means sorting all items.
		*/
	},
	{ DaoLIST_Sort,
		"sort( self: list<@T>, part = 0 )[X: @T, Y: @T => int] => list<@T>"
		/*
		// Sort the list by ordering as defined by the code section.
		// During the sorting, two items "X" and "Y" will be passed to the code
		// section for comparison, a non-zero value produced by the code section
		// indicates "X" is less/smaller than "Y".
		// The "part" parameter has the same meaning as in the above "sort()" method.
		*/
	},
	{ DaoLIST_Apply,
		"apply( self: list<@T>, direction: enum<forward,backward> = $forward )"
			"[item: @T, index: int => @T] => list<@T>"
		/*
		// Apply new values to the items of the list. Each item and its index are
		// passed to the code section, and values produced by the code section are
		// used to replace the items of the list.
		// The direction of iteration can be controlled by the "direction" paramter.
		*/
	},
	{ NULL, NULL }
};

DaoType* DaoList_GetType( DaoList *self )
{
	return self->ctype;
}
int DaoList_SetType( DaoList *self, DaoType *type )
{
	if( self->value->size || self->ctype ) return 0;
	self->ctype = type;
	GC_IncRC( type );
	return 1;
}
daoint DaoList_Size( DaoList *self )
{
	return self->value->size;
}
DaoValue* DaoList_Front( DaoList *self )
{
	if( self->value->size == 0 ) return NULL;
	return self->value->items.pValue[0];
}
DaoValue* DaoList_Back( DaoList *self )
{
	if( self->value->size == 0 ) return NULL;
	return self->value->items.pValue[ self->value->size-1 ];
}
DaoValue* DaoList_GetItem( DaoList *self, daoint pos )
{
	if( (pos = DaoList_MakeIndex( self, pos, 0 )) == -1 ) return NULL;
	return self->value->items.pValue[pos];
}
DaoTuple* DaoList_ToTuple( DaoList *self, DaoTuple *proto )
{
	/* XXX */
	return NULL;
}
int DaoList_SetItem( DaoList *self, DaoValue *it, daoint pos )
{
	DaoValue **val;
	if( (pos = DaoList_MakeIndex( self, pos, 0 )) == -1 ) return 1;
	val = self->value->items.pValue + pos;
	if( self->ctype && self->ctype->nested->size ){
		return DaoValue_Move( it, val, self->ctype->nested->items.pType[0] ) == 0;
	}else{
		DaoValue_Copy( it, val );
	}
	return 0;
}

int DaoList_Insert( DaoList *self, DaoValue *item, daoint pos )
{
	DaoType *tp = self->ctype ? self->ctype->nested->items.pType[0] : NULL;
	DaoValue *temp = NULL;
	if( (pos = DaoList_MakeIndex( self, pos, 1 )) == -1 ) return 1;
	if( DaoValue_Move( item, & temp, tp ) ==0 ){
		GC_DecRC( temp );
		return 1;
	}
	DList_Insert( self->value, NULL, pos );
	self->value->items.pValue[ pos ] = temp;
	return 0;
}
int DaoList_PushFront( DaoList *self, DaoValue *item )
{
	DaoType *tp = self->ctype ? self->ctype->nested->items.pType[0] : NULL;
	DaoValue *temp = NULL;
	if( DaoValue_Move( item, & temp, tp ) ==0 ){
		GC_DecRC( temp );
		return 1;
	}
	DList_PushFront( self->value, NULL );
	self->value->items.pValue[ 0 ] = temp;
	return 0;
}
int DaoList_PushBack( DaoList *self, DaoValue *item )
{
	DaoType *tp = self->ctype ? self->ctype->nested->items.pType[0] : NULL;
	DaoValue *temp = NULL;
	if( DaoValue_Move( item, & temp, tp ) ==0 ){
		GC_DecRC( temp );
		return 1;
	}
	DList_PushBack( self->value, NULL );
	self->value->items.pValue[ self->value->size - 1 ] = temp;
	return 0;
}
void DaoList_PopFront( DaoList *self )
{
	if( self->value->size ==0 ) return;
	DList_PopFront( self->value );
}
void DaoList_PopBack( DaoList *self )
{
	if( self->value->size ==0 ) return;
	DList_PopBack( self->value );
}

DaoTypeBase listTyper=
{
	"list<@T=any>", & listCore, NULL, (DaoFuncItem*)listMeths, {0}, {0},
	(FuncPtrDel) DaoList_Delete, NULL
};

DaoList* DaoList_New()
{
	DaoList *self = (DaoList*) dao_calloc( 1, sizeof(DaoList) );
	DaoValue_Init( self, DAO_LIST );
	self->value = DList_New( DAO_DATA_VALUE );
	self->value->type = DAO_DATA_VALUE;
	self->ctype = NULL;
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
void DaoList_Delete( DaoList *self )
{
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	GC_DecRC( self->ctype );
	DaoList_Clear( self );
	DList_Delete( self->value );
	dao_free( self );
}
void DaoList_Clear( DaoList *self )
{
	DList_Clear( self->value );
}
int DaoList_Append( DaoList *self, DaoValue *value )
{
	return DaoList_PushBack( self, value );
}
void DaoList_Erase( DaoList *self, daoint pos )
{
	if( pos >= self->value->size ) return;
	DList_Erase( self->value, pos, 1 );
}
DaoList* DaoList_Copy( DaoList *self, DaoType *type )
{
	daoint i;
	DaoList *copy = DaoList_New();
	/* no detailed checking of type matching, must be ensured by caller */
	copy->ctype = (type && type->tid == DAO_LIST) ? type : self->ctype;
	GC_IncRC( copy->ctype );
	DList_Resize( copy->value, self->value->size, NULL );
	for(i=0; i<self->value->size; i++)
		DaoList_SetItem( copy, self->value->items.pValue[i], i );
	return copy;
}

/**/
static void DaoMap_Print( DaoValue *self0, DaoProcess *proc, DaoStream *stream, DMap *cycData )
{
	DNode *node = NULL;
	DaoMap *self = & self0->xMap;
	const char *kvsym = self->value->hashing ? "->" : "=>";
	const daoint size = self->value->size;
	daoint i = 0;

	if( cycData ) node = MAP_Find( cycData, self );
	if( node ){
		DaoStream_WriteChars( stream, "{ ... }" );
		return;
	}
	if( cycData ) MAP_Insert( cycData, self, self );
	DaoStream_WriteChars( stream, "{ " );

	node = DMap_First( self->value );
	for( ; node!=NULL; node=DMap_Next(self->value,node) ){
		if( node->key.pValue->type == DAO_STRING ) DaoStream_WriteChar( stream, '"' );
		DaoValue_Print( node->key.pValue, proc, stream, cycData );
		if( node->key.pValue->type == DAO_STRING ) DaoStream_WriteChar( stream, '"' );
		DaoStream_WriteChar( stream, ' ' );
		DaoStream_WriteChars( stream, kvsym );
		DaoStream_WriteChar( stream, ' ' );
		if( node->value.pValue->type == DAO_STRING ) DaoStream_WriteChar( stream, '"' );
		DaoValue_Print( node->value.pValue, proc, stream, cycData );
		if( node->value.pValue->type == DAO_STRING ) DaoStream_WriteChar( stream, '"' );
		if( i+1<size ) DaoStream_WriteChars( stream, ", " );
		i++;
	}
	if( size==0 ) DaoStream_WriteChars( stream, kvsym );
	DaoStream_WriteChars( stream, " }" );
	if( cycData ) MAP_Erase( cycData, self );
}
static int DaoType_CheckHashableType( DaoType *self )
{
	daoint i;

	if( self == NULL || self->tid == DAO_NONE ) return 0;
	if( self->tid <= DAO_ARRAY ) return 1;
	if( self->tid != DAO_TUPLE && self->tid != DAO_VARIANT ) return 0;

	if( self->tid != DAO_TUPLE && (self->nested == NULL || self->nested->size == 0) ) return 0;
	for(i=0; i<self->nested->size; ++i){
		DaoType *type = self->nested->items.pType[i];
		if( type == NULL ) return 0;
		if( type->tid == DAO_PAR_NAMED ) type = (DaoType*) type->aux;
		if( DaoType_CheckHashableType( type ) == 0 ) return 0;
	}
	return 1;
}
static DaoRoutine* DaoRoutine_FindSnapshotMethod( DaoRoutine *self, DaoEnum *mode )
{
	DaoRoutine *routine = NULL;
	int i, max = 0;
	if( self == NULL ) return NULL;
	for(i=0; i<self->overloads->routines->size; ++i){
		DaoRoutine *rout = self->overloads->routines->items.pRoutine[i];
		DaoType *type;
		if( rout->routType->nested->size <= 2 ) continue;
		type = (DaoType*) rout->routType->nested->items.pType[1]->aux;
		if( DaoType_MatchValue( type, (DaoValue*) mode, NULL ) == 0 ) continue;

		type = (DaoType*) rout->routType->nested->items.pType[2];
		if( type->tid != DAO_TYPE ) continue;
		type = type->nested->items.pType[0];
		if( DaoType_CheckHashableType( type ) == 0 ) continue;
		if( type->tid > max ){
			routine = rout;
			max = type->tid;
		}
	}
	return routine;
}
static DaoValue* DaoMap_AdjustKey( DaoMap *self, DaoValue *key, DaoEnum *sym, DaoProcess *proc )
{
	if( key->type != DAO_ENUM || key->xEnum.subtype != DAO_ENUM_SYM ) return key;
	if( self->ctype->nested->items.pType[0]->tid == DAO_ENUM ){
		*sym = self->ctype->nested->items.pType[0]->value->xEnum;
		if( DaoEnum_SetValue( sym, (DaoEnum*) key ) ) key = (DaoValue*) sym;
	}
	return key;
}
DNode* DaoMap_Find( DaoMap *self, DaoValue *key )
{
	DaoEnum sym = {0};
	return DMap_Find( self->value, DaoMap_AdjustKey( self, key, & sym, NULL ) );
}
DNode* DaoMap_Find2( DaoMap *self, DaoValue *key, DaoProcess *proc )
{
	DaoEnum sym = {0};
	key = DaoMap_AdjustKey( self, key, & sym, proc );
	return DMap_FindPro( self->value, key, DAO_KEY_EQ, proc );
}
static void DaoMap_Erase2( DaoMap *self, DaoValue *key, DaoProcess *proc )
{
	DaoEnum sym = {0};
	DMap_ErasePro( self->value, DaoMap_AdjustKey( self, key, & sym, proc ), proc );
}
static DNode* DaoMap_FindGE( DaoMap *self, DaoValue *key, DaoProcess *proc )
{
	DaoEnum sym = {0};
	key = DaoMap_AdjustKey( self, key, & sym, proc );
	return DMap_FindPro( self->value, key, DAO_KEY_GE, proc );
}
static DNode* DaoMap_FindLE( DaoMap *self, DaoValue *key, DaoProcess *proc )
{
	DaoEnum sym = {0};
	key = DaoMap_AdjustKey( self, key, & sym, proc );
	return DMap_FindPro( self->value, key, DAO_KEY_LE, proc );
}
static void DaoMap_GetItem2( DaoValue *self0, DaoProcess *proc, DaoValue *ids[], int N );
static void DaoMap_GetItem1( DaoValue *self0, DaoProcess *proc, DaoValue *pid )
{
	DaoMap *self = & self0->xMap;
	if( pid->type == DAO_TUPLE && pid->xTuple.ctype == dao_type_for_iterator ){
		DaoTuple *iter = & pid->xTuple;
		DaoTuple *tuple = DaoProcess_PutTuple( proc, 2 );
		DNode *node = (DNode*) iter->values[1]->xCdata.data;
		iter->values[0]->xInteger.value = node != NULL;
		if( node == NULL || tuple->size != 2 ) return;
		DaoValue_Copy( node->key.pValue, tuple->values );
		DaoValue_Copy( node->value.pValue, tuple->values + 1 );
		node = DMap_Next( self->value, node );
		iter->values[0]->xInteger.value = node != NULL;
		iter->values[1]->xCdata.data = node;
	}else if( pid->type == DAO_TUPLE && pid->xTuple.size == 2 ){
		DaoMap_GetItem2( self0, proc, pid->xTuple.values, 2 );
	}else{
		DNode *node = DaoMap_Find2( self, pid, proc );
		if( node ==NULL ){
			DaoProcess_RaiseError( proc, "Key", NULL );
			return;
		}
		DaoProcess_PutValue( proc, node->value.pValue );
	}
}
extern DaoType *dao_type_map_any;
static void DaoMap_SetItem1( DaoValue *self0, DaoProcess *proc, DaoValue *pid, DaoValue *value )
{
	DaoMap *self = & self0->xMap;
	int c = DaoMap_Insert2( self, pid, value, proc );
	if( c == 1 ){
		DaoProcess_RaiseError( proc, "Type", "key not matching" );
	}else if( c == 2 ){
		DaoProcess_RaiseError( proc, "Type", "value not matching" );
	}
}
static void DaoMap_GetItem2( DaoValue *self0, DaoProcess *proc, DaoValue *ids[], int N )
{
	DaoMap *self = & self0->xMap;
	DaoMap *map = DaoProcess_PutMap( proc, self->value->hashing );
	DNode *node1 = DMap_First( self->value );
	DNode *node2 = NULL;
	int cmp = DaoValue_ComparePro( ids[0], ids[1], proc );
	if( cmp > 0 ) return;
	if( ids[0]->type ) node1 = DaoMap_FindGE( self, ids[0], proc );
	if( ids[1]->type ) node2 = DaoMap_FindLE( self, ids[1], proc );
	if( cmp == 0 && node1 != node2 ) return;
	if( node2 ) node2 = DMap_Next(self->value, node2 );
	for(; node1 != node2; node1 = DMap_Next(self->value, node1 ) ){
		DaoMap_Insert2( map, node1->key.pValue, node1->value.pValue, proc );
	}
}
static void DaoMap_GetItem( DaoValue *self, DaoProcess *proc, DaoValue *ids[], int N )
{
	switch( N ){
	case 0 : DaoMap_GetItem1( self, proc, dao_none_value ); break;
	case 1 : DaoMap_GetItem1( self, proc, ids[0] ); break;
	default : DaoProcess_RaiseError( proc, "Index", "multi-indexing not supported" );
	}
}
static void DaoMap_SetItem2( DaoValue *self0, DaoProcess *proc, DaoValue *ids[], int N, DaoValue *value )
{
	DaoMap *self = & self0->xMap;
	DaoType *tp = self->ctype;
	DaoType *tp2=NULL;
	DNode *node1 = DMap_First( self->value );
	DNode *node2 = NULL;
	int cmp = DaoValue_ComparePro( ids[0], ids[1], proc );
	if( cmp > 0 ) return;
	if( tp == NULL ){
		/* a : tuple<string,map<string,int>> = ('',{=>});
		   duplicating the constant to assign to "a" may not set the ctype properly */
		tp = proc->activeTypes[ proc->activeCode->c ];
		if( tp == NULL || tp->tid == DAO_UDT ) tp = dao_type_map_any;
		self->ctype = tp;
		GC_IncRC( tp );
	}
	if( tp ){
		if( tp->nested->size != 2 || tp->nested->items.pType[1] == NULL ){
			DaoProcess_RaiseError( proc, "Type", "invalid map" );
			return;
		}
		tp2 = tp->nested->items.pType[1];
		if( DaoType_MatchValue( tp2, value, NULL ) ==0 )
			DaoProcess_RaiseError( proc, "Type", "value not matching" );
	}
	if( ids[0]->type ) node1 = DaoMap_FindGE( self, ids[0], proc );
	if( ids[1]->type ) node2 = DaoMap_FindLE( self, ids[1], proc );
	if( cmp == 0 && node1 != node2 ) return;
	if( node2 ) node2 = DMap_Next(self->value, node2 );
	for(; node1 != node2; node1 = DMap_Next(self->value, node1 ) ){
		DaoValue_Move( value, & node1->value.pValue, tp2 );
	}
}
static void DaoMap_SetItem( DaoValue *self, DaoProcess *proc, DaoValue *ids[], int N, DaoValue *value )
{
	if( N == 1 ){
		if( ids[0]->xBase.subtype == DAO_PAIR ){
			DaoMap_SetItem2( self, proc, ids[0]->xTuple.values, 2, value );
		}else{
			DaoMap_SetItem1( self, proc, ids[0], value );
		}
	}else if( N == 0 ){
		DaoMap_SetItem1( self, proc, dao_none_value, value );
	}else{
		DaoProcess_RaiseError( proc, "Index", "not supported" );
	}
}
static DaoTypeCore mapCore =
{
	NULL,
	DaoValue_GetField,
	DaoValue_SetField,
	DaoMap_GetItem,
	DaoMap_SetItem,
	DaoMap_Print
};

static void DaoMAP_New( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoInteger idint = {DAO_INTEGER,0,0,0,0,0};
	DaoValue *res, *index = (DaoValue*)(void*)&idint;
	DaoMap *map = DaoProcess_PutMap( proc, DaoProcess_GetHashSeed( proc, p[1] ) );
	daoint i, entry, size = p[0]->xInteger.value;
	DaoVmCode *sect;

	if( size < 0 ){
		DaoProcess_RaiseError( proc, "Param", "Invalid parameter value" );
		return;
	}
	if( size == 0 ) return;
	sect = DaoProcess_InitCodeSection( proc, 1 );
	if( sect == NULL ) return;
	entry = proc->topFrame->entry;
	for(i=0; i<size; i++){
		idint.value = i;
		if( sect->b >0 ) DaoProcess_SetValue( proc, sect->a, index );
		proc->topFrame->entry = entry;
		DaoProcess_Execute( proc );
		if( proc->status == DAO_PROCESS_ABORTED ) break;
		res = proc->stackValues[0];
		if( res->type == DAO_TUPLE && res->xTuple.size == 2 )
			DaoMap_Insert2( map, res->xTuple.values[0], res->xTuple.values[1], proc );
	}
	DaoProcess_PopFrame( proc );
}
static void DaoMAP_Clear( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMap_Clear( & p[0]->xMap );
}
static void DaoMAP_Reset( DaoProcess *proc, DaoValue *p[], int N )
{
	if( N == 0 ){
		DaoMap_Reset( & p[0]->xMap, 1 );
	}else if( N > 1 ){
		DMap *map = p[0]->xMap.value;
		unsigned int hashing = p[1]->xEnum.value;
		if( hashing == 2 ) hashing = rand();
		DaoMap_Reset( & p[0]->xMap, hashing );
	}
}
static void DaoMAP_Erase( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMap *self = (DaoMap*) p[0];
	DNode *ml, *mg;
	DList *keys;
	int cmp;

	DaoProcess_PutValue( proc, p[0] );
	N --;
	switch( N ){
	case 0 :
		DMap_Clear( self->value ); break;
	case 1 :
		DaoMap_Erase2( self, p[1], proc );
		break;
	case 2 :
		cmp = DaoValue_ComparePro( p[0], p[1], proc );
		if( cmp > 0 ) return;
		mg = DaoMap_FindGE( self, p[1], proc );
		ml = DaoMap_FindLE( self, p[2], proc );
		if( cmp == 0 && mg != ml ) return;
		if( mg == NULL || ml == NULL ) return;
		ml = DMap_Next( self->value, ml );
		keys = DList_New(0);
		for(; mg != ml; mg=DMap_Next(self->value, mg)) DList_Append( keys, mg->key.pVoid );
		while( keys->size ){
			MAP_Erase( self->value, keys->items.pVoid[0] );
			DList_PopFront( keys );
		}
		DList_Delete( keys );
		break;
	default : break;
	}
}
static void DaoMAP_Insert( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMap *self = & p[0]->xMap;
	int c = DaoMap_Insert2( self, p[1], p[2], proc );
	DaoProcess_PutValue( proc, p[0] );
	switch( c ){
	case 1 : DaoProcess_RaiseError( proc, "Type", "key not matching" ); break;
	case 2 : DaoProcess_RaiseError( proc, "Type", "value not matching" ); break;
	}
}
static void DaoMAP_Invert( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMap *self = & p[0]->xMap;
	DaoMap *inverted = DaoProcess_PutMap( proc, self->value->hashing );
	DNode *it;
	for(it=DMap_First(self->value); it; it=DMap_Next(self->value,it)){
		DMap_InsertPro( inverted->value, it->value.pValue, it->key.pValue, proc );
	}
}
static void DaoMAP_Find( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMap *self = (DaoMap*) p[0];
	DNode *node = NULL;
	int errors = proc->exceptions->size;
	switch( (int)p[2]->xEnum.value ){
	case 0 : node = DaoMap_FindLE( self, p[1], proc ); break;
	case 1 : node = DaoMap_Find2( self, p[1], proc ); break;
	case 2 : node = DaoMap_FindGE( self, p[1], proc ); break;
	default : break;
	}
	if( proc->exceptions->size > errors ) return;
	if( node ){
		DaoTuple *res = DaoProcess_PutTuple( proc, 2 );
		DaoValue_Copy( node->key.pValue, res->values );
		DaoValue_Copy( node->value.pValue, res->values + 1 );
	}else{
		DaoProcess_PutValue( proc, dao_none_value );
	}
}
static void DaoMAP_Keys( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoList *list = DaoProcess_PutList( proc );
	DaoMap *self = & p[0]->xMap;
	DNode *it;
	for(it=DMap_First(self->value); it; it=DMap_Next(self->value,it)){
		DaoList_Append( list, it->key.pValue );
	}
}
static void DaoMAP_Values( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoList *list = DaoProcess_PutList( proc );
	DaoMap *self = & p[0]->xMap;
	DNode *it;
	for(it=DMap_First(self->value); it; it=DMap_Next(self->value,it)){
		DaoList_Append( list, it->value.pValue );
	}
}
static void DaoMAP_Size( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMap *self = & p[0]->xMap;
	DaoProcess_PutInteger( proc, self->value->size );
}
static void DaoMAP_Functional( DaoProcess *proc, DaoValue *p[], int N, int funct )
{
	DaoMap *self = & p[0]->xMap;
	DaoMap *map = NULL;
	DaoList *list = NULL;
	DaoTuple *tuple = NULL;
	DaoType *type = self->ctype;
	DaoVmCode *sect = DaoProcess_InitCodeSection( proc, 2 );
	unsigned int hashing;
	int entry, popped = 0;
	DaoValue *res;
	DNode *node;
	switch( funct ){
	case DVM_FUNCT_ASSOCIATE :
		hashing = DaoProcess_GetHashSeed( proc, p[1] );
		map = DaoProcess_PutMap( proc, hashing );
		break;
	case DVM_FUNCT_COLLECT : list = DaoProcess_PutList( proc ); break;
	case DVM_FUNCT_APPLY : DaoProcess_PutValue( proc, p[0] ); break;
	case DVM_FUNCT_FIND : DaoProcess_PutValue( proc, dao_none_value ); break;
	case DVM_FUNCT_SELECT : map = DaoProcess_PutMap( proc, self->value->hashing ); break;
	}
	if( sect == NULL ) return;
	entry = proc->topFrame->entry;
	type = type && type->nested->size > 1 ? type->nested->items.pType[1] : NULL;
	for(node=DMap_First(self->value); node; node=DMap_Next(self->value,node)){
		if( sect->b >0 ) DaoProcess_SetValue( proc, sect->a, node->key.pValue );
		if( sect->b >1 ) DaoProcess_SetValue( proc, sect->a+1, node->value.pValue );
		proc->topFrame->entry = entry;
		DaoProcess_Execute( proc );
		if( proc->status == DAO_PROCESS_ABORTED ) break;
		res = proc->stackValues[0];
		switch( funct ){
		case DVM_FUNCT_ASSOCIATE :
			if( res->type != DAO_NONE ){
				DaoMap_Insert2( map, res->xTuple.values[0], res->xTuple.values[1], proc );
			}
			break;
		case DVM_FUNCT_SELECT :
			if( res->xBoolean.value ){
				DaoMap_Insert2( map, node->key.pVoid, node->value.pVoid, proc );
			}
			break;
		case DVM_FUNCT_APPLY : DaoValue_Move( res, & node->value.pValue, type ); break;
		case DVM_FUNCT_COLLECT : if( res->type != DAO_NONE ) DaoList_Append( list, res ); break;
		}
		if( funct == DVM_FUNCT_FIND && res->xBoolean.value ){
			popped = 1;
			DaoProcess_PopFrame( proc );
			tuple = DaoProcess_PutTuple( proc, 2 );
			GC_Assign( & tuple->values[0], node->key.pValue );
			GC_Assign( & tuple->values[1], node->value.pValue );
			break;
		}
	}
	if( popped == 0 ) DaoProcess_PopFrame( proc );
}
static void DaoMAP_Iterate( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMAP_Functional( proc, p, N, DVM_FUNCT_ITERATE );
}
static void DaoMAP_Find2( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMAP_Functional( proc, p, N, DVM_FUNCT_FIND );
}
static void DaoMAP_Select( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMAP_Functional( proc, p, N, DVM_FUNCT_SELECT );
}
static void DaoMAP_Associate( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMAP_Functional( proc, p, N, DVM_FUNCT_ASSOCIATE );
}
static void DaoMAP_Collect( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMAP_Functional( proc, p, N, DVM_FUNCT_COLLECT );
}
static void DaoMAP_Apply( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMAP_Functional( proc, p, N, DVM_FUNCT_APPLY );
}
static DaoFuncItem mapMeths[] =
{
	{ DaoMAP_New,
		"map<@K=any,@V=any>( count: int, hashing: enum<none,auto,random>|int = $none )"
			"[index: int => tuple<@K,@V>] => map<@K,@V>"
		/*
		// The last optional parameter "hashing" may take the following values:
		// -- "0" or "$none": indicating the resulting map will be ordered by keys;
		// -- "1" or "$auto": indicating the resulting map will be a hash map with
		//                    the default hashing seed;
		// -- "2" or "$random": indicating the resulting map will be a hash map with
		//                      a random hashing seed;
		// -- Other integers: indicating the resulting map will be a hash map with
		//                    this "hashing" value as the hashing seed;
		*/
	},
	{ DaoMAP_Clear,
		"clear( self: map<@K,@V> )"
		/*
		// Delete all the key-value pairs from the map.
		*/
	},
	{ DaoMAP_Reset,
		"reset( self: map<@K,@V> )"
		/*
		// Remove all the key-value pairs from the map. And cache the key-value
		// nodes internally for more efficient insertion later.
		*/
	},
	{ DaoMAP_Reset,
		"reset( self: map<@K,@V>, hashing: enum<none,auto,random> )"
		/*
		// Reset the map in the same way as the above method.
		// And additionally it may change the map from ordered (hashing=none)
		// to unordered (hashing=auto/random), or unordered to ordered map.
		// A default hashing seed will be used for "auto", and a random
		// hashing seed will be used for "random".
		*/
	},
	{ DaoMAP_Erase,
		"erase( self: map<@K,@V>, from: @K ) => map<@K,@V>"
		/*
		// Erase a key and its corresponding value from the map.
		// Return self map.
		*/
	},
	{ DaoMAP_Erase,
		"erase( self: map<@K,@V>, from: @K, to: @K ) => map<@K,@V>"
		/*
		// Erase keys in the inclusive range between "from" and "to",
		// and their corresponding values from the map.
		// Return self map.
		*/
	},
	{ DaoMAP_Insert,
		"insert( self: map<@K,@V>, key: @K, value: @V ) => map<@K,@V>"
		/*
		// Insert a new key-value pair to the map.
		// Return self map.
		*/
	},
	{ DaoMAP_Invert,
		"invert( self: map<@K,@V> ) => map<@V,@K>"
		/*
		// Invert the key-value relationship.
		// Return a new map.
		*/
	},
	{ DaoMAP_Find,
		"find( invar self: map<@K,@V>, invar key: @K, comparison: enum<LE,EQ,GE> = $EQ )"
			"=> tuple<key:@K,value:@V> | none"
		/*
		// Find the key-value pair that corresponds (or is closest) to "key".
		// According to the "comparison" parameter:
		// 1. "$LE": the key must be less than or equal to the found one;
		// 2. "$EQ": the key must equal to the found one;
		// 3. "$GE": the key must be greater than or equal to the found one;
		*/
	},
	{ DaoMAP_Keys,
		"keys( invar self: map<@K,@V> ) => list<@K>"
		/*
		// Return the keys of the map as a list.
		*/
	},
	{ DaoMAP_Values,
		"values( invar self: map<@K,@V> ) => list<@V>"
		/*
		// Return the values of the map as a list.
		*/
	},
	{ DaoMAP_Size,
		"size( invar self: map<@K,@V> ) => int"
		/*
		// Return the number of key-value pairs in map.
		*/
	},
	{ DaoMAP_Iterate,
		"iterate( invar self: map<@K,@V> )[key: invar<@K>, value: invar<@V>]"
		/*
		// Iterate over the map, and execute the associated code section
		// for each key-value pair.
		*/
	},
	{ DaoMAP_Iterate,
		"iterate( self: map<@K,@V> )[key: invar<@K>, value: @V]"
		/*
		// Iterate over the map, and execute the associated code section
		// for each key-value pair.
		*/
	},
	{ DaoMAP_Collect,
		"collect( invar self: map<@K,@V> )[key: @K, value: @V => none|@T] => list<@T>"
		/*
		// Iterate over the map, and execute the associated code section
		// for each key-value pair.
		// Return a list of non-none values collected from the code section results.
		*/
	},
	{ DaoMAP_Associate,
		"associate( invar self: map<@K,@V>, hashing: enum<none,auto,random>|int = $none )"
			"[key: invar<@K>, value: invar<@V> => none|tuple<@K2,@V2>] => map<@K2,@V2>"
		/*
		// Iterate over the map, and execute the associated code section
		// for each key-value pair.
		// Return a new map that is constructed from the new key-value pairs returned
		// from the code section evaluation.
		*/
	},
	{ DaoMAP_Find2,
		"find( invar self: map<@K,@V> )[key: invar<@K>, value: invar<@V> =>bool]"
			"=> tuple<key:@K,value:@V> | none"
		/*
		// Find the first key-value pair that meets the condition as expressed
		// by the code section. A true value from the code section means the
		// condition is satisfied.
		*/
	},
	{ DaoMAP_Select,
		"select( invar self: map<@K,@V> )[key: invar<@K>, value: invar<@V> =>bool]"
			"=> map<@K,@V>"
		/*
		// Select key-value pairs that meets the condition as expressed
		// by the code section. A true from the code section means the
		// condition is satisfied.
		*/
	},
	{ DaoMAP_Apply,
		"apply( self: map<@K,@V> )[key: @K, value: @V => @V] => map<@K,@V>"
		/*
		// Iterate over the map, and execute the associated code section
		// for each key-value pair.
		// Then update the value of each pair with the value returned by
		// the code section.
		*/
	},
	{ NULL, NULL }
};

DaoType* DaoMap_GetType( DaoMap *self )
{
	return self->ctype;
}
int DaoMap_SetType( DaoMap *self, DaoType *type )
{
	if( self->value->size || self->ctype ) return 0;
	self->ctype = type;
	GC_IncRC( type );
	return 1;
}
daoint DaoMap_Size( DaoMap *self )
{
	return self->value->size;
}
DaoValue* DaoMap_GetValue( DaoMap *self, DaoValue *key  )
{
	DNode *node = MAP_Find( self->value, key );
	if( node ) return node->value.pValue;
	return NULL;
}
int DaoMap_InsertChars( DaoMap *self, const char *key, DaoValue *value )
{
	DaoString vkey = { DAO_STRING,0,0,0,1,NULL};
	DString str = DString_WrapChars( key );
	vkey.value = & str;
	return DaoMap_Insert( self, (DaoValue*) & vkey, value );
}
void DaoMap_EraseChars( DaoMap *self, const char *key )
{
	DaoString vkey = { DAO_STRING,0,0,0,1,NULL};
	DString str = DString_WrapChars( key );
	vkey.value = & str;
	DaoMap_Erase( self, (DaoValue*) & vkey );
}
DaoValue* DaoMap_GetValueChars( DaoMap *self, const char *key  )
{
	DaoString vkey = { DAO_STRING,0,0,0,1,NULL};
	DString str = DString_WrapChars( key );
	DNode *node;
	vkey.value = & str;
	node = MAP_Find( self->value, (DaoValue*) &  vkey );
	if( node ) return node->value.pValue;
	return NULL;
}

DaoTypeBase mapTyper=
{
	"map<@K=any,@V=any>", & mapCore, NULL, (DaoFuncItem*) mapMeths, {0}, {0},
	(FuncPtrDel)DaoMap_Delete, NULL
};

DaoMap* DaoMap_New( unsigned int hashing )
{
	DaoMap *self = (DaoMap*) dao_malloc( sizeof( DaoMap ) );
	DaoValue_Init( self, DAO_MAP );
	self->value = hashing ? DHash_New( DAO_DATA_VALUE, DAO_DATA_VALUE ) : DMap_New( DAO_DATA_VALUE, DAO_DATA_VALUE );
	self->ctype = NULL;
	if( hashing > 1 ) self->value->hashing = hashing;
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
void DaoMap_Delete( DaoMap *self )
{
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	GC_DecRC( self->ctype );
	DaoMap_Clear( self );
	DMap_Delete( self->value );
	dao_free( self );
}
void DaoMap_Clear( DaoMap *self )
{
	DMap_Clear( self->value );
}
void DaoMap_Reset( DaoMap *self, unsigned int hashing )
{
	DMap *map = self->value;

	DMap_Reset( self->value );
	if( hashing == 1 ) return;

	if( hashing == 0 ){
		map->hashing = 0;
		if( map->table ) dao_free( map->table );
		map->table = NULL;
		map->tsize = 0;
	}else{
		if( map->hashing == 0 ){
			map->tsize = 4;
			map->table = (DNode**) dao_calloc( map->tsize, sizeof(DNode*) );
		}
		map->hashing = hashing;
	}
}
DaoMap* DaoMap_Copy( DaoMap *self, DaoType *type )
{
	DaoMap *copy = DaoMap_New( self->value->hashing );
	DNode *node = DMap_First( self->value );
	copy->ctype = (type && type->tid == DAO_MAP) ? type : self->ctype;
	GC_IncRC( copy->ctype );
	for( ; node !=NULL; node = DMap_Next(self->value, node ))
		DaoMap_Insert( copy, node->key.pValue, node->value.pValue );
	return copy;
}
int DaoMap_Insert2( DaoMap *self, DaoValue *key, DaoValue *value, DaoProcess *proc )
{
	DaoType *tp = self->ctype;
	DaoType *tp1=NULL, *tp2=NULL;
	DaoValue *key2 = NULL;
	DaoValue *value2 = NULL;
	int mt;
	if( tp ){
		if( tp->nested->size >=2 ){
			tp1 = tp->nested->items.pType[0];
			tp2 = tp->nested->items.pType[1];
		}else if( tp->nested->size >=1 ){
			tp1 = tp->nested->items.pType[0];
		}
	}
	/* type checking and setting */
	if( tp1 ){
		if( (mt = DaoType_MatchValue( tp1, key, NULL )) ==0 ) return 1;
		if( mt != DAO_MT_EQ ){
			if( DaoValue_Move( key, & key2, tp1 ) == 0 ){
				GC_DecRC( key2 );
				return 1;
			}
			key = key2;
		}
	}
	if( tp2 ){
		if( (mt = DaoType_MatchValue( tp2, value, NULL )) ==0 ) return 2;
		if( mt != DAO_MT_EQ ){
			if( DaoValue_Move( value, & value2, tp2 ) == 0 ){
				GC_DecRC( value2 );
				return 2;
			}
			value = value2;
		}
	}
	DMap_InsertPro( self->value, key, value, proc );
	GC_DecRC( key2 );
	GC_DecRC( value2 );
	return 0;
}
int DaoMap_Insert( DaoMap *self, DaoValue *key, DaoValue *value )
{
	return DaoMap_Insert2( self, key, value, NULL );
}
void DaoMap_Erase( DaoMap *self, DaoValue *key )
{
	DaoEnum sym = {0};
	MAP_Erase( self->value, DaoMap_AdjustKey( self, key, & sym, NULL ) );
}
DNode* DaoMap_First( DaoMap *self )
{
	return DMap_First( self->value );
}
DNode* DaoMap_Next( DaoMap *self, DNode *iter )
{
	return DMap_Next( self->value, iter );
}



/* ---------------------
 * Dao Tuple
 * ---------------------*/
static int DaoTuple_GetIndexE( DaoTuple *self, DaoProcess *proc, DString *name )
{
	int id = DaoTuple_GetIndex( self, name );
	if( id <0 || id >= self->size ){
		DaoProcess_RaiseError( proc, "Field::NonExist", "invalid tuple" );
		return -1;
	}
	return id;
}
static void DaoTupleCore_GetField( DaoValue *self0, DaoProcess *proc, DString *name )
{
	DaoTuple *self = & self0->xTuple;
	int id = DaoTuple_GetIndexE( self, proc, name );
	if( id <0 ) return;
	DaoProcess_PutValue( proc, self->values[id] );
}
static void DaoTupleCore_SetField( DaoValue *self0, DaoProcess *proc, DString *name, DaoValue *value )
{
	DaoTuple *self = & self0->xTuple;
	DaoType *t, **type = self->ctype->nested->items.pType;
	int id = DaoTuple_GetIndexE( self, proc, name );
	if( id <0 ) return;
	t = type[id];
	if( t->tid == DAO_PAR_NAMED ) t = & t->aux->xType;
	if( DaoValue_Move( value, self->values + id, t ) ==0)
		DaoProcess_RaiseError( proc, "Type", "type not matching" );
}
DaoTuple* DaoProcess_GetTuple( DaoProcess *self, DaoType *type, int size, int init );
static void DaoTupleCore_GetCopy( DaoTuple *self, DaoProcess *proc )
{
	DaoTuple *tuple = DaoProcess_GetTuple( proc, self->ctype, self->size, 1 );
	int i;
	for(i=0; i<self->size; i++) DaoTuple_SetItem( tuple, self->values[i], i );
}
static void DaoTupleCore_GetItem1( DaoValue *self0, DaoProcess *proc, DaoValue *pid )
{
	DaoTuple *self = & self0->xTuple;
	int ec = DAO_ERROR_INDEX;
	if( pid->type == DAO_NONE ){
		ec = 0;
		DaoTupleCore_GetCopy( self, proc );
	}else if( pid->type >= DAO_INTEGER && pid->type <= DAO_FLOAT ){
		int id = DaoValue_GetInteger( pid );
		if( id >=0 && id < self->size ){
			DaoProcess_PutValue( proc, self->values[id] );
			ec = 0;
		}else{
			ec = DAO_ERROR_INDEX_OUTOFRANGE;
		}
	}else if( pid->type == DAO_TUPLE && pid->xTuple.ctype == dao_type_for_iterator ){
		int id = DaoValue_GetInteger( pid->xTuple.values[1] );
		if( id >=0 && id < self->size ){
			DaoValue **data = pid->xTuple.values;
			DaoTuple *tup = DaoProcess_PutTuple( proc, 2 );
			DaoValue_Move( self->values[id], & tup->values[1], NULL );
			DString_Reset( tup->values[0]->xString.value, 0 );
			if( id < self->ctype->nested->size ){
				DaoType *itype = self->ctype->nested->items.pType[id];
				if( itype->tid == DAO_PAR_NAMED ){
					DString_Assign( tup->values[0]->xString.value, itype->fname );
				}
			}
			data[1]->xInteger.value += 1;
			data[0]->xInteger.value = data[1]->xInteger.value < self->size;
			ec = 0;
		}else{
			ec = DAO_ERROR_INDEX_OUTOFRANGE;
		}
	}else if( pid->type == DAO_TUPLE && pid->xTuple.subtype == DAO_PAIR ){
		DaoTuple *tuple;
		DaoType *type = proc->activeTypes[ proc->activeCode->c ];
		DaoValue *first = pid->xTuple.values[0];
		DaoValue *second = pid->xTuple.values[1];
		daoint start = DaoValue_GetInteger( first );
		daoint i, end = DaoValue_GetInteger( second );
		if( start < 0 || end < 0 ) goto InvIndex; /* No support for negative index; */
		if( start >= self->size || end >= self->size ) goto InvIndex;
		if( first->type > DAO_FLOAT || second->type > DAO_FLOAT ) goto InvIndex;
		if( first->type == DAO_NONE && second->type == DAO_NONE ){
			DaoTupleCore_GetCopy( self, proc );
		}else{
			if( type->tid != DAO_TUPLE ) type = dao_type_tuple;
			end = second->type == DAO_NONE ? self->size : end + 1;
			tuple = DaoProcess_GetTuple( proc, NULL, end - start, 0 );
			GC_Assign( & tuple->ctype, type );
			for(i=start; i<end; i++) DaoTuple_SetItem( tuple, self->values[i], i-start );
		}
		return;
InvIndex:
		DaoProcess_RaiseError( proc, "Index::Range", NULL );
	}
	if( ec ) DaoProcess_RaiseException( proc, daoExceptionNames[ec], NULL, NULL );
}
static void DaoTupleCore_SetItem1( DaoValue *self0, DaoProcess *proc, DaoValue *pid, DaoValue *value )
{
	DaoTuple *self = & self0->xTuple;
	DaoType *t, **type = self->ctype->nested->items.pType;
	int ec = 0;
	if( pid->type >= DAO_INTEGER && pid->type <= DAO_FLOAT ){
		int id = DaoValue_GetInteger( pid );
		if( id >=0 && id < self->size ){
			t = type[id];
			if( t->tid == DAO_PAR_NAMED ) t = & t->aux->xType;
			if( DaoValue_Move( value, self->values + id, t ) ==0 ) ec = DAO_ERROR_TYPE;
		}else{
			ec = DAO_ERROR_INDEX_OUTOFRANGE;
		}
	}else{
		ec = DAO_ERROR_INDEX;
	}
	if( ec ) DaoProcess_RaiseException( proc, daoExceptionNames[ec], NULL, NULL );
}
static void DaoTupleCore_GetItem( DaoValue *self, DaoProcess *proc, DaoValue *ids[], int N )
{
	switch( N ){
	case 0 : DaoTupleCore_GetItem1( self, proc, dao_none_value ); break;
	case 1 : DaoTupleCore_GetItem1( self, proc, ids[0] ); break;
	default : DaoProcess_RaiseError( proc, "Index", "not supported" );
	}
}
static void DaoTupleCore_SetItem( DaoValue *self, DaoProcess *proc, DaoValue *ids[], int N, DaoValue *value )
{
	switch( N ){
	case 0 : DaoTupleCore_SetItem1( self, proc, dao_none_value, value ); break;
	case 1 : DaoTupleCore_SetItem1( self, proc, ids[0], value ); break;
	default : DaoProcess_RaiseError( proc, "Index", "not supported" );
	}
}
static void DaoTupleCore_Print( DaoValue *self0, DaoProcess *proc, DaoStream *stream, DMap *cycData )
{
	DaoTuple *self = (DaoTuple*) self0;
	Dao_Print( self0, self->values, self->size, '(', ')', proc, stream, cycData );
}
static DaoTypeCore tupleCore=
{
	NULL,
	DaoTupleCore_GetField,
	DaoTupleCore_SetField,
	DaoTupleCore_GetItem,
	DaoTupleCore_SetItem,
	DaoTupleCore_Print
};
DaoTypeBase tupleTyper=
{
	"tuple", & tupleCore, NULL, NULL, {0}, {0}, (FuncPtrDel) DaoTuple_Delete, NULL
};
DaoTuple* DaoTuple_New( int size )
{
	int extra = size > DAO_TUPLE_MINSIZE ? size - DAO_TUPLE_MINSIZE : 0;
	DaoTuple *self = (DaoTuple*) dao_calloc( 1, sizeof(DaoTuple) + extra*sizeof(DaoValue*) );
	DaoValue_Init( self, DAO_TUPLE );
	self->size = size;
	self->ctype = NULL;
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
#if 0
DaoTuple* DaoTuple_Create( DaoType *type, int init )
{
	int i, size = type->nested->size;
	int extit = size > DAO_TUPLE_MINSIZE ? size - DAO_TUPLE_MINSIZE : 0;
	DaoType **types = type->nested->items.pType;
	DaoTuple *self = (DaoTuple*) dao_calloc( 1, sizeof(DaoTuple) + extit*sizeof(DaoValue*) );
	self->type = DAO_TUPLE;
	if( init ){
		for(i=0; i<size; i++){
			DaoType *it = types[i];
			if( it->tid == DAO_PAR_NAMED ) it = & it->aux->xType;
			if( it->tid > DAO_ENUM && it->tid != DAO_ANY && it->tid != DAO_INITYPE ) continue;
			DaoValue_Move( it->value, self->values + i, it );
		}
	}
	GC_IncRC( type );
	self->size = size;
	self->ctype = type;
	return self;
}
#else
DaoTuple* DaoTuple_Create( DaoType *type, int N, int init )
{
	int M = type->nested->size;
	int i, size = N > (M - type->variadic) ? N : (M - type->variadic);
	int extit = size > DAO_TUPLE_MINSIZE ? size - DAO_TUPLE_MINSIZE : 0;
	DaoTuple *self = (DaoTuple*) dao_calloc( 1, sizeof(DaoTuple) + extit*sizeof(DaoValue*) );
	DaoType **types;

	DaoValue_Init( self, DAO_TUPLE );
	GC_IncRC( type );
	self->size = size;
	self->ctype = type;
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	if( init == 0 || M == 0 ) return self;
	types = type->nested->items.pType;
	for(i=0; i<size; i++){
		DaoType *it = i < M ? types[i] : types[M-1];
		if( it->tid == DAO_PAR_NAMED || it->tid == DAO_PAR_VALIST ) it = & it->aux->xType;
		if( it->tid >= DAO_BOOLEAN && it->tid <= DAO_ENUM ){
			DaoValue_Move( it->value, self->values + i, it );
		}
	}
	return self;
}
#endif
DaoTuple* DaoTuple_Copy( DaoTuple *self, DaoType *type )
{
	int i, n;
	DaoTuple *copy = DaoTuple_New( self->size );
	copy->subtype = self->subtype;
	copy->ctype = (type && type->tid == DAO_TUPLE) ? type : self->ctype;
	GC_IncRC( copy->ctype );
	for(i=0,n=self->size; i<n; i++) DaoTuple_SetItem( copy, self->values[i], i );
	return copy;
}
void DaoTuple_Delete( DaoTuple *self )
{
	int i;
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	for(i=0; i<self->size; i++) GC_DecRC( self->values[i] );
	GC_DecRC( self->ctype );
	dao_free( self );
}

DaoType* DaoTuple_GetType( DaoTuple *self )
{
	return self->ctype;
}
int DaoTuple_SetType( DaoTuple *self, DaoType *type )
{
	if( self->size || self->ctype ) return 0;
	self->ctype = type;
	GC_IncRC( type );
	return 1;
}
int  DaoTuple_Size( DaoTuple *self )
{
	return self->size;
}
int DaoTuple_GetIndex( DaoTuple *self, DString *name )
{
	DaoType *abtp = self->ctype;
	DNode *node = NULL;
	if( abtp && abtp->mapNames ) node = MAP_Find( abtp->mapNames, name );
	if( node == NULL || node->value.pInt >= self->size ) return -1;
	return node->value.pInt;
}
void DaoTuple_SetItem( DaoTuple *self, DaoValue *it, int pos )
{
	DaoValue **val;
	if( pos < 0 ) pos += self->size;
	if( pos < 0 || pos >= self->size ) return;
	val = self->values + pos;
	if( self->ctype && pos < self->ctype->nested->size ){
		DaoType *t = self->ctype->nested->items.pType[pos];
		if( t->tid == DAO_PAR_NAMED || t->tid == DAO_PAR_VALIST ) t = & t->aux->xType;
		DaoValue_Move( it, val, t );
	}else{
		DaoValue_Copy( it, val );
	}
}
DaoValue* DaoTuple_GetItem( DaoTuple *self, int pos )
{
	if( pos <0 || pos >= self->size ) return NULL;
	return self->values[pos];
}



void DaoNameValue_Delete( DaoNameValue *self )
{
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	DString_Delete( self->name );
	DaoValue_Clear( & self->value );
	GC_DecRC( self->ctype );
	dao_free( self );
}
static void DaoNameValue_Print( DaoValue *self0, DaoProcess *proc, DaoStream *stream, DMap *cycData )
{
	DaoNameValue *self = & self0->xNameValue;
	DaoStream_WriteString( stream, self->name );
	DaoStream_WriteChars( stream, "=>" );
	if( self->value && self->value->type == DAO_STRING ) DaoStream_WriteChar( stream, '"' );
	DaoValue_Print( self->value, proc, stream, cycData );
	if( self->value && self->value->type == DAO_STRING ) DaoStream_WriteChar( stream, '"' );
}
static DaoTypeCore namevaCore=
{
	NULL,
	DaoValue_GetField,
	DaoValue_SetField,
	DaoValue_GetItem,
	DaoValue_SetItem,
	DaoNameValue_Print
};
DaoTypeBase namevaTyper =
{
	"NameValue", & namevaCore, NULL, NULL, {0}, {0}, (FuncPtrDel) DaoNameValue_Delete, NULL
};
DaoNameValue* DaoNameValue_New( DString *name, DaoValue *value )
{
	DaoNameValue *self = (DaoNameValue*)dao_calloc( 1, sizeof(DaoNameValue) );
	DaoValue_Init( self, DAO_PAR_NAMED );
	self->name = DString_Copy( name );
	DaoValue_Copy( value, & self->value );
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}




/**/
void DaoCstruct_Init( DaoCstruct *self, DaoType *type )
{
	DaoType *intype = type;
	if( type == NULL ) type = dao_type_cdata;
	DaoValue_Init( self, type ? type->tid : DAO_CDATA );
	self->object = NULL;
	self->ctype = type;
	if( self->ctype ) GC_IncRC( self->ctype );
#ifdef DAO_USE_GC_LOGGER
	if( intype != NULL ) DaoObjectLogger_LogNew( (DaoValue*) self );
#endif

}
void DaoCstruct_Free( DaoCstruct *self )
{
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	if( self->ctype && !(self->trait & DAO_VALUE_BROKEN) ) GC_DecRC( self->ctype );
	if( self->object ) GC_DecRC( self->object );
	self->object = NULL;
	self->ctype = NULL;
}
DaoCdata* DaoCdata_New( DaoType *type, void *data )
{
	DaoCdata *self = (DaoCdata*)dao_calloc( 1, sizeof(DaoCdata) );
	DaoCstruct_Init( (DaoCstruct*)self, type );
	self->subtype = DAO_CDATA_CXX;
	self->data = data;
#ifdef DAO_USE_GC_LOGGER
	if( type == NULL ) DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
DaoCdata* DaoCdata_Wrap( DaoType *type, void *data )
{
	DaoCdata *self = DaoCdata_New( type, data );
	self->subtype = DAO_CDATA_PTR;
	return self;
}
static void DaoCdata_DeleteData( DaoCdata *self );
void DaoCdata_Delete( DaoCdata *self )
{
	if( self->type == DAO_CTYPE ){
		DaoCtype_Delete( (DaoCtype*) self );
		return;
	}
	DaoCdata_DeleteData( self );
	dao_free( self );
}
void DaoCdata_DeleteData( DaoCdata *self )
{
	void (*fdel)(void*) = (void (*)(void *))DaoCdata_Delete;
	if( self->subtype == DAO_CDATA_CXX && self->data != NULL ){
		if( self->ctype->typer->Delete && self->ctype->typer->Delete != fdel ){
			self->ctype->typer->Delete( self->data );
		}else{
			dao_free( self->data );
		}
		self->data = NULL;
	}
	DaoCstruct_Free( (DaoCstruct*)self );
}
int DaoCdata_IsType( DaoCdata *self, DaoType *type )
{
	return DaoType_ChildOf( self->ctype, type );
}
int DaoCdata_OwnData( DaoCdata *self )
{
	return self->subtype == DAO_CDATA_CXX;
}
void DaoCdata_SetType( DaoCdata *self, DaoType *type )
{
	if( type == NULL ) return;
	GC_Assign( & self->ctype, type );
}
void DaoCdata_SetData( DaoCdata *self, void *data )
{
	self->data = data;
}
void* DaoCdata_GetData( DaoCdata *self )
{
	return self->data;
}
void** DaoCdata_GetData2( DaoCdata *self )
{
	return & self->data;
}
DaoObject* DaoCdata_GetObject( DaoCdata *self )
{
	return (DaoObject*)self->object;
}
static void* DaoType_CastCxxData( DaoType *self, DaoType *totype, void *data )
{
	daoint i, n;
	if( self == totype || totype == NULL || data == NULL ) return data;
	if( self->bases == NULL ) return NULL;
	for(i=0,n=self->bases->size; i<n; i++){
		void *p = self->typer->casts[i] ? (*self->typer->casts[i])( data, 0 ) : data;
		p = DaoType_CastCxxData( self->bases->items.pType[i], totype, p );
		if( p ) return p;
	}
	return NULL;
}
void* DaoCdata_CastData( DaoCdata *self, DaoType *totype )
{
	if( self == NULL || self->ctype == NULL || self->data == NULL ) return self->data;
	return DaoType_CastCxxData( self->ctype, totype, self->data );
}


DaoCtype* DaoCtype_New( DaoType *cttype, DaoType *cdtype )
{
	DaoCtype *self = (DaoCtype*)dao_calloc( 1, sizeof(DaoCtype) );
	DaoCstruct_Init( (DaoCstruct*)self, cttype );
	GC_IncRC( cdtype );
	self->cdtype = cdtype;
	self->type = DAO_CTYPE;
	self->name = DString_New();
	self->info = DString_New();
	if( cdtype ) DString_Assign( self->name, cdtype->name );
#ifdef DAO_USE_GC_LOGGER
	if( cttype == NULL ) DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
void DaoCtype_Delete( DaoCtype *self )
{
	DaoCstruct_Free( (DaoCstruct*) self );
	DString_Delete( self->name );
	DString_Delete( self->info );
	GC_DecRC( self->cdtype );
	dao_free( self );
}

DaoTypeBase defaultCdataTyper =
{
	"cdata", NULL, NULL, NULL, {0}, {0},
	(FuncPtrDel)DaoCdata_Delete, NULL
};



/*
// In analog to Dao classes, two type objects are created for each cdata type:
// one for the cdata type type, the other for the cdata object type.
// Additionally, two dummy cdata objects are created:
// one with typeid DAO_CTYPE serves an auxiliary value for the two type objects;
// the other with typeid DAO_CDATA serves as the default value for the cdata object type.
*/
DaoType* DaoCdata_NewType( DaoTypeBase *typer )
{
	DaoCtype *ctype = DaoCtype_New( NULL, NULL );
	DaoType *cdata_type;
	DaoType *ctype_type;
	int i;

	DString_SetChars( ctype->name, typer->name );
	ctype->subtype = DAO_CDATA_PTR;
	ctype->trait |= DAO_VALUE_NOCOPY;

	ctype_type = DaoType_New( typer->name, DAO_CTYPE, (DaoValue*)ctype, NULL );
	cdata_type = DaoType_New( typer->name, DAO_CDATA, (DaoValue*)ctype, NULL );
	GC_Assign( & ctype->cdtype, cdata_type );
	GC_Assign( & ctype->ctype, ctype_type );
	ctype_type->typer = typer;
	cdata_type->typer = typer;

	for(i=0; i<DAO_MAX_CDATA_SUPER; i++){
		DaoTypeBase *sup = typer->supers[i];
		if( sup == NULL ) break;
		if( sup->core == NULL || sup->core->kernel->abtype == NULL ){
			printf( "parent type is not wrapped (successfully): %s\n", typer->name );
			return NULL;
		}
		if( ctype_type->bases == NULL ) ctype_type->bases = DList_New( DAO_DATA_VALUE );
		if( cdata_type->bases == NULL ) cdata_type->bases = DList_New( DAO_DATA_VALUE );
		DList_Append( ctype_type->bases, sup->core->kernel->abtype->aux->xCdata.ctype );
		DList_Append( cdata_type->bases, sup->core->kernel->abtype );
	}
	return cdata_type;
}



DaoException* DaoException_New( DaoType *type )
{
	DaoException *self = (DaoException*) dao_malloc( sizeof(DaoException) );
	DaoCstruct_Init( (DaoCstruct*)self, type );
	self->callers = DList_New( DAO_DATA_VALUE );
	self->lines = DList_New(0);
	self->info = DString_New();
	self->data = NULL;
	return self;
}
void DaoException_Delete( DaoException *self )
{
	DaoCstruct_Free( (DaoCstruct*)self );
	GC_DecRC( self->data );
	DString_Delete( self->info );
	DList_Delete( self->callers );
	DList_Delete( self->lines );
	dao_free( self );
}
void DaoException_SetData( DaoException *self, DaoValue *data )
{
	DaoValue_Move( data, & self->data, NULL );
}
void DaoException_GetGCFields( void *p, DList *values, DList *arrays, DList *maps, int remove )
{
	DaoException *self = (DaoException*) p;
	if( self->data ) DList_Append( values, self->data );
	if( self->callers->size ) DList_Append( arrays, self->callers );
	if( remove ) self->data = NULL;
}

static void Dao_Exception_Define( DaoProcess *proc, DaoValue *p[], int N );
static void Dao_Exception_Get_name( DaoProcess *proc, DaoValue *p[], int n );
static void Dao_Exception_Get_summary( DaoProcess *proc, DaoValue *p[], int n );
static void Dao_Exception_Set_summary( DaoProcess *proc, DaoValue *p[], int n );
static void Dao_Exception_Get_data( DaoProcess *proc, DaoValue *p[], int n );
static void Dao_Exception_Set_data( DaoProcess *proc, DaoValue *p[], int n );
static void Dao_Exception_Getf( DaoProcess *proc, DaoValue *p[], int n );
static void Dao_Exception_Setf( DaoProcess *proc, DaoValue *p[], int n );
static void Dao_Exception_Serialize( DaoProcess *proc, DaoValue *p[], int n );
static void Dao_Exception_New( DaoProcess *proc, DaoValue *p[], int n );
static void Dao_Exception_New22( DaoProcess *proc, DaoValue *p[], int n );

static DaoFuncItem dao_Exception_Meths[] =
{
	{ Dao_Exception_Get_name,    ".name( self: Exception )=>string" },
	{ Dao_Exception_Get_summary, ".summary( self: Exception )=>string" },
	{ Dao_Exception_Set_summary, ".summary=( self: Exception, summary: string)" },
	{ Dao_Exception_Get_data,    ".data( self: Exception )=>any" },
	{ Dao_Exception_Set_data,    ".data=( self: Exception, data: any)" },

	/* for testing or demonstration */
	{ Dao_Exception_Get_name, "typename( self: Exception )=>string" },
	{ Dao_Exception_Serialize, "serialize( self: Exception )=>string" },
	{ Dao_Exception_Serialize, "(string)( self: Exception )" },

#ifdef DEBUG
	{ Dao_Exception_Getf, ".( self: Exception, name: string )=>any" },
	{ Dao_Exception_Setf, ".=( self: Exception, name: string, value: any)" },
#endif
	{ NULL, NULL }
};

DaoTypeBase dao_Exception_Typer =
{
	"Exception", NULL, NULL, dao_Exception_Meths, { 0 }, { 0 },
	(FuncPtrDel) DaoException_Delete, DaoException_GetGCFields
};

static void Dao_Exception_Get_name( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoException* self = (DaoException*) p[0];
	DaoProcess_PutChars( proc, self->ctype->typer->name );
}
static void Dao_Exception_Get_summary( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoException* self = (DaoException*) p[0];
	DaoProcess_PutString( proc, self->info );
}
static void Dao_Exception_Set_summary( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoException* self = (DaoException*) p[0];
	DString_Assign( self->info, p[1]->xString.value );
}
static void Dao_Exception_Get_data( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoException* self = (DaoException*) p[0];
	DaoProcess_PutValue( proc, self->data ? self->data : dao_none_value );
}
static void Dao_Exception_Set_data( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoException* self = (DaoException*) p[0];
	DaoValue_Move( p[1], & self->data, NULL );
}
static void Dao_Exception_Serialize( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoException* self = (DaoException*) p[0];
	DaoStream *stream = DaoStream_New();
	stream->mode |= DAO_STREAM_STRING;
	DaoException_Print( self, stream );
	DaoProcess_PutString( proc, stream->streamString );
	DaoGC_TryDelete( (DaoValue*) stream );
}
#ifdef DEBUG
static void Dao_Exception_Getf( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoProcess_PutValue( proc, dao_none_value );
	printf( "Get undefined field: %s\n", DaoValue_TryGetChars( p[1] ) );
}
static void Dao_Exception_Setf( DaoProcess *proc, DaoValue *p[], int n )
{
	printf( "Set undefined field: %s\n", DaoValue_TryGetChars( p[1] ) );
}
#endif
static void Dao_Exception_New( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoType *type = proc->topFrame->routine->routHost;
	DaoException *self = (DaoException*)DaoException_New( type );
	if( n ) DString_Assign( self->info, p[0]->xString.value );
	DaoProcess_PutValue( proc, (DaoValue*)self );
}
static void Dao_Exception_New22( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoType *type = proc->topFrame->routine->routHost;
	DaoException *self = (DaoException*)DaoException_New( type );
	DaoException_SetData( self, p[0] );
	DaoProcess_PutValue( proc, (DaoValue*)self );
}
static void Dao_Exception_Define( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoType *etype;
	DString *host = proc->topFrame->routine->routHost->name;
	DString *name = p[0]->xString.value;
	DString *info = p[1]->xString.value;
	if( DString_Find( name, host, 0 ) != 0 && DString_FindChar( name, ':', 0 ) != host->size ){
		DString_InsertChars( name, "::", 0, 0, -1 );
		DString_Insert( name, host, 0, 0, -1 );
	}
	etype = DaoVmSpace_MakeExceptionType( proc->vmSpace, name->chars );
	if( etype == 0 ){
		DaoProcess_RaiseError( proc, "Param", "Invalid exception name" );
		return;
	}
	if( info->size ) DString_Assign( etype->aux->xCtype.info, info );
	DaoProcess_PutValue( proc, (DaoValue*) etype->aux );
}

static DaoFuncItem dao_ExceptionWarning_Meths[] =
{
	{ Dao_Exception_New, "Warning( summary = \"\" )" },
	{ Dao_Exception_Define, "define( name: string, info = '' ) => class<Warning>" },
	{ NULL, NULL }
};

DaoTypeBase dao_ExceptionWarning_Typer =
{
	"Exception::Warning", NULL, NULL, dao_ExceptionWarning_Meths,
	{ & dao_Exception_Typer, NULL }, {0},
	(FuncPtrDel) DaoException_Delete, DaoException_GetGCFields
};

static DaoFuncItem dao_ExceptionError_Meths[] =
{
	{ Dao_Exception_New, "Error( summary = \"\" )" },
	{ Dao_Exception_New22, "Error( data: any )" },
	{ Dao_Exception_Define, "define( name: string, info = '' ) => class<Error>" },
	{ NULL, NULL }
};
DaoTypeBase dao_ExceptionError_Typer =
{
	"Exception::Error", NULL, NULL, dao_ExceptionError_Meths,
	{ & dao_Exception_Typer, NULL }, {0},
	(FuncPtrDel) DaoException_Delete, DaoException_GetGCFields
};

void DaoException_Setup( DaoNamespace *ns )
{
	dao_type_exception = DaoNamespace_WrapType( ns, & dao_Exception_Typer, 0 );
	dao_type_warning = DaoNamespace_WrapType( ns, & dao_ExceptionWarning_Typer, 0 );
	dao_type_error = DaoNamespace_WrapType( ns, & dao_ExceptionError_Typer, 0 );
	DaoNamespace_AddType( ns, dao_type_warning->name, dao_type_warning );
	DaoNamespace_AddType( ns, dao_type_error->name, dao_type_error );
	DaoNamespace_AddTypeConstant( ns, dao_type_warning->name, dao_type_warning );
	DaoNamespace_AddTypeConstant( ns, dao_type_error->name, dao_type_error );
	DString_SetChars( dao_type_exception->aux->xCtype.info, daoExceptionTitles[0] );
	DString_SetChars( dao_type_warning->aux->xCtype.info, daoExceptionTitles[1] );
	DString_SetChars( dao_type_error->aux->xCtype.info, daoExceptionTitles[2] );
}

void DaoException_Init( DaoException *self, DaoProcess *proc, const char *summary, DaoValue *dat )
{
	DaoVmCodeX **annotCodes;
	DaoVmCode *vmc = proc->activeCode;
	DaoRoutine *rout = proc->activeRoutine;
	DaoStackFrame *frame = proc->topFrame->prev;
	int line, id = (int) (vmc - proc->topFrame->active->codes);

	if( rout == NULL ) return;

	line = rout->defLine;
	annotCodes = rout->body->annotCodes->items.pVmc;
	if( proc->topFrame->active == proc->topFrame->prev ){
		/*
		// proc->activeCode could be a dummy code set by:
		//   DaoProcess_InterceptReturnValue();
		// So always use the entry index whenever possible.
		*/
		id = proc->topFrame->prev->entry;
	}else{
		id = (int) (vmc - proc->topFrame->active->codes);
		if( id < 0 || id > 0xffff ) id = 0; /* Not the precise location, but a safe one; */
	}
	if( vmc && rout->body->vmCodes->size ) line = annotCodes[id]->line;

	if( summary && summary[0] != 0 ) DString_SetChars( self->info, summary );
	GC_Assign( & self->data, dat );

	DList_Clear( self->callers );
	DList_Clear( self->lines );
	DList_Append( self->callers, proc->topFrame->routine );
	DList_Append( self->lines, (daoint) (line<<16)|id );
	while( frame && frame->routine ){
		DaoRoutineBody *body = frame->routine->body;
		if( self->callers->size >= 5 ) break;
		if( frame->entry ){
			/* deferred anonymous function may have been pushed but not executed: */
			line = body ? body->annotCodes->items.pVmc[ frame->entry - 1 ]->line : 0;
			DList_Append( self->callers, frame->routine );
			DList_Append( self->lines, (daoint) (line<<16)|(frame->entry - 1) );
		}
		frame = frame->prev;
	}
}
static void DaoType_WriteMainName( DaoType *self, DaoStream *stream )
{
	DString *name = self->name;
	daoint i, n = DString_FindChar( name, '<', 0 );
	if( n == DAO_NULLPOS ) n = name->size;
	for(i=0; i<n; i++) DaoStream_WriteChar( stream, name->chars[i] );
}
static void DString_Format( DString *self, int width, int head )
{
	daoint i, j, n, k = width - head;
	char buffer[32];
	if( head >= 30 ) head = 30;
	if( width <= head ) return;
	memset( buffer, ' ', head+1 );
	buffer[0] = '\n';
	buffer[head+1] = '\0';
	for(i=0,k=0; i<self->size; ++i,++k){
		if( k >= width ){
			DString_InsertChars( self, buffer, i, 0, head+1 );
			k = 0;
		}else if( self->chars[i] == '\n' ){
			k = 0;
		}
	}
}
static void DaoException_PrintName( DaoValue *exception, DaoStream *ss )
{
	if( exception == NULL || exception->type != DAO_OBJECT ) return;
	if( exception->xObject.parent ) DaoException_PrintName( exception->xObject.parent, ss );
	DaoStream_WriteChars( ss, "::" );
	DaoStream_WriteString( ss, exception->xObject.defClass->className );
}
void DaoException_Print( DaoException *self, DaoStream *stream )
{
	int codeShown = 0;
	int i, h, w = 100, n = self->callers->size;
	DaoStream *ss = DaoStream_New();
	DString *sstring = ss->streamString;
	ss->mode |= DAO_STREAM_STRING;

	DaoStream_WriteChars( ss, "[[" );
	DaoStream_WriteChars( ss, self->ctype->typer->name );
	DaoException_PrintName( (DaoValue*) self->object, ss );
	DaoStream_WriteChars( ss, "]] --- " );
	h = sstring->size;
	if( h > 40 ) h = 40;
	DaoStream_WriteString( ss, self->ctype->aux->xCtype.info );
	DaoStream_WriteChars( ss, ":\n" );
	DaoStream_WriteString( stream, sstring );
	DString_Clear( sstring );
	DaoStream_WriteString( ss, self->info );
	DString_Chop( sstring, 1 );
	DString_Format( sstring, w, h );
	DaoStream_WriteChars( ss, "\n" );
	DaoStream_WriteString( stream, sstring );
	DString_Clear( sstring );

	for(i=0; i<n; i++){
		DaoRoutine *rout = self->callers->items.pRoutine[i];
		if( codeShown == 0 && rout->subtype == DAO_ROUTINE ){
			DaoRoutine_PrintCodeSnippet( rout, stream, self->lines->items.pInt[i] & 0xffff );
			codeShown = 1;
		}
		DaoStream_WriteChars( ss, i == 0 ? "Raised by:  " : "Called by:  " );
		if( rout->attribs & DAO_ROUT_PARSELF ){
			DaoType *type = rout->routType->nested->items.pType[0];
			type = DaoType_GetBaseType( (DaoType*) type->aux );
			DaoType_WriteMainName( type, ss );
			DaoStream_WriteChars( ss, "." );
		}else if( rout->routHost ){
			DaoType_WriteMainName( rout->routHost, ss );
			DaoStream_WriteChars( ss, "." );
		}
		DaoStream_WriteString( ss, rout->routName );
		DaoStream_WriteChars( ss, "()," );
		if( rout->subtype == DAO_ROUTINE ){
			DaoStream_WriteChars( ss, " at instruction " );
			DaoStream_WriteInt( ss, self->lines->items.pInt[i] & 0xffff );
			DaoStream_WriteChars( ss, " in line " );
			DaoStream_WriteInt( ss, self->lines->items.pInt[i] >> 16 );
			DaoStream_WriteChars( ss, " in file \"" );
		}else{
			DaoStream_WriteChars( ss, " from namespace \"" );
		}
		DaoStream_WriteString( ss, rout->nameSpace->name );
		DaoStream_WriteChars( ss, "\";\n" );
		DString_Format( sstring, w, 12 );
		DaoStream_WriteString( stream, sstring );
		DString_Clear( sstring );
	}
	DaoStream_Delete( ss );
}



DaoConstant* DaoConstant_New( DaoValue *value, int subtype )
{
	DaoConstant *self = (DaoConstant*) dao_calloc( 1, sizeof(DaoConstant) );
	DaoValue_Init( self, DAO_CONSTANT );
	DaoValue_Copy( value, & self->value );
	self->subtype = subtype;
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
DaoVariable* DaoVariable_New( DaoValue *value, DaoType *type, int subtype )
{
	DaoVariable *self = (DaoVariable*) dao_calloc( 1, sizeof(DaoVariable) );
	DaoValue_Init( self, DAO_VARIABLE );
	DaoValue_Move( value, & self->value, type );
	self->subtype = subtype;
	self->dtype = type;
	GC_IncRC( type );
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}

void DaoConstant_Delete( DaoConstant *self )
{
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	GC_DecRC( self->value );
	dao_free( self );
}
void DaoVariable_Delete( DaoVariable *self )
{
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	GC_DecRC( self->value );
	GC_DecRC( self->dtype );
	dao_free( self );
}
void DaoConstant_Set( DaoConstant *self, DaoValue *value )
{
	DaoValue_Copy( value, & self->value );
}
int DaoVariable_Set( DaoVariable *self, DaoValue *value, DaoType *type )
{
	if( type ) GC_Assign( & self->dtype, type );
	return DaoValue_Move( value, & self->value, self->dtype );
}
void DaoVariable_SetType( DaoVariable *self, DaoType *type )
{
	GC_Assign( & self->dtype, type );
	if( self->value == NULL || self->value->type != type->value->type ){
		GC_DecRC( self->value );
		self->value = DaoValue_SimpleCopy( type->value );
		GC_IncRC( self->value );
	}
}


