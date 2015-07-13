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
#include"string.h"
#include"assert.h"

#include"daoGC.h"
#include"daoType.h"
#include"daoStdtype.h"
#include"daoStream.h"
#include"daoRoutine.h"
#include"daoClass.h"
#include"daoObject.h"
#include"daoNumtype.h"
#include"daoNamespace.h"
#include"daoVmspace.h"
#include"daoParser.h"
#include"daoProcess.h"
#include"daoValue.h"




int DaoValue_IsZero( DaoValue *self )
{
	if( self == NULL ) return 1;
	switch( self->type ){
	case DAO_NONE : return 1;
	case DAO_BOOLEAN :
	case DAO_INTEGER : return self->xInteger.value == 0;
	case DAO_FLOAT  : return self->xFloat.value == 0.0;
	case DAO_COMPLEX : return self->xComplex.value.real == 0.0 && self->xComplex.value.imag == 0.0;
	case DAO_ENUM : return self->xEnum.value == 0;
	}
	return 0;
}
int DaoValue_IsNumber( DaoValue *self )
{
	if( self->type == DAO_INTEGER || self->type == DAO_FLOAT ) return 1;
	return 0;
}
static dao_integer DString_ToInteger( DString *self )
{
		return strtoll( self->chars, NULL, 0 );
}
dao_float DString_ToFloat( DString *self )
{
	return strtod( self->chars, 0 );
}
dao_integer DaoValue_GetInteger( DaoValue *self )
{
	switch( self->type ){
	case DAO_BOOLEAN :
	case DAO_INTEGER : return self->xInteger.value;
	case DAO_FLOAT   : return self->xFloat.value;
	case DAO_COMPLEX : return self->xComplex.value.real;
	case DAO_STRING  : return DString_ToInteger( self->xString.value );
	case DAO_ENUM    : return self->xEnum.value;
	default : break;
	}
	return 0;
}
dao_float DaoValue_GetFloat( DaoValue *self )
{
	DString *str;
	switch( self->type ){
	case DAO_NONE    : return 0;
	case DAO_BOOLEAN :
	case DAO_INTEGER : return self->xInteger.value;
	case DAO_FLOAT   : return self->xFloat.value;
	case DAO_COMPLEX : return self->xComplex.value.real;
	case DAO_STRING  : return DString_ToFloat( self->xString.value );
	case DAO_ENUM    : return self->xEnum.value;
	default : break;
	}
	return 0.0;
}
dao_complex DaoValue_GetComplex( DaoValue *self )
{
	dao_complex com = { 0.0, 0.0 };
	switch( self->type ){
	case DAO_BOOLEAN :
	case DAO_INTEGER : com.real = self->xInteger.value; break;
	case DAO_FLOAT   : com.real = self->xFloat.value; break;
	case DAO_COMPLEX : com = self->xComplex.value; break;
	default : break;
	}
	return com;
}
DString* DaoValue_GetString( DaoValue *self, DString *str )
{
	dao_complex *com;
	char chs[100] = {0};
	DString_Clear( str );
	switch( self->type ){
	case DAO_COMPLEX :
		com = & self->xComplex.value;
		sprintf( chs, (com->imag < 0) ? "%g%gC" : "%g+%gC", com->real, com->imag ); break;
	case DAO_BOOLEAN : strcat( chs, self->xBoolean.value ? "true" : "false" ); break;
	case DAO_INTEGER : sprintf( chs, "%"DAO_I64, (long long) self->xInteger.value ); break;
	case DAO_FLOAT   : sprintf( chs, "%g", self->xFloat.value ); break;
	case DAO_STRING : DString_Assign( str, self->xString.value ); break;
	case DAO_ENUM : DaoEnum_MakeName( & self->xEnum, str ); break;
	default : break;
	}
	if( self->type <= DAO_COMPLEX ) DString_SetChars( str, chs );
	return str;
}



void DaoValue_MarkConst( DaoValue *self )
{
	DMap *map;
	DNode *it;
	daoint i, n;
	if( self == NULL || (self->xBase.trait & DAO_VALUE_CONST) ) return;
	self->xBase.trait |= DAO_VALUE_CONST;
	switch( self->type ){
	case DAO_LIST :
		for(i=0,n=self->xList.value->size; i<n; i++)
			DaoValue_MarkConst( self->xList.value->items.pValue[i] );
		break;
	case DAO_TUPLE :
		for(i=0,n=self->xTuple.size; i<n; i++)
			DaoValue_MarkConst( self->xTuple.values[i] );
		break;
	case DAO_MAP :
		map = self->xMap.value;
		for(it=DMap_First( map ); it != NULL; it = DMap_Next(map, it) ){
			DaoValue_MarkConst( it->key.pValue );
			DaoValue_MarkConst( it->value.pValue );
		}
		break;
	case DAO_OBJECT :
		n = self->xObject.defClass->instvars->size;
		for(i=1; i<n; i++) DaoValue_MarkConst( self->xObject.objValues[i] );
		if( self->xObject.parent ) DaoValue_MarkConst( self->xObject.parent );
		break;
	default : break;
	}
}

void DaoValue_Clear( DaoValue **self )
{
	DaoValue *value = *self;
	*self = NULL;
	GC_DecRC( value );
}


DaoValue* DaoValue_CopyContainer( DaoValue *self, DaoType *tp )
{
	switch( self->type ){
	case DAO_LIST  : return (DaoValue*) DaoList_Copy( (DaoList*) self, tp );
	case DAO_MAP   : return (DaoValue*) DaoMap_Copy( (DaoMap*) self, tp );
	case DAO_TUPLE : return (DaoValue*) DaoTuple_Copy( (DaoTuple*) self, tp );
#ifdef DAO_WITH_NUMARRAY
	case DAO_ARRAY : return (DaoValue*) DaoArray_CopyX( (DaoArray*) self, tp );
#endif
	default : break;
	}
	return self;
}

/*
// Assumming the value "self" is compatible to the type "tp", if it is not null.
*/
DaoValue* DaoValue_SimpleCopyWithTypeX( DaoValue *self, DaoType *tp, DaoType *cst )
{
	if( self == NULL ) return dao_none_value;
	if( (tp == NULL || tp->tid == self->type) && self->type < DAO_ENUM ){
		/*
		// The following optimization is safe theoretically.
		// But it is not practically safe for DaoProcess_PutChars() etc.,
		// which often uses shallow wraps of "const char*" as the source value,
		// and expects it to be copied at the destination as a primitive value.
		*/
		/* if( cst && cst->invar ) return self; */
		switch( self->type ){
		case DAO_NONE : return self;
		case DAO_BOOLEAN : return (DaoValue*) DaoBoolean_New( self->xBoolean.value );
		case DAO_INTEGER : return (DaoValue*) DaoInteger_New( self->xInteger.value );
		case DAO_FLOAT   : return (DaoValue*) DaoFloat_New( self->xFloat.value );
		case DAO_COMPLEX : return (DaoValue*) DaoComplex_New( self->xComplex.value );
		case DAO_STRING  : return (DaoValue*) DaoString_Copy( & self->xString );
		}
		return self; /* unreachable; */
	}else if( tp && tp->tid >= DAO_BOOLEAN && tp->tid <= DAO_FLOAT ){
		DaoValue *va = NULL;
		switch( tp->tid ){
		case DAO_BOOLEAN : va = (DaoValue*) DaoBoolean_New( DaoValue_GetInteger(self) ); break;
		case DAO_INTEGER : va = (DaoValue*) DaoInteger_New( DaoValue_GetInteger(self) ); break;
		case DAO_FLOAT   : va = (DaoValue*) DaoFloat_New( DaoValue_GetFloat(self) );   break;
		}
		return va;
	}else if( self->type == DAO_ENUM ){
		switch( tp ? tp->tid : 0 ){
		case DAO_ENUM :
			if( tp->subtid == DAO_ENUM_ANY ) tp = NULL;
			return (DaoValue*) DaoEnum_Copy( & self->xEnum, tp );
		case DAO_BOOLEAN : return (DaoValue*) DaoBoolean_New( self->xEnum.value );
		case DAO_INTEGER : return (DaoValue*) DaoInteger_New( self->xEnum.value );
		case DAO_FLOAT   : return (DaoValue*) DaoFloat_New( self->xEnum.value );
		}
		return (DaoValue*) DaoEnum_Copy( & self->xEnum, NULL );
	}else if( tp && tp->tid == DAO_ENUM ){
		switch( self->type ){
		case DAO_BOOLEAN :
		case DAO_INTEGER : return (DaoValue*) DaoEnum_New( tp, self->xInteger.value );
		case DAO_FLOAT   : return (DaoValue*) DaoEnum_New( tp, self->xFloat.value );
		}
	}
	if( tp != NULL ){
		assert( tp->tid == 0 || tp->tid > DAO_ENUM );
		assert( self->type == 0 || self->type > DAO_ENUM );
	}

#ifdef DAO_WITH_NUMARRAY
	if( self->type == DAO_ARRAY && self->xArray.original ){
		DaoArray_Sliced( (DaoArray*)self );
		return self;
	}else
#endif
	if( self->type == DAO_CSTRUCT || self->type == DAO_CDATA ){
		FuncPtrSliced sliced = self->xCstruct.ctype->kernel->Sliced;
		if( sliced ) (*sliced)( self );
		return self;
	}
	if( tp == NULL ){
		switch( self->type ){
		case DAO_LIST  :
			if( self->xList.ctype == dao_type_list_empty ) tp = dao_type_list_empty;
			break;
		case DAO_MAP   :
			if( self->xMap.ctype == dao_type_map_empty ) tp = dao_type_map_empty;
			break;
#ifdef DAO_WITH_NUMARRAY
		case DAO_ARRAY :
			if( self->xMap.ctype == dao_type_array_empty ) tp = dao_type_array_empty;
			break;
#endif
		default : break;
		}
		if( tp ) tp = DaoType_GetBaseType( tp );
	}
	if( self->xBase.trait & DAO_VALUE_NOCOPY ) return self;
	if( (self->xBase.trait & DAO_VALUE_CONST) == 0 ) return self;
	if( cst != NULL && cst->invar != 0 ) return self;
	return DaoValue_CopyContainer( self, tp );
}
DaoValue* DaoValue_SimpleCopyWithType( DaoValue *self, DaoType *tp )
{
	return DaoValue_SimpleCopyWithTypeX( self, tp, NULL );
}
DaoValue* DaoValue_SimpleCopy( DaoValue *self )
{
	return DaoValue_SimpleCopyWithTypeX( self, NULL, NULL );
}

void DaoValue_CopyX( DaoValue *src, DaoValue **dest, DaoType *cst )
{
	DaoValue *dest2 = *dest;
	if( src == dest2 ) return;
	if( dest2 && dest2->xBase.refCount >1 ){
		GC_DecRC( dest2 );
		*dest = dest2 = NULL;
	}
	if( dest2 == NULL ){
		src = DaoValue_SimpleCopyWithTypeX( src, NULL, cst );
		GC_IncRC( src );
		*dest = src;
		return;
	}
	if( src->type != dest2->type || src->type > DAO_ENUM ){
		src = DaoValue_SimpleCopyWithTypeX( src, NULL, cst );
		GC_Assign( dest, src );
		return;
	}
	switch( src->type ){
	case DAO_ENUM    :
		DaoEnum_SetType( & dest2->xEnum, src->xEnum.etype );
		DaoEnum_SetValue( & dest2->xEnum, & src->xEnum );
		break;
	case DAO_BOOLEAN :
	case DAO_INTEGER : dest2->xInteger.value = src->xInteger.value; break;
	case DAO_FLOAT   : dest2->xFloat.value = src->xFloat.value; break;
	case DAO_COMPLEX : dest2->xComplex.value = src->xComplex.value; break;
	case DAO_STRING  : DString_Assign( dest2->xString.value, src->xString.value ); break;
	}
}
void DaoValue_Copy( DaoValue *src, DaoValue **dest )
{
	DaoValue_CopyX( src, dest, NULL );
}
void DaoValue_SetType( DaoValue *to, DaoType *tp )
{
	DaoType *tp2;
	DNode *it;
	if( to->type != tp->tid && tp->tid != DAO_ANY ) return;
	if( tp->attrib & DAO_TYPE_SPEC ) return;
	switch( to->type ){
#ifdef DAO_WITH_NUMARRAY
	case DAO_ARRAY :
		if( to->xArray.size ) return;
		if( tp->tid != DAO_ARRAY || tp->nested == NULL || tp->nested->size == 0 ) break;
		tp = tp->nested->items.pType[0];
		if( tp->tid == DAO_NONE || tp->tid > DAO_COMPLEX ) break;
		DaoArray_SetNumType( (DaoArray*) to, tp->tid );
		break;
#endif
	case DAO_LIST :
		/* v : any = {}, v->ctype should be list<any> */
		if( tp->tid == DAO_ANY ) tp = dao_type_list_any;
		if( to->xList.ctype && !(to->xList.ctype->attrib & DAO_TYPE_UNDEF) ) break;
		GC_Assign( & to->xList.ctype, tp );
		break;
	case DAO_MAP :
		if( tp->tid == DAO_ANY ) tp = dao_type_map_any;
		if( to->xMap.ctype && !(to->xMap.ctype->attrib & DAO_TYPE_UNDEF) ) break;
		GC_Assign( & to->xMap.ctype, tp );
		break;
	case DAO_TUPLE :
		tp2 = to->xTuple.ctype;
		if( tp->tid == DAO_ANY ) break;
		if( tp->nested->size ==0 ) break; /* not to the generic tuple type */
		if( tp2 == NULL || tp2->mapNames == NULL || tp2->mapNames->size ==0 ){
			GC_Assign( & to->xTuple.ctype, tp );
			break;
		}
		if( tp->mapNames == NULL || tp->mapNames->size ) break;
		for(it=DMap_First(tp2->mapNames); it!=NULL; it=DMap_Next(tp2->mapNames, it)){
			if( DMap_Find( tp->mapNames, it->key.pVoid ) == NULL ) break;
		}
		if( it ) break;
		GC_Assign( & to->xTuple.ctype, tp );
		break;
	default : break;
	}
}
static int DaoValue_TryCastTuple( DaoValue *src, DaoValue **dest, DaoType *tp )
{
	DaoTuple *tuple;
	DaoType **item_types = tp->nested->items.pType;
	DaoType *totype = src->xTuple.ctype;
	DaoValue **data = src->xTuple.values;
	DMap *names = totype ? totype->mapNames : NULL;
	DNode *node, *search;
	daoint i, T = tp->nested->size;
	int tm, eqs = 0;
	/*
	// auto-cast tuple type, on the following conditions:
	// (1) the item values of "dest" must match exactly to the item types of "tp";
	// (2) "tp->mapNames" must contain "(*dest)->xTuple.ctype->mapNames";
	*/
	if( src->xTuple.ctype == NULL ){
		GC_IncRC( tp );
		src->xTuple.ctype = tp;
		return 1;
	}
	if( DaoType_MatchValue( tp, src, NULL ) < DAO_MT_SUB ) return 1;
	/*
	// casting is not necessary if the tuple's field names are a superset of the
	// field names of the target type:
	*/
	if( tp->mapNames == NULL || tp->mapNames->size ==0 ) goto Finalize;
	if( names ){
		daoint count = 0;
		for(node=DMap_First(names); node; node=DMap_Next(names,node)){
			search = DMap_Find( tp->mapNames, node->key.pVoid );
			if( search && node->value.pInt != search->value.pInt ) return 0;
			count += search != NULL;
		}
		/* be superset of the field names of the target type: */
		if( count == tp->mapNames->size ) goto Finalize;
	}
Finalize:
	tuple = DaoTuple_New( T );
	for(i=0; i<T; i++){
		DaoType *it = item_types[i];
		if( it->tid == DAO_PAR_NAMED ) it = & it->aux->xType;
		DaoValue_Move( data[i], tuple->values+i, it );
	}
	GC_IncRC( tp );
	tuple->ctype = tp;
	GC_Assign( dest, tuple );
	return 1;
}
static int DaoValue_Move4( DaoValue *S, DaoValue **D, DaoType *T, DaoType *C, DMap *defs );
static int DaoValue_Move5( DaoValue *S, DaoValue **D, DaoType *T, DaoType *C, DMap *defs );
static int DaoValue_MoveVariant( DaoValue *src, DaoValue **dest, DaoType *tp, DaoType *C )
{
	DaoType *itp = NULL;
	int n = tp->nested->size;
	int j, k, mt;
	for(j=0,mt=0; j<n; j++){
		DaoType *itp2 = tp->nested->items.pType[j];
		k = DaoType_MatchValue( itp2, src, NULL );
		if( k > mt ){
			itp = itp2;
			mt = k;
			if( mt >= DAO_MT_EQ ) break;
		}
	}
	if( itp == NULL ) return 0;
	return DaoValue_Move5( src, dest, itp, C, NULL );
}
int DaoValue_Move4( DaoValue *S, DaoValue **D, DaoType *T, DaoType *C, DMap *defs )
{
	DaoCinType *cintype;
	int tm = 1;
	switch( (T->tid << 8) | S->type ){
	case (DAO_BOOLEAN << 8) | DAO_BOOLEAN :
	case (DAO_BOOLEAN << 8) | DAO_INTEGER :
	case (DAO_BOOLEAN << 8) | DAO_FLOAT   :
	case (DAO_INTEGER << 8) | DAO_BOOLEAN :
	case (DAO_INTEGER << 8) | DAO_INTEGER :
	case (DAO_INTEGER << 8) | DAO_FLOAT   :
	case (DAO_FLOAT   << 8) | DAO_BOOLEAN :
	case (DAO_FLOAT   << 8) | DAO_INTEGER :
	case (DAO_FLOAT   << 8) | DAO_FLOAT   :
	case (DAO_COMPLEX << 8) | DAO_COMPLEX :
	case (DAO_STRING  << 8) | DAO_STRING  :
		S = DaoValue_SimpleCopyWithTypeX( S, T, C );
		GC_Assign( D, S );
		return 1;
	}
	switch( S->type ){
	case DAO_ENUM : if( S->xEnum.subtype == DAO_ENUM_SYM && T->realnum ) return 0; break;
	case DAO_OBJECT : if( S->xObject.isNull ) return 0; break;
	case DAO_CDATA  : if( S->xCdata.data == NULL ) return 0; break;
	}
	if( !(S->xBase.trait & DAO_VALUE_CONST) ){
		DaoType *ST = NULL;
		switch( (S->type << 8) | T->tid ){
		case (DAO_TUPLE<<8)|DAO_TUPLE : ST = S->xTuple.ctype; break;
		case (DAO_ARRAY<<8)|DAO_ARRAY : ST = dao_array_types[ S->xArray.etype ]; break;
		case (DAO_LIST <<8)|DAO_LIST  : ST = S->xList.ctype; break;
		case (DAO_MAP  <<8)|DAO_MAP   : ST = S->xMap.ctype; break;
		case (DAO_CDATA<<8)|DAO_CDATA : ST = S->xCdata.ctype; break;
		case (DAO_CSTRUCT<<8)|DAO_CSTRUCT : ST = S->xCstruct.ctype; break;
		case (DAO_OBJECT<<8)|DAO_OBJECT : ST = S->xObject.defClass->objType; break;
		}
		if( ST == T ){
			GC_Assign( D, S );
			return 1;
		}
	}
	if( (T->tid == DAO_OBJECT || T->tid == DAO_CDATA || T->tid == DAO_CSTRUCT) && S->type == DAO_OBJECT ){
		if( S->xObject.defClass != & T->aux->xClass ){
			S = DaoObject_CastToBase( S->xObject.rootObject, T );
			tm = (S != NULL);
		}
	}else if( (T->tid == DAO_CLASS || T->tid == DAO_CTYPE) && S->type == DAO_CLASS ){
		if( S->xClass.clsType != T && T->aux != NULL ){ /* T->aux == NULL for "class"; */
			S = DaoClass_CastToBase( (DaoClass*)S, T );
			tm = (S != NULL);
		}
	}else if( T->tid == DAO_CTYPE && S->type == DAO_CTYPE ){
		if( S->xCtype.ctype != T ){
			S = DaoType_CastToParent( S, T );
			tm = (S != NULL);
		}
	}else if( T->tid == DAO_ROUTINE && T->subtid != DAO_ROUTINES && S->type == DAO_ROUTINE && S->xRoutine.overloads ){
		DList *routines = S->xRoutine.overloads->routines;
		int i, k, n;
		/*
		// Do not use DaoRoutine_ResolveByType( S, ... )
		// "S" should match to "T", not the other way around!
		*/
		tm = 0;
		for(i=0,n=routines->size; i<n; i++){
			DaoRoutine *rout = routines->items.pRoutine[i];
			k = rout->routType == T ? DAO_MT_EQ : DaoType_MatchTo( rout->routType, T, defs );
			if( k > tm ) tm = k;
			if( rout->routType == T ){
				S = (DaoValue*) rout;
				break;
			}
		}
	}else{
		tm = DaoType_MatchValue( T, S, defs );
	}
#if 0
	if( tm ==0 ){
		printf( "T = %p; S = %p, type = %i %i\n", T, S, S->type, DAO_ROUTINE );
		printf( "T: %s %i %i\n", T->name->chars, T->tid, tm );
		if( S->type == DAO_LIST ) printf( "%s\n", S->xList.ctype->name->chars );
		if( S->type == DAO_TUPLE ) printf( "%p\n", S->xTuple.ctype );
	}
	printf( "S->type = %p %s %i\n", S, T->name->chars, tm );
#endif
	if( tm == 0 ) return 0;
	/*
	// composite known types must match exactly. example,
	// where it will not work if composite types are allowed to match loosely.
	// d : list<list<int>> = {};
	// e : list<float> = { 1.0 };
	// d.append( e );
	//
	// but if d is of type list<list<any>>,
	// the matching do not necessary to be exact.
	*/
	cintype = NULL;
	if( T->tid == DAO_CINVALUE ){
		if( DaoType_MatchValue( T->aux->xCinType.target, S, NULL ) >= DAO_MT_EQ ){
			cintype = (DaoCinType*) T->aux;
		}
	}else if( T->tid == DAO_INTERFACE && T->aux->xInterface.concretes ){
		DaoInterface *inter = (DaoInterface*) T->aux;
		DaoType *st = DaoValue_GetType( S );
		DNode *it; 
		cintype = DaoInterface_GetConcrete( inter, st );
		if( cintype == NULL ){
			for(it=DMap_First(inter->concretes); it; it=DMap_Next(inter->concretes,it)){
				if( DaoType_MatchValue( it->key.pType, S, NULL ) >= DAO_MT_EQ ){
					cintype = (DaoCinType*) it->value.pVoid;
					break;
				}
			}
		}
	}
	if( cintype ){
		S = DaoValue_SimpleCopyWithTypeX( S, cintype->target, C );
		S = (DaoValue*) DaoWrappers_MakeCinValue( cintype, S );
	}else{
		S = DaoValue_SimpleCopyWithTypeX( S, T, C );
	}
	GC_Assign( D, S );
	if( S->type == DAO_TUPLE && S->xTuple.ctype != T && tm == DAO_MT_SIM ){
		return DaoValue_TryCastTuple( S, D, T );
	}else if( T && T->tid == S->type && !(T->attrib & DAO_TYPE_SPEC) ){
		DaoValue_SetType( S, T );
	}
	return 1;
}
int DaoValue_FastMatchTo( DaoValue *self, DaoType *type )
{
	int matched = 0;
	switch( self->type ){
	case DAO_LIST :
	case DAO_MAP :
	case DAO_CSTRUCT :
	case DAO_CDATA : 
	case DAO_CTYPE : matched = self->xCstruct.ctype == type; break;
	case DAO_TUPLE : matched = self->xTuple.ctype == type; break;
	case DAO_ROUTINE : matched = self->xRoutine.routType == type; break;
	case DAO_CLASS  : matched = self->xClass.clsType == type; break;
	case DAO_OBJECT : matched = self->xObject.defClass->objType == type; break;
	default : break;
	}
	return matched;
}
int DaoValue_Move5( DaoValue *S, DaoValue **D, DaoType *T, DaoType *C, DMap *defs )
{
	DaoValue *D2 = *D;
	if( S == D2 && (S == NULL || DaoValue_FastMatchTo( S, T )) ) return 1;
	if( S == NULL ){
		GC_DecRC( *D );
		*D = NULL;
		return 0;
	}
	if( T == NULL ){
		DaoValue_CopyX( S, D, C );
		return 1;
	}
	if( T->valtype ){
		if( DaoValue_Compare( S, T->value ) !=0 ) return 0;
		DaoValue_CopyX( S, D, C );
		return 1;
	}
	switch( T->tid ){
	case DAO_UDT :
		DaoValue_CopyX( S, D, C );
		return 1;
	case DAO_THT :
		if( T->aux ) return DaoValue_Move5( S, D, (DaoType*) T->aux, C, defs );
		DaoValue_CopyX( S, D, C );
		return 1;
	case DAO_ANY :
		DaoValue_CopyX( S, D, C );
		DaoValue_SetType( *D, T );
		return 1;
	case DAO_VARIANT :
		return DaoValue_MoveVariant( S, D, T, C );
	default : break;
	}
	if( S->type >= DAO_OBJECT || !(S->xBase.trait & DAO_VALUE_CONST) || T->invar ){
		if( DaoValue_FastMatchTo( S, T ) ){
			GC_Assign( D, S );
			return 1;
		}
	}else if( S->type == DAO_CINVALUE ){
		if( S->xCinValue.cintype->target == T ){
			S = S->xCinValue.value;
		}else if( DaoType_MatchTo( S->xCinValue.cintype->target, T, NULL ) >= DAO_MT_EQ ){
			S = S->xCinValue.value;
		}
	}
	if( D2 && D2->xBase.refCount > 1 ){
		GC_DecRC( D2 );
		*D = D2 = NULL;
	}
	if( D2 && S->type == D2->type && S->type == T->tid && S->type <= DAO_ENUM ){
		switch( S->type ){
		case DAO_ENUM    :
			DaoEnum_SetType( & D2->xEnum, T->subtid == DAO_ENUM_ANY ? S->xEnum.etype : T );
			return DaoEnum_SetValue( & D2->xEnum, & S->xEnum );
		case DAO_BOOLEAN :
		case DAO_INTEGER : D2->xInteger.value = S->xInteger.value; break;
		case DAO_FLOAT   : D2->xFloat.value = S->xFloat.value; break;
		case DAO_COMPLEX : D2->xComplex.value = S->xComplex.value; break;
		case DAO_STRING  : DString_Assign( D2->xString.value, S->xString.value ); break;
		}
		return 1;
	}
	if( D2 == NULL || D2->type != T->tid ) return DaoValue_Move4( S, D, T, C, defs );

	switch( (S->type << 8) | T->tid ){
	case (DAO_STRING<<8)|DAO_STRING :
		DString_Assign( D2->xString.value, S->xString.value );
		break;
	case (DAO_ENUM<<8)|DAO_ENUM :
		DaoEnum_SetType( & D2->xEnum, T->subtid == DAO_ENUM_ANY ? S->xEnum.etype : T );
		DaoEnum_SetValue( & D2->xEnum, & S->xEnum );
		break;
	case (DAO_BOOLEAN<<8)|DAO_BOOLEAN : D2->xInteger.value = S->xInteger.value; break;
	case (DAO_BOOLEAN<<8)|DAO_INTEGER : D2->xInteger.value = S->xInteger.value; break;
	case (DAO_BOOLEAN<<8)|DAO_FLOAT   : D2->xFloat.value   = S->xInteger.value; break;
	case (DAO_INTEGER<<8)|DAO_BOOLEAN : D2->xInteger.value = S->xInteger.value; break;
	case (DAO_INTEGER<<8)|DAO_INTEGER : D2->xInteger.value = S->xInteger.value; break;
	case (DAO_INTEGER<<8)|DAO_FLOAT   : D2->xFloat.value   = S->xInteger.value; break;
	case (DAO_FLOAT  <<8)|DAO_BOOLEAN : D2->xInteger.value = S->xFloat.value; break;
	case (DAO_FLOAT  <<8)|DAO_INTEGER : D2->xInteger.value = S->xFloat.value; break;
	case (DAO_FLOAT  <<8)|DAO_FLOAT   : D2->xFloat.value   = S->xFloat.value; break;
	case (DAO_COMPLEX<<8)|DAO_COMPLEX : D2->xComplex.value = S->xComplex.value; break;
	default : return DaoValue_Move4( S, D, T, C, defs );
	}
	return 1;
}
int DaoValue_Move( DaoValue *S, DaoValue **D, DaoType *T )
{
	return DaoValue_Move5( S, D, T, T, NULL );
}
int DaoValue_Move2( DaoValue *S, DaoValue **D, DaoType *T, DMap *defs )
{
	int rc = DaoValue_Move5( S, D, T, T, defs );
	if( rc == 0 || T == NULL ) return rc;
	if( S->type <= DAO_TUPLE || S->type != T->tid ) return rc;
	if( S->type == DAO_CDATA ){
		if( S->xCdata.data == NULL ) rc = 0;
	}else{
		if( S == T->value ) rc = 0;
	}
	return rc;
}


#ifdef DAO_WITH_NUMARRAY
int DaoArray_Compare( DaoArray *x, DaoArray *y );
#endif

#define number_compare( x, y ) ((x)==(y) ? 0 : ((x)<(y) ? -1 : 1))

/* Invalid comparison returns either -100 or 100: */

int DaoComplex_Compare( DaoComplex *left, DaoComplex *right )
{
	if( left->value.real < right->value.real ) return -100;
	if( left->value.real > right->value.real ) return  100;
	if( left->value.imag < right->value.imag ) return -100;
	if( left->value.imag > right->value.imag ) return  100;
	return 0;
}

int DaoEnum_Compare( DaoEnum *L, DaoEnum *R )
{
	DaoEnum E;
	if( L->etype == R->etype ){
		return L->value == R->value ? 0 : (L->value < R->value ? -1 : 1);
	}else if( L->subtype == DAO_ENUM_SYM && R->subtype == DAO_ENUM_SYM ){
		return DString_CompareUTF8( L->etype->name, R->etype->name );
	}else if( L->subtype == DAO_ENUM_SYM ){
		E = *R;
		if( DaoEnum_SetValue( & E, L ) == 0 ) goto CompareName;
		return E.value == R->value ? 0 : (E.value < R->value ? -1 : 1);
	}else if( R->subtype == DAO_ENUM_SYM ){
		E = *L;
		if( DaoEnum_SetValue( & E, R ) == 0 ) goto CompareName;
		return L->value == E.value ? 0 : (L->value < E.value ? -1 : 1);
	}else if( DString_EQ( L->etype->fname, R->etype->fname ) ){
		return L->value == R->value ? 0 : (L->value < R->value ? -1 : 1);
	}
CompareName:
	return DString_CompareUTF8( L->etype->fname, R->etype->fname );
}
static int DaoValue_ComparePro2( DaoValue *left, DaoValue *right, DaoProcess *proc, int dep );
static void DaoProcess_WarnComparisonRecursion( DaoProcess *self )
{
	DaoStream *stream = self->vmSpace->errorStream;
	DaoStream_WriteChars( stream, "WARNING: recursion limit is reached for comparison!\n" );
}
static int DaoTuple_Compare( DaoTuple *lt, DaoTuple *rt, DaoProcess *process, int dep )
{
	int i, lb, rb, res;
	if( lt->size < rt->size ) return -100;
	if( lt->size > rt->size ) return 100;

	if( dep + (process ? process->depth : 0) > 1000 ){
		DaoProcess_WarnComparisonRecursion( process );
		return number_compare( (size_t)lt, (size_t)rt );
	}

	for(i=0; i<lt->size; i++){
		res = DaoValue_ComparePro2( lt->values[i], rt->values[i], process, dep+1 );
		if( res != 0 ) return res;
	}
	return 0;
}
static int DaoList_Compare( DaoList *list1, DaoList *list2, DaoProcess *process, int dep )
{
	DaoValue **d1 = list1->value->items.pValue;
	DaoValue **d2 = list2->value->items.pValue;
	int size1 = list1->value->size;
	int size2 = list2->value->size;
	int min = size1 < size2 ? size1 : size2;
	int res = size1 == size2 ? 1 : 100;
	int i, cmp = 0;

	if( dep + (process ? process->depth : 0) > 1000 ){
		DaoProcess_WarnComparisonRecursion( process );
		return number_compare( (size_t)list1, (size_t)list2 );
	}

	/* find the first unequal items */
	for(i=0; i<min; i++, d1++, d2++){
		cmp = DaoValue_ComparePro2( *d1, *d2, process, dep+1 );
		if( cmp != 0 ) break;
	}
	if( i < min ){
		if( abs( cmp ) > 1 ) return cmp;
		return cmp * res;
	}
	if( size1 == size2  ) return 0;
	return size1 < size2 ? -100 : 100;
}
static int DaoCstruct_Compare( DaoCstruct *left, DaoCstruct *right, DaoProcess *process, int dep )
{
	DaoRoutine *EQ = left->ctype->kernel->eqOperators;
	DaoRoutine *LT = left->ctype->kernel->ltOperators;
	DaoValue *P[2];
	int NE = 0;

	if( left == right ) return 0;
	if( process == NULL ) goto PointerComparison;
	if( (process->depth + dep) > 1000 ){
		DaoProcess_WarnComparisonRecursion( process );
		goto PointerComparison;
	}

	P[0] = (DaoValue*) left; /* To support calling static methods: */
	P[1] = (DaoValue*) right;

	if( EQ ){
		if( DaoProcess_Call( process, EQ, 0, P, 2 ) ) goto PointerComparison;
		if( DaoValue_GetInteger( process->stackValues[0] ) ) return 0;
		NE = 1;
	}
	if( LT ){
		if( DaoProcess_Call( process, LT, 0, P, 2 ) ) goto PointerComparison;
		if( DaoValue_GetInteger( process->stackValues[0] ) ) return -1;
		if( NE ) return 1;
	}
PointerComparison:
	if( left->ctype != right->ctype ){
		return number_compare( (size_t)left->ctype, (size_t)right->ctype );
	}else if( left->type == DAO_CDATA ){
		DaoCdata *l = (DaoCdata*) left;
		DaoCdata *r = (DaoCdata*) right;
		return number_compare( (size_t)l->data, (size_t)r->data );
	}
	return number_compare( (size_t)left, (size_t)right );
}
static int DaoObject_Compare( DaoObject *left, DaoObject *right, DaoProcess *process, int dep )
{
	DaoRoutine *EQ = left->defClass->eqOperators;
	DaoRoutine *LT = left->defClass->ltOperators;
	DaoValue *P[2];
	int NE = 0;

	if( process == NULL ) goto PointerComparison;
	if( (process->depth + dep) > 1000 ){
		DaoProcess_WarnComparisonRecursion( process );
		goto PointerComparison;
	}

	P[0] = (DaoValue*) left; /* To support calling static methods: */
	P[1] = (DaoValue*) right;

	if( EQ ){
		if( DaoProcess_Call( process, EQ, 0, P, 2 ) ) goto PointerComparison;
		if( DaoValue_GetInteger( process->stackValues[0] ) ) return 0;
		NE = 1;
	}
	if( LT ){
		if( DaoProcess_Call( process, LT, 0, P, 2 ) ) goto PointerComparison;
		if( DaoValue_GetInteger( process->stackValues[0] ) ) return -1;
		if( NE ) return 1;
	}
PointerComparison:
	return number_compare( (size_t)left, (size_t)right );
}
static int DaoType_Compare( DaoType *left, DaoType *right )
{
	if( DaoType_MatchTo( left, right, NULL ) >= DAO_MT_EQ ) return 0;
	return number_compare( (size_t)left, (size_t)right );
}
static int DaoValue_ComparePro2( DaoValue *left, DaoValue *right, DaoProcess *proc, int dep )
{
	double L, R;
	int res = 0;
	if( left == right ) return 0;
	if( left == NULL || right == NULL ) return left < right ? -100 : 100;
	if( left->type != right->type ){
		res = left->type < right->type ? -100 : 100;
		if( left->type < DAO_BOOLEAN || left->type > DAO_FLOAT ) return res;
		if( right->type < DAO_BOOLEAN || right->type > DAO_FLOAT ) return res;
		L = DaoValue_GetFloat( left );
		R = DaoValue_GetFloat( right );
		return L == R ? 0 : (L < R ? -1 : 1);
	}
	switch( left->type ){
	case DAO_NONE : return 0;
	case DAO_BOOLEAN :
	case DAO_INTEGER : return number_compare( left->xInteger.value, right->xInteger.value );
	case DAO_FLOAT   : return number_compare( left->xFloat.value, right->xFloat.value );
	case DAO_COMPLEX : return DaoComplex_Compare( & left->xComplex, & right->xComplex );
	case DAO_STRING  : return DString_CompareUTF8( left->xString.value, right->xString.value );
	case DAO_ENUM    : return DaoEnum_Compare( & left->xEnum, & right->xEnum );
	case DAO_TUPLE   : return DaoTuple_Compare( & left->xTuple, & right->xTuple, proc, dep );
	case DAO_LIST    : return DaoList_Compare( & left->xList, & right->xList, proc, dep );
	case DAO_OBJECT  : return DaoObject_Compare( (DaoObject*)left, (DaoObject*)right, proc, dep );
	case DAO_CDATA   :
	case DAO_CSTRUCT :
	case DAO_CTYPE : return DaoCstruct_Compare( (DaoCstruct*)left, (DaoCstruct*)right, proc, dep );
	case DAO_TYPE : return DaoType_Compare( (DaoType*) left, (DaoType*) right );
#ifdef DAO_WITH_NUMARRAY
	case DAO_ARRAY   : return DaoArray_Compare( & left->xArray, & right->xArray );
#endif
	}
	return left < right ? -100 : 100; /* needed for map */
}
int DaoValue_ComparePro( DaoValue *left, DaoValue *right, DaoProcess *proc )
{
	return DaoValue_ComparePro2( left, right, proc, 0 );
}
int DaoValue_Compare( DaoValue *left, DaoValue *right )
{
	return DaoValue_ComparePro2( left, right, NULL, 0 );
}


extern DaoTypeBase baseTyper;
extern DaoTypeBase numberTyper;
extern DaoTypeBase comTyper;
extern DaoTypeBase stringTyper;
extern DaoTypeBase enumTyper;

DaoType* DaoValue_GetType( DaoValue *self )
{
	if( self == NULL ) return NULL;

	switch( self->type ){
	case DAO_NONE    : return self->xBase.subtype == DAO_ANY ? dao_type_any : dao_type_none;
	case DAO_BOOLEAN : return dao_type_bool;
	case DAO_INTEGER : return dao_type_int;
	case DAO_FLOAT   : return dao_type_float;
	case DAO_COMPLEX : return dao_type_complex;
	case DAO_STRING  : return dao_type_string;
	case DAO_ENUM    : return self->xEnum.etype ? self->xEnum.etype : dao_type_enum;
#ifdef DAO_WITH_NUMARRAY
	case DAO_ARRAY : return dao_array_types[ self->xArray.etype ];
#endif
	case DAO_LIST   : return self->xList.ctype;
	case DAO_MAP    : return self->xMap.ctype;
	case DAO_TUPLE  : return self->xTuple.ctype;
	case DAO_OBJECT : return self->xObject.defClass->objType;
	case DAO_CLASS  : return self->xClass.clsType;
	case DAO_CTYPE   :
	case DAO_CDATA   :
	case DAO_CSTRUCT : return self->xCdata.ctype;
	case DAO_ROUTINE   : return self->xRoutine.routType;
	case DAO_PAR_NAMED : return self->xNameValue.ctype;
	case DAO_INTERFACE : return self->xInterface.abtype;
	case DAO_CINTYPE   : return self->xCinType.citype;
	case DAO_CINVALUE  : return self->xCinValue.cintype->vatype;
	default : break;
	}
	return NULL;
}
DaoTypeBase* DaoValue_GetTyper( DaoValue *self )
{
	if( self == NULL ) return & baseTyper;
	switch( self->type ){
	case DAO_NONE : return & baseTyper;
	case DAO_INTEGER :
	case DAO_FLOAT   : return & numberTyper;
	case DAO_COMPLEX : return & comTyper;
	case DAO_ENUM    : return & enumTyper;
	case DAO_STRING  : return & stringTyper;
	case DAO_CTYPE   :
	case DAO_CSTRUCT :
	case DAO_CDATA   : return self->xCdata.ctype->typer;
	default : break;
	}
	return DaoVmSpace_GetTyper( self->type );
}

void DaoValue_GetField( DaoValue *self, DaoProcess *proc, DString *name )
{
	DaoType *type = DaoNamespace_GetType( proc->activeNamespace, self );
	DaoValue *value = DaoType_FindValue( type, name );
	if( value == NULL ){
		DaoRoutine *func = NULL;
		DaoString str = {DAO_STRING,0,0,0,1,NULL};
		DaoValue *pars = (DaoValue*) & str;
		int error, npar = 0;

		str.value = name;
		DString_SetChars( proc->string, "." );
		DString_Append( proc->string, name );
		func = DaoType_FindFunction( type, proc->string );
		if( func == NULL ){
			npar = 1;
			DString_SetChars( proc->string, "." );
			func = DaoType_FindFunction( type, proc->string );
		}
		if( func == NULL ){
			DaoProcess_RaiseError( proc, "Field::NotExist", "not exist" );
			return;
		}
		if( (error = DaoProcess_PushCallable( proc, func, self, & pars, npar )) != 0 ){
			DaoProcess_RaiseException( proc, daoExceptionNames[error], NULL, NULL );
		}
	}else{
		DaoProcess_PutValue( proc, value );
	}
}
void DaoValue_SetField( DaoValue *self, DaoProcess *proc, DString *name, DaoValue *value )
{
    int npar = 1; 
    DaoValue *pars[2];
    DaoRoutine *func = NULL;
    DaoString str = {DAO_STRING,0,0,0,1,NULL};
	DaoType *type = DaoNamespace_GetType( proc->activeNamespace, self );

    str.value = name;
    pars[0] = pars[1] = value;

    DString_SetChars( proc->string, "." );
    DString_Append( proc->string, name );
    DString_AppendChars( proc->string, "=" );
    func = DaoType_FindFunction( type, proc->string );
    if( func == NULL ){
        pars[0] = (DaoValue*) & str; 
        npar = 2; 
        DString_SetChars( proc->string, ".=" );
        func = DaoType_FindFunction( type, proc->string );
    }    
    if( func == NULL ){
        DaoProcess_RaiseError( proc, "Field::NotExist", name->chars );
        return;
    }    
    DaoProcess_PushCallable( proc, func, self, pars, npar );
}
void DaoValue_GetItem( DaoValue *self, DaoProcess *proc, DaoValue *pid[], int N )
{
	DaoType *type = DaoNamespace_GetType( proc->activeNamespace, self );
	DaoRoutine *func = DaoType_FindFunctionChars( type, "[]" );
	if( func == NULL ){
		DaoProcess_RaiseError( proc, "Field::NonExist", "[]" );
		return;
	}
	DaoProcess_PushCallable( proc, func, self, pid, N );
}
void DaoValue_SetItem( DaoValue *self, DaoProcess *proc, DaoValue *pid[], int N, DaoValue *value )
{
	DaoType *type = DaoNamespace_GetType( proc->activeNamespace, self );
	DaoRoutine *func = DaoType_FindFunctionChars( type, "[]=" );
	DaoValue *p[ DAO_MAX_PARAM ];
	memcpy( p+1, pid, N*sizeof(DaoValue*) );
	p[0] = value;
	if( func == NULL ){
		DaoProcess_RaiseError( proc, "Field::NonExist", "[]=" );
		return;
	}
	DaoProcess_PushCallable( proc, func, self, p, N+1 );
}

static void DaoValue_BasicPrint( DaoValue *self, DaoStream *stream, DMap *cycData )
{
	if( self->type <= DAO_TUPLE )
		DaoStream_WriteChars( stream, coreTypeNames[ self->type ] );
	else
		DaoStream_WriteChars( stream, DaoValue_GetTyper( self )->name );
	if( self->type == DAO_NONE ) return;
	if( self->type == DAO_TYPE ){
		DaoStream_WriteChars( stream, "<" );
		DaoStream_WriteChars( stream, self->xType.name->chars );
		DaoStream_WriteChars( stream, ">" );
	}
	DaoStream_WriteChars( stream, "_" );
	DaoStream_WriteInt( stream, self->type );
	DaoStream_WriteChars( stream, "_" );
	DaoStream_WritePointer( stream, self );
}
static void DaoValue_PrintEx( DaoValue *self, DaoProcess *proc, DaoStream *stream, DMap *cycData )
{
	int ec = 0;
	char buf[50];
	DaoRoutine *meth;
	DaoValue *params[2];
	DaoType *type = DaoNamespace_GetType( proc->activeNamespace, self );
	DaoCinValue *cinvalue = NULL;
	DaoCstruct *cstruct = NULL;

	switch( self->type ){
	case DAO_CDATA :
	case DAO_CSTRUCT  : cstruct  = (DaoCstruct*) self; break;
	case DAO_CINVALUE : cinvalue = (DaoCinValue*) self; break;
	}

	sprintf( buf, "[%p]", self );

	if( cstruct ){
		if( self == cstruct->ctype->value ){
			DaoStream_WriteString( stream, cstruct->ctype->name );
			DaoStream_WriteChars( stream, "[default]" );
			return;
		}
	}
	if( cycData != NULL && DMap_Find( cycData, self ) != NULL ){
		DaoStream_WriteString( stream, type->name );
		DaoStream_WriteChars( stream, buf );
		return;
	}
	if( cycData ) MAP_Insert( cycData, self, self );

	params[0] = (DaoValue*) dao_type_string;
	params[1] = (DaoValue*) stream;
	meth = DaoType_FindFunctionChars( type, "(string)" );
	if( meth ){
		ec = DaoProcess_Call( proc, meth, self, params, 2 );
		if( ec ) ec = DaoProcess_Call( proc, meth, self, params, 1 );
	}else{
		meth = DaoType_FindFunctionChars( type, "serialize" );
		if( meth ) ec = DaoProcess_Call( proc, meth, self, NULL, 0 );
	}
	if( ec ){
		DaoProcess_RaiseException( proc, daoExceptionNames[ec], proc->string->chars, NULL );
	}else if( meth && proc->stackValues[0] ){
		DaoValue_Print( proc->stackValues[0], proc, stream, cycData );
	}else if( cinvalue ){
		DaoValue_Print( cinvalue->value, proc, stream, cycData );
	}else{
		DaoStream_WriteString( stream, type->name );
		DaoStream_WriteChars( stream, buf );
	}
}
void DaoValue_Print( DaoValue *self, DaoProcess *proc, DaoStream *stream, DMap *cycData )
{
	DString *name;
	DaoTypeBase *typer;
	DMap *cd = cycData;
	if( self == NULL ){
		DaoStream_WriteChars( stream, "none[0x0]" );
		return;
	}
	if( cycData == NULL ) cycData = DMap_New(0,0);
	switch( self->type ){
	case DAO_BOOLEAN :
		DaoStream_WriteChars( stream, self->xBoolean.value ? "true" : "false" ); break;
	case DAO_INTEGER :
		DaoStream_WriteInt( stream, self->xInteger.value ); break;
	case DAO_FLOAT   :
		DaoStream_WriteFloat( stream, self->xFloat.value ); break;
	case DAO_COMPLEX :
		DaoStream_WriteFloat( stream, self->xComplex.value.real );
		if( self->xComplex.value.imag >= -0.0 ) DaoStream_WriteChars( stream, "+" );
		DaoStream_WriteFloat( stream, self->xComplex.value.imag );
		DaoStream_WriteChars( stream, "C" );
		break;
	case DAO_ENUM  :
		name = DString_New();
		DaoEnum_MakeName( & self->xEnum, name );
		DaoStream_WriteChars( stream, name->chars );
		DaoStream_WriteChars( stream, "(" );
		DaoStream_WriteInt( stream, self->xEnum.value );
		DaoStream_WriteChars( stream, ")" );
		DString_Delete( name );
		break;
	case DAO_STRING  :
		DaoStream_WriteString( stream, self->xString.value );
		break;
	case DAO_CDATA :
	case DAO_CSTRUCT :
	case DAO_CINVALUE :
		DaoValue_PrintEx( self, proc, stream, cycData );
		break;
	default :
		typer = DaoVmSpace_GetTyper( self->type );
		if( typer->core->Print == DaoValue_Print ){
			DaoValue_BasicPrint( self, stream, cycData );
			break;
		}
		typer->core->Print( self, proc, stream, cycData );
		break;
	}
	if( cycData != cd ) DMap_Delete( cycData );
}



int DaoValue_Type( DaoValue *self )
{
	return self->type;
}

DaoBoolean* DaoValue_CastBoolean( DaoValue *self )
{
	if( self == NULL || self->type != DAO_BOOLEAN ) return NULL;
	return (DaoBoolean*) self;
}
DaoInteger* DaoValue_CastInteger( DaoValue *self )
{
	if( self == NULL || self->type != DAO_INTEGER ) return NULL;
	return (DaoInteger*) self;
}
DaoFloat* DaoValue_CastFloat( DaoValue *self )
{
	if( self == NULL || self->type != DAO_FLOAT ) return NULL;
	return (DaoFloat*) self;
}
DaoComplex* DaoValue_CastComplex( DaoValue *self )
{
	if( self == NULL || self->type != DAO_COMPLEX ) return NULL;
	return (DaoComplex*) self;
}
DaoString* DaoValue_CastString( DaoValue *self )
{
	if( self == NULL || self->type != DAO_STRING ) return NULL;
	return (DaoString*) self;
}
DaoEnum* DaoValue_CastEnum( DaoValue *self )
{
	if( self == NULL || self->type != DAO_ENUM ) return NULL;
	return (DaoEnum*) self;
}
DaoArray* DaoValue_CastArray( DaoValue *self )
{
	if( self == NULL || self->type != DAO_ARRAY ) return NULL;
	return (DaoArray*) self;
}
DaoList* DaoValue_CastList( DaoValue *self )
{
	if( self == NULL || self->type != DAO_LIST ) return NULL;
	return (DaoList*) self;
}
DaoMap* DaoValue_CastMap( DaoValue *self )
{
	if( self == NULL || self->type != DAO_MAP ) return NULL;
	return (DaoMap*) self;
}
DaoTuple* DaoValue_CastTuple( DaoValue *self )
{
	if( self == NULL || self->type != DAO_TUPLE ) return NULL;
	return (DaoTuple*) self;
}
DaoStream* DaoValue_CastStream( DaoValue *self )
{
	if( self == NULL || self->type != DAO_CSTRUCT ) return NULL;
	return (DaoStream*) DaoValue_CastCstruct( self, dao_type_stream );
}
DaoObject* DaoValue_CastObject( DaoValue *self )
{
	if( self == NULL || self->type != DAO_OBJECT ) return NULL;
	return (DaoObject*) self;
}
DaoCstruct* DaoValue_CastCstruct( DaoValue *self, DaoType *type )
{
	if( self == NULL || type == NULL ) return (DaoCstruct*) self;
	if( self->type == DAO_OBJECT ){
		self = (DaoValue*) DaoObject_CastCstruct( (DaoObject*) self, type );
		if( self == NULL ) return NULL;
	}
	if( self->type != DAO_CSTRUCT && self->type != DAO_CDATA ) return NULL;
	if( DaoType_ChildOf( self->xCstruct.ctype, type ) ) return (DaoCstruct*) self;
	return NULL;
}
DaoCdata* DaoValue_CastCdata( DaoValue *self, DaoType *type )
{
	if( self == NULL || type == NULL ) return (DaoCdata*) self;
	if( self->type == DAO_OBJECT ){
		self = (DaoValue*) DaoObject_CastCdata( (DaoObject*) self, type );
		if( self == NULL ) return NULL;
	}
	if( self->type != DAO_CDATA ) return NULL;
	if( DaoType_ChildOf( self->xCdata.ctype, type ) ) return (DaoCdata*) self;
	return NULL;
}
DaoClass* DaoValue_CastClass( DaoValue *self )
{
	if( self == NULL || self->type != DAO_CLASS ) return NULL;
	return (DaoClass*) self;
}
DaoInterface* DaoValue_CastInterface( DaoValue *self )
{
	if( self == NULL || self->type != DAO_INTERFACE ) return NULL;
	return (DaoInterface*) self;
}
DaoRoutine* DaoValue_CastRoutine( DaoValue *self )
{
	if( self == NULL || self->type != DAO_ROUTINE ) return NULL;
	return (DaoRoutine*) self;
}
DaoProcess* DaoValue_CastProcess( DaoValue *self )
{
	if( self == NULL || self->type != DAO_PROCESS ) return NULL;
	return (DaoProcess*) self;
}
DaoNamespace* DaoValue_CastNamespace( DaoValue *self )
{
	if( self == NULL || self->type != DAO_NAMESPACE ) return NULL;
	return (DaoNamespace*) self;
}
DaoType* DaoValue_CastType( DaoValue *self )
{
	if( self == NULL || self->type != DAO_TYPE ) return NULL;
	return (DaoType*) self;
}

DaoValue* DaoValue_MakeNone()
{
	return dao_none_value;
}

dao_boolean DaoValue_TryGetBoolean( DaoValue *self )
{
	if( self->type != DAO_BOOLEAN ) return 0;
	return self->xBoolean.value;
}
dao_integer DaoValue_TryGetInteger( DaoValue *self )
{
	if( self->type != DAO_INTEGER ) return 0;
	return self->xInteger.value;
}
dao_float DaoValue_TryGetFloat( DaoValue *self )
{
	if( self->type != DAO_FLOAT ) return 0.0;
	return self->xFloat.value;
}
dao_complex DaoValue_TryGetComplex( DaoValue *self )
{
	dao_complex com = {0.0,0.0};
	if( self->type != DAO_COMPLEX ) return com;
	return self->xComplex.value;
}
int DaoValue_TryGetEnum(DaoValue *self)
{
	if( self->type != DAO_ENUM ) return 0;
	return self->xEnum.value;
}
char* DaoValue_TryGetChars( DaoValue *self )
{
	if( self->type != DAO_STRING ) return NULL;
	return DString_GetData( self->xString.value );
}
DString* DaoValue_TryGetString( DaoValue *self )
{
	if( self->type != DAO_STRING ) return NULL;
	return self->xString.value;
}
void* DaoValue_TryGetArray( DaoValue *self )
{
	if( self->type != DAO_ARRAY ) return NULL;
	return self->xArray.data.p;
}
void* DaoValue_TryCastCdata( DaoValue *self, DaoType *type )
{
	if( self->type == DAO_OBJECT ){
		self = (DaoValue*) DaoObject_CastCdata( (DaoObject*) self, type );
	}
	if( self == NULL || self->type != DAO_CDATA ) return NULL;
	if( self->type != DAO_CDATA ) return NULL;
	return DaoCdata_CastData( & self->xCdata, type );
}
void* DaoValue_TryGetCdata( DaoValue *self )
{
	if( self->type != DAO_CDATA ) return NULL;
	return self->xCdata.data;
}
void** DaoValue_TryGetCdata2( DaoValue *self )
{
	if( self->type != DAO_CDATA ) return NULL;
	return & self->xCdata.data;
}
void DaoValue_ClearAll( DaoValue *v[], int n )
{
	int i;
	for(i=0; i<n; i++) DaoValue_Clear( v + i );
}

