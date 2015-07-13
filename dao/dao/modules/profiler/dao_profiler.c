/*
// Dao Profiler
//
// Copyright (c) 2013,2014, Limin Fu
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

#include <string.h>
#include <time.h>
#include <stdlib.h>

#include "dao_profiler.h"

#ifdef UNIX
#  include<unistd.h>
#  include<sys/time.h>
#endif

#ifdef MAC_OSX
#  include <crt_externs.h>
#endif


static void DaoProfiler_Reset( DaoProfiler *self );
static void DaoProfiler_EnterFrame( DaoProfiler *self, DaoProcess *, DaoStackFrame *, int );
static void DaoProfiler_LeaveFrame( DaoProfiler *self, DaoProcess *, DaoStackFrame *, int );
static void DaoProfiler_Report( DaoProfiler *self0, DaoStream *stream );

DaoxProfiler* DaoxProfiler_New()
{
	DaoxProfiler *self = (DaoxProfiler*) dao_calloc(1,sizeof(DaoxProfiler));
	self->profile = DHash_New( 0, DAO_DATA_MAP );
	self->one = DHash_New( 0, DAO_DATA_VALUE );
	self->base.EnterFrame = DaoProfiler_EnterFrame;
	self->base.LeaveFrame = DaoProfiler_LeaveFrame;
	self->base.Report = DaoProfiler_Report;
	self->base.Reset = DaoProfiler_Reset;
	DMutex_Init( & self->mutex );
	return self;
}
void DaoxProfiler_Delete( DaoxProfiler *self )
{
	DMutex_Destroy( & self->mutex );
	DMap_Delete( self->profile );
	DMap_Delete( self->one );
	dao_free( self );
}
void DaoProfiler_Reset( DaoProfiler *self0 )
{
	DaoxProfiler *self = (DaoxProfiler*) self0;
	DMap_Clear( self->profile );
}
static void DaoxProfiler_Update( DaoxProfiler *self, DaoStackFrame *frame, double time )
{
	DaoComplex com = {DAO_COMPLEX,0,0,0,1,{0.0,0.0}};
	DaoRoutine *caller = frame->prev ? frame->prev->routine : NULL;
	DaoRoutine *callee = frame->routine;
	DNode *it, *it2;

	if( frame->returning == 0xffff ) caller = NULL;
	while( caller && caller->original ) caller = caller->original;
	while( callee && callee->original ) callee = callee->original;

	DMutex_Lock( & self->mutex );
	{
		it = DMap_Find( self->profile, callee );
		if( it == NULL ) it = DMap_Insert( self->profile, callee, self->one );

		it2 = DMap_Find( it->value.pMap, caller );
		if( it2 == NULL ) it2 = DMap_Insert( it->value.pMap, caller, & com );
		it2->value.pValue->xComplex.value.real += time;
		it2->value.pValue->xComplex.value.imag += 1;
	}
	DMutex_Unlock( & self->mutex );
}


static double dao_clock()
{
	return ((double)clock())/CLOCKS_PER_SEC;
}

static void DMap_DeleteTimeMap( DMap *self )
{
	DMap_Delete( self );
}

static DMap* DaoProcess_GetTimeMap( DaoProcess *self )
{
	DMap *mapTime = (DMap*) DaoProcess_GetAuxData( self, DMap_DeleteTimeMap );
	if( mapTime ) return mapTime;
	mapTime = DHash_New( 0, DAO_DATA_VALUE );
	DaoProcess_SetAuxData( self, DMap_DeleteTimeMap, mapTime );
	return mapTime;
}
static DaoComplex* DaoProcess_GetTimeData( DaoProcess *self, DaoStackFrame *frame )
{
	DaoComplex com = {DAO_COMPLEX,0,0,0,1,{0.0,0.0}};
	DMap *mapTime = DaoProcess_GetTimeMap( self );
	DNode *it = DMap_Find( mapTime, frame );
	if( it == NULL ) it = DMap_Insert( mapTime, frame, & com );
	return (DaoComplex*) it->value.pValue;
}

static void DaoProfiler_EnterFrame( DaoProfiler *self, DaoProcess *proc, DaoStackFrame *frame, int start )
{
	DaoComplex *tmdata;
	if( frame->routine == NULL ) return;
	//printf( "DaoProfiler_EnterFrame: %-20s %s\n", frame->routine->routName->chars, start ? "start":"" );

	tmdata = DaoProcess_GetTimeData( proc, frame );
	if( start ) tmdata->value.real = 0.0;
	tmdata->value.imag = dao_clock();
}
static void DaoProfiler_LeaveFrame( DaoProfiler *self, DaoProcess *proc, DaoStackFrame *frame, int end )
{
	double time;
	DaoComplex *tmdata;
	if( frame->routine == NULL ) return;
	//printf( "DaoProfiler_LeaveFrame: %-20s %s\n", frame->routine->routName->chars, end?"end":"" );

	time = dao_clock();
	tmdata = DaoProcess_GetTimeData( proc, frame );
	tmdata->value.real += time - tmdata->value.imag;
	tmdata->value.imag = time;
	if( end ) DaoxProfiler_Update( (DaoxProfiler*) self, frame, tmdata->value.real );
}

dao_complex DaoProfiler_Sum( DMap *profile )
{
	DNode *it2;
	dao_complex com = {0.0, 0.0};
	for(it2=DMap_First(profile); it2; it2=DMap_Next(profile,it2)){
		DaoRoutine *caller = (DaoRoutine*) it2->key.pValue;
		DaoComplex *data = (DaoComplex*) it2->value.pValue;
		com.real += data->value.real;
		com.imag += data->value.imag;
	}
	return com;
}



const char *delimiter = 
"============== Program Profile (Time in Seconds) ==============\n";

const char *delimiter2 =
"-------------------------------------------------------------------------------\n";

const char *delimiter3 =
"-------------------------------------------------------------------------------\n";

const char *header_format = "%-58s: %9s, %8s\n";
const char *row_format = "%-58s: %9i, %8.2f\n";

const char *header_format2 = "%-32s: %24s, %9s, %8s\n";
const char *row_format2 = "%-32s: %24s, %9i, %8.2f\n";

static void DString_PartialAppend( DString *self, DString *other, int max )
{
	if( other->size > max ){
		DString_AppendBytes( self, other->chars, max-1-!isalnum(other->chars[max-2]) );
		DString_AppendChar( self, '~' );
	}else{
		DString_Append( self, other );
	}
}
void DaoRoutine_MakeName( DaoRoutine *self, DString *name, int max1, int max2, int max3 )
{
	DString *hostName = self->routHost ? self->routHost->name : NULL;
	DaoType *routType = self->routType;
	int M = (routType->attrib & DAO_TYPE_SELF) != 0;
	int N = routType->nested->size;
	int i;

	DString_Reset( name, 0 );

	/* For builtin containers, whose methods may have no routHost set: */
	if( hostName == NULL && M ) hostName = routType->nested->items.pType[0]->aux->xType.name;

	/*
	// Mixin classes have methods converted from constructors of component classes.
	// These methods still have the DAO_ROUT_INITOR flag. So checking the names is
	// the better way to use here.
	*/
	if( hostName && ! DString_EQ( self->routName, hostName ) ){
		if( hostName->size + self->routName->size < (max1-2) ){
			DString_Append( name, hostName );
			DString_AppendChars( name, "::" );
			DString_Append( name, self->routName );
		}else{
			DString_PartialAppend( name, hostName, max1/2-1 );
			DString_AppendChars( name, "::" );
			DString_PartialAppend( name, self->routName, max1/2-1 );
		}
	}else{
		DString_PartialAppend( name, self->routName, max1 );
	}
	if( max3 == 0 ){
		DString_AppendChars( name, "()" );
		return;
	}
	DString_AppendChars( name, "( " );
	if( N == M+1 ){
		DaoType *type = routType->nested->items.pType[M];
		DString_PartialAppend( name, type->name, 2*max2 );
	}else{
		for(i=M; i<N; ++i){
			DaoType *type = routType->nested->items.pType[i];
			if( i > M ) DString_AppendChars( name, ", " );
			if( i < M + max3 ){
				DString_PartialAppend( name, type->name, max2 );
			}else{
				DString_AppendChars( name, "~~" );
				break;
			}
		}
	}
	DString_AppendChars( name, " )" );
}

void DaoProfiler_Report( DaoProfiler *self0, DaoStream *stream )
{
	DaoComplex com = {DAO_COMPLEX,0,0,0,1,{0.0,0.0}};
	DaoxProfiler *self = (DaoxProfiler*) self0;
	DMap *summary = DMap_New( DAO_DATA_VALUE, 0 );
	DMap *summary2 = DMap_New( DAO_DATA_VALUE, 0 );
	DString *name1 = DString_New();
	DString *name2 = DString_New();
	DNode *it, *it2;
	int count, max = 20;
	char buf1[32];
	char buf2[24];
	char buf[120];

	for(it=DMap_First(self->profile); it; it=DMap_Next(self->profile,it)){
		DaoRoutine *callee = (DaoRoutine*) it->key.pValue;
		com.value = DaoProfiler_Sum( it->value.pMap );
		com.value.real = - com.value.real;
		com.value.imag = - com.value.imag;
		DMap_Insert( summary, & com, it );
	}

	DaoStream_WriteChars( stream, "\n" );
	DaoStream_WriteChars( stream, delimiter );
	DaoStream_WriteChars( stream, delimiter2 );
	snprintf( buf, sizeof(buf), header_format, "Routine", "#Calls", "CPU Time" );
	DaoStream_WriteChars( stream, buf );
	DaoStream_WriteChars( stream, delimiter2 );
	for(count=max,it=DMap_First(summary); it; it=DMap_Next(summary,it),--count){
		DNode *it2 = (DNode*) it->value.pVoid;
		dao_complex data = it->key.pValue->xComplex.value;
		DaoRoutine *callee = (DaoRoutine*) it2->key.pValue;
		DaoRoutine_MakeName( callee, name1, 28, 10, 2 );
		snprintf( buf, sizeof(buf), row_format, name1->chars, (int) -data.imag, -data.real );
		DaoStream_WriteChars( stream, buf );
		if( count == 0 ) break;
	}

	DaoStream_WriteChars( stream, "\n" );
	DaoStream_WriteChars( stream, delimiter3 );
	snprintf( buf, sizeof(buf), header_format2, "Routine", "Caller", "#Calls", "CPU Time" );
	DaoStream_WriteChars( stream, buf );
	DaoStream_WriteChars( stream, delimiter3 );
	for(count=max,it=DMap_First(summary); it; it=DMap_Next(summary,it),--count){
		DNode *it2 = (DNode*) it->value.pVoid;
		DaoRoutine *callee = (DaoRoutine*) it2->key.pValue;
		DMap *profile = it2->value.pMap;

		DaoRoutine_MakeName( callee, name1, 30, 0, 0 );

		DMap_Reset( summary2 );
		for(it2=DMap_First(profile); it2; it2=DMap_Next(profile,it2)){
			DaoRoutine *caller = (DaoRoutine*) it2->key.pValue;
			DaoComplex *data = (DaoComplex*) it2->value.pValue;
			com.value.real = - data->value.real;
			com.value.imag = - data->value.imag;
			DMap_Insert( summary2, & com, caller );
		}
		for(it2=DMap_First(summary2); it2; it2=DMap_Next(summary2,it2)){
			DaoRoutine *caller = (DaoRoutine*) it2->value.pValue;
			dao_complex data = it2->key.pValue->xComplex.value;
			DString_Reset( name2, 0 );
			if( caller ) DaoRoutine_MakeName( caller, name2, 22, 0, 0 );
			snprintf( buf, sizeof(buf), row_format2, name1->chars, name2->chars, (int) -data.imag, -data.real );
			DaoStream_WriteChars( stream, buf );
			DString_Reset( name1, 0 );
		}
		if( count == 0 ) break;
	}
	DString_Delete( name1 );
	DString_Delete( name2 );
}

DAO_DLL int DaoProfiler_OnLoad( DaoVmSpace *vmSpace, DaoNamespace *ns )
{
	DaoxProfiler *profiler = DaoxProfiler_New();
	DaoVmSpace_SetUserProfiler( vmSpace, (DaoProfiler*) profiler );
	return 0;
}
