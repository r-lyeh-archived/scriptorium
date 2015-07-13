/*
// Dao Standard Modules
// http://www.daovm.net
//
// Copyright (c) 2011,2012, Limin Fu
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
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
// EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
// SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
// OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include<stdlib.h>
#include<string.h>
#include<math.h>
#include"daoString.h"
#include"daoValue.h"
#include"daoParser.h"
#include"daoNamespace.h"
#include"daoNumtype.h"
#include"daoGC.h"
#include"dao_aux.h"



#define DefineFunction_DaoArray_SetVector( name, type ) \
void name( DaoArray *self, type *vec, daoint N ) \
{ \
	daoint i; \
	if( vec && N == 0 ){ \
		DaoArray_UseData( self, vec ); \
		return; \
	} \
	if( N != self->size ) DaoArray_ResizeData( self, N, self->size ); \
	switch( self->etype ){ \
	case DAO_INTEGER : for(i=0; i<N; i++) self->data.i[i] = (daoint) vec[i]; break; \
	case DAO_FLOAT   : for(i=0; i<N; i++) self->data.f[i] = vec[i]; break; \
	case DAO_COMPLEX : \
		for(i=0; i<N; i++){ \
			self->data.c[i].real = vec[i+i]; \
			self->data.c[i].imag = vec[i+i+1]; \
		} \
		break; \
	default : break; \
	} \
}

DefineFunction_DaoArray_SetVector( DaoArray_SetVectorSInt8, signed char );
DefineFunction_DaoArray_SetVector( DaoArray_SetVectorSInt16, signed short );
DefineFunction_DaoArray_SetVector( DaoArray_SetVectorSInt32, signed int );
DefineFunction_DaoArray_SetVector( DaoArray_SetVectorUInt8, unsigned char );
DefineFunction_DaoArray_SetVector( DaoArray_SetVectorUInt16, unsigned short );
DefineFunction_DaoArray_SetVector( DaoArray_SetVectorUInt32, unsigned int );
DefineFunction_DaoArray_SetVector( DaoArray_SetVectorInt64, long long );
DefineFunction_DaoArray_SetVector( DaoArray_SetVectorFloat32, float );
DefineFunction_DaoArray_SetVector( DaoArray_SetVectorFloat64, double );

#define DefineFunction_DaoArray_SetMatrix( name, type ) \
void name( DaoArray *self, type **mat, daoint R, daoint C ) \
{ \
	daoint dm[2]; \
	daoint i, N = R * C; \
	dm[0] = R; dm[1] = C; \
	if( N != self->size ) DaoArray_ResizeData( self, N, self->size ); \
	DaoArray_Reshape( self, dm, 2 ); \
	switch( self->etype ){ \
	case DAO_INTEGER : for(i=0; i<N; i++) self->data.i[i] = (long long)mat[i/R][i%R]; break; \
	case DAO_FLOAT   : for(i=0; i<N; i++) self->data.f[i] = mat[i/R][i%R]; break; \
	case DAO_COMPLEX : for(i=0; i<N; i++){ \
						   self->data.c[i].real = mat[i/R][2*(i%R)]; \
						   self->data.c[i].imag = mat[i/R][2*(i%R)+1]; \
					   } \
					   break; \
	default : break; \
	} \
}

DefineFunction_DaoArray_SetMatrix( DaoArray_SetMatrixSInt8, signed char );
DefineFunction_DaoArray_SetMatrix( DaoArray_SetMatrixSInt16, signed short );
DefineFunction_DaoArray_SetMatrix( DaoArray_SetMatrixSInt32, signed int );
DefineFunction_DaoArray_SetMatrix( DaoArray_SetMatrixUInt8, unsigned char );
DefineFunction_DaoArray_SetMatrix( DaoArray_SetMatrixUInt16, unsigned short );
DefineFunction_DaoArray_SetMatrix( DaoArray_SetMatrixUInt32, unsigned int );
DefineFunction_DaoArray_SetMatrix( DaoArray_SetMatrixInt64, long long );
DefineFunction_DaoArray_SetMatrix( DaoArray_SetMatrixFloat32, float );
DefineFunction_DaoArray_SetMatrix( DaoArray_SetMatrixFloat64, double );


#ifdef DAO_WITH_NUMARRAY
DaoArray* DaoProcess_NewVectorSInt8( DaoProcess *self, signed char *s, daoint n )
{
	DaoArray *res = DaoArray_New( DAO_INTEGER );
	if( s ) DaoArray_SetVectorSInt8( res, s, n );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewVectorUInt8( DaoProcess *self, unsigned char *s, daoint n )
{
	DaoArray *res = DaoArray_New( DAO_INTEGER );
	if( s ) DaoArray_SetVectorUInt8( res, s, n );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewVectorSInt16( DaoProcess *self, signed short *s, daoint n )
{
	DaoArray *res = DaoArray_New( DAO_INTEGER );
	if( s ) DaoArray_SetVectorSInt16( res, s, n );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewVectorUInt16( DaoProcess *self, unsigned short *s, daoint n )
{
	DaoArray *res = DaoArray_New( DAO_INTEGER );
	if( s ) DaoArray_SetVectorUInt16( res, s, n );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewVectorSInt32( DaoProcess *self, signed int *s, daoint n )
{
	DaoArray *res = DaoArray_New( DAO_INTEGER );
	if( s ) DaoArray_SetVectorSInt32( res, s, n );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewVectorUInt32( DaoProcess *self, unsigned int *s, daoint n )
{
	DaoArray *res = DaoArray_New( DAO_INTEGER );
	if( s ) DaoArray_SetVectorUInt32( res, s, n );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewVectorInt64( DaoProcess *self, long long *s, daoint n )
{
	DaoArray *res = DaoArray_New( DAO_INTEGER );
	if( s ) DaoArray_SetVectorInt64( res, s, n );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewVectorFloat32( DaoProcess *self, float *s, daoint n )
{
	DaoArray *res = DaoArray_New( DAO_FLOAT );
	if( s ) DaoArray_SetVectorFloat32( res, s, n );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewVectorFloat64( DaoProcess *self, double *s, daoint n )
{
	DaoArray *res = DaoArray_New( DAO_FLOAT );
	if( s ) DaoArray_SetVectorFloat64( res, s, n );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewMatrixSInt8( DaoProcess *self, signed char **s, daoint n, daoint m )
{
	DaoArray *res = DaoArray_New( DAO_INTEGER );
	if( s ) DaoArray_SetMatrixSInt8( res, s, n, m );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewMatrixUInt8( DaoProcess *self, unsigned char **s, daoint n, daoint m )
{
	DaoArray *res = DaoArray_New( DAO_INTEGER );
	if( s ) DaoArray_SetMatrixUInt8( res, s, n, m );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewMatrixSInt16( DaoProcess *self, signed short **s, daoint n, daoint m )
{
	DaoArray *res = DaoArray_New( DAO_INTEGER );
	if( s ) DaoArray_SetMatrixSInt16( res, s, n, m );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewMatrixUInt16( DaoProcess *self, unsigned short **s, daoint n, daoint m )
{
	DaoArray *res = DaoArray_New( DAO_INTEGER );
	if( s ) DaoArray_SetMatrixUInt16( res, s, n, m );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewMatrixSInt32( DaoProcess *self, signed int **s, daoint n, daoint m )
{
	DaoArray *res = DaoArray_New( DAO_INTEGER );
	if( s ) DaoArray_SetMatrixSInt32( res, s, n, m );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewMatrixUInt32( DaoProcess *self, unsigned int **s, daoint n, daoint m )
{
	DaoArray *res = DaoArray_New( DAO_INTEGER );
	if( s ) DaoArray_SetMatrixUInt32( res, s, n, m );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewMatrixInt64( DaoProcess *self, long long **s, daoint n, daoint m )
{
	DaoArray *res = DaoArray_New( DAO_INTEGER );
	if( s ) DaoArray_SetMatrixInt64( res, s, n, m );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewMatrixFloat32( DaoProcess *self, float **s, daoint n, daoint m )
{
	DaoArray *res = DaoArray_New( DAO_FLOAT );
	if( s ) DaoArray_SetMatrixFloat32( res, s, n, m );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewMatrixFloat64( DaoProcess *self, double **s, daoint n, daoint m )
{
	DaoArray *res = DaoArray_New( DAO_FLOAT );
	if( s ) DaoArray_SetMatrixFloat64( res, s, n, m );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}
DaoArray* DaoProcess_NewBuffer( DaoProcess *self, void *p, daoint n )
{
	DaoArray *res = DaoArray_New(0);
	DaoArray_SetBuffer( res, p, n );
	DaoProcess_CacheValue( self, (DaoValue*) res );
	return res;
}


DaoArray* DaoProcess_PutVectorSInt8( DaoProcess *self, signed  char *array, daoint N )
{
	DaoArray *res = DaoProcess_PutArray( self );
	DaoArray_SetNumType( res, DAO_INTEGER );
	if( array ) DaoArray_SetVectorSInt8( res, array, N );
	return res;
}
DaoArray* DaoProcess_PutVectorUInt8( DaoProcess *self, unsigned char *array, daoint N )
{
	DaoArray *res = DaoProcess_PutArray( self );
	DaoArray_SetNumType( res, DAO_INTEGER );
	if( array ) DaoArray_SetVectorUInt8( res, array, N );
	return res;
}
DaoArray* DaoProcess_PutVectorSInt16( DaoProcess *self, signed  short *array, daoint N )
{
	DaoArray *res = DaoProcess_PutArray( self );
	DaoArray_SetNumType( res, DAO_INTEGER );
	if( array ) DaoArray_SetVectorSInt16( res, array, N );
	return res;
}
DaoArray* DaoProcess_PutVectorUInt16( DaoProcess *self, unsigned short *array, daoint N )
{
	DaoArray *res = DaoProcess_PutArray( self );
	DaoArray_SetNumType( res, DAO_INTEGER );
	if( array ) DaoArray_SetVectorUInt16( res, array, N );
	return res;
}
DaoArray* DaoProcess_PutVectorSInt32( DaoProcess *self, signed  int *array, daoint N )
{
	DaoArray *res = DaoProcess_PutArray( self );
	DaoArray_SetNumType( res, DAO_INTEGER );
	if( array ) DaoArray_SetVectorSInt32( res, array, N );
	return res;
}
DaoArray* DaoProcess_PutVectorUInt32( DaoProcess *self, unsigned int *array, daoint N )
{
	DaoArray *res = DaoProcess_PutArray( self );
	DaoArray_SetNumType( res, DAO_INTEGER );
	if( array ) DaoArray_SetVectorUInt32( res, array, N );
	return res;
}
DaoArray* DaoProcess_PutVectorInt64( DaoProcess *self, long long *array, daoint N )
{
	DaoArray *res = DaoProcess_PutArray( self );
	DaoArray_SetNumType( res, DAO_INTEGER );
	if( array ) DaoArray_SetVectorInt64( res, array, N );
	return res;
}
DaoArray* DaoProcess_PutVectorFloat32( DaoProcess *self, float *array, daoint N )
{
	DaoArray *res = DaoProcess_PutArray( self );
	DaoArray_SetNumType( res, DAO_FLOAT );
	if( array ) DaoArray_SetVectorFloat32( res, array, N );
	return res;
}
DaoArray* DaoProcess_PutVectorFloat64( DaoProcess *self, double *array, daoint N )
{
	DaoArray *res = DaoProcess_PutArray( self );
	DaoArray_SetNumType( res, DAO_FLOAT );
	if( array ) DaoArray_SetVectorFloat64( res, array, N );
	return res;
}
DaoArray* DaoProcess_PutVectorComplex( DaoProcess *self, dao_complex *array, daoint N )
{
	DaoArray *res = DaoProcess_PutArray( self );
	DaoArray_SetNumType( res, DAO_COMPLEX );
	if( array ) DaoArray_SetVectorFloat64( res, (double*)array, N );
	return res;
}

#else

static DaoArray* DaoValue_NewArray()
{
	printf( "Error: numeric array is disabled!\n" );
	return NULL;
}
DaoArray* DaoProcess_NewVectorB( DaoProcess *self, char *s, daoint n )
{
	return DaoValue_NewArray();
}
DaoArray* DaoProcess_NewVectorUInt8( DaoProcess *self, unsigned char *s, daoint n )
{
	return DaoValue_NewArray();
}
DaoArray* DaoProcess_NewVectorS( DaoProcess *self, short *s, daoint n )
{
	return DaoValue_NewArray();
}
DaoArray* DaoProcess_NewVectorUInt16( DaoProcess *self, unsigned short *s, daoint n )
{
	return DaoValue_NewArray();
}
DaoArray* DaoProcess_NewVectorInt64( DaoProcess *self, long long *s, daoint n )
{
	return DaoValue_NewArray();
}
DaoArray* DaoProcess_NewVectorUInt32( DaoProcess *self, unsigned int *s, daoint n )
{
	return DaoValue_NewArray();
}
DaoArray* DaoProcess_NewVectorFloat32( DaoProcess *self, float *s, daoint n )
{
	return DaoValue_NewArray();
}
DaoArray* DaoProcess_NewVectorFloat64( DaoProcess *self, double *s, daoint n )
{
	return DaoValue_NewArray();
}
DaoArray* DaoProcess_NewMatrixB( DaoProcess *self, signed char **s, daoint n, daoint m )
{
	return DaoValue_NewArray();
}
DaoArray* DaoProcess_NewMatrixUInt8( DaoProcess *self, unsigned char **s, daoint n, daoint m )
{
	return DaoValue_NewArray();
}
DaoArray* DaoProcess_NewMatrixS( DaoProcess *self, short **s, daoint n, daoint m )
{
	return DaoValue_NewArray();
}
DaoArray* DaoProcess_NewMatrixUInt16( DaoProcess *self, unsigned short **s, daoint n, daoint m )
{
	return DaoValue_NewArray();
}
DaoArray* DaoProcess_NewMatrixInt64( DaoProcess *self, long long **s, daoint n, daoint m )
{
	return DaoValue_NewArray();
}
DaoArray* DaoProcess_NewMatrixUInt32( DaoProcess *self, unsigned int **s, daoint n, daoint m )
{
	return DaoValue_NewArray();
}
DaoArray* DaoProcess_NewMatrixFloat32( DaoProcess *self, float **s, daoint n, daoint m )
{
	return DaoValue_NewArray();
}
DaoArray* DaoProcess_NewMatrixFloat64( DaoProcess *self, double **s, daoint n, daoint m )
{
	return DaoValue_NewArray();
}
DaoArray* DaoProcess_NewBuffer( DaoProcess *self, void *s, daoint n )
{
	return DaoValue_NewArray();
}

static DaoArray* NullArray( DaoProcess *self )
{
	DaoProcess_RaiseError( self, NULL, getCtInfo( DAO_DISABLED_NUMARRAY ) );
	return NULL;
}
DaoArray* DaoProcess_PutVectorSInt8( DaoProcess *s, signed  char *v, daoint N ){ return NullArray(s); }
DaoArray* DaoProcess_PutVectorUInt8( DaoProcess *s, unsigned char *v, daoint N ){ return NullArray(s); }
DaoArray* DaoProcess_PutVectorSInt16( DaoProcess *s, signed  short *v, daoint N ){ return NullArray(s); }
DaoArray* DaoProcess_PutVectorUInt16( DaoProcess *s, unsigned short *v, daoint N ){ return NullArray(s); }
DaoArray* DaoProcess_PutVectorSInt32( DaoProcess *s, signed  int *v, daoint N ){ return NullArray(s); }
DaoArray* DaoProcess_PutVectorUInt32( DaoProcess *s, unsigned int *v, daoint N ){ return NullArray(s); }
DaoArray* DaoProcess_PutVectorInt64( DaoProcess *s, long long *v, daoint n ){ return NullArray(s); }
DaoArray* DaoProcess_PutVectorFloat32( DaoProcess *s, float *v, daoint n ){ return NullArray(s); }
DaoArray* DaoProcess_PutVectorFloat64( DaoProcess *s, double *v, daoint n ){ return NullArray(s); }
DaoArray* DaoProcess_PutVectorC( DaoProcess *s, dao_complex *v, daoint n ){ return NullArray(s); }
#endif



DAO_DLL int DaoAux_OnLoad( DaoVmSpace *vmSpace, DaoNamespace *ns )
{
	return 0;
}

