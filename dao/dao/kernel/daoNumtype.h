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

#ifndef DAO_NUMERIC_H
#define DAO_NUMERIC_H

#include"daoStdtype.h"


#define COM_ASSN( self, com ) \
{ (self).real = (com).real; (self).imag = (com).imag; }

#define COM_ASSN2( self, com ) \
{ (self)->real = (com).real; (self)->imag = (com).imag; }

#define COM_IP_ADD( self, com ) \
{ (self).real += (com).real; (self).imag += (com).imag; }

#define COM_IP_SUB( self, com ) \
{ (self).real -= com.real; (self).imag -= com.imag; }

#define COM_IP_MUL( self, com ) \
{ (self).real *= com.real; (self).imag *= com.imag; }

#define COM_IP_DIV( self, com ) \
{ (self).real /= com.real; (self).imag /= com.imag; }

#define COM_ADD( self, left, right ) \
{ (self).real = left.real + right.real; (self).imag = left.imag + right.imag; }

#define COM_SUB( self, left, right ) \
{ (self).real = left.real - right.real; (self).imag = left.imag - right.imag; }

#define COM_MUL( self, left, right ) \
{ (self).real = left.real*right.real - left.imag*right.imag; \
	(self).imag = left.real*right.imag + left.imag*right.real; }

#define COM_DIV( self, L, R ) \
{ (self).real = ( L.real*R.real + L.imag*R.imag ) / ( R.real*R.real + R.imag*R.imag ); \
	(self).imag = ( L.imag*R.real - L.real*R.imag ) / ( R.real*R.real + R.imag*R.imag ); }

#define COM_MINUS( self, com ) \
{ (self).real = - com.real; (self).imag = - com.imag;  }

DAO_DLL double abs_c( const dao_complex com );
DAO_DLL double arg_c( const dao_complex com );
DAO_DLL double norm_c( const dao_complex com );
DAO_DLL dao_complex cos_c( const dao_complex com );
DAO_DLL dao_complex cosh_c( const dao_complex com );
DAO_DLL dao_complex exp_c( const dao_complex com );
DAO_DLL dao_complex log_c( const dao_complex com );
DAO_DLL dao_complex sin_c( const dao_complex com );
DAO_DLL dao_complex sinh_c( const dao_complex com );
DAO_DLL dao_complex sqrt_c( const dao_complex com );
DAO_DLL dao_complex tan_c( const dao_complex com );
DAO_DLL dao_complex tanh_c( const dao_complex com );
DAO_DLL dao_complex ceil_c( const dao_complex com );
DAO_DLL dao_complex floor_c( const dao_complex com );


typedef union DaoArrayData DaoArrayData;

union DaoArrayData
{
	void         *p;
	dao_boolean  *b;
	dao_integer  *i;
	dao_float    *f;
	dao_complex  *c;
};


/* Multi-dimensional array stored in row major order: */
struct DaoArray
{
	DAO_VALUE_COMMON;

	uchar_t  etype; /* element type; */
	uchar_t  owner; /* own the data; */
	short    ndim;  /* number of dimensions; */
	daoint   size;  /* total number of elements; */
	daoint  *dims;  /* 2*ndim values; the first ndim values: size for each dimension; */
	/* the second ndim values: product of the sizes of the remaining dimensions; */

	DaoArrayData data;

	DaoArray *original; /* original array for an array slicing; */
	DArray   *slices;
	/*
	// ::slices structure:
	// The first 2*ndim values: slice in each dimension;
	// The remaining values: slice information in the raw 1D value array;
	//
	//   Start_D1, Count_D1; # Slice in dimension-1;
	//   Start_D2, Count_D2; # Slice in dimension-2;
	//   ...
	//   Start_DN, Count_DN; # Slice in dimension-N;
	//   Slice_Count;        # Number of slices in the raw 1D array;
	//   Slice_Step;         # Step (distance) between two slices;
	//   Slice_Start;        # Starting index of the first slice;
	//   Slice_Length;       # Length of single slice (the same as Count_DN);
	*/
};

#ifdef DAO_WITH_NUMARRAY

DAO_DLL DaoArray* DaoArray_New( int type );
DAO_DLL DaoArray* DaoArray_Copy( DaoArray *self );
DAO_DLL DaoArray* DaoArray_CopyX( DaoArray *self, DaoType *tp );
DAO_DLL int DaoArray_CopyArray( DaoArray *self, DaoArray *other );
DAO_DLL void DaoArray_Delete( DaoArray *self );

DAO_DLL void DaoArray_SetDimCount( DaoArray *self, int D );
DAO_DLL void DaoArray_FinalizeDimData( DaoArray *self );

DAO_DLL void DaoArray_ResizeData( DaoArray *self, daoint size, daoint oldSize );
DAO_DLL void DaoArray_ResizeVector( DaoArray *self, daoint size );
DAO_DLL void DaoArray_ResizeArray( DaoArray *self, daoint *dims, int D );

DAO_DLL int DaoArray_Sliced( DaoArray *self );
DAO_DLL void DaoArray_UseData( DaoArray *self, void *data );

DAO_DLL dao_boolean DaoArray_GetBoolean( DaoArray *self, daoint i );
DAO_DLL dao_integer DaoArray_GetInteger( DaoArray *na, daoint i );
DAO_DLL dao_float   DaoArray_GetFloat( DaoArray *na, daoint i );
DAO_DLL dao_complex DaoArray_GetComplex( DaoArray *na, daoint i );
DAO_DLL DaoValue* DaoArray_GetValue( DaoArray *self, daoint i, DaoValue *res );

DAO_DLL DaoArray* DaoArray_GetWorkArray( DaoArray *self );
DAO_DLL daoint DaoArray_GetWorkSize( DaoArray *self );
DAO_DLL daoint DaoArray_GetWorkStep( DaoArray *self );
DAO_DLL daoint DaoArray_GetWorkStart( DaoArray *self );
DAO_DLL daoint DaoArray_GetWorkIntervalSize( DaoArray *self );

#endif

#endif
