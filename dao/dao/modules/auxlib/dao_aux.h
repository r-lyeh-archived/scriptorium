/*
// Dao Standard Modules
// http://www.daovm.net
//
// Copyright (c) 2011-2014, Limin Fu
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

#include"dao.h"

DAO_DLL void DaoArray_SetVectorInt64( DaoArray *self, long long* vec, daoint N );
DAO_DLL void DaoArray_SetVectorFloat32( DaoArray *self, float* vec, daoint N );
DAO_DLL void DaoArray_SetVectorFloat64( DaoArray *self, double* vec, daoint N );
DAO_DLL void DaoArray_SetVectorSInt8( DaoArray *self, signed char* vec, daoint N );
DAO_DLL void DaoArray_SetVectorUInt8( DaoArray *self, unsigned char* vec, daoint N );
DAO_DLL void DaoArray_SetVectorSInt16( DaoArray *self, signed short* vec, daoint N );
DAO_DLL void DaoArray_SetVectorUInt16( DaoArray *self, unsigned short* vec, daoint N );
DAO_DLL void DaoArray_SetVectorSInt32( DaoArray *self, signed int* vec, daoint N );
DAO_DLL void DaoArray_SetVectorUInt32( DaoArray *self, unsigned int* vec, daoint N );
DAO_DLL void DaoArray_SetMatrixInt64( DaoArray *self, long long **mat, daoint row, daoint col );
DAO_DLL void DaoArray_SetMatrixFloat32( DaoArray *self, float **mat, daoint row, daoint col );
DAO_DLL void DaoArray_SetMatrixFloat64( DaoArray *self, double **mat, daoint row, daoint col );
DAO_DLL void DaoArray_SetMatrixSInt8( DaoArray *self, signed char **mat, daoint row, daoint col );
DAO_DLL void DaoArray_SetMatrixUInt8( DaoArray *self, unsigned char **mat, daoint row, daoint col );
DAO_DLL void DaoArray_SetMatrixSInt16( DaoArray *self, signed short **mat, daoint row, daoint col );
DAO_DLL void DaoArray_SetMatrixUInt16( DaoArray *self, unsigned short **mat, daoint row, daoint col );
DAO_DLL void DaoArray_SetMatrixSInt32( DaoArray *self, signed int **mat, daoint row, daoint col );
DAO_DLL void DaoArray_SetMatrixUInt32( DaoArray *self, unsigned int **mat, daoint row, daoint col );


DAO_DLL DaoArray*  DaoProcess_PutVectorSInt8( DaoProcess *self, signed   char *array, daoint N );
DAO_DLL DaoArray*  DaoProcess_PutVectorUInt8( DaoProcess *self, unsigned char *array, daoint N );
DAO_DLL DaoArray*  DaoProcess_PutVectorSInt16( DaoProcess *self, signed   short *array, daoint N );
DAO_DLL DaoArray*  DaoProcess_PutVectorUInt16( DaoProcess *self, unsigned short *array, daoint N );
DAO_DLL DaoArray*  DaoProcess_PutVectorSInt32( DaoProcess *self, signed   int *array, daoint N );
DAO_DLL DaoArray*  DaoProcess_PutVectorUInt32( DaoProcess *self, unsigned int *array, daoint N );
DAO_DLL DaoArray*  DaoProcess_PutVectorInt64( DaoProcess *self, long long *array, daoint N );
DAO_DLL DaoArray*  DaoProcess_PutVectorFloat32( DaoProcess *self, float  *array, daoint N );
DAO_DLL DaoArray*  DaoProcess_PutVectorFloat64( DaoProcess *self, double *array, daoint N );
DAO_DLL DaoArray*  DaoProcess_PutVectorComplex( DaoProcess *self, dao_complex *array, daoint N );


/*
// DaoProcess_NewVectorSInt8() creates an integer vector from an array of signed byte;
// DaoProcess_NewVectorUInt8() creates an integer vector from an array of unsigned byte;
// DaoProcess_NewVectorSInt16() creates an integer vector from an array of signed short;
// DaoProcess_NewVectorUInt16() creates an integer vector from an array of unsigned short;
// DaoProcess_NewVectorSInt32() creates an integer vector from an array of signed int;
// DaoProcess_NewVectorUInt32() creates an integer vector from an array of unsigned int;
// DaoProcess_NewVectorInt64()  creates an integer vector from an array of long long;
// DaoProcess_NewVectorFloat32()  creates an float vector from an array of float;
// DaoProcess_NewVectorFloat64()  creates an double vector from an array of double;
//
// If "n" is not zero, the created array will allocate a new buffer, and copy
// the data from the C array passed as parameter to the new buffer; otherwise,
// the created array will directly use the C array as buffer.
//
// In the case that the C array is directly used, one can call reshape() to set
// the array to proper shape before using. The C array must be ensured to be valid
// throughout the use of the created array; and its deallocation must be handled by
// the owner of the C array. A typical scenario of using array in this way is to call
// a Dao function from C, and pass a C array to the Dao function.
*/
DAO_DLL DaoArray* DaoProcess_NewVectorSInt8( DaoProcess *self, signed   char *s, daoint n );
DAO_DLL DaoArray* DaoProcess_NewVectorUInt8( DaoProcess *self, unsigned char *s, daoint n );
DAO_DLL DaoArray* DaoProcess_NewVectorSInt16( DaoProcess *self, signed   short *s, daoint n );
DAO_DLL DaoArray* DaoProcess_NewVectorUInt16( DaoProcess *self, unsigned short *s, daoint n );
DAO_DLL DaoArray* DaoProcess_NewVectorSInt32( DaoProcess *self, signed   int *s, daoint n );
DAO_DLL DaoArray* DaoProcess_NewVectorUInt32( DaoProcess *self, unsigned int *s, daoint n );
DAO_DLL DaoArray* DaoProcess_NewVectorInt64( DaoProcess *self, long long *s, daoint n );
DAO_DLL DaoArray* DaoProcess_NewVectorFloat32( DaoProcess *self, float  *s, daoint n );
DAO_DLL DaoArray* DaoProcess_NewVectorFloat64( DaoProcess *self, double *s, daoint n );

/*
// DaoProcess_NewMatrixSInt8() creates an integer matrix from a [n x m] matrix of signed byte;
// DaoProcess_NewMatrixUInt8() creates an integer matrix from a [n x m] matrix of unsigned byte;
// DaoProcess_NewMatrixSInt16() creates an integer matrix from a [n x m] matrix of signed short;
// DaoProcess_NewMatrixUInt16() creates an integer matrix from a [n x m] matrix of unsigned short;
// DaoProcess_NewMatrixSInt32() creates an integer matrix from a [n x m] matrix of signed int;
// DaoProcess_NewMatrixUInt32() creates an integer matrix from a [n x m] matrix of unsigned int;
// DaoProcess_NewMatrixInt64() creates an integer matrix from a [n x m] matrix of long long;
// DaoProcess_NewMatrixFloat32() creates an float matrix from a [n x m] matrix of float;
// DaoProcess_NewMatrixFloat64() creates an double matrix from a [n x m] matrix of double;
*/
DAO_DLL DaoArray* DaoProcess_NewMatrixSInt8( DaoProcess *self, signed   char **s, daoint n, daoint m );
DAO_DLL DaoArray* DaoProcess_NewMatrixUInt8( DaoProcess *self, unsigned char **s, daoint n, daoint m );
DAO_DLL DaoArray* DaoProcess_NewMatrixSInt16( DaoProcess *self, signed   short **s, daoint n, daoint m );
DAO_DLL DaoArray* DaoProcess_NewMatrixUInt16( DaoProcess *self, unsigned short **s, daoint n, daoint m );
DAO_DLL DaoArray* DaoProcess_NewMatrixSInt32( DaoProcess *self, signed   int **s, daoint n, daoint m );
DAO_DLL DaoArray* DaoProcess_NewMatrixUInt32( DaoProcess *self, unsigned int **s, daoint n, daoint m );
DAO_DLL DaoArray* DaoProcess_NewMatrixInt64( DaoProcess *self, long long **s, daoint n, daoint m );
DAO_DLL DaoArray* DaoProcess_NewMatrixFloat32( DaoProcess *self, float  **s, daoint n, daoint m );
DAO_DLL DaoArray* DaoProcess_NewMatrixFloat64( DaoProcess *self, double **s, daoint n, daoint m );
