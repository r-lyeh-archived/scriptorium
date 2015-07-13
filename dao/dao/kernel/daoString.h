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

#ifndef DAO_STRING_H
#define DAO_STRING_H

#include<limits.h>
#include"daoBase.h"

#define DAO_NULLPOS ((daoint)(-1))
#define DAOINT_BITS CHAR_BIT*sizeof(daoint)

typedef struct DCharState DCharState;
typedef struct DStringAux DStringAux;

struct DCharState
{
	uchar_t  type;   /* char type: 0, invalid; 1, ASCII; 2-4, UTF-8; */
	uchar_t  width;  /* number of bytes in the sequence (1-4); */
	uint_t   value;  /* unicode codepoint or byte value; */
};


struct DString
{
	char        *chars;
	daoint       size     : DAOINT_BITS-1;
	size_t       detached : 1;
	daoint       bufSize  : DAOINT_BITS-1;
	size_t       sharing  : 1;
	DStringAux  *aux;
};

DAO_DLL DString* DString_New();
DAO_DLL void DString_Init( DString *self );
DAO_DLL void DString_DeleteData( DString *self );
DAO_DLL void DString_Delete( DString *self );
DAO_DLL void DString_Detach( DString *self, int bufsize );

DAO_DLL void DString_SetSharing( DString *self, int sharing );

DAO_DLL char* DString_GetData( DString *self );
DAO_DLL void DString_SetChars( DString *self, const char *chs );
DAO_DLL void DString_SetBytes( DString *self, const char *bytes, daoint count );

DAO_DLL void DString_ToLower( DString *self );
DAO_DLL void DString_ToUpper( DString *self );

DAO_DLL daoint DString_Size( DString *self );
DAO_DLL void DString_Reset( DString *self, daoint size );
DAO_DLL void DString_Resize( DString *self, daoint size );
DAO_DLL void DString_Reserve( DString *self, daoint size );
DAO_DLL void DString_Clear( DString *self );

DAO_DLL void DString_Erase( DString *self, daoint start, daoint n );
DAO_DLL void DString_Insert( DString *self, DString *chs, daoint at, daoint rm, daoint cp );
DAO_DLL void DString_InsertChars( DString *self, const char *chs, daoint at, daoint rm, daoint cp );
DAO_DLL void DString_InsertChar( DString *self, const char ch, daoint at );
DAO_DLL void DString_Append( DString *self, DString *chs );
DAO_DLL void DString_AppendChar( DString *self, const char ch );
DAO_DLL void DString_AppendChars( DString *self, const char *chs );
DAO_DLL void DString_AppendBytes( DString *self, const char *chs, daoint n );

DAO_DLL void DString_Replace( DString *self, DString *chs, daoint start, daoint rm );
DAO_DLL void DString_ReplaceChars( DString *self, const char *chs, daoint start, daoint rm );
DAO_DLL void DString_SubString( DString *self, DString *sub, daoint from, daoint n );

DAO_DLL daoint DString_Find( DString *self, DString *chs, daoint start );
DAO_DLL daoint DString_RFind( DString *self, DString *chs, daoint start );
DAO_DLL daoint DString_FindChars( DString *self, const char *ch, daoint start );
DAO_DLL daoint DString_RFindChars( DString *self, const char *ch, daoint start );
DAO_DLL daoint DString_FindChar( DString *self, char ch, daoint start );
DAO_DLL daoint DString_RFindChar( DString *self, char ch, daoint start );

DAO_DLL int DString_Match( DString *self, const char *pat, daoint *start, daoint *end );
DAO_DLL int DString_Change( DString *self, const char *pat, const char *target, int index );

DAO_DLL DString* DString_Copy( DString *self );
DAO_DLL DString* DString_DeepCopy( DString *self );
DAO_DLL void DString_Assign( DString *left, DString *right );
DAO_DLL int  DString_Compare( DString *left, DString *right );
DAO_DLL int  DString_EQ( DString *left, DString *right );

DAO_DLL void DString_Add( DString *self, DString *left, DString *right );

DAO_DLL daoint DString_BalancedChar( DString *self, char ch, char lch, char rch,
		char esc, daoint start, daoint end, int countonly );

DAO_DLL DString DString_WrapBytes( const char *mbs, int n );
DAO_DLL DString DString_WrapChars( const char *mbs );


DAO_DLL void DString_AppendPathSep( DString *self );

DAO_DLL DCharState DString_DecodeChar( char *start, char *end );
DAO_DLL daoint DString_LocateChar( DString *self, daoint start, daoint count );
DAO_DLL daoint DString_GetByteIndex( DString *self, daoint chindex );
DAO_DLL daoint DString_GetCharCount( DString *self );
DAO_DLL void DString_AppendWChar( DString *self, size_t ch );
DAO_DLL void DString_Chop( DString *self, int utf8 );
DAO_DLL void DString_Trim( DString *self, int head, int tail, int utf8 );
DAO_DLL int DString_UTF8CharSize( uchar_t ch );
DAO_DLL int DString_CheckUTF8( DString *self );
DAO_DLL int DString_DecodeUTF8( DString *self, DArray *codepoints );
DAO_DLL int DString_ImportUTF8( DString *self, DString *utf8 );
DAO_DLL int DString_ExportUTF8( DString *self, DString *utf8 );
DAO_DLL int DString_ToLocal( DString *self );
DAO_DLL int DString_ToUTF8( DString *self );
DAO_DLL int DString_CompareUTF8( DString *left, DString *right );


#endif
