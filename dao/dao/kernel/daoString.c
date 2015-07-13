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

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include<wctype.h>
#include<wchar.h>

#include"daoString.h"
#include"daoThread.h"

#ifdef DAO_WITH_THREAD
DMutex  mutex_string_sharing;
#endif


typedef struct DCounter DCounter;

struct DCounter
{
	daoint  width : 4;             /* char width; */
	daoint  count : DAOINT_BITS-4; /* char count with this width; */
	daoint  chars;                 /* char index for this counter; */
	daoint  bytes;                 /* byte index for this counter; */
};

struct DStringAux
{
	DCounter  *counters;  /* counter list; */
	daoint     visit;     /* index of the last visited counter; */
	daoint     chars;     /* total number of chars in the string; */
	daoint     size;      /* counter number; */
	daoint     cap;       /* buffer size; */
};

static DStringAux* DStringAux_New()
{
	DStringAux *self = (DStringAux*) dao_calloc(1,sizeof(DStringAux));
	return self;
}
static void DStringAux_Delete( DStringAux *self )
{
	if( self->counters ) dao_free( self->counters );
	dao_free( self );
}
static void DStringAux_Update( DStringAux *self, DString *string )
{
	daoint size = 0;
	DCounter dummy = {7, 0, 0, 0};
	DCounter *last = & dummy;
	uchar_t *bytes = (unsigned char*) string->chars;
	daoint i = 0;

	self->size = 0;
	self->chars = 0;
	self->visit = 0;
	while( i < string->size ){
		daoint pos = DString_LocateChar( string, i, 0 );
		int width = pos == DAO_NULLPOS ? 1 : DString_UTF8CharSize( bytes[i] );
		if( width == last->width ){
			last->count += 1;
		}else{
			daoint chars = last->chars + last->count;
			daoint bytes = last->bytes + last->count * last->width;
			if( size == self->cap ){
				self->cap += 1.25 * self->cap + 5;
				self->counters = (DCounter*) dao_realloc( self->counters, self->cap*sizeof(DCounter) );
			}
			last = self->counters + (size++);
			last->width = width;
			last->count = 1;
			last->chars = chars;
			last->bytes = bytes;
		}
		self->chars += 1;
		i += width;
	}
	self->size = size; /* set after done, for thread safety; */

#if 0
	printf( "counters: %i;  chars: %i;  bytes: %i\n", (int) size, self->chars, string->size );
	for(i=0; i<self->size; ++i){
		DCounter *c = self->counters + i;
		printf( "%5i: %2i %5i %5i %5i\n", i, c->width, c->count, c->chars, c->bytes );
	}
#endif
}
static daoint DStringAux_GetByteIndex( DStringAux *self, DString *string, daoint chindex )
{
	uchar_t *bytes = (uchar_t*) string->chars;
	DCounter *start = self->counters;
	DCounter *visit = start + self->visit;
	DCounter *end = start + self->size;

	if( chindex < 0 ) chindex += self->chars;
	if( chindex < 0 || chindex >= self->chars ) return DAO_NULLPOS;
	while( visit >= start && chindex < visit->chars ) visit -= 1;
	while( visit < end && chindex > visit->chars + visit->count ) visit += 1;
	if( visit < start || visit >= end ) return DAO_NULLPOS;
	self->visit = visit - start;
	return visit->bytes + visit->width * (chindex - visit->chars);
}



static int dao_string[4] = {1,0,0,0};

/**/
void DString_Init( DString *self )
{
	self->chars = (char*)(dao_string + 1);
	self->detached = 1;
	self->sharing = 1;
	self->size = 0;
	self->bufSize = 0;
	self->aux = NULL;
}
DString* DString_New()
{
	DString *self = (DString*)dao_calloc( 1, sizeof(DString) );
	DString_Init( self );
	return self;
}
DString* DString_NewChars( const char *mbs )
{
	DString *self = DString_New();
	DString_SetChars( self, mbs );
	return self;
}

void DString_DeleteData( DString *self )
{
	int *data = (int*)self->chars - self->sharing;

	if( self->aux ){
		DStringAux_Delete( self->aux );
		self->aux = NULL;
	}

	if( data == NULL || data == dao_string ) return;

	if( self->sharing ){
#ifdef DAO_WITH_THREAD
		DMutex_Lock( & mutex_string_sharing );
#endif
		if( self->sharing ){
			data[0] -= 1;
			if( data[0] ==0 ) dao_free( data );
		}
#ifdef DAO_WITH_THREAD
		DMutex_Unlock( & mutex_string_sharing );
#endif
	}else{
		dao_free( data );
	}

	self->sharing = 0;
	self->chars = NULL;
}
void DString_Delete( DString *self )
{
	DString_DeleteData( self );
	dao_free( self );
}
void DString_Detach( DString *self, int bufsize )
{
	daoint size;
	int *data2, *data = (int*)self->chars - self->sharing;

	if( self->aux ) self->aux->size = 0;
	if( self->sharing == 0 ) return;
#ifdef DAO_WITH_THREAD
	DMutex_Lock( & mutex_string_sharing );
#endif
	if( data[0] >1 || data == dao_string ){
		if( bufsize < self->size ) bufsize = self->size;
		data[0] -= 1;
		self->bufSize = bufsize + 1;
		data2 = (int*) dao_malloc( (self->bufSize + 1)*sizeof(char) + sizeof(int) );
		data2[0] = 1;
		memcpy( data2+1, data+1, (self->size + 1)*sizeof(char) );
		self->chars = (char*)(data2 + 1);
	}
#ifdef DAO_WITH_THREAD
	DMutex_Unlock( & mutex_string_sharing );
#endif
}
static int* DString_Realloc( DString *self, daoint bufsize )
{
	daoint bsize = (bufsize + 1)*sizeof(char) + self->sharing*sizeof(int);
	int *data, *data2;

	data = data2 = (int*)self->chars - self->sharing;
	if( data == dao_string ) data = NULL;

	data = (int*)dao_realloc( data, bsize );
	self->chars = (char*)(data + self->sharing);
	if( data2 == dao_string ) self->chars[ self->size ] = '\0';
	if( self->sharing && data2 == dao_string ) data[0] = 1;
	return data;
}
void DString_SetSharing( DString *self, int sharing )
{
	int *data = (int*)self->chars - self->sharing;
	if( (self->sharing == 0) == (sharing == 0) ) return;

	if( sharing && data == dao_string ){
		self->sharing = 1;
		return; /* OK for sharing; */
	}

	DString_Detach( self, self->bufSize );
	data = (int*)self->chars - self->sharing;
	self->sharing = sharing != 0;

#ifdef DAO_WITH_THREAD
	DMutex_Lock( & mutex_string_sharing );
#endif
	if( sharing ==0 ){
		memmove( data, self->chars, self->size*sizeof(char) );
		self->bufSize += sizeof(int)/sizeof(char);
		self->chars = (char*) data;
		self->chars[ self->size ] = 0;
	}else{
		if( self->bufSize < self->size + (daoint)(sizeof(int)/sizeof(char)) ){
			size_t size = (self->size + 1)*sizeof(char) + sizeof(int);
			if( data == dao_string ) data = NULL;
			data = (int*) dao_realloc( data, size );
			self->bufSize = self->size;
		}
		self->chars = (char*)(data + 1);
		memmove( self->chars, data, self->size*sizeof(char) );
		self->chars[ self->size ] = 0;
		data[0] = 1;
	}
#ifdef DAO_WITH_THREAD
	DMutex_Unlock( & mutex_string_sharing );
#endif
}

char*  DString_GetData( DString *self )
{
	return self->chars;
}
void DString_SetChars( DString *self, const char *chs )
{
	if( self->chars && self->chars == chs ) return;
	if( chs == NULL ){
		DString_Clear( self );
		return;
	}
	DString_Reset( self, strlen( chs ) );
	memcpy( self->chars, chs, self->size*sizeof(char) );
}
void DString_Reserve( DString *self, daoint size )
{
	int *data = (int*)self->chars - self->sharing;
	daoint bufsize = size >= self->bufSize ? (1.2*size + 4) : self->bufSize;

	DString_Detach( self, bufsize );
	if( size <= self->bufSize ) return;
	self->bufSize = bufsize;
	DString_Realloc( self, self->bufSize );
}
int DString_IsASCII( DString *self );
void DString_ToLower( DString *self )
{
	daoint i;
	char *bytes;
	DArray *wcs;

	DString_Detach( self, self->size );
	if( DString_IsASCII( self ) ){
		for(i=0,bytes=self->chars; i<self->size; i++, bytes++) *bytes = tolower( *bytes );
		return;
	}
	wcs = DArray_New( sizeof(wchar_t) );
	DString_DecodeUTF8( self, wcs );
	self->size = 0;
	for(i=0; i<wcs->size; ++i){
		DString_AppendWChar( self, towlower( wcs->data.wchars[i] ) );
	}
	DArray_Delete( wcs );
}
void DString_ToUpper( DString *self )
{
	daoint i;
	char *bytes;
	DArray *wcs;

	DString_Detach( self, self->size );
	if( DString_IsASCII( self ) ){
		for(i=0,bytes=self->chars; i<self->size; i++, bytes++) *bytes = toupper( *bytes );
		return;
	}
	wcs = DArray_New( sizeof(wchar_t) );
	DString_DecodeUTF8( self, wcs );
	self->size = 0;
	for(i=0; i<wcs->size; ++i){
		DString_AppendWChar( self, towupper( wcs->data.wchars[i] ) );
	}
	DArray_Delete( wcs );
}
daoint DString_Size( DString *self )
{
	return self->size;
}

void DString_Reset( DString *self, daoint size )
{
	if( size <= self->bufSize ){
		DString_Detach( self, self->bufSize );
		self->size = size;
		self->chars[size] = '\0';
		return;
	}
	DString_Resize( self, size );
}
void DString_Resize( DString *self, daoint size )
{
	daoint i;

	DString_Detach( self, size );
	if( size == self->size && size <= self->bufSize && 2*size >= self->bufSize ) return;

	if( size > self->bufSize || 2*size < self->bufSize ){
		self->bufSize = size;
		DString_Realloc( self, self->bufSize );
	}

	if( size > self->size ){
		memset( self->chars + self->size, 0, (size - self->size) * sizeof(char) );
	}
	self->chars[ size ] = 0;
	self->size = size;
}
void DString_Clear( DString *self )
{
	int share = self->sharing;
	if( ((int*)self->chars - self->sharing) == dao_string ) return;
	DString_Detach( self, 0 );
	DString_DeleteData( self );
	DString_Init( self );
	DString_SetSharing( self, share );
}
void DString_Erase( DString *self, daoint start, daoint n )
{
	daoint i, rest;
	if( start >= self->size ) return;
	if( n < 0 ) n = self->size;
	if( n + start > self->size ) n = self->size - start;
	rest = self->size - start - n;
	if( rest ==0 ){
		DString_Resize( self, start );
		return;
	}

	DString_Detach( self, self->size );
	memmove( self->chars + start, self->chars + start + n, rest * sizeof(char) );
	self->chars[start+rest] = 0;
	self->size -= n;

	if( self->size < 0.5*self->bufSize && self->size+5 < self->bufSize ){
		self->bufSize = (daoint)(0.6 * self->bufSize) + 1;
		DString_Realloc( self, self->bufSize );
	}
}
void DString_InsertChars( DString *self, const char* chs, daoint at, daoint rm, daoint cp )
{
	daoint i;
	if( chs == NULL ) return;
	if( cp <= 0 ) cp = strlen( chs );
	if( at > self->size ) at = self->size;
	if( rm < 0 ) rm = self->size;
	if( rm + at > self->size ) rm = self->size - at;
	DString_Detach( self, self->size + cp - rm );
	if( cp < rm ){
		memmove( self->chars + at+cp, self->chars + at+rm, (self->size-rm-at)*sizeof(char) );
		DString_Reserve( self, self->size + cp - rm );
	}else if( cp > rm ){
		DString_Reserve( self, self->size + cp - rm );
		memmove( self->chars + at+cp, self->chars + at+rm, (self->size-rm-at)*sizeof(char) );
	}
	memcpy( self->chars + at, chs, cp * sizeof(char) );
	self->size += cp-rm;
	self->chars[self->size] = 0;
}
void DString_Insert( DString *self, DString *chs, daoint at, daoint rm, daoint cp )
{
	DString_InsertChars( self, chs->chars, at, rm, cp );
}
void DString_InsertChar( DString *self, const char ch, daoint at )
{
	char chs[2];
	chs[0] = ch;
	chs[1] = '\0';
	DString_InsertChars( self, chs, at, 0, 1 );
}
void DString_AppendBytes( DString *self, const char *chs, daoint n )
{
	daoint i;
	DString_Reserve( self, self->size + n );
	memcpy( self->chars + self->size, chs, n * sizeof(char) );
	self->size += n;
	self->chars[ self->size ] = 0;
}
void DString_AppendChars( DString *self, const char *chs )
{
	DString_AppendBytes(self, chs, strlen( chs ));
}
void DString_Append( DString *self, DString *chs )
{
	DString_AppendBytes( self, chs->chars, chs->size );
}

void DString_AppendChar( DString *self, const char ch )
{
	DString_Reserve( self, self->size + 1 );
	self->chars[self->size] = ch;
	self->size += 1;
	self->chars[ self->size ] = 0;
}


void DString_SetBytes( DString *self, const char *bytes, daoint count )
{
	if( count < 0 ){
		DString_SetChars( self, bytes );
	}else{
		DString_Clear( self );
		DString_AppendBytes( self, bytes, count );
	}
}
void DString_Replace( DString *self, DString *chs, daoint start, daoint rm )
{
	DString_Insert( self, chs, start, rm, chs->size );
}
void DString_ReplaceChars( DString *self, const char *chs, daoint start, daoint rm )
{
	DString_InsertChars( self, chs, start, rm, strlen( chs ) );
}
void DString_SubString( DString *self, DString *sub, daoint from, daoint n )
{
	daoint i, size = self->size;
	if( from >= size ){
		DString_Reset( sub, 0 );
		return;
	}
	if( n < 0 || n > size ) n = size;
	if( from+n > size ) n = size-from;
	DString_Reset( sub, n );
	memcpy( sub->chars, self->chars + from, n * sizeof(char) );
}
/* TODO: better string searching algorithm */
static daoint DMBString_Find( DString *self, daoint S, const char *chs, daoint M )
{
	daoint i, j;

	if( M == 0 ) return DAO_NULLPOS;
	if( M+S > self->size ) return DAO_NULLPOS;
	for(i=S; i<self->size-M+1; i++){
		int found = memcmp( self->chars + i, chs, M ) == 0;
		if( found ) return i;
	}
	return DAO_NULLPOS;
}
static daoint DMBString_RFind( DString *self, daoint S, const char* chs, daoint M )
{
	daoint i, j;

	if( S < 0 ) S += self->size;
	if( M == 0 || self->size == 0 ) return DAO_NULLPOS;
	if( S >= self->size ) S = self->size-1;
	if( (S+1) < M || M > self->size ) return DAO_NULLPOS;
	for( i=S; i>=M-1; i--){
		int found = memcmp( self->chars + i - M + 1, chs, M ) == 0;
		if( found ) return i;
	}
	return DAO_NULLPOS;
}
daoint DString_FindChars( DString *self, const char *chs, daoint start )
{
	return DMBString_Find( self, start, chs, strlen( chs ) );
}
daoint DString_RFindChars( DString *self, const char *chs, daoint start )
{
	return DMBString_RFind( self, start, chs, strlen( chs ) );
}
daoint DString_Find( DString *self, DString *chs, daoint start )
{
	return DMBString_Find( self, start, chs->chars, chs->size );
}
daoint DString_RFind( DString *self, DString *chs, daoint start )
{
	return DMBString_RFind( self, start, chs->chars, chs->size );
}
daoint DString_FindChar( DString *self, char ch, daoint start )
{
	daoint i;
	for(i=start; i<self->size; i++ ) if( self->chars[i] == ch ) return i;
	return DAO_NULLPOS;
}
daoint DString_RFindChar( DString *self, char ch, daoint start )
{
	daoint i;
	if( self->size ==0 ) return DAO_NULLPOS;
	if( start < 0 || start >= self->size ) start = self->size - 1;
	for(i=start; i >=0; i-- ) if( self->chars[i] == ch ) return i;
	return DAO_NULLPOS;
}

DString* DString_Copy( DString *self )
{
	DString *str = DString_New();
	DString_Assign( str, self );
	return str;
}
/* Real copying, no implicit sharing here. For thread safty. */
DString* DString_DeepCopy( DString *self )
{
	int share = self->sharing;
	DString *copy = DString_New();
	DString_SetSharing( copy, share );
	DString_Resize( copy, self->size );
	memcpy( copy->chars, self->chars, self->size *sizeof(char) );
	return copy;
}
void DString_Assign( DString *self, DString *chs )
{
	int *data1 = (int*)self->chars - self->sharing;
	int *data2 = (int*)chs->chars - chs->sharing;
	int assigned = 0;
	if( self == chs ) return;
	if( data1 == data2 ) return;

	if( data2 != dao_string ){
#ifdef DAO_WITH_THREAD
		DMutex_Lock( & mutex_string_sharing );
#endif
		if( self->aux ) self->aux->size = 0;
		if( self->sharing && chs->sharing ){
			if( data1 != dao_string ){
				data1[0] -= 1;
				if( data1[0] ==0 ) dao_free( data1 );
			}
			memcpy( self, chs, sizeof(DString) - sizeof(DStringAux*) );
			data2[0] += 1;
			assigned = 1;
		}else if( data1 == NULL && chs->sharing ){
			memcpy( self, chs, sizeof(DString) - sizeof(DStringAux*) );
			data2[0] += 1;
			assigned = 1;
		}
#ifdef DAO_WITH_THREAD
		DMutex_Unlock( & mutex_string_sharing );
#endif

		if( assigned ) return;
	}

	if( self->chars == NULL ){
		self->size = self->bufSize = chs->size;
		self->chars = (char*) dao_malloc( (chs->size + 1)*sizeof(char) );
		memcpy( self->chars, chs->chars, chs->size*sizeof(char) );
		self->chars[ self->size ] = 0;
	}else{
		DString_Resize( self, chs->size );
		memcpy( self->chars, chs->chars, chs->size*sizeof(char) );
	}
}
int DString_Compare( DString *self, DString *chs )
{
	daoint min = self->size > chs->size ? chs->size : self->size;
	int cmp;
	if( self->chars == chs->chars ) return 0;
	cmp = memcmp( self->chars, chs->chars, min );
	if( cmp != 0 || self->size == chs->size ) return cmp;
	return self->size < chs->size ? -1 : 1;
}
int DString_EQ( DString *self, DString *chs )
{
	return (DString_Compare( self, chs )==0);
}
void DString_Add( DString *self, DString *left, DString *right )
{
	DString_Assign( self, left );
	DString_Append( self, right );
}
daoint DString_BalancedChar( DString *self, char ch, char lch, char rch,
		char esc, daoint start, daoint end, int countonly )
{
	daoint size = self->size;
	daoint i, bc = 0, count = 0;
	char *src = self->chars;

	if( ch < 0 || start >= size ) return DAO_NULLPOS;
	if( end > size ) end = size;
	for(i=start; i<end; ++i){
		char c = src[i];
		if( c == esc ){
			i ++;
			continue;
		}
		if( c == ch && bc ==0 ){
			if( countonly )
				count ++;
			else return i;
		}
		if( c == lch ){
			bc ++;
		}else if( c == rch ){
			bc --;
			if( bc <0 ) return DAO_NULLPOS;
		}
	}
	if( countonly ) return count;
	return DAO_NULLPOS;
}



static char empty_bytes[] = "";

DString DString_WrapBytes( const char *bytes, int n )
{
	DString str = { NULL, 0, 0, 0, 0 };
	str.chars = empty_bytes;
	if( bytes == NULL ) return str;
	str.chars = (char*) bytes;
	str.size = str.bufSize = n;
	return str;
}
DString DString_WrapChars( const char *bytes )
{
	return DString_WrapBytes( bytes, bytes ? strlen( bytes ) : 0 );
}

void DString_AppendPathSep( DString *self )
{
	char last = self->size ? self->chars[self->size-1] : 0;
	if( last != '/' && last != '\\' ) DString_AppendChar( self, '/' );
}



/*
// UTF-8 Encoding Information:
// High 4 Bits: Encoding Schemes (0: ASCII; 1: Trailing; 2-7: Leading);
// Low  4 Bits: Encoding Lengths (Char Size: Byte number for a code point);
 */
const char utf8_markers[128] =
{
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, /* 00>>1 - 0F>>1 */
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, /* 10>>1 - 1F>>1 */
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, /* 20>>1 - 2F>>1 */
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, /* 30>>1 - 3F>>1 */
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, /* 40>>1 - 4F>>1 */
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, /* 50>>1 - 5F>>1 */
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, /* 60>>1 - 6F>>1 */
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, /* 70>>1 - 7F>>1 */
	0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, /* 80>>1 - 8F>>1 */
	0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, /* 90>>1 - 9F>>1 */
	0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, /* A0>>1 - AF>>1 */
	0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, /* B0>>1 - BF>>1 */
	0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x22, /* C0>>1 - CF>>1 */
	0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x22, /* D0>>1 - DF>>1 */
	0x33, 0x33, 0x33, 0x33, 0x33, 0x33, 0x33, 0x33, /* E0>>1 - EF>>1 */
	0x44, 0x44, 0x44, 0x44, 0x51, 0x51, 0x61, 0x71  /* F0>>1 - FF>>1 */
};

#define U8CodeType(ch)           (utf8_markers[(ch)>>1]>>4)
#define U8CharSize(ch)           (utf8_markers[(ch)>>1]&0xF)
#define U8TrailCheck(ch)         (((ch) >> 6) == 0x2)
#define U8TrailCheck2(c1,c2)     (((((c1)>>6)<<2)|((c2)>>6)) == 0xA)
#define U8TrailCheck3(c1,c2,c3)  (((((c1)>>6)<<4)|(((c2)>>6)<<2)|((c3)>>6)) == 0x2A)
#define U8TrailGet(ch,shift)     (((uint_t)(ch) & 0x3F) << 6*shift)
#define U8TrailMake(ch,shift)    ((((ch) >> 6*(shift)) & 0x3F) | (0x1 << 7))

int DString_UTF8CharSize( uchar_t ch )
{
	return U8CharSize( ch );
}
DCharState DString_DecodeChar( char *start, char *end )
{
	DCharState state = { 0, 1, 0 };
	uchar_t *chs = (uchar_t*) start;
	uchar_t ch = *chs;

	state.value = ch;
	if( (start + U8CharSize(ch)) > end ) return state;
	switch( U8CodeType( ch ) ){
	case 0 :
		state.type = 1;
		break;
	case 1 :
		break;
	case 2 :
		if( U8TrailCheck( chs[1] ) == 0 ) break;
		state.value = ((uint_t)(ch&0x1F) << 6) | U8TrailGet( chs[1], 0 );
		state.width = state.type = 2;
		break;
	case 3 :
		if( U8TrailCheck2( chs[1], chs[2] ) == 0 ) break;
		state.value = ((uint_t)(ch&0xF) << 12) | U8TrailGet( chs[1], 1 );
		state.value |= U8TrailGet( chs[2], 0 );
		state.width = state.type = 3;
		break;
	case 4 :
		if( U8TrailCheck3( chs[1], chs[2], chs[3] ) == 0 ) break;
		state.value = ((uint_t)(ch&0x7) << 18) | U8TrailGet( chs[1], 2 );
		state.value |= U8TrailGet( chs[2], 1 ) | U8TrailGet( chs[3], 0 );
		state.width = state.type = 4;
		break;
	}
	return state;
}

void DString_AppendWChar( DString *self, size_t ch )
{
	DString_Reserve( self, self->size + 4 );

	if( ch >= 0x2000000 ) ch = 0xFFFD; /* replacement character; */

	if( ch < 0x80 ){  /* 0xxxxxxx */
		self->chars[self->size++] = ch;
	}else if( ch < 0x800 ){  /* 110xxxxx 10xxxxxx */
		self->chars[self->size++] = (char)((ch >> 6) + (0x3 << 6));
		self->chars[self->size++] = U8TrailMake( ch, 0 );
	}else if( ch < 0x10000 ){   /* 1110xxxx 10xxxxxx 10xxxxxx */
		self->chars[self->size++] = (char)((ch >> 12) + (0x7 << 5));
		self->chars[self->size++] = U8TrailMake( ch, 1 );
		self->chars[self->size++] = U8TrailMake( ch, 0 );
	}else if( ch < 0x200000 ){  /* 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx */
		self->chars[self->size++] = (char)((ch >> 18) + (0xF << 4));
		self->chars[self->size++] = U8TrailMake( ch, 2 );
		self->chars[self->size++] = U8TrailMake( ch, 1 );
		self->chars[self->size++] = U8TrailMake( ch, 0 );
	}
	self->chars[self->size] = 0;
}

static daoint DString_LocateCurrentChar( DString *self, daoint start )
{
	uchar_t *bytes = (uchar_t*) self->chars + start;
	daoint k, next, pos = start;

	if( (*bytes >> 7) == 0 ) return start;

	while( pos >= 0 && U8TrailCheck( *bytes ) ) bytes--, pos--;
	if( pos < 0 ) return DAO_NULLPOS;

	k = U8CodeType( *bytes );
	if( k < 2 || k > 4 ) return DAO_NULLPOS; /* not a leading byte; */

	next = pos + U8CharSize( *bytes );
	if( next <= start ) return DAO_NULLPOS; /* too many continuation bytes; */
	if( next > self->size ) return DAO_NULLPOS; /* not enough continuation bytes; */

	bytes = (uchar_t*) self->chars + start + 1;
	for(k=start+1; k<next; ++k, ++bytes) if( U8TrailCheck( *bytes ) == 0 ) return DAO_NULLPOS;
	return pos;
}

daoint DString_LocateChar( DString *self, daoint start, daoint count )
{
	uchar_t *bytes = (uchar_t*) self->chars;
	daoint pos = DString_LocateCurrentChar( self, start );
	while( count < 0 ){  /* backward location: */
		if( pos == DAO_NULLPOS ) return DAO_NULLPOS;
		if( pos == 0 ) return DAO_NULLPOS;
		pos = DString_LocateCurrentChar( self, pos - 1 );
		count += 1;
	}
	while( count > 0 ){  /* forward location: */
		if( pos == DAO_NULLPOS ) return DAO_NULLPOS;
		pos += U8CharSize( bytes[pos] );
		if( pos >= self->size ) return DAO_NULLPOS;
		pos = DString_LocateCurrentChar( self, pos );
		count -= 1;
	}
	return pos;
}
static void DString_UpdateAux( DString *self )
{
	if( self->aux != NULL && self->aux->size != 0 ) return;
#   ifdef DAO_WITH_THREAD
	DMutex_Lock( & mutex_string_sharing );
#   endif
	if( self->aux == NULL ) self->aux = DStringAux_New();
	if( self->aux->size == 0 ) DStringAux_Update( self->aux, self );
#   ifdef DAO_WITH_THREAD
	DMutex_Unlock( & mutex_string_sharing );
#   endif
}
daoint DString_GetByteIndex( DString *self, daoint chindex )
{
	if( self->size == 0 ) return DAO_NULLPOS;
	DString_UpdateAux( self );
	return DStringAux_GetByteIndex( self->aux, self, chindex );
}
daoint DString_GetCharCount( DString *self )
{
	if( self->size == 0 ) return 0;
	DString_UpdateAux( self );
	return self->aux->chars;
}

int DString_IsASCII( DString *self )
{
	daoint i, count = 0;
	uchar_t *bytes = (uchar_t*) self->chars;
	uchar_t *end = (uchar_t*) self->chars + self->size;
	for(; bytes < end; ++bytes){
		if( U8CodeType( *bytes ) != 0 ) return 0;
	}
	return 1;
}

/*
// A string is considered valid if the number of valid UTF-8 bytes
// is greater or equal to 10 times the number of invalid bytes.
*/
int DString_CheckUTF8( DString *self )
{
	uchar_t *chs = (uchar_t*) self->chars;
	uchar_t *end = chs + self->size;
	daoint valid = 0, invalid = 0;

	while( chs < end ){
		uchar_t ch = *chs;
		int len = U8CharSize( ch );
		if( (chs + len) > end ) goto InvalidByte;
		switch( U8CodeType( ch ) ){
		case 0 : break;
		case 1 : goto InvalidByte;
		case 2 : if( U8TrailCheck( chs[1] ) ) break; goto InvalidByte;
		case 3 : if( U8TrailCheck2( chs[1], chs[2] ) ) break; goto InvalidByte;
		case 4 : if( U8TrailCheck3( chs[1], chs[2], chs[3] ) ) break; goto InvalidByte;
		default: goto InvalidByte;
		}
		valid += len > 1; /* Do not count ASCII, as they are the same for all encodings; */
		chs += len;
		continue;
InvalidByte:
		invalid += 1;
		chs += 1;
	}
	return valid >= 10*invalid;
}

void DString_Chop( DString *self, int utf8 )
{
	if( self->size == 0 ) return;
	DString_Detach( self, self->size );

	if( self->size && self->chars[ self->size-1 ] == EOF  ) self->chars[ --self->size ] = 0;
	if( self->size && self->chars[ self->size-1 ] == '\n' ) self->chars[ --self->size ] = 0;
	if( self->size && self->chars[ self->size-1 ] == '\r' ) self->chars[ --self->size ] = 0;

	if( self->size == 0 || utf8 == 0 ) return;
	while( self->size && DString_LocateCurrentChar( self, self->size - 1 ) == DAO_NULLPOS ){
		self->size -= 1;
		self->chars[self->size] = '\0';
	}
}
void DString_Trim( DString *self, int head, int tail, int utf8 )
{
	int i, ch;

	if( self->size == 0 ) return;
	if( head == 0 && tail == 0 ) return;
	DString_Detach( self, self->size );

	if( head ){
		for(i=0; i<self->size; ++i){
			ch = self->chars[i];
			if( ch != EOF && ! isspace( ch ) ) break;
		}
		DString_Erase( self, 0, i );
		if( utf8 ){
			for(i=0; i<self->size; ++i){
				if( DString_LocateCurrentChar( self, i ) == i ) break;
			}
			if( i ) DString_Erase( self, 0, i );
		}
	}
	if( tail ){
		while( self->size > 0 ){
			ch = self->chars[ self->size-1 ];
			if( ch != EOF && ! isspace( ch ) ) break;
			self->chars[--self->size] = 0;
		}
		if( utf8 == 0 ) return;
		while( self->size && DString_LocateCurrentChar( self, self->size-1 ) == DAO_NULLPOS ){
			self->chars[--self->size] = 0;
		}
	}
}

int DString_CompareUTF8( DString *self, DString *chs )
{
	char *chs1 = self->chars;
	char *chs2 = chs->chars;
	daoint i1, c1 = 0, n1 = self->size;
	daoint i2, c2 = 0, n2 = chs->size;
	DCharState state1 = { 1, 1, 0 };
	DCharState state2 = { 1, 1, 0 };

	if( self->chars == chs->chars ) return 0;
	for(i1=0,i2=0; i1<n1 && i2<n2; c1+=1, c2+=1, i1+=state1.width, i2+=state2.width){
		state1 = DString_DecodeChar( chs1 + i1, chs1 + n1 );
		state2 = DString_DecodeChar( chs2 + i2, chs2 + n2 );
		if( state1.value < state2.value ){
			return -1;
		}else if( state1.value > state2.value ){
			return 1;
		}
	}
	for(; i1<n1; c1+=1, i1+=state1.width) state1 = DString_DecodeChar( chs1 + i1, chs1 + n1 );
	for(; i2<n2; c2+=2, i2+=state2.width) state2 = DString_DecodeChar( chs2 + i2, chs2 + n2 );
	if( c1 < c2 ){
		return -1;
	}else if( c1 > c2 ){
		return 1;
	}
	return 0;
}


int DString_DecodeUTF8( DString *self, DArray *wcs )
{
	uint_t *wch;
	wchar_t *wch1, *wch2;
	char *chs = self->chars;
	char *end = chs + self->size;
	int ret = 1;

	if( wcs->stride != sizeof(wchar_t) && wcs->stride != 4 ) return 0;

	DArray_Reserve( wcs, self->size + 1 );
	while( chs < end ){
		DCharState state = DString_DecodeChar( chs, end );
		if( state.type == 0 ) goto InvalidByte;
		chs += state.width;
        if( wcs->stride == 4 ){ /* UTF-32 */
			wch = (uint_t*) DArray_Push( wcs );
			*wch = state.value;
        }else if( state.value <= 0xFFFF ){ /* UTF-16, BMP */
			wch1 = (wchar_t*) DArray_Push( wcs );
			*wch1 = state.value;
		}else{ /* UTF-16, surrogates */
			state.value -= 0x10000;
			wch1 = (wchar_t*) DArray_Push( wcs );
			wch2 = (wchar_t*) DArray_Push( wcs );
			*wch1 = (state.value >> 10) + 0xD800;
			*wch2 = (state.value & 0x3FF) + 0xDC00;
		}
		continue;
InvalidByte:
		if( wcs->stride == 4 ){ /* UTF-32 */
			wch = (uint_t*) DArray_Push( wcs );
			*wch = 0xFFFD; /* replacement character; */
		}else{
			wch1 = (wchar_t*) DArray_Push( wcs );
			*wch1 = 0xFFFD; /* replacement character; */
		}
		chs += 1;
		ret = 0;
	}
	if( wcs->stride == 4 ){ /* UTF-32 */
		wcs->data.uints[ wcs->size ] = 0;
	}else{
		wcs->data.wchars[ wcs->size ] = 0;
	}
	return ret;
}

static void DMBString_AppendWCS( DString *self, const wchar_t *chs, daoint len );
static void DWCString_AppendMBS( DArray *self, const char *chs, daoint len );

int DString_ImportUTF8( DString *self, DString *utf8 )
{
	DArray *wcs = DArray_New( sizeof(wchar_t) );
	int ret = DString_DecodeUTF8( utf8, wcs );
	DMBString_AppendWCS( self, (const wchar_t*) wcs->data.base, wcs->size );
	DArray_Delete( wcs );
	return ret;
}
int DString_ExportUTF8( DString *self, DString *utf8 )
{
	wchar_t *wchs;
	daoint i;
	DArray *wcs = DArray_New( sizeof(wchar_t) );
	DWCString_AppendMBS( wcs, self->chars, self->size );
	DString_Reserve( utf8, utf8->size + 3*wcs->size );
	wchs = wcs->data.wchars;
	for(i=0; i<wcs->size; ++i, ++wchs) DString_AppendWChar( utf8, *wchs );
	DArray_Delete( wcs );
	return 1;
}
int DString_ToLocal( DString *self )
{
	DArray *wcs = DArray_New( sizeof(wchar_t) );
	int ret = DString_DecodeUTF8( self, wcs );
	DString_Reset( self, 0 );
	DMBString_AppendWCS( self, wcs->data.wchars, wcs->size );
	DArray_Delete( wcs );
	return ret;
}
int DString_ToUTF8( DString *self )
{
	wchar_t *wchs;
	daoint i;
	DArray *wcs = DArray_New( sizeof(wchar_t) );
	DWCString_AppendMBS( wcs, self->chars, self->size );
	DString_Reset( self, 0 );
	DString_Reserve( self, 3*wcs->size );
	wchs = wcs->data.wchars;
	for(i=0; i<wcs->size; ++i, ++wchs) DString_AppendWChar( self, *wchs );
	DArray_Delete( wcs );
	return 1;
}

#define MAX_CHAR_PER_WCHAR 4
static void DMBString_AppendWCS( DString *self, const wchar_t *chs, daoint len )
{
	daoint i, smin, more = 2;
	const wchar_t *end = chs + len;
	mbstate_t state;

	for(i=0; i<len; ++i) more += chs[i] < 128 ? 1 : MAX_CHAR_PER_WCHAR;

	DString_Reserve( self, self->size + more );
	if( len == 0 || chs == NULL ) return;
	while( chs < end ){
		const wchar_t *next = chs;
		char *dst = self->chars + self->size;
		if( *chs == L'\0' ){
			DString_AppendChar( self, '\0' );
			chs += 1;
			continue;
		}
		while( next < end && *next != L'\0' ) next += 1;
		memset( & state, 0, sizeof(mbstate_t) );
		smin = wcsrtombs( dst, (const wchar_t**)& chs, self->bufSize - self->size, & state );
		if( smin > 0 ){
#ifdef _WIN32
			/* On Windows, wcsrtombs() returns the number converted char from the source! */
			self->size += strlen( dst );
#else
			self->size += smin;
#endif
		}else if( chs != NULL ){
			chs += 1;
		}
		if( chs == NULL ) chs = next;
	}
	self->chars[ self->size ] = 0;
	DString_Reset( self, self->size );
}
static void DWCString_AppendMBS( DArray *self, const char *chs, daoint len )
{
	const char *end = chs + len;
	mbstate_t state;
	daoint smin;

	DArray_Reset( self, 0 );
	if( self->stride < sizeof(wchar_t) ) return;
	if( len == 0 || chs == NULL ) return;

	DArray_Reserve( self, len + 2 );
	while( chs < end ){
		const char *next = chs;
		wchar_t *dst = self->data.wchars + self->size;
		if( *chs == '\0' ){
			wchar_t *wch = (wchar_t*) DArray_Push( self );
			*wch = L'\0';
			chs += 1;
			continue;
		}
		while( next < end && *next != '\0' ) next += 1;
		memset( & state, 0, sizeof(mbstate_t) );
		smin = mbsrtowcs( dst, (const char**)& chs, self->capacity - self->size, & state );
		if( smin > 0 ){
#ifdef _WIN32
			/* On Windows, mbsrtowcs() returns the number converted char from the source! */
			self->size += wcslen( dst );
#else
			self->size += smin;
#endif
		}else if( chs != NULL ){
			chs += 1;
		}
		if( chs == NULL ) chs = next;
	}
	memset( self->data.wchars + self->size, 0, sizeof(wchar_t) );
}
