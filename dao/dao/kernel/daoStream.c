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

#include"ctype.h"
#include"string.h"
#include"daoStream.h"
#include"daoVmspace.h"
#include"daoRoutine.h"
#include"daoProcess.h"
#include"daoNumtype.h"
#include"daoNamespace.h"
#include"daoValue.h"
#include"daoGC.h"

#define IO_BUF_SIZE  4096

void DaoStream_Flush( DaoStream *self )
{
	if( self->redirect && self->redirect->StdioFlush ){
		self->redirect->StdioFlush( self->redirect );
	}else if( self->file ){
		fflush( self->file );
	}else{
		fflush( stdout );
	}
}
static int DaoIO_CheckMode( DaoStream *self, DaoProcess *proc, int what )
{
	if( DaoStream_IsOpen( self ) == 0 ){
		DaoProcess_RaiseError( proc, NULL, "stream is not open!" );
		return 0;
	}
	if( DaoStream_EndOfStream( self ) == 1 ){
		DaoProcess_RaiseError( proc, NULL, "stream reached the end!" );
		return 0;
	}
	if( what == DAO_STREAM_READABLE && DaoStream_IsReadable( self ) == 0 ){
		DaoProcess_RaiseError( proc, NULL, "stream is not readable!" );
		return 0;
	}
	if( what == DAO_STREAM_WRITABLE && DaoStream_IsWritable( self ) == 0 ){
		DaoProcess_RaiseError( proc, NULL, "stream is not writable!" );
		return 0;
	}
	return 1;
}
static void DaoIO_Write0( DaoStream *self, DaoProcess *proc, DaoValue *p[], int N )
{
	DMap *cyclic = NULL;
	int i;

	for(i=0; i<N; i++){
		if( p[i]->type > DAO_ARRAY ){
			cyclic = DHash_New(0,0);
			break;
		}
	}
	for(i=0; i<N; i++){
		if( p[i]->type > DAO_ARRAY ) DMap_Reset( cyclic );
		DaoValue_Print( p[i], proc, self, cyclic );
	}
	if( cyclic ) DMap_Delete( cyclic );
}
static void DaoIO_Write( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoStream *self = & p[0]->xStream;
	if( DaoIO_CheckMode( self, proc, DAO_STREAM_WRITABLE ) == 0 ) return;
	DaoIO_Write0( self, proc, p+1, N-1 );
}
static void DaoIO_Write2( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoStream *stream = proc->stdioStream;
	if( stream == NULL ) stream = proc->vmSpace->stdioStream;
	if( DaoIO_CheckMode( stream, proc, DAO_STREAM_WRITABLE ) == 0 ) return;
	DaoIO_Write0( stream, proc, p, N );
}
static void DaoIO_Writeln0( DaoStream *self, DaoProcess *proc, DaoValue *p[], int N )
{
	DaoValue *params[DAO_MAX_PARAM];
	DMap *cyclic = NULL;
	int i;
	if( DaoIO_CheckMode( self, proc, DAO_STREAM_WRITABLE ) == 0 ) return;
	for(i=0; i<N; i++){
		if( p[i]->type > DAO_ARRAY ){
			cyclic = DHash_New(0,0);
			break;
		}
	}
	/*
	// DaoValue_Print() may call user defined function and change the stack
	// and invalidate the parameter array:
	*/
	memmove( params, p, N*sizeof(DaoValue*) );
	for(i=0; i<N; i++){
		if( params[i]->type > DAO_ARRAY ) DMap_Reset( cyclic );
		DaoValue_Print( params[i], proc, self, cyclic );
		if( i+1<N ) DaoStream_WriteChars( self, " ");
	}
	DaoStream_WriteChars( self, "\n");
	if( cyclic ) DMap_Delete( cyclic );
}
static void DaoIO_Writeln( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoStream *self = & p[0]->xStream;
	if( DaoIO_CheckMode( self, proc, DAO_STREAM_WRITABLE ) == 0 ) return;
	DaoIO_Writeln0( self, proc, p+1, N-1 );
}
static void DaoIO_Writeln2( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoStream *stream = proc->stdioStream;
	if( stream == NULL ) stream = proc->vmSpace->stdioStream;
	if( DaoIO_CheckMode( stream, proc, DAO_STREAM_WRITABLE ) == 0 ) return;
	DaoIO_Writeln0( stream, proc, p, N );
}
/*
// C printf format: %[parameter][flags][width][.precision][length]type
//
// Dao writef format: %[flags][width][.precision]type[color]
//
// Where 'flags', 'width' and 'precision' will conform to the C format,
// but 'type' can only be:
//   d, i, o, u, x/X : for integer;
//   e/E, f/F, g/G : for float and double;
//   c/C : for character, C for local encoding;
//   s/S : for string, S for local encoding;
//   p : for any type, write address;
//   a : automatic, for any type, write in the default format;
// Namely the standard ones except 'n', and plus 'a'.
//
// Optional 'color' format will be in form of: [foreground:background], [foreground]
// or [:background]. The supported color name format will depend on the color printing
// handle. Mininum requirement is the support of the following 8 color names:
// black, white, red, green, blue, yellow, magenta, cyan.
*/
static void DaoIO_Writef0( DaoStream *self, DaoProcess *proc, DaoValue *p[], int N )
{
	DaoValue *value;
	DString *fmt2;
	DString *fgcolor = NULL;
	DString *bgcolor = NULL;
	DMap *cyclic = NULL;
	const char *convs = "asSpcCdiouxXfFeEgG";
	char F, *s, *end, *fg, *bg, *fmt, message[100];
	int i, k, id = 0;

	if( DaoIO_CheckMode( self, proc, DAO_STREAM_WRITABLE ) == 0 ) return;

	fmt2 = DString_New();
	for(i=0; i<N; i++){
		if( p[i]->type > DAO_ARRAY ){
			cyclic = DHash_New(0,0);
			break;
		}
	}

	s = p[0]->xString.value->chars;
	end = s + p[0]->xString.value->size;
	for(; s<end; s++){
		if( *s != '%' ){
			DaoStream_WriteChar( self, *s );
			continue;
		}

		fmt = s;
		s += 1;
		if( *s =='%' || *s == '[' ){
			DaoStream_WriteChar( self, *s );
			continue;
		}

		if( ++id > N || p[id] == NULL ) goto NullParameter;
		value = p[id];

		/* flags: */
		while( *s == '+' || *s == '-' || *s == '#' || *s == '0' || *s == ' ' ) s += 1;
		while( isdigit( *s ) ) s += 1; /* width; */
		if( *s == '.' ){ /* precision: */
			s += 1;
			while( isdigit( *s ) ) s += 1;
		}
		DString_SetBytes( fmt2, fmt, s - fmt + 1 );
		if( strchr( convs, *s ) == NULL ){
			DaoProcess_RaiseWarning( proc, NULL, "invalid format conversion" );
			continue;
		}
		F = *s;
		s += 1;
		fg = bg = NULL;
		if( *s == '[' ){
			s += 1;
			fmt = s;
			while( isalnum( *s ) ) s += 1;
			if( fgcolor == NULL ) fgcolor = DString_New();
			DString_SetBytes( fgcolor, fmt, s - fmt );
			if( fgcolor->size ) fg = fgcolor->chars;
			if( *s == ':' ){
				s += 1;
				fmt = s;
				while( isalnum( *s ) ) s += 1;
				if( bgcolor == NULL ) bgcolor = DString_New();
				DString_SetBytes( bgcolor, fmt, s - fmt );
				if( bgcolor->size ) bg = bgcolor->chars;
			}
			if( *s != ']' ) goto WrongColor;
		}else{
			s -= 1;
		}
		if( fg || bg ){
			if( DaoStream_SetColor( self, fg, bg ) == 0 ) goto WrongColor;
		}
		self->format = fmt2->chars;
		if( F == 'c' || F == 'C' ){
			if( value->type != DAO_INTEGER ) goto WrongParameter;
			DString_Reset( fmt2, 0 );
			DString_AppendWChar( fmt2, value->xInteger.value );
			self->format = "%s";
			if( F == 'C' ) DString_ToLocal( fmt2 );
			DaoStream_WriteString( self, fmt2 );
		}else if( F == 'd' || F == 'i' || F == 'o' || F == 'x' || F == 'X' ){
			DString_InsertChars( fmt2, "ll", fmt2->size-1, 0, 2 );
			self->format = fmt2->chars;
			if( value->type == DAO_NONE || value->type > DAO_FLOAT ) goto WrongParameter;
			DaoStream_WriteInt( self, DaoValue_GetInteger( value ) );
		}else if( toupper( F ) == 'E' || toupper( F ) == 'F' || toupper( F ) == 'G' ){
			if( value->type == DAO_NONE || value->type > DAO_FLOAT ) goto WrongParameter;
			DaoStream_WriteFloat( self, DaoValue_GetFloat( value ) );
		}else if( F == 's' && value->type == DAO_STRING ){
			DaoStream_WriteString( self, value->xString.value );
		}else if( F == 'S' && value->type == DAO_STRING ){
			DaoStream_WriteLocalString( self, value->xString.value );
		}else if( F == 'p' ){
			DaoStream_WritePointer( self, value );
		}else if( F == 'a' ){
			self->format = NULL;
			if( value->type > DAO_ARRAY ) DMap_Reset( cyclic );
			DaoValue_Print( value, proc, self, cyclic );
		}else{
			goto WrongParameter;
		}
		self->format = NULL;
		if( fg || bg ) DaoStream_SetColor( self, NULL, NULL );
		continue;
NullParameter:
		sprintf( message, "%i-th parameter is null!", id );
		DaoProcess_RaiseWarning( proc, NULL, message );
		continue;
WrongColor:
		sprintf( message, "%i-th parameter has wrong color format!", id );
		DaoProcess_RaiseWarning( proc, NULL, message );
		continue;
WrongParameter:
		self->format = NULL;
		if( fg || bg ) DaoStream_SetColor( self, NULL, NULL );
		sprintf( message, "%i-th parameter has wrong type for format \"%s\"!", id, fmt2->chars );
		DaoProcess_RaiseWarning( proc, NULL, message );
	}
	if( cyclic ) DMap_Delete( cyclic );
	if( fgcolor ) DString_Delete( fgcolor );
	if( bgcolor ) DString_Delete( bgcolor );
	DString_Delete( fmt2 );
}
static void DaoIO_Writef( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoStream *self = & p[0]->xStream;
	if( DaoIO_CheckMode( self, proc, DAO_STREAM_WRITABLE ) == 0 ) return;
	DaoIO_Writef0( self, proc, p+1, N-1 );
}
static void DaoIO_Writef2( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoStream *stream = proc->stdioStream;
	if( stream == NULL ) stream = proc->vmSpace->stdioStream;
	if( DaoIO_CheckMode( stream, proc, DAO_STREAM_WRITABLE ) == 0 ) return;
	DaoIO_Writef0( stream, proc, p, N );
}
static void DaoIO_Flush( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoStream *self = & p[0]->xStream;
	DaoStream_Flush( self );
}
static void DaoIO_Read( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoStream *self = proc->stdioStream;
	DString *ds = DaoProcess_PutChars( proc, "" );
	int ch, size, amount = -1; /* amount=-2: all; amount=-1: line; amount>=0: bytes; */
	FILE *fin = stdin;
	char buf[IO_BUF_SIZE];

	if( self == NULL ) self = proc->vmSpace->stdioStream;
	if( N > 0 ){
		self = (DaoStream*) p[0];
		if( self->file ) fin = self->file;
		amount = -2;
	}
	if( DaoIO_CheckMode( self, proc, DAO_STREAM_READABLE ) == 0 ) return;
	if( N > 1 ){
		if( p[1]->type == DAO_INTEGER ){
			amount = p[1]->xInteger.value;
			if( amount < 0 ){
				DaoProcess_RaiseError( proc, NULL, "cannot read negative amount!" );
				return;
			}
		}else{
			amount = - 1 - p[1]->xEnum.value;
		}
	}
	DString_Reset( ds, 0 );
	if( self->redirect && self->redirect->StdioRead ){
		self->redirect->StdioRead( self->redirect, ds, amount );
	}else if( amount >= 0 ){ /* bytes */
		if( self->mode & DAO_STREAM_STRING ){
			DString_SubString( self->streamString, ds, self->offset, amount );
			self->offset += ds->size;
		}else{
			DString_Reserve( ds, amount );
			DString_Reset( ds, fread( ds->chars, 1, amount, fin ) );
		}
	}else if( amount <= -2 ){
		if( self->mode & DAO_STREAM_STRING ){
			if( self->offset == 0 ){
				DString_Assign( ds, self->streamString );
			}else{
				DString_SubString( self->streamString, ds, self->offset, -1 );
			}
			self->offset += ds->size;
		}else{
			do {
				size = fread( buf, 1, sizeof(buf), fin );
				DString_AppendBytes( ds, buf, size);
			} while ( size );
		}
	}else{
		DaoStream_ReadLine( self, ds );
	}
	if( self->mode & DAO_STREAM_AUTOCONV ) DString_ToUTF8( ds );
}

extern void Dao_MakePath( DString *base, DString *path );

/*
// Special relative paths:
// 1. ::path, path relative to the current source code file;
// 2. :path, path relative to the current working directory;
*/
static void DaoIO_MakePath( DaoProcess *proc, DString *path )
{
	if( path->size ==0 ) return;
	if( path->chars[0] != ':' ) return;
	if( path->chars[1] == ':' ){
		DString_Erase( path, 0, 2 );
		Dao_MakePath( proc->activeNamespace->path, path );
		return;
	}
	DString_Erase( path, 0, 1 );
	Dao_MakePath( proc->vmSpace->pathWorking, path );
}
static FILE* DaoIO_OpenFile( DaoProcess *proc, DString *name, const char *mode, int silent )
{
	DString *fname = DString_Copy( name );
	char buf[IO_BUF_SIZE];
	FILE *fin;

	DaoIO_MakePath( proc, fname );
	fin = Dao_OpenFile( fname->chars, mode );
	DString_Delete( fname );
	if( fin == NULL && silent == 0 ){
		snprintf( buf, IO_BUF_SIZE, "error opening file: %s", DString_GetData( name ) );
		DaoProcess_RaiseError( proc, NULL, buf );
		return NULL;
	}
	return fin;
}
static void DaoIO_ReadFile( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *res = DaoProcess_PutChars( proc, "" );
	daoint silent = p[1]->xBoolean.value;
	if( DString_Size( p[0]->xString.value ) ==0 ){
		char buf[IO_BUF_SIZE];
		while(1){
			size_t count = fread( buf, 1, sizeof( buf ), stdin );
			if( count ==0 ) break;
			DString_AppendBytes( res, buf, count );
		}
	}else{
		FILE *fin = DaoIO_OpenFile( proc, p[0]->xString.value, "r", silent );
		struct stat info;
		if( fin == NULL ) return;
		fstat( fileno( fin ), &info );
		DString_Reserve( res, info.st_size );
		DString_Reset( res, fread( res->chars, 1, info.st_size, fin ) );
		fclose( fin );
	}
}
static void DaoIO_Open( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoStream *stream = NULL;
	char *mode;
	stream = DaoStream_New();
	if( p[0]->type == DAO_ENUM ){
		if( p[0]->xEnum.value == 0 ){
			stream->mode |= DAO_STREAM_STRING;
		}else{
			stream->mode |= DAO_STREAM_FILE;
			stream->file = tmpfile();
			if( stream->file <= 0 ){
				DaoProcess_RaiseError( proc, NULL, "failed to create temporary file" );
				return;
			}
		}
	}else{
		stream->mode |= DAO_STREAM_FILE;
		mode = DString_GetData( p[1]->xString.value );
		if( p[0]->type == DAO_INTEGER ){
			stream->file = fdopen( p[0]->xInteger.value, mode );
			if( stream->file == NULL ){
				DaoProcess_RaiseError( proc, NULL, "failed to create the stream object" );
				return;
			}
		}else{
			stream->file = DaoIO_OpenFile( proc, p[0]->xString.value, mode, 0 );
		}
		if( strstr( mode, "+" ) ){
			stream->mode |= DAO_STREAM_WRITABLE | DAO_STREAM_READABLE;
		}else{
			if( strstr( mode, "r" ) )
				stream->mode |= DAO_STREAM_READABLE;
			if( strstr( mode, "w" ) || strstr( mode, "a" ) )
				stream->mode |= DAO_STREAM_WRITABLE;
		}
	}
	DaoProcess_PutValue( proc, (DaoValue*)stream );
}
static void DaoIO_Close( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoStream *self = & p[0]->xStream;
	DaoStream_Close( self );
}
static void DaoIO_Check( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoStream *self = & p[0]->xStream;
	int res = 0, what = p[1]->xEnum.value;
	switch( what ){
	case 0 : res = DaoStream_IsReadable( self ); break;
	case 1 : res = DaoStream_IsWritable( self ); break;
	case 2 : res = DaoStream_IsOpen( self ); break;
	case 3 : res = DaoStream_EndOfStream( self ); break;
	}
	DaoProcess_PutBoolean( proc, res );
}
static void DaoIO_Check2( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoStream *self = & p[0]->xStream;
	int res = 0, what = p[1]->xEnum.value;
	switch( what ){
	case 0 : res = (self->mode & DAO_STREAM_AUTOCONV) != 0; break;
	}
	DaoProcess_PutBoolean( proc, res );
}
static void DaoIO_Enable( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoStream *self = & p[0]->xStream;
	int what = p[1]->xEnum.value;
	if( p[2]->xBoolean.value ){
		self->mode |= DAO_STREAM_AUTOCONV;
	}else{
		self->mode &= ~DAO_STREAM_AUTOCONV;
	}
}
static void DaoIO_Seek( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoStream *self = & p[0]->xStream;
	daoint pos = p[1]->xInteger.value;
	int options[] = { SEEK_SET, SEEK_CUR, SEEK_END };
	int where = options[ p[2]->xEnum.value ];
	if( self->mode & DAO_STREAM_STRING ){
		switch( where ){
		case SEEK_SET : self->offset  = pos; break;
		case SEEK_CUR : self->offset += pos; break;
		case SEEK_END : self->offset = self->streamString->size - pos; break;
		}
		if( self->offset < 0 ) self->offset = 0;
	}
	if( self->file == NULL ) return;
	fseek( self->file, pos, where );
}
static void DaoIO_Tell( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoStream *self = & p[0]->xStream;
	dao_integer *num = DaoProcess_PutInteger( proc, 0 );
	if( self->mode & DAO_STREAM_STRING ) *num = self->offset;
	if( self->file == NULL ) return;
	*num = ftell( self->file );
}
static void DaoIO_FileNO( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoStream *self = & p[0]->xStream;
	dao_integer *num = DaoProcess_PutInteger( proc, 0 );
	if( self->file == NULL ) return;
	*num = fileno( self->file );
}

static void DaoStream_ReadLines( DaoStream *self, DaoList *list, DaoProcess *proc, int count, int chop )
{
	DaoValue *res;
	DaoString *line;
	DaoVmCode *sect = DaoProcess_InitCodeSection( proc, 1 );
	daoint i = 0;

	if( sect == NULL ){
		line = DaoString_New();
		while( (count == 0 || (i++) < count) && DaoStream_ReadLine( self, line->value ) ){
			if( chop ) DString_Chop( line->value, 0 );
			DaoList_Append( list, (DaoValue*) line );
		}
		DaoString_Delete( line );
	}else{
		ushort_t entry = proc->topFrame->entry;
		if( sect->b ){
			DaoString tmp = {DAO_STRING,0,0,0,1,NULL};
			DString tmp2 = DString_WrapChars( "" );
			tmp.value = & tmp2;
			line = (DaoString*) DaoProcess_SetValue( proc, sect->a, (DaoValue*)(void*) &tmp );
		}
		while( (count == 0 || (i++) < count) && DaoStream_ReadLine( self, line->value ) ){
			if( chop ) DString_Chop( line->value, 0 );
			proc->topFrame->entry = entry;
			DaoProcess_Execute( proc );
			if( proc->status == DAO_PROCESS_ABORTED ) break;
			res = proc->stackValues[0];
			if( res && res->type != DAO_NONE ) DaoList_Append( list, res );
		}
		DaoProcess_PopFrame( proc );
	}
}
static void DaoIO_ReadLines( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoStream *stream;
	DaoList *list = DaoProcess_PutList( proc );
	FILE *fin = DaoIO_OpenFile( proc, p[0]->xString.value, "r", 0 );
	int chop = p[1]->xBoolean.value;

	if( fin == NULL ) return;

	stream = DaoStream_New();
	stream->file = fin;
	DaoStream_ReadLines( stream, list, proc, 0, chop );
	DaoStream_Delete( stream );
	fclose( fin );
}
static void DaoIO_ReadLines2( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoList *list = DaoProcess_PutList( proc );
	int count = p[1]->xInteger.value;
	int chop = p[2]->xBoolean.value;
	if( DaoIO_CheckMode( (DaoStream*) p[0], proc, DAO_STREAM_READABLE ) == 0 ) return;
	DaoStream_ReadLines( (DaoStream*) p[0], list, proc, count, chop );
}

DaoFuncItem dao_io_methods[] =
{
	{ DaoIO_Open,      "open( type: enum<string,tmpfile> = $string )=>Stream" },
	{ DaoIO_Open,      "open( file: string, mode: string )=>Stream" },
	{ DaoIO_Open,      "open( fileno: int, mode: string )=>Stream" },
	{ DaoIO_Write2,    "write( invar ... : any )" },
	{ DaoIO_Writef2,   "writef( format: string, invar ... : any )" },
	{ DaoIO_Writeln2,  "writeln( invar ... : any )" },
	{ DaoIO_Read,      "read( )=>string" },
	{ DaoIO_ReadFile,  "read( file: string, silent = false )=>string" },
	{ DaoIO_ReadLines, "readlines( file: string, chop = false )[line: string=>none|@T]=>list<@T>" },
	{ NULL, NULL }
};

static DaoFuncItem streamMeths[] =
{
	{ DaoIO_Open,      "Stream( type: enum<string,tmpfile> = $string )=>Stream" },
	{ DaoIO_Open,      "Stream( file: string, mode: string )=>Stream" },
	{ DaoIO_Open,      "Stream( fileno: int, mode: string )=>Stream" },
	{ DaoIO_Write,     "write( self: Stream, data: string )" },
	{ DaoIO_Write,     "write( self: Stream, invar ... : any )" },
	{ DaoIO_Writef,    "writef( self: Stream, format: string, invar ... : any )" },
	{ DaoIO_Writeln,   "writeln( self: Stream, invar ... : any )" },
	{ DaoIO_Read,      "read( self: Stream, count = -1 )=>string" },
	{ DaoIO_Read,      "read( self: Stream, amount: enum<line,all> = $all )=>string" },
	{ DaoIO_ReadLines2,"readlines( self: Stream, numline=0, chop = false )[line: string=>none|@T]=>list<@T>" },

	{ DaoIO_Flush,     "flush( self: Stream )" },
	{ DaoIO_Close,     "close( self: Stream )" },
	{ DaoIO_Seek,      "seek( self: Stream, pos: int, from: enum<begin,current,end> )=>int" },
	{ DaoIO_Tell,      "tell( self: Stream )=>int" },
	{ DaoIO_FileNO,    "fileno( self: Stream )=>int" },
	{ DaoIO_Enable,    "enable( self: Stream, what: enum<auto_conversion>, state: bool )" },
	{ DaoIO_Check,     "check( self: Stream, what: enum<readable,writable,open,eof> ) => bool" },
	{ DaoIO_Check2,     "check( self: Stream, what: enum<auto_conversion> ) => bool" },

	{ NULL, NULL }
};

static DaoFuncItem ioDeviceMeths[] =
{
	{ NULL, "read( self: Device, count = -1 ) => string" },
	{ NULL, "write( self: Device, data: string )" },
	{ NULL, "close( self: Device )" },
	{ NULL, "check( self: Device, what: enum<readable,writable,open,eof> ) => bool" },
	{ NULL, NULL }
};

DaoTypeBase ioDeviceTyper =
{
	"Device", NULL, NULL, (DaoFuncItem*) ioDeviceMeths, {0}, {0},
	(FuncPtrDel) NULL, NULL
};

DaoTypeBase streamTyper =
{
	"Stream", NULL, NULL, (DaoFuncItem*) streamMeths, {0}, {0},
	(FuncPtrDel) DaoStream_Delete, NULL
};

DaoStream* DaoStream_New()
{
	DaoStream *self = (DaoStream*) dao_calloc( 1, sizeof(DaoStream) );
	DaoCstruct_Init( (DaoCstruct*) self, dao_type_stream );
	self->type = DAO_CSTRUCT; /* dao_type_stream may still be null in DaoVmSpace_New(); */
	self->streamString = DString_New();
#ifdef DAO_USE_GC_LOGGER
	if( dao_type_stream == NULL ) DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
void DaoStream_Close( DaoStream *self )
{
	if( self->file ){
		fflush( self->file );
		if( self->ctype == dao_type_stream ) fclose( self->file );
		self->file = NULL;
	}
	self->mode &= ~(DAO_STREAM_WRITABLE | DAO_STREAM_READABLE);
}
void DaoStream_Delete( DaoStream *self )
{
	DaoStream_Close( self );
	DString_Delete( self->streamString );
	DaoCstruct_Free( (DaoCstruct*) self );
	dao_free( self );
}
int DaoStream_IsOpen( DaoStream *self )
{
	if( self->mode & DAO_STREAM_STRING ){
		return 1;
	}else if( self->mode & DAO_STREAM_FILE ){
		return self->file != NULL;
	}else if( self->redirect ){
		return self->redirect->StdioRead != NULL || self->redirect->StdioWrite != NULL;
	}else if( self->file == stdin || self->file == stdout || self->file == stderr ){
		return 1;
	}else if( self->file == NULL ){
		return 1;
	}
	return 0;
}
int DaoStream_EndOfStream( DaoStream *self )
{
	if( self->mode & DAO_STREAM_STRING ){
		return 0;
	}else if( self->mode & DAO_STREAM_FILE ){
		if( self->file == NULL ) return 1;
		return feof( self->file );
	}else if( self->redirect ){
		return self->redirect->StdioRead == NULL && self->redirect->StdioWrite == NULL;
	}else if( self->file == stdin || self->file == stdout || self->file == stderr ){
		return 0;
	}else if( self->file == NULL ){
		return 0;
	}
	return 1;
}
int DaoStream_IsReadable( DaoStream *self )
{
	if( self->mode & DAO_STREAM_STRING ){
		return 1;
	}else if( self->mode & DAO_STREAM_FILE ){
		if( self->file == NULL ) return 0;
		if( feof( self->file ) ) return 0;
		return self->mode & DAO_STREAM_READABLE;
	}else if( self->redirect ){
		return self->redirect->StdioRead != NULL;
	}else if( self->file == NULL || self->file == stdin ){
		return 1;
	}
	return 0;
}
int DaoStream_IsWritable( DaoStream *self )
{
	if( self->mode & DAO_STREAM_STRING ){
		return 1;
	}else if( self->mode & DAO_STREAM_FILE ){
		if( self->file == NULL ) return 0;
		if( feof( self->file ) ) return 0;
		return self->mode & DAO_STREAM_WRITABLE;
	}else if( self->redirect ){
		return self->redirect->StdioWrite != NULL;
	}else if( self->file == NULL || self->file == stdout || self->file == stderr ){
		return 1;
	}
	return 0;
}
DaoUserStream* DaoStream_SetUserStream( DaoStream *self, DaoUserStream *us )
{
	DaoUserStream *stream = self->redirect;
	self->redirect = us;
	if( us ) us->stream = self;
	return stream;
}
static void DaoStream_TryResetStringBuffer( DaoStream *self )
{
	/* When it has been read passing the end of the buffer, reset the buffer: */
	if( self->offset >= self->streamString->size ){
		DString_Reset( self->streamString, 0 );
		self->offset = 0;
	}
}
void DaoStream_WriteChar( DaoStream *self, char val )
{
	const char *format = "%c";
	if( self->redirect && self->redirect->StdioWrite ){
		DString *mbs = DString_New();
		DString_AppendChar( mbs, val );
		self->redirect->StdioWrite( self->redirect, mbs );
		DString_Delete( mbs );
	}else if( self->file ){
		fprintf( self->file, format, val );
	}else if( self->mode & DAO_STREAM_STRING ){
		DaoStream_TryResetStringBuffer( self );
		DString_AppendChar( self->streamString, val );
	}else{
		printf( format, val );
	}
}
void DaoStream_WriteFormatedInt( DaoStream *self, dao_integer val, const char *format )
{
	char buffer[100];
	if( self->redirect && self->redirect->StdioWrite ){
		DString *mbs = DString_New();
		sprintf( buffer, format, val );
		DString_SetChars( mbs, buffer );
		self->redirect->StdioWrite( self->redirect, mbs );
		DString_Delete( mbs );
	}else if( self->file ){
		fprintf( self->file, format, (long long) val );
	}else if( self->mode & DAO_STREAM_STRING ){
		sprintf( buffer, format, (long long) val );
		DaoStream_TryResetStringBuffer( self );
		DString_AppendChars( self->streamString, buffer );
	}else{
		printf( format, (long long) val );
	}
}
void DaoStream_WriteInt( DaoStream *self, dao_integer val )
{
	const char *format = self->format;
	if( format == NULL ) format = "%"DAO_I64;
	DaoStream_WriteFormatedInt( self, val, format );
}
void DaoStream_WriteFloat( DaoStream *self, double val )
{
	const char *format = self->format;
	const char *iconvs = "diouxXcC";
	char buffer[100];
	if( format && strchr( iconvs, format[ strlen(format)-1 ] ) && val ==(dao_integer)val ){
		DaoStream_WriteInt( self, (dao_integer)val );
		return;
	}
	if( format == NULL ) format = "%f";
	if( self->redirect && self->redirect->StdioWrite ){
		DString *mbs = DString_New();
		sprintf( buffer, format, val );
		DString_SetChars( mbs, buffer );
		self->redirect->StdioWrite( self->redirect, mbs );
		DString_Delete( mbs );
	}else if( self->file ){
		fprintf( self->file, format, val );
	}else if( self->mode & DAO_STREAM_STRING ){
		sprintf( buffer, format, val );
		DaoStream_TryResetStringBuffer( self );
		DString_AppendChars( self->streamString, buffer );
	}else{
		printf( format, val );
	}
}
void DaoStream_WriteChars( DaoStream *self, const char *val )
{
	const char *format = self->format;
	if( format == NULL ) format = "%s";
	if( self->redirect && self->redirect->StdioWrite ){
		DString *mbs = DString_New();
		DString_SetChars( mbs, val );
		self->redirect->StdioWrite( self->redirect, mbs );
		DString_Delete( mbs );
	}else if( self->file ){
		fprintf( self->file, format, val );
	}else if( self->mode & DAO_STREAM_STRING ){
		DaoStream_TryResetStringBuffer( self );
		DString_AppendChars( self->streamString, val );
	}else{
		printf( format, val );
	}
}
void DaoStream_WriteString( DaoStream *self, DString *val )
{
	int i;
	const char *data = val->chars;
	if( self->redirect && self->redirect->StdioWrite ){
		DString *mbs = DString_New();
		DString_SetBytes( mbs, data, val->size );
		self->redirect->StdioWrite( self->redirect, mbs );
		DString_Delete( mbs );
	}else if( self->file ){
		if( self->format && strcmp( self->format, "%s" ) != 0 ){
			fprintf( self->file, self->format, data );
		}else{
			DaoFile_WriteString( self->file, val );
		}
	}else if( self->mode & DAO_STREAM_STRING ){
		DaoStream_TryResetStringBuffer( self );
		DString_AppendBytes( self->streamString, data, val->size );
	}else{
		if( self->format && strcmp( self->format, "%s" ) != 0 ){
			printf( self->format, data );
		}else{
			DaoFile_WriteString( stdout, val );
		}
	}
}
void DaoStream_WriteLocalString( DaoStream *self, DString *str )
{
	str = DString_Copy( str );
	DString_ToLocal( str );
	DaoStream_WriteString( self, str );
	DString_Delete( str );
}
void DaoStream_WritePointer( DaoStream *self, void *val )
{
	const char *format = self->format;
	char buffer[100];
	if( format == NULL ) format = "%p";
	if( self->redirect && self->redirect->StdioWrite ){
		DString *mbs = DString_New();
		sprintf( buffer, format, val );
		DString_SetChars( mbs, buffer );
		self->redirect->StdioWrite( self->redirect, mbs );
		DString_Delete( mbs );
	}else if( self->file ){
		fprintf( self->file, format, val );
	}else if( self->mode & DAO_STREAM_STRING ){
		sprintf( buffer, format, val );
		DaoStream_TryResetStringBuffer( self );
		DString_AppendChars( self->streamString, buffer );
	}else{
		printf( format, val );
	}
}
void DaoStream_WriteNewLine( DaoStream *self )
{
	DaoStream_WriteChars( self, daoConfig.iscgi ? "<br/>" : "\n" );
}
int DaoStream_ReadLine( DaoStream *self, DString *line )
{
	int ch, delim = '\n';
	char buf[IO_BUF_SIZE];
	char *start = buf, *end = buf + IO_BUF_SIZE;

	DString_Reset( line, 0 );
	if( self->redirect && self->redirect->StdioRead ){
		self->redirect->StdioRead( self->redirect, line, 0 );
		if( self->mode & DAO_STREAM_AUTOCONV ) DString_ToUTF8( line );
		return line->size >0;
	}else if( self->file ){
		int res = DaoFile_ReadLine( self->file, line );
		if( self->mode & DAO_STREAM_AUTOCONV ) DString_ToUTF8( line );
		return res;
	}else if( self->mode & DAO_STREAM_STRING ){
		daoint pos = DString_FindChar( self->streamString, delim, self->offset );
		if( self->offset == 0 && (pos == DAO_NULLPOS || pos == self->streamString->size-1) ){
			DString_Assign( line, self->streamString );
			self->offset = self->streamString->size;
		}else if( pos == DAO_NULLPOS ){
			DString_SubString( self->streamString, line, pos, -1 );
			self->offset = self->streamString->size;
		}else{
			DString_SubString( self->streamString, line, pos, pos - self->offset + 1 );
			self->offset = pos + 1;
		}
		if( self->mode & DAO_STREAM_AUTOCONV ) DString_ToUTF8( line );
		return self->offset < self->streamString->size;
	}else{
		*start = ch = getchar();
		start += 1;
		while( ch != delim && ch != EOF ){
			*start = ch = getchar();
			start += 1;
			if( start == end ){
				if( ch == EOF ) start -= 1;
				DString_AppendBytes( line, buf, start-buf );
				start = buf;
			}
		}
		if( ch == EOF && start != buf ) start -= 1;
		DString_AppendBytes( line, buf, start-buf );
		if( self->mode & DAO_STREAM_AUTOCONV ) DString_ToUTF8( line );
		clearerr( stdin );
		return ch != EOF;
	}
	return 0;
}
int DaoFile_ReadLine( FILE *fin, DString *line )
{
	int ch;

	DString_Reset( line, 0 );
	if( feof( fin ) ) return 0;

	while( (ch = fgetc(fin)) != EOF ){
		if( line->size == line->bufSize ) DString_Reserve( line, 5 + 1.2*line->size );
		line->chars[ line->size ++ ] = ch;
		line->chars[ line->size ] = '\0';
		if( ch == '\n' ) break;
	}
	return 1;
}
int DaoFile_ReadAll( FILE *fin, DString *output, int close )
{
	char buf[IO_BUF_SIZE];
	DString_Reset( output, 0 );
	if( fin == NULL ) return 0;
	while(1){
		size_t count = fread( buf, 1, IO_BUF_SIZE, fin );
		if( count ==0 ) break;
		DString_AppendBytes( output, buf, count );
	}
	if( close ) fclose( fin );
	return 1;
}
int DaoFile_ReadPart( FILE *fin, DString *output, daoint offset, daoint count )
{
	char buf[IO_BUF_SIZE];
	daoint k, m, size = output->size;

	if( fin == NULL ) return 0;
	fseek( fin, offset, SEEK_SET );
	while( count > 0 ){
		m = count < IO_BUF_SIZE ? count : IO_BUF_SIZE;
		k = fread( buf, 1, m, fin );
		if( k == 0 ) break;
		DString_AppendBytes( output, buf, k );
		count -= k;
	}
//	if( close ) fclose( fin );
	return output->size - size;
}
void DaoFile_WriteString( FILE* file, DString *str )
{
	daoint pos = 0;
	while( 1 ){
		fprintf( file, "%s", str->chars + pos );
		pos = DString_FindChar( str, '\0', pos );
		if( pos == DAO_NULLPOS ) break;
		fprintf( file, "%c", 0 );
		pos += 1;
	}
}

void DaoStream_SetFile( DaoStream *self, FILE *fd )
{
	DaoValue *p = (DaoValue*) self;
	self->file = fd;
}
FILE* DaoStream_GetFile( DaoStream *self )
{
	if( self->file ) return self->file;
	return NULL;
}
