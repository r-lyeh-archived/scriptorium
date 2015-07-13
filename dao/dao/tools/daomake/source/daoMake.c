/*
// DaoMake Tool
// http://www.daovm.net
//
// Copyright (c) 2013-2015, Limin Fu
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
#include<stdint.h>
#include<math.h>
#include<errno.h>

#include"daoValue.h"
#include"daoStdtype.h"
#include"daoNamespace.h"
#include"daoProcess.h"
#include"daoStream.h"
#include"daoVmspace.h"

#ifdef _WIN32

#include"io.h"
#ifdef _MSC_VER
#define getcwd _getcwd
#define mkdir _mkdir
#define rmdir _rmdir
#define chmod _chmod
#endif

#endif

#ifdef UNIX
#include<sys/stat.h>
#include<unistd.h>
#include<dirent.h>
#endif

#ifdef LINUX
#define DAOMAKE_PLATFORM  "linux"
#elif defined( IOS )
#define DAOMAKE_PLATFORM  "ios"
#elif defined( MACOSX )
#define DAOMAKE_PLATFORM  "macosx"
#elif defined( FREEBSD )
#define DAOMAKE_PLATFORM  "freebsd"
#elif defined( OPENBSD )
#define DAOMAKE_PLATFORM  "openbsd"
#elif defined( MINIX )
#define DAOMAKE_PLATFORM  "minix"
#elif defined( BEOS )
#define DAOMAKE_PLATFORM  "beos"
#elif defined( MINGW )
#define DAOMAKE_PLATFORM  "mingw"
#endif


static DaoVmSpace *vmSpace = NULL;


typedef struct DaoMakeUnit     DaoMakeUnit;
typedef struct DaoMakeObjects  DaoMakeObjects;
typedef struct DaoMakeTarget   DaoMakeTarget;
typedef struct DaoMakeProject  DaoMakeProject;


enum DaoMakeModes
{
	DAOMAKE_RELEASE ,
	DAOMAKE_DEBUG ,
	DAOMAKE_PROFILE
};

enum DaoMakeTargetTypes
{
	DAOMAKE_EXECUTABLE ,
	DAOMAKE_SHAREDLIB ,
	DAOMAKE_STATICLIB ,
	DAOMAKE_JAVASCRIPT ,
	DAOMAKE_TESTING ,
	DAOMAKE_COMMAND ,
	DAOMAKE_DIRECTORY
};

const char *const daomake_test_sumfile = "daotest_result_summary.txt";
const char *const daomake_objects_dir = "DaoMake.Objs";

const char *const daomake_mode_keys[] =
{
	"RELEASE-AFLAG" ,
	"RELEASE-CFLAG" ,
	"RELEASE-LFLAG" ,
	"DEBUG-AFLAG" ,
	"DEBUG-CFLAG" ,
	"DEBUG-LFLAG" ,
	"PROFILE-AFLAG" ,
	"PROFILE-CFLAG" ,
	"PROFILE-LFLAG"
};

const char *const daomake_prefix_keys[] =
{
	"" ,
	"DLL-PREFIX" ,
	"LIB-PREFIX" ,
	"" ,
	"" ,
	"" ,
	""
};

const char *const daomake_suffix_keys[] =
{
	"EXE-SUFFIX" ,
	"DLL-SUFFIX" ,
	"LIB-SUFFIX" ,
	"" ,
	"" ,
	"" ,
	""
};


/*
// MakeUnit:
// -- Base type for objects, targets and projects; 
//
// MakeObjects:
// -- Type for compiled object lists;
// -- Supports compiling flags only;
//
// MakeTarget:
// -- Type for linking targets;
// -- Supports linking flags only;
//
// MakeProject:
// -- Type for building projects;
// -- Supports both compiling flags and linking flags;
// -- Compiling flags are applied to all its objects;
// -- Linking flags are applied to all its targets;
// -- These compiling and linking flags are public flags
//    that can be used by other objects, targets and projects.
//
// When using a target (library) from a project (package):
// -- Compiling flags of the project is used by MakeObjects;
// -- Linking flags of the project is used by MakeTarget;
*/
struct DaoMakeUnit
{
	DAO_CSTRUCT_COMMON;

	DaoMakeProject *project;

	DString *sourcePath;  /* Path to "makefile.dao"; */
	DString *binaryPath;  /* Path to the resulting binary; */
	DString *buildPath;   /* Path to "makefile.dao" or the building directory; */

	DList  *definitions;
	DList  *includePaths;
	DList  *linkingPaths;
	DList  *compilingFlags;
	DList  *linkingFlags;
};


struct DaoMakeObjects
{
	DaoMakeUnit  base;

	DList  *headers;
	DList  *sources;
};

struct DaoMakeTarget
{
	DaoMakeUnit  base;

	DString  *name;
	DList    *objects;
	DList    *tests;
	DList    *commands;
	DList    *depends;
	DString  *extradeps;
	DString  *testMacro;
	DString  *path;
	DString  *install;
	uchar_t   ttype;
	uchar_t   dynamicLinking;
	uchar_t   dynamicExporting;
};


struct DaoMakeProject
{
	DaoMakeUnit  base;

	DString  *sourceName;
	DString  *projectName;
	DString  *targetPath;
	uchar_t   generateFinder;

	DList    *targets;
	DList    *targets2;
	DList    *variables;
	DList    *installs;
	DMap     *tests;
	DMap     *exportPaths;

	/*
	// In the following maps:
	// the keys are the macro names or the target names;
	// the values are the entire macro or rule;
	*/
	DMap     *headerMacros;   /* HEADERS = header1.h header2.h; */
	DMap     *aflagsMacros;   /* AFLAGS = ...; */
	DMap     *cflagsMacros;   /* CFLAGS = ...; */
	DMap     *lflagsMacros;   /* LFLAGS = ...; */
	DMap     *objectRules;    /* OBJECT: DEPS \n\t COMMAND; */
	DMap     *objectsMacros;  /* OBJECTS = ...; */
	DMap     *testRules;      /* TEST: DEPS \n\t COMMAND; */
	DMap     *testsMacros;    /* TESTS = ...; */
	DList    *targetRules;    /* TARGET: DEPS \n\t COMMAND; */

	DMap     *signatures;
	uint_t    signature;

	DString  *string;
	DList    *strings;
	uint_t    usedStrings;

	DMap     *mapStringInt;
};



static DaoMap  *daomake_projects = NULL;
static DaoMap  *daomake_settings = NULL;
static DaoMap  *daomake_platforms = NULL;
static DaoMap  *daomake_packages  = NULL;
static DaoMap  *daomake_assemblers = NULL;
static DaoMap  *daomake_compilers = NULL;
static DaoMap  *daomake_linkers = NULL;
static DaoList *daomake_includes = NULL;

static DMap    *daomake_variable_map  = NULL;
static DMap    *daomake_variable_map2 = NULL;
static DMap    *daomake_variable_map3 = NULL;
static DList   *daomake_variable_list = NULL;

static DMap *daomake_boolean_options = NULL;
static DMap *daomake_string_options = NULL;

static DMap *daomake_cmdline_defines = NULL;

static DMap *daomake_makefile_paths = NULL;

static DaoType *daomake_type_unit = NULL;
static DaoType *daomake_type_objects = NULL;
static DaoType *daomake_type_target  = NULL;
static DaoType *daomake_type_project = NULL;

static DString *daomake_platform = NULL;
static DString *daomake_architecture = NULL;
static DString *daomake_current_path = NULL;
static DString *daomake_main_source_path = NULL;
static DString *daomake_test_tool = NULL;
static DString *daomake_test_tool_option = NULL;

static char *daomake_makefile_suffix = "";
static int daomake_build_mode = DAOMAKE_RELEASE;
static int daomake_out_of_source = 0;
static int daomake_local_rpath = 1;
static int daomake_reset_cache = 0;
static int daomake_test_count = 0;

static int daomake_relative_rpath = 1;



void DaoMakeUnit_Init( DaoMakeUnit *self, DaoType *type )
{
	DaoCstruct_Init( (DaoCstruct*)self, type );
	self->project = NULL;
	self->sourcePath = DString_New();
	self->binaryPath = DString_New();
	self->buildPath = DString_New();
	self->definitions = DList_New(DAO_DATA_STRING);
	self->includePaths = DList_New(DAO_DATA_STRING);
	self->linkingPaths = DList_New(DAO_DATA_STRING);
	self->compilingFlags = DList_New(DAO_DATA_STRING);
	self->linkingFlags = DList_New(DAO_DATA_STRING);
}
void DaoMakeUnit_Free( DaoMakeUnit *self )
{
	DaoCstruct_Free( (DaoCstruct*) self );
	DString_Delete( self->sourcePath );
	DString_Delete( self->binaryPath );
	DString_Delete( self->buildPath );
	DList_Delete( self->definitions );
	DList_Delete( self->includePaths );
	DList_Delete( self->linkingPaths );
	DList_Delete( self->compilingFlags );
	DList_Delete( self->linkingFlags );
}

DaoMakeObjects* DaoMakeObjects_New()
{
	DaoMakeObjects *self = (DaoMakeObjects*) dao_calloc( 1, sizeof(DaoMakeObjects) );
	DaoMakeUnit_Init( (DaoMakeUnit*) & self->base, daomake_type_objects );
	self->headers = DList_New(DAO_DATA_STRING);
	self->sources = DList_New(DAO_DATA_STRING);
	return self;
}
void DaoMakeObjects_Delete( DaoMakeObjects *self )
{
	DaoMakeUnit_Free( (DaoMakeUnit*) & self->base );
	DList_Delete( self->headers );
	DList_Delete( self->sources );
	dao_free( self );
}
DaoMakeTarget* DaoMakeTarget_New()
{
	DaoMakeTarget *self = (DaoMakeTarget*) dao_calloc( 1, sizeof(DaoMakeTarget) );
	DaoMakeUnit_Init( (DaoMakeUnit*) & self->base, daomake_type_target );
	self->name = DString_New();
	self->objects = DList_New(DAO_DATA_VALUE);
	self->tests   = DList_New(DAO_DATA_STRING);
	self->commands = DList_New(DAO_DATA_STRING);
	self->depends = DList_New(DAO_DATA_VALUE);
	self->extradeps = DString_New();
	self->testMacro = DString_New();
	self->path = DString_New();
	self->install = DString_New();
	self->ttype = DAOMAKE_EXECUTABLE;
	self->dynamicLinking = 1;
	self->dynamicExporting = 1;
	return self;
}
void DaoMakeTarget_Delete( DaoMakeTarget *self )
{
	DaoMakeUnit_Free( (DaoMakeUnit*) & self->base );
	DString_Delete( self->name );
	DList_Delete( self->objects );
	DList_Delete( self->tests );
	DList_Delete( self->commands );
	DList_Delete( self->depends );
	DString_Delete( self->extradeps );
	DString_Delete( self->testMacro );
	DString_Delete( self->path );
	DString_Delete( self->install );
	dao_free( self );
}
DaoMakeProject* DaoMakeProject_New()
{
	DaoMakeProject *self = (DaoMakeProject*) dao_calloc( 1, sizeof(DaoMakeProject) );
	DaoMakeUnit_Init( (DaoMakeUnit*) & self->base, daomake_type_project );
	self->base.project = self;
	self->sourceName = DString_New();
	self->projectName = DString_New();
	self->targetPath = DString_New();

	self->targets = DList_New(DAO_DATA_VALUE);
	self->targets2 = DList_New(DAO_DATA_VALUE);
	self->variables = DList_New(DAO_DATA_STRING);
	self->installs = DList_New(DAO_DATA_VALUE);
	self->tests = DMap_New(DAO_DATA_STRING,DAO_DATA_LIST);

	self->exportPaths = DMap_New(DAO_DATA_STRING,DAO_DATA_STRING);

	self->headerMacros = DMap_New(DAO_DATA_STRING,DAO_DATA_STRING);
	self->aflagsMacros = DMap_New(DAO_DATA_STRING,DAO_DATA_STRING);
	self->cflagsMacros = DMap_New(DAO_DATA_STRING,DAO_DATA_STRING);
	self->lflagsMacros = DMap_New(DAO_DATA_STRING,DAO_DATA_STRING);
	self->objectRules = DMap_New(DAO_DATA_STRING,DAO_DATA_STRING);
	self->objectsMacros = DMap_New(DAO_DATA_STRING,DAO_DATA_STRING);
	self->testRules = DMap_New(DAO_DATA_STRING,DAO_DATA_STRING);
	self->testsMacros = DMap_New(DAO_DATA_STRING,DAO_DATA_STRING);
	self->targetRules = DList_New(DAO_DATA_STRING);
	self->signatures = DMap_New(DAO_DATA_STRING,DAO_DATA_STRING);
	self->signature = 4;

	self->string = DString_New();
	self->strings = DList_New(DAO_DATA_STRING);
	self->usedStrings = 0;
	DList_Append( self->strings, self->string );

	self->mapStringInt = DMap_New(DAO_DATA_STRING,0);
	return self;
}
void DaoMakeProject_Delete( DaoMakeProject *self )
{
	DaoMakeUnit_Free( (DaoMakeUnit*) & self->base );
	DString_Delete( self->sourceName );
	DString_Delete( self->projectName );
	DString_Delete( self->targetPath );

	DList_Delete( self->targets );
	DList_Delete( self->targets2 );
	DList_Delete( self->variables );
	DList_Delete( self->installs );
	DMap_Delete( self->tests );

	DMap_Delete( self->exportPaths );

	DMap_Delete( self->headerMacros );
	DMap_Delete( self->aflagsMacros );
	DMap_Delete( self->cflagsMacros );
	DMap_Delete( self->lflagsMacros );
	DMap_Delete( self->objectRules );
	DMap_Delete( self->objectsMacros );
	DMap_Delete( self->testRules );
	DMap_Delete( self->testsMacros );
	DMap_Delete( self->signatures );
	DList_Delete( self->targetRules );

	DString_Delete( self->string );
	DList_Delete( self->strings );
	DMap_Delete( self->mapStringInt );
	dao_free( self );
}




static void MD5_Append( DString *md5, uint32_t h )
{
	const char *hex = "0123456789abcdef";
	uint32_t k;
	DString_Reserve( md5, md5->size + 8 );
	k = (h>> 0)&0xff;
	md5->chars[md5->size++] = hex[k>>4];
	md5->chars[md5->size++] = hex[k&0xf];
	k = (h>> 8)&0xff;
	md5->chars[md5->size++] = hex[k>>4];
	md5->chars[md5->size++] = hex[k&0xf];
	k = (h>>16)&0xff;
	md5->chars[md5->size++] = hex[k>>4];
	md5->chars[md5->size++] = hex[k&0xf];
	k = (h>>24)&0xff;
	md5->chars[md5->size++] = hex[k>>4];
	md5->chars[md5->size++] = hex[k&0xf];
	md5->chars[md5->size] = '\0';
}
static void MD5_Update( uint32_t H[4], uint32_t W[16], uint32_t K[64] )
{
	static uint32_t R[64] = {
		7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
		5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
		4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
		6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
	};
	uint32_t A = H[0];
	uint32_t B = H[1];
	uint32_t C = H[2];
	uint32_t D = H[3];
	uint32_t k;
	for(k=0; k<16; k++){
		uint32_t f = (B & C) | ((~B) & D);
		uint32_t g = k;
		uint32_t t = D;
		uint32_t x = A + f + K[k] + W[g];
		D = C;
		C = B;
		B = B + ((x << R[k]) | (x >> (32-R[k])));
		A = t;
	}
	for(k=16; k<32; k++){
		uint32_t f = (D & B) | ((~D) & C);
		uint32_t g = (k*5 + 1) % 16;
		uint32_t t = D;
		uint32_t x = A + f + K[k] + W[g];
		D = C;
		C = B;
		B = B + ((x << R[k]) | (x >> (32-R[k])));
		A = t;
	}
	for(k=32; k<48; k++){
		uint32_t f = B ^ C ^ D;
		uint32_t g = (k*3 + 5) % 16;
		uint32_t t = D;
		uint32_t x = A + f + K[k] + W[g];
		D = C;
		C = B;
		B = B + ((x << R[k]) | (x >> (32-R[k])));
		A = t;
	}
	for(k=48; k<64; k++){
		uint32_t f = C ^ (B | (~D));
		uint32_t g = (k*7) % 16;
		uint32_t t = D;
		uint32_t x = A + f + K[k] + W[g];
		D = C;
		C = B;
		B = B + ((x << R[k]) | (x >> (32-R[k])));
		A = t;
	}
	H[0] += A;
	H[1] += B;
	H[2] += C;
	H[3] += D;
}

void DString_MD5( DString *self, DString *md5 )
{
	DString *padding = md5;
	uint64_t i, k, m, n, twop32 = ((uint64_t)1)<<32;
	uint32_t H[4] = { 0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476 };
	uint32_t K[64], W[16];
	int32_t size = self->size;
	int32_t chunks = self->size / 64;
	uint8_t *data = (uint8_t*) self->chars;

	for(i=0; i<64; i++) K[i] = (uint32_t) floor( fabs( sin(i+1) ) * twop32 );
	for(i=0; i<chunks; i++){
		for(k=0; k<16; k++){
			uint32_t b = i*64 + k*4;
			uint32_t m = data[b];
			m |= ((uint32_t)data[b+1])<<8;
			m |= ((uint32_t)data[b+2])<<16;
			m |= ((uint32_t)data[b+3])<<24;
			W[k] = m;
		}
		MD5_Update( H, W, K );
	}
	DString_Reserve( padding, 128 );
	padding->size = 64;
	m = size - chunks*64;
	if( m ) memcpy( padding->chars, data + chunks*64, m*sizeof(char) );
	if( m + 8 > 64 ) padding->size = 128;
	chunks = padding->size / 64;

	data = (uint8_t*) padding->chars;
	data[m] = 1<<7; // first bit 1 followed by bit 0s;
	for(i=m+1; i<padding->size-8; i++) data[i] = 0;
	n = size * 8;
	// last 64 bits to store the string size in little endian:
	data[i] = n & 0xff;
	data[i+1] = (n >> 8) & 0xff;
	data[i+2] = (n >> 16) & 0xff;
	data[i+3] = (n >> 24) & 0xff;
	data[i+4] = (n >> 32) & 0xff;
	data[i+5] = (n >> 40) & 0xff;
	data[i+6] = (n >> 48) & 0xff;
	data[i+7] = (n >> 56) & 0xff;
	for(i=0; i<chunks; i++){
		for(k=0; k<16; k++){
			uint32_t b = i*64 + k*4;
			uint32_t m = data[b];
			m |= ((uint32_t)data[b+1])<<8;
			m |= ((uint32_t)data[b+2])<<16;
			m |= ((uint32_t)data[b+3])<<24;
			W[k] = m;
		}
		MD5_Update( H, W, K );
	}
	md5->size = 0;
	MD5_Append( md5, H[0] );
	MD5_Append( md5, H[1] );
	MD5_Append( md5, H[2] );
	MD5_Append( md5, H[3] );
}
static void DString_AppendGap( DString *self )
{
	char *mbs = self->chars;
	daoint size = self->size;
	while( size >= 2 && isspace( mbs[size-2] ) && isspace( mbs[size-1] ) ) size -= 1;
	self->chars[size] = '\0';
	self->size = size;
	if( size && isspace( mbs[size-1] ) == 0 ) DString_AppendChar( self, ' ' );
}
static int DString_CommonPrefixLength( DString *self, DString *other )
{
	daoint i = 0;
	while( i < self->size && i < other->size && self->chars[i] == other->chars[i] ) i += 1;
	return i;
}



DString* DaoMake_GetSettingValue( const char *key )
{
	DaoValue *value = DaoMap_GetValueChars( daomake_settings, key );
	if( value == NULL ) return NULL;
	return DaoValue_TryGetString( value );
}

int DaoMake_IsFile( const char *path )
{
#ifdef _WIN32
	int att = GetFileAttributes( path );
	if( att == -1 ) return 0;
	return !(att & FILE_ATTRIBUTE_DIRECTORY);
#else
	return Dao_IsFile( path ); /* Does not work for ".." in msys environment; */
#endif
}
int DaoMake_IsDir( const char *path )
{
#ifdef _WIN32
	int att = GetFileAttributes( path );
	if( att == -1 ) return 0;
	return att & FILE_ATTRIBUTE_DIRECTORY;
#else
	return Dao_IsDir( path ); /* Does not work for ".." in msys environment; */
#endif
}
int DaoMake_MakeDir( const char *dir )
{
#ifdef _WIN32
	return mkdir( dir );
#else
	return mkdir( dir, 0777 );
#endif
}
size_t DaoMake_FindFile( DString *file, DString *hints )
{
	DString *fname = DString_New();
	daoint i = 0, size = hints->size;
	size_t res = 0;
	while( i < size ){
		daoint end = DString_FindChar( hints, ';', i );
		if( end == DAO_NULLPOS ) end = size;
		DString_SubString( hints, fname, i, end - i );
		DString_AppendPathSep( fname );
		DString_Append( fname, file );
		if( DaoMake_IsFile( fname->chars ) ){
			res = (i<<16)|(end-i);
			break;
		}
		i = end + 1;
	}
	DString_Delete( fname );
	return res;
}
void Dao_MakePath( DString *base, DString *path );
void DaoMake_MakePath( DString *base, DString *path )
{
	Dao_MakePath( base, path );
	if( DaoMake_IsDir( path->chars ) ) DString_AppendPathSep( path );
}
void DaoMake_MakeRelativePath( DString *current, DString *path )
{
	daoint i = 0;
	current = DString_Copy( current );
	DString_AppendPathSep( current );
	while( i < current->size && i < path->size && current->chars[i] == path->chars[i] ) i += 1;
	if( i == 0 ) goto Finalize;
	while( i >= 0 && path->chars[i] != '/' ) i -= 1;
	if( i <= 0 ) goto Finalize;
	DString_Erase( current, 0, i + 1 );
	DString_Erase( path, 0, i + 1 );
	i = 0;
	while( i < current->size ){
		while( current->chars[i] != '/' ) i += 1;
		DString_InsertChars( path, "../", 0, 0, 0 );
		i += 1;
	}
Finalize:
	if( path->size == 0 ) DString_AppendChar( path, '.' );
	DString_Delete( current );
}
void DaoMake_MakeDirs( DString *path, int isfile )
{
	daoint k = 0;

	while( k < path->size ){
		while( k < path->size && path->chars[k] != '/' ) k += 1;
		if( k == path->size ) break;
		path->chars[k] = '\0';
		if( DaoMake_IsDir( path->chars ) == 0 && DaoMake_IsFile( path->chars ) == 0 ){
			DaoMake_MakeDir( path->chars );
		}
		path->chars[k] = '/';
		k += 1;
	}
	if( isfile ) return;
	if( DaoMake_IsDir( path->chars ) == 0 && DaoMake_IsFile( path->chars ) == 0 ){
		DaoMake_MakeDir( path->chars );
	}
}
void DaoMake_MakeOutOfSourcePath( DString *path, int isfile )
{
	if( daomake_out_of_source == 0 ) return;
	DaoMake_MakeRelativePath( daomake_main_source_path, path );
	DaoMake_MakePath( vmSpace->startPath, path );

	DaoMake_MakeDirs( path, isfile );
}
void DaoMakeProject_MakeSourcePath( DaoMakeProject *self, DString *path )
{
	DaoMake_MakePath( self->base.sourcePath, path );
}
void DaoMakeProject_MakeBuildPath( DaoMakeProject *self, DString *path )
{
	DaoMake_MakePath( self->base.buildPath, path );
}

static void DaoMakeUnit_InitBuildPath( DaoMakeUnit *self, DaoProcess *proc )
{
	DString_Append( self->sourcePath, proc->activeNamespace->path );
	DString_Append( self->buildPath, proc->activeNamespace->path );
	if( daomake_out_of_source ){
		DaoMake_MakeOutOfSourcePath( self->buildPath, 0 );
	}
	DString_Assign( self->binaryPath, self->buildPath );
}
static void DaoMake_MakeObjectDir( DString *sourcePath )
{
	DString *objdir = DString_NewChars( daomake_objects_dir );

	DaoMake_MakePath( sourcePath, objdir );
	if( daomake_out_of_source ){
		DaoMake_MakeOutOfSourcePath( objdir, 0 );
	}else{
		DaoMake_MakeDir( objdir->chars );
	}
	DString_Delete( objdir );
}


DString* DaoMakeProject_GetBufferString( DaoMakeProject *self )
{
	if( self->usedStrings >= self->strings->size )
		DList_Append( self->strings, self->strings->items.pString[0] );
	self->usedStrings += 1;
	DString_Reset( self->strings->items.pString[ self->usedStrings - 1 ], 0 );
	return self->strings->items.pString[ self->usedStrings - 1 ];
}




static void DString_AppendDefinition( DString *defs, DString *key, DString *value )
{
	DString_AppendGap( defs );
	DString_AppendChars( defs, "-D" );
	DString_Append( defs, key );
	if( value->size ){
		DString_AppendChar( defs, '=' );
		DString_Append( defs, value );
	}
}
void DaoMakeUnit_FormatDefs( DaoMakeUnit *self, DString *defs )
{
	daoint i;
	for(i=0; i<self->definitions->size; i+=2){
		DString *definition = self->definitions->items.pString[i];
		DString *value = self->definitions->items.pString[i+1];
		DString_AppendDefinition( defs, definition, value );
	}
}
void DaoMakeObjects_FormatDefs( DaoMakeObjects *self, DString *defs )
{
	DaoMakeUnit_FormatDefs( (DaoMakeUnit*) self, defs );
}
void DaoMakeProject_FormatDefs( DaoMakeProject *self, DString *defs )
{
	DNode *it;
	for(it=DMap_First(daomake_cmdline_defines); it; it=DMap_Next(daomake_cmdline_defines,it)){
		DString_AppendDefinition( defs, it->key.pString, it->value.pString );
	}
	DaoMakeUnit_FormatDefs( (DaoMakeUnit*) self, defs );
}

void DaoMakeUnit_FormatIPaths( DaoMakeUnit *self, DString *cflags )
{
	DString *path = DString_New();
	daoint i;
	for(i=0; i<self->includePaths->size; ++i){
		DString_Assign( path, self->includePaths->items.pString[i] );
		DaoMake_MakePath( self->sourcePath, path );
		DaoMake_MakeRelativePath( self->buildPath, path );
		DString_AppendGap( cflags );
		DString_AppendChars( cflags, "-I" );
		DString_Append( cflags, path );
	}
	DString_Delete( path );
}
void DaoMakeObjects_FormatIPaths( DaoMakeObjects *self, DString *cflags )
{
	DaoMakeUnit_FormatIPaths( (DaoMakeUnit*) self, cflags );
}
void DaoMakeProject_FormatIPaths( DaoMakeProject *self, DString *cflags )
{
	DaoMakeUnit_FormatIPaths( (DaoMakeUnit*) self, cflags );
}

void DaoMakeUnit_FormatCFlags( DaoMakeUnit *self, DString *cflags )
{
	daoint i, j;
	DString_AppendGap( cflags );
	for(i=0; i<self->compilingFlags->size; ++i){
		DString_AppendGap( cflags );
		DString_Append( cflags, self->compilingFlags->items.pString[i] );
	}
}
void DaoMakeObjects_FormatCFlags( DaoMakeObjects *self, DString *cflags )
{
	DaoMakeUnit_FormatCFlags( (DaoMakeUnit*) self, cflags );
}
void DaoMakeProject_FormatCFlags( DaoMakeProject *self, DString *cflags )
{
	DaoMakeUnit_FormatCFlags( (DaoMakeUnit*) self, cflags );
}


void DaoMakeUnit_FormatLPaths( DaoMakeUnit *self, DString *lflags, DaoMakeUnit *target )
{
	DString *rpath = DaoMake_GetSettingValue( "DLL-RPATH" );
	DString *rpath2 = DaoMake_GetSettingValue( "DLL-RPATH-REL" );
	DString *path = DString_New();
	daoint i;
	if( target == NULL ) target = self;
	for(i=0; i<self->linkingPaths->size; ++i){
		DString_Assign( path, self->linkingPaths->items.pString[i] );
		DaoMake_MakePath( self->sourcePath, path );
		DString_AppendGap( lflags );
		if( rpath->size && rpath2->size ){
			if( daomake_relative_rpath ){
				DaoMake_MakeRelativePath( target->binaryPath, path );
				DString_Append( lflags, rpath2 );
			}else{
				DString_Append( lflags, rpath );
			}
			DString_Append( lflags, path );
		}
		DString_Assign( path, self->linkingPaths->items.pString[i] );
		DaoMake_MakePath( self->sourcePath, path );
		DaoMake_MakeRelativePath( target->binaryPath, path );
		DString_AppendGap( lflags );
		DString_AppendChars( lflags, "-L" );
		DString_Assign( path, self->linkingPaths->items.pString[i] );
		DaoMake_MakePath( self->sourcePath, path );
		DaoMake_MakeRelativePath( target->buildPath, path );
		DString_Append( lflags, path );
	}
	DString_Delete( path );
}
void DaoMakeTarget_FormatLPaths( DaoMakeTarget *self, DString *lflags )
{
	DaoMakeUnit_FormatLPaths( (DaoMakeUnit*) self, lflags, (DaoMakeUnit*) self );
}
void DaoMakeProject_FormatLPaths( DaoMakeProject *self, DString *lflags, DaoMakeTarget *tar )
{
	DaoMakeUnit_FormatLPaths( (DaoMakeUnit*) self, lflags, (DaoMakeUnit*) tar );
}

void DaoMakeUnit_FormatLFlags( DaoMakeUnit *self, DString *lflags )
{
	daoint i, j;
	DString_AppendGap( lflags );
	for(i=0; i<self->linkingFlags->size; ++i){
		DString_AppendGap( lflags );
		DString_Append( lflags, self->linkingFlags->items.pString[i] );
	}
}
void DaoMakeTarget_FormatLFlags( DaoMakeTarget *self, DString *lflags )
{
	DaoMakeUnit_FormatLFlags( (DaoMakeUnit*) self, lflags );
}
void DaoMakeProject_FormatLFlags( DaoMakeProject *self, DString *lflags )
{
	DaoMakeUnit_FormatLFlags( (DaoMakeUnit*) self, lflags );
}

void DList_AppendPaths( DList *one, DList *another, DString *basePath )
{
	int i;
	for(i=0; i<another->size; ++i){
		DString *path;
		DList_Append( one, another->items.pString[i] );
		path = (DString*) DList_Back( one );
		DaoMake_MakePath( basePath, path );
	}
}
void DaoMakeUnit_Use( DaoMakeUnit *self, DaoMakeUnit *other, int import )
{
	if( self->ctype == daomake_type_objects && other->ctype == daomake_type_project ){
		DList_AppendPaths( self->includePaths, other->includePaths, other->sourcePath );
		DList_AppendList( self->definitions, other->definitions );
		DList_AppendList( self->compilingFlags, other->compilingFlags );
	}else if( self->ctype == daomake_type_target && other->ctype == daomake_type_project ){
		DList_AppendPaths( self->linkingPaths, other->linkingPaths, other->sourcePath );
		DList_AppendList( self->linkingFlags, other->linkingFlags );
	}else if( self->ctype == daomake_type_project && other->ctype == daomake_type_project ){
		DList_AppendPaths( self->includePaths, other->includePaths, other->sourcePath );
		DList_AppendPaths( self->linkingPaths, other->linkingPaths, other->sourcePath );
		DList_AppendList( self->definitions, other->definitions );
		DList_AppendList( self->compilingFlags, other->compilingFlags );
		DList_AppendList( self->linkingFlags, other->linkingFlags );
	}else if( self->ctype != daomake_type_target && self->ctype != daomake_type_project ){
	}else if( other->ctype == daomake_type_target ){
		DaoMakeTarget *tar = (DaoMakeTarget*) other;
		DString *flags = DaoMakeProject_GetBufferString( self->project );
		DString *rpath = DaoMake_GetSettingValue( "DLL-RPATH" );
		DString *name = tar->name;
		DString *flag;

		DList_AppendPaths( self->includePaths, other->includePaths, other->sourcePath );
		if( import && DaoMap_GetValueChars( daomake_platforms, "WIN32" ) == NULL ){
			DList_AppendList( self->linkingFlags, other->linkingFlags );
			return;
		}
		if( tar->ttype == DAOMAKE_STATICLIB ){
			if( name->size ){
				DString *prefix = DaoMake_GetSettingValue( daomake_prefix_keys[ tar->ttype ] );
				DString *suffix = DaoMake_GetSettingValue( daomake_suffix_keys[ tar->ttype ] );
				flag = (DString*) DList_PushBack( self->linkingFlags, name );
				if( prefix != NULL ) DString_Insert( flag, prefix, 0, 0, 0 );
				DString_Append( flag, suffix );
				DaoMake_MakePath( tar->base.binaryPath, flag );
			}
		}else if( tar->ttype == DAOMAKE_SHAREDLIB ){
			if( tar->install->size && ! DString_EQ( tar->install, tar->base.binaryPath ) ){
				if( rpath->size ){
					flag = (DString*) DList_PushBack( self->linkingFlags, rpath );
					DString_Assign( flags, tar->install );
					if( daomake_relative_rpath ){
						DaoMake_MakeRelativePath( self->binaryPath, flags );
					}
					DString_Append( flag, flags );
				}
			}
			if( daomake_local_rpath ){
				DList_PushBack( self->linkingPaths, tar->base.binaryPath );
			}else if( rpath->size ){
				flag = (DString*) DList_PushBack( self->linkingFlags, tar->base.binaryPath );
				if( daomake_relative_rpath ){
					DaoMake_MakeRelativePath( self->buildPath, flag );
				}
				DString_InsertChars( flag, "-L", 0, 0, 0 );
			}
			flag = (DString*) DList_PushBack( self->linkingFlags, name );
			if( name->size ) DString_InsertChars( flag, "-l", 0, 0, 0 );
		}
		DList_AppendList( self->linkingFlags, other->linkingFlags );
		self->project->usedStrings -= 1;
	}
}

static void DaoMakeTarget_SetTargetPath( DaoMakeTarget *self, DString *dest )
{
	DString_Assign( self->path, dest );
	DString_Assign( self->base.binaryPath, dest );
	DaoMake_MakePath( self->base.buildPath, self->base.binaryPath );
	if( self->objects->size ) DaoMake_MakeDirs( self->base.binaryPath, 0 );
}




DString* DaoMakeProject_SubMD5( DaoMakeProject *self, DString *data )
{
	DNode *it;
	DString *md5 = self->string;
	DString_MD5( data, md5 );
	DString_ToUpper( md5 );
	DString_Reset( md5, self->signature );
	it = DMap_Find( self->signatures, md5 );
	if( it ){
		if( DString_EQ( data, it->value.pString ) == 0 ) self->signature += 2;
	}else{
		DMap_Insert( self->signatures, md5, data );
	}
	return md5;
}


/* Return macro name: */
DString* DaoMakeProject_MakeHeaderMacro( DaoMakeProject *self, DaoMakeObjects *objects )
{
	DString *file = DaoMakeProject_GetBufferString( self );
	DString *files = DaoMakeProject_GetBufferString( self );
	DString *macro = DaoMakeProject_GetBufferString( self );
	DString *md5 = self->string;
	DNode *it;
	daoint i;

	DString_Reset( files, 0 );
	for(i=0; i<objects->headers->size; ++i){
		DString_AppendGap( files );
		DString_Reset( file, 0 );
		DString_Append( file, objects->headers->items.pString[i] );
		DaoMakeProject_MakeSourcePath( self, file );
		DaoMake_MakeRelativePath( self->base.buildPath, file );
		DString_Append( files, file );
	}
	md5 = DaoMakeProject_SubMD5( self, files );

	DString_Reset( macro, 0 );
	DString_AppendChars( macro, "HEADERS_" );
	DString_Append( macro, md5 );

	it = DMap_Find( self->headerMacros, macro );
	if( it ){
		self->usedStrings -= 2;
		return it->key.pString;
	}

	it = DMap_Insert( self->headerMacros, macro, macro );
	DString_AppendChars( it->value.pString, " = " );
	DString_Append( it->value.pString, files );

	self->usedStrings -= 2;
	return it->key.pString;
}

DString* DaoMakeProject_MakeSimpleMacro( DaoMakeProject *self, DMap *macros, DString *value, const char *prefix )
{
	DString *name = DaoMakeProject_GetBufferString( self );
	DString *md5 = DaoMakeProject_SubMD5( self, value );
	DNode *it;

	DString_Reset( name, 0 );
	DString_AppendChars( name, prefix );
	DString_AppendChar( name, '_' );
	DString_Append( name, md5 );

	it = DMap_Find( macros, name );
	if( it ){
		self->usedStrings -= 1;
		return it->key.pString;
	}

	it = DMap_Insert( macros, name, name );
	DString_AppendChars( it->value.pString, " =" );
	DString_Append( it->value.pString, value );
	self->usedStrings -= 1;
	return it->key.pString;
}
DString* DaoMakeProject_MakeCFlagsMacro( DaoMakeProject *self, DString *flags )
{
	return DaoMakeProject_MakeSimpleMacro( self, self->cflagsMacros, flags, "CFLAGS" );
}
DString* DaoMakeProject_MakeLFlagsMacro( DaoMakeProject *self, DString *flags )
{
	return DaoMakeProject_MakeSimpleMacro( self, self->lflagsMacros, flags, "LFLAGS" );
}
DString* DaoMakeProject_MakeAFlagsMacro( DaoMakeProject *self, DString *flags )
{
	return DaoMakeProject_MakeSimpleMacro( self, self->aflagsMacros, flags, "AFLAGS" );
}

const char* DaoMakeProject_GetFileExtension( DString *file )
{
	daoint pos = DString_RFindChar( file, '.', -1 );
	if( pos == DAO_NULLPOS ) return "";
	return file->chars + pos;
}
DString* DaoMakeProject_GetProgramMacro( DaoMap *map, DString *file )
{
	DaoValue *value;
	const char *ext = DaoMakeProject_GetFileExtension( file );
	if( ext == NULL ) return NULL;
	value = DaoMap_GetValueChars( map, ext );
	if( value == NULL ) return NULL;
	return DaoValue_TryGetString( value );
}
DString* DaoMakeProject_GetLanguageAssembler( DString *file )
{
	return DaoMakeProject_GetProgramMacro( daomake_assemblers, file );
}
DString* DaoMakeProject_GetLanguageCompiler( DString *file )
{
	return DaoMakeProject_GetProgramMacro( daomake_compilers, file );
}
DString* DaoMakeProject_GetLanguageLinker( DString *file )
{
	return DaoMakeProject_GetProgramMacro( daomake_linkers, file );
}

/* Return object file name: */
DString* DaoMakeProject_MakeObjectRule( DaoMakeProject *self, DaoMakeTarget *target, DaoMakeObjects *objects, DString *source )
{
	DString *source2 = DaoMakeProject_GetBufferString( self );
	DString *cflags = DaoMakeProject_GetBufferString( self );
	DString *cflag = DaoMakeProject_GetBufferString( self );
	DString *signature = DaoMakeProject_GetBufferString( self );
	DString *assembler = DaoMakeProject_GetLanguageAssembler( source );
	DString *compiler = DaoMakeProject_GetLanguageCompiler( source );
	DString *md5 = self->string;
	DString *mode;
	DNode *it;
	daoint pos;

	DString_Reset( cflags, 0 );
	DString_Append( source2, source );
	DaoMakeProject_MakeSourcePath( self, source2 );
	DaoMake_MakeRelativePath( self->base.buildPath, source2 );

	if( assembler ){
		mode = DaoMake_GetSettingValue( daomake_mode_keys[ 3*daomake_build_mode ] );
		if( mode ) DString_Append( cflags, mode );
	}else{
		mode = DaoMake_GetSettingValue( daomake_mode_keys[ 3*daomake_build_mode+1 ] );
		if( mode ) DString_Append( cflags, mode );
	}
	DaoMakeProject_FormatDefs( objects->base.project, cflags );
	DaoMakeObjects_FormatDefs( objects, cflags );

	DaoMakeProject_FormatIPaths( objects->base.project, cflags );
	DaoMakeObjects_FormatIPaths( objects, cflags );

	DaoMakeProject_FormatCFlags( objects->base.project, cflags );
	DaoMakeObjects_FormatCFlags( objects, cflags );

	DString_Assign( signature, cflags );
	md5 = DaoMakeProject_SubMD5( self, signature );

	DString_AppendGap( signature );
	DString_Append( signature, source );

	/* Unique (quasi) target name: */
	DString_Reset( signature, 0 );
	DString_Append( signature, source );
	pos = DString_RFindChar( signature, '/', -1 );
	if( pos != DAO_NULLPOS ) DString_Erase( signature, 0, pos + 1 );
	DString_InsertChars( signature, "DaoMake.Objs/", 0, 0, 0 );
	DString_AppendChar( signature, '.' );
	DString_Append( signature, md5 );
	if( target->ttype == DAOMAKE_JAVASCRIPT ) DString_AppendChars( signature, ".ll" );
	DString_AppendChars( signature, ".o" );

	it = DMap_Find( self->objectRules, signature );
	if( it ){
		self->usedStrings -= 4;
		return it->key.pString;
	}

	it = DMap_Insert( self->objectRules, signature, signature );
	DString_AppendChars( it->value.pString, ": " );
	DString_Append( it->value.pString, source2 );
	DString_AppendChars( it->value.pString, " $(" );
	DString_Append( it->value.pString, DaoMakeProject_MakeHeaderMacro( self, objects ) );
	DString_AppendChars( it->value.pString, ")\n\t$(" );
	if( target->ttype == DAOMAKE_JAVASCRIPT ) DString_AppendChars( it->value.pString, "EM" );
	if( assembler ){
		DString_Append( it->value.pString, assembler );
		/* #include main appear in .S file: */
		DString_AppendChars( it->value.pString, ") $(" );
		DString_Append( it->value.pString, DaoMakeProject_MakeAFlagsMacro( self, cflags ) );
		DString_AppendChars( it->value.pString, ") -c " );
	}else{
		if( compiler ){
			DString_Append( it->value.pString, compiler );
		}else{
			DString_AppendChars( it->value.pString, "CC" );
		}
		DString_AppendChars( it->value.pString, ") $(" );
		DString_Append( it->value.pString, DaoMakeProject_MakeCFlagsMacro( self, cflags ) );
		DString_AppendChars( it->value.pString, ") -c " );
	}
	DString_Append( it->value.pString, source2 );
	DString_AppendChars( it->value.pString, " -o " );
	DString_Append( it->value.pString, signature );

	self->usedStrings -= 4;
	return it->key.pString;
}

/* Return objects macro name: */
DString* DaoMakeProject_MakeObjectsMacro( DaoMakeProject *self, DaoMakeTarget *target, DaoMakeObjects *objects )
{
	DString *objs = DaoMakeProject_GetBufferString( self );
	daoint i;

	DString_Reset( objs, 0 );
	for(i=0; i<objects->sources->size; ++i){
		DString *source = objects->sources->items.pString[i];
		DString *obj = DaoMakeProject_MakeObjectRule( self, target, objects, source );
		DString_AppendGap( objs );
		DString_Append( objs, obj );
	}
	objs = DaoMakeProject_MakeSimpleMacro( self, self->objectsMacros, objs, "OBJECTS" );
	self->usedStrings -= 1;
	return objs;
}

void DaoMakeTarget_MakeName( DaoMakeTarget *self, DString *name, int full )
{
	DString *prefix = DaoMake_GetSettingValue( daomake_prefix_keys[ self->ttype ] );
	DString *suffix = DaoMake_GetSettingValue( daomake_suffix_keys[ self->ttype ] );

	DString_Reset( name, 0 );
	if( prefix ) DString_Append( name, prefix );
	DString_Append( name, self->name );
	if( suffix ) DString_Append( name, suffix );
	if( self->ttype == DAOMAKE_JAVASCRIPT ) DString_AppendChars( name, ".js" );
	if( full == 0 || self->ttype == DAOMAKE_DIRECTORY ) return;
	DaoMake_MakePath( self->base.binaryPath, name );
	DaoMake_MakeRelativePath( self->base.buildPath, name );
}
void DaoMakeProject_MakeDependency( DaoMakeProject *self, DaoMakeTarget *target, DString *deps )
{
	DString *tname = DaoMakeProject_GetBufferString( self );
	daoint i;
	for(i=0; i<target->depends->size; ++i){
		DaoMakeTarget *t = (DaoMakeTarget*) target->depends->items.pVoid[i];
		DaoMakeTarget_MakeName( t, tname, 1 );
		if( t->ttype <= DAOMAKE_STATICLIB ) DaoMake_MakePath( t->base.buildPath, tname );
		DString_AppendGap( deps );
		DString_Append( deps, tname );
	}
	DString_AppendGap( deps );
	DString_Append( deps, target->extradeps );
	self->usedStrings -= 1;
}
DString* DaoMakeProject_MakeTargetRule( DaoMakeProject *self, DaoMakeTarget *target )
{
	DString *tname = DaoMakeProject_GetBufferString( self );
	DString *tname2 = DaoMakeProject_GetBufferString( self );
	DString *deps = DaoMakeProject_GetBufferString( self );
	DString *lflags = DaoMakeProject_GetBufferString( self );
	DString *lflag = DaoMakeProject_GetBufferString( self );
	DString *signature = DaoMakeProject_GetBufferString( self );
	DString *rule = DaoMakeProject_GetBufferString( self );
	DString *objs = DaoMakeProject_GetBufferString( self );
	DString *linker = DaoMakeProject_GetBufferString( self );
	DString *macro, *mode;
	DNode *it, *lk = NULL;
	daoint i, j, objCount = 0;

	DaoMake_MakeObjectDir( self->base.sourcePath );
	DaoMakeTarget_MakeName( target, tname, 1 );
	DaoMakeTarget_MakeName( target, tname2, 0 );
	DaoMakeProject_MakeDependency( self, target, deps );
	DString_Reset( rule, 0 );
	DString_Append( rule, tname );
	DString_AppendChars( rule, ": " );
	DString_Append( rule, deps );
	DString_AppendGap( rule );

	if( target->ttype >= DAOMAKE_COMMAND ){
		DString *dir = DaoMakeProject_GetBufferString( self );
		DString_AppendChars( rule, "\n" );
		for(i=0; i<target->commands->size; ++i){
			DString *cmd = target->commands->items.pString[i];
			if( target->ttype == DAOMAKE_DIRECTORY ){
				DString_Reset( dir, 0 );
				DString_Append( dir, cmd );
				DaoMakeProject_MakeSourcePath( self, dir );
				if( DMap_Find( daomake_makefile_paths, dir ) == NULL ) continue;
			}
			DString_AppendChar( rule, '\t' );
			if( target->ttype == DAOMAKE_COMMAND ){
				DString_Append( rule, cmd );
			}else if( target->ttype == DAOMAKE_DIRECTORY ){
				DString_AppendChars( rule, "cd " );
				DString_Append( rule, cmd );
				DString_AppendChars( rule, " && $(MAKE) -f Makefile" );
				DString_AppendChars( rule, daomake_makefile_suffix );
			}
			DString_AppendChar( rule, '\n' );
		}
		self->usedStrings -= 7;
		DList_Append( self->targetRules, tname );
		DList_Append( self->targetRules, rule );
		return tname;
	}else if( target->ttype == DAOMAKE_TESTING ){
		DString_Reset( objs, 0 );
		for(i=0; i<target->tests->size; ++i){
			DString *md5, *test = target->tests->items.pString[i];
			md5 = DaoMakeProject_SubMD5( self, test );
			DString_Reset( signature, 0 );
			DString_AppendChars( signature, daomake_objects_dir );
			DString_AppendPathSep( signature);
			DString_Append( signature, test );
			DString_AppendChar( signature, '.' );
			DString_Append( signature, md5 );
			DString_AppendChars( signature, ".test" );

			DString_AppendGap( objs );
			DString_Append( objs, signature );

			it = DMap_Find( self->testRules, signature );
			if( it ) continue;

			DString_Reset( rule, 0 );
			DString_Append( rule, test );
			DaoMake_MakePath( target->base.sourcePath, rule );

			it = DMap_Insert( self->testRules, signature, signature );
			DString_AppendChars( it->value.pString, ": " );
			DString_Append( it->value.pString, rule );
			DaoMakeProject_MakeDependency( self, target, it->value.pString );
			DString_AppendChars( it->value.pString, "\n\t-$(DAOTEST) " );
			DString_Append( it->value.pString, rule );
			DString_AppendGap( it->value.pString );
			DString_Append( it->value.pString, daomake_test_tool_option );
			DString_AppendGap( it->value.pString );
			DString_Append( it->value.pString, signature );
		}
		DString_Reset( lflag, 0 );
		DString_Append( lflag, target->name );
		DString_ToUpper( lflag );
		macro = DaoMakeProject_MakeSimpleMacro( self, self->testsMacros, objs, lflag->chars );

		DString_Reset( target->testMacro, 0 );
		DString_Append( target->testMacro, macro );

		DString_Reset( rule, 0 );
		DString_Append( rule, target->name );
		DString_AppendChars( rule, ": $(" );
		DString_Append( rule, macro );
		DString_AppendChars( rule, ")" );

		DList_Append( self->targetRules, target->name );
		DList_Append( self->targetRules, rule );
		self->usedStrings -= 7;
		return tname;
	}

	mode = DaoMake_GetSettingValue( daomake_mode_keys[ 3*daomake_build_mode+2 ] );
	if( mode ) DString_Append( lflags, mode );

	DaoMakeProject_FormatLPaths( target->base.project, lflags, target );
	DaoMakeTarget_FormatLPaths( target, lflags );

	DaoMakeProject_FormatLFlags( target->base.project, lflags );
	DaoMakeTarget_FormatLFlags( target, lflags );

	DString_Reset( objs, 0 );
	DMap_Reset( self->mapStringInt );
	for(i=0; i<target->objects->size; ++i){
		DaoMakeObjects *objects = (DaoMakeObjects*) target->objects->items.pVoid[i];
		DString *objmacro = DaoMakeProject_MakeObjectsMacro( self, target, objects );

		objCount += objects->sources->size;
		/* Find common linkers: */
		for(j=0; j<objects->sources->size; ++j){
			DString *source = objects->sources->items.pString[j];
			DString *linkers = DaoMakeProject_GetLanguageLinker( source );
			daoint pos, start = 0;
			pos = DString_FindChar( linkers, ';', start );
			while( start < linkers->size ){
				if( pos == DAO_NULLPOS ) pos = linkers->size;
				DString_SubString( linkers, linker, start, pos - start );
				it = DMap_Find( self->mapStringInt, linker );
				if( it == NULL ) it = DMap_Insert( self->mapStringInt, linker, 0 );
				it->value.pInt += 1;
				start = pos + 1;
				pos = DString_FindChar( linkers, ';', start );
			}
		}

		DString_AppendGap( objs );
		DString_AppendChars( objs, "$(" );
		DString_Append( objs, objmacro );
		DString_AppendChar( objs, ')' );
	}
	DString_Append( rule, objs );
	if( target->ttype == DAOMAKE_STATICLIB ){
		DString *arc = DaoMake_GetSettingValue( "AR" );
		DString_AppendChars( rule, "\n\t" );
		DString_AppendChars( rule, "-@$(DAOMAKE) remove " );
		DString_Append( rule, tname );
		DString_AppendChars( rule, "\n\t" );

		if( arc ){
			DString_Append( rule, arc );
			DString_AppendGap( rule );
			DString_Append( rule, tname );
			DString_AppendGap( rule );
			DString_Append( rule, objs );
		}
	}else{
		for(it=lk=DMap_First(self->mapStringInt); it; it=DMap_Next(self->mapStringInt,it)){
			if( it->value.pInt > lk->value.pInt ) lk = it;
		}
		if( target->ttype == DAOMAKE_EXECUTABLE ){
			DString *flag = DaoMake_GetSettingValue( "EXE-FLAG" );
			if( flag ){
				DString_AppendGap( lflags );
				DString_Append( lflags, flag );
			}
		}else if( target->ttype == DAOMAKE_SHAREDLIB ){
			DString *flag = DaoMake_GetSettingValue( "DLL-FLAG" );
			DString *flag2 = DaoMake_GetSettingValue( "DLL-NAME" );
			if( flag ){
				DString_AppendGap( lflags );
				DString_Append( lflags, flag );
			}
			if( flag2 ){
				DString_AppendGap( lflags );
				DString_Append( lflags, flag2 );
				DString_Append( lflags, tname2 );
			}
		}
		if( target->dynamicExporting ){
			DString *flag = DaoMake_GetSettingValue( "DYNAMIC-EXPORT" );
			if( flag ){
				DString_AppendGap( lflags );
				DString_Append( lflags, flag );
			}
		}
		if( target->dynamicLinking ){
			DString *flag = DaoMake_GetSettingValue( "DYNAMIC-IMPORT" );
			if( flag ){
				DString_AppendGap( lflags );
				DString_Append( lflags, flag );
			}
		}
		macro = DaoMakeProject_MakeLFlagsMacro( self, lflags );
		DString_AppendChars( rule, "\n\t$(" );
		if( target->ttype == DAOMAKE_JAVASCRIPT ) DString_AppendChars( rule, "EM" );
		if( lk && lk->value.pInt == objCount ){
			DString_Append( rule, lk->key.pString );
		}else{
			DString_AppendChars( rule, "CC" );
		}
		DString_AppendChars( rule, ") " );

		DString_Append( rule, objs );

		DString_AppendChars( rule, " $(" );
		DString_Append( rule, macro );
		DString_AppendChars( rule, ") " );

		DString_AppendChars( rule, " -o " );
		DString_Append( rule, tname );
	}

	DList_Append( self->targetRules, tname );
	DList_Append( self->targetRules, rule );
	self->usedStrings -= 8;
	return self->targetRules->items.pString[self->targetRules->size-2];
}

void DaoMakeProject_MakeInstallPath( DaoMakeProject *self, DString *path, DString *install, DString *uninstall, int top )
{
	if( DaoMake_IsDir( path->chars ) ) return;

	DString_AppendChars( install, "\t$(DAOMAKE) mkdir3 " );
	DString_Append( install, path );
	DString_AppendChar( install, '\n' );

	/*
	// It is not reliable to use the existence of directory to determine
	// whether to remove the directory upon uninstallation.
	// Because the path may contain variables that can only be resolved
	// at running time.
	//
	// TODO: add Uninstall() method for explicit removing directories.
	*/

#if 0
	if( top ){
		DString_AppendChars( uninstall, "\t$(DAOMAKE) remove " );
		DString_Append( uninstall, path );
		DString_AppendChar( uninstall, '\n' );
	}
#endif
}
void DaoMakeProject_MakeCopy( DaoMakeProject *self, DString *src, DString *dest, DString *output )
{
	DString_AppendChars( output, "\t$(DAOMAKE) copy " );
	DString_Append( output, src );
	DString_AppendChar( output, ' ' );
	DString_Append( output, dest );
	DString_AppendChar( output, '\n' );
}
void DaoMakeProject_MakeRemove( DaoMakeProject *self, DString *file, DString *path, DString *output )
{
	DString_AppendChars( output, "\t$(DAOMAKE) remove " );
	DString_Append( output, path );
	if( file ){
		daoint pos;
		char *file2;
		pos = DString_RFindChar( file, '/', -1 );
		file2 = pos == DAO_NULLPOS ? file->chars : file->chars + pos + 1;
		DString_AppendPathSep( output );
		DString_AppendChars( output, file2 );
	}
	DString_AppendChar( output, '\n' );
}
void DaoMakeProject_MakeDirectoryMake( DaoMakeProject *self, DString *makefile, const char *mktarget )
{
	DString *dir = DaoMakeProject_GetBufferString( self );
	daoint i, j;
	for(i=0; i<self->targets->size; ++i){
		DaoMakeTarget *target = (DaoMakeTarget*) self->targets->items.pVoid[i];
		if( target->ttype != DAOMAKE_DIRECTORY ) continue;
		for(j=0; j<target->commands->size; ++j){
			DString *subdir = target->commands->items.pString[j];
			DString_Reset( dir, 0 );
			DString_Append( dir, subdir );
			DaoMakeProject_MakeSourcePath( self, dir );
			if( DMap_Find( daomake_makefile_paths, dir ) == NULL ) continue;
			DString_AppendChars( makefile, "\tcd " );
			DString_Append( makefile, subdir );
			DString_AppendChars( makefile, " && $(MAKE) -f Makefile" );
			DString_AppendChars( makefile, daomake_makefile_suffix );
			DString_AppendChar( makefile, ' ' );
			DString_AppendChars( makefile, mktarget );
			DString_AppendChar( makefile, '\n' );
		}
	}
	self->usedStrings -= 1;
}
void DaoMakeProject_MakeInstallation( DaoMakeProject *self, DString *makefile )
{
	DString *tname = DaoMakeProject_GetBufferString( self );
	DString *install = DaoMakeProject_GetBufferString( self );
	DString *uninstall = DaoMakeProject_GetBufferString( self );
	DString *file = DaoMakeProject_GetBufferString( self );
	daoint i;

	for(i=0; i<self->installs->size; i+=2){
		DString *file = DaoValue_TryGetString( self->installs->items.pValue[i] );
		DaoMakeTarget *target = (DaoMakeTarget*) self->installs->items.pVoid[i];
		DString *path = DaoValue_TryGetString( self->installs->items.pValue[i+1] );
		DaoMakeProject_MakeSourcePath( self, path );
		if( file ){
			DString_Assign( tname, file );
			DaoMakeProject_MakeSourcePath( self, tname );
		}else{
			DaoMakeTarget_MakeName( target, tname, 1 );
		}
		if( DaoMake_IsDir( path->chars ) == 0 ){
			DaoMakeProject_MakeInstallPath( self, path, install, uninstall, 1 );
		}
		DaoMakeProject_MakeRemove( self, tname, path, uninstall );
		DaoMake_MakeRelativePath( self->base.buildPath, tname );
		DaoMakeProject_MakeCopy( self, tname, path, install );
	}
	self->usedStrings -= 4;
	DaoMakeProject_MakeDirectoryMake( self, install, "install" );
	DaoMakeProject_MakeDirectoryMake( self, uninstall, "uninstall" );
	DString_AppendChars( makefile, "install:\n" );
	DString_Append( makefile, install );
	DString_AppendChars( makefile, "\n\n" );

	DString_AppendChars( makefile, "uninstall:\n" );
	DString_Append( makefile, uninstall );
	DString_AppendChars( makefile, "\n\n" );
	DString_AppendChars( makefile, ".PHONY: install uninstall\n\n" );
}
void DaoMakeProject_MakeFile( DaoMakeProject *self, DString *makefile )
{
	DNode *it;
	daoint ismain = DString_EQ( self->base.sourcePath, daomake_main_source_path );
	daoint i, j, sig = self->signature;

	self->usedStrings = 0;
	DMap_Reset( self->headerMacros );
	DMap_Reset( self->cflagsMacros );
	DMap_Reset( self->lflagsMacros );
	DMap_Reset( self->objectRules );
	DMap_Reset( self->objectsMacros );
	DMap_Reset( self->signatures );
	DList_Clear( self->targetRules );
	DString_Reset( makefile, 0 );
	if( (self->targets->size + self->installs->size) == 0 ) return;

	DString_AppendChars( makefile, "\nDAOMAKE_SOURCE_DIR = " );
	DString_Append( makefile, self->base.sourcePath );

	DString_AppendChars( makefile, "\nDAOMAKE_BUILD_DIR  = " );
	DString_Append( makefile, self->base.buildPath );
	DString_AppendChars( makefile, "\n" );

	for(i=0; i<daomake_variable_list->size; ++i){
		if( i%3 == 0 ) DString_AppendChars( makefile, "\n" );
		DString_Append( makefile, daomake_variable_list->items.pString[i] );
	}
	DString_AppendChars( makefile, "\n\n" );

	if( 1 ){
		DString *all = DaoMakeProject_GetBufferString( self );
		DString *phony = DaoMakeProject_GetBufferString( self );
		DString *test = DaoMakeProject_GetBufferString( self );
		DString *testsum = DaoMakeProject_GetBufferString( self );
		for(i=0; i<self->targets->size; ++i){
			DaoMakeTarget *target = (DaoMakeTarget*) self->targets->items.pVoid[i];
			DString *ruleName = DaoMakeProject_MakeTargetRule( self, target );
			if( target->ttype == DAOMAKE_TESTING ){
				DString_AppendGap( test );
				DString_Append( test, ruleName );
				continue;
			}
			DString_AppendGap( all );
			DString_Append( all, ruleName );
			DString_Trim( ruleName, 1, 1, 0 );
			if( target->ttype == DAOMAKE_COMMAND && DString_Match( ruleName, "%W", NULL, NULL ) ==0 ){
				DString_AppendGap( phony );
				DString_Append( phony, ruleName );
			}else if( target->ttype >= DAOMAKE_DIRECTORY ){
				DString_AppendGap( phony );
				DString_Append( phony, ruleName );
			}
		}
		DString_AppendChars( makefile, "all: " );
		DString_Append( makefile, all );
		if( ismain && daomake_test_count && daomake_build_mode ){
			DString_AppendGap( makefile );
			DString_AppendChars( makefile, "test" );
		}
		DString_AppendChars( makefile, "\n\n" );

		DString_AppendChars( makefile, ".PHONY: test\n" );
		DString_AppendChars( makefile, "test: " );
		DString_Append( makefile, all );
		DString_AppendGap( makefile );
		DString_Append( makefile, test );

		DString_AppendGap( makefile );
		DString_AppendChars( makefile, "subtest" );
		DString_AppendGap( makefile );
		if( ismain && daomake_test_count ){
			DString_AppendChars( makefile, "testsum" );
		}
		DString_AppendChars( makefile, "\n\n" );
		if( phony->size ){
			DString_AppendChars( makefile, ".PHONY: " );
			DString_Append( makefile, phony );
			DString_AppendChar( makefile, '\n' );
		}
		self->usedStrings -= 4;
	}

	DString_AppendChars( makefile, "\nDAOMAKE = " );
	DString_Append( makefile, vmSpace->daoBinPath );
	DString_AppendPathSep( makefile );
	DString_AppendChars( makefile, "daomake\n\n" );

	for(i=0; i<self->variables->size; i+=3){
		DString_Append( makefile, self->variables->items.pString[i] );
		DString_Append( makefile, self->variables->items.pString[i+2] );
		DString_Append( makefile, self->variables->items.pString[i+1] );
		DString_AppendChar( makefile, '\n' );
	}
	DString_AppendChar( makefile, '\n' );

	for(it=DMap_First(self->headerMacros); it; it=DMap_Next(self->headerMacros,it)){
		DString_Append( makefile, it->value.pString );
		DString_AppendChar( makefile, '\n' );
	}
	DString_AppendChar( makefile, '\n' );

	for(it=DMap_First(self->aflagsMacros); it; it=DMap_Next(self->aflagsMacros,it)){
		DString_Append( makefile, it->value.pString );
		DString_AppendChars( makefile, "\n\n" );
	}
	DString_AppendChar( makefile, '\n' );

	for(it=DMap_First(self->cflagsMacros); it; it=DMap_Next(self->cflagsMacros,it)){
		DString_Append( makefile, it->value.pString );
		DString_AppendChars( makefile, "\n\n" );
	}
	DString_AppendChar( makefile, '\n' );

	for(it=DMap_First(self->lflagsMacros); it; it=DMap_Next(self->lflagsMacros,it)){
		DString_Append( makefile, it->value.pString );
		DString_AppendChars( makefile, "\n\n" );
	}
	DString_AppendChar( makefile, '\n' );

	for(it=DMap_First(self->objectRules); it; it=DMap_Next(self->objectRules,it)){
		DString_Append( makefile, it->value.pString );
		DString_AppendChars( makefile, "\n\n" );
	}
	DString_AppendChar( makefile, '\n' );

	for(it=DMap_First(self->objectsMacros); it; it=DMap_Next(self->objectsMacros,it)){
		DString_Append( makefile, it->value.pString );
		DString_AppendChars( makefile, "\n\n" );
	}

	if( self->testRules->size ){
		DString *suffix = DaoMake_GetSettingValue( daomake_suffix_keys[ DAOMAKE_EXECUTABLE ] );
		DString_AppendChars( makefile, "DAOTEST = " );
		DString_Append( makefile, daomake_test_tool );
		DString_Append( makefile, suffix );
		DString_AppendChars( makefile, "\n\n" );
	}

	for(it=DMap_First(self->testRules); it; it=DMap_Next(self->testRules,it)){
		DString_Append( makefile, it->value.pString );
		DString_AppendChars( makefile, "\n\n" );
	}
	DString_AppendChar( makefile, '\n' );

	for(it=DMap_First(self->testsMacros); it; it=DMap_Next(self->testsMacros,it)){
		DString_Append( makefile, it->value.pString );
		DString_AppendChars( makefile, "\n\n" );
	}

	for(i=0; i<self->targetRules->size; i+=2){
		DString_Append( makefile, self->targetRules->items.pString[i+1] );
		DString_AppendChars( makefile, "\n\n" );
	}

	DString_AppendChars( makefile, "subtest:\n" );
	DaoMakeProject_MakeDirectoryMake( self, makefile, "test" );
	DString_AppendChar( makefile, '\n' );
	DString_AppendChars( makefile, ".PHONY: subtest\n\n" );

	DString_AppendChars( makefile, "TESTSUM =" );
	DString_Append( makefile, vmSpace->startPath );
	DString_AppendPathSep( makefile );
	DString_AppendChars( makefile, daomake_test_sumfile );
	DString_AppendChars( makefile, "\n\n" );

	DString_AppendChars( makefile, "testsum:\n" );
	if( ismain ){
		DString_AppendChars( makefile, "\t@$(DAOMAKE) echo \"Summarizing test results ...\"\n" );
		DString_AppendChars( makefile, "\t-@$(DAOMAKE) remove " );
		DString_AppendChars( makefile, " $(TESTSUM)\n" );
	}
	for(i=0; i<self->targets->size; ++i){
		DaoMakeTarget *target = (DaoMakeTarget*) self->targets->items.pVoid[i];
		if( target->ttype != DAOMAKE_TESTING ) continue;

		DString_AppendChars( makefile, "\t@$(DAOTEST) --sum $(" );
		DString_Append( makefile, target->testMacro );
		DString_AppendChars( makefile, ") --log $(TESTSUM) --group " );
		DString_Append( makefile, target->name );
		DString_AppendChars( makefile, "\n" );
	}
	DaoMakeProject_MakeDirectoryMake( self, makefile, "testsum" );
	if( ismain ) DString_AppendChars( makefile, "\t@$(DAOMAKE) cat $(TESTSUM)\n" );
	DString_AppendChar( makefile, '\n' );
	DString_AppendChars( makefile, ".PHONY: testsum\n\n" );

	DaoMakeProject_MakeInstallation( self, makefile );

	DString_AppendChars( makefile, "clean:\n\t" );
	if( self->objectsMacros->size + self->testsMacros->size ){
		DString_AppendChars( makefile, "$(DAOMAKE) remove" );
		for(it=DMap_First(self->objectsMacros); it; it=DMap_Next(self->objectsMacros,it)){
			DString_AppendGap( makefile );
			DString_AppendChars( makefile, "$(" );
			DString_Append( makefile, it->key.pString );
			DString_AppendChar( makefile, ')' );
		}
		for(it=DMap_First(self->testsMacros); it; it=DMap_Next(self->testsMacros,it)){
			DString_AppendGap( makefile );
			DString_AppendChars( makefile, "$(" );
			DString_Append( makefile, it->key.pString );
			DString_AppendChar( makefile, ')' );
		}
		DString_AppendChar( makefile, '\n' );
	}
	DaoMakeProject_MakeDirectoryMake( self, makefile, "clean" );
	DString_AppendChar( makefile, '\n' );
	DString_AppendChars( makefile, ".PHONY: clean\n\n" );

	DString_AppendChars( makefile, "distclean:\n\t" );
	if( self->objectsMacros->size + self->testsMacros->size ){
		DString_AppendChars( makefile, "$(DAOMAKE) remove " );
		DString_AppendChars( makefile, daomake_objects_dir );
		DString_AppendChar( makefile, '\n' );
	}
	DaoMakeProject_MakeDirectoryMake( self, makefile, "distclean" );
	DString_AppendChar( makefile, '\n' );
	DString_AppendChars( makefile, ".PHONY: distclean\n\n" );

	/* Regenerate if there was MD5 signature conflict: */
	if( self->signature != sig ) DaoMakeProject_MakeFile( self, makefile );
}

void DString_AppendVerbatim( DString *self, DString *text, DString *md5 )
{
	DString_MD5( text, md5 );
	DString_Reset( md5, 8 );
	DString_ToUpper( md5 );
	DString_AppendChars( self, "@[" );
	DString_Append( self, md5 );
	DString_AppendChars( self, "]" );
	DString_Append( self, text );
	DString_AppendChars( self, "@[" );
	DString_Append( self, md5 );
	DString_AppendChars( self, "]" );
}

void DaoMake_AddCallParam1( DString *output, const char *obj, const char *meth, DString *param )
{
	DString_Reserve( output, output->size + 50 + strlen( obj ) + strlen( meth ) + param->size );
	output->size += sprintf( output->chars + output->size, "%s.%s( \"%s\" );\n", obj, meth, param->chars );
}
void DaoMake_AddCallParam2( DString *output, const char *obj, const char *meth, DString *param1, DString *param2 )
{
	DString_Reserve( output, output->size + 50 + strlen( obj ) + strlen( meth ) + param1->size + param2->size );
	output->size += sprintf( output->chars + output->size, "%s.%s( \"%s\", \"%s\" );\n", obj, meth, param1->chars, param2->chars );
}
void DaoMakeUnit_MakeFinderCodes( DaoMakeUnit *self, const char *tarname, DString *output )
{
	DString *def = DString_New();
	int i;
	for(i=0; i<self->definitions->size; i+=2){
		DString *param1 = self->definitions->items.pString[i];
		DString *param2 = self->definitions->items.pString[i+1];
		DString_Assign( def, param2 );
		DString_Change( def, "([\\\"])", "\\%1", 0 );
		DaoMake_AddCallParam2( output, tarname, "AddDefinition", param1, def );
	}
	for(i=0; i<self->compilingFlags->size; ++i){
		DString *param = self->compilingFlags->items.pString[i];
		DaoMake_AddCallParam1( output, tarname, "AddCompilingFlag", param );
	}
	for(i=0; i<self->linkingFlags->size; ++i){
		DString *param = self->linkingFlags->items.pString[i];
		DaoMake_AddCallParam1( output, tarname, "AddLinkingFlag", param );
	}
	DString_Delete( def );
}
void DaoMakeUnit_MakeFinderCodes2( DaoMakeUnit *self, const char *tarname, DString *output )
{
	DString *path = DString_New();
	daoint i;
	for(i=0; i<self->includePaths->size; ++i){
		DString_Assign( path, self->includePaths->items.pString[i] );
		DaoMake_MakePath( self->sourcePath, path );
		DaoMake_AddCallParam1( output, tarname, "AddIncludePath", path );
	}
	for(i=0; i<self->linkingPaths->size; ++i){
		DString_Assign( path, self->linkingPaths->items.pString[i] );
		DaoMake_MakePath( self->sourcePath, path );
		DaoMake_AddCallParam1( output, tarname, "AddLinkingPath", path );
	}
	DString_Delete( path );
}
void DaoMakeTarget_MakeFindPackageForInstall( DaoMakeTarget *self, DString *output, int tab )
{
	DaoMakeProject *project = self->base.project;
	DaoValue **installs = project->installs->items.pValue;
	DString *tarname = DaoMakeProject_GetBufferString( project );
	DString *path = DaoMakeProject_GetBufferString( project );
	DMap *headers = DMap_New(DAO_DATA_STRING,0);
	DNode *it;
	daoint i, j;

	while( tab -- ) DString_AppendChar( tarname, '\t' );
	DString_Append( tarname, self->name );
	DString_AppendChars( tarname, self->ttype == DAOMAKE_SHAREDLIB ? "_dll" : "_lib" );

	DString_Reserve( output, output->size + 50 + 2*self->name->size );
	output->size += sprintf( output->chars + output->size,
			"%s = project.Add%sLibrary( \"%s\" );\n", tarname->chars,
			self->ttype == DAOMAKE_SHAREDLIB ? "Shared" : "Static", self->name->chars );

	DString_Reserve( output, output->size + 50 + self->name->size + self->base.binaryPath->size );
	output->size += sprintf( output->chars + output->size,
			"%s.SetTargetPath( \"%s\" );\n", tarname->chars,
			self->base.binaryPath->chars );

	for(i=0; i<self->objects->size; ++i){
		DaoMakeObjects *objs = (DaoMakeObjects*) DList_Item( self->objects, i );
		for(j=0; j<objs->headers->size; ++j){
			DMap_Insert( headers, DList_String( objs->headers, j ), 0 );
		}
	}
	for(i=0; i<project->installs->size; i+=2){
		DString *dest = DaoValue_TryGetString( installs[i+1] );
		DString *file = DaoValue_TryGetString( installs[i] );
		if( file == NULL ) continue;
		if( i ){
			DString *prev = DaoValue_TryGetString( installs[i-1] );
			if( DString_EQ( dest, prev ) ) continue;
		}
		/*
		// "dest" is added to "-I" compiling flags, only if:
		// 1. "file" is a header file to be installed to "dest"; Or,
		// 2. "file" is a directory, which contains one of the used header files;
		*/
		if( DMap_Find( headers, file ) == NULL ){
			if( DaoMake_IsDir( file->chars ) == 0 ) continue;
			/* Check if the directory contains one of the header files: */
			for(it=DMap_First(headers); it; it=DMap_Next(headers,it)){
				if( DString_Find( it->key.pString, file, 0 ) == 0 ) break;
			}
			if( it== NULL ) continue;
		}
		DString_Reset( path, 0 );
		DString_Append( path, dest );
		DaoMakeProject_MakeSourcePath( project, path );

		DaoMake_AddCallParam1( output, tarname->chars, "AddIncludePath", path );
	}
	DaoMakeUnit_MakeFinderCodes( (DaoMakeUnit*) self, tarname->chars, output );

	if( self->install->size ){
		DaoMake_AddCallParam1( output, tarname->chars, "AddLinkingPath", self->install );
	}

	project->usedStrings -= 2;
}
void DaoMakeTarget_MakeFindPackageForBuild( DaoMakeTarget *self, DString *output, int tab )
{
	DaoMakeProject *project = self->base.project;
	DString *tarname = DaoMakeProject_GetBufferString( project );
	DString *path = DaoMakeProject_GetBufferString( project );
	DNode *it;
	daoint i, j;

	while( tab -- ) DString_AppendChar( tarname, '\t' );
	DString_Append( tarname, self->name );
	DString_AppendChars( tarname, self->ttype == DAOMAKE_SHAREDLIB ? "_dll" : "_lib" );

	DString_Reserve( output, output->size + 50 + 2*self->name->size );
	output->size += sprintf( output->chars + output->size,
			"%s = project.Add%sLibrary( \"%s\" );\n", tarname->chars,
			self->ttype == DAOMAKE_SHAREDLIB ? "Shared" : "Static", self->name->chars );

	DString_Reserve( output, output->size + 50 + self->name->size + self->base.binaryPath->size );
	output->size += sprintf( output->chars + output->size,
			"%s.SetTargetPath( \"%s\" );\n", tarname->chars,
			self->base.binaryPath->chars );

	DaoMakeUnit_MakeFinderCodes( (DaoMakeUnit*) self, tarname->chars, output );
	DaoMakeUnit_MakeFinderCodes2( (DaoMakeUnit*) self, tarname->chars, output );

	if( self->objects->size ){ /* real target: */
		DaoMake_AddCallParam1( output, tarname->chars, "AddLinkingPath", self->base.binaryPath );
	}

	project->usedStrings -= 2;
}
void DaoMakeProject_MakeFindPackageForInstall( DaoMakeProject *self, DString *output, int tab )
{
	DaoValue **installs = self->installs->items.pValue;
	daoint i;

	for(i=0; i<self->installs->size; i+=2){
		DaoMakeTarget *tar = (DaoMakeTarget*) DaoValue_CastCstruct( installs[i], daomake_type_target );
		if( tar == NULL ) continue;
		if( tar->ttype != DAOMAKE_SHAREDLIB && tar->ttype != DAOMAKE_STATICLIB ) continue;
		if( tar->install->size == 0 ) continue;

		DaoMakeTarget_MakeFindPackageForInstall( tar, output, tab );

	}
}
void DaoMakeProject_MakeFindPackageForBuild( DaoMakeProject *self, DString *output, int tab )
{
	daoint i;

	DaoMakeUnit_MakeFinderCodes2( (DaoMakeUnit*) self, "\tproject", output );
	for(i=0; i<self->targets2->size; ++i){
		DaoMakeTarget *tar = (DaoMakeTarget*) self->targets2->items.pVoid[i];
		if( tar->ttype != DAOMAKE_SHAREDLIB && tar->ttype != DAOMAKE_STATICLIB ) continue;

		DaoMakeTarget_MakeFindPackageForBuild( tar, output, tab );
	}
}
static void DString_RepaceVariable( DString *self, DString *name, DString *value )
{
	daoint pos = DString_Find( self, name, 0 );
	daoint offset = 0;

	while( pos != DAO_NULLPOS ){
		DString_Insert( self, value, pos, name->size, value->size );
		offset += value->size - name->size;
		pos = DString_Find( self, name, offset );
	}
}
static void DaoMakeProject_ReplaceVariables( DaoMakeProject *self, DString *output )
{
	DString *name = DString_New();
	DNode *it;
	daoint i;

	for(it=DMap_First(daomake_variable_map2); it; it=DMap_Next(daomake_variable_map2,it)){
		DString_Reset( name, 0 );
		DString_AppendChars( name, "$(" );
		DString_Append( name, it->key.pString );
		DString_AppendChars( name, ")" );
		DString_RepaceVariable( output, name, it->value.pString );
	}
	for(i=0; i<self->variables->size; i+=3){
		DString_Reset( name, 0 );
		DString_AppendChars( name, "$(" );
		DString_Append( name, self->variables->items.pString[i] );
		DString_AppendChars( name, ")" );
		DString_RepaceVariable( output, name, self->variables->items.pString[i+1] );
	}
	DString_Delete( name );
}
void DaoMakeProject_MakeFindPackage( DaoMakeProject *self, DString *output, int caching )
{
	DString *filePath = DaoMakeProject_GetBufferString( self );
	DString *installPath = DaoMakeProject_GetBufferString( self );
	DString *find1 = DaoMakeProject_GetBufferString( self );
	DString *find2 = DaoMakeProject_GetBufferString( self );
	DaoValue **installs = self->installs->items.pValue;
	DString *md5 = self->string;
	DNode *it;
	daoint i;

	DString_Append( filePath, self->base.binaryPath );
	DString_AppendPathSep( filePath );
	DString_AppendChars( filePath, "Find" );
	DString_Append( filePath, self->projectName );
	DString_AppendChars( filePath, ".dao" );

	for(i=0; i<self->installs->size; i+=2){
		DString *dest = DaoValue_TryGetString( installs[i+1] );
		DString *file = DaoValue_TryGetString( installs[i] );
		if( file == NULL ) continue;
		if( DString_EQ( file, filePath ) ){
			DString_Append( installPath, dest );
			break;
		}
	}

	DString_Reset( output, 0 );
	DString_AppendChars( output, "project = DaoMake::Project( \"" );
	DString_Append( output, self->projectName );
	DString_AppendChars( output, "\" )\n" );

	for(i=0; i<daomake_variable_list->size; i+=3){
		DString_AppendChars( output, "DaoMake::Variables[\"" );
		DString_Append( output, daomake_variable_list->items.pString[i] );
		DString_AppendChars( output, "\",\"" );
		DString_Append( output, daomake_variable_list->items.pString[i+1] );
		DString_AppendChars( output, "\"] = \"" );
		DString_Append( output, daomake_variable_list->items.pString[i+2] );
		DString_AppendChars( output, "\"\n" );
	}

	DaoMakeUnit_MakeFinderCodes( (DaoMakeUnit*) self, "project", output );

	if( installPath->size == 0 || caching ){
		DaoMakeProject_MakeFindPackageForBuild( self, find1, 0 );
		DString_Append( output, find1 );
		DString_AppendChars( output, "\n" );
		DaoMakeProject_ReplaceVariables( self, output );
		if( find1->size == 0 ) DString_Reset( output, 0 );
		self->usedStrings -= 4;
		return;
	}
	DaoMakeProject_MakeFindPackageForInstall( self, find1, 1 );
	DaoMakeProject_MakeFindPackageForBuild( self, find2, 1 );

	DaoMakeProject_ReplaceVariables( self, installPath );

	DString_AppendChars( output, "if( project.SourcePath() == " );
	DString_AppendVerbatim( output, installPath, md5 );
	DString_AppendChars( output, " ){\n" );
	DString_Append( output, find1 );
	DString_AppendChars( output, "}else{\n" );
	DString_Append( output, find2 );
	DString_AppendChars( output, "}\n" );
	DaoMakeProject_ReplaceVariables( self, output );
	self->usedStrings -= 4;
	if( find1->size + find2->size == 0 ) DString_Reset( output, 0 );
}

void DaoMakeProject_MakeFinderPath( DaoMakeProject *self, DString *path )
{
	DString_Reset( path, 0 );
	DString_AppendChars( path, "Find" );
	DString_Append( path, self->projectName );
	DString_AppendChars( path, ".dao" );

	/*
	// Better to be placed in the same directory as the binary,
	// for easy guessing the location of the finder file given
	// the path of binary library target.
	//
	// It is necessary for a functional single file deployment mode
	// for building Dao applications that can link the module binaries
	// into the executable.
	*/
	DaoMake_MakePath( self->base.binaryPath, path );
}




static void DList_ImportStringList( DList *self, DaoList *list )
{
	int i, size = DaoList_Size( list );
	for(i=0; i<size; ++i){
		DaoValue *value = DaoList_GetItem( list, i );
		DList_Append( self, DaoValue_TryGetString( value ) );
	}
}


static void DaoMakeUnit_ImportPaths( DaoMakeUnit *self, DList *paths, DaoValue *p[], int N )
{
	int i;
	for(i=0; i<N; ++i){
		DString *path = DaoValue_TryGetString( p[i] );
		if( path == NULL ) continue;
		path = (DString*) DList_Append( paths, path );
	}
}

static void DList_ImportStringParameters( DList *self, DaoValue *p[], int N )
{
	int i;
	for(i=0; i<N; ++i){
		DString *path = DaoValue_TryGetString( p[i] );
		if( path ) DList_Append( self, path );
	}
}



static void UNIT_AddPlatformDefs( DaoProcess *proc, DaoValue *p[], int N )
{
	char buf[30];
	DNode *it;
	DString *value = DString_New();
	DaoMakeUnit *self = (DaoMakeUnit*) p[0];
	for(it=DaoMap_First(daomake_platforms); it; it=DaoMap_Next(daomake_platforms,it)){
		int dep = it->value.pValue->xInteger.value;
		sprintf( buf, "%i", dep );
		DString_SetChars( value, buf );
		DList_Append( self->definitions, it->key.pValue->xString.value );
		DList_Append( self->definitions, value );
	}
}
static void UNIT_AddDefinition( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeUnit *self = (DaoMakeUnit*) p[0];
	DList_Append( self->definitions, DaoValue_TryGetString( p[1] ) );
	DList_Append( self->definitions, DaoValue_TryGetString( p[2] ) );
}
static void UNIT_AddIncludePath( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeUnit *self = (DaoMakeUnit*) p[0];
	DaoMakeUnit_ImportPaths( self, self->includePaths, p+1, N-1 );
}
static void UNIT_AddLinkingPath( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeUnit *self = (DaoMakeUnit*) p[0];
	DaoMakeUnit_ImportPaths( self, self->linkingPaths, p+1, N-1 );
}
static void UNIT_AddCompilingFlag( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeUnit *self = (DaoMakeUnit*) p[0];
	DList_ImportStringParameters( self->compilingFlags, p+1, N-1 );
}
static void UNIT_AddLinkingFlag( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeUnit *self = (DaoMakeUnit*) p[0];
	DList_ImportStringParameters( self->linkingFlags, p+1, N-1 );
}
static void UNIT_AddRpath( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeUnit *self = (DaoMakeUnit*) p[0];
	DString *rpath = DaoMake_GetSettingValue( "DLL-RPATH" );
	int i;
	if( rpath->size == 0 ) return; /* TODO: WARNING; */
	for(i=1; i<N; ++i){
		DString *flag, *path = DaoValue_TryGetString( p[i] );
		if( path == NULL ) continue;
		flag = (DString*) DList_PushBack( self->linkingFlags, rpath );
		DString_Append( flag, path );
	}
}
static void UNIT_MakeDefinitions( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeUnit *self = (DaoMakeUnit*) p[0];
	DString *res = DaoProcess_PutChars( proc, "" );
	DaoMakeUnit_FormatDefs( self, res );
}
static void UNIT_MakeIncludePaths( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeUnit *self = (DaoMakeUnit*) p[0];
	DString *res = DaoProcess_PutChars( proc, "" );
	DaoMakeUnit_FormatIPaths( self, res );
}
static void UNIT_MakeLinkingPaths( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeUnit *self = (DaoMakeUnit*) p[0];
	DString *res = DaoProcess_PutChars( proc, "" );
	DaoMakeUnit_FormatLPaths( self, res, NULL );
}
static void UNIT_MakeCompilingFlags( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeUnit *self = (DaoMakeUnit*) p[0];
	DString *res = DaoProcess_PutChars( proc, "" );
	DaoMakeUnit_FormatDefs( self, res );
	DaoMakeUnit_FormatIPaths( self, res );
	DaoMakeUnit_FormatCFlags( self, res );
}
static void UNIT_MakeLinkingFlags( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeUnit *self = (DaoMakeUnit*) p[0];
	DString *res = DaoProcess_PutChars( proc, "" );
	DaoMakeUnit_FormatLPaths( self, res, NULL );
	DaoMakeUnit_FormatLFlags( self, res );
}
static void UNIT_MakeSourcePath( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeUnit *self = (DaoMakeUnit*) p[0];
	DString *path = p[1]->xString.value;
	DString *res = DaoProcess_PutString( proc, path );
	DaoMake_MakePath( self->sourcePath, res );
}
static void UNIT_MakeBuildPath( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeUnit *self = (DaoMakeUnit*) p[0];
	DString *path = p[1]->xString.value;
	DString *res = DaoProcess_PutString( proc, path );
	DaoMake_MakePath( self->buildPath, res );
}
static DaoFuncItem DaoMakeUnitMeths[]=
{
	{ UNIT_AddPlatformDefs,   "AddPlatformDefs( self: Unit )" },
	{ UNIT_AddDefinition,     "AddDefinition( self: Unit, name: string, value = '' )" },
	{ UNIT_AddIncludePath,    "AddIncludePath( self: Unit, path: string, ...: string )" },
	{ UNIT_AddLinkingPath,    "AddLinkingPath( self: Unit, path: string, ...: string )" },
	{ UNIT_AddCompilingFlag,  "AddCompilingFlag( self: Unit, flag: string, ...: string )" },
	{ UNIT_AddLinkingFlag,    "AddLinkingFlag( self: Unit, flag: string, ...: string )" },
	{ UNIT_AddRpath,          "AddRpath( self: Unit, flag: string, ...: string )" },

	{ UNIT_MakeDefinitions,     "MakeDefinitions( self: Unit ) => string" },
	{ UNIT_MakeIncludePaths,    "MakeIncludePaths( self: Unit ) => string" },
	{ UNIT_MakeLinkingPaths,    "MakeLinkingPaths( self: Unit ) => string" },
	{ UNIT_MakeCompilingFlags,  "MakeCompilingFlags( self: Unit ) => string" },
	{ UNIT_MakeLinkingFlags,    "MakeLinkingFlags( self: Unit ) => string" },

	{ UNIT_MakeSourcePath,    "MakeSourcePath( self: Unit, path: string ) => string" },
	{ UNIT_MakeBuildPath,     "MakeBuildPath( self: Unit, path: string ) => string" },
	{ NULL, NULL }
};
DaoTypeBase DaoMakeUnit_Typer =
{
	"Unit", NULL, NULL, (DaoFuncItem*) DaoMakeUnitMeths, {0}, {0}, NULL, NULL
};




static void OBJECTS_AddHeaders( DaoProcess *proc, DaoValue *p[], int N )
{
	int i;
	DaoMakeObjects *self = (DaoMakeObjects*) p[0];
	for(i=1; i<N; ++i) DList_Append( self->headers, DaoValue_TryGetString( p[i] ) );
}
static void OBJECTS_AddSources( DaoProcess *proc, DaoValue *p[], int N )
{
	int i;
	DaoMakeObjects *self = (DaoMakeObjects*) p[0];
	for(i=1; i<N; ++i) DList_Append( self->sources, DaoValue_TryGetString( p[i] ) );
}
static void OBJECTS_AddHeaders2( DaoProcess *proc, DaoValue *p[], int N )
{
	int i;
	DaoMakeObjects *self = (DaoMakeObjects*) p[0];
	DList *files = p[1]->xList.value;
	for(i=0; i<files->size; ++i){
		DString *file = files->items.pValue[i]->xString.value;
		DList_Append( self->headers, file );
	}
}
static void OBJECTS_AddSources2( DaoProcess *proc, DaoValue *p[], int N )
{
	int i;
	DaoMakeObjects *self = (DaoMakeObjects*) p[0];
	DList *files = p[1]->xList.value;
	for(i=0; i<files->size; ++i){
		DString *file = files->items.pValue[i]->xString.value;
		DList_Append( self->sources, file );
	}
}
static void OBJECTS_UseProject( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeUnit *self = (DaoMakeUnit*) p[0];
	DaoMakeUnit *pro = (DaoMakeUnit*) p[1];
	DaoMakeUnit_Use( self, pro, 0 );
}
static DaoFuncItem DaoMakeObjectsMeths[]=
{
	{ OBJECTS_AddHeaders,  "AddHeaders( self: Objects, file: string, ...: string )" },
	{ OBJECTS_AddSources,  "AddSources( self: Objects, file: string, ...: string )" },
	{ OBJECTS_AddHeaders2, "AddHeaders( self: Objects, files: list<string> )" },
	{ OBJECTS_AddSources2, "AddSources( self: Objects, files: list<string> )" },
	{ OBJECTS_UseProject,  "UseProject( self: Objects, pro: Project )" },
	{ NULL, NULL }
};
DaoTypeBase DaoMakeObjects_Typer =
{
	"Objects", NULL, NULL, (DaoFuncItem*) DaoMakeObjectsMeths,
	{ & DaoMakeUnit_Typer, NULL }, {0},
	(FuncPtrDel)DaoMakeObjects_Delete, NULL
};




static void TARGET_Name( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeTarget *self = (DaoMakeTarget*) p[0];
	DString *res = DaoProcess_PutChars( proc, "" );
	DaoMakeTarget_MakeName( self, res, 0 );
}
static void TARGET_AddObjects( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeTarget *self = (DaoMakeTarget*) p[0];
	int i;
	for(i=1; i<N; ++i) DList_Append( self->objects, p[i] );
}
static void TARGET_AddCommand( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeTarget *self = (DaoMakeTarget*) p[0];
	int i;
	for(i=1; i<N; ++i){
		DString *cmd = DaoValue_TryGetString( p[i] );
		if( cmd == NULL ) continue;
		DList_Append( self->commands, cmd );
	}
}
static void TARGET_AddTest( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeTarget *self = (DaoMakeTarget*) p[0];
	int i;
	for(i=1; i<N; ++i){
		DString *test = DaoValue_TryGetString( p[i] );
		if( test == NULL ) continue;
		DList_Append( self->tests, test );
	}
}
static void TARGET_AddDepends( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeTarget *self = (DaoMakeTarget*) p[0];
	int i;
	for(i=1; i<N; ++i){
		DaoMakeTarget *target = (DaoMakeTarget*) DaoValue_CastCstruct( p[i], daomake_type_target );
		if( target == NULL ) continue;
		DList_Append( self->depends, target );
	}
}
DaoMakeTarget* DaoMakeProject_FindLibrary( DaoMakeProject *self, DString *name, int ttype )
{
	int i;
	for(i=0; i<self->targets2->size; ++i){
		DaoMakeTarget *tar = (DaoMakeTarget*) self->targets2->items.pValue[i];
		if( tar->ttype != ttype ) continue;
		if( DString_EQ( tar->name, name ) ) return tar;
	}
	return NULL;
}
void DaoMakeUnit_UseAll( DaoMakeUnit *self, DaoMakeProject *proj, int ttype, int import )
{
	int i;
	for(i=0; i<proj->targets2->size; ++i){
		DaoMakeTarget *tar = (DaoMakeTarget*) proj->targets2->items.pValue[i];
		if( tar->ttype != ttype ) continue;
		DaoMakeUnit_Use( self, (DaoMakeUnit*) tar, import );
	}
}
static void UNIT_UseImportLib( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeUnit *self = (DaoMakeUnit*) p[0];
	DaoMakeProject *proj = (DaoMakeProject*) p[1];
	int i;

	DaoMakeUnit_Use( (DaoMakeUnit*) self, (DaoMakeUnit*) proj, 1 );
	if( N == 2 ) DaoMakeUnit_UseAll( self, proj, DAOMAKE_SHAREDLIB, 1 );
	for(i=2; i<N; ++i){
		DaoMakeTarget *tar;
		DString *name = DaoValue_TryGetString( p[i] );
		if( name == NULL ) continue;
		tar = DaoMakeProject_FindLibrary( proj, name, DAOMAKE_SHAREDLIB );
		if( tar ) DaoMakeUnit_Use( (DaoMakeUnit*) self, (DaoMakeUnit*) tar, 1 );
	}
}
static void UNIT_UseSharedLib( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeUnit *self = (DaoMakeUnit*) p[0];
	DaoMakeProject *proj = (DaoMakeProject*) p[1];
	int i;

	DaoMakeUnit_Use( (DaoMakeUnit*) self, (DaoMakeUnit*) proj, 0 );
	if( N == 2 ) DaoMakeUnit_UseAll( self, proj, DAOMAKE_SHAREDLIB, 0 );
	for(i=2; i<N; ++i){
		DaoMakeTarget *tar;
		DString *name = DaoValue_TryGetString( p[i] );
		if( name == NULL ) continue;
		tar = DaoMakeProject_FindLibrary( proj, name, DAOMAKE_SHAREDLIB );
		if( tar ) DaoMakeUnit_Use( (DaoMakeUnit*) self, (DaoMakeUnit*) tar, 0 );
	}
}
static void UNIT_UseStaticLib( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeUnit *self = (DaoMakeUnit*) p[0];
	DaoMakeProject *proj = (DaoMakeProject*) p[1];
	int i;

	DaoMakeUnit_Use( (DaoMakeUnit*) self, (DaoMakeUnit*) proj, 0 );
	if( N == 2 ) DaoMakeUnit_UseAll( self, proj, DAOMAKE_STATICLIB, 0 );
	for(i=2; i<N; ++i){
		DaoMakeTarget *tar;
		DString *name = DaoValue_TryGetString( p[i] );
		if( name == NULL ) continue;
		tar = DaoMakeProject_FindLibrary( proj, name, DAOMAKE_STATICLIB );
		if( tar ) DaoMakeUnit_Use( (DaoMakeUnit*) self, (DaoMakeUnit*) tar, 0 );
	}
}
static void TARGET_EnableDynamicExporting( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeTarget *self = (DaoMakeTarget*) p[0];
	self->dynamicExporting = p[1]->xEnum.value;
}
static void TARGET_EnableDynamicLinking( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeTarget *self = (DaoMakeTarget*) p[0];
	self->dynamicLinking = p[1]->xEnum.value;
}
static void TARGET_SetTargetPath( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeTarget *self = (DaoMakeTarget*) p[0];
	DString *dest = p[1]->xString.value;
	DaoMakeTarget_SetTargetPath( self, dest );
}
static void TARGET_BinaryPath( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeTarget *self = (DaoMakeTarget*) p[0];
	DaoProcess_PutString( proc, self->base.binaryPath );
}
static void TARGET_Install( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoNamespace *ns = proc->activeNamespace;
	DaoMakeTarget *self = (DaoMakeTarget*) p[0];
	DString *dest = p[1]->xString.value;
	DString_Assign( self->install, dest );
	DaoMake_MakePath( ns->path, self->install );
}
static DaoFuncItem DaoMakeTargetMeths[]=
{
	{ TARGET_Name,  "Name( self: Target ) => string" },
	{ TARGET_AddObjects,  "AddObjects( self: Target, objects: Objects, ...: Objects )" },
	{ TARGET_AddCommand,  "AddCommand( self: Target, command: string, ...: string )" },
	{ TARGET_AddTest,     "AddTest( self: Target, test: string, ...: string )" },
	{ TARGET_AddDepends,  "AddDependency( self: Target, target: Target, ...: Target )" },
	{ UNIT_UseImportLib,  "UseImportLibrary( self: Target, pro: Project, ...: string )" },
	{ UNIT_UseSharedLib,  "UseSharedLibrary( self: Target, pro: Project, ...: string )" },
	{ UNIT_UseStaticLib,  "UseStaticLibrary( self: Target, pro: Project, ...: string )" },
	{ TARGET_EnableDynamicExporting,  "EnableDynamicExporting( self: Target, bl :enum<FALSE,TRUE> = $TRUE )" },
	{ TARGET_EnableDynamicLinking,    "EnableDynamicLinking( self: Target, bl :enum<FALSE,TRUE> = $TRUE )" },
	{ TARGET_SetTargetPath,  "SetTargetPath( self: Target, path: string )" },
	{ TARGET_BinaryPath,  "BinaryPath( self: Target ) => string" },
	{ TARGET_Install,  "Install( self: Target, dest: string )" },
	{ NULL, NULL }
};
static void TARGET_GetGCFields( void *p, DList *values, DList *arrays, DList *maps, int rm )
{
	DaoMakeTarget *self = (DaoMakeTarget*) p;
	DList_Append( arrays, self->objects );
	DList_Append( arrays, self->depends );
}
DaoTypeBase DaoMakeTarget_Typer =
{
	"Target", NULL, NULL, (DaoFuncItem*) DaoMakeTargetMeths,
	{ & DaoMakeUnit_Typer, NULL }, {0},
	(FuncPtrDel)DaoMakeTarget_Delete, TARGET_GetGCFields
};





static void PROJECT_New( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *name = DaoValue_TryGetString( p[0] );
	DaoNamespace *ns = proc->activeNamespace;
	DaoMakeProject *self = DaoMakeProject_New();

	DString_Append( self->base.sourcePath, proc->activeNamespace->path );
	DString_Append( self->base.buildPath, proc->activeNamespace->path );
	if( daomake_out_of_source ){
		DString *binpath = vmSpace->startPath;
		DString *main = daomake_main_source_path;
		DString_Insert( self->base.buildPath, binpath, 0, main->size, binpath->size );
	}
	DString_Assign( self->base.binaryPath, self->base.buildPath );
	DString_Assign( self->targetPath, self->base.buildPath );
	self->base.project = self;
	DaoProcess_PutValue( proc, (DaoValue*) self );
	DString_Assign( self->sourceName, ns->file );
	DString_Assign( self->projectName, name );
	DaoMap_InsertChars( daomake_projects, name->chars, (DaoValue*) self );
}
static void PROJECT_AddOBJ( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeProject *self = (DaoMakeProject*) p[0];
	DaoMakeObjects *objs = DaoMakeObjects_New();

	DaoMakeUnit_InitBuildPath( (DaoMakeUnit*) objs, proc );
	objs->base.project = self;
	DList_ImportStringList( objs->sources, (DaoList*) p[1] );
	DList_ImportStringList( objs->headers, (DaoList*) p[2] );
	DaoProcess_PutValue( proc, (DaoValue*) objs );
}
static void PROJECT_AddTarget( DaoProcess *proc, DaoValue *p[], int N, int ttype )
{
	DaoMakeProject *self = (DaoMakeProject*) p[0];
	DaoMakeTarget *target = DaoMakeTarget_New();
	int i;
	DaoMakeUnit_InitBuildPath( (DaoMakeUnit*) target, proc );
	target->ttype = ttype;
	target->base.project = self;
	DString_Assign( target->name, DaoValue_TryGetString( p[1] ) );
	DString_Assign( target->base.binaryPath, self->targetPath );
	for(i=2; i<N; ++i) DList_Append( target->objects, p[i] );
	DaoProcess_PutValue( proc, (DaoValue*) target );
	if( N > 2 ) DList_Append( self->targets, (DaoValue*) target );
	DList_Append( self->targets2, (DaoValue*) target );
}
static void PROJECT_AddEXE( DaoProcess *proc, DaoValue *p[], int N )
{
	PROJECT_AddTarget( proc, p, N, DAOMAKE_EXECUTABLE );
}
static void PROJECT_AddDLL( DaoProcess *proc, DaoValue *p[], int N )
{
	PROJECT_AddTarget( proc, p, N, DAOMAKE_SHAREDLIB );
}
static void PROJECT_AddARC( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *arc = DaoMake_GetSettingValue( "AR" );
	if( arc == NULL ) DaoProcess_RaiseError( proc, NULL, "The platform does not support static library!" );
	PROJECT_AddTarget( proc, p, N, DAOMAKE_STATICLIB );
}
static void PROJECT_AddJS( DaoProcess *proc, DaoValue *p[], int N )
{
	PROJECT_AddTarget( proc, p, N, DAOMAKE_JAVASCRIPT );
}
static void PROJECT_AddTest( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeProject *self = (DaoMakeProject*) p[0];
	DaoMakeTarget *target = DaoMakeTarget_New();
	int i;
	DaoMakeUnit_InitBuildPath( (DaoMakeUnit*) target, proc );
	daomake_test_count += (N-2);
	target->ttype = DAOMAKE_TESTING;
	target->base.project = self;
	DString_Assign( target->name, DaoValue_TryGetString( p[1] ) );
	for(i=2; i<N; ++i) DList_Append( target->tests, DaoValue_TryGetString( p[i] ) );
	DaoProcess_PutValue( proc, (DaoValue*) target );
	DList_Append( self->targets, (DaoValue*) target );
}
static void PROJECT_AddCMD( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeProject *self = (DaoMakeProject*) p[0];
	DString *name = DaoValue_TryGetString( p[1] );
	DaoMakeTarget *target = DaoMakeTarget_New();
	int i;
	DaoMakeUnit_InitBuildPath( (DaoMakeUnit*) target, proc );
	target->ttype = DAOMAKE_COMMAND;
	target->base.project = self;
	DString_Assign( target->name, name );
	i = DString_FindChar( target->name, ':', 0 );
	if( i >= 0 ){
		DString_Erase( target->name, i, -1 );
		DString_SubString( name, target->extradeps, i+1, -1 );
	}
	for(i=2; i<N; ++i){
		DString *cmd = DaoValue_TryGetString( p[i] );
		DList_Append( target->commands, cmd );
	}
	DaoProcess_PutValue( proc, (DaoValue*) target );
	DList_Append( self->targets, (DaoValue*) target );
}
static void PROJECT_AddDIR( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeProject *self = (DaoMakeProject*) p[0];
	DString *file = DaoMakeProject_GetBufferString( self );
	DString *name = DaoValue_TryGetString( p[1] );
	DaoMakeTarget *target = DaoMakeTarget_New();
	DaoNamespace *ns;
	int i;
	DaoMakeUnit_InitBuildPath( (DaoMakeUnit*) target, proc );
	target->ttype = DAOMAKE_DIRECTORY;
	target->base.project = self;
	DString_Assign( target->name, name );
	for(i=2; i<N; ++i){
		DString *path = DaoValue_TryGetString( p[i] );
		DString_Append( file, path );
		DString_AppendPathSep( file );
		DString_AppendChars( file, "makefile.dao" );
		ns = DaoVmSpace_Load( proc->vmSpace, file->chars );
		if( ns == NULL ) continue;
		DList_Append( target->commands, path );
	}
	DaoProcess_PutValue( proc, (DaoValue*) target );
	DList_Append( self->targets, (DaoValue*) target );
	self->usedStrings -= 1;
}
static void PROJECT_AddVAR( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeProject *self = (DaoMakeProject*) p[0];
	DString *name = DaoValue_TryGetString( p[1] );
	DString *value = DaoValue_TryGetString( p[2] );
	DString *oper = DaoValue_TryGetString( p[3] );
	DList_Append( self->variables, name );
	DList_Append( self->variables, value );
	DList_Append( self->variables, oper );
}
static void PROJECT_FindTarget( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeProject *self = (DaoMakeProject*) p[0];
	DString *name = DaoValue_TryGetString( p[1] );
	int ttype = p[2]->xEnum.value;
	int i;
	for(i=0; i<self->targets->size; ++i){
		DaoMakeTarget *tar = (DaoMakeTarget*) self->targets->items.pVoid[i];
		int check = 0;
		if( (ttype & 1) && tar->ttype == DAOMAKE_SHAREDLIB ) check = 1;
		if( (ttype & 2) && tar->ttype == DAOMAKE_STATICLIB ) check = 1;
		if( check == 0 ) continue;
		if( DString_EQ( tar->name, name ) ){
			DaoProcess_PutValue( proc, (DaoValue*) tar );
			return;
		}
	}
	DaoProcess_PutNone( proc );
}
static void PROJECT_GetTarget( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoList *list = DaoProcess_PutList( proc );
	DaoMakeProject *self = (DaoMakeProject*) p[0];
	int ttype = p[1]->xEnum.value;
	int i, j;
	for(i=0; i<self->targets->size; ++i){
		DaoMakeTarget *tar = (DaoMakeTarget*) self->targets->items.pVoid[i];
		int check = 0;
		for(j=DAOMAKE_EXECUTABLE; j<=DAOMAKE_DIRECTORY; ++j) check |= ttype & (1<<j);
		if( ttype & (1<<tar->ttype) ) DaoList_Append( list, (DaoValue*) tar );
	}
}
static void PROJECT_SetTargetPath( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeProject *self = (DaoMakeProject*) p[0];
	DString *dest = p[1]->xString.value;
	int i;
	DString_Assign( self->targetPath, dest );
	DaoMake_MakePath( self->base.buildPath, self->targetPath );
	DaoMake_MakeDirs( self->targetPath, 0 );
	DString_Assign( self->base.binaryPath, self->targetPath );
	for(i=0; i<self->targets->size; ++i){
		DaoMakeTarget *target = (DaoMakeTarget*) self->targets->items.pVoid[i];
		DaoMakeTarget_SetTargetPath( target, dest );
	}
}
static void PROJECT_InstallTarget( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoNamespace *ns = proc->activeNamespace;
	DaoMakeProject *self = (DaoMakeProject*) p[0];
	DString *dest = DaoValue_TryGetString( p[1] );
	int i;
	DaoMake_MakePath( ns->path, dest );
	for(i=2; i<N; ++i){
		DaoMakeTarget *target = (DaoMakeTarget*) p[i];
		DList_Append( self->installs, p[i] );
		DList_Append( self->installs, p[1] );
		if( target->ttype <= DAOMAKE_STATICLIB ) DString_Assign( target->install, dest );
	}
}
static void PROJECT_InstallFile( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoNamespace *ns = proc->activeNamespace;
	DaoMakeProject *self = (DaoMakeProject*) p[0];
	DString *dest = DaoValue_TryGetString( p[1] );
	int i;
	DaoMake_MakePath( ns->path, dest );
	for(i=2; i<N; ++i){
		DString *file = DaoValue_TryGetString( p[i] );
		DaoMake_MakePath( ns->path, file );
		DList_Append( self->installs, p[i] );
		DList_Append( self->installs, p[1] );
	}
}
static void PROJECT_InstallFiles( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoNamespace *ns = proc->activeNamespace;
	DaoMakeProject *self = (DaoMakeProject*) p[0];
	DaoList *list = (DaoList*) p[2];
	int i, size = DaoList_Size( list );
	DaoMake_MakePath( ns->path, p[1]->xString.value );
	for(i=0; i<size; ++i){
		DaoValue *it = DaoList_GetItem( list, i );
		DList_Append( self->installs, it );
		DList_Append( self->installs, p[1] );
	}
}
static void PROJECT_SourcePath( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeProject *self = (DaoMakeProject*) p[0];
	DaoProcess_PutString( proc, self->base.sourcePath );
}
static void PROJECT_BinaryPath( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeProject *self = (DaoMakeProject*) p[0];
	DaoProcess_PutString( proc, self->base.binaryPath );
}
static void PROJECT_BuildPath( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeProject *self = (DaoMakeProject*) p[0];
	DaoProcess_PutString( proc, self->base.buildPath );
}
static void PROJECT_GenerateFinder( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoMakeProject *self = (DaoMakeProject*) p[0];
	DString *path = DaoProcess_PutChars( proc, "" );
	self->generateFinder = p[1]->xEnum.value;
	DaoMakeProject_MakeFinderPath( self, path );
}
static DaoFuncItem DaoMakeProjectMeths[]=
{
	{ PROJECT_New,     "Project( name: string ) => Project" },

	{ PROJECT_AddOBJ,  "AddObjects( self: Project, sources: list<string>, headers: list<string> = {} ) => Objects" },
	{ PROJECT_AddEXE,  "AddExecutable( self: Project, name: string, objs: Objects, ...: Objects ) =>Target" },
	{ PROJECT_AddDLL,  "AddSharedLibrary( self: Project, name: string, objs: Objects, ...: Objects ) =>Target" },
	{ PROJECT_AddARC,  "AddStaticLibrary( self: Project, name: string, objs: Objects, ...: Objects ) =>Target" },
	{ PROJECT_AddJS,   "AddJavaScriptLibrary( self: Project, name: string, objs: Objects, ...: Objects ) =>Target" },
	{ PROJECT_AddDLL,  "AddSharedLibrary( self: Project, name: string ) =>Target" },
	{ PROJECT_AddARC,  "AddStaticLibrary( self: Project, name: string ) =>Target" },
	{ PROJECT_AddTest, "AddTest( self: Project, group: string, test: string, ...: string ) => Target" },
	{ PROJECT_AddCMD,  "AddCommand( self: Project, name: string, command: string, ...: string ) => Target" },

	{ PROJECT_AddDIR,  "AddDirectory( self: Project, name: string, path: string, ...: string ) => Target" },

	{ PROJECT_AddVAR,  "AddVariable( self: Project, name: string, value: string, op = '=' )" },
	{ UNIT_UseImportLib,  "UseImportLibrary( self: Project, pro: Project, ...: string )" },
	{ UNIT_UseSharedLib,  "UseSharedLibrary( self: Project, pro: Project, ...: string )" },
	{ UNIT_UseStaticLib,  "UseStaticLibrary( self: Project, pro: Project, ...: string )" },

	{ PROJECT_FindTarget,  "FindTarget( self: Project, name: string, ttype: enum<shared;static> = $shared ) => Target|none" },

	{ PROJECT_GetTarget,  "GetTargets( self: Project, ttype: enum<exec;shared;static;js;test;cmd;dir> = $shared ) => list<Target>" },

	{ PROJECT_SetTargetPath,  "SetTargetPath( self: Project, path: string )" },

	{ PROJECT_InstallTarget,  "Install( self: Project, dest: string, target: Target, ...: Target )" },
	{ PROJECT_InstallFile,    "Install( self: Project, dest: string, file: string, ...: string )" },
	{ PROJECT_InstallFiles,   "Install( self: Project, dest: string, headers: list<string> )" },

	{ PROJECT_SourcePath,  "SourcePath( self: Project ) => string" },
	{ PROJECT_BinaryPath,  "BinaryPath( self: Project ) => string" },
	{ PROJECT_BuildPath,   "BuildPath( self: Project ) => string" },

	{ PROJECT_GenerateFinder, "GenerateFinder( self: Project, bl: enum<FALSE,TRUE> = $TRUE ) => string" },
	{ NULL, NULL }
};
static void PROJ_GetGCFields( void *p, DList *values, DList *arrays, DList *maps, int rm )
{
	DaoMakeProject *self = (DaoMakeProject*) p;
	DList_Append( arrays, self->targets );
	DList_Append( arrays, self->installs );
}
DaoTypeBase DaoMakeProject_Typer =
{
	"Project", NULL, NULL, (DaoFuncItem*) DaoMakeProjectMeths,
	{ & DaoMakeUnit_Typer, NULL }, {0},
	(FuncPtrDel)DaoMakeProject_Delete,  PROJ_GetGCFields
};



static void Vars_Get( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *key = DaoValue_TryGetString( p[0] );
	DNode *it = DMap_Find( daomake_variable_map, key );
	if( it == NULL ){
		DaoProcess_RaiseError( proc, "Key", key->chars );
		return;
	}
	DaoProcess_PutString( proc, it->value.pString );
}
static void Vars_Set( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *key = DaoValue_TryGetString( p[1] );
	DString *oper = DaoValue_TryGetString( p[2] );
	DString *value = DaoValue_TryGetString( p[0] );
	DNode *it = DMap_Find( daomake_variable_map, key );
	if( it && DString_EQ( value, it->value.pString ) ) return;
	it = DMap_Find( daomake_variable_map3, key );
	if( it ){
		DString_Assign( daomake_variable_list->items.pString[it->value.pInt], value );
	}else{
		DList_Append( daomake_variable_list, key );
		DList_Append( daomake_variable_list, oper );
		DList_Append( daomake_variable_list, value );
	}
	DMap_Insert( daomake_variable_map, key, value );
	DMap_Insert( daomake_variable_map3, key, (void*)(size_t)(daomake_variable_list->size-1) );
	if( oper->chars[0] == '?' ) DMap_Insert( daomake_variable_map2, key, value );
}
static DaoFuncItem DaoMakeVarsMeths[]=
{
	{ Vars_Get,     "[]( key: string ) => string" },
	{ Vars_Set,     "[]=( value: string, key: string, mode = '?=' )" },
	{ NULL, NULL }
};

DaoTypeBase DaoMakeVariables_Typer =
{
	"Variables", NULL, NULL, (DaoFuncItem*) DaoMakeVarsMeths,
	{ NULL }, { NULL }, NULL, NULL
};




static void DAOMAKE_FindPackage( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoValue *project = DaoMap_GetValue( daomake_projects, p[0] );
	DString *name = DaoValue_TryGetString( p[0] );
	DString *cache = DString_Copy( proc->activeNamespace->path );
	DString *original = DString_New();
	DString *message = DString_New();
	daoint i, reset = daomake_reset_cache;
	size_t otime, ctime;
	FILE *fout = NULL;

	DString_AppendChars( message, "Package \"" );
	DString_Append( message, name );
	DString_AppendChars( message, "\" not found!" );

	DString_SetChars( original, "packages/Find" );
	DString_Append( original, name);
	DString_AppendChars( original, ".dao" );

	DString_AppendPathSep( cache );
	DString_AppendChars( cache, "CacheFind" );
	DString_Append( cache, name );
	DString_AppendChar( cache, '-' );
	DString_Append( cache, daomake_platform );
	if( daomake_architecture->size ){
		DString_AppendChar( cache, '-' );
		DString_Append( cache, daomake_architecture );
	}
	DString_AppendChars( cache, ".dao" );
	DaoMake_MakeOutOfSourcePath( cache, 1 );

	if( DaoVmSpace_CompleteModuleName( vmSpace, cache, DAO_MODULE_DAO ) == DAO_MODULE_NONE ) reset = 1;
	DaoVmSpace_CompleteModuleName( vmSpace, original, DAO_MODULE_DAO );
	otime = Dao_FileChangedTime( original->chars );
	ctime = Dao_FileChangedTime( cache->chars );
	if( otime > ctime ) reset = 1;

	if( project == NULL && reset == 0 ){
		if( DaoVmSpace_Load( vmSpace, cache->chars ) ){
			project = DaoMap_GetValue( daomake_projects, p[0] );
			if( project == NULL ){
				if( p[1]->xEnum.value ){
					DaoProcess_RaiseError( proc, NULL, message->chars );
				}
				project = dao_none_value;
			}
			DaoProcess_PutValue( proc, project );
			DString_Delete( original );
			DString_Delete( message );
			DString_Delete( cache );
			return;
		}
	}
	if( project == NULL ){
		DNode *it = DMap_Find( daomake_packages->value, p[0] );
		if( it != NULL ){
			DaoTuple *tuple = (DaoTuple*) it->value.pValue;
			DString *hints = tuple->values[0]->xString.value;
			DString *file = tuple->values[1]->xString.value;
			size_t loc = DaoMake_FindFile( file, hints );
			if( loc ){
				DaoMakeProject *project = DaoMakeProject_New();

				DString_SubString( hints, project->projectName, loc>>16, loc&0xffff );
				DList_Append( project->base.includePaths, project->projectName );
				DList_Append( project->base.compilingFlags, tuple->values[2]->xString.value );
				DList_Append( project->base.linkingFlags, tuple->values[3]->xString.value );

				DString_Assign( project->projectName, name );
				DaoMap_InsertChars( daomake_projects, name->chars, (DaoValue*) project );

				DaoProcess_PutValue( proc, (DaoValue*) project );
				DString_Delete( original );
				DString_Delete( message );
				DString_Delete( cache );
				return;
			}
		}
	}
	if( project == NULL ){
		if( DaoVmSpace_Load( vmSpace, original->chars ) ){
			fout = Dao_OpenFile( cache->chars, "w+" );
		}
	}
	project = DaoMap_GetValue( daomake_projects, p[0] );
	if( project == NULL ){
		if( p[1]->xEnum.value ){
			DaoProcess_RaiseError( proc, NULL, message->chars );
		}
		project = dao_none_value;
	}else{
		DaoMakeProject *proj = (DaoMakeProject*) project;
		DaoMakeProject_MakeFindPackage( proj, cache, 1 );
		if( fout ) fprintf( fout, "%s", cache->chars );
	}
	if( fout ) fclose( fout );
	DaoProcess_PutValue( proc, project );
	DString_Delete( original );
	DString_Delete( message );
	DString_Delete( cache );
}
static void DAOMAKE_FindFile( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *res = DaoProcess_PutChars( proc, "" );
	DString *file = DaoValue_TryGetString( p[0] );
	DString *hints = DaoValue_TryGetString( p[1] );
	size_t loc = DaoMake_FindFile( file, hints );
	if( loc == 0 ){
		int i;
		for(i=0; i<daomake_includes->value->size; ++i){
			hints = daomake_includes->value->items.pValue[i]->xString.value;
			loc = DaoMake_FindFile( file, hints );
			if( loc ) break;
		}
	}
	if( loc ) DString_SubString( hints, res, loc>>16, loc&0xffff );
}
static void DAOMAKE_TestCompile( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *md5, *command, *source, *output;
	DString *code = DaoValue_TryGetString( p[0] );
	DString *lflag = DaoValue_TryGetString( p[1] );
	DString *cflag = DaoValue_TryGetString( p[2] );
	daoint pos1, pos2, pos3, cxx = DaoValue_TryGetInteger( p[3] );
	dao_integer *res = DaoProcess_PutInteger( proc, 0 );
	FILE *file;

	DString_Trim( cflag, 1, 1, 0 );
	DString_Trim( lflag, 1, 1, 0 );
	DString_Trim( code, 1, 1, 0 );
	if( code->size == 0 ) return;

	md5 = DString_New();
	command = DString_New();
	source = DString_New();
	output = DString_New();
	DString_AppendChars( command, cxx ? "c++ " : "cc " );
	if( cflag->size ){
		DString_Append( command, cflag );
		DString_AppendChars( command, " " );
	}
	pos1 = command->size;
	DString_AppendChars( command, "-o " );
	pos2 = command->size;
	if( lflag->size ){
		DString_Append( command, lflag );
		DString_AppendChars( command, " " );
	}
	pos3 = command->size;

	DString_Append( command, code );
	DString_MD5( command, md5 );
	DString_Reset( md5, 12 );

	DString_AppendChars( source, daomake_objects_dir );
	DString_AppendChars( source, "/source_" );
	DString_Append( source, md5 );
	DString_AppendChars( source, cxx ? ".cxx" : ".c" );

	DString_AppendChars( output, daomake_objects_dir );
	DString_AppendChars( output, "/binary_" );
	DString_Append( output, md5 );

	DaoMake_MakePath( daomake_main_source_path, source );
	DaoMake_MakePath( daomake_main_source_path, output );
	if( daomake_out_of_source ){
		DaoMake_MakeOutOfSourcePath( source, 1 );
		DaoMake_MakeOutOfSourcePath( output, 1 );
	}

	DString_Delete( md5 );
	if( daomake_reset_cache == 0 && DaoMake_IsFile( output->chars ) ){
		DString_Delete( command );
		DString_Delete( source );
		DString_Delete( output );
		*res = 1;
		return;
	}

	DString_Erase( command, pos3, -1 );
	DString_InsertChars( command, " ", pos2, 0, -1 );
	DString_Insert( command, output, pos2, 0, 0 );
	DString_InsertChars( command, " ", pos1, 0, -1 );
	DString_Insert( command, source, pos1, 0, -1 );

	printf( "Checking: %s\n", command->chars );
	file = Dao_OpenFile( source->chars, "w+" );
	DaoFile_WriteString( file, code );
	fclose( file );

	DString_Assign( source, output );
	DString_SetChars( output, daomake_objects_dir );
	DString_AppendChars( output, "/null" );
	DString_AppendChars( command, " &> " );
	DString_Append( command, output );

	system( command->chars );
	*res = DaoMake_IsFile( source->chars );
	DString_Delete( command );
	DString_Delete( source );
	DString_Delete( output );
}
static void DAOMAKE_OptionBOOL( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *name = DaoValue_TryGetString( p[0] );
	DaoEnum *value = DaoValue_CastEnum( p[1] );
	DaoEnum *res = (DaoEnum*) DaoProcess_PutValue( proc, p[1] );
	DNode *it = DMap_Find( daomake_boolean_options, name );
	if( it ) res->value = it->value.pInt;
}
static void DAOMAKE_OptionSTR( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *name = DaoValue_TryGetString( p[0] );
	DString *res = DaoValue_TryGetString( p[1] );
	DNode *it = DMap_Find( daomake_string_options, name );
	if( it ) res = it->value.pString;
	DaoProcess_PutString( proc, res );
}
static void DAOMAKE_Suffix( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoNamespace *ns = proc->activeNamespace;
	DaoProcess_PutChars( proc, daomake_makefile_suffix );
}
static void DAOMAKE_IsFile( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *path = DaoValue_TryGetString( p[0] );
	DaoMake_MakePath( proc->activeNamespace->path, path );
	DaoProcess_PutInteger( proc, DaoMake_IsFile( path->chars ) );
}
static void DAOMAKE_IsDir( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *path = DaoValue_TryGetString( p[0] );
	DaoMake_MakePath( proc->activeNamespace->path, path );
	DaoProcess_PutInteger( proc, DaoMake_IsDir( path->chars ) );
}
static void DAOMAKE_MakeDir( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *path = DaoValue_TryGetString( p[0] );
	if( DaoMake_IsFile( path->chars ) ){
		DaoProcess_RaiseError( proc, NULL, "The path is a file" );
		return;
	}
	if( DaoMake_IsDir( path->chars ) ) return;
	path = DString_Copy( path );
	DaoMake_MakeDirs( path, 0 );
	DString_Delete( path );
}
static void DaoProcess_WriteChars( DaoProcess *self, const char *chars )
{
	DaoStream *stream = self->stdioStream;
	if( stream == NULL ) stream = self->vmSpace->stdioStream;
	DaoStream_WriteChars( stream, "Executing: " );
	DaoStream_WriteChars( stream, chars );
	DaoStream_WriteChars( stream, "\n" );
	DaoStream_Flush( stream );
}
static void DAOMAKE_Shell( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *res = DaoProcess_PutChars( proc, "" );
	DString *cmd = DaoValue_TryGetString( p[0] );
	FILE *fin = popen( DString_GetData( cmd ), "r" );
	DaoProcess_WriteChars( proc, cmd->chars );
	if( fin == NULL ){
		DaoProcess_RaiseError( proc, NULL, "Command failed to execute!" );
		return;
	}
	DaoFile_ReadAll( fin, res, 0 );
	pclose( fin );
}
static void DAOMAKE_Shell2( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *cmd = DaoValue_TryGetString( p[0] );
	DaoProcess_WriteChars( proc, cmd->chars );
	DaoProcess_PutInteger( proc, system( cmd->chars ) );
}
static void DAOMAKE_GetEnv( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *res = DaoProcess_PutChars( proc, "" );
	DString *name = DaoValue_TryGetString( p[0] );
	char *value = getenv( DString_GetData( name ) );
	if( value ) DString_SetChars( res, value );
}
static void DAOMAKE_SourcePath( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoNamespace *ns = proc->activeNamespace;
	DaoProcess_PutString( proc, ns->path );
}
static void DAOMAKE_BinaryPath( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoProcess_PutString( proc, vmSpace->startPath );
}
static void DAOMAKE_MakePath( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *base = DaoValue_TryGetString( p[0] );
	DString *sub = DaoValue_TryGetString( p[1] );
	DString *res = DaoProcess_PutString( proc, sub );
	DaoMake_MakePath( base, res );
}
static void DAOMAKE_MakeRpath( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *flag = DaoProcess_PutChars( proc, "" );
	DString *rpath = DaoMake_GetSettingValue( "DLL-RPATH" );
	int i;
	if( rpath->size == 0 ) return; /* TODO: WARNING; */
	for(i=0; i<N; ++i){
		DString *path = DaoValue_TryGetString( p[i] );
		if( path == NULL ) continue;
		DString_AppendGap( flag );
		DString_Append( flag, rpath );
		DString_Append( flag, path );
	}
}
static void DAOMAKE_BuildMode( DaoProcess *proc, DaoValue *p[], int N )
{
	static const char *const build_modes[] = { "RELEASE", "DEBUG", "PROFILE" };
	DaoProcess_PutEnum( proc, build_modes[ daomake_build_mode ] );
}
static void DAOMAKE_SetTestTool( DaoProcess *proc, DaoValue *p[], int N )
{
	DString *tool = DaoValue_TryGetString( p[0] );
	DString *option = DaoValue_TryGetString( p[1] );

	DString_Reset( daomake_test_tool, 0 );
	DString_Reset( daomake_test_tool_option, 0 );
	DString_Append( daomake_test_tool, tool );
	DString_Append( daomake_test_tool_option, option );
}
static void DAOMAKE_Platform( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoProcess_PutString( proc, daomake_platform );
}
static void DAOMAKE_Arch( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoProcess_PutString( proc, daomake_architecture );
}
static void DAOMAKE_IsPlatform( DaoProcess *proc, DaoValue *p[], int N)
{
	DString *platform = p[0]->xString.value;
	DaoValue *value = DaoMap_GetValueChars( daomake_platforms, DString_GetData( platform ) );
	DaoProcess_PutInteger( proc, value != NULL );
}
static void DAOMAKE_Is64Bit( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoProcess_PutInteger( proc, sizeof(void*) == 8 );
}
static void DAOMAKE_IsOutOfSource( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoProcess_PutInteger( proc, daomake_out_of_source );
}
static void DAOMAKE_UseLocalRPath( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoProcess_PutInteger( proc, daomake_local_rpath );
}

static DaoFuncItem DaoMakeMeths[] =
{
	{ DAOMAKE_FindPackage, "FindPackage( name: string, opt :enum<OPTIONAL,REQUIRED> = $OPTIONAL ) => Project|none" },
	{ DAOMAKE_FindFile,    "FindFile( file: string, hints = '' ) => string" },
	{ DAOMAKE_TestCompile, "TestCompile( code :string, lflag='', cflag='', cxx=0 ) => int" },

	{ DAOMAKE_OptionBOOL,  "Option( name: string, value: enum<OFF,ON> ) => enum<OFF,ON>" },
	{ DAOMAKE_OptionSTR,   "Option( name: string, value = '' ) => string" },

	{ DAOMAKE_Suffix,      "MakefileSuffix() => string" },

	{ DAOMAKE_GetEnv,      "GetEnv( name: string ) => string" },
	{ DAOMAKE_Shell,       "Shell( command: string ) => string" },
	{ DAOMAKE_Shell2,      "Shell2( command: string ) => int" },

	{ DAOMAKE_SourcePath,  "SourcePath() => string" },
	{ DAOMAKE_BinaryPath,  "BinaryPath() => string" },

	{ DAOMAKE_MakePath,    "MakePath( base: string, sub: string ) => string" },
	{ DAOMAKE_MakeRpath,   "MakeRpath( path: string, ...: string ) => string" },

	{ DAOMAKE_BuildMode,   "BuildMode() => enum<RELEASE,DEBUG,PROFILE>" },

	{ DAOMAKE_SetTestTool, "SetTestTool( test: string, log_option = '--log' )" },

	{ DAOMAKE_IsFile,      "IsFile( path: string ) => int" },
	{ DAOMAKE_IsDir,       "IsDir( path: string ) => int" },
	{ DAOMAKE_MakeDir,     "MakeDir( path: string )" },

	{ DAOMAKE_Platform,    "Platform() => string" },
	{ DAOMAKE_Arch,        "Architecture() => string" },

	{ DAOMAKE_IsPlatform,  "IsPlatform( platform: string ) => int" },
	{ DAOMAKE_Is64Bit,     "Is64Bit() => int" },

	{ DAOMAKE_UseLocalRPath,  "UseLocalRPath() => int" },
	{ DAOMAKE_IsOutOfSource,  "IsOutOfSourceBuild() => int" },
	{ NULL, NULL }
};



static void DaoMap_AddKeyValues( DaoMap *self, const char *const keyvalues[] )
{
	int i;
	for(i=0; keyvalues[i]; i+=2){
		DaoString key = { DAO_STRING,0,0,0,1,NULL};
		DaoString value = { DAO_STRING,0,0,0,1,NULL};
		DString sk = DString_WrapChars( keyvalues[i] );
		DString sv = DString_WrapChars( keyvalues[i+1] );
		key.value = & sk;
		value.value = & sv;
		DMap_Insert( self->value, & key, & value );
	}
}



int DaoMake_RemoveFile( const char *path )
{
	return remove( path );
}
int DaoMake_RemoveDirectory( const char *path );
int DaoMake_RemovePath( const char *from )
{
	if( DaoMake_IsFile( from ) ){
		return DaoMake_RemoveFile( from );
	}else if( DaoMake_IsDir( from ) ){
		return DaoMake_RemoveDirectory( from );
	}
	return 1;
}
int DaoMake_RemoveDirectory( const char *path )
{
	DString *src;
	char *dirname;
	int rc = 0;
#ifdef _WIN32
	intptr_t handle;
	struct _finddata_t finfo;
#else
	DIR *handle;
	struct dirent *finfo;
#endif

	if( DaoMake_IsFile( path ) ) return 1;

	src = DString_New();

#ifdef _WIN32
	DString_AppendChars( src, path );
	if( src->size && src->chars[src->size-1] == '/' || src->chars[src->size-1] == '\\' ){
		DString_AppendChars( src, "*" );
	}else{
		DString_AppendChars( src, "/*" );
	}
	handle = _findfirst( src->chars, & finfo );
	if( handle != -1 ){
		do {
			if( strcmp( finfo.name, "." ) && strcmp( finfo.name, ".." ) ){
				DString_Reset( src, 0 );
				DString_AppendChars( src, path );
				DString_AppendPathSep( src );
				DString_AppendChars( src, finfo.name );
				DaoMake_RemovePath( src->chars );
			}
		} while( !_findnext( handle, &finfo ) );
		_findclose( handle );
	}else rc = errno;
#else
	/* Using POSIX opendir/readdir otherwise */
	handle = opendir( path );
	if( handle ){
		while( ( finfo = readdir( handle ) ) ){
			if( strcmp( finfo->d_name, "." ) && strcmp( finfo->d_name, ".." ) ){
				DString_Reset( src, 0 );
				DString_AppendChars( src, path );
				DString_AppendPathSep( src );
				DString_AppendChars( src, finfo->d_name );
				DaoMake_RemovePath( src->chars );
			}
		}
		closedir( handle );
	}else rc = errno;
#endif
	DString_Delete( src );
	rc |= rmdir( path );
	return rc;
}
int DaoMake_Remove( int argc, char *argv[] )
{
	int i, rec = 0;
	for(i=0; i<argc; i++){
		char *path = argv[i];
		if( DaoMake_IsFile( path ) ){
			rec |= DaoMake_RemoveFile( path );
		}else if( DaoMake_IsDir( path ) ){
			rec |= DaoMake_RemoveDirectory( path );
		}
	}
	return rec;
}
int DaoMake_CopyFile( const char *from, const char *to, int update )
{
	FILE *fin, *fout;
	struct stat info;
	DString *dest;

	if( DaoMake_IsFile( from ) == 0 ) return 1;

	dest = DString_New();
	DString_SetChars( dest, to );
	if( DaoMake_IsDir( dest->chars ) ){
		char *sep = strrchr( from, '/' );
		DString_AppendPathSep( dest );
		if( sep == NULL ){
			DString_AppendChars( dest, from );
		}else{
			DString_AppendChars( dest, sep + 1 );
		}
	}
	if( update && Dao_FileChangedTime( from ) <= Dao_FileChangedTime( dest->chars ) ){
		DString_Delete( dest );
		return 0;
	}
	fin = Dao_OpenFile( from, "rb" );
	fout = Dao_OpenFile( dest->chars, "w+b" );
	if( fin == NULL || fout == NULL ){
		if( fin ) fclose( fin );
		if( fout ) fclose( fout );
		DString_Delete( dest );
		return 1;
	}
	if( stat( from, & info ) == 0 ) chmod( dest->chars, info.st_mode );
	DaoFile_ReadAll( fin, dest, 1 );
	DaoFile_WriteString( fout, dest );
	fclose( fout );
	DString_Delete( dest );
	return 0;
}
int DaoMake_CopyDirectory( const char *from, const char *to, int update );
int DaoMake_CopyPathFile( const char *from, const char *to, int update )
{
	if( DaoMake_IsDir( from ) )  return DaoMake_CopyDirectory( from, to, update );
	if( DaoMake_IsFile( from ) ) return DaoMake_CopyFile( from, to, update );
	return 1;
}
int DaoMake_CopyDirectory( const char *from, const char *to, int update )
{
	DString *src, *dest;
	char *dirname;
	int i, rc = 0;
#ifdef _WIN32
	intptr_t handle;
	struct _finddata_t finfo;
#else
	DIR *handle;
	struct dirent *finfo;
#endif

	if( DaoMake_IsFile( from ) || DaoMake_IsFile( to ) ) return 1;

	src = DString_New();
	dest = DString_New();
	DString_SetChars( src, from );
	DString_SetChars( dest, to );
	if( DaoMake_IsFile( dest->chars ) ){
		DString_Delete( src );
		DString_Delete( dest );
		return 1;
	}
	DString_AppendPathSep( dest );
	if( DaoMake_IsDir( dest->chars ) == 0 ){
		/* Copy to a new folder: */
		DaoMake_MakeDir( dest->chars );
	}else{
		/* Copy under an existing folder: */
		int lastsep = src->size && src->chars[src->size-1] == '/';
		int pos = DString_RFindChar( src, '/', src->size - 1 - lastsep );
		if( pos > 0 ){
			DString_AppendChars( dest, src->chars + pos + 1 );
		}else{
			DString_AppendChars( dest, from );
		}
		if( DaoMake_IsDir( dest->chars ) == 0 ) DaoMake_MakeDir( dest->chars );
	}

#ifdef _WIN32
	DString_AppendPathSep( src );
	DString_AppendChars( src, "*" );
	handle = _findfirst( src->chars, & finfo );
	if( handle != -1 ){
		do {
			if( strcmp( finfo.name, "." ) && strcmp( finfo.name, ".." ) ){
				DString_Reset( src, 0 );
				DString_AppendChars( src, from );
				DString_AppendPathSep( src );
				DString_AppendChars( src, finfo.name );
				DaoMake_CopyPathFile( src->chars, dest->chars, update );
			}
		} while( !_findnext( handle, &finfo ) );
		_findclose( handle );
	}else rc = errno;
#else
	/* Using POSIX opendir/readdir otherwise */
	handle = opendir( from );
	if( handle ){
		while( ( finfo = readdir( handle ) ) ){
			if( strcmp( finfo->d_name, "." ) && strcmp( finfo->d_name, ".." ) ){
				DString_Reset( src, 0 );
				DString_AppendChars( src, from );
				DString_AppendPathSep( src );
				DString_AppendChars( src, finfo->d_name );
				DaoMake_CopyPathFile( src->chars, dest->chars, update );
			}
		}
		closedir( handle );
	}else rc = errno;
#endif
	DString_Delete( src );
	DString_Delete( dest );
	return rc;
}
int DaoMake_Copy( int argc, char *argv[] )
{
	int i, update = 0;
	if( argc == 0 ) return 1;
	if( strcmp( argv[0], "-u" ) == 0 ){
		update = 1;
		argc -= 1;
		argv += 1;
	}
	if( argc < 2 ) return 1;
	if( argc > 2 && DaoMake_IsFile( argv[argc-1] ) ) return 1;
	for(i=0; (i+1)<argc; ++i){
		if( DaoMake_CopyPathFile( argv[i], argv[argc-1], update ) ) return 1;
	}
	return 0;
}

int DaoMake_Run( int argc, char *argv[] )
{
	DString *opts, *args;
	int i, k, idsrc = -1;
	for(i=1; i<argc; i++){
		if( strcmp( argv[i], "-e" ) ==0 || strcmp( argv[i], "--eval" ) ==0 ) break;
		/* also allows execution of script files without suffix .dao */
		if( argv[i][0] != '-' ){
			idsrc = i;
			break;
		}
	}

	k = idsrc;
	if( k < 0 ) k = argc;

	opts = DString_New();
	args  = DString_New();
	for(i=1; i<k; i++ ){
		DString_AppendChars( opts, argv[i] );
		DString_AppendChar( opts, '\1' );
	}
	if( idsrc >= 0 ){
		for(i=idsrc; i<argc; i++ ){
			DString_AppendChars( args, argv[i] );
			DString_AppendChar( args, '\1' );
		}
	}
	DaoVmSpace_ParseOptions( vmSpace, DString_GetData( opts ) );

	if( idsrc < 0 && argc == 1 ){
		DString_AppendChar( opts, '\1' );
		DString_AppendChars( opts, "-vi" );
		DaoVmSpace_ParseOptions( vmSpace, DString_GetData( opts ) );
	}

	/* Start execution. */
	k = ! DaoVmSpace_RunMain( vmSpace, DString_GetData( args ) );

	DString_Delete( args );
	DString_Delete( opts );
	DaoQuit();
	return k;
}


static const char *const daomake_doc_options =
"DaoMake Options: \n\
    --platform          platform name for which to generate makefiles;\n\
    --mode              building mode (release, debug or profile);\n\
    --suffix            makefile suffix (default none);\n\
    --reset             reset package searching caches;\n\
    --help              print this help information;\n\
    --option-OPT=value  create an option entry;\n\
    --define-DEF=value  pass a definition to the compiler;\n\
";

const char *const daomake_error_makefile_existing =
"Error: existing Makefile was not generated by DaoMake:\n  %s\n"
"Please use a (different) Makefile extension with the \"--suffix\" option.\n\n";

static const char *const daomake_lang_assemblers[] =
{
	".s" ,    "CC" , /* gcc supports command line arguments such as -I -c etc.; */
	".S" ,    "CC" ,
	NULL ,    NULL
};

static const char *const daomake_lang_compilers[] =
{
	".c" ,    "CC" ,
	".m" ,    "CC" ,
	".cc" ,   "CXX" ,
	".cpp" ,  "CXX" ,
	".cxx" ,  "CXX" ,
	".c++" ,  "CXX" ,
	".mm" ,   "CXX" ,
	".f" ,    "FC" ,
	NULL ,    NULL
};

static const char *const daomake_lang_linkers[] =
{
	".c" ,    "CC;CXX" ,
	".m" ,    "CC;CXX" ,
	".cc" ,   "CXX" ,
	".cpp" ,  "CXX" ,
	".cxx" ,  "CXX" ,
	".c++" ,  "CXX" ,
	".mm" ,   "CXX" ,
	".f" ,    "FC;CC;CXX" ,
	".s" ,    "FC;CC;CXX" ,
	".S" ,    "FC;CC;CXX" ,
	NULL ,    NULL
};


int main( int argc, char *argv[] )
{
	int i, k, m;
	char *platform = DAOMAKE_PLATFORM;
	char *architecture = "";
	char *mode = NULL;
	FILE *fin, *fout;
	DaoNamespace *nspace;
	DString *makefile = DString_New();
	DString *srcdir = DString_New();
	DString *source;
	DString *name;
	DNode *it;

	vmSpace = DaoInit( argv[0] );
	daomake_current_path = DString_New();
	DString_Reset( daomake_current_path, 1024 );
	getcwd( daomake_current_path->chars, 1024 );
	DString_Reset( daomake_current_path, strlen( daomake_current_path->chars ) );

	/* Utility subcommands: */
	if( argc > 1 ){
		if( strcmp( argv[1], "cat" ) == 0 ){
			for(i=2; i<argc; i++){
				fin = Dao_OpenFile( argv[i], "rb" );
				if( fin == NULL ) continue;
				DaoFile_ReadAll( fin, makefile, 1 );
				printf( "%s\n", makefile->chars );
			}
			return 0;
		}else if( strcmp( argv[1], "echo" ) == 0 ){
			for(i=2; i<argc; i++){
				if( i > 2 ) printf( "\n" );
				printf( "%s", argv[i] );
			}
			printf( "\n" );
			return 0;
		}else if( strcmp( argv[1], "isfile" ) == 0 ){
			if( argc == 2 ) return 1;
			return DaoMake_IsFile( argv[2] ) == 0;
		}else if( strcmp( argv[1], "isdir" ) == 0 ){
			if( argc == 2 ) return 1;
			return DaoMake_IsDir( argv[2] ) == 0;
		}else if( strcmp( argv[1], "mkdir" ) == 0 ){
			return DaoMake_MakeDir( argv[2] );
		}else if( strcmp( argv[1], "mkdir2" ) == 0 ){
			if( DaoMake_IsFile( argv[2] ) ) return 1;
			if( DaoMake_IsDir( argv[2] ) ) return 0;
			return DaoMake_MakeDir( argv[2] );
		}else if( strcmp( argv[1], "mkdir3" ) == 0 ){
			DString *path;
			if( DaoMake_IsFile( argv[2] ) ) return 1;
			if( DaoMake_IsDir( argv[2] ) ) return 0;
			path = DString_NewChars( argv[2] );
			DaoMake_MakeDirs( path, 0 );
			DString_Delete( path );
			return 0;
		}else if( strcmp( argv[1], "remove" ) == 0 ){
			return DaoMake_Remove( argc - 2, argv + 2 );
		}else if( strcmp( argv[1], "copy" ) == 0 ){
			return DaoMake_Copy( argc - 2, argv + 2 );
		}else if( strcmp( argv[1], "cat2" ) == 0 ){
			FILE *fout = Dao_OpenFile( argv[2], "w+" );
			if( fout == NULL ) return 1;
			while( (k = getchar()) != EOF ) fprintf( fout, "%c", k );
			fclose( fout );
			return 0;
		}else if( strcmp( argv[1], "eval" ) == 0 ){
			DaoRoutine *rout;
			DaoNamespace *ns = DaoVmSpace_MainNamespace( vmSpace );
			DaoProcess *vmp = DaoVmSpace_MainProcess( vmSpace );
			if( argc <= 2 ) return 1;
			DList_PushFront( vmSpace->nameLoading, vmSpace->pathWorking );
			DList_PushFront( vmSpace->pathLoading, vmSpace->pathWorking );
			DString_SetChars( vmSpace->mainNamespace->name, "command line codes" );
			if( DaoProcess_Compile( vmp, ns, argv[2] ) ==0 ) return 0;
			rout = ns->mainRoutines->items.pRoutine[ ns->mainRoutines->size-1 ];
			return DaoProcess_Call( vmp, rout, NULL, NULL, 0 );
		}else if( strcmp( argv[1], "run" ) == 0 ){
			return DaoMake_Run( argc-1, argv + 1 );
		}
	}

	daomake_makefile_paths = DMap_New(DAO_DATA_STRING,0);
	daomake_boolean_options = DMap_New(DAO_DATA_STRING,0);
	daomake_string_options = DMap_New(DAO_DATA_STRING,DAO_DATA_STRING);
	daomake_cmdline_defines = DMap_New(DAO_DATA_STRING,0);
	DString_SetChars( makefile, "makefile.dao" );
	for(i=1; i<argc; i++){
		char *arg = argv[i];
		if( strcmp( arg, "--platform" ) == 0 ){
			if( (i + 1) == argc ) goto ErrorMissingArgValue;
			platform = argv[++i];
		}else if( strcmp( arg, "--arch" ) == 0 ){
			if( (i + 1) == argc ) goto ErrorMissingArgValue;
			architecture = argv[++i];
		}else if( strcmp( arg, "--mode" ) == 0 ){
			if( (i + 1) == argc ) goto ErrorMissingArgValue;
			mode = argv[++i];
			if( strcmp( mode, "release" ) == 0 ){
				daomake_build_mode = DAOMAKE_RELEASE;
			}else if( strcmp( mode, "debug" ) == 0 ){
				daomake_build_mode = DAOMAKE_DEBUG;
			}else if( strcmp( mode, "profile" ) == 0 ){
				daomake_build_mode = DAOMAKE_PROFILE;
			}else{
				goto ErrorInvalidArgValue;
			}
		}else if( strstr( arg, "--option-" ) == arg ){
			DString key = DString_WrapChars( arg + 9 );
			DString value;
			daoint bl = -1;
			if( (i + 1) == argc ) goto ErrorMissingArgValue;
			value = DString_WrapChars( argv[++i] );
			DMap_Insert( daomake_string_options, & key, & value );
			if( strcmp( argv[i], "FALSE" ) ==0 || strcmp( argv[i], "false" ) ==0 ){
				bl = 0;
			}else if( strcmp( argv[i], "NO" ) ==0 || strcmp( argv[i], "no" ) ==0 ){
				bl = 0;
			}else if( strcmp( argv[i], "OFF" ) ==0 || strcmp( argv[i], "off" ) ==0 ){
				bl = 0;
			}else if( strcmp( argv[i], "TRUE" ) ==0 || strcmp( argv[i], "true" ) ==0 ){
				bl = 1;
			}else if( strcmp( argv[i], "YES" ) ==0 || strcmp( argv[i], "yes" ) ==0 ){
				bl = 1;
			}else if( strcmp( argv[i], "ON" ) ==0 || strcmp( argv[i], "on" ) ==0 ){
				bl = 1;
			}
			if( bl >= 0 ) DMap_Insert( daomake_boolean_options, & key, (void*)bl );
		}else if( strstr( arg, "--define-" ) == arg ){
			DString key = DString_WrapChars( arg + 9 );
			DString value;
			daoint bl = -1;
			if( (i + 1) == argc ) goto ErrorMissingArgValue;
			value = DString_WrapChars( argv[++i] );
			DMap_Insert( daomake_cmdline_defines, & key, & value );
		}else if( strcmp( arg, "--suffix" ) == 0 ){
			if( (i + 1) == argc ) goto ErrorMissingArgValue;
			daomake_makefile_suffix = argv[++i];
		}else if( strcmp( arg, "--reset" ) == 0 ){
			daomake_reset_cache = 1;
		}else if( strcmp( arg, "--no-local-rpath" ) == 0 ){
			daomake_local_rpath = 0;
		}else if( strcmp( arg, "--help" ) == 0 ){
			printf( "%s\n", daomake_doc_options );
			if( i == 1 && argc == 2 ) return 0;
		}else if( arg[0] == '-' ){
			fprintf( stderr, "Error: unknown argument \"%s\"!\n", arg );
			return 1;
		}else if( (i + 1) == argc ){
			DString_SetChars( makefile, argv[i] );
			if( DaoMake_IsDir( makefile->chars ) ){
				DString *file = DString_New();
				const char *names[] = { "makefile", "Makefile", "make", "Make" };
				DString_AppendPathSep( makefile );
				for(k=0; k<4; ++k){
					DString_Reset( file, 0 );
					DString_Append( file, makefile );
					DString_AppendChars( file, names[k] );
					DString_AppendChars( file, ".dao" );
					if( DaoMake_IsFile( file->chars ) ){
						DString_Assign( makefile, file );
						break;
					}
				}
				DString_Delete( file );
			}
		}
		continue;
ErrorMissingArgValue:
		fprintf( stderr, "Error: missing argument value for \"%s\"!\n", arg );
		return 1;
ErrorInvalidArgValue:
		fprintf( stderr, "Error: invalid argument value for \"%s\"!\n", arg );
		return 1;
	}

	/* Use no hashing: the same string will be hashed differently in MBS and WCS! */
	daomake_platform = DString_New();
	daomake_architecture = DString_New();
	daomake_projects = DaoMap_New(0);
	daomake_settings = DaoMap_New(0);
	daomake_platforms = DaoMap_New(0);
	daomake_packages = DaoMap_New(0);
	daomake_assemblers = DaoMap_New(0);
	daomake_compilers = DaoMap_New(0);
	daomake_linkers = DaoMap_New(0);
	daomake_includes = DaoList_New();
	daomake_variable_map  = DHash_New(DAO_DATA_STRING,DAO_DATA_STRING);
	daomake_variable_map2 = DHash_New(DAO_DATA_STRING,DAO_DATA_STRING);
	daomake_variable_map3 = DHash_New(DAO_DATA_STRING,0);
	daomake_variable_list = DList_New(DAO_DATA_STRING);
	DaoGC_IncRC( (DaoValue*) daomake_projects );
	DaoGC_IncRC( (DaoValue*) daomake_settings );
	DaoGC_IncRC( (DaoValue*) daomake_platforms );
	DaoGC_IncRC( (DaoValue*) daomake_packages );
	DaoGC_IncRC( (DaoValue*) daomake_assemblers );
	DaoGC_IncRC( (DaoValue*) daomake_compilers );
	DaoGC_IncRC( (DaoValue*) daomake_linkers );
	DaoGC_IncRC( (DaoValue*) daomake_includes );

	nspace = DaoVmSpace_GetNamespace( vmSpace, "DaoMake" );
	DaoNamespace_AddConst( vmSpace->daoNamespace, nspace->name, (DaoValue*) nspace, DAO_PERM_PUBLIC );
	DaoNamespace_AddConst( vmSpace->mainNamespace, nspace->name, (DaoValue*) nspace, DAO_PERM_PUBLIC );

	daomake_type_unit    = DaoNamespace_WrapType( nspace, & DaoMakeUnit_Typer, 0 );
	daomake_type_objects = DaoNamespace_WrapType( nspace, & DaoMakeObjects_Typer, 0 );
	daomake_type_target  = DaoNamespace_WrapType( nspace, & DaoMakeTarget_Typer, 0 );
	daomake_type_project = DaoNamespace_WrapType( nspace, & DaoMakeProject_Typer, 0 );
	DaoNamespace_WrapType( nspace, & DaoMakeVariables_Typer, 0 );
	DaoNamespace_WrapFunctions( nspace, DaoMakeMeths );

	DaoNamespace_AddValue( nspace, "Settings", (DaoValue*) daomake_settings, "map<string,string>" );
	DaoNamespace_AddValue( nspace, "Platforms", (DaoValue*) daomake_platforms, "map<string,int>" );
	DaoNamespace_AddValue( nspace, "Assemblers", (DaoValue*) daomake_assemblers, "map<string,string>" );
	DaoNamespace_AddValue( nspace, "Compilers", (DaoValue*) daomake_compilers, "map<string,string>" );
	DaoNamespace_AddValue( nspace, "Linkers", (DaoValue*) daomake_linkers, "map<string,string>" );
	DaoNamespace_AddValue( nspace, "Includes", (DaoValue*) daomake_includes, "list<string>" );
	DaoNamespace_AddValue( nspace, "Packages", (DaoValue*) daomake_packages, "map<string,tuple<includes:string,header:string,cflags:string,lflags:string>>" );

	DaoMap_AddKeyValues( daomake_assemblers, daomake_lang_assemblers );
	DaoMap_AddKeyValues( daomake_compilers, daomake_lang_compilers );
	DaoMap_AddKeyValues( daomake_linkers, daomake_lang_linkers );

	name = DString_New();
	DaoVmSpace_AddPath( vmSpace, vmSpace->daoBinPath->chars );
	DString_SetChars( name, ".." );
	Dao_MakePath( vmSpace->daoBinPath, name );
	DaoVmSpace_AddPath( vmSpace, name->chars );
	DString_SetChars( name, "../lib/daomake" );
	Dao_MakePath( vmSpace->daoBinPath, name );
	DaoVmSpace_AddPath( vmSpace, name->chars );
	if( architecture && *architecture ) DString_SetChars( daomake_architecture, architecture );
	if( platform && *platform ){
		DaoNamespace *pns;
		DString_SetChars( daomake_platform, platform );
		DString_SetChars( name, "platforms/" );
		DString_AppendChars( name, platform );
		DString_AppendChars( name, ".dao" );
		pns = DaoVmSpace_Load( vmSpace, name->chars );
		if( pns == NULL ){
			fprintf( stderr, "Error: invalid platform \"%s\"!\n", platform );
			return 1;
		}
	}

	DaoMake_MakePath( vmSpace->startPath, makefile );
	DString_Append( srcdir, makefile );
	while( srcdir->size && srcdir->chars[srcdir->size-1] != '/' ) srcdir->size -= 1;
	srcdir->chars[srcdir->size] = '\0';

	daomake_out_of_source = DString_EQ( srcdir, vmSpace->startPath ) == 0;
	daomake_main_source_path = srcdir;
	daomake_test_tool = DString_New();
	daomake_test_tool_option = DString_New();
	DString_SetChars( daomake_test_tool, "daotest" );
	DString_SetChars( daomake_test_tool_option, "--log" );

	DString_SetChars( name, daomake_objects_dir );
	DaoMake_MakePath( daomake_main_source_path, name );
	if( daomake_out_of_source ){
		DaoMake_MakeOutOfSourcePath( name, 0 );
	}else{
		DaoMake_MakeDir( name->chars );
	}

	/* Start execution. */
	k = ! DaoVmSpace_RunMain( vmSpace, makefile->chars );

	source = DString_New();
	for(it=DaoMap_First(daomake_projects); it; it=DaoMap_Next(daomake_projects,it)){
		DaoMakeProject *project = (DaoMakeProject*) it->value.pVoid;
		if( (project->targets->size + project->installs->size) == 0 ) continue;
		DMap_Insert( daomake_makefile_paths, project->base.sourcePath, 0 );
	}

	for(it=DaoMap_First(daomake_projects); it; it=DaoMap_Next(daomake_projects,it)){
		DaoMakeProject *project = (DaoMakeProject*) it->value.pVoid;
		if( (project->targets->size + project->installs->size) == 0 ) continue;
		DString_Reset( name, 0 );
		DString_Append( name, project->base.sourcePath );
		DString_AppendPathSep( name );
		DString_AppendChars( name, "Makefile" );
		DString_AppendChars( name, daomake_makefile_suffix );
		DaoMake_MakeOutOfSourcePath( name, 1 );
		if( daomake_reset_cache == 0 ){
			fin = Dao_OpenFile( name->chars, "r" );
			if( fin ){
				DaoFile_ReadAll( fin, source, 1 );
				if( DString_FindChars( source, "# Generated by DaoMake:", 0 ) != 0 ){
					fprintf( stderr, daomake_error_makefile_existing, name->chars );
					return 1;
				}
			}
		}
		fout = Dao_OpenFile( name->chars, "w+" );
		fprintf( fout, "# Generated by DaoMake: DO NOT EDIT!\n" );
		fprintf( fout, "# Targeting platform %s.\n\n", platform ? platform : "none" );
		DaoMakeProject_MakeFile( project, source );
		DaoFile_WriteString( fout, source );
		fclose( fout );

		if( project->generateFinder == 0 ) continue;
		DaoMakeProject_MakeFindPackage( project, source, 0 );
		if( source->size == 0 ) continue;

		DaoMakeProject_MakeFinderPath( project, name );

		fout = Dao_OpenFile( name->chars, "w+" );
		fprintf( fout, "# Generated by DaoMake: DO NOT EDIT!\n" );
		DaoFile_WriteString( fout, source );
		fclose( fout );
	}
	DString_Delete( name );
	DString_Delete( source );
	DString_Delete( makefile );
	DString_Delete( srcdir );

	DaoGC_DecRC( (DaoValue*) daomake_projects );
	DaoGC_DecRC( (DaoValue*) daomake_settings );
	DaoGC_DecRC( (DaoValue*) daomake_platforms );
	DaoGC_DecRC( (DaoValue*) daomake_packages );
	DaoGC_DecRC( (DaoValue*) daomake_compilers );
	DaoGC_DecRC( (DaoValue*) daomake_linkers );
	DaoGC_DecRC( (DaoValue*) daomake_includes );
	DList_Delete( daomake_variable_list );
	DMap_Delete( daomake_variable_map );
	DMap_Delete( daomake_variable_map2 );
	DMap_Delete( daomake_variable_map3 );

	DaoQuit();
	return k;
}
