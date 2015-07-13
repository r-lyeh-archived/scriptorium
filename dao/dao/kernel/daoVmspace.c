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

#include<string.h>
#include<ctype.h>
#include<locale.h>
#include<assert.h>


#include"daoNamespace.h"
#include"daoVmspace.h"
#include"daoParser.h"
#include"daoBytecode.h"
#include"daoStream.h"
#include"daoRoutine.h"
#include"daoNumtype.h"
#include"daoClass.h"
#include"daoObject.h"
#include"daoRoutine.h"
#include"daoRegex.h"
#include"daoStdlib.h"
#include"daoProcess.h"
#include"daoGC.h"
#include"daoTasklet.h"
#include"daoValue.h"

#ifdef DAO_WITH_THREAD
#include"daoThread.h"
#endif


typedef struct DaoVirtualFile DaoVirtualFile;

/*
// If DaoVirtualFile::offset is greater than zero, DaoVirtualFile::data will
// be the path to an archive file, and DaoVirtualFile::offset will be the location
// of the virtual file's data in the archive file. Otherwise, DaoVirtualFile::data
// contains the data for the virtual file.
*/
struct DaoVirtualFile
{
	daoint   offset;
	daoint   size;
	DString *data;
};

DaoVirtualFile* DaoVirtualFile_New()
{
	DaoVirtualFile *self = (DaoVirtualFile*) dao_calloc( 1, sizeof(DaoVirtualFile) );
	self->data = DString_New();
	return self;
}
void DaoVirtualFile_Delete( DaoVirtualFile *self )
{
	DString_Delete( self->data );
	dao_free( self );
}


DaoConfig daoConfig =
{
	1, /* cpu */
	0, /* jit */
	1, /* optimize */
	0, /* iscgi */
	8, /* tabspace */
};

DaoVmSpace *mainVmSpace = NULL;
DaoProcess *mainProcess = NULL;

void dao_abort( const char *error )
{
	fprintf( stderr, "Dao aborted with fatal error: %s!\n", error );
	exit(0);
}
void* dao_malloc( size_t size )
{
	void *p = malloc( size );
	if( size && p == NULL ) dao_abort( "memory allocation failed" );
	return p;
}
void* dao_calloc( size_t nmemb, size_t size )
{
	void *p = calloc( nmemb, size );
	if( nmemb && size && p == NULL ) dao_abort( "memory allocation failed" );
	return p;
}
void* dao_realloc( void *ptr, size_t size )
{
	void *p = realloc( ptr, size );
	if( size && p == NULL ) dao_abort( "memory allocation failed" );
	return p;
}
void  dao_free( void *p )
{
	free( p );
}

int DaoVmSpace_TestFile( DaoVmSpace *self, DString *fname )
{
	if( MAP_Find( self->vfiles, fname ) ) return 1;
	if( MAP_Find( self->vmodules, fname ) ) return 1;
	return Dao_IsFile( fname->chars );
}
static int TestPath( DaoVmSpace *vms, DString *fname, int type )
{
	if( type == DAO_FILE_PATH ) return DaoVmSpace_TestFile( vms, fname );
	return Dao_IsDir( fname->chars );
}
void DaoAux_Delete( DMap *aux )
{
	DNode *it;
	typedef void (*aux_delete)(void*);
	for(it=DMap_First(aux); it; it=DMap_Next(aux,it)){
		aux_delete del = (aux_delete) it->key.pVoid;
		(*del)( it->value.pVoid );
	}
	DMap_Delete( aux );
}


#define DAO_FILE_TYPE_NUM  3

static const char* const daoDllPrefix[] =
{
	"", "", DAO_DLL_PREFIX "dao_"
};
static const char* const daoFileSuffix[] =
{
	".dac", ".dao", DAO_DLL_SUFFIX
};
static int daoModuleTypes[] =
{
	DAO_MODULE_DAC, DAO_MODULE_DAO, DAO_MODULE_DLL
};

#ifndef TARGET_PLAT
#define TARGET_PLAT "Undefined"
#endif

#ifndef CHANGESET_ID
#define CHANGESET_ID "Undefined"
#endif

const char *const dao_copy_notice =
"  Dao Virtual Machine " DAO_VERSION "\n"
"  Target: " TARGET_PLAT "\n"
"  Built date: " __DATE__ "\n"
"  Changeset ID: " CHANGESET_ID "\n\n"
"  Copyright(C) 2006-2015, Fu Limin\n"
"  Dao is released under the terms of the Simplified BSD License\n"
"  Dao Language website: http://www.daovm.net\n"
;

static const char *const cmd_help =
"\n Usage: dao [options] script_file\n"
" Options:\n"
"   -h, --help:           print this help information;\n"
"   -v, --version:        print version information;\n"
"   -e, --eval:           evaluate command line codes;\n"
"   -i, --interactive:    run in interactive mode;\n"
"   -d, --debug:          run in debug mode;\n"
"   -p, --profile:        run in profile mode;\n"
"   -r, --restart:        restart program on crash (unix) or nozero exit (win);\n"
"   -c, --compile:        compile to bytecodes;\n"
"   -a, --archive:        build archive file;\n"
"   -l, --list-code:      print compiled bytecodes;\n"
"   -j, --jit:            enable just-in-time compiling;\n"
"   -Ox:                  optimization level (x=0 or 1);\n"
"   --threads=number      minimum number of threads for processing tasklets;\n"
"   --path=directory      add module searching path;\n"
"   --module=module       preloading module;\n"
"   --config=config       use configure file;\n"
;


extern DaoTypeBase  baseTyper;
extern DaoTypeBase  numberTyper;
extern DaoTypeBase  stringTyper;
extern DaoTypeBase  enumTyper;
extern DaoTypeBase  listTyper;
extern DaoTypeBase  mapTyper;
extern DaoTypeBase  ioDeviceTyper;
extern DaoTypeBase  streamTyper;
extern DaoTypeBase  routTyper;
extern DaoTypeBase  interTyper;
extern DaoTypeBase  cinTypeTyper;
extern DaoTypeBase  cinValueTyper;
extern DaoTypeBase  classTyper;
extern DaoTypeBase  objTyper;
extern DaoTypeBase  nsTyper;
extern DaoTypeBase  tupleTyper;
extern DaoTypeBase  namevaTyper;

extern DaoTypeBase  numarTyper;
extern DaoTypeBase  comTyper;
extern DaoTypeBase  abstypeTyper;
extern DaoTypeBase  rgxMatchTyper;
extern DaoTypeBase  futureTyper;
extern DaoTypeBase  channelTyper;
extern DaoTypeBase  defaultCdataTyper;

extern DaoTypeBase macroTyper;
extern DaoTypeBase regexTyper;
extern DaoTypeBase vmpTyper;
extern DaoTypeBase typeKernelTyper;

static DaoTypeBase vmsTyper=
{
	"vmspace", NULL, NULL, NULL, {0}, {0},
	(FuncPtrDel) DaoVmSpace_Delete, NULL
};

DaoTypeBase* DaoVmSpace_GetTyper( short type )
{
	switch( type ){
	case DAO_BOOLEAN  :
	case DAO_INTEGER  :  return & numberTyper;
	case DAO_FLOAT    :  return & numberTyper;
	case DAO_COMPLEX  :  return & comTyper;
	case DAO_ENUM     :  return & enumTyper;
	case DAO_STRING   :  return & stringTyper;
	case DAO_LIST     :  return & listTyper;
	case DAO_MAP      :  return & mapTyper;
	case DAO_PAR_NAMED : return & namevaTyper;
	case DAO_TUPLE     : return & tupleTyper;
#ifdef DAO_WITH_NUMARRAY
	case DAO_ARRAY  :  return & numarTyper;
#else
	case DAO_ARRAY  :  return & baseTyper;
#endif
	case DAO_CTYPE   :
	case DAO_CSTRUCT :
	case DAO_CDATA   :  return & defaultCdataTyper;
	case DAO_ROUTINE   :  return & routTyper;
	case DAO_INTERFACE :  return & interTyper;
	case DAO_CINTYPE   :  return & cinTypeTyper;
	case DAO_CINVALUE  :  return & cinValueTyper;
	case DAO_CLASS     :  return & classTyper;
	case DAO_OBJECT    :  return & objTyper;
	case DAO_NAMESPACE :  return & nsTyper;
	case DAO_PROCESS   :  return & vmpTyper;
	case DAO_VMSPACE   :  return & vmsTyper;
	case DAO_TYPE      :  return & abstypeTyper;
	case DAO_TYPEKERNEL : return & typeKernelTyper;
	default : break;
	}
	return & baseTyper;
}
const char*const DaoVmSpace_GetCopyNotice()
{
	return dao_copy_notice;
}

void DaoVmSpace_SetOptions( DaoVmSpace *self, int options )
{
	self->options = options;
}
int DaoVmSpace_GetOptions( DaoVmSpace *self )
{
	return self->options;
}
DaoNamespace* DaoVmSpace_FindNamespace( DaoVmSpace *self, DString *name )
{
	DNode *node;
	DaoNamespace *ns = NULL;
	DaoVmSpace_Lock( self );
	node = DMap_Find( self->nsModules, name );
	if( node ){
		ns = (DaoNamespace*) node->value.pValue;
		DList_PushFront( self->loadedModules, ns );
	}
	DaoVmSpace_Unlock( self );
	return ns;
}
DaoNamespace* DaoVmSpace_GetNamespace( DaoVmSpace *self, const char *name )
{
	DString str = DString_WrapChars( name );
	DaoNamespace *ns = DaoVmSpace_FindNamespace( self, & str );
	if( ns ) return ns;
	ns = DaoNamespace_New( self, name );
	DaoVmSpace_Lock( self );
	DMap_Insert( self->nsModules, & str, ns );
	DList_PushFront( self->loadedModules, ns );
	DaoVmSpace_Unlock( self );
	return ns;
}
DaoNamespace* DaoVmSpace_MainNamespace( DaoVmSpace *self )
{
	return self->mainNamespace;
}
DaoProcess* DaoVmSpace_MainProcess( DaoVmSpace *self )
{
	return self->mainProcess;
}
DaoProcess* DaoVmSpace_AcquireProcess( DaoVmSpace *self )
{
	DaoProcess *proc = NULL;
#ifdef DAO_WITH_THREAD
	DMutex_Lock( & self->mutexProc );
#endif
	if( self->processes->size ){
		proc = (DaoProcess*) DList_Back( self->processes );
		proc->active = 0;
		proc->depth = 0;
		DList_PopBack( self->processes );
	}else{
		proc = DaoProcess_New( self );
		DMap_Insert( self->allProcesses, proc, 0 );
	}
#ifdef DAO_WITH_THREAD
	DMutex_Unlock( & self->mutexProc );
#endif
	return proc;
}
void DaoVmSpace_ReleaseProcess( DaoVmSpace *self, DaoProcess *proc )
{
#ifdef DAO_WITH_THREAD
	DMutex_Lock( & self->mutexProc );
#endif
	if( DMap_Find( self->allProcesses, proc ) ){
		if( proc->factory ) DList_Clear( proc->factory );
		if( proc->aux ) DaoAux_Delete( proc->aux );
		GC_DecRC( proc->future );
		proc->future = NULL;
		proc->aux = NULL;
		DaoProcess_PopFrames( proc, proc->firstFrame );
		DList_Clear( proc->exceptions );
		DList_PushBack( self->processes, proc );
	}
#ifdef DAO_WITH_THREAD
	DMutex_Unlock( & self->mutexProc );
#endif
}

#if 0
#define SHARE_NO_PARSER
#define SHARE_NO_INFERENCER
#define SHARE_NO_OPTIMIZER
#endif

DaoParser* DaoVmSpace_AcquireParser( DaoVmSpace *self )
{
	DaoParser *parser = NULL;

#ifdef SHARE_NO_PARSER
	parser = DaoParser_New();
	parser->vmSpace = self;
	return parser;
#endif

#ifdef DAO_WITH_THREAD
	DMutex_Lock( & self->mutexMisc );
#endif
	if( self->parsers->size ){
		parser = (DaoParser*) DList_Back( self->parsers );
		DList_PopBack( self->parsers );
	}else{
		parser = DaoParser_New();
		parser->vmSpace = self;
		DMap_Insert( self->allParsers, parser, 0 );
	}
#ifdef DAO_WITH_THREAD
	DMutex_Unlock( & self->mutexMisc );
#endif
	return parser;
}
void DaoVmSpace_ReleaseParser( DaoVmSpace *self, DaoParser *parser )
{
#ifdef SHARE_NO_PARSER
	DaoParser_Delete( parser ); return;
#endif

	DaoParser_Reset( parser );
#ifdef DAO_WITH_THREAD
	DMutex_Lock( & self->mutexMisc );
#endif
	if( DMap_Find( self->allParsers, parser ) ){
		DList_PushBack( self->parsers, parser );
	}
#ifdef DAO_WITH_THREAD
	DMutex_Unlock( & self->mutexMisc );
#endif
}
DaoByteCoder* DaoVmSpace_AcquireByteCoder( DaoVmSpace *self )
{
	DaoByteCoder *byteCoder = NULL;

#ifdef SHARE_NO_PARSER
	byteCoder = DaoByteCoder_New( self );
	//byteCoder->vmSpace = self;
	return byteCoder;
#endif

#ifdef DAO_WITH_THREAD
	DMutex_Lock( & self->mutexMisc );
#endif
	if( self->byteCoders->size ){
		byteCoder = (DaoByteCoder*) DList_Back( self->byteCoders );
		DList_PopBack( self->byteCoders );
	}else{
		byteCoder = DaoByteCoder_New( self );
		//byteCoder->vmSpace = self;
		DMap_Insert( self->allByteCoders, byteCoder, 0 );
	}
#ifdef DAO_WITH_THREAD
	DMutex_Unlock( & self->mutexMisc );
#endif
	return byteCoder;
}
void DaoVmSpace_ReleaseByteCoder( DaoVmSpace *self, DaoByteCoder *byteCoder )
{
#ifdef SHARE_NO_PARSER
	DaoByteCoder_Delete( byteCoder ); return;
#endif

	DaoByteCoder_Reset( byteCoder );
#ifdef DAO_WITH_THREAD
	DMutex_Lock( & self->mutexMisc );
#endif
	if( DMap_Find( self->allByteCoders, byteCoder ) ){
		DList_PushBack( self->byteCoders, byteCoder );
	}
#ifdef DAO_WITH_THREAD
	DMutex_Unlock( & self->mutexMisc );
#endif
}
DaoInferencer* DaoVmSpace_AcquireInferencer( DaoVmSpace *self )
{
	DaoInferencer *inferencer = NULL;

#ifdef SHARE_NO_INFERENCER
	return DaoInferencer_New();
#endif

#ifdef DAO_WITH_THREAD
	DMutex_Lock( & self->mutexMisc );
#endif
	if( self->inferencers->size ){
		inferencer = (DaoInferencer*) DList_Back( self->inferencers );
		DList_PopBack( self->inferencers );
	}else{
		inferencer = DaoInferencer_New();
		DMap_Insert( self->allInferencers, inferencer, 0 );
	}
#ifdef DAO_WITH_THREAD
	DMutex_Unlock( & self->mutexMisc );
#endif
	return inferencer;
}
void DaoVmSpace_ReleaseInferencer( DaoVmSpace *self, DaoInferencer *inferencer )
{
#ifdef SHARE_NO_INFERENCER
	DaoInferencer_Delete( inferencer ); return;
#endif

	DaoInferencer_Reset( inferencer );
#ifdef DAO_WITH_THREAD
	DMutex_Lock( & self->mutexMisc );
#endif
	if( DMap_Find( self->allInferencers, inferencer ) ){
		DList_PushBack( self->inferencers, inferencer );
	}
#ifdef DAO_WITH_THREAD
	DMutex_Unlock( & self->mutexMisc );
#endif
}
DaoOptimizer* DaoVmSpace_AcquireOptimizer( DaoVmSpace *self )
{
	DaoOptimizer *optimizer = NULL;

#ifdef SHARE_NO_OPTIMIZER
	return DaoOptimizer_New();
#endif

#ifdef DAO_WITH_THREAD
	DMutex_Lock( & self->mutexMisc );
#endif
	if( self->optimizers->size ){
		optimizer = (DaoOptimizer*) DList_Back( self->optimizers );
		DList_PopBack( self->optimizers );
	}else{
		optimizer = DaoOptimizer_New();
		DMap_Insert( self->allOptimizers, optimizer, 0 );
	}
#ifdef DAO_WITH_THREAD
	DMutex_Unlock( & self->mutexMisc );
#endif
	return optimizer;
}
void DaoVmSpace_ReleaseOptimizer( DaoVmSpace *self, DaoOptimizer *optimizer )
{
#ifdef SHARE_NO_OPTIMIZER
	DaoOptimizer_Delete( optimizer ); return;
#endif

#ifdef DAO_WITH_THREAD
	DMutex_Lock( & self->mutexMisc );
#endif
	if( DMap_Find( self->allOptimizers, optimizer ) ){
		DList_PushBack( self->optimizers, optimizer );
	}
#ifdef DAO_WITH_THREAD
	DMutex_Unlock( & self->mutexMisc );
#endif
}
DaoStream* DaoVmSpace_StdioStream( DaoVmSpace *self )
{
	return self->stdioStream;
}
DaoStream* DaoVmSpace_ErrorStream( DaoVmSpace *self )
{
	return self->errorStream;
}
DaoUserStream* DaoVmSpace_SetUserStdio( DaoVmSpace *self, DaoUserStream *stream )
{
	return DaoStream_SetUserStream( self->stdioStream, stream );
}
DaoUserStream* DaoVmSpace_SetUserStdError( DaoVmSpace *self, DaoUserStream *stream )
{
	if( self->errorStream == self->stdioStream ){
		DaoStream *stream = DaoStream_New();
		GC_Assign( & self->errorStream, stream );
	}
	return DaoStream_SetUserStream( self->errorStream, stream );
}
DaoDebugger* DaoVmSpace_SetUserDebugger( DaoVmSpace *self, DaoDebugger *handler )
{
	DaoDebugger *hd = self->debugger;
	self->debugger = handler;
	return hd;
}
DaoProfiler* DaoVmSpace_SetUserProfiler( DaoVmSpace *self, DaoProfiler *profiler )
{
	DaoProfiler *hd = self->profiler;
	if( profiler == NULL || profiler->EnterFrame == NULL || profiler->LeaveFrame == NULL ){
		profiler = NULL;
	}
	self->profiler = profiler;
	return hd;
}
DaoUserHandler* DaoVmSpace_SetUserHandler( DaoVmSpace *self, DaoUserHandler *handler )
{
	DaoUserHandler *hd = self->userHandler;
	self->userHandler = handler;
	return hd;
}
void DaoVmSpace_ReadLine( DaoVmSpace *self, ReadLine fptr )
{
	self->ReadLine = fptr;
}
void DaoVmSpace_AddHistory( DaoVmSpace *self, AddHistory fptr )
{
	self->AddHistory = fptr;
}
void DaoVmSpace_Stop( DaoVmSpace *self, int bl )
{
	self->stopit = bl;
}
static int dao_default_test_inliner( DaoNamespace *NS, DString *mode, DString *vb, DString *out, int line )
{
	DString_Reset( out, 0 );
	DString_AppendChar( out, ' ' );
	return 0;
}

static int DaoVmSpace_InitModulePath( DaoVmSpace *self, DString *path, DString *buffer )
{
	DString_Reset( buffer, 0 );
	DString_AppendChars( buffer, DAO_DLL_PREFIX "dao_aux" DAO_DLL_SUFFIX );
	Dao_MakePath( path, buffer );
	if( Dao_IsFile( buffer->chars ) ){
		DaoVmSpace_AddPath( self, path->chars );
		return 1;
	}
	return 0;
}
static void DaoVmSpace_InitPath( DaoVmSpace *self )
{
	DString *file = DString_New();
	DString *path = DString_New();
	const char *const paths[] = { "../lib/dao/modules", "./lib/dao/modules" };
	char *daodir;
	int i;

	DaoVmSpace_SetPath( self, self->startPath->chars );

	DString_AppendChars( file, DAO_DIR );
	DString_AppendChars( path, "lib/dao/modules/" );
	Dao_MakePath( file, path );
	if( DaoVmSpace_InitModulePath( self, path, file ) ) goto Done;

	for(i=0; i<2; ++i){
		DString_Reset( path, 0 );
		DString_AppendChars( path, paths[i] );
		Dao_MakePath( mainVmSpace->daoBinPath, path );
		if( DaoVmSpace_InitModulePath( self, path, file ) ) goto Done;
	}

	daodir = getenv( "DAO_DIR" );
	if( daodir ){
		DString_Reset( path, 0 );
		DString_AppendChars( path, daodir );
		if( DaoVmSpace_InitModulePath( self, path, file ) ) goto Done;
	}
Done:
	DString_Delete( file );
	DString_Delete( path );
}



DaoVmSpace* DaoVmSpace_New()
{
	DaoVmSpace *self = (DaoVmSpace*) dao_calloc( 1, sizeof(DaoVmSpace) );
	DaoValue_Init( self, DAO_VMSPACE );
	self->stdioStream = DaoStream_New();
	self->errorStream = DaoStream_New();
	self->errorStream->file = stderr;
	self->daoBinPath = DString_New();
	self->startPath = DString_New();
	self->mainSource = DString_New();
	self->vfiles = DHash_New( DAO_DATA_STRING, 0 );
	self->vmodules = DHash_New( DAO_DATA_STRING, 0 );
	self->nsModules = DHash_New( DAO_DATA_STRING, 0 );
	self->pathWorking = DString_New();
	self->nameLoading = DList_New( DAO_DATA_STRING );
	self->pathLoading = DList_New( DAO_DATA_STRING );
	self->pathSearching = DList_New( DAO_DATA_STRING );
	self->virtualPaths = DList_New( DAO_DATA_STRING );
	self->sourceArchive = DList_New( DAO_DATA_STRING );
	self->processes = DList_New(0);
	self->parsers = DList_New(0);
	self->byteCoders = DList_New(0);
	self->inferencers = DList_New(0);
	self->optimizers = DList_New(0);
	self->allProcesses = DMap_New( DAO_DATA_VALUE, 0 );
	self->allParsers = DMap_New(0,0);
	self->allByteCoders = DMap_New(0,0);
	self->allInferencers = DMap_New(0,0);
	self->allOptimizers = DMap_New(0,0);
	self->loadedModules = DList_New( DAO_DATA_VALUE );

#ifdef DAO_WITH_THREAD
	DMutex_Init( & self->mutexLoad );
	DMutex_Init( & self->mutexProc );
	DMutex_Init( & self->mutexMisc );
	DCondVar_Init( & self->condvWait );
#endif

	self->daoNamespace = NULL; /* need to be set for DaoNamespace_New() */
	self->daoNamespace = DaoNamespace_New( self, "dao" );
	self->daoNamespace->vmSpace = self;
	GC_IncRC( self->daoNamespace );
	DMap_Insert( self->nsModules, self->daoNamespace->name, self->daoNamespace );
	DList_PushFront( self->loadedModules, self->daoNamespace );

	DaoNamespace_AddCodeInliner( self->daoNamespace, "test", dao_default_test_inliner );

	self->mainNamespace = DaoNamespace_New( self, "MainNamespace" );
	self->mainNamespace->vmSpace = self;
	GC_IncRC( self->mainNamespace );
	GC_IncRC( self->stdioStream );
	GC_IncRC( self->errorStream );

	self->mainProcess = DaoProcess_New( self );
	GC_IncRC( self->mainProcess );

	if( mainVmSpace ) DaoNamespace_AddParent( self->daoNamespace, mainVmSpace->daoNamespace );

#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogNew( (DaoValue*) self );
#endif
	return self;
}
void DaoVmSpace_DeleteData( DaoVmSpace *self )
{
	DNode *it;
	for(it=DMap_First(self->allParsers); it; it=DMap_Next(self->allParsers,it)){
		DaoParser_Delete( (DaoParser*) it->key.pVoid );
	}
	for(it=DMap_First(self->allByteCoders); it; it=DMap_Next(self->allByteCoders,it)){
		DaoByteCoder_Delete( (DaoByteCoder*) it->key.pVoid );
	}
	for(it=DMap_First(self->allInferencers); it; it=DMap_Next(self->allInferencers,it)){
		DaoInferencer_Delete( (DaoInferencer*) it->key.pVoid );
	}
	for(it=DMap_First(self->allOptimizers); it; it=DMap_Next(self->allOptimizers,it)){
		DaoOptimizer_Delete( (DaoOptimizer*) it->key.pVoid );
	}
	for(it=DMap_First(self->vfiles); it; it=DMap_Next(self->vfiles,it)){
		DaoVirtualFile_Delete( (DaoVirtualFile*) it->value.pVoid );
	}
	GC_DecRC( self->daoNamespace );
	GC_DecRC( self->mainNamespace );
	GC_DecRC( self->stdioStream );
	GC_DecRC( self->errorStream );
	DString_Delete( self->daoBinPath );
	DString_Delete( self->startPath );
	DString_Delete( self->mainSource );
	DString_Delete( self->pathWorking );
	DList_Delete( self->nameLoading );
	DList_Delete( self->pathLoading );
	DList_Delete( self->pathSearching );
	DList_Delete( self->virtualPaths );
	DList_Delete( self->processes );
	DList_Delete( self->loadedModules );
	DList_Delete( self->sourceArchive );
	DList_Delete( self->parsers );
	DList_Delete( self->byteCoders );
	DList_Delete( self->inferencers );
	DList_Delete( self->optimizers );
	DMap_Delete( self->vfiles );
	DMap_Delete( self->vmodules );
	DMap_Delete( self->allProcesses );
	DMap_Delete( self->allParsers );
	DMap_Delete( self->allByteCoders );
	DMap_Delete( self->allInferencers );
	DMap_Delete( self->allOptimizers );
	GC_DecRC( self->mainProcess );
	self->stdioStream = NULL;
	if( self->preloadModules ) DList_Delete( self->preloadModules );
}
void DaoVmSpace_Delete( DaoVmSpace *self )
{
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_LogDelete( (DaoValue*) self );
#endif
	if( self->stdioStream ) DaoVmSpace_DeleteData( self );
	DMap_Delete( self->nsModules );
#ifdef DAO_WITH_THREAD
	DMutex_Destroy( & self->mutexLoad );
	DMutex_Destroy( & self->mutexProc );
	DMutex_Destroy( & self->mutexMisc );
	DCondVar_Destroy( & self->condvWait );
#endif
	dao_free( self );
}
void DaoVmSpace_Lock( DaoVmSpace *self )
{
#ifdef DAO_WITH_THREAD
	DMutex_Lock( & self->mutexLoad );
#endif
}
void DaoVmSpace_Unlock( DaoVmSpace *self )
{
#ifdef DAO_WITH_THREAD
	DMutex_Unlock( & self->mutexLoad );
#endif
}
int DaoDecodeUInt16( const char *data )
{
	const uchar_t *p = (const uchar_t*) data;
	return (p[0]<<8) + p[1];
}
int DaoDecodeUInt32( const char *data )
{
	const uchar_t *p = (const uchar_t*) data;
	return (p[0]<<24) + (p[1]<<16) + (p[2]<<8) + p[3];
}
static void DaoStream_PrintFileInfo( DaoStream *self, DString *name, const char *info )
{
	DaoStream_WriteChars( self, info );
	DaoStream_WriteString( self, name );
	if( info[strlen(info)-1] == '"' ) DaoStream_WriteChar( self, '"' );
	DaoStream_WriteChars( self, ";\n" );
}
int DaoVmSpace_ReadFile( DaoVmSpace *self, DString *fname, DString *source )
{
	FILE *fin;
	DNode *node = MAP_Find( self->vfiles, fname );
	/* printf( "reading %s\n", fname->chars ); */
	if( node ){
		DaoVirtualFile *vfile = (DaoVirtualFile*) node->value.pVoid;
		if( vfile->offset == 0 ){
			DString_Assign( source, vfile->data );
			return 1;
		}
		fin = Dao_OpenFile( vfile->data->chars, "r" );
		if( fin == NULL ) goto Failed;
		source->size = 0;
		DaoFile_ReadPart( fin, source, vfile->offset, vfile->size );
		fclose( fin );
		return 1;
	}
	if( DaoFile_ReadAll( Dao_OpenFile( fname->chars, "r" ), source, 1 ) ) return 1;
Failed:
	DaoStream_PrintFileInfo( self->errorStream, fname, "ERROR: cannot open file \"" );
	return 0;
}
/* modules/debugger */
DAO_DLL void SplitByWhiteSpaces( const char *chs, DList *tokens )
{
	DString temp = DString_WrapChars( chs );
	DString *tok = DString_New();
	DString *str = & temp;
	daoint i, j, k=0, size = str->size;
	DList_Clear( tokens );
	if( (j=DString_FindChar( str, '\1', k )) != DAO_NULLPOS ){
		while( (j=DString_FindChar( str, '\1', k )) != DAO_NULLPOS ){
			if( j > k ){
				DString_SubString( str, tok, k, j-k );
				DList_Append( tokens, tok );
			}
			k = j + 1;
		}
		if( k < str->size ){
			DString_SubString( str, tok, k, str->size-k );
			DList_Append( tokens, tok );
		}
		DString_Delete( tok );
		return;
	}
	for( i=0; i<size; i++){
		if( chs[i] == '\\' && i+1 < size ){
			DString_AppendChar( tok, chs[i+1] );
			++i;
			continue;
		}else if( isspace( chs[i] ) ){
			if( tok->size > 0 ){
				DList_Append( tokens, tok );
				DString_Clear( tok );
			}
			continue;
		}
		DString_AppendChar( tok, chs[i] );
	}
	if( tok->size > 0 ) DList_Append( tokens, tok );
	DString_Delete( tok );
}

static void DaoConfigure_FromFile( const char *name );
int DaoVmSpace_TryInitDebugger( DaoVmSpace *self, const char *module );
int DaoVmSpace_TryInitProfiler( DaoVmSpace *self, const char *module );
int DaoVmSpace_TryInitJIT( DaoVmSpace *self, const char *module );
int DaoVmSpace_ParseOptions( DaoVmSpace *self, const char *options )
{
	DString *str = DString_New();
	DList *array = DList_New( DAO_DATA_STRING );
	DaoNamespace *ns;
	daoint i, j;

	SplitByWhiteSpaces( options, array );
	for( i=0; i<array->size; i++ ){
		DString *token = array->items.pString[i];
		if( self->evalCmdline ){
			DString_Append( self->mainSource, token );
			DString_AppendChar( self->mainSource, ' ' );
			continue;
		}
		if( token->chars[0] =='-' && token->size >1 && token->chars[1] =='-' ){
			if( strcmp( token->chars, "--help" ) ==0 ){
				self->options |= DAO_OPTION_HELP;
			}else if( strcmp( token->chars, "--version" ) ==0 ){
				self->options |= DAO_OPTION_VINFO;
			}else if( strcmp( token->chars, "--eval" ) ==0 ){
				self->evalCmdline = 1;
				DString_Clear( self->mainSource );
			}else if( strcmp( token->chars, "--interactive" ) ==0 ){
				self->options |= DAO_OPTION_INTERUN;
			}else if( strcmp( token->chars, "--debug" ) ==0 ){
				self->options |= DAO_OPTION_DEBUG;
			}else if( strcmp( token->chars, "--profile" ) ==0 ){
				self->options |= DAO_OPTION_PROFILE;
			}else if( strcmp( token->chars, "--restart" ) ==0 ){
			}else if( strcmp( token->chars, "--list-code" ) ==0 ){
				self->options |= DAO_OPTION_LIST_BC;
			}else if( strcmp( token->chars, "--compile" ) ==0 ){
				self->options |= DAO_OPTION_COMP_BC;
			}else if( strcmp( token->chars, "--archive" ) ==0 ){
				self->options |= DAO_OPTION_ARCHIVE;
			}else if( strcmp( token->chars, "--jit" ) ==0 ){
				self->options |= DAO_OPTION_JIT;
				daoConfig.jit = 1;
			}else if( strstr( token->chars, "--threads=" ) == token->chars ){
				daoConfig.cpu = strtol( token->chars + 10, 0, 0 );
			}else if( strstr( token->chars, "--path=" ) == token->chars ){
				DaoVmSpace_AddPath( self, token->chars + 7 );
			}else if( strstr( token->chars, "--module=" ) == token->chars ){
				if( self->preloadModules == NULL ) self->preloadModules = DList_New( DAO_DATA_VALUE );
				if( (ns = DaoVmSpace_Load( self, token->chars + 9 )) ){
					DList_Append( self->preloadModules, ns );
					DList_Append( self->mainNamespace->namespaces, ns );
					DaoNamespace_UpdateLookupTable( self->mainNamespace );
				}else{
					DaoStream_WriteChars( self->errorStream, "Preloading failed for module: " );
					DaoStream_WriteChars( self->errorStream, token->chars );
					DaoStream_WriteChars( self->errorStream, ";\n" );
				}
			}else if( strstr( token->chars, "--config=" ) == token->chars ){
				DaoConfigure_FromFile( token->chars + 9 );
			}else if( token->size ){
				DaoStream_WriteChars( self->errorStream, "Unknown option: " );
				DaoStream_WriteChars( self->errorStream, token->chars );
				DaoStream_WriteChars( self->errorStream, ";\n" );
			}
		}else if( strcmp( token->chars, "-O0" ) ==0 ){
			daoConfig.optimize = 0;
		}else if( strcmp( token->chars, "-O1" ) ==0 ){
			daoConfig.optimize = 1;
		}else{
			daoint len = token->size;
			DString_Clear( str );
			for( j=0; j<len; j++ ){
				switch( token->chars[j] ){
				case 'h' : self->options |= DAO_OPTION_HELP;      break;
				case 'v' : self->options |= DAO_OPTION_VINFO;     break;
				case 'i' : self->options |= DAO_OPTION_INTERUN;   break;
				case 'd' : self->options |= DAO_OPTION_DEBUG;     break;
				case 'p' : self->options |= DAO_OPTION_PROFILE;   break;
				case 'l' : self->options |= DAO_OPTION_LIST_BC;   break;
				case 'c' : self->options |= DAO_OPTION_COMP_BC;   break;
				case 'a' : self->options |= DAO_OPTION_ARCHIVE;   break;
				case 'j' : self->options |= DAO_OPTION_JIT;
						   daoConfig.jit = 1;
						   break;
				case 'e' : self->evalCmdline = 1;
						   DString_Clear( self->mainSource );
						   break;
				case 'r' : break;
				case '-' : break;
				default :
						   if( token->chars[j] ){
							   DString_AppendChar( str, token->chars[j] );
							   DString_AppendChar( str, ' ' );
						   }
						   break;
				}
			}
			if( str->size > 0 ){
				DaoStream_WriteChars( self->errorStream, "Unknown option: " );
				DaoStream_WriteChars( self->errorStream, str->chars );
				DaoStream_WriteChars( self->errorStream, ";\n" );
			}
		}
	}
	if( self->options & DAO_OPTION_DEBUG ) daoConfig.optimize = 0;
	DString_Delete( str );
	DList_Delete( array );
	if( daoConfig.jit && dao_jit.Compile == NULL ) DaoVmSpace_TryInitJIT( self, NULL );
	if( self->options & DAO_OPTION_PROFILE ) DaoVmSpace_TryInitProfiler( self, NULL );
	if( self->options & DAO_OPTION_DEBUG ) DaoVmSpace_TryInitDebugger( self, NULL );
	return 1;
}


static void DaoVmSpace_MakePath( DaoVmSpace *self, DString *path );
static DaoNamespace* DaoVmSpace_LoadDaoModuleExt( DaoVmSpace *self, DString *p, DList *a, int run );
static DaoNamespace* DaoVmSpace_LoadDllModule( DaoVmSpace *self, DString *libpath );

static void DaoVmSpace_ParseArguments( DaoVmSpace *self, DaoNamespace *ns,
		const char *file, DList *args, DList *argNames, DList *argValues )
{
	DString *str = DString_New();
	DString *key = DString_New();
	DString *val = DString_New();
	DList *array = args;
	daoint i, tk, offset = 0;
	int eq = 0;

	if( array == NULL && file ){
		array = DList_New( DAO_DATA_STRING );
		SplitByWhiteSpaces( file, array );
		DString_Assign( ns->name, array->items.pString[0] );
	}
	DString_Assign( val, array->items.pString[0] );
	DaoVmSpace_MakePath( self, ns->name );
	DaoNamespace_SetName( ns, ns->name->chars ); /* to update ns->path and ns->file; */
	i = 1;
	while( i < array->size ){
		DString *s = array->items.pString[i];
		i ++;
		offset = 0;
		if( s->chars[0] == '-' ){
			offset += 1;
			if( s->chars[1] == '-' ) offset += 1;
		}
		tk = DaoToken_Check( s->chars+offset, s->size-offset, & eq );
		if( tk == DTOK_IDENTIFIER && s->chars[eq+offset] == '=' ){
			DString_SubString( s, key, offset, eq );
			DString_SubString( s, val, eq+offset+1, s->size-offset-eq );
			DList_Append( argNames, key );
			DList_Append( argValues, val );
		}else if( tk == DTOK_IDENTIFIER && offset && i < array->size ){
			DString_SubString( s, key, offset, s->size-offset );
			DString_Assign( val, array->items.pString[i] );
			DList_Append( argNames, key );
			DList_Append( argValues, val );
			i += 1;
		}else{
			DString_Clear( key );
			DString_Assign( val, s );
			DList_Append( argNames, key );
			DList_Append( argValues, s );
		}
	}
	if( args == NULL ) DList_Delete( array );
	DString_Delete( key );
	DString_Delete( val );
	DString_Delete( str );
}
static DaoValue* DaoVmSpace_MakeNameArgument( DaoNamespace *NS, DString *name, DaoValue *argv )
{
	DaoEnum *em;
	DaoTuple *tuple;
	DaoType *type, *types[2];
	em = DaoNamespace_MakeSymbol( NS, name->chars );
	types[0] = em->etype;
	types[1] = dao_type_string;
	type = DaoNamespace_MakeType( NS, "tuple", DAO_TUPLE, NULL, types, 2 );
	tuple = DaoTuple_Create( type, 2, 0 );
	DaoTuple_SetItem( tuple, (DaoValue*) em, 0 );
	DaoTuple_SetItem( tuple, argv, 1 );
	return (DaoValue*) tuple;
}
static int DaoList_SetArgument( DaoList *self, int i, DaoType *type, DString *name, DaoValue *string, DaoNamespace *NS )
{
	int isnum = DaoToken_IsNumber( string->xString.value->chars, string->xString.value->size );
	DaoValue ival = {DAO_INTEGER};
	DaoValue fval = {DAO_FLOAT};
	DaoValue *argv;
	DaoType *emtype;
	DString *sym;

	switch( type->tid ){
	case DAO_INTEGER :
		ival.xInteger.value = strtoll( string->xString.value->chars, 0, 0 );
		DaoList_SetItem( self, & ival, i );
		return 10 * isnum;
	case DAO_FLOAT :
		fval.xFloat.value = strtod( string->xString.value->chars, 0 );
		DaoList_SetItem( self, & fval, i );
		return 10 * isnum;
	case DAO_ENUM :
		argv = (DaoValue*) DaoNamespace_MakeSymbol( NS, sym->chars );
		DaoList_SetItem( self, argv, i );
		if( type && DaoType_MatchValue( type, argv, NULL ) == 0 ) return 0;
		return 10;
	default :
		argv = string;
		if( name && name->size ) argv = DaoVmSpace_MakeNameArgument( NS, name, argv );
		DaoList_SetItem( self, argv, i );
		if( type && DaoType_MatchValue( type, argv, NULL ) == 0 ) return 0;
		break;
	}
	return 1;
}
static int DaoVmSpace_ConvertArguments( DaoRoutine *routine, DList *argNames, DList *argValues )
{
	DString *val;
	DaoValue sval = {DAO_STRING};
	DaoNamespace *ns = routine->nameSpace;
	DaoList *argParams = ns->argParams;
	DaoType *routype = routine->routType;
	DaoType *type;
	int set[2*DAO_MAX_PARAM];
	int i, j, s, score = 1;

	val = DString_New();
	sval.xString.value = val;
	DaoList_Clear( argParams );

	for(i=0; i<routype->nested->size - routype->variadic; ++i){
		DaoList_Append( argParams, routine->routConsts->value->items.pValue[i] );
		set[i] = 0;
	}
	for(i=0; i<argNames->size; ++i){
		DaoValue *argv = & sval;
		DString *name = argNames->items.pString[i];
		DString *value = argValues->items.pString[i];
		int ito = i;
		type = (DaoType*) DList_Back( routype->nested );
		if( i < routype->nested->size ) type = routype->nested->items.pType[i];
		DString_Assign( val, value );
		if( type && type->tid == DAO_PAR_VALIST ){
			DaoType *type2 = (DaoType*) type->aux;
			for(j=i; j<argNames->size; ++j){
				name = argNames->items.pString[j];
				value = argValues->items.pString[j];
				DString_Assign( val, value );
				DaoList_Append( argParams, NULL );
				s = DaoList_SetArgument( argParams, j, type2, name, argv, ns );
				if( s == 0 ) goto InvalidArgument;
				score += s;
			}
			break;
		}
		if( name->size ){
			DNode *node = DMap_Find( routype->mapNames, name );
			if( node ){
				ito = node->value.pInt;
				name = NULL;
			}
		}
		if( ito >= routype->nested->size ) goto InvalidArgument;
		type = (DaoType*) routype->nested->items.pType[ito]->aux;
		if( set[ito] ) goto InvalidArgument;
		set[ito] = DaoList_SetArgument( argParams, ito, type, name, argv, ns );
		if( set[ito] == 0 ) goto InvalidArgument;
		score += 10 * set[ito];
	}
	for(i=0; i<routype->nested->size - routype->variadic; ++i){
		DaoType *partype = routine->routType->nested->items.pType[i];
		if( set[i] == 0 && partype->tid != DAO_PAR_DEFAULT ) goto InvalidArgument;
	}
	DString_Delete( val );
	return score;
InvalidArgument:
	DString_Delete( val );
	return 0;
}
static DaoRoutine* DaoVmSpace_FindExplicitMain( DaoNamespace *ns, DList *argNames, DList *argValues, int *error )
{
	DString *name;
	DaoRoutine *rout = NULL;
	DaoRoutine *best = NULL;
	DaoRoutine **routs = NULL;
	int i, max = 0, count = 0;

	name = DString_New();

	*error = 0;
	DString_SetChars( name, "main" );
	i = DaoNamespace_FindConst( ns, name );
	if( i >= 0 ) rout = DaoValue_CastRoutine( DaoNamespace_GetConst( ns, i ) );
	if( rout == NULL ){
		DString_Delete( name );
		return NULL;
	}
	if( rout->overloads ){
		routs = rout->overloads->routines->items.pRoutine;
		count = rout->overloads->routines->size;
	}else{
		routs = & rout;
		count = 1;
	}
	for(i=0; i<count; ++i){
		int s = DaoVmSpace_ConvertArguments( routs[i], argNames, argValues );
		if( s > max ){
			best = routs[i];
			max = s;
		}
	}
	if( best ){
		DaoVmSpace_ConvertArguments( best, argNames, argValues );
	}else{
		*error = 1;
	}
	DString_Delete( name );
	return best;
}

DaoNamespace* DaoVmSpace_LoadEx( DaoVmSpace *self, const char *file, int run )
{
	DList *args = DList_New( DAO_DATA_STRING );
	DString *path = DString_New();
	DaoNamespace *ns = NULL;

	SplitByWhiteSpaces( file, args );
	DString_Assign( path, args->items.pString[0] );
	switch( DaoVmSpace_CompleteModuleName( self, path, DAO_MODULE_ANY ) ){
	case DAO_MODULE_DAC :
	case DAO_MODULE_DAO : ns = DaoVmSpace_LoadDaoModuleExt( self, path, args, run ); break;
	case DAO_MODULE_DLL : ns = DaoVmSpace_LoadDllModule( self, path ); break;
	case DAO_MODULE_ANY : ns = DaoVmSpace_LoadDaoModuleExt( self, path, args, run ); break; /* any suffix */
	default : break;
	}
	DList_Delete( args );
	DString_Delete( path );
	if( ns == NULL ) return 0;
	return ns;
}
DaoNamespace* DaoVmSpace_Load( DaoVmSpace *self, const char *file )
{
	return DaoVmSpace_LoadEx( self, file, 0 );
}
/* Link "ns" to the module/namespace corresponding to "mod". */
/* If the module "mod" is not loaded yet, it will be loaded first. */
/* Return the namespace corresponding to "mod". */
DaoNamespace* DaoVmSpace_LinkModule( DaoVmSpace *self, DaoNamespace *ns, const char *mod )
{
	DaoNamespace *modns = DaoVmSpace_Load( self, mod );
	if( modns == NULL ) return NULL;
	DaoNamespace_AddParent( ns, modns );
	return modns;
}

static int CheckCodeCompletion( DString *source, DaoLexer *lexer )
{
	DList *tokens = lexer->tokens;
	int i, bcount, cbcount, sbcount, tki = 0, completed = 1;

	DaoLexer_Tokenize( lexer, source->chars, DAO_LEX_COMMENT|DAO_LEX_SPACE );
	if( tokens->size ) tki = tokens->items.pToken[tokens->size-1]->type;
	switch( tki ){
	case DTOK_LB :
	case DTOK_LCB :
	case DTOK_LSB :
	case DTOK_VBT_OPEN :
	case DTOK_CMT_OPEN :
	case DTOK_MBS_OPEN :
	case DTOK_WCS_OPEN :
		completed = 0;
		break;
	}
	if( tokens->size && completed ){
		bcount = sbcount = cbcount = 0;
		for(i=0; i<tokens->size; i++){
			DaoToken *tk = tokens->items.pToken[i];
			switch( tk->type ){
			case DTOK_LB : bcount --; break;
			case DTOK_RB : bcount ++; break;
			case DTOK_LCB : cbcount --; break;
			case DTOK_RCB : cbcount ++; break;
			case DTOK_LSB : sbcount --; break;
			case DTOK_RSB : sbcount ++; break;
			default : break;
			}
		}
		if( bcount <0 || sbcount <0 || cbcount <0 ) completed = 0;
	}
	return completed;
}
static void DaoVmSpace_Interun( DaoVmSpace *self, CallbackOnString callback )
{
	DaoValue *value;
	DaoNamespace *ns;
	DaoLexer *lexer = DaoLexer_New();
	DString *input = DString_New();
	DString *line = DString_New();
	const char *varRegex = "^ %s* = %s* %S+";
	const char *srcRegex = "^ %s* %w+ %. dao .* $";
	const char *sysRegex = "^ %\\ %s* %w+ %s* .* $";
	char *chs;
	int ch;

	DString_SetChars( self->mainNamespace->name, "interactive codes" );
	self->mainNamespace->options |= DAO_NS_AUTO_GLOBAL;
	ns = DaoVmSpace_LinkModule( self, self->mainNamespace, "help" );
	value = ns ? DaoNamespace_FindData( ns, "help_message" ) : NULL;
	if( value && value->type == DAO_STRING ) printf( "%s\n", DaoValue_TryGetChars( value ) );
	while(1){
		DString_Clear( input );
		DaoValue_Clear( self->mainProcess->stackValues );
		if( self->ReadLine ){
			chs = self->ReadLine( "(dao) ", line );
			if( chs == NULL ){
				printf( "\n" );
				break;
			}
			while( chs ){
				DString_AppendChars( input, chs );
				DString_AppendChar( input, '\n' );
				if( CheckCodeCompletion( input, lexer ) ){
					DString_Trim( input, 1, 1, 0 );
					if( input->size && self->AddHistory ) self->AddHistory( input->chars );
					break;
				}
				chs = self->ReadLine( "..... ", line );
			}
		}else{
			printf( "(dao) " );
			fflush( stdout );
			ch = getchar();
			if( ch == EOF ) break;
			while( ch != EOF ){
				if( ch == '\n' ){
					if( CheckCodeCompletion( input, lexer ) ) break;
					printf("..... ");
					fflush( stdout );
				}
				DString_AppendChar( input, (char)ch );
				ch = getchar();
			}
			if( ch == EOF ) clearerr( stdin );
			DString_Trim( input, 1, 1, 0 );
		}
		if( input->size == 0 ) continue;
		self->stopit = 0;
		if( strcmp( input->chars, "q" ) == 0 ){
			break;
		}else if( DString_Match( input, sysRegex, NULL, NULL ) ){
			if( system( input->chars+1 ) ==-1) printf( "shell command failed\n" );
		}else if( DString_Match( input, srcRegex, NULL, NULL ) ){
			DString_InsertChars( input, "std.load(", 0, 0, 0 );
			DString_AppendChars( input, ", 0, 1 )" );
			if( callback ){
				(*callback)( input->chars );
				continue;
			}
			DaoProcess_Eval( self->mainProcess, self->mainNamespace, input->chars );
		}else if( DString_Match( input, varRegex, NULL, NULL ) ){
			DString_Change( input, "^ %s* = %s*", "", 0 );
			DString_InsertChars( input, "return ", 0, 0, 0 );
			if( callback ){
				(*callback)( input->chars );
				continue;
			}
			DaoProcess_Eval( self->mainProcess, self->mainNamespace, input->chars );
		}else{
			if( callback ){
				(*callback)( input->chars );
				continue;
			}
			DaoProcess_Eval( self->mainProcess, self->mainNamespace, input->chars );
		}
#ifdef DAO_WITH_CONCURRENT
		if( self->mainProcess->status >= DAO_PROCESS_SUSPENDED ){
			if( DaoCallServer_GetThreadCount() == 0 ) DaoCallServer_AddThread( NULL, NULL );
		}
		DaoCallServer_Join();
#endif
		/*
		   printf( "%s\n", input->chars );
		 */
	}
	self->mainNamespace->options &= ~DAO_NS_AUTO_GLOBAL;
	DString_Delete( line );
	DString_Delete( input );
	DaoLexer_Delete( lexer );
}

static void DaoVmSpace_PrintCode( DaoVmSpace *self )
{
	DMap *printed = DHash_New(0,0);
	DaoNamespace *ns = self->mainNamespace;
	daoint i, j;
	if( self->options & DAO_OPTION_LIST_BC ){
		for(i=ns->cstUser; i<ns->constants->size; i++){
			DaoValue *p = ns->constants->items.pConst[i]->value;
			if( p->type == DAO_ROUTINE && p != (DaoValue*) ns->mainRoutine ){
				DaoRoutine *routine = (DaoRoutine*) p;
				if( routine->overloads == NULL ){
					if( DMap_Find( printed, routine ) ) continue;
					DMap_Insert( printed, routine, NULL );
					DaoRoutine_PrintCode( routine, self->stdioStream );
				}else{
					for(j=0; j<routine->overloads->routines->size; ++j){
						DaoRoutine *rout = routine->overloads->routines->items.pRoutine[j];
						if( DMap_Find( printed, rout ) ) continue;
						DMap_Insert( printed, rout, NULL );
						DaoRoutine_PrintCode( rout, self->stdioStream );
					}
				}
			}else if( p->type == DAO_CLASS ){
				if( DMap_Find( printed, p ) ) continue;
				DMap_Insert( printed, p, NULL );
				DaoStream_WriteChars( self->stdioStream, "\n\n" );
				DaoClass_PrintCode( & p->xClass, self->stdioStream );
			}
		}
		DaoStream_Flush( self->stdioStream );
		if( ns->mainRoutine ) DaoRoutine_PrintCode( ns->mainRoutine, self->stdioStream );
	}
	DMap_Delete( printed );
}
static void DaoVmSpace_ExeCmdArgs( DaoVmSpace *self )
{
	if( self->options & DAO_OPTION_VINFO ){
		DaoStream_WriteNewLine( self->stdioStream );
		DaoStream_WriteChars( self->stdioStream, dao_copy_notice );
		DaoStream_WriteNewLine( self->stdioStream );
	}
	if( self->options & DAO_OPTION_HELP )  DaoStream_WriteChars( self->stdioStream, cmd_help );
	DaoStream_Flush( self->stdioStream );

	DaoVmSpace_PrintCode( self );
}
void DaoVmSpace_ConvertPath( DaoVmSpace *self, DString *path )
{
	char *daodir = getenv( "DAO_DIR" );
	char *home = getenv( "HOME" );

	if( DString_Find( path, self->startPath, 0 ) == 0 ){
		DString_ReplaceChars( path, "$(CMD_DIR)/", 0, self->startPath->size );
	}else if( DString_Find( path, self->daoBinPath, 0 ) == 0 ){
		DString_ReplaceChars( path, "$(EXE_DIR)/", 0, self->daoBinPath->size );
	}else if( daodir && DString_FindChars( path, daodir, 0 ) == 0 ){
		DString_ReplaceChars( path, "$(DAO_DIR)", 0, strlen(daodir) );
	}else if( home && DString_FindChars( path, home, 0 ) == 0 ){
		DString_ReplaceChars( path, "$(HOME)", 0, strlen(home) );
	}
}
void DaoVmSpace_ConvertPath2( DaoVmSpace *self, DString *path )
{
	DString *pathLoading = self->pathLoading->items.pString[0];
	char *daodir = getenv( "DAO_DIR" );
	char *home = getenv( "HOME" );

	if( DString_FindChars( path, "$(DAR_DIR)/", 0 ) == 0 ){
		DString_Replace( path, pathLoading, 0, strlen( "$(DAR_DIR)/" ) );
	}else if( DString_FindChars( path, "$(CMD_DIR)/", 0 ) == 0 ){
		DString_Replace( path, self->startPath, 0, strlen( "$(CMD_DIR)/" ) );
	}else if( DString_FindChars( path, "$(EXE_DIR)/", 0 ) == 0 ){
		DString_Replace( path, self->daoBinPath, 0, strlen( "$(EXE_DIR)/" ) );
	}else if( daodir && DString_FindChars( path, "$(DAO_DIR)", 0 ) == 0 ){
		DString_ReplaceChars( path, daodir, 0, strlen( "$(DAO_DIR)" ) );
	}else if( home && DString_FindChars( path, "$(HOME)", 0 ) == 0 ){
		DString_ReplaceChars( path, home, 0, strlen( "$(HOME)" ) );
	}
}
void DaoVmSpace_SaveByteCodes( DaoVmSpace *self, DaoByteCoder *coder, DaoNamespace *ns )
{
	FILE *fout;
	DString *fname = DString_New();
	DString *output = DString_New();

	DString_Append( fname, ns->name );
	if( fname->size > ns->lang->size ) fname->size -= ns->lang->size;
	DString_AppendChars( fname, "dac" );
	fout = Dao_OpenFile( fname->chars, "w+" );

	DaoByteCoder_EncodeHeader( coder, ns->name->chars, output );
	DaoByteCoder_EncodeToString( coder, output );

	DaoFile_WriteString( fout, output );
	DString_Delete( output );
	fclose( fout );

	DaoStream_WriteChars( self->stdioStream, "Source file: " );
	DaoStream_WriteString( self->stdioStream, ns->name );
	DaoStream_WriteChar( self->stdioStream, '\n' );
	DaoStream_WriteChars( self->stdioStream, "Compiled to: " );
	DaoStream_WriteString( self->stdioStream, fname );
	DaoStream_WriteChars( self->stdioStream, "\n\n" );
	DString_Delete( fname );
}

void DString_AppendUInt16( DString *bytecodes, int value )
{
	uchar_t bytes[2];
	bytes[0] = (value >> 8) & 0xFF;
	bytes[1] = value & 0xFF;
	DString_AppendBytes( bytecodes, (char*) bytes, 2 );
}
void DString_AppendUInt32( DString *bytecodes, uint_t value )
{
	uchar_t bytes[4];
	bytes[0] = (value >> 24) & 0xFF;
	bytes[1] = (value >> 16) & 0xFF;
	bytes[2] = (value >>  8) & 0xFF;
	bytes[3] = value & 0xFF;
	DString_AppendBytes( bytecodes, (char*) bytes, 4 );
}

/*
// Archive File Format:
// -- Header:
//    Byte[4]   # Signature: \33\33\r\n
//    Byte[4]   # Count of files in the archive;
// -- Body (repetition of the following structure):
//    Byte[2]   # Length of the file name;
//    Byte[]    # File name;
//    Byte[4]   # File size;
//    Byte[]    # File data;
// Note: the count, lengths and sizes are encoded in big endian;
*/
static void DaoVmSpace_SaveArchive( DaoVmSpace *self, DList *argValues )
{
	FILE *fin, *fout, *fout2;
	int i, count = 1;
	int slen = strlen( DAO_DLL_SUFFIX );
	DaoNamespace *ns = self->mainNamespace;
	DMap *archives = DMap_New(DAO_DATA_STRING,DAO_DATA_STRING);
	DMap *counts = DMap_New(DAO_DATA_STRING,0);
	DString *archive = DString_New();
	DString *group = DString_New();
	DString *data = DString_New();
	DString *pathLoading;
	DNode *it, *it2;

	DString_Append( archive, ns->name );
	if( archive->size > ns->lang->size ) archive->size -= ns->lang->size;
	DString_AppendChars( archive, "dar" );

	DaoStream_PrintFileInfo( self->stdioStream, archive, "Creating Dao archive file: " );

	fout = Dao_OpenFile( archive->chars, "w+" );
	archive->size = 0;

	count = self->sourceArchive->size/2;
	/*
	// Store the scripts and modules in the main archive:
	// The archive file is created in the same directory as the main script;
	*/
	for(i=0; i<self->sourceArchive->size; i+=2){
		DString *name = self->sourceArchive->items.pString[i];
		DString *source = self->sourceArchive->items.pString[i+1];
		DaoStream_PrintFileInfo( self->stdioStream, name, "Adding to the archive: " );
		DaoVmSpace_ConvertPath( self, name );
		DString_AppendUInt16( archive, name->size );
		DString_Append( archive, name );
		DString_AppendUInt32( archive, source->size );
		DString_Append( archive, source );
		if( DString_FindChars( name, DAO_DLL_SUFFIX, 0 ) != name->size - slen ){
			// TODO:
		}
	}

	/*
	// Store the resource files in the main archive or separated archives:
	// -- Resources prefixed with <GroupName>:: are stored in separated
	//    archives which are identified by the <GroupName>;
	// -- Resources without group prefixes are stored in the main archive;
	*/
	for(i=0; i<argValues->size; ++i){
		DString *archsource = archive;
		DString *file = argValues->items.pString[i];
		int pos = DString_FindChars( file, "::", 0 );
		DString_Reset( group, 0 );
		DString_Assign( data, file );
		if( pos != DAO_NULLPOS ){
			DString_SubString( file, group, 0, pos );
			DString_SubString( file, data, pos+2, -1 );
		}
		Dao_MakePath( self->pathWorking, data );
		fin = Dao_OpenFile( data->chars, "r" );
		if( fin == NULL ){
			DaoStream_PrintFileInfo( self->errorStream, data, "WARNING: cannot open resource: " );
			continue;
		}
		DaoStream_PrintFileInfo( self->stdioStream, data, "Adding to the archive(s): " );

		if( group->size == 0 ){
			count += 1;
		}else{
			it = DMap_Find( archives, group );
			it2 = DMap_Find( counts, group );
			if( it == NULL ){
				it = DMap_Insert( archives, group, group );
				it2 = DMap_Insert( counts, group, 0 );
				it->value.pString->size = 0;
			}
			archsource = it->value.pString;
			it2->value.pInt += 1;
		}
		DaoVmSpace_ConvertPath( self, data );
		DString_AppendUInt16( archsource, data->size );
		DString_Append( archsource, data );
		DaoFile_ReadAll( fin, data, 1 );
		DString_AppendUInt32( archsource, data->size );
		DString_Append( archsource, data );
	}

	/*
	// Created separated archives for each resource group:
	// Such archives are created in the same directory as the main archive;
	*/
	for(it=DMap_First(archives); it; it=DMap_Next(archives,it)){
		DString_Assign( group, it->key.pString );
		it2 = DMap_Find( counts, group );
		Dao_MakePath( ns->path, group );
		DString_AppendChars( group, ".dar" );
		fout2 = Dao_OpenFile( group->chars, "w+" );
		fprintf( fout2, "\33\33\r\n" );
		DString_Clear( data );
		DString_AppendUInt32( data, it2->value.pInt );
		DaoFile_WriteString( fout2, data );
		DaoFile_WriteString( fout2, it->value.pString );
		fclose( fout2 );

		/*
		// Add place holders in the main archive to reference the separated archives:
		// Byte[ 2]:  0x00 0xA
		// Byte[10]:  $(ARCHIVE)
		// Byte[ 4]:  <LengthOfTheFollowingString>
		// Byte[  ]:  $(DAR_DIR)/<GroupName>.dar
		*/
		count += 1;
		pathLoading = self->pathLoading->items.pString[0];
		if( DString_Find( group, pathLoading, 0 ) == 0 ){
			DString_ReplaceChars( group, "$(DAR_DIR)/", 0, pathLoading->size );
		}
		DaoVmSpace_ConvertPath( self, group );
		DString_AppendUInt16( archive, 10 );
		DString_AppendChars( archive, "$(ARCHIVE)" );
		DString_AppendUInt32( archive, group->size );
		DString_Append( archive, group );
	}

	fprintf( fout, "\33\33\r\n" );
	DString_Clear( data );
	DString_AppendUInt32( data, count );
	DaoFile_WriteString( fout, data );
	DaoFile_WriteString( fout, archive );
	fclose( fout );

	DMap_Delete( archives );
	DMap_Delete( counts );
	DString_Delete( archive );
	DString_Delete( group );
	DString_Delete( data );
}
void DaoVmSpace_LoadArchive( DaoVmSpace *self, DString *archive, DString *group )
{
	DString *name, *source;
	DaoVModule module = { NULL, 0, NULL, NULL };
	char *data = (char*) archive->chars;
	int slen = strlen( DAO_DLL_SUFFIX );
	int pos = 4, size = archive->size;
	int i, m, n, files;

	if( group == NULL ) DString_Clear( self->mainSource );
	if( size < 8 ) return;

	/*
	// Push the archive name and path to the ::nameLoading and ::pathLoading stacks
	// for resolving the $(DAR_DIR) variables in the references to other archives:
	*/
	if( group != NULL ){
		DString *name = DList_PushFront( self->nameLoading, group );
		DString *path = DList_PushFront( self->pathLoading, group );
		DString_Change( name, "^ .* ([^/\\]+) $", "%1", 0 );
		DString_Change( path, "[^/\\]* $", "", 0 );
	}

	name = DString_New();
	source = DString_New();
	data = (char*) archive->chars;
	files = DaoDecodeUInt32( data + pos );
	pos += 4;
	for(i=0; i<files; ++i){
		if( (pos + 2) >= size ) break;
		m = DaoDecodeUInt16( data + pos );
		if( (pos + 2 + m + 4) >= size ) break;
		n = DaoDecodeUInt32( data + pos + 2 + m );
		DString_SetBytes( name, data + pos + 2, m );
		if( i == 0 && group == NULL ){ /* Retrieve the main script: */
			DString_SetChars( self->mainSource, "/@/" );
			DString_AppendBytes( self->mainSource, data + pos + 2, m );
			DaoNamespace_SetName( self->mainNamespace, self->mainSource->chars );
			DString_SetBytes( self->mainSource, data + pos + 2 + m + 4, n );
		}else if( strcmp( name->chars, "$(ARCHIVE)" ) == 0 ){ /* Load referenced archive: */
			FILE *fin;
			DString_SetChars( name, data + pos + 2 + m + 4 );
			DaoVmSpace_ConvertPath2( self, name );
			Dao_MakePath( self->startPath, name );
			fin = Dao_OpenFile( name->chars, "r" );
			if( fin == NULL ){
				DaoStream_PrintFileInfo( self->errorStream, name, "WARNING: cannot open archive: " );
				continue;
			}
			DaoFile_ReadAll( fin, source, 1 );
			DaoVmSpace_LoadArchive( self, source, name );
		}else if( DString_FindChars( name, DAO_DLL_SUFFIX, 0 ) != m - slen ){
			module.name = name->chars;
			module.length = n;
			module.data = (uchar_t*) data + pos + 2 + m + 4;
			if( group ){
				/* Negative length to indicate the file is not from the main archive: */
				module.length = - (pos + 2 + m);
				module.data = (uchar_t*)group->chars;
			}
			DaoVmSpace_AddVirtualModule( self, & module );
		}else{
			/* Ignore DLL modules: */
		}
		pos += 2 + m + 4 + n;
	}
	if( group != NULL ){
		DList_PopFront( self->nameLoading );
		DList_PopFront( self->pathLoading );
	}
	DString_Delete( source );
	DString_Delete( name );
}
int DaoVmSpace_Eval( DaoVmSpace *self, const char *src )
{
	DaoProcess *process = DaoVmSpace_AcquireProcess( self );
	DaoNamespace *ns = DaoNamespace_New( self, "?" );
	int retc = DaoProcess_Eval( process, ns, src );
	DaoVmSpace_ReleaseProcess( self, process );
	DaoGC_TryDelete( (DaoValue*) ns );
	return retc;
}
int DaoVmSpace_RunMain( DaoVmSpace *self, const char *file )
{
	DaoNamespace *ns = self->mainNamespace;
	DaoProcess *vmp = self->mainProcess;
	DaoStream *io = self->errorStream;
	DaoRoutine *mainRoutine, *expMain = NULL;
	DaoValue **ps;
	DList *argNames;
	DList *argValues;
	size_t tm = 0;
	daoint N;
	int res;

	if( file == NULL || file[0] ==0 || self->evalCmdline ){
		DList_PushFront( self->nameLoading, self->pathWorking );
		DList_PushFront( self->pathLoading, self->pathWorking );
		if( self->evalCmdline ){
			DaoRoutine *rout;
			DString_SetChars( self->mainNamespace->name, "command line codes" );
			if( DaoProcess_Compile( vmp, ns, self->mainSource->chars ) ==0 ) return 0;
			DaoVmSpace_ExeCmdArgs( self );
			rout = ns->mainRoutines->items.pRoutine[ ns->mainRoutines->size-1 ];
			if( DaoProcess_Call( vmp, rout, NULL, NULL, 0 ) ) return 0;
		}else{
			DaoVmSpace_ExeCmdArgs( self );
		}
		if( (self->options & DAO_OPTION_INTERUN) && self->userHandler == NULL )
			DaoVmSpace_Interun( self, NULL );
		return 1;
	}
	argNames = DList_New( DAO_DATA_STRING );
	argValues = DList_New( DAO_DATA_STRING );
	DaoVmSpace_ParseArguments( self, ns, file, NULL, argNames, argValues );
	DaoVmSpace_AddPath( self, ns->path->chars );
	DList_PushFront( self->nameLoading, ns->name );
	DList_PushFront( self->pathLoading, ns->path );
	DList_PushFront( self->loadedModules, ns );
	if( DMap_Find( self->nsModules, ns->name ) == NULL ){
		MAP_Insert( self->nsModules, ns->name, ns );
	}

	tm = Dao_FileChangedTime( ns->name->chars );
	ns->time = tm;

	/* self->fileName may has been changed */
	res = DaoVmSpace_ReadFile( self, ns->name, self->mainSource );
	if( strncmp( self->mainSource->chars, "\33\33\r\n", 4 ) == 0 ){
		DString *archive = DString_Copy( self->mainSource );
		DaoVmSpace_LoadArchive( self, archive, NULL );
		DString_Delete( archive );
	}
	if( self->options & DAO_OPTION_ARCHIVE ){
		DList_Append( self->sourceArchive, ns->name );
		DList_Append( self->sourceArchive, self->mainSource );
	}
	if( self->mainSource->chars[0] == DAO_BC_SIGNATURE[0] ){
		DaoByteCoder *byteCoder = DaoVmSpace_AcquireByteCoder( self );
		res = DaoByteCoder_Decode( byteCoder, self->mainSource );
		if( self->options & DAO_OPTION_LIST_BC ) DaoByteCoder_Disassemble( byteCoder );
		res = res && DaoByteCoder_Build( byteCoder, ns );
		DaoVmSpace_ReleaseByteCoder( self, byteCoder );
	}else{
		DaoParser *parser = DaoVmSpace_AcquireParser( self );

		if( self->options & DAO_OPTION_COMP_BC ){
			parser->byteCoder = DaoVmSpace_AcquireByteCoder( self );
			parser->byteBlock = DaoByteCoder_Init( parser->byteCoder );
		}
		parser->nameSpace = ns;
		DString_Assign( parser->fileName, ns->name );
		res = res && DaoParser_LexCode( parser, self->mainSource->chars, 1 );
		res = res && DaoParser_ParseScript( parser );

		if( res && (self->options & DAO_OPTION_COMP_BC) ){
			DaoVmSpace_SaveByteCodes( self, parser->byteCoder, ns );
			if( self->options & DAO_OPTION_LIST_BC ){
				DaoByteCoder_Disassemble( parser->byteCoder );
			}
		}
		if( parser->byteCoder ) DaoVmSpace_ReleaseByteCoder( self, parser->byteCoder );
		DaoVmSpace_ReleaseParser( self, parser );
	}
	if( res && !(self->options & DAO_OPTION_ARCHIVE) ){
		DString name = DString_WrapChars( "main" );
		int error = 0;
		expMain = DaoVmSpace_FindExplicitMain( ns, argNames, argValues, & error );
		if( error ){
			res = 0;
			DaoVmSpace_PrintCode( self );
			DaoStream_WriteChars( io, "ERROR: invalid command line arguments.\n" );
		}
	}
	DList_Delete( argNames );

	if( res == 0 ){
		DList_Delete( argValues );
		return 0;
	}

	if( self->options & DAO_OPTION_ARCHIVE ){
		DaoVmSpace_SaveArchive( self, argValues );
		DList_Delete( argValues );
		return 1;
	}
	DList_Delete( argValues );
	if( self->options & DAO_OPTION_COMP_BC ) return 1;

	mainRoutine = ns->mainRoutine;

	DaoVmSpace_ExeCmdArgs( self );
	/* always execute default __main__() routine first for initialization: */
	if( mainRoutine ){
		DaoProcess_PushRoutine( vmp, mainRoutine, NULL );
		DaoProcess_Execute( vmp );
	}
	/* check and execute explicitly defined main() routine  */
	ps = ns->argParams->value->items.pValue;
	N = ns->argParams->value->size;
	if( expMain != NULL ){
		int ret = DaoProcess_Call( vmp, expMain, NULL, ps, N );
		if( ret == DAO_ERROR_PARAM ){
			DaoStream_WriteChars( io, "ERROR: invalid command line arguments.\n" );
		}
		if( ret ) return 0;
	}
	if( (self->options & DAO_OPTION_INTERUN) && self->userHandler == NULL )
		DaoVmSpace_Interun( self, NULL );

	return 1;
}
int DaoVmSpace_CompleteModuleName( DaoVmSpace *self, DString *fname, int types )
{
	int i, modtype = DAO_MODULE_NONE;
	daoint slen = strlen( DAO_DLL_SUFFIX );
	daoint size;

	size = fname->size;
	if( (types & DAO_MODULE_DAC) && size >4 && DString_FindChars( fname, ".dac", 0 ) == size-4 ){
		DaoVmSpace_SearchPath( self, fname, DAO_FILE_PATH, 1 );
		if( DaoVmSpace_TestFile( self, fname ) ) modtype = DAO_MODULE_DAC;
	}else if( (types & DAO_MODULE_DAO) && size >4 && DString_FindChars( fname, ".dao", 0 ) == size-4 ){
		DaoVmSpace_SearchPath( self, fname, DAO_FILE_PATH, 1 );
		if( DaoVmSpace_TestFile( self, fname ) ) modtype = DAO_MODULE_DAO;
	}else if( types == DAO_MODULE_ANY && size >4 && DString_FindChars( fname, ".cgi", 0 ) == size-4 ){
		DaoVmSpace_SearchPath( self, fname, DAO_FILE_PATH, 1 );
		if( DaoVmSpace_TestFile( self, fname ) ) modtype = DAO_MODULE_DAO;
	}else if( (types & DAO_MODULE_DLL) && size > slen && DString_FindChars( fname, DAO_DLL_SUFFIX, 0 ) == size - slen ){
		DaoVmSpace_SearchPath( self, fname, DAO_FILE_PATH, 1 );
		if( DaoVmSpace_TestFile( self, fname ) ) modtype = DAO_MODULE_DLL;
	}else{
		DString *fn = DString_New();
		DString *path = DString_New();
		DString *file = DString_New();
		daoint pos = fname->size;
		while( pos && (fname->chars[pos-1] == '_' || isalnum( fname->chars[pos-1] )) ) pos -= 1;
		if( pos && (fname->chars[pos-1] == '/' || fname->chars[pos-1] == '\\') ){
			DString_SubString( fname, path, 0, pos );
			DString_SubString( fname, file, pos, fname->size - pos );
		}else{
			DString_Assign( file, fname );
		}
		for(i=0; i<DAO_FILE_TYPE_NUM; i++){
			if( !(types & daoModuleTypes[i]) ) continue;
			if( daoModuleTypes[i] < DAO_MODULE_DLL ){
				DString_Assign( fn, fname );
			}else if( strstr( fname->chars, "dao_" ) == fname->chars ){
				/* See modules/canvas/canvas.dao; */
				DString_Assign( fn, path );
				DString_AppendChars( fn, DAO_DLL_PREFIX "" );
				DString_Append( fn, file );
			}else{
				if( strstr( fname->chars, daoDllPrefix[i] ) == fname->chars ) break;
				DString_Assign( fn, path );
				DString_AppendChars( fn, daoDllPrefix[i] );
				DString_Append( fn, file );
			}
			DString_AppendChars( fn, daoFileSuffix[i] );
			DaoVmSpace_SearchPath( self, fn, DAO_FILE_PATH, 1 );

			if( DaoVmSpace_TestFile( self, fn ) ){
				modtype = daoModuleTypes[i];
				if( modtype > DAO_MODULE_DLL ) modtype = DAO_MODULE_DLL;
				DString_Assign( fname, fn );
				break;
			}
		}
		if( modtype == DAO_MODULE_NONE ){
			DaoVmSpace_SearchPath( self, fname, DAO_FILE_PATH, 1 );
			if( DaoVmSpace_TestFile( self, fname ) ) modtype = DAO_MODULE_ANY;
		}else if( modtype == DAO_MODULE_DAC ){
			size_t tmdac = Dao_FileChangedTime( fname->chars );
			fname->chars[ fname->size - 1 ] = 'o';  /* .dac to .dao; */
			if( DaoVmSpace_TestFile( self, fname ) ){
				size_t tmdao = Dao_FileChangedTime( fname->chars );
				/* Check if the source file has been changed: */
				if( tmdac < tmdao ) modtype = DAO_MODULE_DAO;
			}
			if( modtype == DAO_MODULE_DAC ) fname->chars[ fname->size - 1 ] = 'c';
		}
		DString_Delete( fn );
		DString_Delete( path );
		DString_Delete( file );
	}
	return modtype;
}
static void DaoVmSpace_PopLoadingNamePath( DaoVmSpace *self, int path )
{
	DaoVmSpace_Lock( self );
	if( path ) DList_PopFront( self->pathLoading );
	DList_PopFront( self->nameLoading );
	DaoVmSpace_Unlock( self );
}
/*
// Loading module in Dao source file.
// The first time the module is loaded:
//   (1) its implicit main (codes outside of any class and function) is executed;
//   (2) then, its explicit main that matches with "args" will be executed.
// The next time the module is loaded:
//   (1) its implicit main is executed, IF run != 0; (mainly for IDE)
//   (2) its explicit main that matches with "args" will be executed.
*/
DaoNamespace* DaoVmSpace_LoadDaoModuleExt( DaoVmSpace *self, DString *libpath, DList *args, int run )
{
	DString *source = NULL;
	DList *argNames = NULL, *argValues = NULL;
	DaoNamespace *ns = NULL, *ns2 = NULL;
	DaoRoutine *mainRoutine = NULL;
	DaoParser *parser = NULL;
	DaoProcess *process;
	daoint nsCount = self->loadedModules->size;
	int poppath = 0;
	size_t tm = 0;

	if( args ){
		argNames = DList_New( DAO_DATA_STRING );
		argValues = DList_New( DAO_DATA_STRING );
	}

	ns = ns2 = DaoVmSpace_FindNamespace( self, libpath );

	tm = Dao_FileChangedTime( libpath->chars );
	/* printf( "time = %lli,  %s  %p\n", tm, libpath->chars, node ); */
	if( ns && ns->time >= tm ){
		if( args ) DaoVmSpace_ParseArguments( self, ns, NULL, args, argNames, argValues );
		if( run ) goto ExecuteImplicitMain;
		goto ExecuteExplicitMain;
	}

	source = DString_New();
	if( ! DaoVmSpace_ReadFile( self, libpath, source ) ) goto LoadingFailed;

	if( sizeof(dao_integer) != 8 || sizeof(dao_float) != 8 ){
		int daofile = DString_Match( libpath, "%w %. dao $", 0, 0 );
		int daocmd = DString_Match( source, "^ #! [^%n]* dao %s* {{\n}}", 0, 0 );
		if( daofile == 0 && daocmd == 0 ) goto LoadingFailed;
	}

	if( self->options & DAO_OPTION_ARCHIVE ){
		DList_Append( self->sourceArchive, libpath );
		DList_Append( self->sourceArchive, source );
	}

	/*
	   printf("%p : loading %s\n", self, libpath->chars );
	 */
	ns = DaoNamespace_New( self, libpath->chars );
	ns->time = tm;
	if( args ) DaoVmSpace_ParseArguments( self, ns, NULL, args, argNames, argValues );

	DaoVmSpace_Lock( self );
	DList_PushFront( self->loadedModules, ns );
	MAP_Insert( self->nsModules, libpath, ns );
	DList_PushFront( self->nameLoading, ns->name );
	if( ns->path->size ) DList_PushFront( self->pathLoading, ns->path );
	DaoVmSpace_Unlock( self );
	poppath = ns->path->size;

	if( source->chars[0] == DAO_BC_SIGNATURE[0] ){
		DaoByteCoder *byteCoder = DaoVmSpace_AcquireByteCoder( self );
		int bl = DaoByteCoder_Decode( byteCoder, source );
		if( self->options & DAO_OPTION_LIST_BC ) DaoByteCoder_Disassemble( byteCoder );
		bl = bl && DaoByteCoder_Build( byteCoder, ns );
		DaoVmSpace_ReleaseByteCoder( self, byteCoder );
		if( bl == 0 ) goto LoadingFailed;
	}else{
		parser = DaoVmSpace_AcquireParser( self );
		parser->vmSpace = self;
		parser->nameSpace = ns;
		DString_Assign( parser->fileName, libpath );
		if( ! DaoParser_LexCode( parser, DString_GetData( source ), 1 ) ) goto LoadingFailed;
		if( self->options & DAO_OPTION_COMP_BC ){
			parser->byteCoder = DaoVmSpace_AcquireByteCoder( self );
			parser->byteBlock = DaoByteCoder_Init( parser->byteCoder );
		}
		if( ! DaoParser_ParseScript( parser ) ) goto LoadingFailed;
		if( ns->mainRoutine == NULL ) goto LoadingFailed;
		DString_SetChars( ns->mainRoutine->routName, "__main__" );
		if( parser->byteCoder ){
			DaoVmSpace_SaveByteCodes( self, parser->byteCoder, ns );
			DaoVmSpace_ReleaseByteCoder( self, parser->byteCoder );
		}
		DaoVmSpace_ReleaseParser( self, parser );
		parser = NULL;
	}

ExecuteImplicitMain :
	if( ns->mainRoutine->body->vmCodes->size > 1 ){
		int status;
		process = DaoVmSpace_AcquireProcess( self );
		DaoVmSpace_Lock( self );
		DList_PushFront( self->nameLoading, ns->path );
		DList_PushFront( self->pathLoading, ns->path );
		DaoVmSpace_Unlock( self );
		DaoProcess_PushRoutine( process, ns->mainRoutine, NULL );
		DaoProcess_Execute( process );
		status = process->status;
		DaoVmSpace_ReleaseProcess( self, process );
		DaoVmSpace_Lock( self );
		DList_PopFront( self->nameLoading );
		DList_PopFront( self->pathLoading );
		DaoVmSpace_Unlock( self );
		if( status == DAO_PROCESS_ABORTED ) goto LoadingFailed;
	}

ExecuteExplicitMain :

	/* check and execute explicitly defined main() routine  */
	if( args ){
		int error = 0;
		mainRoutine = DaoVmSpace_FindExplicitMain( ns, argNames, argValues, & error );
		if( error ) goto InvalidArgument;
	}
	if( mainRoutine != NULL ){
		int ret, N = ns->argParams->value->size;
		DaoValue **ps = ns->argParams->value->items.pValue;
		process = DaoVmSpace_AcquireProcess( self );
		ret = DaoProcess_Call( process, mainRoutine, NULL, ps, N );
		DaoVmSpace_ReleaseProcess( self, process );
		if( ret == DAO_ERROR_PARAM ) goto InvalidArgument;
		if( ret ) goto LoadingFailed;
	}
	DaoVmSpace_PopLoadingNamePath( self, poppath );
	if( self->loadedModules->size > (nsCount+1) ){
		DaoVmSpace_Lock( self );
		DList_Erase( self->loadedModules, 0, self->loadedModules->size - (nsCount+1) );
		DaoVmSpace_Unlock( self );
	}
	if( source ) DString_Delete( source );
	if( argNames ) DList_Delete( argNames );
	if( argValues ) DList_Delete( argValues );
	return ns;
InvalidArgument:
	DaoStream_WriteChars( self->errorStream, "ERROR: invalid arguments for the explicit main().\n" );
LoadingFailed :
	DaoVmSpace_PopLoadingNamePath( self, poppath );
	if( self->loadedModules->size > nsCount ){
		DaoVmSpace_Lock( self );
		DList_Erase( self->loadedModules, 0, self->loadedModules->size - nsCount );
		DaoVmSpace_Unlock( self );
	}
	if( source ) DString_Delete( source );
	if( argNames ) DList_Delete( argNames );
	if( argValues ) DList_Delete( argValues );
	if( parser ) DaoVmSpace_ReleaseParser( self, parser );
	return 0;
}
DaoNamespace* DaoVmSpace_LoadDaoModule( DaoVmSpace *self, DString *libpath )
{
	return DaoVmSpace_LoadDaoModuleExt( self, libpath, NULL, 0 );
}

static DaoNamespace* DaoVmSpace_LoadDllModule( DaoVmSpace *self, DString *libpath )
{
	DNode *node;
	DString *name = NULL;
	DaoModuleOnLoad funpter = NULL;
	DaoNamespace *ns = NULL;
	void *handle = NULL;
	daoint nsCount = self->loadedModules->size;
	daoint i, retc;

	ns = DaoVmSpace_FindNamespace( self, libpath );
	if( ns ) return ns;

	if( (node = MAP_Find( self->vmodules, libpath ) ) ){
		funpter = (DaoModuleOnLoad) node->value.pVoid;
		ns = DaoNamespace_New( self, libpath->chars );
	}else{
		handle = Dao_OpenDLL( libpath->chars );
		if( ! handle ){
			DaoStream_WriteChars( self->errorStream, "ERROR: unable to open the library file \"" );
			DaoStream_WriteChars( self->errorStream, libpath->chars );
			DaoStream_WriteChars( self->errorStream, "\".\n");
			return 0;
		}
		name = DString_New();
		DString_SetChars( name, "DaoOnLoad" );
		ns = DaoNamespace_New( self, libpath->chars );
		ns->libHandle = handle;
		funpter = (DaoModuleOnLoad) Dao_GetSymbolAddress( handle, "DaoOnLoad" );
		if( funpter == NULL ){
			int size = strlen( DAO_DLL_SUFFIX );
			DString_SetBytes( name, ns->file->chars, ns->file->size - size );
			for(i=0; i<DAO_FILE_TYPE_NUM; i++){
				if( daoModuleTypes[i] < DAO_MODULE_DLL ) continue;
				if( DString_FindChars( name, daoDllPrefix[i], 0 ) != 0 ) continue;
				DString_Erase( name, 0, strlen( daoDllPrefix[i] ) );
			}
			DString_InsertChars( name, "Dao", 0, 0, 3 );
			DString_AppendChars( name, "_OnLoad" );
			funpter = (DaoModuleOnLoad) Dao_GetSymbolAddress( handle, name->chars );
			if( funpter == NULL ){
				for(i=3; i<name->size-7; i++) name->chars[i] = tolower( name->chars[i] );
				funpter = (DaoModuleOnLoad) Dao_GetSymbolAddress( handle, name->chars );
			}
			if( funpter == NULL ){
				name->chars[3] = toupper( name->chars[3] );
				funpter = (DaoModuleOnLoad) Dao_GetSymbolAddress( handle, name->chars );
			}
			if( funpter == NULL ){
				for(i=3; i<name->size-7; i++) name->chars[i] = toupper( name->chars[i] );
				funpter = (DaoModuleOnLoad) Dao_GetSymbolAddress( handle, name->chars );
			}
		}
	}
	if( self->options & DAO_OPTION_ARCHIVE ){
		if( name == NULL ) name = DString_New();
		if( funpter == NULL ) DString_Clear( name );
		DList_Append( self->sourceArchive, libpath );
		DList_Append( self->sourceArchive, name );
	}
	if( name ) DString_Delete( name );

	DaoVmSpace_Lock( self );
	DList_PushFront( self->loadedModules, ns );
	MAP_Insert( self->nsModules, libpath, ns );
	DaoVmSpace_Unlock( self );

	/*
	// no warning or error for loading a C/C++ dynamic linking library
	// for solving symbols in Dao modules.
	*/
	if( funpter == NULL ) return ns;

	DaoVmSpace_Lock( self );
	DList_PushFront( self->nameLoading, ns->name );
	if( ns->path->size ) DList_PushFront( self->pathLoading, ns->path );
	DaoVmSpace_Unlock( self );

	retc = (*funpter)( self, ns );

	DaoVmSpace_Lock( self );
	if( ns->path->size ) DList_PopFront( self->pathLoading );
	DList_PopFront( self->nameLoading );
	DaoVmSpace_Unlock( self );
	if( retc ){
		DaoVmSpace_Lock( self );
		MAP_Erase( self->nsModules, ns->name );
		DList_Erase( self->loadedModules, 0, self->loadedModules->size - nsCount );
		DaoVmSpace_Unlock( self );
		return NULL;
	}
	DaoNamespace_UpdateLookupTable( ns );
	if( self->loadedModules->size > (nsCount+1) ){
		DaoVmSpace_Lock( self );
		DList_Erase( self->loadedModules, 0, self->loadedModules->size - (nsCount+1) );
		DaoVmSpace_Unlock( self );
	}
	return ns;
}
int DaoVmSpace_AddVirtualModules( DaoVmSpace *self, DaoVModule modules[] )
{
	int vmods = 0;
	/* For $(DAR_DIR) in archive paths: */
	DList_PushFront( self->nameLoading, mainVmSpace->daoBinPath );
	DList_PushFront( self->pathLoading, mainVmSpace->daoBinPath );
	DString_Change( self->nameLoading->items.pString[0], "^ .* ([^/\\]+) $", "%1", 0 );
	DString_Change( self->pathLoading->items.pString[0], "[^/\\]* $", "", 0 );
	while( modules[vmods].name ){
		DaoVmSpace_AddVirtualModule( self, & modules[vmods] );
		vmods ++;
	}
	return vmods;
}
void DaoVmSpace_AddVirtualModule( DaoVmSpace *self, DaoVModule *module )
{
	FILE *fin;
	DNode *node;
	DString *fname = DString_New();
	DString *source = DString_New();
	char *data = (char*) module->data;
	daoint pos, n = module->length;

#if 0
	printf( "DaoVmSpace_AddVirtualModule: %s %s\n", module->name, module->data );
#endif

	if( strcmp( module->name, "$(ARCHIVE)" ) == 0 ){ /* External archive: */
		DString_SetBytes( fname, data, module->length );
		DaoVmSpace_ConvertPath2( self, fname );
		Dao_MakePath( self->startPath, fname );
		fin = Dao_OpenFile( fname->chars, "r" );
		if( fin != NULL ){
			DaoFile_ReadAll( fin, source, 1 );
			DaoVmSpace_LoadArchive( self, source, fname );
		}else{
			DaoStream_PrintFileInfo( self->errorStream, fname, "WARNING: cannot open archive: " );
		}
		DString_Delete( fname );
		DString_Delete( source );
		return;
	}

	/* Add a fake root "/@/" to all virtual files: */
	DString_SetChars( fname, "/@/" );
	DString_AppendChars( fname, module->name );
	if( module->onload ){
		MAP_Insert( self->vmodules, fname, module->onload );
	}else if( n >= 0 ){
		if( n == 0 ) n = strlen( data );
		node = DMap_Find( self->vfiles, fname );
		if( node ){
			DaoVirtualFile *vfile = (DaoVirtualFile*) node->value.pVoid;
			DString_AppendBytes( vfile->data, data, n );
		}else{
			DaoVirtualFile *vfile = DaoVirtualFile_New();
			DString_SetBytes( vfile->data, data, n );
			MAP_Insert( self->vfiles, fname, vfile );
		}
	}else{
		/*
		// Add virtual file from external archive:
		// Only the archive name, byte offset and byte count are stored;
		*/
		fin = Dao_OpenFile( data, "r" );
		node = DMap_Find( self->vfiles, fname );
		if( node == NULL && fin != NULL ){
			DaoFile_ReadPart( fin, source, abs(n), 4 );
			if( source->size == 4 ){
				DaoVirtualFile *vfile = DaoVirtualFile_New();
				DString_SetChars( vfile->data, data );
				vfile->offset = abs(n) + 4;
				vfile->size = DaoDecodeUInt32( source->chars );
				MAP_Insert( self->vfiles, fname, vfile );
			}
		}
		if( fin != NULL ) fclose( fin );
	}
	pos = DString_RFindChar( fname, '/', -1 );
	DString_Erase( fname, pos, -1 );
	DList_PushFront( self->virtualPaths, fname );
	DString_Delete( fname );
	DString_Delete( source );
}

void Dao_MakePath( DString *base, DString *path )
{
	if( base->size == 0 ) return;
	if( path->size >= 2 && path->chars[0] == '$' && path->chars[1] == '(' ) return;

	if( path->size >= 2 && isalpha( path->chars[0] ) && path->chars[1] == ':' ) return;
	if( path->chars[0] == '/' ) return;

	base = DString_Copy( base );
	Dao_NormalizePathSep( base );
	Dao_NormalizePathSep( path );
	while( DString_Match( path, " ^ %.%. / ", NULL, NULL ) ){
		if( DString_Match( base, " [^/] + ( / | ) $ ", NULL, NULL ) ){
			DString_Change( path, " ^ %.%. / ", "", 1 );
			DString_Change( base, " [^/] + ( / |) $ ", "", 0 );
		}else{
			DString_Delete( base );
			return;
		}
	}
	if( DString_Match( path, " ^ %.%. $ ", NULL, NULL ) ){
		if( DString_Match( base, " [^/] + ( / | ) $ ", NULL, NULL ) ){
			DString_Clear( path );
			DString_Change( base, " [^/] + ( / |) $ ", "", 0 );
		}
	}
	if( base->size && path->size ){
		if( base->chars[ base->size-1 ] != '/' && path->chars[0] != '/' )
			DString_InsertChar( path, '/', 0 );
		DString_Insert( path, base, 0, 0, 0 );
	}else if( base->size && path->size == 0 ){
		DString_Assign( path, base );
	}
	while( DString_Change( path, "/ %. (/|$)", "/", 0 ) );
	DString_Delete( base );
}
int DaoVmSpace_SearchResource( DaoVmSpace *self, DString *fname, DString *search )
{
	DString *path;
	if( fname->size == 0 ) return 0;
	path = DString_New();
	DString_AppendChars( path, "/@/" );
	DString_Append( path, fname );
	if( TestPath( self, path, DAO_FILE_PATH ) == 0 ){
		DString_Assign( path, fname );
		Dao_MakePath( search, path );
	}
	if( TestPath( self, path, DAO_FILE_PATH ) ){
		DString_Assign( fname, path );
		DString_Delete( path );
		return 1;
	}
	DString_Delete( path );
	return 0;
}
int DaoVmSpace_SearchPath2( DaoVmSpace *self, DList *paths, DString *fname, int type )
{
	DString *path = DString_New();
	daoint i;
	for(i=0; i<paths->size; ++i){
		DString_Assign( path, paths->items.pString[i] );
		if( path->size && path->chars[ path->size-1 ] != '/' ) DString_AppendChars( path, "/" );
		DString_Append( path, fname );
		/*
		   printf( "%s %s\n", paths->items.pString[i]->chars, path->chars );
		 */
		if( TestPath( self, path, type ) ){
			DString_Assign( fname, path );
			DString_Delete( path );
			return 1;
		}
	}
	DString_Delete( path );
	return 0;
}
void DaoVmSpace_SearchPath( DaoVmSpace *self, DString *fname, int type, int check )
{
	char *p;
	DString *path;

	if( DaoVmSpace_SearchResource( self, fname, self->pathWorking ) ) return;

	DString_Change( fname, "/ %s* %. %s* /", "/", 0 );
	DString_Change( fname, "[^%./] + / %. %. /", "", 0 );
	/* erase the last '/' */
	if( fname->size && fname->chars[ fname->size-1 ] =='/' ){
		fname->size --;
		fname->chars[ fname->size ] = 0;
	}

	/* C:\dir\source.dao; /home/...  */
	if( fname->size >1 && ( fname->chars[0]=='/' || fname->chars[1]==':' ) ) return;

	while( ( p = strchr( fname->chars, '\\') ) !=NULL ) *p = '/';

	path = DString_Copy( self->pathWorking );

	/* Virtual paths are more preferrable than other paths: */
	if( DaoVmSpace_SearchPath2( self, self->virtualPaths, fname, type ) ) goto FreeString;

	/* ./source.dao; ../../source.dao */
	if( strstr( fname->chars, "./" ) !=NULL || strstr( fname->chars, "../" ) !=NULL ){

		if( self->pathLoading->size ){
			DString_Assign( path, self->pathLoading->items.pString[0] );
			if( path->size ==0 ) goto FreeString;
		}else if( self->pathWorking->size==0 ) goto FreeString;

		Dao_MakePath( path, fname );
		goto FreeString;
	}

	if( DaoVmSpace_SearchPath2( self, self->pathLoading, fname, type ) ) goto FreeString;

	if( path->size > 0 && path->chars[ path->size -1 ] != '/' ) DString_AppendChars( path, "/" );
	DString_Append( path, fname );
	/* printf( "%s %s\n", path->chars, path->chars ); */
	if( ! check || TestPath( self, path, type ) ){
		DString_Assign( fname, path );
		goto FreeString;
	}
	if( DaoVmSpace_SearchPath2( self, self->pathSearching, fname, type ) ) goto FreeString;

FreeString:
	DString_Delete( path );
}
void DaoVmSpace_SetPath( DaoVmSpace *self, const char *path )
{
	char *p;
	DString_SetChars( self->pathWorking, path );
	while( ( p = strchr( self->pathWorking->chars, '\\') ) !=NULL ) *p = '/';
}
/* Make path only relative to the current loading path or working path: */
void DaoVmSpace_MakePath( DaoVmSpace *self, DString *path )
{
	DString *wpath = self->pathWorking;

	if( path->size == 0 ) return;
	if( path->size > 0 && path->chars[0] == '/' ) return;
	if( path->size > 0 && path->chars[0] == '$' ) return;
	if( path->size > 1 && path->chars[1] == ':' ) return;

	if( self->pathLoading->size ) wpath = self->pathLoading->items.pString[0];
	if( path->chars[0] == '.' ){
		Dao_MakePath( wpath, path );
	}else{
		DString *tmp = DString_Copy( wpath );
		if( tmp->size > 0 && tmp->chars[ tmp->size-1 ] != '/' ) DString_AppendChars( tmp, "/" );
		DString_Append( tmp, path );
		DString_Assign( path, tmp );
		DString_Delete( tmp );
	}
}
void DaoVmSpace_AddPath( DaoVmSpace *self, const char *path )
{
	DString *pstr;
	char *p;

	if( path == NULL || path[0] == '\0' ) return;

	pstr = DString_New();
	DString_SetChars( pstr, path );
	while( ( p = strchr( pstr->chars, '\\') ) !=NULL ) *p = '/';

	DaoVmSpace_MakePath( self, pstr );

	if( pstr->chars[pstr->size-1] == '/' ) DString_Erase( pstr, pstr->size-1, 1 );

	if( Dao_IsDir( pstr->chars ) ) DList_PushFront( self->pathSearching, pstr );
	DString_Delete( pstr );
	/*
	   for(i=0; i<self->pathSearching->size; i++ )
	   printf( "%s\n", self->pathSearching->items.pString[i]->chars );
	 */
}
void DaoVmSpace_DelPath( DaoVmSpace *self, const char *path )
{
	DString *pstr;
	char *p;
	int i, id = -1;

	pstr = DString_New();
	DString_SetChars( pstr, path );
	while( ( p = strchr( pstr->chars, '\\') ) !=NULL ) *p = '/';

	DaoVmSpace_MakePath( self, pstr );

	if( pstr->chars[pstr->size-1] == '/' ) DString_Erase( pstr, pstr->size-1, 1 );

	for(i=0; i<self->pathSearching->size; i++ ){
		if( DString_Compare( pstr, self->pathSearching->items.pString[i] ) == 0 ){
			id = i;
			break;
		}
	}
	if( id >= 0 ) DList_Erase( self->pathSearching, id, 1 );
	DString_Delete( pstr );
}
const char* DaoVmSpace_CurrentWorkingPath( DaoVmSpace *self )
{
	return self->pathWorking->chars;
}
const char* DaoVmSpace_CurrentLoadingPath( DaoVmSpace *self )
{
	if( self->pathLoading->size ==0 ) return NULL;
	return self->pathLoading->items.pString[0]->chars;
}

extern DaoTypeBase vmpTyper;

extern DaoTypeBase DaoFdSet_Typer;

extern void DaoInitLexTable();

static void DaoConfigure_FromFile( const char *name )
{
	double number;
	int i, ch, isnum, isint, integer=0, yes;
	FILE *fin = Dao_OpenFile( name, "r" );
	DaoToken *tk1, *tk2;
	DaoLexer *lexer;
	DList *tokens;
	DString *mbs;
	if( fin == NULL ) return;
	mbs = DString_New();
	lexer = DaoLexer_New();
	tokens = lexer->tokens;
	while( ( ch=getc(fin) ) != EOF ) DString_AppendChar( mbs, ch );
	fclose( fin );
	DString_ToLower( mbs );
	DaoLexer_Tokenize( lexer, mbs->chars, DAO_LEX_ESCAPE );
	i = 0;
	while( i < tokens->size ){
		tk1 = tokens->items.pToken[i];
		/* printf( "%s\n", tk1->string.chars ); */
		if( tk1->type == DTOK_IDENTIFIER ){
			if( i+2 >= tokens->size ) goto InvalidConfig;
			if( tokens->items.pToken[i+1]->type != DTOK_ASSN ) goto InvalidConfig;
			tk2 = tokens->items.pToken[i+2];
			isnum = isint = 0;
			yes = -1;
			if( tk2->type >= DTOK_DIGITS_DEC && tk2->type <= DTOK_NUMBER_SCI ){
				isnum = 1;
				isint = tk2->type <= DTOK_NUMBER_HEX;
				number = DaoToken_ToFloat( tk2 );
				integer = number;
			}else if( tk2->type == DTOK_IDENTIFIER ){
				if( strcmp( tk2->string.chars, "yes" )==0 ) yes = 1;
				if( strcmp( tk2->string.chars, "no" )==0 ) yes = 0;
			}
			if( strcmp( tk1->string.chars, "cpu" )==0 ){
				/* printf( "%s  %i\n", tk2->string->chars, tk2->type ); */
				if( isint == 0 ) goto InvalidConfigValue;
				daoConfig.cpu = integer;
			}else if( strcmp( tk1->string.chars, "jit" )==0 ){
				if( yes <0 ) goto InvalidConfigValue;
				daoConfig.jit = yes;
			}else if( strcmp( tk1->string.chars, "optimize" )==0 ){
				if( yes <0 ) goto InvalidConfigValue;
				daoConfig.optimize = yes;
			}else{
				goto InvalidConfigName;
			}
			i += 3;
			continue;
		}else if( tk1->type == DTOK_COMMA || tk1->type == DTOK_SEMCO ){
			i ++;
			continue;
		}
InvalidConfig :
		printf( "ERROR: invalid configuration file format at line: %i!\n", tk1->line );
		break;
InvalidConfigName :
		printf( "ERROR: invalid configuration option name: %s!\n", tk1->string.chars );
		break;
InvalidConfigValue :
		printf( "ERROR: invalid configuration option value: %s!\n", tk2->string.chars );
		break;
	}
	DaoLexer_Delete( lexer );
	DString_Delete( mbs );
}
static void DaoConfigure()
{
	char *daodir = getenv( "DAO_DIR" );
	DString *mbs = DString_New();

	DaoInitLexTable();
	daoConfig.iscgi = getenv( "GATEWAY_INTERFACE" ) ? 1 : 0;

	DString_SetChars( mbs, DAO_DIR );
	DString_AppendChars( mbs, "/dao.conf" );
	DaoConfigure_FromFile( mbs->chars );
	if( daodir ){
		DString_SetChars( mbs, daodir );
		if( daodir[ mbs->size-1 ] == '/' ){
			DString_AppendChars( mbs, "dao.conf" );
		}else{
			DString_AppendChars( mbs, "/dao.conf" );
		}
		DaoConfigure_FromFile( mbs->chars );
	}
	DaoConfigure_FromFile( "./dao.conf" );
	DString_Delete( mbs );
}


extern void DaoType_Init();


DaoType *dao_type_future  = NULL;

#ifdef DAO_WITH_THREAD
DaoType *dao_type_channel = NULL;

extern DMutex mutex_string_sharing;
extern DMutex mutex_type_map;
extern DMutex mutex_values_setup;
extern DMutex mutex_methods_setup;
extern DMutex mutex_routines_update;
extern DMutex mutex_routine_specialize;
extern DMutex mutex_routine_specialize2;
extern DaoFuncItem dao_mt_methods[];
#endif

DaoType *dao_type_stream = NULL;
extern DaoFuncItem dao_std_methods[];
extern DaoFuncItem dao_io_methods[];

#include<signal.h>
void print_trace();


int DaoVmSpace_TryInitDebugger( DaoVmSpace *self, const char *module )
{
	DaoDebugger *debugger = self->debugger;
	DaoVmSpace_Load( self, module ? module : "debugger" );
	if( self->debugger != debugger ) return 1;
	DaoStream_WriteChars( self->errorStream, "Failed to enable debugger!\n" );
	return 0;
}
int DaoVmSpace_TryInitProfiler( DaoVmSpace *self, const char *module )
{
	DaoProfiler *profiler = self->profiler;
	DaoVmSpace_Load( self, module ? module : "profiler" );
	if( self->profiler && self->profiler != profiler ) return 1;
	DaoStream_WriteChars( self->errorStream, "Failed to enable profiler!\n" );
	return 0;
}
int DaoVmSpace_TryInitJIT( DaoVmSpace *self, const char *module )
{
	DaoVmSpace_Load( self, module ? module : "jit" );
	if( dao_jit.Compile ) return 1;
	DaoStream_WriteChars( self->errorStream, "Failed to enable Just-In-Time compiling!\n" );
	return 0;
}

static int Dao_GetExecutablePath( const char *command, DString *path )
{
	char *PATH = getenv( "PATH" );
	DString paths = DString_WrapChars( PATH );
	daoint i = 0;

	DString_Reset( path, 0 );
	if( PATH == NULL ) return 0;

	while( i < paths.size ){
		daoint j = DString_FindChar( & paths, DAO_ENV_PATH_SEP, i );
		daoint len = (j == DAO_NULLPOS) ? paths.size - i : j - i;
		DString base = DString_WrapBytes( paths.chars + i, len );
		DString_SetChars( path, command );
		Dao_MakePath( & base, path );
		Dao_NormalizePathSep( path );
		if( Dao_IsFile( path->chars ) ) return 1;
		if( j == DAO_NULLPOS ) break;
		i = j + 1;
	}
	return 0;
}


DaoVmSpace* DaoVmSpace_MainVmSpace()
{
	return mainVmSpace;
}


DaoVmSpace* DaoInit( const char *command )
{
	DString *mbs;
	DaoVmSpace *vms;
	DaoNamespace *daons, *ns2;
	DaoType *type, *tht;
	char *cwd;
	int i;

	if( mainVmSpace ) return mainVmSpace;

	/* signal( SIGSEGV, print_trace ); */
	/* signal( SIGABRT, print_trace ); */

#ifdef DAO_WITH_THREAD
	DMutex_Init( & mutex_string_sharing );
	DMutex_Init( & mutex_type_map );
	DMutex_Init( & mutex_values_setup );
	DMutex_Init( & mutex_methods_setup );
	DMutex_Init( & mutex_routines_update );
	DMutex_Init( & mutex_routine_specialize );
	DMutex_Init( & mutex_routine_specialize2 );
#endif

	mbs = DString_New();
	setlocale( LC_CTYPE, "" );

	DaoConfigure();
	DaoType_Init();
	/*
	   printf( "number of VM instructions: %i\n", DVM_NULL );
	 */

#if 0
	for(i=0; i<=DVM_UNUSED; ++i){
		printf( "%3i: %3i %s\n", i, DaoVmCode_GetOpcodeBase(i), DaoVmCode_GetOpcodeName(i) );
		if( i != DaoVmCode_GetOpcodeBase(i) ) printf( "!!!!!!!!!!\n" );
	}
#endif

#ifdef DAO_WITH_THREAD
	DaoInitThread();
#endif

	DaoGC_Start();
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_Init();
#endif

#if 0
#warning"-------------using concurrent GC by default!"
	DaoCGC_Start();
#endif

	dao_type_udf = DaoType_New( "?", DAO_UDT, NULL, NULL );
	dao_type_any = DaoType_New( "any", DAO_ANY, NULL, NULL );
	dao_type_bool = DaoType_New( "bool", DAO_BOOLEAN, NULL, NULL );
	dao_type_int = DaoType_New( "int", DAO_INTEGER, NULL, NULL );
	dao_type_float = DaoType_New( "float", DAO_FLOAT, NULL, NULL );
	dao_type_complex = DaoType_New( "complex", DAO_COMPLEX, NULL, NULL );
	dao_type_string = DaoType_New( "string", DAO_STRING, NULL, NULL );
	dao_type_enum = DaoType_New( "enum", DAO_ENUM, NULL, NULL );
	dao_type_tht = tht = DaoType_New( "@X", DAO_THT, NULL, NULL );
	dao_type_routine = DaoType_New( "routine<=>@X>", DAO_ROUTINE, (DaoValue*)tht, NULL );

	mainVmSpace = vms = DaoVmSpace_New();

	DString_Reserve( mainVmSpace->startPath, 512 );
	cwd = getcwd( mainVmSpace->startPath->chars, 511 );
	DString_Reset( mainVmSpace->startPath, cwd ? strlen( cwd ) : 0 );
	Dao_NormalizePathSep( mainVmSpace->startPath );

	DString_AppendPathSep( mainVmSpace->startPath );
	if( command ){
		int absolute = command[0] == '/';
		int relative = command[0] == '.';
		DString_SetChars( mainVmSpace->daoBinPath, command );
		Dao_NormalizePathSep( mainVmSpace->daoBinPath );
#ifdef _WIN32
		absolute = isalpha( command[0] ) && command[1] == ':';
#endif
		if( absolute == 0 ){
			if( relative ){
				Dao_MakePath( mainVmSpace->startPath, mainVmSpace->daoBinPath );
			}else{
				Dao_GetExecutablePath( command, mainVmSpace->daoBinPath );
			}
		}
#ifdef DEBUG
		if( ! Dao_IsFile( mainVmSpace->daoBinPath->chars ) ){
			printf( "WARNING: the path of the executable cannot be located!" );
		}
#endif
		DString_Change( mainVmSpace->daoBinPath, "[^/\\]* $", "", 0 );
	}

	daons = vms->daoNamespace;

	DaoProcess_CacheValue( vms->mainProcess, (DaoValue*) dao_type_udf );
	DaoProcess_CacheValue( vms->mainProcess, (DaoValue*) dao_type_tht );
	DaoProcess_CacheValue( vms->mainProcess, (DaoValue*) dao_type_routine );
	DaoNamespace_AddTypeConstant( daons, dao_type_any->name, dao_type_any );
	DaoNamespace_AddTypeConstant( daons, dao_type_bool->name, dao_type_bool );
	DaoNamespace_AddTypeConstant( daons, dao_type_int->name, dao_type_int );
	DaoNamespace_AddTypeConstant( daons, dao_type_float->name, dao_type_float );
	DaoNamespace_AddTypeConstant( daons, dao_type_complex->name, dao_type_complex );
	DaoNamespace_AddTypeConstant( daons, dao_type_string->name, dao_type_string );
	DaoNamespace_AddTypeConstant( daons, dao_type_enum->name, dao_type_enum );

	dao_type_none = DaoNamespace_MakeValueType( daons, dao_none_value );
	dao_type_for_iterator = DaoNamespace_DefineType( daons, "tuple<valid:bool,iterator:any>", "ForIterator" );

	dao_type_tuple = DaoNamespace_DefineType( daons, "tuple<...>", NULL );

	DaoNamespace_SetupType( daons, & stringTyper, dao_type_string );
	DaoNamespace_SetupType( daons, & comTyper, dao_type_complex );

#ifdef DAO_WITH_NUMARRAY
	dao_type_array = DaoNamespace_WrapGenericType( daons, & numarTyper, DAO_ARRAY );
	dao_type_array_empty = DaoType_Specialize( dao_type_array, NULL, 0 );
	dao_type_array_empty = DaoType_GetConstType( dao_type_array_empty );
	dao_array_types[DAO_NONE] = dao_type_array_empty;
	dao_array_types[DAO_BOOLEAN] = DaoType_Specialize( dao_type_array, & dao_type_bool, 1 );
	dao_array_types[DAO_INTEGER] = DaoType_Specialize( dao_type_array, & dao_type_int, 1 );
	dao_array_types[DAO_FLOAT] = DaoType_Specialize( dao_type_array, & dao_type_float, 1 );
	dao_array_types[DAO_COMPLEX] = DaoType_Specialize( dao_type_array, & dao_type_complex, 1 );
#endif

	dao_type_list = DaoNamespace_WrapGenericType( daons, & listTyper, DAO_LIST );
	dao_type_map = DaoNamespace_WrapGenericType( daons, & mapTyper, DAO_MAP );

	dao_type_list_any = DaoType_Specialize( dao_type_list, NULL, 0 );
	dao_type_map_any  = DaoType_Specialize( dao_type_map, NULL, 0 );

	/*
	// These types should not be accessible by developers using type annotation.
	*/
	dao_type_list_empty = DaoType_Copy( dao_type_list_any );
	dao_type_map_empty = DaoType_Copy( dao_type_map_any );
	dao_type_list_empty = DaoType_GetConstType( dao_type_list_empty );
	dao_type_map_empty = DaoType_GetConstType( dao_type_map_empty );
	DaoProcess_CacheValue( vms->mainProcess, (DaoValue*) dao_type_list_empty );
	DaoProcess_CacheValue( vms->mainProcess, (DaoValue*) dao_type_map_empty );

	ns2 = DaoVmSpace_GetNamespace( vms, "io" );
	DaoNamespace_AddConstValue( daons, "io", (DaoValue*) ns2 );
	DaoNamespace_WrapInterface( ns2, & ioDeviceTyper );
	dao_type_stream = DaoNamespace_WrapType( ns2, & streamTyper, 0 );
	GC_Assign( & vms->stdioStream->ctype, dao_type_stream );
	GC_Assign( & vms->errorStream->ctype, dao_type_stream );
	DaoNamespace_WrapFunctions( ns2, dao_io_methods );
	DaoNamespace_AddConstValue( ns2, "stdio",  (DaoValue*) vms->stdioStream );
	DaoNamespace_AddConstValue( ns2, "stderr", (DaoValue*) vms->errorStream );

	dao_type_cdata = DaoNamespace_WrapType( vms->daoNamespace, & defaultCdataTyper, 1 );

	DaoException_Setup( vms->daoNamespace );

	ns2 = DaoVmSpace_GetNamespace( vms, "mt" );
	DaoNamespace_AddConstValue( daons, "mt", (DaoValue*) ns2 );
	dao_type_future  = DaoNamespace_WrapType( ns2, & futureTyper, 0 );
#ifdef DAO_WITH_CONCURRENT
	dao_type_channel = DaoNamespace_WrapType( ns2, & channelTyper, 0 );
	DaoNamespace_WrapFunctions( ns2, dao_mt_methods );
#endif

	DaoNamespace_SetupType( vms->daoNamespace, & vmpTyper, NULL );

	ns2 = DaoVmSpace_GetNamespace( vms, "std" );
	DaoNamespace_AddConstValue( daons, "std", (DaoValue*) ns2 );
	DaoNamespace_WrapFunctions( ns2, dao_std_methods );

	DaoNamespace_AddParent( vms->mainNamespace, vms->daoNamespace );

	DaoVmSpace_InitPath( vms );

	/*
	   printf( "initialized...\n" );
	 */
	DString_Delete( mbs );
	return vms;
}

void DaoQuit()
{
	int i;
	DNode *it = NULL;

	/* TypeTest(); */
#ifdef DAO_WITH_CONCURRENT
	DaoCallServer_Stop();
#endif

	if( daoConfig.iscgi ) return;

	if( (mainVmSpace->options & DAO_OPTION_PROFILE) && mainVmSpace->profiler ){
		DaoProfiler *profiler = mainVmSpace->profiler;
		if( profiler->Report ) profiler->Report( profiler, mainVmSpace->stdioStream );
	}

	DaoVmSpace_DeleteData( mainVmSpace );

	DaoGC_Finish();
#ifdef DEBUG
	for(it=DMap_First(mainVmSpace->nsModules); it; it=DMap_Next(mainVmSpace->nsModules,it) ){
		printf( "Warning: namespace/module \"%s\" is not collected with reference count %i!\n",
				((DaoNamespace*)it->value.pValue)->name->chars, it->value.pValue->xBase.refCount );
	}
#endif
	DaoVmSpace_Delete( mainVmSpace );
#ifdef DAO_USE_GC_LOGGER
	DaoObjectLogger_Quit();
#endif
	dao_type_stream = NULL;
	mainVmSpace = NULL;
	mainProcess = NULL;
	if( dao_jit.Quit ){
		dao_jit.Quit();
		dao_jit.Quit = NULL;
		dao_jit.Free = NULL;
		dao_jit.Compile = NULL;
		dao_jit.Execute = NULL;
	}
#ifdef DAO_WITH_THREAD
	DMutex_Destroy( & mutex_string_sharing );
	DMutex_Destroy( & mutex_type_map );
	DMutex_Destroy( & mutex_values_setup );
	DMutex_Destroy( & mutex_methods_setup );
	DMutex_Destroy( & mutex_routines_update );
	DMutex_Destroy( & mutex_routine_specialize );
	DMutex_Destroy( & mutex_routine_specialize2 );
	DaoQuitThread();
#endif
}
DaoNamespace* DaoVmSpace_FindModule( DaoVmSpace *self, DString *fname )
{
	DaoNamespace* ns = DaoVmSpace_FindNamespace( self, fname );
	if( ns ) return ns;
	DaoVmSpace_CompleteModuleName( self, fname, DAO_MODULE_ANY );
	return DaoVmSpace_FindNamespace( self, fname );
}
DaoNamespace* DaoVmSpace_LoadModule( DaoVmSpace *self, DString *fname )
{
	DaoNamespace *ns = NULL;
	int modtype = DAO_MODULE_ANY;
	if( self->options & DAO_OPTION_COMP_BC ) modtype = DAO_MODULE_DAO | DAO_MODULE_DLL;
	switch( DaoVmSpace_CompleteModuleName( self, fname, modtype ) ){
	case DAO_MODULE_DAC :
	case DAO_MODULE_DAO : ns = DaoVmSpace_LoadDaoModule( self, fname ); break;
	case DAO_MODULE_DLL : ns = DaoVmSpace_LoadDllModule( self, fname ); break;
	}
	return ns;
}


DaoType* DaoVmSpace_MakeExceptionType( DaoVmSpace *self, const char *name )
{
	DaoTypeBase *typer;
	DaoValue *value;
	DaoType *type, *parent = NULL;
	DString *basename, sub;
	daoint i, offset = 0;

	if( strcmp( name, "Exception" ) == 0 ) return  dao_type_exception;
	if( strcmp( name, "Warning" ) == 0 ) return  dao_type_warning;
	if( strcmp( name, "Error" ) == 0 ) return  dao_type_error;

	basename = DString_NewChars( name );
	offset = DString_RFindChars( basename, "::", -1 );
	if( offset != DAO_NULLPOS ){
		DString_Erase( basename, offset - 1, -1 );
		parent = DaoVmSpace_MakeExceptionType( self, basename->chars );
		offset += 1;
	}
	DString_Delete( basename );
	if( parent == NULL ) return NULL;

	sub = DString_WrapChars( name + offset );
	value = DaoType_FindValueOnly( parent, & sub );
	if( value != NULL ){
		if( value->type == DAO_CTYPE ) return value->xCtype.cdtype;
		return NULL;
	}

	typer = (DaoTypeBase*) dao_calloc( 1, sizeof(DaoTypeBase) );
	typer->name = (char*) dao_malloc( (strlen(name)+1) * sizeof(char) );
	strcpy( (char*) typer->name, name );
	typer->supers[0] = parent->typer;
	typer->Delete = parent->typer->Delete;
	typer->GetGCFields = parent->typer->GetGCFields;
	type = DaoNamespace_WrapType( self->daoNamespace, typer, 0 );
	if( type == NULL ) return NULL;

	if( parent->kernel->initRoutines ){
		DaoRoutine *initors = parent->kernel->initRoutines;
		DaoType_FindFunctionChars( type, "x" ); /* To trigger method setup; */
		for(i=0; i<initors->overloads->routines->size; ++i){
			DaoRoutine *tmp = initors->overloads->routines->items.pRoutine[i];
			DaoRoutine *initor = DaoRoutine_Copy( tmp, 1, 0, 0 );
			DaoNamespace *nspace = tmp->nameSpace;
			DaoType *routype = tmp->routType;
			routype = DaoNamespace_MakeRoutType( nspace, tmp->routType, NULL, NULL, type );
			GC_Assign( & initor->routType, routype );
			GC_Assign( & initor->routHost, type );
			DString_Assign( initor->routName, type->name );
			initor->attribs |= DAO_ROUT_INITOR;
			DaoTypeKernel_InsertInitor( type->kernel, nspace, type, initor );
			DaoMethods_Insert( type->kernel->methods, initor, nspace, type );
		}
	}

	type->kernel->attribs |= DAO_TYPEKERNEL_FREE;
	for(i=DAO_EXCEPTION; i<ENDOF_BASIC_EXCEPT; i++){
		if( strcmp( typer->name, daoExceptionNames[i] ) == 0 ){
			DString_SetChars( type->aux->xCtype.info, daoExceptionTitles[i] );
			break;
		}
	}
	return type;
}
