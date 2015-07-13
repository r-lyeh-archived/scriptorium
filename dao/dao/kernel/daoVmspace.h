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

#ifndef DAO_VMSPACE_H
#define DAO_VMSPACE_H

#include"stdio.h"

#include"daoType.h"
#include"daoThread.h"
#include"daoOptimizer.h"
#include"daoInferencer.h"
#include"daoProcess.h"
#include"daoBytecode.h"

enum DaoPathType
{
	DAO_FILE_PATH,
	DAO_DIR_PATH
};

enum DaoModuleTypes
{
	DAO_MODULE_NONE = 0,
	DAO_MODULE_DAC  = 1,
	DAO_MODULE_DAO  = 2,
	DAO_MODULE_DLL  = 4,
	DAO_MODULE_ANY  = DAO_MODULE_DAC|DAO_MODULE_DAO|DAO_MODULE_DLL
};

extern const char *const dao_copy_notice;

/*
// Dao Virtual Machine Space:
// For handling:
// -- Execution options and configuration;
// -- Module loading and namespace management;
// -- C types and functions defined in modules;
// -- Path management;
*/
struct DaoVmSpace
{
	DAO_VALUE_COMMON;

	/*
	// To run the main script specified in the commad line (or the first loaded one),
	// or scripts from an interactive console.
	*/
	DaoProcess  *mainProcess;
	/*
	// To store globals in the main script,
	// or scripts from an interactive console.
	*/
	DaoNamespace  *mainNamespace;

	/* for standard objects or types etc.: */
	DaoNamespace  *daoNamespace;

	DaoStream  *stdioStream;
	DaoStream  *errorStream;

	DMap    *allProcesses;
	DMap    *allParsers;
	DMap    *allByteCoders;
	DMap    *allInferencers;
	DMap    *allOptimizers;

	DList   *processes;
	DList   *parsers;
	DList   *byteCoders;
	DList   *inferencers;
	DList   *optimizers;

	DString *daoBinPath;
	DString *startPath;
	DString *mainSource;
	DString *pathWorking;
	DList   *nameLoading;
	DList   *pathLoading;
	DList   *pathSearching; /* <DString*> */
	DList   *virtualPaths;  /* <DString*> */

	DList   *preloadModules;
	DList   *loadedModules;
	DList   *sourceArchive;

	int    stopit;
	int    options;
	int    evalCmdline;

	DMap  *vfiles;
	DMap  *vmodules;

	/* map full file name (including path and suffix) to module namespace */
	DMap  *nsModules; /* No GC for this, namespaces should remove themselves from this; */

	DaoDebugger     *debugger;
	DaoProfiler     *profiler;
	DaoUserHandler  *userHandler;

	char* (*ReadLine)( const char *prompt, DString *buffer );
	int   (*AddHistory)( const char *cmd );

#ifdef DAO_WITH_THREAD
	DMutex    mutexLoad;
	DMutex    mutexProc;
	DMutex    mutexMisc;
	DCondVar  condvWait;
#endif
};

extern DaoVmSpace *mainVmSpace;

DAO_DLL DaoVmSpace* DaoVmSpace_New();
/*
// DaoVmSpace is not handled by GC, it should be deleted manually.
// Normally, DaoVmSpace structures are allocated in the beginning of a program and
// persist until the program exits. So DaoVmSpace_Delete() is rarely needed to be called.
*/
DAO_DLL void DaoVmSpace_Delete( DaoVmSpace *self );

DAO_DLL void DaoVmSpace_Lock( DaoVmSpace *self );
DAO_DLL void DaoVmSpace_Unlock( DaoVmSpace *self );

DAO_DLL int DaoVmSpace_ParseOptions( DaoVmSpace *self, const char *options );

DAO_DLL int DaoVmSpace_RunMain( DaoVmSpace *self, const char *file );

DAO_DLL DaoNamespace* DaoVmSpace_Load( DaoVmSpace *self, const char *file );
DAO_DLL DaoNamespace* DaoVmSpace_LoadEx( DaoVmSpace *self, const char *file, int run );

DAO_DLL DaoNamespace* DaoVmSpace_LoadModule( DaoVmSpace *self, DString *fname );
DAO_DLL DaoNamespace* DaoVmSpace_FindModule( DaoVmSpace *self, DString *fname );
DAO_DLL DaoNamespace* DaoVmSpace_FindNamespace( DaoVmSpace *self, DString *name );

DAO_DLL int DaoVmSpace_TestFile( DaoVmSpace *self, DString *fname );
DAO_DLL int DaoVmSpace_ReadFile( DaoVmSpace *self, DString *fname, DString *source );
DAO_DLL int DaoVmSpace_SearchResource( DaoVmSpace *self, DString *fname, DString *search );

DAO_DLL void DaoVmSpace_ConvertPath( DaoVmSpace *self, DString *path );

DAO_DLL void DaoVmSpace_SearchPath( DaoVmSpace *self, DString *fname, int type, int check );
DAO_DLL int DaoVmSpace_CompleteModuleName( DaoVmSpace *self, DString *fname, int types );

DAO_DLL void DaoVmSpace_SetPath( DaoVmSpace *self, const char *path );
DAO_DLL void DaoVmSpace_AddPath( DaoVmSpace *self, const char *path );
DAO_DLL void DaoVmSpace_DelPath( DaoVmSpace *self, const char *path );

DAO_DLL const char*const DaoVmSpace_GetCopyNotice();
DAO_DLL DaoTypeBase* DaoVmSpace_GetTyper( short type );

DAO_DLL DaoType* DaoVmSpace_MakeExceptionType( DaoVmSpace *self, const char *name );

DAO_DLL DaoParser* DaoVmSpace_AcquireParser( DaoVmSpace *self );
DAO_DLL DaoByteCoder* DaoVmSpace_AcquireByteCoder( DaoVmSpace *self );
DAO_DLL DaoInferencer* DaoVmSpace_AcquireInferencer( DaoVmSpace *self );
DAO_DLL DaoOptimizer* DaoVmSpace_AcquireOptimizer( DaoVmSpace *self );
DAO_DLL void DaoVmSpace_ReleaseParser( DaoVmSpace *self, DaoParser *parser );
DAO_DLL void DaoVmSpace_ReleaseByteCoder( DaoVmSpace *self, DaoByteCoder *byteCoder );
DAO_DLL void DaoVmSpace_ReleaseInferencer( DaoVmSpace *self, DaoInferencer *inferencer );
DAO_DLL void DaoVmSpace_ReleaseOptimizer( DaoVmSpace *self, DaoOptimizer *optimizer );

void DaoAux_Delete( DMap *aux );

#endif
