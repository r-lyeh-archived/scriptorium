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

#ifndef __DAO_PLATFORMS_H__
#define __DAO_PLATFORMS_H__

#define DAO_KERNEL
#include "dao.h"


#ifdef _WIN32

#  define DAO_DIR "C:\\dao"
#  define DAO_DLL_PREFIX ""
#  define DAO_DLL_SUFFIX ".dll"

#  define DAO_ENV_PATH_SEP ';'


#  include <windows.h>

#  ifdef _MSC_VER
#    include<direct.h>
#  else
#    include<unistd.h>
#  endif

#  ifndef __GNUC__
#    define strtoll _strtoi64
#    define wcstoll _wcstoi64
#    define snprintf _snprintf
#    define getcwd _getcwd
#    define stat _stat
#    define fstat _fstat
#    define fileno _fileno
#  endif


#elif defined(UNIX) /* UNIX */

#  define DAO_DIR "/usr/local"
#  define DAO_ENV_PATH_SEP ':'

#  include<unistd.h>
#  include<dlfcn.h>
#  include<sys/time.h>

#  define DAO_DLL_PREFIX "lib"

#  ifdef MACOSX
#    define DAO_DLL_SUFFIX ".dylib"
#  else /* UNIX */
#    define DAO_DLL_SUFFIX ".so"
#  endif /* MACOSX */


#else /* other system */

#  define DAO_DIR ""
#  define DAO_DLL_PREFIX ""
#  define DAO_DLL_SUFFIX ".unkown"
#  define DAO_ENV_PATH_SEP ':'

#endif /* WIN32 */


#include<time.h>
#include<sys/types.h>
#include<sys/stat.h>
#include<stdio.h>
#include<wchar.h>


DAO_DLL void Dao_NormalizePathSep( DString *path );
DAO_DLL int Dao_IsFile( const char *file );
DAO_DLL int Dao_IsDir( const char *file );
DAO_DLL FILE* Dao_OpenFile( const char *file, const char *mode );

DAO_DLL size_t Dao_FileChangedTime( const char *file );

DAO_DLL double Dao_GetCurrentTime();

DAO_DLL void* Dao_OpenDLL( const char *name );
DAO_DLL void* Dao_GetSymbolAddress( void *handle, const char *name );

#endif
