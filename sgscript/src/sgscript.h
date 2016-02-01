
#ifndef SGSCRIPT_H_INCLUDED
#define SGSCRIPT_H_INCLUDED


#define SGS_VERSION_MAJOR 1
#define SGS_VERSION_MINOR 0
#define SGS_VERSION_INCR  4
#define SGS_VERSION "1.0.4"

#define SGS_VERSION_OFFSET 8
#define SGS_VERSION_INT ( ( ( ( SGS_VERSION_MAJOR << SGS_VERSION_OFFSET ) | \
        SGS_VERSION_MINOR ) << SGS_VERSION_OFFSET ) | SGS_VERSION_INCR )


#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>


/******
* CONFIGURATION *
     ==========*/

/* global debug switch */
#ifndef SGS_DEBUG
#  ifdef _DEBUG
#    define SGS_DEBUG 1
#  else
#    define SGS_DEBUG 0
#  endif
#endif

/* compile without standard libraries */
#ifndef SGS_NO_STDLIB
#  define SGS_NO_STDLIB 0
#endif

/* stage output dumps */
#ifndef SGS_DEBUG_DATA
#  define SGS_DEBUG_DATA 0
#endif

/* function entry/exit tagging */
#ifndef SGS_DEBUG_FLOW
#  define SGS_DEBUG_FLOW 0
#endif

/* instruction logging */
#ifndef SGS_DEBUG_INSTR
#  define SGS_DEBUG_INSTR 0
#endif

/* stack dump before execution of each instruction */
#ifndef SGS_DEBUG_STATE
#  define SGS_DEBUG_STATE 0
#endif

/* break-if */
#ifndef SGS_DEBUG_VALIDATE
#  define SGS_DEBUG_VALIDATE 1
#endif

/* extensive buffer overflow tests */
#ifndef SGS_DEBUG_EXTRA
#  define SGS_DEBUG_EXTRA 0
#endif

/* checks for having more memory allocated with..
	..the context memory functions than freed
	very fast, runs only in sgs_DestroyEngine */
#ifndef SGS_DEBUG_LEAKS
#  define SGS_DEBUG_LEAKS 1
#endif


/* profiling */
#ifndef SGS_DUMP_BYTECODE
#  define SGS_DUMP_BYTECODE 0
#endif


/* SGScript error messages */
#define SGS_ERRMSG_CALLSTACKLIMIT "max. call stack size reached"


/* bytes reserved initially for token storage
	used to prevent multiple reallocations in the beginning */
#define SGS_TOKENLIST_PREALLOC 1024

/* maximum length for a string to be included in the string table */
#define SGS_STRINGTABLE_MAXLEN 0x7fffffff

/* max. length of the call stack */
#define SGS_MAX_CALL_STACK_SIZE 256

/* the size of the stack buffer for temporary vsprintf'ed text
	if predicted text length is longer than that, the memory
	is allocated using the context allocator */
#define SGS_OUTPUT_STACKBUF_SIZE 128

/* postfix for function names in C functions
	EXTRABYTES should be set to the length of EXTRASTRING */
#define SGS_MSG_EXTRASTRING "(): "
#define SGS_MSG_EXTRABYTES 4

/* object pool settings
	- size of object pool (max. number of items)
	- max. appended memory size for pool usage */
#define SGS_OBJPOOL_SIZE 64
#define SGS_OBJPOOL_MAX_APPMEM 256

/* stack frame pool settings */
#define SGS_STACKFRAMEPOOL_SIZE 512

/* inclusion system settings:
	- name of entry point function to look for
	- the default include path */
#define SGS_LIB_ENTRY_POINT "sgscript_main"
#define SGS_INCLUDE_PATH "|/?;|/?" SGS_MODULE_EXT ";|/?.sgc;|/?.sgs;?;?" SGS_MODULE_EXT ";?.sgc;?.sgs;@/?;@/?" SGS_MODULE_EXT ";@/?.sgc;@/?.sgs"


/*****
* X-PLATFORM *
    ========*/

#ifdef _MSC_VER
#  pragma warning( disable: 4996 )
#  include <stdint.h>
#  if _MSC_VER >= 1700
#    include <inttypes.h>
#  else
#    define PRId32 "d"
#    define PRId64 "lld"
#  endif
#  define SGS_INLINE
#  define snprintf _snprintf
#  define SGS_VSPRINTF_LEN( str, args ) _vscprintf( str, args )
#else
#  include <inttypes.h>
#  define SGS_INLINE inline
#  define SGS_VSPRINTF_LEN( str, args ) vsnprintf( NULL, 0, str, args )
#endif

#define SGS_UNUSED( x ) (void)(x)

#if defined(__GNUC__) && ( __GNUC__ > 4 || ( __GNUC__ == 4 && __GNUC_MINOR__ >= 7 ) )
#  define SGS_ASSUME_ALIGNED __builtin_assume_aligned
#else
#  define SGS_ASSUME_ALIGNED( x, a ) (x)
#endif

#ifdef __cplusplus
#define SGS_DECLARE extern
#else
#define SGS_DECLARE static
#endif


#ifdef SGS_COMPILE_MODULE
#  define BUILDING_SGS 1
#endif

#if SGS_DLL && defined( _WIN32 )
#  if BUILDING_SGS
#    define SGS_APIFUNC __declspec(dllexport)
#  else
#    define SGS_APIFUNC __declspec(dllimport)
#  endif
#else
#  define SGS_APIFUNC
#endif

#if SGS_DLL
#  define SGS_IF_DLL( dll, nodll ) dll
#else
#  define SGS_IF_DLL( dll, nodll ) nodll
#endif

#if defined(WINAPI_FAMILY) && (WINAPI_FAMILY == WINAPI_FAMILY_PC_APP || WINAPI_FAMILY == WINAPI_FAMILY_PHONE_APP)
#  define SGS_WINAPP 1
#endif

#ifdef SGS_USE_FILESYSTEM
#  include <sys/types.h>
#  include <sys/stat.h>

#  ifdef _WIN32
#    if SGS_WINAPP
#      include <windows.h>
#    else
#      include <direct.h>
#      define getcwd _getcwd
#      define mkdir _mkdir
#      define rmdir _rmdir
#      define stat _stat
#      include "sgs_msvc_dirent.h"
#    endif
#  else
#    include <unistd.h>
#    include <dirent.h>
#  endif
#endif

#ifdef _WIN32
#  define SGS_MODULE_EXT ".dll"
#else
#  define SGS_MODULE_EXT ".so"
#endif

/* basic platform info */
#if _WIN32 || __WIN32__ || __WINDOWS__
#  define SGS_OS_TYPE "Windows"
#elif __APPLE__
#  define SGS_OS_TYPE "Mac OS X"
#elif __ANDROID__
#  define SGS_OS_TYPE "Android"
#elif __linux || __linux__
#  define SGS_OS_TYPE "Linux"
#elif __unix || __unix__
#  define SGS_OS_TYPE "Unix"
#elif __posix
#  define SGS_OS_TYPE "POSIX"
#else
#  define SGS_OS_TYPE "Unknown"
#endif

/* simplified architecture info */
#if defined __arm__ || defined __thumb__ || defined _M_ARM || defined _M_ARMT
#  define SGS_ARCH_ARM 1
#endif
#if defined i386 || defined __i386 || defined __i386 || defined _M_IX86 || defined _X86_ || defined __X86__
#  define SGS_ARCH_X86 1
#endif
#if defined __amd64__ || defined __amd64 || defined __x86_64__ || defined __x86_64 || defined _M_X64 || defined _M_AMD64
#  define SGS_ARCH_AMD64 1
#endif
#if defined __ia64__ || defined _IA64 || defined __IA64__ || defined _M_IA64
#  define SGS_ARCH_IA64 1
#endif
#if defined __powerpc || defined __powerpc__ || defined __powerpc64__ || \
	defined __ppc__ || defined __ppc64 || defined _M_PPC || defined _XENON
#  define SGS_ARCH_PPC 1
#endif


/* http://stackoverflow.com/a/2103095/1648140 */

enum
{
    O32_LITTLE_ENDIAN = 0x03020100ul,
    O32_BIG_ENDIAN = 0x00010203ul
};

static const union { unsigned char bytes[4]; uint32_t value; } o32_host_order =
    { { 0, 1, 2, 3 } };

#define O32_HOST_ORDER (o32_host_order.value)


#ifdef __cplusplus
extern "C" {
#endif

#define SGS_XPC_NOFILE -1
#define SGS_XPC_NOPROC -2
#define SGS_XPC_NOTSUP -3
#define SGS_XPC_NOTLIB -4
#define SGS_XPC_LDFAIL -5
int sgsXPC_GetProcAddress( const char* file, const char* proc, void** out );

char* sgsXPC_GetCurrentDirectory();
int sgsXPC_SetCurrentDirectory( char* path );
char* sgsXPC_GetModuleFileName();

#ifdef __cplusplus
}
#endif


/***
* SYSTEM *
  ======*/

/* API self-doc helpers */
#define SGSZERO   int     /* always 0 */
#define SGSONE    int     /* always 1 */
#define SGSRESULT int     /* output code */
#define SGSBOOL   int     /* true (1) / false (0) */


/* Output codes */
#define SGS_SUCCESS  0  /* success (no errors) */
#define SGS_ENOTFND -1  /* item was not found */
#define SGS_ECOMP   -2  /* compile error */
#define SGS_ENOTSUP -4  /* not supported */
#define SGS_EBOUNDS -5  /* index out of bounds */
#define SGS_EINVAL  -6  /* invalid value was passed */
#define SGS_EINPROC -7  /* process was interrupted */


/* Code tests */
#define SGS_FAILED( e ) ((e)<0)
#define SGS_SUCCEEDED( e ) ((e)>=0)


/* Accessible / transferable data */
typedef int32_t sgs_Bool;
typedef int64_t sgs_Int;
typedef double  sgs_Real;
typedef int32_t sgs_SizeVal;
typedef int32_t sgs_StkIdx;
typedef struct _sgs_Context sgs_Context;
typedef struct _sgs_Variable sgs_Variable;
typedef struct _sgs_StackFrame sgs_StackFrame;
typedef int (*sgs_CFunc) ( sgs_Context* );

#define sgs_Integer sgs_Int
#define sgs_Float sgs_Real


/* Memory management */
typedef void* (*sgs_MemFunc)
(
	void* /* userdata */,
	void* /* curmem */,
	size_t /* numbytes */
);

/*
	unlike realloc, if size = 0, this function MUST return NULL
*/
static void* sgs_DefaultMemFunc( void* ud, void* ptr, size_t size )
{
	SGS_UNUSED( ud );
	if( ptr && size ) return realloc( ptr, size );
	else if( size )   return malloc( size );
	if( ptr )         free( ptr );
	return NULL;
}


/* Main output */
typedef void (*sgs_OutputFunc) (
	void* /* userdata */,
	sgs_Context* /* ctx / SGS_CTX */,
	const void* /* ptr */,
	size_t /* size */
);


/* Debug output */
#define SGS_INFO    100  /* information about potential issues and state of the system */
#define SGS_WARNING 200  /* non-fatal problems */
#define SGS_ERROR   300  /* fatal problems */
#define SGS_APIERR  330  /* API usage errors */
#define SGS_INTERR  360  /* internal/integral/interesting errors */

typedef void (*sgs_MsgFunc) (
	void* /* data */,
	sgs_Context* /* ctx / SGS_CTX */,
	int /* type */,
	const char* /* message */
);


/* Event Hook */
/* additional info can be found in the last stack frame */
#define SGS_HOOK_ENTER 1 /* entered a function */
#define SGS_HOOK_EXIT  2 /* exited a function */
#define SGS_HOOK_STEP  3 /* about to execute an instruction */
#define SGS_HOOK_PAUSE 4 /* paused execution */
#define SGS_HOOK_CONT  5 /* continuing execution */
#define SGS_HOOK_CREAT 6 /* context created */
#define SGS_HOOK_CFORK 7 /* context forked */
#define SGS_HOOK_CFREE 8 /* context freed */
typedef void (*sgs_HookFunc) (
	void* /* userdata */,
	sgs_Context* /* ctx / SGS_CTX */,
	int /* event_id */
);


/* Script file system */
typedef struct _sgs_ScriptFSData
{
	void* userhandle;
	const char* filename;
	void* output;
	size_t size;
}
sgs_ScriptFSData;
#define SGS_SFS_FILE_EXISTS 1
#define SGS_SFS_FILE_OPEN 2
#define SGS_SFS_FILE_READ 3
#define SGS_SFS_FILE_CLOSE 4
typedef SGSRESULT (*sgs_ScriptFSFunc) (
	void* /* userdata */,
	sgs_Context* /* ctx / SGS_CTX */,
	int /* op */,
	sgs_ScriptFSData* /* data */
);


/* for state/query functions */
#define SGS_TRUE 1
#define SGS_FALSE 0
#define SGS_QUERY -1


/* Statistics / debugging */
#define SGS_STAT_VERSION      0
#define SGS_STAT_STATECOUNT   1
#define SGS_STAT_OBJCOUNT     2
#define SGS_STAT_MEMSIZE      3
#define SGS_STAT_NUMALLOCS    4
#define SGS_STAT_NUMFREES     5
#define SGS_STAT_NUMBLOCKS    6
#define SGS_STAT_DUMP_STACK   10
#define SGS_STAT_DUMP_GLOBALS 11
#define SGS_STAT_DUMP_OBJECTS 12
#define SGS_STAT_DUMP_FRAMES  13
#define SGS_STAT_DUMP_STATS   14
#define SGS_STAT_DUMP_SYMBOLS 15
#define SGS_STAT_DUMP_RSRC    16
#define SGS_STAT_XDUMP_STACK  20


/* Virtual machine control */
#define SGS_CNTL_STATE      1
#define SGS_CNTL_GET_STATE  2
#define SGS_CNTL_MINLEV     3
#define SGS_CNTL_GET_MINLEV 4
#define SGS_CNTL_APILEV     5
#define SGS_CNTL_GET_APILEV 6
#define SGS_CNTL_ERRNO      7
#define SGS_CNTL_SET_ERRNO  8
#define SGS_CNTL_GET_ERRNO  9
#define SGS_CNTL_ERRSUP     10
#define SGS_CNTL_GET_ERRSUP 11
#define SGS_CNTL_SERIALMODE 12
#define SGS_CNTL_NUMRETVALS 13
#define SGS_CNTL_GET_PAUSED 14
#define SGS_CNTL_GET_ABORT  15


/* Context internals */
/* - variable types */
#define SGS_VT_NULL   0
#define SGS_VT_BOOL   1
#define SGS_VT_INT    2
#define SGS_VT_REAL   3
#define SGS_VT_STRING 4
#define SGS_VT_FUNC   5
#define SGS_VT_CFUNC  6
#define SGS_VT_OBJECT 7
#define SGS_VT_PTR    8
#define SGS_VT_THREAD 9
#define SGS_VT__COUNT 10

/* - object data */
typedef struct sgs_ObjData sgs_VarObj;
typedef int (*sgs_ObjCallback) ( sgs_Context*, sgs_VarObj* );

typedef int (*sgs_OC_Self) ( sgs_Context*, sgs_VarObj* );
typedef int (*sgs_OC_SlPr) ( sgs_Context*, sgs_VarObj*, int );

typedef struct _sgs_ObjInterface
{
	const char* name;
	
	sgs_OC_Self destruct;
	sgs_OC_Self gcmark;
	
	sgs_OC_Self getindex;
	sgs_OC_Self setindex;
	
	sgs_OC_SlPr convert;
	sgs_OC_Self serialize;
	sgs_OC_SlPr dump;
	sgs_OC_SlPr getnext;
	
	sgs_OC_Self call;
	sgs_OC_Self expr;
}
sgs_ObjInterface;

struct sgs_ObjData
{
	sgs_SizeVal refcount;
	uint32_t appsize;
	uint8_t redblue;    /* red or blue? mark & sweep */
	uint8_t mm_enable;  /* use metamethods? */
	uint8_t is_iface;   /* whether is also in the interface table */
	uint8_t in_setindex; /* whether already running custom setindex code */
	void* data;         /* should have offset=8 with packing alignment>=8 */
	sgs_ObjInterface* iface;
	sgs_VarObj* prev;   /* pointer to previous GC object */
	sgs_VarObj* next;   /* pointer to next GC object */
	sgs_VarObj* metaobj; /* pointer to meta-object */
};

typedef struct _sgs_iStr sgs_iStr;
struct _sgs_iStr
{
	sgs_SizeVal refcount;
	uint32_t size;
	uint32_t hash;
};
#define sgs_str_cstr( pstr ) (((char*)(pstr))+sizeof(sgs_iStr))
#define sgs_str_c_cstr( pstr ) (((const char*)(pstr))+sizeof(sgs_iStr))
#define sgs_var_cstr( var ) sgs_str_cstr( (var)->data.S )

typedef struct _sgs_iFunc sgs_iFunc;
typedef union _sgs_VarData
{
	sgs_SizeVal* pRC;
	sgs_Bool     B;
	sgs_Int      I;
	sgs_Real     R;
	sgs_iStr*    S;
	sgs_iFunc*   F;
	sgs_CFunc    C;
	sgs_VarObj*  O;
	void*        P;
	sgs_Context* T;
}
sgs_VarData;

struct _sgs_Variable
{
	uint32_t    type;
	sgs_VarData data;
};


/* parameter flags / special values */
#define SGS_GETNEXT_KEY   0x01
#define SGS_GETNEXT_VALUE 0x02

#define SGS_EOP_ADD       0
#define SGS_EOP_SUB       1
#define SGS_EOP_MUL       2
#define SGS_EOP_DIV       3
#define SGS_EOP_MOD       4
#define SGS_EOP_COMPARE   5
#define SGS_EOP_NEGATE    6

#define SGS_CONVOP_CLONE  0x10000
#define SGS_CONVOP_TOITER 0x30000


/* Engine context */
#define SGS_CTX sgs_Context* C


SGS_APIFUNC
sgs_Context*
sgs_CreateEngineExt
(
	sgs_MemFunc memfunc,
	void* mfuserdata
);
static SGS_INLINE sgs_Context* sgs_CreateEngine()
	{ return sgs_CreateEngineExt( sgs_DefaultMemFunc, NULL ); }

SGS_APIFUNC void sgs_DestroyEngine( SGS_CTX );
SGS_APIFUNC sgs_Context* sgs_RootContext( SGS_CTX );
SGS_APIFUNC sgs_Context* sgs_ForkState( SGS_CTX, int copystate );
SGS_APIFUNC void sgs_ReleaseState( SGS_CTX );
SGS_APIFUNC SGSBOOL sgs_PauseState( SGS_CTX );
SGS_APIFUNC SGSBOOL sgs_ResumeStateRet( SGS_CTX, int args, int* outrvc );
SGS_APIFUNC SGSBOOL sgs_ResumeStateExp( SGS_CTX, int args, int expect );
#define sgs_ResumeState( C ) sgs_ResumeStateExp( C, 0, 0 )
SGS_APIFUNC SGSBOOL sgs_CreateSubthread( sgs_Context* T, SGS_CTX,
	sgs_Variable* out, sgs_Variable func, int gotthis, sgs_StkIdx size );
SGS_APIFUNC int sgs_ProcessSubthreads( SGS_CTX, sgs_Real dt );
SGS_APIFUNC void sgs_EndOn( SGS_CTX, sgs_Variable ev, int enable );


#define SGS_CODE_ER 0 /* error codes */
#define SGS_CODE_VT 1 /* variable type */
#define SGS_CODE_OP 2 /* VM instruction */
SGS_APIFUNC const char* sgs_CodeString( int type, int val );


/* Core systems */
#define SGSOUTPUTFN_DEFAULT ((sgs_OutputFunc)-1)
SGS_APIFUNC void sgs_GetOutputFunc( SGS_CTX, sgs_OutputFunc* outf, void** outc );
SGS_APIFUNC void sgs_SetOutputFunc( SGS_CTX, sgs_OutputFunc func, void* ctx );
SGS_APIFUNC void sgs_Write( SGS_CTX, const void* ptr, size_t size );
SGS_APIFUNC SGSBOOL sgs_Writef( SGS_CTX, const char* what, ... );

SGS_APIFUNC void sgs_GetErrOutputFunc( SGS_CTX, sgs_OutputFunc* outf, void** outc );
SGS_APIFUNC void sgs_SetErrOutputFunc( SGS_CTX, sgs_OutputFunc func, void* ctx );
SGS_APIFUNC void sgs_ErrWrite( SGS_CTX, const void* ptr, size_t size );
SGS_APIFUNC SGSBOOL sgs_ErrWritef( SGS_CTX, const char* what, ... );

#define SGSMSGFN_DEFAULT ((sgs_MsgFunc)-1)
#define SGSMSGFN_DEFAULT_NOABORT ((sgs_MsgFunc)-2)
SGS_APIFUNC void sgs_GetMsgFunc( SGS_CTX, sgs_MsgFunc* outf, void** outc );
SGS_APIFUNC void sgs_SetMsgFunc( SGS_CTX, sgs_MsgFunc func, void* ctx );
SGS_APIFUNC SGSZERO sgs_Msg( SGS_CTX, int type, const char* what, ... );

typedef int (*sgs_ErrorOutputFunc) ( void*, const char*, ... );
#define SGS_ERRORINFO_STACK 0x01
#define SGS_ERRORINFO_ERROR 0x02
#define SGS_ERRORINFO_FULL (SGS_ERRORINFO_STACK | SGS_ERRORINFO_ERROR)
SGS_APIFUNC void sgs_WriteErrorInfo( SGS_CTX, int flags,
	sgs_ErrorOutputFunc func, void* ctx, int type, const char* msg );
SGS_APIFUNC void sgs_PushErrorInfo( SGS_CTX, int flags, int type, const char* msg );

SGS_APIFUNC SGSBOOL sgs_GetHookFunc( SGS_CTX, sgs_HookFunc* outf, void** outc );
SGS_APIFUNC void sgs_SetHookFunc( SGS_CTX, sgs_HookFunc func, void* ctx );

SGS_APIFUNC SGSBOOL sgs_GetScriptFSFunc( SGS_CTX, sgs_ScriptFSFunc* outf, void** outc );
SGS_APIFUNC void sgs_SetScriptFSFunc( SGS_CTX, sgs_ScriptFSFunc func, void* ctx );

SGS_APIFUNC void* sgs_Memory( SGS_CTX, void* ptr, size_t size );
#define sgs_Malloc( C, size ) sgs_Memory( C, NULL, size )
#define sgs_Free( C, ptr ) sgs_Memory( C, ptr, 0 )
#define sgs_Realloc sgs_Memory
/* assumes SGS_CTX: */
#define sgs_Alloc( what ) (what*) sgs_Malloc( C, sizeof(what) )
#define sgs_Alloc_n( what, cnt ) (what*) sgs_Malloc( C, sizeof(what) * cnt )
#define sgs_Alloc_a( what, app ) (what*) sgs_Malloc( C, sizeof(what) + app )
#define sgs_Dealloc( ptr ) sgs_Free( C, ptr )


SGS_APIFUNC SGSRESULT sgs_EvalBuffer( SGS_CTX, const char* buf, size_t size, int* outrvc );
SGS_APIFUNC SGSRESULT sgs_EvalFile( SGS_CTX, const char* file, int* outrvc );
SGS_APIFUNC SGSBOOL sgs_IncludeExt( SGS_CTX, const char* name, const char* searchpath );
SGS_APIFUNC SGSRESULT sgs_Compile( SGS_CTX, const char* buf, size_t size, char** outbuf, size_t* outsize );

SGS_APIFUNC SGSRESULT sgs_DumpCompiled( SGS_CTX, const char* buf, size_t size );
SGS_APIFUNC SGSBOOL sgs_Abort( SGS_CTX );
SGS_APIFUNC ptrdiff_t sgs_Stat( SGS_CTX, int type );
SGS_APIFUNC int32_t sgs_Cntl( SGS_CTX, int what, int32_t val );

SGS_APIFUNC void sgs_StackFrameInfo( SGS_CTX, sgs_StackFrame* frame, const char** name, const char** file, int* line );
SGS_APIFUNC sgs_StackFrame* sgs_GetFramePtr( SGS_CTX, sgs_StackFrame* from, int bwd );


#ifndef SGS_STRINGLENGTHFUNC
#define SGS_STRINGLENGTHFUNC strlen
#endif

#define sgs_ExecBuffer( C, buf, sz ) sgs_EvalBuffer( C, buf, sz, NULL )
#define sgs_ExecString( C, str ) sgs_ExecBuffer( C, str, SGS_STRINGLENGTHFUNC( str ) )
#define sgs_EvalString( C, str, outrvc ) sgs_EvalBuffer( C, str, SGS_STRINGLENGTHFUNC( str ), outrvc )
#define sgs_ExecFile( C, str ) sgs_EvalFile( C, str, NULL )
#define sgs_Include( C, str ) sgs_IncludeExt( C, str, NULL )
#define sgs_WriteStr( C, str ) sgs_Write( C, str, SGS_STRINGLENGTHFUNC( str ) )
#define sgs_ErrWriteStr( C, str ) sgs_ErrWrite( C, str, SGS_STRINGLENGTHFUNC( str ) )


/* Additional libraries */

#if !SGS_NO_STDLIB
SGS_APIFUNC void sgs_LoadLib_Fmt( SGS_CTX );
SGS_APIFUNC void sgs_LoadLib_IO( SGS_CTX );
SGS_APIFUNC void sgs_LoadLib_Math( SGS_CTX );
SGS_APIFUNC void sgs_LoadLib_OS( SGS_CTX );
SGS_APIFUNC void sgs_LoadLib_RE( SGS_CTX );
SGS_APIFUNC void sgs_LoadLib_String( SGS_CTX );
#endif

#define SGS_RC_END() { NULL, NULL }

SGS_APIFUNC void sgs_RegSymbol( SGS_CTX, const char* prefix, const char* name, sgs_Variable var );
SGS_APIFUNC SGSBOOL sgs_GetSymbol( SGS_CTX, sgs_Variable var, sgs_Variable* out );

typedef struct _sgs_RegFuncConst
{
	const char* name;
	sgs_CFunc value;
}
sgs_RegFuncConst;
SGS_APIFUNC void sgs_RegFuncConstsExt( SGS_CTX, const sgs_RegFuncConst* list, int size, const char* prefix );
#define sgs_RegFuncConsts( C, list, size ) sgs_RegFuncConstsExt( C, list, size, "" );
SGS_APIFUNC void sgs_StoreFuncConsts( SGS_CTX, sgs_Variable var, const sgs_RegFuncConst* list, int size, const char* prefix );

typedef struct _sgs_RegIntConst
{
	const char* name;
	sgs_Int value;
}
sgs_RegIntConst;
SGS_APIFUNC void sgs_RegIntConsts( SGS_CTX, const sgs_RegIntConst* list, int size );
SGS_APIFUNC void sgs_StoreIntConsts( SGS_CTX, sgs_Variable var, const sgs_RegIntConst* list, int size );

typedef struct _sgs_RegRealConst
{
	const char* name;
	sgs_Real value;
}
sgs_RegRealConst;
SGS_APIFUNC void sgs_RegRealConsts( SGS_CTX, const sgs_RegRealConst* list, int size );
SGS_APIFUNC void sgs_StoreRealConsts( SGS_CTX, sgs_Variable var, const sgs_RegRealConst* list, int size );

SGS_APIFUNC SGSBOOL sgs_RegisterType( SGS_CTX, const char* name, sgs_ObjInterface* iface );
SGS_APIFUNC SGSBOOL sgs_UnregisterType( SGS_CTX, const char* name );
SGS_APIFUNC sgs_ObjInterface* sgs_FindType( SGS_CTX, const char* name );

SGS_APIFUNC SGSONE sgs_PushInterface( SGS_CTX, sgs_CFunc igfn );
SGS_APIFUNC void sgs_InitInterface( SGS_CTX, sgs_Variable* var, sgs_CFunc igfn );


/* The core interface */

/*
	OBJECT CREATION
*/

static SGS_INLINE sgs_Variable sgs_MakeNull()
{
	sgs_Variable out; out.type = SGS_VT_NULL; return out;
}
static SGS_INLINE sgs_Variable sgs_MakeBool( sgs_Bool v )
{
	sgs_Variable out; out.type = SGS_VT_BOOL; out.data.B = v ? 1 : 0; return out;
}
static SGS_INLINE sgs_Variable sgs_MakeInt( sgs_Int v )
{
	sgs_Variable out; out.type = SGS_VT_INT; out.data.I = v; return out;
}
static SGS_INLINE sgs_Variable sgs_MakeReal( sgs_Real v )
{
	sgs_Variable out; out.type = SGS_VT_REAL; out.data.R = v; return out;
}
static SGS_INLINE sgs_Variable sgs_MakeCFunc( sgs_CFunc v )
{
	sgs_Variable out; out.type = SGS_VT_CFUNC; out.data.C = v; return out;
}
static SGS_INLINE sgs_Variable sgs_MakePtr( void* v )
{
	sgs_Variable out; out.type = SGS_VT_PTR; out.data.P = v; return out;
}

SGS_APIFUNC void sgs_InitStringBuf( SGS_CTX, sgs_Variable* out, const char* str, sgs_SizeVal size );
SGS_APIFUNC void sgs_InitString( SGS_CTX, sgs_Variable* out, const char* str );
SGS_APIFUNC void sgs_InitObjectPtr( sgs_Variable* out, sgs_VarObj* obj );
SGS_APIFUNC void sgs_InitThreadPtr( sgs_Variable* out, sgs_Context* T );

/* pushed to stack if out = null */
SGS_APIFUNC SGSONE sgs_CreateObject( SGS_CTX, sgs_Variable* out, void* data, sgs_ObjInterface* iface );
SGS_APIFUNC void* sgs_CreateObjectIPA( SGS_CTX, sgs_Variable* out, uint32_t added, sgs_ObjInterface* iface );
SGS_APIFUNC SGSONE sgs_CreateArray( SGS_CTX, sgs_Variable* out, sgs_SizeVal numitems );
SGS_APIFUNC SGSONE sgs_CreateDict( SGS_CTX, sgs_Variable* out, sgs_SizeVal numitems );
SGS_APIFUNC SGSONE sgs_CreateMap( SGS_CTX, sgs_Variable* out, sgs_SizeVal numitems );
SGS_APIFUNC SGSONE sgs_CreateEvent( SGS_CTX, sgs_Variable* out );
SGS_APIFUNC SGSONE sgs_CreatePooledEventBuf( SGS_CTX, sgs_Variable* out, sgs_Variable dict, const char* str, sgs_SizeVal size );
#define sgs_CreatePooledEvent( C, out, dict, str ) sgs_CreatePooledEventBuf( C, out, dict, str, SGS_STRINGLENGTHFUNC( str ) )

/*
	STACK & SUB-ITEMS
*/
#define sgs_PushNull( C ) sgs_PushNulls( C, 1 )
SGS_APIFUNC SGSONE sgs_PushNulls( SGS_CTX, int count );
SGS_APIFUNC SGSONE sgs_PushBool( SGS_CTX, sgs_Bool value );
SGS_APIFUNC SGSONE sgs_PushInt( SGS_CTX, sgs_Int value );
SGS_APIFUNC SGSONE sgs_PushReal( SGS_CTX, sgs_Real value );
SGS_APIFUNC SGSONE sgs_PushStringBuf( SGS_CTX, const char* str, sgs_SizeVal size );
SGS_APIFUNC SGSONE sgs_PushString( SGS_CTX, const char* str );
SGS_APIFUNC SGSONE sgs_PushCFunc( SGS_CTX, sgs_CFunc func );
SGS_APIFUNC SGSONE sgs_PushObjectPtr( SGS_CTX, sgs_VarObj* obj );
SGS_APIFUNC SGSONE sgs_PushThreadPtr( SGS_CTX, sgs_Context* T );
SGS_APIFUNC SGSONE sgs_PushPtr( SGS_CTX, void* ptr );

SGS_APIFUNC SGSONE sgs_PushVariable( SGS_CTX, sgs_Variable var );
#define sgs_PushItem( C, item ) sgs_PushVariable( C, sgs_StackItem( C, item ) )
SGS_APIFUNC void sgs_StoreVariable( SGS_CTX, sgs_Variable* out );
SGS_APIFUNC void sgs_SetStackItem( SGS_CTX, sgs_StkIdx item, sgs_Variable val );
SGS_APIFUNC void sgs_InsertVariable( SGS_CTX, sgs_StkIdx pos, sgs_Variable val );

/* string generation */
SGS_APIFUNC char* sgs_PushStringAlloc( SGS_CTX, sgs_SizeVal size );
SGS_APIFUNC char* sgs_InitStringAlloc( SGS_CTX, sgs_Variable* var, sgs_SizeVal size );
SGS_APIFUNC void sgs_FinalizeStringAlloc( SGS_CTX, sgs_StkIdx item );
SGS_APIFUNC void sgs_FinalizeStringAllocP( SGS_CTX, sgs_Variable* var );

/* almost-raw access */
SGS_APIFUNC SGSBOOL sgs_GetIndex( SGS_CTX, sgs_Variable obj, sgs_Variable idx, sgs_Variable* out, int isprop );
SGS_APIFUNC SGSBOOL sgs_SetIndex( SGS_CTX, sgs_Variable obj, sgs_Variable idx, sgs_Variable val, int isprop );
SGS_APIFUNC SGSBOOL sgs_PushIndex( SGS_CTX, sgs_Variable obj, sgs_Variable idx, int isprop );

/* special case sub-variable access */
SGS_APIFUNC SGSBOOL sgs_PushProperty( SGS_CTX, sgs_Variable obj, const char* name );
SGS_APIFUNC SGSBOOL sgs_SetProperty( SGS_CTX, sgs_Variable obj, const char* name, sgs_Variable val );
SGS_APIFUNC SGSBOOL sgs_PushNumIndex( SGS_CTX, sgs_Variable obj, sgs_Int idx );
SGS_APIFUNC SGSBOOL sgs_SetNumIndex( SGS_CTX, sgs_Variable obj, sgs_Int idx, sgs_Variable val );

/* raw & simplified global access */
SGS_APIFUNC SGSBOOL sgs_GetGlobal( SGS_CTX, sgs_Variable idx, sgs_Variable* out );
SGS_APIFUNC SGSBOOL sgs_SetGlobal( SGS_CTX, sgs_Variable idx, sgs_Variable val );
SGS_APIFUNC SGSBOOL sgs_PushGlobalByName( SGS_CTX, const char* name );
SGS_APIFUNC SGSBOOL sgs_GetGlobalByName( SGS_CTX, const char* name, sgs_Variable* out );
SGS_APIFUNC void sgs_SetGlobalByName( SGS_CTX, const char* name, sgs_Variable val );

#define SGS_REG_ROOT 0
#define SGS_REG_SYM 1
#define SGS_REG_INC 2
SGS_APIFUNC sgs_Variable sgs_Registry( SGS_CTX, int subtype );

SGS_APIFUNC void sgs_GetEnv( SGS_CTX, sgs_Variable* out );
SGS_APIFUNC void sgs_SetEnv( SGS_CTX, sgs_Variable var );
SGS_APIFUNC void sgs_PushEnv( SGS_CTX );

SGS_APIFUNC SGSBOOL sgs_PushPath( SGS_CTX, sgs_Variable var, const char* path, ... );
SGS_APIFUNC SGSBOOL sgs_StorePath( SGS_CTX, sgs_Variable var, sgs_Variable val, const char* path, ... );

SGS_APIFUNC int sgs_ArgErrorExt( SGS_CTX, int argid, int gotthis, const char* expect, const char* expfx );
SGS_APIFUNC int sgs_ArgError( SGS_CTX, int argid, int gotthis, int expect, int is_strict );

#define sgs_FuncArgError( C, argid, expect, strict ) sgs_ArgError( C, argid, 0, expect, strict )
#define sgs_MethodArgError( C, argid, expect, strict ) sgs_ArgError( C, argid, 1, expect, strict )

typedef int (*sgs_ArgCheckFunc)
(
	sgs_Context* /* ctx / SGS_CTX */,
	int /* argid */,
	va_list* /* args */,
	int /* flags */
);
#define SGS_LOADARG_STRICT 0x01
#define SGS_LOADARG_WRITE 0x02
#define SGS_LOADARG_OPTIONAL 0x04
#define SGS_LOADARG_INTSIGN 0x08
#define SGS_LOADARG_INTRANGE 0x10
#define SGS_LOADARG_INTCLAMP 0x20

SGS_APIFUNC SGSBOOL sgs_LoadArgsExtVA( SGS_CTX, int from, const char* cmd, va_list* args );
SGS_APIFUNC SGSBOOL sgs_LoadArgsExt( SGS_CTX, int from, const char* cmd, ... );
SGS_APIFUNC SGSBOOL sgs_LoadArgs( SGS_CTX, const char* cmd, ... );
SGS_APIFUNC SGSBOOL sgs_ParseMethod( SGS_CTX, sgs_ObjInterface* iface, void** ptrout,
	const char* method_name, const char* func_name );
#define SGS_PARSE_METHOD( C, iface, ptr, objname, methodname ) \
	sgs_ParseMethod( C, iface, (void**) &ptr, #objname "." #methodname, #objname "_" #methodname )

SGS_APIFUNC int sgs_ArgCheck_Object( SGS_CTX, int argid, va_list* args, int flags );

SGS_APIFUNC void sgs_Pop( SGS_CTX, sgs_StkIdx count );
SGS_APIFUNC void sgs_PopSkip( SGS_CTX, sgs_StkIdx count, sgs_StkIdx skip );

SGS_APIFUNC sgs_StkIdx sgs_StackSize( SGS_CTX );
SGS_APIFUNC void sgs_SetStackSize( SGS_CTX, sgs_StkIdx size );
SGS_APIFUNC void sgs_SetDeltaSize( SGS_CTX, sgs_StkIdx diff );
SGS_APIFUNC sgs_StkIdx sgs_AbsIndex( SGS_CTX, sgs_StkIdx item );
SGS_APIFUNC SGSBOOL sgs_IsValidIndex( SGS_CTX, sgs_StkIdx item );
SGS_APIFUNC sgs_Variable sgs_OptStackItem( SGS_CTX, sgs_StkIdx item );
SGS_APIFUNC sgs_Variable sgs_StackItem( SGS_CTX, sgs_StkIdx item );
SGS_APIFUNC SGSBOOL sgs_GetStackItem( SGS_CTX, sgs_StkIdx item, sgs_Variable* out );
SGS_APIFUNC uint32_t sgs_ItemType( SGS_CTX, sgs_StkIdx item );

SGS_APIFUNC void sgs_Assign( SGS_CTX, sgs_Variable* var_to, sgs_Variable* var_from );
SGS_APIFUNC void sgs_ArithOp( SGS_CTX, sgs_Variable* out, sgs_Variable* A, sgs_Variable* B, int op );
SGS_APIFUNC void sgs_IncDec( SGS_CTX, sgs_Variable* out, sgs_Variable* A, int inc );

/*
	CLOSURES
*/
SGS_APIFUNC void sgs_ClPushNulls( SGS_CTX, sgs_StkIdx num );
SGS_APIFUNC void sgs_ClPushVariable( SGS_CTX, sgs_Variable var );
SGS_APIFUNC void sgs_ClPushItem( SGS_CTX, sgs_StkIdx item );
SGS_APIFUNC void sgs_ClPop( SGS_CTX, sgs_StkIdx num );
SGS_APIFUNC void sgs_MakeClosure( SGS_CTX, sgs_Variable* func, sgs_StkIdx clcount, sgs_Variable* out );
SGS_APIFUNC void sgs_ClGetItem( SGS_CTX, sgs_StkIdx item, sgs_Variable* out );
SGS_APIFUNC void sgs_ClSetItem( SGS_CTX, sgs_StkIdx item, sgs_Variable* var );

/*
	OPERATIONS
*/
#define SGS_FSTKTOP sgs_FuncStackTopHint() /* for use only with **Call() */
static SGS_INLINE sgs_Variable sgs_FuncStackTopHint()
{
	sgs_Variable sv;
	sv.type = 255;
	return sv;
}

SGS_APIFUNC void sgs_XFCall( SGS_CTX, sgs_Variable callable, int args, int* outrvc, int gotthis );
#define sgs_XCall( C, callable, args, outrvc ) sgs_XFCall( C, callable, args, outrvc, 0 )
#define sgs_XThisCall( C, callable, args, outrvc ) sgs_XFCall( C, callable, args, outrvc, 1 )
SGS_APIFUNC void sgs_FCall( SGS_CTX, sgs_Variable callable, int args, int expect, int gotthis );
#define sgs_Call( C, callable, args, expect ) sgs_FCall( C, callable, args, expect, 0 )
#define sgs_ThisCall( C, callable, args, expect ) sgs_FCall( C, callable, args, expect, 1 )
SGS_APIFUNC SGSBOOL sgs_GlobalCall( SGS_CTX, const char* name, int args, int expect );
SGS_APIFUNC void sgs_TypeOf( SGS_CTX, sgs_Variable var );
SGS_APIFUNC void sgs_DumpVar( SGS_CTX, sgs_Variable var, int maxdepth );
SGS_APIFUNC void sgs_GCExecute( SGS_CTX );

SGS_APIFUNC const char* sgs_DebugDumpVarExt( SGS_CTX, sgs_Variable var, int maxdepth );
#define sgs_DebugDumpVar( C, var ) sgs_DebugDumpVarExt( C, var, 5 )
#define sgs_DebugPrintVar( C, var ) sgs_DebugDumpVarExt( C, var, -1 )

SGS_APIFUNC void sgs_PadString( SGS_CTX );
SGS_APIFUNC void sgs_ToPrintSafeString( SGS_CTX );
SGS_APIFUNC void sgs_StringConcat( SGS_CTX, sgs_StkIdx args );
SGS_APIFUNC void sgs_CloneItem( SGS_CTX, sgs_Variable var );

SGS_APIFUNC SGSBOOL sgs_IsArray( sgs_Variable var );
SGS_APIFUNC sgs_SizeVal sgs_ArraySize( sgs_Variable var );
SGS_APIFUNC void sgs_ArrayPush( SGS_CTX, sgs_Variable var, sgs_StkIdx count );
SGS_APIFUNC void sgs_ArrayPop( SGS_CTX, sgs_Variable var, sgs_StkIdx count, SGSBOOL ret );
SGS_APIFUNC void sgs_ArrayErase( SGS_CTX, sgs_Variable var, sgs_StkIdx at, sgs_StkIdx count );
SGS_APIFUNC sgs_SizeVal sgs_ArrayFind( SGS_CTX, sgs_Variable var, sgs_Variable what );
SGS_APIFUNC sgs_SizeVal sgs_ArrayRemove( SGS_CTX, sgs_Variable var, sgs_Variable what, SGSBOOL all );
SGS_APIFUNC SGSBOOL sgs_IsDict( sgs_Variable var );
SGS_APIFUNC SGSBOOL sgs_IsMap( sgs_Variable var );
SGS_APIFUNC SGSBOOL sgs_Unset( SGS_CTX, sgs_Variable var, sgs_Variable key );
SGS_APIFUNC SGSBOOL sgs_EventState( SGS_CTX, sgs_Variable evt, int state );

SGS_APIFUNC void sgs_Serialize( SGS_CTX, sgs_Variable var );
SGS_APIFUNC void sgs_SerializeObject( SGS_CTX, sgs_StkIdx args, const char* func );
SGS_APIFUNC SGSBOOL sgs_Unserialize( SGS_CTX, sgs_Variable var );

SGS_APIFUNC void sgs_SerializeV1( SGS_CTX, sgs_Variable var );
SGS_APIFUNC SGSBOOL sgs_UnserializeV1( SGS_CTX, sgs_Variable var );
SGS_APIFUNC void sgs_SerializeV2( SGS_CTX, sgs_Variable var );
SGS_APIFUNC SGSBOOL sgs_UnserializeV2( SGS_CTX, sgs_Variable var );

SGS_APIFUNC int sgs_Compare( SGS_CTX, sgs_Variable* v1, sgs_Variable* v2 );
SGS_APIFUNC SGSBOOL sgs_EqualTypes( sgs_Variable* v1, sgs_Variable* v2 );

/*
	CONVERSION / RETRIEVAL
*/
/* pointer versions */
SGS_APIFUNC sgs_Bool sgs_GetBoolP( SGS_CTX, sgs_Variable* var );
SGS_APIFUNC sgs_Int sgs_GetIntP( SGS_CTX, sgs_Variable* var );
SGS_APIFUNC sgs_Real sgs_GetRealP( SGS_CTX, sgs_Variable* var );
SGS_APIFUNC void* sgs_GetPtrP( SGS_CTX, sgs_Variable* var );

SGS_APIFUNC char* sgs_ToStringBufP( SGS_CTX, sgs_Variable* var, sgs_SizeVal* outsize );
#define sgs_ToStringP( ctx, var ) sgs_ToStringBufP( ctx, var, NULL )
SGS_APIFUNC char* sgs_ToStringBufFastP( SGS_CTX, sgs_Variable* var, sgs_SizeVal* outsize );
#define sgs_ToStringFastP( ctx, var ) sgs_ToStringBufFast( ctx, var, NULL )

SGS_APIFUNC SGSBOOL sgs_IsObjectP( sgs_Variable* var, sgs_ObjInterface* iface );
#define sgs_IsTypeP( C, var, name ) sgs_IsObjectP( var, sgs_FindType( C, name ) )
SGS_APIFUNC SGSBOOL sgs_IsCallableP( sgs_Variable* var );
SGS_APIFUNC SGSBOOL sgs_ParseStringP( SGS_CTX, sgs_Variable* var, char** out, sgs_SizeVal* size );

/* index versions */
SGS_APIFUNC sgs_Bool sgs_GetBool( SGS_CTX, sgs_StkIdx item );
SGS_APIFUNC sgs_Int sgs_GetInt( SGS_CTX, sgs_StkIdx item );
SGS_APIFUNC sgs_Real sgs_GetReal( SGS_CTX, sgs_StkIdx item );
SGS_APIFUNC void* sgs_GetPtr( SGS_CTX, sgs_StkIdx item );

SGS_APIFUNC char* sgs_ToStringBuf( SGS_CTX, sgs_StkIdx item, sgs_SizeVal* outsize );
#define sgs_ToString( ctx, item ) sgs_ToStringBuf( ctx, item, NULL )
SGS_APIFUNC char* sgs_ToStringBufFast( SGS_CTX, sgs_StkIdx item, sgs_SizeVal* outsize );
#define sgs_ToStringFast( ctx, item ) sgs_ToStringBufFast( ctx, item, NULL )

SGS_APIFUNC SGSBOOL sgs_IsObject( SGS_CTX, sgs_StkIdx item, sgs_ObjInterface* iface );
#define sgs_IsType( C, item, name ) sgs_IsObject( C, item, sgs_FindType( C, name ) )
SGS_APIFUNC SGSBOOL sgs_IsCallable( SGS_CTX, sgs_StkIdx item );
SGS_APIFUNC SGSBOOL sgs_ParseBool( SGS_CTX, sgs_StkIdx item, sgs_Bool* out );
SGS_APIFUNC SGSBOOL sgs_ParseInt( SGS_CTX, sgs_StkIdx item, sgs_Int* out );
SGS_APIFUNC SGSBOOL sgs_ParseReal( SGS_CTX, sgs_StkIdx item, sgs_Real* out );
SGS_APIFUNC SGSBOOL sgs_ParseString( SGS_CTX, sgs_StkIdx item, char** out, sgs_SizeVal* size );
SGS_APIFUNC SGSBOOL sgs_ParseObjectPtr( SGS_CTX, sgs_StkIdx item,
	sgs_ObjInterface* iface, sgs_VarObj** out, int strict );
SGS_APIFUNC SGSBOOL sgs_ParsePtr( SGS_CTX, sgs_StkIdx item, void** out );

/* global versions */
SGS_APIFUNC sgs_Bool sgs_GlobalBool( SGS_CTX, const char* name );
SGS_APIFUNC sgs_Int sgs_GlobalInt( SGS_CTX, const char* name );
SGS_APIFUNC sgs_Real sgs_GlobalReal( SGS_CTX, const char* name );
SGS_APIFUNC char* sgs_GlobalStringBuf( SGS_CTX, const char* name, sgs_SizeVal* outsize );
#define sgs_GlobalString( C, name ) sgs_GlobalStringBuf( C, name, NULL )

/* iterator interface fns */
SGS_APIFUNC SGSBOOL sgs_PushIterator( SGS_CTX, sgs_Variable var );
SGS_APIFUNC SGSBOOL sgs_GetIterator( SGS_CTX, sgs_Variable var, sgs_Variable* out );
SGS_APIFUNC SGSBOOL sgs_IterAdvance( SGS_CTX, sgs_Variable var );
SGS_APIFUNC void sgs_IterPushData( SGS_CTX, sgs_Variable var, int key, int value );
SGS_APIFUNC void sgs_IterGetData( SGS_CTX, sgs_Variable var, sgs_Variable* key, sgs_Variable* value );

SGS_APIFUNC SGSBOOL sgs_IsNumericString( const char* str, sgs_SizeVal size );

/*
	EXTENSION UTILITIES
*/
SGS_APIFUNC SGSBOOL sgs_Method( SGS_CTX );
SGS_APIFUNC SGSBOOL sgs_HideThis( SGS_CTX );
SGS_APIFUNC SGSBOOL sgs_ForceHideThis( SGS_CTX );
SGS_APIFUNC int sgs_ObjectArg( SGS_CTX );

SGS_APIFUNC void sgs_Acquire( SGS_CTX, sgs_Variable* var );
SGS_APIFUNC void sgs_AcquireArray( SGS_CTX, sgs_Variable* var, sgs_SizeVal count );
SGS_APIFUNC void sgs_Release( SGS_CTX, sgs_Variable* var );
SGS_APIFUNC void sgs_ReleaseArray( SGS_CTX, sgs_Variable* var, sgs_SizeVal count );
SGS_APIFUNC void sgs_GCMark( SGS_CTX, sgs_Variable* var );
SGS_APIFUNC void sgs_GCMarkArray( SGS_CTX, sgs_Variable* var, sgs_SizeVal count );
SGS_APIFUNC void sgs_ObjAcquire( SGS_CTX, sgs_VarObj* obj );
SGS_APIFUNC void sgs_ObjRelease( SGS_CTX, sgs_VarObj* obj );
SGS_APIFUNC void sgs_ObjGCMark( SGS_CTX, sgs_VarObj* obj );
SGS_APIFUNC void sgs_ObjAssign( SGS_CTX, sgs_VarObj** dest, sgs_VarObj* src );
SGS_APIFUNC void sgs_ObjCallDtor( SGS_CTX, sgs_VarObj* obj );
SGS_APIFUNC void sgs_ObjSetMetaObj( SGS_CTX, sgs_VarObj* obj, sgs_VarObj* metaobj );
SGS_APIFUNC sgs_VarObj* sgs_ObjGetMetaObj( sgs_VarObj* obj );
SGS_APIFUNC void sgs_ObjSetMetaMethodEnable( sgs_VarObj* obj, SGSBOOL enable );
SGS_APIFUNC SGSBOOL sgs_ObjGetMetaMethodEnable( sgs_VarObj* obj );

SGS_APIFUNC char* sgs_GetStringPtrP( sgs_Variable* var );
SGS_APIFUNC sgs_SizeVal sgs_GetStringSizeP( sgs_Variable* var );
SGS_APIFUNC sgs_VarObj* sgs_GetObjectStructP( sgs_Variable* var );
SGS_APIFUNC void* sgs_GetObjectDataP( sgs_Variable* var );
SGS_APIFUNC sgs_ObjInterface* sgs_GetObjectIfaceP( sgs_Variable* var );
SGS_APIFUNC void sgs_SetObjectDataP( sgs_Variable* var, void* data );
SGS_APIFUNC void sgs_SetObjectIfaceP( sgs_Variable* var, sgs_ObjInterface* iface );

SGS_APIFUNC char* sgs_GetStringPtr( SGS_CTX, sgs_StkIdx item );
SGS_APIFUNC sgs_SizeVal sgs_GetStringSize( SGS_CTX, sgs_StkIdx item );
SGS_APIFUNC sgs_VarObj* sgs_GetObjectStruct( SGS_CTX, sgs_StkIdx item );
SGS_APIFUNC void* sgs_GetObjectData( SGS_CTX, sgs_StkIdx item );
SGS_APIFUNC sgs_ObjInterface* sgs_GetObjectIface( SGS_CTX, sgs_StkIdx item );
SGS_APIFUNC void sgs_SetObjectData( SGS_CTX, sgs_StkIdx item, void* data );
SGS_APIFUNC void sgs_SetObjectIface( SGS_CTX, sgs_StkIdx item, sgs_ObjInterface* iface );

SGS_APIFUNC int sgs_HasFuncName( SGS_CTX );
SGS_APIFUNC void sgs_FuncName( SGS_CTX, const char* fnliteral );

#define SGSFN( x ) sgs_FuncName( C, x )
#define SGSBASEFN( x ) if( !sgs_HasFuncName( C ) ) sgs_FuncName( C, x )

static SGS_INLINE int sgs_Errno( SGS_CTX, int clear )
{
	sgs_Cntl( C, SGS_CNTL_ERRNO, clear );
	return clear;
}
#define sgs_SetErrno( C, err ) sgs_Cntl( C, SGS_CNTL_SET_ERRNO, err )
#define sgs_GetLastErrno( C ) sgs_Cntl( C, SGS_CNTL_GET_ERRNO, 0 )


/* predefined output / messaging functions */

static SGS_INLINE void sgs_StdOutputFunc( void* userdata, SGS_CTX, const void* ptr, size_t size )
{
	SGS_UNUSED( C );
	fwrite( ptr, 1, size, (FILE*) userdata );
}

static SGS_INLINE void sgs_StdMsgFunc_NoAbort( void* ctx, SGS_CTX, int type, const char* msg )
{
	SGS_UNUSED( ctx );
	sgs_WriteErrorInfo( C, SGS_ERRORINFO_FULL, (sgs_ErrorOutputFunc) sgs_ErrWritef, C, type, msg );
}

static SGS_INLINE void sgs_StdMsgFunc( void* ctx, SGS_CTX, int type, const char* msg )
{
	sgs_StdMsgFunc_NoAbort( ctx, C, type, msg );
	if( type >= SGS_ERROR )
		sgs_Abort( C );
}

static SGS_INLINE SGSRESULT sgs_StdScriptFSFunc( void* ctx, SGS_CTX, int op, sgs_ScriptFSData* data )
{
	SGS_UNUSED( ctx );
	SGS_UNUSED( C );
	switch( op )
	{
	case SGS_SFS_FILE_EXISTS:
		{
			FILE* f = fopen( data->filename, "rb" );
			if( f )
				fclose( f );
			return f ? SGS_SUCCESS : SGS_ENOTFND;
		}
	case SGS_SFS_FILE_OPEN:
		{
			long sz;
			FILE* f = fopen( data->filename, "rb" );
			if( f == NULL )
				return SGS_ENOTFND;
			fseek( f, 0, SEEK_END );
			sz = ftell( f );
			if( sz < 0 )
			{
				fclose( f );
				return SGS_EINPROC;
			}
			data->userhandle = f;
			data->size = (size_t) sz;
			return SGS_SUCCESS;
		}
	case SGS_SFS_FILE_READ:
		fseek( (FILE*) data->userhandle, 0, SEEK_SET );
		return fread( data->output, 1, data->size, (FILE*) data->userhandle ) == data->size ? SGS_SUCCESS : SGS_EINPROC;
	case SGS_SFS_FILE_CLOSE:
		fclose( (FILE*) data->userhandle );
		data->userhandle = NULL;
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}


/* utility macros */
#define SGS_MODULE_CHECK_VERSION( C ) if( SGS_VERSION_INT != sgs_Stat( C, SGS_STAT_VERSION ) ) \
	{ sgs_Msg( C, SGS_ERROR, "SGScript version mismatch: module compiled for %06X, loaded in %06X", \
	(int) SGS_VERSION_INT, (int) sgs_Stat( C, SGS_STAT_VERSION ) ); return SGS_ENOTSUP; }

#define SGS_RETURN_THIS( C ) sgs_Method( C ); sgs_SetStackSize( C, 1 ); return 1;

#define SGS_ARGS_OBJFUNC SGS_CTX, sgs_VarObj* obj
#define SGS_ARGS_GETINDEXFUNC SGS_CTX, sgs_VarObj* obj
#define SGS_ARGS_SETINDEXFUNC SGS_CTX, sgs_VarObj* obj
#define SGS_BEGIN_INDEXFUNC char* str; if( sgs_ParseString( C, 0, &str, NULL ) ){
#define SGS_END_INDEXFUNC } return SGS_ENOTFND;
#define SGS_CASE( name ) if( !strcmp( str, name ) )


#ifdef __cplusplus
}
#endif

#endif /* SGSCRIPT_H_INCLUDED */
