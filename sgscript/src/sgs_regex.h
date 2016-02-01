

#pragma once

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>


#define RXSUCCESS 0
#define RXEINMOD  -1 /* invalid modifier */
#define RXEPART   -2 /* partial (sub-)expression */
#define RXEUNEXP  -3 /* unexpected character */
#define RXERANGE  -4 /* invalid range (min > max) */
#define RXELIMIT  -5 /* too many digits */
#define RXEEMPTY  -6 /* expression is effectively empty */
#define RXENOREF  -7 /* the specified backreference cannot be used here */

#define RX_ALLMODS "mis"

#ifndef RX_STRLENGTHFUNC
#define RX_STRLENGTHFUNC( str ) strlen( str )
#endif


typedef void* (*srx_MemFunc)
(
	void* /* userdata */,
	void* /* ptr */,
	size_t /* size */
);

#ifdef RX_NEED_DEFAULT_MEMFUNC
static void* srx_DefaultMemFunc( void* userdata, void* ptr, size_t size )
{
	(void) userdata;
	if( size )
		return realloc( ptr, size );
	free( ptr );
	return NULL;
}
#endif

typedef char RX_Char;

typedef struct _srx_Context srx_Context;


srx_Context* srx_CreateExt( const RX_Char* str, size_t strsize, const RX_Char* mods, int* errnpos, srx_MemFunc memfn, void* memctx );
#define srx_Create( str, mods ) srx_CreateExt( str, RX_STRLENGTHFUNC(str), mods, NULL, srx_DefaultMemFunc, NULL )
int srx_Destroy( srx_Context* R );
void srx_DumpToStdout( srx_Context* R );

int srx_MatchExt( srx_Context* R, const RX_Char* str, size_t size, size_t offset );
#define srx_Match( R, str, off ) srx_MatchExt( R, str, RX_STRLENGTHFUNC(str), off )
int srx_GetCaptureCount( srx_Context* R );
int srx_GetCaptured( srx_Context* R, int which, size_t* pbeg, size_t* pend );
int srx_GetCapturedPtrs( srx_Context* R, int which, const RX_Char** pbeg, const RX_Char** pend );

RX_Char* srx_ReplaceExt( srx_Context* R, const RX_Char* str, size_t strsize, const RX_Char* rep, size_t repsize, size_t* outsize );
#define srx_Replace( R, str, rep ) srx_ReplaceExt( R, str, RX_STRLENGTHFUNC(str), rep, RX_STRLENGTHFUNC(rep), NULL )
void srx_FreeReplaced( srx_Context* R, RX_Char* repstr );


#ifdef __cplusplus
}
#endif

