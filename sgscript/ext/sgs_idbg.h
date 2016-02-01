
#ifndef SGS_IDBG_H_INCLUDED
#define SGS_IDBG_H_INCLUDED


#ifdef __cplusplus
extern "C" {
#endif


#ifndef HEADER_SGSCRIPT_H
# define HEADER_SGSCRIPT_H <sgscript.h>
#endif
#include HEADER_SGSCRIPT_H
#ifndef HEADER_SGS_UTIL_H
# define HEADER_SGS_UTIL_H <sgs_util.h>
#endif
#include HEADER_SGS_UTIL_H

typedef
struct _sgs_IDbg
{
	SGS_CTX;
	sgs_MsgFunc pfn;
	void* pctx;
	sgs_MemBuf input;
	char iword[ 32 ];
	int inside;
	ptrdiff_t stkoff;
	ptrdiff_t stksize;

	int minlev;
}
sgs_IDbg;

#define SGS_IDBG sgs_IDbg* D

int sgs_InitIDbg( SGS_CTX, SGS_IDBG );
int sgs_CloseIDbg( SGS_CTX, SGS_IDBG );


#ifdef __cplusplus
}
#endif

#endif
