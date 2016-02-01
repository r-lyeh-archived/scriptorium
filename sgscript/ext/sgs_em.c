
// compile with:
// emcc sgs_em.c -o sgscript.js -s EXPORTED_FUNCTIONS="['_runsgs']" -O2 --memory-init-file 0

#define SGS_INTERNAL_STRINGTABLES
#define SGS_USE_FILESYSTEM

#include "../src/sgs_util.c"
#include "../src/sgs_ctx.c"
#include "../src/sgs_tok.c"
#include "../src/sgs_fnt.c"
#include "../src/sgs_bcg.c"
#include "../src/sgs_proc.c"
#include "../src/sgs_regex.c"
#include "../src/sgs_std.c"
#include "../src/sgs_stdL.c"
#include "../src/sgs_xpc.c"

#define HEADER_SGSCRIPT_H "../src/sgscript.h"
#include "sgsxgmath.c"

#define MAX_OUTPUT_SIZE 1024*1024

char* outbuf = NULL;
char* outbuf_at = NULL;
void output_to_buffer( void* userdata, SGS_CTX, const void* ptr, size_t size )
{
	char* end = outbuf + MAX_OUTPUT_SIZE;
	size_t realsize = end - outbuf_at - 1;
	if( size > realsize )
		size = realsize;
	memcpy( outbuf_at, ptr, size );
	outbuf_at += size;
}

char* runsgs( const char* script )
{
	if( !outbuf )
	{
		outbuf = malloc( MAX_OUTPUT_SIZE );
	}
	
	SGS_CTX = sgs_CreateEngine();
	outbuf_at = outbuf;
	sgs_SetOutputFunc( C, output_to_buffer, NULL );
	sgs_SetErrOutputFunc( C, output_to_buffer, NULL );
	sgs_LoadLib_Fmt( C );
	/* no need for I/O - can't use it from the browser */
	sgs_LoadLib_Math( C );
	sgs_LoadLib_OS( C );
	sgs_LoadLib_RE( C );
	sgs_LoadLib_String( C );
	xgm_module_entry_point( C );
	sgs_ExecString( C, script );
	sgs_DestroyEngine( C );
	*outbuf_at = 0;
	return outbuf;
}
