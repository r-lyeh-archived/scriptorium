
#include <stdio.h>
#include <time.h>


#ifdef _WIN32
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>
#  define sgsthread_sleep( ms ) Sleep( (DWORD) ms )
#else
#  include <unistd.h>
static void sgsthread_sleep( unsigned ms )
{
	if( ms >= 1000 )
	{
		sleep( ms / 1000 );
		ms %= 1000;
	}
	if( ms > 0 )
	{
		usleep( ms * 1000 );
	}
}
#endif


#define SGS_USE_FILESYSTEM

#include "sgs_int.h"
#include "sgs_prof.h"


const char* outfile = "apitests-output.log";
const char* outfile_errors = "apitests-errors.log";



#define DEFINE_TEST( name ) static void at_##name()

typedef void (*testfunc) ();
typedef struct _test_t { testfunc fn; const char* nm; } test_t;
#define TST( name ) { at_##name, #name }


extern test_t all_tests[];
const char* testname;


int all_tests_count();
void exec_tests()
{
	int i, count = all_tests_count();
	fclose( fopen( outfile, "w" ) );
	fclose( fopen( outfile_errors, "w" ) );
	
	for( i = 0; i < count; ++i )
	{
		testname = all_tests[ i ].nm;
		printf( "- [%02d] %-20s ...", i+1, all_tests[ i ].nm );
		all_tests[ i ].fn();
		printf( " OK\n" );
	}
}

int
#ifdef _MSC_VER
__cdecl
#endif
main( int argc, char** argv )
{
	printf( "\n//\n/// SGScript [API] test framework\n//\n" );

	exec_tests();

	return 0;
}


/* API */

void atf_abort(){ abort(); }
void atf_error( const char* msg, int line )
{
	printf( "\nERROR at line %d\n%s\n", line, msg );
	atf_abort();
}
void atf_warning( const char* msg, int line )
{
	printf( "\nWARNING at line %d\n%s\n", line, msg );
}

#define atf_defmsg( chk ) "test failed: \"" #chk "\""

#define atf_assert_( chk, msg, line ) do{ if( !(chk) ) atf_error( msg, line ); }while(0)
#define atf_assert( chk ) atf_assert_( chk, atf_defmsg( chk ), __LINE__ )

#define atf_check_( chk, msg, line ) do{ if( !(chk) ) atf_warning( msg, line ); }while(0)
#define atf_check( chk ) atf_check_( chk, atf_defmsg( chk ), __LINE__ )

void atf_assert_string_( const char* str1, const char* str2, const char* msg, int line )
{
	const char* s1 = str1, *s2 = str2;
	if( str1 == NULL || str2 == NULL )
	{
		printf( "\nERROR at line %d\n%s (at least one of the strings is NULL)\n", line, msg );
		atf_abort();
	}
	
	do
	{
		if( *s1 != *s2 )
		{
			const char* spaces = "           ";
			printf( "\nERROR at line %d\n%s (mismatch at pos. %d)\n", line, msg, (int)(size_t)( s1-str1+1 ) );
			printf( "...%-20s...\n", s1 - 10 > str1 ? s1 : str1 );
			printf( "...%-20s...\n", s2 - 10 > str2 ? s2 : str2 );
			printf( "   %.*s^\n", (int)(size_t)( s1 - str1 > 10 ? 10 : s1 - str1 ), spaces );
			atf_abort();
		}
		s1++;
		s2++;
	}
	while( *s1 && *s2 );
}
#define atf_assert_string( str1, str2 ) \
	atf_assert_string_( str1, str2, atf_defmsg( str1 == str2 ), __LINE__ )


/*************\
*             *
*  T E S T S  *
*             *
\*************/


sgs_Context* currctx;
sgs_MemBuf outbuf;
sgs_MemBuf errbuf;
FILE* outfp;
FILE* errfp;
static void outfn_buffer( void* ud, SGS_CTX, const void* ptr, size_t size )
{
	sgs_MemBuf* mb = (sgs_MemBuf*) ud;
	sgs_membuf_appbuf( mb, C, ptr, size );
}
static void msgfn_buffer( void* ud, SGS_CTX, int type, const char* msg )
{
	const char* pfxs[] = { "[I:", "[W:", "[E:" };
	type = type / 100 - 1;
	if( type < 0 ) type = 0;
	if( type > 2 ) type = 2;
	
	sgs_WriteErrorInfo( C, SGS_ERRORINFO_FULL, (sgs_ErrorOutputFunc) sgs_ErrWritef, C, type, msg );
	
	sgs_MemBuf* mb = (sgs_MemBuf*) ud;
	sgs_membuf_appbuf( mb, C, pfxs[type], strlen(pfxs[type]) );
	sgs_membuf_appbuf( mb, C, msg, strlen(msg) );
	sgs_membuf_appbuf( mb, C, "]", 1 );
}
#define REDIR_NONE 0
#define REDIR_FILE 1
#define REDIR_BUF 2
sgs_Context* get_context_( int redir_out )
{
	SGS_CTX = sgs_CreateEngine();
	currctx = C;
	atf_assert_( C, "could not create context (out of memory?)", __LINE__ );
	
	outfp = NULL;
	if( redir_out == REDIR_FILE )
	{
		outfp = fopen( outfile, "a" );
		atf_assert_( outfp, "could not create output file", __LINE__ );
		setvbuf( outfp, NULL, _IONBF, 0 );
		
		sgs_SetOutputFunc( C, SGSOUTPUTFN_DEFAULT, outfp );
		atf_assert( C->shared->output_ctx == outfp );
		
		fprintf( outfp, "//\n/// O U T P U T  o f  %s\n//\n\n", testname );
	}
	else if( redir_out == REDIR_BUF )
	{
		outbuf = sgs_membuf_create();
		sgs_SetOutputFunc( C, outfn_buffer, &outbuf );
		atf_assert( C->shared->output_ctx == &outbuf );
	}
	
	errfp = fopen( outfile_errors, "a" );
	atf_assert_( errfp, "could not create error output file", __LINE__ );
	setvbuf( errfp, NULL, _IONBF, 0 );
	
	sgs_SetErrOutputFunc( C, SGSOUTPUTFN_DEFAULT, errfp );
	atf_assert( C->shared->erroutput_ctx == errfp );
	
	errbuf = sgs_membuf_create();
	sgs_SetMsgFunc( C, msgfn_buffer, &errbuf );
	atf_assert( C->msg_ctx == &errbuf );
	
	return C;
}
sgs_Context* get_context(){ return get_context_( REDIR_FILE ); }
void destroy_context( SGS_CTX )
{
	if( C->shared->output_fn == outfn_buffer )
	{
		sgs_membuf_destroy( &outbuf, C );
	}
	sgs_membuf_destroy( &errbuf, C );
	currctx = NULL;
	sgs_DestroyEngine( C );
	if( outfp )
	{
		fclose( outfp );
		outfp = NULL;
	}
	fclose( errfp );
}

void atf_clear_errors()
{
	sgs_membuf_resize( &errbuf, currctx, 0 );
}
#define atf_check_errors( expect ) atf_check_errors_( expect, __LINE__ )
void atf_check_errors_( const char* expect, int line )
{
	size_t len = strlen( expect );
	if( len != (size_t) errbuf.size || memcmp( errbuf.ptr, expect, len ) != 0 )
	{
		printf( "\nERROR MESSAGE MISMATCH\nexpected: %s\ngot: ", expect );
		fwrite( errbuf.ptr, errbuf.size, 1, stdout );
		printf( "\n" );
		atf_abort();
	}
}

int sgs_dummy_func( SGS_CTX ){ return 0; }
sgs_ObjInterface sgs_dummy_iface[1] =
{{
	"dummy",
	NULL, NULL,
	NULL, NULL,
	NULL, NULL, NULL, NULL,
	NULL, NULL
}};


DEFINE_TEST( create_and_destroy )
{
	SGS_CTX = get_context();
	SGS_SHCTX_USE;
	sgs_Writef( C, "New context memory usage: %d B (%.2f kB)\n", (int) S->memsize, (double) S->memsize / 1024.0 );
	destroy_context( C );
}

DEFINE_TEST( array_mem )
{
	SGS_CTX = get_context();
	SGS_SHCTX_USE;
	sgs_CreateArray( C, NULL, 0 );
	sgs_Writef( C, "context[array] memory usage: %d B (%.2f kB)\n", (int) S->memsize, (double) S->memsize / 1024.0 );
	destroy_context( C );
}

DEFINE_TEST( stack_101 )
{
	sgs_Variable sgs_dummy_var;
	SGS_CTX = get_context();
	
	sgs_dummy_var.type = SGS_VT_NULL;
	
	atf_assert( C->stack_base == C->stack_off );
	atf_assert( C->stack_top == C->stack_off );
	
	sgs_PushNull( C );
	sgs_PushBool( C, 1 );
	sgs_PushInt( C, 1337 );
	sgs_PushReal( C, 13.37 );
	sgs_PushStringBuf( C, "what is this", 7 );
	sgs_PushString( C, "what is this" );
	sgs_PushCFunc( C, sgs_dummy_func );
	sgs_CreateObject( C, NULL, NULL, sgs_dummy_iface );
	sgs_PushVariable( C, sgs_dummy_var );
	
	atf_assert( C->stack_base == C->stack_off );
	atf_assert( sgs_StackSize( C ) == 9 );
	atf_assert( C->stack_top == C->stack_off + 9 );
	
	atf_assert( C->stack_off[0].type == SGS_VT_NULL );
	atf_assert( C->stack_off[1].type == SGS_VT_BOOL );
	atf_assert( C->stack_off[2].type == SGS_VT_INT );
	atf_assert( C->stack_off[3].type == SGS_VT_REAL );
	atf_assert( C->stack_off[4].type == SGS_VT_STRING );
	atf_assert( C->stack_off[5].type == SGS_VT_STRING );
	atf_assert( C->stack_off[6].type == SGS_VT_CFUNC );
	atf_assert( C->stack_off[7].type == SGS_VT_OBJECT );
	atf_assert( C->stack_off[8].type == SGS_VT_NULL );
	
	atf_assert( C->stack_off[1].data.B == 1 );
	atf_assert( C->stack_off[2].data.I == 1337 );
	atf_assert( C->stack_off[3].data.R == 13.37 );
	atf_assert( C->stack_off[4].data.S->size == 7 );
	atf_assert_string( sgs_var_cstr( C->stack_off+4 ), "what is" );
	atf_assert( C->stack_off[5].data.S->size == 12 );
	atf_assert_string( sgs_var_cstr( C->stack_off+5 ), "what is this" );
	atf_assert( C->stack_off[6].data.C == sgs_dummy_func );
	atf_assert( C->stack_off[7].data.O->data == NULL );
	atf_assert( C->stack_off[7].data.O->iface == sgs_dummy_iface );
	
	sgs_Pop( C, 10 );
	atf_assert( sgs_StackSize( C ) == 9 );
	atf_assert( C->stack_top == C->stack_off + 9 );
	
	sgs_Pop( C, 9 );
	atf_assert( C->stack_off == C->stack_top );
	atf_assert( C->stack_base == C->stack_off );
	atf_assert( sgs_StackSize( C ) == 0 );
	
	destroy_context( C );
}

DEFINE_TEST( stack_insert )
{
	sgs_Variable sgs_dummy_var;
	SGS_CTX = get_context();
	
	sgs_dummy_var.type = SGS_VT_NULL;
	
	sgs_PushInt( C, 1 );
	sgs_PushInt( C, 2 );
	atf_assert( C->stack_top == C->stack_off + 2 );
	
	atf_clear_errors();
	sgs_InsertVariable( C, 3, sgs_dummy_var ); atf_assert( sgs_StackSize( C ) == 2 );
	atf_check_errors( "[E:sgs_InsertVariable: invalid index - 3 (stack size = 2)]" );
	
	atf_clear_errors();
	sgs_InsertVariable( C, -4, sgs_dummy_var ); atf_assert( sgs_StackSize( C ) == 2 );
	atf_check_errors( "[E:sgs_InsertVariable: invalid index - -4 (stack size = 2)]" );
	
	atf_clear_errors();
	sgs_InsertVariable( C, -3, sgs_dummy_var ); atf_assert( sgs_StackSize( C ) == 3 );
	atf_check_errors( "" );
	
	atf_clear_errors();
	sgs_InsertVariable( C, 3, sgs_dummy_var ); atf_assert( sgs_StackSize( C ) == 4 );
	atf_check_errors( "" );
	
	atf_clear_errors();
	sgs_InsertVariable( C, 2, sgs_dummy_var ); atf_assert( sgs_StackSize( C ) == 5 );
	atf_check_errors( "" );
	
	atf_assert( C->stack_off[0].type == SGS_VT_NULL );
	atf_assert( C->stack_off[1].type == SGS_VT_INT );
	atf_assert( C->stack_off[2].type == SGS_VT_NULL );
	atf_assert( C->stack_off[3].type == SGS_VT_INT );
	atf_assert( C->stack_off[4].type == SGS_VT_NULL );
	
	sgs_Pop( C, 5 );
	atf_assert( sgs_StackSize( C ) == 0 );
	
	destroy_context( C );
}

DEFINE_TEST( stack_arraydict )
{
	SGS_CTX = get_context();
	
	sgs_PushNull( C );
	sgs_PushString( C, "key-one" );
	sgs_PushInt( C, 5 );
	sgs_PushString( C, "key-two" );
	
	sgs_CreateArray( C, NULL, 5 ); atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_NULL ); sgs_Pop( C, 1 );
	sgs_CreateArray( C, NULL, 0 ); atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_OBJECT ); sgs_Pop( C, 1 );
	
	sgs_PushNull( C );
	sgs_PushString( C, "key-one" );
	sgs_PushInt( C, 5 );
	sgs_PushString( C, "key-two" );
	
	sgs_CreateArray( C, NULL, 4 );
	atf_assert( sgs_StackSize( C ) == 5 );
	
	sgs_CreateDict( C, NULL, 6 ); atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_NULL ); sgs_Pop( C, 1 );
	sgs_CreateDict( C, NULL, 5 ); atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_NULL ); sgs_Pop( C, 1 );
	sgs_CreateDict( C, NULL, 0 ); atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_OBJECT ); sgs_Pop( C, 1 );
	sgs_CreateDict( C, NULL, 4 ); atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_OBJECT );
	
	atf_assert( sgs_StackSize( C ) == 2 );
	sgs_Pop( C, 2 );
	atf_assert( sgs_StackSize( C ) == 0 );
	
	/* string-convertible keys */
	sgs_PushInt( C, 1 );
	sgs_PushInt( C, 2 );
	atf_assert( sgs_StackSize( C ) == 2 );
	sgs_CreateDict( C, NULL, 2 );
	atf_assert( sgs_StackSize( C ) == 1 );
	atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_OBJECT );
	{
		sgs_VHTable* tbl = (sgs_VHTable*) sgs_GetObjectData( C, 0 );
		atf_assert( tbl->vars[0].key.type == SGS_VT_STRING );
	}
	sgs_Pop( C, 1 );
	atf_assert( sgs_StackSize( C ) == 0 );
	
	/* not-string key in dict creation - should be forcefully converted */
	sgs_PushCFunc( C, sgs_dummy_func );
	sgs_PushInt( C, 2 );
	atf_assert( sgs_StackSize( C ) == 2 );
	sgs_CreateDict( C, NULL, 2 );
	atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_OBJECT );
	{
		sgs_VHTable* tbl = (sgs_VHTable*) sgs_GetObjectData( C, 0 );
		atf_assert( tbl->vars[0].key.type == SGS_VT_STRING );
	}
	sgs_Pop( C, 1 );
	atf_assert( sgs_StackSize( C ) == 0 );
	
	destroy_context( C );
}

DEFINE_TEST( stack_push )
{
	SGS_CTX = get_context();
	
	sgs_PushNull( C );
	sgs_PushInt( C, 5 );
	
	atf_clear_errors();
	sgs_PushItem( C, 2 ); atf_assert( sgs_StackSize( C ) == 3 );
	atf_check_errors( "[E:invalid stack index - 2 (abs = 2, stack size = 2)]" );
	sgs_Pop( C, 1 );
	
	atf_clear_errors();
	sgs_PushItem( C, -3 ); atf_assert( sgs_StackSize( C ) == 3 );
	atf_check_errors( "[E:invalid stack index - -3 (abs = -1, stack size = 2)]" );
	sgs_Pop( C, 1 );
	
	atf_clear_errors();
	sgs_PushItem( C, -2 ); atf_assert( sgs_StackSize( C ) == 3 );
	atf_check_errors( "" );
	sgs_Pop( C, 1 );
	
	atf_clear_errors();
	sgs_PushItem( C, 1 ); atf_assert( sgs_StackSize( C ) == 3 );
	atf_check_errors( "" );
	sgs_Pop( C, 1 );
	
	atf_assert( C->stack_off[1].type == SGS_VT_INT );
	atf_assert( C->stack_off[1].data.I == 5 );
	sgs_Pop( C, 2 );
	atf_assert( sgs_StackSize( C ) == 0 );
	
	destroy_context( C );
}

DEFINE_TEST( stack_propindex )
{
	SGS_CTX = get_context();
	
	sgs_PushString( C, "key-one" );
	atf_assert( sgs_PushIndex( C, sgs_StackItem( C, 0 ), sgs_StackItem( C, 0 ), 0 ) == 0 ); sgs_Pop( C, 1 );
	
	sgs_PushInt( C, 15 );
	atf_assert( sgs_PushIndex( C, sgs_StackItem( C, -2 ), sgs_StackItem( C, -1 ), 0 ) == 0 );
	sgs_Pop( C, 2 );
	sgs_PushInt( C, 5 );
	atf_assert( sgs_PushIndex( C, sgs_StackItem( C, -2 ), sgs_StackItem( C, -1 ), 0 ) == 1 );
	sgs_Pop( C, 1 );
	
	sgs_PushString( C, "key-two" );
	sgs_PushNull( C );
	sgs_CreateDict( C, NULL, 4 ); atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_OBJECT );
	atf_assert( sgs_StackSize( C ) == 1 );
	
	destroy_context( C );
}

static sgs_RegIntConst nidxints[] =
{
	{ "test", 42 },
	{ NULL, 0 },
};
DEFINE_TEST( stack_negidx )
{
	SGS_CTX = get_context();
	
	sgs_CreateDict( C, NULL, 0 ); atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_OBJECT );
	
	sgs_StoreIntConsts( C, sgs_StackItem( C, -1 ), nidxints, -1 );
	atf_assert( sgs_PushProperty( C, sgs_StackItem( C, -1 ), "test" ) == 1 );
	atf_assert( sgs_GetInt( C, -1 ) == 42 );
	sgs_Pop( C, 1 );
	atf_assert( sgs_StackSize( C ) == 1 );
	
	destroy_context( C );
}

DEFINE_TEST( indexing )
{
	sgs_Variable var;
	SGS_CTX = get_context();
	
	sgs_CreateDict( C, NULL, 0 );
	atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_OBJECT );
	atf_assert( sgs_StackSize( C ) == 1 );
	sgs_PushString( C, "test" );
	atf_assert( sgs_StackSize( C ) == 2 );
	
	atf_assert( sgs_SetIndex( C, sgs_StackItem( C, 0 ), sgs_StackItem( C, 1 ), sgs_StackItem( C, 1 ), 0 ) == 1 );
	atf_assert( sgs_GetIndex( C, sgs_StackItem( C, 0 ), sgs_StackItem( C, 1 ), &var, 0 ) == 1 );
	atf_assert( var.type == SGS_VT_STRING );
	sgs_Release( C, &var );
	
	destroy_context( C );
}

DEFINE_TEST( globals_101 )
{
	SGS_CTX = get_context();
	
	atf_assert( sgs_PushGlobalByName( C, "array" ) );
	atf_assert( sgs_StackSize( C ) == 1 );
	atf_assert( sgs_PushGlobalByName( C, "donut_remover" ) == 0 );
	atf_assert_( sgs_StackSize( C ) == 2, "wrong stack size after failed PushGlobalByName", __LINE__ );
	
	sgs_CreateArray( C, NULL, 1 );
	atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_OBJECT );
	atf_assert_( sgs_StackSize( C ) == 2, "wrong stack size after CreateArray", __LINE__ );
	sgs_SetGlobalByName( C, "yarra", sgs_StackItem( C, -1 ) );
	atf_assert( sgs_StackSize( C ) == 2 );
	
	destroy_context( C );
}

DEFINE_TEST( libraries )
{
	SGS_CTX = get_context();
	
	sgs_LoadLib_Fmt( C );
	sgs_LoadLib_IO( C );
	sgs_LoadLib_Math( C );
	sgs_LoadLib_OS( C );
	sgs_LoadLib_RE( C );
	sgs_LoadLib_String( C );
	
	atf_assert( sgs_PushGlobalByName( C, "fmt_text" ) );
	atf_assert( sgs_PushGlobalByName( C, "io_file" ) );
	atf_assert( sgs_PushGlobalByName( C, "pow" ) );
	atf_assert( sgs_PushGlobalByName( C, "os_date_string" ) );
	atf_assert( sgs_PushGlobalByName( C, "re_replace" ) );
	atf_assert( sgs_PushGlobalByName( C, "string_replace" ) );
	
	destroy_context( C );
}

DEFINE_TEST( function_calls )
{
	sgs_Variable v_func;
	SGS_CTX = get_context();
	
	atf_assert( sgs_GetGlobalByName( C, "array", &v_func ) );
	atf_clear_errors();
	sgs_Call( C, v_func, 5, 1 );
	sgs_Call( C, v_func, 1, 0 );
	atf_check_errors( "[E:sgs_FCall: not enough items in stack (need: 5, got: 0)]"
		"[E:sgs_FCall: not enough items in stack (need: 1, got: 0)]" );
	atf_clear_errors();
	sgs_Call( C, v_func, 0, 0 );
	sgs_Call( C, v_func, 0, 1 );
	atf_check_errors( "" );
	atf_assert( sgs_StackSize( C ) == 1 );
	atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_OBJECT );
	
	atf_assert( sgs_StackSize( C ) == 1 );
	atf_clear_errors();
	sgs_ThisCall( C, v_func, 1, 0 );
	atf_check_errors( "[E:sgs_FCall: not enough items in stack (need: 2, got: 1)]" );
	atf_clear_errors();
	sgs_ThisCall( C, v_func, 0, 0 );
	atf_check_errors( "" );
	atf_assert( sgs_StackSize( C ) == 0 );
	
	sgs_Release( C, &v_func );
	destroy_context( C );
}

DEFINE_TEST( complex_gc )
{
	sgs_SizeVal objcount;
	SGS_CTX = get_context();
	
	const char* str =
	"o = { a = [ { b = {}, c = 5 }, { d = { e = {} }, f = [] } ], g = [] };"
	"o.a[0].parent = o; o.a[1].d.parent = o; o.a[1].d.e.parent = o.a[1].d;"
	"o.a[1].f.push(o); o.g.push( o.a[1].f ); o.a.push( o.a );";
	
	objcount = (sgs_SizeVal) sgs_Stat( C, SGS_STAT_OBJCOUNT );
	atf_assert( sgs_ExecString( C, str ) == SGS_SUCCESS );
	atf_assert( sgs_Stat( C, SGS_STAT_OBJCOUNT ) > objcount );
	sgs_GCExecute( C );
	atf_assert( sgs_Stat( C, SGS_STAT_OBJCOUNT ) == objcount );
	
	destroy_context( C );
}

void output_strcat( void* ud, SGS_CTX, const void* ptr, size_t size )
{
	char* dst = (char*) ud;
	const char* src = (const char*) ptr;
	
	if( strlen( dst ) + size + 1 > 64 )
	{
		/* WP: good enough for these tests */
		printf( "size: %d, whole string: %s%s\n", (int) size, dst, src );
		fflush( stdout );
	}
	atf_assert( strlen( dst ) + size + 1 <= 64 );
	
	strcat( dst, src );
}
DEFINE_TEST( commands )
{
	char pbuf[ 64 ] = {0};
	
	SGS_CTX = get_context();
	
	const char* str =
	"print('1');print('2','3');print();"
	"print '4'; print '5', '6'; print;";
	
	sgs_SetOutputFunc( C, output_strcat, pbuf );
	atf_assert( sgs_ExecString( C, str ) == SGS_SUCCESS );
	sgs_SetOutputFunc( C, SGSOUTPUTFN_DEFAULT, outfp );
	
	atf_assert_string( pbuf, "123456" );
	
	destroy_context( C );
}

DEFINE_TEST( debugging )
{
	int rvc;
	SGS_CTX = get_context();
	
	sgs_PushNull( C );
	sgs_PushBool( C, 1 );
	sgs_PushInt( C, 1337 );
	sgs_PushReal( C, 3.14 );
	sgs_PushString( C, "wat" );
	sgs_EvalString( C, "return function(){};", &rvc );
	atf_assert( sgs_PushGlobalByName( C, "print" ) );
	sgs_CreateArray( C, NULL, 0 );
	sgs_CreateDict( C, NULL, 0 );
	
	atf_assert( sgs_StackSize( C ) == 9 );
	sgs_Stat( C, SGS_STAT_DUMP_STACK );
	sgs_Stat( C, SGS_STAT_DUMP_GLOBALS );
	sgs_Stat( C, SGS_STAT_DUMP_OBJECTS );
	sgs_Stat( C, SGS_STAT_DUMP_FRAMES );
	sgs_Stat( C, SGS_STAT_DUMP_SYMBOLS );
	
	destroy_context( C );
}

DEFINE_TEST( varpaths )
{
	SGS_CTX = get_context();
	
	const char* str =
	"global o = { a = [ { b = {}, c = 5 }, { d = { e = {} }, f = [] } ], g = [] };"
	"o.a[0].parent = o; o.a[1].d.parent = o; o.a[1].d.e.parent = o.a[1].d;"
	"o.a[1].f.push(o); o.g.push( o.a[1].f ); o.a.push( o.a );";
	
	atf_assert( sgs_ExecString( C, str ) == SGS_SUCCESS );
	atf_assert( sgs_PushGlobalByName( C, "o" ) );
	
	/* basic property retrieval */
	atf_assert( sgs_PushPath( C, sgs_StackItem( C, -1 ), "p", "g" ) == SGS_TRUE );
	atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_OBJECT );
	atf_assert( sgs_IsObject( C, -1, sgsstd_array_iface ) );
	sgs_Pop( C, 1 );
	atf_assert( sgs_StackSize( C ) == 1 );
	
	/* properties and indices 1 */
	atf_assert( sgs_PushPath( C, sgs_StackItem( C, -1 ), "pi", "a", 1 ) == SGS_TRUE );
	atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_OBJECT );
	atf_assert( sgs_IsObject( C, -1, sgsstd_dict_iface ) );
	sgs_Pop( C, 1 );
	atf_assert( sgs_StackSize( C ) == 1 );
	
	/* properties and indices 2 */
	atf_assert( sgs_PushPath( C, sgs_StackItem( C, -1 ), "o", 0 ) == SGS_TRUE );
	atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_OBJECT );
	atf_assert( sgs_IsObject( C, -1, sgsstd_array_iface ) );
	sgs_Pop( C, 1 );
	atf_assert( sgs_StackSize( C ) == 1 );
	
	/* properties and indices 3 */
	atf_assert( sgs_PushPath( C, sgs_StackItem( C, -1 ), "piso", "a", 1, 1, "d", 0 ) == SGS_TRUE );
	atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_OBJECT );
	atf_assert( sgs_IsObject( C, -1, sgsstd_dict_iface ) );
	sgs_Pop( C, 1 );
	atf_assert( sgs_StackSize( C ) == 1 );
	
	/* storing data */
	sgs_PushInt( C, 42 );
	atf_assert( sgs_StorePath( C, sgs_StackItem( C, -2 ), sgs_StackItem( C, -1 ), "pippp", "a", 1, "d", "e", "test" ) == SGS_TRUE );
	atf_assert( sgs_PushPath( C, sgs_StackItem( C, -1 ), "pippp", "a", 1, "d", "e", "test" ) == SGS_TRUE );
	atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_INT );
	atf_assert( (C->stack_top-1)->data.I == 42 );
	sgs_Pop( C, 1 );
	atf_assert( sgs_StackSize( C ) == 1 );
	
	destroy_context( C );
}

DEFINE_TEST( iterators )
{
	SGS_CTX = get_context();
	
	/* ARRAY ITERATOR */
	sgs_PushBool( C, 1 );
	sgs_PushInt( C, 42 );
	sgs_PushString( C, "wat" );
	
	atf_assert( sgs_StackSize( C ) == 3 );
	sgs_CreateArray( C, NULL, 3 );
	atf_assert( sgs_StackSize( C ) == 1 );
	
	/* test iteration */
	atf_assert( sgs_PushIterator( C, sgs_StackItem( C, 0 ) ) == 1 );
	atf_assert( (C->stack_top-1)->type == SGS_VT_OBJECT );
	atf_assert( (C->stack_top-1)->data.O->iface == sgsstd_array_iter_iface );
	for(;;)
	{
		int ret = sgs_IterAdvance( C, sgs_StackItem( C, -1 ) );
		atf_assert( SGS_SUCCEEDED( ret ) );
		if( !ret )
			break;
	}
	sgs_Pop( C, 1 );
	
	/* test keys & values */
	atf_assert( sgs_PushIterator( C, sgs_StackItem( C, 0 ) ) == 1 );
	atf_assert( (C->stack_top-1)->type == SGS_VT_OBJECT );
	atf_assert( (C->stack_top-1)->data.O->iface == sgsstd_array_iter_iface );
	/* - values 1 */
	atf_assert( sgs_IterAdvance( C, sgs_StackItem( C, -1 ) ) == 1 );
	sgs_IterPushData( C, sgs_StackItem( C, -1 ), 1, 1 ); atf_check_errors( "" );
	atf_assert( sgs_ItemType( C, -2 ) == SGS_VT_INT );
	atf_assert( sgs_GetInt( C, -2 ) == 0 );
	atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_BOOL );
	atf_assert( sgs_GetBool( C, -1 ) == 1 );
	sgs_Pop( C, 2 );
	/* - values 2 */
	atf_assert( sgs_IterAdvance( C, sgs_StackItem( C, -1 ) ) == 1 );
	sgs_IterPushData( C, sgs_StackItem( C, -1 ), 1, 1 ); atf_check_errors( "" );
	atf_assert( sgs_ItemType( C, -2 ) == SGS_VT_INT );
	atf_assert( sgs_GetInt( C, -2 ) == 1 );
	atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_INT );
	atf_assert( sgs_GetInt( C, -1 ) == 42 );
	sgs_Pop( C, 2 );
	/* - values 3 */
	atf_assert( sgs_IterAdvance( C, sgs_StackItem( C, -1 ) ) == 1 );
	sgs_IterPushData( C, sgs_StackItem( C, -1 ), 1, 1 ); atf_check_errors( "" );
	atf_assert( sgs_ItemType( C, -2 ) == SGS_VT_INT );
	atf_assert( sgs_GetInt( C, -2 ) == 2 );
	atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_STRING );
	atf_assert( sgs_GetStringSize( C, -1 ) == 3 );
	atf_assert( strcmp( sgs_GetStringPtr( C, -1 ), "wat" ) == 0 );
	sgs_Pop( C, 2 );
	/* - end */
	atf_assert( sgs_IterAdvance( C, sgs_StackItem( C, -1 ) ) == 0 );
	
	/* clean up */
	sgs_Pop( C, 2 );
	atf_assert( sgs_StackSize( C ) == 0 );
	
	/* DICT ITERATOR */
	sgs_PushString( C, "1st" );
	sgs_PushBool( C, 1 );
	sgs_PushString( C, "2nd" );
	sgs_PushInt( C, 42 );
	sgs_PushString( C, "3rd" );
	sgs_PushString( C, "wat" );
	
	atf_assert( sgs_StackSize( C ) == 6 );
	sgs_CreateDict( C, NULL, 6 );
	atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_OBJECT );
	atf_assert( sgs_StackSize( C ) == 1 );
	
	/* test iteration */
	atf_assert( sgs_PushIterator( C, sgs_StackItem( C, 0 ) ) == 1 );
	atf_assert( (C->stack_top-1)->type == SGS_VT_OBJECT );
	atf_assert( (C->stack_top-1)->data.O->iface == sgsstd_dict_iter_iface );
	for(;;)
	{
		int ret = sgs_IterAdvance( C, sgs_StackItem( C, -1 ) );
		atf_assert( SGS_SUCCEEDED( ret ) );
		if( !ret )
			break;
	}
	sgs_Pop( C, 1 );
	
	/* test keys & values */
	atf_assert( sgs_PushIterator( C, sgs_StackItem( C, 0 ) ) == 1 );
	atf_assert( (C->stack_top-1)->type == SGS_VT_OBJECT );
	atf_assert( (C->stack_top-1)->data.O->iface == sgsstd_dict_iter_iface );
	/* - values 1 */
	atf_assert( sgs_IterAdvance( C, sgs_StackItem( C, -1 ) ) == 1 );
	sgs_IterPushData( C, sgs_StackItem( C, -1 ), 1, 1 ); atf_check_errors( "" );
	atf_assert( sgs_ItemType( C, -2 ) == SGS_VT_STRING );
	atf_assert( sgs_GetStringSize( C, -2 ) == 3 );
	atf_assert( strcmp( sgs_GetStringPtr( C, -2 ), "1st" ) == 0 );
	atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_BOOL );
	atf_assert( sgs_GetBool( C, -1 ) == 1 );
	sgs_Pop( C, 2 );
	/* - values 2 */
	atf_assert( sgs_IterAdvance( C, sgs_StackItem( C, -1 ) ) == 1 );
	sgs_IterPushData( C, sgs_StackItem( C, -1 ), 1, 1 ); atf_check_errors( "" );
	atf_assert( sgs_ItemType( C, -2 ) == SGS_VT_STRING );
	atf_assert( sgs_GetStringSize( C, -2 ) == 3 );
	atf_assert( strcmp( sgs_GetStringPtr( C, -2 ), "2nd" ) == 0 );
	atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_INT );
	atf_assert( sgs_GetInt( C, -1 ) == 42 );
	sgs_Pop( C, 2 );
	/* - values 3 */
	atf_assert( sgs_IterAdvance( C, sgs_StackItem( C, -1 ) ) == 1 );
	sgs_IterPushData( C, sgs_StackItem( C, -1 ), 1, 1 ); atf_check_errors( "" );
	atf_assert( sgs_ItemType( C, -2 ) == SGS_VT_STRING );
	atf_assert( sgs_GetStringSize( C, -2 ) == 3 );
	atf_assert( strcmp( sgs_GetStringPtr( C, -2 ), "3rd" ) == 0 );
	atf_assert( sgs_ItemType( C, -1 ) == SGS_VT_STRING );
	atf_assert( sgs_GetStringSize( C, -1 ) == 3 );
	atf_assert( strcmp( sgs_GetStringPtr( C, -1 ), "wat" ) == 0 );
	sgs_Pop( C, 2 );
	/* - end */
	atf_assert( sgs_IterAdvance( C, sgs_StackItem( C, -1 ) ) == 0 );
	
	/* clean up */
	sgs_Pop( C, 2 );
	atf_assert( sgs_StackSize( C ) == 0 );
	
	destroy_context( C );
}


int nom_method_A( SGS_CTX )
{
	SGSFN( "nom::method_A" );
	sgs_PushInt( C, 5 );
	if( sgs_Method( C ) )
	{
		sgs_SetProperty( C, sgs_StackItem( C, 0 ), "a", sgs_StackItem( C, -1 ) );
	}
	return 1;
}

int nom_method_B( SGS_CTX )
{
	SGSFN( "nom::method_B" );
	sgs_PushInt( C, 7 );
	if( sgs_Method( C ) )
	{
		sgs_SetProperty( C, sgs_StackItem( C, 0 ), "b", sgs_StackItem( C, -1 ) );
	}
	return 1;
}

int nom_iface( SGS_CTX );
int nom_ctor( SGS_CTX )
{
	SGSFN( "nom::nom" );
	sgs_PushInterface( C, nom_iface );
	sgs_CreateDict( C, NULL, 0 );
	sgs_ObjSetMetaObj( C, sgs_GetObjectStruct( C, -1 ), sgs_GetObjectStruct( C, -2 ) );
	return 1;
}

static sgs_RegFuncConst nom_funcs[] =
{
	{ "method_A", nom_method_A },
	{ "method_B", nom_method_B },
	{ "__call", nom_ctor },
	{ NULL, NULL },
};

int nom_iface( SGS_CTX )
{
	SGSFN( "nom_iface" );
	sgs_CreateDict( C, NULL, 0 );
	sgs_StoreFuncConsts( C, sgs_StackItem( C, -1 ), nom_funcs, -1, "nom." );
	atf_assert( sgs_StackSize( C ) == 1 );
	sgs_ObjSetMetaMethodEnable( sgs_GetObjectStruct( C, -1 ), 1 );
	return 1;
}

DEFINE_TEST( native_obj_meta )
{
	SGS_CTX = get_context();
	
	/* register as ctor/iface */
	sgs_PushInterface( C, nom_iface );
	sgs_SetGlobalByName( C, "nom", sgs_StackItem( C, -1 ) );
	
	/* try creating and running the object */
	atf_assert( sgs_ExecString( C, ""
		"a = nom();\n"
		"printvar(a.method_A());\n"
		"printvar(a.method_B());\n"
		"function nom.method_C(){ this.c = 9; return 9; }\n"
		"printvar(a.method_C());\n"
		"printvar(a);\n"
	"" ) == SGS_SUCCESS );
	
	destroy_context( C );
}

DEFINE_TEST( fork_state )
{
	SGS_CTX, *CFF, *CFP;
	
	/* --- basic usage --- */
	C = get_context();
	
	/* fork the state */
	CFF = sgs_ForkState( C, 1 );
	CFP = sgs_ForkState( C, 0 );
	atf_assert( CFF != C );
	atf_assert( CFP != C );
	
	/* root context */
	atf_assert( sgs_RootContext( NULL ) == NULL );
	atf_assert( sgs_RootContext( C ) == C );
	atf_assert( sgs_RootContext( CFF ) == C );
	atf_assert( sgs_RootContext( CFP ) == C );
	
	/* check state count */
	atf_assert( sgs_Stat( C, SGS_STAT_STATECOUNT ) == 3 );
	atf_assert( sgs_Stat( CFF, SGS_STAT_STATECOUNT ) == 3 );
	atf_assert( sgs_Stat( CFP, SGS_STAT_STATECOUNT ) == 3 );
	
	/* release in the creating order (except root) */
	sgs_ReleaseState( CFF );
	atf_assert( sgs_Stat( CFP, SGS_STAT_STATECOUNT ) == 2 );
	sgs_ReleaseState( CFP );
	atf_assert( sgs_Stat( C, SGS_STAT_STATECOUNT ) == 1 );
	sgs_ReleaseState( C );
	
	/* --- try running something on both --- */
	C = get_context();
	CFF = sgs_ForkState( C, 1 );
	CFP = sgs_ForkState( C, 0 );
	const char* str =
	"global o = { a = [ { b = {}, c = 5 }, { d = { e = {} }, f = [] } ], g = [] };"
	"o.a[0].parent = o; o.a[1].d.parent = o; o.a[1].d.e.parent = o.a[1].d;"
	"o.a[1].f.push(o); o.g.push( o.a[1].f ); o.a.push( o.a );";
	atf_assert( sgs_ExecString( C, str ) == SGS_SUCCESS );
	atf_assert( sgs_ExecString( CFF, str ) == SGS_SUCCESS );
	atf_assert( sgs_ExecString( CFP, str ) == SGS_SUCCESS );
	sgs_ReleaseState( CFF );
	sgs_ReleaseState( CFP );
	sgs_ReleaseState( C );
}

DEFINE_TEST( yield_resume )
{
	SGS_CTX;
	
	C = get_context();
	
	atf_assert( sgs_ExecString( C, ""
		"global m0 = println('one') || true;\n"
		"yield();\n"
		"global m1 = println('two') || true;\n"
	"" ) == SGS_SUCCESS );
	
	// check if paused
	atf_assert( C->sf_last && SGS_HAS_FLAG( C->sf_last->flags, SGS_SF_PAUSED ) );
	atf_assert( sgs_GlobalBool( C, "m0" ) == SGS_TRUE );
	atf_assert( sgs_GlobalBool( C, "m1" ) == SGS_FALSE );
	
	// resume
	atf_assert( sgs_ResumeState( C ) == SGS_TRUE );
	// check if done
	atf_assert( C->sf_last == NULL );
	atf_assert( sgs_GlobalBool( C, "m1" ) == SGS_TRUE );
	
	destroy_context( C );
	
	// advanced test
	C = get_context();
	
	atf_assert( C->sf_count == 0 );
	atf_assert( sgs_ExecString( C, ""
		"function testfn(){ println('test'); return 1; }\n"
		"global m0 = println('one') || true;\n"
		"yield();\n"
		"global m1 = println('two') || true;\n"
	"" ) == SGS_SUCCESS );
	
	// check if paused
	atf_assert( C->sf_last && SGS_HAS_FLAG( C->sf_last->flags, SGS_SF_PAUSED ) );
	atf_assert( sgs_GlobalBool( C, "m0" ) == SGS_TRUE );
	atf_assert( sgs_GlobalBool( C, "m1" ) == SGS_FALSE );
	
	// call the test function
	{
		int ssz = sgs_StackSize( C ), sfc = C->sf_count;
		sgs_GlobalCall( C, "testfn", 0, 0 );
		atf_assert( sfc == C->sf_count );
		atf_assert( ssz == sgs_StackSize( C ) );
	}
	
	// resume
	atf_assert( sgs_ResumeState( C ) == SGS_TRUE );
	// check if done
	atf_assert( C->sf_last == NULL );
	atf_assert( sgs_GlobalBool( C, "m1" ) == SGS_TRUE );
	
	destroy_context( C );
}

DEFINE_TEST( yield_abandon )
{
	SGS_CTX = get_context(), *CF;
	
	CF = sgs_ForkState( C, 0 );
	const char* str = "yield();";
	atf_assert( sgs_ExecString( CF, str ) == SGS_SUCCESS );
	sgs_ReleaseState( CF );
	
	destroy_context( C );
	
	C = get_context();
	atf_assert( sgs_ExecString( C, "thread (function(){yield();})();" ) == SGS_SUCCESS );
	atf_assert( sgs_Stat( C, SGS_STAT_STATECOUNT ) == 2 );
	destroy_context( C );
	
	C = get_context();
	atf_assert( sgs_ExecString( C, ""
		"thread (function(){"
		"	subthread (function(){yield();})();"
		"	yield();})();"
		"" ) == SGS_SUCCESS );
	atf_assert( sgs_Stat( C, SGS_STAT_STATECOUNT ) == 3 );
	destroy_context( C );
}

int sm_tick_id = 0;
int sm_resume_id = 0;
static int sm_wait( SGS_CTX )
{
	SGSFN( "sm_wait" );
	sm_resume_id = sm_tick_id + (int) sgs_GetInt( C, 0 );
	fprintf( outfp, "paused on tick %d until %d\n", sm_tick_id, sm_resume_id );
	atf_assert( sgs_PauseState( C ) );
	return 1;
}
DEFINE_TEST( state_machine_core )
{
	SGS_CTX = get_context();
	
	sgs_RegFuncConst fns[] = {{ "wait", sm_wait }};
	sgs_RegFuncConsts( C, fns, 1 );
	
	sm_tick_id = 0;
	sm_resume_id = 0;
	int rvc = 0;
	atf_assert( sgs_EvalString( C, ""
		"println('a');\n"
		"wait(15);\n"
		"println('b');\n"
		"wait(10);\n"
		"println('c');\n"
		"(function test(){\n"
			"wait(15);\n"
			"println('d');\n"
			"wait(10);\n"
			"println('e');\n"
		"})();\n"
	"", &rvc ) == SGS_SUCCESS );
	atf_assert( rvc == 1 );
	fprintf( outfp, "[EvalString value-returned %d]\n", (int) sgs_GetInt( C, -1 ) );
	sgs_Pop( C, 1 );
	
	while( sgs_Cntl( C, SGS_CNTL_GET_PAUSED, 0 ) )
	{
		while( sm_tick_id < sm_resume_id )
			sm_tick_id++;
		fprintf( outfp, "resuming on tick %d\n", sm_tick_id );
		atf_assert( sgs_ResumeStateExp( C, 0, 1 ) == SGS_TRUE );
		fprintf( outfp, "[ResumeStateExp value-returned %d]\n", (int) sgs_GetInt( C, -1 ) );
		sgs_Pop( C, 1 );
	}
	
	destroy_context( C );
}

static const char* _str_after( const char* str, const char* substr, size_t subsz )
{
	const char* at = strstr( str, substr );
	if( at )
		return at + subsz;
	else
		return "";
}
#define STR_AFTER( str, substr ) _str_after( str, substr, sizeof(substr) - 1 )

DEFINE_TEST( profiling )
{
	sgs_Prof P;
	SGS_CTX = get_context_( REDIR_BUF );
	
	/*****************|
	|   M O D E   1   |  [time per call stack]
	-----------------*/
	/* basic profiling */
	sgs_ProfInit( C, &P, SGS_PROF_FUNCTIME );
	atf_assert( sgs_ExecString( C, ""
		"rand();\n"
		"rand();\n"
		"rand();\n"
		"for( i = 0; i < 100; ++i ){\n"
			"(function test(){\n"
				"rand();\n"
				"rand();\n"
			"})();\n"
		"}\n"
	"" ) == SGS_SUCCESS );
	/* expecting to see main/test/rand in the dump */
	sgs_ProfDump( C, &P );
	sgs_membuf_appchr( &outbuf, C, '\0' ); /* make buffer into a C-string */
	/* puts( outbuf.ptr ); //*/
	atf_assert( strstr( outbuf.ptr, "Time by call stack frame" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main> -" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main>::rand -" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main>::test -" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main>::test::rand -" ) != NULL );
	atf_assert( atof( STR_AFTER( outbuf.ptr, "<main> - " ) ) < 0.2f ); /* verify for the next test */
	sgs_ProfClose( C, &P );
	sgs_membuf_resize( &outbuf, C, 0 ); /* clear the buffer */
	
	/* paused function */
	sgs_ProfInit( C, &P, SGS_PROF_FUNCTIME );
	atf_assert( sgs_ExecString( C, ""
		"rand();\n"
		"(function test1(){\n"
			"for( i = 0; i < 10000; ++i ) _G.tst = 1;\n"
			"(function test2(){\n"
				"yield();\n"
				"for( i = 0; i < 10000; ++i ) _G.tst = 2;\n"
			"})();\n"
		"})();\n"
		"randf();\n"
	"" ) == SGS_SUCCESS );
	sgsthread_sleep( 500 ); /* wait 0.5s */
	atf_assert( sgs_ResumeStateExp( C, 0, 0 ) == SGS_TRUE );
	/* expecting to see main/rand/randf in the dump */
	sgs_ProfDump( C, &P );
	sgs_membuf_appchr( &outbuf, C, '\0' ); /* make buffer into a C-string */
	/* puts( outbuf.ptr ); //*/
	atf_assert( strstr( outbuf.ptr, "Time by call stack frame" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main> -" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main>::rand -" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main>::test1 -" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main>::test1::test2 -" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main>::test1::test2::yield -" ) != NULL );
	atf_assert(
		atof( STR_AFTER( outbuf.ptr, "<main> - " ) ) >=
		atof( STR_AFTER( outbuf.ptr, "<main>::test1 - " ) )
	);
	atf_assert(
		atof( STR_AFTER( outbuf.ptr, "<main>::test1 - " ) ) >=
		atof( STR_AFTER( outbuf.ptr, "<main>::test1::test2 - " ) )
	);
	atf_assert( strstr( outbuf.ptr, "<main>::randf -" ) != NULL );
	/* sleep should not affect the profile */
	atf_assert( atof( strstr( outbuf.ptr, "<main> - " ) + 9 ) < 0.5f );
	sgs_ProfClose( C, &P );
	sgs_membuf_resize( &outbuf, C, 0 ); /* clear the buffer */
	
	/* aborted function */
	sgs_ProfInit( C, &P, SGS_PROF_FUNCTIME );
	atf_assert( sgs_ExecString( C, ""
		"rand();\n"
		"abort();\n"
		"randf();\n"
	"" ) == SGS_SUCCESS );
	/* expecting to see main/rand in the dump, but no randf */
	sgs_ProfDump( C, &P );
	sgs_membuf_appchr( &outbuf, C, '\0' ); /* make buffer into a C-string */
	/* puts( outbuf.ptr ); //*/
	atf_assert( strstr( outbuf.ptr, "Time by call stack frame" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main> -" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main>::rand -" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main>::abort -" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main>::randf -" ) == NULL );
	sgs_ProfClose( C, &P );
	sgs_membuf_resize( &outbuf, C, 0 ); /* clear the buffer */
	
	/* timed coroutine */
	sgs_ProfInit( C, &P, SGS_PROF_FUNCTIME );
	atf_assert( sgs_ExecString( C, ""
		"co = co_create(function testfun(){\n"
		"	yield 0.01;\n"
		"	(function in1(){\n"
		"		for( i = 0; i < 1000; ++i ) _G.tst = 1;\n"
		"		yield 0.5;\n"
		"	})();\n"
		"	yield 0.01;\n"
		"});\n"
		"while( co.can_resume ){\n"
		"	resumeat = toreal( co_resume( co ) );\n"
		"	resumeat += ftime();\n"
		"	while( ftime() < resumeat ) ;\n"
		"}\n"
	"" ) == SGS_SUCCESS );
	sgs_ProfDump( C, &P );
	sgs_membuf_appchr( &outbuf, C, '\0' ); /* make buffer into a C-string */
	/* puts( outbuf.ptr ); //*/
	atf_assert( atof( STR_AFTER( outbuf.ptr, "<main>::co_resume - " ) ) < 0.52f );
	atf_assert( atof( STR_AFTER( outbuf.ptr, "testfun - " ) ) < 0.52f );
	atf_assert( atof( STR_AFTER( outbuf.ptr, "testfun::in1 - " ) ) < 0.52f );
	atf_assert( atof( STR_AFTER( outbuf.ptr, "<main> - " ) ) >= 0.52f );
	atf_assert( atof( STR_AFTER( outbuf.ptr, "<main> - " ) ) >= 0.52f );
	sgs_ProfClose( C, &P );
	sgs_membuf_resize( &outbuf, C, 0 ); /* clear the buffer */
	
	/*****************|
	|   M O D E   2   |  [time/count per VM instruction]
	-----------------*/
	/* basic profiling */
	sgs_ProfInit( C, &P, SGS_PROF_OPTIME );
	atf_assert( sgs_ExecString( C, ""
		"rand();\n"
		"rand();\n"
		"rand();\n"
		"for( i = 0; i < 100; ++i ){\n"
			"(function test(){\n"
				"rand();\n"
				"rand();\n"
			"})();\n"
		"}\n"
	"" ) == SGS_SUCCESS );
	sgs_ProfDump( C, &P );
	sgs_membuf_appchr( &outbuf, C, '\0' ); /* make buffer into a C-string */
	/* puts( outbuf.ptr ); //*/
	atf_assert( strstr( outbuf.ptr, "Time by VM instruction" ) != NULL );
	{
		char* firsttime = strstr( outbuf.ptr, "." ) - 1;
		atf_assert( *(firsttime-1) == ' ' && atof( firsttime ) < 0.1f ); /* first entry should not exceed 0.1f */
	}
	sgs_ProfClose( C, &P );
	sgs_membuf_resize( &outbuf, C, 0 ); /* clear the buffer */
	
	/* paused function */
	sgs_ProfInit( C, &P, SGS_PROF_OPTIME );
	atf_assert( sgs_ExecString( C, ""
		"rand();\n"
		"yield();\n"
		"randf();\n"
	"" ) == SGS_SUCCESS );
	sgsthread_sleep( 500 ); /* wait 0.5s */
	atf_assert( sgs_ResumeStateExp( C, 0, 0 ) == SGS_TRUE );
	sgs_ProfDump( C, &P );
	sgs_membuf_appchr( &outbuf, C, '\0' ); /* make buffer into a C-string */
	/* puts( outbuf.ptr ); //*/
	atf_assert( strstr( outbuf.ptr, "Time by VM instruction" ) != NULL );
	{
		char* firsttime = strstr( outbuf.ptr, "." ) - 1;
		atf_assert( *(firsttime-1) == ' ' && atof( firsttime ) < 0.1f ); /* first entry should not exceed 0.1f */
	}
	sgs_ProfClose( C, &P );
	sgs_membuf_resize( &outbuf, C, 0 ); /* clear the buffer */
	
	/*****************|
	|   M O D E   3   |  [memory usage per call stack]
	-----------------*/
	/* basic profiling */
	sgs_ProfInit( C, &P, SGS_PROF_MEMUSAGE );
	atf_assert( sgs_ExecString( C, ""
		"rand();\n"
		"rand();\n"
		"rand();\n"
		"for( i = 0; i < 100; ++i ){\n"
			"(function test(){\n"
				"rand();\n"
				"rand();\n"
			"})();\n"
		"}\n"
	"" ) == SGS_SUCCESS );
	/* expecting to see main/test/rand in the dump */
	sgs_ProfDump( C, &P );
	sgs_membuf_appchr( &outbuf, C, '\0' ); /* make buffer into a C-string */
	/* puts( outbuf.ptr ); //*/
	atf_assert( strstr( outbuf.ptr, "Memory usage by call stack frame" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main> -" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main>::rand -" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main>::test -" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main>::test::rand -" ) != NULL );
	{
		char* firsttime = strstr( outbuf.ptr, "." ) - 1;
		atf_assert( *(firsttime-1) == ' ' && atof( firsttime ) < 0.51f ); /* first entry should not exceed 0.51 (64-bit) */
	}
	sgs_ProfClose( C, &P );
	sgs_membuf_resize( &outbuf, C, 0 ); /* clear the buffer */
	
	/* aborted function */
	sgs_ProfInit( C, &P, SGS_PROF_MEMUSAGE );
	atf_assert( sgs_ExecString( C, ""
		"rand();\n"
		"abort();\n"
		"randf();\n"
	"" ) == SGS_SUCCESS );
	/* expecting to see main/rand in the dump, but no randf */
	sgs_ProfDump( C, &P );
	sgs_membuf_appchr( &outbuf, C, '\0' ); /* make buffer into a C-string */
	/* puts( outbuf.ptr ); //*/
	atf_assert( strstr( outbuf.ptr, "Memory usage by call stack frame" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main> -" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main>::rand -" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main>::abort -" ) != NULL );
	atf_assert( strstr( outbuf.ptr, "<main>::randf -" ) == NULL );
	{
		char* firsttime = strstr( outbuf.ptr, "." ) - 1;
		atf_assert( *(firsttime-1) == ' ' && atof( firsttime ) < 0.5f ); /* first entry should not exceed 0.5f */
	}
	sgs_ProfClose( C, &P );
	sgs_membuf_resize( &outbuf, C, 0 ); /* clear the buffer */
	
	destroy_context( C );
}


test_t all_tests[] =
{
	TST( create_and_destroy ),
	TST( array_mem ),
	TST( stack_101 ),
	TST( stack_insert ),
	TST( stack_arraydict ),
	TST( stack_push ),
	TST( stack_propindex ),
	TST( stack_negidx ),
	TST( indexing ),
	TST( globals_101 ),
	TST( libraries ),
	TST( function_calls ),
	TST( complex_gc ),
	TST( commands ),
	TST( debugging ),
	TST( varpaths ),
	TST( iterators ),
	TST( native_obj_meta ),
	TST( fork_state ),
	TST( yield_resume ),
	TST( yield_abandon ),
	TST( state_machine_core ),
	TST( profiling ),
};
int all_tests_count(){ return sizeof(all_tests)/sizeof(test_t); }

