
#include <stdio.h>
#include <time.h>

#define SGS_USE_FILESYSTEM

#include "sgscript.h"
#include "sgs_int.h"


const char* outfile = "tests-output.log";
const char* outfile_errors = "tests-errors.log";



static int sgrx_snprintf( char* buf, size_t len, const char* fmt, ... )
{
	if( len == 0 )
		return -1;
	va_list args;
	va_start( args, fmt );
	int ret = vsnprintf( buf, len, fmt, args );
	va_end( args );
	buf[ len - 1 ] = 0;
	return ret;
}



typedef struct testfile_
{
	char* nameonly;
	char* fullname;
	int sucfail;
	int loadtf;
	int sortkey;
}
testfile;


int tf_compare( const void* p1, const void* p2 )
{
	const testfile* f1 = (const testfile*) p1;
	const testfile* f2 = (const testfile*) p2;
	if( f1->sortkey != f2->sortkey )
		return f1->sortkey - f2->sortkey;
	return strcmp( f1->nameonly, f2->nameonly );
}

int load_testfiles( const char* dir, testfile** files, size_t* count )
{
	DIR* d;
	struct dirent* e;
	struct stat sdata;
	char namebuf[ 260 ];
	testfile* TF;
	size_t TFM = 32, TFC = 0;
	d = opendir( dir );
	if( !d )
		return 0;
	TF = (testfile*) malloc( sizeof( testfile ) * TFM );

	while( ( e = readdir( d ) ) != NULL )
	{
		int disp = 0;
		if( strcmp( e->d_name, "." ) == 0 || strcmp( e->d_name, ".." ) == 0 )
			continue;
		if( strncmp( e->d_name, "!_", 2 ) == 0 ) continue;
		if( strncmp( e->d_name, "s_", 2 ) == 0 ) disp = 1;
		if( strncmp( e->d_name, "f_", 2 ) == 0 ) disp = -1;
		if( strstr( e->d_name, ".sgs" ) != e->d_name + strlen( e->d_name ) - 4 )
			continue;
		
		sgrx_snprintf( namebuf, 260, "%s/%s", dir, e->d_name );
		stat( namebuf, &sdata );
		if( !( sdata.st_mode & S_IFREG ) )
			continue;
		
		if( TFC == TFM )
		{
			TFM *= 2;
			TF = (testfile*) realloc( TF, sizeof( testfile ) * TFM );
		}
		
		TF[ TFC ].nameonly = (char*) malloc( strlen( e->d_name ) + 1 );
		strcpy( TF[ TFC ].nameonly, e->d_name );
		TF[ TFC ].fullname = (char*) malloc( strlen( namebuf ) + 1 );
		strcpy( TF[ TFC ].fullname, namebuf );
		TF[ TFC ].sucfail = disp;
		TF[ TFC ].loadtf = strstr( e->d_name, "TF" ) != NULL;
		if( TF[ TFC ].loadtf )
			TF[ TFC ].sucfail = 1;
		TF[ TFC ].sortkey = ( disp != 1 ) * 2 + ( disp != -1 ) * 1 + TF[ TFC ].loadtf * 4;
		TFC++;
	}
	closedir( d );

	qsort( TF, TFC, sizeof( testfile ), tf_compare );

	*files = TF;
	*count = TFC;
	return 1;
}

void free_testfiles( testfile* files, size_t count )
{
	testfile* f = files, *fend = files + count;
	while( f < fend )
	{
		free( f->nameonly );
		free( f->fullname );
		f++;
	}
	free( files );
}




static void TF_printfn( void* ctx, SGS_CTX, int type, const char* message )
{
	const char* pfxs[] = { "[I:", "[W:", "[E:" };
	type = type / 100 - 1;
	if( type < 0 ) type = 0;
	if( type > 2 ) type = 2;
	sgs_PushGlobalByName( C, "ERRORS" );
	sgs_PushString( C, pfxs[ type ] );
	sgs_PushString( C, message );
	sgs_PushString( C, "]" );
	sgs_StringConcat( C, 4 );
	sgs_SetGlobalByName( C, "ERRORS", sgs_StackItem( C, -1 ) );
	sgs_Pop( C, 1 );
}

static void prepengine( sgs_Context* C )
{
	int ret;
	const char* sgs_testapi =
	"global ERRORS = \"..uninitialized..\";\n"
	"global tests_failed = 0, tests_ran = 0;\n"
	"function test( result, name, onfail ){\n"
	"	global tests_failed, tests_ran;\n"
	"	tests_ran++;\n"
	"	if( result ){\n"
	"		print( \"OK   `\", name, \"`\\n\" );\n"
	"	}else{\n"
	"		tests_failed++;\n"
	"		print( \"FAIL `\", name, \"`\" );\n"
	"		if( onfail !== null )\n"
	"			print( \" - \", onfail );\n"
	"		print( \"\\n\" );\n"
	"	}\n"
	"}\n";
	/* for ISO C90 support */
	const char* sgs_testapi2 =
	"function testEqual( what, expect, name, onfail ){\n"
	"	var failmsg = \"expected \\\"\" $ expect $ \"\\\", got \\\"\" $ what $ \"\\\"\";\n"
	"	if( onfail !== null ) failmsg $= \" (\" $ onfail $ \")\";\n"
	"	if( name === null ) name = \"...expecting \\\"\" $ expect $ \"\\\"\";\n"
	"	test( what === expect, name, failmsg );\n"
	"}\n"
	;

	ret = sgs_ExecString( C, sgs_testapi );
	SGS_UNUSED( ret );
	sgs_BreakIf( ret != SGS_SUCCESS );

	ret = sgs_ExecString( C, sgs_testapi2 );
	SGS_UNUSED( ret );
	sgs_BreakIf( ret != SGS_SUCCESS );

	sgs_SetMsgFunc( C, TF_printfn, NULL );
}

/* test statistics */
int tests_executed = 0;
int tests_failed = 0;

int numallocs = 0;
static void* ext_memfunc( void* ud, void* ptr, size_t size )
{
	if( ptr ) numallocs--;
	if( size ) numallocs++;
	else if( !ptr )
		return NULL;
	SGS_UNUSED( ud );
	return realloc( ptr, size );
}


#define ATTMKR "\n\n#### "
static void checkdestroy_context( sgs_Context* C )
{
	int all = 1;

	/* pre-destroy */
	all &= C->stack_top == C->stack_base;
	if( C->stack_top != C->stack_base ) printf( ATTMKR "stack left in bad state\n" );

	/* DESTROY */
	sgs_DestroyEngine( C );

	/* post-destroy */
	all &= numallocs == 0;
	if( numallocs > 0 ) printf( ATTMKR "memory leaks detected (numallocs = %d)\n", numallocs );
	if( numallocs < 0 ) printf( ATTMKR "repeated frees detected (numallocs = %d)\n", numallocs );

	if( !all )
	{
		printf( "\n\n\tcritical error in tests - aborting now\n\n" );
		exit( 1 );
	}
}

static void exec_test( const char* fname, const char* nameonly, int disp )
{
	FILE* fp, *fpe;
	int retval;
	sgs_Context* C;
	double tm1, tm2;

	fpe = fopen( outfile_errors, "a" );
	numallocs = 0;
	C = sgs_CreateEngineExt( ext_memfunc, NULL );
	sgs_SetErrOutputFunc( C, SGSOUTPUTFN_DEFAULT, fpe );
	sgs_SetMsgFunc( C, SGSMSGFN_DEFAULT_NOABORT, NULL );

	fprintf( fpe, "\n>>> test: %s\n", nameonly );
	printf( "> running %20s [%s]\t", nameonly, disp == 0 ? " " : ( disp >= 0 ? "+" : "-" ) );

	if( strstr( nameonly, "TF" ) != NULL )
	{
		prepengine( C );
	}

	fp = fopen( outfile, "a" );
	setvbuf( fp, NULL, _IONBF, 0 );
	sgs_SetOutputFunc( C, SGSOUTPUTFN_DEFAULT, fp );
	fprintf( fp, "//\n/// O U T P U T  o f  %s\n//\n\n", nameonly );

	tm1 = sgs_GetTime();
	retval = sgs_ExecFile( C, fname );
	tm2 = sgs_GetTime();
	
	if( retval != SGS_SUCCESS && disp > 0 && strstr( nameonly, "TF" ) != NULL )
	{
		sgs_PushGlobalByName( C, "ERRORS" );
		puts( sgs_DebugPrintVar( C, sgs_StackItem( C, -1 ) ) );
		sgs_Pop( C, 2 );
	}
	
	if( strstr( nameonly, "TF" ) != NULL &&
		sgs_PushGlobalByName( C, "tests_failed" ) )
	{
		if( sgs_GetInt( C, -1 ) )
			retval = SGS_EINPROC;
		sgs_Pop( C, 1 );
	}

	fprintf( fp, "\n\n" );
	fclose( fp );
	sgs_SetOutputFunc( C, SGSOUTPUTFN_DEFAULT, stdout );

	tests_executed++;
/*	if( disp )	*/
	{
		int has_errors = retval == SGS_SUCCESS ? 1 : -1;
		const char* sucfail = has_errors * disp >= 0 ? " OK" : " FAIL";
		if( has_errors * disp < 0 )
			tests_failed++;
		if( disp == 0 && has_errors < 0 ) sucfail = " ~~";
		printf( "time: %f seconds |%s", tm2 - tm1, sucfail );
	}
	checkdestroy_context( C );
	fclose( fpe );
	printf( "\n" );
}

static void exec_tests( const char* dirname )
{
	int ret;
	size_t count;
	testfile* files, *f, *fend;
	fclose( fopen( outfile, "w" ) );
	fclose( fopen( outfile_errors, "w" ) );
	printf( "\n" );

	ret = load_testfiles( dirname, &files, &count );
	if( !ret )
	{
		printf( "\n\nfailed to load tests! aborting...\n\n" );
		exit( 1 );
	}

	f = files;
	fend = files + count;
	while( f < fend )
	{
		exec_test( f->fullname, f->nameonly, f->sucfail );
		f++;
	}

	free_testfiles( files, count );
}

int
#ifdef _MSC_VER
__cdecl
#endif
main( int argc, char** argv )
{
	int i;
	char *testname = NULL, *dirname = "tests";
	setvbuf( stdout, NULL, _IONBF, 0 );
	printf( "\n//\n/// SGScript test framework\n//\n" );
	
	for( i = 1; i < argc; ++i )
	{
		if( ( !strcmp( argv[i], "--dir" ) || !strcmp( argv[i], "-d" ) ) && i + 1 < argc )
			dirname = argv[++i];
		else if( ( !strcmp( argv[i], "--test" ) || !strcmp( argv[i], "-t" ) ) && i + 1 < argc )
			testname = argv[++i];
	}

	if( testname )
	{
		printf( "\n/// Executing test %s...\n", testname );
		fclose( fopen( outfile, "w" ) );
		fclose( fopen( outfile_errors, "w" ) );
		exec_test( testname, testname, 0 );
		return 0;
	}

	exec_tests( dirname );

	printf( "\n///\n/// Tests failed:  %d  / %d\n///\n", tests_failed, tests_executed );
	printf( "..note: some tests may fail in different ways,\nmight want to review the logs..\n\n" );

	return 0;
}
