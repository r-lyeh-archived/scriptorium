
#include <stdio.h>
#include <assert.h>

#include "sgscript.h"
#include "sgs_idbg.h"
#include "sgs_prof.h"


#define EPFX "SGSVM Error: "
#define EPRINT( x ) printf( EPFX x "\n" )

void readme()
{
	puts( "syntax:" );
	puts( "\tsgsvm [files|options]" );
	puts( "\tsgsvm [options] -p <file>[, <arguments>]" );
	puts( "" );
	puts( "options:" );
	puts( "\t-h, --help: print this text" );
	puts( "\t-v, --version: print version info" );
	puts( "\t-s, --separate: restart the engine between scripts" );
	puts( "\t-d, --debug: enable interactive debugging on errors" );
	puts( "\t-p, --program: translate the following arguments into a SGS program call" );
	puts( "\t--stats: print VM stats after running the scripts" );
	puts( "\t--profile: enable profiling by collecting call stack timings" );
	puts( "\t--profile-ops: enable low-level VM instruction profiling" );
	puts( "\t--profile-mem: enable memory usage profiling" );
}

int sep = 0, v = 0;
sgs_Context* C;
sgs_IDbg D;
sgs_Prof P;
int idbg = 0;
int prof = 0;
int stats = 0;

void sgs_init()
{
	C = sgs_CreateEngine();
	if( idbg ) sgs_InitIDbg( C, &D );
	if( prof ) sgs_ProfInit( C, &P, prof );
}
void sgs_close()
{
	if( idbg ) sgs_CloseIDbg( C, &D );
	if( prof )
	{
		sgs_ProfDump( C, &P );
		sgs_ProfClose( C, &P );
	}
	if( stats )
		sgs_Stat( C, SGS_STAT_DUMP_STATS );
	sgs_DestroyEngine( C );
}
void sgs_dofile( const char* name, int incl )
{
	int rv = incl ? sgs_Include( C, name ) : sgs_ExecFile( C, name );

	if( rv < 0 )
	{
		if( rv == SGS_ENOTFND ) printf( EPFX "file was not found: %s\n", name );
		else if( rv == SGS_EINPROC ) printf( EPFX "failed to load file: %s\n", name );
		else printf( EPFX "failed to run file: %s\n", name );
	}
}
void sgs_printversion()
{
	if( v )
		printf( "SGSVM [SGScript v%s]\n", SGS_VERSION );
}

int main( int argc, char** argv )
{
	int i, j;
	
	if( argc < 2 )
	{
		EPRINT( "need to specify at least one file" );
		readme();
		return 1;
	}
	
	for( i = 1; i < argc; ++i )
	{
		if( strcmp( argv[ i ], "--separate" ) == 0 ||
			strcmp( argv[ i ], "-s" ) == 0 ){ sep = 1; argv[ i ] = 0; }
		else if( strcmp( argv[ i ], "--debug" ) == 0 ||
			strcmp( argv[ i ], "-d" ) == 0 ){ idbg = 1; argv[ i ] = 0; }
		else if( strcmp( argv[ i ], "--profile" ) == 0 )
			{ prof = 1; argv[ i ] = 0; }
		else if( strcmp( argv[ i ], "--profile-ops" ) == 0 )
			{ prof = 2; argv[ i ] = 0; }
		else if( strcmp( argv[ i ], "--profile-mem" ) == 0 )
			{ prof = 3; argv[ i ] = 0; }
		else if( strcmp( argv[ i ], "--help" ) == 0 ||
			strcmp( argv[ i ], "-h" ) == 0 ){ readme(); return 0; }
		else if( strcmp( argv[ i ], "--version" ) == 0 ||
			strcmp( argv[ i ], "-v" ) == 0 ){ v = 1; argv[ i ] = 0; }
		else if( strcmp( argv[ i ], "--stats" ) == 0 ){ stats = 1; argv[ i ] = 0; }
		else if( strcmp( argv[ i ], "--program" ) == 0 ||
			strcmp( argv[ i ], "-p" ) == 0 )
		{
			i++;
			if( i == argc )
			{
				EPRINT( "file name expected" );
				return 1;
			}
			
			sgs_printversion();
			sgs_init();
			
			for( j = i; j < argc; ++j )
				sgs_PushString( C, argv[ j ] );
			sgs_CreateArray( C, NULL, argc - i );
			sgs_SetGlobalByName( C, "argv", sgs_StackItem( C, -1 ) );
			sgs_Pop( C, 1 );
			
			sgs_SetGlobalByName( C, "argc", sgs_MakeInt( argc - i ) );
			
			sgs_dofile( argv[ i ], 1 );
			sgs_close();
			return 0;
		}
	}
	
	sgs_printversion();
	
	sgs_init();
	for( i = 1; i < argc; ++i )
	{
		if( argv[ i ] )
		{
			sgs_dofile( argv[ i ], 0 );

			if( sep )
			{
				sgs_close();
				sgs_init();
			}
		}
	}
	
	sgs_close();
	return 0;
}
