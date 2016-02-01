
#include <stddef.h>
#include <stdio.h>
#include <assert.h>
#include <errno.h>

#include "sgscript.h"


#define EPFX "SGSC Error: "
#define EPRINT( x ) printf( EPFX x "\n" )

int action = 0;
const char* infile = NULL;
const char* outfile = NULL;
const char* outext = ".sgc";

void print_help()
{
	printf( "Available options:\n"
	        "  -h\t- print help info\n"
	        "  -c\t- compile a file\n"
	        "  -d\t- dump bytecode to STDOUT\n"
	        "  -o\t- set compiled output file name (optional)\n"
	        "note: either -c or -d must be specified\n"
	        "example usage:\n"
	        "> sgsc -c script.sgs -o compiled.sgc\n"
	        "> sgsc -d compiled.sgc\n"
	        "\n" );
}

int loadfile( const char* file, char** out, size_t* outsize )
{
	char* data;
	size_t len;
	ssize_t ftrv;
	FILE* f;

	f = fopen( file, "rb" );
	if( !f )
		return SGS_ENOTFND;
	fseek( f, 0, SEEK_END );
	ftrv = ftell( f );
	if( ftrv < 0 )
	{
		fclose( f );
		return SGS_EINPROC;
	}
	len = (size_t) ftrv;
	fseek( f, 0, SEEK_SET );

	data = malloc( len );
	if( fread( data, 1, len, f ) != len )
	{
		fclose( f );
		free( data );
		return SGS_EINPROC;
	}
	fclose( f );

	*out = data;
	*outsize = len;
	return SGS_SUCCESS;
}

int main( int argc, char** argv )
{
	int i;

	printf( "SGSC [SGScript v%s]\n", SGS_VERSION );

	/* parse */
	for( i = 1; i < argc; ++i )
	{
		if( 0 == strcmp( argv[ i ], "-h" ) )
		{
			print_help();
			return 0;
		}
		else if( 0 == strcmp( argv[ i ], "-c" ) )
			action = 1;
		else if( 0 == strcmp( argv[ i ], "-d" ) )
			action = 2;
		else if( 0 == strcmp( argv[ i ], "-o" ) )
		{
			i++;
			if( i >= argc )
			{
				EPRINT( "expected file name after -o" );
				return 1;
			}
			outfile = argv[ i ];
		}
		else if( argv[ i ][ 0 ] == '-' )
		{
			EPRINT( "unrecognized option" );
			print_help();
			return 1;
		}
		else if( infile )
		{
			EPRINT( "can only process one file at a time" );
			return 1;
		}
		else
			infile = argv[ i ];
	}

	/* validate */
	if( !action )
	{
		EPRINT( "action (-c or -d) not specified" );
		print_help();
		return 1;
	}
	else if( !infile )
	{
		EPRINT( "no input file" );
		print_help();
		return 1;
	}

	/* do */
	{
		int ret;
		FILE* f;
		char of[ 270 ];
		size_t size;
		char* data = NULL;
		sgs_Context* C = sgs_CreateEngine(); /* RSRC: sgs_CreateEngine -> C */

		ret = loadfile( infile, &data, &size );
		if( ret != SGS_SUCCESS )
		{
			printf( EPFX "failed to read the file, error %d\n", ret );
			sgs_DestroyEngine( C );
			return ret;
		}
		/* RSRC: loadfile -> data */

		if( action == 1 )
		{
			char* data2 = NULL;
			size_t size2;
			ret = sgs_Compile( C, data, size, &data2, &size2 );
			free( data );
			/* FREE: loadfile */
			if( ret != SGS_SUCCESS )
			{
				printf( EPFX "failed to compile, error %d\n", ret );
				sgs_DestroyEngine( C );
				return ret;
			}
			/* RSRC: sgs_Compile -> data2 */
			if( outfile )
				strncpy( of, outfile, 260 );
			else
			{
				char* sp, *pp = NULL;
				strncpy( of, infile, 260 );
				sp = strstr( of, ".sgs" );
				while( sp )
				{
					pp = sp;
					sp = strstr( sp + 1, ".sgs" );
				}
				if( pp - of + 4 == (ptrdiff_t) strlen( of ) )
					memcpy( pp, ".sgc", 4 );
				else
					strcat( of, ".sgc" );
			}
			f = fopen( of, "wb" );
			if( !f )
			{
				sgs_Free( C, data2 );
				sgs_DestroyEngine( C );
				printf( EPFX "failed to open output file for writing\n" );
				return errno;
			}
			/* RSRC: fopen -> f */
			if( fwrite( data2, size2, 1, f ) < 1 )
			{
				fclose( f );
				sgs_Free( C, data2 );
				sgs_DestroyEngine( C );
				printf( EPFX "failed to write to '%s'\n", of );
				return errno;
			}
			sgs_Free( C, data2 ); /* FREE: sgs_Compile */
			fclose( f ); /* FREE: fopen */

			printf( "successfully wrote bytecode to '%s'\n", of );
		}
		else if( action == 2 )
		{
			/* MAINTAIN
				- RSRC: loadfile -> data
				- RSRC: sgs_CreateEngine -> C
			*/
			ret = sgs_DumpCompiled( C, data, size );
			free( data );
			/* FREE: loadfile */

			if( ret != SGS_SUCCESS )
			{
				printf( EPFX "failed to dump input file, error %d\n", ret );
				sgs_DestroyEngine( C );
				return ret;
			}
		}

		sgs_DestroyEngine( C );
	}

	return 0;
}
