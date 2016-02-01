
/*
	multithreading test
	- makes sure there are no conflicts that could prevent this system from
		being used in multithreaded software
*/

#include <pthread.h>
#include <assert.h>
#include <stdio.h>
#include <sgscript.h>


#define NUM_THREADS 4

const char* testcode =
"start = ftime();\n"
"while(ftime()-start<5){\n"
"	test = dict();}\n"
;

void* threadproc( void* arg )
{
	SGS_CTX = sgs_CreateEngine();
	sgs_ExecString( C, testcode );
	sgs_DestroyEngine( C );
	return NULL;
}

int main()
{
	int i, rc;
	pthread_t threads[ NUM_THREADS ];

	printf( "SGScript Multithreading Test\n- runs for 5 seconds and exits\n" );

	for( i = 0; i < NUM_THREADS; ++i )
	{
		rc = pthread_create( threads + i, NULL, threadproc, NULL );
		assert( !rc );
	}

	for( i = 0; i < NUM_THREADS; ++i )
	{
		rc = pthread_join( threads[ i ], NULL );
		assert( !rc );
	}

	printf( "\ndone!\n" );

	return 0;
}

