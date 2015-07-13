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

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include"dao.h"

#ifdef DAO_WITH_THREAD
#include "daoThread.h"
#endif

#ifdef DAO_USE_READLINE
#include"readline/readline.h"
#include"readline/history.h"
#endif

#include"signal.h"
/*#include"mcheck.h" */

static int readingline = 0;
static DaoVmSpace *vmSpace = NULL;

static int count = 0;
static char* DaoReadLine( const char *s, DString *buffer )
{
	int ch;
	char *line;

	DString_Reset( buffer, 0 );

#ifdef DAO_WITH_THREAD
	if( ! DThread_IsMain() ){
		printf( "%s", s );
		fflush( stdout );
		while( (ch = getchar()) != '\n' ) DString_AppendWChar( buffer, ch );
		return DString_GetData( buffer );
	}
#endif

	readingline = 1;
	count = 0;

#ifdef DAO_USE_READLINE
	line = readline( s );
	DString_SetChars( buffer, line );
	free( line );
#endif
	readingline = 0;
	return DString_GetData( buffer );
}
static void DaoSignalHandler( int sig )
{
#ifdef DAO_WITH_THREAD
	if( ! DThread_IsMain() ) return;
#endif

	DaoVmSpace_Stop( vmSpace, 1);
	if( count ++ ) exit(1);
	count += 1;

	if( readingline ){
		printf( "\n" );
#ifdef DAO_USE_READLINE
		if( rl_end ==0 ) printf( "type \"q\" to quit.\n" );
#ifndef MACOSX
		rl_replace_line( "", 0 );
		rl_forced_update_display();
#else
		rl_reset_terminal( "" );
#endif
#endif
	}else{
		printf( "keyboard interrupt...\n" );
	}
}


#if defined(DAO_WITH_RESTART) && defined(UNIX)

#include <unistd.h>

void DaoRestartRun( char **argv, int argc, int optid )
{
	int forked = 0;
	for(;;){
		int status = 0;
		if( forked ) waitpid( 0, & status, 0 );
		if( forked == 0 || !WIFEXITED( status ) ){
			int pid = fork();
			forked = 1;
			if( pid == 0 ){
				fprintf( stderr, "Dao process forked!\n" );
				break;
			}else if( pid < 0 ){
				fprintf( stderr, "Failed to fork and restart!\n" );
				exit(1);
			}
		}else if( forked ){
			exit(1);
		}
	}
}

#elif defined(DAO_WITH_RESTART) && defined(WIN32)

#include <windows.h>

void DaoRestartRun( char **argv, int argc, int optid )
{
	int i, m, offset = 0;
	char cmdline[1024] = {0};
    STARTUPINFO si;
    PROCESS_INFORMATION pi;

	for(i=0; i<argc; ++i){
		if( i == optid ) continue;
		m = strlen( argv[i] );
		if( offset + m + 1 >= sizeof(cmdline) ){
			fprintf( stderr, "Failed option \"%s\": command line too long!\n", argv[optid] );
			return;
		}
		if( i ) strcat( cmdline + (offset++), " " );
		strcat( cmdline + offset, argv[i] );
	}

	for(;;){
		DWORD exitCode = 0;
		ZeroMemory( &si, sizeof(si) );
		ZeroMemory( &pi, sizeof(pi) );
		si.cb = sizeof(si);

		/* Start the child process.  */
		if( !CreateProcess( NULL,  /* No module name (use command line) */
					cmdline,  /* Command line */
					NULL,     /* Process handle not inheritable */
					NULL,     /* Thread handle not inheritable */
					FALSE,    /* Set handle inheritance to FALSE */
					0,        /* No creation flags */
					NULL,     /* Use parent's environment block */
					NULL,     /* Use parent's starting directory  */
					&si,      /* Pointer to STARTUPINFO structure */
					&pi )     /* Pointer to PROCESS_INFORMATION structure */
		  ){
			fprintf( stderr, "CreateProcess failed (%d).\n", GetLastError() );
			return;
		}
		fprintf( stderr, "Dao process forked!\n" );

		/* Wait until child process exits. */
		WaitForSingleObject( pi.hProcess, INFINITE );

		GetExitCodeProcess( pi.hProcess, & exitCode );
		if( exitCode == 0 ) exit(0);

		/* Close process and thread handles.  */
		CloseHandle( pi.hProcess );
		CloseHandle( pi.hThread );
	}
}

#else

void DaoRestartRun( char **argv, int argc, int optid )
{
}

#endif


/*
// Adding virtual modules:
//
// * Create a C source file and define an variable of DaoVModule array;
// * Name the variable as "dao_virtual_modules";
// * Terminate the array with { NULL, ... };
// * Compile files with -DDAO_WITH_STATIC_MODULES;
// * Link these file together (possibly with the C modules);
//
// In this way, these virtual modules can be loaded in the normal way.
//
// DaoVModule dao_virtual_modules[] =
// {
//     { "hello.dao", 21, "io.writeln( 'hello world!' )", NULL },
//     { NULL, 0, NULL, NULL }
// };
*/

extern DaoVModule dao_virtual_modules[];


int main( int argc, char **argv )
{
	int restart = 0;
	int i, k, idsrc, vmods = 0;
	DString *opts, *args;

	/*mtrace(); */

	for(i=1; i<argc; i++){
		if( strcmp( argv[i], "-r" ) ==0 || strcmp( argv[i], "--restart" ) ==0 ){
			restart = i;
			break;
		}
	}
	if( restart ) DaoRestartRun( argv, argc, restart );

	vmSpace = DaoInit( argv[0] );

	idsrc = -1;
	for(i=1; i<argc; i++){
		if( strcmp( argv[i], "-e" ) ==0 || strcmp( argv[i], "--eval" ) ==0 ) break;
		/* also allows execution of script files without suffix .dao */
		if( argv[i][0] != '-' ){
			idsrc = i;
			break;
		}
	}

#ifdef DAO_WITH_STATIC_MODULES
	idsrc = 1;
	vmods = DaoVmSpace_AddVirtualModules( vmSpace, dao_virtual_modules );
#endif

	k = idsrc;
	if( k < 0 ) k = argc;

	opts = DString_New();
	args  = DString_New();
	for(i=1; i<k; i++ ){
		DString_AppendChars( opts, argv[i] );
		DString_AppendChar( opts, '\1' );
	}
	if( idsrc >= 0 ){
		for(i=idsrc; i<argc; i++ ){
			DString_AppendChars( args, argv[i] );
			DString_AppendChar( args, '\1' );
		}
	}
	DaoVmSpace_ParseOptions( vmSpace, DString_GetData( opts ) );

#ifdef DAO_WITH_STATIC_MODULES
	if( vmods ){
		DString_InsertChars( args, "/@/\1", 0, 0, 0 );
		DString_InsertChars( args, dao_virtual_modules[0].name, 3, 0, 0 );
		/* set the path for the virtual files: */
		DaoVmSpace_SetPath( vmSpace, "/@/" );
	}else
#endif
	if( idsrc < 0 && argc == 1 ){
		DString_AppendChar( opts, '\1' );
		DString_AppendChars( opts, "-vi" );
		DaoVmSpace_ParseOptions( vmSpace, DString_GetData( opts ) );
	}


#ifdef DAO_USE_READLINE
	DaoVmSpace_ReadLine( vmSpace, DaoReadLine );
	DaoVmSpace_AddHistory( vmSpace, (AddHistory) add_history );
	read_history( NULL );
#endif

	signal( SIGINT, DaoSignalHandler );

	/* Start execution. */
	k = ! DaoVmSpace_RunMain( vmSpace, DString_GetData( args ) );

#ifdef DAO_USE_READLINE
	write_history( NULL );
#endif

	DString_Delete( args );
	DString_Delete( opts );
	DaoQuit();
	return k;
}
