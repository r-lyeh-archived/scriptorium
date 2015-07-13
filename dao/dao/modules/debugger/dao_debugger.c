/*
// Dao Debugger
//
// Copyright (c) 2013,2014, Limin Fu
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

#include <string.h>

#include "daoValue.h"
#include "daoStream.h"
#include "daoRoutine.h"
#include "daoProcess.h"
#include "daoNamespace.h"
#include "daoVmspace.h"

extern void SplitByWhiteSpaces( const char *str, DList *tokens );
extern void Dao_AboutVar( DaoNamespace *ns, DaoValue *var, DString *str );

static const char *const sep =
"-------------------------------------------------------------------\n";
static const char *const help =
"h, help:       print this help info.\n"
"q, quit:       quit debugging.\n"
"k, kill:       kill the current virtual process.\n"
"a, about reg:  print info. about the data held by the register.\n"
"g, goto id:    goto id-th instruction.\n"
"l, list num:   list num instructions before or after the current.\n"
"p, print reg:  print the data held by the register.\n"
"t, trace dep:  trace back dep-depth in the calling stack.\n";


DAO_DLL void DaoDebugger_Debug( DaoDebugger *self, DaoProcess *proc, DaoStream *stream )
{
	DaoRoutine *routine = proc->activeRoutine;
	DString *input;
	DList *tokens;
	DString *line;
	DMap *cycData;
	char *chs, *cmd;
	int i;
	if( ! (proc->vmSpace->options & DAO_OPTION_DEBUG ) ) return;
	input = DString_New();
	tokens = DList_New(DAO_DATA_STRING);
	cycData = DMap_New(0,0);
	line = DString_New();
	if( stream == NULL ) stream = proc->vmSpace->stdioStream;
	while( proc->vmSpace->stopit == 0 ){
		if( proc->vmSpace->ReadLine ){
			chs = proc->vmSpace->ReadLine( "(debug) ", line );
			if( chs ){
				DString_SetChars( input, chs );
				DString_Trim( input, 1, 1, 0 );
				if( input->size && proc->vmSpace->AddHistory )
					proc->vmSpace->AddHistory( chs );
			}
		}else{
			DaoStream_WriteChars( stream, "(debug) " );
			DaoStream_ReadLine( stream, input );
		}
		if( input->size == 0 ) continue;
		SplitByWhiteSpaces( input->chars, tokens );
		if( tokens->size == 0 ) continue;
		cmd = tokens->items.pString[0]->chars;
		if( strcmp( cmd, "q" ) == 0 || strcmp( cmd, "quit" ) == 0 ){
			break;
		}else if( strcmp( cmd, "k" ) == 0 || strcmp( cmd, "kill" ) == 0 ){
			proc->status = DAO_PROCESS_ABORTED;
			break;
		}else if( strcmp( cmd, "a" ) == 0 || strcmp( cmd, "about" ) == 0 ){
			if( tokens->size > 1 ){
				ushort_t reg = (ushort_t)strtod( tokens->items.pString[1]->chars, 0 );
				DaoType *tp = proc->activeTypes[ reg ];
				DString_Clear( input );
				Dao_AboutVar( proc->activeNamespace, proc->activeValues[reg], input );
				DaoStream_WriteChars( stream, "type: " );
				if( tp )
					DaoStream_WriteString( stream, tp->name );
				else
					DaoStream_WriteChars( stream, "?" );
				DaoStream_WriteChars( stream, ", value: " );
				DaoStream_WriteString( stream, input );
				DaoStream_WriteChars( stream, "\n" );
			}
		}else if( strcmp( cmd, "g" ) == 0 || strcmp( cmd, "goto" ) == 0 ){
			if( tokens->size > 1 ){
				int n = atoi( tokens->items.pString[1]->chars );
				int entry = proc->activeCode - proc->activeRoutine->body->vmCodes->data.codes;
				if( n < 0 ) n = entry - n;
				if( n >= routine->body->vmCodes->size ) n = routine->body->vmCodes->size -1;
				proc->topFrame->entry = n;
				proc->status = DAO_PROCESS_STACKED;
				return;
			}
		}else if( strcmp( cmd, "h" ) == 0 || strcmp( cmd, "help" ) == 0 ){
			DaoStream_WriteChars( stream, help );
		}else if( strcmp( cmd, "l" ) == 0 || strcmp( cmd, "list" ) == 0 ){
			DString *mbs = DString_New();
			int entry = proc->activeCode - proc->activeRoutine->body->vmCodes->data.codes;
			int start = entry - 10;
			int end = entry;
			if( tokens->size >1 ){
				int dn = atoi( tokens->items.pString[1]->chars );
				if( dn < 0 ){
					start = entry + dn;
				}else if( dn > 0 ){
					start = entry;
					end = entry + dn;
				}
			}
			if( start < 0 ) start = 0;
			if( end >= routine->body->vmCodes->size ) end = routine->body->vmCodes->size - 1;
			DaoStream_WriteString( stream, routine->routName );
			DaoStream_WriteChars( stream, "(): " );
			if( routine->routType ) DaoStream_WriteString( stream, routine->routType->name );
			DaoStream_WriteChars( stream, "\n" );
			DaoStream_WriteChars( stream, daoRoutineCodeHeader );
			DaoStream_WriteChars( stream, sep );
			for( i=start; i<=end; i++ ){
				DaoRoutine_FormatCode( routine, i, *routine->body->annotCodes->items.pVmc[i], mbs );
				DaoStream_WriteString( stream, mbs );
			}
			DString_Delete( mbs );
		}else if( strcmp( cmd, "p" ) == 0 || strcmp( cmd, "print" ) == 0 ){
			if( tokens->size > 1 ){
				ushort_t reg = (ushort_t)atoi( tokens->items.pString[1]->chars );
				DaoValue_Print( proc->activeValues[reg], proc, stream, cycData );
				DaoStream_WriteChars( stream, "\n" );
			}
		}else if( strcmp( cmd, "t" ) == 0 || strcmp( cmd, "trace" ) == 0 ){
			int depth = 1;
			if( tokens->size >1 ) depth = atoi( tokens->items.pString[1]->chars );
			DaoProcess_Trace( proc, depth );
		}else{
			DaoStream_WriteChars( stream, "Unknown debugging command.\n" );
		}
	}
	DString_Delete( input );
	DString_Delete( line );
	DList_Delete( tokens );
}

static DaoDebugger debugger = { NULL, NULL };

DAO_DLL int DaoDebugger_OnLoad( DaoVmSpace *vmSpace, DaoNamespace *ns )
{
	debugger.Debug = DaoDebugger_Debug;
	DaoVmSpace_SetUserDebugger( vmSpace, & debugger );
	return 0;
}
