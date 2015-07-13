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

#include"stdlib.h"
#include"stdio.h"
#include"string.h"
#include"locale.h"
#include"ctype.h"
#include<assert.h>

#include"daoConst.h"
#include"daoVmcode.h"
#include"daoParser.h"
#include"daoRegex.h"
#include"daoNumtype.h"
#include"daoRoutine.h"
#include"daoClass.h"
#include"daoObject.h"
#include"daoVmspace.h"
#include"daoNamespace.h"
#include"daoStream.h"
#include"daoStdlib.h"
#include"daoProcess.h"
#include"daoGC.h"
#include"daoBase.h"
#include"daoValue.h"

enum InExpressionAssignmentStatus { ASSIGNMENT_OK, ASSIGNMENT_WARNING, ASSIGNMENT_ERROR };

/* Expression node */
typedef struct DaoEnode DaoEnode;
struct DaoEnode
{
	int reg;    /* vm register id, for the value produced by the expression */
	int konst;  /* constant id for the value of a constant expression */
	int count;  /* number of expressions in an expression list */
	int scope;  /* constant id for scope value of a constant expression */
	DaoInode *prev;   /* the previous instruction, should never be NULL */
	DaoInode *first;  /* the first instruction node for the expression */
	DaoInode *last;   /* the last instruction node for the expression */
	DaoInode *update; /* the last instruction that updates the value of "reg" */
};


DAO_DLL void DaoParser_Warn2( DaoParser *self, int code, int start, int end );
DAO_DLL void DaoParser_Warn( DaoParser *self, int code, DString *ext );
DAO_DLL void DaoParser_Error( DaoParser *self, int code, DString *ext );
DAO_DLL void DaoParser_Error2( DaoParser *self, int code, int m, int n, int single_line );
DAO_DLL void DaoParser_Error3( DaoParser *self, int code, int m );
DAO_DLL void DaoParser_MakeCodes( DaoParser *self, int start, int end, DString *output );
DAO_DLL int DaoParser_ParseLoadStatement( DaoParser *self, int start, int end );
DAO_DLL int DaoParser_FindOpenToken( DaoParser *self, uchar_t tok, int start, int end, int warn );
DAO_DLL int DaoParser_FindPairToken( DaoParser *self,  uchar_t lw, uchar_t rw, int start, int stop );


DAO_DLL int DaoParser_GetOperPrecedence( DaoParser *self );
DAO_DLL int DaoParser_CurrentTokenType( DaoParser *self );
DAO_DLL int DaoParser_CurrentTokenName( DaoParser *self );
DAO_DLL int DaoParser_NextTokenType( DaoParser *self );
DAO_DLL int DaoParser_NextTokenName( DaoParser *self );

static int DaoParser_CheckTokenType( DaoParser *self, int tok, const char *str );
static int DaoParser_ParseSymbol( DaoParser *self, DString *symbol );
static DaoEnode DaoParser_ParsePrimary( DaoParser *self, int stop, int eltype );
static DaoEnode DaoParser_ParseUnary( DaoParser *self, int stop, int eltype );
static DaoEnode DaoParser_ParseExpression( DaoParser *self, int stop );
static DaoEnode DaoParser_ParseExpression2( DaoParser *self, int stop, int eltype, int warn );
static DaoEnode DaoParser_ParseExpressionList( DaoParser *self, int, DaoInode*, DList* );
static DaoEnode DaoParser_ParseExpressionList2( DaoParser *self, int, DaoInode*, DList*, int);
static DaoEnode DaoParser_ParseExpressionLists( DaoParser *self, int, int, int*, DList* );
static DaoEnode DaoParser_ParseEnumeration( DaoParser *self, int etype, int btype, int lb, int rb );
static void DaoParser_PushTokenIndices( DaoParser *self, int first, int middle, int last );

DaoProcess* DaoNamespace_ReserveFoldingOperands( DaoNamespace *self, int N );
static int DaoParser_MakeEnumConst( DaoParser *self, DaoEnode *enode, DList *cid, int regcount );
static int DaoParser_MakeArithConst( DaoParser *self, ushort_t, DaoValue*, DaoValue*, int*, DaoInode*, int );
static int DaoParser_ParseClosure( DaoParser *self, int start );
static DaoValue* DaoParser_EvalConst( DaoParser *self, DaoProcess *proc, int nvalues );


typedef struct DStringIntPair
{
	const char *key;
	int   value;
}DStringIntPair;

extern DOper daoArithOper[DAO_NOKEY2];
extern DIntStringPair dao_keywords[];

static const int mapAithOpcode[]=
{
	DVM_NOP ,
	DVM_MOVE , /* DAO_OPER_ASSN */
	DVM_ADD , /* DAO_OPER_ASSN_ADD */
	DVM_SUB , /* DAO_OPER_ASSN_SUB */
	DVM_MUL , /* DAO_OPER_ASSN_DIV */
	DVM_DIV , /* DAO_OPER_ASSN_MUL */
	DVM_MOD , /* DAO_OPER_ASSN_MOD */
	DVM_BITAND , /* DAO_OPER_ASSN_AND */
	DVM_BITOR  , /* DAO_OPER_ASSN_OR */
	DVM_BITXOR , /* DAO_OPER_ASSN_XOR */

	DVM_NOP,
	DVM_PAIR , /* DAO_OPER_COLON */

	DVM_BITLFT, /* << */
	DVM_BITRIT, /* >> */
	DVM_BITAND, /* & */
	DVM_BITOR,  /* | */
	DVM_BITXOR, /* ^ */

	DVM_AND , /* DAO_OPER_AND */
	DVM_OR  , /* DAO_OPER_OR */

	DVM_IN  , /* DAO_OPER_IN */
	-DVM_IN , /* DAO_OPER_NOTIN */

	DVM_LT  , /* DAO_OPER_LT */
	-DVM_LT , /* DAO_OPER_GT */
	DVM_EQ  , /* DAO_OPER_EQ */
	DVM_NE  , /* DAO_OPER_NE */
	DVM_LE  , /* DAO_OPER_LE */
	-DVM_LE , /* DAO_OPER_GE */
	DVM_SAME ,
	DVM_ISA ,

	DVM_ADD , /* DAO_OPER_ADD */
	DVM_SUB , /* DAO_OPER_SUB */
	DVM_DIV , /* DAO_OPER_DIV */
	DVM_MUL , /* DAO_OPER_MUL */
	DVM_MOD , /* DAO_OPER_MOD */
	DVM_POW , /* ** */

	DVM_NOT /* DAO_OPER_NOT */
};

void DaoInode_Print( DaoInode *self, int index );
void DaoInode_Delete( DaoInode *self );

DaoParser* DaoParser_New()
{
	DaoParser *self = (DaoParser*) dao_calloc( 1, sizeof(DaoParser) );

	self->levelBase = 0;
	self->lexLevel = 0;
	self->evalMode = 0;

	self->fileName = DString_New();
	self->lexer = DaoLexer_New();
	self->tokens = self->lexer->tokens;
	self->tokenTriples = DArray_New( sizeof(int) );
	DaoParser_PushTokenIndices( self, 0, 0, 0 );

	self->vmCodes = DList_New( DAO_DATA_VMCODE );
	self->vmcBase = DaoInode_New();
	self->vmcFirst = self->vmcLast = self->vmcBase;
	self->vmcBase->code = DVM_UNUSED;
	self->vmcFree = NULL;

	self->allConsts = DHash_New( DAO_DATA_STRING, 0 );
	self->initTypes = DMap_New( DAO_DATA_STRING, DAO_DATA_VALUE );

	self->scopeOpenings = DList_New(0);
	self->scopeClosings = DList_New(0);
	self->decoFuncs   = DList_New(0);
	self->decoFuncs2  = DList_New(0);
	self->decoParams  = DList_New( DAO_DATA_VALUE );
	self->decoParams2 = DList_New( DAO_DATA_VALUE );
	self->refCountedList = DList_New( DAO_DATA_VALUE );
	self->nsDefines = NULL;
	self->nsSymbols = NULL;

	self->elexer = DaoLexer_New();
	self->wlexer = DaoLexer_New();
	self->errors = self->elexer->tokens;
	self->warnings = self->wlexer->tokens;

	self->noneValue = -1;
	self->integerZero = -1;
	self->integerOne = -1;
	self->imaginaryOne = -1;

	self->string = DString_New();
	self->string2 = DString_New();
	self->str = DString_New();
	self->denum = DaoEnum_New(NULL,0);

	self->toks = DList_New(0);
	self->table = DMap_New( DAO_DATA_STRING, 0 );
	self->lists = DList_New( DAO_DATA_LIST );
	self->strings = DList_New( DAO_DATA_STRING );
	self->lookupTables = DList_New( DAO_DATA_MAP );
	self->switchTables = DList_New( DAO_DATA_MAP );
	self->switchNames = DList_New( DAO_DATA_STRING );
	self->enumTypes = DList_New(0);
	self->routCompilable = DList_New(0);
	self->routReInferable = DList_New(0);
	DList_Append( self->lookupTables, self->table );
	DList_Append( self->strings, self->string );
	DList_Append( self->lists, self->toks );

	self->typeItems = DList_New(0);
	self->usedString = 0;
	self->usedList = 0;
	return self;
}
void DaoParser_ClearCodes( DaoParser *self );
void DaoParser_Delete( DaoParser *self )
{
	DaoInode *node;

	DaoEnum_Delete( self->denum );
	DString_Delete( self->fileName );
	DString_Delete( self->string );
	DString_Delete( self->string2 );
	DString_Delete( self->str );
	DArray_Delete( self->tokenTriples );
	DList_Delete( self->decoFuncs );
	DList_Delete( self->decoFuncs2 );
	DList_Delete( self->decoParams );
	DList_Delete( self->decoParams2 );
	DList_Delete( self->refCountedList );
	DList_Delete( self->toks );
	DList_Delete( self->scopeOpenings );
	DList_Delete( self->scopeClosings );
	DList_Delete( self->lookupTables );
	DList_Delete( self->switchTables );
	DList_Delete( self->switchNames );
	DList_Delete( self->enumTypes );
	DList_Delete( self->vmCodes );
	DList_Delete( self->strings );
	DList_Delete( self->lists );
	DList_Delete( self->routCompilable );
	DList_Delete( self->routReInferable );
	DList_Delete( self->typeItems );

	DaoLexer_Delete( self->lexer );
	DaoLexer_Delete( self->elexer );
	DaoLexer_Delete( self->wlexer );
	if( self->nsDefines ) DList_Delete( self->nsDefines );
	if( self->nsSymbols ) DaoLexer_Delete( self->nsSymbols );
	if( self->argName ) DaoToken_Delete( self->argName );
	if( self->decoArgName ) DaoToken_Delete( self->decoArgName );
	if( self->uplocs ) DArray_Delete( self->uplocs );
	if( self->outers ) DArray_Delete( self->outers );
	if( self->allConsts ) DMap_Delete( self->allConsts );
	DMap_Delete( self->initTypes );
	DMap_Delete( self->table );
	DaoParser_ClearCodes( self );
	node = self->vmcFree;
	while( node ){
		DaoInode *node2 = node;
		node = node->next;
		DaoInode_Delete( node2 );
	}
	DaoInode_Delete( self->vmcBase );
	dao_free( self );
}
void DaoParser_Reset( DaoParser *self )
{
	int i;
	for(i=0; i<=self->lexLevel; ++i) DMap_Reset( self->lookupTables->items.pMap[i] );
	self->evalMode = 0;
	self->autoReturn = 0;
	self->isClassBody = 0;
	self->isInterBody = 0;
	self->isCinTypeBody = 0;
	self->permission = 0;
	self->isSection = 0;
	self->needConst = 0;
	self->usingGlobal = 0;

	self->curToken = 0;
	self->regCount = 0;
	self->levelBase = 0;
	self->lexLevel = 0;
	self->usedString = 0;
	self->usedList = 0;

	self->curLine = 0;
	self->lineCount = 0;

	self->firstToken = 0;
	self->middleToken = 0;
	self->lastToken = 0;
	self->tokenTriples->size = 0;
	DaoParser_PushTokenIndices( self, 0, 0, 0 );

	self->noneValue = -1;
	self->integerZero = -1;
	self->integerOne = -1;
	self->imaginaryOne = -1;

	DString_Reset( self->string, 0 );
	DString_Reset( self->string2, 0 );
	DString_Reset( self->str, 0 );

	self->typeItems->size = 0;
	self->decoFuncs->size = 0;
	self->decoFuncs2->size = 0;
	self->toks->size = 0;

	self->scopeOpenings->size = 0;
	self->scopeClosings->size = 0;

	self->nameSpace = NULL;
	self->defParser = NULL;
	self->routine = NULL;
	self->hostType = NULL;
	self->hostCtype = NULL;
	self->hostClass = NULL;
	self->hostInter = NULL;
	self->hostCinType = NULL;
	self->outerParser = NULL;
	self->innerParser = NULL;
	self->byteCoder = NULL;
	self->byteBlock = NULL;
	self->returnType = NULL;
	self->vmcValue = NULL;

	DList_Clear( self->decoParams );
	DList_Clear( self->decoParams2 );
	DList_Clear( self->refCountedList );
	DList_Clear( self->switchTables );
	DList_Clear( self->switchNames );
	DList_Clear( self->enumTypes );
	DList_Clear( self->vmCodes );
	DList_Clear( self->routCompilable );
	DList_Clear( self->routReInferable );

	DaoLexer_Reset( self->lexer );
	DaoLexer_Reset( self->elexer );
	DaoLexer_Reset( self->wlexer );

	self->nsDefined = 0;
	if( self->nsDefines ) self->nsDefines->size = 0;
	if( self->nsSymbols ) DaoLexer_Reset( self->nsSymbols );
	if( self->uplocs ) DArray_Clear( self->uplocs );
	if( self->outers ) DArray_Clear( self->outers );
	if( self->allConsts ) DMap_Reset( self->allConsts );
	if( self->argName ) DaoToken_Delete( self->argName );
	if( self->decoArgName ) DaoToken_Delete( self->decoArgName );
	self->argName = NULL;
	self->decoArgName = NULL;

	DMap_Clear( self->initTypes );
	DMap_Reset( self->table );
	DaoParser_ClearCodes( self );
}

static DString* DaoParser_GetString( DaoParser *self )
{
	if( self->usedString >= self->strings->size )
		DList_Append( self->strings, self->strings->items.pString[0] );
	self->usedString += 1;
	self->strings->items.pString[ self->usedString - 1 ]->size = 0;
	return self->strings->items.pString[ self->usedString - 1 ];
}
static DList* DaoParser_GetArray( DaoParser *self )
{
	if( self->usedList >= self->lists->size )
		DList_Append( self->lists, self->lists->items.pList[0] );
	self->usedList += 1;
	self->lists->items.pList[ self->usedList - 1 ]->size = 0;
	return self->lists->items.pList[ self->usedList - 1 ];
}
DMap* DaoParser_CurrentSymbolTable( DaoParser *self )
{
	return self->lookupTables->items.pMap[ self->lexLevel ];
}
void DaoParser_PushLevel( DaoParser *self )
{
	self->lexLevel ++;
	if( self->lexLevel >= self->lookupTables->size ){
		DList_Append( self->lookupTables, self->table );
	}
}
void DaoParser_PopLevel( DaoParser *self )
{
	DMap_Reset( self->lookupTables->items.pMap[ self->lexLevel ] );
	self->lexLevel --;
}
static int DaoParser_PushOuterRegOffset( DaoParser *self, int start, int end )
{
	if( self->outers == NULL ) self->outers = DArray_New(sizeof(int));
	if( self->outers->size >= DAO_MAX_SECTDEPTH ){
		DaoParser_Error2( self, DAO_SECTION_TOO_DEEP, start, end, 0 );
		return 0;
	}
	DArray_PushInt( self->outers, self->regCount );
	return 1;
}
static int DaoParser_GetOuterLevel( DaoParser *self, int reg )
{
	int i = 0;
	if( self->outers == NULL ) return 0;
	while( i < self->outers->size && reg >= self->outers->data.ints[i] ) i += 1;
	if( i >= self->outers->size ) return 0;
	return self->outers->size - i;
}
static void DaoParser_PushTokenIndices( DaoParser *self, int first, int middle, int last )
{
	if( first < 0 ) first = self->curToken;
	if( middle < first ) middle = first;
	if( last < middle ) last = middle;
	DArray_PushInt( self->tokenTriples, first );
	DArray_PushInt( self->tokenTriples, middle );
	DArray_PushInt( self->tokenTriples, last );
	self->firstToken = first;
	self->middleToken = middle;
	self->lastToken = last;
}
static void DaoParser_PopTokenIndices( DaoParser *self, int triples )
{
	int k = self->tokenTriples->size - 3*triples - 3;
	assert( k >= 0 );
	self->tokenTriples->size -= 3*triples;
	self->firstToken = self->tokenTriples->data.ints[k];
	self->middleToken = self->tokenTriples->data.ints[k+1];
	self->lastToken = self->tokenTriples->data.ints[k+2];
}

static void DaoParser_PrintCodes( DaoParser *self )
{
	DaoInode *it = self->vmcFirst;
	int i = 0;
	while( it ){
		DaoInode_Print( it, i++ );
		it = it->next;
	}
}

void DaoParser_CacheNode( DaoParser *self, DaoInode *node )
{
	node->prev = NULL;
	node->extra = NULL;
	node->next = self->vmcFree;
	self->vmcFree = node;
}
DaoInode* DaoParser_NewNode( DaoParser *self )
{
	if( self->vmcFree ){
		DaoInode *node = self->vmcFree;
		self->vmcFree = self->vmcFree->next;
		node->next = NULL;
		return node;
	}
	return DaoInode_New();
}
void DaoParser_ClearCodes( DaoParser *self )
{
	DaoInode *it = self->vmcFirst;
	while( it != self->vmcBase ){
		it = it->next;
		DaoParser_CacheNode( self, it->prev );
	}
	it = self->vmcLast;
	while( it != self->vmcBase ){
		it = it->prev;
		DaoParser_CacheNode( self, it->next );
	}
	self->vmcBase->prev = self->vmcBase->next = NULL;
	self->vmcFirst = self->vmcLast = self->vmcBase;
	self->vmcCount = 0;
}
static void DaoParser_PopBackCode( DaoParser *self )
{
	if( self->vmcLast == NULL || self->vmcLast == self->vmcBase ) return;
	self->vmcLast = self->vmcLast->prev;
	DaoParser_CacheNode( self, self->vmcLast->next );
	self->vmcLast->next = NULL;
	self->vmcCount --;
}
static int DaoParser_PopCodes( DaoParser *self, DaoInode *back )
{
	int count = 0;
	DaoInode *node = NULL;
	while( (node=self->vmcLast) != back ) DaoParser_PopBackCode( self ), count ++;
	self->vmcValue = NULL;
	return count;
}
/* In constant folding, do not actually remove the codes, which may invalidate
 * some references in DaoEnode structures: */
static int DaoParser_PopCodes2( DaoParser *self, DaoInode *back )
{
	int count = 0;
	DaoInode *node = self->vmcLast;
	while( node != back ){
		node->code = DVM_UNUSED;
		node = node->prev;
		count ++;
	}
	self->vmcValue = NULL;
	return count;
}
static void DaoParser_AppendCode( DaoParser *self, DaoInode *inode )
{
	if( inode == self->vmcLast ) return;
	if( inode->next ){
		inode->prev->next = inode->next;
		inode->next->prev = inode->prev;
	}
	inode->prev = self->vmcLast;
	self->vmcLast->next = inode;
	self->vmcLast = inode;
	inode->next = NULL;
}
static DaoInode* DaoParser_AddCode2( DaoParser *self, ushort_t code, ushort_t a, ushort_t b, ushort_t c );
static DaoInode* DaoParser_InsertCode( DaoParser *self, DaoInode *after, int code, int a, int b, int c );
static DaoInode* DaoParser_PushBackCode( DaoParser *self, DaoVmCodeX *vmc )
{
	DaoInode *node = DaoParser_NewNode( self );
	memcpy( node, vmc, sizeof(DaoVmCode) );
	node->level = vmc->level;
	node->line = vmc->line;
	node->first = vmc->first;
	node->middle = vmc->middle;
	node->last = vmc->last;

	self->vmcLast->next = node;
	node->prev = self->vmcLast;
	self->vmcLast = node;
	self->vmcCount ++;
	return self->vmcLast;
}

void DaoParser_Warn( DaoParser *self, int code, DString *ext )
{
	if( ext && ext->size > 100 ) DString_Erase( ext, 100, -1 );
	DaoLexer_Append( self->wlexer, code, self->curLine, ext ? ext->chars : "" );
}
void DaoParser_Error( DaoParser *self, int code, DString *ext )
{
	if( code != DAO_EVAL_EXCEPTION &&  ext && ext->size > 100 ) DString_Erase( ext, 100, -1 );
	DaoLexer_Append( self->elexer, code, self->curLine, ext ? ext->chars : "" );
}
void DaoParser_SumTokens( DaoParser *self, DString *sum, int m, int n, int single_line )
{
	DaoToken **tokens = self->tokens->items.pToken;
	DaoToken *tok, *tok0 = NULL;
	int i, line = self->curLine;
	DString_Clear( sum );
	if( m < 0 ) m = 0;
	if( n >= self->tokens->size ) n = self->tokens->size - 1;
	if( m < n ) line = tokens[m]->line;
	for(i=m; i<=n; i++){
		tok = tokens[i];
		if( single_line && (int)tok->line > line ) break;
		if( tok0 && (tok->line != tok0->line || tok->cpos > (tok0->cpos + tok0->string.size)) )
			DString_AppendChar( sum, ' ' );
		tok0 = tok;
		DString_Append( sum, & tok->string );
		if( i<n && sum->size > 30 ){
			DString_AppendChars( sum, " ..." );
			break;
		}
	}
}
void DaoParser_Warn2( DaoParser *self, int code, int start, int end )
{
	DaoParser_SumTokens( self, self->string, start, end, 0 );
	DaoParser_Warn( self, code, self->string );
}
/* tokens from m to n as message */
void DaoParser_Error2( DaoParser *self, int code, int m, int n, int single_line )
{
	DString *mbs = DaoParser_GetString( self );
	DaoParser_SumTokens( self, mbs, m, n, single_line );
	DaoLexer_Append( self->elexer, code, self->curLine, mbs->chars );
}
/* tokens from m until the end of the line as message */
void DaoParser_Error3( DaoParser *self, int code, int m )
{
	DString *mbs = DaoParser_GetString( self );
	DaoParser_SumTokens( self, mbs, m, self->tokens->size-1, 1 );
	DaoLexer_Append( self->elexer, code, self->curLine, mbs->chars );
}
void DaoParser_Error4( DaoParser *self, int code, int line, const char *msg )
{
	DaoLexer_Append( self->elexer, code, line, msg );
}
void DaoParser_ErrorToken( DaoParser *self, int code, DaoToken *token )
{
	DaoParser_Error4( self, code, token->line, token->string.chars );
}
void DaoParser_Suggest( DaoParser *self, const char *suggestion )
{
	DaoStream_WriteChars( self->vmSpace->errorStream, "suggestion:\n" );
	DaoStream_WriteChars( self->vmSpace->errorStream, suggestion );
	DaoStream_WriteChar( self->vmSpace->errorStream, '\n' );
}
void DaoParser_PrintInformation( DaoParser *self, DList *infolist, const char *header )
{
	int i;
	DaoStream *stream = self->vmSpace->errorStream;

	if( infolist->size ==0 ) return;
	DaoStream_WriteChars( stream, header );
	DaoStream_WriteChars( stream, " in file \"" );
	if( self->fileName->size )
		DaoStream_WriteString( stream, self->fileName );
	else
		DaoStream_WriteString( stream, self->nameSpace->name );
	DaoStream_WriteChars( stream, "\":\n" );

	for(i=infolist->size-1; i>=0; i--){
		DaoToken *tok = infolist->items.pToken[i];
		if( i < infolist->size-1 ){
			DaoToken *tok2 = infolist->items.pToken[i+1];
			if( tok->line == tok2->line && tok->name == tok2->name ){
				if( DString_EQ( & tok->string, & tok2->string ) ) continue;
			}
		}
		if( tok->name == 0 ){
			DaoStream_WriteChars( stream, "  From file : " );
		}else{
			DaoStream_WriteChars( stream, "  At line " );
			DaoStream_WriteInt( stream, tok->line );
			DaoStream_WriteChars( stream, " : " );
			if( tok->name < DAO_CTW_END ){
				DaoStream_WriteChars( stream, getCtInfo( tok->name ) );
				if( tok->string.size ) DaoStream_WriteChars( stream, " --- " );
			}
		}
		if( tok->name == DAO_EVAL_EXCEPTION ){
			DaoStream_WriteChars( stream, "\n" );
			DaoStream_WriteString( stream, & tok->string );
			DaoStream_WriteChars( stream, "\n" );
			continue;
		}
		if( tok->string.size ){
			DaoStream_WriteChars( stream, "\" " );
			DaoStream_WriteString( stream, & tok->string );
			DaoStream_WriteChars( stream, " \"" );
		}
		DaoStream_WriteChars( stream, ";\n" );
	}
	DList_Clear( infolist );
}
static void DaoParser_PrintWarnings( DaoParser *self )
{
	DaoParser_PrintInformation( self, self->warnings, "[[WARNING]]" );
}
void DaoParser_PrintError( DaoParser *self, int line, int code, DString *ext )
{
	DaoParser_PrintWarnings( self );
	if( code ) DaoParser_Error4( self, code, line, ext ? ext->chars : "" );
	DaoParser_PrintInformation( self, self->errors, "[[ERROR]]" );
}
static void DaoParser_StatementError( DaoParser *self, DaoParser *parser, int code )
{
	DaoInode *prev = NULL, *inode = parser->vmcLast;
	while( inode != parser->vmcBase ){
		int end = inode->first + inode->last;
		if( prev == NULL || inode->line != prev->line ){
			DString *mbs = DaoParser_GetString( self );
			DaoParser_SumTokens( parser, mbs, inode->first, end, 0 );
			DaoParser_Error4( self, code, inode->line, mbs->chars );
		}
		prev = inode;
		inode = inode->prev;
	}
}

int DaoParser_LexCode( DaoParser *self, const char *src, int replace )
{
	int flags = replace ? DAO_LEX_ESCAPE : 0;

	self->lineCount = DaoLexer_Tokenize( self->lexer, src, flags );
	if( self->lineCount == 0 ) return 0;

#if 0
	for(i=0; i<self->tokens->size; i++){
		DaoToken *tk = self->tokens->items.pToken[i];
		printf( "%4i: %4i %4i, %4i,  %s\n", i, tk->type, tk->name, tk->cpos, tk->string.chars );
	}
#endif
	return 1;
}

static int DaoParser_CheckTokenType( DaoParser *self, int tok, const char *str )
{
	daoint cur = self->curToken;
	DaoToken **tokens = self->tokens->items.pToken;
	if( cur < self->tokens->size && tokens[cur]->type == tok ) return 1;
	DaoParser_Error4( self, DAO_TOKEN_EXPECTING, tokens[cur]->line, str );
	return 0;
}
int DaoParser_CurrentTokenType( DaoParser *self )
{
	if( self->curToken >= self->tokens->size ) return 0;
	return self->tokens->items.pToken[self->curToken]->type;
}
int DaoParser_CurrentTokenName( DaoParser *self )
{
	if( self->curToken >= self->tokens->size ) return 0;
	return self->tokens->items.pToken[self->curToken]->name;
}
int DaoParser_NextTokenType( DaoParser *self )
{
	if( (self->curToken+1) >= self->tokens->size ) return 0;
	return self->tokens->items.pToken[self->curToken+1]->type;
}
int DaoParser_NextTokenName( DaoParser *self )
{
	if( (self->curToken+1) >= self->tokens->size ) return 0;
	return self->tokens->items.pToken[self->curToken+1]->name;
}

int DaoParser_FindOpenToken( DaoParser *self, uchar_t tok, int start, int end/*=-1*/, int warn/*=1*/ )
{
	int i, n1, n2, n3, n4;
	DaoToken **tokens = self->tokens->items.pToken;

	if( start < 0 ) goto ErrorTokenExpect;
	if( end == -1 || end >= self->tokens->size ) end = self->tokens->size-1;

	n1 = n2 = n3 = n4 = 0;
	for( i=start; i<=end; i++){
		uchar_t tki = tokens[i]->name;
		if( ! ( n1 | n2 | n3 | n4 ) && tki == tok ){
			return i;
		}else if( n1 <0 || n2 <0 || n3 <0 || n4 <0 ){
			goto ErrorTokenExpect;
		}else{
			switch( tki ){
			case DTOK_LCB : n1 ++; break;
			case DTOK_RCB : n1 --; break;
			case DTOK_LB  : n2 ++; break;
			case DTOK_RB  : n2 --; break;
			case DTOK_LSB : n3 ++; break;
			case DTOK_RSB : n3 --; break;
			}
		}
	}
ErrorTokenExpect:
	DString_SetChars( self->string, DaoToken_NameToString( tok ) );
	if( warn ) DaoParser_Error( self, DAO_TOKEN_NOT_FOUND, self->string );
	return -10000;
}
int DaoParser_FindPairToken( DaoParser *self,  uchar_t lw, uchar_t rw, int start, int stop/*=-1*/ )
{
	DaoToken **tokens = self->tokens->items.pToken;
	int k = 0;
	int i = start;
	int found = 0;
	uchar_t tk;

	if( start <0 ) goto ErrorUnPaired;
	if( stop== -1 ) stop = self->tokens->size-1;

	while(1){
		if( i > stop ) break;
		if( i >= (int) self->tokens->size ) break;

		tk = tokens[i]->name;
		if( tk == lw )
			k++;
		else if( tk == rw ){
			k--;
			found = 1;
		}

		if( k==0 && found ) return i;
		i++;
	}
ErrorUnPaired:
	if( self->vmSpace ){
		DString_SetChars( self->string, DaoToken_NameToString( lw ) );
		if( k ==0 ){
			DaoParser_Error( self, DAO_TOKEN_NOT_FOUND, self->string );
		}else{
			DString_AppendChar( self->string, ' ' );
			DString_AppendChars( self->string, DaoToken_NameToString( rw ) );
			DaoParser_Error( self, DAO_TOKENS_NOT_PAIRED, self->string );
		}
	}
	return -100;
}
static int DaoParser_FindPairToken2( DaoParser *self,  uchar_t l, uchar_t r, int m, int n )
{
	DaoToken **tokens = self->tokens->items.pToken;
	if( tokens[m]->name != l ){
		DaoParser_Error( self, DAO_TOKEN_NOT_FOUND, self->string );
		return -1;
	}
	return DaoParser_FindPairToken( self, l, r, m, n );
}

static int DaoParser_PushRegister( DaoParser *self );
static DaoInode* DaoParser_AddCode( DaoParser *self, ushort_t code, ushort_t a, ushort_t b, ushort_t c );

static int DaoClass_BaseCstrOffset( DaoClass *self, DaoClass *base, int idx )
{
	daoint j, offset = 0;
	if( idx < self->mixinBases->size ){
		for(j=0; j<self->mixins->size; ++j){
			if( self->mixins->items.pClass[j] == base ){
				offset = self->ranges->data.ushorts[6*j] + DAO_CLASS_CONST_CSTOR;
				break;
			}
		}
	}else{
		offset = self->cstParentStart;
		offset += (base->type == DAO_CLASS ? DAO_CLASS_CONST_CSTOR : 0);
	}
	return offset;
}
static int DaoParser_AddDefaultInitializer( DaoParser *self, DaoClass *klass, int flags )
{
	daoint i, j;
	DaoParser_PushTokenIndices( self, 0, 0, 0 );
	for(i=0; i<klass->allBases->size; i++){
		DaoClass *base = (DaoClass*) klass->allBases->items.pValue[i];
		DaoCdata *cdata = (DaoCdata*) klass->allBases->items.pValue[i];
		DaoInode *inode;
		int reg, opb = 0;
		if( flags & (1<<i) ) continue;
		inode = DaoParser_AddCode( self, DVM_GETCK, 1, 0, 0 );
		inode->b = DaoClass_BaseCstrOffset( klass, base, i );
		inode->c = DaoParser_PushRegister( self );
		reg = DaoParser_PushRegister( self );
		DaoParser_AddCode( self, DVM_CALL, inode->c, DAO_CALL_INIT, reg );
	}
	DaoParser_PopTokenIndices( self, 1 );
	return 1;
}

static DaoValue* DaoParser_GetVariable( DaoParser *self, int reg );
static int DaoParser_GetRegister( DaoParser *self, DaoToken *name );

int DaoParser_ParseMaybeScopeConst( DaoParser *self, DaoValue **scope, DaoValue **value, int start, int stop, int type )
{
	DaoEnode enode;
	int count = self->errors->size;

	self->curToken = start;
	enode = DaoParser_ParsePrimary( self, stop, type );
	if( scope ) *scope = enode.scope ? DaoParser_GetVariable( self, enode.scope ) : NULL;
	if( value ) *value = enode.konst ? DaoParser_GetVariable( self, enode.konst ) : NULL;
	if( enode.reg < 0 || (enode.scope == 0 && enode.konst == 0) ){
		if( type & DAO_EXPRLIST_SCOPE ){
			DList_Erase( self->errors, count, -1 );
			return start;
		}
		DaoParser_Error3( self, DAO_EXPR_NEED_CONST_EXPR, start );
		return -1;
	}
	if( self->curToken >= self->tokens->size ) return self->curToken;
	if( enode.konst == 0 && self->tokens->items.pToken[self->curToken]->type == DTOK_COLON2 ){
		return self->curToken + 1;
	}
	return self->curToken - 1;
}
int DaoParser_FindScopeAndConst( DaoParser *self, DaoValue **scope, DaoValue **value, int start, int stop, int type )
{
	int pos = DaoParser_ParseMaybeScopeConst( self, scope, value, start, stop, type );
	if( pos < 0 ) return pos;
	if( *scope == NULL || *value == NULL ){
		DaoParser_Error3( self, DAO_EXPR_NEED_CONST_EXPR, start );
		return -1;
	}
	return pos;
}
int DaoParser_FindMaybeScopedConst( DaoParser *self, DaoValue **value, int start, int stop )
{
	DaoValue *scope = NULL;
	int pos = DaoParser_ParseMaybeScopeConst( self, & scope, value, start, stop, 0 );
	if( pos < 0 ) return pos;
	if( *value == NULL ){
		DaoParser_Error3( self, DAO_EXPR_NEED_CONST_EXPR, start );
		return -1;
	}
	return pos;
}
int DaoParser_ParseScopedConstOrName( DaoParser *self, DaoValue **scope, DaoValue **value, int start, int stop )
{
	return DaoParser_ParseMaybeScopeConst( self, scope, value, start, stop, DAO_EXPRLIST_SCOPE );
}

static int DaoParser_ParseInitSuper( DaoParser *self, DaoParser *module, int start )
{
	DaoLexer *init = NULL;
	DaoRoutine *routine = module->routine;
	DaoClass *klass = module->hostClass;
	DaoToken **tokens = self->tokens->items.pToken;
	daoint i, size = self->tokens->size;
	int isconstru = klass && DString_EQ( routine->routName, klass->className );
	int triples = self->tokenTriples->size;
	int line = 0, flags = 0; /* XXX number of super classes */
	int dlm = start;
	int rb = 0;
	if( isconstru == 0 ) return start;
	init = DaoLexer_New();
	if( tokens[start]->name == DTOK_COLON ){
		do {
			DaoEnode enode;
			DaoInode *inode;
			DaoValue *value = NULL;
			DaoLexer *lexer = module->lexer;
			DaoInode *back = self->vmcLast;
			int pos = DaoParser_FindMaybeScopedConst( self, & value, dlm+1, DTOK_LB );
			int reg, count, found = -1;

			DaoParser_PopCodes( self, back );
			if( value == NULL ) goto ErrorRoutine;
			if( value->type != DAO_CLASS && value->type != DAO_CTYPE ) goto ErrorRoutine;
			if( pos < 0 || tokens[pos+1]->type != DTOK_LB ) goto ErrorRoutine;

			for(i=0; i<klass->allBases->size; ++i){
				if( value == klass->allBases->items.pValue[i] ){
					found = i;
					break;
				}
			}
			if( found < 0 ) goto ErrorRoutine;
			flags |= 1<<found;

			rb = DaoParser_FindPairToken( self, DTOK_LB, DTOK_RB, dlm, -1 );
			if( rb < 0 ) goto ErrorRoutine;

			DaoParser_PushTokenIndices( self, dlm+1, dlm+1, dlm+1 );
			inode = DaoParser_AddCode( module, DVM_GETCK, 1, 0, 0 );
			inode->b = DaoClass_BaseCstrOffset( klass, (DaoClass*) value, found );
			inode->c = DaoParser_PushRegister( module );
			inode->first = inode->middle = start + 1;
			inode->last = pos - 1;

			if( rb > (pos + 2) ){
				module->lexer = self->lexer;
				module->tokens = self->lexer->tokens;
				module->curToken = pos + 2;
				enode = DaoParser_ParseExpressionList( module, DTOK_COMMA, inode, NULL );
				module->lexer = lexer;
				module->tokens = lexer->tokens;
				if( enode.reg < 0 ) goto ErrorRoutine;
			}else{
				enode.reg = inode->c;
				enode.count = 1;
			}

			DaoParser_PushTokenIndices( self, dlm+1, pos+1, rb );
			reg = DaoParser_PushRegister( module );
			inode = DaoParser_AddCode( module, DVM_CALL, enode.reg, 0, reg );
			inode->b = (enode.count-1)|DAO_CALL_INIT;
			dlm = rb + 1;
		} while( dlm < size && tokens[dlm]->name == DTOK_COMMA );
		start = dlm;
		if( tokens[start]->name != DTOK_LCB ) goto ErrorRoutine;
	}
	if( tokens[start]->name == DTOK_LCB ){
		DaoParser_AddDefaultInitializer( module, klass, flags );
	}
	DaoParser_PopTokenIndices( self, (self->tokenTriples->size - triples)/3 );
	if( init ) DaoLexer_Delete( init );
	return start;
ErrorRoutine:
	DaoParser_PopTokenIndices( self, (self->tokenTriples->size - triples)/3 );
	if( init ) DaoLexer_Delete( init );
	return -1;
}

static int DaoParser_PushRegister( DaoParser *self )
{
	int line, reg = self->regCount;
	self->regCount += 1;
	if( self->routine == NULL || self->routine->body == NULL ) return reg;
	line = self->curLine - self->routine->body->codeStart - 1;
	return reg;
}
static int DaoParser_PushRegisters( DaoParser *self, int n )
{
	int i, line, reg = self->regCount;
	self->regCount += n;
	if( self->routine == NULL || self->routine->type != DAO_ROUTINE ) return reg;
	line = self->curLine - self->routine->body->codeStart - 1;
	return reg;
}
static void DaoParser_PopRegister( DaoParser *self )
{
	self->regCount --;
	MAP_Erase( self->routine->body->localVarType, self->regCount );
}
static void DaoParser_PopRegisters( DaoParser *self, int n )
{
	int i;
	if( n <0 ) return;
	for(i=0; i<n; i++){
		MAP_Erase( self->routine->body->localVarType, self->regCount - i - 1 );
	}
	self->regCount -= n;
}
static void DaoParser_Restore( DaoParser *self, DaoInode *back, int regCount )
{
	DaoLexer_Reset( self->elexer );
	DaoLexer_Reset( self->wlexer );
	DaoParser_PopCodes( self, back );
	DaoParser_PopRegisters( self, self->regCount - regCount );
}

void DaoType_MapNames( DaoType *self );
DaoType* DaoParser_ParseTypeItems( DaoParser *self, int start, int end, int valtype, DList *types );
static int DaoParser_MakeArithTree( DaoParser *self, int start, int end, int *cst );

static DaoType* DaoParser_ParseCodeBlockType( DaoParser *self, int start, int *next )
{
	DaoType *type;
	DaoNamespace *ns = self->nameSpace;
	DList *types = self->typeItems;
	int tcount = types->size;
	int ecount = self->errors->size;
	int rb = DaoParser_FindPairToken( self, DTOK_LSB, DTOK_RSB, start, -1 );
	if( rb < 0 ) return NULL;
	type = DaoParser_ParseTypeItems( self, start+1, rb-1, 0, types );
	if( self->errors->size > ecount ) return NULL;
	*next = rb + 1;
	type = DaoNamespace_MakeType( ns, "", DAO_CODEBLOCK, (DaoValue*) type, types->items.pType + tcount, types->size - tcount );
	types->size = tcount;
	return type;
}

static DaoType* DaoParser_MakeVarTypeHolder( DaoParser *self )
{
	DaoType *type = DaoType_New( "@X", DAO_THT, NULL, NULL );
	DList_Append( self->nameSpace->auxData, type );
	return type;
}
static DaoType* DaoParser_MakeParTypeHolder( DaoParser *self, DString *name )
{
	DaoType *type = DaoType_New( "@", DAO_THT, NULL, NULL );
	DString_Append( type->name, name );
	DMap_Insert( self->initTypes, type->name, type );
	return type;
}
DaoToken* DaoToken_Copy( DaoToken *self );

static int DaoParser_ParseDecoTargets( DaoParser *self, int start, int to, DList *targets )
{
	DaoToken **tks = self->tokens->items.pToken;
	int i = start;
	while( i <= to && (tks[i]->name == DTOK_IDENTIFIER || tks[i]->name == DTOK_TILDE) ){
		DString *pat = (DString*) DList_PushBack( targets, & tks[i]->string );
		int expecting = tks[i]->name == DTOK_IDENTIFIER ? DTOK_TILDE : DTOK_IDENTIFIER;
		int expecting2 = tks[i]->name == DTOK_IDENTIFIER ? DTOK_IDENTIFIER : 0;
		if( ++i <= to && tks[i]->name == expecting ){
			DString_Append( pat, & tks[i]->string );
			if( ++i <= to && tks[i]->name == expecting2 ){
				DString_Append( pat, & tks[i++]->string );
			}
		}
		if( tks[i]->name != DTOK_COMMA ) break;
	}
	if( tks[i]->name != DTOK_LCB ) return -1;
	return i;
}
static int DaoParser_ExtractRoutineBody( DaoParser *self, DaoParser *parser, int left )
{
	DaoRoutine *routine = parser->routine;
	DaoToken **tokens = self->tokens->items.pToken;
	int i, right = DaoParser_FindPairToken( self, DTOK_LCB, DTOK_RCB, left, -1 );
	if( right < 0 ) return -1;

	DList_Append( routine->nameSpace->definedRoutines, routine );
	routine->body->codeStart = tokens[left]->line;
	routine->body->codeEnd = tokens[right]->line;
	for(i=left+1; i<right; ++i) DaoLexer_AppendToken( parser->lexer, tokens[i] );
	return right;
}
static DaoInode* DaoParser_AddCode( DaoParser *self, ushort_t code, ushort_t a, ushort_t b, ushort_t c );

int DaoParser_ParseSignature( DaoParser *self, DaoParser *module, int start )
{
	DNode *node;
	DaoToken **tokens = self->tokens->items.pToken;
	DaoToken *tk, *tok, *nameTok = tokens[start];
	DaoNamespace *NS = self->nameSpace;
	DaoInterface *inter = module->hostInter;
	DaoRoutine *routine = module->routine;
	DaoClass  *klass = module->hostClass;
	DaoCtype  *ctype = module->hostCtype;
	DaoType *hostype = module->hostType;
	DaoType *type, *type_default, *cast = NULL;
	DaoType *cbtype = NULL, *retype = NULL;
	DList *types = NULL, *nested = NULL;
	DString *hostname = NULL;
	DString *pname = NULL;
	DString *mbs = NULL;
	int invarhost = routine->routHost && DaoType_IsImmutable( routine->routHost );
	int size = self->tokens->size;
	int i, j, k, right, invarpar = 0;
	int line = 0; /* XXX number of super classes */
	int e1=start, e2=size-1, ec = 0;
	int isMeth, notStatic, notConstr;
	int hasdeft = 0, selfpar = 0;
	int isconstru = klass != NULL;
	int lb = 0, offset = 0;

	DString_Assign( routine->routName, & nameTok->string );
	DString_Assign( module->fileName, self->fileName );
	GC_Assign( & routine->nameSpace, self->nameSpace );
	module->nameSpace = self->nameSpace;
	self->returnType = NULL;
	if( start + 2 >= size ) return -1;

	start += 1;
	if( tokens[start-1]->name == DTOK_LB ){ /* operator () or operator (type) */
		int rb = start;
		if( tokens[start]->name != DTOK_RB ){
			rb = DaoParser_FindPairToken( self, DTOK_LB, DTOK_RB, start-1, -1 );
			cast = DaoParser_ParseType( self, start, rb-1, & rb, NULL );
			if( cast == NULL || tokens[rb]->name != DTOK_RB ) goto ErrorInvalidOperator;
		}
		lb = DaoParser_FindOpenToken( self, DTOK_LB, rb+1, -1, 1 );
	}else if( tokens[start-1]->name == DTOK_LSB ){ /* operator [] */
		if( tokens[start]->name != DTOK_RSB ) goto ErrorInvalidOperator;
		lb = DaoParser_FindOpenToken( self, DTOK_LB, start+1, -1, 1 );
	}else if( tokens[start-1]->type != DTOK_IDENTIFIER ){
		lb = DaoParser_FindOpenToken( self, DTOK_LB, start, -1, 1 );
	}else if( tokens[start]->name == DTOK_LT ){ /* constructor of template types */
		int lb = DaoParser_FindPairToken( self, DTOK_LT, DTOK_GT, start, -1 );
		if( lb < 0 ) return -1;
		for(i=start; i<=lb; i++) DString_Append( routine->routName, & tokens[i]->string );
		start = lb + 1;
	}
	if( lb < 0 ) goto ErrorInvalidOperator;
	if( lb > start ){
		for(i=start; i<lb; i++) DString_Append( routine->routName, & tokens[i]->string );
		start = lb;
	}

	if( klass ) isconstru &= DString_EQ( routine->routName, klass->className );

	if( tokens[start]->name != DTOK_LB ) return -1;
	right = DaoParser_FindPairToken( self, DTOK_LB, DTOK_RB, start, -1 );
	if( right < 0 ) return -1;

	if( module->hostType ) hostname = module->hostType->name;

	mbs = DaoParser_GetString( self );
	pname = DaoParser_GetString( self );
	nested = DaoParser_GetArray( self );
	if( nameTok->name == DTOK_ID_THTYPE && isconstru == 0 ){
		routine->attribs |= DAO_ROUT_DECORATOR;
		DString_AppendChar( pname, '@' );
	}
	DString_AppendChars( pname, "routine<" );
	routine->parCount = 0;
	if( tokens[start+1+(tokens[start+1]->name==DKEY_INVAR)]->name == DKEY_SELF ) selfpar = 1;

	isMeth = klass && routine != klass->initRoutine;
	notStatic = (routine->attribs & DAO_ROUT_STATIC) ==0;
	notConstr = hostname && DString_EQ( routine->routName, hostname ) == 0;
	if( routine->routHost && hostname && DString_EQ( routine->routName, hostname ) ){
		routine->attribs |= DAO_ROUT_INITOR;
	}
	if( (isMeth || inter || module->hostCinType) && selfpar == 0 && notStatic && notConstr ){
		if( invarhost ) routine->attribs |= DAO_ROUT_INVAR;
		type = hostype;
		if( routine->attribs & DAO_ROUT_INVAR ) type = DaoType_GetInvarType( type );
		type = DaoNamespace_MakeType( NS, "self", DAO_PAR_NAMED, (DaoValue*)type, NULL, 0 );
		DList_Append( nested, (void*) type ); /* self parameter type */
		DaoRoutine_AddConstant( routine, NULL ); /* no default parameter; */
		DString_AppendChars( pname, type->name->chars );
		if( routine->body ){
			tok = DList_Append( routine->body->defLocals, tokens[start] );
			DaoToken_Set( tok, 1, 0, routine->parCount, "self" );
			MAP_Insert( routine->body->localVarType, module->regCount, type );
		}
		DString_SetChars( mbs, "self" );
		MAP_Insert( DaoParser_CurrentSymbolTable( module ), mbs, module->regCount );
		DaoParser_PushRegister( module );
		routine->parCount ++;
		selfpar = 1;
	}
	offset = pname->size;
	if( selfpar ) routine->attribs |= DAO_ROUT_PARSELF;
	DMap_Clear( module->initTypes );
	if( hostype ) DaoType_GetTypeHolders( hostype, module->initTypes );
	self->innerParser = module;
	type = NULL;
	i = start + 1;
	while( i < right ){
		unsigned char tki, tki2;
		int comma, regCount = module->regCount;
		DaoInode *back = self->vmcLast;
		DaoValue *dft = NULL;
		DString *tks = NULL;

		if( nested->size == 1 ) offset = pname->size;

		e1 = i;
		e2 = right;
		module->curLine = self->curLine = tokens[i]->line;
		tks = & tokens[i]->string;
		tki = tokens[i]->name;
		tki2 = tokens[i+1]->name;
		if( tokens[i]->name == DKEY_INVAR ){
			invarpar = 1;
			i += 1;
			continue;
		}
		if( tokens[i]->type == DTOK_IDENTIFIER ){
			/*
			   printf( "name = %s; reg = %i\n", tokens[i]->string.chars, module->regCount );
			 */
			if( routine->body ){
				tk = DList_Append( routine->body->defLocals, tokens[i] );
				DaoToken_Set( tk, 1, 0, routine->parCount, NULL );
			}
			if( nested->size == 0 && tki == DKEY_SELF ){
				routine->attribs |= DAO_ROUT_PARSELF;
			}
			MAP_Insert( DaoParser_CurrentSymbolTable( module ), tks, module->regCount );
			DaoParser_PushRegister( module );
			routine->parCount ++;
			if( (routine->attribs & DAO_ROUT_DECORATOR) && i == start + 1 ){
				module->invarDecoArg = 0;
				if( i+3 >= right ) goto ErrorInvalidDecoParam;
				if( tokens[i+1]->name != DTOK_LB ) goto ErrorInvalidDecoParam;
				if( tokens[i+2]->name == DKEY_INVAR ){
					module->invarDecoArg = 1;
					i += 1;
				}
				if( tokens[i+2]->name != DTOK_IDENTIFIER ) goto ErrorInvalidDecoParam;
				if( tokens[i+3]->name != DTOK_RB ) goto ErrorInvalidDecoParam;
				module->decoArgName = DaoToken_Copy( tokens[i+2] );
				tki2 = tokens[i+4]->name;
				i += 3;
			}
		}
		if( type && type->tid == DAO_PAR_VALIST ){
			e1 = i;  e2 = right;
			goto ErrorMiddleValist;
		}

		type_default = type = NULL;
		if( tki == DTOK_DOTS ){
			routine->parCount = DAO_MAX_PARAM;
			module->regCount = DAO_MAX_PARAM;
			i += 1;
			if( tki2 == DTOK_COLON ){
				if( i+1 >= right || tokens[i+1]->type != DTOK_IDENTIFIER ) goto ErrorNeedType;
				type = DaoParser_ParseType( self, i+1, right-1, &i, NULL );
				if( type == NULL ) goto ErrorParamParsing;
			}
			if( invarpar ){
				if( type == NULL ) type = dao_type_any;
				type = DaoType_GetInvarType( type );
			}
			type = DaoNamespace_MakeType( NS, "...", DAO_PAR_VALIST, (DaoValue*)type, NULL, 0 );
		}else if( tki == DTOK_ID_THTYPE ){
			type = DaoParser_ParseType( self, i, right-1, &i, NULL );
			if( type == NULL ) goto ErrorInvalidParam;
			type = DaoNamespace_GetType( NS, (DaoValue*) type );
		}else if( tki2 == DTOK_COLON || tki2 == DTOK_ASSN ){
			i ++;
			if( tki2 == DTOK_COLON ){
				if( i+1 >= right || tokens[i+1]->type != DTOK_IDENTIFIER ) goto ErrorNeedType;
				type = DaoParser_ParseType( self, i+1, right-1, &i, NULL );
				if( type == NULL ) goto ErrorParamParsing;
				if( tki == DKEY_SELF && invarhost ) type = DaoType_GetInvarType( type );
			}
			if( tokens[i]->type == DTOK_ASSN ){
				int reg=1, cst = 0;
				hasdeft = i;
				if( i+1 >= right || tokens[i+1]->name == DTOK_COMMA ) goto ErrorNeedDefault;
				comma = DaoParser_FindOpenToken( self, DTOK_COMMA, i, -1, 0 );
				if( comma < 0 ) comma = right;
				e1 = i + 1;
				e2 = comma - 1;
#if 0
				printf( "cst = %i;  reg = %i, %s\n", cst, reg, type?type->name->chars:"" );
				for(j=i+1; j<comma; j++) printf( "%s\n", tokens[j]->string.chars );
#endif
				self->needConst += 1;
				DList_PushFront( self->enumTypes, type );
				reg = DaoParser_MakeArithTree( self, i+1, comma-1, & cst );
				DList_PopFront( self->enumTypes );
				self->needConst -= 1;
				if( reg < 0 ) goto ErrorInvalidDefault;
				if( cst ){
					dft = DaoParser_GetVariable( self, cst );
					type_default = DaoNamespace_GetType( NS, dft );
				}else if( module->uplocs ){
					int loc = routine->routConsts->value->size;
					DArray_PushInt( module->uplocs, reg );
					DArray_PushInt( module->uplocs, loc );
					DArray_PushInt( module->uplocs, i+1 );
					DArray_PushInt( module->uplocs, comma-1 );
					type_default = DaoParser_MakeParTypeHolder( module, tks );
				}else{
					goto ErrorVariableDefault;
				}
				if( type_default == NULL ) goto ErrorInvalidDefault;
				if( cst && type && DaoType_MatchValue( type, dft, NULL ) ==0 ) goto ErrorImproperDefault;
				if( type == NULL ) type = type_default;
				i = comma;
			}
		}else if( tokens[i]->type == DTOK_IDENTIFIER ){
			type = DaoParser_MakeParTypeHolder( module, tks );
			i += 1;
		}else{
			goto ErrorInvalidParam;
		}
		if( hasdeft && dft == NULL && type_default == NULL && module->outerParser == NULL ){
			e1 = hasdeft;  e2 = right;
			goto ErrorMiddleDefault;
		}
		if( nameTok->name == DTOK_ID_THTYPE ){
			if( nested->size == selfpar && type->tid != DAO_ROUTINE ) goto ErrorInvalidParam;
		}
		if( invarhost && (routine->attribs & DAO_ROUT_INITOR) ){
			DaoType *type2 = type;
			if( type2->tid == DAO_PAR_VALIST ) type2 = (DaoType*) type2->aux;
			if( DaoType_IsPrimitiveOrImmutable( type2 ) == 0 ) goto ErrorParamParsing2;
		}

		if( invarpar && type->tid != DAO_PAR_VALIST ) type = DaoType_GetInvarType( type );
		if( routine->body ) MAP_Insert( routine->body->localVarType, regCount, type );
		if( type->tid != DAO_PAR_VALIST ){
			j = type_default ? DAO_PAR_DEFAULT : DAO_PAR_NAMED;
			type = DaoNamespace_MakeType( NS, tks->chars, j, (DaoValue*) type, NULL, 0 );
		}
		invarpar = 0;

		DList_Append( nested, (void*) type );
		DaoRoutine_AddConstant( routine, dft );
		k = pname->size >0 ? pname->chars[pname->size-1] : 0;
		if( k !='<' ) DString_AppendChars( pname, "," );
		DString_AppendChars( pname, type->name->chars );
		if( module->outerParser == NULL ) DaoParser_PopCodes( self, back );

		if( i >= right ) break;
		if( tokens[i]->name == DKEY_AS ){
			module->invarArg = 0;
			if( (i+1) < right && tokens[i+1]->name == DKEY_INVAR ){
				module->invarArg = 1;
				i += 1;
			}
			if( (i+1) >= right || tokens[i+1]->type != DTOK_IDENTIFIER ) goto ErrorParamParsing;
			if( module->argName ) goto ErrorParamParsing; // TODO: error, duplicate "as";
			module->argName = DaoToken_Copy( tokens[i+1] );
			i += 2;
			if( i < right ) goto ErrorParamParsing;
		}else if( tokens[i]->name != DTOK_COMMA ){
			goto ErrorParamParsing;
		}
		i ++;
	}
	if( nested->size == 1 ) offset = pname->size;
	if( routine->parCount > DAO_MAX_PARAM ) goto ErrorTooManyParams;

	e1 = right + 1;
	e2 = size - 1;
	if( right+1 < size && tokens[right+1]->name == DTOK_LSB ){
		cbtype = DaoParser_ParseCodeBlockType( self, right+1, & start );
		if( cbtype == NULL ) goto ErrorInvalidTypeForm;
		right = start - 1;
	}
	if( right+1 < size && tokens[right+1]->name == DTOK_FIELD ){
		e1 += 1;
		start = right + 1;
		if( routine->attribs & DAO_ROUT_DECORATOR ) goto ErrorInvalidReturn;
		if( isconstru ) goto ErrorConstructorReturn; /* class constructor should not return a value */
		retype = DaoParser_ParseType( self, start + 1, self->tokens->size-1, & right, NULL );
		if( retype == NULL ) goto ErrorInvalidTypeForm;
		right -= 1;
	}
	self->innerParser = NULL;

	k = pname->size;
	if( cast != NULL ){
		DaoType *tt;
		if( nested->size > (selfpar+1) ) goto ErrorTooManyParams;
		if( retype != NULL ) goto ErrorInvalidReturn;
		retype = cast;
		tt = DaoNamespace_GetType( NS, (DaoValue*) cast );
		DString_InsertChar( pname, ',', offset + 1 );
		DString_Insert( pname, tt->name, offset + 1, 0, -1 );
		DList_Insert( routine->routConsts->value, NULL, 1 );
		DList_Insert( nested, (void*) tt, 1 );
		DList_Append( NS->auxData, (void*) tt );
		DaoRoutine_AddConstant( routine, NULL );
		DaoParser_PushRegister( module );
		routine->parCount ++;
	}
	if( notConstr == 0 ){
		if( klass && routine->routHost == klass->objType ){
			retype = klass->objType;
		}else if( ctype && routine->routHost == ctype->cdtype ){
			retype = ctype->cdtype;
		}else if( inter && routine->routHost == inter->abtype ){
			retype = inter->abtype;
		}
	}
	if( retype == NULL ){
		if( routine->body == NULL ){
			retype = DaoNamespace_MakeValueType( NS, dao_none_value );
		}else{
			DString_Assign( mbs, routine->routName );
			if( isalpha( mbs->chars[0] ) == 0 ) DString_SetChars( mbs, "X" );
			retype = DaoParser_MakeParTypeHolder( self, mbs );
		}
	}
	if( invarhost && DaoType_IsPrimitiveOrImmutable( retype ) == 0 ){
		retype = DaoType_GetInvarType( retype );
	}
	DString_AppendChars( pname, "=>" );
	DString_Append( pname, retype->name );
	DString_AppendChars( pname, ">" );
	type = DaoType_New( pname->chars, DAO_ROUTINE, (DaoValue*) retype, nested );
	DList_Append( NS->auxData, type );
	if( cbtype ){
		GC_Assign( & type->cbtype, cbtype );
		type->attrib |= DAO_TYPE_CODESECT;
		routine->attribs |= DAO_ROUT_CODESECT;
		DString_Append( type->name, cbtype->name );
		DString_Append( pname, cbtype->name );
	}
	assert( routine->routType == NULL );
	routine->routType = DaoNamespace_FindType( NS, pname );
	if( DaoType_MatchTo( type, routine->routType, NULL ) != DAO_MT_EQ ){
		routine->routType = type;
		DaoNamespace_AddType( NS, pname, routine->routType );
		DString_SetChars( mbs, "self" );
		node = MAP_Find( routine->routType->mapNames, mbs );
		if( node && node->value.pInt == 0 ) routine->routType->attrib |= DAO_TYPE_SELF;
	}
#if 0
	printf( "%i  %s\n", routine->parCount, routine->routType->name->chars );
	for(j=0; j<nested->size; j++) printf( "%s\n", nested->items.pType[j]->name->chars );
#endif
	GC_IncRC( routine->routType );

	/*
	// The casting methods should be organized into a single overloaded method,
	// for two reasons:
	// 1. Searching casting methods by type names may be unreliable;
	// 2. Casting similar types can be supported in this way; For example,
	//    if "operator(float)" is defined, casting to "int" will be allowed.
	 */
	if( cast != NULL ){
		routine->attribs |= DAO_ROUT_CASTOR;
		if( ctype ) DaoTypeKernel_InsertCastor( hostype->kernel, NS, hostype, routine );
		else if( klass ) DaoClass_CastingMethod( klass, routine );
	}

	/*  remove vmcode for consts */
	DaoParser_ClearCodes( module );
	/* one parser might be used to compile multiple C functions: */
	if( routine->body == NULL ) DMap_Reset( module->allConsts );

	/* Resever enough space for default parameters for function decoration: */
	if( (routine->attribs & DAO_ROUT_DECORATOR) && routine->routConsts->value->size < DAO_MAX_PARAM )
		DList_Resize( routine->routConsts->value, DAO_MAX_PARAM, NULL );

	if( routine->body == NULL || right+1 >= size ) return right;

	if( isconstru ){
		right = DaoParser_ParseInitSuper( self, module, right + 1 );
		if( right <0 ) return -1;
		right --;
	}else if( tokens[right+1]->name == DTOK_COLON ){
		goto ErrorRoutine;
	}
	right += 1;
	if( tokens[right]->name == DKEY_FOR ){
		DaoRoutineBody *body = routine->body;
		ec = DAO_INVALID_DECO_PATTERN;
		if( !(routine->attribs & DAO_ROUT_DECORATOR) ) goto ErrorRoutine;
		if( body->decoTargets == NULL ) body->decoTargets = DList_New( DAO_DATA_STRING );
		right = DaoParser_ParseDecoTargets( self, right+1, size-1, body->decoTargets );
		if( right < 0 ) goto ErrorRoutine;
		ec = 0;
	}
	if( tokens[right]->name != DTOK_LCB ) return right - 1;

	if( module->hostCinType && selfpar ){
		int b = DaoRoutine_AddConstant( routine, (DaoValue*) module->hostCinType->target );
		DString_SetChars( mbs, "self" );
		MAP_Insert( DaoParser_CurrentSymbolTable( module ), mbs, module->regCount );
		DaoParser_AddCode( module, DVM_CAST, 0, b, module->regCount );
		DaoParser_PushRegister( module );
	}

	start = right;
	e2 = start;
	if( tokens[start]->name == DTOK_LCB ){
		right = DaoParser_ExtractRoutineBody( self, module, start );
		if( right < 0 ) goto ErrorRoutine;
	}
	return right;
ErrorInvalidOperator: ec = DAO_ROUT_INVALID_OPERATOR; goto ErrorRoutine;
ErrorConstructorReturn: ec = DAO_ROUT_INVALID_RETURN; goto ErrorRoutine;
ErrorNeedReturnType:  ec = DAO_ROUT_NEED_RETURN_TYPE; goto ErrorRoutine;
ErrorInvalidDecoParam:   ec = DAO_ROUT_INVALID_DECO_PARAM; goto ErrorRoutine;
ErrorInvalidTypeForm: ec = DAO_INVALID_TYPE_FORM; goto ErrorRoutine;
ErrorTooManyParams:  ec = DAO_PARAM_TOO_MANY; goto ErrorRoutine;
ErrorInvalidParam:   ec = DAO_PARAM_INVALID; goto ErrorRoutine;
ErrorNeedSeparator:  ec = DAO_PARAM_NEED_SEPARATOR; goto ErrorRoutine;
ErrorMiddleValist:   ec = DAO_PARAM_MIDDLE_VALIST; goto ErrorRoutine;
ErrorRedundantType:  ec = DAO_PARAM_REDUNDANT_TYPE; goto ErrorRoutine;
ErrorNeedType:       ec = DAO_PARAM_NEED_TYPE;    goto ErrorRoutine;
ErrorNeedDefault:    ec = DAO_PARAM_NEED_DEFAULT; goto ErrorRoutine;
ErrorInvalidDefault: ec = DAO_PARAM_INVALID_DEFAULT; goto ErrorRoutine;
ErrorMiddleDefault: ec = DAO_PARAM_MIDDLE_DEFAULT; goto ErrorRoutine;
ErrorVariableDefault: ec = DAO_PARAM_VARIABLE_DEFAULT; goto ErrorRoutine;
ErrorImproperDefault: ec = DAO_PARAM_IMPROPER_DEFAULT; goto ErrorRoutine;
ErrorInvalidReturn:  ec = DAO_PARAM_INVALID_RETURN; goto ErrorRoutine;
ErrorParamParsing2: ec = DAO_PARAM_INVALID_MUTABLE; goto ErrorRoutine;
ErrorParamParsing: ec = DAO_PARAM_INVALID;
ErrorRoutine:
	if( ec ){
		if( e2 >= size ) e2 = size - 1;
		DString_Clear( self->string );
		for(i=e1; i<=e2; i++){
			if( tokens[i]->type == tokens[i-1]->type ) DString_AppendChar( self->string, ' ' );
			DString_Append( self->string, & tokens[i]->string );
			if( self->string->size > 20 ) break;
		}
		DaoParser_Error( self, ec, self->string );
	}
	return -1;
}

static void DaoParser_ByteEncodeGetConst( DaoParser *self, DString *name )
{
	DaoByteBlock *block = self->byteBlock;
	DaoNamespace *ns = self->nameSpace;
	DaoValue *value = NULL;
	int id, opcode = 0;
	if( self->hostClass && (id = DaoClass_FindConst( self->hostClass, name )) >= 0 ){
		value = DaoClass_GetConst( self->hostClass, id );
		opcode = DVM_GETCK;
	}else if( (id = DaoNamespace_FindConst( ns, name )) >= 0 ){
		value = DaoNamespace_GetConst( ns, id );
		opcode = DVM_GETCG;
	}
	if( value != NULL && DaoByteBlock_FindObjectBlock( block, value ) == NULL ){
		DaoByteBlock *namebk = DaoByteBlock_EncodeString( block, name );
		DaoByteBlock *eval = DaoByteBlock_AddEvalBlock( block, value, opcode, 1, 0, NULL );
		DaoByteBlock_InsertBlockIndex( eval, eval->end, namebk );
	}
}
static int DaoParser_ParseAtomicExpression( DaoParser *self, int start, int *cst );
static DaoType* DaoParser_ParseValueType( DaoParser *self, int start )
{
	DaoValue *value;
	int cst = 0;
	self->needConst += 1;
	DaoParser_ParseAtomicExpression( self, start, & cst );
	self->needConst -= 1;
	if( cst ==0 ) return NULL;
	value = DaoParser_GetVariable( self, cst );
	return DaoNamespace_MakeValueType( self->nameSpace, value );
}

static int type_stop_tokens[] = { DTOK_LB, DTOK_LSB, DTOK_LCB };

static DaoType* DaoParser_ParseUserType( DaoParser *self, int start, int end, int *newpos )
{
	DaoType *type = NULL;
	DaoValue *value = NULL;
	DaoInode *back = self->vmcLast;
	DaoToken **tokens = self->tokens->items.pToken;
	DaoNamespace *ns = self->nameSpace;
	DString *name = & tokens[start]->string;
	int i, k, t, min = end, stop = tokens[end]->name;
	for(i=0; i<3; ++i){
		int pos = DaoParser_FindOpenToken( self, type_stop_tokens[i], start, min, 0 );
		if( pos >= 0 && pos < min ){
			stop = type_stop_tokens[i];
			min = pos;
		}
	}
	k = DaoParser_FindMaybeScopedConst( self, &value, start, stop );
	if( self->byteBlock && k == start ){
		DaoParser_ByteEncodeGetConst( self, name );
	}
	DaoParser_PopCodes( self, back );
	if( k <0 ) k = start;
	*newpos = k + 1;
	switch( value ? value->type : 0 ){
	case DAO_CLASS : type = value->xClass.objType; break;
	case DAO_CTYPE : type = value->xCtype.cdtype; break; /* get its cdata type */
	case DAO_TYPE  : type = & value->xType; break;
	case DAO_CINTYPE : type = value->xCinType.vatype; break;
	case DAO_INTERFACE : type = value->xInterface.abtype; break;
	default : break;
	}
	if( type ) return type;
	if( value == NULL ) return NULL;
	return DaoNamespace_MakeValueType( ns, value );
}
static DaoType* DaoParser_FindTypeHolder( DaoParser *self, DString *name )
{
	DNode *it = NULL;
	if( self->innerParser ) it = DMap_Find( self->innerParser->initTypes, name );
	if( it == NULL ) it = DMap_Find( self->initTypes, name );
	if( it ) return it->value.pType;
	if( self->outerParser ) return DaoParser_FindTypeHolder( self->outerParser, name );
	return NULL;
}
static DaoType* DaoParser_ParsePlainType( DaoParser *self, int start, int end, int *newpos )
{
	DaoType *type = NULL;
	DaoNamespace *ns = self->nameSpace;
	DaoClass *klass = self->hostClass;
	DaoRoutine *routine = self->routine;
	DaoToken **tokens = self->tokens->items.pToken;
	DaoToken *token = tokens[start];
	DString *name = & token->string;
	int i = token->name > DKEY_LOAD ? dao_keywords[ token->name - DKEY_LOAD ].value : 0;

	if( self->byteBlock && end >= start && token->name == DTOK_IDENTIFIER ){
		DaoParser_ByteEncodeGetConst( self, name );
	}

	if( end > start && token->name == DTOK_IDENTIFIER ){
		type = DaoParser_ParseUserType( self, start, end, newpos );
		if( type ) return type;
	}

	*newpos = start + 1;
	if( i > 0 && i < 100 ){
		/* Always compile unscoped builtin type names as builtin types: */
		type = DaoNamespace_MakeType( self->vmSpace->daoNamespace, name->chars, i, NULL, 0,0 );
		if( type->tid == DAO_TUPLE ) type->variadic = 1; /* "tuple" is variadic; */
		if( type->tid == DAO_ENUM ) type->subtid = DAO_ENUM_ANY; /* "enum"; */
		return type;
	}else if( token->name == DTOK_ID_THTYPE ){
		type = DaoParser_FindTypeHolder( self, & token->string );
		if( type ) return type;
	}
	if( i > 0 && i < 100 ){
		type = DaoNamespace_MakeType( ns, name->chars, i, NULL, 0,0 );
	}else if( token->name == DKEY_NONE ){
		type = DaoNamespace_MakeValueType( ns, dao_none_value );
	}else if( token->name == DTOK_ID_THTYPE ){
		DMap *initypes = self->innerParser ? self->innerParser->initTypes : self->initTypes;
		type = DaoType_New( token->string.chars, DAO_THT, NULL, NULL );
		DMap_Insert( initypes, type->name, type );
	}else{
		/* scoped type or user defined template class */
		type = DaoParser_ParseUserType( self, start, end, newpos );
		if( type == NULL ) goto InvalidTypeName;
	}
	return type;
InvalidTypeName:
	DaoParser_ErrorToken( self, DAO_INVALID_TYPE_NAME, tokens[start] );
	return NULL;
}

DaoType* DaoParser_ParseTypeItems( DaoParser *self, int start, int end, int valtype, DList *types )
{
	DaoNamespace *ns = self->nameSpace;
	DaoToken **tokens = self->tokens->items.pToken;
	int i = start;
	while( i <= end ){
		DaoType *type = NULL;
		DString *name = NULL;
		int tid = DAO_NONE, t = tokens[i]->type;
		int t2 = (i+1 <= end) ? tokens[i+1]->type : 0;
		int invar = 0;

		if( i == start && tokens[i]->type == DTOK_FIELD ) goto ReturnType;
		if( (i+1) <= end && tokens[i]->name == DKEY_INVAR && tokens[i+1]->name != DTOK_LT ){
			invar = 1;
			t = tokens[++i]->type;
			t2 = (i+1 <= end) ? tokens[i+1]->type : 0;
		}
		if( tokens[i]->type >= DTOK_ID_SYMBOL && tokens[i]->type <= DTOK_WCS ){
			if( valtype == 0 ) goto InvalidTypeForm;
			type = DaoParser_ParseValueType( self, i );
			if( invar && type ) type = DaoType_GetInvarType( type );
			i += 1;
		}else if( tokens[i]->type == DTOK_DOTS ){
			i += 1;
			if( tokens[i]->type == DTOK_COLON ){
				type = DaoParser_ParseType( self, i+1, end, & i, types );
				if( type == NULL ) goto InvalidTypeForm;
			}
			type = DaoNamespace_MakeType( ns, "...", DAO_PAR_VALIST, (DaoValue*) type, NULL, 0 );
			if( invar && type ) type = DaoType_GetInvarType( type );
		}else{
			if( t == DTOK_IDENTIFIER && (t2 == DTOK_COLON || t2 == DTOK_ASSN) ){
				name = & tokens[i]->string;
				tid = t2 == DTOK_COLON ? DAO_PAR_NAMED : DAO_PAR_DEFAULT;
				if( i + 2 > end ) goto InvalidTypeForm;
				i = i + 2;
			}
			type = DaoParser_ParseType( self, i, end, & i, types );
			if( type == NULL ) goto InvalidTypeForm;
			if( valtype == 0 && type->valtype && type->tid != DAO_NONE ) goto InvalidTypeForm;
			if( invar ) type = DaoType_GetInvarType( type );
			if( name ){
				type = DaoNamespace_MakeType( ns, name->chars, tid, (DaoValue*)type, NULL,0 );
			}
		}
		if( type == NULL ) return NULL;
		DList_Append( types, type );
		if( i > end ) break;
ReturnType:
		if( tokens[i]->type == DTOK_FIELD ){
			type = DaoParser_ParseType( self, i+1, end, & i, NULL );
			if( type == NULL ) return NULL;
			if( i <= end ) goto InvalidTypeForm;
			return type;
		}else if( tokens[i]->type != DTOK_COMMA ){
			goto InvalidTypeForm;
		}
		i += 1;
	}
	return NULL;
InvalidTypeForm:
	DaoParser_ErrorToken( self, DAO_INVALID_TYPE_FORM, tokens[i] );
	return NULL;
}
int DaoParser_ParseTemplateParams( DaoParser *self, int start, int end, DList *holders, DList *defaults, DString *name )
{
	DaoToken **tokens = self->tokens->items.pToken;
	int i = start;
	while( i < end ){
		DaoType *holder, *deft = NULL;
		DString *str = & tokens[i]->string;
#if 0
		if( tokens[i]->name != DTOK_ID_THTYPE ){
			DaoParser_Error( self, DAO_TOKEN_NOT_EXPECTED, str );
			return 0;
		}
#endif
		if( tokens[i]->type >= DTOK_ID_SYMBOL && tokens[i]->type <= DTOK_WCS ){
			holder = DaoParser_ParseValueType( self, i );
			i += 1;
		}else{
			holder = DaoParser_ParseType( self, i, end-1, &i, NULL );
		}
		if( holder == NULL ) return 0;
		if( name ){
			if( holders->size ) DString_AppendChar( name, ',' );
			DString_Append( name, holder->name );
		}
		if( i < end && tokens[i]->type == DTOK_ASSN ){
			deft = DaoParser_ParseType( self, i+1, end-1, &i, NULL );
			if( deft == NULL ) return 0;
		}
		if( deft == NULL && DList_Back( defaults ) != NULL ){
			DaoParser_Error( self, DAO_PARAM_NEED_DEFAULT, & tokens[i-1]->string );
			return 0;
		}
		DList_Append( holders, holder );
		DList_Append( defaults, deft );
		if( i < end && tokens[i]->type != DTOK_COMMA ){
			DaoParser_Error( self, DAO_TOKEN_NOT_EXPECTED, & tokens[i]->string );
			return 0;
		}
		i += 1;
	}
	return 1;
}

static DaoType* DaoParser_ParseEnumTypeItems( DaoParser *self, int start, int end )
{
	DaoType *type, *type2;
	DaoToken *tok;
	DaoToken **tokens = self->tokens->items.pToken;
	DMap *values = DHash_New(0,0);
	DString *field = NULL;
	uchar_t sep = 0;
	daoint value = 0;
	int k, set=0, sign = 1;
	char c;

	type = DaoType_New( "enum<", DAO_ENUM, NULL, NULL );
	DString_Reserve( type->name, 128 );
	for(k=start; k<=end; k++){
		tok = tokens[k];
		field = & tok->string;
		c = tok->string.chars[0];
		sign = 1;
		if( tok->type != DTOK_IDENTIFIER ) goto WrongForm;
		if( tok->name == DTOK_ID_THTYPE || tok->name == DTOK_ID_SYMBOL ) goto WrongForm;
		if( DMap_Find( type->mapNames, field ) ) goto WrongForm;
		if( k+1 <= end && tokens[k+1]->type == DTOK_ASSN ){
			k += 1;
			if( k+1 <= end ){
				c = tokens[k+1]->type;
				if( c == DTOK_ADD || c == DTOK_SUB ){
					k += 1;
					if( c == DTOK_SUB ) sign = -1;
				}
			}
			if( k+1 > end ) goto WrongForm;
			c = tokens[k+1]->type;
			if( c >= DTOK_DIGITS_DEC && c <= DTOK_NUMBER_HEX ){
				k += 1;
				set = 1;
				value = DaoToken_ToInteger( tokens[k] );
				if( DMap_Find( values, IntToPointer( value ) ) != NULL ){
					DaoParser_Error2( self, DAO_VALUE_WAS_USED, k, k, 0 );
					break;
				}
				DMap_Insert( values, IntToPointer( value ), 0 );
			}else goto WrongForm;
		}
		if( sep ==0 && (k+1) <= end ){
			sep = tokens[k+1]->type;
			if( sep != DTOK_COMMA && sep != DTOK_SEMCO ) goto WrongForm;
			if( sep == DTOK_SEMCO && set == 0 ) value = 1;
		}
		if( sign < 0 ) value = - value;
		DMap_Insert( type->mapNames, field, (void*)value );
		if( sep == DTOK_SEMCO ){
			value <<= 1;
		}else{
			value += 1;
		}
		if( k+1 > end ) break;
		k += 1;
		tok = tokens[k];
		if( sep != tok->type ) break;
	}
	switch( sep ){
	default : type->subtid = DAO_ENUM_SYM; break;
	case DTOK_COMMA : type->subtid = DAO_ENUM_STATE; break;
	case DTOK_SEMCO : type->subtid = DAO_ENUM_FLAG;  break;
	}
	type->value->xBase.subtype = type->subtid;
	if( k < end ) goto WrongForm;
	for(k=start; k<=end; k++) DString_Append( type->name, & tokens[k]->string );
	DString_AppendChar( type->name, '>' );
	DString_Assign( type->fname, type->name );
	/*
	   printf( "%i  %i  %s\n", end, i, type->name->chars );
	 */
	if( (type2 = DaoNamespace_FindType( self->nameSpace, type->name )) ){
		DaoType_Delete( type );
		type = type2;
	}else{
		DaoNamespace_AddType( self->nameSpace, type->name, type );
	}
	DMap_Delete( values );
	return type;
WrongForm:
	DaoParser_ErrorToken( self, DAO_INVALID_TYPE_FORM, tokens[k] );
	DaoType_Delete( type );
	DMap_Delete( values );
	return NULL;
}
static DaoType* DaoParser_MakeCSRoutineType( DaoParser *self, DaoType *type, DaoType *cbtype )
{
	DaoType *tt;
	DaoNamespace *ns = self->nameSpace;
	DString *name = DaoParser_GetString( self );

	DString_Assign( name, type->name );
	DString_Append( name, cbtype->name );
	tt = DaoNamespace_FindType( ns, name );
	if( tt ){
		type = tt;
	}else{
		type = DaoType_Copy( type );
		//type->attrib = 0; // XXX
		type->attrib |= DAO_TYPE_CODESECT;
		DString_Assign( type->name, name );
		DList_Append( ns->auxData, type );
		GC_Assign( & type->cbtype, cbtype );
	}
	return type;
}
static DaoType* DaoParser_ParseType2( DaoParser *self, int start, int end, int *newpos, DList *types )
{
	DaoType *type = NULL;
	DaoType *type2 = NULL;
	DaoValue *retype = NULL;
	DaoType **nested = NULL;
	DaoNamespace *ns = self->nameSpace;
	DaoToken **tokens = self->tokens->items.pToken;
	DaoToken *tok = tokens[start];
	DString *tks = & tok->string;
	int i, j, t = tokens[start]->name;
	int gt, tid, count, count2;
	int daons = 0, tokname = 0;

#if 0
	for(i=start; i<=end; i++) printf("%s  ", tokens[i]->string.chars); printf("\n\n");
#endif

	*newpos = start + 1;
	if( start == end || t == DTOK_QUERY || t == DTOK_ID_THTYPE ){
		DaoValue *initype = NULL;
		DaoType *vartype = NULL;
		type = DaoParser_ParsePlainType( self, start, end, newpos );
		if( type == NULL ) return type;
		initype = (DaoValue*) type;
		if( type->tid == DAO_THT && start < end && tokens[start+1]->name == DTOK_LT ){
			DString *name;
			DaoParser *scope = self->innerParser ? self->innerParser : self;
			int gt = DaoParser_FindPairToken( self, DTOK_LT, DTOK_GT, start+1, end );
			if( gt < 0 ) goto WrongType;
			vartype = DaoParser_ParseType( self, start + 2, gt, newpos, types );
			if( vartype == NULL || *newpos != gt ) goto WrongType;
			name = DaoParser_GetString( self );
			DString_Assign( name, type->name );
			DString_AppendChar( name, '<' );
			DString_Append( name, vartype->name );
			DString_AppendChar( name, '>' );
			type2 = DaoParser_FindTypeHolder( self, name );
			if( type2 == NULL ) type2 = DaoType_New( name->chars, DAO_THT, (DaoValue*) vartype, NULL );
			DMap_Insert( scope->initTypes, type2->name, type2 );
			DMap_Insert( scope->initTypes, type->name, type2 );
			type = type2;
			*newpos = gt + 1;
			if( type2 == NULL ) goto WrongType;
		}
		return type;
WrongType:
		DaoParser_ErrorToken( self, DAO_INVALID_TYPE_FORM, tokens[start] );
		return NULL;
	}
	count = types->size;
	if( tokens[start]->type != DTOK_IDENTIFIER ) goto InvalidTypeName;
	if( (start+1) < end && tokens[start+1]->type == DTOK_COLON2 ){
		if( strcmp( tokens[start]->string.chars, "dao" ) ==0 ){
			daons = 1;
			start += 2;
			ns = self->vmSpace->daoNamespace;
			tok = tokens[start];
			if( tok->name > DKEY_LOAD ) tokname = dao_keywords[ tok->name - DKEY_LOAD ].value;
			if( tok->type != DTOK_IDENTIFIER ) goto InvalidTypeName;
		}
	}
	if( tokens[start]->name == DTOK_IDENTIFIER ){
		/* scoped type or user defined template class */
		type = DaoParser_ParseUserType( self, start, end, newpos );
		if( type == NULL ) goto InvalidTypeName;
	}else if( start < end && tokens[start]->name == DKEY_ENUM && tokens[start+1]->name == DTOK_LT ){
		gt = DaoParser_FindPairToken( self, DTOK_LT, DTOK_GT, start, end );
		if( gt < 0 ) goto InvalidTypeForm;
		*newpos = gt + 1;
		type = DaoParser_ParseEnumTypeItems( self, start+2, gt-1 );
	}else if( start < end && tokens[start+1]->name == DTOK_LT ){
		int ecount = self->errors->size;
		gt = DaoParser_FindPairToken( self, DTOK_LT, DTOK_GT, start, end );
		if( gt < 0 ) goto InvalidTypeForm;
		*newpos = gt + 1;
		type = DaoParser_ParseTypeItems( self, start+2, gt-1, 0, types );
		if( self->errors->size > ecount ) goto InvalidTypeForm;
		if( type && tokens[start]->name != DKEY_ROUTINE ) goto InvalidTypeForm;
		count2 = types->size - count;
		retype = NULL;
		tid = DAO_NONE;
		switch( tokens[start]->name ){
		case DKEY_TYPE :
			tid = DAO_TYPE;
			if( count2 != 1 ) goto InvalidTypeForm;
			break;
		case DKEY_ARRAY :
			tid = DAO_ARRAY;
			if( count2 != 1 ) goto InvalidTypeForm;
			type = types->items.pType[ count ];
			if( type->tid == 0 ) goto InvalidTypeForm;
			if( type->tid > DAO_COMPLEX && type->tid != DAO_THT ) goto InvalidTypeForm;
			break;
		case DKEY_LIST :
			tid = DAO_LIST;
			if( count2 != 1 ) goto InvalidTypeForm;
			break;
		case DKEY_MAP  :
			tid = DAO_MAP;
			if( count2 != 2 ) goto InvalidTypeForm;
			break;
		case DKEY_TUPLE :
			tid = DAO_TUPLE;
			break;
		case DKEY_ROUTINE :
			tid = DAO_ROUTINE;
			if( type == NULL ) type = DaoNamespace_MakeValueType( ns, dao_none_value );
			if( type->tid >= DAO_PAR_NAMED && type->tid <= DAO_PAR_VALIST ){
				goto InvalidTypeForm;
			}
			retype = (DaoValue*) type;
			break;
		case DKEY_CLASS :
			if( count2 != 1 ) goto InvalidTypeForm;
			type = types->items.pType[ count ];
			DList_Erase( types, count, count2 );
			switch( type ? type->tid : 0 ){
			case DAO_CDATA :
			case DAO_CSTRUCT : type = type->aux->xCtype.ctype;  goto DoneGenericType;
			case DAO_OBJECT : type = type->aux->xClass.clsType; goto DoneGenericType;
			}
			goto InvalidTypeForm;
		case DKEY_VAR :
			if( count2 != 1 ) goto InvalidTypeForm;
			type = types->items.pType[ count ];
			type = DaoType_GetVarType( type );
			goto DoneGenericType;
		case DKEY_INVAR :
			if( count2 != 1 ) goto InvalidTypeForm;
			type = types->items.pType[ count ];
			type = DaoType_GetInvarType( type );
			goto DoneGenericType;
		default : goto InvalidTypeForm;
		}
		if( tid != DAO_TUPLE && tid != DAO_ROUTINE && tid != DAO_CODEBLOCK ){
			for(i=count; i<types->size; ++i){
				DaoType *tp = types->items.pType[i];
				if( tp->tid >= DAO_PAR_NAMED && tp->tid <= DAO_PAR_VALIST ){
					goto InvalidTypeForm;
				}
			}
		}
		tks = & tokens[start]->string;
		nested = types->items.pType + count;
		type = DaoNamespace_MakeType( ns, tks->chars, tid, retype, nested, count2 );
		if( type == NULL ) goto InvalidTypeForm;
		if( tid == DAO_ROUTINE ){
			DString sname = DString_WrapChars( "self" );
			DNode *node = MAP_Find( type->mapNames, & sname );
			if( node && node->value.pInt == 0 ) type->attrib |= DAO_TYPE_SELF;
		}
		if( tid == DAO_ROUTINE && gt < end && tokens[gt+1]->type == DTOK_LSB ){
			DaoType *cbtype = DaoParser_ParseCodeBlockType( self, gt+1, newpos );
			DString *name = DaoParser_GetString( self );
			if( cbtype == NULL ) goto InvalidTypeForm;
			type = DaoParser_MakeCSRoutineType( self, type, cbtype );
		}
DoneGenericType:
		DList_Erase( types, count, count2 );
	}else if( tokens[start]->name == DKEY_ROUTINE ){
		type = DaoNamespace_MakeType( ns, "routine", DAO_ROUTINE, NULL, NULL, 0  );
		if( start < end && tokens[start+1]->type == DTOK_LSB ){
			DaoType *cbtype = DaoParser_ParseCodeBlockType( self, start+1, newpos );
			if( cbtype == NULL ) goto InvalidTypeForm;
			type = DaoParser_MakeCSRoutineType( self, type, cbtype );
		}
	}else if( tokens[start]->type == DTOK_IDENTIFIER ){
		if( daons ) start -= 2;
		type = DaoParser_ParsePlainType( self, start, end, newpos );
		if( type == NULL ) goto InvalidTypeName;
	}else{
		goto InvalidTypeForm;
	}
#if 0
	printf( "%s %i %p\n", type->name->chars, *newpos, type->cbtype );
#endif
	return type;
InvalidTypeName:
InvalidTypeForm:
	DaoParser_ErrorToken( self, DAO_INVALID_TYPE_FORM, tokens[start] );
	return NULL;
}
DaoType* DaoParser_ParseType( DaoParser *self, int start, int end, int *next, DList *types )
{
	DaoNamespace *ns = self->nameSpace;
	DaoToken **tokens = self->tokens->items.pToken;
	DaoType *type = NULL;
	int count;
	if( types == NULL ) types = self->typeItems;
	count = types->size;
	type = DaoParser_ParseType2( self, start, end, next, types );
	if( type == NULL ) goto InvalidType;
	if( type->tid != DAO_NONE && type->valtype ) goto InvalidType;
	DList_Append( types, type );
	while( type && *next <= end && tokens[*next]->name == DTOK_PIPE ){
		type = DaoParser_ParseType2( self, *next + 1, end, next, types );
		if( type == NULL ) goto InvalidType;
		if( type->tid != DAO_NONE && type->valtype ) goto InvalidType;
		DList_Append( types, type );
	}
	if( types->size == count + 1 ){
		type = types->items.pType[count];
		DList_PopBack( types );
	}else{
		DaoType **nested = types->items.pType + count;
		int i, count2 = types->size - count;
		type = DaoNamespace_MakeType( ns, "", DAO_VARIANT, NULL, nested, count2 );
		DList_Erase( types, count, count2 );
		if( type == NULL ) goto InvalidType;
	}
	if( self->byteBlock ) DaoByteBlock_EncodeType( self->byteBlock, type );
	return type;
InvalidType:
	return NULL;
}

int DaoParser_ParseRoutine( DaoParser *self );
DAO_DLL int DaoParser_ParseLoadStatement( DaoParser *self, int start, int end );

static DaoValue* DaoParse_InstantiateType( DaoParser *self, DaoValue *tpl, int start, int end )
{
	DList *types = DList_New(0);
	DaoCtype *ctype = (DaoCtype*) tpl;
	DaoType *sptype, *gentype;
	DaoCinType *cintype = NULL;
	int errors = self->errors->size;

	if( tpl == NULL ) goto FailedInstantiation;

	if( tpl->type != DAO_INTERFACE && tpl->type != DAO_CTYPE && tpl->type != DAO_TYPE ){
		goto FailedInstantiation;
	}
	DaoParser_ParseTypeItems( self, start, end, 1, types );
	if( self->errors->size > errors ) goto FailedInstantiation;

	if( tpl->type == DAO_INTERFACE ){
		if( types->size != 1 ) goto FailedInstantiation;
		cintype = DaoInterface_GetConcrete( (DaoInterface*) tpl, types->items.pType[0] );
		if( cintype == NULL ) goto FailedInstantiation;
	}else{
		gentype = tpl->type == DAO_CTYPE ? ctype->cdtype : (DaoType*) tpl;
		sptype = DaoType_Specialize( gentype, types->items.pType, types->size );
		if( sptype == NULL ) goto FailedInstantiation;
		if( self->byteBlock && tpl->type == DAO_CTYPE ){
			DaoCtype *ctype = (DaoCtype*) sptype->aux;
			DaoCtype *gtype = (DaoCtype*) gentype->aux;
			DaoType **ts = types->items.pType;
			DaoByteBlock_EncodeCtype( self->byteBlock, ctype, gtype, ts, types->size );
			DaoByteBlock_EncodeType( self->byteBlock, sptype );
		}
	}

DoneInstantiation:
	DList_Delete( types );
	if( tpl->type == DAO_INTERFACE ) return (DaoValue*) cintype; 
	return tpl->type == DAO_CTYPE ? sptype->aux : (DaoValue*) sptype;
FailedInstantiation:
	DList_Delete( types );
	return NULL;
}


static int DaoParser_Preprocess( DaoParser *self );
int DaoParser_ParseScript( DaoParser *self )
{
	DaoNamespace *ns = self->nameSpace;
	DaoVmSpace   *vmSpace = self->vmSpace;
	DaoRoutine *routMain = self->routine; /* could be set in DaoVmSpace_Eval() */
	daoint i, bl;

	if( routMain == NULL ) routMain = DaoRoutine_New( ns, NULL, 1 );

	/*
	   printf("DaoParser_ParseScript() ns=%p, rout=%p, %s\n", ns, routMain, self->fileName->chars );
	 */

	if( self->byteBlock ){
		GC_Assign( & self->byteBlock->value, routMain );
	}
	if( routMain->routType == NULL ){
		routMain->routType = dao_type_routine;
		GC_IncRC( dao_type_routine );
	}
	routMain->attribs |= DAO_ROUT_MAIN;
	ns->mainRoutine = routMain;
	DaoNamespace_SetConst( ns, DVR_NSC_MAIN, (DaoValue*) routMain );
	DString_SetChars( routMain->routName, "__main__" );
	DList_Append( ns->mainRoutines, routMain );
	/* the name of routMain will be set in DaoParser_ParseRoutine() */

	routMain->body->codeStart = 1;
	routMain->body->codeEnd = self->lineCount;
	self->routine = routMain;
	self->vmSpace = vmSpace;
	self->nameSpace = ns;

	if( DaoParser_Preprocess( self ) == 0 || DaoParser_ParseRoutine( self ) == 0 ){
		DaoParser_PrintError( self, 0, 0, NULL );
		return 0;
	}
	/*
	// Currently the __main__ routine is inferenced after other routines.
	// As a result, some code in those routines cannot be specialized,
	// because the type information for some global variable may not be
	// available. Redoing inference on those routines may allow these
	// code to be specialized.
	//
	// Do not redo such inference on compiling to bytecode.
	// Because the type information for some global variables (declare
	// without explicit types) are not yet available when those routines
	// are decoded. Without proper type information, type errors may arise
	// for specialized code that use global variables.
	//
	// Example:
	//   global glb = rand(100)
	//   routine func() 
	//   {
	//     return glb + 456 
	//   }
	 */
	if( self->byteBlock ) return 1;
	for(i=0; i<self->routReInferable->size; i++){
		DaoRoutine *rout = (DaoRoutine*) self->routReInferable->items.pValue[i];
		DaoRoutine_DoTypeInference( rout, 0 );
	}
	self->routReInferable->size = 0;
	return 1;
}

static int DaoParser_GetTokenLine( DaoParser *self, int index )
{
	int line = 0;
	DaoToken **tokens = self->tokens->items.pToken;
	if( index < self->tokens->size ){
		line = tokens[index]->line;
	}else if( self->tokens->size ){
		line = tokens[self->tokens->size-1]->line;
	}
	return line;
}
static void DaoVmCode_Set( DaoVmCodeX *self, ushort_t code, ushort_t a, ushort_t b,
		ushort_t c, uchar_t lev, int line, int first, int mid, int last )
{
	if( mid >= first ) mid -= first;
	if( last >= first ) last -= first;
	self->code = code;
	self->a = a;
	self->b = b;
	self->c = c;
	self->level = lev;
	self->line = line;
	self->first = first;
	self->middle = mid;
	self->last = last;
}
static DaoInode* DaoParser_AddCode( DaoParser *self, ushort_t code, ushort_t a, ushort_t b, ushort_t c )
{
	return DaoParser_AddCode2( self, code, a, b, c );
}
static DaoInode* DaoParser_AddCode2( DaoParser *self, ushort_t code, ushort_t a, ushort_t b, ushort_t c )
{
	int first = self->firstToken;
	int mid = self->middleToken;
	int last = self->lastToken;
	int line = DaoParser_GetTokenLine( self, first );
	DaoInode *node = DaoParser_NewNode( self );

	if( mid >= first ) mid -= first;
	if( last >= first ) last -= first;

	node->code = code;
	node->a = a;
	node->b = b;
	node->c = c;
	node->line = line;
	node->first = first;
	node->middle = mid;
	node->last = last;
	node->level = self->lexLevel;
	node->prev = self->vmcLast;
	self->vmcLast->next = node;
	self->vmcLast = node;
	self->vmcCount ++;

	return node;
}

static int DaoParser_DeclareVariable( DaoParser *self, DaoToken *tok, int vt, DaoType *tp );
static int DaoParser_ParseCondition( DaoParser *self, int start, int dec, DaoInode *opening );
static int DaoParser_ParseForLoop( DaoParser *self, int start, int end );
static int DaoParser_PostParsing( DaoParser *self );

DaoType* DaoParser_ParseTypeName( const char *name, DaoNamespace *ns, DaoClass *cls )
{
	DaoParser *parser = DaoVmSpace_AcquireParser( ns->vmSpace );
	DaoType *type = NULL;
	int i = 0;
	if( ! DaoLexer_Tokenize( parser->lexer, name, DAO_LEX_ESCAPE ) ) goto ErrorType;
	DaoNamespace_InitConstEvalData( ns );
	parser->nameSpace = ns;
	parser->hostClass = cls;
	parser->routine = ns->constEvalRoutine;
	parser->vmSpace = ns->vmSpace;
	type = DaoParser_ParseType( parser, 0, parser->tokens->size-1, &i, NULL );
	if( i < parser->tokens->size && type ) type = NULL;
	DaoVmSpace_ReleaseParser( ns->vmSpace, parser );
	return type;
ErrorType:
	DaoVmSpace_ReleaseParser( ns->vmSpace, parser );
	return NULL;
}
static void DaoParser_SetupSwitch( DaoParser *self, DaoInode *opening )
{
	DaoValue *key, **cst = self->routine->routConsts->value->items.pValue;
	DaoInode *node = opening->jumpTrue;
	DaoInode *it2, *aux;
	DMap *table;
	DNode *iter;
	int i, min, max, count, direct = 0, casemode = 0;
	min = max = 0;
	count = 0;
	table = self->switchTables->items.pMap[ node->b ];
	for(iter=DMap_First(table); iter !=NULL; iter=DMap_Next(table, iter) ){
		key = cst[iter->value.pInode->a];
		if( key->type == DAO_INTEGER ){
			if( count == 0 ) min = max = key->xInteger.value;
			if( min > key->xInteger.value ) min = key->xInteger.value;
			if( max < key->xInteger.value ) max = key->xInteger.value;
			count ++;
		}
	}
	if( count == table->size && count > 0.75 * (max - min) ){
		DaoValue temp = {DAO_INTEGER};
		key = (DaoValue*) & temp;
		for(i=min+1; i<max; i++){
			key->xInteger.value = i;
			if( DMap_Find( table, key ) ==NULL ) DMap_Insert( table, key, NULL );
		}
		direct = 1;
	}
	casemode = direct ? DAO_CASE_TABLE : DAO_CASE_ORDERED;
	if( node->c ) casemode = DAO_CASE_TYPES;
	node->c = table->size;
	aux = node;
	for(iter=DMap_First(table); iter !=NULL; iter=DMap_Next(table, iter) ){
		it2 = DaoParser_NewNode( self );
		it2->code = DVM_CASE;
		it2->c = casemode; /* mark case mode; */
		if( iter->value.pInode ){
			it2->a = iter->value.pInode->a;
			it2->jumpTrue = iter->value.pInode;
			it2->first = iter->value.pInode->first;
			it2->middle = iter->value.pInode->middle;
			it2->last = iter->value.pInode->last;
		}else{
			it2->a = DaoRoutine_AddConstant( self->routine, iter->key.pValue );
			it2->jumpTrue = node->jumpFalse; /* jump to default */
		}
		it2->prev = aux;
		it2->next = aux->next;
		aux->next->prev = it2;
		aux->next = it2;
		aux = it2;
	}
}
static DaoInode* DaoParser_AddScope( DaoParser *self, int code, DaoInode *closing )
{
	DaoInode *node = DaoParser_AddCode( self, code, 0, 0, 0 );
	DList_Append( self->scopeOpenings, node );
	DList_Append( self->scopeClosings, closing );
	node->jumpFalse = closing;
	DaoParser_PushLevel( self );
	return node;
}
static int DaoParser_AddScope2( DaoParser *self, int at )
{
	DaoToken **tokens = self->tokens->items.pToken;
	int token = at < self->tokens->size ? tokens[at]->name : 0;
	int code = token == DTOK_LCB ? (int)DVM_LBRA : (int)DVM_NOP;
	DaoParser_AddScope( self, code, NULL );
	return token == DTOK_LCB;
}
static int DaoParser_DelScope( DaoParser *self, DaoInode *node )
{
#if 0
	printf( "DaoParser_DelScope() %i %li\n", self->lexLevel, self->scopeOpenings->size );
	DaoParser_PrintCodes( self );
#endif
	DaoInode *opening = (DaoInode*) DList_Back( self->scopeOpenings );
	DaoInode *closing = (DaoInode*) DList_Back( self->scopeClosings );
	DaoParser_PopLevel( self );
	self->vmcValue = NULL;
	if( self->lexLevel < 0 || self->scopeOpenings->size == 0 ){
		DaoParser_Error3( self, DAO_INVALID_SCOPE_ENDING, self->curToken );
		return 0;
	}
	DaoParser_PushTokenIndices( self, self->curToken-1, 0, 0 );
	if( opening->code == DVM_BRANCH && closing->c == DVM_SWITCH ){
		DaoInode *branch = opening->jumpTrue; /* condition test */
		DaoParser_PopLevel( self );
		DaoParser_SetupSwitch( self, opening );
	}else if( opening->code == DVM_BRANCH ){
		DaoInode *branch = opening->jumpTrue; /* condition test */
		branch->jumpFalse = closing;
	}else if( opening->code == DVM_LOOP ){
		DaoInode *branch = opening->jumpTrue; /* condition test */
		node = DaoParser_AddCode( self, DVM_GOTO, 0, 0, 0 );
		branch->jumpFalse = closing;
		node->jumpTrue = opening;
	}else if( opening->code == DVM_LBRA ){
		node = DaoParser_AddCode( self, DVM_RBRA, 0, 0, 0 );
	}
	if( closing && closing->next ){
		closing->prev->next = closing->next;
		closing->next->prev = closing->prev;
		closing->prev = self->vmcLast;
		closing->next = NULL;
		self->vmcLast->next = closing;
		self->vmcLast = closing;
	}
	DaoParser_PopTokenIndices( self, 1 );
	DList_Pop( self->scopeOpenings );
	DList_Pop( self->scopeClosings );
	return 1;
}
static int DaoParser_CompleteScope( DaoParser *self, int at )
{
	DaoToken **tokens = self->tokens->items.pToken;
	int token, next = at + 1, size = self->tokens->size;
	while( next < size && tokens[next]->name == DTOK_SEMCO ) next += 1;
	token = next < size ? tokens[next]->name : 0;
	while( self->scopeOpenings->size >0 ){
		DaoInode *back = (DaoInode*) DList_Back( self->scopeOpenings );
		DaoInode *close = (DaoInode*) DList_Back( self->scopeClosings );
		if( back->code == DVM_LBRA ) break;
		if( close != NULL && close->a ){
			if( close->a == token ) break;
			if( close->a == DKEY_WHILE ){ /* Too many statements between do-while: */
				DaoParser_Error3( self, DAO_INVALID_SCOPE_ENDING, self->curToken );
				return 0;
			}
		}
		if( DaoParser_DelScope( self, NULL ) ==0 ) return 0;
	}
	return 1;
}
static DaoInode* DaoParser_GetBreakableScope( DaoParser *self )
{
	int i;
	for(i=(int)self->scopeOpenings->size-1; i>=0; i--){
		DaoInode *opening = self->scopeOpenings->items.pInode[i];
		DaoInode *closing = self->scopeClosings->items.pInode[i];
		if( closing && closing->b ) return opening;
	}
	return NULL;
}
void DaoParser_MakeCodes( DaoParser *self, int start, int end, DString *output )
{
	DaoToken *tok, **tokens = self->tokens->items.pToken;
	int i, cpos = 0, line = -1;

	tok = start ? tokens[start-1] : NULL;
	if( tok && tok->name >= DKEY_PRIVATE && tok->name <= DKEY_PUBLIC ){
		DString_Append( output, & tok->string );
		cpos = tok->cpos;
		line = tok->line;
	}
	for(i=start; i<end; i++){
		tok = tokens[i];
		if( (int)tok->line != line ) DString_AppendChar( output, '\n' );
		if( tok->cpos > cpos+1 ) DString_AppendChar( output, ' ' );
		DString_Append( output, & tok->string );
		cpos = tok->cpos;
		line = tok->line;
	}
}
static int DaoParser_HandleVerbatim( DaoParser *self, int start )
{
	DaoNamespace *ns = self->nameSpace;
	DString *verbatim = & self->tokens->items.pToken[start]->string;
	daoint pstart = 0, pend = verbatim->size-1, wcs = verbatim->chars[1] == '@';
	const char *pat = "^ @{1,2} %[ %s* %w+ %s* ( %( %s* (|%w+) %s* %) | %])";
	int line = self->tokens->items.pToken[start]->line;

	self->curLine = line;
	if( DString_Match( verbatim, pat, & pstart, & pend ) ){ /* code inlining */
		DaoCodeInliner inliner;
		daoint lb = DString_FindChar( verbatim, '(', 0 );
		if( lb > pend ) lb = DAO_NULLPOS;

		if( lb != DAO_NULLPOS ){
			daoint rb = DString_FindChar( verbatim, ')', 0 );
			DString_SetBytes( self->string, verbatim->chars + 2 + wcs, lb - 2 - wcs );
			DString_SetBytes( self->string2, verbatim->chars + lb + 1, rb - lb - 1 );
			DString_Trim( self->string2, 1, 1, 0 );
		}else{
			daoint rb = DString_FindChar( verbatim, ']', 0 );
			DString_SetBytes( self->string, verbatim->chars + 2 + wcs, rb - 2 - wcs );
			DString_Clear( self->string2 );
		}
		DString_Trim( self->string, 1, 1, 0 );
		inliner = DaoNamespace_FindCodeInliner( ns, self->string );
		if( inliner == NULL ){
			if( lb != DAO_NULLPOS )
				printf( "inlined code not handled, inliner \"%s\" not found\n", self->string->chars );
			start ++;
			return start;
		}
		if( self->byteBlock ){
			DaoByteBlock_EncodeVerbatim( self->byteBlock, self->string, self->string2, verbatim, line );
		}
		DString_Clear( self->string );
		if( (*inliner)( ns, self->string2, verbatim, self->string, line ) ){
			DString_InsertChars( self->string, "code inlining failed: ", 0, 0, 0 );
			DaoParser_Error( self, DAO_CTW_INTERNAL, self->string );
			return -1;
		}
		DList_Erase( self->tokens, start, 1 );
		if( self->string->size ){
			DaoLexer *lexer = DaoLexer_New();
			DList *tokens = lexer->tokens;
			DaoLexer_Tokenize( lexer, self->string->chars, 0 );
			DList_InsertList( self->tokens, start, tokens, 0, -1 );
			DaoLexer_Delete( lexer );
		}
	}else{
		start ++;
	}
	return start;
}


static int DaoParser_ApplyTokenFilters( DaoParser *self, DaoNamespace *nspace )
{
	int i, ret = 1;
	for(i=1; i<nspace->namespaces->size; ++i){
		ret &= DaoParser_ApplyTokenFilters( self, nspace->namespaces->items.pNS[i] );
	}
	for(i=0; i<nspace->tokenFilters->size; ++i){
		DaoTokenFilter filter = (DaoTokenFilter) nspace->tokenFilters->items.pVoid[i];
		ret &= (*filter)( self );
	}
	return ret;
}
static int DaoParser_Preprocess( DaoParser *self )
{
	DaoNamespace *ns = self->nameSpace;
	DaoVmSpace *vmSpace = self->vmSpace;
	DaoToken **tokens = self->tokens->items.pToken;
	int cons = (vmSpace->options & DAO_OPTION_INTERUN) && (ns->options & DAO_NS_AUTO_GLOBAL);
	int i, end, tag = 0;
	int k, right, start = 0;
	unsigned char tki, tki2;

#if 0
	printf("routine = %p\n", self->routine );
	for(i=0; i<self->tokens->size; i++) printf("%s  ", tokens[i]->string->chars); printf("\n\n");
#endif

	while( start >=0 && start < self->tokens->size ){
		self->curLine = tokens[start]->line;
#if 0
		printf( "start = %i\n", start );
		printf("At tokPos : %i, %s\n", tokens[start]->index, tokens[ start ]->string->chars );
#endif

		tki = tokens[start]->name;
		tki2 = start+1 < self->tokens->size ? tokens[start+1]->name : 0;
		if( tki == DKEY_LOAD && tki2 != DTOK_LB ){
			if( start+2 < self->tokens->size && tokens[start+2]->name == DTOK_LB ){
				if( tki2 == DTOK_IDENTIFIER && tokens[start+1]->line == tokens[start+2]->line ){
					start += 1;
					continue;
				}
			}
			/* only for top level "load", for macros in the module  */
			end = DaoParser_ParseLoadStatement( self, start, self->tokens->size-1 );
			if( end < 0 ) return 0;
			if( cons ) DaoParser_MakeCodes( self, start, end, ns->inputs );
			DList_Erase( self->tokens, start, end-start );
			tokens = self->tokens->items.pToken;
		}else if( tki == DTOK_VERBATIM ){
			start = DaoParser_HandleVerbatim( self, start );
			if( start < 0 ) return 0;
			tokens = self->tokens->items.pToken;
		}else{
			if( tki == DKEY_NAMESPACE ) self->nsDefined = 1;
			start ++;
		}
	}

	if( DaoParser_ApplyTokenFilters( self, ns ) == 0 ) return 0;

#if 0
	for(i=0; i<self->tokens->size; i++) printf("%s  ", tokens[i]->string.chars); printf("\n\n");
#endif

	/* Join string literals after handling macro: */
	for(i=0; i<self->tokens->size; i++ ){
		DaoToken *t = self->tokens->items.pToken[i];
		if( (t->name == DTOK_MBS || t->name == DTOK_WCS) && i+1<self->tokens->size ){
			DaoToken *t2 = self->tokens->items.pToken[i+1];
			if( t->name == t2->name ){
				int len = t->string.size;
				DString_Erase( & t->string, len-1, DAO_NULLPOS );
				DString_AppendChars( & t->string, t2->string.chars + 1 );
				DList_Erase( self->tokens, i+1, 1 );
				i --;
			}
		}
	}
	return 1;
}
static int DaoParser_AddToScope( DaoParser *self, DString *name, DaoValue *value, DaoType *abtype, int store )
{
	DMap *symtable = DaoParser_CurrentSymbolTable( self );
	DaoRoutine *routine = self->routine;
	DaoNamespace *NS = self->nameSpace;
	int size = routine->routConsts->value->size;
	int perm = self->permission;
	int id, st, ret = 0;

	if( (self->levelBase + self->lexLevel) == 0 ){
		if( abtype ) DaoNamespace_AddType( NS, name, abtype );
		ret = DaoNamespace_AddConst( NS, name, value, perm );
	}else if( self->isClassBody && self->hostClass ){
		ret = DaoClass_AddConst( self->hostClass, name, value, perm );
	}else{
		DMap *symtable = DaoParser_CurrentSymbolTable( self );
		if( MAP_Find( symtable, name ) != NULL ) goto SymbolWasDefined;
		MAP_Insert( symtable, name, LOOKUP_BIND_LC( size ) );
		ret = DaoRoutine_AddConstant( routine, value );
	}
	if( ret >= 0 ) return 1;
SymbolWasDefined:
	DaoParser_Error( self, DAO_SYMBOL_WAS_DEFINED, name );
	return 0;
}
static DaoParser* DaoParser_NewRoutineParser( DaoParser *self, int start, int attribs );

static int DaoParser_CheckNameToken( DaoParser *self, int start, int to, int ecode, int estart )
{
	DaoToken **tokens = self->tokens->items.pToken;
	if( start >to || tokens[start]->type != DTOK_IDENTIFIER ){
		DaoParser_Error( self, DAO_TOKEN_NEED_NAME, & tokens[start]->string );
		if( ecode ) DaoParser_Error2( self, ecode, estart, start, 1 );
		return 0;
	}
	return 1;
}
static int DaoParser_AddConstant( DaoParser *self, DString *name, DaoValue *value, DaoToken *tok )
{
	DaoNamespace *NS = self->nameSpace;
	DaoRoutine *routine = self->routine;
	int perm = self->permission;
	int line = tok->line;
	if( self->isClassBody ){
		DaoClass_AddConst( self->hostClass, name, value, perm );
	}else if( self->levelBase + self->lexLevel == 0 ){
		DaoNamespace_AddConst( NS, name, value, perm );
	}else{
		daoint id = routine->routConsts->value->size;
		MAP_Insert( DaoParser_CurrentSymbolTable( self ), name, LOOKUP_BIND_LC(id) );
		DaoRoutine_AddConstant( routine, value );
	}
	/* TODO was defined warning: */
	return 0;
}
static int DaoParser_ParseTypeAliasing( DaoParser *self, int start, int to )
{
	DaoToken *nameTok, **tokens = self->tokens->items.pToken;
	DaoNamespace *NS = self->nameSpace;
	DaoRoutine *routine = self->routine;
	DaoRoutine *tmpRoutine;
	DaoParser *tmpParser;
	DaoType *type, *old, *tht;
	DString *str;
	DNode *node;
	int estart = start;
	int use = start;
	int id, recursive;

	start ++;
	if( DaoParser_CheckNameToken( self, start, to, DAO_INVALID_TYPE_ALIAS, use ) ==0 ) return -1;
	nameTok = tokens[start];
	str = & nameTok->string;
	start += 2;
	if( start >to || tokens[start]->type != DTOK_IDENTIFIER ) goto InvalidAliasing;
	if( DaoParser_GetRegister( self, nameTok ) >= 0 ){
		DaoParser_Error( self, DAO_SYMBOL_WAS_DEFINED, str );
		goto InvalidAliasing;
	}

	/* Add a new temporary type in a new scope: */
	tht = DaoType_New( str->chars, DAO_THT, NULL, NULL );
	if( self->byteBlock ){
		DaoByteBlock_EncodeType( self->byteBlock, tht );
	}
	DaoParser_PushLevel( self );
	id = LOOKUP_BIND_LC( routine->routConsts->value->size );
	MAP_Insert( DaoParser_CurrentSymbolTable( self ), str, id );
	DaoRoutine_AddConstant( routine, (DaoValue*) tht );
	id = tht->refCount;

	type = old = DaoParser_ParseType( self, start, to, & start, NULL );
	recursive = tht->refCount > id; /* The temporary type is used; */
	DaoParser_PopLevel( self );

	if( type == NULL ) goto InvalidAliasing;
	if( self->byteBlock ){
		DaoType *rt = recursive ? tht : NULL;
		DaoByteBlock_EncodeTypeAlias( self->byteBlock, old, type, str, rt, self->permission );
	}
	if( (type->tid && type->tid <= DAO_TUPLE) || type->tid == DAO_VARIANT ){
		/* See comments in: DaoNamespace_TypeDefine(); */
		type = DaoType_Copy( type );
		DString_Assign( type->name, str );
	}
	if( recursive ){
		type->recursive = 1;
		DaoType_SetupRecursive( type, tht, type );
	}
	if( (self->levelBase + self->lexLevel) == 0 ){
		DaoNamespace_AddType( NS, str, type );
		DaoNamespace_AddTypeConstant( NS, str, type );
	}else if( self->isClassBody ){
		DaoClass_AddConst( self->hostClass, str, (DaoValue*) type, self->permission );
	}else{
		DMap *table = DaoParser_CurrentSymbolTable( self );
		id = LOOKUP_BIND_LC( routine->routConsts->value->size );
		MAP_Insert( table, str, id );
		DaoRoutine_AddConstant( routine, (DaoValue*) type );
	}
	return start;
InvalidAliasing:
	DaoParser_Error3( self, DAO_INVALID_TYPE_ALIAS, estart );
	return -1;
}
#ifdef DAO_WITH_DECORATOR
static void DaoParser_DecorateRoutine( DaoParser *self, DaoRoutine *rout )
{
	DaoValue *selfpar = NULL;
	DaoValue *params[DAO_MAX_PARAM+1];
	DaoObject object = {0}, *obj = & object;
	int i, j, n, count = self->decoFuncs->size;

	if( self->byteBlock && count ){
		DaoByteBlock_EncodeDecorators( self->byteBlock, self->decoFuncs, self->decoParams );
		return;
	}

	if( rout->routHost ){
		object.type = DAO_OBJECT;
		object.defClass = (DaoClass*) rout->routHost->aux;
		selfpar = (DaoValue*) obj;
	}
	params[0] = (DaoValue*) rout;
	for(i=0; i<count; i++){
		DaoRoutine *decoFunc = self->decoFuncs->items.pRoutine[i];
		DaoList *decoParam = (DaoList*) self->decoParams->items.pValue[i];
		n = decoParam->value->size;
		for(j=0; j<n; j++) params[j+1] = decoParam->value->items.pValue[j];
		decoFunc = DaoRoutine_ResolveX( decoFunc, selfpar, NULL, params, NULL, n+1, 0 );
		if( decoFunc == NULL || DaoRoutine_Decorate( rout, decoFunc, params, n+1, 1 ) == NULL ){
			DaoParser_Error( self, DAO_INVALID_FUNCTION_DECORATION, rout->routName );
			return;
		}
	}
}
#endif
static DaoParser* DaoParser_NewRoutineParser( DaoParser *self, int start, int attribs )
{
	DaoToken **tokens = self->tokens->items.pToken;
	DaoRoutine *rout = NULL;
	DaoParser *parser;
	if( self->isInterBody ){
		DaoInterface *inter = self->hostInter;
		rout = DaoRoutine_New( self->nameSpace, inter->abtype, 0 );
	}else if( self->isCinTypeBody ){
		DaoCinType *cintype = self->hostCinType;
		rout = DaoRoutine_New( self->nameSpace, cintype->vatype, 1 );
	}else if( self->isClassBody ){
		rout = DaoRoutine_New( self->nameSpace, self->hostClass->objType, 1 );
		rout->attribs |= attribs;
	}else{
		rout = DaoRoutine_New( self->nameSpace, NULL, 1 );
	}
	DList_Append( self->refCountedList, rout );
	rout->defLine = tokens[start]->line;
	parser = DaoVmSpace_AcquireParser( self->vmSpace );
	parser->routine = rout;
	parser->vmSpace = self->vmSpace;
	parser->hostType = self->hostType;
	parser->hostClass = self->hostClass;
	parser->hostInter = self->hostInter;
	parser->hostCinType = self->hostCinType;
	parser->levelBase = self->levelBase + self->lexLevel + 1;
	parser->defParser = self;
	DList_Assign( parser->decoFuncs, self->decoFuncs2 );
	DList_Assign( parser->decoParams, self->decoParams2 );
	return parser;
}
static DaoRoutine* DaoParser_CheckDeclared( DaoParser *self, DaoRoutine *newrout, DaoRoutine *old )
{
	int id;
	DaoRoutine *rout = newrout;
	if( old && old->type != DAO_ROUTINE ){
		DaoParser_Error( self, DAO_SYMBOL_WAS_DEFINED, newrout->routName );
		return NULL;
	}
	/* Check for forward declaration: */
	if( old && old->overloads ){
		for(id=0; id<old->overloads->routines->size; ++id){
			DaoRoutine *R = old->overloads->routines->items.pRoutine[id];
			/* TODO: this is not completely precise; */
			if( DString_EQ( R->routName, newrout->routName ) == 0 ) continue;
			if( DString_EQ( R->routType->name, newrout->routType->name ) == 0 ) continue;
			rout = R;
		}
	}else if( old ){
		if( DString_EQ( old->routType->name, newrout->routType->name ) ) rout = old;
	}
	if( rout != newrout && rout->body->codeStart > 0 ){
		DaoParser_Error( self, DAO_ROUT_WAS_IMPLEMENTED, newrout->routName );
		return NULL;
	}
	return rout;
}
/*
// Parse routines:
// 'routine' { Identifier '::' } ( RoutineSig | CodeBlockSig | DecoratorSig )
// 'operator' { Identifier '::' } OverloadableOperator ParamReturn
//
// NOTES:
// Routine definition nested insided other routines is not supported.
// Anonymous routines or closures should be used instead if local routines
// encapsulated inside a routine are preferred.
//
// There are reasons not supporting normal routines nested inside routines.
// The primary reason is that a reasonable support for such routines would
// require to support them in exactly the same way as anonymous routines and
// closures, namely allowing accession of the outer variables of the host routines,
// and running time creation of the routine (closure) if it does access the outer
// variables. Another issue with supporting normal routines inside other routines
// is about overloading, as support for overloading is naturally expected for
// normal routines. But in some cases, there are serious issues related to
// overloading for nested normal routines. For example,
//
//     routine Test( a: string ) { io.writeln( "Test(a:string)" ) }
//     routine MakeRoutine(){
//         var loc = 123
//         routine Test( a: int ) {
//             io.writeln( "MakeRoutine::Test(a:int)" )
//             return a + loc
//         }
//         Test( 'abc' )  # Should be allowed, so overloading is required;
//         return Test    # What Test should it be?
//     }
//     test = MakeRoutine()
//     test( 'abc' )  # Allowed? Does it make sense to allow or not allow it?
//
// With this example, the problem of supporting normal routines nested in other
// routines can be clearly seen. Anonymous routine and closure have no such issues.
 */
static int DaoParser_ParseRoutineDefinition( DaoParser *self, int start, int from, int to, int attribs )
{
	DaoToken *ptok, **tokens = self->tokens->items.pToken;
	DaoNamespace *NS = self->nameSpace;
	DaoRoutine *rout = NULL;
	DaoParser *parser = NULL;
	DaoRoutine *declared = NULL;
	DaoValue *value = NULL, *scope = NULL;
	DaoClass *klass = NULL;
	DString *mbs = self->string;
	DString *mbs2 = self->string2;
	int perm = self->permission;
	int tki = tokens[start]->name;
	int k, id, right = -1;
	int errorStart = start;

	start += 1;
	if( start+2 > to ) goto InvalidDefinition;
	if( tokens[start+1]->name == DTOK_COLON2 ){
		/* For methods define outside the class body: */
		int oldpos = start, evalMode = self->evalMode;
		self->evalMode |= DAO_CONST_EVAL_METHDEF;
		start = DaoParser_ParseScopedConstOrName( self, & scope, & value, start, DTOK_LB );
		self->evalMode = evalMode;
		if( start < 0 || scope == NULL || scope->type != DAO_CLASS ){
			DaoToken *ptok = tokens[ oldpos ];
			self->curLine = ptok->line;
			if( start >= 0 ) DaoParser_Error( self, DAO_SYMBOL_NEED_CLASS, & ptok->string );
			goto InvalidDefinition;
		}
		klass = (DaoClass*) scope;
		DString_Assign( mbs, & tokens[start]->string );
		if( value == NULL ){
			int i, lb = start;
			if( tokens[start]->name == DTOK_LB ){ /* operator () or operator (type) */
				lb = DaoParser_FindPairToken( self, DTOK_LB, DTOK_RB, lb, -1 );
				if( lb < 0 ) goto InvalidDefinition;
			}
			lb = DaoParser_FindOpenToken( self, DTOK_LB, lb+1, -1, 1 );
			if( lb < 0 ) goto InvalidDefinition;
			DString_Reset( mbs, 0 );
			for(i=start; i<lb; i++) DString_Append( mbs, & tokens[i]->string );
			i = DaoClass_FindConst( klass, mbs );
			if( i & DAO_CLASS_CONSTANT ) value = DaoClass_GetConst( klass, i );
		}
		if( value == NULL ){
			DaoParser_Error( self, DAO_ROUT_NOT_DECLARED, mbs );
			goto InvalidDefinition;
		}
		if( value->type == DAO_CLASS ) value = (DaoValue*) value->xClass.initRoutine;
		if( value->type != DAO_ROUTINE || value->xRoutine.routHost != klass->objType ){
			goto InvalidDefinition;
		}
		rout = (DaoRoutine*) value;

		parser = DaoParser_NewRoutineParser( self, start, attribs );
		GC_Assign( & parser->routine->routHost, klass->objType );
		parser->routine->attribs |= attribs;
		parser->hostType = klass->objType;
		parser->hostClass = klass;
		right = DaoParser_ParseSignature( self, parser, start );
		if( right < 0 ) goto InvalidDefinition;
		if( DString_EQ( self->nameSpace->name, klass->initRoutine->nameSpace->name ) == 0 ){
			DaoParser_Error2( self, DAO_ROUT_MISPLACED_IMPLEMENTATION, errorStart+1, right, 0 );
			goto InvalidDefinition;
		}
		DString_Assign( mbs, parser->routine->routName );
		DString_AppendChar( mbs, ':' );
		DString_Append( mbs, parser->routine->routType->name );
		rout = DaoClass_GetOverloadedRoutine( klass, mbs );
		if( rout == NULL ){
			DMap *signatures = klass->methSignatures;
			DNode *it = DMap_First( signatures );
			int defined = 0;
			for(; it; it=DMap_Next(signatures,it)){
				DaoRoutine *meth = (DaoRoutine*) it->value.pValue;
				if( DString_EQ( meth->routName, parser->routine->routName ) && meth->body ){
					if( meth->body->codeStart ==0 ){
						self->curLine = meth->defLine;
						DaoParser_Error( self, DAO_ROUT_DECLARED_SIGNATURE, it->key.pString );
					}
					defined = 1;
				}
			}
			self->curLine = tokens[ start+1 ]->line;
			if( defined ){
				DaoParser_Error( self, DAO_ROUT_WRONG_SIGNATURE, mbs );
			}else{
				DaoParser_Error2( self, DAO_ROUT_NOT_DECLARED, errorStart+1, right, 0 );
			}
			goto InvalidDefinition;
		}else if( rout->body->codeStart > 0 ){
			self->curLine = rout->defLine;
			DaoParser_Error2( self, DAO_ROUT_WAS_IMPLEMENTED, errorStart+1, right, 0 );
			self->curLine = tokens[ start+1 ]->line;
			DaoParser_Error2( self, DAO_ROUT_REDUNDANT_IMPLEMENTATION, errorStart+1, right, 0 );
			goto InvalidDefinition;
		}
		parser->routine = rout;
	}else if( self->isClassBody ){
		klass = self->hostClass;
		parser = DaoParser_NewRoutineParser( self, start, attribs );
		rout = parser->routine;
		right = DaoParser_ParseSignature( self, parser, start );
		if( right < 0 ) goto InvalidDefinition;
		DString_Assign( mbs, rout->routName );
		DString_AppendChar( mbs, ':' );
		DString_Append( mbs, rout->routType->name );
		declared = DaoClass_GetOverloadedRoutine( klass, mbs );
		if( declared ){
			rout = DaoParser_CheckDeclared( self, parser->routine, declared );
			if( rout == NULL ) goto InvalidDefinition;
		}

		if( rout == parser->routine ){
			switch( perm ){
			case DAO_PERM_PRIVATE   : rout->attribs |= DAO_ROUT_PRIVATE; break;
			case DAO_PERM_PROTECTED : rout->attribs |= DAO_ROUT_PROTECTED; break;
			}
			DaoClass_AddOverloadedRoutine( klass, mbs, rout );
			if( rout->attribs & DAO_ROUT_INITOR ){ /* overloading constructor */
				/* DRoutines_Add( klass->initRoutines->overloads, rout ); */
				DaoClass_AddConst( klass, klass->initRoutine->routName, (DaoValue*)rout, perm );
			}else{
				DaoClass_AddConst( klass, rout->routName, (DaoValue*)rout, perm );
			}
		}else{
			parser->routine = rout;
		}
	}else if( self->isInterBody ){
		parser = DaoParser_NewRoutineParser( self, start, attribs );
		rout = parser->routine;
		right = DaoParser_ParseSignature( self, parser, start );
		if( right < 0 ) goto InvalidDefinition;
		GC_Assign( & rout->routHost, self->hostInter->abtype );
		parser->hostInter = self->hostInter;
		parser->hostType = self->hostType;
		DaoMethods_Insert( self->hostInter->methods, rout, NS, rout->routHost );
	}else if( self->isCinTypeBody ){
		parser = DaoParser_NewRoutineParser( self, start, attribs );
		rout = parser->routine;
		right = DaoParser_ParseSignature( self, parser, start );
		if( right < 0 ) goto InvalidDefinition;
		GC_Assign( & rout->routHost, self->hostCinType->vatype );
		parser->hostCinType = self->hostCinType;
		parser->hostType = self->hostType;
		DaoMethods_Insert( self->hostCinType->methods, rout, NS, rout->routHost );
	}else{
		/* Nested normal routine not allowed: */
		if( self->lexLevel != 0 ) goto InvalidDefinition; /* TODO: better information; */

		parser = DaoParser_NewRoutineParser( self, start, attribs );
		right = DaoParser_ParseSignature( self, parser, start );
		if( right < 0 ) goto InvalidDefinition;

		rout = parser->routine;
		id = DaoNamespace_FindConst( NS, parser->routine->routName );
		declared = (DaoRoutine*) DaoNamespace_GetConst( NS, id );
		if( declared ){
			rout = DaoParser_CheckDeclared( self, parser->routine, declared );
			if( rout == NULL ) goto InvalidDefinition;
		}

		if( rout == parser->routine ){
			if( strcmp( rout->routName->chars, "main" ) ==0 ) rout->attribs |= DAO_ROUT_MAIN;
			DaoNamespace_AddConst( NS, rout->routName, (DaoValue*) rout, perm );
		}else{
			parser->routine = rout;
		}
	}
	if( attribs && rout->routHost == NULL ){
		int efrom = errorStart - (attribs != 0);  /* XXX */
		DaoParser_Error2( self, DAO_INVALID_STORAGE, efrom, errorStart+1, 0 );
		goto InvalidDefinition;
	}
	if( self->byteBlock ){
		parser->byteCoder = self->byteCoder;
		parser->byteBlock = DaoByteBlock_AddRoutineBlock( self->byteBlock, parser->routine, perm );
	}
	if( self->isClassBody && tokens[right]->name == DTOK_RCB ){
		DList_Append( self->routCompilable, parser );
		return right + 1;
	}
	if( tokens[right]->name == DTOK_RCB ){ /* with body */
		if( rout->body == NULL ){
			DaoParser_Error2( self, DAO_ROUT_REDUNDANT_IMPLEMENTATION, errorStart+1, right, 0 );
			goto InvalidDefinition;
		}
		if( DaoParser_ParseRoutine( parser ) == 0 ) goto Failed;
		if( parser->usingGlobal ) DList_Append( self->routReInferable, parser->routine );
	}
	if( parser ) DaoVmSpace_ReleaseParser( self->vmSpace, parser );
	return right + 1;
InvalidDefinition:
	DaoParser_Error3( self, DAO_INVALID_FUNCTION_DEFINITION, errorStart );
Failed:
	if( parser ) DaoVmSpace_ReleaseParser( self->vmSpace, parser );
	return -1;
}
static int DaoParser_ParseCodes( DaoParser *self, int from, int to );
static int DaoParser_ParseInterfaceDefinition( DaoParser *self, int start, int to, int storeType )
{
	DaoToken **tokens = self->tokens->items.pToken;
	DaoRoutine *routine = self->routine;
	DaoNamespace *NS = self->nameSpace;
	DaoNamespace *ns = NULL;
	DaoInterface *inter = NULL;
	DaoCinType *cintype = NULL;
	DaoParser *parser = NULL;
	DaoValue *value = NULL;
	DaoToken *tokName;
	DString *interName;
	DString *ename = NULL;
	int i, right, ec = 0, errorStart = start;
	int pm = self->permission;

	parser = NULL;
	if( start+1 > to ) goto ErrorInterfaceDefinition;
	tokName = tokens[start+1];
	interName = ename = & tokName->string;
	start = DaoParser_ParseScopedConstOrName( self, NULL, & value, start + 1, DTOK_LCB );
	if( start <0 ) goto ErrorInterfaceDefinition;
	if( value == NULL || value->type == 0 ){
		int t = tokens[start]->name;
		if( (t != DTOK_IDENTIFIER && t < DKEY_CEIL) || t > DKEY_TANH ) goto ErrorInterfaceDefinition;
		interName = & tokens[start]->string;
		if( tokens[start+1]->name == DKEY_FOR ){
			ename = interName;
			ec = DAO_SYMBOL_NEED_INTERFACE;
		}
		inter = DaoInterface_New( interName->chars );
		if( routine != NS->mainRoutine ) ns = NULL;
		value = (DaoValue*) inter;
		DaoParser_AddToScope( self, interName, value, inter->abtype, storeType );

		if( start+1 <= to && tokens[start+1]->name == DTOK_SEMCO ){
			if( self->byteBlock ){
				DaoByteBlock_AddInterfaceBlock( self->byteBlock, inter, self->permission );
			}
			start += 2;
			return start;
		}
	}else if( value->type == DAO_INTERFACE ){
		inter = (DaoInterface*) value;
		if( tokens[start+1]->name == DKEY_FOR ){
			DString *name = DaoParser_GetString( self );
			DaoType *target;
			
			target = DaoParser_ParseType( self, start+2, to, & start, NULL );
			if( target == NULL ){
				ec = DAO_INVALID_INTERFACE_TARGET;
				goto ErrorInterfaceDefinition;
			}
			start -= 1;

			cintype = DaoCinType_New( inter, target );
		}else if( value->xInterface.derived ){
			ec = DAO_SYMBOL_WAS_DEFINED;
			goto ErrorInterfaceDefinition;
		}
	}else{
		ec = DAO_SYMBOL_WAS_DEFINED;
		goto ErrorInterfaceDefinition;
	}
	start += 1; /* token after class name. */
	if( tokens[start]->name == DTOK_COLON ){
		/* interface AB : NS::BB, CC{ } */
		unsigned char sep = DTOK_COLON;
		while( tokens[start]->name == sep ){
			int estart = start + 1;
			DaoType *sutype = DaoParser_ParseType( self, start+1, to, & start, NULL );
			if( start < 0 ) goto ErrorInterfaceBase;
			ename = & tokens[estart]->string;
			if( sutype == NULL || (sutype->tid != DAO_INTERFACE && sutype->tid != DAO_CINVALUE ) ){
				ec = DAO_SYMBOL_NEED_INTERFACE;
				goto ErrorInterfaceBase;
			}
			if( cintype == NULL && sutype->tid != DAO_INTERFACE ){
				ec = DAO_INVALID_PARENT_INTERFACE;
				goto ErrorInterfaceDefinition;
			}else if( cintype != NULL && sutype->tid != DAO_CINVALUE ){
				ec = DAO_INVALID_PARENT_INTERFACE;
				goto ErrorInterfaceDefinition;
			}
			/* Add a reference to its super interfaces: */
			if( cintype ){
				DaoCinType *cinbase = (DaoCinType*) sutype->aux;
				int childof = DaoType_ChildOf( inter->abtype, cinbase->abstract->abtype );
				if( inter == cinbase->abstract || childof == 0 ){
					ec = DAO_INVALID_PARENT_INTERFACE;
					goto ErrorInterfaceDefinition;
				}
				DList_Append( cintype->supers, sutype->aux );
			}else{
				DList_Append( inter->supers, sutype->aux );
			}
			sep = DTOK_COMMA;
		}
	}
	DaoNamespace_InitConstEvalData( NS );
	parser = DaoVmSpace_AcquireParser( self->vmSpace );
	parser->routine = NS->constEvalRoutine;
	parser->vmSpace = self->vmSpace;
	parser->nameSpace = NS;
	parser->hostInter = inter;
	parser->isInterBody = 1;
	parser->levelBase = self->levelBase + self->lexLevel + 1;
	parser->hostType = inter->abtype;

	if( cintype ){
		parser->hostType = cintype->vatype;
		parser->hostCinType = cintype;
		parser->isCinTypeBody = 1;
		parser->hostInter = NULL;
		parser->isInterBody = 0;
	}

	DString_Assign( parser->fileName, self->fileName );

	right = tokens[start]->name == DTOK_LCB ?
		DaoParser_FindPairToken( self, DTOK_LCB, DTOK_RCB, start, -1 ) : -1 ;
	if( right < 0 ) goto ErrorInterfaceDefinition;

	if( self->byteBlock ){
		if( cintype ){
			parser->byteBlock = DaoByteBlock_AddCinTypeBlock( self->byteBlock, cintype, pm );
		}else{
			parser->byteBlock = DaoByteBlock_AddInterfaceBlock( self->byteBlock, inter, pm );
		}
		parser->byteCoder = self->byteCoder;
	}

	if( cintype ){
		DaoCinType_DeriveMethods( cintype );
	}else{
		DaoInterface_DeriveMethods( inter );
	}
	for(i=start+1; i<right; i++) DaoLexer_AppendToken( parser->lexer, tokens[i] );

	if( DaoParser_ParseCodes( parser, 0, parser->tokens->size-1 )==0 ){
		if( DString_EQ( self->fileName, parser->fileName ) )
			DList_InsertList( self->errors, self->errors->size, parser->errors, 0, -1 );
		else
			DaoParser_PrintError( parser, 0, 0, NULL );
		goto ErrorInterfaceDefinition;
	}
	if( parser->vmcLast != parser->vmcBase ){
		DList_InsertList( self->errors, self->errors->size, parser->errors, 0, -1 );
		DaoParser_StatementError( self, parser, DAO_STATEMENT_IN_INTERFACE );
		goto ErrorInterfaceDefinition;
	}
	DaoVmSpace_ReleaseParser( self->vmSpace, parser );
	if( cintype ){
		if( DaoType_MatchInterface( cintype->vatype, inter, NULL ) == 0 ){
			ename = cintype->vatype->name;
			ec = DAO_INCOMPLETE_INTERFACE_IMPL;
			goto ErrorInterfaceDefinition;
		}
		if( inter->concretes == NULL ){
			inter->concretes = DHash_New(0,DAO_DATA_VALUE);
		}
		DMap_Insert( inter->concretes, cintype->target, cintype );
	}
	return right + 1;
ErrorInterfaceBase:
	DaoParser_Error( self, DAO_SYMBOL_NEED_INTERFACE, ename );
ErrorInterfaceDefinition:
	if( cintype ) DaoGC_TryDelete( (DaoValue*) cintype );
	if( parser ) DaoVmSpace_ReleaseParser( self->vmSpace, parser );
	if( ec ) DaoParser_Error( self, ec, ename );
	DaoParser_Error2( self, DAO_INVALID_INTERFACE_DEFINITION, errorStart, to, 0 );
	return -1;
}
static int DaoParser_CompileRoutines( DaoParser *self )
{
	daoint i, error = 0;
	for(i=0; i<self->routCompilable->size; i++){
		DaoParser *parser = (DaoParser*) self->routCompilable->items.pValue[i];
		DaoRoutine *rout = parser->routine;
		if( self->byteBlock ){
			parser->byteCoder = self->byteCoder;
			parser->byteBlock = DaoByteBlock_AddRoutineBlock( self->byteBlock, parser->routine, 0 );
		}
		error |= DaoParser_ParseRoutine( parser ) == 0;
		if( parser->usingGlobal ) DList_Append( self->routReInferable, rout );
		if( error ) DaoParser_PrintError( parser, 0, 0, NULL );
		DaoVmSpace_ReleaseParser( self->vmSpace, parser );
		if( error ) break;
	}
	self->routCompilable->size = 0;
	return error == 0;
}
int DList_MatchAffix( DList *self, DString *name );
int DaoClass_UseMixinDecorators( DaoClass *self );
static int DaoParser_ParseClassDefinition( DaoParser *self, int start, int to, int storeType )
{
	DaoToken **tokens = self->tokens->items.pToken;
	DaoNamespace *NS = self->nameSpace;
	DaoNamespace *ns = NULL;
	DaoRoutine *routine = self->routine;
	DaoRoutine *rout = NULL;
	DaoParser *parser = NULL;
	DaoClass *klass = NULL;
	DaoValue *value = NULL;
	DaoToken *tokName;
	DString *str, *mbs = DaoParser_GetString( self );
	DString *className, *ename = NULL;
	daoint begin, line = self->curLine;
	daoint i, k, rb, right, error = 0;
	int errorStart = start;
	int immutable = storeType == DAO_DECL_INVAR;
	int pm1, pm2, ec = 0;

	if( start+1 > to ) goto ErrorClassDefinition;
	tokName = tokens[start+1];
	className = ename = & tokName->string;
	start = DaoParser_ParseScopedConstOrName( self, NULL, & value, start+1, DTOK_LCB );
	if( start <0 ) goto ErrorClassDefinition;
	ename = & tokens[start]->string;
	if( value == NULL || value->type == 0 ){
		DString *name = & tokens[start]->string;
		int t = tokens[start]->name;
		if( t != DTOK_IDENTIFIER && t != DTOK_ID_THTYPE && t < DKEY_CEIL ) goto ErrorClassDefinition;
		klass = DaoClass_New();
		if( immutable ) klass->attribs |= DAO_CLS_INVAR;

		className = klass->className;
		DString_Assign( className, name );
		DaoClass_SetName( klass, className, NS );

		DList_Append( NS->definedRoutines, klass->initRoutine );
		if( routine != NS->mainRoutine ) ns = NULL;
		value = (DaoValue*) klass;
		DaoParser_AddToScope( self, className, value, klass->objType, storeType );

		if( start < to && tokens[start+1]->name == DTOK_BANG2 ){
			klass->attribs |= DAO_CLS_ASYNCHRONOUS;
			start += 1;
		}
		if( self->byteBlock ){
			int pm = self->permission;
			DaoByteBlock_AddClassBlock( self->byteBlock, klass, pm );
		}

		if( start+1 <= to ){
			int tkn = tokens[start+1]->name;
			if( tkn != DTOK_LB && tkn != DTOK_COLON && tkn != DTOK_LCB && tkn != DKEY_FOR ){
				start += 1;
				return start;
			}
		}else{
			return start;
		}
	}else if( value->type != DAO_CLASS ){
		ec = DAO_SYMBOL_WAS_DEFINED;
		goto ErrorClassDefinition;
	}else if( value->xClass.derived ){
		klass = & value->xClass;
		ec = DAO_SYMBOL_WAS_DEFINED;
		goto ErrorClassDefinition;
	}else{
		klass = & value->xClass;
	}

	rout = klass->initRoutine;
	rout->defLine = tokens[start]->line;
	GC_Assign( & rout->nameSpace, NS );

	parser = DaoVmSpace_AcquireParser( self->vmSpace );
	parser->vmSpace = self->vmSpace;
	parser->routine = rout;
	parser->isClassBody = 1;
	parser->hostClass = klass;
	parser->nameSpace = NS;
	parser->levelBase = self->levelBase + self->lexLevel + 1;
	parser->hostType = klass->objType;

	DString_Assign( parser->fileName, self->fileName );

	start ++; /* token after class name. */
	if( start > to ) goto ErrorClassDefinition;

	/* Apply aspects (to normal classes only): */
	if( klass->className->chars[0] != '@' ){
		DNode *it;
		for(it=DMap_First(NS->lookupTable); it; it=DMap_Next(NS->lookupTable,it)){
			int id = LOOKUP_ID( it->value.pInt );
			DaoClass *mixin = (DaoClass*) NS->constants->items.pConst[id]->value;
			if( LOOKUP_ST( it->value.pInt ) != DAO_GLOBAL_CONSTANT ) continue;
			if( LOOKUP_UP( it->value.pInt ) > 1 ) continue; /* skip indirectly loaded; */
			if( mixin->type != DAO_CLASS ) continue;
			if( mixin->parent != NULL ) continue;
			if( mixin->className->chars[0] != '@' ) continue; /* Not an aspect class; */
			if( DList_MatchAffix( mixin->decoTargets, klass->className ) == 0 ) continue;
			DaoClass_AddMixinClass( klass, mixin );
		}
	}

	if( start <= to && tokens[start]->name == DTOK_LB ){
		int rb = DaoParser_FindPairToken( self, DTOK_LB, DTOK_RB, start, -1 );
		unsigned char sep = DTOK_LB;
		while( tokens[start]->name == sep ){
			DaoClass *mixin;
			start = DaoParser_FindMaybeScopedConst( self, & value, start+1, 0 );
			if( start <0 ) goto ErrorClassDefinition;
			ename = & tokens[start]->string;
			if( value == NULL || value->type != DAO_CLASS ){
				ec = DAO_SYMBOL_NEED_CLASS;
				if( value == NULL || value->type == 0 || value->type == DAO_STRING )
					ec = DAO_SYMBOL_POSSIBLY_UNDEFINED;
				goto ErrorClassDefinition;
			}
			mixin = (DaoClass*) value;
			if( mixin->parent != NULL ){
				/* Class with parent classes cannot be used as mixin: */
				ec = DAO_INVALID_MIXIN_CLASS;
				goto ErrorClassDefinition;
			}
			DaoClass_AddMixinClass( klass, mixin );
			sep = DTOK_COMMA;
			start ++;
		}
		start += 1;
	}

	if( tokens[start]->name == DTOK_COLON ){
		/* class AA : NS::BB { } */
		int async = DAO_CLS_ASYNCHRONOUS;
		DaoClass *base = NULL;
		DaoType *sutype = DaoParser_ParseType( self, start+1, to, & start, NULL );
		if( start < 0 ) goto ErrorClassDefinition;
		ename = & tokens[start-1]->string;
		if( sutype == NULL || sutype->tid < DAO_OBJECT || sutype->tid > DAO_CDATA ){
			ec = DAO_SYMBOL_NEED_CLASS_CTYPE;
			if( sutype == NULL ) ec = DAO_SYMBOL_POSSIBLY_UNDEFINED;
			goto ErrorClassDefinition;
		}
		base = (DaoClass*) sutype->aux;
		if( base == NULL ){
			ec = DAO_SYMBOL_POSSIBLY_UNDEFINED;
			goto ErrorClassDefinition;
		}
		if( base->type == DAO_CLASS && (base->attribs & async) != (klass->attribs & async) ){
			goto ErrorClassDefinition;
		}
		if( klass->parent ){
			ec = DAO_TOO_MANY_PARENT_TYPES;
			goto ErrorClassDefinition;
		}
		/* Add a reference to its base classes: */
		DaoClass_AddSuperClass( klass, (DaoValue*) base );
	} /* End parsing base classes */

	if( klass->attribs & DAO_CLS_INVAR ){
		for(i=0; i<klass->allBases->size; ++i){
			DaoValue *base = klass->allBases->items.pValue[i];
			DaoType *btype = NULL;
			switch( base->type ){
			case DAO_CLASS : btype = base->xClass.objType; break;
			case DAO_CTYPE : btype = base->xCtype.cdtype;  break;
			}
			if( DaoType_IsImmutable( btype ) == 0 ){
				DaoParser_Error( self, DAO_INVALID_PARENT_CLASS, btype->name );
				goto ErrorClassDefinition;
			}
		}
	}

	if( tokens[start]->name == DKEY_FOR ){
		ec = DAO_INVALID_DECO_PATTERN;
		if( klass->className->chars[0] != '@' ) goto ErrorClassDefinition;
		if( klass->decoTargets == NULL ) klass->decoTargets = DList_New( DAO_DATA_STRING );
		start = DaoParser_ParseDecoTargets( self, start+1, to, klass->decoTargets );
		if( start < 0 ) goto ErrorClassDefinition;
		ec = 0;
	}
	if( self->byteBlock ){
		parser->byteBlock = DaoByteBlock_AddClassBlock( self->byteBlock, klass, self->permission );
		parser->byteCoder = self->byteCoder;
	}
	begin = start;
	right = tokens[start]->name == DTOK_LCB ?
		DaoParser_FindPairToken( self, DTOK_LCB, DTOK_RCB, start, -1 ) : -1 ;

	if( right < 0 ) goto ErrorClassDefinition;

	if( DaoClass_DeriveClassData( klass ) == 0 ) goto ErrorClassDefinition;
	DaoClass_DeriveObjectData( klass ); /* Moved before parsing the body, for bytecode; */

	for(i=begin+1; i<right; i++) DaoLexer_AppendToken( parser->lexer, tokens[i] );
	if( DaoParser_ParseCodes( parser, 0, parser->tokens->size-1 )==0 ){
		if( DString_EQ( self->fileName, parser->fileName ) )
			DList_InsertList( self->errors, self->errors->size, parser->errors, 0, -1 );
		else
			DaoParser_PrintError( parser, 0, 0, NULL );
		goto ErrorClassDefinition;
	}
	DaoList_Clear( klass->initRoutine->routConsts );
	if( parser->vmcLast != parser->vmcBase ){
#if 0
		DaoParser_PrintCodes( parser );
#endif
		DList_InsertList( self->errors, self->errors->size, parser->errors, 0, -1 );
		DaoParser_StatementError( self, parser, DAO_STATEMENT_IN_CLASS );
		goto ErrorClassDefinition;
	}
	for(i=0; i<klass->cstDataName->size; i++){
		DNode *it1, *it2;
		value = klass->constants->items.pConst[i]->value;
		if( value == NULL || value->type != DAO_ROUTINE ) continue;
		if( value->xRoutine.routName->chars[0] != '.' ) continue;
		DString_SetChars( mbs, value->xRoutine.routName->chars + 1 );
		if( mbs->chars[ mbs->size - 1 ] == '=' ) DString_Erase( mbs, mbs->size - 1, 1 );
		it1 = MAP_Find( klass->lookupTable, value->xRoutine.routName );
		it2 = MAP_Find( klass->lookupTable, mbs );
		if( it1 == NULL || it2 == NULL ) continue;
		pm1 = LOOKUP_PM( it1->value.pInt );
		pm2 = LOOKUP_PM( it2->value.pInt );
		if( pm1 <= pm2 || pm2 != DAO_PERM_PRIVATE ){
			self->curLine = value->xRoutine.defLine;
			DaoParser_Warn( self, DAO_WARN_GET_SETTER, mbs );
		}
	}
	DaoClass_UpdateAttributes( klass );
	if( parser->byteBlock ){
		DaoByteCoder_EncodeUInt32( parser->byteBlock->begin+4, klass->attribs );
	}
	error = DaoParser_CompileRoutines( parser ) == 0;
	if( error == 0 && klass->initRoutines->overloads->routines->size == 0 ){
		DString *ctorname = klass->cstDataName->items.pString[DAO_CLASS_CONST_CSTOR];
		DList_Clear( parser->tokens );
		DaoParser_AddDefaultInitializer( parser, klass, 0 );
		error |= DaoParser_ParseRoutine( parser ) == 0;
		DaoClass_AddConst( klass, ctorname, (DaoValue*)klass->initRoutine, DAO_PERM_PUBLIC );
		if( parser->byteBlock ){
			DaoByteBlock *bk = parser->byteBlock;
			bk = DaoByteBlock_AddRoutineBlock( bk, klass->initRoutine, DAO_PERM_PUBLIC );
			DaoByteCoder_FinalizeRoutineBlock( self->byteCoder, bk );
		}
	}
	DaoVmSpace_ReleaseParser( self->vmSpace, parser );
	DaoClass_UpdateMixinConstructors( klass );
	DaoClass_UpdateVirtualMethods( klass );
	if( error ) return -1;

	if( self->byteBlock == NULL ){
		if( DaoClass_UseMixinDecorators( klass ) == 0 ){
			DaoParser_Error( self, DAO_INVALID_FUNCTION_DECORATION, NULL );
			goto ErrorClassDefinition;
		}
	}

	return right + 1;
ErrorClassDefinition:
	if( parser ) DaoVmSpace_ReleaseParser( self->vmSpace, parser );
	if( ec ) DaoParser_Error( self, ec, ename );
	DaoParser_Error2( self, DAO_INVALID_CLASS_DEFINITION, errorStart, to, 0 );
	return -1;
}
static int DaoParser_ParseEnumDefinition( DaoParser *self, int start, int to, int storeType )
{
	DaoToken *ptok, **tokens = self->tokens->items.pToken;
	DaoType *abtp, *abtp2;
	DString *str, *alias = NULL;
	DaoValue *dv = NULL;
	DMap *values = NULL;
	int sep = DTOK_COMMA, value = 0;
	int id, rb, comma, semco, explicitdef=0;
	int enumkey = start + 1;
	int reg, cst = 0;
	char buf[32];
	rb = -1;
	abtp = NULL;
	rb = DaoParser_FindPairToken( self, DTOK_LCB, DTOK_RCB, start+1, -1 );
	if( tokens[start+2]->type != DTOK_LCB || rb <0 ) goto ErrorEnumDefinition;
	ptok = tokens[start+1];
	if( ptok->type != DTOK_IDENTIFIER ) goto ErrorEnumDefinition;
	alias = & ptok->string;
	if( ptok->name != DTOK_ID_SYMBOL && ptok->name != DTOK_ID_THTYPE ) start += 1;
	if( (id = DaoParser_GetRegister( self, ptok )) >=0 ){
		DaoParser_Error( self, DAO_SYMBOL_WAS_DEFINED, alias );
		goto ErrorEnumDefinition;
	}
	if( DaoNamespace_FindType( self->nameSpace, alias) ){
		DaoParser_Error( self, DAO_SYMBOL_WAS_DEFINED, alias );
		goto ErrorEnumDefinition;
	}

	abtp = DaoType_New( "enum<", DAO_ENUM, NULL, NULL );
	comma = DaoParser_FindOpenToken( self, DTOK_COMMA, start+2, -1, 0 );
	semco = DaoParser_FindOpenToken( self, DTOK_SEMCO, start+2, -1, 0 );
	if( comma >=0 && semco >=0 ){
		if( semco < comma ){
			sep = DTOK_SEMCO;
			comma = semco;
		}
	}else if( semco >=0 ){
		sep = DTOK_SEMCO;
		comma = semco;
	}
	if( comma <0 ) comma = rb;
	values = DHash_New(0,0);
	value = sep == DTOK_SEMCO;
	abtp->subtid = sep == DTOK_SEMCO ? DAO_ENUM_FLAG : DAO_ENUM_STATE;
	abtp->value->xBase.subtype = abtp->subtid;
	start = start + 2;
	while( comma >=0 ){
		if( start >= comma ) break;
		if( tokens[start]->name != DTOK_IDENTIFIER ){
			DaoParser_Error( self, DAO_TOKEN_NEED_NAME, & tokens[start]->string );
			goto ErrorEnumDefinition;
		}
		str = & tokens[start]->string;
		if( DMap_Find( abtp->mapNames, str ) ){
			DaoParser_Error( self, DAO_SYMBOL_WAS_DEFINED, str );
			goto ErrorEnumDefinition;
		}
		explicitdef = 0;
		if( tokens[start+1]->type == DTOK_ASSN ){
			explicitdef = 1;
			reg = DaoParser_MakeArithTree( self, start+2, comma-1, & cst );
			if( reg < 0 ) goto ErrorEnumDefinition;
			dv = NULL;
			if( cst ) dv = DaoParser_GetVariable( self, cst );
			if( dv == NULL || dv->type < DAO_INTEGER || dv->type > DAO_FLOAT ){
				DaoParser_Error( self, DAO_EXPR_NEED_CONST_NUMBER, & tokens[start+2]->string );
				goto ErrorEnumDefinition;
			}
			value = DaoValue_GetInteger( dv );
			if( DMap_Find( values, IntToPointer( value ) ) != NULL ){
				DaoParser_Error2( self, DAO_VALUE_WAS_USED, start+2, self->curToken, 0 );
				goto ErrorEnumDefinition;
			}
			DMap_Insert( values, IntToPointer( value ), 0 );
		}else if( start+1 != rb && tokens[start+1]->type != sep ){
			DaoParser_Error( self, DAO_TOKEN_NOT_EXPECTED, & tokens[start+1]->string );
			goto ErrorEnumDefinition;
		}
		if( abtp->mapNames->size ){
			DString_AppendChar( abtp->name, sep == DTOK_SEMCO ? ';' : ',' );
		}
		DString_Append( abtp->name, str );
		if( explicitdef ){
			sprintf( buf, "=%i", value );
			DString_AppendChars( abtp->name, buf );
		}
		DMap_Insert( abtp->mapNames, str, (void*)(daoint)value );
		if( sep == DTOK_SEMCO ) value <<= 1; else value += 1;
		if( comma == rb ) break;
		start = comma + 1;
		comma = DaoParser_FindOpenToken( self, sep, comma+1, -1, 0 );
		if( comma <0 ) comma = rb;
	}
	DString_AppendChar( abtp->name, '>' );
	DString_Assign( abtp->fname, abtp->name );
	abtp2 = DaoNamespace_FindType( self->nameSpace, abtp->name );
	if( abtp2 ){
		DaoType_Delete( abtp );
	}else{
		DaoNamespace_AddType( self->nameSpace, abtp->name, abtp );
		abtp2 = abtp;
	}
	if( alias ){
		DaoType *old = abtp2;
		abtp2 = DaoType_Copy( abtp2 );
		DString_Assign( abtp2->name, alias );
		DaoNamespace_AddType( self->nameSpace, abtp2->name, abtp2 );
		DaoParser_AddConstant( self, abtp2->name, (DaoValue*) abtp2, tokens[enumkey] );
		if( self->byteBlock ){
			DaoByteBlock_EncodeTypeAlias( self->byteBlock, old, abtp2, alias, NULL, DAO_PERM_PUBLIC );
		}
	}
	if( values ) DMap_Delete( values );
	return rb + 1;
ErrorEnumDefinition:
	if( rb >=0 )
		DaoParser_Error2( self, DAO_INVALID_ENUM_DEFINITION, start, rb, 0 );
	else
		DaoParser_Error3( self, DAO_INVALID_ENUM_DEFINITION, start );
	if( values ) DMap_Delete( values );
	if( abtp ) DaoType_Delete( abtp );
	return 0;
}
static int DaoParser_GetNormRegister( DaoParser *self, int reg, int exp, int first, int mid, int last );
static void DaoParser_CheckStatementSeparation( DaoParser *self, int check, int end )
{
	DaoInode *close = (DaoInode*) DList_Back( self->scopeClosings );
	DaoToken **tokens = self->tokens->items.pToken;
	if( check >= end ) return;
	self->curLine = tokens[check]->line;
	if( tokens[check]->line != tokens[check+1]->line ) return;
	switch( tokens[check+1]->name ){
	case DTOK_RCB : case DTOK_SEMCO : case DKEY_ELSE : break;
	case DKEY_WHILE : if( close && close->a == DKEY_WHILE ) break; /* else fall through; */
	default : DaoParser_Warn( self, DAO_WARN_STATEMENT_SEPARATION, NULL ); break;
	}
}
static int DaoParser_ImportSymbols( DaoParser *self, DaoNamespace *mod, int start, int to, int level );
static int DaoParser_ParseImportStatement( DaoParser *self, int start, int end, int full );
static int DaoParser_ParseNamespaceStatement( DaoParser *self, int start, int end );
static int DaoParser_MultipleAssignment( DaoParser *self, int start, int rb, int to, int store );
static int DaoParser_ParseVarExpressions( DaoParser *self, int start, int to, int st );
static int DaoParser_GetEnumTokenType( DaoType *type )
{
	int tok = 0;
	switch( type ? type->tid : 0 ){
	case DAO_ARRAY : tok = DTOK_LSB; break;
	case DAO_LIST  : tok = DTOK_LCB; break;
	case DAO_MAP   : tok = DTOK_LCB; break;
	case DAO_TUPLE : tok = DTOK_LB; break;
	}
	return tok;
}
static int DaoParser_ParseCodes( DaoParser *self, int from, int to )
{
	DaoNamespace *ns = self->nameSpace;
	DaoVmSpace *vmSpace = self->vmSpace;
	DaoRoutine *routine = self->routine;
	DaoClass *hostClass = self->hostClass;

	DaoEnode enode = {-1,0,0,0,NULL,NULL,NULL,NULL};
	DaoToken **tokens = self->tokens->items.pToken;
	DaoToken *ptok;
	DaoType *casetype;
	DMap *switchTable;
	DString *switchName;
	int switchType, switchNameType, caseConst;
	int startTriples, triples = self->tokenTriples->size;
	int regcount = self->regCount;
	int cons = (vmSpace->options & DAO_OPTION_INTERUN) && (ns->options & DAO_NS_AUTO_GLOBAL);
	int i, k, lb, rb, end, start = from, N = 0;
	int reg, reg1, cst = 0, topll = 0;
	int colon, comma, last, errorStart, needName;
	int storeType = 0, scopeType = 0;
	int reset_decos = 0;
	int stmtStart;
	unsigned char tok, tkt, tki, tki2;

	DaoInode *back = self->vmcLast;
	DaoInode *inode, *opening, *closing;
	DaoType *retype = self->returnType; /* Updated for code section; */
	DString *mbs = self->string;
	DaoValue *value;

	self->permission = DAO_PERM_PUBLIC;

	if( from ==0 && (to+1) == self->tokens->size ){
		for(i=0; i<self->tokens->size; i++) tokens[i]->index = i;
	}

#if 0
	printf("routine = %p; %i, %i\n", routine, start, to );
	for(i=start; i<=to; i++) printf("%s  ", tokens[i]->string.chars); printf("\n\n");
#endif

	self->curToken = from;
	tok = DaoParser_GetEnumTokenType( retype );
	if( to >= from && tokens[start]->type == tok ){
		/* routine test() => list<tuple<a:int,b:int>> { {(1,2), (3,4)} } */
		int rb = DaoParser_FindPairToken( self, tok, tok + 1, start, to );
		DList_PushFront( self->enumTypes, retype );
		enode = DaoParser_ParseEnumeration( self, 0, tok, start+1, rb-1 );
		DList_PopFront( self->enumTypes );
		self->curToken += 1;
	}else{
		/* Ensure single assignment statement will not pass this parsing: */
		enode = DaoParser_ParseExpression2( self, 0, 0, ASSIGNMENT_ERROR );
	}
	if( enode.reg >= 0 ){
		i = self->curToken;
		while( i <= to && tokens[i]->type == DTOK_SEMCO ) i += 1;
		if( i > to ){
			if( !(routine->attribs & (DAO_ROUT_INITOR|DAO_ROUT_DEFER)) ){
				DaoParser_PushTokenIndices( self, from, from, to );
				DaoParser_AddCode( self, DVM_RETURN, enode.reg, 1, 0 );
			}
			goto ReturnTrue;
		}
	}
	DaoParser_Restore( self, back, regcount );

	startTriples = self->tokenTriples->size;
	self->vmcValue = NULL;
	while( start >= from && start <= to ){

		DaoParser_PopTokenIndices( self, (self->tokenTriples->size - startTriples) / 3 );
		DaoParser_PushTokenIndices( self, start, start, start );

		self->vmcValue = NULL;
		self->usedString = 0;
		self->usedList = 0;
		self->curLine = tokens[start]->line;
		ptok = tokens[start];
		tki = tokens[start]->name;
		topll = (self->levelBase + self->lexLevel) ==0;
#if 0
		printf("At tokPos : %i, %i, %p\n", start, ptok->line, ptok->string );
		printf("At tokPos : %i, %i, %s\n", tki, ptok->line, ptok->string.chars );
#endif
		if( self->warnings->size ) DaoParser_PrintWarnings( self );
		if( self->errors->size ) goto ReturnFalse;
		if( reset_decos && self->decoFuncs2->size ){
			DList_Clear( self->decoFuncs2 );
			DList_Clear( self->decoParams2 );
		}
		if( self->enumTypes->size ) DList_Clear( self->enumTypes );
		stmtStart = errorStart = start;
		if( ! self->isClassBody ){
			self->permission = DAO_PERM_PUBLIC;
			if( self->nsDefined ) self->permission = DAO_PERM_PRIVATE;
		}
		if( tki >= DKEY_PRIVATE && tki <= DKEY_PUBLIC ){
			if( ! self->isClassBody ){
				DaoParser_Error3( self, DAO_STATEMENT_OUT_OF_CONTEXT, start );
				goto ReturnFalse;
			}
			self->permission = tki - DKEY_PRIVATE + DAO_PERM_PRIVATE;
			start += 1;
			if( start <= to && tokens[start]->name == DTOK_COLON ) start += 1;
			if( start > to ) break;
			tki = tokens[start]->name;
			continue;
		}

		needName = 0;
		storeType = 0;
		scopeType = 0;
		if( tki >= DKEY_CONST && tki <= DKEY_VAR ){
			needName = 1;
			switch( tki ){
			case DKEY_CONST  : storeType = DAO_DECL_CONST; break;
			case DKEY_INVAR  : storeType = DAO_DECL_INVAR; break;
			case DKEY_STATIC : storeType = DAO_DECL_STATIC; break;
			case DKEY_VAR : storeType = DAO_DECL_VAR; break;
			default : break;
			}
			start ++;
			ptok = tokens[start];
			tki = ptok->name;
		}
		if( self->levelBase + self->lexLevel == 0 ){
			scopeType = storeType ? DAO_DECL_GLOBAL : DAO_DECL_LOCAL;
			if( ns->options & DAO_NS_AUTO_GLOBAL ) scopeType = DAO_DECL_GLOBAL;
		}else if( self->isClassBody ){
			scopeType = DAO_DECL_MEMBER;
			if( (storeType & DAO_DECL_VAR) && (hostClass->attribs & DAO_CLS_INVAR) ){
				DaoParser_Error( self, DAO_INVALID_STORAGE, & tokens[start-1]->string );
				goto ReturnFalse;
			}
		}else{
			scopeType = DAO_DECL_LOCAL;
		}

		rb = -1;
		tki = tokens[start]->type;
		if( tki == DTOK_LB ) rb = DaoParser_FindPairToken( self, DTOK_LB, DTOK_RB, start, -1 );
		if( rb >0 && (rb+1) <= to && tokens[rb+1]->type == DTOK_ASSN ){
			start = DaoParser_MultipleAssignment( self, start, rb, to, storeType );
			if( start < 0 ) goto ReturnFalse;
			if( DaoParser_CompleteScope( self, start ) == 0 ) goto ReturnFalse;
			continue;
		}

		tki = tokens[start]->name;
		tkt = tokens[start]->type;
		tki2 = start+1 <= to ? tokens[start+1]->name : 0;

		if( tki == DTOK_ID_THTYPE ){
#ifdef DAO_WITH_DECORATOR
			DaoInode *back = self->vmcLast;
			DaoRoutine *decfunc = NULL;
			DaoList *declist = NULL;
			DList *cid = DaoParser_GetArray( self );
			DString *name = & tokens[start]->string;
			reset_decos = 0;
			reg = DaoParser_GetRegister( self, tokens[start] );
			if( reg < 0 ) goto DecoratorError;
			if( LOOKUP_ISCST( reg ) == 0 ) goto DecoratorError;
			value = DaoParser_GetVariable( self, reg );
			if( value == NULL || value->type != DAO_ROUTINE ) goto DecoratorError;
			decfunc = & value->xRoutine;
			declist = DaoList_New();
			if( start+1 <= to && tokens[start+1]->name == DTOK_LB ){
				self->curToken = start + 2;
				enode = DaoParser_ParseExpressionList( self, DTOK_COMMA, NULL, cid );
				if( enode.reg < 0 ) goto DecoratorError;
				if( DaoParser_CheckTokenType( self, DTOK_RB, ")" ) == 0 ) goto DecoratorError;
				rb = self->curToken;
				if( enode.konst ==0 ){
					DaoParser_Error2( self, DAO_EXPR_NEED_CONST_EXPR, start+2, rb-1, 0 );
					goto DecoratorError;
				}
				if( cid->size >= DAO_MAX_PARAM ){
					DaoParser_Error2( self, DAO_PARAM_TOO_MANY, start+2, rb-1, 0 );
					goto DecoratorError;
				}
				for(k=0; k<cid->size; k++ ){
					DaoValue *v = DaoParser_GetVariable( self, cid->items.pInt[k] );
					DaoList_Append( declist, v );
				}
				start = rb;
			}
			if( self->byteBlock ){
				DaoByteBlock *eval, *block = self->byteBlock;
				DaoValue *value =(DaoValue*) decfunc;
				int opcode = 0;
				switch( LOOKUP_ST( reg ) ){
				case DAO_CLASS_CONSTANT  : opcode = DVM_GETCK; break;
				case DAO_GLOBAL_CONSTANT : opcode = DVM_GETCG; break;
				}
				if( opcode != 0 && DaoByteBlock_FindObjectBlock( block, value ) == NULL ){
					DaoByteBlock *namebk = DaoByteBlock_EncodeString( block, name );
					eval = DaoByteBlock_AddEvalBlock( block, value, opcode, 1, 0, NULL );
					DaoByteBlock_InsertBlockIndex( eval, eval->end, namebk );
				}
				GC_Assign( & declist->ctype, dao_type_list_any );
				DaoByteBlock_EncodeValue( self->byteBlock, (DaoValue*) declist );
			}
			DList_PushFront( self->decoFuncs2, decfunc );
			DList_PushFront( self->decoParams2, declist );
			DaoParser_PopCodes( self, back );
			start ++;
			continue;
DecoratorError:
			if( declist ) DaoList_Delete( declist );
			DaoParser_Error3( self, DAO_CTW_INVA_SYNTAX, start );
			goto ReturnFalse;
#else
			DaoParser_Error( self, DAO_DISABLED_DECORATOR, NULL );
			goto ReturnFalse;
#endif
		}

		reset_decos = 1;

		/*
		// Parsing routine definition:
		// Note: routine(){...}();
		// It is allowed to define and call immediately an anonymous function.
		*/
		if( tki == DKEY_ROUTINE && (tki2 != DTOK_LB || self->isClassBody || self->isInterBody
			|| self->isCinTypeBody) ){
			int attribs = 0;
			switch( storeType ){
			case DAO_DECL_VAR:
			case DAO_DECL_CONST :
				DaoParser_Error3( self, DAO_INVALID_FUNCTION_DEFINITION, errorStart );
				goto ReturnFalse;
			case DAO_DECL_STATIC : attribs = DAO_ROUT_STATIC; break;
			case DAO_DECL_INVAR  : attribs = DAO_ROUT_INVAR;  break;
			}
			start = DaoParser_ParseRoutineDefinition( self, start, from, to, attribs );
			if( start <0 ) goto ReturnFalse;
			if( cons && topll ) DaoParser_MakeCodes( self, errorStart, start, ns->inputs );
			continue;
		}else if( tki == DKEY_INTERFACE && tki2 == DKEY_ROUTINE ){
			int attribs = DAO_ROUT_INTERFACE;
			if( storeType == DAO_DECL_INVAR ){
				attribs |= DAO_ROUT_INVAR;
			}else if( storeType ){
				DaoParser_Error3( self, DAO_INVALID_FUNCTION_DEFINITION, errorStart );
				goto ReturnFalse;
			}
			start = DaoParser_ParseRoutineDefinition( self, start+1, from, to, attribs );
			if( start <0 ) goto ReturnFalse;
			if( cons && topll ) DaoParser_MakeCodes( self, errorStart, start, ns->inputs );
			continue;
		}else if( tki == DKEY_CLASS ){
			if( storeType && storeType != DAO_DECL_INVAR ){
				DaoParser_Error( self, DAO_INVALID_STORAGE, & tokens[start]->string );
				goto ReturnFalse;
			}
			/* parsing class definition */
			start = DaoParser_ParseClassDefinition( self, start, to, storeType );
			if( start <0 ) goto ReturnFalse;
			if( cons && topll ) DaoParser_MakeCodes( self, errorStart, start, ns->inputs );
			continue;
		}else if( tki == DKEY_ENUM && (tki2 == DTOK_LCB || tki2 == DTOK_IDENTIFIER) ){
			start = DaoParser_ParseEnumDefinition( self, start, to, storeType );
			if( start <0 ) goto ReturnFalse;
			if( cons && topll ) DaoParser_MakeCodes( self, errorStart, start, ns->inputs );
			continue;
		}

		if( needName && (tkt != DTOK_IDENTIFIER || (tki > DAO_NOKEY1 && tki < DKEY_CEIL)) ){
			DaoParser_Error( self, DAO_TOKEN_NEED_NAME, & tokens[start]->string );
			DaoParser_Error3( self, DAO_INVALID_STATEMENT, errorStart );
			goto ReturnFalse;
		}

		if( tki == DTOK_SEMCO ){
			if( DaoParser_CompleteScope( self, start ) == 0 ) goto ReturnFalse;
			start ++;
			continue;
		}else if( tki == DKEY_LOAD && tki2 != DTOK_LB ){
			start = DaoParser_ParseLoadStatement( self, start, to );
			if( start < 0 ) goto ReturnFalse;
			if( cons ) DaoParser_MakeCodes( self, start, to, ns->inputs );
			continue;
		}else if( tki == DKEY_IMPORT ){
			start = DaoParser_ParseImportStatement( self, start, to, 0 );
			if( start <0 ) goto ReturnFalse;
			if( cons && topll ) DaoParser_MakeCodes( self, errorStart, start, ns->inputs );
			continue;
		}else if( tki == DKEY_NAMESPACE ){
			start = DaoParser_ParseNamespaceStatement( self, start, to );
			if( start <0 ) goto ReturnFalse;
			if( cons && topll ) DaoParser_MakeCodes( self, errorStart, start, ns->inputs );
			continue;
		}else if( tki == DKEY_TYPE ){
			start = DaoParser_ParseTypeAliasing( self, start, to );
			if( start <0 ) goto ReturnFalse;
			if( cons && topll ) DaoParser_MakeCodes( self, errorStart, start, ns->inputs );
			continue;
		}else if( tki == DKEY_INTERFACE ){
			start = DaoParser_ParseInterfaceDefinition( self, start, to, storeType );
			if( start <0 ) goto ReturnFalse;
			if( cons && topll ) DaoParser_MakeCodes( self, errorStart, start, ns->inputs );
			continue;
		}

		self->curToken = start;
		tki = tokens[start]->name;
		switch( tki ){
		case DTOK_LCB :
			DaoParser_AddScope2( self, start );
			start++;
			continue;
		case DTOK_RCB :
			if( DaoParser_CompleteScope( self, start ) == 0 ) goto ReturnFalse;
			if( DaoParser_DelScope( self, NULL ) == 0 ) goto ReturnFalse;
			if( DaoParser_CompleteScope( self, start ) == 0 ) goto ReturnFalse;
			start++;
			continue;
		case DKEY_WHILE :
			opening = (DaoInode*) DList_Back( self->scopeOpenings );
			closing = (DaoInode*) DList_Back( self->scopeClosings );
			if( closing && closing->c == DVM_DO ){
				if( DaoParser_CompleteScope( self, start-1 ) == 0 ) goto ReturnFalse;
				inode = self->vmcLast;
				if( (rb = DaoParser_ParseCondition( self, start+1, 0, NULL )) <0 ) goto ReturnFalse;
				opening->jumpTrue = inode->next; /* first instruction in the condition */
				self->vmcLast->jumpFalse = closing; /* jump for failed testing */
				DaoParser_PushTokenIndices( self, start, start, start );
				inode = DaoParser_AddCode( self, DVM_GOTO, 0, 0, 0 );
				inode->jumpTrue = opening; /* looping back */
				if( DaoParser_DelScope( self, NULL ) == 0 ) goto ReturnFalse;
				start = rb+1;
			}else{
				/* see comments in case DKEY_IF: */
				DaoParser_PushTokenIndices( self, start, start, start );
				closing = DaoParser_AddCode( self, DVM_LABEL, 0, 1, 0 );
				opening = DaoParser_AddScope( self, DVM_LOOP, closing );
				if( (rb = DaoParser_ParseCondition( self, start+1, 1, opening )) <0 ) goto ReturnFalse;
				opening->jumpTrue = self->vmcLast;
				start = 1 + rb + DaoParser_AddScope2( self, rb+1 );
			}
			continue;
		case DKEY_IF :
			/*
			// Add an auxiliary scope to simplify the handling of branchings.
			// Such scoping is marked by an opening inode and a closing inode.
			// The closing inode will be moved to the place where the scope is closed.
			//
			// opening->jumpTrue shall point to the start of the condition expression.
			// opening->jumpFalse = closing.
			//
			// closing->a holds the token name that will allow the scope to be extended.
			// closing->b indicates if scope is break-able (loop/switch): 0 (no), 1 (yes).
			//
			// In this case, the "else" keyword will always extend the scope of an "if" block.
			// Any other token will end the "if" block, and the closing node will be
			// moved to this place to serve a proper branching target!
			 */
			DaoParser_PushTokenIndices( self, start, start, start );
			closing = DaoParser_AddCode( self, DVM_LABEL, DKEY_ELSE, 0, 0 );
			opening = DaoParser_AddScope( self, DVM_BRANCH, closing );
			if( (rb = DaoParser_ParseCondition( self, start+1, 1, NULL )) <0 ) goto ReturnFalse;
			opening->jumpTrue = self->vmcLast;
			start = 1 + rb + DaoParser_AddScope2( self, rb+1 );
			continue;
		case DKEY_ELSE :
			opening = (DaoInode*) DList_Back( self->scopeOpenings );
			closing = (DaoInode*) DList_Back( self->scopeClosings );
			/* If not following "if" or "else if", abort with error: */
			if( closing == NULL || closing->a != DKEY_ELSE ){
				DaoParser_Error3( self, DAO_STATEMENT_OUT_OF_CONTEXT, start );
				goto ReturnFalse;
			}
			DaoParser_PushTokenIndices( self, start, start, start );
			inode = DaoParser_AddCode( self, DVM_GOTO, 0, 0, 0 );
			inode->jumpTrue = closing; /* jump out of the if block */
			inode = DaoParser_AddCode( self, DVM_NOP, 0, 0, 0 );
			opening->jumpTrue->jumpFalse = inode; /* previous condition test jump here */
			opening->jumpTrue = inode; /* reset */

			if( tokens[start+1]->name == DKEY_IF ){
				if( (rb = DaoParser_ParseCondition( self, start+2, 1, NULL )) <0 ) goto ReturnFalse;
				opening->jumpTrue = self->vmcLast; /* update the condition test */
				start = 1 + rb + DaoParser_AddScope2( self, rb+1 );
			}else{
				closing->a = 0; /* the if block is done */
				start += 1 + DaoParser_AddScope2( self, start+1 );
			}
			continue;
		case DKEY_FOR :
			start = DaoParser_ParseForLoop( self, start, to );
			if( start < 0 ){
				DaoParser_Error3( self, DAO_INVALID_FOR, errorStart );
				goto ReturnFalse;
			}
			continue;
		case DKEY_DO :
			DaoParser_PushTokenIndices( self, start, start, start );
			closing = DaoParser_AddCode( self, DVM_LABEL, DKEY_WHILE, 1, DVM_DO );
			opening = DaoParser_AddScope( self, DVM_LOOP, closing );
			opening->jumpTrue = DaoParser_AddCode( self, DVM_UNUSED, 0,0,0 );
			start += 1 + DaoParser_AddScope2( self, start+1 );
			continue;
		case DKEY_SWITCH :
			switchName = NULL;
			switchNameType = 0;
			switchType = 0;
			lb = DaoParser_FindOpenToken( self, DTOK_LB, start, -1, 1 );
			if( lb < 0 ) goto ReturnFalse;
			rb = DaoParser_FindPairToken( self, DTOK_LB, DTOK_RB, start, -1 );
			if( rb < 0 ) goto ReturnFalse;
			k = start + 2;
			if( tokens[k]->name == DKEY_VAR || tokens[k]->name == DKEY_INVAR ){
				/*  var|invar <name> =  */
				if( DaoParser_CheckNameToken( self, k, rb, 0, k ) == 0 ) goto ReturnFalse;
				switchNameType = tokens[k]->name;
				switchName = & tokens[k+1]->string;
				self->curToken = k + 2;
				if( DaoParser_CheckTokenType( self, DTOK_ASSN, "=" ) ==0 ) goto ReturnFalse;
				k += 3;
			}
			reg1 = DaoParser_MakeArithTree( self, k, rb-1, & cst );
			if( reg1 < 0 ) goto ReturnFalse;
			if( tokens[rb+1]->name == DKEY_TYPE ){ /* switch() type {} */
				if( switchName == NULL && (lb+2) == rb ){ /* Single name expression: */
					switchName = & tokens[lb+1]->string;
				}
				switchType = 1;
				rb += 1;
			}
			self->curToken = rb + 1;
			if( DaoParser_CheckTokenType( self, DTOK_LCB, "{" ) ==0 ) goto ReturnFalse;

			DaoParser_PushTokenIndices( self, rb, rb, rb );
			closing = DaoParser_AddCode( self, DVM_LABEL, 0, 1, DVM_SWITCH );
			opening = DaoParser_AddScope( self, DVM_BRANCH, closing );

			k = 0;
			switchTable = DMap_New( DAO_DATA_VALUE, 0 );
			if( switchType ){
				/*
				// Store two strings for the name:
				// 1. var <name> = <expr>   : ( <name>, "" )
				// 2. invar <name> = <expr> : ( "", <name> )
				// 3. <name> as expression  : ( <name>, <name> )
				 */
				k = self->switchNames->size + 1;
				DString_Reset( self->string, 0 );
				DList_Append( self->switchNames, self->string );
				DList_Append( self->switchNames, self->string );
				if( switchName != NULL ){
					if( switchNameType == DKEY_VAR ){
						DString_Assign( self->switchNames->items.pString[k-1], switchName );
					}else if( switchNameType == DKEY_INVAR ){
						DString_Assign( self->switchNames->items.pString[k], switchName );
					}else{
						DString_Assign( self->switchNames->items.pString[k-1], switchName );
						DString_Assign( self->switchNames->items.pString[k], switchName );
					}
				}
			}
			if( switchType == 0 && switchNameType != 0 && switchName != NULL ){
				/* switch( var|invar <name> = <expr> ) { } */
				int movetype = 0, pos = start + 1 + (switchNameType != 0);
				int reg = DaoParser_PushRegister( self );
				switch( switchNameType ){
				case DKEY_VAR   : movetype = 1<<1; break;
				case DKEY_INVAR : movetype = 3<<1; break;
				}
				DaoParser_PushLevel( self );
				MAP_Insert( DaoParser_CurrentSymbolTable( self ), switchName, reg );
				DaoParser_PushTokenIndices( self, pos, pos, pos );
				DaoParser_AddCode( self, DVM_MOVE, reg1, movetype|1, reg );
			}
			DaoParser_PushTokenIndices( self, start, start + 1, rb );
			DaoParser_AddCode( self, DVM_SWITCH, reg1, self->switchTables->size, k );
			opening->jumpTrue = self->vmcLast;
			opening->jumpTrue->jumpFalse = closing;
			DList_Append( self->switchTables, switchTable );
			DMap_Delete( switchTable );
			start = 1 + rb + DaoParser_AddScope2( self, rb+1 );
			DaoParser_PushLevel( self );
			continue;
		case DKEY_CASE :
		case DKEY_DEFAULT :
			opening = closing = NULL;
			k = self->scopeOpenings->size;
			DaoParser_PopLevel( self );
			if( k >= 2 && self->scopeOpenings->items.pInode[k-1]->code == DVM_LBRA ){
				opening = self->scopeOpenings->items.pInode[k-2];
				closing = self->scopeClosings->items.pInode[k-2];
			}
			if( closing == NULL || closing->c != DVM_SWITCH ){
				DaoParser_Error3( self, DAO_STATEMENT_OUT_OF_CONTEXT, start );
				goto ReturnFalse;
			}
			DaoParser_PushTokenIndices( self, start, start + 1, start + 1 );
			inode = DaoParser_AddCode( self, DVM_GOTO, 0, 0, 0 );
			inode->jumpTrue = closing;
			if( tki == DKEY_DEFAULT ){
				if( opening->jumpFalse && opening->jumpFalse->code == DVM_DEFAULT ){
					DaoParser_Error2( self, DAO_DEFAULT_DUPLICATED, start, to, 1 );
					goto ReturnFalse;
				}
				self->curToken = start + 1;
				if( DaoParser_CheckTokenType( self, DTOK_COLON, ":" ) ==0 ) goto ReturnFalse;
				DaoParser_AddCode( self, DVM_DEFAULT, 0, 0, 0 );
				DaoParser_PushLevel( self );
				opening->jumpTrue->jumpFalse = self->vmcLast;
				start += 2;
				continue;
			}
			switchTable = self->switchTables->items.pMap[ opening->jumpTrue->b ];
			switchType = 0;
			switchNameType = 0;
			switchName = NULL;
			if( opening->jumpTrue->c ){
				int len1 = self->switchNames->items.pString[opening->jumpTrue->c-1]->size;
				int len2 = self->switchNames->items.pString[opening->jumpTrue->c]->size;
				switchType = 1;
				if( len1 && len2 ){  /* <name> as expression */
					switchName = self->switchNames->items.pString[opening->jumpTrue->c];
					switchNameType = 0;
				}else if( len1 ){ /* var <name> = <expr> */
					switchName = self->switchNames->items.pString[opening->jumpTrue->c-1];
					switchNameType = DKEY_VAR;
				}else if( len2 ){ /* invar <name> = <expr> */
					switchName = self->switchNames->items.pString[opening->jumpTrue->c];
					switchNameType = DKEY_INVAR;
				}
			}
			colon = DaoParser_FindOpenToken( self, DTOK_COLON, start, -1, 1 );
			comma = DaoParser_FindOpenToken( self, DTOK_COMMA, start, colon, 0 );
			last = start + 1;
			if( colon < 0 ){
				DaoParser_Error2( self, DAO_CASE_NOT_VALID, start, to, 1 );
				goto ReturnFalse;
			}
			if( comma < 0 ) comma = colon;

			casetype = NULL;
			caseConst = 0;
			if( switchType ){  /* switch( ... ) type { case <type> : } */
				DNode *it;
				int konst;
				casetype = DaoParser_ParseType( self, last, colon-1, & k, NULL );
				if( casetype == NULL ){
					DaoParser_Error2( self, DAO_CASE_NOT_TYPE, start, to, 1 );
					goto ReturnFalse;
				}
				if( k != colon ){
					DaoParser_Error2( self, DAO_CASE_NOT_VALID, start, to, 1 );
					goto ReturnFalse;
				}
				DaoParser_PushTokenIndices( self, last, last + 1, colon );
				caseConst = DaoRoutine_AddConstant( routine, (DaoValue*) casetype );
				DaoParser_AddCode( self, DVM_NOP, caseConst, 0, 0 );
				for(it=DMap_First(switchTable); it; it=DMap_Next(switchTable,it)){
					/* Check uniqueness of the case types: */
					DaoType *key = it->key.pType;
					int bl = DaoType_MatchTo( casetype, key, NULL ) >= DAO_MT_EXACT;
					bl |= DaoType_MatchTo( key, casetype, NULL ) >= DAO_MT_EXACT;
					if( bl ){
						DaoParser_Error2( self, DAO_CASE_DUPLICATED, start, colon, 1 );
						goto ReturnFalse;
					}
				}
				DMap_Insert( switchTable, casetype, self->vmcLast );
				last = colon;
			}
			while( last < colon ){
				DNode *it;
				DaoEnode item = {-1,0,0,0,NULL,NULL,NULL,NULL};
				int oldcount = self->regCount;
				back = self->vmcLast;
				self->curToken = last;
				item = DaoParser_ParseExpression( self, DTOK_COLON );
				if( item.reg < 0 ){
					DaoParser_Error2( self, DAO_CASE_NOT_VALID, start, colon, 1 );
					goto ReturnFalse;
				}else if( item.konst ==0 ){
					DaoParser_Error2( self, DAO_CASE_NOT_CONSTANT, last, comma-1, 0 );
					DaoParser_Error2( self, DAO_CASE_NOT_VALID, start, colon, 1 );
					goto ReturnFalse;
				}else if( LOOKUP_ST( item.konst ) != DAO_LOCAL_CONSTANT ){
					value = DaoParser_GetVariable( self, item.konst );
					item.konst = DaoRoutine_AddConstant( routine, value );
				}else if( LOOKUP_UP( item.konst ) != 0 ){
					value = DaoParser_GetVariable( self, item.konst );
					item.konst = DaoRoutine_AddConstant( routine, value );
				}else{
					item.konst = LOOKUP_ID( item.konst );
				}
				/*
				// remove GETC so that CASETAG will be together,
				// which is neccessary to properly setup switch table:
				 */
				DaoParser_PopCodes( self, back );
				DaoParser_PopRegisters( self, self->regCount - oldcount );
				DaoParser_PushTokenIndices( self, last, last + 1, colon );
				DaoParser_AddCode( self, DVM_NOP, item.konst, 0, 0 );
				value = DaoParser_GetVariable( self, LOOKUP_BIND_LC( item.konst ) );
				for(it=DMap_First(switchTable); it; it=DMap_Next(switchTable,it)){
					DaoValue *key = it->key.pValue;
					int bl = DaoValue_Compare( value, key ) == 0;
					bl |= DaoValue_Compare( key, value ) == 0;
					if( bl ){
						DaoParser_Error2( self, DAO_CASE_DUPLICATED, start, colon, 1 );
						goto ReturnFalse;
					}
				}
				DMap_Insert( switchTable, value, self->vmcLast );
				if( self->curToken == colon ) break;
				if( tokens[self->curToken]->name != DTOK_COMMA ){
					DaoParser_Error2( self, DAO_CASE_NOT_VALID, start, colon, 1 );
					goto ReturnFalse;
				}else if( DaoParser_NextTokenName( self ) == DKEY_CASE ){
					self->curToken += 1;
				}
				last = self->curToken + 1;
			}
			DaoParser_AddCode( self, DVM_UNUSED, 0, 0, 0 );
			DaoParser_PushLevel( self );
			if( switchName != NULL ){ /* Create a new variable for the case block: */
				int pos = start + 1 + (switchNameType != 0);
				int reg = DaoParser_PushRegister( self );
				int movetype = 0;
				switch( switchNameType ){
				case DKEY_VAR   : movetype = 1<<1; break;
				case DKEY_INVAR : movetype = 3<<1; break;
				}
				MAP_Insert( DaoParser_CurrentSymbolTable( self ), switchName, reg );
				DaoParser_PushTokenIndices( self, pos, pos, pos );
				if( casetype ){
					int reg2 = DaoParser_PushRegister( self );
					DaoParser_AddCode( self, DVM_CAST, reg1, caseConst, reg2 );
					DaoParser_AddCode( self, DVM_MOVE, reg2, movetype|1, reg );
				}else{
					DaoParser_AddCode( self, DVM_MOVE, reg1, movetype|1, reg );
				}
			}
			start = colon + 1;
			continue;
		case DKEY_BREAK :
		case DKEY_SKIP :
			inode = DaoParser_AddCode( self, DVM_GOTO, 0, 0, tki );
			opening = DaoParser_GetBreakableScope( self );
			if( opening == NULL ){
				DaoParser_Error3( self, DAO_STATEMENT_OUT_OF_CONTEXT, start );
				goto ReturnFalse;
			}
			if( opening->next->code == DVM_SWITCH ){
				DaoParser_Error3( self, DAO_STATEMENT_OUT_OF_CONTEXT, start );
				goto ReturnFalse;
			}
			inode->jumpTrue = tki == DKEY_BREAK ? opening->jumpFalse : opening->next;
			if( inode->jumpTrue->code == DVM_SWITCH ) inode->jumpTrue = inode->jumpTrue->jumpFalse;
			if( DaoParser_CompleteScope( self, start ) == 0 ) goto ReturnFalse;
			start += 1;
			continue;
		case DKEY_DEFER :
			reg = DaoParser_ParseClosure( self, start );
			if( reg < 0 ) goto ReturnFalse;
			start = self->curToken;
			if( DaoParser_CompleteScope( self, start ) == 0 ) goto ReturnFalse;
			continue;
		case DKEY_RETURN :
			start += 1;
			reg = N = end = 0;
			if( start <= to && tokens[start]->line == tokens[start-1]->line ){
				DaoType *retype = self->returnType; /* Updated for code section; */
				int tok = DaoParser_GetEnumTokenType( retype );
				int ecount = self->errors->size;
				self->curToken = start;
				if( tokens[start]->type == tok ){
					/* routine test() => list<tuple<a:int,b:int>> { return {(1,2), (3,4)} } */
					int rb = DaoParser_FindPairToken( self, tok, tok + 1, start, to );
					DList_PushFront( self->enumTypes, retype );
					enode = DaoParser_ParseEnumeration( self, 0, tok, start+1, rb-1 );
					DList_PopFront( self->enumTypes );
					self->curToken += 1;
				}else{
					enode = DaoParser_ParseExpressionList( self, DTOK_COMMA, NULL, NULL );
				}
				if( tokens[start]->type != DTOK_SEMCO && tokens[start]->type != DTOK_RCB ){
					if( self->errors->size > ecount ) goto ReturnFalse;
				}
				if( enode.reg >= 0 ){
					reg = enode.reg;
					N = enode.count;
					end = self->curToken;
					start = end;
				}
			}
			DaoParser_PushTokenIndices( self, stmtStart, stmtStart + 1, end );
			if( self->isSection && N > 1 ){
				int tup = DaoParser_PushRegister( self );
				DaoParser_PushTokenIndices( self, stmtStart + 1, stmtStart + 1, end );
				DaoParser_AddCode( self, DVM_TUPLE, reg, N, tup );
				DaoParser_PopTokenIndices( self, 1 );
				DaoParser_AddCode( self, DVM_RETURN, tup, 1, 0 );
			}else if( self->isSection ){
				DaoParser_AddCode( self, DVM_RETURN, reg, N, 0 );
			}else if( N && (routine->attribs & DAO_ROUT_INITOR) ){
				DaoParser_Error3( self, DAO_ROUT_INVALID_RETURN, errorStart );
				goto ReturnFalse;
			}else if( N && (routine->attribs & DAO_ROUT_DEFER) ){
				if( routine->attribs & DAO_ROUT_DEFER_RET ){
					DaoParser_AddCode( self, DVM_RETURN, reg, N, 0 );
				}else{
					DaoParser_Error3( self, DAO_ROUT_INVALID_RETURN, errorStart );
					goto ReturnFalse;
				}
			}else{
				DaoParser_AddCode( self, DVM_RETURN, reg, N, 0 );
			}
			if( DaoParser_CompleteScope( self, start-1 ) == 0 ) goto ReturnFalse;
			continue;
		}

		end = DaoParser_ParseVarExpressions( self, start, to, storeType | scopeType );
		if( end < 0 ) goto ReturnFalse;
		if( DaoParser_CompleteScope( self, end-1 ) == 0 ) goto ReturnFalse;
		DaoParser_CheckStatementSeparation( self, end-1, to );
		start = end;
	}
	if( DaoParser_CompleteScope( self, to ) == 0 ) goto ReturnFalse;
	DaoParser_PrintWarnings( self );
ReturnTrue:
	DaoParser_PopTokenIndices( self, (self->tokenTriples->size - triples)/3 );
	return 1;
ReturnFalse:
	DaoParser_PopTokenIndices( self, (self->tokenTriples->size - triples)/3 );
	return 0;
}

static int DaoParser_SetInitValue( DaoParser *self, DaoVariable *var, DaoValue *value, DaoType *type, int start, int end )
{
	DaoNamespace *ns = self->nameSpace;
	DaoToken **tokens = self->tokens->items.pToken;
	DaoType *tp1 = var->dtype;
	if( value != NULL && type == NULL ) type = DaoNamespace_GetType( ns, value );
	if( tp1 == NULL && type != NULL ){
		GC_IncRC( type );
		var->dtype = tp1 = type;
	}
	if( DaoValue_Move( value, & var->value, var->dtype ) == 0 ){
		DaoType *tp2 = DaoNamespace_GetType( ns, value );
		self->curLine = tokens[start]->line;
		DaoParser_Error( self, DAO_TYPE_PRESENTED, tp2->name );
		self->curLine = tokens[start]->line;
		DaoParser_Error( self, DAO_TYPE_EXPECTED, tp1->name );
		DaoParser_Error2( self, DAO_TYPE_NOT_MATCHING, start, end, 0 );
		return 0;
	}
	return 1;
}
int DaoParser_MultipleAssignment( DaoParser *self, int start, int rb, int to, int store )
{
	DaoEnode enode;
	DList *inodes = DList_New(0);
	DaoToken **tokens = self->tokens->items.pToken;
	int triples = self->tokenTriples->size;
	int movetype = 1|((store!=0)<<1)|(((store & DAO_DECL_INVAR) != 0)<<2);
	int foldConst = store & (DAO_DECL_CONST|DAO_DECL_STATIC);
	int i, k, reg, errorStart = start;

	self->curToken = start + 1;
	while( self->curToken < rb ){
		int tid = self->curToken;
		int cur = tokens[tid]->name;
		int nxt = tokens[tid+1]->name;
		cur = cur == DTOK_IDENTIFIER || cur >= DKEY_CEIL;
		nxt = nxt == DTOK_COMMA || nxt == DTOK_RB;
		DaoParser_PushTokenIndices( self, tid, tid, tid );
		if( cur && nxt ){
			k = DaoParser_GetRegister( self, tokens[tid] );
			if( k < 0 ){
				k = DaoParser_DeclareVariable( self, tokens[tid], store, NULL );
				if( k < 0 ) goto InvalidMultiAssignment;
			}
		}
		enode = DaoParser_ParseExpression( self, 0 );
		if( enode.reg < 0 ) goto InvalidMultiAssignment;
		if( enode.update == NULL ){
			DaoParser_AddCode( self, DVM_MOVE, 0, movetype, enode.reg );
			DList_Append( inodes, self->vmcLast );
		}else{
			int code = enode.update->code;
			if( code < DVM_GETVH || code > DVM_GETF ){
				DaoParser_Error3( self, DAO_INVALID_STATEMENT, errorStart );
				goto InvalidMultiAssignment;
			}
			enode.update->code += DVM_SETVH - DVM_GETVH;
			enode.update->c = enode.update->a;
			DaoParser_PopRegister( self );
			DList_Append( inodes, enode.update );
		}
		if( DaoParser_CurrentTokenType( self ) == DTOK_COMMA ) self->curToken += 1;
	}
	self->curToken = rb + 2;
	if( foldConst ) self->needConst += 1;
	enode = DaoParser_ParseExpression( self, 0 );
	if( foldConst ) self->needConst -= 1;
	if( enode.reg < 0 ) goto InvalidMultiAssignment;
	if( foldConst && enode.konst == 0 ){
		DaoParser_Error2( self, DAO_EXPR_NEED_CONST_EXPR, start, to, 0 );
		goto InvalidMultiAssignment;
	}
	i = DaoParser_PushRegister( self );
	for(k=0; k<inodes->size; k++){
		int p1 = inodes->items.pInode[k]->first;
		int p2 = p1 + inodes->items.pInode[k]->last;
		reg = DaoParser_PushRegister( self );
		DaoParser_PushTokenIndices( self, start, rb, self->curToken-1 );
		DaoParser_AddCode( self, DVM_GETDI, enode.reg, k, reg );
		DaoParser_AppendCode( self, inodes->items.pInode[k] );
		self->vmcLast->a = reg;
	}
	DaoParser_PopTokenIndices( self, (self->tokenTriples->size - triples)/3 );
	DList_Delete( inodes );
	return self->curToken;
InvalidMultiAssignment:
	DaoParser_PopTokenIndices( self, (self->tokenTriples->size - triples)/3 );
	DList_Delete( inodes );
	return -1;
}
int DaoParser_ParseVarExpressions( DaoParser *self, int start, int to, int store )
{
	DaoValue *value;
	DaoEnode enode;
	DaoType *abtp, *extype;
	DaoVmSpace *vms = self->vmSpace;
	DaoNamespace *ns = self->nameSpace;
	DaoRoutine *routine = self->routine;
	DaoClass *hostClass = self->hostClass;
	DaoToken *ptok, *lastok, **tokens = self->tokens->items.pToken;
	DaoInode *back = self->vmcLast;
	int triples = self->tokenTriples->size;
	int cons = (vms->options & DAO_OPTION_INTERUN) && (ns->options & DAO_NS_AUTO_GLOBAL);
	int topll = (self->levelBase + self->lexLevel) ==0;
	int nameStart, oldcount = self->regCount;
	int explicit_store = (store>>3)<<3;
	int expression = explicit_store == 0;
	int reg, cst, temp, eq, errorStart = start;
	int k, m, rb = to, end = start, remove = 1;
	int tki, tki2, errors;

#if 0
	int mm;
	for(mm=start; mm<=to; mm++) printf( "%s ", tokens[mm]->string.chars );
	printf("\n");
#endif

	/*
	// Storage prefixes have been parsed, if there were any.
	// Check variable declaration patterns:
	// [ SPECIFIERS ] IDENTIFIER { , IDENTIFIER } [ : TYPE ] [ = VALUE ]
	 */
	k = start;
	ptok = tokens[k];
	nameStart = self->toks->size;
	if( ptok->type == DTOK_LB ){
		rb = DaoParser_FindPairToken( self, DTOK_LB, DTOK_RB, start, to );
		if( rb > 0 && (rb+1) <= to && tokens[rb+1]->type == DTOK_ASSN ){
			return DaoParser_MultipleAssignment( self, start, rb, to, store );
		}
	}
	while( ptok->name == DTOK_IDENTIFIER || ptok->name >= DKEY_CEIL ){
		DList_Append( self->toks, ptok );
		if( (++k) > to ) break;
		lastok = ptok;
		ptok = tokens[k];
		if( ptok->name == DTOK_SEMCO || ptok->line != lastok->line ){
			expression &= 1;
			break;
		}
		if( ptok->name == DTOK_COLON || ptok->name == DTOK_ASSN ){
			expression = 0;
			break;
		}
		if( ptok->name != DTOK_COMMA ){
			expression = 1;
			break;
		}
		if( (++k) > to ) break; /* skip comma */
		ptok = tokens[k];
	}
	lastok = ptok;
	if( explicit_store == 0 && ptok->name == DTOK_ASSN && self->toks->size == nameStart+1 ){
		DaoToken *varTok = self->toks->items.pToken[nameStart];
		expression = DaoParser_GetRegister( self, varTok ) >= 0;
	}
	if( expression ){
		DList_Erase( self->toks, nameStart, self->toks->size - nameStart );
		if( explicit_store ){
			DaoParser_Error3( self, DAO_INVALID_STATEMENT, errorStart );
			goto ReturnError;
		}
		self->curToken = start;
		enode = DaoParser_ParseExpression2( self, 0, 0, ASSIGNMENT_OK );
		if( enode.reg < 0 ) goto ReturnError;
		while( DaoParser_CurrentTokenType( self ) == DTOK_COMMA ){
			self->curToken += 1;
			enode = DaoParser_ParseExpression2( self, 0, 0, ASSIGNMENT_OK );
			if( enode.reg < 0 ) goto ReturnError;
		}
		return self->curToken;
	}
	self->curToken = k;
	eq = lastok->type == DTOK_ASSN ? k : -1;

	errors = self->errors->size;
	if( explicit_store != 0 || lastok->type == DTOK_COLON ){
		for(k=nameStart; k<self->toks->size; k++){
			DString *name = & self->toks->items.pToken[k]->string;
			DNode *node = MAP_Find( DaoParser_CurrentSymbolTable( self ), name );
			if( node ) DaoParser_Error( self, DAO_SYMBOL_WAS_DEFINED, name );
		}
		if( self->errors->size > errors ) goto ReturnError;
	}
	abtp = extype = NULL;
	if( DaoParser_CurrentTokenType( self ) == DTOK_COLON ){
		int pos = self->curToken + 1;
		if( pos <= to ) extype = DaoParser_ParseType( self, pos, to, & k, NULL );
		if( extype == NULL ){
			DaoParser_Error3( self, DAO_INVALID_TYPE_FORM, pos );
			DaoParser_Error3( self, DAO_INVALID_STATEMENT, errorStart );
			goto ReturnError;
		}
		self->curToken = k;
		if( ((store&DAO_DECL_VAR) && extype->invar) || ((store&DAO_DECL_INVAR) && extype->var) ){
			DaoParser_Error3( self, DAO_INVALID_DECLARATION, errorStart - 1 );
			goto ReturnError;
		}
	}
	abtp = extype;
	oldcount = self->regCount;
	back = self->vmcLast;
	enode.reg = -1;
	enode.konst = 0;
	if( DaoParser_CurrentTokenType( self ) == DTOK_ASSN ){
		int foldConst = self->isClassBody && (store & DAO_DECL_MEMBER);
		foldConst |= store & (DAO_DECL_CONST|DAO_DECL_STATIC);
		if( self->curToken + 1 > to ){
			DaoParser_Error3( self, DAO_INVALID_STATEMENT, errorStart );
			goto ReturnError;
		}
		if( abtp && abtp->tid >= DAO_ARRAY && abtp->tid <= DAO_TUPLE )
			DList_PushFront( self->enumTypes, abtp );
		self->curToken += 1;
		if( foldConst ) self->needConst += 1;
		enode = DaoParser_ParseExpression( self, 0 );
		if( foldConst ) self->needConst -= 1;
		start = end = self->curToken;
		if( enode.reg < 0 ){
			DaoParser_Error2( self, DAO_INVALID_STATEMENT, start, end, 0 );
			goto ReturnError;
		}
		if( foldConst && enode.konst == 0 ){
			DaoParser_Error2( self, DAO_EXPR_NEED_CONST_EXPR, start, end, 0 );
			goto ReturnError;
		}
	}else if( self->isClassBody == 0 ){
		DaoParser_Error2( self, DAO_VARIABLE_WITHOUT_INIT, start, end, 0 );
		goto ReturnError;
	}

	reg = enode.reg;
	cst = enode.konst;
	start = end = self->curToken;
	if( cst == 0 && (store & DAO_DECL_CONST) ){
		DaoParser_Error2( self, DAO_EXPR_NEED_CONST_EXPR, start + 1, end, 0 );
		goto ReturnError;
	}
	value = NULL;
	if( enode.konst ){
		value = DaoParser_GetVariable( self, enode.konst );
		if( abtp && DaoType_MatchValue( abtp, value, NULL ) == 0 ){
			DaoParser_Error3( self, DAO_TYPE_NOT_MATCHING, errorStart );
			goto ReturnError;
		}
		/*
		// The following should just declare variable of any type:
		// [var|invar] name = none
		*/
		if( abtp == NULL && value && value->type == DAO_NONE ) abtp = dao_type_any;
	}
	if( abtp == NULL && value ){
		if( store != 0 && (store & DAO_DECL_INVAR) == 0 ){
			/*
			// Type of local variable should not be set here prematurely.
			// It should be the inferencer that infer its implicit type.
			*/
			abtp = DaoNamespace_GetType( ns, value );
		}else if( store >> 1 ){
			/*
			// No instruction will be generated for declaration of non-local variables
			// with constant intialization values, so there will be no type inference
			// for such declarations. So get the type here for the declaration.
			*/
			abtp = DaoNamespace_GetType( ns, value );
		}
		if( abtp && self->byteBlock ) DaoByteBlock_EncodeTypeOf( self->byteBlock, abtp, value );
	}
	if( store & DAO_DECL_VAR ){
		if( abtp == NULL ) abtp = DaoParser_MakeVarTypeHolder( self );
		abtp = DaoType_GetVarType( abtp );
	}else if( store & DAO_DECL_INVAR ){
		if( abtp == NULL ) abtp = DaoParser_MakeVarTypeHolder( self );
		abtp = DaoType_GetInvarType( abtp );
	}
	for(k=nameStart; k<self->toks->size; k++){
		DaoByteBlock *block = self->byteBlock;
		DaoToken *varTok = self->toks->items.pToken[k];
		DString *name = & varTok->string;
		int regC = DaoParser_DeclareVariable( self, varTok, store, abtp );
		int id = 0;
		/*
		printf( "declaring %s\n", varTok->string.chars );
		*/
		if( regC < 0 ) goto ReturnError;
		if( store & DAO_DECL_CONST ){
			DaoConstant *konst;
			if( store & DAO_DECL_GLOBAL ){
				id = DaoNamespace_FindConst( ns, & varTok->string );
				if( id < 0 ){
					DaoParser_Error( self, DAO_SYMBOL_NOT_DEFINED, & varTok->string );
					goto ReturnError;
				}
				konst = ns->constants->items.pConst[ LOOKUP_ID( id ) ];
				DaoValue_Move( value, & konst->value, abtp );
				if( block ){
					DaoByteBlock_DeclareConst( block, name, value, LOOKUP_PM(id) );
				}
			}else if( self->isClassBody && hostClass ){
				id = DaoClass_FindConst( hostClass, & varTok->string );
				if( cst ){
					konst = hostClass->constants->items.pConst[ LOOKUP_ID( id ) ];
					DaoValue_Move( value, & konst->value, abtp );
				}else if( eq >=0 ){
					DaoParser_Error2( self, DAO_EXPR_NEED_CONST_EXPR, eq+1, end, 0 );
					goto ReturnError;
				}
				if( block ){
					DaoByteBlock_DeclareConst( block, name, value, LOOKUP_PM(id) );
				}
			}else{
				id = LOOKUP_ID( DaoParser_GetRegister( self, varTok) );
				DaoValue_Move( value, routine->routConsts->value->items.pValue + id, abtp );
				DaoValue_MarkConst( routine->routConsts->value->items.pValue[id] );
			}
		}else{
			int st = LOOKUP_ST( regC );
			int up = LOOKUP_UP( regC );
			int id = LOOKUP_ID( regC );
			int pm = LOOKUP_PM( regC );
			int isdecl = self->isClassBody && (store & DAO_DECL_MEMBER);
			int explicit_decl = explicit_store != 0;
			int explicit_invar = (explicit_store & DAO_DECL_INVAR) != 0;
			int first = varTok->index;
			int mid = eq >= 0 ? eq : 0;
			remove = 0;
			DaoParser_PushTokenIndices( self, first, mid, end );
			switch( st ){
			case DAO_LOCAL_VARIABLE :
				if( reg < 0 ) continue;
				if( (up = DaoParser_GetOuterLevel( self, id )) > 0 ){
					DaoParser_AddCode( self, DVM_SETVH, reg, id, up );
				}else{
					int opb = 1|(explicit_decl<<1)|(explicit_invar<<2);
					DaoParser_AddCode( self, DVM_MOVE, reg, opb, id );
				}
				break;
			case DAO_OBJECT_VARIABLE :
				if( isdecl && cst ){
					DaoVariable *var = hostClass->instvars->items.pVar[id];
					if( DaoParser_SetInitValue( self, var, value, NULL, start, end ) == 0 ){
						goto ReturnError;
					}
					remove = 1;
					DaoValue_MarkConst( var->value );
					if( block ){
						DaoByteBlock_DeclareVar( block, name, var->value, var->dtype, pm );
					}
				}else if( ! self->isClassBody ){
					if( reg < 0 ) continue;
					DaoParser_AddCode( self, DVM_SETVO, reg, id, 0 );
				}else if( eq >=0 ){
					DaoParser_Error2( self, DAO_EXPR_NEED_CONST_EXPR, eq+1, end, 0 );
					goto ReturnError;
				}else if( block ){
					DaoByteBlock_DeclareVar( block, name, NULL, extype, pm );
				}
				break;
			case DAO_CLASS_VARIABLE :
				if( isdecl && cst ){
					DaoVariable *var = hostClass->variables->items.pVar[id];
					DaoVariable_Set( var, value, DaoNamespace_GetType( ns, value ) );
					remove = 1;
					if( block ){
						DaoByteBlock_Declare( block, DAO_ASM_STATIC, name, var->value, var->dtype, pm );
					}
				}else if( ! self->isClassBody ){
					if( reg < 0 ) continue;
					DaoParser_AddCode( self, DVM_SETVK, reg, id, 0 );
				}else if( eq < 0 ){
					DaoParser_Error2( self, DAO_VARIABLE_WITHOUT_INIT, start, end, 0 );
					goto ReturnError;
				}else{ /* eq >=0 */
					DaoParser_Error2( self, DAO_EXPR_NEED_CONST_EXPR, eq+1, end, 0 );
					goto ReturnError;
				}
				break;
			case DAO_GLOBAL_VARIABLE :
				if( reg < 0 ){
					if( explicit_store && block ){
						DaoByteBlock_DeclareGlobal( block, name, NULL, extype, pm );
					}
					continue;
				}
				if( cst && explicit_store && (store & DAO_DECL_GLOBAL) ){
					DaoVariable *var = ns->variables->items.pVar[id];
					if( DaoParser_SetInitValue( self, var, value, abtp, start, end ) == 0 ){
						goto ReturnError;
					}
					remove = 1;
					if( block ){
						DaoByteBlock_DeclareGlobal( block, name, var->value, var->dtype, pm );
					}
				}else{
					int mode = (explicit_decl<<1)|(explicit_invar<<2);
					DaoParser_AddCode( self, DVM_SETVG, reg, id, mode );
					self->usingGlobal = 1;
					if( explicit_store && block ){
						DaoVariable *var = ns->variables->items.pVar[id];
						DaoByteBlock_DeclareGlobal( block, name, NULL, var->dtype, pm );
					}
				}
				break;
			case DAO_STATIC_VARIABLE :
				if( cst && (explicit_store & DAO_DECL_STATIC) ){
					DaoVariable *var = ns->variables->items.pVar[id];
					if( DaoParser_SetInitValue( self, var, value, abtp, start, end ) == 0 ){
						goto ReturnError;
					}
					remove = 1;
					if( block ){
						DaoByteBlock_DeclareStatic( block, name, var->value, var->dtype, self->lexLevel, id );
					}
				}else if( reg >= 0 ){
					DaoParser_AddCode( self, DVM_SETVG, reg, id, (explicit_decl<<1) );
				}else if( explicit_store && block ){
					DaoByteBlock_DeclareStatic( block, name, NULL, extype, self->lexLevel, id );
				}
				break;
			default :
				DaoParser_Error( self, DAO_EXPR_MODIFY_CONSTANT, & varTok->string );
				goto ReturnError;
			}
		}
	}
	DList_Erase( self->toks, nameStart, self->toks->size - nameStart );
	if( cst && remove ){
		DaoParser_PopCodes( self, back );
		DaoParser_PopRegisters( self, self->regCount - oldcount );
	}
	self->curToken = end;
	if( DaoParser_CurrentTokenType( self ) == DTOK_COMMA && (end+1) <= to ){
		if( explicit_store == 0 && self->vmcLast != back ){
			DaoParser_Error3( self, DAO_INVALID_STATEMENT, errorStart );
			goto ReturnError;
		}
		DaoParser_PopTokenIndices( self, (self->tokenTriples->size - triples)/3 );
		return DaoParser_ParseVarExpressions( self, end+1, to, store );
	}
	DaoParser_PopTokenIndices( self, (self->tokenTriples->size - triples)/3 );
	return end;
ReturnError:
	DaoParser_PopTokenIndices( self, (self->tokenTriples->size - triples)/3 );
	return -1;
}
static int DaoParser_SetupBranching( DaoParser *self )
{
	DaoInode *it, *it2 = NULL;
	int id = 0, unused = 0;
	if( self->vmcLast->code != DVM_RETURN ){
		DaoVmSpace *vms = self->vmSpace;
		DaoNamespace *ns = self->nameSpace;
		int first = self->tokens->size-1;
		int opa = 0, autoret = self->autoReturn;

		if( first < 0 ) first = 0;
		DaoParser_PushTokenIndices( self, first, first, first );
		if( autoret == 0 ){
			int print = (vms->options & DAO_OPTION_INTERUN) && (ns->options & DAO_NS_AUTO_GLOBAL);
			int ismain = self->routine->attribs & DAO_ROUT_MAIN;
			autoret = ismain && (print || vms->evalCmdline);
		}
		if( autoret && self->vmcValue ) opa = self->vmcValue->c;
		autoret &= self->vmcValue != NULL;
		DaoParser_AddCode( self, DVM_RETURN, opa, autoret, 0 );
	}

	for(it=self->vmcFirst; it; it=it->next){
		switch( it->code ){
		case DVM_NOP : it->code = DVM_UNUSED; break;
		case DVM_GOTO : if( it->jumpTrue == it->next ) it->code = DVM_UNUSED; break;
		default : if( it->code >= DVM_NULL ) it->code = DVM_UNUSED; break;
		}
	}
	for(it=self->vmcFirst,id=0; it; it=it->next){
		it->index = id;
		id += it->code != DVM_UNUSED;
	}
	if( self->regCount > 0xefff || id > 0xefff ){
		/*
		// Though Dao VM instructions can hold operand id or jump id as big as 0xffff,
		// the type inference procedure may need to allocate additional registers, or
		// add additional instructions to handle code specialization or type casting.
		 */
		char buf[100];
		sprintf( buf, "registers (%i) or instructions (%i)!", self->regCount, id );
		DString_SetChars( self->string, "too big function with too many " );
		DString_AppendChars( self->string, buf );
		DaoParser_Error( self, DAO_CTW_INTERNAL, self->string );
		DaoParser_PrintError( self, 0, 0, NULL );
		return 0;
	}
	/* DaoParser_PrintCodes( self ); */
	DList_Clear( self->vmCodes );
	for(it=self->vmcFirst; it; it=it->next){
		/*
		   DaoInode_Print( it, it->index );
		 */
		switch( it->code ){
		case DVM_NOP : break;
		case DVM_TEST   : it->b = it->jumpFalse->index; break;
		case DVM_GOTO   : it->b = it->jumpTrue->index;  break;
		case DVM_SWITCH : it->b = it->jumpFalse->index; break;
		case DVM_CASE   : it->b = it->jumpTrue->index;  break;
		default : break;
		}
		if( it->code != DVM_UNUSED ) DList_Append( self->vmCodes, (DaoVmCodeX*) it );
	}
	return 1;
}
int DaoParser_ParseRoutine( DaoParser *self )
{
	DaoType *tt, *ft;
	DaoNamespace *NS = self->nameSpace;
	DaoRoutine *routine = self->routine;
	const int tokCount = self->tokens->size;
	int i, j, k, id, np, offset = 0, defLine = routine->defLine;

	GC_Assign( & routine->nameSpace, NS );
	self->returnType = (DaoType*) routine->routType->aux;

	if( (routine->attribs & DAO_ROUT_DECORATOR) && routine->routType->nested->size ){
		assert( routine->parCount == self->regCount );
		ft = routine->routType->nested->items.pType[0];
		if( ft->attrib & DAO_TYPE_SELFNAMED ){
			if( routine->routType->nested->size == 1 ) return 0;
			ft = routine->routType->nested->items.pType[1];
		}
		if( ft->tid == DAO_PAR_NAMED ) ft = (DaoType*) ft->aux;
		if( ft->tid != DAO_ROUTINE ) return 0;
		np = ft->nested->size;
		//if( np && ft->nested->items.pType[np-1]->tid == DAO_PAR_VALIST ) np -= 1;
		tt = DaoNamespace_MakeType( NS, "tuple", DAO_TUPLE, 0, ft->nested->items.pType, np );
		if( self->invarDecoArg ) tt = DaoType_GetInvarType( tt );
		if( DaoParser_DeclareVariable( self, self->decoArgName, 0, tt ) < 0 ) return 0;
	}
	if( self->argName ){
		int tokidx = self->argName->index;
		int opa = routine->routHost != NULL && !(routine->attribs & DAO_ROUT_STATIC);
		int mode = DVM_ENUM_MODE1 << 14;
		int np = routine->routType->nested->size;
		DaoType **partypes = routine->routType->nested->items.pType;
		tt = DaoNamespace_MakeType( NS, "tuple", DAO_TUPLE, 0, partypes+opa, np-opa );
		id = self->regCount;
		if( self->invarArg ) tt = DaoType_GetInvarType( tt );
		if( DaoParser_DeclareVariable( self, self->argName, 0, tt ) < 0 ) return 0;
		DaoParser_PushTokenIndices( self, tokidx, tokidx, tokidx );
		DaoParser_AddCode( self, DVM_TUPLE, opa, mode|(routine->parCount - opa), id );
	}

	if( DaoParser_ParseCodes( self, offset, tokCount-1 )==0 ){
		DaoParser_PrintError( self, 0, 0, NULL );
		return 0;
	}
	if( self->scopeOpenings->size ){
		DaoParser_Error3( self, DAO_INVALID_UNCLOSED_SCOPE, self->curToken );
		DaoParser_PrintError( self, 0, 0, NULL );
		return 0;
	}
	routine->defLine = defLine;

	if( self->errors->size ) return 0;
	if( self->nsDefines && self->nsDefines->size ){
		for(i=0; i<self->nsDefines->size; i+=3){
			DString *names[3] = {NULL, NULL, NULL};
			DaoNamespace *defns = self->nsDefines->items.pNS[i];
			int start = self->nsDefines->items.pInt[i+1];
			int end = self->nsDefines->items.pInt[i+2];
			for(j=start,k=0; j<end; ++j){
				DaoToken *token = self->nsSymbols->tokens->items.pToken[j];
				DString *symbol = & token->string;
				DNode *it1 = DMap_Find( NS->lookupTable, symbol );
				DNode *it2 = DMap_Find( defns->lookupTable, symbol );
				int st, id, lookup, count = 0;
				self->curLine = token->line;
				if( it1 == NULL || it2 != NULL ){
					if( it1 == NULL ) DaoParser_Error( self, DAO_SYMBOL_NOT_DEFINED, symbol );
					if( it2 != NULL ) DaoParser_Error( self, DAO_SYMBOL_WAS_DEFINED, symbol );
					return 0;
				}
				st = LOOKUP_ST( it1->value.pInt );
				id = LOOKUP_ID( it1->value.pInt );
				if( st == DAO_GLOBAL_CONSTANT ){
					count = defns->constants->size;
					DList_Append( defns->constants, NS->constants->items.pVoid[id] );
				}else{
					count = defns->variables->size;
					DList_Append( defns->variables, NS->variables->items.pVoid[id] );
				}
				lookup = LOOKUP_BIND( st, DAO_PERM_PUBLIC, 0, count );
				MAP_Insert( defns->lookupTable, symbol, lookup );
				if( self->byteBlock ){
					names[k++] = symbol;
					if( k == 3 || (j+1) == end ){
						DaoByteBlock_EncodeExport( self->byteBlock, defns, names );
						names[0] = names[1] = names[2] = NULL;
						k = 0;
					}
				}
			}
		}
	}
	if( DaoParser_PostParsing( self ) == 0 ) return 0;
#ifdef DAO_WITH_DECORATOR
	DaoParser_DecorateRoutine( self, routine );
	if( self->errors->size ){
		DaoParser_PrintError( self, 0, 0, NULL );
		return 0;
	}
#endif
	return 1;
}
static DaoEnode DaoParser_NoneValue( DaoParser *self )
{
	DaoEnode enode = {0,0,0,0,NULL,NULL,NULL,NULL};
	int cst = 0;
	if( self->noneValue >= 0 ){
		cst = self->noneValue;
	}else{
		cst = DaoRoutine_AddConstant( self->routine, dao_none_value );
		self->noneValue = cst;
	}
	enode.reg = DaoParser_PushRegister( self );
	enode.konst = self->noneValue = LOOKUP_BIND_LC( cst );
	DaoParser_PushTokenIndices( self, self->curToken, 0, 0 );
	DaoParser_AddCode( self, DVM_DATA, DAO_NONE, 0, enode.reg );
	enode.first = enode.last = enode.update = self->vmcLast;
	enode.prev = self->vmcLast->prev;
	return enode;
}
static int DaoParser_IntegerOne( DaoParser *self, int start )
{
	int reg = DaoParser_PushRegister( self );
	DaoParser_PushTokenIndices( self, self->curToken, 0, 0 );
	DaoParser_AddCode( self, DVM_DATA, DAO_INTEGER, 1, reg );
	return reg;
}
int DaoParser_DeclareVariable( DaoParser *self, DaoToken *tok, int storeType, DaoType *abtp )
{
	DaoVariable *var;
	DaoNamespace *nameSpace = self->nameSpace;
	DaoRoutine *routine = self->routine;
	DString *name = & tok->string;
	DMap *table = DaoParser_CurrentSymbolTable( self );
	DNode *it = MAP_Find( table, name );
	int perm = self->permission;
	int found, id = -1;

	if( self->isInterBody || self->isCinTypeBody ) goto OutOfContext;
	if( tok->name >= DKEY_LOAD && tok->name <= DKEY_PUBLIC ){
		DaoParser_Error3( self, DAO_TOKEN_NEED_NAME, tok->index );
		return -1;
	}
	if( storeType != 0 && it != NULL ) goto WasDefined;

	if( (storeType & DAO_DECL_VAR) && (storeType & DAO_DECL_LOCAL) ){
		id = self->regCount;
		if( abtp ) MAP_Insert( self->routine->body->localVarType, id, abtp );
		MAP_Insert( table, name, id );
		DaoParser_PushRegister( self );
		return id;
	}else if( storeType & DAO_DECL_MEMBER ){
		DaoClass *klass = self->hostClass;
		if( self->hostClass == NULL ) goto OutOfContext;
		if( self->isClassBody == 0 ) goto OutOfContext;
		it = DMap_Find( klass->lookupTable, name );
		if( it != NULL && LOOKUP_UP( it->value.pInt ) == 0 ) goto WasDefined;
		if( storeType & DAO_DECL_CONST ){
			id = DaoClass_AddConst( klass, name, dao_none_value, perm );
		}else if( storeType & DAO_DECL_STATIC ){
			id = DaoClass_AddGlobalVar( klass, name, NULL, abtp, perm );
		}else if( storeType & (DAO_DECL_VAR|DAO_DECL_INVAR) ){
			id = DaoClass_AddObjectVar( klass, name, NULL, abtp, perm );
			var = (DaoVariable*) DList_Back( klass->instvars );
			if( storeType & DAO_DECL_INVAR ) var->subtype = DAO_INVAR;
		}else{
			goto OutOfContext;
		}
		return id;
	}else if( storeType & DAO_DECL_GLOBAL ){
		it = DMap_Find( nameSpace->lookupTable, name );
		if( it != NULL && LOOKUP_UP( it->value.pInt ) == 0 ) goto WasDefined;
		if( storeType & DAO_DECL_CONST ){
			id = DaoNamespace_AddConst( nameSpace, name, dao_none_value, perm );
		}else{
			id = DaoNamespace_AddVariable( nameSpace, name, NULL, abtp, perm );
		}
		return id;
	}else if( storeType & DAO_DECL_STATIC ){
		DaoValue *value = abtp ? abtp->value : NULL;
		id = DaoNamespace_AddStaticVar( nameSpace, name, value, abtp, self->lexLevel );
		id = (id & ~(DAO_GLOBAL_VARIABLE<<24)) | (DAO_STATIC_VARIABLE<<24);
		MAP_Insert( table, name, id );
		routine->body->hasStatic = 1;
		return id;
	}

	if( storeType & DAO_DECL_CONST ){
		id = LOOKUP_BIND_LC( routine->routConsts->value->size );
		MAP_Insert( table, name, id );
		DaoRoutine_AddConstant( routine, dao_none_value );
	}else{
		id = self->regCount;
		if( abtp ) MAP_Insert( self->routine->body->localVarType, id, abtp );
		MAP_Insert( table, name, id );
		DaoParser_PushRegister( self );
	}
	tok = DList_Append( routine->body->defLocals, tok );
	DaoToken_Set( tok, !(storeType & DAO_DECL_CONST), self->lexLevel, id, NULL );

	found = DaoParser_GetRegister( self, tok );
	assert( found == id );
	MAP_Insert( table, name, found );
	return found;
OutOfContext:
	DaoParser_Error3( self, DAO_VARIABLE_OUT_OF_CONTEXT, tok->index );
	return -1;
WasDefined:
	DaoParser_Error( self, DAO_SYMBOL_WAS_DEFINED, name );
	return -1;
}
static int DaoParser_GetRegister2( DaoParser *self, DaoToken *nametok )
{
	DaoNamespace *NS = self->nameSpace;
	DaoRoutine *routine = self->routine;
	DString *name = & nametok->string;
	DNode *node = NULL;
	int i;

	if( self->hostCtype ){
		/* QStyleOption( version: int = QStyleOption::Version, ... ) */
		DaoValue *it = DaoType_FindValueOnly( self->hostType, name );
		if( it ){
			i = routine->routConsts->value->size;
			MAP_Insert( DaoParser_CurrentSymbolTable( self ), name, LOOKUP_BIND_LC(i) );
			DaoRoutine_AddConstant( routine, it );
			return LOOKUP_BIND_LC( i );
		}
	}
	if( self->isClassBody ){ /* a=1; b=class('t'){ var a = 2 }; */
		/* Look for variable in class: */
		if( self->hostClass && (node = MAP_Find( self->hostClass->lookupTable, name )) ){
			return node->value.pInt;
		}
	}

	/* Look for local data: */
	for( i=self->lexLevel; i>=0; i-- ){
		node = MAP_Find( self->lookupTables->items.pMap[i], name );
		if( node ) return node->value.pInt;
	}

	/* Look for variable in class: */
	if( self->hostClass && (node = MAP_Find( self->hostClass->lookupTable, name )) ){
		int st = LOOKUP_ST( node->value.pInt );
		if( st == DAO_OBJECT_VARIABLE ){
			if( routine->attribs & DAO_ROUT_STATIC ){
				DaoParser_ErrorToken( self, DAO_VARIABLE_OUT_OF_CONTEXT, nametok );
				return -1;
			}
		}
		return node->value.pInt;
	}

	if( (i = DaoNamespace_FindVariable( NS, name )) >= 0 ) return i;
	if( (node = MAP_Find( self->allConsts, name )) ) return LOOKUP_BIND_LC( node->value.pInt );
	if( (i = DaoNamespace_FindConst( NS, name )) >= 0 ) return i;
	return -1;
}
int DaoParser_GetRegister( DaoParser *self, DaoToken *nametok )
{
	int loc = DaoParser_GetRegister2( self, nametok );
	int out = LOOKUP_ST(loc) > DAO_LOCAL_CONSTANT && LOOKUP_ST(loc) < DAO_CLOSURE_VARIABLE;
	/* Search up-value if, name look up failed, or the name is not local: */
	if( self->outerParser != NULL && (loc < 0 || out) ){
		DaoRoutine *routine = self->routine;
		int i = DaoParser_GetRegister( self->outerParser, nametok );
		int st = LOOKUP_ST( i );
		/*
		// Use up-value if, name look up failed, or the up-value is local;
		// namely local data of the outer scope preceeds the global data;
		 */
		if( i >=0 && (loc < 0 || st <= DAO_LOCAL_CONSTANT) ){
			if( st == DAO_LOCAL_CONSTANT ){
				int id = LOOKUP_ID( i );
				DaoValue *cst = self->outerParser->routine->routConsts->value->items.pValue[id];
				i = LOOKUP_BIND_LC( routine->routConsts->value->size );
				MAP_Insert( DaoParser_CurrentSymbolTable( self ), & nametok->string, i );
				DaoRoutine_AddConstant( routine, cst );
			}else{
				int tokpos = nametok->index;
				if( routine->body->upValues == NULL ){
					routine->body->upValues = DList_New( DAO_DATA_VALUE );
				}
				i = DaoParser_GetNormRegister( self->outerParser, i, 0, tokpos, 0, tokpos );
				DArray_PushInt( self->uplocs, i );
				DArray_PushInt( self->uplocs, routine->body->upValues->size + DAO_MAX_PARAM );
				DArray_PushInt( self->uplocs, tokpos );
				DArray_PushInt( self->uplocs, tokpos );
				i = LOOKUP_BIND( DAO_CLOSURE_VARIABLE, 0, 0, routine->body->upValues->size );
				MAP_Insert( DaoParser_CurrentSymbolTable( self ), & nametok->string, i );
				DList_Append( routine->body->upValues, DaoVariable_New(NULL,NULL,0) );
			}
			return i;
		}
	}
	return loc;
}
DaoValue* DaoParser_GetVariable( DaoParser *self, int reg )
{
	DaoInteger daoIntegerZero = {DAO_INTEGER,0,0,0,1,0};
	DaoInteger daoIntegerOne  = {DAO_INTEGER,0,0,0,1,1};
	DaoComplex daoComplexImag = {DAO_COMPLEX,0,0,0,1,{0.0,1.0}};
	DaoNamespace *ns = self->nameSpace;
	DaoRoutine *routine = self->routine;
	DaoClass *klass = self->hostClass;
	DaoValue *val = NULL;
	int st = LOOKUP_ST( reg );
	int up = LOOKUP_UP( reg );
	int id = LOOKUP_ID( reg );

	if( st == DAO_LOCAL_VARIABLE ){
		if( reg == self->integerZero ){
			val = (DaoValue*) & daoIntegerZero;
		}else if( reg == self->integerOne ){
			val = (DaoValue*) & daoIntegerOne;
		}else if( reg == self->imaginaryOne ){
			val = (DaoValue*) & daoComplexImag;
		}
		return val;
	}
	switch( st ){
	case DAO_LOCAL_CONSTANT  : val = routine->routConsts->value->items.pValue[id]; break;
	case DAO_CLASS_CONSTANT  : val = klass->constants->items.pConst[id]->value; break;
	case DAO_GLOBAL_VARIABLE : val = ns->variables->items.pVar[id]->value; break;
	case DAO_GLOBAL_CONSTANT : val = ns->constants->items.pConst[id]->value; break;
	case DAO_STATIC_VARIABLE : val = ns->variables->items.pVar[id]->value; break;
	default : break;
	}
	return val;
}
int DaoParser_GetNormRegister( DaoParser *self, int reg, int exp, int first, int mid, int last )
{
	DaoVmCodeX vmc = {0,0,0,0,0,0,0,0,0,0};
	int line = self->tokens->items.pToken[first]->line;
	int st = LOOKUP_ST( reg );
	int up = LOOKUP_UP( reg );
	int id = LOOKUP_ID( reg );
	int regc, code = DVM_NOP;

	/* printf( "reg = %x\n", reg ); */
	switch( st ){
	case DAO_LOCAL_VARIABLE :
		up = DaoParser_GetOuterLevel( self, id );
		if( up == 0 ) return id;
		code = DVM_GETVH; /* Host variable accessed from code sections: */
		break;
	case DAO_LOCAL_CONSTANT  :
		code = DVM_GETCL;
		up = exp;
		break;
	case DAO_OBJECT_VARIABLE  : code = DVM_GETVO; break;
	case DAO_CLASS_VARIABLE   : code = DVM_GETVK; break;
	case DAO_CLASS_CONSTANT   : code = DVM_GETCK; break;
	case DAO_GLOBAL_VARIABLE  : code = DVM_GETVG; break;
	case DAO_GLOBAL_CONSTANT  : code = DVM_GETCG; break;
	case DAO_STATIC_VARIABLE  : code = DVM_GETVG; up = 1; break;
	case DAO_CLOSURE_VARIABLE : code = DVM_GETVS; break;
	default : break;
	}
	self->usingGlobal |= code == DVM_GETVG;
	/*
	   printf( "i = %i %s %i\n", i, DaoVmCode_GetOpcodeName(get), leftval );
	 */
	regc = DaoParser_PushRegister( self );
	DaoVmCode_Set( & vmc, code, up, id, regc, self->lexLevel, line, first, mid, last );
	DaoParser_PushBackCode( self, & vmc );
	return regc;
}

int DaoParser_PostParsing( DaoParser *self )
{
	DaoRoutine *routine = self->routine;
	DaoVmCodeX **vmCodes;
	int i, j, k;

	DaoRoutine_SetSource( routine, self->tokens, self->nameSpace );
	if( DaoParser_SetupBranching( self ) == 0 ) return 0;

	routine->body->regCount = self->regCount;

	vmCodes = self->vmCodes->items.pVmc;

	if( DaoRoutine_SetVmCodes( routine, self->vmCodes ) ==0) return 0;

	if( self->byteBlock ) DaoByteCoder_FinalizeRoutineBlock( self->byteCoder, self->byteBlock );
	/*
	   DaoRoutine_PrintCode( routine, self->vmSpace->errorStream );
	 */
	return 1;
}
int DaoNamespace_CyclicParent( DaoNamespace *self, DaoNamespace *parent );
int DaoParser_ParseLoadStatement( DaoParser *self, int start, int end )
{
	DaoNamespace *mod = NULL, *nameSpace = self->nameSpace;
	DaoRoutine *mainRout = nameSpace->mainRoutine;
	DaoVmSpace *vmSpace = self->vmSpace;
	DaoToken **tokens = self->tokens->items.pToken;
	DList   *modpaths = DList_New( DAO_DATA_STRING );
	DList   *modlist = DaoParser_GetArray( self );
	DString *modpath = DaoParser_GetString( self );
	DString *modname = NULL;
	int i = start+1, j, code = 0, cyclic = 0;
	int perm = self->permission;
	unsigned char tki;

	if( i > end ) goto ErrorLoad;
	if( (self->levelBase + self->lexLevel) != 0 ) goto ErrorLoad;

	tki = tokens[i]->name;
	if( tki == DTOK_MBS || tki == DTOK_WCS ){
		DString_SubString( & tokens[i]->string, modpath, 1, tokens[i]->string.size-2 );
		DList_Append( modpaths, modpath );
		i ++;
	}else if( tki == DKEY_AS ){
		code = DAO_CTW_LOAD_INVALID;
		goto ErrorLoad;
	}else if( tki == DTOK_IDENTIFIER && i < end && tokens[i+1]->name == DTOK_LB ){
		DaoEnode enode;
		self->curToken = i;
		self->needConst += 1;
		enode = DaoParser_ParseExpression( self, 0 );
		self->needConst -= 1;
		if( enode.reg < 0 || enode.konst == 0 ) goto ErrorLoad;
		i = self->curToken;
		mod = (DaoNamespace*) DaoParser_GetVariable( self, enode.konst );
		if( mod == NULL || mod->type != DAO_NAMESPACE ) goto ErrorLoad;
		DList_Append( modlist, mod );
		if( self->byteBlock ){
			DaoByteBlock_EncodeImport( self->byteBlock, (DaoValue*) mod, NULL, 0, 0 );
		}
	}else{
		while( i <= end && tokens[i]->type == DTOK_IDENTIFIER ){
			DString_Append( modpath, & tokens[i]->string );
			i += 1;
			if( i <= end && (tokens[i]->type == DTOK_COLON2 || tokens[i]->type == DTOK_DOT) ){
				i += 1;
				DString_AppendChars( modpath, "/" );
			}else break;
		}
		if( i <= end && tokens[i]->type == DTOK_LCB ){
			i += 1;
			if( i > end || tokens[i]->type != DTOK_IDENTIFIER ) goto ErrorLoad;
			while( i <= end && tokens[i]->type == DTOK_IDENTIFIER ){
				DString *path = (DString*) DList_Append( modpaths, modpath );
				DString_Append( path, & tokens[i]->string );
				i += 1;
				if( i > end ) goto ErrorLoad;
				if( tokens[i]->type == DTOK_RCB ) break;
				if( tokens[i]->type != DTOK_COMMA ) goto ErrorLoad;
				i += 1;
			}
			if( i > end || tokens[i]->type != DTOK_RCB ) goto ErrorLoad;
			i += 1;
		}else{
			DList_Append( modpaths, modpath );
		}
	}
	if( i <= end && tokens[i]->name == DKEY_AS ){
		if( modpaths->size > 1 ) goto ErrorLoad;
		if( (i+1) > end || tokens[i+1]->type != DTOK_IDENTIFIER ){
			code = DAO_CTW_LOAD_INVA_MOD_NAME;
			goto ErrorLoad;
		}
		modname = & tokens[i+1]->string;
		i += 2;
	}

	if( mod == NULL ){
		for(j=0; j<modpaths->size; ++j){
			DString_Assign( self->string, modpaths->items.pString[j] );
			if( (mod = DaoNamespace_FindNamespace(nameSpace, self->string)) == NULL ){
				mod = DaoVmSpace_LoadModule( vmSpace, self->string );
				if( mod == NULL && modname == NULL ){
					mod = DaoVmSpace_FindModule( vmSpace, self->string );
					cyclic = mod && DaoNamespace_CyclicParent( mod, nameSpace );
					mod = NULL;
				}
			}
			if( mod == NULL ) break;
			DList_Append( modlist, mod );
		}
	}

	if( mod == NULL ){
		code = DAO_CTW_LOAD_FAILED;
		if( vmSpace->stopit ) code = DAO_CTW_LOAD_CANCELLED;
		goto ErrorLoad;
	}
	for(j=0; j<modpaths->size; ++j){
		mod = modlist->items.pNS[j];
		DString_Assign( self->string, modpaths->items.pString[j] );
		if( self->byteBlock ){
			DaoByteBlock_EncodeLoad( self->byteBlock, mod, self->string, modname );
		}
		if( modname == NULL ){
			cyclic = (DaoNamespace_AddParent( nameSpace, mod ) == 0);
		}else{
			DaoNamespace_AddConst( nameSpace, modname, (DaoValue*) mod, perm );
		}
		if( cyclic ) DaoParser_Warn( self, DAO_LOAD_CYCLIC, NULL );
	}

	while( i <= end && tokens[i]->name == DKEY_IMPORT && tokens[i]->line == tokens[i-1]->line ){
		do {
			if( (i+1) > end ) goto ErrorLoad;
			if( tokens[i+1]->name == DTOK_IDENTIFIER ){
				i = DaoParser_ParseImportStatement( self, i+1, end, 1 );
				if( i < 0 ) goto ErrorLoad;
			}else if( tokens[i+1]->name == DTOK_LCB ){
				if( modlist->size > 1 ) goto ErrorLoad;
				i = DaoParser_ImportSymbols( self, mod, i+1, end, 0 );
				if( i < 0 ) goto ErrorLoad;
				i += 1;
			}else{
				goto ErrorLoad;
			}
		} while( i <= end && tokens[i]->name == DTOK_COMMA );
	}

	/*
	   printf("ns=%p; mod=%p; myns=%p\n", ns, mod, nameSpace);
	 */
	DaoParser_CheckStatementSeparation( self, i-1, end );
	if( modpaths ) DList_Delete( modpaths );

	return i;
ErrorLoad:
	DaoParser_Error( self, code, NULL );
	if( code != DAO_CTW_LOAD_FAILED ) DaoParser_Error( self, DAO_CTW_LOAD_FAILED, NULL );
	if( modpaths ) DList_Delete( modpaths );
	return -1;
}
static int DaoParser_Import( DaoParser *self, DaoNamespace *mod, DString *name )
{
	DaoValue *exist = NULL, *value = NULL;
	DaoNamespace *NS = self->nameSpace;
	DMap *symtable = DaoParser_CurrentSymbolTable( self );
	DNode *it = DMap_Find( mod->lookupTable, name );
	int pm = it ? LOOKUP_PM( it->value.pInt ) : 0;
	int level = self->levelBase + self->lexLevel;
	int i, ret = 0;

	if( it == NULL || pm != DAO_PERM_PUBLIC ) return -1;
	if( (self->levelBase + self->lexLevel) == 0 ){
		if( LOOKUP_ST( it->value.pInt ) == DAO_GLOBAL_CONSTANT ){
			value = mod->constants->items.pConst[ LOOKUP_ID(it->value.pInt) ]->value;
			ret = DaoNamespace_AddConst( NS, name, value, DAO_PERM_PUBLIC );
		}else{
			DaoVariable *var = mod->variables->items.pVar[ LOOKUP_ID(it->value.pInt) ];
			ret = DaoNamespace_AddVariable( NS, name, var->value, var->dtype, pm );
		}
	}else if( DMap_Find( symtable, name ) != NULL ){
		goto ErrorSymbolDefined;
	}else if( LOOKUP_ST( it->value.pInt ) == DAO_GLOBAL_CONSTANT ){
		/*
		// Not overloading for locally imported routines, because "import module.name"
		// should be semantically equivalent to "const name = module.name";
		*/
		value = mod->constants->items.pConst[ LOOKUP_ID(it->value.pInt) ]->value;
		ret = DaoNamespace_AddStaticConst( NS, name, value, level );
		MAP_Insert( symtable, name, ret );
	}else{
		DaoVariable *var = mod->variables->items.pVar[ LOOKUP_ID(it->value.pInt) ];
		ret = DaoNamespace_AddStaticVar( NS, name, var->value, var->dtype, level );
		ret = (ret & ~(DAO_GLOBAL_VARIABLE<<24)) | (DAO_STATIC_VARIABLE<<24);
		MAP_Insert( symtable, name, ret );
		self->routine->body->hasStatic = 1;
	}
	if( ret < 0 ) goto ErrorSymbolDefined;
	return ret;
ErrorSymbolDefined:
	DaoParser_Error( self, DAO_SYMBOL_WAS_DEFINED, name );
	return -1;
}
static int DaoParser_ImportSymbols( DaoParser *self, DaoNamespace *mod, int start, int to, int level )
{
	DaoToken **tokens = self->tokens->items.pToken;
	int error = DAO_INVALID_IMPORT_STMT;
	int id, i = start + 1;
	while( i <= to ){
		int ret = 0;
		if( DaoParser_CheckNameToken( self, i, to, error, i ) == 0 ) return -1;
		id = DaoParser_Import( self, mod, & tokens[i]->string );
		if( id < 0 || i >= to ) return -1;
		if( self->byteBlock ){
			DString *name = & tokens[i]->string;
			id = LOOKUP_ID( id );
			DaoByteBlock_EncodeImport( self->byteBlock, (DaoValue*)mod, name, level, id );
		}
		if( tokens[i+1]->name == DTOK_RCB ) break;
		if( tokens[i+1]->name != DTOK_COMMA || (i+2) > to ) return -1;
		i += 2;
	}
	return i + 1;
}
static int DaoParser_ParseImportStatement( DaoParser *self, int start, int to, int full )
{
	DaoValue *value = NULL;
	DaoToken **tokens = self->tokens->items.pToken;
	DaoNamespace *mod, *NS = self->nameSpace;
	DaoRoutine *routine = self->routine;
	DMap *symtable = DaoParser_CurrentSymbolTable( self );
	int level = self->levelBase + self->lexLevel;
	int error = DAO_INVALID_IMPORT_STMT;
	int estart = start;
	int import = start;
	int i, id, tok;

	start += tokens[start]->name == DKEY_IMPORT;
	if( DaoParser_CheckNameToken( self, start, to, error, import ) == 0 ) return -1;
	if( self->isClassBody || self->isInterBody || self->isCinTypeBody ) goto InvalidImport;

	start = DaoParser_FindMaybeScopedConst( self, & value, start, DTOK_DOT );
	if( start < 0 || value == NULL || value->type != DAO_NAMESPACE ) goto InvalidImport;

	mod = (DaoNamespace*) value;
	if( (start+2) > to || tokens[start+1]->type != DTOK_DOT ){
		if( full == 0 ) goto InvalidImport;
		DaoNamespace_AddParent( NS, mod );
		if( self->byteBlock ){
			DaoByteBlock_EncodeImport( self->byteBlock, (DaoValue*) mod, NULL, 0, 0 );
		}
		return start+1;
	}

	start += 2;
	tok = tokens[start]->type;
	if( tok == DTOK_IDENTIFIER ){
		if( DaoParser_CheckNameToken( self, start, to, error, import ) == 0 ) return -1;
		id = DaoParser_Import( self, mod, & tokens[start]->string );
		if( id < 0 ) goto InvalidImport;
		if( self->byteBlock ){
			DString *name = & tokens[start]->string;
			id = LOOKUP_ID( id );
			DaoByteBlock_EncodeImport( self->byteBlock, value, name, level, LOOKUP_ID(id) );
		}
	}else if( tok == DTOK_LCB ){
		start = DaoParser_ImportSymbols( self, mod, start, to, level );
		if( start < 0 ) goto InvalidImport;
	}else{
		goto InvalidImport;
	}
	return start + 1;
InvalidImport:
	DaoParser_Error3( self, DAO_INVALID_IMPORT_STMT, estart );
	return -1;
}
int DaoParser_ParseNamespaceStatement( DaoParser *self, int start, int end )
{
	DaoToken **tokens = self->tokens->items.pToken;
	DaoNamespace *NS = self->nameSpace;
	DaoNamespace *defNS = NULL;
	DaoValue *scope = NULL, *value = NULL;
	DString *spaceName, *ename = NULL;
	daoint i, k, rb, right, error = 0;
	int errorStart = start;
	int perm = self->permission;

	if( start+1 > end ) goto InvalidNamespace;
	if( (self->levelBase + self->lexLevel) != 0 ) goto InvalidNamespace;

	start = DaoParser_ParseScopedConstOrName( self, & scope, & value, start+1, DTOK_LCB );
	if( start <0 ) goto InvalidNamespace;

	ename = & tokens[start]->string;
	if( value == NULL || value->type == 0 ){
		DString *name = & tokens[start]->string;
		int tok = tokens[start]->name;
		if( tok != DTOK_IDENTIFIER && tok < DKEY_CEIL ) goto InvalidNamespace;
		if( scope && scope->type != DAO_NAMESPACE ) goto InvalidNamespace;

		defNS = DaoNamespace_New( NS->vmSpace, name->chars );
		value = (DaoValue*) defNS;
		if( scope ){
			DaoNamespace_AddConst( (DaoNamespace*) scope, name, value, DAO_PERM_PUBLIC );
		}else{
			DaoNamespace_AddConst( NS, name, value, DAO_PERM_PUBLIC );
		}
		if( self->byteBlock ){
			DaoByteBlock_AddNamespace( self->byteBlock, defNS, name, (DaoNamespace*) scope );
		}
	}else{
		if( value->type != DAO_NAMESPACE ) goto InvalidNamespace;
		defNS = (DaoNamespace*) value;
		if( self->byteBlock ) DaoByteBlock_AddNamespace( self->byteBlock, defNS, NULL, defNS );
	}
	if( tokens[++start]->type != DTOK_LCB ) goto InvalidNamespace;
	rb = DaoParser_FindPairToken( self, DTOK_LCB, DTOK_RCB, start, end );
	if( rb < 0 ) goto InvalidNamespace;
	
	if( self->nsDefines == NULL ){
		self->nsDefines = DList_New(0);
		self->nsSymbols = DaoLexer_New();
	}
	DList_Append( self->nsDefines, defNS );
	DList_Append( self->nsDefines, self->nsSymbols->tokens->size );
	DList_Append( self->nsDefines, self->nsSymbols->tokens->size );
	start += 1;
	while( start < rb ){
		int ret = 0;
		if( DaoParser_CheckNameToken( self, start, rb, error, start ) == 0 ){
			goto InvalidNamespace;
		}
		DaoLexer_AppendToken( self->nsSymbols, tokens[start] );
		if( tokens[start+1]->name == DTOK_RCB ) break;
		if( tokens[start+1]->name != DTOK_COMMA || (start+2) > rb ) goto InvalidNamespace;
		start += 2;
	}
	self->nsDefines->items.pInt[self->nsDefines->size-1] = self->nsSymbols->tokens->size;
	return rb + 1;

SymbolWasDefined:
	DaoParser_Error( self, DAO_SYMBOL_WAS_DEFINED, ename );
	goto InvalidNamespace;
InvalidSymbolName:
	goto InvalidNamespace;
InvalidNamespace:
	DaoParser_Error2( self, DAO_INVALID_NAMESPACE_DEFINITION, errorStart, end, 0 );
	return -1;
}

static void DaoParser_TryAddSetVX( DaoParser *self, int index, int local, int first, int mid, int last )
{
	int st = LOOKUP_ST( index );
	int up = LOOKUP_UP( index );
	int id = LOOKUP_ID( index );
	int set = 0;
	switch( st ){
	case DAO_LOCAL_VARIABLE  :
		if( up ){
			up = 0;
			set = DVM_SETVH;
		}else if( (up = DaoParser_GetOuterLevel( self, id )) > 0 ){
			set = DVM_SETVH;
		}
		break;
	case DAO_OBJECT_VARIABLE : set = DVM_SETVO; break;
	case DAO_CLASS_VARIABLE  : set = DVM_SETVK; break;
	case DAO_GLOBAL_VARIABLE : set = DVM_SETVG; break;
	case DAO_STATIC_VARIABLE : set = DVM_SETVG; break;
	}
	self->usingGlobal |= set == DVM_SETVG;
	DaoParser_PushTokenIndices( self, first, mid, last );
	if( set ) DaoParser_AddCode( self, set, local, id, up );
	DaoParser_PopTokenIndices( self, 1 );
}



enum DaoForInLoopData
{
	FOR_IN_CONTAINER ,  /* Container variable index; */
	FOR_IN_IMP_ITEM  ,  /* Implicit item variable index (local);  */
	FOR_IN_EXP_ITEM  ,  /* Explicit item variable index (global/local); */
	FOR_IN_TOKEN1    ,  /* First token index; */
	FOR_IN_TOKEN2    ,  /* Last token index; */
	FOR_IN_NAME      ,  /* Name string; */
	FOR_IN_STEP      ,
};

int DaoParser_ParseForLoop( DaoParser *self, int start, int end )
{
	DaoInode *opening, *closing, *inode;
	DaoToken *tok, **tokens = self->tokens->items.pToken;
	int semic1, semic2, reg1, reg2, fromCode, colon1, colon2;
	int pos, movetype = 0, movemask = 0x3;
	int cst, store = 0, forever = 0;
	int rb = -1;
	int in = -1;
	if( start+1 >= self->tokens->size ) return -1;
	if( tokens[start+1]->name == DTOK_LB )
		rb = DaoParser_FindPairToken( self, DTOK_LB, DTOK_RB, start, -1 );
	if( rb >= 0 ) in = DaoParser_FindOpenToken( self, DKEY_IN, start+2, rb, 0 );
	if( (rb < 0 || rb >= end) && in < 0 ) return -1;
	if( tokens[start+2]->name == DKEY_VAR ){
		store = DAO_DECL_LOCAL|DAO_DECL_VAR;
		movetype = 1|(1<<1);
		start += 1;
	}else if( tokens[start+2]->name == DKEY_INVAR ){
		store = DAO_DECL_LOCAL|DAO_DECL_INVAR;
		movetype = 1|(3<<1);
		movemask = 0x0;
		start += 1;
	}

	DaoParser_AddScope( self, DVM_UNUSED, NULL );
	if( in >= 0 ){
		DList *tuples;
		int k, L, elem, semic, regItemt, reg, exp, first, firstIter;
		daoint *t;

		elem = start + 2;
		semic = DaoParser_FindOpenToken( self, DTOK_SEMCO, start+2, rb, 0 );
		if( semic < 0 && elem < rb ) semic = rb;
		first = 1;
		regItemt = 0;
		tuples = DList_New(0);
		while( semic >=0 ){
			if( tokens[elem+1]->name != DKEY_IN ){
				DaoParser_Error( self, DAO_INVALID_FORIN, NULL );
				goto CleanUp;
			}
			tok = tokens[elem];
			reg = DaoParser_GetRegister( self, tok );
			if( reg < 0 || store ) reg = DaoParser_DeclareVariable( self, tok, store, NULL );
			if( reg < 0 ) goto CleanUp;
			if( LOOKUP_ISCST( reg ) ){
				DaoParser_Error( self, DAO_EXPR_MODIFY_CONSTANT, & tok->string );
				goto CleanUp;
			}
			exp = reg;
			if( LOOKUP_ST( reg ) > DAO_LOCAL_CONSTANT ) reg = DaoParser_PushRegister( self );
			cst = 0;
			reg1 = DaoParser_MakeArithTree( self, elem+2, semic-1, & cst );
			if( reg1 < 0 ){
				DaoParser_Error( self, DAO_INVALID_FORIN, NULL );
				goto CleanUp;
			}
			DList_Append( tuples, reg1 ); /* list */
			DList_Append( tuples, reg ); /* item */
			DList_Append( tuples, exp ); /* item */
			DList_Append( tuples, elem ); /* first token */
			DList_Append( tuples, semic-1 ); /* last token */
			DList_Append( tuples, & tok->string ); /* name */

			elem = semic + 1;
			semic = DaoParser_FindOpenToken( self, DTOK_SEMCO, elem, rb, 0 );
			if( semic < 0 && elem < rb ) semic = rb;
			first = 0;
		}
		L = tokens[rb]->line;
		fromCode = self->vmcCount;
		firstIter = self->regCount;
		for(k=0, t=tuples->items.pInt; k<tuples->size; k+=FOR_IN_STEP, t+=FOR_IN_STEP){
			daoint first = t[FOR_IN_TOKEN1];
			daoint last = t[FOR_IN_TOKEN2];
			daoint cont = t[FOR_IN_CONTAINER];
			DaoParser_PushTokenIndices( self, first, first+1, last );
			DaoParser_AddCode( self, DVM_ITER, cont, 0, self->regCount );
			DaoParser_PushRegister( self );
		}
		/* see the comments for parsing if-else: */
		DaoParser_PushTokenIndices( self, start, 0, 0 );
		closing = DaoParser_AddCode( self, DVM_LABEL, 0, 1, 0 );
		opening = DaoParser_AddScope( self, DVM_LOOP, closing );

		reg = DaoParser_PushRegister( self );
		DaoParser_PushTokenIndices( self, start, in, rb );
		DaoParser_AddCode( self, DVM_ITER, firstIter, tuples->size/FOR_IN_STEP, reg );
		DaoParser_AddCode( self, DVM_TEST, reg, fromCode, 0 );
		opening->jumpTrue = self->vmcLast;
		self->vmcLast->jumpFalse = closing;
		reg = DaoParser_PushRegister( self );
		DaoParser_AddCode( self, DVM_DATA, DAO_INTEGER, 0, reg );
		for(k=0, t=tuples->items.pInt; k<tuples->size; k+=FOR_IN_STEP, t+=FOR_IN_STEP){
			daoint first = t[FOR_IN_TOKEN1];
			daoint last = t[FOR_IN_TOKEN2];
			daoint iter = firstIter + (k/FOR_IN_STEP);
			daoint cont = t[FOR_IN_CONTAINER];
			daoint item = t[FOR_IN_IMP_ITEM];
			daoint item2 = t[FOR_IN_EXP_ITEM];
			DaoParser_PushTokenIndices( self, first, first+1, last );
			DaoParser_AddCode( self, DVM_GETI, cont, iter, self->regCount );
			DaoParser_AddCode( self, DVM_MOVE, self->regCount, movetype&movemask, item );
			DaoParser_PushRegister( self );
			if( LOOKUP_ST( item2 ) > DAO_LOCAL_CONSTANT ){
				DaoParser_TryAddSetVX( self, item2, item, first, 0, last );
			}
		}

		if( store & DAO_DECL_INVAR ){
			for(k=0; k<tuples->size; k+=FOR_IN_STEP){
				DString *name = tuples->items.pString[k+FOR_IN_NAME];
				int opa = tuples->items.pInt[k+FOR_IN_IMP_ITEM];
				int reg = DaoParser_PushRegister( self );
				MAP_Insert( DaoParser_CurrentSymbolTable( self ), name, reg );
				DaoParser_PushTokenIndices( self, rb, 0, 0 );
				DaoParser_AddCode( self, DVM_MOVE, opa, movetype, reg );
			}
		}
		start = 1 + rb + DaoParser_AddScope2( self, rb+1 );
		DList_Delete( tuples );
		return start;
CleanUp:
		DList_Delete( tuples );
		return -1;
	}
	colon1 = DaoParser_FindOpenToken( self, DTOK_COLON, start+2, rb, 0 );
	if( colon1 >=0 ){
		int eq, index, first, step, last = 0;
		int loc, pos, st, up, id, set = 0;
		eq = DaoParser_FindOpenToken( self, DTOK_ASSN, start+2, colon1, 1 );
		if( eq < 0 ) return -1;
		if( start+2 != eq-1 ){
			DString_SetChars( self->string, "need a variable" );
			DaoParser_Error( self, DAO_INVALID_FOR, self->string );
			return -1;
		}
		tokens[colon1]->type = DTOK_SEMCO;
		tokens[colon1]->name = DTOK_SEMCO;
		st = store & (~DAO_DECL_INVAR); /* To compile "invar a=init" as "var a=init"; */
		pos = DaoParser_ParseVarExpressions( self, start+2, colon1-1, st );
		tokens[colon1]->type = DTOK_COLON;
		tokens[colon1]->name = DTOK_COLON;
		if( pos < 0 ) return -1;
		tok = tokens[start+2];
		index = DaoParser_GetRegister( self, tok );
		if( index < 0 ) return -1;
		st = LOOKUP_ST( index );
		loc = index;
		if( LOOKUP_ISCST( st ) ){
			DString_SetChars( self->string, "can not modify constant" );
			DaoParser_Error( self, DAO_INVALID_FOR, self->string );
			return -1;
		}else if( st >= DAO_LOCAL_CONSTANT ){
			loc = DaoParser_GetNormRegister( self, loc, 0, start+2, eq, colon1 );
		}
		pos = tokens[eq]->line;
		if( colon1 + 1 == rb ){
			/* infinite looping */
			forever = 1;
			step = DaoParser_IntegerOne( self, colon1 );
		}else{
			colon2 = DaoParser_FindOpenToken( self, DTOK_COLON, colon1+1, rb, 0 );
			if( colon2 >= 0 ){
				step = DaoParser_MakeArithTree( self, colon1+1, colon2-1, & cst );
				last = DaoParser_MakeArithTree( self, colon2+1, rb-1, & cst );
			}else{
				step = DaoParser_IntegerOne( self, colon1 );
				last = DaoParser_MakeArithTree( self, colon1+1, rb-1, & cst );
			}
			if( step < 0 || last <0 ) return -1;
		}
		DaoParser_TryAddSetVX( self, index, loc, start+2, eq, colon1 );

		pos = tokens[colon1]->line;
		/* see the comments for parsing if-else: */
		DaoParser_PushTokenIndices( self, start+2, eq, rb-1 );
		inode = DaoParser_AddCode( self, DVM_GOTO, 0, 0, 0 );
		closing = DaoParser_AddCode( self, DVM_LABEL, 0, 1, 0 );
		opening = DaoParser_AddScope( self, DVM_LOOP, closing );
		DaoParser_AddCode( self, DVM_ADD, loc, step, loc );
		DaoParser_TryAddSetVX( self, index, loc, start+2, eq, colon1 );
		if( forever ){
			DaoParser_AddCode( self, DVM_NOP, 0, 0, 0 );
			inode->jumpTrue = self->vmcLast;
		}else{
			DaoParser_AddCode( self, DVM_LE, loc, last, self->regCount );
			inode->jumpTrue = self->vmcLast;
			DaoParser_PushTokenIndices( self, start, colon1, rb );
			DaoParser_AddCode( self, DVM_TEST, self->regCount, 0, 0 );
		}
		opening->jumpTrue = self->vmcLast;
		self->vmcLast->jumpFalse = closing;
		DaoParser_PushRegister( self );
		if( store & DAO_DECL_INVAR ){
			int reg = DaoParser_PushRegister( self );
			MAP_Insert( DaoParser_CurrentSymbolTable( self ), & tok->string, reg );
			DaoParser_PushTokenIndices( self, rb, 0, 0 );
			DaoParser_AddCode( self, DVM_MOVE, loc, movetype, reg );
		}
		return 1 + rb + DaoParser_AddScope2( self, rb+1 );
	}
	semic1 = DaoParser_FindOpenToken( self, DTOK_SEMCO, start+2, rb, 1 );
	semic2 = DaoParser_FindOpenToken( self, DTOK_SEMCO, semic1+1, rb, 1 );
	if( rb <0 || semic1 <0 || semic2 <0 ){
		DaoParser_Error( self, DAO_INVALID_FOR, NULL );
		return -1;
	}
	/* init arith; */
	cst = 0;
	if( start+2 < semic1 ){
		pos = DaoParser_ParseVarExpressions( self, start+2, semic1-1, store );
		if( pos < 0 ) return -1;
		if( pos != semic1 ){
			DaoParser_Error2( self, DAO_INVALID_EXPRESSION, start+2, semic1-1, 0 );
			return -1;
		}
	}
	/* see the comments for parsing if-else: */
	DaoParser_PushTokenIndices( self, start+2, 0, 0 );
	inode = DaoParser_AddCode( self, DVM_GOTO, 0, 0, 0 );
	closing = DaoParser_AddCode( self, DVM_LABEL, 0, 1, 0 );
	opening = DaoParser_AddScope( self, DVM_LOOP, closing );
	/* step arith */
	if( semic2 + 1 == rb ){
		DaoParser_PushTokenIndices( self, semic2, semic2, rb );
		DaoParser_AddCode( self, DVM_NOP, 0, 0, 0 );
	}else{
		DaoEnode enode;
		self->curToken = semic2 + 1;
		enode = DaoParser_ParseExpression( self, DTOK_COMMA );
		while( enode.reg >= 0 && self->curToken < rb ){
			self->curToken += 1;
			enode = DaoParser_ParseExpression( self, DTOK_COMMA );
		}
		if( enode.reg < 0 ){
			DaoParser_Error2( self, DAO_INVALID_EXPRESSION, semic2+1, rb-1, 0 );
			return -1;
		}
	}
	/* cond airth */
	DaoParser_PushTokenIndices( self, start, semic1, semic2 );
	DaoParser_AddCode( self, DVM_UNUSED, 0, 0, 0 );
	inode->jumpTrue = self->vmcLast;
	if( semic1 + 1 == semic2 ){
		DaoParser_AddCode( self, DVM_NOP, 0, 0, 0 );
	}else{
		reg1 = DaoParser_MakeArithTree( self, semic1+1, semic2-1, & cst );
		if( reg1 < 0 ) return -1;
		DaoParser_PushTokenIndices( self, start, start+1, rb );
		DaoParser_AddCode( self, DVM_TEST, reg1, 0, 0 );
	}
	opening->jumpTrue = self->vmcLast;
	self->vmcLast->jumpFalse = closing;

AddScope:
	return 1 + rb + DaoParser_AddScope2( self, rb+1 );
}
/* Parse a condition test expression: */
int DaoParser_ParseCondition( DaoParser *self, int start, int dec, DaoInode *opening )
{
	DaoToken **tokens = self->tokens->items.pToken;
	int from = self->vmcCount;
	int pos, semico, lb = start, rb = -1;
	int reg, cst = 0, store = 0;

	if( start < self->tokens->size && tokens[start]->name == DTOK_LB ){
		rb = DaoParser_FindPairToken( self, DTOK_LB, DTOK_RB, start, -1 );
	}else{
		DString_SetChars( self->string, "()" );
		DaoParser_Error( self, DAO_CTW_IS_EXPECTED, self->string );
	}
	if( lb < 0 || rb < 0 ) return -1;
	if( opening ) DaoParser_AppendCode( self, opening ); /* move to back */

	start = lb + 1;
	semico = DaoParser_FindOpenToken( self, DTOK_SEMCO, start, rb, 0 );
	if( dec && semico >= 0 ){
		if( tokens[start]->name == DKEY_VAR ){
			store = DAO_DECL_LOCAL|DAO_DECL_VAR;
			start += 1;
		}else if( tokens[start]->name == DKEY_INVAR ){
			store = DAO_DECL_LOCAL|DAO_DECL_INVAR;
			start += 1;
		}
		pos = DaoParser_ParseVarExpressions( self, start, rb-1, store );
		if( pos < 0 ) return -1;
		if( pos != semico ){
			DaoParser_Error4( self, DAO_TOKEN_EXPECTING, tokens[pos]->line, ";" );
			return -1;
		}
		start = semico + 1;
	}

	reg = DaoParser_MakeArithTree( self, start, rb-1, & cst );
	if( reg < 0 ) return -1;
	DaoParser_PushTokenIndices( self, lb-1, lb, rb );
	DaoParser_AddCode( self, DVM_TEST, reg, from, 0 );
	return rb;
}

static int DaoParser_AddFieldConst( DaoParser *self, DString *field )
{
	DString_SetChars( self->string, "." );
	DString_Append( self->string, field );
	if( MAP_Find( self->allConsts, self->string )==NULL ){
		DaoString str = {DAO_STRING,0,0,0,0,NULL};
		str.value = field;
		MAP_Insert( self->allConsts, self->string, self->routine->routConsts->value->size );
		DaoRoutine_AddConstant( self->routine, (DaoValue*) & str );
	}
	return MAP_Find( self->allConsts, self->string )->value.pInt;
}

static void DaoParser_PushItemType( DaoParser *self, DaoType *type, int id, uchar_t sep1 )
{
	if( type && type->nested && type->nested->size ){
		DaoType *itp = NULL;
		switch( type->tid ){
		case DAO_ARRAY :
			if( sep1 == DTOK_COLON && id == 0 ){
				itp = type->nested->items.pType[0];
			}
			break;
		case DAO_LIST : // XXX
			if( sep1 == DTOK_COLON && id == 0 ){
				itp = type->nested->items.pType[0];
			}else{
				itp = type->nested->items.pType[0];
			}
			break;
		case DAO_MAP :
			if( type->nested->size > 1 ) itp = type->nested->items.pType[id%2];
			break;
		case DAO_TUPLE :
			itp = type->nested->items.pType[id];
			break;
		default : break;
		}
		DList_PushFront( self->enumTypes, itp );
	}else{
		DList_PushFront( self->enumTypes, NULL );
	}
}
static DaoValue* DaoParseNumber( DaoParser *self, DaoToken *tok, DaoValue *value )
{
	char *str = tok->string.chars;
	daoint pl = 0;
	if( tok->name >= DTOK_NUMBER_DEC && tok->name <= DTOK_NUMBER_SCI ){
		value->type = DAO_FLOAT;
		value->xFloat.value = DaoToken_ToFloat( tok );
	}else if( tok->name == DTOK_NUMBER_IMG ){
		value->type = DAO_COMPLEX;
		value->xComplex.value.real = 0;
		value->xComplex.value.imag = DaoToken_ToFloat( tok );
	}else{
		value->type = DAO_INTEGER;
		value->xInteger.value = DaoToken_ToInteger( tok );
	}
	return value;
}
static int DaoParser_ParseSymbol( DaoParser *self, DString *symbol )
{
	DNode *node;
	DaoNamespace *ns = self->nameSpace;
	DaoRoutine *routine = self->routine;
	DaoType *type = DaoNamespace_MakeSymbolType( ns, symbol->chars );
	if( (node = MAP_Find( self->allConsts, symbol )) == NULL ){
		self->denum->value = 0;
		DaoEnum_SetType( self->denum, type );
		node = MAP_Insert( self->allConsts, symbol, routine->routConsts->value->size );
		DaoRoutine_AddConstant( routine, (DaoValue*) self->denum );
	}
	return LOOKUP_BIND_LC( node->value.pInt );
}
static int DaoParser_ParseAtomicExpression( DaoParser *self, int start, int *cst )
{
	DNode *node;
	DaoValue buffer = {0};
	DaoToken **tokens = self->tokens->items.pToken;
	DaoNamespace *ns = self->nameSpace;
	DaoRoutine *routine = self->routine;
	DaoValue *value = NULL;
	DString *str = & tokens[start]->string;
	int varReg, exp = tokens[start]->name == DTOK_IDENTIFIER;
	char *tok = tokens[start]->string.chars;
	unsigned char tki = 0;

	/*printf("DaoParser_ParseAtomicExpression()\n"); */

	memset( & buffer, 0, sizeof(DaoValue) );

	*cst = 0;
	varReg = DaoParser_GetRegister( self, tokens[start] );
	tki = tokens[start]->name;


	/*
	   printf("name=%s; %i\n", tok, varReg);
	 */
	if( varReg >= 0 ){
		int st = LOOKUP_ST( varReg );
		if( LOOKUP_ISCST( varReg ) ){
			*cst = varReg;
			value = DaoParser_GetVariable( self, varReg );
		}
		if( self->byteBlock && (st & DAO_LOCAL_CONSTANT) ){
			int opcode = 0;
			DaoByteBlock *eval, *block = self->byteBlock;
			switch( st ){
			case DAO_LOCAL_CONSTANT: opcode = DVM_GETCL; break;
			case DAO_CLASS_CONSTANT: opcode = DVM_GETCK; break;
			case DAO_GLOBAL_CONSTANT: opcode = DVM_GETCG; break;
			}
			if( value && value->type >= DAO_ENUM ){
				if( DaoByteBlock_FindObjectBlock( block, value ) == NULL ){
					DaoByteBlock *name = DaoByteBlock_EncodeString( block, str );
					eval = DaoByteBlock_AddEvalBlock( block, value, opcode, 1, 0, NULL );
					DaoByteBlock_InsertBlockIndex( eval, eval->end, name );
				}
			}
		}
		/*
		   printf("value = %i; %i; c : %i\n", value->type, varReg, *cst );
		 */
	}else if( tki == DTOK_MBS || tki == DTOK_WCS || tki == DTOK_VERBATIM ){
		if( (node = MAP_Find( self->allConsts, str )) == NULL ){
			DaoString dummy = {DAO_STRING,0,0,0,0,NULL};
			int wcs = tok[0] == '"';
			dummy.value = self->str;
			if( tki == DTOK_VERBATIM ){
				daoint pos = DString_FindChar( str, ']', 1 );
				DString_SetBytes( self->str, tok + pos + 1, str->size - 2*(pos + 1) );
				wcs = tok[1] == '@';
			}else{
				DString_SetBytes( self->str, tok + 1, str->size-2 );
			}
			node = MAP_Insert( self->allConsts, str, routine->routConsts->value->size );
			DaoRoutine_AddConstant( routine, (DaoValue*) & dummy );
		}
		varReg = LOOKUP_BIND_LC( node->value.pInt );
		*cst = varReg;
	}else if( tki >= DTOK_DIGITS_DEC && tki <= DTOK_NUMBER_IMG ){
		if( (node = MAP_Find( self->allConsts, str )) == NULL ){
			value = DaoParseNumber( self, tokens[start], & buffer );
			if( value == NULL ) return -1;
			node = MAP_Insert( self->allConsts, str, routine->routConsts->value->size );
			DaoRoutine_AddConstant( routine, value );
		}
		*cst = LOOKUP_BIND_LC( node->value.pInt );
		value = routine->routConsts->value->items.pValue[ node->value.pInt ];
		varReg = *cst;
	}else if( tki == DTOK_ID_SYMBOL ){
		varReg = DaoParser_ParseSymbol( self, str );
		*cst = varReg;
	}else{
		*cst = 0;
		DaoParser_Error( self, DAO_SYMBOL_NOT_DEFINED, str );
		return -1;
	}
	if( value && value->type == DAO_INTEGER && (value->xInteger.value >> 16) == 0 ){
		varReg = DaoParser_PushRegister( self );
		DaoParser_PushTokenIndices( self, start, 0, 0 );
		DaoParser_AddCode( self, DVM_DATA, DAO_INTEGER, value->xInteger.value, varReg );
	}else if( value && value->type == DAO_FLOAT && value->xFloat.value == 0.0 ){
		varReg = DaoParser_PushRegister( self );
		DaoParser_PushTokenIndices( self, start, 0, 0 );
		DaoParser_AddCode( self, DVM_DATA, DAO_FLOAT, 0, varReg );
	}
	return DaoParser_GetNormRegister( self, varReg, exp, start, 0, start );
}

static int DaoParser_ParseClosure( DaoParser *self, int start )
{
	char name[100];
	daoint offset, regCall, opc, rb = 0;
	daoint i, k, n, end = self->tokens->size-1;
	daoint tokPos = self->tokens->items.pToken[ start ]->line;
	DString *mbs = DaoParser_GetString( self );
	DaoNamespace *NS = self->nameSpace;
	DaoRoutine *routine = self->routine;
	DaoToken **tokens = self->tokens->items.pToken;
	DaoRoutine *rout;
	DaoParser *parser;
	DArray *uplocs;
	DNode *it;

	parser = DaoVmSpace_AcquireParser( self->vmSpace );
	rout = DaoRoutine_New( NS, NULL, 1 );
	parser->routine = rout;
	parser->levelBase = self->levelBase + self->lexLevel + 1;
	parser->nameSpace = self->nameSpace;
	parser->vmSpace = self->vmSpace;
	parser->outerParser = self;
	if( parser->uplocs == NULL ) parser->uplocs = DArray_New(sizeof(int));
	uplocs = parser->uplocs;
	DString_Assign( parser->fileName, self->fileName );
	if( self->hostClass ){
		GC_Assign( & rout->routHost, self->hostClass->objType );
	}
	DList_Append( NS->definedRoutines, rout );

	if( tokens[start]->name == DKEY_DEFER ){
		DaoType *type = NULL;
		int offset = start + 1;
		rout->attribs |= DAO_ROUT_DEFER;
		if( tokens[offset]->name == DTOK_LB ){
			int pos = 0, rb = DaoParser_FindPairToken( self, DTOK_LB, DTOK_RB, offset, end );
			if( rb != offset + 1 ){
				int compatible;
				type = DaoParser_ParseType( self, offset + 1, rb-1, & pos, NULL );
				if( type == NULL ) goto ErrorParsing;
				compatible = DaoType_MatchTo( type, dao_type_error, NULL );
				compatible |= type->tid == DAO_NONE || type->tid == DAO_ANY;
				if( compatible == 0 ) goto ErrorParsing;
				/* defer block that may consume exception objects may return value: */
				if( type->tid != DAO_NONE ) rout->attribs |= DAO_ROUT_DEFER_RET;
				offset = pos;
				if( tokens[offset]->name == DKEY_AS ){
					DString *name = & tokens[offset+1]->string;
					if( tokens[++offset]->name != DTOK_IDENTIFIER ){
						DaoParser_Error( self, DAO_TOKEN_EXPECTING, & tokens[offset]->string );
						goto ErrorParsing;
					}
					type = DaoNamespace_MakeType( NS, name->chars, DAO_PAR_NAMED, (DaoValue*)type, NULL, 0 );
					MAP_Insert( DaoParser_CurrentSymbolTable(parser), name, parser->regCount );
					offset += 1;
				}
				DaoRoutine_AddConstant( rout, NULL ); /* no default parameter; */
				DaoParser_PushRegister( parser );
				rout->parCount ++;
				if( offset != rb ) goto ErrorParsing;
			}else{
				offset += 1;
			}
			offset += 1;
		}
		type = DaoNamespace_MakeType( NS, "routine", DAO_ROUTINE, NULL, & type, type != NULL );
		GC_Assign( & rout->routType, type );
		rb = DaoParser_ExtractRoutineBody( self, parser, offset );
		if( rb < 0 ) goto ErrorParsing;
	}else if( tokens[start+1]->name == DTOK_LB ){
		rb = DaoParser_ParseSignature( self, parser, start );
	}else if( tokens[start+1]->name == DTOK_LCB ){
		DaoType *type = DaoType_New( "@X", DAO_THT, NULL, NULL );
		type = DaoType_New( "routine<=>@X>", DAO_ROUTINE, (DaoValue*)type, NULL);
		GC_Assign( & rout->routType, type );
		rb = DaoParser_ExtractRoutineBody( self, parser, start+1 );
		if( rb < 0 ) goto ErrorParsing;
	}else{
		goto ErrorParsing;
	}

	/* Routine name may have been changed by DaoParser_ParseSignature() */
	sprintf( name, "AnonymousFunction_%p", rout );
	DString_SetChars( rout->routName, name );
	if( self->byteBlock ){
		parser->byteCoder = self->byteCoder;
		parser->byteBlock = DaoByteBlock_AddRoutineBlock( self->byteBlock, parser->routine, 0 );
	}
	offset = rb - parser->tokens->size;
	if( rb < 0 || tokens[rb]->name != DTOK_RCB ){
		DaoParser_Error( self, DAO_CTW_INVA_SYNTAX, NULL );
		goto ErrorParsing;
	}
	if( ! DaoParser_ParseRoutine( parser ) ) goto ErrorParsing;

	regCall = self->regCount;
	DaoParser_PushRegister( self );
	i = DaoRoutine_AddConstant( routine, (DaoValue*)rout );
	DaoParser_PushTokenIndices( self, start, rb, end );
	DaoParser_AddCode( self, DVM_GETCL, 0, i, regCall );

	for(i=0; i<uplocs->size; i+=4 ){
		int up = uplocs->data.ints[i];
		int loc = uplocs->data.ints[i+1];
		int first = uplocs->data.ints[i+2] + offset;
		int last = uplocs->data.ints[i+3] + offset;
		DaoParser_PushTokenIndices( self, first, first, last );
		DaoParser_AddCode( self, DVM_MOVE, up, 0, regCall+1+i/2 );
		DaoParser_AddCode( self, DVM_DATA, DAO_INTEGER, loc, regCall+2+i/2 );
	}
	DaoParser_PushRegisters( self, uplocs->size/2 );

	self->curToken = rb + 1;
	opc = DaoParser_PushRegister( self );
	/* DVM_ROUTINE rout_proto, upv1, upv2, ..., opc */
	DaoParser_PushTokenIndices( self, start, rb, end );
	DaoParser_AddCode( self, DVM_ROUTINE, regCall, uplocs->size/2, opc );
	DaoVmSpace_ReleaseParser( self->vmSpace, parser );
	return opc;
ErrorParsing:
	if( rout->attribs & DAO_ROUT_DEFER ){
		DString_SetChars( mbs, "invalid defer block" );
	}else{
		DString_SetChars( mbs, "invalid anonymous function" );
	}
	DaoParser_Error( self, DAO_CTW_INVA_SYNTAX, mbs );
	DaoVmSpace_ReleaseParser( self->vmSpace, parser );
	GC_IncRC( rout );
	GC_DecRC( rout );
	return -1;
}

int DaoParser_MakeArithTree( DaoParser *self, int start, int end, int *cst )
{
	int i, reg, stop = 0;
	DaoEnode enode;
	DaoToken **tokens = self->tokens->items.pToken;
#if 0
	printf("MakeArithTree(): start = %i; end = %i;\n", start, end );
	for( i=start;i<=end;i++) printf("%s  ", tokens[i]->string->chars); printf("\n");
#endif
	if( (end+1) < self->tokens->size ){
		i = tokens[end+1]->name;
		if( i == DTOK_COLON || i == DTOK_DOTS ) stop = i;
	}
	self->curToken = start;
	enode = DaoParser_ParseExpression( self, stop );
	reg = enode.reg;
	*cst = enode.konst;
	if( self->curToken != end+1 ) reg = -1;
	if( reg < 0 ) DaoParser_Error2( self, DAO_INVALID_EXPRESSION, start, end, 0 );
	return reg;
}

static int DaoToken_LineSpan( DaoToken *self )
{
	int i, span = 1;
	if( self->type < DTOK_VERBATIM || self->type > DTOK_WCS ) return 1;
	for(i=0; i<self->string.size; ++i) span += self->string.chars[i] == '\n';
	return span;
}
int DaoParser_GetOperPrecedence( DaoParser *self )
{
	DOper oper;
	DaoToken **tokens = self->tokens->items.pToken;
	if( self->curToken < self->tokens->size && tokens[self->curToken]->type == DTOK_SPACE ){
		self->curToken += 1;
	}
	if( self->curToken >= self->tokens->size ) return -1;
	if( (self->curToken+1) < self->tokens->size ){
		DaoToken *t1 = tokens[self->curToken];
		DaoToken *t2 = tokens[self->curToken+1];
		if( t1->line == t2->line && (t1->cpos+1) == t2->cpos ){
			/* check for operators: <<, >>, <=, >= */
			int newtok = 0;
			switch( ((int)t1->type<<8)|t2->type ){
			case (DTOK_LT<<8)|DTOK_LT : newtok = DTOK_LSHIFT; break;
			case (DTOK_GT<<8)|DTOK_GT : newtok = DTOK_RSHIFT; break;
			case (DTOK_LT<<8)|DTOK_ASSN : newtok = DTOK_LE; break;
			case (DTOK_GT<<8)|DTOK_ASSN : newtok = DTOK_GE; break;
			}
			if( newtok ){
				DString_Insert( & t2->string, & t1->string, 0, 0, 1 );
				t1->string.chars[0] = '\0';
				t1->string.size = 0;
				t1->type = t1->name = DTOK_SPACE;
				t2->type = t2->name = newtok;
				t2->cpos = t1->cpos;
				self->curToken += 1;
			}
		}else if( t1->name == DKEY_NOT && t2->name == DKEY_IN ){
			DString_AppendChar( & t1->string, ' ' );
			DString_Insert( & t2->string, & t1->string, 0, 0, 4 );
			t1->string.chars[0] = '\0';
			t1->string.size = 0;
			t1->type = t1->name = DTOK_SPACE;
			t2->type = t2->name = DTOK_NOTIN;
			t2->cpos = t1->cpos;
			self->curToken += 1;
		}
	}
	if( self->curToken > 0 ){
		DaoToken *t1 = tokens[self->curToken-1];
		DaoToken *t2 = tokens[self->curToken];
		int span = DaoToken_LineSpan( t1 );
		if( t1->line != t2->line && (t1->line + span - 1) != t2->line ) return -1;
	}
	oper = daoArithOper[tokens[self->curToken]->name];
	if( oper.oper == 0 || oper.binary == 0 ) return -1;
	return 10*(20 - oper.binary);
}
static DaoInode* DaoParser_InsertCode( DaoParser *self, DaoInode *after, int code, int a, int b, int c )
{
	DaoInode *node = DaoParser_NewNode( self );
	node->code = code;
	node->a = a;
	node->b = b;
	node->c = c;
	node->first = self->firstToken;
	node->last = self->lastToken;
	node->line = DaoParser_GetTokenLine( self, self->firstToken );
	node->jumpTrue = node->jumpFalse = NULL;
	node->prev = after;
	node->next = after->next;
	if( after->next ) after->next->prev = node;
	after->next = node;
	if( self->vmcLast->next ) self->vmcLast = node;
	return node;
}
static DaoInode* DaoParser_AddBinaryCode( DaoParser *self, int code, DaoEnode *LHS, DaoEnode *RHS, int mid )
{
	int opa = LHS->reg;
	int opb = RHS->reg;
	int first = mid - 1;
	int last = mid + 1;
	int regc = DaoParser_PushRegister( self );
	if( LHS->first ) first = LHS->first->first;
	if( RHS->last ) last = RHS->last->first + RHS->last->last;
	if( code < 0 ){
		code = -code;
		if( code != DVM_IN ){
			int ca = LHS->konst;
			int cb = RHS->konst;
			LHS->konst = cb;
			RHS->konst = ca;
			opa = RHS->reg;
			opb = LHS->reg;
		}
	}
	DaoParser_PushTokenIndices( self, first, mid, last );
	DaoParser_AddCode( self, code, opa, opb, regc );
	return self->vmcLast;
}
/* list, map, matrix, tuple expressions: */
/* { a=>1, b=>[] }; {1,2,10}; {1:2:10}; {1:10}; [1,2,10]; [1:2:10]; [1:10] */
DaoEnode DaoParser_ParseEnumeration( DaoParser *self, int etype, int btype, int lb, int rb )
{
	DList *cid = DaoParser_GetArray( self );
	DaoEnode enode, result = { -1, 0, 1, 0, NULL, NULL, NULL, NULL };
	DaoInode *back = self->vmcLast;
	DaoType *tp = self->enumTypes->size ? self->enumTypes->items.pType[0] : 0;
	int start = lb - 1 - (etype != 0);
	int mid = lb, end = rb + 1;
	int regcount = self->regCount;
	int enumcode = DVM_LIST;
	int pto = DaoParser_FindOpenToken( self, DTOK_FIELD, lb, rb, 0 );
	int arrow = DaoParser_FindOpenToken( self, DTOK_ARROW, lb, rb, 0 );
	int colon = DaoParser_FindOpenToken( self, DTOK_COLON, lb, rb, 0 );
	int semi = DaoParser_FindOpenToken( self, DTOK_SEMCO, lb, rb, 0 );
	int comma = DaoParser_FindOpenToken( self, DTOK_COMMA, lb, rb, 0 );
	int isempty = 0, step = 0;
	int regC;

	if( tp && (tp->tid & DAO_ANY) ) tp = NULL;

	if( btype == DTOK_LSB ) enumcode = DVM_VECTOR;
	if( etype == DKEY_ARRAY ) enumcode = DVM_VECTOR;
	result.prev = self->vmcLast;
	enode = result;

#ifndef DAO_WITH_NUMARRAY
	if( enumcode == DVM_VECTOR ){
		DaoParser_Error( self, DAO_DISABLED_NUMARRAY, NULL );
		goto ParsingError;
	}
#endif

	self->curToken = lb;
	DaoParser_PushTokenIndices( self, start, mid, end );
	if( etype == DKEY_TUPLE || btype == DTOK_LB ){
		/* ( a, b ) */
		if( tp && tp->tid != DAO_TUPLE ) goto ParsingError;
		enode = DaoParser_ParseExpressionList2( self, DTOK_COMMA, NULL, cid, DAO_EXPRLIST_TUPLE );
		if( enode.reg < 0 || self->curToken != end ) goto ParsingError;
		regC = DaoParser_PushRegister( self );
		enumcode = DVM_TUPLE;
		DaoParser_AddCode( self, DVM_TUPLE, enode.reg, enode.count, regC );
	}else if( etype == DKEY_MAP || (etype == 0 && btype == DTOK_LCB && (pto >= 0 || arrow >= 0) ) ){
		/* { a=>1, b=>[] }; {=>}; */
		/* { a->1, b->[] }; {->}; */
		int enummode = pto >= 0 ? DVM_ENUM_MODE0 : DVM_ENUM_MODE1;
		if( etype == DKEY_MAP && arrow < 0 ) enummode = DVM_ENUM_MODE0;
		if( tp && tp->tid != DAO_MAP ) goto ParsingError;
		isempty = lb >= rb;
		if( lb >= rb ){
			if( self->needConst ){
				DaoMap *hm = DaoMap_New(colon>=0);
				hm->ctype = tp ? tp : dao_type_map_any;
				GC_IncRC( hm->ctype );
				regC = DaoRoutine_AddConstant( self->routine, (DaoValue*) hm );
				enode.konst = LOOKUP_BIND_LC( regC );
				enode.count = 0;
				regC = DaoParser_GetNormRegister( self, enode.konst, 0, start, 0, end );
			}else{
				regC = DaoParser_PushRegister( self );
				DaoParser_AddCode( self, DVM_MAP, regC, (enummode<<14)|0, regC );
			}
		}else{
			int sep = pto >= 0 ? DTOK_FIELD : DTOK_ARROW;
			step = 2;
			enode = DaoParser_ParseExpressionLists( self, sep, DTOK_COMMA, & step, cid );
			if( enode.reg < 0 || self->curToken != end ) goto ParsingError;
			regC = DaoParser_PushRegister( self );
			DaoParser_AddCode( self, DVM_MAP, enode.reg, (enummode<<14)|enode.count, regC );
		}
	}else if( colon > lb && comma < 0 && semi < 0 ){
		/* arithmetic progression: [ 1 : 2 : 10 ]; [ 1 : 10 ] */
		if( tp && (enumcode == DVM_LIST && tp->tid != DAO_LIST) ) goto ParsingError;
		if( tp && (enumcode == DVM_VECTOR && tp->tid != DAO_ARRAY) ) goto ParsingError;
		enode = DaoParser_ParseExpressionList2( self, DTOK_COLON, NULL, cid, enumcode == DVM_VECTOR ? DAO_EXPRLIST_ARRAY : 0 );
		if( enode.reg < 0 || self->curToken != end ) goto ParsingError;
		isempty = lb > rb;
		if( enode.reg < 0 || enode.count < 2 || enode.count > 3 ){
			DaoParser_Error( self, DAO_CTW_ENUM_INVALID, NULL );
			goto ParsingError;
		}
		regC = DaoParser_PushRegister( self );
		DaoParser_AddCode( self, enumcode, enode.reg, (DVM_ENUM_MODE1<<14)|enode.count, regC );
	}else if( semi < 0 ){
		/* [a,b,c] */
		if( tp && (enumcode == DVM_LIST && tp->tid != DAO_LIST) ) goto ParsingError;
		if( tp && (enumcode == DVM_VECTOR && tp->tid != DAO_ARRAY) ) goto ParsingError;
		enode = DaoParser_ParseExpressionList2( self, DTOK_COMMA, NULL, cid, enumcode == DVM_VECTOR ? DAO_EXPRLIST_ARRAY : 0 );
		if( enode.reg < 0 || self->curToken != end ) goto ParsingError;
		isempty = lb > rb;
		regC = DaoParser_PushRegister( self );
		DaoParser_AddCode( self, enumcode, enode.reg, enode.count, regC );
	}else if( etype == DKEY_ARRAY || (etype == 0 && btype == DTOK_LSB) ){
		/* [1,2; 3,4] */
		int row = 0, col = 0;
		if( tp && (enumcode == DVM_VECTOR && tp->tid != DAO_ARRAY) ) goto ParsingError;
		enumcode = DVM_MATRIX;
		isempty = lb > rb;
		enode = DaoParser_ParseExpressionLists( self, DTOK_COMMA, DTOK_SEMCO, & step, cid );
		if( enode.reg < 0 || self->curToken != end ) goto ParsingError;
		col = step;
		if( enode.count && col ) row = enode.count / col;
		if( row >= 255 || col >= 255 ){
			DaoParser_Error( self, DAO_CTW_ENUM_LIMIT, NULL );
			goto ParsingError;
		}
		regC = DaoParser_PushRegister( self );
		DaoParser_AddCode( self, DVM_MATRIX, enode.reg, ((row<<8)|col), regC );
	}else{
		regC = -1;
	}
	if( regC < 0 ){
		DaoParser_Error( self, DAO_CTW_ENUM_INVALID, NULL );
		goto ParsingError;
	}
	if( colon >= 0 && btype != DTOK_LCB ) enode.konst = 0;
	if( self->needConst && enode.konst == enode.count ){
		regC = DaoParser_MakeEnumConst( self, & enode, cid, regcount );
	}else if( enode.count ){
		enode.konst = 0;
	}
	if( self->enumTypes->size ){
		tp = self->enumTypes->items.pType[0];
		if( tp && tp->tid != DAO_ANY ) MAP_Insert( self->routine->body->localVarType, regC, tp );
	}
	result.reg = regC;
	result.konst = enode.konst;
	result.first = result.last = result.update = self->vmcLast;
	self->vmcValue = self->vmcLast;
	if( back->next ) result.first = back->next;
	return result;
ParsingError:
	DaoParser_Error2( self, DAO_INVALID_EXPRESSION, self->curToken, end, 0 );
	return result;
}
static DaoEnode DaoParser_ParseParenthesis( DaoParser *self )
{
	DaoEnode enode, result = { -1, 0, 1, 0, NULL, NULL, NULL, NULL };
	DaoToken **tokens = self->tokens->items.pToken;
	DaoInode *back = self->vmcLast;
	int start = self->curToken;
	int end = self->tokens->size-1;
	int rb = DaoParser_FindPairToken( self, DTOK_LB, DTOK_RB, start, end );
	int comma = DaoParser_FindOpenToken( self, DTOK_COMMA, start+1, end, 0 );
	int maybeType = tokens[start+1]->type == DTOK_IDENTIFIER;
	int regC;

	if( rb > 0 && rb < end && tokens[rb]->line == tokens[rb+1]->line ){
		DOper oper = daoArithOper[ tokens[rb+1]->type ];
		if( oper.left != 0 && oper.binary == 0 ) goto ParsingError;
	}

	result.prev = self->vmcLast;
	if( rb > 0 && rb < end && maybeType && tokens[rb]->line == tokens[rb+1]->line ){
		int cur, count = self->errors->size;
		self->curToken = rb + 1;
		/* To skip the explicit enum type, which is for the entire casting expression: */
		DList_PushFront( self->enumTypes, NULL );
		enode = DaoParser_ParsePrimary( self, 0, 0 );
		DList_PopFront( self->enumTypes );
		cur = self->curToken;
		if( enode.reg >= 0 ){
			/* type casting expression */
			int it, newpos = 0;
			DaoType *abtp = DaoParser_ParseType( self, start+1, rb-1, & newpos, NULL );
			self->curToken = cur;
			if( abtp == NULL || newpos != rb ){
				GC_IncRC( abtp );
				GC_DecRC( abtp );
				goto ParseNoCasting;
			}
			regC = DaoParser_PushRegister( self );
			it = DaoRoutine_AddConstant( self->routine, (DaoValue*) abtp );
			DaoParser_PushTokenIndices( self, start, rb, self->curToken-1 );
			DaoParser_AddCode( self, DVM_CAST, enode.reg, it, regC );
			result.reg = regC;
			result.first = back->next;
			result.last = result.update = self->vmcLast;
			return result;
		}
ParseNoCasting:
		DList_Erase( self->errors, count, -1 );
	}
	self->curToken = start + 1;
	if( rb >=0 && comma >= 0 && comma < rb ){
		/* tuple enumeration expression */
		result = DaoParser_ParseEnumeration( self, 0, DTOK_LB, start+1, rb-1 );
		self->curToken = rb + 1;
		return result;
	}
	result = DaoParser_ParseExpression( self, 0 );
	if( result.reg < 0 ) return result;
	if( self->curToken < rb ) goto ParsingError;
	self->curToken += 1;
	return result;
ParsingError:
	self->curToken = start;
	DaoParser_Error3( self, DAO_INVALID_EXPRESSION, start );
	result.reg = -1;
	return result;
}

static DaoEnode DaoParser_ParseIntrinsicMath( DaoParser *self, int tki, int start, int end )
{
	DaoInode *last = self->vmcLast;
	DaoEnode enode, result = { -1, 0, 1, 0, NULL, NULL, NULL, NULL };
	DaoEnode error = { -1, 0, 1, 0, NULL, NULL, NULL, NULL };
	int rb = DaoParser_FindPairToken( self, DTOK_LB, DTOK_RB, start, end );
	int reg, regLast = -1, cst = 0;

	self->curToken = start;
	DaoParser_PushTokenIndices( self, start, start+1, rb );
	if( rb < 0 || rb == start+2 ){
		if( rb == start+2 ) DaoParser_Error( self, DAO_PARAM_INVALID, NULL );
		return error;
	}
	reg = DaoParser_MakeArithTree( self, start+2, rb-1, &cst );
	if( reg <0 ) return error;
	if( cst ){
		DaoProcess *proc;
		DaoVmCode vmc = { DVM_MATH, 0, 1, 0 };
		DaoValue *value;

		vmc.a = tki - DKEY_CEIL;
		proc = DaoNamespace_ReserveFoldingOperands( self->nameSpace, 2 );
		DaoValue_Copy( DaoParser_GetVariable( self, cst ), & proc->activeValues[1] );
		proc->activeCode = & vmc;
		value = DaoParser_EvalConst( self, proc, 2 );
		if( value == NULL ) return error;
		result.konst = LOOKUP_BIND_LC( DaoRoutine_AddConstant( self->routine, value ));
		regLast = DaoParser_GetNormRegister( self, result.konst, 0, start, 0, rb );
	}else{
		regLast = DaoParser_PushRegister( self );
		DaoParser_AddCode( self, DVM_MATH, tki - DKEY_CEIL, reg, regLast );
	}
	result.reg = regLast;
	result.first = last->next;
	result.last = result.update = self->vmcLast;
	self->curToken = rb + 1;
	return result;
}

static DaoEnode DaoParser_ParseSelfGetField( DaoParser *self )
{
	DaoValue *data;
	DaoClass *klass = self->hostClass;
	DaoEnode enode, result = { -1, 0, 1, 0, NULL, NULL, NULL, NULL };
	DaoEnode error = { -1, 0, 1, 0, NULL, NULL, NULL, NULL };
	DaoToken **tokens = self->tokens->items.pToken;
	DString *field = & tokens[ self->curToken+2]->string;
	int start = self->curToken;
	int opa, opb, opc, code = 0;
	int index = 0;

	if( self->hostClass == NULL ) goto SelfNotDefined;
	if( self->routine->attribs & DAO_ROUT_STATIC ) goto SelfInStaticError;

	data = DaoClass_GetData( klass, field, klass );
	if( data != NULL ){
		index = DaoClass_GetDataIndex( klass, field );

		opa = LOOKUP_UP( index );
		opb = LOOKUP_ID( index );
		opc = DaoParser_PushRegister( self );
		DaoParser_PushTokenIndices( self, start, start, start );
		switch( LOOKUP_ST( index ) ){
		case DAO_OBJECT_VARIABLE : code = DVM_GETVO; break;
		case DAO_CLASS_VARIABLE  : code = DVM_GETVK; break;
		case DAO_CLASS_CONSTANT  : code = DVM_GETCK; break;
		}
		DaoParser_AddCode( self, code, opa, opb, opc );
		result.reg = opc;
		result.last = result.update = self->vmcLast;
	}else{
		DString *name = self->string;
		DString_SetChars( name, "." );
		DString_Append( name, field );
		data = DaoClass_GetData( klass, name, klass );
		if( data == NULL ){
			DString_SetChars( name, "." );
			data = DaoClass_GetData( klass, name, klass );
		}
		if( data == NULL ) goto FieldNotExistError;

		opa = DaoParser_PushRegister( self );
		DaoParser_PushTokenIndices( self, start, start, start );
		DaoParser_AddCode( self, DVM_GETVO, 0, 0, opa );

		opc = DaoParser_PushRegister( self );
		opb = DaoParser_AddFieldConst( self, field );
		DaoParser_PushTokenIndices( self, start, start+1, start+2 );
		DaoParser_AddCode( self, DVM_GETF, opa, opb, opc );

		result.reg = opc;
		result.last = result.update = self->vmcLast;
		DaoParser_PushTokenIndices( self, start, start+1, start+2 );
		DaoParser_AddCode( self, DVM_LOAD2, opa, 0, 0 );
	}
	return result;

SelfNotDefined:
	DaoParser_Error( self, DAO_SYMBOL_NOT_DEFINED, & tokens[self->curToken]->string );
	return error;
SelfInStaticError:
	DaoParser_Error( self, DAO_INVALID_SELF_IN_STATIC, NULL );
	return error;
FieldNotExistError:
	DaoParser_Error( self, DAO_FIELD_NOT_EXIST, NULL );
	return error;
}

static DaoEnode DaoParser_ParsePrimary( DaoParser *self, int stop, int eltype )
{
	DString *name;
	DaoValue *dbase;
	DaoValue *value = NULL;
	DaoInode *last = self->vmcLast;
	DaoEnode enode, result = { -1, 0, 1, 0, NULL, NULL, NULL, NULL };
	DaoEnode error = { -1, 0, 1, 0, NULL, NULL, NULL, NULL };
	DaoRoutine *routine = self->routine;
	DaoToken **tokens = self->tokens->items.pToken;
	DaoString daostr = {DAO_STRING,0,0,0,1,NULL};
	DaoValue *svalue = (DaoValue*) & daostr;
	DString *mbs = self->string;
	unsigned char tkn, tki, tki2, tki3 = 0;
	int regcount = self->regCount;
	int size = self->tokens->size;
	int start = self->curToken;
	int end = size - 1;
	int regLast = -1;
	int reg, rb, cst = 0;

	/*
	   int i; for(i=start;i<=end;i++) printf("%s  ", tokens[i]->string.chars);printf("\n");
	 */
	result.prev = self->vmcLast;
	if( start >= size ) return result;
	tkn = tokens[start]->type;
	tki = tokens[start]->name;
	tki2 = DaoParser_NextTokenName( self );
	if( tki2 == stop ) tki2 = 0;
	if( (start + 2) <= end ) tki3 = tokens[start+2]->type;
	if( (tki == DTOK_IDENTIFIER || tki >= DKEY_CEIL) && tki2 == DTOK_ASSN && eltype == DAO_EXPRLIST_TUPLE ){
		DaoType *type = self->enumTypes->size ? self->enumTypes->items.pType[0] : NULL;
		DaoString ds = {DAO_STRING,0,0,0,1,NULL};
		DaoValue *value = (DaoValue*) & ds;
		DString *field = & tokens[start]->string;

		ds.value = field;
		self->curToken += 2;
		if( type && type->tid == DAO_PAR_NAMED ) type = (DaoType*) type->aux;
		if( type ) DList_PushFront( self->enumTypes, type );
		enode = DaoParser_ParseExpression( self, stop );
		DList_PopFront( self->enumTypes );
		if( enode.reg < 0 ) return enode;
		if( enode.konst ){
			DaoValue *v2 = DaoParser_GetVariable( self, enode.konst );
			result.reg = DaoParser_MakeArithConst( self, DVM_NAMEVA, value, v2, & result.konst, last, regcount );
			if( result.reg < 0 ){
				DaoParser_Error( self, DAO_CTW_INV_CONST_EXPR, NULL );
				return error;
			}
		}else{
			reg = DaoRoutine_AddConstant( routine, value );
			result.reg = DaoParser_PushRegister( self );
			DaoParser_PushTokenIndices( self, start, start+1, self->curToken-1 );
			DaoParser_AddCode( self, DVM_NAMEVA, reg, enode.reg, result.reg );
		}
		result.last = result.update = self->vmcLast;
		result.first = result.last;
		return result;
	}else if( (tki == DTOK_IDENTIFIER || tki >= DKEY_CEIL) && tki2 == DTOK_ASSN && eltype == DAO_EXPRLIST_PARAM ){
		DString *symbol = DaoParser_GetString( self );

		DString_AppendChar( symbol, '$' );
		DString_Append( symbol, & tokens[start]->string );
		reg = DaoParser_ParseSymbol( self, symbol );

		self->curToken += 2;
		enode = DaoParser_ParseExpression( self, stop );
		if( enode.reg < 0 ) return enode;
		DaoParser_PushTokenIndices( self, start, start+1, self->curToken-1 );
		if( enode.konst ){
			int end = self->curToken - 1;
			DaoVmCode vmcValue = { DVM_TUPLE, 1, 2, 0 };
			DaoProcess *proc = DaoNamespace_ReserveFoldingOperands( self->nameSpace, 3 );
			DaoValue *symvalue = DaoParser_GetVariable( self, reg );
			DaoValue *valvalue = DaoParser_GetVariable( self, enode.konst );
			DaoValue *value;

			DaoValue_Copy( symvalue, & proc->activeValues[1] );
			DaoValue_Copy( valvalue, & proc->activeValues[2] );
			/* Execute the instruction to get the const result: */
			GC_DecRC( proc->activeTypes[0] );
			proc->activeTypes[0] = NULL;
			proc->activeCode = & vmcValue;
			value = DaoParser_EvalConst( self, proc, 2 );
			if( value == NULL ){
				DaoParser_Error( self, DAO_CTW_INV_CONST_EXPR, NULL );
				return error;
			}
			result.konst = LOOKUP_BIND_LC( DaoRoutine_AddConstant( self->routine, value ));
			result.reg = DaoParser_GetNormRegister( self, result.konst, 0, start, 1, end );
		}else{
			int reg2, end = self->curToken - 1;
			reg = DaoParser_GetNormRegister( self, reg, 0, start, 0, start );
			reg2 = DaoParser_PushRegisters( self, 3 );
			DaoParser_AddCode( self, DVM_MOVE, reg, 0, reg2 );
			DaoParser_AddCode( self, DVM_MOVE, enode.reg, 0, reg2+1 );
			DaoParser_AddCode( self, DVM_TUPLE, reg2, 2, reg2+2 );
			result.reg = reg2 + 2;
		}
		result.last = result.update = self->vmcLast;
		result.first = result.last;
		return result;
	}else if( tki == DKEY_SELF && tki2 == DTOK_DOT ){
		if( tki3 != DTOK_IDENTIFIER && tki3 < DKEY_CEIL ){
			DaoParser_Error( self, DAO_INVALID_EXPRESSION, NULL );
			return error;
		}
		result = DaoParser_ParseSelfGetField( self );
		start += 3;
	}else if( tki == DKEY_TYPE && tki2 == DTOK_LB ){
		int start0 = start + 2;
		DaoType *type = DaoParser_ParseType( self, start + 2, end, & start, NULL );
		if( type == NULL || start > end || tokens[start]->type != DTOK_RB ){
			DaoParser_Error3( self, DAO_INVALID_TYPE_FORM, start0 );
			return error;
		}
		result.konst = LOOKUP_BIND_LC( DaoRoutine_AddConstant( routine, (DaoValue*)type ) );
		result.reg = DaoParser_GetNormRegister( self, result.konst, 0, start, 0, self->curToken );
		result.first = last->next;
		result.last = result.update = self->vmcLast;
		start += 1;
	}else if( tki >= DKEY_ARRAY && tki <= DKEY_TUPLE && tki2 == DTOK_LCB ){
		int rb = DaoParser_FindPairToken( self, DTOK_LCB, DTOK_RCB, start, end );
		if( rb < 0 ) return result;
		result = DaoParser_ParseEnumeration( self, tki, DTOK_LCB, start+2, rb-1 );
		start = rb + 1;
	}else if( tki == DTOK_LCB ){
		int rb = DaoParser_FindPairToken( self, DTOK_LCB, DTOK_RCB, start, end );
		if( rb < 0 ) return result;
		result = DaoParser_ParseEnumeration( self, 0, tki, start+1, rb-1 );
		start = rb + 1;
	}else if( tki == DTOK_LSB ){
		int rb = DaoParser_FindPairToken( self, DTOK_LSB, DTOK_RSB, start, end );
		if( rb < 0 ) return result;
		result = DaoParser_ParseEnumeration( self, 0, tki, start+1, rb-1 );
		start = rb + 1;
	}else if( tki == DTOK_LB ){
		result = DaoParser_ParseParenthesis( self );
		start = self->curToken;
	}else if( tki == DKEY_ROUTINE && self->isClassBody == 0 && self->isInterBody == 0
			&& self->isCinTypeBody == 0 ){
		int tokname = DaoParser_NextTokenName( self );
		/* anonymous function or closure expression */
		self->curToken += 1;
		if( tokname != DTOK_LB && tokname != DTOK_LCB ){
			DaoParser_Error( self, DAO_CTW_INVA_SYNTAX, self->string );
			return error;
		}
		result.reg = regLast = DaoParser_ParseClosure( self, start );
		result.first = last->next;
		result.last = result.update = self->vmcLast;
		start = self->curToken;
	}else if( tki == DKEY_YIELD ){
		int mode = 0;
		if( start+1 > end || tokens[start+1]->name != DTOK_LB ){
			DaoParser_Error( self, DAO_INVALID_EXPRESSION, NULL );
			return error;
		}
		self->curToken = start + 2;
		enode = DaoParser_ParseExpressionList( self, DTOK_COMMA, NULL, NULL );
		if( DaoParser_CurrentTokenName( self ) == DTOK_DOTS ){
			mode = DAO_CALL_EXPAR;
			self->curToken += 1;
		}
		if( enode.reg < 0 || DaoParser_CheckTokenType( self, DTOK_RB, ")" ) == 0 ) return error;
		if( enode.count > DAO_MAX_PARAM ){
			DaoParser_Error( self, DAO_CTW_LIMIT_PAR_NUM, NULL );
			return error;
		}
		rb = self->curToken;
		regLast = DaoParser_PushRegister( self );
		DaoParser_PushTokenIndices( self, start, start+1, rb );
		DaoParser_AddCode( self, DVM_YIELD, enode.reg, enode.count|mode, regLast );
		result.reg = regLast;
		result.first = last->next;
		result.last = result.update = self->vmcLast;
		start = rb + 1;
	}else if( tki == DTOK_ID_THTYPE && tki2 == DTOK_LB ){
		DaoToken tok = *tokens[start];
		regLast = DaoParser_GetRegister( self, & tok );
		if( regLast <0 ){
			DString_SetChars( self->string, tokens[start]->string.chars + 1 );
			tok.string = *self->string;
			regLast = DaoParser_GetRegister( self, & tok );
			if( regLast < 0 ){
				DaoParser_Error( self, DAO_SYMBOL_NOT_DEFINED, self->string );
				return error;
			}
		}
		if( LOOKUP_ISCST( regLast ) ) result.konst = regLast;
		result.reg = regLast = DaoParser_GetNormRegister( self, regLast, 0, start, 0, start );
		result.first = last->next;
		result.last = result.update = self->vmcLast;
		start += 1;
	}else if( (tki >= DTOK_IDENTIFIER && tki <= DTOK_WCS) || tki == DTOK_COLON
			|| (tki >= DKEY_ANY && tki <= DKEY_TUPLE) || tki >= DKEY_CEIL || tki == DKEY_SELF ){
		int count = self->errors->size;
		int cur = start;
		regLast = DaoParser_ParseAtomicExpression( self, start, & cst );
		if( last != self->vmcLast ) result.first = result.last = result.update = self->vmcLast;
		result.reg = regLast;
		result.konst = cst;
		value = regLast >= 0 && cst ? DaoParser_GetVariable( self, cst ) : NULL;
		if( regLast < 0 || value == (DaoValue*) self->vmSpace->daoNamespace ){
			cur = start;
			if( value == (DaoValue*) self->vmSpace->daoNamespace ){
				if( cur+2 < self->tokens->size && tokens[cur+1]->type == DTOK_COLON2 ) cur += 2;
			}
			tki = tokens[cur]->name;
			tki2 = cur + 1 < self->tokens->size ? tokens[cur+1]->name : 0;
			if( tki2 == DTOK_LB && (tki >= DKEY_CEIL && tki <= DKEY_TANH) ){
				DList_Erase( self->errors, count, -1 );
				result = DaoParser_ParseIntrinsicMath( self, tki, cur, end );
				start = self->curToken - 1;
			}
		}
		start += 1;
	}
	self->curToken = start;
	if( result.reg < 0 ) return result;
	while( self->curToken < self->tokens->size ){
		DList *cid;
		DaoInode *extra = self->vmcLast;
		DaoInode *back = self->vmcLast;
		DaoInode *getx = result.last;
		int curStart = self->curToken;
		int regcount = self->regCount;
		int postart = start - 1;
		tkn = DaoParser_CurrentTokenName( self );
		if( tokens[self->curToken]->line != tokens[self->curToken-1]->line ){
			if( tkn != DTOK_DOT && tkn != DTOK_COLON2 ) return result;
		}
		if( result.first == NULL && result.last ) result.first = result.last;
		if( result.last ){
			postart = result.last->first + result.last->middle;
			if( getx->code < DVM_GETVH || getx->code > DVM_GETF ) getx = NULL;
		}
		start = self->curToken;
		if( tkn == stop ) return result;
		switch( tkn ){
		case DTOK_LB :
			{
				int rb, rb2, mode = 0, konst = 0, code = DVM_CALL;
				DaoInode *inode;
				rb = rb2 = DaoParser_FindPairToken( self, DTOK_LB, DTOK_RB, start, end );
				if( rb < 0 ) return error;
				if( (rb+1) <= end && tokens[rb+1]->name == DTOK_ASSN ) return result;

				if( (rb+1) <= end ){
					if( tokens[rb+1]->name == DTOK_BANG2 ){
						mode |= DAO_CALL_ASYNC;
						rb += 1;
					}
				}
				inode = self->vmcLast;
				if( result.last && inode->code == DVM_LOAD2 ){ /* X.Y */
					DaoParser_PopRegister( self ); /* opc of GETF will be reallocated; */
					inode->code = DVM_LOAD;
					inode->b = 0;
					inode = inode->prev;
					code = DVM_MCALL;
					extra = back->prev;
				}else if( result.last &&  DaoVmCode_CheckPermutable( result.last->code ) ){
					inode = result.last;
					extra = back;
				}else if( result.last == NULL ){
					DaoParser_PushTokenIndices( self, start, start, start );
					DaoParser_AddCode( self, DVM_LOAD, regLast, 0, 0/*unset*/ );
					inode = self->vmcLast;
					extra = self->vmcLast;
				}
				/*
				// Marking the call to the decorated function decorators,
				// so that decoration can be done properly for constructors:
				 */
				if( routine->attribs & DAO_ROUT_DECORATOR ){
					int mv = inode->code == DVM_MOVE || inode->code == DVM_LOAD;
					int fp = (routine->attribs & DAO_ROUT_PARSELF) != 0;
					if( mv && inode->a == fp ) mode |= DAO_CALL_DECSUB;
				}
				self->curToken += 1;
				cid = DaoParser_GetArray( self );
				enode = DaoParser_ParseExpressionList2( self, DTOK_COMMA, inode, cid, DAO_EXPRLIST_PARAM );
				if( DaoParser_CurrentTokenName( self ) == DTOK_DOTS ){
					mode |= DAO_CALL_EXPAR;
					self->curToken += 1;
				}
				if( enode.reg < 0 || self->curToken != rb2 ) return error;
				if( enode.count > DAO_MAX_PARAM ){
					DaoParser_Error( self, DAO_CTW_LIMIT_PAR_NUM, NULL );
					return error;
				}
				regLast = DaoParser_PushRegister( self );
				DaoParser_PushTokenIndices( self, postart, start, rb );
				DaoParser_AddCode( self, code, enode.reg, (enode.count-1)|mode, regLast );
				if( self->needConst && result.konst && enode.konst == (enode.count-1) ){
					DaoRoutine *rout = (DaoRoutine*) DaoParser_GetVariable( self, result.konst );
					if( rout != NULL ){
						cid->items.pInt[0] = result.konst;
						enode.prev = extra ? extra->prev : back;
						/*
						// DaoProcess_DoCall() will check for the returned type,
						// if it is NULL, a none value will be returned instead.
						// Set dao_type_udf as the returned type to avoid this.
						//
						// Don't use dao_type_any, because the evaluation may call
						// DaoProcess_GetReturnType(), which need to find the
						// specialized return type.
						//
						// static s = state<int>()
						 */
						DList_PushFront( self->enumTypes, dao_type_udf );
						regLast = DaoParser_MakeEnumConst( self, & enode, cid, regcount );
						DList_PopFront( self->enumTypes );
						if( regLast >=0 ){
							result.first = self->vmcLast;
							konst = enode.konst;
						}
					}
				}
				result.konst = konst;
				result.reg = regLast;
				result.last = result.update = self->vmcLast;
				self->curToken = rb + 1;
				break;
			}
		case DTOK_LCB :
			{
				DMap *varSection;
				DaoToken *argname = NULL;
				DaoInode *jump, *label, *sect, *call;
				int isSection, opa = result.reg, opb = -1;
				int lb = start, regCount, regX = -1, regY = -1;
				int rb = DaoParser_FindPairToken( self, DTOK_LCB, DTOK_RCB, start, end );
				if( rb < 0 ) return error;

				self->curToken += 1;
				DaoParser_PushTokenIndices( self, postart, start, rb );
				if( result.last && back->code == DVM_LOAD2 ){ /* X.Y */
					DaoParser_PopRegister( self ); /* opc of GETF will be reallocated; */
					back->code = DVM_LOAD;
					back->b = 0;
					back->prev->c = DaoParser_PushRegister( self );
					back->c = DaoParser_PushRegister( self );
					regLast = DaoParser_PushRegister( self );
					DaoParser_AddCode( self, DVM_MCALL, back->prev->c, 1, regLast );
				}else if( back->code != DVM_CALL && back->code != DVM_MCALL ){
					regLast = DaoParser_PushRegister( self );
					DaoParser_AddCode( self, DVM_CALL, opa, 0, regLast );
				}
				call = self->vmcLast;
				call->b |= DAO_CALL_BLOCK;

				if( DaoParser_PushOuterRegOffset( self, start, rb ) == 0 )
					goto InvalidSection;

				DaoParser_PushTokenIndices( self, start, start, rb );
				jump = DaoParser_AddCode( self, DVM_GOTO, 0, 0, DVM_SECT );
				sect = DaoParser_AddCode( self, DVM_SECT, self->regCount, 0,0 );
				label = jump->jumpTrue = DaoParser_AddCode( self, DVM_LABEL, 0,0,0 );
				DaoParser_AddScope( self, DVM_LBRA, NULL );
				varSection = DaoParser_CurrentSymbolTable( self );
				start += 1;
				regCount = self->regCount;
				if( tokens[start]->name == DTOK_DOTS && (start+1) == rb ) goto YieldSection;
				if( tokens[start]->name == DTOK_LSB ){
					int j, i = start + 1;
					int rb2 = DaoParser_FindPairToken( self, DTOK_LSB, DTOK_RSB, start, rb );
					if( rb2 < 0 ) goto InvalidSection;
					while( i < rb2 ){
						if( tokens[i]->type == DTOK_DOTS ){
							int count = self->regCount - regCount;
							DaoParser_PushRegisters( self, DAO_MAX_PARAM - count );
							i += 1;
							break;
						}
						if( tokens[i]->type != DTOK_IDENTIFIER ) break;
						sect->c += 1;
						j = DaoParser_PushRegister( self );
						MAP_Insert( varSection, & tokens[i]->string, j );
						if( tokens[++i]->name != DTOK_COMMA ) break;
						i += 1;
					}
					if( tokens[i]->name == DKEY_AS ){
						if( tokens[i+1]->type != DTOK_IDENTIFIER ) goto InvalidSection;
						argname = tokens[i+1];
						i += 2;
					}
					if( i < rb2 ){
						/* Possible case: array<int>(5){ [] [1, 2, 3] } */
						DaoParser_PopRegisters( self, self->regCount - regCount );
					}else{
						start = rb2 + 1;
					}
				}
				if( start == lb + 1 ){
					DString X = DString_WrapChars( "X" );
					DString Y = DString_WrapChars( "Y" );
					regX = DaoParser_PushRegister( self );
					regY = DaoParser_PushRegister( self );
					MAP_Insert( varSection, & X, regX );
					MAP_Insert( varSection, & Y, regY );
					sect->c = 2;
				}
				sect->b = self->regCount - regCount;

				if( argname ){
					int i = start - 3;
					int reg = DaoParser_DeclareVariable( self, argname, 0, NULL );
					int mode = DVM_ENUM_MODE2 << 14;
					DaoParser_PushTokenIndices( self, i, i, i+1 );
					DaoParser_AddCode( self, DVM_TUPLE, regCount, mode|sect->b, reg );
				}

				back = self->vmcLast;
				regCount = self->regCount;
				isSection = self->isSection;
				self->isSection = 1;
				self->curToken = start;
				enode = DaoParser_ParseExpression2( self, 0, 0, ASSIGNMENT_ERROR );
				if( enode.reg >= 0 && self->curToken == rb ){
					DaoParser_PushTokenIndices( self, start+1, start+1, rb-1 );
					DaoParser_AddCode( self, DVM_RETURN, enode.reg, 1, DVM_SECT );
				}else{
					DaoType *oldret = self->returnType;
					self->curToken = start;
					DaoParser_Restore( self, back, regCount );
					DaoParser_Restore( self, back, regCount );
					self->returnType = NULL;
					if( DaoParser_ParseCodes( self, start, rb-1 ) == 0 ){
						self->returnType = oldret;
						goto InvalidSection;
					}
					self->returnType = oldret;
				}
				if( self->vmcLast->code == DVM_RETURN ){
					self->vmcLast->c = DVM_SECT;
				}else{
					int first = self->curToken;
					DaoParser_PushTokenIndices( self, first, first, rb );
					DaoParser_AddCode( self, DVM_RETURN, 0, 0, DVM_SECT );
				}
				self->isSection = isSection;
				if( regX >= 0 ){
					DaoInode *inode = sect->next;
					while( inode ){
						DaoVmCode operands = DaoVmCode_CheckOperands( (DaoVmCode*) inode );
						ushort_t *bools = & operands.a;
						ushort_t *regs = & inode->a;
						int k;

						inode = inode->next;
						for(k=0; k<3; ++k){
							if( bools[k] && regs[k] == regX ) regX = -1;
							if( bools[k] && regs[k] == regY ) regY = -1;
						}
						if( regX < 0 && regY < 0 ) break;
					}
					if( regY >= 0 ){
						sect->b -= 1;
						if( regX >= 0 ) sect->b -= 1;
					}
					sect->c = sect->b;
				}

YieldSection:
				self->curToken = rb + 1;
				DaoParser_DelScope( self, NULL );
				DaoParser_PushTokenIndices( self, rb, 0, 0 );
				DaoParser_AddCode( self, DVM_GOTO, 0, 0, DVM_SECT );
				DArray_Pop( self->outers );
				self->vmcLast->jumpTrue = jump;
				DaoParser_AppendCode( self, label ); /* move to back */
				regLast = call->c;
				result.konst = 0;
				result.reg = regLast;
				result.last = self->vmcLast;
				result.update = call;
				self->curToken = rb + 1;
				break;
InvalidSection:
				self->curToken = rb + 1;
				enode.reg = -1;
				return enode;
			}
		case DTOK_LSB :
			{
				/*  map/list/array[ i ] : */
				int regcount = self->regCount;
				int assignment = 0;

				rb = DaoParser_FindPairToken( self, DTOK_LSB, DTOK_RSB, start, end );
				if( rb < 0 ) return error;

				if( (rb+1) < self->tokens->size ){
					int T = tokens[rb+1]->type;
					assignment = T == DTOK_ASSN || (T >= DTOK_ADDASN && T <= DTOK_XORASN);
				}

				cst = 0;
				self->curToken += 1;
				if( DaoParser_FindOpenToken( self, DTOK_COMMA, start+1, rb, 0 ) < 0 ){
					if( (start+1) == rb ){
						enode = DaoParser_NoneValue( self );
					}else{
						enode = DaoParser_ParseExpression2( self, 0, DAO_EXPRLIST_SLICE, ASSIGNMENT_WARNING );
					}
					if( enode.reg < 0 || self->curToken != rb ) return error;
					/* TODO: the same for GETF; */
					if( result.konst && enode.konst && assignment == 0 ){
						DaoValue *v1 = DaoParser_GetVariable( self, result.konst );
						DaoValue *v2 = DaoParser_GetVariable( self, enode.konst );
						if( v1->type != DAO_CLASS && v1->type != DAO_CTYPE ){
							regLast = DaoParser_MakeArithConst( self, DVM_GETI, v1, v2, & cst, back, regcount );
							result.konst = cst;
						}else{
							result.konst = 0;
						}
					}else{
						result.konst = 0;
					}
					if( result.konst == 0 ){
						regLast = DaoParser_PushRegister( self );
						DaoParser_PushTokenIndices( self, postart, start, rb );
						DaoParser_AddCode( self, DVM_GETI, result.reg, enode.reg, regLast );
						if( getx ) self->vmcLast->extra = getx;
					}
				}else{
					reg = -1;
					cid = DaoParser_GetArray( self );
					if( result.last == NULL ){
						DaoParser_PushTokenIndices( self, postart, start, start );
						DaoParser_AddCode( self, DVM_LOAD, regLast,0,0/*unset*/ );
					}
					enode = DaoParser_ParseExpressionList2( self, DTOK_COMMA, self->vmcLast, cid, DAO_EXPRLIST_SLICE );
					if( enode.reg < 0 || self->curToken != rb ) return enode;
					regLast = DaoParser_PushRegister( self );
					DaoParser_PushTokenIndices( self, postart, start, rb );
					DaoParser_AddCode( self, DVM_GETMI, enode.reg, enode.count-1, regLast );

					if( result.konst && assignment == 0 ){
						DaoValue *v = DaoParser_GetVariable( self, result.konst );
						cid->items.pInt[0] = result.konst;
						enode.prev = extra ? extra->prev : back;

						if( v->type != DAO_CLASS && v->type != DAO_CTYPE ){
							reg = DaoParser_MakeEnumConst( self, & enode, cid, regcount );
						}
					}
					if( reg >=0 ){
						result.first = self->vmcLast;
						result.konst = cst = enode.konst;
						regLast = reg;
					}else{
						result.konst = 0;
						if( getx ) self->vmcLast->extra = getx;
					}
				}
				result.reg = regLast;
				result.last = result.update = self->vmcLast;
				result.konst = cst;
				self->curToken = rb + 1;
				break;
			}
		case DTOK_COLON2 :
			{
				int j, opa = result.reg, opb = -1;

				self->curToken += 1;

				if( DaoParser_CurrentTokenType( self ) != DTOK_IDENTIFIER ){
					if( eltype & DAO_EXPRLIST_SCOPE ){ /* operator Klass::+() { } */
						result.scope = result.konst;
						result.konst = 0;
						self->curToken --;
						return result;
					}
					DaoParser_Error2( self, DAO_INVALID_EXPRESSION, start, 1, 0 );
					return error;
				}
				name = & tokens[self->curToken]->string;
				/* printf( "%s  %i\n", name->chars, result.konst ); */
				if( result.konst ){
					int count = self->errors->size;
					DaoValue *obj = DaoParser_GetVariable( self, result.konst );
					daostr.value = name;
					regLast = DaoParser_MakeArithConst( self, DVM_GETF, obj, svalue, & cst, back, regcount );
					result.scope = result.konst;
					result.konst = cst;
					if( cst == 0 && (eltype & DAO_EXPRLIST_SCOPE) ){
						DList_Erase( self->errors, count, -1 );
						self->curToken --;
						return result;
					}
				}
				if( result.konst == 0 ){
					opb = DaoParser_AddFieldConst( self, name );
					regLast = DaoParser_PushRegister( self );
					DaoParser_PushTokenIndices( self, postart, start, start+1 );
					DaoParser_AddCode( self, DVM_GETF, opa, opb, regLast );
					if( getx ) self->vmcLast->extra = getx;
				}
				result.reg = regLast;
				result.last = result.update = self->vmcLast;
				self->curToken += 1;
				break;
			}
		case DTOK_DOT :
			{
				int opb, opa = result.reg;
				self->curToken += 1;
				if( self->curToken >= self->tokens->size ){
					DaoParser_Error2( self, DAO_INVALID_EXPRESSION, start, 1, 0 );
					return error;
				}
				name = & tokens[self->curToken]->string;
				if( tokens[self->curToken]->type == DTOK_DIGITS_DEC ){
					daoint id = DaoToken_ToInteger( tokens[self->curToken] );
					if( id > 0xffff ){
						DaoParser_Error( self, DAO_INVALID_INDEX, name );
						return error;
					}
					if( result.konst ){
						DaoInteger di = {DAO_INTEGER,0,0,0,0};
						DaoValue *obj = DaoParser_GetVariable( self, result.konst );
						DaoValue *b = (DaoValue*) & di;
						di.value = id;
						regLast = DaoParser_MakeArithConst( self, DVM_GETDI, obj, b, & cst, back, regcount );
						result.scope = result.konst;
						result.konst = cst;
					}
					if( result.konst == 0 ){
						regLast = DaoParser_PushRegister( self );
						DaoParser_PushTokenIndices( self, postart, start, rb );
						DaoParser_AddCode( self, DVM_GETDI, opa, id, regLast );
					}
					result.reg = regLast;
					result.last = result.update = self->vmcLast;
					self->curToken += 1;
					break;
				}else if( tokens[start+1]->name == DTOK_LCB ){ /* .{}, data packing */
					/* dao_class.{ members } enumeration,
					 * or routine.{ parameters } */
					DaoInode *inode = self->vmcLast;
					int code = DVM_PACK;
					int rb = DaoParser_FindPairToken( self, DTOK_LCB, DTOK_RCB, start, end );
					if( rb < 0 ) return error;

					if( result.last && back->code == DVM_LOAD2 ){ /* X.Y */
						DaoParser_PopRegister( self ); /* opc of GETF will be reallocated; */
						extra = back->prev;
						back->code = DVM_LOAD;
						back->b = 0;
						code = DVM_MPACK;
					}else if( result.last &&  DaoVmCode_CheckPermutable( back->code ) ){
						extra = back;
					}else{
						DaoParser_PushTokenIndices( self, postart, start, start );
						DaoParser_AddCode( self, DVM_LOAD, regLast, 0, 0/*unset*/ );
						extra = self->vmcLast;
					}
					self->curToken += 1;
					cid = DaoParser_GetArray( self );
					enode = DaoParser_ParseExpressionList2( self, DTOK_COMMA, extra, cid, DAO_EXPRLIST_TUPLE );
					if( enode.reg < 0 || self->curToken != rb ) return enode;

					regLast = DaoParser_PushRegister( self );
					DaoParser_PushTokenIndices( self, postart, start, rb );
					DaoParser_AddCode( self, code, enode.reg, enode.count-1, regLast );

					if( self->needConst && result.konst && enode.konst == (enode.count-1) ){
						value = DaoParser_GetVariable( self, result.konst );
						if( code == DVM_PACK && (value == NULL || value->type != DAO_CLASS) ){
							cid->items.pInt[0] = result.konst;
							if( extra ){
								DaoInode *inode = self->vmcLast;
								inode->last = inode->first + inode->last - extra->first;
								inode->middle = inode->first - extra->first;
								inode->first = extra->first;
							}
							enode.prev = extra ? extra->prev : back;
							regLast = DaoParser_MakeEnumConst( self, & enode, cid, regcount );
							if( regLast >=0 ){
								result.first = self->vmcLast;
								result.konst = enode.konst;
							}
						}else{
							result.konst = 0;
						}
					}else{
						result.konst = 0;
					}
					result.reg = regLast;
					result.last = result.update = self->vmcLast;
					self->curToken = rb + 1;
					break;
				}
				if( DaoParser_CurrentTokenType( self ) != DTOK_IDENTIFIER ){
					DaoParser_Error2( self, DAO_INVALID_EXPRESSION, start, 1, 0 );
					return error;
				}
				if( result.konst ){
					DaoValue *obj = DaoParser_GetVariable( self, result.konst );
					daostr.value = name;
					regLast = DaoParser_MakeArithConst( self, DVM_GETF, obj, svalue, & cst, back, regcount );
					result.konst = cst;
				}
				if( result.konst == 0 ){
					regLast = DaoParser_PushRegister( self );
					opb = DaoParser_AddFieldConst( self, name );
					DaoParser_PushTokenIndices( self, postart, start, start+1 );
					DaoParser_AddCode( self, DVM_GETF, opa, opb, regLast );
					if( getx ) self->vmcLast->extra = getx;
				}
				result.reg = regLast;
				result.last = result.update = self->vmcLast;
				DaoParser_PushTokenIndices( self, postart, start, start+1 );
				DaoParser_AddCode( self, DVM_LOAD2, opa, 0, 0 );
				self->curToken += 1;
				break;
			}
		case DTOK_LT :
			if( result.konst == 0 ) return result;
			value = DaoParser_GetVariable( self, result.konst );
			if( value->type != DAO_CTYPE && value->type != DAO_TYPE
					&& value->type != DAO_INTERFACE ) return result;

			rb = DaoParser_FindPairToken( self, DTOK_LT, DTOK_GT, start, end );
			if( rb < 0 ) return result;
			dbase = DaoParse_InstantiateType( self, value, start+1, rb-1 );
			if( dbase ){
				DaoInode *prev = result.first ? result.first->prev : NULL;
				DaoParser_PopBackCode( self );
				DaoParser_PopRegister( self );
				self->curToken = rb + 1;
				cst = DaoRoutine_AddConstant( self->routine, dbase );
				cst = LOOKUP_BIND_LC( cst );
				result.konst = cst;
				result.reg = DaoParser_GetNormRegister( self, cst, 0, start, 0, rb );
				result.last = result.update = self->vmcLast;
				if( prev ) result.first = prev->next;
				break;
			}else{
				DaoParser_Error2( self, DAO_FAILED_INSTANTIATION, start-1, rb, 0 );
				self->curToken = curStart;
			}
			return result;
		default : return result;
		}
	}
	return result;
}
static void DaoParser_TryAddSetCodes( DaoParser *self )
{
	DaoInode *setx, *inode = self->vmcLast;
	unsigned short opc = inode->c;
	if( inode->code < DVM_SETVH || inode->code > DVM_SETF ) return;
	while( inode->extra && inode->c == opc ){
		inode = inode->extra;
		if( inode->code < DVM_GETVH || inode->code > DVM_GETF ) continue;
		setx = DaoParser_PushBackCode( self, (DaoVmCodeX*)inode );
		setx->code += DVM_SETVH - DVM_GETVH;
		setx->c = inode->a;
		setx->a = inode->c;
		/* DaoInode_Print( inode ); */
	}
}
static DaoEnode DaoParser_ParseUnary( DaoParser *self, int stop, int eltype )
{
	DaoEnode result;
	DaoInode *back = self->vmcLast;
	int oldcount = self->regCount;
	int tok = DaoParser_CurrentTokenName( self );
	int oper = daoArithOper[ tok ].oper;
	int code = mapAithOpcode[ oper ];
	int end, start = self->curToken;
	int opa, opb = 0, ec = 0;

	if( daoArithOper[ tok ].left == 0 ) return DaoParser_ParsePrimary( self, stop, eltype );

	/* parse left hand unary operator */
	self->curToken += 1;
	result = DaoParser_ParseUnary( self, stop, 0 );
	if( result.reg < 0 ) return result;
	if( oper == DAO_OPER_ADD ) return result;

	switch( oper ){
	case DAO_OPER_NOT : code = DVM_NOT; break;
	case DAO_OPER_INCR : code = DVM_ADD; break;
	case DAO_OPER_DECR : code = DVM_SUB; break;
	case DAO_OPER_SUB  : code = DVM_MINUS; break;
	case DAO_OPER_TILDE : code = DVM_TILDE; break;
	case DAO_OPER_MOD   : code = DVM_SIZE; break;
	default : ec = DAO_INVALID_EXPRESSION; goto ErrorParsing;
	}
	if( result.konst && (code == DVM_ADD || code == DVM_SUB) ){
		ec = DAO_EXPR_MODIFY_CONSTANT;
		goto ErrorParsing;
	}

	opa = result.reg;
	end = self->curToken - 1;
	if( result.konst && code != DVM_ADD && code != DVM_SUB ){
		DaoValue *value = DaoParser_GetVariable( self, result.konst );
		result.reg = DaoParser_MakeArithConst( self, code, value, dao_none_value, & result.konst, back, oldcount );
		if( result.reg < 0 ){
			ec = DAO_CTW_INV_CONST_EXPR;
			goto ErrorParsing;
		}
		result.prev = back;
		result.first = result.last = result.update = self->vmcLast;
		return result;
	}else if( code == DVM_ADD || code == DVM_SUB ){
		DaoInode *vmc = result.last;
		int code2 = vmc ? vmc->code : -1;
		opb = DaoParser_IntegerOne( self, start );
		DaoParser_PushTokenIndices( self, start, start, end );
		DaoParser_AddCode( self, code, opa, opb, opa );
		if( code2 >= DVM_GETVH && code2 <= DVM_GETF ){
			DaoParser_PushBackCode( self, (DaoVmCodeX*) vmc );
			self->vmcLast->extra = vmc->extra;
			vmc = self->vmcLast;
			opa = vmc->a; vmc->a = vmc->c; vmc->c = opa;
			vmc->code += DVM_SETVH - DVM_GETVH;
			DaoParser_TryAddSetCodes( self );
		}
		result.update = self->vmcLast;
		/* to prevent the previous instruction from beeing updated by ParseExpressionList(s) */
		DaoParser_AddCode( self, DVM_UNUSED, 0,0,0 );
	}else{
		result.reg = DaoParser_PushRegister( self );
		DaoParser_PushTokenIndices( self, start, start, end );
		DaoParser_AddCode( self, code, opa, opb, result.reg );
	}
	result.konst = 0;
	result.prev = back;
	result.first = result.last = self->vmcLast;
	if( result.update == NULL ) result.update = self->vmcLast;
	return result;
ErrorParsing:
	DaoParser_Error( self, ec, & self->tokens->items.pToken[start]->string );
	result.reg = -1;
	return result;
}
static DaoEnode DaoParser_ParseOperator( DaoParser *self, DaoEnode LHS, int prec, int stop, int eltype, int astat )
{
	DaoEnode result = { -1, 0, 1, 0, NULL, NULL, NULL, NULL };
	DaoEnode RHS = { -1, 0, 1, 0, NULL, NULL, NULL, NULL };
	DaoToken **tokens = self->tokens->items.pToken;
	DaoInode *move, *test, *jump, *jump2, *inode = NULL;
	int oper, code, postart = self->curToken-1, posend;

	if( LHS.first ) postart = LHS.first->first;
	result.prev = LHS.prev;
	while(1){
		DaoInode *last2, *last1;
		int pos = self->curToken, fold = 0;
		int thisPrec, nextPrec, curtok = DaoParser_CurrentTokenName( self );
		if( curtok == stop ) return LHS;
		thisPrec = DaoParser_GetOperPrecedence( self );

		/* If this is not an operator, or is an operator with precedence
		 * less than the precedence of the previous operator: */
		if(thisPrec < prec) return LHS;

		/* Surely an operator: */
		oper = daoArithOper[ tokens[self->curToken]->name ].oper;
		self->curToken += 1; /* eat the operator */

		code = LHS.update ? LHS.update->code : DVM_NOP;
		if( oper == DAO_OPER_ASSN && code >= DVM_GETVH && code <= DVM_GETF ){
			/* GETX will be to SETX, pop unused register: */
			DaoParser_PopRegister( self ); /* opc of LHS.last */
		}

		/* Parse the primary expression after the binary operator: */
		last1 = self->vmcLast;
		RHS = DaoParser_ParseUnary( self, stop, 0 );
		if( RHS.reg < 0 ){
			if( oper != DAO_OPER_COLON || self->curToken > pos + 1 ) return RHS;
			/* e : , */
			RHS = DaoParser_NoneValue( self );
		}
		result.update = NULL;
		if( oper == DAO_OPER_IF ){ /* conditional operation:  c ? e1 : e2 */
			DaoEnode RHS1, RHS2;
			int colon, prec2 = 10*(20 - daoArithOper[DTOK_COLON].binary);
			RHS1 = DaoParser_ParseOperator(self, RHS, prec2 + 1, DTOK_COLON, 0, 1 );
			colon = self->curToken;
			if( DaoParser_CheckTokenType( self, DTOK_COLON, ":" ) == 0 ) RHS1.reg = -1;
			if( RHS1.reg < 0 ) return RHS1;
			if( last1 == self->vmcLast ){
				DaoParser_PushTokenIndices( self, pos, pos, pos );
				last1 = DaoParser_AddCode( self, DVM_MOVE, RHS1.reg, 0, RHS1.reg );
				RHS1.first = RHS1.update = RHS1.last = last1;
			}
			last2 = self->vmcLast;
			self->curToken += 1;
			RHS2 = DaoParser_ParseUnary( self, DTOK_COLON, 0 );
			if( RHS2.reg < 0 ) return RHS2;
			RHS2 = DaoParser_ParseOperator(self, RHS2, prec2 + 1, DTOK_COLON, 0, 1 );
			if( RHS2.reg < 0 ) return RHS2;
			if( last2 == self->vmcLast ){
				DaoParser_PushTokenIndices( self, pos, pos, pos );
				last2 = DaoParser_AddCode( self, DVM_MOVE, RHS2.reg, 0, RHS2.reg );
				RHS2.first = RHS2.update = RHS2.last = last2;
			}

			DaoParser_PushTokenIndices( self, pos, pos, pos );
			result.reg = DaoParser_PushRegister( self );
			if( LHS.last == NULL ) LHS.last = RHS1.prev;
			test = DaoParser_InsertCode( self, LHS.last, DVM_TEST, LHS.reg, 0, 0 );
			jump = DaoParser_InsertCode( self, RHS2.prev, DVM_GOTO, 0, 0, 0 );
			DaoParser_InsertCode( self, RHS2.prev, DVM_MOVE, RHS1.reg, 0, result.reg );
			DaoParser_PushTokenIndices( self, pos, colon, self->curToken-1 );
			DaoParser_AddCode( self, DVM_MOVE, RHS2.reg, 0, result.reg );
			DaoParser_AddCode( self, DVM_UNUSED, 0,0,0 );
			jump->jumpTrue = self->vmcLast;
			test->jumpFalse = jump->next;
			result.last = self->vmcLast;
			result.update = result.last->prev;
			result.first = LHS.first;
			if( result.first == NULL ) result.first = result.last;
			if( result.update == NULL ) result.update = result.last;
			LHS = result;
			continue;
		}

		nextPrec = DaoParser_GetOperPrecedence( self );
		/* If the pending operator has higher precedence,
		 * use RHS as the LHS of the pending operator: */
		if (thisPrec < nextPrec) {
			RHS = DaoParser_ParseOperator(self, RHS, thisPrec+1, stop, 0, 1 );
			if( RHS.reg < 0 ) return RHS;
		}

		posend = self->curToken - 1;
		if( oper == DAO_OPER_ASSN ){
			if( LHS.konst ) goto InvalidConstModificatioin;
			if( curtok == DTOK_ASSN && astat ){
				DaoParser_Error2( self, DAO_WARN_ASSIGNMENT, postart, posend, 0 );
				return result;
			}

			if( code >= DVM_GETVH && code <= DVM_GETF ){ /* change GETX to SETX */
				LHS.last->code += DVM_SETVH - DVM_GETVH;
				LHS.last->c = LHS.last->a;
				LHS.last->a = RHS.reg;
				DaoParser_AppendCode( self, LHS.last ); /* move to back */
				result.reg = RHS.reg;
				result.update = RHS.update;
				result.last = self->vmcLast;
				DaoParser_TryAddSetCodes( self );
			}else{
				DaoParser_PushTokenIndices( self, postart, postart, posend );
				DaoParser_AddCode( self, DVM_MOVE, RHS.reg, 0x1, LHS.reg );
				result.reg = LHS.reg;
				result.last = self->vmcLast;
			}
		}else if( oper >= DAO_OPER_ASSN_ADD && oper <= DAO_OPER_ASSN_XOR ){
			if( LHS.konst ) goto InvalidConstModificatioin;
			result.last = DaoParser_AddBinaryCode( self, mapAithOpcode[oper], & LHS, & RHS, pos );
			result.update = result.last;
			DaoParser_PopRegister( self ); /* result.last->c */
			if( code >= DVM_GETVH && code <= DVM_GETF ){ /* add SETX */
				/*
				// For X += Y, if X is not local, it will be compiled into:
				//   GETX A1, B1, C1; # LHS.last;
				//   ADD  C1, B2, C2; # result.last;
				//   SETX C2, B1, A1; # inode;
				// If X is a string and Y an integer, it will require that
				//   C1 == C2;
				// to pass static type checking:
				*/
				result.last->c = LHS.last->c;
				result.reg = LHS.last->c;

				inode = DaoParser_PushBackCode( self, (DaoVmCodeX*) LHS.last );
				inode->extra = LHS.last->extra;
				inode->code += DVM_SETVH - DVM_GETVH;
				inode->c = inode->a;
				inode->a = result.reg;
				result.last = inode;
				DaoParser_TryAddSetCodes( self );
			}else{
				result.last->c = result.last->a;
				result.reg = result.last->a;
			}
		}else if( oper == DAO_OPER_AND || oper == DAO_OPER_OR ){
			result.last = DaoParser_AddBinaryCode( self, mapAithOpcode[oper], & LHS, & RHS, pos );
			result.reg = result.last->c;
			fold = 1;
			if( LHS.first != LHS.last || RHS.first != RHS.last ){ /* use branching */
				DaoInode *it = LHS.first;
				assert( LHS.update != NULL || RHS.update != NULL );
				if( LHS.last == NULL ) LHS.last = RHS.prev;
				if( RHS.first == NULL ) RHS.first = self->vmcLast->prev;
				result.reg = DaoParser_PushRegister( self );
				DaoParser_PushTokenIndices( self, postart, 0, 0 );
				move = DaoParser_InsertCode( self, LHS.last, DVM_MOVE, LHS.reg, 0, result.reg );
				test = DaoParser_InsertCode( self, move, DVM_TEST, LHS.reg, 0, 0 );
				while( it ){
					if( it->jumpTrue == test->next ) it->jumpTrue = move;
					if( it->jumpFalse == test->next ) it->jumpFalse = move;
					it = it->next;
				}
				result.last->code = mapAithOpcode[oper];
				result.last->a = result.reg;
				result.last->b = result.last->b;
				result.last->c = result.reg;
				DaoParser_PushTokenIndices( self, postart, 0, 0 );
				test->jumpFalse = DaoParser_AddCode( self, DVM_NOP, 0, 0, 0 );
				if( oper == DAO_OPER_OR ){
					jump = DaoParser_InsertCode( self, test, DVM_GOTO, 0, 0, 0 );
					jump->jumpTrue = self->vmcLast;
					test->jumpFalse = jump->next;
				}
				fold = 0;
			}
		}else{
			if( oper == DAO_OPER_COLON && !(eltype & DAO_EXPRLIST_SLICE) )
				goto InvalidExpression;
			result.last = DaoParser_AddBinaryCode( self, mapAithOpcode[oper], & LHS, & RHS, pos );
			result.reg = result.last->c;
			fold = 1;
		}
		result.konst = 0;
		if( fold && LHS.konst && RHS.konst ){
			DaoValue *v1 = DaoParser_GetVariable( self, LHS.konst );
			DaoValue *v2 = DaoParser_GetVariable( self, RHS.konst );
			code = result.last->code;
			result.reg = DaoParser_MakeArithConst( self, code, v1, v2, & result.konst, LHS.prev, LHS.reg );
			result.last = self->vmcLast;
			if( result.reg < 0 ){
				DaoParser_Error( self, DAO_CTW_INV_CONST_EXPR, NULL );
				return result;
			}
		}else{
			result.first = LHS.first;
			if( result.first == NULL ) result.first = RHS.first;
		}
		if( oper == DAO_OPER_NOTIN ){
			/* It was compiled as IN, now add NOT: */
			if( result.konst ){
				DaoValue *value = DaoParser_GetVariable( self, result.konst );
				result.reg = DaoParser_MakeArithConst( self, DVM_NOT, value, dao_none_value, & result.konst, LHS.prev, LHS.reg );
				if( result.reg < 0 ) return result;
				result.last = result.update = self->vmcLast;
			}else{
				int reg = DaoParser_PushRegister( self );
				DaoParser_PushTokenIndices( self, postart, postart, posend );
				DaoParser_AddCode( self, DVM_NOT, result.reg, 0, reg );
				result.reg = reg;
				result.last = result.update = self->vmcLast;
			}
		}
		if( result.first == NULL ) result.first = result.last;
		if( result.update == NULL ) result.update = result.last;
		LHS = result;
	}
	return LHS;
InvalidConstModificatioin:
	DaoParser_Error3( self, DAO_EXPR_MODIFY_CONSTANT, postart );
InvalidExpression:
	DaoParser_Error3( self, DAO_INVALID_EXPRESSION, postart );
	result.reg = -1;
	return result;
}
static DaoEnode DaoParser_ParseExpression2( DaoParser *self, int stop, int eltype, int astat )
{
	int tt, start = self->curToken;
	DaoEnode RHS, LHS = { -1, 0, 1, 0, NULL, NULL, NULL, NULL };
	DaoToken **tokens = self->tokens->items.pToken;

#if 0
	int i, end = self->tokens->size;
	printf("DaoParser_ParseExpression(): start = %i;\n", start );
	for( i=start;i<end;i++) printf("%s  ", tokens[i]->string.chars); printf("\n");
#endif
	if( DaoParser_CurrentTokenType( self ) == DTOK_COLON ){
		/* : e , */
		if( !(eltype & DAO_EXPRLIST_SLICE) ) goto Done;
		LHS = DaoParser_NoneValue( self );

		tt = DaoParser_NextTokenType( self );
		if( tt == DTOK_COMMA || tt == DTOK_SEMCO || tt == DTOK_RSB ){
			/* : in slicing, equivalent to none: */
			self->curToken += 1;
			goto Done;
		}
	}else{
		LHS = DaoParser_ParseUnary( self, stop, eltype );
	}
	if( LHS.reg >= 0 && DaoParser_GetOperPrecedence( self ) >= 0 ){
		LHS = DaoParser_ParseOperator( self, LHS, 0, stop, eltype, astat );
	}
Done:
	if( LHS.reg < 0 ){
		if( self->curToken < self->tokens->size && DaoParser_CurrentTokenType( self ) < DTOK_COMMENT ){
			DString *tok = & self->tokens->items.pToken[ self->curToken ]->string;
			DaoParser_Error( self, DAO_INVALID_TOKEN, tok );
		}
		self->curToken = start;
		DaoParser_Error3( self, DAO_INVALID_EXPRESSION, start );
	}
	self->vmcValue = LHS.update;
	return LHS;
}
static DaoEnode DaoParser_ParseExpression( DaoParser *self, int stop )
{
	return DaoParser_ParseExpression2( self, stop, 0, ASSIGNMENT_WARNING );
}

static DaoEnode DaoParser_ParseExpressionList2( DaoParser *self, int sep, DaoInode *pre, DList *cids, int eltype )
{
	DaoInode *inode;
	DaoType *type = self->enumTypes->size ? self->enumTypes->items.pType[0] : NULL;
	DaoEnode item, result = { -1, 0, 1, 0, NULL, NULL, NULL, NULL };
	DMap *vartypes = self->routine->body->localVarType;
	DList *inodes = DList_New(0);
	int i, tok, cur, id = 0;
	while( pre != NULL ){
		DList_Append( inodes, pre );
		pre = pre->next;
		if( cids ) DList_Append( cids, 0 );
	}
	result.konst = 0;
	result.prev = self->vmcLast;
	self->curToken -= 1;
	do {
		self->curToken += 1;
		cur = self->curToken;
		tok = DaoParser_CurrentTokenName( self );
		if( tok == DTOK_RB || tok == DTOK_RCB || tok == DTOK_RSB ) break;
		if( tok == DTOK_DOTS || tok == DTOK_SEMCO ) break;
		if( eltype & DAO_EXPRLIST_ARRAY ){
			/* bar : array<float> = [[1.5 : 3] : [10.5 : 3] : 5] */
			DList_PushFront( self->enumTypes, type );
		}else{
			DaoParser_PushItemType( self, type, id++, sep );
		}
		item = DaoParser_ParseExpression2( self, sep, eltype, ASSIGNMENT_WARNING );
		DList_PopFront( self->enumTypes );
		if( item.reg < 0 ){
			if( self->curToken == cur ) break;
			goto Finalize;
		}
		result.konst += item.konst != 0;
		/* Avoid adding an extra MOVE for intermediate result: */
		if( item.update == self->vmcLast && MAP_Find( vartypes, item.update->c ) == NULL ){
			DaoParser_PopRegister( self );
			DList_Append( inodes, item.last );
		}else{ /* { a[1] += 2 }: item.update is ADD, but item.last is SETI */
			int p1, p2, p3;
			p1 = p2 = p3 = self->curToken - 1;
			if( item.update ){
				p1 = item.update->first;
				p2 = p1 + item.update->middle;
				p3 = p1 + item.update->last;
			}
			DaoParser_PushTokenIndices( self, p1, p2, p3 );
			inode = DaoParser_AddCode( self, DVM_MOVE, item.reg, 0, 0 );
			DList_Append( inodes, inode );
		}
		if( cids ) DList_Append( cids, item.konst );
	} while( DaoParser_CurrentTokenName( self ) == sep );
	result.count = inodes->size;
	result.reg = DaoParser_PushRegisters( self, inodes->size );
	/*
	// Do not change the order of these instructions!
	// Consider: b = 2; a = { b, b += 3 };
	*/
	for(i=0; i<inodes->size; i++) inodes->items.pInode[i]->c = result.reg + i;
	result.first = result.prev->next;
	result.last = self->vmcLast;
Finalize:
	DList_Delete( inodes );
	return result;
}
static DaoEnode DaoParser_ParseExpressionList( DaoParser *self, int sep, DaoInode *pre, DList *cids )
{
	return DaoParser_ParseExpressionList2( self, sep, pre, cids, 0 );
}
/* sep2 should be a natural seperator such as comma and semicolon: */
static DaoEnode DaoParser_ParseExpressionLists( DaoParser *self, int sep1, int sep2, int *step, DList *cids )
{
	DaoInode *inode;
	DaoType *type = self->enumTypes->size ? self->enumTypes->items.pType[0] : NULL;
	DaoEnode item, result = { -1, 0, 1, 0, NULL, NULL, NULL, NULL };
	DMap *vartypes = self->routine->body->localVarType;
	DList *inodes = DList_New(0);
	int i, tok, id=0, count = 0;

	result.konst = 0;
	result.prev = self->vmcLast;
	self->curToken -= 1;
	do {
		self->curToken += 1;
		tok = DaoParser_CurrentTokenName( self );
		if( tok == DTOK_RB || tok == DTOK_RCB || tok == DTOK_RSB ) break;
		if( tok == DTOK_DOTS || tok == DTOK_SEMCO ) break;
		DaoParser_PushItemType( self, type, id++, sep1 );
		item = DaoParser_ParseExpression( self, sep1 );
		DList_PopFront( self->enumTypes );
		if( item.reg < 0 ) goto Finalize;
		result.konst += item.konst != 0;
		/* Avoid adding an extra MOVE for intermediate result: */
		if( item.update == self->vmcLast && MAP_Find( vartypes, item.update->c ) == NULL ){
			DaoParser_PopRegister( self );
			DList_Append( inodes, item.last );
		}else{ /* { a[1] += 2 }: item.update is ADD, but item.last is SETI */
			int p1, p2, p3;
			p1 = p2 = p3 = self->curToken - 1;
			if( item.update ){
				p1 = item.update->first;
				p2 = p1 + item.update->middle;
				p3 = p1 + item.update->last;
			}
			DaoParser_PushTokenIndices( self, p1, p2, p3 );
			inode = DaoParser_AddCode( self, DVM_MOVE, item.reg, 0, 0 );
			DList_Append( inodes, inode );
		}
		if( cids ) DList_Append( cids, item.konst );
		tok = DaoParser_CurrentTokenName( self );
		count += 1;
		if( tok != sep1 ){
			if( step && *step && count != *step ) return result;
			if( step ) *step = count;
			count = 0;
		}
	} while( tok == sep1 || tok == sep2 );
	result.count = inodes->size;
	result.reg = DaoParser_PushRegisters( self, inodes->size );
	/* Do not change the order of these instructions! */
	for(i=0; i<inodes->size; i++) inodes->items.pInode[i]->c = result.reg + i;
	result.first = result.prev->next;
	result.last = self->vmcLast;
Finalize:
	DList_Delete( inodes );
	return result;
}

DaoProcess* DaoNamespace_ReserveFoldingOperands( DaoNamespace *self, int N )
{
	DaoRoutine *rout;
	DaoProcess *proc;

	DaoNamespace_InitConstEvalData( self );
	proc = self->constEvalProcess;
	rout = self->constEvalRoutine;
	if( rout->body->regType->size < N ) DList_Resize( rout->body->regType, N, NULL );
	DaoProcess_PushFrame( proc, N );
	DaoProcess_PopFrame( proc );
	proc->activeRoutine = self->constEvalRoutine;
	proc->activeValues = proc->stackValues;
	proc->activeTypes = rout->body->regType->items.pType;
	proc->activeNamespace = self;
	return proc;
}
static DaoValue* DaoParser_EvalConst( DaoParser *self, DaoProcess *proc, int nvalues )
{
	DaoVmCode *vmc = proc->activeCode;
	DaoValue **operands = proc->activeValues + 1;
	DaoByteBlock *eval, *coder = self->byteBlock;
	DaoStream *stream;
	DaoValue *value;
	int i, j, count, max = 0;

	if( self->hostType ){
		GC_Assign( & proc->activeRoutine->routHost, self->hostType );
	}
	if( self->byteBlock ){
		for(i=0; i<nvalues; ++i){
			DaoByteBlock *block = DaoByteBlock_FindObjectBlock( coder, operands[i] );
			if( operands[i] && operands[i]->type > max ){
				max = operands[i]->type;
			}else if( block != NULL && block->type == DAO_ASM_EVAL ){
				max = DAO_OBJECT;
			}
		}
		/*
		// Encode the constant as ASM_EVAL if any operand was encoded as ASM_EVAL.
		// Otherwise, do not encode it now as it can be encode as value later.
		//
		// Encode the operands before doing constant folding, because they might
		// be changed by the evaluation.
		*/
		if( max > DAO_COMPLEX ){
			DaoByteBlock_EncodeValues( coder, operands, nvalues );
			DaoByteBlock_EncodeType( coder, proc->activeTypes[0] );
		}
	}
	value = DaoProcess_MakeConst( proc, self->evalMode );
	if( value != NULL && max > DAO_COMPLEX ){
		if( max > DAO_ENUM || DaoByteBlock_FindObjectBlock( coder, value ) == NULL ){
			DaoType* retype = proc->activeTypes[0];
			int opb = vmc->code == DVM_GETF ? 2 : vmc->b;
			eval = DaoByteBlock_AddEvalBlock( coder, value, vmc->code, opb, self->evalMode, retype );
			DaoByteBlock_AddBlockIndexData( eval, 0, nvalues );
		}
	}
	GC_DecRC( proc->activeRoutine->routHost );
	proc->activeRoutine->routHost = NULL;

	/*
	// DaoProcess_MakeConst() may produce a NULL value without
	// raising an exceptioin. This is acceptable for constant
	// folding for field expressions such as: namespace::variable.
	*/
	if( proc->exceptions->size == 0 ) return value;

	stream = DaoStream_New();
	stream->mode |= DAO_STREAM_STRING;
	DaoProcess_PrintException( proc, stream, 1 );
	DaoParser_Error( self, DAO_EVAL_EXCEPTION, stream->streamString );
	DaoParser_Error( self, DAO_CTW_INV_CONST_EXPR, NULL );
	DaoStream_Delete( stream );
	return value;
}
int DaoParser_MakeEnumConst( DaoParser *self, DaoEnode *enode, DList *cid, int regcount )
{
	DaoProcess *proc;
	DaoType *type = self->enumTypes->size ? self->enumTypes->items.pType[0] : NULL;
	DaoVmCode vmcValue = {0,1,0,0};
	DaoValue *value;
	int p1 = self->vmcLast->first;
	int p3 = p1 + self->vmcLast->last;
	int i, N = enode->count;

	vmcValue.code = self->vmcLast->code;
	vmcValue.b = self->vmcLast->b;
	proc = DaoNamespace_ReserveFoldingOperands( self->nameSpace, N+1 );
	/* printf( "code = %s, %i\n", DaoVmCode_GetOpcodeName( code ), N ); */
	/* Prepare registers for the instruction. */
	for( i=0; i<N; i++ ){
		/* printf( "reg = %i\n", cid->items.pInt[i] ); */
		/* No need GC here: */
		DaoValue *v = DaoParser_GetVariable( self, cid->items.pInt[i] );
		DaoValue_Copy( v, & proc->activeValues[i+1] );
	}
	DaoParser_PopCodes2( self, enode->prev );
	for(i=regcount; i<self->regCount; i++) MAP_Erase( self->routine->body->localVarType, i );
	DaoParser_PopRegisters( self, self->regCount - regcount );
	/* Execute the instruction to get the const result: */
	GC_Assign( & proc->activeTypes[0], type );
	proc->activeCode = & vmcValue;
	value = DaoParser_EvalConst( self, proc, N );
	if( value == NULL ) return -1;
	enode->konst = LOOKUP_BIND_LC( DaoRoutine_AddConstant( self->routine, value ));
	return DaoParser_GetNormRegister( self, enode->konst, 0, p1, 0, p3 );
}
int DaoParser_MakeArithConst( DaoParser *self, ushort_t code, DaoValue *a, DaoValue *b, int *cst, DaoInode *back, int regcount )
{
	DaoValue *value;
	DaoProcess *proc;
	DaoVmCode vmc = { 0, 1, 2, 0 };
	int p1 = self->vmcLast->first;
	int p2 = p1 + self->vmcLast->middle;
	int p3 = p1 + self->vmcLast->last;

	DaoParser_PopCodes2( self, back );
	DaoParser_PopRegisters( self, self->regCount - regcount );

	*cst = 0;
	vmc.code = code;
	proc = DaoNamespace_ReserveFoldingOperands( self->nameSpace, 3 );
	if( code == DVM_NAMEVA ) vmc.a = DaoRoutine_AddConstant( proc->activeRoutine, a );
	if( code == DVM_GETF ) vmc.b = DaoRoutine_AddConstant( proc->activeRoutine, b );
	if( code == DVM_GETDI ) vmc.b = b->xInteger.value;
	if( a ) DaoValue_Copy( a, & proc->activeValues[1] );
	if( b ) DaoValue_Copy( b, & proc->activeValues[2] );
	GC_DecRC( proc->activeTypes[0] );
	proc->activeTypes[0] = NULL;
	proc->activeCode = & vmc;
	value = DaoParser_EvalConst( self, proc, 2 );
	if( value == NULL ) return -1;
	*cst = LOOKUP_BIND_LC( DaoRoutine_AddConstant( self->routine, value ));
	return DaoParser_GetNormRegister( self, *cst, 0, p1, p2, p3 );
}
