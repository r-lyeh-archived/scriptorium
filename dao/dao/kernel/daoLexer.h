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

#ifndef DAO_LEXER_H
#define DAO_LEXER_H

#include"daoType.h"

enum DaoTokNames
{
	DTOK_NONE ,
	DTOK_CMT_OPEN , /* used by DaoStudio for code highlighting */
	DTOK_VBT_OPEN , /* verbatim opening */
	DTOK_MBS_OPEN ,
	DTOK_WCS_OPEN ,
	DTOK_COMMENT ,
	DTOK_IDENTIFIER ,
	DTOK_ID_THTYPE , /* @type_holder, @decorator */
	DTOK_ID_SYMBOL , /* $symbol */
	DTOK_DIGITS_DEC ,
	DTOK_NUMBER_HEX ,
	DTOK_NUMBER_DEC , /* 123. 123.5 */
	DTOK_NUMBER_SCI ,
	DTOK_NUMBER_IMG , /* 123.0C, imaginary part of complex */
	DTOK_VERBATIM ,
	DTOK_MBS , /* MBS */
	DTOK_WCS , /* WCS */
	DTOK_SPACE , /*  */
	DTOK_TAB ,   /* \t */
	DTOK_NEWLN , /* \n */
	DTOK_SEMCO , /* ; */
	DTOK_LB ,  /* ( */
	DTOK_RB ,  /* ) */
	DTOK_LCB , /* { */
	DTOK_RCB , /* } */
	DTOK_LSB , /* [ */
	DTOK_RSB , /* ] */
	DTOK_DOT ,   /* . */
	DTOK_COMMA , /* , */
	DTOK_COLON , /* : */
	DTOK_COLON2 , /* :: */
	DTOK_ASSN ,  /* = */
	DTOK_ADD , /* + */
	DTOK_SUB , /* - */
	DTOK_MUL , /* * */
	DTOK_DIV , /* / */
	DTOK_MOD , /* % */
	DTOK_QUERY , /* ? */
	DTOK_AMAND , /* & */
	DTOK_PIPE ,  /* | */
	DTOK_XOR ,   /* ^ */
	DTOK_NOT ,   /* ! */
	DTOK_TILDE , /* ~ */
	DTOK_DOLLAR , /* $ */
	DTOK_AT ,     /* @ */
	DTOK_BANG2,   /* !! */
	DTOK_POW ,    /* ** */
	DTOK_AND ,    /* && */
	DTOK_OR ,     /* || */
	DTOK_NOTIN ,  /* not in */
	DTOK_ADDASN , /* += */
	DTOK_SUBASN , /* -= */
	DTOK_MULASN , /* *= */
	DTOK_DIVASN , /* /= */
	DTOK_MODASN , /* %= */
	DTOK_ANDASN , /* &= */
	DTOK_ORASN  , /* |= */
	DTOK_XORASN , /* ^= */
	DTOK_EQ , /* == */
	DTOK_NE , /* != */
	DTOK_LT , /* < */
	DTOK_GT , /* > */
	DTOK_LE , /* <= */
	DTOK_GE , /* >= */
	DTOK_TEQ ,  /* ?=, Type EQ */
	DTOK_TISA , /* ?<, Type IS A */
	DTOK_ARROW ,  /* -> */
	DTOK_FIELD ,  /* => */
	DTOK_LSHIFT , /* << */
	DTOK_RSHIFT , /* >> */
	DTOK_INCR , /* ++ */
	DTOK_DECR , /* -- */
	DTOK_DOTS , /* ... */
	DTOK_NONE2
};
enum DaoKeyNames
{
	DAO_NOKEY1 = DTOK_NONE2 ,
	DKEY_LOAD ,
	DKEY_IMPORT ,
	DKEY_AS ,
	DKEY_CLASS ,
	DKEY_ROUTINE ,
	DKEY_INTERFACE ,
	DKEY_NAMESPACE ,
	DKEY_SELF ,
	DKEY_TYPE ,
	DKEY_ANY ,
	DKEY_TRUE ,
	DKEY_FALSE ,
	DKEY_NONE ,
	DKEY_BOOL ,
	DKEY_INT ,
	DKEY_FLOAT ,
	DKEY_COMPLEX ,
	DKEY_STRING ,
	DKEY_ENUM ,
	DKEY_ARRAY ,
	DKEY_LIST ,
	DKEY_MAP ,
	DKEY_TUPLE ,
	DKEY_AND ,
	DKEY_OR ,
	DKEY_NOT ,
	DKEY_IF ,
	DKEY_ELSE ,
	DKEY_FOR ,
	DKEY_IN ,
	DKEY_DO ,
	DKEY_WHILE ,
	DKEY_SWITCH ,
	DKEY_CASE ,
	DKEY_DEFAULT ,
	DKEY_BREAK ,
	DKEY_SKIP ,
	DKEY_DEFER ,
	DKEY_RETURN ,
	DKEY_YIELD ,
	DKEY_CONST ,
	DKEY_STATIC ,
	DKEY_INVAR ,
	DKEY_VAR ,
	DKEY_PRIVATE ,
	DKEY_PROTECTED ,
	DKEY_PUBLIC ,
	/* Built-in math functions; not real keywords, handle as keywords for convenience: */
	DKEY_CEIL ,
	DKEY_FLOOR ,
	DKEY_ABS ,
	DKEY_ARG ,
	DKEY_IMAG ,
	DKEY_NORM ,
	DKEY_REAL ,
	DKEY_ACOS ,
	DKEY_ASIN ,
	DKEY_ATAN ,
	DKEY_COS ,
	DKEY_COSH ,
	DKEY_EXP ,
	DKEY_LOG ,
	DKEY_SIN ,
	DKEY_SINH ,
	DKEY_SQRT ,
	DKEY_TAN ,
	DKEY_TANH ,
	DAO_NOKEY2
};

int dao_key_hash( const char *str, int len );

typedef struct DIntStringPair
{
	int   value;
	const char *key;
} DIntStringPair;

typedef struct {
	unsigned char  oper;
	unsigned char  left;
	unsigned char  right;
	unsigned char  binary;
} DOper;

DAO_DLL DOper DaoLexer_GetTokenOperInfo( int token );


enum DaoLexFlags
{
	DAO_LEX_ESCAPE = 1,  /* replace escape characters; */
	DAO_LEX_COMMENT = 2, /* extract comment tokens; */
	DAO_LEX_SPACE = 4    /* extract whitespace tokens; */
};


struct DaoToken
{
	unsigned char   type; /* token type: take value in DaoTokNames */
	unsigned char   name; /* token name: may take value in DaoKeyNames */
	unsigned short  cpos; /* charactor position in the line */
	unsigned int    line; /* file line position of the token */
	unsigned int    index; /* index of the token in current routine */
	DString         string; /* token string */

	/*
	// When DaoToken is used in an array to store the definitions
	// of local constants and variables in a routine,
	// (1) type field indicates if it is a constant=0, or varaible=1;
	// (2) name field indicates the lixical level of the cst/var;
	// (3) index field indicates the index of the cst/var value;
	// (4) string field stores the name.
	*/
};

DAO_DLL DaoToken* DaoToken_New();
DAO_DLL void DaoToken_Delete( DaoToken *self );
DAO_DLL DaoToken* DaoToken_Copy( DaoToken *self );

DAO_DLL const char* DaoToken_NameToString( unsigned char name );
DAO_DLL dao_integer DaoToken_ToInteger( DaoToken *self );
DAO_DLL dao_float DaoToken_ToFloat( DaoToken *self );
DAO_DLL int DaoToken_Check( const char *src, int size, int *length );
DAO_DLL int DaoToken_IsNumber( const char *src, int size );
DAO_DLL int DaoToken_IsValidName( const char *src, int size );

DAO_DLL void DaoToken_Set( DaoToken *self, int type, int name, int index, const char *s );



typedef struct DaoLexer  DaoLexer;

struct DaoLexer
{
	DList  *tokens;
	DList  *tokbuf;
};
DAO_DLL DaoLexer* DaoLexer_New();
DAO_DLL void DaoLexer_Delete( DaoLexer *self );

DAO_DLL void DaoLexer_Reset( DaoLexer *self );
DAO_DLL int DaoLexer_Tokenize( DaoLexer *self, const char *src, int flags );

DAO_DLL void DaoLexer_AppendToken( DaoLexer *self, DaoToken *token );
DAO_DLL void DaoLexer_Append( DaoLexer *self, int name, int line, const char *data );

DAO_DLL void DaoLexer_AnnotateCode( DList *tokens, DaoVmCodeX vmc, DString *annot, int max );

DAO_DLL DaoToken*  DArray_PushToken( DArray *self, DaoToken token );

#endif
