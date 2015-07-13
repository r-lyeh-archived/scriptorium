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

#ifndef DAO_BYTECODE_H
#define DAO_BYTECODE_H

#include "daoStdtype.h"


#define DAO_BC_SIGNATURE  "\33Dao\2\0\r\n"


/*
//##########################################################################
//
// Header:
//
// Byte       # ESC, 0x1B;
// Byte       # 0x44, namely 'D';
// Byte       # 0x61, namely 'a';
// Byte       # 0x6F, namely 'o';
// Byte       # major version number, 0x2;
// Byte       # minor version number, 0x0;
// Byte       # Carriage Return (CR), 0x0D;
// Byte       # Line Feed (LF), 0x0A;
// Byte       # format class: 0x0, official; 0x1, encrypted;
// Byte       # size of integer type, standard 0x8;
// Byte       # size of float type, standard 0x8;
// Byte       # one reserved byte;
// Byte[4]    # format hash (rotating hash of the ASM tags and VM opcodes);
// Byte[14]   # 16 reserved bytes;
// Byte       # Carriage Return (CR), 0x0D;
// Byte       # Line Feed (LF), 0x0A;
// Byte[2]    # length of the source path;
// Byte[]     # source path (null-terminated);
// Byte       # Carriage Return (CR), 0x0D;
// Byte       # Line Feed (LF), 0x0A;
//
//##########################################################################
//
// Chunk structure: one byte for chunk type, eight bytes for data.
//
// Typical structure:
// A (1 byte), B (2 bytes), C (2 bytes), D (2 bytes), E (2 bytes)
// A (1 byte), B (2 bytes), C (2 bytes), D (4 bytes)
// A (1 byte), B (4 bytes), C (4 bytes)
//
//##########################################################################
//
// Specifications:
//
//########
// Values:
//########
//
// bool:
// ASM_VALUE(1Byte): DAO_BOOLEAN(1Bytes), Zeros(7Bytes);
// ASM_END(1B): Value(1B), Zeros(7Bytes);
//
//
// int:
// ASM_VALUE(1Byte): DAO_INTEGER(1Bytes), Zeros(7Bytes);
// ASM_END(1B): Value(8B);
//
//
// float:
// ASM_VALUE(1B): DAO_FLOAT(1B), Zeros(7B);
// ASM_END(1B): Value(8B);
//
//
// complex:
// ASM_VALUE(1B): DAO_COMPLEX(1B), Zeros(7B);
//   ASM_DATA(1B): Real(8B);
// ASM_END(1B): Imag(8B);
//
//
// string:
// ASM_VALUE(1B): DAO_STRING(1B), SizeMod16(1B), Bytes(6B);
//   ASM_DATA(1B); Bytes(8B);
// ASM_END(1B): Bytes(8B);
//
//
// enum symbol:
// ASM_VALUE(1B): DAO_ENUM(1B), Zeros(1B), Type-Index(2B), Zeros(4B);
// ASM_END(1B): Value(4B), Zeros(0);
//
//   Notes:
//   The "Type-Index" reference previous blocks which are located backwardly
//   by a such "index" offset. Only blocks that represent values are indexed,
//   And such index is stored as a two-byte short.
//
//   In case short is not sufficient to represent such index, an intermediate
//   indexing chunk can be used:
//
//     ASM_SEEK(1B): New-Index(2B), Zeros(6B);
// 
//   When "New-Index" is also seeked backwardly, and is relative to the
//   seek chunk.
//
//
// array:
// ASM_VALUE(1B): DAO_ARRAY(1B), Numeric-Type(1B), Dimensions(2B), Size(4B);
//   ASM_DATA(1B); Dim1(4B), Dim2(4B);
//   ASM_DATA(1B); More dimensions;
//   ASM_DATA(1B); Data(4B), Data(4B); Or Data(8B);
//   ASM_DATA(1B); More Data;
// ASM_END(1B): Data(8B);
//
//
// list:
// ASM_VALUE(1B): DAO_LIST(1B), Zeros(1B), Type-Index(2B), Size(4B);
//   ASM_DATA(1B); Value-Index(2B), Value-Index(2B), Value-Index(2B), Value-Index(2B);
// ASM_END(1B): Value-Index(2B), Value-Index(2B), Value-Index(2B), Value-Index(2B);
//
//
// map:
// ASM_VALUE(1B): DAO_MAP(1B), Zeros(1B), Type-Index(2B), Hash-Seed(4B);
//   ASM_DATA(1B); Key-Index(2B), Value-Index(2B), Key-Index(2B), Value-Index(2B);
// ASM_END(1B): Key-Index(2B), Value-Index(2B), Key-Index(2B), Value-Index(2B);
//
// A pair of "Value-Index"s is for a pair of key-value, zero marks the end.
//
//
// tuple:
// ASM_VALUE(1B): DAO_TUPLE(1B), SubTypeID(1B), Type-Index(2B), Size(2B), Value-Index(2B);
//   ASM_DATA(1B); Value-Index(2B), Value-Index(2B), Value-Index(2B), Value-Index(2B);
// ASM_END(1B): Value-Index(2B), Value-Index(2B), Value-Index(2B), Value-Index(2B);
//
//
// concrete interface value:
// ASM_VALUE(1B): DAO_CINVALUE(1B), SubTypeID(1B), CinType-Index(2B), Value-Index(2B), Zeros(2B);
// ASM_END(1B): Zeros(8B);
//
//
// namevalue:
// ASM_VALUE(1B): DAO_PAR_NAMED(1B), Zeros(1B), Name-Index(2B), Value-Index(2B), Type-Index(2B);
// ASM_END(1B): Zeros(8B);
// 
//
// specialized ctype:
// ASM_VALUE(1B): DAO_CTYPE(1B), Zeros(1B), Value-Index(2B), Type-Index(2B) X 2;
//   ASM_DATA(1B): Type-Index(2B) X 4;
// ASM_END(1B): Type-Index(2B) X 4;
//
//
//#############
// Other Values
//#############
//
// copied value:
// ASM_COPY(1B): Value-Index(2B), Zeros(6B);
//
// type of a value:
// ASM_TYPEOF(1B): Value-Index(2B), Zeros(6B);
//
// const/invar/var type:
// ASM_AUXTYPE(1B): Type-Index(2B), SubType(2B), Zeros(4B);
//
// type alias:
// ASM_TYPEDEF(1B): Name-Index(2B), Type-Index(2B), RecurType(2B), Permision(2B);
//
// namespace:
// ASM_NAMESPACE(1B): Name/Def-Index(2B), ScopeNS-Index(2B), Zeros(4B);
//
//
//#########
// Blocks:
//#########
//
// routine:
// ASM_ROUTINE(1B): Name-Index(2B), Type-Index(2B), Host-Index(2B), Attrib(2B);
//   ...
// ASM_END: RegCount(2B), UpValCount(2B), Zeros(2B), DefaultConstructor(1B), Permission(1B);
//
//
// class:
// ASM_CLASS(1B): Name/Decl-Index(2B), Parent-Index(2B), Attrib(4B);
//   ASM_BASES(1B): Value-Index(2B), Value-Index(2B), Value-Index(2B), Value-Index(2B);
//   ...
// ASM_END(1B): LineDef(2B), Zeros(5B), Permission(1B);
//
//
// interface:
// ASM_INTERFACE(1B): Name/Decl-Index(2B), Target-TypeID(2B), Parent-Count(2B), Zeros(2B);
//   ASM_BASES(1B): Value-Index(2B), Value-Index(2B), Value-Index(2B), Value-Index(2B);
//   ...
// ASM_END(1B): LineDef(2B), Zeros(5B), Permission(1B);
//
//
// enum:
// ASM_ENUM(1B): Name-Index(2B), SubType(2B), Count(4B);
//   ASM_DATA(1B): Name-Index(2B), Value(4B), Zeros(2B);
// ASM_END(1B): Name-Index(2B), Value(4B), Zeros(2B);
//
//
// type:
// ASM_TYPE(1B): Name-Index(2B), TypeID(2B), Aux-Index(2B), CodeBlockType-Index(2B);
//   ASM_DATA(1B): Type-Index(2B) X 4;
// ASM_END(1B): Type-Index(2B) X 4;
//
// Note 1: the nested types are zero Type-Index terminated;
// Note 2: "Aux-Index" could be index to returned type or class block etc;
//
// value:
// See above;
//
//
// evaluation:
// ASM_EVAL(1B): Opcode(2B), OpB(2B), Mode(2B), Type-Index(2B);
//   ASM_DATA(1B): Value-Index(2B), Value-Index(2B), Value-Index(2B), Value-Index(2B);
// ASM_END(1B): Value-Index(2B), Value-Index(2B), Value-Index(2B), Value-Index(2B);
//
//
// bases (mixin components or interface parents):
// ASM_BASES(1B): Value-Index(2B), Value-Index(2B), Value-Index(2B), Value-Index(2B);
//   ASM_DATA(1B): Value-Index(2B) X 4;
// ASM_END(1B): Value-Index(2B) X 4;
//
//
// decorators for the current routine:
// ASM_DECOS(1B): Func-Index(2B), ParList-Index(2B), Func-Index(2B), ParList-Index(2B);
//   ASM_DATA(1B): Func-Index(2B), ParList-Index(2B), Func-Index(2B), ParList-Index(2B);
// ASM_END(1B): Func-Index(2B), ParList-Index(2B), Func-Index(2B), ParList-Index(2B);
//
//
// patterns for automatic decorator application:
// ASM_PATTERNS(1B): PatternString-Index(2B) X 4;
//   ASM_DATA(1B): PatternString-Index(2B) X 4;
// ASM_END(1B): PatternString-Index(2B) X 4;
//
//
// consts:
// ASM_CONSTS(1B): Count(2B), Value-Index(2B), Value-Index(2B), Value-Index(2B);
//   ASM_DATA(1B): Value-Index(2B), Value-Index(2B), Value-Index(2B), Value-Index(2B);
// ASM_END(1B): Value-Index(2B), Value-Index(2B), Value-Index(2B), Value-Index(2B);
//
//
// types:
// ASM_TYPES(1B): Count(2B), Zeros(2B), Var-Index(2B), Type-Index(2B);
//   ASM_DATA(1B): Var-Index(2B), Type-Index(2B), Var-Index(2B), Type-Index(2B);
// ASM_END(1B): Var-Index(2B), Type-Index(2B), Var-Index(2B), Type-Index(2B);
//
//
// code:
// ASM_CODE(1B): CodeNum(2B), Line-Num-Count(2B), LineNum(2B), Count(2B);
//   ASM_DATA(1B): LineDiff(2B), Count(2B), LineDiff(2B), Count(2B);
//   ASM_DATA(1B): Opcode(2B), A(2B), B(2B), C(2B);
// ASM_END(1B): Opcode(2B), A(2B), B(2B), C(2B);
//
//
//###########
// Statement:
//###########
//
// load statement:
// ASM_LOAD(1B): File-Path-Index(2B), Optional-Name-Index(2B), Zeros(4B);
//
// export to namespace:
// ASM_EXPORT(1B): NS-Index(2B), Name-Index(2B), Name-Index(2B), Name-Index(2B);
//
// import from namespace/module:
// ASM_IMPORT(1B): Mod-Index(2B), Name-Index(2B), Scope(2B), Offset(2B);
//
// verbatim:
// ASM_VERBATIM(1B): Tag-Index(2B), Mode-Index(2B), Text-Index(2B), LineNum(2B);
//
// var declaration:
// ASM_VAR(1B): Name-Index(2B), Value-Index(2B), Type-Index(2B), Scope(1B), Perm(1B);
//
// const declaration:
// ASM_CONST(1B): Name-Index(2B), Value-Index(2B), Zeros(2B), Scope(1B), Permission(1B);
//
// static declaration:
// ASM_STATIC(1B): Name-Index(2B), Value-Index(2B), Type-Index(2B), Scope(1B), Perm(1B);
//
// global declaration:
// ASM_GLOBAL(1B): Name-Index(2B), Value-Index(2B), Type-Index(2B), Scope(1B), Perm(1B);
// 
// seek:
// ASM_SEEK(1B): New-Index(2B), Zeros(6B);
//
//##########################################################################
//
// Note:
//
// 1. Constant folding:
//    If a constant folding involves NO function call and NONE user defined
//    types, encode the value directly; Otherwise, encode the expressions
//    the produce the value;
//
//    Because a function call may change the environment, the only way
//    to ensure reproducing an identical (equivalent) one is to make the
//    calls; And the only way to obtain a meaningful object of a user defined
//    type is to evaluate the leaves of the expression, which may make
//    it easier or possible to get the object.
//
// 2. Global constant and variables (and class constants and statics):
//    Instructions that access these data should be encoded with the
//    constant/variable's lookup names; If no lookup name is available,
//    automatically generate a unique one;
//
//##########################################################################
*/
enum DaoAuxOpcode
{
	DAO_ASM_NONE      ,
	DAO_ASM_LOAD      ,
	DAO_ASM_COPY      ,
	DAO_ASM_TYPEOF    ,
	DAO_ASM_TYPEDEF   ,
	DAO_ASM_AUXTYPE ,
	DAO_ASM_NAMESPACE ,
	DAO_ASM_ROUTINE   ,
	DAO_ASM_CLASS     ,
	DAO_ASM_INTERFACE ,
	DAO_ASM_ENUM      ,
	DAO_ASM_TYPE      ,
	DAO_ASM_VALUE     ,
	DAO_ASM_EVAL      ,
	DAO_ASM_BASES     ,
	DAO_ASM_DECOS     ,
	DAO_ASM_PATTERNS  ,
	DAO_ASM_CONSTS    ,
	DAO_ASM_TYPES     ,
	DAO_ASM_CODE      ,
	DAO_ASM_END       ,
	DAO_ASM_EXPORT    ,
	DAO_ASM_IMPORT    ,
	DAO_ASM_VERBATIM  ,
	DAO_ASM_CONST     ,
	DAO_ASM_STATIC    ,
	DAO_ASM_GLOBAL    ,
	DAO_ASM_VAR       ,
	DAO_ASM_DATA      ,
	DAO_ASM_DATA2     ,
	DAO_ASM_SEEK      ,
	DAO_ASM_INVALID
};



typedef struct DaoByteCoder  DaoByteCoder;
typedef struct DaoByteBlock  DaoByteBlock;


struct DaoByteBlock
{
	uint_t   type  : 8;
	uint_t   index : 24;

	uchar_t  begin[8];
	uchar_t  end[8];

	DMap_(uchar_t*,DaoByteBlock*)    *wordToBlocks;
	DHash_(DaoValue*,DaoByteBlock*)  *valueDataBlocks;   /* Same data to same block; */
	DHash_(DaoValue*,DaoByteBlock*)  *valueObjectBlocks; /* Same object to same block; */

	DaoValue  *value;

	DaoByteCoder   *coder;

	DaoByteBlock  *parent;

	/* Children blocks: */
	DaoByteBlock  *first;
	DaoByteBlock  *last;

	/* Sibling blocks: */
	DaoByteBlock  *prev;
	DaoByteBlock  *next;
};

struct DaoByteCoder
{
	uint_t   index;
	uint_t   fmthash;
	uchar_t  intSize;
	uchar_t  floatSize;
	uchar_t  error;

	DaoByteBlock  *top;

	DString  *path;

	DHash_(DaoValue*,DaoByteBlock*)  *valueDataBlocks;   /* Same data to same block; */
	DHash_(DaoValue*,DaoByteBlock*)  *valueObjectBlocks; /* Same object to same block; */

	DList_(DaoByteBlock*)  *stack;
	DList_(DaoByteBlock*)  *caches;
	DList_(DaoByteBlock*)  *iblocks;
	DList_(DaoValue*)      *ivalues;
	DList_(daoint)         *indices;
	DList_(daoint)         *lines;

	DList_(DaoRoutine*)    *routines;

	DaoNamespace  *nspace;
	DaoVmSpace    *vmspace;
};

DaoByteBlock* DaoByteBlock_New( DaoByteCoder *coder );
void DaoByteBlock_Delete( DaoByteBlock *self );

DaoByteCoder* DaoByteCoder_New( DaoVmSpace *vms );
void DaoByteCoder_Delete( DaoByteCoder *self );

void DaoByteCoder_Reset( DaoByteCoder *self );

DaoByteBlock* DaoByteCoder_Init( DaoByteCoder *self );
DaoByteBlock* DaoByteCoder_NewBlock( DaoByteCoder *self, int type );

DaoByteBlock* DaoByteBlock_NewBlock( DaoByteBlock *self, int type );
DaoByteBlock* DaoByteBlock_FindObjectBlock( DaoByteBlock *self, DaoValue *value );
DaoByteBlock* DaoByteBlock_AddBlock( DaoByteBlock *self, DaoValue *value, int type );

DaoByteBlock* DaoByteBlock_AddNamespace( DaoByteBlock *self, DaoNamespace *ns, DString *name, DaoNamespace *defOrScope );
DaoByteBlock* DaoByteBlock_AddRoutineBlock( DaoByteBlock *self, DaoRoutine *routine, int pm );
DaoByteBlock* DaoByteBlock_AddClassBlock( DaoByteBlock *self, DaoClass *klass, int pm );
DaoByteBlock* DaoByteBlock_AddInterfaceBlock( DaoByteBlock *self, DaoInterface *inter, int pm );
DaoByteBlock* DaoByteBlock_AddCinTypeBlock( DaoByteBlock *self, DaoCinType *cintype, int pm );
DaoByteBlock* DaoByteBlock_AddEvalBlock( DaoByteBlock *self, DaoValue *value, int code, int opb, int mode, DaoType *type );

void DaoByteCoder_FinalizeRoutineBlock( DaoByteCoder *self, DaoByteBlock *block );

void DaoByteBlock_InsertBlockIndex( DaoByteBlock *self, uchar_t *code, DaoByteBlock *block );

void DaoByteCoder_EncodeUInt32( uchar_t *data, uint_t value );

DaoByteBlock* DaoByteBlock_EncodeString( DaoByteBlock *self, DString *string );
DaoByteBlock* DaoByteBlock_EncodeType( DaoByteBlock *self, DaoType *type );
DaoByteBlock* DaoByteBlock_EncodeValue( DaoByteBlock *self, DaoValue *value );
DaoByteBlock* DaoByteBlock_EncodeCtype( DaoByteBlock *self, DaoCtype *ctype, DaoCtype *generic, DaoType **types, int n );
DaoByteBlock* DaoByteBlock_EncodeTypeAlias( DaoByteBlock *self, DaoType *type, DaoType *aliased, DString *alias, DaoType *rectype, int perm );
DaoByteBlock* DaoByteBlock_EncodeTypeOf( DaoByteBlock *self, DaoType *type, DaoValue *value );
DaoByteBlock* DaoByteBlock_EncodeLoad( DaoByteBlock *self, DaoNamespace *mod, DString *modname, DString *asname );
DaoByteBlock* DaoByteBlock_EncodeExport( DaoByteBlock *self, DaoNamespace *ns, DString *names[3] );
DaoByteBlock* DaoByteBlock_EncodeImport( DaoByteBlock *self, DaoValue *mod, DString *name, int scope, int index );
DaoByteBlock* DaoByteBlock_EncodeSeekStmt( DaoByteBlock *self, DaoByteBlock *target );
DaoByteBlock* DaoByteBlock_EncodeVerbatim( DaoByteBlock *self, DString *tag, DString *mode, DString *text, int line );
DaoByteBlock* DaoByteBlock_EncodeDecorators( DaoByteBlock *self, DList *decos, DList *pars );

DaoByteBlock* DaoByteBlock_Declare( DaoByteBlock *self, int tag, DString *name, DaoValue *value, DaoType *type, int perm );
DaoByteBlock* DaoByteBlock_DeclareConst( DaoByteBlock *self, DString *name, DaoValue *value, int perm );
DaoByteBlock* DaoByteBlock_DeclareVar( DaoByteBlock *self, DString *name, DaoValue *value, DaoType *type, int perm );
DaoByteBlock* DaoByteBlock_DeclareStatic( DaoByteBlock *self, DString *name, DaoValue *value, DaoType *type, int level, int id );
DaoByteBlock* DaoByteBlock_DeclareGlobal( DaoByteBlock *self, DString *name, DaoValue *value, DaoType *type, int perm );

DaoByteBlock* DaoByteBlock_EncodeInteger( DaoByteBlock *self, DaoInteger *value );
DaoByteBlock* DaoByteBlock_EncodeFloat( DaoByteBlock *self, DaoFloat *value );
DaoByteBlock* DaoByteBlock_EncodeComplex( DaoByteBlock *self, DaoComplex *value );
DaoByteBlock* DaoByteBlock_EncodeEnum( DaoByteBlock *self, DaoEnum *value );

DaoByteBlock* DaoByteBlock_EncodeArray( DaoByteBlock *self, DaoArray *value );
DaoByteBlock* DaoByteBlock_EncodeList( DaoByteBlock *self, DaoList *value );

DaoByteBlock* DaoByteBlock_EncodeValue( DaoByteBlock *self, DaoValue *value );

void DaoByteBlock_EncodeValues( DaoByteBlock *self, DaoValue **values, int count );
int DaoByteBlock_EncodeValues2( DaoByteBlock *self, DList *values );
void DaoByteBlock_AddBlockIndexData( DaoByteBlock *self, int head, int size );

void DaoByteCoder_EncodeHeader( DaoByteCoder *self, const char *fname, DString *output );
void DaoByteCoder_EncodeToString( DaoByteCoder *self, DString *output );
void DaoByteCoder_Disassemble( DaoByteCoder *self );


int DaoByteCoder_Decode( DaoByteCoder *self, DString *source );
int DaoByteCoder_Build( DaoByteCoder *self, DaoNamespace *nspace );

typedef void (*DaoByteCodeEncrypt)( DString *data, int aux );
typedef void (*DaoByteCodeDecrypt)( DString *data, int aux );

#endif
