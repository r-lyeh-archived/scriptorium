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

#include <math.h>
#include <string.h>
#include <ctype.h>

#include "daoBytecode.h"
#include "daoOptimizer.h"
#include "daoNamespace.h"
#include "daoVmspace.h"
#include "daoValue.h"
#include "daoGC.h"


extern DaoByteCodeEncrypt dao_bytecode_encrypt;
extern DaoByteCodeDecrypt dao_bytecode_decrypt;

DaoByteCodeEncrypt dao_bytecode_encrypt = NULL;
DaoByteCodeDecrypt dao_bytecode_decrypt = NULL;


static const char* const dao_asm_names[] =
{
	"ASM_NONE"      ,
	"ASM_LOAD"      ,
	"ASM_COPY"      ,
	"ASM_TYPEOF"    ,
	"ASM_TYPEDEF"   ,
	"ASM_AUXTYPE" ,
	"ASM_NAMESPACE" ,
	"ASM_ROUTINE"   ,
	"ASM_CLASS"     ,
	"ASM_INTERFACE" ,
	"ASM_ENUM"      ,
	"ASM_TYPE"      ,
	"ASM_VALUE"     ,
	"ASM_EVAL"      ,
	"ASM_BASES"     ,
	"ASM_DECOS"     ,
	"ASM_PATTERNS"  ,
	"ASM_CONSTS"    ,
	"ASM_TYPES"     ,
	"ASM_CODE"      ,
	"ASM_END"       ,
	"ASM_EXPORT"    ,
	"ASM_IMPORT"    ,
	"ASM_VERBATIM"  ,
	"ASM_CONST"     ,
	"ASM_STATIC"    ,
	"ASM_GLOBAL"    ,
	"ASM_VAR"       ,
	"ASM_DATA"      ,
	"ASM_DATA2"     ,
	"ASM_SEEK"      ,
	"ASM_INVALID"
};
const char* DaoByteCoder_GetASM( unsigned tag )
{
	return dao_asm_names[ tag > DAO_ASM_INVALID ? DAO_ASM_INVALID : tag ];
}
uint_t DaoRotatingHash( DString *text )
{
	int i, len = text->size;
	uint_t hash = text->size;
	for(i=0; i<len; ++i) hash = ((hash<<4)^(hash>>28)^text->chars[i])&0x7fffffff;
	return hash;
}

DaoByteBlock* DaoByteBlock_New( DaoByteCoder *coder )
{
	DaoByteBlock *self = (DaoByteBlock*) dao_calloc(1,sizeof(DaoByteBlock));
	self->coder = coder;
	return self;
}
void DaoByteBlock_Delete( DaoByteBlock *self )
{
	GC_DecRC( self->value );
	if( self->wordToBlocks ) DMap_Delete( self->wordToBlocks );
	if( self->valueDataBlocks ) DMap_Delete( self->valueDataBlocks );
	if( self->valueObjectBlocks ) DMap_Delete( self->valueObjectBlocks );
	dao_free( self );
}
void DaoByteBlock_MoveToBack( DaoByteBlock *self, DaoByteBlock *block )
{
	if( block == self->last ) return;
	if( block->prev ) block->prev->next = block->next;
	if( block->next ) block->next->prev = block->prev;
	if( self->first == block ) self->first = block->next;
	if( self->last != block ){
		block->prev = self->last;
		self->last->next = block;
	}
	self->last = block;
	block->next = NULL;
}


DaoByteCoder* DaoByteCoder_New( DaoVmSpace *vms )
{
	int i;
	char buf[64];
	DString *format = DString_New();
	DaoByteCoder *self = (DaoByteCoder*) dao_calloc(1,sizeof(DaoByteCoder));
	self->valueDataBlocks  = DHash_New( DAO_DATA_VALUE2, 0 );
	self->valueObjectBlocks = DHash_New( DAO_DATA_VALUE3, 0 );
	self->caches = DList_New(0);
	self->stack = DList_New(0);
	self->lines = DList_New(0);
	self->ivalues = DList_New(0);
	self->iblocks = DList_New(0);
	self->indices = DList_New(0);
	self->routines = DList_New(0);
	self->path = DString_New();
	self->intSize = sizeof(dao_integer);
	self->floatSize = sizeof(dao_float);
	self->vmspace = vms;
	for(i=0; i<DAO_ASM_INVALID; ++i){
		snprintf( buf, sizeof(buf), "%i:%s;", i, dao_asm_names[i] );
		DString_AppendChars( format, buf );
	}
	DString_AppendChars( format, " " );
	for(i=0; i<DVM_NULL; ++i){
		snprintf( buf, sizeof(buf), "%i:%s;", i, DaoVmCode_GetOpcodeName(i) );
		DString_AppendChars( format, buf );
	}
	self->fmthash = DaoRotatingHash( format );
	DString_Delete( format );
	return self;
}
void DaoByteCoder_Remove( DaoByteCoder *self, DaoByteBlock *block, DaoByteBlock *parent )
{
	DaoByteBlock *it = block->first;
	while( it ){
		DaoByteBlock *b = it;
		it = it->next;
		DaoByteCoder_Remove( self, b, block );
	}
	if( block->prev ) block->prev->next = block->next;
	if( block->next ) block->next->prev = block->prev;
	if( parent && parent->first == block ) parent->first = block->next;
	if( parent && parent->last == block ) parent->last = block->prev;
	block->parent = NULL;
	block->first = block->last = NULL;
	block->prev = block->next = NULL;
	GC_DecRC( block->value );
	block->value = NULL;
	if( block->valueDataBlocks ) DMap_Reset( block->valueDataBlocks );
	if( block->valueObjectBlocks ) DMap_Reset( block->valueObjectBlocks );
	DList_Append( self->caches, block );
}
void DaoByteCoder_Reset( DaoByteCoder *self )
{
	if( self->top ) DaoByteCoder_Remove( self, self->top, NULL );
	self->error = 0;
	self->top = NULL;
	DList_Clear( self->stack );
	DMap_Reset( self->valueDataBlocks );
	DMap_Reset( self->valueObjectBlocks );
}
void DaoByteCoder_Delete( DaoByteCoder *self )
{
	int i, n;
	DaoByteCoder_Reset( self );
	for(i=0,n=self->caches->size; i<n; ++i){
		DaoByteBlock *block = (DaoByteBlock*) self->caches->items.pVoid[i];
		DaoByteBlock_Delete( block );
	}
	DList_Delete( self->caches );
	DList_Delete( self->stack );
	DList_Delete( self->lines );
	DList_Delete( self->ivalues );
	DList_Delete( self->iblocks );
	DList_Delete( self->indices );
	DList_Delete( self->routines );
	DString_Delete( self->path );
	DMap_Delete( self->valueDataBlocks );
	DMap_Delete( self->valueObjectBlocks );
	dao_free( self );
}

DaoByteBlock* DaoByteCoder_Init( DaoByteCoder *self )
{
	DaoByteCoder_Reset( self );
	self->top = DaoByteCoder_NewBlock( self, DAO_ASM_ROUTINE );
	return self->top;
}

DaoByteBlock* DaoByteCoder_NewBlock( DaoByteCoder *self, int type )
{
	DaoByteBlock *block = (DaoByteBlock*) DList_PopBack( self->caches );
	if( block == NULL ) block = DaoByteBlock_New( self );
	if( block->wordToBlocks ) DMap_Reset( block->wordToBlocks );
	block->type = type;
	memset( block->begin, 0, 8 );
	memset( block->end, 0, 8 );
	return block;
}

DaoByteBlock* DaoByteBlock_NewBlock( DaoByteBlock *self, int type )
{
	DaoByteBlock *block = DaoByteCoder_NewBlock( self->coder, type );
	block->parent = self;
	if( self->last == NULL ){
		self->last = self->first = block;
	}else{
		block->prev = self->last;
		self->last->next = block;
		self->last = block;
	}
	return block;
}

DaoByteBlock* DaoByteBlock_FindDataBlock( DaoByteBlock *self, DaoValue *value )
{
	DNode *it;
	if( value == NULL ) return NULL;
	if( self->valueDataBlocks ){
		it = DMap_Find( self->valueDataBlocks, value );
		if( it ) return (DaoByteBlock*) it->value.pVoid;
	}
	it = DMap_Find( self->coder->valueDataBlocks, value );
	if( it ) return (DaoByteBlock*) it->value.pVoid;
	return NULL;
}
DaoByteBlock* DaoByteBlock_FindObjectBlock( DaoByteBlock *self, DaoValue *value )
{
	DNode *it;
	if( value == NULL ) return NULL;
	if( self->valueObjectBlocks ){
		it = DMap_Find( self->valueObjectBlocks, value );
		if( it ) return (DaoByteBlock*) it->value.pVoid;
	}
	it = DMap_Find( self->coder->valueObjectBlocks, value );
	if( it ) return (DaoByteBlock*) it->value.pVoid;
	return NULL;
}
static void DaoByteBlock_SetValue( DaoByteBlock *self, DaoValue *value )
{
	if( value && value->type <= DAO_ENUM ){
		DaoValue_Copy( value, & self->value );
		return;
	}
	GC_Assign( & self->value, value );
}
DaoByteBlock* DaoByteBlock_FindOrCopyBlock( DaoByteBlock *self, DaoValue *value )
{
	DaoByteBlock *newbk, *block = DaoByteBlock_FindObjectBlock( self, value );
	if( block ) return block;
	block = DaoByteBlock_FindDataBlock( self, value );
	if( block == NULL ) return NULL;
	newbk = DaoByteBlock_NewBlock( self, DAO_ASM_COPY );
	if( self->valueObjectBlocks == NULL ) self->valueObjectBlocks = DHash_New( DAO_DATA_VALUE3, 0 );
	DaoByteBlock_SetValue( newbk, value );
	DMap_Insert( self->coder->valueObjectBlocks, value, newbk );
	DMap_Insert( self->valueObjectBlocks, value, newbk );
	DaoByteBlock_InsertBlockIndex( newbk, newbk->begin, block );
	return newbk;
}
DaoByteBlock* DaoByteBlock_AddBlock( DaoByteBlock *self, DaoValue *value, int type )
{
	DaoByteBlock *block = DaoByteBlock_NewBlock( self, type );
	if( self->valueDataBlocks == NULL ) self->valueDataBlocks = DHash_New( DAO_DATA_VALUE2, 0 );
	if( self->valueObjectBlocks == NULL ) self->valueObjectBlocks = DHash_New( DAO_DATA_VALUE3, 0 );
	DaoByteBlock_SetValue( block, value );
	DMap_Insert( self->coder->valueDataBlocks, value, block );
	DMap_Insert( self->coder->valueObjectBlocks, value, block );
	DMap_Insert( self->valueDataBlocks, value, block );
	DMap_Insert( self->valueObjectBlocks, value, block );
	return block;
}



void DaoByteCoder_EncodeUInt16( uchar_t *data, uint_t value )
{
	data[0] = (value >> 8) & 0xFF;
	data[1] = value & 0xFF;
}
void DaoByteCoder_EncodeUInt32( uchar_t *data, uint_t value )
{
	data[0] = (value >> 24) & 0xFF;
	data[1] = (value >> 16) & 0xFF;
	data[2] = (value >>  8) & 0xFF;
	data[3] = value & 0xFF;
}
void DaoByteCoder_EncodeDaoInt( uchar_t *data, dao_integer value )
{
	uchar_t i, m = sizeof(dao_integer);
	for(i=0; i<m; ++i) data[i] = (value >> 8*(m-1-i)) & 0xFF;
}
/*
// IEEE 754 double-precision binary floating-point format:
//   sign(1)--exponent(11)------------fraction(52)---------------------
//   S EEEEEEEEEEE FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
//   63         52                                                    0
//
//   value = (-1)^S  *  ( 1 + \sigma_0^51 (b_i * 2^{-(52-i)}) )  *  2^{E-1023}
//
// Exponents 0x000 is used to represent zero (if F=0) and subnormals (if F!=0);
// Exponents 0x7FF is used to represent inf (if F=0) and NaNs (if F!=0);
// Where F is the fraction mantissa.
*/
static int DaoByteCoder_BigEndianFloat()
{
	double inf = INFINITY;
	uchar_t *bytes = (uchar_t*) & inf;
	return bytes[0] != 0;
}
static void DaoByteCoder_EncodeFloat( uchar_t *data, double value )
{
	int i;
	uchar_t *bytes = (uchar_t*) & value;
	if( DaoByteCoder_BigEndianFloat() ){
		for(i=0; i<8; ++i) data[i] = bytes[i];
	}else{
		for(i=0; i<8; ++i) data[i] = bytes[7-i];
	}
}
uint_t DaoByteCoder_DecodeUInt8( uchar_t *data )
{
	return data[0];
}
uint_t DaoByteCoder_DecodeUInt16( uchar_t *data )
{
	int value = (data[0]<<8) + data[1];
	data += 2;
	return value;
}
uint_t DaoByteCoder_DecodeUInt32( uchar_t *data )
{
	uint_t value = data[0] << 24;
	value += data[1] << 16;
	value += data[2] << 8;
	value += data[3];
	data += 4;
	return value;
}
dao_integer DaoByteCoder_DecodeDaoInt( DaoByteCoder *self, uchar_t *data )
{
	DaoStream *stream = self->vmspace->errorStream;
	uchar_t i, m = self->intSize;
	dao_integer value = 0;

	if( self->intSize > sizeof(dao_integer) ){ /* self->intSize=8, sizeof(dao_integer)=4 */
		dao_integer B1 = data[0], B2 = data[1], B3 = data[2], B4 = data[3];
		dao_integer B5 = data[4], B6 = data[5], B7 = data[6], B8 = data[7];

		if( (B1 == 0x7F || B1 == 0xFF) && B2 == 0xFF && B3 == 0xFF && B4 == 0xFF ){
			if( B5 & 0x80 ) goto TooBigInteger;
			if( B1 == 0xFF ) B5 |= 0x80;
		}else if( B1 || B2 || B3 || B4 ){
			goto TooBigInteger;
		}
		return (B5<<24)|(B6<<16)|(B7<<8)|B8;
	}else if( self->intSize < sizeof(dao_integer) ){ /* self->intSize=4, sizeof(dao_integer)=8 */
		dao_integer B1 = data[0], B2 = data[1], B3 = data[2], B4 = data[3];

		if( B1 & 0x80 ){
			dao_integer leading = (0xFF<<24)|(0xFF<<16)|(0xFF<<8)|0xFF;
			dao_integer shift = 32; /* just to avoid a warning on 32 bit systems; */
			return (leading<<shift)|(0xFF<<24)|((B1&0x7F)<<24)|(B2<<16)|(B3<<8)|B4;
		}
		return (B1<<24)|(B2<<16)|(B3<<8)|B4;
	}

	for(i=0; i<m; ++i) value |= ((dao_integer)data[i]) << 8*(m-1-i);
	return value;
TooBigInteger:
	DaoStream_WriteChars( stream, "Error: too big integer value for the platform!" );
	return 0;
}
/*
// IEEE 754 double-precision binary floating-point format:
//   sign(1)--exponent(11)------------fraction(52)---------------------
//   S EEEEEEEEEEE FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
//   63         52                                                    0
//
//   value = (-1)^S  *  ( 1 + \sigma_0^51 (b_i * 2^{-(52-i)}) )  *  2^{E-1023}
*/
double DaoByteCoder_DecodeFloat( DaoByteCoder *self, uchar_t *data )
{
	int i;
	uchar_t bytes[8];
	if( DaoByteCoder_BigEndianFloat() ) return *(double*) data;
	for(i=0; i<8; ++i) bytes[i] = data[7-i];
	return *(double*) bytes;
}
void DaoByteCoder_DecodeChunk2222( uchar_t *data, uint_t *A, uint_t *B, uint_t *C, uint_t *D )
{
	*A = DaoByteCoder_DecodeUInt16( data + 0 );
	*B = DaoByteCoder_DecodeUInt16( data + 2 );
	*C = DaoByteCoder_DecodeUInt16( data + 4 );
	*D = DaoByteCoder_DecodeUInt16( data + 6 );
}
void DaoByteCoder_DecodeChunk2114( uchar_t *data, uint_t *A, uint_t *B, uint_t *C, uint_t *D )
{
	*A = DaoByteCoder_DecodeUInt16( data + 0 );
	*B = DaoByteCoder_DecodeUInt8( data + 2 );
	*C = DaoByteCoder_DecodeUInt8( data + 3 );
	*D = DaoByteCoder_DecodeUInt32( data + 4 );
}
void DaoByteCoder_DecodeChunk224( uchar_t *data, uint_t *A, uint_t *B, uint_t *C )
{
	*A = DaoByteCoder_DecodeUInt16( data + 0 );
	*B = DaoByteCoder_DecodeUInt16( data + 2 );
	*C = DaoByteCoder_DecodeUInt32( data + 4 );
}
void DaoByteCoder_DecodeSubChunk222( uchar_t *data, uint_t *A, uint_t *B, uint_t *C )
{
	*A = DaoByteCoder_DecodeUInt16( data + 0 );
	*B = DaoByteCoder_DecodeUInt16( data + 2 );
	*C = DaoByteCoder_DecodeUInt16( data + 4 );
}
void DaoByteCoder_DecodeSubChunk114( uchar_t *data, uint_t *A, uint_t *B, uint_t *C )
{
	*A = DaoByteCoder_DecodeUInt8( data + 0 );
	*B = DaoByteCoder_DecodeUInt8( data + 1 );
	*C = DaoByteCoder_DecodeUInt32( data + 2 );
}
void DaoByteCoder_DecodeSubChunk24( uchar_t *data, uint_t *A, uint_t *B )
{
	*A = DaoByteCoder_DecodeUInt16( data + 0 );
	*B = DaoByteCoder_DecodeUInt32( data + 2 );
}


DaoByteBlock* DaoByteBlock_EncodeBoolean( DaoByteBlock *self, DaoBoolean *value )
{
	DaoByteBlock *block = DaoByteBlock_FindOrCopyBlock( self, (DaoValue*) value );
	if( block ) return block;
	block = DaoByteBlock_AddBlock( self, (DaoValue*) value, DAO_ASM_VALUE );
	block->begin[0] = DAO_BOOLEAN;
	block->end[0] = value->value;
	return block;
}
DaoByteBlock* DaoByteBlock_EncodeInteger( DaoByteBlock *self, DaoInteger *value )
{
	DaoByteBlock *block = DaoByteBlock_FindOrCopyBlock( self, (DaoValue*) value );
	if( block ) return block;
	block = DaoByteBlock_AddBlock( self, (DaoValue*) value, DAO_ASM_VALUE );
	block->begin[0] = DAO_INTEGER;
	DaoByteCoder_EncodeDaoInt( block->end, value->value );
	return block;
}
DaoByteBlock* DaoByteBlock_EncodeFloat( DaoByteBlock *self, DaoFloat *value )
{
	DaoByteBlock *block = DaoByteBlock_FindOrCopyBlock( self, (DaoValue*) value );
	if( block ) return block;
	block = DaoByteBlock_AddBlock( self, (DaoValue*) value, DAO_ASM_VALUE );
	block->begin[0] = DAO_FLOAT;
	DaoByteCoder_EncodeFloat( block->end, value->value );
	return block;
}
DaoByteBlock* DaoByteBlock_EncodeComplex( DaoByteBlock *self, DaoComplex *value )
{
	DaoByteBlock *block2, *block = DaoByteBlock_FindOrCopyBlock( self, (DaoValue*) value );
	if( block ) return block;
	block = DaoByteBlock_AddBlock( self, (DaoValue*) value, DAO_ASM_VALUE );
	block2 = DaoByteBlock_NewBlock( block, DAO_ASM_DATA );
	block->begin[0] = DAO_COMPLEX;
	DaoByteCoder_EncodeFloat( block2->begin, value->value.real );
	DaoByteCoder_EncodeFloat( block->end, value->value.imag );
	return block;
}
DaoByteBlock* DaoByteBlock_EncodeString( DaoByteBlock *self, DString *string )
{
	DaoByteBlock *block;
	DaoString daostring = {DAO_STRING,0,0,0,1,NULL};
	DaoValue *value = (DaoValue*) & daostring;
	int i, j, size = string->size;

	daostring.value = string;
	block = DaoByteBlock_FindOrCopyBlock( self, value );
	if( block ) return block;
	block = DaoByteBlock_AddBlock( self, value, DAO_ASM_VALUE );
	block->begin[0] = DAO_STRING;
	block->begin[1] = string->size % 16;
	if( string->chars ){
		for(i=0; i<6 && i<size; ++i) block->begin[i+2] = string->chars[i];
		for(i=6; (i+8)<size; i+=8){
			DaoByteBlock *dataBlock = DaoByteBlock_NewBlock( block, DAO_ASM_DATA );
			memcpy( dataBlock->begin, string->chars+i, 8*sizeof(char) );
		}
		if( i < size ) memcpy( block->end, string->chars+i, (size-i)*sizeof(char) );
	}else{
	}
	return block;
}



static void DaoByteCoder_PrintBlock( DaoByteCoder *self, DaoByteBlock *block, int spaces, int error );

static void DaoByteCoder_Error( DaoByteCoder *self, DaoByteBlock *block, const char *msg )
{
	DaoStream_WriteChars( self->vmspace->errorStream, "ERROR: " );
	DaoStream_WriteChars( self->vmspace->errorStream, msg );
	DaoStream_WriteChars( self->vmspace->errorStream, "\n" );
	if( block ) DaoByteCoder_PrintBlock( self, block, 0, 1 );
	self->error = 1;
}
static void DaoByteCoder_Error2( DaoByteCoder *self, DaoByteBlock *block, const char *msg, int value )
{
	char buffer[256];
	snprintf( buffer, sizeof(buffer), msg, value );
	DaoByteCoder_Error( self, block, buffer );
}
static void DaoByteCoder_Error3( DaoByteCoder *self, DaoByteBlock *block, const char *msg, const char *value )
{
	char buffer[256];
	snprintf( buffer, sizeof(buffer), msg, value );
	DaoByteCoder_Error( self, block, buffer );
}

static void DaoByteBlock_CopyToEndFromBegin( DaoByteBlock *self, DaoByteBlock *other )
{
	int i;
	memcpy( self->end, other->begin, 8 );
	if( other->wordToBlocks == NULL ) return;
	if( other->wordToBlocks && self->wordToBlocks == NULL ) self->wordToBlocks = DMap_New(0,0);
	for(i=0; i<8; i+=2){
		DNode *it = DMap_Find( other->wordToBlocks, (other->begin + i) );
		if( it ) DMap_Insert( self->wordToBlocks, (self->end + i), it->value.pVoid );
	}
}
void DaoByteBlock_EncodeValues( DaoByteBlock *self, DaoValue **values, int count )
{
	int i;
	for(i=0; i<count; ++i){
		DaoByteBlock *block = DaoByteBlock_EncodeValue( self, values[i] );
		DList_Append( self->coder->stack, block );
	}
}
int DaoByteBlock_EncodeValues2( DaoByteBlock *self, DList *values )
{
	DaoByteBlock_EncodeValues( self, values->items.pValue, values->size );
	return values->size;
}
void DaoByteBlock_InsertBlockIndex( DaoByteBlock *self, uchar_t *code, DaoByteBlock *block )
{
	if( self->wordToBlocks == NULL ) self->wordToBlocks = DMap_New(0,0);
	DMap_Insert( self->wordToBlocks, code, block );
}
void DaoByteBlock_AddBlockIndexData( DaoByteBlock *self, int head, int size )
{
	DaoByteBlock *dataBlock = self;
	uchar_t *data = dataBlock->begin + 2*(4-head);
	int i, j, offset;
	if( size > self->coder->stack->size ) size = self->coder->stack->size;
	offset = self->coder->stack->size - size;
	for(i=0; i<size; ++i, data+=2){
		DaoByteBlock *block = (DaoByteBlock*) self->coder->stack->items.pVoid[i+offset];
		if( data >= dataBlock->begin + 8 ){
			dataBlock = DaoByteBlock_NewBlock( self, DAO_ASM_DATA );
			data = dataBlock->begin;
		}
		DaoByteBlock_InsertBlockIndex( dataBlock, data, block );
	}
	if( dataBlock != self ){
		DaoByteBlock_CopyToEndFromBegin( self, dataBlock );
		DaoByteCoder_Remove( self->coder, dataBlock, self );
	}
	DList_Erase( self->coder->stack, offset, size );
}



DaoByteBlock* DaoByteBlock_EncodeEnum( DaoByteBlock *self, DaoEnum *value )
{
	DaoByteBlock *typeBlock;
	DaoByteBlock *newBlock = DaoByteBlock_FindOrCopyBlock( self, (DaoValue*) value );
	
	if( newBlock ) return newBlock;

	typeBlock = DaoByteBlock_EncodeType( self, value->etype );
	newBlock = DaoByteBlock_AddBlock( self, (DaoValue*) value, DAO_ASM_VALUE );

	newBlock->begin[0] = DAO_ENUM;
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+2, typeBlock );
	DaoByteCoder_EncodeUInt32( newBlock->end, value->value );
	return newBlock;
}
DaoByteBlock* DaoByteBlock_EncodeArray( DaoByteBlock *self, DaoArray *value )
{
	int i;
	DaoByteBlock *databk;
	DaoByteBlock *block = DaoByteBlock_FindOrCopyBlock( self, (DaoValue*) value );
	if( block ) return block;
	block = DaoByteBlock_AddBlock( self, (DaoValue*) value, DAO_ASM_VALUE );
	block->begin[0] = DAO_ARRAY;
	block->begin[1] = value->etype;
	DaoByteCoder_EncodeUInt16( block->begin+2, value->ndim );
	DaoByteCoder_EncodeUInt32( block->begin+4, value->size );
	for(i=0; (i+2)<value->ndim; i+=2){
		databk = DaoByteBlock_NewBlock( block, DAO_ASM_DATA );
		DaoByteCoder_EncodeUInt32( databk->begin, value->dims[i] );
		DaoByteCoder_EncodeUInt32( databk->begin+4, value->dims[i+1] );
	}
	databk = DaoByteBlock_NewBlock( block, DAO_ASM_DATA );
	DaoByteCoder_EncodeUInt32( databk->begin, value->dims[i] );
	if( (i+1)<value->ndim ) DaoByteCoder_EncodeUInt32( databk->begin+4, value->dims[i+1] );
	if( value->etype == DAO_INTEGER && sizeof(dao_integer) == 8 ){
		for(i=0; (i+1)<value->size; i+=1){
			databk = DaoByteBlock_NewBlock( block, DAO_ASM_DATA );
			DaoByteCoder_EncodeDaoInt( databk->begin, value->data.i[i] );
		}
		if( i < value->size ) DaoByteCoder_EncodeDaoInt( block->end, value->data.i[i] );
	}else if( value->etype == DAO_INTEGER ){
		for(i=0; (i+2)<value->size; i+=2){
			databk = DaoByteBlock_NewBlock( block, DAO_ASM_DATA );
			DaoByteCoder_EncodeDaoInt( databk->begin, value->data.i[i] );
			DaoByteCoder_EncodeDaoInt( databk->begin+4, value->data.i[i+1] );
		}
		if( i < value->size ) DaoByteCoder_EncodeDaoInt( block->end, value->data.i[i] );
		if( (i+1)<value->size ) DaoByteCoder_EncodeDaoInt( block->end+4, value->data.i[i+1] );
	}else if( value->etype == DAO_FLOAT ){
		for(i=0; (i+1)<value->size; i+=1){
			databk = DaoByteBlock_NewBlock( block, DAO_ASM_DATA );
			DaoByteCoder_EncodeFloat( databk->begin, value->data.f[i] );
		}
		if( i < value->size ) DaoByteCoder_EncodeFloat( block->end, value->data.f[i] );
	}else if( value->etype == DAO_COMPLEX ){
		for(i=0; (i+1)<value->size; i+=1){
			DaoByteBlock *databk1 = DaoByteBlock_NewBlock( block, DAO_ASM_DATA );
			DaoByteBlock *databk2 = DaoByteBlock_NewBlock( block, DAO_ASM_DATA );
			DaoByteCoder_EncodeFloat( databk1->begin, value->data.c[i].real );
			DaoByteCoder_EncodeFloat( databk2->begin, value->data.c[i].imag );
		}
		if( i < value->size ){
			databk = DaoByteBlock_NewBlock( block, DAO_ASM_DATA );
			DaoByteCoder_EncodeFloat( databk->begin, value->data.c[i].real );
			DaoByteCoder_EncodeFloat( block->end, value->data.c[i].imag );
		}
	}
	return block;
}
DaoByteBlock* DaoByteBlock_EncodeList( DaoByteBlock *self, DaoList *value )
{
	DaoByteBlock *typeBlock;
	DaoByteBlock *newBlock = DaoByteBlock_FindOrCopyBlock( self, (DaoValue*) value );
	
	if( newBlock ) return newBlock;

	DaoByteBlock_EncodeValues2( self, value->value );
	typeBlock = DaoByteBlock_EncodeType( self, value->ctype );
	newBlock = DaoByteBlock_AddBlock( self, (DaoValue*) value, DAO_ASM_VALUE );

	newBlock->begin[0] = DAO_LIST;
	DaoByteCoder_EncodeUInt32( newBlock->begin+4, value->value->size );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+2, typeBlock );
	DaoByteBlock_AddBlockIndexData( newBlock, 0, value->value->size );
	return newBlock;
}
DaoByteBlock* DaoByteBlock_EncodeMap( DaoByteBlock *self, DaoMap *value )
{
	DNode *it;
	DaoByteBlock *typeBlock;
	DaoByteBlock *newBlock = DaoByteBlock_FindOrCopyBlock( self, (DaoValue*) value );
	
	if( newBlock ) return newBlock;

	for(it=DaoMap_First(value); it; it=DaoMap_Next(value,it)){
		DaoByteBlock *key = DaoByteBlock_EncodeValue( self, it->key.pValue );
		DaoByteBlock *value = DaoByteBlock_EncodeValue( self, it->value.pValue );
		DList_Append( self->coder->stack, key );
		DList_Append( self->coder->stack, value );
	}
	typeBlock = DaoByteBlock_EncodeType( self, value->ctype );
	newBlock = DaoByteBlock_AddBlock( self, (DaoValue*) value, DAO_ASM_VALUE );

	newBlock->begin[0] = DAO_MAP;
	DaoByteCoder_EncodeUInt32( newBlock->begin+4, value->value->hashing );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+2, typeBlock );
	DaoByteBlock_AddBlockIndexData( newBlock, 0, 2*value->value->size );
	return newBlock;
}
DaoByteBlock* DaoByteBlock_EncodeTuple( DaoByteBlock *self, DaoTuple *value )
{
	DaoByteBlock *typeBlock;
	DaoByteBlock *newBlock = DaoByteBlock_FindOrCopyBlock( self, (DaoValue*) value );
	
	if( newBlock ) return newBlock;

	DaoByteBlock_EncodeValues( self, value->values, value->size );
	typeBlock = DaoByteBlock_EncodeType( self, value->ctype );
	newBlock = DaoByteBlock_AddBlock( self, (DaoValue*) value, DAO_ASM_VALUE );

	newBlock->begin[0] = DAO_TUPLE;
	newBlock->begin[1] = value->subtype;
	DaoByteCoder_EncodeUInt16( newBlock->begin+4, value->size );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+2, typeBlock );
	DaoByteBlock_AddBlockIndexData( newBlock, 1, value->size );
	return newBlock;
}
DaoByteBlock* DaoByteBlock_EncodeCinValue( DaoByteBlock *self, DaoCinValue* value )
{
	DaoByteBlock *cintypeBlock = DaoByteBlock_FindOrCopyBlock( self, (DaoValue*) value->cintype );
	DaoByteBlock *newBlock = DaoByteBlock_FindOrCopyBlock( self, (DaoValue*) value );
	DaoByteBlock *valueBlock;
	
	if( newBlock ) return newBlock;

	valueBlock = DaoByteBlock_EncodeValue( self, value->value );
	newBlock = DaoByteBlock_AddBlock( self, (DaoValue*) value, DAO_ASM_VALUE );

	newBlock->begin[0] = DAO_CINVALUE;
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+2, cintypeBlock );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+4, valueBlock );
	return newBlock;
}
DaoByteBlock* DaoByteBlock_EncodeNameValue( DaoByteBlock *self, DaoNameValue* value )
{
	DaoByteBlock *typeBlock, *valueBlock, *nameBlock;
	DaoByteBlock *newBlock = DaoByteBlock_FindOrCopyBlock( self, (DaoValue*) value );
	
	if( newBlock ) return newBlock;

	typeBlock = DaoByteBlock_EncodeType( self, value->ctype );
	valueBlock = DaoByteBlock_EncodeValue( self, value->value );
	nameBlock = DaoByteBlock_EncodeString( self, value->name );
	newBlock = DaoByteBlock_AddBlock( self, (DaoValue*) value, DAO_ASM_VALUE );

	newBlock->begin[0] = DAO_PAR_NAMED;
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+2, nameBlock );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+4, valueBlock );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+6, typeBlock );
	return newBlock;
}
DaoByteBlock* DaoByteBlock_EncodeValue( DaoByteBlock *self, DaoValue *value )
{
	char *chs;
	DaoByteBlock *newBlock;
	if( value == NULL ) return NULL;
	newBlock = DaoByteBlock_FindOrCopyBlock( self, value );
	if( newBlock ) return newBlock;
	switch( value->type ){
	default :
		DaoByteCoder_Error2( self->coder, NULL, "Unencoded value (type id = %i)!", value->type );
		break;
	case DAO_INTERFACE :
		chs = value->xInterface.abtype->name->chars;
		DaoByteCoder_Error3( self->coder, NULL, "Unencoded interface type (name = %s)!", chs );
		break;
	case DAO_CINTYPE :
		chs = value->xCinType.vatype->name->chars;
		DaoByteCoder_Error3( self->coder, NULL, "Unencoded interface type (name = %s)!", chs );
		break;
	case DAO_CLASS :
		chs = value->xClass.className->chars;
		DaoByteCoder_Error3( self->coder, NULL, "Unencoded class (name = %s)!", chs );
		break;
	case DAO_CTYPE :
	case DAO_CDATA :
		chs = value->xCtype.ctype->name->chars;
		DaoByteCoder_Error3( self->coder, NULL, "Unencoded cdata type (name = %s)!", chs );
		break;
	case DAO_ROUTINE :
		chs = value->xRoutine.routName->chars;
		DaoByteCoder_Error3( self->coder, NULL, "Unencoded routine (name = %s)!", chs );
		chs = value->xRoutine.routType->name->chars;
		DaoByteCoder_Error3( self->coder, NULL, "Unencoded routine (type = %s)!", chs );
		break;
	case DAO_NONE : return DaoByteBlock_AddBlock( self, value, DAO_ASM_VALUE );
	case DAO_BOOLEAN : return DaoByteBlock_EncodeBoolean( self, (DaoBoolean*) value );
	case DAO_INTEGER : return DaoByteBlock_EncodeInteger( self, (DaoInteger*) value );
	case DAO_FLOAT   : return DaoByteBlock_EncodeFloat( self, (DaoFloat*) value );
	case DAO_COMPLEX : return DaoByteBlock_EncodeComplex( self, (DaoComplex*) value );
	case DAO_STRING : return DaoByteBlock_EncodeString( self, value->xString.value );
	case DAO_ENUM : return DaoByteBlock_EncodeEnum( self, (DaoEnum*) value );
	case DAO_ARRAY  : return DaoByteBlock_EncodeArray( self, (DaoArray*) value );
	case DAO_LIST  : return DaoByteBlock_EncodeList( self, (DaoList*) value );
	case DAO_MAP   : return DaoByteBlock_EncodeMap( self, (DaoMap*) value );
	case DAO_TUPLE : return DaoByteBlock_EncodeTuple( self, (DaoTuple*) value );
	case DAO_TYPE  : return DaoByteBlock_EncodeType( self, (DaoType*) value );
	case DAO_CINVALUE : return DaoByteBlock_EncodeCinValue( self, (DaoCinValue*) value );
	case DAO_PAR_NAMED : return DaoByteBlock_EncodeNameValue( self, (DaoNameValue*) value );
	}
	return NULL;
}
DaoByteBlock* DaoByteBlock_EncodeEnumType( DaoByteBlock *self, DaoType *type )
{
	DaoByteBlock *newBlock = DaoByteBlock_FindOrCopyBlock( self, (DaoValue*) type );
	DaoByteBlock *nameBlock;
	DNode *it;

	if( newBlock ) return newBlock;
	for(it=DMap_First(type->mapNames); it; it=DMap_Next(type->mapNames,it)){
		DaoByteBlock_EncodeString( self, it->key.pString );
	}
	nameBlock = DaoByteBlock_EncodeString( self, type->name );
	newBlock = DaoByteBlock_AddBlock( self, (DaoValue*) type, DAO_ASM_ENUM );

	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin, nameBlock );
	DaoByteCoder_EncodeUInt16( newBlock->begin+2, type->subtid );
	DaoByteCoder_EncodeUInt32( newBlock->begin+4, type->mapNames->size );
	if( type->mapNames->size == 0 ) return newBlock;
	for(it=DMap_First(type->mapNames); it; it=DMap_Next(type->mapNames,it)){
		DaoByteBlock *data = DaoByteBlock_NewBlock( newBlock, DAO_ASM_DATA );
		DaoByteBlock *namebk = DaoByteBlock_EncodeString( self, it->key.pString );
		DaoByteBlock_InsertBlockIndex( data, data->begin, namebk );
		DaoByteCoder_EncodeUInt32( data->begin+2, it->value.pInt );
	}
	DaoByteBlock_CopyToEndFromBegin( newBlock, newBlock->last );
	DaoByteCoder_Remove( self->coder, newBlock->last, newBlock );
	return newBlock;
}
static DaoByteBlock* DaoByteBlock_EncodeAuxType( DaoByteBlock *self, DaoType *type )
{
	DaoByteBlock *newBlock = DaoByteBlock_FindOrCopyBlock( self, (DaoValue*) type );
	DaoByteBlock *vatBlock = DaoByteBlock_EncodeType( self, DaoType_GetBaseType( type ) );
	if( newBlock ) return newBlock;
	if( vatBlock == NULL ) return NULL;

	newBlock = DaoByteBlock_AddBlock( self, (DaoValue*) type, DAO_ASM_AUXTYPE );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin, vatBlock );
	DaoByteCoder_EncodeUInt16( newBlock->begin+2, (type->konst<<2)|(type->invar<<1)|type->var );
	return newBlock;
}
/*
// Currently there is no need to encode DaoType::subtid, because the two types
// that use DaoType::subtid are the enum types and overloaded routine types.
// But enum types are encoded separately, and overloaded routine types are not
// declarable types.
*/
DaoByteBlock* DaoByteBlock_EncodeType( DaoByteBlock *self, DaoType *type )
{
	DaoByteBlock *newBlock = DaoByteBlock_FindOrCopyBlock( self, (DaoValue*) type );
	DaoByteBlock *nameBlock, *auxBlock = NULL, *cbtypebk = NULL;
	int size = 0;

	if( type == NULL ) return NULL;
	if( newBlock ) return newBlock;
	if( type->invar || type->var ) return DaoByteBlock_EncodeAuxType( self, type );
	if( type->tid == DAO_ENUM ) return DaoByteBlock_EncodeEnumType( self, type );
	if( type->nested ) size = DaoByteBlock_EncodeValues2( self, type->nested );
	if( type->aux ) auxBlock = DaoByteBlock_EncodeValue( self, type->aux );
	if( type->cbtype ) cbtypebk = DaoByteBlock_EncodeType( self, type->cbtype );
	nameBlock = DaoByteBlock_EncodeString( self, type->name );
	newBlock = DaoByteBlock_AddBlock( self, (DaoValue*) type, DAO_ASM_TYPE );

	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin, nameBlock );
	if( auxBlock ) DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+4, auxBlock );
	if( cbtypebk ) DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+6, cbtypebk );
	DaoByteCoder_EncodeUInt16( newBlock->begin+2, type->tid );
	DaoByteBlock_AddBlockIndexData( newBlock, 0, size );
	return newBlock;
}
DaoByteBlock* DaoByteBlock_EncodeTypeAlias( DaoByteBlock *self, DaoType *type, DaoType *aliased, DString *alias, DaoType *rectype, int perm )
{
	DaoByteBlock *newBlock = NULL;
	DaoByteBlock *nameBlock = DaoByteBlock_EncodeString( self, alias );
	DaoByteBlock *typeBlock = DaoByteBlock_EncodeType( self, type );
	DaoByteBlock *typeBlock2 = DaoByteBlock_EncodeType( self, rectype );
	if( aliased == type ){
		newBlock = DaoByteBlock_AddBlock( self, (DaoValue*) aliased, DAO_ASM_TYPEDEF );
	}else{
		newBlock = DaoByteBlock_FindObjectBlock( self, (DaoValue*) aliased );
		if( newBlock ) return newBlock;

		newBlock = DaoByteBlock_AddBlock( self, (DaoValue*) aliased, DAO_ASM_TYPEDEF );
	}
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+0, nameBlock );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+2, typeBlock );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+4, typeBlock2 );
	DaoByteCoder_EncodeUInt16( newBlock->begin+6, perm );
	return newBlock;
}
DaoByteBlock* DaoByteBlock_EncodeTypeOf( DaoByteBlock *self, DaoType *type, DaoValue *value )
{
	DaoByteBlock *newBlock = DaoByteBlock_FindOrCopyBlock( self, (DaoValue*) type );
	DaoByteBlock *valBlock = DaoByteBlock_FindOrCopyBlock( self, value );
	if( newBlock ) return newBlock;
	if( valBlock == NULL ) return NULL;

	newBlock = DaoByteBlock_AddBlock( self, (DaoValue*) type, DAO_ASM_TYPEOF );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin, valBlock );
	return newBlock;
}
DaoByteBlock* DaoByteBlock_EncodeCtype( DaoByteBlock *self, DaoCtype *ctype, DaoCtype *generic, DaoType **types, int n )
{
	DaoByteBlock *newBlock = DaoByteBlock_FindOrCopyBlock( self, (DaoValue*) ctype );
	DaoByteBlock *genBlock = DaoByteBlock_FindOrCopyBlock( self, (DaoValue*) generic );

	if( newBlock ) return newBlock;
	DaoByteBlock_EncodeValues( self, (DaoValue**) types, n );
	newBlock = DaoByteBlock_AddBlock( self, (DaoValue*) ctype, DAO_ASM_VALUE );
	newBlock->begin[0] = DAO_CTYPE;
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+2, genBlock );
	DaoByteBlock_AddBlockIndexData( newBlock, 2, n );
	return newBlock;
}


void DaoByteBlock_EncodeDecoPatterns( DaoByteBlock *self, DList *patterns );

DaoByteBlock* DaoByteBlock_AddNamespace( DaoByteBlock *self, DaoNamespace *ns, DString *name, DaoNamespace *defOrScope )
{
	DaoByteBlock *dsblock = DaoByteBlock_FindObjectBlock( self, (DaoValue*) defOrScope );
	DaoByteBlock *namebk = name ? DaoByteBlock_EncodeString( self, name ) : NULL;
	DaoByteBlock *nsblock = DaoByteBlock_AddBlock( self, (DaoValue*) ns, DAO_ASM_NAMESPACE );
	if( namebk == NULL ){ /* namespace defined: */
		DaoByteBlock_InsertBlockIndex( nsblock, nsblock->begin, dsblock );
	}else{
		DaoByteBlock_InsertBlockIndex( nsblock, nsblock->begin, namebk );
		if( dsblock ) DaoByteBlock_InsertBlockIndex( nsblock, nsblock->begin + 2, dsblock );
	}
	return nsblock;
}

DaoByteBlock* DaoByteBlock_AddRoutineBlock( DaoByteBlock *self, DaoRoutine *routine, int perm )
{
	DaoByteBlock *decl = DaoByteBlock_FindObjectBlock( self, (DaoValue*) routine );
	DaoByteBlock *type = DaoByteBlock_EncodeType( self, routine->routType );
	DaoByteBlock *host = DaoByteBlock_EncodeType( self, routine->routHost );
	DaoByteBlock *name = DaoByteBlock_EncodeString( self, routine->routName );
	DaoByteBlock *rout = DaoByteBlock_AddBlock( self, (DaoValue*) routine, DAO_ASM_ROUTINE );
	if( decl ){
		DaoByteBlock_InsertBlockIndex( rout, rout->begin, decl );
	}else{
		DaoByteBlock_InsertBlockIndex( rout, rout->begin, name );
	}
	DaoByteBlock_InsertBlockIndex( rout, rout->begin+2, type );
	if( host ) DaoByteBlock_InsertBlockIndex( rout, rout->begin+4, host );
	DaoByteCoder_EncodeUInt16( rout->begin+6, routine->attribs );
	if( routine->routHost && routine->routHost->tid == DAO_CLASS ){
		rout->end[6] = routine == routine->routHost->aux->xClass.initRoutine;
	}
	rout->end[7] = perm;
	if( routine->body ) DaoByteBlock_EncodeDecoPatterns( rout, routine->body->decoTargets );
	return rout;
}
DaoByteBlock* DaoByteBlock_AddClassBlock( DaoByteBlock *self, DaoClass *klass, int perm )
{
	daoint i, j;
	DaoByteBlock *decl = DaoByteBlock_FindObjectBlock( self, (DaoValue*) klass );
	DaoByteBlock *name = DaoByteBlock_EncodeString( self, klass->className );
	DaoByteBlock *parent = DaoByteBlock_EncodeValue( self, klass->parent );
	DaoByteBlock *block = DaoByteBlock_AddBlock( self, (DaoValue*) klass, DAO_ASM_CLASS );
	DaoByteBlock *data;
	if( decl ){
		DaoByteBlock_InsertBlockIndex( block, block->begin, decl );
	}else{
		DaoByteBlock_InsertBlockIndex( block, block->begin, name );
	}
	DaoByteBlock_InsertBlockIndex( block, block->begin+2, parent );
	DaoByteCoder_EncodeUInt32( block->begin+4, klass->attribs );
	block->end[7] = perm;
	if( decl == NULL ) return block;
	data = DaoByteBlock_NewBlock( block, DAO_ASM_BASES );
	DaoByteBlock_EncodeValues2( self, klass->mixinBases );
	DaoByteBlock_AddBlockIndexData( data, 4, klass->mixinBases->size );
	DaoByteBlock_EncodeDecoPatterns( block, klass->decoTargets );
	return block;
}
DaoByteBlock* DaoByteBlock_AddInterfaceBlock( DaoByteBlock *self, DaoInterface *inter, int pm )
{
	DaoByteBlock *decl = DaoByteBlock_FindObjectBlock( self, (DaoValue*) inter );
	DaoByteBlock *name = DaoByteBlock_EncodeString( self, inter->abtype->name );
	DaoByteBlock *block = DaoByteBlock_AddBlock( self, (DaoValue*) inter, DAO_ASM_INTERFACE );
	if( decl ){
		DaoByteBlock_InsertBlockIndex( block, block->begin, decl );
	}else{
		DaoByteBlock_InsertBlockIndex( block, block->begin, name );
	}
	DaoByteCoder_EncodeUInt16( block->begin+4, inter->supers->size );
	block->end[7] = pm;
	DaoByteBlock *data = DaoByteBlock_NewBlock( block, DAO_ASM_BASES );
	DaoByteBlock_EncodeValues2( self, inter->supers );
	DaoByteBlock_AddBlockIndexData( data, 4, inter->supers->size );
	return block;
}
DaoByteBlock* DaoByteBlock_AddCinTypeBlock( DaoByteBlock *self, DaoCinType *cintype, int pm )
{
	DaoByteBlock *inter = DaoByteBlock_FindObjectBlock( self, (DaoValue*) cintype->abstract );
	DaoByteBlock *type = DaoByteBlock_EncodeType( self, cintype->target );
	DaoByteBlock *block = DaoByteBlock_AddBlock( self, (DaoValue*) cintype, DAO_ASM_INTERFACE );

	DaoByteBlock_InsertBlockIndex( block, block->begin, inter );
	DaoByteBlock_InsertBlockIndex( block, block->begin + 2, type );
	DaoByteCoder_EncodeUInt16( block->begin+4, cintype->supers->size );
	block->end[7] = pm;
	DaoByteBlock *data = DaoByteBlock_NewBlock( block, DAO_ASM_BASES );
	DaoByteBlock_EncodeValues2( self, cintype->supers );
	DaoByteBlock_AddBlockIndexData( data, 4, cintype->supers->size );
	return block;
}

DaoByteBlock* DaoByteBlock_EncodeLoad( DaoByteBlock *self, DaoNamespace *mod, DString *modname, DString *asname )
{
	DaoByteBlock *fileBlock = DaoByteBlock_EncodeString( self, modname );
	DaoByteBlock *nameBlock = asname ? DaoByteBlock_EncodeString( self, asname ) : NULL;
	DaoByteBlock *newBlock = DaoByteBlock_AddBlock( self, (DaoValue*) mod, DAO_ASM_LOAD );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin, fileBlock );
	if( asname ) DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+2, nameBlock );
	return newBlock;
}
DaoByteBlock* DaoByteBlock_EncodeExport( DaoByteBlock *self, DaoNamespace *ns, DString *names[3] )
{
	DaoByteBlock *nsBlock = DaoByteBlock_FindObjectBlock( self, (DaoValue*) ns );
	DaoByteBlock *namebk1 = names[0] ? DaoByteBlock_EncodeString( self, names[0] ) : NULL;
	DaoByteBlock *namebk2 = names[1] ? DaoByteBlock_EncodeString( self, names[1] ) : NULL;
	DaoByteBlock *namebk3 = names[2] ? DaoByteBlock_EncodeString( self, names[2] ) : NULL;
	DaoByteBlock *newBlock = DaoByteBlock_NewBlock( self, DAO_ASM_EXPORT );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin, nsBlock );
	if( namebk1 ) DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+2, namebk1 );
	if( namebk2 ) DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+4, namebk2 );
	if( namebk3 ) DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+6, namebk3 );
	return newBlock;
}
DaoByteBlock* DaoByteBlock_EncodeImport( DaoByteBlock *self, DaoValue *mod, DString *name, int scope, int index )
{
	DaoByteBlock *modBlock = DaoByteBlock_FindObjectBlock( self, mod );
	DaoByteBlock *nameBlock = name ? DaoByteBlock_EncodeString( self, name ) : NULL;
	DaoByteBlock *newBlock = DaoByteBlock_NewBlock( self, DAO_ASM_IMPORT );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin, modBlock );
	if( name ) DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+2, nameBlock );
	DaoByteCoder_EncodeUInt16( newBlock->begin+4, scope );
	DaoByteCoder_EncodeUInt16( newBlock->begin+6, index );
	return newBlock;
}
DaoByteBlock* DaoByteBlock_AddEvalBlock( DaoByteBlock *self, DaoValue *value, int code, int opb, int mode, DaoType *type )
{
	DaoByteBlock *tblock = DaoByteBlock_EncodeType( self, type );
	DaoByteBlock *block = DaoByteBlock_AddBlock( self, value, DAO_ASM_EVAL );
	DaoByteCoder_EncodeUInt16( block->begin, code );
	DaoByteCoder_EncodeUInt16( block->begin+2, opb );
	DaoByteCoder_EncodeUInt16( block->begin+4, mode );
	DaoByteBlock_InsertBlockIndex( block, block->begin+6, tblock );
	return block;
}

DaoByteBlock* DaoByteBlock_DeclareConst( DaoByteBlock *self, DString *name, DaoValue *value, int perm )
{
	DaoByteBlock *nameBlock = DaoByteBlock_EncodeString( self, name );
	DaoByteBlock *valueBlock = DaoByteBlock_EncodeValue( self, value );
	DaoByteBlock *newBlock = DaoByteBlock_NewBlock( self, DAO_ASM_CONST );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin, nameBlock );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+2, valueBlock );
	newBlock->begin[6] = 0;
	newBlock->begin[7] = perm;
	return newBlock;
}
DaoByteBlock* DaoByteBlock_Declare( DaoByteBlock *self, int tag, DString *name, DaoValue *value, DaoType *type, int perm )
{
	DaoByteBlock *nameBlock = name ? DaoByteBlock_EncodeString( self, name ) : NULL;
	DaoByteBlock *valueBlock = DaoByteBlock_EncodeValue( self, value );
	DaoByteBlock *typeBlock = DaoByteBlock_EncodeType( self, type );
	DaoByteBlock *newBlock = DaoByteBlock_NewBlock( self, tag );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin, nameBlock );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+2, valueBlock );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+4, typeBlock );
	newBlock->begin[6] = 0;
	newBlock->begin[7] = perm;
	return newBlock;
}
DaoByteBlock* DaoByteBlock_DeclareVar( DaoByteBlock *self, DString *name, DaoValue *value, DaoType *type, int perm )
{
	return DaoByteBlock_Declare( self, DAO_ASM_VAR, name, value, type, perm );
}
DaoByteBlock* DaoByteBlock_DeclareStatic( DaoByteBlock *self, DString *name, DaoValue *value, DaoType *type, int level, int id )
{
	DaoByteBlock *block;
	char suffix[32];
	sprintf( suffix, "{%i}[%i]", level, id );
	name = DString_Copy( name );
	DString_AppendChars( name, suffix );
	block = DaoByteBlock_Declare( self, DAO_ASM_STATIC, name, value, type, DAO_PERM_NONE );
	DString_Delete( name );
	return block;
}
DaoByteBlock* DaoByteBlock_DeclareGlobal( DaoByteBlock *self, DString *name, DaoValue *value, DaoType *type, int perm )
{
	return DaoByteBlock_Declare( self, DAO_ASM_GLOBAL, name, value, type, perm );
}
DaoByteBlock* DaoByteBlock_EncodeSeekStmt( DaoByteBlock *self, DaoByteBlock *target )
{
	DaoByteBlock *newBlock = DaoByteBlock_NewBlock( self, DAO_ASM_SEEK );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin, target );
	return newBlock;
}
DaoByteBlock* DaoByteBlock_EncodeVerbatim( DaoByteBlock *self, DString *tag, DString *mode, DString *text, int line )
{
	DaoByteBlock *tagBlock = DaoByteBlock_EncodeString( self, tag );
	DaoByteBlock *modeBlock = DaoByteBlock_EncodeString( self, mode );
	DaoByteBlock *textBlock = DaoByteBlock_EncodeString( self, text );
	DaoByteBlock *newBlock = DaoByteBlock_NewBlock( self, DAO_ASM_VERBATIM );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin, tagBlock );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+2, modeBlock );
	DaoByteBlock_InsertBlockIndex( newBlock, newBlock->begin+4, textBlock );
	DaoByteCoder_EncodeUInt16( newBlock->begin+6, line );
	return newBlock;
}
DaoByteBlock* DaoByteBlock_EncodeDecorators( DaoByteBlock *self, DList *decos, DList *pars )
{
	int i;
	DaoByteBlock *decoBlock = DaoByteBlock_NewBlock( self, DAO_ASM_DECOS );
	DaoByteBlock *data = decoBlock;
	for(i=0; i<decos->size; i++){
		DaoRoutine *decoFunc = decos->items.pRoutine[i];
		DaoList *decoParam = (DaoList*) pars->items.pValue[i];
		DaoByteBlock *b1 = DaoByteBlock_FindObjectBlock( self, (DaoValue*) decoFunc );
		DaoByteBlock *b2 = DaoByteBlock_FindObjectBlock( self, (DaoValue*) decoParam );
		if( b1 == NULL || b2 == NULL ) continue;
		DaoByteBlock_InsertBlockIndex( data, data->begin, b1 );
		DaoByteBlock_InsertBlockIndex( data, data->begin+2, b2 );
		data = DaoByteBlock_NewBlock( decoBlock, DAO_ASM_DATA );
	}
	if( data != decoBlock ){
		DaoByteBlock_CopyToEndFromBegin( decoBlock, data );
		DaoByteCoder_Remove( self->coder, data, decoBlock );
	}
	return decoBlock;
}
void DaoByteBlock_EncodeDecoPatterns( DaoByteBlock *self, DList *patterns )
{
	DaoByteBlock *ruleBlock, *pat, *data;
	int i, j;
	if( patterns == NULL || patterns->size == 0 ) return;
	ruleBlock = data = DaoByteBlock_NewBlock( self, DAO_ASM_PATTERNS );
	for(i=0; i<patterns->size; i+=4){
		if( i ) data = DaoByteBlock_NewBlock( ruleBlock, DAO_ASM_DATA );
		for(j=0; j<4; ++j){
			if( i+j >= patterns->size ) break;
			pat = DaoByteBlock_EncodeString( self, patterns->items.pString[i+j] );
			DaoByteBlock_InsertBlockIndex( data, data->begin+2*j, pat );
		}
	}
	if( data != ruleBlock ){
		DaoByteBlock_CopyToEndFromBegin( ruleBlock, data );
		DaoByteCoder_Remove( self->coder, data, ruleBlock );
	}
	DaoByteBlock_MoveToBack( self, ruleBlock );
}



static int DaoByteCoder_UpdateIndex( DaoByteCoder *self, DaoByteBlock *block )
{
	int inserted = 0;
	DaoByteBlock *pb = block->first;
	DNode *it;

	self->index += block->type > 0 && block->type <= DAO_ASM_EVAL;
	block->index = self->index;
	if( block->wordToBlocks ){
		for(it=DMap_First(block->wordToBlocks); it; it=DMap_Next(block->wordToBlocks,it)){
			DaoByteBlock *pb = (DaoByteBlock*) it->value.pVoid;
			if( pb != NULL && block->index > (pb->index + 0x7fff) ){
				it->value.pVoid = DaoByteBlock_EncodeSeekStmt( block, pb );
				inserted += 1;
			}
		}
	}
	while( pb ){
		inserted += DaoByteCoder_UpdateIndex( self, pb );
		pb = pb->next;
	}
	return inserted;
}
static void DaoByteCoder_FinalizeBlock( DaoByteCoder *self, DaoByteBlock *block )
{
	DaoByteBlock *defblock, *pb = block->first;

	for(pb=block->first; pb; pb=pb->next){
		DaoByteCoder_FinalizeBlock( self, pb );
	}
	defblock = DaoByteBlock_FindObjectBlock( block, block->value );
	if( defblock && defblock != block ) return; /* Just declaration; */
	if( block->type == DAO_ASM_CLASS ){
		DaoClass *klass = (DaoClass*) block->value;
		DaoByteCoder_EncodeUInt32( block->begin+4, klass->attribs );
	}
}

void DaoByteCoder_FinalizeRoutineBlock( DaoByteCoder *self, DaoByteBlock *block )
{
	DaoType *type;
	DaoNamespace *nspace;
	DaoByteBlock *cur, *typebk, *namebk, *defblock;
	DaoByteBlock *newbk, *consts, *types, *code;
	DaoByteBlock *pb = block->first;
	DaoByteBlock *decos = NULL;
	DaoRoutine *routine;
	DMap *id2names;
	DMap *varblocks;
	DMap *vartypes;
	DNode *it;
	uchar_t *data;
	int i, N;

	if( block->type != DAO_ASM_ROUTINE ) return;

	for(pb=block->first; pb; pb=pb->next){
		if( pb->type == DAO_ASM_DECOS ) decos = pb;
	}

	routine = (DaoRoutine*) block->value;
	if( routine->body == NULL ) return;

	nspace = routine->nameSpace;
	DaoByteCoder_EncodeUInt16( block->begin+6, routine->attribs );
	DaoByteCoder_EncodeUInt16( block->end, routine->body->regCount );
	if( routine->body->upValues ){
		DaoByteCoder_EncodeUInt16( block->end + 2, routine->body->upValues->size );
	}
	if( routine->routHost && routine->routHost->tid == DAO_OBJECT ){
		/* Default constructor; */
		block->end[6] = routine == routine->routHost->aux->xClass.initRoutine;
	}

	/* local constants: */
	N = DaoByteBlock_EncodeValues2( block, routine->routConsts->value );
	consts = DaoByteBlock_NewBlock( block, DAO_ASM_CONSTS );
	DaoByteCoder_EncodeUInt16( consts->begin, N );
	DaoByteBlock_AddBlockIndexData( consts, 3, N );

	/* explicit types: */
	varblocks = DMap_New(0,0);
	vartypes = routine->body->localVarType;
	for(it=DMap_First(vartypes); it; it=DMap_Next(vartypes,it)){
		DaoByteBlock *pb = DaoByteBlock_EncodeType( block, it->value.pType );
		DMap_Insert( varblocks, it->key.pVoid, pb );
	}
	types = DaoByteBlock_NewBlock( block, DAO_ASM_TYPES );
	DaoByteCoder_EncodeUInt16( types->begin, vartypes->size );
	cur = types;
	data = types->begin + 4;
	for(it=DMap_First(varblocks); it; it=DMap_Next(varblocks,it), data+=4){
		DaoByteBlock *pb = (DaoByteBlock*) it->value.pVoid;
		if( data >= cur->begin + 8 ){
			cur = DaoByteBlock_NewBlock( types, DAO_ASM_DATA );
			data = cur->begin;
		}
		DaoByteCoder_EncodeUInt16( data, it->key.pInt );
		DaoByteBlock_InsertBlockIndex( cur, data+2, pb );
	}
	if( cur != types ){
		DaoByteBlock_CopyToEndFromBegin( types, cur );
		DaoByteCoder_Remove( types->coder, cur, types );
	}
	DMap_Delete( varblocks );

	id2names = DHash_New(0,0);
	for(it=DMap_First(nspace->lookupTable); it; it=DMap_Next(nspace->lookupTable,it)){
		int id = LOOKUP_BIND( LOOKUP_ST(it->value.pInt), 0, 0, LOOKUP_ID(it->value.pInt) );
		DMap_Insert( id2names, IntToPointer( id ), it->key.pVoid );
		//printf( "%s %i\n", it->key.pString->chars, LOOKUP_ID(id) );
	}
	if( routine->routHost ){
		DaoClass *klass = (DaoClass*) routine->routHost->aux;
		for(it=DMap_First(nspace->lookupTable); it; it=DMap_Next(nspace->lookupTable,it)){
			int id = LOOKUP_BIND( LOOKUP_ST(it->value.pInt), 0, 0, LOOKUP_ID(it->value.pInt) );
			DMap_Insert( id2names, IntToPointer( id ), it->key.pVoid );
		}
	}

	/* code: */
	code = DaoByteBlock_NewBlock( block, DAO_ASM_CODE );
	self->lines->size = 0;
	for(i=0; i<routine->body->annotCodes->size; ++i){
		DaoVmCodeX *vmc = routine->body->annotCodes->items.pVmc[i];
		int count = self->lines->size;
		int lastline = count ? self->lines->items.pInt[count-2] : -1;
		int lastcount = count ? self->lines->items.pInt[count-1] : -1;
		if( vmc->line != lastline ){
			DList_PushBack( self->lines, IntToPointer( vmc->line ) );
			DList_PushBack( self->lines, IntToPointer( 1 ) );
		}else{
			self->lines->items.pInt[count-1] += 1;
		}
	}
	DaoByteCoder_EncodeUInt16( code->begin, routine->body->annotCodes->size );
	DaoByteCoder_EncodeUInt16( code->begin + 2, self->lines->size/2 );
	cur = code;
	data = cur->begin + 4;
	for(i=0; i<self->lines->size; i+=2, data+=4){
		int last = i ? self->lines->items.pInt[i-2] : 0;
		int line = self->lines->items.pInt[i];
		int count = self->lines->items.pInt[i+1];
		if( data >= cur->begin + 8 ){
			cur = DaoByteBlock_NewBlock( code, DAO_ASM_DATA );
			data = cur->begin;
		}
		DaoByteCoder_EncodeUInt16( data, line - last );
		DaoByteCoder_EncodeUInt16( data+2, count );
	}

	for(i=0; i<routine->body->annotCodes->size; ++i){
		DaoVmCodeX *vmc = routine->body->annotCodes->items.pVmc[i];
		int st = 0;
		//printf( "%4i: ", i );DaoVmCodeX_Print( *vmc, NULL, NULL );
		switch( vmc->code ){
#if 0
		case DVM_GETCK :
			st = DAO_CLASS_CONSTANT;
			break;
		case DVM_GETVK :
		case DVM_SETVK :
			st = DAO_CLASS_VARIABLE;
			break;
		case DVM_GETVO :
		case DVM_SETVO :
			st = DAO_OBJECT_VARIABLE;
			break;
#endif
		case DVM_GETCG :
		case DVM_GETCG_I : case DVM_GETCG_F : case DVM_GETCG_C :
			st = DAO_GLOBAL_CONSTANT;
			break;
		case DVM_GETVG :
		case DVM_GETVG_I : case DVM_GETVG_F : case DVM_GETVG_C :
		case DVM_SETVG :
		case DVM_SETVG_II : case DVM_SETVG_FF : case DVM_SETVG_CC :
			st = DAO_GLOBAL_VARIABLE;
			break;
		}
		cur = DaoByteBlock_NewBlock( code, DAO_ASM_DATA );
		DaoByteCoder_EncodeUInt16( cur->begin + 0, vmc->code );
		DaoByteCoder_EncodeUInt16( cur->begin + 2, vmc->a );
		DaoByteCoder_EncodeUInt16( cur->begin + 4, vmc->b );
		DaoByteCoder_EncodeUInt16( cur->begin + 6, vmc->c );
		if( st ){
			it = DMap_Find( id2names, IntToPointer( LOOKUP_BIND(st, 0, 0, vmc->b)) );
			if( it == NULL ){
				DaoByteCoder_Error( self, cur, "global variable/constant name not found!" );
				break;
			}
			pb = DaoByteBlock_EncodeString( block, it->value.pString );
			DaoByteBlock_InsertBlockIndex( cur, cur->begin+4, pb );
		}
	}
	if( cur != code ){
		DaoByteBlock_CopyToEndFromBegin( code, cur );
		DaoByteCoder_Remove( code->coder, cur, code );
	}
	if( code != block->last ){ /* encoded new names */
		code->prev->next = code->next;
		code->next->prev = code->prev;
		code->prev = block->last;
		code->next = NULL;
		block->last->next = code;
		block->last = code;
	}
	DMap_Delete( id2names );
	if( decos != NULL ) DaoByteBlock_MoveToBack( block, decos );
}
static void DaoByteCoder_FinalizeEncoding( DaoByteCoder *self, DaoByteBlock *block )
{
	DaoByteBlock *pb = block->first;
	DaoRoutine *routine;
	DNode *it;
	int i, N;

	if( block->wordToBlocks ){
		for(it=DMap_First(block->wordToBlocks); it; it=DMap_Next(block->wordToBlocks,it)){
			DaoByteBlock *pb = (DaoByteBlock*) it->value.pVoid;
			int diff = 0;
			if( pb != NULL ){
				DaoByteBlock *base = block;
				if( base->type == DAO_ASM_DATA || base->type == DAO_ASM_DATA2 )
					base = base->parent;
				diff = base->index - pb->index;
				/*
				// For a non-value block, its index is the same as the last
				// value block. So one should be added to the difference.
				*/
				diff += (base->type == 0 || base->type > DAO_ASM_EVAL);
			}
			DaoByteCoder_EncodeUInt16( (uchar_t*) it->key.pVoid, diff );
			if( diff < 0 ){
				DaoByteCoder_Error( self, block, "Invalid encoding order between blocks!" );
				DaoByteCoder_PrintBlock( self, pb, 0, 1 );
#if DEBUG
				if( pb->value ){
					if( pb->value->type == DAO_ROUTINE )
						printf( "%s\n", pb->value->xRoutine.routName->chars );
					else if( pb->value->type == DAO_TYPE )
						printf( "%s\n", pb->value->xType.name->chars );
				}
#endif
			}
		}
	}
	while( pb ){
		DaoByteCoder_FinalizeEncoding( self, pb );
		pb = pb->next;
	}
}
static void DaoByteCoder_MergeData( DaoByteCoder *self, DaoByteBlock *block )
{
	uint_t A, B, C, D, E, F, G, H;
	DaoByteBlock *pb = block->first;
	while( pb ){
		if( pb->type == DAO_ASM_DATA ){
			DaoByteBlock *pb2 = pb->next;
			DaoByteCoder_DecodeChunk2222( pb->begin, & A, & B,  & C, & D );
			if( A <= 0xff && B <= 0xff && C <= 0xff && D <= 0xff && pb2 != NULL ){
				DaoByteCoder_DecodeChunk2222( pb2->begin, & E, & F,  & G, & H );
				if( E <= 0xff && F <= 0xff && G <= 0xff && H <= 0xff ){
					pb->begin[0] = A; pb->begin[1] = B; pb->begin[2] = C; pb->begin[3] = D;
					pb->begin[4] = E; pb->begin[5] = F; pb->begin[6] = G; pb->begin[7] = H;
					pb->type = DAO_ASM_DATA2;
					pb2->type = DAO_ASM_NONE;
					pb = pb->next;
				}
			}
		}else{
			DaoByteCoder_MergeData( self, pb );
		}
		pb = pb->next;
	}
}
static void DaoByteCoder_SplitData( DaoByteCoder *self, DaoByteBlock *block )
{
	int i;
	DaoByteBlock *pb = block->first;
	while( pb ){
		if( pb->type == DAO_ASM_DATA2 ){
			DaoByteBlock *pb2 = pb->next;
			if( pb2 == NULL || pb2->type != DAO_ASM_NONE ){
				pb2 = DaoByteCoder_NewBlock( self, DAO_ASM_DATA );
				pb2->parent = block;
				pb2->next = pb->next;
				pb2->prev = pb;
				if( pb->next ) pb->next->prev = pb2;
				pb->next = pb2;
			}
			pb->type = DAO_ASM_DATA;
			pb2->type = DAO_ASM_DATA;
			for(i=0; i<4; ++i)  DaoByteCoder_EncodeUInt16( pb2->begin + 2*i, pb->begin[4+i] );
			for(i=3; i>=0; --i) DaoByteCoder_EncodeUInt16( pb->begin + 2*i, pb->begin[i] );
			pb = pb->next;
		}else{
			DaoByteCoder_SplitData( self, pb );
		}
		pb = pb->next;
	}
}
void DaoByteCoder_Finalize( DaoByteCoder *self )
{
	if( self->top == NULL ) return;

	DaoByteCoder_FinalizeBlock( self, self->top );

	self->index = 0;
	while( DaoByteCoder_UpdateIndex( self, self->top ) ) self->index = 0;
	DaoByteCoder_FinalizeEncoding( self, self->top );
}

void DaoByteBlock_EncodeToString( DaoByteBlock *block, DString *output )
{
	DaoByteBlock *pb = block->first;

	if( block->type == DAO_ASM_NONE ) return;

	DString_AppendChar( output, block->type );
	DString_AppendBytes( output, (char*) block->begin, 8 );
	while( pb ){
		DaoByteBlock_EncodeToString( pb, output );
		pb = pb->next;
	}
	if( block->type < DAO_ASM_ROUTINE || block->type > DAO_ASM_CODE ) return;
	DString_AppendChar( output, DAO_ASM_END );
	DString_AppendBytes( output, (char*) block->end, 8 );
}
void DaoByteCoder_EncodeToString( DaoByteCoder *self, DString *output )
{
	if( self->top == NULL ) return;
	DaoByteCoder_Finalize( self );
	DaoByteCoder_MergeData( self, self->top );
	if( dao_bytecode_encrypt != NULL ){
		DString *dstring = DString_New();
		DaoByteBlock_EncodeToString( self->top, dstring );
		dao_bytecode_encrypt( dstring, 0 );
		DString_Append( output, dstring );
		DString_Delete( dstring );
	}else{
		DaoByteBlock_EncodeToString( self->top, output );
	}
}

void DaoByteCoder_EncodeHeader( DaoByteCoder *self, const char *fname, DString *output )
{
	DString *path = DString_New();
	uchar_t bytes2[2];
	uchar_t bytes4[4];

	DString_SetChars( path, fname );
	DaoVmSpace_ConvertPath( self->vmspace, path );

	DaoByteCoder_EncodeUInt16( bytes2, path->size );
	DaoByteCoder_EncodeUInt32( bytes4, self->fmthash );

	DString_AppendBytes( output, DAO_BC_SIGNATURE, 8 );
	if( dao_bytecode_encrypt != NULL ){
		DString_AppendChar( output, 0x1 );
	}else{
		DString_AppendChar( output, 0x0 );
	}
	DString_AppendChar( output, sizeof(dao_integer) == 4 ? '\4' : '\x8' );
	DString_AppendChar( output, sizeof(dao_float) == 4 ? '\4' : '\x8' );
	DString_AppendChar( output, '\0' );
	DString_AppendBytes( output, (char*) bytes4, 4 );
	DString_AppendBytes( output, "\0\0\0\0\0", 5 );
	DString_AppendBytes( output, "\0\0\0\0\0", 5 );
	DString_AppendBytes( output, "\0\0\0\0\r\n", 6 );

	DString_AppendBytes( output, (char*) bytes2, 2 );
	DString_AppendBytes( output, path->chars, path->size );
	DString_AppendBytes( output, "\0\r\n", 3 );
	DString_Delete( path );
}





int DaoByteCoder_Decode( DaoByteCoder *self, DString *input )
{
	daoint i;
	uchar_t *codes, *end;
	uint_t hash, fmtclass;
	DString header = *input;
	DString signature = DString_WrapBytes( DAO_BC_SIGNATURE, 8 );
	DString *dstring = NULL;

	if( input->chars == NULL ) goto InvalidFormat;

	if( header.size > 8 ) header.size = 8;
	if( DString_EQ( & header, & signature ) == 0 ) goto InvalidFormat;
	if( input->chars[8] != 0 && input->chars[8] != 1 ) goto InvalidVersion;

	hash = DaoByteCoder_DecodeUInt32( (uchar_t*)input->chars+12 );
	if( hash != self->fmthash ) goto InvalidHash;

	DaoByteCoder_Init( self );

	fmtclass = input->chars[8];
	self->intSize = input->chars[9];
	self->floatSize = input->chars[10];
	codes = (uchar_t*) input->chars + 32;
	end = (uchar_t*) input->chars + input->size;
	if( self->intSize != 4 && self->intSize != 8 ) goto InvalidFormat;
	if( self->floatSize != 4 && self->floatSize != 8 ) goto InvalidFormat;
	if( sizeof(dao_integer) == 4 && self->intSize == 8 ) goto InvalidFormat;
	if( sizeof(dao_float) == 4 && self->floatSize == 8 ) goto InvalidFormat;

	i = DaoByteCoder_DecodeUInt16( codes );
	DString_Reset( self->path, i );
	memcpy( self->path->chars, codes + 2, i*sizeof(char) );

	DList_Append( self->stack, self->top );

	codes += i + 5;

	if( fmtclass == 1 && dao_bytecode_decrypt != NULL ){
		dstring = DString_New();
		DString_SetBytes( dstring, (char*)codes, end - codes );
		dao_bytecode_decrypt( dstring, 0 );
		codes = (uchar_t*) dstring->chars;
		end = (uchar_t*) dstring->chars + dstring->size;
	}else if( fmtclass == 1 ){
		DaoByteCoder_Error( self, NULL, "unable to decode encrypted bytecode!" );
		goto ReturnZero;
	}

	codes += 9;  /* Skip: ASM_ROUTINE: 0, 0, 0, 0; */
	for(; codes < end; codes += 9){
		DaoByteBlock *current = (DaoByteBlock*) DList_Back( self->stack );
		int type = codes[0];
		if( type == DAO_ASM_END ){
			memcpy( current->end, codes+1, 8*sizeof(char) );
			DList_PopBack( self->stack );
		}else{
			DaoByteBlock *sublock = DaoByteBlock_NewBlock( current, type );
			memcpy( sublock->begin, codes+1, 8*sizeof(char) );
			if( type >= DAO_ASM_ROUTINE && type <= DAO_ASM_CODE ){
				DList_PushBack( self->stack, sublock );
			}
		}
	}
	if( dstring != NULL ) DString_Delete( dstring );
	return 1;
InvalidFormat:
	DaoByteCoder_Error( self, NULL, "invalid format!" );
	goto ReturnZero;
InvalidVersion:
	DaoByteCoder_Error( self, NULL, "unofficial format!" );
	goto ReturnZero;
InvalidHash:
	DaoByteCoder_Error( self, NULL, "format not matching!" );
ReturnZero:
	if( dstring != NULL ) DString_Delete( dstring );
	self->error = 1;
	return 0;
}


static void DaoByteCoder_DecodeBlock( DaoByteCoder *self, DaoByteBlock *block );

static DaoByteBlock* DaoByteCoder_LookupBlock( DaoByteCoder *self, DaoByteBlock *block, int index )
{
	int id = index;
	/*
	// For a value block, it has been already pushed to the stack, so the
	// base index 1 refers to itself. Adding one to refer to the correct one.
	*/
	if( index == 0 ) return NULL;
	if( block->type == DAO_ASM_DATA || block->type == DAO_ASM_DATA2 ) block = block->parent;
	index += block->type && block->type <= DAO_ASM_EVAL;
	if( index > self->stack->size ){
		DaoByteCoder_Error2( self, block, "invalid byteblock reference number %i in:", id );
		return NULL;
	}
	return (DaoByteBlock*) self->stack->items.pVoid[ self->stack->size - index ];
}
static DaoByteBlock* DaoByteCoder_LookupBlock2( DaoByteCoder *self, DaoByteBlock *block, int index )
{
	DaoByteBlock *bk = DaoByteCoder_LookupBlock( self, block, index );
	if( self->error ) return bk;
	if( bk == NULL ){
		DaoByteCoder_Error2( self, block, "null byteblock for reference number %i in:", index );
	}
	return bk;
}
static DaoByteBlock* DaoByteCoder_LookupBlock3( DaoByteCoder *self, DaoByteBlock *block, int index, int tid, const char *tname )
{
	DaoByteBlock *bk = DaoByteCoder_LookupBlock2( self, block, index );
	if( self->error ) return bk;
	if( bk->value == NULL || bk->value->type != tid ){
		DaoByteCoder_Error3( self, bk, "byteblock encodes no %s!", tname );
		DaoByteCoder_Error( self, block, "referenced in:" );
	}
	return bk;
}
static DaoByteBlock* DaoByteCoder_LookupValueBlock( DaoByteCoder *self, DaoByteBlock *block, int index )
{
	DaoByteBlock *bk = DaoByteCoder_LookupBlock2( self, block, index );
	if( self->error ) return bk;
	if( bk->value == NULL ){
		DaoByteCoder_Error( self, bk, "byteblock encodes no value!" );
		DaoByteCoder_Error( self, block, "referenced in:" );
	}
	return bk;
}
static DaoByteBlock* DaoByteCoder_LookupStringBlock( DaoByteCoder *self, DaoByteBlock *block, int index )
{
	return DaoByteCoder_LookupBlock3( self, block, index, DAO_STRING, "string" );
}
static DaoByteBlock* DaoByteCoder_LookupTypeBlock( DaoByteCoder *self, DaoByteBlock *block, int index )
{
	return DaoByteCoder_LookupBlock3( self, block, index, DAO_TYPE, "type" );
}
static int DaoByteCoder_CheckDataBlocks( DaoByteCoder *self, DaoByteBlock *block )
{
	DaoByteBlock *pbk;
	for(pbk=block->first; pbk; pbk=pbk->next){
		if( pbk->type != DAO_ASM_DATA ){
			DaoByteCoder_Error( self, NULL, "invalid byte sub chuncks (expecting ASM_DATA):" );
			DaoByteCoder_Error3( self, block, "find %s in:", DaoByteCoder_GetASM(pbk->type) );
			return 0;
		}
	}
	return 1;
}
static void DaoByteCoder_GetBlocks( DaoByteCoder *self, DaoByteBlock *block, uchar_t *data, uchar_t *end, int stride, int nullterm )
{
	for(; data < end; data+=stride){
		uint_t idx = DaoByteCoder_DecodeUInt16( data );
		DaoByteBlock *pb = DaoByteCoder_LookupBlock( self, block, idx );;
		if( self->error ) break;
		if( idx == 0 && nullterm ) break;
		DList_Append( self->iblocks, idx ? pb : NULL );
	}
}
static void DaoByteCoder_GetIntegers( DaoByteCoder *self, DList *ints, uchar_t *data, uchar_t *end, int stride )
{
	for(; data < end; data+=stride){
		uint_t idx = DaoByteCoder_DecodeUInt16( data );
		DList_Append( ints, IntToPointer( idx ) );
	}
}
void DaoByteBlock_GetAllBlocks( DaoByteCoder *self, DaoByteBlock *block, int head, int size, int nullterm )
{
	DaoByteBlock *pb, *dataBlock = block;
	uchar_t *data = dataBlock->begin + 2*(4-head);
	uchar_t *end = dataBlock->begin + 8;
	uint_t idx, offset = self->iblocks->size;
	DaoByteCoder_CheckDataBlocks( self, block );
	if( self->error ) return;
	for(; ; data+=2){
		if( data >= end ){
			if( end == block->end + 8 ) break;
			if( dataBlock == block ){
				dataBlock = block->first;
			}else{
				dataBlock = dataBlock->next;
			}
			if( dataBlock ){
				data = dataBlock->begin;
			}else{
				dataBlock = block;
				data = dataBlock->end;
			}
			end = data + 8;
		}
		idx = DaoByteCoder_DecodeUInt16( data );
		pb = DaoByteCoder_LookupBlock( self, dataBlock, idx );;
		if( self->error ) break;
		if( idx == 0 && nullterm ) break;
		DList_Append( self->iblocks, pb );
	}
	if( size >= 0 && size > (self->iblocks->size - offset) ){
		int count = self->iblocks->size - offset;
		char buffer[128];
		snprintf( buffer, sizeof(buffer), "expecting %i, but decoded %i!", size, count );
		DaoByteCoder_Error( self, block, "invalid number of byteblock reference numbers in:" );
		DaoByteCoder_Error( self, NULL, buffer );
	}
}
void DaoByteBlock_GetAllValues( DaoByteCoder *self, DaoByteBlock *block, int head, int size, int nullterm )
{
	uint_t i, offset = self->iblocks->size;
	DaoByteBlock_GetAllBlocks( self, block, head, size, nullterm );
	if( self->error ) return;
	for(i=offset; i<self->iblocks->size; ++i){
		DaoByteBlock *pb = (DaoByteBlock*) self->iblocks->items.pVoid[i];
		DList_Append( self->ivalues, pb->value );
	}
	DList_Erase( self->iblocks, offset, -1 );
}

static void DaoByteCoder_DecodeValue( DaoByteCoder *self, DaoByteBlock *block )
{
	DString *dstring;
	DaoMap *map;
	DaoList *list;
	DaoType *type;
	DaoType **itypes;
	DaoTuple *tuple;
	DaoArray *array;
	DaoValue *value = NULL;
	DaoNameValue *namevalue;
	DaoByteBlock *pb = block->first;
	DaoByteBlock *pb2, *pb3, *namebk, *valuebk, *typebk = NULL;
	uint_t i, A, B, C, D;
	int tid = block->begin[0];

	if( block->first != NULL ) DaoByteCoder_CheckDataBlocks( self, block );
	if( self->error ) return;

	switch( tid ){
	case DAO_NONE :
		value = dao_none_value;
		break;
	case DAO_BOOLEAN :
		value = (DaoValue*) DaoBoolean_New(0);
		value->xBoolean.value = block->end[0];
		break;
	case DAO_INTEGER :
		value = (DaoValue*) DaoInteger_New(0);
		value->xInteger.value = DaoByteCoder_DecodeDaoInt( self, block->end );
		break;
	case DAO_FLOAT :
		value = (DaoValue*) DaoFloat_New(0.0);
		value->xFloat.value = DaoByteCoder_DecodeFloat( self, block->end );
		break;
	case DAO_COMPLEX :
		value = (DaoValue*) DaoComplex_New2(0.0,0.0);
		value->xComplex.value.real = DaoByteCoder_DecodeFloat( self, block->first->begin );
		value->xComplex.value.imag = DaoByteCoder_DecodeFloat( self, block->end );
		pb = pb->next;
		break;
	case DAO_STRING :
		B = block->begin[1];
		value = (DaoValue*) DaoString_New();
		dstring = value->xString.value;
		DString_Reserve( dstring, B );
		for(i=0; i<6 && (i<B || pb!=NULL); ++i){
			DString_AppendChar( dstring, block->begin[i+2] );
		}
		for(; pb; pb=pb->next){
			DString_AppendBytes( dstring, (char*) pb->begin, 8 );
		}
		for(i=0; i<8 && (dstring->size%16)!=B; ++i){
			DString_AppendChar( dstring, block->end[i] );
		}
		//printf( ">>>>>>>>>>>>>> %i %i %i %s\n", dstring->size, B, i, dstring->chars );
		if( (dstring->size%16) != B ) DaoByteCoder_Error( self, block, "size not matching!" );
		break;
	case DAO_ENUM :
		A = DaoByteCoder_DecodeUInt16( block->begin+2 );
		B = DaoByteCoder_DecodeUInt32( block->end );
		typebk = DaoByteCoder_LookupTypeBlock( self, block, A );
		if( self->error ) break;
		value = (DaoValue*) DaoEnum_New( (DaoType*) typebk->value, B );
		break;
#ifdef DAO_WITH_NUMARRAY
	case DAO_ARRAY :
		A = block->begin[1];
		B = D = DaoByteCoder_DecodeUInt16( block->begin+2 );
		C = DaoByteCoder_DecodeUInt32( block->begin+4 );
		i = 0;
		array = DaoArray_New( A );
		value = (DaoValue*) array;
		DaoArray_SetDimCount( array, D );
		while( B > 0 && pb != NULL ){
			if( i < D ) array->dims[i++] = DaoByteCoder_DecodeUInt32( pb->begin );
			if( i < D ) array->dims[i++] = DaoByteCoder_DecodeUInt32( pb->begin+4 );
			pb = pb->next;
			B -= 2;
		}
		DaoArray_ResizeArray( array, array->dims, array->ndim );
		if( array->size != C ) DaoByteCoder_Error( self, block, "size not matching!" );
		if( self->error ) break;

		if( array->etype == DAO_INTEGER && sizeof(dao_integer) == 8 ){
			for(i=0; (i+1)<C && pb != NULL; i+=1, pb=pb->next){
				array->data.i[i] = DaoByteCoder_DecodeDaoInt( self, pb->begin );
			}
			if(C) array->data.i[i] = DaoByteCoder_DecodeDaoInt( self, block->end );
		}else if( array->etype == DAO_INTEGER ){
			for(i=0; (i+2)<C && pb != NULL; i+=2, pb=pb->next){
				array->data.i[i] = DaoByteCoder_DecodeDaoInt( self, pb->begin );
				array->data.i[i+1] = DaoByteCoder_DecodeDaoInt( self, pb->begin+4 );
			}
			if(C){
				array->data.i[i] = DaoByteCoder_DecodeDaoInt( self, block->end );
				if( !(C%2) ) array->data.i[i+1] = DaoByteCoder_DecodeDaoInt( self, block->end+4 );
			}
		}else if( array->etype == DAO_FLOAT ){
			for(i=0; (i+1)<C && pb != NULL; i+=1, pb=pb->next){
				array->data.f[i] = DaoByteCoder_DecodeFloat( self, pb->begin );
			}
			if(C) array->data.f[i] = DaoByteCoder_DecodeFloat( self, block->end );
		}else if( array->etype == DAO_COMPLEX ){
			for(i=0; (i+1)<C && pb != NULL && pb->next != NULL; i+=1, pb=pb->next->next){
				array->data.c[i].real = DaoByteCoder_DecodeFloat( self, pb->begin );
				array->data.c[i].imag = DaoByteCoder_DecodeFloat( self, pb->next->begin );
			}
			if( C ){
				array->data.c[i].real = DaoByteCoder_DecodeFloat( self, pb->begin );
				array->data.c[i].imag = DaoByteCoder_DecodeFloat( self, block->end );
			}
		}
		break;
#endif
	case DAO_LIST :
		A = DaoByteCoder_DecodeUInt16( block->begin+2 );
		B = DaoByteCoder_DecodeUInt32( block->begin+4 );
		pb2 = DaoByteCoder_LookupTypeBlock( self, block, A );
		if( self->error ) break;
		list = DaoList_New();
		value = (DaoValue*) list;
		if( A ) GC_Assign( & list->ctype, pb2->value );
		D = self->iblocks->size;
		DaoByteBlock_GetAllBlocks( self, block, 0, B, 1 );
		if( self->error ) break;
		for(i=D; i<self->iblocks->size; ++i){
			pb2 = (DaoByteBlock*) self->iblocks->items.pVoid[i];
			DaoList_Append( list, pb2->value );
		}
		DList_Erase( self->iblocks, D, -1 );
		if( list->value->size != B ) DaoByteCoder_Error( self, block, "size not matching!" );
		break;
	case DAO_MAP :
		A = DaoByteCoder_DecodeUInt16( block->begin+2 );
		B = DaoByteCoder_DecodeUInt32( block->begin+4 );
		pb2 = DaoByteCoder_LookupTypeBlock( self, block, A );
		if( self->error ) break;
		map = DaoMap_New( B );
		value = (DaoValue*) map;
		if( A ) GC_Assign( & map->ctype, pb2->value );
		D = self->iblocks->size;
		DaoByteBlock_GetAllBlocks( self, block, 0, -1, 1 );
		if( self->error ) break;
		for(i=D; i<self->iblocks->size; i+=2){
			pb2 = (DaoByteBlock*) self->iblocks->items.pVoid[i];
			pb3 = (DaoByteBlock*) self->iblocks->items.pVoid[i+1];
			DaoMap_Insert( map, pb2->value, pb3->value );
		}
		DList_Erase( self->iblocks, D, -1 );
		break;
	case DAO_TUPLE :
		A = DaoByteCoder_DecodeUInt16( block->begin+2 );
		B = DaoByteCoder_DecodeUInt16( block->begin+4 );
		pb2 = DaoByteCoder_LookupTypeBlock( self, block, A );
		if( self->error ) break;
		tuple = DaoTuple_Create( (DaoType*) pb2->value, B, 0 );
		tuple->subtype = block->begin[1];
		value = (DaoValue*) tuple;
		D = self->iblocks->size;
		DaoByteBlock_GetAllBlocks( self, block, 1, B, 1 );
		if( self->error ) break;
		for(i=D; i<self->iblocks->size; ++i){
			pb2 = (DaoByteBlock*) self->iblocks->items.pVoid[i];
			DaoTuple_SetItem( tuple, pb2->value, i-D );
		}
		DList_Erase( self->iblocks, D, -1 );
		if( tuple->size != B ) DaoByteCoder_Error( self, block, "size not matching!" );
		break;
	case DAO_CINVALUE :
		B = DaoByteCoder_DecodeUInt16( block->begin+2 );
		C = DaoByteCoder_DecodeUInt16( block->begin+4 );
		typebk = DaoByteCoder_LookupValueBlock( self, block, B );
		valuebk = DaoByteCoder_LookupValueBlock( self, block, C );
		if( self->error ) break;
		value = (DaoValue*) DaoWrappers_MakeCinValue( (DaoCinType*) typebk->value, valuebk->value );
		break;
	case DAO_PAR_NAMED :
		B = DaoByteCoder_DecodeUInt16( block->begin+2 );
		C = DaoByteCoder_DecodeUInt16( block->begin+4 );
		D = DaoByteCoder_DecodeUInt16( block->begin+6 );
		namebk = DaoByteCoder_LookupStringBlock( self, block, B );
		valuebk = DaoByteCoder_LookupValueBlock( self, block, C );
		typebk = DaoByteCoder_LookupTypeBlock( self, block, D );
		if( self->error ) break;
		namevalue = DaoNameValue_New( namebk->value->xString.value, valuebk->value );
		GC_IncRC( typebk->value );
		namevalue->ctype = (DaoType*) typebk->value;
		value = (DaoValue*) namevalue;
		break;
	case DAO_CTYPE :
		C = self->ivalues->size;
		A = DaoByteCoder_DecodeUInt16( block->begin+2 );
		pb2 = DaoByteCoder_LookupBlock3( self, block, A, DAO_CTYPE, "ctype" );
		if( self->error ) break;
		DaoByteBlock_GetAllValues( self, block, 2, -1, 1 );
		if( self->error ) break;
		itypes = self->ivalues->items.pType + C;
		D = self->ivalues->size - C;
		type = DaoType_Specialize( pb2->value->xCtype.cdtype, itypes, D );
		//printf( ">>>>>>>>> %p %s %s\n", type, type->name->chars, type->typer->name );
		//printf( "%p %p\n", type->aux, type->aux->xCtype.ctype );
		value = type->aux;
		DList_Erase( self->ivalues, C, -1 );
		break;
	default :
		DaoByteCoder_Error2( self, block, "Decoding not supported for value: %i!", tid );
	}
	GC_Assign( & block->value, value );
	//printf( "%p %p\n", block, value );
}
static void DaoByteCoder_DecodeCopyValue( DaoByteCoder *self, DaoByteBlock *block )
{
	uint_t A = DaoByteCoder_DecodeUInt16( block->begin );
	DaoByteBlock *valuebk = DaoByteCoder_LookupValueBlock( self, block, A );

	if( self->error ) return;
	DaoValue_Copy( valuebk->value, & block->value );
}
static void DaoByteCoder_DecodeType( DaoByteCoder *self, DaoByteBlock *block )
{
	DString *sname = DString_New();
	DaoType *type, **itypes;
	DaoByteBlock *name, *aux, *cbtype;
	DaoByteBlock *pb2, *pb = block->first;
	uint_t offset = self->ivalues->size;
	uint_t A, B, C, D;
	int i, count;
	daoint pos;

	DaoByteCoder_DecodeChunk2222( block->begin, & A, & B, & C, & D );
	name = DaoByteCoder_LookupStringBlock( self, block, A );
	aux = DaoByteCoder_LookupBlock( self, block, C );
	
	if( self->error ) goto Finalize2;

	DaoByteBlock_GetAllValues( self, block, 0, -1, 1 );
	if( self->error ) goto Finalize2;

	DString_Assign( sname, name->value->xString.value );
	pos = DString_FindChar( sname, '<', 0 );
	if( pos >= 0 ) DString_Erase( sname, pos, -1 );
	if( B == DAO_PAR_NAMED ){
		daoint pos = DString_FindChar( sname, ':', 0 );
		if( pos >= 0 ) DString_Erase( sname, pos, -1 );
	}else if( B == DAO_PAR_DEFAULT ){
		daoint pos = DString_FindChar( sname, '=', 0 );
		if( pos >= 0 ) DString_Erase( sname, pos, -1 );
	}
	itypes = self->ivalues->items.pType + offset;
	count = self->ivalues->size - offset;
	type = DaoNamespace_MakeType( self->nspace, sname->chars, B, aux?aux->value:NULL, itypes, count );
	if( D ){
		DaoType *cbt;
		cbtype = DaoByteCoder_LookupTypeBlock( self, block, D );
		if( self->error ) goto Finalize1;
		DString_Assign( sname, type->name );
		DString_Append( sname, cbtype->value->xType.name );
		cbt = DaoNamespace_FindType( self->nspace, sname );
		if( cbt != NULL ){
			type = cbt;
		}else{
			type = DaoType_Copy( type );
			DString_Assign( type->name, sname );
			GC_Assign( & type->cbtype, cbtype->value );
			DaoType_CheckAttributes( type );
			DaoNamespace_AddType( self->nspace, type->name, type );
		}
	}
Finalize1:
	GC_Assign( & block->value, type );
Finalize2:
	DString_Delete( sname );
	DList_Erase( self->ivalues, offset, -1 );
}
static void DaoByteCoder_DecodeTypeAlias( DaoByteCoder *self, DaoByteBlock *block )
{
	uint_t A = DaoByteCoder_DecodeUInt16( block->begin );
	uint_t B = DaoByteCoder_DecodeUInt16( block->begin+2 );
	uint_t C = DaoByteCoder_DecodeUInt16( block->begin+4 );
	uint_t D = DaoByteCoder_DecodeUInt16( block->begin+6 );
	DaoByteBlock *namebk = DaoByteCoder_LookupStringBlock( self, block, A );
	DaoByteBlock *typebk = DaoByteCoder_LookupTypeBlock( self, block, B );
	DString *name;
	DaoType *type;

	if( self->error ) return;

	name = namebk->value->xString.value;
	type = (DaoType*) typebk->value;

	type = DaoType_Copy( type );
	if( C ){
		DaoByteBlock *typebk2 = DaoByteCoder_LookupTypeBlock( self, block, C );
		type->recursive = 1; 
		DaoType_SetupRecursive( type, (DaoType*) typebk2->value, type );
	}
	DString_Assign( type->name, name );
	if( block->parent == self->top ){
		DaoNamespace_AddType( self->nspace, type->name, type );
		DaoNamespace_AddTypeConstant( self->nspace, type->name, type );
	}else if( block->parent->type == DAO_ASM_CLASS ){
		DaoClass *klass = DaoValue_CastClass( block->parent->value );
		DaoClass_AddConst( klass, type->name, (DaoValue*) type, D );
	}

	GC_Assign( & block->value, type );
}
static void DaoByteCoder_DecodeAuxType( DaoByteCoder *self, DaoByteBlock *block )
{
	uint_t A = DaoByteCoder_DecodeUInt16( block->begin );
	uint_t B = DaoByteCoder_DecodeUInt16( block->begin + 2 );
	DaoByteBlock *typebk = DaoByteCoder_LookupTypeBlock( self, block, A );
	DaoType *type;

	if( self->error ) return;
	if( B == (3<<1) ){
		type = DaoType_GetConstType( (DaoType*) typebk->value );
	}else if( B == (1<<1) ){
		type = DaoType_GetInvarType( (DaoType*) typebk->value );
	}else if( B == 1 ){
		type = DaoType_GetVarType( (DaoType*) typebk->value );
	}else{
		DaoByteCoder_Error( self, block, "Invalid auxiliary type!" );
		return;
	}

	GC_Assign( & block->value, type );
}
static void DaoByteCoder_DecodeTypeOf( DaoByteCoder *self, DaoByteBlock *block )
{
	uint_t A = DaoByteCoder_DecodeUInt16( block->begin );
	DaoByteBlock *valuebk = DaoByteCoder_LookupValueBlock( self, block, A );
	DaoType *type;

	if( self->error ) return;
	type = DaoNamespace_GetType( self->nspace, valuebk->value );

	GC_Assign( & block->value, type );
}
static void DaoByteCoder_AddToScope( DaoByteCoder *self, DaoByteBlock *block, DString *name, DaoValue *value )
{
	int perm = block->end[7];
	if( block->parent == NULL ){
		DaoByteCoder_Error( self, block, "Invalid scope!" );
		return;
	}

	if( block->parent == self->top ){
		DaoNamespace_AddConst( self->nspace, name, value, perm );
	}else if( block->parent->type == DAO_ASM_CLASS ){
		DaoClass *klass = DaoValue_CastClass( block->parent->value );
		DaoRoutine *rout = DaoValue_CastRoutine( value );
		if( rout && rout->routHost == klass->objType ){
			if( rout->attribs & DAO_ROUT_INITOR ){
				DaoClass_AddConst( klass, klass->initRoutine->routName, value, perm );
			}else if( rout->attribs & DAO_ROUT_CASTOR ){
				DaoClass_AddConst( klass, name, value, perm );
				if( klass->castOperators == NULL ){
					klass->castOperators = DaoRoutines_New( self->nspace, klass->objType, NULL);
					GC_IncRC( klass->castOperators );
				}
				DRoutines_Add( klass->castOperators->overloads, rout );
			}else{
				DaoClass_AddConst( klass, name, value, perm );
			}
		}else{
			DaoClass_AddConst( klass, name, value, perm );
		}
	}else if( block->parent->type == DAO_ASM_INTERFACE ){
		DaoRoutine *rout = DaoValue_CastRoutine( value );
		if( block->parent->value->type == DAO_INTERFACE ){
			DaoInterface *inter = (DaoInterface*) block->parent->value;
			DaoMethods_Insert( inter->methods, rout, self->nspace, inter->abtype );
		}else{
			DaoCinType *cintype = (DaoCinType*) block->parent->value;
			DaoMethods_Insert( cintype->methods, rout, self->nspace, cintype->vatype );
		}
	}else{
		/* No error here, it may be closure. */
	}
}
static void DaoByteCoder_DecodeEnum( DaoByteCoder *self, DaoByteBlock *block )
{
	uint_t A = DaoByteCoder_DecodeUInt16( block->begin );
	uint_t B = DaoByteCoder_DecodeUInt16( block->begin+2 );
	uint_t C = DaoByteCoder_DecodeUInt32( block->begin+4 );
	DaoByteBlock *pb, *namebk = DaoByteCoder_LookupStringBlock( self, block, A );
	DaoType *type;
	DString *name;

	if( self->error ) return;
	name = namebk->value->xString.value;
	type = DaoNamespace_FindType( self->nspace, name );
	if( type ){
		GC_Assign( & block->value, type );
		return;
	}
	type = DaoType_New( name->chars, DAO_ENUM, NULL, NULL );
	type->subtid = B;
	DaoByteCoder_CheckDataBlocks( self, block );
	for(pb=block->first; pb; pb=pb->next){
		A = DaoByteCoder_DecodeUInt16( pb->begin+0 );
		B = DaoByteCoder_DecodeUInt32( pb->begin+2 );
		namebk = DaoByteCoder_LookupStringBlock( self, pb, A );
		if( self->error ) goto Finalize;
		DMap_Insert( type->mapNames, namebk->value->xString.value, IntToPointer(B) );
	}
	if( C ){
		A = DaoByteCoder_DecodeUInt16( block->end+0 );
		B = DaoByteCoder_DecodeUInt32( block->end+2 );
		namebk = DaoByteCoder_LookupStringBlock( self, block, A );
		if( self->error ) goto Finalize;
		DMap_Insert( type->mapNames, namebk->value->xString.value, IntToPointer(B) );
	}
	DaoType_CheckAttributes( type );
	DaoNamespace_AddType( self->nspace, type->name, type );
Finalize:
	GC_Assign( & block->value, type );
}
static void DaoByteCoder_DecodeBases( DaoByteCoder *self, DaoByteBlock *block )
{
	uint_t i, D = self->ivalues->size;
	DaoByteBlock_GetAllValues( self, block, 4, -1, 1 );
	if( self->error ) return;
	if( block->parent->type == DAO_ASM_CLASS ){
		DaoClass *klass = DaoValue_CastClass( block->parent->value );
		for(i=D; i<self->ivalues->size; ++i){
			DaoClass_AddMixinClass( klass, self->ivalues->items.pClass[i] );
		}
		DaoClass_DeriveClassData( klass );
		DaoClass_DeriveObjectData( klass );
	}else if( block->parent->type == DAO_ASM_INTERFACE ){
		if( block->parent->value->type == DAO_INTERFACE ){
			DaoInterface *inter = DaoValue_CastInterface( block->parent->value );
			for(i=D; i<self->ivalues->size; ++i){
				DList_Append( inter->supers, self->ivalues->items.pValue[i] );
			}
			DaoInterface_DeriveMethods( inter );
		}else{
			DaoCinType *cintype = (DaoCinType*) block->parent->value;
			DaoInterface *inter = cintype->abstract;
			for(i=D; i<self->ivalues->size; ++i){
				DList_Append( cintype->supers, self->ivalues->items.pValue[i] );
			}
			DaoCinType_DeriveMethods( cintype );
			if( inter->concretes == NULL ){
				inter->concretes = DHash_New(0,DAO_DATA_VALUE);
			}
			DMap_Insert( inter->concretes, cintype->target, cintype );
		}
	}else{
		DaoByteCoder_Error( self, block, "Invalid scope!" );
	}
	DList_Erase( self->ivalues, D, -1 );
}
static void DaoByteCoder_DecodePatterns( DaoByteCoder *self, DaoByteBlock *block )
{
	uint_t i, D = self->ivalues->size;
	DaoByteBlock_GetAllValues( self, block, 4, -1, 1 );
	if( self->error ) return;
	if( block->parent->type == DAO_ASM_CLASS ){
		DaoClass *klass = DaoValue_CastClass( block->parent->value );
		if( klass->decoTargets == NULL ) klass->decoTargets = DList_New( DAO_DATA_STRING );
		for(i=D; i<self->ivalues->size; ++i){
			DString *pat = self->ivalues->items.pValue[i]->xString.value;
			DList_Append( klass->decoTargets, pat );
		}
	}else if( block->parent->type == DAO_ASM_ROUTINE ){
		DaoRoutine *routine = DaoValue_CastRoutine( block->parent->value );
		DaoRoutineBody *body = routine->body;
		if( body->decoTargets == NULL ) body->decoTargets = DList_New( DAO_DATA_STRING );
		for(i=D; i<self->ivalues->size; ++i){
			DString *pat = self->ivalues->items.pValue[i]->xString.value;
			DList_Append( routine->body->decoTargets, pat );
		}
	}else{
		DaoByteCoder_Error( self, block, "Invalid scope!" );
	}
	DList_Erase( self->ivalues, D, -1 );
}
static void DaoByteCoder_DecodeInterface( DaoByteCoder *self, DaoByteBlock *block )
{
	uint_t A = DaoByteCoder_DecodeUInt16( block->begin );
	uint_t B = DaoByteCoder_DecodeUInt16( block->begin+2 );
	uint_t C = DaoByteCoder_DecodeUInt16( block->begin+4 );
	DaoByteBlock *name = DaoByteCoder_LookupValueBlock( self, block, A );
	DaoByteBlock *target = DaoByteCoder_LookupBlock( self, block, B );
	DaoByteBlock *pb = block->first;
	DaoInterface *inter = NULL;
	DaoCinType *cintype = NULL;
	
	if( self->error ) return;
	if( name->value->type == DAO_INTERFACE ){
		inter = (DaoInterface*) name->value;
		if( target ){
			cintype = DaoCinType_New( inter, (DaoType*) target->value );
		}
	}else{
		inter = DaoInterface_New( name->value->xString.value->chars );
		DaoByteCoder_AddToScope( self, block, inter->abtype->name, (DaoValue*) inter );
	}

	GC_Assign( & block->value, cintype ? (DaoValue*) cintype : (DaoValue*) inter );

	if( self->error ) return;
	while( pb ){
		DaoByteCoder_DecodeBlock( self, pb );
		if( self->error ) return;
		pb = pb->next;
	}
}
static void DaoByteCoder_DecodeClass( DaoByteCoder *self, DaoByteBlock *block )
{
	uint_t A = DaoByteCoder_DecodeUInt16( block->begin );
	uint_t B = DaoByteCoder_DecodeUInt16( block->begin+2 );
	uint_t C = DaoByteCoder_DecodeUInt32( block->begin+4 );
	DaoByteBlock *name = DaoByteCoder_LookupValueBlock( self, block, A );
	DaoByteBlock *parent = DaoByteCoder_LookupBlock( self, block, B );
	DaoByteBlock *pb = block->first;
	DaoClass *klass = NULL;
	
	if( self->error ) return;
	if( name->value->type == DAO_CLASS ){
		klass = (DaoClass*) name->value;
	}else{
		klass = DaoClass_New();
		DaoClass_SetName( klass, name->value->xString.value, self->nspace );
		DaoByteCoder_AddToScope( self, block, klass->className, (DaoValue*) klass );
	}

	GC_Assign( & block->value, klass );
	if( self->error ) return;
	if( block->first == NULL ) return; /* Declaration only; */

	if( parent && parent->value ) DaoClass_AddSuperClass( klass, parent->value );

	while( pb ){
		DaoByteCoder_DecodeBlock( self, pb );
		if( self->error ) return;
		pb = pb->next;
	}
	DaoClass_UpdateMixinConstructors( klass );
	DaoClass_UpdateVirtualMethods( klass );
	DaoClass_UseMixinDecorators( klass );
	klass->intOperators = DaoClass_FindMethod( klass, "(int)", NULL );
	klass->eqOperators = DaoClass_FindMethod( klass, "==", NULL );
	klass->ltOperators = DaoClass_FindMethod( klass, "<", NULL );
	/* Check inferred attributes: */
	if( klass->attribs != (C & ~(DAO_CLS_INVAR|DAO_CLS_ASYNCHRONOUS)) ){
		DaoByteCoder_Error( self, block, "Class attributes not matching!" );
	}
	klass->attribs = C;
}
static int DaoByteCoder_VerifyRoutine( DaoByteCoder *self, DaoByteBlock *block )
{
	DaoVmCodeX *vmc2, *vmc = NULL;
	DMap *current, *outer = DHash_New(0,0);
	DList *outers = DList_New( DAO_DATA_MAP );
	DaoInferencer *inferencer;
	DaoRoutine *routine = (DaoRoutine*) block->value;
	int regCount = routine->body->regCount;
	int i, T, M, ret, N = routine->body->annotCodes->size;
	char buf[200];

	DList_PushBack( outers, outer );
	DList_PushBack( outers, outer );
	current = (DMap*) DList_Back( outers );
	for(i=0; i<routine->routType->nested->size; ++i){
		DMap_Insert( current, IntToPointer(i), 0 );
	}
	for(i=0; i<N; ++i){
		current = (DMap*) DList_Back( outers );
		vmc = (DaoVmCodeX*) routine->body->annotCodes->items.pVmc[i];

		if( vmc->code >= DVM_NULL ) goto InvalidInstruction;

#if 0
		DaoVmCodeX_Print( *vmc, NULL, buf );
		printf( "%3i: %s", i, buf );
#endif

		T = DaoVmCode_GetOpcodeType( (DaoVmCode*) vmc );
		switch( T ){
		case DAO_CODE_GETU :
			if( vmc->c >= regCount ) goto InvalidInstruction;
			DMap_Insert( current, IntToPointer( vmc->c ), 0 );
			if( vmc->a ){
				DMap *map = outers->items.pMap[vmc->a];
				if( vmc->a >= outers->size ) goto InvalidInstruction;
				if( vmc->b >= regCount ) goto InvalidInstruction;
				if( DMap_Find( map, IntToPointer(vmc->b) ) == NULL ) goto InvalidInstruction;
			}
			break;
		case DAO_CODE_SETU :
			if( vmc->a >= regCount ) goto InvalidInstruction;
			if( vmc->c ){
				DMap *map = outers->items.pMap[vmc->c];
				if( vmc->c >= outers->size ) goto InvalidInstruction;
				if( vmc->b >= regCount ) goto InvalidInstruction;
				if( DMap_Find( map, IntToPointer(vmc->b) ) == NULL ) goto InvalidInstruction;
			}
			break;
		case DAO_CODE_GETC :
		case DAO_CODE_GETG :
			if( vmc->c >= regCount ) goto InvalidInstruction;
			switch( vmc->code ){
			case DVM_GETVS :
			case DVM_GETVS_I : case DVM_GETVS_F : case DVM_GETVS_C :
				if( routine->body->upValues == NULL ) goto InvalidInstruction;
				if( vmc->b >= routine->body->upValues->size ) goto InvalidInstruction;
			}
			DMap_Insert( current, IntToPointer( vmc->c ), 0 );
			break;
		case DAO_CODE_SETG :
			if( vmc->a >= regCount ) goto InvalidInstruction;
			switch( vmc->code ){
			case DVM_SETVS :
			case DVM_SETVS_II : case DVM_SETVS_FF : case DVM_SETVS_CC :
				if( routine->body->upValues == NULL ) goto InvalidInstruction;
				if( vmc->b >= routine->body->upValues->size ) goto InvalidInstruction;
			}
			break;
		case DAO_CODE_GETF :
		case DAO_CODE_SETF :
		case DAO_CODE_MOVE :
		case DAO_CODE_UNARY :
			if( vmc->a >= regCount ) goto InvalidInstruction;
			if( vmc->c >= regCount ) goto InvalidInstruction;
			if( T != DAO_CODE_SETF ) DMap_Insert( current, IntToPointer( vmc->c ), 0 );
			break;
		case DAO_CODE_UNARY2 :
			if( vmc->b >= regCount ) goto InvalidInstruction;
			if( vmc->c >= regCount ) goto InvalidInstruction;
			DMap_Insert( current, IntToPointer( vmc->c ), 0 );
			break;
		case DAO_CODE_GETM :
		case DAO_CODE_SETM :
			if( vmc->a >= regCount ) goto InvalidInstruction;
			if( vmc->c >= regCount ) goto InvalidInstruction;
			if( T == DAO_CODE_GETM && (vmc->a + vmc->b) >= regCount ) goto InvalidInstruction;
			if( T == DAO_CODE_SETM && (vmc->c + vmc->b) >= regCount ) goto InvalidInstruction;
			if( T == DAO_CODE_GETM ) DMap_Insert( current, IntToPointer( vmc->c ), 0 );
			break;
		case DAO_CODE_GETI :
		case DAO_CODE_SETI :
		case DAO_CODE_BINARY :
			if( vmc->a >= regCount ) goto InvalidInstruction;
			if( vmc->b >= regCount ) goto InvalidInstruction;
			if( vmc->c >= regCount ) goto InvalidInstruction;
			if( T != DAO_CODE_SETI ) DMap_Insert( current, IntToPointer( vmc->c ), 0 );
			break;
		case DAO_CODE_MATRIX :
			M = (vmc->b>>8)*(vmc->b&0xff);
			if( vmc->a >= regCount ) goto InvalidInstruction;
			if( vmc->c >= regCount ) goto InvalidInstruction;
			if( (vmc->a + M) >= regCount ) goto InvalidInstruction;
			DMap_Insert( current, IntToPointer( vmc->c ), 0 );
			break;
		case DAO_CODE_ENUM :
			if( vmc->a >= regCount ) goto InvalidInstruction;
			if( vmc->c >= regCount ) goto InvalidInstruction;
			if( (vmc->a + (vmc->b&(0xffff>>2)) - 1) >= regCount ) goto InvalidInstruction;
			DMap_Insert( current, IntToPointer( vmc->c ), 0 );
			break;
		case DAO_CODE_YIELD :
			if( vmc->a >= regCount ) goto InvalidInstruction;
			if( vmc->c >= regCount ) goto InvalidInstruction;
			if( (vmc->a + (vmc->b&0xff) - 1) >= regCount ) goto InvalidInstruction;
			DMap_Insert( current, IntToPointer( vmc->c ), 0 );
			break;
		case DAO_CODE_CALL :
			M = vmc->b & 0xff;
			if( vmc->a >= regCount ) goto InvalidInstruction;
			if( vmc->c >= regCount ) goto InvalidInstruction;
			if( (vmc->a + M) >= regCount ) goto InvalidInstruction;
			DMap_Insert( current, IntToPointer( vmc->c ), 0 );
			break;
		case DAO_CODE_ENUM2 :
		case DAO_CODE_ROUTINE :
			if( vmc->a >= regCount ) goto InvalidInstruction;
			if( vmc->c >= regCount ) goto InvalidInstruction;
			if( (vmc->a + vmc->b) >= regCount ) goto InvalidInstruction;
			DMap_Insert( current, IntToPointer( vmc->c ), 0 );
			break;
		case DAO_CODE_EXPLIST :
			if( vmc->b == 0 ) break;
			if( vmc->a >= regCount ) goto InvalidInstruction;
			if( (vmc->a + vmc->b - 1) >= regCount ) goto InvalidInstruction;
			break;
		case DAO_CODE_BRANCH :
			if( vmc->a >= regCount ) goto InvalidInstruction;
			if( vmc->b >= N ) goto InvalidInstruction;
			break;
		case DAO_CODE_JUMP :
			if( vmc->b >= N ) goto InvalidInstruction;
			break;
		default :
			break;
		}
		switch( vmc->code ){
		case DVM_SECT :
			DList_PushBack( outers, outer );
			break;
		case DVM_GOTO :
			if( vmc->b >= i ) break;
			vmc2 = (DaoVmCodeX*) routine->body->annotCodes->items.pVmc[vmc->b+1];
			if( vmc2->code == DVM_SECT ) DList_PopBack( outers );
			break;
		}
	}
	DMap_Delete( outer );
	DList_Delete( outers );
	vmc = NULL;
	outer = NULL;
	outers = NULL;
	inferencer = DaoInferencer_New();
	DList_Resize( routine->body->regType, routine->body->regCount, NULL );
	DaoInferencer_Init( inferencer, routine, 0 );
	ret = DaoInferencer_DoInference( inferencer );
	DaoInferencer_Delete( inferencer );
	if( ret ) return 1;
InvalidInstruction:
	self->error = 1;
	DaoStream_WriteChars( self->vmspace->errorStream, "ERROR: code verification failed for " );
	DaoStream_WriteString( self->vmspace->errorStream, routine->routName );
	DaoStream_WriteChars( self->vmspace->errorStream, "()!\n" );
	if( vmc ){
		sprintf( buf, "%5i: ", i );
		DaoVmCodeX_Print( *vmc, NULL, buf + 7 );
		DaoStream_WriteChars( self->vmspace->errorStream, buf );
	}
	if( outers ) DList_Delete( outers );
	if( outer ) DMap_Delete( outer );
	return 0;
}
static void DaoByteCoder_DecodeRoutine( DaoByteCoder *self, DaoByteBlock *block )
{
	DaoRoutine *routine = NULL;
	DaoByteBlock *pb = block->first;
	DaoByteBlock *name, *type, *host;
	int i, ret, count, add = 0;
	uint_t A, B, C, D, ids[4];

	//DaoByteCoder_PrintBlock( self, block, 0 );

	DaoByteCoder_DecodeChunk2222( block->begin, & A, & B, & C, & D );
	if( block->end[6] ){  /* Default constructor; */
		host = DaoByteCoder_LookupTypeBlock( self, block, C );
		if( self->error ) return;
		routine = host->value->xType.aux->xClass.initRoutine;
		add = 1;
	}else if( A ){
		name = DaoByteCoder_LookupValueBlock( self, block, A );
		if( self->error ) return;
		if( name->value->type == DAO_ROUTINE ){
			routine = (DaoRoutine*) name->value;
		}else{
			routine = DaoRoutine_New( self->nspace, NULL, 1);
			DString_Assign( routine->routName, name->value->xString.value );
			add = 1;
		}
	}else if( block == self->top ){
		routine = DaoRoutine_New( self->nspace, NULL, 1);
		self->nspace->mainRoutine = routine;
		DString_SetChars( routine->routName, "__main__" );
		DList_Append( self->nspace->mainRoutines, routine );
		DaoNamespace_SetConst( self->nspace, DVR_NSC_MAIN, (DaoValue*) routine );
	}
	if( routine == NULL ){
		DaoByteCoder_Error( self, block, "Invalid routine block!" );
		return;
	}

	GC_Assign( & block->value, routine );
	routine->attribs = D;
	if( B ){
		type = DaoByteCoder_LookupTypeBlock( self, block, B );
		if( self->error ) return;
		GC_Assign( & routine->routType, type->value );
		routine->parCount = routine->routType->nested->size;
		if( routine->routType->variadic ) routine->parCount = DAO_MAX_PARAM;
	}else{
		GC_Assign( & routine->routType, dao_type_routine );
	}
	if( C ){
		host = DaoByteCoder_LookupTypeBlock( self, block, C );
		if( self->error ) return;
		GC_Assign( & routine->routHost, host->value );
	}
	if( block->parent && block->parent->type == DAO_ASM_CLASS && block->first != NULL ){
		DaoClass *klass = (DaoClass*) block->parent->value;
		if( routine->routHost == klass->objType && block->prev != NULL ){
			if( block->prev->type != DAO_ASM_ROUTINE || block->prev->first == NULL ){
				/* Not a routine block, or just a routine declation block: */
				DaoClass_UpdateAttributes( klass );
			}
		}
	}
	if( add ) DaoByteCoder_AddToScope( self, block, routine->routName, (DaoValue*) routine );

	DaoByteCoder_DecodeChunk2222( block->end, & A, & B, & C, & D );
	routine->body->regCount = A;
	if( B ){
		if( routine->body->upValues == NULL ){
			routine->body->upValues = DList_New( DAO_DATA_VALUE );
		}
		while( routine->body->upValues->size < B ){
			DList_Append( routine->body->upValues, DaoVariable_New(NULL,NULL,0) );
		}
	}

	/* Set proper namespace: */
	GC_Assign( & routine->nameSpace, self->nspace );

	if( block->first == NULL ) return; /* Declaration only; */

	for(pb=block->first; pb; pb = pb->next){
		DaoByteCoder_DecodeBlock( self, pb );
		if( self->error ) return;
	}

	ret = DaoByteCoder_VerifyRoutine( self, block );
	if( self->error ) return;

	for(pb=block->first; pb; pb = pb->next){
		DaoValue *selfpar = NULL;
		DaoValue *params[DAO_MAX_PARAM+1];
		DaoObject object, *obj = & object;
		if( pb->type != DAO_ASM_DECOS ) continue;
#ifdef DAO_WITH_DECORATOR
		DaoByteCoder_DecodeChunk2222( pb->begin, ids, ids+1, ids+2, ids+3 );
		if( routine->routHost ){
			/* To circumvent the default object issue for type matching: */
			object = *(DaoObject*) routine->routHost->value;
			selfpar = (DaoValue*) obj;
		}
		params[0] = (DaoValue*) routine;
		for(i=0; i<4 && ids[i] && ids[i+1]; i+=2){
			DaoByteBlock *B1, *B2;
			DaoRoutine *decoFunc;
			DaoList *decoParam;
			int j, n;
			B1 = DaoByteCoder_LookupBlock3( self, pb, ids[i], DAO_ROUTINE, "routine" );
			B2 = DaoByteCoder_LookupBlock3( self, pb, ids[+1], DAO_LIST, "list" );
			if( self->error ) return;
			decoFunc = DaoValue_CastRoutine( B1->value );
			decoParam = DaoValue_CastList( B2->value );
			n = decoParam->value->size;
			for(j=0; j<n; j++) params[j+1] = decoParam->value->items.pValue[j];
			decoFunc = DaoRoutine_ResolveX( decoFunc, selfpar, NULL, params, NULL, n+1, 0 );
			if( decoFunc == NULL || DaoRoutine_Decorate( routine, decoFunc, params, n+1, 1 ) == NULL ){
				DaoByteCoder_Error( self, block, "Routine decoration failed!" );
				return;
			}
		}
#else
		DaoByteCoder_Error( self, block, "Decorator is not enabled!" );
#endif
	}
}
static void DaoByteCoder_DecodeRoutineConsts( DaoByteCoder *self, DaoByteBlock *block )
{
	DaoByteBlock *pb = block->first;
	DaoRoutine *routine = (DaoRoutine*) block->parent->value;
	int i, count = DaoByteCoder_DecodeUInt16( block->begin );
	int offset = self->iblocks->size;

	if( block->parent == NULL || block->parent->type != DAO_ASM_ROUTINE ){
		DaoByteCoder_Error( self, block, "Invalid context for the block!" );
		return;
	}

	DaoByteBlock_GetAllBlocks( self, block, 3, count, 0 );
	if( self->error ) return;
	//DaoByteCoder_PrintBlock( self, block, 0 );
	for(i=0; i<count; ++i){
		DaoByteBlock *block = (DaoByteBlock*) self->iblocks->items.pVoid[offset+i];
		DaoRoutine_AddConstant( routine, block ? block->value : NULL );
	}
	DList_Erase( self->iblocks, offset, -1 );
}
static void DaoByteCoder_DecodeRoutineTypes( DaoByteCoder *self, DaoByteBlock *block )
{
	DaoByteBlock *pb = block->first;
	DaoRoutine *routine = (DaoRoutine*) block->parent->value;
	int i, max, count = DaoByteCoder_DecodeUInt16( block->begin );
	int offset1 = self->indices->size;
	int offset2 = self->iblocks->size;
	//DaoByteCoder_PrintBlock( self, block, 0 );

	if( block->parent == NULL || block->parent->type != DAO_ASM_ROUTINE ){
		DaoByteCoder_Error( self, block, "Invalid context for the block!" );
		return;
	}

	DaoByteCoder_CheckDataBlocks( self, block );
	if( self->error ) return;
	if( count ){
		DaoByteCoder_GetIntegers( self, self->indices, block->begin + 4, block->begin + 8, 4 );
		DaoByteCoder_GetBlocks( self, block, block->begin + 6, block->begin + 8, 4, 0 );
		if( self->error ) return;
		i = DaoByteCoder_DecodeUInt16( block->begin + 6 );
	}
	while( pb ){
		DaoByteCoder_GetIntegers( self, self->indices, pb->begin, pb->begin + 8, 4 );
		DaoByteCoder_GetBlocks( self, block, pb->begin + 2, pb->begin + 8, 4, 0 );
		if( self->error ) return;
		pb = pb->next;
	}
	max = (count - 1) % 2;
	if( max == 0 ) max = 2;
	DaoByteCoder_GetIntegers( self, self->indices, block->end, block->end + 2*(max+1), 4 );
	DaoByteCoder_GetBlocks( self, block, block->end + 2, block->end + 2 + 2*(max+1), 4, 0 );
	if( self->error ) return;

	for(i=0; i<count; ++i){
		DaoByteBlock *block = (DaoByteBlock*) self->iblocks->items.pVoid[offset2+i];
		daoint idx = self->indices->items.pInt[offset1+i];
		MAP_Insert( routine->body->localVarType, idx, (DaoType*) block->value );
	}
	DList_Erase( self->indices, offset1, -1 );
	DList_Erase( self->iblocks, offset2, -1 );
}
static void DaoByteCoder_DecodeRoutineCode( DaoByteCoder *self, DaoByteBlock *block )
{
	DaoByteBlock *pb = block->first;
	DaoRoutine *routine = (DaoRoutine*) block->parent->value;
	int numcode = DaoByteCoder_DecodeUInt16( block->begin );
	int numlines = DaoByteCoder_DecodeUInt16( block->begin+2 );
	int defline = DaoByteCoder_DecodeUInt16( block->end );
	int offset1 = self->lines->size;
	int offset2 = self->indices->size;
	int i, k, m, useGlobal = 0;

	if( block->parent == NULL || block->parent->type != DAO_ASM_ROUTINE ){
		DaoByteCoder_Error( self, block, "Invalid context for the block!" );
		return;
	}

	DaoByteCoder_CheckDataBlocks( self, block );
	if( self->error ) return;
	if( numlines ){
		DaoByteCoder_GetIntegers( self, self->lines, block->begin + 4, block->begin + 8, 2 );
		numlines -= 1;
	}
	while( pb ){
		if( numlines > 0 ){
			DaoByteCoder_GetIntegers( self, self->lines, pb->begin, pb->begin + 8, 2 );
		}else{
			DaoByteCoder_GetIntegers( self, self->indices, pb->begin, pb->begin + 8, 2 );
		}
		numlines -= 2;
		pb = pb->next;
	}
	DaoByteCoder_GetIntegers( self, self->indices, block->end, block->end + 8, 2 );

	numlines = DaoByteCoder_DecodeUInt16( block->begin+2 );
	for(i=offset1+2; i<self->lines->size; i+=2){
		short diff = (short) self->lines->items.pInt[i];
		self->lines->items.pInt[i] = self->lines->items.pInt[i-2] + diff;
	}

	//DaoByteCoder_PrintBlock( self, block, 0 );
	numcode = self->indices->size - offset2;
	m = self->lines->size > offset1+1 ? self->lines->items.pInt[offset1+1] : 0;
	for(i=0, k=1; i<numcode; i+=4){
		DMap *lookupTable = NULL;
		daoint *idx = self->indices->items.pInt + offset2 + i;
		DaoVmCodeX vmc = {0,0,0,0,0,0,0,0,0,0};
		vmc.code = idx[0];
		vmc.a = idx[1];
		vmc.b = idx[2];
		vmc.c = idx[3];
		if( i/4 >= m && k < numlines ){
			m += self->lines->items.pInt[offset1+2*k+1];
			k += 1;
		}
		vmc.line = k <= numlines ? self->lines->items.pInt[offset1+2*(k-1)] : defline;
		//printf( "%4i: ", i );DaoVmCodeX_Print( vmc, NULL, NULL );
		switch( vmc.code ){
#if 0
		case DVM_GETCK : case DVM_GETVK : case DVM_SETVK :
			break;
		case DVM_GETVO : case DVM_SETVO :
			break;
#endif
		case DVM_GETCG :
		case DVM_GETCG_I : case DVM_GETCG_F : case DVM_GETCG_C :
			lookupTable = routine->nameSpace->lookupTable;
			break;
		case DVM_GETVG :
		case DVM_GETVG_I : case DVM_GETVG_F : case DVM_GETVG_C :
		case DVM_SETVG :
		case DVM_SETVG_II : case DVM_SETVG_FF : case DVM_SETVG_CC :
			lookupTable = routine->nameSpace->lookupTable;
			useGlobal = 1;
			break;
		}
		if( lookupTable != NULL ){
			DaoByteBlock *pb = DaoByteCoder_LookupStringBlock( self, block, vmc.b );
			DString *name = pb->value->xString.value;
			DNode *it = DMap_Find( lookupTable, name );
			if( it == NULL ){
				DaoByteCoder_Error3( self, block, "global name \"%s\" not found!", name->chars );
				break;
			}
			vmc.b = LOOKUP_ID( it->value.pInt );
		}
		DList_Append( routine->body->annotCodes, & vmc );
		DArray_PushCode( routine->body->vmCodes, * (DaoVmCode*) & vmc );
	}
	if( useGlobal ) DList_Append( self->routines, routine );
	DList_Erase( self->lines, offset1, -1 );
	DList_Erase( self->indices, offset2, -1 );
}
DaoProcess* DaoNamespace_ReserveFoldingOperands( DaoNamespace *self, int N );
static void DaoByteCoder_EvaluateValue( DaoByteCoder *self, DaoByteBlock *block )
{
	DaoProcess *process;
	DaoType *retype = NULL;
	DaoValue *value = NULL;
	DaoVmCode vmcode = {0,1,2,0};
	DaoByteBlock *pb = block->first;
	DaoRoutine *routine = DaoValue_CastRoutine( block->parent->value );
	DaoClass *klass = DaoValue_CastClass( block->parent->value );
	uint_t idx, offset = self->iblocks->size;
	int i, count, mode;

	vmcode.code = DaoByteCoder_DecodeUInt16( block->begin );
	vmcode.b = DaoByteCoder_DecodeUInt16( block->begin + 2 );
	mode = DaoByteCoder_DecodeUInt16( block->begin + 4 );
	if( vmcode.code == DVM_GETCG || vmcode.code == DVM_GETCK ){
		int idx = DaoByteCoder_DecodeUInt16( block->end );
		DaoByteBlock *namebk = DaoByteCoder_LookupStringBlock( self, block, idx );
		DString *name;
		if( self->error ) return;
		name = namebk->value->xString.value;
		if( vmcode.code == DVM_GETCG ){
			idx = DaoNamespace_FindConst( self->nspace, name );
			if( idx < 0 ) goto ConstantNotFound;
			value = DaoNamespace_GetConst( self->nspace, idx );
			goto Done;
		}else{
			DaoClass *klass = NULL;
			if( block->parent->type == DAO_ASM_CLASS ){
				klass = DaoValue_CastClass( block->parent->value );
			}else if( block->parent->type == DAO_ASM_ROUTINE ){
				DaoRoutine *rout = DaoValue_CastRoutine( block->parent->value );
				if( rout && rout->routHost ) klass = DaoValue_CastClass( rout->routHost->aux );
			}
			if( klass == NULL ){
				DaoByteCoder_Error( self, block, "Invalid context for the block!" );
				return;
			}
			idx = DaoClass_FindConst( klass, name );
			if( idx < 0 ) goto ConstantNotFound;
			value = DaoClass_GetConst( klass, idx );
			goto Done;
		}
ConstantNotFound:
		DaoByteCoder_Error3( self, block, "constant \"%s\" not found!", name->chars );
		return;
	}

	idx = DaoByteCoder_DecodeUInt16( block->begin + 6 );
	if( idx ){
		DaoByteBlock *bk = DaoByteCoder_LookupTypeBlock( self, block, idx );
		if( self->error ) return;
		retype = (DaoType*) bk->value;
	}

	DaoByteCoder_CheckDataBlocks( self, block );
	if( self->error ) return;
	while( pb ){
		DaoByteCoder_GetBlocks( self, block, pb->begin, pb->begin + 8, 2, 0 );
		if( self->error ) return;
		pb = pb->next;
	}
	DaoByteCoder_GetBlocks( self, block, block->end, block->end + 8, 2, 1 );
	if( self->error ) return;

	count = self->iblocks->size - offset;
	process = DaoNamespace_ReserveFoldingOperands( self->nspace, count + 1 );
	if( vmcode.code == DVM_GETF ){
		DaoByteBlock *block = (DaoByteBlock*) self->iblocks->items.pVoid[offset+1];
		vmcode.b = DaoRoutine_AddConstant( process->activeRoutine, block->value );
	}else if( vmcode.code == DVM_NAMEVA ){
		DaoByteBlock *block = (DaoByteBlock*) self->iblocks->items.pVoid[offset];
		vmcode.a = DaoRoutine_AddConstant( process->activeRoutine, block->value );
	}
	for(i=0; i<count; ++i){
		DaoByteBlock *block = (DaoByteBlock*) self->iblocks->items.pVoid[offset+i];
		DaoValue *value = block->value ? block->value : dao_none_value;
		DaoValue_Copy( value, & process->activeValues[i+1] );
	}
	GC_Assign( & process->activeTypes[0], retype );
	process->activeCode = & vmcode;
	if( routine && routine->routHost ){
		GC_Assign( & process->activeRoutine->routHost, routine->routHost );
	}else if( klass ){
		GC_Assign( & process->activeRoutine->routHost, klass->objType );
	}
	value = DaoProcess_MakeConst( process, mode );

	if( process->exceptions->size ){
		DaoByteCoder_Error( self, block, "Constant evaluation failed!" );
		DaoProcess_PrintException( process, self->nspace->vmSpace->errorStream, 1 );
	}
	GC_DecRC( process->activeRoutine->routHost );
	process->activeRoutine->routHost = NULL;

Done:
	//DaoByteCoder_PrintBlock( self, block, 0, 0 );
	//printf( "here: %p\n", value );
	DList_Erase( self->iblocks, offset, -1 );
	GC_Assign( & block->value, value );
}
static void DaoByteCoder_DecodeDeclaration( DaoByteCoder *self, DaoByteBlock *block )
{
	uint_t A = DaoByteCoder_DecodeUInt16( block->begin + 0 );
	uint_t B = DaoByteCoder_DecodeUInt16( block->begin + 2 );
	uint_t C = DaoByteCoder_DecodeUInt16( block->begin + 4 );
	uint_t global = block->begin[6];
	uint_t perm = block->begin[7];
	DaoByteBlock *name0  = A ? DaoByteCoder_LookupStringBlock( self, block, A ) : NULL;
	DaoByteBlock *value0 = DaoByteCoder_LookupBlock( self, block, B );
	DaoByteBlock *type0  = DaoByteCoder_LookupBlock( self, block, C );
	DaoValue *value = value0 ? value0->value : NULL;
	DaoType *type = type0 ? (DaoType*) type0->value : NULL;
	DString *name = name0 ? name0->value->xString.value : NULL;

	//DaoByteCoder_PrintBlock( self, block, 0, 0 );
	if( block->parent == self->top || global == 1 ){
		switch( block->type ){
		case DAO_ASM_CONST   :
			DaoNamespace_AddConst( self->nspace, name, value, perm );
			break;
		case DAO_ASM_GLOBAL  :
			DaoNamespace_AddVariable( self->nspace, name, value, type, perm );
			break;
		default: goto Error;
		}
	}else if( block->parent->type == DAO_ASM_ROUTINE ){
		DaoRoutine *routine = (DaoRoutine*) block->parent->value;
		if( block->type == DAO_ASM_STATIC ){
			DaoVariable *var = DaoVariable_New( value, type, DAO_GLOBAL_VARIABLE );
			int i = LOOKUP_BIND( DAO_GLOBAL_VARIABLE, 0, 0, self->nspace->variables->size );
			MAP_Insert( self->nspace->lookupTable, name, i );
			DList_Append( self->nspace->variables, var );
		}else{
			goto Error;
		}
	}else if( block->parent->type == DAO_ASM_CLASS ){
		DaoClass *klass = (DaoClass*) block->parent->value;
		switch( block->type ){
		case DAO_ASM_CONST  : DaoClass_AddConst( klass, name, value, perm ); break;
		case DAO_ASM_VAR    : DaoClass_AddObjectVar( klass, name, value, type, perm ); break;
		case DAO_ASM_STATIC : DaoClass_AddGlobalVar( klass, name, value, type, perm ); break;
		default: goto Error;
		}
	}else{
		goto Error;
	}
	return;
Error:
	DaoByteCoder_Error( self, block, "Invalid declaration!" );
}
static void DaoByteCoder_LoadModule( DaoByteCoder *self, DaoByteBlock *block )
{
	uint_t A = DaoByteCoder_DecodeUInt16( block->begin );
	uint_t B = DaoByteCoder_DecodeUInt16( block->begin+2 );
	DaoByteBlock *path = DaoByteCoder_LookupStringBlock( self, block, A );
	DaoByteBlock *mod = DaoByteCoder_LookupBlock( self, block, B );
	DaoNamespace *ns = NULL;
	DString *spath;

	if( self->error ) return;
	if( B && (mod == NULL || mod->value == NULL || mod->value->type != DAO_STRING) ){
		DaoByteCoder_Error( self, block, "Invalid module name!" );
		return;
	}
	spath = DString_Copy( path->value->xString.value );
	if( (ns = DaoNamespace_FindNamespace(self->nspace, spath)) == NULL ){
		ns = DaoVmSpace_LoadModule( self->nspace->vmSpace, spath );
	}
	if( ns == NULL ){
		self->error = 1;
		DaoByteCoder_Error3( self, block, "Module loading failed for %s!", spath->chars );
	}
	DString_Delete( spath );
	if( self->error ) return;

	GC_Assign( & block->value, ns );

	if( mod == NULL ){
		if( DaoNamespace_AddParent( self->nspace, ns ) == 0 ){
			DaoByteCoder_Error( self, block, "Cyclic loading!" );
		}
	}else{
		DaoNamespace_AddConst( self->nspace, mod->value->xString.value, (DaoValue*) ns, 0 );
	}
}
static void DaoByteCoder_DecodeNamespace( DaoByteCoder *self, DaoByteBlock *block )
{
	uint_t A = DaoByteCoder_DecodeUInt16( block->begin + 0 );
	uint_t B = DaoByteCoder_DecodeUInt16( block->begin + 2 );
	DaoByteBlock *nameOfDef = DaoByteCoder_LookupValueBlock( self, block, A );
	DaoByteBlock *scope = DaoByteCoder_LookupBlock( self, block, B );
	DaoValue *NS = NULL;

	if( self->error ) return;
	if( nameOfDef->value->type == DAO_NAMESPACE ){
		NS = nameOfDef->value;
	}else{
		DString *name = nameOfDef->value->xString.value;
		NS = (DaoValue*) DaoNamespace_New( self->vmspace, name->chars );
		if( scope ){
			DaoNamespace_AddConst( (DaoNamespace*) scope->value, name, NS, DAO_PERM_PUBLIC );
		}else{
			DaoNamespace_AddConst( self->nspace, name, NS, DAO_PERM_PUBLIC );
		}
	}
	GC_Assign( & block->value, NS );
}
static void DaoByteCoder_DecodeExport( DaoByteCoder *self, DaoByteBlock *block )
{
	uint_t i, ids[4];
	int st, id, lookup, count; 
	DaoByteBlock *nsblock, *symblock;
	DaoNamespace *defns;
	DString *symbol;
	DNode *it1, *it2;

	DaoByteCoder_DecodeChunk2222( block->begin, ids, ids+1, ids+2, ids+3 );
	nsblock = DaoByteCoder_LookupValueBlock( self, block, ids[0] );
	if( self->error ) goto InvalidExport;
	if( nsblock->value->type != DAO_NAMESPACE ) goto InvalidExport;
	defns = (DaoNamespace*) nsblock->value;
	for(i=1; i<=3; ++i){
		if( ids[i] == 0 ) break;
		symblock = DaoByteCoder_LookupStringBlock( self, block, ids[i] );
		if( self->error ) goto InvalidExport;
		symbol = symblock->value->xString.value;

		it1 = DMap_Find( self->nspace->lookupTable, symbol );
		it2 = DMap_Find( defns->lookupTable, symbol );
		if( it1 == NULL || it2 != NULL ) goto InvalidExport;
		count = 0;
		st = LOOKUP_ST( it1->value.pInt );
		id = LOOKUP_ID( it1->value.pInt );
		if( st == DAO_GLOBAL_CONSTANT ){
			count = defns->constants->size;
			DList_Append( defns->constants, self->nspace->constants->items.pVoid[id] );
		}else{
			count = defns->variables->size;
			DList_Append( defns->variables, self->nspace->variables->items.pVoid[id] );
		}    
		lookup = LOOKUP_BIND( st, DAO_PERM_PUBLIC, 0, count );
		MAP_Insert( defns->lookupTable, symbol, lookup );
	}
	return;
InvalidExport:
	DaoByteCoder_Error( self, block, "Invalid export statement!" );
}
static void DaoByteCoder_DecodeImport( DaoByteCoder *self, DaoByteBlock *block )
{
	DNode *it;
	DString *name, *tag;
	DaoValue *value;
	DaoVariable *var;
	DaoNamespace *NS = self->nspace;
	DaoByteBlock *modBlock, *namebk, *scopebk;
	DaoNamespace *mod = NULL;
	int i, st, pm, index = -1;
	uint_t A, B, C, D;

	DaoByteCoder_DecodeChunk2222( block->begin, & A, & B, & C, & D );
	modBlock = DaoByteCoder_LookupValueBlock( self, block, A );

	if( self->error ) goto InvalidImport;
	if( block->parent == NULL || block->parent->type != DAO_ASM_ROUTINE ) goto InvalidScope;
	if( modBlock->value->type != DAO_NAMESPACE ) goto InvalidImport;

	mod = (DaoNamespace*) modBlock->value;
	if( B == 0 ){
		if( C != 0 ) goto InvalidImport;
		DaoNamespace_AddParent( self->nspace, mod );
		return;
	}
	namebk = DaoByteCoder_LookupStringBlock( self, block, B );
	name = namebk->value->xString.value;
	it = DMap_Find( mod->lookupTable, name );
	if( it == NULL || LOOKUP_PM(it->value.pInt) != DAO_PERM_PUBLIC ) goto InvalidImport;
	st = LOOKUP_ST( it->value.pInt );
	pm = LOOKUP_PM( it->value.pInt );
	if( C ){
		char suffix[32];
		sprintf( suffix, "{%i}[%i]", C, D );
		name = DString_Copy( name );
		DString_AppendChars( name, suffix );
		pm = DAO_PERM_NONE;
	}
	if( st == DAO_GLOBAL_CONSTANT ){
		value = mod->constants->items.pConst[ LOOKUP_ID(it->value.pInt) ]->value;
		index = DaoNamespace_AddConst( NS, name, value, pm );
	}else{
		var = mod->variables->items.pVar[ LOOKUP_ID(it->value.pInt) ];
		index = DaoNamespace_AddVariable( NS, name, var->value, var->dtype, pm );
	}
	if( name != namebk->value->xString.value ) DString_Delete( name );
	if( index < 0 ) goto InvalidImport;
	return;

InvalidScope:
	DaoByteCoder_Error( self, block, "Invalid scope for importing!" );
	return;
InvalidImport:
	DaoByteCoder_Error( self, block, "Invalid import statement!" );
}
static void DaoByteCoder_DecodeVerbatim( DaoByteCoder *self, DaoByteBlock *block )
{
	uint_t A = DaoByteCoder_DecodeUInt16( block->begin );
	uint_t B = DaoByteCoder_DecodeUInt16( block->begin+2 );
	uint_t C = DaoByteCoder_DecodeUInt16( block->begin+4 );
	uint_t D = DaoByteCoder_DecodeUInt16( block->begin+6 );
	DaoByteBlock *tagbk = DaoByteCoder_LookupStringBlock( self, block, A );
	DaoByteBlock *modebk = DaoByteCoder_LookupStringBlock( self, block, B );
	DaoByteBlock *textbk = DaoByteCoder_LookupStringBlock( self, block, C );
	DString *tag, *mode, *text, *output;
	DaoCodeInliner inliner;

	if( self->error ) return;

	tag = tagbk->value->xString.value;
	mode = modebk->value->xString.value;
	text = textbk->value->xString.value;
	inliner = DaoNamespace_FindCodeInliner( self->nspace, tag );
	if( inliner == NULL ){
		/*
		// It is an error, since this block was encoded only
		// when the inliner was present.
		*/
		DaoByteCoder_Error( self, NULL, "inliner not found!" );
		DaoByteCoder_Error( self, block, tag->chars );
		return;
	}
	output = DString_New();
	if( (*inliner)( self->nspace, mode, text, output, D ) ){
		DaoByteCoder_Error( self, NULL, "code inlining failed:" );
		DaoByteCoder_Error( self, block, output->chars );
	}
	DString_Delete( output );
}
static void DaoByteCoder_DecodeBlock( DaoByteCoder *self, DaoByteBlock *block )
{
	DaoByteBlock *bk;
	uint_t A;

	if( (block->type && block->type <= DAO_ASM_EVAL) || block->type == DAO_ASM_SEEK ){
		DList_PushBack( self->stack, block );
	}
	switch( block->type ){
	case DAO_ASM_SEEK :
		A = DaoByteCoder_DecodeUInt16( block->begin );
		bk = DaoByteCoder_LookupValueBlock( self, block, A );
		if( self->error ) return;
		GC_Assign( & block->value, bk->value );
		break;
	case DAO_ASM_CONST     :
	case DAO_ASM_VAR       :
	case DAO_ASM_STATIC    :
	case DAO_ASM_GLOBAL    : DaoByteCoder_DecodeDeclaration( self, block ); break;
	case DAO_ASM_VERBATIM  : DaoByteCoder_DecodeVerbatim( self, block ); break;
	case DAO_ASM_COPY      : DaoByteCoder_DecodeCopyValue( self, block ); break;
	case DAO_ASM_VALUE     : DaoByteCoder_DecodeValue( self, block ); break;
	case DAO_ASM_TYPE      : DaoByteCoder_DecodeType( self, block ); break;
	case DAO_ASM_ENUM      : DaoByteCoder_DecodeEnum( self, block ); break;
	case DAO_ASM_INTERFACE : DaoByteCoder_DecodeInterface( self, block ); break;
	case DAO_ASM_CLASS     : DaoByteCoder_DecodeClass( self, block ); break;
	case DAO_ASM_ROUTINE   : DaoByteCoder_DecodeRoutine( self, block ); break;
	case DAO_ASM_EVAL      : DaoByteCoder_EvaluateValue( self, block ); break;
	case DAO_ASM_LOAD      : DaoByteCoder_LoadModule( self, block ); break;
	case DAO_ASM_CONSTS    : DaoByteCoder_DecodeRoutineConsts( self, block ); break;
	case DAO_ASM_TYPES     : DaoByteCoder_DecodeRoutineTypes( self, block ); break;
	case DAO_ASM_CODE      : DaoByteCoder_DecodeRoutineCode( self, block ); break;
	case DAO_ASM_NAMESPACE : DaoByteCoder_DecodeNamespace( self, block ); break;
	case DAO_ASM_EXPORT    : DaoByteCoder_DecodeExport( self, block ); break;
	case DAO_ASM_IMPORT    : DaoByteCoder_DecodeImport( self, block ); break;
	case DAO_ASM_AUXTYPE   : DaoByteCoder_DecodeAuxType( self, block ); break;
	case DAO_ASM_TYPEDEF   : DaoByteCoder_DecodeTypeAlias( self, block ); break;
	case DAO_ASM_TYPEOF    : DaoByteCoder_DecodeTypeOf( self, block ); break;
	case DAO_ASM_BASES     : DaoByteCoder_DecodeBases( self, block ); break;
	case DAO_ASM_PATTERNS  : DaoByteCoder_DecodePatterns( self, block ); break;
	case DAO_ASM_DECOS : break;
	default: break;
	}
}
int DaoByteCoder_Build( DaoByteCoder *self, DaoNamespace *nspace )
{
	int i;

	if( self->top == NULL ) return 0;
	DaoByteCoder_SplitData( self, self->top );

	self->error = 0;
	self->stack->size = 0;
	self->ivalues->size = 0;
	self->iblocks->size = 0;
	self->indices->size = 0;
	self->routines->size = 0;
	self->nspace = nspace;
	DaoByteCoder_DecodeBlock( self, self->top );
	for(i=0; i<self->routines->size && self->error == 0; i++){
		DaoRoutine *rout = (DaoRoutine*) self->routines->items.pValue[i];
		self->error |= DaoRoutine_DoTypeInference( rout, 0 ) == 0;
	}
	return self->error == 0;
}



static void DaoStream_PrintDaoInt( DaoStream *self, const char *fmt, dao_integer A )
{
	char buffer[128];
	snprintf( buffer, sizeof(buffer), fmt, A );
	DaoStream_WriteChars( self, buffer );
}
static void DaoStream_PrintFloat( DaoStream *self, const char *fmt, double A )
{
	char buffer[128];
	snprintf( buffer, sizeof(buffer), fmt, A );
	DaoStream_WriteChars( self, buffer );
}
static void DaoStream_PrintInt( DaoStream *self, const char *fmt, int A )
{
	char buffer[128];
	snprintf( buffer, sizeof(buffer), fmt, A );
	DaoStream_WriteChars( self, buffer );
}
static void DaoStream_PrintInt2( DaoStream *self, const char *fmt, int A, int B )
{
	char buffer[128];
	snprintf( buffer, sizeof(buffer), fmt, A, B );
	DaoStream_WriteChars( self, buffer );
}
static void DaoStream_PrintInt3( DaoStream *self, const char *fmt, int A, int B, int C )
{
	char buffer[128];
	snprintf( buffer, sizeof(buffer), fmt, A, B, C );
	DaoStream_WriteChars( self, buffer );
}
static void DaoStream_PrintInt4( DaoStream *self, const char *fmt, int A, int B, int C, int D )
{
	char buffer[128];
	snprintf( buffer, sizeof(buffer), fmt, A, B, C, D );
	DaoStream_WriteChars( self, buffer );
}
static void DaoStream_PrintString( DaoStream *self, const char *fmt, const char *string )
{
	char buffer[128];
	snprintf( buffer, sizeof(buffer), fmt, string );
	DaoStream_WriteChars( self, buffer );
}
static void DaoStream_PrintChar( DaoStream *self, const char *fmt, int ch )
{
	char buffer[128];
	snprintf( buffer, sizeof(buffer), fmt, ch );
	DaoStream_WriteChars( self, buffer );
}

static void DaoStream_PrintSubBlocks( DaoStream *self, DaoByteBlock *block, int spaces )
{
	DaoVmSpace *vms = block->coder->vmspace;
	DaoByteBlock *pb = block->first;
	while( pb ){
		if( pb != block->first && pb->type != DAO_ASM_END && pb->type != DAO_ASM_DATA && pb->type != DAO_ASM_DATA2 ) DaoStream_WriteChars( self, "\n" );
		DaoByteCoder_PrintBlock( block->coder, pb, spaces, self == vms->errorStream );
		pb = pb->next;
	}
}
static void DaoStream_PrintTag( DaoStream *self, int tag, int spaces )
{
	while( spaces -- ) DaoStream_WriteChars( self, " " );
	DaoStream_PrintString( self, "%s: ", DaoByteCoder_GetASM( tag ) );
}
static void DaoStream_PrintChars( DaoStream *self, uchar_t *chars, int count )
{
	int i;
	DaoStream_WriteChars( self, "\'" );
	for(i=0; i<count; ++i){
		int ch = chars[i];
		if( isprint(ch) ){
			DaoStream_PrintChar( self, "%c", ch );
		}else{
			DaoStream_PrintChar( self, "\\%i", ch );
		}
	}
	DaoStream_WriteChars( self, "\';\n" );
}
void DaoByteCoder_PrintBlock( DaoByteCoder *self, DaoByteBlock *block, int spaces, int error )
{
	int lines;
	daoint k, m;
	uint_t i, A, B, C, D = 0;
	DaoByteBlock *pb = block->first;
	DaoStream *stream = self->vmspace->stdioStream;

	if( error ) stream = self->vmspace->errorStream;

	DaoStream_PrintTag( stream, block->type, spaces );
	switch( block->type ){
	case DAO_ASM_ROUTINE :
	case DAO_ASM_CLASS :
	case DAO_ASM_INTERFACE :
		DaoByteCoder_DecodeChunk2222( block->begin, & A, & B, & C, & D );
		DaoStream_PrintInt4( stream, "%i, %i, %i, %i;\n", A, B, C, D );
		DaoStream_PrintSubBlocks( stream, block, spaces + 4 );
		DaoStream_PrintTag( stream, DAO_ASM_END, spaces );
		DaoStream_WriteChars( stream, " ;\n" );
		break;
	case DAO_ASM_VALUE :
		A = block->begin[0];
		switch( A ){
		case DAO_INTEGER :
			DaoStream_WriteChars( stream, "DAO_INTEGER;\n" );
			DaoStream_PrintTag( stream, DAO_ASM_END, spaces );
			DaoStream_PrintDaoInt( stream, " %" DAO_I64 " ;\n", DaoByteCoder_DecodeDaoInt( self, block->end ) );
			break;
		case DAO_FLOAT :
			DaoStream_WriteChars( stream, "DAO_FLOAT;\n" );
			DaoStream_PrintTag( stream, DAO_ASM_END, spaces );
			DaoStream_PrintFloat( stream, " %g ;\n", DaoByteCoder_DecodeFloat( self, block->end ) );
			break;
		case DAO_STRING :
			C = block->begin[1];
			DaoStream_PrintInt( stream, "DAO_STRING, %i, ", C );
			D = block->first || C >= 6 ? 6 : C;
			DaoStream_PrintChars( stream, block->begin+2, D );
			while( pb ){
				DaoStream_PrintTag( stream, pb->type, spaces + 4 );
				DaoStream_PrintChars( stream, pb->begin, 8 );
				D += 8;
				pb = pb->next;
			}
			DaoStream_PrintTag( stream, DAO_ASM_END, spaces );
			for(i=0; i<8 && (D%16 != C); ++i) D += 1;
			DaoStream_PrintChars( stream, block->end, i );
			break;
		case DAO_ENUM :
			B = DaoByteCoder_DecodeUInt16( block->begin+2 );
			C = DaoByteCoder_DecodeUInt32( block->end );
			DaoStream_PrintInt( stream, "DAO_ENUM, %i;\n", B );
			DaoStream_PrintTag( stream, DAO_ASM_END, spaces );
			DaoStream_PrintInt( stream, " %i;\n", C );
			break;
		case DAO_ARRAY :
			i = 0;
			D = DaoByteCoder_DecodeUInt16( block->begin+2 );
			C = DaoByteCoder_DecodeUInt32( block->begin+4 );
			DaoStream_PrintInt3( stream, "DAO_ARRAY, %i, %i, %i;\n", block->begin[1], D, C );
			while( D > 0 && pb != NULL ){
				k = DaoByteCoder_DecodeUInt32( pb->begin );
				m = DaoByteCoder_DecodeUInt32( pb->begin+4 );
				DaoStream_PrintTag( stream, DAO_ASM_DATA, spaces + 4 );
				if( i++ < D ) DaoStream_PrintInt( stream, "%12i", k );
				if( i++ < D ) DaoStream_PrintInt( stream, ", %12i", m );
				DaoStream_WriteChars( stream, ";\n" );
				pb = pb->next;
				D -= 2;
			}
			if( block->begin[1] == DAO_INTEGER && sizeof(dao_integer) == 8 ){
				for(; pb!=NULL; pb=pb->next){
					dao_integer ivalue = DaoByteCoder_DecodeDaoInt( self, pb->begin );
					DaoStream_PrintTag( stream, DAO_ASM_DATA, spaces + 4 );
					DaoStream_PrintDaoInt( stream, "%12"DAO_I64";\n", ivalue );
				}
				DaoStream_PrintTag( stream, DAO_ASM_END, spaces );
				if(C){
					dao_integer ivalue = DaoByteCoder_DecodeDaoInt( self, block->end );
					DaoStream_PrintDaoInt( stream, " %12"DAO_I64"", ivalue );
				}
			}else if( block->begin[1] == DAO_INTEGER ){
				for(; pb!=NULL; pb=pb->next){
					k = DaoByteCoder_DecodeDaoInt( self, pb->begin );
					m = DaoByteCoder_DecodeDaoInt( self, pb->begin+4 );
					DaoStream_PrintTag( stream, DAO_ASM_DATA, spaces + 4 );
					DaoStream_PrintInt2( stream, "%12"DAO_INT", %12"DAO_INT";\n", k, k );
				}
				DaoStream_PrintTag( stream, DAO_ASM_END, spaces );
				if( C ){
					k = DaoByteCoder_DecodeDaoInt( self, block->end );
					DaoStream_PrintDaoInt( stream, "%12"DAO_INT"", k );
					if( !(C%2) ){
						k = DaoByteCoder_DecodeDaoInt( self, block->end+4 );
						DaoStream_PrintDaoInt( stream, ", %12"DAO_INT"", k );
					}
				}
			}else{
				double f;
				for(; pb!=NULL; pb=pb->next){
					f = DaoByteCoder_DecodeFloat( self, pb->begin );
					DaoStream_PrintTag( stream, DAO_ASM_DATA, spaces + 4 );
					DaoStream_PrintFloat( stream, "%12g;\n", f );
				}
				DaoStream_PrintTag( stream, DAO_ASM_END, spaces );
				if( C ){
					f = DaoByteCoder_DecodeFloat( self, block->end );
					DaoStream_PrintFloat( stream, "%12g", f );
				}
			}
			DaoStream_WriteChars( stream, ";\n" );
			break;
		case DAO_LIST :
		case DAO_MAP :
			B = DaoByteCoder_DecodeUInt16( block->begin+2 );
			if( A == DAO_LIST ){
				C = DaoByteCoder_DecodeUInt32( block->begin+4 );
				DaoStream_WriteChars( stream, "DAO_LIST" );
				DaoStream_PrintInt3( stream, ", %i, %i, %i;\n", block->begin[1], B, C );
			}else{
				C = DaoByteCoder_DecodeUInt16( block->begin+4 );
				D = DaoByteCoder_DecodeUInt16( block->begin+6 );
				DaoStream_WriteChars( stream, "DAO_MAP" );
				DaoStream_PrintInt4( stream, ", %i, %i, %i, %i;\n", block->begin[1], B, C, D );
			}
			while( pb ){
				DaoStream_PrintTag( stream, pb->type, spaces + 4 );
				DaoByteCoder_DecodeChunk2222( pb->begin, & A, & B, & C, & D );
				DaoStream_PrintInt4( stream, "%6i,%6i,%6i,%6i;\n", A, B, C, D );
				pb = pb->next;
			}
			DaoByteCoder_DecodeChunk2222( block->end, & A, & B, & C, & D );
			DaoStream_PrintTag( stream, DAO_ASM_END, spaces );
			DaoStream_PrintInt4( stream, "%6i,%6i,%6i,%6i;\n", A, B, C, D );
			break;
		default :
			DaoByteCoder_DecodeChunk2222( block->begin, & A, & B, & C, & D );
			DaoStream_PrintInt2( stream, "%i, %i, ", block->begin[0], block->begin[1] );
			DaoStream_PrintInt3( stream, "%i, %i, %i;\n", B, C, D );
			DaoStream_PrintSubBlocks( stream, block, spaces + 4 );
			DaoStream_PrintTag( stream, DAO_ASM_END, spaces );
			DaoStream_WriteChars( stream, " ;\n" );
			break;
		}
		break;
	case DAO_ASM_ENUM :
		DaoByteCoder_DecodeChunk224( block->begin, & A, & B, & C );
		DaoStream_PrintInt3( stream, "%i, %i, %i;\n", A, B, C );
		while( pb ){
			DaoByteCoder_DecodeSubChunk24( pb->begin, & A, & B );
			DaoStream_PrintTag( stream, pb->type, spaces + 4 );
			DaoStream_PrintInt2( stream, "%i, %i;\n", A, B );
			pb = pb->next;
		}
		DaoByteCoder_DecodeSubChunk24( block->end, & A, & B );
		DaoStream_PrintTag( stream, DAO_ASM_END, spaces );
		DaoStream_PrintInt2( stream, "%i, %i;\n", A, B );
		break;
	case DAO_ASM_EVAL :
		DaoByteCoder_DecodeChunk2222( block->begin, & A, & B, & C, & D );
		DaoStream_WriteChars( stream, DaoVmCode_GetOpcodeName( A ) );
		DaoStream_PrintInt3( stream, ", %i, %i, %i;\n", B, C, D );
		while( pb ){
			DaoByteCoder_DecodeChunk2222( pb->begin, & A, & B, & C, & D );
			DaoStream_PrintTag( stream, pb->type, spaces + 4 );
			DaoStream_PrintInt4( stream, "%6i,%6i,%6i,%6i;\n", A, B, C, D );
			pb = pb->next;
		}
		DaoByteCoder_DecodeChunk2222( block->end, & A, & B, & C, & D );
		DaoStream_PrintTag( stream, DAO_ASM_END, spaces );
		DaoStream_PrintInt4( stream, "%6i,%6i,%6i,%6i;\n", A, B, C, D );
		break;
	case DAO_ASM_TYPE :
	case DAO_ASM_BASES :
	case DAO_ASM_DECOS :
	case DAO_ASM_CONSTS :
	case DAO_ASM_TYPES :
		DaoByteCoder_DecodeChunk2222( block->begin, & A, & B, & C, & D );
		DaoStream_PrintInt4( stream, "%6i,%6i,%6i,%6i;\n", A, B, C, D );
		while( pb ){
			DaoByteCoder_DecodeChunk2222( pb->begin, & A, & B, & C, & D );
			DaoStream_PrintTag( stream, pb->type, spaces + 4 );
			DaoStream_PrintInt4( stream, "%6i,%6i,%6i,%6i;\n", A, B, C, D );
			pb = pb->next;
		}
		k = strlen( DaoByteCoder_GetASM( block->type ) );
		m = strlen( DaoByteCoder_GetASM( DAO_ASM_END ) );
		DaoByteCoder_DecodeChunk2222( block->end, & A, & B, & C, & D );
		DaoStream_PrintTag( stream, DAO_ASM_END, spaces );
		while( (k--) > m ) DaoStream_WriteChar( stream, ' ' );
		DaoStream_PrintInt4( stream, "%6i,%6i,%6i,%6i;\n", A, B, C, D );
		break;
	case DAO_ASM_CODE :
		DaoByteCoder_DecodeChunk2222( block->begin, & A, & B, & C, & D );
		DaoStream_PrintInt4( stream, "%6i,%6i,%6i,%6i;\n", A, B, C, D );
		lines = B - 1;
		while( pb ){
			DaoByteCoder_DecodeChunk2222( pb->begin, & A, & B, & C, & D );
			DaoStream_PrintTag( stream, pb->type, spaces + 4 );
			if( lines > 0 ){
				DaoStream_PrintInt4( stream, "%6i,%6i,%6i,%6i;\n", (short)A, B, (short)C, D );
			}else{
				DaoStream_PrintString( stream, "   %-11s,", DaoVmCode_GetOpcodeName( A ) );
				DaoStream_PrintInt3( stream, "%6i,%6i,%6i;\n", B, C, D );
			}
			pb = pb->next;
			lines -= 2;
		}
		DaoByteCoder_DecodeChunk2222( block->end, & A, & B, & C, & D );
		DaoStream_PrintTag( stream, DAO_ASM_END, spaces );
		DaoStream_PrintString( stream, "        %-11s,", DaoVmCode_GetOpcodeName( A ) );
		DaoStream_PrintInt3( stream, "%6i,%6i,%6i;\n", B, C, D );
		break;
	case DAO_ASM_NONE :
	case DAO_ASM_END :
		DaoStream_WriteChars( stream, ";\n" );
		break;
	default:
		DaoByteCoder_DecodeChunk2222( block->begin, & A, & B, & C, & D );
		DaoStream_PrintInt4( stream, "%5i, %5i, %5i, %5i;\n", A, B, C, D );
		if( block->type >= DAO_ASM_ROUTINE && block->type < DAO_ASM_END ){
			DaoStream_PrintTag( stream, DAO_ASM_END, spaces );
			DaoStream_WriteChars( stream, ";\n" );
		}
		break;
	}
}
void DaoByteCoder_Disassemble( DaoByteCoder *self )
{
	if( self->top == NULL ) return;
	DaoByteCoder_SplitData( self, self->top );
	DaoByteCoder_PrintBlock( self, self->top, 0, 0 );
}

