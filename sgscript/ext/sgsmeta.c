

#include <sgs_int.h>


int sgs_meta_globals( SGS_CTX )
{
#define _META_RGS( instr ) sgs_SetGlobalByName( C, #instr, sgs_MakeInt( SGS_##instr ) );
	
	_META_RGS( SI_NOP );
	_META_RGS( SI_PUSH );
	
	_META_RGS( SI_RETN );
	_META_RGS( SI_JUMP );
	_META_RGS( SI_JMPT );
	_META_RGS( SI_JMPF );
	_META_RGS( SI_JMPN );
	_META_RGS( SI_CALL );
	
	_META_RGS( SI_FORPREP );
	_META_RGS( SI_FORLOAD );
	_META_RGS( SI_FORJUMP );
	
	_META_RGS( SI_LOADCONST );
	_META_RGS( SI_GETVAR );
	_META_RGS( SI_SETVAR );
	_META_RGS( SI_GETPROP );
	_META_RGS( SI_SETPROP );
	_META_RGS( SI_GETINDEX );
	_META_RGS( SI_SETINDEX );
	
	_META_RGS( SI_GENCLSR );
	_META_RGS( SI_PUSHCLSR );
	_META_RGS( SI_MAKECLSR );
	_META_RGS( SI_GETCLSR );
	_META_RGS( SI_SETCLSR );
	
	_META_RGS( SI_SET );
	_META_RGS( SI_MCONCAT );
	_META_RGS( SI_CONCAT );
	_META_RGS( SI_NEGATE );
	_META_RGS( SI_BOOL_INV );
	_META_RGS( SI_INVERT );
	
	_META_RGS( SI_INC );
	_META_RGS( SI_DEC );
	_META_RGS( SI_ADD );
	_META_RGS( SI_SUB );
	_META_RGS( SI_MUL );
	_META_RGS( SI_DIV );
	_META_RGS( SI_MOD );
	
	_META_RGS( SI_AND );
	_META_RGS( SI_OR );
	_META_RGS( SI_XOR );
	_META_RGS( SI_LSH );
	_META_RGS( SI_RSH );
	
	_META_RGS( SI_SEQ );
	_META_RGS( SI_SNEQ );
	_META_RGS( SI_EQ );
	_META_RGS( SI_NEQ );
	_META_RGS( SI_LT );
	_META_RGS( SI_GTE );
	_META_RGS( SI_GT );
	_META_RGS( SI_LTE );
	_META_RGS( SI_RAWCMP );
	
	_META_RGS( SI_ARRAY );
	_META_RGS( SI_DICT );
#undef _META_RGS
	return 0;
}


static int _sgs_meta_dumpfn( SGS_CTX, sgs_iFunc* func );

static int _sgs_meta_dumpconstlist( SGS_CTX, sgs_Variable* var, size_t numvars )
{
	sgs_Variable* vend = var + numvars;
	
	sgs_CreateArray( C, NULL, 0 );
	
	while( var < vend )
	{
		sgs_PushString( C, "type" );
		sgs_PushInt( C, var->type );
		
		sgs_PushString( C, "data" );
		switch( var->type )
		{
		case SGS_VT_NULL:
		case SGS_VT_BOOL:
		case SGS_VT_INT:
		case SGS_VT_REAL:
		case SGS_VT_STRING:
			sgs_PushVariable( C, *var );
			break;
		case SGS_VT_FUNC:
			if( !_sgs_meta_dumpfn( C, var->data.F ) )
				return 0;
			break;
		default:
			return 0;
		}
		
		sgs_CreateDict( C, NULL, 4 );
		sgs_ArrayPush( C, sgs_StackItem( C, -2 ), 1 );
		
		var++;
	}
	
	return 1;
}

static int _sgs_meta_dumpbclist( SGS_CTX, sgs_instr_t* data, size_t numinstr )
{
	sgs_instr_t* dend = data + numinstr;
	
	sgs_CreateArray( C, NULL, 0 );
	
	while( data < dend )
	{
		sgs_instr_t i = *data++;
		
		sgs_PushString( C, "op" );
		sgs_PushInt( C, SGS_INSTR_GET_OP( i ) );
		sgs_PushString( C, "a" );
		sgs_PushInt( C, SGS_INSTR_GET_A( i ) );
		sgs_PushString( C, "b" );
		sgs_PushInt( C, SGS_INSTR_GET_B( i ) );
		sgs_PushString( C, "c" );
		sgs_PushInt( C, SGS_INSTR_GET_C( i ) );
		sgs_PushString( C, "e" );
		sgs_PushInt( C, SGS_INSTR_GET_E( i ) );
		
		sgs_CreateDict( C, NULL, 10 );
		sgs_ArrayPush( C, sgs_StackItem( C, -2 ), 1 );
	}
	
	return 1;
}

static int _sgs_meta_dumplnlist( SGS_CTX, sgs_LineNum* data, size_t numinstr )
{
	sgs_LineNum* dend = data + numinstr;
	
	sgs_CreateArray( C, NULL, 0 );
	
	while( data < dend )
	{
		sgs_PushInt( C, *data++ );
		sgs_ArrayPush( C, sgs_StackItem( C, -2 ), 1 );
	}
	
	return 1;
}

static int _sgs_meta_dumpfn( SGS_CTX, sgs_iFunc* func )
{
	sgs_Variable strvar;
	int ssz = sgs_StackSize( C );
	
	sgs_PushString( C, "consts" );
	if( !_sgs_meta_dumpconstlist( C, sgs_func_consts( func ),
			(size_t) func->instr_off / sizeof(sgs_Variable) ) )
		return 0;
	
	sgs_PushString( C, "code" );
	if( !_sgs_meta_dumpbclist( C, sgs_func_bytecode( func ),
			(size_t) ( func->size - func->instr_off ) / sizeof(sgs_instr_t) ) )
		return 0;
	
	sgs_PushString( C, "lines" );
	if( !_sgs_meta_dumplnlist( C, func->lineinfo,
			(size_t) ( func->size - func->instr_off ) / sizeof(sgs_instr_t) ) )
		return 0;
	
	sgs_PushString( C, "gotthis" );
	sgs_PushBool( C, func->gotthis );
	
	sgs_PushString( C, "numargs" );
	sgs_PushInt( C, func->numargs );
	
	sgs_PushString( C, "numtmp" );
	sgs_PushInt( C, func->numtmp );
	
	sgs_PushString( C, "numclsr" );
	sgs_PushInt( C, func->numclsr );
	
	strvar.type = SGS_VT_STRING;
	
	sgs_PushString( C, "name" );
	strvar.data.S = func->sfuncname;
	sgs_PushVariable( C, strvar );
	
	sgs_PushString( C, "filename" );
	strvar.data.S = func->sfilename;
	sgs_PushVariable( C, strvar );
	
	sgs_PushString( C, "line" );
	sgs_PushInt( C, func->linenum );
	
	sgs_CreateDict( C, NULL, sgs_StackSize( C ) - ssz );
	
	return 1;
}

static int _sgs_meta_dumpcomp( SGS_CTX, sgs_CompFunc* func )
{
	int ssz = sgs_StackSize( C );
	
	sgs_PushString( C, "consts" );
	if( !_sgs_meta_dumpconstlist( C, (sgs_Variable*) (void*) SGS_ASSUME_ALIGNED( func->consts.ptr, 4 ),
			func->consts.size / sizeof(sgs_Variable) ) )
		return 0;
	
	sgs_PushString( C, "code" );
	if( !_sgs_meta_dumpbclist( C, (sgs_instr_t*) (void*) SGS_ASSUME_ALIGNED( func->code.ptr, 4 ),
			func->code.size / sizeof(sgs_instr_t) ) )
		return 0;
	
	sgs_PushString( C, "lines" );
	if( !_sgs_meta_dumplnlist( C, (sgs_LineNum*) (void*) SGS_ASSUME_ALIGNED( func->lnbuf.ptr, 4 ),
			func->lnbuf.size / sizeof(sgs_LineNum) ) )
		return 0;
	
	sgs_PushString( C, "gotthis" );
	sgs_PushBool( C, func->gotthis );
	
	sgs_PushString( C, "numargs" );
	sgs_PushInt( C, func->numargs );
	
	sgs_PushString( C, "numtmp" );
	sgs_PushInt( C, func->numtmp );
	
	sgs_PushString( C, "numclsr" );
	sgs_PushInt( C, func->numclsr );
	
	sgs_PushString( C, "name" );
	sgs_PushString( C, "<main>" );
	
	sgs_PushString( C, "line" );
	sgs_PushInt( C, 0 );
	
	sgs_CreateDict( C, NULL, sgs_StackSize( C ) - ssz );
	
	return 1;
}

int sgs_meta_unpack( SGS_CTX )
{
	int ret;
	char* buf;
	const char* bfret;
	sgs_SizeVal size;
	sgs_CompFunc* func;
	
	SGSFN( "meta_unpack" );
	
	if( !sgs_LoadArgs( C, "m", &buf, &size ) )
		return 0;
	
	ret = sgsBC_ValidateHeader( buf, (size_t) size );
	if( ret < SGS_HEADER_SIZE )
		return sgs_Msg( C, SGS_WARNING, "compiled code header error "
			"detected at position %d", ret );
	
	bfret = sgsBC_Buf2Func( C, "", buf, (size_t) size, &func );
	if( bfret )
		return sgs_Msg( C, SGS_WARNING, bfret );
	
	ret = _sgs_meta_dumpcomp( C, func );
	sgsBC_Free( C, func );
	
	if( !ret )
		return sgs_Msg( C, SGS_WARNING, "internal error while converting data" );
	
	return 1;
}


int sgs_meta_opname( SGS_CTX )
{
	const char* str;
	sgs_Int op;
	
	if( !sgs_LoadArgs( C, "i", &op ) )
		return 0;
	
	str = sgs_CodeString( SGS_CODE_OP, (int) op );
	if( str )
	{
		sgs_PushString( C, str );
		return 1;
	}
	return 0;
}


static sgs_RegFuncConst meta_funcs[] =
{
	{ "meta_globals", sgs_meta_globals },
	{ "meta_unpack", sgs_meta_unpack },
	{ "meta_opname", sgs_meta_opname },
};


#ifdef SGS_COMPILE_MODULE
#  define meta_module_entry_point sgscript_main
#endif


#ifdef __cplusplus
extern "C"
#endif
#ifdef WIN32
__declspec(dllexport)
#endif
int meta_module_entry_point( SGS_CTX )
{
	SGS_MODULE_CHECK_VERSION( C );
	sgs_RegFuncConsts( C, meta_funcs, SGS_ARRAY_SIZE( meta_funcs ) );
	return SGS_SUCCESS;
}

