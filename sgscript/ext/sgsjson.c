

#include <math.h>

#ifndef HEADER_SGSCRIPT_H
# define HEADER_SGSCRIPT_H <sgscript.h>
#endif
#include HEADER_SGSCRIPT_H
#ifndef HEADER_SGS_UTIL_H
# define HEADER_SGS_UTIL_H <sgs_util.h>
#endif
#include HEADER_SGS_UTIL_H


/* disables trailing commas and other possibly useful things */
#define STRICT_JSON


static void skipws( const char** p, const char* end )
{
	const char* pos = *p;
	while( pos < end )
	{
		if( *pos != ' ' && *pos != '\t' &&
			*pos != '\n' && *pos != '\r' )
			break;
		pos++;
	}
	*p = pos;
}


#define STK_TOP stack->ptr[ stack->size - 1 ]
#define STK_POP sgs_membuf_resize( stack, C, stack->size - 1 )
#define STK_PUSH( what ) sgs_membuf_appchr( stack, C, what )

static const char* json_parse( SGS_CTX, sgs_MemBuf* stack, const char* buf, sgs_SizeVal size, int proto )
{
	int stk = sgs_StackSize( C );
	const char* pos = buf, *end = buf + size;
	for(;;)
	{
		int push = 0;
		skipws( &pos, end );
		if( pos >= end )
			break;

		if( STK_TOP == '{' && *pos != '"' && *pos != '}' )
			return pos;

		if( STK_TOP == 0 && sgs_StackSize( C ) > stk )
			return pos;

		if( *pos == '{' )
		{
			STK_PUSH( '{' );
			if( proto )
			{
				sgs_CloneItem( C, sgs_StackItem( C, 1 ) );
			}
			else
			{
				sgs_CreateDict( C, NULL, 0 );
			}
		}
		else if( *pos == '}' )
		{
			if( STK_TOP != '{' && STK_TOP != ':' )
				return pos;
			STK_POP;
			push = 1;
		}
		else if( *pos == '[' )
		{
			STK_PUSH( '[' );
			sgs_CreateArray( C, NULL, 0 );
		}
		else if( *pos == ']' )
		{
			if( STK_TOP != '[' )
				return pos;
			STK_POP;
			push = 1;
		}
		else if( *pos == '"' )
		{
			const char* beg = ++pos;
			sgs_MemBuf str = sgs_membuf_create();
			while( pos < end && *pos != '"' )
			{
				uint8_t cc = (uint8_t) *pos;
#ifdef STRICT_JSON
				if( cc <= 0x1f )
				{
					sgs_membuf_destroy( &str, C );
					return pos;
				}
#endif
				if( *pos == '\\' )
				{
					pos++;
					switch( *pos )
					{
					case '"':
					case '\\':
					case '/':
						sgs_membuf_appchr( &str, C, *pos );
						break;
					case 'b': sgs_membuf_appchr( &str, C, '\b' ); break;
					case 'f': sgs_membuf_appchr( &str, C, '\f' ); break;
					case 'n': sgs_membuf_appchr( &str, C, '\n' ); break;
					case 'r': sgs_membuf_appchr( &str, C, '\r' ); break;
					case 't': sgs_membuf_appchr( &str, C, '\t' ); break;
					case 'u':
						{
							uint8_t hex[ 4 ];
							uint8_t tmpbuf[ 2 ];
							uint16_t uchar;
							pos++;
							if( sgs_hexchar( *pos ) ) pos++; else return pos;
							if( sgs_hexchar( *pos ) ) pos++; else return pos;
							if( sgs_hexchar( *pos ) ) pos++; else return pos;
							if( sgs_hexchar( *pos ) ) pos++; else return pos;
							pos--;
							hex[ 0 ] = (uint8_t) ( sgs_gethex( *(pos-3) ) );
							hex[ 1 ] = (uint8_t) ( sgs_gethex( *(pos-2) ) );
							hex[ 2 ] = (uint8_t) ( sgs_gethex( *(pos-1) ) );
							hex[ 3 ] = (uint8_t) ( sgs_gethex( *pos ) );
							if( hex[0] == 0xff || hex[1] == 0xff ||
								hex[2] == 0xff || hex[3] == 0xff )
								return pos;
							tmpbuf[ 0 ] = (uint8_t) ( ( hex[0] << 4 ) | hex[1] );
							tmpbuf[ 1 ] = (uint8_t) ( ( hex[2] << 4 ) | hex[3] );
							uchar = (uint16_t) ( ( tmpbuf[0]<<8 ) | tmpbuf[1] );
							if( uchar <= 0x7f )
							{
								sgs_membuf_appchr( &str, C, (char) tmpbuf[1] );
								break;
							}
							if( uchar <= 0x7ff )
							{
								char obuf[ 2 ];
								{
									obuf[0] = (char) ( 0xC0 | ((tmpbuf[1] & 0xC0) >> 6) | ((tmpbuf[0] & 0x7) << 2) );
									obuf[1] = (char) ( 0x80 | (tmpbuf[1] & 0x3F) );
								}
								sgs_membuf_appbuf( &str, C, obuf, 2 );
								break;
							}

							{
								char obuf[ 3 ];
								{
									obuf[0] = (char) ( 0xE0 | ((tmpbuf[0] & 0xF0) >> 4) );
									obuf[1] = (char) ( 0x80 | ((tmpbuf[0] & 0xF) << 2) | ((tmpbuf[1] & 0xC0) >> 6) );
									obuf[2] = (char) ( 0x80 | (tmpbuf[1] & 0x3F) );
								}
								sgs_membuf_appbuf( &str, C, obuf, 3 );
							}
						}
						break;
					default:
#ifdef STRICT_JSON
						return pos;
#else
						sgs_membuf_appbuf( &str, C, pos - 1, 2 ); break;
#endif
					}
				}
				else
					sgs_membuf_appchr( &str, C, *pos );
				pos++;
			}
			if( pos >= end )
			{
				sgs_membuf_destroy( &str, C );
				return beg;
			}
			if( str.size > 0x7fffffff )
			{
				sgs_membuf_destroy( &str, C );
				return beg;
			}
			sgs_PushStringBuf( C, str.ptr, (sgs_SizeVal) str.size );
			sgs_membuf_destroy( &str, C );
			if( STK_TOP == '{' )
			{
				STK_TOP = ':';
				pos++;
				skipws( &pos, end );
				if( *pos != ':' )
					return pos;
			}
			else
			{
				push = 1;
			}
		}
		else if( sgs_decchar( *pos ) || *pos == '-' )
		{
			sgs_Real val = 0;
			int neg = *pos == '-', num;
			if( neg )
			{
				pos++;
				if( pos >= end ) goto endnumparse;
				if( !sgs_decchar( *pos ) )
					return pos;
			}
			num = sgs_getdec( *pos++ );
			val = num;
			if( pos >= end ) goto endnumparse;
			if( num )
			{
				while( pos < end && sgs_decchar( *pos ) )
				{
					val *= 10;
					val += sgs_getdec( *pos++ );
				}
			}
			if( *pos == '.' )
			{
				sgs_Real dp = 1;
				pos++;
				while( pos < end && sgs_decchar( *pos ) )
				{
					dp /= 10;
					val += dp * sgs_getdec( *pos++ );
				}
			}
			if( *pos == 'E' || *pos == 'e' )
			{
				sgs_Real expnt = 0, sign = 1;
				pos++;
				if( pos >= end ) goto endnumparse;
				if( *pos == '-' )
				{
					sign = -1;
					pos++;
				}
				else if( *pos == '+' )
					pos++;
				else if( !sgs_decchar( *pos ) )
					return pos;
				if( pos >= end ) goto endnumparse;
				while( pos < end && sgs_decchar( *pos ) )
				{
					expnt *= 10;
					expnt += sgs_getdec( *pos++ );
				}
				val *= pow( 10, expnt * sign );
			}
endnumparse:
			if( neg )
				val = -val;
			if( val == (sgs_Int) val )
				sgs_PushInt( C, (sgs_Int) val );
			else
				sgs_PushReal( C, val );
			push = 1;
			pos--;
		}
		/* special constants */
		else if( *pos == 'n' )
		{
			pos++;
			if( *pos == 'u' ) pos++; else return pos;
			if( *pos == 'l' ) pos++; else return pos;
			if( *pos == 'l' ) pos++; else return pos;
			sgs_PushNull( C );
			pos--;
			push = 1;
		}
		else if( *pos == 't' )
		{
			pos++;
			if( *pos == 'r' ) pos++; else return pos;
			if( *pos == 'u' ) pos++; else return pos;
			if( *pos == 'e' ) pos++; else return pos;
			sgs_PushBool( C, SGS_TRUE );
			pos--;
			push = 1;
		}
		else if( *pos == 'f' )
		{
			pos++;
			if( *pos == 'a' ) pos++; else return pos;
			if( *pos == 'l' ) pos++; else return pos;
			if( *pos == 's' ) pos++; else return pos;
			if( *pos == 'e' ) pos++; else return pos;
			sgs_PushBool( C, SGS_FALSE );
			pos--;
			push = 1;
		}
		else
			return pos;

		if( push )
		{
			if( STK_TOP == '[' || STK_TOP == ':' )
			{
				int revchr = STK_TOP == '[' ? ']' : '}';
				pos++;
				skipws( &pos, end );
				if( pos >= end )
					break;
				if( *pos != ',' && *pos != revchr ) return pos;
#ifdef STRICT_JSON
				if( *pos == ',' )
				{
					pos++;
					skipws( &pos, end );
					if( pos >= end )
						break;
					if( *pos == revchr )
						return pos;
					if( *pos == ',' )
						pos--;
				}
#endif
				if( *pos != ',' )
					pos--;
			}
			if( STK_TOP == '[' )
			{
				sgs_ArrayPush( C, sgs_StackItem( C, -2 ), 1 );
			}
			if( STK_TOP == ':' )
			{
				sgs_SetIndex( C, sgs_StackItem( C, -3 ), sgs_StackItem( C, -2 ), sgs_StackItem( C, -1 ), SGS_FALSE );
				sgs_Pop( C, 2 );
				STK_TOP = '{';
			}
		}
		pos++;
	}
/*	printf( "%d, %.*s, %d\n", stack->size, stack->size, stack->ptr, sgs_StackSize(C)-stk ); */
	return sgs_StackSize( C ) > stk && stack->size == 1 ? NULL : buf;
}

static int json_decode( SGS_CTX )
{
	char* str;
	sgs_SizeVal size;
	int argc = sgs_StackSize( C );
	
	SGSFN( "json_decode" );
	if( !sgs_LoadArgs( C, "m|?o", &str, &size, NULL ) )
		return 0;
	
	{
		const char* ret = NULL;
		sgs_MemBuf stack = sgs_membuf_create();
		sgs_membuf_appchr( &stack, C, 0 );
		ret = json_parse( C, &stack, str, size, argc >= 2 );
		sgs_membuf_destroy( &stack, C );
		if( ret )
		{
		/*	printf( "pos %d, %.8s...\n", ret - str, ret ); */
			return 0;
		}
		return 1;
	}
}


static int encode_var( SGS_CTX, sgs_MemBuf* buf )
{
	sgs_Variable var = sgs_StackItem( C, -1 );
	switch( var.type )
	{
	case SGS_VT_NULL:
		sgs_membuf_appbuf( buf, C, "null", 4 );
		return 1;
	case SGS_VT_BOOL:
		sgs_membuf_appbuf( buf, C, var.data.B ? "true" : "false", 5 - !!var.data.B );
		return 1;
	case SGS_VT_INT:
		{
			char tmp[ 64 ];
			sprintf( tmp, "%" PRId64, var.data.I );
			sgs_membuf_appbuf( buf, C, tmp, strlen( tmp ) );
			return 1;
		}
	case SGS_VT_REAL:
		{
			char tmp[ 64 ];
			sprintf( tmp, "%g", var.data.R );
			sgs_membuf_appbuf( buf, C, tmp, strlen( tmp ) );
			return 1;
		}
	case SGS_VT_STRING:
		{
			sgs_membuf_appchr( buf, C, '"' );
			{
				char* str = sgs_GetStringPtr( C, -1 );
				char* frm = str, *end = str + sgs_GetStringSize( C, -1 );
				while( str < end )
				{
					if( *str == '"' || *str == '\\' )
					{
						char pp[2];
						{
							pp[0] = '\\';
							pp[1] = *str;
						}
						if( str != frm )
							sgs_membuf_appbuf( buf, C, frm, (size_t) ( str - frm ) );
						sgs_membuf_appbuf( buf, C, pp, 2 );
						frm = str + 1;
					}
					str++;
				}
				if( str != frm )
					sgs_membuf_appbuf( buf, C, frm, (size_t) ( str - frm ) );
			}
			sgs_membuf_appchr( buf, C, '"' );
			return 1;
		}
	case SGS_VT_FUNC:
	case SGS_VT_CFUNC:
		sgs_Msg( C, SGS_WARNING, "json_encode: cannot encode functions" );
		return 0;
	case SGS_VT_OBJECT:
		{
			/* stack: Obj */
			int isarr = sgs_ArraySize( sgs_StackItem( C, -1 ) ) >= 0, first = 1;
			sgs_membuf_appchr( buf, C, isarr ? '[' : '{' );
			if( sgs_PushIterator( C, sgs_StackItem( C, -1 ) ) == SGS_FALSE )
				return 0;
			/* stack: Obj, Iter */
			while( sgs_IterAdvance( C, sgs_StackItem( C, -1 ) ) > 0 )
			{
				/* stack: Obj, Iter */
				sgs_IterPushData( C, sgs_StackItem( C, -1 ), 0, 1 );
				/* stack: Obj, Iter[, Key], Value */

				if( first ) first = 0;
				else sgs_membuf_appchr( buf, C, ',' );

				if( !isarr )
				{
					sgs_IterPushData( C, sgs_StackItem( C, -2 ), 1, 0 );
					/* stack: Obj, Iter, Value, Key */
					sgs_ToString( C, -1 );
					if( !encode_var( C, buf ) )
						return 0;
					sgs_membuf_appchr( buf, C, ':' );
					
					sgs_Pop( C, 1 );
				}
				/* stack: Obj, Iter, Value */
				if( !encode_var( C, buf ) )
					return 0;
				/* stack: -- (?) */
				sgs_Pop( C, 1 );
				/* stack: Obj, Iter */
			}
			sgs_Pop( C, 1 );
			/* stack: Obj */
			sgs_membuf_appchr( buf, C, isarr ? ']' : '}' );
			return 1;
		}
	}
	return 0;
}

static int json_encode( SGS_CTX )
{
	sgs_MemBuf buf = sgs_membuf_create();
	int argc = sgs_StackSize( C ), ret;
	
	SGSFN( "json_encode" );

	if( argc != 1 )
	{
		sgs_Msg( C, SGS_WARNING, "function expects 1 argument" );
		return 0;
	}

	ret = encode_var( C, &buf );
	if( buf.size > 0x7fffffff )
	{
		sgs_membuf_destroy( &buf, C );
		return sgs_Msg( C, SGS_WARNING, "generated more string data than allowed to store" );
	}
	if( ret )
		sgs_PushStringBuf( C, buf.ptr, (sgs_SizeVal) buf.size );
	sgs_membuf_destroy( &buf, C );
	return ret;
}


#ifdef SGS_COMPILE_MODULE
#  define json_module_entry_point sgscript_main
#endif


#ifdef __cplusplus
extern "C"
#endif
SGS_APIFUNC int json_module_entry_point( SGS_CTX )
{
	SGS_MODULE_CHECK_VERSION( C );
	sgs_RegFuncConst rfc[] =
	{
		{ "json_decode", json_decode },
		{ "json_encode", json_encode },
	};
	sgs_RegFuncConsts( C, rfc, SGS_ARRAY_SIZE( rfc ) );
	return SGS_SUCCESS;
}

