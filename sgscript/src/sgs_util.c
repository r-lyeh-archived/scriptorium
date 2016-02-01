

#include <time.h>
#include <math.h>
#if !defined( _MSC_VER ) && !defined( __GNUC__ )
#  include <assert.h>
#endif
#include <limits.h>

#include "sgs_int.h"


void sgs_BreakIfFunc( const char* code, const char* file, int line )
{
	fprintf( stderr, "\n== Error detected: \"%s\", file: %s, line %d ==\n", code, file, line );
#if defined( _MSC_VER )
	__debugbreak();
#elif defined( __GNUC__ )
#  if SGS_ARCH_X86
	__asm__( "int $3" );
#  else
	__builtin_trap();
#  endif
#else
	assert( 0 );
#endif
}


int sgs_isoneof( int chr, const char* str )
{
	while( *str )
	{
		if( *str == chr )
			return 1;
		str++;
	}
	return 0;
}

int sgs_hexchar( int c )
{
	return ( (c) >= '0' && (c) <= '9' ) ||
	( (c) >= 'a' && (c) <= 'f' ) || ( (c) >= 'A' && (c) <= 'F' );
}

int sgs_gethex( int c )
{
	return ( (c) >= '0' && (c) <= '9' ) ? ( (c) - '0' ) :
	( ( (c) >= 'a' && (c) <= 'f' ) ? ( (c) - 'a' + 10 ) : ( (c) - 'A' + 10 ) );
}

int sgs_tolower( int c )
{
	return c >= 'A' && c <= 'Z' ? c - 'A' + 'a' : c;
}


void sgs_print_safe( FILE* fp, const char* buf, size_t size )
{
	size_t i;
	for( i = 0; i < size; ++i )
	{
		if( sgs_isgraph( buf[ i ] ) || buf[ i ] == ' ' )
			fputc( buf[ i ], fp );
		else
			fprintf( fp, "\\x%02X", (int) (unsigned char) buf[ i ] );
	}
}


sgs_MemBuf sgs_membuf_create( void )
{
	sgs_MemBuf sb = { NULL, 0, 0 };
	return sb;
}

void sgs_membuf_destroy( sgs_MemBuf* sb, SGS_CTX )
{
	if( sb->ptr )
		sgs_Dealloc( sb->ptr );
	sb->ptr = NULL;
}

sgs_MemBuf sgs_membuf_partial( char* ch, size_t size )
{
	sgs_MemBuf sb;
	sb.ptr = ch;
	sb.size = size;
	sb.mem = size;
	return sb;
}

void sgs_membuf_reserve( sgs_MemBuf* mb, SGS_CTX, size_t size )
{
	if( size <= mb->mem )
		return;

	mb->mem = size;
	mb->ptr = (char*) sgs_Realloc( C, mb->ptr, size );
}

void sgs_membuf_resize( sgs_MemBuf* mb, SGS_CTX, size_t size )
{
	sgs_membuf_reserve( mb, C, size );
	mb->size = size;
}

void sgs_membuf_resize_opt( sgs_MemBuf* mb, SGS_CTX, size_t size )
{
	if( size > mb->mem )
		sgs_membuf_reserve( mb, C, mb->mem * 2 < size ? size : mb->mem * 2 );
	if( size > mb->size )
		mb->size = size;
}

void sgs_membuf_insbuf( sgs_MemBuf* mb, SGS_CTX, size_t pos, const void* buf, size_t size )
{
	sgs_membuf_reserve( mb, C, mb->mem < mb->size + size ? SGS_MAX( mb->mem * 2, mb->size + size ) : 0 );
	memmove( mb->ptr + pos + size, mb->ptr + pos, mb->size - pos );
	memcpy( mb->ptr + pos, buf, size );
	mb->size += size;
}

void sgs_membuf_erase( sgs_MemBuf* mb, size_t from, size_t to )
{
	sgs_BreakIf( from > mb->size );
	sgs_BreakIf( to > mb->size );
	sgs_BreakIf( from > to );
	if( mb->size - to > 0 )
		memmove( mb->ptr + from, mb->ptr + to, mb->size - to );
	mb->size -= to - from;
}

void sgs_membuf_appbuf( sgs_MemBuf* mb, SGS_CTX, const void* buf, size_t size )
{
	sgs_membuf_reserve( mb, C, mb->mem < mb->size + size ? SGS_MAX( mb->mem * 2, mb->size + size ) : 0 );
	memcpy( mb->ptr + mb->size, buf, size );
	mb->size += size;
}


sgs_Hash sgs_HashFunc( const char* str, size_t size )
{
	size_t i, adv = size / 127 + 1;
	sgs_Hash h = 2166136261u;
	for( i = 0; i < size; i += adv )
	{
		h ^= (sgs_Hash) (uint8_t) str[ i ];
		h *= 16777619u;
	}
	return h;
}

sgs_Hash sgs_HashVar( const sgs_Variable* v )
{
	size_t size;
	switch( v->type )
	{
	/* special */
	case SGS_VT_NULL: return 0;
	case SGS_VT_BOOL: return ( v->data.B != 0 );
	case SGS_VT_STRING: return sgs_HashFunc( sgs_var_cstr( v ), v->data.S->size );
	/* data */
	case SGS_VT_INT: size = sizeof( sgs_Int ); break;
	case SGS_VT_REAL: size = sizeof( sgs_Real ); break;
	case SGS_VT_FUNC:
	case SGS_VT_CFUNC:
	case SGS_VT_OBJECT:
	case SGS_VT_PTR:
	case SGS_VT_THREAD:
		size = sizeof( void* ); break;
	default:
		return 0;
	}
	return sgs_HashFunc( (const char*) &v->data, size );
}



static int equal_variables( sgs_Variable* v1, sgs_Variable* v2 )
{
	if( v1->type != v2->type )
		return 0;
	switch( v1->type )
	{
	case SGS_VT_BOOL: return v1->data.B == v2->data.B;
	case SGS_VT_INT: return v1->data.I == v2->data.I;
	case SGS_VT_REAL: return v1->data.R == v2->data.R;
	case SGS_VT_STRING:
#if SGS_STRINGTABLE_MAXLEN >= 0x7fffffff
		return v1->data.S == v2->data.S;
#else
		if( v1->data.S == v2->data.S ) return 1;
		return v1->data.S->size == v2->data.S->size &&
			memcmp( sgs_var_cstr( v1 ), sgs_var_cstr( v2 ), v1->data.S->size ) == 0;
#endif
	case SGS_VT_FUNC: return v1->data.F == v2->data.F;
	case SGS_VT_CFUNC: return v1->data.C == v2->data.C;
	case SGS_VT_OBJECT: return v1->data.O == v2->data.O;
	case SGS_VT_PTR: return v1->data.P == v2->data.P;
	case SGS_VT_THREAD: return v1->data.T == v2->data.T;
	}
	return 1;
}


void sgs_vht_init( sgs_VHTable* T, SGS_CTX, sgs_VHTIdx initial_pair_mem, sgs_VHTIdx initial_var_mem )
{
	sgs_BreakIf( initial_pair_mem < 1 );
	sgs_BreakIf( initial_var_mem < 1 );
	
	T->pairs = sgs_Alloc_n( sgs_VHTIdx, (size_t) initial_pair_mem );
	T->pair_mem = initial_pair_mem;
	T->vars = sgs_Alloc_n( sgs_VHTVar, (size_t) initial_var_mem );
	T->var_mem = initial_var_mem;
	T->size = 0;
	T->num_rem = 0;
	
	memset( T->pairs, SGS_VHTIDX_EMPTY, sizeof(sgs_VHTIdx) * (size_t) initial_pair_mem );
}

void sgs_vht_free( sgs_VHTable* T, SGS_CTX )
{
	sgs_VHTVar* p = T->vars;
	sgs_VHTVar* pend = p + T->size;
	while( p < pend )
	{
		sgs_Release( C, &p->key );
		sgs_Release( C, &p->val );
		p++;
	}
	
	sgs_Dealloc( T->pairs );
	sgs_Dealloc( T->vars );
}

static void sgs_vht_rehash( sgs_VHTable* T, SGS_CTX, sgs_VHTIdx size )
{
	sgs_Hash h;
	sgs_VHTIdx i, si, sp, idx, *np;
	sgs_BreakIf( size < T->size );
	
	if( size == T->pair_mem )
		return;
	if( size < 4 )
		size = 4;
	
	np = sgs_Alloc_n( sgs_VHTIdx, (size_t) size );
	memset( np, SGS_VHTIDX_EMPTY, sizeof(sgs_VHTIdx) * (size_t) size );
	
#if 0
	printf( "rehash %d -> %d (size = %d, mem = %d kB)\n", T->pair_mem, size, T->size,
		(size * sizeof(sgs_VHTIdx) + T->var_mem * sizeof(sgs_VHTVar)) / 1024 );
#endif
	
	for( si = 0; si < T->pair_mem; ++si )
	{
		idx = T->pairs[ si ];
		if( idx >= 0 )
		{
			h = T->vars[ idx ].hash;
			sp = i = (sgs_VHTIdx)( h % (sgs_Hash) size );
			do
			{
				sgs_VHTIdx nidx = np[ i ];
				if( nidx == SGS_VHTIDX_EMPTY )
				{
					np[ i ] = idx;
					break;
				}
				i++;
				if( i >= size )
					i = 0;
			}
			while( i != sp );
		}
	}
	
	sgs_Dealloc( T->pairs );
	T->pairs = np;
	T->pair_mem = size;
	T->num_rem = 0;
}

static void sgs_vht_reserve( sgs_VHTable* T, SGS_CTX, sgs_VHTIdx size )
{
	sgs_VHTVar* p;
	
	sgs_BreakIf( size < T->size );
	
	if( size == T->var_mem )
		return;
	if( size < 4 )
		size = 4;
	
#if 0
	printf( "reserve %d -> %d (size = %d, mem = %d kB)\n", T->var_mem, size, T->size,
		(T->pair_mem * sizeof(sgs_VHTIdx) + size * sizeof(sgs_VHTVar)) / 1024 );
#endif
	
	/* WP: hash table limit */
	p = sgs_Alloc_n( sgs_VHTVar, (size_t) size );
	memcpy( p, T->vars, sizeof(sgs_VHTVar) * (size_t) T->size );
	sgs_Dealloc( T->vars );
	T->vars = p;
	T->var_mem = size;
}

sgs_VHTIdx sgs_vht_pair_id( sgs_VHTable* T, sgs_Variable* K, sgs_Hash hash )
{
	sgs_VHTIdx i, sp = (sgs_VHTIdx)( hash % (sgs_Hash) T->pair_mem );
	i = sp;
	do
	{
		sgs_VHTIdx idx = T->pairs[ i ];
		if( idx == SGS_VHTIDX_EMPTY )
			break;
		if( idx != SGS_VHTIDX_REMOVED && equal_variables( K, &T->vars[ idx ].key ) )
			return i;
		i++;
		if( i >= T->pair_mem )
			i = 0;
	}
	while( i != sp );
	return -1;
}

sgs_VHTVar* sgs_vht_get( sgs_VHTable* T, sgs_Variable* K )
{
	sgs_VHTIdx i = sgs_vht_pair_id( T, K, sgs_HashVar( K ) );
	if( i >= 0 )
		return T->vars + T->pairs[ i ];
	else
		return NULL;
}

sgs_VHTVar* sgs_vht_get_str( sgs_VHTable* T, const char* str, uint32_t size, sgs_Hash hash )
{
	sgs_VHTIdx i, sp = (sgs_VHTIdx)( hash % (sgs_Hash) T->pair_mem );
	i = sp;
	do
	{
		sgs_VHTIdx idx = T->pairs[ i ];
		if( idx == SGS_VHTIDX_EMPTY )
			return NULL;
		else if( idx != SGS_VHTIDX_REMOVED )
		{
			sgs_Variable* var = &T->vars[ idx ].key;
			if( var->type == SGS_VT_STRING )
			{
				sgs_iStr* S = var->data.S;
				if( S->size == size && memcmp( sgs_str_cstr( S ), str, size ) == 0 )
					return T->vars + idx;
			}
		}
		i++;
		if( i >= T->pair_mem )
			i = 0;
	}
	while( i != sp );
	return NULL;
}

sgs_VHTVar* sgs_vht_set( sgs_VHTable* T, SGS_CTX, sgs_Variable* K, sgs_Variable* V )
{
	sgs_Hash h = sgs_HashVar( K );
	sgs_VHTIdx sp, i = sgs_vht_pair_id( T, K, h );
	if( i >= 0 )
	{
		sgs_VHTVar* p = T->vars + T->pairs[ i ];
		if( V )
			sgs_Acquire( C, V );
		sgs_Release( C, &p->val );
		if( V )
			p->val = *V;
		else
			p->val.type = SGS_VT_NULL;
		return p;
	}
	else
	{
		sgs_VHTIdx osize = T->size;
		SGS_UNUSED( osize );
		
		/* prefer to rehash if too many removed (num_rem) items are found */
		if( T->size + T->num_rem + 1.0 >= T->pair_mem * 0.7 )
			sgs_vht_rehash( T, C, (sgs_VHTIdx) SGS_MAX( T->pair_mem * 1.5, T->size + 16 ) );
		if( T->size >= T->var_mem )
			sgs_vht_reserve( T, C, (sgs_VHTIdx) SGS_MAX( T->size * 1.5, T->size + 16 ) );
		
		{
			sgs_VHTVar* p = T->vars + T->size;
			p->key = *K;
			p->hash = h;
			sgs_Acquire( C, K );
			if( V )
			{
				p->val = *V;
				sgs_Acquire( C, V );
			}
			else
				p->val.type = SGS_VT_NULL;
		}
		
		sp = i = (sgs_VHTIdx)( h % (sgs_Hash) T->pair_mem );
		do
		{
			sgs_VHTIdx idx = T->pairs[ i ];
			if( idx == SGS_VHTIDX_EMPTY || idx == SGS_VHTIDX_REMOVED )
			{
				if( idx == SGS_VHTIDX_REMOVED )
					T->num_rem--;
				T->pairs[ i ] = T->size;
				T->size++;
				break;
			}
			i++;
			if( i >= T->pair_mem )
				i = 0;
		}
		while( i != sp );
		
		sgs_BreakIf( T->size == osize );
		
		return T->vars + T->size - 1;
	}
}

void sgs_vht_unset( sgs_VHTable* T, SGS_CTX, sgs_Variable* K )
{
	sgs_Hash h = sgs_HashVar( K );
	sgs_VHTIdx i = sgs_vht_pair_id( T, K, h );
	if( i >= 0 )
	{
		sgs_VHTIdx idx = T->pairs[ i ];
		sgs_VHTVar* p = T->vars + idx;
		sgs_VHTVar bp = *p;
		
		T->pairs[ i ] = SGS_VHTIDX_REMOVED;
		
		T->num_rem++;
		T->size--;
		if( p < T->vars + T->size )
		{
			sgs_VHTVar* ep = T->vars + T->size;
			i = sgs_vht_pair_id( T, &ep->key, ep->hash );
			sgs_BreakIf( i == -1 );
			*p = *ep;
			T->pairs[ i ] = idx;
		}
		
		sgs_Release( C, &bp.key );
		sgs_Release( C, &bp.val );
	}
	
	if( T->num_rem > T->var_mem * 0.25 + 16 )
	{
		sgs_vht_reserve( T, C, (sgs_VHTIdx) ( T->size * 0.75 + T->var_mem * 0.25 ) );
		sgs_vht_rehash( T, C, (sgs_VHTIdx) ( T->size * 0.5 + T->var_mem * 0.5 ) );
	}
}


double sgs_GetTime()
{
#ifdef __linux
	struct timespec ts;
	clock_gettime( CLOCK_MONOTONIC, &ts );
	return (double) ts.tv_sec + 0.000000001 * (double) ts.tv_nsec;
#else
	clock_t clk = clock();
	return (double)( clk ) / (double)( CLOCKS_PER_SEC );
#endif
}


/* string -> number conversion */

typedef const char CCH;

static int strtonum_hex( CCH** at, CCH* end, sgs_Int* outi )
{
	sgs_Int val = 0;
	CCH* str = *at + 2;
	while( str < end && sgs_hexchar( *str ) )
	{
		val *= 16;
		val += sgs_gethex( *str );
		str++;
	}
	*at = str;
	*outi = val;
	return 1;
}

static int strtonum_oct( CCH** at, CCH* end, sgs_Int* outi )
{
	sgs_Int val = 0;
	CCH* str = *at + 2;
	while( str < end && sgs_octchar( *str ) )
	{
		val *= 8;
		val += sgs_getoct( *str );
		str++;
	}
	*at = str;
	*outi = val;
	return 1;
}

static int strtonum_bin( CCH** at, CCH* end, sgs_Int* outi )
{
	sgs_Int val = 0;
	CCH* str = *at + 2;
	while( str < end && sgs_binchar( *str ) )
	{
		val *= 2;
		val += sgs_getbin( *str );
		str++;
	}
	*at = str;
	*outi = val;
	return 1;
}

static int strtonum_real( CCH** at, CCH* end, sgs_Real* outf )
{
	sgs_Real val = 0;
	sgs_Real vsign = 1;
	CCH* str = *at, *teststr;
	
	if( *str == '+' ) str++;
	else if( *str == '-' ){ vsign = -1; str++; }
	
	teststr = str;
	while( str < end && sgs_decchar( *str ) )
	{
		val *= 10;
		val += sgs_getdec( *str );
		str++;
	}
	if( str == teststr )
		return 0;
	if( str >= end )
		goto done;
	if( *str == '.' )
	{
		sgs_Real mult = 1.0;
		str++;
		while( str < end && sgs_decchar( *str ) )
		{
			mult /= 10;
			val += sgs_getdec( *str ) * mult;
			str++;
		}
	}
	if( str < end && ( *str == 'e' || *str == 'E' ) )
	{
		sgs_Real sign, e = 0;
		str++;
		if( str >= end || ( *str != '+' && *str != '-' ) )
			goto done;
		sign = *str++ == '-' ? -1 : 1;
		while( str < end && sgs_decchar( *str ) )
		{
			e *= 10;
			e += sgs_getdec( *str );
			str++;
		}
		val *= pow( 10, e * sign );
	}
	
done:
	*outf = val * vsign;
	*at = str;
	return 2;
}

static int strtonum_dec( CCH** at, CCH* end, sgs_Int* outi, sgs_Real* outf )
{
	CCH* str = *at, *teststr;
	if( *str == '+' || *str == '-' ) str++;
	teststr = str;
	while( str < end && sgs_decchar( *str ) )
		str++;
	if( str == teststr )
		return 0;
	if( str < end && ( *str == '.' || *str == 'E' || *str == 'e' ) )
		return strtonum_real( at, end, outf );
	else
	{
		sgs_Int val = 0;
		int invsign = 0;
		
		str = *at;
		if( *str == '+' ) str++;
		else if( *str == '-' ){ invsign = 1; str++; }
		
		while( str < end && sgs_decchar( *str ) )
		{
			val *= 10;
			val += sgs_getdec( *str );
			str++;
		}
		if( invsign ) val = -val;
		*outi = val;
		*at = str;
		return 1;
	}
}

int sgs_util_strtonum( CCH** at, CCH* end, sgs_Int* outi, sgs_Real* outf )
{
	CCH* str = *at;
	if( str >= end )
		return 0;
	if( end - str >= 3 && *str == '0' )
	{
		if( str[1] == 'x' ) return strtonum_hex( at, end, outi );
		else if( str[1] == 'o' ) return strtonum_oct( at, end, outi );
		else if( str[1] == 'b' ) return strtonum_bin( at, end, outi );
	}
	return strtonum_dec( at, end, outi, outf );
}


sgs_Int sgs_util_atoi( const char* str, size_t len )
{
	sgs_Int vi = 0;
	sgs_Real vr = 0;
	const char* p = str;
	int ret = sgs_util_strtonum( &p, str + len, &vi, &vr );
	if( p == str ) return 0;
	if( ret == 1 ) return vi;
	else if( ret == 2 ) return (sgs_Int) vr;
	else return 0;
}

sgs_Real sgs_util_atof( const char* str, size_t len )
{
	sgs_Int vi = 0;
	sgs_Real vr = 0;
	const char* p = str;
	int ret = sgs_util_strtonum( &p, str + len, &vi, &vr );
	if( p == str ) return 0;
	if( ret == 1 ) return (sgs_Real) vi;
	else if( ret == 2 ) return vr;
	else return 0;
}



/*------------------------------------------------------------*\
 * part of CRC-32 v2.0.0 by Craig Bruce, 2006-04-29.
 * license: public domain
 * original source at http://www.csbruce.com/software/crc32.c
\*------------------------------------------------------------*/
uint32_t sgs_crc32( const void* buf, size_t len, uint32_t in_crc )
{
	static const uint32_t crcTable[256] =
	{
		0x00000000,0x77073096,0xEE0E612C,0x990951BA,0x076DC419,0x706AF48F,0xE963A535,
		0x9E6495A3,0x0EDB8832,0x79DCB8A4,0xE0D5E91E,0x97D2D988,0x09B64C2B,0x7EB17CBD,
		0xE7B82D07,0x90BF1D91,0x1DB71064,0x6AB020F2,0xF3B97148,0x84BE41DE,0x1ADAD47D,
		0x6DDDE4EB,0xF4D4B551,0x83D385C7,0x136C9856,0x646BA8C0,0xFD62F97A,0x8A65C9EC,
		0x14015C4F,0x63066CD9,0xFA0F3D63,0x8D080DF5,0x3B6E20C8,0x4C69105E,0xD56041E4,
		0xA2677172,0x3C03E4D1,0x4B04D447,0xD20D85FD,0xA50AB56B,0x35B5A8FA,0x42B2986C,
		0xDBBBC9D6,0xACBCF940,0x32D86CE3,0x45DF5C75,0xDCD60DCF,0xABD13D59,0x26D930AC,
		0x51DE003A,0xC8D75180,0xBFD06116,0x21B4F4B5,0x56B3C423,0xCFBA9599,0xB8BDA50F,
		0x2802B89E,0x5F058808,0xC60CD9B2,0xB10BE924,0x2F6F7C87,0x58684C11,0xC1611DAB,
		0xB6662D3D,0x76DC4190,0x01DB7106,0x98D220BC,0xEFD5102A,0x71B18589,0x06B6B51F,
		0x9FBFE4A5,0xE8B8D433,0x7807C9A2,0x0F00F934,0x9609A88E,0xE10E9818,0x7F6A0DBB,
		0x086D3D2D,0x91646C97,0xE6635C01,0x6B6B51F4,0x1C6C6162,0x856530D8,0xF262004E,
		0x6C0695ED,0x1B01A57B,0x8208F4C1,0xF50FC457,0x65B0D9C6,0x12B7E950,0x8BBEB8EA,
		0xFCB9887C,0x62DD1DDF,0x15DA2D49,0x8CD37CF3,0xFBD44C65,0x4DB26158,0x3AB551CE,
		0xA3BC0074,0xD4BB30E2,0x4ADFA541,0x3DD895D7,0xA4D1C46D,0xD3D6F4FB,0x4369E96A,
		0x346ED9FC,0xAD678846,0xDA60B8D0,0x44042D73,0x33031DE5,0xAA0A4C5F,0xDD0D7CC9,
		0x5005713C,0x270241AA,0xBE0B1010,0xC90C2086,0x5768B525,0x206F85B3,0xB966D409,
		0xCE61E49F,0x5EDEF90E,0x29D9C998,0xB0D09822,0xC7D7A8B4,0x59B33D17,0x2EB40D81,
		0xB7BD5C3B,0xC0BA6CAD,0xEDB88320,0x9ABFB3B6,0x03B6E20C,0x74B1D29A,0xEAD54739,
		0x9DD277AF,0x04DB2615,0x73DC1683,0xE3630B12,0x94643B84,0x0D6D6A3E,0x7A6A5AA8,
		0xE40ECF0B,0x9309FF9D,0x0A00AE27,0x7D079EB1,0xF00F9344,0x8708A3D2,0x1E01F268,
		0x6906C2FE,0xF762575D,0x806567CB,0x196C3671,0x6E6B06E7,0xFED41B76,0x89D32BE0,
		0x10DA7A5A,0x67DD4ACC,0xF9B9DF6F,0x8EBEEFF9,0x17B7BE43,0x60B08ED5,0xD6D6A3E8,
		0xA1D1937E,0x38D8C2C4,0x4FDFF252,0xD1BB67F1,0xA6BC5767,0x3FB506DD,0x48B2364B,
		0xD80D2BDA,0xAF0A1B4C,0x36034AF6,0x41047A60,0xDF60EFC3,0xA867DF55,0x316E8EEF,
		0x4669BE79,0xCB61B38C,0xBC66831A,0x256FD2A0,0x5268E236,0xCC0C7795,0xBB0B4703,
		0x220216B9,0x5505262F,0xC5BA3BBE,0xB2BD0B28,0x2BB45A92,0x5CB36A04,0xC2D7FFA7,
		0xB5D0CF31,0x2CD99E8B,0x5BDEAE1D,0x9B64C2B0,0xEC63F226,0x756AA39C,0x026D930A,
		0x9C0906A9,0xEB0E363F,0x72076785,0x05005713,0x95BF4A82,0xE2B87A14,0x7BB12BAE,
		0x0CB61B38,0x92D28E9B,0xE5D5BE0D,0x7CDCEFB7,0x0BDBDF21,0x86D3D2D4,0xF1D4E242,
		0x68DDB3F8,0x1FDA836E,0x81BE16CD,0xF6B9265B,0x6FB077E1,0x18B74777,0x88085AE6,
		0xFF0F6A70,0x66063BCA,0x11010B5C,0x8F659EFF,0xF862AE69,0x616BFFD3,0x166CCF45,
		0xA00AE278,0xD70DD2EE,0x4E048354,0x3903B3C2,0xA7672661,0xD06016F7,0x4969474D,
		0x3E6E77DB,0xAED16A4A,0xD9D65ADC,0x40DF0B66,0x37D83BF0,0xA9BCAE53,0xDEBB9EC5,
		0x47B2CF7F,0x30B5FFE9,0xBDBDF21C,0xCABAC28A,0x53B39330,0x24B4A3A6,0xBAD03605,
		0xCDD70693,0x54DE5729,0x23D967BF,0xB3667A2E,0xC4614AB8,0x5D681B02,0x2A6F2B94,
		0xB40BBE37,0xC30C8EA1,0x5A05DF1B,0x2D02EF8D
	};
	uint32_t crc32;
	const uint8_t *byteBuf;
	size_t i;
	
	crc32 = in_crc ^ 0xFFFFFFFF;
	byteBuf = (const uint8_t*) buf;
	for( i = 0; i < len; ++i )
		crc32 = ( crc32 >> 8 ) ^ crcTable[ ( crc32 ^ byteBuf[ i ] ) & 0xFF ];
	
	return crc32 ^ 0xFFFFFFFF;
}



/**** BEGIN CUSTOM QSORT CODE ****/

/*******************************************************************************
*
*  Author:  Remi Dufour - remi.dufour@gmail.com
*  ! code is modified !, for original, refer to:
*    http://www.codeproject.com/Articles/426706/A-simple-portable-yet-efficient-Quicksort-implemen
*  Date:    July 23rd, 2012
*
*  Name:        Quicksort
*
*  Description: This is a well-known sorting algorithm developed by C. A. R. 
*               Hoare. It is a comparison sort and in this implementation,
*               is not a stable sort.
*
*  Note:        This is public-domain C implementation written from
*               scratch.  Use it at your own risk.
*
*******************************************************************************/

/* Insertion sort threshold shift
 *
 * This macro defines the threshold shift (power of 2) at which the insertion
 * sort algorithm replaces the Quicksort.  A zero threshold shift disables the
 * insertion sort completely.
 *
 * The value is optimized for Linux and MacOS on the Intel x86 platform.
 */
#ifndef INSERTION_SORT_THRESHOLD_SHIFT
# if defined( __APPLE__ ) && defined( __MACH__ )
#  define INSERTION_SORT_THRESHOLD_SHIFT 0
# else
#  define INSERTION_SORT_THRESHOLD_SHIFT 2
# endif
#endif

/* Macro SWAP
 *
 * Swaps the elements of two arrays.
 *
 * The length of the swap is determined by the value of "SIZE".  While both
 * arrays can't overlap, the case in which both pointers are the same works.
 */
#define SWAP(A,B,SIZE)                               \
	{                                                \
		register char       *a_byte = A;             \
		register char       *b_byte = B;             \
		register const char *a_end  = a_byte + SIZE; \
		while (a_byte < a_end)                       \
		{                                            \
			register const char swap_byte = *b_byte; \
			*b_byte++ = *a_byte;                     \
			*a_byte++ = swap_byte;                   \
		}                                            \
	}

/* Macro SWAP_NEXT
 *
 * Swaps the elements of an array with its next value.
 *
 * The length of the swap is determined by the value of "size".  This macro
 * must be used at the beginning of a scope and "A" shouldn't be an expression.
 */
#define SWAP_NEXT(A,SIZE)                                 \
	register char       *a_byte = A;                      \
	register const char *a_end  = A + SIZE;               \
	while (a_byte < a_end)                                \
	{                                                     \
		register const char swap_byte = *(a_byte + SIZE); \
		*(a_byte + SIZE) = *a_byte;                       \
		*a_byte++ = swap_byte;                            \
	}

void sgs_quicksort( void *array, size_t length, size_t size,
	int(*compare)(const void *, const void *, void*), void* userdata)
{
	struct stackframe
	{
		void *left;
		void *right;
	} stack[CHAR_BIT * sizeof(void *)];

	/* Recursion level */
	struct stackframe *recursion = stack;

#if INSERTION_SORT_THRESHOLD_SHIFT != 0
	/* Insertion sort threshold */
	const ptrdiff_t threshold = (ptrdiff_t) size << INSERTION_SORT_THRESHOLD_SHIFT;
#endif
	
	if( length <= 1 )
		return;

	/* Assign the first recursion level of the sorting */
	recursion->left = array;
	recursion->right = (char *)array + size * (length - 1);

	do
	{
		/* Partition the array */
		register char *idx = (char*) recursion->left;
		register char *right = (char*) recursion->right;
		char          *left  = idx;

		/* Assigning store to the left */
		register char *store = idx;

		/* Pop the stack */
		--recursion;

		/* Determine a pivot (in the middle) and move it to the end */
		/* @modification@ changed the left address to something that works */
		SWAP(left + ((size_t)((right - left) >> 1) / size * size),right,size)

		/* From left to right */
		while (idx < right)
		{
			/* If item is smaller than pivot */
			if (compare(right, idx, userdata) > 0)
			{
				/* Swap item and store */
				SWAP(idx,store,size)

				/* We increment store */
				store += size;
			}

			idx += size;
		}

	    /* Move the pivot to its final place */
		SWAP(right,store,size)

/* Performs a recursion to the left */
#define RECURSE_LEFT                     \
	if (left < store - size)             \
	{                                    \
		(++recursion)->left = left;      \
		recursion->right = store - size; \
	}

/* Performs a recursion to the right */
#define RECURSE_RIGHT                       \
	if (store + size < right)               \
	{                                       \
		(++recursion)->left = store + size; \
		recursion->right = right;           \
	}

/* Insertion sort inner-loop */
#define INSERTION_SORT_LOOP(LEFT)                                 \
	{                                                             \
		register char *trail = idx - size;                        \
		while (trail >= LEFT && compare(trail, trail + size, userdata) > 0) \
		{                                                         \
			SWAP_NEXT(trail,size)                                 \
			trail -= size;                                        \
		}                                                         \
	}

/* Performs insertion sort left of the pivot */
#define INSERTION_SORT_LEFT                                \
	for (idx = left + size; idx < store; idx +=size)       \
		INSERTION_SORT_LOOP(left)

/* Performs insertion sort right of the pivot */
#define INSERTION_SORT_RIGHT                                        \
	for (idx = store + (size << 1); idx <= right; idx +=size)       \
		INSERTION_SORT_LOOP(store + size)

/* Sorts to the left */
#if INSERTION_SORT_THRESHOLD_SHIFT == 0
# define SORT_LEFT RECURSE_LEFT
#else
# define SORT_LEFT                 \
	if (store - left <= threshold) \
	{                              \
		INSERTION_SORT_LEFT        \
	}                              \
	else                           \
	{                              \
		RECURSE_LEFT               \
	}
#endif

/* Sorts to the right */
#if INSERTION_SORT_THRESHOLD_SHIFT == 0
# define SORT_RIGHT RECURSE_RIGHT
#else
# define SORT_RIGHT                 \
	if (right - store <= threshold) \
	{                               \
		INSERTION_SORT_RIGHT        \
	}                               \
	else                            \
	{                               \
		RECURSE_RIGHT               \
	}
#endif

		/* Recurse into the smaller partition first */
		if (store - left < right - store)
		{
		/* Left side is smaller */
			SORT_RIGHT
			SORT_LEFT

			continue;
		}

		/* Right side is smaller */
		SORT_LEFT
		SORT_RIGHT

#undef RECURSE_LEFT
#undef RECURSE_RIGHT
#undef INSERTION_SORT_LOOP
#undef INSERTION_SORT_LEFT
#undef INSERTION_SORT_RIGHT
#undef SORT_LEFT
#undef SORT_RIGHT
	}
	while (recursion >= stack);
}

#undef INSERTION_SORT_THRESHOLD_SHIFT
#undef SWAP
#undef SWAP_NEXT

/**** END CUSTOM QSORT CODE ****/


#define U8NFL( x ) ((x&0xC0)!=0x80)

int sgs_utf8_decode( char* buf, size_t size, uint32_t* outchar )
{
	char c;
	if( size == 0 )
		return 0;
	
	c = *buf;
	if( !( c & 0x80 ) )
	{
		*outchar = (uint32_t) c;
		return 1;
	}
	
	if( ( c & 0xE0 ) == 0xC0 )
	{
		if( size < 2 || U8NFL( buf[1] ) )
			return - (int) SGS_MIN(size,2);
		*outchar = (uint32_t) ( ( ((int)(buf[0]&0x1f)) << 6 ) | ((int)(buf[1]&0x3f)) );
		return 2;
	}
	
	if( ( c & 0xF0 ) == 0xE0 )
	{
		if( size < 3 || U8NFL( buf[1] ) || U8NFL( buf[2] ) )
			return - (int) SGS_MIN(size,3);
		*outchar = (uint32_t) ( ( ((int)(buf[0]&0x0f)) << 12 ) | ( ((int)(buf[1]&0x3f)) << 6 )
			| ((int)(buf[2]&0x3f)) );
		return 3;
	}
	
	if( ( c & 0xF8 ) == 0xF0 )
	{
		if( size < 4 || U8NFL( buf[1] ) || U8NFL( buf[2] ) || U8NFL( buf[3] ) )
			return - (int) SGS_MIN(size,4);
		*outchar = (uint32_t) ( ( ((int)(buf[0]&0x07)) << 18 ) | ( ((int)(buf[1]&0x3f)) << 12 )
				| ( ((int)(buf[2]&0x3f)) << 6 ) | ((int)(buf[3]&0x3f)) );
		return 4;
	}
	
	return -1;
}

int sgs_utf8_encode( uint32_t ch, char* out )
{
	if( ch <= 0x7f )
	{
		*out = (char) ch;
		return 1;
	}
	if( ch <= 0x7ff )
	{
		out[ 0 ] = (char)( 0xc0 | ( ( ch >> 6 ) & 0x1f ) );
		out[ 1 ] = (char)( 0x80 | ( ch & 0x3f ) );
		return 2;
	}
	if( ch <= 0xffff )
	{
		out[ 0 ] = (char)( 0xe0 | ( ( ch >> 12 ) & 0x0f ) );
		out[ 1 ] = (char)( 0x80 | ( ( ch >> 6 ) & 0x3f ) );
		out[ 2 ] = (char)( 0x80 | ( ch & 0x3f ) );
		return 3;
	}
	if( ch <= 0x10ffff )
	{
		out[ 0 ] = (char)( 0xf0 | ( ( ch >> 18 ) & 0x07 ) );
		out[ 1 ] = (char)( 0x80 | ( ( ch >> 12 ) & 0x3f ) );
		out[ 2 ] = (char)( 0x80 | ( ( ch >> 6 ) & 0x3f ) );
		out[ 3 ] = (char)( 0x80 | ( ch & 0x3f ) );
		return 4;
	}

	return 0;
}


