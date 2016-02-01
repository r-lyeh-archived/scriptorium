
#ifndef SGS_UTIL_H_INCLUDED
#define SGS_UTIL_H_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

#include "sgscript.h"


/* useful shortcut definitions */
#define SGS_MAX( a, b ) ((a)>(b)?(a):(b))
#define SGS_MIN( a, b ) ((a)<(b)?(a):(b))
#define SGS_ARRAY_SIZE( a ) (sizeof(a)/sizeof(a[0]))
#define SGS_HAS_FLAG( val, flag ) (((val)&(flag))==(flag))
#define SGS_HAS_ANY_FLAG( val, flag ) (((val)&(flag))!=0)

#define SGS_GLUE(a,b) __SGS_GLUE(a,b)
#define __SGS_GLUE(a,b) a ## b
#define SGS_CASSERT(expr, msg) typedef char SGS_GLUE (compiler_verify_, msg) [(expr) ? (+1) : (-1)]


typedef int16_t sgs_LineNum;

SGS_APIFUNC void sgs_BreakIfFunc( const char* code, const char* file, int line );
#if SGS_DEBUG && SGS_DEBUG_VALIDATE
#  define sgs_BreakIf( expr ) { if( (expr) != 0 ){ sgs_BreakIfFunc( #expr, __FILE__, __LINE__ ); } }
#else
#  define sgs_BreakIf( expr )
#endif


/* text/character ops */
int sgs_isoneof( int chr, const char* str );
SGS_APIFUNC int sgs_hexchar( int c );
SGS_APIFUNC int sgs_gethex( int c );
int sgs_tolower( int c );
#define sgs_decchar( c ) ((c) >= '0' && (c) <= '9')
#define sgs_getdec( c ) ((c) - '0')
#define sgs_octchar( c ) ((c) >= '0' && (c) <= '7')
#define sgs_getoct( c ) ((c) - '0')
#define sgs_binchar( c ) ((c) == '0' || (c) == '1')
#define sgs_getbin( c ) ((c) - '0')
#define sgs_isalpha( c ) (((c) >= 'a' && (c) <= 'z') || ((c) >= 'A' && (c) <= 'Z' ))
#define sgs_isalnum( c ) (((c) >= 'a' && (c) <= 'z') || \
	((c) >= 'A' && (c) <= 'Z') || ((c) >= '0' && (c) <= '9'))
#define sgs_isdigit( c ) ((c) >= '0' && (c) <= '9')
#define sgs_isgraph( c ) ((c) >= 0x21 && (c) <= 0x7E)


#define SGS_AS_( tgt, ptr, wat ) do{ memcpy( &(tgt), (ptr), sizeof(wat) ); }while(0)
#define SGS_AS_INT8( tgt, ptr ) SGS_AS_( tgt, ptr, int8_t )
#define SGS_AS_UINT8( tgt, ptr ) SGS_AS_( tgt, ptr, uint8_t )
#define SGS_AS_INT16( tgt, ptr ) SGS_AS_( tgt, ptr, int16_t )
#define SGS_AS_UINT16( tgt, ptr ) SGS_AS_( tgt, ptr, uint16_t )
#define SGS_AS_INT32( tgt, ptr ) SGS_AS_( tgt, ptr, int32_t )
#define SGS_AS_UINT32( tgt, ptr ) SGS_AS_( tgt, ptr, uint32_t )
#define SGS_AS_INT64( tgt, ptr ) SGS_AS_( tgt, ptr, int64_t )
#define SGS_AS_UINT64( tgt, ptr ) SGS_AS_( tgt, ptr, uint64_t )
#define SGS_AS_FLOAT( tgt, ptr ) SGS_AS_( tgt, ptr, float )
#define SGS_AS_DOUBLE( tgt, ptr ) SGS_AS_( tgt, ptr, double )

#define SGS_AS_INTEGER( tgt, ptr ) SGS_AS_( tgt, ptr, sgs_Int )
#define SGS_AS_REAL( tgt, ptr ) SGS_AS_( tgt, ptr, sgs_Real )


/* flow/data debugging */
#if SGS_DEBUG && SGS_DEBUG_FLOW
#  define SGS_FN_HIT( what ) \
	printf( "Hit \"%s\" line %d in function \"%s\"\n", what, __LINE__, __FUNCTION__ );
#  define SGS_FN_ENTER \
	printf( "Entering a function from \"%s\" at line %d\n", __FUNCTION__, __LINE__ );
#  define SGS_FN_BEGIN \
	printf( "Inside \"%s\"\n", __FUNCTION__ );
#  define SGS_FN_END \
	printf( "Out of \"%s\" at line %d\n", __FUNCTION__, __LINE__ );
#else
#  define SGS_FN_HIT( what )
#  define SGS_FN_ENTER
#  define SGS_FN_BEGIN
#  define SGS_FN_END
#endif

SGS_APIFUNC void sgs_print_safe( FILE* fp, const char* buf, size_t size );


/* string buffer */
typedef
struct _sgs_MemBuf
{
	char*  ptr;
	size_t size;
	size_t mem;
}
sgs_MemBuf;


/* data buffer */
SGS_APIFUNC sgs_MemBuf sgs_membuf_create( void );
SGS_APIFUNC void sgs_membuf_destroy( sgs_MemBuf* sb, SGS_CTX );
SGS_APIFUNC sgs_MemBuf sgs_membuf_partial( char* ch, size_t size );
SGS_APIFUNC void sgs_membuf_reserve( sgs_MemBuf* mb, SGS_CTX, size_t size );
SGS_APIFUNC void sgs_membuf_resize( sgs_MemBuf* mb, SGS_CTX, size_t size );
SGS_APIFUNC void sgs_membuf_resize_opt( sgs_MemBuf* mb, SGS_CTX, size_t size );
SGS_APIFUNC void sgs_membuf_insbuf( sgs_MemBuf* mb, SGS_CTX, size_t pos, const void* buf, size_t size );
SGS_APIFUNC void sgs_membuf_erase( sgs_MemBuf* mb, size_t from, size_t to );
SGS_APIFUNC void sgs_membuf_appbuf( sgs_MemBuf* mb, SGS_CTX, const void* buf, size_t size );
static SGS_INLINE void sgs_membuf_setstr( sgs_MemBuf* mb, SGS_CTX, const char* str )
	{ mb->size = 0; sgs_membuf_appbuf( mb, C, str, strlen( str ) + 1 ); mb->size--; }
static SGS_INLINE void sgs_membuf_setstrbuf( sgs_MemBuf* mb, SGS_CTX, const char* str, size_t size )
	{ sgs_membuf_reserve( mb, C, size + 1 ); mb->size = 0;
		sgs_membuf_appbuf( mb, C, str, size ); mb->ptr[ mb->size ] = 0; }
static SGS_INLINE void sgs_membuf_appchr( sgs_MemBuf* mb, SGS_CTX, char chr )
	{ sgs_membuf_appbuf( mb, C, &chr, 1 ); }


/* hashing functions */
typedef uint32_t sgs_Hash;
SGS_APIFUNC sgs_Hash sgs_HashFunc( const char* str, size_t size );
SGS_APIFUNC sgs_Hash sgs_HashVar( const sgs_Variable* v );


/* hash table */
typedef sgs_SizeVal sgs_VHTIdx;

#define SGS_VHTIDX_EMPTY -1
#define SGS_VHTIDX_REMOVED -2

typedef
struct _sgs_VHTVar
{
	sgs_Variable key;
	sgs_Variable val;
	sgs_Hash     hash;
}
sgs_VHTVar;

typedef
struct _sgs_VHTable
{
	sgs_VHTIdx* pairs;
	sgs_VHTVar* vars;
	sgs_VHTIdx  pair_mem;
	sgs_VHTIdx  var_mem;
	sgs_VHTIdx  size;
	sgs_VHTIdx  num_rem;
}
sgs_VHTable;

SGS_APIFUNC void sgs_vht_init( sgs_VHTable* T, SGS_CTX, int32_t initial_pair_mem, int32_t initial_var_mem );
SGS_APIFUNC void sgs_vht_free( sgs_VHTable* T, SGS_CTX );
SGS_APIFUNC sgs_VHTIdx sgs_vht_pair_id( sgs_VHTable* T, sgs_Variable* K, sgs_Hash hash );
SGS_APIFUNC sgs_VHTVar* sgs_vht_get( sgs_VHTable* T, sgs_Variable* K );
SGS_APIFUNC sgs_VHTVar* sgs_vht_get_str( sgs_VHTable* T, const char* str, uint32_t size, sgs_Hash hash );
SGS_APIFUNC sgs_VHTVar* sgs_vht_set( sgs_VHTable* T, SGS_CTX, sgs_Variable* K, sgs_Variable* V );
SGS_APIFUNC void sgs_vht_unset( sgs_VHTable* T, SGS_CTX, sgs_Variable* K );

#define sgs_vht_size( T ) ((T)->size)


SGS_APIFUNC double sgs_GetTime();


/* returns 0 on failure, 1/2 on integer/real */
SGS_APIFUNC int sgs_util_strtonum( const char** at, const char* end, sgs_Int* outi, sgs_Real* outf );
SGS_APIFUNC sgs_Int sgs_util_atoi( const char* str, size_t len );
SGS_APIFUNC sgs_Real sgs_util_atof( const char* str, size_t len );


/* crc32 */
SGS_APIFUNC uint32_t sgs_crc32( const void* buf, size_t len, uint32_t in_crc );


SGS_APIFUNC void sgs_quicksort( void *array, size_t length, size_t size,
	int(*compare)(const void *, const void *, void*), void* userdata);

/*
	UNICODE helper functions
	- utf8_decode: returns number of bytes parsed (negated if input was invalid)
	- utf8_encode: returns number of bytes written (up to 4, make sure there's space)
*/
#define SGS_UNICODE_INVCHAR 0xfffd
#define SGS_UNICODE_INVCHAR_STR "\xef\xbf\xbd"
#define SGS_UNICODE_INVCHAR_LEN 3
SGS_APIFUNC int sgs_utf8_decode( char* buf, size_t size, uint32_t* outchar );
SGS_APIFUNC int sgs_utf8_encode( uint32_t ch, char* out );


#ifdef __cplusplus
}
#endif

#endif /* SGS_UTIL_H_INCLUDED */
