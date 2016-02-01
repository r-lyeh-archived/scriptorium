

#include <stdio.h>
#include <errno.h>

#include "sgs_int.h"

#ifndef STDLIB_WARN
#  define STDLIB_WARN( warn ) return sgs_Msg( C, SGS_WARNING, warn );
#endif
#ifndef STDLIB_ERR
#  define STDLIB_ERR( err ) return sgs_Msg( C, SGS_ERROR, err );
#endif


static int sgsstd_expectnum( SGS_CTX, sgs_StkIdx n )
{
	sgs_StkIdx ssz = sgs_StackSize( C );
	if( n != ssz )
		return sgs_Msg( C, SGS_WARNING, "function expects exactly %d arguments"
			", %d given", n, ssz );
	return 1;
}


/* Containers */


/*
	ARRAY
*/

#define SGSARR_UNIT sizeof( sgs_Variable )
#define SGSARR_HDR sgsstd_array_header_t* hdr = (sgsstd_array_header_t*) data
#define SGSARR_HDR_OI sgsstd_array_header_t* hdr = (sgsstd_array_header_t*) obj->data
#define SGSARR_HDRSIZE sizeof(sgsstd_array_header_t)
#define SGSARR_ALLOCSIZE( cnt ) ((size_t)(cnt)*SGSARR_UNIT)
#define SGSARR_PTR( base ) (((sgsstd_array_header_t*)base)->data)

static void sgsstd_array_reserve( SGS_CTX, sgsstd_array_header_t* hdr, sgs_SizeVal size )
{
	if( size <= hdr->mem )
		return;

	hdr->data = (sgs_Variable*) sgs_Realloc( C, hdr->data, SGSARR_ALLOCSIZE( size ) );
	hdr->mem = size;
}

static void sgsstd_array_clear( SGS_CTX, sgsstd_array_header_t* hdr )
{
	sgs_ReleaseArray( C, SGSARR_PTR( hdr ), hdr->size );
	hdr->size = 0;
}

/* off = offset in stack to start inserting from */
static void sgsstd_array_insert( SGS_CTX, sgsstd_array_header_t* hdr, sgs_SizeVal pos, sgs_StkIdx off )
{
	int i;
	sgs_StkIdx cnt = sgs_StackSize( C ) - off;
	sgs_SizeVal nsz = hdr->size + cnt;
	sgs_Variable* ptr = SGSARR_PTR( hdr );
	
	if( !cnt ) return;
	
	if( nsz > hdr->mem )
	{
		sgsstd_array_reserve( C, hdr, SGS_MAX( nsz, hdr->mem * 2 ) );
		ptr = SGSARR_PTR( hdr );
	}
	if( pos < hdr->size )
		memmove( ptr + pos + cnt, ptr + pos, SGSARR_ALLOCSIZE( hdr->size - pos ) );
	for( i = off; i < sgs_StackSize( C ); ++i )
	{
		sgs_Variable* var = ptr + pos + i - off;
		sgs_GetStackItem( C, i, var );
	}
	hdr->size = nsz;
}

static void sgsstd_array_insert_p( SGS_CTX, sgsstd_array_header_t* hdr, sgs_SizeVal pos, sgs_Variable* var )
{
	sgs_SizeVal nsz = hdr->size + 1;
	sgs_Variable* ptr = SGSARR_PTR( hdr );
	
	if( nsz > hdr->mem )
	{
		sgsstd_array_reserve( C, hdr, SGS_MAX( nsz, hdr->mem * 2 ) );
		ptr = SGSARR_PTR( hdr );
	}
	if( pos < hdr->size )
		memmove( ptr + pos + 1, ptr + pos, SGSARR_ALLOCSIZE( hdr->size - pos ) );
	
	ptr[ pos ] = *var;
	sgs_Acquire( C, var );
	
	hdr->size = nsz;
}

static void sgsstd_array_erase( SGS_CTX, sgsstd_array_header_t* hdr, sgs_SizeVal from, sgs_SizeVal to )
{
	sgs_SizeVal i;
	sgs_SizeVal cnt = to - from + 1, to1 = to + 1;
	sgs_Variable* ptr = SGSARR_PTR( hdr );
	
	sgs_BreakIf( from < 0 || from >= hdr->size || to < 0 || to >= hdr->size || from > to );
	
	for( i = from; i <= to; ++i )
		sgs_Release( C, ptr + i );
	if( to1 < hdr->size )
		memmove( ptr + from, ptr + to1, SGSARR_ALLOCSIZE( hdr->size - to1 ) );
	hdr->size -= cnt;
}


#define SGSARR_IHDR( name ) \
	sgsstd_array_header_t* hdr; \
	if( !SGS_PARSE_METHOD( C, sgsstd_array_iface, hdr, array, name ) ) return 0; \
	SGS_UNUSED( hdr );


static int sgsstd_arrayI_push( SGS_CTX )
{
	SGSARR_IHDR( push );
	sgsstd_array_insert( C, hdr, hdr->size, 0 );
	
	SGS_RETURN_THIS( C );
}
static int sgsstd_arrayI_pop( SGS_CTX )
{
	sgs_Variable* ptr;
	SGSARR_IHDR( pop );
	ptr = SGSARR_PTR( hdr );
	if( !hdr->size )
		STDLIB_WARN( "array is empty, cannot pop" );
	
	sgs_PushVariable( C, ptr[ hdr->size - 1 ] );
	sgsstd_array_erase( C, hdr, hdr->size - 1, hdr->size - 1 );
	return 1;
}
static int sgsstd_arrayI_shift( SGS_CTX )
{
	sgs_Variable* ptr;
	SGSARR_IHDR( shift );
	ptr = SGSARR_PTR( hdr );
	if( !hdr->size )
		STDLIB_WARN( "array is empty, cannot shift" );
	
	sgs_PushVariable( C, ptr[ 0 ] );
	sgsstd_array_erase( C, hdr, 0, 0 );
	return 1;
}
static int sgsstd_arrayI_unshift( SGS_CTX )
{
	SGSARR_IHDR( unshift );
	sgsstd_array_insert( C, hdr, 0, 0 );
	
	SGS_RETURN_THIS( C );
}

static int sgsstd_arrayI_insert( SGS_CTX )
{
	sgs_Int at;
	SGSARR_IHDR( insert );
	
	if( !sgs_LoadArgs( C, "i?v", &at ) )
		return 0;
	
	if( at < 0 )
		at += hdr->size + 1;
	if( at < 0 || at > hdr->size )
		STDLIB_WARN( "index out of bounds" )
	
	sgsstd_array_insert( C, hdr, (sgs_SizeVal) at, 1 );
	
	SGS_RETURN_THIS( C );
}
static int sgsstd_arrayI_erase( SGS_CTX )
{
	int cnt = sgs_StackSize( C );
	sgs_Int at, at2;
	SGSARR_IHDR( erase );
	
	if( !sgs_LoadArgs( C, "i|i", &at, &at2 ) )
		return 0;
	
	if( at < 0 )
		at += hdr->size;
	if( at < 0 || at >= hdr->size )
		STDLIB_WARN( "index out of bounds" )
	
	if( cnt == 1 )
		at2 = at;
	else
	{
		if( at2 < 0 )
			at2 += hdr->size;
		if( at2 < 0 || at2 >= hdr->size )
			STDLIB_WARN( "index out of bounds" )
		
		if( at2 < at )
			STDLIB_WARN( "after resolving,"
				" index #1 must be smaller or equal than index #2" )
	}
	
	sgsstd_array_erase( C, hdr, (sgs_SizeVal) at, (sgs_SizeVal) at2 );
	
	SGS_RETURN_THIS( C );
}
static int sgsstd_arrayI_part( SGS_CTX )
{
	sgs_SizeVal from, max = 0x7fffffff, to, i;
	SGSARR_IHDR( part );
	
	if( !sgs_LoadArgs( C, "l|l", &from, &max ) )
		return 0;
	
	if( max < 0 )
		STDLIB_WARN( "argument 2 (count) cannot be negative" )
	
	if( from < 0 )
		from += hdr->size;
	to = from + max;
	if( to < from )
		to = hdr->size;
	
	sgs_CreateArray( C, NULL, 0 );
	if( from < hdr->size && 0 < to )
	{
		from = SGS_MAX( from, 0 );
		to = SGS_MIN( to, hdr->size );
		
		if( from < to )
		{
			sgs_Variable *psrc, *pdst;
			sgs_SizeVal cnt = to - from;
			sgsstd_array_header_t* nhdr = (sgsstd_array_header_t*) sgs_GetObjectData( C, -1 );
			sgsstd_array_reserve( C, nhdr, to - from );
			nhdr->size = cnt;
			psrc = SGSARR_PTR( hdr ) + from;
			pdst = SGSARR_PTR( nhdr );
			for( i = 0; i < cnt; ++i )
			{
				pdst[ i ] = psrc[ i ];
				sgs_Acquire( C, pdst + i );
			}
		}
	}
	
	return 1;
}

static int sgsstd_arrayI_clear( SGS_CTX )
{
	SGSARR_IHDR( clear );
	sgsstd_array_clear( C, hdr );
	
	SGS_RETURN_THIS( C );
}

static int sgsstd_arrayI_reverse( SGS_CTX )
{
	SGSARR_IHDR( reverse );
	
	/* emit a warning if any arguments are passed */
	if( !sgs_LoadArgs( C, "." ) )
		return 0;
	
	{
		sgs_Variable tmp;
		sgs_Variable* P = SGSARR_PTR( hdr );
		sgs_SizeVal i, j, hsz = hdr->size / 2;
		for( i = 0, j = hdr->size - 1; i < hsz; ++i, --j )
		{
			tmp = P[ i ];
			P[ i ] = P[ j ];
			P[ j ] = tmp;
		}
	}
	
	SGS_RETURN_THIS( C );
}

static void sgsstd_array_adjust( SGS_CTX, sgsstd_array_header_t* hdr, sgs_SizeVal cnt )
{
	while( hdr->size > cnt )
	{
		sgs_Release( C, SGSARR_PTR( hdr ) + hdr->size - 1 );
		hdr->size--;
	}
	while( hdr->size < cnt )
	{
		( SGSARR_PTR( hdr ) + (hdr->size++) )->type = SGS_VT_NULL;
	}
}
static int sgsstd_arrayI_resize( SGS_CTX )
{
	sgs_SizeVal sz;
	SGSARR_IHDR( resize );
	
	if( !sgs_LoadArgs( C, "l", &sz ) )
		return 0;
	
	if( sz < 0 )
		STDLIB_WARN( "argument 1 (size) must be bigger than or equal to 0" )
	
	sgsstd_array_reserve( C, hdr, sz );
	sgsstd_array_adjust( C, hdr, sz );
	
	SGS_RETURN_THIS( C );
}
static int sgsstd_arrayI_reserve( SGS_CTX )
{
	sgs_SizeVal sz;
	SGSARR_IHDR( reserve );
	
	if( !sgs_LoadArgs( C, "l", &sz ) )
		return 0;
	
	if( sz < 0 )
		STDLIB_WARN( "argument 1 (size) must be bigger than or equal to 0" )
	
	sgsstd_array_reserve( C, hdr, sz );
	
	SGS_RETURN_THIS( C );
}

static SGS_INLINE int sgsarrcomp_basic( const void* p1, const void* p2, void* userdata )
{
	SGS_CTX = (sgs_Context*) userdata;
	sgs_Variable v1 = *(const sgs_Variable*) p1;
	sgs_Variable v2 = *(const sgs_Variable*) p2;
	return sgs_Compare( C, &v1, &v2 );
}
static SGS_INLINE int sgsarrcomp_basic_rev( const void* p1, const void* p2, void* userdata )
{ return sgsarrcomp_basic( p2, p1, userdata ); }
static int sgsstd_arrayI_sort( SGS_CTX )
{
	int rev = 0;
	SGSARR_IHDR( sort );
	
	if( !sgs_LoadArgs( C, "|b", &rev ) )
		return 0;
	
	/* WP: array limit */
	sgs_quicksort( SGSARR_PTR( hdr ), (size_t) hdr->size, sizeof( sgs_Variable ),
		rev ? sgsarrcomp_basic_rev : sgsarrcomp_basic, C );
	
	SGS_RETURN_THIS( C );
}

typedef struct sgsarrcomp_cl2_s
{
	SGS_CTX;
	sgs_Variable sortfunc;
}
sgsarrcomp_cl2;
static SGS_INLINE int sgsarrcomp_custom( const void* p1, const void* p2, void* userdata )
{
	sgsarrcomp_cl2* u = (sgsarrcomp_cl2*) userdata;
	sgs_Variable v1 = *(const sgs_Variable*) p1;
	sgs_Variable v2 = *(const sgs_Variable*) p2;
	SGS_CTX = u->C;
	sgs_PushVariable( C, v1 );
	sgs_PushVariable( C, v2 );
	sgs_Call( C, u->sortfunc, 2, 1 );
	{
		sgs_Real r = sgs_GetReal( C, -1 );
		sgs_Pop( C, 1 );
		return r == 0 ? 0 : ( r < 0 ? -1 : 1 );
	}
}
static SGS_INLINE int sgsarrcomp_custom_rev( const void* p1, const void* p2, void* userdata )
{ return sgsarrcomp_custom( p2, p1, userdata ); }
static int sgsstd_arrayI_sort_custom( SGS_CTX )
{
	int rev = 0;
	sgsarrcomp_cl2 u;
	SGSARR_IHDR( sort_custom );
	
	u.C = C;
	u.sortfunc.type = SGS_VT_NULL;
	
	if( !sgs_LoadArgs( C, "?p<v|b", &u.sortfunc, &rev ) )
		return 0;
	
	/* WP: array limit */
	sgs_quicksort( SGSARR_PTR( hdr ), (size_t) hdr->size,
		sizeof( sgs_Variable ), rev ? sgsarrcomp_custom_rev : sgsarrcomp_custom, &u );
	
	SGS_RETURN_THIS( C );
}

typedef struct sgsarr_smi_s
{
	sgs_Real value;
	sgs_SizeVal pos;
}
sgsarr_smi;
static SGS_INLINE int sgsarrcomp_smi( const void* p1, const void* p2, void* userdata )
{
	const sgsarr_smi *v1 = (const sgsarr_smi*) p1;
	const sgsarr_smi *v2 = (const sgsarr_smi*) p2;
	if( v1->value < v2->value )
		return -1;
	return v1->value > v2->value ? 1 : 0;
}
static SGS_INLINE int sgsarrcomp_smi_rev( const void* p1, const void* p2, void* userdata )
{ return sgsarrcomp_smi( p2, p1, userdata ); }
static int sgsstd_arrayI_sort_mapped( SGS_CTX )
{
	sgs_Variable arr;
	sgs_SizeVal i, asize = 0;
	int rev = 0;
	
	SGSARR_IHDR( sort_mapped );
	if( !sgs_LoadArgs( C, "a<v|b", &asize, &arr, &rev ) )
		return 0;
	
	if( asize != hdr->size )
		STDLIB_WARN( "array sizes must match" )
	
	{
		/* WP: array limit */
		sgsarr_smi* smis = sgs_Alloc_n( sgsarr_smi, (size_t) asize );
		for( i = 0; i < asize; ++i )
		{
			if( sgs_PushNumIndex( C, arr, i ) == SGS_FALSE )
			{
				sgs_Dealloc( smis );
				STDLIB_WARN( "error in mapping array" )
			}
			smis[ i ].value = sgs_GetReal( C, -1 );
			smis[ i ].pos = i;
			sgs_Pop( C, 1 );
		}
		sgs_quicksort( smis, (size_t) asize, sizeof( sgsarr_smi ),
			rev ? sgsarrcomp_smi_rev : sgsarrcomp_smi, NULL );
		
		{
			sgs_Variable *p1, *p2;
			p1 = SGSARR_PTR( hdr );
			p2 = sgs_Alloc_n( sgs_Variable, (size_t) hdr->mem );
			memcpy( p2, p1, SGSARR_ALLOCSIZE( hdr->mem ) );
			for( i = 0; i < asize; ++i )
				p1[ i ] = p2[ smis[ i ].pos ];
			sgs_Dealloc( p2 );
		}
		
		sgs_Dealloc( smis );
		
		SGS_RETURN_THIS( C );
	}
}

static int sgsstd_arrayI_find( SGS_CTX )
{
	sgs_Variable comp;
	int strict = SGS_FALSE;
	sgs_SizeVal off = 0;
	
	SGSARR_IHDR( find );
	if( !sgs_LoadArgs( C, "v|bl", &comp, &strict, &off ) )
		return 0;
	
	while( off < hdr->size )
	{
		sgs_Variable* p = SGSARR_PTR( hdr ) + off;
		if( ( !strict || sgs_EqualTypes( p, &comp ) )
			&& sgs_Compare( C, p, &comp ) == 0 )
		{
			sgs_PushInt( C, off );
			return 1;
		}
		off++;
	}
	return 0;
}

static int sgsstd_arrayI_remove( SGS_CTX )
{
	sgs_Variable comp;
	int strict = SGS_FALSE, all = SGS_FALSE, rmvd = 0;
	sgs_SizeVal off = 0;
	
	SGSARR_IHDR( remove );
	if( !sgs_LoadArgs( C, "v|bbl", &comp, &strict, &all, &off ) )
		return 0;
	
	while( off < hdr->size )
	{
		sgs_Variable* p = SGSARR_PTR( hdr ) + off;
		if( ( !strict || sgs_EqualTypes( p, &comp ) )
			&& sgs_Compare( C, p, &comp ) == 0 )
		{
			sgsstd_array_erase( C, hdr, off, off );
			rmvd++;
			if( !all )
				break;
		}
		else
			off++;
	}
	sgs_PushInt( C, rmvd );
	return 1;
}

static int _in_array( SGS_CTX, void* data, sgs_Variable* var, int strconv )
{
	sgs_SizeVal off = 0;
	SGSARR_HDR;
	
	if( !strconv )
	{
		while( off < hdr->size )
		{
			sgs_Variable* cur = SGSARR_PTR( hdr ) + off;
			if( sgs_EqualTypes( var, cur ) && sgs_Compare( C, var, cur ) == 0 )
				return SGS_TRUE;
			off++;
		}
		return SGS_FALSE;
	}
	else
	{
		int found = 0;
		sgs_Variable A, B;
		
		A = *var;
		sgs_Acquire( C, &A );
		sgs_ToStringP( C, &A );
		while( off < hdr->size )
		{
			B = SGSARR_PTR( hdr )[ off ];
			sgs_Acquire( C, &B );
			sgs_ToStringP( C, &B );
			
			found = sgs_EqualTypes( &A, &B ) && sgs_Compare( C, &A, &B ) == 0;
			sgs_Release( C, &B );
			if( found )
				break;
			off++;
		}
		sgs_Release( C, &A );
		return found;
	}
}

static int sgsstd_arrayI_unique( SGS_CTX )
{
	int strconv = SGS_FALSE;
	sgs_SizeVal off = 0, asz = 0;
	void* nadata;
	
	SGSARR_IHDR( unique );
	if( !sgs_LoadArgs( C, "|b", &strconv ) )
		return 0;
	
	sgs_CreateArray( C, NULL, 0 );
	nadata = sgs_GetObjectData( C, -1 );
	while( off < hdr->size )
	{
		sgs_Variable* var = SGSARR_PTR( hdr ) + off;
		if( !_in_array( C, nadata, var, strconv ) )
		{
			sgsstd_array_insert_p( C, (sgsstd_array_header_t*) nadata, asz, var );
			asz++;
		}
		off++;
	}
	
	return 1;
}

static int sgsstd_arrayI_random( SGS_CTX )
{
	sgs_SizeVal num, asz = 0;
	sgsstd_array_header_t* nadata;
	
	SGSARR_IHDR( random );
	if( !sgs_LoadArgs( C, "l", &num ) )
		return 0;
	
	if( num < 0 )
		STDLIB_WARN( "argument 1 (count) cannot be negative" )
	
	sgs_CreateArray( C, NULL, 0 );
	nadata = (sgsstd_array_header_t*) sgs_GetObjectData( C, -1 );
	sgsstd_array_reserve( C, nadata, num );
	while( num-- )
	{
		sgsstd_array_insert_p( C, nadata, asz, SGSARR_PTR( hdr ) + ( rand() % hdr->size ) );
		asz++;
	}
	
	return 1;
}

static int sgsstd_arrayI_shuffle( SGS_CTX )
{
	sgs_Variable tmp;
	sgs_SizeVal i, j;
	
	SGSARR_IHDR( shuffle );
	if( !sgs_LoadArgs( C, "." ) )
		return 0;
	
	for( i = hdr->size - 1; i >= 1; i-- )
	{
		j = rand() % ( i + 1 );
		tmp = SGSARR_PTR( hdr )[ i ];
		SGSARR_PTR( hdr )[ i ] = SGSARR_PTR( hdr )[ j ];
		SGSARR_PTR( hdr )[ j ] = tmp;
	}
	
	SGS_RETURN_THIS( C );
}


static int sgsstd_array( SGS_CTX );
static sgs_RegFuncConst array_iface_fconsts[] =
{
	{ "push", sgsstd_arrayI_push },
	{ "pop", sgsstd_arrayI_pop },
	{ "shift", sgsstd_arrayI_shift },
	{ "unshift", sgsstd_arrayI_unshift },
	{ "insert", sgsstd_arrayI_insert },
	{ "erase", sgsstd_arrayI_erase },
	{ "part", sgsstd_arrayI_part },
	{ "clear", sgsstd_arrayI_clear },
	{ "reverse", sgsstd_arrayI_reverse },
	{ "resize", sgsstd_arrayI_resize },
	{ "reserve", sgsstd_arrayI_reserve },
	{ "sort", sgsstd_arrayI_sort },
	{ "sort_custom", sgsstd_arrayI_sort_custom },
	{ "sort_mapped", sgsstd_arrayI_sort_mapped },
	{ "find", sgsstd_arrayI_find },
	{ "remove", sgsstd_arrayI_remove },
	{ "unique", sgsstd_arrayI_unique },
	{ "random", sgsstd_arrayI_random },
	{ "shuffle", sgsstd_arrayI_shuffle },
	{ "__call", sgsstd_array },
};

static int sgsstd_array_iface_gen( SGS_CTX )
{
	sgs_CreateDict( C, NULL, 0 );
	sgs_StoreFuncConsts( C, sgs_StackItem( C, -1 ), array_iface_fconsts,
		SGS_ARRAY_SIZE(array_iface_fconsts), "array." );
	sgs_ObjSetMetaMethodEnable( sgs_GetObjectStruct( C, -1 ), 1 );
	return 1;
}

static int sgsstd_array_getprop( SGS_CTX, void* data )
{
	char* name;
	SGSARR_HDR;
	if( sgs_ParseString( C, 0, &name, NULL ) )
	{
		if( 0 == strcmp( name, "size" ) )
		{
			sgs_PushInt( C, hdr->size );
			return SGS_SUCCESS;
		}
		else if( 0 == strcmp( name, "capacity" ) )
		{
			sgs_PushInt( C, hdr->mem );
			return SGS_SUCCESS;
		}
		else if( 0 == strcmp( name, "first" ) )
		{
			if( hdr->size )
				sgs_PushVariable( C, SGSARR_PTR( hdr )[ 0 ] );
			else
			{
				sgs_PushNull( C );
				sgs_Msg( C, SGS_WARNING, "array is empty, cannot get first item" );
			}
			return SGS_SUCCESS;
		}
		else if( 0 == strcmp( name, "last" ) )
		{
			if( hdr->size )
				sgs_PushVariable( C, SGSARR_PTR( hdr )[ hdr->size - 1 ] );
			else
			{
				sgs_PushNull( C );
				sgs_Msg( C, SGS_WARNING, "array is empty, cannot get last item" );
			}
			return SGS_SUCCESS;
		}
	}
	return SGS_ENOTFND;
}

int sgsstd_array_getindex( SGS_ARGS_GETINDEXFUNC )
{
	if( C->object_arg )
		return sgsstd_array_getprop( C, obj->data );
	else
	{
		SGSARR_HDR_OI;
		sgs_Variable* ptr = SGSARR_PTR( hdr );
		sgs_Int i = sgs_GetInt( C, 0 );
		if( i < 0 || i >= hdr->size )
		{
			sgs_Msg( C, SGS_WARNING, "array index out of bounds" );
			return SGS_EBOUNDS;
		}
		sgs_PushVariable( C, ptr[ i ] );
		return SGS_SUCCESS;
	}
}

static int sgsstd_array_setindex( SGS_ARGS_SETINDEXFUNC )
{
	if( C->object_arg )
		return SGS_ENOTSUP;
	else
	{
		SGSARR_HDR_OI;
		sgs_Variable* ptr = SGSARR_PTR( hdr );
		sgs_Int i = sgs_GetInt( C, 0 );
		if( i < 0 || i >= hdr->size )
		{
			sgs_Msg( C, SGS_WARNING, "array index out of bounds" );
			return SGS_EBOUNDS;
		}
		sgs_Release( C, ptr + i );
		sgs_StoreVariable( C, ptr + i );
		return SGS_SUCCESS;
	}
}

static int sgsstd_array_dump( SGS_CTX, sgs_VarObj* obj, int depth )
{
	char bfr[ 32 ];
	int i, ssz = sgs_StackSize( C );
	SGSARR_HDR_OI;
	sprintf( bfr, "array (%"PRId32")\n[", hdr->size );
	sgs_PushString( C, bfr );
	if( depth )
	{
		if( hdr->size )
		{
			for( i = 0; i < hdr->size; ++i )
			{
				sgs_PushString( C, "\n" );
				sgs_DumpVar( C, SGSARR_PTR( hdr )[ i ], depth );
			}
			sgs_StringConcat( C, hdr->size * 2 );
			sgs_PadString( C );
		}
	}
	else
	{
		sgs_PushString( C, "\n..." );
		sgs_PadString( C );
	}
	sgs_PushString( C, "\n]" );
	sgs_StringConcat( C, sgs_StackSize( C ) - ssz );
	return SGS_SUCCESS;
}

static int sgsstd_array_gcmark( SGS_CTX, sgs_VarObj* obj )
{
	SGSARR_HDR_OI;
	sgs_GCMarkArray( C, SGSARR_PTR( hdr ), hdr->size );
	return SGS_SUCCESS;
}

/* iterator */

static int sgsstd_array_iter_destruct( SGS_CTX, sgs_VarObj* obj )
{
	sgs_Release( C, &((sgsstd_array_iter_t*) obj->data)->ref );
	return SGS_SUCCESS;
}

static int sgsstd_array_iter_getnext( SGS_CTX, sgs_VarObj* obj, int mask )
{
	sgsstd_array_iter_t* iter = (sgsstd_array_iter_t*) obj->data;
	sgsstd_array_header_t* hdr = (sgsstd_array_header_t*) iter->ref.data.O->data;
	if( iter->size != hdr->size )
	{
		sgs_Msg( C, SGS_ERROR, "array changed size during iteration" );
		return SGS_EINPROC;
	}
	
	if( !mask )
	{
		iter->off++;
		return iter->off < iter->size;
	}
	else
	{
		if( mask & SGS_GETNEXT_KEY )
			sgs_PushInt( C, iter->off );
		if( mask & SGS_GETNEXT_VALUE )
			sgs_PushVariable( C, SGSARR_PTR( hdr )[ iter->off ] );
		return SGS_SUCCESS;
	}
}

static int sgsstd_array_iter_gcmark( SGS_CTX, sgs_VarObj* obj )
{
	sgsstd_array_iter_t* iter = (sgsstd_array_iter_t*) obj->data;
	sgs_GCMark( C, &iter->ref );
	return SGS_SUCCESS;
}

SGS_APIFUNC sgs_ObjInterface sgsstd_array_iter_iface[1] =
{{
	"array_iterator",
	sgsstd_array_iter_destruct, sgsstd_array_iter_gcmark,
	NULL, NULL,
	NULL, NULL, NULL, sgsstd_array_iter_getnext,
	NULL, NULL
}};

static int sgsstd_array_convert( SGS_CTX, sgs_VarObj* obj, int type )
{
	SGSARR_HDR_OI;
	if( type == SGS_CONVOP_TOITER )
	{
		sgsstd_array_iter_t* iter = (sgsstd_array_iter_t*)
			sgs_CreateObjectIPA( C, NULL, sizeof(*iter), sgsstd_array_iter_iface );
		
		sgs_InitObjectPtr( &iter->ref, obj ); /* acquires ref */
		iter->size = hdr->size;
		iter->off = -1;
		
		return SGS_SUCCESS;
	}
	else if( type == SGS_VT_BOOL )
	{
		sgs_PushBool( C, !!hdr->size );
		return SGS_SUCCESS;
	}
	else if( type == SGS_VT_STRING )
	{
		sgs_Variable* var = SGSARR_PTR( obj->data );
		sgs_Variable* vend = var + hdr->size;
		
		sgs_PushString( C, "[" );
		while( var < vend )
		{
			sgs_PushVariable( C, *var );
			sgs_ToStringFast( C, -1 );
			var++;
			if( var < vend )
				sgs_PushString( C, "," );
		}
		sgs_PushString( C, "]" );
		sgs_StringConcat( C, hdr->size * 2 + 1 + !hdr->size );
		return SGS_SUCCESS;
	}
	else if( type == SGS_CONVOP_CLONE )
	{
		sgsstd_array_header_t* hdr2 = (sgsstd_array_header_t*)
			sgs_CreateObjectIPA( C, NULL, sizeof( sgsstd_array_header_t ), sgsstd_array_iface );
		memcpy( hdr2, hdr, sizeof( sgsstd_array_header_t ) );
		hdr2->data = sgs_Alloc_n( sgs_Variable, (size_t) hdr->mem );
		memcpy( hdr2->data, hdr->data, SGSARR_ALLOCSIZE( hdr->mem ) );
		{
			sgs_Variable* ptr = SGSARR_PTR( hdr );
			sgs_Variable* pend = ptr + hdr->size;
			while( ptr < pend )
				sgs_Acquire( C, ptr++ );
		}
		
		sgs_PushInterface( C, sgsstd_array_iface_gen );
		sgs_ObjSetMetaObj( C, sgs_GetObjectStruct( C, -2 ), sgs_GetObjectStruct( C, -1 ) );
		sgs_Pop( C, 1 );
		
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int sgsstd_array_serialize( SGS_CTX, sgs_VarObj* obj )
{
	sgs_Variable* pos, *posend;
	SGSARR_HDR_OI;
	pos = SGSARR_PTR( hdr );
	posend = pos + hdr->size;
	while( pos < posend )
	{
		sgs_Serialize( C, *pos++ );
	}
	sgs_SerializeObject( C, hdr->size, "array" );
	return SGS_SUCCESS;
}

static int sgsstd_array_destruct( SGS_CTX, sgs_VarObj* obj )
{
	SGSARR_HDR_OI;
	sgsstd_array_clear( C, hdr );
	sgs_Dealloc( hdr->data );
	return 0;
}

SGS_APIFUNC sgs_ObjInterface sgsstd_array_iface[1] =
{{
	"array",
	sgsstd_array_destruct, sgsstd_array_gcmark,
	sgsstd_array_getindex, sgsstd_array_setindex,
	sgsstd_array_convert, sgsstd_array_serialize, sgsstd_array_dump, NULL,
	NULL, NULL
}};

static int sgsstd_array( SGS_CTX )
{
	int i = 0, objcnt = sgs_StackSize( C );
	void* data = sgs_Malloc( C, SGSARR_ALLOCSIZE( objcnt ) );
	sgs_Variable *p, *pend;
	sgsstd_array_header_t* hdr = (sgsstd_array_header_t*)
		sgs_CreateObjectIPA( C, NULL, sizeof( sgsstd_array_header_t ), sgsstd_array_iface );
	hdr->size = objcnt;
	hdr->mem = objcnt;
	p = hdr->data = (sgs_Variable*) data;
	pend = p + objcnt;
	while( p < pend )
		sgs_GetStackItem( C, i++, p++ );
	
	sgs_PushInterface( C, sgsstd_array_iface_gen );
	sgs_ObjSetMetaObj( C, sgs_GetObjectStruct( C, -2 ), sgs_GetObjectStruct( C, -1 ) );
	sgs_Pop( C, 1 );
	
	return 1;
}


/*
	VHT containers
*/

typedef
struct _DictHdr
{
	sgs_VHTable ht;
}
DictHdr;

#define HTHDR DictHdr* dh = (DictHdr*) obj->data; sgs_VHTable* ht = &dh->ht;

static int sgsstd_vht_serialize( SGS_CTX, sgs_VarObj* obj, const char* initfn )
{
	sgs_VHTVar *pair, *pend;
	HTHDR;
	pair = ht->vars;
	pend = ht->vars + sgs_vht_size( ht );
	while( pair < pend )
	{
		sgs_Serialize( C, pair->key );
		sgs_Serialize( C, pair->val );
		pair++;
	}
	sgs_SerializeObject( C, sgs_vht_size( ht ) * 2, initfn );
	return SGS_SUCCESS;
}

static int sgsstd_vht_dump( SGS_CTX, sgs_VarObj* obj, int depth, const char* name )
{
	int ssz;
	char bfr[ 32 ];
	sgs_VHTVar *pair, *pend;
	HTHDR;
	pair = ht->vars;
	pend = ht->vars + sgs_vht_size( ht );
	ssz = sgs_StackSize( C );
	sprintf( bfr, "%s (%"PRId32")\n{", name, sgs_vht_size( ht ) );
	sgs_PushString( C, bfr );
	if( depth )
	{
		if( sgs_vht_size( ht ) )
		{
			while( pair < pend )
			{
				sgs_PushString( C, "\n" );
				if( pair->key.type == SGS_VT_STRING )
				{
					sgs_PushVariable( C, pair->key );
					sgs_ToPrintSafeString( C );
				}
				else
				{
					sgs_DumpVar( C, pair->key, depth );
				}
				sgs_PushString( C, " = " );
				sgs_DumpVar( C, pair->val, depth );
				pair++;
			}
			/* WP: stack limit */
			sgs_StringConcat( C, (sgs_StkIdx) ( pend - ht->vars ) * 4 );
			sgs_PadString( C );
		}
	}
	else
	{
		sgs_PushString( C, "\n..." );
		sgs_PadString( C );
	}
	sgs_PushString( C, "\n}" );
	sgs_StringConcat( C, sgs_StackSize( C ) - ssz );
	return SGS_SUCCESS;
}


/*
	DICT
*/

static DictHdr* mkdict( SGS_CTX, sgs_Variable* out )
{
	sgs_ObjInterface* iface = sgsstd_dict_iface;
	SGS_IF_DLL( ;,
	if( !iface )
		iface = sgsstd_dict_iface;
	)
	DictHdr* dh = (DictHdr*) sgs_CreateObjectIPA( C, out, sizeof( DictHdr ), iface );
	sgs_vht_init( &dh->ht, C, 4, 4 );
	return dh;
}


static int sgsstd_dict_destruct( SGS_CTX, sgs_VarObj* obj )
{
	HTHDR;
	sgs_vht_free( ht, C );
	return SGS_SUCCESS;
}

static int sgsstd_dict_gcmark( SGS_CTX, sgs_VarObj* obj )
{
	sgs_VHTVar *pair, *pend;
	HTHDR;
	pair = ht->vars;
	pend = ht->vars + sgs_vht_size( ht );
	while( pair < pend )
	{
		sgs_GCMark( C, &pair->key );
		sgs_GCMark( C, &pair->val );
		pair++;
	}
	return SGS_SUCCESS;
}

static int sgsstd_dict_dump( SGS_CTX, sgs_VarObj* obj, int depth )
{
	return sgsstd_vht_dump( C, obj, depth, "dict" );
}

/* ref'd in sgs_proc.c */ int sgsstd_dict_getindex( SGS_ARGS_GETINDEXFUNC )
{
	sgs_VHTVar* pair = NULL;
	HTHDR;
	
	if( C->object_arg && sgs_ItemType( C, 0 ) == SGS_VT_INT )
	{
		int32_t off = (int32_t) sgs_GetInt( C, 0 );
		if( off < 0 || off > sgs_vht_size( ht ) )
			return SGS_EBOUNDS;
		sgs_PushVariable( C, ht->vars[ off ].val );
		return SGS_SUCCESS;
	}
	
	if( sgs_ParseString( C, 0, NULL, NULL ) )
	{
		sgs_Variable key = sgs_StackItem( C, 0 );
		pair = sgs_vht_get( ht, &key );
		if( !pair )
			return SGS_ENOTFND;
		
		sgs_PushVariable( C, pair->val );
		return SGS_SUCCESS;
	}
	return SGS_EINVAL;
}

static int sgsstd_dict_setindex( SGS_ARGS_SETINDEXFUNC )
{
	HTHDR;
	if( sgs_ParseString( C, 0, NULL, NULL ) )
	{
		sgs_Variable key = sgs_StackItem( C, 0 ), val = sgs_StackItem( C, 1 );
		sgs_vht_set( ht, C, &key, &val );
		return SGS_SUCCESS;
	}
	return SGS_EINVAL;
}

/* iterator */

typedef struct sgsstd_dict_iter_s
{
	sgs_Variable ref;
	int32_t size;
	int32_t off;
}
sgsstd_dict_iter_t;

static int sgsstd_dict_iter_destruct( SGS_CTX, sgs_VarObj* obj )
{
	sgs_Release( C, &((sgsstd_dict_iter_t*) obj->data)->ref );
	return SGS_SUCCESS;
}

static int sgsstd_dict_iter_getnext( SGS_CTX, sgs_VarObj* obj, int flags )
{
	sgsstd_dict_iter_t* iter = (sgsstd_dict_iter_t*) obj->data;
	sgs_VHTable* ht = (sgs_VHTable*) iter->ref.data.O->data;
	if( iter->size != sgs_vht_size( ht ) )
		return SGS_EINVAL;
	
	if( !flags )
	{
		iter->off++;
		return iter->off < iter->size;
	}
	else
	{
		if( flags & SGS_GETNEXT_KEY )
			sgs_PushVariable( C, ht->vars[ iter->off ].key );
		if( flags & SGS_GETNEXT_VALUE )
			sgs_PushVariable( C, ht->vars[ iter->off ].val );
		return SGS_SUCCESS;
	}
}

static int sgsstd_dict_iter_gcmark( SGS_CTX, sgs_VarObj* obj )
{
	sgsstd_dict_iter_t* iter = (sgsstd_dict_iter_t*) obj->data;
	sgs_GCMark( C, &iter->ref );
	return SGS_SUCCESS;
}

SGS_APIFUNC sgs_ObjInterface sgsstd_dict_iter_iface[1] =
{{
	"dict_iterator",
	sgsstd_dict_iter_destruct, sgsstd_dict_iter_gcmark,
	NULL, NULL,
	NULL, NULL, NULL, sgsstd_dict_iter_getnext,
	NULL, NULL
}};


static int sgsstd_dict_convert( SGS_CTX, sgs_VarObj* obj, int type )
{
	HTHDR;
	if( type == SGS_CONVOP_TOITER )
	{
		sgsstd_dict_iter_t* iter = (sgsstd_dict_iter_t*)
			sgs_CreateObjectIPA( C, NULL, sizeof(*iter), sgsstd_dict_iter_iface );
		
		sgs_InitObjectPtr( &iter->ref, obj ); /* acquires ref */
		iter->size = sgs_vht_size( ht );
		iter->off = -1;
		
		return SGS_SUCCESS;
	}
	else if( type == SGS_VT_BOOL )
	{
		sgs_PushBool( C, sgs_vht_size( ht ) != 0 );
		return SGS_SUCCESS;
	}
	else if( type == SGS_VT_STRING )
	{
		sgs_VHTVar *pair = ht->vars, *pend = ht->vars + sgs_vht_size( ht );
		int cnt = 0;
		sgs_PushString( C, "{" );
		while( pair < pend )
		{
			if( cnt )
				sgs_PushString( C, "," );
			sgs_PushVariable( C, pair->key );
			sgs_PushString( C, "=" );
			sgs_PushVariable( C, pair->val );
			sgs_ToStringFast( C, -1 );
			cnt++;
			pair++;
		}
		sgs_PushString( C, "}" );
		sgs_StringConcat( C, cnt * 4 + 1 + !cnt );
		return SGS_SUCCESS;
	}
	else if( type == SGS_CONVOP_CLONE )
	{
		int i, htsize = sgs_vht_size( ht );
		DictHdr* ndh = mkdict( C, NULL );
		for( i = 0; i < htsize; ++i )
		{
			sgs_vht_set( &ndh->ht, C, &ht->vars[ i ].key, &ht->vars[ i ].val );
		}
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int sgsstd_dict_serialize( SGS_CTX, sgs_VarObj* obj )
{
	return sgsstd_vht_serialize( C, obj, "dict" );
}

SGS_APIFUNC sgs_ObjInterface sgsstd_dict_iface[1] =
{{
	"dict",
	sgsstd_dict_destruct, sgsstd_dict_gcmark,
	sgsstd_dict_getindex, sgsstd_dict_setindex,
	sgsstd_dict_convert, sgsstd_dict_serialize, sgsstd_dict_dump, NULL,
	NULL, NULL
}};

static int sgsstd_dict( SGS_CTX )
{
	DictHdr* dh;
	sgs_VHTable* ht;
	int i, objcnt = sgs_StackSize( C );
	
	SGSFN( "dict" );
	
	if( objcnt % 2 != 0 )
		STDLIB_WARN( "function expects 0 or an even number of arguments" )
	
	dh = mkdict( C, NULL );
	ht = &dh->ht;
	
	for( i = 0; i < objcnt; i += 2 )
	{
		sgs_Variable val = sgs_StackItem( C, i + 1 );
		sgs_ToString( C, i );
		sgs_vht_set( ht, C, (C->stack_off+i), &val );
	}
	
	return 1;
}


/* MAP */

static DictHdr* mkmap( SGS_CTX, sgs_Variable* out )
{
	DictHdr* dh = (DictHdr*) sgs_CreateObjectIPA( C, out, sizeof( DictHdr ), sgsstd_map_iface );
	sgs_vht_init( &dh->ht, C, 4, 4 );
	return dh;
}

#define sgsstd_map_destruct sgsstd_dict_destruct
#define sgsstd_map_gcmark sgsstd_dict_gcmark

static int sgsstd_map_dump( SGS_CTX, sgs_VarObj* obj, int depth )
{
	return sgsstd_vht_dump( C, obj, depth, "map" );
}

static int sgsstd_map_serialize( SGS_CTX, sgs_VarObj* obj )
{
	return sgsstd_vht_serialize( C, obj, "map" );
}

static int sgsstd_map_convert( SGS_CTX, sgs_VarObj* obj, int type )
{
	HTHDR;
	if( type == SGS_CONVOP_TOITER )
	{
		sgsstd_dict_iter_t* iter = (sgsstd_dict_iter_t*)
			sgs_CreateObjectIPA( C, NULL, sizeof(*iter), sgsstd_dict_iter_iface );
		
		sgs_InitObjectPtr( &iter->ref, obj ); /* acquires ref */
		iter->size = sgs_vht_size( ht );
		iter->off = -1;
		
		return SGS_SUCCESS;
	}
	else if( type == SGS_VT_BOOL )
	{
		sgs_PushBool( C, sgs_vht_size( ht ) != 0 );
		return SGS_SUCCESS;
	}
	else if( type == SGS_VT_STRING )
	{
		sgs_VHTVar *pair = ht->vars, *pend = ht->vars + sgs_vht_size( ht );
		int cnt = 0;
		sgs_PushString( C, "[map]{" );
		while( pair < pend )
		{
			if( cnt )
				sgs_PushString( C, "," );
			sgs_PushVariable( C, pair->key );
			sgs_ToStringFast( C, -1 );
			sgs_PushString( C, "=" );
			sgs_PushVariable( C, pair->val );
			sgs_ToStringFast( C, -1 );
			cnt++;
			pair++;
		}
		sgs_PushString( C, "}" );
		sgs_StringConcat( C, cnt * 4 + 1 + !cnt );
		return SGS_SUCCESS;
	}
	else if( type == SGS_CONVOP_CLONE )
	{
		int i, htsize = sgs_vht_size( ht );
		DictHdr* ndh = mkmap( C, NULL );
		for( i = 0; i < htsize; ++i )
		{
			sgs_vht_set( &ndh->ht, C, &ht->vars[ i ].key, &ht->vars[ i ].val );
		}
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

/* copy in sgs_proc.c */ static int sgsstd_map_getindex( SGS_ARGS_GETINDEXFUNC )
{
	sgs_Variable key;
	sgs_VHTVar* pair = NULL;
	HTHDR;
	
	key = sgs_StackItem( C, 0 );
	pair = sgs_vht_get( ht, &key );
	if( !pair )
		return SGS_ENOTFND;
	
	sgs_PushVariable( C, pair->val );
	return SGS_SUCCESS;
}

static int sgsstd_map_setindex( SGS_ARGS_SETINDEXFUNC )
{
	HTHDR;
	sgs_Variable key = sgs_StackItem( C, 0 ), val = sgs_StackItem( C, 1 );
	sgs_vht_set( ht, C, &key, &val );
	return SGS_SUCCESS;
}

SGS_APIFUNC sgs_ObjInterface sgsstd_map_iface[1] =
{{
	"map",
	sgsstd_map_destruct, sgsstd_map_gcmark,
	sgsstd_map_getindex, sgsstd_map_setindex,
	sgsstd_map_convert, sgsstd_map_serialize, sgsstd_map_dump, NULL,
	NULL, NULL
}};

static int sgsstd_map( SGS_CTX )
{
	DictHdr* dh;
	sgs_VHTable* ht;
	int i, objcnt = sgs_StackSize( C );
	
	SGSFN( "map" );
	
	if( objcnt % 2 != 0 )
		STDLIB_WARN( "function expects 0 or an even number of arguments" )
	
	dh = mkmap( C, NULL );
	ht = &dh->ht;
	
	for( i = 0; i < objcnt; i += 2 )
	{
		sgs_vht_set( ht, C, C->stack_off+i, C->stack_off+i+1 );
	}
	
	return 1;
}


static int sgsstd_class( SGS_CTX )
{
	sgs_VarObj *obj1, *obj2;
	SGSFN( "class" );
	if( !sgs_LoadArgs( C, "!x!x", sgs_ArgCheck_Object, &obj1, sgs_ArgCheck_Object, &obj2 ) )
		return 0;
	sgs_ObjSetMetaObj( C, obj1, obj2 );
	sgs_ObjSetMetaMethodEnable( obj1, SGS_TRUE );
	sgs_SetStackSize( C, 1 );
	return 1;
}


/*
	closure memory layout:
	- sgs_Variable: function
	- int32: closure count
	- sgs_Closure* x ^^: closures
*/

static int sgsstd_closure_destruct( SGS_CTX, sgs_VarObj* obj )
{
	uint8_t* cl = (uint8_t*) obj->data;
	int32_t i, cc = *(int32_t*) (void*) SGS_ASSUME_ALIGNED(cl+sizeof(sgs_Variable),sizeof(void*));
	sgs_Closure** cls = (sgs_Closure**) (void*) SGS_ASSUME_ALIGNED(cl+sizeof(sgs_Variable)+sizeof(int32_t),sizeof(void*));
	
	sgs_Release( C, (sgs_Variable*) (void*) SGS_ASSUME_ALIGNED( cl, sizeof(void*) ) );
	
	for( i = 0; i < cc; ++i )
	{
		if( --cls[ i ]->refcount < 1 )
		{
			sgs_Release( C, &cls[ i ]->var );
			sgs_Dealloc( cls[i] );
		}
	}
	
	return SGS_SUCCESS;
}

static int sgsstd_closure_getindex( SGS_ARGS_GETINDEXFUNC )
{
	char* str;
	if( sgs_ParseString( C, 0, &str, NULL ) )
	{
		if( !strcmp( str, "call" ) )
		{
			sgs_PushCFunc( C, sgs_specfn_call );
			return SGS_SUCCESS;
		}
		if( !strcmp( str, "apply" ) )
		{
			sgs_PushCFunc( C, sgs_specfn_apply );
			return SGS_SUCCESS;
		}
	}
	return SGS_ENOTFND;
}

static int sgsstd_closure_call( SGS_CTX, sgs_VarObj* obj )
{
	int rvc = 0, ismethod = sgs_Method( C );
	uint8_t* cl = (uint8_t*) obj->data;
	int32_t cc = *(int32_t*) (void*) SGS_ASSUME_ALIGNED(cl+sizeof(sgs_Variable),sizeof(void*));
	sgs_Closure** cls = (sgs_Closure**) (void*) SGS_ASSUME_ALIGNED(cl+sizeof(sgs_Variable)+sizeof(int32_t),sizeof(void*));
	
	sgsVM_PushClosures( C, cls, cc );
	sgsVM_VarCall( C, (sgs_Variable*) (void*) SGS_ASSUME_ALIGNED( cl, sizeof(void*) ),
		C->sf_last->argcount, cc, &rvc, ismethod );
	return rvc;
}

static int sgsstd_closure_gcmark( SGS_CTX, sgs_VarObj* obj )
{
	uint8_t* cl = (uint8_t*) obj->data;
	int32_t i, cc = *(int32_t*) (void*) SGS_ASSUME_ALIGNED(cl+sizeof(sgs_Variable),sizeof(void*));
	sgs_Closure** cls = (sgs_Closure**) (void*) SGS_ASSUME_ALIGNED(cl+sizeof(sgs_Variable)+sizeof(int32_t),sizeof(void*));
	
	sgs_GCMark( C, (sgs_Variable*) (void*) SGS_ASSUME_ALIGNED( cl, sizeof(void*) ) );
	
	for( i = 0; i < cc; ++i )
	{
		sgs_GCMark( C, &cls[ i ]->var );
	}
	
	return SGS_SUCCESS;
}

static int sgsstd_closure_dump( SGS_CTX, sgs_VarObj* obj, int depth )
{
	uint8_t* cl = (uint8_t*) obj->data;
	int32_t i, ssz, cc = *(int32_t*) (void*) SGS_ASSUME_ALIGNED(cl+sizeof(sgs_Variable),sizeof(void*));
	sgs_Closure** cls = (sgs_Closure**) (void*) SGS_ASSUME_ALIGNED(cl+sizeof(sgs_Variable)+sizeof(int32_t),sizeof(void*));
	
	sgs_PushString( C, "closure\n{" );
	
	ssz = sgs_StackSize( C );
	sgs_PushString( C, "\nfunc: " );
	sgs_DumpVar( C, *(sgs_Variable*) (void*) SGS_ASSUME_ALIGNED( cl, sizeof(void*) ), depth ); /* function */
	for( i = 0; i < cc; ++i )
	{
		char intro[ 64 ];
		sprintf( intro, "\n#%"PRId32" (rc=%"PRId32"): ", i, cls[ i ]->refcount );
		sgs_PushString( C, intro );
		sgs_DumpVar( C, cls[ i ]->var, depth );
	}
	sgs_StringConcat( C, sgs_StackSize( C ) - ssz );
	sgs_PadString( C );
	
	sgs_PushString( C, "\n}" );
	sgs_StringConcat( C, 3 );
	return SGS_SUCCESS;
}

SGS_APIFUNC sgs_ObjInterface sgsstd_closure_iface[1] =
{{
	"closure",
	sgsstd_closure_destruct, sgsstd_closure_gcmark,
	sgsstd_closure_getindex, NULL,
	NULL, NULL, sgsstd_closure_dump, NULL,
	sgsstd_closure_call, NULL
}};

void sgsSTD_MakeClosure( SGS_CTX, sgs_Variable* func, uint32_t clc )
{
	/* WP: range not affected by conversion */
	uint32_t i, clsz = (uint32_t) sizeof(sgs_Closure*) * clc;
	uint32_t memsz = clsz + (uint32_t) ( sizeof(sgs_Variable) + sizeof(int32_t) );
	uint8_t* cl = (uint8_t*) sgs_CreateObjectIPA( C, NULL, memsz, sgsstd_closure_iface );
	
	memcpy( cl, func, sizeof(sgs_Variable) );
	sgs_Acquire( C, func );
	
	memcpy( cl + sizeof(sgs_Variable), &clc, sizeof(clc) );
	
	memcpy( cl + sizeof(sgs_Variable) + sizeof(clc), C->clstk_top - clc, clsz );
	for( i = 0; i < clc; ++i )
		(*(C->clstk_top - clc + i))->refcount++;
}


/* UTILITIES */

static int sgsstd_array_filter( SGS_CTX )
{
	SGSBOOL cset = 0, use;
	sgs_SizeVal asz, off = 0, nasz = 0;
	sgs_Variable v_func = sgs_MakeNull();
	void *data;
	sgsstd_array_header_t* nadata;
	
	SGSFN( "array_filter" );
	if( !sgs_LoadArgs( C, "a|p<v", &asz, &cset, &v_func ) )
		return 0;
	
	sgs_CreateArray( C, NULL, 0 );
	data = sgs_GetObjectData( C, 0 );
	nadata = (sgsstd_array_header_t*) sgs_GetObjectData( C, -1 );
	
	{
		SGSARR_HDR;
		
		while( off < asz )
		{
			sgs_PushVariable( C, SGSARR_PTR( hdr )[ off ] );
			if( cset )
			{
				sgs_PushInt( C, off );
				sgs_Call( C, v_func, 2, 1 );
			}
			use = sgs_GetBool( C, -1 );
			sgs_Pop( C, 1 );
			sgs_PushVariable( C, SGSARR_PTR( hdr )[ off ] );
			if( use )
			{
				sgsstd_array_insert( C, nadata, nasz, sgs_StackSize( C ) - 1 );
				nasz++;
			}
			sgs_Pop( C, 1 );
			
			off++;
		}
	}
	
	return 1;
}

static int sgsstd_array_process( SGS_CTX )
{
	sgs_SizeVal asz, off = 0;
	sgs_Variable v_func;
	void *data;
	
	SGSFN( "array_process" );
	if( !sgs_LoadArgs( C, "a?p<v", &asz, &v_func ) )
		return 0;
	
	data = sgs_GetObjectData( C, 0 );
	{
		SGSARR_HDR;
		
		while( off < asz )
		{
			sgs_PushVariable( C, SGSARR_PTR( hdr )[ off ] );
			sgs_PushInt( C, off );
			sgs_Call( C, v_func, 2, 1 );
			sgs_SetIndex( C, sgs_StackItem( C, 0 ), sgs_MakeInt( off ), sgs_StackItem( C, -1 ), SGS_FALSE );
			sgs_Pop( C, 1 );
			off++;
		}
	}
	
	sgs_SetStackSize( C, 1 );
	return 1;
}

static int _sgsstd_ht_filter( SGS_CTX, int usemap )
{
	SGSBOOL cset = 0, use;
	sgs_Variable v_func = sgs_MakeNull(), v_dest;
	
	if( !sgs_LoadArgs( C, usemap ? "?h|p<v" : "?t|p<v", &cset, &v_func ) )
		return 0;
	
	if( usemap )
		sgs_CreateMap( C, NULL, 0 );
	else
		sgs_CreateDict( C, NULL, 0 );
	v_dest = sgs_StackItem( C, -1 );
	sgs_PushIterator( C, sgs_StackItem( C, 0 ) );
	while( sgs_IterAdvance( C, sgs_StackItem( C, -1 ) ) > 0 )
	{
		sgs_IterPushData( C, sgs_StackItem( C, -1 ), 1, 1 );
		if( cset )
		{
			sgs_PushItem( C, -1 );
			sgs_PushItem( C, -3 );
			sgs_Call( C, v_func, 2, 1 );
		}
		use = sgs_GetBool( C, -1 );
		if( cset )
			sgs_Pop( C, 1 );
		if( use )
		{
			/* src-dict, ... dest-dict, iterator, key, value */
			sgs_SetIndex( C, v_dest, sgs_StackItem( C, -2 ), sgs_StackItem( C, -1 ), SGS_FALSE );
			sgs_Pop( C, 1 );
		}
		sgs_Pop( C, use ? 1 : 2 );
	}
	sgs_Pop( C, 1 );
	return 1;
}

static int sgsstd_dict_filter( SGS_CTX )
{
	SGSFN( "dict_filter" );
	return _sgsstd_ht_filter( C, 0 );
}

static int sgsstd_map_filter( SGS_CTX )
{
	SGSFN( "map_filter" );
	return _sgsstd_ht_filter( C, 1 );
}

static int sgsstd_map_process( SGS_CTX )
{
	sgs_Variable v_func;
	SGSFN( "map_process" );
	if( !sgs_LoadArgs( C, "?v?p<v", &v_func ) )
		return 0;
	
	sgs_PushIterator( C, sgs_StackItem( C, 0 ) );
	while( sgs_IterAdvance( C, sgs_StackItem( C, -1 ) ) > 0 )
	{
		sgs_IterPushData( C, sgs_StackItem( C, -1 ), 1, 1 );
		sgs_PushItem( C, -2 );
		sgs_Call( C, v_func, 2, 1 );
		/* src-dict, callable, ... iterator, key, proc.val. */
		sgs_SetIndex( C, sgs_StackItem( C, 0 ), sgs_StackItem( C, -2 ), sgs_StackItem( C, -1 ), SGS_FALSE );
		sgs_Pop( C, 2 );
	}
	
	sgs_SetStackSize( C, 1 );
	return 1;
}


static int sgsstd_dict_size( SGS_CTX )
{
	sgs_SizeVal size;
	
	SGSFN( "dict_size" );
	if( !sgs_LoadArgs( C, "t.", &size ) )
		return 0;
	
	sgs_PushInt( C, size );
	return 1;
}

static int sgsstd_map_size( SGS_CTX )
{
	sgs_SizeVal size;
	
	SGSFN( "map_size" );
	if( !sgs_LoadArgs( C, "h.", &size ) )
		return 0;
	
	sgs_PushInt( C, size );
	return 1;
}

static int sgsstd_isset( SGS_CTX )
{
	int ret;
	int32_t oldapilev;
	SGSFN( "isset" );
	if( !sgs_LoadArgs( C, "?v?v." ) )
		return 0;
	
	oldapilev = sgs_Cntl( C, SGS_CNTL_APILEV, SGS_ERROR );
	ret = sgs_PushIndex( C, sgs_StackItem( C, 0 ), sgs_StackItem( C, 1 ), SGS_FALSE );
	sgs_Cntl( C, SGS_CNTL_APILEV, oldapilev );
	sgs_PushBool( C, ret );
	return 1;
}

static int sgsstd_unset( SGS_CTX )
{
	DictHdr* dh;
	
	SGSFN( "unset" );
	
	if( !sgs_IsObject( C, 0, sgsstd_dict_iface ) &&
		!sgs_IsObject( C, 0, sgsstd_map_iface ) )
		return sgs_ArgErrorExt( C, 0, 0, "dict / map", "" );
	
	dh = (DictHdr*) sgs_GetObjectData( C, 0 );
	
	if( sgs_IsObject( C, 0, sgsstd_dict_iface ) )
	{
		if( !sgs_LoadArgs( C, ">?m." ) )
			return 0;
	}
	else
	{
		if( !sgs_LoadArgs( C, ">?v." ) )
			return 0;
	}
	sgs_vht_unset( &dh->ht, C, (C->stack_off+1) );
	
	return 0;
}

static int sgsstd_clone( SGS_CTX )
{
	SGSFN( "clone" );
	
	if( !sgs_LoadArgs( C, "?v." ) )
		return 0;
	
	sgs_CloneItem( C, sgs_StackItem( C, 0 ) );
	return 1;
}


static int _foreach_lister( SGS_CTX, int vnk )
{
	if( !sgs_LoadArgs( C, ">." ) )
		return 0;
	
	if( sgs_PushIterator( C, sgs_StackItem( C, 0 ) ) == SGS_FALSE )
		return sgs_ArgErrorExt( C, 0, 0, "iterable", "" );
	
	sgs_CreateArray( C, NULL, 0 );
	/* arg1, arg1 iter, output */
	while( sgs_IterAdvance( C, sgs_StackItem( C, 1 ) ) > 0 )
	{
		sgs_IterPushData( C, sgs_StackItem( C, 1 ), !vnk, vnk );
		sgs_ArrayPush( C, sgs_StackItem( C, 2 ), 1 );
	}
	return 1;
}
static int sgsstd_get_keys( SGS_CTX )
{
	SGSFN( "get_keys" );
	return _foreach_lister( C, 0 );
}
static int sgsstd_get_values( SGS_CTX )
{
	SGSFN( "get_values" );
	return _foreach_lister( C, 1 );
}

static int sgsstd_get_concat( SGS_CTX )
{
	int i, ssz = sgs_StackSize( C );
	SGSFN( "get_concat" );
	if( ssz < 2 )
	{
		return sgs_Msg( C, SGS_WARNING,
			"function expects at least 2 arguments, got %d",
			sgs_StackSize( C ) );
	}
	
	sgs_CreateArray( C, NULL, 0 );
	for( i = 0; i < ssz; ++i )
	{
		if( sgs_PushIterator( C, sgs_StackItem( C, i ) ) == SGS_FALSE )
			return sgs_ArgErrorExt( C, i, 0, "iterable", "" );
		while( sgs_IterAdvance( C, sgs_StackItem( C, -1 ) ) > 0 )
		{
			/* ..., output, arg2 iter */
			sgs_IterPushData( C, sgs_StackItem( C, -1 ), SGS_FALSE, SGS_TRUE );
			sgs_ArrayPush( C, sgs_StackItem( C, -3 ), 1 );
		}
		sgs_Pop( C, 1 );
	}
	return 1;
}

static int sgsstd__get_merged__common( SGS_CTX, sgs_SizeVal ssz )
{
	sgs_SizeVal i;
	sgs_Variable v_dest = sgs_StackItem( C, -1 );
	for( i = 0; i < ssz; ++i )
	{
		if( sgs_PushIterator( C, sgs_StackItem( C, i ) ) == SGS_FALSE )
			return sgs_ArgErrorExt( C, i, 0, "iterable", "" );
		while( sgs_IterAdvance( C, sgs_StackItem( C, -1 ) ) > 0 )
		{
			/* ..., output, arg2 iter */
			sgs_IterPushData( C, sgs_StackItem( C, -1 ), SGS_TRUE, SGS_TRUE );
			sgs_SetIndex( C, v_dest, sgs_StackItem( C, -2 ), sgs_StackItem( C, -1 ), SGS_FALSE );
			sgs_Pop( C, 2 );
		}
		sgs_Pop( C, 1 );
	}
	return 1;
}

static int sgsstd_get_merged( SGS_CTX )
{
	sgs_SizeVal ssz = sgs_StackSize( C );
	SGSFN( "get_merged" );
	if( ssz < 2 )
	{
		return sgs_Msg( C, SGS_WARNING,
			"function expects at least 2 arguments, got %d",
			sgs_StackSize( C ) );
	}
	
	sgs_CreateDict( C, NULL, 0 );
	return sgsstd__get_merged__common( C, ssz );
}

static int sgsstd_get_merged_map( SGS_CTX )
{
	sgs_SizeVal ssz = sgs_StackSize( C );
	SGSFN( "get_merged_map" );
	if( ssz < 2 )
	{
		return sgs_Msg( C, SGS_WARNING,
			"function expects at least 2 arguments, got %d",
			sgs_StackSize( C ) );
	}
	
	sgs_CreateMap( C, NULL, 0 );
	return sgsstd__get_merged__common( C, ssz );
}

static int sgsstd_get_iterator( SGS_CTX )
{
	SGSFN( "get_iterator" );
	if( !sgs_LoadArgs( C, "?!v" ) )
		return 0;
	sgs_PushIterator( C, sgs_StackItem( C, 0 ) );
	return 1;
}

static int sgsstd_iter_advance( SGS_CTX )
{
	int ret;
	SGSFN( "iter_advance" );
	if( !sgs_LoadArgs( C, "?!v" ) )
		return 0;
	ret = sgs_IterAdvance( C, sgs_StackItem( C, 0 ) );
	if( SGS_FAILED( ret ) )
		STDLIB_WARN( "failed to advance iterator" )
	sgs_PushBool( C, ret != 0 );
	return 1;
}

static int sgsstd_iter_getdata( SGS_CTX )
{
	sgs_Bool pushkey = 0, pushval = 1;
	SGSFN( "iter_getdata" );
	if( !sgs_LoadArgs( C, "?!v|bb", &pushkey, &pushval ) )
		return 0;
	if( pushkey + pushval == 0 )
		STDLIB_WARN( "no data requested from iterator" );
	sgs_IterPushData( C, sgs_StackItem( C, 0 ), pushkey, pushval );
	return pushkey + pushval;
}


static int sgsstd_tobool( SGS_CTX )
{
	SGSFN( "tobool" );
	return sgs_PushBool( C, sgs_GetBool( C, 0 ) );
}

static int sgsstd_toint( SGS_CTX )
{
	SGSFN( "toint" );
	return sgs_PushInt( C, sgs_GetInt( C, 0 ) );
}

static int sgsstd_toreal( SGS_CTX )
{
	SGSFN( "toreal" );
	return sgs_PushReal( C, sgs_GetReal( C, 0 ) );
}

static int sgsstd_tostring( SGS_CTX )
{
	SGSFN( "tostring" );
	sgs_SetStackSize( C, 1 );
	sgs_ToString( C, 0 );
	return 1;
}

static int sgsstd_toptr( SGS_CTX )
{
	SGSFN( "toptr" );
	return sgs_PushPtr( C, sgs_GetPtr( C, 0 ) );
}


static int sgsstd_parseint( SGS_CTX )
{
	sgs_Int i;
	SGSFN( "parseint" );
	if( sgs_ParseInt( C, 0, &i ) )
		sgs_PushInt( C, i );
	else
		sgs_PushNull( C );
	return 1;
}

static int sgsstd_parsereal( SGS_CTX )
{
	sgs_Real r;
	SGSFN( "parsereal" );
	if( sgs_ParseReal( C, 0, &r ) )
		sgs_PushReal( C, r );
	else
		sgs_PushNull( C );
	return 1;
}


static int sgsstd_typeof( SGS_CTX )
{
	SGSFN( "typeof" );
	if( !sgsstd_expectnum( C, 1 ) )
		return 0;
	sgs_TypeOf( C, sgs_StackItem( C, 0 ) );
	return 1;
}

static int sgsstd_typeid( SGS_CTX )
{
	SGSFN( "typeid" );
	if( !sgsstd_expectnum( C, 1 ) )
		return 0;
	sgs_PushInt( C, sgs_ItemType( C, 0 ) );
	return 1;
}

static int sgsstd_typeptr( SGS_CTX )
{
	SGSFN( "typeptr" );
	if( !sgs_LoadArgs( C, ">." ) )
		return 0;
	if( sgs_ItemType( C, 0 ) == SGS_VT_OBJECT )
		sgs_PushPtr( C, sgs_GetObjectIface( C, 0 ) );
	else
		sgs_PushPtr( C, NULL );
	return 1;
}

static int sgsstd_typeptr_by_name( SGS_CTX )
{
	char* typenm;
	SGSFN( "typeptr_by_name" );
	if( !sgs_LoadArgs( C, "s", &typenm ) )
		return 0;
	sgs_PushPtr( C, sgs_FindType( C, typenm ) );
	return 1;
}

static int sgsstd_is_numeric( SGS_CTX )
{
	int res;
	uint32_t ty;
	
	SGSFN( "is_numeric" );
	if( !sgsstd_expectnum( C, 1 ) )
		return 0;
	
	ty = sgs_ItemType( C, 0 );
	if( ty == SGS_VT_NULL || ty == SGS_VT_FUNC ||
		ty == SGS_VT_CFUNC || ty == SGS_VT_OBJECT )
		res = SGS_FALSE;
	else
		res = ty != SGS_VT_STRING || sgs_IsNumericString(
			sgs_GetStringPtr( C, 0 ), sgs_GetStringSize( C, 0 ) );
	
	sgs_PushBool( C, res );
	return 1;
}

static int sgsstd_is_callable( SGS_CTX )
{
	SGSFN( "is_callable" );
	if( !sgsstd_expectnum( C, 1 ) )
		return 0;
	
	sgs_PushBool( C, sgs_IsCallable( C, 0 ) );
	return 1;
}

static int sgsstd_is_array( SGS_CTX )
{
	SGSFN( "is_array" );
	sgs_PushBool( C, sgs_IsObject( C, 0, sgsstd_array_iface ) );
	return 1;
}

static int sgsstd_is_dict( SGS_CTX )
{
	SGSFN( "is_dict" );
	sgs_PushBool( C, sgs_IsObject( C, 0, sgsstd_dict_iface ) );
	return 1;
}

static int sgsstd_is_map( SGS_CTX )
{
	SGSFN( "is_map" );
	sgs_PushBool( C, sgs_IsObject( C, 0, sgsstd_map_iface ) );
	return 1;
}


/* I/O */

static int sgsstd_print( SGS_CTX )
{
	sgs_StkIdx i, ssz;
	SGSBASEFN( "print" );
	ssz = sgs_StackSize( C );
	for( i = 0; i < ssz; ++i )
	{
		sgs_SizeVal size;
		char* buf = sgs_ToStringBuf( C, i, &size );
		/* WP: string limit */
		sgs_Write( C, buf, (size_t) size );
	}
	return 0;
}

static int sgsstd_println( SGS_CTX )
{
	SGSFN( "println" );
	sgsstd_print( C );
	sgs_Write( C, "\n", 1 );
	return 0;
}

static int sgsstd_printlns( SGS_CTX )
{
	sgs_StkIdx i, ssz;
	ssz = sgs_StackSize( C );
	
	SGSFN( "printlns" );
	
	for( i = 0; i < ssz; ++i )
	{
		sgs_SizeVal size;
		char* buf = sgs_ToStringBuf( C, i, &size );
		/* WP: string limit */
		sgs_Write( C, buf, (size_t) size );
		sgs_Write( C, "\n", 1 );
	}
	return 0;
}

static int sgsstd_errprint( SGS_CTX )
{
	sgs_StkIdx i, ssz;
	SGSBASEFN( "errprint" );
	ssz = sgs_StackSize( C );
	for( i = 0; i < ssz; ++i )
	{
		sgs_SizeVal size;
		char* buf = sgs_ToStringBuf( C, i, &size );
		/* WP: string limit */
		sgs_ErrWrite( C, buf, (size_t) size );
	}
	return 0;
}

static int sgsstd_errprintln( SGS_CTX )
{
	SGSFN( "errprintln" );
	sgsstd_errprint( C );
	sgs_ErrWrite( C, "\n", 1 );
	return 0;
}

static int sgsstd_errprintlns( SGS_CTX )
{
	sgs_StkIdx i, ssz;
	ssz = sgs_StackSize( C );
	
	SGSFN( "errprintlns" );
	
	for( i = 0; i < ssz; ++i )
	{
		sgs_SizeVal size;
		char* buf = sgs_ToStringBuf( C, i, &size );
		/* WP: string limit */
		sgs_ErrWrite( C, buf, (size_t) size );
		sgs_ErrWrite( C, "\n", 1 );
	}
	return 0;
}

static int sgsstd_printvar( SGS_CTX )
{
	sgs_StkIdx i, ssz;
	ssz = sgs_StackSize( C );
	
	SGSFN( "printvar" );
	
	for( i = 0; i < ssz; ++i )
	{
		sgs_DumpVar( C, sgs_StackItem( C, i ), 5 );
		{
			sgs_SizeVal bsz;
			char* buf = sgs_ToStringBuf( C, -1, &bsz );
			/* WP: string limit */
			sgs_Write( C, buf, (size_t) bsz );
			sgs_Write( C, "\n", 1 );
		}
		sgs_Pop( C, 1 );
	}
	return 0;
}
static int sgsstd_printvar_ext( SGS_CTX )
{
	sgs_Int depth = 5;
	
	SGSFN( "printvar_ext" );
	
	if( !sgs_LoadArgs( C, ">|i.", &depth ) )
		return 0;
	
	sgs_DumpVar( C, sgs_StackItem( C, 0 ), (int) depth );
	{
		sgs_SizeVal bsz;
		char* buf = sgs_ToStringBuf( C, -1, &bsz );
		/* WP: string limit */
		sgs_Write( C, buf, (size_t) bsz );
		sgs_Write( C, "\n", 1 );
	}
	return 0;
}

static int sgsstd_read_stdin( SGS_CTX )
{
	sgs_MemBuf B;
	char bfr[ 1024 ];
	int all = 0;
	
	SGSFN( "read_stdin" );
	
	if( !sgs_LoadArgs( C, "|b", &all ) )
		return 0;
	
	B = sgs_membuf_create();
	while( fgets( bfr, 1024, stdin ) )
	{
		size_t len = strlen( bfr );
		sgs_membuf_appbuf( &B, C, bfr, len );
		if( len && !all && bfr[ len - 1 ] == '\n' )
		{
			B.size--;
			break;
		}
	}
	if( B.size > 0x7fffffff )
	{
		sgs_membuf_destroy( &B, C );
		STDLIB_WARN( "read more bytes than allowed to store" );
	}
	/* WP: error condition */
	sgs_PushStringBuf( C, B.ptr, (sgs_SizeVal) B.size );
	sgs_membuf_destroy( &B, C );
	return 1;
}


/* OS */

static int sgsstd_ftime( SGS_CTX )
{
	SGSFN( "ftime" );
	sgs_PushReal( C, sgs_GetTime() );
	return 1;
}


/* utils */

static int sgsstd_rand( SGS_CTX )
{
	SGSFN( "rand" );
	sgs_PushInt( C, rand() );
	return 1;
}

static int sgsstd_randf( SGS_CTX )
{
	SGSFN( "randf" );
	sgs_PushReal( C, (double) rand() / (double) RAND_MAX );
	return 1;
}

static int sgsstd_srand( SGS_CTX )
{
	uint32_t s;
	SGSFN( "srand" );
	if( !sgs_LoadArgs( C, "+l", &s ) )
		return 0;
	srand( s );
	return 0;
}

static int sgsstd_hash_fnv( SGS_CTX )
{
	uint8_t* buf;
	sgs_SizeVal i, bufsize;
	uint32_t hv = 2166136261u;
	sgs_Bool as_hexstr = 0;
	SGSFN( "hash_fnv" );
	if( !sgs_LoadArgs( C, "m|b", &buf, &bufsize, &as_hexstr ) )
		return 0;
	for( i = 0; i < bufsize; ++i )
	{
		hv ^= buf[ i ];
		hv *= 16777619u;
	}
	if( as_hexstr )
	{
		char hexstr[ 9 ] = {0};
		sprintf( hexstr, "%08x", hv );
		sgs_PushStringBuf( C, hexstr, 8 );
	}
	else
		sgs_PushInt( C, hv );
	return 1;
}

static int sgsstd_hash_crc32( SGS_CTX )
{
	uint8_t* buf;
	sgs_SizeVal bufsize;
	uint32_t hv;
	sgs_Bool as_hexstr = 0;
	SGSFN( "hash_crc32" );
	if( !sgs_LoadArgs( C, "m|b", &buf, &bufsize, &as_hexstr ) )
		return 0;
	hv = sgs_crc32( buf, (size_t) bufsize, 0 );
	if( as_hexstr )
	{
		char hexstr[ 9 ] = {0};
		sprintf( hexstr, "%08x", hv );
		sgs_PushStringBuf( C, hexstr, 8 );
	}
	else
		sgs_PushInt( C, hv );
	return 1;
}


/* internal utils */

static int sgsstd_va_get_args( SGS_CTX )
{
	uint8_t i, xac, pcnt;
	sgs_StackFrame* sf;
	SGSFN( "va_get_args" );
	if( !C->sf_last || !C->sf_last->prev )
		STDLIB_WARN( "not called from function" )
	sf = C->sf_last->prev;
	/* WP: argument count limit */
	
	/* accepted arguments */
	pcnt = SGS_MIN( sf->argcount, sf->inexp );
	for( i = 0; i < pcnt; ++i )
		sgs_PushVariable( C, C->stack_base[ sf->argend - pcnt + i ] );
	/* extra arguments */
	if( sf->argcount > sf->inexp )
	{
		sgs_Variable* tpv;
		xac = (uint8_t)( sf->argcount - sf->inexp );
		tpv = C->stack_base + sf->argbeg + xac - 1;
		for( i = 0; i < xac; ++i )
			sgs_PushVariable( C, *(tpv - i) );
	}
	sgs_CreateArray( C, NULL, sf->argcount );
	return 1;
}

static int sgsstd_va_get_arg( SGS_CTX )
{
	sgs_Int argnum;
	uint8_t i, xac, pcnt;
	sgs_StackFrame* sf;
	SGSFN( "va_get_arg" );
	if( !C->sf_last || !C->sf_last->prev )
		STDLIB_WARN( "not called from function" )
	if( !sgs_LoadArgs( C, "i", &argnum ) )
		return 0;
	sf = C->sf_last->prev;
	if( argnum < 0 || argnum >= sf->argcount )
		STDLIB_WARN( "argument ID out of bounds" )
	
	/* WP: argument count limit */
	i = (uint8_t) argnum;
	
	/* accepted arguments */
	pcnt = SGS_MIN( sf->argcount, sf->inexp );
	if( i < pcnt )
	{
		sgs_PushVariable( C, C->stack_base[ sf->argend - pcnt + i ] );
	}
	else
	/* extra arguments */
	if( sf->argcount > sf->inexp )
	{
		sgs_Variable* tpv;
		i = (uint8_t)( i - pcnt );
		xac = (uint8_t)( sf->argcount - sf->inexp );
		tpv = C->stack_base + sf->argbeg + xac - 1;
		sgs_PushVariable( C, *( tpv - i ) );
	}
	else
		sgs_PushNull( C );
	
	return 1;
}

static int sgsstd_va_arg_count( SGS_CTX )
{
	SGSFN( "va_arg_count" );
	if( !C->sf_last || !C->sf_last->prev )
		STDLIB_WARN( "not called from function" )
	sgs_PushInt( C, C->sf_last->prev->argcount );
	return 1;
}


static int sgsstd_metaobj_set( SGS_CTX )
{
	sgs_VarObj *obj1, *obj2;
	SGSFN( "metaobj_set" );
	if( !sgs_LoadArgs( C, "!xx", sgs_ArgCheck_Object, &obj1, sgs_ArgCheck_Object, &obj2 ) )
		return 0;
	sgs_ObjSetMetaObj( C, obj1, obj2 );
	sgs_SetStackSize( C, 1 );
	return 1;
}

static int sgsstd_metaobj_get( SGS_CTX )
{
	sgs_VarObj* obj;
	SGSFN( "metaobj_get" );
	if( !sgs_LoadArgs( C, "!x", sgs_ArgCheck_Object, &obj ) )
		return 0;
	obj = sgs_ObjGetMetaObj( obj );
	if( obj )
	{
		sgs_PushObjectPtr( C, obj );
		return 1;
	}
	return 0;
}

static int sgsstd_metamethods_enable( SGS_CTX )
{
	sgs_VarObj* obj;
	sgs_Bool enable;
	SGSFN( "metamethods_enable" );
	if( !sgs_LoadArgs( C, "!xb", sgs_ArgCheck_Object, &obj, &enable ) )
		return 0;
	sgs_ObjSetMetaMethodEnable( obj, enable );
	sgs_SetStackSize( C, 1 );
	return 1;
}

static int sgsstd_metamethods_test( SGS_CTX )
{
	sgs_VarObj* obj;
	SGSFN( "metamethods_test" );
	if( !sgs_LoadArgs( C, "!x", sgs_ArgCheck_Object, &obj ) )
		return 0;
	sgs_PushBool( C, sgs_ObjGetMetaMethodEnable( obj ) );
	return 1;
}

static int sgsstd_mm_getindex_router( SGS_CTX )
{
	sgs_Variable func, movar;
	SGSFN( "mm_getindex_router" );
	
	if( sgs_StackSize( C ) < 1 ) goto fail;
	if( !sgs_Method( C ) || sgs_ItemType( C, 0 ) != SGS_VT_OBJECT ) goto fail;
	if( !( movar.data.O = sgs_ObjGetMetaObj( sgs_GetObjectStruct( C, 0 ) ) ) ) goto fail;
	movar.type = SGS_VT_OBJECT;
	
	sgs_PushString( C, "__get_" );
	sgs_PushItem( C, 1 );
	sgs_StringConcat( C, 2 );
	if( sgs_GetIndex( C, movar, sgs_StackItem( C, -1 ), &func, SGS_FALSE ) == SGS_FALSE ) goto fail;
	
	sgs_SetStackSize( C, 1 );
	sgs_ThisCall( C, func, 0, 1 );
	sgs_Release( C, &func );
	return 1;
	
fail:
	return 0;
}

static int sgsstd_mm_setindex_router( SGS_CTX )
{
	sgs_Variable func, movar;
	SGSFN( "mm_setindex_router" );
	
	if( sgs_StackSize( C ) < 2 ) goto fail;
	if( !sgs_Method( C ) || sgs_ItemType( C, 0 ) != SGS_VT_OBJECT ) goto fail;
	if( !( movar.data.O = sgs_ObjGetMetaObj( sgs_GetObjectStruct( C, 0 ) ) ) ) goto fail;
	movar.type = SGS_VT_OBJECT;
	
	sgs_PushString( C, "__set_" );
	sgs_PushItem( C, 1 );
	sgs_StringConcat( C, 2 );
	if( sgs_GetIndex( C, movar, sgs_StackItem( C, -1 ), &func, SGS_FALSE ) == SGS_FALSE ) goto fail;
	
	sgs_SetStackSize( C, 3 );
	sgs_PopSkip( C, 1, 1 );
	sgs_ThisCall( C, func, 1, 0 );
	sgs_Release( C, &func );
	return 0;
	
fail:
	return 0;
}



static int sgsstd_event_getindex( SGS_ARGS_GETINDEXFUNC )
{
	SGS_BEGIN_INDEXFUNC
		SGS_CASE( "signaled" ) return sgs_PushBool( C, obj->data != NULL );
	SGS_END_INDEXFUNC;
}

static int sgsstd_event_setindex( SGS_ARGS_SETINDEXFUNC )
{
	SGS_BEGIN_INDEXFUNC
		SGS_CASE( "signaled" )
		{
			sgs_Bool val = SGS_FALSE;
			if( sgs_ParseBool( C, 1, &val ) == SGS_FALSE )
				return SGS_EINVAL;
			obj->data = val ? obj : NULL;
			return SGS_SUCCESS;
		}
	SGS_END_INDEXFUNC;
}

static int sgsstd_event_convert( SGS_CTX, sgs_VarObj* obj, int type )
{
	if( type == SGS_VT_BOOL )
	{
		return sgs_PushBool( C, obj->data != NULL );
	}
	return SGS_ENOTSUP;
}

static int sgsstd_event_serialize( SGS_CTX, sgs_VarObj* obj )
{
	sgs_PushBool( C, obj->data != NULL );
	sgs_SerializeObject( C, 1, "event" );
	return SGS_SUCCESS;
}

static int sgsstd_event_dump( SGS_CTX, sgs_VarObj* obj, int maxdepth )
{
	char bfr[ 32 ];
	SGS_UNUSED( maxdepth );
	sprintf( bfr, "event(signaled=%s)", obj->data ? "true" : "false" );
	return sgs_PushString( C, bfr );
}

SGS_APIFUNC sgs_ObjInterface sgsstd_event_iface[1] =
{{
	"event",
	NULL, NULL,
	sgsstd_event_getindex, sgsstd_event_setindex,
	sgsstd_event_convert, sgsstd_event_serialize, sgsstd_event_dump, NULL,
	NULL, NULL
}};

SGSONE sgs_CreateEvent( SGS_CTX, sgs_Variable* out )
{
	return sgs_CreateObject( C, out, NULL, sgsstd_event_iface );
}

SGSBOOL sgs_EventState( SGS_CTX, sgs_Variable evt, int state )
{
	SGSBOOL origstate;
	if( !sgs_IsObjectP( &evt, sgsstd_event_iface ) )
		return sgs_Msg( C, SGS_APIERR, "sgs_EventState: specified variable is not of 'event' type" );
	origstate = sgs_GetObjectDataP( &evt ) != NULL;
	if( state != SGS_QUERY )
		sgs_SetObjectDataP( &evt, state ? evt.data.O : NULL );
	return origstate;
}

static int sgsstd_event( SGS_CTX )
{
	sgs_Bool val = SGS_FALSE;
	SGSFN( "event" );
	if( !sgs_LoadArgs( C, "|b", &val ) )
		return 0;
	sgs_CreateEvent( C, NULL );
	if( val )
		sgs_EventState( C, sgs_StackItem( C, -1 ), SGS_TRUE );
	return 1;
}

static void sgs__create_pooled_event( SGS_CTX, sgs_Variable* out, sgs_Variable dict, sgs_Variable key, int val )
{
	sgs_Variable evar;
	if( out ? sgs_GetIndex( C, dict, key, out, SGS_FALSE ) : sgs_PushIndex( C, dict, key, SGS_FALSE ) )
		return;
	sgs_CreateEvent( C, out );
	evar = out ? *out : sgs_StackItem( C, -1 );
	if( val )
		sgs_EventState( C, evar, SGS_TRUE );
	sgs_SetIndex( C, dict, key, evar, SGS_FALSE );
}

SGSONE sgs_CreatePooledEventBuf( SGS_CTX, sgs_Variable* out, sgs_Variable dict, const char* str, sgs_SizeVal size )
{
	sgs_Variable key;
	sgs_InitStringBuf( C, &key, str, size );
	sgs__create_pooled_event( C, out, dict, key, 0 );
	sgs_Release( C, &key );
	return 1;
}

static int sgsstd_pooled_event( SGS_CTX )
{
	sgs_Bool val = SGS_FALSE;
	SGSFN( "pooled_event" );
	if( !sgs_LoadArgs( C, "?t?s|b", &val ) )
		return 0;
	sgs__create_pooled_event( C, NULL, sgs_StackItem( C, 0 ), sgs_StackItem( C, 1 ), val );
	return 1;
}

static void sgs__check_threadendtbl( SGS_CTX )
{
	sgs_Variable endtbl;
	if( C->_E )
		return;
	
	sgsSTD_MakeMap( C, &endtbl, 0 );
	C->_E = endtbl.data.O;
}

void sgs_EndOn( SGS_CTX, sgs_Variable ev, int enable )
{
	/* if we're trying to disable an end event and ..
	.. table doesn't exist - it's already disabled */
	if( enable )
		sgs__check_threadendtbl( C );
	if( C->_E )
	{
		sgs_VHTable* ht = &((DictHdr*)C->_E->data)->ht;
		if( enable )
		{
			sgs_Variable val = sgs_MakeNull();
			sgs_vht_set( ht, C, &ev, &val );
		}
		else
		{
			sgs_vht_unset( ht, C, &ev );
		}
	}
}

int sgsstd_end_on( SGS_CTX )
{
	sgs_Bool enable = SGS_TRUE;
	sgs_Context* which = C;
	SGSFN( "end_on" );
	if( sgs_Method( C ) )
	{
		if( !sgs_LoadArgs( C, "@y", &which ) )
			return 0;
		sgs_HideThis( C );
	}
	if( !sgs_LoadArgs( C, "?*|b", &enable ) )
		return 0;
	
	sgs_EndOn( which, sgs_StackItem( C, 0 ), enable );
	return 0;
}



static int sgsstd_co_create( SGS_CTX )
{
	sgs_Context* T;
	SGSFN( "co_create" );
	if( !sgs_LoadArgs( C, "?p." ) )
		return 0;
	
	T = sgsCTX_ForkState( C, 0 );
	sgs_PushVariable( T, sgs_StackItem( C, 0 ) );
	T->state |= SGS_STATE_COROSTART;
	sgs_BreakIf( T->refcount != 0 );
	return sgs_PushThreadPtr( C, T );
}

int sgsstd_co_resume( SGS_CTX )
{
	sgs_Context* T = NULL;
	sgs_StkIdx i, ssz;
	int rvc = 0;
	
	SGSFN( "co_resume" );
	sgs_Method( C );
	if( !sgs_LoadArgs( C, "@y", &T ) )
		return 0;
	sgs_ForceHideThis( C );
	
	if( ( T->state & SGS_STATE_COROSTART ) == 0 && T->sf_last == NULL )
	{
		STDLIB_WARN( "coroutine is finished, cannot resume" );
	}
	
	ssz = sgs_StackSize( C );
	
	if( C->hook_fn )
		C->hook_fn( C->hook_ctx, C, SGS_HOOK_PAUSE );
	
	if( T->sf_last )
	{
		for( i = 0; i < ssz; ++i )
			sgs_PushVariable( T, sgs_StackItem( C, i ) );
		if( !sgs_ResumeStateRet( T, ssz, &rvc ) )
		{
			if( C->hook_fn )
				C->hook_fn( C->hook_ctx, C, SGS_HOOK_CONT );
			STDLIB_WARN( "failed to resume coroutine" );
		}
	}
	else if( T->state & SGS_STATE_COROSTART )
	{
		sgs_Variable func;
		T->state &= ~(uint32_t)SGS_STATE_COROSTART;
		sgs_StoreVariable( T, &func );
		for( i = 0; i < ssz; ++i )
			sgs_PushVariable( T, sgs_StackItem( C, i ) );
		sgs_XCall( T, func, ssz, &rvc );
		sgs_Release( C, &func );
	}
	/* else - already handled */
	
	if( C->hook_fn )
		C->hook_fn( C->hook_ctx, C, SGS_HOOK_CONT );
	
	for( i = -rvc; i < 0; ++i )
	{
		sgs_PushVariable( C, sgs_StackItem( T, i ) );
	}
	
	return rvc;
}

int sgsstd_abort( SGS_CTX )
{
	sgs_SizeVal i, ssz, abc = 0;
	SGSFN( "abort" );
	sgs_Method( C );
	ssz = sgs_StackSize( C );
	if( ssz == 0 )
		return sgs_PushBool( C, sgs_Abort( C ) );
	
	for( i = 0; i < ssz; ++i )
	{
		sgs_Context* T = NULL;
		if( !sgs_LoadArgsExt( C, i, "y", &T ) )
			return 0;
		abc += sgs_Abort( T );
	}
	
	sgs_PushInt( C, abc );
	return 1;
}

sgs_Context* sgs_TopmostContext( SGS_CTX )
{
	while( C->parent )
		C = C->parent;
	return C;
}

static void sgs__check_threadtbl( SGS_CTX )
{
	sgs_Variable thrtbl;
	if( C->_T )
		return;
	
	sgsSTD_MakeMap( C, &thrtbl, 0 );
	C->_T = thrtbl.data.O;
}

SGSBOOL sgs_CreateSubthread( sgs_Context* T, SGS_CTX,
	sgs_Variable* out, sgs_Variable func, int gotthis, sgs_StkIdx size )
{
	sgs_Real waittime = 0;
	sgs_StkIdx i;
	sgs_Context* co_ctx;
	
	/* call the function */
	co_ctx = sgsCTX_ForkState( T, 0 );
	if( gotthis )
	{
		sgs_PushVariable( co_ctx, sgs_StackItem( C, -size - 1 ) );
	}
	for( i = 0; i < size; ++i )
	{
		sgs_PushVariable( co_ctx, sgs_StackItem( C, i - size ) );
	}
	sgs_FCall( co_ctx, func, size, 1, gotthis );
	
	waittime = sgs_GetReal( co_ctx, -1 );
	sgs_Pop( co_ctx, 1 );
	
	/* register thread if not done */
	sgs_BreakIf( co_ctx->refcount != 0 );
	if( co_ctx->sf_last && ( co_ctx->sf_last->flags & SGS_SF_PAUSED ) )
	{
		sgs_Variable varT, varSubT;
		sgs__check_threadtbl( T );
		varT.type = SGS_VT_OBJECT;
		varT.data.O = T->_T;
		varSubT.type = SGS_VT_THREAD;
		varSubT.data.T = co_ctx;
		sgs_SetIndex( C, varT, varSubT, sgs_MakeReal( waittime ), SGS_FALSE );
		co_ctx->parent = T;
	}
	if( out )
	{
		sgs_InitThreadPtr( out, co_ctx );
		return SGS_TRUE;
	}
	else
		return sgs_PushThreadPtr( C, co_ctx );
}

static int sgsstd_thread_create( SGS_CTX )
{
	SGSFN( "thread_create" );
	if( !sgs_LoadArgs( C, "?p" ) )
		return 0;
	
	if( sgs_CreateSubthread( sgs_TopmostContext( C ), C, NULL,
		sgs_StackItem( C, 0 ), 1, sgs_StackSize( C ) - 2 ) == SGS_FALSE )
	{
		return sgs_Msg( C, SGS_WARNING, "failed to create a thread - could not call function" );
	}
	return 1;
}

static int sgsstd_subthread_create( SGS_CTX )
{
	SGSFN( "subthread_create" );
	if( !sgs_LoadArgs( C, "?p" ) )
		return 0;
	
	if( sgs_CreateSubthread( C, C, NULL, sgs_StackItem( C, 0 ), 1, sgs_StackSize( C ) - 2 ) == SGS_FALSE )
	{
		return sgs_Msg( C, SGS_WARNING, "failed to create a thread - could not call function" );
	}
	return 1;
}

static int sgs__anyevent( SGS_CTX )
{
	if( C->_E )
	{
		sgs_VHTIdx i;
		sgs_VHTable* ht = &((DictHdr*)C->_E->data)->ht;
		for( i = 0; i < ht->size; ++i )
		{
			sgs_VHTVar* v = &ht->vars[ i ];
			if( sgs_GetBoolP( C, &v->key ) )
				return 1;
		}
	}
	return 0;
}

int sgs_ProcessSubthreads( SGS_CTX, sgs_Real dt )
{
	C->wait_timer += dt;
	if( C->_T )
	{
		sgs_VHTIdx i;
		sgs_VHTable* ht = &((DictHdr*)C->_T->data)->ht;
		for( i = 0; i < ht->size; ++i )
		{
			sgs_VHTVar* v = &ht->vars[ i ];
			sgs_Context* thctx = v->key.data.T;
			sgs_ProcessSubthreads( thctx, dt );
			v->val.data.R -= dt;
			if( sgs__anyevent( thctx ) )
			{
				sgs_Abort( thctx );
			}
			else if( v->val.data.R <= 0 )
			{
				sgs_ResumeStateExp( thctx, 0, 1 );
				v->val.data.R = sgs_GetReal( thctx, -1 );
				sgs_Pop( thctx, 1 );
			}
			if( thctx->sf_last == NULL || ( thctx->sf_first->flags & SGS_SF_ABORTED ) )
			{
				thctx->parent = NULL;
				sgs_vht_unset( ht, C, &v->key );
				i--; /* unset replaces current element in array with last */
			}
		}
		return ht->size;
	}
	return 0;
}

void sgsSTD_ThreadsFree( SGS_CTX )
{
	if( C->_T )
	{
		sgs_VHTIdx i;
		sgs_VHTable* ht = &((DictHdr*)C->_T->data)->ht;
		for( i = 0; i < ht->size; ++i )
		{
			sgs_VHTVar* v = &ht->vars[ i ];
			sgs_Context* thctx = v->key.data.T;
			thctx->parent = NULL;
			sgs_vht_unset( ht, C, &v->key );
			i--; /* unset replaces current element in array with last */
		}
		sgs_BreakIf( C->_T->refcount != 1 );
		sgs_ObjRelease( C, C->_T );
		C->_T = NULL;
	}
	if( C->parent )
	{
		sgs_Context* PC = C->parent;
		sgs_VHTable* ht = &((DictHdr*)C->parent->_T->data)->ht;
		sgs_Variable selfkey;
		selfkey.type = SGS_VT_THREAD;
		selfkey.data.T = C;
		C->parent = NULL;
		PC->refcount++; /* prevent freeing of parent during the following operation */
		sgs_vht_unset( ht, PC, &selfkey );
		PC->refcount--;
		if( PC->refcount == 0 )
			sgsCTX_FreeState( PC );
	}
	if( C->_E )
	{
		sgs_ObjRelease( C, C->_E );
		C->_E = NULL;
	}
}

void sgsSTD_ThreadsGC( SGS_CTX )
{
	if( C->_T )
	{
		sgs_ObjGCMark( C, C->_T );
	}
	if( C->_E )
	{
		sgs_ObjGCMark( C, C->_E );
	}
}

static int sgsstd_process_threads( SGS_CTX )
{
	sgs_Real dt = 0;
	SGSFN( "process_threads" );
	if( !sgs_LoadArgs( C, "|r", &dt ) )
		return 0;
	
	return sgs_PushInt( C, sgs_ProcessSubthreads( C, dt ) );
}

static int sgsstd_yield( SGS_CTX )
{
	SGSFN( "yield" );
	if( sgs_PauseState( C ) == SGS_FALSE )
		STDLIB_WARN( "cannot yield with C functions in stack" );
	return sgs_StackSize( C );
}



struct pcall_printinfo
{
	sgs_MsgFunc pfn;
	void* pctx;
	sgs_Variable handler;
	int depth;
};

static void sgsstd_pcall_print( void* data, SGS_CTX, int type, const char* message )
{
	int ret = 0;
	struct pcall_printinfo* P = (struct pcall_printinfo*) data;
	P->depth++;
	
	if( P->depth > 1 )
		ret = type; /* don't handle errors thrown inside handlers */
	else if( P->handler.type != SGS_VT_NULL )
	{
		sgs_PushInt( C, type );
		sgs_PushString( C, message );
		sgs_Call( C, P->handler, 2, 1 );
		if( sgs_Cntl( C, SGS_CNTL_GET_ABORT, 0 ) )
			sgs_Abort( C );
		ret = (int) sgs_GetInt( C, -1 );
		sgs_Pop( C, 1 );
	}
	
	if( ret > 0 )
		P->pfn( P->pctx, C, ret, message );
	
	P->depth--;
}

static int sgsstd_pcall( SGS_CTX )
{
	struct pcall_printinfo P;
	int b = 0;
	
	SGSFN( "pcall" );
	
	if( !sgs_LoadArgs( C, "?p|p", &b ) )
		return 0;
	
	P.pfn = C->msg_fn;
	P.pctx = C->msg_ctx;
	P.handler.type = SGS_VT_NULL;
	P.depth = 0;
	if( b )
		sgs_GetStackItem( C, 1, &P.handler );
	
	C->msg_fn = sgsstd_pcall_print;
	C->msg_ctx = &P;
	
	sgs_Call( C, sgs_StackItem( C, 0 ), 0, 0 );
	
	C->msg_fn = P.pfn;
	C->msg_ctx = P.pctx;
	if( b )
		sgs_Release( C, &P.handler );
	
	return 0;
}

static int sgsstd_assert( SGS_CTX )
{
	char* str = NULL;
	
	SGSFN( "assert" );
	
	if( !sgs_LoadArgs( C, "?v|s", &str ) )
		return 0;
	
	SGSFN( NULL );
	if( !sgs_GetBool( C, 0 ) )
		sgs_Msg( C, SGS_ERROR, !str ? "assertion failed" :
			"assertion failed: %s", str );
	return 0;
}

static int sgsstd_sym_register( SGS_CTX )
{
	char* str = NULL;
	sgs_Variable var;
	
	SGSFN( "sym_register" );
	if( !sgs_LoadArgs( C, "sv", &str, &var ) )
		return 0;
	
	sgs_RegSymbol( C, "", str, var );
	return 0;
}

static int sgsstd_sym_get( SGS_CTX )
{
	sgs_Variable var, sym;
	SGSFN( "sym_get" );
	if( !sgs_LoadArgs( C, "v", &var ) )
		return 0;
	
	if( !sgs_GetSymbol( C, var, &sym ) )
		return sgs_Msg( C, SGS_WARNING, "symbol not found" );
	sgs_PushVariable( C, sym );
	sgs_Release( C, &sym );
	return 1;
}

static int sgsstd_eval( SGS_CTX )
{
	char* str;
	sgs_SizeVal size;
	int rvc = 0;
	
	SGSFN( "eval" );
	
	if( !sgs_LoadArgs( C, "m", &str, &size ) )
		return 0;
	
	/* WP: string limit */
	sgs_EvalBuffer( C, str, (size_t) size, &rvc );
	return rvc;
}

static int sgsstd_eval_file( SGS_CTX )
{
	int ret, retcnt = 0;
	char* str;
	
	SGSFN( "eval_file" );
	
	if( !sgs_LoadArgs( C, "s", &str ) )
		return 0;
	
	ret = sgs_EvalFile( C, str, &retcnt );
	if( ret == SGS_ENOTFND )
		STDLIB_WARN( "file not found" )
	return retcnt;
}


static void _sgsstd_compile_pfn( void* data, SGS_CTX, int type, const char* msg )
{
	sgs_Variable* pvar = (sgs_Variable*) data;
	
	sgs_PushVariable( C, *pvar );
	
	sgs_PushString( C, "type" );
	sgs_PushInt( C, type );
	sgs_PushString( C, "msg" );
	sgs_PushString( C, msg );
	sgs_CreateDict( C, NULL, 4 );
	
	sgs_ArrayPush( C, sgs_StackItem( C, -2 ), 1 );
	sgs_Pop( C, 1 );
}

static int sgsstd_compile_sgs( SGS_CTX )
{
	int ret;
	char* buf = NULL, *outbuf = NULL;
	sgs_SizeVal size = 0;
	size_t outsize = 0;
	sgs_Variable var;
	
	sgs_MsgFunc oldpfn;
	void* oldpctx;
	
	SGSFN( "compile_sgs" );
	
	if( !sgs_LoadArgs( C, "m", &buf, &size ) )
		return 0;
	
	sgs_CreateArray( C, NULL, 0 );
	sgs_GetStackItem( C, -1, &var );
	sgs_Pop( C, 1 );
	
	oldpfn = C->msg_fn;
	oldpctx = C->msg_ctx;
	sgs_SetMsgFunc( C, _sgsstd_compile_pfn, &var );
	SGSFN( NULL );
	
	/* WP: string limit */
	ret = sgs_Compile( C, buf, (size_t) size, &outbuf, &outsize );
	
	SGSFN( "compile_sgs" );
	C->msg_fn = oldpfn;
	C->msg_ctx = oldpctx;
	
	if( ret < 0 )
		sgs_PushNull( C );
	else
	{
		if( outsize <= 0x7fffffff )
			sgs_PushStringBuf( C, outbuf, (sgs_SizeVal) outsize );
		else
		{
			sgs_PushNull( C );
			sgs_Msg( C, SGS_WARNING, "size of compiled code is bigger than allowed to store" );
		}
		sgs_Dealloc( outbuf );
	}
	sgs_PushVariable( C, var );
	sgs_Release( C, &var );
	
	return 2;
}


static SGSBOOL sgsstd__inclib( SGS_CTX, const char* name, int override )
{
	char buf[ 16 ];
	sgs_Variable reginc = sgs_Registry( C, SGS_REG_INC );
	
	sprintf( buf, ":%.14s", name );
	if( !override && sgs_PushProperty( C, reginc, buf ) )
	{
		sgs_Pop( C, 1 );
		return SGS_TRUE;
	}
	sgs_Pop( C, 1 );
	
#if !SGS_NO_STDLIB
	if( strcmp( name, "fmt" ) == 0 ) sgs_LoadLib_Fmt( C );
	else if( strcmp( name, "io" ) == 0 ) sgs_LoadLib_IO( C );
	else if( strcmp( name, "math" ) == 0 ) sgs_LoadLib_Math( C );
	else if( strcmp( name, "os" ) == 0 ) sgs_LoadLib_OS( C );
	else if( strcmp( name, "re" ) == 0 ) sgs_LoadLib_RE( C );
	else if( strcmp( name, "string" ) == 0 ) sgs_LoadLib_String( C );
	else return SGS_FALSE;
#endif
	
	sgs_SetProperty( C, reginc, buf, sgs_MakeBool( SGS_TRUE ) );
	return SGS_TRUE;
}

static int sgsstd__chkinc( SGS_CTX, int argid )
{
	sgs_Variable val;
	if( sgs_GetIndex( C, sgs_Registry( C, SGS_REG_INC ),
		sgs_StackItem( C, argid ), &val, SGS_FALSE ) )
	{
		sgs_Release( C, &val );
		return SGS_TRUE;
	}
	return SGS_FALSE;
}

static void sgsstd__setinc( SGS_CTX, int argid )
{
	sgs_SetIndex( C, sgs_Registry( C, SGS_REG_INC ), sgs_StackItem( C, argid ),
		sgs_MakeBool( SGS_TRUE ), SGS_FALSE );
}

static int sgsstd_include_library( SGS_CTX )
{
	int ret, over = SGS_FALSE;
	char* str;
	
	SGSBASEFN( "include_library" );
	
	if( !sgs_LoadArgs( C, "s|b", &str, &over ) )
		return 0;
	
	ret = sgsstd__inclib( C, str, over );
	
	if( ret == SGS_FALSE )
		STDLIB_WARN( "library not found" )
	sgs_PushBool( C, ret );
	return 1;
}

static int sgsstd_include_file( SGS_CTX )
{
	int ret, over = SGS_FALSE;
	char* str;
	
	SGSBASEFN( "include_file" );
	
	if( !sgs_LoadArgs( C, "s|b", &str, &over ) )
		return 0;
	
	if( !over && sgsstd__chkinc( C, 0 ) )
		return 1;
	
	ret = sgs_ExecFile( C, str );
	if( ret == SGS_ENOTFND )
		return sgs_Msg( C, SGS_WARNING, "file '%s' was not found", str );
	if( ret == SGS_SUCCESS )
		sgsstd__setinc( C, 0 );
	sgs_PushBool( C, ret == SGS_SUCCESS );
	return 1;
}

static int sgsstd_include_shared( SGS_CTX )
{
	char* fnstr;
	int ret, over = SGS_FALSE;
	sgs_CFunc func;
	
	SGSBASEFN( "include_shared" );
	
	if( !sgs_LoadArgs( C, "s|b", &fnstr, &over ) )
		return 0;
	
	if( !over && sgsstd__chkinc( C, 0 ) )
		return 1;
	
	ret = sgsXPC_GetProcAddress( fnstr, SGS_LIB_ENTRY_POINT, (void**) &func );
	if( ret != 0 )
	{
		if( ret == SGS_XPC_NOFILE )
			return sgs_Msg( C, SGS_WARNING, "file '%s' was not found", fnstr );
		else if( ret == SGS_XPC_NOPROC )
			return sgs_Msg( C, SGS_WARNING, "procedure '" SGS_LIB_ENTRY_POINT "' was not found" );
		else if( ret == SGS_XPC_NOTSUP )
			STDLIB_WARN( "feature is not supported on this platform" )
		else STDLIB_WARN( "unknown error occured" )
	}
	
	ret = func( C );
	if( ret != SGS_SUCCESS )
		STDLIB_WARN( "module failed to initialize" )
	
	sgsstd__setinc( C, 0 );
	sgs_PushBool( C, SGS_TRUE );
	return 1;
}

static int _push_curdir( SGS_CTX )
{
	const char* file, *fend;
	sgs_StackFrame* sf;
	
	sf = sgs_GetFramePtr( C, NULL, 1 )->prev;
	if( !sf )
		return 0;
	
	sgs_StackFrameInfo( C, sf, NULL, &file, NULL );
	if( file )
	{
		fend = file + strlen( file );
		while( fend > file && *fend != '/' && *fend != '\\' )
			fend--;
		if( fend == file )
		{
#ifdef _WIN32
			sgs_PushString( C, "." );
#else
			if( *file == '/' )
				sgs_PushString( C, "" );
			else
				sgs_PushString( C, "." );
#endif
		}
		else
		{
			ptrdiff_t len = fend - file;
			if( sizeof(ptrdiff_t) > 4 && len > 0x7fffffff )
				return 0;
			else
				/* WP: error condition */
				sgs_PushStringBuf( C, file, (sgs_SizeVal) len );
		}
		return 1;
	}
	
	return 0;
}

static int _push_procdir( SGS_CTX )
{
	char* mfn = sgsXPC_GetModuleFileName();
	if( mfn )
	{
		char* mfnend = mfn + strlen( mfn );
		while( mfnend > mfn && *mfnend != '/' && *mfnend != '\\' )
			mfnend--;
		if( ((size_t)( mfnend - mfn )) > 0x7fffffff )
		{
			free( mfn );
			return 0;
		}
		/* WP: added error condition */
		sgs_PushStringBuf( C, mfn, (sgs_SizeVal)( mfnend - mfn ) );
		free( mfn );
		return 1;
	}
	else
		return 0;
}

static int _find_includable_file( SGS_CTX, sgs_MemBuf* tmp, char* ps,
	size_t pssize, char* fn, size_t fnsize, char* dn, size_t dnsize, char* pd, size_t pdsize )
{
	SGS_SHCTX_USE;
	if( ( fnsize > 2 && *fn == '.' && ( fn[1] == '/' || fn[1] == '\\' ) ) ||
#ifdef _WIN32
		( fnsize > 2 && fn[1] == ':' ) )
#else
		( fnsize > 1 && *fn == '/' ) )
#endif
	{
		sgs_membuf_setstrbuf( tmp, C, fn, fnsize );
		
		sgs_ScriptFSData fsd = {0};
		fsd.filename = tmp->ptr;
		if( SGS_SUCCEEDED( S->sfs_fn( S->sfs_ctx, C, SGS_SFS_FILE_EXISTS, &fsd ) ) )
			return 1;
	}
	else
	{
		char* pse = ps + pssize;
		char* psc = ps;
		while( ps <= pse )
		{
			if( ps == pse || *ps == ';' )
			{
				sgs_membuf_resize( tmp, C, 0 );
				while( psc < ps )
				{
					if( *psc == '?' )
						sgs_membuf_appbuf( tmp, C, fn, fnsize );
					else if( *psc == '|' )
					{
						if( dn )
							sgs_membuf_appbuf( tmp, C, dn, dnsize );
						else
						{
							psc = ps;
							goto notthispath;
						}
					}
					else if( *psc == '@' )
					{
						if( pd )
							sgs_membuf_appbuf( tmp, C, pd, pdsize );
						else
						{
							psc = ps;
							goto notthispath;
						}
					}
					else
						sgs_membuf_appchr( tmp, C, *psc );
					psc++;
				}
				sgs_membuf_appchr( tmp, C, 0 );
				
				{
					sgs_ScriptFSData fsd = {0};
					fsd.filename = tmp->ptr;
					if( SGS_SUCCEEDED( S->sfs_fn( S->sfs_ctx, C, SGS_SFS_FILE_EXISTS, &fsd ) ) )
						return 1;
				}
notthispath:
				psc++;
			}
			ps++;
		}
	}
	return 0;
}

static int sgsstd_include( SGS_CTX )
{
	char* fnstr, *dnstr = NULL, *pdstr = NULL;
	sgs_SizeVal fnsize, dnsize = 0, pdsize = 0;
	int over = 0, ret;
	
	SGSFN( "include" );
	
	if( !sgs_LoadArgs( C, "m|b", &fnstr, &fnsize, &over ) )
		return 0;
	
	if( !over && sgsstd__chkinc( C, 0 ) )
		goto success;
	
	ret = sgsstd__inclib( C, fnstr, over );
	if( ret )
		goto success;
	else
	{
		char* ps;
		sgs_SizeVal pssize;
		sgs_CFunc func;
		sgs_MemBuf mb = sgs_membuf_create();
		
		ret = sgs_PushGlobalByName( C, "SGS_PATH" );
		if( ret != SGS_SUCCESS ||
			( ps = sgs_ToStringBuf( C, -1, &pssize ) ) == NULL )
		{
			ps = SGS_INCLUDE_PATH;
			pssize = (sgs_SizeVal) strlen( ps );
		}
		
		if( _push_curdir( C ) )
		{
			dnstr = sgs_GetStringPtr( C, -1 );
			dnsize = sgs_GetStringSize( C, -1 );
		}
		if( _push_procdir( C ) )
		{
			pdstr = sgs_GetStringPtr( C, -1 );
			pdsize = sgs_GetStringSize( C, -1 );
		}
		/* WP: string limit */
		ret = _find_includable_file( C, &mb, ps, (size_t) pssize, fnstr, (size_t) fnsize, dnstr, (size_t) dnsize, pdstr, (size_t) pdsize );
		if( ret == 0 || mb.size == 0 )
		{
			sgs_membuf_destroy( &mb, C );
			return sgs_Msg( C, SGS_WARNING, "could not find '%.*s' "
				"with include path '%.*s'", fnsize, fnstr, pssize, ps );
		}
		
		sgs_PushString( C, mb.ptr );
		sgs_PushString( C, " - include" );
		sgs_StringConcat( C, 2 );
		SGSFN( sgs_GetStringPtr( C, -1 ) );
		ret = sgs_ExecFile( C, mb.ptr );
		SGSFN( "include" );
		if( ret == SGS_SUCCESS )
		{
			sgs_membuf_destroy( &mb, C );
			goto success;
		}
		else if( ret == SGS_ECOMP || ret == SGS_EINVAL )
		{
			/* compilation / parsing error, already printed */
			sgs_membuf_destroy( &mb, C );
			sgs_PushBool( C, 0 );
			return 1;
		}
		/* SGS_ENOTSUP - binary detected */
		
		ret = sgsXPC_GetProcAddress( mb.ptr, SGS_LIB_ENTRY_POINT, (void**) &func );
		if( SGS_SUCCEEDED( ret ) )
		{
			ret = func( C );
			if( SGS_SUCCEEDED( ret ) )
			{
				sgs_membuf_destroy( &mb, C );
				goto success;
			}
		}
		else
		{
			sgs_membuf_destroy( &mb, C );
			return sgs_Msg( C, SGS_ERROR, "failed to load native module '%.*s'", fnsize, fnstr );
		}
		
		sgs_membuf_destroy( &mb, C );
	}
	
	sgs_Msg( C, SGS_WARNING, "could not load '%.*s'", fnsize, fnstr );
	sgs_PushBool( C, 0 );
	return 1;
	
success:
	sgsstd__setinc( C, 0 );
	sgs_PushBool( C, 1 );
	return 1;
}

static int sgsstd_import_cfunc( SGS_CTX )
{
	char* fnstr, *pnstr;
	int ret;
	sgs_CFunc func;
	
	SGSFN( "import_cfunc" );
	
	if( !sgs_LoadArgs( C, "ss", &fnstr, &pnstr ) )
		return 0;
	
	ret = sgsXPC_GetProcAddress( fnstr, pnstr, (void**) &func );
	if( ret != 0 )
	{
		if( ret == SGS_XPC_NOFILE )
			return sgs_Msg( C, SGS_WARNING, "file '%s' was not found", fnstr );
		else if( ret == SGS_XPC_NOPROC )
			return sgs_Msg( C, SGS_WARNING, "procedure '%s' was not found", pnstr );
		else if( ret == SGS_XPC_NOTSUP )
			STDLIB_WARN( "feature is not supported on this platform" )
		else STDLIB_WARN( "unknown error occured" )
	}
	
	sgs_PushCFunc( C, func );
	return 1;
}

static int sgsstd_sys_curfile( SGS_CTX )
{
	const char* file;
	sgs_StackFrame* sf;
	
	SGSFN( "sys_curfile" );
	
	if( sgs_StackSize( C ) )
		STDLIB_WARN( "function expects 0 arguments" )
	
	sf = sgs_GetFramePtr( C, NULL, 1 )->prev;
	if( !sf )
		return 0;
	
	sgs_StackFrameInfo( C, sf, NULL, &file, NULL );
	if( file )
	{
		sgs_PushString( C, file );
		return 1;
	}
	return 0;
}

static int sgsstd_sys_curfiledir( SGS_CTX )
{
	SGSFN( "sys_curfiledir" );
	
	if( sgs_StackSize( C ) )
		STDLIB_WARN( "function expects 0 arguments" )
	
	return _push_curdir( C );
}

static int sgsstd_sys_curprocfile( SGS_CTX )
{
	char* path;
	SGSFN( "sys_curprocfile" );
	
	if( sgs_StackSize( C ) )
		STDLIB_WARN( "function expects 0 arguments" )
	
	path = sgsXPC_GetModuleFileName();
	sgs_Errno( C, path != NULL );
	if( path )
	{
		sgs_PushString( C, path );
		free( path );
		return 1;
	}
	else
		return 0;
}

static int sgsstd_sys_curprocdir( SGS_CTX )
{
	SGSFN( "sys_curprocdir" );
	
	if( sgs_StackSize( C ) )
		STDLIB_WARN( "function expects 0 arguments" )
	
	return _push_procdir( C );
}

static int sgsstd_multiply_path_ext_lists( SGS_CTX )
{
	char *pp, *ss, *prefixes, *osfx, *suffixes = "?;?" SGS_MODULE_EXT ";?.sgc;?.sgs", *joinstr = "/";
	size_t joinstrlen;
	SGSFN( "multiply_path_ext_lists" );
	
	if( !sgs_LoadArgs( C, "s|ss", &prefixes, &joinstr, &suffixes ) )
		return 0;
	
	joinstrlen = strlen( joinstr );
	
	sgs_CreateArray( C, NULL, 0 );
	osfx = suffixes;
	pp = prefixes;
	for(;;)
	{
		if( *pp == ';' || *pp == '\0' )
		{
			ss = suffixes = osfx;
			for(;;)
			{
				if( *ss == ';' || *ss == '\0' )
				{
					char* tmp;
					/* WP: assume no wrap-arounds on string iteration */
					size_t pplen = (size_t) ( pp - prefixes );
					size_t sslen = (size_t) ( ss - suffixes );
					if( pplen + sslen + joinstrlen > 0x7fffffff )
						STDLIB_WARN( "generated path size is bigger than allowed to store" )
					/* WP: error condition */
					tmp = sgs_PushStringAlloc( C, (sgs_SizeVal)( pplen + sslen + joinstrlen ) );
					memcpy( tmp, prefixes, pplen );
					memcpy( tmp + pplen, joinstr, joinstrlen );
					memcpy( tmp + pplen + joinstrlen, suffixes, sslen );
					sgs_FinalizeStringAlloc( C, -1 );
					sgs_ArrayPush( C, sgs_StackItem( C, -2 ), 1 );
					if( !*ss )
						break;
					suffixes = ++ss;
				}
				else
					ss += !!*ss;
			}
			if( !*pp )
				break;
			prefixes = ++pp;
		}
		else
			pp += !!*pp;
	}
	
	return 1;
}

static int sgsstd_sys_backtrace( SGS_CTX )
{
	sgs_Bool as_errinfo = 0;
	SGSFN( "sys_backtrace" );
	
	if( !sgs_LoadArgs( C, "|b", &as_errinfo ) )
		return 0;
	
	if( as_errinfo )
		sgs_PushErrorInfo( C, SGS_ERRORINFO_STACK, 0, NULL );
	else
	{
		sgs_StkIdx sz = sgs_StackSize( C );
		sgs_StackFrame* p = sgs_GetFramePtr( C, NULL, SGS_FALSE );
		while( p != NULL )
		{
			const char* file, *name;
			int ln;
			if( !p->next && !p->code )
				break;
			sgs_StackFrameInfo( C, p, &name, &file, &ln );
			
			sgs_PushString( C, "func" );
			sgs_PushString( C, name );
			sgs_PushString( C, "line" );
			if( ln )
				sgs_PushInt( C, ln );
			else
				sgs_PushNull( C );
			sgs_PushString( C, "file" );
			sgs_PushString( C, file );
			
			sgs_CreateDict( C, NULL, 6 );
			
			p = p->next;
		}
		sgs_CreateArray( C, NULL, sgs_StackSize( C ) - sz );
	}
	return 1;
}

static int sgsstd_sys_msg( SGS_CTX )
{
	char* errmsg;
	sgs_Int errcode;
	
	SGSFN( "sys_msg" );
	
	if( !sgs_LoadArgs( C, "is", &errcode, &errmsg ) )
		return 0;
	
	SGSFN( NULL );
	
	sgs_Msg( C, (int) errcode, "%s", errmsg );
	return 0;
}

static int sgsstd__msgwrapper( SGS_CTX, const char* fn, int code )
{
	char* msg;
	SGSFN( fn );
	if( sgs_LoadArgs( C, "s", &msg ) )
	{
		SGSFN( NULL );
		sgs_Msg( C, code, "%s", msg );
	}
	return 0;
}

static int sgsstd_INFO( SGS_CTX ){ return sgsstd__msgwrapper( C, "INFO", SGS_INFO ); }
static int sgsstd_WARNING( SGS_CTX ){ return sgsstd__msgwrapper( C, "WARNING", SGS_WARNING ); }
static int sgsstd_ERROR( SGS_CTX ){ return sgsstd__msgwrapper( C, "ERROR", SGS_ERROR ); }

static int sgsstd_app_abort( SGS_CTX )
{
	SGSFN( "app_abort" );
	abort();
	return 0;
}
static int sgsstd_app_exit( SGS_CTX )
{
	sgs_Int ret = 0;
	
	SGSFN( "app_exit" );
	if( !sgs_LoadArgs( C, "|i", &ret ) )
		return 0;
	
	exit( (int) ret );
	return 0;
}

static int sgsstd_sys_replevel( SGS_CTX )
{
	int lev = C->minlev;
	
	SGSFN( "sys_replevel" );
	
	if( sgs_StackSize( C ) )
	{
		sgs_Int i;
		if( !sgs_LoadArgs( C, "i", &i ) )
			return 0;
		C->minlev = (int) i;
		return 0;
	}
	sgs_PushInt( C, lev );
	return 1;
}

static int sgsstd_sys_stat( SGS_CTX )
{
	sgs_Int type;
	
	SGSFN( "sys_stat" );
	
	if( !sgs_LoadArgs( C, "i", &type ) )
		return 0;
	
	sgs_PushInt( C, sgs_Stat( C, (int) type ) );
	return 1;
}

static int sgsstd_errno( SGS_CTX )
{
	int retstr = 0;
	
	SGSFN( "errno" );
	
	if( !sgs_LoadArgs( C, "|b", &retstr ) )
		return 0;
	
	if( retstr )
		sgs_PushString( C, strerror( C->last_errno ) );
	else
		sgs_PushInt( C, C->last_errno );
	
	return 1;
}

static int sgsstd_errno_string( SGS_CTX )
{
	sgs_Int e;
	
	SGSFN( "errno_string" );
	
	if( !sgs_LoadArgs( C, "i", &e ) )
		return 0;
	
	sgs_PushString( C, strerror( (int) e ) );
	
	return 1;
}


#define AE( x ) #x, (const char*) x
static const char* errno_key_table[] =
{
	AE( E2BIG ), AE( EACCES ), AE( EAGAIN ), AE( EBADF ), AE( EBUSY ),
	AE( ECHILD ), AE( EDEADLK ), AE( EDOM ), AE( EEXIST ), AE( EFAULT ),
	AE( EFBIG ), AE( EILSEQ ), AE( EINTR ), AE( EINVAL ), AE( EIO ),
	AE( EISDIR ), AE( EMFILE ), AE( EMLINK ), AE( ENAMETOOLONG ), AE( ENFILE ),
	AE( ENODEV ), AE( ENOENT ), AE( ENOEXEC ), AE( ENOLCK ), AE( ENOMEM ),
	AE( ENOSPC ), AE( ENOSYS ), AE( ENOTDIR ), AE( ENOTEMPTY ), AE( ENOTTY ),
	AE( ENXIO ), AE( EPERM ), AE( EPIPE ), AE( ERANGE ), AE( EROFS ),
	AE( ESPIPE ), AE( ESRCH ), AE( EXDEV ),
	
	NULL,
};
#undef AE

static int sgsstd_errno_value( SGS_CTX )
{
	const char** ekt = errno_key_table;
	char* str;
	
	SGSFN( "errno_value" );
	
	if( !sgs_LoadArgs( C, "s", &str ) )
		return 0;
	
	while( *ekt )
	{
		if( strcmp( *ekt, str ) == 0 )
		{
			sgs_PushInt( C, (int) (size_t) ekt[1] );
			return 1;
		}
		ekt += 2;
	}
	
	sgs_Msg( C, SGS_ERROR, "this errno value is unsupported" );
	return 0;
}

static int sgsstd_dumpvar( SGS_CTX )
{
	int i, ssz, rc = 0;
	ssz = sgs_StackSize( C );
	
	SGSFN( "dumpvar" );
	
	for( i = 0; i < ssz; ++i )
	{
		sgs_DumpVar( C, sgs_StackItem( C, i ), 5 );
		sgs_PushString( C, "\n" );
		rc += 2;
	}
	if( rc )
	{
		sgs_StringConcat( C, rc );
		return 1;
	}
	return 0;
}
static int sgsstd_dumpvar_ext( SGS_CTX )
{
	sgs_Int depth = 5;
	
	SGSFN( "dumpvar_ext" );
	
	if( !sgs_LoadArgs( C, ">|i.", &depth ) )
		return 0;
	
	sgs_DumpVar( C, sgs_StackItem( C, 0 ), (int) depth );
	return 1;
}

static int sgsstd_gc_collect( SGS_CTX )
{
	ptrdiff_t orvc;
	
	SGSFN( "gc_collect" );
	
	orvc = sgs_Stat( C, SGS_STAT_OBJCOUNT );
	sgs_GCExecute( C );
	sgs_PushInt( C, orvc - sgs_Stat( C, SGS_STAT_OBJCOUNT ) );
	return 1;
}


static int sgsstd_serialize_core( SGS_CTX, int which )
{
	sgs_Variable var;
	
	if( !sgs_LoadArgs( C, "v.", &var ) )
		return 0;
	
	if( which )
		sgs_SerializeV2( C, var );
	else
		sgs_SerializeV1( C, var );
	return 1;
}

static int sgsstd_serialize( SGS_CTX ){ SGSFN( "serialize" ); return sgsstd_serialize_core( C, 0 ); }
static int sgsstd_serialize2( SGS_CTX ){ SGSFN( "serialize2" ); return sgsstd_serialize_core( C, 1 ); }


static int check_arrayordict_fn( SGS_CTX, int argid, va_list args, int flags )
{
	uint32_t ty = sgs_ItemType( C, argid );
	if( ty != SGS_VT_OBJECT || (
		!sgs_IsObject( C, argid, sgsstd_array_iface ) &&
		!sgs_IsObject( C, argid, sgsstd_dict_iface ) ) )
		return sgs_ArgErrorExt( C, argid, 0, "array or dict", "" );
	return 1;
}

static int sgsstd_unserialize_core( SGS_CTX, int which )
{
	int ret;
	sgs_StkIdx ssz = sgs_StackSize( C ), dictpos;
	sgs_Variable env;
	
	if( !sgs_LoadArgs( C, "?s|x", check_arrayordict_fn ) )
		return 0;
	
	if( ssz >= 2 )
	{
		sgs_GetEnv( C, &env );
		if( sgs_IsObject( C, 1, sgsstd_array_iface ) )
		{
			dictpos = sgs_StackSize( C );
			sgs_CreateDict( C, NULL, 0 );
			sgs_PushIterator( C, sgs_StackItem( C, 1 ) );
			while( sgs_IterAdvance( C, sgs_StackItem( C, -1 ) ) > 0 )
			{
				sgs_IterPushData( C, sgs_StackItem( C, -1 ), 0, 1 );
				sgs_ToString( C, -1 );
				sgs_PushIndex( C, env, sgs_StackItem( C, -1 ), SGS_FALSE );
				sgs_SetIndex( C, sgs_StackItem( C, dictpos ), sgs_StackItem( C, -2 ), sgs_StackItem( C, -1 ), 0 );
				sgs_Pop( C, 2 ); /* pop name and value */
			}
			sgs_Pop( C, 1 ); /* pop iterator */
		}
		else
			dictpos = 1;
		
		sgs_SetEnv( C, sgs_StackItem( C, dictpos ) );
	}
	
	ret = which
		? sgs_UnserializeV2( C, sgs_StackItem( C, 0 ) )
		: sgs_UnserializeV1( C, sgs_StackItem( C, 0 ) );
	
	if( ssz >= 2 )
	{
		sgs_SetEnv( C, env );
		sgs_Release( C, &env );
	}
	
	return ret;
}

static int sgsstd_unserialize( SGS_CTX ){ SGSFN( "unserialize" ); return sgsstd_unserialize_core( C, 0 ); }
static int sgsstd_unserialize2( SGS_CTX ){ SGSFN( "unserialize2" ); return sgsstd_unserialize_core( C, 1 ); }


/* register all */
#ifndef STDLIB_FN
#  define STDLIB_FN( x ) { #x, sgsstd_##x }
#endif
static sgs_RegFuncConst regfuncs[] =
{
	/* containers */
	/* STDLIB_FN( array ), -- object */ STDLIB_FN( dict ), STDLIB_FN( map ), { "class", sgsstd_class },
	STDLIB_FN( array_filter ), STDLIB_FN( array_process ),
	STDLIB_FN( dict_filter ), STDLIB_FN( map_filter ), STDLIB_FN( map_process ),
	STDLIB_FN( dict_size ), STDLIB_FN( map_size ), STDLIB_FN( isset ), STDLIB_FN( unset ), STDLIB_FN( clone ),
	STDLIB_FN( get_keys ), STDLIB_FN( get_values ), STDLIB_FN( get_concat ),
	STDLIB_FN( get_merged ), STDLIB_FN( get_merged_map ),
	STDLIB_FN( get_iterator ), STDLIB_FN( iter_advance ), STDLIB_FN( iter_getdata ),
	/* types */
	STDLIB_FN( tobool ), STDLIB_FN( toint ), STDLIB_FN( toreal ), STDLIB_FN( tostring ), STDLIB_FN( toptr ),
	STDLIB_FN( parseint ), STDLIB_FN( parsereal ),
	STDLIB_FN( typeof ), STDLIB_FN( typeid ), STDLIB_FN( typeptr ), STDLIB_FN( typeptr_by_name ),
	STDLIB_FN( is_numeric ), STDLIB_FN( is_callable ),
	STDLIB_FN( is_array ), STDLIB_FN( is_dict ), STDLIB_FN( is_map ),
	/* I/O */
	STDLIB_FN( print ), STDLIB_FN( println ), STDLIB_FN( printlns ),
	STDLIB_FN( errprint ), STDLIB_FN( errprintln ), STDLIB_FN( errprintlns ),
	STDLIB_FN( printvar ), STDLIB_FN( printvar_ext ),
	STDLIB_FN( read_stdin ),
	/* OS */
	STDLIB_FN( ftime ),
	/* utils */
	STDLIB_FN( rand ), STDLIB_FN( randf ), STDLIB_FN( srand ), STDLIB_FN( hash_fnv ), STDLIB_FN( hash_crc32 ),
	/* internal utils */
	STDLIB_FN( va_get_args ), STDLIB_FN( va_get_arg ), STDLIB_FN( va_arg_count ),
	{ "sys_call", sgs_specfn_call }, { "sys_apply", sgs_specfn_apply },
	STDLIB_FN( metaobj_set ), STDLIB_FN( metaobj_get ), STDLIB_FN( metamethods_enable ), STDLIB_FN( metamethods_test ),
	STDLIB_FN( mm_getindex_router ), STDLIB_FN( mm_setindex_router ),
	STDLIB_FN( event ), STDLIB_FN( pooled_event ), STDLIB_FN( end_on ),
	STDLIB_FN( co_create ), STDLIB_FN( co_resume ),
	STDLIB_FN( thread_create ), STDLIB_FN( subthread_create ), STDLIB_FN( abort ),
	STDLIB_FN( process_threads ),
	STDLIB_FN( yield ),
	STDLIB_FN( pcall ), STDLIB_FN( assert ),
	STDLIB_FN( sym_register ), STDLIB_FN( sym_get ),
	STDLIB_FN( eval ), STDLIB_FN( eval_file ), STDLIB_FN( compile_sgs ),
	STDLIB_FN( include_library ), STDLIB_FN( include_file ),
	STDLIB_FN( include_shared ), STDLIB_FN( import_cfunc ),
	STDLIB_FN( include ),
	STDLIB_FN( sys_curfile ), STDLIB_FN( sys_curfiledir ), STDLIB_FN( sys_curprocfile ), STDLIB_FN( sys_curprocdir ),
	STDLIB_FN( multiply_path_ext_lists ),
	STDLIB_FN( sys_backtrace ), STDLIB_FN( sys_msg ), STDLIB_FN( INFO ), STDLIB_FN( WARNING ), STDLIB_FN( ERROR ),
	STDLIB_FN( app_abort ), STDLIB_FN( app_exit ),
	STDLIB_FN( sys_replevel ), STDLIB_FN( sys_stat ),
	STDLIB_FN( errno ), STDLIB_FN( errno_string ), STDLIB_FN( errno_value ),
	STDLIB_FN( dumpvar ), STDLIB_FN( dumpvar_ext ),
	STDLIB_FN( gc_collect ),
	STDLIB_FN( serialize ), STDLIB_FN( unserialize ),
	STDLIB_FN( serialize2 ), STDLIB_FN( unserialize2 ),
};

static const sgs_RegIntConst regiconsts[] =
{
	{ "SGS_INFO", SGS_INFO },
	{ "SGS_WARNING", SGS_WARNING },
	{ "SGS_ERROR", SGS_ERROR },
	{ "SGS_APIERR", SGS_APIERR },
	
	{ "VT_NULL", SGS_VT_NULL },
	{ "VT_BOOL", SGS_VT_BOOL },
	{ "VT_INT", SGS_VT_INT },
	{ "VT_REAL", SGS_VT_REAL },
	{ "VT_STRING", SGS_VT_STRING },
	{ "VT_FUNC", SGS_VT_FUNC },
	{ "VT_CFUNC", SGS_VT_CFUNC },
	{ "VT_OBJECT", SGS_VT_OBJECT },
	{ "VT_PTR", SGS_VT_PTR },
	{ "VT_THREAD", SGS_VT_THREAD },
	
	{ "RAND_MAX", RAND_MAX },
};


void sgsSTD_PostInit( SGS_CTX )
{
	sgs_PushEnv( C );
	sgs_RegSymbol( C, "", "_G", sgs_StackItem( C, -1 ) );
	sgs_Pop( C, 1 );
	
	sgs_RegIntConsts( C, regiconsts, SGS_ARRAY_SIZE( regiconsts ) );
	sgs_RegFuncConsts( C, regfuncs, SGS_ARRAY_SIZE( regfuncs ) );
	
	sgs_PushInterface( C, sgsstd_array_iface_gen );
	sgs_SetGlobalByName( C, "array", sgs_StackItem( C, -1 ) );
	sgs_RegSymbol( C, "", "array", sgs_StackItem( C, -1 ) );
	sgs_Pop( C, 1 );
	
	sgs_PushString( C, SGS_INCLUDE_PATH );
	sgs_SetGlobalByName( C, "SGS_PATH", sgs_StackItem( C, -1 ) );
	sgs_Pop( C, 1 );
	
	sgs_RegisterType( C, "array", sgsstd_array_iface );
	sgs_RegisterType( C, "array_iterator", sgsstd_array_iter_iface );
	sgs_RegisterType( C, "dict", sgsstd_dict_iface );
	sgs_RegisterType( C, "dict_iterator", sgsstd_dict_iter_iface );
	sgs_RegisterType( C, "map", sgsstd_map_iface );
	sgs_RegisterType( C, "closure", sgsstd_closure_iface );
}

SGSBOOL sgsSTD_MakeArray( SGS_CTX, sgs_Variable* out, sgs_SizeVal cnt )
{
	sgs_StkIdx i = 0, ssz = sgs_StackSize( C );
	sgs_BreakIf( out == NULL ); /* CreateObjectIPA modifies the stack otherwise */
	
	if( ssz < cnt )
	{
		sgs_Msg( C, SGS_APIERR, "sgs_CreateArray: not enough items on stack (need at least %d, got %d)",
			(int) cnt, (int) ssz );
		return SGS_FALSE;
	}
	else
	{
		sgs_Variable *p, *pend;
		void* data = sgs_Malloc( C, SGSARR_ALLOCSIZE( cnt ) );
		sgsstd_array_header_t* hdr = (sgsstd_array_header_t*) sgs_CreateObjectIPA( C,
			out, sizeof( sgsstd_array_header_t ), sgsstd_array_iface );
		
		hdr->size = cnt;
		hdr->mem = cnt;
		p = hdr->data = (sgs_Variable*) data;
		pend = p + cnt;
		while( p < pend )
			sgs_GetStackItem( C, i++ - cnt, p++ );
		
		sgs_Pop( C, cnt );
		
		sgs_PushInterface( C, sgsstd_array_iface_gen );
		sgs_ObjSetMetaObj( C, sgs_GetObjectStructP( out ), sgs_GetObjectStruct( C, -1 ) );
		sgs_Pop( C, 1 );
		
		return SGS_TRUE;
	}
}

SGSBOOL sgsSTD_MakeDict( SGS_CTX, sgs_Variable* out, sgs_SizeVal cnt )
{
	DictHdr* dh;
	sgs_VHTable* ht;
	sgs_StkIdx i, ssz = sgs_StackSize( C );
	
	if( cnt % 2 != 0 )
	{
		sgs_Msg( C, SGS_APIERR, "sgs_CreateDict: specified item count not even (multiple of 2 required, got %d)",
			(int) cnt );
		return SGS_FALSE;
	}
	if( cnt > ssz )
	{
		sgs_Msg( C, SGS_APIERR, "sgs_CreateDict: not enough items on stack (need at least %d, got %d)",
			(int) cnt, (int) ssz );
		return SGS_FALSE;
	}
	
	sgs_BreakIf( out == NULL );
	dh = mkdict( C, out );
	ht = &dh->ht;
	
	for( i = 0; i < cnt; i += 2 )
	{
		sgs_ToString( C, i - cnt );
		sgs_vht_set( ht, C, (C->stack_top+i-cnt), (C->stack_top+i+1-cnt) );
	}
	
	sgs_Pop( C, cnt );
	return SGS_TRUE;
}

SGSBOOL sgsSTD_MakeMap( SGS_CTX, sgs_Variable* out, sgs_SizeVal cnt )
{
	DictHdr* dh;
	sgs_VHTable* ht;
	sgs_StkIdx i, ssz = sgs_StackSize( C );
	
	if( cnt % 2 != 0 )
	{
		sgs_Msg( C, SGS_APIERR, "sgs_CreateMap: specified item count not even (multiple of 2 required, got %d)",
			(int) cnt );
		return SGS_FALSE;
	}
	if( cnt > ssz )
	{
		sgs_Msg( C, SGS_APIERR, "sgs_CreateMap: not enough items on stack (need at least %d, got %d)",
			(int) cnt, (int) ssz );
		return SGS_FALSE;
	}
	
	dh = mkmap( C, out );
	ht = &dh->ht;
	
	for( i = 0; i < cnt; i += 2 )
	{
		sgs_vht_set( ht, C, (C->stack_top+i-cnt), (C->stack_top+i+1-cnt) );
	}
	
	sgs_Pop( C, cnt );
	return SGS_TRUE;
}


#define RLBP S->_R
#define SYMP S->_SYM
#define INCP S->_INC

void sgsSTD_RegistryInit( SGS_CTX )
{
	SGS_SHCTX_USE;
	sgs_Variable var, subtbl, subkey;
	sgsSTD_MakeMap( C, &var, 0 );
	/* symbol table */
	{
		sgs_InitString( C, &subkey, "$sym" );
		sgsSTD_MakeMap( C, &subtbl, 0 );
		sgs_SetIndex( C, var, subkey, subtbl, SGS_TRUE );
		sgs_Release( C, &subkey );
		SYMP = subtbl.data.O;
	}
	/* include table */
	{
		sgs_InitString( C, &subkey, "$inc" );
		sgsSTD_MakeMap( C, &subtbl, 0 );
		sgs_SetIndex( C, var, subkey, subtbl, SGS_TRUE );
		sgs_Release( C, &subkey );
		INCP = subtbl.data.O;
	}
	RLBP = var.data.O;
}

void sgsSTD_RegistryFree( SGS_CTX )
{
	SGS_SHCTX_USE;
	
	/* include table */
	if( INCP )
	{
		sgs_ObjRelease( C, INCP );
		INCP = NULL;
	}
	
	/* symbol table */
	if( SYMP )
	{
		sgs_ObjRelease( C, SYMP );
		SYMP = NULL;
	}
	
	/* registry */
	if( RLBP )
	{
		sgs_ObjRelease( C, RLBP );
		RLBP = NULL;
	}
}

void sgsSTD_RegistryGC( SGS_CTX )
{
	SGS_SHCTX_USE;
	sgs_VarObj* obj = RLBP;
	if( obj )
	{
		sgs_ObjGCMark( C, obj );
	}
}

void sgsSTD_RegistryIter( SGS_CTX, int subtype, sgs_VHTVar** outp, sgs_VHTVar** outpend )
{
	SGS_SHCTX_USE;
	sgs_VarObj* obj = NULL;
	switch( subtype )
	{
	case SGS_REG_ROOT: obj = RLBP; break;
	case SGS_REG_SYM: obj = SYMP; break;
	case SGS_REG_INC: obj = INCP; break;
	}
	sgs_BreakIf( !obj );
	{
		HTHDR;
		*outp = ht->vars;
		*outpend = ht->vars + sgs_vht_size( ht );
	}
}


#define GLBP C->_G

void sgsSTD_GlobalInit( SGS_CTX )
{
	sgs_Variable var;
	sgsSTD_MakeMap( C, &var, 0 );
	GLBP = var.data.O;
}

void sgsSTD_GlobalFree( SGS_CTX )
{
	sgs_ObjRelease( C, GLBP );
	GLBP = NULL;
}

SGSBOOL sgsSTD_GlobalGet( SGS_CTX, sgs_Variable* out, sgs_Variable* idx )
{
	const char* name;
	sgs_VHTVar* pair;
	sgs_VarObj* obj = GLBP;
	HTHDR;
	
	/* `out` is expected to point at an initialized variable, which could be same as `idx` */
	
	if( idx->type != SGS_VT_STRING )
		return SGS_FALSE;
	
	name = sgs_var_cstr( idx );
	
	if( strcmp( name, "_G" ) == 0 )
	{
		sgs_Variable tmp;
		sgs_InitObjectPtr( &tmp, obj );
		sgs_Release( C, out );
		*out = tmp;
		return SGS_TRUE;
	}
	
	if( strcmp( name, "_R" ) == 0 )
	{
		SGS_SHCTX_USE;
		sgs_Release( C, out );
		sgs_InitObjectPtr( out, RLBP );
		return SGS_TRUE;
	}
	
	if( strcmp( name, "_T" ) == 0 )
	{
		sgs_Variable tmp;
		sgs_InitThreadPtr( &tmp, C );
		sgs_Release( C, out );
		*out = tmp;
		return SGS_TRUE;
	}
	
	if( strcmp( name, "_F" ) == 0 )
	{
		sgs_Variable tmp = sgs_MakeNull();
		if( C->sf_last )
		{
			tmp = C->sf_last->func;
			sgs_Acquire( C, &tmp );
		}
		sgs_Release( C, out );
		*out = tmp;
		return SGS_TRUE;
	}
	
	if( obj->mm_enable )
	{
		int ret;
		sgs_Variable obv, tmp;
		obv.type = SGS_VT_OBJECT;
		obv.data.O = obj;
		ret = sgs_GetIndex( C, obv, *idx, &tmp, 0 );
		sgs_Release( C, out );
		*out = tmp;
		return ret;
	}
	
	if( ( pair = sgs_vht_get( ht, idx ) ) != NULL )
	{
		sgs_Release( C, out );
		*out = pair->val;
		sgs_Acquire( C, out );
		return SGS_TRUE;
	}
	
	sgs_Msg( C, SGS_WARNING, "variable '%s' was not found", sgs_str_cstr( idx->data.S ) );
	sgs_Release( C, out );
	return SGS_FALSE;
}

SGSBOOL sgsSTD_GlobalSet( SGS_CTX, sgs_Variable* idx, sgs_Variable* val )
{
	const char* name;
	sgs_VarObj* obj = GLBP;
	HTHDR;
	
	if( idx->type != SGS_VT_STRING )
		return SGS_FALSE;
	
	name = sgs_var_cstr( idx );
	
	if( strcmp( name, "_G" ) == 0 )
	{
		if( val->type != SGS_VT_OBJECT ||
			( val->data.O->iface != sgsstd_dict_iface && val->data.O->iface != sgsstd_map_iface ) )
		{
			sgs_Msg( C, SGS_ERROR, "_G only accepts 'map'/'dict' values" );
			return SGS_FALSE;
		}
		sgs_SetEnv( C, *val );
		return SGS_TRUE;
	}
	
	if( strcmp( name, "_R" ) == 0 ||
		strcmp( name, "_T" ) == 0 ||
		strcmp( name, "_F" ) == 0 )
	{
		sgs_Msg( C, SGS_WARNING, "cannot change %s", name );
		return SGS_FALSE;
	}
	
	if( obj->mm_enable )
	{
		sgs_Variable obv;
		obv.type = SGS_VT_OBJECT;
		obv.data.O = obj;
		return sgs_SetIndex( C, obv, *idx, *val, 0 );
	}
	
	sgs_vht_set( ht, C, idx, val );
	return SGS_TRUE;
}

void sgsSTD_GlobalGC( SGS_CTX )
{
	sgs_VarObj* obj = GLBP;
	if( obj )
	{
		sgs_ObjGCMark( C, obj );
	}
}

void sgsSTD_GlobalIter( SGS_CTX, sgs_VHTVar** outp, sgs_VHTVar** outpend )
{
	sgs_VarObj* obj = GLBP;
	sgs_BreakIf( !obj );
	{
		HTHDR;
		*outp = ht->vars;
		*outpend = ht->vars + sgs_vht_size( ht );
	}
}



void sgs_RegSymbol( SGS_CTX, const char* prefix, const char* name, sgs_Variable var )
{
	sgs_Variable str, symtbl = sgs_Registry( C, SGS_REG_SYM );
	
	if( prefix == NULL )
		prefix = "";
	if( name == NULL )
		name = "";
	sgs_BreakIf( *prefix == '\0' && *name == '\0' );
	
	if( *prefix )
		sgs_PushString( C, prefix );
	if( *name )
		sgs_PushString( C, name );
	if( *prefix && *name )
		sgs_StringConcat( C, 2 );
	
	str = sgs_StackItem( C, -1 );
	sgs_SetIndex( C, symtbl, str, var, SGS_FALSE );
	sgs_SetIndex( C, symtbl, var, str, SGS_FALSE );
	sgs_Pop( C, 1 );
}

SGSBOOL sgs_GetSymbol( SGS_CTX, sgs_Variable key, sgs_Variable* out )
{
	sgs_Variable symtbl = sgs_Registry( C, SGS_REG_SYM );
	return sgs_GetIndex( C, symtbl, key, out, SGS_FALSE );
}

void sgs_RegFuncConstsExt( SGS_CTX, const sgs_RegFuncConst* list, int size, const char* prefix )
{
	while( size-- )
	{
		sgs_Variable v_func;
		if( !list->name )
			break;
		v_func = sgs_MakeCFunc( list->value );
		sgs_SetGlobalByName( C, list->name, v_func );
		/* put into symbol table */
		if( prefix )
			sgs_RegSymbol( C, prefix, list->name, v_func );
		list++;
	}
}

void sgs_RegIntConsts( SGS_CTX, const sgs_RegIntConst* list, int size )
{
	while( size-- )
	{
		if( !list->name )
			break;
		sgs_SetGlobalByName( C, list->name, sgs_MakeInt( list->value ) );
		list++;
	}
}

void sgs_RegRealConsts( SGS_CTX, const sgs_RegRealConst* list, int size )
{
	while( size-- )
	{
		if( !list->name )
			break;
		sgs_SetGlobalByName( C, list->name, sgs_MakeReal( list->value ) );
		list++;
	}
}


void sgs_StoreFuncConsts( SGS_CTX, sgs_Variable var, const sgs_RegFuncConst* list, int size, const char* prefix )
{
	sgs_Variable key;
	while( size-- )
	{
		sgs_Variable v_func;
		if( !list->name )
			break;
		v_func = sgs_MakeCFunc( list->value );
		sgs_InitString( C, &key, list->name );
		sgs_SetIndex( C, var, key, v_func, 1 );
		sgs_Release( C, &key );
		if( prefix )
			sgs_RegSymbol( C, prefix, list->name, v_func );
		list++;
	}
}

void sgs_StoreIntConsts( SGS_CTX, sgs_Variable var, const sgs_RegIntConst* list, int size )
{
	sgs_Variable key;
	while( size-- )
	{
		if( !list->name )
			break;
		sgs_InitString( C, &key, list->name );
		sgs_SetIndex( C, var, key, sgs_MakeInt( list->value ), 1 );
		sgs_Release( C, &key );
		list++;
	}
}

void sgs_StoreRealConsts( SGS_CTX, sgs_Variable var, const sgs_RegRealConst* list, int size )
{
	sgs_Variable key;
	while( size-- )
	{
		if( !list->name )
			break;
		sgs_InitString( C, &key, list->name );
		sgs_SetIndex( C, var, key, sgs_MakeReal( list->value ), 1 );
		sgs_Release( C, &key );
		list++;
	}
}


SGSBOOL sgs_IncludeExt( SGS_CTX, const char* name, const char* searchpath )
{
	int ret = 0, sz, sz0;
	int pathrep = 0;
	
	sz0 = sgs_StackSize( C );
	if( searchpath )
	{
		pathrep = sgs_PushGlobalByName( C, "SGS_PATH" ) ? 1 : 2;
		sgs_PushString( C, searchpath );
		sgs_SetGlobalByName( C, "SGS_PATH", sgs_StackItem( C, -1 ) );
		sgs_Pop( C, 1 );
	}
	
	sz = sgs_StackSize( C );
	sgs_PushString( C, name );
	sgs_Call( C, sgs_MakeCFunc( sgsstd_include ), 1, 1 );
	ret = sgs_GetBool( C, -1 );
	sgs_SetStackSize( C, sz );
	
	if( pathrep == 1 )
		sgs_SetGlobalByName( C, "SGS_PATH", sgs_StackItem( C, -1 ) );
	else if( pathrep == 2 )
	{
		sgs_PushEnv( C );
		sgs_PushString( C, "SGS_PATH" );
		sgs_Unset( C, sgs_StackItem( C, -2 ), sgs_StackItem( C, -1 ) );
	}
	
	sgs_SetStackSize( C, sz0 );
	return ret;
}


SGSBOOL sgs_IsArray( sgs_Variable var )
{
	return sgs_IsObjectP( &var, sgsstd_array_iface );
}

sgs_SizeVal sgs_ArraySize( sgs_Variable var )
{
	if( !sgs_IsObjectP( &var, sgsstd_array_iface ) )
		return -1; /* no error */
	return ((sgsstd_array_header_t*)sgs_GetObjectDataP( &var ))->size;
}

void sgs_ArrayPush( SGS_CTX, sgs_Variable var, sgs_StkIdx count )
{
	if( !sgs_IsObjectP( &var, sgsstd_array_iface ) )
	{
		sgs_Msg( C, SGS_APIERR, "sgs_ArrayPush: variable is not an array" );
		return;
	}
	if( sgs_StackSize( C ) < count )
	{
		sgs_Msg( C, SGS_APIERR, "sgs_ArrayPush: too few items on stack "
			"(need: %d, got: %d)", (int) count, (int) sgs_StackSize( C ) );
		return;
	}
	if( count )
	{
		sgsstd_array_header_t* hdr = (sgsstd_array_header_t*) sgs_GetObjectDataP( &var );
		sgsstd_array_insert( C, hdr, hdr->size, sgs_StackSize( C ) - count );
		sgs_Pop( C, count );
	}
}

void sgs_ArrayPop( SGS_CTX, sgs_Variable var, sgs_StkIdx count, SGSBOOL ret )
{
	if( !sgs_IsObjectP( &var, sgsstd_array_iface ) )
	{
		sgs_Msg( C, SGS_APIERR, "sgs_ArrayPush: variable is not an array" );
		return;
	}
	if( count )
	{
		sgsstd_array_header_t* hdr = (sgsstd_array_header_t*) sgs_GetObjectDataP( &var );
		if( hdr->size < count )
		{
			sgs_Msg( C, SGS_APIERR, "sgs_ArrayPush: too few items on stack "
				"(need: %d, got: %d)", (int) count, (int) sgs_StackSize( C ) );
			return;
		}
		if( ret )
		{
			sgs_SizeVal i;
			for( i = hdr->size - count; i < hdr->size; ++i )
				sgs_PushVariable( C, SGSARR_PTR( hdr )[ i ] );
		}
		sgsstd_array_erase( C, hdr, hdr->size - count, hdr->size - 1 );
	}
}

void sgs_ArrayErase( SGS_CTX, sgs_Variable var, sgs_StkIdx at, sgs_StkIdx count )
{
	if( !sgs_IsObjectP( &var, sgsstd_array_iface ) )
	{
		sgs_Msg( C, SGS_APIERR, "sgs_ArrayErase: variable is not an array" );
		return;
	}
	if( count )
	{
		sgsstd_array_header_t* hdr = (sgsstd_array_header_t*) sgs_GetObjectDataP( &var );
		if( at < 0 || at > hdr->size || at + count > hdr->size )
		{
			sgs_Msg( C, SGS_APIERR, "sgs_ArrayErase: invalid range (erasing: %d - %d, size: %d)",
				(int) at, (int) ( at + count - 1 ), (int) hdr->size );
			return;
		}
		sgsstd_array_erase( C, hdr, at, at + count - 1 );
	}
}

sgs_SizeVal sgs_ArrayFind( SGS_CTX, sgs_Variable var, sgs_Variable what )
{
	if( !sgs_IsObjectP( &var, sgsstd_array_iface ) )
	{
		sgs_Msg( C, SGS_APIERR, "sgs_ArrayFind: variable is not an array" );
		return -1;
	}
	//
	{
		sgs_SizeVal off = 0;
		sgsstd_array_header_t* hdr = (sgsstd_array_header_t*) sgs_GetObjectDataP( &var );
		while( off < hdr->size )
		{
			sgs_Variable* p = SGSARR_PTR( hdr ) + off;
			if( sgs_EqualTypes( p, &what ) &&
				sgs_Compare( C, p, &what ) == 0 )
			{
				return off;
			}
			off++;
		}
		return -1;
	}
}

sgs_SizeVal sgs_ArrayRemove( SGS_CTX, sgs_Variable var, sgs_Variable what, SGSBOOL all )
{
	if( !sgs_IsObjectP( &var, sgsstd_array_iface ) )
	{
		sgs_Msg( C, SGS_APIERR, "sgs_ArrayRemove: variable is not an array" );
		return 0;
	}
	//
	{
		sgs_SizeVal off = 0, rmvd = 0;
		sgsstd_array_header_t* hdr = (sgsstd_array_header_t*) sgs_GetObjectDataP( &var );
		while( off < hdr->size )
		{
			sgs_Variable* p = SGSARR_PTR( hdr ) + off;
			if( sgs_EqualTypes( p, &what ) &&
				sgs_Compare( C, p, &what ) == 0 )
			{
				sgsstd_array_erase( C, hdr, off, off );
				rmvd++;
				if( all == SGS_FALSE )
					break;
			}
			off++;
		}
		return rmvd;
	}
}

SGSBOOL sgs_IsDict( sgs_Variable var )
{
	return sgs_IsObjectP( &var, sgsstd_dict_iface );
}

SGSBOOL sgs_IsMap( sgs_Variable var )
{
	return sgs_IsObjectP( &var, sgsstd_map_iface );
}

SGSBOOL sgs_Unset( SGS_CTX, sgs_Variable var, sgs_Variable key )
{
	if( !sgs_IsObjectP( &var, sgsstd_dict_iface ) &&
		!sgs_IsObjectP( &var, sgsstd_map_iface ) )
	{
		sgs_Msg( C, SGS_APIERR, "sgs_Unset: variable is not dict/map" );
		return SGS_FALSE;
	}
	//
	{
		sgs_VHTable* T = &((DictHdr*)sgs_GetObjectDataP( &var ))->ht;
		sgs_SizeVal sz = T->size;
		sgs_vht_unset( T, C, &key );
		return T->size < sz;
	}
}

