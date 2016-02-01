

#include <stdio.h>

#include "sgsxgmath.h"


#define XGM_WARNING( err ) sgs_Msg( C, SGS_WARNING, err )
#define XGM_OHDR XGM_VT* hdr = (XGM_VT*) obj->data;
#define XGM_FLAHDR xgm_vtarray* flarr = (xgm_vtarray*) obj->data;

#define XGM_V3_ADD( o, a, b ) (o)[0] = (a)[0] + (b)[0]; (o)[1] = (a)[1] + (b)[1]; (o)[2] = (a)[2] + (b)[2];
#define XGM_V3_SUB( o, a, b ) (o)[0] = (a)[0] - (b)[0]; (o)[1] = (a)[1] - (b)[1]; (o)[2] = (a)[2] - (b)[2];
#define XGM_V3_SCALE( v, f ) (v)[0] *= f; (v)[1] *= f; (v)[2] *= f;
#define XGM_V3_SCALETO( o, a, f ) (o)[0] = (a)[0] * f; (o)[1] = (a)[1] * f; (o)[2] = (a)[2] * f;
#define XGM_V3_CROSS( o, a, b ) (o)[0] = (a)[1] * (b)[2] - (a)[2] * (b)[1]; \
		(o)[1] = (a)[2] * (b)[0] - (a)[0] * (b)[2]; (o)[2] = (a)[0] * (b)[1] - (a)[1] * (b)[0];

#define XGM_VMUL_INNER2(a,b) ((a)[0]*(b)[0]+(a)[1]*(b)[1])
#define XGM_VMUL_INNER3(a,b) ((a)[0]*(b)[0]+(a)[1]*(b)[1]+(a)[2]*(b)[2])
#define XGM_VMUL_INNER4(a,b) ((a)[0]*(b)[0]+(a)[1]*(b)[1]+(a)[2]*(b)[2]+(a)[3]*(b)[3])

#define XGM_SET2( v, a, b ) (v)[0] = a; (v)[1] = b;
#define XGM_SET3( v, a, b, c ) XGM_SET2( v, a, b ); (v)[2] = c;
#define XGM_SET4( v, a, b, c, d ) XGM_SET3( v, a, b, c ); (v)[3] = d;

#define XGM_COPY2( t, f ) (t)[0] = (f)[0]; (t)[1] = (f)[1];
#define XGM_COPY3( t, f ) XGM_COPY2( t, f ); (t)[2] = (f)[2];
#define XGM_COPY4( t, f ) XGM_COPY3( t, f ); (t)[3] = (f)[3];
#define XGM_COPY6( t, f ) XGM_COPY4( t, f ); (t)[4] = (f)[4]; (t)[5] = (f)[5];

#define XGM_BB2_EXPAND_V2(b,v) \
	if( (b)[0] > (v)[0] ) (b)[0] = (v)[0]; \
	if( (b)[1] > (v)[1] ) (b)[1] = (v)[1]; \
	if( (b)[2] < (v)[0] ) (b)[2] = (v)[0]; \
	if( (b)[3] < (v)[1] ) (b)[3] = (v)[1];

#define XGM_BB3_EXPAND_V3(b,v) \
	if( (b)[0] > (v)[0] ) (b)[0] = (v)[0]; \
	if( (b)[1] > (v)[1] ) (b)[1] = (v)[1]; \
	if( (b)[2] > (v)[2] ) (b)[2] = (v)[2]; \
	if( (b)[3] < (v)[0] ) (b)[3] = (v)[0]; \
	if( (b)[4] < (v)[1] ) (b)[4] = (v)[1]; \
	if( (b)[5] < (v)[1] ) (b)[5] = (v)[2];


SGSBOOL sgs_ParseVT( SGS_CTX, sgs_StkIdx item, XGM_VT* out )
{
	sgs_Real val;
	if( sgs_ParseReal( C, item, &val ) )
	{
		*out = (XGM_VT) val;
		return 1;
	}
	return 0;
}


/*  2 D   V E C T O R  */

#define VEC2_IFN( fn ) \
	int is_method = sgs_Method( C ); \
	sgs_FuncName( C, is_method ? "vec2." #fn : "vec2_" #fn ); \
	if( !sgs_IsObject( C, 0, xgm_vec2_iface ) ) \
		return sgs_ArgErrorExt( C, 0, is_method, "vec2", "" ); \
	XGM_VT* data = (XGM_VT*) sgs_GetObjectData( C, 0 );

static int xgm_v2m_rotate( SGS_CTX )
{
	XGM_VT angle, s, c;
	
	VEC2_IFN( fn );
	
	if( !sgs_LoadArgs( C, "@>f", &angle ) )
		return 0;
	
	c = (XGM_VT) cos( angle );
	s = (XGM_VT) sin( angle );
	sgs_CreateVec2( C, NULL, data[0] * c - data[1] * s, data[0] * s + data[1] * c );
	return 1;
}

static int xgm_v2_convert( SGS_CTX, sgs_VarObj* obj, int type )
{
	XGM_OHDR;
	if( type == SGS_CONVOP_CLONE )
	{
		sgs_CreateVec2p( C, NULL, hdr );
		return SGS_SUCCESS;
	}
	else if( type == SGS_VT_STRING )
	{
		char buf[ 128 ];
		sprintf( buf, "vec2(%g;%g)", hdr[0], hdr[1] );
		sgs_PushString( C, buf );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_v2_getindex( SGS_ARGS_GETINDEXFUNC )
{
	char* str;
	XGM_OHDR;
	
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT )
	{
		sgs_Int pos = sgs_GetInt( C, 0 );
		if( pos != 0 && pos != 1 )
			return SGS_ENOTFND;
		sgs_PushReal( C, hdr[ pos ] );
		return SGS_SUCCESS;
	}
	
	if( !sgs_ParseString( C, 0, &str, NULL ) )
		return SGS_EINVAL;
	if( !strcmp( str, "x" ) ) return sgs_PushReal( C, hdr[ 0 ] );
	if( !strcmp( str, "y" ) ) return sgs_PushReal( C, hdr[ 1 ] );
	if( !strcmp( str, "length" ) )
	{
		sgs_PushReal( C, sqrt( XGM_VMUL_INNER2( hdr, hdr ) ) );
		return SGS_SUCCESS;
	}
	if( !strcmp( str, "length_squared" ) )
	{
		sgs_PushReal( C, XGM_VMUL_INNER2( hdr, hdr ) );
		return SGS_SUCCESS;
	}
	if( !strcmp( str, "normalized" ) )
	{
		XGM_VT lensq = XGM_VMUL_INNER2( hdr, hdr );
		if( lensq != 0.0f )
		{
			lensq = (XGM_VT) 1.0 / (XGM_VT) sqrt( lensq );
			sgs_CreateVec2( C, NULL, hdr[0] * lensq, hdr[1] * lensq );
		}
		else
			sgs_CreateVec2( C, NULL, 0, 0 );
		return SGS_SUCCESS;
	}
	if( !strcmp( str, "angle" ) ) return sgs_PushReal( C, atan2( hdr[1], hdr[0] ) );
	if( !strcmp( str, "perp" ) ) return sgs_CreateVec2( C, NULL, -hdr[1], hdr[0] );
	if( !strcmp( str, "perp2" ) ) return sgs_CreateVec2( C, NULL, hdr[1], -hdr[0] );
	if( !strcmp( str, "rotate" ) ) return sgs_PushCFunc( C, xgm_v2m_rotate );
	if( !strcmp( str, "size" ) ) return sgs_PushInt( C, 2 );
	return SGS_ENOTFND;
}

static int xgm_v2_setindex( SGS_ARGS_SETINDEXFUNC )
{
	XGM_VT* hdr = (XGM_VT*) obj->data;
	
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT )
	{
		sgs_Real vv;
		if( !sgs_ParseReal( C, 1, &vv ) )
			return SGS_EINVAL;
		sgs_Int pos = sgs_GetInt( C, 0 );
		if( pos != 0 && pos != 1 )
			return SGS_ENOTFND;
		hdr[ pos ] = (XGM_VT) vv;
		return SGS_SUCCESS;
	}
	else
	{
		sgs_Real vv;
		char* str;
		if( !sgs_ParseString( C, 0, &str, NULL ) )
			return SGS_EINVAL;
		SGS_CASE( "x" )
		{
			if( !sgs_ParseReal( C, 1, &vv ) )
				return SGS_EINVAL;
			hdr[0] = (XGM_VT) vv;
			return SGS_SUCCESS;
		}
		SGS_CASE( "y" )
		{
			if( !sgs_ParseReal( C, 1, &vv ) )
				return SGS_EINVAL;
			hdr[1] = (XGM_VT) vv;
			return SGS_SUCCESS;
		}
		SGS_CASE( "angle" )
		{
			if( !sgs_ParseReal( C, 1, &vv ) )
				return SGS_EINVAL;
			XGM_VT curang = (XGM_VT) atan2( hdr[1], hdr[0] );
			XGM_VT c = (XGM_VT) cos( (XGM_VT) vv - curang );
			XGM_VT s = (XGM_VT) sin( (XGM_VT) vv - curang );
			XGM_VT x = hdr[0] * c - hdr[1] * s,
			       y = hdr[0] * s + hdr[1] * c;
			hdr[0] = x; hdr[1] = y;
			return SGS_SUCCESS;
		}
	}
	return SGS_ENOTFND;
}

static int xgm_v2_expr( SGS_ARGS_OBJFUNC )
{
	int type = sgs_ObjectArg( C );
	if( type == SGS_EOP_ADD ||
		type == SGS_EOP_SUB ||
		type == SGS_EOP_MUL ||
		type == SGS_EOP_DIV ||
		type == SGS_EOP_MOD )
	{
		XGM_VT r[ 2 ], v1[ 2 ], v2[ 2 ];
		if( !sgs_ParseVec2( C, 0, v1, 0 ) || !sgs_ParseVec2( C, 1, v2, 0 ) )
			return SGS_EINVAL;
		
		if( ( type == SGS_EOP_DIV || type == SGS_EOP_MOD ) &&
			( v2[0] == 0 || v2[1] == 0 ) )
		{
			const char* errstr = type == SGS_EOP_DIV ?
				"vec2 operator '/' - division by zero" :
				"vec2 operator '%' - modulo by zero";
			sgs_Msg( C, SGS_ERROR, errstr );
			return SGS_EINPROC;
		}
		
		if( type == SGS_EOP_ADD )
			{ r[0] = v1[0] + v2[0]; r[1] = v1[1] + v2[1]; }
		else if( type == SGS_EOP_SUB )
			{ r[0] = v1[0] - v2[0]; r[1] = v1[1] - v2[1]; }
		else if( type == SGS_EOP_MUL )
			{ r[0] = v1[0] * v2[0]; r[1] = v1[1] * v2[1]; }
		else if( type == SGS_EOP_DIV )
			{ r[0] = v1[0] / v2[0]; r[1] = v1[1] / v2[1]; }
		else
			{ r[0] = (XGM_VT) fmod( v1[0], v2[0] ); r[1] = (XGM_VT) fmod( v1[1], v2[1] ); }
		
		sgs_CreateVec2p( C, NULL, r );
		return SGS_SUCCESS;
	}
	else if( type == SGS_EOP_COMPARE )
	{
		XGM_VT *v1, *v2;
		if( !sgs_IsObject( C, 0, xgm_vec2_iface ) ||
			!sgs_IsObject( C, 1, xgm_vec2_iface ) )
			return SGS_EINVAL;
		
		v1 = (XGM_VT*) sgs_GetObjectData( C, 0 );
		v2 = (XGM_VT*) sgs_GetObjectData( C, 1 );
		
		if( v1[0] != v2[0] )
			sgs_PushReal( C, v1[0] - v2[0] );
		else
			sgs_PushReal( C, v1[1] - v2[1] );
		return SGS_SUCCESS;
	}
	else if( type == SGS_EOP_NEGATE )
	{
		XGM_OHDR;
		sgs_CreateVec2( C, NULL, -hdr[0], -hdr[1] );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_v2_serialize( SGS_CTX, sgs_VarObj* obj )
{
	XGM_OHDR;
	sgs_Serialize( C, sgs_MakeReal( hdr[0] ) );
	sgs_Serialize( C, sgs_MakeReal( hdr[1] ) );
	sgs_SerializeObject( C, 2, "vec2" );
	return SGS_SUCCESS;
}

static int xgm_v2_dump( SGS_CTX, sgs_VarObj* obj, int maxdepth )
{
	SGS_UNUSED( maxdepth );
	return xgm_v2_convert( C, obj, SGS_VT_STRING );
}

static int xgm_vec2( SGS_CTX )
{
	int argc = sgs_StackSize( C );
	uint32_t ty0 = sgs_ItemType( C, 0 );
	XGM_VT v[ 2 ] = { 0, 0 };
	
	SGSFN( "vec2" );
	
	if( ty0 != SGS_VT_INT && ty0 != SGS_VT_REAL && sgs_ParseVec2( C, 0, v, 0 ) )
	{
		sgs_CreateVec2p( C, NULL, v );
		return 1;
	}
	
	if( !sgs_LoadArgs( C, "f|f.", v, v + 1 ) )
		return 0;
	
	if( argc == 1 )
		v[1] = v[0];
	
	sgs_CreateVec2p( C, NULL, v );
	return 1;
}

static int xgm_vec2_dot( SGS_CTX )
{
	XGM_VT v1[2], v2[2];
	
	SGSFN( "vec2_dot" );
	
	if( !sgs_LoadArgs( C, "!x!x", sgs_ArgCheck_Vec2, v1, sgs_ArgCheck_Vec2, v2 ) )
		return 0;
	
	sgs_PushReal( C, XGM_VMUL_INNER2( v1, v2 ) );
	return 1;
}



/*  3 D   V E C T O R  */

static int xgm_v3_convert( SGS_CTX, sgs_VarObj* obj, int type )
{
	XGM_OHDR;
	if( type == SGS_CONVOP_CLONE )
	{
		sgs_CreateVec3p( C, NULL, hdr );
		return SGS_SUCCESS;
	}
	else if( type == SGS_VT_STRING )
	{
		char buf[ 192 ];
		sprintf( buf, "vec3(%g;%g;%g)", hdr[0], hdr[1], hdr[2] );
		sgs_PushString( C, buf );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_v3_getindex( SGS_ARGS_GETINDEXFUNC )
{
	char* str;
	sgs_SizeVal size;
	XGM_OHDR;
	
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT )
	{
		sgs_Int pos = sgs_GetInt( C, 0 );
		if( pos != 0 && pos != 1 && pos != 2 )
			return SGS_ENOTFND;
		sgs_PushReal( C, hdr[ pos ] );
		return SGS_SUCCESS;
	}
	
	if( sgs_ParseString( C, 0, &str, &size ) )
	{
		if( !strcmp( str, "x" ) ){ sgs_PushReal( C, hdr[ 0 ] ); return SGS_SUCCESS; }
		if( !strcmp( str, "y" ) ){ sgs_PushReal( C, hdr[ 1 ] ); return SGS_SUCCESS; }
		if( !strcmp( str, "z" ) ){ sgs_PushReal( C, hdr[ 2 ] ); return SGS_SUCCESS; }
		if( !strcmp( str, "length" ) )
		{
			sgs_PushReal( C, sqrt( XGM_VMUL_INNER3( hdr, hdr ) ) );
			return SGS_SUCCESS;
		}
		if( !strcmp( str, "length_squared" ) )
		{
			sgs_PushReal( C, XGM_VMUL_INNER3( hdr, hdr ) );
			return SGS_SUCCESS;
		}
		if( !strcmp( str, "normalized" ) )
		{
			XGM_VT lensq = XGM_VMUL_INNER3( hdr, hdr );
			if( lensq != 0.0f )
			{
				lensq = (XGM_VT) 1.0 / (XGM_VT) sqrt( lensq );
				sgs_CreateVec3( C, NULL, hdr[0] * lensq, hdr[1] * lensq, hdr[2] * lensq );
			}
			else
				sgs_CreateVec3( C, NULL, 0, 0, 0 );
			return SGS_SUCCESS;
		}
		if( !strcmp( str, "size" ) ){ sgs_PushInt( C, 3 ); return SGS_SUCCESS; }
	}
	return SGS_ENOTFND;
}

static int xgm_v3_setindex( SGS_ARGS_SETINDEXFUNC )
{
	sgs_Real val;
	XGM_OHDR;
	
	if( !sgs_ParseReal( C, 1, &val ) )
		return SGS_EINVAL;
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT )
	{
		sgs_Int pos = sgs_GetInt( C, 0 );
		if( pos != 0 && pos != 1 && pos != 2 )
			return SGS_ENOTFND;
		hdr[ pos ] = (XGM_VT) val;
		return SGS_SUCCESS;
	}
	else
	{
		char* str;
		sgs_SizeVal size;
		if( sgs_ParseString( C, 0, &str, &size ) )
		{
			if( !strcmp( str, "x" ) ){ hdr[0] = (XGM_VT) val; return SGS_SUCCESS; }
			if( !strcmp( str, "y" ) ){ hdr[1] = (XGM_VT) val; return SGS_SUCCESS; }
			if( !strcmp( str, "z" ) ){ hdr[2] = (XGM_VT) val; return SGS_SUCCESS; }
		}
	}
	return SGS_ENOTFND;
}

static int xgm_v3_expr( SGS_ARGS_OBJFUNC )
{
	int type = sgs_ObjectArg( C );
	if( type == SGS_EOP_ADD ||
		type == SGS_EOP_SUB ||
		type == SGS_EOP_MUL ||
		type == SGS_EOP_DIV ||
		type == SGS_EOP_MOD )
	{
		XGM_VT r[3], v1[3], v2[3];
		if( !sgs_ParseVec3( C, 0, v1, 0 ) || !sgs_ParseVec3( C, 1, v2, 0 ) )
			return SGS_EINVAL;
		
		if( ( type == SGS_EOP_DIV || type == SGS_EOP_MOD ) &&
			( v2[0] == 0 || v2[1] == 0 || v2[2] == 0 ) )
		{
			const char* errstr = type == SGS_EOP_DIV ?
				"vec3 operator '/' - division by zero" :
				"vec3 operator '%' - modulo by zero";
			sgs_Msg( C, SGS_ERROR, errstr );
			return SGS_EINPROC;
		}
		
		if( type == SGS_EOP_ADD )
			{ r[0] = v1[0] + v2[0]; r[1] = v1[1] + v2[1]; r[2] = v1[2] + v2[2]; }
		else if( type == SGS_EOP_SUB )
			{ r[0] = v1[0] - v2[0]; r[1] = v1[1] - v2[1]; r[2] = v1[2] - v2[2]; }
		else if( type == SGS_EOP_MUL )
			{ r[0] = v1[0] * v2[0]; r[1] = v1[1] * v2[1]; r[2] = v1[2] * v2[2]; }
		else if( type == SGS_EOP_DIV )
			{ r[0] = v1[0] / v2[0]; r[1] = v1[1] / v2[1]; r[2] = v1[2] / v2[2]; }
		else
		{
			r[0] = (XGM_VT) fmod( v1[0], v2[0] );
			r[1] = (XGM_VT) fmod( v1[1], v2[1] );
			r[2] = (XGM_VT) fmod( v1[2], v2[2] );
		}
		
		sgs_CreateVec3p( C, NULL, r );
		return SGS_SUCCESS;
	}
	else if( type == SGS_EOP_COMPARE )
	{
		XGM_VT *v1, *v2;
		if( !sgs_IsObject( C, 0, xgm_vec3_iface ) ||
			!sgs_IsObject( C, 1, xgm_vec3_iface ) )
			return SGS_EINVAL;
		
		v1 = (XGM_VT*) sgs_GetObjectData( C, 0 );
		v2 = (XGM_VT*) sgs_GetObjectData( C, 1 );
		
		if( v1[0] != v2[0] )
			sgs_PushReal( C, v1[0] - v2[0] );
		else if( v1[1] != v2[1] )
			sgs_PushReal( C, v1[1] - v2[1] );
		else
			sgs_PushReal( C, v1[2] - v2[2] );
		return SGS_SUCCESS;
	}
	else if( type == SGS_EOP_NEGATE )
	{
		XGM_OHDR;
		sgs_CreateVec3( C, NULL, -hdr[0], -hdr[1], -hdr[2] );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_v3_serialize( SGS_CTX, sgs_VarObj* obj )
{
	XGM_OHDR;
	sgs_Serialize( C, sgs_MakeReal( hdr[0] ) );
	sgs_Serialize( C, sgs_MakeReal( hdr[1] ) );
	sgs_Serialize( C, sgs_MakeReal( hdr[2] ) );
	sgs_SerializeObject( C, 3, "vec3" );
	return SGS_SUCCESS;
}

static int xgm_v3_dump( SGS_CTX, sgs_VarObj* obj, int unused )
{
	return xgm_v3_convert( C, obj, SGS_VT_STRING );
}

static int xgm_vec3( SGS_CTX )
{
	int argc = sgs_StackSize( C );
	uint32_t ty0 = sgs_ItemType( C, 0 );
	XGM_VT v[ 3 ] = { 0, 0, 0 };
	
	SGSFN( "vec3" );
	
	if( ty0 != SGS_VT_INT && ty0 != SGS_VT_REAL && sgs_ParseVec3( C, 0, v, 0 ) )
	{
		sgs_CreateVec3p( C, NULL, v );
		return 1;
	}
	
	if( !sgs_LoadArgs( C, "f|ff.", v, v + 1, v + 2 ) )
		return 0;
	
	if( argc == 2 )
		return XGM_WARNING( "expected 1 or 3 real values" );
	
	if( argc == 1 )
		v[2] = v[1] = v[0];
	
	sgs_CreateVec3p( C, NULL, v );
	return 1;
}

static int xgm_vec3_dot( SGS_CTX )
{
	XGM_VT v1[3], v2[3];
	
	SGSFN( "vec3_dot" );
	
	if( !sgs_LoadArgs( C, "!x!x", sgs_ArgCheck_Vec3, v1, sgs_ArgCheck_Vec3, v2 ) )
		return 0;
	
	sgs_PushReal( C, XGM_VMUL_INNER3( v1, v2 ) );
	return 1;
}

static int xgm_vec3_cross( SGS_CTX )
{
	XGM_VT v1[3], v2[3];
	
	SGSFN( "vec3_cross" );
	
	if( !sgs_LoadArgs( C, "!x!x", sgs_ArgCheck_Vec3, v1, sgs_ArgCheck_Vec3, v2 ) )
		return 0;
	
	sgs_CreateVec3( C, NULL,
		v1[1] * v2[2] - v1[2] * v2[1],
		v1[2] * v2[0] - v1[0] * v2[2],
		v1[0] * v2[1] - v1[1] * v2[0]
	);
	return 1;
}



/*  4 D   V E C T O R  */

static int xgm_v4_convert( SGS_CTX, sgs_VarObj* obj, int type )
{
	XGM_OHDR;
	if( type == SGS_CONVOP_CLONE )
	{
		sgs_CreateVec4p( C, NULL, hdr );
		return SGS_SUCCESS;
	}
	else if( type == SGS_VT_STRING )
	{
		char buf[ 256 ];
		sprintf( buf, "vec4(%g;%g;%g;%g)", hdr[0], hdr[1], hdr[2], hdr[3] );
		sgs_PushString( C, buf );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_v4_getindex( SGS_ARGS_GETINDEXFUNC )
{
	char* str;
	XGM_OHDR;
	
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT )
	{
		sgs_Int pos = sgs_GetInt( C, 0 );
		if( pos != 0 && pos != 1 && pos != 2 && pos != 3 )
			return SGS_ENOTFND;
		sgs_PushReal( C, hdr[ pos ] );
		return SGS_SUCCESS;
	}
	
	if( sgs_ParseString( C, 0, &str, NULL ) )
	{
		if( !strcmp( str, "x" ) ){ sgs_PushReal( C, hdr[ 0 ] ); return SGS_SUCCESS; }
		if( !strcmp( str, "y" ) ){ sgs_PushReal( C, hdr[ 1 ] ); return SGS_SUCCESS; }
		if( !strcmp( str, "z" ) ){ sgs_PushReal( C, hdr[ 2 ] ); return SGS_SUCCESS; }
		if( !strcmp( str, "w" ) ){ sgs_PushReal( C, hdr[ 3 ] ); return SGS_SUCCESS; }
		if( !strcmp( str, "length" ) )
		{
			sgs_PushReal( C, sqrt( XGM_VMUL_INNER4( hdr, hdr ) ) );
			return SGS_SUCCESS;
		}
		if( !strcmp( str, "length_squared" ) )
		{
			sgs_PushReal( C, XGM_VMUL_INNER4( hdr, hdr ) );
			return SGS_SUCCESS;
		}
		if( !strcmp( str, "normalized" ) )
		{
			XGM_VT lensq = XGM_VMUL_INNER4( hdr, hdr );
			if( lensq != 0.0f )
			{
				lensq = (XGM_VT) 1.0 / (XGM_VT) sqrt( lensq );
				sgs_CreateVec4( C, NULL, hdr[0] * lensq, hdr[1] * lensq, hdr[2] * lensq, hdr[3] * lensq );
			}
			else
				sgs_CreateVec4( C, NULL, 0, 0, 0, 0 );
			return SGS_SUCCESS;
		}
		if( !strcmp( str, "size" ) ){ sgs_PushInt( C, 4 ); return SGS_SUCCESS; }
	}
	return SGS_ENOTFND;
}

static int xgm_v4_setindex( SGS_ARGS_SETINDEXFUNC )
{
	sgs_Real val;
	XGM_OHDR;
	
	if( !sgs_ParseReal( C, 1, &val ) )
		return SGS_EINVAL;
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT )
	{
		sgs_Int pos = sgs_GetInt( C, 0 );
		if( pos != 0 && pos != 1 && pos != 2 && pos != 3 )
			return SGS_ENOTFND;
		hdr[ pos ] = (XGM_VT) val;
		return SGS_SUCCESS;
	}
	else
	{
		char* str;
		if( sgs_ParseString( C, 0, &str, NULL ) )
		{
			if( !strcmp( str, "x" ) ){ hdr[0] = (XGM_VT) val; return SGS_SUCCESS; }
			if( !strcmp( str, "y" ) ){ hdr[1] = (XGM_VT) val; return SGS_SUCCESS; }
			if( !strcmp( str, "z" ) ){ hdr[2] = (XGM_VT) val; return SGS_SUCCESS; }
			if( !strcmp( str, "w" ) ){ hdr[3] = (XGM_VT) val; return SGS_SUCCESS; }
		}
	}
	return SGS_ENOTFND;
}

static int xgm_v4_expr( SGS_ARGS_OBJFUNC )
{
	int type = sgs_ObjectArg( C );
	if( type == SGS_EOP_ADD ||
		type == SGS_EOP_SUB ||
		type == SGS_EOP_MUL ||
		type == SGS_EOP_DIV ||
		type == SGS_EOP_MOD )
	{
		XGM_VT r[4], v1[4], v2[4];
		if( !sgs_ParseVec4( C, 0, v1, 0 ) || !sgs_ParseVec4( C, 1, v2, 0 ) )
			return SGS_EINVAL;
		
		if( ( type == SGS_EOP_DIV || type == SGS_EOP_MOD ) &&
			( v2[0] == 0 || v2[1] == 0 || v2[2] == 0 || v2[3] == 0 ) )
		{
			const char* errstr = type == SGS_EOP_DIV ?
				"vec4 operator '/' - division by zero" :
				"vec4 operator '%' - modulo by zero";
			sgs_Msg( C, SGS_ERROR, errstr );
			return SGS_EINPROC;
		}
		
		if( type == SGS_EOP_ADD )
		{
			r[0] = v1[0] + v2[0]; r[1] = v1[1] + v2[1];
			r[2] = v1[2] + v2[2]; r[3] = v1[3] + v2[3];
		}
		else if( type == SGS_EOP_SUB )
		{
			r[0] = v1[0] - v2[0]; r[1] = v1[1] - v2[1];
			r[2] = v1[2] - v2[2]; r[3] = v1[3] - v2[3];
		}
		else if( type == SGS_EOP_MUL )
		{
			r[0] = v1[0] * v2[0]; r[1] = v1[1] * v2[1];
			r[2] = v1[2] * v2[2]; r[3] = v1[3] * v2[3];
		}
		else if( type == SGS_EOP_DIV )
		{
			r[0] = v1[0] / v2[0]; r[1] = v1[1] / v2[1];
			r[2] = v1[2] / v2[2]; r[3] = v1[3] / v2[3];
		}
		else
		{
			r[0] = (XGM_VT) fmod( v1[0], v2[0] );
			r[1] = (XGM_VT) fmod( v1[1], v2[1] );
			r[2] = (XGM_VT) fmod( v1[2], v2[2] );
			r[3] = (XGM_VT) fmod( v1[3], v2[3] );
		}
		
		sgs_CreateVec4p( C, NULL, r );
		return SGS_SUCCESS;
	}
	else if( type == SGS_EOP_COMPARE )
	{
		XGM_VT *v1, *v2;
		if( !sgs_IsObject( C, 0, xgm_vec4_iface ) ||
			!sgs_IsObject( C, 1, xgm_vec4_iface ) )
			return SGS_EINVAL;
		
		v1 = (XGM_VT*) sgs_GetObjectData( C, 0 );
		v2 = (XGM_VT*) sgs_GetObjectData( C, 1 );
		
		if( v1[0] != v2[0] ) sgs_PushReal( C, v1[0] - v2[0] );
		else if( v1[1] != v2[1] ) sgs_PushReal( C, v1[1] - v2[1] );
		else if( v1[2] != v2[2] ) sgs_PushReal( C, v1[2] - v2[2] );
		else sgs_PushReal( C, v1[3] - v2[3] );
		return SGS_SUCCESS;
	}
	else if( type == SGS_EOP_NEGATE )
	{
		XGM_OHDR;
		sgs_CreateVec4( C, NULL, -hdr[0], -hdr[1], -hdr[2], -hdr[3] );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_v4_serialize( SGS_CTX, sgs_VarObj* obj )
{
	XGM_OHDR;
	sgs_Serialize( C, sgs_MakeReal( hdr[0] ) );
	sgs_Serialize( C, sgs_MakeReal( hdr[1] ) );
	sgs_Serialize( C, sgs_MakeReal( hdr[2] ) );
	sgs_Serialize( C, sgs_MakeReal( hdr[3] ) );
	sgs_SerializeObject( C, 4, "vec4" );
	return SGS_SUCCESS;
}

static int xgm_v4_dump( SGS_CTX, sgs_VarObj* obj, int unused )
{
	return xgm_v4_convert( C, obj, SGS_VT_STRING );
}

static int xgm_vec4( SGS_CTX )
{
	int argc = sgs_StackSize( C );
	uint32_t ty0 = sgs_ItemType( C, 0 );
	XGM_VT v[ 4 ] = { 0, 0, 0, 0 };
	
	SGSFN( "vec4" );
	
	if( ty0 != SGS_VT_INT && ty0 != SGS_VT_REAL && sgs_ParseVec4( C, 0, v, 0 ) )
	{
		sgs_CreateVec4p( C, NULL, v );
		return 1;
	}
	
	if( !sgs_LoadArgs( C, "f|fff.", v, v + 1, v + 2, v + 3 ) )
		return 0;
	
	if( argc == 1 )
		v[3] = v[2] = v[1] = v[0];
	else if( argc == 2 )
	{
		v[3] = v[1];
		v[2] = v[1] = v[0];
	}
	
	sgs_CreateVec4p( C, NULL, v );
	return 1;
}

static int xgm_vec4_dot( SGS_CTX )
{
	XGM_VT v1[4], v2[4];
	
	SGSFN( "vec4_dot" );
	
	if( !sgs_LoadArgs( C, "!x!x", sgs_ArgCheck_Vec4, v1, sgs_ArgCheck_Vec4, v2 ) )
		return 0;
	
	sgs_PushReal( C, XGM_VMUL_INNER4( v1, v2 ) );
	return 1;
}



/*  2 D   A A B B  */

static int xgm_aabb2_expand( SGS_CTX );

static int xgm_b2_convert( SGS_CTX, sgs_VarObj* obj, int type )
{
	XGM_OHDR;
	if( type == SGS_CONVOP_CLONE )
	{
		sgs_CreateAABB2( C, NULL, hdr[0], hdr[1], hdr[2], hdr[3] );
		return SGS_SUCCESS;
	}
	else if( type == SGS_VT_STRING )
	{
		char buf[ 256 ];
		sprintf( buf, "aabb2(%g;%g - %g;%g)", hdr[0], hdr[1], hdr[2], hdr[3] );
		sgs_PushString( C, buf );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_b2_getindex( SGS_ARGS_GETINDEXFUNC )
{
	char* str;
	XGM_OHDR;
	if( sgs_ParseString( C, 0, &str, NULL ) )
	{
		SGS_CASE( "x1" )     return sgs_PushReal( C, hdr[0] );
		SGS_CASE( "y1" )     return sgs_PushReal( C, hdr[1] );
		SGS_CASE( "x2" )     return sgs_PushReal( C, hdr[2] );
		SGS_CASE( "y2" )     return sgs_PushReal( C, hdr[3] );
		SGS_CASE( "p1" )     return sgs_CreateVec2p( C, NULL, hdr );
		SGS_CASE( "p2" )     return sgs_CreateVec2p( C, NULL, hdr + 2 );
		SGS_CASE( "width" )  return sgs_PushReal( C, hdr[2] - hdr[0] );
		SGS_CASE( "height" ) return sgs_PushReal( C, hdr[3] - hdr[1] );
		SGS_CASE( "center" ) return sgs_CreateVec2( C, NULL, (hdr[0]+hdr[2])*0.5f, (hdr[1]+hdr[3])*0.5f );
		SGS_CASE( "area" )   return sgs_PushReal( C, (hdr[2] - hdr[0]) * (hdr[3] - hdr[1]) );
		SGS_CASE( "valid" )  return sgs_PushBool( C, hdr[2] >= hdr[0] && hdr[3] >= hdr[1] );
		SGS_CASE( "expand" ) return sgs_PushCFunc( C, xgm_aabb2_expand );
	}
	return SGS_ENOTFND;
}

static int xgm_b2_setindex( SGS_ARGS_SETINDEXFUNC )
{
	char* str;
	XGM_OHDR;
	if( sgs_ParseString( C, 0, &str, NULL ) )
	{
		if( !strcmp( str, "x1" ) ) return sgs_ParseVT( C, 1, &hdr[0] ) ? SGS_SUCCESS : SGS_EINVAL;
		if( !strcmp( str, "y1" ) ) return sgs_ParseVT( C, 1, &hdr[1] ) ? SGS_SUCCESS : SGS_EINVAL;
		if( !strcmp( str, "x2" ) ) return sgs_ParseVT( C, 1, &hdr[2] ) ? SGS_SUCCESS : SGS_EINVAL;
		if( !strcmp( str, "y2" ) ) return sgs_ParseVT( C, 1, &hdr[3] ) ? SGS_SUCCESS : SGS_EINVAL;
		if( !strcmp( str, "p1" ) ) return sgs_ParseVec2( C, 1, hdr, 1 ) ? SGS_SUCCESS : SGS_EINVAL;
		if( !strcmp( str, "p2" ) ) return sgs_ParseVec2( C, 1, hdr + 2, 1 ) ? SGS_SUCCESS : SGS_EINVAL;
	}
	return SGS_ENOTFND;
}

static int xgm_b2_expr( SGS_ARGS_OBJFUNC )
{
	int type = sgs_ObjectArg( C );
	if( type == SGS_EOP_COMPARE )
	{
		XGM_VT *v1, *v2;
		if( !sgs_IsObject( C, 0, xgm_aabb2_iface ) ||
			!sgs_IsObject( C, 1, xgm_aabb2_iface ) )
			return SGS_EINVAL;
		
		v1 = (XGM_VT*) sgs_GetObjectData( C, 0 );
		v2 = (XGM_VT*) sgs_GetObjectData( C, 1 );
		
		if( v1[0] != v2[0] ) sgs_PushReal( C, v1[0] - v2[0] );
		else if( v1[1] != v2[1] ) sgs_PushReal( C, v1[1] - v2[1] );
		else if( v1[2] != v2[2] ) sgs_PushReal( C, v1[2] - v2[2] );
		else sgs_PushReal( C, v1[3] - v2[3] );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_b2_serialize( SGS_CTX, sgs_VarObj* obj )
{
	XGM_OHDR;
	sgs_Serialize( C, sgs_MakeReal( hdr[0] ) );
	sgs_Serialize( C, sgs_MakeReal( hdr[1] ) );
	sgs_Serialize( C, sgs_MakeReal( hdr[2] ) );
	sgs_Serialize( C, sgs_MakeReal( hdr[3] ) );
	sgs_SerializeObject( C, 4, "aabb2" );
	return SGS_SUCCESS;
}

static int xgm_b2_dump( SGS_CTX, sgs_VarObj* obj, int maxdepth )
{
	SGS_UNUSED( maxdepth );
	return xgm_b2_convert( C, obj, SGS_VT_STRING );
}

static int xgm_aabb2( SGS_CTX )
{
	XGM_VT b[4] = { 0, 0, 0, 0 };
	
	SGSFN( "aabb2" );
	
	if( !sgs_LoadArgs( C, "ffff", b, b + 1, b + 2, b + 3 ) )
		return 0;
	
	sgs_CreateAABB2p( C, NULL, b );
	return 1;
}

static int xgm_aabb2v( SGS_CTX )
{
	XGM_VT b[4] = { 0, 0, 0, 0 };
	
	SGSFN( "aabb2v" );
	
	if( !sgs_LoadArgs( C, "!x!x", sgs_ArgCheck_Vec2, b, sgs_ArgCheck_Vec2, b + 2 ) )
		return 0;
	
	sgs_CreateAABB2p( C, NULL, b );
	return 1;
}

static int xgm_aabb2_intersect( SGS_CTX )
{
	XGM_VT b1[4], b2[4];
	
	SGSFN( "aabb2_intersect" );
	
	if( !sgs_LoadArgs( C, "xx", sgs_ArgCheck_AABB2, b1, sgs_ArgCheck_AABB2, b2 ) )
		return 0;
	
	sgs_PushBool( C, b1[0] < b2[2] && b2[0] < b1[2] && b1[1] < b2[3] && b2[1] < b1[3] );
	return 1;
}

static int xgm_aabb2_expand( SGS_CTX )
{
	XGM_VT* bb, tmp[4];
	
	int i, ssz = sgs_StackSize( C );
	int method_call = sgs_Method( C );
	SGSFN( method_call ? "aabb2.expand" : "aabb2_expand" );
	if( !sgs_IsObject( C, 0, xgm_aabb2_iface ) )
		return sgs_ArgErrorExt( C, 0, method_call, "aabb2", "" );
	bb = (XGM_VT*) sgs_GetObjectData( C, 0 );
	
	for( i = 1; i < ssz; ++i )
	{
		if( sgs_ParseVec2( C, i, tmp, 0 ) )
		{
			XGM_BB2_EXPAND_V2( bb, tmp );
		}
		else if( sgs_ParseAABB2( C, i, tmp ) )
		{
			XGM_BB2_EXPAND_V2( bb, tmp );
			XGM_BB2_EXPAND_V2( bb, tmp + 2 );
		}
		else
			return sgs_ArgErrorExt( C, i, 0, "aabb2 or vec2", "" );
	}
	return 0;
}



/*  3 D   A A B B  */

static int xgm_aabb3_expand( SGS_CTX );

static int xgm_b3_convert( SGS_CTX, sgs_VarObj* obj, int type )
{
	XGM_OHDR;
	if( type == SGS_CONVOP_CLONE )
	{
		sgs_CreateAABB3p( C, NULL, hdr );
		return SGS_SUCCESS;
	}
	else if( type == SGS_VT_STRING )
	{
		char buf[ 384 ] = {0};
		snprintf( buf, 383, "aabb3(%g;%g;%g - %g;%g;%g)", hdr[0], hdr[1], hdr[2], hdr[3], hdr[4], hdr[5] );
		sgs_PushString( C, buf );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_b3_getindex( SGS_ARGS_GETINDEXFUNC )
{
	char* str;
	XGM_OHDR;
	if( sgs_ParseString( C, 0, &str, NULL ) )
	{
		SGS_CASE( "x1" )     return sgs_PushReal( C, hdr[0] );
		SGS_CASE( "y1" )     return sgs_PushReal( C, hdr[1] );
		SGS_CASE( "z1" )     return sgs_PushReal( C, hdr[2] );
		SGS_CASE( "x2" )     return sgs_PushReal( C, hdr[3] );
		SGS_CASE( "y2" )     return sgs_PushReal( C, hdr[4] );
		SGS_CASE( "z2" )     return sgs_PushReal( C, hdr[5] );
		SGS_CASE( "p1" )     return sgs_CreateVec3p( C, NULL, hdr );
		SGS_CASE( "p2" )     return sgs_CreateVec3p( C, NULL, hdr + 3 );
		SGS_CASE( "width" )  return sgs_PushReal( C, hdr[3] - hdr[0] );
		SGS_CASE( "height" ) return sgs_PushReal( C, hdr[4] - hdr[1] );
		SGS_CASE( "depth" )  return sgs_PushReal( C, hdr[5] - hdr[2] );
		SGS_CASE( "center" ) return sgs_CreateVec3( C, NULL, (hdr[0]+hdr[3])*0.5f, (hdr[1]+hdr[4])*0.5f, (hdr[2]+hdr[5])*0.5f );
		SGS_CASE( "area" )   return sgs_PushReal( C, (hdr[3] - hdr[0]) * (hdr[4] - hdr[1]) * (hdr[5] - hdr[2]) );
		SGS_CASE( "valid" )  return sgs_PushBool( C, hdr[3] >= hdr[0] && hdr[4] >= hdr[1] && hdr[5] >= hdr[2] );
		SGS_CASE( "expand" ) return sgs_PushCFunc( C, xgm_aabb3_expand );
	}
	return SGS_ENOTFND;
}

static int xgm_b3_setindex( SGS_ARGS_SETINDEXFUNC )
{
	char* str;
	XGM_OHDR;
	if( sgs_ParseString( C, 0, &str, NULL ) )
	{
		if( !strcmp( str, "x1" ) ) return sgs_ParseVT( C, 1, &hdr[0] ) ? SGS_SUCCESS : SGS_EINVAL;
		if( !strcmp( str, "y1" ) ) return sgs_ParseVT( C, 1, &hdr[1] ) ? SGS_SUCCESS : SGS_EINVAL;
		if( !strcmp( str, "z1" ) ) return sgs_ParseVT( C, 1, &hdr[2] ) ? SGS_SUCCESS : SGS_EINVAL;
		if( !strcmp( str, "x2" ) ) return sgs_ParseVT( C, 1, &hdr[3] ) ? SGS_SUCCESS : SGS_EINVAL;
		if( !strcmp( str, "y2" ) ) return sgs_ParseVT( C, 1, &hdr[4] ) ? SGS_SUCCESS : SGS_EINVAL;
		if( !strcmp( str, "z2" ) ) return sgs_ParseVT( C, 1, &hdr[5] ) ? SGS_SUCCESS : SGS_EINVAL;
		if( !strcmp( str, "p1" ) ) return sgs_ParseVec3( C, 1, hdr, 0 ) ? SGS_SUCCESS : SGS_EINVAL;
		if( !strcmp( str, "p2" ) ) return sgs_ParseVec3( C, 1, hdr + 3, 0 ) ? SGS_SUCCESS : SGS_EINVAL;
	}
	return SGS_ENOTFND;
}

static int xgm_b3_expr( SGS_ARGS_OBJFUNC )
{
	int type = sgs_ObjectArg( C );
	if( type == SGS_EOP_COMPARE )
	{
		XGM_VT *v1, *v2;
		if( !sgs_IsObject( C, 0, xgm_aabb3_iface ) ||
			!sgs_IsObject( C, 1, xgm_aabb3_iface ) )
			return SGS_EINVAL;
		
		v1 = (XGM_VT*) sgs_GetObjectData( C, 0 );
		v2 = (XGM_VT*) sgs_GetObjectData( C, 1 );
		
		if( v1[0] != v2[0] ) sgs_PushReal( C, v1[0] - v2[0] );
		else if( v1[1] != v2[1] ) sgs_PushReal( C, v1[1] - v2[1] );
		else if( v1[2] != v2[2] ) sgs_PushReal( C, v1[2] - v2[2] );
		else if( v1[3] != v2[3] ) sgs_PushReal( C, v1[3] - v2[3] );
		else if( v1[4] != v2[4] ) sgs_PushReal( C, v1[4] - v2[4] );
		else sgs_PushReal( C, v1[5] - v2[5] );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_b3_serialize( SGS_CTX, sgs_VarObj* obj )
{
	XGM_OHDR;
	sgs_Serialize( C, sgs_MakeReal( hdr[0] ) );
	sgs_Serialize( C, sgs_MakeReal( hdr[1] ) );
	sgs_Serialize( C, sgs_MakeReal( hdr[2] ) );
	sgs_Serialize( C, sgs_MakeReal( hdr[3] ) );
	sgs_Serialize( C, sgs_MakeReal( hdr[4] ) );
	sgs_Serialize( C, sgs_MakeReal( hdr[5] ) );
	sgs_SerializeObject( C, 4, "aabb3" );
	return SGS_SUCCESS;
}

static int xgm_b3_dump( SGS_CTX, sgs_VarObj* obj, int maxdepth )
{
	SGS_UNUSED( maxdepth );
	return xgm_b3_convert( C, obj, SGS_VT_STRING );
}

static int xgm_aabb3( SGS_CTX )
{
	XGM_VT b[6] = { 0, 0, 0, 0, 0, 0 };
	
	SGSFN( "aabb3" );
	
	if( !sgs_LoadArgs( C, "ffffff", b, b + 1, b + 2, b + 3, b + 4, b + 5 ) )
		return 0;
	
	sgs_CreateAABB3p( C, NULL, b );
	return 1;
}

static int xgm_aabb3v( SGS_CTX )
{
	XGM_VT b[6] = { 0, 0, 0, 0, 0, 0 };
	
	SGSFN( "aabb3v" );
	
	if( !sgs_LoadArgs( C, "!x!x", sgs_ArgCheck_Vec3, b, sgs_ArgCheck_Vec3, b + 3 ) )
		return 0;
	
	sgs_CreateAABB3p( C, NULL, b );
	return 1;
}

static int xgm_aabb3_intersect( SGS_CTX )
{
	XGM_VT b1[6], b2[6];
	
	SGSFN( "aabb3_intersect" );
	
	if( !sgs_LoadArgs( C, "xx", sgs_ArgCheck_AABB3, b1, sgs_ArgCheck_AABB3, b2 ) )
		return 0;
	
	sgs_PushBool( C, b1[0] < b2[3] && b2[0] < b1[3] && b1[1] < b2[4] && b2[1] < b1[4] && b1[2] < b2[5] && b2[2] < b1[5] );
	return 1;
}

static int xgm_aabb3_expand( SGS_CTX )
{
	XGM_VT* bb, tmp[6];
	
	int i, ssz = sgs_StackSize( C );
	int method_call = sgs_Method( C );
	SGSFN( method_call ? "aabb3.expand" : "aabb3_expand" );
	if( !sgs_IsObject( C, 0, xgm_aabb3_iface ) )
		return sgs_ArgErrorExt( C, 0, method_call, "aabb3", "" );
	bb = (XGM_VT*) sgs_GetObjectData( C, 0 );
	
	for( i = 1; i < ssz; ++i )
	{
		if( sgs_ParseVec3( C, i, tmp, 0 ) )
		{
			XGM_BB3_EXPAND_V3( bb, tmp );
		}
		else if( sgs_ParseAABB3( C, i, tmp ) )
		{
			XGM_BB3_EXPAND_V3( bb, tmp );
			XGM_BB3_EXPAND_V3( bb, tmp + 3 );
		}
		else
			return sgs_ArgErrorExt( C, i, 0, "aabb3 or vec3", "" );
	}
	return 0;
}



/*  C O L O R  */

static int xgm_col_convert( SGS_CTX, sgs_VarObj* obj, int type )
{
	XGM_OHDR;
	if( type == SGS_CONVOP_CLONE )
	{
		sgs_CreateColorp( C, NULL, hdr );
		return SGS_SUCCESS;
	}
	else if( type == SGS_VT_STRING )
	{
		char buf[ 256 ];
		sprintf( buf, "color(%g;%g;%g;%g)", hdr[0], hdr[1], hdr[2], hdr[3] );
		sgs_PushString( C, buf );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_col_getindex( SGS_ARGS_GETINDEXFUNC )
{
	char* str;
	XGM_OHDR;
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT )
	{
		sgs_Int pos = sgs_GetInt( C, 0 );
		if( pos != 0 && pos != 1 && pos != 2 && pos != 3 )
			return SGS_ENOTFND;
		sgs_PushReal( C, hdr[ pos ] );
		return SGS_SUCCESS;
	}
	
	if( sgs_ParseString( C, 0, &str, NULL ) )
	{
		if( !strcmp( str, "r" ) ){ sgs_PushReal( C, hdr[ 0 ] ); return SGS_SUCCESS; }
		if( !strcmp( str, "g" ) ){ sgs_PushReal( C, hdr[ 1 ] ); return SGS_SUCCESS; }
		if( !strcmp( str, "b" ) ){ sgs_PushReal( C, hdr[ 2 ] ); return SGS_SUCCESS; }
		if( !strcmp( str, "a" ) ){ sgs_PushReal( C, hdr[ 3 ] ); return SGS_SUCCESS; }
		if( !strcmp( str, "size" ) ){ sgs_PushInt( C, 4 ); return SGS_SUCCESS; }
	}
	return SGS_ENOTFND;
}

static int xgm_col_setindex( SGS_ARGS_SETINDEXFUNC )
{
	sgs_Real val;
	XGM_OHDR;
	if( !sgs_ParseReal( C, 1, &val ) )
		return SGS_EINVAL;
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT )
	{
		sgs_Int pos = sgs_GetInt( C, 0 );
		if( pos != 0 && pos != 1 && pos != 2 && pos != 3 )
			return SGS_ENOTFND;
		hdr[ pos ] = (XGM_VT) val;
		return SGS_SUCCESS;
	}
	else
	{
		char* str;
		if( !sgs_ParseString( C, 0, &str, NULL ) )
		{
			if( !strcmp( str, "r" ) ){ hdr[0] = (XGM_VT) val; return SGS_SUCCESS; }
			if( !strcmp( str, "g" ) ){ hdr[1] = (XGM_VT) val; return SGS_SUCCESS; }
			if( !strcmp( str, "b" ) ){ hdr[2] = (XGM_VT) val; return SGS_SUCCESS; }
			if( !strcmp( str, "a" ) ){ hdr[3] = (XGM_VT) val; return SGS_SUCCESS; }
		}
	}
	return SGS_ENOTFND;
}

static int xgm_col_expr( SGS_ARGS_OBJFUNC )
{
	int type = sgs_ObjectArg( C );
	if( type == SGS_EOP_ADD ||
		type == SGS_EOP_SUB ||
		type == SGS_EOP_MUL ||
		type == SGS_EOP_DIV ||
		type == SGS_EOP_MOD )
	{
		XGM_VT r[4], v1[4], v2[4];
		if( !sgs_ParseColor( C, 0, v1, 0 ) || !sgs_ParseColor( C, 1, v2, 0 ) )
			return SGS_EINVAL;
		
		if( ( type == SGS_EOP_DIV || type == SGS_EOP_MOD ) &&
			( v2[0] == 0 || v2[1] == 0 || v2[2] == 0 || v2[3] == 0 ) )
		{
			const char* errstr = type == SGS_EOP_DIV ?
				"color operator '/' - division by zero" :
				"color operator '%' - modulo by zero";
			sgs_Msg( C, SGS_ERROR, errstr );
			return SGS_EINPROC;
		}
		
		if( type == SGS_EOP_ADD )
		{
			r[0] = v1[0] + v2[0]; r[1] = v1[1] + v2[1];
			r[2] = v1[2] + v2[2]; r[3] = v1[3] + v2[3];
		}
		else if( type == SGS_EOP_SUB )
		{
			r[0] = v1[0] - v2[0]; r[1] = v1[1] - v2[1];
			r[2] = v1[2] - v2[2]; r[3] = v1[3] - v2[3];
		}
		else if( type == SGS_EOP_MUL )
		{
			r[0] = v1[0] * v2[0]; r[1] = v1[1] * v2[1];
			r[2] = v1[2] * v2[2]; r[3] = v1[3] * v2[3];
		}
		else if( type == SGS_EOP_DIV )
		{
			r[0] = v1[0] / v2[0]; r[1] = v1[1] / v2[1];
			r[2] = v1[2] / v2[2]; r[3] = v1[3] / v2[3];
		}
		else
		{
			r[0] = (XGM_VT) fmod( v1[0], v2[0] );
			r[1] = (XGM_VT) fmod( v1[1], v2[1] );
			r[2] = (XGM_VT) fmod( v1[2], v2[2] );
			r[3] = (XGM_VT) fmod( v1[3], v2[3] );
		}
		
		sgs_CreateColorp( C, NULL, r );
		return SGS_SUCCESS;
	}
	else if( type == SGS_EOP_COMPARE )
	{
		XGM_VT *v1, *v2;
		if( !sgs_IsObject( C, 0, xgm_vec4_iface ) ||
			!sgs_IsObject( C, 1, xgm_vec4_iface ) )
			return SGS_EINVAL;
		
		v1 = (XGM_VT*) sgs_GetObjectData( C, 0 );
		v2 = (XGM_VT*) sgs_GetObjectData( C, 1 );
		
		if( v1[0] != v2[0] ) sgs_PushReal( C, v1[0] - v2[0] );
		else if( v1[1] != v2[1] ) sgs_PushReal( C, v1[1] - v2[1] );
		else if( v1[2] != v2[2] ) sgs_PushReal( C, v1[2] - v2[2] );
		else sgs_PushReal( C, v1[3] - v2[3] );
		return SGS_SUCCESS;
	}
	else if( type == SGS_EOP_NEGATE )
	{
		XGM_OHDR;
		sgs_CreateColor( C, NULL, -hdr[0], -hdr[1], -hdr[2], -hdr[3] );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_col_serialize( SGS_CTX, sgs_VarObj* obj )
{
	int i;
	XGM_OHDR;
	for( i = 0; i < 4; ++i )
	{
		sgs_Serialize( C, sgs_MakeReal( hdr[i] ) );
	}
	sgs_SerializeObject( C, 4, "color" );
	return SGS_SUCCESS;
}

static int xgm_col_dump( SGS_CTX, sgs_VarObj* obj, int unused )
{
	return xgm_col_convert( C, obj, SGS_VT_STRING );
}

static int xgm_color( SGS_CTX )
{
	int argc = sgs_StackSize( C );
	XGM_VT v[ 4 ] = { 0, 0, 0, 0 };
	
	SGSFN( "color" );
	
	if( !sgs_LoadArgs( C, "f|fff.", v, v + 1, v + 2, v + 3 ) )
		return 0;
	
	sgs_CreateColorvp( C, NULL, v, argc );
	return 1;
}



typedef XGM_VT VEC3[3];
typedef XGM_VT VEC4[4];
typedef VEC3 MAT3[3];
typedef VEC4 MAT4[4];
typedef VEC4 QUAT;


static void VEC4_Normalize( VEC4 r, VEC4 a )
{
	float lensq = XGM_VMUL_INNER4( a, a );
	if( lensq != 0.0f )
	{
		float invlen = 1.0f / sqrtf( lensq );
		XGM_SET4( r, a[0] * invlen, a[1] * invlen, a[2] * invlen, a[3] * invlen );
	}
	else { XGM_SET4( r, 0, 0, 0, 0 ); }
}

#define QUAT_Identity( q ) (q)[0] = (q)[1] = (q)[2] = 0.0f; (q)[3] = 1.0f
#define QUAT_Add( r, a, b ) (r)[0] = (a)[0] + (b)[0]; (r)[1] = (a)[1] + (b)[1]; \
	(r)[2] = (a)[2] + (b)[2]; (r)[3] = (a)[3] + (b)[3]
#define QUAT_Conj( r, a ) XGM_SET4( r, -(a)[0], -(a)[1], -(a)[2], (a)[3] )
#define QUAT_Norm VEC4_Normalize

static void QUAT_AxisAngle( QUAT r, float x, float y, float z, float a )
{
	float angsin = sinf( a / 2.0f );
	r[0] = x * angsin;
	r[1] = y * angsin;
	r[2] = z * angsin;
	r[3] = cosf( a / 2.0f );
	QUAT_Norm( r, r );
}

static void QUAT_Mul( QUAT r, QUAT q1, QUAT q2 )
{
	QUAT t;
	t[0] =  q1[0] * q2[3] + q1[1] * q2[2] - q1[2] * q2[1] + q1[3] * q2[0];
	t[1] = -q1[0] * q2[2] + q1[1] * q2[3] + q1[2] * q2[0] + q1[3] * q2[1];
	t[2] =  q1[0] * q2[1] - q1[1] * q2[0] + q1[2] * q2[3] + q1[3] * q2[2];
	t[3] = -q1[0] * q2[0] - q1[1] * q2[1] - q1[2] * q2[2] + q1[3] * q2[3];
	memcpy( r, t, sizeof(QUAT) );
}

static void QUAT_Transform( VEC3 r, QUAT q, VEC3 v )
{
	QUAT q_, qin;
	QUAT v_ = { v[0], v[1], v[2], 0.0f };
	
	QUAT_Conj( qin, q );
	QUAT_Conj( q_, qin );
	QUAT_Norm( q_, q_ );
	QUAT_Mul( q_, v_, q_ );
	QUAT_Mul( q_, qin, q_ );
	memcpy( r, q_, sizeof(VEC3) );
}

static void MAT3_FromQuat( MAT3 M, QUAT q )
{
	float a = -q[3];
	float b = q[0];
	float c = q[1];
	float d = q[2];
	float a2 = a*a;
	float b2 = b*b;
	float c2 = c*c;
	float d2 = d*d;
	
	M[0][0] = a2 + b2 - c2 - d2;
	M[0][1] = 2*(b*c + a*d);
	M[0][2] = 2*(b*d - a*c);
	
	M[1][0] = 2*(b*c - a*d);
	M[1][1] = a2 - b2 + c2 - d2;
	M[1][2] = 2*(c*d + a*b);
	
	M[2][0] = 2*(b*d + a*c);
	M[2][1] = 2*(c*d - a*b);
	M[2][2] = a2 - b2 - c2 + d2;
}

static void MAT4_FromQuat( MAT4 M, QUAT q )
{
	float a = -q[3];
	float b = q[0];
	float c = q[1];
	float d = q[2];
	float a2 = a*a;
	float b2 = b*b;
	float c2 = c*c;
	float d2 = d*d;
	
	M[0][0] = a2 + b2 - c2 - d2;
	M[0][1] = 2*(b*c + a*d);
	M[0][2] = 2*(b*d - a*c);
	M[0][3] = 0.0f;
	
	M[1][0] = 2*(b*c - a*d);
	M[1][1] = a2 - b2 + c2 - d2;
	M[1][2] = 2*(c*d + a*b);
	M[1][3] = 0.0f;
	
	M[2][0] = 2*(b*d + a*c);
	M[2][1] = 2*(c*d - a*b);
	M[2][2] = a2 - b2 - c2 + d2;
	M[2][3] = 0.0f;
	
	M[3][0] = M[3][1] = M[3][2] = 0.0f;
	M[3][3] = 1.0f;
}

static void QUAT_FromMatrix( QUAT q, float* row0, float* row1, float* row2 )
{
	float tr = row0[0] + row1[1] + row2[2];
	
	if( tr > 0 )
	{ 
		float S = sqrtf( tr + 1.0f ) * 2;
		q[3] = 0.25f * S;
		q[0] = (row2[1] - row1[2]) / S;
		q[1] = (row0[2] - row2[0]) / S; 
		q[2] = (row1[0] - row0[1]) / S; 
	}
	else if( ( row0[0] > row1[1] ) & ( row0[0] > row2[2] ) )
	{ 
		float S = sqrtf( 1.0f + row0[0] - row1[1] - row2[2] ) * 2;
		q[3] = ( row2[1] - row1[2] ) / S;
		q[0] = 0.25f * S;
		q[1] = ( row0[1] + row1[0] ) / S; 
		q[2] = ( row0[2] + row2[0] ) / S; 
	}
	else if( row1[1] > row2[2] )
	{ 
		float S = sqrtf( 1.0f + row1[1] - row0[0] - row2[2] ) * 2;
		q[3] = ( row0[2] - row2[0] ) / S;
		q[0] = ( row0[1] + row1[0] ) / S; 
		q[1] = 0.25f * S;
		q[2] = ( row1[2] + row2[1] ) / S; 
	}
	else
	{ 
		float S = sqrtf( 1.0f + row2[2] - row0[0] - row1[1] ) * 2;
		q[3] = ( row1[0] - row0[1] ) / S;
		q[0] = ( row0[2] + row2[0] ) / S;
		q[1] = ( row1[2] + row2[1] ) / S;
		q[2] = 0.25f * S;
	}
}

#define MAT4_DUMP( M ) \
	puts( "--- MATRIX DUMP ---" ); \
	printf( "%g %g %g %g\n", M[0][0], M[0][1], M[0][2], M[0][3] ); \
	printf( "%g %g %g %g\n", M[1][0], M[1][1], M[1][2], M[1][3] ); \
	printf( "%g %g %g %g\n", M[2][0], M[2][1], M[2][2], M[2][3] ); \
	printf( "%g %g %g %g\n", M[3][0], M[3][1], M[3][2], M[3][3] ); \
	puts( "--- --- END --- ---" );

void MAT4_Transpose( MAT4 mtx )
{
	XGM_VT tmp;
#define _MSWAP( a, b ) { tmp = a; a = b; b = tmp; }
	_MSWAP( mtx[0][1], mtx[1][0] );
	_MSWAP( mtx[0][2], mtx[2][0] );
	_MSWAP( mtx[0][3], mtx[3][0] );
	_MSWAP( mtx[1][2], mtx[2][1] );
	_MSWAP( mtx[1][3], mtx[3][1] );
	_MSWAP( mtx[2][3], mtx[3][2] );
#undef _MSWAP
}

void MAT4_Multiply( MAT4 out, MAT4 A, MAT4 B )
{
	MAT4 tmp;
	tmp[0][0] = A[0][0] * B[0][0] + A[0][1] * B[1][0] + A[0][2] * B[2][0] + A[0][3] * B[3][0];
	tmp[0][1] = A[0][0] * B[0][1] + A[0][1] * B[1][1] + A[0][2] * B[2][1] + A[0][3] * B[3][1];
	tmp[0][2] = A[0][0] * B[0][2] + A[0][1] * B[1][2] + A[0][2] * B[2][2] + A[0][3] * B[3][2];
	tmp[0][3] = A[0][0] * B[0][3] + A[0][1] * B[1][3] + A[0][2] * B[2][3] + A[0][3] * B[3][3];
	tmp[1][0] = A[1][0] * B[0][0] + A[1][1] * B[1][0] + A[1][2] * B[2][0] + A[1][3] * B[3][0];
	tmp[1][1] = A[1][0] * B[0][1] + A[1][1] * B[1][1] + A[1][2] * B[2][1] + A[1][3] * B[3][1];
	tmp[1][2] = A[1][0] * B[0][2] + A[1][1] * B[1][2] + A[1][2] * B[2][2] + A[1][3] * B[3][2];
	tmp[1][3] = A[1][0] * B[0][3] + A[1][1] * B[1][3] + A[1][2] * B[2][3] + A[1][3] * B[3][3];
	tmp[2][0] = A[2][0] * B[0][0] + A[2][1] * B[1][0] + A[2][2] * B[2][0] + A[2][3] * B[3][0];
	tmp[2][1] = A[2][0] * B[0][1] + A[2][1] * B[1][1] + A[2][2] * B[2][1] + A[2][3] * B[3][1];
	tmp[2][2] = A[2][0] * B[0][2] + A[2][1] * B[1][2] + A[2][2] * B[2][2] + A[2][3] * B[3][2];
	tmp[2][3] = A[2][0] * B[0][3] + A[2][1] * B[1][3] + A[2][2] * B[2][3] + A[2][3] * B[3][3];
	tmp[3][0] = A[3][0] * B[0][0] + A[3][1] * B[1][0] + A[3][2] * B[2][0] + A[3][3] * B[3][0];
	tmp[3][1] = A[3][0] * B[0][1] + A[3][1] * B[1][1] + A[3][2] * B[2][1] + A[3][3] * B[3][1];
	tmp[3][2] = A[3][0] * B[0][2] + A[3][1] * B[1][2] + A[3][2] * B[2][2] + A[3][3] * B[3][2];
	tmp[3][3] = A[3][0] * B[0][3] + A[3][1] * B[1][3] + A[3][2] * B[2][3] + A[3][3] * B[3][3];
	memcpy( out, tmp, sizeof(tmp) );
};

int MAT4_Invert( MAT4 out, MAT4 M )
{
	XGM_VT inv[16], *m = M[0], *outInv = out[0], det;
	int i;
	
	inv[0] = m[5]  * m[10] * m[15] -
			 m[5]  * m[11] * m[14] -
			 m[9]  * m[6]  * m[15] +
			 m[9]  * m[7]  * m[14] +
			 m[13] * m[6]  * m[11] -
			 m[13] * m[7]  * m[10];
	
	inv[4] = -m[4]  * m[10] * m[15] +
			  m[4]  * m[11] * m[14] +
			  m[8]  * m[6]  * m[15] -
			  m[8]  * m[7]  * m[14] -
			  m[12] * m[6]  * m[11] +
			  m[12] * m[7]  * m[10];
	
	inv[8] = m[4]  * m[9] * m[15] -
			 m[4]  * m[11] * m[13] -
			 m[8]  * m[5] * m[15] +
			 m[8]  * m[7] * m[13] +
			 m[12] * m[5] * m[11] -
			 m[12] * m[7] * m[9];
	
	inv[12] = -m[4]  * m[9] * m[14] +
			   m[4]  * m[10] * m[13] +
			   m[8]  * m[5] * m[14] -
			   m[8]  * m[6] * m[13] -
			   m[12] * m[5] * m[10] +
			   m[12] * m[6] * m[9];
	
	inv[1] = -m[1]  * m[10] * m[15] +
			  m[1]  * m[11] * m[14] +
			  m[9]  * m[2] * m[15] -
			  m[9]  * m[3] * m[14] -
			  m[13] * m[2] * m[11] +
			  m[13] * m[3] * m[10];
	
	inv[5] = m[0]  * m[10] * m[15] -
			 m[0]  * m[11] * m[14] -
			 m[8]  * m[2] * m[15] +
			 m[8]  * m[3] * m[14] +
			 m[12] * m[2] * m[11] -
			 m[12] * m[3] * m[10];
	
	inv[9] = -m[0]  * m[9] * m[15] +
			  m[0]  * m[11] * m[13] +
			  m[8]  * m[1] * m[15] -
			  m[8]  * m[3] * m[13] -
			  m[12] * m[1] * m[11] +
			  m[12] * m[3] * m[9];
	
	inv[13] = m[0]  * m[9] * m[14] -
			  m[0]  * m[10] * m[13] -
			  m[8]  * m[1] * m[14] +
			  m[8]  * m[2] * m[13] +
			  m[12] * m[1] * m[10] -
			  m[12] * m[2] * m[9];
	
	inv[2] = m[1]  * m[6] * m[15] -
			 m[1]  * m[7] * m[14] -
			 m[5]  * m[2] * m[15] +
			 m[5]  * m[3] * m[14] +
			 m[13] * m[2] * m[7] -
			 m[13] * m[3] * m[6];
	
	inv[6] = -m[0]  * m[6] * m[15] +
			  m[0]  * m[7] * m[14] +
			  m[4]  * m[2] * m[15] -
			  m[4]  * m[3] * m[14] -
			  m[12] * m[2] * m[7] +
			  m[12] * m[3] * m[6];
	
	inv[10] = m[0]  * m[5] * m[15] -
			  m[0]  * m[7] * m[13] -
			  m[4]  * m[1] * m[15] +
			  m[4]  * m[3] * m[13] +
			  m[12] * m[1] * m[7] -
			  m[12] * m[3] * m[5];
	
	inv[14] = -m[0]  * m[5] * m[14] +
			   m[0]  * m[6] * m[13] +
			   m[4]  * m[1] * m[14] -
			   m[4]  * m[2] * m[13] -
			   m[12] * m[1] * m[6] +
			   m[12] * m[2] * m[5];
	
	inv[3] = -m[1] * m[6] * m[11] +
			  m[1] * m[7] * m[10] +
			  m[5] * m[2] * m[11] -
			  m[5] * m[3] * m[10] -
			  m[9] * m[2] * m[7] +
			  m[9] * m[3] * m[6];
	
	inv[7] = m[0] * m[6] * m[11] -
			 m[0] * m[7] * m[10] -
			 m[4] * m[2] * m[11] +
			 m[4] * m[3] * m[10] +
			 m[8] * m[2] * m[7] -
			 m[8] * m[3] * m[6];
	
	inv[11] = -m[0] * m[5] * m[11] +
			   m[0] * m[7] * m[9] +
			   m[4] * m[1] * m[11] -
			   m[4] * m[3] * m[9] -
			   m[8] * m[1] * m[7] +
			   m[8] * m[3] * m[5];
	
	inv[15] = m[0] * m[5] * m[10] -
			  m[0] * m[6] * m[9] -
			  m[4] * m[1] * m[10] +
			  m[4] * m[2] * m[9] +
			  m[8] * m[1] * m[6] -
			  m[8] * m[2] * m[5];
	
	det = m[0] * inv[0] + m[1] * inv[4] + m[2] * inv[8] + m[3] * inv[12];
	
	if( det == 0 )
		return 0;
	
	det = 1.0f / det;
	
	for( i = 0; i < 16; ++i )
		outInv[ i ] = inv[ i ] * det;
	
	return 1;
}

void MAT4_Transform( VEC4 out, VEC4 v, MAT4 mtx )
{
	VEC4 r_;
	int i, j;
	for(j=0; j<4; ++j) {
		r_[j] = 0.;
		for(i=0; i<4; ++i) {
			r_[j] += mtx[i][j] * v[i];
		}
	}
	memcpy(out, r_, sizeof(r_));
}

void MAT4_TransformPos( VEC3 out, VEC3 pos, MAT4 mtx )
{
	VEC4 tmp, xpos = {pos[0],pos[1],pos[2],1};
	MAT4_Transform( tmp, xpos, mtx );
	out[0] = tmp[0] / tmp[3]; out[1] = tmp[1] / tmp[3]; out[2] = tmp[2] / tmp[3];
}

void MAT4_TransformNormal( VEC3 out, VEC3 pos, MAT4 mtx )
{
	VEC4 tmp, xpos = {pos[0],pos[1],pos[2],0};
	MAT4_Transform( tmp, xpos, mtx );
	out[0] = tmp[0]; out[1] = tmp[1]; out[2] = tmp[2];
}

void MAT4_Translate( MAT4 out, XGM_VT x, XGM_VT y, XGM_VT z )
{
	out[0][0] = out[1][1] = out[2][2] = out[3][3] = 1.0f;
	out[0][1] = out[0][2] = out[0][3] = 0.0f;
	out[1][0] = out[1][2] = out[1][3] = 0.0f;
	out[2][0] = out[2][1] = out[2][3] = 0.0f;
	out[3][0] = x;
	out[3][1] = y;
	out[3][2] = z;
}

void MAT4_RotateDefaultAxis( MAT4 out, int axis0, int axis1, XGM_VT angle )
{
	int x, y;
	XGM_VT s = (XGM_VT) sin( angle );
	XGM_VT c = (XGM_VT) cos( angle );
	for( y = 0; y < 4; ++y )
		for( x = 0; x < 4; ++x )
			out[x][y] = x == y ? 1.0f : 0.0f;
	out[ axis0 ][ axis0 ] = c;
	out[ axis0 ][ axis1 ] = -s;
	out[ axis1 ][ axis0 ] = s;
	out[ axis1 ][ axis1 ] = c;
}

void MAT4_RotateAxisAngle( MAT4 out, XGM_VT x, XGM_VT y, XGM_VT z, XGM_VT angle )
{
	XGM_VT s = (XGM_VT) sin( angle );
	XGM_VT c = (XGM_VT) cos( angle );
	XGM_VT Ic = 1 - c;
	
	out[3][0] = out[3][1] = out[3][2] = 0;
	out[0][3] = out[1][3] = out[2][3] = 0;
	out[3][3] = 1;
	
	out[0][0] = x * x * Ic + c;
	out[0][1] = y * x * Ic - z * s;
	out[0][2] = z * x * Ic + y * s;
	out[1][0] = x * y * Ic + z * s;
	out[1][1] = y * y * Ic + c;
	out[1][2] = z * y * Ic - x * s;
	out[2][0] = x * z * Ic - y * s;
	out[2][1] = y * z * Ic + x * s;
	out[2][2] = z * z * Ic + c;
}

void MAT4_Scale( MAT4 out, XGM_VT x, XGM_VT y, XGM_VT z )
{
	out[0][1] = out[0][2] = out[0][3] = 0.0f;
	out[1][0] = out[1][2] = out[1][3] = 0.0f;
	out[2][0] = out[2][1] = out[2][3] = 0.0f;
	out[3][0] = out[3][1] = out[3][2] = 0.0f;
	out[0][0] = x;
	out[1][1] = y;
	out[2][2] = z;
	out[3][3] = 1.0f;
}


/*  Q U A T E R N I O N  */

#define XGM_QUAT_IHDR( funcname ) QUAT* Q; \
	if( !SGS_PARSE_METHOD( C, xgm_quat_iface, Q, quat, funcname ) ) return 0;


static int xgm_qti_identity( SGS_CTX )
{
	XGM_QUAT_IHDR( identity );
	XGM_SET4( *Q, 0, 0, 0, 1 );
	SGS_RETURN_THIS( C );
}

static int xgm_qti_multiply( SGS_CTX )
{
	QUAT Q2;
	
	XGM_QUAT_IHDR( multiply );
	if( !sgs_LoadArgs( C, "x", sgs_ArgCheck_Quat, Q2 ) )
		return 0;
	
	QUAT_Mul( *Q, *Q, Q2 );
	SGS_RETURN_THIS( C );
}

static int xgm_qti_multiply_left( SGS_CTX )
{
	QUAT Q2;
	
	XGM_QUAT_IHDR( multiply_left );
	if( !sgs_LoadArgs( C, "x", sgs_ArgCheck_Quat, Q2 ) )
		return 0;
	
	QUAT_Mul( *Q, Q2, *Q );
	SGS_RETURN_THIS( C );
}

static int xgm_qti_multiply2( SGS_CTX )
{
	QUAT Q1, Q2;
	
	XGM_QUAT_IHDR( multiply2 );
	if( !sgs_LoadArgs( C, "xx", sgs_ArgCheck_Quat, Q1, sgs_ArgCheck_Quat, Q2 ) )
		return 0;
	
	QUAT_Mul( *Q, Q1, Q2 );
	SGS_RETURN_THIS( C );
}

static int xgm_qti_invert( SGS_CTX )
{
	XGM_QUAT_IHDR( invert );
	QUAT_Conj( *Q, *Q );
	SGS_RETURN_THIS( C );
}

static int xgm_qti_invert_from( SGS_CTX )
{
	QUAT Q2;
	
	XGM_QUAT_IHDR( invert_from );
	if( !sgs_LoadArgs( C, "x", sgs_ArgCheck_Quat, Q2 ) )
		return 0;
	
	QUAT_Conj( *Q, Q2 );
	SGS_RETURN_THIS( C );
}


static int xgm_qti_rotateX( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT angle;
	QUAT tmp;
	
	XGM_QUAT_IHDR( rotateX );
	if( !sgs_LoadArgs( C, "f|b", &angle, &reset ) )
		return 0;
	
	QUAT_AxisAngle( tmp, 1, 0, 0, angle );
	if( !reset )
		QUAT_Mul( *Q, *Q, tmp );
	else
		memcpy( *Q, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_qti_rotateY( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT angle;
	QUAT tmp;
	
	XGM_QUAT_IHDR( rotateY );
	if( !sgs_LoadArgs( C, "f|b", &angle, &reset ) )
		return 0;
	
	QUAT_AxisAngle( tmp, 0, 1, 0, angle );
	if( !reset )
		QUAT_Mul( *Q, *Q, tmp );
	else
		memcpy( *Q, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_qti_rotateZ( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT angle;
	QUAT tmp;
	
	XGM_QUAT_IHDR( rotateZ );
	if( !sgs_LoadArgs( C, "f|b", &angle, &reset ) )
		return 0;
	
	QUAT_AxisAngle( tmp, 0, 0, 1, angle );
	if( !reset )
		QUAT_Mul( *Q, *Q, tmp );
	else
		memcpy( *Q, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_qti_rotate_axis_angle( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT x, y, z, angle;
	QUAT tmp;
	
	XGM_QUAT_IHDR( rotate_axis_angle );
	if( !sgs_LoadArgs( C, "ffff|b", &x, &y, &z, &angle, &reset ) )
		return 0;
	
	QUAT_AxisAngle( tmp, x, y, z, angle );
	if( !reset )
		QUAT_Mul( *Q, *Q, tmp );
	else
		memcpy( *Q, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_qti_rotate_axis_angle_v3( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT v3[3], angle;
	QUAT tmp;
	
	XGM_QUAT_IHDR( rotate_axis_angle );
	if( !sgs_LoadArgs( C, "xf|b", sgs_ArgCheck_Vec3, v3, &angle, &reset ) )
		return 0;
	
	QUAT_AxisAngle( tmp, v3[0], v3[1], v3[2], angle );
	if( !reset )
		QUAT_Mul( *Q, *Q, tmp );
	else
		memcpy( *Q, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_qti_transform( SGS_CTX )
{
	VEC3 v3;
	XGM_QUAT_IHDR( transform );
	if( !sgs_LoadArgs( C, "x", sgs_ArgCheck_Vec3, v3 ) )
		return 0;
	
	QUAT_Transform( v3, *Q, v3 );
	sgs_CreateVec3p( C, NULL, v3 );
	return 1;
}


static int xgm_quat_convert( SGS_CTX, sgs_VarObj* obj, int type )
{
	XGM_OHDR;
	if( type == SGS_CONVOP_CLONE )
	{
		sgs_CreateQuatp( C, NULL, hdr );
		return SGS_SUCCESS;
	}
	else if( type == SGS_VT_STRING )
	{
		char buf[ 256 ];
		sprintf( buf, "quat(%g;%g;%g|%g)", hdr[0], hdr[1], hdr[2], hdr[3] );
		sgs_PushString( C, buf );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_quat_getindex( SGS_ARGS_GETINDEXFUNC )
{
	char* str;
	XGM_OHDR;
	
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT )
	{
		sgs_Int pos = sgs_GetInt( C, 0 );
		if( pos != 0 && pos != 1 && pos != 2 && pos != 3 )
			return SGS_ENOTFND;
		sgs_PushReal( C, hdr[ pos ] );
		return SGS_SUCCESS;
	}
	
	if( sgs_ParseString( C, 0, &str, NULL ) )
	{
		SGS_CASE( "identity" ) return sgs_PushCFunc( C, xgm_qti_identity );
		SGS_CASE( "multiply" ) return sgs_PushCFunc( C, xgm_qti_multiply );
		SGS_CASE( "multiply_left" ) return sgs_PushCFunc( C, xgm_qti_multiply_left );
		SGS_CASE( "multiply2" ) return sgs_PushCFunc( C, xgm_qti_multiply2 );
		SGS_CASE( "invert" ) return sgs_PushCFunc( C, xgm_qti_invert );
		SGS_CASE( "invert_from" ) return sgs_PushCFunc( C, xgm_qti_invert_from );
		
		SGS_CASE( "rotateX" ) return sgs_PushCFunc( C, xgm_qti_rotateX );
		SGS_CASE( "rotateY" ) return sgs_PushCFunc( C, xgm_qti_rotateY );
		SGS_CASE( "rotateZ" ) return sgs_PushCFunc( C, xgm_qti_rotateZ );
		SGS_CASE( "rotate_axis_angle" ) return sgs_PushCFunc( C, xgm_qti_rotate_axis_angle );
		SGS_CASE( "rotate_axis_angle_v3" ) return sgs_PushCFunc( C, xgm_qti_rotate_axis_angle_v3 );
		
		SGS_CASE( "transform" ) return sgs_PushCFunc( C, xgm_qti_transform );
		
		if( !strcmp( str, "x" ) ){ sgs_PushReal( C, hdr[ 0 ] ); return SGS_SUCCESS; }
		if( !strcmp( str, "y" ) ){ sgs_PushReal( C, hdr[ 1 ] ); return SGS_SUCCESS; }
		if( !strcmp( str, "z" ) ){ sgs_PushReal( C, hdr[ 2 ] ); return SGS_SUCCESS; }
		if( !strcmp( str, "w" ) ){ sgs_PushReal( C, hdr[ 3 ] ); return SGS_SUCCESS; }
		if( !strcmp( str, "length" ) )
		{
			sgs_PushReal( C, sqrt( XGM_VMUL_INNER4( hdr, hdr ) ) );
			return SGS_SUCCESS;
		}
		if( !strcmp( str, "length_squared" ) )
		{
			sgs_PushReal( C, XGM_VMUL_INNER4( hdr, hdr ) );
			return SGS_SUCCESS;
		}
		if( !strcmp( str, "normalized" ) )
		{
			XGM_VT lensq = XGM_VMUL_INNER4( hdr, hdr );
			if( lensq != 0.0f )
			{
				lensq = (XGM_VT) 1.0 / (XGM_VT) sqrt( lensq );
				sgs_CreateQuat( C, NULL, hdr[0] * lensq, hdr[1] * lensq, hdr[2] * lensq, hdr[3] * lensq );
			}
			else
				sgs_CreateQuat( C, NULL, 0, 0, 0, 0 );
			return SGS_SUCCESS;
		}
		if( !strcmp( str, "mat3" ) )
		{
			MAT3 out;
			MAT3_FromQuat( out, hdr );
			sgs_CreateMat3( C, NULL, *out, 0 );
			return SGS_SUCCESS;
		}
		if( !strcmp( str, "mat4" ) )
		{
			MAT4 out;
			MAT4_FromQuat( out, hdr );
			sgs_CreateMat4( C, NULL, *out, 0 );
			return SGS_SUCCESS;
		}
		if( !strcmp( str, "size" ) ){ sgs_PushInt( C, 4 ); return SGS_SUCCESS; }
	}
	return SGS_ENOTFND;
}

static int xgm_quat_setindex( SGS_ARGS_SETINDEXFUNC )
{
	sgs_Real val;
	XGM_OHDR;
	
	if( !sgs_ParseReal( C, 1, &val ) )
		return SGS_EINVAL;
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT )
	{
		sgs_Int pos = sgs_GetInt( C, 0 );
		if( pos != 0 && pos != 1 && pos != 2 && pos != 3 )
			return SGS_ENOTFND;
		hdr[ pos ] = (XGM_VT) val;
		return SGS_SUCCESS;
	}
	else
	{
		char* str;
		if( sgs_ParseString( C, 0, &str, NULL ) )
		{
			if( !strcmp( str, "x" ) ){ hdr[0] = (XGM_VT) val; return SGS_SUCCESS; }
			if( !strcmp( str, "y" ) ){ hdr[1] = (XGM_VT) val; return SGS_SUCCESS; }
			if( !strcmp( str, "z" ) ){ hdr[2] = (XGM_VT) val; return SGS_SUCCESS; }
			if( !strcmp( str, "w" ) ){ hdr[3] = (XGM_VT) val; return SGS_SUCCESS; }
		}
	}
	return SGS_ENOTFND;
}

static int xgm_quat_expr( SGS_ARGS_OBJFUNC )
{
	int type = sgs_ObjectArg( C );
	if( type == SGS_EOP_ADD ||
		type == SGS_EOP_SUB ||
		type == SGS_EOP_MUL ||
		type == SGS_EOP_DIV ||
		type == SGS_EOP_MOD )
	{
		XGM_VT r[4], v1[4], v2[4];
		if( !sgs_ParseQuat( C, 0, v1, 0 ) || !sgs_ParseQuat( C, 1, v2, 0 ) )
			return SGS_EINVAL;
		
		if( ( type == SGS_EOP_DIV || type == SGS_EOP_MOD ) &&
			( v2[0] == 0 || v2[1] == 0 || v2[2] == 0 || v2[3] == 0 ) )
		{
			const char* errstr = type == SGS_EOP_DIV ?
				"quat operator '/' - division by zero" :
				"quat operator '%' - modulo by zero";
			sgs_Msg( C, SGS_ERROR, errstr );
			return SGS_EINPROC;
		}
		
		if( type == SGS_EOP_ADD )
		{
			r[0] = v1[0] + v2[0]; r[1] = v1[1] + v2[1];
			r[2] = v1[2] + v2[2]; r[3] = v1[3] + v2[3];
		}
		else if( type == SGS_EOP_SUB )
		{
			r[0] = v1[0] - v2[0]; r[1] = v1[1] - v2[1];
			r[2] = v1[2] - v2[2]; r[3] = v1[3] - v2[3];
		}
		else if( type == SGS_EOP_MUL )
		{
			r[0] = v1[0] * v2[0]; r[1] = v1[1] * v2[1];
			r[2] = v1[2] * v2[2]; r[3] = v1[3] * v2[3];
		}
		else if( type == SGS_EOP_DIV )
		{
			r[0] = v1[0] / v2[0]; r[1] = v1[1] / v2[1];
			r[2] = v1[2] / v2[2]; r[3] = v1[3] / v2[3];
		}
		else
		{
			r[0] = (XGM_VT) fmod( v1[0], v2[0] );
			r[1] = (XGM_VT) fmod( v1[1], v2[1] );
			r[2] = (XGM_VT) fmod( v1[2], v2[2] );
			r[3] = (XGM_VT) fmod( v1[3], v2[3] );
		}
		
		sgs_CreateQuatp( C, NULL, r );
		return SGS_SUCCESS;
	}
	else if( type == SGS_EOP_COMPARE )
	{
		XGM_VT *v1, *v2;
		if( !sgs_IsObject( C, 0, xgm_quat_iface ) ||
			!sgs_IsObject( C, 1, xgm_quat_iface ) )
			return SGS_EINVAL;
		
		v1 = (XGM_VT*) sgs_GetObjectData( C, 0 );
		v2 = (XGM_VT*) sgs_GetObjectData( C, 1 );
		
		if( v1[0] != v2[0] ) sgs_PushReal( C, v1[0] - v2[0] );
		else if( v1[1] != v2[1] ) sgs_PushReal( C, v1[1] - v2[1] );
		else if( v1[2] != v2[2] ) sgs_PushReal( C, v1[2] - v2[2] );
		else sgs_PushReal( C, v1[3] - v2[3] );
		return SGS_SUCCESS;
	}
	else if( type == SGS_EOP_NEGATE )
	{
		XGM_OHDR;
		sgs_CreateQuat( C, NULL, -hdr[0], -hdr[1], -hdr[2], -hdr[3] );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_quat_serialize( SGS_CTX, sgs_VarObj* obj )
{
	XGM_OHDR;
	sgs_Serialize( C, sgs_MakeReal( hdr[0] ) );
	sgs_Serialize( C, sgs_MakeReal( hdr[1] ) );
	sgs_Serialize( C, sgs_MakeReal( hdr[2] ) );
	sgs_Serialize( C, sgs_MakeReal( hdr[3] ) );
	sgs_SerializeObject( C, 4, "quat" );
	return SGS_SUCCESS;
}

static int xgm_quat_dump( SGS_CTX, sgs_VarObj* obj, int unused )
{
	return xgm_quat_convert( C, obj, SGS_VT_STRING );
}

static int xgm_quat( SGS_CTX )
{
	XGM_VT v[ 4 ] = { 0, 0, 0, 1 };
	
	SGSFN( "quat" );
	
	if( sgs_StackSize( C ) == 0 )
	{
		sgs_CreateQuat( C, NULL, 0, 0, 0, 1 );
		return 1;
	}
	
	if( !sgs_LoadArgs( C, "ffff.", v, v + 1, v + 2, v + 3 ) )
		return 0;
	
	sgs_CreateQuat( C, NULL, v[0], v[1], v[2], v[3] );
	return 1;
}



/*  3 x 3   M A T R I X  */

#define MAT3_DUMP( M ) \
	puts( "--- MATRIX DUMP ---" ); \
	printf( "%g %g %g\n", M[0][0], M[0][1], M[0][2] ); \
	printf( "%g %g %g\n", M[1][0], M[1][1], M[1][2] ); \
	printf( "%g %g %g\n", M[2][0], M[2][1], M[2][2] ); \
	puts( "--- --- END --- ---" );

void MAT3_Transpose( MAT3 mtx )
{
	XGM_VT tmp;
#define _MSWAP( a, b ) { tmp = a; a = b; b = tmp; }
	_MSWAP( mtx[0][1], mtx[1][0] );
	_MSWAP( mtx[0][2], mtx[2][0] );
	_MSWAP( mtx[1][2], mtx[2][1] );
#undef _MSWAP
}

void MAT3_Multiply( MAT3 out, MAT3 A, MAT3 B )
{
	MAT3 tmp;
	tmp[0][0] = A[0][0] * B[0][0] + A[0][1] * B[1][0] + A[0][2] * B[2][0];
	tmp[0][1] = A[0][0] * B[0][1] + A[0][1] * B[1][1] + A[0][2] * B[2][1];
	tmp[0][2] = A[0][0] * B[0][2] + A[0][1] * B[1][2] + A[0][2] * B[2][2];
	tmp[1][0] = A[1][0] * B[0][0] + A[1][1] * B[1][0] + A[1][2] * B[2][0];
	tmp[1][1] = A[1][0] * B[0][1] + A[1][1] * B[1][1] + A[1][2] * B[2][1];
	tmp[1][2] = A[1][0] * B[0][2] + A[1][1] * B[1][2] + A[1][2] * B[2][2];
	tmp[2][0] = A[2][0] * B[0][0] + A[2][1] * B[1][0] + A[2][2] * B[2][0];
	tmp[2][1] = A[2][0] * B[0][1] + A[2][1] * B[1][1] + A[2][2] * B[2][1];
	tmp[2][2] = A[2][0] * B[0][2] + A[2][1] * B[1][2] + A[2][2] * B[2][2];
	memcpy( out, tmp, sizeof(tmp) );
};

int MAT3_Invert( MAT3 out, MAT3 M )
{
	MAT3 R;
#define result(x,y) R[y][x]
#define A(x,y) M[y][x]
	XGM_VT det = +A(0,0)*(A(1,1)*A(2,2)-A(2,1)*A(1,2))
	             -A(0,1)*(A(1,0)*A(2,2)-A(1,2)*A(2,0))
	             +A(0,2)*(A(1,0)*A(2,1)-A(1,1)*A(2,0));
	if( det == 0 )
		return 0;
	XGM_VT invdet = 1 / det;
	result(0,0) =  (A(1,1)*A(2,2)-A(2,1)*A(1,2))*invdet;
	result(1,0) = -(A(0,1)*A(2,2)-A(0,2)*A(2,1))*invdet;
	result(2,0) =  (A(0,1)*A(1,2)-A(0,2)*A(1,1))*invdet;
	result(0,1) = -(A(1,0)*A(2,2)-A(1,2)*A(2,0))*invdet;
	result(1,1) =  (A(0,0)*A(2,2)-A(0,2)*A(2,0))*invdet;
	result(2,1) = -(A(0,0)*A(1,2)-A(1,0)*A(0,2))*invdet;
	result(0,2) =  (A(1,0)*A(2,1)-A(2,0)*A(1,1))*invdet;
	result(1,2) = -(A(0,0)*A(2,1)-A(2,0)*A(0,1))*invdet;
	result(2,2) =  (A(0,0)*A(1,1)-A(1,0)*A(0,1))*invdet;
	memcpy( out, R, sizeof(MAT3) );
	return 1;
}

void MAT3_Transform( VEC3 out, VEC3 v, MAT3 mtx )
{
	VEC3 r_;
	int i, j;
	for(j=0; j<3; ++j) {
		r_[j] = 0.;
		for(i=0; i<3; ++i) {
			r_[j] += mtx[i][j] * v[i];
		}
	}
	memcpy(out, r_, sizeof(r_));
}

void MAT3_RotateDefaultAxis( MAT3 out, int axis0, int axis1, XGM_VT angle )
{
	int x, y;
	XGM_VT s = (XGM_VT) sin( angle );
	XGM_VT c = (XGM_VT) cos( angle );
	for( y = 0; y < 3; ++y )
		for( x = 0; x < 3; ++x )
			out[x][y] = x == y ? 1.0f : 0.0f;
	out[ axis0 ][ axis0 ] = c;
	out[ axis0 ][ axis1 ] = -s;
	out[ axis1 ][ axis0 ] = s;
	out[ axis1 ][ axis1 ] = c;
}

void MAT3_RotateAxisAngle( MAT3 out, XGM_VT x, XGM_VT y, XGM_VT z, XGM_VT angle )
{
	XGM_VT s = (XGM_VT) sin( angle );
	XGM_VT c = (XGM_VT) cos( angle );
	XGM_VT Ic = 1 - c;
	
	out[0][0] = x * x * Ic + c;
	out[0][1] = y * x * Ic - z * s;
	out[0][2] = z * x * Ic + y * s;
	out[1][0] = x * y * Ic + z * s;
	out[1][1] = y * y * Ic + c;
	out[1][2] = z * y * Ic - x * s;
	out[2][0] = x * z * Ic - y * s;
	out[2][1] = y * z * Ic + x * s;
	out[2][2] = z * z * Ic + c;
}

void MAT3_Scale( MAT3 out, XGM_VT x, XGM_VT y, XGM_VT z )
{
	out[0][1] = out[0][2] = 0.0f;
	out[1][0] = out[1][2] = 0.0f;
	out[2][0] = out[2][1] = 0.0f;
	out[0][0] = x;
	out[1][1] = y;
	out[2][2] = z;
}


#define XGM_M3_IHDR( funcname ) MAT3* M; \
	if( !SGS_PARSE_METHOD( C, xgm_mat3_iface, M, mat3, funcname ) ) return 0;


static int xgm_m3i_identity( SGS_CTX )
{
	XGM_M3_IHDR( identity );
	MAT3_Scale( *M, 1, 1, 1 );
	SGS_RETURN_THIS( C );
}

static int xgm_m3i_multiply( SGS_CTX )
{
	MAT3 M2;
	
	XGM_M3_IHDR( multiply );
	if( !sgs_LoadArgs( C, "x", sgs_ArgCheck_Mat3, M2 ) )
		return 0;
	
	MAT3_Multiply( *M, *M, M2 );
	SGS_RETURN_THIS( C );
}

static int xgm_m3i_multiply_left( SGS_CTX )
{
	MAT3 M2;
	
	XGM_M3_IHDR( multiply_left );
	if( !sgs_LoadArgs( C, "x", sgs_ArgCheck_Mat3, M2 ) )
		return 0;
	
	MAT3_Multiply( *M, M2, *M );
	SGS_RETURN_THIS( C );
}

static int xgm_m3i_multiply2( SGS_CTX )
{
	MAT3 M1, M2;
	
	XGM_M3_IHDR( multiply2 );
	if( !sgs_LoadArgs( C, "xx", sgs_ArgCheck_Mat3, M1, sgs_ArgCheck_Mat3, M2 ) )
		return 0;
	
	MAT3_Multiply( *M, M1, M2 );
	SGS_RETURN_THIS( C );
}

static int xgm_m3i_transpose( SGS_CTX )
{
	XGM_M3_IHDR( transpose );
	MAT3_Transpose( *M );
	return 0;
}

static int xgm_m3i_transpose_from( SGS_CTX )
{
	MAT3 M2;
	
	XGM_M3_IHDR( transpose_from );
	if( !sgs_LoadArgs( C, "x", sgs_ArgCheck_Mat3, M2 ) )
		return 0;
	
	memcpy( *M, M2, sizeof(M2) );
	MAT3_Transpose( *M );
	return 0;
}

static int xgm_m3i_invert( SGS_CTX )
{
	XGM_M3_IHDR( invert );
	sgs_PushBool( C, MAT3_Invert( *M, *M ) );
	return 1;
}

static int xgm_m3i_invert_from( SGS_CTX )
{
	MAT3 M2;
	
	XGM_M3_IHDR( invert_from );
	if( !sgs_LoadArgs( C, "x", sgs_ArgCheck_Mat3, M2 ) )
		return 0;
	
	sgs_PushBool( C, MAT3_Invert( *M, M2 ) );
	return 1;
}


static int xgm_m3i_rotateX( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT angle;
	MAT3 tmp;
	
	XGM_M3_IHDR( rotateX );
	if( !sgs_LoadArgs( C, "f|b", &angle, &reset ) )
		return 0;
	
	MAT3_RotateDefaultAxis( tmp, 1, 2, angle );
	if( !reset )
		MAT3_Multiply( *M, *M, tmp );
	else
		memcpy( *M, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_m3i_rotateY( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT angle;
	MAT3 tmp;
	
	XGM_M3_IHDR( rotateY );
	if( !sgs_LoadArgs( C, "f|b", &angle, &reset ) )
		return 0;
	
	MAT3_RotateDefaultAxis( tmp, 2, 0, angle );
	if( !reset )
		MAT3_Multiply( *M, *M, tmp );
	else
		memcpy( *M, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_m3i_rotateZ( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT angle;
	MAT3 tmp;
	
	XGM_M3_IHDR( rotateZ );
	if( !sgs_LoadArgs( C, "f|b", &angle, &reset ) )
		return 0;
	
	MAT3_RotateDefaultAxis( tmp, 0, 1, angle );
	if( !reset )
		MAT3_Multiply( *M, *M, tmp );
	else
		memcpy( *M, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_m3i_rotate_axis_angle( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT x, y, z, angle;
	MAT3 tmp;
	
	XGM_M3_IHDR( rotate_axis_angle );
	if( !sgs_LoadArgs( C, "ffff|b", &x, &y, &z, &angle, &reset ) )
		return 0;
	
	MAT3_RotateAxisAngle( tmp, x, y, z, angle );
	if( !reset )
		MAT3_Multiply( *M, *M, tmp );
	else
		memcpy( *M, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_m3i_rotate_axis_angle_v3( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT v3[3], angle;
	MAT3 tmp;
	
	XGM_M3_IHDR( rotate_axis_angle_v3 );
	if( !sgs_LoadArgs( C, "xf|b", sgs_ArgCheck_Vec3, v3, &angle, &reset ) )
		return 0;
	
	MAT3_RotateAxisAngle( tmp, v3[0], v3[1], v3[2], angle );
	if( !reset )
		MAT3_Multiply( *M, *M, tmp );
	else
		memcpy( *M, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_m3i_rotate_quat( SGS_CTX )
{
	sgs_Bool reset = 0;
	QUAT q;
	MAT3 tmp;
	
	XGM_M3_IHDR( rotate_quat );
	if( !sgs_LoadArgs( C, "x|b", sgs_ArgCheck_Quat, q, &reset ) )
		return 0;
	
	MAT3_FromQuat( tmp, q );
	if( !reset )
		MAT3_Multiply( *M, *M, tmp );
	else
		memcpy( *M, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_m3i_scale( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT x, y, z;
	MAT3 tmp;
	
	XGM_M3_IHDR( scale );
	if( !sgs_LoadArgs( C, "fff|b", &x, &y, &z, &reset ) )
		return 0;
	
	MAT3_Scale( tmp, x, y, z );
	if( !reset )
		MAT3_Multiply( *M, *M, tmp );
	else
		memcpy( *M, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_m3i_scale_v3( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT v3[3];
	MAT3 tmp;
	
	XGM_M3_IHDR( scale_v3 );
	if( !sgs_LoadArgs( C, "x|b", sgs_ArgCheck_Vec3, v3, &reset ) )
		return 0;
	
	MAT3_Scale( tmp, v3[0], v3[1], v3[2] );
	if( !reset )
		MAT3_Multiply( *M, *M, tmp );
	else
		memcpy( *M, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}


static int xgm_m3i_transform( SGS_CTX )
{
	VEC3 v3;
	XGM_M3_IHDR( transform );
	if( !sgs_LoadArgs( C, "x", sgs_ArgCheck_Vec3, v3 ) )
		return 0;
	
	MAT3_Transform( v3, v3, *M );
	sgs_CreateVec3p( C, NULL, v3 );
	return 1;
}


static int xgm_m3_convert( SGS_CTX, sgs_VarObj* obj, int type )
{
	XGM_OHDR;
	if( type == SGS_CONVOP_CLONE )
	{
		sgs_CreateMat3( C, NULL, hdr, 0 );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_m3_getindex( SGS_ARGS_GETINDEXFUNC )
{
	char* str;
	XGM_OHDR;
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT )
	{
		sgs_Int pos = sgs_GetInt( C, 0 );
		if( pos < 0 || pos > 8 )
			return SGS_ENOTFND;
		sgs_PushReal( C, hdr[ pos ] );
		return SGS_SUCCESS;
	}
	
	if( sgs_ParseString( C, 0, &str, NULL ) )
	{
		SGS_CASE( "identity" ) return sgs_PushCFunc( C, xgm_m3i_identity );
		SGS_CASE( "multiply" ) return sgs_PushCFunc( C, xgm_m3i_multiply );
		SGS_CASE( "multiply_left" ) return sgs_PushCFunc( C, xgm_m3i_multiply_left );
		SGS_CASE( "multiply2" ) return sgs_PushCFunc( C, xgm_m3i_multiply2 );
		SGS_CASE( "transpose" ) return sgs_PushCFunc( C, xgm_m3i_transpose );
		SGS_CASE( "transpose_from" ) return sgs_PushCFunc( C, xgm_m3i_transpose_from );
		SGS_CASE( "invert" ) return sgs_PushCFunc( C, xgm_m3i_invert );
		SGS_CASE( "invert_from" ) return sgs_PushCFunc( C, xgm_m3i_invert_from );
		
		SGS_CASE( "rotateX" ) return sgs_PushCFunc( C, xgm_m3i_rotateX );
		SGS_CASE( "rotateY" ) return sgs_PushCFunc( C, xgm_m3i_rotateY );
		SGS_CASE( "rotateZ" ) return sgs_PushCFunc( C, xgm_m3i_rotateZ );
		SGS_CASE( "rotate_axis_angle" ) return sgs_PushCFunc( C, xgm_m3i_rotate_axis_angle );
		SGS_CASE( "rotate_axis_angle_v3" ) return sgs_PushCFunc( C, xgm_m3i_rotate_axis_angle_v3 );
		SGS_CASE( "rotate_quat" ) return sgs_PushCFunc( C, xgm_m3i_rotate_quat );
		SGS_CASE( "scale" ) return sgs_PushCFunc( C, xgm_m3i_scale );
		SGS_CASE( "scale_v3" ) return sgs_PushCFunc( C, xgm_m3i_scale_v3 );
		
		SGS_CASE( "transform" ) return sgs_PushCFunc( C, xgm_m3i_transform );
		
		if( *str == 'm' && str[1] && str[2] && !str[3] )
		{
			int nx = str[1] - '0';
			int ny = str[2] - '0';
			if( nx >= 0 && nx < 3 && ny >= 0 && ny < 3 )
			{
				/* rows = x, columns = y, matrix is column-major */
				sgs_PushReal( C, hdr[ ny + nx * 3 ] );
				return SGS_SUCCESS;
			}
		}
		if( !strcmp( str, "quat" ) )
		{
			QUAT out;
			QUAT_FromMatrix( out, hdr, &hdr[3], &hdr[6] );
			sgs_CreateQuatp( C, NULL, out );
			return SGS_SUCCESS;
		}
		if( !strcmp( str, "mat4" ) )
		{
			MAT4 out;
			MAT4_Scale( out, 1, 1, 1 );
			memcpy( out[0], &hdr[0], sizeof(XGM_VT) * 3 );
			memcpy( out[1], &hdr[3], sizeof(XGM_VT) * 3 );
			memcpy( out[2], &hdr[6], sizeof(XGM_VT) * 3 );
			sgs_CreateMat4( C, NULL, *out, 0 );
			return SGS_SUCCESS;
		}
		if( !strcmp( str, "size" ) ){ sgs_PushInt( C, 9 ); return SGS_SUCCESS; }
	}
	return SGS_ENOTFND;
}

static int xgm_m3_setindex( SGS_ARGS_SETINDEXFUNC )
{
	char* str;
	XGM_OHDR;
	sgs_Real val;
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT )
	{
		sgs_Int pos = sgs_GetInt( C, 0 );
		if( pos < 0 || pos > 8 )
			return SGS_ENOTFND;
		if( sgs_ParseReal( C, 1, &val ) )
		{
			hdr[ pos ] = (XGM_VT) val;
			return SGS_SUCCESS;
		}
		else
			return SGS_EINVAL;
	}
	
	if( sgs_ParseString( C, 0, &str, NULL ) )
	{
		if( *str == 'm' && str[1] && str[2] && !str[3] )
		{
			int nx = str[1] - '0';
			int ny = str[2] - '0';
			if( nx >= 0 && nx < 3 && ny >= 0 && ny < 3 )
			{
				if( sgs_ParseReal( C, 1, &val ) )
				{
					/* rows = x, columns = y, matrix is column-major */
					hdr[ ny + nx * 3 ] = (XGM_VT) val;
					return SGS_SUCCESS;
				}
				else
					return SGS_EINVAL;
			}
		}
	}
	return SGS_ENOTFND;
}

static int xgm_m3_expr( SGS_ARGS_OBJFUNC )
{
	int type = sgs_ObjectArg( C );
	if( type == SGS_EOP_COMPARE )
	{
		int i;
		XGM_VT *v1, *v2;
		if( !sgs_IsObject( C, 0, xgm_mat3_iface ) ||
			!sgs_IsObject( C, 1, xgm_mat3_iface ) )
			return SGS_EINVAL;
		
		v1 = (XGM_VT*) sgs_GetObjectData( C, 0 );
		v2 = (XGM_VT*) sgs_GetObjectData( C, 1 );
		
		for( i = 0; i < 9; ++i )
		{
			if( v1[i] != v2[i] )
			{
				sgs_PushReal( C, v1[i] - v2[0] );
				break;
			}
		}
		if( i == 9 )
			sgs_PushReal( C, 0 );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_m3_serialize( SGS_CTX, sgs_VarObj* obj )
{
	int i;
	XGM_OHDR;
	for( i = 0; i < 9; ++i )
	{
		sgs_Serialize( C, sgs_MakeReal( hdr[i] ) );
	}
	sgs_SerializeObject( C, 9, "mat3" );
	return SGS_SUCCESS;
}

static int xgm_m3_dump( SGS_CTX, sgs_VarObj* obj, int maxdepth )
{
	char bfr[ 512 ];
	XGM_OHDR;
	SGS_UNUSED( maxdepth );
	snprintf( bfr, 512,
		"\n%10.6g %10.6g %10.6g"
		"\n%10.6g %10.6g %10.6g"
		"\n%10.6g %10.6g %10.6g",
		hdr[0], hdr[3], hdr[6],
		hdr[1], hdr[4], hdr[7],
		hdr[2], hdr[5], hdr[8] );
	bfr[ 511 ] = 0;
	sgs_PushString( C, "mat3\n(" );
	sgs_PushString( C, bfr );
	sgs_PadString( C );
	sgs_PushString( C, "\n)" );
	sgs_StringConcat( C, 3 );
	return SGS_SUCCESS;
}

static int xgm_mat3( SGS_CTX )
{
	XGM_VT v[ 9 ];
	int argc = sgs_StackSize( C );
	
	SGSFN( "mat3" );
	
	if( !argc )
	{
		int i;
		for( i = 0; i < 9; ++i )
			v[ i ] = 0;
		v[0] = v[4] = v[8] = 1;
		sgs_CreateMat3( C, NULL, v, 0 );
		return 1;
	}
	else if( argc == 1 && sgs_ParseMat3( C, 0, v ) )
	{
		sgs_CreateMat3( C, NULL, v, 0 );
		return 1;
	}
	else if( argc == 3 )
	{
		if( sgs_ParseVec3( C, 0, v, 0 ) &&
			sgs_ParseVec3( C, 1, v+3, 0 ) &&
			sgs_ParseVec3( C, 2, v+6, 0 ) )
		{
			sgs_CreateMat3( C, NULL, v, 0 );
			return 1;
		}
	}
	else if( argc == 9 )
	{
		int i;
		for( i = 0; i < 9; ++i )
		{
			sgs_Real val;
			if( !sgs_ParseReal( C, i, &val ) )
				break;
			v[ i ] = (XGM_VT) val;
		}
		if( i == 9 )
		{
			sgs_CreateMat3( C, NULL, v, 0 );
			return 1;
		}
	}
	return sgs_Msg( C, SGS_WARNING, "expected 0 arguments or "
		"1 mat3 argument or 1 mat4 argument or 3 vec3 arguments or 9 real arguments" );
}



/*  4 x 4   M A T R I X  */

#define XGM_M4_IHDR( funcname ) MAT4* M; \
	if( !SGS_PARSE_METHOD( C, xgm_mat4_iface, M, mat4, funcname ) ) return 0;


static int xgm_m4i_identity( SGS_CTX )
{
	XGM_M4_IHDR( identity );
	MAT4_Scale( *M, 1, 1, 1 );
	SGS_RETURN_THIS( C );
}

static int xgm_m4i_multiply( SGS_CTX )
{
	MAT4 M2;
	
	XGM_M4_IHDR( multiply );
	if( !sgs_LoadArgs( C, "x", sgs_ArgCheck_Mat4, M2 ) )
		return 0;
	
	MAT4_Multiply( *M, *M, M2 );
	SGS_RETURN_THIS( C );
}

static int xgm_m4i_multiply_left( SGS_CTX )
{
	MAT4 M2;
	
	XGM_M4_IHDR( multiply_left );
	if( !sgs_LoadArgs( C, "x", sgs_ArgCheck_Mat4, M2 ) )
		return 0;
	
	MAT4_Multiply( *M, M2, *M );
	SGS_RETURN_THIS( C );
}

static int xgm_m4i_multiply2( SGS_CTX )
{
	MAT4 M1, M2;
	
	XGM_M4_IHDR( multiply2 );
	if( !sgs_LoadArgs( C, "xx", sgs_ArgCheck_Mat4, M1, sgs_ArgCheck_Mat4, M2 ) )
		return 0;
	
	MAT4_Multiply( *M, M1, M2 );
	SGS_RETURN_THIS( C );
}

static int xgm_m4i_transpose( SGS_CTX )
{
	XGM_M4_IHDR( transpose );
	MAT4_Transpose( *M );
	return 0;
}

static int xgm_m4i_transpose_from( SGS_CTX )
{
	MAT4 M2;
	
	XGM_M4_IHDR( transpose_from );
	if( !sgs_LoadArgs( C, "x", sgs_ArgCheck_Mat4, M2 ) )
		return 0;
	
	memcpy( *M, M2, sizeof(M2) );
	MAT4_Transpose( *M );
	return 0;
}

static int xgm_m4i_invert( SGS_CTX )
{
	XGM_M4_IHDR( invert );
	sgs_PushBool( C, MAT4_Invert( *M, *M ) );
	return 1;
}

static int xgm_m4i_invert_from( SGS_CTX )
{
	MAT4 M2;
	
	XGM_M4_IHDR( invert_from );
	if( !sgs_LoadArgs( C, "x", sgs_ArgCheck_Mat4, M2 ) )
		return 0;
	
	sgs_PushBool( C, MAT4_Invert( *M, M2 ) );
	return 1;
}


static int xgm_m4i_translate( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT x, y, z;
	MAT4 tmp;
	
	XGM_M4_IHDR( translate );
	if( !sgs_LoadArgs( C, "fff|b", &x, &y, &z, &reset ) )
		return 0;
	
	MAT4_Translate( tmp, x, y, z );
	if( !reset )
		MAT4_Multiply( *M, *M, tmp );
	else
		memcpy( *M, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_m4i_translate_v3( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT v3[3];
	MAT4 tmp;
	
	XGM_M4_IHDR( translate );
	if( !sgs_LoadArgs( C, "x|b", sgs_ArgCheck_Vec3, v3, &reset ) )
		return 0;
	
	MAT4_Translate( tmp, v3[0], v3[1], v3[2] );
	if( !reset )
		MAT4_Multiply( *M, *M, tmp );
	else
		memcpy( *M, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_m4i_rotateX( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT angle;
	MAT4 tmp;
	
	XGM_M4_IHDR( rotateX );
	if( !sgs_LoadArgs( C, "f|b", &angle, &reset ) )
		return 0;
	
	MAT4_RotateDefaultAxis( tmp, 1, 2, angle );
	if( !reset )
		MAT4_Multiply( *M, *M, tmp );
	else
		memcpy( *M, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_m4i_rotateY( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT angle;
	MAT4 tmp;
	
	XGM_M4_IHDR( rotateY );
	if( !sgs_LoadArgs( C, "f|b", &angle, &reset ) )
		return 0;
	
	MAT4_RotateDefaultAxis( tmp, 2, 0, angle );
	if( !reset )
		MAT4_Multiply( *M, *M, tmp );
	else
		memcpy( *M, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_m4i_rotateZ( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT angle;
	MAT4 tmp;
	
	XGM_M4_IHDR( rotateZ );
	if( !sgs_LoadArgs( C, "f|b", &angle, &reset ) )
		return 0;
	
	MAT4_RotateDefaultAxis( tmp, 0, 1, angle );
	if( !reset )
		MAT4_Multiply( *M, *M, tmp );
	else
		memcpy( *M, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_m4i_rotate_axis_angle( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT x, y, z, angle;
	MAT4 tmp;
	
	XGM_M4_IHDR( rotate_axis_angle );
	if( !sgs_LoadArgs( C, "ffff|b", &x, &y, &z, &angle, &reset ) )
		return 0;
	
	MAT4_RotateAxisAngle( tmp, x, y, z, angle );
	if( !reset )
		MAT4_Multiply( *M, *M, tmp );
	else
		memcpy( *M, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_m4i_rotate_axis_angle_v3( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT v3[3], angle;
	MAT4 tmp;
	
	XGM_M4_IHDR( rotate_axis_angle_v3 );
	if( !sgs_LoadArgs( C, "xf|b", sgs_ArgCheck_Vec3, v3, &angle, &reset ) )
		return 0;
	
	MAT4_RotateAxisAngle( tmp, v3[0], v3[1], v3[2], angle );
	if( !reset )
		MAT4_Multiply( *M, *M, tmp );
	else
		memcpy( *M, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_m4i_rotate_quat( SGS_CTX )
{
	sgs_Bool reset = 0;
	QUAT q;
	MAT4 tmp;
	
	XGM_M4_IHDR( rotate_quat );
	if( !sgs_LoadArgs( C, "x|b", sgs_ArgCheck_Quat, q, &reset ) )
		return 0;
	
	MAT4_FromQuat( tmp, q );
	if( !reset )
		MAT4_Multiply( *M, *M, tmp );
	else
		memcpy( *M, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_m4i_scale( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT x, y, z;
	MAT4 tmp;
	
	XGM_M4_IHDR( scale );
	if( !sgs_LoadArgs( C, "fff|b", &x, &y, &z, &reset ) )
		return 0;
	
	MAT4_Scale( tmp, x, y, z );
	if( !reset )
		MAT4_Multiply( *M, *M, tmp );
	else
		memcpy( *M, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}

static int xgm_m4i_scale_v3( SGS_CTX )
{
	sgs_Bool reset = 0;
	XGM_VT v3[3];
	MAT4 tmp;
	
	XGM_M4_IHDR( scale_v3 );
	if( !sgs_LoadArgs( C, "x|b", sgs_ArgCheck_Vec3, v3, &reset ) )
		return 0;
	
	MAT4_Scale( tmp, v3[0], v3[1], v3[2] );
	if( !reset )
		MAT4_Multiply( *M, *M, tmp );
	else
		memcpy( *M, tmp, sizeof(tmp) );
	
	SGS_RETURN_THIS( C );
}


static int xgm_m4i_transform( SGS_CTX )
{
	VEC4 v4;
	XGM_M4_IHDR( transform );
	if( !sgs_LoadArgs( C, "x", sgs_ArgCheck_Vec4, v4 ) )
		return 0;
	
	MAT4_Transform( v4, v4, *M );
	sgs_CreateVec4p( C, NULL, v4 );
	return 1;
}

static int xgm_m4i_transform_pos( SGS_CTX )
{
	VEC3 v3;
	XGM_M4_IHDR( transform_pos );
	if( !sgs_LoadArgs( C, "x", sgs_ArgCheck_Vec3, v3 ) )
		return 0;
	
	MAT4_TransformPos( v3, v3, *M );
	sgs_CreateVec3p( C, NULL, v3 );
	return 1;
}

static int xgm_m4i_transform_normal( SGS_CTX )
{
	VEC3 v3;
	XGM_M4_IHDR( transform_normal );
	if( !sgs_LoadArgs( C, "x", sgs_ArgCheck_Vec3, v3 ) )
		return 0;
	
	MAT4_TransformNormal( v3, v3, *M );
	sgs_CreateVec3p( C, NULL, v3 );
	return 1;
}


static int xgm_m4_convert( SGS_CTX, sgs_VarObj* obj, int type )
{
	XGM_OHDR;
	if( type == SGS_CONVOP_CLONE )
	{
		sgs_CreateMat4( C, NULL, hdr, 0 );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_m4_getindex( SGS_ARGS_GETINDEXFUNC )
{
	char* str;
	XGM_OHDR;
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT )
	{
		sgs_Int pos = sgs_GetInt( C, 0 );
		if( pos < 0 || pos > 15 )
			return SGS_ENOTFND;
		sgs_PushReal( C, hdr[ pos ] );
		return SGS_SUCCESS;
	}
	
	if( sgs_ParseString( C, 0, &str, NULL ) )
	{
		SGS_CASE( "identity" ) return sgs_PushCFunc( C, xgm_m4i_identity );
		SGS_CASE( "multiply" ) return sgs_PushCFunc( C, xgm_m4i_multiply );
		SGS_CASE( "multiply_left" ) return sgs_PushCFunc( C, xgm_m4i_multiply_left );
		SGS_CASE( "multiply2" ) return sgs_PushCFunc( C, xgm_m4i_multiply2 );
		SGS_CASE( "transpose" ) return sgs_PushCFunc( C, xgm_m4i_transpose );
		SGS_CASE( "transpose_from" ) return sgs_PushCFunc( C, xgm_m4i_transpose_from );
		SGS_CASE( "invert" ) return sgs_PushCFunc( C, xgm_m4i_invert );
		SGS_CASE( "invert_from" ) return sgs_PushCFunc( C, xgm_m4i_invert_from );
		
		SGS_CASE( "translate" ) return sgs_PushCFunc( C, xgm_m4i_translate );
		SGS_CASE( "translate_v3" ) return sgs_PushCFunc( C, xgm_m4i_translate_v3 );
		SGS_CASE( "rotateX" ) return sgs_PushCFunc( C, xgm_m4i_rotateX );
		SGS_CASE( "rotateY" ) return sgs_PushCFunc( C, xgm_m4i_rotateY );
		SGS_CASE( "rotateZ" ) return sgs_PushCFunc( C, xgm_m4i_rotateZ );
		SGS_CASE( "rotate_axis_angle" ) return sgs_PushCFunc( C, xgm_m4i_rotate_axis_angle );
		SGS_CASE( "rotate_axis_angle_v3" ) return sgs_PushCFunc( C, xgm_m4i_rotate_axis_angle_v3 );
		SGS_CASE( "rotate_quat" ) return sgs_PushCFunc( C, xgm_m4i_rotate_quat );
		SGS_CASE( "scale" ) return sgs_PushCFunc( C, xgm_m4i_scale );
		SGS_CASE( "scale_v3" ) return sgs_PushCFunc( C, xgm_m4i_scale_v3 );
		
		SGS_CASE( "transform" ) return sgs_PushCFunc( C, xgm_m4i_transform );
		SGS_CASE( "transform_pos" ) return sgs_PushCFunc( C, xgm_m4i_transform_pos );
		SGS_CASE( "transform_normal" ) return sgs_PushCFunc( C, xgm_m4i_transform_normal );
		
		if( *str == 'm' && str[1] && str[2] && !str[3] )
		{
			int nx = str[1] - '0';
			int ny = str[2] - '0';
			if( nx >= 0 && nx < 4 && ny >= 0 && ny < 4 )
			{
				/* rows = x, columns = y, matrix is column-major */
				sgs_PushReal( C, hdr[ ny + nx * 4 ] );
				return SGS_SUCCESS;
			}
		}
		if( !strcmp( str, "quat" ) )
		{
			QUAT out;
			QUAT_FromMatrix( out, hdr, &hdr[4], &hdr[8] );
			sgs_CreateQuatp( C, NULL, out );
			return SGS_SUCCESS;
		}
		if( !strcmp( str, "mat3" ) )
		{
			MAT3 out;
			memcpy( out[0], &hdr[0], sizeof(XGM_VT) * 3 );
			memcpy( out[1], &hdr[4], sizeof(XGM_VT) * 3 );
			memcpy( out[2], &hdr[8], sizeof(XGM_VT) * 3 );
			sgs_CreateMat3( C, NULL, *out, 0 );
			return SGS_SUCCESS;
		}
		if( !strcmp( str, "size" ) ){ sgs_PushInt( C, 16 ); return SGS_SUCCESS; }
	}
	return SGS_ENOTFND;
}

static int xgm_m4_setindex( SGS_ARGS_SETINDEXFUNC )
{
	char* str;
	XGM_OHDR;
	sgs_Real val;
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT )
	{
		sgs_Int pos = sgs_GetInt( C, 0 );
		if( pos < 0 || pos > 15 )
			return SGS_ENOTFND;
		if( sgs_ParseReal( C, 1, &val ) )
		{
			hdr[ pos ] = (XGM_VT) val;
			return SGS_SUCCESS;
		}
		else
			return SGS_EINVAL;
	}
	
	if( sgs_ParseString( C, 0, &str, NULL ) )
	{
		if( *str == 'm' && str[1] && str[2] && !str[3] )
		{
			int nx = str[1] - '0';
			int ny = str[2] - '0';
			if( nx >= 0 && nx < 4 && ny >= 0 && ny < 4 )
			{
				if( sgs_ParseReal( C, 1, &val ) )
				{
					/* rows = x, columns = y, matrix is column-major */
					hdr[ ny + nx * 4 ] = (XGM_VT) val;
					return SGS_SUCCESS;
				}
				else
					return SGS_EINVAL;
			}
		}
	}
	return SGS_ENOTFND;
}

static int xgm_m4_expr( SGS_ARGS_OBJFUNC )
{
	int type = sgs_ObjectArg( C );
	if( type == SGS_EOP_COMPARE )
	{
		int i;
		XGM_VT *v1, *v2;
		if( !sgs_IsObject( C, 0, xgm_mat4_iface ) ||
			!sgs_IsObject( C, 1, xgm_mat4_iface ) )
			return SGS_EINVAL;
		
		v1 = (XGM_VT*) sgs_GetObjectData( C, 0 );
		v2 = (XGM_VT*) sgs_GetObjectData( C, 1 );
		
		for( i = 0; i < 16; ++i )
		{
			if( v1[i] != v2[i] )
			{
				sgs_PushReal( C, v1[i] - v2[0] );
				break;
			}
		}
		if( i == 16 )
			sgs_PushReal( C, 0 );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_m4_serialize( SGS_CTX, sgs_VarObj* obj )
{
	int i;
	XGM_OHDR;
	for( i = 0; i < 16; ++i )
	{
		sgs_Serialize( C, sgs_MakeReal( hdr[i] ) );
	}
	sgs_SerializeObject( C, 16, "mat4" );
	return SGS_SUCCESS;
}

static int xgm_m4_dump( SGS_CTX, sgs_VarObj* obj, int maxdepth )
{
	char bfr[ 1024 ];
	XGM_OHDR;
	SGS_UNUSED( maxdepth );
	snprintf( bfr, 1024,
		"\n%10.6g %10.6g %10.6g %10.6g"
		"\n%10.6g %10.6g %10.6g %10.6g"
		"\n%10.6g %10.6g %10.6g %10.6g"
		"\n%10.6g %10.6g %10.6g %10.6g",
		hdr[0], hdr[4], hdr[8], hdr[12],
		hdr[1], hdr[5], hdr[9], hdr[13],
		hdr[2], hdr[6], hdr[10], hdr[14],
		hdr[3], hdr[7], hdr[11], hdr[15] );
	bfr[ 1023 ] = 0;
	sgs_PushString( C, "mat4\n(" );
	sgs_PushString( C, bfr );
	sgs_PadString( C );
	sgs_PushString( C, "\n)" );
	sgs_StringConcat( C, 3 );
	return SGS_SUCCESS;
}

static int xgm_mat4( SGS_CTX )
{
	XGM_VT v[ 16 ];
	int argc = sgs_StackSize( C );
	
	SGSFN( "mat4" );
	
	if( !argc )
	{
		int i;
		for( i = 0; i < 16; ++i )
			v[ i ] = 0;
		v[0] = v[5] = v[10] = v[15] = 1;
		sgs_CreateMat4( C, NULL, v, 0 );
		return 1;
	}
	else if( argc == 1 && sgs_ParseMat4( C, 0, v ) )
	{
		sgs_CreateMat4( C, NULL, v, 0 );
		return 1;
	}
	else if( argc >= 3 && argc <= 4 )
	{
		if( sgs_ParseVec4( C, 0, v, 0 ) &&
			sgs_ParseVec4( C, 1, v+4, 0 ) &&
			sgs_ParseVec4( C, 2, v+8, 0 ) )
		{
			if( !sgs_ParseVec4( C, 3, v+12, 0 ) )
			{
				v[12] = v[13] = v[14] = 0;
				v[15] = 1;
			}
			sgs_CreateMat4( C, NULL, v, 0 );
			return 1;
		}
	}
	else if( argc == 16 )
	{
		int i;
		for( i = 0; i < 16; ++i )
		{
			sgs_Real val;
			if( !sgs_ParseReal( C, i, &val ) )
				break;
			v[ i ] = (XGM_VT) val;
		}
		if( i == 16 )
		{
			sgs_CreateMat4( C, NULL, v, 0 );
			return 1;
		}
	}
	return sgs_Msg( C, SGS_WARNING, "expected 0 arguments or "
		"1 mat4 argument or 1 mat3 argument or 3-4 vec4 arguments or 16 real arguments" );
}



/*  F L O A T A R R A Y  */

static int xgm_fla_destruct( SGS_CTX, sgs_VarObj* obj )
{
	XGM_FLAHDR;
	if( flarr->data )
		sgs_Dealloc( flarr->data );
	return SGS_SUCCESS;
}

static int xgm_fla_convert( SGS_CTX, sgs_VarObj* obj, int type )
{
	XGM_FLAHDR;
	if( type == SGS_CONVOP_CLONE )
	{
		sgs_CreateFloatArray( C, NULL, flarr->data, flarr->size );
		return SGS_SUCCESS;
	}
	return SGS_ENOTSUP;
}

static int xgm_fla_serialize( SGS_CTX, sgs_VarObj* obj )
{
	sgs_SizeVal i;
	XGM_FLAHDR;
	for( i = 0; i < flarr->size; ++i )
	{
		sgs_Serialize( C, sgs_MakeReal( flarr->data[ i ] ) );
	}
	sgs_SerializeObject( C, flarr->size, "floatarray" );
	return SGS_SUCCESS;
}

static int xgm_fla_dump( SGS_CTX, sgs_VarObj* obj, int maxdepth )
{
	XGM_FLAHDR;
	sgs_SizeVal i, vc = flarr->size > 64 ? 64 : flarr->size;
	sgs_PushString( C, "\n{" );
	for( i = 0; i < vc; ++i )
	{
		char bfr[ 128 ];
		snprintf( bfr, 128, "\n%10.6g", flarr->data[ i ] );
		bfr[ 127 ] = 0;
		sgs_PushString( C, bfr );
	}
	if( vc < flarr->size )
	{
		sgs_PushString( C, "\n..." );
		vc++;
	}
	if( vc > 1 ) /* concatenate all numbers and "..." if it exists" */
		sgs_StringConcat( C, vc );
	sgs_PadString( C );
	sgs_PushString( C, "\n}" );
	sgs_StringConcat( C, 3 );
	return SGS_SUCCESS;
}

static int xgm_fla_getindex_aabb2( SGS_CTX, xgm_vtarray* flarr )
{
	if( flarr->size < 2 )
	{
		XGM_WARNING( "cannot get AABB2 of floatarray with size < 2" );
		return SGS_EINPROC;
	}
	else
	{
		sgs_SizeVal i;
		XGM_VT bb[4] = {
			flarr->data[0], flarr->data[1], flarr->data[0], flarr->data[1]
		};
		for( i = 2; i < flarr->size; i += 2 )
		{
			XGM_VT* pp = flarr->data + i;
			XGM_BB2_EXPAND_V2( bb, pp );
		}
		sgs_CreateAABB2p( C, NULL, bb );
		return SGS_SUCCESS;
	}
}

static int xgm_fla_getindex_aabb3( SGS_CTX, xgm_vtarray* flarr )
{
	if( flarr->size < 3 )
	{
		XGM_WARNING( "cannot get AABB3 of floatarray with size < 3" );
		return SGS_EINPROC;
	}
	else
	{
		sgs_SizeVal i;
		XGM_VT bb[6] = {
			flarr->data[0], flarr->data[1], flarr->data[2], flarr->data[0], flarr->data[1], flarr->data[2]
		};
		for( i = 3; i < flarr->size; i += 3 )
		{
			XGM_VT* pp = flarr->data + i;
			XGM_BB3_EXPAND_V3( bb, pp );
		}
		sgs_CreateAABB3p( C, NULL, bb );
		return SGS_SUCCESS;
	}
}

/* methods */
#define XGM_FLA_IHDR( funcname ) xgm_vtarray* flarr; \
	if( !SGS_PARSE_METHOD( C, xgm_floatarr_iface, flarr, floatarray, funcname ) ) return 0;

#define XGM_FLA_UNOPMETHOD( opname, opcode ) \
static int xgm_fla_##opname( SGS_CTX ) \
{ \
	sgs_SizeVal i; \
	XGM_VT R, A; \
	SGS_UNUSED( A ); \
	 \
	XGM_FLA_IHDR( opname ) \
	 \
	for( i = 0; i < flarr->size; ++i ) \
	{ \
		A = flarr->data[ i ]; opcode; flarr->data[ i ] = R; \
	} \
	return 0; \
}

XGM_FLA_UNOPMETHOD( clear, R = 0 );
XGM_FLA_UNOPMETHOD( set1, R = 1 );
XGM_FLA_UNOPMETHOD( negate, R = -A );

#define XGM_FLA_BINOPMETHOD( opname, opcode ) \
static int xgm_fla_##opname( SGS_CTX ) \
{ \
	sgs_Real rl; \
	XGM_VT v4f1[ 4 ] = {0}; \
	XGM_VT* vfa1 = v4f1; \
	sgs_SizeVal i, sz, unit1 = 1, stride1 = 0; \
	XGM_VT R, A, B; \
	SGS_UNUSED( A ); \
	 \
	XGM_FLA_IHDR( opname ) \
	 \
	if( sgs_ParseVec2( C, 0, vfa1, 1 ) ) unit1 = 2; \
	else if( sgs_ParseVec3( C, 0, vfa1, 1 ) ) unit1 = 3; \
	else if( sgs_ParseVec4( C, 0, vfa1, 1 ) ) unit1 = 4; \
	else if( sgs_ParseFloatArray( C, 0, &vfa1, &sz ) ) \
	{ \
		if( sz != flarr->size ) \
			return XGM_WARNING( "array sizes don't match" ); \
		stride1 = 1; \
	} \
	else if( sgs_ParseReal( C, 0, &rl ) ){ vfa1[0] = (XGM_VT) rl; unit1 = 1; } \
	else return XGM_WARNING( "expected real, vec[2|3|4] or floatarray" ); \
	 \
	for( i = 0; i < flarr->size; ++i ) \
	{ \
		A = flarr->data[ i ]; B = vfa1[ i % unit1 ]; opcode; flarr->data[ i ] = R; \
		vfa1 += stride1; \
	} \
	return 0; \
}

XGM_FLA_BINOPMETHOD( assign, R = B );
XGM_FLA_BINOPMETHOD( negate_from, R = -B );
XGM_FLA_BINOPMETHOD( add_assign, R = A + B );
XGM_FLA_BINOPMETHOD( sub_assign, R = A - B );
XGM_FLA_BINOPMETHOD( mul_assign, R = A * B );
XGM_FLA_BINOPMETHOD( div_assign, R = A / B );
XGM_FLA_BINOPMETHOD( mod_assign, R = fmodf( A, B ) );
XGM_FLA_BINOPMETHOD( pow_assign, R = (XGM_VT) pow( A, B ) );

#define XGM_FLA_TEROPMETHOD( opname, opcode ) \
static int xgm_fla_##opname( SGS_CTX ) \
{ \
	sgs_Real rl; \
	XGM_VT v4f1[ 4 ] = {0}, v4f2[ 4 ] = {0}; \
	XGM_VT* vfa1 = v4f1, *vfa2 = v4f2; \
	sgs_SizeVal i, sz, unit1 = 1, unit2 = 1, stride1 = 0, stride2 = 0; \
	XGM_VT R, A, B, T; \
	SGS_UNUSED( T ); \
	 \
	XGM_FLA_IHDR( opname ) \
	 \
	if( sgs_ParseVec2( C, 0, vfa1, 1 ) ) unit1 = 2; \
	else if( sgs_ParseVec3( C, 0, vfa1, 1 ) ) unit1 = 3; \
	else if( sgs_ParseVec4( C, 0, vfa1, 1 ) ) unit1 = 4; \
	else if( sgs_ParseFloatArray( C, 0, &vfa1, &sz ) ) \
	{ \
		if( sz != flarr->size ) \
			return XGM_WARNING( "array sizes (this : argument 1) don't match" ); \
		stride1 = 1; \
	} \
	else if( sgs_ParseReal( C, 0, &rl ) ){ vfa1[0] = (XGM_VT) rl; unit1 = 1; } \
	else return XGM_WARNING( "expected real, vec[2|3|4] or floatarray as argument 1" ); \
	 \
	if( sgs_ParseVec2( C, 1, vfa2, 1 ) ) unit2 = 2; \
	else if( sgs_ParseVec3( C, 1, vfa2, 1 ) ) unit2 = 3; \
	else if( sgs_ParseVec4( C, 1, vfa2, 1 ) ) unit2 = 4; \
	else if( sgs_ParseFloatArray( C, 1, &vfa2, &sz ) ) \
	{ \
		if( sz != flarr->size ) \
			return XGM_WARNING( "array sizes (this : argument 2) don't match" ); \
		stride2 = 1; \
	} \
	else if( sgs_ParseReal( C, 1, &rl ) ){ vfa2[0] = (XGM_VT) rl; unit2 = 1; } \
	else return XGM_WARNING( "expected real, vec[2|3|4] or floatarray as argument 2" ); \
	 \
	for( i = 0; i < flarr->size; ++i ) \
	{ \
		T = flarr->data[ i ]; A = vfa1[ i % unit1 ]; B = vfa2[ i % unit2 ]; opcode; flarr->data[ i ] = R; \
		vfa1 += stride1; \
		vfa2 += stride2; \
	} \
	return 0; \
}

XGM_FLA_TEROPMETHOD( add, R = A + B );
XGM_FLA_TEROPMETHOD( sub, R = A - B );
XGM_FLA_TEROPMETHOD( mul, R = A * B );
XGM_FLA_TEROPMETHOD( div, R = A / B );
XGM_FLA_TEROPMETHOD( mod, R = fmodf( A, B ) );
XGM_FLA_TEROPMETHOD( pow, R = (XGM_VT) pow( A, B ) );

static XGM_VT randlerp( XGM_VT A, XGM_VT B )
{
	XGM_VT t = (XGM_VT) rand() / (XGM_VT) RAND_MAX; return A * (1-t) + B * t;
}
XGM_FLA_TEROPMETHOD( randbox, R = randlerp( A, B ) );
XGM_FLA_TEROPMETHOD( randext, R = randlerp( A - B, A + B ) );

XGM_FLA_TEROPMETHOD( multiply_add_assign, R = T + A * B );
XGM_FLA_TEROPMETHOD( lerp_to, R = T * (1-B) + A * B );

/* WP: assuming string buffer data alignment >= 8 */
#define XGM_FLA_CONVMETHOD( opname, typename ) \
static int xgm_fla_##opname( SGS_CTX ) \
{ \
	float scale = 1; \
	sgs_SizeVal i; \
	typename* data; \
	XGM_FLA_IHDR( opname ) \
	if( !sgs_LoadArgs( C, "|f", &scale ) ) \
		return 0; \
	 \
	sgs_PushStringBuf( C, NULL, (sgs_SizeVal) ( sizeof(typename) * (size_t) flarr->size ) ); \
	data = (typename*) (void*) sgs_GetStringPtr( C, -1 ); \
	for( i = 0; i < flarr->size; ++i ) \
	{ \
		data[ i ] = (typename) ( flarr->data[ i ] * scale ); \
	} \
	return 1; \
}

XGM_FLA_CONVMETHOD( to_int8_buffer, int8_t );
XGM_FLA_CONVMETHOD( to_int16_buffer, int16_t );
XGM_FLA_CONVMETHOD( to_int32_buffer, int32_t );
XGM_FLA_CONVMETHOD( to_int64_buffer, int64_t );
XGM_FLA_CONVMETHOD( to_uint8_buffer, uint8_t );
XGM_FLA_CONVMETHOD( to_uint16_buffer, uint16_t );
XGM_FLA_CONVMETHOD( to_uint32_buffer, uint32_t );
XGM_FLA_CONVMETHOD( to_uint64_buffer, uint64_t );
XGM_FLA_CONVMETHOD( to_float32_buffer, float );
XGM_FLA_CONVMETHOD( to_float64_buffer, double );

static int xgm_fla_getindex( SGS_ARGS_GETINDEXFUNC )
{
	XGM_FLAHDR;
	
	if( sgs_ObjectArg( C ) )
	{
		char* str;
		if( sgs_ParseString( C, 0, &str, NULL ) )
		{
			if( !strcmp( str, "aabb2" ) ) return xgm_fla_getindex_aabb2( C, flarr );
			if( !strcmp( str, "aabb3" ) ) return xgm_fla_getindex_aabb3( C, flarr );
			if( !strcmp( str, "size" ) ){ sgs_PushInt( C, flarr->size ); return SGS_SUCCESS; }
			if( !strcmp( str, "size2" ) ){ sgs_PushInt( C, flarr->size / 2 ); return SGS_SUCCESS; }
			if( !strcmp( str, "size3" ) ){ sgs_PushInt( C, flarr->size / 3 ); return SGS_SUCCESS; }
			if( !strcmp( str, "size4" ) ){ sgs_PushInt( C, flarr->size / 4 ); return SGS_SUCCESS; }
			if( !strcmp( str, "size16" ) ){ sgs_PushInt( C, flarr->size / 16 ); return SGS_SUCCESS; }
			
			SGS_CASE( "clear" ) return sgs_PushCFunc( C, xgm_fla_clear );
			SGS_CASE( "set1" ) return sgs_PushCFunc( C, xgm_fla_set1 );
			SGS_CASE( "negate" ) return sgs_PushCFunc( C, xgm_fla_negate );
			
			SGS_CASE( "assign" ) return sgs_PushCFunc( C, xgm_fla_assign );
			SGS_CASE( "negate_from" ) return sgs_PushCFunc( C, xgm_fla_negate_from );
			SGS_CASE( "add_assign" ) return sgs_PushCFunc( C, xgm_fla_add_assign );
			SGS_CASE( "sub_assign" ) return sgs_PushCFunc( C, xgm_fla_sub_assign );
			SGS_CASE( "mul_assign" ) return sgs_PushCFunc( C, xgm_fla_mul_assign );
			SGS_CASE( "div_assign" ) return sgs_PushCFunc( C, xgm_fla_div_assign );
			SGS_CASE( "mod_assign" ) return sgs_PushCFunc( C, xgm_fla_mod_assign );
			SGS_CASE( "pow_assign" ) return sgs_PushCFunc( C, xgm_fla_pow_assign );
			
			SGS_CASE( "add" ) return sgs_PushCFunc( C, xgm_fla_add );
			SGS_CASE( "sub" ) return sgs_PushCFunc( C, xgm_fla_sub );
			SGS_CASE( "mul" ) return sgs_PushCFunc( C, xgm_fla_mul );
			SGS_CASE( "div" ) return sgs_PushCFunc( C, xgm_fla_div );
			SGS_CASE( "mod" ) return sgs_PushCFunc( C, xgm_fla_mod );
			SGS_CASE( "pow" ) return sgs_PushCFunc( C, xgm_fla_pow );
			SGS_CASE( "randbox" ) return sgs_PushCFunc( C, xgm_fla_randbox );
			SGS_CASE( "randext" ) return sgs_PushCFunc( C, xgm_fla_randext );
			SGS_CASE( "multiply_add_assign" ) return sgs_PushCFunc( C, xgm_fla_multiply_add_assign );
			SGS_CASE( "lerp_to" ) return sgs_PushCFunc( C, xgm_fla_lerp_to );
			
			SGS_CASE( "to_int8_buffer" ) return sgs_PushCFunc( C, xgm_fla_to_int8_buffer );
			SGS_CASE( "to_int16_buffer" ) return sgs_PushCFunc( C, xgm_fla_to_int16_buffer );
			SGS_CASE( "to_int32_buffer" ) return sgs_PushCFunc( C, xgm_fla_to_int32_buffer );
			SGS_CASE( "to_int64_buffer" ) return sgs_PushCFunc( C, xgm_fla_to_int64_buffer );
			SGS_CASE( "to_uint8_buffer" ) return sgs_PushCFunc( C, xgm_fla_to_uint8_buffer );
			SGS_CASE( "to_uint16_buffer" ) return sgs_PushCFunc( C, xgm_fla_to_uint16_buffer );
			SGS_CASE( "to_uint32_buffer" ) return sgs_PushCFunc( C, xgm_fla_to_uint32_buffer );
			SGS_CASE( "to_uint64_buffer" ) return sgs_PushCFunc( C, xgm_fla_to_uint64_buffer );
			SGS_CASE( "to_float32_buffer" ) return sgs_PushCFunc( C, xgm_fla_to_float32_buffer );
			SGS_CASE( "to_float64_buffer" ) return sgs_PushCFunc( C, xgm_fla_to_float64_buffer );
		}
	}
	else
	{
		sgs_SizeVal pos = (sgs_SizeVal) sgs_GetInt( C, 0 );
		if( pos < 0 || pos > flarr->size )
			return SGS_ENOTFND;
		sgs_PushReal( C, flarr->data[ pos ] );
		return SGS_SUCCESS;
	}
	
	return SGS_ENOTFND;
}

static int xgm_fla_setindex( SGS_ARGS_SETINDEXFUNC )
{
	sgs_Real val;
	XGM_FLAHDR;
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT )
	{
		sgs_SizeVal pos = (sgs_SizeVal) sgs_GetInt( C, 0 );
		if( pos < 0 || pos > flarr->size )
			return SGS_ENOTFND;
		if( sgs_ParseReal( C, 1, &val ) )
		{
			flarr->data[ pos ] = (XGM_VT) val;
			return SGS_SUCCESS;
		}
		else
			return SGS_EINVAL;
	}
	return SGS_ENOTFND;
}

static XGM_VT* _xgm_pushvxa( SGS_CTX, sgs_SizeVal size, sgs_SizeVal cc )
{
	xgm_vtarray* np = (xgm_vtarray*) sgs_CreateObjectIPA( C, NULL, sizeof(xgm_vtarray), xgm_floatarr_iface );
	np->size = size * cc;
	np->mem = size * cc;
	np->data = size ? sgs_Alloc_n( XGM_VT, (size_t)( np->mem * cc ) ) : NULL;
	return np->data;
}

static int xgm_floatarray_buffer( SGS_CTX )
{
	XGM_VT* data;
	sgs_Int size;
	sgs_SizeVal i, sz;
	SGSFN( "floatarray_buffer" );
	
	if( !sgs_LoadArgs( C, "i", &size ) )
		return 0;
	sz = (sgs_SizeVal) size;
	data = _xgm_pushvxa( C, sz, 1 );
	for( i = 0; i < sz; ++i )
		data[ i ] = 0;
	return 1;
}

static int xgm_floatarray( SGS_CTX )
{
	sgs_SizeVal asize;
	
	SGSFN( "floatarray" );
	/* create floatarray from array */
	if( ( asize = sgs_ArraySize( sgs_StackItem( C, 0 ) ) ) >= 0 )
	{
		XGM_VT* fdata = _xgm_pushvxa( C, asize, 1 );
		sgs_PushIterator( C, sgs_StackItem( C, 0 ) );
		while( sgs_IterAdvance( C, sgs_StackItem( C, -1 ) ) > 0 )
		{
			sgs_IterPushData( C, sgs_StackItem( C, -1 ), 0, 1 );
			if( !sgs_ParseVT( C, -1, fdata ) )
				return XGM_WARNING( "failed to parse array" );
			fdata++;
			sgs_Pop( C, 1 );
		}
		sgs_Pop( C, 1 );
		return 1;
	}
	
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT || sgs_ItemType( C, 0 ) == SGS_VT_REAL )
	{
		sgs_StkIdx i, ssz = sgs_StackSize( C );
		XGM_VT* fdata = _xgm_pushvxa( C, ssz, 1 );
		for( i = 0; i < ssz; ++i )
		{
			fdata[ 0 ] = (XGM_VT) sgs_GetReal( C, i );
			fdata++;
		}
		return 1;
	}
	
	return XGM_WARNING( "expected array of floats or float list" );
}

static int xgm_vec2array( SGS_CTX )
{
	sgs_SizeVal asize;
	
	SGSFN( "vec2array" );
	/* create vec2array from array */
	if( ( asize = sgs_ArraySize( sgs_StackItem( C, 0 ) ) ) >= 0 )
	{
		XGM_VT* fdata = _xgm_pushvxa( C, asize, 2 );
		sgs_PushIterator( C, sgs_StackItem( C, 0 ) );
		while( sgs_IterAdvance( C, sgs_StackItem( C, -1 ) ) > 0 )
		{
			sgs_IterPushData( C, sgs_StackItem( C, -1 ), 0, 1 );
			if( !sgs_ParseVec2( C, -1, fdata, 0 ) )
				return XGM_WARNING( "failed to parse array" );
			fdata += 2;
			sgs_Pop( C, 1 );
		}
		sgs_Pop( C, 1 );
		return 1;
	}
	
	/* create vec2array from list of vec2 */
	if( sgs_IsObject( C, 0, xgm_vec2_iface ) )
	{
		sgs_StkIdx i, ssz = sgs_StackSize( C );
		XGM_VT* fdata = _xgm_pushvxa( C, ssz, 2 );
		for( i = 0; i < ssz; ++i )
		{
			if( !sgs_ParseVec2( C, i, fdata, 1 ) )
				return sgs_Msg( C, SGS_WARNING, "failed to parse argument %d as vec2", i + 1 );
			fdata += 2;
		}
		return 1;
	}
	
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT || sgs_ItemType( C, 0 ) == SGS_VT_REAL )
	{
		sgs_StkIdx i, ssz = sgs_StackSize( C );
		XGM_VT* fdata;
		if( ssz % 2 != 0 )
			return XGM_WARNING( "scalar argument count not multiple of 2" );
		fdata = _xgm_pushvxa( C, ssz, 2 );
		for( i = 0; i < ssz; i += 2 )
		{
			fdata[ 0 ] = (XGM_VT) sgs_GetReal( C, i+0 );
			fdata[ 1 ] = (XGM_VT) sgs_GetReal( C, i+1 );
			fdata += 2;
		}
		return 1;
	}
	
	return XGM_WARNING( "expected array of vec2, vec2 list or float list" );
}

static int xgm_vec3array( SGS_CTX )
{
	sgs_SizeVal asize;
	
	SGSFN( "vec3array" );
	/* create vec3array from array */
	if( ( asize = sgs_ArraySize( sgs_StackItem( C, 0 ) ) ) >= 0 )
	{
		XGM_VT* fdata = _xgm_pushvxa( C, asize, 3 );
		sgs_PushIterator( C, sgs_StackItem( C, 0 ) );
		while( sgs_IterAdvance( C, sgs_StackItem( C, -1 ) ) > 0 )
		{
			sgs_IterPushData( C, sgs_StackItem( C, -1 ), 0, 1 );
			if( !sgs_ParseVec3( C, -1, fdata, 0 ) )
				return XGM_WARNING( "failed to parse array" );
			fdata += 3;
			sgs_Pop( C, 1 );
		}
		sgs_Pop( C, 1 );
		return 1;
	}
	
	/* create vec3array from list of vec3 */
	if( sgs_IsObject( C, 0, xgm_vec3_iface ) )
	{
		sgs_StkIdx i, ssz = sgs_StackSize( C );
		XGM_VT* fdata = _xgm_pushvxa( C, ssz, 3 );
		for( i = 0; i < ssz; ++i )
		{
			if( !sgs_ParseVec3( C, i, fdata, 0 ) )
				return sgs_Msg( C, SGS_WARNING, "failed to parse argument %d as vec3", i + 1 );
			fdata += 3;
		}
		return 1;
	}
	
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT || sgs_ItemType( C, 0 ) == SGS_VT_REAL )
	{
		sgs_StkIdx i, ssz = sgs_StackSize( C );
		XGM_VT* fdata;
		if( ssz % 3 != 0 )
			return XGM_WARNING( "scalar argument count not multiple of 3" );
		fdata = _xgm_pushvxa( C, ssz, 3 );
		for( i = 0; i < ssz; i += 3 )
		{
			fdata[ 0 ] = (XGM_VT) sgs_GetReal( C, i+0 );
			fdata[ 1 ] = (XGM_VT) sgs_GetReal( C, i+1 );
			fdata[ 2 ] = (XGM_VT) sgs_GetReal( C, i+2 );
			fdata += 3;
		}
		return 1;
	}
	
	return XGM_WARNING( "expected array of vec3, vec3 list or float list" );
}

static int xgm_vec4array( SGS_CTX )
{
	sgs_SizeVal asize;
	
	SGSFN( "vec4array" );
	/* create vec4array from array */
	if( ( asize = sgs_ArraySize( sgs_StackItem( C, 0 ) ) ) >= 0 )
	{
		XGM_VT* fdata = _xgm_pushvxa( C, asize, 4 );
		sgs_PushIterator( C, sgs_StackItem( C, 0 ) );
		while( sgs_IterAdvance( C, sgs_StackItem( C, -1 ) ) > 0 )
		{
			sgs_IterPushData( C, sgs_StackItem( C, -1 ), 0, 1 );
			if( !sgs_ParseVec4( C, -1, fdata, 0 ) )
				return XGM_WARNING( "failed to parse array" );
			fdata += 4;
			sgs_Pop( C, 1 );
		}
		sgs_Pop( C, 1 );
		return 1;
	}
	
	/* create vec4array from list of vec4 */
	if( sgs_IsObject( C, 0, xgm_vec4_iface ) )
	{
		sgs_StkIdx i, ssz = sgs_StackSize( C );
		XGM_VT* fdata = _xgm_pushvxa( C, ssz, 4 );
		for( i = 0; i < ssz; ++i )
		{
			if( !sgs_ParseVec4( C, i, fdata, 1 ) )
				return sgs_Msg( C, SGS_WARNING, "failed to parse argument %d as vec4", i + 1 );
			fdata += 4;
		}
		return 1;
	}
	
	if( sgs_ItemType( C, 0 ) == SGS_VT_INT || sgs_ItemType( C, 0 ) == SGS_VT_REAL )
	{
		sgs_StkIdx i, ssz = sgs_StackSize( C );
		XGM_VT* fdata;
		if( ssz % 4 != 0 )
			return XGM_WARNING( "scalar argument count not multiple of 4" );
		fdata = _xgm_pushvxa( C, ssz, 4 );
		for( i = 0; i < ssz; i += 4 )
		{
			fdata[ 0 ] = (XGM_VT) sgs_GetReal( C, i+0 );
			fdata[ 1 ] = (XGM_VT) sgs_GetReal( C, i+1 );
			fdata[ 2 ] = (XGM_VT) sgs_GetReal( C, i+2 );
			fdata[ 3 ] = (XGM_VT) sgs_GetReal( C, i+3 );
			fdata += 4;
		}
		return 1;
	}
	
	return XGM_WARNING( "expected array of vec4, vec4 list or float list" );
}

#define XGM_FLA_BUFCREATEFUNC( funcname, typename ) \
static int xgm_##funcname( SGS_CTX ) \
{ \
	float scale = 1; \
	sgs_SizeVal i, bufsize, offset = 0, stride = 1; \
	typename* bufdata; \
	XGM_VT* data; \
	if( !sgs_LoadArgs( C, "m|fll", &bufdata, &bufsize, &scale, &stride, &offset ) ) \
		return 0; \
	 \
	bufsize /= (sgs_SizeVal) sizeof( typename ); \
	data = _xgm_pushvxa( C, bufsize, 1 ); \
	for( i = offset; i < bufsize; i += stride ) \
		*data++ = scale * (XGM_VT) bufdata[ i ]; \
	return 1; \
}

XGM_FLA_BUFCREATEFUNC( floatarray_from_int8_buffer, int8_t );
XGM_FLA_BUFCREATEFUNC( floatarray_from_int16_buffer, int16_t );
XGM_FLA_BUFCREATEFUNC( floatarray_from_int32_buffer, int32_t );
XGM_FLA_BUFCREATEFUNC( floatarray_from_int64_buffer, int64_t );
XGM_FLA_BUFCREATEFUNC( floatarray_from_uint8_buffer, uint8_t );
XGM_FLA_BUFCREATEFUNC( floatarray_from_uint16_buffer, uint16_t );
XGM_FLA_BUFCREATEFUNC( floatarray_from_uint32_buffer, uint32_t );
XGM_FLA_BUFCREATEFUNC( floatarray_from_uint64_buffer, uint64_t );
XGM_FLA_BUFCREATEFUNC( floatarray_from_float32_buffer, float );
XGM_FLA_BUFCREATEFUNC( floatarray_from_float64_buffer, double );



/* UTILITY FUNCTIONS */

static int xgm_ray_plane_intersect( SGS_CTX )
{
	/* vec3 ray_pos, vec3 ray_dir, vec4 plane;
		returns <distance to intersection, signed origin distance from plane>
			\ <false> on (near-)parallel */
	XGM_VT pos[3], dir[3], plane[4], sigdst, dirdot;
	SGSFN( "ray_plane_intersect" );
	if( !sgs_LoadArgs( C, "xxx", sgs_ArgCheck_Vec3, pos, sgs_ArgCheck_Vec3, dir, sgs_ArgCheck_Vec4, plane ) )
		return 0;
	
	sigdst = XGM_VMUL_INNER3( pos, plane ) - plane[3];
	dirdot = XGM_VMUL_INNER3( dir, plane );
	
	if( fabs( dirdot ) < XGM_SMALL_VT )
	{
		sgs_PushBool( C, 0 );
		return 1;
	}
	else
	{
		sgs_PushReal( C, -sigdst / dirdot );
		sgs_PushReal( C, sigdst );
		return 2;
	}
}

static int xgm_ray_sphere_intersect( SGS_CTX )
{
	/* vec3 ray_pos, vec3 ray_dir, vec3 sphere_pos, float radius;
		returns <distance to intersection> \ false on no intersection */
	XGM_VT pos[3], dir[3], sphpos[3], sphrad, r2s[3], a, b;
	SGSFN( "ray_sphere_intersect" );
	if( !sgs_LoadArgs( C, "xxxf", sgs_ArgCheck_Vec3, pos, sgs_ArgCheck_Vec3, dir, sgs_ArgCheck_Vec3, sphpos, &sphrad ) )
		return 0;
	
	XGM_V3_SUB( r2s, sphpos, pos );
	a = XGM_VMUL_INNER3( dir, r2s );
	if( a < 0 )
	{
		return sgs_PushBool( C, 0 );
	}
	b = XGM_VMUL_INNER3( r2s, r2s ) - a * a;
	if( b > sphrad * sphrad )
	{
		return sgs_PushBool( C, 0 );
	}
	return sgs_PushReal( C, a - sqrtf( sphrad * sphrad - b ) );
}


//
// ADAPTATION BEGIN
// - http://geomalgorithms.com/a07-_distance.html
// - http://geomalgorithms.com/a02-_lines.html
// Copyright 2001 softSurfer, 2012 Dan Sunday
// This code may be freely used and modified for any purpose
// providing that this copyright notice is included with it.
// SoftSurfer makes no warranty for this code, and cannot be held
// liable for any real or imagined damage resulting from its use.
// Users of this code must verify correctness for their application.

static int xgm_distance_lines( SGS_CTX )
{
	/* vec3 line 1 point 1, l1p2, l2p1, l2p2;
		returns <distance between two lines> */
	XGM_VT l1p1[3], l1p2[3], l2p1[3], l2p2[3], u[3], v[3], w[3], dP[3], a, b, c, d, e, D, sc, tc;
	SGSFN( "distance_lines" );
	if( !sgs_LoadArgs( C, "xxxx", sgs_ArgCheck_Vec3, l1p1, sgs_ArgCheck_Vec3, l1p2, sgs_ArgCheck_Vec3, l2p1, sgs_ArgCheck_Vec3, l2p2 ) )
		return 0;
	
	XGM_V3_SUB( u, l1p2, l1p1 );
	XGM_V3_SUB( v, l2p2, l2p1 );
	XGM_V3_SUB( w, l1p1, l2p1 );
	
	a = XGM_VMUL_INNER3( u, u ); // always >= 0
	b = XGM_VMUL_INNER3( u, v );
	c = XGM_VMUL_INNER3( v, v ); // always >= 0
	d = XGM_VMUL_INNER3( u, w );
	e = XGM_VMUL_INNER3( v, w );
	D = a*c - b*b; // always >= 0
	
	// compute the line parameters of the two closest points
	if( D < XGM_SMALL_VT ) // the lines are almost parallel
	{
		sc = 0.0f;
		tc = ( b > c ? d / b : e / c ); // use the largest denominator
	}
	else
	{
		sc = ( b * e - c * d ) / D;
		tc = ( a * e - b * d ) / D;
	}
	
	// get the difference of the two closest points
	// dP = w + (sc * u) - (tc * v) // =  L1(sc) - L2(tc)
	XGM_V3_SCALE( u, sc );
	XGM_V3_SCALE( v, tc );
	XGM_V3_ADD( dP, w, u );
	XGM_V3_SUB( dP, dP, v );
	
	return sgs_PushReal( C, sqrtf( XGM_VMUL_INNER3( dP, dP ) ) ); // return the closest distance
}

static int xgm_distance_line_segments( SGS_CTX )
{
	/* vec3 line 1 point 1, l1p2, l2p1, l2p2;
		returns <distance between two line segments> */
	XGM_VT l1p1[3], l1p2[3], l2p1[3], l2p2[3], u[3], v[3], w[3], dP[3], a, b, c, d, e, D, sc, tc, sN, sD, tN, tD;
	SGSFN( "distance_line_segments" );
	if( !sgs_LoadArgs( C, "xxxx", sgs_ArgCheck_Vec3, l1p1, sgs_ArgCheck_Vec3, l1p2, sgs_ArgCheck_Vec3, l2p1, sgs_ArgCheck_Vec3, l2p2 ) )
		return 0;
	
	XGM_V3_SUB( u, l1p2, l1p1 );
	XGM_V3_SUB( v, l2p2, l2p1 );
	XGM_V3_SUB( w, l1p1, l2p1 );
	
	a = XGM_VMUL_INNER3( u, u ); // always >= 0
	b = XGM_VMUL_INNER3( u, v );
	c = XGM_VMUL_INNER3( v, v ); // always >= 0
	d = XGM_VMUL_INNER3( u, w );
	e = XGM_VMUL_INNER3( v, w );
	D = a*c - b*b; // always >= 0
	
	sD = D; // sc = sN / sD, default sD = D >= 0
	tD = D; // tc = tN / tD, default tD = D >= 0
	
	// compute the line parameters of the two closest points
	if( D < XGM_SMALL_VT ) // the lines are almost parallel
	{
	    sN = 0.0f; // force using point P0 on segment S1
	    sD = 1.0f; // to prevent possible division by 0.0 later
	    tN = e;
	    tD = c;
	}
	else // get the closest points on the infinite lines
	{
		sN = ( b * e - c * d );
		tN = ( a * e - b * d );
		if( sN < 0.0f ) // sc < 0 => the s=0 edge is visible
		{
			sN = 0.0f;
			tN = e;
			tD = c;
		}
		else if( sN > sD ) // sc > 1  => the s=1 edge is visible
		{
			sN = sD;
			tN = e + b;
			tD = c;
		}
	}

	if( tN < 0.0f ) // tc < 0 => the t=0 edge is visible
	{
		tN = 0.0f;
		// recompute sc for this edge
		if( -d < 0.0f )
			sN = 0.0f;
		else if( -d > a )
			sN = sD;
		else
		{
			sN = -d;
			sD = a;
		}
	}
	else if( tN > tD ) // tc > 1  => the t=1 edge is visible
	{
		tN = tD;
		// recompute sc for this edge
		if( -d + b < 0.0f )
			sN = 0;
		else if( -d + b > a )
			sN = sD;
		else
		{
			sN = -d + b;
			sD = a;
		}
	}
	// finally do the division to get sc and tc
	sc = fabs( sN ) < XGM_SMALL_VT ? 0.0f : sN / sD;
	tc = fabs( tN ) < XGM_SMALL_VT ? 0.0f : tN / tD;
	
	// get the difference of the two closest points
	// dP = w + (sc * u) - (tc * v) // =  S1(sc) - S2(tc)
	XGM_V3_SCALE( u, sc );
	XGM_V3_SCALE( v, tc );
	XGM_V3_ADD( dP, w, u );
	XGM_V3_SUB( dP, dP, v );
	
	return sgs_PushReal( C, sqrtf( XGM_VMUL_INNER3( dP, dP ) ) ); // return the closest distance
}

static int xgm_distance_point_line( SGS_CTX )
{
	/* vec3 point, line point 1, line point 2;
		returns <distance between point and line> */
	XGM_VT pt[3], lp1[3], lp2[3], v[3], w[3], c1, c2, b;
	SGSFN( "distance_point_line" );
	if( !sgs_LoadArgs( C, "xxx", sgs_ArgCheck_Vec3, pt, sgs_ArgCheck_Vec3, lp1, sgs_ArgCheck_Vec3, lp2 ) )
		return 0;
	
	XGM_V3_SUB( v, lp2, lp1 );
	XGM_V3_SUB( w, pt, lp1 );
	
	c1 = XGM_VMUL_INNER3( w, v );
	c2 = XGM_VMUL_INNER3( v, v );
	b = c1 / c2;
	
	XGM_V3_SCALE( v, b );
	XGM_V3_ADD( v, lp1, v );
	XGM_V3_SUB( v, pt, v );
	
	return sgs_PushReal( C, sqrtf( XGM_VMUL_INNER3( v, v ) ) );
}

static int xgm_distance_point_line_segment( SGS_CTX )
{
	/* vec3 point, line point 1, line point 2;
		returns <distance between point and line segment> */
	XGM_VT pt[3], lp1[3], lp2[3], v[3], w[3], c1, c2, b;
	SGSFN( "distance_point_line_segment" );
	if( !sgs_LoadArgs( C, "xxx", sgs_ArgCheck_Vec3, pt, sgs_ArgCheck_Vec3, lp1, sgs_ArgCheck_Vec3, lp2 ) )
		return 0;
	
	XGM_V3_SUB( v, lp2, lp1 );
	XGM_V3_SUB( w, pt, lp1 );
	
	c1 = XGM_VMUL_INNER3( w, v );
	if( c1 <= 0 )
	{
		XGM_V3_SUB( v, pt, lp1 );
		goto end;
	}
	c2 = XGM_VMUL_INNER3( v, v );
	if( c2 <= c1 )
	{
		XGM_V3_SUB( v, pt, lp2 );
		goto end;
	}
	b = c1 / c2;
	
	XGM_V3_SCALE( v, b );
	XGM_V3_ADD( v, lp1, v );
	XGM_V3_SUB( v, pt, v );
	
end:
	return sgs_PushReal( C, sqrtf( XGM_VMUL_INNER3( v, v ) ) );
}

// ADAPTATION END
//



sgs_ObjInterface xgm_vec2_iface[1] =
{{
	"vec2",
	NULL, NULL,
	xgm_v2_getindex, xgm_v2_setindex,
	xgm_v2_convert, xgm_v2_serialize, xgm_v2_dump, NULL,
	NULL, xgm_v2_expr
}};

sgs_ObjInterface xgm_vec3_iface[1] =
{{
	"vec3",
	NULL, NULL,
	xgm_v3_getindex, xgm_v3_setindex,
	xgm_v3_convert, xgm_v3_serialize, xgm_v3_dump, NULL,
	NULL, xgm_v3_expr
}};

sgs_ObjInterface xgm_vec4_iface[1] =
{{
	"vec4",
	NULL, NULL,
	xgm_v4_getindex, xgm_v4_setindex,
	xgm_v4_convert, xgm_v4_serialize, xgm_v4_dump, NULL,
	NULL, xgm_v4_expr
}};

sgs_ObjInterface xgm_aabb2_iface[1] =
{{
	"aabb2",
	NULL, NULL,
	xgm_b2_getindex, xgm_b2_setindex,
	xgm_b2_convert, xgm_b2_serialize, xgm_b2_dump, NULL,
	NULL, xgm_b2_expr
}};

sgs_ObjInterface xgm_aabb3_iface[1] =
{{
	"aabb3",
	NULL, NULL,
	xgm_b3_getindex, xgm_b3_setindex,
	xgm_b3_convert, xgm_b3_serialize, xgm_b3_dump, NULL,
	NULL, xgm_b3_expr
}};

sgs_ObjInterface xgm_color_iface[1] =
{{
	"color",
	NULL, NULL,
	xgm_col_getindex, xgm_col_setindex,
	xgm_col_convert, xgm_col_serialize, xgm_col_dump, NULL,
	NULL, xgm_col_expr
}};

sgs_ObjInterface xgm_quat_iface[1] =
{{
	"quat",
	NULL, NULL,
	xgm_quat_getindex, xgm_quat_setindex,
	xgm_quat_convert, xgm_quat_serialize, xgm_quat_dump, NULL,
	NULL, xgm_quat_expr
}};

sgs_ObjInterface xgm_mat3_iface[1] =
{{
	"mat3",
	NULL, NULL,
	xgm_m3_getindex, xgm_m3_setindex,
	xgm_m3_convert, xgm_m3_serialize, xgm_m3_dump, NULL,
	NULL, xgm_m3_expr
}};

sgs_ObjInterface xgm_mat4_iface[1] =
{{
	"mat4",
	NULL, NULL,
	xgm_m4_getindex, xgm_m4_setindex,
	xgm_m4_convert, xgm_m4_serialize, xgm_m4_dump, NULL,
	NULL, xgm_m4_expr
}};

sgs_ObjInterface xgm_floatarr_iface[1] =
{{
	"floatarray",
	xgm_fla_destruct, NULL,
	xgm_fla_getindex, xgm_fla_setindex,
	xgm_fla_convert, xgm_fla_serialize, xgm_fla_dump, NULL,
	NULL, NULL
}};


SGSONE sgs_CreateVec2( SGS_CTX, sgs_Variable* var, XGM_VT x, XGM_VT y )
{
	XGM_VT* nv = (XGM_VT*) sgs_CreateObjectIPA( C, var, sizeof(XGM_VT) * 2, xgm_vec2_iface );
	XGM_SET2( nv, x, y );
	return 1;
}

SGSONE sgs_CreateVec3( SGS_CTX, sgs_Variable* var, XGM_VT x, XGM_VT y, XGM_VT z )
{
	XGM_VT* nv = (XGM_VT*) sgs_CreateObjectIPA( C, var, sizeof(XGM_VT) * 3, xgm_vec3_iface );
	XGM_SET3( nv, x, y, z );
	return 1;
}

SGSONE sgs_CreateVec4( SGS_CTX, sgs_Variable* var, XGM_VT x, XGM_VT y, XGM_VT z, XGM_VT w )
{
	XGM_VT* nv = (XGM_VT*) sgs_CreateObjectIPA( C, var, sizeof(XGM_VT) * 4, xgm_vec4_iface );
	XGM_SET4( nv, x, y, z, w );
	return 1;
}

SGSONE sgs_CreateAABB2( SGS_CTX, sgs_Variable* var, XGM_VT x1, XGM_VT y1_shdef, XGM_VT x2, XGM_VT y2 )
{
	XGM_VT* nv = (XGM_VT*) sgs_CreateObjectIPA( C, var, sizeof(XGM_VT) * 4, xgm_aabb2_iface );
	XGM_SET4( nv, x1, y1_shdef, x2, y2 ); /* shadowed declaration warning */
	return 1;
}

SGSONE sgs_CreateAABB3( SGS_CTX, sgs_Variable* var, const XGM_VT* v3a, const XGM_VT* v3b )
{
	XGM_VT* nv = (XGM_VT*) sgs_CreateObjectIPA( C, var, sizeof(XGM_VT) * 6, xgm_aabb3_iface );
	XGM_COPY3( nv, v3a );
	XGM_COPY3( nv+3, v3b );
	return 1;
}

SGSONE sgs_CreateColor( SGS_CTX, sgs_Variable* var, XGM_VT x, XGM_VT y, XGM_VT z, XGM_VT w )
{
	XGM_VT* nv = (XGM_VT*) sgs_CreateObjectIPA( C, var, sizeof(XGM_VT) * 4, xgm_color_iface );
	XGM_SET4( nv, x, y, z, w );
	return 1;
}

SGSONE sgs_CreateQuat( SGS_CTX, sgs_Variable* var, XGM_VT x, XGM_VT y, XGM_VT z, XGM_VT w )
{
	XGM_VT* nv = (XGM_VT*) sgs_CreateObjectIPA( C, var, sizeof(XGM_VT) * 4, xgm_quat_iface );
	XGM_SET4( nv, x, y, z, w );
	return 1;
}

SGSONE sgs_CreateMat3( SGS_CTX, sgs_Variable* var, const XGM_VT* v9f, int transpose )
{
	XGM_VT* nv = (XGM_VT*) sgs_CreateObjectIPA( C, var, sizeof(XGM_VT) * 9, xgm_mat3_iface );
	if( transpose )
	{
		nv[ 0 ] = v9f[ 0 ]; nv[ 1 ] = v9f[ 3 ]; nv[ 2 ] = v9f[ 6 ];
		nv[ 3 ] = v9f[ 1 ]; nv[ 4 ] = v9f[ 4 ]; nv[ 5 ] = v9f[ 7 ];
		nv[ 6 ] = v9f[ 2 ]; nv[ 7 ] = v9f[ 5 ]; nv[ 8 ] = v9f[ 8 ];
	}
	else
		memcpy( nv, v9f, sizeof(XGM_VT) * 9 );
	return 1;
}

SGSONE sgs_CreateMat4( SGS_CTX, sgs_Variable* var, const XGM_VT* v16f, int transpose )
{
	XGM_VT* nv = (XGM_VT*) sgs_CreateObjectIPA( C, var, sizeof(XGM_VT) * 16, xgm_mat4_iface );
	if( transpose )
	{
		nv[ 0 ] = v16f[ 0 ]; nv[ 1 ] = v16f[ 4 ]; nv[ 2 ] = v16f[ 8 ]; nv[ 3 ] = v16f[ 12 ];
		nv[ 4 ] = v16f[ 1 ]; nv[ 5 ] = v16f[ 5 ]; nv[ 6 ] = v16f[ 9 ]; nv[ 7 ] = v16f[ 13 ];
		nv[ 8 ] = v16f[ 2 ]; nv[ 9 ] = v16f[ 6 ]; nv[ 10 ] = v16f[ 10 ]; nv[ 11 ] = v16f[ 14 ];
		nv[ 12 ] = v16f[ 3 ]; nv[ 13 ] = v16f[ 7 ]; nv[ 14 ] = v16f[ 11 ]; nv[ 15 ] = v16f[ 15 ];
	}
	else
		memcpy( nv, v16f, sizeof(XGM_VT) * 16 );
	return 1;
}

SGSONE sgs_CreateFloatArray( SGS_CTX, sgs_Variable* var, const XGM_VT* vfn, sgs_SizeVal size )
{
	xgm_vtarray* np = (xgm_vtarray*) sgs_CreateObjectIPA( C, var, sizeof(xgm_vtarray), xgm_floatarr_iface );
	np->size = size;
	np->mem = size;
	np->data = size ? sgs_Alloc_n( XGM_VT, (size_t) np->mem ) : NULL;
	memcpy( np->data, vfn, sizeof( XGM_VT ) * (size_t) np->mem );
	return 1;
}


SGSONE sgs_CreateVec2p( SGS_CTX, sgs_Variable* var, const XGM_VT* v2f )
{
	XGM_VT* nv = (XGM_VT*) sgs_CreateObjectIPA( C, var, sizeof(XGM_VT) * 2, xgm_vec2_iface );
	XGM_COPY2( nv, v2f );
	return 1;
}

SGSONE sgs_CreateVec3p( SGS_CTX, sgs_Variable* var, const XGM_VT* v3f )
{
	XGM_VT* nv = (XGM_VT*) sgs_CreateObjectIPA( C, var, sizeof(XGM_VT) * 3, xgm_vec3_iface );
	XGM_COPY3( nv, v3f );
	return 1;
}

SGSONE sgs_CreateVec4p( SGS_CTX, sgs_Variable* var, const XGM_VT* v4f )
{
	XGM_VT* nv = (XGM_VT*) sgs_CreateObjectIPA( C, var, sizeof(XGM_VT) * 4, xgm_vec4_iface );
	XGM_COPY4( nv, v4f );
	return 1;
}

SGSONE sgs_CreateAABB2p( SGS_CTX, sgs_Variable* var, const XGM_VT* v4f )
{
	XGM_VT* nv = (XGM_VT*) sgs_CreateObjectIPA( C, var, sizeof(XGM_VT) * 4, xgm_aabb2_iface );
	XGM_COPY4( nv, v4f );
	return 1;
}

SGSONE sgs_CreateAABB3p( SGS_CTX, sgs_Variable* var, const XGM_VT* v6f )
{
	XGM_VT* nv = (XGM_VT*) sgs_CreateObjectIPA( C, var, sizeof(XGM_VT) * 6, xgm_aabb3_iface );
	XGM_COPY6( nv, v6f );
	return 1;
}

SGSONE sgs_CreateColorp( SGS_CTX, sgs_Variable* var, const XGM_VT* v4f )
{
	XGM_VT* nv = (XGM_VT*) sgs_CreateObjectIPA( C, var, sizeof(XGM_VT) * 4, xgm_color_iface );
	XGM_COPY4( nv, v4f );
	return 1;
}

SGSONE sgs_CreateColorvp( SGS_CTX, sgs_Variable* var, const XGM_VT* vf, int numfloats )
{
	XGM_VT* nv = (XGM_VT*) sgs_CreateObjectIPA( C, var, sizeof(XGM_VT) * 4, xgm_color_iface );
	if( numfloats == 0 ) nv[0] = nv[1] = nv[2] = nv[3] = 0;
	else if( numfloats == 1 ) nv[0] = nv[1] = nv[2] = nv[3] = vf[0];
	else if( numfloats == 2 ){ nv[0] = nv[1] = nv[2] = vf[0]; nv[3] = vf[1]; }
	else if( numfloats == 3 ){ nv[0] = vf[0]; nv[1] = vf[1]; nv[2] = vf[2]; nv[3] = 1; }
	else { nv[0] = vf[0]; nv[1] = vf[1]; nv[2] = vf[2]; nv[3] = vf[3]; }
	return 1;
}

SGSONE sgs_CreateQuatp( SGS_CTX, sgs_Variable* var, const XGM_VT* v4f )
{
	XGM_VT* nv = (XGM_VT*) sgs_CreateObjectIPA( C, var, sizeof(XGM_VT) * 4, xgm_quat_iface );
	XGM_COPY4( nv, v4f );
	return 1;
}


SGSBOOL sgs_ParseVec2( SGS_CTX, sgs_StkIdx item, XGM_VT* v2f, int strict )
{
	sgs_Variable var = sgs_OptStackItem( C, item );
	if( !strict && ( var.type == SGS_VT_INT || var.type == SGS_VT_REAL ) )
	{
		v2f[0] = v2f[1] = (XGM_VT) sgs_GetRealP( C, &var );
		return 1;
	}
	
	if( sgs_IsObjectP( &var, xgm_vec2_iface ) ||
		( !strict && sgs_IsObjectP( &var, xgm_vec3_iface ) ) ||
		( !strict && sgs_IsObjectP( &var, xgm_vec4_iface ) ) ||
		( !strict && sgs_IsObjectP( &var, xgm_color_iface ) ) )
	{
		XGM_VT* hdr = (XGM_VT*) sgs_GetObjectDataP( &var );
		XGM_COPY2( v2f, hdr );
		return 1;
	}
	return 0;
}

SGSBOOL sgs_ParseVec3( SGS_CTX, sgs_StkIdx item, XGM_VT* v3f, int strict )
{
	sgs_Variable var = sgs_OptStackItem( C, item );
	if( !strict && ( var.type == SGS_VT_INT || var.type == SGS_VT_REAL ) )
	{
		v3f[0] = v3f[1] = v3f[2] = (XGM_VT) sgs_GetRealP( C, &var );
		return 1;
	}
	
	if( !strict && sgs_IsObjectP( &var, xgm_vec2_iface ) )
	{
		XGM_VT* hdr = (XGM_VT*) sgs_GetObjectDataP( &var );
		XGM_COPY2( v3f, hdr );
		v3f[2] = 0;
		return 1;
	}
	if( sgs_IsObjectP( &var, xgm_vec3_iface ) ||
		( !strict && sgs_IsObjectP( &var, xgm_vec4_iface ) ) ||
		( !strict && sgs_IsObjectP( &var, xgm_color_iface ) ) )
	{
		XGM_VT* hdr = (XGM_VT*) sgs_GetObjectDataP( &var );
		XGM_COPY3( v3f, hdr );
		return 1;
	}
	return 0;
}

SGSBOOL sgs_ParseVec4( SGS_CTX, sgs_StkIdx item, XGM_VT* v4f, int strict )
{
	sgs_Variable var = sgs_OptStackItem( C, item );
	if( !strict && ( var.type == SGS_VT_INT || var.type == SGS_VT_REAL ) )
	{
		v4f[0] = v4f[1] = v4f[2] = v4f[3] = (XGM_VT) sgs_GetRealP( C, &var );
		return 1;
	}
	
	if( !strict && sgs_IsObjectP( &var, xgm_vec2_iface ) )
	{
		XGM_VT* hdr = (XGM_VT*) sgs_GetObjectDataP( &var );
		XGM_COPY2( v4f, hdr );
		v4f[2] = 0;
		v4f[3] = 0;
		return 1;
	}
	if( !strict && sgs_IsObjectP( &var, xgm_vec3_iface ) )
	{
		XGM_VT* hdr = (XGM_VT*) sgs_GetObjectDataP( &var );
		XGM_COPY3( v4f, hdr );
		v4f[3] = 0;
		return 1;
	}
	if( sgs_IsObjectP( &var, xgm_vec4_iface ) ||
		sgs_IsObjectP( &var, xgm_color_iface ) )
	{
		XGM_VT* hdr = (XGM_VT*) sgs_GetObjectDataP( &var );
		XGM_COPY4( v4f, hdr );
		return 1;
	}
	return 0;
}

SGSBOOL sgs_ParseAABB2( SGS_CTX, sgs_StkIdx item, XGM_VT* v4f )
{
	sgs_Variable var = sgs_OptStackItem( C, item );
	if( sgs_IsObjectP( &var, xgm_aabb2_iface ) )
	{
		XGM_VT* hdr = (XGM_VT*) sgs_GetObjectDataP( &var );
		XGM_COPY4( v4f, hdr );
		return 1;
	}
	return 0;
}

SGSBOOL sgs_ParseAABB3( SGS_CTX, sgs_StkIdx item, XGM_VT* v6f )
{
	sgs_Variable var = sgs_OptStackItem( C, item );
	if( sgs_IsObjectP( &var, xgm_aabb3_iface ) )
	{
		XGM_VT* hdr = (XGM_VT*) sgs_GetObjectDataP( &var );
		XGM_COPY6( v6f, hdr );
		return 1;
	}
	return 0;
}

SGSBOOL sgs_ParseColor( SGS_CTX, sgs_StkIdx item, XGM_VT* v4f, int strict )
{
	return sgs_ParseVec4( C, item, v4f, strict );
}

SGSBOOL sgs_ParseQuat( SGS_CTX, sgs_StkIdx item, XGM_VT* v4f, int strict )
{
	sgs_Variable var = sgs_OptStackItem( C, item );
	if( sgs_IsObjectP( &var, xgm_quat_iface ) )
	{
		XGM_VT* hdr = (XGM_VT*) sgs_GetObjectDataP( &var );
		XGM_COPY4( v4f, hdr );
		return 1;
	}
	return 0;
}

SGSBOOL sgs_ParseMat3( SGS_CTX, sgs_StkIdx item, XGM_VT* v9f )
{
	sgs_Variable var = sgs_OptStackItem( C, item );
	if( sgs_IsObjectP( &var, xgm_mat3_iface ) )
	{
		XGM_VT* hdr = (XGM_VT*) sgs_GetObjectDataP( &var );
		memcpy( v9f, hdr, sizeof(XGM_VT) * 9 );
		return 1;
	}
	if( sgs_IsObjectP( &var, xgm_mat4_iface ) )
	{
		XGM_VT* hdr = (XGM_VT*) sgs_GetObjectDataP( &var );
		v9f[0] = hdr[0]; v9f[1] = hdr[1]; v9f[2] = hdr[2];
		v9f[3] = hdr[4]; v9f[4] = hdr[5]; v9f[5] = hdr[6];
		v9f[6] = hdr[8]; v9f[7] = hdr[9]; v9f[8] = hdr[10];
		return 1;
	}
	return 0;
}

SGSBOOL sgs_ParseMat4( SGS_CTX, sgs_StkIdx item, XGM_VT* v16f )
{
	sgs_Variable var = sgs_OptStackItem( C, item );
	if( sgs_IsObjectP( &var, xgm_mat3_iface ) )
	{
		XGM_VT* hdr = (XGM_VT*) sgs_GetObjectDataP( &var );
		v16f[0] = hdr[0]; v16f[1] = hdr[1]; v16f[2] = hdr[2]; v16f[3] = 0;
		v16f[4] = hdr[3]; v16f[5] = hdr[4]; v16f[6] = hdr[5]; v16f[7] = 0;
		v16f[8] = hdr[6]; v16f[9] = hdr[7]; v16f[10] = hdr[8]; v16f[11] = 0;
		v16f[12] = 0; v16f[13] = 0; v16f[14] = 0; v16f[15] = 1;
		return 1;
	}
	if( sgs_IsObjectP( &var, xgm_mat4_iface ) )
	{
		XGM_VT* hdr = (XGM_VT*) sgs_GetObjectDataP( &var );
		memcpy( v16f, hdr, sizeof(XGM_VT) * 16 );
		return 1;
	}
	return 0;
}

SGSBOOL sgs_ParseFloatArray( SGS_CTX, sgs_StkIdx item, XGM_VT** v2fa, sgs_SizeVal* osz )
{
	sgs_Variable var = sgs_OptStackItem( C, item );
	if( sgs_IsObjectP( &var, xgm_floatarr_iface ) )
	{
		xgm_vtarray* data = (xgm_vtarray*) sgs_GetObjectDataP( &var );
		if( v2fa ) *v2fa = data->data;
		if( osz ) *osz = data->size;
		return 1;
	}
	return 0;
}


int sgs_ArgCheck_Vec2( SGS_CTX, int argid, va_list* args, int flags )
{
	XGM_VT* out = NULL;
	XGM_VT v[2];
	if( flags & SGS_LOADARG_WRITE )
		out = va_arg( *args, XGM_VT* );
	
	if( sgs_ParseVec2( C, argid, v, flags & SGS_LOADARG_STRICT ? 1 : 0 ) )
	{
		if( out )
		{
			XGM_COPY2( out, v );
		}
		return 1;
	}
	if( flags & SGS_LOADARG_OPTIONAL )
		return 1;
	return sgs_ArgErrorExt( C, argid, 0, "vec2", flags & SGS_LOADARG_STRICT ? "strict " : "" );
}

int sgs_ArgCheck_Vec3( SGS_CTX, int argid, va_list* args, int flags )
{
	XGM_VT* out = NULL;
	XGM_VT v[3];
	if( flags & SGS_LOADARG_WRITE )
		out = va_arg( *args, XGM_VT* );
	
	if( sgs_ParseVec3( C, argid, v, flags & SGS_LOADARG_STRICT ? 1 : 0 ) )
	{
		if( out )
		{
			XGM_COPY3( out, v );
		}
		return 1;
	}
	if( flags & SGS_LOADARG_OPTIONAL )
		return 1;
	return sgs_ArgErrorExt( C, argid, 0, "vec3", flags & SGS_LOADARG_STRICT ? "strict " : "" );
}

static int sgs_ArgCheck_4F( SGS_CTX, int argid, va_list* args, int flags, const char* name )
{
	XGM_VT* out = NULL;
	XGM_VT v[4];
	if( flags & SGS_LOADARG_WRITE )
		out = va_arg( *args, XGM_VT* );
	
	if( sgs_ParseVec4( C, argid, v, flags & SGS_LOADARG_STRICT ? 1 : 0 ) )
	{
		if( out )
		{
			XGM_COPY4( out, v );
		}
		return 1;
	}
	if( flags & SGS_LOADARG_OPTIONAL )
		return 1;
	return sgs_ArgErrorExt( C, argid, 0, name, flags & SGS_LOADARG_STRICT ? "strict " : "" );
}

int sgs_ArgCheck_Vec4( SGS_CTX, int argid, va_list* args, int flags )
{
	return sgs_ArgCheck_4F( C, argid, args, flags, "vec4" );
}

int sgs_ArgCheck_AABB2( SGS_CTX, int argid, va_list* args, int flags )
{
	XGM_VT* out = NULL;
	XGM_VT v[4];
	if( flags & SGS_LOADARG_WRITE )
		out = va_arg( *args, XGM_VT* );
	
	if( sgs_ParseAABB2( C, argid, v ) )
	{
		if( out )
		{
			XGM_COPY4( out, v );
		}
		return 1;
	}
	if( flags & SGS_LOADARG_OPTIONAL )
		return 1;
	return sgs_ArgErrorExt( C, argid, 0, "aabb2", flags & SGS_LOADARG_STRICT ? "strict " : "" );
}

int sgs_ArgCheck_AABB3( SGS_CTX, int argid, va_list* args, int flags )
{
	XGM_VT* out = NULL;
	XGM_VT v[6];
	if( flags & SGS_LOADARG_WRITE )
		out = va_arg( *args, XGM_VT* );
	
	if( sgs_ParseAABB3( C, argid, v ) )
	{
		if( out )
		{
			XGM_COPY6( out, v );
		}
		return 1;
	}
	if( flags & SGS_LOADARG_OPTIONAL )
		return 1;
	return sgs_ArgErrorExt( C, argid, 0, "aabb3", flags & SGS_LOADARG_STRICT ? "strict " : "" );
}

int sgs_ArgCheck_Color( SGS_CTX, int argid, va_list* args, int flags )
{
	return sgs_ArgCheck_4F( C, argid, args, flags, "color" );
}

int sgs_ArgCheck_Quat( SGS_CTX, int argid, va_list* args, int flags )
{
	XGM_VT* out = NULL;
	XGM_VT v[4];
	if( flags & SGS_LOADARG_WRITE )
		out = va_arg( *args, XGM_VT* );
	
	if( sgs_ParseQuat( C, argid, v, flags & SGS_LOADARG_STRICT ? 1 : 0 ) )
	{
		if( out )
		{
			XGM_COPY4( out, v );
		}
		return 1;
	}
	if( flags & SGS_LOADARG_OPTIONAL )
		return 1;
	return sgs_ArgErrorExt( C, argid, 0, "quat", flags & SGS_LOADARG_STRICT ? "strict " : "" );
}

int sgs_ArgCheck_Mat3( SGS_CTX, int argid, va_list* args, int flags )
{
	XGM_VT v[9];
	if( sgs_ParseMat3( C, argid, v ) )
	{
		if( flags & SGS_LOADARG_WRITE )
		{
			XGM_VT* out = va_arg( *args, XGM_VT* );
			memcpy( out, v, sizeof(v) );
		}
		return 1;
	}
	if( flags & SGS_LOADARG_OPTIONAL )
		return 1;
	return sgs_ArgErrorExt( C, argid, 0, "mat3", "" );
}

int sgs_ArgCheck_Mat4( SGS_CTX, int argid, va_list* args, int flags )
{
	XGM_VT v[16];
	if( sgs_ParseMat4( C, argid, v ) )
	{
		if( flags & SGS_LOADARG_WRITE )
		{
			XGM_VT* out = va_arg( *args, XGM_VT* );
			memcpy( out, v, sizeof(v) );
		}
		return 1;
	}
	if( flags & SGS_LOADARG_OPTIONAL )
		return 1;
	return sgs_ArgErrorExt( C, argid, 0, "mat4", "" );
}

int sgs_ArgCheck_FloatArray( SGS_CTX, int argid, va_list* args, int flags )
{
	xgm_vtarray** out = NULL;
	if( flags & SGS_LOADARG_WRITE )
		out = va_arg( *args, xgm_vtarray** );
	
	if( sgs_ParseFloatArray( C, argid, NULL, NULL ) )
	{
		if( out )
			*out = (xgm_vtarray*) sgs_GetObjectData( C, argid );
		return 1;
	}
	if( flags & SGS_LOADARG_OPTIONAL )
		return 1;
	return sgs_ArgErrorExt( C, argid, 0, "floatarray", "" );
}


static sgs_RegFuncConst xgm_fconsts[] =
{
	{ "vec2", xgm_vec2 },
	{ "vec2_dot", xgm_vec2_dot },
	
	{ "vec3", xgm_vec3 },
	{ "vec3_dot", xgm_vec3_dot },
	{ "vec3_cross", xgm_vec3_cross },
	
	{ "vec4", xgm_vec4 },
	{ "vec4_dot", xgm_vec4_dot },
	
	{ "aabb2", xgm_aabb2 },
	{ "aabb2v", xgm_aabb2v },
	{ "aabb2_intersect", xgm_aabb2_intersect },
	
	{ "aabb3", xgm_aabb3 },
	{ "aabb3v", xgm_aabb3v },
	{ "aabb3_intersect", xgm_aabb3_intersect },
	
	{ "color", xgm_color },
	
	{ "quat", xgm_quat },
	
	{ "mat3", xgm_mat3 },
	{ "mat4", xgm_mat4 },
	
	{ "floatarray_buffer", xgm_floatarray_buffer },
	{ "floatarray", xgm_floatarray },
	{ "vec2array", xgm_vec2array },
	{ "vec3array", xgm_vec3array },
	{ "vec4array", xgm_vec4array },
	{ "floatarray_from_int8_buffer", xgm_floatarray_from_int8_buffer },
	{ "floatarray_from_int16_buffer", xgm_floatarray_from_int16_buffer },
	{ "floatarray_from_int32_buffer", xgm_floatarray_from_int32_buffer },
	{ "floatarray_from_int64_buffer", xgm_floatarray_from_int64_buffer },
	{ "floatarray_from_uint8_buffer", xgm_floatarray_from_uint8_buffer },
	{ "floatarray_from_uint16_buffer", xgm_floatarray_from_uint16_buffer },
	{ "floatarray_from_uint32_buffer", xgm_floatarray_from_uint32_buffer },
	{ "floatarray_from_uint64_buffer", xgm_floatarray_from_uint64_buffer },
	{ "floatarray_from_float32_buffer", xgm_floatarray_from_float32_buffer },
	{ "floatarray_from_float64_buffer", xgm_floatarray_from_float64_buffer },
	
	{ "ray_plane_intersect", xgm_ray_plane_intersect },
	{ "ray_sphere_intersect", xgm_ray_sphere_intersect },
	{ "distance_lines", xgm_distance_lines },
	{ "distance_line_segments", xgm_distance_line_segments },
	{ "distance_point_line", xgm_distance_point_line },
	{ "distance_point_line_segment", xgm_distance_point_line_segment },
};


SGS_APIFUNC int xgm_module_entry_point( SGS_CTX )
{
	SGS_MODULE_CHECK_VERSION( C );
	sgs_RegFuncConsts( C, xgm_fconsts, sizeof(xgm_fconsts) / sizeof(xgm_fconsts[0]) );
	sgs_RegisterType( C, "vec2", xgm_vec2_iface );
	sgs_RegisterType( C, "vec3", xgm_vec3_iface );
	sgs_RegisterType( C, "vec4", xgm_vec4_iface );
	sgs_RegisterType( C, "aabb2", xgm_aabb2_iface );
	sgs_RegisterType( C, "aabb3", xgm_aabb3_iface );
	sgs_RegisterType( C, "color", xgm_color_iface );
	sgs_RegisterType( C, "quat", xgm_quat_iface );
	sgs_RegisterType( C, "mat3", xgm_mat3_iface );
	sgs_RegisterType( C, "mat4", xgm_mat4_iface );
	sgs_RegisterType( C, "floatarray", xgm_floatarr_iface );
	return SGS_SUCCESS;
}


#ifdef SGS_COMPILE_MODULE
#ifdef __cplusplus
extern "C"
#endif
SGS_APIFUNC int sgscript_main( SGS_CTX )
{
	return xgm_module_entry_point( C );
}
#endif

