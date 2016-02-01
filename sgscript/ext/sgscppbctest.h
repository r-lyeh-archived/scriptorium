

#include <math.h>

#define SGS_CPPBC_WITH_STD_STRING
#define SGS_CPPBC_WITH_STD_VECTOR

#include "sgs_cppbc.h"

struct Vec3
{
	SGS_OBJECT_LITE;
	
	Vec3( float _x, float _y, float _z ) : x(_x), y(_y), z(_z){}
	
	float _get_length(){ return (float) sqrtf(x*x+y*y+z*z); }
	
	SGS_PROPERTY float x;
	SGS_PROPERTY float y;
	SGS_PROPERTY float z;
	
	SGS_PROPERTY_FUNC( READ _get_length ) SGS_ALIAS( float length );
	SGS_METHOD float getLength(){ return _get_length(); }
	SGS_METHOD void setLength( float len )
	{
		if( x == 0 && y == 0 && z == 0 )
			x = 1;
		else
		{
			float ol = _get_length();
			len /= ol;
		}
		x *= len;
		y *= len;
		z *= len;
	}
	SGS_IFUNC( CONVERT ) int _convert( SGS_CTX, sgs_VarObj* data, int type )
	{
		Vec3* V = (Vec3*) data->data;
		if( type == SGS_VT_STRING )
		{
			char bfr[ 128 ] = {0};
			snprintf( bfr, 127, "Vec3(%g;%g;%g)", V->x, V->y, V->z );
			sgs_PushString( C, bfr );
			return SGS_SUCCESS;
		}
		return SGS_ENOTSUP;
	}
};


class Account
{
public:
	
	Account() : maybeIntTest2(42), bitfieldTest1(true)
	{
		vectorTest2.push_back( 1337 );
		stdStringTest2 = "test";
	}
	
	typedef sgsHandle< Account > Handle;
	
	SGS_OBJECT;
	
	SGS_PROPERTY sgsString name;
	SGS_PROPERTY_FUNC( READ WRITE VARNAME name2 ) SGS_ALIAS( sgsString name );
	
	SGS_PROPERTY Handle attached;
	SGS_GCREF( attached );
	SGS_NODUMP( attached );
	SGS_PROPERTY_FUNC( READ WRITE VALIDATE attached.not_null()
		SOURCE attached->name ) SGS_ALIAS( sgsString attachedName );
	SGS_PROPERTY sgsMaybe<int> maybeIntTest1;
	SGS_PROPERTY sgsMaybe<int> maybeIntTest2;
	SGS_PROPERTY bool bitfieldTest1 : 1;
	SGS_PROPERTY sgsVariable sgsVarTest1;
	SGS_PROPERTY Handle handleTest1;
	std::vector< int > vectorTest1; SGS_DUMP( vectorTest1 );
	std::vector< int > vectorTest2; SGS_DUMP( vectorTest2 );
	SGS_PROPERTY std::string stdStringTest1;
	SGS_PROPERTY std::string stdStringTest2;
	
	/* `Handle` must be resolved since it's going to be used out of scope */
	SGS_METHOD bool sendMoney( Account::Handle to, float amount, sgsString currency )
	{
		if( !to.not_null() )
		{
			printf( "--- sending money ---\n! recipient not specified\n" );
			return false;
		}
		printf( "--- sending money ---\nfrom: %.*s\nto: %.*s\namount: %.4f\ncurrency: %.*s\n---\n",
			(int) name.size(), name.c_str(),
			(int) to->name.size(), to->name.c_str(),
			amount,
			(int) currency.size(), currency.c_str() );
		return true;
	}
	
	// purposefully compacted formatting
	SGS_METHOD_NAMED( coroAware ) int sgsCoroAware(int a,SGS_CTX,int b,sgs_Context* c,int d){
		return a + b + ( C == c ) + ( C == this->C ) + d;
	}
	
	SGS_IFUNC( CONVERT ) int _convert( SGS_CTX, sgs_VarObj* data, int type )
	{
		Account* A = (Account*) data->data;
		if( type == SGS_VT_STRING )
		{
			sgs_PushString( C, "Account(" );
			A->name.push( C );
			sgs_PushString( C, ")" );
			sgs_StringConcat( C, 3 );
			return SGS_SUCCESS;
		}
		return SGS_ENOTSUP;
	}
};

