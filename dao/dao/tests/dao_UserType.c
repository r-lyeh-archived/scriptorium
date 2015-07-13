
#include<math.h>
#include<stdlib.h>
#include<string.h>
#include<assert.h>
#include"daoStdtype.h"
#include"daoValue.h"
#include"daoProcess.h"


typedef struct DaoxUserType  DaoxUserType;

/* bit integer */
struct DaoxUserType
{
	DAO_CSTRUCT_COMMON;

	dao_integer value;
};
DaoType *daox_type_user_type = NULL;

DAO_DLL DaoxUserType* DaoxUserType_New();
DAO_DLL void DaoxUserType_Delete( DaoxUserType *self );


DaoxUserType* DaoxUserType_New()
{
	DaoxUserType *self = (DaoxUserType*) dao_calloc( 1, sizeof(DaoxUserType) );
	DaoCstruct_Init( (DaoCstruct*) self, daox_type_user_type );
	return self;
}
void DaoxUserType_Delete( DaoxUserType *self )
{
	DaoCstruct_Free( (DaoCstruct*) self );
	dao_free( self );
}

static void DaoxUserType_GetItem1( DaoValue *self0, DaoProcess *proc, DaoValue *pid )
{
}
static void DaoxUserType_SetItem1( DaoValue *self0, DaoProcess *proc, DaoValue *pid, DaoValue *value )
{
}
static void DaoxUserType_GetItem( DaoValue *self, DaoProcess *proc, DaoValue *ids[], int N )
{
	switch( N ){
	case 0 : DaoxUserType_GetItem1( self, proc, dao_none_value ); break;
	case 1 : DaoxUserType_GetItem1( self, proc, ids[0] ); break;
	default : DaoProcess_RaiseError( proc, "Index", "not supported" );
	}
}
static void DaoxUserType_SetItem( DaoValue *self, DaoProcess *proc, DaoValue *ids[], int N, DaoValue *value )
{
	switch( N ){
	case 0 : DaoxUserType_SetItem1( self, proc, dao_none_value, value ); break;
	case 1 : DaoxUserType_SetItem1( self, proc, ids[0], value ); break;
	default : DaoProcess_RaiseError( proc, "Index", "not supported" );
	}
}
static DaoTypeCore userTypeCore=
{
	NULL,
	DaoValue_GetField,
	DaoValue_SetField,
	DaoxUserType_GetItem,
	DaoxUserType_SetItem,
	NULL
};
static void UT_New1( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoxUserType *self = DaoxUserType_New();
	DaoProcess_PutValue( proc, (DaoValue*) self );
	self->value = p[0]->xInteger.value;
}
static void UT_GETI( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoxUserType *self = (DaoxUserType*) DaoValue_CastCstruct( p[0], daox_type_user_type );
	DaoxUserType_GetItem1( p[0], proc, p[1] );
}
static void UT_BinaryOper2( DaoProcess *proc, DaoValue *p[], int N, int oper )
{
	DaoxUserType *C = DaoxUserType_New();
	DaoxUserType *A = (DaoxUserType*) p[0];
	DaoxUserType *B = (DaoxUserType*) p[1];
	DaoProcess_PutValue( proc, (DaoValue*) C );
}
static void UT_CompOper2( DaoProcess *proc, DaoValue *p[], int N, int oper )
{
	DaoValue *C = NULL;
	DaoxUserType *A = (DaoxUserType*) p[0];
	DaoxUserType *B = (DaoxUserType*) p[1];
	daoint D = 0;
	if( C ) DaoProcess_PutValue( proc, C );
	else DaoProcess_PutInteger( proc, D );
}
static void UT_ADD2( DaoProcess *proc, DaoValue *p[], int N )
{
	UT_BinaryOper2( proc, p, N, DVM_ADD );
}
static void UT_SUB2( DaoProcess *proc, DaoValue *p[], int N )
{
	UT_BinaryOper2( proc, p, N, DVM_SUB );
}
static void UT_MUL2( DaoProcess *proc, DaoValue *p[], int N )
{
	UT_BinaryOper2( proc, p, N, DVM_MUL );
}
static void UT_DIV2( DaoProcess *proc, DaoValue *p[], int N )
{
	UT_BinaryOper2( proc, p, N, DVM_DIV );
}
static void UT_MOD2( DaoProcess *proc, DaoValue *p[], int N )
{
	UT_BinaryOper2( proc, p, N, DVM_MOD );
}
static void UT_POW2( DaoProcess *proc, DaoValue *p[], int N )
{
	UT_BinaryOper2( proc, p, N, DVM_POW );
}
static void UT_AND2( DaoProcess *proc, DaoValue *p[], int N )
{
	UT_CompOper2( proc, p, N, DVM_AND );
}
static void UT_OR2( DaoProcess *proc, DaoValue *p[], int N )
{
	UT_CompOper2( proc, p, N, DVM_OR );
}
static void UT_LT2( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoxUserType *A = (DaoxUserType*) p[0];
	DaoxUserType *B = (DaoxUserType*) p[1];
	DaoProcess_PutInteger( proc, A->value < B->value );
}
static void UT_LE2( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoxUserType *A = (DaoxUserType*) p[0];
	DaoxUserType *B = (DaoxUserType*) p[1];
	DaoProcess_PutInteger( proc, A->value <= B->value );
}
static void UT_EQ2( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoxUserType *A = (DaoxUserType*) p[0];
	DaoxUserType *B = (DaoxUserType*) p[1];
	DaoProcess_PutInteger( proc, A->value == B->value );
}
static void UT_NE2( DaoProcess *proc, DaoValue *p[], int N )
{
	DaoxUserType *A = (DaoxUserType*) p[0];
	DaoxUserType *B = (DaoxUserType*) p[1];
	DaoProcess_PutInteger( proc, A->value != B->value );
}
static void UT_UnaryOper( DaoProcess *proc, DaoValue *p[], int N, int oper )
{
	daoint ta;
	DaoxUserType *A = (DaoxUserType*) p[0];
	DaoxUserType *C = DaoxUserType_New();
	DaoProcess_PutValue( proc, (DaoValue*) C );
}
static void UT_MINUS( DaoProcess *proc, DaoValue *p[], int N )
{
	UT_UnaryOper( proc, p, N, DVM_MINUS );
}
static void UT_NOT( DaoProcess *proc, DaoValue *p[], int N )
{
	UT_UnaryOper( proc, p, N, DVM_NOT );
}
static void UT_TILDE( DaoProcess *proc, DaoValue *p[], int N )
{
	UT_UnaryOper( proc, p, N, DVM_TILDE );
}
static void UT_BitOper2( DaoProcess *proc, DaoValue *p[], int N, int oper )
{
	DaoxUserType *A = (DaoxUserType*) p[0];
	DaoxUserType *B = (DaoxUserType*) p[1];
	DaoxUserType *C = DaoxUserType_New();
	DaoProcess_PutValue( proc, (DaoValue*) C );
}
static void UT_BITAND2( DaoProcess *proc, DaoValue *p[], int N )
{
	UT_BitOper2( proc, p, N, DVM_BITAND );
}
static void UT_BITOR2( DaoProcess *proc, DaoValue *p[], int N )
{
	UT_BitOper2( proc, p, N, DVM_BITOR );
}
static void UT_BITXOR2( DaoProcess *proc, DaoValue *p[], int N )
{
	UT_BitOper2( proc, p, N, DVM_BITXOR );
}
static void UT_BITLFT2( DaoProcess *proc, DaoValue *p[], int N )
{
	UT_BitOper2( proc, p, N, DVM_BITLFT );
}
static void UT_BITRIT2( DaoProcess *proc, DaoValue *p[], int N )
{
	UT_BitOper2( proc, p, N, DVM_BITRIT );
}
static void UT_CastToInt( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoxUserType *self = (DaoxUserType*) p[0];
	DaoProcess_PutInteger( proc, self->value );
}
static void UT_CastToString( DaoProcess *proc, DaoValue *p[], int n )
{
	DaoxUserType *self = (DaoxUserType*) p[0];
	DString *res = DaoProcess_PutChars( proc, "" );
	DString_Reserve( res, 50 );
	res->size = sprintf( res->chars, "UserType.{%" DAO_I64 "}", self->value );
}
static DaoFuncItem userTypeMeths[]=
{
	{ UT_New1, "UserType( value: int ) => UserType" },

	{ UT_GETI, "[]( self: UserType, idx: none ) => UserType" },
	{ UT_GETI, "[]( self: UserType, idx: int ) => int" },

	{ UT_ADD2, "+( A: UserType, B: UserType ) => UserType" },
	{ UT_SUB2, "-( A: UserType, B: UserType ) => UserType" },
	{ UT_MUL2, "*( A: UserType, B: UserType ) => UserType" },
	{ UT_DIV2, "/( A: UserType, B: UserType ) => UserType" },
	{ UT_MOD2, "%( A: UserType, B: UserType ) => UserType" },
	{ UT_POW2, "**( A: UserType, B: UserType ) => UserType" },

	{ UT_AND2, "&&( A: UserType, B: UserType ) => UserType" },
	{ UT_OR2,  "||( A: UserType, B: UserType ) => UserType" },
	{ UT_LT2,  "< ( A: UserType, B: UserType ) => int" },
	{ UT_LE2,  "<=( A: UserType, B: UserType ) => int" },
	{ UT_EQ2,  "==( A: UserType, B: UserType ) => int" },
	{ UT_NE2,  "!=( A: UserType, B: UserType ) => int" },

	{ UT_MINUS, "-( A: UserType ) => UserType" },
	{ UT_NOT,   "!( A: UserType ) => UserType" },
	{ UT_TILDE, "~( A: UserType ) => UserType" },

	{ UT_BITAND2, "&( A: UserType, B: UserType ) => UserType" },
	{ UT_BITOR2,  "|( A: UserType, B: UserType ) => UserType" },
	{ UT_BITXOR2, "^( A: UserType, B: UserType ) => UserType" },
	{ UT_BITLFT2, "<<( A: UserType, B: UserType ) => UserType" },
	{ UT_BITRIT2, ">>( A: UserType, B: UserType ) => UserType" },

	{ UT_CastToInt,     "(int)( self: UserType, hashing = false )" },
	{ UT_CastToString,  "(string)( self: UserType )" },

	{ NULL, NULL },
};
DaoTypeBase userTypeTyper =
{
	"UserType", NULL, NULL, (DaoFuncItem*) userTypeMeths, {0}, {0},
	(FuncPtrDel)DaoxUserType_Delete, NULL
};

DAO_DLL int DaoUsertype_OnLoad( DaoVmSpace *vmSpace, DaoNamespace *ns )
{
	daox_type_user_type = DaoNamespace_WrapType( ns, & userTypeTyper, DAO_CTYPE_INVAR );
	return 0;
}
