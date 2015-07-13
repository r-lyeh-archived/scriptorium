
#include <ctype.h>
#include "dao.h"

static void dao_isalnum( DaoProcess *proc, DaoValue *P[], int N )
{
	DaoProcess_PutInteger( proc, isalnum( DaoValue_TryGetInteger( P[0] ) ) );
}
static void dao_isalpha( DaoProcess *proc, DaoValue *P[], int N )
{
	DaoProcess_PutInteger( proc, isalpha( DaoValue_TryGetInteger( P[0] ) ) );
}
static void dao_isblank( DaoProcess *proc, DaoValue *P[], int N )
{
	DaoProcess_PutInteger( proc, isblank( DaoValue_TryGetInteger( P[0] ) ) );
}
static void dao_iscntrl( DaoProcess *proc, DaoValue *P[], int N )
{
	DaoProcess_PutInteger( proc, iscntrl( DaoValue_TryGetInteger( P[0] ) ) );
}
static void dao_isdigit( DaoProcess *proc, DaoValue *P[], int N )
{
	DaoProcess_PutInteger( proc, isdigit( DaoValue_TryGetInteger( P[0] ) ) );
}
static void dao_isgraph( DaoProcess *proc, DaoValue *P[], int N )
{
	DaoProcess_PutInteger( proc, isgraph( DaoValue_TryGetInteger( P[0] ) ) );
}
static void dao_islower( DaoProcess *proc, DaoValue *P[], int N )
{
	DaoProcess_PutInteger( proc, islower( DaoValue_TryGetInteger( P[0] ) ) );
}
static void dao_isprint( DaoProcess *proc, DaoValue *P[], int N )
{
	DaoProcess_PutInteger( proc, isprint( DaoValue_TryGetInteger( P[0] ) ) );
}
static void dao_ispunct( DaoProcess *proc, DaoValue *P[], int N )
{
	DaoProcess_PutInteger( proc, ispunct( DaoValue_TryGetInteger( P[0] ) ) );
}
static void dao_isspace( DaoProcess *proc, DaoValue *P[], int N )
{
	DaoProcess_PutInteger( proc, isspace( DaoValue_TryGetInteger( P[0] ) ) );
}
static void dao_isupper( DaoProcess *proc, DaoValue *P[], int N )
{
	DaoProcess_PutInteger( proc, isupper( DaoValue_TryGetInteger( P[0] ) ) );
}
static void dao_isxdigit( DaoProcess *proc, DaoValue *P[], int N )
{
	DaoProcess_PutInteger( proc, isxdigit( DaoValue_TryGetInteger( P[0] ) ) );
}
static void dao_tolower( DaoProcess *proc, DaoValue *P[], int N )
{
	DaoProcess_PutInteger( proc, tolower( DaoValue_TryGetInteger( P[0] ) ) );
}
static void dao_toupper( DaoProcess *proc, DaoValue *P[], int N )
{
	DaoProcess_PutInteger( proc, toupper( DaoValue_TryGetInteger( P[0] ) ) );
}
static void dao_isascii( DaoProcess *proc, DaoValue *P[], int N )
{
	DaoProcess_PutInteger( proc, isascii( DaoValue_TryGetInteger( P[0] ) ) );
}
static void dao_toascii( DaoProcess *proc, DaoValue *P[], int N )
{
	DaoProcess_PutInteger( proc, toascii( DaoValue_TryGetInteger( P[0] ) ) );
}

static DaoFuncItem charTypeMeths[]=
{
	{ dao_isalnum,  "isalnum( ch : int ) => int" } ,
	{ dao_isalpha,  "isalpha( ch : int ) => int" } ,
	{ dao_isblank,  "isblank( ch : int ) => int" } ,
	{ dao_iscntrl,  "iscntrl( ch : int ) => int" } ,
	{ dao_isdigit,  "isdigit( ch : int ) => int" } ,
	{ dao_isgraph,  "isgraph( ch : int ) => int" } ,
	{ dao_islower,  "islower( ch : int ) => int" } ,
	{ dao_isprint,  "isprint( ch : int ) => int" } ,
	{ dao_ispunct,  "ispunct( ch : int ) => int" } ,
	{ dao_isspace,  "isspace( ch : int ) => int" } ,
	{ dao_isupper,  "isupper( ch : int ) => int" } ,
	{ dao_isxdigit, "isxdigit( ch : int ) => int" } ,
	{ dao_tolower,  "tolower( ch : int ) => int" } ,
	{ dao_toupper,  "toupper( ch : int ) => int" } ,
	{ dao_isascii,  "isascii( ch : int ) => int" } ,
	{ dao_toascii,  "toascii( ch : int ) => int" } ,
	{ NULL, NULL }
};

DAO_DLL int DaoOnLoad( DaoVmSpace *vmSpace, DaoNamespace *ns )
{
	DaoNamespace_WrapFunctions( ns, charTypeMeths );
	return 0;
}
