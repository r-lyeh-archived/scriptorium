
#include<stdio.h>

// If you want to use direct APIs of Dao, define the following
// preprocessor option before EVERY including of dao.h.
// Or add it in your Makefile or project compiling settings.
//#define DAO_DIRECT_API
#include<dao.h>

static void dao_FakeNumber_FakeNumber( DaoContext *_ctx, DValue *_p[], int _n );
static void dao_FakeNumber_AddSubMulDiv( DaoContext *_ctx, DValue *_p[], int _n );
static void dao_FakeNumber_AddSubMulDiv2( DaoContext *_ctx, DValue *_p[], int _n );
static void dao_FakeNumber_Compare( DaoContext *_ctx, DValue *_p[], int _n );

static DaoFuncItem dao_FakeNumber_Meths[] = 
{
  { dao_FakeNumber_FakeNumber, "FakeNumber()=>FakeNumber" },
  { dao_FakeNumber_AddSubMulDiv, "+( a : FakeNumber, b : FakeNumber, op=0 )" },
  { dao_FakeNumber_AddSubMulDiv, "-( a : FakeNumber, b : FakeNumber, op=1 )" },
  { dao_FakeNumber_AddSubMulDiv, "*( a : FakeNumber, b : FakeNumber, op=2 )" },
  { dao_FakeNumber_AddSubMulDiv, "/( a : FakeNumber, b : FakeNumber, op=3 )" },
  { dao_FakeNumber_AddSubMulDiv2, "+( c : FakeNumber, a : FakeNumber, b : FakeNumber, op=0 )" },
  { dao_FakeNumber_AddSubMulDiv2, "-( c : FakeNumber, a : FakeNumber, b : FakeNumber, op=1 )" },
  { dao_FakeNumber_AddSubMulDiv2, "*( c : FakeNumber, a : FakeNumber, b : FakeNumber, op=2 )" },
  { dao_FakeNumber_AddSubMulDiv2, "/( c : FakeNumber, a : FakeNumber, b : FakeNumber, op=3 )" },
  { dao_FakeNumber_Compare, "<( a : FakeNumber, b : FakeNumber, op=0 )" },
  { dao_FakeNumber_Compare, "<=( a : FakeNumber, b : FakeNumber, op=1 )" },
  { dao_FakeNumber_Compare, "==( a : FakeNumber, b : FakeNumber, op=2 )" },
  { dao_FakeNumber_Compare, "!=( a : FakeNumber, b : FakeNumber, op=3 )" },
  { NULL, NULL }
};
static void Dao_FakeNumber_Delete( void *self ){}
static DaoTypeBase FakeNumber_Typer = 
{ "FakeNumber", NULL, NULL, dao_FakeNumber_Meths, {0}, {0}, Dao_FakeNumber_Delete, NULL };
DaoTypeBase *dao_FakeNumber_Typer = & FakeNumber_Typer;

static void dao_FakeNumber_FakeNumber( DaoContext *_ctx, DValue *_p[], int _n )
{
  DaoContext_PutCData( _ctx, NULL, dao_FakeNumber_Typer );
}
static void dao_FakeNumber_AddSubMulDiv( DaoContext *_ctx, DValue *_p[], int _n )
{
  DaoContext_PutCData( _ctx, NULL, dao_FakeNumber_Typer );
  printf( "Arithmetic on two numbers (a fake). opcode: %i\n", _p[2]->v.i );
}
static void dao_FakeNumber_AddSubMulDiv2( DaoContext *_ctx, DValue *_p[], int _n )
{
  DaoContext_PutValue( _ctx, *_p[0] );
  printf( "Arithmetic on two numbers (a fake), with C. opcode: %i\n", _p[3]->v.i );
}
static void dao_FakeNumber_Compare( DaoContext *_ctx, DValue *_p[], int _n )
{
  DaoContext_PutCData( _ctx, NULL, dao_FakeNumber_Typer );
  printf( "Compare on two numbers (a fake). opcode: %i\n", _p[2]->v.i );
}

const char* dao_source = 
"f1 = FakeNumber()\n"
"f2 = FakeNumber()\n"
"for( i = 1 : 2 )\n"
"f3 = f1 + f2\n"
"for( i = 1 : 2 )\n"
"f3 = f1 - f2\n"
"f3 = f1 * f2\n"
"f3 = f1 / f2\n"
"f3 = f1 < f2\n"
"class FakeNumber2 : FakeNumber{ routine FakeNumber2() : FakeNumber(){} }"
"ff1 = FakeNumber2()\n"
"ff2 = FakeNumber2()\n"
"ff3 = ff1 + ff2\n"
;

int main( int argc, char *argv[] )
{
  DString *src;
  DaoVmSpace *vms;
  DaoNameSpace *ns;
  DaoVmProcess *vmp;

  // Search and load the Dao library.
  // DaoInitLibrary() can take a parameter which is the path
  // to the dynamic loading file of the Dao library.
  // If the parameter is NULL, the current path is searched,
  // then the path defined by environment variable DAO_DIR,
  // then $(HOME)/dao, and then the default system path:
  // /usr/local/dao/ or C:\dao\.
  //
  // With direct APIs, the example must be linked against the Dao library.
  // So if direct APIs are used, the following call is not necessary.
#ifndef DAO_DIRECT_API
  if( DaoInitLibrary( NULL ) ==0 ) return 1;
#endif

  // Initialize Dao library, and get the default DaoVmSpace object.
  // DaoVmSpace is responsible for handling interpreter settings,
  // paths and module loading etc. It is need to create several
  // other types of objects.
  vms = DaoInit();

  // Get the main namespace of an DaoVmSpace object.
  // You can also call DaoNameSpace_New( vms ) to create one.
  ns  = DaoVmSpace_MainNameSpace( vms );

  // Get the main virtual machine process of an DaoVmSpace object.
  // You can also call DaoVmProcess_New( vms ) to create one.
  vmp = DaoVmSpace_MainVmProcess( vms );

  // Prepare the Dao source codes:
  src = DString_New(1);
  DString_SetMBS( src, dao_source );

  // Wrap and setup a C/C++ type:
  DaoNameSpace_WrapType( ns, dao_FakeNumber_Typer );

  // Execute the Dao scripts:
  // Since the wrapped functions and types are imported into
  // namespace ns, it is need to access the wrapped functions and types
  // in the Dao scripts when it is executed:
  DaoVmProcess_Eval( vmp, ns, src, 1 );

  DString_Delete( src );
  DaoQuit(); // Finalizing
  return 0;
}
