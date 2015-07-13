
#include<stdio.h>

// If you want to use direct APIs of Dao, define the following
// preprocessor option before EVERY including of dao.h.
// Or add it in your Makefile or project compiling settings.
//#define DAO_DIRECT_API
#include<dao.h>

static void dao_FakeList_FakeList( DaoContext *_ctx, DValue *_p[], int _n );
static void dao_FakeList_Iter( DaoContext *_ctx, DValue *_p[], int _n );
static void dao_FakeList_Size( DaoContext *_ctx, DValue *_p[], int _n );
static void dao_FakeList_GetItem( DaoContext *_ctx, DValue *_p[], int _n );
static void dao_FakeList_GetItem2( DaoContext *_ctx, DValue *_p[], int _n );
static void dao_FakeList_SetItem( DaoContext *_ctx, DValue *_p[], int _n );
static void dao_FakeList_GetName( DaoContext *_ctx, DValue *_p[], int _n );
static void dao_FakeList_SetName( DaoContext *_ctx, DValue *_p[], int _n );

static DaoFuncItem dao_FakeList_Meths[] = 
{
  { dao_FakeList_FakeList, "FakeList( size=0 )=>FakeList" },
  { dao_FakeList_Size, "size( self : FakeList )=>int" },
  { dao_FakeList_Iter, "__for_iterator__( self : FakeList, iter : for_iterator )" },
  { dao_FakeList_GetItem, "[]( self : FakeList, iter : for_iterator )=>int" },
  { dao_FakeList_GetItem2, "[]( self : FakeList, index : int )=>int" },
  { dao_FakeList_SetItem, "[]=( self : FakeList, index : int, value : int )=>int" },
  { dao_FakeList_GetName, ".name( self : FakeList )=>string" },
  { dao_FakeList_SetName, ".name=( self : FakeList, name : string )" },
  { NULL, NULL }
};
static void Dao_FakeList_Delete( void *self ){}
static DaoTypeBase FakeList_Typer = 
{ "FakeList", NULL, NULL, dao_FakeList_Meths, {0}, {0}, Dao_FakeList_Delete, NULL };
DaoTypeBase *dao_FakeList_Typer = & FakeList_Typer;

static void dao_FakeList_FakeList( DaoContext *_ctx, DValue *_p[], int _n )
{
  int size = _p[0]->v.i;
  DaoContext_PutCData( _ctx, (void*)(size_t)size, dao_FakeList_Typer );
}
static void dao_FakeList_Size( DaoContext *_ctx, DValue *_p[], int _n )
{
  int size = (int) DaoCData_GetData( _p[0]->v.cdata );
  DaoContext_PutInteger( _ctx, size );
}
static void dao_FakeList_Iter( DaoContext *_ctx, DValue *_p[], int _n )
{
  int size = (int) DaoCData_GetData( _p[0]->v.cdata );
  DaoTuple *it = _p[1]->v.tuple;
  DValue valid = DValue_NewInteger( size >0 );
  DValue index = DValue_NewInteger( 0 );
  DaoTuple_SetItem( it, valid, 0 );
  DaoTuple_SetItem( it, index, 1 );
}
static void dao_FakeList_GetItem( DaoContext *_ctx, DValue *_p[], int _n )
{
  int size = (int) DaoCData_GetData( _p[0]->v.cdata );
  DaoTuple *it = _p[1]->v.tuple;
  DValue index = DaoTuple_GetItem( it, 1 );
  DValue valid = DValue_NewInteger( (index.v.i+1) < size );
  index.v.i += 1;
  DaoTuple_SetItem( it, valid, 0 );
  DaoTuple_SetItem( it, index, 1 );
  DaoContext_PutInteger( _ctx, index.v.i * 100 );
}
static void dao_FakeList_GetItem2( DaoContext *_ctx, DValue *_p[], int _n )
{
  DaoContext_PutInteger( _ctx, 123 );
}
static void dao_FakeList_SetItem( DaoContext *_ctx, DValue *_p[], int _n )
{
  int index = _p[1]->v.i;
  int value = _p[2]->v.i;
  printf( "set %i to %i\n", index, value );
}
static void dao_FakeList_GetName( DaoContext *_ctx, DValue *_p[], int _n )
{
  DaoContext_PutMBString( _ctx, "FakeList" );
}
static void dao_FakeList_SetName( DaoContext *_ctx, DValue *_p[], int _n )
{
  const char *name = DString_GetMBS( _p[1]->v.s );
  printf( "new name: %s\n", name );
}

const char* dao_source = 
"fl = FakeList(5)\n"
"for( it in fl ) io.writeln( it )\n"
"interface Interface{ routine size() }\n"
"bind Interface to FakeList\n"
"routine Test( a : Interface ){ io.writeln( a.size() ) }\n"
"Test( fl );\n"
"io.writeln( fl[1] )\n"
"fl[2] = 456\n"
"io.writeln( fl.name )\n"
"fl.name = 'FAKELIST'"
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
  DaoNameSpace_WrapType( ns, dao_FakeList_Typer );

  // Execute the Dao scripts:
  // Since the wrapped functions and types are imported into
  // namespace ns, it is need to access the wrapped functions and types
  // in the Dao scripts when it is executed:
  DaoVmProcess_Eval( vmp, ns, src, 1 );

  DString_Delete( src );
  DaoQuit(); // Finalizing
  return 0;
}
