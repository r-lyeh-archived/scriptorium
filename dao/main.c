#include <dao.h>

/*
#include <stdio.h>
#include "daoValue.h"
static void Square( DaoProcess *proc, DaoValue *param[], int nparam )
{
    daoint num = param[0]->xInteger.value;
    DaoProcess_PutInteger( proc, num*num );
}
int DaoOnLoad( DaoVmSpace *vmspace, DaoNamespace *nspace )
{
    DaoNamespace_WrapFunction( nspace, Square, "Square( num : int ) => int" );
    return 0;
}
*/

int main(int argc, const char **argv) {
    int k;
    if( argc <= 1 ) return -1;
    // Initialize Dao:
    DaoVmSpace *vmspace = DaoInit( NULL );
    // Load "myscript.dao":
    DaoVmSpace_Load( vmspace, argv[1] );
    k = ! DaoVmSpace_RunMain( vmspace, NULL );
    // Finalize (or Quit) Dao:
    DaoQuit();
    return 0;
}