#include "cscript.h"
#include <stdio.h>

void print_int(int value)
{
    printf("%i\n",value);
}

void print_float(float value)
{
    printf("%f\n",value);
}

// wrappers

static cell AMX_NATIVE_CALL n_print_int(AMX *amx, const cell *params)
{
    print_int( (int)params[1] );
    return 0;
}

static cell AMX_NATIVE_CALL n_print_float(AMX *amx, const cell *params)
{
    print_float( amx_ctof(params[1]) );
    return 0;
}

const AMX_NATIVE_INFO print_Natives[] =
{
    { "print_int", n_print_int },
    { "print_float", n_print_float },
    {NULL,NULL}
    /* terminator */
};


int main( int argc, const char **argv )
{
    CScript * script = createScript( argc > 1 ? argv[1] : "test.amx" );

    if(!script)
        return -1;

    script->registerNatives(print_Natives);

    script->ExecMain();

    script->drop();

    return 0;
}
