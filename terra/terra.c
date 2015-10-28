#include <stdio.h>
#include "terra.h"

int main(int argc, char ** argv) {
    lua_State * L = luaL_newstate(); //create a plain lua state
    luaL_openlibs(L);                //initialize its libraries
    //initialize the terra state in lua
    terra_init(L);
    for(int i = 1; i < argc; i++)
        //run the terra code in each file
        if(terra_dofile(L,argv[i]))  
            exit(1);
    return 0;
}