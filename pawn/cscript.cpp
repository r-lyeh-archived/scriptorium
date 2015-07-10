// Ernestas Januševičius

#include "cscript.h"
#include <stdio.h>
#include <string.h>
#include "amx/amxaux.h"

extern "C" {
  int AMXAPI amx_ConsoleInit(AMX *amx);
  int AMXAPI amx_ConsoleCleanup(AMX *amx);
  int AMXAPI amx_CoreInit(AMX *amx);
  int AMXAPI amx_CoreCleanup(AMX *amx);
}

CScript * createScript(char * filename)
{
    long memsize = aux_ProgramSize(filename);
    if(memsize==0)
    {
        printf("Script file not found or corrupted\n");
        return NULL;
    }

    void * program = malloc(memsize);

    if (program == NULL)
    {
        printf("Memory allocation for script failed\n");
        return NULL;
    }

    CScript * my_script= new CScript();
    if(!my_script->LoadProgram(filename,program))
    {
        printf("Loading script into Abstract Machine failed\n");
        delete my_script;
        return NULL;
    }
    return my_script;

}

CScript::CScript()
{
    program=NULL;
}

CScript::~CScript()
{
#ifdef PAWN_USE_CORE
    amx_CoreCleanup(&amx);
#endif
#ifdef PAWN_USE_CONSOLE
    amx_ConsoleCleanup(&amx);
#endif
    amx_Cleanup(&amx);
    if (program != NULL)
        free(this->program);
}

void CScript::drop()
{
    delete this;
}


void CScript::Error(int error)
{
    printf("Run time error %d: \"%s\"\n",
           error, aux_StrError(error));
}

bool CScript::LoadProgram(char*file,void * _program)
{

    this->err = aux_LoadProgram(&amx, file, program);
    if (err != AMX_ERR_NONE)
    {
        Error(this->err);
        return false;
    }

 #ifdef PAWN_USE_CONSOLE
    this->err = amx_ConsoleInit(&amx);
    if (0) // (err != AMX_ERR_NONE)
    {
        Error(this->err);
        return false;
    }
#endif

#ifdef PAWN_USE_CORE
    this->err = amx_CoreInit(&amx);
    if (0) // (err != AMX_ERR_NONE)
    {
        Error(this->err);
        return false;
    }
#endif

    return true;
}

int CScript::ExecMain()
{
    cell ret;
    this->err = amx_Exec(&amx, &ret, AMX_EXEC_MAIN);
    if (err != AMX_ERR_NONE)
        Error( this->err);

    if (ret != 0)
        printf("Returns %ld\n", (long)ret);
    return ret;
}

bool CScript::registerNatives(const AMX_NATIVE_INFO *list)
{
    err=amx_Register(&amx,list,-1);
    if (err != AMX_ERR_NONE)
    {
        Error( this->err);
        return false;
    }

    int num=-1;
    amx_NumNatives(&amx,&num);
    //printf("Registered %i native functions.\n",num);
    return true;
}