// Ernestas Januševičius

#ifndef CSCRIPT_H
#define CSCRIPT_H
#include <amx/amx.h>

class CScript
{
public:
    /**Executes main method in script**/
    int ExecMain();

    /**Registers native c/c++ functions to Abstract machine**/
    bool registerNatives(const AMX_NATIVE_INFO *list);

    void push(int value);

    /**Use to destroy script/delete**/
    void drop();

    friend CScript * createScript(char * filename);
protected:
    ///User should not initialize this class, instead method createScript should be used!
    CScript();
    ~CScript();
    bool LoadProgram(char * file,void * _program);
    void Error(int error);


    void *program;
    AMX amx;
    int err;
};

CScript * createScript(char * filename);
#endif // CSCRIPT_H
