#include <objectscript.h>
#include <os-binder.h>

float test(float a, float b){ return a + b; }

int main( int argc, const char **argv )
{
    if( argc <= 1 ) return -1;

    using namespace ObjectScript;
    OS * os = OS::create();
    //os->setGlobal(def("test", test));   // use binder
    //os->eval("print(test(2, 3))");      // outputs: 5
    os->require(argv[1]);
    os->release();
    return 0;
}
