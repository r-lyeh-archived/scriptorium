#include "jog.h"
int num_objects = 0;

int main( int argc, const char **argv )
{
  Ref<JogVM> vm = new JogVM();
  //Ref<JogScanner> scanner = new JogScanner(new JogReader("test.java"));

  try
  {
    vm->parse("libraries/jog/jog_stdlib.java");

    vm->parse( argc > 1 ? argv[1] : "test.java" );

#if 0
    vm->parse( "hello.java",
        "public class Hello { Hello(){ println(\"Hello World!\\n\"); } }"
        );
#endif

    vm->compile();
    vm->timeout_seconds = 0; //5;
    vm->run("Test");
    //while (scanner->peek()->type != TOKEN_EOF)
    //{
      //scanner->read()->print();
    //}
  }
  catch (Ref<JogError> error)
  {
    error->print();
  }
  catch (...)
  {
    fprintf( stderr, "[Internal compiler error]\n" );
  }

  return 0;
}

