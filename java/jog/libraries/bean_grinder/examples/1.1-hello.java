// Print the string "Hello World!".
class HelloTest extends UnitTest
{
  HelloTest()
  {
    // A stdout match is the clunkiest-looking test.
    BeanGrinder.configureTest( "println" );
    BeanGrinder.setReferenceOutput( "Hello World!\n" );
    BeanGrinder.startUserTest();
    helloWorld();
    BeanGrinder.endTest();
  }

  void helloWorld()
  {
    $USERCODE
  }
}

