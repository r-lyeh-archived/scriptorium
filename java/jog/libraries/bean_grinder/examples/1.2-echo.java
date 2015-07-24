// The echo method should print its parameter 'str' using println().

class EchoTest extends UnitTest
{
  EchoTest()
  {
    // A stdout match has the clunkiest-looking tests...
    Random gen = new Random($SEED);
    for (int i=1; i<=10; ++i)
    {
      if (gen.nextBoolean())
      {
        BeanGrinder.configureTest( "echo(\"Alpha\")" );
        BeanGrinder.setReferenceOutput( "Alpha\n" );
        BeanGrinder.startUserTest();
        echo("Alpha");
        BeanGrinder.endTest();
      }
      else
      {
        BeanGrinder.configureTest( "echo(\"Beta\")" );
        BeanGrinder.setReferenceOutput( "Beta\n" );
        BeanGrinder.startUserTest();
        echo("Beta");
        BeanGrinder.endTest();
      }
    }
  }

  $USERCODE
}

