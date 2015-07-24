// Print the string "Programs must be written for people to read, and only incidentally for machines to execute." using a method named 'sussman'.
class SussmanTest extends UnitTest
{
  SussmanTest()
  {
    // A stdout match is the clunkiest-looking test.
    BeanGrinder.configureTest( "sussman()" );
    BeanGrinder.setReferenceOutput( "Programs must be written for people to read, and only incidentally for machines to execute.\n" );
    BeanGrinder.startUserTest();
    sussman();
    BeanGrinder.endTest();
  }

  $USERCODE
}

