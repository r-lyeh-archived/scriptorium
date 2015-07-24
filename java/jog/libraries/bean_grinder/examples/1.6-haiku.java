// Print the following haiku using a method named haiku():
//   Two tires fly. Two wail.
//   A bamboo grove, all chopped down.
//   From it, warring songs.
class HaikuTest extends UnitTest
{
  HaikuTest()
  {
    // A stdout match is the clunkiest-looking test.
    BeanGrinder.configureTest( "haiku()" );
    BeanGrinder.setReferenceOutput( "Two tires fly. Two wail.\nA bamboo grove, all chopped down.\nFrom it, warring songs.\n" );
    BeanGrinder.startUserTest();
    haiku();
    BeanGrinder.endTest();
  }

  $USERCODE
}

