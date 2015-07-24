// The timesPi() method should multiply the parameter 'n' by the value of pi.

class TimesPiTest extends UnitTest
{
  TimesPiTest()
  {
    Random gen = new Random($SEED);
    for (int i=1; i<=10; ++i) 
    {
      double r = gen.nextInt(1000) / 10.0;
      r -= 50;
      unit_test( "timesPi("+r+")", timesPi(r), timesPi$SEED(r) );
    }
  }

  double timesPi$SEED( double n )
  {
    return n * Math.PI;
  }

  $USERCODE
}

