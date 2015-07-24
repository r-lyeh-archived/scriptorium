// The increment() method return the value of its parameter plus 1.

class IncrementTest extends UnitTest
{
  IncrementTest()
  {
    Random gen = new Random($SEED);
    for (int i=1; i<=10; ++i) 
    {
      int r = gen.nextInt(1000);
      r -= 500;
      unit_test( "increment("+r+")", increment(r), increment$SEED(r) );
    }
  }

  int increment$SEED( int n )
  {
    return n + 1;
  }

  $USERCODE
}

