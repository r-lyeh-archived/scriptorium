// The timesTen() method return the value of its parameter times 10.

class TimesTenTest extends UnitTest
{
  TimesTenTest()
  {
    Random gen = new Random($SEED);
    for (int i=1; i<=10; ++i) 
    {
      int r = gen.nextInt(1000);
      r -= 500;
      unit_test( "timesTen("+r+")", timesTen(r), timesTen$SEED(r) );
    }
  }

  int timesTen$SEED( int n )
  {
    return n * 10;
  }

  $USERCODE
}

