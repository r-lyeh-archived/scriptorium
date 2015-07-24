// Write a method called 'average' that takes two int parameters and returns the integer average of the two numbers.

class AverageTest extends UnitTest
{
  AverageTest()
  {
    Random gen = new Random($SEED);
    for (int i=1; i<=10; ++i) 
    {
      int r1 = gen.nextInt(1000);
      r1 -= 500;
      int r2 = gen.nextInt(1000);
      r2 -= 500;
      unit_test( "average("+r1+","+r2+")", average(r1,r2), average$SEED(r1,r2) );
    }
  }

  int average$SEED( int r1, int r2 )
  {
    return (r1 + r2) / 2;
  }

  $USERCODE
}

