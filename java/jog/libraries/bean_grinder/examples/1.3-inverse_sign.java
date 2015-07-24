// Numbers have signs that make them either negative or positive.  You can multiply a number by -1 in order to invert its sign.  Write a method invertSign that returns the sign-inverted value of its parameter 'number'.

class InverseSignTest extends UnitTest
{
  InverseSignTest()
  {
    Random gen = new Random($SEED);
    for (int i=1; i<=10; ++i) 
    {
      int r = gen.nextInt(1000);
      r -= 500;
      unit_test( "invertSign("+r+")", invertSign(r), invertSign$SEED(r) );
    }
  }

  int invertSign$SEED( int n )
  {
    return -n;
  }

  $USERCODE
}

