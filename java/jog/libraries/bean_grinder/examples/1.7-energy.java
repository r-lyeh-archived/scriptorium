// The energy() method should evaluate the formula E = mc^2, where the variables m and c are passed in as parameters and E (energy) is the return value.

class EnergyTest extends UnitTest
{
  EnergyTest()
  {
    Random gen = new Random($SEED);
    for (int i=1; i<=10; ++i) 
    {
      double m = gen.nextInt(1000) / 10.0;
      m -= 50;
      double c = gen.nextInt(1000) / 10.0;
      c -= 50;
      unit_test( "energy("+m+","+c+")", energy(m,c), energy$SEED(m,c) );
    }
  }

  double energy$SEED( double m, double c )
  {
    return m * c * c;
  }

  $USERCODE
}

