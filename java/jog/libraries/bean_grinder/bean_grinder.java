class BeanGrinder
{
  native static public void configureTest( String test_name );

  native static public void startReferenceTest();
  native static public void setReferenceOutput( String output );

  native static public void startUserTest();

  native static public void endTest();

  static public void unit_test( String action, String user, String reference )
  {
    if (reference == null) reference = "null";
    if (user == null) user = "null";

    configureTest( action );

    startReferenceTest();
    print( reference );

    startUserTest();
    print( user );

    endTest();
  }

  native static public void printSound( float[] data );
  native static public void playSound( float[] data );
  native static public void printImage( byte[] data, int width );

  static public void printImage( byte[][][] data )
  {
    int w = data.length;
    int h = data[0].length;
    byte[] packed = new byte[w*h*3];

    int pos = 0;
    for (int j=0; j<h; ++j)
    {
      for (int i=0; i<w; ++i)
      {
        packed[pos++] = data[i][j][0];
        packed[pos++] = data[i][j][1];
        packed[pos++] = data[i][j][2];
      }
    }

    printImage( packed, w );
  }
}

class UnitTest
{
  public void unit_test( String action, String user_result, String reference_result )
  {
    BeanGrinder.unit_test( action, user_result, reference_result );
  }

  public void unit_test( String action, int user_result, int reference_result )
  {
    BeanGrinder.unit_test( action, ""+user_result, ""+reference_result );
  }

  public void unit_test( String action, double user_result, double reference_result )
  {
    BeanGrinder.unit_test( action, ""+user_result, ""+reference_result );
  }

  public void unit_test( String action, boolean user_result, boolean reference_result )
  {
    BeanGrinder.unit_test( action, ""+user_result, ""+reference_result );
  }

  public void unit_test( String action, char user_result, char reference_result )
  {
    BeanGrinder.unit_test( action, ""+user_result, ""+reference_result );
  }
}

