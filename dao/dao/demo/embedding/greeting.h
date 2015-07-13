
#include<stdio.h>
#include<stdlib.h>

class otto
{
	private:
	int a;
	public:
	otto(int b=123){ a = b; printf( "new otto\n" ); }
	virtual ~otto(){};
	int geta(){ return a; }

	virtual void vtest(){}

	int& operator[](int i){ return a; }

protected:
	virtual otto test(const otto &value){
		printf( "call protected method\n" );
		return otto();
	}
};
class otto2 : public otto
{
	public:
	void vtest(){}
};

class Greeting
{
  int   size;
  char *message;

  public:

  Greeting( const char * msg=NULL );
  ~Greeting(){ if( message ) free( message ); }
  void SetMessage( const char * msg );
  void PrintMessage();

  // virtual function to be re-implemented by Dao class.
  virtual void DoGreeting( const char *name );

  // This method will invoke g->DoGreeting( name ).
  //
  // When a Dao class is derived from this C++ class,
  // an instance (object) of that Dao class will contain 
  // a parent object that is an instance of a wrapping 
  // C++ class derived from Greeting (see dao_greeting.h).
  //
  // The derived Dao class may re-implement the above
  // virtual method.
  //
  // If g is an parent object of an Dao object,
  // a call to g->DoGreeting( name ) will execute the
  // virtual method re-implemented in Dao.
  void TestGreeting( Greeting *g, const char *name );

  virtual void VirtWithDefault( const Greeting & g = Greeting() ){}

  class Null{};
  Null TestNull( const Null & ){ return Null(); }
};
class Greeting2 : public Greeting{};

class AutobindTest
{
  public:
 // virtual void InternalTransformDerivative(const float in[3], float out[3],
 //     float derivative[3][3]) = 0;
 // virtual void InternalTransformDerivative(const double in[3], double out[3],
 //     double derivative[3][3]) = 0;
};

Greeting* GetGreetingObject();

enum Enum1 { AA, BB, CC };

#ifdef TRUE
#undef TRUE
#undef FALSE
#endif

namespace CxxNS
{
  enum Bool { FALSE, TRUE };
  enum Enum2 { AA, BB, CC };
  //namespace NestedNS { enum NestedEnum { DD, EE, FF }; }

  typedef Bool Bool2;

  class Test
  {
    public:

    int index;
    double value;

    void Print(){ printf( "%5i: %9f\n", index, value ); }
  };
  /* reference a class from global scope */
  void Testing( Greeting *greeting, Bool bl=FALSE );
  void Testing( int a, Bool2 bl=FALSE );
  void Testing( Test *t, int b=0, const Test & o = Test(), const Test & g=Test(), int c=0 );
  int Testing2( Test *t, int b=0, const Test & o = Test(), const Test & g=Test(), int c=0 );
}

typedef CxxNS::Test Test2;

/* reference to a type and a constant from a namespace */
void Testing( CxxNS::Bool bl=CxxNS::FALSE );

namespace CxxNS2
{
  /* cross referencing to members from another namespace */
  void Testing( CxxNS::Test *test, CxxNS::Bool bl=CxxNS::FALSE );
}
