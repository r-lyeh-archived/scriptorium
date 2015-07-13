
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include"greeting.h"

Greeting::Greeting( const char * msg )
{
  size = 0;
  message = NULL;
  if( msg ) SetMessage( msg );
}
void Greeting::SetMessage( const char * msg )
{
  int n = strlen( msg );
  if( message == NULL ){
    message = (char*) malloc( n+1 );
    size = n;
  }else if( size < n ){
    free( message );
    message = (char*) malloc( n+1 );
    size = n;
  }
  strcpy( message, msg );
}
void Greeting::PrintMessage()
{
  if( message ) printf( "%s\n", message );
}
void Greeting::DoGreeting( const char *name )
{
  printf( "C++: hi %s!\n", name );
}
void Greeting::TestGreeting( Greeting *g, const char *name )
{
  g->DoGreeting( name );
}

Greeting* GetGreetingObject()
{
  static Greeting* singleton = new Greeting("Hello, from C++");
  return singleton;
}

namespace CxxNS{
void Testing( Greeting *greeting, Bool bl )
{
  printf( "CxxNS::Testing(): %i\n", bl );
}
void Testing( int a, Bool2 bl )
{
  printf( "CxxNS::Testing(): %i\n", bl );
}
void Testing( Test *t, int b, const Test & o, const Test &g, int c )
{
  printf( "CxxNS::Testing(): %p\n", &o );
}
int Testing2( Test *t, int b, const Test & o, const Test &g, int c )
{
  printf( "CxxNS::Testing(): %p\n", &o );
  return 1;
}
}

void Testing( CxxNS::Bool bl )
{
  printf( "Testing(): %i\n", bl );
}

namespace CxxNS2{
void Testing( CxxNS::Test *test, CxxNS::Bool bl )
{
  printf( "CxxNS2::Testing(): %i\n", bl );
}
}
