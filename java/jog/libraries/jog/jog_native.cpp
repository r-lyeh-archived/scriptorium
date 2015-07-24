#include <time.h>
#include <sys/timeb.h>
#include <sys/types.h>
#include <cmath>
#include <algorithm>
using namespace std;

#if !defined(_WIN32)
#  include <sys/time.h>
#endif

#include "jog.h"

// See add_native_handlers() at bottom.

//=============================================================================
//  Math
//=============================================================================
static void Math__abs__double( JogVM* vm )
{
  double n = vm->pop_double();
  vm->pop_frame();
  if (n >= 0) vm->push( n );
  else        vm->push( -n );
}

static void Math__abs__float( JogVM* vm )
{
  float n = (float) vm->pop_double();
  vm->pop_frame();
  if (n >= 0) vm->push( n );
  else        vm->push( -n );
}

static void Math__abs__int( JogVM* vm )
{
  int n = vm->pop_int();
  vm->pop_frame();
  if (n >= 0) vm->push( n );
  else        vm->push( -n );
}

static void Math__abs__long( JogVM* vm )
{
  double n = vm->pop_double();
  vm->pop_frame();
  if (n >= 0) vm->push( n );
  else        vm->push( -n );
}

static void Math__cos__double( JogVM* vm )
{
  double rads = vm->pop_double();
  vm->pop_frame();
  vm->push( cos(rads) );
}

static void Math__sin__double( JogVM* vm )
{
  double rads = vm->pop_double();
  vm->pop_frame();
  vm->push( sin(rads) );
}

static void Math__tan__double( JogVM* vm )
{
  double rads = vm->pop_double();
  vm->pop_frame();
  vm->push( tan(rads) );
}

static void Math__acos__double( JogVM* vm )
{
  double n = vm->pop_double();
  vm->pop_frame();
  vm->push( acos(n) );
}

static void Math__asin__double( JogVM* vm )
{
  double n = vm->pop_double();
  vm->pop_frame();
  vm->push( asin(n) );
}

static void Math__atan__double( JogVM* vm )
{
  double n = vm->pop_double();
  vm->pop_frame();
  vm->push( atan(n) );
}

static void Math__atan2__double_double( JogVM* vm )
{
  double x = vm->pop_double();
  double y = vm->pop_double();
  vm->pop_frame();
  vm->push( atan2(y,x) );
}

static void Math__ceil__double( JogVM* vm )
{
  double n = vm->pop_double();
  vm->pop_frame();
  vm->push( ceil(n) );
}

static void Math__floor__double( JogVM* vm )
{
  double n = vm->pop_double();
  vm->pop_frame();
  vm->push( floor(n) );
}

static void Math__pow__double_double( JogVM* vm )
{
  double p = vm->pop_double();
  double n = vm->pop_double();
  vm->pop_frame();
  vm->push( pow(n,p) );
}

static void Math__sqrt__double( JogVM* vm )
{
  double n = vm->pop_double();
  vm->pop_frame();
  vm->push( sqrt(n) );
}

static void Math__min__double_double( JogVM* vm )
{
  double m = vm->pop_double();
  double n = vm->pop_double();
  vm->pop_frame();
  vm->push( (min)(n,m) );
}

static void Math__min__float_float( JogVM* vm )
{
  float m = (float) vm->pop_double();
  float n = (float) vm->pop_double();
  vm->pop_frame();
  vm->push( (min)(n,m) );
}

static void Math__min__int_int( JogVM* vm )
{
  int m = vm->pop_int();
  int n = vm->pop_int();
  vm->pop_frame();
  vm->push( (min)(n,m) );
}

static void Math__min__long_long( JogVM* vm )
{
  double m = vm->pop_double();
  double n = vm->pop_double();
  vm->pop_frame();
  vm->push( (min)(n,m) );
}

static void Math__max__double_double( JogVM* vm )
{
  double m = vm->pop_double();
  double n = vm->pop_double();
  vm->pop_frame();
  vm->push( (max)(n,m) );
}

static void Math__max__float_float( JogVM* vm )
{
  float m = (float) vm->pop_double();
  float n = (float) vm->pop_double();
  vm->pop_frame();
  vm->push( (max)(n,m) );
}

static void Math__max__int_int( JogVM* vm )
{
  int m = vm->pop_int();
  int n = vm->pop_int();
  vm->pop_frame();
  vm->push( (max)(n,m) );
}

static void Math__max__long_long( JogVM* vm )
{
  double m = vm->pop_double();
  double n = vm->pop_double();
  vm->pop_frame();
  vm->push( (max)(n,m) );
}

//=============================================================================
//  PrintWriter
//=============================================================================
static void PrintWriter__print__char( JogVM* vm )
{
  putchar( vm->pop_int() );
}

static void PrintWriter__print__String( JogVM* vm )
{
  JogObject* str = *(vm->pop_ref());
  if (str == NULL)
  {
    printf( "null" );
  }
  else
  {
    JogObject* array = *((JogObject**)&(str->data[0]));
    if (array)
    {
      int count = array->count + 1;
      JogChar* cur  = ((JogChar*) array->data) - 1;
      while (--count)
      {
        putchar( *(++cur) );
      }
    }
    else
    {
      printf( "[Internal] null array on String print." );
    }
  }
}

//=============================================================================
//  System
//=============================================================================
static void System__currentTimeMillis( JogVM* vm )
{
  vm->pop_frame();

#if defined(_WIN32)
  struct __timeb64 time_struct;
  JogInt64 time_ms;
  _ftime64_s( &time_struct );
  time_ms = (JogInt64) time_struct.time;
  time_ms *= 1000;
  time_ms += time_struct.millitm;
#else
  struct timeval time_struct;
  JogInt64 time_ms;
  gettimeofday( &time_struct, 0 );
  time_ms = (JogInt64) time_struct.tv_sec;
  time_ms *= 1000;
  time_ms += (time_struct.tv_usec / 1000);
#endif

  vm->push( time_ms );
}


void JogVM::add_native_handlers()
{
  add_native_handler( "Math::abs(double)", Math__abs__double );
  add_native_handler( "Math::abs(float)", Math__abs__float );
  add_native_handler( "Math::abs(int)", Math__abs__int );
  add_native_handler( "Math::abs(long)", Math__abs__long );

  add_native_handler( "Math::cos(double)", Math__cos__double );
  add_native_handler( "Math::sin(double)", Math__sin__double );
  add_native_handler( "Math::tan(double)", Math__tan__double );
  add_native_handler( "Math::acos(double)", Math__acos__double );
  add_native_handler( "Math::asin(double)", Math__asin__double );
  add_native_handler( "Math::atan(double)", Math__atan__double );
  add_native_handler( "Math::atan2(double,double)", Math__atan2__double_double );

  add_native_handler( "Math::ceil(double)", Math__ceil__double );
  add_native_handler( "Math::floor(double)", Math__floor__double );

  add_native_handler( "Math::pow(double,double)", Math__pow__double_double );
  add_native_handler( "Math::sqrt(double)", Math__sqrt__double );

  add_native_handler( "Math::min(double,double)", Math__min__double_double );
  add_native_handler( "Math::min(float,float)", Math__min__float_float );
  add_native_handler( "Math::min(int,int)", Math__min__int_int );
  add_native_handler( "Math::min(long,long)", Math__min__long_long );

  add_native_handler( "Math::max(double,double)", Math__max__double_double );
  add_native_handler( "Math::max(float,float)", Math__max__float_float );
  add_native_handler( "Math::max(int,int)", Math__max__int_int );
  add_native_handler( "Math::max(long,long)", Math__max__long_long );

  add_native_handler( "PrintWriter::print(char)", PrintWriter__print__char );
  add_native_handler( "PrintWriter::print(String)", PrintWriter__print__String );
  add_native_handler( "System::currentTimeMillis()", System__currentTimeMillis );
}

