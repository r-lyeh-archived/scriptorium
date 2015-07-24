// 2010.05.26 by Abe Pralle
#ifndef STRING_BUILDER_H
#define STRING_BUILDER_H

struct StringBuilder
{
  char* data;
  int   capacity, count;

  StringBuilder( int capacity=80 ) : capacity(capacity), count(0)
  {
    data = new char[capacity];
  }

  ~StringBuilder()
  {
    delete data;
    data = 0;
  }

  void ensure_capacity( int n )
  {
    if (capacity >= n) return;

    int new_cap = capacity * 2;
    char* new_data = new char[new_cap];
    memcpy( new_data, data, count );
    delete data;
    data = new_data;
    capacity = new_cap;
  }

  void print( char ch )
  {
    ensure_capacity( count + 1 );
    data[count++] = ch;
  }

  void print( int n )
  {
    if (n < 0)
    {
      print('-');
      n = -n;
    }

    print_recursive( n / 10 );
    print( (char)((n%10) + '0') );
  }

  void print_recursive( int n )
  {
    if (n == 0) return;
    print_recursive(n/10);
    print( (char)((n%10) + '0') );
  }

  void print( const char* st )
  {
    int len = strlen(st);
    ensure_capacity(count+len);
    ++len;
    while (--len) data[count++] = *(st++);
  }

  bool operator==( const char* st )
  {
    int c = count + 1;
    char* st1 = data - 1;
    const char* st2 = st - 1;
    while (--c)
    {
      if (*(++st1) != *(++st2)) return false;
    }
    return true;
  }

  bool operator!=( const char* st )
  {
    return !operator==(st);
  }

  char operator[]( int index )
  {
    return data[index];
  }

  char* to_string()
  {
    ensure_capacity(count+1);
    data[count] = 0;  // NULL terminate
    return data;
  }

  void clear()
  {
    count = 0;
  }
};

struct UnicodeStringBuilder
{
  short int* data;
  int   capacity, count;

  UnicodeStringBuilder( int capacity=80 ) : capacity(capacity), count(0)
  {
    data = new short int[capacity];
  }

  ~UnicodeStringBuilder()
  {
    delete data;
    data = 0;
  }

  void ensure_capacity( int n )
  {
    if (capacity >= n) return;

    int new_cap = capacity * 2;
    short int* new_data = new short int[new_cap];
    memcpy( new_data, data, count*2 );
    delete data;
    data = new_data;
    capacity = new_cap;
  }

  void print( int ch )
  {
    ensure_capacity( count + 1 );
    data[count++] = (short int) ch;
  }

  void print( const char* st )
  {
    int len = strlen(st);
    ensure_capacity( count + len );
    while (*st) data[count++] = (short int) *(st++);
  }

  void print( short int* st )
  {
    int len = 0;
    while (st[len]) ++len;

    ensure_capacity( count + len );
    while (*st) data[count++] = *(st++);
  }

  void print( StringBuilder& buffer )
  {
    buffer.ensure_capacity(count+1);
    for (int i=0; i<count; ++i) buffer.print((char)data[i]);
  }

  short int* to_string()
  {
    ensure_capacity(count+1);
    data[count] = 0;  // NULL terminate
    return data;
  }

  void clear()
  {
    count = 0;
  }
};

#endif  // STRING_BUILDER_H
