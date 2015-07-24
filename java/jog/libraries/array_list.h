#ifndef ARRAY_LIST_H
#define ARRAY_LIST_H

#include "ref_counted.h"

template <typename DataType> 
struct ArrayList
{
  DataType* data;
  int count;
  int capacity;

  ArrayList( int capacity=0 )
  {
    this->capacity = capacity;
    if (capacity)
    {
      int bytes = sizeof(DataType) * capacity;
      data = (DataType*) new char[bytes];
      memset( data, 0, bytes );
    }
    else data = 0;
    count = 0;
  }

  ~ArrayList()
  {
    if (data) 
    {
      delete (char*) data;
      data = 0;
    }
  }

  void clear() { count = 0; }

  void add( DataType value )
  {
    if (count == capacity) ensure_capacity( capacity ? capacity*2 : 10 );
    data[count++] = value;
  }

  void insert( DataType value )
  {
    if (count == capacity) ensure_capacity( capacity ? capacity*2 : 10 );
    for (int i=count; i>0; --i)
    {
      data[i] = data[i-1];
    }
    ++count;
    data[0] = value;
  }

  void add( ArrayList<DataType>& list )
  {
    for (int i=0; i<list.count; ++i) add(list[i]);
  }

  bool contains( DataType value )
  {
    for (int i=0; i<count; ++i)
    {
      if (data[i] == value) return true;
    }
    return false;
  }

  DataType remove( DataType value )
  {
    int n = count + 1;
    DataType* cur = data - 1;
    while (--n)
    {
      if (*(++cur) == value)
      {
        if (--n)
        {
          memmove( cur, cur+1, sizeof(DataType) * n );
        }
        --count;
        return value;
      }
    }
    return value;
  }

  DataType remove_index( int index )
  {
    DataType result = data[index];
    if (index < --count)
    {
      memmove( data+index, data+index+1, sizeof(DataType) * (count - index) );
    }
    return result;
  }

  DataType remove_first()
  {
    return remove_index(0);
  }

  DataType remove_last()
  {
    return data[--count];
  }

  void discard_from( int index )
  {
    if (count > index) count = index;
  }

  DataType& operator[]( int index )
  {
    return data[index];
  }

  DataType last()
  {
    return data[count-1];
  }

  void ensure_count( int c )
  {
    ensure_capacity(c);
    if (c > count) count = c;
  }

  void ensure_capacity( int c )
  {
    if (capacity >= c) return;

    capacity = c;

    int bytes = sizeof(DataType) * c;

    if ( !data )
    {
      data = (DataType*) new char[bytes];
      memset( data, 0, bytes );
    }
    else
    {
      int old_bytes = sizeof(DataType) * count;

      DataType* new_data = (DataType*) new char[bytes];
      memset( ((char*)new_data) + old_bytes, 0, bytes - old_bytes );

      memcpy( new_data, data, old_bytes );
      delete (char*) data;

      data = new_data;
    }
  }
};

template <typename DataType> 
struct ObjectList
{
  DataType* data;
  int count;
  int capacity;

  ObjectList( int capacity=0 )
  {
    this->capacity = capacity;
    if (capacity)
    {
      data = new DataType[capacity];
    }
    else data = 0;
    count = 0;
  }

  ~ObjectList()
  {
    if (data) 
    {
      delete [] data;
      data = 0;
    }
  }

  void clear()
  {
    while (count) remove_last();
  }

  void add( DataType value )
  {
    if (count == capacity) ensure_capacity( capacity ? capacity*2 : 10 );
    data[count++] = value;
  }

  bool contains( DataType value )
  {
    for (int i=0; i<count; ++i)
    {
      if (data[i] == value) return true;
    }
    return false;
  }

  DataType remove_index( int index )
  {
    DataType result = data[index];
    --count;
    while (index < count)
    {
      data[index] = data[index+1];
      ++index;
    }
    DataType empty;
    data[count] = empty;
    return result;
  }

  DataType remove_first()
  {
    return remove_index(0);
  }

  DataType remove_last()
  {
    DataType result = data[--count];
    DataType empty;
    data[count] = empty;
    return result;
  }

  void discard_from( int index )
  {
    while (count > index) remove_last();
  }

  DataType& operator[]( int index )
  {
    return data[index];
  }

  DataType last()
  {
    return data[count-1];
  }

  void ensure_capacity( int c )
  {
    if (capacity >= c) return;

    if ( !data )
    {
      data = new DataType[c];
    }
    else
    {
      DataType* new_data = new DataType[c];
      for (int i=0; i<capacity; ++i) new_data[i] = data[i];

      delete [] data;
      data = new_data;
    }
    capacity = c;
  }
};


template <typename DataType> 
struct RefList
{
  Ref<DataType>* data;
  int count;
  int capacity;

  RefList( int capacity=0 )
  {
    this->capacity = capacity;
    if (capacity)
    {
      data = new Ref<DataType>[capacity];
    }
    else data = 0;
    count = 0;
  }

  ~RefList()
  {
    if (data) 
    {
      clear();
      delete [] data;
      data = 0;
    }
  }

  void clear()
  { 
    for (int i=0; i<count; ++i) data[i] = NULL;
    count = 0; 
  }

  void add( Ref<DataType> value )
  {
    if (count == capacity) ensure_capacity( capacity ? capacity*2 : 10 );
    data[count++] = value;
  }

  void insert( Ref<DataType> value )
  {
    if (count == capacity) ensure_capacity( capacity ? capacity*2 : 10 );
    for (int i=count; i>0; --i)
    {
      data[i] = data[i-1];
    }
    ++count;
    data[0] = value;
  }

  Ref<DataType> remove_index( int index )
  {
    Ref<DataType> result = data[index];
    --count;
    while (index < count)
    {
      data[index] = data[index+1];
      ++index;
    }
    data[count] = NULL;
    return result;
  }

  Ref<DataType> remove_first()
  {
    return remove_index(0);
  }

  Ref<DataType> remove_last()
  {
    Ref<DataType> result = data[--count];
    data[count] = NULL;
    return result;
  }

  void discard_from( int index )
  {
    while (count > index) remove_last();
  }

  Ref<DataType>& operator[]( int index )
  {
    return data[index];
  }

  Ref<DataType> last()
  {
    return data[count-1];
  }

  void ensure_capacity( int c )
  {
    if (capacity >= c) return;

    if ( !data )
    {
      data = new Ref<DataType>[c];
    }
    else
    {
      Ref<DataType>* new_data = new Ref<DataType>[c];
      for (int i=0; i<capacity; ++i) new_data[i] = data[i];

      delete [] data;
      data = new_data;
    }
    capacity = c;
  }

  bool replace( Ref<DataType> old_value, Ref<DataType> new_value )
  {
    for (int i=0; i<count; ++i)
    {
      if (*data[i] == *old_value)
      {
        data[i] = new_value;
        return true;
      }
    }
    return false;
  }
};

#endif

