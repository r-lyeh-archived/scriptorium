#ifndef REF_COUNTED_H
#define REF_COUNTED_H
//====================================================================
//  ref_counted.h
//
//  2010.07.10 by Abe.Pralle (at) plasmaworks.com
//
//  This code is released to the public domain.
//
//  Usage:
//    struct MyStruct : RefCounted { ... };
//
//    Ref<MyStruct> ref1 = new MyStruct();
//    Ref<MyStruct> ref2 = ref1;
//    ...
//    ref1 = NULL;
//    ref2 = NULL; // MyStruct object deleted
//====================================================================

//extern int num_objects;

struct RefCounted
{
  int reference_count;

  RefCounted() : reference_count(0) 
  { 
    //printf( "%d objects\n", ++num_objects );
  }

  virtual ~RefCounted() 
  { 
    //printf( "%d objects\n", --num_objects );
  }

  void retain() 
  { 
    ++reference_count; 
  }

  void release()
  {
    if (--reference_count == 0)
    {
      delete this;
    }
  }
};

template <class ObjectType>
struct Ref
{
  ObjectType* object;

  Ref() : object(0) { }

  Ref( ObjectType* object ) : object(object)
  {
    if (object) object->retain();
  }

  Ref( const Ref& existing )
  {
    object = 0;
    operator=(existing);
  }

  ~Ref()
  {
    if (object) 
    {
      object->release();
      object = 0;
    }
  }

  void operator=( const Ref& other )
  {
    if (other.object) other.object->retain();
    if (object) object->release();
    object = other.object;
  }

  void operator=( ObjectType* new_object )
  {
    if (object) object->release();
    object = new_object;
    if (object) object->retain();
  }

  ObjectType* operator*()
  {
    return (ObjectType*) object;
  }

  ObjectType* operator->()
  {
    return (ObjectType*) object;
  }

  bool operator==( void* other )
  {
    return object == other;
  }

  bool operator==( Ref<ObjectType> other )
  {
    return object == other.object;
  }

  bool operator!=( void* other )
  {
    return object != other;
  }

  bool operator!=( Ref<ObjectType> other )
  {
    return object != other.object;
  }

  bool operator!()
  {
    return object ? false : true;
  }

};


#endif // REF_COUNTED_H
