#include "jog.h"

#include <sstream>
using namespace std;

//=============================================================================
//  JogObject
//=============================================================================
JogObject::~JogObject()
{
  //if (type->is_array()) printf("DELETE ARRAY\n");
  //else printf("DELETE OBJECT\n");
}

void JogObject::release_refs()
{
  if (type->is_array())
  {
    if (type->element_type->is_reference())
    {
      JogObject** cur = ((JogObject**) data) - 1;
      int c = count + 1;
      while (--c)
      {
        JogObject* obj = *(++cur);
        if (obj) obj->release();
      }
    }
  }
  else
  {
    Ref<JogPropertyInfo>* cur = type->properties.data - 1;
    int count = type->properties.count + 1;
    while (--count)
    {
      JogPropertyInfo* p = **(++cur);
      if (p->type->is_reference())
      {
        JogObject* obj = *((JogObject**)&data[p->index]);
        if (obj) 
        {
          obj->release();
        }
      }
    }
  }
}

int JogObject::total_object_bytes()
{
  if (type->is_array())
  {
    return (sizeof(JogObject) - 8) + count * type->element_type->element_size;
  }
  else
  {
    return type->object_size;
  }
}


//=============================================================================
//  JogError
//=============================================================================
void JogError::print()
{
  printf( "===============================================================================\n" );
  printf( "ERROR:   %s\n", message->data );

  if ( !(*reader) )
  {
    printf( "===============================================================================\n" );
    return;
  }

  printf( "\nAT:      \"%s\" line %d\n\n", ((JogReader*)*reader)->filename->data, line );
  printf( "CONTEXT: " );

  //printf( "CONTEXT: " );
  Ref<JogReader> r = (JogReader*) *reader;
  Ref<JogReader> context_reader = new JogReader(r);

  while (context_reader->line < line) context_reader->read();

  int c = column;
  while (c > 0 && context_reader->peek() == ' ')
  {
    --c;
    context_reader->read();
  }
  while (c > 60)
  {
    --c;
    context_reader->read();
  }
  int len = 70;
  while (len--)
  {
    int ch = context_reader->read();
    if (ch == -1 || ch == 10) break;
    printf( "%c", (char) ch );
  }
  printf("\n         ");
  while (--c > 0) printf(" ");
  printf("^\n");

  printf( "===============================================================================\n" );
}

Ref<ASCIIString> JogError::context()
{
  Ref<JogReader> context_reader = new JogReader( (JogReader*) *reader );

  StringBuilder buffer;
  while (context_reader->line < line) context_reader->read();
  while (context_reader->line == line)
  {
    int ch = context_reader->read();
    if (ch == 10 || ch == -1) break;
    buffer.print( (char) ch );
  }
  return new ASCIIString(buffer.to_string());
}

//=============================================================================
//  JogCmd
//=============================================================================
JogTypeInfo* JogCmd::as_type()
{
  JogTypeInfo* possible_type = reinterpret_as_type();
  if (possible_type && possible_type->is_type()) return possible_type;
  return NULL;
}

JogTypeInfo* JogCmd::require_value()
{
  JogTypeInfo* result = type();
  if (result == NULL) throw error( "Expression must result in a value." );
  return result;
}

Ref<JogCmd> JogCmd::discarding_result()
{
  JogTypeInfo* result_type = type();

  if (result_type == NULL) return this;

  if (result_type->is_primitive()) return new JogCmdDiscardDataResult(t,this);

  return new JogCmdDiscardRefResult(t,this);
}

Ref<JogCmd> JogCmd::cast_to_type( JogTypeInfo* to_type )
{
  JogTypeInfo* cur_type = type();

  if (cur_type == NULL)
  {
    throw t->error( "Expression must result in a value." );
  }

  if (cur_type == to_type) return this;

  if (to_type->is_primitive())
  {
    if (cur_type->is_primitive())
    {
      if (to_type == jog_type_manager.type_boolean && cur_type != jog_type_manager.type_boolean)
      {
        throw t->error( "Cannot cast a boolean value to a non-boolean value." );
      }

      if (to_type != jog_type_manager.type_boolean && cur_type == jog_type_manager.type_boolean)
      {
        throw t->error( "Cannot cast a non-boolean value to a boolean value." );
      }

      return new JogCmdCast( t, this, to_type );
    }
    else
    {
      // Possibly un-box primitive
      if (cur_type == jog_type_manager.type_boolean_wrapper)
      {
        if (to_type == jog_type_manager.type_boolean)
        {
          return new JogCmdMemberAccess( t, this,
              new JogCmdMethodCall( t, new JogString("booleanValue"), new JogCmdList(t) )
            );
        }
      }

      if (cur_type->instance_of(jog_type_manager.type_number))
      {
        const char* m_name;
        if (to_type == jog_type_manager.type_real64) m_name = "doubleValue";
        else if (to_type == jog_type_manager.type_real32) m_name = "floatValue";
        else if (to_type == jog_type_manager.type_int64)  m_name = "longValue";
        else if (to_type == jog_type_manager.type_int32)  m_name = "intValue";
        else if (to_type == jog_type_manager.type_int16)  m_name = "shortValue";
        else if (to_type == jog_type_manager.type_int8)   m_name = "byteValue";
        else                                              m_name = "charValue";

        return new JogCmdMemberAccess( t, this,
            new JogCmdMethodCall( t, new JogString(m_name), new JogCmdList(t) )
          );
      }
    }
  }
  else
  {
    if (cur_type->is_primitive())
    {
      // Possibly box primitive type.
      if (to_type == cur_type->wrapper_type() || to_type == jog_type_manager.type_object
          || to_type->instance_of(jog_type_manager.type_number))
      {
        Ref<JogCmdList> args = new JogCmdList(t);
        args->add( this );
        return new JogCmdNewObject( t, cur_type->wrapper_type(), args );
      }
    }
  }

  if (to_type == jog_type_manager.type_string)
  {
    if (cur_type->is_reference())
    {
      Ref<JogCmdList> args = new JogCmdList(t);
      Ref<JogCmd> cmd = new JogCmdMemberAccess( t,
          this,
          new JogCmdMethodCall( t, new JogString("toString"), args )
        );
      return cmd->resolve();
    }
  }

  if (cur_type->is_reference() && to_type->is_reference())
  {
    if (cur_type->instance_of(to_type))
    {
      return new JogCmdWideningCast(t,this,to_type);
    }
    else if (to_type->instance_of(cur_type))
    {
      return new JogCmdNarrowingCast(t,this,to_type);
    }
    else
    {
      StringBuilder buffer;
      buffer.print( "Cannot cast '" );
      cur_type->name->print(buffer);
      buffer.print( "' to '" );
      to_type->name->print(buffer);
      buffer.print( "'; types are unrelated." );
      throw error( (const char*) buffer.to_string() );
    }
  }

  StringBuilder buffer;
  buffer.print( "Cannot cast '" );
  cur_type->name->print(buffer);
  buffer.print( "' to '" );
  to_type->name->print(buffer);
  buffer.print( "'." );
  throw error( (const char*) buffer.to_string() );
}

JogTypeInfo* JogCmd::require_reference()
{
  JogTypeInfo* this_type = type();
  if (this_type == NULL || (this_type->qualifiers & JOG_QUALIFIER_REFERENCE) == 0)
  {
    throw error( "Object reference required." );
  }
  return this_type;
}

JogTypeInfo* JogCmd::require_instance_of( JogTypeInfo* base_type )
{
  JogTypeInfo* this_type = require_reference();
  if ( !this_type->instance_of(base_type) )
  {
    StringBuilder buffer;
    buffer.print( "'" );
    base_type->name->print(buffer);
    buffer.print( "' reference required." );
    throw error( (const char*) buffer.to_string() );
  }
  return this_type;
}

JogTypeInfo* JogCmd::require_primitive()
{
  JogTypeInfo* this_type = type();
  if (this_type == NULL || (this_type->qualifiers & JOG_QUALIFIER_PRIMITIVE) == 0)
  {
    throw error( "Primitive value required." );
  }
  return this_type;
}

Ref<JogCmd> JogCmd::box( JogTypeInfo* as_type )
{
  JogTypeInfo* this_type = type();
  if (this_type->instance_of(jog_type_manager.type_number)) return this;

  JogTypeInfo* wrapper_type = this_type->wrapper_type();
  if (wrapper_type) 
  {
    wrapper_type->resolve();
  }
  /*
  else
  {
    StringBuilder buffer;
    buffer.print( "[Internal] Cannot box type '" );
    this_type->name->print(buffer);
    buffer.print( "'." );
    Ref<JogError> err = new JogError( (const char*) buffer.to_string() );
    throw err;
  }
  */

  if (this_type->is_primitive()  && wrapper_type
      && (wrapper_type == as_type || as_type == jog_type_manager.type_object
        || as_type->instance_of(jog_type_manager.type_number)))
  {
    Ref<JogCmdList> args = new JogCmdList(t);
    args->add( this );
    return (new JogCmdNewObject( t, wrapper_type, args ))->resolve();
  }
  return this;
}


//=============================================================================
//  JogVM
//=============================================================================
JogVM::JogVM() : max_object_bytes(1024*1024), cur_object_bytes(0), 
          all_objects(NULL), user_context(NULL), timeout_seconds(0)
{
  random_seed = (int) time(0);
  ostringstream buffer;
  buffer << random_seed;
  JogReader::random_seed = buffer.str();

  jog_type_manager.init();
  JogMethodInfo::next_method_id = 1;

  jog_context = NULL;
  instruction_stack_limit = instruction_stack + JOG_INSTRUCTION_STACK_CAPACITY;
  instruction_stack_ptr   = instruction_stack_limit;
  data_stack_limit        = data_stack + JOG_DATA_STACK_CAPACITY;
  data_stack_ptr          = data_stack_limit;
  ref_stack_limit         = ref_stack + JOG_REF_STACK_CAPACITY;
  ref_stack_ptr           = ref_stack_limit;
  frame_stack_limit       = frames + JOG_FRAME_STACK_CAPACITY;
  frame_ptr               = frame_stack_limit;
  add_native_handlers();
}

void JogVM::reset()
{
  jog_type_manager.clear();
  parsed_types.clear();
  delete_all_objects();
}

void JogVM::parse( string filename )
{
  //printf( "Parsing %s\n", filename.c_str() );
  Ref<JogParser> parser = new JogParser(filename.c_str());
  parse(parser);
}

void JogVM::parse( string filename, string content )
{
  //printf( "Parsing %s\n", filename.c_str() );
  Ref<JogParser> parser = new JogParser( new JogScanner( 
        new JogReader( filename.c_str(), content.c_str(), content.length() )
      ) );
  parse(parser);
}

void JogVM::parse( Ref<JogParser> parser )
{
  JogTypeInfo* type = parser->parse_type_def();
  while (type)
  {
    type = parser->parse_type_def();
  }
  parser->scanner->must_consume( TOKEN_EOF, "Unrecognized code." );

  for (int i=0; i<parser->parsed_types.count; ++i)
  {
    JogTypeInfo* type = parser->parsed_types[i];
    if (type->is_template()) continue;

    parsed_types.add( parser->parsed_types[i] );
  }
}

void JogVM::compile()
{
  jog_type_manager.type_object = jog_type_manager.must_find_type("Object");
  jog_type_manager.type_string = jog_type_manager.must_find_type("String");
  jog_type_manager.type_char_array = jog_type_manager.must_find_type("char[]");

  jog_type_manager.type_number = jog_type_manager.must_find_type("Number");
  jog_type_manager.type_real64_wrapper = jog_type_manager.must_find_type("Double");
  jog_type_manager.type_real32_wrapper = jog_type_manager.must_find_type("Float");
  jog_type_manager.type_int64_wrapper = jog_type_manager.must_find_type("Long");
  jog_type_manager.type_int32_wrapper = jog_type_manager.must_find_type("Integer");
  jog_type_manager.type_int16_wrapper = jog_type_manager.must_find_type("Short");
  jog_type_manager.type_int8_wrapper = jog_type_manager.must_find_type("Byte");
  jog_type_manager.type_char_wrapper = jog_type_manager.must_find_type("Character");
  jog_type_manager.type_boolean_wrapper = jog_type_manager.must_find_type("Boolean");
  jog_type_manager.type_real64_wrapper->prep();
  jog_type_manager.type_real32_wrapper->prep();
  jog_type_manager.type_int64_wrapper->prep();
  jog_type_manager.type_int32_wrapper->prep();
  jog_type_manager.type_int16_wrapper->prep();
  jog_type_manager.type_int8_wrapper->prep();
  jog_type_manager.type_char_wrapper->prep();
  jog_type_manager.type_boolean_wrapper->prep();

  for (int i=0; i<parsed_types.count; ++i)
  {
    parsed_types[i]->resolve();
  }
}

void JogVM::run( const char* main_class_name )
{
  JogTypeInfo* main_class = JogTypeInfo::find(main_class_name);
  if (main_class == NULL)
  {
    StringBuilder buffer;
    buffer.print( "Main class '" );
    buffer.print( main_class_name );
    buffer.print( "' not found." );
    Ref<JogError> err = new JogError( (const char*) buffer.to_string() );
    throw err;
  }

//JogTypeInfo* debug_type = JogTypeInfo::find("Test");
//if (debug_type) debug_type->print_members();  //DEBUG

  // Call static initializer blocks
  for (int i=0; i<parsed_types.count; ++i)
  {
    JogTypeInfo* type = parsed_types[i];

    for (int j=0; j<type->static_initializers.count; ++j)
    {
      JogMethodInfo* m = *(type->static_initializers[j]);
      Ref<JogToken>  t = m->t;
      Ref<JogCmd> cmd = new JogCmdClassCall( t, m, NULL, NULL );
      push(*cmd);
      execute();
    }
  }

  JogRef main_object = main_class->create_instance(this);
  if ( *(main_class->call_init_object) )
  {
    push( main_object );
    push( *(main_class->call_init_object) );
    execute();
    /*
    Ref<JogToken>  t = main_class->m_init_object->t;
    Ref<JogCmd> cmd = new JogCmdStaticCall( t,
        *(main_class->m_init_object), new JogCmdObjectRef(t,main_object), NULL );
    push(*cmd);
    execute();
    */
  }

  call_void_method( main_object, "<init>()" );
}

void JogVM::add_native_handler( Ref<JogString> signature, JogNativeMethodHandler handler )
{
  native_methods[signature] = handler;
}

JogRef JogVM::create_object( JogTypeInfo* of_type )
{
  return of_type->create_instance(this);
}

JogRef JogVM::create_array( JogTypeInfo* of_type, int count )
{
  return of_type->create_array( this, count );
}

JogRef JogVM::create_string( JogChar* data, int count )
{
  JogRef chars = jog_type_manager.type_char_array->create_array(this,count);
  memcpy( (JogChar*)(chars->data), data, count*2 );
  return create_string( chars );
}

JogRef JogVM::create_string( JogRef array )
{
  JogChar* data = (JogChar*) array->data;
  int count = array->count;

  int hash = 0;
  JogChar* cur = (JogChar*) (data - 1);
  int c = count + 1;
  while (--c) hash = (hash << 1) + *(++cur);

  JogTypeInfo* type_string = jog_type_manager.type_string;
  JogRef obj = type_string->create_instance(this);

  *((JogObject**)&(obj->data[0])) = *array;
  array->retain();
  obj->data[1] = hash;

  return obj;
}

void JogVM::force_garbage_collection()
{
printf("GC\n");
  // Delete unused objects at head of list.
  while (all_objects && !all_objects->reference_count)
  {
    JogObject* next = all_objects->next_object;
    cur_object_bytes -= all_objects->total_object_bytes();
    delete all_objects;
    all_objects = next;
  }

  // Delete unused objects in body of list.
  if (all_objects)
  {
    JogObject* prev = all_objects;
    JogObject* cur = all_objects->next_object;
    while (cur)
    {
      if (cur->reference_count)
      {
        prev = cur;
        cur = cur->next_object;
      }
      else
      {
        JogObject* next = cur->next_object;
        cur_object_bytes -= cur->total_object_bytes();
        delete cur;
        prev->next_object = next;
        cur = next;
      }
    }
  }
}

void JogVM::delete_all_objects()
{
  // Release any objects on reference stack
  while (ref_stack_ptr != ref_stack_limit) *(ref_stack_ptr++) = NULL;

  while (all_objects)
  {
    JogObject* next = all_objects->next_object;
    delete all_objects;
    all_objects = next;
  }
}

void JogVM::register_object( JogObject* obj, int byte_size )
{
  obj->next_object = all_objects;
  all_objects = obj;
  cur_object_bytes += byte_size;
  if (cur_object_bytes > max_object_bytes)
  {
    force_garbage_collection();
    if (cur_object_bytes > max_object_bytes)
    {
      Ref<JogError> err = new JogError("Out of allotted memory.");
      throw err;
    }
  }
}

void JogVM::push_frame( JogMethodInfo* method_info )
{
  // Set frame registers to point just before start of object
  // context and parameters.
  (--frame_ptr)->init( instruction_stack_ptr, 
      data_stack_ptr + method_info->param_data_count, 
      ref_stack_ptr + method_info->param_ref_count,
      method_info );

  data_stack_ptr -= method_info->local_data_count;
  ref_stack_ptr  -= method_info->local_ref_count;

}

void JogVM::push( JogRef object )
{
  if (ref_stack_ptr == ref_stack)
  {
    Ref<JogError> err = new JogError("Reference stack limit reached during recursion.");
    throw err;
  }
  *(--ref_stack_ptr) = object;
}

//=============================================================================
//  JogTypeInfo
//=============================================================================
JogTypeInfo* JogTypeInfo::create( Ref<JogToken> t, int qualifiers, Ref<JogString> name )
{
  JogTypeLookup::iterator entry = jog_type_manager.type_lookup.find(name);

  if (entry == jog_type_manager.type_lookup.end())
  {
    JogTypeInfo* type = new JogTypeInfo();
    type->t = t;
    type->name = name;
    type->qualifiers = qualifiers;
    jog_type_manager.type_lookup[name] = type;
    return type;
  }
  else
  {
    if (entry->second->qualifiers == 0)
    {
      // Only referenced before, not defined - still good.
      JogTypeInfo* type = *entry->second;
      type->t = t;
      type->qualifiers = qualifiers;
      return type;
    }
    else
    {
      // Already defined
      JogTypeInfo* type = *entry->second;

      StringBuilder buffer;
      buffer.print( "Type " );
      buffer.print( type->name->to_ascii()->data );
      buffer.print( " was already defined at line " );
      buffer.print( type->t->line );

      if (type->t->reader->filename == t->reader->filename)
      {
        buffer.print(".");
      }
      else
      {
        buffer.print( " of \"" );
        buffer.print( type->t->reader->filename->data );
        buffer.print( "." );
      }
      Ref<ASCIIString> error_mesg = new ASCIIString( buffer.to_string() );
      throw entry->second->t->error( error_mesg );
    }
  }
}

JogTypeInfo* JogTypeInfo::create( Ref<JogToken> t, int qualifiers, const char* name )
{
  return create( t, qualifiers, new JogString(name) );
}

JogTypeInfo* JogTypeInfo::reference( Ref<JogToken> t, Ref<JogString> name )
{
  JogTypeLookup::iterator entry = jog_type_manager.type_lookup.find(name);

  if (entry == jog_type_manager.type_lookup.end())
  {
    JogTypeInfo* type = new JogTypeInfo();
    type->t = t;
    type->name = name;
    type->qualifiers = 0;
    jog_type_manager.type_lookup[name] = type;
    return type;
  }
  else if (entry->second == jog_type_manager.type_void)
  {
    return NULL;
  }
  else
  {
    return *(entry->second);
  }
}

JogTypeInfo* JogTypeInfo::reference_array( Ref<JogToken> t, Ref<JogString> name, int dimensions )
{
  Ref<JogString> new_name = new JogString(name);
  for (int i=0; i<dimensions; ++i)
  {
    new_name->add( "[]" );
  }
  return reference( t, new_name );
}

JogTypeInfo* JogTypeInfo::find( Ref<JogString> name )
{
  JogTypeLookup::iterator entry = jog_type_manager.type_lookup.find(name);

  if (entry == jog_type_manager.type_lookup.end())
  {
    return NULL;
  }
  else
  {
    return *(entry->second);
  }
}

JogRef JogTypeInfo::create_array( JogVM* vm, int count )
{
  int obj_size = (sizeof(JogObject) - 8) + count * element_type->element_size;

  JogObject* obj = (JogObject*) new char[obj_size];
  memset( obj, 0, obj_size );
  obj->type = this;
  obj->count = count;

  JogRef result((JogObject*)obj);
  vm->register_object(obj,obj_size);

  return result;
}

bool JogTypeInfo::instance_of( JogTypeInfo* base_type )
{
  // Same type
  if (this == base_type) return true;

  // Everything is instanceof Object.
  if (base_type == jog_type_manager.type_object) return true;

  // 'null' is instanceof anything.
  if (this == jog_type_manager.type_null) return true;

  if (base_class && base_class->instance_of(base_type)) return true;

  for (int i=0; i<interfaces.count; ++i)
  {
    if (interfaces[i]->instance_of(base_type)) return true;
  }

  return false;
}

bool JogTypeInfo::is_boolean()
{
  return (this == jog_type_manager.type_boolean);
}

bool JogTypeInfo::is_real()
{
  return (this == jog_type_manager.type_real64 || this == jog_type_manager.type_real32);
}

bool JogTypeInfo::is_integer()
{
  return (this == jog_type_manager.type_int64 || this == jog_type_manager.type_int32
      || this == jog_type_manager.type_int16 || this == jog_type_manager.type_int8
      || this == jog_type_manager.type_char);
}


int  JogTypeInfo::precision()
{
  if (is_primitive())
  {
    if (this == jog_type_manager.type_real64) return 7;
    if (this == jog_type_manager.type_real32) return 6;
    if (this == jog_type_manager.type_int64)  return 5;
    if (this == jog_type_manager.type_int32)  return 4;
    if (this == jog_type_manager.type_int16)  return 3;
    if (this == jog_type_manager.type_char)   return 2;
    if (this == jog_type_manager.type_int8)   return 1;
  }
  return 0;
}

bool JogTypeInfo::is_compatible_with( JogTypeInfo* other )
{
  if (this == other) return true;


  if (this->is_reference() ^ other->is_reference()) 
  {
    if (this->is_reference())
    {
      if (this == other->wrapper_type()) return true;
      if (this == jog_type_manager.type_number) return true;
      if (this == jog_type_manager.type_object) return true;
    }
    else
    {
      if (other == this->wrapper_type()) return true;
      if (other == jog_type_manager.type_number) return true;
      if (other == jog_type_manager.type_object) return true;
    }
    return false;
  }

  if (is_primitive())
  {
    if (other == jog_type_manager.type_real64) return true;
    if (this  == jog_type_manager.type_real64) return false;
    if (other == jog_type_manager.type_real32) return true;
    if (this  == jog_type_manager.type_real32) return false;
    if (other == jog_type_manager.type_int64) return true;
    if (this  == jog_type_manager.type_int64) return false;
    if (other == jog_type_manager.type_int32) return true;
    if (this  == jog_type_manager.type_int32) return false;
    return (other == jog_type_manager.type_int16 && this == jog_type_manager.type_int8);
  }
  else
  {
    return instance_of(other);
  }
}

JogTypeInfo* JogTypeInfo::wrapper_type()
{
  if (this == jog_type_manager.type_real64)
  {
    return jog_type_manager.type_real64_wrapper;
  }
  if (this == jog_type_manager.type_real32)
  {
    return jog_type_manager.type_real32_wrapper;
  }
  if (this == jog_type_manager.type_int64)
  {
    return jog_type_manager.type_int64_wrapper;
  }
  if (this == jog_type_manager.type_int32)
  {
    return jog_type_manager.type_int32_wrapper;
  }
  if (this == jog_type_manager.type_int16)
  {
    return jog_type_manager.type_int16_wrapper;
  }
  if (this == jog_type_manager.type_int8)
  {
    return jog_type_manager.type_int8_wrapper;
  }
  if (this == jog_type_manager.type_char)
  {
    return jog_type_manager.type_char_wrapper;
  }
  if (this == jog_type_manager.type_boolean)
  {
    return jog_type_manager.type_boolean_wrapper;
  }
  return NULL;
}


//=============================================================================
//  JogTypeManager
//=============================================================================
JogTypeManager jog_type_manager;


Ref<JogCmd> JogCmdLiteralReal64::cast_to_type( JogTypeInfo* to_type )
{
  if (to_type == jog_type_manager.type_real64) return this;

  if (to_type == jog_type_manager.type_real32)
  {
    return new JogCmdLiteralReal32( t, (float) value );
  }

  if (to_type == jog_type_manager.type_int64)
  {
    return new JogCmdLiteralInt64( t, (JogInt64) value );
  }

  if (to_type == jog_type_manager.type_int32)
  {
    return new JogCmdLiteralInt32( t, (int) value );
  }

  if (to_type == jog_type_manager.type_int16)
  {
    return new JogCmdLiteralInt16( t, (JogInt16) value );
  }

  if (to_type == jog_type_manager.type_int8)
  {
    return new JogCmdLiteralInt8( t, (JogInt8) value );
  }

  if (to_type == jog_type_manager.type_char)
  {
    return new JogCmdLiteralChar( t, (JogChar) value );
  }

  if (to_type == jog_type_manager.type_real64_wrapper || to_type == jog_type_manager.type_object)
  {
    Ref<JogCmdList> args = new JogCmdList(t);
    args->add( this );
    return new JogCmdNewObject( t, jog_type_manager.type_real64_wrapper, args );
  }

  StringBuilder buffer;
  buffer.print( "Can't cast type 'double' to type '" );
  buffer.print( to_type->name->to_ascii()->data );
  buffer.print( "'." );
  throw error( buffer.to_string() );
}

Ref<JogCmd> JogCmdLiteralReal32::cast_to_type( JogTypeInfo* to_type )
{
  if (to_type == jog_type_manager.type_real32) return this;

  if (to_type == jog_type_manager.type_real64)
  {
    return new JogCmdLiteralReal64( t, value );
  }

  if (to_type == jog_type_manager.type_int64)
  {
    return new JogCmdLiteralInt64( t, (JogInt64) value );
  }

  if (to_type == jog_type_manager.type_int32)
  {
    return new JogCmdLiteralInt32( t, (int) value );
  }

  if (to_type == jog_type_manager.type_int16)
  {
    return new JogCmdLiteralInt16( t, (JogInt16) value );
  }

  if (to_type == jog_type_manager.type_int8)
  {
    return new JogCmdLiteralInt8( t, (JogInt8) value );
  }

  if (to_type == jog_type_manager.type_char)
  {
    return new JogCmdLiteralChar( t, (JogChar) value );
  }

  if (to_type == jog_type_manager.type_real32_wrapper || to_type == jog_type_manager.type_object)
  {
    Ref<JogCmdList> args = new JogCmdList(t);
    args->add( this );
    return new JogCmdNewObject( t, jog_type_manager.type_real32_wrapper, args );
  }

  StringBuilder buffer;
  buffer.print( "Can't cast type 'float' to type '" );
  buffer.print( to_type->name->to_ascii()->data );
  buffer.print( "'." );
  throw error( buffer.to_string() );
}

Ref<JogCmd> JogCmdLiteralInt64::cast_to_type( JogTypeInfo* to_type )
{
  if (to_type == jog_type_manager.type_int64) return this;

  if (to_type == jog_type_manager.type_real64)
  {
    return new JogCmdLiteralReal64( t, (double) value );
  }

  if (to_type == jog_type_manager.type_real32)
  {
    return new JogCmdLiteralReal32( t, (float) value );
  }

  if (to_type == jog_type_manager.type_int32)
  {
    return new JogCmdLiteralInt32( t, (JogInt32) value );
  }

  if (to_type == jog_type_manager.type_int16)
  {
    return new JogCmdLiteralInt16( t, (JogInt16) value );
  }

  if (to_type == jog_type_manager.type_int8)
  {
    return new JogCmdLiteralInt8( t, (JogInt8) value );
  }

  if (to_type == jog_type_manager.type_char)
  {
    return new JogCmdLiteralChar( t, (JogChar) value );
  }

  if (to_type == jog_type_manager.type_int64_wrapper || to_type == jog_type_manager.type_object)
  {
    Ref<JogCmdList> args = new JogCmdList(t);
    args->add( this );
    return new JogCmdNewObject( t, jog_type_manager.type_int64_wrapper, args );
  }

  StringBuilder buffer;
  buffer.print( "Can't cast type 'long' to type '" );
  buffer.print( to_type->name->to_ascii()->data );
  buffer.print( "'." );
  throw error( buffer.to_string() );
}

Ref<JogCmd> JogCmdLiteralInt32::cast_to_type( JogTypeInfo* to_type )
{
  if (to_type == jog_type_manager.type_int32) return this;

  if (to_type == jog_type_manager.type_real64)
  {
    return new JogCmdLiteralReal64( t, (double) value );
  }

  if (to_type == jog_type_manager.type_real32)
  {
    return new JogCmdLiteralReal32( t, (float) value );
  }

  if (to_type == jog_type_manager.type_int64)
  {
    return new JogCmdLiteralInt64( t, (JogInt64) value );
  }

  if (to_type == jog_type_manager.type_int16)
  {
    return new JogCmdLiteralInt16( t, (JogInt16) value );
  }

  if (to_type == jog_type_manager.type_int8)
  {
    return new JogCmdLiteralInt8( t, (JogInt8) value );
  }

  if (to_type == jog_type_manager.type_char)
  {
    return new JogCmdLiteralChar( t, (JogChar) value );
  }

  if (to_type == jog_type_manager.type_int32_wrapper || to_type == jog_type_manager.type_object)
  {
    Ref<JogCmdList> args = new JogCmdList(t);
    args->add( this );
    return new JogCmdNewObject( t, jog_type_manager.type_int32_wrapper, args );
  }

  StringBuilder buffer;
  buffer.print( "Can't cast type 'int' to type '" );
  buffer.print( to_type->name->to_ascii()->data );
  buffer.print( "'." );
  throw error( buffer.to_string() );
}

Ref<JogCmd> JogCmdLiteralInt16::cast_to_type( JogTypeInfo* to_type )
{
  if (to_type == jog_type_manager.type_int16) return this;

  if (to_type == jog_type_manager.type_real64)
  {
    return new JogCmdLiteralReal64( t, (double) value );
  }

  if (to_type == jog_type_manager.type_real32)
  {
    return new JogCmdLiteralReal32( t, (float) value );
  }

  if (to_type == jog_type_manager.type_int64)
  {
    return new JogCmdLiteralInt64( t, (JogInt64) value );
  }

  if (to_type == jog_type_manager.type_int32)
  {
    return new JogCmdLiteralInt32( t, (JogInt32) value );
  }

  if (to_type == jog_type_manager.type_int8)
  {
    return new JogCmdLiteralInt8( t, (JogInt8) value );
  }

  if (to_type == jog_type_manager.type_char)
  {
    return new JogCmdLiteralChar( t, (JogChar) value );
  }

  if (to_type == jog_type_manager.type_int16_wrapper || to_type == jog_type_manager.type_object)
  {
    Ref<JogCmdList> args = new JogCmdList(t);
    args->add( this );
    return new JogCmdNewObject( t, jog_type_manager.type_int16_wrapper, args );
  }

  StringBuilder buffer;
  buffer.print( "Can't cast type 'short' to type '" );
  buffer.print( to_type->name->to_ascii()->data );
  buffer.print( "'." );
  throw error( buffer.to_string() );
}

Ref<JogCmd> JogCmdLiteralInt8::cast_to_type( JogTypeInfo* to_type )
{
  if (to_type == jog_type_manager.type_int8) return this;

  if (to_type == jog_type_manager.type_real64)
  {
    return new JogCmdLiteralReal64( t, (double) value );
  }

  if (to_type == jog_type_manager.type_real32)
  {
    return new JogCmdLiteralReal32( t, (float) value );
  }

  if (to_type == jog_type_manager.type_int64)
  {
    return new JogCmdLiteralInt64( t, (JogInt64) value );
  }

  if (to_type == jog_type_manager.type_int32)
  {
    return new JogCmdLiteralInt32( t, (JogInt32) value );
  }

  if (to_type == jog_type_manager.type_int16)
  {
    return new JogCmdLiteralInt16( t, (JogInt16) value );
  }

  if (to_type == jog_type_manager.type_char)
  {
    return new JogCmdLiteralChar( t, (JogChar) value );
  }

  if (to_type == jog_type_manager.type_int8_wrapper || to_type == jog_type_manager.type_object)
  {
    Ref<JogCmdList> args = new JogCmdList(t);
    args->add( this );
    return new JogCmdNewObject( t, jog_type_manager.type_int8_wrapper, args );
  }


  StringBuilder buffer;
  buffer.print( "Can't cast type 'byte' to type '" );
  buffer.print( to_type->name->to_ascii()->data );
  buffer.print( "'." );
  throw error( buffer.to_string() );
}

Ref<JogCmd> JogCmdLiteralChar::cast_to_type( JogTypeInfo* to_type )
{
  if (to_type == jog_type_manager.type_char) return this;

  if (to_type == jog_type_manager.type_real64)
  {
    return new JogCmdLiteralReal64( t, (double) value );
  }

  if (to_type == jog_type_manager.type_real32)
  {
    return new JogCmdLiteralReal32( t, (float) value );
  }

  if (to_type == jog_type_manager.type_int64)
  {
    return new JogCmdLiteralInt64( t, (JogInt64) value );
  }

  if (to_type == jog_type_manager.type_int32)
  {
    return new JogCmdLiteralInt32( t, (JogInt32) value );
  }

  if (to_type == jog_type_manager.type_int16)
  {
    return new JogCmdLiteralInt16( t, (JogInt16) value );
  }

  if (to_type == jog_type_manager.type_int8)
  {
    return new JogCmdLiteralInt8( t, (JogInt8) value );
  }

  if (to_type == jog_type_manager.type_char_wrapper || to_type == jog_type_manager.type_object)
  {
    Ref<JogCmdList> args = new JogCmdList(t);
    args->add( this );
    return new JogCmdNewObject( t, jog_type_manager.type_char_wrapper, args );
  }

  StringBuilder buffer;
  buffer.print( "Can't cast type 'char' to type '" );
  buffer.print( to_type->name->to_ascii()->data );
  buffer.print( "'." );
  throw error( buffer.to_string() );
}

Ref<JogCmd> JogCmdLiteralBoolean::cast_to_type( JogTypeInfo* to_type )
{
  if (to_type == jog_type_manager.type_boolean) return this;

  if (to_type == jog_type_manager.type_boolean_wrapper || to_type == jog_type_manager.type_object)
  {
    Ref<JogCmdList> args = new JogCmdList(t);
    args->add( this );
    return new JogCmdNewObject( t, jog_type_manager.type_boolean_wrapper, args );
  }

  throw error( "A boolean cannot be cast to any other type." );
}

void JogCmd::require_boolean()
{
  if (type() != jog_type_manager.type_boolean)
  {
    throw error( "A boolean value is required." );
  }
}

JogTypeInfo* JogCmd::require_integer()
{
  JogTypeInfo* this_type = type();
  if (this_type == jog_type_manager.type_int64
    || this_type == jog_type_manager.type_int32
    || this_type == jog_type_manager.type_int16
    || this_type == jog_type_manager.type_int8
    || this_type == jog_type_manager.type_char)
  {
    return this_type;
  }
  throw error( "An integer value is required." );
}

JogTypeInfo* JogCmd::require_integer_or_boolean()
{
  JogTypeInfo* this_type = type();
  if (this_type == jog_type_manager.type_int64
    || this_type == jog_type_manager.type_int32
    || this_type == jog_type_manager.type_int16
    || this_type == jog_type_manager.type_int8
    || this_type == jog_type_manager.type_char
    || this_type == jog_type_manager.type_boolean)
  {
    return this_type;
  }
  throw error( "An integer or boolean value is required." );
}


//=============================================================================
//  JogVM
//=============================================================================
void JogVM::call_native( JogMethodInfo* m )
{
  if (m->native_handler == NULL)
  {
    m->native_handler = native_methods[m->full_signature];
    if (m->native_handler == NULL)
    {
      throw m->t->error( "Native method not implemented in virtual machine." );
    }
  }
  m->native_handler(this);
}

void JogVM::call_void_method( JogRef context, const char* signature )
{
  JogMethodInfo* m = context->type->methods_by_signature[signature];
  Ref<JogToken>  t = m->t;
  Ref<JogCmd> cmd = new JogCmdDynamicCall( t, m, new JogCmdObjectRef(t,context), NULL );

  push(*cmd);
  execute();
}


//=============================================================================
//  JogPropertyInfo
//=============================================================================
JogPropertyInfo::JogPropertyInfo( Ref<JogToken> t, int qualifiers, JogTypeInfo* type_context, 
    JogTypeInfo* type, Ref<JogString> name, Ref<JogCmd> initial_value )
  : t(t), qualifiers(qualifiers), index(-1), type_context(type_context), 
    type(type), name(name), initial_value(initial_value)
{
}

void JogPropertyInfo::print()
{
  type->print();
  printf(" ");
  name->print();
  if (*initial_value) 
  {
    printf("=");
    initial_value->print();
  }
  printf("\n");
}


//=============================================================================
//  JogLocalVarInfo
//=============================================================================
JogLocalVarInfo::JogLocalVarInfo( Ref<JogToken> t, JogTypeInfo* type, Ref<JogString> name )
    : t(t), type(type), name(name), index(-1)
{
}

void JogLocalVarInfo::print()
{
  type->print();
  printf(" ");
  name->print();
}



//=============================================================================
//  JogMethodInfo
//=============================================================================
int JogMethodInfo::next_method_id = 1;

JogMethodInfo::JogMethodInfo( Ref<JogToken> t, int qualifiers, JogTypeInfo* type_context,
    JogTypeInfo* return_type, Ref<JogString> name )
  : t(t), qualifiers(qualifiers), type_context(type_context), return_type(return_type), 
    calls_super_constructor(false),
    name(name), native_handler(NULL), organized(false), resolved(false)
{
  statements = new JogStatementList(t);
  method_id = next_method_id++;
  dispatch_id = 0;
}

void JogMethodInfo::print()
{
  if (return_type)
  {
    return_type->print();
    printf(" ");
  }
  else
  {
    printf("void ");
  }
  name->print();
  printf("\n");
  printf("{\n");
  statements->print();
  printf("}\n\n");
}

bool JogMethodInfo::is_less_specific_than( JogMethodInfo* other )
{
  // Requires: 
  //   - Methods have the same name.
  //   - Method parameters are compatible.
  // Return types are ignored.
  if (parameters.count != other->parameters.count) return false;

  int count = parameters.count;
  int less_specific = 0;
  int more_specific = 0;
  for (int i=0; i<count; ++i)
  {
    JogTypeInfo* this_type  = parameters[i]->type;
    JogTypeInfo* other_type = other->parameters[i]->type;
    if (this_type != other_type)
    {
      if (this_type->is_compatible_with(other_type))
      {
        if (this_type->is_reference() && other_type->is_primitive()) ++less_specific;
        else ++more_specific;
      }
      else
      {
        ++less_specific;
      }
    }
  }

  return (less_specific && !more_specific);
}

bool JogMethodInfo::is_inherited( JogTypeInfo* type )
{
  return type_context != type;
}


void JogMethodInfo::organize()
{
  if (organized) return;
  organized = true;

  if (return_type) return_type->organize();
  {
    UnicodeStringBuilder buffer;
    name->print(buffer);
    buffer.print('(');

    for (int i=0; i<parameters.count; ++i)
    {
      parameters[i]->type->organize();
      if (i > 0) buffer.print(',');
      parameters[i]->type->name->print(buffer);
    }
    buffer.print(')');
    signature = new JogString(buffer.to_string());
  }
  {
    UnicodeStringBuilder buffer;
    type_context->name->print(buffer);
    buffer.print("::");
    signature->print(buffer);
    full_signature = new JogString(buffer.to_string());
  }

  dispatch_id = jog_type_manager.dispatch_id_lookup[signature];
}

