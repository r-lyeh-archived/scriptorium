#include "jog.h"

JogContext* jog_context = NULL;

//=============================================================================
//  JogTypeManager
//=============================================================================
JogTypeInfo* JogTypeManager::must_find_type( const char* name )
{
  JogTypeInfo* type = JogTypeInfo::find(name);
  if (type == NULL)
  {
    StringBuilder buffer;
    buffer.print( "[Internal] Type '" );
    buffer.print( name );
    buffer.print( "' not found." );
    Ref<JogError> err = new JogError( (const char*) buffer.to_string() );
    throw err;
  }
  return type;
}

JogTypeInfo* JogTypeManager::find_common_type( JogToken* t, JogCmd* cmd1, JogCmd* cmd2, bool min32 )
{
  JogTypeInfo* type1 = cmd1->type();
  if (type1 == NULL)
  {
    throw cmd1->error( "Expression does not result in value." );
  }

  JogTypeInfo* type2 = cmd2->type();
  if (type2 == NULL)
  {
    throw cmd2->error( "Expression does not result in value." );
  }

  return find_common_type( t, type1, type2, min32 );
}

JogTypeInfo* JogTypeManager::find_common_type( JogToken* t, 
    JogTypeInfo* type1, JogTypeInfo* type2, bool min32 )
{
  if (type1 == type2 && !min32) return type1;

  if (type1->is_primitive())
  {
    if (type2->is_primitive())
    {
      if (type1 == type_boolean || type2 == type_boolean)
      {
        if (type1 == type_boolean && type2 == type_boolean) return type_boolean;
        throw t->error( "Cannot mix boolean and non-boolean types in an operation." );
      }
    }

    if (type1 == type_real64 || type2 == type_real64)
    {
      return type_real64;
    }

    if (type1 == type_real32 || type2 == type_real32)
    {
      return type_real32;
    }

    if (type1 == type_int64 || type2 == type_int64)
    {
      return type_int64;
    }

    return type_int32;
  }

  if (type1->instance_of(type2)) return type2;
  if (type2->instance_of(type1)) return type1;

  /*
  if (type1->is_primitive())
  {
    if (type2 == jog_type_manager.type_object) return jog_type_manager.type_object;
    if (type2 == jog_type_manager.type_string) return jog_type_manager.type_string;
    if (type2->instance_of(jog_type_manager.type_number)) return jog_type_manager.type_number;
  }
  else if (type2->is_primitive())
  {
    if (type1 == jog_type_manager.type_object) return jog_type_manager.type_object;
    if (type1 == jog_type_manager.type_string) return jog_type_manager.type_string;
    if (type1->instance_of(jog_type_manager.type_number)) return jog_type_manager.type_number;
  }
  */

  throw t->error( "Types are unrelated." );
}

//=============================================================================
//  JogTypeInfo
//=============================================================================
void JogTypeInfo::organize()
{
  if (organized) return;
  organized = true;

  element_size = sizeof(void*);

  if (qualifiers == 0)
  {
    if (name->get(-1) == ']')
    {
      element_type = JogTypeInfo::reference( t, name->substring(0,name->count-3) );
      element_type->organize();
      qualifiers = JOG_QUALIFIER_REFERENCE;
    }
    else if (name->get(-1) == '>')
    {
      // Template type - find base template definition, duplicate its token set
      // while substituting types, and the type def.
      Ref<JogString> templ_name = name->before_first('<');
      JogTypeInfo* templ = JogTypeInfo::reference( t, templ_name );
      templ->organize();
      qualifiers = templ->qualifiers;

      if (templ->placeholder_types.count == 0)
      {
        StringBuilder buffer;
        buffer.print( "'" );
        buffer.print( templ->name->to_ascii()->data );
        buffer.print( "' is not a generic class." );
        throw t->error( buffer.to_string() );
      }

      RefList<JogString> subst_names;
      name->substring(templ_name->count+1,name->count-2)->split(',',subst_names);
      if (templ->placeholder_types.count != subst_names.count)
      {
        throw t->error( "Incorrect number of substitution types." );
      }

      //ArrayList<JogTypeInfo*> subst_types;
      for (int i=0; i<subst_names.count; ++i)
      {
        JogTypeInfo* subst_type = JogTypeInfo::reference( t, subst_names[i] );
        subst_type->organize();
        if (subst_type->is_primitive())
        {
          throw t->error( "Primitives cannot be used as substitution types." );
        }
      }

      int count = templ->template_tokens.count;
      RefList<JogToken> tokens;
      for (int i=0; i<count; ++i)
      {
        Ref<JogToken> t2 = templ->template_tokens[i];

        if (t2->type == TOKEN_ID)
        {
          if (t2->content->equals(templ->name))
          {
            // E.g. constructors for ArrayList<DataType> are written
            // as ArrayList() but are transformed to ArrayList<DataType>()
            Ref<JogToken> t3 = new JogToken( t2->reader, t2->line, t2->column );
            t3->type = t2->type;
            t3->content = name;
            t2 = t3;
          }
          else
          {
            for (int i=0; i<templ->placeholder_types.count; ++i)
            {
              if (t2->content->equals(templ->placeholder_types[i].type->name))
              {
                Ref<JogToken> t3 = new JogToken( t2->reader, t2->line, t2->column );
                t3->type = t2->type;
                t3->content = subst_names[i];
                t2 = t3;
                break;
              }
            }
          }
        }

        tokens.add( t2 );
      }
      Ref<JogScanner> scanner = new JogScanner( tokens );

      Ref<JogParser> parser = new JogParser(scanner);
      parser->parse_type_def( t, this );
    }
    else
    {
      StringBuilder buffer;
      buffer.print( "Reference to undefined type '" );
      buffer.print( name->to_ascii()->data );
      buffer.print( "'." );
      throw t->error( buffer.to_string() );
    }
  }

  if ( !base_class && this != jog_type_manager.type_object )
  {
    base_class = jog_type_manager.type_object;
  }

  if (base_class && is_class() && base_class->is_interface())
  {
    throw t->error( "Use 'implements' instead of 'extends' for base interface types." );
  }

  if (base_class) base_class->organize();

  for (int i=0; i<interfaces.count; ++i) interfaces[i]->organize();
}

void JogTypeInfo::prep()
{
  if (prepped) return;
  prepped = true;

  organize();

  RefList<JogMethodInfo> original_class_methods;
  for (int i=0; i<class_methods.count; ++i) original_class_methods.add(class_methods[i]);
  class_methods.clear();

  RefList<JogMethodInfo> original_methods;
  for (int i=0; i<methods.count; ++i) original_methods.add(methods[i]);
  methods.clear();

  if (base_class)
  {
    base_class->prep();

    if (base_class->instance_of(this))
    {
      throw t->error( "Illegal cyclic inheritance." );
    }

    for (int i=base_class->properties.count-1; i>=0; --i)
    {
      properties.insert( base_class->properties[i] );
    }

    for (int i=0; i<base_class->class_methods.count; ++i)
    {
      add( base_class->class_methods[i] );
    }

    for (int i=0; i<base_class->methods.count; ++i)
    {
      add( base_class->methods[i] );
    }
  }

  for (int iface=0; iface<interfaces.count; ++iface)
  {
    JogTypeInfo* base_interface = interfaces[iface];
    base_interface->prep();
    for (int i=0; i<base_interface->methods.count; ++i)
    {
      if ( base_interface->methods[i]->is_abstract() )
      {
        add( base_interface->methods[i] );
      }
    }
  }

  for (int i=0; i<original_class_methods.count; ++i)
  {
    add( original_class_methods[i] );
  }

  for (int i=0; i<original_methods.count; ++i)
  {
    add( original_methods[i] );
  }

  class_data_count = 0;
  data_count = 0;

  for (int i=0; i<class_properties.count; ++i)
  {
    class_properties[i]->type->organize();
    class_properties[i]->index = i;
    ++class_data_count;
  }

  for (int i=0; i<properties.count; ++i)
  {
    properties[i]->type->organize();
    properties[i]->index = i;
    ++data_count;
  }

  if (class_data_count)
  {
    class_data = new JogInt64[class_data_count];
    memset( class_data, 0, class_data_count*8 );
  }

  object_size = sizeof(JogObject) + (data_count - 1)*8;

  for (int i=0; i<class_methods.count; ++i)
  {
    class_methods[i]->organize();
      // Ensures types are defined, sets up class hierarchy, and creates method
      // signatures.
  }

  for (int i=0; i<methods.count; ++i)
  {
    if (methods[i]->is_abstract() && !this->is_abstract() && this->is_class())
    {
      StringBuilder buffer;
      buffer.print( "This class inherits one or more 'abstract' methods, including " );
      methods[i]->signature->print(buffer);
      buffer.print(".  You must either override all abstract methods or declare this class 'abstract'." );
      throw t->error( buffer.to_string() );
    }

    methods[i]->organize();
      // Ensures types are defined, sets up class hierarchy, and creates method
      // signatures.
  }

  // Store class property initialization values in first (currently empty)
  // static initializer block.
  for (int i=0; i<class_properties.count; ++i)
  {
    JogPropertyInfo* p = *(class_properties[i]);
    if ( *(p->initial_value) )
    {
      static_initializers[0]->statements->add(
          new JogCmdAssign(
            p->t,
            new JogCmdIdentifier(p->t,p->name),
            p->initial_value
          )
        );
    }
  }

  if (is_reference())
  {
    // Add init_object for initial property value assignments.
    JogMethodInfo* m = NULL;
    for (int i=0; i<properties.count; ++i)
    {
      JogPropertyInfo* p = *(properties[i]);
      if ( *(p->initial_value) )
      {
        if ( !m )
        {
          m = new JogMethodInfo( t, JOG_QUALIFIER_PUBLIC, 
                this, NULL, new JogString("init_object") );
        }

        m->statements->add(
            new JogCmdAssign(
              p->t,
              new JogCmdIdentifier(p->t,p->name),
              p->initial_value
            )
          );
        m->organize();
      }
    }
    if (m)
    {
      m_init_object = m;
      call_init_object = new JogCmdCallInitObject( m->t, m );
    }

    // Add a default constructor if necessary
    Ref<JogString> ctor_name = new JogString("<init>");
    if ( !methods_by_name.contains(ctor_name) )
    {
      methods_by_name[ctor_name] = new ArrayList<JogMethodInfo*>();

      Ref<JogMethodInfo> m = new JogMethodInfo( t, 
          JOG_QUALIFIER_PUBLIC|JOG_QUALIFIER_CONSTRUCTOR, 
          this, NULL, ctor_name );
      add(m);
    }
  }

  // Add properties to lookup tables.
  for (int i=0; i<class_properties.count; ++i)
  {
    JogPropertyInfo* p = *(class_properties[i]);
    if (class_properties_by_name.contains(p->name))
    {
      StringBuilder buffer;
      buffer.print( "A class property named \"" );
      p->name->print(buffer);
      buffer.print( "\" already exists in type \"" );
      name->print(buffer);
      buffer.print( "\"." );
      throw p->t->error( buffer.to_string() );
    }
    class_properties_by_name[p->name] = p;
  }

  for (int i=0; i<properties.count; ++i)
  {
    JogPropertyInfo* p = *(properties[i]);
    if (properties_by_name.contains(p->name))
    {
      StringBuilder buffer;
      buffer.print( "A property named \"" );
      p->name->print(buffer);
      buffer.print( "\" already exists in type \"" );
      name->print(buffer);
      buffer.print( "\"." );
      throw p->t->error( buffer.to_string() );
    }
    properties_by_name[p->name] = p;
  }

}

void JogTypeInfo::add( Ref<JogMethodInfo> m )
{
  m->organize();

  if (m->is_constructor() && m->is_inherited(this)) return;

  if (m->is_static())
  {
    if (class_methods_by_signature.contains(m->full_signature))
    {
      if (m->is_inherited(this)) return;

      JogMethodInfo* existing = class_methods_by_signature[m->full_signature];
      if ( !existing->is_inherited(this) )
      {
        StringBuilder buffer;
        buffer.print( "A class method with the signature \"" );
        m->signature->print(buffer);
        buffer.print( "\" already exists in type \"" );
        name->print(buffer);
        buffer.print( "\"." );
        throw m->t->error( buffer.to_string() );
      }
    }

    class_methods.add(m);
    class_methods_by_signature[m->signature] = *m;

    if ( !class_methods_by_name.contains(m->name) )
    {
      class_methods_by_name[m->name] = new ArrayList<JogMethodInfo*>();
    }
    class_methods_by_name[m->name]->add(*m);
  }
  else
  {
    dispatch_table.ensure_count(m->dispatch_id+1);

    JogMethodInfo* existing = dispatch_table[m->dispatch_id];
    if (existing)
    {
      if ( ((m->return_type!=NULL) ^ (existing->return_type!=NULL))
          || (m->return_type 
            && (m->return_type->is_primitive() ^ existing->return_type->is_primitive()))
          || (m->return_type && !m->return_type->instance_of(existing->return_type)) )
      {
        throw m->t->error( "Return type incompatible with inherited method." );
      }

      if (m->is_abstract()) return;

      if ( !existing->is_inherited(this) )
      {
        throw m->t->error( "A method with this signature already exists." );
      }

      methods.replace(existing,m);
    }
    else
    {
      methods.add(m);
    }

    dispatch_table[m->dispatch_id] = *m;

    methods_by_signature[m->signature] = *m;

    if ( !methods_by_name.contains(m->name) )
    {
      methods_by_name[m->name] = new ArrayList<JogMethodInfo*>();
    }

    if (existing)
    {
      ArrayList<JogMethodInfo*> &list = *methods_by_name[m->name];
      for (int i=0; i<list.count; ++i)
      {
        if (list[i] == existing)
        {
          list[i] = *m;
          break;
        }
      }
    }
    else
    {
      methods_by_name[m->name]->add(*m);
    }
  }
}


void JogTypeInfo::resolve()
{
  if (resolved) return;
  resolved = true;

  prep();

  if (element_type) element_type->resolve();

  for (int i=0; i<class_methods.count; ++i)
  {
    class_methods[i]->resolve();
  }

  for (int i=0; i<methods.count; ++i)
  {
    methods[i]->resolve();
  }

  for (int i=0; i<static_initializers.count; ++i)
  {
    static_initializers[i]->resolve();
  }

  if (*m_init_object) 
  {
    m_init_object->resolve();
  }
}

void JogMethodInfo::resolve()
{
  if (resolved) return;
  resolved = true;

  organize();

  int ref_offset;
  int data_offset = -1;

  param_data_count = 0;
  if (is_static()) 
  {
    param_ref_count = 0;
    ref_offset = -1;
  }
  else 
  {
    param_ref_count = 1;  // starts at one with object context
    ref_offset = -2;
  }

  for (int i=0; i<parameters.count; ++i)
  {
    parameters[i]->type->resolve();
    if (parameters[i]->type->is_reference())
    {
      ++param_ref_count;
      parameters[i]->offset = ref_offset--;
    }
    else
    {
      ++param_data_count;
      parameters[i]->offset = data_offset--;
    }
  }

  JogContext context(this);
  statements->resolve();

  local_data_count = 0;
  local_ref_count = 0;
  for (int i=parameters.count; i<locals.count; ++i)
  {
    if (locals[i]->type->is_reference())
    {
      ++local_ref_count;
      locals[i]->offset = ref_offset--;
    }
    else
    {
      ++local_data_count;
      locals[i]->offset = data_offset--;
    }
  }

  if (is_constructor() && !calls_super_constructor && type_context->base_class)
  {
    type_context->base_class->resolve();
    JogMethodInfo* m = statements->resolve_call( t, type_context->base_class,
        new JogString("<init>"), new JogCmdList(t) );
    statements->commands.insert( new JogCmdStaticCall( t, m, new JogCmdThis(t,type_context), NULL ) );
  }
}



Ref<JogCmd> JogCmd::resolve() { return this; }

static void print_candidates( Ref<JogToken> t, 
    JogTypeInfo* context_type, Ref<JogString> name, 
    RefList<JogCmd> &args, ArrayList<JogMethodInfo*>& methods )
{
  bool is_ctor = (name->equals("<init>"));

  UnicodeStringBuilder buffer16;
  if (methods.count)
  {
    buffer16.print("No exact match for call to ");
    if (is_ctor) 
    {
      buffer16.print( "constructor " );
      context_type->name->print(buffer16);
    }
    else 
    {
      name->print(buffer16);
    }
  }
  else
  {
    if (is_ctor) 
    {
      buffer16.print("No such constructor ");
      context_type->name->print(buffer16);
    }
    else
    {
      buffer16.print("No such method " );
      name->print(buffer16);
    }
  }

  buffer16.print("(");
  for (int i=0; i<args.count; ++i)
  {
    if (i > 0) buffer16.print(',');
    args[i]->type()->name->print(buffer16);
  }
  buffer16.print(") in type '");
  context_type->name->print(buffer16);
  buffer16.print("'.");


  if (methods.count)
  {
    buffer16.print("  Candidates:");
    buffer16.print("\n");

    for (int i=0; i<methods.count; ++i)
    {
      buffer16.print("\n           ");
      methods[i]->type_context->name->print(buffer16);
      if (is_ctor)
      {
        methods[i]->signature->substring(6)->print(buffer16);
      }
      else
      {
        buffer16.print("::");
        methods[i]->signature->print(buffer16);
      }
    }
  }

  StringBuilder buffer8;
  buffer16.print(buffer8);
  throw t->error( buffer8.to_string() );
}


JogMethodInfo* JogCmd::resolve_call( Ref<JogToken> t, 
    JogTypeInfo* context_type, Ref<JogString> name, Ref<JogCmd> _args,
    bool allow_object_methods )
{
  Ref<JogCmdList> args = (JogCmdList*) *_args;

  args->resolve();

  int args_count = args->commands.count;
  for (int i=0; i<args_count; ++i)
  {
    args->commands[i]->require_value();
  }

  ArrayList<JogMethodInfo*> candidates;
  ArrayList<JogMethodInfo*> matches;

  // Add all methods with a matching name.
  if (context_type->class_methods_by_name.contains(name))
  {
    ArrayList<JogMethodInfo*>& class_methods = *(context_type->class_methods_by_name[name]);
    for (int i=0; i<class_methods.count; ++i) candidates.add(class_methods[i]);
  }

  if (allow_object_methods && context_type->methods_by_name.contains(name))
  {
    ArrayList<JogMethodInfo*>& methods = *(context_type->methods_by_name[name]);
    for (int i=0; i<methods.count; ++i) candidates.add(methods[i]);
  }

  // Remove methods with insufficient number of parameters.
  for (int i=0; i<candidates.count; ++i)
  {
    JogMethodInfo* m = candidates[i];
    if (m->parameters.count == args_count) matches.add(candidates[i]);
  }

  if (matches.count == 0) 
  {
    print_candidates( t, context_type, name, args->commands, candidates );
  }
  else
  {
    candidates.clear();
    candidates.add( matches );
    matches.clear();
  }

  // Remove methods with incompatible types.
  bool have_perfect_match = false;
  for (int i=0; i<candidates.count; ++i)
  {
    JogMethodInfo* m = candidates[i];
    bool compatible_match = true;
    bool perfect_match = true;
    for (int p=0; p<m->parameters.count; ++p)
    {
      JogTypeInfo* type_a = args->commands[p]->type();
      JogTypeInfo* type_b = m->parameters[p]->type;
      if (type_a != type_b) 
      {
        perfect_match = false;
        if ( !type_a->is_compatible_with(type_b) )
        {
          compatible_match = false;
          break;
        }
      }
    }
    if (perfect_match)
    {
      if ( !have_perfect_match )
      {
        matches.clear();
        have_perfect_match = true;
      }
      matches.add(candidates[i]);
    }
    else if (compatible_match && !have_perfect_match)
    {
      matches.add(candidates[i]);
    }
  }

  if (matches.count == 0) 
  {
    print_candidates( t, context_type, name, args->commands, candidates );
  }
  else
  {
    candidates.clear();
    candidates.add( matches );
    matches.clear();
  }

  // Given multiple compatible methods, remove any method
  // where another method is fully compatible with it.
  // (e.g. remove abs(double) in favor of abs(int)).
  if (candidates.count > 1)
  {
    for (int i=0; i<candidates.count; ++i)
    {
      bool discard = false;
      for (int j=0; j<candidates.count; ++j)
      {
        if (i == j) continue;

        JogMethodInfo* m1 = candidates[i];
        JogMethodInfo* m2 = candidates[j];

        if (m1->is_less_specific_than(m2))
        {
          discard = true;
          break;
        }
      }
      if (!discard) matches.add(candidates[i]);
    }

    if (matches.count == 0) 
    {
      print_candidates( t, context_type, name, args->commands, candidates );
    }
    else
    {
      candidates.clear();
      candidates.add( matches );
      matches.clear();
    }
  }

  // Remove class methods in favor of object methods.
  bool have_object_methods = false;
  bool have_class_methods = false;
  for (int i=0; i<candidates.count; ++i)
  {
    if (candidates[i]->is_static()) have_class_methods = true;
    else have_object_methods = true;
  }

  if (have_class_methods && have_object_methods)
  {
    for (int i=0; i<candidates.count; ++i)
    {
      JogMethodInfo* m = candidates[i];
      if (m->parameters.count == args_count)
      {
        if ( !m->is_static() ) matches.add(candidates[i]);
      }
    }

    if (matches.count == 0) 
    {
      print_candidates( t, context_type, name, args->commands, candidates );
    }
    else
    {
      candidates.clear();
      candidates.add( matches );
      matches.clear();
    }
  }

  // Prefer primitives over primitive wrappers.
  /*
  if (candidates.count > 1)
  {
    for (int i=0; i<candidates.count; ++i)
    {
      bool discard = false;
      for (int j=0; j<candidates.count; ++j)
      {
        if (i == j) continue;

        JogMethodInfo* m1 = candidates[i];
        JogMethodInfo* m2 = candidates[j];

        if (m1->uses_wrappers_more_than(m2))
        {
          discard = true;
          break;
        }
      }
      if (!discard) matches.add(candidates[i]);
    }

    if (matches.count == 0) 
    {
      print_candidates( t, context_type, name, args->commands, candidates );
    }
    else
    {
      candidates.clear();
      candidates.add( matches );
      matches.clear();
    }
  }
  */

  // We're left with the best match.
  if (candidates.count != 1) 
  {
    print_candidates( t, context_type, name, args->commands, candidates );
  }


  JogMethodInfo* match = candidates[0];

  // Cast args to match parameter types as necessary.
  for (int i=0; i<args->count(); ++i)
  {
    args->commands[i] = args->commands[i]->cast_to_type(match->parameters[i]->type)->resolve();
  }

  return match;
}

Ref<JogCmd> JogCmdList::resolve()
{
  for (int i=0; i<commands.count; ++i)
  {
    commands[i] = commands[i]->resolve();
  }
  return this;
}

Ref<JogCmd> JogStatementList::resolve()
{
  for (int i=0; i<commands.count; ++i)
  {
    commands[i] = commands[i]->resolve()->discarding_result();
  }
  return this;
}

void JogCmdBinary::validate()
{
  JogTypeInfo* common_type = jog_type_manager.find_common_type( *t, *lhs, *rhs, true );
  this->lhs = lhs->cast_to_type( common_type )->resolve();
  this->rhs = rhs->cast_to_type( common_type )->resolve();
}

Ref<JogCmd> JogCmdCast::resolve()
{
  operand = operand->resolve();

  JogTypeInfo* from_type = operand->type();
  if (from_type == to_type) return operand;

  if (from_type->is_boolean() || to_type->is_boolean())
  {
    throw t->error( "Cannot cast to or from type boolean." );
  }

  if (from_type->is_primitive() ^ to_type->is_primitive())
  {
    throw t->error( "Cannot between primitive and reference types." );
  }

  if (from_type->is_primitive())
  {
    if (operand->is_literal()) return operand->cast_to_type(to_type);

    if (to_type == jog_type_manager.type_real64)
    {
      if (from_type == jog_type_manager.type_real32)
      {
        return new JogCmdCastReal32ToReal64( t, operand );
      }
      else
      {
        return new JogCmdCastIntegerToReal64( t, operand );
      }
    }
    else if (to_type == jog_type_manager.type_real32)
    {
      if (from_type == jog_type_manager.type_real64)
      {
        return new JogCmdCastReal64ToReal32( t, operand );
      }
      else
      {
        return new JogCmdCastIntegerToReal32( t, operand );
      }
    }
    else if (to_type == jog_type_manager.type_int64)
    {
      if (from_type->is_real())
      {
        return new JogCmdCastRealToInt64( t, operand );
      }
      else
      {
        return new JogCmdCastIntegerToInt64( t, operand );
      }
    }
    else if (to_type == jog_type_manager.type_int32)
    {
      if (from_type->is_real())
      {
        return new JogCmdCastRealToInt32( t, operand );
      }
      else
      {
        return new JogCmdCastIntegerToInt32( t, operand );
      }
    }
    else if (to_type == jog_type_manager.type_int16)
    {
      if (from_type->is_real())
      {
        return new JogCmdCastIntegerToInt16(t,new JogCmdCastRealToInt64(t,operand));
      }
      else
      {
        return new JogCmdCastIntegerToInt16(t,operand);
      }
    }
    else if (to_type == jog_type_manager.type_int8)
    {
      if (from_type->is_real())
      {
        return new JogCmdCastIntegerToInt8(t,new JogCmdCastRealToInt64(t,operand));
      }
      else
      {
        return new JogCmdCastIntegerToInt8(t,operand);
      }
    }
    else
    {
      // must be char
      if (from_type->is_real())
      {
        return new JogCmdCastIntegerToChar(t,new JogCmdCastRealToInt64(t,operand));
      }
      else
      {
        return new JogCmdCastIntegerToChar(t,operand);
      }
    }
  }

  if (to_type == jog_type_manager.type_object || from_type->instance_of(to_type))
  {
    return new JogCmdWideningCast( t, operand, to_type );
  }
  else
  {
    return new JogCmdNarrowingCast( t, operand, to_type );
  }
}

Ref<JogCmd> JogCmdAssign::resolve()
{
  return location->resolve_assignment( NULL, new_value->resolve() );
}

Ref<JogCmd> JogCmdAddAssign::resolve() 
{ 
  return location->resolve_op_assign( TOKEN_ADD_ASSIGN, NULL, rhs ); 
}

Ref<JogCmd> JogCmdSubAssign::resolve()
{
  return location->resolve_op_assign( TOKEN_SUB_ASSIGN, NULL, rhs );
}

Ref<JogCmd> JogCmdMulAssign::resolve()
{ 
  return location->resolve_op_assign( TOKEN_MUL_ASSIGN, NULL, rhs ); 
}

Ref<JogCmd> JogCmdDivAssign::resolve()
{ 
  return location->resolve_op_assign( TOKEN_DIV_ASSIGN, NULL, rhs ); 
}

Ref<JogCmd> JogCmdModAssign::resolve()
{ 
  return location->resolve_op_assign( TOKEN_MOD_ASSIGN, NULL, rhs ); 
}

Ref<JogCmd> JogCmdAndAssign::resolve()
{ 
  return location->resolve_op_assign( TOKEN_AND_ASSIGN, NULL, rhs ); 
}

Ref<JogCmd> JogCmdOrAssign::resolve()
{ 
  return location->resolve_op_assign( TOKEN_OR_ASSIGN, NULL, rhs ); 
}

Ref<JogCmd> JogCmdXorAssign::resolve()
{ 
  return location->resolve_op_assign( TOKEN_XOR_ASSIGN, NULL, rhs ); 
}

Ref<JogCmd> JogCmdLeftShiftAssign::resolve()
{ 
  return location->resolve_op_assign( TOKEN_SHL_ASSIGN, NULL, rhs ); 
}

Ref<JogCmd> JogCmdRightXShiftAssign::resolve()
{ 
  return location->resolve_op_assign( TOKEN_SHRX_ASSIGN, NULL, rhs ); 
}

Ref<JogCmd> JogCmdRightShiftAssign::resolve()
{ 
  return location->resolve_op_assign( TOKEN_SHR_ASSIGN, NULL, rhs ); 
}

Ref<JogCmd> JogCmdConditional::resolve()
{ 
  condition = condition->resolve();
  true_value = true_value->resolve();
  false_value = false_value->resolve();

  condition->require_boolean();
  JogTypeInfo* common_type = jog_type_manager.find_common_type( *t, *true_value, *false_value );
  /*
  if (common_type->is_reference())
  {
    true_value = true_value->box(common_type);
    false_value = false_value->box(common_type);
  }
  else
  */
  {
    true_value = true_value->cast_to_type(common_type)->resolve();
    false_value = false_value->cast_to_type(common_type)->resolve();
  }

  return this;
}

Ref<JogCmd> JogCmdLogicalOr::resolve()
{
  lhs = lhs->resolve();
  rhs = rhs->resolve();

  lhs->require_boolean();
  rhs->require_boolean();

  if (lhs->is_literal() && rhs->is_literal())
  {
    JogTypeInfo* op_type = lhs->type();
    if (op_type == jog_type_manager.type_boolean)
    {
      bool a = ((JogCmdLiteralBoolean*) *lhs)->value;
      bool b = ((JogCmdLiteralBoolean*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a || b );
    }
  }
  return this;
}

Ref<JogCmd> JogCmdLogicalAnd::resolve()
{
  lhs = lhs->resolve();
  rhs = rhs->resolve();

  lhs->require_boolean();
  rhs->require_boolean();

  if (lhs->is_literal() && rhs->is_literal())
  {
    JogTypeInfo* op_type = lhs->type();
    if (op_type == jog_type_manager.type_boolean)
    {
      bool a = ((JogCmdLiteralBoolean*) *lhs)->value;
      bool b = ((JogCmdLiteralBoolean*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a && b );
    }
  }
  return this;
}

Ref<JogCmd> JogCmdBitwiseOr::resolve()
{
  lhs = lhs->resolve();
  rhs = rhs->resolve();

  JogCmdBinary::validate();
  JogTypeInfo* op_type = lhs->require_integer_or_boolean();

  if (lhs->is_literal() && rhs->is_literal())
  {
    if (op_type == jog_type_manager.type_int64)
    {
      JogInt64 a = ((JogCmdLiteralInt64*) *lhs)->value;
      JogInt64 b = ((JogCmdLiteralInt64*) *rhs)->value;
      return new JogCmdLiteralInt64( t, a | b );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      JogInt32 a = ((JogCmdLiteralInt32*) *lhs)->value;
      JogInt32 b = ((JogCmdLiteralInt32*) *rhs)->value;
      return new JogCmdLiteralInt32( t, a | b );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      JogInt16 a = ((JogCmdLiteralInt16*) *lhs)->value;
      JogInt16 b = ((JogCmdLiteralInt16*) *rhs)->value;
      return new JogCmdLiteralInt16( t, a | b );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      JogInt8 a = ((JogCmdLiteralInt8*) *lhs)->value;
      JogInt8 b = ((JogCmdLiteralInt8*) *rhs)->value;
      return new JogCmdLiteralInt8( t, a | b );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      JogChar a = ((JogCmdLiteralChar*) *lhs)->value;
      JogChar b = ((JogCmdLiteralChar*) *rhs)->value;
      return new JogCmdLiteralChar( t, a | b );
    }
    else if (op_type == jog_type_manager.type_boolean)
    {
      bool a = ((JogCmdLiteralBoolean*) *lhs)->value;
      bool b = ((JogCmdLiteralBoolean*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a | b );
    }
  }

  if (op_type == jog_type_manager.type_int64)
  {
    return new JogCmdOrInt64( t, lhs, rhs );
  }
  else
  {
    return new JogCmdOrInt32( t, lhs, rhs );
  }
}

Ref<JogCmd> JogCmdBitwiseXor::resolve()
{
  lhs = lhs->resolve();
  rhs = rhs->resolve();

  JogCmdBinary::validate();
  JogTypeInfo* op_type = lhs->require_integer_or_boolean();

  if (lhs->is_literal() && rhs->is_literal())
  {
    if (op_type == jog_type_manager.type_int64)
    {
      JogInt64 a = ((JogCmdLiteralInt64*) *lhs)->value;
      JogInt64 b = ((JogCmdLiteralInt64*) *rhs)->value;
      return new JogCmdLiteralInt64( t, a ^ b );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      JogInt32 a = ((JogCmdLiteralInt32*) *lhs)->value;
      JogInt32 b = ((JogCmdLiteralInt32*) *rhs)->value;
      return new JogCmdLiteralInt32( t, a ^ b );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      JogInt16 a = ((JogCmdLiteralInt16*) *lhs)->value;
      JogInt16 b = ((JogCmdLiteralInt16*) *rhs)->value;
      return new JogCmdLiteralInt16( t, a ^ b );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      JogInt8 a = ((JogCmdLiteralInt8*) *lhs)->value;
      JogInt8 b = ((JogCmdLiteralInt8*) *rhs)->value;
      return new JogCmdLiteralInt8( t, a ^ b );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      JogChar a = ((JogCmdLiteralChar*) *lhs)->value;
      JogChar b = ((JogCmdLiteralChar*) *rhs)->value;
      return new JogCmdLiteralChar( t, a ^ b );
    }
    else if (op_type == jog_type_manager.type_boolean)
    {
      bool a = ((JogCmdLiteralBoolean*) *lhs)->value;
      bool b = ((JogCmdLiteralBoolean*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a ^ b );
    }
  }

  if (op_type == jog_type_manager.type_int64)
  {
    return new JogCmdXorInt64( t, lhs, rhs );
  }
  else
  {
    return new JogCmdXorInt32( t, lhs, rhs );
  }
}

Ref<JogCmd> JogCmdBitwiseAnd::resolve()
{
  lhs = lhs->resolve();
  rhs = rhs->resolve();

  JogCmdBinary::validate();
  JogTypeInfo* op_type = lhs->require_integer_or_boolean();

  if (lhs->is_literal() && rhs->is_literal())
  {
    if (op_type == jog_type_manager.type_int64)
    {
      JogInt64 a = ((JogCmdLiteralInt64*) *lhs)->value;
      JogInt64 b = ((JogCmdLiteralInt64*) *rhs)->value;
      return new JogCmdLiteralInt64( t, a & b );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      JogInt32 a = ((JogCmdLiteralInt32*) *lhs)->value;
      JogInt32 b = ((JogCmdLiteralInt32*) *rhs)->value;
      return new JogCmdLiteralInt32( t, a & b );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      JogInt16 a = ((JogCmdLiteralInt16*) *lhs)->value;
      JogInt16 b = ((JogCmdLiteralInt16*) *rhs)->value;
      return new JogCmdLiteralInt16( t, a & b );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      JogInt8 a = ((JogCmdLiteralInt8*) *lhs)->value;
      JogInt8 b = ((JogCmdLiteralInt8*) *rhs)->value;
      return new JogCmdLiteralInt8( t, a & b );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      JogChar a = ((JogCmdLiteralChar*) *lhs)->value;
      JogChar b = ((JogCmdLiteralChar*) *rhs)->value;
      return new JogCmdLiteralChar( t, a & b );
    }
    else if (op_type == jog_type_manager.type_boolean)
    {
      bool a = ((JogCmdLiteralBoolean*) *lhs)->value;
      bool b = ((JogCmdLiteralBoolean*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a & b );
    }
  }

  if (op_type == jog_type_manager.type_int64)
  {
    return new JogCmdAndInt64( t, lhs, rhs );
  }
  else
  {
    return new JogCmdAndInt32( t, lhs, rhs );
  }
}

Ref<JogCmd> JogCmdAdd::resolve()
{
  lhs = lhs->resolve();
  rhs = rhs->resolve();

  JogTypeInfo* lhs_type = lhs->require_value();
  JogTypeInfo* rhs_type = rhs->require_value();

  if (lhs_type->is_reference())
  {
    if (lhs_type->instance_of(jog_type_manager.type_string))
    {
      if (rhs_type->is_reference())
      {
        if (rhs_type == jog_type_manager.type_null)
        {
          rhs = new JogCmdLiteralString( t, new JogString("null") );
        }
        else if ( !rhs_type->instance_of(jog_type_manager.type_string) )
        {
          rhs = new JogCmdMemberAccess( t,
              rhs,
              new JogCmdMethodCall( t, new JogString("toString"), new JogCmdList(t) )
              );
        }
        Ref<JogCmdList> args = new JogCmdList(t);
        args->add(rhs);
        return (new JogCmdMemberAccess( t, 
              lhs,
              new JogCmdMethodCall( t, new JogString("concat"), args )
              ))->resolve();
      }
      else
      {
        Ref<JogCmdList> args = new JogCmdList(t);
        args->add(rhs);
        JogMethodInfo* m = resolve_call( t, rhs_type->wrapper_type(), 
            new JogString("toString"), *args, false );
        rhs = new JogCmdClassCall( t, m, NULL, args );
        return resolve();
      }
    }
    throw error( "Invalid operands to '+'." );
  }
  else if (rhs_type->is_reference() && rhs_type->instance_of(jog_type_manager.type_string))
  {
    // We know the LHS is a primitive
    Ref<JogCmdList> args = new JogCmdList(t);
    args->add(lhs);
    JogMethodInfo* m = resolve_call( t, lhs_type->wrapper_type(), 
        new JogString("toString"), *args, false );
    lhs = new JogCmdClassCall( t, m, NULL, args );
    return resolve();
  }
  else
  {
    JogCmdBinary::validate();
  }

  JogTypeInfo* op_type = lhs->type();

  if (lhs->is_literal() && rhs->is_literal())
  {
    if (op_type == jog_type_manager.type_real64)
    {
      double a = ((JogCmdLiteralReal64*) *lhs)->value;
      double b = ((JogCmdLiteralReal64*) *rhs)->value;
      return new JogCmdLiteralReal64( t, a + b );
    }
    else if (op_type == jog_type_manager.type_real32)
    {
      float a = ((JogCmdLiteralReal32*) *lhs)->value;
      float b = ((JogCmdLiteralReal32*) *rhs)->value;
      return new JogCmdLiteralReal32( t, a + b );
    }
    else if (op_type == jog_type_manager.type_int64)
    {
      JogInt64 a = ((JogCmdLiteralInt64*) *lhs)->value;
      JogInt64 b = ((JogCmdLiteralInt64*) *rhs)->value;
      return new JogCmdLiteralInt64( t, a + b );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      JogInt32 a = ((JogCmdLiteralInt32*) *lhs)->value;
      JogInt32 b = ((JogCmdLiteralInt32*) *rhs)->value;
      return new JogCmdLiteralInt32( t, a + b );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      JogInt16 a = ((JogCmdLiteralInt16*) *lhs)->value;
      JogInt16 b = ((JogCmdLiteralInt16*) *rhs)->value;
      return new JogCmdLiteralInt16( t, a + b );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      JogInt8 a = ((JogCmdLiteralInt8*) *lhs)->value;
      JogInt8 b = ((JogCmdLiteralInt8*) *rhs)->value;
      return new JogCmdLiteralInt8( t, a + b );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      JogChar a = ((JogCmdLiteralChar*) *lhs)->value;
      JogChar b = ((JogCmdLiteralChar*) *rhs)->value;
      return new JogCmdLiteralChar( t, a + b );
    }
    else
    {
      throw error( "Boolean values cannot be added." );
    }
  }

  if (op_type->is_primitive())
  {
    if (op_type->is_boolean())
    {
      throw error( "Boolean values cannot be added." );
    }
    else if (op_type == jog_type_manager.type_real64)
    {
      return new JogCmdAddReal64( t, lhs, rhs );
    }
    else if (op_type == jog_type_manager.type_real32)
    {
      return new JogCmdAddReal32( t, lhs, rhs );
    }
    else if (op_type == jog_type_manager.type_int64)
    {
      return new JogCmdAddInt64( t, lhs, rhs );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      return new JogCmdAddInt32( t, lhs, rhs );
    }
    else
    {
      // Operands are short, byte, or char - consider them type int.
      return new JogCmdAddInt32( t, lhs, rhs );
    }
  }

  op_type->print();
  throw error("TODO: finish Add");
}

Ref<JogCmd> JogCmdSub::resolve()
{
  lhs = lhs->resolve();
  rhs = rhs->resolve();

  JogCmdBinary::validate();

  JogTypeInfo* op_type = lhs->type();
  if (lhs->is_literal() && rhs->is_literal())
  {
    if (op_type == jog_type_manager.type_real64)
    {
      double a = ((JogCmdLiteralReal64*) *lhs)->value;
      double b = ((JogCmdLiteralReal64*) *rhs)->value;
      return new JogCmdLiteralReal64( t, a - b );
    }
    else if (op_type == jog_type_manager.type_real32)
    {
      float a = ((JogCmdLiteralReal32*) *lhs)->value;
      float b = ((JogCmdLiteralReal32*) *rhs)->value;
      return new JogCmdLiteralReal32( t, a - b );
    }
    else if (op_type == jog_type_manager.type_int64)
    {
      JogInt64 a = ((JogCmdLiteralInt64*) *lhs)->value;
      JogInt64 b = ((JogCmdLiteralInt64*) *rhs)->value;
      return new JogCmdLiteralInt64( t, a - b );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      int a = ((JogCmdLiteralInt32*) *lhs)->value;
      int b = ((JogCmdLiteralInt32*) *rhs)->value;
      return new JogCmdLiteralInt32( t, a - b );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      JogInt16 a = ((JogCmdLiteralInt16*) *lhs)->value;
      JogInt16 b = ((JogCmdLiteralInt16*) *rhs)->value;
      return new JogCmdLiteralInt16( t, a - b );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      JogInt8 a = ((JogCmdLiteralInt8*) *lhs)->value;
      JogInt8 b = ((JogCmdLiteralInt8*) *rhs)->value;
      return new JogCmdLiteralInt8( t, a - b );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      JogChar a = ((JogCmdLiteralChar*) *lhs)->value;
      JogChar b = ((JogCmdLiteralChar*) *rhs)->value;
      return new JogCmdLiteralChar( t, a - b );
    }
    else
    {
      throw error( "Boolean values cannot be subtracted." );
    }
  }

  if (op_type->is_primitive())
  {
    if (op_type->is_boolean())
    {
      throw error( "Boolean values cannot be subtracted." );
    }
    else if (op_type == jog_type_manager.type_real64)
    {
      return new JogCmdSubReal64( t, lhs, rhs );
    }
    else if (op_type == jog_type_manager.type_real32)
    {
      return new JogCmdSubReal32( t, lhs, rhs );
    }
    else if (op_type == jog_type_manager.type_int64)
    {
      return new JogCmdSubInt64( t, lhs, rhs );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      return new JogCmdSubInt32( t, lhs, rhs );
    }
    else
    {
      // Operands are short, byte, or char - consider them type int.
      return new JogCmdSubInt32( t, lhs, rhs );
    }
  }

  op_type->print();
  throw error("TODO: finish Sub");
}

Ref<JogCmd> JogCmdMul::resolve()
{
  lhs = lhs->resolve();
  rhs = rhs->resolve();

  JogCmdBinary::validate();

  JogTypeInfo* op_type = lhs->type();
  if (lhs->is_literal() && rhs->is_literal())
  {
    if (op_type == jog_type_manager.type_real64)
    {
      double a = ((JogCmdLiteralReal64*) *lhs)->value;
      double b = ((JogCmdLiteralReal64*) *rhs)->value;
      return new JogCmdLiteralReal64( t, a * b );
    }
    else if (op_type == jog_type_manager.type_real32)
    {
      float a = ((JogCmdLiteralReal32*) *lhs)->value;
      float b = ((JogCmdLiteralReal32*) *rhs)->value;
      return new JogCmdLiteralReal32( t, a * b );
    }
    else if (op_type == jog_type_manager.type_int64)
    {
      JogInt64 a = ((JogCmdLiteralInt64*) *lhs)->value;
      JogInt64 b = ((JogCmdLiteralInt64*) *rhs)->value;
      return new JogCmdLiteralInt64( t, a * b );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      int a = ((JogCmdLiteralInt32*) *lhs)->value;
      int b = ((JogCmdLiteralInt32*) *rhs)->value;
      return new JogCmdLiteralInt32( t, a * b );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      JogInt16 a = ((JogCmdLiteralInt16*) *lhs)->value;
      JogInt16 b = ((JogCmdLiteralInt16*) *rhs)->value;
      return new JogCmdLiteralInt16( t, a * b );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      JogInt8 a = ((JogCmdLiteralInt8*) *lhs)->value;
      JogInt8 b = ((JogCmdLiteralInt8*) *rhs)->value;
      return new JogCmdLiteralInt8( t, a * b );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      JogChar a = ((JogCmdLiteralChar*) *lhs)->value;
      JogChar b = ((JogCmdLiteralChar*) *rhs)->value;
      return new JogCmdLiteralChar( t, a * b );
    }
    else
    {
      throw error( "Boolean values cannot be multiplied." );
    }
  }

  if (op_type->is_primitive())
  {
    if (op_type->is_boolean())
    {
      throw error( "Boolean values cannot be multiplied." );
    }
    else if (op_type == jog_type_manager.type_real64)
    {
      return new JogCmdMulReal64( t, lhs, rhs );
    }
    else if (op_type == jog_type_manager.type_real32)
    {
      return new JogCmdMulReal32( t, lhs, rhs );
    }
    else if (op_type == jog_type_manager.type_int64)
    {
      return new JogCmdMulInt64( t, lhs, rhs );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      return new JogCmdMulInt32( t, lhs, rhs );
    }
    else
    {
      // Operands are short, byte, or char - consider them type int.
      return new JogCmdMulInt32( t, lhs, rhs );
    }
  }

  op_type->print();
  throw error("TODO: finish Mul");
}

Ref<JogCmd> JogCmdDiv::resolve()
{
  lhs = lhs->resolve();
  rhs = rhs->resolve();

  JogCmdBinary::validate();

  JogTypeInfo* op_type = lhs->type();
  if (lhs->is_literal() && rhs->is_literal())
  {
    if (op_type == jog_type_manager.type_real64)
    {
      double a = ((JogCmdLiteralReal64*) *lhs)->value;
      double b = ((JogCmdLiteralReal64*) *rhs)->value;
      return new JogCmdLiteralReal64( t, a / b );
    }
    else if (op_type == jog_type_manager.type_real32)
    {
      float a = ((JogCmdLiteralReal32*) *lhs)->value;
      float b = ((JogCmdLiteralReal32*) *rhs)->value;
      return new JogCmdLiteralReal32( t, a / b );
    }
    else if (op_type == jog_type_manager.type_int64)
    {
      JogInt64 a = ((JogCmdLiteralInt64*) *lhs)->value;
      JogInt64 b = ((JogCmdLiteralInt64*) *rhs)->value;
      if (b == 0) throw error( "Illegal division by zero." );
      return new JogCmdLiteralInt64( t, a / b );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      int a = ((JogCmdLiteralInt32*) *lhs)->value;
      int b = ((JogCmdLiteralInt32*) *rhs)->value;
      if (b == 0) throw error( "Illegal division by zero." );
      return new JogCmdLiteralInt32( t, a / b );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      JogInt16 a = ((JogCmdLiteralInt16*) *lhs)->value;
      JogInt16 b = ((JogCmdLiteralInt16*) *rhs)->value;
      if (b == 0) throw error( "Illegal division by zero." );
      return new JogCmdLiteralInt16( t, a / b );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      JogInt8 a = ((JogCmdLiteralInt8*) *lhs)->value;
      JogInt8 b = ((JogCmdLiteralInt8*) *rhs)->value;
      if (b == 0) throw error( "Illegal division by zero." );
      return new JogCmdLiteralInt8( t, a / b );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      JogChar a = ((JogCmdLiteralChar*) *lhs)->value;
      JogChar b = ((JogCmdLiteralChar*) *rhs)->value;
      if (b == 0) throw error( "Illegal division by zero." );
      return new JogCmdLiteralChar( t, a / b );
    }
    else
    {
      throw error( "Boolean values cannot be divided." );
    }
  }

  if (op_type->is_primitive())
  {
    if (op_type->is_boolean())
    {
      throw error( "Boolean values cannot be divided." );
    }
    else if (op_type == jog_type_manager.type_real64)
    {
      return new JogCmdDivReal64( t, lhs, rhs );
    }
    else if (op_type == jog_type_manager.type_real32)
    {
      return new JogCmdDivReal32( t, lhs, rhs );
    }
    else if (op_type == jog_type_manager.type_int64)
    {
      return new JogCmdDivInt64( t, lhs, rhs );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      return new JogCmdDivInt32( t, lhs, rhs );
    }
    else
    {
      // Operands are short, byte, or char - consider them type int.
      return new JogCmdDivInt32( t, lhs, rhs );
    }
  }

  op_type->print();
  throw error("TODO: finish Div");
}

Ref<JogCmd> JogCmdMod::resolve()
{
  lhs = lhs->resolve();
  rhs = rhs->resolve();

  JogCmdBinary::validate();

  JogTypeInfo* op_type = lhs->type();
  if (lhs->is_literal() && rhs->is_literal())
  {
    if (op_type == jog_type_manager.type_int64)
    {
      JogInt64 a = ((JogCmdLiteralInt64*) *lhs)->value;
      JogInt64 b = ((JogCmdLiteralInt64*) *rhs)->value;
      if (b == 0) throw error( "Illegal division by zero in modulo operation." );
      return new JogCmdLiteralInt64( t, a % b );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      int a = ((JogCmdLiteralInt32*) *lhs)->value;
      int b = ((JogCmdLiteralInt32*) *rhs)->value;
      if (b == 0) throw error( "Illegal division by zero in modulo operation." );
      return new JogCmdLiteralInt32( t, a % b );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      JogInt16 a = ((JogCmdLiteralInt16*) *lhs)->value;
      JogInt16 b = ((JogCmdLiteralInt16*) *rhs)->value;
      if (b == 0) throw error( "Illegal division by zero in modulo operation." );
      return new JogCmdLiteralInt16( t, a % b );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      JogInt8 a = ((JogCmdLiteralInt8*) *lhs)->value;
      JogInt8 b = ((JogCmdLiteralInt8*) *rhs)->value;
      if (b == 0) throw error( "Illegal division by zero in modulo operation." );
      return new JogCmdLiteralInt8( t, a % b );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      JogChar a = ((JogCmdLiteralChar*) *lhs)->value;
      JogChar b = ((JogCmdLiteralChar*) *rhs)->value;
      if (b == 0) throw error( "Illegal division by zero in modulo operation." );
      return new JogCmdLiteralChar( t, a % b );
    }
    else
    {
      throw error( "Only integer types support modulo." );
    }
  }

  if (op_type->is_primitive())
  {
    if (op_type->is_real() || op_type->is_boolean())
    {
      throw error( "Modulo can only be used on integer values." );
    }
    else if (op_type == jog_type_manager.type_int64)
    {
      return new JogCmdModInt64( t, lhs, rhs );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      return new JogCmdModInt32( t, lhs, rhs );
    }
    else
    {
      // Operands are short, byte, or char - consider them type int.
      return new JogCmdModInt32( t, lhs, rhs );
    }
  }

  op_type->print();
  throw error("TODO: finish Mod");
}

Ref<JogCmd> JogCmdEQ::resolve()
{
  lhs = lhs->resolve();
  rhs = rhs->resolve();

  JogTypeInfo* lhs_type = lhs->type();
  JogTypeInfo* rhs_type = rhs->type();
  if (lhs_type && lhs_type->is_reference() && rhs_type && rhs_type->is_reference())
  {
    return new JogCmdEQRef( t, lhs, rhs );
  }
  else
  {
    JogCmdBinary::validate();
  }

  JogTypeInfo* op_type = lhs->type();
  if (lhs->is_literal() && rhs->is_literal())
  {
    if (op_type == jog_type_manager.type_real64)
    {
      double a = ((JogCmdLiteralReal64*) *lhs)->value;
      double b = ((JogCmdLiteralReal64*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a == b );
    }
    else if (op_type == jog_type_manager.type_real32)
    {
      float a = ((JogCmdLiteralReal32*) *lhs)->value;
      float b = ((JogCmdLiteralReal32*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a == b );
    }
    else if (op_type == jog_type_manager.type_int64)
    {
      JogInt64 a = ((JogCmdLiteralInt64*) *lhs)->value;
      JogInt64 b = ((JogCmdLiteralInt64*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a == b );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      int a = ((JogCmdLiteralInt32*) *lhs)->value;
      int b = ((JogCmdLiteralInt32*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a == b );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      JogInt16 a = ((JogCmdLiteralInt16*) *lhs)->value;
      JogInt16 b = ((JogCmdLiteralInt16*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a == b );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      JogInt8 a = ((JogCmdLiteralInt8*) *lhs)->value;
      JogInt8 b = ((JogCmdLiteralInt8*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a == b );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      JogChar a = ((JogCmdLiteralChar*) *lhs)->value;
      JogChar b = ((JogCmdLiteralChar*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a == b );
    }
    else
    {
      bool a = ((JogCmdLiteralBoolean*) *lhs)->value;
      bool b = ((JogCmdLiteralBoolean*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a == b );
    }
  }

  if (op_type->is_real())
  {
    return new JogCmdEQReal( t, lhs, rhs );
  }
  else
  {
    return new JogCmdEQInteger( t, lhs, rhs );
  }
}

Ref<JogCmd> JogCmdNE::resolve()
{
  lhs = lhs->resolve();
  rhs = rhs->resolve();

  JogTypeInfo* lhs_type = lhs->type();
  JogTypeInfo* rhs_type = rhs->type();
  if (lhs_type && lhs_type->is_reference() && rhs_type && rhs_type->is_reference())
  {
    return new JogCmdNERef( t, lhs, rhs );
  }
  else
  {
    JogCmdBinary::validate();
  }

  JogTypeInfo* op_type = lhs->type();
  if (lhs->is_literal() && rhs->is_literal())
  {
    if (op_type == jog_type_manager.type_real64)
    {
      double a = ((JogCmdLiteralReal64*) *lhs)->value;
      double b = ((JogCmdLiteralReal64*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a != b );
    }
    else if (op_type == jog_type_manager.type_real32)
    {
      float a = ((JogCmdLiteralReal32*) *lhs)->value;
      float b = ((JogCmdLiteralReal32*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a != b );
    }
    else if (op_type == jog_type_manager.type_int64)
    {
      JogInt64 a = ((JogCmdLiteralInt64*) *lhs)->value;
      JogInt64 b = ((JogCmdLiteralInt64*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a != b );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      int a = ((JogCmdLiteralInt32*) *lhs)->value;
      int b = ((JogCmdLiteralInt32*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a != b );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      JogInt16 a = ((JogCmdLiteralInt16*) *lhs)->value;
      JogInt16 b = ((JogCmdLiteralInt16*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a != b );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      JogInt8 a = ((JogCmdLiteralInt8*) *lhs)->value;
      JogInt8 b = ((JogCmdLiteralInt8*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a != b );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      JogChar a = ((JogCmdLiteralChar*) *lhs)->value;
      JogChar b = ((JogCmdLiteralChar*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a != b );
    }
    else
    {
      bool a = ((JogCmdLiteralBoolean*) *lhs)->value;
      bool b = ((JogCmdLiteralBoolean*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a != b );
    }
  }

  if (op_type->is_real())
  {
    return new JogCmdNEReal( t, lhs, rhs );
  }
  else
  {
    return new JogCmdNEInteger( t, lhs, rhs );
  }

  return this;
}

Ref<JogCmd> JogCmdLT::resolve()
{
  lhs = lhs->resolve();
  rhs = rhs->resolve();

  JogCmdBinary::validate();

  JogTypeInfo* op_type = lhs->type();
  if (lhs->is_literal() && rhs->is_literal())
  {
    if (op_type == jog_type_manager.type_real64)
    {
      double a = ((JogCmdLiteralReal64*) *lhs)->value;
      double b = ((JogCmdLiteralReal64*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a < b );
    }
    else if (op_type == jog_type_manager.type_real32)
    {
      float a = ((JogCmdLiteralReal32*) *lhs)->value;
      float b = ((JogCmdLiteralReal32*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a < b );
    }
    else if (op_type == jog_type_manager.type_int64)
    {
      JogInt64 a = ((JogCmdLiteralInt64*) *lhs)->value;
      JogInt64 b = ((JogCmdLiteralInt64*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a < b );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      int a = ((JogCmdLiteralInt32*) *lhs)->value;
      int b = ((JogCmdLiteralInt32*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a < b );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      JogInt16 a = ((JogCmdLiteralInt16*) *lhs)->value;
      JogInt16 b = ((JogCmdLiteralInt16*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a < b );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      JogInt8 a = ((JogCmdLiteralInt8*) *lhs)->value;
      JogInt8 b = ((JogCmdLiteralInt8*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a < b );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      JogChar a = ((JogCmdLiteralChar*) *lhs)->value;
      JogChar b = ((JogCmdLiteralChar*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a < b );
    }
    else
    {
      bool a = ((JogCmdLiteralBoolean*) *lhs)->value;
      bool b = ((JogCmdLiteralBoolean*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a < b );
    }
  }

  if (op_type->is_primitive())
  {
    if (op_type->is_real())
    {
      return new JogCmdLTReal( t, lhs, rhs );
    }
    else
    {
      return new JogCmdLTInteger( t, lhs, rhs );
    }
  }
  else
  {
    throw error( "Object references cannot be compared with '<'." );
  }

  return this;
}

Ref<JogCmd> JogCmdLE::resolve()
{
  lhs = lhs->resolve();
  rhs = rhs->resolve();

  JogCmdBinary::validate();

  JogTypeInfo* op_type = lhs->type();
  if (lhs->is_literal() && rhs->is_literal())
  {
    if (op_type == jog_type_manager.type_real64)
    {
      double a = ((JogCmdLiteralReal64*) *lhs)->value;
      double b = ((JogCmdLiteralReal64*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a <= b );
    }
    else if (op_type == jog_type_manager.type_real32)
    {
      float a = ((JogCmdLiteralReal32*) *lhs)->value;
      float b = ((JogCmdLiteralReal32*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a <= b );
    }
    else if (op_type == jog_type_manager.type_int64)
    {
      JogInt64 a = ((JogCmdLiteralInt64*) *lhs)->value;
      JogInt64 b = ((JogCmdLiteralInt64*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a <= b );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      int a = ((JogCmdLiteralInt32*) *lhs)->value;
      int b = ((JogCmdLiteralInt32*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a <= b );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      JogInt16 a = ((JogCmdLiteralInt16*) *lhs)->value;
      JogInt16 b = ((JogCmdLiteralInt16*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a <= b );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      JogInt8 a = ((JogCmdLiteralInt8*) *lhs)->value;
      JogInt8 b = ((JogCmdLiteralInt8*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a <= b );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      JogChar a = ((JogCmdLiteralChar*) *lhs)->value;
      JogChar b = ((JogCmdLiteralChar*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a <= b );
    }
    else
    {
      bool a = ((JogCmdLiteralBoolean*) *lhs)->value;
      bool b = ((JogCmdLiteralBoolean*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a <= b );
    }
  }

  if (op_type->is_primitive())
  {
    if (op_type->is_real())
    {
      return new JogCmdLEReal( t, lhs, rhs );
    }
    else
    {
      return new JogCmdLEInteger( t, lhs, rhs );
    }
  }
  else
  {
    throw error( "Object references cannot be compared with '<'." );
  }

  return this;
}

Ref<JogCmd> JogCmdGT::resolve()
{
  lhs = lhs->resolve();
  rhs = rhs->resolve();

  JogCmdBinary::validate();

  JogTypeInfo* op_type = lhs->type();
  if (lhs->is_literal() && rhs->is_literal())
  {
    if (op_type == jog_type_manager.type_real64)
    {
      double a = ((JogCmdLiteralReal64*) *lhs)->value;
      double b = ((JogCmdLiteralReal64*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a > b );
    }
    else if (op_type == jog_type_manager.type_real32)
    {
      float a = ((JogCmdLiteralReal32*) *lhs)->value;
      float b = ((JogCmdLiteralReal32*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a > b );
    }
    else if (op_type == jog_type_manager.type_int64)
    {
      JogInt64 a = ((JogCmdLiteralInt64*) *lhs)->value;
      JogInt64 b = ((JogCmdLiteralInt64*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a > b );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      int a = ((JogCmdLiteralInt32*) *lhs)->value;
      int b = ((JogCmdLiteralInt32*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a > b );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      JogInt16 a = ((JogCmdLiteralInt16*) *lhs)->value;
      JogInt16 b = ((JogCmdLiteralInt16*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a > b );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      JogInt8 a = ((JogCmdLiteralInt8*) *lhs)->value;
      JogInt8 b = ((JogCmdLiteralInt8*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a > b );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      JogChar a = ((JogCmdLiteralChar*) *lhs)->value;
      JogChar b = ((JogCmdLiteralChar*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a > b );
    }
    else
    {
      bool a = ((JogCmdLiteralBoolean*) *lhs)->value;
      bool b = ((JogCmdLiteralBoolean*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a > b );
    }
  }

  if (op_type->is_primitive())
  {
    if (op_type->is_real())
    {
      return new JogCmdGTReal( t, lhs, rhs );
    }
    else
    {
      return new JogCmdGTInteger( t, lhs, rhs );
    }
  }
  else
  {
    throw error( "Object references cannot be compared with '<'." );
  }

  return this;
}

Ref<JogCmd> JogCmdGE::resolve()
{
  lhs = lhs->resolve();
  rhs = rhs->resolve();

  JogCmdBinary::validate();

  JogTypeInfo* op_type = lhs->type();
  if (lhs->is_literal() && rhs->is_literal())
  {
    if (op_type == jog_type_manager.type_real64)
    {
      double a = ((JogCmdLiteralReal64*) *lhs)->value;
      double b = ((JogCmdLiteralReal64*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a >= b );
    }
    else if (op_type == jog_type_manager.type_real32)
    {
      float a = ((JogCmdLiteralReal32*) *lhs)->value;
      float b = ((JogCmdLiteralReal32*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a >= b );
    }
    else if (op_type == jog_type_manager.type_int64)
    {
      JogInt64 a = ((JogCmdLiteralInt64*) *lhs)->value;
      JogInt64 b = ((JogCmdLiteralInt64*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a >= b );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      int a = ((JogCmdLiteralInt32*) *lhs)->value;
      int b = ((JogCmdLiteralInt32*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a >= b );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      JogInt16 a = ((JogCmdLiteralInt16*) *lhs)->value;
      JogInt16 b = ((JogCmdLiteralInt16*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a >= b );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      JogInt8 a = ((JogCmdLiteralInt8*) *lhs)->value;
      JogInt8 b = ((JogCmdLiteralInt8*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a >= b );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      JogChar a = ((JogCmdLiteralChar*) *lhs)->value;
      JogChar b = ((JogCmdLiteralChar*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a >= b );
    }
    else
    {
      bool a = ((JogCmdLiteralBoolean*) *lhs)->value;
      bool b = ((JogCmdLiteralBoolean*) *rhs)->value;
      return new JogCmdLiteralBoolean( t, a >= b );
    }
  }

  if (op_type->is_primitive())
  {
    if (op_type->is_real())
    {
      return new JogCmdGEReal( t, lhs, rhs );
    }
    else
    {
      return new JogCmdGEInteger( t, lhs, rhs );
    }
  }
  else
  {
    throw error( "Object references cannot be compared with '<'." );
  }

  return this;
}

Ref<JogCmd> JogCmdLeftShift::resolve()
{
  operand = operand->resolve();
  shift_amount = shift_amount->resolve();

  if (shift_amount->type() != jog_type_manager.type_int32)
  {
    throw shift_amount->error( "Shift amount must be type 'int'." );
  }

  JogTypeInfo* op_type = operand->require_integer();
  if (operand->is_literal() && shift_amount->is_literal())
  {
    int shift = ((JogCmdLiteralInt32*) *shift_amount)->value;
    if (op_type == jog_type_manager.type_int64)
    {
      JogInt64 a = ((JogCmdLiteralInt64*) *operand)->value;
      return new JogCmdLiteralInt64( t, a << shift );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      int a = ((JogCmdLiteralInt32*) *operand)->value;
      return new JogCmdLiteralInt32( t, a << shift );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      JogInt16 a = ((JogCmdLiteralInt16*) *operand)->value;
      return new JogCmdLiteralInt32( t, a << shift );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      JogInt8 a = ((JogCmdLiteralInt8*) *operand)->value;
      return new JogCmdLiteralInt32( t, a << shift );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      JogChar a = ((JogCmdLiteralChar*) *operand)->value;
      return new JogCmdLiteralInt32( t, a << shift );
    }
  }

  if (op_type == jog_type_manager.type_int64)
  {
    return new JogCmdLeftShiftInt64( t, operand, shift_amount );
  }
  else
  {
    return new JogCmdLeftShiftInt32( t, operand, shift_amount );
  }
}


Ref<JogCmd> JogCmdRightShift::resolve()
{
  operand = operand->resolve();
  shift_amount = shift_amount->resolve();

  if (shift_amount->type() != jog_type_manager.type_int32)
  {
    throw shift_amount->error( "Shift amount must be type 'int'." );
  }

  JogTypeInfo* op_type = operand->require_integer();
  if (operand->is_literal() && shift_amount->is_literal())
  {
    int shift = ((JogCmdLiteralInt32*) *shift_amount)->value;

    if (op_type == jog_type_manager.type_int64)
    {
      JogInt64 a = ((JogCmdLiteralInt64*) *operand)->value;
      return new JogCmdLiteralInt64( t, JOG_SHR(JogInt64,a,shift) );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      int a = ((JogCmdLiteralInt32*) *operand)->value;
      return new JogCmdLiteralInt32( t, JOG_SHR(JogInt32,a,shift) );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      JogInt16 a = ((JogCmdLiteralInt16*) *operand)->value;
      return new JogCmdLiteralInt32( t, JOG_SHR(JogInt32,a,shift) );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      JogInt8 a = ((JogCmdLiteralInt8*) *operand)->value;
      return new JogCmdLiteralInt32( t, JOG_SHR(JogInt32,a,shift) );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      JogChar a = ((JogCmdLiteralChar*) *operand)->value;
      return new JogCmdLiteralInt32( t, JOG_SHR(JogInt32,a,shift) );
    }
  }

  if (op_type == jog_type_manager.type_int64)
  {
    return new JogCmdRightShiftInt64( t, operand, shift_amount );
  }
  else
  {
    return new JogCmdRightShiftInt32( t, operand, shift_amount );
  }
}

Ref<JogCmd> JogCmdRightXShift::resolve()
{
  operand = operand->resolve();
  shift_amount = shift_amount->resolve();

  if (shift_amount->type() != jog_type_manager.type_int32)
  {
    throw shift_amount->error( "Shift amount must be type 'int'." );
  }

  JogTypeInfo* op_type = operand->require_integer();
  if (operand->is_literal() && shift_amount->is_literal())
  {
    int shift = ((JogCmdLiteralInt32*) *shift_amount)->value;

    if (op_type == jog_type_manager.type_int64)
    {
      JogInt64 a = ((JogCmdLiteralInt64*) *operand)->value;
      return new JogCmdLiteralInt64( t, a >> shift );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      int a = ((JogCmdLiteralInt32*) *operand)->value;
      return new JogCmdLiteralInt32( t, a >> shift );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      JogInt16 a = ((JogCmdLiteralInt16*) *operand)->value;
      return new JogCmdLiteralInt32( t, a >> shift );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      JogInt8 a = ((JogCmdLiteralInt8*) *operand)->value;
      return new JogCmdLiteralInt32( t, a >> shift );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      JogChar a = ((JogCmdLiteralChar*) *operand)->value;
      return new JogCmdLiteralInt32( t, a >> shift );
    }
  }

  if (op_type == jog_type_manager.type_int64)
  {
    return new JogCmdRightXShiftInt64( t, operand, shift_amount );
  }
  else
  {
    return new JogCmdRightXShiftInt32( t, operand, shift_amount );
  }
}

Ref<JogCmd> JogCmdIdentifier::resolve()
{
  if (name->equals("this"))
  {
    return new JogCmdThis( t, jog_context->this_type );
  }

  JogLocalVarInfo* var_info = jog_context->find_local(name);

  if (var_info)
  {
    if (var_info->type->is_reference())
    {
      return new JogCmdReadLocalRef( t, var_info );
    }
    else
    {
      return new JogCmdReadLocalData( t, var_info );
    }
  }

  if (jog_context->this_method->is_static())
  {
    return resolve( jog_context->this_type, NULL );
  }
  else
  {
    return resolve( new JogCmdThis(t,jog_context->this_type) );
  }
}

Ref<JogCmd> JogCmdIdentifier::resolve( Ref<JogCmd> context )
{
  JogTypeInfo* context_type = context->type();
  context_type->resolve();

  if (context_type->is_array())
  {
    if (name->equals("length"))
    {
      return new JogCmdArraySize( t, context );
    }
  }

  JogPropertyInfo* var_info = context_type->properties_by_name[name];

  if (var_info)
  {
    if (var_info->type->is_reference())
    {
      return new JogCmdReadPropertyRef( t, context, var_info );
    }
    else
    {
      return new JogCmdReadPropertyData( t, context, var_info );
    }
  }

  if (context_type->class_properties_by_name[name])
  {
    return resolve( context_type, context );
  }

  Ref<JogString> mesg = new JogString( "No such property '" );
  mesg->add(name);
  mesg->add("'.");
  throw t->error( mesg->to_ascii() );
}

Ref<JogCmd> JogCmdIdentifier::resolve( JogTypeInfo* class_context, Ref<JogCmd> context )
{
  JogPropertyInfo* var_info = class_context->class_properties_by_name[name];

  if (var_info)
  {
    var_info->type->resolve();
    if (var_info->type->is_reference())
    {
      return new JogCmdReadClassPropertyRef( t, context, var_info );
    }
    else
    {
      return new JogCmdReadClassPropertyData( t, context, var_info );
    }
  }

  Ref<JogString> mesg = new JogString( "No such class property '" );
  mesg->add(name);
  mesg->add("'.");
  throw t->error( mesg->to_ascii() );
}

Ref<JogCmd> JogCmdIdentifier::resolve_assignment( Ref<JogCmd> context, Ref<JogCmd> new_value )
{
  if (*context == NULL)
  {
    JogLocalVarInfo* var_info = jog_context->find_local(name);

    if (var_info)
    {
      if (var_info->type->is_reference())
      {
        var_info->type->resolve();
        new_value = new_value->box(var_info->type);
        new_value->require_instance_of(var_info->type);
        return new JogCmdWriteLocalRef( t, var_info, new_value );
      }
      else
      {
        Ref<JogCmd> result = new JogCmdWriteLocalData( t, var_info, new_value );
        JogTypeInfo* common_type = jog_type_manager.find_common_type( *t, *result, *new_value );
        if (common_type != var_info->type)
        {
          if (new_value->is_literal_int())
          {
            int n = ((JogCmdLiteralInt32*)(*new_value))->value;
            if (n == (short) n 
                || var_info->type == jog_type_manager.type_char && n == (JogChar) n)
            {
              ((JogCmdWriteLocalData*)*result)->new_value = new_value->cast_to_type(common_type)->resolve();
              return result;
            }
          }
          throw error( "Loss of precision in assignment requires an explicit cast, e.g. \"int x = y;\" -> \"int x = (int) y;\"." );
        }
        else if (common_type != new_value->type())
        {
          ((JogCmdWriteLocalData*)*result)->new_value = new_value->cast_to_type(common_type)->resolve();
        }
        return result;
      }
    }
  }

  {
    if (*context == NULL) 
    {
      JogPropertyInfo* var_info = jog_context->this_type->class_properties_by_name[name];
      if (var_info) return resolve_assignment( jog_context->this_type, NULL, new_value );
      context = new JogCmdThis( t, jog_context->this_type );
    }

    JogTypeInfo* context_type = context->type();

    if (context_type->is_array())
    {
      if (name->equals("length"))
      {
        throw error( "Array length property is read-only." );
      }
    }

    JogPropertyInfo* var_info = context_type->properties_by_name[name];

    if (var_info)
    {
      if (var_info->type->is_reference())
      {
        var_info->type->resolve();
        new_value = new_value->box(var_info->type);
        new_value->require_instance_of(var_info->type);
        return new JogCmdWritePropertyRef( t, context, var_info, new_value );
      }
      else
      {
        Ref<JogCmd> result = new JogCmdWritePropertyData( t, context, var_info, new_value );
        JogTypeInfo* common_type = jog_type_manager.find_common_type( *t, *result, *new_value );
        if (common_type != var_info->type)
        {
          if (new_value->is_literal_int())
          {
            int n = ((JogCmdLiteralInt32*)(*new_value))->value;
            if (n == (short) n 
                || var_info->type == jog_type_manager.type_char && n == (JogChar) n)
            {
              ((JogCmdWritePropertyData*)*result)->new_value 
                = new_value->cast_to_type(common_type)->resolve();
              return result;
            }
          }
          throw error( "Loss of precision in assignment requires an explicit cast, e.g. \"int x = y;\" -> \"int x = (int) y;\"." );
        }
        else if (common_type != new_value->type())
        {
          ((JogCmdWritePropertyData*)*result)->new_value 
            = new_value->cast_to_type(common_type)->resolve();
        }
        return result;
      }
    }

    var_info = context_type->class_properties_by_name[name];
    if (var_info) return resolve_assignment( context_type, context, new_value );
  }

  Ref<JogString> mesg = new JogString( "No such variable '" );
  mesg->add(name);
  mesg->add("'.");
  throw t->error( mesg->to_ascii() );
}

Ref<JogCmd> JogCmdIdentifier::resolve_assignment( JogTypeInfo* class_context, 
    Ref<JogCmd> context, Ref<JogCmd> new_value )
{
  JogPropertyInfo* var_info = class_context->class_properties_by_name[name];

  if (var_info)
  {
    if (var_info->type->is_reference())
    {
      var_info->type->resolve();
      new_value = new_value->box(var_info->type);
      new_value->require_instance_of(var_info->type);
      return new JogCmdWriteClassPropertyRef( t, context, var_info, new_value );
    }
    else
    {
      Ref<JogCmd> result = new JogCmdWriteClassPropertyData(t, context, 
          var_info, new_value);
      JogTypeInfo* common_type = jog_type_manager.find_common_type( *t, *result, *new_value );
      if (common_type != var_info->type)
      {
        if (new_value->is_literal_int())
        {
          int n = ((JogCmdLiteralInt32*)(*new_value))->value;
          if (n == (short) n 
              || var_info->type == jog_type_manager.type_char && n == (JogChar) n)
          {
            ((JogCmdWriteClassPropertyData*)*result)->new_value 
              = new_value->cast_to_type(common_type)->resolve();
            return result;
          }
        }
        throw error( "Loss of precision in assignment requires an explicit cast, e.g. \"int x = y;\" -> \"int x = (int) y;\"." );
      }
      else if (common_type != new_value->type())
      {
        ((JogCmdWriteClassPropertyData*)*result)->new_value 
          = new_value->cast_to_type(common_type)->resolve();
      }
      return result;
    }
  }

  return JogCmd::resolve_assignment(class_context,context,new_value);
}

Ref<JogCmd> JogCmdIdentifier::resolve_op_assign( int op_type, Ref<JogCmd> context, Ref<JogCmd> rhs )
{
  {
    JogLocalVarInfo* var_info = jog_context->find_local(name);

    if (var_info)
    {
      rhs = rhs->resolve();

      if (op_type == TOKEN_ADD_ASSIGN && var_info->type->is_reference())
      {
        if ( !var_info->type->instance_of(jog_type_manager.type_string) )
        {
          throw error( "'+=' can only be used with numerical and String values." );
        }

        JogTypeInfo* rhs_type = rhs->require_value();
        if (rhs_type->is_primitive())
        {
          Ref<JogCmdList> args = new JogCmdList(t);
          args->add(rhs);
          JogMethodInfo* m = resolve_call( t, rhs_type->wrapper_type(), 
              new JogString("toString"), *args, false );
          rhs = (new JogCmdClassCall( t, m, NULL, args ))->resolve();
        }
      }

      rhs = rhs->cast_to_type( var_info->type )->resolve();

      if (op_type == TOKEN_ADD_ASSIGN && var_info->type->is_reference())
      {
        // We've already established this as a String.
        Ref<JogCmdList> args = new JogCmdList(t);
        args->add(rhs);

        Ref<JogCmd> result = new JogCmdAssign( t,
            this,
            new JogCmdMemberAccess( t,
              new JogCmdIdentifier(t,name),
              new JogCmdMethodCall(t,new JogString("concat"),args)
            )
          );
        return result->resolve();
      }

      if (var_info->type == jog_type_manager.type_real64)
      {
        switch (op_type)
        {
          case TOKEN_ADD_ASSIGN:
            return (new JogCmdAddAssignLocalReal<double>())->init( t, var_info, rhs );
          case TOKEN_SUB_ASSIGN:
            return (new JogCmdSubAssignLocalReal<double>())->init( t, var_info, rhs );
          case TOKEN_MUL_ASSIGN:
            return (new JogCmdMulAssignLocalReal<double>())->init( t, var_info, rhs );
          case TOKEN_DIV_ASSIGN:
            return (new JogCmdDivAssignLocalReal<double>())->init( t, var_info, rhs );
          case TOKEN_MOD_ASSIGN:
            throw error( "Modulo can only be used on integer values." );
          case TOKEN_AND_ASSIGN:
            throw error( "Bitwise And can only be used on integer and boolean values." );
          case TOKEN_OR_ASSIGN:
            throw error( "Bitwise Or can only be used on integer and boolean values." );
          case TOKEN_XOR_ASSIGN:
            throw error( "Bitwise Xor can only be used on integer and boolean values." );
          case TOKEN_SHL_ASSIGN:
            throw error( "Left Shift can only be used on integer values." );
          case TOKEN_SHR_ASSIGN:
            throw error( "Right Shift can only be used on integer values." );
          case TOKEN_SHRX_ASSIGN:
            throw error( "Right Shift with Sign Extend can only be used on integer values." );
        }
      }
      else if (var_info->type == jog_type_manager.type_real32)
      {
        switch (op_type)
        {
          case TOKEN_ADD_ASSIGN:
            return (new JogCmdAddAssignLocalReal<float>())->init( t, var_info, rhs );
          case TOKEN_SUB_ASSIGN:
            return (new JogCmdSubAssignLocalReal<float>())->init( t, var_info, rhs );
          case TOKEN_MUL_ASSIGN:
            return (new JogCmdMulAssignLocalReal<float>())->init( t, var_info, rhs );
          case TOKEN_DIV_ASSIGN:
            return (new JogCmdDivAssignLocalReal<float>())->init( t, var_info, rhs );
          case TOKEN_MOD_ASSIGN:
            throw error( "Modulo can only be used on integer values." );
          case TOKEN_AND_ASSIGN:
            throw error( "Bitwise And can only be used on integer and boolean values." );
          case TOKEN_OR_ASSIGN:
            throw error( "Bitwise Or can only be used on integer and boolean values." );
          case TOKEN_XOR_ASSIGN:
            throw error( "Bitwise Xor can only be used on integer and boolean values." );
          case TOKEN_SHL_ASSIGN:
            throw error( "Left Shift can only be used on integer values." );
          case TOKEN_SHR_ASSIGN:
            throw error( "Right Shift can only be used on integer values." );
          case TOKEN_SHRX_ASSIGN:
            throw error( "Right Shift with Sign Extend can only be used on integer values." );
        }
      }
      else if (var_info->type == jog_type_manager.type_int64)
      {
        switch (op_type)
        {
          case TOKEN_ADD_ASSIGN:
            return (new JogCmdAddAssignLocalInteger<JogInt64>())->init( t, var_info, rhs );
          case TOKEN_SUB_ASSIGN:
            return (new JogCmdSubAssignLocalInteger<JogInt64>())->init( t, var_info, rhs );
          case TOKEN_MUL_ASSIGN:
            return (new JogCmdMulAssignLocalInteger<JogInt64>())->init( t, var_info, rhs );
          case TOKEN_DIV_ASSIGN:
            return (new JogCmdDivAssignLocalInteger<JogInt64>())->init( t, var_info, rhs );
          case TOKEN_MOD_ASSIGN:
            return (new JogCmdModAssignLocal<JogInt64>())->init( t, var_info, rhs );
          case TOKEN_AND_ASSIGN:
            return (new JogCmdAndAssignLocal<JogInt64>())->init( t, var_info, rhs );
          case TOKEN_OR_ASSIGN:
            return (new JogCmdOrAssignLocal<JogInt64>())->init( t, var_info, rhs );
          case TOKEN_XOR_ASSIGN:
            return (new JogCmdXorAssignLocal<JogInt64>())->init( t, var_info, rhs );
          case TOKEN_SHL_ASSIGN:
            return (new JogCmdSHLAssignLocal<JogInt64>())->init( t, var_info, rhs );
          case TOKEN_SHR_ASSIGN:
            return (new JogCmdSHRAssignLocal<JogInt64>())->init( t, var_info, rhs );
          case TOKEN_SHRX_ASSIGN:
            return (new JogCmdSHRXAssignLocal<JogInt64>())->init( t, var_info, rhs );
        }
      }
      else if (var_info->type == jog_type_manager.type_int32)
      {
        switch (op_type)
        {
          case TOKEN_ADD_ASSIGN:
            return (new JogCmdAddAssignLocalInteger<JogInt32>())->init( t, var_info, rhs );
          case TOKEN_SUB_ASSIGN:
            return (new JogCmdSubAssignLocalInteger<JogInt32>())->init( t, var_info, rhs );
          case TOKEN_MUL_ASSIGN:
            return (new JogCmdMulAssignLocalInteger<JogInt32>())->init( t, var_info, rhs );
          case TOKEN_DIV_ASSIGN:
            return (new JogCmdDivAssignLocalInteger<JogInt32>())->init( t, var_info, rhs );
          case TOKEN_MOD_ASSIGN:
            return (new JogCmdModAssignLocal<JogInt32>())->init( t, var_info, rhs );
          case TOKEN_AND_ASSIGN:
            return (new JogCmdAndAssignLocal<JogInt32>())->init( t, var_info, rhs );
          case TOKEN_OR_ASSIGN:
            return (new JogCmdOrAssignLocal<JogInt32>())->init( t, var_info, rhs );
          case TOKEN_XOR_ASSIGN:
            return (new JogCmdXorAssignLocal<JogInt32>())->init( t, var_info, rhs );
          case TOKEN_SHL_ASSIGN:
            return (new JogCmdSHLAssignLocal<JogInt32>())->init( t, var_info, rhs );
          case TOKEN_SHR_ASSIGN:
            return (new JogCmdSHRAssignLocal<JogInt32>())->init( t, var_info, rhs );
          case TOKEN_SHRX_ASSIGN:
            return (new JogCmdSHRXAssignLocal<JogInt32>())->init( t, var_info, rhs );
        }
      }
      else if (var_info->type == jog_type_manager.type_int16)
      {
        switch (op_type)
        {
          case TOKEN_ADD_ASSIGN:
            return (new JogCmdAddAssignLocalInteger<JogInt16>())->init( t, var_info, rhs );
          case TOKEN_SUB_ASSIGN:
            return (new JogCmdSubAssignLocalInteger<JogInt16>())->init( t, var_info, rhs );
          case TOKEN_MUL_ASSIGN:
            return (new JogCmdMulAssignLocalInteger<JogInt16>())->init( t, var_info, rhs );
          case TOKEN_DIV_ASSIGN:
            return (new JogCmdDivAssignLocalInteger<JogInt16>())->init( t, var_info, rhs );
          case TOKEN_MOD_ASSIGN:
            return (new JogCmdModAssignLocal<JogInt16>())->init( t, var_info, rhs );
          case TOKEN_AND_ASSIGN:
            return (new JogCmdAndAssignLocal<JogInt16>())->init( t, var_info, rhs );
          case TOKEN_OR_ASSIGN:
            return (new JogCmdOrAssignLocal<JogInt16>())->init( t, var_info, rhs );
          case TOKEN_XOR_ASSIGN:
            return (new JogCmdXorAssignLocal<JogInt16>())->init( t, var_info, rhs );
          case TOKEN_SHL_ASSIGN:
            return (new JogCmdSHLAssignLocal<JogInt16>())->init( t, var_info, rhs );
          case TOKEN_SHR_ASSIGN:
            return (new JogCmdSHRAssignLocal<JogInt16>())->init( t, var_info, rhs );
          case TOKEN_SHRX_ASSIGN:
            return (new JogCmdSHRXAssignLocal<JogInt16>())->init( t, var_info, rhs );
        }
      }
      else if (var_info->type == jog_type_manager.type_int8)
      {
        switch (op_type)
        {
          case TOKEN_ADD_ASSIGN:
            return (new JogCmdAddAssignLocalInteger<JogInt8>())->init( t, var_info, rhs );
          case TOKEN_SUB_ASSIGN:
            return (new JogCmdSubAssignLocalInteger<JogInt8>())->init( t, var_info, rhs );
          case TOKEN_MUL_ASSIGN:
            return (new JogCmdMulAssignLocalInteger<JogInt8>())->init( t, var_info, rhs );
          case TOKEN_DIV_ASSIGN:
            return (new JogCmdDivAssignLocalInteger<JogInt8>())->init( t, var_info, rhs );
          case TOKEN_MOD_ASSIGN:
            return (new JogCmdModAssignLocal<JogInt8>())->init( t, var_info, rhs );
          case TOKEN_AND_ASSIGN:
            return (new JogCmdAndAssignLocal<JogInt8>())->init( t, var_info, rhs );
          case TOKEN_OR_ASSIGN:
            return (new JogCmdOrAssignLocal<JogInt8>())->init( t, var_info, rhs );
          case TOKEN_XOR_ASSIGN:
            return (new JogCmdXorAssignLocal<JogInt8>())->init( t, var_info, rhs );
          case TOKEN_SHL_ASSIGN:
            return (new JogCmdSHLAssignLocal<JogInt8>())->init( t, var_info, rhs );
          case TOKEN_SHR_ASSIGN:
            return (new JogCmdSHRAssignLocal<JogInt8>())->init( t, var_info, rhs );
          case TOKEN_SHRX_ASSIGN:
            return (new JogCmdSHRXAssignLocal<JogInt8>())->init( t, var_info, rhs );
        }
      }
      else if (var_info->type == jog_type_manager.type_char)
      {
        switch (op_type)
        {
          case TOKEN_ADD_ASSIGN:
            return (new JogCmdAddAssignLocalInteger<JogChar>())->init( t, var_info, rhs );
          case TOKEN_SUB_ASSIGN:
            return (new JogCmdSubAssignLocalInteger<JogChar>())->init( t, var_info, rhs );
          case TOKEN_MUL_ASSIGN:
            return (new JogCmdMulAssignLocalInteger<JogChar>())->init( t, var_info, rhs );
          case TOKEN_DIV_ASSIGN:
            return (new JogCmdDivAssignLocalInteger<JogChar>())->init( t, var_info, rhs );
          case TOKEN_MOD_ASSIGN:
            return (new JogCmdModAssignLocal<JogChar>())->init( t, var_info, rhs );
          case TOKEN_AND_ASSIGN:
            return (new JogCmdAndAssignLocal<JogChar>())->init( t, var_info, rhs );
          case TOKEN_OR_ASSIGN:
            return (new JogCmdOrAssignLocal<JogChar>())->init( t, var_info, rhs );
          case TOKEN_XOR_ASSIGN:
            return (new JogCmdXorAssignLocal<JogChar>())->init( t, var_info, rhs );
          case TOKEN_SHL_ASSIGN:
            return (new JogCmdSHLAssignLocal<JogChar>())->init( t, var_info, rhs );
          case TOKEN_SHR_ASSIGN:
            return (new JogCmdSHRAssignLocal<JogChar>())->init( t, var_info, rhs );
          case TOKEN_SHRX_ASSIGN:
            return (new JogCmdSHRXAssignLocal<JogChar>())->init( t, var_info, rhs );
        }
      }
      else
      {
        switch (op_type)
        {
          case TOKEN_ADD_ASSIGN:
            throw error( "Boolean values cannot be added." );
          case TOKEN_SUB_ASSIGN:
            throw error( "Boolean values cannot be subtracted." );
          case TOKEN_MUL_ASSIGN:
            throw error( "Boolean values cannot be multiplied." );
          case TOKEN_DIV_ASSIGN:
            throw error( "Boolean values cannot be divided." );
          case TOKEN_MOD_ASSIGN:
            throw error( "Modulo can only be used on integer values." );
          case TOKEN_AND_ASSIGN:
            return (new JogCmdAndAssignLocal<JogInt64>())->init( t, var_info, rhs );
          case TOKEN_OR_ASSIGN:
            return (new JogCmdOrAssignLocal<JogInt64>())->init( t, var_info, rhs );
          case TOKEN_XOR_ASSIGN:
            return (new JogCmdXorAssignLocal<JogInt64>())->init( t, var_info, rhs );
          case TOKEN_SHL_ASSIGN:
          case TOKEN_SHR_ASSIGN:
          case TOKEN_SHRX_ASSIGN:
            throw error( "Boolean values cannot be bit-shifted." );
        }
      }
    }
  }

  {
    if (*context == NULL) 
    {
      JogPropertyInfo* var_info = jog_context->this_type->class_properties_by_name[name];
      if (var_info) return resolve_op_assign( op_type, jog_context->this_type, NULL, rhs );
      context = new JogCmdThis( t, jog_context->this_type );
    }

    JogTypeInfo* context_type = context->type();
    JogPropertyInfo* var_info = context_type->properties_by_name[name];

    if (var_info)
    {
      rhs = rhs->resolve();

      if (op_type == TOKEN_ADD_ASSIGN && var_info->type->is_reference())
      {
        if ( !var_info->type->instance_of(jog_type_manager.type_string) )
        {
          throw error( "'+=' can only be used with numerical and String values." );
        }

        JogTypeInfo* rhs_type = rhs->require_value();
        if (rhs_type->is_primitive())
        {
          Ref<JogCmdList> args = new JogCmdList(t);
          args->add(rhs);
          JogMethodInfo* m = resolve_call( t, rhs_type->wrapper_type(), 
              new JogString("toString"), *args, false );
          rhs = (new JogCmdClassCall( t, m, NULL, args ))->resolve();
        }
        return (new JogCmdAddAssignPropertyString())->init( t, context, var_info, rhs );
      }

      rhs = rhs->cast_to_type( var_info->type )->resolve();

      if (var_info->type == jog_type_manager.type_real64)
      {
        switch (op_type)
        {
          case TOKEN_ADD_ASSIGN:
            return (new JogCmdAddAssignPropertyReal<double>())->init( t, context, var_info, rhs );
          case TOKEN_SUB_ASSIGN:
            return (new JogCmdSubAssignPropertyReal<double>())->init( t, context, var_info, rhs );
          case TOKEN_MUL_ASSIGN:
            return (new JogCmdMulAssignPropertyReal<double>())->init( t, context, var_info, rhs );
          case TOKEN_DIV_ASSIGN:
            return (new JogCmdDivAssignPropertyReal<double>())->init( t, context, var_info, rhs );
          case TOKEN_MOD_ASSIGN:
            throw error( "Modulo can only be used on integer values." );
          case TOKEN_AND_ASSIGN:
            throw error( "Bitwise And can only be used on integer and boolean values." );
          case TOKEN_OR_ASSIGN:
            throw error( "Bitwise Or can only be used on integer and boolean values." );
          case TOKEN_XOR_ASSIGN:
            throw error( "Bitwise Xor can only be used on integer and boolean values." );
          case TOKEN_SHL_ASSIGN:
            throw error( "Left Shift can only be used on integer values." );
          case TOKEN_SHR_ASSIGN:
            throw error( "Right Shift can only be used on integer values." );
          case TOKEN_SHRX_ASSIGN:
            throw error( "Right Shift with Sign Extend can only be used on integer values." );
        }
      }
      else if (var_info->type == jog_type_manager.type_real32)
      {
        switch (op_type)
        {
          case TOKEN_ADD_ASSIGN:
            return (new JogCmdAddAssignPropertyReal<float>())->init( t, context, var_info, rhs );
          case TOKEN_SUB_ASSIGN:
            return (new JogCmdSubAssignPropertyReal<float>())->init( t, context, var_info, rhs );
          case TOKEN_MUL_ASSIGN:
            return (new JogCmdMulAssignPropertyReal<float>())->init( t, context, var_info, rhs );
          case TOKEN_DIV_ASSIGN:
            return (new JogCmdDivAssignPropertyReal<float>())->init( t, context, var_info, rhs );
          case TOKEN_MOD_ASSIGN:
            throw error( "Modulo can only be used on integer values." );
          case TOKEN_AND_ASSIGN:
            throw error( "Bitwise And can only be used on integer and boolean values." );
          case TOKEN_OR_ASSIGN:
            throw error( "Bitwise Or can only be used on integer and boolean values." );
          case TOKEN_XOR_ASSIGN:
            throw error( "Bitwise Xor can only be used on integer and boolean values." );
          case TOKEN_SHL_ASSIGN:
            throw error( "Left Shift can only be used on integer values." );
          case TOKEN_SHR_ASSIGN:
            throw error( "Right Shift can only be used on integer values." );
          case TOKEN_SHRX_ASSIGN:
            throw error( "Right Shift with Sign Extend can only be used on integer values." );
        }
      }
      else if (var_info->type == jog_type_manager.type_int64)
      {
        switch (op_type)
        {
          case TOKEN_ADD_ASSIGN:
            return (new JogCmdAddAssignPropertyInteger<JogInt64>())->init( t, context, var_info, rhs );
          case TOKEN_SUB_ASSIGN:
            return (new JogCmdSubAssignPropertyInteger<JogInt64>())->init( t, context, var_info, rhs );
          case TOKEN_MUL_ASSIGN:
            return (new JogCmdMulAssignPropertyInteger<JogInt64>())->init( t, context, var_info, rhs );
          case TOKEN_DIV_ASSIGN:
            return (new JogCmdDivAssignPropertyInteger<JogInt64>())->init( t, context, var_info, rhs );
          case TOKEN_MOD_ASSIGN:
            return (new JogCmdModAssignProperty<JogInt64>())->init( t, context, var_info, rhs );
          case TOKEN_AND_ASSIGN:
            return (new JogCmdAndAssignProperty<JogInt64>())->init( t, context, var_info, rhs );
          case TOKEN_OR_ASSIGN:
            return (new JogCmdOrAssignProperty<JogInt64>())->init( t, context, var_info, rhs );
          case TOKEN_XOR_ASSIGN:
            return (new JogCmdXorAssignProperty<JogInt64>())->init( t, context, var_info, rhs );
          case TOKEN_SHL_ASSIGN:
            return (new JogCmdSHLAssignProperty<JogInt64>())->init( t, context, var_info, rhs );
          case TOKEN_SHR_ASSIGN:
            return (new JogCmdSHRAssignProperty<JogInt64>())->init( t, context, var_info, rhs );
          case TOKEN_SHRX_ASSIGN:
            return (new JogCmdSHRXAssignProperty<JogInt64>())->init( t, context, var_info, rhs );
        }
      }
      else if (var_info->type == jog_type_manager.type_int32)
      {
        switch (op_type)
        {
          case TOKEN_ADD_ASSIGN:
            return (new JogCmdAddAssignPropertyInteger<JogInt32>())->init( t, context, var_info, rhs );
          case TOKEN_SUB_ASSIGN:
            return (new JogCmdSubAssignPropertyInteger<JogInt32>())->init( t, context, var_info, rhs );
          case TOKEN_MUL_ASSIGN:
            return (new JogCmdMulAssignPropertyInteger<JogInt32>())->init( t, context, var_info, rhs );
          case TOKEN_DIV_ASSIGN:
            return (new JogCmdDivAssignPropertyInteger<JogInt32>())->init( t, context, var_info, rhs );
          case TOKEN_MOD_ASSIGN:
            return (new JogCmdModAssignProperty<JogInt32>())->init( t, context, var_info, rhs );
          case TOKEN_AND_ASSIGN:
            return (new JogCmdAndAssignProperty<JogInt32>())->init( t, context, var_info, rhs );
          case TOKEN_OR_ASSIGN:
            return (new JogCmdOrAssignProperty<JogInt32>())->init( t, context, var_info, rhs );
          case TOKEN_XOR_ASSIGN:
            return (new JogCmdXorAssignProperty<JogInt32>())->init( t, context, var_info, rhs );
          case TOKEN_SHL_ASSIGN:
            return (new JogCmdSHLAssignProperty<JogInt32>())->init( t, context, var_info, rhs );
          case TOKEN_SHR_ASSIGN:
            return (new JogCmdSHRAssignProperty<JogInt32>())->init( t, context, var_info, rhs );
          case TOKEN_SHRX_ASSIGN:
            return (new JogCmdSHRXAssignProperty<JogInt32>())->init( t, context, var_info, rhs );
        }
      }
      else if (var_info->type == jog_type_manager.type_int16)
      {
        switch (op_type)
        {
          case TOKEN_ADD_ASSIGN:
            return (new JogCmdAddAssignPropertyInteger<JogInt16>())->init( t, context, var_info, rhs );
          case TOKEN_SUB_ASSIGN:
            return (new JogCmdSubAssignPropertyInteger<JogInt16>())->init( t, context, var_info, rhs );
          case TOKEN_MUL_ASSIGN:
            return (new JogCmdMulAssignPropertyInteger<JogInt16>())->init( t, context, var_info, rhs );
          case TOKEN_DIV_ASSIGN:
            return (new JogCmdDivAssignPropertyInteger<JogInt16>())->init( t, context, var_info, rhs );
          case TOKEN_MOD_ASSIGN:
            return (new JogCmdModAssignProperty<JogInt16>())->init( t, context, var_info, rhs );
          case TOKEN_AND_ASSIGN:
            return (new JogCmdAndAssignProperty<JogInt16>())->init( t, context, var_info, rhs );
          case TOKEN_OR_ASSIGN:
            return (new JogCmdOrAssignProperty<JogInt16>())->init( t, context, var_info, rhs );
          case TOKEN_XOR_ASSIGN:
            return (new JogCmdXorAssignProperty<JogInt16>())->init( t, context, var_info, rhs );
          case TOKEN_SHL_ASSIGN:
            return (new JogCmdSHLAssignProperty<JogInt16>())->init( t, context, var_info, rhs );
          case TOKEN_SHR_ASSIGN:
            return (new JogCmdSHRAssignProperty<JogInt16>())->init( t, context, var_info, rhs );
          case TOKEN_SHRX_ASSIGN:
            return (new JogCmdSHRXAssignProperty<JogInt16>())->init( t, context, var_info, rhs );
        }
      }
      else if (var_info->type == jog_type_manager.type_int8)
      {
        switch (op_type)
        {
          case TOKEN_ADD_ASSIGN:
            return (new JogCmdAddAssignPropertyInteger<JogInt8>())->init( t, context, var_info, rhs );
          case TOKEN_SUB_ASSIGN:
            return (new JogCmdSubAssignPropertyInteger<JogInt8>())->init( t, context, var_info, rhs );
          case TOKEN_MUL_ASSIGN:
            return (new JogCmdMulAssignPropertyInteger<JogInt8>())->init( t, context, var_info, rhs );
          case TOKEN_DIV_ASSIGN:
            return (new JogCmdDivAssignPropertyInteger<JogInt8>())->init( t, context, var_info, rhs );
          case TOKEN_MOD_ASSIGN:
            return (new JogCmdModAssignProperty<JogInt8>())->init( t, context, var_info, rhs );
          case TOKEN_AND_ASSIGN:
            return (new JogCmdAndAssignProperty<JogInt8>())->init( t, context, var_info, rhs );
          case TOKEN_OR_ASSIGN:
            return (new JogCmdOrAssignProperty<JogInt8>())->init( t, context, var_info, rhs );
          case TOKEN_XOR_ASSIGN:
            return (new JogCmdXorAssignProperty<JogInt8>())->init( t, context, var_info, rhs );
          case TOKEN_SHL_ASSIGN:
            return (new JogCmdSHLAssignProperty<JogInt8>())->init( t, context, var_info, rhs );
          case TOKEN_SHR_ASSIGN:
            return (new JogCmdSHRAssignProperty<JogInt8>())->init( t, context, var_info, rhs );
          case TOKEN_SHRX_ASSIGN:
            return (new JogCmdSHRXAssignProperty<JogInt8>())->init( t, context, var_info, rhs );
        }
      }
      else if (var_info->type == jog_type_manager.type_char)
      {
        switch (op_type)
        {
          case TOKEN_ADD_ASSIGN:
            return (new JogCmdAddAssignPropertyInteger<JogChar>())->init( t, context, var_info, rhs );
          case TOKEN_SUB_ASSIGN:
            return (new JogCmdSubAssignPropertyInteger<JogChar>())->init( t, context, var_info, rhs );
          case TOKEN_MUL_ASSIGN:
            return (new JogCmdMulAssignPropertyInteger<JogChar>())->init( t, context, var_info, rhs );
          case TOKEN_DIV_ASSIGN:
            return (new JogCmdDivAssignPropertyInteger<JogChar>())->init( t, context, var_info, rhs );
          case TOKEN_MOD_ASSIGN:
            return (new JogCmdModAssignProperty<JogChar>())->init( t, context, var_info, rhs );
          case TOKEN_AND_ASSIGN:
            return (new JogCmdAndAssignProperty<JogChar>())->init( t, context, var_info, rhs );
          case TOKEN_OR_ASSIGN:
            return (new JogCmdOrAssignProperty<JogChar>())->init( t, context, var_info, rhs );
          case TOKEN_XOR_ASSIGN:
            return (new JogCmdXorAssignProperty<JogChar>())->init( t, context, var_info, rhs );
          case TOKEN_SHL_ASSIGN:
            return (new JogCmdSHLAssignProperty<JogChar>())->init( t, context, var_info, rhs );
          case TOKEN_SHR_ASSIGN:
            return (new JogCmdSHRAssignProperty<JogChar>())->init( t, context, var_info, rhs );
          case TOKEN_SHRX_ASSIGN:
            return (new JogCmdSHRXAssignProperty<JogChar>())->init( t, context, var_info, rhs );
        }
      }
      else
      {
        switch (op_type)
        {
          case TOKEN_ADD_ASSIGN:
            throw error( "Boolean values cannot be added." );
          case TOKEN_SUB_ASSIGN:
            throw error( "Boolean values cannot be subtracted." );
          case TOKEN_MUL_ASSIGN:
            throw error( "Boolean values cannot be multiplied." );
          case TOKEN_DIV_ASSIGN:
            throw error( "Boolean values cannot be divided." );
          case TOKEN_MOD_ASSIGN:
            throw error( "Modulo can only be used on integer values." );
          case TOKEN_AND_ASSIGN:
            return (new JogCmdAndAssignProperty<JogInt64>())->init( t, context, var_info, rhs );
          case TOKEN_OR_ASSIGN:
            return (new JogCmdOrAssignProperty<JogInt64>())->init( t, context, var_info, rhs );
          case TOKEN_XOR_ASSIGN:
          case TOKEN_SHL_ASSIGN:
          case TOKEN_SHR_ASSIGN:
          case TOKEN_SHRX_ASSIGN:
            throw error( "Boolean values cannot be bit-shifted." );
        }
      }
    }

    var_info = context_type->class_properties_by_name[name];
    if (var_info) return resolve_op_assign( op_type, context_type, context, rhs );
  }

  return JogCmd::resolve_op_assign(op_type,context,rhs);
}

Ref<JogCmd> JogCmdIdentifier::resolve_op_assign( int op_type, JogTypeInfo* class_context,
    Ref<JogCmd> context, Ref<JogCmd> rhs )
{
  JogPropertyInfo* var_info = class_context->class_properties_by_name[name];

  if (var_info)
  {
    rhs = rhs->resolve();

    if (op_type == TOKEN_ADD_ASSIGN && var_info->type->is_reference())
    {
      if ( !var_info->type->instance_of(jog_type_manager.type_string) )
      {
        throw error( "'+=' can only be used with numerical and String values." );
      }

      JogTypeInfo* rhs_type = rhs->require_value();
      if (rhs_type->is_primitive())
      {
        Ref<JogCmdList> args = new JogCmdList(t);
        args->add(rhs);
        JogMethodInfo* m = resolve_call( t, rhs_type->wrapper_type(), 
            new JogString("toString"), *args, false );
        rhs = (new JogCmdClassCall( t, m, NULL, args ))->resolve();
      }
      return (new JogCmdAddAssignClassPropertyString())->init( t, context, var_info, rhs );
    }

    rhs = rhs->cast_to_type( var_info->type )->resolve();

    if (var_info->type == jog_type_manager.type_real64)
    {
      switch (op_type)
      {
        case TOKEN_ADD_ASSIGN:
          return (new JogCmdAddAssignClassPropertyReal<double>())->init( t, context, var_info, rhs );
        case TOKEN_SUB_ASSIGN:
          return (new JogCmdSubAssignClassPropertyReal<double>())->init( t, context, var_info, rhs );
        case TOKEN_MUL_ASSIGN:
          return (new JogCmdMulAssignClassPropertyReal<double>())->init( t, context, var_info, rhs );
        case TOKEN_DIV_ASSIGN:
          return (new JogCmdDivAssignClassPropertyReal<double>())->init( t, context, var_info, rhs );
        case TOKEN_MOD_ASSIGN:
          throw error( "Modulo can only be used on integer values." );
        case TOKEN_AND_ASSIGN:
          throw error( "Bitwise And can only be used on integer and boolean values." );
        case TOKEN_OR_ASSIGN:
          throw error( "Bitwise Or can only be used on integer and boolean values." );
        case TOKEN_XOR_ASSIGN:
          throw error( "Bitwise Xor can only be used on integer and boolean values." );
        case TOKEN_SHL_ASSIGN:
          throw error( "Left Shift can only be used on integer values." );
        case TOKEN_SHR_ASSIGN:
          throw error( "Right Shift can only be used on integer values." );
        case TOKEN_SHRX_ASSIGN:
          throw error( "Right Shift with Sign Extend can only be used on integer values." );
      }
    }
    else if (var_info->type == jog_type_manager.type_real32)
    {
      switch (op_type)
      {
        case TOKEN_ADD_ASSIGN:
          return (new JogCmdAddAssignClassPropertyReal<float>())->init( t, context, var_info, rhs );
        case TOKEN_SUB_ASSIGN:
          return (new JogCmdSubAssignClassPropertyReal<float>())->init( t, context, var_info, rhs );
        case TOKEN_MUL_ASSIGN:
          return (new JogCmdMulAssignClassPropertyReal<float>())->init( t, context, var_info, rhs );
        case TOKEN_DIV_ASSIGN:
          return (new JogCmdDivAssignClassPropertyReal<float>())->init( t, context, var_info, rhs );
        case TOKEN_MOD_ASSIGN:
          throw error( "Modulo can only be used on integer values." );
        case TOKEN_AND_ASSIGN:
          throw error( "Bitwise And can only be used on integer and boolean values." );
        case TOKEN_OR_ASSIGN:
          throw error( "Bitwise Or can only be used on integer and boolean values." );
        case TOKEN_XOR_ASSIGN:
          throw error( "Bitwise Xor can only be used on integer and boolean values." );
        case TOKEN_SHL_ASSIGN:
          throw error( "Left Shift can only be used on integer values." );
        case TOKEN_SHR_ASSIGN:
          throw error( "Right Shift can only be used on integer values." );
        case TOKEN_SHRX_ASSIGN:
          throw error( "Right Shift with Sign Extend can only be used on integer values." );
      }
    }
    else if (var_info->type == jog_type_manager.type_int64)
    {
      switch (op_type)
      {
        case TOKEN_ADD_ASSIGN:
          return (new JogCmdAddAssignClassPropertyInteger<JogInt64>())->init( t, context, var_info, rhs );
        case TOKEN_SUB_ASSIGN:
          return (new JogCmdSubAssignClassPropertyInteger<JogInt64>())->init( t, context, var_info, rhs );
        case TOKEN_MUL_ASSIGN:
          return (new JogCmdMulAssignClassPropertyInteger<JogInt64>())->init( t, context, var_info, rhs );
        case TOKEN_DIV_ASSIGN:
          return (new JogCmdDivAssignClassPropertyInteger<JogInt64>())->init( t, context, var_info, rhs );
        case TOKEN_MOD_ASSIGN:
          return (new JogCmdModAssignClassProperty<JogInt64>())->init( t, context, var_info, rhs );
        case TOKEN_AND_ASSIGN:
          return (new JogCmdAndAssignClassProperty<JogInt64>())->init( t, context, var_info, rhs );
        case TOKEN_OR_ASSIGN:
          return (new JogCmdOrAssignClassProperty<JogInt64>())->init( t, context, var_info, rhs );
        case TOKEN_XOR_ASSIGN:
          return (new JogCmdXorAssignClassProperty<JogInt64>())->init( t, context, var_info, rhs );
        case TOKEN_SHL_ASSIGN:
          return (new JogCmdSHLAssignClassProperty<JogInt64>())->init( t, context, var_info, rhs );
        case TOKEN_SHR_ASSIGN:
          return (new JogCmdSHRAssignClassProperty<JogInt64>())->init( t, context, var_info, rhs );
        case TOKEN_SHRX_ASSIGN:
          return (new JogCmdSHRXAssignClassProperty<JogInt64>())->init( t, context, var_info, rhs );
      }
    }
    else if (var_info->type == jog_type_manager.type_int32)
    {
      switch (op_type)
      {
        case TOKEN_ADD_ASSIGN:
          return (new JogCmdAddAssignClassPropertyInteger<JogInt32>())->init( t, context, var_info, rhs );
        case TOKEN_SUB_ASSIGN:
          return (new JogCmdSubAssignClassPropertyInteger<JogInt32>())->init( t, context, var_info, rhs );
        case TOKEN_MUL_ASSIGN:
          return (new JogCmdMulAssignClassPropertyInteger<JogInt32>())->init( t, context, var_info, rhs );
        case TOKEN_DIV_ASSIGN:
          return (new JogCmdDivAssignClassPropertyInteger<JogInt32>())->init( t, context, var_info, rhs );
        case TOKEN_MOD_ASSIGN:
          return (new JogCmdModAssignClassProperty<JogInt32>())->init( t, context, var_info, rhs );
        case TOKEN_AND_ASSIGN:
          return (new JogCmdAndAssignClassProperty<JogInt32>())->init( t, context, var_info, rhs );
        case TOKEN_OR_ASSIGN:
          return (new JogCmdOrAssignClassProperty<JogInt32>())->init( t, context, var_info, rhs );
        case TOKEN_XOR_ASSIGN:
          return (new JogCmdXorAssignClassProperty<JogInt32>())->init( t, context, var_info, rhs );
        case TOKEN_SHL_ASSIGN:
          return (new JogCmdSHLAssignClassProperty<JogInt32>())->init( t, context, var_info, rhs );
        case TOKEN_SHR_ASSIGN:
          return (new JogCmdSHRAssignClassProperty<JogInt32>())->init( t, context, var_info, rhs );
        case TOKEN_SHRX_ASSIGN:
          return (new JogCmdSHRXAssignClassProperty<JogInt32>())->init( t, context, var_info, rhs );
      }
    }
    else if (var_info->type == jog_type_manager.type_int16)
    {
      switch (op_type)
      {
        case TOKEN_ADD_ASSIGN:
          return (new JogCmdAddAssignClassPropertyInteger<JogInt16>())->init( t, context, var_info, rhs );
        case TOKEN_SUB_ASSIGN:
          return (new JogCmdSubAssignClassPropertyInteger<JogInt16>())->init( t, context, var_info, rhs );
        case TOKEN_MUL_ASSIGN:
          return (new JogCmdMulAssignClassPropertyInteger<JogInt16>())->init( t, context, var_info, rhs );
        case TOKEN_DIV_ASSIGN:
          return (new JogCmdDivAssignClassPropertyInteger<JogInt16>())->init( t, context, var_info, rhs );
        case TOKEN_MOD_ASSIGN:
          return (new JogCmdModAssignClassProperty<JogInt16>())->init( t, context, var_info, rhs );
        case TOKEN_AND_ASSIGN:
          return (new JogCmdAndAssignClassProperty<JogInt16>())->init( t, context, var_info, rhs );
        case TOKEN_OR_ASSIGN:
          return (new JogCmdOrAssignClassProperty<JogInt16>())->init( t, context, var_info, rhs );
        case TOKEN_XOR_ASSIGN:
          return (new JogCmdXorAssignClassProperty<JogInt16>())->init( t, context, var_info, rhs );
        case TOKEN_SHL_ASSIGN:
          return (new JogCmdSHLAssignClassProperty<JogInt16>())->init( t, context, var_info, rhs );
        case TOKEN_SHR_ASSIGN:
          return (new JogCmdSHRAssignClassProperty<JogInt16>())->init( t, context, var_info, rhs );
        case TOKEN_SHRX_ASSIGN:
          return (new JogCmdSHRXAssignClassProperty<JogInt16>())->init( t, context, var_info, rhs );
      }
    }
    else if (var_info->type == jog_type_manager.type_int8)
    {
      switch (op_type)
      {
        case TOKEN_ADD_ASSIGN:
          return (new JogCmdAddAssignClassPropertyInteger<JogInt8>())->init( t, context, var_info, rhs );
        case TOKEN_SUB_ASSIGN:
          return (new JogCmdSubAssignClassPropertyInteger<JogInt8>())->init( t, context, var_info, rhs );
        case TOKEN_MUL_ASSIGN:
          return (new JogCmdMulAssignClassPropertyInteger<JogInt8>())->init( t, context, var_info, rhs );
        case TOKEN_DIV_ASSIGN:
          return (new JogCmdDivAssignClassPropertyInteger<JogInt8>())->init( t, context, var_info, rhs );
        case TOKEN_MOD_ASSIGN:
          return (new JogCmdModAssignClassProperty<JogInt8>())->init( t, context, var_info, rhs );
        case TOKEN_AND_ASSIGN:
          return (new JogCmdAndAssignClassProperty<JogInt8>())->init( t, context, var_info, rhs );
        case TOKEN_OR_ASSIGN:
          return (new JogCmdOrAssignClassProperty<JogInt8>())->init( t, context, var_info, rhs );
        case TOKEN_XOR_ASSIGN:
          return (new JogCmdXorAssignClassProperty<JogInt8>())->init( t, context, var_info, rhs );
        case TOKEN_SHL_ASSIGN:
          return (new JogCmdSHLAssignClassProperty<JogInt8>())->init( t, context, var_info, rhs );
        case TOKEN_SHR_ASSIGN:
          return (new JogCmdSHRAssignClassProperty<JogInt8>())->init( t, context, var_info, rhs );
        case TOKEN_SHRX_ASSIGN:
          return (new JogCmdSHRXAssignClassProperty<JogInt8>())->init( t, context, var_info, rhs );
      }
    }
    else if (var_info->type == jog_type_manager.type_char)
    {
      switch (op_type)
      {
        case TOKEN_ADD_ASSIGN:
          return (new JogCmdAddAssignClassPropertyInteger<JogChar>())->init( t, context, var_info, rhs );
        case TOKEN_SUB_ASSIGN:
          return (new JogCmdSubAssignClassPropertyInteger<JogChar>())->init( t, context, var_info, rhs );
        case TOKEN_MUL_ASSIGN:
          return (new JogCmdMulAssignClassPropertyInteger<JogChar>())->init( t, context, var_info, rhs );
        case TOKEN_DIV_ASSIGN:
          return (new JogCmdDivAssignClassPropertyInteger<JogChar>())->init( t, context, var_info, rhs );
        case TOKEN_MOD_ASSIGN:
          return (new JogCmdModAssignClassProperty<JogChar>())->init( t, context, var_info, rhs );
        case TOKEN_AND_ASSIGN:
          return (new JogCmdAndAssignClassProperty<JogChar>())->init( t, context, var_info, rhs );
        case TOKEN_OR_ASSIGN:
          return (new JogCmdOrAssignClassProperty<JogChar>())->init( t, context, var_info, rhs );
        case TOKEN_XOR_ASSIGN:
          return (new JogCmdXorAssignClassProperty<JogChar>())->init( t, context, var_info, rhs );
        case TOKEN_SHL_ASSIGN:
          return (new JogCmdSHLAssignClassProperty<JogChar>())->init( t, context, var_info, rhs );
        case TOKEN_SHR_ASSIGN:
          return (new JogCmdSHRAssignClassProperty<JogChar>())->init( t, context, var_info, rhs );
        case TOKEN_SHRX_ASSIGN:
          return (new JogCmdSHRXAssignClassProperty<JogChar>())->init( t, context, var_info, rhs );
      }
    }
    else
    {
      switch (op_type)
      {
        case TOKEN_ADD_ASSIGN:
          throw error( "Boolean values cannot be added." );
        case TOKEN_SUB_ASSIGN:
          throw error( "Boolean values cannot be subtracted." );
        case TOKEN_MUL_ASSIGN:
          throw error( "Boolean values cannot be multiplied." );
        case TOKEN_DIV_ASSIGN:
          throw error( "Boolean values cannot be divided." );
        case TOKEN_MOD_ASSIGN:
          throw error( "Modulo can only be used on integer values." );
        case TOKEN_AND_ASSIGN:
          return (new JogCmdAndAssignClassProperty<JogInt64>())->init( t, context, var_info, rhs );
        case TOKEN_OR_ASSIGN:
          return (new JogCmdOrAssignClassProperty<JogInt64>())->init( t, context, var_info, rhs );
        case TOKEN_XOR_ASSIGN:
        case TOKEN_SHL_ASSIGN:
        case TOKEN_SHR_ASSIGN:
        case TOKEN_SHRX_ASSIGN:
          throw error( "Boolean values cannot be bit-shifted." );
      }
    }
  }

  return JogCmd::resolve_op_assign(op_type,class_context,context,rhs);
}


Ref<JogCmd> JogCmdIdentifier::resolve_stepcount_access( int when, int modifier )
{
  JogLocalVarInfo* var_info = jog_context->find_local(name);

  if (var_info)
  {
    if (var_info->type->is_reference())
    {
      throw error( "++/-- cannot be used on references." );
    }
    else if (when == 0)
    {
      if (var_info->type == jog_type_manager.type_real64)
      {
        return (new JogCmdPreStepLocalReal<double>())->init( t, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_real32)
      {
        return (new JogCmdPreStepLocalReal<float>())->init( t, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int64)
      {
        return (new JogCmdPreStepLocalInteger<JogInt64>())->init( t, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int32)
      {
        return (new JogCmdPreStepLocalInteger<JogInt32>())->init( t, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int16)
      {
        return (new JogCmdPreStepLocalInteger<JogInt16>())->init( t, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int8)
      {
        return (new JogCmdPreStepLocalInteger<JogInt8>())->init( t, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_char)
      {
        return (new JogCmdPreStepLocalInteger<JogChar>())->init( t, var_info, modifier );
      }
      else
      {
        throw error( "++/-- cannot be used with boolean variables." );
      }
    }
    else
    {
      if (var_info->type == jog_type_manager.type_real64)
      {
        return (new JogCmdPostStepLocalReal<double>())->init( t, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_real32)
      {
        return (new JogCmdPostStepLocalReal<float>())->init( t, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int64)
      {
        return (new JogCmdPostStepLocalInteger<JogInt64>())->init( t, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int32)
      {
        return (new JogCmdPostStepLocalInteger<JogInt32>())->init( t, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int16)
      {
        return (new JogCmdPostStepLocalInteger<JogInt16>())->init( t, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int8)
      {
        return (new JogCmdPostStepLocalInteger<JogInt8>())->init( t, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_char)
      {
        return (new JogCmdPostStepLocalInteger<JogChar>())->init( t, var_info, modifier );
      }
      else
      {
        throw error( "++/-- cannot be used with boolean variables." );
      }
    }
  }

  if (jog_context->this_type->class_properties_by_name.contains(name))
  {
    return resolve_stepcount_access( when, modifier, jog_context->this_type, NULL );
  }
  else if (jog_context->this_type->properties_by_name.contains(name))
  {
    return resolve_stepcount_access( when, modifier, new JogCmdThis(t,jog_context->this_type) );
  }
  else
  {
    Ref<JogString> mesg = new JogString( "No such variable '" );
    mesg->add(name);
    mesg->add("'.");
    throw t->error( mesg->to_ascii() );
  }
}

Ref<JogCmd> JogCmdIdentifier::resolve_stepcount_access( int when, int modifier, 
    JogTypeInfo* class_context, Ref<JogCmd> context )
{
  JogPropertyInfo* var_info = class_context->class_properties_by_name[name];

  if (var_info)
  {
    if (var_info->type->is_reference())
    {
      throw error( "++/-- cannot be used on references." );
    }
    else if (when == 0)
    {
      if (var_info->type == jog_type_manager.type_real64)
      {
        return (new JogCmdPreStepClassPropertyReal<double>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_real32)
      {
        return (new JogCmdPreStepClassPropertyReal<float>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int64)
      {
        return (new JogCmdPreStepClassPropertyInteger<JogInt64>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int32)
      {
        return (new JogCmdPreStepClassPropertyInteger<JogInt32>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int16)
      {
        return (new JogCmdPreStepClassPropertyInteger<JogInt16>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int8)
      {
        return (new JogCmdPreStepClassPropertyInteger<JogInt8>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_char)
      {
        return (new JogCmdPreStepClassPropertyInteger<JogChar>())->init( t, context, var_info, modifier );
      }
      else
      {
        throw error( "++/-- cannot be used with boolean variables." );
      }
    }
    else
    {
      if (var_info->type == jog_type_manager.type_real64)
      {
        return (new JogCmdPostStepClassPropertyReal<double>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_real32)
      {
        return (new JogCmdPostStepClassPropertyReal<float>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int64)
      {
        return (new JogCmdPostStepClassPropertyInteger<JogInt64>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int32)
      {
        return (new JogCmdPostStepClassPropertyInteger<JogInt32>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int16)
      {
        return (new JogCmdPostStepClassPropertyInteger<JogInt16>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int8)
      {
        return (new JogCmdPostStepClassPropertyInteger<JogInt8>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_char)
      {
        return (new JogCmdPostStepClassPropertyInteger<JogChar>())->init( t, context, var_info, modifier );
      }
      else
      {
        throw error( "++/-- cannot be used with boolean variables." );
      }
    }
  }

  Ref<JogString> mesg = new JogString( "No such class property '" );
  mesg->add(name);
  mesg->add("'.");
  throw t->error( mesg->to_ascii() );
}


Ref<JogCmd> JogCmdIdentifier::resolve_stepcount_access( int when, int modifier, 
    Ref<JogCmd> context )
{
  JogPropertyInfo* var_info = context->type()->properties_by_name[name];

  if (var_info)
  {
    if (var_info->type->is_reference())
    {
      throw error( "++/-- cannot be used on references." );
    }
    else if (when == 0)
    {
      if (var_info->type == jog_type_manager.type_real64)
      {
        return (new JogCmdPreStepPropertyReal<double>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_real32)
      {
        return (new JogCmdPreStepPropertyReal<float>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int64)
      {
        return (new JogCmdPreStepPropertyInteger<JogInt64>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int32)
      {
        return (new JogCmdPreStepPropertyInteger<JogInt32>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int16)
      {
        return (new JogCmdPreStepPropertyInteger<JogInt16>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int8)
      {
        return (new JogCmdPreStepPropertyInteger<JogInt8>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_char)
      {
        return (new JogCmdPreStepPropertyInteger<JogChar>())->init( t, context, var_info, modifier );
      }
      else
      {
        throw error( "++/-- cannot be used with boolean variables." );
      }
    }
    else
    {
      if (var_info->type == jog_type_manager.type_real64)
      {
        return (new JogCmdPostStepPropertyReal<double>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_real32)
      {
        return (new JogCmdPostStepPropertyReal<float>())->init( t, context, var_info, modifier );
      }
      else if (var_info->type == jog_type_manager.type_int64)
      {
        return (new JogCmdPostStepPropertyInteger<JogInt64>())->init( t, context, var_info, 
            modifier );
      }
      else if (var_info->type == jog_type_manager.type_int32)
      {
        return (new JogCmdPostStepPropertyInteger<JogInt16>())->init( t, context, var_info, 
            modifier );
      }
      else if (var_info->type == jog_type_manager.type_int16)
      {
        return (new JogCmdPostStepPropertyInteger<JogInt32>())->init( t, context, var_info, 
            modifier );
      }
      else if (var_info->type == jog_type_manager.type_int8)
      {
        return (new JogCmdPostStepPropertyInteger<JogInt8>())->init( t, context, var_info, 
            modifier );
      }
      else if (var_info->type == jog_type_manager.type_char)
      {
        return (new JogCmdPostStepPropertyInteger<JogChar>())->init( t, context, var_info, 
            modifier );
      }
      else
      {
        throw error( "++/-- cannot be used with boolean variables." );
      }
    }
  }

  Ref<JogString> mesg = new JogString( "No such property '" );
  mesg->add(name);
  mesg->add("'.");
  throw t->error( mesg->to_ascii() );
}


Ref<JogCmd> JogCmdLocalVarDeclaration::resolve()
{
  of_type->resolve();
  var_info = new JogLocalVarInfo( t, of_type, name );
  jog_context->add( var_info );
  if (*initial_value)
  {
    Ref<JogCmd> result = (new JogCmdAssign( t, new JogCmdIdentifier(t,name), 
          initial_value ))->resolve();
    result->require_value();
    return result->discarding_result();
  }
  else
  {
    // TODO: remove this in favor of code that detects reading from unitialized vars
    if (of_type->is_reference())
    {
      return (new JogCmdAssign( t, new JogCmdIdentifier(t,name), new JogCmdNullRef(t) ))->resolve();
    }
    else
    {
      return (new JogCmdAssign( t, new JogCmdIdentifier(t,name), new JogCmdLiteralInt32(t,0) ))->resolve();
    }
  }
}

Ref<JogCmd> JogCmdAssert::resolve()
{
  if (resolved) return this;
  resolved = true;

  expression = expression->resolve();
  expression->require_boolean();

  return this;
}


Ref<JogCmd> JogCmdBlock::resolve()
{
  int old_local_count = jog_context->locals.count;
  statements->resolve();
  jog_context->locals.discard_from(old_local_count);
  return this;
}

Ref<JogCmd> JogCmdIf::resolve()
{
  expression = expression->resolve();
  expression->require_boolean();

  if (*body)
  {
    int old_local_count = jog_context->locals.count;
    body = body->resolve();
    jog_context->locals.discard_from(old_local_count);
  }

  if (*else_body)
  {
    int old_local_count = jog_context->locals.count;
    else_body = else_body->resolve();
    jog_context->locals.discard_from(old_local_count);
  }
  return this;
}

Ref<JogCmd> JogCmdWhile::resolve()
{
  expression = expression->resolve();
  expression->require_boolean();

  if (*body)
  {
    int old_local_count = jog_context->locals.count;
    body = body->resolve();
    jog_context->locals.discard_from(old_local_count);
  }

  return this;
}

Ref<JogCmd> JogCmdFor::resolve()
{
  int old_local_count = jog_context->locals.count;

  if (*initialization) initialization = initialization->resolve()->discarding_result();
  condition = condition->resolve();
  condition->require_boolean();
  if (*var_mod) var_mod = var_mod->resolve()->discarding_result();

  if (*body) body = body->resolve();

  jog_context->locals.discard_from(old_local_count);

  return this;
}

Ref<JogCmd> JogCmdForEach::resolve()
{
  Ref<JogString> iter_name = new JogString("_");
  iter_name->add( local_name );
  iter_name->add( "_iterator" );

  Ref<JogCmd> create_iter_call = new JogCmdMemberAccess( t,
        iterable_expr,
        new JogCmdMethodCall( t, new JogString("iterator"), new JogCmdList(t) )
      );
  create_iter_call = create_iter_call->resolve();
  JogTypeInfo* iter_type = create_iter_call->require_value();

  Ref<JogCmdBlock> commands = new JogCmdBlock(t);

  Ref<JogCmdLocalVarDeclaration> iter_decl;
  iter_decl = new JogCmdLocalVarDeclaration( t, iter_type, iter_name );
  iter_decl->initial_value = create_iter_call;
  commands->add( *iter_decl );

  Ref<JogCmdLocalVarDeclaration> assign_local;
  assign_local = new JogCmdLocalVarDeclaration( t, local_type, local_name );
  assign_local->initial_value = new JogCmdMemberAccess( t,
        new JogCmdIdentifier(t,iter_name),
        new JogCmdMethodCall( t, new JogString("next"), new JogCmdList(t) )
      );

  Ref<JogCmdWhile> while_loop = new JogCmdWhile( t,
      new JogCmdMemberAccess( t,
        new JogCmdIdentifier(t,iter_name),
        new JogCmdMethodCall( t, new JogString("hasNext"), new JogCmdList(t) )
      ) );

  Ref<JogCmdBlock> while_body = new JogCmdBlock(t);
  while_body->add( assign_local->discarding_result() );
  while_body->add( body );

  while_loop->body = *while_body;

  commands->add( *while_loop );

  return commands->resolve();
}

Ref<JogCmd> JogCmdMethodCall::resolve()
{
  if (name->equals("this"))
  {
    if ( !jog_context->this_method->is_constructor() )
    {
      throw error( "this() call is only valid in a constructor." );
    }

    if (jog_context->this_method->statements->commands[0] != this)
    {
      throw error( "this() call must be the first statement." );
    }

    JogMethodInfo* m = resolve_call( t, jog_context->this_type, new JogString("<init>"), *args );

    return new JogCmdStaticCall( t, m, new JogCmdThis(t,jog_context->this_type), args );
  }

  if (jog_context->this_method->is_static())
  {
    return resolve( jog_context->this_type, NULL );
  }
  else
  {
    return resolve( new JogCmdThis(t,jog_context->this_type) );
  }
};

Ref<JogCmd> JogCmdMethodCall::resolve( Ref<JogCmd> context )
{
  JogMethodInfo* m = resolve_call( t, context->type(), name, *args );

  if (m->is_static())
  {
    return new JogCmdClassCall( t, m, context, args );
  }
  else
  {
    return new JogCmdDynamicCall( t, m, context, args );
  }
};

Ref<JogCmd> JogCmdMethodCall::resolve( JogTypeInfo* class_context, Ref<JogCmd> context )
{
  JogMethodInfo* m = resolve_call( t, class_context, name, *args, false );
  return new JogCmdClassCall( t, m, context, args );
};

Ref<JogCmd> JogCmdSuperCall::resolve()
{
  if (jog_context->this_method->is_static())
  {
    throw error( "Illegal super() call in static method." );
  }
  else
  {
    JogTypeInfo* base_class = jog_context->this_type->base_class;
    if ( !base_class )
    {
      throw error( "Current class does not have a superclass." );
    }

    JogMethodInfo* m = resolve_call( t, base_class, name, *args );

    if (m->is_static())
    {
      throw error( "Illegal superclass call to a static method." );
    }
    else if (m->is_abstract())
    {
      throw error( "Illegal superclass call to an abstract method." );
    }
    else
    {
      return new JogCmdStaticCall( t, m, new JogCmdThis(t,base_class), args );
    }
  }
};

Ref<JogCmd> JogCmdSuperCall::resolve( Ref<JogCmd> context )
{
  throw error("[Internal] JogCmdSuperCall" );
};

Ref<JogCmd> JogCmdSuperCall::resolve( JogTypeInfo* class_context, Ref<JogCmd> context )
{
  throw error("[Internal] JogCmdSuperCall" );
};

Ref<JogCmd> JogCmdNewObject::resolve()
{
  if (method_info) return this;

  of_type->resolve();
  if (of_type->is_abstract())
  {
    throw t->error( "Cannot create an instance of an abstract class." );
  }

  method_info = resolve_call( t, of_type, new JogString("<init>"), *args );

  return this;
};

Ref<JogCmd> JogCmdNewArray::resolve()
{
  if (resolved) return this;
  resolved = true;

  of_type->resolve();
  size_expr = size_expr->resolve();

  if (size_expr->require_integer() != jog_type_manager.type_int32)
  {
    throw error( "'int' value expected." );
  }

  if (*element_expr)
  {
    element_expr = element_expr->resolve();
  }

  return this;
};

Ref<JogCmd> JogCmdLiteralArray::resolve()
{
  if (resolved) return this;
  resolved = true;

  of_type->resolve();
  if ( !of_type->is_array() )
  {
    throw error( "Array syntax used with a non-array datatype." );
  }

  terms->resolve();

  for (int i=0; i<terms->count(); ++i)
  {
    terms->commands[i]->require_value();
    terms->commands[i] = (terms->commands[i]->cast_to_type(of_type->element_type))->resolve();
  }

  JogTypeInfo* element_type = of_type->element_type;
  if (element_type->is_primitive())
  {
    if (element_type == jog_type_manager.type_real64)
    {
      return new JogCmdLiteralArrayPrimitive<double>( t, of_type, terms );
    }
    else if (element_type == jog_type_manager.type_real32)
    {
      return new JogCmdLiteralArrayPrimitive<float>( t, of_type, terms );
    }
    else if (element_type == jog_type_manager.type_int64)
    {
      return new JogCmdLiteralArrayPrimitive<JogInt64>( t, of_type, terms );
    }
    else if (element_type == jog_type_manager.type_int32)
    {
      return new JogCmdLiteralArrayPrimitive<JogInt32>( t, of_type, terms );
    }
    else if (element_type == jog_type_manager.type_int16)
    {
      return new JogCmdLiteralArrayPrimitive<JogInt16>( t, of_type, terms );
    }
    else if (element_type == jog_type_manager.type_int8)
    {
      return new JogCmdLiteralArrayPrimitive<JogInt8>( t, of_type, terms );
    }
    else if (element_type == jog_type_manager.type_char)
    {
      return new JogCmdLiteralArrayPrimitive<JogChar>( t, of_type, terms );
    }
    else
    {
      return new JogCmdLiteralArrayPrimitive<char>( t, of_type, terms );
    }
  }

  return this;
}

Ref<JogCmd> JogCmdPreIncrement::resolve()
{
  return operand->resolve_stepcount_access(0,1);
}

Ref<JogCmd> JogCmdPreDecrement::resolve()
{
  return operand->resolve_stepcount_access(0,-1);
}

Ref<JogCmd> JogCmdPostIncrement::resolve()
{
  return operand->resolve_stepcount_access(1,1);
}

Ref<JogCmd> JogCmdPostDecrement::resolve()
{
  return operand->resolve_stepcount_access(1,-1);
}

Ref<JogCmd> JogCmdNegate::resolve()
{
  operand = operand->resolve();
  JogTypeInfo* op_type = operand->require_primitive();

  if (operand->is_literal())
  {
    if (op_type == jog_type_manager.type_int64)
    {
      JogInt64 n = ((JogCmdLiteralInt64*) *operand)->value;
      return new JogCmdLiteralInt64( t, -n );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      int n = ((JogCmdLiteralInt32*) *operand)->value;
      return new JogCmdLiteralInt32( t, -n );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      JogInt16 n = ((JogCmdLiteralInt16*) *operand)->value;
      return new JogCmdLiteralInt16( t, -n );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      JogInt8 n = ((JogCmdLiteralInt8*) *operand)->value;
      return new JogCmdLiteralInt8( t, -n );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      JogChar n = ((JogCmdLiteralChar*) *operand)->value;
      return new JogCmdLiteralChar( t, -n );
    }
    else if (op_type == jog_type_manager.type_real64)
    {
      double n = ((JogCmdLiteralReal64*) *operand)->value;
      return new JogCmdLiteralReal64( t, -n );
    }
    else if (op_type == jog_type_manager.type_real32)
    {
      float n = ((JogCmdLiteralReal32*) *operand)->value;
      return new JogCmdLiteralReal32( t, -n );
    }
    else
    {
      throw error( "Cannot negate a boolean value." );
    }
  }
  else
  {
    if (op_type == jog_type_manager.type_int64)
    {
      return new JogCmdNegateInt64( t, operand );
    }
    else if (op_type == jog_type_manager.type_int32)
    {
      return new JogCmdNegateInt32( t, operand );
    }
    else if (op_type == jog_type_manager.type_int16)
    {
      return new JogCmdNegateInt16( t, operand );
    }
    else if (op_type == jog_type_manager.type_int8)
    {
      return new JogCmdNegateInt8( t, operand );
    }
    else if (op_type == jog_type_manager.type_char)
    {
      return new JogCmdNegateChar( t, operand );
    }
    else if (op_type == jog_type_manager.type_real64)
    {
      return new JogCmdNegateReal64( t, operand );
    }
    else if (op_type == jog_type_manager.type_real32)
    {
      return new JogCmdNegateReal32( t, operand );
    }
    else
    {
      throw error( "Cannot negate a boolean value." );
    }
  }
}

Ref<JogCmd> JogCmdLogicalNot::resolve()
{
  operand = operand->resolve();
  operand->require_boolean();

  if (operand->is_literal())
  {
    bool value = ((JogCmdLiteralBoolean*) *operand)->value;
    return new JogCmdLiteralBoolean( t, !value );
  }

  return this;
}

Ref<JogCmd> JogCmdBitwiseNot::resolve()
{
  operand = operand->resolve();
  operand->require_integer();
  return this;
}

Ref<JogCmd> JogCmdMemberAccess::resolve()
{
  JogTypeInfo* class_context = context->as_type();
  if (class_context) 
  {
    class_context->resolve();
    return member->resolve( class_context, NULL );
  }

  context = context->resolve();
  context->require_reference();
  return member->resolve( context );
}

Ref<JogCmd> JogCmdMemberAccess::resolve_assignment( Ref<JogCmd> assignment_context, 
    Ref<JogCmd> new_value )
{
  JogTypeInfo* class_context = context->as_type();
  if (class_context) 
  {
    class_context->resolve();
    return member->resolve_assignment( class_context, context, new_value );
  }

  return member->resolve_assignment( context->resolve(), new_value );
}

Ref<JogCmd> JogCmdMemberAccess::resolve_op_assign( int op_type, 
    Ref<JogCmd> assignment_context, Ref<JogCmd> rhs )
{
  if (*assignment_context != NULL)
  {
    throw error( "[Internal] JogCmdMemberAccess::resolve_assignment( !NULL, ... )" );
  }
  JogTypeInfo* class_context = context->as_type();
  if (class_context) 
  {
    class_context->resolve();
    return member->resolve_op_assign( op_type, class_context, context, rhs );
  }

  return member->resolve_op_assign( op_type, context->resolve(), rhs );
}

Ref<JogCmd> JogCmdMemberAccess::resolve_stepcount_access( int when, int modifier )
{
  JogTypeInfo* class_context = context->as_type();
  if (class_context) 
  {
    class_context->resolve();
    return member->resolve_stepcount_access( when, modifier, class_context, context );
  }
  return member->resolve_stepcount_access( when, modifier, context->resolve() );
}

Ref<JogCmd> JogCmdArrayAccess::resolve()
{
  context = context->resolve();
  JogTypeInfo* context_type = context->require_reference();
  if ( !context_type->is_array() )
  {
    throw context->error( "Array reference expected." );
  }

  index_expr = index_expr->resolve();
  index_expr->require_integer();
  index_expr = (index_expr->cast_to_type(jog_type_manager.type_int32))->resolve();

  JogTypeInfo* element_type = context_type->element_type;
  if (element_type->is_reference())
  {
    return new JogCmdArrayReadRef( t, context, index_expr );
  }
  else if (element_type == jog_type_manager.type_real64)
  {
    return new JogCmdArrayReadReal64( t, context, index_expr );
  }
  else if (element_type == jog_type_manager.type_real32)
  {
    return new JogCmdArrayReadReal32( t, context, index_expr );
  }
  else if (element_type == jog_type_manager.type_int64)
  {
    return new JogCmdArrayReadInt64( t, context, index_expr );
  }
  else if (element_type == jog_type_manager.type_int32)
  {
    return new JogCmdArrayReadInt32( t, context, index_expr );
  }
  else if (element_type == jog_type_manager.type_int16)
  {
    return new JogCmdArrayReadInt16( t, context, index_expr );
  }
  else if (element_type == jog_type_manager.type_int8)
  {
    return new JogCmdArrayReadInt8( t, context, index_expr );
  }
  else if (element_type == jog_type_manager.type_char)
  {
    return new JogCmdArrayReadChar( t, context, index_expr );
  }
  else
  {
    return new JogCmdArrayReadBoolean( t, context, index_expr );
  }
}

Ref<JogCmd> JogCmdArrayAccess::resolve_assignment( Ref<JogCmd> assignment_context, 
    Ref<JogCmd> new_value )
{
  if (*assignment_context != NULL)
  {
    throw error( "[Internal] JogCmdArrayAccess::resolve_assignment( !NULL, ... )" );
  }

  context = context->resolve();
  JogTypeInfo* context_type = context->require_value();
  if ( !context_type->is_array() )
  {
    throw context->error( "Array reference expected." );
  }

  index_expr = index_expr->resolve();
  index_expr->require_integer();
  index_expr = (index_expr->cast_to_type(jog_type_manager.type_int32))->resolve();

  new_value = new_value->resolve();
  JogTypeInfo* new_value_type = new_value->require_value();

  JogTypeInfo* common_type = jog_type_manager.find_common_type( *t, 
      context_type->element_type, new_value_type, false );
  if (common_type != context_type->element_type)
  {
    throw new_value->error( "Mismatched type in assignment." );
  }

  if (common_type != new_value_type)
  {
    new_value = new_value->cast_to_type(common_type)->resolve();
  }

  if (common_type->is_reference())
  {
    return new JogCmdArrayWriteRef( t, context, index_expr, new_value );
  }
  else if (common_type == jog_type_manager.type_real64)
  {
    return new JogCmdArrayWriteReal64( t, context, index_expr, new_value );
  }
  else if (common_type == jog_type_manager.type_real32)
  {
    return new JogCmdArrayWriteReal32( t, context, index_expr, new_value );
  }
  else if (common_type == jog_type_manager.type_int64)
  {
    return new JogCmdArrayWriteInt64( t, context, index_expr, new_value );
  }
  else if (common_type == jog_type_manager.type_int32)
  {
    return new JogCmdArrayWriteInt32( t, context, index_expr, new_value );
  }
  else if (common_type == jog_type_manager.type_int16)
  {
    return new JogCmdArrayWriteInt16( t, context, index_expr, new_value );
  }
  else if (common_type == jog_type_manager.type_int8)
  {
    return new JogCmdArrayWriteInt8( t, context, index_expr, new_value );
  }
  else if (common_type == jog_type_manager.type_char)
  {
    return new JogCmdArrayWriteChar( t, context, index_expr, new_value );
  }
  else
  {
    return new JogCmdArrayWriteBoolean( t, context, index_expr, new_value );
  }
}

Ref<JogCmd> JogCmdArrayAccess::resolve_op_assign( int op_type, Ref<JogCmd> assignment_context,
    Ref<JogCmd> rhs)
{
  if (*assignment_context != NULL)
  {
    throw error( "[Internal] JogCmdArrayAccess::resolve_assignment( !NULL, ... )" );
  }

  context = context->resolve();
  JogTypeInfo* context_type = context->require_value();
  if ( !context_type->is_array() )
  {
    throw context->error( "Array reference expected." );
  }

  index_expr = index_expr->resolve();
  index_expr->require_integer();
  index_expr = (index_expr->cast_to_type(jog_type_manager.type_int32))->resolve();

  rhs = rhs->resolve();

  JogTypeInfo* element_type = context_type->element_type;
  JogTypeInfo* rhs_type = rhs->require_value();

  if (element_type == jog_type_manager.type_string)
  {
    if (rhs_type->is_reference())
    {
      if (rhs_type == jog_type_manager.type_null)
      {
        rhs = new JogCmdLiteralString( t, new JogString("null") );
      }
      else if ( rhs_type != jog_type_manager.type_string )
      {
        rhs = (new JogCmdMemberAccess( t,
            rhs,
            new JogCmdMethodCall( t, new JogString("toString"), new JogCmdList(t) )
            ))->resolve();
      }
    }
    else
    {
      Ref<JogCmdList> args = new JogCmdList(t);
      args->add(rhs);
      JogMethodInfo* m = resolve_call( t, rhs_type->wrapper_type(), 
          new JogString("toString"), *args, false );
      rhs = new JogCmdClassCall( t, m, NULL, args );
    }
    return (new JogCmdAddAssignArrayString())->init( t, context, index_expr, rhs );
  }

  rhs = rhs->cast_to_type( element_type )->resolve();

  if (element_type == jog_type_manager.type_real64)
  {
    switch (op_type)
    {
      case TOKEN_ADD_ASSIGN:
        return (new JogCmdAddAssignArrayReal<double>())->init( t, context, index_expr, rhs );
      case TOKEN_SUB_ASSIGN:
        return (new JogCmdSubAssignArrayReal<double>())->init( t, context, index_expr, rhs );
      case TOKEN_MUL_ASSIGN:
        return (new JogCmdMulAssignArrayReal<double>())->init( t, context, index_expr, rhs );
      case TOKEN_DIV_ASSIGN:
        return (new JogCmdDivAssignArrayReal<double>())->init( t, context, index_expr, rhs );
      case TOKEN_MOD_ASSIGN:
        throw error( "Modulo can only be used on integer values." );
      case TOKEN_AND_ASSIGN:
        throw error( "Bitwise And can only be used on integer and boolean values." );
      case TOKEN_OR_ASSIGN:
        throw error( "Bitwise Or can only be used on integer and boolean values." );
      case TOKEN_XOR_ASSIGN:
        throw error( "Bitwise Xor can only be used on integer and boolean values." );
      case TOKEN_SHL_ASSIGN:
        throw error( "Left Shift can only be used on integer values." );
      case TOKEN_SHR_ASSIGN:
        throw error( "Right Shift can only be used on integer values." );
      case TOKEN_SHRX_ASSIGN:
        throw error( "Right Shift with Sign Extend can only be used on integer values." );
    }
  }
  else if (element_type == jog_type_manager.type_real32)
  {
    switch (op_type)
    {
      case TOKEN_ADD_ASSIGN:
        return (new JogCmdAddAssignArrayReal<float>())->init( t, context, index_expr, rhs );
      case TOKEN_SUB_ASSIGN:
        return (new JogCmdSubAssignArrayReal<float>())->init( t, context, index_expr, rhs );
      case TOKEN_MUL_ASSIGN:
        return (new JogCmdMulAssignArrayReal<float>())->init( t, context, index_expr, rhs );
      case TOKEN_DIV_ASSIGN:
        return (new JogCmdDivAssignArrayReal<float>())->init( t, context, index_expr, rhs );
      case TOKEN_MOD_ASSIGN:
        throw error( "Modulo can only be used on integer values." );
      case TOKEN_AND_ASSIGN:
        throw error( "Bitwise And can only be used on integer and boolean values." );
      case TOKEN_OR_ASSIGN:
        throw error( "Bitwise Or can only be used on integer and boolean values." );
      case TOKEN_XOR_ASSIGN:
        throw error( "Bitwise Xor can only be used on integer and boolean values." );
      case TOKEN_SHL_ASSIGN:
        throw error( "Left Shift can only be used on integer values." );
      case TOKEN_SHR_ASSIGN:
        throw error( "Right Shift can only be used on integer values." );
      case TOKEN_SHRX_ASSIGN:
        throw error( "Right Shift with Sign Extend can only be used on integer values." );
    }
  }
  else if (element_type == jog_type_manager.type_int64)
  {
    switch (op_type)
    {
      case TOKEN_ADD_ASSIGN:
        return (new JogCmdAddAssignArrayInteger<JogInt64>())->init( t, context, index_expr, rhs );
      case TOKEN_SUB_ASSIGN:
        return (new JogCmdSubAssignArrayInteger<JogInt64>())->init( t, context, index_expr, rhs );
      case TOKEN_MUL_ASSIGN:
        return (new JogCmdMulAssignArrayInteger<JogInt64>())->init( t, context, index_expr, rhs );
      case TOKEN_DIV_ASSIGN:
        return (new JogCmdDivAssignArrayInteger<JogInt64>())->init( t, context, index_expr, rhs );
      case TOKEN_MOD_ASSIGN:
        return (new JogCmdModAssignArray<JogInt64>())->init( t, context, index_expr, rhs );
      case TOKEN_AND_ASSIGN:
        return (new JogCmdAndAssignArray<JogInt64>())->init( t, context, index_expr, rhs );
      case TOKEN_OR_ASSIGN:
        return (new JogCmdOrAssignArray<JogInt64>())->init( t, context, index_expr, rhs );
      case TOKEN_XOR_ASSIGN:
        return (new JogCmdXorAssignArray<JogInt64>())->init( t, context, index_expr, rhs );
      case TOKEN_SHL_ASSIGN:
        return (new JogCmdSHLAssignArray<JogInt64>())->init( t, context, index_expr, rhs );
      case TOKEN_SHR_ASSIGN:
        return (new JogCmdSHRAssignArray<JogInt64>())->init( t, context, index_expr, rhs );
      case TOKEN_SHRX_ASSIGN:
        return (new JogCmdSHRXAssignArray<JogInt64>())->init( t, context, index_expr, rhs );
    }
  }
  else if (element_type == jog_type_manager.type_int32)
  {
    switch (op_type)
    {
      case TOKEN_ADD_ASSIGN:
        return (new JogCmdAddAssignArrayInteger<JogInt32>())->init( t, context, index_expr, rhs );
      case TOKEN_SUB_ASSIGN:
        return (new JogCmdSubAssignArrayInteger<JogInt32>())->init( t, context, index_expr, rhs );
      case TOKEN_MUL_ASSIGN:
        return (new JogCmdMulAssignArrayInteger<JogInt32>())->init( t, context, index_expr, rhs );
      case TOKEN_DIV_ASSIGN:
        return (new JogCmdDivAssignArrayInteger<JogInt32>())->init( t, context, index_expr, rhs );
      case TOKEN_MOD_ASSIGN:
        return (new JogCmdModAssignArray<JogInt32>())->init( t, context, index_expr, rhs );
      case TOKEN_AND_ASSIGN:
        return (new JogCmdAndAssignArray<JogInt32>())->init( t, context, index_expr, rhs );
      case TOKEN_OR_ASSIGN:
        return (new JogCmdOrAssignArray<JogInt32>())->init( t, context, index_expr, rhs );
      case TOKEN_XOR_ASSIGN:
        return (new JogCmdXorAssignArray<JogInt32>())->init( t, context, index_expr, rhs );
      case TOKEN_SHL_ASSIGN:
        return (new JogCmdSHLAssignArray<JogInt32>())->init( t, context, index_expr, rhs );
      case TOKEN_SHR_ASSIGN:
        return (new JogCmdSHRAssignArray<JogInt32>())->init( t, context, index_expr, rhs );
      case TOKEN_SHRX_ASSIGN:
        return (new JogCmdSHRXAssignArray<JogInt32>())->init( t, context, index_expr, rhs );
    }
  }
  else if (element_type == jog_type_manager.type_int16)
  {
    switch (op_type)
    {
      case TOKEN_ADD_ASSIGN:
        return (new JogCmdAddAssignArrayInteger<JogInt16>())->init( t, context, index_expr, rhs );
      case TOKEN_SUB_ASSIGN:
        return (new JogCmdSubAssignArrayInteger<JogInt16>())->init( t, context, index_expr, rhs );
      case TOKEN_MUL_ASSIGN:
        return (new JogCmdMulAssignArrayInteger<JogInt16>())->init( t, context, index_expr, rhs );
      case TOKEN_DIV_ASSIGN:
        return (new JogCmdDivAssignArrayInteger<JogInt16>())->init( t, context, index_expr, rhs );
      case TOKEN_MOD_ASSIGN:
        return (new JogCmdModAssignArray<JogInt16>())->init( t, context, index_expr, rhs );
      case TOKEN_AND_ASSIGN:
        return (new JogCmdAndAssignArray<JogInt16>())->init( t, context, index_expr, rhs );
      case TOKEN_OR_ASSIGN:
        return (new JogCmdOrAssignArray<JogInt16>())->init( t, context, index_expr, rhs );
      case TOKEN_XOR_ASSIGN:
        return (new JogCmdXorAssignArray<JogInt16>())->init( t, context, index_expr, rhs );
      case TOKEN_SHL_ASSIGN:
        return (new JogCmdSHLAssignArray<JogInt16>())->init( t, context, index_expr, rhs );
      case TOKEN_SHR_ASSIGN:
        return (new JogCmdSHRAssignArray<JogInt16>())->init( t, context, index_expr, rhs );
      case TOKEN_SHRX_ASSIGN:
        return (new JogCmdSHRXAssignArray<JogInt16>())->init( t, context, index_expr, rhs );
    }
  }
  else if (element_type == jog_type_manager.type_int8)
  {
    switch (op_type)
    {
      case TOKEN_ADD_ASSIGN:
        return (new JogCmdAddAssignArrayInteger<JogInt8>())->init( t, context, index_expr, rhs );
      case TOKEN_SUB_ASSIGN:
        return (new JogCmdSubAssignArrayInteger<JogInt8>())->init( t, context, index_expr, rhs );
      case TOKEN_MUL_ASSIGN:
        return (new JogCmdMulAssignArrayInteger<JogInt8>())->init( t, context, index_expr, rhs );
      case TOKEN_DIV_ASSIGN:
        return (new JogCmdDivAssignArrayInteger<JogInt8>())->init( t, context, index_expr, rhs );
      case TOKEN_MOD_ASSIGN:
        return (new JogCmdModAssignArray<JogInt8>())->init( t, context, index_expr, rhs );
      case TOKEN_AND_ASSIGN:
        return (new JogCmdAndAssignArray<JogInt8>())->init( t, context, index_expr, rhs );
      case TOKEN_OR_ASSIGN:
        return (new JogCmdOrAssignArray<JogInt8>())->init( t, context, index_expr, rhs );
      case TOKEN_XOR_ASSIGN:
        return (new JogCmdXorAssignArray<JogInt8>())->init( t, context, index_expr, rhs );
      case TOKEN_SHL_ASSIGN:
        return (new JogCmdSHLAssignArray<JogInt8>())->init( t, context, index_expr, rhs );
      case TOKEN_SHR_ASSIGN:
        return (new JogCmdSHRAssignArray<JogInt8>())->init( t, context, index_expr, rhs );
      case TOKEN_SHRX_ASSIGN:
        return (new JogCmdSHRXAssignArray<JogInt8>())->init( t, context, index_expr, rhs );
    }
  }
  else if (element_type == jog_type_manager.type_char)
  {
    switch (op_type)
    {
      case TOKEN_ADD_ASSIGN:
        return (new JogCmdAddAssignArrayInteger<JogChar>())->init( t, context, index_expr, rhs );
      case TOKEN_SUB_ASSIGN:
        return (new JogCmdSubAssignArrayInteger<JogChar>())->init( t, context, index_expr, rhs );
      case TOKEN_MUL_ASSIGN:
        return (new JogCmdMulAssignArrayInteger<JogChar>())->init( t, context, index_expr, rhs );
      case TOKEN_DIV_ASSIGN:
        return (new JogCmdDivAssignArrayInteger<JogChar>())->init( t, context, index_expr, rhs );
      case TOKEN_MOD_ASSIGN:
        return (new JogCmdModAssignArray<JogChar>())->init( t, context, index_expr, rhs );
      case TOKEN_AND_ASSIGN:
        return (new JogCmdAndAssignArray<JogChar>())->init( t, context, index_expr, rhs );
      case TOKEN_OR_ASSIGN:
        return (new JogCmdOrAssignArray<JogChar>())->init( t, context, index_expr, rhs );
      case TOKEN_XOR_ASSIGN:
        return (new JogCmdXorAssignArray<JogChar>())->init( t, context, index_expr, rhs );
      case TOKEN_SHL_ASSIGN:
        return (new JogCmdSHLAssignArray<JogChar>())->init( t, context, index_expr, rhs );
      case TOKEN_SHR_ASSIGN:
        return (new JogCmdSHRAssignArray<JogChar>())->init( t, context, index_expr, rhs );
      case TOKEN_SHRX_ASSIGN:
        return (new JogCmdSHRXAssignArray<JogChar>())->init( t, context, index_expr, rhs );
    }
  }
  else
  {
    switch (op_type)
    {
      case TOKEN_ADD_ASSIGN:
        throw error( "Boolean values cannot be added." );
      case TOKEN_SUB_ASSIGN:
        throw error( "Boolean values cannot be subtracted." );
      case TOKEN_MUL_ASSIGN:
        throw error( "Boolean values cannot be multiplied." );
      case TOKEN_DIV_ASSIGN:
        throw error( "Boolean values cannot be divided." );
      case TOKEN_MOD_ASSIGN:
        throw error( "Modulo can only be used on integer values." );
      case TOKEN_AND_ASSIGN:
        return (new JogCmdAndAssignArray<JogInt64>())->init( t, context, index_expr, rhs );
      case TOKEN_OR_ASSIGN:
        return (new JogCmdOrAssignArray<JogInt64>())->init( t, context, index_expr, rhs );
      case TOKEN_XOR_ASSIGN:
      case TOKEN_SHL_ASSIGN:
      case TOKEN_SHR_ASSIGN:
      case TOKEN_SHRX_ASSIGN:
        throw error( "Boolean values cannot be bit-shifted." );
    }
  }

  throw error( "[Internal] JogCmdArrayAccess::resolve_op_assign()" );
}

Ref<JogCmd> JogCmdArrayAccess::resolve_stepcount_access( int when, int modifier )
{
  context = context->resolve();
  JogTypeInfo* context_type = context->require_value();
  if ( !context_type->is_array() )
  {
    throw context->error( "Array reference expected." );
  }

  index_expr = index_expr->resolve();
  index_expr->require_integer();
  index_expr = (index_expr->cast_to_type(jog_type_manager.type_int32))->resolve();

  JogTypeInfo* element_type = context_type->element_type;

  if (element_type->is_reference())
  {
    throw error( "++/-- cannot be used on references." );
  }
  else if (when == 0)
  {
    if (element_type == jog_type_manager.type_real64)
    {
      return (new JogCmdPreStepArrayReal<double>())->init( t, context, index_expr, modifier );
    }
    else if (element_type == jog_type_manager.type_real32)
    {
      return (new JogCmdPreStepArrayReal<float>())->init( t, context, index_expr, modifier );
    }
    else if (element_type == jog_type_manager.type_int64)
    {
      return (new JogCmdPreStepArrayInteger<JogInt64>())->init( t, context, index_expr, modifier );
    }
    else if (element_type == jog_type_manager.type_int32)
    {
      return (new JogCmdPreStepArrayInteger<JogInt32>())->init( t, context, index_expr, modifier );
    }
    else if (element_type == jog_type_manager.type_int16)
    {
      return (new JogCmdPreStepArrayInteger<JogInt16>())->init( t, context, index_expr, modifier );
    }
    else if (element_type == jog_type_manager.type_int8)
    {
      return (new JogCmdPreStepArrayInteger<JogInt8>())->init( t, context, index_expr, modifier );
    }
    else if (element_type == jog_type_manager.type_char)
    {
      return (new JogCmdPreStepArrayInteger<JogChar>())->init( t, context, index_expr, modifier );
    }
    else
    {
      throw error( "++/-- cannot be used with boolean variables." );
    }
  }
  else
  {
    if (element_type == jog_type_manager.type_real64)
    {
      return (new JogCmdPostStepArrayReal<double>())->init( t, context, index_expr, modifier );
    }
    else if (element_type == jog_type_manager.type_real32)
    {
      return (new JogCmdPostStepArrayReal<float>())->init( t, context, index_expr, modifier );
    }
    else if (element_type == jog_type_manager.type_int64)
    {
      return (new JogCmdPostStepArrayInteger<JogInt64>())->init( t, context, index_expr, 
          modifier );
    }
    else if (element_type == jog_type_manager.type_int32)
    {
      return (new JogCmdPostStepArrayInteger<JogInt16>())->init( t, context, index_expr, 
          modifier );
    }
    else if (element_type == jog_type_manager.type_int16)
    {
      return (new JogCmdPostStepArrayInteger<JogInt32>())->init( t, context, index_expr, 
          modifier );
    }
    else if (element_type == jog_type_manager.type_int8)
    {
      return (new JogCmdPostStepArrayInteger<JogInt8>())->init( t, context, index_expr, 
          modifier );
    }
    else if (element_type == jog_type_manager.type_char)
    {
      return (new JogCmdPostStepArrayInteger<JogChar>())->init( t, context, index_expr, 
          modifier );
    }
    else
    {
      throw error( "++/-- cannot be used with boolean variables." );
    }
  }
}


Ref<JogCmd> JogCmdReturnValue::resolve()
{
  operand = operand->resolve();
  JogTypeInfo* return_type = operand->require_value();

  if (jog_context->this_method->return_type == NULL)
  {
    throw error( "Cannot return a value from a method with a \"void\" return type." );
  }

  if ( !return_type->instance_of(jog_context->this_method->return_type) )
  {
    throw error( "Return value incompatible with declared return type." );
  }

  if (return_type->is_primitive())
  {
    return new JogCmdReturnData( t, operand );
  }
  else
  {
    return new JogCmdReturnRef( t, operand );
  }
}

Ref<JogCmd> JogCmdCallSuperConstructor::resolve()
{
  if (jog_context->this_type == jog_type_manager.type_object)
  {
    throw error( "Illegal superclass call in root class Object." );
  }

  JogTypeInfo* base_class = jog_context->this_type->base_class;
  base_class->resolve();
  JogMethodInfo* m = resolve_call( t, base_class, new JogString("<init>"), *args );
  return new JogCmdStaticCall( t, m, new JogCmdThis(t,jog_context->this_type), *args );
}

JogCmdThis::JogCmdThis( Ref<JogToken> t, JogTypeInfo* this_type ) 
  : JogCmd(t), this_type(this_type)
{
  if (jog_context && jog_context->this_method->is_static())
  {
    throw error( "Invalid reference to 'this' object from a static method context." );
  }
}

