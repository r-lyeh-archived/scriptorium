#include "jog.h"

JogVM* vm;
bool error;
bool compile_error;
char* error_filename;
char* error_message;
char* error_context;
int error_line;
int error_column;
  
bool is_reference_test;
char* test_results;
char* current_test_name;
char* current_reference_result;
char* current_user_result;
bool all_tests_passed;

#define unichar short

void append_string(char**A, char*b) {
  char *a = *A;
  int len = strlen(a) + strlen(b) + 1;
  char *r = (char*)malloc(len);
  strcpy(r, a);
  strcpy(&(r[strlen(a)]), b);
  r[len-1] = 0;
  free(a);
  *A = r;
}

char* to_string( JogRef ref ) {
  JogObject* str = *ref;
  
  if (str == NULL)
  {
    return strdup("null");
  }
  else
  {
    JogObject* array = *((JogObject**)&(str->data[0]));
    if (array)
    {
      unichar* chars = (unichar*) array->data;
      char * r = (char*)malloc(array->count+1);
      for (int i=0; i<array->count; i++) {
	r[i] = chars[i];
      }
      r[array->count] = 0;
      return r;
    }
    else
    {
      return strdup("[Error]");
    }
  }
}

char* to_string( Ref<ASCIIString> str )
{
  if (*str == NULL)
  {
    return strdup("null");
  }
  else
  {
    int count = str->count;
    char* chars = (char*)malloc(count+1);
    for (int i=0; i<count; ++i)
    {
      chars[i] = (char) str->data[i];
    }
    chars[count] = 0;

    return chars;
  }
}

//=============================================================================
//  Native Method Implementations
//=============================================================================

static void BeanGrinder__configureTest__String( JogVM* vm)
{
  free(current_test_name);
  free(current_reference_result);
  free(current_user_result);
  current_test_name = to_string( vm->pop_ref() );
  current_reference_result = strdup("");
  current_user_result = strdup("");
}

static void BeanGrinder__startReferenceTest( JogVM* vm )
{
  is_reference_test = true;
}

static void BeanGrinder__setReferenceOutput__String( JogVM* vm )
{
  free(current_reference_result);
  current_reference_result = to_string(vm->pop_ref());
}

static void BeanGrinder__startUserTest( JogVM* vm )
{
  is_reference_test = false;
}

void fprint_field(FILE*fp, const char* property, char*s, int comma=0) {
  fprintf(fp, "\"%s\": \"", property);
  int len = strlen(s);
  for (int i=0; i<len; i++) {
    if (s[i] == '\n') { fputc('\\', fp); fputc('n', fp); }
    else if (s[i] == '"') { fputc('\\', fp); fputc('"', fp); }
    else if (s[i] == '\\') { fputc('\\', fp); fputc('\\', fp); }
    else fputc(s[i], fp);
  }
  fputc('\"', fp);
  if (comma)
    fputc(',', fp);
  fputc('\n', fp);
}

static void BeanGrinder__endTest( JogVM* vm ) {
  size_t buffer_size = 
    2*(strlen(current_test_name) + 
       strlen(current_reference_result) + 
       strlen(current_user_result)) + 
    512;
  char *buffer = (char*)malloc(buffer_size);

  FILE* fp = open_memstream(&buffer, &buffer_size);

  if (fp == NULL) {
    exit(0);
  }

  fprintf(fp, "{");

  fprint_field(fp, "action", current_test_name, 1);
  fprint_field(fp, "referenceOutput", current_reference_result, 1);
  fprint_field(fp, "userOutput", current_user_result, 1);

  if ( strcmp(current_reference_result, current_user_result) == 0)
  {
    fprintf(fp, "\"success\": \"true\"\n");
  }
  else
  {
    fprintf(fp, "\"success\": \"false\"\n");
    all_tests_passed = false;
  }

  fprintf(fp, "}");
  fclose(fp);

  if (strlen(test_results)) {
    char *tmp = test_results;
    asprintf(&test_results, "%s,\n%s", test_results, buffer);
    free(tmp);
  } else {
    asprintf(&test_results, "%s", buffer);
  }
  free(buffer);
}

static void BeanGrinder__printSound__Array_of_float( JogVM* vm )
{
/*
  JogInterpreter* jog = ((JogInterpreter*) vm->user_context);

  JogObject* array = *(vm->pop_ref());

  if ( !array )
  {
    printf( "[Internal] null pointer exception in native BeanGrinder::printSound(float[])." );
    return;
  }

  int count = array->count;
  [jog printSound:((float*) array->data) withCount:count];
*/
}

static void BeanGrinder__playSound__Array_of_float( JogVM* vm )
{
/*
  JogInterpreter* jog = ((JogInterpreter*) vm->user_context);

  JogObject* array = *(vm->pop_ref());

  if ( !array )
  {
    printf( "[Internal] null pointer exception in native BeanGrinder::playSound(float[])." );
    return;
  }

  int count = array->count;
  [jog playSound:((float*) array->data) withCount:count];
*/
}

static void BeanGrinder__printImage__Array_of_byte_int( JogVM* vm )
{
/*
  JogInterpreter* jog = ((JogInterpreter*) vm->user_context);

  int width = (int) vm->pop_data();
  JogObject* array = *(vm->pop_ref());

  if ( !array )
  {
    printf( "[Internal] null pointer exception in native BeanGrinder::printImage(byte[],int)." );
    return;
  }

  int count = array->count;
  [jog printImage:((char*) array->data) withCount:count withWidth:width];
*/
}

//-----------------------------------------------------------------------------

static void PrintWriter__print__char( JogVM* vm )
{
  //JogInterpreter* jog = ((JogInterpreter*) vm->user_context);
  //bool is_reference_test = jog->is_reference_test;

  int ch = vm->pop_int();
  char chars[2];
  chars[0] = (char) ch;
  chars[1] = 0;
  if (is_reference_test)
  {
    append_string(&current_reference_result, chars);
  }
  else
  {
    append_string(&current_user_result, chars);
  }
}

static void PrintWriter__print__String( JogVM* vm )
{
  JogObject* str = *(vm->pop_ref());

  if (is_reference_test)
  {
    char* tmp = to_string(str);
    append_string(&current_reference_result, tmp);
    free(tmp);
  }
  else
  {
    char* tmp = to_string(str);
    append_string(&current_user_result, tmp);
    free(tmp);
  }
}


//=============================================================================
//  JogInterpreter
//=============================================================================


void init ()
{
  error = false;
  compile_error = false;
  
  vm = new JogVM();
  // vm_object = vm;
  vm->retain();
  
  vm->add_native_handler( "BeanGrinder::configureTest(String)", 
			  BeanGrinder__configureTest__String );
  vm->add_native_handler( "BeanGrinder::startReferenceTest()", 
			  BeanGrinder__startReferenceTest );
  vm->add_native_handler( "BeanGrinder::setReferenceOutput(String)", 
			  BeanGrinder__setReferenceOutput__String );
  vm->add_native_handler( "BeanGrinder::startUserTest()", 
			  BeanGrinder__startUserTest );
  vm->add_native_handler( "BeanGrinder::endTest()", 
			  BeanGrinder__endTest );
  vm->add_native_handler( "BeanGrinder::printSound(float[])", 
			  BeanGrinder__printSound__Array_of_float );
  vm->add_native_handler( "BeanGrinder::playSound(float[])", 
			  BeanGrinder__playSound__Array_of_float );
  vm->add_native_handler( "BeanGrinder::printImage(byte[],int)", 
			  BeanGrinder__printImage__Array_of_byte_int );
    
  vm->add_native_handler( "PrintWriter::print(char)", PrintWriter__print__char );
  vm->add_native_handler( "PrintWriter::print(String)", PrintWriter__print__String );

  is_reference_test = true;
  test_results = strdup("");
  
  current_test_name = strdup("");
  current_reference_result = strdup(""); //[NSMutableString stringWithCapacity:80];
  current_user_result = strdup(""); //[NSMutableString stringWithCapacity:80];
  
  all_tests_passed = true;

  error_filename = 0;
  error_context = 0;
  error_message = 0;
  
}

/*
- (void) dealloc
{
  //NSLog( @"JogInterpreter Destroy" );

  if (vm_object)
  {
    ((JogVM*)vm_object)->release();
    vm_object = NULL;
  }
  [super dealloc];
}
*/

char* fileToString(char* filename)
{
  FILE* file = fopen(filename,"r");
  if(file == NULL)
  {
    return NULL;
  }

  fseek(file, 0, SEEK_END);
  long int size = ftell(file);
  rewind(file);

  char* content = (char*)calloc(size + 1, 1);

  fread(content,1,size,file);
  content[size] = 0;

  return content;
}

void setCompileError(Ref<JogError> err)
{
  if (*err->reader)
  {
    error_filename = to_string( ((JogReader*)*err->reader)->filename );
    error_context = to_string(err->context());
  }
  else
  {
    error_filename = strdup("[Internal]");
    error_context = strdup("");
  }
  error_line = err->line;
  error_column = err->column;
  error_message = to_string(err->message);
  compile_error = true;
  error = true;
}

void parse (char* filename, char* content);

void parse(char* filename)
{
  char* data = strdup(fileToString(filename));
  if ( !data ) return; // error code already set

  parse(filename, data);
}

void parse (char* filename, char* content)
{
  if (error) return;

  try
  {
    //JogVM* vm = (JogVM*) vm_object;
    vm->parse( filename, content );
  }
  catch (Ref<JogError> err)
  {
    setCompileError(err);
  }
  catch (...)
  {
    fprintf( stderr, "[Internal compiler error]\n" );
    error = true;
  }
}

void compile ()
{
  if (error) return;

  //NSLog( @"JogInterpreter Compile" );
  try
  {
    //JogVM* vm = (JogVM*) vm_object;
    vm->compile();
  }
  catch (Ref<JogError> err)
  {
    setCompileError(err);
  }
  catch (...)
  {
    fprintf( stderr, "[Internal compiler error]\n" );
    error = true;
  }
}

void run (char* main_class_name)
{
  if (error) return;

  try
  {
    //JogVM* vm = (JogVM*) vm_object;
    vm->timeout_seconds = 5;
    vm->run( main_class_name); //to_cpp_string(main_class_name).c_str() );
  }
  catch (Ref<JogError> err)
  {
    setCompileError(err);
    compile_error = false;
  }
  catch (...)
  {
    fprintf( stderr, "[Internal compiler error]\n" );
    error = true;
  }
}

void results()
{
  printf("{\n");
  //NSMutableDictionary* results = [NSMutableDictionary dictionaryWithCapacity:6];
  if (error)
  {
    if (compile_error)
    {
      printf("\"result\": \"compileError\",\n");
    }
    else
    {
      printf("\"result\": \"runtimeError\",\n");
    }

    fprint_field(stdout, "errorFilename", error_filename, 1);
    fprint_field(stdout, "errorContext", error_context, 1);
    fprint_field(stdout, "errorMessage", error_message, 1);
    printf("\"errorLine\": %d,\n", error_line);
    printf("\"errorColumn\": %d\n", error_column);
  }
  else 
  {
    if (all_tests_passed)
    {
      printf("\"result\": \"success\",\n");
    }
    else
    {
      printf("\"result\": \"logicError\",\n");
    }
    printf("\"resultArray\": [\n");
    printf("%s", test_results);
    printf("]\n");
  }
  printf("}\n");
}

/*
- (void) printSound:(float*)soundData withCount:(int)count
{
  NSLog( @"TODO: [printSound] in JogInterpreter.mm" );
}

- (void) playSound:(float*)soundData withCount:(int)count
{
  NSLog( @"TODO: [playSound] in JogInterpreter.mm" );
}

- (void) printImage:(char*)imageData withCount:(int)count withWidth:(int)width
{
  // imageData[0]..[2] is RGB of pixel (0,0).
  // imageData[3]..[5] is RGB of pixel (1,0).
  // imageData[count-3]..[count-1] is RGB of pixel (width-1,height-1).
  int height = (count/3)/width;
  NSLog( @"TODO: [printImage] in JogInterpreter.mm" );
}
*/

int main(int argc, char *argv[])
{
  char* input = argv[1];
  //strdup("/tmp/");
  //append_string(&input, argv[1]);

  init();
  parse("jog_stdlib.java");
  parse("bean_grinder.java");
  parse(input);
  compile();
  run("MyTest");
  results();
}
