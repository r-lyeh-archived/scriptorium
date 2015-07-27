#ifndef _LANG_CONTEXT_HEADER
#define _LANG_CONTEXT_HEADER

#ifdef _DEBUG
#ifndef DBG_NEW      
#define DBG_NEW new ( _NORMAL_BLOCK , __FILE__ , __LINE__ )     
#define new DBG_NEW   
#endif

#define _CRTDBG_MAP_ALLOC
//#define _CRTDBG_MAP_ALLOC_NEW
#include <crtdbg.h>
#endif

#include <functional>
#include <string>
#include <map>
#include <algorithm>

#include "Value.h"

#include "VMStack.h"
#include "Parser.h"
#include "JetInstructions.h"
#include "JetExceptions.h"
#include "GarbageCollector.h"


#ifdef _WIN32
#include <Windows.h>
//#define JET_TIME_EXECUTION
#endif

#define GC_INTERVAL 200//number of allocations before running the GC
#define GC_STEPS 4//number of g0 collections before a gen1 collection

#define JET_STACK_SIZE 800
#define JET_MAX_CALLDEPTH 400

namespace Jet
{
	typedef std::function<void(Jet::JetContext*,Jet::Value*,int)> JetFunction;
#define JetBind(context, fun) 	auto temp__bind_##fun = [](Jet::JetContext* context,Jet::Value* args, int numargs) { return Value(fun(args[0]));};context[#fun] = Jet::Value(temp__bind_##fun);
	//void(*temp__bind_##fun)(Jet::JetContext*,Jet::Value*,int)> temp__bind_##fun = &[](Jet::JetContext* context,Jet::Value* args, int numargs) { context->Return(fun(args[0]));}; context[#fun] = &temp__bind_##fun;
#define JetBind2(context, fun) 	auto temp__bind_##fun = [](Jet::JetContext* context,Jet::Value* args, int numargs) {  return Value(fun(args[0],args[1]));};context[#fun] = Jet::Value(temp__bind_##fun);
#define JetBind3(context, fun, type) 	auto temp__bind_##fun = [](Jet::JetContext* context,Jet::Value* args, int numargs) {  return Value(fun((type)args[0],(type)args[1]));};context[#fun] = Jet::Value(temp__bind_##fun);

	//builtin function definitions
	Value gc(JetContext* context,Value* args, int numargs);
	Value print(JetContext* context,Value* args, int numargs);
	Value tostring(JetContext* context, Value* args, int numargs);
	
	class JetContext
	{
		friend struct Generator;
		friend struct Value;
		friend class JetObject;
		friend class GarbageCollector;
		VMStack<Value> stack;
		VMStack<std::pair<unsigned int, Closure*> > callstack;

		std::map<std::string, Function*> functions;
		std::vector<Function*> entrypoints;
		std::map<std::string, unsigned int> variables;//mapping from string to location in vars array

		//actual data being worked on
		std::vector<Value> vars;//where they are actually stored

		CompilerContext compiler;//root compiler context

		//core library prototypes
		JetObject* string;
		JetObject* Array;
		JetObject* object;
		JetObject* arrayiter;
		JetObject* objectiter;
		JetObject* function;

		//require cache
		std::map<std::string, Value> require_cache;
		std::map<std::string, Value> libraries;

		//manages memory
		GarbageCollector gc;
		std::vector<JetObject*> prototypes;

		Closure* lastadded;
		struct OpenCapture
		{
			Capture* capture;
#ifdef _DEBUG
			Closure* creator;
#endif
		};
		std::deque<OpenCapture> opencaptures;

	public:
		//use these
		Value NewObject();
		Value NewArray();
		Value NewUserdata(void* data, const Value& proto);
		Value NewString(const char* string, bool copy = true);//if copy is false, it takes ownership of the char array
		 
		void AddLibrary(const std::string& name, Value& library)
		{
			library.AddRef();
			this->libraries[name] = library;
		}

		//a helper function for registering metatables for userdata, these are never gc'd
		//and are freed with the context
		Value NewPrototype(const char* Typename);

		JetContext();
		~JetContext();

		//allows assignment and reading of gobal variables
		Value& operator[](const std::string& id);
		Value Get(const std::string& name);
		void Set(const std::string& name, const Value& value);

		std::string Script(const std::string code, const std::string filename = "file");
		Value Script(const char* code, const char* filename = "file");//compiles, assembles and executes the script


		//compiles source code to ASM for the VM to read in
		std::vector<IntermediateInstruction> Compile(const char* code, const char* filename = "file");

		//parses in ASM, returns a function
		Value Assemble(const std::vector<IntermediateInstruction>& code);

		//executes a function in the VM context
		Value Call(const char* function, Value* args = 0, unsigned int numargs = 0);
		Value Call(const Value* function, Value* args = 0, unsigned int numargs = 0);

		void RunGC();//runs an iteration of the garbage collector

	private:
		Value* sptr;//stack pointer
		Closure* curframe;
		Value localstack[JET_STACK_SIZE];

		//begin executing instructions at iptr index
		Value Execute(int iptr, Closure* frame);
		unsigned int Call(const Value* function, unsigned int iptr, unsigned int args);//used for calls in the VM

		//debug functions
		void GetCode(int ptr, Closure* closure, std::string& ret, unsigned int& line);
		void StackTrace(int curiptr, Closure* cframe);

		static Value Callstack(JetContext* context, Value* v, int ar);
	};
}

#endif