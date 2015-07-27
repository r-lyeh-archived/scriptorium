#ifdef _DEBUG
#ifndef DBG_NEW      
#define DBG_NEW new ( _NORMAL_BLOCK , __FILE__ , __LINE__ )     
#define new DBG_NEW   
#endif

#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#endif

#include "JetContext.h"
#include "UniquePtr.h"

#include <stack>
#include <fstream>
#include <memory>

#undef Yield

using namespace Jet;

#define JET_BAD_INSTRUCTION 123456789

Value Jet::gc(JetContext* context,Value* args, int numargs) 
{ 
	context->RunGC();
	return Value();
}


Value Jet::tostring(JetContext* context, Value* args, int numargs)
{
	if (numargs >= 1)
	{
		auto str = context->NewString(args->ToString().c_str(), true);
		return str;
	}
	throw RuntimeException("Invalid tostring call");
}

Value JetContext::Callstack(JetContext* context, Value* args, int numargs)
{
	context->StackTrace(JET_BAD_INSTRUCTION, 0);
	return Value();
}

Value Jet::print(JetContext* context,Value* args, int numargs) 
{ 
	for (int i = 0; i < numargs; i++)
	{
		printf("%s", args[i].ToString().c_str());
	}
	printf("\n");
	return Value();
};

Value& JetContext::operator[](const std::string& id)
{
	auto iter = variables.find(id);
	if (iter == variables.end())
	{
		//add it
		variables[id] = variables.size();
		vars.push_back(Value());
		return vars[variables[id]];
	}
	else
	{
		return vars[(*iter).second];
	}
}

Value JetContext::Get(const std::string& name)
{
	auto iter = variables.find(name);
	if (iter == variables.end())
	{
		return Value();//return null
	}
	else
	{
		return vars[(*iter).second];
	}
}

void JetContext::Set(const std::string& name, const Value& value)
{
	auto iter = variables.find(name);
	if (iter == variables.end())
	{
		//add it
		variables[name] = variables.size();
		vars.push_back(value);
	}
	else
	{
		vars[(*iter).second] = value;
	}
}

Value JetContext::NewObject()
{
	auto v = this->gc.New<JetObject>(this);
	v->refcount = 0;
	v->type = ValueType::Object;
	v->grey = v->mark = false;

	return Value(v);
}

Value JetContext::NewPrototype(const char* Typename)
{
	auto v = new JetObject(this);//this->gc.New<JetObject>(this);//auto v = new JetObject(this);
	v->refcount = 0;
	v->type = ValueType::Object;
	v->grey = v->mark = false;
	this->prototypes.push_back(v);
	return v;
}

Value JetContext::NewArray()
{
	auto a = gc.New<JetArray>();//new JetArray;
	a->refcount = 0;
	a->grey = a->mark = false;
	a->type = ValueType::Array;
	a->context = this;

	return Value(a);
}

Value JetContext::NewUserdata(void* data, const Value& proto)
{
	if (proto.type != ValueType::Object)
		throw RuntimeException("NewUserdata: Prototype supplied was not of the type 'object'\n");

	auto ud = gc.New<JetUserdata>(data, proto._object);
	ud->grey = ud->mark = false;
	ud->refcount = 0;
	ud->type = ValueType::Userdata;
	return Value(ud, proto._object);
}

Value JetContext::NewString(const char* string, bool copy)
{
	if (copy)
	{
		size_t len = strlen(string);
		auto temp = new char[len+1];
		memcpy(temp, string, len);
		temp[len] = 0;
		string = temp;
	}
	auto str = gc.New<GCVal<char*>>((char*)string);
	str->grey = str->mark = false;
	str->refcount = 0;
	str->type = ValueType::String;
	return Value(str);
}

#include "Libraries/File.h"
#include "Libraries/Math.h"
JetContext::JetContext() : gc(this), stack(500000), callstack(JET_MAX_CALLDEPTH, "Exceeded Max Call Depth!")
{
	this->sptr = this->localstack;//initialize stack pointer
	this->curframe = 0;

	//add more functions and junk
	(*this)["print"] = print;
	(*this)["gc"] = ::gc;
	(*this)["callstack"] = JetContext::Callstack;
	(*this)["tostring"] = ::tostring;
	(*this)["pcall"] = [](JetContext* context, Value* args, int argc)
	{
		if (argc == 0)
			throw RuntimeException("Invalid argument count to pcall!");
		try
		{
			if (argc > 1)
				return context->Call(args, &args[1], argc-1);
			else if (argc == 1)
				return context->Call(args);
		}
		catch(RuntimeException e)
		{
			printf("PCall got exception: %s", e.reason.c_str());
			return Value(0);
		}
	};
	(*this)["error"] = [](JetContext* context, Value* args, int argc)
	{
		if (argc > 0)
			throw RuntimeException(args->ToString());
		else
			throw RuntimeException("User Error Thrown!");
		return Value();
	};

	/*this should probably be an instruction ...
	(*this)["unpack"] = [](JetContext* context, Value* args, int argc)
	{
	if (argc < 1 || args->type != ValueType::Array || args->_array->data.size() == 0)
	throw RuntimeException("Cannot unpack non or empty array!");

	for (int i = 0; i < args[0]._array->data.size()-1; i++)
	context->stack.Push(args[0]._array->data[i]);
	return args[0]._array->data[args[0]._array->data.size()-1];
	};*/

	(*this)["loadstring"] = [](JetContext* context, Value* args, int argc)
	{
		if (argc < 1 || args[0].type != ValueType::String)
			throw RuntimeException("Cannot load non string");

		return context->Assemble(context->Compile(args[0]._string->data, "loadstring"));
	};

	//tiny test for "async" like functionality
	/*(*this)["createServer"] = [](JetContext* context, Value* v, int args)
	{
		Value object = context->NewObject();
		if (v->IsGenerator())
		{
			//make use of this for a simple tcp server
			Value iterator = v->Call();
			iterator.Call();//run until it yields
			Value test = 52;
			iterator.Call(&test, 1);
			test = context->NewString("Hello world");
			iterator.Call(&test, 1);
		}
		return object;
	};*/

	(*this)["setprototype"] = [](JetContext* context, Value* v, int args)
	{
		if (args != 2)
			throw RuntimeException("Invalid Call, Improper Arguments!");

		if (v->type == ValueType::Object && v[1].type == ValueType::Object)
		{
			Value val = v[0];
			val._object->prototype = v[1]._object;
			return val;
		}
		else
		{
			throw RuntimeException("Improper arguments!");
		}
	};

	(*this)["require"] = [](JetContext* context, Value* v, int args)
	{
		if (args != 1 || v->type != ValueType::String)
			throw RuntimeException("Invalid Call, Improper Arguments!");

		auto iter = context->require_cache.find(v->_string->data);
		if (iter == context->require_cache.end())
		{
			//check from list of libraries
			auto lib = context->libraries.find(v->_string->data);
			if (lib != context->libraries.end())
				return lib->second;

			//else load from file
			std::ifstream t(v->_string->data, std::ios::in | std::ios::binary);
			if (t)
			{
				int length;
				t.seekg(0, std::ios::end);    // go to the end
				length = t.tellg();           // report location (this is the length)
				t.seekg(0, std::ios::beg);    // go back to the beginning
				UniquePtr<char[]> buffer(new char[length+1]);    // allocate memory for a buffer of appropriate dimension
				t.read(buffer, length);       // read the whole file into the buffer
				buffer[length] = 0;
				t.close();

				auto out = context->Compile(buffer, v->_string->data);
				auto fun = context->Assemble(out);
				auto temp = context->NewObject();
				context->require_cache[v->_string->data] = temp;
				auto obj = context->Call(&fun);
				if (obj.type == ValueType::Object)
				{
					for (auto ii: *obj._object)//copy stuff into the temporary object
						temp[ii.first] = ii.second;

					return temp;
				}
				else
				{
					context->require_cache[v->_string->data] = obj;//just use what was returned
					return obj;
				}
			}
			else
			{
				throw RuntimeException("Require could not find include: '" + (std::string)v->_string->data + "'");
			}
		}
		else
		{
			return Value(iter->second);
		}
	};

	(*this)["getprototype"] = [](JetContext* context, Value* v, int args)
	{
		if (args == 1 && (v->type == ValueType::Object || v->type == ValueType::Userdata))
			return Value(v->GetPrototype());
		else
			throw RuntimeException("getprototype expected an object or userdata value!");
	};


	//setup the string and array tables
	this->string = new JetObject(this);
	this->string->prototype = 0;
	(*this->string)["append"] = Value([](JetContext* context, Value* v, int args)
	{
		if (args == 2 && v[0].type == ValueType::String && v[1].type == ValueType::String)
		{
			size_t len = v[0].length + v[1].length + 1;
			char* text = new char[len];
			memcpy(text, v[0]._string->data, v[0].length);
			memcpy(text+v[0].length, v[1]._string->data, v[1].length);
			text[len-1] = 0;
			return context->NewString(text, false);
		}
		else
			throw RuntimeException("bad append call!");
	});
	(*this->string)["lower"] = Value([](JetContext* context, Value* v, int args)
	{
		if (args && v->type == ValueType::String)
		{
			char* str = new char[v->length+1];
			memcpy(str, v->_string->data, v->length);
			for (unsigned int i = 0; i < v->length; i++)
				str[i] = tolower(str[i]);
			str[v->length] = 0;
			return context->NewString(str, false);
		}
		throw RuntimeException("bad lower call");
	});
	(*this->string)["upper"] = Value([](JetContext* context, Value* v, int args)
	{
		if (args && v->type == ValueType::String)
		{
			char* str = new char[v->length+1];
			memcpy(str, v->_string->data, v->length);
			for (unsigned int i = 0; i < v->length; i++)
				str[i] = toupper(str[i]);
			str[v->length] = 0;
			return context->NewString(str, false);
		}
		throw RuntimeException("bad upper call");
	});
	//figure out how to get this working with strings
	(*this->string)["_add"] = Value([](JetContext* context, Value* v, int args)
	{
		if (args == 2 && v[0].type == ValueType::String && v[1].type == ValueType::String)
		{
			size_t len = v[0].length + v[1].length + 1;
			char* text = new char[len];
			memcpy(text, v[1]._string->data, v[1].length);
			memcpy(text+v[1].length, v[0]._string->data, v[0].length);
			text[len-1] = 0;
			return context->NewString(text, false);
		}
		else
			throw RuntimeException("bad string::append() call!");
	});
	(*this->string)["length"] = Value([](JetContext* context, Value* v, int args)
	{
		if (args == 1 && v->type == ValueType::String)
			return Value((double)v->length);
		else
			throw RuntimeException("bad string:length() call!");
	});
	(*this->string)["sub"] = [](JetContext* context, Value* v, int args)
	{
		if (args == 2)
		{
			if (v[0].type != ValueType::String)
				throw RuntimeException("must be a string");

			int len = v[0].length-(int)v[1];
			if (len < 0)
				throw RuntimeException("Invalid string index");

			char* str = new char[len+1];
			strncpy(str, &v[0]._string->data[(int)v[1]], len);
			str[len] = 0;
			return context->NewString(str, false);
		}
		else if (args == 3)
		{
			throw RuntimeException("Not Implemented!");
		}
		else
			throw RuntimeException("bad sub call");
	};


	this->Array = new JetObject(this);
	this->Array->prototype = 0;
	(*this->Array)["add"] = Value([](JetContext* context, Value* v, int args)
	{
		if (args == 2)
			v->_array->data.push_back(v[1]);
		else
			throw RuntimeException("Invalid add call!!");
		return Value();
	});
	(*this->Array)["size"] = Value([](JetContext* context, Value* v, int args)
	{
		if (args == 1)
			return Value((int)v->_array->data.size());
		else
			throw RuntimeException("Invalid size call!!");
	});
	(*this->Array)["resize"] = Value([](JetContext* context, Value* v, int args)
	{
		if (args == 2)
			v->_array->data.resize((int)v[1]);
		else
			throw RuntimeException("Invalid resize call!!");
		return Value();
	});

	(*this->Array)["remove"] = Value([](JetContext* context, Value* v, int args)
	{
		if (args == 2)
			v->_array->data.erase(v->_array->data.begin()+(int)v[1]);
		else
			throw RuntimeException("Invalid remove call!!");
		return Value();
	});

	struct arrayiter
	{
		JetArray* container;
		Value current;
		std::vector<Value>::iterator iterator;
	};
	//ok iterators need to hold a reference to their underlying data structure somehow
	(*this->Array)["iterator"] = Value([](JetContext* context, Value* v, int args)
	{
		if (args == 1)
		{
			auto it = new arrayiter;
			it->container = v->_array;
			v->AddRef();
			it->iterator = v->_array->data.begin();
			return Value(context->NewUserdata(it, context->arrayiter));
		}
		throw RuntimeException("Bad call to getIterator");
	});

	this->object = new JetObject(this);
	this->object->prototype = 0;
	(*this->object)["size"] = Value([](JetContext* context, Value* v, int args)
	{
		//how do I get access to the array from here?
		if (args == 1)
			return Value((int)v->_object->size());
		else
			throw RuntimeException("Invalid size call!!");
	});

	struct objiter
	{
		JetObject* container;
		Value current;
		JetObject::Iterator iterator;
	};
	(*this->object)["iterator"] = Value([](JetContext* context, Value* v, int args)
	{
		if (args == 1)
		{
			auto it = new objiter;
			it->container = v->_object;
			v->AddRef();
			it->iterator = v->_object->begin();
			return (context->NewUserdata(it, context->objectiter));
		}
		throw RuntimeException("Bad call to getIterator");
	});
	this->objectiter = new JetObject(this);
	this->objectiter->prototype = 0;
	(*this->objectiter)["current"] = Value([](JetContext* context, Value* v, int args)
	{
		auto iterator = v->GetUserdata<objiter>();
		return iterator->current;
	});
	(*this->objectiter)["advance"] = Value([](JetContext* context, Value* v, int args)
	{
		auto iterator = v->GetUserdata<objiter>();
		if (iterator->iterator == iterator->container->end())
			return Value(0);

		iterator->current = iterator->iterator->second;
		++iterator->iterator;
		return Value(1);
	});
	(*this->objectiter)["_gc"] = Value([](JetContext* context, Value* v, int args)
	{
		auto iter = v->GetUserdata<objiter>();
		Value(iter->container).Release();
		delete iter;
		return Value();
	});

	this->arrayiter = new JetObject(this);
	this->arrayiter->prototype = 0;
	(*this->arrayiter)["current"] = Value([](JetContext* context, Value* v, int args)
	{
		auto iterator = v->GetUserdata<arrayiter>();
		return iterator->current;
	});

	(*this->arrayiter)["advance"] = Value([](JetContext* context, Value* v, int args)
	{
		auto iterator = v->GetUserdata<arrayiter>();
		if (iterator->iterator == iterator->container->data.end())
			return Value(0);

		iterator->current = *iterator->iterator;
		++iterator->iterator;
		return Value(1);
	});
	(*this->arrayiter)["_gc"] = Value([](JetContext* context, Value* v, int args)
	{
		auto iter = v->GetUserdata<arrayiter>();
		Value(iter->container).Release();
		delete iter;
		return Value();
	});

	this->function = new JetObject(this);
	this->function->prototype = 0;
	(*this->function)["done"] = Value([](JetContext* context, Value* v, int args)
	{
		if (args >= 1 && v->type == ValueType::Function && v->_function->generator)
		{
			if (v->_function->generator->state == Generator::GeneratorState::Dead)
				return Value(1);
			else
				return Value(0);
		}
		throw RuntimeException("Cannot index a non generator or table");
	});
	/*ok get new iterator interface working
	add an iterator function to containers and function, that returns an iterator
	or a generator in the case of a yielding function

	match iterator functions for iteration on generators*/

	//for each loop needs current, next and advance
	//only work on functions
	(*this->function)["iterator"] = Value([](JetContext* context, Value* v, int args)
	{
		if (args >= 1 && v->type == ValueType::Function && v->_function->prototype->generator)
		{
			if (v->_function->generator)
			{
				if (v->_function->generator->state == Generator::GeneratorState::Dead)
					return Value();
				else
					return *v;//hack for foreach loops
			}

			Closure* closure = new Closure;
			closure->refcount = 0;
			closure->grey = closure->mark = false;
			closure->prev = v->_function->prev;
			closure->numupvals = v->_function->numupvals;
			closure->generator = new Generator(context, v->_function, 0);
			if (closure->numupvals)
				closure->upvals = new Capture*[closure->numupvals];
			closure->prototype = v->_function->prototype;
			context->gc.AddObject((GarbageCollector::gcval*)closure);

			if (closure->generator->state == Generator::GeneratorState::Dead)
				return Value();
			else
				return Value(closure);
		}
		throw RuntimeException("Cannot index non generator");
	});

	//these only work on generators
	(*this->function)["current"] = Value([](JetContext* context, Value* v, int args)
	{
		if (args >= 1 && v->type == ValueType::Function && v->_function->generator)
		{
			//return last yielded value
			return v->_function->generator->lastyielded;
		}
		throw RuntimeException("");
	});

	(*this->function)["advance"] = Value([](JetContext* context, Value* v, int args)
	{
		if (args >= 1 && v->type == ValueType::Function && v->_function->generator)
		{
			//execute generator here
			context->Call(v);//todo add second arg if we have it
			if (v->_function->generator->state == Generator::GeneratorState::Dead)
				return Value(0);
			else
				return Value(1);
		}
		throw RuntimeException("");
	});

	//load default libraries
	RegisterFileLibrary(this);
	RegisterMathLibrary(this);
};

JetContext::~JetContext()
{
	this->gc.Cleanup();

	for (auto ii: this->functions)
		delete ii.second;

	for (auto ii: this->entrypoints)
		delete ii;

	for (auto ii: this->prototypes)
		delete ii;

	delete this->string;
	delete this->Array;
	delete this->object;
	delete this->arrayiter;
	delete this->objectiter;
	delete this->function;
}

#ifndef _WIN32
typedef signed long long INT64;
#endif
//INT64 rate;
std::vector<IntermediateInstruction> JetContext::Compile(const char* code, const char* filename)
{
#ifdef JET_TIME_EXECUTION
	INT64 start, end, rate;
	QueryPerformanceFrequency( (LARGE_INTEGER *)&rate );
	QueryPerformanceCounter( (LARGE_INTEGER *)&start );
#endif

	Lexer lexer = Lexer(code, filename);
	Parser parser = Parser(&lexer);

	//printf("In: %s\n\nResult:\n", code);
	BlockExpression* result = parser.parseAll();
	//result->print();
	//printf("\n\n");

	std::vector<IntermediateInstruction> out = compiler.Compile(result, filename);

	delete result;

#ifdef JET_TIME_EXECUTION
	QueryPerformanceCounter( (LARGE_INTEGER *)&end );
	INT64 diff = end - start;
	double dt = ((double)diff)/((double)rate);

	printf("Took %lf seconds to compile\n\n", dt);
#endif

	return std::move(out);
}


class StackProfile
{
	char* name;
	INT64 start;
public:
	StackProfile(char* name)
	{
		this->name = name;
#ifdef JET_TIME_EXECUTION
#ifdef _WIN32
		//QueryPerformanceFrequency( (LARGE_INTEGER *)&rate );
		QueryPerformanceCounter( (LARGE_INTEGER *)&start );
#endif
#endif
	};

	~StackProfile()
	{
#ifdef JET_TIME_EXECUTION
#ifdef _WIN32
		INT64 end,rate;
		QueryPerformanceCounter( (LARGE_INTEGER *)&end );
		QueryPerformanceFrequency( (LARGE_INTEGER*)&rate);
		char o[100];
		INT64 diff = end - start;
		float dt = ((float)diff)/((float)rate);
		printf("%s took %f seconds\n", name, dt);
#endif
#endif
	}
};

void JetContext::RunGC()
{
	this->gc.Run();
}

unsigned int JetContext::Call(const Value* fun, unsigned int iptr, unsigned int args)
{
	if (fun->type == ValueType::Function)
	{
		//let generators be called
		if (fun->_function->generator)
		{
			callstack.Push(std::pair<unsigned int, Closure*>(iptr, curframe));

			sptr += curframe->prototype->locals;

			if ((sptr - localstack) >= JET_STACK_SIZE)
				throw RuntimeException("Stack Overflow!");

			curframe = fun->_function;

			if (args == 0)
				stack.Push(Value());
			else if (args > 1)
				for (unsigned int i = 1; i < args; i++)
					stack.Pop();
			return fun->_function->generator->Resume(this)-1;
		}

		if (fun->_function->prototype->generator)
		{
			//create generator and return it
			Closure* closure = new Closure;
			closure->grey = closure->mark = false;
			closure->prev = fun->_function->prev;
			closure->numupvals = fun->_function->numupvals;
			closure->refcount = 0;
			closure->generator = new Generator(this, fun->_function, args);
			if (closure->numupvals)
				closure->upvals = new Capture*[closure->numupvals];
			closure->prototype = fun->_function->prototype;
			closure->type = ValueType::Function;
			this->gc.AddObject((GarbageCollector::gcval*)closure);

			this->stack.Push(Value(closure));
			return iptr;
		}

		//manipulate frame pointer
		callstack.Push(std::pair<unsigned int, Closure*>(iptr, curframe));

		sptr += curframe->prototype->locals;

		//clean out the new stack for the gc
		for (unsigned int i = 0; i < fun->_function->prototype->locals; i++)
			sptr[i] = Value();

		if ((sptr - localstack) >= JET_STACK_SIZE)
			throw RuntimeException("Stack Overflow!");

		curframe = fun->_function;

		/*if (false)//curframe->closed)
		{
		//allocate new local frame here
		Closure* closure = new Closure;
		closure->grey = closure->mark = false;
		closure->prev = curframe->prev;
		closure->numupvals = curframe->numupvals;
		closure->closed = false;
		closure->refcount = 0;
		closure->generator = 0;
		closure->type = ValueType::Function;
		if (closure->numupvals)
		{
		closure->upvals = new Value*[closure->numupvals];
		for (int i = 0; i < closure->numupvals; i++)
		closure->upvals[i] = (Value*)0xcdcdcdcd;
		}
		closure->prototype = curframe->prototype;
		gc.AddObject((GarbageCollector::gcval*)closure);

		curframe = closure;

		if (gc.allocationCounter++%GC_INTERVAL == 0)
		this->RunGC();
		}*/
		//printf("ECall: Stack Ptr At: %d\n", sptr - localstack);

		Function* func = curframe->prototype;
		//set all the locals
		if (args <= func->args)
		{
			for (int i = func->args-1; i >= 0; i--)
			{
				if (i < args)
					sptr[i] = stack.Pop();
				else
					sptr[i] = Value();
			}
		}
		else if (func->vararg)
		{
			sptr[func->locals-1] = this->NewArray();
			auto arr = &sptr[func->locals-1]._array->data;
			arr->resize(args - func->args);
			for (int i = args-1; i >= 0; i--)
			{
				if (i < func->args)
					sptr[i] = stack.Pop();
				else
					(*arr)[i] = stack.Pop();
			}
		}
		else
		{
			for (int i = args-1; i >= 0; i--)
			{
				if (i < func->args)
					sptr[i] = stack.Pop();
				else
					stack.Pop();
			}
		}

		//go to function
		return -1;
	}
	else if (fun->type == ValueType::NativeFunction)
	{
		Value* tmp = &stack.mem[stack.size()-args];

		//ok fix this to be cleaner and resolve stack printing
		//should just push a value to indicate that we are in a native function call
		callstack.Push(std::pair<unsigned int, Closure*>(iptr, curframe));
		callstack.Push(std::pair<unsigned int, Closure*>(JET_BAD_INSTRUCTION, 0));
		Closure* temp = curframe;
		sptr += temp->prototype->locals;
		curframe = 0;
		Value ret = (*fun->func)(this,tmp,args);
		stack.QuickPop(args);
		sptr -= temp->prototype->locals;		
		curframe = temp;
		callstack.QuickPop(2);
		stack.Push(ret);
		return iptr;
	}
	else if (fun->type == ValueType::Object)
	{
		Value* tmp = &stack.mem[stack.size()-args];
		Value ret;
		if(fun->TryCallMetamethod("_call", tmp, args, &ret))
		{
			stack.QuickPop(args);
			stack.Push(ret);
			return iptr;
		}
		stack.QuickPop(args);
	}
	throw RuntimeException("Cannot call non function type " + std::string(fun->Type()) + "!!!");
}

Value JetContext::Execute(int iptr, Closure* frame)
{
#ifdef JET_TIME_EXECUTION
	INT64 start, rate, end;
	QueryPerformanceFrequency( (LARGE_INTEGER *)&rate );
	QueryPerformanceCounter( (LARGE_INTEGER *)&start );
#endif
	//frame and stack pointer reset
	unsigned int startcallstack = this->callstack.size();
	unsigned int startstack = this->stack.size();
	auto startlocalstack = this->sptr;

	callstack.Push(std::pair<unsigned int, Closure*>(JET_BAD_INSTRUCTION, 0));//bad value to get it to return;
	curframe = frame;

	//printf("Execute: Stack Ptr At: %d\n", sptr - localstack);

	try
	{
		while(curframe && iptr < curframe->prototype->instructions.size() && iptr >= 0)
		{
			Instruction in = curframe->prototype->instructions[iptr];
			switch(in.instruction)
			{
			case InstructionType::Add:
				{
					Value one = stack.Pop();
					Value two = stack.Pop();
					stack.Push(two+one);
					break;
				}
			case InstructionType::Sub:
				{
					Value one = stack.Pop();
					Value two = stack.Pop();
					stack.Push(two-one);
					break;
				}
			case InstructionType::Mul:
				{
					Value one = stack.Pop();
					Value two = stack.Pop();
					stack.Push(two*one);
					break;
				}
			case InstructionType::Div:
				{
					Value one = stack.Pop();
					Value two = stack.Pop();
					stack.Push(two/one);
					break;
				}
			case InstructionType::Modulus:
				{
					Value one = stack.Pop();
					Value two = stack.Pop();
					stack.Push(two%one);
					break;
				}
			case InstructionType::BAnd:
				{
					Value one = stack.Pop();
					Value two = stack.Pop();
					stack.Push(two&one);
					break;
				}
			case InstructionType::BOr:
				{
					Value one = stack.Pop();
					Value two = stack.Pop();
					stack.Push(two|one);
					break;
				}
			case InstructionType::Xor:
				{
					Value one = stack.Pop();
					Value two = stack.Pop();
					stack.Push(two^one);
					break;
				}
			case InstructionType::BNot:
				{
					Value one = stack.Pop();
					stack.Push(~one);
					break;
				}
			case InstructionType::LeftShift:
				{
					Value one = stack.Pop();
					Value two = stack.Pop();
					stack.Push(two<<one);
					break;
				}
			case InstructionType::RightShift:
				{
					Value one = stack.Pop();
					Value two = stack.Pop();
					stack.Push(two>>one);
					break;
				}
			case InstructionType::Incr:
				{
					Value one = stack.Pop();

					stack.Push(one+Value(1));
					break;
				}
			case InstructionType::Decr:
				{
					Value one = stack.Pop();

					stack.Push(one-Value(1));
					break;
				}
			case InstructionType::Negate:
				{
					Value one = stack.Pop();
					stack.Push(-one);
					break;
				}
			case InstructionType::Eq:
				{
					Value one = stack.Pop();
					Value two = stack.Pop();

					if (one == two)
						stack.Push(Value(1));
					else
						stack.Push(Value(0));

					break;
				}
			case InstructionType::NotEq:
				{
					Value one = stack.Pop();
					Value two = stack.Pop();

					if (one == two)
						stack.Push(Value(0));
					else
						stack.Push(Value(1));

					break;
				}
			case InstructionType::Lt:
				{
					Value one = stack.Pop();
					Value two = stack.Pop();

					if ((double)one > (double)two)
						stack.Push(Value(1));
					else
						stack.Push(Value(0));

					break;
				}
			case InstructionType::Gt:
				{
					Value one = stack.Pop();
					Value two = stack.Pop();

					if ((double)one < (double)two)
						stack.Push(Value(1));
					else
						stack.Push(Value(0));

					break;
				}
			case InstructionType::GtE:
				{
					Value one = stack.Pop();
					Value two = stack.Pop();

					if ((double)one <= (double)two)
						stack.Push(Value(1));
					else
						stack.Push(Value(0));

					break;
				}
			case InstructionType::LtE:
				{
					Value one = stack.Pop();
					Value two = stack.Pop();

					if ((double)one >= (double)two)
						stack.Push(Value(1));
					else
						stack.Push(Value(0));

					break;
				}
			case InstructionType::LdNull:
				{
					stack.Push(Value());
					break;
				}
			case InstructionType::LdNum:
				{
					stack.Push(in.lit);
					break;
				}
			case InstructionType::LdStr:
				{
					stack.Push(Value(in.strlit));
					break;
				}
			case InstructionType::Jump:
				{
					iptr = in.value-1;
					break;
				}
			case InstructionType::JumpTrue:
				{
					auto temp = stack.Pop();
					switch (temp.type)
					{
					case ValueType::Number:
						if (temp.value != 0.0)
							iptr = in.value-1;
						break;
					case ValueType::Null:
						break;
					default:
						iptr = in.value-1;
					}
					//if ((int)temp)
					//	iptr = in.value-1;
					break;
				}
			case InstructionType::JumpTruePeek:
				{
					auto temp = stack.Peek();
					switch (temp.type)
					{
					case ValueType::Number:
						if (temp.value != 0.0)
							iptr = in.value-1;
						break;
					case ValueType::Null:
						break;
					default:
						iptr = in.value-1;
					}
					//if ((int)temp)
					//	iptr = in.value-1;
					break;
				}
			case InstructionType::JumpFalse:
				{
					auto temp = stack.Pop();
					switch (temp.type)
					{
					case ValueType::Number:
						if (temp.value == 0.0)
							iptr = in.value-1;
						break;
					case ValueType::Null:
						iptr = in.value-1;
						break;
					}
					//if (!(int)temp)
					//iptr = in.value-1;
					break;
				}
			case InstructionType::JumpFalsePeek:
				{
					auto temp = stack.Peek();
					switch (temp.type)
					{
					case ValueType::Number:
						if (temp.value == 0.0)
							iptr = in.value-1;
						break;
					case ValueType::Null:
						iptr = in.value-1;
						break;
					}
					break;
				}
			case InstructionType::Load:
				{
					stack.Push(vars[in.value]);

					break;
				}
			case InstructionType::Store:
				{
					auto temp = stack.Pop();
					//store me
					vars[in.value] = temp;
					break;
				}
			case InstructionType::LLoad:
				{
					//printf("Load at: Stack Ptr: %d\n", sptr - localstack + in.value);
					stack.Push(sptr[in.value]);
					break;
				}
			case InstructionType::LStore:
				{
					//printf("Store at: Stack Ptr: %d\n", sptr - localstack + in.value);
					sptr[in.value] = stack.Pop();
					break;
				}
			case InstructionType::CLoad:
				{
					auto frame = curframe;
					int index = in.value2;
					while ( index++ < 0)
						frame = frame->prev;

					stack.Push(*frame->upvals[in.value]->v);
					//printf("Read Capture %d %s in %s\n", in.value, frame->upvals[in.value]->v->ToString().c_str(), frame->prototype->name.c_str());

					break;
				}
			case InstructionType::CStore:
				{
					auto frame = curframe;
					int index = in.value2;
					while ( index++ < 0)
						frame = frame->prev;

					if (frame->mark)
					{
						frame->mark = false;
						gc.greys.Push(frame);
					}

					if (frame->upvals[in.value]->closed)
					{
						frame->upvals[in.value]->value = stack.Pop();

						//fix up this write barrier
						//do a write barrier
						/*if (frame->upvals[in.value]->value.type > ValueType::NativeFunction && frame->upvals[in.value]->value._object->grey == false)
						{
						frame->upvals[in.value]->value._object->grey = true;
						this->gc.greys.Push(frame->upvals[in.value]->value);
						}*/
					}
					else
					{
						*frame->upvals[in.value]->v = stack.Pop();
					}
					//printf("Wrote Capture %d %s in %s\n", in.value, frame->upvals[in.value]->v->ToString().c_str(), frame->prototype->name.c_str());

					break;
				}
			case InstructionType::LoadFunction:
				{
					//construct a new closure with the right number of upvalues
					//from the Func* object
					Closure* closure = new Closure;
					closure->grey = closure->mark = false;
					closure->prev = curframe;
					closure->refcount = 0;
					closure->generator = 0;
					closure->numupvals = in.func->upvals;
					if (in.func->upvals)
					{
						closure->upvals = new Capture*[in.func->upvals];
						//#ifdef _DEBUG
						for (unsigned int i = 0; i < in.func->upvals; i++)
							closure->upvals[i] = 0;//this is done for the GC
						//#endif
						this->lastadded = closure;
					}

					closure->prototype = in.func;
					closure->type = ValueType::Function;
					gc.AddObject((GarbageCollector::gcval*)closure);
					stack.Push(Value(closure));

					if (gc.allocationCounter++%GC_INTERVAL == 0)
						this->RunGC();

					break;
				}
			case InstructionType::CInit:
				{
					//allocate and add new upvalue
					auto frame = lastadded;
					//first see if we already have this closure open for this variable
					bool found = false;
					for (auto ii: opencaptures)
					{
						if (ii.capture->v == &sptr[in.value])
						{
							//we found it
							frame->upvals[in.value2] = ii.capture;
							found = true;
							//printf("Reused Capture %d %s in %s\n", in.value2, sptr[in.value].ToString().c_str(), curframe->prototype->name.c_str());

							if (frame->mark)
							{
								frame->mark = false;
								gc.greys.Push(frame);
							}
							break;
						}
					}

					if (!found)
					{
						//allocate closure here
						auto capture = gc.New<Capture>();
						capture->closed = false;
						capture->grey = capture->mark = false;
						capture->refcount = 0;
						capture->type = ValueType::Capture;
						capture->v = &sptr[in.value];
#ifdef _DEBUG
						capture->usecount = 1;
						capture->owner = frame;
#endif
						frame->upvals[in.value2] = capture;

						OpenCapture c;
						c.capture = capture;
#ifdef _DEBUG
						c.creator = frame->prev;
#endif
						//printf("Initalized Capture %d %s in %s\n", in.value2, sptr[in.value].ToString().c_str(), curframe->prototype->name.c_str());
						this->opencaptures.push_back(c);

						if (frame->mark)
						{
							frame->mark = false;
							gc.greys.Push(frame);
						}

						if (gc.allocationCounter++%GC_INTERVAL == 0)
							this->RunGC();
					}

					break;
				}
			case InstructionType::Close:
				{
					//remove from the back
					while (opencaptures.size() > 0)
					{
						auto cur = opencaptures.back();
						int index = cur.capture->v-sptr;
						if (index < in.value)
							break;

#ifdef _DEBUG
						//this just verifies that the break above works right
						if (cur.creator != this->curframe)
							throw RuntimeException("RUNTIME ERROR: Tried to close capture in wrong scope!");
#endif

						cur.capture->closed = true;
						cur.capture->value = *cur.capture->v;
						cur.capture->v = &cur.capture->value;
						//printf("Closed capture with value %s\n", cur->value.ToString().c_str());
						//printf("Closed capture %d in %d as %s\n", i, cur, cur->upvals[i]->v->ToString().c_str());

						//do a write barrier
						if (cur.capture->value.type > ValueType::NativeFunction && cur.capture->value._object->grey == false)
						{
							cur.capture->value._object->grey = true;
							this->gc.greys.Push(cur.capture->value);
						}
						opencaptures.pop_back();
					}

					break;
				}
			case InstructionType::Call:
				{
					iptr = this->Call(&vars[in.value], iptr, in.value2);

					break;

					//find the variable name from the in.value which is the index into the variable array
					std::string var;
					for (auto ii: variables)
					{
						if (ii.second == in.value)
						{
							var = ii.first;
							break;
						}
					}
					throw RuntimeException("Cannot call non function '" + var + " of type " + vars[in.value].Type() + "'!!!");

					break;
				}
			case InstructionType::ECall:
				{
					//allocate capture area here
					Value fun = stack.Pop();
					iptr = this->Call(&fun, iptr, in.value);

					//throw RuntimeException("Cannot call non function type " + std::string(fun.Type()) + "!!!");

					break;
				}
			case InstructionType::Return:
				{
					auto oframe = callstack.Pop();
					iptr = oframe.first;
					if (curframe && curframe->generator)
						curframe->generator->Kill();

					if (oframe.first != JET_BAD_INSTRUCTION)
					{
#ifdef _DEBUG
						//this makes sure that the gc doesnt overrun its boundaries
						for (int i = 0; i < oframe.second->prototype->locals; i++)
						{
							//need to mark stack with garbage values for error checking
							sptr[i].type = ValueType::Object;
							sptr[i]._object = (JetObject*)0xcdcdcdcd;
						}
#endif
						sptr -= oframe.second->prototype->locals;
					}
					//printf("Return: Stack Ptr At: %d\n", sptr - localstack);
					curframe = oframe.second;

					break;
				}
			case InstructionType::Yield:
				{
					if (curframe->generator)
						curframe->generator->Yield(this, iptr);
					else
						throw RuntimeException("Cannot Yield from outside a generator");

					auto oframe = callstack.Pop();
					iptr = oframe.first;
					curframe = oframe.second;
					if (oframe.second)
						sptr -= oframe.second->prototype->locals;

					break;
				}
			case InstructionType::Resume:
				{
					//resume last item placed on stack
					Value v = this->stack.Pop();
					if (v.type != ValueType::Function || v._function->generator == 0)
						throw RuntimeException("Cannot resume a non generator!");

					callstack.Push(std::pair<unsigned int, Closure*>(iptr, curframe));

					sptr += curframe->prototype->locals;

					if ((sptr - localstack) >= JET_STACK_SIZE)
						throw RuntimeException("Stack Overflow!");

					curframe = v._function;

					iptr = v._function->generator->Resume(this)-1;

					break;
				}
			case InstructionType::Dup:
				{
					stack.Push(stack.Peek());
					break;
				}
			case InstructionType::Pop:
				{
					stack.Pop();
					break;
				}
			case InstructionType::StoreAt:
				{
					if (in.string)
					{
						Value loc = stack.Pop();
						Value val = stack.Pop();	

						if (loc.type == ValueType::Object)
							(*loc._object)[in.string] = val;
						else
							throw RuntimeException("Could not index a non array/object value!");

						//this may be redundant and already done in object
						//check me
						if (loc._object->mark)
						{
							//reset to grey and push back for reprocessing
							//printf("write barrier triggered!\n");
							loc._object->mark = false;
							gc.greys.Push(loc);//push to grey stack
						}
						//write barrier
					}
					else
					{
						Value index = stack.Pop();
						Value loc = stack.Pop();
						Value val = stack.Pop();	

						if (loc.type == ValueType::Array)
						{
							int in = (int)index;
							if (in >= loc._array->data.size() || in < 0)
								throw RuntimeException("Array index out of range!");
							loc._array->data[in] = val;

							//write barrier
							if (loc._array->mark)
							{
								//reset to grey and push back for reprocessing
								//printf("write barrier triggered!\n");
								loc._array->mark = false;
								gc.greys.Push(loc);//push to grey stack
							}
						}
						else if (loc.type == ValueType::Object)
						{
							(*loc._object)[index] = val;

							//write barrier
							//this may be redundant, lets check
							//its also done in the object object
							if (loc._object->mark)
							{
								//reset to grey and push back for reprocessing
								//printf("write barrier triggered!\n");
								loc._object->mark = false;
								gc.greys.Push(loc);//push to grey stack
							}
						}
						else if (loc.type == ValueType::String)
						{
							int in = (int)index;
							if (in >= loc.length || in < 0)
								throw RuntimeException("String index out of range!");

							loc._string->data[in] = (int)val;
						}
						else
							throw RuntimeException("Could not index a non array/object value!");
					}
					break;
				}
			case InstructionType::LoadAt:
				{
					if (in.string)
					{
						Value loc = stack.Pop();
						if (loc.type == ValueType::Object)
						{
							auto n = loc._object->findNode(in.string);
							if (n)
							{
								stack.Push(n->second);
							}
							else
							{
								auto obj = loc._object->prototype;
								while (obj)
								{
									n = obj->findNode(in.string);
									if (n)
									{
										stack.Push(n->second);
										break;
									}
									obj = obj->prototype;
								}
							}
						}
						else if (loc.type == ValueType::String)
							stack.Push((*this->string)[in.string]);
						else if (loc.type == ValueType::Array)
							stack.Push((*this->Array)[in.string]);
						else if (loc.type == ValueType::Userdata)
							stack.Push((*loc._userdata->prototype)[in.string]);
						else if (loc.type == ValueType::Function && loc._function->prototype->generator)
							stack.Push((*this->function)[in.string]);
						else
							throw RuntimeException("Could not index a non array/object value!");
					}
					else
					{
						Value index = stack.Pop();
						Value loc = stack.Pop();

						if (loc.type == ValueType::Array)
						{
							int in = (int)index;
							if (in >= loc._array->data.size() || in < 0)
								throw RuntimeException("Array index out of range!");
							stack.Push(loc._array->data[in]);
						}
						else if (loc.type == ValueType::Object)
							stack.Push((*loc._object).get(index));
						else if (loc.type == ValueType::String)
						{
							int in = (int)index;
							if (in >= (int)loc.length || in < 0)
								throw RuntimeException("String index out of range!");

							stack.Push(Value(loc._string->data[in]));
						}

						else
							throw RuntimeException("Could not index a non array/object value!");
					}
					break;
				}
			case InstructionType::NewArray:
				{
					auto arr = new JetArray;//GCVal<std::vector<Value>>();
					arr->grey = arr->mark = false;
					arr->refcount = 0;
					arr->context = this;
					arr->type = ValueType::Array;
					this->gc.gen1.push_back((GarbageCollector::gcval*)arr);
					arr->data.resize(in.value);
					for (int i = in.value-1; i >= 0; i--)
						arr->data[i] = stack.Pop();
					stack.Push(Value(arr));

					if (gc.allocationCounter++%GC_INTERVAL == 0)
						this->RunGC();

					break;
				}
			case InstructionType::NewObject:
				{
					auto obj = new JetObject(this);
					obj->grey = obj->mark = false;
					obj->refcount = 0;
					obj->type = ValueType::Object;
					this->gc.gen1.push_back((GarbageCollector::gcval*)obj);
					for (int i = in.value-1; i >= 0; i--)
					{
						auto value = stack.Pop();
						auto key = stack.Pop();
						(*obj)[key] = value;
					}
					stack.Push(Value(obj));

					if (gc.allocationCounter++%GC_INTERVAL == 0)
						this->RunGC();

					break;
				}
			default:
				throw RuntimeException("Unimplemented Instruction!");
			}

			iptr++;
		}
	}
	catch(RuntimeException e)
	{
		if (e.processed == false)
		{
			printf("RuntimeException: %s\nCallstack:\n", e.reason.c_str());

			//generate call stack
			this->StackTrace(iptr, curframe);

			if (curframe && curframe->prototype->locals)
			{
				printf("\nLocals:\n");
				for (unsigned int i = 0; i < curframe->prototype->locals; i++)
				{
					Value v = this->sptr[i];
					if (v.type >= ValueType(0))
						printf("%s = %s\n", curframe->prototype->debuglocal[i].c_str(), v.ToString().c_str());
				}
			}

			if (curframe && curframe->prototype->upvals)
			{
				printf("\nCaptures:\n");
				for (unsigned int i = 0; i < curframe->prototype->upvals; i++)
				{
					Value v = *curframe->upvals[i]->v;
					if (v.type >= ValueType(0))
						printf("%s = %s\n", curframe->prototype->debugcapture[i].c_str(), v.ToString().c_str());
				}
			}

			printf("\nGlobals:\n");
			for (auto ii: variables)
			{
				if (vars[ii.second].type != ValueType::Null)
					printf("%s = %s\n", ii.first.c_str(), vars[ii.second].ToString().c_str());
			}
			e.processed = true;
		}

		//make sure I reset everything in the event of an error

		//clear the stacks
		this->callstack.QuickPop(this->callstack.size()-startcallstack);
		this->stack.QuickPop(this->stack.size()-startstack);

		//reset the local variable stack
		this->sptr = startlocalstack;

		//ok add the exception details to the exception as a string or something rather than just printing them
		//maybe add more details to the exception when rethrowing
		throw e;
	}
	catch(...)
	{
		//this doesnt work right
		printf("Caught Some Other Exception\n\nCallstack:\n");

		this->StackTrace(iptr, curframe);

		printf("\Globals:\n");
		for (auto ii: variables)
		{
			printf("%s = %s\n", ii.first.c_str(), vars[ii.second].ToString().c_str());
		}

		//ok, need to properly roll back callstack
		this->callstack.QuickPop(this->callstack.size()-startcallstack);
		this->stack.QuickPop(this->stack.size()-startstack);

		//reset the local variable stack
		this->sptr = startlocalstack;

		//rethrow the exception
		auto exception = RuntimeException("Unknown Exception Thrown From Native!");
		exception.processed = true;
		throw exception;
	}


#ifdef JET_TIME_EXECUTION
	QueryPerformanceCounter( (LARGE_INTEGER *)&end );

	INT64 diff = end - start;
	double dt = ((double)diff)/((double)rate);

	printf("Took %lf seconds to execute\n\n", dt);
#endif

#ifdef _DEBUG
	//debug checks for stack and what not
	if (this->callstack.size() == 0)
	{
		if (this->sptr != this->localstack)
			throw RuntimeException("FATAL ERROR: Local stack did not properly reset");
	}

	//check for stack leaks
	if (this->stack.size() > startstack+1)
	{
		this->stack.QuickPop(stack.size());
		throw RuntimeException("FATAL ERROR: Stack leak detected!");
	}
#endif

	return stack.Pop();
}

void JetContext::GetCode(int ptr, Closure* closure, std::string& ret, unsigned int& line)
{
	if (closure->prototype->debuginfo.size() == 0)//make sure we have debug info
	{
		ret = "No Debug Line Info";
		line = 0;
		return;
	}

	int imax = closure->prototype->debuginfo.size()-1;
	int imin = 0;
	while (imax >= imin)
	{
		// calculate the midpoint for roughly equal partition
		int imid = (imin+imax)/2;//midpoint(imin, imax);
		if(closure->prototype->debuginfo[imid].code == ptr)
		{
			// key found at index imid
			ret = closure->prototype->debuginfo[imid].file;
			line = closure->prototype->debuginfo[imid].line;
			return;// imid; 
		}
		// determine which subarray to search
		else if (closure->prototype->debuginfo[imid].code < ptr)
			// change min index to search upper subarray
			imin = imid + 1;
		else         
			// change max index to search lower subarray
			imax = imid - 1;
	}
#undef min
#undef max
	unsigned int index = std::max(std::min(imin, imax),0);

	ret = closure->prototype->debuginfo[index].file;
	line = closure->prototype->debuginfo[index].line;
}

void JetContext::StackTrace(int curiptr, Closure* cframe)
{
	auto tempcallstack = this->callstack.Copy();
	if (curframe)
		tempcallstack.Push(std::pair<unsigned int, Closure*>(curiptr,cframe));

	while(tempcallstack.size() > 0)
	{
		auto top = tempcallstack.Pop();
		int greatest = -1;

		if (top.first == JET_BAD_INSTRUCTION)
			printf("{Native}\n");
		else
		{
			std::string fun = top.second->prototype->name;
			std::string file;
			unsigned int line;
			this->GetCode(top.first, top.second, file, line);
			printf("%s() %s Line %d (Instruction %d)\n", fun.c_str(), file.c_str(), line, top.first);
		}
	}
}

Value JetContext::Assemble(const std::vector<IntermediateInstruction>& code)
{
#ifdef JET_TIME_EXECUTION
	INT64 start, rate, end;
	QueryPerformanceFrequency( (LARGE_INTEGER *)&rate );
	QueryPerformanceCounter( (LARGE_INTEGER *)&start );
#endif
	std::map<std::string, unsigned int> labels;
	int labelposition = 0;

	for (auto inst: code)
	{
		switch (inst.type)
		{
		case InstructionType::Capture:
		case InstructionType::Local:
		case InstructionType::DebugLine:
			{
				break;
			}
		case InstructionType::Function:
			{
				labelposition = 0;

				//do something with argument and local counts
				Function* func = new Function;
				func->args = inst.a;
				func->locals = inst.b;
				func->upvals = inst.c;
				func->name = inst.string;
				func->context = this;
				func->generator = inst.d & 2 ? true : false;
				func->vararg = inst.d & 1? true : false;

				if (functions.find(inst.string) == functions.end())
					functions[inst.string] = func;
				else if (strcmp(inst.string, "{Entry Point}") == 0)
				{
					//have to do something with old entry point because it leaks
					entrypoints.push_back(functions[inst.string]);
					functions[inst.string] = func;
				}
				else
					throw RuntimeException("ERROR: Duplicate Function Label Name: %s\n" + std::string(inst.string));

				break;
			}
		case InstructionType::Label:
			{
				if (labels.find(inst.string) == labels.end())
				{
					labels[inst.string] = labelposition;
					delete[] inst.string;
				}
				else
				{
					delete[] inst.string;
					throw RuntimeException("ERROR: Duplicate Label Name: %s\n" + std::string(inst.string));
				}
				break;
			}
		default:
			{
				labelposition++;
			}
		}
	}

	Function* current = 0;
	for (auto inst: code)
	{
		switch (inst.type)
		{
		case InstructionType::Local:
			{
				current->debuglocal.push_back(inst.string);

				delete[] inst.string;
				break;
			}
		case InstructionType::Capture:
			{
				current->debugcapture.push_back(inst.string);

				delete[] inst.string;
				break;
			}
		case InstructionType::Label:
			{
				break;
			}
		case InstructionType::DebugLine:
			{
				//this should contain line/file info
				Function::DebugInfo info;
				info.file = inst.string;
				info.line = (unsigned int)inst.second;
				info.code = current->instructions.size();
				current->debuginfo.push_back(info);
				//push something into the array at the instruction pointer
				delete[] inst.string;

				break;
			}
		case InstructionType::Function:
			{
				current = this->functions[inst.string];
				delete[] inst.string;
				break;
			}
		default:
			{
				Instruction ins;
				ins.instruction = inst.type;
				ins.string = inst.string;
				ins.value = inst.first;
				if (inst.string == 0 || inst.type == InstructionType::Call)
					ins.value2 = (int)inst.second;

				switch (inst.type)
				{
				case InstructionType::Call:
				case InstructionType::Store:
				case InstructionType::Load:
					{
						if (variables.find(inst.string) == variables.end())
						{
							//add it
							variables[inst.string] = variables.size();
							vars.push_back(Value());
						}
						ins.value = variables[inst.string];
						delete[] inst.string;
						break;
					}
				case InstructionType::LdStr:
					{
						Value str = this->NewString(inst.string, false);
						str.AddRef();
						ins.strlit = str._string;
						break;
					}
				case InstructionType::LdNum:
					{
						ins.lit = inst.second;
						break;
					}
				case InstructionType::LoadFunction:
					{
						ins.func = functions[inst.string];
						delete[] inst.string;
						break;
					}
				case InstructionType::Jump:
				case InstructionType::JumpFalse:
				case InstructionType::JumpTrue:
				case InstructionType::JumpFalsePeek:
				case InstructionType::JumpTruePeek:
					{
						if (labels.find(inst.string) == labels.end())
							throw RuntimeException("Label '" + (std::string)inst.string + "' does not exist!");
						ins.value = labels[inst.string];
						break;
					}
				case InstructionType::ForEach:
					{
						if (labels.find(inst.string) == labels.end())
							throw RuntimeException("Label '" + (std::string)inst.string + "' does not exist!");
						ins.value = labels[inst.string];
						if (labels.find(inst.string2) == labels.end())
							throw RuntimeException("Label '" + (std::string)inst.string2 + "' does not exist!");
						ins.value2 = labels[inst.string2];

						delete[] inst.string;
						delete[] inst.string2;
						break;
					}
				}
				current->instructions.push_back(ins);
			}
		}
	}

	if (labels.size() > 100000)
		throw CompilerException("test", 5, "problem with labels!");

	auto frame = new Closure;
	frame->grey = frame->mark = false;
	frame->refcount = 0;
	frame->prev = 0;
	frame->generator = 0;
	frame->prototype = this->functions["{Entry Point}"];
	frame->numupvals = frame->prototype->upvals;
	frame->type = ValueType::Function;
	//if (frame->numupvals)
	//frame->upvals = new Value*[frame->numupvals];
	//else
	frame->upvals = 0;

	gc.AddObject((GarbageCollector::gcval*)frame);

#ifdef JET_TIME_EXECUTION
	QueryPerformanceCounter( (LARGE_INTEGER *)&end );

	INT64 diff = end - start;
	double dt = ((double)diff)/((double)rate);

	printf("Took %lf seconds to assemble\n\n", dt);
#endif

	return frame;
};


Value JetContext::Call(const Value* fun, Value* args, unsigned int numargs)
{
	if (fun->type != ValueType::NativeFunction && fun->type != ValueType::Function)
	{
		printf("ERROR: Variable is not a function\n");
		return Value();
	}
	else if (fun->type == ValueType::NativeFunction)
	{
		//call it
		int s = this->stack.size();
		(*fun->func)(this,args,numargs);
		if (s == this->stack.size())
			return Value();

		return this->stack.Pop();
	}
	else if (fun->_function->generator)
	{
		if (curframe)
			sptr += curframe->prototype->locals;

		if ((sptr - localstack) >= JET_STACK_SIZE)
			throw RuntimeException("Stack Overflow!");

		curframe = fun->_function;

		if (numargs == 0)
			stack.Push(Value());
		else if (numargs >= 1)
			stack.Push(args[0]);
		int iptr = fun->_function->generator->Resume(this);

		return this->Execute(iptr, fun->_function);
	}

	if (fun->_function->prototype->generator)
	{
		//create generator and return it
		Closure* closure = new Closure;
		closure->grey = closure->mark = false;
		closure->refcount = 0;

		closure->prev = fun->_function->prev;
		closure->numupvals = fun->_function->numupvals;
		closure->generator = new Generator(fun->_function->prototype->context, fun->_function, numargs);
		if (closure->numupvals)
		{
			closure->upvals = new Capture*[closure->numupvals];
			memset(closure->upvals, 0, sizeof(Capture*)*closure->numupvals);
		}
		closure->prototype = fun->_function->prototype;
		closure->type = ValueType::Function;
		this->gc.AddObject((GarbageCollector::gcval*)closure);

		return Value(closure);
	}

	bool pushed = false;
	if (this->curframe)
	{
		//need to advance stack pointer
		sptr += curframe->prototype->locals;
		pushed = true;
		this->callstack.Push(std::pair<unsigned int, Closure*>(0, curframe));
	}

	//clear stack values for the gc
	for (unsigned int i = 0; i < fun->_function->prototype->locals; i++)
		sptr[i] = Value();

	//push args onto stack
	for (unsigned int i = 0; i < numargs; i++)
		this->stack.Push(args[i]);

	auto func = fun->_function;
	if (numargs <= func->prototype->args)
	{
		for (int i = func->prototype->args-1; i >= 0; i--)
		{
			if (i < numargs)
				sptr[i] = stack.Pop();
			else
				sptr[i] = Value();
		}
	}
	else if (func->prototype->vararg)
	{
		sptr[func->prototype->locals-1] = this->NewArray();
		auto arr = &sptr[func->prototype->locals-1]._array->data;
		arr->resize(numargs - func->prototype->args);
		for (int i = numargs-1; i >= 0; i--)
		{
			if (i < func->prototype->args)
				sptr[i] = stack.Pop();
			else
				(*arr)[i] = stack.Pop();
		}
	}
	else
	{
		for (int i = numargs-1; i >= 0; i--)
		{
			if (i < func->prototype->args)
				sptr[i] = stack.Pop();
			else
				stack.Pop();
		}
	}

	Value ret = this->Execute(0, fun->_function);

	if (pushed)
	{
		curframe = this->callstack.Pop().second;//restore
		sptr -= curframe->prototype->locals;
	}
	return ret;
}

//executes a function in the VM context
Value JetContext::Call(const char* function, Value* args, unsigned int numargs)
{
	if (variables.find(function) == variables.end())
	{
		printf("ERROR: No variable named: '%s' to call\n", function);
		return Value();
	}

	Value fun = vars[variables[function]];
	if (fun.type != ValueType::NativeFunction && fun.type != ValueType::Function)
	{
		printf("ERROR: Variable '%s' is not a function\n", function);
		return Value();
	}

	return this->Call(&fun, args, numargs);
};

std::string JetContext::Script(const std::string code, const std::string filename)
{
	try
	{
		//try and execute
		return this->Script(code.c_str(), filename.c_str()).ToString();
	}
	catch(CompilerException E)
	{
		printf("Exception found:\n");
		printf("%s (%d): %s\n", E.file.c_str(), E.line, E.ShowReason());
		return "";
	}
}

Value JetContext::Script(const char* code, const char* filename)//compiles, assembles and executes the script
{
	auto asmb = this->Compile(code, filename);
	Value fun = this->Assemble(asmb);

	return this->Call(&fun);
}

#ifdef EMSCRIPTEN
#include <emscripten/bind.h>
using namespace emscripten;
using namespace std;
EMSCRIPTEN_BINDINGS(Jet) {
	class_<Jet::JetContext>("JetContext")
		.constructor()
		.function<std::string, std::string, std::string>("Script", &Jet::JetContext::Script);
}
#endif