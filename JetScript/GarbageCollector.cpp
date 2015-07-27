#include "GarbageCollector.h"
#include "JetContext.h"

using namespace Jet;

//#define JETGCDEBUG

GarbageCollector::GarbageCollector(JetContext* context) : context(context)
{
	this->allocationCounter = 1;//messes up if these start at 0
	this->collectionCounter = 1;
}

void GarbageCollector::Cleanup()
{
	//need to do a two pass system to properly destroy userdata
	//first pass of destructing
	for (auto ii: this->gen1)
	{
		switch (ii->type)
		{
		case ValueType::Function:
		case ValueType::Object:
		case ValueType::Array:
		case ValueType::String:
			break;
		case ValueType::Userdata:
			{
				Value ud = Value(((JetUserdata*)ii), ((JetUserdata*)ii)->prototype);
				Value _gc = (*((JetUserdata*)ii)->prototype).get("_gc");
				if (_gc.type == ValueType::NativeFunction)
					_gc.func(this->context, &ud, 1);
				else if (_gc.type == ValueType::Function)
					throw RuntimeException("Non Native _gc Hooks Not Implemented!");//todo
				else if (_gc.type != ValueType::Null)
					throw RuntimeException("Invalid _gc Hook!");
				break;
			}
		}
	}

	for (auto ii: this->gen2)
	{
		switch (ii->type)
		{
		case ValueType::Function:
		case ValueType::Object:
		case ValueType::Array:
		case ValueType::String:
			break;
		case ValueType::Userdata:
			{
				Value ud = Value(((JetUserdata*)ii), ((JetUserdata*)ii)->prototype);
				Value _gc = (*((JetUserdata*)ii)->prototype).get("_gc");
				if (_gc.type == ValueType::NativeFunction)
					_gc.func(this->context, &ud, 1);
				else if (_gc.type == ValueType::Function)
					throw RuntimeException("Non Native _gc Hooks Not Implemented!");//todo
				else if (_gc.type != ValueType::Null)
					throw RuntimeException("Invalid _gc Hook!");
				break;
			}
		}
	}

	//delete everything else
	for (auto ii: this->gen1)
	{
		switch (ii->type)
		{
		case ValueType::Function:
			{
				Closure* fun = (Closure*)ii;
				if (fun->numupvals)
					delete[] fun->upvals;
				delete fun->generator;
				delete fun;
				break;
			}
		case ValueType::Object:
			delete (JetObject*)ii;
			break;
		case ValueType::Array:
			((JetArray*)ii)->data.~vector();
			delete[] (char*)ii;
			break;
		case ValueType::Userdata:
			//did in first pass
			delete (JetUserdata*)ii;
			break;
		case ValueType::String:
			{
				JetString* str = (JetString*)ii;
				delete[] str->data;
				delete str;
				break;
			}
		case ValueType::Capture:
			{
				Capture* c = (Capture*)ii;
				delete c;
				break;
			}
		}
	}

	for (auto ii: this->gen2)
	{
		switch (ii->type)
		{
		case ValueType::Function:
			{
				Closure* fun = (Closure*)ii;
				if (fun->numupvals)
					delete[] fun->upvals;
				delete fun->generator;
				delete fun;
				break;
			}
		case ValueType::Object:
			delete (JetObject*)ii;
			break;
		case ValueType::Array:
			delete ((JetArray*)ii);
			break;
		case ValueType::Userdata:
			//did in first pass
			delete (JetUserdata*)ii;
			break;
		case ValueType::String:
			{
				JetString* str = (JetString*)ii;
				delete[] str->data;
				delete str;
				break;
			}
		case ValueType::Capture:
			{
				Capture* c = (Capture*)ii;
				delete c;
				break;
			}
		}
	}
}

void GarbageCollector::Mark()
{
	//mark basic types
	this->greys.Push(context->Array);
	this->greys.Push(context->arrayiter);
	this->greys.Push(context->object);
	this->greys.Push(context->objectiter);
	this->greys.Push(context->string);
	this->greys.Push(context->function);

	for (unsigned int i = 0; i < this->context->prototypes.size(); i++)
		this->greys.Push(this->context->prototypes[i]);

	//mark all objects being held by native code
	for (unsigned int i = 0; i < this->nativeRefs.size(); i++)
	{
		if (this->nativeRefs[i]._object->grey == false)
		{
			this->nativeRefs[i]._object->grey = true;
			this->greys.Push(this->nativeRefs[i]);
		}
	}

	//add more write barriers to detect when objects are removed and what not
	//if flag is marked, then black
	//if no flag and grey bit, then grey
	//if no flag or grey bit, then white

	//push all reachable items onto grey stack
	//this means globals
	{
		//StackProfile profile("Mark Globals as Grey");
		for (unsigned int i = 0; i < context->vars.size(); i++)
		{
			if (context->vars[i].type > ValueType::NativeFunction)
			{
				if (context->vars[i]._object->grey == false)
				{
					context->vars[i]._object->grey = true;
					this->greys.Push(context->vars[i]);
				}
			}
		}
	}


	if (context->stack.size() > 0)
	{
		//StackProfile prof("Make Stack Grey");
		for (unsigned int i = 0; i < context->stack.size(); i++)
		{
			if (context->stack.mem[i].type > ValueType::NativeFunction)
			{
				if (context->stack.mem[i]._object->grey == false)
				{
					context->stack.mem[i]._object->grey = true;
					this->greys.Push(context->stack.mem[i]);
				}
			}
		}
	}


	//this is really part of the sweep section
	if (context->callstack.size() > 0)
	{
		//StackProfile prof("Traverse/Mark Stack");
		//printf("GC run at runtime!\n");
		//traverse all local vars
		if (context->curframe && context->curframe->grey == false)
		{
			context->curframe->grey = true;
			this->greys.Push(Value(context->curframe));
		}

		int sp = 0;
		for (unsigned int i = 0; i < context->callstack.size(); i++)
		{
			auto closure = context->callstack[i].second;
			if (closure == 0)
				continue;

			if (closure->grey == false)
			{
				closure->grey = true;
				this->greys.Push(Value(closure));
			}
			int max = sp+closure->prototype->locals;
			for (; sp < max; sp++)
			{
				if (context->localstack[sp].type > ValueType::NativeFunction)
				{
					if (context->localstack[sp]._object->grey == false)
					{
						context->localstack[sp]._object->grey = true;
						this->greys.Push(context->localstack[sp]);
					}
				}
			}
		}

		//mark curframe locals
		if (context->curframe)
		{
			int max = sp+context->curframe->prototype->locals;
			for (; sp < max; sp++)
			{
				if (context->localstack[sp].type > ValueType::NativeFunction)
				{
					if (context->localstack[sp]._object->grey == false)
					{
						context->localstack[sp]._object->grey = true;
						this->greys.Push(context->localstack[sp]);
					}
				}
			}
		}
	}


	{
		//StackProfile prof("Traverse Greys");
		while(this->greys.size() > 0)
		{
			//traverse the object
			auto obj = this->greys.Pop();
			switch (obj.type)
			{
			case ValueType::Object:
				{
					//obj._object->DebugPrint();
					if (obj._object->prototype && obj._object->prototype->grey == false)
					{
						obj._object->prototype->grey = true;

						this->greys.Push(obj._object->prototype);
					}

					obj._object->mark = true;
					for (auto ii: *obj._object)
					{
						if (ii.first.type > ValueType::NativeFunction && ii.first._object->grey == false)
						{
							ii.first._object->grey = true;
							greys.Push(ii.first);
						}
						if (ii.second.type > ValueType::NativeFunction && ii.second._object->grey == false)
						{
							ii.second._object->grey = true;
							greys.Push(ii.second);
						}
					}
					break;
				}
			case ValueType::Array:
				{
					obj._array->mark = true;

					for (auto ii: obj._array->data)
					{
						if (ii.type > ValueType::NativeFunction && ii._object->grey == false)
						{
							ii._object->grey = true;
							greys.Push(ii);
						}
					}

					break;
				}
			case ValueType::String:
				{
					obj._string->mark = true;

					break;
				}
#ifdef _DEBUG
			case ValueType::Capture:
				{
					throw RuntimeException("There should not be an upvalue in the grey loop");
					break;
				}
#endif
			case ValueType::Function:
				{
					obj._function->mark = true;
					//printf("Function Marked\n");
					if (obj._function->prev && obj._function->prev->grey == false)
					{
						obj._function->prev->grey = true;
						greys.Push(Value(obj._function->prev));
					}

					if (obj._function->numupvals)
					{
						for (unsigned int i = 0; i < obj._function->numupvals; i++)
						{
							auto uv = obj._function->upvals[i];
							if (uv && uv->grey == false)
							{
								if (uv->closed)
								{
									//mark the value stored in it
									if (uv->value.type > ValueType::NativeFunction)
									{
										if (uv->value._object->grey == false)
										{
											uv->value._object->grey = true;
											greys.Push(uv->value);
										}
									}
								}
								//mark it
								uv->grey = true;
								uv->mark = true;
							}
						}
					}

					if (obj._function->generator)
					{
						//mark generator stack
						for (unsigned int i = 0; i < obj._function->prototype->locals; i++)
						{
							if (obj._function->generator->stack[i].type > ValueType::NativeFunction)
							{
								if (obj._function->generator->stack[i]._object->grey == false)
								{
									obj._function->generator->stack[i]._object->grey = true;
									greys.Push(obj._function->generator->stack[i]);
								}
							}
						}
					}
					break;
				}
			case ValueType::Userdata:
				{
					obj._userdata->mark = true;

					if (obj._userdata->prototype && obj._userdata->prototype->grey == false)
					{
						obj._userdata->prototype->grey = true;

						greys.Push(obj._userdata->prototype);
					}
					break;
				}
			}
		}
	}
}

void GarbageCollector::Sweep()
{
	bool nextIncremental = ((this->collectionCounter+1)%GC_STEPS)!=0;
	bool incremental = ((this->collectionCounter)%GC_STEPS)!=0;
	/*if (this->collectionCounter % GC_STEPS == 0)
	printf("Full Collection!\n");
	else
	printf("Incremental Collection!\n");*/

	/* SWEEPING SECTION */


	//this must all be done when sweeping!!!!!!!

	//process stack variables, stack vars are ALWAYS grey

	//finally sweep through
	//sweep and free all whites and make all blacks white
	//iterate through all gc values
#ifdef _DEBUG
	if (this->greys.size() > 0)
		throw RuntimeException("Runtime Error: Garbage collector grey array not empty when collecting!");
#endif

	if (!incremental)//do a gen2 collection
	{
		auto g2list = std::move(this->gen2);
		this->gen2.clear();
		for (auto ii: g2list)
		{
			if (ii->mark || ii->refcount)
			{
				ii->mark = false;
				ii->grey = false;

				this->gen2.push_back(ii);
			}
			else
			{
				//printf("Freeing Gen 2, %d!\n", ii->type);
				this->Free(ii);
			}
		}
	}

	auto g1list = std::move(this->gen1);
	this->gen1.clear();
	for (auto ii: g1list)
	{
		if (ii->mark || ii->refcount)
		{
			//ii->mark = false;//perhaps do this ONLY IF we just did a gen2 collection
			//ii->grey = false;
			this->gen2.push_back(ii);//promote, it SURVIVED

			//printf("Promoting %d!\n", ii->type);
		}
		else
		{
			//printf("Freeing %d!\n", ii->type);
			this->Free(ii);
		}
	}

	//Obviously, this approach doesn't work for a non-copying GC. But the main insights behind a generational GC can be abstracted:

	//Minor collections only take care of newly allocated objects.
	//Major collections deal with all objects, but are run much less often.

	//The basic idea is to modify the sweep phase: 
	//1. free the (unreachable) white objects, but don't flip the color of black objects before a minor collection. 
	//2. The mark phase of the following minor collection then only traverses newly allocated blocks and objects written to (marked gray). 
	//3. All other objects are assumed to be still reachable during a minor GC and are neither traversed, nor swept, nor are their marks changed (kept black). A regular sweep phase is used if a major collection is to follow.
}

void GarbageCollector::Run()
{
	//printf("Running GC: %d Greys, %d Globals, %d Stack\n%d Closures, %d Arrays, %d Objects, %d Userdata\n", this->greys.size(), this->vars.size(), 0, this->closures.size(), this->arrays.size(), this->objects.size(), this->userdata.size());
#ifdef JET_TIME_EXECUTION
	INT64 start, end;
	//QueryPerformanceFrequency( (LARGE_INTEGER *)&rate );
	QueryPerformanceCounter( (LARGE_INTEGER *)&start );
#endif
	//mark all references in the grey stack
	this->Mark();

	//clear up dead memory
	this->Sweep();

	this->collectionCounter++;//used to determine collection mode
#ifdef JET_TIME_EXECUTION
	INT64 rate;
	QueryPerformanceCounter( (LARGE_INTEGER *)&end );
	QueryPerformanceCounter((LARGE_INTEGER*)&rate);
	INT64 diff = end - start;
	double dt = ((double)diff)/((double)rate);

	printf("Took %lf seconds to collect garbage\n\n", dt);
#endif
	//printf("collection done\n");
	//printf("GC Complete: %d Greys, %d Globals, %d Stack\n%d Closures, %d Arrays, %d Objects, %d Userdata\n", this->greys.size(), this->vars.size(), 0, this->closures.size(), this->arrays.size(), this->objects.size(), this->userdata.size());
}

void GarbageCollector::Free(gcval* ii)
{
	switch (ii->type)
	{
	case ValueType::Function:
		{
			Closure* fun = (Closure*)ii;
#ifdef JETGCDEBUG
			printf("GC Freeing Function %d\n", ii);

			fun->generator = (Generator*)0xcdcdcdcd;
#else
			if (fun->numupvals)
				delete[] fun->upvals;

			delete fun->generator;
			delete fun;
#endif
			break;
		}
	case ValueType::Object:
		{
#ifdef JETGCDEBUG
			JetObject* obj = (JetObject*)ii;
			obj->nodes = (ObjNode*)0xcdcdcdcd;
#else
			delete (JetObject*)ii;
#endif
			break;
		}
	case ValueType::Array:
		{
#ifdef JETGCDEBUG
			JetArray* arr = (JetArray*)ii;
			arr->data.clear();
#endif
			delete (JetArray*)ii;
			//#endif
			break;
		}
	case ValueType::Userdata:
		{
			Value ud = Value(((JetUserdata*)ii), ((JetUserdata*)ii)->prototype);
			Value _gc = (*((JetUserdata*)ii)->prototype).get("_gc");
			if (_gc.type == ValueType::NativeFunction)
				_gc.func(this->context, &ud, 1);
			else if (_gc.type == ValueType::Function)
				throw RuntimeException("Non Native _gc Hooks Not Implemented!");//todo
			else if (_gc.type != ValueType::Null)
				throw RuntimeException("Invalid _gc Hook!");
			delete (JetUserdata*)ii;
			break;
		}
	case ValueType::String:
		{
			JetString* str = (JetString*)ii;
			delete[] str->data;
			delete str;
			break;
		}
	case ValueType::Capture:
		{
			Capture* uv = (Capture*)ii;
			delete uv;
			//printf("Freeing capture!\n");
			break;
		}
#ifdef _DEBUG
	default:
		throw RuntimeException("Runtime Error: Invalid GC Object Typeid!");
#endif
	}
}