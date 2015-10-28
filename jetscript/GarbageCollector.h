#ifndef _JET_GC_HEADER
#define _JET_GC_HEADER

#ifdef _DEBUG
#ifndef DBG_NEW      
#define DBG_NEW new ( _NORMAL_BLOCK , __FILE__ , __LINE__ )     
#define new DBG_NEW   
#endif

#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#endif

#include "Value.h"
#include "VMStack.h"
#include <vector>

namespace Jet
{
	class JetContext;

	class GarbageCollector
	{
		friend class JetObject;
		friend struct Value;
		//must free with GCFree, pointer is a bit offset to leave room for the flag

		/*template<class T> 
		T* GCAllocate2(unsigned int size)
		{
		return (T*)((new char[sizeof(T)]));
		}

		char* GCAllocate(unsigned int size)
		{
		//this leads to less indirection, and simplified cleanup
		char* data = new char[size];//enough room for the flag
		//this->gcObjects.push_back(data);
		return data;
		}

		//need to call destructor first
		template<class T>
		void GCFree(T* data)
		{
		data->~T();
		delete[] (((char*)data));
		}

		template<class T[]>
		void GCFree(T* data)
		{
		data->~T();
		delete[] (((char*)data));
		}

		void GCFree(char* data)
		{
		delete[] (data);
		}*/

		JetContext* context;
	public:
		//garbage collector stuff
		//need to unify everything except userdata
		struct gcval
		{
			bool mark;
			bool grey;
			Jet::ValueType type : 8;
			unsigned char refcount;
		};
		std::vector<gcval*> gen1;
		std::vector<gcval*> gen2;
		//std::vector<gcval*> gen3;basically permanent objects, todo

		std::vector<Value> nativeRefs;//a list of all objects stored natively to mark

		int allocationCounter;//used to determine when to run the GC
		int collectionCounter;//state of the gc
		VMStack<Value> greys;//stack of grey objects for processing

		GarbageCollector(JetContext* context);

		void Cleanup();

		inline void AddObject(gcval* obj)
		{
			this->gen1.push_back(obj);
		}

		template<class T> 
		T* New()
		{
			//need to call constructor
			T* buf = new T;
#undef new
			this->gen1.push_back((gcval*)buf);
			//new (buf) T();
			return (T*)(buf);

#ifdef _DEBUG
#ifndef DBG_NEW      
#define DBG_NEW new ( _NORMAL_BLOCK , __FILE__ , __LINE__ )     
#define new DBG_NEW   
#endif
#endif
		}

		//hack for objects
		template<class T> 
		T* New(JetContext* arg1)
		{
			//need to call constructor
			T* buf = new T(arg1);
#undef new
			this->gen1.push_back((gcval*)buf);
			//new (buf) T(arg1);
			return (T*)(buf);

#ifdef _DEBUG
#ifndef DBG_NEW      
#define DBG_NEW new ( _NORMAL_BLOCK , __FILE__ , __LINE__ )     
#define new DBG_NEW   
#endif
#endif
		}

		//hack for strings
		template<class T> 
		T* New(char* arg1)
		{
			//need to call constructor
			T* buf = new T(arg1);
#undef new
			this->gen1.push_back((gcval*)buf);
			//new (buf) T(arg1);
			return (T*)(buf);

#ifdef _DEBUG
#ifndef DBG_NEW      
#define DBG_NEW new ( _NORMAL_BLOCK , __FILE__ , __LINE__ )     
#define new DBG_NEW   
#endif
#endif
		}

		//hack for userdata
		template<class T> 
		T* New(void* arg1, JetObject* arg2)
		{
			//need to call constructor
			T* buf = new T(arg1, arg2);
#undef new
			this->gen1.push_back((gcval*)buf);
			//new (buf) T(arg1, arg2);
			return (T*)(buf);

#ifdef _DEBUG
#ifndef DBG_NEW      
#define DBG_NEW new ( _NORMAL_BLOCK , __FILE__ , __LINE__ )     
#define new DBG_NEW   
#endif
#endif
		}

		void Run();

	private:
		void Mark();
		void Sweep();

		void Free(gcval* val);
	};
}
#endif
