template<size_t S> class OverLoad{};
template<size_t S, int poolsize = 256> class MemoryPool
{
public:
	MemoryPool()	{current = p.ptr;}
	void *nextptr()
	{
		if (!current)
			current = p.add();
		DATA *c = current;
		current = c->next;
		return c;
	}
	void release(void *x)
	{
		DATA *c = static_cast<DATA*>(x);
		c->next = current;
		current = c;
	}
private:
	union DATA
	{
		char s[S];
		DATA *next;
	};
	DATA *current;
	struct pool
	{
		DATA ptr[poolsize];
		pool *next;
		pool()	{next = NULL;
			for (DATA *p = ptr; p != ptr+poolsize;)
			{
				DATA *c = p;
				c->next = ++p;
			}
			ptr[poolsize-1].next = NULL;
		}
		~pool()	{delete next;}
		DATA *add()
		{
			pool *n = new pool;
			if (next)
				n->next = next;
			next = n;
			return next->ptr;
		}
	} p;
};
class VMemoryPool
{
public:
	const static int poolsize = 256;
	VMemoryPool()	{current = p.ptr;}
	void *nextptr()
	{
		if (!current)
			current = p.add();
		DATA *c = current;
		current = c->next;
		return c;
	}
	void release(void *x)
	{
		DATA *c = static_cast<DATA*>(x);
		c->x = 0;
		c->next = current;
		current = c;
	}
	void GarbageCollection()
	{
		#ifdef PSL_SHARED_GLOBAL
		StaticObject::global().get()->mark();
		#endif
		Mark();
		#ifdef PSL_USE_DESTRUCTOR
		Destructor();
		#endif
		Delete();
		UnMark();
		ReleaseEmpty();
	}
private:
	union DATA
	{
		char s[sizeof(Variable)];
		struct
		{
			int x;
			DATA *next;
		};
	};
	DATA *current;
	struct pool
	{
		DATA ptr[poolsize];
		pool *next;
		pool()	{next = NULL;
			for (int i = 0; i < poolsize; ++i)
			{
				ptr[i].x = 0;
				ptr[i].next = ptr+i+1;
			}
			ptr[poolsize-1].next = NULL;
		}
		~pool()	{delete next;}
		DATA *add()
		{
			pool *n = new pool;
			if (next)
				n->next = next;
			next = n;
			return next->ptr;
		}
		bool empty()
		{
			for (int i = 0; i < poolsize; ++i)
				if (ptr[i].x)
					return false;
			return true;
		}
	} p;
	#define POOLOOP for (pool *pl = &p; pl != NULL; pl = pl->next)for (int i = 0; i < poolsize; ++i)if (pl->ptr[i].x)
	void searchcount(Variable *v, int &c)
	{
		POOLOOP
		{
			Variable *x = (Variable*)(pl->ptr+i);
			if (v == x)
				continue;
			x->searchcount(v, c);
		}
	}
	void Mark()
	{
		POOLOOP
		{
			Variable *v = (Variable*)(pl->ptr+i);
			int count = 0;
			if (v->searchcount(v, count))
				continue;
			searchcount(v, count);
			v->markstart(count);
			Mark2();
		}
	}
	void Mark2()		{POOLOOP{((Variable*)(pl->ptr+i))->unmark(0x7FFFFFFF);}}
	void UnMark()		{POOLOOP{((Variable*)(pl->ptr+i))->unmark(0x3FFFFFFF);}}
	void Destructor()	{POOLOOP{((Variable*)(pl->ptr+i))->destructor_unmark();}}
	void Delete()		{POOLOOP{((Variable*)(pl->ptr+i))->delete_unmark();}}
	void ReleaseEmpty()
	{
		for (pool **pl = &p.next; *pl != NULL;)
		{
			if ((*pl)->empty())
			{
				pool *n = (*pl)->next;
				(*pl)->next = NULL;
				delete *pl;
				*pl = n;
				continue;
			}
			pl = &(*pl)->next;
		}
		DATA **ptr = &current;
		POOLOOP;else{
			*ptr = pl->ptr+i;
			ptr = &pl->ptr[i].next;
		}
		*ptr = NULL;
	}
	#undef POOLOOP
};
#ifdef PSL_CHECK_SCOPE_NEST
class SMemoryPool : public MemoryPool<sizeof(Variable::MethodScope),256>
{
public:
	void *nextptr()
	{
		if (!current)
			throw PSLException(PSLException::Scope);
		DATA *c = current;
		current = c->next;
		return c;
	}
};
#else
typedef MemoryPool<sizeof(Variable::MethodScope),32> SMemoryPool;
#endif

class StaticObject
{
	struct sobj
	{
		#define pool(s,n) MemoryPool<s> pool##n;
		#if defined(__x86_64__) || defined(_WIN64)
		pool(24,24)
		pool(32,32)
		#else
		pool(4,4)
		pool(12,12)
		#endif
		pool(8,8)
		pool(16,16)
		#ifdef __BORLANDC__
		pool(24,24)
		#endif
		pool(sizeof(Variable::Code),Co)
		pool(sizeof(Environment),Env)
		pool(sizeof(Variable::vObject),VO)
		#undef pool
		SMemoryPool poolScope;
		VMemoryPool vpool;
		~sobj()
		{
			#ifdef PSL_GARBAGECOLLECTION_ON_DELETING_STATIC_OBJ
			vpool.GarbageCollection();
			#endif
			#ifdef PSL_SHARED_GLOBAL
			delete global_p();
			#else
			delete envtemp_p();
			#endif
			delete rsvnull_p();
			delete string_p();
		}
	};
	friend struct sobj;
	static sobj &so()
	{
		static sobj o;
		return o;
	}
	static rsv *rsvnull_p()
	{
		static rsv *null = new rsv;
		return null;
	}
	#ifdef PSL_SHARED_GLOBAL
	static rsv *global_p()
	{
		static rsv *global = new rsv;
		return global;
	}
public:
	static rsv &global()			{return *global_p();}
	#else
	static Environment *envtemp_p()
	{
		static Environment *envtemp = new Environment;
		return envtemp;
	}
public:
	static Environment &envtemp()	{return *envtemp_p();}
	#endif
	static rsv &rsvnull()			{return *rsvnull_p();}
	#define pool(s,n) static MemoryPool<s> &pool(OverLoad<s> x)	{return so().pool##n;}
	#if defined(__x86_64__) || defined(_WIN64)
	pool(24,24)
	pool(32,32)
	#else
	pool(4,4)
	pool(12,12)
	#endif
	pool(8,8)
	pool(16,16)
	#ifdef __BORLANDC__
	pool(24,24)
	#endif
	pool(sizeof(Variable::Code),Co)
	pool(sizeof(Environment),Env)
	pool(sizeof(Variable::vObject),VO)
	#undef pool
	static SMemoryPool &spool()		{return so().poolScope;}
	static VMemoryPool &vpool()		{return so().vpool;}
	struct String
	{
		const string d;
		String():d("destructor"){}
		static const string &destructor()		{return string_p()->d;}
	};
	friend struct String;
private:
	static String *string_p()
	{
		static String *str = new String;
		return str;
	}
};

#define MM(p) public:static void *Next(){return StaticObject::p.nextptr();}static void Release(void *ptr){StaticObject::p.release(ptr);}
class VMemoryManager{MM(vpool())};
class SMemoryManager{MM(spool())};
template<size_t S> class MemoryManager{MM(pool(OverLoad<S>()))};
#undef MM
