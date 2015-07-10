#ifdef PSL_DEBUG
	#define PSL_DUMP(x) void dump x
#else
	#define PSL_DUMP(x)
#endif
#ifdef PSL_THREAD_SAFE
	#undef PSL_USE_VARIABLE_MEMORY_MANAGER
	#undef PSL_SHARED_GLOBAL
	#define PSL_CONST_STATIC const
#else
	#define PSL_CONST_STATIC const static
#endif
#ifdef PSL_USE_VARIABLE_MEMORY_MANAGER
	#define PSL_MEMORY_MANAGER(x) static void *operator new(size_t t){return MemoryManager<sizeof(x)>::Next();}static void operator delete(void *ptr){MemoryManager<sizeof(x)>::Release(ptr);}
#else
	#define PSL_MEMORY_MANAGER(x)
#endif
#if defined(PSL_USE_VARIABLE_MEMORY_MANAGER) && !defined(PSL_SHARED_GLOBAL)
	#define PSL_TEMPORARY_ENV(x) Environment &x = StaticObject::envtemp()
	#define PSL_TEMPORARY_ENV0(x) Environment &x = StaticObject::envtemp()
#else
	#define PSL_TEMPORARY_ENV(x) Environment x
	#define PSL_TEMPORARY_ENV0(x) Environment x(0)
#endif

class variable
{
	#ifndef _WIN32
	typedef long long __int64;
	typedef unsigned long long uint64;
	#else
	typedef unsigned __int64 uint64;
	#endif
public:
	#include "pstring.h"
	struct PSLException
	{
		enum ErrorCode
		{
			Scope,
		} errorcode;
		PSLException(ErrorCode e):errorcode(e){}
	};
	typedef variable(*function)(variable&);
	typedef variable(*method)(variable&,variable&);
	typedef unsigned long hex;
	enum Type
	{
		NIL,
		INT,
		HEX,
		FLOAT,
		STRING,
		POINTER,
		RARRAY,
		OBJECT,
		CFUNCTION,
		BCFUNCTION,
		METHOD,
		CMETHOD,
		CCMETHOD,
		CPOINTER,
		SPOINTER,
		THREAD,
	};
	class Function{};
	template<class C>class Method{};
	class IntPtr{};
	class DoublePtr{};
private:
	class Variable;
	#include "container.h"
	friend class rsv;
public:

	variable()					{x = new Variable();}
	variable(bool b)			{x = new Variable(static_cast<int>(b));}
	variable(int i)				{x = new Variable(i);}
	variable(long i)			{x = new Variable(static_cast<int>(i));}
	variable(unsigned u)		{x = new Variable(static_cast<int>(u));}
	variable(hex h)				{x = new Variable(h);}
	variable(__int64 i)			{x = new Variable(static_cast<int>(i));}
	variable(uint64 i)			{x = new Variable(static_cast<int>(i));}
	variable(double d)			{x = new Variable(d);}
	variable(const char *s)		{x = new Variable(s);}
	variable(const wchar_t *s)	{x = new Variable(s);}
	variable(const string &s)	{x = new Variable(s);}
	variable(function f)		{x = new Variable(f);}
	variable(method f)			{x = new Variable(f);}
	variable(void *p)			{x = new Variable(p);}
	variable(const variable &v)	{x = v.x->clone();}
	variable(Type t)			{x = new Variable(t);}
	variable(const rsv &v)		{x = v.get()->ref();}
	~variable()					{x->finalize();}

	Type type()			const	{return x->type();}
	bool type(Type t)	const	{return x->type() == t;}

	variable clone()	const	{return *this;}
	variable &substitution(const variable &v)	{x->substitution(v.x);return *this;}
	variable &assignment(const variable &v)		{x->assignment(v.x);return *this;}
	variable &operator=(const variable &v)		{x->substitution(v.x);return *this;}
	variable &operator->*(const variable &v)	{x->assignment(v.x);return *this;}		// ‰½‚¹—]‚Á‚Ä‚é‰‰ŽZŽq‚ª‚±‚ê‚È‚Ì‚Åc
	variable &operator=(const rsv &v)			{x->finalize();x = v.get()->ref();return *this;}

	variable &operator=(function f)		{
		if (f == NULL)
		{
			variable v = 0;
			x->substitution(v.x);
		}
		else
		{
			variable v = f;
			x->substitution(v.x);
		}
		return *this;
	}
	template<class T>variable &operator=(T t)	{
		variable v = t;
		x->substitution(v.x);
		return *this;
	}

	#define OP(n,o) variable &operator o##=(const variable &v)	{x->n(v.x);return *this;}\
					variable operator o(const variable &v)	const	{variable z = *this;z.x->n(v.x);return z;}
	OP(add,+)
	OP(sub,-)
	OP(mul,*)
	OP(div,/)
	OP(mod,%)
	OP(oand,&)
	OP(oor,|)
	OP(oxor,^)
	OP(shl,<<)
	OP(shr,>>)
	#undef OP
	#define CMP(n,o) bool operator o(const variable &v)	const	{return x->n(v.x);}
	CMP(eq,==)
	CMP(ne,!=)
	CMP(le,<=)
	CMP(ge,>=)
	CMP(lt,<)
	CMP(gt,>)
	#undef CMP
	variable operator+();
	variable operator-()	const {variable v = *this;v.x->neg();return v;}
	variable operator*()	const {return x->deref();}
	variable operator~()	const {variable v = *this;v.x->Compl();return v;}
	bool operator!()	const {return !x->toBool();}
	variable &operator++();
	variable &operator--();
	variable operator++(int i);	// suf
	variable operator--(int i);

	int toInt()			const	{return x->toInt();}
	double toDouble()	const	{return x->toDouble();}
	bool toBool()		const	{return x->toBool();}
	operator int()		const	{return x->toInt();}
	operator double()	const	{return x->toDouble();}
	operator bool()		const	{return x->toBool();}
	operator char()				const	{return static_cast<char>(x->toInt());}
	operator signed char()		const	{return static_cast<signed char>(x->toInt());}
	operator unsigned char()	const	{return static_cast<unsigned char>(x->toInt());}
	operator short()			const	{return static_cast<short>(x->toInt());}
	operator unsigned short()	const	{return static_cast<unsigned short>(x->toInt());}
	operator unsigned()			const	{return static_cast<unsigned>(x->toInt());}
	operator long()				const	{return static_cast<long>(x->toInt());}
	operator unsigned long()	const	{return static_cast<unsigned long>(x->toInt());}
	operator __int64()			const	{return x->toInt();}
	operator uint64()			const	{return static_cast<unsigned>(x->toInt());}
	operator float()		const	{return static_cast<float>(x->toDouble());}
	operator long double()	const	{return x->toDouble();}
	operator string()		const	{return x->toString();}
	string toString()		const	{return x->toString();}
	operator void*()		const	{return x->toPointer();}
	template<class T>	T *toPointer()	const	{return static_cast<T*>(x->toPointer());}
	template<class T>	operator T*()	const	{return static_cast<T*>(x->toPointer());}
	#ifndef PSL_THREAD_SAFE
		#ifdef __BORLANDC__
		template<>
		#endif
	operator const char*()	const	{static string s;s = x->toString();return s.c_str();}
	const char *c_str()		const	{static string s;s = x->toString();return s.c_str();}
	#endif

	variable operator[](size_t i)			const	{return x->index(i);}
	variable operator[](int i)				const	{return x->index(static_cast<size_t>(minusindex(i)));}
	variable operator[](const char *s)		const	{return x->child(s);}
	variable operator[](const string &s)	const	{return x->child(s);}
	variable operator[](const variable &v)	const	{
		Type t = v.type();
		variable z = v.ref();
		while (t == POINTER)
		{
			z = (*v).ref();
			t = z.type();
		}
		if (t == STRING || t == FLOAT)	return x->child(z);
		if (t == RARRAY)
		{
			int s = minusindex(z.x->index(0)->toInt());
			int l = z.x->index(1)->toInt();
			variable r(RARRAY);
			if (l < 0)	for (int i = 0; i > l; --i)	r.x->push(x->index(static_cast<size_t>(s+i)));
			else		for (int i = 0; i < l; ++i)	r.x->push(x->index(static_cast<size_t>(s+i)));
			return r.x;
		}
		return x->index(static_cast<size_t>(minusindex(z)));
	}
	size_t length()				const	{return x->length();}
	size_t memberLength()		const	{return x->memberLength();}
	bool exist(const string &s) const	{return x->exist(s);}
	void push(const variable &v)		{return x->push(v.x);}
	variable keys()				const	{return x->keys();}
	bool set(const string &s, const variable &v)	{return x->set(s, v);}
	void del(const string &s)						{return x->del(s);}
private:
	int minusindex(int i) const
	{
		if (i < 0)
		{
			int l = static_cast<int>(x->length());
			i = l + i;
			if (i < 0)
				i = 0;
		}
		return i;
	}
	#include "PSLlib.h"
	#include "tokenizer.h"
	#include "parser.h"
	class Variable
	{
	public:
		Variable()					{rc = 1;x = new vObject();}
		Variable(int i)				{rc = 1;x = new vInt(i);}
		Variable(hex h)				{rc = 1;x = new vHex(h);}
		Variable(double d)			{rc = 1;x = new vFloat(d);}
		Variable(const string &s)	{rc = 1;x = new vString(s);}
		Variable(function f)		{rc = 1;x = new vCFunction(f);}
		Variable(method f)			{rc = 1;x = new vCMethod(f, NULL);}
		Variable(void *p)			{rc = 1;x = new vCPointer(p);}
		Variable(Variable *v)		{rc = 1;x = new vPointer(v);}
		Variable(Type t, size_t i)		{rc = 1;x = new vRArray(i);}
		Variable(Type t){rc = 1;switch (t){
			case NIL:		x = new vBase();break;
			case INT:		x = new vInt(0);break;
			case HEX:		x = new vHex(0);break;
			case FLOAT:		x = new vFloat(0);break;
			case STRING:	x = new vString("");break;
			case POINTER:	x = new vPointer();break;
			case RARRAY:	x = new vRArray();break;
			case THREAD:	x = new vThread();break;
			default:		x = new vObject();break;
		}}
		#ifdef PSL_USE_DESTRUCTOR
		void finalize()		{if (rc==1)	{x->destructor();delete this;}else --rc;}
		#else
		void finalize()		{if (!--rc)	delete this;}
		#endif
		void safedelete()	{rsv v(new Variable(x), 0);x = NULL;x = new vInt(0);}
		bool searchcount(Variable *v, int &c)
		{
			if (rc & 0x40000000)
				return true;
			if (static_cast<unsigned>(rc) & 0x80000000)
			{
				if (v == this)
					++c;
				return false;
			}
			rc |= 0x80000000;
			x->searchcount(v, c);
			return false;
		}
		void markstart(int c)
		{
			if ((rc & 0xFFFFFF) != c)
				mark();
		}
		void mark()
		{
			if (rc & 0x40000000)
				return;
			rc |= 0x40000000;
			x->mark();
		}
		void unmark(unsigned long m)
		{
			rc &= m;
		}
		void destructor_unmark()
		{
			if (rc & 0x40000000)
				return;
			x->destructor();
		}
		void delete_unmark()
		{
			if (rc & 0x40000000)
				return;
			safedelete();
		}
		Type type()	const	{return x->type();}

		void substitution(Variable *v)	{x = x->substitution(v);x->method_this(this);}
		void assignment(Variable *v)	{x = x->assignment(v);x->method_this(this);}
		void gset(Variable *v)			{x = x->assignment(v);}

		void add(Variable *v)	{x->add(v);x->method_this(this);}
		#define OP(n) void n(Variable *v)	{x->n(v);}
		OP(sub)
		OP(mul)
		OP(div)
		OP(mod)
		OP(oand)
		OP(oor)
		OP(oxor)
		OP(shl)
		OP(shr)
		#undef OP
		#define CMP(n) bool n(Variable *v)	{return x->n(v);}
		CMP(eq)
		CMP(ne)
		CMP(le)
		CMP(ge)
		CMP(lt)
		CMP(gt)
		#undef CMP
		void neg()	{x->neg();}
		void Compl(){x->Compl();}
		Variable *deref()	{Variable *v = x->deref();return v ? v : this;}

		bool toBool()		const {return x->toBool();}
		int toInt()			const {return x->toInt();}
		double toDouble()	const {return x->toDouble();}
		string toString()	const {return x->toString();}
		void *toPointer()	const {return x->toPointer();}

		size_t length()				const {return x->length();}
		size_t memberLength()		const {return x->memberLength();}
		bool exist(const string &s)	const {return x->exist(s);}
		void push(Variable *v)		{x->push(v);}
		Variable *index(size_t t)			{Variable *v = x->index(t);return v ? v : this;}
		Variable *child(const string &s)	{Variable *v = x->child(s);return v ? v : this;}
		Variable *keys()	{Variable *v = x->keys();v->rc = 0;return v;}
		Variable *getifexist(const string &s)	{return x->getifexist(s);}
		bool set(const string &s, const variable &v)	{return x->set(s, v);}
		void del(const string &s)						{x->del(s);}

		#include "bytecode.h"
	private:
		#include "environment.h"
		friend class Parser;
		friend class variable;
	public:
		void prepare(Environment &env)			{x->prepare(env, this);}
		void prepareInstance(Environment &env)	{x->prepareInstance(env, this);}
		variable call(Environment &env, variable &arg)	{return x->call(env, arg, this);}
		rsv instance(Environment &env)			{return x->instance(env, this);}

//		Variable(Code *c)		{rc = 1;x = new vObject(c);}
		size_t codelength()		{return x->codelength();}
		Code *getcode()			{return x->getcode();}
		void pushcode(OpCode *c){return x->pushcode(c);}
		void pushlabel(const string &s){return x->pushlabel(s);}
		void write(const string &s, bytecode &b){x->write(s, b);}
	private:
		class vBase
		{
		public:
			PSL_MEMORY_MANAGER(vBase)
			vBase()	{}
			virtual ~vBase(){}
			virtual Type type()	const	{return NIL;}
			virtual vBase *clone()	{return new vBase();}
			virtual void searchcount(Variable *v, int &c){}
			virtual void mark(){}
			virtual void destructor(){}

			virtual vBase *substitution(Variable *v)	{vBase *x = v->bclone();delete this;return x;}
			virtual vBase *assignment(Variable *v)		{vBase *x = v->bclone();delete this;return x;}

			#define OP(n) virtual void n(Variable *v)	{}
			OP(add)
			OP(sub)
			OP(mul)
			OP(div)
			OP(mod)
			OP(oand)
			OP(oor)
			OP(oxor)
			OP(shl)
			OP(shr)
			#undef OP
			#define CMP(n) virtual bool n(Variable *v)	{return false;}
			CMP(eq)
			CMP(ne)
			CMP(le)
			CMP(ge)
			CMP(lt)
			CMP(gt)
			#undef CMP
			virtual void neg()	{}
			virtual void Compl(){}
			virtual Variable *deref()	{return NULL;}

			virtual bool toBool()		const {return false;}
			virtual int toInt()			const {return 0;}
			virtual double toDouble()	const {return 0;}
			virtual string toString()	const {return "";}
			virtual void *toPointer()	const {return NULL;}

			virtual size_t length()				const {return 0;}
			virtual size_t memberLength()		const {return 0;}
			virtual bool exist(const string &s)	const {return false;}
			virtual Variable *index(size_t t)			{return NULL;}
			virtual Variable *child(const string &s)	{return NULL;}
			virtual void push(Variable *v){}
			virtual Variable *keys()	{return new Variable();}
			virtual Variable *getifexist(const string &s)	{return NULL;}
			virtual bool set(const string &s, const variable &v)	{return false;}
			virtual void del(const string &s)	{}
			virtual void method_this(Variable *v)	{}

			virtual void prepare(Environment &env, Variable *v)	{
				variable a = env.pop();
				variable c(v->clone(), 0);
				env.push(c.substitution(a));
			}
			virtual void prepareInstance(Environment &env, Variable *v)	{env.push(rsv(v->clone(), 0));}
			virtual rsv call(Environment &env, variable &arg, Variable *v)	{return variable(NIL);}
			virtual rsv instance(Environment &env, Variable *v)	{return v->clone();}

			virtual size_t codelength()	{return 0;}
			virtual Code *getcode()		{return NULL;}
			virtual void pushcode(OpCode *c){delete c;}
			virtual void pushlabel(const string &s){}
			virtual void write(const string &s, bytecode &b){}

			virtual void dump(){PSL_PRINTF(("vBase\n"));}
		} *x;
	private:
		int rc;
		~Variable()	{delete x;}
	public:
		Variable(vBase *v)	{rc = 1;x = v;x->method_this(this);}
		vBase *bclone()		{return x->clone();}
		Variable *clone()	{return new Variable(x->clone());}
		Variable *ref()		{++rc;return this;}
		PSL_DUMP((){PSL_PRINTF(("rc:%4d, ", rc));x->dump();})
#ifdef PSL_USE_VARIABLE_MEMORY_MANAGER
		static void *operator new(size_t t)		{return VMemoryManager::Next();}
		static void operator delete(void *ptr)	{VMemoryManager::Release(ptr);}
#endif
	private:
		#include "vdata.h"
		friend class vRArray;
		template<class F>Variable(Function z, F f, const variable &d)	{rc = 1;x = BCFunction(f, d);}
		template<class C, class M>Variable(Method<C> z, M m, const variable &d)	{rc = 1;x = CCMethod<C>(m, d);}
		Variable(IntPtr z, int *i)			{rc = 1;x = new vIntPtr(i);}
		Variable(DoublePtr z, double *d)	{rc = 1;x = new vFloatPtr(d);}
	} *x;
	typedef Variable::Environment Environment;
	#ifdef PSL_USE_VARIABLE_MEMORY_MANAGER
	#include "memory.h"
	#endif
	variable(Variable *v)	{x = v->ref();}
	variable(Variable *v, int i)	{x = v;}
	variable(Type t, Variable *v)	{x = new Variable(v);}

	void gset(const variable &v)	{x->gset(v.x);}

	void prepare(Environment &env)			{x->prepare(env);}

	Variable::Code *getcode()			{return x->getcode();}
	void pushcode(Variable::OpCode *c)	{return x->pushcode(c);}
	void pushlabel(const string &s)		{return x->pushlabel(s);}
	friend class PSLVM;
	friend class Parser;
	friend class Variable::vBase;
	friend class Variable::vCMethod;
	friend class Variable::CALL;
	friend class Variable::bcreader;
	friend class Variable::Code;
public:
	size_t codelength()	const			{return x->codelength();}
	typedef Variable::bytecode buffer;
	rsv ref()		const	{return x;}
	rsv pointer()	const	{return variable(POINTER, x);}

	variable operator()()									{PSL_TEMPORARY_ENV(env);variable v;return x->call(env, v);}
	variable operator()(const variable &arg)				{PSL_TEMPORARY_ENV(env);variable v = arg.ref();return x->call(env, v);}
	#ifndef PSL_SHARED_GLOBAL
	variable operator()(Environment &env, variable &arg)	{return x->call(env, arg);}
	variable instance(Environment &env)						{return x->instance(env);}
	#endif
	variable instance()										{PSL_TEMPORARY_ENV(env);return x->instance(env);}
	#define cva(n) const variable &arg##n
	#define ap(n) arg.push(arg##n);
	#define CALL(z,y) variable operator()z{variable arg(RARRAY);y PSL_TEMPORARY_ENV(env);return x->call(env, arg);}
	CALL((cva(1),cva(2)),								ap(1)ap(2))
	CALL((cva(1),cva(2),cva(3)),						ap(1)ap(2)ap(3))
	CALL((cva(1),cva(2),cva(3),cva(4)),					ap(1)ap(2)ap(3)ap(4))
	CALL((cva(1),cva(2),cva(3),cva(4),cva(5)),			ap(1)ap(2)ap(3)ap(4)ap(5))
	CALL((cva(1),cva(2),cva(3),cva(4),cva(5),cva(6)),	ap(1)ap(2)ap(3)ap(4)ap(5)ap(6))
	#undef CALL
	#undef ap

	#define ap(n) x->push(arg##n.x);
	#define LIST(n,z,y) variable z{x = new Variable(RARRAY, n);y}
	LIST(2,(cva(1),cva(2)),								ap(1)ap(2))
	LIST(3,(cva(1),cva(2),cva(3)),						ap(1)ap(2)ap(3))
	LIST(4,(cva(1),cva(2),cva(3),cva(4)),				ap(1)ap(2)ap(3)ap(4))
	LIST(5,(cva(1),cva(2),cva(3),cva(4),cva(5)),		ap(1)ap(2)ap(3)ap(4)ap(5))
	LIST(6,(cva(1),cva(2),cva(3),cva(4),cva(5),cva(6)),	ap(1)ap(2)ap(3)ap(4)ap(5)ap(6))
	#undef LIST
	#undef ap
	#undef cva

	template<class F>variable(Function z, F f, const variable &d = variable())	{x = new Variable(z, f, d);}
	template<class C, class M>variable(Method<C> z, M m, const variable &d = variable())	{x = new Variable(z, m, d);}
	variable(IntPtr z, int *i)			{x = new Variable(z, i);}
	variable(DoublePtr z, double *d)	{x = new Variable(z, d);}
	template<class T>static variable make_variable(T *p)
	{
		return variable(new Variable(new Variable::vSmartPointer<T>(p)), 0).pointer();
	}

	PSL_DUMP((){x->dump();})

	class iterator : public std::iterator<std::random_access_iterator_tag, variable, int, variable*, variable&>
	{
		Variable *v;
		size_t index;
		Variable *temp;
	public:
		iterator(Variable *x, size_t i){v = x;index = i;}
		variable &operator*()			{temp = v->index(index);return *reinterpret_cast<variable*>(&temp);}
		bool operator==(iterator i)		{return index == i.index;}
		bool operator!=(iterator i)		{return index != i.index;}
		bool operator<(iterator i)		{return index < i.index;}
		iterator &operator++()			{++index;return *this;}
		iterator &operator--()			{--index;return *this;}
		iterator &operator+=(size_t i)	{index += i;return *this;}
		iterator &operator-=(size_t i)	{index -= i;return *this;}
		iterator operator+(size_t i)	{return iterator(v, index+i);}
		iterator operator-(size_t i)	{return iterator(v, index-i);}
		int operator-(iterator i)		{return static_cast<int>(index) - static_cast<int>(i.index);}
	};
	iterator begin(){return iterator(x, 0);}
	iterator end()	{return iterator(x, length());}
};
