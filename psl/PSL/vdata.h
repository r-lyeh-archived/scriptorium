public:
static bool isFunction(Variable *v)
{
	switch (v->type())
	{
	case OBJECT:
		if (!v->getcode())
			return false;
	case CFUNCTION:
	case BCFUNCTION:
		return true;
	default:
		return false;
	}
}
static bool isMethod(Variable *v)
{
	switch (v->type())
	{
	case METHOD:
	case CMETHOD:
	case CCMETHOD:
		return true;
	default:
		return false;
	}
}
private:
class vInt : public vBase
{
public:
	PSL_MEMORY_MANAGER(vInt)
	vInt(int i)	{x = i;}
	Type type()	const	{return INT;}
	vBase *clone()	{return new vInt(x);}

	vBase *substitution(Variable *v)	{x = v->toInt();return this;}

	#define OP(n,o) void n(Variable *v)	{x o v->toInt();}
	OP(add,+=)
	OP(sub,-=)
	OP(mul,*=)
	OP(oand,&=)
	OP(oor,|=)
	OP(oxor,^=)
	OP(shl,<<=)
	OP(shr,>>=)
	#undef OP
	#define OP(n,o) void n(Variable *v)	{if (int i = v->toInt())x o i;else x=0;}
	OP(div,/=)
	OP(mod,%=)
	#undef OP
	#define CMP(n,o) bool n(Variable *v)	{return x o v->toInt();}
	CMP(eq,==)
	CMP(ne,!=)
	CMP(le,<=)
	CMP(ge,>=)
	CMP(lt,<)
	CMP(gt,>)
	#undef CMP
	void neg()	{x = -x;}
	void Compl(){x = ~x;}

	bool toBool()		const {return x != 0;}
	int toInt()			const {return x;}
	double toDouble()	const {return x;}
	string toString()	const {return x;}

	size_t length()		const {return 1;}

	PSL_DUMP((){PSL_PRINTF(("vInt:%d\n", x));})
private:
	int x;
};
class vIntPtr : public vBase
{
public:
	PSL_MEMORY_MANAGER(vIntPtr)
	vIntPtr(int *i)	{x = i;}
	Type type()	const	{return INT;}
	vBase *clone()	{return new vInt(*x);}

	vBase *substitution(Variable *v)	{*x = v->toInt();return this;}

	#define OP(n,o) void n(Variable *v)	{*x o v->toInt();}
	OP(add,+=)
	OP(sub,-=)
	OP(mul,*=)
	OP(oand,&=)
	OP(oor,|=)
	OP(oxor,^=)
	OP(shl,<<=)
	OP(shr,>>=)
	#undef OP
	#define OP(n,o) void n(Variable *v)	{if (int i = v->toInt())*x o i;else x=0;}
	OP(div,/=)
	OP(mod,%=)
	#undef OP
	#define CMP(n,o) bool n(Variable *v)	{return *x o v->toInt();}
	CMP(eq,==)
	CMP(ne,!=)
	CMP(le,<=)
	CMP(ge,>=)
	CMP(lt,<)
	CMP(gt,>)
	#undef CMP
	void neg()	{*x = -*x;}
	void Compl(){*x = ~*x;}

	bool toBool()		const {return *x != 0;}
	int toInt()			const {return *x;}
	double toDouble()	const {return *x;}
	string toString()	const {return *x;}

	size_t length()		const {return 1;}

	PSL_DUMP((){PSL_PRINTF(("vInt:%d\n", *x));})
private:
	int *x;
};

class vHex : public vBase
{
public:
	PSL_MEMORY_MANAGER(vHex)
	vHex(hex i)	{x = i;}
	Type type()	const	{return HEX;}
	vBase *clone()	{return new vHex(x);}

	vBase *substitution(Variable *v)	{x = static_cast<hex>(v->toInt());return this;}

	#define OP(n,o) void n(Variable *v)	{x o static_cast<hex>(v->toInt());}
	OP(add,+=)
	OP(sub,-=)
	OP(mul,*=)
	OP(oand,&=)
	OP(oor,|=)
	OP(oxor,^=)
	OP(shl,<<=)
	OP(shr,>>=)
	#undef OP
	#define OP(n,o) void n(Variable *v)	{if (hex i = static_cast<hex>(v->toInt()))x o i;else x=0;}
	OP(div,/=)
	OP(mod,%=)
	#undef OP
	#define CMP(n,o) bool n(Variable *v)	{return x o static_cast<hex>(v->toInt());}
	CMP(eq,==)
	CMP(ne,!=)
	CMP(le,<=)
	CMP(ge,>=)
	CMP(lt,<)
	CMP(gt,>)
	#undef CMP
	void neg()	{x = ~x + 1;}
	void Compl(){x = ~x;}

	bool toBool()		const {return x != 0;}
	int toInt()			const {return static_cast<int>(x);}
	double toDouble()	const {return x;}
	string toString()	const {return string(x);}

	size_t length()		const {return 1;}

	PSL_DUMP((){PSL_PRINTF(("vHex:%lX\n", x));})
private:
	hex x;
};

class vFloat : public vBase
{
public:
	PSL_MEMORY_MANAGER(vFloat)
	vFloat(double d)	{x = d;}
	Type type()	const	{return FLOAT;}
	vBase *clone()	{return new vFloat(x);}

	vBase *substitution(Variable *v)	{x = v->toDouble();return this;}

	#define OP(n,o) void n(Variable *v)	{x o v->toDouble();}
	OP(add,+=)
	OP(sub,-=)
	OP(mul,*=)
	#undef OP
	void div(Variable *v)	{if (double d = v->toDouble())x /= d;else x=0;}
	#define CMP(n,o) bool n(Variable *v)	{return x o v->toDouble();}
	CMP(eq,==)
	CMP(ne,!=)
	CMP(le,<=)
	CMP(ge,>=)
	CMP(lt,<)
	CMP(gt,>)
	#undef CMP
	void neg()	{x = -x;}

	bool toBool()		const {return x != 0;}
	int toInt()			const {return static_cast<int>(x);}
	double toDouble()	const {return x;}
	string toString()	const {return x;}

	size_t length()		const {return 1;}

	PSL_DUMP((){PSL_PRINTF(("vFloat:%f\n", x));})
private:
	double x;
};
class vFloatPtr : public vBase
{
public:
	PSL_MEMORY_MANAGER(vFloatPtr)
	vFloatPtr(double *d)	{x = d;}
	Type type()	const	{return FLOAT;}
	vBase *clone()	{return new vFloat(*x);}

	vBase *substitution(Variable *v)	{*x = v->toDouble();return this;}

	#define OP(n,o) void n(Variable *v)	{*x o v->toDouble();}
	OP(add,+=)
	OP(sub,-=)
	OP(mul,*=)
	#undef OP
	void div(Variable *v)	{if (double d = v->toDouble())*x /= d;else x=0;}
	#define CMP(n,o) bool n(Variable *v)	{return *x o v->toDouble();}
	CMP(eq,==)
	CMP(ne,!=)
	CMP(le,<=)
	CMP(ge,>=)
	CMP(lt,<)
	CMP(gt,>)
	#undef CMP
	void neg()	{*x = -*x;}

	bool toBool()		const {return *x != 0;}
	int toInt()			const {return static_cast<int>(*x);}
	double toDouble()	const {return *x;}
	string toString()	const {return *x;}

	size_t length()		const {return 1;}

	PSL_DUMP((){PSL_PRINTF(("vFloat:%f\n", *x));})
private:
	double *x;
};

class vString : public vBase
{
public:
	PSL_MEMORY_MANAGER(vString)
	vString(const string &s)	{x = s;}
	Type type()	const	{return STRING;}
	vBase *clone()	{return new vString(x);}

	vBase *substitution(Variable *v)	{x = v->toString();return this;}

	void add(Variable *v)	{x += v->toString();}
	#define OP(n,o) void n(Variable *v)	{x o static_cast<size_t>(v->toInt());}
	OP(sub,-=)
	OP(mul,*=)
	OP(div,/=)
	OP(mod,%=)
	#undef OP
	#define CMP(n,o) bool n(Variable *v)	{return x o v->toString();}
	CMP(eq,==)
	CMP(ne,!=)
	CMP(le,<=)
	CMP(ge,>=)
	CMP(lt,<)
	CMP(gt,>)
	#undef CMP
	void neg()	{x.reverse();}

	bool toBool()		const {return x.length() != 0;}
	int toInt()			const {return x;}
	double toDouble()	const {return x;}
	string toString()	const {return x;}

	size_t length()		const {return 1;}

	PSL_DUMP((){PSL_PRINTF(("vString:%s\n", x.c_str()));})
private:
	string x;
};

class vPointer : public vBase
{
public:
	PSL_MEMORY_MANAGER(vPointer)
	vPointer()	{x = NULL;}
	vPointer(Variable *v)	{x = v ? v->ref() : NULL;}
	~vPointer()	{if (x)x->finalize();}
	Type type()	const	{return POINTER;}
	vBase *clone()	{return new vPointer(x);}
	void searchcount(Variable *v, int &c){if (x)x->searchcount(v, c);}
	void mark(){if (x)x->mark();}

	vBase *substitution(Variable *v)
	{
		if (v->type() == POINTER)
		{
			if (x)	x->finalize();
			if (v->toBool())	x = v->deref()->ref();
			else				x = NULL;
		}
		else if (x)				x->substitution(v);
		else					x = v->ref();
		return this;
	}

	#define OP(n) void n(Variable *v)	{if (x)x->n(v);}
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
	#define CMP(n) bool n(Variable *v)	{if (x)return x->n(v);return false;}
	CMP(le)
	CMP(ge)
	CMP(lt)
	CMP(gt)
	#undef CMP
	void neg()	{if (x)x->neg();}

	bool eq(Variable *v)	{if (!x && v->type() == POINTER && !v->toBool())return true ;return x == v->deref();}
	bool ne(Variable *v)	{if (!x && v->type() == POINTER && !v->toBool())return false;return x != v->deref();}
	Variable *deref()	{return x;}

	bool toBool()		const {return x ? true : false;}
	int toInt()			const {return x ? x->toInt() : 0;}
	double toDouble()	const {return x ? x->toDouble() : 0;}
	string toString()	const {return x ? x->toString() : string("[nullptr]");}
	void *toPointer()	const {return x ? x->toPointer() : NULL;}

	size_t length()		const {return x ? x->length() : 0;}
	bool exist(const string &s)	const {return x ? x->exist(s) : false;}
	Variable *index(size_t t)			{return x ? x->index(t) : NULL;}
	Variable *child(const string &s)	{return x ? x->child(s) : NULL;}
	Variable *keys()					{return x ? x->keys() : new Variable();}
	bool set(const string &s, const variable &v)	{return x ? x->set(s, v) : false;}
	void del(const string &s)	{if(x)x->del(s);}

	void prepare(Environment &env, Variable *v)	{if(x)x->prepare(env);else{variable a = env.pop();env.push(a.pointer());}}
	rsv call(Environment &env, variable &arg, Variable *v)	{return x ? x->call(env, arg) : variable(NIL);}
	void prepareInstance(Environment &env, Variable *v)	{if (x)x->prepareInstance(env);else env.push(rsv(v->clone(), 0));}
	rsv instance(Environment &env, Variable *v)	{if (x)return x->instance(env);else return v->clone();}

	size_t codelength()							{return x ? x->codelength() : 0;}
	Code *getcode()								{return x ? x->getcode() : NULL;}
	void pushcode(OpCode *c)					{if (x)x->pushcode(c);}
	void pushlabel(const string &s)				{if (x)x->pushlabel(s);}
	void write(const string &s, bytecode &b)	{if (x)x->write(s, b);}

	PSL_DUMP((){PSL_PRINTF(("vPointer:%s\n", x ? "" : "NULL"));if(x)x->dump();})
private:
	Variable *x;
};

class vRArray : public vBase
{
public:
	PSL_MEMORY_MANAGER(vRArray)
	vRArray():x(2){}
	vRArray(size_t i):x(i){}
	Type type()	const	{return RARRAY;}
	void searchcount(Variable *v, int &c)
	{
		size_t size = x.size();
		for (size_t i = 0; i < size; ++i)
			x[i].get()->searchcount(v, c);
	}
	void mark()
	{
		size_t size = x.size();
		for (size_t i = 0; i < size; ++i)
			x[i].get()->mark();
	}
	vBase *clone()
	{
		size_t size = x.size();
		if (size == 1)
			return x[0].get()->x->clone();
		vObject *v = new vObject(size);
		for (size_t i = 0; i < size; ++i)
			v->push(x[i].get());
		return v;
	}

	vBase *substitution(Variable *v)
	{
		Variable *o = v;
		rsv temp(v);
		if (v->type() == RARRAY)
			temp = rsv(v = v->clone(), 0);

		size_t size = x.size();
		size_t vsize = v->length();
		for (size_t i = 0; i < size && i < vsize; ++i)
		{
			if (x[i].get()->type() == POINTER)
				x[i].get()->substitution(o->index(i));
			else
				x[i].get()->substitution(v->index(i));
		}

		return this;
	}
	vBase *assignment(Variable *v)
	{
		Variable *o = v;
		rsv temp(v);
		if (v->type() == RARRAY)
			temp = rsv(v = v->clone(), 0);

		size_t size = x.size();
		size_t vsize = v->length();
		for (size_t i = 0; i < size && i < vsize; ++i)
			x[i].get()->assignment((x[i].get()->type() == POINTER ? o : v)->index(i));

		return this;
	}

	#define OP(n) void n(Variable *v)	{if (x.size() == 1)x[0].get()->n(v);}
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
	bool eq(Variable *v)
	{
		size_t size = x.size();
		if (size == 1)
			return x[0].get()->eq(v);
		if (size != v->length())
			return false;
		for (size_t i = 0; i < size; ++i)
			if (x[i].get()->ne(v->index(i)))
				return false;
		return true;
	}
	bool ne(Variable *v)	{return !eq(v);}
	#define CMP(n) bool n(Variable *v)	{if (x.size() == 1)return x[0].get()->n(v);return false;}
	CMP(le)
	CMP(ge)
	CMP(lt)
	CMP(gt)
	#undef CMP
	void neg()	{}
	Variable *deref()	{if (x.size() == 1)return x[0].get()->deref();return NULL;}

	bool toBool()		const {if (x.size() == 1)return x[0].get()->toBool();return x.size() != 0;}
	int toInt()			const {if (x.size() == 1)return x[0].get()->toInt();return static_cast<int>(x.size());}
	double toDouble()	const {if (x.size() == 1)return x[0].get()->toDouble();return x.size();}
	string toString()	const {if (x.size() == 1)return x[0].get()->toString();return "[tuple]";}
	void *toPointer()	const {if (x.size() == 1)return x[0].get()->toPointer();return NULL;}

	size_t length()				const {if (x.size() == 1)return x[0].get()->length();return x.size();}
	bool exist(const string &s)	const {if (x.size() == 1)return x[0].get()->exist(s);return false;}
	Variable *index(size_t t)	{
		if (x.size() == 1)return x[0].get()->index(t);
		if (t>=x.size())x.resize(t+1);return x[t].get();
	}
	Variable *child(const string &s)				{if (x.size() == 1)return x[0].get()->child(s);return NULL;}
	void push(Variable *v)							{x.push_back(v);}
	Variable *keys()								{if (x.size() == 1)return x[0].get()->keys();return new Variable();}
	bool set(const string &s, const variable &v)	{if (x.size() == 1)return x[0].get()->set(s, v);return false;}
	void del(const string &s)						{if (x.size() == 1)return x[0].get()->del(s);}

	void prepare(Environment &env, Variable *v)				{if (x.size() == 1)x[0].get()->prepare(env);}
	void prepareInstance(Environment &env, Variable *v)		{if (x.size() == 1)x[0].get()->prepareInstance(env);}
	rsv instance(Environment &env, Variable *v)				{if (x.size() == 1)return x[0].get()->instance(env);return v->clone();}
	rsv call(Environment &env, variable &arg, Variable *v)	{if (x.size() == 1)return x[0].get()->call(env, arg);return variable(NIL);}

	size_t codelength()							{if (x.size() == 1)return x[0].get()->codelength();return 0;}
	Code *getcode()								{if (x.size() == 1)return x[0].get()->getcode();return NULL;}
	void pushcode(OpCode *c)					{if (x.size() == 1)x[0].get()->pushcode(c);}
	void pushlabel(const string &s)				{if (x.size() == 1)x[0].get()->pushlabel(s);}
	void write(const string &s, bytecode &b)	{if (x.size() == 1)x[0].get()->write(s, b);}

	PSL_DUMP((){PSL_PRINTF(("vRArray:%d\n", (int)x.size()));for (size_t i = 0; i < x.size(); ++i)x[i].get()->dump();})
private:
	rlist x;
};

class vObject : public vBase
{
public:
	PSL_MEMORY_MANAGER(vObject)
	vObject()				{code = NULL;}
	vObject(size_t i):array(i)	{code = NULL;}
	vObject(Code *c)		{code = c->inc();}
	~vObject()				{if (code)	code->finalize();}
	void destructor()
	{
		#ifdef PSL_USE_VARIABLE_MEMORY_MANAGER
		const string &destructor = StaticObject::String::destructor();
		#else
			#ifdef PSL_THREAD_SAFE
		if (member.empty())	return;
			#endif
		PSL_CONST_STATIC string destructor = "destructor";
		#endif
		if (!member.count(destructor))
			return;
		Type t = member[destructor].get()->type();
		if (t == METHOD || t == CMETHOD || (t == CCMETHOD && array.size()))
		{
			PSL_TEMPORARY_ENV(env);
			variable arg;
			member[destructor].get()->call(env, arg);
			member.erase(destructor);
		}
	}
	Type type()	const	{return OBJECT;}
	vBase *clone()	{return new vObject(this);}
	vObject(vObject *v)
	{
		size_t size = v->array.size();
		array.resize(size);
		for (size_t i = 0; i < size; ++i)
			array[i].copy(v->array[i]);
		for (table::iterator it = v->member.begin(); it != v->member.end(); ++it)
			member[it->first].copy(it->second);
		if (v->code)	code = v->code->inc();
		else			code = NULL;
	}
	void searchcount(Variable *v, int &c)
	{
		size_t size = array.size();
		for (size_t i = 0; i < size; ++i)
			array[i].get()->searchcount(v, c);
		for (table::iterator it = member.begin(); it != member.end(); ++it)
			it->second.get()->searchcount(v, c);
	}
	void mark()
	{
		size_t size = array.size();
		for (size_t i = 0; i < size; ++i)
			array[0].get()->mark();
		for (table::iterator it = member.begin(); it != member.end(); ++it)
			it->second.get()->mark();
	}

	vBase *substitution(Variable *v)
	{
		if (!vObject::toBool() || (array.empty() && member.empty() && isFunction(v)))
		{
			vBase *x = v->bclone();
			#ifdef PSL_USE_DESTRUCTOR
			destructor();
			#endif
			delete this;
			return x;
		}

		Variable *o = v;
		rsv temp(v);
		if (v->type() == RARRAY)
			temp = rsv(v = v->clone(), 0);
		size_t size = v->length();
		if (size > array.size())
			array.resize(size);
		for (size_t i = 0; i < size; ++i)
		{
			Variable *z = array[i].get();
			z->substitution((z->type() == POINTER ? o : v)->index(i));
		}

		kcopy(v);
		ccopy(v);
		return this;
	}
	vBase *assignment(Variable *v)
	{
		if (!vObject::toBool() || (v->type() != OBJECT && v->type() != RARRAY))
		{
			vBase *x = v->bclone();
			#ifdef PSL_USE_DESTRUCTOR
			destructor();
			#endif
			delete this;
			return x;
		}

		Variable *o = v;
		rsv temp(v);
		if (v->type() == RARRAY)
			temp = rsv(v = v->clone(), 0);
		size_t size = v->length();
		if (size > array.size())
			array.resize(size);
		for (size_t i = 0; i < size; ++i)
		{
			Variable *z = array[i].get();
			z->assignment((z->type() == POINTER ? o : v)->index(i));
		}

		rsv k(v->keys());
		size = k.get()->length();
		for (size_t i = 0; i < size; ++i)
		{
			string s = k.get()->index(i)->toString();
			member[s].get()->assignment(v->child(s));
		}

		ccopy(v);
		return this;
	}

	void add(Variable *v)
	{
		size_t size = v->length();
		size_t c = array.size();
		array.resize(c + size);
		for (size_t i = 0; i < size; ++i)
			array[i+c].get()->substitution(v->index(i));

		kcopy(v);

		Code *co = v->getcode();
		if (co && code)
		{
			code = code->only();
			code->add(co);
		}
		else
		{
			ccopy(v);
		}
	}
	bool eq(Variable *v)
	{
		size_t size = v->length();
		if (size != array.size())
			return false;
		for (size_t i = 0; i < size; ++i)
		{
			if (array[i].get()->ne(v->index(i)))
				return false;
		}
		rsv k(v->keys());
		size = k.get()->length();
		if (member.size() != size)
			return false;
		for (size_t i = 0; i < size; ++i)
		{
			string s = k.get()->index(i)->toString();
			if (member[s].get()->ne(v->child(s)))
				return false;
		}
		return true;
	}
	bool ne(Variable *v)	{return !vObject::eq(v);}

	bool toBool()		const {return !array.empty() || !member.empty() || code;}
	int toInt()			const {return static_cast<int>(array.size());}
	double toDouble()	const {return array.size();}
	string toString()	const {return !array.empty() || !member.empty() ? "[Object]" : code ? "[function]" : "[null]";}
	void *toPointer()	const {return array.empty() ? NULL : array[0].get()->toPointer();}

	size_t length()				const {return array.size();}
	size_t memberLength()		const {return member.size();}
	bool exist(const string &s)	const {return member.count(s) > 0;}
	Variable *index(size_t t)			{if(t>=array.size())array.resize(t+1);return array[t].get();}
	Variable *child(const string &s)	{return member[s].get();}
	void push(Variable *v)	{rsv x(v->clone(),0);array.push_back(x);}
	Variable *keys()
	{
		rsv v;
		for (table::iterator it = member.begin(); it != member.end(); ++it)
		{
			rsv x(new Variable(it->first), 0);
			v.get()->push(x.get());
		}
		return v.get()->ref();
	}
	bool set(const string &s, const variable &v)
	{
		#ifdef PSL_USE_STL_MAP
		bool r = true;
		if (exist(s))
			r = false;
		member[s] = v;
		return r;
		#else
		return member.set(s, v);
		#endif
	}
	Variable *getifexist(const string &s)
	{
		table::iterator it = member.find(s);
		return (it != member.end()) ? it->second.get() : NULL;
	}

	void del(const string &s)	{member.erase(s);}
	void method_this(Variable *v)
	{
		size_t size = array.size();
		for (size_t i = 0; i < size; ++i)
			if (isMethod(array[i].get()))
				array[i].get()->push(v);
		for (table::iterator it = member.begin(); it != member.end(); ++it)
			if (isMethod(it->second.get()))
				it->second.get()->push(v);
	}

	void prepare(Environment &env, Variable *v)
	{
		if (!code)
		{
			vBase::prepare(env, v);
			return;
		}
		env.addScope(new FunctionScope(code, v));
	}
	void prepareInstance(Environment &env, Variable *v)
	{
		rsv x(createinstance(v), 0);
		if (!code)
		{
			env.push(x);
		}
		else
		{
			env.addScope(new ConstructorScope(code, v, x.get()));
			env.push(variable());
		}
	}
	rsv call(Environment &env, variable &arg, Variable *v)
	{
		if (!code)
			return variable(NIL);
		env.addScope(new FunctionScope(code, v));
		env.push(arg);
		env.Run();
		return env.pop();
	}
	rsv instance(Environment &env, Variable *v)
	{
		rsv x(createinstance(v), 0);
		if (!code)
			return x;
		env.addScope(new ConstructorScope(code, v, x.get()));
		env.push(variable());
		env.Run();
		return env.pop();
	}

	size_t codelength()		{return code ? code->length() : 0;}
	Code *getcode()			{return code;}
	void pushcode(OpCode *c){if (!code) code = new Code();code->push(c);}
	void pushlabel(const string &s){if (!code) code = new Code();code->pushlabel(s);}
	void write(const string &s, bytecode &b)
	{
		if (!code)
			return;
		b.push(OpCode::MNEMONIC::BEGIN);
		b.push(s.c_str(), s.length()+1);
		for (table::iterator it = member.begin(); it != member.end(); ++it)
			it->second.get()->write(it->first, b);
		code->write(b);
	}

	PSL_DUMP((){PSL_PRINTF(("vObject:%d,%d,%d\n", (int)array.size(), (int)member.size(), (int)codelength()));
		for (size_t i = 0; i < array.size(); ++i)array[i].get()->dump();
		for (table::iterator it = member.begin(); it != member.end(); ++it){PSL_PRINTF(("[%s]:\n", it->first.c_str()));it->second.get()->dump();}
		if(code){PSL_PRINTF(("--CODE--\n"));code->dump();}
	})
private:
	rlist array;
	table member;
	Code *code;
	void kcopy(Variable *v)
	{
		rsv k(v->keys());
		size_t size = k.get()->length();
		for (size_t i = 0; i < size; ++i)
		{
			string s = k.get()->index(i)->toString();
			member[s].get()->substitution(v->child(s));
		}
	}
	void ccopy(Variable *v)
	{
		if (Code *c = v->getcode())
		{
			if (code)
				code->finalize();
			code = c->inc();
		}
	}
	Variable *createinstance(Variable *v)
	{
		vObject *o = new vObject();
		Variable *x = new Variable(o);
		rsv temp(x, 0);

		size_t size = array.size();
		o->array.resize(size);
		for (size_t i = 0; i < size; ++i)
			o->array[i].copy(array[i]);

		for (table::iterator it = member.begin(); it != member.end(); ++it)
		{
			if (isMethod(it->second.get()))
			{
				rsv z(it->second.get()->clone(), 0);
				z.get()->push(x);
				o->member[it->first].set(z.get()->ref());
			}
			else if (it->second.get()->getcode())
			{
				rsv z(new Variable(new vMethod(it->second.get(), x)), 0);
				o->member[it->first].set(z.get()->ref());
			}
			else
			{
				o->member[it->first].copy(it->second);
			}
		}
		return x->ref();
	}
};

class vMethod : public vBase
{
public:
	PSL_MEMORY_MANAGER(vMethod)
	vMethod(Variable *v, Variable *x)
	{
		function = v->ref();
//		this_v = x->ref();	// circular reference
		this_v = x;
	}
	~vMethod()
	{
		function->finalize();
//		this_v->finalize();
	}
	Type type()	const	{return METHOD;}
	vBase *clone()	{return new vMethod(function, this_v);}
	void searchcount(Variable *v, int &c){function->searchcount(v, c);}
	void mark(){function->mark();}

	vBase *substitution(Variable *v)
	{
		Type t = v->type();
		if (t == METHOD)
		{
			function->finalize();
			function = v->index(0)->ref();
		}
		else if (t == OBJECT && v->getcode())
		{
			function->finalize();
			function = v->ref();
		}
		else if (t == CFUNCTION)
		{
			vBase *x = v->bclone();
			delete this;
			return x;
		}
		else if (t == CMETHOD)
		{
			vBase *x = v->bclone();
			x->push(this_v);
			delete this;
			return x;
		}
		return this;
	}

	bool toBool()		const {return true;}
	int toInt()			const {return 1;}
	string toString()	const {return "[Method]";}
	size_t length()		const {return 1;}
	void push(Variable *v)	{this_v = v;}
	Variable *index(size_t t)	{return t ? this_v : function;}

	void prepare(Environment &env, Variable *v)
	{
		if (!function->getcode())
			return;
		env.addScope(new MethodScope(function->getcode(), function, this_v));
	}
	rsv call(Environment &env, variable &arg, Variable *v)
	{
		if (!function->getcode())
			return variable(NIL);
		env.addScope(new MethodScope(function->getcode(), function, this_v));
		env.push(arg);
		env.Run();
		return env.pop();
	}
	Code *getcode()			{return function->getcode();}

	PSL_DUMP((){PSL_PRINTF(("vMethod:\n"));})
private:
	Variable *function;
	Variable *this_v;
};

class vCFunction : public vBase
{
public:
	PSL_MEMORY_MANAGER(vCFunction)
	vCFunction(function func)	{f = func;}
	Type type()	const	{return CFUNCTION;}
	vBase *clone()	{return new vCFunction(f);}

	vBase *substitution(Variable *v)	{
		if (isFunction(v))
		{
			vBase *x = v->bclone();
			delete this;
			return x;
		}
		return this;
	}

	bool toBool()		const {return true;}
	int toInt()			const {return 1;}
	string toString()	const {return "[CFunction]";}
	size_t length()		const {return 1;}

	void prepare(Environment &env, Variable *v)
	{
		variable x = env.pop();
		variable r = f(x);
		env.push(r);
	}
	rsv call(Environment &env, variable &arg, Variable *v)	{return f(arg);}
	PSL_DUMP((){PSL_PRINTF(("vCFunction:\n"));})
private:
	function f;
};

class vCMethod : public vBase
{
public:
	PSL_MEMORY_MANAGER(vCMethod)
	vCMethod(method func, Variable *x)	{f = func;this_v = x;}
	Type type()	const	{return CMETHOD;}
	vBase *clone()	{return new vCMethod(f, this_v);}

	vBase *substitution(Variable *v)	{
		if (isMethod(v))
		{
			vBase *x = v->bclone();
			x->push(this_v);
			delete this;
			return x;
		}
		return this;
	}

	bool toBool()		const {return true;}
	int toInt()			const {return this_v ? 1 : 0;}
	string toString()	const {return "[CMethod]";}
	size_t length()		const {return 1;}
	void push(Variable *v)	{this_v = v;}
	Variable *index(size_t t)	{return t ? this_v : NULL;}

	void prepare(Environment &env, Variable *v)
	{
		variable x = env.pop();
		if (this_v)
		{
			variable t = this_v;
			env.push(f(t, x));
		}
		else
		{
			variable z(NIL);
			env.push(f(z, x));
		}
	}
	rsv call(Environment &env, variable &arg, Variable *v)
	{
		if (this_v)
		{
			variable t = this_v;
			return f(t, arg);
		}
		else
		{
			variable z(NIL);
			return f(z, arg);
		}
	}
	PSL_DUMP((){PSL_PRINTF(("vCMethod:\n"));})
private:
	method f;
	Variable *this_v;
};

class vCPointer : public vBase
{
public:
	PSL_MEMORY_MANAGER(vCPointer)
	vCPointer(void *p)	{x = p;}
	Type type()	const	{return CPOINTER;}
	vBase *clone()	{return new vCPointer(x);}

	vBase *substitution(Variable *v)	{x = v->toPointer();return this;}

	#define CMP(n,o) bool n(Variable *v)	{return x o v->toPointer();}
	CMP(eq,==)
	CMP(ne,!=)
	CMP(le,<=)
	CMP(ge,>=)
	CMP(lt,<)
	CMP(gt,>)
	#undef CMP

	bool toBool()		const {return x ? true : false;}
	int toInt()			const {return x ? 1 : 0;}
	string toString()	const {return x ? "[cpointer]" : "NULL";}
	void *toPointer()	const {return x;}

	size_t length()		const {return x ? static_cast<size_t>(1) : 0;}

	PSL_DUMP((){PSL_PRINTF(("vCPointer:%p\n", x));})
private:
	void *x;
};

template<class T>class vSmartPointer : public vBase
{
public:
	PSL_MEMORY_MANAGER(vSmartPointer)
	vSmartPointer(T *p)	{x = p;}
	~vSmartPointer()	{delete x;}
	Type type()	const	{return SPOINTER;}
	vBase *clone()	{return new vBase();}

	vBase *substitution(Variable *v)	{return this;}
	bool eq(Variable *v)	{return x == v->toPointer();}
	bool ne(Variable *v)	{return x != v->toPointer();}

	bool toBool()		const {return x ? true : false;}
	int toInt()			const {return x ? 1 : 0;}
	string toString()	const {return x ? "[spointer]" : "NULL";}
	void *toPointer()	const {return x;}
	size_t length()		const {return x ? static_cast<size_t>(1) : 0;}

	PSL_DUMP((){PSL_PRINTF(("vSmartPointer:%p\n", x));})
private:
	T *x;
};

class vThread : public vBase
{
public:
	PSL_MEMORY_MANAGER(vThread)
	vThread()	{x = NULL;e = NULL;}
	vThread(Variable *v, Environment *ee)	{x = v ? v->ref() : NULL;e = ee ? ee->clone() : NULL;}
	~vThread()	{if (x)x->finalize();delete e;}
	Type type()	const	{return THREAD;}
	vBase *clone()	{return new vThread(x, e);}
	void searchcount(Variable *v, int &c){if (x)x->searchcount(v, c);}
	void mark(){if (x)x->mark();}

	vBase *substitution(Variable *v)
	{
		if (x)
		{
			x->finalize();
			delete e;
			e = NULL;
		}
		x = v->ref();
		return this;
	}

	bool toBool() const
	{
		if (!x)	return false;
		if (!e)	return x->getcode() != NULL;
		return e->Runable();
	}
	int toInt()			const {return x ? 1 : 0;}
	string toString()	const {return "[Thread]";}

	size_t length()		const {return x ? static_cast<size_t>(1) : 0;}
	Variable *index(size_t t)			{return t ? x : NULL;}
	Variable *child(const string &s)	{return (e && e->Runable()) ? e->getVariable(s).get() : NULL;}

	void prepare(Environment &env, Variable *v)
	{
		if (!vThread::toBool())
		{
			rsv r(new Variable(new vThread(env.pop().get(), NULL)), 0);
			env.push(r);
			return;
		}
		newenv(env);
		e->push(env.pop());
		e->Run();
		env.push(e->pop());
	}
	rsv call(Environment &env, variable &arg, Variable *v)
	{
		if (!vThread::toBool())
			return variable(NIL);
		newenv(env);
		e->push(arg);
		e->Run();
		return e->pop();
	}

	PSL_DUMP((){PSL_PRINTF(("vThread:%p\n", e));if(x)x->dump();})
private:
	Variable *x;
	Environment *e;
	void newenv(Environment &env)
	{
		if (e)	return;
		e = new Environment(env);
		x->prepare(*e);
	}
};
#include "binder.h"
