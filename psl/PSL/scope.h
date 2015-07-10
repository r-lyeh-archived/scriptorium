class AnonymousScope : public Scope
{
public:
	AnonymousScope(Code *statement) : Scope(statement)	{}
	virtual Scope *clone()
	{
		AnonymousScope *s = new AnonymousScope(code);
		copyto(s);
		return s;
	}
	virtual Scope *Return()
	{
		Scope *s = owner->Return();
		owner = NULL;
		delete this;
		return s;
	}
	virtual Scope *Break()
	{
		Scope *s = owner->Break();
		owner = NULL;
		delete this;
		return s;
	}
	virtual Scope *Continue()
	{
		Scope *s = owner->Continue();
		owner = NULL;
		delete this;
		return s;
	}
};

class LoopScope : public Scope
{
public:
	LoopScope(Code *statement, size_t c) : Scope(statement)	{cline = c;}
	virtual Scope *clone()
	{
		LoopScope *s = new LoopScope(code, cline);
		copyto(s);
		return s;
	}
	virtual Scope *Return()
	{
		Scope *s = owner->Return();
		owner = NULL;
		delete this;
		return s;
	}
	virtual Scope *Break()
	{
		Scope *s = owner;
		owner = NULL;
		delete this;
		return s;
	}
	virtual Scope *Continue()
	{
		line = cline;
		return this;
	}
private:
	size_t cline;
};

class FunctionScope : public Scope
{
public:
	FunctionScope(Code *statement, Variable *v) : Scope(statement),static_v(v)	{}
	virtual Scope *clone()
	{
		FunctionScope *s = new FunctionScope(code, static_v.get());
		copyto(s);
		return s;
	}
	virtual Variable *getVariable(const string &name)
	{
		Variable *v;
		v = local.get()->getifexist(name);		if (v)return v;
		v = static_v.get()->getifexist(name);	if (v)return v;
		return NULL;
	}
	virtual bool addStatic(const string &name, variable &v, Environment *env)
	{
		if (static_v.get()->exist(name))
		{
			variable x;
			if (env)	env->push(x);
			return false;
		}
		bool r = static_v.get()->set(name, v);
		if (env)	env->push(v);
		return r;
	}
	virtual Scope *Return()
	{
		Scope *s = owner;
		owner = NULL;
		delete this;
		return s;
	}
	virtual Scope *Break()
	{
		return this;
	}
	virtual Scope *Continue()
	{
		return this;
	}
	virtual Scope *Goto(const string &label)
	{
		if (code->Goto(label, line))
			return this;
		return NULL;
	}
	virtual Scope *End(Environment &env)
	{
		Scope *s = owner;
		owner = NULL;
		delete this;
		return s;
	}
	virtual rsv getLocal()	{return local;}
	virtual rsv setLocal(const rsv &v)
	{
		variable x = v;
		variable l = local;
		variable k = l.keys();
		for (size_t i = 0; i < k.length(); ++i)
		{
			string s = k[i];
			rsv z = l[s];
			if (v.get() != z.get() && (z.get()->type() != POINTER || z.get()->toBool()))
				x.set(s, l[s]);
		}
		return v;
	}
protected:
	rsv static_v;
};

class MethodScope : public FunctionScope
{
public:
	MethodScope(Code *statement, Variable *s, Variable *t) : FunctionScope(statement, s),this_v(t)	{}
	virtual Scope *clone()
	{
		MethodScope *s = new MethodScope(code, static_v.get(), this_v.get());
		copyto(s);
		return s;
	}
	Variable *getVariable(const string &name)
	{
		Variable *v;
		v = local.get()->getifexist(name);		if (v)return v;
		v = this_v.get()->getifexist(name);		if (v)return v;
		v = static_v.get()->getifexist(name);	if (v)return v;
		return NULL;
	}
protected:
	rsv this_v;
};
class ConstructorScope : public MethodScope
{
public:
	ConstructorScope(Code *statement, Variable *s, Variable *t) : MethodScope(statement, s, t)	{}
	bool Declaration(const string &name, variable &v, Environment *env)
	{
		bool r = this_v.get()->set(name, v);
		if (env)	env->push(v);
		return r;
	}
	Scope *End(Environment &env)
	{
		Scope *s = owner;
		owner = NULL;
		env.top() = this_v;
		delete this;
		return s;
	}
};
