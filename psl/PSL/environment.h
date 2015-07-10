class Environment;
class OpCode
{
public:
	struct MNEMONIC
	{
		enum mnemonic
		{
			NOP = 0,
			CONSTANT,
			PUSH_INT,
			PUSH_HEX,
			PUSH_FLOAT,
			PUSH_STRING,
			PUSH_NULL,
			PUSH_CODE,
			VARIABLE,
			POP,

			GLOBAL,LOCAL,STATIC,
			DECLARATION,
			INSTANCE,

			MEMBER,
			INDEX,
			LOCALINDEX,
			SET_MEMBER,
			SET_INDEX,
			ARRAY_PUSH,

			CALL,RETURN,
			BREAK,CONTINUE,
			YIELD,
			GOTO,
			SCOPE,IF,LOOP,

			SUBSTITUTION,ASSIGNMENT,
			ARGUMENT,

			UNARY,
			PLUS,MINUS,
			DEREF,REF,
			NOT,COMPL,
			INC,DEC,PINC,PDEC,
			CLOSURE,

			BINARY,
			ADD,SUB,MUL,DIV,MOD,AND,OR,XOR,SHL,SHR,
			SADD,SSUB,SMUL,SDIV,SMOD,SAND,SOR,SXOR,SSHL,SSHR,

			EQ,NEQ,LE,GE,LT,GT,
			BAND,BOR,

			JMP,JT,JF,JR,JRT,JRF,

			LIST,			// , comma
			PARENTHESES,	// ()

			LABELBEGIN = 0xF0,
			BEGIN,
			END = 0xFF
		};
	};
	struct RC
	{
		enum RETURNCODE
		{
			NONE = 0,
			CALL,RETURN,END,
			BREAK,CONTINUE,
			GOTO,
			YIELD,
			SCOPE,IF,LOOP,
		};
	};
	PSL_MEMORY_MANAGER(OpCode)
	virtual ~OpCode(){}
	virtual RC::RETURNCODE Execute(Environment &env) = 0;
	virtual OpCode *clone() = 0;
	virtual void set(int s){}
	virtual MNEMONIC::mnemonic get()	{return MNEMONIC::NOP;}
	virtual void write(bytecode &b)	{};
	virtual void dump(int d = 0){};
};

class Scope;
class Environment
{
	const static unsigned long WARNING = 16
	#ifdef PSL_WARNING_POP_EMPTY_STACK
		|4
	#endif
	#ifdef PSL_WARNING_STACK_REMAINED
		|1
	#endif
	#ifdef PSL_WARNING_UNDECLARED_IDENTIFIER
		|2
	#endif
	#ifdef PSL_WARNING_DECLARED_IDENTIFIER_ALREADY_EXIST
		|8
	#endif
	;
public:
	void warning(const int n, const string &s = "")
	{
		if (WARNING & (1 << n))
		{
			PSL_PRINTF(("%s : ", scope->getCurrentLabel().c_str()));
			switch (n)
			{
			case 0:PSL_PRINTF(("runtime warning : %s stack remained when deleting env\n", s.c_str()));break;
			case 1:PSL_PRINTF(("runtime warning : undeclared identidier %s\n", s.c_str()));break;
			case 2:PSL_PRINTF(("runtime error : pop empty stack\n"));break;
			case 3:PSL_PRINTF(("runtime warning : declared identidier %s already exist, over write\n", s.c_str()));break;
			case 4:PSL_PRINTF(("runtime warning : undefined member name %s\n", s.c_str()));break;
			}
		}
	}
	PSL_MEMORY_MANAGER(Environment)
	#if defined(PSL_SHARED_GLOBAL) && defined(PSL_USE_VARIABLE_MEMORY_MANAGER)
	Environment():global(StaticObject::global())		{scope = NULL;}
	Environment(int i):global(StaticObject::global())	{scope = NULL;if (i){PSLlib::Basic(global);PSLlib::Standard(global);}}
	#else
	Environment(int i)	{scope = NULL;if (i){PSLlib::Basic(global);PSLlib::Standard(global);}}
	Environment()		{scope = NULL;PSLlib::Basic(global);}
	#endif
	Environment(const Environment &env):global(env.global)	{scope = NULL;}
	~Environment()	{delete scope;if (stack.size())warning(0, static_cast<int>(stack.size()));}
	Environment *clone()
	{
		Environment *e = new Environment(*this);
		if (scope)
			e->scope = scope->clone();
		size_t s = stack.size();
		#ifdef PSL_USE_STL_STACK
		if (s)
		{
			rlist l(s);
			for (size_t i = s; i > 0;)
			{
				l[--i] = stack.top();
				stack.pop();
			}
			for (size_t i = 0; i < s; ++i)
			{
				stack.push(l[i]);
				e->stack.push(l[i]);
			}
		}
		#else
		for (size_t i = 0; i < s; ++i)
			e->stack[i] = stack[i];
		#endif
		return e;
	}
	rsv getVariable(const string &name)
	{
		Variable *v;
		v = scope->getVariable(name);		if (v)return v;
		v = global.get()->getifexist(name);	if (v)return v;
		warning(1, name);
		variable x;
		scope->addLocal(name, x);
		return x;
	}
	rsv getLocalIndex(size_t i)	{return scope->getLocalIndex(i);}
	rsv getLocal()				{return scope->getLocal();}
	rsv setLocal(const rsv &v)	{return scope->setLocal(v);}
	void addLocal(const string &name, variable &v)		{if (!scope->addLocal(name, v, this))warning(3, name);}
	void addGlobal(const string &name, variable &v)		{if (!global.get()->set(name, v))warning(3, name);push(v);}
	void addStatic(const string &name, variable &v)		{scope->addStatic(name, v, this);}
	void Declaration(const string &name, variable &v)	{if (!scope->Declaration(name, v, this))warning(3, name);}
	void addScope(Scope *s)	{s->set(scope);scope = s;}
	void Jump(int l)	{scope->Jump(l);}
	void RJump(int l)	{scope->RJump(l);}
	void endScope()		{scope = scope->End(*this);}
	void Run()			{while (scope && scope->Run(*this));}
	#ifdef PSL_DEBUG
	void StepExec()		{if (scope) scope->StepExec(*this);}
	#endif
	void Return()		{scope = scope->Return();}
	void Break()		{scope = scope->Break();}
	void Continue()		{scope = scope->Continue();}
	void Goto(const string &label)	{Scope *s = scope->Goto(label);if (s)scope = s;}
	OpCode::MNEMONIC::mnemonic getNext()		{return scope->getNext();}
	void push(const rsv &v)	{stack.push(v);}
	rsv pop()
	{
		#ifdef PSL_CHECKSTACK_POP
		if (stack.empty())
		{
			warning(2);
			return rsv();
		}
		#endif
	#ifdef PSL_USE_STL_STACK
		rsv v = stack.top();
		stack.pop();
		return v;
	#else
		return stack.pop();
	#endif
	}
	rsv &top()
	{
		#ifdef PSL_CHECKSTACK_POP
		if (stack.empty())
		{
			warning(2);
			stack.push(rsv());
		}
		#endif
		return stack.top();
	}
	bool Runable()	{return scope != NULL;}
	#ifdef PSL_MEMBER_REGISTER
	rsv reg;
	#endif
private:
	rsv global;
	rstack stack;
	Scope *scope;
	friend class PSLVM;
};

class Code
{
public:
	PSL_MEMORY_MANAGER(Code)
	Code()	{rc = 1;}
	~Code()
	{
		for (size_t i = 0; i < code.size(); ++i)
			delete code[i];
	}
	void finalize()	{if (!--rc)delete this;}
	Code(Code *c)
	{
		rc = 1;
		for (size_t i = 0; i < c->code.size(); ++i)
			code.push_back(c->code[i]->clone());
		for (table::iterator it = c->label.begin(); it != c->label.end(); ++it)
			label[it->first] = it->second;
	}
	Code *only()
	{
		if (rc == 1)
			return this;
		else
		{
			finalize();
			return new Code(this);
		}
	}
	void add(Code *c)
	{
		size_t s = code.size();
		if (s > 0 && code[s-1]->get() == OpCode::MNEMONIC::RETURN)
		{
			delete code[--s];
			code.resize(s);
		}
		Code *cl = new Code(c);
		vObject o(cl);
		cl->finalize();
		push(cl);
		for (table::iterator it = cl->label.begin(); it != cl->label.end(); ++it)
			label[it->first] = variable(it->second.get()->toInt() + static_cast<int>(s));
	}
	bool Run(Environment &env, size_t &line)
	{
		size_t size = code.size();
		while (line < size)
		{
			OpCode::RC::RETURNCODE r = code[line++]->Execute(env);
			if (!r)						continue;
			if (r == OpCode::RC::YIELD)	return false;
			return true;
		}
		env.endScope();
		return true;
	}
	#ifdef PSL_DEBUG
	void StepExec(Environment &env, size_t &line)
	{
		if (line < code.size())
		{
			for (table::iterator it = label.begin(); it != label.end(); ++it)
			{
				if (static_cast<size_t>(it->second.get()->toInt()) == line)
					PSL_PRINTF(("%s:\n", it->first.c_str()));
			}
			PSL_PRINTF(("exec: "));
			code[line]->dump(1);
			if (code[line++]->Execute(env) == OpCode::RC::YIELD)	// スタック不整合を防ぐ為に、ひとまずyield用にNULLを渡すことにする
				env.push(rsv());
		}
		else
		{
			PSL_PRINTF(("exec: END\n"));
			env.endScope();
		}
	}
	#endif
	OpCode::MNEMONIC::mnemonic get(size_t line)
	{
		if (line < code.size())	return code[line]->get();
		else					return OpCode::MNEMONIC::END;
	}
	bool Goto(const string &s, size_t &line)
	{
		if (!label.count(s))
			return false;
		line = static_cast<size_t>(label[s].get()->toInt());
		return true;
	}
	void pushlabel(const string &s)			{label[s] = variable(code.size());}
	void pushlabel(const string &s, int l)	{label[s] = variable(l);}
	void push(OpCode *c)
	{
		#ifdef PSL_OPTIMIZE_IN_COMPILE
		if (optimize(c))
		#endif
		code.push_back(c);
	}
	void push(Code *c)
	{
		size_t size = c->code.size();
		for (size_t i = 0; i < size; ++i)
			code.push_back(c->code[i]);
		c->code.resize(0);
	}
	size_t length()	{return code.size();}
	Code *inc()	{++rc;return this;}
	string getCurrentLabel(size_t line)
	{
		string s;
		size_t c = 0;
		for (table::iterator it = label.begin(); it != label.end(); ++it)
		{
			size_t i = static_cast<size_t>(it->second.get()->toInt());
			if (i <= line && i >= c)
			{
				s = it->first;
				c = i;
			}
		}
		return s;
	}
	PSL_DUMP((){
		for (size_t i = 0; i < code.size(); ++i)
			code[i]->dump();
		for (table::iterator it = label.begin(); it != label.end(); ++it)
			PSL_PRINTF(("%s:%d\n", it->first.c_str(), it->second.get()->toInt()));
	})
	void write(bytecode &b)
	{
		size_t size = code.size();
		for (size_t i = 0; i < size; ++i)
			code[i]->write(b);
		if (label.size())
		{
			b.push(OpCode::MNEMONIC::LABELBEGIN);
			for (table::iterator it = label.begin(); it != label.end(); ++it)
			{
				int x = it->second.get()->toInt();
				b.push(&x, sizeof(x));
				b.push(it->first.c_str(), it->first.length()+1);
			}
			b.push(OpCode::MNEMONIC::END);
		}
		b.push(OpCode::MNEMONIC::END);
	};
private:
	#ifdef PSL_USE_STL_VECTOR
	std::
	#endif
	vector<OpCode*> code;
	table label;
	int rc;
	bool optimize(OpCode *c)
	{
		size_t s = code.size();
		OpCode::MNEMONIC::mnemonic cn = c->get();
		if (cn == OpCode::MNEMONIC::PLUS)
		{
			delete c;
			return false;
		}
		if (s < 1)
			return true;
		if (cn == OpCode::MNEMONIC::POP)
		{
			OpCode::MNEMONIC::mnemonic n = code[s-1]->get();
			#ifdef PSL_OPTIMIZE_IMMEDIATELY_POP
			if (n == OpCode::MNEMONIC::CONSTANT || n == OpCode::MNEMONIC::VARIABLE)
			{
				if (s < 2 || code[s-2]->get() != OpCode::MNEMONIC::JR)
				{
					delete c;
					delete code[--s];
					code.resize(s);
					return false;
				}
			}
			#endif
			#ifdef PSL_OPTIMIZE_SUFFIX_INCREMENT
			if (n == OpCode::MNEMONIC::INC)
			{
				delete code[--s];
				code[s] = new PINC();
				return true;
			}
			if (n == OpCode::MNEMONIC::DEC)
			{
				delete code[--s];
				code[s] = new PDEC();
				return true;
			}
			#endif
		}
		#ifdef PSL_OPTIMIZE_CONSTANT_CALCULATION
		if (cn == OpCode::MNEMONIC::UNARY || cn == OpCode::MNEMONIC::INC || cn == OpCode::MNEMONIC::DEC)
		{
			if (code[s-1]->get() == OpCode::MNEMONIC::CONSTANT)
			{
				if (s < 2 || code[s-2]->get() != OpCode::MNEMONIC::JR)
				{
					PSL_TEMPORARY_ENV0(optimizer);
					code[s-1]->Execute(optimizer);
					variable v = optimizer.pop();
					optimizer.push(v);
					c->Execute(optimizer);
					delete c;
					variable a = optimizer.pop();
					v = a;
					return false;
				}
			}
		}
		if (cn == OpCode::MNEMONIC::BINARY && s >= 2)
		{
			if ((code[s-1]->get() == OpCode::MNEMONIC::CONSTANT) && (code[s-2]->get() == OpCode::MNEMONIC::CONSTANT))
			{
				if (s < 3 || code[s-3]->get() != OpCode::MNEMONIC::JR)
				{
					PSL_TEMPORARY_ENV0(optimizer);
					code[s-2]->Execute(optimizer);
					code[--s]->Execute(optimizer);
					variable r = optimizer.pop();
					variable l = optimizer.pop();
					optimizer.push(l);
					optimizer.push(r);
					c->Execute(optimizer);
					delete c;
					variable a = optimizer.pop();
					l = a;
					delete code[s];
					code.resize(s);
					return false;
				}
			}
		}
		#endif
		return true;
	}
};

class Scope
{
public:
#ifdef PSL_USE_VARIABLE_MEMORY_MANAGER
	static void *operator new(size_t t)		{return SMemoryManager::Next();}
	static void operator delete(void *ptr)	{SMemoryManager::Release(ptr);}
#endif
	Scope(Code *c)			{code = c->inc();line = 0;owner = NULL;}
	virtual ~Scope()		{code->finalize();delete owner;}
	virtual Scope *clone() = 0;
	virtual Variable *getVariable(const string &name)
	{
		if (Variable *v = local.get()->getifexist(name))return v;
		else											return owner->getVariable(name);
	}
	void set(Scope *s)	{owner = s;}
	void Jump(int l)	{line = static_cast<size_t>(l);}
	void RJump(int l)	{line = static_cast<size_t>(static_cast<int>(line) + l);}
	bool Run(Environment &env)	{return code->Run(env, line);}
	#ifdef PSL_DEBUG
	void StepExec(Environment &env)	{return code->StepExec(env, line);}
	#endif
	OpCode::MNEMONIC::mnemonic getNext()	{return code->get(line);}
	virtual Scope *Return() = 0;
	virtual Scope *Break() = 0;
	virtual Scope *Continue() = 0;
	virtual Scope *Goto(const string &label)
	{
		if (code->Goto(label, line))
			return this;
		if (!owner)
			return NULL;
		if (Scope *s = owner->Goto(label))
		{
			owner = NULL;
			delete this;
			return s;
		}
		return NULL;
	}
	virtual Scope *End(Environment &env)
	{
		Scope *s = owner;
		owner = NULL;
		delete this;
		return s;
	}
	bool addLocal(const string &name, variable &v, Environment *env = NULL)
	{
		bool r = local.get()->set(name, v);
		if (env)	env->push(v);
		return r;
	}
	virtual bool addStatic(const string &name, variable &v, Environment *env)
	{
		if (owner)	return owner->addStatic(name, v, env);
		if (env)	env->push(v);
		return false;
	}
	virtual bool Declaration(const string &name, variable &v, Environment *env)
	{
		bool r = local.get()->set(name, v);
		if (env)	env->push(v);
		return r;
	}
	rsv getLocalIndex(size_t i)	{return local.get()->index(i);}
	virtual rsv getLocal()
	{
		if (owner)
		{
			variable v = owner->getLocal();
			variable l = local;
			v = l;
			return v;
		}
		return local;
	}
	virtual rsv setLocal(const rsv &v)
	{
		if (owner)
			owner->setLocal(v);
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
	string getCurrentLabel()	{return code->getCurrentLabel(line);}
protected:
	rsv local;
	Scope *owner;
	Code *code;
	size_t line;
	void copyto(Scope *s)
	{
		variable l = local;
		variable c = l;
		s->local = c;
		if (owner)
			s->owner = owner->clone();
		s->line = line;
	}
};

#include "scope.h"
#include "code.h"
