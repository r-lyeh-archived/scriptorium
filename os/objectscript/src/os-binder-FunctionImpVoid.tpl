template <class R> 
struct OS_BIND_FUNC_RUN_CLASS_NAME
{
	typedef R(OS_BIND_FUNC_CC *F)() OS_BIND_FUNC_CC_GNUC;

	static int run(OS * os, int params, int, int, void * user_param)
	{
		OS_BIND_FUNC_GET_ARGS;
		typedef typename RemoveConst<R>::type type;
		F& f = *(F*)user_param;
		CtypeValue<type>::push(os, (*f)());
		return 1;
	}
};

template <> 
struct OS_BIND_FUNC_RUN_CLASS_NAME<void>
{
	typedef void(OS_BIND_FUNC_CC *F)() OS_BIND_FUNC_CC_GNUC;

	static int run(OS * os, int params, int, int, void * user_param)
	{
		OS_BIND_FUNC_GET_ARGS;
		F& f = *(F*)user_param;
		(*f)();
		return 0;
	}
};

template <class R>
struct OS_BIND_FUNC_CLASS_NAME
{
	typedef R(OS_BIND_FUNC_CC *F)() OS_BIND_FUNC_CC_GNUC;

	const char * name;
	F f;

	OS_BIND_FUNC_CLASS_NAME(const char * _name, F _f): name(_name), f(_f){}

	operator OS::FuncDef() const 
	{ 
		OS::FuncDef def = {name, 
			OS_BIND_FUNC_RUN_CLASS_NAME<R>::run, 
			&(FunctionData<F>::create(f))->f}; 
		return def; 
	}
};

// namespace ObjectScript {

template <class R> 
OS::FuncDef def(const char * name, R(OS_BIND_FUNC_CC *f)() OS_BIND_FUNC_CC_GNUC)
{
	typedef OS_BIND_FUNC_CLASS_NAME<R> Func; 
	return Func(name, f);
}

// } // namespace ObjectScript
