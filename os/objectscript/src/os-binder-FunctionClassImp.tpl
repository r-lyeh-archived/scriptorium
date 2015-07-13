template <class R, class T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_TEMPLATE_PARMS> 
struct OS_BIND_FUNC_RUN_CLASS_NAME
{
	typedef R(OS_BIND_FUNC_CC T::*F)(OS_BIND_FUNC_PARMS){const} OS_BIND_FUNC_CC_GNUC;

	static int run(OS * os, int params, int, int, void * user_param)
	{
		OS_GET_TEMPLATE_SELF(T*);
		OS_BIND_FUNC_GET_ARGS;
		typedef typename RemoveConst<R>::type type;
		F& f = *(F*)user_param;
		// CtypeValue<type>::push(os, CtypeValue<type>::to((self->*f)(OS_BIND_FUNC_ARGS)));
		CtypeValue<type>::push(os, (self->*f)(OS_BIND_FUNC_ARGS));
		return 1;
	}
};

template <class T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_TEMPLATE_PARMS> 
struct OS_BIND_FUNC_RUN_CLASS_NAME<void, T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_PARMS>
{
	typedef void(OS_BIND_FUNC_CC T::*F)(OS_BIND_FUNC_PARMS){const} OS_BIND_FUNC_CC_GNUC;

	static int run(OS * os, int params, int, int, void * user_param)
	{
		OS_GET_TEMPLATE_SELF(T*);
		OS_BIND_FUNC_GET_ARGS;
		F& f = *(F*)user_param;
		(self->*f)(OS_BIND_FUNC_ARGS);
		return 0;
	}
};

template <class R, class T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_TEMPLATE_PARMS>
struct OS_BIND_FUNC_CLASS_NAME
{
	typedef R(OS_BIND_FUNC_CC T::*F)(OS_BIND_FUNC_PARMS){const} OS_BIND_FUNC_CC_GNUC;

	const char * name;
	F f;

	OS_BIND_FUNC_CLASS_NAME(const char * _name, F _f): name(_name), f(_f){}
	
	operator OS::FuncDef() const 
	{ 
		OS::FuncDef def = {name, 
			OS_BIND_FUNC_RUN_CLASS_NAME<R, T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_PARMS>::run, 
			&(FunctionData<F>::create(f))->f}; 
		return def; 
	}
};

// namespace ObjectScript {

template <class R, class T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_TEMPLATE_PARMS> 
OS::FuncDef def(const char * name, R(OS_BIND_FUNC_CC T::*f)(OS_BIND_FUNC_PARMS){const} OS_BIND_FUNC_CC_GNUC)
{
	typedef OS_BIND_FUNC_CLASS_NAME<R, T OS_BIND_FUNC_PARMS_COMMA OS_BIND_FUNC_PARMS> Func; 
	return Func(name, f);
}

// } // namespace ObjectScript
