#ifndef __OS_BINDER_H__
#define __OS_BINDER_H__

/******************************************************************************
* Copyright (C) 2012-2014 Evgeniy Golovin (evgeniy.golovin@unitpoint.ru)
*
* Please feel free to contact me at anytime, 
* my email is evgeniy.golovin@unitpoint.ru, skype: egolovin
*
* Latest source code: https://github.com/unitpoint/objectscript
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************/

#include "objectscript.h"
#include <string>

namespace ObjectScript {

// =====================================================================

template <class T> struct RemoveConst          { typedef T type; };
template <class T> struct RemoveConst<const T> { typedef T type; };
template <class T> struct RemoveConst<const T&> { typedef T type; };
template <class T> struct RemoveConst<const T*> { typedef T * type; };

template <class T> struct RemoveRef		{ typedef T type; };
template <class T> struct RemoveRef<T&> { typedef T type; };

template <class T> struct RemovePtr		{ typedef T type; };
template <class T> struct RemovePtr<T*> { typedef T type; };

template <class T> struct PlainType { typedef typename RemovePtr<typename RemoveRef<typename RemoveConst<T>::type>::type>::type type; };

// =====================================================================

template <class T>
struct CtypeId
{
	static int getId(){ static int id = (int)(intptr_t)&id; return id; }
	static int getInstanceId(){ static int id = (int)(intptr_t)&id; return id; }
};

template <class T>
struct CtypeName
{
	static const OS_CHAR * getName();
};

// =====================================================================

#define OS_DECL_CTYPE(type) OS_DECL_CTYPE_NAME(type, #type)
#define OS_DECL_CTYPE_NAME(type, name) template <> struct CtypeName<type> { static const OS_CHAR * getName(){ return name; } };

// =====================================================================

template <class T>
struct CtypeValue
{
};

// =====================================================================

// }

OS_DECL_CTYPE(bool);

// namespace ObjectScript {

template <>
struct CtypeValue<bool>
{
	typedef bool type;

	static bool isValid(type){ return true; }

	static type def(ObjectScript::OS*){ return type(); }
	static type getArg(ObjectScript::OS * os, int offs)
	{
		return os->toBool(offs);
	}

	static void push(ObjectScript::OS * os, bool val)
	{
		os->pushBool(val);
	}
};

// =====================================================================

OS_DECL_CTYPE(std::string);

template <>
struct CtypeValue<std::string>
{
	typedef std::string type;

	static bool isValid(const type&){ return true; }

	static type def(ObjectScript::OS*){ return type(); }
	static type getArg(ObjectScript::OS * os, int offs)
	{
		return os->toString(offs).toChar();
	}

	static void push(ObjectScript::OS * os, const type& val)
	{
		os->pushString(val.c_str());
	}
};

// =====================================================================

OS_DECL_CTYPE(ObjectScript::OS::String);

template <>
struct CtypeValue<ObjectScript::OS::String>
{
	typedef ObjectScript::OS::String type;

	static bool isValid(const type&){ return true; }

	static type def(ObjectScript::OS * os){ return type(os); }
	static type getArg(ObjectScript::OS * os, int offs)
	{
		return os->toString(offs);
	}

	static void push(ObjectScript::OS * os, const type& val)
	{
		os->pushString(val);
	}
};

// =====================================================================

OS_DECL_CTYPE_NAME(OS_CHAR*, "char_ptr");
OS_DECL_CTYPE(OS_CHAR);

template <>
struct CtypeValue<OS_CHAR*>
{
	typedef const OS_CHAR * type;

	static bool isValid(const OS_CHAR *){ return true; }

	static type def(ObjectScript::OS*){ return ""; }
	static type getArg(ObjectScript::OS * os, int offs)
	{
		return os->toString(offs).toChar();
	}

	static void push(ObjectScript::OS * os, const OS_CHAR * val)
	{
		os->pushString(val);
	}
};

// =====================================================================

OS_DECL_CTYPE(ObjectScript::OS);

template <>
struct CtypeValue<ObjectScript::OS*>
{
	typedef ObjectScript::OS * type;

	static bool isValid(ObjectScript::OS * p){ return p != NULL; }

	static type def(ObjectScript::OS * os){ return os; }
	static type getArg(ObjectScript::OS * os, int& offs)
	{
		offs--;
		return os;
	}
};

// =====================================================================

template <class T>
struct CtypeNumber
{
	typedef typename RemoveConst<T>::type type;

	static bool isValid(type){ return true; }

	static type def(ObjectScript::OS*){ return type(); }
	static type getArg(ObjectScript::OS * os, int offs)
	{
		return (type)os->toNumber(offs);
	}

	static void push(ObjectScript::OS * os, const type& val)
	{
		os->pushNumber((OS_NUMBER)val);
	}
};

#define OS_DECL_CTYPE_NUMBER(type) \
	OS_DECL_CTYPE(type); \
	template <> struct CtypeValue<type>: public CtypeNumber<type> {}

// };

OS_DECL_CTYPE_NUMBER(float);
OS_DECL_CTYPE_NUMBER(double);
OS_DECL_CTYPE_NUMBER(long double);
OS_DECL_CTYPE_NUMBER(int);
OS_DECL_CTYPE_NUMBER(unsigned int);
OS_DECL_CTYPE_NUMBER(signed char);
OS_DECL_CTYPE_NUMBER(unsigned char);
OS_DECL_CTYPE_NUMBER(short);
OS_DECL_CTYPE_NUMBER(unsigned short);
OS_DECL_CTYPE_NUMBER(long);
OS_DECL_CTYPE_NUMBER(unsigned long);
OS_DECL_CTYPE_NUMBER(long long);
OS_DECL_CTYPE_NUMBER(unsigned long long);

// namespace ObjectScript {

// =====================================================================

template <class T> void pushCtypeValue(ObjectScript::OS * os, const T& obj)
{
	typedef typename RemoveConst<T>::type type;
	// CtypeValue<type>::push(os, CtypeValue<type>::to(obj));
	CtypeValue<type>::push(os, (const type&)obj);
}

// =====================================================================

template <class T> struct UserObjectDestructor
{
	static void	dtor(T * p)
	{
		// delete p;
	}
};

template <class T> struct UserDataDestructor
{
	static void	dtor(ObjectScript::OS * os, void * data, void * user_param)
	{
		UserObjectDestructor<T>::dtor((T*)data);
	}
};

template <class T> struct CtypeUserClass{};
template <class T> struct CtypeUserClass<T*>
{
	typedef typename RemoveConst<T>::type ttype;
	typedef typename RemoveConst<T>::type * type;

	static bool isValid(const type p){ return p != NULL; }
	static type def(ObjectScript::OS*){ return type(); }
	static type getArg(ObjectScript::OS * os, int offs){ return (type)os->toUserdata(CtypeId<ttype>::getInstanceId(), offs, CtypeId<ttype>::getId()); }
	static void push(ObjectScript::OS * os, const type val)
	{
		if(!val){
			os->pushNull();
			return;
		}
		// pushCtypeValue(os, val);
		os->pushUserPointer(CtypeId<ttype>::getInstanceId(), val, UserDataDestructor<ttype>::dtor);
		os->pushStackValue();
		os->getGlobal(CtypeName<ttype>::getName());
		if(!os->isUserdata(CtypeId<ttype>::getId(), -1)){
			os->pop(2);
		}else{
			os->setPrototype(CtypeId<ttype>::getInstanceId());
		}
	}
};

#define OS_DECL_USER_CLASS(type) \
	OS_DECL_CTYPE(type); \
	template <> struct CtypeValue<type*>: public CtypeUserClass<type*>{}; \
	template <> struct UserObjectDestructor<type>{ static void dtor(type * p){ delete p; } };

// =====================================================================

#define OS_GET_TEMPLATE_SELF(argType) \
	argType self = CtypeValue< typename RemoveConst<argType>::type >::getArg(os, -params-1); \
	if(!self){ \
		os->setException(ObjectScript::OS::String(os, CtypeName< typename PlainType<argType>::type >::getName())+" 'this' must not be null"); \
		return 0; \
	}

#define OS_GET_SELF(argType) \
	argType self = CtypeValue< RemoveConst<argType>::type >::getArg(os, -params-1); \
	if(!self){ \
		os->setException(ObjectScript::OS::String(os, CtypeName< PlainType<argType>::type >::getName())+" 'this' must not be null"); \
		return 0; \
	}

// =====================================================================

#define OS_GET_TEMPLATE_ARG(num, argType) \
	OS_ASSERT(num > 0); \
	typename CtypeValue< typename RemoveConst<argType>::type >::type arg##num = cur_param_offs < 0 ? CtypeValue< typename RemoveConst<argType>::type >::getArg(os, cur_param_offs) : CtypeValue< typename RemoveConst<argType>::type >::def(os); \
	if(!CtypeValue< typename RemoveConst<argType>::type >::isValid(arg##num)){ \
		os->setException(ObjectScript::OS::String(os, CtypeName< typename PlainType<argType>::type >::getName())+" expected"); \
		return 0; \
	} cur_param_offs++

#define OS_GET_ARG(num, argType) \
	OS_ASSERT(num > 0); \
	CtypeValue< RemoveConst<argType>::type >::type arg##num = cur_param_offs < 0 ? CtypeValue< RemoveConst<argType>::type >::getArg(os, cur_param_offs) : CtypeValue< RemoveConst<argType>::type >::def(os); \
	if(!CtypeValue< RemoveConst<argType>::type >::isValid(arg##num)){ \
		os->setException(ObjectScript::OS::String(os, CtypeName< PlainType<argType>::type >::getName())+" expected"); \
		return 0; \
	} cur_param_offs++

// =====================================================================

// namespace ObjectScript {

template <class T>
void registerUserClass(ObjectScript::OS * os, const ObjectScript::OS::FuncDef * list, const ObjectScript::OS::NumberDef * numbers = NULL, bool instantiable = true)
{
	os->pushGlobals();
	os->pushString(CtypeName<T>::getName());
	os->pushUserdata(CtypeId<T>::getId(), 0, NULL, NULL);
	os->setFuncs(list);
	os->setNumbers(numbers);
	os->pushBool(instantiable);
	os->setProperty(-2, OS_TEXT("__instantiable"), false);
	os->setProperty();
}

template <class T, class Prototype>
void registerUserClass(ObjectScript::OS * os, const ObjectScript::OS::FuncDef * list, const ObjectScript::OS::NumberDef * numbers = NULL, bool instantiable = true)
{
	os->pushGlobals();
	os->pushString(CtypeName<T>::getName());
	os->pushUserdata(CtypeId<T>::getId(), 0, NULL, NULL);
	os->setFuncs(list);
	os->setNumbers(numbers);
	os->pushBool(instantiable);
	os->setProperty(-2, OS_TEXT("__instantiable"), false);
	os->pushStackValue();
	os->getGlobal(CtypeName<Prototype>::getName());
	os->setPrototype(CtypeId<T>::getId());
	os->setProperty();
}

// } // namespace ObjectScript

// =====================================================================
// =====================================================================
// =====================================================================

struct FunctionDataChain
{
	void * ptr;
	int data_size;
	// int data_hash;
	FunctionDataChain * next;

	FunctionDataChain();
	virtual ~FunctionDataChain();

	FunctionDataChain * find();
	void registerFunctionData();
};

template <class F> struct FunctionData: public FunctionDataChain
{
	F f;
	FunctionData(F _f): f(_f)
	{
		ptr = &f;
		data_size = sizeof(f);
		// data_hash = ObjectScript::OS::Utils::keyToHash(ptr, data_size);
	}

	FunctionData(const FunctionData& _f): f(_f.f)
	{
		ptr = &f;
		data_size = _f.data_size;
		// data_hash = _f.data_hash;
	}

	static FunctionData * create(F _f)
	{
		FunctionData<F> f(_f);
		FunctionDataChain * found = f.find();
		if(found){
			FunctionData<F> * r = dynamic_cast<FunctionData<F>*>(found);
			// fix compiler bug!?
			if(r){
				return r;
			}
		}
		FunctionData<F> * ret = new FunctionData<F>(f);
		ret->registerFunctionData();
		return ret;
	}
};

// =====================================================================

#include "os-binder-arg-cc-functions.h"

// =====================================================================

// finalizeAllBinds is called on programm exit
// call it if you use leak system integrated when all OS instances already destroyed
void finalizeAllBinds();

} // namespace ObjectScript

// =====================================================================
// =====================================================================
// =====================================================================

#endif // __OS_BINDER_H__