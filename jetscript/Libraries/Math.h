#ifndef _JET_MATH_LIBRARY_HEADER
#define _JET_MATH_LIBRARY_HEADER

#include "../JetContext.h"

void RegisterMathLibrary(Jet::JetContext* context)
{
	auto lib = context->NewObject();
	lib["Sin"] = [](Jet::JetContext* context, Jet::Value* arg, int args)
	{
		if (args < 1)
			throw Jet::RuntimeException("Too few arguments to Sin!");
		return Jet::Value(sin((double)*arg));
	};
	lib["Cos"] = [](Jet::JetContext* context, Jet::Value* arg, int args)
	{
		if (args < 2)
			throw Jet::RuntimeException("Too few arguments to Cos!");
		return Jet::Value(cos((double)*arg));
	};
	lib["Tan"] = [](Jet::JetContext* context, Jet::Value* arg, int args)
	{
		if (args < 1)
			throw Jet::RuntimeException("Too few arguments to Tan!");
		return Jet::Value(tan((double)*arg));
	};

	lib["Asin"] = [](Jet::JetContext* context, Jet::Value* arg, int args)
	{
		if (args < 1)
			throw Jet::RuntimeException("Too few arguments to Asin!");
		return Jet::Value(asin((double)*arg));
	};
	lib["Acos"] = [](Jet::JetContext* context, Jet::Value* arg, int args)
	{
		if (args < 1)
			throw Jet::RuntimeException("Too few arguments to Acos!");
		return Jet::Value(acos((double)*arg));
	};
	lib["Atan"] = [](Jet::JetContext* context, Jet::Value* arg, int args)
	{
		if (args < 1)
			throw Jet::RuntimeException("Too few arguments to Atan!");
		return Jet::Value(atan((double)*arg));
	};
	lib["Atan2"] = [](Jet::JetContext* context, Jet::Value* arg, int args)
	{
		if (args < 2)
			throw Jet::RuntimeException("Too few arguments to Atan2!");
		return Jet::Value(atan2((double)*arg, (double)arg[1]));
	};

	lib["Fmod"] = [](Jet::JetContext* context, Jet::Value* arg, int args)
	{
		if (args < 2)
			throw Jet::RuntimeException("Too few arguments to Fmod!");
		return Jet::Value(fmod((double)*arg, (double)arg[1]));
	};

	lib["Pow"] = [](Jet::JetContext* context, Jet::Value* arg, int args)
	{
		if (args < 2)
			throw Jet::RuntimeException("Too few arguments to Pow!");
		return Jet::Value(pow((double)*arg, (double)arg[1]));
	};
	lib["Sqrt"] = [](Jet::JetContext* context, Jet::Value* arg, int args)
	{
		if (args < 1)
			throw Jet::RuntimeException("Too few arguments to Sqrt!");
		return Jet::Value(sqrt((double)*arg));
	};

	lib["Log"] = [](Jet::JetContext* context, Jet::Value* arg, int args)
	{
		if (args < 1)
			throw Jet::RuntimeException("Too few arguments to Log!");
		return Jet::Value(log10((double)*arg));
	};
	lib["Ln"] = [](Jet::JetContext* context, Jet::Value* arg, int args)
	{
		if (args < 1)
			throw Jet::RuntimeException("Too few arguments to Ln!");
		return Jet::Value(log((double)*arg));
	};
	
	lib["Floor"] = [](Jet::JetContext* context, Jet::Value* arg, int args)
	{
		if (args < 1)
			throw Jet::RuntimeException("Too few arguments to Floor!");
		return Jet::Value(floor((double)*arg));
	};
	lib["Ceil"] = [](Jet::JetContext* context, Jet::Value* arg, int args)
	{
		if (args < 1)
			throw Jet::RuntimeException("Too few arguments to Ceil!");
		return Jet::Value(ceil((double)*arg));
	};
	context->AddLibrary("Math", lib);
}

#endif
