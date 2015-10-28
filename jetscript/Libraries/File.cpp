#include "File.h"
#include "../JetContext.h"

using namespace Jet;

JetObject* filePrototype = 0;
void RegisterFileLibrary(JetContext* context)
{
	Value lib = context->NewObject();
	lib["Open"] = [](JetContext* context, Value* v, int args)
	{
		if (args != 2)
			throw RuntimeException("Invalid number of arguments to File.Open!");

		FILE* f = fopen(v[0].ToString().c_str(), v[1].ToString().c_str());
		return context->NewUserdata(f, filePrototype);
	};
	/*lib["Close"] = Value([](JetContext* context, Value* v, int args)
	{
		if (args != 1)
			throw RuntimeException("Invalid number of arguments to File.Close!");
		
		fclose(v->GetUserdata<FILE>());
		return Value();
	});*/
	context->AddLibrary("File", lib);

	filePrototype = context->NewPrototype("file")._object;//new JetObject(context);
	filePrototype->SetPrototype(0);
	(*filePrototype)["Read"] = Value([](JetContext* context, Value* v, int args)
	{
		if (args != 2)
			throw RuntimeException("Invalid number of arguments to read!");

		if (v[1].GetUserdata<FILE>() == 0)
			throw RuntimeException("Tried to read from a closed file!");

		char* out = new char[((int)v->value)+1];//context->GCAllocate((v)->value);
		fread(out, 1, (int)v[1], v[0].GetUserdata<FILE>());
		out[(int)v[1]] = 0;
		return context->NewString(out, false);
	});
	(*filePrototype)["Write"] = Value([](JetContext* context, Value* v, int args)
	{
		if (args != 2)
			throw RuntimeException("Invalid number of arguments to write!");
		
		if (v[0].GetUserdata<FILE>() == 0)
			throw RuntimeException("Tried to write to a closed file!");

		std::string str = v[1].ToString();
		fwrite(str.c_str(), 1, str.length(), v[0].GetUserdata<FILE>());
		return Value();
	});

	(*filePrototype)["Close"] = [](JetContext* context, Value* v, int args)
	{
		if (v->type != ValueType::Userdata || args != 1)
			throw RuntimeException("Invalid args to file:Close()");

		fclose(v->GetUserdata<FILE>());
		v->_userdata->data = 0;
		return Value();
	};
	
	//this function is called when the value is garbage collected
	(*filePrototype)["_gc"] = Value([](JetContext* context, Value* v, int args)
	{
		fclose(v->GetUserdata<FILE>());
		return Value();
	});
}