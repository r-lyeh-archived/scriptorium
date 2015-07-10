#include <iostream>  // cout
#include <assert.h>  // assert()
#include <string.h>  // strstr()
#include <vector>
#include <stdio.h>
#include <sstream>   // stringstream
#include <angelscript.h>
#include "../../../add_on/scriptbuilder/scriptbuilder.h"
#include "../../../add_on/scriptstdstring/scriptstdstring.h"
#include "../../../add_on/scriptarray/scriptarray.h"
#include "../../../add_on/scriptdictionary/scriptdictionary.h"
#include "../../../add_on/scriptfile/scriptfile.h"
#include "../../../add_on/scripthelper/scripthelper.h"
#include "../../../add_on/debugger/debugger.h"
#include "../../../add_on/contextmgr/contextmgr.h"

#if defined(_MSC_VER)
#include <crtdbg.h>   // MSVC debugging routines
#endif

using namespace std;

// Function prototypes
int               ConfigureEngine(asIScriptEngine *engine);
void              InitializeDebugger(asIScriptEngine *engine);
int               CompileScript(asIScriptEngine *engine, const char *scriptFile);
int               ExecuteScript(asIScriptEngine *engine, const char *scriptFile, bool debug);
void              MessageCallback(const asSMessageInfo *msg, void *param);
asIScriptContext *RequestContextCallback(asIScriptEngine *engine, void *param);
void              ReturnContextCallback(asIScriptEngine *engine, asIScriptContext *ctx, void *param);
void              PrintString(const string &str);
CScriptArray     *GetCommandLineArgs();

// The command line arguments
CScriptArray *g_commandLineArgs = 0;
int           g_argc = 0;
char        **g_argv = 0;

// The context manager is used to manage the execution of co-routines
CContextMgr *g_ctxMgr = 0;

// The debugger is used to debug the script
CDebugger *g_dbg = 0;

// Context pool
vector<asIScriptContext*> g_ctxPool;

int main(int argc, char **argv)
{
#if defined(_MSC_VER)
	// Tell MSVC to report any memory leaks
	_CrtSetDbgFlag(_CRTDBG_LEAK_CHECK_DF|_CRTDBG_ALLOC_MEM_DF);
	_CrtSetReportMode(_CRT_ASSERT,_CRTDBG_MODE_FILE);
	_CrtSetReportFile(_CRT_ASSERT,_CRTDBG_FILE_STDERR);

	// Use _CrtSetBreakAlloc(n) to find a specific memory leak
#endif

	int r;

	// Validate the command line arguments
	bool argsValid = true;
	if( argc < 2 ) 
		argsValid = false;
	else if( argc == 2 && strcmp(argv[1], "-d") == 0 )
		argsValid = false;

	if( !argsValid )
	{
		cout << "Usage: " << endl;
		cout << "asrun [-d] <script file> [<args>]" << endl;
		cout << " -d             inform if the script should be runned with debug" << endl;
		cout << " <script file>  is the script file that should be runned" << endl;
		cout << " <args>         zero or more args for the script" << endl;
		return -1;
	}

	// Create the script engine
	asIScriptEngine *engine = asCreateScriptEngine(ANGELSCRIPT_VERSION);
	if( engine == 0 )
	{
		cout << "Failed to create script engine." << endl;
		return -1;
	}

	// Configure the script engine with all the functions, 
	// and variables that the script should be able to use.
	r = ConfigureEngine(engine);
	if( r < 0 ) return -1;
	
	// Check if the script is to be debugged
	bool debug = false;
	if( strcmp(argv[1], "-d") == 0 )
		debug = true;

	// Store the command line arguments for the script
	int scriptArg = debug ? 2 : 1;
	g_argc = argc - (scriptArg + 1);
	g_argv = argv + (scriptArg + 1);

	// Compile the script code
	r = CompileScript(engine, argv[scriptArg]);
	if( r < 0 ) return -1;

	// Execute the script
	r = ExecuteScript(engine, argv[scriptArg], debug);
	
	// Release the engine
	if( g_commandLineArgs )
		g_commandLineArgs->Release();
	engine->Release();

	return r;
}

// This message callback is used by the engine to send compiler messages
void MessageCallback(const asSMessageInfo *msg, void *param)
{
	const char *type = "ERR ";
	if( msg->type == asMSGTYPE_WARNING ) 
		type = "WARN";
	else if( msg->type == asMSGTYPE_INFORMATION ) 
		type = "INFO";

	printf("%s (%d, %d) : %s : %s\n", msg->section, msg->row, msg->col, type, msg->message);
}

// This function will register the application interface
int ConfigureEngine(asIScriptEngine *engine)
{
	int r;

	// The script compiler will send any compiler messages to the callback
	r = engine->SetMessageCallback(asFUNCTION(MessageCallback), 0, asCALL_CDECL); assert( r >= 0 );

	// Register the standard add-ons that we'll allow the scripts to use
	RegisterStdString(engine);
	RegisterScriptArray(engine, false);
	RegisterStdStringUtils(engine);
	RegisterScriptDictionary(engine);
	RegisterScriptFile(engine);

	// Register a couple of extra functions for the scripts
	r = engine->RegisterGlobalFunction("void print(const string &in)", asFUNCTION(PrintString), asCALL_CDECL); assert( r >= 0 );
	r = engine->RegisterGlobalFunction("array<string> @getCommandLineArgs()", asFUNCTION(GetCommandLineArgs), asCALL_CDECL); assert( r >= 0 );

	// Setup the context manager and register the support for co-routines
	g_ctxMgr = new CContextMgr();
	g_ctxMgr->RegisterCoRoutineSupport(engine);

	// Tell the engine to use our context pool. This will also 
	// allow us to debug internal script calls made by the engine
	r = engine->SetContextCallbacks(RequestContextCallback, ReturnContextCallback, 0); assert( r >= 0 );

	// TODO: There should be an option of outputting the engine 
	//       configuration for use with the offline compiler asbuild.
	//       It should then be possible to execute pre-compiled bytecode.

	return 0;
}

// This is the to-string callback for the string type
std::string StringToString(void *obj, bool expandMembers, CDebugger *dbg)
{
	// We know the received object is a string
	std::string *val = reinterpret_cast<std::string*>(obj);

	// Format the output string
	// TODO: Should convert non-readable characters to escape sequences
	std::stringstream s;
	s << "(len=" << val->length() << ") \"";
	if( val->length() < 20 )
		s << *val << "\"";
	else
		s << val->substr(0, 20) << "...";

	return s.str();
}

// This is the to-string callback for the array type
// This is generic and will take care of all template instances based on the array template
std::string ArrayToString(void *obj, bool expandMembers, CDebugger *dbg)
{
	CScriptArray *arr = reinterpret_cast<CScriptArray*>(obj);

	std::stringstream s;
	s << "(len=" << arr->GetSize() << ")";
	
	if( expandMembers )
	{
		s << " [";
		for( asUINT n = 0; n < arr->GetSize(); n++ )
		{
			s << dbg->ToString(arr->At(n), arr->GetElementTypeId(), false, arr->GetArrayObjectType()->GetEngine());
			if( n < arr->GetSize()-1 )
				s << ", ";
		}
		s << "]";
	}

	return s.str();
}

// This function initializes the debugger and let's the user set initial break points
void InitializeDebugger(asIScriptEngine *engine)
{
	// Create the debugger instance and store it so the context callback can attach
	// it to the scripts contexts that will be used to execute the scripts
	g_dbg = new CDebugger();

	// Register the to-string callbacks so the user can see the contents of strings
	// TODO: Add a callback for the dictionary type too
	g_dbg->RegisterToStringCallback(engine->GetObjectTypeByName("string"), StringToString);
	g_dbg->RegisterToStringCallback(engine->GetObjectTypeByName("array"), ArrayToString);

	// Allow the user to initialize the debugging before moving on
	cout << "Debugging, waiting for commands. Type 'h' for help." << endl;
	g_dbg->TakeCommands(0);
}

// This is where the script is compiled into bytecode that can be executed
int CompileScript(asIScriptEngine *engine, const char *scriptFile)
{
	int r;

	// We will only initialize the global variables once we're 
	// ready to execute, so disable the automatic initialization
	engine->SetEngineProperty(asEP_INIT_GLOBAL_VARS_AFTER_BUILD, false);

	CScriptBuilder builder;
	r = builder.StartNewModule(engine, "script");
	if( r < 0 ) return -1;

	r = builder.AddSectionFromFile(scriptFile);
	if( r < 0 ) return -1;

	r = builder.BuildModule();
	if( r < 0 )
	{
		engine->WriteMessage(scriptFile, 0, 0, asMSGTYPE_ERROR, "Script failed to build");
		return -1;
	}

	return 0;
}

// Execute the script by calling the main() function
int ExecuteScript(asIScriptEngine *engine, const char *scriptFile, bool debug)
{
	asIScriptModule *mod = engine->GetModule("script", asGM_ONLY_IF_EXISTS);
	if( !mod ) return -1;

	// Find the main function
	asIScriptFunction *func = mod->GetFunctionByDecl("int main()");
	if( func == 0 )
	{
		// Try again with "void main()"
		func = mod->GetFunctionByDecl("void main()");
	}

	if( func == 0 )
	{
		engine->WriteMessage(scriptFile, 0, 0, asMSGTYPE_ERROR, "Cannot find 'int main()' or 'void main()'");
		return -1;
	}

	if( debug )
		InitializeDebugger(engine);

	// Once we have the main function, we first need to initialize the global variables
	// Since we've set up the request context callback we will be able to debug the 
	// initialization without passing in a pre-created context
	int r = mod->ResetGlobalVars(0);
	if( r < 0 )
	{
		engine->WriteMessage(scriptFile, 0, 0, asMSGTYPE_ERROR, "Failed while initializing global variables");
		return -1;
	}

	// Set up a context to execute the script
	// The context manager will request the context from the 
	// pool, which will automatically attach the debugger
	asIScriptContext *ctx = g_ctxMgr->AddContext(engine, func);

	// Execute the script until completion
	// The script may create co-routines. These will automatically
	// be managed by the context manager
	while( g_ctxMgr->ExecuteScripts() );

	// Check if the main script finished normally
	r = ctx->GetState();
	if( r != asEXECUTION_FINISHED )
	{
		if( r == asEXECUTION_EXCEPTION )
		{
			cout << "The script failed with an exception" << endl;
			cout << GetExceptionInfo(ctx, true).c_str();
			r = -1;
		}
		else if( r == asEXECUTION_ABORTED )
		{
			cout << "The script was aborted" << endl;
			r = -1;
		}
		else
		{
			cout << "The script terminated unexpectedly (" << r << ")" << endl;
			r = -1;
		}
	}
	else
	{
		// Get the return value from the script
		if( func->GetReturnTypeId() == asTYPEID_INT32 )
		{
			r = *(int*)ctx->GetAddressOfReturnValue();
		}
		else
			r = 0;
	}

	// Destroy the context manager
	if( g_ctxMgr )
	{
		delete g_ctxMgr;
		g_ctxMgr = 0;
	}

	// Before leaving, allow the engine to clean up remaining objects by 
	// discarding the module and doing a full garbage collection so that 
	// this can also be debugged if desired
	mod->Discard();
	engine->GarbageCollect();

	// Release all contexts that have been allocated
	for( auto ctx : g_ctxPool )
		ctx->Release();
	g_ctxPool.clear();

	// Destroy debugger
	if( g_dbg )
	{
		delete g_dbg;
		g_dbg = 0;
	}

	return r;
}

// This little function allows the script to print a string to the screen
void PrintString(const string &str)
{
	cout << str;
}

// This function returns the command line arguments that were passed to the script
CScriptArray *GetCommandLineArgs()
{
	if( g_commandLineArgs )
	{
		g_commandLineArgs->AddRef();
		return g_commandLineArgs;
	}

	// Obtain a pointer to the engine
	asIScriptContext *ctx = asGetActiveContext();
	asIScriptEngine *engine = ctx->GetEngine();

	// Create the array object
	asIObjectType *arrayType = engine->GetObjectTypeById(engine->GetTypeIdByDecl("array<string>"));
	g_commandLineArgs = CScriptArray::Create(arrayType, (asUINT)0);

	// Find the existence of the delimiter in the input string
	for( int n = 0; n < g_argc; n++ )
	{
		// Add the arg to the array
		g_commandLineArgs->Resize(g_commandLineArgs->GetSize()+1);
		((string*)g_commandLineArgs->At(n))->assign(g_argv[n]);
	}

	// Return the array by handle
	g_commandLineArgs->AddRef();
	return g_commandLineArgs;
}

// This function is called by the engine whenever a context is needed for an 
// execution we use it to pool contexts and to attach the debugger if needed.
asIScriptContext *RequestContextCallback(asIScriptEngine *engine, void * /*param*/)
{
	asIScriptContext *ctx = 0;

	// Check if there is a free context available in the pool
	if( g_ctxPool.size() )
	{
		ctx = g_ctxPool.back();
		g_ctxPool.pop_back();
	}
	else
	{
		// No free context was available so we'll have to create a new one
		ctx = engine->CreateContext();
	}

	// Attach the debugger if needed
	if( ctx && g_dbg )
	{
		// Set the line callback for the debugging
		ctx->SetLineCallback(asMETHOD(CDebugger, LineCallback), g_dbg, asCALL_THISCALL);
	}

	return ctx;
}

// This function is called by the engine when the context is no longer in use
void ReturnContextCallback(asIScriptEngine *engine, asIScriptContext *ctx, void * /*param*/)
{
	// Place the context into the pool for when it will be needed again
	g_ctxPool.push_back(ctx);

	// We can also check for possible script exceptions here if so desired

	// Unprepare the context to free any objects it may still hold (e.g. return value)
	ctx->Unprepare();
}

