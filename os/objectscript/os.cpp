#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#include "win/os-fcgi/stdafx.h"
#include <Windows.h>
#pragma comment (lib, "Ws2_32.lib")
#endif

#include "objectscript.h"

#include <stdlib.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>

#ifndef _MSC_VER
#include <pthread.h>
#endif

#ifdef OS_EMSCRIPTEN

#include <emscripten.h>
#include "ext-url/os-url.h"
#include "ext-base64/os-base64.h"
#include "ext-json/os-json.h"
#include "ext-datetime/os-datetime.h"

#else // #ifdef OS_EMSCRIPTEN

#include "ext-process/os-process.h"
#include "ext-filesystem/os-filesystem.h"
#include "ext-hashlib/os-hashlib.h"
#include "ext-url/os-url.h"
#include "ext-base64/os-base64.h"
#include "ext-datetime/os-datetime.h"
#include "ext-json/os-json.h"

#ifndef OS_CURL_DISABLED
#include "ext-curl/os-curl.h"
#endif

#ifndef OS_SQLITE3_DISABLED
#include "ext-sqlite3/os-sqlite3.h"
#endif

#ifndef OS_ICONV_DISABLED
#include "ext-iconv/os-iconv.h"
#endif

#ifndef OS_REGEXP_DISABLED
#include "ext-regexp/os-regexp.h"
#endif

#ifndef OS_ODBO_DISABLED
#include "ext-odbo/os-odbo.h"
#endif

#ifndef OS_ZLIB_DISABLED
#include "ext-zlib/os-zlib.h"
#endif

#endif // #if !defined OS_EMSCRIPTEN

#ifdef _MSC_VER
#ifndef IW_SDK
#include <direct.h>
#endif // IW_SDK
#else // _MSC_VER
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#endif // _MSC_VER

#include <cstdio>

using namespace ObjectScript;

/* check that argument has no extra characters at the end */
#define noextrachars(x)		{if ((x)[2] != '\0') return -1;}

/* indices of various argument indicators in array args */
#define has_i		0	/* -i */
#define has_v		1	/* -v */
#define has_e		2	/* -e */
#define has_E		3	/* -E */
#define has_cache	4	/* -cache */
#define has_debug	5	/* -debug */

#define NUM_HAS		6	/* number of 'has_*' */

#ifndef OS_PROMPT
#define OS_PROMPT	"> "
#define OS_PROMPT2	">> "
#endif

#ifndef OS_MAXINPUT
#define OS_MAXINPUT	512
#endif

#define EOFMARK		"<eof>"
#define marklen		(sizeof(EOFMARK)/sizeof(char) - 1)

char init_cache_path[128] = 
#ifdef _MSC_VER
	"cache-osc"
#else
	"/tmp"
#endif
;

bool use_cache = false;
time_t start_time = 0;

void createCacheDir()
{
#ifdef _MSC_VER
	_mkdir(init_cache_path);
#else
	mkdir(init_cache_path, 0755);
#endif
}

void initStartTime()
{
	createCacheDir();

	char touch_filename[256];
	strcpy(touch_filename, init_cache_path);
	strcat(touch_filename, "/os-cache-touch");
	std::remove(touch_filename);

	FILE * f = fopen(touch_filename, "wb");
	OS_ASSERT(f);
	if(f){
		fclose(f);
		struct stat filename_st;
		stat(touch_filename, &filename_st);
		start_time = filename_st.st_mtime;
	}
}

class ConsoleOS: public OS
{
protected:

	// int shutdown_funcs_id;
	bool header_sent;
	Core::String * cache_path;

	virtual ~ConsoleOS()
	{
	}

	virtual bool init(MemoryManager * mem)
	{
		if(OS::init(mem)){
			// setGCStartUsedBytes(32 * 1024 * 1024);
			cache_path = new (malloc(sizeof(Core::String) OS_DBG_FILEPOS)) Core::String(this, init_cache_path);

#ifdef OS_EMSCRIPTEN

			initEmscriptenExtension();

			initUrlExtension(this);
			initBase64Extension(this);
			initDateTimeExtension(this);
			initJsonExtension(this);

#else // #ifdef OS_EMSCRIPTEN

			initProcessExtension(this);
			initFileSystemExtension(this);
			initHashExtension(this);
			initUrlExtension(this);
			initBase64Extension(this);
			initDateTimeExtension(this);
			initJsonExtension(this);

#ifndef OS_CURL_DISABLED
			initCurlExtension(this);
#endif

#ifndef OS_SQLITE3_DISABLED
			initSqlite3Extension(this);
#endif

#ifndef OS_ICONV_DISABLED
			initIconvExtension(this);
#endif

#ifndef OS_REGEXP_DISABLED
			initRegexpExtension(this);
#endif

#ifndef OS_ODBO_DISABLED
			initODBOExtension(this);
#endif

#ifndef OS_ZLIB_DISABLED
			initZlibExtension(this);
#endif

#endif // #ifndef OS_EMSCRIPTEN
			return true;
		}
		return false;
	}

	virtual void shutdown()
	{
		deleteObj(cache_path);
		OS::shutdown();
	}

public:

	ConsoleOS()
	{
		header_sent = false;
	}

	void initSettings()
	{
		OS::initSettings();

#if defined _MSC_VER && defined OS_DEBUG
		setSetting(OS_SETTING_CREATE_TEXT_EVAL_OPCODES, false);
		setSetting(OS_SETTING_CREATE_TEXT_OPCODES, true);
#else
		setSetting(OS_SETTING_CREATE_TEXT_EVAL_OPCODES, false);
		setSetting(OS_SETTING_CREATE_TEXT_OPCODES, false);
#endif
		setSetting(OS_SETTING_CREATE_DEBUG_INFO, true);
		setSetting(OS_SETTING_CREATE_COMPILED_FILE, false);

		setSetting(OS_SETTING_SOURCECODE_MUST_EXIST, true);
	}

	void initEnv(const char * var_name, char ** envp)
	{
		newObject();
		for(; *envp; envp++){
			const char * value = *envp;
			const char * split = strchr(value, '=');
			OS_ASSERT(split);
			if(split){
				pushStackValue(-1);
				pushString(value, (int)(split - value));
				pushString(split + 1);
				setProperty();
			}
		}
		setGlobal(var_name);
	}

	String md5(const String& buf)
	{
		getGlobal(OS_TEXT("hashlib"));
		getProperty(-1, OS_TEXT("md5"));
		OS_ASSERT(isFunction());
		pushString(buf);
		callTF(1, 1);
		OS_ASSERT(isString());
		return popString();
	}

	String getCompiledFilename(const String& resolved_filename)
	{
#if 1
		String ext = getFilenameExt(resolved_filename);
		if(ext == OS_EXT_COMPILED){
			return resolved_filename;
		}
#else
		String path = getFilenamePath(resolved_filename);
		if(path == *cache_path){
			return resolved_filename;
		}
#endif	
		Core::Buffer buf(this);
		buf.append(*cache_path);
		buf.append(OS_TEXT("/os-cache-"));
		buf.append(md5(resolved_filename));
		buf.append(OS_EXT_COMPILED);
		// buf.append(changeFilenameExt(md5(resolved_filename), OS_EXT_COMPILED));
		return buf.toStringOS(); 
	}

	String getTextOpcodesFilename(const String& resolved_filename)
	{
		return changeFilenameExt(getCompiledFilename(resolved_filename), OS_EXT_TEXT_OPCODES);
	}

	OS_EFileUseType checkFileUsage(const String& sourcecode_filename, const String& compiled_filename)
	{
		if(!use_cache){
			return COMPILE_SOURCECODE_FILE;
		}
		struct stat sourcecode_st, compiled_st;
		stat(sourcecode_filename, &sourcecode_st);
		stat(compiled_filename, &compiled_st);
		if(sourcecode_st.st_mtime >= compiled_st.st_mtime || compiled_st.st_mtime < start_time){
			return COMPILE_SOURCECODE_FILE;
		}
		return LOAD_COMPILED_FILE;
	}

	/*
	static int registerShutdownFunction(OS * p_os, int params, int, int, void*)
	{
		if(params > 0){
			ConsoleOS * os = dynamic_cast<ConsoleOS*>(p_os);
			int offs = os->getAbsoluteOffs(-params);
			os->pushValueById(os->shutdown_funcs_id);
			for(int i = params-1; i >= 0; i--){
				os->pushStackValue();
				os->pushStackValue(offs+i);
				os->pushStackValue();
				os->setProperty();
			}
		}
		return 0;
	}
	*/

	void triggerShutdownFunctions()
	{
		resetTerminated();
		getGlobal("triggerShutdownFunctions");
		OS_ASSERT(isFunction() || isNull());
		pushGlobals();
		callFT();
	}

	void triggerCleanupFunctions()
	{
		resetTerminated();
		getGlobal("triggerCleanupFunctions");
		OS_ASSERT(isFunction() || isNull());
		pushGlobals();
		callFT();
	}

	void initGlobalFunctions()
	{
		FuncDef funcs[] = {
			// {"triggerHeaderSent", FCGX_OS::triggerHeaderSent},
			{}
		};
		pushGlobals();
		setFuncs(funcs);
		pop();
	}

#ifdef OS_EMSCRIPTEN

	static int runJSStringResult(OS * os, int params, int, int, void*)
	{
		if(params > 0){
			char * ret = emscripten_run_script_string(os->toString(-params+0).toChar());
			os->pushString(ret);
			return 1;
		}
		return 0;
	}

	static int runJSIntResult(OS * os, int params, int, int, void*)
	{
		if(params > 0){
			int ret = emscripten_run_script_int(os->toString(-params+0).toChar());
			os->pushNumber(ret);
			return 1;
		}
		return 0;
	}

	void initEmscriptenExtension()
	{
		FuncDef funcs[] = {
			{"runJSIntResult", ConsoleOS::runJSIntResult},
			{"runJSStringResult", ConsoleOS::runJSStringResult},
			{}
		};
		pushGlobals();
		setFuncs(funcs);
		pop();
	}

#endif // #ifdef OS_EMSCRIPTEN

	void printUsage(char ** argv, const char *badoption)
	{
		const char * progname = argv[0] && argv[0][0] ? argv[0] : "unknown progname";
		if(badoption){
			printf("%s: ", progname);
			if(badoption[1] == 'e' || badoption[1] == 'l')
				printf("'%s' needs argument\n", badoption);
			else
				printf("unrecognized option '%s'\n", badoption);
		}else{
			printVersion();
		}
		printf(
			"Usage: %s [options] [script [args]]\n"
			"available options are:\n"
			"  -e stat  execute string 'stat'\n"
			"  -i       enter interactive mode after executing 'script'\n"
			"  -l name  require library 'name'\n"
			"  -v       show version information\n"
			"  -E       ignore environment variables\n"
			"  -cache   use cache of compiled files\n"
			"  -debug   create debug human readable text files\n"
			"  --       stop handling options\n"
			"  -        stop handling options and execute stdin\n"
			"examples:\n"
#ifdef _MSC_VER
			"%s -e 'print(\\\"2*5 - 1 = \\\"..(2*5 - 1))'"
#else
			"%s -e 'print(\"2*5 - 1 = \"..(2*5 - 1))'"
#endif
			"\n"
			,
			progname, progname);
	}

	void printVersion()
	{
		printf("%s\n", OS_COPYRIGHT);
		printf("%s\n", OS_OPENSOURCE);
	}

	OS::String parseQString(int& i, int argc, char **argv)
	{
		Core::Buffer buf(this);
		if(i < argc){
			char c = argv[i][0] == '\'' ? '\'' : argv[i][0] == '"' ? '"' : argv[i][0] == '`' ? '`' : '\0';
			if(!c){
				buf.append(argv[i]);
			}else{
				int len = OS_STRLEN(argv[i]+1);
				if(!len){
					printUsage(argv, argv[i]);
					exit(1);
				}
				if(argv[i][1+len-1] == c){
					buf.append(argv[i]+1, len-1);
				}else{
					buf.append(argv[i]+1, len);
					for(i++; i < argc; i++){
						len = OS_STRLEN(argv[i]);
						if(!len){
							printUsage(argv, argv[i]);
							exit(1);
						}
						buf.append(" ");
						if(argv[i][len-1] == c){
							buf.append(argv[i], len-1);
							break;
						}
						buf.append(argv[i], len);
					}
				}
			}
		}
		return buf.toStringOS();
	}

	int collectArgs(int argc, char **argv, int *args)
	{
		for(int i = 1; i < argc && argv[i] != NULL; i++){
			if (argv[i][0] != '-'){ /* not an option? */
				return i;
			}
			if(strcmp(argv[i]+1, "cache") == 0){
				args[has_cache] = 1;
				continue;
			}
			if(strcmp(argv[i]+1, "debug") == 0){
				args[has_debug] = 1;
				continue;
			}
			switch (argv[i][1]) {  /* option */
			case '-':
				noextrachars(argv[i]);
				return (argv[i+1] != NULL ? i+1 : 0);
			case '\0':
				return i;
			case 'E':
				args[has_E] = 1;
				break;
			case 'i':
				noextrachars(argv[i]);
				args[has_i] = 1;  /* go through */
			case 'v':
				noextrachars(argv[i]);
				args[has_v] = 1;
				break;
			case 'e':
				args[has_e] = 1;
				if (argv[i][2] == '\0') {  /* no concatenated argument? */
					i++;  /* try next 'argv' */
					parseQString(i, argc, argv);
					if (i == argc || argv[i] == NULL || argv[i][0] == '-')
						return -(i - 1);  /* no next argument or it is another option */
				}
				break;
			case 'l':  /* both options need an argument */
				if (argv[i][2] == '\0') {  /* no concatenated argument? */
					i++;  /* try next 'argv' */
					parseQString(i, argc, argv);
					if (i == argc || argv[i] == NULL || argv[i][0] == '-')
						return -(i - 1);  /* no next argument or it is another option */
				}
				break;
			default:  
				/* invalid option; return its index... */
				return -i;  /* ...as a negative value */
			}
		}
		return 0;
	}

	bool runCommand(const OS::String& command)
	{
		eval(command, 0, 0, OS_SOURCECODE_PLAIN, false, false);
		if(isExceptionSet()){
			handleException();
			return false;
		}
		return true;
	}

	bool requireLibrary(const OS::String& filename)
	{
		require(filename, true, 0, OS_SOURCECODE_AUTO, true, false);
		if(isExceptionSet()){
			handleException();
			return false;
		}
		return true;
	}

	int runArgs(int n, char **argv)
	{
		for(int i = 1; i < n; i++){
			OS_ASSERT(argv[i][0] == '-');
			switch(argv[i][1]){  /* option */
			case 'e': 
				{
					i++;
					String str = parseQString(i, n, argv);
					if(!runCommand(str))
						return 0;
					break;
				}
			case 'l': 
				{
					i++;
					String filename = parseQString(i, n, argv);
					if(!requireLibrary(filename))
						return 0;  /* stop if file fails */
					break;
				}
			default: 
				break;
			}
		}
		return 1;
	}

	bool inComplete()
	{
		OS::String str = toString(-1);
		int lmsg = str.getLen();
		const char *msg = str;
		if (lmsg >= marklen && strcmp(msg + lmsg - marklen, EOFMARK) == 0) {
			pop();
			return true;
		}
		return false;
	}

	bool readLine(char * buf, const char * promt)
	{
		fputs(promt, stdout); fflush(stdout); /* show prompt */
		return fgets(buf, OS_MAXINPUT, stdin) != NULL; /* get line */
	}

	/* void saveLine(const char * str)
	{
	}

	void freeLine(char * buf)
	{
	} */

	const char * getPrompt(bool firstline)
	{
		return firstline ? OS_PROMPT : OS_PROMPT2;
	}

	bool pushLine(bool firstline = false)
	{
		char buf[OS_MAXINPUT];
		if(!readLine(buf, getPrompt(firstline))){
			return false;  /* no input */
		}
		int len = OS_STRLEN(buf);
		for(; len > 0 && OS_IS_SPACE(buf[len-1]); len--){
			buf[len-1] = '\0';
		}
		if(firstline && buf[0] == '='){  /* first line starts with `=' ? */
			OS::String str = OS::String(this, buf+1);
			if(str.isEmpty()){
				return false;
			}
			pushString(OS::String(this, "return ") + str);  /* change it to `return' */
		}else{
			OS::String str = OS::String(this, buf);
			if(str.isEmpty()){
				return false;
			}
			pushString(str);
		}
		// freeLine(b);
		return true;
	}

	bool compileLine()
	{
		if(!pushLine(true)){
			return false;  /* no input */
		}
		for (;;) {  /* repeat until gets a complete line */
			if(compile(toString(), OS_SOURCECODE_PLAIN, false)){
				remove(-2);
				return true;
			}
			pop(); // pop null function
			handleException();
			if(!pushLine()){  /* no more input? */
				pop(); // pop prev string
				return false;
			}
			Core::Buffer buf(this);
			buf.append(toString(-2));
			buf.append("\n");
			buf.append(toString(-1));
			pushString(buf);
			remove(-3, 2);
		}
		return false; // shutup compiler
	}

	void dotty()
	{
		bool ok;
		while((ok = compileLine())){
			OS_ASSERT(isFunction());
			int offs = getAbsoluteOffs(-1);
			pushNull();
			int res_count = 10; // getAbsoluteOffs(-1) - offs;
			callFT(0, res_count);
			ok = !isExceptionSet();
			if(!ok){
				handleException();
			}
			if(res_count > 0){
				getGlobal("print");
				pushNull();
				int i = res_count-1;
				for(; i >= 0; i--){
					if(!isNull(offs + i)){
						break;
					}
				}
				int count = i+1;
				if(count > 0){
					for(i = 0; i < count; i++){
						pushStackValue(offs + i);
						// pushString("\n");
					}
					callFT(count * 1, 0);
				}else{
					// pop(2);
					callFT(0, 0);
				}
				pop(res_count);
			}
		}
	}

#ifdef OS_EMSCRIPTEN
	void initEmscripten()
	{
	}
#endif // #ifdef OS_EMSCRIPTEN

	void processRequest(int argc, char * argv[])
	{
		if(argc == 1){
			printUsage(argv, NULL);
			return;
		}
		int args[NUM_HAS];
		OS_MEMSET(&args, 0, sizeof(args));
		int script = collectArgs(argc, argv, args);
		// pushStackValue(OS_REGISTER_USERPOOL);
		if(script < 0){
			printUsage(argv, argv[-script]);
			return;
		}
		if(args[has_E]){  /* option '-E'? */
#if 0
			char * environ[] = {NULL};
			initEnv("_ENV", environ);
#endif
		}else{
			extern char **environ;
			initEnv("_ENV", environ);
		}
		if(args[has_cache]){
			use_cache = true;
			initStartTime();
			setSetting(OS_SETTING_CREATE_COMPILED_FILE, true);
		}
		if(args[has_debug]){
			createCacheDir();
			setSetting(OS_SETTING_CREATE_TEXT_OPCODES, true);
		}
		
		getGlobal("process");
		pushString("argv");
		newArray();
		{
			pushStackValue();
			pushString(argv[0]);
			addProperty();

			script = script > 0 ? script : argc;
			for(int i = script; i < argc; i++){
				pushStackValue();
				pushString(argv[i]);
				addProperty();
			}
		}
		setProperty();
		
		/* newObject();
		shutdown_funcs_id = getValueId();
		retainValueById(shutdown_funcs_id);
		pop(); */
		// addProperty();

		initGlobalFunctions();

		char * server_env[] = {NULL};
		initEnv("_SERVER", server_env);

#ifdef _MSC_VER
		pushBool(true);
		setGlobal("_PLATFORM_WINDOWS");
		
		pushBool(false);
		setGlobal("_PLATFORM_UNIX");
#else
		pushBool(false);
		setGlobal("_PLATFORM_WINDOWS");
		
		pushBool(true);
		setGlobal("_PLATFORM_UNIX");
#endif
		pushString(*cache_path);
		setGlobal("OS_CACHE_PATH");

		if(args[has_v]){
			printVersion();
		}
		/* execute arguments -e and -l */
		if(!runArgs(script, argv)){
			return;
		}
		if(script < argc){
			String script_filename(this, argv[script]);
		
			if(script_filename.isEmpty()){
				echo("filename is not defined\n");
			}else{
				getGlobal("_SERVER");
				pushString("SCRIPT_FILENAME");
				pushString(script_filename);
				setProperty();

				require(script_filename, true);
			}
		}
		if(args[has_i]){ /* -i option? */
			dotty();
		}else if(script == 0 && !args[has_e] && !args[has_v]){  /* no arguments? */
			if(1){ // lua_stdin_is_tty()) {
				printVersion();
				dotty();
			}else{
				// dofile(L, NULL);  /* executes stdin as a file */
			}
		}

		triggerShutdownFunctions();
		triggerCleanupFunctions();
	}
};

void log(const char * msg)
{
	FILE * f = fopen("/tmp/os-fcgi.log", "wt");
	if(f){
		fwrite(msg, strlen(msg), 1, f);
		fclose(f);
	}
}

#ifdef OS_EMSCRIPTEN

extern "C" {

static ConsoleOS * os = NULL;

int main()
{
	emscripten_exit_with_live_runtime();
	return 0;
}

void OS_create()
{
	if(os){
		// TODO: release??
		return;
	}
	os = OS::create(new ConsoleOS());
}

void OS_eval(const char * text)
{
	if(!os){
		OS_create();
	}
	os->eval(text);
}

void OS_evalFakeFile(const char * filename, const char * text)
{
	if(!os){
		OS_create();
	}
	os->evalFakeFile(filename, text);
}

void OS_release()
{
	if(os){
		os->release();
		os = NULL;
	}
}

} // extern "C"

#else // #ifdef OS_EMSCRIPTEN

#if defined _MSC_VER && 0
int _tmain(int argc, _TCHAR* _argv[])
{
	char ** argv = new char*[argc];
	{ 
		for(int i = 0; i < argc; i++){
			int len = 0; for(; _argv[i][len]; len++);
			argv[i] = new char[len+1];
			for(int j = 0; j <= len; j++){
				argv[i][j] = (char)_argv[i][j];
			}
		}
	}
	struct ArgvFinalizer {
		int argc;
		char ** argv;
		~ArgvFinalizer(){
			for(int i = 0; i < argc; i++){
				delete [] argv[i];
			}
			delete [] argv;
		}
	} __argv_finalizer__ = {argc, argv};
#else
int main(int argc, char * argv[])
{
#endif

#if 1
	ConsoleOS * os = OS::create(new ConsoleOS());
#else
	ConsoleOS * os = OS::create(new ConsoleOS(), new OSMemoryManagerOld());
#endif
	// os->eval("print json.decode(json.encode({a=2, 10=\"qwerty\"}))");
	os->processRequest(argc, argv);
    os->release();

	return 0;
}

#endif // #ifndef OS_EMSCRIPTEN