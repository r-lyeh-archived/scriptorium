#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "os-process.h"
#include "../objectscript.h"
#include "../os-binder.h"

#ifdef _MSC_VER
#include <direct.h>
#define OS_GETCWD ::_getcwd
#define OS_CHDIR ::_chdir
#define OS_POPEN ::_popen
#define OS_PCLOSE ::_pclose
#else
#include <unistd.h>
#define OS_GETCWD ::getcwd
#define OS_CHDIR ::chdir
#define OS_POPEN ::popen
#define OS_PCLOSE ::pclose
#endif

namespace ObjectScript {

class ProcessOS: public OS
{
public:

	static int cwd(OS * os, int params, int, int, void*)
	{
#ifndef IW_SDK
        const int OS_PATH_MAX = 1024;
		Core::Buffer buf(os);
        buf.reserveCapacity((OS_PATH_MAX+1) * sizeof(OS_CHAR));
        OS_GETCWD((OS_CHAR*)buf.buffer.buf, OS_PATH_MAX);
        os->pushString(buf);
		return 1;
#else
		os->setException(OS_TEXT("this function is disabled for platform"));
		return 0;
#endif
	}
		
	static int chdir(OS * os, int params, int, int, void*)
	{
#ifndef IW_SDK
		if(params >= 1){
			os->pushBool(OS_CHDIR(os->toString(-params).toChar()) == 0);
			return 1;
		}
		os->setException(OS_TEXT("argument required"));
		return 0;
#else
		os->setException(OS_TEXT("this function is disabled for platform"));
		return 0;
#endif
	}
		
	static int exitFunc(OS * os, int params, int, int, void*)
	{
#if 1		// TODO: script should use 'terminate' function instead of exit if possible
		::exit(params >= 1 ? os->toInt(-params) : 0);
#else
		os->setException(OS_TEXT("this function is disabled due to security reason"));
#endif
		return 0;
	}
		
	static int getgid(OS * os, int params, int, int, void*)
	{
#if 0
		// TODO: implement under linux
#else
		os->setException(OS_TEXT("this function is not implemented yet"));
		return 0;
#endif
	}
		
	static int setgid(OS * os, int params, int, int, void*)
	{
#if 0
		// TODO: implement under linux
#else
		os->setException(OS_TEXT("this function is not implemented yet"));
		return 0;
#endif
	}
		
	static int getuid(OS * os, int params, int, int, void*)
	{
#if 0
		// TODO: implement under linux
#else
		os->setException(OS_TEXT("this function is not implemented yet"));
		return 0;
#endif
	}
		
	static int setuid(OS * os, int params, int, int, void*)
	{
#if 0
		// TODO: implement under linux
#else
		os->setException(OS_TEXT("this function is not implemented yet"));
		return 0;
#endif
	}
		
	static int getPID(OS * os, int params, int, int, void*)
	{
#if 0
		// TODO: implement under linux
#else
		os->setException(OS_TEXT("this function is not implemented yet"));
		return 0;
#endif
	}
		
	static int kill(OS * os, int params, int, int, void*)
	{
#if 0
		// TODO: implement under linux
#else
		os->setException(OS_TEXT("this function is not implemented yet"));
		return 0;
#endif
	}
		
	static int umask(OS * os, int params, int, int, void*)
	{
#if 0
		// TODO: implement under linux
#else
		os->setException(OS_TEXT("this function is not implemented yet"));
		return 0;
#endif
	}

	static int exec(OS * os, int params, int, int need_ret_values, void*)
	{
		if(params > 0){
			OS::String cmd = os->toString(-params+0);
			FILE * pipe = OS_POPEN(cmd.toChar(), "r");
			if(!pipe){
				return 0;
			}
			Core::Buffer buf(os);

			Core::Buffer temp(os);
			const int MAX_TEMP_SIZE = 1024*10;
			temp.reserveCapacity(MAX_TEMP_SIZE);
			
			while(!feof(pipe)){
				int len = fread(temp.buffer.buf, 1, MAX_TEMP_SIZE, pipe);
				if(len > 0){
					if(need_ret_values > 0){
						buf.append((void*)temp.buffer.buf, len);
					}
				}else{
					// TODO: throw exception?
					break;
				}
			}
			OS_PCLOSE(pipe);
			os->pushString(buf);
			return 1;
		}
		return 0;
	}
};

void initProcessExtension(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("cwd"), ProcessOS::cwd},
		{OS_TEXT("chdir"), ProcessOS::chdir},
		{OS_TEXT("exit"), ProcessOS::exitFunc},
		{OS_TEXT("getgid"), ProcessOS::getgid},
		{OS_TEXT("setgid"), ProcessOS::setgid},
		{OS_TEXT("getuid"), ProcessOS::getuid},
		{OS_TEXT("setuid"), ProcessOS::setuid},
		{OS_TEXT("__get@PID"), ProcessOS::getPID},
		{OS_TEXT("kill"), ProcessOS::kill},
		{OS_TEXT("umask"), ProcessOS::umask},
		{OS_TEXT("exec"), ProcessOS::exec},
		{}
	};

	os->getModule(OS_TEXT("process"));
	os->setFuncs(funcs);

	os->pushStackValue();
	os->pushString(OS_TEXT("argv"));
	os->newArray();
	os->setProperty(); // process.argv should be populated later

	os->pop();
}

} // namespace ObjectScript

