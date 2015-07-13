#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "os-filesystem.h"
#include "../objectscript.h"
#include "../os-binder.h"

#include <sys/stat.h>

#ifdef _MSC_VER
#include <windows.h>
#include <direct.h>
#include <io.h>
#else
#include <dirent.h>
#include <unistd.h>
#endif

namespace ObjectScript {

class FileSystemOS: public OS
{
public:

	static int renameFunc(OS * os, int params, int, int, void*)
	{
#ifndef IW_SDK
		if(params >= 2){
			os->pushBool(::rename(os->toString(-params+0), os->toString(-params+1)) == 0);
			return 1;
		}
		return 0;
#else
		os->setException(OS_TEXT("this function is disabled for this platform"));
		return 0;
#endif
	}
		
	static int truncateFunc(OS * os, int params, int, int, void*)
	{
		os->setException(OS_TEXT("this function is not implemented yet"));
		return 0;
	}
		
	static int chownFunc(OS * os, int params, int, int, void*)
	{
		os->setException(OS_TEXT("this function is not implemented yet"));
		return 0;
	}
		
	static int chmodFunc(OS * os, int params, int, int, void*)
	{
#ifndef IW_SDK
#ifdef _MSC_VER
#define OS_CHMOD ::_chmod
#else
#define OS_CHMOD ::chmod
#endif
		if(params >= 2){
			int mode = os->toInt(-params+1);
			os->pushBool(OS_CHMOD(os->toString(-params), mode) == 0);
			return 1;
		}
		return 0;
#else
		os->setException(OS_TEXT("this function is disabled for this platform"));
		return 0;
#endif
	}

	static void pushDateTime(OS * os, const time_t& time)
	{
		os->getGlobal(OS_TEXT("DateTime"));
		os->pushGlobals();
		os->newObject();
		os->pushNumber(time);
		os->setProperty(-2, OS_TEXT("ticks"));
		os->callFT(1, 1);
	}
				
	static int statFunc(OS * os, int params, int, int, void*)
	{
		if(params < 1){
			return 0;
		}
		struct stat st;
		if(stat(os->toString(-params+0), &st) != 0){
			return 0;
		}

		os->newObject();

		os->pushNumber(st.st_dev);
		os->setProperty(-2, OS_TEXT("dev"));

        os->pushNumber(st.st_ino);
		os->setProperty(-2, OS_TEXT("ino"));

        os->pushNumber(st.st_mode);
		os->setProperty(-2, OS_TEXT("mode"));

        os->pushNumber(st.st_nlink);
		os->setProperty(-2, OS_TEXT("nlink"));

        os->pushNumber(st.st_uid);
		os->setProperty(-2, OS_TEXT("uid"));

        os->pushNumber(st.st_gid);
		os->setProperty(-2, OS_TEXT("gid"));

        os->pushNumber(st.st_rdev);
		os->setProperty(-2, OS_TEXT("rdev"));

        os->pushNumber(st.st_size);
		os->setProperty(-2, OS_TEXT("size"));

		pushDateTime(os, st.st_atime);
		os->setProperty(-2, OS_TEXT("atime"));

		pushDateTime(os, st.st_mtime);
		os->setProperty(-2, OS_TEXT("mtime"));

		pushDateTime(os, st.st_ctime);
		os->setProperty(-2, OS_TEXT("ctime"));

#ifdef _MSC_VER
		bool is_dir = (st.st_mode & S_IFDIR) != 0;
		bool is_file = (st.st_mode & S_IFREG) != 0;
#else
		bool is_dir = S_ISDIR(st.st_mode);
		bool is_file = S_ISREG(st.st_mode);
#endif
		os->pushBool(is_dir);
		os->setProperty(-2, OS_TEXT("isDirectory"));

		os->pushBool(is_file);
		os->setProperty(-2, OS_TEXT("isFile"));

		return 1;
	}
		
	static int unlinkFunc(OS * os, int params, int, int, void*)
	{
#ifndef IW_SDK
#ifdef _MSC_VER
#define OS_UNLINK ::_unlink
#else
#define OS_UNLINK ::unlink
#endif
		if(params >= 1){
			os->pushBool(OS_UNLINK(os->toString(-params)) == 0);
			return 1;
		}
		return 0;
#else
		os->setException(OS_TEXT("this function is disabled for this platform"));
		return 0;
#endif
	}
		
	static int mkdirFunc(OS * os, int params, int, int, void*)
	{
#ifndef IW_SDK
		if(params >= 1){
#ifdef _MSC_VER
			os->pushBool(::_mkdir(os->toString(-params)) == 0);
#else
			int mode = params >= 2 ? os->toInt(-params+1) : 0755;
			os->pushBool(::mkdir(os->toString(-params), mode) == 0);
#endif
			return 1;
		}
		os->setException(OS_TEXT("argument required"));
		return 0;
#else
		os->setException(OS_TEXT("this function is disabled for platform"));
		return 0;
#endif
	}
		
	static int rmdirFunc(OS * os, int params, int, int, void*)
	{
#ifndef IW_SDK
#ifdef _MSC_VER
#define OS_RMDIR ::_rmdir
#else
#define OS_RMDIR ::rmdir
#endif
		if(params >= 1){
			os->pushBool(OS_RMDIR(os->toString(-params)) == 0);
			return 1;
		}
		return 0;
#else
		os->setException(OS_TEXT("this function is disabled for platform"));
		return 0;
#endif
	}
		
	static int readdirFunc(OS * os, int params, int, int, void*)
	{
		if(params < 0){
			return 0;
		}

		OS::String dirname = os->toString(-params+0);

#ifdef _MSC_VER
		WIN32_FIND_DATAA find_data;
		HANDLE h = FindFirstFileA(dirname + OS_TEXT("\\*"), &find_data);
		if(h == INVALID_HANDLE_VALUE){
			return 0;
		}
		os->newArray();
		for(int i = 0;;){
			if(FindNextFileA(h, &find_data) == 0){
                /* if (GetLastError() != ERROR_SUCCESS && GetLastError() != ERROR_NO_MORE_FILES){
					os->setException(OS::String::format(os, OS_TEXT("internal error: %d"), (int)GetLastError()));
					break;
                } */
				break;
			}
			if(OS_STRCMP(find_data.cFileName, ".") != 0 && OS_STRCMP(find_data.cFileName, "..") != 0){
				os->pushStackValue();
				os->pushNumber(i++);
				os->pushString(find_data.cFileName);
				os->setProperty();
			}
		}
		FindClose(h);
		return 1;
#else
		DIR * d = opendir(dirname);
		if(!d){
			return 0;
		}
		os->newArray();
		for(int i = 0;;){
			struct dirent * e = readdir(d);
			if(!e){
				break;
			}
			if(OS_STRCMP(e->d_name, ".") != 0 && OS_STRCMP(e->d_name, "..") != 0){
				os->pushStackValue();
				os->pushNumber(i++);
				os->pushString(e->d_name);
				os->setProperty();
			}
		}
		closedir(d);
		return 1;
#endif
	}
		
	static int existsFunc(OS * os, int params, int, int, void*)
	{
		if(params > 0){
			os->pushBool(os->isFileExist(os->toString(-params+0)));
			return 1;
		}
		return 0;
	}

};

void initFileSystemExtension(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("rename"), &FileSystemOS::renameFunc},
		{OS_TEXT("truncate"), &FileSystemOS::truncateFunc},
		{OS_TEXT("chown"), &FileSystemOS::chownFunc},
		{OS_TEXT("chmod"), &FileSystemOS::chmodFunc},
		{OS_TEXT("mkdir"), &FileSystemOS::mkdirFunc},
		{OS_TEXT("rmdir"), &FileSystemOS::rmdirFunc},
		{OS_TEXT("stat"), &FileSystemOS::statFunc},
		{OS_TEXT("unlink"), &FileSystemOS::unlinkFunc},
		{OS_TEXT("readdir"), &FileSystemOS::readdirFunc},
		{OS_TEXT("exists"), &FileSystemOS::existsFunc},
		{}
	};
	os->getModule("fs");
	os->setFuncs(funcs);
	os->pop();
}

} // namespace ObjectScript

