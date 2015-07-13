#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#include "win/os-fcgi/stdafx.h"
#include <Windows.h>
#pragma comment (lib, "Ws2_32.lib")
#endif

#include "objectscript.h"
#include "3rdparty/fcgi-2.4.1/include/fcgi_stdio.h"
#include "3rdparty/MPFDParser-1.0/Parser.h"
#include <stdlib.h>

#define OS_FCGI_VERSION	OS_TEXT("1.3.2")

#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>

#ifndef _MSC_VER
#include <pthread.h>
#endif

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

#ifdef _MSC_VER
#ifndef IW_SDK
#include <direct.h>
#endif // IW_SDK
#else // _MSC_VER
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#endif // _MSC_VER

#define PID_FILE "/var/run/os-fcgi.pid"

using namespace ObjectScript;

char init_cache_path[128] = 
#ifdef _MSC_VER
	"cache-osc"
#else
	"/tmp"
#endif
;
int listen_socket = 0;
int post_max_size = 0;

#include <cstdio>

time_t start_time = 0;

void initStartTime()
{
	char touch_filename[256];
	strcpy(touch_filename, init_cache_path);
#ifdef _MSC_VER
	_mkdir(touch_filename);
#else
	mkdir(touch_filename, 0755);
#endif
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

void dolog(const char * format, ...)
{
	va_list va;
	va_start(va, format);
	FILE * f = fopen("/tmp/os-fcgi.log", "at");
	if(f){
		vfprintf(f, format, va);
		// fwrite(msg, strlen(msg), 1, f);
		fclose(f);
	}
	va_end(va);
}

class FCGX_OS: public OS
{
protected:

	FCGX_Request * request;
	// int shutdown_funcs_id;
	bool headers_sent;
	Core::String * cache_path;

	virtual ~FCGX_OS()
	{
	}

	virtual bool init(MemoryManager * mem)
	{
		if(OS::init(mem)){
			setGCStartWhenUsedBytes(32 * 1024 * 1024);
			cache_path = new (malloc(sizeof(Core::String) OS_DBG_FILEPOS)) Core::String(this, init_cache_path);

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

	FCGX_OS()
	{
		request = NULL;
		headers_sent = false;
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
		setSetting(OS_SETTING_CREATE_COMPILED_FILE, true);

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

	void appendBuffer(const void * buf, int size)
	{
		FCGX_PutStr((char*)buf, size, request->out);
	}

	void appendBuffer(const OS_CHAR * str)
	{
		appendBuffer((const char*)str, (int)OS_STRLEN(str) * sizeof(OS_CHAR));
	}

	void echo(const void * buf, int size)
	{
		if(!headers_sent){
			headers_sent = true;
			appendBuffer("Content-type: text/html; charset=utf-8\r\n\r\n");
		}
		appendBuffer(buf, size);
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
		struct stat sourcecode_st, compiled_st;
		stat(sourcecode_filename, &sourcecode_st);
		stat(compiled_filename, &compiled_st);
		if(sourcecode_st.st_mtime >= compiled_st.st_mtime || compiled_st.st_mtime < start_time){
			return COMPILE_SOURCECODE_FILE;
		}
		return LOAD_COMPILED_FILE;
	}

	static int notifyHeadersSent(OS * p_os, int params, int, int, void*)
	{
		FCGX_OS * os = (FCGX_OS*)p_os;
		os->headers_sent = true;
		return 0;
	}

	static int getFCGIVersion(OS * p_os, int params, int, int, void*)
	{
		p_os->pushString(OS_FCGI_VERSION);
		return 1;
	}

	/*
	static int registerShutdownFunction(OS * p_os, int params, int, int, void*)
	{
		if(params > 0){
			FCGX_OS * os = dynamic_cast<FCGX_OS*>(p_os);
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
			{"notifyHeadersSent", FCGX_OS::notifyHeadersSent},
			{"__get@OS_FCGI_VERSION", FCGX_OS::getFCGIVersion},
			{}
		};
		pushGlobals();
		setFuncs(funcs);
		pop();
	}

	void processRequest(FCGX_Request * p_request)
	{
		request = p_request;
 
		initGlobalFunctions();

		initEnv("_SERVER", request->envp);

		newObject();
		setGlobal("_POST");
		
		newObject();
		setGlobal("_GET");
		
		newObject();
		setGlobal("_FILES");
		
		newObject();
		setGlobal("_COOKIE");

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

		getGlobal("_SERVER");
		getProperty("CONTENT_LENGTH");
		int content_length = popInt();

		// int post_max_size = 1024*1024*8;
		if(content_length > post_max_size){
			FCGX_FPrintF(request->out, "POST Content-Length of %d bytes exceeds the limit of %d bytes", content_length, post_max_size);
			return;
		}

		getGlobal("_SERVER");
		getProperty("CONTENT_TYPE");
		String content_type = popString();

		const char * multipart_form_data = "multipart/form-data;";
		int multipart_form_data_len = (int)strlen(multipart_form_data);

		const char * form_urlencoded = "application/x-www-form-urlencoded";
		int form_urlencoded_len = (int)strlen(form_urlencoded);

		bool is_valid_headers = true;

		MPFD::Parser POSTParser = MPFD::Parser();
		if(content_length > 0 && content_type.getLen() > 0 && strncmp(content_type.toChar(), multipart_form_data, multipart_form_data_len) == 0){
			char * temp_buf = NULL;
			try{
				// dolog("begin multipart_form_data");
				POSTParser.SetTempDirForFileUpload("/tmp");
				// POSTParser.SetMaxCollectedDataLength(20*1024);
				POSTParser.SetContentType(content_type.toChar());

				int max_temp_buf_size = (int)(1024*1024*0.1);
				int temp_buf_size = content_length < max_temp_buf_size ? content_length : max_temp_buf_size;
				temp_buf = (char*)malloc(temp_buf_size + 1 OS_DBG_FILEPOS); // new char[temp_buf_size + 1];
				for(int cur_len; (cur_len = FCGX_GetStr(temp_buf, temp_buf_size, request->in)) > 0;){
					POSTParser.AcceptSomeData(temp_buf, cur_len);
				}
				free(temp_buf); // delete [] temp_buf;
				temp_buf = NULL;
			
				// POSTParser.SetExternalDataBuffer(buf, len);
				POSTParser.FinishData();
			}catch(MPFD::Exception& e){
				is_valid_headers = false;
				free(temp_buf);
#if defined _MSC_VER && 1
				fprintf(stderr, "error post data: %s\n", e.GetError().c_str());
#endif
			}
			if(is_valid_headers){
				std::map<std::string, MPFD::Field *> fields = POSTParser.GetFieldsMap();
				// FCGX_FPrintF(request->out, "Have %d fields<p>\n", fields.size());

				std::map<std::string, MPFD::Field *>::iterator it;
				for(it = fields.begin(); it != fields.end(); it++){
					MPFD::Field * field = fields[it->first];
					if(field->GetType() == MPFD::Field::TextType){
						getGlobal("_POST");
						pushString(field->GetTextTypeContent().c_str());
						setSmartProperty(it->first.c_str());
					}else{
						getGlobal("_FILES");
						newObject();
						{
							pushStackValue();
							pushString(field->GetFileName().c_str());
							setProperty("name");
						
							pushStackValue();
							pushString(field->GetFileMimeType().c_str());
							setProperty("type");
						
							pushStackValue();
							pushString(field->GetTempFileNameEx().c_str());
							setProperty("temp");
						
							pushStackValue();
							pushNumber(getFileSize(field->GetTempFileNameEx().c_str()));
							setProperty("size");
						}
						setSmartProperty(it->first.c_str());
					}
				}
				// dolog("end multipart_form_data");
			}
		}else if(content_length > 0 && strncmp(content_type.toChar(), form_urlencoded, form_urlencoded_len) == 0){
			// dolog("begin form_urlencoded");
			Core::Buffer buf(this);
			buf.reserveCapacity(content_length+4);
			for(int cur_len; (cur_len = FCGX_GetStr((char*)buf.buffer.buf, content_length, request->in)) > 0;){
				buf.buffer.count = cur_len;
				OS_ASSERT(content_length == cur_len);
				int temp; (void)temp;
				OS_ASSERT(FCGX_GetStr((char*)&temp, sizeof(temp), request->in) == 0);
				break;
			}
			buf.buffer.buf[buf.buffer.count] = '\0';
			char * form = (char*)buf.buffer.buf;
			for(; form ;){
				char * assign = strchr(form, '=');
				if(assign){
					getGlobal("url");
					getProperty(-1, "decode");
					OS_ASSERT(isFunction());
					pushString(form, assign - form);
					callTF(1, 1);
					String name = popString();

					getGlobal("url");
					getProperty(-1, "decode");
					OS_ASSERT(isFunction());
					char * value_str = assign+1;
					char * end_str = strchr(value_str, '&');
					if(end_str){
						pushString(value_str, end_str - value_str);
						form = end_str+1;
					}else{
						pushString(value_str);
						form = NULL;
					}
					callTF(1, 1);
					String value = popString();
					
					getGlobal("_POST");
					pushString(value);
					setSmartProperty(name);
				}else{
					break;
				}
			}
			// dolog("end form_urlencoded");
		}
		
		extern char **environ;
		initEnv("_ENV", environ);
		
		getGlobal("_SERVER");
		getProperty("SCRIPT_FILENAME");
		if(isNull()){
			pop();
			getGlobal("_SERVER");
			
			getProperty(-1, "DOCUMENT_ROOT");
			String document_root = popString("");
			
			getProperty(-1, "SCRIPT_NAME");
			String script_name = popString("");

			pushString(document_root + script_name);
			setProperty("SCRIPT_FILENAME");

			getGlobal("_SERVER");
			getProperty("SCRIPT_FILENAME");
			OS_ASSERT(isString());
		}
		String script_filename = popString("");
#if defined _MSC_VER && 0
		fprintf(stderr, "%s\n", script_filename.toChar());
#endif
		do{
			static const char * not_found = "Content-type: text/html; charset=utf-8\r\n"
				"Status: 404 Not Found\r\n"
				"\r\n"
				"<html><head><title>404 Not Found</title></head><body bgcolor=\"white\">"
				"<center><h1>404 Not Found %s</h1></center><hr><center>"
					"ObjectScript " OS_VERSION "<br />"
					// OS_COPYRIGHT "<br />"
					OS_OPENSOURCE
				"</center></body></html>";

			static const char * just_ready = "Content-type: text/html; charset=utf-8\r\n"
				"\r\n"
				"<html><head><title>Server is just ready to use ObjectScript</title></head><body bgcolor=\"white\">"
				"<center><h1>Server is just ready to use ObjectScript</h1></center><hr><center>"
					"ObjectScript " OS_VERSION "<br />"
					// OS_COPYRIGHT "<br />"
					OS_OPENSOURCE
				"</center></body></html>";

			if(script_filename.isEmpty() || !is_valid_headers){
				if(!headers_sent){
					headers_sent = true;
					FCGX_PutS(just_ready, request->out);
				}else
					FCGX_PutS("Server is just ready to use ObjectScript", request->out);
				break;
			}
			if(getFilename(script_filename).isEmpty()){
				static const char * ext[] = {
					OS_EXT_TEMPLATE,
					OS_EXT_SOURCECODE,
					OS_EXT_TEMPLATE_HTML,
					OS_EXT_TEMPLATE_HTM,
					NULL
				};
				bool found = false;
				for(int i = 0; ext[i]; i++){
					String new_script_filename = script_filename + OS_TEXT("index") + ext[i];
					if(isFileExist(new_script_filename)){
						script_filename = new_script_filename;
						found = true;
						break;
					}
				}
				if(!found){
					if(!headers_sent){
						headers_sent = true;
						FCGX_PutS(just_ready, request->out);
					}else
						FCGX_PutS("Server is just ready to use ObjectScript", request->out);
					break;
				}
			}
			String ext = getFilenameExt(script_filename);
			if(ext == OS_EXT_SOURCECODE || ext == OS_EXT_TEMPLATE || ext == OS_EXT_TEMPLATE_HTML || ext == OS_EXT_TEMPLATE_HTM){
				require(script_filename, true);
				triggerShutdownFunctions();
				if(!headers_sent){
					headers_sent = true;
					FCGX_PutS(just_ready, request->out);
				}
			}else{
				// print requested file, it's not recommended, only ObjectScript scripts are recommended
				FileHandle * f = openFile(script_filename, "rb");
				if(f){
					if(!headers_sent){
						headers_sent = true;
						FCGX_PutS("Content-type: ", request->out);
						FCGX_PutS(getContentType(ext), request->out);
						FCGX_PutS("\r\n\r\n", request->out);
					}
					const int BUF_SIZE = 1024*256;
					int size = getFileSize(f);
					void * buf = malloc(BUF_SIZE < size ? BUF_SIZE : size OS_DBG_FILEPOS);
					for(int i = 0; i < size; i += BUF_SIZE){
						int len = BUF_SIZE < size - i ? BUF_SIZE : size - i;
						readFile(buf, len, f);
						FCGX_PutStr((const char*)buf, len, request->out);
					}
					free(buf);				
					closeFile(f);
				}else{
					if(!headers_sent){
						headers_sent = true;
						FCGX_FPrintF(request->out, not_found, getFilename(script_filename).toChar());
					}else{
						FCGX_FPrintF(request->out, "404 Not Found %s", getFilename(script_filename).toChar());
					}
				}
			}
		}while(false);

		triggerShutdownFunctions();
		
		FCGX_Finish_r(request);

		triggerCleanupFunctions();
	}

	const OS_CHAR * getContentType(const OS_CHAR * ext)
	{
		if(ext[0] == OS_TEXT('.')){
			ext++;
		}
		static const OS_CHAR * mime_types[][2] = {
			{OS_EXT_SOURCECODE, "text/os"},
			{OS_EXT_TEMPLATE, "text/osh"},
			{"html", "text/html"},
			{"htm", "text/html"},
			{"js", "text/javascript"},
			{"css", "text/css"},
			{"png", "image/png"},
			{"jpeg", "image/jpeg"},
			{"jpg", "image/jpeg"},
			{"gif", "image/gif"},
			{"ico", "image/x-icon"},
			{"txt", "text/plain"},
			{"log", "text/plain"},
			{}
		};
		for(int i = 0;; i++){
			const OS_CHAR ** mime = mime_types[i];
			if(!mime[0]){
				break;
			}
			if(OS_STRCMP(ext, mime[0]) == 0){
				return mime[1];
			}
		}
		return "application/octet-stream";
	}
};

void * doit(void * a)
{
    // int listen_socket = (int)(ptrdiff_t)a;

    FCGX_Request * request = new FCGX_Request();
    if(FCGX_InitRequest(request, listen_socket, 0)){
		printf("error init request \n");
		exit(1);
	}

#ifndef _MSC_VER
	static pthread_mutex_t accept_mutex = PTHREAD_MUTEX_INITIALIZER;
#endif

    for(;;){
#ifndef _MSC_VER
		pthread_mutex_lock(&accept_mutex);
#endif		
		int rc = FCGX_Accept_r(request);
#ifndef _MSC_VER
		pthread_mutex_unlock(&accept_mutex);
#endif
		if(rc){
			// TODO: log error
			printf("Error accept code: %d\n", rc);
			exit(1);
		}

		/*
			TODO: need to fork request but FCGX_Detach & FCGX_Attach are not fully implemented

		FCGX_Detach(request);
		fork();
		FCGX_Attach(request);
		*/

		FCGX_OS * os = OS::create(new FCGX_OS());
		os->processRequest(request);
        os->release();

		// FCGX_Finish_r(request);
    }
	// we are not here
	delete request;
}

#ifndef _MSC_VER
void signalHandler(int sig)
{
	unlink(PID_FILE);
	exit(EXIT_SUCCESS);
}

void setPidFile(const char * filename)
{
	FILE * f = fopen(filename, "w+");
	if (f) {
		fprintf(f, "%u", getpid());
		fclose(f);
	}
}

void demonize()
{
	pid_t pid = fork();
	if (pid < 0) {
		printf("Error: Start Daemon failed (%s)\n", strerror(errno));
		exit(EXIT_FAILURE);
	}
	if (pid > 0)
		exit(EXIT_SUCCESS);

	umask(0);

	if (setsid() < 0)
		exit(EXIT_FAILURE);

	if ((chdir("/")) < 0)
		exit(EXIT_FAILURE);

	close(STDIN_FILENO);
	close(STDOUT_FILENO);
	close(STDERR_FILENO);

	struct sigaction sa;
	sa.sa_handler = signalHandler;
	sigaction(SIGINT, &sa, 0);
	sigaction(SIGQUIT, &sa, 0);
	sigaction(SIGTERM, &sa, 0);

	setPidFile(PID_FILE);
}
#endif

#define OS_FCGI_PROC "os-fcgi"

void usage(const char * error = NULL)
{
	if(error){
		printf("%s\n", error);
	}
	printf("\n");
	printf("Usage: %s [args...]\n", OS_FCGI_PROC);
	printf(" -c <file> Config file name\n");
	printf(" -h        This help\n");
	printf("\n");
	exit(1);
}

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
	initStartTime();

	printf("ObjectScript FastCGI Process Manager %s\n", OS_FCGI_VERSION);
	printf("%s\n", OS_COPYRIGHT);
	printf("%s\n", OS_OPENSOURCE);

	if(FCGX_Init()){
		usage("Error: FCGX initialization is failed\n");
	}

	int threads;
	{
		OS * os = OS::create();

		os->setSetting(OS_SETTING_CREATE_TEXT_EVAL_OPCODES, false);
		os->setSetting(OS_SETTING_CREATE_TEXT_OPCODES, false);
		os->setSetting(OS_SETTING_CREATE_COMPILED_FILE, false);
		os->setSetting(OS_SETTING_SOURCECODE_MUST_EXIST, true);

#ifdef _MSC_VER
		const char * config_flename = "conf\\etc\\os-fcgi\\conf.os";
		if(!os->isFileExist(config_flename)){
			config_flename = "..\\..\\conf\\etc\\os-fcgi\\conf.os";
			// os->isFileExist(config_flename);
		}
#else
		const char * config_flename = "/etc/os-fcgi/conf.os";
#endif
		for(int i = 1; i < argc; i++){
			if(strcmp(argv[i], "-c") == 0){
				if(i+1 < argc){
					config_flename = argv[++i];
				}else{
					usage("Error: no argument for option c\n");
				}
				continue;
			}
			if(strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "-?") == 0){
				usage();
				continue;
			}
		}
		
		printf("config: %s\n", config_flename);
		os->require(config_flename, false, 1, OS_SOURCECODE_AUTO, true, false);
		if(os->isExceptionSet()){
			printf("\nError in config file: %s\n", config_flename);
			os->handleException();
			usage();
		}
#ifdef _MSC_VER
#define DEF_NUM_THREADS 1
#else
#define DEF_NUM_THREADS 8
#endif
		threads			  =	(os->getProperty(-1, "threads"),		os->popInt(DEF_NUM_THREADS));
		OS::String listen = (os->getProperty(-1, "listen"),			os->popString(":9000"));
		post_max_size	  =	(os->getProperty(-1, "post_max_size"),	os->popInt(1024*1024*8));
		os->release();

		int listen_queue_backlog = 400;
		listen_socket = FCGX_OpenSocket(listen, listen_queue_backlog);
		if(listen_socket < 0){
			printf("Error: listen address is incorrect %s\n", listen.toChar());
			usage();
		}
// #ifdef _MSC_VER
		printf("listen: %s\n", listen.toChar());
// #endif
	}

#ifndef _MSC_VER
	const int MAX_THREAD_COUNT = 64;
	if(threads < 1){
		threads = 1;
		printf("threads number should be in range 1 .. %d, use threads: %d\n", MAX_THREAD_COUNT, threads);
	}else if(threads > MAX_THREAD_COUNT){ 
		threads = MAX_THREAD_COUNT;
		printf("threads number should be in range 1 .. %d, use threads: %d\n", MAX_THREAD_COUNT, threads);
	}else{
		printf("threads: %d\n", threads);
	}
	printf("post_max_size: %.1f Mb\n", (float)post_max_size / (1024.0f * 1024.0f));
	demonize();
	
	pthread_t id[MAX_THREAD_COUNT];
	for(int i = 1; i < threads; i++){
        pthread_create(&id[i], NULL, doit, NULL);
	}
#else
	if(threads != 1){
		threads = 1;
		printf("threads: %d (only one thread is supported for windows at the moment)\n", threads);
	}else{
		printf("threads: %d\n", threads);
	}
	printf("post_max_size: %.1f Mb\n", (float)post_max_size / (1024.0f * 1024.0f));
#endif
	doit(NULL);

	return 0;
}

