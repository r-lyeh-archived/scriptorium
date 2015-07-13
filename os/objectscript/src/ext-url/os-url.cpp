#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "os-url.h"
#include "../objectscript.h"
#include "../os-binder.h"

namespace ObjectScript {

class UrlOS: public OS
{
public:

	static int decodeHexChar(OS_CHAR c)
	{
		if(c >= '0' && c <= '9') return      c - '0';
		if(c >= 'A' && c <= 'F') return 10 + c - 'A';
		if(c >= 'a' && c <= 'f') return 10 + c - 'a';
		return 0;
	}

	static OS_CHAR decodeHexCode(const OS_CHAR * s)
	{
		// OS_ASSERT(s[0] && s[1]);
		int c = decodeHexChar(s[0]) * 16 + decodeHexChar(s[1]);
		return (OS_CHAR)c;
	}

	static int encode(OS * os, int params, int, int, void*)
	{
		if(params >= 1){
			String str = os->toString(-params+0);
			const OS_CHAR * s = str;
			const OS_CHAR * end = s + str.getLen();
			
			Core::Buffer buf(os);
			for(; s < end; s++){
				if( (*s >= OS_TEXT('0') && *s <= OS_TEXT('9'))
					|| (*s >= OS_TEXT('A') && *s <= OS_TEXT('Z'))
					|| (*s >= OS_TEXT('a') && *s <= OS_TEXT('z')) 
					|| *s == OS_TEXT('-') || *s == OS_TEXT('_')
					|| *s == OS_TEXT('.') || *s == OS_TEXT('~')
					// || *s == OS_TEXT('!') || *s == OS_TEXT('*')
					// || *s == OS_TEXT('\'')|| *s == OS_TEXT('(') || *s == OS_TEXT(')')
					)
				{
					buf.append(*s);
				}else if(*s == OS_TEXT(' ')){
					buf.append(OS_TEXT('+'));
				}else{
					buf.append(OS_TEXT('%'));
					buf.append(OS_TEXT("0123456789ABCDEF")[((OS_BYTE)(*s) >> 4) & 0xf]);
					buf.append(OS_TEXT("0123456789ABCDEF")[((OS_BYTE)(*s) >> 0) & 0xf]);
				}
			}
			os->pushString(buf);
			return 1;
		}
		return 0;
	}

	static int decode(OS * os, int params, int, int, void*)
	{
		if(params >= 1){
			String str = os->toString(-params+0);
			const OS_CHAR * s = str;
			const OS_CHAR * end = s + str.getLen();
			
			Core::Buffer buf(os);
			for(; s < end;){
				if(*s == OS_TEXT('%')){
					if(s+3 <= end){
						buf.append(decodeHexCode(s+1));
					}
					s += 3;
				}else if(*s == OS_TEXT('+')){
					buf.append(OS_TEXT(' '));
					s++;
				}else{
					buf.append(*s++);
				}
			}
			os->pushString(buf);
			return 1;
		}
		return 0;
	}
};

void initUrlExtension(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("encode"), &UrlOS::encode},
		{OS_TEXT("decode"), &UrlOS::decode},
		{}
	};
	os->getModule("url");
	os->setFuncs(funcs);
	os->pop();
}

} // namespace ObjectScript

