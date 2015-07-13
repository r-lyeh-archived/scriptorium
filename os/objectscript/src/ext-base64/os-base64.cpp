#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "os-base64.h"
#include "../objectscript.h"
#include "../os-binder.h"

#include "cencode.h"
#include "cdecode.h"

namespace ObjectScript {

class Base64OS: public OS
{
public:

	static int encode(OS * os, int params, int, int, void * user_param)
	{
		if(params > 0){
			String str = os->toString(-params+0);
			int size = str.getDataSize();
			
			Core::Buffer buf(os);
			buf.reserveCapacity(size*2 + 7);
			
			base64_encodestate state;
			base64_init_encodestate(&state);
			int r1 = base64_encode_block(str.toChar(), size, (char*)buf.buffer.buf, &state);
			int r2 = base64_encode_blockend((char*)buf.buffer.buf + r1, &state);
			base64_init_encodestate(&state);
			buf.buffer.count = r1 + r2;    

			os->pushString(buf.toString());
			return 1;
		}
		return 0;
	}

	static int decode(OS * os, int params, int, int, void * user_param)
	{
		if(params > 0){
			String str = os->toString(-params+0);
			int size = str.getDataSize();
			
			Core::Buffer buf(os);
			buf.reserveCapacity(size + 7);
			
			base64_decodestate state;
			base64_init_decodestate(&state);
			int r1 = base64_decode_block(str.toChar(), size, (char*)buf.buffer.buf, &state);
			buf.buffer.count = r1;    

			os->pushString(buf.toString());
			return 1;
		}
		return 0;
	}
};

void initBase64Extension(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("encode"), &Base64OS::encode},
		{OS_TEXT("decode"), &Base64OS::decode},
		{}
	};
	os->getModule("base64");
	os->setFuncs(funcs);
	os->pop();
}

} // namespace ObjectScript

