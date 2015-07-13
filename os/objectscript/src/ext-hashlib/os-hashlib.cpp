#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "os-hashlib.h"
#include "../objectscript.h"
#include "../os-binder.h"

#include "md5/md5.h"
#include "sha/sha.h"

#define DEF_HMAC_SALT "hmacHgas67gaj"
#define DEF_DESC_SALT "desHsy6aqJ7"
extern unsigned char * deshash(unsigned char *dst, const unsigned char *key, const unsigned char *src);

extern "C" unsigned long __crc32(unsigned long crc, const unsigned char * buf, unsigned int len);

namespace ObjectScript {

class HashlibOS: public OS
{
public:

	friend struct Internal;
	struct Internal
	{
		static OS_CHAR * md5(OS_CHAR * r, const OS_CHAR * str, int size)
		{
			MD5Context context;
			unsigned char digest[16];
			MD5Init(&context);
			MD5Update(&context, (md5byte*)str, size);
			MD5Final(&context, digest);
		
			for(int i = 0; i < 16; i++){
				r[i*2+0] = OS_TEXT("0123456789ABCDEF")[(digest[i] >> 4) & 0xf];
				r[i*2+1] = OS_TEXT("0123456789ABCDEF")[(digest[i] >> 0) & 0xf];
			}
			r[32] = 0;
			return r;
		}

		static String md5(OS * os, const String& buf)
		{
			OS_CHAR r[34];
			return String(os, md5(r, buf.toChar(), buf.getDataSize()), 32);
		}
	};

	static int md5(OS * os, int params, int, int, void * user_param)
	{
		if(params > 0){
			String str = os->toString(-params+0);
			os->pushString(Internal::md5(os, str));
			return 1;
		}
		return 0;
	}

	static int des(OS * os, int params, int, int, void * user_param)
	{
		if(params > 0){
			String str = os->toString(-params+0);
			String salt = params >= 2 ? os->toString(-params+1) : String(os, DEF_DESC_SALT);
			unsigned char buf[64];
			deshash(buf, (const unsigned char*)salt.toChar(), (const unsigned char*)str.toChar());
			os->pushString((char*)buf);
			return 1;
		}
		return 0;
	}

	static int hmacFunc(OS * os, int params, int, int, void * user_param)
	{
		if(params > 1){
			int sha_type = os->toInt(-params+0);
			switch(sha_type){
			case SHA1:
			case SHA224:
			case SHA256:
			case SHA384:
			case SHA512:
				break;

			default:
				os->setException("error sha type");
				return 0;
			}
			String str = os->toString(-params+1);
			String salt = params >= 3 ? os->toString(-params+2) : String(os, DEF_HMAC_SALT);

			unsigned char buf[USHAMaxHashSize+1];
			int r = hmac((SHAversion)sha_type, (const unsigned char*)str.toChar(), str.getDataSize(),
							(const unsigned char*)salt.toChar(), salt.getDataSize(), buf);
			OS_ASSERT(r == shaSuccess);
			os->pushString((char*)buf);
			return 1;
		}
		return 0;
	}

	static int sha1(OS * os, int params, int, int, void * user_param)
	{
		if(params > 0){
			String str = os->toString(-params+0);
			unsigned char digest[USHAMaxHashSize+1];
			SHA1Context ctx;
			int r = SHA1Reset(&ctx) ||
					SHA1Input(&ctx, (const uint8_t*)str.toChar(), str.getDataSize()) ||
					SHA1Result(&ctx, digest);
			OS_ASSERT(r == 0);
			os->pushString((char*)digest);
			return 1;
		}
		return 0;
	}

	static int sha224(OS * os, int params, int, int, void * user_param)
	{
		if(params > 0){
			String str = os->toString(-params+0);
			unsigned char digest[USHAMaxHashSize+1];
			SHA224Context ctx;
			int r = SHA224Reset(&ctx) ||
					SHA224Input(&ctx, (const uint8_t*)str.toChar(), str.getDataSize()) ||
					SHA224Result(&ctx, digest);
			OS_ASSERT(r == 0);
			os->pushString((char*)digest);
			return 1;
		}
		return 0;
	}

	static int sha256(OS * os, int params, int, int, void * user_param)
	{
		if(params > 0){
			String str = os->toString(-params+0);
			unsigned char digest[USHAMaxHashSize+1];
			SHA256Context ctx;
			int r = SHA256Reset(&ctx) ||
					SHA256Input(&ctx, (const uint8_t*)str.toChar(), str.getDataSize()) ||
					SHA256Result(&ctx, digest);
			OS_ASSERT(r == 0);
			os->pushString((char*)digest);
			return 1;
		}
		return 0;
	}

	static int sha384(OS * os, int params, int, int, void * user_param)
	{
		if(params > 0){
			String str = os->toString(-params+0);
			unsigned char digest[USHAMaxHashSize+1];
			SHA384Context ctx;
			int r = SHA384Reset(&ctx) ||
					SHA384Input(&ctx, (const uint8_t*)str.toChar(), str.getDataSize()) ||
					SHA384Result(&ctx, digest);
			OS_ASSERT(r == 0);
			os->pushString((char*)digest);
			return 1;
		}
		return 0;
	}

	static int sha512(OS * os, int params, int, int, void * user_param)
	{
		if(params > 0){
			String str = os->toString(-params+0);
			unsigned char digest[USHAMaxHashSize+1];
			SHA512Context ctx;
			int r = SHA512Reset(&ctx) ||
					SHA512Input(&ctx, (const uint8_t*)str.toChar(), str.getDataSize()) ||
					SHA512Result(&ctx, digest);
			OS_ASSERT(r == 0);
			os->pushString((char*)digest);
			return 1;
		}
		return 0;
	}

	static int usha(OS * os, int params, int, int, void * user_param)
	{
		if(params > 1){
			int sha_type = os->toInt(-params+0);
			switch(sha_type){
			case SHA1:
			case SHA224:
			case SHA256:
			case SHA384:
			case SHA512:
				break;

			default:
				os->setException("error sha type");
				return 0;
			}
			String str = os->toString(-params+1);
			unsigned char digest[USHAMaxHashSize+1];
			USHAContext ctx;
			int r = USHAReset(&ctx, (SHAversion)sha_type) ||
					USHAInput(&ctx, (const uint8_t*)str.toChar(), str.getDataSize()) ||
					USHAResult(&ctx, digest);
			OS_ASSERT(r == 0);
			os->pushString((char*)digest);
			return 1;
		}
		return 0;
	}

	static int crc32(OS * os, int params, int, int, void * user_param)
	{
		if(params > 0){
			String str = os->toString(-params+0);
			unsigned long crc = __crc32(0L, NULL, 0);
			crc = __crc32(crc, (const unsigned char*)str.toChar(), str.getDataSize());
			os->pushNumber(crc);
			return 1;
		}
		return 0;
	}
};

void initHashExtension(OS * os)
{
	OS::FuncDef funcs[] = {
		{OS_TEXT("md5"), &HashlibOS::md5},
		{OS_TEXT("des"), &HashlibOS::des},
		{OS_TEXT("hmac"), &HashlibOS::hmacFunc},
		{OS_TEXT("usha"), &HashlibOS::usha},
		{OS_TEXT("sha1"), &HashlibOS::sha1},
		{OS_TEXT("sha224"), &HashlibOS::sha224},
		{OS_TEXT("sha256"), &HashlibOS::sha256},
		{OS_TEXT("sha384"), &HashlibOS::sha384},
		{OS_TEXT("sha512"), &HashlibOS::sha512},
		{OS_TEXT("crc32"), &HashlibOS::crc32},
		{}
	};
	OS::NumberDef numbers[] = {
		{OS_TEXT("SHA1"), SHA1},
		{OS_TEXT("SHA224"), SHA224},
		{OS_TEXT("SHA256"), SHA256},
		{OS_TEXT("SHA384"), SHA384},
		{OS_TEXT("SHA512"), SHA512},
		{}
	};
	os->getModule("hashlib");
	os->setFuncs(funcs);
	os->setNumbers(numbers);
	os->pop();
}

} // namespace ObjectScript

