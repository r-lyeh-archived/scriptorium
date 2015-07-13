#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "os-zlib.h"
#include "../objectscript.h"
#include "../os-binder.h"

#include "zlib/zlib.h"

#define ZLIB_ENCODING_RAW		-0xf
#define ZLIB_ENCODING_GZIP		0x1f
#define ZLIB_ENCODING_DEFLATE	0x0f
#define ZLIB_ENCODING_ANY		0x2f

#define ZLIB_BUFFER_SIZE_GUESS(in_len) (((size_t) ((double) in_len * (double) 1.015)) + 10 + 8 + 4 + 1)

namespace ObjectScript {

class ZlibOS: public OS
{
public:

	static void triggerError(OS * os, const OS::String& msg)
	{
		os->getGlobal(OS_TEXT("Exception"));
		os->pushGlobals();
		os->pushString(msg);
		os->callFT(1, 1);
		os->setException();
	}

	static void triggerError(OS * os, const char * msg)
	{
		os->getGlobal(OS_TEXT("Exception"));
		os->pushGlobals();
		os->pushString(msg);
		os->callFT(1, 1);
		os->setException();
	}

	static void initExtension(OS * os);

	/* proto binary encode(binary data, int encoding[, int level = -1])
	   Compress data with the specified encoding */
	static int encodeInternal(OS * os, int params, int encoding)
	{
		if(params < 1){
			return 0;
		}
		switch(encoding){
		case ZLIB_ENCODING_RAW:
		case ZLIB_ENCODING_GZIP:
		case ZLIB_ENCODING_DEFLATE:
			break;

		default:
			triggerError(os, "encoding mode must be either ZLIB_ENCODING_RAW, ZLIB_ENCODING_GZIP or ZLIB_ENCODING_DEFLATE");
			return 0;
		}
		OS::String str = os->toString(-params + 0);
		long level = params >= 2 ? os->toInt(-params + 1) : -1;
		if(level < -1 || level > 9){
			triggerError(os, OS::String::format(os, "compression level (%ld) must be within -1..9", level));
			return 0;
		}
		
		int status;
		z_stream Z;

		memset(&Z, 0, sizeof(z_stream));
		// Z.zalloc = php_zlib_alloc;
		// Z.zfree = php_zlib_free;

		if (Z_OK == (status = deflateInit2(&Z, level, Z_DEFLATED, encoding, MAX_MEM_LEVEL, Z_DEFAULT_STRATEGY))) {
			OS::Core::Buffer out(os);
			const char * in_buf = str.toChar();
			int in_len = str.getLen();

			out.reserveCapacity(ZLIB_BUFFER_SIZE_GUESS(in_len));

			Z.next_in = (Bytef *) in_buf;
			Z.next_out = (Bytef *) out.buffer.buf;
			Z.avail_in = in_len;
			Z.avail_out = out.buffer.capacity;

			status = deflate(&Z, Z_FINISH);
			deflateEnd(&Z);

			if (Z_STREAM_END == status) {
				/* size buffer down to actual length */
				OS_ASSERT((int)Z.total_out <= out.buffer.capacity);
				out.buffer.count = Z.total_out;
				os->pushString(out);
				return 1;
			} else {
			}
		}
		// php_error_docref(NULL TSRMLS_CC, E_WARNING, "%s", zError(status));
		return 0;
	}

	static int inflateRounds(z_stream *Z, int max, OS::Core::Buffer& out)
	{
		int status, round = 0;
		int buf_size = (max && (max < (int)Z->avail_in)) ? max : (int)Z->avail_in;

		do {
			if ((max && (max <= out.buffer.count)) || !(out.reserveCapacity(buf_size), out.buffer.buf)) {
				status = Z_MEM_ERROR;
			} else {
				// buffer.data = buffer.aptr;
				int buf_free = out.buffer.capacity - out.buffer.count;
				if(!buf_free){
					out.reserveCapacity(out.buffer.count + (out.buffer.count>>2) + 1);
				}
				Z->avail_out = buf_free;
				Z->next_out = (Bytef *)out.buffer.buf + out.buffer.count;
				status = inflate(Z, Z_NO_FLUSH);
				out.buffer.count += buf_free - Z->avail_out;
				buf_free = Z->avail_out;
			}
		} while ((Z_BUF_ERROR == status || (Z_OK == status && Z->avail_in)) && ++round < 100);

		if (status == Z_STREAM_END) {
		} else {
			/* HACK: See zlib/examples/zpipe.c inf() function for explanation. */
			/* This works as long as this function is not used for streaming. Required to catch very short invalid data. */
			status = (status == Z_OK) ? Z_DATA_ERROR : status;
		}
		return status;
	}

	static int decodeInternal(OS * os, int params, int encoding)
	{
		if(params < 1){
			return 0;
		}
		OS::String str = os->toString(-params + 0);
		long max_len = params >= 2 ? os->toInt(-params + 1) : 0;
		if(max_len < 0){
			triggerError(os, OS::String::format(os, "length (%ld) must be greater or equal zero", max_len));
			return 0;
		} 
		const char * in_buf = str.toChar();
		int in_len = str.getLen();
		
		int status = Z_DATA_ERROR;
		z_stream Z;
		memset(&Z, 0, sizeof(Z));
		// Z.zalloc = php_zlib_alloc;
		// Z.zfree = php_zlib_free;
		if(in_len > 0){
			OS::Core::Buffer out(os);

retry_raw_inflate:
			status = inflateInit2(&Z, encoding);
			if(Z_OK == status){
				Z.next_in = (Bytef *) in_buf;
				Z.avail_in = in_len + 1; /* NOTE: data must be zero terminated */

				switch(status = inflateRounds(&Z, max_len, out)){
				case Z_STREAM_END:
					inflateEnd(&Z);
					os->pushString(out);
					return 1;

				case Z_DATA_ERROR:
					/* raw deflated data? */
					if(ZLIB_ENCODING_ANY == encoding){
						inflateEnd(&Z);
						encoding = ZLIB_ENCODING_RAW;
						goto retry_raw_inflate;
					}
				}
				inflateEnd(&Z);
			}
		}
		// php_error_docref(NULL TSRMLS_CC, E_WARNING, "%s", zError(status));
		return 0;
	}

	/* proto binary zlib_encode(binary data, int encoding[, int level = -1])
	   Compress data with the specified encoding */
	static int encode(OS * os, int params, int, int, void * user_param)
	{
		return encodeInternal(os, params, ZLIB_ENCODING_ANY);
	}

	/* proto binary zlib_decode(binary data[, int max_decoded_len])
	   Uncompress any raw/gzip/zlib encoded data */
	static int decode(OS * os, int params, int, int, void * user_param)
	{
		return decodeInternal(os, params, ZLIB_ENCODING_ANY);
	}

	/* NOTE: The naming of these userland functions was quite unlucky */
	/* proto binary gzdeflate(binary data[, int level = -1[, int encoding = ZLIB_ENCODING_RAW])
	   Encode data with the raw deflate encoding */
	static int gzdeflate(OS * os, int params, int, int, void * user_param)
	{
		return encodeInternal(os, params, ZLIB_ENCODING_RAW);
	}

	/* proto binary gzencode(binary data[, int level = -1[, int encoding = ZLIB_ENCODING_GZIP])
	   Encode data with the gzip encoding */
	static int gzencode(OS * os, int params, int, int, void * user_param)
	{
		return encodeInternal(os, params, ZLIB_ENCODING_GZIP);
	}

	/* proto binary gzcompress(binary data[, int level = -1[, int encoding = ZLIB_ENCODING_DEFLATE])
	   Encode data with the zlib encoding */
	static int gzcompress(OS * os, int params, int, int, void * user_param)
	{
		return encodeInternal(os, params, ZLIB_ENCODING_DEFLATE);
	}

	/* proto binary gzinflate(binary data[, int max_decoded_len])
	   Decode raw deflate encoded data */
	static int gzinflate(OS * os, int params, int, int, void * user_param)
	{
		return decodeInternal(os, params, ZLIB_ENCODING_RAW);
	}

	/* proto binary gzdecode(binary data[, int max_decoded_len])
	   Decode gzip encoded data */
	static int gzdecode(OS * os, int params, int, int, void * user_param)
	{
		return decodeInternal(os, params, ZLIB_ENCODING_GZIP);
	}

	/* {{{ proto binary gzuncompress(binary data[, int max_decoded_len])
	   Decode zlib encoded data */
	static int gzuncompress(OS * os, int params, int, int, void * user_param)
	{
		return decodeInternal(os, params, ZLIB_ENCODING_DEFLATE);
	}
};

void ZlibOS::initExtension(OS * os)
{
	OS::FuncDef funcs[] = {
		// {OS_TEXT("encode"), encode},
		// {OS_TEXT("decode"), decode},
		{OS_TEXT("gzdeflate"), gzdeflate},
		{OS_TEXT("gzencode"), gzencode},
		{OS_TEXT("gzcompress"), gzcompress},
		{OS_TEXT("gzinflate"), gzinflate},
		{OS_TEXT("gzdecode"), gzdecode},
		{OS_TEXT("gzuncompress"), gzuncompress},
		{}
	};
	os->getModule(OS_TEXT("zlib"));
	os->setFuncs(funcs);
	os->pop();
}

void initZlibExtension(OS* os)
{
	ZlibOS::initExtension(os);
}

} // namespace ObjectScript

