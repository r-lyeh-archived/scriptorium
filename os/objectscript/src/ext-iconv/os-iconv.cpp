#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "os-iconv.h"
#include "../objectscript.h"
#include "../os-binder.h"

#include <iconv.h>

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#define ICONV_SUPPORTS_ERRNO

#ifdef HAVE_IBM_ICONV
# define ICONV_INPUT_ENCODING "ISO8859-1"
# define ICONV_OUTPUT_ENCODING "ISO8859-1"
# define ICONV_INTERNAL_ENCODING "ISO8859-1"
# define ICONV_ASCII_ENCODING "IBM-850"
# define ICONV_UCS4_ENCODING "UCS-4"
#else
# define ICONV_INPUT_ENCODING "ISO-8859-1"
# define ICONV_OUTPUT_ENCODING "ISO-8859-1"
# define ICONV_INTERNAL_ENCODING "ISO-8859-1"
# define ICONV_ASCII_ENCODING "ASCII"
# define ICONV_UCS4_ENCODING "UCS-4LE"
#endif

static char _generic_superset_name[] = ICONV_UCS4_ENCODING;
#define GENERIC_SUPERSET_NAME _generic_superset_name
#define GENERIC_SUPERSET_NBYTES 4

#define TMP_BUF_SIZE 4096

#define ICONV_MEMEQUAL(a, b, c) \
  ((c) == sizeof(OS_U64) ? *((OS_U64*)(a)) == *((OS_U64*)(b)) : ((c) == sizeof(OS_U32) ? *((OS_U32*)(a)) == *((OS_U32*)(b)) : memcmp(a, b, c) == 0)) 

namespace ObjectScript {

class IconvOS: public OS
{
public:

	friend struct Internal;
	struct Internal
	{
		static bool append(OS * os, Core::Buffer& out, const char *s, size_t l, iconv_t cd)
		{
			const char *in_p = s;
			size_t in_left = l;
			char *out_p;
			size_t out_left = 0;
			size_t buf_growth = 128;
		#ifndef ICONV_SUPPORTS_ERRNO
			size_t prev_in_left = in_left;
		#endif

			if (in_p != NULL) {
				while (in_left > 0) {
					out_left = buf_growth - out_left;
					out.reserveCapacity(out.buffer.count + out_left);
					out_p = (char*)out.buffer.buf + out.buffer.count;

					if (iconv(cd, (const char **)&in_p, &in_left, (char **) &out_p, &out_left) == (size_t)-1) {
		#ifdef ICONV_SUPPORTS_ERRNO
						switch (errno) {
						case EINVAL:
							os->setException(OS_TEXT("iconv illegal char"));
							return false;

						case EILSEQ:
							os->setException(OS_TEXT("iconv illegal seq"));
							return false;

						case E2BIG:
							break;

						default:
							os->setException(OS_TEXT("iconv error"));
							return false;
						}
		#else
						if (prev_in_left == in_left) {
							os->setException(OS_TEXT("iconv error"));
							return false;
						}
		#endif
					}
		#ifndef ICONV_SUPPORTS_ERRNO
					prev_in_left = in_left;
		#endif
					out.buffer.count += buf_growth - out_left;
					buf_growth <<= 1;
				}
			} else {
				for (;;) {
					out_left = buf_growth - out_left;
					out.reserveCapacity(out.buffer.count + out_left);
					out_p = (char*)out.buffer.buf + out.buffer.count;

					if (iconv(cd, NULL, NULL, (char **) &out_p, &out_left) == (size_t)0) {
						out.buffer.count += buf_growth - out_left;
						break;
					} else {
		#ifdef ICONV_SUPPORTS_ERRNO
						if (errno != E2BIG) {
							os->setException(OS_TEXT("iconv error"));
							return false;
						}
		#else
						if (out_left != 0) {
							os->setException(OS_TEXT("iconv error"));
							return false;
						}
		#endif
					}
					out.buffer.count += buf_growth - out_left;
					buf_growth <<= 1;
				}
			}
			return true;
		}

		static bool append(OS * os, Core::Buffer& out, const char c, iconv_t cd)
		{
			return append(os, out, &c, 1, cd);
		}

		static bool convert(OS * os, const char* tocode, const char* fromcode,
			const char* start, const char* end,
			Core::Buffer& out)
		{
			out.clear();
			iconv_t cd = iconv_open(tocode, fromcode);
			size_t length;
			char* result;
			if (cd == (iconv_t)(-1)) {
				if (errno != EINVAL)
					return false;
				/* Unsupported fromcode or tocode. Check whether the caller requested
				autodetection. */
				if (!strcmp(fromcode,"autodetect_utf8")) {
					bool ret;
					/* Try UTF-8 first. There are very few ISO-8859-1 inputs that would
					be valid UTF-8, but many UTF-8 inputs are valid ISO-8859-1. */
					ret = convert(os, tocode,"UTF-8",start,end,out);
					if (!(ret == false && errno == EILSEQ))
						return ret;
					ret = convert(os, tocode,"ISO-8859-1",start,end,out);
					return ret;
				}
				if (!strcmp(fromcode,"autodetect_jp")) {
					bool ret;
					/* Try 7-bit encoding first. If the input contains bytes >= 0x80,
					it will fail. */
					ret = convert(os, tocode,"ISO-2022-JP-2",start,end,out);
					if (!(ret == false && errno == EILSEQ))
						return ret;
					/* Try EUC-JP next. Short SHIFT_JIS inputs may come out wrong. This
					is unavoidable. People will condemn SHIFT_JIS.
					If we tried SHIFT_JIS first, then some short EUC-JP inputs would
					come out wrong, and people would condemn EUC-JP and Unix, which
					would not be good. */
					ret = convert(os, tocode,"EUC-JP",start,end,out);
					if (!(ret == false && errno == EILSEQ))
						return ret;
					/* Finally try SHIFT_JIS. */
					ret = convert(os, tocode,"SHIFT_JIS",start,end,out);
					return ret;
				}
				if (!strcmp(fromcode,"autodetect_kr")) {
					bool ret;
					/* Try 7-bit encoding first. If the input contains bytes >= 0x80,
					it will fail. */
					ret = convert(os, tocode,"ISO-2022-KR",start,end,out);
					if (!(ret == false && errno == EILSEQ))
						return ret;
					/* Finally try EUC-KR. */
					ret = convert(os, tocode,"EUC-KR",start,end,out);
					return ret;
				}
				errno = EINVAL;
				return false;
			}
			/* Determine the length we need. */
			Core::Buffer temp(os);
			temp.reserveCapacity(TMP_BUF_SIZE);
			char * tmpbuf = (char*)temp.buffer.buf;
			{
				size_t count = 0;
				// char tmpbuf[TMP_BUF_SIZE];
				const char* inptr = start;
				size_t insize = end-start;
				while (insize > 0) {
					char* outptr = tmpbuf;
					size_t outsize = TMP_BUF_SIZE;
					size_t res = iconv(cd,&inptr,&insize,&outptr,&outsize);
					if (res == (size_t)(-1) && errno != E2BIG) {
						int saved_errno = (errno == EINVAL ? EILSEQ : errno);
						iconv_close(cd);
						errno = saved_errno;
						return false;
					}
					count += outptr-tmpbuf;
				}
				{
					char* outptr = tmpbuf;
					size_t outsize = TMP_BUF_SIZE;
					size_t res = iconv(cd,NULL,NULL,&outptr,&outsize);
					if (res == (size_t)(-1)) {
						int saved_errno = errno;
						iconv_close(cd);
						errno = saved_errno;
						return false;
					}
					count += outptr-tmpbuf;
				}
				length = count;
			}
			out.reserveCapacity(length);
			if (length == 0) {
				iconv_close(cd);
				return true;
			}
			result = (char*)out.buffer.buf;
			if (result == NULL) {
				iconv_close(cd);
				errno = ENOMEM;
				return false;
			}
			iconv(cd,NULL,NULL,NULL,NULL); /* return to the initial state */
			/* Do the conversion for real. */
			{
				const char* inptr = start;
				size_t insize = end-start;
				char* outptr = result;
				size_t outsize = length;
				while (insize > 0) {
					size_t res = iconv(cd,&inptr,&insize,&outptr,&outsize);
					if (res == (size_t)(-1)) {
						if (errno == EINVAL)
							break;
						else {
							int saved_errno = errno;
							iconv_close(cd);
							errno = saved_errno;
							return false;
						}
					}
				}
				{
					size_t res = iconv(cd,NULL,NULL,&outptr,&outsize);
					if (res == (size_t)(-1)) {
						int saved_errno = errno;
						iconv_close(cd);
						errno = saved_errno;
						return false;
					}
				}
				// if (outsize != 0) abort();
				OS_ASSERT(outsize == 0);
			}
			iconv_close(cd);
			out.buffer.count = length;
			return true;
		}

		static bool strlen(OS * os, int *pretval, const char *str, size_t nbytes, const char *enc)
		{
			char buf[GENERIC_SUPERSET_NBYTES*2];

			iconv_t cd;

			const char *in_p;
			size_t in_left;

			char *out_p;
			size_t out_left;

			unsigned int cnt;

			*pretval = 0;

			cd = iconv_open(GENERIC_SUPERSET_NAME, enc);

			if (cd == (iconv_t)(-1)) {
	#ifdef ICONV_SUPPORTS_ERRNO
				if (errno == EINVAL) {
					os->setException(OS_TEXT("iconv wrong charset"));
					iconv_close(cd);
					return false;
				} else {
					os->setException(OS_TEXT("iconv error converter"));
					iconv_close(cd);
					return false;
				}
	#else
				os->setException(OS_TEXT("iconv error"));
				iconv_close(cd);
				return false;
	#endif
			}

			errno = out_left = 0;

			for (in_p = str, in_left = nbytes, cnt = 0; in_left > 0; cnt+=2) {
				size_t prev_in_left;
				out_p = buf;
				out_left = sizeof(buf);

				prev_in_left = in_left;

				if (iconv(cd, (const char **)&in_p, &in_left, (char **) &out_p, &out_left) == (size_t)-1) {
					if (prev_in_left == in_left) {
						break;
					}
				}
			}

			if (out_left > 0) {
				cnt -= out_left / GENERIC_SUPERSET_NBYTES;
			}

	#ifdef ICONV_SUPPORTS_ERRNO
			switch (errno) {
			case EINVAL:
				os->setException(OS_TEXT("iconv error: illegal char"));
				iconv_close(cd);
				return false;

			case EILSEQ:
				os->setException(OS_TEXT("iconv error: illegal seq"));
				iconv_close(cd);
				return false;

			case E2BIG:
			case 0:
				*pretval = cnt;
				break;

			default:
				os->setException(OS_TEXT("iconv error"));
				iconv_close(cd);
				return false;
			}
	#else
			*pretval = cnt;
	#endif

			iconv_close(cd);
			return true;
		}

		static bool substr(OS * os, Core::Buffer& out, const OS::String& str, int start, int len, const char *enc)
		{
			char buf[GENERIC_SUPERSET_NBYTES];

			iconv_t cd1, cd2;

			const char *in_p;
			size_t in_left;

			char *out_p;
			size_t out_left;

			int cnt;
			int total_len;

			int str_len = str.getLen();
			if (!strlen(os, &total_len, str, str_len, enc)) {
				return false;
			}

			if(start >= total_len){
				os->pushString(OS_TEXT(""));
				return true;
			}
			if(len < 0){
				len = total_len - start + len;
			}
			if(len <= 0){
				os->pushString(OS_TEXT(""));
				return true;
			}
			if(start + len > total_len){
				len = total_len - start;
			}
			if(!start && len == total_len){
				os->pushString(str);
				return true;
			}

			cd1 = iconv_open(GENERIC_SUPERSET_NAME, enc);

			if (cd1 == (iconv_t)(-1)) {
	#ifdef ICONV_SUPPORTS_ERRNO
				if (errno == EINVAL) {
					os->setException(OS_TEXT("iconv wrong charset"));
					iconv_close(cd1);
					return false;
				} else {
					os->setException(OS_TEXT("iconv error converter"));
					iconv_close(cd1);
					return false;
				}
	#else
				os->setException(OS_TEXT("iconv error"));
				iconv_close(cd1);
				return false;
	#endif
			}

			cd2 = (iconv_t)NULL;
			errno = 0;

			out.clear();
			for (in_p = str, in_left = str_len, cnt = 0; in_left > 0 && len > 0; ++cnt) {
				size_t prev_in_left;
				out_p = buf;
				out_left = sizeof(buf);

				prev_in_left = in_left;

				if (iconv(cd1, (const char **)&in_p, &in_left, (char **) &out_p, &out_left) == (size_t)-1) {
					if (prev_in_left == in_left) {
						break;
					}
				}

				if (cnt >= start) {
					if (cd2 == (iconv_t)NULL) {
						cd2 = iconv_open(enc, GENERIC_SUPERSET_NAME);

						if (cd2 == (iconv_t)(-1)) {
							cd2 = (iconv_t)NULL;
	#ifdef ICONV_SUPPORTS_ERRNO
							if (errno == EINVAL) {
								os->setException(OS_TEXT("iconv wrong charset"));
								iconv_close(cd2);
								iconv_close(cd1);
								return false;
							} else {
								os->setException(OS_TEXT("iconv error converter"));
								iconv_close(cd2);
								iconv_close(cd1);
								return false;
							}
	#else
							os->setException(OS_TEXT("iconv error"));
							iconv_close(cd2);
							iconv_close(cd1);
							return false;
	#endif
							break;
						}
					}

					if (!append(os, out, buf, sizeof(buf), cd2)) {
						break;
					}
					--len;
				}

			}

	#ifdef ICONV_SUPPORTS_ERRNO
			switch (errno) {
			case EINVAL:
				os->setException(OS_TEXT("iconv error: illegal char"));
				if(cd2) append(os, out, NULL, 0, cd2);
				if(cd2) iconv_close(cd2);
				if(cd1) iconv_close(cd1);
				return false;

			case EILSEQ:
				os->setException(OS_TEXT("iconv error: illegal seq"));
				if(cd2) append(os, out, NULL, 0, cd2);
				if(cd2) iconv_close(cd2);
				if(cd1) iconv_close(cd1);
				return false;

			case E2BIG:
				break;
			}
	#endif
			if(cd2) iconv_close(cd2);
			if(cd1) iconv_close(cd1);
			return true;
		}

		static bool find(OS * os, int *pretval,
			const char *haystk, size_t haystk_nbytes,
			const char *ndl, size_t ndl_nbytes,
			int offset, const char *enc)
		{
			char buf[GENERIC_SUPERSET_NBYTES];

			iconv_t cd;

			const char *in_p;
			size_t in_left;

			char *out_p;
			size_t out_left;

			unsigned int cnt;

			char *ndl_buf;
			const char *ndl_buf_p;
			size_t ndl_buf_len, ndl_buf_left;

			unsigned int match_ofs;

			*pretval = (unsigned int)-1;

			Core::Buffer temp(os);
			bool err = convert(os, GENERIC_SUPERSET_NAME, enc, ndl, ndl + ndl_nbytes, temp);
			if (!err) {
				return err;
			}

			cd = iconv_open(GENERIC_SUPERSET_NAME, enc);
			if (cd == (iconv_t)(-1)) {
		#ifdef ICONV_SUPPORTS_ERRNO
				if (errno == EINVAL) {
					os->setException(OS_TEXT("iconv wrong charset"));
					return false;
				} else {
					os->setException(OS_TEXT("iconv error converter"));
					return false;
				}
		#else
				os->setException(OS_TEXT("iconv error"));
				return false;
		#endif
			}

			ndl_buf = (char*)temp.buffer.buf;
			ndl_buf_len = temp.buffer.count;

			ndl_buf_p = ndl_buf;
			ndl_buf_left = ndl_buf_len;
			match_ofs = (unsigned int)-1;

			for (in_p = haystk, in_left = haystk_nbytes, cnt = 0; in_left > 0; ++cnt) {
				size_t prev_in_left;
				out_p = buf;
				out_left = sizeof(buf);

				prev_in_left = in_left;

				if (iconv(cd, (const char **)&in_p, &in_left, (char **) &out_p, &out_left) == (size_t)-1) {
					if (prev_in_left == in_left) {
		#ifdef ICONV_SUPPORTS_ERRNO
						switch (errno) {
							case EINVAL:
								os->setException(OS_TEXT("iconv error: illegal char"));
								iconv_close(cd);
								return false;

							case EILSEQ:
								os->setException(OS_TEXT("iconv error: illegal seq"));
								iconv_close(cd);
								return false;

							case E2BIG:
								break;

							default:
								os->setException(OS_TEXT("iconv error"));
								iconv_close(cd);
								return false;
						}
		#endif
						break;
					}
				}
				if (offset >= 0) {
					if (cnt >= (unsigned int)offset) {
						if (ICONV_MEMEQUAL(buf, ndl_buf_p, sizeof(buf))) {
							if (match_ofs == (unsigned int)-1) {
								match_ofs = cnt;
							}
							ndl_buf_p += GENERIC_SUPERSET_NBYTES;
							ndl_buf_left -= GENERIC_SUPERSET_NBYTES;
							if (ndl_buf_left == 0) {
								*pretval = match_ofs;
								break;
							}
						} else {
							unsigned int i, j, lim;

							i = 0;
							j = GENERIC_SUPERSET_NBYTES;
							lim = (unsigned int)(ndl_buf_p - ndl_buf);

							while (j < lim) {
								if (ICONV_MEMEQUAL(&ndl_buf[j], &ndl_buf[i],
										   GENERIC_SUPERSET_NBYTES)) {
									i += GENERIC_SUPERSET_NBYTES;
								} else {
									j -= i;
									i = 0;
								}
								j += GENERIC_SUPERSET_NBYTES;
							}

							if (ICONV_MEMEQUAL(buf, &ndl_buf[i], sizeof(buf))) {
								match_ofs += (lim - i) / GENERIC_SUPERSET_NBYTES;
								i += GENERIC_SUPERSET_NBYTES;
								ndl_buf_p = &ndl_buf[i];
								ndl_buf_left = ndl_buf_len - i;
							} else {
								match_ofs = (unsigned int)-1;
								ndl_buf_p = ndl_buf;
								ndl_buf_left = ndl_buf_len;
							}
						}
					}
				} else {
					if (ICONV_MEMEQUAL(buf, ndl_buf_p, sizeof(buf))) {
						if (match_ofs == (unsigned int)-1) {
							match_ofs = cnt;
						}
						ndl_buf_p += GENERIC_SUPERSET_NBYTES;
						ndl_buf_left -= GENERIC_SUPERSET_NBYTES;
						if (ndl_buf_left == 0) {
							*pretval = match_ofs;
							ndl_buf_p = ndl_buf;
							ndl_buf_left = ndl_buf_len;
							match_ofs = -1;
						}
					} else {
						unsigned int i, j, lim;

						i = 0;
						j = GENERIC_SUPERSET_NBYTES;
						lim = (unsigned int)(ndl_buf_p - ndl_buf);

						while (j < lim) {
							if (ICONV_MEMEQUAL(&ndl_buf[j], &ndl_buf[i],
									   GENERIC_SUPERSET_NBYTES)) {
								i += GENERIC_SUPERSET_NBYTES;
							} else {
								j -= i;
								i = 0;
							}
							j += GENERIC_SUPERSET_NBYTES;
						}

						if (ICONV_MEMEQUAL(buf, &ndl_buf[i], sizeof(buf))) {
							match_ofs += (lim - i) / GENERIC_SUPERSET_NBYTES;
							i += GENERIC_SUPERSET_NBYTES;
							ndl_buf_p = &ndl_buf[i];
							ndl_buf_left = ndl_buf_len - i;
						} else {
							match_ofs = (unsigned int)-1;
							ndl_buf_p = ndl_buf;
							ndl_buf_left = ndl_buf_len;
						}
					}
				}
			}

			iconv_close(cd);

			return err;
		} 
	};

	static int convert(OS * os, int params, int, int, void * user_param)
	{
		if(params < 3){
			return 0;
		}
		OS::String in_charset = os->toString(-params+0);
		OS::String out_charset = os->toString(-params+1);
		OS::String str = os->toString(-params+2);
		const char* tocode = out_charset.toChar();
		const char* fromcode = in_charset.toChar();
		const char* start = str.toChar();
		const char* end = start + str.getLen();
		Core::Buffer buf(os);
		bool ok = Internal::convert(os, tocode, fromcode, start, end, buf);
		if(ok){
			os->pushString(buf);
			return 1;
		}
		return 0;
	}

	static OS::String getDefaultCharset(OS * os)
	{
		os->getGlobal(OS_TEXT("iconv"));
		os->getProperty(OS_TEXT("defaultCharset"));
		return os->popString();
	}

	static int len(OS * os, int params, int, int, void * user_param)
	{
		OS::String str = os->toString(-params-1);
		OS::String charset = params >= 1 ? os->toString(-params+0) : getDefaultCharset(os);

		int len = 0;
		if(Internal::strlen(os, &len, str.toChar(), str.getLen(), charset.toChar())){
			os->pushNumber(len);
			return 1;
		}
		return 0;
	}

	static int find(OS * os, int params, int, int, void * user_param)
	{
		if(params >= 1){
			OS::String str = os->toString(-params-1);
			OS::String what = os->toString(-params+0);
			int offset = params >= 2 ? os->toInt(-params+1) : 0;
			OS::String charset = params >= 3 ? os->toString(-params+2) : getDefaultCharset(os);

			int pos = 0;
			if(Internal::find(os, &pos, str.toChar(), str.getLen(), what.toChar(), what.getLen(), offset, charset.toChar())){
				os->pushNumber(pos);
				return 1;
			}
		}
		return 0;
	}

	static int sub(OS * os, int params, int, int, void * user_param)
	{
		int start, len;
		OS::String str = os->toString(-params-1);
		switch(params){
		case 0:
			os->pushStackValue(-params-1);
			return 1;

		case 1:
			start = os->toInt(-params);
			len = str.getLen();
			break;

		default:
			start = os->toInt(-params);
			len = os->toInt(-params+1);
		}
		OS::String charset = params >= 3 ? os->toString(-params+2) : getDefaultCharset(os);

		Core::Buffer out(os);
		if(Internal::substr(os, out, str, start, len, charset)){
			os->pushString(out);
			return 1;
		}
		return 0;
	}

};

void initIconvExtension(OS * os)
{
	{
		OS::FuncDef funcs[] = {
			{OS_TEXT("iconv"), &IconvOS::convert},
			{}
		};
		os->pushGlobals();
		os->setFuncs(funcs);
		os->pop();
	}
	{
		os->getGlobal(OS_TEXT("String"));
		OS::FuncDef funcs[] = {
			{OS_TEXT("lenIconv"), &IconvOS::len},
			{OS_TEXT("subIconv"), &IconvOS::sub},
			{OS_TEXT("findIconv"), &IconvOS::find},
			{}
		};
		os->setFuncs(funcs);
		os->pop();

		os->getGlobal(OS_TEXT("iconv"));
		os->pushString(OS_TEXT("utf-8"));
		os->setProperty(OS_TEXT("defaultCharset"));
	}
}

} // namespace ObjectScript

