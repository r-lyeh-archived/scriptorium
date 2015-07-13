#ifndef __OBJECT_SCRIPT_H__
#define __OBJECT_SCRIPT_H__

/******************************************************************************
* Copyright (C) 2012-2014 Evgeniy Golovin (evgeniy.golovin@unitpoint.ru)
*
* Please feel free to contact me at anytime, 
* my email is evgeniy.golovin@unitpoint.ru, skype: egolovin
*
* Latest source code: https://github.com/unitpoint/objectscript
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************/

#include <stdarg.h>
#include <stdio.h>
#include <assert.h>
#include <ctype.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#ifndef __APPLE__
#include <malloc.h>
#else
#include <malloc/malloc.h>
#endif

#if !defined __GNUC__ || defined IW_SDK
#include <new.h>
#elif defined(__GNUC__)
#include <new>
#else
inline void *operator new(size_t, void * p){ return p; }
inline void operator delete(void *, void *){}
#endif

#if defined _MSC_VER && !defined IW_SDK
#include <vadefs.h>
#endif

#if INTPTR_MAX == INT32_MAX
#define OS_PLATFORM_BITS_VERSION
#elif INTPTR_MAX == INT64_MAX
#define OS_PLATFORM_BITS_VERSION OS_TEXT("-x64")
#else
#define OS_PLATFORM_BITS_VERSION
#endif

#if defined _DEBUG && !defined OS_RELEASE && !defined OS_DEBUG
#define OS_DEBUG
#endif

#ifdef OS_DEBUG
#define OS_DEBUG_VERSION OS_TEXT("-d")
#else
#define OS_DEBUG_VERSION
#endif

#define OS_VERSION		OS_TEXT("2.6.5-rc") OS_PLATFORM_BITS_VERSION OS_DEBUG_VERSION
#define OS_COPYRIGHT	OS_TEXT("OS ") OS_VERSION OS_TEXT(" Copyright (C) 2012-2014 by Evgeniy Golovin")
#define OS_OPENSOURCE	OS_TEXT("ObjectScript is free and open source: https://github.com/unitpoint/objectscript")

// select ObjectScript number type here
#ifndef OS_NUMBER
// #define OS_NUMBER long double
#define OS_NUMBER double
// #define OS_NUMBER float	// could be a bit faster
// #define OS_NUMBER int	// not recomended, math.random returns float value [0..1]
#endif // OS_NUMBER

#define OS_MATH_POW_OPERATOR(a, b) (OS_NUMBER)::pow((double)(a), (double)(b))
// #define OS_MATH_MOD_OPERATOR(a, b) (OS_NUMBER)((OS_INT)(a) % (OS_INT)(b))
#define OS_MATH_MOD_OPERATOR(a, b) (OS_NUMBER)((double)(a) - ::floor((double)(a)/(double)(b))*(double)(b))

#define OS_CHAR char
#define OS_TEXT(s) s

#define OS_FUNC_VAR_NAME OS_TEXT("_F")
#define OS_THIS_VAR_NAME OS_TEXT("this")
#define OS_ENV_VAR_NAME OS_TEXT("_E")
#define OS_GLOBALS_VAR_NAME OS_TEXT("_G")

#define OS_INT8 signed char
#define OS_BYTE unsigned char
#define OS_INT16 short
#define OS_U16 unsigned short

#if defined __GNUC__ 
#include <inttypes.h>

#define OS_INT32 int32_t
#define OS_INT64 int64_t
#define OS_U32 uint32_t
#define OS_U64 uint64_t

#elif defined IW_SDK

#define OS_INT32 int32
#define OS_INT64 int64
#define OS_U32 uint32
#define OS_U64 uint64

#else

#define OS_INT32 __int32
#define OS_INT64 __int64
#define OS_U32 unsigned __int32
#define OS_U64 unsigned __int64

#endif

#ifdef OS_EMSCRIPTEN
#define OS_INT	OS_INT32
#define OS_UINT	OS_U32
#else
#define OS_INT	OS_INT64	// dependence on OS_NUMBER
#define OS_UINT	OS_U64		// dependence on OS_NUMBER
#endif

#define OS_MEMCMP ::memcmp
#define OS_MEMMOVE ::memmove
#define OS_MEMSET ::memset
#define OS_MEMCPY ::memcpy
#define OS_STRLEN ::strlen
#define OS_STRCMP ::strcmp
#define OS_STRNCMP ::strncmp
#define OS_STRCHR ::strchr
#define OS_STRSTR ::strstr

#define OS_VPRINTF ::vprintf
#define OS_PRINTF ::printf
#define OS_OUTPUT(buf, size) fwrite((const char*)buf, size, 1, stdout)

#define OS_IS_SPACE(c) ((c) > OS_TEXT('\0') && (c) <= OS_TEXT(' '))
#define OS_IS_ALPHA ::isalpha
#define OS_IS_ALNUM(c) ((c) >= OS_TEXT('0') && (c) <= OS_TEXT('9'))
#define OS_IS_SLASH(c) ((c) == OS_TEXT('/') || (c) == OS_TEXT('\\'))

#define OS_CHAR_UPPER(c) toupper(c)
#define OS_CHAR_LOWER(c) tolower(c)

#define OS_TOP_STACK_NULL_VALUES 20
#define OS_DEF_FMT_BUF_LEN (1024*10)
#define OS_PATH_SEPARATOR OS_TEXT("/")

// uncomment it if need
// #define OS_INFINITE_LOOP_OPCODES 100000000

#ifdef OS_DEBUG
#define OS_DEF_MAX_CALL_STACK_SIZE 80
#else
#define OS_DEF_MAX_CALL_STACK_SIZE 200
#endif

#define OS_COMPILED_HEADER OS_TEXT("OBJECTSCRIPT")
#define OS_EXT_SOURCECODE OS_TEXT(".os")
#define OS_EXT_TEMPLATE OS_TEXT(".osh")
#define OS_EXT_TEMPLATE_HTML OS_TEXT(".html")
#define OS_EXT_TEMPLATE_HTM OS_TEXT(".htm")
#define OS_EXT_COMPILED OS_TEXT(".osc")
#define OS_EXT_TEXT_OPCODES OS_TEXT(".ost")

#ifdef OS_DEBUG

#define OS_ASSERT assert

#define OS_DBG_FILEPOS_DECL , const OS_CHAR * dbg_filename, int dbg_line
#define OS_DBG_FILEPOS_PARAM , dbg_filename, dbg_line
#define OS_DBG_FILEPOS , __FILE__, __LINE__

#define OS_DBG_FILEPOS_START_DECL const OS_CHAR * dbg_filename, int dbg_line
#define OS_DBG_FILEPOS_START_PARAM dbg_filename, dbg_line
#define OS_DBG_FILEPOS_START __FILE__, __LINE__

#else

#define OS_ASSERT(a)

#define OS_DBG_FILEPOS_DECL
#define OS_DBG_FILEPOS_PARAM
#define OS_DBG_FILEPOS

#define OS_DBG_FILEPOS_START_DECL
#define OS_DBG_FILEPOS_START_PARAM
#define OS_DBG_FILEPOS_START

#endif // OS_DEBUG

#if defined _MSC_VER // && !defined IW_SDK
#define DEBUG_BREAK __debugbreak()
#elif !defined __GNUC__
#include <signal.h>
#define DEBUG_BREAK raise(SIGTRAP)
// #define DEBUG_BREAK __builtin_trap()
#else
#define DEBUG_BREAK 
#endif

#ifndef OS_PROFILE_BEGIN_OPCODE
#define OS_PROFILE_BEGIN_OPCODE(a)
#define OS_PROFILE_END_OPCODE(a)
#endif

#ifndef OS_PROFILE_BEGIN_GC
#define OS_PROFILE_BEGIN_GC
#define OS_PROFILE_END_GC
#endif

namespace ObjectScript
{
	template <class T> struct FloatType { typedef float type; };
	template <> struct FloatType<double> { typedef double type; };
	template <> struct FloatType<long double> { typedef long double type; };
	#define OS_FLOAT FloatType<OS_NUMBER>::type

	class OS;

	typedef void (*OS_UserdataDtor)(OS*, void * data, void * user_param);
	typedef int (*OS_CFunction)(OS*, int params, int closure_values, int need_ret_values, void * user_param);

	enum OS_ESettings
	{
		OS_SETTING_CREATE_TEXT_OPCODES,
		OS_SETTING_CREATE_TEXT_EVAL_OPCODES,
		OS_SETTING_CREATE_DEBUG_INFO,
		OS_SETTING_CREATE_COMPILED_FILE,
		OS_SETTING_PRIMARY_COMPILED_FILE,
		OS_SETTING_SOURCECODE_MUST_EXIST,
	};

	enum OS_EValueType
	{
		// OS_VALUE_TYPE_UNKNOWN,
		OS_VALUE_TYPE_NULL,
		OS_VALUE_TYPE_BOOL,
		OS_VALUE_TYPE_NUMBER,
		OS_VALUE_TYPE_STRING,	// min GC type, don't change order of value types
		OS_VALUE_TYPE_ARRAY,
		OS_VALUE_TYPE_OBJECT,
		OS_VALUE_TYPE_USERDATA,
		OS_VALUE_TYPE_USERPTR,
		OS_VALUE_TYPE_FUNCTION,
		OS_VALUE_TYPE_CFUNCTION		
	};

	enum OS_ESourceCodeType
	{
		OS_SOURCECODE_AUTO,
		OS_SOURCECODE_PLAIN,
		OS_SOURCECODE_TEMPLATE
	};

	enum OS_EFileUseType
	{
		COMPILE_SOURCECODE_FILE,
		LOAD_COMPILED_FILE
	};

	enum // OS_ValueRegister
	{
		OS_REGISTER_GLOBALS = 0x10000000,
		OS_REGISTER_USERPOOL
	};

	enum
	{
		TERMINATED_EXCEPTION_CODE = 1<<30
	};

	enum OS_EOpcode
	{
		// binary operators

		OP_LOGIC_PTR_EQ,	// ===
		OP_LOGIC_PTR_NE,	// !==
		OP_LOGIC_EQ,		// ==
		OP_LOGIC_NE,		// !=
		OP_LOGIC_GE,		// >=
		OP_LOGIC_LE,		// <=
		OP_LOGIC_GREATER,	// >
		OP_LOGIC_LESS,		// <

		OP_BIT_AND,	// &
		OP_BIT_OR,	// |
		OP_BIT_XOR,	// ^

		OP_COMPARE, // <=>
		OP_ADD, // +
		OP_SUB, // -
		OP_MUL, // *
		OP_DIV, // /
		OP_MOD, // %
		OP_LSHIFT, // <<
		OP_RSHIFT, // >>
		OP_POW, // **

		OP_CONCAT,	// ..
		OP_IN,		// in
		OP_IS,		// is
		OP_AS,		// as

		// unary operators

		OP_BIT_NOT,		// ~
		OP_PLUS,		// +
		OP_MINUS,			// -
		OP_LENGTH,		// #
	};

	enum OS_ECallType
	{
		OS_CALLTYPE_AUTO,
		OS_CALLTYPE_FUNC
	};

	enum OS_ECallEnter
	{
		OS_CALLENTER_ALLOW_ONLY_ENTER,
		OS_CALLENTER_EXECUTE_AND_RETURN,
	};

	enum OS_ECallThisUsage
	{
		OS_CALLTHIS_FUNCTION_OVERWRITE,
		OS_CALLTHIS_KEEP_STACK_VALUE,
	};

	class OS
#ifdef OBJECT_SCRIPT_EXTENDS_CLASS
		: public OBJECT_SCRIPT_EXTENDS_CLASS
#endif
	{
	public:

		class MemoryManager
		{
		protected:

			int ref_count;

			virtual ~MemoryManager();

		public:

			MemoryManager();

			MemoryManager * retain();
			void release();

			virtual void * malloc(int size OS_DBG_FILEPOS_DECL) = 0;
			virtual void free(void * p) = 0;
			virtual void setBreakpointId(int id) = 0;

			virtual int getAllocatedBytes() = 0;
			virtual int getMaxAllocatedBytes() = 0;
			virtual int getUsedBytes() = 0;
			virtual int getCachedBytes() = 0;
		};

		struct Utils
		{
			enum ENumberParseType
			{
				PARSE_TOKEN, // 0x - hex, 0b - bin, 0 - octal, else - dec
				PARSE_INT,
				PARSE_FLOAT,
			};

			static bool parseFloat(const OS_CHAR *& str, OS_FLOAT& val, ENumberParseType parse_type, int int_radix = 10);

			static OS_CHAR * numToStr(OS_CHAR*, OS_INT32 value);
			static OS_CHAR * numToStr(OS_CHAR*, OS_INT64 value);
			static OS_CHAR * numToStr(OS_CHAR*, OS_FLOAT value);
			static OS_CHAR * numToStr(OS_CHAR*, OS_FLOAT value, int precision);

			static OS_INT strToInt(const OS_CHAR*);
			static OS_FLOAT strToFloat(const OS_CHAR*);

			static int keyToHash(const void*, int size);
			static int keyToHash(const void * buf1, int size1, const void * buf2, int size2);

			static int cmp(const void * buf1, int len1, const void * buf2, int len2);

			static double round(double a, int precision = 0);
		};

		class String;
		class ObjectScriptExtention;
		
		struct FileHandle {};

	// protected:

		template<class T>
		struct Vector
		{
			T * buf;
			int capacity;
			int count;

			Vector()
			{
				buf = NULL;
				capacity = 0;
				count = 0;
			}
			~Vector()
			{
				OS_ASSERT(!buf && !capacity && !count);
			}
			T& operator [] (int i)
			{
				OS_ASSERT(i >= 0 && i < count);
				return buf[i];
			}
			const T& operator [] (int i) const
			{
				OS_ASSERT(i >= 0 && i < count);
				return buf[i];
			}

			T& lastElement()
			{
				OS_ASSERT(count > 0);
				return buf[count-1];
			}
			const T& lastElement() const
			{
				OS_ASSERT(count > 0);
				return buf[count-1];
			}

			bool contains(const T& val) const 
			{
				for(int i = count-1; i >= 0; i--){
					if(buf[i] == val){
						return true;
					}
				}
				return false;
			}

			int indexOf(const T& val) const
			{
				for(int i = 0; i < count; i++){
					if(buf[i] == val){
						return i;
					}
				}
				return -1;
			}
		};

		template<class T> void vectorReserveCapacity(Vector<T>& vec, int new_capacity OS_DBG_FILEPOS_DECL)
		{
			if(vec.capacity < new_capacity){
				vec.capacity = vec.capacity > 0 ? vec.capacity*2 : 4;
				if(vec.capacity < new_capacity){
					vec.capacity = new_capacity; // (capacity+3) & ~3;
				}
				T * new_buf = (T*)malloc(sizeof(T)*vec.capacity OS_DBG_FILEPOS_PARAM);
				OS_ASSERT(new_buf);
				for(int i = 0; i < vec.count; i++){
					new (new_buf+i) T(vec.buf[i]);
					vec.buf[i].~T();
				}
				free(vec.buf);
				vec.buf = new_buf;
			}
		}

		template<class T> void vectorReserveCapacityExact(Vector<T>& vec, int capacity OS_DBG_FILEPOS_DECL)
		{
			if(vec.capacity < capacity){
				vec.capacity = capacity;
				T * new_buf = (T*)malloc(sizeof(T)*vec.capacity OS_DBG_FILEPOS_PARAM);
				OS_ASSERT(new_buf);
				for(int i = 0; i < vec.count; i++){
					new (new_buf+i) T(vec.buf[i]);
					vec.buf[i].~T();
				}
				free(vec.buf);
				vec.buf = new_buf;
			}
		}

		template<class T> void vectorAddItem(Vector<T>& vec, const T& val OS_DBG_FILEPOS_DECL)
		{
			vectorReserveCapacity(vec, vec.count+1 OS_DBG_FILEPOS_PARAM);
			new (vec.buf + vec.count++) T(val);
		}

		template<class T> void vectorClear(Vector<T>& vec)
		{
			for(int i = 0; i < vec.count; i++){
				vec.buf[i].~T();
			}
			free(vec.buf);
			vec.buf = NULL;
			vec.capacity = 0;
			vec.count = 0;
		}

		template<class T> void vectorReleaseItems(Vector<T>& vec)
		{
			for(int i = 0; i < vec.count; i++){
				vec.buf[i]->release();
			}
			// free(vec.buf);
			// vec.buf = NULL;
			// vec.capacity = 0;
			vec.count = 0;
		}

		template<class T> void vectorDeleteItems(Vector<T*>& vec)
		{
			for(int i = 0; i < vec.count; i++){
				T * item = vec.buf[i];
				item->~T();
				free(item);
			}
			// free(vec.buf);
			// vec.buf = NULL;
			// vec.capacity = 0;
			vec.count = 0;
		}

		template<class T> void vectorInsertAtIndex(Vector<T>& vec, int i, const T& val OS_DBG_FILEPOS_DECL)
		{
			OS_ASSERT(i >= 0 && i <= vec.count);
			vectorReserveCapacity(vec, vec.count+1 OS_DBG_FILEPOS_PARAM);
			for(int j = vec.count-1; j >= i; j--){
				new (vec.buf+j+1) T(vec.buf[j]);
				vec.buf[j].~T();
			}
			new (vec.buf+i) T(val);
			vec.count++;
		}

		template<class T> void vectorRemoveAtIndex(Vector<T>& vec, int i)
		{
			OS_ASSERT(i >= 0 && i < vec.count);
			// T val = vec.buf[i];
			vec.buf[i].~T();
			for(i++; i < vec.count; i++){
				new (vec.buf+i-1) T(vec.buf[i]);
				vec.buf[i].~T();
			}
			vec.count--;
			// return val;
		}

		template<class T> void vectorPush(Vector<T>& vec, const T& val OS_DBG_FILEPOS_DECL)
		{
			vectorAddItem(vec, val OS_DBG_FILEPOS_PARAM);
		}

		template<class T> T vectorPop(Vector<T>& vec)
		{
			T ret = vec.lastElement();
			vectorRemoveAtIndex(vec, vec.count-1);
			return ret;
		}

		template<class T> void releaseObj(T *& obj)
		{
			if(--obj->ref_count <= 0){
				OS_ASSERT(obj->ref_count == 0);
				obj->~T();
				free(obj);
				obj = NULL;
			}
		}

		template<class T> void deleteObj(T *& obj)
		{
			if(obj){
				obj->~T();
				free(obj);
				obj = NULL;
			}
		}

		template<class T> void destroyObj(T& obj)
		{
			obj.~T();
		}

	protected:

		friend class OSMemoryManagerOld;

		class Core
		{
			friend class OSMemoryManagerOld;

		public:

			class StreamReader;
			class StreamWriter
			{
			public:

				OS * allocator;

				StreamWriter(OS*);
				virtual ~StreamWriter();

				virtual int getPos() const = 0;
				virtual void setPos(int) = 0;
				
				virtual int getSize() const = 0;

				virtual void writeFromStream(StreamReader*);

				virtual void writeBytes(const void*, int len) = 0;
				virtual void writeBytesAtPos(const void*, int len, int pos) = 0;

				virtual void writeByte(int);
				virtual void writeByteAtPos(int value, int pos);

				virtual void writeUVariable(int);

				virtual void writeU16(int);
				virtual void writeU16AtPos(int value, int pos);

				virtual void writeInt8(int);
				virtual void writeInt8AtPos(int value, int pos);

				virtual void writeInt16(int);
				virtual void writeInt16AtPos(int value, int pos);

				virtual void writeInt32(int);
				virtual void writeInt32AtPos(int value, int pos);

				virtual void writeInt64(OS_INT64);
				virtual void writeInt64AtPos(OS_INT64 value, int pos);

				virtual void writeFloat(float);
				virtual void writeFloatAtPos(float value, int pos);

				virtual void writeDouble(double);
				virtual void writeDoubleAtPos(double value, int pos);

				virtual void writeLongDouble(long double);
				virtual void writeLongDoubleAtPos(long double value, int pos);
			};

			class MemStreamWriter: public StreamWriter
			{
			public:

				Vector<OS_BYTE> buffer;
				int pos;

				MemStreamWriter(OS*);
				~MemStreamWriter();

				int getPos() const;
				void setPos(int);
				
				int getSize() const;

				void clear();
				void reserveCapacity(int new_capacity);

				void writeBytes(const void*, int len);
				void writeBytesAtPos(const void*, int len, int pos);

				void writeByte(int);
				void writeByteAtPos(int value, int pos);
			};

			class FileStreamWriter: public StreamWriter
			{
			public:

				FileHandle * f;

				FileStreamWriter(OS*, const OS_CHAR * filename);
				~FileStreamWriter();

				int getPos() const;
				void setPos(int);
				
				int getSize() const;

				void writeBytes(const void*, int len);
				void writeBytesAtPos(const void*, int len, int pos);
			};

			class StreamReader
			{
			public:

				OS * allocator; // if NULL then buffer will not be freed

				StreamReader(OS*);
				virtual ~StreamReader();

				virtual int getPos() const = 0;
				virtual void setPos(int) = 0;
				
				virtual int getSize() const = 0;

				virtual void movePos(int len) = 0;
				virtual bool checkBytes(const void*, int len) = 0;

				virtual void * readBytes(void*, int len) = 0;
				virtual void * readBytesAtPos(void*, int len, int pos) = 0;

				virtual OS_BYTE readByte();
				virtual OS_BYTE readByteAtPos(int pos);

				virtual int readUVariable();

				virtual OS_U16 readU16();
				virtual OS_U16 readU16AtPos(int pos);

				virtual OS_INT8 readInt8();
				virtual OS_INT8 readInt8AtPos(int pos);

				virtual OS_INT16 readInt16();
				virtual OS_INT16 readInt16AtPos(int pos);

				virtual OS_INT32 readInt32();
				virtual OS_INT32 readInt32AtPos(int pos);

				virtual OS_INT64 readInt64();
				virtual OS_INT64 readInt64AtPos(int pos);

				virtual float readFloat();
				virtual float readFloatAtPos(int pos);

				virtual double readDouble();
				virtual double readDoubleAtPos(int pos);

				virtual long double readLongDouble();
				virtual long double readLongDoubleAtPos(int pos);
			};

			class MemStreamReader: public StreamReader
			{
			public:

				OS_BYTE * buffer;
				int size;
				// int pos;
				OS_BYTE * cur;

				// if allocator is NULL then buffer will not be freed
				MemStreamReader(OS*, int buf_size);
				MemStreamReader(OS*, OS_BYTE * buf, int buf_size);
				~MemStreamReader();

				int getPos() const;
				void setPos(int);
				
				int getSize() const;

				void movePos(int len);
				bool checkBytes(const void*, int len);

				void * readBytes(void*, int len);
				void * readBytesAtPos(void*, int len, int pos);

				OS_BYTE readByte();
				OS_BYTE readByteAtPos(int pos);

				OS_INT8 readInt8();
				OS_INT16 readInt16();
				OS_INT32 readInt32();
			};

			class FileStreamReader: public StreamReader
			{
			public:

				FileHandle * f;

				FileStreamReader(OS*, const OS_CHAR * filename);
				~FileStreamReader();

				int getPos() const;
				void setPos(int);
				
				int getSize() const;

				void movePos(int len);
				bool checkBytes(const void*, int len);

				void * readBytes(void*, int len);
				void * readBytesAtPos(void*, int len, int pos);
			};

			struct GCStringValue;

			class String
			{
			public:

#ifdef OS_DEBUG
				const OS_CHAR * str;
#endif
				GCStringValue * string;
				OS * allocator;

				String(OS*);
				String(OS * os, GCStringValue*);
				String(const String&);
				String(OS*, const String&, const String&);
				String(OS*, const OS_CHAR*);
				String(OS*, const OS_CHAR*, int len);
				String(OS*, const OS_CHAR*, int len, const OS_CHAR*, int len2);
				String(OS*, const OS_CHAR*, int len, bool trim_left, bool trim_right);
				String(OS*, const String&, bool trim_left, bool trim_right);
				String(OS*, const void*, int size);
				String(OS*, const void * buf1, int len1, const void * buf2, int len2);
				String(OS*, const void * buf1, int len1, const void * buf2, int len2, const void * buf3, int len3);
				String(OS*, OS_INT value);
				String(OS*, OS_FLOAT value);
				String(OS*, OS_FLOAT value, int precision);
				~String();

				static String format(OS*, int temp_buf_len, const OS_CHAR * fmt, ...);
				static String formatVa(OS*, int temp_buf_len, const OS_CHAR * fmt, va_list va);
				static String format(OS*, const OS_CHAR * fmt, ...);
				static String formatVa(OS*, const OS_CHAR * fmt, va_list va);

				const OS_CHAR * toChar() const { return string->toChar(); }
				operator const OS_CHAR*() const { return string->toChar(); }

				OS_CHAR operator[](int i)
				{
					if(i >= 0 && i < getLen()){
						return toChar()[i];
					}
					return OS_TEXT('\0');
				}

				int getDataSize() const { return string->data_size; }
				int getLen() const { return string->getLen(); }
				bool isEmpty() const { return getDataSize() == 0; }

				String& operator=(const String&);

				bool operator==(const String&) const;
				bool operator==(const OS_CHAR*) const;
				bool operator==(GCStringValue*) const;

				bool operator!=(const String&) const;
				bool operator!=(const OS_CHAR*) const;
				bool operator!=(GCStringValue*) const;

				bool operator<=(const String&) const;
				bool operator<=(const OS_CHAR*) const;

				bool operator<(const String&) const;
				bool operator<(const OS_CHAR*) const;

				bool operator>=(const String&) const;
				bool operator>=(const OS_CHAR*) const;

				bool operator>(const String&) const;
				bool operator>(const OS_CHAR*) const;

				int cmp(const String&) const;
				int cmp(const OS_CHAR*) const;
				int getHash() const;

				OS_NUMBER toNumber() const;
				OS_NUMBER toNumberRadix(int radix) const;
			};

			class Buffer: public MemStreamWriter
			{
			protected:

				Core::GCStringValue * cache_str;
				OS * allocator;

			public:

				Buffer(OS*);
				Buffer(const Buffer&);
				~Buffer();

				Buffer& append(OS_CHAR);
				Buffer& append(const OS_CHAR*);
				Buffer& append(const OS_CHAR*, int len);
				Buffer& append(const void*, int size);
				Buffer& append(const Core::String&);
				Buffer& append(const Buffer&);

				Buffer& operator+=(const Core::String&);
				Buffer& operator+=(const OS_CHAR*);

				operator Core::String();
				Core::String toString();
				OS::String toStringOS();

				void clear();

				Core::GCStringValue * toGCStringValue();
				void freeCacheStr();
				void swap(Buffer&);
			};

			class File
			{
			protected:

				OS * os;
				FileHandle * f;

			public:

				File(OS*);
				virtual ~File();

				bool open(const OS_CHAR * filename, const OS_CHAR * mode = "rb");
				void close();

				bool isOpen() const;
				int getSize() const;
				int getPos() const;
				void setPos(int);

				String read();
				String read(int len);
				int write(const void * data, int len);
				int write(const Core::String&);
			};

			class Tokenizer
			{
			public:

				enum Error
				{
					ERROR_NOTHING,
					ERROR_MULTI_LINE_COMMENT, // multi line comment not end
					ERROR_CONST_STRING,             // string not end
					ERROR_CONST_STRING_ESCAPE_CHAR, // string escape error
					ERROR_SYNTAX
				};

				enum TokenType
				{
					NOTHING,

					BEGIN_CODE_BLOCK,  // {
					END_CODE_BLOCK,    // }

					BEGIN_BRACKET_BLOCK,  // (
					END_BRACKET_BLOCK,    // )

					BEGIN_ARRAY_BLOCK,  // [
					END_ARRAY_BLOCK,    // ]

					CODE_SEPARATOR,     // ;
					PARAM_SEPARATOR,    // ,

					COMMENT_LINE,
					COMMENT_MULTI_LINE,

					NAME,           // [a..z_$][a..z0..9_$]*

					STRING,         // ["].*?["]
					OUTPUT_STRING,
					OUTPUT_NEXT_VALUE,

					REGEXP_STRING,  // /.*?[^\\]/\w+

					BEFORE_INJECT_VAR, 
					AFTER_INJECT_VAR, 

					NUMBER,      // -?[0..9][.]?[0..9]+(e[+-]?[0..9]+)?

					// [not real operators]
					OPERATOR,
					BINARY_OPERATOR,
					SEPARATOR,
					// [/not real operators]

					OPERATOR_INDIRECT,    // .
					OPERATOR_CONCAT,    // ..
					REST_ARGUMENTS,  // ...

					OPERATOR_THIS, // @

					OPERATOR_LOGIC_AND, // &&
					OPERATOR_LOGIC_OR,  // ||

					OPERATOR_LOGIC_PTR_EQ,  // ===
					OPERATOR_LOGIC_PTR_NE,  // !==
					OPERATOR_LOGIC_EQ,  // ==
					OPERATOR_LOGIC_NE,  // !=
					OPERATOR_LOGIC_GE,  // >=
					OPERATOR_LOGIC_LE,  // <=
					OPERATOR_LOGIC_GREATER, // >
					OPERATOR_LOGIC_LESS,    // <
					OPERATOR_COMPARE,		// <=>
					OPERATOR_LOGIC_NOT,     // !

					OPERATOR_INC,     // ++
					OPERATOR_DEC,     // --

					OPERATOR_QUESTION,  // ?
					OPERATOR_COLON,     // :

					OPERATOR_IN,		// in
					OPERATOR_IS,		// is
					OPERATOR_AS,		// as
					OPERATOR_LENGTH,	// #

					OPERATOR_BIT_AND, // &
					OPERATOR_BIT_OR,  // |
					OPERATOR_BIT_XOR, // ^
					OPERATOR_BIT_NOT, // ~
					OPERATOR_ADD, // +
					OPERATOR_SUB, // -
					OPERATOR_MUL, // *
					OPERATOR_DIV, // /
					OPERATOR_MOD, // %
					OPERATOR_LSHIFT, // <<
					OPERATOR_RSHIFT, // >>
					OPERATOR_POW, // **

					OPERATOR_BIT_AND_ASSIGN, // &=
					OPERATOR_BIT_OR_ASSIGN,  // |=
					OPERATOR_BIT_XOR_ASSIGN, // ^=
					OPERATOR_BIT_NOT_ASSIGN, // ~=
					OPERATOR_ADD_ASSIGN, // +=
					OPERATOR_SUB_ASSIGN, // -=
					OPERATOR_MUL_ASSIGN, // *=
					OPERATOR_DIV_ASSIGN, // /=
					OPERATOR_MOD_ASSIGN, // %=
					OPERATOR_LSHIFT_ASSIGN, // <<=
					OPERATOR_RSHIFT_ASSIGN, // >>=
					OPERATOR_POW_ASSIGN, // **=

					OPERATOR_ASSIGN, // =

					OPERATOR_RESERVED,

					OPERATOR_END,

					ERROR_TOKEN
				};

				class TextData
				{
				protected:

					~TextData();

				public:

					OS * allocator;
					String filename;
					bool is_real_file;
					Vector<String> lines;

					int ref_count;

					TextData(OS*);

					TextData * retain();
					void release();
				};

				class TokenData
				{
				protected:

					union
					{
						// OS_INT int_value;
						OS_FLOAT float_value;
					};

					~TokenData();

				public:

					TextData * text_data;

					String str;
					int line, pos;
					int ref_count;
					TokenType type;

					OS * getAllocator() const;

					OS_FLOAT getFloat() const;

					TokenData(TextData * text_data, const String& p_str, TokenType p_type, int p_line, int p_pos);

					TokenData * retain();
					void release();

					void setFloat(OS_FLOAT value);

					operator String () const { return str; }

					bool isTypeOf(TokenType tokenType) const;
				};

			protected:

				struct Settings
				{
					bool save_comments;
				} settings;

				OS * allocator;
				TextData * text_data;

				int cur_line, cur_pos;

				Vector<TokenData*> tokens;
				Error error;

				struct OperatorDesc
				{
					TokenType type;
					const OS_CHAR * name;
					int len;
				};

				static const int operator_count;
				static OperatorDesc operator_desc[];
				static bool operator_initialized;

				struct InitOperators
				{
					InitOperators();
				};
				static InitOperators init_operators;

				static int compareOperatorDesc(const void * a, const void * b) ;
				static void initOperatorsTable();

				TokenData * addToken(const String& token, TokenType type, int line, int pos OS_DBG_FILEPOS_DECL);

				bool parseFloat(const OS_CHAR *& str, OS_FLOAT& fval, bool parse_end_spaces);
				bool parseLines(OS_ESourceCodeType source_code_type, bool check_utf8_bom);

			public:

				Tokenizer(OS*);
				~Tokenizer();

				OS * getAllocator();
				TextData * getTextData() const { return text_data; }

				bool isError() const { return error != ERROR_NOTHING; }
				Error getErrorCode() const { return error; }
				int getErrorLine() const { return cur_line; }
				int getErrorPos() const { return cur_pos; }

				static const OS_CHAR * getTokenTypeName(TokenType tokenType);

				String getFilename() const { return text_data->filename; }
				String getLineString(int i) const { return text_data->lines[i]; }
				int getNumLines() const { return text_data->lines.count; }

				bool getSettingSaveComment() const { return settings.save_comments; }
				void setSettingSaveComment(bool value){ settings.save_comments = value; }

				bool parseText(const OS_CHAR * text, int len, const String& filename, bool is_real_file, OS_ESourceCodeType source_code_type, bool check_utf8_bom);

				int getNumTokens() const { return tokens.count; }
				TokenData * getToken(int i) const { return tokens[i]; }
				TokenData * removeToken(int i);
				void insertToken(int i, TokenData * token OS_DBG_FILEPOS_DECL);
			};

			typedef Tokenizer::TokenType TokenType;
			typedef Tokenizer::TokenData TokenData;
			typedef Tokenizer::TextData TextData;

			struct Property;
			struct Value;
			struct Table
			{
				struct IteratorState
				{
					Table * table;
					Property * prop;
					IteratorState * next;
					bool ascending;

					IteratorState();
					~IteratorState();
				};

				Property ** heads;
				int head_mask;
				int count;
				OS_INT next_index;

				Property * first, * last;
				IteratorState * iterators;

				Table();    
				~Table();

				// Property * get(const Value& index);
				Property * get(const Value& index, int index_type);

				bool containsIterator(IteratorState*);
				void addIterator(IteratorState*);
				void removeIterator(IteratorState*);
			};

			struct GCValue
			{
				int value_id;
				int ref_count;
				int external_ref_count; // used to detect external strings, 
					// they are not conected to values root tree but should not be freed, 
					// ref_count could not be used to detect this situation
				GCValue * prototype;
				GCValue * hash_next;

				GCValue * hash_next_free_candidate;
				// int mark_created_values;
				
				Table * table;
				GCStringValue * name;

				int gc_step_type;

				OS_EValueType type;
				// bool is_object_instance;
				bool is_destructor_called;

				// EGCColor gc_color;

				GCValue();
				virtual ~GCValue();
			};

			struct GCObjectValue: public GCValue
			{
			};

			struct GCArrayValue;

			struct GCStringValue: public GCValue
			{
#ifdef OS_DEBUG
				OS_CHAR * str;
#endif
				int data_size;
				int hash;

				GCStringValue * hash_next_ref;

				GCStringValue(int p_data_size);
				~GCStringValue();

				int getDataSize() const { return data_size; }
				int getLen() const { return data_size/sizeof(OS_CHAR); }
				OS_CHAR * toChar() const { return (OS_CHAR*)(this + 1); }
				OS_BYTE * toBytes() const { return (OS_BYTE*)(this + 1); }
				void * toMemory() const { return (void*)(this + 1); }

				static GCStringValue * allocAndPush(OS*, int hash, const void *, int data_size OS_DBG_FILEPOS_DECL);
				static GCStringValue * allocAndPush(OS*, int hash, const void * buf1, int len1, const void * buf2, int len2 OS_DBG_FILEPOS_DECL);
				static GCStringValue * allocAndPush(OS*, GCStringValue * a, GCStringValue * b OS_DBG_FILEPOS_DECL);

				bool isNumber(OS_NUMBER*) const;
				bool isNumberRadix(int radix, OS_NUMBER*) const;
				
				OS_NUMBER toNumber() const;
				OS_NUMBER toNumberRadix(int radix) const;

				int cmp(GCStringValue*) const;
				int cmp(const OS_CHAR*) const;
				int cmp(const OS_CHAR*, int len) const;

				bool isEqual(int hash, const void * b, int size) const;
				bool isEqual(int hash, const void * buf1, int size1, const void * buf2, int size2) const;

				void calcHash();
			};

			struct GCUserdataValue: public GCObjectValue
			{
				int crc;
				void * ptr;
				OS_UserdataDtor dtor;
				void * user_param;
				GCUserdataValue * hash_next_ref;
				// ~GCUserdataValue();
			};

			struct GCCFunctionValue: public GCValue
			{
				OS_CFunction func;
				void * user_param;
				int num_closure_values;
				int cfunc_hash;
				GCCFunctionValue * hash_next_ref;
			};

			struct GCFunctionValue;

#if defined(_MSC_VER) && defined(_M_IX86) && !defined(OS_NUMBER_TO_INT_ASM_DISABLED)
#define OS_NUMBER_TO_INT(i, _n) do { OS_FLOAT n = (OS_FLOAT)(_n); __asm { __asm fld n __asm fistp i } }while(false)
#else
#define OS_NUMBER_TO_INT(i, n) i = (int)(n)
#endif

// #define OS_VALUE_MARK_PLAIN		(0<<7)
#define OS_VALUE_MARK_GC_TYPE	(1<<7)
#define OS_VALUE_MIN_GC_TYPE	ObjectScript::OS_VALUE_TYPE_STRING

/* Microsoft compiler on a Pentium (32 bit) ? */
#if defined(_MSC_VER) && defined(_M_IX86)

#define OS_NUMBER_IEEEENDIAN	0

#ifndef OS_NUMBER_NAN_TRICK_DISABLED
#define OS_NUMBER_NAN_TRICK
#endif // OS_NUMBER_NAN_TRICK_DISABLED

/* pentium 32 bits? */
#elif defined(__i386__) || defined(__i386) || defined(__X86__)

#define OS_NUMBER_IEEEENDIAN	1

#ifndef OS_NUMBER_NAN_TRICK_DISABLED
#define OS_NUMBER_NAN_TRICK
#endif // OS_NUMBER_NAN_TRICK_DISABLED

#elif defined(__x86_64)

#define OS_NUMBER_IEEEENDIAN	0

#elif defined(__POWERPC__) || defined(__ppc__)

#define OS_NUMBER_IEEEENDIAN	1

#else

#endif // OS_NUMBER_IEEEENDIAN & OS_NUMBER_NAN_TRICK

			union ValueUnion
			{
				int boolean;
				// int value_id;
				GCValue * value;
				GCObjectValue * object;
				GCArrayValue * arr;
				GCStringValue * string;
				GCUserdataValue * userdata;
				GCFunctionValue * func;
				GCCFunctionValue * cfunc;
#ifndef OS_NUMBER_NAN_TRICK
				OS_NUMBER number;
#endif
			};

			struct Value
			{
#ifndef OS_NUMBER_NAN_TRICK
				struct {
					ValueUnion v; 
					int type;
				} u;

#define OS_VALUE_VARIANT(a)	(a).u.v
#define OS_VALUE_NUMBER(a)	(a).u.v.number
#define OS_VALUE_TAGGED_TYPE(a)	(a).u.type
#define OS_VALUE_TYPE(a)	(OS_VALUE_TAGGED_TYPE(a) & ~OS_VALUE_MARK_GC_TYPE)

#define OS_IS_VALUE_GC(a) (OS_VALUE_TAGGED_TYPE(a) & OS_VALUE_MARK_GC_TYPE)
#define OS_IS_VALUE_NUMBER(a)	(OS_VALUE_TYPE(a) == OS_VALUE_TYPE_NUMBER)
#define OS_MAKE_VALUE_TAGGED_TYPE(t)	((t) < OS_VALUE_MIN_GC_TYPE ? (t) : (t) | OS_VALUE_MARK_GC_TYPE)

#define OS_SET_VALUE_NUMBER(a, n)	((OS_VALUE_NUMBER(a) = (OS_NUMBER)(n)), OS_SET_VALUE_TYPE(a, OS_VALUE_TYPE_NUMBER))
#define OS_SET_VALUE_TYPE(a, t)		(OS_VALUE_TAGGED_TYPE(a) = OS_MAKE_VALUE_TAGGED_TYPE(t))
#define OS_SET_VALUE_TYPE_GC(a, t)	do{ OS_ASSERT((t) >= OS_VALUE_MIN_GC_TYPE); (OS_VALUE_TAGGED_TYPE(a) = (t) | OS_VALUE_MARK_GC_TYPE); }while(false)
#define OS_SET_VALUE_NULL(a) do{ Value& local_value_7 = (a); OS_VALUE_VARIANT(local_value_7).value = NULL; OS_SET_VALUE_TYPE(local_value_7, OS_VALUE_TYPE_NULL); }while(false)
#define OS_SET_NULL_VALUES(a, c) do{ Value * local_value_9 = (a); for(int count = c; count > 0; --count, ++local_value_9) OS_SET_VALUE_NULL(*local_value_9); }while(false)

#elif !defined(OS_NUMBER_IEEEENDIAN)
#error option 'OS_NUMBER_NAN_TRICK' needs 'OS_NUMBER_IEEEENDIAN'
#else

#define OS_NUMBER_NAN_MASK	0x7FFFFF00
#define OS_NUMBER_NAN_MARK	0x7FF7A500

#define OS_VALUE_VARIANT(a)	(a).u.i.v
#define OS_VALUE_NUMBER(a)	(a).u.number
#define OS_VALUE_TAGGED_TYPE(a)	(a).u.i.type
#define OS_VALUE_TYPE(a)	(OS_IS_VALUE_NUMBER(a) ? OS_VALUE_TYPE_NUMBER : OS_VALUE_TAGGED_TYPE(a) & (0xff & ~OS_VALUE_MARK_GC_TYPE))

#define OS_IS_VALUE_GC(a) (OS_IS_VALUE_NUMBER(a) ? false : OS_VALUE_TAGGED_TYPE(a) & OS_VALUE_MARK_GC_TYPE)
#define OS_IS_VALUE_NUMBER(a)	((OS_VALUE_TAGGED_TYPE(a) & OS_NUMBER_NAN_MASK) != OS_NUMBER_NAN_MARK)
#define OS_MAKE_VALUE_TAGGED_TYPE(t)	((t) < OS_VALUE_MIN_GC_TYPE ? (t) | OS_NUMBER_NAN_MARK : (t) | OS_NUMBER_NAN_MARK | OS_VALUE_MARK_GC_TYPE)

#define OS_SET_VALUE_NUMBER(a, n)	(OS_VALUE_NUMBER(a) = (OS_NUMBER)(n))
// #define OS_SET_VALUE_OBJECT(a, v)	(OS_VALUE_VARIANT(a) = (v))
#define OS_SET_VALUE_TYPE(a, t)		(OS_VALUE_TAGGED_TYPE(a) = OS_MAKE_VALUE_TAGGED_TYPE(t))
#define OS_SET_VALUE_TYPE_GC(a, t)	do{ OS_ASSERT((t) >= OS_VALUE_MIN_GC_TYPE); (OS_VALUE_TAGGED_TYPE(a) = (t) | OS_NUMBER_NAN_MARK | OS_VALUE_MARK_GC_TYPE); OS_ASSERT(OS_VALUE_TYPE(a) == (t)); }while(false)
#define OS_SET_VALUE_NULL(a) do{ Value& local_value_7 = (a); OS_VALUE_VARIANT(local_value_7).value = NULL; OS_SET_VALUE_TYPE(local_value_7, OS_VALUE_TYPE_NULL); }while(false)
#define OS_SET_NULL_VALUES(a, c) do{ Value * local_value_9 = (a); for(int count = c; count > 0; --count, ++local_value_9) OS_SET_VALUE_NULL(*local_value_9); }while(false)

#if OS_NUMBER_IEEEENDIAN == 0
				union {
					struct { ValueUnion v; int type; } i;
					OS_NUMBER number;
				} u;
#else
				union {
					struct { int type; ValueUnion v; } i;
					OS_NUMBER number;
				} u;
#endif
#endif // OS_NUMBER_NAN_TRICK

				struct Valid {};

				Value();
				Value(bool);
				Value(OS_INT32);
				Value(OS_INT64);
				Value(float);
				Value(double);
				Value(long double);
				Value(GCValue*);
				Value(GCValue*, const Valid&);
				Value(const String&);

				Value& operator=(GCValue*);
				Value& operator=(bool);
				Value& operator=(OS_INT32);
				Value& operator=(OS_INT64);
				Value& operator=(float);
				Value& operator=(double);
				Value& operator=(long double);

#ifdef OS_NUMBER_NAN_TRICK
				// Value& operator=(const Value& b){ OS_SET_VALUE_NUMBER(*this, OS_VALUE_NUMBER(b)); return *this;  }
#endif
				
				void clear();

				GCValue * getGCValue() const;

				bool operator==(const Value& b) const
				{
					return OS_MEMCMP(this, &b, sizeof(b)) == 0;
				}

				bool isNull() const;
				bool isFunction() const;
				bool isUserdata() const;
			};

			struct GCArrayValue: public GCValue
			{
				Vector<Value> values;
			};

			class Program;
			struct FunctionDecl;
			struct Locals;
			struct GCFunctionValue: public GCValue
			{
				Program * prog; // retained
				FunctionDecl * func_decl;
				Value env;
				Value self;
				Locals * locals; // retained

				GCFunctionValue();
				~GCFunctionValue();
			};

			static bool isEqual(const Value& index, int hash, const void * b, int size);
			static bool isEqual(const Value& index, int hash, const void * buf1, int size1, const void * buf2, int size2) ;
			static int getValueHash(const Value& index, int index_type);

			struct Property
			{
				Value index;
				Value value;

				Property * hash_next;
				Property * prev, * next;

				Property(const Value& index, const Value& value);
				~Property();
			};

			enum {
				OP_MULTI_GET_ARGUMENTS,
				OP_MULTI_GET_REST_ARGUMENTS,
				OP_MULTI_SUPER,
				OP_MULTI_DEBUGGER,
				OP_MULTI_THROW,
			};

			enum OpcodeType
			{
				// OP_NOP,
				OP_NEW_FUNCTION,
				OP_NEW_ARRAY,
				OP_NEW_OBJECT,
				OP_RETURN,
				OP_JUMP,
				OP_MULTI,
				OP_MOVE,
				OP_MOVE2,
				OP_GET_XCONST,

				OP_SUPER_CALL,
				OP_CALL,
				OP_CALL_METHOD,

#ifdef OS_TAIL_CALL_ENABLED
				OP_TAIL_CALL,
				OP_TAIL_CALL_METHOD,
#endif
				OP_INIT_ITER,

				OP_GET_PROPERTY,
				OP_SET_PROPERTY,
				OP_INIT_PROPERTY,

				OP_GET_UPVALUE,
				OP_SET_UPVALUE,

				OP_LOGIC_PTR_EQ,
				OP_LOGIC_EQ,
				OP_LOGIC_GREATER,
				OP_LOGIC_GE,
				OP_LOGIC_BOOL,

				OP_BIT_AND, // &
				OP_BIT_OR,  // |
				OP_BIT_XOR, // ^

				OP_COMPARE, // <=>
				OP_ADD, // +
				OP_SUB, // -
				OP_MUL, // *
				OP_DIV, // /
				OP_MOD, // %
				OP_LSHIFT, // <<
				OP_RSHIFT, // >>
				OP_POW, // **

				OP_BIT_NOT,
				OP_PLUS,
				OP_MINUS,

				// -----

				OP_NUMBER_LOGIC_EQ,
				OP_NUMBER_LOGIC_GREATER,
				OP_NUMBER_LOGIC_GE,

				OP_NUMBER_BIT_AND, // &
				OP_NUMBER_BIT_OR,  // |
				OP_NUMBER_BIT_XOR, // ^

				OP_NUMBER_ADD, // +
				OP_NUMBER_SUB, // -
				OP_NUMBER_MUL, // *
				OP_NUMBER_DIV, // /
				OP_NUMBER_MOD, // %
				OP_NUMBER_LSHIFT, // <<
				OP_NUMBER_RSHIFT, // >>
				OP_NUMBER_POW, // **

				OP_NUMBER_ADD_LC, // +
				OP_NUMBER_SUB_LC, // -

				OP_NUMBER_ADD_LL, // +
				OP_NUMBER_SUB_LL, // -

				OPCODE_COUNT	// max is 64
			};

			class Compiler
			{
			public:

				enum ExpressionType
				{
					EXP_TYPE_UNKNOWN,
					EXP_TYPE_NOP,
					EXP_TYPE_NEW_LOCAL_VAR,
					EXP_TYPE_SCOPE,
					EXP_TYPE_LOOP_SCOPE,
					EXP_TYPE_FOR_LOOP_SCOPE,
					EXP_TYPE_CODE_LIST,
					EXP_TYPE_NAME, // temp
					EXP_TYPE_POP_VALUE,
					EXP_TYPE_SUPER_CALL,
					EXP_TYPE_CALL,
					EXP_TYPE_CALL_AUTO_PARAM,
					EXP_TYPE_CALL_DIM, // temp
					EXP_TYPE_VALUE,
					EXP_TYPE_PARAMS,
					EXP_TYPE_FUNCTION,
					EXP_TYPE_EXTENDS,
					EXP_TYPE_DELETE,
					EXP_TYPE_RETURN,
					EXP_TYPE_BREAK,
					EXP_TYPE_CONTINUE,
					EXP_TYPE_DEBUGGER,
					EXP_TYPE_DEBUG_LOCALS,

					EXP_TYPE_IF,
					EXP_TYPE_QUESTION,

					EXP_TYPE_TRY_CATCH,
					EXP_TYPE_THROW,

					EXP_TYPE_ARRAY,

					EXP_TYPE_OBJECT,
					EXP_TYPE_OBJECT_SET_BY_NAME,
					EXP_TYPE_OBJECT_SET_BY_INDEX,
					EXP_TYPE_OBJECT_SET_BY_EXP,
					EXP_TYPE_OBJECT_SET_BY_AUTO_INDEX,
					EXP_TYPE_OBJECT_CREATE_CONST,

					EXP_TYPE_SUPER,

					EXP_TYPE_GET_THIS,
					EXP_TYPE_GET_ARGUMENTS,
					EXP_TYPE_GET_REST_ARGUMENTS,

					EXP_TYPE_GET_LOCAL_VAR, // sets local variable (not visible from script???)
					EXP_TYPE_GET_LOCAL_VAR_AUTO_CREATE,
					EXP_TYPE_SET_LOCAL_VAR,
					EXP_TYPE_SET_LOCAL_VAR_NO_POP,
					
					EXP_TYPE_SET_LOCAL_VAR_BY_BIN_OPERATOR_LOCALS,
					EXP_TYPE_SET_LOCAL_VAR_BY_BIN_OPERATOR_LOCAL_AND_NUMBER,

					EXP_TYPE_GET_ENV_VAR, // sets variable visible from script
					EXP_TYPE_GET_ENV_VAR_AUTO_CREATE,
					EXP_TYPE_SET_ENV_VAR,
					EXP_TYPE_SET_ENV_VAR_NO_POP,

					EXP_TYPE_INDIRECT, // temp

					EXP_TYPE_GET_PROPERTY,
					EXP_TYPE_GET_PROPERTY_AUTO_CREATE,
					EXP_TYPE_SET_PROPERTY,
					EXP_TYPE_INIT_PROPERTY,
					EXP_TYPE_SET_PROPERTY_NO_POP,

					EXP_TYPE_GET_THIS_PROPERTY_BY_STRING,

					EXP_TYPE_GET_PROPERTY_BY_LOCALS,
					EXP_TYPE_GET_PROPERTY_BY_LOCAL_AND_NUMBER,
					EXP_TYPE_SET_PROPERTY_BY_LOCALS_AUTO_CREATE,

					EXP_TYPE_GET_SET_PROPERTY_BY_LOCALS_AUTO_CREATE,
					
					EXP_TYPE_SET_DIM,
					EXP_TYPE_SET_DIM_NO_POP,

					EXP_TYPE_CALL_METHOD,
					EXP_TYPE_INIT_ITER,

					EXP_TYPE_TAIL_CALL,
					EXP_TYPE_TAIL_CALL_METHOD,

					EXP_TYPE_CONST_NULL,
					EXP_TYPE_CONST_NUMBER,
					EXP_TYPE_CONST_STRING,
					EXP_TYPE_CONST_TRUE,
					EXP_TYPE_CONST_FALSE,

					EXP_TYPE_LOGIC_BOOL,    // !!
					EXP_TYPE_LOGIC_NOT,     // !
					EXP_TYPE_BIT_NOT,		// ~
					EXP_TYPE_PLUS,			// +
					EXP_TYPE_MINUS,			// -
					EXP_TYPE_LENGTH,		// #
					EXP_TYPE_IN,			// in
					EXP_TYPE_IS,			// is
					EXP_TYPE_AS,			// as

					EXP_TYPE_BIN_OPERATOR_BY_LOCALS,
					EXP_TYPE_BIN_OPERATOR_BY_LOCAL_AND_NUMBER,

					EXP_TYPE_CONCAT, // ..
					EXP_TYPE_BEFORE_INJECT_VAR, // ..
					EXP_TYPE_AFTER_INJECT_VAR, // ..

					EXP_TYPE_LOGIC_AND, // &&
					EXP_TYPE_LOGIC_OR,  // ||

					EXP_TYPE_LOGIC_PTR_EQ,  // ===
					EXP_TYPE_LOGIC_PTR_NE,  // !==
					EXP_TYPE_LOGIC_EQ,  // ==
					EXP_TYPE_LOGIC_NE,  // !=
					EXP_TYPE_LOGIC_GE,  // >=
					EXP_TYPE_LOGIC_LE,  // <
					EXP_TYPE_LOGIC_GREATER, // >
					EXP_TYPE_LOGIC_LESS,    // <

					EXP_TYPE_PRE_INC,     // ++
					EXP_TYPE_PRE_DEC,     // --

					EXP_TYPE_POST_INC,    // ++
					EXP_TYPE_POST_DEC,    // --

					EXP_TYPE_BIT_AND, // &
					EXP_TYPE_BIT_OR,  // |
					EXP_TYPE_BIT_XOR, // ^

					EXP_TYPE_BIT_AND_ASSIGN, // &=
					EXP_TYPE_BIT_OR_ASSIGN,  // |=
					EXP_TYPE_BIT_XOR_ASSIGN, // ^=
					EXP_TYPE_BIT_NOT_ASSIGN, // ~=

					EXP_TYPE_COMPARE, // <=>
					EXP_TYPE_ADD, // +
					EXP_TYPE_SUB, // -
					EXP_TYPE_MUL, // *
					EXP_TYPE_DIV, // /
					EXP_TYPE_MOD, // %
					EXP_TYPE_LSHIFT, // <<
					EXP_TYPE_RSHIFT, // >>
					EXP_TYPE_POW, // **

					EXP_TYPE_ADD_ASSIGN, // +=
					EXP_TYPE_SUB_ASSIGN, // -=
					EXP_TYPE_MUL_ASSIGN, // *=
					EXP_TYPE_DIV_ASSIGN, // /=
					EXP_TYPE_MOD_ASSIGN, // %=
					EXP_TYPE_LSHIFT_ASSIGN, // <<=
					EXP_TYPE_RSHIFT_ASSIGN, // >>=
					EXP_TYPE_POW_ASSIGN, // **=

					EXP_TYPE_ASSIGN,

					EXP_TYPE_GET_UPVALUE,
					EXP_TYPE_SET_UPVALUE,
					EXP_TYPE_SET_UPVALUE_NO_POP,

					EXP_TYPE_MOVE,
					EXP_TYPE_GET_XCONST,

					EXP_TYPE_SWITCH_SCOPE, // switch() {
					EXP_TYPE_CASE,         // case:
					EXP_TYPE_CASE_DEFAULT, // default:
					EXP_TYPE_CASE_JUMP,    // internal - used to generate JUMP opcode
				};

			protected:

				friend class Program;

				struct Expression;
				struct ExpressionList: public Vector<Expression*>
				{
					OS * allocator;

					ExpressionList(OS*);
					~ExpressionList();

					bool isValue() const;
					bool isClear() const;
					bool isWriteable() const;

					Expression * add(Expression* OS_DBG_FILEPOS_DECL);
					Expression * insert(int i, Expression* OS_DBG_FILEPOS_DECL);
					Expression * removeIndex(int i);
					Expression * removeLast();

					void swap(ExpressionList&);
				};

				enum ELocalVarScopeType
				{
					LOCAL_GENERIC,
					LOCAL_PARAM,
					LOCAL_TEMP
				};

				enum ECompiledValueType
				{
					CVT_UNKNOWN,
					CVT_NUMBER,
					CVT_DYNAMIC
				};

				struct LocalVarDesc
				{
					OS_U16 up_count;
					OS_U16 up_scope_count;
					OS_U16 index;
					ELocalVarScopeType scope_type;
					ECompiledValueType type;

					LocalVarDesc();
				};

				struct Scope;
				struct Expression
				{
					TokenData * token;
					ExpressionList list;
					LocalVarDesc local_var;
					OS_U16 active_locals;
					OS_U16 ret_values;
					struct {
						OS_INT16 a, b, c;
					} slots;
					ExpressionType type;
					
					Expression(ExpressionType type, TokenData*);
					Expression(ExpressionType type, TokenData*, Expression * e1 OS_DBG_FILEPOS_DECL);
					Expression(ExpressionType type, TokenData*, Expression * e1, Expression * e2 OS_DBG_FILEPOS_DECL);
					Expression(ExpressionType type, TokenData*, Expression * e1, Expression * e2, Expression * e3 OS_DBG_FILEPOS_DECL);
					virtual ~Expression();

					OS * getAllocator(){ return list.allocator; }

					OS_NUMBER toNumber();
					OS_INT toInt();
					String toString();

					bool isConstValue() const;
					bool isValue() const;
					bool isClear() const;
					bool isWriteable() const;
					bool isOperator() const;
					bool isUnaryOperator() const;
					bool isBinaryOperator() const;
					bool isAssignOperator() const;
					static bool isAssignOperator(ExpressionType);
					bool isLogicOperator() const;

					String getSlotStr(Compiler * compiler, Scope * scope, int slot_num, int up_count = 0);
					void debugPrint(Buffer&, Compiler * compiler, Scope * scope, int depth);
				};

				struct Scope: public Expression
				{
					Scope * parent;
					Scope * function;

					struct LocalVar
					{
						String name;
						int index;
						ECompiledValueType type;
						bool upvalue;

						LocalVar(const String& name, int index, ECompiledValueType type = CVT_UNKNOWN);
					};

					struct LocalVarCompiled
					{
						int cached_name_index;
						int start_code_pos;
						int end_code_pos;
						bool upvalue;

						LocalVarCompiled();
					};

					enum ELoopBreakType
					{
						LOOP_CONTINUE,
						LOOP_BREAK
					};

					struct LoopBreak
					{
						int pos;
						ELoopBreakType type;
					};

					struct TryBlock
					{
						int start_code_pos;
						int end_code_pos;
						int catch_var_index;
					};

					struct SwitchCaseLabel
					{
						TokenData * key; // used as key to search label
						Expression * exp;
						Expression * jump_exp;
						int to_pos;
						int from_pos;
					};

					// used by function scope
					int prog_func_index;
					Vector<LocalVar> locals;
					Vector<LocalVarCompiled> locals_compiled;
					Vector<TryBlock> try_blocks;
					int num_params;
					int num_locals;
					int opcodes_pos;
					int opcodes_size;
					int max_up_count;
					int func_depth;
					int func_index;
					int num_local_funcs;

					int stack_size;
					int stack_cur_size;

					Vector<LoopBreak> loop_breaks;

					Vector<SwitchCaseLabel> case_labels;

					bool parser_started;

					Scope(Scope * parent, ExpressionType, TokenData*);
					virtual ~Scope();

					LocalVar& getLocalVar(const LocalVarDesc&);

					bool addLoopBreak(int pos, ELoopBreakType);
					void fixLoopBreaks(Compiler*, int scope_start_pos, int scope_end_pos);

					Scope * findLoopScope();

					bool addCaseLabel(TokenData  * key, Expression * exp);
					// set address of JUMP opcode
					bool setCaseLabelJump(TokenData  * key, int pos, Compiler* cmp);
					// set address of target opcode
					bool setCaseLabelPos(TokenData  * key, int pos, Compiler* cmp);
					// set expression to evaluate to jump to label ("default" element has no expression)
					// bool setCaseLabelExp(TokenData * key, Expression * exp);
					// returns parent Scope object if it has type EXP_TY_SWITCH_SCOPE
					Scope* getSwitchScope();
					// finds EXP_TY_SWITCH_SCOPE in parent's scopes
					Scope* findSwitchScope();

					void addPreVars();
					void addPostVars();
					void addLocalVar(const String& name, ECompiledValueType type = CVT_UNKNOWN);
					void addLocalVar(const String& name, LocalVarDesc&, ECompiledValueType type = CVT_UNKNOWN);

					int allocTempVar();
					void popTempVar(int count = 1);

					void addTryBlock(int start, int end, Scope * catch_block);
				};

				enum ErrorType {
					ERROR_NOTHING,
					ERROR_SYNTAX,
					ERROR_NESTED_ROOT_BLOCK,
					ERROR_LOCAL_VAL_NOT_DECLARED,
					ERROR_VAR_NAME,
					ERROR_EXPECT_TOKEN_TYPE,
					ERROR_EXPECT_TOKEN_STR,
					ERROR_EXPECT_TOKEN,
					ERROR_EXPECT_VALUE,
					ERROR_EXPECT_WRITEABLE,
					ERROR_EXPECT_GET_OR_SET,
					ERROR_EXPECT_EXPRESSION,
					ERROR_EXPECT_FUNCTION_SCOPE,
					ERROR_EXPECT_CODE_SEP_BEFORE_NESTED_BLOCK,
					ERROR_EXPECT_SWITCH_SCOPE,
					ERROR_EXPECT_LOOP_SCOPE,
					ERROR_FINISH_BINARY_OP,
					ERROR_FINISH_UNARY_OP,
				};

				enum OpcodeLevel {
					OP_LEVEL_NOTHING = -1,
					OP_LEVEL_0,
					OP_LEVEL_1, // = += -= *= /= %=
					OP_LEVEL_1_1, // ,
					OP_LEVEL_2, // ?:
					OP_LEVEL_3, // ||
					OP_LEVEL_4, // &&
					OP_LEVEL_5, // ..
					OP_LEVEL_6, // == !=
					OP_LEVEL_7, // < <= > >=
					OP_LEVEL_8, // |
					OP_LEVEL_9, // & ^
					OP_LEVEL_10, // << >> >>>
					OP_LEVEL_11, // + -
					OP_LEVEL_12, // * / %
					OP_LEVEL_13, // ** in as is
					OP_LEVEL_14, // ++ --
					OP_LEVEL_15, // unary ! ~ + #
					OP_LEVEL_16, // .

					OP_LEVEL_COUNT
				};

				OS * allocator;
				Tokenizer * tokenizer;

				ErrorType error;
				TokenData * error_token;
				TokenType expect_token_type;
				String expect_token;

				TokenData * recent_token;
				int next_token_index;

				// String recent_printed_filename;
				TextData * recent_printed_text_data;
				int recent_printed_line;

				// code generation
				struct DebugInfoItem
				{
					OS_U32 line;
					OS_U32 pos;
					DebugInfoItem(int line, int pos);
				};

				Table * prog_numbers_table;
				Table * prog_strings_table;
				Vector<OS_NUMBER> prog_numbers;
				Vector<String> prog_strings;
				Vector<Scope*> prog_functions;
				Vector<OS_U32> prog_opcodes;
				Vector<DebugInfoItem> prog_debug_info;
				int prog_filename_string_index;
				int prog_max_up_count;
				int prog_optimize_offs;

				bool isError();
				void resetError();
				void setError();
				void setError(ErrorType value, TokenData * error_token);
				void setError(TokenType expect_token_type, TokenData * error_token);
				void setError(const String& str, TokenData * error_token);

				void * malloc(int size OS_DBG_FILEPOS_DECL);

				TokenData * setNextTokenIndex(int i);
				TokenData * setNextToken(TokenData * token);
				TokenData * putNextTokenType(TokenType tokenType);
				TokenData * ungetToken();

				bool isNextTokens(TokenType * list, int count);
				bool isNextToken(TokenType t0);
				bool isNextTokens(TokenType t0, TokenType t1);
				bool isNextTokens(TokenType t0, TokenType t1, TokenType t2);
				bool isNextTokens(TokenType t0, TokenType t1, TokenType t2, TokenType t3);

				void deleteNops(ExpressionList& list);

				ExpressionType getUnaryExpressionType(TokenType);
				ExpressionType getExpressionType(TokenType);
				OpcodeLevel getOpcodeLevel(ExpressionType exp_type);

				TokenData * readToken();
				TokenData * getPrevToken();
				TokenData * expectToken(TokenType);
				TokenData * expectToken();

				struct Params
				{
					bool allow_root_blocks;
					bool allow_var_decl;
					bool allow_inline_nested_block;
					bool allow_binary_operator;
					bool allow_in_operator;
					bool allow_assing;
					bool allow_params;
					bool allow_auto_call;
					bool allow_call;
					bool allow_nop_result;

					Params();
					Params(const Params&);

					Params& setAllowRootBlocks(bool);
					Params& setAllowVarDecl(bool);
					Params& setAllowInlineNestedBlock(bool);
					Params& setAllowBinaryOperator(bool);
					Params& setAllowInOperator(bool);
					Params& setAllowAssign(bool);
					Params& setAllowParams(bool);
					Params& setAllowAutoCall(bool);
					Params& setAllowCall(bool);
					Params& setAllowNopResult(bool);
				};

				Expression * expectSingleExpression(Scope*, const Params& p);
				Expression * expectSingleExpression(Scope*, bool allow_nop_result = false, bool allow_inline_nested_block = false, bool allow_params = true);

				Expression * expectExpressionValues(Expression * exp, int ret_values, bool auto_no_values = false);
				Expression * newExpressionFromList(ExpressionList& list, int ret_values, bool auto_no_values = false);
				Expression * newAssingExpression(Scope * scope, Expression * var_exp, Expression * value_exp);
				Expression * newSingleValueExpression(Expression * exp);
				
				Expression * postCompileExpression(Scope * scope, Expression * exp);
				Expression * postCompilePass2(Scope * scope, Expression * exp);
				Expression * postCompileFixValueType(Scope * scope, Expression * exp);
				Expression * postCompilePass3(Scope * scope, Expression * exp);
				Expression * postCompileNewVM(Scope * scope, Expression * exp);
				void registerUpvalue(Scope * scope, Expression * exp);

				bool isVarNameValid(const String& name);

				Scope * expectTextExpression();
				Scope * expectCodeExpression(Scope*);
				Expression * expectFunctionExpression(Scope*);
				Expression * expectFunctionSugarExpression(Scope*);
				Expression * expectFunctionBlockExpression(Scope*);
				Expression * expectExtendsExpression(Scope*);
				Expression * expectDeleteExpression(Scope*);
				Expression * expectVarExpression(Scope*);
				Expression * expectObjectOrFunctionExpression(Scope*, const Params& p, bool allow_finish_exp = true);
				Expression * expectArrayExpression(Scope*, const Params& p);
				Expression * expectParamsExpression(Scope*);
				Expression * expectReturnExpression(Scope*);
				Expression * expectTryExpression(Scope*);
				Expression * expectThrowExpression(Scope*);
				
				enum EFilenameType {
					GET_FILENAME,
					GET_DIRNAME
				};
				
				Expression * expectFilenameExpression(Scope*, EFilenameType);
				
				Expression * expectIfExpression(Scope*);
				Expression * expectWhileExpression(Scope*);
				Expression * expectDoExpression(Scope*);
				Expression * expectSwitchExpression(Scope*);
				Expression * expectCaseExpression(Scope*);
				Expression * expectForExpression(Scope*);
				Expression * expectDebugLocalsExpression(Scope*);
				Expression * expectBracketExpression(Scope*, const Params& p);
				Expression * finishValueExpression(Scope*, Expression*, const Params& p);
				Expression * finishValueExpressionNoAutoCall(Scope*, Expression*, const Params& p);
				Expression * finishValueExpressionNoNextCall(Scope*, Expression*, const Params& p);
				Expression * finishBinaryOperator(Scope * scope, OpcodeLevel prev_level, Expression * exp, const Params& p, bool& is_finished); // bool allow_param, bool& is_finished);
				Expression * finishQuestionOperator(Scope*, TokenData * token, Expression * left_exp, Expression * right_exp);
				Expression * newBinaryExpression(Scope * scope, ExpressionType, TokenData*, Expression * left_exp, Expression * right_exp);

				bool findLocalVar(LocalVarDesc&, Scope * scope, const String& name, int active_locals, bool all_scopes, bool decl = false);

				void debugPrintSourceLine(Buffer& out, TokenData*);
				static const OS_CHAR * getExpName(ExpressionType, ECompiledValueType = CVT_UNKNOWN);

				int cacheString(Table * strings_table, Vector<String>& strings, const String& str);
				int cacheString(const String& str);
				int cacheDebugString(const String& str);
				int cacheNumber(OS_NUMBER);

				void writeJumpOpcode(int offs);
				void fixJumpOpcode(int offs, int pos);

				int getOpcodePos();
				int writeOpcode(OS_U32 opcode);
				int writeOpcode(OpcodeType opcode);
				int writeOpcodeABC(OpcodeType opcode, int a, int b = 0, int c = 0);
				int writeOpcodeABx(OpcodeType opcode, int a, int b);
				void writeOpcodeAt(OS_U32 opcode, int pos);

				bool writeOpcodes(Scope*, Expression*);
				bool writeOpcodes(Scope*, ExpressionList&, bool optimization_enabled = false);

				void writeJumpOpcodeOld(int offs);
				void fixJumpOpcodeOld(StreamWriter * writer, int offs, int pos);
				void fixJumpOpcodeOld(StreamWriter * writer, int offs, int pos, int opcode);

				bool writeOpcodesOld(Scope*, Expression*);
				bool writeOpcodesOld(Scope*, ExpressionList&);
				void writeDebugInfo(Expression*);
				bool saveToStream(StreamWriter * writer);

			public:

				static const int EXPRESSION_SIZE = sizeof(Expression);

				Compiler(Tokenizer*);
				virtual ~Compiler();

				bool compile(); // compile text and push text root function
			};

			struct FunctionDecl
			{
				struct LocalVar
				{
					String name;
					int start_code_pos;
					int end_code_pos;
					bool upvalue;

					LocalVar(const String&);
					~LocalVar();
				};

				struct TryBlock
				{
					int start_code_pos;
					int end_code_pos;
					int catch_var_index;
				};

#ifdef OS_DEBUG
				int prog_func_index;
#endif
				int prog_parent_func_index;
				LocalVar * locals;
				int stack_size;
				int num_locals;
				int num_params; // this included
				int max_up_count;
				int func_depth;
				int func_index; // in parent space
				int num_local_funcs;
				TryBlock * try_blocks;
				int num_try_blocks;
				int opcodes_pos;
				int opcodes_size;

				FunctionDecl(); // Program*);
				~FunctionDecl();
			};

			class Program
			{
			protected:

				int ref_count;

				virtual ~Program();

			public:

				OS * allocator;
				String filename;

				int num_strings;
				int num_numbers;

				Value * const_values;

				FunctionDecl * functions;
				int num_functions;

				Vector<OS_U32> opcodes;
				
				struct DebugInfoItem
				{
					OS_U32 line;
					OS_U32 pos;
					DebugInfoItem(int line, int pos);
				};
				Vector<DebugInfoItem> debug_info;

				Program(OS * allocator);

				Program * retain();
				void release();

				static OpcodeType getOpcodeType(Compiler::ExpressionType, Compiler::ECompiledValueType = Compiler::CVT_UNKNOWN);

				bool loadFromStream(StreamReader * reader);
				DebugInfoItem * getDebugInfo(int opcode_pos);

				void pushStartFunction();
			};

			enum {
				PRE_VAR_FUNC,
				PRE_VAR_THIS,
				// -----------------
				PRE_VARS
			};

			enum {
				POST_VAR_ENV,
				POST_VAR_GLOBALS,
			};

			enum {
				CONST_NULL,
				CONST_TRUE,
				CONST_FALSE,
				// -----------------
				CONST_STD_VALUES
			};

			struct Locals
			{
				int ref_count;
				int gc_step_type;

				Program * prog; // retained
				FunctionDecl * func_decl;

				Value * values;
				bool is_stack_locals;
				
				// int num_parents;

				Locals ** getParents();
				Locals * getParent(int i);
				void setParent(int i, Locals*);

				Locals * retain();
			};

			struct StackFunction
			{
				GCFunctionValue * func;
				GCValue * self_for_proto;

				Locals * locals;
				int num_params; // func + this + params

				GCArrayValue * arguments;
				GCArrayValue * rest_arguments;

				Vector<GCFunctionValue*> sub_funcs;
				
				int caller_stack_size;
				int locals_stack_pos;
				
				int need_ret_values;
				OS_U32 * opcodes;
			};

			/* struct StringRef
			{
				int string_hash;
				int string_value_id;
				StringRef * hash_next;
			}; */

			struct StringRefs
			{
				// StringRef ** heads;
				GCStringValue ** heads;
				int head_mask;
				int count;

				StringRefs();
				~StringRefs();
			};

			/* struct UserptrRef
			{
				int userptr_hash;
				int userptr_value_id;
				UserptrRef * hash_next;
			}; */

			struct UserptrRefs
			{
				GCUserdataValue ** heads;
				int head_mask;
				int count;

				UserptrRefs();
				~UserptrRefs();
			};

			/* struct CFuncRef
			{
				int cfunc_hash;
				int cfunc_value_id;
				CFuncRef * hash_next;
			}; */

			struct CFuncRefs
			{
				GCCFunctionValue ** heads;
				int head_mask;
				int count;

				CFuncRefs();
				~CFuncRefs();
			};

			struct Values
			{
				GCValue ** heads;
				int head_mask;
				int count;

				int next_id;

				Values();
				~Values();

				GCValue * get(int value_id);
			};

			OS * allocator;

			struct Strings
			{
				String __construct;
				String __destruct;
				String __instantiable;
				String __newinstance;
				String __callinstance;
				String __object;
				String __get;
				String __set;
				String __isset;
				String __getAt;
				String __setAt;
				String __issetAt;
				String __del;
				String __delAt;
				String __getempty;
				String __setempty;
				String __delempty;
				String __getdim;
				String __setdim;
				String __deldim;
				String __iter;
				// String __concat;

				String __cmp;
				String __bitand;
				String __bitor;
				String __bitxor;
				String __bitnot;
				String __plus;
				String __minus;
				String __len;
				String __add;
				String __sub;
				String __mul;
				String __div;
				String __mod;
				String __lshift;
				String __rshift;
				String __pow;

				String __rcmp;
				String __rbitand;
				String __rbitor;
				String __rbitxor;
				String __radd;
				String __rsub;
				String __rmul;
				String __rdiv;
				String __rmod;
				String __rlshift;
				String __rrshift;
				String __rpow;

				String func_unhandledException;
				String func_getFilename;
				String func_getDirname;
				String func_extends;
				String func_delete;
				String func_length;
				String func_in;
				String func_is;
				String func_as;
				String func_push;
				String func_valueOf;
				String func_clone;
				String func_concat;
				String func_echo;
				String func_require;
				String func_call;
				String func_core;
				String func_main;
				String func_defineConstProperty;

				String typeof_null;
				String typeof_boolean;
				String typeof_number;
				String typeof_string;
				String typeof_object;
				String typeof_array;
				String typeof_userdata;
				String typeof_function;

				String syntax_get;
				String syntax_set;
				String syntax_super;
				String syntax_is;
				String syntax_as;
				String syntax_extends;
				String syntax_delete;
				String syntax_prototype;
				String syntax_var;
				String syntax_local;
				String syntax_arguments;
				String syntax_function;
				String syntax_null;
				String syntax_true;
				String syntax_false;
				String syntax_return;
				String syntax_class;
				String syntax_enum;
				String syntax_switch;
				String syntax_case;
				String syntax_default;
				String syntax_if;
				String syntax_else;
				String syntax_elseif;
				String syntax_for;
				String syntax_in;
				String syntax_break;
				String syntax_continue;
				String syntax_try;
				String syntax_catch;
				String syntax_finally;
				String syntax_throw;
				String syntax_new;
				String syntax_while;
				String syntax_do;
				String syntax_const;
				String syntax_public;
				String syntax_protected;
				String syntax_private;
				String syntax_yield;
				String syntax_static;
				String syntax_debugger;
				String syntax_debuglocals;
				String syntax_line;
				String syntax_file;
				String syntax_dir;

				String var_globals;
				String var_func;
				String var_this;
				String var_env;
				String var_temp_prefix;

				String empty;

				Strings(OS * allocator);
			} * strings;

			Values values;
			int num_created_values;
			int num_destroyed_values;

			StringRefs string_refs;
			UserptrRefs userptr_refs;
			CFuncRefs cfunc_refs;

			// GCObjectValue * check_recursion;
			Value global_vars;
			Value user_pool;
			Value retain_pool;
			Value check_get_recursion;
			Value check_set_recursion;
			Value check_valueof_recursion;
			
			enum {
				PROTOTYPE_BOOL,
				PROTOTYPE_NUMBER,
				PROTOTYPE_STRING,
				PROTOTYPE_OBJECT,
				PROTOTYPE_ARRAY,
				PROTOTYPE_FUNCTION,
				PROTOTYPE_USERDATA,
				// -----------------
				PROTOTYPE_COUNT
			};

			GCObjectValue * prototypes[PROTOTYPE_COUNT];

			struct StackValues {
				Value * buf;
				int capacity;
				int count;

				StackValues();
				~StackValues();

				Value& operator[](int i)
				{
					OS_ASSERT(i >= 0 && i < count);
					return buf[i];
				}

				Value& lastElement()
				{
					OS_ASSERT(count > 0);
					return buf[count-1];
				}
			} stack_values;

			void reserveStackValues(int new_capacity);
			void growStackValues(int new_capacity);

			Vector<StackFunction> call_stack_funcs;
			int max_call_stack;

			StackFunction * stack_func;
			Value * stack_func_locals;
			int stack_func_env_index;
			Value * stack_func_prog_values;

			struct FreeCandidateValues
			{
				GCValue ** heads;
				int head_mask;
				int count;

				FreeCandidateValues();
				~FreeCandidateValues();

				GCValue * get(int value_id);
			};

			FreeCandidateValues gc_candidate_values;
			
			int gc_start_when_used_bytes;
			int gc_next_when_used_bytes;
			int gc_step_type;
			bool gc_in_progress;
			bool gc_fix_in_progress;

			void addFreeCandidateValue(GCValue * value);
			void registerFreeCandidateValue(GCValue * value);
			void unregisterFreeCandidateValue(GCValue * value);
			void deleteFreeCandidateValues();
			void gcFreeCandidateValues(bool full = false);
			void gcFull();

			void dumpValues(Buffer& out);
			void dumpValuesToFile(const OS_CHAR * filename);
			void appendQuotedString(Buffer& buf, const String& string);

			struct {
				bool create_text_opcodes;
				bool create_text_eval_opcodes;
				bool create_debug_info;
				bool create_compiled_file;
				bool primary_compiled_file;
				bool sourcecode_must_exist;
			} settings;

			enum {
				RAND_STATE_SIZE = 624
			};

			OS_U32 rand_state[RAND_STATE_SIZE+1];
			OS_U32 rand_seed;
			OS_U32 * rand_next;
			int rand_left;

			bool terminated;
			bool call_stack_overflow;
			int terminated_code;
			Value terminated_exception;

			int getRandSeed();
			void setRandSeed(int seed);
			void randReload();
			double getRand();
			double getRand(double up);
			double getRand(double min, double max);

			void * malloc(int size OS_DBG_FILEPOS_DECL);
			void free(void * p);

			struct DebugInfo
			{
				Program * prog;
				Program::DebugInfoItem * pos;

				bool isValid() const { return prog && pos; }
			};
			DebugInfo getDebugInfo();

			void errorDivisionByZero();

			void triggerValueDestructor(GCValue*);
			void clearValue(GCValue*);
			void deleteValue(GCValue*);
			void saveFreeCandidateValue(GCValue*);

			void retainValue(const Value&);
			void retainValue(GCValue*);
			void retainValues(const Value*, int count);
			void releaseValue(const Value&);
			void releaseValueAndClear(Value&);
			void releaseValue(GCValue*);
			template <class T> void releaseValueAndClear(T*& out)
			{
				if(out){
					if(gc_fix_in_progress && out->gc_step_type != gc_step_type){
						out = NULL;
						return;
					}
					OS_ASSERT(out->ref_count > 0);
					/* if(out->value_id >= 15622 && out->value_id <= 15622){
						int i = 0;
					} */
					if(!--out->ref_count){
						saveFreeCandidateValue(out);
					}
					out = NULL;
				}
			}
			void releaseValues(const Value*, int count);
			void releaseValuesAndClear(Value*, int count);
			void clearStackValues(Value*, int count);
			void setValue(Value& out, const Value& b);			
			template <class T, class T2> void setValue(T*& out, T2 * b)
			{
				if(out == b){
					return;
				}
				if(out){
					if(gc_fix_in_progress && out->gc_step_type != gc_step_type){
						int i = 0;
					}else{
						// release
						OS_ASSERT(out->ref_count > 0);
						/* if(out->value_id >= 15622 && out->value_id <= 15622){
							int i = 0;
						} */
						if(!--out->ref_count){
							saveFreeCandidateValue(out);
						}
					}
				}
				out = b;
				if(b){
					// retain
					/* if(b->value_id >= 15622 && b->value_id <= 15622){
						int i = 0;
					} */
					++b->ref_count;
				}
			}
			
#ifdef OS_DEBUG
			bool isValueUsed(GCValue*);
			bool isValueExist(GCValue*);
#endif

			GCFunctionValue * pushFunctionValue(StackFunction*, Program*, FunctionDecl*, Value env, Value self);
			void clearFunctionValue(GCFunctionValue*);

			void releaseLocals(Locals*);
			void deleteLocals(Locals*);
			void clearStackFunction(StackFunction*);

			bool pushRecursion(Value root, Value obj, Value name);
			void popRecursion(Value root, Value obj, Value name);

			bool pushGetRecursion(const Value& obj, const Value& name);
			void popGetRecursion(const Value& obj, const Value& name);

			bool pushSetRecursion(const Value& obj, const Value& name);
			void popSetRecursion(const Value& obj, const Value& name);

			bool pushValueOfRecursion(Value obj);
			void popValueOfRecursion(Value obj);

			GCStringValue * pushStringValue(const String&);
			GCStringValue * pushStringValue(const String&, const String&);
			GCStringValue * pushStringValue(const String&, bool trim_left, bool trim_right);
			GCStringValue * pushStringValue(const OS_CHAR*);
			GCStringValue * pushStringValue(const OS_CHAR*, int len);
			GCStringValue * pushStringValue(const OS_CHAR*, int len, const OS_CHAR*, int len2);
			GCStringValue * pushStringValue(const OS_CHAR*, int len, bool trim_left, bool trim_right);
			GCStringValue * pushStringValue(const void * buf, int size);
			GCStringValue * pushStringValue(const void * buf1, int size1, const void * buf2, int size2);
			GCStringValue * pushStringValue(const void * buf1, int size1, const void * buf2, int size2, const void * buf3, int size3);
			GCStringValue * pushStringValue(GCStringValue*);
			GCStringValue * pushStringValue(GCStringValue*, GCStringValue*);
			GCStringValue * pushStringValue(OS_INT);
			GCStringValue * pushStringValue(OS_FLOAT);
			GCStringValue * pushStringValue(OS_FLOAT, int);
			GCStringValue * pushStringValue(int temp_buf_len, const OS_CHAR * fmt, ...);
			GCStringValue * pushStringValueVa(int temp_buf_len, const OS_CHAR * fmt, va_list va);

			GCCFunctionValue * pushCFunctionValue(OS_CFunction func, void * user_param);
			GCCFunctionValue * pushCFunctionValue(OS_CFunction func, int closure_values, void * user_param);
			GCUserdataValue * pushUserdataValue(int crc, int data_size, OS_UserdataDtor dtor, void * user_param);
			GCUserdataValue * pushUserPointerValue(int crc, void * data, OS_UserdataDtor dtor, void * user_param);
			GCUserdataValue * findUserPointerValue(void * data);
			GCObjectValue * pushObjectValue();
			GCObjectValue * pushObjectValue(GCValue * prototype);
			GCArrayValue * pushArrayValue(int initial_capacity = 0);

			GCValue * initNewInstance(GCValue*);

			template<class T> T * pushValue(T * val){ pushValue(Value(val)); return val; }

			void pushValue(const Value& val);
			void pushStackValue(int offs);
			void copyValue(int raw_from, int raw_to);
			void insertValue(Value val, int offs);
			void pushNull();
			void pushBool(bool);
			
			template<class T> void pushNumber(const T& val)
			{
			#if 1 // speed optimization
				StackValues& stack_values = this->stack_values;
				if(stack_values.capacity < stack_values.count+1){
					OS_NUMBER number = (OS_NUMBER)val;
					reserveStackValues(stack_values.count+1);
					stack_values.buf[stack_values.count++] = number;
				}else{
					stack_values.buf[stack_values.count++] = (OS_NUMBER)val;
				}
			#else
				pushValue((OS_NUMBER)val);
			#endif
			}
			
			String getTypeStr(const Value& val);
			void pushTypeOf(const Value& val);
			bool pushBoolOf(const Value& val);
			bool pushNumberOf(const Value& val);
			bool pushStringOf(const Value& val);
			bool pushValueOf(Value val);
			GCArrayValue * pushArrayOf(const Value& val);
			GCObjectValue * pushObjectOf(const Value& val);
			GCUserdataValue * pushUserdataOf(const Value& val);
			bool pushFunctionOf(const Value& val);

			void pushCloneValue(Value val);
			void pushCloneValueFrom(OS * other, Value other_val);

			// unary operator
			void pushOpResultValue(OpcodeType opcode, const Value& value);

			// binary operator
			bool pushOpResultValue(OpcodeType opcode, const Value& left_value, const Value& right_value);
			static bool isEqualExactly(const Value& left_value, const Value& right_value);

			void setGlobalValue(const String& name, Value value, bool setter_enabled);
			void setGlobalValue(const OS_CHAR * name, Value value, bool setter_enabled);

			int getStackOffs(int offs);
			Value getStackValue(int offs);

			void setExceptionValue(Value);

			void removeStackValues(int offs, int count);
			void removeStackValue(int offs = -1);
			void removeAllStackValues();
			void pop(int count = 1);
			void moveStackValues(int offs, int count, int new_offs);
			void moveStackValue(int offs, int new_offs);
			void exchangeStackValues(int offs);

			void registerStringRef(GCStringValue*);
			void unregisterStringRef(GCStringValue*);
			void deleteStringRefs();

			void registerUserptrRef(GCUserdataValue*);
			void unregisterUserptrRef(GCUserdataValue*);
			void unregisterUserptrRef(void*, int);
			void deleteUserptrRefs();

			void registerCFuncRef(GCCFunctionValue*);
			void unregisterCFuncRef(GCCFunctionValue*);
			void unregisterCFuncRef(OS_CFunction, void*, int);
			void deleteCFuncRefs();

			void registerValueAndPush(GCValue * val);
			GCValue * unregisterValue(int value_id);
			// void deleteValues(bool del_ref_counted_also);
			static int compareGCValues(const void * a, const void * b);

			String getValueClassname(const Value& val);
			String getValueClassname(GCValue * val);

			String getValueName(const Value& val);
			String getValueName(GCValue * val);

			String getValueNameOrClassname(const Value& val);
			String getValueNameOrClassname(GCValue * val);

			bool valueToBool(const Value& val);
			OS_INT valueToInt(const Value& val, bool valueof_enabled = false);
			OS_INT valueToIntRadix(const Value& val, int radix, bool valueof_enabled = false);
			OS_NUMBER valueToNumberRadix(const Value& val, int radix, bool valueof_enabled);
			OS_NUMBER valueToNumber(const Value& val, bool valueof_enabled = false);
			String valueToString(const Value& val, bool valueof_enabled = false);
			OS::String valueToStringOS(const Value& val, bool valueof_enabled = false);

			bool isValueNumber(const Value& val, OS_NUMBER * out = NULL);
			bool isValueString(const Value& val, String * out = NULL);
			bool isValueStringOS(const Value& val, OS::String * out = NULL);
			bool isValueInstanceOf(GCValue * val, GCValue * prototype_val);
			bool isValueInstanceOf(const Value& val, const Value& prototype_val);
			bool isValueOf(GCValue * val, GCValue * prototype_val);
			bool isValueOfUserdata(GCValue * val, int prototype_crc);
			bool isValueOf(const Value& val, const Value& prototype_val);
			bool isValueInValue(const Value& val, const Value& prototype_val);

			Table * newTable(OS_DBG_FILEPOS_START_DECL);
			void clearTable(Table*);
			void deleteTable(Table*);
			Property * addTableProperty(Table * table, const Value& index, const Value& value);

#ifdef OS_DEBUG
			static int checkSavedType(int type, const Value& value);
#endif

			void changePropertyIndex(Table * table, Property * prop, const Value& new_index);
			bool deleteTableProperty(Table * table, const Value& index);
			void deleteValueProperty(GCValue * table_value, Value index, bool del_enabled, bool prototype_enabled);
			void deleteValueProperty(const Value& table_value, const Value& index, bool del_enabled, bool prototype_enabled);
			
			void copyTableProperties(Table * dst, Table * src);
			void copyTableProperties(GCValue * dst_value, GCValue * src_value, bool setter_enabled);

			void sortTable(Table * table, int(*comp)(OS*, const void*, const void*, void*), void* = NULL, bool reorder_keys = false);
			void sortArray(GCArrayValue * arr, int(*comp)(OS*, const void*, const void*, void*), void* = NULL);

			static int comparePropValues(OS*, const void*, const void*, void*);
			static int comparePropValuesReverse(OS*, const void*, const void*, void*);
			static int compareObjectProperties(OS*, const void*, const void*, void*);
			static int compareObjectPropertiesReverse(OS*, const void*, const void*, void*);
			static int compareUserPropValues(OS*, const void*, const void*, void*);
			static int compareUserPropValuesReverse(OS*, const void*, const void*, void*);
			
			static int comparePropKeys(OS*, const void*, const void*, void*);
			static int comparePropKeysReverse(OS*, const void*, const void*, void*);
			static int compareUserPropKeys(OS*, const void*, const void*, void*);
			static int compareUserPropKeysReverse(OS*, const void*, const void*, void*);

			static int compareArrayValues(OS*, const void*, const void*, void*);
			static int compareArrayValuesReverse(OS*, const void*, const void*, void*);
			static int compareUserArrayValues(OS*, const void*, const void*, void*);
			static int compareUserArrayValuesReverse(OS*, const void*, const void*, void*);

			static int compareUserReverse(OS*, const void*, const void*, void*);

			Property * setTableValue(Table * table, const Value& index, const Value& val);
			void setPropertyValue(GCValue * table_value, const Value& index, Value val, bool setter_enabled);
			void setPropertyValue(const Value& table_value, const Value& index, const Value& val, bool setter_enabled);

			bool getPropertyValue(Value& result, GCValue * table_value, const Value& index, bool prototype_enabled);
			bool getPropertyValue(Value& result, const Value& table_value, const Value& index, bool prototype_enabled);
			bool getPropertyValueByPrototype(Value& result, const Value& table_value, const Value& index, bool prototype_enabled);

			bool hasProperty(GCValue * table_value, Value index, bool getter_enabled, bool prototype_enabled);
			void pushPropertyValue(GCValue * table_value, const Value& index, bool getter_enabled, bool prototype_enabled);
			void pushPropertyValue(const Value& table_value, const Value& index, bool getter_enabled, bool prototype_enabled);

			void setPrototypeValue(const Value& val, const Value& proto);
			void setPrototypeValue(const Value& val, const Value& proto, int userdata_crc);
			void pushPrototypeValue(const Value& val);

			void pushBackTrace(int skip_funcs, int max_trace_funcs = 20);
			void pushArguments(StackFunction*);
			void pushArgumentsWithNames(StackFunction*);
			void pushRestArguments(StackFunction*);

			void execute();
			void reloadStackFunctionCache();

			void callFT(int start_pos, int call_params, int ret_values, GCValue * self_for_proto, OS_ECallEnter call_enter, OS_ECallType call_type, OS_ECallThisUsage call_this_usage);
			void callFT(int params, int ret_values, GCValue * self_for_proto, OS_ECallEnter call_enter, OS_ECallType call_type, OS_ECallThisUsage call_this_usage);
			
			void callFT(int params, int ret_values, OS_ECallType call_type = OS_CALLTYPE_AUTO, OS_ECallThisUsage call_this_usage = OS_CALLTHIS_KEEP_STACK_VALUE);
			void callTF(int params, int ret_values, OS_ECallType call_type = OS_CALLTYPE_AUTO, OS_ECallThisUsage call_this_usage = OS_CALLTHIS_KEEP_STACK_VALUE);
			void callF(int params, int ret_values, OS_ECallType call_type = OS_CALLTYPE_AUTO);

			static int prototypeFunctionApply(OS * os, int params, int, int need_ret_values, void*);
			static int prototypeFunctionCall(OS * os, int params, int, int need_ret_values, void*);

			Core(OS*);
			~Core();

			bool init();
			void shutdown();
		};

		MemoryManager * memory_manager;
		Core * core;
		int ref_count;

#ifdef OS_DEBUG
		int native_stack_start_mark;
		int native_stack_max_usage;
		bool native_stack_in_process;

		void checkNativeStackUsage(const OS_CHAR * func_name);
#endif

		virtual ~OS();

		virtual void shutdown();

		void qsort(void *base, unsigned num, unsigned width, int (*comp)(OS*, const void *, const void *, void*), void*);

		void initCoreFunctions();
		void initObjectClass();
		void initArrayClass();
		void initFunctionClass();
		void initStringClass();
		void initNumberClass();
		void initBooleanClass();
		void initBufferClass();
		void initFileClass();
		void initExceptionClass();
		void initPathModule();
		void initMathModule();
		void initJsonModule();
		void initGCModule();
		void initLangTokenizerModule();
		virtual void initSettings();
		virtual void initPreScript();
		virtual void initPostScript();

		template<class Core> friend struct UserDataDestructor;

	public:

		typedef Core::Buffer CoreBuffer;
		typedef Core::File CoreFile;

		class String: public Core::String // this string retains OS
		{
			typedef Core::String super;
			friend class Core;
			friend class Buffer;
			friend class OS;

		protected:

			// OS * allocator;
			String(OS*, Core::GCStringValue*);

		public:

			String(OS*);
			String(const String&);
			String(const Core::String&);
			String(const Core::String&, const Core::String&);
			String(OS*, const OS_CHAR*);
			String(OS*, const OS_CHAR*, int len);
			String(OS*, const OS_CHAR*, int len, const OS_CHAR*, int len2);
			String(OS*, const OS_CHAR*, int len, bool trim_left, bool trim_right);
			String(const Core::String&, bool trim_left, bool trim_right);
			String(OS*, const void*, int size);
			String(OS*, const void * buf1, int len1, const void * buf2, int len2);
			String(OS*, OS_INT value);
			String(OS*, OS_FLOAT value);
			String(OS*, OS_FLOAT value, int precision);
			~String();

			String& operator=(const Core::String&);
			// String& operator=(const String&);
			String& operator+=(const Core::String&);
			String& operator+=(const OS_CHAR*);
			String operator+(const Core::String&) const;
			String operator+(const OS_CHAR*) const;

			String trim(bool trim_left = true, bool trim_right = true) const;

			static String format(OS*, const OS_CHAR * fmt, ...);
			static String formatVa(OS*, const OS_CHAR * fmt, va_list va);
		};

		static OS * create(MemoryManager* = NULL);

		template <class T>
		static T * create(T * os, MemoryManager * manager = NULL)
		{
			OS_ASSERT(dynamic_cast<OS*>(os));
			return (T*)os->start(manager);
		}

		OS();

		virtual OS * start(MemoryManager* = NULL);
		virtual bool init(MemoryManager* = NULL);

		OS * retain();
		void release();

		virtual void * malloc(int size OS_DBG_FILEPOS_DECL);
		virtual void free(void * p);

		int getAllocatedBytes();
		int getMaxAllocatedBytes();
		int getUsedBytes();
		int getCachedBytes();

		void setMemBreakpointId(int id);

		int getMaxCallStack();
		void setMaxCallStack(int);

		bool isTerminated();
		int getTerminatedCode();
		void setTerminated(bool = true, int = 0);
		void resetTerminated();

		bool isExceptionSet();
		void getException();
		void setException(); // set & pop value from stack
		void setException(const OS_CHAR*);
		void setException(const Core::String&);
		void handleException();
		void resetException();

		void getProperty(bool getter_enabled = true, bool prototype_enabled = true);
		void getProperty(const OS_CHAR*, bool getter_enabled = true, bool prototype_enabled = true);
		void getProperty(const Core::String&, bool getter_enabled = true, bool prototype_enabled = true);
		void getProperty(int offs, const OS_CHAR*, bool getter_enabled = true, bool prototype_enabled = true);
		void getProperty(int offs, const Core::String&, bool getter_enabled = true, bool prototype_enabled = true);
		
		void setProperty(bool setter_enabled = true);
		void setProperty(const OS_CHAR*, bool setter_enabled = true);
		void setProperty(const Core::String&, bool setter_enabled = true);
		void setProperty(int offs, const OS_CHAR*, bool setter_enabled = true);
		void setProperty(int offs, const Core::String&, bool setter_enabled = true);
		
		void addProperty(bool setter_enabled = true);
		void addProperty(int offs, bool setter_enabled = true);

		bool setSmartProperty(const OS_CHAR*, bool setter_enabled = true);
		bool setSmartProperty(const Core::String&, bool setter_enabled = true);

		void deleteProperty(bool del_enabled = true);
		void deleteProperty(const OS_CHAR*, bool del_enabled = true);
		void deleteProperty(const Core::String&, bool del_enabled = true);

		void getGlobal(const OS_CHAR*, bool getter_enabled = true, bool prototype_enabled = true);
		void getGlobal(const Core::String&, bool getter_enabled = true, bool prototype_enabled = true);

		void setGlobal(const OS_CHAR*, bool setter_enabled = true);
		void setGlobal(const Core::String&, bool setter_enabled = true);

		struct FuncDef;
		
		void setGlobal(const FuncDef& func, bool setter_enabled = true);

		void getPrototype();
		void setPrototype();
		void setPrototype(int userdata_crc);

		int getCFuncClosureCount(int offs = -1); // no pop
		
		void getCFuncClosure(int offs, int i);
		void getCFuncClosure(int i);
		
		void setCFuncClosure(int offs, int i);
		void setCFuncClosure(int i);

		int getValueId(int offs = -1);
		String getValueName(int offs = -1);
		String getValueClassname(int offs = -1);
		String getValueNameOrClassname(int offs = -1);

		void pushNull();
		template<class T> void pushNumber(const T& val){ core->pushNumber(val); }
		void pushBool(bool);
		void pushString(const OS_CHAR*);
		void pushString(const OS_CHAR*, int len);
		void pushString(const void*, int size);
		void pushString(const Core::String&);
		void pushCFunction(OS_CFunction func, void * user_param = NULL);
		void pushCFunction(OS_CFunction func, int closure_values, void * user_param = NULL);
		void * pushUserdata(int crc, int data_size, OS_UserdataDtor dtor = NULL, void * user_param = NULL);
		void * pushUserdata(int data_size, OS_UserdataDtor dtor = NULL, void * user_param = NULL);
		void * pushUserPointer(int crc, void * data, OS_UserdataDtor dtor = NULL, void * user_param = NULL);
		void * pushUserPointer(void * data, OS_UserdataDtor dtor = NULL, void * user_param = NULL);
		int findUserPointerValueId(void * data);
		void newObject();
		void newArray(int initial_capacity = 0);

		void pushBackTrace(int skip_funcs, int max_trace_funcs = 20);

		void pushStackValue(int offs = -1);
		void pushGlobals();
		void pushUserPool();
		void pushValueById(int id);

		void retainValueById(int id);
		void releaseValueById(int id);

		void clone(int offs = -1);

		int getStackSize();
		int getAbsoluteOffs(int offs);
		
		void remove(int start_offs = -1, int count = 1);
		// void removeAll(); it's not safe to remove all
		void pop(int count = 1);
		void move(int start_offs, int count, int new_offs);
		void move(int offs, int new_offs);
		void exchange(int offs = -2); // var temp = stack[absolute(offs)]; stack[absolute(offs)] = stack[absolute(offs)+1]; stack[absolute(offs)+1] = temp

		void runOp(OS_EOpcode opcode);

		// returns length of object, array, string or result of __len method
		// keep stack not changed
		int getLen(int offs = -1);

		OS_EValueType getType(int offs = -1);
		OS_EValueType getTypeById(int id);
		String getTypeStr(int offs = -1);
		String getTypeStrById(int id);
		bool isNumber(int offs = -1, OS_NUMBER * out = NULL);
		bool isString(int offs = -1, String * out = NULL);
		bool isType(OS_EValueType, int offs = -1);
		bool isNull(int offs = -1);
		bool isObject(int offs = -1);
		bool isArray(int offs = -1);
		bool isFunction(int offs = -1);
		bool isUserdata(int offs = -1);
		bool isUserdata(int crc, int offs, int prototype_crc = 0);
		bool is(int value_offs = -2, int prototype_offs = -1);
		bool as(int value_offs = -2, int prototype_offs = -1);
		bool in(int name_offs = -2, int obj_offs = -1);

		void * toUserdata(int crc, int offs = -1, int prototype_crc = 0);
		void clearUserdata(int crc, int offs = -1, int prototype_crc = 0);

		bool		toBool(int offs = -1);
		OS_NUMBER	toNumber(int offs = -1, bool valueof_enabled = true);
		float		toFloat(int offs = -1, bool valueof_enabled = true);
		double		toDouble(int offs = -1, bool valueof_enabled = true);
		int			toInt(int offs = -1, bool valueof_enabled = true);
		int			toIntRadix(int offs = -1, int radix = 10, bool valueof_enabled = true);
		String		toString(int offs = -1, bool valueof_enabled = true);
		
		bool		toBool(int offs, bool def);
		OS_NUMBER	toNumber(int offs, OS_NUMBER def, bool valueof_enabled = true);
		float		toFloat(int offs, float def, bool valueof_enabled = true);
		double		toDouble(int offs, double def, bool valueof_enabled = true);
		int			toInt(int offs, int def, bool valueof_enabled = true);
		int			toIntRadix(int offs, int def, int radix, bool valueof_enabled = true);
		String		toString(int offs, const String& def, bool valueof_enabled = true);
		String		toString(int offs, const OS_CHAR * def, bool valueof_enabled = true);

		bool		popBool();
		OS_NUMBER	popNumber(bool valueof_enabled = true);
		float		popFloat(bool valueof_enabled = true);
		double		popDouble(bool valueof_enabled = true);
		int			popInt(bool valueof_enabled = true);
		int			popIntRadix(int radix = 0, bool valueof_enabled = true);
		String		popString(bool valueof_enabled = true);

		bool		popBool(bool def);
		OS_NUMBER	popNumber(OS_NUMBER def, bool valueof_enabled = true);
		float		popFloat(float def, bool valueof_enabled = true);
		double		popDouble(double def, bool valueof_enabled = true);
		int			popInt(int def, bool valueof_enabled = true);
		int			popIntRadix(int def, int radix, bool valueof_enabled = true);
		String		popString(const String& def, bool valueof_enabled = true);
		String		popString(const OS_CHAR * def, bool valueof_enabled = true);

		int getSetting(OS_ESettings);
		int setSetting(OS_ESettings, int);

		bool compileFile(const String& filename, bool required = false, OS_ESourceCodeType source_code_type = OS_SOURCECODE_AUTO, bool check_utf8_bom = true);
		bool compileFakeFile(const String& filename, const String& str, OS_ESourceCodeType source_code_type = OS_SOURCECODE_AUTO, bool check_utf8_bom = true);
		bool compile(const String& str, OS_ESourceCodeType source_code_type = OS_SOURCECODE_AUTO, bool check_utf8_bom = true);
		bool compile(OS_ESourceCodeType source_code_type = OS_SOURCECODE_AUTO, bool check_utf8_bom = true);

		// deprecated, use callFT, callTF or callF
		// void call(int params = 0, int ret_values = 0, OS_ECallType call_type = OS_CALLTYPE_AUTO, OS_ECallThisUsage call_this_usage = OS_CALLTHIS_KEEP_STACK_VALUE);
		
		// stack: func + this + params
		void callFT(int params = 0, int ret_values = 0, OS_ECallType call_type = OS_CALLTYPE_AUTO, OS_ECallThisUsage call_this_usage = OS_CALLTHIS_KEEP_STACK_VALUE);

		// stack: this + func + params
		void callTF(int params = 0, int ret_values = 0, OS_ECallType call_type = OS_CALLTYPE_AUTO, OS_ECallThisUsage call_this_usage = OS_CALLTHIS_KEEP_STACK_VALUE);
		
		// stack: func + params, it uses function's this
		void callF(int params = 0, int ret_values = 0, OS_ECallType call_type = OS_CALLTYPE_AUTO);

		void eval(const OS_CHAR * str, int params = 0, int ret_values = 0, OS_ESourceCodeType source_code_type = OS_SOURCECODE_AUTO, bool check_utf8_bom = true, bool handle_exception = true);
		void eval(const String& str, int params = 0, int ret_values = 0, OS_ESourceCodeType source_code_type = OS_SOURCECODE_AUTO, bool check_utf8_bom = true, bool handle_exception = true);

		void evalFakeFile(const OS_CHAR * filename, const OS_CHAR * str, int params = 0, int ret_values = 0, OS_ESourceCodeType source_code_type = OS_SOURCECODE_AUTO, bool check_utf8_bom = true, bool handle_exception = true);
		void evalFakeFile(const String& filename, const String& str, int params = 0, int ret_values = 0, OS_ESourceCodeType source_code_type = OS_SOURCECODE_AUTO, bool check_utf8_bom = true, bool handle_exception = true);

		void evalProtected(const OS_CHAR * str, int params = 0, int ret_values = 0, OS_ESourceCodeType source_code_type = OS_SOURCECODE_AUTO, bool check_utf8_bom = true, bool handle_exception = true);

		void require(const OS_CHAR * filename, bool required = false, int ret_values = 0, OS_ESourceCodeType source_code_type = OS_SOURCECODE_AUTO, bool check_utf8_bom = true, bool handle_exception = true);
		virtual void require(const String& filename, bool required = false, int ret_values = 0, OS_ESourceCodeType source_code_type = OS_SOURCECODE_AUTO, bool check_utf8_bom = true, bool handle_exception = true);

		// return next gc phase
		// int gcStep();
		void gcFull();

		void setGCStartWhenUsedBytes(int);
		int getGCStartWhenUsedBytes();

		struct FuncDef {
			const OS_CHAR * name;
			OS_CFunction func;
			void * user_param;
		};
		
		struct NumberDef {
			const OS_CHAR * name;
			OS_NUMBER value;
		};
		
		struct StringDef {
			const OS_CHAR * name;
			const OS_CHAR * value;
		};

		struct NullDef {
			const OS_CHAR * name;
		};
		
		struct Pop {
			OS * os; 
			Pop(OS * p_os): os(p_os){}
			~Pop(){ os->pop(); }
		};

		struct SaveStackSize
		{
			OS * os;
			int stackSize;

			SaveStackSize(OS * _os)
			{
				os = _os;
				stackSize = _os->getStackSize();
			}

			~SaveStackSize()
			{
				OS_ASSERT(os->getStackSize() >= stackSize);
				os->pop(os->getStackSize() - stackSize);
			}
		};

		void setFuncs(const FuncDef * list, bool setter_enabled = true, int closure_values = 0, void * user_param = NULL); // null terminated list
		void setFunc(const FuncDef& def, bool setter_enabled = true, int closure_values = 0, void * user_param = NULL); // null terminated list
		void setNumbers(const NumberDef * list, bool setter_enabled = true);
		void setNumber(const NumberDef& def, bool setter_enabled = true);
		void setStrings(const StringDef * list, bool setter_enabled = true);
		void setString(const StringDef& def, bool setter_enabled = true);
		void setNulls(const NullDef * list, bool setter_enabled = true);
		void setNull(const NullDef& def, bool setter_enabled = true);

		void getObject(const OS_CHAR * name, bool getter_enabled = true, bool prototype_enabled = true);
		void getGlobalObject(const OS_CHAR * name, bool getter_enabled = true, bool prototype_enabled = true);
		void getModule(const OS_CHAR * name, bool getter_enabled = true, bool prototype_enabled = true);

		bool nextIteratorStep(int results = 2);
		bool nextIteratorStep(int results, const Core::String& iter_func);

		String changeFilenameExt(const String& filename, const String& ext);
		String changeFilenameExt(const String& filename, const OS_CHAR * ext);
		
		String getFilenameExt(const String& filename);
		String getFilenameExt(const OS_CHAR * filename);
		String getFilenameExt(const OS_CHAR * filename, int len);
		
		String getFilename(const String& filename);
		String getFilename(const OS_CHAR * filename);
		String getFilename(const OS_CHAR * filename, int len);
		
		String getFilenamePath(const String& filename);
		String getFilenamePath(const OS_CHAR * filename);
		String getFilenamePath(const OS_CHAR * filename, int len);

		bool isAbsolutePath(const String& filename);
		String resolvePath(const String& filename);
		virtual String resolvePath(const String& filename, const String& cur_path);
		virtual String getCompiledFilename(const String& resolved_filename);
		virtual String getTextOpcodesFilename(const String& resolved_filename);

		virtual OS_EFileUseType checkFileUsage(const String& sourcecode_filename, const String& compiled_filename);

		virtual OS_ESourceCodeType getSourceCodeType(const String& filename);

		virtual bool isFileExist(const OS_CHAR * filename);
		virtual int getFileSize(const OS_CHAR * filename);
		virtual int getFileSize(FileHandle * f);
		virtual FileHandle * openFile(const OS_CHAR * filename, const OS_CHAR * mode);
		virtual int readFile(void * buf, int size, FileHandle * f);
		virtual int writeFile(const void * buf, int size, FileHandle * f);
		virtual int seekFile(FileHandle * f, int offset, int whence);
		virtual void closeFile(FileHandle * f);

		virtual void echo(const void * buf, int size);
		void echo(const OS_CHAR * str);
		void echo(const Core::String& str);
		virtual void printf(const OS_CHAR * fmt, ...);

		void appendQuotedString(Core::Buffer& buf, const Core::String& string);

		int getRandSeed();
		void setRandSeed(int seed);
		// void randReload();
		double getRand();
		double getRand(double up);
		double getRand(double min, double max);
	};
} // namespace ObjectScript

#endif // __OBJECT_SCRIPT_H__
