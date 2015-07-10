#ifndef _42TinyJS_config_h__
#define _42TinyJS_config_h__

/*
 * 42TinyJS
 *
 * A fork of TinyJS with the goal to makes a more JavaScript/ECMA compliant engine
 *
 * Authored By Armin Diedering <armin@diedering.de>
 *
 * Copyright (C) 2010-2015 ardisoft
 *
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:

 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.

 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

//////////////////////////////////////////////////////////////////////////

/* POOL-ALLOCATOR 
 * ==============
 * To speed-up new & delete 42TinyJS adds an object-pool
 * The pool is activated by default.
 * To deactivate this stuff define NO_POOL_ALLOCATOR 
 */
//#define NO_POOL_ALLOCATOR

/*
 * for debugging-stuff you can define DEBUG_POOL_ALLOCATOR
 * if a memory-leak detected the allocator usage is printed to stderr
 */
//#define DEBUG_POOL_ALLOCATOR

/*
 * with define LOG_POOL_ALLOCATOR_MEMORY_USAGE
 * the allocator usage is always printed to stderr
 */
//#define LOG_POOL_ALLOCATOR_MEMORY_USAGE

// NOTE: _DEBUG or LOG_POOL_ALLOCATOR_MEMORY_USAGE implies DEBUG_POOL_ALLOCATOR

/*
 * to increasing of speed a spin lock is used, if available 
 * if c++11 <atomic> not available or NO_SPINLOCK_IN_POOL_ALLOCATOR is defined
 * a normal mutex lock is used or noting if NO_THREADING defined
 */
//#define NO_SPINLOCK_IN_POOL_ALLOCATOR

//////////////////////////////////////////////////////////////////////////

/* REGEXP-SUPPORT
 * ==============
 * The RegExp-support needs c++11, boost-regex or TR1-regex
 * To deactivate this stuff define NO_REGEXP 
 */
//#define NO_REGEXP

/* if NO_REGEXP not defined <regex> is included and std::regex is used
 * you can define HAVE_BOOST_REGEX and <boost/regex.hpp> is included and boost::regex is used
 */
//#define HAVE_BOOST_REGEX

/* or you can define HAVE_TR1_REGEX and <tr1/regex> is included and std::tr1::regex is used
 */
//#define HAVE_TR1_REGEX

/* or you can define HAVE_CXX_REGEX and <regex> is included and std::regex is used
 */
//#define HAVE_CXX_REGEX


//////////////////////////////////////////////////////////////////////////

/* LET-STUFF
 * =========
 * Redeclaration of LET-vars is not allowed in block-scopes.
 * But in the root- and functions-scopes it is currently allowed
 * In future ECMAScript versions this will be also in root-and functions-scopes forbidden
 * To enable the future behavior define PREVENT_REDECLARATION_IN_FUNCTION_SCOPES
 */
//#define PREVENT_REDECLARATION_IN_FUNCTION_SCOPES

//////////////////////////////////////////////////////////////////////////

/* GENERATOR's
 * ===========
 * functions with "yield" in it is detected as Generator.
 * Generator-support needs threading-stuff
 * To disable Generators define NO_GENERATORS
 * NOTE NO_THREADING implies NO_GENERATORS
 */
//#define NO_GENERATORS


//////////////////////////////////////////////////////////////////////////

/* MULTI-THREADING
 * ===============
 * 42TinyJS is basically thread-save.
 * You can run different or the same JS-code simultaneously in different instances of class TinyJS. 
 * >>> NOTE: You can NOT run more threads on the SAME instance of class TinyJS <<<
 * The threading-stuff is needed by the pool-allocator (locking) and the generator-/yield-stuff
 * to deactivate threading define NO_THREADING
 * NOTE: if NO_THREADING defined and NO_POOL_ALLOCATOR is undefined you can not run JS-code simultaneously
 */
//#define NO_THREADING

/* with C++2011 (or MS VisualC++ 2012 and above) the C++ 2011 STL-Threading-API is used. 
 * You can define NO_CXX_THREADS to use alternate API's
 */
//#define NO_CXX_THREADS 

/* if C++ 2011 STL-Threading-API not available
 * - on Windows the windows-threading-API is used by default.
 * - on non-Windows (WIN32 is not defined) it is tried to use the POSIX pthread-API
 * to force the pthread-API define HAVE_PTHREAD (windows needs in this case 
 *   a pthread-lib e.g http://http://sourceware.org/pthreads-win32/)
 */
//#define HAVE_PTHREAD

/* you can implement your own custom thread-implementation.
 * to prevent the using of the win- or pthread-API define HAVE_CUSTOM_THREADING_IMPL
 */
//#define HAVE_CUSTOM_THREADING_IMPL



/* for Date we need the time in a resolution of 1 ms
 * on Windows the function "GetSystemTimeAsFileTime" is used
 * on non-Windows (WIN32 is not defined) it is tried to use "gettimeofday"
 * unset HAVE_GETTIMEOFDAY to use a simple time()*1000 
 */ 
#define HAVE_GETTIMEOFDAY


////////////////////////////////////////////////
// DO NOT MAKE CHANGES OF THE FOLLOWING STUFF //
////////////////////////////////////////////////

#if defined(NO_THREADING) && !defined(NO_GENERATORS)
#	define NO_GENERATORS
#pragma message("\n***********************************************************************\n\
* You have defined NO_THREADING and not defined NO_GENERATORS\n\
* NOTE: GENERATORS needs THREADING. Generators/Yield are deactivated\n\
***********************************************************************\n")
#endif

#if defined(NO_POOL_ALLOCATOR) && defined(NO_GENERATORS) && !defined(NO_THREADING)
#	define NO_THREADING
#endif

#if !defined(NO_POOL_ALLOCATOR) && defined(NO_THREADING)
#pragma message("\n***********************************************************************\n\
* You have defined NO_THREADING and not defined NO_POOL_ALLOCATOR\n\
* NOTE: you can not run JS-code simultaneously in different threads\n\
***********************************************************************\n")
#endif

#define isCXX0x(version, minor) (defined(__GXX_EXPERIMENTAL_CXX0X__) && (__GNUC__ > version || __GNUC__ == version && __GNUC_MINOR__ >= version))

#if __cplusplus >= 201103L || isCXX0x(4,3) || _MSC_VER >= 1600 // Visual Studio 2010
#	define HAVE_CXX11_RVALUE_REFERENCE 1
#endif

#if __cplusplus >= 201103L || defined(__GXX_EXPERIMENTAL_CXX0X__) || _MSC_VER >= 1700 // Visual Studio 2012
#		if !defined(NO_CXX_THREADS) && !defined(NO_THREADING)
#			define HAVE_CXX_THREADS 1
#		endif
#endif

#if __cplusplus >= 201103L || isCXX0x(4,4) || _MSC_VER >= 1800 // Visual Studio 2013
#	define HAVE_MEMBER_DELETE 1
#	define HAVE_MEMBER_DEFAULT 1
#	define MEMBER_DELETE =delete
#	define MEMBER_DEFAULT =default
#endif

#if !defined(NO_SPINLOCK_IN_POOL_ALLOCATOR) && (__cplusplus >= 201103L || isCXX0x(4,5) || _MSC_VER >= 1700 /* Visual Studio 2012 */)
#	define SPINLOCK_IN_POOL_ALLOCATOR 1
#endif

#if __cplusplus >= 201103L || isCXX0x(4,6) ||  _MSC_VER > 1800 // > Visual Studio 2013
#	define NOEXPECT noexpect
#else
#	define NOEXPECT throw()
#endif


#ifndef MEMBER_DELETE
#	define MEMBER_DELETE
#	define MEMBER_DEFAULT
#endif

#if !defined(NO_REGEXP) && !defined(HAVE_BOOST_REGEX) && !defined(HAVE_TR1_REGEX) && !defined(HAVE_CXX_REGEX) && __cplusplus < 201103L
#	define NO_REGEXP
#	if _MSC_VER > 1500 // Visual Studio 2010 and above
#		undef NO_REGEXP
#	elif _MSC_VER == 1500 && _MSC_FULL_VER >= 150030729 // Visual Studio 2008 SP1
#		undef NO_REGEXP
#		define HAVE_TR1_REGEX // untested
#	elif isCXX0x(4,6) // a better idea?? testet with libstdc++ shipped with GCC 4.6.3
#		undef NO_REGEXP
#	endif
#endif
#if defined(NO_REGEXP)
#pragma message("\n***********************************************************************\n\
* tiny-js is compiled without support for regular expessions\n\
* try make WITH_BOOST=1\n\
***********************************************************************\n")
#endif


#endif // _42TinyJS_config_h__
