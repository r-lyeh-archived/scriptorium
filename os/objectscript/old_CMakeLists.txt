cmake_minimum_required (VERSION 2.8)
include(ExternalProject.cmake)

set(target "os-fcgi")
project(${target})

set(INSTALL_DIR ${CMAKE_CURRENT_BINARY_DIR}/_install)

OPTION(FORCE32 "Force a 32bit compile on 64bit" OFF)
IF(FORCE32)
    SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -m32")
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -m32")

    link_directories(/lib32)
    link_directories(/usr/lib32)

    include_directories(${INSTALL_DIR}/include)
    link_directories(${INSTALL_DIR}/lib)

    SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -m32 -ldl -L/lib32 -L/usr/lib32")
    SET(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} -m32 -ldl -L/lib32 -L/usr/lib32")
    SET(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -m32 -ldl -L/lib32 -L/usr/lib32")

    if(CMAKE_SIZEOF_VOID_P MATCHES "8" AND NOT(FORCE32) )
        set(BIN_LIBROOT "linux64")
        set(BIN_RPATH "\$ORIGIN/lib64")
    else()
        set(BIN_LIBROOT "linux")
        set(BIN_RPATH "\$ORIGIN/lib32")
    endif()

    set(CMAKE_SKIP_BUILD_RPATH TRUE)
    set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)
    set(CMAKE_INSTALL_RPATH ${BIN_RPATH})
    set(CMAKE_INSTALL_RPATH_USE_LINK_PATH FALSE)

    set(CMAKE_FIND_ROOT_PATH ${INSTALL_DIR})
    set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
    set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
    set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
ENDIF()

IF (FORCE32)
	include(ExternalProject)
	ExternalProject_Add(ncurses
		SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/3rdparty/ncurses-5.9
		CONFIGURE_COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/3rdparty/ncurses-5.9/configure --with-shared --prefix=${INSTALL_DIR} CFLAGS=-m32 CXXFLAGS=-m32
		BUILD_COMMAND make -j8
	)

	ExternalProject_Add(mysql
		SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/3rdparty/mysql-5.5.32
		CMAKE_ARGS -DCMAKE_CXX_FLAGS=${CMAKE_CXX_FLAGS} -m32 
				   -DCMAKE_C_FLAGS=${CMAKE_C_FLAGS} -m32 
				   -DWITH_LIBEDIT=ON 
				   -DCURSES_LIBRARY=${INSTALL_DIR}/lib/libncurses.so 
				   -DCURSES_INCLUDE_PATH=${INSTALL_DIR}/include 
				   -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR}
		BUILD_COMMAND make -j8
	)

	ExternalProject_Add(soci
		SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/3rdparty/soci-3.2.1/src
		CMAKE_ARGS -DCMAKE_CXX_FLAGS=${CMAKE_CXX_FLAGS} -m32 
				   -DCMAKE_C_FLAGS=${CMAKE_C_FLAGS} -m32 
				   -DMYSQL_LIBRARIES=${INSTALL_DIR}/lib/libmysqlclient_r.so 
				   -DMYSQL_INCLUDE_DIR=${INSTALL_DIR}/include 
				   -DWITH_BOOST=OFF 
				   -DWITH_MYSQL=ON 
				   -DSOCI_STATIC=OFF 
				   -DWITH_ODBC=ON 
				   -DWITHSQLITE3=OFF 
				   -DWITHORACLE=OFF 
				   -DWITHDB2=OFF
				   -DWITH_FIREBIRD=OFF 
				   -DWITH_POSTGRESQL=OFF 
				   -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR}
		BUILD_COMMAND make -j8 lib
	)
        set(SOCI_odbc_FOUND 1)
	ExternalProject_Add(curl
		SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/3rdparty/curl-7.32.0
		CMAKE_ARGS -DCMAKE_CXX_FLAGS=${CMAKE_CXX_FLAGS} -m32 
				   -DCMAKE_C_FLAGS=${CMAKE_C_FLAGS} -m32 
				   -DCMAKE_USE_OPENSSL=OFF 
				   -DBUILD_CURL_EXE=OFF 
				   -DBUILD_CURL_TESTS=OFF 
				   -DCURL_ZLIB=OFF 
				   -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR}
		BUILD_COMMAND make -j8
	)

	ExternalProject_Add(pcre
		SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/3rdparty/pcre-8.33 
		CMAKE_ARGS -DCMAKE_CXX_FLAGS=${CMAKE_CXX_FLAGS} -m32 
				   -DCMAKE_C_FLAGS=${CMAKE_C_FLAGS} -m32 
				   -DPCRE_SUPPORT_LIBZ=OFF 
				   -DPCRE_SUPPORT_LIBREADLINE=OFF 
				   -DBUILD_SHARED_LIBS=ON 
				   -DPCRE_SUPPORT_UTF=ON 
				   -DPCRE_SUPPORT_UNICODE_PROPERTIES=ON 
				   -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR}
		BUILD_COMMAND make -j8
	)
ELSE()
    OPTION(BUILD_SOCI "Force build soci and install into your system (you must have root privilege)" OFF)
    IF(BUILD_SOCI)
	ExternalProject_Add(soci
		SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/3rdparty/soci-3.2.2
		CMAKE_ARGS -DWITH_BOOST=OFF 
			   -DWITH_MYSQL=ON 
			   -DSOCI_STATIC=OFF 
			   -DWITH_ODBC=ON 
			   -DWITH_FIREBIRD=OFF 
			   -DWITH_POSTGRESQL=OFF 
		BUILD_COMMAND make -j8
                INSTALL_COMMAND make install
                STEP_TARGETS build install
	)
    set(SOCI_odbc_FOUND 1)
    ENDIF()
ENDIF()

# begin objectscript
list(APPEND SOURCES src/objectscript.cpp)
list(APPEND SOURCES src/os-heap.cpp)
list(APPEND HEADERS src/objectscript.h)
list(APPEND HEADERS src/os-heap.h)
# end objectscript

list(APPEND SOURCES 3rdparty/fcgi-2.4.1/libfcgi/os_unix.c)
list(APPEND SOURCES 3rdparty/fcgi-2.4.1/libfcgi/fcgiapp.c)
list(APPEND SOURCES 3rdparty/MPFDParser-1.0/Parser.cpp)
list(APPEND SOURCES 3rdparty/MPFDParser-1.0/Field.cpp)
list(APPEND SOURCES 3rdparty/MPFDParser-1.0/Exception.cpp)

include_directories(src)
include_directories(3rdparty/fcgi-2.4.1)
include_directories(3rdparty/fcgi-2.4.1/include)
include_directories(3rdparty/MPFDParser-1.0)


find_package(Threads QUIET)
if(NOT Threads_FOUND)
    message(FATAL_ERROR "[ ERROR ]: Could not find thread library.")
endif()

IF (NOT FORCE32)
    set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR})
    find_package(PCRE QUIET)
    if(PCRE_FOUND)
		include_directories(${PCRE_INCLUDE_DIR})
		list(APPEND SOURCES src/ext-regexp/os-regexp.cpp)
		list(APPEND HEADERS src/ext-regexp/os-regexp.h)
    else()
		message(FATAL_ERROR "[ ERROR ]: Couldn't find PCRE library")
		add_definitions( -DOS_REGEXP_DISABLED )
    endif()
ELSE()
    list(APPEND SOURCES src/ext-regexp/os-regexp.cpp)
    list(APPEND HEADERS src/ext-regexp/os-regexp.h)
    set(PCRE_LIBRARIES ${INSTALL_DIR}/lib/libpcrecpp.so ${INSTALL_DIR}/lib/libpcreposix.so ${INSTALL_DIR}/lib/libpcre.so)
ENDIF()

IF (NOT FORCE32)
    find_package(MySQL QUIET)
    if(MYSQL_FOUND)
        include_directories(${MYSQL_INCLUDE_DIR})
    else()
        message(FATAL_ERROR "[ERROR]: Couldn't find MySQL include files and/or library.")
    endif()
ELSE()
    include_directories(${INSTALL_DIR}/include/mysql)
    set(MYSQL_LIBRARIES ${INSTALL_DIR}/lib/libmysqlclient_r.so)
ENDIF()

IF (NOT FORCE32)
    IF (NOT BUILD_SOCI)
        find_package(Soci QUIET)
        if(SOCI_FOUND)
            include_directories(${SOCI_INCLUDE_DIR})
            include_directories(${SOCI_INCLUDE_DIR}/mysql)
            include_directories(${SOCI_INCLUDE_DIR}/odbc)
        else()
            message(FATAL_ERROR "[ERROR]: Couldn't find Soci include files and/or library.")
        endif()
    ELSE()
        include_directories(/usr/local/include/soci)
        include_directories(/usr/local/include/soci/mysql)
        include_directories(/usr/local/include/soci/odbc)
        if (${CMAKE_SIZEOF_VOID_P} EQUAL 8)
            set(SOCI_LIBRARY /usr/local/lib64/libsoci_core.so)
            set(SOCI_mysql_PLUGIN /usr/local/lib64/libsoci_mysql.so)
            set(SOCI_odbc_PLUGIN /usr/local/lib64/libsoci_odbc.so)
        else()
            set(SOCI_LIBRARY /usr/local/lib/libsoci_core.so)
            set(SOCI_mysql_PLUGIN /usr/local/lib/libsoci_mysql.so)
            set(SOCI_odbc_PLUGIN /usr/local/lib/libsoci_odbc.so)
        endif()
    ENDIF()
ELSE()
    include_directories(${INSTALL_DIR}/include/soci)
    include_directories(${INSTALL_DIR}/include/soci/mysql)
    include_directories(${INSTALL_DIR}/include/soci/odbc)
    set(SOCI_LIBRARY ${INSTALL_DIR}/lib/libsoci_core.so)
    set(SOCI_mysql_PLUGIN ${INSTALL_DIR}/lib/libsoci_mysql.so)
    set(SOCI_odbc_PLUGIN ${INSTALL_DIR}/lib/libsoci_odbc.so)
ENDIF()

IF (NOT FORCE32)
    find_package(CURL QUIET)
    if(CURL_FOUND)
		include_directories(${CMAKE_CURRENT_BINARY_DIR}/3rdparty/curl-7.32.0/include)
		list(APPEND SOURCES src/ext-curl/os-curl.cpp)
		list(APPEND HEADERS src/ext-curl/os-curl.h)
    else()
    	message(STATUS "[ WARNING ]: Could not find cURL")
		add_definitions( -DOS_CURL_DISABLED )
    endif()
ELSE()
    include_directories(${INSTALL_DIR}/include/curl)
    list(APPEND SOURCES src/ext-curl/os-curl.cpp)
    list(APPEND HEADERS src/ext-curl/os-curl.h)
    set(CURL_LIBRARIES ${INSTALL_DIR}/lib/libcurl.so)
ENDIF()

# begin sqlite3
list(APPEND SOURCES src/ext-sqlite3/sqlite3.c)
list(APPEND HEADERS src/ext-sqlite3/sqlite3.h)
list(APPEND SOURCES src/ext-sqlite3/os-sqlite3.cpp)
list(APPEND HEADERS src/ext-sqlite3/os-sqlite3.h)

SET(GCC_COVERAGE_COMPILE_FLAGS "${GCC_COVERAGE_COMPILE_FLAGS} ")
SET(GCC_COVERAGE_LINK_FLAGS    "${GCC_COVERAGE_LINK_FLAGS} -lpthread -ldl")
# end sqlite3

# begin process
list(APPEND SOURCES src/ext-process/os-process.cpp)
list(APPEND HEADERS src/ext-process/os-process.h)
# end process

# begin filesystem
list(APPEND SOURCES src/ext-filesystem/os-filesystem.cpp)
list(APPEND HEADERS src/ext-filesystem/os-filesystem.h)
# end filesystem

# begin datetime
list(APPEND SOURCES src/ext-datetime/os-datetime.cpp)
list(APPEND HEADERS src/ext-datetime/os-datetime.h)
# end datetime

# begin json
list(APPEND SOURCES src/ext-json/os-json.cpp)
list(APPEND HEADERS src/ext-json/os-json.h)
# end json

# begin hashlib
list(APPEND SOURCES src/ext-hashlib/os-hashlib.cpp)
list(APPEND HEADERS src/ext-hashlib/os-hashlib.h)
list(APPEND SOURCES src/ext-hashlib/crc32/os-crc32.c)
list(APPEND HEADERS src/ext-hashlib/crc32/os-crc32.h)
list(APPEND SOURCES src/ext-hashlib/des/des.cpp)
list(APPEND SOURCES src/ext-hashlib/md5/md5.cpp)
list(APPEND HEADERS src/ext-hashlib/md5/md5.h)
list(APPEND SOURCES src/ext-hashlib/sha/hmac.cpp)
list(APPEND SOURCES src/ext-hashlib/sha/sha1.cpp)
list(APPEND SOURCES src/ext-hashlib/sha/sha224-256.cpp)
list(APPEND SOURCES src/ext-hashlib/sha/sha384-512.cpp)
list(APPEND SOURCES src/ext-hashlib/sha/usha.cpp)
list(APPEND HEADERS src/ext-hashlib/sha/sha.h)
list(APPEND HEADERS src/ext-hashlib/sha/sha-private.h)
# end hashlib

# begin base64
list(APPEND SOURCES src/ext-base64/os-base64.cpp)
list(APPEND SOURCES src/ext-base64/cdecode.cpp)
list(APPEND SOURCES src/ext-base64/cencode.cpp)
list(APPEND HEADERS src/ext-base64/cdecode.h)
list(APPEND HEADERS src/ext-base64/cencode.h)
list(APPEND HEADERS src/ext-base64/os-base64.h)
# end base64

# begin url
list(APPEND SOURCES src/ext-url/os-url.cpp)
list(APPEND HEADERS src/ext-url/os-url.h)
# end url

# begin odbo
list(APPEND SOURCES src/ext-odbo/os-odbo.cpp)
list(APPEND HEADERS src/ext-odbo/os-odbo.h)
# end odbo

# begin iconv
list(APPEND SOURCES src/ext-iconv/os-iconv.cpp)
list(APPEND HEADERS src/ext-iconv/os-iconv.h)

include_directories(BEFORE 3rdparty/libiconv-1.14/libcharset/include)
include_directories(BEFORE 3rdparty/libiconv-1.14/include)
include_directories(BEFORE 3rdparty/libiconv-1.14/lib)

list(APPEND SOURCES 3rdparty/libiconv-1.14/libcharset/lib/localcharset.c)
list(APPEND SOURCES 3rdparty/libiconv-1.14/lib/iconv.c)

list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/aliases.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/aliases2.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/aliases_aix.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/aliases_aix_sysaix.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/aliases_dos.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/aliases_extra.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/aliases_osf1.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/aliases_osf1_sysosf1.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/aliases_sysaix.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/aliases_syshpux.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/aliases_sysosf1.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/aliases_syssolaris.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/armscii_8.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/ascii.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/atarist.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/big5.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/big5hkscs1999.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/big5hkscs2001.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/big5hkscs2004.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/big5hkscs2008.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/big5_2003.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/c99.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/canonical.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/canonical_aix.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/canonical_aix_sysaix.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/canonical_dos.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/canonical_extra.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/canonical_local.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/canonical_local_sysaix.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/canonical_local_syshpux.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/canonical_local_sysosf1.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/canonical_local_syssolaris.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/canonical_osf1.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/canonical_osf1_sysosf1.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/canonical_sysaix.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/canonical_syshpux.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/canonical_sysosf1.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/canonical_syssolaris.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/ces_big5.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/ces_gbk.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cjk_variants.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cns11643.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cns11643_1.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cns11643_15.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cns11643_2.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cns11643_3.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cns11643_4.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cns11643_4a.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cns11643_4b.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cns11643_5.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cns11643_6.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cns11643_7.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cns11643_inv.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/converters.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp1046.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp1124.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp1125.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp1129.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp1131.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp1133.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp1161.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp1162.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp1163.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp1250.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp1251.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp1252.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp1253.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp1254.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp1255.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp1256.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp1257.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp1258.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp437.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp737.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp775.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp850.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp852.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp853.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp855.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp856.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp857.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp858.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp860.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp861.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp862.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp863.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp864.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp865.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp866.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp869.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp874.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp922.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp932.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp932ext.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp936.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp936ext.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp943.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp949.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp950.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/cp950ext.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/dec_hanyu.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/dec_kanji.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/euc_cn.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/euc_jisx0213.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/euc_jp.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/euc_kr.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/euc_tw.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/flags.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/flushwc.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/gb12345.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/gb12345ext.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/gb18030.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/gb18030ext.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/gb18030uni.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/gb2312.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/gbk.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/gbkext1.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/gbkext2.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/gbkext_inv.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/georgian_academy.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/georgian_ps.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/hkscs1999.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/hkscs2001.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/hkscs2004.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/hkscs2008.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/hp_roman8.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/hz.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iconv_open1.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iconv_open2.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso2022_cn.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso2022_cnext.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso2022_jp.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso2022_jp1.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso2022_jp2.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso2022_jp3.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso2022_kr.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso646_cn.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso646_jp.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso8859_1.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso8859_10.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso8859_11.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso8859_13.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso8859_14.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso8859_15.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso8859_16.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso8859_2.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso8859_3.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso8859_4.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso8859_5.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso8859_6.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso8859_7.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso8859_8.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/iso8859_9.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/isoir165.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/isoir165ext.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/java.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/jisx0201.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/jisx0208.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/jisx0212.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/jisx0213.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/johab.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/johab_hangul.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/koi8_r.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/koi8_ru.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/koi8_t.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/koi8_u.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/ksc5601.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/loops.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/loop_unicode.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/loop_wchar.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/mac_arabic.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/mac_centraleurope.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/mac_croatian.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/mac_cyrillic.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/mac_greek.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/mac_hebrew.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/mac_iceland.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/mac_roman.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/mac_romania.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/mac_thai.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/mac_turkish.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/mac_ukraine.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/mulelao.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/nextstep.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/pt154.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/relocatable.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/riscos1.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/rk1048.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/shift_jisx0213.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/sjis.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/tcvn.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/tds565.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/tis620.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/translit.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/ucs2.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/ucs2be.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/ucs2internal.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/ucs2le.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/ucs2swapped.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/ucs4.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/ucs4be.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/ucs4internal.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/ucs4le.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/ucs4swapped.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/uhc_1.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/uhc_2.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/utf16.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/utf16be.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/utf16le.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/utf32.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/utf32be.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/utf32le.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/utf7.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/utf8.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/vietcomb.h)
list(APPEND HEADERS 3rdparty/libiconv-1.14/lib/viscii.h)
# end iconv

# begin zlib
list(APPEND SOURCES src/ext-zlib/os-zlib.cpp)
list(APPEND HEADERS src/ext-zlib/os-zlib.h)

list(APPEND SOURCES src/ext-zlib/zlib/adler32.c)
list(APPEND SOURCES src/ext-zlib/zlib/compress.c)
list(APPEND SOURCES src/ext-zlib/zlib/crc32.c)
list(APPEND SOURCES src/ext-zlib/zlib/deflate.c)
list(APPEND SOURCES src/ext-zlib/zlib/gzclose.c)
list(APPEND SOURCES src/ext-zlib/zlib/gzlib.c)
list(APPEND SOURCES src/ext-zlib/zlib/gzread.c)
list(APPEND SOURCES src/ext-zlib/zlib/gzwrite.c)
list(APPEND SOURCES src/ext-zlib/zlib/infback.c)
list(APPEND SOURCES src/ext-zlib/zlib/inffast.c)
list(APPEND SOURCES src/ext-zlib/zlib/inflate.c)
list(APPEND SOURCES src/ext-zlib/zlib/inftrees.c)
list(APPEND SOURCES src/ext-zlib/zlib/trees.c)
list(APPEND SOURCES src/ext-zlib/zlib/uncompr.c)
list(APPEND SOURCES src/ext-zlib/zlib/zutil.c)
# end zlib

SET( CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} ${GCC_COVERAGE_COMPILE_FLAGS}" )
SET( CMAKE_EXE_LINKER_FLAGS  "${CMAKE_EXE_LINKER_FLAGS} ${GCC_COVERAGE_LINK_FLAGS}" )

if(NOT "${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
  add_custom_target(strip_${target} ALL COMMAND ${CMAKE_STRIP}
      ${CMAKE_CURRENT_BINARY_DIR}/${target})
endif()

if(NOT "${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
  add_custom_target(strip_os ALL COMMAND ${CMAKE_STRIP}
      ${CMAKE_CURRENT_BINARY_DIR}/os)
endif()

add_executable(${target} os-fcgi.cpp ${SOURCES} ${HEADERS})
target_link_libraries(${target} ${CMAKE_THREAD_LIBS_INIT} ${CURL_LIBRARIES} ${PCRE_LIBRARIES} ${MYSQL_LIBRARIES} ${SOCI_LIBRARY} ${SOCI_mysql_PLUGIN} ${SOCI_odbc_PLUGIN} dl)

add_executable(os os.cpp ${SOURCES} ${HEADERS})
target_link_libraries(os ${CMAKE_THREAD_LIBS_INIT} ${CURL_LIBRARIES} ${PCRE_LIBRARIES} ${MYSQL_LIBRARIES} ${SOCI_LIBRARY} ${SOCI_mysql_PLUGIN} ${SOCI_odbc_PLUGIN} dl)

IF (FORCE32)
    add_dependencies(mysql ncurses)
    add_dependencies(soci mysql)
    add_dependencies(${target} pcre curl soci)
ENDIF()

IF(NOT FORCE32 AND BUILD_SOCI)
    add_dependencies(os soci)
    add_dependencies(${target} soci)
ENDIF()

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/os DESTINATION usr/bin
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
                GROUP_EXECUTE GROUP_READ
                WORLD_READ WORLD_EXECUTE)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${target} DESTINATION usr/sbin
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
                GROUP_EXECUTE GROUP_READ
                WORLD_READ WORLD_EXECUTE)

install(FILES conf/etc/init.d/${target} DESTINATION etc/init.d
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
                GROUP_EXECUTE GROUP_READ
                WORLD_READ WORLD_EXECUTE)

install(CODE "
    if(NOT EXISTS ${CMAKE_INSTALL_PREFIX}/etc/${target}/conf.os)
        execute_process(COMMAND ${CMAKE_COMMAND} -E
            copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/conf/etc/${target} ${CMAKE_INSTALL_PREFIX}/etc/${target})
    endif()"
)

IF (FORCE32)
    install(CODE "execute_process(COMMAND ${CMAKE_COMMAND} -E copy_directory ${INSTALL_DIR}/lib ${CMAKE_INSTALL_PREFIX}/usr/lib32)")
ENDIF()


configure_file (
        "${PROJECT_SOURCE_DIR}/os.config.h.in"
        "${PROJECT_BINARY_DIR}/os.config.h"
)
include_directories("${PROJECT_BINARY_DIR}")

