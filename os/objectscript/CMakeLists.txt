cmake_minimum_required(VERSION 2.8.9)
project(ObjectScript)

# Prevent CMake from stripping install paths.
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

# We want to build "as static as it gets"
# http://stackoverflow.com/questions/3762057/cmake-how-to-produce-binaries-as-static-as-possible
#set(OLD_FIND "${CMAKE_FIND_LIBRARY_SUFFIXES}")
#macro(prefer_static)
#    IF(WIN32)
#        SET(CMAKE_FIND_LIBRARY_SUFFIXES .lib .a ${CMAKE_FIND_LIBRARY_SUFFIXES})
#    ELSE(WIN32)
#        SET(CMAKE_FIND_LIBRARY_SUFFIXES .a ${CMAKE_FIND_LIBRARY_SUFFIXES})
#    ENDIF(WIN32)
#endmacro(prefer_static)
#macro(reset_static)
#    set(CMAKE_FIND_LIBRARY_SUFFIXES "${OLD_FIND}")
#endmacro(reset_static)
set(BUILD_SHARED OFF)

# This is the base of ObjectScript.
set(OSBASE_HDR
    ${CMAKE_CURRENT_SOURCE_DIR}/src/objectscript.h
    ${CMAKE_CURRENT_SOURCE_DIR}/src/os-heap.h
)
set(OSBASE_SRC
    ${CMAKE_CURRENT_SOURCE_DIR}/src/os-heap.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/objectscript.cpp
)
file(GLOB OSBASE_BINDER "${CMAKE_CURRENT_SOURCE_DIR}/src/os-binder*.h")

# Used later
set(DEFLIST "")

# Now come the options
option(USE_ODBO "Build the OBDO extension." ON)
option(USE_ICONV "Build the iconf extension." OFF)
option(USE_CURL "Build the libcurl extension." ON)
option(USE_SQLITE3 "Build the sqlite3 extension." ON)
option(USE_REGEXP "Build the RegularExpression extension based on libpcre." ON)
option(USE_ZLIB "Build the ZLib extension." ON)
option(BUILD_SOCI "Build the delivered version of SoCi" OFF)


# Gather globs
file(GLOB GLOB_EXT_BASE "src/ext-base64/*.cpp")
file(
    GLOB_RECURSE GLOB_EXT_HASH
    "src/ext-hashlib/*.cpp"
    "src/ext-hashlib/*.c"
)

# Begin to gather sources
set(EXT_SRC
    src/ext-json/os-json.cpp
    src/ext-process/os-process.cpp
    src/ext-filesystem/os-filesystem.cpp
    src/ext-datetime/os-datetime.cpp
    src/ext-url/os-url.cpp
)

# MPFD Library
# 3rdparty/MPFDParser-1.0/*.cpp
file(GLOB MPFD_SRC "3rdparty/MPFDParser-1.0/*.cpp")
add_library(mpfd ${MPFD_SRC})

# fcgi library
# 3rdparty/fcgi
set(FCGI_SRC
    3rdparty/fcgi-2.4.1/libfcgi/fcgi_stdio.c
    3rdparty/fcgi-2.4.1/libfcgi/fcgiapp.c
    3rdparty/fcgi-2.4.1/libfcgi/fcgio.cpp
)
if("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
    list(APPEND FCGI_SRC 3rdparty/fcgi-2.4.1/libfcgi/os_win32.c)
else()
    list(APPEND FCGI_SRC 3rdparty/fcgi-2.4.1/libfcgi/os_unix.c)
endif()
add_library(fcgi ${FCGI_SRC})
#target_include_directories(fcgi PUBLIC
#    ${CMAKE_CURRENT_SOURCE_DIR}/3rdparty/fcgi-2.4.1/
#    ${CMAKE_CURRENT_SOURCE_DIR}/3rdparty/fcgi-2.4.1/libfcgi
#    ${CMAKE_CURRENT_SOURCE_DIR}/3rdparty/fcgi-2.4.1/include
#)
set_property(TARGET fcgi APPEND PROPERTY INCLUDE_DIRECTORIES ${CMAKE_CURRENT_SOURCE_DIR}/3rdparty/fcgi-2.4.1/)
set_property(TARGET fcgi APPEND PROPERTY INCLUDE_DIRECTORIES ${CMAKE_CURRENT_SOURCE_DIR}/3rdparty/fcgi-2.4.1/libfcgi)
set_property(TARGET fcgi APPEND PROPERTY INCLUDE_DIRECTORIES ${CMAKE_CURRENT_SOURCE_DIR}/3rdparty/fcgi-2.4.1/include)


# Other settings, mostly from original file
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
ELSE()
    # We build for 64 bit just as we should.
    set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
endif()

find_package(Threads)
if(NOT Threads_FOUND)
    message(FATAL_ERROR "[ ERROR ]: Could not find thread library.")
endif()

# PCRE
if(USE_REGEXP)
    find_package(PCRE)
    if(PCRE_FOUND)
    	include_directories(${PCRE_INCLUDE_DIR})
    	list(APPEND EXT_SRC src/ext-regexp/os-regexp.cpp)
    	list(APPEND EXT_SRC src/ext-regexp/os-regexp.h)
    else()
    	message(FATAL_ERROR "[ ERROR ]: Couldn't find the PCRE library")
        list(APPEND DEFLIST "OS_REGEXP_DISABLED")
    endif()
else()
    list(APPEND DEFLIST "OS_REGEXP_DISABLED")
endif()

# iconv
if(USE_ICONV)
    find_package(ICONV)
    if(ICONV_FOUND)
        list(APPEND EXT_SRC src/ext-iconv/os-iconv.cpp)
        list(APPEND EXT_SRC src/ext-iconv/os-iconv.h)
    else()
        list(APPEND DEFLIST "OS_ICONV_DISABLED")
        message(WARNING "Building without iconv!")
    endif()
else()
    list(APPEND DEFLIST "OS_ICONV_DISABLED")
endif()

# SoCi
# SoCi is required and can additionally be built.
if(NOT BUILD_SOCI)
    find_package(Soci)
    if(SOCI_FOUND)
        include_directories(${SOCI_INCLUDE_DIR})
        include_directories(${SOCI_INCLUDE_DIR}/mysql)
        include_directories(${SOCI_INCLUDE_DIR}/odbc)
    else()
        message(FATAL_ERROR "[ERROR]: Couldn't find Soci include files and/or library.")
    endif()
else()
    # Should make this better.
    add_subdirectory(3rdparty/soci-3.2.2/)
endif()

# Database stuff.
if(USE_ODBO)
    # MySQL
    #prefer_static()
    find_package(MySQL)
    if(MYSQL_FOUND)
        include_directories(${MYSQL_INCLUDE_DIR})
        list(APPEND EXT_SRC src/ext-odbo/os-odbo.cpp)
        list(APPEND EXT_SRC src/ext-odbo/os-odbo.h)
    else()
        message(FATAL_ERROR "[ERROR]: Couldn't find MySQL include files and/or library.")
    endif()
    #reset_static()
else()
    list(APPEND DEFLIST "OS_ODBO_DISABLED")
endif()

# Curl
if(USE_CURL)
    #prefer_static()
    find_package(CURL)
    if(CURL_FOUND)
		include_directories(${CURL_INCLUDE_DIR})
		list(APPEND EXT_SRC src/ext-curl/os-curl.cpp)
		list(APPEND EXT_SRC src/ext-curl/os-curl.h)
        # Because we are totally linking staticaly:
        #execute_process(COMMAND curl-config --libs OUTPUT_VARIABLE CURL_STUFF)
        #set(CMAKE_EXE_LINKER_FLAGS "${CURL_STUFF} ${CMAKE_EXE_LINKER_FLAGS}")
    else()
    	message(STATUS "[ WARNING ]: Could not find cURL")
        list(APPEND DEFLIST "OS_CURL_DISABLED")
    endif()
    #reset_static()
else()
    list(APPEND DEFLIST "OS_CURL_DISABLED")
endif()

# Sqlite3
if(USE_SQLITE3)
    list(APPEND EXT_SRC src/ext-sqlite3/os-sqlite3.cpp)
    list(APPEND EXT_SRC src/ext-sqlite3/os-sqlite3.h)
    list(APPEND EXT_SRC src/ext-sqlite3/sqlite3.c)
    list(APPEND EXT_SRC src/ext-sqlite3/sqlite3.h)
else()
    list(APPEND DEFLIST "OS_SQLITE_DISABLED")
endif()

# Zlib
if(USE_ZLIB)
    list(APPEND EXT_SRC src/ext-zlib/os-zlib.cpp)
    list(APPEND EXT_SRC src/ext-zlib/os-zlib.h)
    find_package(ZLIB)
    if(ZLIB_FOUND)
        include_directories(${ZLIB_INCLUDE_DIR})
    else()
        list(APPEND EXT_SRC src/ext-zlib/zlib/adler32.c)
        list(APPEND EXT_SRC src/ext-zlib/zlib/compress.c)
        list(APPEND EXT_SRC src/ext-zlib/zlib/crc32.c)
        list(APPEND EXT_SRC src/ext-zlib/zlib/deflate.c)
        list(APPEND EXT_SRC src/ext-zlib/zlib/gzclose.c)
        list(APPEND EXT_SRC src/ext-zlib/zlib/gzlib.c)
        list(APPEND EXT_SRC src/ext-zlib/zlib/gzread.c)
        list(APPEND EXT_SRC src/ext-zlib/zlib/gzwrite.c)
        list(APPEND EXT_SRC src/ext-zlib/zlib/infback.c)
        list(APPEND EXT_SRC src/ext-zlib/zlib/inffast.c)
        list(APPEND EXT_SRC src/ext-zlib/zlib/inflate.c)
        list(APPEND EXT_SRC src/ext-zlib/zlib/inftrees.c)
        list(APPEND EXT_SRC src/ext-zlib/zlib/trees.c)
        list(APPEND EXT_SRC src/ext-zlib/zlib/uncompr.c)
        list(APPEND EXT_SRC src/ext-zlib/zlib/zutil.c)
    endif()
else()
    list(APPEND DEFLIST "OS_ZLIB_DISABLED")
endif()


# Create a libobjectscript for other apps to link against.
add_library(objectscript STATIC
    ${OSBASE_SRC}
    ${OSBASE_HDR}
    ${OSBASE_BINDER}
    ${EXT_SRC}
    ${GLOB_EXT_BASE}
    ${GLOB_EXT_HASH}
)

# Now its time to put stuff in place
# First, OS-FCGI
add_executable(os-fcgi os-fcgi.cpp)
set_property(TARGET os-fcgi APPEND PROPERTY INCLUDE_DIRECTORIES ${CMAKE_CURRENT_SOURCE_DIR}/src)
target_link_libraries(os-fcgi
    ${CMAKE_THREAD_LIBS_INIT}
    ${CURL_LIBRARIES}
    ${PCRE_LIBRARIES}
    ${MYSQL_LIBRARIES}
    ${SOCI_LIBRARY}
    ${ZLIB_LIBRARY}
    ${EXT_LIBS}
    dl
    mpfd
    fcgi
    objectscript
)

# Now, standalone OS
add_executable(os os.cpp)
set_property(TARGET os APPEND PROPERTY INCLUDE_DIRECTORIES ${CMAKE_CURRENT_SOURCE_DIR}/src)
target_link_libraries(os
    ${CURL_LIBRARIES}
    ${PCRE_LIBRARIES}
    ${MYSQL_LIBRARIES}
    ${SOCI_LIBRARY}
    ${ZLIB_LIBRARY}
    ${EXT_LIBS}
    dl
    mpfd
    objectscript
)

# Short additions:
if(SOCI_mysql_PLUGIN)
    target_link_libraries(os ${SOCI_mysql_PLUGIN})
    target_link_libraries(os-fcgi ${SOCI_mysql_PLUGIN})
endif()

if(SOCI_odbc_PLUGIN)
    target_link_libraries(os ${SOCI_odbc_PLUGIN})
    target_link_libraries(os-fcgi ${SOCI_odbc_PLUGIN})
endif()

# Dont forget the defines.
set_target_properties(os PROPERTIES COMPILE_DEFINITIONS "${DEFLIST}")
set_target_properties(os-fcgi PROPERTIES COMPILE_DEFINITIONS "${DEFLIST}")

# A SoCi build will require this
if(BUILD_SOCI)
    # SoCi doesnt properly export its paths. So we hack it together.
    target_link_libraries(os-fcgi soci_core)
    # target_include_directories(os-fcgi PRIVATE "${SOCI_SOURCE_DIR}/core")
	set_property(TARGET os-fcgi APPEND PROPERTY INCLUDE_DIRECTORIES "${SOCI_SOURCE_DIR}/core")
    add_dependencies(os-fcgi soci_core)
    target_link_libraries(os soci_core)
    # target_include_directories(os PRIVATE "${SOCI_SOURCE_DIR}/core")
	set_property(TARGET os APPEND PROPERTY INCLUDE_DIRECTORIES "${SOCI_SOURCE_DIR}/core")
    add_dependencies(os soci_core)

    # Backends
    if(MYSQL_FOUND)
        set(SOCI_mysql_FOUND 1)
        # target_include_directories(os PRIVATE "${SOCI_SOURCE_DIR}/backends/mysql")
		set_property(TARGET os APPEND PROPERTY INCLUDE_DIRECTORIES "${SOCI_SOURCE_DIR}/backends/mysql")
        # target_include_directories(os-fcgi PRIVATE "${SOCI_SOURCE_DIR}/backends/mysql")
		set_property(TARGET os-fcgi APPEND PROPERTY INCLUDE_DIRECTORIES "${SOCI_SOURCE_DIR}/backends/mysql")
        target_link_libraries(os soci_mysql)
        target_link_libraries(os-fcgi soci_mysql)
    else()
        set(SOCI_mysql_FOUND 0)
    endif()

    if(ODBC_FOUND)
        set(SOCI_odbc_FOUND 1)
        # target_include_directories(os PRIVATE "${SOCI_SOURCE_DIR}/backends/odbc")
		set_property(TARGET os APPEND PROPERTY INCLUDE_DIRECTORIES "${SOCI_SOURCE_DIR}/backends/mysql")
        # target_include_directories(os-fcgi PRIVATE "${SOCI_SOURCE_DIR}/backends/odbc")
		set_property(TARGET os-fcgi APPEND PROPERTY INCLUDE_DIRECTORIES "${SOCI_SOURCE_DIR}/backends/odbc")
        target_link_libraries(os soci_odbc)
        target_link_libraries(os-fcgi soci_odbc)
    else()
        set(SOCI_odbc_FOUND 0)
    endif()
endif()

# Finish
configure_file (
        "${PROJECT_SOURCE_DIR}/os.config.h.in"
        "${PROJECT_BINARY_DIR}/os.config.h"
)
include_directories("${PROJECT_BINARY_DIR}")


# Installation
# FIXME: Make it possible to install libobjectscript, libfcgi, libmpfd together and provide headers too.
# That also implies a objectscript-config.cmake... Its a thing to work on.

# Install `os` and `os-fcgi`:
# Windows users must do this on their own.
if(UNIX)
    # Binaries
    install(TARGETS os-fcgi  DESTINATION sbin)
    install(TARGETS os  DESTINATION bin)
    # Library + headers
    install(TARGETS objectscript DESTINATION lib)
    install(FILES ${OSBASE_HDR} ${OSBASE_BINDER} DESTINATION include)
    # Extra files and startup script
    install(FILES conf/etc/init.d/os-fcgi
        DESTINATION /etc/init.d
        PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
                    GROUP_EXECUTE GROUP_READ
                    WORLD_EXECUTE WORLD_READ
    )
    install(DIRECTORY conf/etc/os-fcgi DESTINATION /etc)
endif()
