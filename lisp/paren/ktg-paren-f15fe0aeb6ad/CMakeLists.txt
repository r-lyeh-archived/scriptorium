# The Paren Programming Language

cmake_minimum_required(VERSION 2.8)

set(CMAKE_BUILD_TYPE Release)
if(CMAKE_COMPILER_IS_GNUCXX)
  add_definitions(-std=c++11)
  set(CMAKE_EXE_LINKER_FLAGS "-s -static")  # Strip binary and static link
endif()

if(MSVC)
  add_definitions(-D_CRT_SECURE_NO_WARNINGS)
endif()

add_library(libparen libparen.cpp)
add_executable(paren paren.cpp)
target_link_libraries(paren libparen)
