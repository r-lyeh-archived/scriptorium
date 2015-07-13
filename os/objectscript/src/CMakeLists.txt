cmake_minimum_required(VERSION 2.8)

set(target os)
project(${target})

list(APPEND HEADERS objectscript.h)
list(APPEND HEADERS os-binder-arg-cc-functions.h)
list(APPEND HEADERS os-binder-cc-functions.h)
list(APPEND HEADERS os-binder-function.h)
list(APPEND HEADERS os-binder.h)

list(APPEND SOURCES objectscript.cpp)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR})

if(NOT "${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
  add_custom_target(strip_${target} ALL COMMAND ${CMAKE_STRIP} ${CMAKE_CURRENT_BINARY_DIR}/${target})
endif()

add_executable(${target} ${SOURCES} ${HEADERS})
target_link_libraries(${target} ${Libedit_LIBRARIES})
install(TARGETS ${target} RUNTIME DESTINATION bin)
