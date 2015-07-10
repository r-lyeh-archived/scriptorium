project(game)

add_executable(
    game
    ../../source/gamemgr.cpp
    ../../source/gameobj.cpp
    ../../source/main.cpp
    ../../source/scriptmgr.cpp
    ../../../../add_on/scriptbuilder/scriptbuilder.cpp
    ../../../../add_on/scripthandle/scripthandle.cpp
    ../../../../add_on/scriptstdstring/scriptstdstring.cpp
    ../../../../add_on/weakref/weakref.cpp
)
target_link_libraries(game Angelscript)

set_target_properties(game PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/../../bin)
