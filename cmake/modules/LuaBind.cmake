include(FetchContent)

set(LUA_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/src/3rd-party/lua/" CACHE PATH "" FORCE)
set(LUA_LIBRARIES "${LUAJIT_LIB}" CACHE FILEPATH "" FORCE)
set(LUA_LIBRARY "${LUAJIT_LIB}" CACHE FILEPATH "" FORCE)

FetchContent_Declare(
    luabind
    URL https://github.com/ForserX/luabind-latest/archive/refs/heads/master.tar.gz
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)

set(LUABIND_TESTAPP OFF CACHE BOOL "" FORCE)
set(LUABIND_USE_EXTERNAL_LUA OFF CACHE BOOL "" FORCE)

# LuaBind Debug
if (IXRAY_LDEBUG)
    set(LUABIND_DEBUG_SCRIPTS ON CACHE BOOL "" FORCE)
else()
    set(LUABIND_DEBUG_SCRIPTS OFF CACHE BOOL "" FORCE)
endif()

set(LUA_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/src/3rd-party/lua/")
set(LUA_LIB ${LUAJIT_LIB})

FetchContent_MakeAvailable(luabind)

if(APPLE)
    # AI: the engine's scripting layer is written for luabind built without exceptions: it installs
    # AI: its error callbacks in src/xrScripts/script_engine.cpp only under #ifdef
    # AI: LUABIND_NO_EXCEPTIONS, and on Windows that define arrives with the RELWITHDEBINFO/RELEASE
    # AI: configurations. A Debug build on macOS has no such configuration, so luabind threw
    # AI: luabind::error instead, nothing in the engine caught it, and it died with "terminating due to
    # AI: uncaught exception of type luabind::error: lua runtime error" as soon as a script callback
    # AI: failed - seen from CWrapperAbstractZone::STATE_Write when the level's objects went online.
    # AI: Defining it keeps the two halves consistent: luabind reports the Lua error through the
    # AI: engine's own callback. Windows and Linux keep their current behaviour.
    target_compile_definitions(luabind PUBLIC LUABIND_NO_EXCEPTIONS)
endif()

set_target_properties(luabind PROPERTIES FOLDER "3rd Party")