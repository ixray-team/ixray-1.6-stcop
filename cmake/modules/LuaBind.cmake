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

set_target_properties(luabind PROPERTIES FOLDER "3rd Party")