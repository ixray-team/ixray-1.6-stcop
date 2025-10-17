include(FetchContent)

if(WIN32)
    set(LUAJIT ${CMAKE_BINARY_DIR}/packages/IXRay.LuaJIT.Binaries.win10.0.19041.0-${NUGET_PACKAGE_PLATFORM}.1626960173.0.0-open/)
    set(LUAJIT_NAME lua51.dll)
    set(LUAJIT_LIB ${LUAJIT}lib/lua51.lib)
    set(LUAJIT_BIN ${LUAJIT}bin/${LUAJIT_NAME})
    set(LUAJIT_INC ${LUAJIT}include)

    # Для luabind
    set(LUA_INCLUDE_DIR "${LUAJIT_INC}" CACHE PATH "" FORCE)
    set(LUA_LIBRARIES "${LUAJIT_LIB}" CACHE FILEPATH "" FORCE)
    set(LUA_LIBRARY "${LUAJIT_LIB}" CACHE FILEPATH "" FORCE)

    add_library(Lua::Lua UNKNOWN IMPORTED)
    set_target_properties(Lua::Lua PROPERTIES
        IMPORTED_LOCATION "${LUAJIT_LIB}"
        INTERFACE_INCLUDE_DIRECTORIES "${IXRAY_SDK_INC}/lua/"
    )

    add_imported_lib(
        LuaJIT::lua51
        "${LUAJIT_INC}"
        "${LUAJIT_LIB}"
        "${LUAJIT_BIN}"
    )

else()
    set(CMAKE_POSITION_INDEPENDENT_CODE ON)
    FetchContent_Declare(
        luajit
        GIT_REPOSITORY https://github.com/ixray-team/luajit-ixray.git
        GIT_TAG default
    )
    FetchContent_MakeAvailable(luajit)

    set(LUAJIT_INC "${luajit_SOURCE_DIR}/src")
    set(LUAJIT_LIB "libluajit")

    # Для luabind
    set(LUA_INCLUDE_DIR "${LUAJIT_INC}" CACHE PATH "" FORCE)
    set(LUA_LIBRARIES "${LUAJIT_LIB}" CACHE STRING "" FORCE)
    set(LUA_LIBRARY "${LUAJIT_LIB}" CACHE STRING "" FORCE)

    add_library(Lua::Lua INTERFACE IMPORTED)
    set_target_properties(Lua::Lua PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${LUAJIT_INC}"
        INTERFACE_LINK_LIBRARIES "${LUAJIT_LIB}"
    )

    if(NOT TARGET LuaJIT::lua51)
        add_library(LuaJIT::lua51 INTERFACE IMPORTED)
        set_target_properties(LuaJIT::lua51 PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${LUAJIT_INC}"
            INTERFACE_LINK_LIBRARIES "${LUAJIT_LIB}"
        )
    endif()
endif()
