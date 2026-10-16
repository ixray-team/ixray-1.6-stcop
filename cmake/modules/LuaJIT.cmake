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
        INTERFACE_INCLUDE_DIRECTORIES "${CMAKE_SOURCE_DIR}/src/3rd-party/lua/"
    )

    add_imported_lib(
        LuaJIT::lua51
        "${LUAJIT_INC}"
        "${LUAJIT_LIB}"
        "${LUAJIT_BIN}"
    )

else()
    set(CMAKE_POSITION_INDEPENDENT_CODE ON)
    if(APPLE)
        # AI: macOS builds OpenXRay's LuaJIT fork (OpenXRay/LuaJIT), which is what the OpenXRay engine
        # AI: runs on Apple Silicon: measured on this Mac, that build keeps the JIT enabled and works,
        # AI: while the project's own fork (ixray-team/luajit-ixray) ends up in lj_mcode.c's
        # AI: "OS probably ignores hints?" path (LJ_TRERR_MCODEAL -> unprotected Lua panic) as soon as
        # AI: the renderer creates its Lua state. Both forks are the same "2.1.0-beta3" generation, so
        # AI: the engine's scripting layer stays compatible. See references/luaJIT-arm-issue.md in the
        # AI: openxray-build skill for the measurements behind this. Windows and Linux keep the fork.
        FetchContent_Declare(
            luajit_apple
            GIT_REPOSITORY https://github.com/OpenXRay/LuaJIT.git
            GIT_TAG 5a5cd82e435a7b08d61b4e0af887493d24d9eca1
        )
        FetchContent_GetProperties(luajit_apple)
        if(NOT luajit_apple_POPULATED)
            FetchContent_Populate(luajit_apple)
        endif()

        # AI: upstream returns a value from its void mcode_setprot() in the MAP_JIT branch, which
        # AI: current clang rejects as an error; that branch is unused here, so the return goes.
        file(READ "${luajit_apple_SOURCE_DIR}/src/lj_mcode.c" LUAJIT_MCODE_SRC)
        set(LUAJIT_MCODE_BEFORE "${LUAJIT_MCODE_SRC}")
        string(REPLACE
            "  pthread_jit_write_protect_np((prot & PROT_EXEC));\n  return 0;\n"
            "  pthread_jit_write_protect_np((prot & PROT_EXEC));\n"
            LUAJIT_MCODE_SRC "${LUAJIT_MCODE_SRC}")
        if(NOT LUAJIT_MCODE_SRC STREQUAL LUAJIT_MCODE_BEFORE)
            file(WRITE "${luajit_apple_SOURCE_DIR}/src/lj_mcode.c" "${LUAJIT_MCODE_SRC}")
            message(STATUS "LuaJIT: dropped upstream's void return in lj_mcode.c, clang rejects it")
        endif()

        # AI: upstream ships a Makefile (the fork shipped a CMake project, which is why this module
        # AI: used FetchContent_MakeAvailable before). Built once, incremental after that. The shared
        # AI: library matters: luabind links LuaJIT directly (luabind/luabind/CMakeLists.txt) and a
        # AI: static library used to be absorbed into libluabind.dylib, so the process ended up with
        # AI: two LuaJIT copies - one inside luabind, one inside xrScripts - and the garbage collector
        # AI: of one span on objects of the other, which hung luabind::open() forever.
        set(LUAJIT_APPLE_LIB_DIR "${CMAKE_BINARY_DIR}/luajit")
        set(LUAJIT_APPLE_DYLIB "${LUAJIT_APPLE_LIB_DIR}/libluajit.dylib")

        if(NOT EXISTS "${LUAJIT_APPLE_DYLIB}")
            if(CMAKE_OSX_DEPLOYMENT_TARGET)
                set(LUAJIT_APPLE_DEPLOYMENT_TARGET "${CMAKE_OSX_DEPLOYMENT_TARGET}")
            else()
                set(LUAJIT_APPLE_DEPLOYMENT_TARGET "11.0")
            endif()

            # AI: upstream builds with its own Makefile, and CMAKE_MAKE_PROGRAM is ninja in this
            # AI: project, so the system make is looked up explicitly.
            find_program(LUAJIT_MAKE_PROGRAM NAMES make gmake REQUIRED)

            execute_process(
                COMMAND "${LUAJIT_MAKE_PROGRAM}" -j8 BUILDMODE=dynamic
                WORKING_DIRECTORY "${luajit_apple_SOURCE_DIR}"
                ENVIRONMENT "MACOSX_DEPLOYMENT_TARGET=${LUAJIT_APPLE_DEPLOYMENT_TARGET}"
                RESULT_VARIABLE LUAJIT_BUILD_RESULT
                OUTPUT_VARIABLE LUAJIT_BUILD_OUTPUT
                ERROR_VARIABLE LUAJIT_BUILD_OUTPUT
            )
            if(NOT LUAJIT_BUILD_RESULT EQUAL 0)
                message(FATAL_ERROR "LuaJIT: upstream build failed:\n${LUAJIT_BUILD_OUTPUT}")
            endif()

            # AI: upstream names it libluajit.so and gives it the install name
            # AI: /usr/local/lib/libluajit-5.1.2.dylib, which does not exist at run time; the library
            # AI: is taken next to the binaries and pointed at through @rpath, like the other
            # AI: dependencies (see cmake/clang.cmake for the run path).
            file(MAKE_DIRECTORY "${LUAJIT_APPLE_LIB_DIR}")
            configure_file("${luajit_apple_SOURCE_DIR}/src/libluajit.so"
                           "${LUAJIT_APPLE_DYLIB}" COPYONLY)
            find_program(LUAJIT_INSTALL_NAME_TOOL NAMES install_name_tool REQUIRED)
            execute_process(
                COMMAND "${LUAJIT_INSTALL_NAME_TOOL}" -id "@rpath/libluajit.dylib" "${LUAJIT_APPLE_DYLIB}"
                RESULT_VARIABLE LUAJIT_ID_RESULT
            )
            if(NOT LUAJIT_ID_RESULT EQUAL 0)
                message(FATAL_ERROR "LuaJIT: install_name_tool -id failed")
            endif()

            if(CMAKE_BUILD_TYPE)
                set(LUAJIT_APPLE_RUNTIME_DIR "${CMAKE_BINARY_DIR}/bin/${CMAKE_BUILD_TYPE}")
            else()
                set(LUAJIT_APPLE_RUNTIME_DIR "${CMAKE_BINARY_DIR}/bin")
            endif()
            file(COPY "${LUAJIT_APPLE_DYLIB}" DESTINATION "${LUAJIT_APPLE_RUNTIME_DIR}")

            message(STATUS "LuaJIT: built upstream libluajit.dylib for macOS")
        endif()

        set(LUAJIT_INC "${luajit_apple_SOURCE_DIR}/src")
        set(LUAJIT_LIB "${LUAJIT_APPLE_DYLIB}")
    else()
        FetchContent_Declare(
            luajit
            GIT_REPOSITORY https://github.com/ixray-team/luajit-ixray.git
            GIT_TAG default
        )
        FetchContent_MakeAvailable(luajit)

        set(LUAJIT_INC "${luajit_SOURCE_DIR}/src")
        set(LUAJIT_LIB "libluajit")
    endif()

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
