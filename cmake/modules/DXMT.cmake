# DXMT for macOS: D3D11 -> Metal, no Vulkan in between.
#
# The native build (no Wine) links every module into one libdxmt-native.dylib
# and ships the D3D11/DXGI headers the engine compiles against, so a single
# checkout provides both.
#
# Fetched and built here, like every other dependency of the project. Pass
# -DDXMT_NATIVE_DIR=<dir> to use an already built dylib instead.

include(FetchContent)

set(DXMT_REPO "https://github.com/Zagrios/dxmt.git")
set(DXMT_TAG  "85c51603a49b806af22ac5d8a3c040dc92f993df")

set(DXMT_NATIVE_DIR "" CACHE PATH
    "Directory containing libdxmt-native.dylib; empty fetches and builds DXMT")

set(DXMT_LLVM_PATH "/opt/homebrew/opt/llvm@15" CACHE PATH
    "LLVM 15 prefix used by DXMT's shader compiler")

if(DXMT_NATIVE_DIR)
    set(DXMT_LIBRARIES "${DXMT_NATIVE_DIR}/libdxmt-native.dylib")
    set(DXMT_INCLUDE_DIRS "${DXMT_NATIVE_DIR}")
else()
    FetchContent_Declare(dxmt
        GIT_REPOSITORY ${DXMT_REPO}
        GIT_TAG        ${DXMT_TAG}
        GIT_SHALLOW    TRUE
    )

    FetchContent_GetProperties(dxmt)

    if(NOT dxmt_POPULATED)
        FetchContent_Populate(dxmt)

        find_program(MESON meson REQUIRED)

        if(NOT EXISTS "${DXMT_LLVM_PATH}/bin/llvm-config")
            message(FATAL_ERROR
                "LLVM 15 not found in '${DXMT_LLVM_PATH}'. Install it (brew install llvm@15) "
                "or pass -DDXMT_LLVM_PATH=<prefix>.")
        endif()

        set(DXMT_BUILD_DIR "${dxmt_BINARY_DIR}/build-native")
        set(DXMT_INSTALL_DIR "${dxmt_BINARY_DIR}/install")

        if(EXISTS "${DXMT_BUILD_DIR}/meson-private")
            set(DXMT_SETUP_ARGS --reconfigure)
        else()
            set(DXMT_SETUP_ARGS)
        endif()

        message(STATUS "Configuring DXMT native...")

        execute_process(
            COMMAND ${MESON} setup ${DXMT_SETUP_ARGS}
                    ${dxmt_SOURCE_DIR} ${DXMT_BUILD_DIR}
                    --buildtype=release
                    --prefix=${DXMT_INSTALL_DIR}
                    -Dnative_llvm_path=${DXMT_LLVM_PATH}
                    -Denable_tests=false
            RESULT_VARIABLE DXMT_SETUP_RESULT
            OUTPUT_VARIABLE DXMT_SETUP_OUTPUT
            ERROR_VARIABLE  DXMT_SETUP_ERROR
        )

        if(NOT DXMT_SETUP_RESULT EQUAL 0)
            message(FATAL_ERROR "DXMT: meson setup failed:\n${DXMT_SETUP_ERROR}")
        endif()

        execute_process(
            COMMAND ${MESON} compile -C ${DXMT_BUILD_DIR}
            RESULT_VARIABLE DXMT_COMPILE_RESULT
            ERROR_VARIABLE  DXMT_COMPILE_ERROR
        )

        if(NOT DXMT_COMPILE_RESULT EQUAL 0)
            message(FATAL_ERROR "DXMT: meson compile failed:\n${DXMT_COMPILE_ERROR}")
        endif()

        execute_process(
            COMMAND ${MESON} install -C ${DXMT_BUILD_DIR}
            RESULT_VARIABLE DXMT_INSTALL_RESULT
            ERROR_VARIABLE  DXMT_INSTALL_ERROR
        )

        if(NOT DXMT_INSTALL_RESULT EQUAL 0)
            message(FATAL_ERROR "DXMT: meson install failed:\n${DXMT_INSTALL_ERROR}")
        endif()
    endif()

    set(DXMT_LIBRARIES "${DXMT_INSTALL_DIR}/lib/libdxmt-native.dylib")
    set(DXMT_INCLUDE_DIRS "${DXMT_INSTALL_DIR}/include")
endif()

if(NOT EXISTS "${DXMT_LIBRARIES}")
    message(FATAL_ERROR "DXMT: libdxmt-native.dylib not found at '${DXMT_LIBRARIES}'.")
endif()

if(dxmt_SOURCE_DIR)
    set(DXMT_D3D_HEADERS "${dxmt_SOURCE_DIR}/include/native/directx")
else()
    # Prebuilt tree: build-native/src/dxmt-native -> dxmt root
    get_filename_component(DXMT_SOURCE_DIR "${DXMT_NATIVE_DIR}/../../.." ABSOLUTE)
    set(DXMT_D3D_HEADERS "${DXMT_SOURCE_DIR}/include/native/directx")
endif()

if(NOT EXISTS "${DXMT_D3D_HEADERS}/d3d11_4.h")
    message(FATAL_ERROR "DXMT: D3D headers not found in '${DXMT_D3D_HEADERS}'.")
endif()

# The D3D headers DXMT ships include <windows.h>; the engine runs without one,
list(APPEND DXMT_INCLUDE_DIRS
    "${CMAKE_CURRENT_LIST_DIR}/../../src/3rd-party/windows-compat"
    "${DXMT_D3D_HEADERS}")

# Keep the dylib next to the executable, so a run directory only needs the
# engine binaries.
if(CMAKE_BUILD_TYPE)
    set(DXMT_RUNTIME_DIR "${CMAKE_BINARY_DIR}/bin/${CMAKE_BUILD_TYPE}")
else()
    set(DXMT_RUNTIME_DIR "${CMAKE_BINARY_DIR}/bin")
endif()
file(COPY "${DXMT_LIBRARIES}" DESTINATION "${DXMT_RUNTIME_DIR}")
