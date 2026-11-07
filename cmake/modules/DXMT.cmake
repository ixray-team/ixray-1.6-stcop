include(FetchContent)
include(ExternalProject)

set(IXRAY_APPLE_DEVELOPER_DIR "/Applications/Xcode.app/Contents/Developer" CACHE PATH "Xcode developer directory containing the Metal toolchain")
set(IXRAY_LLVM15_ROOT "/opt/homebrew/opt/llvm@15" CACHE PATH "Native LLVM 15 installation with static libraries")
if(NOT EXISTS "${IXRAY_LLVM15_ROOT}/include/llvm/Config/llvm-config.h")
    message(FATAL_ERROR "DXMT requires LLVM 15 headers and static libraries. Set IXRAY_LLVM15_ROOT (brew install llvm@15).")
endif()
file(STRINGS "${IXRAY_LLVM15_ROOT}/include/llvm/Config/llvm-config.h" DXMT_LLVM_VERSION REGEX "^#define LLVM_VERSION_MAJOR ")
if(NOT DXMT_LLVM_VERSION MATCHES "LLVM_VERSION_MAJOR 15$")
    message(FATAL_ERROR "DXMT requires exactly LLVM major version 15")
endif()
find_program(DXMT_MESON meson REQUIRED)
find_program(DXMT_XCRUN xcrun REQUIRED)
execute_process(
    COMMAND "${CMAKE_COMMAND}" -E env "DEVELOPER_DIR=${IXRAY_APPLE_DEVELOPER_DIR}"
        "${DXMT_XCRUN}" -sdk macosx metal --version
    RESULT_VARIABLE DXMT_METAL_RESULT OUTPUT_QUIET ERROR_VARIABLE DXMT_METAL_ERROR
)
if(NOT DXMT_METAL_RESULT EQUAL 0)
    message(FATAL_ERROR "DXMT requires the Xcode Metal toolchain: ${DXMT_METAL_ERROR}\nRun DEVELOPER_DIR=${IXRAY_APPLE_DEVELOPER_DIR} xcodebuild -downloadComponent MetalToolchain")
endif()

FetchContent_Declare(dxmt
    GIT_REPOSITORY https://github.com/3Shain/dxmt.git
    GIT_TAG 647b0020ba5764e5593b12407afd76964fa11e20
    GIT_SUBMODULES include/native/directx
    GIT_SUBMODULES_RECURSE FALSE
)
FetchContent_GetProperties(dxmt)
if(NOT dxmt_POPULATED)
    FetchContent_Populate(dxmt)
endif()

set(DXMT_NATIVE_BUILD_DIR "${dxmt_BINARY_DIR}/native")
set(DXMT_D3D11_LIBRARY "${DXMT_NATIVE_BUILD_DIR}/src/d3d11/d3d11.dylib")
set(DXMT_DXGI_LIBRARY "${DXMT_NATIVE_BUILD_DIR}/src/dxgi/dxgi.dylib")
set(DXMT_METAL_LIBRARY "${DXMT_NATIVE_BUILD_DIR}/src/nativemetal/winemetal.dylib")
ExternalProject_Add(dxmt_native
    SOURCE_DIR "${dxmt_SOURCE_DIR}"
    BINARY_DIR "${DXMT_NATIVE_BUILD_DIR}"
    DOWNLOAD_COMMAND ""
    UPDATE_COMMAND ""
    CONFIGURE_COMMAND "${CMAKE_COMMAND}" -E env "DEVELOPER_DIR=${IXRAY_APPLE_DEVELOPER_DIR}"
        "CC=${CMAKE_C_COMPILER}" "CXX=${CMAKE_CXX_COMPILER}"
        "${DXMT_MESON}" setup <BINARY_DIR> <SOURCE_DIR>
        --buildtype=release "-Dnative_llvm_path=${IXRAY_LLVM15_ROOT}" "-Dc_args=-x objective-c"
    BUILD_COMMAND "${CMAKE_COMMAND}" -E env "DEVELOPER_DIR=${IXRAY_APPLE_DEVELOPER_DIR}"
        "${DXMT_MESON}" compile -C <BINARY_DIR> -j4
    INSTALL_COMMAND ""
    BUILD_BYPRODUCTS "${DXMT_D3D11_LIBRARY}" "${DXMT_DXGI_LIBRARY}" "${DXMT_METAL_LIBRARY}"
    LOG_CONFIGURE TRUE LOG_BUILD TRUE LOG_OUTPUT_ON_FAILURE TRUE
)
add_library(DXMT::D3D11 SHARED IMPORTED GLOBAL)
set_target_properties(DXMT::D3D11 PROPERTIES IMPORTED_LOCATION "${DXMT_D3D11_LIBRARY}")
add_dependencies(DXMT::D3D11 dxmt_native)
add_library(DXMT::DXGI SHARED IMPORTED GLOBAL)
set_target_properties(DXMT::DXGI PROPERTIES IMPORTED_LOCATION "${DXMT_DXGI_LIBRARY}")
add_dependencies(DXMT::DXGI dxmt_native)
