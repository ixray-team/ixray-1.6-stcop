# Native D3DCompile / D3DReflect implementation for the macOS D3D11 renderer.
include(ExternalProject)

find_program(VKD3D_MAKE gmake REQUIRED)
find_program(VKD3D_BISON bison
    HINTS /opt/homebrew/opt/bison/bin /usr/local/opt/bison/bin
    REQUIRED
)
execute_process(COMMAND "${VKD3D_BISON}" --version OUTPUT_VARIABLE VKD3D_BISON_VERSION)
if(NOT VKD3D_BISON_VERSION MATCHES "bison[^\n]* ([3-9][.][0-9])")
    message(FATAL_ERROR "vkd3d requires Bison 3 or newer; install Homebrew bison")
endif()
find_path(VKD3D_SPIRV_INCLUDE_DIR spirv/unified1/spirv.h REQUIRED)

set(VKD3D_INSTALL_DIR "${CMAKE_BINARY_DIR}/_deps/vkd3d/install")
set(VKD3D_UTILS_LIBRARY "${VKD3D_INSTALL_DIR}/lib/libvkd3d-utils.1.dylib")

ExternalProject_Add(vkd3d_native
    URL https://dl.winehq.org/vkd3d/source/vkd3d-2.1.tar.xz
    URL_HASH SHA256=7510146aff2adfb4ae07ab890701a607e5ff7c66e57100cfc9f630ec92eeda6a
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
    PREFIX "${CMAKE_BINARY_DIR}/_deps/vkd3d"
    INSTALL_DIR "${VKD3D_INSTALL_DIR}"
    CONFIGURE_COMMAND "${CMAKE_COMMAND}" -E env
        "CC=${CMAKE_C_COMPILER}"
        "BISON=${VKD3D_BISON}"
        "CFLAGS=-O2 -Wno-unknown-attributes -Wno-ignored-attributes"
        "CPPFLAGS=-I${Vulkan_INCLUDE_DIR} -I${VKD3D_SPIRV_INCLUDE_DIR}"
        "SONAME_LIBVULKAN=${Vulkan_LIBRARY}"
        <SOURCE_DIR>/configure --prefix=<INSTALL_DIR>
        --disable-tests --disable-doxygen-doc --disable-static
        --without-xcb --without-opengl
    BUILD_COMMAND "${VKD3D_MAKE}" -j2
    INSTALL_COMMAND "${VKD3D_MAKE}" install-libLTLIBRARIES
    BUILD_BYPRODUCTS "${VKD3D_UTILS_LIBRARY}"
    LOG_CONFIGURE TRUE
    LOG_BUILD TRUE
    LOG_INSTALL TRUE
    LOG_OUTPUT_ON_FAILURE TRUE
)

add_library(VKD3D::Utils SHARED IMPORTED GLOBAL)
set_target_properties(VKD3D::Utils PROPERTIES IMPORTED_LOCATION "${VKD3D_UTILS_LIBRARY}")
add_dependencies(VKD3D::Utils vkd3d_native)

