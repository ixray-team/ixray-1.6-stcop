# vkd3d provides D3DCompile for the R4 shader library on macOS.
#

if(NOT APPLE)
    return()
endif()

include(FetchContent)

set(VKD3D_VERSION "2.1")

FetchContent_Declare(
    vkd3d
    URL "https://dl.winehq.org/vkd3d/source/vkd3d-${VKD3D_VERSION}.tar.xz"
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)

FetchContent_GetProperties(vkd3d)

if(NOT vkd3d_POPULATED)
    FetchContent_Populate(vkd3d)

    find_program(VKD3D_BISON NAMES bison HINTS /opt/homebrew/opt/bison/bin REQUIRED)
    get_filename_component(VKD3D_BISON_DIR "${VKD3D_BISON}" DIRECTORY)

    find_path(VKD3D_VULKAN_INCLUDE_DIR vulkan/vulkan.h
        HINTS /opt/homebrew/opt/vulkan-headers/include REQUIRED)
    find_path(VKD3D_SPIRV_INCLUDE_DIR spirv/unified1/spirv.h
        HINTS /opt/homebrew/opt/spirv-headers/include REQUIRED)

    find_program(VKD3D_MAKE NAMES make HINTS /usr/bin REQUIRED)
    find_program(VKD3D_INSTALL_NAME_TOOL NAMES install_name_tool REQUIRED)

    set(VKD3D_BUILD_DIR "${vkd3d_BINARY_DIR}/build-native")
    file(MAKE_DIRECTORY "${VKD3D_BUILD_DIR}")

    set(VKD3D_BUILD_ENV
        "PATH=${VKD3D_BISON_DIR}:$ENV{PATH}"
        "CPPFLAGS=-I${VKD3D_VULKAN_INCLUDE_DIR} -I${VKD3D_SPIRV_INCLUDE_DIR}"
        "LDFLAGS=-L/opt/homebrew/lib"
    )

    message(STATUS "Configuring vkd3d ${VKD3D_VERSION} for macOS...")
    execute_process(
        COMMAND ${CMAKE_COMMAND} -E env ${VKD3D_BUILD_ENV}
                "${vkd3d_SOURCE_DIR}/configure" --disable-demos --disable-tests --without-spirv-tools
        WORKING_DIRECTORY "${VKD3D_BUILD_DIR}"
        RESULT_VARIABLE VKD3D_CONFIGURE_RESULT
        OUTPUT_VARIABLE VKD3D_CONFIGURE_OUTPUT
        ERROR_VARIABLE  VKD3D_CONFIGURE_ERROR
    )
    if(NOT VKD3D_CONFIGURE_RESULT EQUAL 0)
        message(FATAL_ERROR "vkd3d configure failed: ${VKD3D_CONFIGURE_ERROR}")
    endif()

    message(STATUS "Building vkd3d ${VKD3D_VERSION} for macOS...")
    execute_process(
        COMMAND ${CMAKE_COMMAND} -E env ${VKD3D_BUILD_ENV} "${VKD3D_MAKE}" -j8
        WORKING_DIRECTORY "${VKD3D_BUILD_DIR}"
        RESULT_VARIABLE VKD3D_BUILD_RESULT
        OUTPUT_VARIABLE VKD3D_BUILD_OUTPUT
        ERROR_VARIABLE  VKD3D_BUILD_ERROR
    )
    if(NOT VKD3D_BUILD_RESULT EQUAL 0)
        message(FATAL_ERROR "vkd3d build failed: ${VKD3D_BUILD_ERROR}")
    endif()

    file(GLOB VKD3D_BUILT_DYLIBS "${VKD3D_BUILD_DIR}/.libs/libvkd3d*.dylib")
    foreach(VKD3D_DYLIB IN LISTS VKD3D_BUILT_DYLIBS)
        get_filename_component(VKD3D_DYLIB_NAME "${VKD3D_DYLIB}" NAME)
        if(VKD3D_DYLIB_NAME MATCHES "^libvkd3d.*\\.[0-9]+\\.dylib$")
            execute_process(
                COMMAND "${VKD3D_INSTALL_NAME_TOOL}" -id "@rpath/${VKD3D_DYLIB_NAME}" "${VKD3D_DYLIB}"
                RESULT_VARIABLE VKD3D_ID_RESULT
            )
            if(NOT VKD3D_ID_RESULT EQUAL 0)
                message(FATAL_ERROR "install_name_tool failed for ${VKD3D_DYLIB}")
            endif()
        endif()
    endforeach()

    find_library(VKD3D_UTILS_LIBRARY
        NAMES vkd3d-utils
        PATHS "${VKD3D_BUILD_DIR}/.libs"
        NO_DEFAULT_PATH
        REQUIRED
    )

    set(VKD3D_LIBRARIES "${VKD3D_UTILS_LIBRARY}" CACHE FILEPATH "vkd3d shader compiler library" FORCE)
    set(VKD3D_INCLUDE_DIRS "${vkd3d_SOURCE_DIR}/include" CACHE PATH "vkd3d include directory" FORCE)
endif()

if(APPLE AND VKD3D_BUILD_DIR)
    file(GLOB VKD3D_DYLIBS "${VKD3D_BUILD_DIR}/.libs/libvkd3d*.dylib")
    find_program(VKD3D_INSTALL_NAME_TOOL NAMES install_name_tool REQUIRED)

    foreach(VKD3D_TARGET IN LISTS VKD3D_DYLIBS)
        get_filename_component(VKD3D_TARGET_NAME "${VKD3D_TARGET}" NAME)
        if(NOT VKD3D_TARGET_NAME MATCHES "^libvkd3d.*\\.[0-9]+\\.dylib$")
            continue()
        endif()

        execute_process(
            COMMAND "${VKD3D_INSTALL_NAME_TOOL}" -id "@rpath/${VKD3D_TARGET_NAME}" "${VKD3D_TARGET}"
            RESULT_VARIABLE VKD3D_ID_RESULT
        )
        if(NOT VKD3D_ID_RESULT EQUAL 0)
            message(FATAL_ERROR "install_name_tool -id failed for ${VKD3D_TARGET}")
        endif()

        foreach(VKD3D_DEP IN LISTS VKD3D_DYLIBS)
            get_filename_component(VKD3D_DEP_NAME "${VKD3D_DEP}" NAME)
            if(VKD3D_DEP_NAME MATCHES "^libvkd3d.*\\.[0-9]+\\.dylib$")
                execute_process(
                    COMMAND "${VKD3D_INSTALL_NAME_TOOL}" -change "/usr/local/lib/${VKD3D_DEP_NAME}"
                            "@rpath/${VKD3D_DEP_NAME}" "${VKD3D_TARGET}"
                    RESULT_VARIABLE VKD3D_CHANGE_RESULT
                    OUTPUT_QUIET ERROR_QUIET
                )
            endif()
        endforeach()

        if(CMAKE_BUILD_TYPE)
            set(VKD3D_RUNTIME_DIR "${CMAKE_BINARY_DIR}/bin/${CMAKE_BUILD_TYPE}")
        else()
            set(VKD3D_RUNTIME_DIR "${CMAKE_BINARY_DIR}/bin")
        endif()
        file(COPY "${VKD3D_TARGET}" DESTINATION "${VKD3D_RUNTIME_DIR}")
    endforeach()

    set(VKD3D_DYLIBS "${VKD3D_DYLIBS}" CACHE STRING "vkd3d run-time libraries" FORCE)
endif()
