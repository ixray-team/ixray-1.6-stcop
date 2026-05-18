if(NOT SDL3_SOURCE_DIR)
    message(FATAL_ERROR "SDL3_SOURCE_DIR not set. Please include SDL3 configuration first.")
endif()

find_package(Vulkan)

FetchContent_Declare(
    dxvk
    GIT_REPOSITORY https://github.com/doitsujin/dxvk.git
    GIT_TAG        v2.7.1
    GIT_SHALLOW    TRUE
)

FetchContent_GetProperties(dxvk)
FetchContent_GetProperties(SDL3)

if(NOT dxvk_POPULATED)
    FetchContent_Populate(dxvk)
    find_program(MESON meson REQUIRED)
    
    set(SDL3_PC_DIR "${SDL3_BINARY_DIR}/pkgconfig")
    file(MAKE_DIRECTORY ${SDL3_PC_DIR})
    
    if(WIN32)
        set(SDL3_LIBRARY_DIR "${SDL3_BINARY_DIR}/SDL3-shared${CMAKE_STATIC_LIBRARY_SUFFIX}")
    else()
        set(SDL3_LIBRARY_DIR "${SDL3_BINARY_DIR}/libSDL3-shared.so")
    endif()
    
    # Создаём файл sdl3.pc
    set(SDL3_PC_FILE "${SDL3_PC_DIR}/sdl3.pc")
    file(WRITE ${SDL3_PC_FILE} "
prefix=${SDL3_BINARY_DIR}
exec_prefix=\${prefix}
libdir=\${prefix}
includedir=${SDL3_SOURCE_DIR}/include

Name: sdl3
Description: Simple DirectMedia Layer 3
Version: 3.2.22
Requires:
Libs: -L\${libdir} -lSDL3-shared
Cflags: -I\${includedir}
")
    
    message(STATUS "Created sdl3.pc at: ${SDL3_PC_FILE}")
    set(ENV{PKG_CONFIG_PATH} "${SDL3_PC_DIR}:$ENV{PKG_CONFIG_PATH}")
    set(ENV{CMAKE_PREFIX_PATH} "${SDL3_BINARY_DIR}:$ENV{CMAKE_PREFIX_PATH}")
    
    if(NOT EXISTS "${SDL3_BINARY_DIR}/libSDL3-shared.so")
        message(WARNING "SDL3 shared library not found at ${SDL3_BINARY_DIR}/libSDL3-shared.so")
        message(STATUS "Looking for SDL3 library in ${SDL3_BINARY_DIR}")
        file(GLOB SDL3_LIB_FOUND "${SDL3_BINARY_DIR}/*SDL3*")
        message(STATUS "Found: ${SDL3_LIB_FOUND}")
    endif()
    
    set(DXVK_BUILD_DIR "${dxvk_BINARY_DIR}/build-native")
    set(DXVK_INSTALL_DIR "${dxvk_BINARY_DIR}/install")
    
    message(STATUS "Configuring DXVK Native with SDL3 backend...")
    message(STATUS "PKG_CONFIG_PATH=${SDL3_PC_DIR}")
    
    # === ЗАПУСКАЕМ MESON ===
    execute_process(
        COMMAND ${MESON} setup 
                ${dxvk_SOURCE_DIR} ${DXVK_BUILD_DIR}
                --buildtype=debug
                --prefix=${DXVK_INSTALL_DIR}
                -Denable_d3d9=true
                -Denable_d3d11=true
                -Dnative_sdl3=enabled   
                -Dnative_sdl2=disabled
                -Dnative_glfw=disabled
        RESULT_VARIABLE MESON_CONFIG_RESULT
        ERROR_VARIABLE  MESON_CONFIG_ERROR
    )
    
    if(NOT MESON_CONFIG_RESULT EQUAL 0)
        message(FATAL_ERROR "meson setup failed: ${MESON_CONFIG_ERROR}")
    endif()
    
    # Компиляция
    execute_process(
        COMMAND ${MESON} compile -C ${DXVK_BUILD_DIR}
        RESULT_VARIABLE MESON_COMPILE_RESULT
        ERROR_VARIABLE  MESON_COMPILE_ERROR
    )
    
    if(NOT MESON_COMPILE_RESULT EQUAL 0)
        message(FATAL_ERROR "meson compile failed: ${MESON_COMPILE_ERROR}")
    endif()
    
    # Установка
    execute_process(
        COMMAND ${MESON} install -C ${DXVK_BUILD_DIR}
        RESULT_VARIABLE MESON_INSTALL_RESULT
        ERROR_VARIABLE  MESON_INSTALL_ERROR
    )
    
    # install
    execute_process(
        COMMAND ${MESON} install -C ${DXVK_BUILD_DIR}
        RESULT_VARIABLE MESON_INSTALL_RESULT
    )

    if(NOT MESON_INSTALL_RESULT EQUAL 0)
        message(FATAL_ERROR "meson install failed")
    endif()

    # detect actual lib dir
    find_library(DXVK_D3D9
        NAMES dxvk_d3d9 libdxvk_d3d9
        PATHS
            ${DXVK_INSTALL_DIR}/lib
            ${DXVK_INSTALL_DIR}/lib64
            ${DXVK_INSTALL_DIR}/lib/x86_64-linux-gnu
        NO_DEFAULT_PATH
        REQUIRED
    )

    find_library(DXVK_D3D11
        NAMES dxvk_d3d11 libdxvk_d3d11
        PATHS
            ${DXVK_INSTALL_DIR}/lib
            ${DXVK_INSTALL_DIR}/lib64
            ${DXVK_INSTALL_DIR}/lib/x86_64-linux-gnu
        NO_DEFAULT_PATH
        REQUIRED
    )

    set(DXVK_LIBRARIES 
        ${DXVK_D3D9}
        ${DXVK_D3D11}
        CACHE PATH "kal" FORCE
    )

    ##if(NOT MESON_INSTALL_RESULT EQUAL 0)
    ##    message(FATAL_ERROR "meson install failed: ${MESON_INSTALL_ERROR}")
    ##endif()
    ##
    ##set(DXVK_LIB_DIR "${DXVK_INSTALL_DIR}/lib64")
    ##
    ##if(NOT EXISTS ${DXVK_LIB_DIR})
    ##    set(DXVK_LIB_DIR "${DXVK_INSTALL_DIR}/lib")
    ##endif()

    ##set(DXVK_LIBRARIES ${DXVK_LIB_DIR}/libdxvk_d3d9.so ${DXVK_LIB_DIR}/libdxvk_d3d11.so CACHE FILEPATH "DXVK native libraries" FORCE)
    set(DXVK_INCLUDE_DIRS 
        "${DXVK_INSTALL_DIR}/include/dxvk" 
        CACHE PATH "DXVK native include directories" FORCE
    )
endif()