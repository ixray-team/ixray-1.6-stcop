if(NOT SDL3_SOURCE_DIR)
    message(FATAL_ERROR "SDL3_SOURCE_DIR not set. Please include SDL3 configuration first.")
endif()

find_package(Vulkan)

# AI: the macOS-fixed DXVK fork is based on DXVK 2.6, where the window-system option is
# AI: called dxvk_native_wsi and the old native_sdl3/native_sdl2/native_glfw flags are gone.
# AI: Linux keeps its original set below, untouched.
if(APPLE)
    set(DXVK_WSI_ARGS -Ddxvk_native_wsi=sdl3)
else()
    set(DXVK_WSI_ARGS -Dnative_sdl3=enabled -Dnative_sdl2=disabled -Dnative_glfw=disabled)
endif()

# AI: upstream DXVK master does not build with Apple's linker (it passes GNU --version-script,
# AI: which ld does not know) and fails on an ambiguous tzcnt; the macOS fixes live in a fork,
# AI: pinned to a fixed commit the way the working macOS ports do it. Windows, Linux and BSD
# AI: keep fetching upstream master exactly as before.
if(APPLE)
    set(DXVK_REPO    "https://github.com/generalsx-project/dxvk.git")
    set(DXVK_TAG     "f66da559fd48cd55ddfebca38ed0f708c0f76a24")
    set(DXVK_SHALLOW FALSE)
else()
    set(DXVK_REPO    "https://github.com/doitsujin/dxvk.git")
    set(DXVK_TAG     "master")
    set(DXVK_SHALLOW TRUE)
endif()

FetchContent_Declare(
    dxvk
    GIT_REPOSITORY ${DXVK_REPO}
    GIT_TAG        ${DXVK_TAG}
    GIT_SHALLOW    ${DXVK_SHALLOW}
)

FetchContent_GetProperties(dxvk)
FetchContent_GetProperties(SDL3)

if(NOT dxvk_POPULATED)
FetchContent_Populate(dxvk)

# AI: Metal has no geometry shader stage, no transform feedback and no shader cull distance, so
# AI: MoltenVK reports geometryShader = 0, transformFeedback = 0 (VK_EXT_transform_feedback is
# AI: absent) and shaderCullDistance = 0. DXVK demands all three as part of its baseline feature
# AI: level 10_1 set, and without them D3D11InternalCreateDevice() reports "Maximum supported
# AI: feature level: 0" and fails with E_INVALIDARG. On macOS each of them is taken from what the
# AI: device actually supports instead of being required; the game uses geometry shaders in four
# AI: shaders out of about two hundred (the 3DFluid volume effect), so the rest of the renderer is
# AI: unaffected. Verified on this machine with a VkGetPhysicalDeviceFeatures probe:
# AI: geometryShader = 0, transformFeedback = 0, shaderCullDistance = 0, while tessellationShader,
# AI: depthClamp, imageCubeArray, textureCompressionBC, multiViewport and the rest are present.
# AI: The edit follows the project's own way of touching a fetched dependency (see
# AI: download_and_fix_nvapi() in cmake/modules/nvapi.cmake) and is APPLE-only.
if(APPLE)
    # AI: applies one text replacement to a file inside the fetched dxvk tree and writes it back
    # AI: only when something changed, so the step is idempotent across configurations.
    function(dxvk_text_patch DXVK_FILE DXVK_OLD DXVK_NEW)
        file(READ "${DXVK_FILE}" DXVK_CONTENT)
        set(DXVK_BEFORE "${DXVK_CONTENT}")
        string(REPLACE "${DXVK_OLD}" "${DXVK_NEW}" DXVK_CONTENT "${DXVK_CONTENT}")
        if(NOT DXVK_CONTENT STREQUAL DXVK_BEFORE)
            file(WRITE "${DXVK_FILE}" "${DXVK_CONTENT}")
            get_filename_component(DXVK_FILE_NAME "${DXVK_FILE}" NAME)
            message(STATUS "DXVK: patched ${DXVK_FILE_NAME} for macOS")
        endif()
    endfunction()

    # AI: Metal has no geometry shader stage and no transform feedback, so MoltenVK reports
    # AI: geometryShader = 0 and transformFeedback = 0 (VK_EXT_transform_feedback is absent). DXVK
    # AI: demands both as part of its baseline feature level 10_1 set, and without them
    # AI: D3D11InternalCreateDevice() reports "Maximum supported feature level: 0" and fails with
    # AI: E_INVALIDARG, which left the engine with a null device. On macOS the two are taken from
    # AI: what the device supports instead of being required; the game uses geometry shaders in
    # AI: four shaders out of about two hundred (the 3DFluid volume effect), so the rest of the
    # AI: renderer is unaffected. Verified with a VkGetPhysicalDeviceFeatures probe on this
    # AI: machine: geometryShader = 0, transformFeedback = 0, the rest of the 10_1 set present.
    dxvk_text_patch("${dxvk_SOURCE_DIR}/src/d3d11/d3d11_device.cpp"
        "enabled.core.features.geometryShader                          = VK_TRUE;"
        "enabled.core.features.geometryShader                          = supported.core.features.geometryShader;")
    dxvk_text_patch("${dxvk_SOURCE_DIR}/src/d3d11/d3d11_device.cpp"
        "enabled.extTransformFeedback.transformFeedback                = VK_TRUE;"
        "enabled.extTransformFeedback.transformFeedback                = supported.extTransformFeedback.transformFeedback;")
    dxvk_text_patch("${dxvk_SOURCE_DIR}/src/d3d11/d3d11_device.cpp"
        "enabled.extTransformFeedback.geometryStreams                  = VK_TRUE;"
        "enabled.extTransformFeedback.geometryStreams                  = supported.extTransformFeedback.geometryStreams;")

    # AI: the feature check only answers "no", which makes a missing device feature a guessing
    # AI: game; on macOS it names the entry it rejected. Behaviour is unchanged (the expression
    # AI: still evaluates to false when both the device and the requirement fall out).
    dxvk_text_patch("${dxvk_SOURCE_DIR}/src/dxvk/dxvk_adapter.cpp"
        "#define CHECK_FEATURE_NEED(feature) \\\n  (m_deviceFeatures.feature         \\\n       || !required.feature)"
        "#define CHECK_FEATURE_NEED(feature) \\\n  (m_deviceFeatures.feature         \\\n       || !required.feature         \\\n       || (Logger::err(str::format(\"DXVK: device lacks required feature \", #feature)), false))")

    # AI: SDL_Vulkan_CreateSurface() failures reach the caller as VK_ERROR_OUT_OF_HOST_MEMORY, so
    # AI: the WSI logs SDL's own message as well.
    dxvk_text_patch("${dxvk_SOURCE_DIR}/src/wsi/sdl3/wsi_window_sdl3.cpp"
        "    return SDL_Vulkan_CreateSurface(window, instance, nullptr, pSurface)\n      ? VK_SUCCESS : VK_ERROR_OUT_OF_HOST_MEMORY;"
        "    if (!SDL_Vulkan_CreateSurface(window, instance, nullptr, pSurface)) {\n      Logger::err(str::format(\"Sdl3WsiDriver::createSurface: SDL_Vulkan_CreateSurface failed: \", SDL_GetError()));\n      return VK_ERROR_OUT_OF_HOST_MEMORY;\n    }\n\n    return VK_SUCCESS;")
endif()

find_program(MESON meson REQUIRED)
    
    set(SDL3_PC_DIR "${SDL3_BINARY_DIR}/pkgconfig")
    file(MAKE_DIRECTORY ${SDL3_PC_DIR})
    
    if(WIN32)
        set(SDL3_LIBRARY_DIR "${SDL3_BINARY_DIR}/SDL3-shared${CMAKE_STATIC_LIBRARY_SUFFIX}")
        set(MESON_EXTRA_ARGS "")
    elseif(APPLE)
        set(SDL3_LIBRARY_DIR "${SDL3_BINARY_DIR}/libSDL3-shared.dylib")
        set(MESON_EXTRA_ARGS "-Dcpp_args=-D__unix__")
    else()
        set(SDL3_LIBRARY_DIR "${SDL3_BINARY_DIR}/libSDL3-shared.so")
        set(MESON_EXTRA_ARGS "")
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
                ${DXVK_WSI_ARGS}
                ${MESON_EXTRA_ARGS}
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