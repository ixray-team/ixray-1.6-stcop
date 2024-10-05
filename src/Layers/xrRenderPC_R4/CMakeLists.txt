project(xrRender_R4)
set(XRR4_SRC_FILES)

# Files import
file(GLOB_RECURSE XRR4_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
)
file(GLOB_RECURSE XRR4_SOURCE_COMP_FILES CONFIGURE_DEPENDS
    "../xrRender/*.cpp"
    "../xrRender/*.h"
)
file(GLOB_RECURSE XRR4_SOURCE_INCL_FILES CONFIGURE_DEPENDS
    "../../Include/*.h"
)
file(GLOB_RECURSE XRR4_SOURCE_COMP_BLEND_FILES
    "../xrRender/Blender_*.cpp"
    "../xrRender/Blender_*.h"
)
file(GLOB_RECURSE XRR4_SOURCE_BLEND_FILES
    "Blender_*.cpp"
    "Blender_*.h"
)
file(GLOB_RECURSE XRR4_SOURCE_DX9_FILES CONFIGURE_DEPENDS
    "../xrRenderDX10/*.cpp"
    "../xrRenderDX10/*.h"
)
file(GLOB_RECURSE XRR4_SOURCE_LUA_FILES
    "../../xrEngine/ai_script_lua*.cpp"
    "../../xrEngine/ai_script_lua*.h"
)

# Source groups
source_group("render/src" FILES ${XRR4_SOURCE_ALL_FILES})
source_group("render/include" FILES ${XRR4_SOURCE_INCL_FILES})
source_group("layers" FILES ${XRR4_SOURCE_COMP_FILES})
source_group("general_dx11" FILES ${XRR4_SOURCE_DX9_FILES})
source_group("lua_ext" FILES ${XRR4_SOURCE_LUA_FILES})
source_group("blend" FILES ${XRR4_SOURCE_COMP_BLEND_FILES})
source_group("blend" FILES ${XRR4_SOURCE_BLEND_FILES})

# Apply list
list(APPEND XRR4_SRC_FILES ${XRR4_SOURCE_ALL_FILES})
list(APPEND XRR4_SRC_FILES ${XRR4_SOURCE_COMP_FILES})
list(APPEND XRR4_SRC_FILES ${XRR4_SOURCE_DX9_FILES})
list(APPEND XRR4_SRC_FILES ${XRR4_SOURCE_LUA_FILES})
list(APPEND XRR4_SRC_FILES ${XRR4_SOURCE_INCL_FILES})

# Remove unused files
#list(FILTER XRR4_SRC_FILES EXCLUDE REGEX "../xrRender/HW.cpp")

# xrRender_R4 project
add_library(xrRender_R4 SHARED ${XRR4_SRC_FILES})
target_precompile_headers(xrRender_R4 PRIVATE "stdafx.h")

# Include directories
target_include_directories(xrRender_R4 PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrRender_R4 PUBLIC "${IXRAY_SDK_INC}")
target_include_directories(xrRender_R4 PUBLIC "${AMD_FSR2}/build/native/include/")
target_include_directories(xrRender_R4 PUBLIC "${NVIDIA_DLSS_SDK}/include/")

# Project defines
target_compile_definitions(xrRender_R4 PRIVATE XRRENDER_R4_EXPORTS)
target_compile_definitions(xrRender_R4 PRIVATE USE_DX11)
target_compile_definitions(xrRender_R4 PRIVATE _USRDLL)
target_compile_definitions(xrRender_R4 PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrRender_R4 PUBLIC xrAbstractions)
target_link_libraries(xrRender_R4 PUBLIC xrParticles)
target_link_libraries(xrRender_R4 PUBLIC "dxgi.lib")
target_link_libraries(xrRender_R4 PUBLIC "d3d11.lib")
target_link_libraries(xrRender_R4 PUBLIC "d3dcompiler.lib")
target_link_libraries(xrRender_R4 PUBLIC "dxguid.lib")
target_link_libraries(xrRender_R4 PUBLIC "ffx_fsr2_api_${NUGET_PACKAGE_PLATFORM}.lib")
target_link_libraries(xrRender_R4 PUBLIC "ffx_fsr2_api_dx11_${NUGET_PACKAGE_PLATFORM}.lib")

## AMD FidelityFX FSR 2
target_link_directories(xrRender_R4 PUBLIC "${AMD_FSR2}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/")
target_link_libraries(xrRender_R4 PUBLIC "ffx_fsr2_api_${NUGET_PACKAGE_PLATFORM}.lib")
target_link_libraries(xrRender_R4 PUBLIC "ffx_fsr2_api_dx11_${NUGET_PACKAGE_PLATFORM}.lib")
add_custom_command(TARGET ${PROJECT_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${AMD_FSR2}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/ffx_fsr2_api_${NUGET_PACKAGE_PLATFORM}.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${AMD_FSR2}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/ffx_fsr2_api_dx11_${NUGET_PACKAGE_PLATFORM}.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)

if (WIN32 AND "${CMAKE_VS_PLATFORM_NAME}" MATCHES "(x64)")
    target_link_directories(xrRender_R4 PUBLIC "${NVIDIA_DLSS_SDK}/lib/Windows_x86_64/x86_64/")
    target_link_libraries(xrRender_R4 PUBLIC "nvsdk_ngx_d.lib")

    # Copy binaries
    add_custom_command(TARGET ${PROJECT_NAME}
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${NVIDIA_DLSS_SDK}/lib/Windows_x86_64/rel/nvngx_dlss.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    )
endif()

## DirectX SDK
target_include_directories(xrRender_R4 PUBLIC "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_2019.2024.6.5.1/include/")
target_include_directories(xrRender_R4 PUBLIC "${CMAKE_BINARY_DIR}/packages/directxtex_desktop_2019.2024.6.5.1/include/")
target_link_libraries(xrRender_R4 PUBLIC ${CMAKE_BINARY_DIR}/packages/directxtex_desktop_2019.2024.6.5.1/native/lib/${NUGET_PACKAGE_PLATFORM}/release/DirectXTex.lib)

## RenderDoc
target_include_directories(xrRender_R4 PUBLIC "${RENDERDOC_API}")
