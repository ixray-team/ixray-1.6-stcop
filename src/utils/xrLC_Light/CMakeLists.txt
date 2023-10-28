project(xrLC_Light)
set(LCL_SRC_FILES)

# Files import
file(GLOB_RECURSE LCL_SOURCE_ALL_FILES
    "${IXRAY_SDK_INC}/hxgrid/Interface/hxGridInterface.cpp"
    "../../xrEngine/xrLoadSurface.cpp"
    "*.cpp"
    "*.h"
)

file(GLOB_RECURSE LCL_SOURCE_GPU_ALL_FILES
    "*.cl"
    "*.cu"
)

# Source groups
source_group("kernel" FILES ${LCL_SOURCE_ALL_FILES})
source_group("gpu" FILES ${LCL_SOURCE_GPU_ALL_FILES})

# Apply list
list(APPEND LCL_SRC_FILES ${LCL_SOURCE_GPU_ALL_FILES})
list(APPEND LCL_SRC_FILES ${LCL_SOURCE_ALL_FILES})

# xrLC_Light project
add_library(xrLC_Light SHARED ${LCL_SRC_FILES})
target_include_directories(xrLC_Light PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrLC_Light PUBLIC "${IXRAY_SDK_INC}")
target_link_directories(xrLC_Light PUBLIC "${IXRAY_SDK_LIB}")

target_precompile_headers(xrLC_Light PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(xrLC_Light PRIVATE XRLC_LIGHT_EXPORTS)
target_compile_definitions(xrLC_Light PRIVATE _WINDOWS)
target_compile_definitions(xrLC_Light PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(xrLC_Light PRIVATE "$<$<CONFIG:Debug>:DEBUG>")
target_link_libraries(xrLC_Light PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/LightPoint.obj")

# Linker list
target_link_libraries(xrLC_Light PUBLIC xrCore)
target_link_libraries(xrLC_Light PUBLIC xrDXT)
target_link_libraries(xrLC_Light PUBLIC xrCDB)
#target_link_libraries(xrLC_Light PRIVATE "Calc.lib")
target_link_libraries(xrLC_Light PRIVATE "cuda.lib")
target_link_libraries(xrLC_Light PRIVATE "optix_prime.1.lib")
target_link_libraries(xrLC_Light PRIVATE "cudart.lib")

target_include_directories(xrLC_Light PUBLIC "${IXRAY_SDK_INC}/optix")
target_include_directories(xrLC_Light PUBLIC "${IXRAY_SDK_INC}/cuda")

# Nuget
add_custom_command(TARGET ${PROJECT_NAME}
    PRE_BUILD
    COMMAND ${NUGET_COMMAND} restore ${CMAKE_CURRENT_SOURCE_DIR}/Packages.config -SolutionDirectory ${CMAKE_BINARY_DIR}
)

add_custom_command(TARGET ${PROJECT_NAME}
    PRE_LINK
    COMMAND ${IXRAY_SDK_TOOLS}/nvcc/compiler/nvcc.exe "${CMAKE_CURRENT_SOURCE_DIR}/LightPoint.cu" -I "${IXRAY_SDK_INC}/cuda" -maxrregcount=0  --machine 64 --compile -cudart static -o "${CMAKE_CURRENT_SOURCE_DIR}/LightPoint.obj" -Xcompiler "/EHsc /W3 /nologo /Od /FS /Zi /RTC1 /MD " -g   -DFORCE_NO_EXCEPTIONS -D_USE_MATH_DEFINES -DWIN32 -D_WINDOWS -D_USRDLL -DXRLC_LIGHT_EXPORTS -D_SECURE_SCL=0 -D_ITERATOR_DEBUG_LEVEL=0 -DWIN32 -D_WINSOCK_DEPRECATED_NO_WARNINGS -D_CRT_SECURE_NO_WARNINGS -D_WINDLL -D_MBCS
)

add_custom_command(TARGET ${PROJECT_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Zlib.1.2.13.1/native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/zlib.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${IXRAY_SDK_BIN}optix_prime.1.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${IXRAY_SDK_BIN}Calc.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)
# DirectX SDK
target_include_directories(xrLC_Light PUBLIC "${CMAKE_BINARY_DIR}/packages/directxtex_desktop_win10.2023.6.14.1/include/")

#target_link_libraries(xrLC_Light PUBLIC ${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/release/lib/x86/d3dx9.lib)
target_include_directories(xrLC_Light PUBLIC "${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/include/")
target_include_directories(xrLC_Light PUBLIC "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_win10.2023.4.28.1/include/")

# Zlib
target_include_directories(xrLC_Light PUBLIC "${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Zlib.1.2.13.1/native/include/")
target_link_libraries(xrLC_Light PUBLIC ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Zlib.1.2.13.1/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/zlib.lib)
