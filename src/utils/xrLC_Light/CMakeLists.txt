project(xrLC_Light)
set(LCL_SRC_FILES)

# Files import
file(GLOB_RECURSE LCL_SOURCE_ALL_FILES
    "${IXRAY_SDK_INC}/hxgrid/Interface/hxGridInterface.cpp"
    "../../xrEngine/xrLoadSurface.cpp"
    "*.cpp"
    "*.h"
)

# Source groups
source_group("kernel" FILES ${LCL_SOURCE_ALL_FILES})

# Apply list
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

# Linker list
target_link_libraries(xrLC_Light PUBLIC xrCore)
target_link_libraries(xrLC_Light PUBLIC xrDXT)
target_link_libraries(xrLC_Light PUBLIC xrCDB)

# Nuget
add_custom_command(TARGET ${PROJECT_NAME}
    PRE_BUILD
    COMMAND ${NUGET_COMMAND} restore ${CMAKE_CURRENT_SOURCE_DIR}/Packages.config -SolutionDirectory ${CMAKE_BINARY_DIR}
)

add_custom_command(TARGET ${PROJECT_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Zlib.1.2.13.1/native/bin/Win32/Release/zlib.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)


#target_link_libraries(xrLC_Light PUBLIC ${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/release/lib/x86/d3dx9.lib)
target_include_directories(xrLC_Light PUBLIC "${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/include/")
target_include_directories(xrLC_Light PUBLIC "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_win10.2023.4.28.1/include/")

# Zlib
target_include_directories(xrLC_Light PUBLIC "${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Zlib.1.2.13.1/native/include/")
target_link_libraries(xrLC_Light PUBLIC ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Zlib.1.2.13.1/native/lib/Win32/Release/zlib.lib)
