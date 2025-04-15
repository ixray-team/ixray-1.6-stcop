project(ETools)
set(ETL_SRC_FILES)

# Files import
file(GLOB_RECURSE ETL_SOURCE_ALL_FILES
    "*.cpp"
    "*.h"
)

# Source groups
source_group("kernel" FILES ${ETL_SOURCE_ALL_FILES})

# Apply list
list(APPEND ETL_SRC_FILES ${ETL_SOURCE_ALL_FILES})

# Remove unused files
#list(FILTER ETL_SRC_FILES EXCLUDE REGEX "nvdxt.cpp$")

# ETools project
add_library(ETools SHARED ${ETL_SRC_FILES})
target_include_directories(ETools PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_precompile_headers(ETools PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(ETools PRIVATE ETOOLS_EXPORTS)
target_compile_definitions(ETools PRIVATE _USRDLL)

# Linker list
target_link_libraries(ETools PUBLIC xrCore)
target_link_libraries(ETools PUBLIC xrCDB)
target_link_libraries(ETools PUBLIC xrSound)
target_link_libraries(ETools PUBLIC imgui)

# DirectX SDK
target_link_libraries(ETools PUBLIC ${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/release/lib/${NUGET_PACKAGE_PLATFORM}/d3dx9.lib)
target_include_directories(ETools PUBLIC "${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/include/")
target_include_directories(ETools PUBLIC "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_2019.2024.6.5.1/include/")

## OGG
target_link_libraries(ETools PRIVATE "${SND_OGG}native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libogg.lib")
target_include_directories(ETools PRIVATE "${SND_OGG}/native/include/")

## Vorbis
target_link_libraries(ETools PRIVATE "${SND_VOB}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libvorbisfile.lib")
target_link_libraries(ETools PRIVATE "${SND_VOB}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libvorbis.lib")
target_include_directories(ETools PRIVATE "${SND_VOB}/native/include/")

## OpenAL
target_link_libraries(ETools PRIVATE "${SND_OAL}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/OpenAL32.lib")
target_include_directories(ETools PRIVATE "${SND_OAL}/native/include/")
