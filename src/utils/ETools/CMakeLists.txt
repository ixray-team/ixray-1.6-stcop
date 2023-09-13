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

# project defines 
target_compile_definitions(ETools PRIVATE ETOOLS_EXPORTS)
target_compile_definitions(ETools PRIVATE _WINDOWS)
target_compile_definitions(ETools PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(ETools PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(ETools PUBLIC xrCore)
target_link_libraries(ETools PUBLIC xrCDB)
target_link_libraries(ETools PUBLIC xrSound)
target_link_libraries(ETools PUBLIC QSlim)

# Packages paths

set(SND_OGG ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibOgg.1.3.5.4/)
set(SND_OAL ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.OpenALSoft.1.23.1.1/)
set(SND_VOB ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibVorbis.1.3.7.4/)

# DX9
target_link_libraries(ETools PUBLIC ${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/release/lib/x86/d3dx9.lib)
target_include_directories(ETools PUBLIC "${CMAKE_BINARY_DIR}/packages/Microsoft.DXSDK.D3DX.9.29.952.8/build/native/include/")
target_include_directories(ETools PUBLIC "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_win10.2023.4.28.1/include/")

## LibOGG
target_link_libraries(ETools PRIVATE ${SND_OGG}native/lib/Win32/Release/libogg.lib)
target_include_directories(ETools PRIVATE "${SND_OGG}/native/include/")

## Vorbice
target_link_libraries(ETools PRIVATE ${SND_VOB}/native/lib/Win32/Release/libvorbisfile.lib)
target_link_libraries(ETools PRIVATE ${SND_VOB}/native/lib/Win32/Release/libvorbis.lib)
target_include_directories(ETools PRIVATE "${SND_VOB}/native/include/")

## OpenAL
target_link_libraries(ETools PRIVATE ${SND_OAL}/native/lib/Win32/Release/OpenAL32.lib)
target_include_directories(ETools PRIVATE "${SND_OAL}/native/include/")
