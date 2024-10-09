project(ShaderEditor)

# Files import
file(GLOB_RECURSE ESHE_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.*"
)

set(ESHE_SRC_FILES)

source_group("src" FILES ${ESHE_SOURCE_ALL_FILES})

# Apply list
list(APPEND ESHE_SRC_FILES ${ESHE_SOURCE_ALL_FILES})

# Remove unused files
list(FILTER ESHE_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# ShaderEditor project
add_executable(ShaderEditor WIN32 ${ESHE_SRC_FILES})
target_include_directories(ShaderEditor PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_precompile_headers(ShaderEditor PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(ShaderEditor PRIVATE _SHADER_EDITOR)

target_compile_options(ShaderEditor PRIVATE /W0)
target_compile_definitions(ShaderEditor PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(ShaderEditor PUBLIC xrCore)
target_link_libraries(ShaderEditor PUBLIC xrEUI)
target_link_libraries(ShaderEditor PUBLIC xrEProps)
target_link_libraries(ShaderEditor PUBLIC xrECore)
target_link_libraries(ShaderEditor PUBLIC xrSound)

# Move precompiled header
set_target_properties(ShaderEditor PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

file(GLOB_RECURSE ESHE_SOURCE_PCH_FILES "stdafx.*")
source_group("pch" FILES ${ESHE_SOURCE_PCH_FILES})

## OpenAL
target_link_libraries(ShaderEditor PRIVATE ${SND_OAL}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/OpenAL32.lib)
target_include_directories(ShaderEditor PRIVATE "${SND_OAL}/native/include/")

## LibOGG
target_link_libraries(ShaderEditor PRIVATE ${SND_OGG}native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libogg.lib)
target_include_directories(ShaderEditor PRIVATE "${SND_OGG}/native/include/")

## Vorbis
target_link_libraries(ShaderEditor PRIVATE ${SND_VOB}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libvorbisfile.lib)
target_link_libraries(ShaderEditor PRIVATE ${SND_VOB}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libvorbis.lib)
target_include_directories(ShaderEditor PRIVATE "${SND_VOB}/native/include/")
