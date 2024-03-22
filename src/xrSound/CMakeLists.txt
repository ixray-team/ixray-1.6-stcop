project(xrSound)
set(SND_SRC_FILES)

# Files import
file(GLOB_RECURSE SND_SOURCE_ALL_FILES
    "*.cpp"
    "*.h"
)
file(GLOB_RECURSE SND_SOURCE_RENDER_FILES
    "SoundRender*.cpp"
    "SoundRender*.h"
)
file(GLOB_RECURSE SND_SOURCE_CORE_FILES
    "SoundRender_Core*.cpp"
    "SoundRender_Core*.h"
)
file(GLOB_RECURSE SND_SOURCE_TRG_FILES
    "SoundRender_Target*.cpp"
    "SoundRender_Target*.h"
)
file(GLOB_RECURSE SND_SOURCE_EMIT_FILES
    "SoundRender_Emitter*.cpp"
    "SoundRender_Emitter*.h"
)
file(GLOB_RECURSE SND_SOURCE_3RDI_FILES
    "ogg_utils.*"
    "OpenALDeviceList.*"
    "guids.*"
    "cl_intersect.*"
)

# Source groups
source_group("kernel" FILES ${SND_SOURCE_ALL_FILES})
source_group("render" FILES ${SND_SOURCE_RENDER_FILES})
source_group("core" FILES ${SND_SOURCE_CORE_FILES})
source_group("target" FILES ${SND_SOURCE_TRG_FILES})
source_group("emitter" FILES ${SND_SOURCE_EMIT_FILES})
source_group("3rd Interfaces" FILES ${SND_SOURCE_3RDI_FILES})

# Apply list
list(APPEND SND_SRC_FILES ${SND_SOURCE_ALL_FILES})

# Remove unused files
#list(FILTER SND_SRC_FILES EXCLUDE REGEX ".cpp$")

# xrSound project
add_library(xrSound SHARED ${SND_SRC_FILES})
target_include_directories(xrSound PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_precompile_headers(xrSound PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(xrSound PRIVATE XRSOUND_EXPORTS)
target_compile_definitions(xrSound PRIVATE _WINDOWS)
target_compile_definitions(xrSound PRIVATE LUABIND_NO_EXCEPTIONS)
target_compile_definitions(xrSound PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(xrSound PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrSound PUBLIC xrCore)
target_link_libraries(xrSound PUBLIC xrCDB)

# Nuget
set(SND_OGG ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibOgg.1.3.5.4/)
set(SND_OAL ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.OpenALSoft.1.23.1.1/)
set(SND_VOB ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibVorbis.1.3.7.4/)

add_custom_command(TARGET ${PROJECT_NAME}
    PRE_BUILD
    COMMAND ${NUGET_COMMAND} restore ${CMAKE_CURRENT_SOURCE_DIR}/Packages.config -SolutionDirectory ${CMAKE_BINARY_DIR}
)

add_custom_command(TARGET ${PROJECT_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SND_OGG}native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/libogg.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SND_OAL}native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/OpenAL32.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SND_VOB}native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/libvorbisfile.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SND_VOB}native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/libvorbis.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)

## LibOGG
target_link_libraries(xrSound PRIVATE ${SND_OGG}native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libogg.lib)
target_include_directories(xrSound PRIVATE "${SND_OGG}/native/include/")

## Vorbis
target_link_libraries(xrSound PRIVATE ${SND_VOB}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libvorbisfile.lib)
target_link_libraries(xrSound PRIVATE ${SND_VOB}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libvorbis.lib)
target_include_directories(xrSound PRIVATE "${SND_VOB}/native/include/")

## OpenAL
target_link_libraries(xrSound PRIVATE ${SND_OAL}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/OpenAL32.lib)
target_include_directories(xrSound PRIVATE "${SND_OAL}/native/include/")
