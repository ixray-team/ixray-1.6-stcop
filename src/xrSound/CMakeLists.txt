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
target_include_directories(xrSound PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_precompile_headers(xrSound PUBLIC "stdafx.h")

# project defines 
target_compile_definitions(xrSound PRIVATE XRSOUND_EXPORTS)
target_compile_definitions(xrSound PRIVATE _WINDOWS)
target_compile_definitions(xrSound PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(xrSound PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrSound PUBLIC xrCore)
target_link_libraries(xrSound PUBLIC xrCDB)

# Nuget
add_custom_command(TARGET ${PROJECT_NAME}
    PRE_BUILD
    COMMAND ${CMAKE_HOME_DIRECTORY}/nuget restore ${CMAKE_CURRENT_SOURCE_DIR}/Packages.config -SolutionDirectory ${CMAKE_CACHEFILE_DIR}
)

## LibOGG
target_link_libraries(xrSound PUBLIC ${CMAKE_CACHEFILE_DIR}/packages/ImeSense.Packages.LibOgg.1.3.5.4/native/lib/Win32/Release/libogg.lib)
target_include_directories(xrSound PUBLIC "${CMAKE_CACHEFILE_DIR}/packages/ImeSense.Packages.LibOgg.1.3.5.4/native/include/")

## Vorbice
target_link_libraries(xrSound PUBLIC ${CMAKE_CACHEFILE_DIR}/packages/ImeSense.Packages.LibVorbis.1.3.7.4/native/lib/Win32/Release/libvorbisfile.lib)
target_link_libraries(xrSound PUBLIC ${CMAKE_CACHEFILE_DIR}/packages/ImeSense.Packages.LibVorbis.1.3.7.4/native/lib/Win32/Release/libvorbis.lib)
target_include_directories(xrSound PUBLIC "${CMAKE_CACHEFILE_DIR}/packages/ImeSense.Packages.LibVorbis.1.3.7.4/native/include/")

## OpenAL
target_link_libraries(xrSound PUBLIC ${CMAKE_CACHEFILE_DIR}/packages/ImeSense.Packages.OpenALSoft.1.23.1.1/native/lib/Win32/Release/OpenAL32.lib)
target_include_directories(xrSound PUBLIC "${CMAKE_CACHEFILE_DIR}/packages/ImeSense.Packages.OpenALSoft.1.23.1.1/native/include/")
