project(xrSound)
set(SND_SRC_FILES)

# Files import
file(GLOB_RECURSE SND_SOURCE_ALL_FILES CONFIGURE_DEPENDS
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
file(GLOB_RECURSE SND_SOURCE_REC_FILES
    "Recorder/*.*"
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
source_group("recorder" FILES ${SND_SOURCE_REC_FILES})
source_group("3rd Interfaces" FILES ${SND_SOURCE_3RDI_FILES})

# Apply list
list(APPEND SND_SRC_FILES ${SND_SOURCE_ALL_FILES})

if(NOT WIN32)
    list(FILTER SND_SRC_FILES EXCLUDE REGEX "System/NotificationClient.*")
endif()

# xrSound project
add_library(xrSound SHARED ${SND_SRC_FILES})
target_include_directories(xrSound PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_precompile_headers(xrSound PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(xrSound PRIVATE XRSOUND_EXPORTS)
target_compile_definitions(xrSound PRIVATE _USRDLL)

# Linker list
target_link_libraries(xrSound PUBLIC xrCore)

setup_audio_libs(xrSound)