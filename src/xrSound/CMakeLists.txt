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
file(GLOB_RECURSE SND_SOURCE_3RDI_FILES
    "ogg_utils.*"
    "OpenALDeviceList.*"
    "guids.*"
    "cl_intersect.*"
)
file(GLOB_RECURSE SND_SOURCE_CORE_RECORD_FILES "Recorder/*.*")
file(GLOB_RECURSE SND_SOURCE_CORE_NEW_FILES "New/*.*")

# Source groups
source_group("kernel" FILES ${SND_SOURCE_ALL_FILES})
source_group("render" FILES ${SND_SOURCE_RENDER_FILES})
source_group("core" FILES ${SND_SOURCE_CORE_FILES})
source_group("recorder" FILES ${SND_SOURCE_CORE_RECORD_FILES})
source_group("backend" FILES ${SND_SOURCE_CORE_NEW_FILES})
source_group("3rd Interfaces" FILES ${SND_SOURCE_3RDI_FILES})

# Apply list
list(APPEND SND_SRC_FILES ${SND_SOURCE_ALL_FILES})

if(NOT IXRAY_MP)
    list(FILTER SND_SRC_FILES EXCLUDE REGEX "Recorder/*.*")
endif()

# xrSound project
add_library(xrSound SHARED ${SND_SRC_FILES})
target_include_directories(xrSound PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrSound PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../xrCore/")
target_precompile_headers(xrSound PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(xrSound PRIVATE XRSOUND_EXPORTS)
target_compile_definitions(xrSound PRIVATE _USRDLL)

# Linker list
target_link_libraries(xrSound PUBLIC xrCore)
target_link_libraries(xrSound PUBLIC pffft)
target_link_libraries(xrSound PRIVATE Ogg::ogg)
target_link_libraries(xrSound PRIVATE Vorbis::vorbis)
target_link_libraries(xrSound PRIVATE Vorbis::vorbisfile)

if (IXRAY_ENABLE_RESONANCEAUDIO)
    target_compile_definitions(xrSound PUBLIC DISABLE_STEAM_AUDIO)
    target_link_libraries(xrSound PRIVATE ResonanceAudioStatic)
elseif(DEVIXRAY_ENABLE_STEAMAUDIO)
    target_compile_definitions(xrSound PUBLIC DISABLE_RESONANCE_AUDIO)
    target_link_libraries(xrSound PRIVATE SteamAudio::phonon)
else()
    target_compile_definitions(xrSound PUBLIC DISABLE_STEAM_AUDIO)
    target_compile_definitions(xrSound PUBLIC DISABLE_RESONANCE_AUDIO)
endif()

if (IXRAY_MP)
    target_link_libraries(xrSound PRIVATE SpeexDSP::SpeexDSP)
    target_link_libraries(xrSound PRIVATE Opus::Opus)
	target_compile_definitions(xrSound PUBLIC XR_MP_BUILD)
endif()