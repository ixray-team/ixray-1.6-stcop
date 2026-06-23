project(xrSound)
set(SND_SRC_FILES)

# Files import
file(GLOB_RECURSE SND_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.h"
)

source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}" FILES ${SND_SOURCE_ALL_FILES})

# Apply list
list(APPEND SND_SRC_FILES ${SND_SOURCE_ALL_FILES})

if(NOT IXRAY_MP)
    list(FILTER SND_SRC_FILES EXCLUDE REGEX "Recorder/*.*")
endif()

if (IXRAY_ENABLE_RESONANCEAUDIO)
	list(FILTER SND_SRC_FILES EXCLUDE REGEX "Plugins/SteamAudio.*")
elseif(DEVIXRAY_ENABLE_STEAMAUDIO)
	list(FILTER SND_SRC_FILES EXCLUDE REGEX "Plugins/ResonanceAudio.*")
else()
	list(FILTER SND_SRC_FILES EXCLUDE REGEX "Plugins/SteamAudio.*")
	list(FILTER SND_SRC_FILES EXCLUDE REGEX "Plugins/ResonanceAudio.*")
endif()

# xrSound project
add_library(xrSound SHARED ${SND_SRC_FILES})
target_include_directories(xrSound PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrSound PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/..")
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
    target_compile_definitions(xrSound PRIVATE DISABLE_STEAM_AUDIO)
    target_link_libraries(xrSound PRIVATE ResonanceAudioStatic)
elseif(DEVIXRAY_ENABLE_STEAMAUDIO)
    target_compile_definitions(xrSound PRIVATE DISABLE_RESONANCE_AUDIO)
    target_link_libraries(xrSound PRIVATE SteamAudio::phonon)
else()
    target_compile_definitions(xrSound PRIVATE DISABLE_STEAM_AUDIO)
    target_compile_definitions(xrSound PRIVATE DISABLE_RESONANCE_AUDIO)
endif()

if (IXRAY_MP)
    target_link_libraries(xrSound PRIVATE SpeexDSP::SpeexDSP)
    target_link_libraries(xrSound PRIVATE Opus::Opus)
	target_compile_definitions(xrSound PUBLIC XR_MP_BUILD)
endif()