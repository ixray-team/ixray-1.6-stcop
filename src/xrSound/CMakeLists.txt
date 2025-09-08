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

# xrSound project
add_library(xrSound SHARED ${SND_SRC_FILES})
target_include_directories(xrSound PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_precompile_headers(xrSound PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(xrSound PRIVATE XRSOUND_EXPORTS)
target_compile_definitions(xrSound PRIVATE _USRDLL)

# Linker list
target_link_libraries(xrSound PUBLIC xrCore)

# Copy binaries
if(WIN32)
    add_custom_command(TARGET ${PROJECT_NAME}
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SND_OGG}native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/libogg.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SND_OAL}native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/OpenAL32.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SND_VOB}native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/libvorbisfile.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SND_VOB}native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/libvorbis.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SPEEXDSP}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/speexdsp.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/speexdsp.dll
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${OPUS}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/opus.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    )

    ## LibOGG (Windows)
    target_link_libraries(xrSound PRIVATE ${SND_OGG}native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libogg.lib)
    target_include_directories(xrSound PRIVATE "${SND_OGG}/native/include/")

    ## Vorbis (Windows)
    target_link_libraries(xrSound PRIVATE ${SND_VOB}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libvorbisfile.lib)
    target_link_libraries(xrSound PRIVATE ${SND_VOB}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/libvorbis.lib)
    target_include_directories(xrSound PRIVATE "${SND_VOB}/native/include/")

    ## OpenAL (Windows)
    target_link_libraries(xrSound PRIVATE ${SND_OAL}/native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/OpenAL32.lib)
    target_include_directories(xrSound PRIVATE "${SND_OAL}/native/include/")

    ## SpeexDSP (Windows)
    target_link_libraries(xrSound PRIVATE ${SPEEXDSP}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/speexdsp.lib)
    target_include_directories(xrSound PRIVATE "${SPEEXDSP}/build/native/include/")

    ## OPUS (Windows)
    target_link_libraries(xrSound PRIVATE ${OPUS}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/opus.lib)
    target_include_directories(xrSound PRIVATE "${OPUS}/build/native/include/")
else()
    # On non-Windows platforms, prefer system libraries when available.
    find_package(PkgConfig REQUIRED)

    # libogg
    pkg_check_modules(OGG ogg QUIET)
    if(OGG_FOUND)
        target_include_directories(xrSound PRIVATE ${OGG_INCLUDE_DIRS})
        target_link_libraries(xrSound PRIVATE ${OGG_LIBRARIES})
    else()
        find_library(OGG_LIB ogg)
        if(OGG_LIB)
            target_link_libraries(xrSound PRIVATE ${OGG_LIB})
        else()
            message(WARNING "libogg not found on system and no NuGet package available; build may fail.")
        endif()
    endif()

    # libvorbis
    pkg_check_modules(VORBIS vorbisfile vorbis QUIET)
    if(VORBIS_FOUND)
        target_include_directories(xrSound PRIVATE ${VORBIS_INCLUDE_DIRS})
        target_link_libraries(xrSound PRIVATE ${VORBIS_LIBRARIES})
    else()
        find_library(VORBISFILE_LIB vorbisfile)
        find_library(VORBIS_LIB vorbis)
        if(VORBISFILE_LIB)
            target_link_libraries(xrSound PRIVATE ${VORBISFILE_LIB} ${VORBIS_LIB})
        else()
            message(WARNING "libvorbis not found on system; optional features disabled.")
        endif()
    endif()

    # OpenAL
    find_package(OpenAL QUIET)
    if(OpenAL_FOUND)
        target_include_directories(xrSound PRIVATE ${OPENAL_INCLUDE_DIR})
        target_link_libraries(xrSound PRIVATE ${OPENAL_LIBRARY})
    else()
        find_library(OPENAL_LIB OpenAL)
        if(OPENAL_LIB)
            target_link_libraries(xrSound PRIVATE ${OPENAL_LIB})
        else()
            message(WARNING "OpenAL not found on system; audio output may be unavailable.")
        endif()
    endif()

    # SpeexDSP
    pkg_check_modules(SPEEXDSP speexdsp QUIET)
    if(SPEEXDSP_FOUND)
        target_include_directories(xrSound PRIVATE ${SPEEXDSP_INCLUDE_DIRS})
        target_link_libraries(xrSound PRIVATE ${SPEEXDSP_LIBRARIES})
    endif()

    # Opus
    pkg_check_modules(OPUS opus QUIET)
    if(OPUS_FOUND)
        target_include_directories(xrSound PRIVATE ${OPUS_INCLUDE_DIRS})
        target_link_libraries(xrSound PRIVATE ${OPUS_LIBRARIES})
    endif()
endif()