if(WIN32)
    set(STEAMAUDIO_ROOT "${CMAKE_BINARY_DIR}/packages/IXRay.SteamAudio.4.6.1")

    add_imported_lib(
        SteamAudio::phonon
        "${STEAMAUDIO_ROOT}/include"
        "${STEAMAUDIO_ROOT}/lib/phonon.lib"
        "${STEAMAUDIO_ROOT}/bin/phonon.dll;${STEAMAUDIO_ROOT}/bin/GPUUtilities.dll;${STEAMAUDIO_ROOT}/bin/TrueAudioNext.dll"
    )
elseif(DEVIXRAY_ENABLE_STEAMAUDIO)
    include(FetchContent)

    FetchContent_Declare(
        SteamAudioSDK
        GIT_REPOSITORY https://github.com/ValveSoftware/steam-audio.git
        GIT_TAG master
    )

    FetchContent_MakeAvailable(SteamAudioSDK)

    # Если upstream изменит layout — поправим.
    set(STEAMAUDIO_INCLUDE_DIR "${SteamAudioSDK_SOURCE_DIR}/api/core/include")

    if(NOT TARGET phonon)
        message(FATAL_ERROR "SteamAudio SDK did not produce target 'phonon'. Layout changed?")
    endif()

    add_library(SteamAudio::phonon INTERFACE IMPORTED)

    set_target_properties(SteamAudio::phonon PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${STEAMAUDIO_INCLUDE_DIR}"
        INTERFACE_LINK_LIBRARIES phonon
    )
endif()
