include(FetchContent)

if(WIN32)
    if(NOT CMAKE_VS_PLATFORM_NAME)
        if(CMAKE_SIZEOF_VOID_P EQUAL 8)
            set(ARCH_DIR "x64")
        else()
            set(ARCH_DIR "Win32")
        endif()
    else()
        set(ARCH_DIR ${CMAKE_VS_PLATFORM_NAME})
    endif()

    set(SND_VOB ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.LibVorbis.1.3.7.4/)
    add_imported_lib(
        Vorbis::vorbis
        "${SND_VOB}/native/include"
        "${SND_VOB}/native/lib/${ARCH_DIR}/Release/libvorbis.lib"
        "${SND_VOB}/native/bin/${ARCH_DIR}/Release/libvorbis.dll"
    )
    add_imported_lib(
        Vorbis::vorbisfile
        "${SND_VOB}/native/include"
        "${SND_VOB}/native/lib/${ARCH_DIR}/Release/libvorbisfile.lib"
        "${SND_VOB}/native/bin/${ARCH_DIR}/Release/libvorbisfile.dll"
    )
else()
    if(PkgConfig_FOUND)
        pkg_check_modules(VORBIS QUIET vorbis)
        pkg_check_modules(VORBISFILE QUIET vorbisfile)
    endif()

    if(VORBIS_FOUND AND VORBISFILE_FOUND)
        add_library(Vorbis::vorbis INTERFACE IMPORTED)
        set_target_properties(Vorbis::vorbis PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${VORBIS_INCLUDE_DIRS}"
            INTERFACE_LINK_LIBRARIES "${VORBIS_LIBRARIES}"
        )

        add_library(Vorbis::vorbisfile INTERFACE IMPORTED)
        set_target_properties(Vorbis::vorbisfile PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${VORBISFILE_INCLUDE_DIRS}"
            INTERFACE_LINK_LIBRARIES "${VORBISFILE_LIBRARIES}"
        )
    else()
        set(CMAKE_POSITION_INDEPENDENT_CODE ON)
        FetchContent_Declare(
            libvorbis
            GIT_REPOSITORY https://github.com/xiph/vorbis.git
            GIT_TAG v1.3.7
        )
        
        FetchContent_MakeAvailable(libvorbis)

        target_link_libraries(vorbis PUBLIC Ogg::ogg)
        target_link_libraries(vorbisfile PUBLIC Ogg::ogg)
        
        if(TARGET vorbis AND NOT TARGET Vorbis::vorbis)
            add_library(Vorbis::vorbis ALIAS vorbis)
        endif()
        if(TARGET vorbisfile AND NOT TARGET Vorbis::vorbisfile)
            add_library(Vorbis::vorbisfile ALIAS vorbisfile)
        endif()
    endif()
endif()

set(Vorbis_FOUND TRUE CACHE INTERNAL "")
set(Vorbis_IN_PROGRESS FALSE CACHE INTERNAL "")