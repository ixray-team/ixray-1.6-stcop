include(FetchContent)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
find_package(PkgConfig QUIET)

# ======================
# libogg
# ======================
find_package(Ogg QUIET)

if(NOT TARGET Ogg::ogg)
    if(PkgConfig_FOUND)
        pkg_check_modules(OGG QUIET ogg)
    endif()

    if(OGG_FOUND)
        message(STATUS "Using system libogg from pkg-config")
        add_library(Ogg::ogg INTERFACE IMPORTED)
        set_target_properties(Ogg::ogg PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${OGG_INCLUDE_DIRS}"
            INTERFACE_LINK_LIBRARIES "${OGG_LIBRARIES}"
        )
    else()
        message(STATUS "libogg not found, fetching from source...")
        FetchContent_Declare(
            libogg
            GIT_REPOSITORY https://github.com/xiph/ogg.git
            GIT_TAG        v1.3.5
        )
        FetchContent_MakeAvailable(libogg)
    endif()
endif()

# ======================
# libvorbis
# ======================
find_package(Vorbis QUIET)

if(NOT TARGET Vorbis::vorbis)
    if(PkgConfig_FOUND)
        pkg_check_modules(VORBIS QUIET vorbis)
        pkg_check_modules(VORBISFILE QUIET vorbisfile)
    endif()

    if(VORBIS_FOUND AND VORBISFILE_FOUND)
        message(STATUS "Using system libvorbis from pkg-config")

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
        message(STATUS "libvorbis not found, fetching from source...")
        FetchContent_Declare(
            libvorbis
            GIT_REPOSITORY https://github.com/xiph/vorbis.git
            GIT_TAG        v1.3.7
        )

        if(TARGET Ogg::ogg)
            set(OGG_INCLUDE_DIR "${libogg_SOURCE_DIR}/include" CACHE INTERNAL "")
            set(OGG_LIBRARY Ogg::ogg CACHE INTERNAL "")
        endif()

        FetchContent_MakeAvailable(libvorbis)

        if(TARGET vorbis AND NOT TARGET Vorbis::vorbis)
            add_library(Vorbis::vorbis ALIAS vorbis)
        endif()
        
        if(TARGET vorbisfile AND NOT TARGET Vorbis::vorbisfile)
            add_library(Vorbis::vorbisfile ALIAS vorbisfile)
        endif()
    endif()
endif()
