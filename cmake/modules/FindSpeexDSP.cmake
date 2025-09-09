# ======================
# speexdsp
# ======================
find_package(SpeexDSP QUIET)

if(NOT TARGET SpeexDSP::speexdsp)
    if(PkgConfig_FOUND)
        pkg_check_modules(SPEEXDSP QUIET speexdsp)
    endif()

    if(SPEEXDSP_FOUND)
        message(STATUS "Using system speexdsp from pkg-config")

        add_library(SpeexDSP::speexdsp INTERFACE IMPORTED)
        set_target_properties(SpeexDSP::speexdsp PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${SPEEXDSP_INCLUDE_DIRS}"
            INTERFACE_LINK_LIBRARIES "${SPEEXDSP_LIBRARIES}"
        )
    else()
        message(STATUS "speexdsp not found, fetching from source...")
        FetchContent_Declare(
            speexdsp
            GIT_REPOSITORY https://github.com/thewh1teagle/speexdsp.git
            GIT_TAG        feat/add-cmake
        )
        FetchContent_MakeAvailable(speexdsp)

        if(TARGET speexdsp)
        target_include_directories(speexdsp PUBLIC
            ${speexdsp_SOURCE_DIR}/include
            ${speexdsp_SOURCE_DIR}/libspeexdsp
        )

        if(NOT TARGET SpeexDSP::speexdsp)
            add_library(SpeexDSP::speexdsp ALIAS speexdsp)
        endif()
endif()
        
    endif()
endif()
