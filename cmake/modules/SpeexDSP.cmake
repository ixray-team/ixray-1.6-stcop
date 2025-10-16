# ======================
# speexdsp
# ======================

if(WIN32)
    set(SPEEXDSP "${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.SpeexDsp.Runtimes.win-${NUGET_PACKAGE_PLATFORM}.2024.6.4.1-open")

    add_imported_lib(
        SpeexDSP::SpeexDSP
        "${SPEEXDSP}/build/native/include"
        "${SPEEXDSP}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/speexdsp.lib"
        "${SPEEXDSP}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/speexdsp.dll"
    )
else()
    find_package(SpeexDSP QUIET)

    if(NOT TARGET SpeexDSP::SpeexDSP)
        if(PkgConfig_FOUND)
            pkg_check_modules(SPEEXDSP QUIET SpeexDSP)
        endif()

        if(SPEEXDSP_FOUND)
            message(STATUS "Using system speexdsp from pkg-config")

            add_library(SpeexDSP::SpeexDSP INTERFACE IMPORTED)
            set_target_properties(SpeexDSP::SpeexDSP PROPERTIES
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
                target_include_directories(speexdsp PUBLIC ${speexdsp_SOURCE_DIR}/include ${speexdsp_SOURCE_DIR}/libspeexdsp)

                if(NOT TARGET SpeexDSP::SpeexDSP)
                    add_library(SpeexDSP::SpeexDSP ALIAS speexdsp)
                endif()
            endif()
        endif()
    endif()
endif()