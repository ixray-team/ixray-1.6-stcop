# ======================
# opus
# ======================
find_package(Opus QUIET)

if(NOT TARGET Opus::opus)
    if(PkgConfig_FOUND)
        pkg_check_modules(OPUS QUIET opus)
    endif()

    if(OPUS_FOUND)
        message(STATUS "Using system opus from pkg-config")

        add_library(Opus::opus INTERFACE IMPORTED)
        set_target_properties(Opus::opus PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${OPUS_INCLUDE_DIRS}"
            INTERFACE_LINK_LIBRARIES "${OPUS_LIBRARIES}"
        )
    else()
        message(STATUS "opus not found, fetching from source...")
        FetchContent_Declare(
            opus
            GIT_REPOSITORY https://github.com/xiph/opus.git
            GIT_TAG        v1.5.2
        )
        FetchContent_MakeAvailable(opus)

        #add_library(Opus::opus ALIAS opus)
        target_include_directories(opus INTERFACE $<BUILD_INTERFACE:${opus_SOURCE_DIR}/include> $<INSTALL_INTERFACE:include>)
    endif()
endif()
