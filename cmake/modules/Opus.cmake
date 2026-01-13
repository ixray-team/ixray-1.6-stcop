# ======================
# opus
# ======================
include(FetchContent)

if(WIN32)
    set(OPUS ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Opus.Runtimes.win-${NUGET_PACKAGE_PLATFORM}.2024.5.22-open)
    
    add_imported_lib(
        Opus::Opus
        "${OPUS}/build/native/include/opus"
        "${OPUS}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/opus.lib"
        "${OPUS}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/opus.dll"
    )
else()
    find_package(Opus QUIET)
    
    if(NOT TARGET Opus::Opus)
        if(PkgConfig_FOUND)
            pkg_check_modules(OPUS QUIET opus)
        endif()
    
        if(OPUS_FOUND)
            message(STATUS "Using system opus from pkg-config")
    
            add_library(Opus::Opus INTERFACE IMPORTED)
            set_target_properties(Opus::Opus PROPERTIES
                INTERFACE_INCLUDE_DIRECTORIES "${OPUS_INCLUDE_DIRS}"
                INTERFACE_LINK_LIBRARIES "${OPUS_LIBRARIES}"
            )
        else()
            set(CMAKE_POSITION_INDEPENDENT_CODE ON)
            message(STATUS "opus not found, fetching from source...")
            FetchContent_Declare(
                opus
                GIT_REPOSITORY https://github.com/xiph/opus.git
                GIT_TAG        v1.5.2
            )
            FetchContent_MakeAvailable(opus)
    
            add_library(Opus::Opus ALIAS opus)
            target_include_directories(opus INTERFACE $<BUILD_INTERFACE:${opus_SOURCE_DIR}/include> $<INSTALL_INTERFACE:include>)
        endif()
    endif()
endif()