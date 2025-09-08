include(FetchContent)
set(FETCHCONTENT_BASE_DIR ${CMAKE_BINARY_DIR}/dep)

message(STATUS "[packages] Resolving third-party repositories (Windows)...")

# yaml-cpp (prefer NuGet variable if already restored)
if(NOT TARGET yaml-cpp)
    FetchContent_Declare(
        yaml-cpp
        GIT_REPOSITORY https://github.com/jbeder/yaml-cpp.git
        GIT_TAG        0.8.0
    )
    FetchContent_MakeAvailable(yaml-cpp)
    message(STATUS "[packages] yaml-cpp - fetched (FetchContent)")
else()
    message(STATUS "[packages] yaml-cpp - already available (NuGet or previous)")
endif()

# TBB (NuGet package usually defines IXR_TBB_SDK; fallback to oneTBB if not)
find_package(TBB QUIET)
if(NOT TBB_FOUND AND NOT EXISTS ${IXR_TBB_SDK})
    message(STATUS "[packages] TBB not found; fetching oneTBB fallback ...")
    FetchContent_Declare(
        tbb
        GIT_REPOSITORY https://github.com/oneapi-src/oneTBB.git
        GIT_TAG v2021.5.0
    )
    set(TBB_TEST OFF CACHE BOOL "Disable TBB tests" FORCE)
    set(TBB_STRICT OFF CACHE BOOL "Disable TBB strict warnings" FORCE)
    FetchContent_MakeAvailable(tbb)
    if(TARGET TBB::tbb)
        message(STATUS "[packages] TBB - fetched fallback")
    else()
        message(FATAL_ERROR "[packages] Failed to fetch oneTBB fallback")
    endif()
else()
    message(STATUS "[packages] TBB - provided via NuGet or system")
endif()

# LZO (provided via NuGet on Windows: variables from nuget.cmake)
if(DEFINED LZO AND EXISTS ${LZO_LIB})
    message(STATUS "[packages] LZO - provided via NuGet (${LZO_LIB})")
else()
    message(STATUS "[packages] LZO - NuGet variables not set yet (will rely on later inclusion of nuget.cmake)")
endif()

# NVTT (provided via NuGet; do not FetchContent again to avoid double build)
if(DEFINED NVTT)
    message(STATUS "[packages] nvtt - provided via NuGet (${NVTT})")
else()
    message(STATUS "[packages] nvtt - NuGet variable not defined; if missing you may add a FetchContent block similar to Linux version")
endif()

# lunasvg
FetchContent_Declare(
    lunasvg
    GIT_REPOSITORY https://github.com/sammycage/lunasvg.git
    GIT_TAG v3.4.0
)
FetchContent_MakeAvailable(lunasvg)
message(STATUS "[packages] lunasvg - fetched")

# Vulkan dependencies (common script)
include(${CMAKE_CURRENT_LIST_DIR}/../common/packages-vulkan.cmake)

message(STATUS "[packages] All Windows packages resolved")