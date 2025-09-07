include(FetchContent)

set(FETCHCONTENT_BASE_DIR ${CMAKE_BINARY_DIR}/dep)

message(STATUS "[packages] Resolving third-party repositories (Linux)...")

# yaml-cpp
FetchContent_Declare(
  yaml-cpp
  GIT_REPOSITORY https://github.com/jbeder/yaml-cpp.git
  GIT_TAG        0.8.0
)
FetchContent_MakeAvailable(yaml-cpp)
message(STATUS "[packages] yaml-cpp - available")

# TBB (prefer system, fallback if option enabled)
find_package(TBB QUIET)
if(NOT TBB_FOUND AND IXRAY_ENABLE_TBB_FETCH)
  message(STATUS "[packages] TBB not found; fetching oneTBB (option enabled)...")
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
elseif(TBB_FOUND)
  message(STATUS "[packages] TBB - system package found")
else()
  message(STATUS "[packages] TBB - not found and fallback disabled (IXRAY_ENABLE_TBB_FETCH=OFF)")
endif()

# LZO (headers + lib must exist on system; fail early if missing)
find_path(LZO_INCLUDE_DIR
    NAMES lzo/lzo1x.h
    PATHS /usr/include /usr/local/include
)
find_library(LZO_LIBRARY
    NAMES lzo2
    PATHS /usr/lib /usr/lib64 /usr/local/lib
)
if (NOT LZO_INCLUDE_DIR)
  message(FATAL_ERROR "[packages] LZO header 'lzo/lzo1x.h' not found; install liblzo2-dev")
endif()
if (NOT LZO_LIBRARY)
  message(FATAL_ERROR "[packages] LZO library 'lzo2' not found; install liblzo2-dev")
endif()
message(STATUS "[packages] LZO - system package found")

# NVTT (texture tools) allow force fetch even if NuGet / external present
if(IXRAY_FORCE_FETCH_NVTT OR NOT TARGET nvtt)
  if(IXRAY_FORCE_FETCH_NVTT)
    message(STATUS "[packages] nvtt - force FetchContent (IXRAY_FORCE_FETCH_NVTT=ON)")
  else()
    message(STATUS "[packages] nvtt - fetching (no existing target)")
  endif()
  FetchContent_Declare(
      nvtt
      GIT_REPOSITORY https://github.com/imesense-forks/castano-nvidia-texture-tools.git
      GIT_TAG        default
  )
  FetchContent_MakeAvailable(nvtt)
  message(STATUS "[packages] nvtt - fetched")
else()
  message(STATUS "[packages] nvtt - already available")
endif()

# lunasvg (needed for some tools, keep consistent with Windows script)
FetchContent_Declare(
    lunasvg
    GIT_REPOSITORY https://github.com/sammycage/lunasvg.git
    GIT_TAG v3.4.0
)
FetchContent_MakeAvailable(lunasvg)
message(STATUS "[packages] lunasvg - fetched")

# Vulkan (delegated to common script)
include(${CMAKE_CURRENT_SOURCE_DIR}/../common/packages-vulkan.cmake)

message(STATUS "[packages] All Linux packages resolved")