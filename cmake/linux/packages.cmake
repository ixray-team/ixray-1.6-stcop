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

# TBB (prefer system, fallback to oneTBB)
find_package(TBB QUIET)
if(NOT TBB_FOUND)
  message(STATUS "[packages] TBB not found via find_package; fetching oneTBB ...")
  FetchContent_Declare(
    tbb
    GIT_REPOSITORY https://github.com/oneapi-src/oneTBB.git
    GIT_TAG v2021.5.0
  )
  set(TBB_TEST OFF CACHE BOOL "Disable TBB tests" FORCE)
  set(TBB_STRICT OFF CACHE BOOL "Disable TBB strict warnings" FORCE)
  FetchContent_MakeAvailable(tbb)
  if(TARGET TBB::tbb)
    message(STATUS "[packages] TBB - fetched")
  else()
    message(FATAL_ERROR "[packages] Failed to fetch oneTBB")
  endif()
else()
  message(STATUS "[packages] TBB - system package found")
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

# NVTT (texture tools)
FetchContent_Declare(
    nvtt
    GIT_REPOSITORY https://github.com/imesense-forks/castano-nvidia-texture-tools.git
    GIT_TAG        default
)
FetchContent_MakeAvailable(nvtt)
message(STATUS "[packages] nvtt - fetched")

# lunasvg (needed for some tools, keep consistent with Windows script)
FetchContent_Declare(
    lunasvg
    GIT_REPOSITORY https://github.com/sammycage/lunasvg.git
    GIT_TAG v3.4.0
)
FetchContent_MakeAvailable(lunasvg)
message(STATUS "[packages] lunasvg - fetched")

# Vulkan dependencies (conditionally, only if Vulkan renderer enabled)
if(IXRAY_USE_VK)
  if("${IXRAY_VULKAN_TAG_VERSION}" STREQUAL "")
    set(IXRAY_VULKAN_TAG_VERSION "vulkan-sdk-1.4.309.0")
  endif()
  message(STATUS "[packages][vulkan] Using tag ${IXRAY_VULKAN_TAG_VERSION}")

  # Headers
  FetchContent_Declare(
      VulkanHeaders
      GIT_REPOSITORY https://github.com/KhronosGroup/Vulkan-Headers.git
      GIT_TAG "${IXRAY_VULKAN_TAG_VERSION}"
  )
  FetchContent_MakeAvailable(VulkanHeaders)
  message(STATUS "[packages][vulkan] Vulkan-Headers - fetched")

  # Loader
  FetchContent_Declare(
      VulkanLoader
      GIT_REPOSITORY https://github.com/KhronosGroup/Vulkan-Loader.git
      GIT_TAG "${IXRAY_VULKAN_TAG_VERSION}"
  )
  FetchContent_MakeAvailable(VulkanLoader)
  message(STATUS "[packages][vulkan] Vulkan-Loader - fetched")

  # Validation Layers
  FetchContent_Declare(
      VulkanValidationLayers
      GIT_REPOSITORY https://github.com/KhronosGroup/Vulkan-ValidationLayers.git
      GIT_TAG "${IXRAY_VULKAN_TAG_VERSION}"
  )
  FetchContent_MakeAvailable(VulkanValidationLayers)
  message(STATUS "[packages][vulkan] Vulkan-ValidationLayers - fetched")

  # SPIRV Headers
  FetchContent_Declare(
      SPIRV-Headers
      GIT_REPOSITORY https://github.com/KhronosGroup/SPIRV-Headers.git
      GIT_TAG "${IXRAY_VULKAN_TAG_VERSION}"
  )
  FetchContent_MakeAvailable(SPIRV-Headers)
  message(STATUS "[packages][vulkan] SPIRV-Headers - fetched")

  # SPIRV Tools
  FetchContent_Declare(
      SPIRV-Tools
      GIT_REPOSITORY https://github.com/KhronosGroup/SPIRV-Tools.git
      GIT_TAG "${IXRAY_VULKAN_TAG_VERSION}"
  )
  # Avoid building tests/tools to cut CI time
  set(SPIRV_TOOLS_BUILD_STATIC ON CACHE BOOL "Build SPIRV-Tools static" FORCE)
  set(SPIRV_TOOLS_BUILD_TESTS OFF CACHE BOOL "Disable SPIRV-Tools tests" FORCE)
  set(SPIRV_TOOLS_BUILD_FUZZERS OFF CACHE BOOL "Disable SPIRV-Tools fuzzers" FORCE)
  FetchContent_MakeAvailable(SPIRV-Tools)
  message(STATUS "[packages][vulkan] SPIRV-Tools - fetched")
endif()

message(STATUS "[packages] All Linux packages resolved")