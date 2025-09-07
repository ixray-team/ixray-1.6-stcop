# Common Vulkan FetchContent logic shared by Windows & Linux
# Expects:
#  - IXRAY_USE_VK (option)
#  - IXRAY_DEFAULT_VULKAN_TAG_VERSION (string)
# Optional override: IXRAY_VULKAN_TAG_VERSION (user-set).

include(FetchContent)

if(NOT IXRAY_USE_VK)
  message(STATUS "[packages][vulkan] Vulkan renderer disabled (IXRAY_USE_VK=OFF)")
  return()
endif()

# Determine effective tag
if("${IXRAY_VULKAN_TAG_VERSION}" STREQUAL "")
  set(IXRAY_VULKAN_TAG_VERSION "${IXRAY_DEFAULT_VULKAN_TAG_VERSION}")
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
# Consistent CI-friendly opts
set(SPIRV_TOOLS_BUILD_STATIC ON CACHE BOOL "Build SPIRV-Tools static" FORCE)
set(SPIRV_TOOLS_BUILD_TESTS OFF CACHE BOOL "Disable SPIRV-Tools tests" FORCE)
set(SPIRV_TOOLS_BUILD_FUZZERS OFF CACHE BOOL "Disable SPIRV-Tools fuzzers" FORCE)
FetchContent_MakeAvailable(SPIRV-Tools)
message(STATUS "[packages][vulkan] SPIRV-Tools - fetched")
