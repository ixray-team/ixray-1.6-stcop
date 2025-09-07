set(FETCHCONTENT_BASE_DIR ${CMAKE_BINARY_DIR}/dep)
include(FetchContent)

message(STATUS "[packages] downloading repositories: ")

FetchContent_Declare(
    lunasvg
    GIT_REPOSITORY https://github.com/sammycage/lunasvg.git
    GIT_TAG v3.4.0
)

FetchContent_MakeAvailable(lunasvg)

message(STATUS "[packages] lunasvg - downloaded!")

if ("${IXRAY_VULKAN_TAG_VERSION}" STREQUAL "")
    set(IXRAY_VULKAN_TAG_VERSION "vulkan-sdk-1.4.309.0")
endif()

FetchContent_Declare(
    VulkanHeaders
    GIT_REPOSITORY https://github.com/KhronosGroup/Vulkan-Headers.git
    GIT_TAG "${IXRAY_VULKAN_TAG_VERSION}" # Replace <version> with a specific SDK version tag
)
FetchContent_MakeAvailable(VulkanHeaders)

message(STATUS "[packages] VulkanHeaders - downloaded!")

  
FetchContent_Declare(
    VulkanLoader
    GIT_REPOSITORY https://github.com/KhronosGroup/Vulkan-Loader.git
    GIT_TAG "${IXRAY_VULKAN_TAG_VERSION}" # Use the same tag as Headers
)
FetchContent_MakeAvailable(VulkanLoader)

message(STATUS "[packages] VulkanLoader - downloaded!")

 
FetchContent_Declare(
    VulkanValidationLayers
    GIT_REPOSITORY https://github.com/KhronosGroup/Vulkan-ValidationLayers.git
    GIT_TAG "${IXRAY_VULKAN_TAG_VERSION}" # Use the same tag as Headers
)
FetchContent_MakeAvailable(VulkanValidationLayers)

message(STATUS "[packages] VulkanValidationLayers - downloaded!")

FetchContent_Declare(
    SPIRV-Headers
    GIT_REPOSITORY https://github.com/KhronosGroup/SPIRV-Headers.git
    GIT_TAG "${IXRAY_VULKAN_TAG_VERSION}"
)

FetchContent_MakeAvailable(SPIRV-Headers)

message(STATUS "[packages] SPIRV-Headers - downloaded!")

FetchContent_Declare(
    SPIRV-Tools
    GIT_REPOSITORY https://github.com/KhronosGroup/SPIRV-Tools.git
    GIT_TAG "${IXRAY_VULKAN_TAG_VERSION}"
)

FetchContent_MakeAvailable(SPIRV-Tools)

message(STATUS "[packages] SPIRV-Tools - downloaded!")