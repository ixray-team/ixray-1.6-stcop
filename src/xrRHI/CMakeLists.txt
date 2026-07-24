project(xrRHI)
set(XRHI_SRC_FILES)

# Files import
file(GLOB XRHI_SOURCE_ALL_FILES CONFIGURE_DEPENDS
	"*.cpp" "*.h"
)
file(GLOB_RECURSE XRHI_SOURCE_D3D11_FILES CONFIGURE_DEPENDS "D3D11/*.*")
file(GLOB_RECURSE XRHI_SOURCE_D3D9_FILES CONFIGURE_DEPENDS "D3D9/*.*")
file(GLOB_RECURSE XRHI_SOURCE_GPU_FILES CONFIGURE_DEPENDS "Drivers/*.*")
file(GLOB_RECURSE XRHI_SOURCE_PRIVATE_FILES CONFIGURE_DEPENDS "Private/*.*")
file(GLOB_RECURSE XRHI_SOURCE_IMGUI_FILES CONFIGURE_DEPENDS "Layout/ImGui/*.*")

# File groups
source_group("Core" FILES ${XRHI_SOURCE_ALL_FILES})
source_group("Interface/D3D11" FILES ${XRHI_SOURCE_D3D11_FILES})
source_group("Interface/D3D9" FILES ${XRHI_SOURCE_D3D9_FILES})
source_group("Drivers" FILES ${XRHI_SOURCE_GPU_FILES})
source_group("Layout/ImGui" FILES ${XRHI_SOURCE_IMGUI_FILES})
source_group("Private" FILES ${XRHI_SOURCE_PRIVATE_FILES})

# Apply list
list(APPEND XRHI_SRC_FILES ${XRHI_SOURCE_ALL_FILES})
list(APPEND XRHI_SRC_FILES ${XRHI_SOURCE_IMGUI_FILES})
list(APPEND XRHI_SRC_FILES ${XRHI_SOURCE_GPU_FILES})
list(APPEND XRHI_SRC_FILES ${XRHI_SOURCE_D3D11_FILES})
list(APPEND XRHI_SRC_FILES ${XRHI_SOURCE_D3D9_FILES})
list(APPEND XRHI_SRC_FILES ${XRHI_SOURCE_PRIVATE_FILES})

if (NOT WIN32)
	# Исключаем Windows-специфичные файлы
	list(FILTER XRHI_SRC_FILES EXCLUDE REGEX ".*AMDGPUTransferee.*")
	list(FILTER XRHI_SRC_FILES EXCLUDE REGEX ".*NvGPUTransferee.*")
	list(FILTER XRHI_SRC_FILES EXCLUDE REGEX ".*IntelGPUTransferee.*")
	list(FILTER XRHI_SRC_FILES EXCLUDE REGEX "imgui_impl_dx(9|11)\\.cpp$")
endif()

# xrRHI project
add_library(xrRHI SHARED ${XRHI_SRC_FILES})
target_include_directories(xrRHI PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrRHI PRIVATE "${IXRAY_SDK_INC}")
target_include_directories(xrRHI PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../xrCore/")
target_include_directories(xrRHI PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../")
target_compile_definitions(xrRHI PRIVATE XR_RHI_EXPORTS)
set_target_properties(xrRHI PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

# Project defines
target_compile_definitions(xrRHI PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# -----------------------------------------------------------------------------
# Linux + DXVK Native поддержка
# -----------------------------------------------------------------------------
if(UNIX AND NOT APPLE)
    target_include_directories(xrRHI PUBLIC ${DXVK_INCLUDE_DIRS})
    target_compile_definitions(xrRHI PRIVATE XR_USE_DXVK_NATIVE=1 DXVK_WSI_DRIVER_SDL3=1)
    target_link_libraries(xrRHI PUBLIC ${DXVK_LIBRARIES} Vulkan::Vulkan pthread dl)
elseif(WIN32)
    target_link_libraries(xrRHI PUBLIC WindowsSDK::D3D11)
    target_link_libraries(xrRHI PUBLIC WindowsSDK::D3D9)
    target_link_libraries(xrRHI PUBLIC WindowsSDK::DXGI)
    target_link_libraries(xrRHI PRIVATE WindowsSDK::D3DCompiler)
    target_link_libraries(xrRHI PRIVATE AMD::AMD_AGS)
endif()

target_link_libraries(xrRHI PUBLIC xrCore)
target_link_libraries(xrRHI PUBLIC imgui)
target_link_libraries(xrRHI PRIVATE DirectX::Mesh)