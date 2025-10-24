project(xrRHI)
set(XRHI_SRC_FILES)

# Files import
file(GLOB XRHI_SOURCE_ALL_FILES CONFIGURE_DEPENDS
	"*.cpp" "*.h"
)
file(GLOB_RECURSE XRHI_SOURCE_D3D11_FILES CONFIGURE_DEPENDS "D3D11/*.*")
file(GLOB_RECURSE XRHI_SOURCE_D3D9_FILES CONFIGURE_DEPENDS "D3D9/*.*")
file(GLOB_RECURSE XRHI_SOURCE_GPU_FILES CONFIGURE_DEPENDS "Drivers/*.*")
file(GLOB_RECURSE XRHI_SOURCE_IMGUI_FILES CONFIGURE_DEPENDS "Layout/ImGui/*.*")

# File groups
source_group("Core" FILES ${XRHI_SOURCE_ALL_FILES})
source_group("Interface/D3D11" FILES ${XRHI_SOURCE_D3D11_FILES})
source_group("Interface/D3D9" FILES ${XRHI_SOURCE_D3D9_FILES})
source_group("Drivers" FILES ${XRHI_SOURCE_GPU_FILES})
source_group("Layout/ImGui" FILES ${XRHI_SOURCE_IMGUI_FILES})

# Apply list
list(APPEND XRHI_SRC_FILES ${XRHI_SOURCE_ALL_FILES})

if (WIN32)
	list(APPEND XRHI_SRC_FILES ${XRHI_SOURCE_D3D11_FILES})
	list(APPEND XRHI_SRC_FILES ${XRHI_SOURCE_D3D9_FILES})
	list(APPEND XRHI_SRC_FILES ${XRHI_SOURCE_GPU_FILES})
	list(APPEND XRHI_SRC_FILES ${XRHI_SOURCE_IMGUI_FILES})
endif()


# xrRHI project
add_library(xrRHI SHARED ${XRHI_SRC_FILES})
target_include_directories(xrRHI PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrRHI PRIVATE "${IXRAY_SDK_INC}")
target_compile_definitions(xrRHI PRIVATE XR_RHI_EXPORTS)
set_target_properties(xrRHI PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

# Project defines
target_compile_definitions(xrRHI PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrRHI PUBLIC xrCore)
target_link_libraries(xrRHI PUBLIC imgui)
if (WIN32)
	target_link_libraries(xrRHI PUBLIC "d3d11.lib")
	target_link_libraries(xrRHI PUBLIC "d3d9.lib")
	target_link_libraries(xrRHI PUBLIC "dxgi.lib")
	target_link_libraries(xrRHI PRIVATE "d3dcompiler.lib")

	# Deps
	target_include_directories(xrRHI PRIVATE "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_2019.2024.6.5.1/include/")
	target_link_libraries(xrRHI PRIVATE "${CMAKE_BINARY_DIR}/packages/directxmesh_desktop_2019.2024.6.5.1/native/lib/x64/Release/DirectXMesh.lib")
	
	# AMD AGS SDK
	target_include_directories(xrRHI PUBLIC ${CMAKE_BINARY_DIR}/dep/amd_ags_sdk/inc/)
	add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
		COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_BINARY_DIR}/dep/amd_ags_sdk/lib/amd_ags_${NUGET_PACKAGE_PLATFORM}.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)
endif()