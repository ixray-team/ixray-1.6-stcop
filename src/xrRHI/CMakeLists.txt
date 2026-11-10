project(xrRHI)

set(XRHI_SRC_FILES)

# Files import
file(GLOB XRHI_SOURCE_ALL_FILES CONFIGURE_DEPENDS
	"*.cpp" "*.h"
)
file(GLOB_RECURSE XRHI_SOURCE_D3D11_FILES CONFIGURE_DEPENDS "D3D11/*.*")
if(WIN32)
    file(GLOB_RECURSE XRHI_SOURCE_D3D12_FILES CONFIGURE_DEPENDS "D3D12/*.cpp" "D3D12/*.h")
endif()
file(GLOB_RECURSE XRHI_SOURCE_GPU_FILES CONFIGURE_DEPENDS "Drivers/*.*")
file(GLOB_RECURSE XRHI_SOURCE_PRIVATE_FILES CONFIGURE_DEPENDS "Private/*.*")
file(GLOB_RECURSE XRHI_SOURCE_IMGUI_FILES CONFIGURE_DEPENDS "Layout/ImGui/*.*")

# File groups
source_group("Core" FILES ${XRHI_SOURCE_ALL_FILES})
source_group("Interface/D3D11" FILES ${XRHI_SOURCE_D3D11_FILES})
source_group("Interface/D3D12" FILES ${XRHI_SOURCE_D3D12_FILES})
source_group("Drivers" FILES ${XRHI_SOURCE_GPU_FILES})
source_group("Layout/ImGui" FILES ${XRHI_SOURCE_IMGUI_FILES})
source_group("Private" FILES ${XRHI_SOURCE_PRIVATE_FILES})

# Apply list
list(APPEND XRHI_SRC_FILES ${XRHI_SOURCE_ALL_FILES})
list(APPEND XRHI_SRC_FILES ${XRHI_SOURCE_IMGUI_FILES})
list(APPEND XRHI_SRC_FILES ${XRHI_SOURCE_GPU_FILES})
list(APPEND XRHI_SRC_FILES ${XRHI_SOURCE_D3D11_FILES})
list(APPEND XRHI_SRC_FILES ${XRHI_SOURCE_D3D12_FILES})
list(APPEND XRHI_SRC_FILES ${XRHI_SOURCE_PRIVATE_FILES})

if (NOT WIN32)
	# Исключаем Windows-специфичные файлы
	list(FILTER XRHI_SRC_FILES EXCLUDE REGEX ".*AMDGPUTransferee.*")
	list(FILTER XRHI_SRC_FILES EXCLUDE REGEX ".*NvGPUTransferee.*")
	list(FILTER XRHI_SRC_FILES EXCLUDE REGEX ".*IntelGPUTransferee.*")
	list(FILTER XRHI_SRC_FILES EXCLUDE REGEX "imgui_impl_dx11\\.cpp$")
    list(FILTER XRHI_SRC_FILES EXCLUDE REGEX "imgui_impl_dx12\\.cpp$")
    # The bundled Direct3D upscaler SDKs are linked only on Windows.
    list(FILTER XRHI_SRC_FILES EXCLUDE REGEX "D3D11/(DLSS|FSR3)Wrapper\\.cpp$")
endif()

if(WIN32)
    list(FILTER XRHI_SRC_FILES EXCLUDE REGEX "D3D11/DirectXTexD3D11Native\\.cpp$")
endif()

if(NOT APPLE)
	list(FILTER XRHI_SRC_FILES EXCLUDE REGEX "D3D11/MacMetalBridge\\.mm$")
endif()

# xrRHI project
set_source_files_properties(Private/DxcAPI.cpp PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON SKIP_PRECOMPILE_HEADERS ON)
if(APPLE)
	set_source_files_properties(D3D11/MacMetalBridge.mm PROPERTIES SKIP_PRECOMPILE_HEADERS ON)
endif()
add_library(xrRHI SHARED ${XRHI_SRC_FILES})
target_include_directories(xrRHI PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrRHI PRIVATE "${IXRAY_SDK_INC}")
target_include_directories(xrRHI PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../xrCore/")
target_compile_definitions(xrRHI PRIVATE XR_RHI_EXPORTS IMGUI_IMPL_API=)
target_precompile_headers(xrRHI PRIVATE "$<$<COMPILE_LANGUAGE:CXX>:${CMAKE_CURRENT_SOURCE_DIR}/RHI.h>")

# Project defines
target_compile_definitions(xrRHI PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# -----------------------------------------------------------------------------
# Linux + DXVK Native поддержка
# -----------------------------------------------------------------------------
if(APPLE)
    target_include_directories(xrRHI PUBLIC ${DXMT_INCLUDE_DIRS})
    target_compile_definitions(xrRHI PRIVATE XR_USE_NATIVE_D3D11=1)
    target_link_libraries(xrRHI PUBLIC ${DXMT_LIBRARIES})
    target_link_libraries(xrRHI PRIVATE "-framework Cocoa" "-framework Metal" "-framework QuartzCore")
    target_link_libraries(xrRHI PRIVATE ${VKD3D_LIBRARIES})
elseif(UNIX AND NOT APPLE)
    target_include_directories(xrRHI PUBLIC ${DXVK_INCLUDE_DIRS})
    target_compile_definitions(xrRHI PRIVATE XR_USE_NATIVE_D3D11=1 DXVK_WSI_DRIVER_SDL3=1)
    target_link_libraries(xrRHI PUBLIC ${DXVK_LIBRARIES} Vulkan::Vulkan pthread dl)
    target_link_libraries(xrRHI PRIVATE ${VKD3D_LIBRARIES})
    set_property(TARGET xrRHI APPEND PROPERTY BUILD_RPATH "$ORIGIN")
    foreach(VKD3D_RUNTIME_LIBRARY IN LISTS VKD3D_RUNTIME_LIBRARIES)
        target_copy_dependency(xrRHI "${VKD3D_RUNTIME_LIBRARY}")
    endforeach()
elseif(WIN32)
    target_link_libraries(xrRHI PRIVATE WindowsSDK::D3D11 WindowsSDK::D3D12)
    target_link_libraries(xrRHI PRIVATE WindowsSDK::DXGI)
    target_link_libraries(xrRHI PRIVATE WindowsSDK::D3DCompiler)
    if(CMAKE_SIZEOF_VOID_P EQUAL 8)
        # Deploy the compiler and validator from the same SDK directory.
        # Without the validator DXC can produce unsigned shader containers.
        foreach(_sdk_root IN LISTS WINDOWSSDK_PREFERRED_FIRST_DIRS)
            foreach(_dxc_dir
                "${_sdk_root}/Redist/${CMAKE_VS_WINDOWS_TARGET_PLATFORM_VERSION}/d3d/x64"
                "${_sdk_root}/Redist/D3D/x64")
                if(EXISTS "${_dxc_dir}/dxcompiler.dll" AND EXISTS "${_dxc_dir}/dxil.dll")
                    target_copy_dependency(xrRHI "${_dxc_dir}/dxcompiler.dll")
                    target_copy_dependency(xrRHI "${_dxc_dir}/dxil.dll")
                    set(_dxc_runtime_found TRUE)
                    break()
                endif()
            endforeach()
            if(_dxc_runtime_found)
                break()
            endif()
        endforeach()
    endif()
    target_link_libraries(xrRHI PRIVATE AMD::AMD_AGS)
    if(CMAKE_SIZEOF_VOID_P EQUAL 8)
        target_include_directories(xrRHI PRIVATE "${INTEL_XESS}")
        target_link_libraries(xrRHI PRIVATE "${INTEL_XESS_LIB}" "${INTEL_XESS_DX11_LIB}")
        target_copy_dependency(xrRHI "${INTEL_XESS_BIN}")
        target_copy_dependency(xrRHI "${INTEL_XESS_DX11_BIN}")
    endif()
	target_link_libraries(xrRHI PRIVATE AMD::FSR3_DX11 AMD::FSR3 NVIDIA::DLSS WindowsSDK::DXGUID)
endif()

target_link_libraries(xrRHI PUBLIC xrCore)
target_link_libraries(xrRHI PUBLIC imgui)
target_link_libraries(xrRHI PRIVATE DirectX::Mesh)
target_link_libraries(xrRHI PRIVATE DirectX::Tex)
target_link_libraries(xrRHI PRIVATE DXC::Headers)

ixray_split_compile(xrRHI)
