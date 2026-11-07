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
target_compile_definitions(xrRHI PRIVATE XR_RHI_EXPORTS)
set_target_properties(xrRHI PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

# Project defines
target_compile_definitions(xrRHI PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# -----------------------------------------------------------------------------
# Linux + DXVK Native поддержка
# -----------------------------------------------------------------------------
if(UNIX)
    target_include_directories(xrRHI PUBLIC ${DXVK_INCLUDE_DIRS})
    target_compile_definitions(xrRHI PRIVATE XR_USE_DXVK_NATIVE=1 DXVK_WSI_DRIVER_SDL3=1)
    if(APPLE)
        # D3D9 retains DXVK; the native macOS R4 path uses DXMT.
        target_link_libraries(xrRHI PUBLIC ${DXVK_D3D9} DXMT::D3D11 DXMT::DXGI Vulkan::Vulkan)
        target_compile_definitions(xrRHI PRIVATE XR_USE_DXMT_NATIVE=1)
        target_sources(xrRHI PRIVATE Platform/MacOS/MetalWindow.mm Platform/MacOS/DirectXTexture.cpp)
        target_link_libraries(xrRHI PRIVATE DirectX::Tex)
        target_link_libraries(xrRHI PRIVATE "-framework Cocoa" "-framework QuartzCore" "-framework Metal")
    else()
        target_link_libraries(xrRHI PUBLIC ${DXVK_LIBRARIES} Vulkan::Vulkan pthread dl)
    endif()
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

if(APPLE)
    target_link_libraries(xrRHI PUBLIC VKD3D::Utils)
endif()

target_sources(xrRHI PRIVATE Platform/RHIWindow.cpp)

if(APPLE)
    add_executable(TestMacOSShaderCompiler EXCLUDE_FROM_ALL "${CMAKE_SOURCE_DIR}/src/test/macos/ShaderCompilerSmoke.cpp")
    target_include_directories(TestMacOSShaderCompiler PRIVATE ${DXVK_INCLUDE_DIRS})
    target_link_libraries(TestMacOSShaderCompiler PRIVATE VKD3D::Utils)
    set_target_properties(TestMacOSShaderCompiler PROPERTIES FOLDER "Test" DISABLE_PRECOMPILE_HEADERS ON)
endif()

if(APPLE)
    add_executable(TestMacOSPlatform EXCLUDE_FROM_ALL "${CMAKE_SOURCE_DIR}/src/test/macos/PlatformSmoke.cpp")
    target_include_directories(TestMacOSPlatform PRIVATE "${CMAKE_SOURCE_DIR}/src/xrCore")
    target_link_libraries(TestMacOSPlatform PRIVATE xrRHI)
    set_target_properties(TestMacOSPlatform PROPERTIES FOLDER "Test" DISABLE_PRECOMPILE_HEADERS ON)
endif()

if(APPLE)
    add_executable(TestMacOSLua EXCLUDE_FROM_ALL "${CMAKE_SOURCE_DIR}/src/test/macos/LuaSmoke.cpp")
    target_include_directories(TestMacOSLua PRIVATE "${LUAJIT_INC}")
    target_link_libraries(TestMacOSLua PRIVATE libluajit)
    set_target_properties(TestMacOSLua PROPERTIES FOLDER "Test" DISABLE_PRECOMPILE_HEADERS ON)
endif()

if(APPLE)
    add_executable(TestMacOSTexture EXCLUDE_FROM_ALL "${CMAKE_SOURCE_DIR}/src/test/macos/TextureSmoke.cpp")
    target_include_directories(TestMacOSTexture PRIVATE "${CMAKE_BINARY_DIR}/_deps_header_only")
    target_link_libraries(TestMacOSTexture PRIVATE DirectX::Tex)
    set_target_properties(TestMacOSTexture PROPERTIES FOLDER "Test" DISABLE_PRECOMPILE_HEADERS ON)
endif()
