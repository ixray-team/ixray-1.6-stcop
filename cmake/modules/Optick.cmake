# Optick profiler core, built from the vendored source so the library itself
# can be modified (Lua support, protocol, etc.) instead of relying on the
# prebuilt NuGet runtime.
set(OPTICK_ROOT_DIR "${CMAKE_SOURCE_DIR}/src/3rd-party/optick")

# Optick settings (visible in CMake GUI).
option(OPTICK_GPU "Optick: enable GPU profiling" OFF)
option(OPTICK_GPU_D3D11 "Optick: enable D3D11 GPU timestamps" ON)
option(OPTICK_GPU_D3D12 "Optick: enable D3D12 GPU timestamps" OFF)
option(OPTICK_GPU_VULKAN "Optick: enable Vulkan GPU timestamps (requires Vulkan SDK)" OFF)
option(OPTICK_TRACING "Optick: enable kernel-level tracing (switch contexts, autosampling)" ON)

set(OPTICK_ENABLE_GPU 0)
set(OPTICK_ENABLE_GPU_D3D11 0)
set(OPTICK_ENABLE_GPU_D3D12 0)
set(OPTICK_ENABLE_GPU_VULKAN 0)
set(OPTICK_ENABLE_TRACING 0)

if(OPTICK_TRACING)
	set(OPTICK_ENABLE_TRACING 1)
endif()

if(OPTICK_GPU)
	set(OPTICK_ENABLE_GPU 1)

	if(OPTICK_GPU_D3D11)
		set(OPTICK_ENABLE_GPU_D3D11 1)
	endif()

	if(OPTICK_GPU_D3D12)
		set(OPTICK_ENABLE_GPU_D3D12 1)
	endif()

	if(OPTICK_GPU_VULKAN)
		find_package(Vulkan QUIET)
		if(Vulkan_FOUND)
			set(OPTICK_ENABLE_GPU_VULKAN 1)
		else()
			message(STATUS "Optick: Vulkan SDK not found, OPTICK_GPU_VULKAN will be ignored")
		endif()
	endif()
endif()

file(GLOB OPTICK_SOURCE_FILES CONFIGURE_DEPENDS "${OPTICK_ROOT_DIR}/src/*.cpp")

add_library(OptickCore SHARED ${OPTICK_SOURCE_FILES})

target_include_directories(OptickCore
	PUBLIC
		"${OPTICK_ROOT_DIR}/src"
)

target_compile_definitions(OptickCore PRIVATE
	OPTICK_EXPORTS=1
	OPTICK_ENABLE_GPU=${OPTICK_ENABLE_GPU}
	OPTICK_ENABLE_TRACING=${OPTICK_ENABLE_TRACING}
	OPTICK_ENABLE_GPU_D3D11=${OPTICK_ENABLE_GPU_D3D11}
	OPTICK_ENABLE_GPU_VULKAN=${OPTICK_ENABLE_GPU_VULKAN}
	OPTICK_ENABLE_GPU_D3D12=${OPTICK_ENABLE_GPU_D3D12}
)

if(MSVC)
	target_compile_definitions(OptickCore PRIVATE _SILENCE_ALL_CXX17_DEPRECATION_WARNINGS)
	target_link_libraries(OptickCore PRIVATE advapi32 dbghelp ws2_32)

	if(OPTICK_ENABLE_GPU_D3D11)
		target_link_libraries(OptickCore PRIVATE d3d11)
	endif()

	if(OPTICK_ENABLE_GPU_D3D12)
		target_link_libraries(OptickCore PRIVATE d3d12 dxgi)
	endif()

	if(OPTICK_ENABLE_GPU_VULKAN)
		target_include_directories(OptickCore PRIVATE ${Vulkan_INCLUDE_DIRS})
		target_link_libraries(OptickCore PRIVATE Vulkan::Vulkan)
	endif()

	set_target_properties(OptickCore PROPERTIES DEBUG_POSTFIX d)
elseif(UNIX)
	target_link_libraries(OptickCore PRIVATE pthread dl)
endif()
