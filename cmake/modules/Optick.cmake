# Optick profiler core, built from the vendored source so the library itself
# can be modified (Lua support, protocol, etc.) instead of relying on the
# prebuilt NuGet runtime.
set(OPTICK_ROOT_DIR "${CMAKE_SOURCE_DIR}/src/3rd-party/optick")

file(GLOB OPTICK_SOURCE_FILES CONFIGURE_DEPENDS "${OPTICK_ROOT_DIR}/src/*.cpp")

add_library(OptickCore SHARED ${OPTICK_SOURCE_FILES})

target_include_directories(OptickCore
	PUBLIC
		"${OPTICK_ROOT_DIR}/src"
)

target_compile_definitions(OptickCore PRIVATE
	OPTICK_EXPORTS=1
	OPTICK_ENABLE_GPU=1
	OPTICK_ENABLE_GPU_D3D11=1
	OPTICK_ENABLE_GPU_VULKAN=0
	OPTICK_ENABLE_GPU_D3D12=0
)

if(MSVC)
	target_compile_definitions(OptickCore PRIVATE _SILENCE_ALL_CXX17_DEPRECATION_WARNINGS)
	target_link_libraries(OptickCore PRIVATE advapi32 dbghelp ws2_32 d3d11)
	set_target_properties(OptickCore PROPERTIES DEBUG_POSTFIX d)
elseif(UNIX)
	target_link_libraries(OptickCore PRIVATE pthread dl)
endif()
