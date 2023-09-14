project(RenderNew)
set(RENDER_NEW_SOURCE_ALL_FILES)

# Files import
file(GLOB_RECURSE RENDER_NEW_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
	"*.h"
)

# Apply list
list(APPEND RENDER_NEW_SRC_FILES ${RENDER_NEW_SOURCE_ALL_FILES})

# RenderNew project
add_library(RenderNew SHARED ${RENDER_NEW_SRC_FILES})
target_precompile_headers(RenderNew PRIVATE "pch.h")

# Include directories
target_include_directories(RenderNew PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

# project defines 
target_compile_definitions(RenderNew PRIVATE _WINDOWS)
target_compile_definitions(RenderNew PRIVATE RENDER_NEW_EXPORTS)
target_compile_definitions(RenderNew PRIVATE _USRDLL)

add_compile_options(/fp:fast)
target_compile_definitions(RenderNew PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(RenderNew PRIVATE xrEngine)
target_link_libraries(RenderNew PRIVATE xrParticles)
target_link_libraries(RenderNew PRIVATE "TinyGLTF::tinygltf")
target_link_libraries(RenderNew PRIVATE "ImGui::imgui")
target_link_libraries(RenderNew PRIVATE "NRI")
target_link_libraries(RenderNew PRIVATE "NRI_D3D11")
target_link_libraries(RenderNew PRIVATE "NRI_D3D12")
target_link_libraries(RenderNew PRIVATE "DirectXMath::dxmath")
target_link_libraries(RenderNew PRIVATE "d3dcompiler.lib")
target_link_libraries(RenderNew PRIVATE ${IXRAY_SDK_LIB}/dxcompiler.lib)